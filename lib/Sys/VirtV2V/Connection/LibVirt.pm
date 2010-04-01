# Sys::VirtV2V::Connection::LibVirt
# Copyright (C) 2009 Red Hat Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

package Sys::VirtV2V::Connection::LibVirt;

use strict;
use warnings;

use Sys::VirtV2V::Connection;

use Net::Netrc;
use URI;
use XML::DOM;

use Sys::Virt;

use Sys::VirtV2V;
use Sys::VirtV2V::UserMessage qw(user_message);

use Locale::TextDomain 'virt-v2v';

@Sys::VirtV2V::Connection::LibVirt::ISA = qw(Sys::VirtV2V::Connection);

=pod

=head1 NAME

Sys::VirtV2V::Connection::LibVirt - Read libvirt metadata from libvirtd

=head1 SYNOPSIS

 use Sys::VirtV2V::Connection::LibVirt;

 $conn = Sys::VirtV2V::Connection::LibVirt->new
    ("xen+ssh://xenserver.example.com/", $name, $target);
 $dom = $conn->get_dom();

=head1 DESCRIPTION

Sys::VirtV2V::Connection::LibVirt is an implementation of
Sys::VirtV2V::Connection which reads a guest's libvirt XML directly from a
libvirt connection.

=head1 METHODS

=over

=item new(uri, name, target)

Create a new Sys::VirtV2V::Connection::LibVirt. Domain I<name> will be
obtained from I<uri>. Remote storage will be create on I<target>.

=cut

sub new
{
    my $class = shift;

    my ($uri, $name, $target) = @_;

    my $self = {};

    bless($self, $class);

    $self->{uri} = URI->new($uri);
    $self->{name} = $name;

    # Parse uri authority for hostname and username
    $self->{uri}->authority() =~ /^(?:([^:]*)(?::([^@]*))?@)?(.*)$/
        or die(user_message(__x("Unable to parse URI authority: {auth}",
                                auth => $self->{uri}->authority())));

    my $username = $self->{username} = $1;
    my $hostname = $self->{hostname} = $3;

    print STDERR user_message(__("WARNING: Specifying a password in the ".
                                 "connection URI is not supported. It has ".
                                 "been ignored.")) if (defined($2));

    # Look for credentials in .netrc if the URI contains a hostname
    if (defined($hostname)) {
        if (defined($username)) {
            my $mach = Net::Netrc->lookup($hostname, $username);
            $self->{password} = $mach->password if (defined($mach));
        }

        else {
            my $mach = Net::Netrc->lookup($hostname);

            if (defined($mach)) {
                $self->{username} = $mach->login;
                $self->{password} = $mach->password;
            }
        }
    }

    my $sourcevmm;
    eval {
        $sourcevmm = Sys::Virt->new(
            uri => $uri,
            readonly => 1,
            auth => 1,
            credlist => [
                Sys::Virt::CRED_AUTHNAME,
                Sys::Virt::CRED_PASSPHRASE
            ],
            callback => sub {
                my $creds = shift;

                foreach my $cred (@$creds) {
                    if ($cred->{type} == Sys::Virt::CRED_AUTHNAME) {
                        $cred->{result} = $self->{username};
                    }

                    elsif ($cred->{type} == Sys::Virt::CRED_PASSPHRASE) {
                        $cred->{result} = $self->{password};
                    }

                    else { die($cred->{type}, "\n"); }
                }
            }
        );
    };
    die(user_message(__x("Failed to connect to {uri}: {error}",
                         uri => $uri,
                         error => $@->stringify()))) if ($@);

    $self->{sourcevmm} = $sourcevmm;

    $self->_check_shutdown();

    $self->_get_dom();

    my $transfer;
    if ($self->{uri}->scheme eq "esx") {
        $transfer = "Sys::VirtV2V::Transfer::ESX";
    }

    # Default to LocalCopy
    # XXX: Need transfer methods for remote libvirt connections, e.g. scp
    else {
        $transfer = "Sys::VirtV2V::Transfer::LocalCopy";
    }

    $self->_storage_iterate($transfer, $target);

    return $self;
}

sub _check_shutdown
{
    my $self = shift;

    my $vmm = $self->{vmm};
    my $domain = $self->_get_domain();

    # Check the domain is shutdown
    die(user_message(__x("Guest {name} is currently {state}. It must be ".
                         "shut down first.",
                         state => _state_string($domain->get_info()->{state}),
                         name => $self->{name})))
        unless ($domain->get_info()->{state} ==
                Sys::Virt::Domain::STATE_SHUTOFF);
}

sub _state_string
{
    my ($state) = @_;

    if ($state == Sys::Virt::Domain::STATE_NOSTATE) {
        return __"idle";
    } elsif ($state == Sys::Virt::Domain::STATE_RUNNING) {
        return __"running";
    } elsif ($state == Sys::Virt::Domain::STATE_BLOCKED) {
        return __"blocked";
    } elsif ($state == Sys::Virt::Domain::STATE_PAUSED) {
        return __"paused";
    } elsif ($state == Sys::Virt::Domain::STATE_SHUTDOWN) {
        return __"shutting down";
    } elsif ($state == Sys::Virt::Domain::STATE_SHUTOFF) {
        return __"shut down";
    } elsif ($state == Sys::Virt::Domain::STATE_CRASHED) {
        return __"crashed";
    } else {
        return "unknown state ($state)";
    }
}

sub _get_domain
{
    my $self = shift;

    return $self->{domain} if (defined($self->{domain}));

    my $vmm = $self->{sourcevmm};
    my $name = $self->{name};

    # Lookup the domain
    my $domain;
    eval {
        $domain = $vmm->get_domain_by_name($name);
    };
    die($@) if ($@);

    # Check we found it
    die(user_message(__x("{name} isn't a valid guest name", name => $name)))
        unless($domain);

    $self->{domain} = $domain;

    return $domain;
}

sub _get_dom
{
    my $self = shift;

    my $vmm = $self->{vmm};
    my $name = $self->{name};

    # Lookup the domain
    my $domain = $self->_get_domain();

    # Warn and exit if we didn't find it
    return undef unless(defined($domain));

    my $xml = $domain->get_xml_description();

    my $dom = new XML::DOM::Parser->parse($xml);
    $self->{dom} = $dom;
}

=back

=head1 COPYRIGHT

Copyright (C) 2009,2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Connection(3)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
