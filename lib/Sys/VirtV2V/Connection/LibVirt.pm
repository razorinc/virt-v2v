# Sys::VirtV2V::Connection::LibVirt
# Copyright (C) 2009,2010 Red Hat Inc.
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

use Net::Netrc;
use URI;
use XML::DOM;

use Sys::Virt;

use Sys::VirtV2V;
use Sys::VirtV2V::Connection;
use Sys::VirtV2V::Transfer::ESX;
use Sys::VirtV2V::Transfer::SSH;
use Sys::VirtV2V::Transfer::Local;
use Sys::VirtV2V::Util qw(user_message);

use Locale::TextDomain 'virt-v2v';

@Sys::VirtV2V::Connection::LibVirt::ISA = qw(Sys::VirtV2V::Connection);

=pod

=head1 NAME

Sys::VirtV2V::Connection::LibVirt - Access storage and metadata from libvirt

=head1 DESCRIPTION

Do not use C<Sys::VirtV2V::Connection::LibVirt> directly. Instead use either
C<Sys::VirtV2V::Connection::LibVirtSource> or
C<Sys::VirtV2V::Connection::LibVirtTarget>.

=cut

sub _libvirt_new
{
    my $class = shift;
    my ($uri) = @_;

    my $self = {};
    bless($self, $class);

    $self->{uri} = $uri = URI->new($uri);

    # Parse uri authority for hostname and username
    $uri->authority() =~ /^(?:([^:]*)(?::([^@]*))?@)?(.*)$/
        or die(user_message(__x("Unable to parse URI authority: {auth}",
                                auth => $uri->authority())));

    warn user_message(__"WARNING: Specifying a password in the connection URI ".
                        "is not supported. It has been ignored.")
        if (defined($2));

    $self->{username} = $1;
    $self->{hostname} = $3;

    # Look for credentials in .netrc if the URI contains a hostname
    if (defined($self->{hostname})) {
        if (defined($self->{username})) {
            my $mach = Net::Netrc->lookup($self->{hostname}, $self->{username});
            $self->{password} = $mach->password if (defined($mach));
        }

        else {
            my $mach = Net::Netrc->lookup($self->{hostname});

            if (defined($mach)) {
                $self->{username} = $mach->login;
                $self->{password} = $mach->password;
            }
        }
    }

    my $vmm;
    eval {
        $vmm = Sys::Virt->new(
            uri => $uri,
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

    $self->{vmm} = $vmm;

    return $self;
}

sub _get_transfer
{
    my $self = shift;
    my ($path, $format, $is_sparse) = @_;

    my $uri = $self->{uri};

    if ($uri->scheme eq "esx") {
        my %query = $uri->query_form;
        my $noverify = $query{no_verify} eq "1" ? 1 : 0;

        return new Sys::VirtV2V::Transfer::ESX($path,
                                               $self->{hostname},
                                               $self->{username},
                                               $self->{password},
                                               $noverify,
                                               $is_sparse);
    }

    elsif ($uri->scheme =~ /\+ssh$/) {
        return new Sys::VirtV2V::Transfer::SSH($path,
                                               $self->{hostname},
                                               $self->{username},
                                               $is_sparse);
    }

    # Default to Local
    return new Sys::VirtV2V::Transfer::Local($path, $format, $is_sparse);
}

=head1 COPYRIGHT

Copyright (C) 2009,2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Connection(3)>,
L<Sys::VirtV2V::Connection::LibVirtSource(3)>,
L<Sys::VirtV2V::Connection::LibVirtTarget(3)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
