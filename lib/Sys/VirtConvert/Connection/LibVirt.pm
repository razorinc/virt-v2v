# Sys::VirtConvert::Connection::LibVirt
# Copyright (C) 2009-2011 Red Hat Inc.
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

package Sys::VirtConvert::Connection::LibVirt;

use strict;
use warnings;

use Net::Netrc;
use URI;
use XML::DOM;

use Sys::Virt;

use Sys::VirtConvert;
use Sys::VirtConvert::Transfer::ESX;
use Sys::VirtConvert::Transfer::SSH;
use Sys::VirtConvert::Transfer::Local;
use Sys::VirtConvert::Util;

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtConvert::Connection::LibVirt - Access storage and metadata from libvirt

=head1 DESCRIPTION

Do not use C<Sys::VirtConvert::Connection::LibVirt> directly. Instead use either
C<Sys::VirtConvert::Connection::LibVirtSource> or
C<Sys::VirtConvert::Connection::LibVirtTarget>.

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
        or v2vdie __x('Unable to parse URI authority: {auth}',
                      auth => $uri->authority());

    logmsg WARN, __('Specifying a password in the connection URI '.
                    'is not supported. It has been ignored.') if defined($2);

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
    v2vdie __x('Failed to connect to {uri}: {error}',
               uri => $uri, error => $@->stringify()) if $@;

    $self->{vmm} = $vmm;

    return $self;
}

sub _get_transfer
{
    my $self = shift;
    my ($path, $is_block, $format, $is_sparse) = @_;

    my $uri = $self->{uri};

    if ($uri->scheme eq "esx") {
        my %query = $uri->query_form;
        my $noverify = $query{no_verify} eq "1" ? 1 : 0;

        return new Sys::VirtConvert::Transfer::ESX($path,
                                                   $self->{hostname},
                                                   $self->{username},
                                                   $self->{password},
                                                   $noverify,
                                                   $is_sparse);
    }

    elsif ($uri->scheme =~ /\+ssh$/) {
        return new Sys::VirtConvert::Transfer::SSH($path, $format,
                                                   $self->{hostname},
                                                   $self->{username},
                                                   $is_sparse);
    }

    # Default to Local
    return new Sys::VirtConvert::Transfer::Local($path, $is_block,
                                                 $format, $is_sparse);
}

=head1 COPYRIGHT

Copyright (C) 2009,2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtConvert::Connection::LibVirtSource(3)>,
L<Sys::VirtConvert::Connection::LibVirtTarget(3)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
