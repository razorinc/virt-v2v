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

package Sys::VirtConvert::Connection::LibVirtSource;

use strict;
use warnings;

use URI;
use XML::DOM::XPath;

use Sys::Virt;

use Sys::VirtConvert;
use Sys::VirtConvert::Connection::Source;
use Sys::VirtConvert::Connection::LibVirt;
use Sys::VirtConvert::Connection::Volume;
use Sys::VirtConvert::Transfer::ESX;
use Sys::VirtConvert::Util qw(:DEFAULT parse_libvirt_volinfo);

use Locale::TextDomain 'virt-v2v';

@Sys::VirtConvert::Connection::LibVirtSource::ISA =
    qw(Sys::VirtConvert::Connection::Source
       Sys::VirtConvert::Connection::LibVirt);

=pod

=head1 NAME

Sys::VirtConvert::Connection::LibVirtSource - Get storage and metadata from
libvirt

=head1 SYNOPSIS

 use Sys::VirtConvert::Connection::LibVirtSource;

 $conn = Sys::VirtConvert::Connection::LibVirtSource->new
    ("xen+ssh://xenserver.example.com/", $name);
 $meta = $conn->get_meta();

=head1 DESCRIPTION

Sys::VirtConvert::Connection::LibVirtSource reads a guest's libvirt XML directly
from a libvirt connection. It accesses the guest's storage over the same
transport as the libvirt connection.

=head1 METHODS

=over

=item new(uri, name)

Create a new libvirt source connection. Domain I<name> will be obtained from
I<uri>.

=cut

sub new
{
    my $class = shift;
    my ($uri, $name) = @_;

    my $self = $class->SUPER::_libvirt_new($uri);

    $self->{name} = $name;

    $self->_check_shutdown();
    $self->_get_meta();

    return $self;
}

=item get_name

Return the name of the domain.

=cut

sub get_name
{
    return shift->{name};
}

=item get_volume(path)

Return a volume object for I<path>, where I<path> is the path of a volume on the
connected hypervisor.

=cut

sub get_volume
{
    my $self = shift;
    my ($path) = @_;

    my $uri = $self->{uri};

    my ($name, $format, $size, $usage, $is_sparse, $is_block);

    # The libvirt storage APIs aren't yet reliably implemented for ESX, so we
    # need to get volume metadata some other way
    my $transfer;
    if ($uri->scheme eq "esx") {
        $format = "raw";

        $transfer = $self->_get_transfer(undef, $path, $format, 0, 0);

        $name = $transfer->esx_get_name();
        $size = $transfer->esx_get_size();
        $usage = $size;
        $is_sparse = 0;
        $is_block = 0;
    }

    else {
        my $vol;
        eval {
            $vol = $self->{vmm}->get_storage_volume_by_path($path);
        };
        if ($@) {
            if ($@->code == Sys::Virt::Error->ERR_NO_STORAGE_VOL) {
                v2vdie __x(
                    "Failed to retrieve volume information for {path}. This ".
                    "could be because the volume doesn't exist, or because ".
                    "the volume exists but is not contained in a storage ".
                    "pool.\n\n".
                    "In the latter case, you must create a storage pool of ".
                    "the correct type to contain the volume. Note that you ".
                    "do not have to re-create or move the volume itself, only ".
                    "define a pool which contains it. libvirt will ".
                    "automatically detect the volume when it scans the pool ".
                    "after creation.\n\n".
                    "virt-manager is able to create storage pools. Select ".
                    "Edit->Connection Details from the application menu. ".
                    "Storage pools are displayed in the Storage tab.",
                    path => $path);
            } else {
                v2vdie __x('Failed to retrieve storage volume {path}: {error}',
                           path => $path, error => $@->stringify());
            }
        }

        ($name, $format, $size, $usage, $is_sparse, $is_block) =
            parse_libvirt_volinfo($vol);

        $transfer = $self->_get_transfer($name, $path, $format,
                                         $is_sparse, $is_block);
    }

    return new Sys::VirtConvert::Connection::Volume($name, $format, $path,
                                                    $size, $usage,
                                                    $is_sparse, $is_block,
                                                    $transfer);
}

sub _check_shutdown
{
    my $self = shift;

    my $vmm = $self->{vmm};
    my $domain = $self->_get_domain();

    # Check the domain is shutdown
    v2vdie __x('Guest {name} is currently {state}. It must be shut down first.',
               state => _state_string($domain->get_info()->{state}),
               name => $self->{name})
        unless $domain->get_info()->{state} == Sys::Virt::Domain::STATE_SHUTOFF;
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

    my $vmm = $self->{vmm};
    my $name = $self->{name};

    # Lookup the domain
    my $domain;
    eval {
        $domain = $vmm->get_domain_by_name($name);
    };
    die($@) if ($@);

    # Check we found it
    v2vdie __x('{name} isn\'t a valid guest name', name => $name)
        unless $domain;

    $self->{domain} = $domain;

    return $domain;
}

sub _get_meta
{
    my $self = shift;

    my $vmm = $self->{vmm};
    my $name = $self->{name};

    # Lookup the domain
    my $domain = $self->_get_domain();

    # Warn and exit if we didn't find it
    return undef unless(defined($domain));

    my $dom = new XML::DOM::Parser->parse($domain->get_xml_description(1));
    $self->{meta} = Sys::VirtConvert::Connection::LibVirt::_parse_dom($self,
                                                                      $dom);
}

=back

=head1 COPYRIGHT

Copyright (C) 2009-2011 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtConvert::Connection::Source(3)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
