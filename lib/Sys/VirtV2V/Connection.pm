# Sys::VirtV2V::Connection
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

package Sys::VirtV2V::Connection;

use strict;
use warnings;

use Sys::Virt;

use Sys::VirtV2V::Transfer::ESX;
use Sys::VirtV2V::Transfer::LocalCopy;
use Sys::VirtV2V::UserMessage qw(user_message);

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::Connection - Obtain domain metadata

=head1 SYNOPSIS

 use Sys::VirtV2V::Connection::LibVirt;

 $conn = Sys::VirtV2V::Connection::LibVirt->new($uri, $name, $target);
 $dom = $conn->get_dom();
 $storage = $conn->get_storage_paths();
 $devices = $conn->get_storage_devices();

=head1 DESCRIPTION

Sys::VirtV2V::Connection describes a connection to a, possibly remote, source of
guest metadata and storage. It is a virtual superclass and can't be instantiated
directly. Use one of the subclasses:

 Sys::VirtV2V::Connection::LibVirt
 Sys::VirtV2V::Connection::LibVirtXML

=head1 METHODS

=over

=item get_storage_paths

Return an arrayref of local paths to the guest's storage devices. This list is
guaranteed to be in the same order as the list returned by get_storage_devices.

=cut

sub get_storage_paths
{
    my $self = shift;

    return $self->{paths};
}

=item get_storage_devices

Return an arrayref of libvirt device names for the guest's storage prior to
conversion. This list is guaranteed to be in the same order as the list returned
by get_storage_paths.

=cut

sub get_storage_devices
{
    my $self = shift;

    return $self->{devices};
}

=item get_dom()

Returns an XML::DOM::Document describing a libvirt configuration equivalent to
the input.

Returns undef and displays an error if there was an error

=cut

sub get_dom
{
    my $self = shift;

    return $self->{dom};
}


# Iterate over returned storage. Transfer it and update DOM as necessary. To be
# called by subclasses.
sub _storage_iterate
{
    my $self = shift;

    my ($transfer, $target) = @_;

    my $dom = $self->get_dom();

    # An list of local paths to guest storage
    my @paths;
    # A list of libvirt target device names
    my @devices;
    foreach my $disk ($dom->findnodes('/domain/devices/disk')) {
        my ($source_e) = $disk->findnodes('source');

        my ($source) = $source_e->findnodes('@file | @dev');
        defined($source) or die("source element has neither dev nor file: \n".
                                $dom->toString());

        my ($dev) = $disk->findnodes('target/@dev');
        defined($dev) or die("disk does not have a target device: \n".
                                $dom->toString());

        # If the disk is a floppy or a cdrom, blank its source
        my $device = $disk->getAttribute('device');
        if ($device eq 'floppy' || $device eq 'cdrom') {
            $source_e->setAttribute($source->getName(), '');
        }

        else {
            my $path = $source->getValue();

            # Die if transfer required and no output target
            die (user_message(__"No output target was specified"))
                unless (defined($target));

            # Fetch the remote storage
            my $vol = $transfer->transfer($self, $path, $target);

            # Export the new path
            $path = $vol->get_path();

            # Find any existing driver element.
            my ($driver) = $disk->findnodes('driver');

            # Create a new driver element if none exists
            unless (defined($driver)) {
                $driver =
                    $disk->getOwnerDocument()->createElement("driver");
                $disk->appendChild($driver);
            }
            $driver->setAttribute('name', 'qemu');
            $driver->setAttribute('type', $vol->get_format());

            # Remove the @file or @dev attribute before adding a new one
            $source_e->removeAttributeNode($source);

            # Set @file or @dev as appropriate
            if ($vol->is_block())
            {
                $disk->setAttribute('type', 'block');
                $source_e->setAttribute('dev', $path);
            } else {
                $disk->setAttribute('type', 'file');
                $source_e->setAttribute('file', $path);
            }

            push(@paths, $path);
            push(@devices, $dev->getNodeValue());
        }
    }

    $self->{paths} = \@paths;
    $self->{devices} = \@devices;
}

=back

=head1 COPYRIGHT

Copyright (C) 2009,2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Connection::LibVirt(3pm)>,
L<Sys::VirtV2V::Connection::LibVirtXML(3pm)>,
L<virt-v2v(1)>,
L<v2v-snapshot(1)>,
L<http://libguestfs.org/>.

=cut

1;
