# Sys::VirtV2V::Connection::Source
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

package Sys::VirtV2V::Connection::Source;

use strict;
use warnings;

use Sys::Virt;
use Term::ProgressBar;

use Sys::VirtV2V::Util qw(user_message);

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::Connection - Obtain domain metadata

=head1 SYNOPSIS

 use Sys::VirtV2V::Connection::LibVirtSource;

 $conn = Sys::VirtV2V::Connection::LibVirtSource->new($uri, $name, $target);
 $dom = $conn->get_dom();
 $storage = $conn->get_storage_paths();
 $devices = $conn->get_storage_devices();

=head1 DESCRIPTION

Sys::VirtV2V::Source provides access to a source of guest metadata and storage.
It is a virtual superclass and can't be instantiated directly. Use one of the
subclasses:

 Sys::VirtV2V::Target::LibVirtSource
 Sys::VirtV2V::Source::LibVirtXML

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

sub _volume_copy
{
    my ($src, $dst) = @_;

    my $src_s;
    my $dst_s;

    # Do we need to do format conversion?
    my $convert = $src->get_format() eq $dst->get_format() ? 0 : 1;
    $src_s = $src->get_read_stream($convert);
    $dst_s = $dst->get_write_stream($convert);

    my $expected;
    # We expect to read all data for raw volumes or volume we're converting to
    # raw
    if ($src->get_format() eq "raw" || $convert) {
        $expected = $src->get_size();
    }

    # ... but only allocated data for other formats
    else {
        $expected = $src->get_usage();
    }

    # Copy the contents of the source stream to the destination stream
    my $total = 0;
    my $progress = new Term::ProgressBar({name => $src->get_name(),
                                          count => $expected,
                                          ETA => 'linear' });
    my $next_update = 0;
    for (;;) {
        my $buf = $src_s->read(4 * 1024 * 1024);
        last if (length($buf) == 0);

        $total += length($buf);
        $dst_s->write($buf);

        $next_update = $progress->update($total) if ($total > $next_update);
    }
    # Indicate that we finished regardless of how much data was written
    $progress->update($expected);
    # The progress bar doesn't print a newline on completion
    print STDERR "\n";

    # This would be closed implicitly, but we want to report read/write errors
    # before checking for a short volume
    $src_s->close();
    $dst_s->close();

    # Sanity check that we received all bytes of a raw volume
    # TODO: Add similar check for other formats. How much data do we expect to
    #       receive for a partially allocated qcow2 volume?
    die(user_message(__x("Didn't receive full volume. Received {received} ".
                         "of {total} bytes.",
                         received => $total,
                         total => $src->get_size())))
        if ($src->get_format() eq "raw" && $total != $src->get_size());

    return $dst;
}

=item copy_storage(target)

Copy all of a guests storage devices to I<target>. Update the guest metadata to
reflect their new locations and properties.

=cut

sub copy_storage
{
    my $self = shift;
    my ($target, $output_format, $output_sparse) = @_;

    my $dom = $self->get_dom();

    # An list of local paths to guest storage
    my @paths;
    # A list of libvirt target device names
    my @devices;

    foreach my $disk ($dom->findnodes("/domain/devices/disk[\@device='disk']"))
    {
        my ($source_e) = $disk->findnodes('source');

        my ($source) = $source_e->findnodes('@file | @dev');
        defined($source) or die("source element has neither dev nor file: \n".
                                $dom->toString());

        my ($dev) = $disk->findnodes('target/@dev');
        defined($dev) or die("disk does not have a target device: \n".
                             $dom->toString());

        my $src = $self->get_volume($source->getValue());
        my $dst;
        if ($target->volume_exists($src->get_name())) {
            warn user_message(__x("WARNING: storage volume {name} already ".
                                  "exists on the target. NOT copying it ".
                                  "again. Delete the volume and retry to ".
                                  "copy again.",
                                  name => $src->get_name()));
            $dst = $target->get_volume($src->get_name());;
        } else {
            $dst = $target->create_volume(
                $src->get_name(),
                defined($output_format) ?  $output_format : $src->get_format(),
                $src->get_size(),
                defined($output_sparse) ? $output_sparse : $src->is_sparse()
            );

            _volume_copy($src, $dst);
        }

        # This will die if libguestfs can't use the result directly, so we do it
        # before copying all the data.
        push(@paths, $dst->get_local_path());

        # Export the new path
        my $path = $dst->get_path();

        # Find any existing driver element.
        my ($driver) = $disk->findnodes('driver');

        # Create a new driver element if none exists
        unless (defined($driver)) {
            $driver =
                $disk->getOwnerDocument()->createElement("driver");
            $disk->appendChild($driver);
        }
        $driver->setAttribute('name', 'qemu');
        $driver->setAttribute('type', $dst->get_format());

        # Remove the @file or @dev attribute before adding a new one
        $source_e->removeAttributeNode($source);

        # Set @file or @dev as appropriate
        if ($dst->is_block()) {
            $disk->setAttribute('type', 'block');
            $source_e->setAttribute('dev', $path);
        } else {
            $disk->setAttribute('type', 'file');
            $source_e->setAttribute('file', $path);
        }

        push(@devices, $dev->getNodeValue());
    }

    # Blank the source of floppies or cdroms
    foreach my $disk ($dom->findnodes('/domain/devices/disk'.
                                      "[\@device='floppy' or \@device='cdrom']"))
    {
        my ($source_e) = $disk->findnodes('source');

        # Nothing to do if there's no source element
        next unless (defined($source_e));

        # Blank file or dev as appropriate
        my ($source) = $source_e->findnodes('@file | @dev');
        defined($source) or die("source element has neither dev nor file: \n".
                                $dom->toString());

        $source_e->setAttribute($source->getName(), '');
    }

    die(user_message(__("Guest doesn't define any recognised storage devices")))
        unless (@paths > 0);

    $self->{paths} = \@paths;
    $self->{devices} = \@devices;
}

=back

=head1 COPYRIGHT

Copyright (C) 2009,2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Source::LibVirt(3pm)>,
L<Sys::VirtV2V::Source::LibVirtXML(3pm)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
