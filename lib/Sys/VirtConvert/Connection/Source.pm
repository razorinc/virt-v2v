# Sys::VirtConvert::Connection::Source
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

package Sys::VirtConvert::Connection::Source;

use strict;
use warnings;

use Sys::Virt;
use Term::ProgressBar;

use Sys::VirtConvert::Util;

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtConvert::Connection - A source connection

=head1 SYNOPSIS

 use Sys::VirtConvert::Connection::LibVirtSource;

 $conn = Sys::VirtConvert::Connection::LibVirtSource->new($uri, $name, $target);
 $meta = $conn->get_meta();

=head1 DESCRIPTION

Sys::VirtConvert::Source provides access to a source of guest metadata and
storage.  It is a virtual superclass and can't be instantiated directly. Use one
of the subclasses:

 Sys::VirtConvert::Target::LibVirtSource
 Sys::VirtConvert::Source::LibVirtXML

=head1 METHODS

=over

=item get_meta()

Return guest metadata.

Returns undef and displays an error if there was an error.

=cut

sub get_meta
{
    my $self = shift;

    return $self->{meta};
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

    # Initialize a progress bar if STDERR is on a tty
    my $progress;
    if (-t STDERR) {
        $progress = new Term::ProgressBar({name => $src->get_name(),
                                           count => $expected,
                                           ETA => 'linear' });
    } else {
        logmsg INFO, __x('Transferring storage volume {name}: {size} bytes',
                         name => $src->get_name(), size => $expected);
    }

    my $next_update = 0;
    for (;;) {
        my $buf = $src_s->read(4 * 1024 * 1024);
        last if (length($buf) == 0);

        $total += length($buf);
        $dst_s->write($buf);

        $next_update = $progress->update($total)
            if (defined($progress) && $total > $next_update);
    }
    if (defined($progress)) {
        # Indicate that we finished regardless of how much data was written
        $progress->update($expected);
        # The progress bar doesn't print a newline on completion
        print STDERR "\n";
    }

    # This would be closed implicitly, but we want to report read/write errors
    # before checking for a short volume
    $src_s->close();
    $dst_s->close();

    # Sanity check that we received all bytes of a raw volume
    # TODO: Add similar check for other formats. How much data do we expect to
    #       receive for a partially allocated qcow2 volume?
    v2vdie __x('Didn\'t receive full volume. Received {received} '.
               'of {total} bytes.',
               received => $total, total => $src->get_size())
        if  $src->get_format() eq "raw" && $total < $src->get_size();

    return $dst;
}

=item copy_storage(target, format, is_sparse)

Copy all of a guests storage devices to I<target>. Update the guest metadata to
reflect their new locations and properties.

=cut

sub copy_storage
{
    my $self = shift;
    my ($target, $output_format, $output_sparse) = @_;

    my $meta = $self->get_meta();

    foreach my $disk (@{$meta->{disks}}) {
        my $src = $self->get_volume($disk->{path});
        my $dst;
        if ($target->volume_exists($src->get_name())) {
            logmsg WARN, __x('Storage volume {name} already exists on the '.
                             'target. NOT copying it again. Delete the volume '.
                             'and retry to copy again.',
                             name => $src->get_name());
            $dst = $target->get_volume($src->get_name());
        } else {
            $dst = $target->create_volume(
                $src->get_name(),
                defined($output_format) ?  $output_format : $src->get_format(),
                $src->get_size(),
                defined($output_sparse) ? $output_sparse : $src->is_sparse()
            );

            # This will die if libguestfs can't use the result directly, so we
            # do it before copying all the data.
            $disk->{local_path} = $dst->get_local_path();

            _volume_copy($src, $dst);
        }

        # Update the volume path to point to the copy
        $disk->{path} = $dst->get_path();
        $disk->{is_block} = $dst->is_block();
    }
}

=back

=head1 COPYRIGHT

Copyright (C) 2009-2011 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtConvert::Source::LibVirt(3pm)>,
L<Sys::VirtConvert::Source::LibVirtXML(3pm)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
