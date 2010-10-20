# Sys::VirtV2V::SparseWriter
# Copyright (C) 2010 Red Hat Inc.
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

package Sys::VirtV2V::SparseWriter;

use strict;
use warnings;

use Fcntl qw(:seek);
use File::stat;

use Sys::VirtV2V::Util qw(user_message);
use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::SparseWriter - Write sparse files

=head1 METHODS

=over

=item new (path)

Create a new SparseWriter for writing to I<path>.

=cut

sub new
{
    my $class = shift;
    my ($path) = @_;

    my $self = {};
    bless($self, $class);

    my $out;
    open($out, '>', $path)
        or die(user_message(__x("Failed to open {path}: {error}",
                                path => $path,
                                error => $!)));
    binmode($out);

    # Stat the output handle to get its block size
    my $st = stat($out);

    $self->{path} = $path;
    $self->{out} = $out;
    $self->{blksize} = $st->blksize;
    $self->{sparse} = 0;
    $self->{usage} = 0;
    $self->{inbuf} = '';

    return $self;
}

=item write (data)

Write I<data> to the sparse file.

=cut

sub write
{
    my $self = shift;
    my ($data) = @_;

    $self->{inbuf} .= $data;

    my $inbufref = \$self->{inbuf};
    my $blksize = $self->{blksize};
    my $out = $self->{out};
    my $path = $self->{path};

    my $align = length($$inbufref) % $blksize;

    my $allocstart = 0;
    my $alloc = 0;

    # Process data in the input buffer in $blksize chunks, excluding any
    # data in an incomplete final block
    for (my $i = 0; $i + $align < length($$inbufref); $i += $blksize) {
        if (substr($$inbufref, $i, $blksize) =~ /[^\0]/) { # Allocated
            # Seek past any sparse section before writing
            if ($self->{sparse} > 0) {
                seek($out, $self->{sparse}, SEEK_CUR)
                    or die(user_message(__x("Seek on {path} failed: ".
                                            "{error}",
                                            path => $path,
                                            error => $!)));
                $self->{sparse} = 0;
                $allocstart = $i;
            }

            $alloc += $blksize;
        }

        else { # Sparse
            # Flush pending data before start of sparse section
            if ($alloc > 0) {
                print $out substr($$inbufref, $allocstart, $alloc)
                    or die(user_message(__x("Error writing to {path}: {error}",
                                            path => $path,
                                            error => $!)));
                $alloc = 0;
                $self->{usage} += $alloc;
            }

            $self->{sparse} += $blksize;
        }
    }

    # Flush any identified allocated data from the input buffer
    if ($alloc > 0) {
        print $out substr($$inbufref, $allocstart, $alloc)
            or die(user_message(__x("Error writing to {path}: {error}",
                                    path => $path,
                                    error => $!)));
        $self->{usage} += $alloc;
    }

    if ($align == 0) {
        $$inbufref = '';
    } else {
        $$inbufref =  substr($$inbufref, length($$inbufref) - $align);
    }
}

=item close

Close the sparse file.

=cut

sub close
{
    my $self = shift;

    my $inbufref = \$self->{inbuf};
    my $out = $self->{out};
    my $path = $self->{path};

    # Check if the remaining input buffer is sparse
    if (length($$inbufref) > 0 && $$inbufref !~ /[^\0]/) {
        $self->{sparse} += length($$inbufref);
        $$inbufref = '';
    }

    # If there's data left in the input buffer, write it out
    if (length($$inbufref) > 0) {
        seek($out, $self->{sparse}, SEEK_CUR)
            or die(user_message(__x("Seek on {path} failed: {error}",
                                    path => $path, error => $!)))
            if ($self->{sparse} > 0);

        print $out $$inbufref
            or die(user_message(__x("Error writing to {path}: {error}",
                                    path => $path,
                                    error => $!)));

        $self->{usage} += length($$inbufref);
    }

    elsif ($self->{sparse} > 0) {
        truncate($out, tell($out) + $self->{sparse})
            or die(user_message(__x("Error truncating {path}: {error}",
                                    path => $path,
                                    error => $!)));
    }

    close($out) or die(user_message(__x("Error closing {path}: {error}",
                                        path => $path, error => $!)));

    # Ensure DESTROY won't attempt to close again
    delete($self->{out});
}

=item get_usage

Return the disk usage of the sparse file, in bytes.

=cut

sub get_usage
{
    return shift->{usage};
}

sub DESTROY
{
    my $self = shift;

    $self->close() if (exists($self->{out}));
}

=back

=head1 COPYRIGHT

Copyright (C) 2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
