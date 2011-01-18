# Sys::VirtV2V::Transfer::Local
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

use strict;
use warnings;

package Sys::VirtV2V::Transfer::Local::ReadStream;

use Sys::VirtV2V::Util qw(user_message);
use Locale::TextDomain 'virt-v2v';

sub new
{
    my $class = shift;
    my ($path) = @_;

    my $self = {};
    bless($self, $class);

    my $fh;
    open($fh, '<', $path)
        or die(user_message(__x("Unable to open {path} for reading: {error}",
                                path => $path,
                                error => $!)));

    $self->{fh} = $fh;

    return $self;
}

sub read
{
    my $self = shift;
    my ($size) = @_;

    my $buf;
    my $in = read($self->{fh}, $buf, $size);
    $self->_read_error($!) unless (defined($in));

    return "" if ($in == 0);
    return $buf;
}

sub close
{
    my $self = shift;
    return unless(defined($self->{fh}));

    close($self->{fh}) or $self->_read_error($!);
    delete($self->{fh});
}

sub DESTROY
{
    my $self = shift;
    $self->close();
}

sub _read_error
{
    my $self = shift;
    my ($error) = @_;

    die(user_message(__x("Error reading from {path}: {error}",
                         path => $self->{path},
                         error => $error)));
}


package Sys::VirtV2V::Transfer::Local::WriteStream;

use Fcntl qw(:seek);
use File::stat;

use Sys::VirtV2V::Util qw(user_message);
use Locale::TextDomain 'virt-v2v';

sub new
{
    my $class = shift;
    my ($path) = @_;

    my $self = {};
    bless($self, $class);

    my $fh;
    open($fh, '+<', $path)
        or die(user_message(__x("Unable to open {path} for writing: {error}",
                                path => $path,
                                error => $!)));
    binmode($fh);

    $self->{path} = $path;
    $self->{fh} = $fh;
    $self->{usage} = 0;
    $self->{truncate} = 0;

    return $self;
}

sub write
{
    my $self = shift;
    my ($buf) = @_;

    print { $self->{fh} } $buf or $self->_write_error($!);
    $self->{usage} += length($buf);

    $self->{truncate} = 0;
}

sub _write_zeroes
{
    my $self = shift;
    my ($size) = @_;

    seek($self->{fh}, $size, SEEK_CUR) or $self->_write_error($!);
    $self->{truncate} = 1;
}

sub _get_blocksize
{
    my $self = shift;

    my $st = stat($self->{fh});
    return $st->blksize;
}

sub get_usage
{
    return shift->{usage};
}

sub close
{
    my $self = shift;
    return unless (defined($self->{fh}));

    # If we have seek()ed to the end of the file without writing anything, the
    # file size won't have been correctly updated. In this case we need to
    # truncate the file to the current file handle position.
    truncate($self->{fh}, tell($self->{fh}))
        or $self->_write_error($!)
        if ($self->{truncate});

    close($self->{fh}) or $self->_write_error($!);
    delete($self->{fh});
}

sub _write_error
{
    my $self = shift;
    my ($error) = @_;

    die(user_message(__x("Error writing to {path}: {error}",
                         path => $self->{path},
                         error => $error)));
}

sub DESTROY
{
    my $self = shift;
    $self->close();
}


package Sys::VirtV2V::Transfer::GuestfsStream;

sub new
{
    my $class = shift;
    my ($path) = @_;

    my $self = {};
    bless($self, $class);

    $self->{g} = new Sys::VirtV2V::GuestfsHandle([$path], undef, 0);

    return $self;
}

sub close
{
    my $self = shift;

    my $g = $self->{g};
    return unless (defined($g));
    delete($self->{g});

    if ($g->is_alive()) {
        $g->sync();
        $g->close();
    }

    delete($self->{g});
}

sub DESTROY
{
    shift->close();
}


package Sys::VirtV2V::Transfer::Local::GuestfsReadStream;

@Sys::VirtV2V::Transfer::Local::GuestfsReadStream::ISA =
    qw(Sys::VirtV2V::Transfer::GuestfsStream);

sub new
{
    my $self = shift->SUPER::new(@_);
    $self->{pos} = 0;

    return $self;
}

sub read
{
    my $self = shift;
    my ($size) = @_;

    # Don't exceed the protocol limit on pread_device
    $size = 3*1024*1024 if ($size > 3*1024*1024);

    my $buf = $self->{g}->pread_device('/dev/sda', $size, $self->{pos});
    $self->{pos} += length($buf);

    return $buf;
}


package Sys::VirtV2V::Transfer::Local::GuestfsWriteStream;

@Sys::VirtV2V::Transfer::Local::GuestfsWriteStream::ISA =
    qw(Sys::VirtV2V::Transfer::GuestfsStream);

use constant chunksize => 2*1024*1024;

sub new
{
    my $self = shift->SUPER::new(@_);
    $self->{pos} = 0;
    $self->{usage} = 0;
    $self->{buf} = '';

    return $self;
}

sub write
{
    my $self = shift;
    my ($data) = @_;

    my $bufref = \$self->{buf};

    $$bufref .= $data;

    my $chunk = 0;
    while ($chunk + chunksize <= length($$bufref)) {
        my $written = $self->{g}->pwrite_device(
            '/dev/sda',
            substr($$bufref, $chunk, chunksize),
            $self->{pos}
        );
        $chunk += $written;
        $self->{pos} += $written;
        $self->{usage} += $written;
    }

    $$bufref = substr($$bufref, $chunk);
}

sub _flush
{
    my $self = shift;

    my $bufref = \$self->{buf};

    # If the buffer is larger than a single chunk, which might happen if we
    # were interrupted during write(), flush the buffer first.
    $self->write('') if (length($$bufref) > chunksize);

    my $remaining = length($$bufref);
    my $bufpos = 0;
    while ($remaining > 0) {
        my $written = $self->{g}->pwrite_device(
            '/dev/sda',
            substr($$bufref, $bufpos),
            $self->{pos}
        );
        $self->{pos} += $written;
        $bufpos += $written;
        $remaining -= $written;
    }

    $self->{usage} += length($$bufref);
    $self->{buf} = '';
}

sub _write_zeroes
{
    my $self = shift;
    my ($size) = @_;

    $self->_flush() if (length($self->{buf}) > 0);
    $self->{pos} += $size;
}

sub _get_blocksize
{
    # This is the default chunksize created by qemu-img create.
    return 64*1024;
}

sub get_usage
{
    return shift->{usage};
}

sub close
{
    my $self = shift;

    my $g = $self->{g};
    return unless (defined($g));
    # Don't delete $self->{g} here because its used by SUPER

    # If the daemon isn't running, for example because it was killed by the same
    # signal which is causing close() to be called here, there's no point in
    # trying to flush anything to it.
    $self->_flush() if ($g->is_alive());

    $self->{buf} = '';
    $self->SUPER::close();
}


package Sys::VirtV2V::Transfer::Local::SparseWriter;

sub new
{
    my $class = shift;
    my ($writer) = @_;

    my $self = {};
    bless($self, $class);

    $self->{writer} = $writer;
    $self->{blksize} = $writer->_get_blocksize();
    $self->{sparse} = 0;
    $self->{inbuf} = '';

    return $self;
}

sub write
{
    my $self = shift;
    my ($data) = @_;

    my $inbufref = \$self->{inbuf};

    $$inbufref .= $data;
    my $blksize = $self->{blksize};
    my $writer = $self->{writer};

    my $align = length($$inbufref) % $blksize;

    my $allocstart = 0;
    my $alloc = 0;

    # Process data in the input buffer in $blksize chunks, excluding any
    # data in an incomplete final block
    for (my $i = 0; $i < length($$inbufref) - $align; $i += $blksize) {
        if (substr($$inbufref, $i, $blksize) =~ /[^\0]/) { # Allocated
            # Seek past any sparse section before writing
            if ($self->{sparse} > 0) {
                $writer->_write_zeroes($self->{sparse});
                $self->{sparse} = 0;
                $allocstart = $i;
            }

            $alloc += $blksize;
        }

        else { # Sparse
            # Flush pending data before start of sparse section
            if ($alloc > 0) {
                $writer->write(substr($$inbufref, $allocstart, $alloc));
                $alloc = 0;
            }

            $self->{sparse} += $blksize;
        }
    }

    # Flush any identified allocated data from the input buffer
    $writer->write(substr($$inbufref, $allocstart, $alloc)) if ($alloc > 0);

    if ($align == 0) {
        $$inbufref = '';
    } else {
        $$inbufref =  substr($$inbufref, length($$inbufref) - $align);
    }
}

sub close
{
    my $self = shift;

    return unless (exists($self->{inbuf}));

    my $inbufref = \$self->{inbuf};
    my $writer = $self->{writer};

    # Check if the remaining input buffer is sparse
    if (length($$inbufref) > 0 && $$inbufref !~ /[^\0]/) {
        $self->{sparse} += length($$inbufref);
        $$inbufref = '';
    }

    # Ordering doesn't matter here, as it shouldn't be possible for both to be
    # true.

    # Write any remaining sparse section
    $writer->_write_zeroes($self->{sparse}) if ($self->{sparse} > 0);

    # Write any remaining data
    $writer->write($$inbufref) if (length($$inbufref) > 0);

    $writer->close();

    # Ensure DESTROY won't attempt to close again
    delete($self->{inbuf});
}

sub get_usage
{
    return shift->{writer}->get_usage();
}

sub DESTROY
{
    shift->close();
}


package Sys::VirtV2V::Transfer::Local;

use POSIX;
use File::Spec;
use File::stat;

use Sys::VirtV2V::Util qw(user_message);

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::Transfer::Local - Access local storage

=head1 METHODS

=over

=item new(path, is_sparse)

Create a new Local transfer object.

=cut

sub new
{
    my $class = shift;
    my ($path, $format, $is_sparse) = @_;

    my $self = {};
    bless($self, $class);

    $self->{path}      = $path;
    $self->{format}    = $format;
    $self->{is_sparse} = $is_sparse;

    return $self;
}

=item local_path

Return a local path to the file.

=cut

sub local_path
{
    return shift->{path};
}

=item get_read_stream(convert)

Get a ReadStream for this volume. Data will be converted to raw format if
I<convert> is 1.

=cut

sub get_read_stream
{
    my $self = shift;
    my ($convert) = @_;

    return new Sys::VirtV2V::Transfer::Local::GuestfsReadStream(
        $self->{path},
        $self->{format}
    ) if ($convert && $self->{format} ne 'raw');

    return new Sys::VirtV2V::Transfer::Local::ReadStream(
        $self->{path}
    );
}

=item get_write_stream(convert)

Get a WriteStream for this volume. Data will be converted from raw format if
I<convert> is 1.

=cut

sub get_write_stream
{
    my $self = shift;
    my ($convert) = @_;

    my $writer;
    if ($convert && $self->{format} ne 'raw') {
        $writer = new Sys::VirtV2V::Transfer::Local::GuestfsWriteStream(
            $self->{path},
            $self->{format}
        );
    } else {
        $writer = new Sys::VirtV2V::Transfer::Local::WriteStream(
            $self->{path}
        );
    }

    if ($self->{is_sparse}) {
        return new Sys::VirtV2V::Transfer::Local::SparseWriter($writer);
    } else {
        return $writer;
    }
}

=back

=head1 COPYRIGHT

Copyright (C) 2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Converter(3pm)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
