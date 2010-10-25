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
    my ($path, $is_sparse) = @_;

    my $self = {};
    bless($self, $class);

    my $fh;
    open($fh, '<', $path)
        or die(user_message(__x("Unable to open {path} for reading: {error}",
                                path => $path,
                                error => $!)));

    $self->{fh} = $fh;
    $self->{is_sparse} = $is_sparse;

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

use Sys::VirtV2V::Util qw(user_message);
use Locale::TextDomain 'virt-v2v';

sub new
{
    my $class = shift;
    my ($path, $is_sparse) = @_;

    my $self = {};
    bless($self, $class);

    my $fh;
    open($fh, '+<', $path)
        or die(user_message(__x("Unable to open {path} for writing: {error}",
                                path => $path,
                                error => $!)));

    $self->{path} = $path;
    $self->{fh} = $fh;
    $self->{is_sparse} = $is_sparse;
    $self->{usage} = 0;

    return $self;
}

sub write
{
    my $self = shift;
    my ($buf) = @_;

    print { $self->{fh} } $buf or $self->_write_error($!);
    $self->{usage} += length($buf);
}

sub get_usage
{
    return shift->{usage};
}

sub close
{
    my $self = shift;
    return unless (defined($self->{fh}));

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
    my ($path, $is_sparse) = @_;

    my $self = {};
    bless($self, $class);

    $self->{g} = new Sys::VirtV2V::GuestfsHandle([$path], undef, 0);
    $self->{is_sparse} = $is_sparse;

    return $self;
}

sub close
{
    my $self = shift;

    my $g = $self->{g};
    return unless (defined($g));
    delete($self->{g});

    $g->sync();
    $g->close();
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

use constant chunk => 2*1024*1024;

sub new
{
    my $self = shift->SUPER::new(@_);
    $self->{pos} = 0;
    $self->{buf} = '';

    return $self;
}

sub write
{
    my $self = shift;
    my ($data) = @_;

    my $bufref = \$self->{buf};

    $$bufref .= $data;

    my $split = chunk;
    while ($split <= length($$bufref)) {
        $self->{g}->pwrite_device('/dev/sda',
                                  substr($$bufref, $split - chunk, chunk),
                                  $self->{pos});
        $split += chunk;
        $self->{pos} += chunk;
    }

    if ($split > chunk) {
        $$bufref = substr($$bufref, $split - chunk);
    }
}

sub get_usage
{
    return shift->{pos};
}

sub close
{
    my $self = shift;

    my $g = $self->{g};
    return unless (defined($g));
    # Don't delete $self->{g} here because its used by SUPER

    my $bufref = \$self->{buf};

    # If the buffer is larger than a single chunk, which might happen if we
    # were interrupted during write(), flush the buffer first.
    $self->write('') if (length($$bufref) > chunk);

    $g->pwrite_device('/dev/sda', $$bufref, $self->{pos});
    $self->{pos} += length($$bufref);
    $$bufref = '';

    $self->SUPER::close();
}


package Sys::VirtV2V::Transfer::Local;

use POSIX;
use File::Spec;
use File::stat;

use Sys::VirtV2V::SparseWriter;
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
        $self->{format},
        $self->{is_sparse}
    ) if ($convert && $self->{format} ne 'raw');

    return new Sys::VirtV2V::Transfer::Local::ReadStream(
        $self->{path},
        $self->{is_sparse}
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

    return new Sys::VirtV2V::Transfer::Local::GuestfsWriteStream(
        $self->{path},
        $self->{format},
        $self->{is_sparse}
    ) if ($convert && $self->{format} ne 'raw');

    if ($self->{is_sparse}) {
        return new Sys::VirtV2V::SparseWriter($self->{path});
    } else {
        return new Sys::VirtV2V::Transfer::Local::WriteStream(
            $self->{path},
            $self->{is_sparse}
        );
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
