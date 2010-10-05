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
    open($fh, '>', $path)
        or die(user_message(__x("Unable to open {path} for writing: {error}",
                                path => $path,
                                error => $!)));

    $self->{path} = $path;
    $self->{fh} = $fh;
    $self->{is_sparse} = $is_sparse;

    return $self;
}

sub write
{
    my $self = shift;
    my ($buf) = @_;

    print { $self->{fh} } $buf or $self->_write_error($!);
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
    my ($path, $is_sparse) = @_;

    my $self = {};
    bless($self, $class);

    $self->{path}      = $path;
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

=item get_read_stream

Get a stream to receive data from a local file.

=cut

sub get_read_stream
{
    my $self = shift;

    return new Sys::VirtV2V::Transfer::Local::ReadStream(
        $self->{path},
        $self->{is_sparse}
    );
}

=item get_write_stream

Get a stream to write data to a local file.

=cut

sub get_write_stream
{
    my $self = shift;

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
