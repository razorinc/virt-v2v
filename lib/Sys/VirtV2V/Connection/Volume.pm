# Sys::VirtV2V::Connection::Volume
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

package Sys::VirtV2V::Connection::Volume;

use strict;
use warnings;

use Data::Dumper;

use Sys::VirtV2V::Util qw(user_message);

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::Connection::Volume - Read and write storage volumes

=head1 METHODS

=over

=item new(name, format, path, size, usage, is_block, transfer)

Create a new Volume which returns the given metadata, and uses I<transfer> for
transferring data.

=cut

sub new
{
    my $class = shift;
    my ($name, $format, $path, $size, $usage,
        $is_sparse, $is_block, $transfer) = @_;

    my $self = {};
    bless($self, $class);

    $self->{name}      = $name;
    $self->{format}    = $format;
    $self->{path}      = $path;
    $self->{size}      = $size;
    $self->{usage}     = $usage;
    $self->{is_sparse} = $is_sparse;
    $self->{is_block}  = $is_block;
    $self->{transfer}  = $transfer;

    return $self;
}

=item get_size

Return size for this Volume.

=cut

sub get_size
{
    return shift->{size};
}

=item get_usage

Return the disk usage for this Volume.

=cut

sub get_usage
{
    return shift->{usage};
}

=item get_format

Return the on-disk format of this Volume.

=cut

sub get_format
{
    return shift->{format};
}

=item get_name

Return the name of this Volume.

=cut

sub get_name
{
    return shift->{name};
}

=item get_path

Return the native path of this Volume.

=cut

sub get_path
{
    return shift->{path};
}

=item get_local_path

=cut

sub get_local_path
{
    return shift->{transfer}->local_path();
}

=item is_block

Return 1 if the volume is stored directly on a block device, 0 otherwise.

=cut

sub is_block
{
    return shift->{is_block};
}

=item is_sparse

Return 1 if the volume is not fully allocated, 0 otherwise.

=cut

sub is_sparse
{
    return shift->{is_sparse};
}

=item get_read_stream

Return a ReadStream for this volume.

=cut

sub get_read_stream
{
    return shift->{transfer}->get_read_stream();
}

=item get_write_stream

Return a WriteStream for this volume.

=cut

sub get_write_stream
{
    return shift->{transfer}->get_write_stream();
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
