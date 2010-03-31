# Sys::VirtV2V::Transfer::LocalCopy
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

package Sys::VirtV2V::Transfer::LocalCopy;

use File::Spec;
use File::stat;

use Sys::VirtV2V::UserMessage qw(user_message);

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::Transfer::LocalCopy - Copy a guest's local storage

=head1 SYNOPSIS

 use Sys::VirtV2V::Transfer::LocalCopy;

 $vol = Sys::VirtV2V::Transfer::LocalCopy->transfer($conn, $path, $target);

=head1 DESCRIPTION

Sys::VirtV2V::Transfer::LocalCopy retrieves guest storage devices from local
storage.

=head1 METHODS

=over

=item transfer(conn, path, target)

Transfer <path> from local storage. Storage will be created using <target>.

=cut

sub transfer
{
    my $class = shift;

    my ($conn, $path, $target) = @_;

    my (undef, undef, $name) = File::Spec->splitpath($path);

    if ($target->volume_exists($name)) {
        print STDERR user_message(__x("WARNING: storage volume {name} ".
                                      "already exists on the target. NOT ".
                                      "copying it again. Delete the volume ".
                                      "and retry to copy again.",
                                      name => $name));
        return $target->get_volume($name);
    }

    my $fh;
    open($fh, '<', $path)
        or die(user_message(__x("Unable to open {path} for reading: {error}",
                                path => $path,
                                error => $!)));

    my $st = stat($fh)
        or die(user_message(__x("Unable to stat {path}: {error}",
                                path => $path,
                                error => $!)));

    my $vol = $target->create_volume($name, $st->size);
    $vol->open();

    for (;;) {
        my $buffer;
        # Transfer in block chunks
        my $in = sysread($fh, $buffer, $st->blksize);
        die(user_message(__x("Error reading data from {path}: {error}",
                             path => $path,
                             error => $!))) if (!defined($in));

        last if ($in == 0);

        $vol->write($buffer);
    }

    $vol->close();

    return $vol;
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
