# Sys::Guestfs::GuestOS
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

package Sys::Guestfs::GuestOS;

use strict;
use warnings;

use Module::Pluggable::Ordered sub_name => 'modules',
                               search_path => 'Sys::Guestfs::GuestOS',
                               require => 1;

use Carp;

=pod

=head1 NAME

Sys::Guestfs::GuestOS - Guest OS specific queries and manipulation

=head1 SYNOPSIS

 use Sys::Guestfs::GuestOS;

 $guestos = Sys::Guestfs::GuestOS->get_instance($os, $distro, $version)

=head1 DESCRIPTION

Sys::Guestfs::GuestOS provides a mechanism for querying and manipulating a
specific guest operating system.

Sys::Guestfs::GuestOS is an interface to various backends, each of
which implement a consistent API. Sys::Guestfs::GuestOS itself only
implements methods to access backends.

=head1 METHODS

=item instantiate(desc)

Instantiate a GuestOS object capable of manipulating the os described by $desc.

Returns a Sys::Guestfs::GuestOS object if one is found.
Returns undef otherwise.

=cut

sub instantiate
{
    my $class = shift;

    my ($g, $desc, $files) = @_;
    defined($g) or carp("get_instance called without g argument");
    defined($desc) or carp("get_instance called without desc argument");
    defined($files) or carp("get_instance called without files argument");

    foreach my $module ($class->modules()) {
        return $module->new($g, $desc, $files) if($module->can_handle($desc));
    }

    return undef;
}

1;

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-inspector(1)>,
L<Sys::Guestfs(3)>,
L<guestfs(3)>,
L<http://libguestfs.org/>,
L<Sys::Virt(3)>,
L<http://libvirt.org/>,
L<guestfish(1)>.

=cut
