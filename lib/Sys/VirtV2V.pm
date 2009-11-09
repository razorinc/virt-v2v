# Sys::VirtV2V
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

package Sys::VirtV2V;

use strict;
use warnings;

$Sys::VirtV2V::VERSION = "0.3.0";

=pod

=head1 NAME

Sys::VirtV2V - Convert a virtual guest to use KVM

=head1 DESCRIPTION

Modules under Sys::VirtV2V are used by the L<virt-v2v(1)> and L<v2v-snapshot(1)>
tools. See the documentation for those tools for a more detailed description.

The Sys::VirtV2V module provides package information for virt-v2v.

=head1 VARIABLES

=over

=item $Sys::VirtV2V::VERSION

Version number.

=back

=head1 AUTHORS

Matthew Booth <mbooth@redhat.com>

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-v2v(1)>,
L<v2v-snapshot(1)>,
L<Sys::VirtV2V::GuestOS(3pm)>,
L<Sys::VirtV2V::HVSource(3pm)>,
L<Sys::VirtV2V::HVTarget(3pm)>,
L<Sys::VirtV2V::MetadataReader(3pm)>,
L<http://libguestfs.org/>

=cut

1;
