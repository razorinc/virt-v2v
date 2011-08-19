# Sys::VirtConvert
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

package Sys::VirtConvert;

use strict;
use warnings;

$Sys::VirtConvert::VERSION = "0.8.3";

=pod

=head1 NAME

Sys::VirtConvert - Convert a virtual guest to use KVM

=head1 DESCRIPTION

Modules under Sys::VirtConvert are used by the L<virt-v2v(1)> and virt-p2v
tools. See the virt-v2v documentation for a more detailed description.

The Sys::VirtConvert module provides package information for virt-v2v.

=head1 VARIABLES

=over

=item $Sys::VirtConvert::VERSION

Version number.

=back

=head1 AUTHORS

Matthew Booth <mbooth@redhat.com>

=head1 COPYRIGHT

Copyright (C) 2009-2011 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-v2v(1)>,
L<http://libguestfs.org/>

=cut

1;
