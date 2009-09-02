# Sys::VirtV2V::HVTarget
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

package Sys::VirtV2V::HVTarget;

use strict;
use warnings;

use Module::Pluggable sub_name => 'modules',
                      search_path => ['Sys::VirtV2V::HVTarget'],
                      require => 1;

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::HVTarget - Manipulate a guest's storage during V2V migration

=head1 SYNOPSIS

 use Sys::VirtV2V::HVTarget;

=head1 DESCRIPTION

=head1 METHODS

=item configure(guestos, mdr, $desc)

Instantiate a backend instance with the given name.

=cut

sub configure
{
    my $class = shift;

    my ($vmm, $guestos, $dom, $desc) = @_;
    carp("configure called without vmm argument") unless defined($vmm);
    carp("configure called without guestos argument") unless defined($guestos);
    carp("configure called without dom argument") unless defined($dom);
    carp("configure called without desc argument") unless defined($desc);

    # Find a module which can configure this guest and run it
    foreach my $module ($class->modules()) {
        if($module->can_handle($desc)) {
            $module->configure($vmm, $guestos, $dom, $desc);
            return;
        }
    }

    die(__"Unable to find a module to configure this guest");
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
