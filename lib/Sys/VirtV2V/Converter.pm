# Sys::VirtV2V::Converter
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

package Sys::VirtV2V::Converter;

use strict;
use warnings;

use Module::Pluggable sub_name => 'modules',
                      search_path => ['Sys::VirtV2V::Converter'],
                      require => 1;

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::Converter - Convert a guest to run on KVM

=head1 SYNOPSIS

 use Sys::VirtV2V::GuestOS;
 use Sys::VirtV2V::Converter;

 my $guestos = Sys::VirtV2V::GuestOS->instantiate($g, $os);
 Sys::VirtV2V::Converter->convert($vmm, $guestos, $dom, $os);

=head1 DESCRIPTION

Sys::VirtV2V::Converter instantiates an appropriate backend for the target guest
OS, and uses it to convert the guest to run on KVM.

=head1 METHODS

=over

=item Sys::VirtV2V::Converter->convert(vmm, guestos, dom, desc)

Instantiate an appropriate backend and call convert on it.

=over

=item vmm

A Sys::Virt connection.

=item guestos

An initialised Sys::VirtV2V::GuestOS object for the guest.

=item dom

An XML::DOM object resulting from parsing the guests's libvirt domain XML.

=item desc

The OS description returned by Sys::Guestfs::Lib.

=back

=cut

sub convert
{
    my $class = shift;

    my ($vmm, $guestos, $dom, $desc) = @_;
    carp("convert called without vmm argument") unless defined($vmm);
    carp("convert called without guestos argument") unless defined($guestos);
    carp("convert called without dom argument") unless defined($dom);
    carp("convert called without desc argument") unless defined($desc);

    # Find a module which can convert this guest and run it
    foreach my $module ($class->modules()) {
        if($module->can_handle($desc)) {
            $module->convert($vmm, $guestos, $dom, $desc);
            return;
        }
    }

    die(__"Unable to find a module to convert this guest");
}

=back

=head1 BACKEND INTERFACE

=over

=item CLASS->can_handle(desc)

Returns 1 if the backend can handle the guest described by $desc, 0 otherwise.

=over

=item desc

An OS description as returned by Sys::Guestfs::Lib.

=back

=item CLASS->convert(vmm, guestos, dom, desc)

Convert the target guest to run on KVM.

can_handle() must have been checked prior to running convert().

=over

=item vmm

A Sys::Virt connection.

=item guestos

An initialised Sys::VirtV2V::GuestOS object for the guest.

=item dom

An XML::DOM object resulting from parsing the guests's libvirt domain XML.

=item desc

The OS description returned by Sys::Guestfs::Lib.

=back

=back

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Converter::Linux(3pm)>,
L<Sys::VirtV2V::GuestOS(3pm)>,
L<Sys::Guestfs::Lib(3pm)>,
L<Sys::Virt(3pm)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
