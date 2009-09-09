# Sys::VirtV2V::HVSource
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

package Sys::VirtV2V::HVSource;

use strict;
use warnings;

use Module::Pluggable sub_name => 'modules',
                      search_path => ['Sys::VirtV2V::HVSource'],
                      require => 1;
use Carp;

=pod

=head1 NAME

Sys::VirtV2V::HVSource - Discover source hypervisor artifacts in a guest

=head1 SYNOPSIS

 use Sys::VirtV2V::HVSource;

 my @modules = Sys::VirtV2V::HVSource->find_kernel_modules(desc);
 my @apps    = Sys::VirtV2V::HVSource->find_applications(desc);
 my @kernels = Sys::VirtV2V::HVSource->find_kernels(desc);
 my @xpaths  = Sys::VirtV2V::HVSource->find_metadata(dom);

 Sys::VirtV2V::HVSource->unconfigure(g, guestos, desc);

=head1 DESCRIPTION

Sys::VirtV2V::HVSource provides a mechanism for identifying properties of a
guest operating system which relate specifically to a particular hypervisor. It
is used by a Sys::VirtV2V::HVTarget when reconfiguring the guest.

A call to any of these methods will call, and aggregate if relevant,  all
implemented Sys::VirtV2V::HVSource backends.

=head1 METHODS

In each of these methods, the desc argument is an OS description as returned by
Sys::Guestfs::Lib.

=over

=item Sys::VirtV2V::HVSource->find_kernel_modules(desc)

Return a list of modprobe aliases which load hypervisor-specific modules.

=cut

sub find_kernel_modules
{
    my $class = shift;

    my $desc = shift;
    carp("find_kernel_modules called without desc argument")
        unless defined($desc);

    my @modules = ();
    foreach my $module ($class->modules()) {
        push(@modules, $module->find_kernel_modules($desc));
    }

    return @modules;
}

=item Sys::VirtV2V::HVSource->find_applications(desc)

Return a list of installed hypervisor-specific applications. The list contains
package names as understood by the guest operating system.

=cut

sub find_applications
{
    my $class = shift;

    my $desc = shift;
    carp("find_applications called without desc argument")
        unless defined($desc);

    my @applications = ();
    foreach my $module ($class->modules()) {
        push(@applications, $module->find_applications($desc));
    }

    return @applications;
}

=item Sys::VirtV2V::HVSource->find_kernels(desc)

Return a list of version numbers of kernels which will only boot on a specific
hypervisor.

=cut

sub find_kernels
{
    my $class = shift;

    my $desc = shift;
    carp("find_kernels called without desc argument")
        unless defined($desc);

    my @kernels = ();
    foreach my $module ($class->modules()) {
        push(@kernels, $module->find_kernels($desc));
    }

    return @kernels;
}

=item Sys::VirtV2V::HVSource->find_metadata(dom)

Return guest libvirt metadata which is specific to a particular hypervisor. The
data is returned as a list of XPath paths which relate to the guest's libvirt
domain XML.

=over

=item dom

An XML::DOM resulting from parsing the guest's libvirt domain XML.

=back

=cut

sub find_metadata
{
    my $class = shift;

    my $dom = shift;
    carp("find_metadata called without dom argument") unless defined($dom);

    my @nodeinfo = ();
    foreach my $module ($class->modules()) {
        push(@nodeinfo, $module->find_metadata($dom));
    }

    return @nodeinfo;
}

=item Sys::VirtV2V::HVSource->unconfigure(guestos, desc)

Perform custom unconfiguration tasks. These tasks differ from the above, in they
require no replacement configuration. Examples are removing VMWare tools or Xen
PV drivers.

=cut

sub unconfigure
{
    my $class = shift;

    my ($guestos, $desc) = @_;
    carp("unconfigure called without guestos argument")
        unless defined($guestos);
    carp("unconfigure called without desc argument")
        unless defined($desc);

    foreach my $module ($class->modules()) {
        $module->unconfigure($guestos, $desc);
    }
}

=back

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-v2v(1)>,
L<Sys::Guestfs::Lib(3pm)>,
L<Sys::VirtV2V(3pm)>,
L<http://libguestfs.org/>.

=cut

1;
