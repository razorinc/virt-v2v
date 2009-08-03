# Sys::Guestfs::HVSource
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

package Sys::Guestfs::HVSource;

use strict;
use warnings;

use Module::Pluggable sub_name => 'modules',
                      search_path => ['Sys::Guestfs::HVSource'],
                      require => 1;
use Carp;

=pod

=head1 NAME

Sys::Guestfs::HVSource - Manipulate a guest based on its source Hypervisor

=head1 SYNOPSIS

 use Sys::Guestfs::HVSource;

 Sys::Guestfs::HVSource->unconfigure_all();

=head1 DESCRIPTION

Sys::Guestfs::HVSource provides a mechanism for identifying hypervisor specific
changes made to a guest operating system.

=head1 METHODS

=cut

sub find_kernel_modules
{
    my $class = shift;

    my $guestos = shift;
    carp("find_kernel_modules called without guestos argument")
        unless defined($guestos);

    my @modules = ();
    foreach my $module ($class->modules()) {
        push(@modules, $module->find_kernel_modules($guestos));
    }

    return @modules;
}

sub find_applications
{
    my $class = shift;

    my $guestos = shift;
    carp("find_applications called without guestos argument")
        unless defined($guestos);

    my @applications = ();
    foreach my $module ($class->modules()) {
        push(@applications, $module->find_applications($guestos));
    }

    return @applications;
}

sub find_kernels
{
    my $class = shift;

    my $guestos = shift;
    carp("find_kernels called without guestos argument")
        unless defined($guestos);

    my @kernels = ();
    foreach my $module ($class->modules()) {
        push(@kernels, $module->find_kernels($guestos));
    }

    return @kernels;
}

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
