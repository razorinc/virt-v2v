# Sys::VirtV2V::Storage
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

package Sys::VirtV2V::Storage;

use strict;
use warnings;

use Module::Pluggable sub_name => 'modules',
                      search_path => ['Sys::VirtV2V::Storage'],
                      require => 1;

=pod

=head1 NAME

Sys::VirtV2V::Storage - Manipulate a guest's storage during V2V migration

=head1 SYNOPSIS

 use Sys::VirtV2V::Storage;

 $storage = Sys::VirtV2V::Storage->get_instance("snapshot");
 $storage->update_guest($dom);

=head1 DESCRIPTION

Sys::VirtV2V::Storage changes a guest's underlying storage underlying storage
during a V2V migration.

Sys::VirtV2V::Storage is an interface to various backends, each of
which implement a consistent API. Sys::VirtV2V::Storage itself only
implements methods to access backends.

=head1 METHODS

=item instantiate(name, config)

=over

=item name

The name of the backend module to instantiate.

=item config

The parsed virt-v2v configuration file, as returned by Config::Tiny.

=back

Instantiate a backend instance with the given name.

=cut

sub instantiate
{
    my $class = shift;

    # Get the name of the module we're going to instantiate
    my $name = shift;
    carp("instantiate called without name argument") unless(defined($name));

    # Get the options for the module
    my $config = shift;
    carp("instantiate called without config argument") unless(defined($config));

    my $instance;
    foreach my $module ($class->modules()) {
        return $module->new($config->{$name}) if($module->get_name() eq $name);
    }

    return undef;
}

1;

=head1 BACKEND INTERFACE

=item new(config)

Create an instance of the backend

=item get_name()

Return the module's name.

=item is_configured()

Return 1 if the module has been suffiently configured to proceed.
Return 0 and display an error message otherwise.

=item update_guest(dom)

dom is an XML::DOM::Document object describing a libvirt configuration.
update_guest finds the storage defined in the guest, creates new storage for it
and updates the guest DOM accordingly.

Returns 1 on success or 0 on error.

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
