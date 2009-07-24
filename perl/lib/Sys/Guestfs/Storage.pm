# Sys::Guestfs::Storage
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

package Sys::Guestfs::Storage;

use strict;
use warnings;

use Module::Pluggable sub_name => 'modules',
                      search_path => ['Sys::Guestfs::Storage'],
                      require => 1;

=pod

=head1 NAME

Sys::Guestfs::Storage - Manipulate a guest's storage during V2V migration

=head1 SYNOPSIS

 use Sys::Guestfs::Storage;

 $storage = Sys::Guestfs::Storage->get_instance("snapshot");
 $storage->update_guest($dom);

=head1 DESCRIPTION

Sys::Guestfs::Storage changes a guest's underlying storage underlying storage
during a V2V migration.

Sys::Guestfs::MetadataReader is an interface to various backends, each of
which implement a consistent API. Sys::Guestfs::MetadataReader itself only
implements methods to access backends.

=head1 METHODS

=item instantiate(name)

Instantiate a backend instance with the given name.

=cut

sub instantiate
{
    my $class = shift;

    # Get the name of the module we're going to instantiate
    my $name = shift;
    defined($name) or carp("instantiate called without name argument");

    # Get the options for the module
    my $options = shift;
    defined($options) or carp("instantiate called without options argument");

    my $instance;
    foreach my $module ($class->modules()) {
        return $module->new($options) if($module->get_name() eq $name);
    }

    return undef;
}

=item get_options()

Return a hashref containing module_name => (module options).

=cut

sub get_options
{
    my $class = shift;

    my %options;
    foreach my $module ($class->modules()) {
        $options{$module->get_name()} = [ $module->get_options() ];
    }

    return \%options;
}

1;

=head1 BACKEND INTERFACE

=item new()

Instantiate an instance of the backend

=item get_name()

Return the module's name.

=item get_options()

Return a list of command line options in the correct format for GetOptions. This
list will be added to those of other modules and the main program.

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
