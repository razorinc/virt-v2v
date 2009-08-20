# Sys::VirtV2V::MetadataReader
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

package Sys::VirtV2V::MetadataReader;

use strict;
use warnings;

use Module::Pluggable sub_name => 'modules',
                      search_path => ['Sys::VirtV2V::MetadataReader'],
                      require => 1;

use Carp;

=pod

=head1 NAME

Sys::VirtV2V::MetadataReader - Read a variety of guest metadata formats

=head1 SYNOPSIS

 use Sys::VirtV2V::MetadataReader;

 $reader = Sys::VirtV2V::MetadataReader->get_instance("libvirtxml);
 $dom = $reader->get_dom();

=head1 DESCRIPTION

Sys::VirtV2V::MetadataReader reads the metadata of a, possibly foreign,
guest. It provides the DOM representation of an equivalent libvirt XML
representation.

Sys::VirtV2V::MetadataReader is an interface to various backends, each of
which implement a consistent API. Sys::VirtV2V::MetadataReader itself only
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

    # Get virt-v2v configuration
    my $config = shift;
    defined($config) or carp("instantiate called without config argument");

    my $instance;
    foreach my $module ($class->modules()) {
        return $module->new($config->{$name}) if($module->get_name() eq $name);
    }

    return undef;
}

1;

=head1 BACKEND INTERFACE

=item new(config)

=over

=item config

The parsed virt-v2v configuration, as returned by Config::Tiny

=back

Instantiate an instance of the backend.

=item get_name()

Return the module's name.

=item is_configured()

Return 1 if the module has been suffiently configured to proceed.
Return 0 and display an error message otherwise.

=item handle_arguments(@arguments)

A backend may take any number of arguments describing where its data is located.

=item get_dom

Returns an XML::DOM::Document describing a libvirt configuration equivalent to
the input.

Returns undef and displays an error if there was an error

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
