# Sys::VirtV2V::Connection
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

package Sys::VirtV2V::Connection;

use strict;
use warnings;

use Module::Pluggable sub_name => 'modules',
                      search_path => ['Sys::VirtV2V::Connection'],
                      require => 1;

use Carp;

=pod

=head1 NAME

Sys::VirtV2V::Connection - Read a variety of guest metadata formats

=head1 SYNOPSIS

 use Sys::VirtV2V::Connection;

 $reader = Sys::VirtV2V::Connection->instantiate("libvirtxml", $uri,
                                                     $config, @args);
 exit 1 unless($mdr->is_configured());
 $dom = $reader->get_dom();

=head1 DESCRIPTION

Sys::VirtV2V::Connection reads the metadata of a, possibly foreign,
guest. It provides the DOM representation of an equivalent libvirt XML
representation.

Sys::VirtV2V::Connection is an interface to various backends, each of
which implement a consistent API. Sys::VirtV2V::Connection itself only
implements methods to access backends.

=head1 METHODS

=over

=item instantiate(name, $uri, $config, @args)

=over

=item name

The name of the module to instantiate.

=item uri

A URI describing the target connection.

=item config

A parsed virt-v2v configuration file.

=item args

Backend-specific arguments describing where its data is located.

=back

Instantiate a backend instance with the given name.

=cut

sub instantiate
{
    my $class = shift;

    my ($name, $uri, $config, @args) = @_;

    defined($name) or carp("instantiate called without name argument");
    defined($uri)  or carp("instantiate called without uri argument");
    defined($config) or carp("instantiate called without config argument");

    foreach my $module ($class->modules()) {
        if($module->get_name() eq $name) {
            return $module->_new($uri, $config->{$name}, @args);
        }
    }

    return undef;
}

=back

=head1 BACKEND INTERFACE

=over

=item CLASS->get_name()

Return the module's name.

=item is_configured()

Return 1 if the module has been suffiently configured to proceed.
Return 0 and display an error message otherwise.

=item get_dom()

Returns an XML::DOM::Document describing a libvirt configuration equivalent to
the input.

Returns undef and displays an error if there was an error

=back

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Connection::LibVirt(3pm)>,
L<Sys::VirtV2V::Connection::LibVirtXML(3pm)>,
L<virt-v2v(1)>,
L<v2v-snapshot(1)>,
L<http://libguestfs.org/>.

=cut

1;
