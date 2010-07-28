# Sys::VirtV2V::Connection::LibVirtXML
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

package Sys::VirtV2V::Connection::LibVirtXML;

use strict;
use warnings;

use XML::DOM;
use XML::DOM::XPath;

use Sys::VirtV2V::Connection;
use Sys::VirtV2V::Util qw(user_message);

use Locale::TextDomain 'virt-v2v';

@Sys::VirtV2V::Connection::LibVirtXML::ISA = qw(Sys::VirtV2V::Connection);

=pod

=head1 NAME

Sys::VirtV2V::Connection::LibVirtXML - Read libvirt XML from a file

=head1 SYNOPSIS

 use Sys::VirtV2V::Connection::LibVirtXML;

 $conn = Sys::VirtV2V::Connection::LibVirtXML->new($path, $target);
 $dom = $conn->get_dom();

=head1 DESCRIPTION

Sys::VirtV2V::Connection::LibVirtXML is an implementation of
Sys::VirtV2V::Connection which reads libvirt XML guest descriptions from a
file.

=head1 METHODS

=over

=item new(path, target)

Create a new LibVirtXML connection. The metadata itself is read from I<path>.
Storage will be copied to I<target>.

=cut

sub new
{
    my $class = shift;

    my ($path, $target) = @_;

    my $self = {};
    $self->{path} = $path;

    bless($self, $class);

    $self->_get_dom($path);

    # Only support LocalCopy for libvirtxml
    $self->_storage_iterate("Sys::VirtV2V::Transfer::LocalCopy", $target);

    return $self;
}

sub _get_dom
{
    my $self = shift;

    # Open the input file
    my $xml; # Implicitly closed on function exit
    open($xml, '<', $self->{path})
        or die(user_message(__x("Failed to open {path}: {error}",
                                path => $self->{path}, error => $!)));

    # Parse the input file
    eval { $self->{dom} = new XML::DOM::Parser->parse ($xml); };

    # Display any parse errors
    die(user_message(__x("Unable to parse domain from file {path}: {error}",
                         path => $self->{path}, error => $@))) if ($@);

    # Check it looks like domain XML
    my ($dummy) = $self->{dom}->findnodes('/domain/name');
    die(user_message(__x("{path} doesn't look like a libvirt domain XML file",
                         path => $self->{path}))) unless (defined($dummy));
}

=back

=head1 COPYRIGHT

Copyright (C) 2009,2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Connection(3pm>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
