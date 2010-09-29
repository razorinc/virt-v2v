# Sys::VirtV2V::Connection::LibVirtXMLSource
# Copyright (C) 2009,2010 Red Hat Inc.
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

package Sys::VirtV2V::Connection::LibVirtXMLSource;

use strict;
use warnings;

use Sys::Virt;
use XML::DOM;
use XML::DOM::XPath;

use Sys::VirtV2V::Connection::Source;
use Sys::VirtV2V::Transfer::Local;
use Sys::VirtV2V::Util qw(user_message parse_libvirt_volinfo);

use Locale::TextDomain 'virt-v2v';

@Sys::VirtV2V::Connection::Source::LibVirtXMLSource::ISA =
    qw(Sys::VirtV2V::Connection::Source);

=pod

=head1 NAME

Sys::VirtV2V::Connection::Source::LibVirtXMLSource - Read domain XML from a file

=head1 METHODS

=over

=item new(path)

Create a new LibVirtXMLSource connection. The metadata itself is read from
I<path>.

=cut

sub new
{
    my $class = shift;

    my ($path, $target) = @_;

    my $self = {};
    $self->{path} = $path;

    bless($self, $class);

    $self->_get_dom($path);

    return $self;
}

=item get_name

Return the name of the domain.

=cut

sub get_name
{
    my $dom = shift->{dom};

    my ($name) = $dom->findnodes('/domain/name');
    return $name;
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

=item get_volume(path)

Return a Sys::VirtV2V::Connection::Volume object for I<path>, where I<path> is
the path to a locally available volume.

=cut

sub get_volume
{
    my $self = shift;
    my ($path) = @_;

    # Use a libvirt session connection to inspect local volumes
    my $vmm = Sys::Virt->new(uri => 'qemu:///session');
    my $vol = $vmm->get_storage_volume_by_path($path);

    my ($name, $format, $size, $usage, $is_sparse, $is_block) =
        parse_libvirt_volinfo($vol, $path);

    my $transfer = new Sys::VirtV2V::Transfer::Local($path, $is_sparse);

    return new Sys::VirtV2V::Connection::Volume($name, $format, $path,
                                                $size, $usage,
                                                $is_sparse, $is_block,
                                                $transfer);
}

=back

=head1 COPYRIGHT

Copyright (C) 2009,2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Connection::Source(3pm>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
