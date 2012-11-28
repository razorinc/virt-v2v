# Sys::VirtConvert::Connection::LibVirtXMLSource
# Copyright (C) 2009-2011 Red Hat Inc.
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

package Sys::VirtConvert::Connection::LibVirtXMLSource;

use strict;
use warnings;

use File::stat;
use Sys::Virt;
use XML::DOM;
use XML::DOM::XPath;

use Sys::VirtConvert::Connection::Source;
use Sys::VirtConvert::Transfer::Local;
use Sys::VirtConvert::Util;

use Locale::TextDomain 'virt-v2v';

@Sys::VirtConvert::Connection::LibVirtXMLSource::ISA =
    qw(Sys::VirtConvert::Connection::Source);

=pod

=head1 NAME

Sys::VirtConvert::Connection::Source::LibVirtXMLSource - Read domain XML from a
file

=head1 METHODS

=over

=item new(path)

Create a new LibVirtXMLSource connection. The metadata itself is read from
I<path>.

=cut

sub new
{
    my $class = shift;
    my ($path) = @_;

    my $self = {};
    $self->{path} = $path;

    bless($self, $class);

    $self->_get_meta($path);

    return $self;
}

=item get_name

Return the name of the domain.

=cut

sub get_name
{
    my $meta = shift->{meta};

    return $meta->{name};
}

sub _get_meta
{
    my $self = shift;

    # Open the input file
    my $xml; # Implicitly closed on function exit
    open($xml, '<', $self->{path})
        or v2vdie __x('Failed to open {path}: {error}',
                      path => $self->{path}, error => $!);

    # Parse the input file
    my $dom;
    eval { $dom = new XML::DOM::Parser->parse ($xml); };

    # Display any parse errors
    v2vdie __x('Unable to parse domain from file {path}: {error}',
               path => $self->{path}, error => $@) if $@;

    # Check it looks like domain XML
    my ($dummy) = $dom->findnodes('/domain/name');
    v2vdie __x('{path} doesn\'t look like a libvirt domain XML file',
               path => $self->{path}) unless defined($dummy);

    $self->{meta} = Sys::VirtConvert::Connection::LibVirt::_parse_dom($self,
                                                                      $dom);
}

=item get_volume(path)

Return a Sys::VirtConvert::Connection::Volume object for I<path>, where I<path>
is the path to a locally available volume.

=cut

sub get_volume
{
    my $self = shift;
    my ($path) = @_;

    die __x("Guest disk image {path} is not readable.\n", path => $path)
        unless (-r $path);

    # Use the output of qemu-img to inspect the path
    open(my $qemuimg, '-|', 'env', 'LANG=C', 'qemu-img', 'info', $path)
        or die __x("Unable to execute qemu-img: {error}\n", error => $!);

    # qemu-img outputs data similar to:
    # image: /var/lib/libvirt/images/p2v.img
    # file format: raw
    # virtual size: 8.0G (8589934592 bytes)
    # disk size: 8.0G
    my $output = '';
    my %info;
    while(<$qemuimg>) {
        $output .= $_;
        next unless /^([^:]+):\s+(.*?)\s*$/;
        $info{$1} = $2;
    }

    my (undef, undef, $name) = File::Spec->splitpath($path);

    my $format = $info{'file format'};
    my $vsize = $info{'virtual size'};

    die __x("Unexpected output from qemu-img:\n{output}\n",
            output => $output)
        unless (defined($format) && defined($vsize));

    $vsize =~ /\s+\((\d+)\s+bytes\)$/
        or die __x("qemu-img returned unexpected virtual size: {size}\n",
                   size => $vsize);
    my $size = $1;

    # For $usage, $is_sparse and $is_block, we need to know if it's a block
    # device
    # N.B. qemu-img's 'disk size' output isn't useful here
    my ($usage, $is_sparse, $is_block);
    if (-b $path) {
        $is_block = 1;
        $usage = $size;
        $is_sparse = 0;
    } else {
        $is_block = 0;
        my $st = stat($path);
        $usage = $st->blocks * 512;

        # Usage can be reported greater than size for large files due to the
        # requirement for indirect blocks
        $usage = $size if $usage > $size;

        $is_sparse = $usage < $size ? 1 : 0;
    }

    my $transfer = new Sys::VirtConvert::Transfer::Local($path, $is_block,
                                                         $format, $is_sparse);

    return new Sys::VirtConvert::Connection::Volume($name, $format, $path,
                                                    $size, $usage,
                                                    $is_sparse, $is_block,
                                                    $transfer);
}

=back

=head1 COPYRIGHT

Copyright (C) 2009-2011 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtConvert::Connection::Source(3pm>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
