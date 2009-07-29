# Sys::Guestfs::MetadataReader::LibVirtXML
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

package Sys::Guestfs::MetadataReader::LibVirtXML;

use strict;
use warnings;

use XML::DOM;
use XML::DOM::XPath;

=pod

=head1 NAME

Sys::Guestfs::MetadataReader::LibVirtXML - Read libvirt XML from a file

=head1 SYNOPSIS

 use Sys::Guestfs::MetadataReader;

 $reader = Sys::Guestfs::MetadataReader->get_instance("libvirtxml);
 $dom = $reader->get_dom();

=head1 DESCRIPTION

Sys::Guestfs::MetadataReader::LibVirtXML is a backend for
Sys::Guestfs::MetadataReader which reads libvirt XML guest descriptions from a
file.

See L<Sys::Guestfs::MetadataReader> for a description of its exported
methods.

=cut

use constant NAME => "libvirtxml";

sub new
{
    my $class = shift;

    my $options = shift;
    carp("new called without options") unless(defined($options));

    my $self = $options;
    bless($self, $class);

    # Convert mapbridge and mapnetwork options into hashes
    foreach my $i qw(bridges networks) {
        my %hash;
        if(exists($self->{$i})) {
            foreach my $map (@{$self->{$i}}) {
                if ($map =~ /^(.+)=(.+)$/) {
                    $hash{$1} = $2;
                } else {
                    print STDERR "$map is not of the format oldvalid=newvalue\n";
                    $self->{invalidconfig} = 1;
                }
            }
        }
        $self->{$i} = \%hash;
    }

    return $self;
}

sub get_name
{
    my $class = shift;

    return NAME;
}

sub get_options
{
    my $class = shift;

    return (
        [ "mapbridge=s@", "bridges",
          "Map network bridge names between old and new hypervisors. ".
          "e.g. --mapbridge xenbr1=virbr0" ],
        [ "mapnetwork=s@", "networks",
          "Map network names between old and new hypervisors. ".
          "e.g. --mapnetwork default=newnet1" ]
    );
}

sub is_configured
{
    my $self = shift;

    if(!defined($self->{path})) {
        print STDERR "You must specify a filename when using ".NAME.".\n";
        return 0;
    }

    return 0 if(exists($self->{invalidconfig}));

    return 1;
}

sub handle_arguments
{
    my $self = shift;

    # The first argument is the libvirt xml file's path
    $self->{path} = shift;

    # Warn if we were given more than 1 argument
    if(scalar(@_) > 0) {
        print STDERR "Warning: ".NAME." only takes a single filename.\n";
    }
}

sub get_dom
{
    my $self = shift;

    # Open the input file
    my $xml; # Implicitly closed on function exit
    if(!open($xml, '<', $self->{path})) {
        print STDERR "Failed to open ".$self->{path}.": $!\n";
        return undef;
    }

    # Parse the input file
    my $parser = new XML::DOM::Parser;
    my $dom;
    eval { $dom = $parser->parse ($xml); };

    # Display any parse errors
    if ($@) {
        print STDERR "Unable to parse ".$self->{path}.": $@\n";
        return undef;
    }

    # Rewrite bridge names
    foreach my $bridge
        ($dom->findnodes("/domain/devices/interface[\@type='bridge']/".
                         "source/\@bridge"))
    {
        my $name = $bridge->getNodeValue();
        if(exists($self->{bridges}->{$name})) {
            $bridge->setNodeValue($self->{bridges}->{$name});
        }
    }

    # Rewrite network names
    foreach my $network
        ($dom->findnodes("/domain/devices/interface[\@type='network']/".
                         "source/\@network"))
    {
        my $name = $network->getNodeValue();
        if(exists($self->{networks}->{$name})) {
            $network->setNodeValue($self->{networks}->{$name});
        }
    }

    return $dom;
}

1;

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::Guestfs::MetadataReader(3)>,
L<virt-inspector(1)>,
L<Sys::Guestfs(3)>,
L<guestfs(3)>,
L<http://libguestfs.org/>,
L<Sys::Virt(3)>,
L<http://libvirt.org/>,
L<guestfish(1)>.

=cut
