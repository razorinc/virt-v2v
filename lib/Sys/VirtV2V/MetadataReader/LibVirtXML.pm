# Sys::VirtV2V::MetadataReader::LibVirtXML
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

package Sys::VirtV2V::MetadataReader::LibVirtXML;

use strict;
use warnings;

use XML::DOM;
use XML::DOM::XPath;

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::MetadataReader::LibVirtXML - Read libvirt XML from a file

=head1 SYNOPSIS

 use Sys::VirtV2V::MetadataReader;

 $reader = Sys::VirtV2V::MetadataReader->instantiate("libvirtxml);
 $dom = $reader->get_dom($vmm);

=head1 DESCRIPTION

Sys::VirtV2V::MetadataReader::LibVirtXML is a backend for
Sys::VirtV2V::MetadataReader which reads libvirt XML guest descriptions from a
file.

=head1 METHODS

See BACKEND INTERFACE in L<Sys::VirtV2V::MetadataReader> for a detailed
description of its exported methods.

=over

=cut

use constant NAME => "libvirtxml";

sub _new
{
    my $class = shift;

    my $config = shift;

    my %obj = ();
    my $self = \%obj;

    bless($self, $class);

    if(defined($config)) {
        my %bridges;
        my %networks;

        $self->{bridges} = \%bridges;
        $self->{networks} = \%networks;

        # Split bridges and networks into separate hashes
        foreach my $directive (keys(%$config)) {
            if($directive =~ /^bridge\.(.*)$/) {
                $bridges{$1} = $config->{$directive};
            }

            elsif($directive =~ /^network\.(.*)$/) {
                $networks{$1} = $config->{$directive};
            }

            else {
                print STDERR "virt-v2v: ".
                    __x("WARNING unknown configuration directive {directive} ".
                        "in {name} section",
                        directive => $directive, name => NAME)."\n";
                $self->{invalidconfig} = 1;
            }
        }
    }

    return $self;
}

=item Sys::VirtV2V::MetadataReader::LibVirtXML->get_name()

See BACKEND INTERFACE in L<Sys::VirtV2V::MetadataReader> for details.

=cut

sub get_name
{
    my $class = shift;

    return NAME;
}

=item is_configured()

See BACKEND INTERFACE in L<Sys::VirtV2V::MetadataReader> for details.

=cut

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

=item handle_arguments(@arguments)

See BACKEND INTERFACE in L<Sys::VirtV2V::MetadataReader> for details.

=cut

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

=item get_dom(vmm)

See BACKEND INTERFACE in L<Sys::VirtV2V::MetadataReader> for details.

=cut

sub get_dom
{
    my $self = shift;
    my ($vmm) = shift; # Unused in this backend

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

=back

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::MetadataReader(3)>,
L<virt-v2v(1)>,
L<virt-snapshot(1)>,
L<http://libguestfs.org/>.

=cut

1;
