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

use Sys::VirtV2V::UserMessage qw(user_message);

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::Connection::LibVirtXML - Read libvirt XML from a file

=head1 SYNOPSIS

 use Sys::VirtV2V::Connection;

 $reader = Sys::VirtV2V::Connection->instantiate("libvirtxml", undef,
                                                     $config, @args);
 $dom = $reader->get_dom();

=head1 DESCRIPTION

Sys::VirtV2V::Connection::LibVirtXML is a backend for
Sys::VirtV2V::Connection which reads libvirt XML guest descriptions from a
file.

=head1 METHODS

See BACKEND INTERFACE in L<Sys::VirtV2V::Connection> for a detailed
description of its exported methods.

=over

=cut

use constant NAME => "libvirtxml";

sub _new
{
    my $class = shift;

    my ($uri, $config, @args) = @_;

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
                print STDERR user_message
                    (__x("WARNING: unknown configuration directive ".
                         "{directive} in {name} section.",
                         directive => $directive, name => NAME));
                $self->{invalidconfig} = 1;
            }
        }
    }

    $self->_handle_args(@args);

    return $self;
}

sub _handle_args
{
    my $self = shift;

    # The first argument is the libvirt xml file's path
    $self->{path} = shift;

    # Warn if we were given more than 1 argument
    if(scalar(@_) > 0) {
        print STDERR user_message
            (__x("WARNING: {modulename} only takes a single filename.",
                 modulename => NAME));
    }
}

=item Sys::VirtV2V::Connection::LibVirtXML->get_name()

See BACKEND INTERFACE in L<Sys::VirtV2V::Connection> for details.

=cut

sub get_name
{
    my $class = shift;

    return NAME;
}

=item is_configured()

See BACKEND INTERFACE in L<Sys::VirtV2V::Connection> for details.

=cut

sub is_configured
{
    my $self = shift;

    if(!defined($self->{path})) {
        print STDERR user_message
            (__x("You must specify a filename when using {modulename}",
                 modulename => NAME));
        return 0;
    }

    return 0 if(exists($self->{invalidconfig}));

    return 1;
}

=item get_dom()

See BACKEND INTERFACE in L<Sys::VirtV2V::Connection> for details.

=cut

sub get_dom
{
    my $self = shift;

    # Open the input file
    my $xml; # Implicitly closed on function exit
    if(!open($xml, '<', $self->{path})) {
        print STDERR user_message
            (__x("Failed to open {path}: {error}",
                 path => $self->{path}, error => $!));
        return undef;
    }

    # Parse the input file
    my $parser = new XML::DOM::Parser;
    my $dom;
    eval { $dom = $parser->parse ($xml); };

    # Display any parse errors
    if ($@) {
        print STDERR user_message
            (__x("Unable to parse {path}: {error}",
                 path => $self->{path}, error => $@));
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

L<Sys::VirtV2V::Connection(3)>,
L<virt-v2v(1)>,
L<v2v-snapshot(1)>,
L<http://libguestfs.org/>.

=cut

1;
