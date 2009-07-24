# Sys::Guestfs::HVSource::Xen::Linux
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

package Sys::Guestfs::HVSource::Xen::Linux;

use strict;
use warnings;

use Locale::TextDomain 'libguestfs';

use XML::DOM;
use XML::DOM::XPath;

=pod

=head1 NAME

Sys::Guestfs::HVSource::Xen::Linux - Unconfigure Xen/Linux changes

=head1 SYNOPSIS

 use Sys::Guestfs::HVSource;

=head1 DESCRIPTION

=cut

sub find_drivers
{
    my $class = shift;

    my $desc = shift;
    carp("find_drivers called without desc argument")
        unless defined($desc);

    my $aliases = $desc->{modprobe_aliases};
    return unless defined($aliases);

    my @drivers = ();
    foreach my $alias (keys(%$aliases)) {
        my $modulename = $aliases->{$alias}->{modulename};

        foreach my $xen_driver qw(xennet xen-vnif xenblk xen-vbd) {
            if($modulename eq $xen_driver) {
                push(@drivers, $alias);
                last;
            }
        }
    }

    return @drivers;
}

sub find_applications
{
    my $class = shift;

    my $desc = shift;
    carp("find_applications called without desc argument")
        unless defined($desc);

    return ();
}

sub find_kernels
{
    my $class = shift;

    my $desc = shift;
    carp("find_kernels called without desc argument")
        unless defined($desc);

    return ();
}

sub find_metadata
{
    my $class = shift;

    my $dom = shift;
    defined($dom) or carp("find_metadata called without dom argument");

    # List of nodes requiring changes if they exist and match a particular
    # pattern
    my @check_nodes = (
        [ '/domain/@type', 'xen' ],
        [ '/domain/os/loader', 'xen' ],
        [ '/domain/devices/input/@bus', 'xen' ]
    );

    my @nodes = ();
    foreach my $check_node (@check_nodes) {
        my $xpath = $check_node->[0];
        my $pattern = $check_node->[1];

        foreach my $node ($dom->findnodes($xpath)) {
            if($node->getValue() =~ m{$pattern}) {
                push(@nodes, $xpath);
            }
        }
    }

    return @nodes;
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
