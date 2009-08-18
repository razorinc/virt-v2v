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

sub find_kernel_modules
{
    my $class = shift;

    my $desc = shift;
    carp("find_kernel_modules called without desc argument")
        unless defined($desc);

    my $aliases = $desc->{modprobe_aliases};
    return unless defined($aliases);

    my @modules = ();
    foreach my $alias (keys(%$aliases)) {
        my $modulename = $aliases->{$alias}->{modulename};

        foreach my $xen_module qw(xennet xen-vnif xenblk xen-vbd) {
            if($modulename eq $xen_module) {
                push(@modules, $alias);
                last;
            }
        }
    }

    return @modules;
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

    my $boot = $desc->{boot};
    return () unless(defined($boot));

    my $configs = $desc->{boot}->{configs};
    return () unless(defined($configs));

    my @kernels = ();
    foreach my $config (@$configs) {
        my $kernel = $config->{kernel};
        next unless(defined($kernel));

        my $modules = $kernel->{modules};
        next unless(defined($modules));

        # Look for the xennet driver in the modules list
        if(grep(/^xennet$/, @$modules) > 0) {
            push(@kernels, $kernel->{version});
        }
    }

    return @kernels;
}

sub find_metadata
{
    my $class = shift;

    my $dom = shift;
    defined($dom) or carp("find_metadata called without dom argument");

    # List of nodes requiring changes if they exist and match a particular
    # pattern, and whether they need to be replaced for a guest to function
    # Most of this is taken from inspection of domain.rng
    my @check_nodes = (
        [ '/domain/@type', 'xen', 1 ],
        [ '/domain/devices/emulator', 'xen', 0 ],
        [ '/domain/devices/input/@bus', 'xen', 1 ],
        [ '/domain/devices/interface/script/@path', 'vif-bridge', 0],
        [ '/domain/os/loader', 'xen', 0 ],
        [ '/domain/os/type/@machine', '(xenpv|xenfv|xenner)', 0 ],
        [ '/domain/devices/disk/target/@bus', 'xen', 0 ],
        [ '/domain/bootloader', undef, 0],
        [ '/domain/bootloader_args', undef, 0]
    );

    my @nodeinfo = ();
    foreach my $check_node (@check_nodes) {
        my $xpath = $check_node->[0];
        my $pattern = $check_node->[1];
        my $required = $check_node->[2];

        foreach my $node ($dom->findnodes($xpath)) {
            if(defined($pattern)) {
                my $value;
                if($node->isa('XML::DOM::Attr')) {
                    $value = $node->getNodeValue();
                } else {
                    my ($text) = $node->findnodes('text()');
                    $value = $text->getNodeValue();
                }

                next unless($value =~ m{$pattern});
            }

            push(@nodeinfo, $node => [ $xpath, $required ]);
        }
    }

    return @nodeinfo;
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
