# Sys::Guestfs::Storage::QCOW2
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

package Sys::Guestfs::Storage::QCOW2;

use strict;
use warnings;

use File::Temp qw(:mktemp);
use Locale::TextDomain 'libguestfs';

use XML::DOM;
use XML::DOM::XPath;

=pod

=head1 NAME

Sys::Guestfs::Storage::QCOW2 - Create QCOW2 images for guest storage

=head1 SYNOPSIS

 use Sys::Guestfs::Storage;

 $storage = Sys::Guestfs::Storage->get_instance("qcow2");
 $storage->update_guest($dom);

=head1 DESCRIPTION

Sys::Guestfs::Storage::QCOW2 is a backend for Sys::Guestfs::Storage. See
L<Sys::Guestfs::Storage> for a description of its exported methods.

=cut

use constant NAME => "qcow2";

sub new
{
    my $class = shift;

    my $config = shift;

    my %obj = ();
    my $self = \%obj;
    bless ($self, $class);

    if(defined($config)) {
        $self->{storagedir} = $config->{storagedir};
    }

    # Configuration defaults
    $self->{storagedir} = "/var/tmp" if(!defined($self->{storagedir}));

    return $self;
}

sub get_name
{
    my $class = shift;

    return NAME;
}

sub is_configured
{
    my $self = shift;

    return 1;
}

# Create new qcow2 storage for a guest, and update the guest to use the new
# storage
sub update_guest
{
    my $self = shift;
    my $dom = shift;

    # First, get a list of existing storage
    foreach my $source ($dom->findnodes('/domain/devices/disk/source')) {
        my $attributes = $source->getAttributes();

        # Look for the source location
        my $path;
        foreach my $attr qw(dev file) {
            my $item = $attributes->getNamedItem($attr);
            if(defined($item)) {
                $path = $item->getValue();

                # Remove the attribute. We'll add a new one in below.
                $attributes->removeNamedItem($attr);
            }
        }

        # Warn and ignore this source if we didn't find either
        if(!defined($path)) {
            print STDERR "qcow2: invalid source: ".$source->toString()."\n";
            next;
        }

        # XXX: Do something intelligent if it's already a qcow2 image

        # Create a qcow2 image for the underlying storage
        my $qcow2_path = $self->_create_qcow2($path);

        # Update the source to be a "file" with the new path
        $source->setAttribute("file", $qcow2_path);

        # Also update the disk element to be a "file"
        $source->getParentNode()->setAttribute('type', 'file');

        # Remove the driver element which is a sibling of source because it
        # might specify a physical device
        # XXX: Do we need to store the old value for any reason?

        foreach my $driver ($source->findnodes('../driver')) {
            $driver->getParent()->removeChild($driver);
        }
    }

    return 1;
}

# Create a qcow2 image for <source> in the storagedir directory
# Return the path of the newly created qcow2, or undef if there was a problem
# XXX: This should use a libvirt storage pool
sub _create_qcow2
{
    my $self = shift;
    my $source = shift;

    my $qcow2 = mktemp($self->{storagedir}."/qcow2.XXXXXX");

    system("qemu-img create -b $source -f qcow2 $qcow2");

    if(0 != $?) {
        print STDERR __"QCOW2: Failed to create qcow2 image"."\n";
        return undef;
    }

    return $qcow2;
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
