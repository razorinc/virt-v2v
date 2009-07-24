# Sys::Guestfs::HVTarget::Linux
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

package Sys::Guestfs::HVTarget::Linux;

use strict;
use warnings;

use Data::Dumper;
use Locale::TextDomain 'libguestfs';

use Sys::Guestfs::HVSource;

use Carp;

=pod

=head1 NAME

Sys::Guestfs::HVTarget::Linux - Configure a Linux guest for a target hypervisor

=head1 SYNOPSIS

 use Sys::Guestfs::HVTarget;

=head1 DESCRIPTION

=head1 METHODS

=cut

sub can_handle
{
    my $class = shift;

    my $desc = shift;
    carp("can_handle called without desc argument") unless defined($desc);

    return ($desc->{os} eq 'linux');
}

sub configure
{
    my $class = shift;

    my ($guestos, $dom, $desc) = @_;
    carp("configure called without guestos argument") unless defined($guestos);
    carp("configure called without dom argument") unless defined($dom);
    carp("configure called without desc argument") unless defined($desc);

    configure_drivers($guestos, $desc);
    configure_applications($guestos, $desc);
    configure_kernels($guestos, $desc);
    configure_metadata($dom, $desc);
}

sub configure_drivers
{
    my ($guestos, $desc) = @_;
    die("configure_drivers called without guestos argument")
        unless defined($guestos);
    die("configure_drivers called without desc argument")
        unless defined($desc);

    # Get a list of all old-hypervisor specific drivers which need to be
    # replaced or removed
    my %hvs_drivers;
    foreach my $driver (Sys::Guestfs::HVSource->find_drivers($guestos)) {
        $hvs_drivers{$driver} = undef;
    }

    # Go through all drivers looking for network or scsi devices
    my $drivers = $desc->{modprobe_aliases};

    foreach my $driver (keys(%$drivers)) {
        # Replace network drivers with virtio_net
        if($driver =~ /^eth\d+$/) {
            # Make a note that we updated an old-HV specific driver
            if(exists($hvs_drivers{$driver})) {
                $hvs_drivers{$driver} = "virtio_net";
            }

            $guestos->update_driver($driver, "virtio_net");

            print STDERR __x("Replaced {driver} driver with virtio_net\n",
                      driver => $driver);
        }

        # Replace block drivers with virtio_blk
        if($driver =~ /^scsi_hostadapter/) {
            # Make a note that we updated an old-HV specific driver
            if(exists($hvs_drivers{$driver})) {
                $hvs_drivers{$driver} = "virtio_blk";
            }

            $guestos->update_driver($driver, "virtio_blk");

            print STDERR __x("Replaced {driver} driver with virtio_blk\n",
                      driver => $driver);
        }
    }

    # Warn if any old-HV specific drivers weren't updated
    foreach my $driver (keys(%hvs_drivers)) {
        if(!defined($hvs_drivers{$driver})) {
            print STDERR __x("WARNING: Don't know how to update {driver}, ".
                             "which loads the {module} module.\n",
                             driver => $driver,
                             module => $drivers->{$driver}->{modulename});
        }
    }
}

sub configure_applications
{
    my ($guestos, $desc) = @_;
    die("configure_applications called without guestos argument")
        unless defined($guestos);
    die("configure_applications called without desc argument")
        unless defined($desc);

    my @hvs_apps = Sys::Guestfs::HVSource->find_applications($guestos);
}

sub configure_kernels
{
    my ($guestos, $desc) = @_;
    die("configure_kernels called without guestos argument")
        unless defined($guestos);
    die("configure_kernels called without desc argument")
        unless defined($desc);

    my %kernels;

    # Look for installed kernels with virtio support
    foreach my $kernel (@{$desc->{kernels}}) {
        my %checklist = (
            "virtio_blk" => undef,
            "virtio_pci" => undef,
            "virtio_net" => undef
        );

        foreach my $driver ($kernel->{modules}) {
            if(exists($checklist{$driver})) {
                $checklist{$driver} = 1;
            }
        }

        my $virtio = 1;
        foreach my $driver (keys(%checklist)) {
            if(!defined($checklist{$driver})) {
                $virtio = 0;
                last;
            }
        }

        if($virtio) {
            $kernels{$kernel->{version}} = 1;
        } else {
            $kernels{$kernel->{version}} = 0;
        }
    }

    # Remove old-HV kernels
    foreach my $kernel (Sys::Guestfs::HVSource->find_kernels($guestos)) {
        # Remove the kernel from our cache
        delete($kernels{$kernel});

        # Uninstall the kernel from the guest
        $guestos->remove_kernel($kernel);
    }

    # Find the highest versioned, virtio capable, installed kernel
    my $boot_kernel;
    foreach my $kernel (sort {$b cmp $a} (keys(%kernels))) {
        if($kernels{$kernel}) {
            if($kernels{$kernel}) {
                $boot_kernel = $kernel;
                last;
            }
        }
    }

    # If none of the installed kernels are appropriate, install a new one
    if(!defined($boot_kernel)) {
        $boot_kernel = $guestos->add_kernel();
    }

    $guestos->prepare_bootable($boot_kernel,
                               "virtio_pci", "virtio_blk", "virtio_net");
}

sub configure_metadata
{
    my ($dom, $desc) = @_;

    die("configure_metadata called without dom argument")
        unless defined($dom);
    die("configure_metadata called without desc argument")
        unless defined($desc);

    my @hvs_metadata = Sys::Guestfs::HVSource->find_metadata($dom);
}

1;

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-inspector(1)>,
L<Sys::Guestfs(3)>,
L<guestfs(3)>,
L<http://libguestfs.org/>,
L<Sys::Virt(3)>,
L<http://libvirt.org/>,
L<guestfish(1)>.

=cut
