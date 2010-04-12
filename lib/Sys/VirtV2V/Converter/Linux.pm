# Sys::VirtV2V::Converter::Linux
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

package Sys::VirtV2V::Converter::Linux;

use strict;
use warnings;

use Data::Dumper;
use Locale::TextDomain 'virt-v2v';

use XML::DOM;
use XML::DOM::XPath;

use Sys::VirtV2V::UserMessage qw(user_message);

use Carp;

=pod

=head1 NAME

Sys::VirtV2V::Converter::Linux - Convert a Linux guest to run on KVM

=head1 SYNOPSIS

 use Sys::VirtV2V::GuestOS;
 use Sys::VirtV2V::Converter;

 my $guestos = Sys::VirtV2V::GuestOS->instantiate($g, $os);
 Sys::VirtV2V::Converter->convert($g, $guestos, $dom, $os);

=head1 DESCRIPTION

Sys::VirtV2V::Converter::Linux converts a Linux guest to use KVM.

=head1 METHODS

=over

=item Sys::VirtV2V::Converter::Linux->can_handle(desc)

Return 1 if Sys::VirtV2V::Converter::Linux can convert the guest described by
I<desc>, 0 otherwise.

=cut

sub can_handle
{
    my $class = shift;

    my $desc = shift;
    carp("can_handle called without desc argument") unless defined($desc);

    return ($desc->{os} eq 'linux');
}

=item Sys::VirtV2V::Converter::Linux->convert(guestos, dom, desc)

Convert a Linux guest. Assume that can_handle has previously returned 1.

=over

=item guestos

An initialised Sys::VirtV2V::GuestOS for manipulating the guest OS>.

=item desc

A description of the guest OS as returned by Sys::Guestfs::Lib.

=item devices

An arrayref of libvirt storage device names, in the order they will be presented
to the guest.

=back

=cut

sub convert
{
    my $class = shift;

    my ($g, $guestos, $desc, $devices) = @_;
    carp("convert called without guestos argument") unless defined($guestos);
    carp("convert called without desc argument") unless defined($desc);
    carp("convert called without devices argument") unless defined($devices);

    # Un-configure HV specific attributes which don't require a direct
    # replacement
    _unconfigure_hv($g, $guestos, $desc);

    # Get the best available kernel
    my $kernel = _configure_kernel($guestos, $desc);

    # Check if the resulting kernel will support virtio
    my $virtio = $guestos->supports_virtio($kernel);

    # Configure the rest of the system
    _configure_display_driver($guestos, $virtio);
    $guestos->remap_block_devices($devices, $virtio);
    _configure_kernel_modules($guestos, $desc, $virtio);
    _configure_boot($guestos, $kernel, $virtio);

    my %guestcaps;

    $guestcaps{virtio} = $virtio;
    $guestcaps{arch}   = _get_os_arch($desc);
    $guestcaps{acpi}   = _supports_acpi($desc, $guestcaps{arch});

    return \%guestcaps;
}

sub _configure_kernel_modules
{
    my ($guestos, $desc, $virtio) = @_;

    # Get a list of all old-hypervisor specific kernel modules which need to be
    # replaced or removed
    my %hvs_modules;
    foreach my $module (_find_hv_kernel_modules($guestos)) {
        $hvs_modules{$module} = undef;
    }

    # Go through all kernel modules looking for network or scsi devices
    my $modules = $desc->{modprobe_aliases};

    # Make a note of whether we've added scsi_hostadapter
    # We need this on RHEL 4/virtio because mkinitrd can't detect root on
    # virtio. For simplicity we always ensure this is set for virtio disks.
    my $scsi_hostadapter = 0;

    foreach my $module (keys(%$modules)) {
        # Replace network modules with virtio_net
        if($module =~ /^eth\d+$/) {
            # Make a note that we updated an old-HV specific kernel module
            if(exists($hvs_modules{$module})) {
                $hvs_modules{$module} = 1;
            }

            $guestos->update_kernel_module($module,
                                           $virtio ? "virtio_net" : "e1000");
        }

        # Replace block drivers with virtio_blk
        if($module =~ /^scsi_hostadapter/) {
            # Make a note that we updated an old-HV specific kernel module
            if(exists($hvs_modules{$module})) {
                $hvs_modules{$module} = 1;
            }

            if ($virtio) {
                $guestos->update_kernel_module($module, 'virtio_blk');
                $scsi_hostadapter = 1;
            }

            # IDE doesn't need scsi_hostadapter
            else {
                $guestos->disable_kernel_module($module);
            }
        }
    }

    # Add an explicit scsi_hostadapter if it wasn't there before
    if ($virtio && !$scsi_hostadapter) {
        $guestos->enable_kernel_module('scsi_hostadapter', 'virtio_blk');
    }

    # Warn if any old-HV specific kernel modules weren't updated
    foreach my $module (keys(%hvs_modules)) {
        if(!defined($hvs_modules{$module})) {
            print STDERR user_message
                (__x("WARNING: Don't know how to update {module}, which loads ".
                     "the {modulename} module.",
                     module => $module,
                     modulename => $modules->{$module}->{modulename}));
        }
    }
}

sub _configure_display_driver
{
    my ($guestos, $virtio) = @_;

    $guestos->update_display_driver("cirrus");
}

sub _configure_kernel
{
    my ($guestos, $desc) = @_;

    my %kernels;

    # Look for installed kernels with virtio support
    foreach my $kernel (@{$desc->{kernels}}) {
        my %checklist = (
            "virtio_blk" => undef,
            "virtio_pci" => undef,
            "virtio_net" => undef
        );

        foreach my $module (@{$kernel->{modules}}) {
            if(exists($checklist{$module})) {
                $checklist{$module} = 1;
            }
        }

        my $virtio = 1;
        foreach my $module (keys(%checklist)) {
            if(!defined($checklist{$module})) {
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

    my @remove_kernels = ();

    # Remove foreign hypervisor specific kernels from the list of available
    # kernels
    foreach my $kernel (_find_hv_kernels($desc)) {
        # Remove the kernel from our cache
        delete($kernels{$kernel});

        # Don't actually try to remove them yet in case we remove them all. This
        # would make your dependency checker unhappy.
        push(@remove_kernels, $kernel);
    }

    # Find the highest versioned, virtio capable, installed kernel
    my $boot_kernel;
    foreach my $kernel (sort {$b cmp $a} (keys(%kernels))) {
        if($kernels{$kernel}) {
            $boot_kernel = $kernel;
            last;
        }
    }

    # If none of the installed kernels are appropriate, install a new one
    if(!defined($boot_kernel)) {
        $boot_kernel = $guestos->add_kernel();
    }

    # Check that either there are still kernels in the cache, or we just added a
    # kernel. If neither of these is the case, we're about to try to remove all
    # kernels, which will fail unpleasantly. Fail nicely instead.
    die(user_message(__"No bootable kernels installed, and no replacement ".
                       "specified in configuration.\nUnable to continue."))
        unless(keys(%kernels) > 0 || defined($boot_kernel));

    # It's safe to remove kernels now
    foreach my $kernel (@remove_kernels) {
        # Uninstall the kernel from the guest
        $guestos->remove_kernel($kernel);
    }

    # If we didn't install a new kernel, pick the default kernel
    $boot_kernel ||= $guestos->get_default_kernel();

    return $boot_kernel;
}

sub _configure_boot
{
    my ($guestos, $kernel, $virtio) = @_;

    if($virtio) {
        $guestos->prepare_bootable($kernel, "virtio_pci", "virtio_blk");
    } else {
        $guestos->prepare_bootable($kernel, "sym53c8xx");
    }
}

# Get the target architecture from the default boot kernel
sub _get_os_arch
{
    my ($desc) = @_;

    my $boot = $desc->{boot};
    my $default_boot = $boot->{default} if(defined($boot));

    my $arch;
    if(defined($default_boot)) {
        my $config = $boot->{configs}->[$default_boot];

        if(defined($config->{kernel})) {
            $arch = $config->{kernel}->{arch};
        }
    }

    # Default to i686 if we didn't find an architecture
    return 'i686' if(!defined($arch));

    # i386 should really be i686
    return 'i686' if($arch eq 'i386');

    return $arch;
}

# Return a list of foreign hypervisor specific kernels
sub _find_hv_kernels
{
    my $desc = shift;

    my $boot = $desc->{boot};
    return () unless(defined($boot));

    my $configs = $desc->{boot}->{configs};
    return () unless(defined($configs));

    # Xen PV kernels can be distinguished from other kernels by their inclusion
    # of the xennet driver
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

sub _unconfigure_hv
{
    my ($g, $guestos, $desc) = @_;

    _unconfigure_xen($g, $guestos, $desc);
    _unconfigure_vmware($guestos, $desc);
}

# Unconfigure Xen specific guest modifications
sub _unconfigure_xen
{
    my ($g, $guestos, $desc) = @_;

    my $found_kmod = 0;

    # Look for kmod-xenpv-*, which can be found on RHEL 3 machines
    foreach my $app (@{$desc->{apps}}) {
        my $name = $app->{name};

        if($name =~ /^kmod-xenpv(-.*)?$/) {
            $guestos->remove_application($name);
            $found_kmod = 1;
        }
    }

    # Undo related nastiness if kmod-xenpv was installed
    if($found_kmod) {
        # kmod-xenpv modules may have been manually copied to other kernels.
        # Hunt them down and destroy them.
        foreach my $dir (grep(m{/xenpv$}, $g->find('/lib/modules'))) {
            $dir = '/lib/modules/'.$dir;

            # Check it's a directory
            next unless($g->is_dir($dir));

            # Check it's not owned by an installed application
            eval {
                $g->get_application_owner($dir);
            };

            # Remove it if get_application_owner didn't find an owner
            if($@) {
                $g->rm_rf($dir);
            }
        }

        # rc.local may contain an insmod or modprobe of the xen-vbd driver
        my @rc_local = ();
        eval {
            @rc_local = $g->read_lines('/etc/rc.local');
        };

        if($@) {
            print STDERR user_message(__x("Unable to open /etc/rc.local: ".
                                          "{error}", error => $@));
        }

        else {
            my $size = 0;

            foreach my $line (@rc_local) {
                if($line =~ /\b(insmod|modprobe)\b.*\bxen-vbd/) {
                    $line = '#'.$line;
                }

                $size += length($line) + 1;
            }

            $g->write_file('/etc/rc.local', join("\n", @rc_local)."\n", $size);
        }
    }
}

# Unconfigure VMware specific guest modifications
sub _unconfigure_vmware
{
    my ($guestos, $desc) = @_;

    # Uninstall VMwareTools
    foreach my $app (@{$desc->{apps}}) {
        my $name = $app->{name};

        if ($name eq "VMwareTools") {
            $guestos->remove_application($name);
        }
    }
}

# Get a list of all foreign hypervisor specific kernel modules which are being
# used by the guest
sub _find_hv_kernel_modules
{
    my ($desc) = @_;

    return _find_xen_kernel_modules($desc);
}

# Get a list of xen specific kernel modules which are being used by the guest
sub _find_xen_kernel_modules
{
    my ($desc) = @_;
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

# Return 1 if the guest supports ACPI, 0 otherwise
sub _supports_acpi
{
    my ($desc, $arch) = @_;

    # Blacklist configurations which are known to fail
    # RHEL 3, x86_64
    if ($desc->{distro} eq 'rhel' && $desc->{major_version} == 3 &&
        $arch eq 'x86_64') {
        return 0;
    }

    return 1;
}

=back

=head1 COPYRIGHT

Copyright (C) 2009,2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Converter(3pm)>,
L<Sys::VirtV2V(3pm)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
