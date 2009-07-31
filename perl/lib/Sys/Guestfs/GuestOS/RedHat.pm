# Sys::Guestfs::GuestOS:RedHat
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

package Sys::Guestfs::GuestOS::RedHat;

use strict;
use warnings;

use Carp;
use Locale::TextDomain 'libguestfs';

=pod

=head1 NAME

Sys::Guestfs::GuestOS::RedHat - Manipulate and query a Red Hat guest

=head1 SYNOPSIS

 use Sys::Guestfs::GuestOS;

 $guestos = Sys::Guestfs::GuestOS->get_instance($os, $distro, $version)

=head1 DESCRIPTION

Sys::Guestfs::GuestOS provides a mechanism for querying and manipulating a
specific guest operating system.

Sys::Guestfs::GuestOS is an interface to various backends, each of
which implement a consistent API. Sys::Guestfs::GuestOS itself only
implements methods to access backends.

=head1 METHODS

=cut

sub can_handle
{
    my $class = shift;

    my $desc = shift;

    return ($desc->{os} eq 'linux') && ($desc->{package_format} eq 'rpm');
}

sub new
{
    my $class = shift;

    my $self = {};

    # Guest handle
    my $g = $self->{g} = shift;
    carp("new called without guest handle") unless defined($g);

    # Guest description
    $self->{desc} = shift;
    carp("new called without guest description") unless defined($self->{desc});

    # Guest file map
    $self->{files} = shift;
    carp("new called without files description") unless defined($self->{files});

    # Check how new modules should be configured. Possibilities, in descending
    # order of preference, are:
    #   modprobe.d/
    #   modprobe.conf
    #   modules.conf
    #   conf.modules

    # Note that we're checking in ascending order of preference so that the last
    # discovered method will be chosen

    # Files which the augeas Modprobe lens doesn't look for by default
    my @modprobe_add = ();
    foreach my $file qw(/etc/conf.modules /etc/modules.conf) {
        if($g->exists($file)) {
            push(@modprobe_add, $file);
            $self->{modules} = $file;
        }
    }

    if($g->exists("/etc/modprobe.conf")) {
        $self->{modules} = "modprobe.conf";
    }

    # If the modprobe.d directory exists, create new entries in
    # modprobe.d/libguestfs-added.conf
    if($g->exists("/etc/modprobe.d")) {
        $self->{modules} = "modprobe.d/libguestfs-added.conf";
    }

    die(__"Unable to find any valid modprobe configuration")
        unless(defined($self->{modules}));

    # Initialise augeas
    eval {
        $g->aug_close();
        $g->aug_init("/", 1);

        # Add files which exist, but the augeas Modprobe lens doesn't look for
        # by default
        if(scalar(@modprobe_add) > 0) {
            foreach (@modprobe_add) {
                $g->aug_set("/augeas/load/Modprobe/incl[last()+1]", $_);
            }

            # Make augeas pick up the new configuration
            $g->aug_load();
        }

        # Add /boot/grub/grub.conf to the Grub lens
        $g->aug_set("/augeas/load/Grub/incl[last()+1]", "/boot/grub/grub.conf");
    };

    # The augeas calls will die() on any error.
    die($@) if($@);

    bless($self, $class);

    return $self;
}

sub enable_kernel_module
{
    my $self = shift;
    my ($device, $module) = @_;

    my $g = $self->{g};

    eval {
        $g->aug_set("/files/etc/".$self->{modules}."/alias[last()+1]", $device);
        $g->aug_set("/files/etc/".$self->{modules}."/alias[last()]/modulename",
                    $module)
    };

    # Propagate augeas errors
    die($@) if($@);
}

sub update_kernel_module
{
    my $self = shift;
    my ($device, $module) = @_;

    # We expect the module to have been discovered during inspection
    my $desc = $self->{desc};
    my $augeas = $desc->{modprobe_aliases}->{$device}->{augeas};

    # Error if the module isn't defined
    die("$augeas isn't defined") unless defined($augeas);

    my $g = $self->{g};
    $augeas = $self->_check_augeas_device($augeas, $device);

    eval {
        $g->aug_set($augeas."/modulename", $module);

        # XXX: The following save should not be required, but is
        # If this save is omitted, by the time save is called just before
        # mkinitrd, these changes will have been lost.
        $g->aug_save();
    };

    # Propagate augeas errors
    die($@) if($@);
}

sub disable_kernel_module
{
    my $self = shift;
    my $device = shift;

    # We expect the module to have been discovered during inspection
    my $desc = $self->{desc};
    my $augeas = $desc->{modprobe_aliases}->{$device}->{augeas};

    # Nothing to do if the module isn't defined
    return if(!defined($augeas));

    my $g = $self->{g};

    $augeas = $self->_check_augeas_device($augeas, $device);
    eval {
        $g->aug_rm($augeas);
    };

    # Propagate augeas errors
    die($@) if($@);
}

sub update_display_driver
{
    my $self = shift;
    my $driver = shift;

    my $g = $self->{g};

    # Update the display driver if it exists
    eval {
        foreach my $path
            ($g->aug_match('/files/etc/X11/xorg.conf/Device/Driver'))
        {
            $g->aug_set($path, $driver);
        }

        $g->aug_save();
    };

    # Propagate augeas errors
    die($@) if($@);
}

# We can't rely on the index in the augeas path because it will change if
# something has been inserted or removed before it.
# Look for the alias again in the same file which contained it on the first
# pass.
sub _check_augeas_device
{
    my $self = shift;
    my ($path, $device) = @_;

    my $g = $self->{g};

    $path =~ m{^(.*)/alias(?:\[\d+\])?$}
        or die("Unexpected augeas modprobe alias path: $path");

    my $augeas;
    eval {
        my @aliases = $g->aug_match($1."/alias");

        foreach my $alias (@aliases) {
            if($g->aug_get($alias) eq $device) {
                $augeas = $alias;
                last;
            }
        }
    };

    # Propagate augeas errors
    die($@) if($@);

    return $augeas if(defined($augeas));
    die("Unable to find augeas path similar to $path for $device");
}

sub add_kernel
{
    my $self = shift;
    my $kernel_arch = "i386"; # XXX: Need to get this from inspection!

    my $g = $self->{g};

    my $filename = $self->_match_file('kernel', $kernel_arch);

    # Inspect the rpm to work out what kernel version it contains
    my $version;
    foreach my $file ($g->command_lines(["rpm", "-qlp", $filename])) {
        if($file =~ m{^/boot/vmlinuz-(.*)$}) {
            $version = $1;
            last;
        }
    }

    die(__x"{filename} doesn't contain a valid kernel\n",
            filename => $filename) if(!defined($version));

    $self->_install_rpm($filename);

    # Make augeas reload so it'll find the new kernel
    $g->aug_load();

    return $version;
}

sub remove_kernel
{
    my $self = shift;
    my $version = shift;

    my $g = $self->{g};
    eval {
        # Work out which rpm contains the kernel
        my $rpm = $g->command(["rpm", "-qf", "/boot/vmlinuz-".$version]);

        $g->command(["rpm", "-e", $rpm]);
    };

    die($@) if($@);
}

sub add_application
{
    my $self = shift;
    my $label = shift;
    my $user_arch = "i386"; # XXX: Need to get this from inspection!

    my $filename = $self->_match_file($label, $user_arch);
    $self->_install_rpm($filename);
}

sub remove_application
{
    my $self = shift;
    my $name = shift;

    my $g = $self->{g};
    eval {
        $g->command(["rpm", "-e", $name]);
    };
    die($@) if($@);
}

sub _match_file
{
    my $self = shift;
    my ($label, $arch) = @_;

    my $desc = $self->{desc};
    my $distro = $desc->{distro};
    my $major = $desc->{major_version};
    my $minor = $desc->{minor_version};

    my $files = $self->{files};

    if(values(%$files) > 0) {
        # Ensure that whatever file is returned is accessible
        $self->_ensure_transfer_mounted();

        # Search for a matching entry in the file map, in descending order of
        # specificity
        for my $name ("$distro.$major.$minor.$arch.$label",
                      "$distro.$major.$minor.$label",
                      "$distro.$major.$arch.$label",
                      "$distro.$major.$label",
                      "$distro.$arch.$label",
                      "$distro.$label") {
            return $self->{transfer_mount}.'/'.$files->{$name}
                if(defined($files->{$name}));
        }
    }

    die (__x("No file given matching {label}\n", label =>
        "$distro.$major.$minor.$arch.$label"));

}

# Internal use only
sub _install_rpm
{
    my $self = shift;
    my $filename = shift;

    my $g = $self->{g};
    eval {
        $g->command(["rpm", "-i", $filename]);
    };

    # Propagate command failure
    die($@) if($@);
}

sub _ensure_transfer_mounted
{
    my $self = shift;

    # Return immediately if it's already mounted
    return if(exists($self->{transfer_mount}));

    my $g = $self->{g};

    # Find the transfer device
    my @devices = $g->list_devices();
    my $transfer = $devices[$#devices];

    $self->{transfer_mount} = $g->mkdtemp("/tmp/transferXXXXXX");
    $g->mount_ro($transfer, $self->{transfer_mount});
}

sub remap_block_devices
{
    my $self = shift;
    my $map = (@_);

    my $g = $self->{g};

    # Iterate over fstab. Any entries with a spec in the the map, replace them
    # with their mapped values
    eval {
        foreach my $spec ($g->aug_match('/etc/fstab/*/spec')) {
            my $device = $g->aug_get($spec);

            next unless($device =~ m{^/dev/((?:sd|hd|xvd)(?:[a-z]+))});

            if(exists($map->{$device})) {
                $g->aug_set($spec, $map->{$device});
            } else {
                print STDERR __x("No mapping found for block device {device}",
                                 device => $device)."\n";
            }
        }
    };

    # Propagate augeas failure
    die($@) if($@);
}

sub prepare_bootable
{
    my $self = shift;

    my $version = shift;
    my @modules = @_;

    my $g = $self->{g};

    # Find the grub entry for the given kernel
    my $initrd;
    my $found = 0;
    eval {
        foreach my $kernel
                ($g->aug_match('/files/boot/grub/grub.conf/title/kernel')) {
            if($g->aug_get($kernel) eq "/vmlinuz-$version") {
                # Ensure it's the default
                $kernel =~ m{/files/boot/grub/grub.conf/title(?:\[(\d+)\])?/kernel}
                    or die($kernel);

                my $aug_index;
                if(defined($1)) {
                    $aug_index = $1;
                } else {
                    $aug_index = 1;
                }

                $g->aug_set('/files/boot/grub/grub.conf/default',
                            $aug_index - 1);

                # Get the initrd for this kernel
                $initrd = $g->aug_get("/files/boot/grub/grub.conf/title[$aug_index]/initrd");

                $found = 1;
                last;
            }
        }
    };

    # Propagate augeas failure
    die($@) if($@);

    if(!$found) {
        die(__x"Didn't find a grub entry for kernel version {version}",
               version => $version);
    }

    if(!defined($initrd)) {
        print STDERR __x("WARNING: Kernel version {version} doesn't have an ".
                         "initrd entry in grub", version => $version);
    } else {
        # Initrd as returned by grub is relative to /boot
        $initrd = "/boot$initrd";

        # Backup the original initrd
        $g->mv("$initrd", "$initrd.pre-v2v");

        # Create a new initrd which preloads the required kernel modules
        my @preload_args = ();
        foreach my $module (@modules) {
            push(@preload_args, "--preload=$module");
        }

        # mkinitrd reads configuration which we've probably changed
        eval {
            $g->aug_save();
        };

        if($@) {
            foreach my $error ($g->aug_match('/augeas//error/*')) {
                print STDERR "$error: ".$g->aug_get($error)."\n";
            }
            die($@);
        }

        $g->command(["/sbin/mkinitrd", @preload_args, $initrd, $version]);
    }
}

sub DESTROY
{
    my $self = shift;

    my $g = $self->{g};

    # Remove the transfer mount point if it was used
    if(defined($self->{transfer_mount})) {
        $g->umount($self->{transfer_mount});
        $g->rmdir($self->{transfer_mount});
    }
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
