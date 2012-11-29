# Sys::VirtConvert::Converter::RedHat
# Copyright (C) 2009-2012 Red Hat Inc.
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

use strict;
use warnings;


# Functions supported by grubby, and therefore common between gruby legacy and
# grub2
package Sys::VirtConvert::Converter::RedHat::Grub;

use Sys::VirtConvert::Util;
use Locale::TextDomain 'virt-v2v';

sub get_initrd
{
    my $self = shift;
    my ($path) = @_;

    my $g = $self->{g};

    foreach my $line ($g->command_lines(['grubby', '--info', $path])) {
        return $1 if $line =~ /^initrd=(\S+)/;
    }

    v2vdie __x('Didn\'t find initrd for kernel {path}', path => $path);
}


# Methods for inspecting and manipulating grub legacy
package Sys::VirtConvert::Converter::RedHat::GrubLegacy;

use Sys::VirtConvert::Util;

use File::Basename;
use Locale::TextDomain 'virt-v2v';

@Sys::VirtConvert::Converter::RedHat::GrubLegacy::ISA =
    qw(Sys::VirtConvert::Converter::RedHat::Grub);

sub new
{
    my $class = shift;
    my ($g, $root) = @_;

    my $self = {};
    bless($self, $class);

    $self->{g} = $g;
    $self->{root} = $root;

    # Look for the grub configuration file
    foreach my $path ('/boot/grub/menu.lst', '/boot/grub/grub.conf')
    {
        if ($g->exists($path)) {
            $self->{grub_conf} = $path;
            last;
        }
    }

    # We can't continue without a config file
    die unless defined ($self->{grub_conf});

    # Find the path which needs to be prepended to paths in grub.conf to
    # make them absolute
    # Look for the most specific mount point discovered

    # Default to / (no prefix required)
    $self->{grub_fs} ||= "";

    my %mounts = $g->inspect_get_mountpoints($root);
    foreach my $path ('/boot/grub', '/boot') {
        if (exists($mounts{$path})) {
            $self->{grub_fs} = $path;
            last;
        }
    }

    # Initialise augeas
    eval {
        # Check grub_conf is included by the Grub lens
        my $found = 0;
        foreach my $incl ($g->aug_match("/augeas/load/Grub/incl")) {
            if ($g->aug_get($incl) eq $self->{grub_conf}) {
                $found = 1;
                last;
            }
        }

        # If it wasn't there, add it
        unless ($found) {
            $g->aug_set("/augeas/load/Grub/incl[last()+1]", $self->{grub_conf});

            # Make augeas pick up the new configuration
            $g->aug_load();
        }
    };
    augeas_error($g, $@) if ($@);

    return $self;
}

sub list_kernels
{
    my $self = shift;

    my $g = $self->{g};
    my $grub_conf = $self->{grub_conf};
    my $grub_fs = $self->{grub_fs};

    # Look for a kernel, starting with the default
    my @paths;
    eval {
        # Get the default kernel from grub if it's set
        my $default = eval { $g->aug_get("/files$grub_conf/default") };
        # Doesn't matter if get fails

        push(@paths, $g->aug_match("/files$grub_conf/title[$default]/kernel"))
            if defined($default);
        push(@paths, $g->aug_match("/files$grub_conf/title/kernel"));
    };
    augeas_error($g, $@) if ($@);

    my @kernels;
    my %checked;
    foreach my $path (@paths) {
        next if $checked{$path};
        $checked{$path} = 1;

        my $kernel = eval { $g->aug_get($path) };
        augeas_error($g, $@) if ($@);

        # Prepend the grub filesystem to the kernel path
        $kernel = "$grub_fs$kernel" if defined $grub_fs;

        # Check the kernel exists
        if ($g->exists($kernel)) {
            push(@kernels, $kernel);
        }

        else {
            logmsg WARN, __x('grub refers to {path}, which doesn\'t exist.',
                             path => $kernel);
        }
    }

    return @kernels;
}

sub update_console
{
    my $self = shift;
    my ($remove) = @_;

    my $g = $self->{g};
    my $grub_conf = $self->{grub_conf};

    # Update any kernel console lines
    foreach my $augpath
        ($g->aug_match("/files$grub_conf/title/kernel/console"))
    {
        my $console = $g->aug_get($augpath);
        if ($console =~ /\b(x|h)vc0\b/) {
            if ($remove) {
                $g->aug_rm($augpath);
            } else {
                $console =~ s/\b(x|h)vc0\b/ttyS0/g;
                $g->aug_set($augpath, $console);
            }
        }

        if ($remove && $console =~ /\bttyS0\b/) {
            $g->aug_rm($augpath);
        }
    }
}

sub check
{
    my $self = shift;
    my ($path) = @_;

    my $g = $self->{g};
    my $grub_conf = $self->{grub_conf};
    my $grub_fs   = $self->{grub_fs};
    $path =~ /^$grub_fs(.*)/
       or v2vdie __x('Kernel {kernel} is not under grub tree {grub}',
                     kernel => $path, grub => $grub_fs);
    my $grub_path = $1;

    # Nothing to do if the kernel already has a grub entry
    return if $g->aug_match("/files$grub_conf/title/kernel[. = '$grub_path']") > 0;

    my $kernel =
        Sys::VirtConvert::Converter::RedHat::_inspect_linux_kernel($g, $path);
    my $version = $kernel->{version};
    my $grub_initrd = dirname($path)."/initrd-$version";

    # No point in dying if /etc/redhat-release can't be read
    my ($title) = eval { $g->read_lines('/etc/redhat-release') };
    $title ||= 'Linux';

    # This is how new-kernel-pkg does it
    $title =~ s/ release.*//;
    $title .= " ($version)";

    # Doesn't matter if there's no default
    my $default = eval { $g->aug_get("/files$grub_conf/default") };

    if (defined($default)) {
        $g->aug_defvar('template',
             "/files$grub_conf/title[".($default + 1).']');
    }

    # If there's no default, take the first entry with a kernel
    else {
        my ($match) = $g->aug_match("/files$grub_conf/title/kernel");
        die("No template kernel found in grub.") unless defined($match);

        $match =~ s/\/kernel$//;
        $g->aug_defvar('template', $match);
    }

    # Add a new title node at the end
    $g->aug_defnode('new', "/files$grub_conf/title[last()+1]", $title);

    # N.B. Don't change the order of root, kernel and initrd below, or the
    # guest will not boot.

    # Copy root from the template
    $g->aug_set('$new/root', $g->aug_get('$template/root'));

    # Set kernel and initrd to the new values
    $g->aug_set('$new/kernel', $grub_path);
    $g->aug_set('$new/initrd', $grub_initrd);

    # Copy all kernel command-line arguments
    foreach my $arg ($g->aug_match('$template/kernel/*')) {
        # kernel arguments don't necessarily have values
        my $val = eval { $g->aug_get($arg) };

        $arg =~ /([^\/]*)$/;
        $arg = $1;

        if (defined($val)) {
            $g->aug_set('$new/kernel/'.$arg, $val);
        } else {
            $g->aug_clear('$new/kernel/'.$arg);
        }
    }
}

sub write
{
    my $self = shift;
    my ($path) = @_;

    my $g = $self->{g};
    my $grub_conf = $self->{grub_conf};
    my $grub_fs   = $self->{grub_fs};

    $path =~ /^$grub_fs(.*)/
       or v2vdie __x('Kernel {kernel} is not under grub tree {grub}',
                     kernel => $path, grub => $grub_fs);
    my $grub_path = $1;

    # Find the grub entry for the given kernel
    eval {
        my ($aug_path) =
            $g->aug_match("/files$grub_conf/title/kernel[. = '$grub_path']");

        v2vdie __x('Didn\'t find grub entry for kernel {kernel}',
                   kernel => $path)
            unless defined($aug_path);

        $aug_path =~ m{/files$grub_conf/title(?:\[(\d+)\])?/kernel}
            or die($aug_path);
        my $grub_index = defined($1) ? $1 - 1 : 0;

        $g->aug_set("/files$grub_conf/default", $grub_index);
        $g->aug_save();
    };
    augeas_error($g, $@) if ($@);
}


# Methods for inspecting and manipulating grub2. Note that we don't actually
# attempt to use grub2's configuration because it's utterly insane. Instead,
# we reverse engineer the way the config is automatically generated and use
# that instead.
package Sys::VirtConvert::Converter::RedHat::Grub2;

use Sys::VirtConvert::Util;
use Locale::TextDomain 'virt-v2v';

@Sys::VirtConvert::Converter::RedHat::Grub2::ISA =
    qw(Sys::VirtConvert::Converter::RedHat::Grub);

sub new
{
    my $class = shift;
    my ($g, $root) = @_;

    # Check we have a grub2 configuration
    die unless $g->exists('/boot/grub2/grub.cfg');

    my $self = {};
    bless($self, $class);

    $self->{g} = $g;
    $self->{root} = $root;

    return $self;
}

sub list_kernels
{
    my $self = shift;
    my $g = $self->{g};

    my @kernels;

    # Start by adding the default kernel
    my $default = $g->command(['grubby', '--default-kernel']);
    chomp($default);
    push(@kernels, $default) if length($default) > 0;

    # This is how the grub2 config generator enumerates kernels
    foreach my $kernel ($g->glob_expand('/boot/kernel-*'),
                        $g->glob_expand('/boot/vmlinuz-*'),
                        $g->glob_expand('/vmlinuz-*'))
    {
        push(@kernels, $kernel)
           unless $kernel =~ /\.(?:dpkg-.*|rpmsave|rpmnew)$/;
    }

    return @kernels;
}

sub update_console
{
    my $self = shift;
    my ($remove) = @_;

    my $g = $self->{g};

    my $cmdline =
        eval { $g->aug_get('/files/etc/sysconfig/grub/GRUB_CMDLINE_LINUX') };

    if (defined($cmdline) && $cmdline =~ /\bconsole=(?:x|h)vc0\b/) {
        if ($remove) {
            $cmdline =~ s/\bconsole=(?:x|h)vc0\b\s*//g;
        } else {
            $cmdline =~ s/\bconsole=(?:x|h)vc0\b/console=ttyS0/g;
        }

        eval {
            $g->aug_set('/files/etc/sysconfig/grub/GRUB_CMDLINE_LINUX', $cmdline);
            $g->aug_save();
        };
        augeas_error($g, $@) if ($@);

        # We need to re-generate the grub config if we've updated this file
        $g->command(['grub2-mkconfig', '-o', '/boot/grub2/grub.cfg']);
    }
}

sub check
{
    # Nothing required for grub2
}

sub write
{
    my $self = shift;
    my ($path) = @_;

    my $g = $self->{g};

    my $default = $g->command(['grubby', '--default-kernel']);
    chomp($default);

    if ($default ne $path) {
        $g->command(['grubby', '--set-default', $path]);
    }
}


package Sys::VirtConvert::Converter::RedHat;

use Sys::VirtConvert::Util qw(:DEFAULT augeas_error);
use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtConvert::Converter::RedHat - Convert a Red Hat based guest to run on KVM

=head1 SYNOPSIS

 use Sys::VirtConvert::Converter;

 Sys::VirtConvert::Converter->convert($g, $root, $meta);

=head1 DESCRIPTION

Sys::VirtConvert::Converter::RedHat converts a Red Hat based guest to use KVM.

=head1 METHODS

=over

=cut

sub _is_rhel_family
{
    my ($g, $root) = @_;

    return ($g->inspect_get_type($root) eq 'linux') &&
           ($g->inspect_get_distro($root) =~ /^(rhel|centos|scientificlinux|redhat-based)$/);
}

=item Sys::VirtConvert::Converter::RedHat->can_handle(g, root)

Return 1 if Sys::VirtConvert::Converter::RedHat can convert the given guest

=cut

sub can_handle
{
    my $class = shift;

    my ($g, $root) = @_;

    return ($g->inspect_get_type($root) eq 'linux' &&
            (_is_rhel_family($g, $root) || $g->inspect_get_distro($root) eq 'fedora'));
}

=item Sys::VirtConvert::Converter::RedHat->convert(g, root, config, meta, options)

Convert a Red Hat based guest. Assume that can_handle has previously returned 1.

=over

=item g

An initialised Sys::Guestfs handle

=item root

The root device of this operating system.

=item config

An initialised Sys::VirtConvert::Config

=item meta

Guest metadata.

=item options

A hashref of options which can influence the conversion

=back

=cut

sub convert
{
    my $class = shift;

    my ($g, $root, $config, $meta, $options) = @_;

    _clean_rpmdb($g);
    _init_selinux($g);
    _init_augeas($g);

    my $grub;
    $grub = eval { Sys::VirtConvert::Converter::RedHat::Grub2->new($g, $root) };
    $grub = eval { Sys::VirtConvert::Converter::RedHat::GrubLegacy->new($g, $root) }
        unless defined($grub);
    v2vdie __('No grub configuration found') unless defined($grub);

    # Un-configure HV specific attributes which don't require a direct
    # replacement
    _unconfigure_hv($g, $root);

    # Try to install the virtio capability
    my $virtio = _install_capability('virtio', $g, $root, $config, $meta, $grub);

    # Get an appropriate kernel, or install one if none is available
    my $kernel = _configure_kernel($virtio, $g, $root, $config, $meta, $grub);

    # Install user custom packages
    if (! _install_capability('user-custom', $g, $root, $config, $meta, $grub)) {
        logmsg WARN, __('Failed to install user-custom packages');
    }

    # Configure the rest of the system
    my $remove_serial_console = exists($options->{NO_SERIAL_CONSOLE});
    _configure_console($g, $grub, $remove_serial_console);

    _configure_display_driver($g, $root, $config, $meta, $grub);
    _remap_block_devices($meta, $virtio, $g, $root);
    _configure_kernel_modules($g, $virtio);
    _configure_boot($kernel, $virtio, $g, $root, $grub);

    my %guestcaps;

    $guestcaps{block} = $virtio == 1 ? 'virtio' : 'ide';
    $guestcaps{net}   = $virtio == 1 ? 'virtio' : 'e1000';
    $guestcaps{arch}  = _get_os_arch($g, $root, $grub);
    $guestcaps{acpi}  = _supports_acpi($g, $root, $guestcaps{arch});

    return \%guestcaps;
}

sub _init_selinux
{
    my ($g) = @_;

    # Assume SELinux isn't in use if load_policy isn't available
    return if(!$g->exists('/usr/sbin/load_policy'));

    # Actually loading the policy has proven to be problematic. We make whatever
    # changes are necessary, and make the guest relabel on the next boot.
    $g->touch('/.autorelabel');
}

sub _init_augeas
{
    my ($g) = @_;

    # Initialise augeas
    eval { $g->aug_init("/", 1) };
    augeas_error($g, $@) if ($@);
}

# Execute an augeas modprobe query against all possible modprobe locations
sub _aug_modprobe
{
    my ($g, $query) = @_;

    my @paths;
    for my $pattern ('/files/etc/conf.modules/alias',
                     '/files/etc/modules.conf/alias',
                     '/files/etc/modprobe.conf/alias',
                     '/files/etc/modprobe.d/*/alias') {
        push(@paths, $g->aug_match($pattern.'['.$query.']'));
    }

    return @paths;
}

# Check how new modules should be configured. Possibilities, in descending
# order of preference, are:
#   modprobe.d/
#   modprobe.conf
#   modules.conf
#   conf.modules
sub _discover_modpath
{
    my ($g) = @_;

    my $modpath;

    # Note that we're checking in ascending order of preference so that the last
    # discovered method will be chosen

    foreach my $file ('/etc/conf.modules', '/etc/modules.conf') {
        if($g->exists($file)) {
            $modpath = $file;
        }
    }

    if($g->exists("/etc/modprobe.conf")) {
        $modpath = "modprobe.conf";
    }

    # If the modprobe.d directory exists, create new entries in
    # modprobe.d/virtv2v-added.conf
    if($g->exists("/etc/modprobe.d")) {
        $modpath = "modprobe.d/virtv2v-added.conf";
    }

    v2vdie __('Unable to find any valid modprobe configuration')
        unless defined($modpath);

    return $modpath;
}

sub _configure_kernel_modules
{
    my ($g, $virtio, $modpath) = @_;

    # Make a note of whether we've added scsi_hostadapter
    # We need this on RHEL 4/virtio because mkinitrd can't detect root on
    # virtio. For simplicity we always ensure this is set for virtio disks.
    my $scsi_hostadapter = 0;

    eval {
        foreach my $path (_aug_modprobe($g, ". =~ regexp('eth[0-9]+')"))
        {
            $g->aug_set($path.'/modulename',
                        $virtio == 1 ? 'virtio_net' : 'e1000');
        }

        my @paths = _aug_modprobe($g, ". =~ regexp('scsi_hostadapter.*')");
        if ($virtio) {
            # There's only 1 scsi controller in the converted guest.
            # Convert only the first scsi_hostadapter entry to virtio.

            # Note that we delete paths in reverse order. This means we don't
            # have to worry about alias indices being changed.
            while (@paths > 1) {
                $g->aug_rm(pop(@paths));
            }

            if (@paths == 1) {
                $g->aug_set(pop(@paths).'/modulename', 'virtio_blk');
                $scsi_hostadapter = 1;
            }
        }

        else {
            # There's no scsi controller in an IDE guest
            while (@paths) {
                $g->aug_rm(pop(@paths));
            }
        }

        # Display a warning about any leftover xen modules which we haven't
        # converted
        my @xen_modules = qw(xennet xen-vnif xenblk xen-vbd);
        my $query = '('.join('|', @xen_modules).')';

        foreach my $path (_aug_modprobe($g, "modulename =~ regexp('$query')")) {
            my $device = $g->aug_get($path);
            my $module = $g->aug_get($path.'/modulename');

            logmsg WARN, __x("Don't know how to update ".
                             '{device}, which loads the {module} module.',
                             device => $device, module => $module);
        }

        # Add an explicit scsi_hostadapter if it wasn't there before
        if ($virtio && !$scsi_hostadapter) {
            my $modpath = _discover_modpath($g);

            $g->aug_set("/files$modpath/alias[last()+1]",
                        'scsi_hostadapter');
            $g->aug_set("/files$modpath/alias[last()]/modulename",
                        'virtio_blk');
        }

        $g->aug_save();
    };
    augeas_error($g, $@) if $@;
}

# We configure a console on ttyS0. Make sure existing console references use it.
# N.B. Note that the RHEL 6 xen guest kernel presents a console device called
# /dev/hvc0, whereas previous xen guest kernels presented /dev/xvc0. The regular
# kernel running under KVM also presents a virtio console device called
# /dev/hvc0, so ideally we would just leave it alone. However, RHEL 6 libvirt
# doesn't yet support this device so we can't attach to it. We therefore use
# /dev/ttyS0 for RHEL 6 anyway.
#
# If the target doesn't support a serial console, we want to remove all
# references to it instead.
sub _configure_console
{
    my ($g, $grub, $remove) = @_;

    # Look for gettys which use xvc0 or hvc0
    # RHEL 6 doesn't use /etc/inittab, but this doesn't hurt
    foreach my $augpath ($g->aug_match("/files/etc/inittab/*/process")) {
        my $proc = $g->aug_get($augpath);

        # If the process mentions xvc0, change it to ttyS0
        if ($proc =~ /\b(x|h)vc0\b/) {
            if ($remove) {
                $g->aug_rm($augpath.'/..');
            } else {
                $proc =~ s/\b(x|h)vc0\b/ttyS0/g;
                $g->aug_set($augpath, $proc);
            }
        }

        if ($remove && $proc =~ /\bttyS0\b/) {
            $g->aug_rm($augpath.'/..');
        }
    }

    # Replace any mention of xvc0 or hvc0 in /etc/securetty with ttyS0
    foreach my $augpath ($g->aug_match('/files/etc/securetty/*')) {
        my $tty = $g->aug_get($augpath);

        if($tty eq "xvc0" || $tty eq "hvc0") {
            if ($remove) {
                $g->aug_rm($augpath);
            } else {
                $g->aug_set($augpath, 'ttyS0');
            }
        }

        if ($remove && $tty eq 'ttyS0') {
            $g->aug_rm($augpath);
        }
    }

    $grub->update_console($remove);

    eval { $g->aug_save() };
    augeas_error($g, $@) if ($@);
}

sub _configure_display_driver
{
    my ($g, $root, $config, $meta, $grub) = @_;

    # Update the display driver if it exists
    my $updated = 0;
    eval {
        my $xorg;

        # Check which X configuration we have, and make augeas load it if
        # necessary
        if (! $g->exists('/etc/X11/xorg.conf') &&
            $g->exists('/etc/X11/XF86Config'))
        {
            $g->aug_set('/augeas/load/Xorg/incl[last()+1]',
                        '/etc/X11/XF86Config');

            # Reload to pick up the new configuration
            $g->aug_load();

            $xorg = '/etc/X11/XF86Config';
        } else {
            $xorg = '/etc/X11/xorg.conf';
        }

        foreach my $path ($g->aug_match('/files'.$xorg.'/Device/Driver')) {
            $g->aug_set($path, 'cirrus');
            $updated = 1;
        }

        # Remove VendorName and BoardName if present
        foreach my $path
            ($g->aug_match('/files'.$xorg.'/Device/VendorName'),
             $g->aug_match('/files'.$xorg.'/Device/BoardName'))
        {
            $g->aug_rm($path);
        }

        $g->aug_save();
    };

    # Propagate augeas errors
    augeas_error($g, $@) if ($@);

    # If we updated the X driver, check if X itself is actually installed. If it
    # is, ensure the cirrus driver is installed.
    if ($updated &&
        ($g->exists('/usr/bin/X') || $g->exists('/usr/bin/X11/X')) &&
        !_install_capability('cirrus', $g, $root, $config, $meta, $grub))
    {
        logmsg WARN, __('Display driver was updated to cirrus, but unable to '.
                        'install cirrus driver. X may not function correctly');
    }
}

# If the guest was shutdown uncleanly, it's possible that transient state was
# left lying around in the rpm database. Given we know that nothing is using the
# rpmdb at this point, it's safe to delete these files.
sub _clean_rpmdb
{
    my $g = shift;

    foreach my $f ($g->glob_expand('/var/lib/rpm/__db.00?')) {
        $g->rm($f);
    }
}

# Use various methods to try to work out what Linux kernel we've got.
# Returns a hashref containing:
#   path => path to kernel (same as $path variable passed in)
#   package => base package name (eg. "kernel", "kernel-PAE")
#   version => version string
#   modules => array ref list of modules (paths to *.ko files)
#   arch => architecture of the kernel
sub _inspect_linux_kernel
{
    my ($g, $path) = @_;

    my %kernel = ();

    $kernel{path} = $path;

    # If this is a packaged kernel, try to work out the name of the package
    # which installed it. This lets us know what to install to replace it with,
    # e.g. kernel, kernel-smp, kernel-hugemem, kernel-PAE
    my $package = eval { $g->command(['rpm', '-qf', '--qf',
                                      '%{NAME}', $path]) };
    $kernel{package} = $package if defined($package);;

    # Try to get the kernel version by running file against it
    my $version;
    my $filedesc = $g->file($path);
    if($filedesc =~ /^$path: Linux kernel .*\bversion\s+(\S+)\b/) {
        $version = $1;
    }

    # Sometimes file can't work out the kernel version, for example because it's
    # a Xen PV kernel. In this case try to guess the version from the filename
    else {
        if($path =~ m{/boot/vmlinuz-(.*)}) {
            $version = $1;

            # Check /lib/modules/$version exists
            if(!$g->is_dir("/lib/modules/$version")) {
                warn __x("Didn't find modules directory {modules} for kernel ".
                         "{path}\n", modules => "/lib/modules/$version",
                         path => $path);

                # Give up
                return undef;
            }
        } else {
            warn __x("Couldn't guess kernel version number from path for ".
                     "kernel {path}\n", path => $path);

            # Give up
            return undef;
        }
    }

    $kernel{version} = $version;

    # List modules.
    my @modules;
    my $any_module;
    my $prefix = "/lib/modules/$version";
    foreach my $module ($g->find ($prefix)) {
        if ($module =~ m{/([^/]+)\.(?:ko|o)$}) {
            $any_module = "$prefix$module" unless defined $any_module;
            push @modules, $1;
        }
    }

    $kernel{modules} = \@modules;

    # Determine kernel architecture by looking at the arch
    # of any kernel module.
    $kernel{arch} = $g->file_architecture ($any_module);

    return \%kernel;
}

sub _configure_kernel
{
    my ($virtio, $g, $root, $config, $meta, $grub) = @_;

    # Pick first appropriate kernel returned by list_kernels
    my $boot_kernel;
    foreach my $path ($grub->list_kernels()) {
        my $kernel = _inspect_linux_kernel($g, $path);
        my $version = $kernel->{version};

        # Skip foreign kernels
        next if _is_hv_kernel($g, $version);

        # If we're configuring virtio, check this kernel supports it
        next if ($virtio && !_supports_virtio($version, $g));

        $boot_kernel = $version;
        last;
    }

    # There should be an installed virtio capable kernel if virtio was installed
    die("virtio configured, but no virtio kernel found")
        if ($virtio && !defined($boot_kernel));

    # If none of the installed kernels are appropriate, install a new one
    if(!defined($boot_kernel)) {
        my ($kernel_pkg, $kernel_arch, undef) =
           _discover_kernel($g, $root, $grub);

        # If the guest is using a Xen PV kernel, choose an appropriate
        # normal kernel replacement
        if ($kernel_pkg eq "kernel-xen" || $kernel_pkg eq "kernel-xenU") {
            $kernel_pkg = _get_replacement_kernel_name($g, $root,
                                                       $kernel_arch, $meta);

            # Check there isn't already one installed
            my ($kernel) = _get_installed("$kernel_pkg.$kernel_arch", $g);
            $boot_kernel = $kernel->[1].'-'.$kernel->[2].'.'.$kernel_arch
                if defined($kernel);
        }

        if (!defined($boot_kernel)) {
            # List of kernels before the new kernel installation
            my @k_before = $g->glob_expand('/boot/vmlinuz-*');

            if (_install_any([$kernel_pkg, $kernel_arch], undef, undef,
                             $g, $root, $config, $grub))
            {
                # Figure out which kernel has just been installed
                my $version;
                foreach my $k ($g->glob_expand('/boot/vmlinuz-*')) {
                    if (!grep(/^$k$/, @k_before)) {
                        # Check which directory in /lib/modules the kernel rpm
                        # creates
                        foreach my $file
                                ($g->command_lines(['rpm', '-qlf', $k]))
                        {
                            next unless ($file =~ m{^/lib/modules/([^/]+)$});

                            if ($g->is_dir("/lib/modules/$1")) {
                                $boot_kernel = $1;
                                last;
                            }
                        }

                        last if defined($boot_kernel);
                    }
                }

                v2vdie __x('Couldn\'t determine version of installed kernel')
                    unless defined($boot_kernel);
            } else {
                v2vdie __x('Failed to find a {name} package to install',
                           name => "kernel_pkg.$kernel_arch");
            }
        }
    }

    # Check we have a bootable kernel.
    v2vdie __('No bootable kernels installed, and no replacement '.
              "is available.\nUnable to continue.")
        unless defined($boot_kernel);

    # Ensure DEFAULTKERNEL is set to boot kernel package name
    # It's not fatal if this rpm command fails
    my ($kernel_pkg) =
        eval { $g->command_lines(['rpm', '-qf', "/lib/modules/$boot_kernel",
                                         '--qf', '%{NAME}\n']) };
    if (defined($kernel_pkg) && $g->exists('/etc/sysconfig/kernel')) {
        eval {
            foreach my $path ($g->aug_match('/files/etc/sysconfig/kernel'.
                                            '/DEFAULTKERNEL/value'))
            {
                $g->aug_set($path, $kernel_pkg);
            }

            $g->aug_save();
        };
        # Propagate augeas errors
        augeas_error($g, $@) if ($@);
    }

    return $boot_kernel;
}

sub _configure_boot
{
    my ($kernel, $virtio, $g, $root, $grub) = @_;

    if($virtio) {
        # The order of modules here is deliberately the same as the order
        # specified in the postinstall script of kmod-virtio in RHEL3. The
        # reason is that the probing order determines the major number of vdX
        # block devices. If we change it, RHEL 3 KVM guests won't boot.
        _prepare_bootable($g, $root, $grub, $kernel, "virtio", "virtio_ring",
                                                     "virtio_blk", "virtio_net",
                                                     "virtio_pci");
    } else {
        _prepare_bootable($g, $root, $grub, $kernel, "sym53c8xx");
    }
}

# Get the target architecture from the default boot kernel
sub _get_os_arch
{
    my ($g, $root, $grub) = @_;

    # Get the arch of the default kernel
    my @kernels = $grub->list_kernels();
    my $path = $kernels[0] if @kernels > 0;
    my $kernel = _inspect_linux_kernel($g, $path) if defined($path);
    my $arch = $kernel->{arch} if defined($kernel);

    # Use the libguestfs-detected arch if the above failed
    $arch = $g->inspect_get_arch($root) unless defined($arch);

    # Default to x86_64 if we still didn't find an architecture
    return 'x86_64' unless defined($arch);

    # We want an i686 guest for i[345]86
    return 'i686' if($arch =~ /^i[345]86$/);

    return $arch;
}

# Determine if a specific kernel is hypervisor-specific
sub _is_hv_kernel
{
    my ($g, $version) = @_;

    # Xen PV kernels can be distinguished from other kernels by their inclusion
    # of the xennet driver
    foreach my $entry ($g->find("/lib/modules/$version/")) {
        return 1 if $entry =~ /(^|\/)xennet\.k?o$/;
    }

    return 0;
}

sub _remove_applications
{
    my ($g, @apps) = @_;

    # Nothing to do if we were given an empty list
    return if scalar(@apps) == 0;

    $g->command(['rpm', '-e', @apps]);

    # Make augeas reload in case the removal changed anything
    eval { $g->aug_load() };
    augeas_error($g, $@) if ($@);
}

sub _get_application_owner
{
    my ($file, $g) = @_;

    return $g->command(['rpm', '-qf', $file]);
}

sub _unconfigure_hv
{
    my ($g, $root) = @_;

    my @apps = $g->inspect_list_applications($root);

    _unconfigure_xen($g, \@apps);
    _unconfigure_vbox($g, \@apps);
    _unconfigure_vmware($g, \@apps);
    _unconfigure_citrix($g, \@apps);
}

# Unconfigure Xen specific guest modifications
sub _unconfigure_xen
{
    my ($g, $apps) = @_;

    # Look for kmod-xenpv-*, which can be found on RHEL 3 machines
    my @remove;
    foreach my $app (@$apps) {
        my $name = $app->{app_name};

        if($name =~ /^kmod-xenpv(-.*)?$/) {
            push(@remove, $name);
        }
    }
    _remove_applications($g, @remove);

    # Undo related nastiness if kmod-xenpv was installed
    if(scalar(@remove) > 0) {
        # kmod-xenpv modules may have been manually copied to other kernels.
        # Hunt them down and destroy them.
        foreach my $dir (grep(m{/xenpv$}, $g->find('/lib/modules'))) {
            $dir = '/lib/modules/'.$dir;

            # Check it's a directory
            next unless($g->is_dir($dir));

            # Check it's not owned by an installed application
            eval { _get_application_owner($dir, $g) };

            # Remove it if _get_application_owner didn't find an owner
            if($@) {
                $g->rm_rf($dir);
            }
        }

        # rc.local may contain an insmod or modprobe of the xen-vbd driver
        my @rc_local = eval { $g->read_lines('/etc/rc.local') };
        if ($@) {
            logmsg WARN, __x('Unable to open /etc/rc.local: {error}',
                             error => $@);
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

# Unconfigure VirtualBox specific guest modifications
sub _unconfigure_vbox
{
    my ($g, $apps) = @_;

    # Uninstall VirtualBox Guest Additions
    my @remove;
    foreach my $app (@$apps) {
        my $name = $app->{app_name};

        if ($name eq "virtualbox-guest-additions") {
            push(@remove, $name);
        }
    }
    _remove_applications($g, @remove);

    # VirtualBox Guest Additions may have been installed from tarball, in which
    # case the above won't detect it. Look for the uninstall tool, and run it
    # if it's present.
    #
    # Note that it's important we do this early in the conversion process, as
    # this uninstallation script naively overwrites configuration files with
    # versions it cached prior to installation.
    my $vboxconfig = '/var/lib/VBoxGuestAdditions/config';
    if ($g->exists($vboxconfig)) {
        my $vboxuninstall;
        foreach (split /\n/, $g->cat($vboxconfig)) {
            if ($_ =~ /^INSTALL_DIR=(.*$)/) {
                $vboxuninstall = $1 . '/uninstall.sh';
            }
        }
        if ($g->exists($vboxuninstall)) {
            eval { $g->command([$vboxuninstall]) };
            logmsg WARN, __x('VirtualBox Guest Additions were detected, but '.
                             'uninstallation failed. The error message was: '.
                             '{error}', error => $@) if $@;

            # Reload augeas to detect changes made by vbox tools uninstallation
            eval { $g->aug_load() };
            augeas_error($g, $@) if $@;
        }
    }
}

# Unconfigure VMware specific guest modifications
sub _unconfigure_vmware
{
    my ($g, $apps) = @_;

    # Look for any configured vmware yum repos, and disable them
    foreach my $repo ($g->aug_match(
        '/files/etc/yum.repos.d/*/*'.
        '[baseurl =~ regexp(\'https?://([^/]+\.)?vmware\.com/.*\')]'))
    {
        eval {
            $g->aug_set($repo.'/enabled', 0);
            $g->aug_save();
        };
        augeas_error($g, $@) if ($@);
    }

    # Uninstall VMwareTools
    my @remove;
    my @libraries;
    foreach my $app (@$apps) {
        my $name = $app->{app_name};

        if ($name =~ /^vmware-tools-libraries-/) {
            push(@libraries, $name);
        }
        elsif ($name eq "VMwareTools" || $name =~ /^(?:kmod-)?vmware-tools-/) {
            push(@remove, $name);
        }
    }

    # VMware tools includes 'libraries' packages which provide custom versions
    # of core functionality. We need to install non-custom versions of
    # everything provided by these packages before attempting to uninstall them,
    # or we'll hit dependency issues
    if (@libraries > 0) {
        # We only support removal of these libraries packages on systems which
        # use yum.
        if ($g->exists('/usr/bin/yum')) {
            _net_run($g, sub {
                foreach my $library (@libraries) {
                    eval {
                        my @provides = $g->command_lines
                                (['rpm', '-q', '--provides', $library]);

                        # The packages also explicitly provide themselves.
                        # Filter this out.
                        @provides = grep {$_ !~ /$library/}

                        # Trim whitespace
                                    map { s/^\s*(\S+)\s*$/$1/; $_ } @provides;

                        # Install the dependencies with yum. We use yum
                        # explicitly here, as up2date wouldn't work anyway and
                        # local install is impractical due to the large number
                        # of required dependencies out of our control.
                        my %alts;
                        foreach my $alt ($g->command_lines
                                       (['yum', '-q', 'resolvedep', @provides]))
                        {
                            $alts{$alt} = 1;
                        }

                        $g->command(['yum', 'install', '-y', keys(%alts)]);

                        push(@remove, $library);
                    };
                    logmsg WARN, __x('Failed to install replacement '.
                                     'dependencies for {lib}. Package will '.
                                     'not be uninstalled. Error was: {error}',
                                     lib => $library, error => $@) if $@;
                }
            });
        }
    }

    _remove_applications($g, @remove);

    # VMwareTools may have been installed from tarball, in which case the above
    # won't detect it. Look for the uninstall tool, and run it if it's present.
    #
    # Note that it's important we do this early in the conversion process, as
    # this uninstallation script naively overwrites configuration files with
    # versions it cached prior to installation.
    my $vmwaretools = '/usr/bin/vmware-uninstall-tools.pl';
    if ($g->exists($vmwaretools)) {
        eval { $g->command([$vmwaretools]) };
        logmsg WARN, __x('VMware Tools was detected, but uninstallation '.
                         'failed. The error message was: {error}',
                         error => $@) if $@;

        # Reload augeas to detect changes made by vmware tools uninstallation
        eval { $g->aug_load() };
        augeas_error($g, $@) if $@;
    }
}

sub _unconfigure_citrix
{
    my ($g, $apps) = @_;

    # Look for xe-guest-utilities*
    my @remove;
    foreach my $app (@$apps) {
        my $name = $app->{app_name};

        if($name =~ /^xe-guest-utilities(-.*)?$/) {
            push(@remove, $name);
        }
    }
    _remove_applications($g, @remove);

    # Installing these guest utilities automatically unconfigures ttys in
    # /etc/inittab if the system uses it. We need to put them back.
    if (scalar(@remove) > 0) {
        eval {
            my $updated = 0;
            for my $commentp ($g->aug_match('/files/etc/inittab/#comment')) {
                my $comment = $g->aug_get($commentp);

                # The entries in question are named 1-6, and will normally be
                # active in runlevels 2-5. They will be gettys. We could be
                # extremely prescriptive here, but allow for a reasonable amount
                # of variation just in case.
                next unless $comment =~ /^([1-6]):([2-5]+):respawn:(.*)/;

                my $name = $1;
                my $runlevels = $2;
                my $process = $3;

                next unless $process =~ /getty/;

                # Create a new entry immediately after the comment
                $g->aug_insert($commentp, $name, 0);
                $g->aug_set("/files/etc/inittab/$name/runlevels", $runlevels);
                $g->aug_set("/files/etc/inittab/$name/action", 'respawn');
                $g->aug_set("/files/etc/inittab/$name/process", $process);

                # Create a variable to point to the comment node so we can
                # delete it later. If we deleted it here it would invalidate
                # subsquent comment paths returned by aug_match.
                $g->aug_defvar("delete$updated", $commentp);

                $updated++;
            }

            # Delete all the comments
            my $i = 0;
            while ($i < $updated) {
                $g->aug_rm("\$delete$i");
                $i++;
            }

            $g->aug_save();
        };
        augeas_error($g, $@) if ($@);
    }
}

sub _install_capability
{
    my ($name, $g, $root, $config, $meta, $grub) = @_;

    my $cap = eval { $config->match_capability($g, $root, $name) };
    if ($@) {
        warn($@);
        return 0;
    }

    if (!defined($cap)) {
        logmsg WARN, __x('{name} capability not found in configuration',
                         name => $name);
        return 0;
    }

    my @install;
    my @upgrade;
    my $kernel;
    foreach my $name (keys(%$cap)) {
        my $props = $cap->{$name};
        my $ifinstalled = $props->{ifinstalled};

        # Parse epoch, version and release from minversion
        my ($min_epoch, $min_version, $min_release);
        if (exists($props->{minversion})) {
            eval {
                ($min_epoch, $min_version, $min_release) =
                    _parse_evr($props->{minversion});
            };
            v2vdie __x('Unrecognised format for {field} in config: '.
                       '{value}. {field} must be in the format '.
                       '[epoch:]version[-release].',
                       field => 'minversion', value => $props->{minversion})
                if $@;
        }

        # Kernels are special
        if ($name eq 'kernel') {
            my ($kernel_pkg, $kernel_arch, $kernel_rpmver) =
                _discover_kernel($g, $root, $grub);

            # If we didn't establish a kernel version, assume we have to upgrade
            # it.
            if (!defined($kernel_rpmver)) {
                $kernel = [$kernel_pkg, $kernel_arch];
            }

            else {
                my ($kernel_epoch, $kernel_ver, $kernel_release) =
                    eval { _parse_evr($kernel_rpmver) };
                if ($@) {
                    # Don't die here, just make best effort to do a version
                    # comparison by directly comparing the full strings
                    $kernel_epoch = undef;
                    $kernel_ver = $kernel_rpmver;
                    $kernel_release = undef;

                    $min_epoch = undef;
                    $min_version = $props->{minversion};
                    $min_release = undef;
                }

                # If the guest is using a Xen PV kernel, choose an appropriate
                # normal kernel replacement
                if ($kernel_pkg eq "kernel-xen" || $kernel_pkg eq "kernel-xenU")
                {
                    $kernel_pkg =
                        _get_replacement_kernel_name($g, $root, $kernel_arch,
                                                     $meta);

                    # Check if we've got already got an appropriate kernel
                    my ($inst) =
                        _get_installed("$kernel_pkg.$kernel_arch", $g);

                    if (!defined($inst) ||
                        (defined($min_version) &&
                         _evr_cmp($inst->[0], $inst->[1], $inst->[2],
                                  $min_epoch, $min_version, $min_release) < 0))
                    {
                        # filter out xen/xenU from release field
                        if (defined($kernel_release) &&
                            $kernel_release =~ /^(\S+?)(xen)?(U)?$/)
                        {
                            $kernel_release = $1;
                        }

                        # If the guest kernel is new enough, but PV, try to
                        # replace it with an equivalent version FV kernel
                        if (!defined($min_version) ||
                            _evr_cmp($kernel_epoch, $kernel_ver,
                                     $kernel_release,
                                     $min_epoch, $min_version,
                                     $min_release) >= 0)
                        {
                            $kernel = [$kernel_pkg, $kernel_arch,
                                       $kernel_epoch, $kernel_ver,
                                       $kernel_release];
                        }

                        # Otherwise, just grab the latest
                        else {
                            $kernel = [$kernel_pkg, $kernel_arch];
                        }
                    }
                }

                # If the kernel is too old, grab the latest replacement
                elsif (defined($min_version) &&
                       _evr_cmp($kernel_epoch, $kernel_ver, $kernel_release,
                                $min_epoch, $min_version, $min_release) < 0)
                {
                    $kernel = [$kernel_pkg, $kernel_arch];
                }
            }
        }

        else {
            my @installed = _get_installed($name, $g);

            # Ignore an 'ifinstalled' dep if it's not currently installed
            next if (@installed == 0 && $ifinstalled);

            # Ok if any version is installed and no minversion was specified
            next if (@installed > 0 && !defined($min_version));

            if (defined($min_version)) {
                # Check if any installed version meets the minimum version
                my $found = 0;
                foreach my $app (@installed) {
                    my ($epoch, $version, $release) = @$app;

                    if (_evr_cmp($app->[0], $app->[1], $app->[2],
                                 $min_epoch, $min_version, $min_release) >= 0) {
                        $found = 1;
                        last;
                    }
                }

                # Install the latest available version of the dep if it wasn't
                # found
                if (!$found) {
                    if (@installed == 0) {
                        push(@install, [$name]);
                    } else {
                        push(@upgrade, [$name]);
                    }
                }
            } else {
                push(@install, [$name]);
            }
        }
    }

    # Capability is already installed
    if (!defined($kernel) && @install == 0 && @upgrade == 0) {
        return 1;
    }

    my $success = _install_any($kernel, \@install, \@upgrade,
                               $g, $root, $config, $grub);

    return $success;
}

sub _net_run {
    my ($g, $sub) = @_;

    my $resolv_bak = $g->exists('/etc/resolv.conf');
    $g->mv('/etc/resolv.conf', '/etc/resolv.conf.v2vtmp') if ($resolv_bak);

    # XXX We should get the nameserver from the appliance here, but
    # there's no current api other than debug to do this.
    $g->write_file('/etc/resolv.conf', "nameserver 169.254.2.3", 0);

    eval &$sub();
    my $err = $@;

    $g->mv('/etc/resolv.conf.v2vtmp', '/etc/resolv.conf') if ($resolv_bak);

    die $err if $err;
}

sub _install_any
{
    my ($kernel, $install, $upgrade, $g, $root, $config, $grub) = @_;

    # If we're installing a kernel, check which kernels are there first
    my @k_before = $g->glob_expand('/boot/vmlinuz-*') if defined($kernel);

    my $success = 0;
    _net_run($g, sub {
        eval {
            # Try to fetch these dependencies using the guest's native update
            # tool
            $success = _install_up2date($kernel, $install, $upgrade, $g);
            $success = _install_yum($kernel, $install, $upgrade, $g)
                unless ($success);

            # Fall back to local config if the above didn't work
            $success = _install_config($kernel, $install, $upgrade,
                                       $g, $root, $config)
                unless ($success);
        };
        warn($@) if $@;
    });

    # Make augeas reload to pick up any altered configuration
    eval { $g->aug_load() };
    augeas_error($g, $@) if ($@);

    # Installing a new kernel in RHEL 5 under libguestfs fails to add a grub
    # entry. Find the kernel we installed and ensure it has a grub entry.
    if (defined($kernel)) {
        foreach my $k ($g->glob_expand('/boot/vmlinuz-*')) {
            if (!grep(/^$k$/, @k_before)) {
                $grub->check($k);
                last;
            }
        }
    }

    return $success;
}

sub _install_up2date
{
    my ($kernel, $install, $upgrade, $g) = @_;

    # Check this system has actions.packages
    return 0 unless ($g->exists('/usr/bin/up2date'));

    # Check this system is registered to rhn
    return 0 unless ($g->exists('/etc/sysconfig/rhn/systemid'));

    my @pkgs;
    foreach my $pkg ($kernel, @$install, @$upgrade) {
        next unless defined($pkg);

        # up2date doesn't do arch
        my ($name, undef, $epoch, $version, $release) = @$pkg;

        $epoch   ||= "";
        $version ||= "";
        $release ||= "";

        push(@pkgs, "['$name', '$version', '$release', '$epoch']");
    }

    eval {
         $g->command(['/usr/bin/python', '-c',
                     "import sys; sys.path.append('/usr/share/rhn'); ".
                     "import actions.packages;                       ".
                     "actions.packages.cfg['forceInstall'] = 1;      ".
                     "ret = actions.packages.update([".join(',', @pkgs)."]); ".
                     "sys.exit(ret[0]);                              "]);
    };
    if ($@) {
        logmsg WARN, __x('Failed to install packages using up2date. '.
                         'Error message was: {error}', error => $@);
        return 0;
    }

    return 1;
}

sub _install_yum
{
    my ($kernel, $install, $upgrade, $g) = @_;

    # Check this system has yum installed
    return 0 unless ($g->exists('/usr/bin/yum'));

    # Install or upgrade the kernel?
    # If it isn't installed (because we're replacing a PV kernel), we need to
    # install
    # If we're installing a specific version, we need to install
    # If the kernel package we're installing is already installed and we're
    # just upgrading to the latest version, we need to upgrade
    if (defined($kernel)) {
        my @installed = _get_installed($kernel->[0], $g);

        # Don't modify the contents of $install and $upgrade in case we fall
        # through and they're reused in another function
        if (@installed == 0 || defined($kernel->[2])) {
            my @tmp = defined($install) ? @$install : ();
            push(@tmp, $kernel);
            $install = \@tmp;
        } else {
            my @tmp = defined($upgrade) ? @$upgrade : ();
            push(@tmp, $kernel);
            $upgrade = \@tmp;
        }
    }

    my $success = 1;
    YUM: foreach my $task (
        [ "install", $install, qr/(^No package|already installed)/ ],
        [ "upgrade", $upgrade, qr/^No Packages/ ]
    ) {
        my ($action, $list, $failure) = @$task;

        # We can't do these all in a single transaction, because yum offers us
        # no way to tell if a transaction partially succeeded
        foreach my $entry (@$list) {
            next unless (defined($entry));

            # You can't specify epoch without architecture to yum, so we just
            # ignore epoch and hope
            my ($name, undef, undef, $version, $release) = @$entry;

            # Construct n-v-r
            my $pkg = $name;
            $pkg .= "-$version" if (defined($version));
            $pkg .= "-$release" if (defined($release));

            my @output =
                eval { $g->sh_lines("LANG=C /usr/bin/yum -y $action $pkg") };
            if ($@) {
                logmsg WARN, __x('Failed to install packages using yum. '.
                                 'Output was: {output}', output => $@);
                $success = 0;
                last YUM;
            }

            foreach my $line (@output) {
                # Yum probably just isn't configured. Don't bother with an error
                # message
                if ($line =~ /$failure/) {
                    $success = 0;
                    last YUM;
                }
            }
        }
    }

    return $success;
}

sub _install_config
{
    my ($kernel_naevr, $install, $upgrade, $g, $root, $config) = @_;

    my ($kernel, $user);
    if (defined($kernel_naevr)) {
        my ($kernel_pkg, $kernel_arch) = @$kernel_naevr;

        ($kernel, $user) =
            $config->match_app($g, $root, $kernel_pkg, $kernel_arch);
    } else {
        $user = [];
    }

    foreach my $pkg (@$install, @$upgrade) {
        push(@$user, $pkg->[0]);
    }

    my @missing;
    if (defined($kernel)) {
        my $transfer_path = $config->get_transfer_path($kernel);
        if (!defined($transfer_path) || !$g->exists($transfer_path)) {
            push(@missing, $kernel);
        }
    }

    my @user_paths = _get_deppaths($g, $root, $config,
                                   \@missing, $g->inspect_get_arch($root), @$user);

    # We can't proceed if there are any files missing
    v2vdie __x('Installation failed because the following '.
               'files referenced in the configuration file are '.
               'required, but missing: {list}',
               list => join(' ', @missing)) if scalar(@missing) > 0;
    # Install any non-kernel requirements
    _install_rpms($g, $config, 1, @user_paths);

    if (defined($kernel)) {
        _install_rpms($g, $config, 0, ($kernel));
    }

    return 1;
}

# Install a set of rpms
sub _install_rpms
{
    local $_;
    my ($g, $config, $upgrade, @rpms) = @_;

    # Nothing to do if we got an empty set
    return if(scalar(@rpms) == 0);

    # All paths are relative to the transfer mount. Need to make them absolute.
    # No need to check get_transfer_path here as all paths have been previously
    # checked
    @rpms = map { $_ = $config->get_transfer_path($_) } @rpms;

    $g->command(['rpm', $upgrade == 1 ? '-U' : '-i', @rpms]);

    # Reload augeas in case the rpm installation changed anything
    eval { $g->aug_load() };
    augeas_error($g, $@) if($@);
}

# Return a list of dependency paths which need to be installed for the given
# apps
sub _get_deppaths
{
    my ($g, $root, $config, $missing, $arch, @apps) = @_;

    my %required;
    foreach my $app (@apps) {
        my ($path, $deps) = $config->match_app($g, $root, $app, $arch);

        my $transfer_path = $config->get_transfer_path($path);
        my $exists = defined($transfer_path) && $g->exists($transfer_path);

        if (!$exists) {
            push(@$missing, $path);
        }

        if (!$exists || !_newer_installed($transfer_path, $g, $config)) {
            $required{$path} = 1;

            foreach my $deppath (_get_deppaths($g, $root, $config,
                                               $missing, $arch, @$deps))
            {
                $required{$deppath} = 1;
            }
        }

        # For x86_64, also check if there is any i386 or i686 version installed.
        # If there is, check if it needs to be upgraded.
        if ($arch eq 'x86_64') {
            $path = undef;
            $deps = undef;

            # It's not an error if no i386 package is available
            eval {
                ($path, $deps) = $config->match_app($g, $root, $app, 'i386');
            };

            if (defined($path)) {
                $transfer_path = $config->get_transfer_path($path);
                if (!defined($transfer_path) || !$g->exists($transfer_path)) {
                    push(@$missing, $path);

                    foreach my $deppath (_get_deppaths($g, $root, $config,
                                                      $missing, 'i386', @$deps))
                    {
                        $required{$deppath} = 1;
                    }
                }
            }
        }
    }

    return keys(%required);
}

# Return 1 if the requested rpm, or a newer version, is installed
# Return 0 otherwise
sub _newer_installed
{
    my ($rpm, $g, $config) = @_;

    my ($name, $epoch, $version, $release, $arch) =
        _get_nevra($rpm, $g, $config);

    my @installed = _get_installed("$name.$arch", $g);

    # Search installed rpms matching <name>.<arch>
    foreach my $pkg (@installed) {
        next if _evr_cmp($pkg->[0], $pkg->[1], $pkg->[2],
                         $epoch, $version, $release) < 0;
        return 1;
    }

    return 0;
}

sub _get_nevra
{
    my ($rpm, $g, $config) = @_;

    # Get NEVRA for the rpm to be installed
    my $nevra = $g->command(['rpm', '-qp', '--qf',
                             '%{NAME} %{EPOCH} %{VERSION} %{RELEASE} %{ARCH}',
                             $rpm]);

    $nevra =~ /^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)$/
        or die("Unexpected return from rpm command: $nevra");
    my ($name, $epoch, $version, $release, $arch) = ($1, $2, $3, $4, $5);

    # Ensure epoch is always numeric
    $epoch = 0 if('(none)' eq $epoch);

    return ($name, $epoch, $version, $release, $arch);
}

# Inspect the guest to work out what kernel package is in use
# Returns ($kernel_pkg, $kernel_arch)
sub _discover_kernel
{
    my ($g, $root, $grub) = @_;

    # Get a current bootable kernel, preferrably the default
    my $kernel_pkg;
    my $kernel_arch;
    my $kernel_ver;

    foreach my $path ($grub->list_kernels()) {
        my $kernel = _inspect_linux_kernel($g, $path);

        # Check its architecture is known
        $kernel_arch = $kernel->{arch};
        next unless (defined($kernel_arch));

        # Get the kernel package name
        $kernel_pkg = $kernel->{package};

        # Get the kernel package version
        $kernel_ver = $kernel->{version};

        last;
    }

    # Default to 'kernel' if package name wasn't discovered
    $kernel_pkg = "kernel" if (!defined($kernel_pkg));

    # Default the kernel architecture to the userspace architecture if it wasn't
    # directly detected
    $kernel_arch = $g->inspect_get_arch($root) unless defined($kernel_arch);

    # We haven't supported anything other than i686 for the kernel on 32 bit for
    # a very long time.
    $kernel_arch = 'i686' if ('i386' eq $kernel_arch);

    return ($kernel_pkg, $kernel_arch, $kernel_ver);
}

sub _get_replacement_kernel_name
{
    my ($g, $root, $arch, $meta) = @_;

    # Make an informed choice about a replacement kernel for distros we know
    # about

    # RHEL 5
    if (_is_rhel_family($g, $root) && $g->inspect_get_major_version($root) eq '5') {
        if ($arch eq 'i686') {
            # XXX: This assumes that PAE will be available in the hypervisor.
            # While this is almost certainly true, it's theoretically possible
            # that it isn't. The information we need is available in the
            # capabilities XML.  If PAE isn't available, we should choose
            # 'kernel'.
            return 'kernel-PAE';
        }

        # There's only 1 kernel package on RHEL 5 x86_64
        else {
            return 'kernel';
        }
    }

    # RHEL 4
    elsif (_is_rhel_family($g, $root) && $g->inspect_get_major_version($root) eq '4') {
        if ($arch eq 'i686') {
            # If the guest has > 10G RAM, give it a hugemem kernel
            if ($meta->{memory} > 10 * 1024 * 1024 * 1024) {
                return 'kernel-hugemem';
            }

            # SMP kernel for guests with >1 CPU
            elsif ($meta->{cpus} > 1) {
                return 'kernel-smp';
            }

            else {
                return 'kernel';
            }
        }

        else {
            if ($meta->{cpus} > 8) {
                return 'kernel-largesmp';
            }

            elsif ($meta->{cpus} > 1) {
                return 'kernel-smp';
            }
            else {
                return 'kernel';
            }
        }
    }

    # RHEL 3 didn't have a xen kernel

    # XXX: Could do with a history of Fedora kernels in here

    # For other distros, be conservative and just return 'kernel'
    return 'kernel';
}

sub _get_installed
{
    my ($name, $g) = @_;

    my $rpmcmd = ['rpm', '-q', '--qf', '%{EPOCH} %{VERSION} %{RELEASE}\n',
                  $name];
    my @output = eval { $g->command_lines($rpmcmd) };
    if ($@) {
        # RPM command returned non-zero. This might be because there was
        # actually an error, or might just be because the package isn't
        # installed.
        # Unfortunately, rpm sent its error to stdout instead of stderr, and
        # command_lines only gives us stderr in $@. To get round this we'll
        # execute the command again, sending all output to stdout and ignoring
        # failure. If the output contains 'not installed', we'll assume it's not
        # a real error.
        my $error = $g->sh("LANG=C '".join("' '", @$rpmcmd)."' 2>&1 ||:");

        return () if ($error =~ /not installed/);

        v2vdie __x('Error running {command}: {error}',
                   command => join(' ', @$rpmcmd), error => $error);
    }

    my @installed = ();
    foreach my $installed (@output) {
        $installed =~ /^(\S+)\s+(\S+)\s+(\S+)$/
            or die("Unexpected return from rpm command: $installed");
        my ($epoch, $version, $release) = ($1, $2, $3);

        # Ensure iepoch is always numeric
        $epoch = 0 if('(none)' eq $epoch);

        push(@installed, [$epoch, $version, $release]);
    }

    return sort { _evr_cmp($a->[0], $a->[1], $a->[2],
                           $b->[0], $b->[1], $b->[2]) } @installed;
}

sub _parse_evr
{
    my ($evr) = @_;

    $evr =~ /^(?:(\d+):)?([^-]+)(?:-(\S+))?$/ or die();

    my $epoch = $1;
    my $version = $2;
    my $release = $3;

    return ($epoch, $version, $release);
}

sub _evr_cmp
{
    my ($e1, $v1, $r1, $e2, $v2, $r2) = @_;

    # Treat epoch as zero if undefined
    $e1 ||= 0;
    $e2 ||= 0;

    return -1 if ($e1 < $e2);
    return 1 if ($e1 > $e2);

    # version must be defined
    my $cmp = _rpmvercmp($v1, $v2);
    return $cmp if ($cmp != 0);

    # Treat release as the empty string if undefined
    $r1 ||= "";
    $r2 ||= "";

    return _rpmvercmp($r1, $r2);
}

# An implementation of rpmvercmp. Compares two rpm version/release numbers and
# returns -1/0/1 as appropriate.
# Note that this is intended to have the exact same behaviour as the real
# rpmvercmp, not be in any way sane.
sub _rpmvercmp
{
    my ($a, $b) = @_;

    # Simple equality test
    return 0 if($a eq $b);

    my @aparts;
    my @bparts;

    # [t]ransformation
    # [s]tring
    # [l]ist
    foreach my $t ([$a => \@aparts],
                   [$b => \@bparts]) {
        my $s = $t->[0];
        my $l = $t->[1];

        # We split not only on non-alphanumeric characters, but also on the
        # boundary of digits and letters. This corresponds to the behaviour of
        # rpmvercmp because it does 2 types of iteration over a string. The
        # first iteration skips non-alphanumeric characters. The second skips
        # over either digits or letters only, according to the first character
        # of $a.
        @$l = split(/(?<=[[:digit:]])(?=[[:alpha:]]) | # digit<>alpha
                     (?<=[[:alpha:]])(?=[[:digit:]]) | # alpha<>digit
                     [^[:alnum:]]+                # sequence of non-alphanumeric
                    /x, $s);
    }

    # Find the minimun of the number of parts of $a and $b
    my $parts = scalar(@aparts) < scalar(@bparts) ?
                scalar(@aparts) : scalar(@bparts);

    for(my $i = 0; $i < $parts; $i++) {
        my $acmp = $aparts[$i];
        my $bcmp = $bparts[$i];

        # Return 1 if $a is numeric and $b is not
        if($acmp =~ /^[[:digit:]]/) {
            return 1 if($bcmp !~ /^[[:digit:]]/);

            # Drop leading zeroes
            $acmp =~ /^0*(.*)$/;
            $acmp = $1;
            $bcmp =~ /^0*(.*)$/;
            $bcmp = $1;

            # We do a string comparison of 2 numbers later. At this stage, if
            # they're of differing lengths, one is larger.
            return 1 if(length($acmp) > length($bcmp));
            return -1 if(length($bcmp) > length($acmp));
        }

        # Return -1 if $a is letters and $b is not
        else {
            return -1 if($bcmp !~ /^[[:alpha:]]/);
        }

        # Return only if they differ
        return -1 if($acmp lt $bcmp);
        return 1 if($acmp gt $bcmp);
    }

    # We got here because all the parts compared so far have been equal, and one
    # or both have run out of parts.

    # Whichever has the greatest number of parts is the largest
    return -1 if(scalar(@aparts) < scalar(@bparts));
    return 1  if(scalar(@aparts) > scalar(@bparts));

    # We can get here if the 2 strings differ only in non-alphanumeric
    # separators.
    return 0;
}

sub _remap_block_devices
{
    my ($meta, $virtio, $g, $root) = @_;

    my @devices = map { $_->{device} } @{$meta->{disks}};
    @devices = sort @devices;

    # @devices contains an ordered list of libvirt device names. Because
    # libvirt uses a similar naming scheme to Linux, these will mostly be the
    # same names as used by the guest. They are ordered as they were passed to
    # libguestfs, which means their device name in the appliance can be
    # inferred.

    # If the guest is using libata, IDE drives could have different names in the
    # guest from their libvirt device names.

    # Modern distros use libata, and IDE devices are presented as sdX
    my $libata = 1;

    # RHEL 2, 3 and 4 didn't use libata
    # RHEL 5 does use libata, but udev rules call IDE devices hdX anyway
    if (_is_rhel_family($g, $root)) {
        my $major_version = $g->inspect_get_major_version($root);
        if ($major_version eq '2' ||
            $major_version eq '3' ||
            $major_version eq '4' ||
            $major_version eq '5')
        {
            $libata = 0;
        }
    }
    # Fedora has used libata since FC7, which is long out of support. We assume
    # that all Fedora distributions in use use libata.

    if ($libata) {
        # If there are any IDE devices, the guest will have named these sdX
        # before any SCSI devices. i.e. If we have disks hda, hdb, sda and sdb,
        # these will have been presented to the guest as sda, sdb, sdc and sdd
        # respectively.
        #
        # Here we take advantage of the fact that 'hd' comes alphabetically
        # before 'sd' to rename all 'hd' and 'sd' devices into a single 'sd'
        # namespace, with IDE devices coming first.

        my @newdevices;
        my $suffix = 'a';
        foreach my $device (@devices) {
            $device = 'sd'.$suffix++ if ($device =~ /(?:h|s)d[a-z]+/);
            push(@newdevices, $device);
        }
        @devices = @newdevices;
    }

    # We now assume that @devices contains an ordered list of device names, as
    # used by the guest. Create a map of old guest device names to new guest
    # device names.
    my %map;

    # Everything will be converted to either vdX, sdX or hdX
    my $prefix;
    if ($virtio) {
        $prefix = 'vd';
    } elsif ($libata) {
        $prefix = 'sd';
    } else {
        $prefix = 'hd'
    }

    my $letter = 'a';
    foreach my $device (@devices) {
        my $mapped = $prefix.$letter;


        # If a Xen guest has non-PV devices, Xen also simultaneously presents
        # these as xvd devices. i.e. hdX and xvdX both exist and are the same
        # device.
        # This mapping is also useful for P2V conversion of Citrix Xenserver
        # guests done in HVM mode. Disks are detected as sdX, although the guest
        # uses xvdX natively.
        if ($device =~ /^(?:h|s)d([a-z]+)/) {
            $map{'xvd'.$1} = $mapped;
        }
        $map{$device} = $mapped;
        $letter++;
    }

    eval {
        # Update bare device references in fstab and grub's device.map
        foreach my $spec ($g->aug_match('/files/etc/fstab/*/spec'),
                          $g->aug_match('/files/boot/grub/device.map/*'.
                                            '[label() != "#comment"]'))
        {
            my $device = $g->aug_get($spec);

            # Match device names and partition numbers
            my $name; my $part;
            foreach my $r (qr{^/dev/(cciss/c\d+d\d+)(?:p(\d+))?$},
                           qr{^/dev/([a-z]+)(\d*)?$}) {
                if ($device =~ $r) {
                    $name = $1;
                    $part = $2;
                    last;
                }
            }

            # Ignore this entry if it isn't a device name
            next unless defined($name);

            # Ignore md and floppy devices, which don't need to be mapped
            next if $name =~ /(md|fd)/;

            # Ignore this entry if it refers to a device we don't know anything
            # about. The user will have to fix this post-conversion.
            if (!exists($map{$name})) {
                my $warned = 0;
                for my $file ('/etc/fstab', '/boot/grub/device.map') {
                    if ($spec =~ m{^/files$file}) {
                        logmsg WARN, __x('{file} references unknown device '.
                                         '{device}. This entry must be '.
                                         'manually fixed after conversion.',
                                         file => $file, device => $device);
                        $warned = 1;
                    }
                }

                # Shouldn't happen. Not fatal if it does, though.
                if (!$warned) {
                    logmsg WARN, 'Please report this warning as a bug. '.
                                 "augeas path $spec refers to unknown device ".
                                 "$device. This entry must be manually fixed ".
                                 'after conversion.'
                }

                next;
            }

            my $mapped = '/dev/'.$map{$name};
            $mapped .= $part if defined($part);
            $g->aug_set($spec, $mapped);
        }

        $g->aug_save();
    };

    augeas_error($g, $@) if ($@);

    # Delete cached (and now out of date) blkid info if it exists
    foreach my $blkidtab ('/etc/blkid/blkid.tab', '/etc/blkid.tab') {
        $g->rm($blkidtab) if ($g->exists($blkidtab));
    }
}

sub _drivecmp
{
    my ($prefix, $a, $b) = @_;

    map {
        $_ =~ /^$prefix([a-z]+)/ or die("drive $_ doesn't have prefix $prefix");
        $_ = $1;
    } ($a, $b);

    return -1 if (length($a) < length($b));
    return 1 if (length($a) > length($b));

    return -1 if ($a lt $b);
    return 0 if ($a eq $b);
    return 1;
}

sub _prepare_bootable
{
    my ($g, $root, $grub, $version, @modules) = @_;

    my $path = "/boot/vmlinuz-$version";
    $grub->write($path);
    my $grub_initrd = $grub->get_initrd($path);

    # Backup the original initrd
    $g->mv($grub_initrd, "$grub_initrd.pre-v2v")
    if $g->exists($grub_initrd);

    if ($g->exists('/sbin/dracut')) {
        $g->command(['/sbin/dracut', '--add-drivers', join(" ", @modules),
                     $grub_initrd, $version]);
    }

    elsif ($g->exists('/sbin/mkinitrd')) {
        # Create a new initrd which probes the required kernel modules
        my @module_args = ();
        foreach my $module (@modules) {
            push(@module_args, "--with=$module");
        }

        # We explicitly modprobe ext2 here. This is required by mkinitrd on
        # RHEL 3, and shouldn't hurt on other OSs. We don't care if this
        # fails.
        eval { $g->modprobe('ext2') };

        # loop is a module in RHEL 5. Try to load it. Doesn't matter for
        # other OSs if it doesn't exist, but RHEL 5 will complain:
        #   All of your loopback devices are in use.
        eval { $g->modprobe('loop') };

        my @env;

        # RHEL 4 mkinitrd determines if the root filesystem is on LVM by
        # checking if the device name (after following symlinks) starts with
        # /dev/mapper. However, on recent kernels/udevs, /dev/mapper/foo is
        # just a symlink to /dev/dm-X. This means that RHEL 4 mkinitrd
        # running in the appliance fails to detect root on LVM. We check
        # ourselves if root is on LVM, and frig RHEL 4's mkinitrd if it is
        # by setting root_lvm=1 in its environment. This overrides an
        # internal variable in mkinitrd, and is therefore extremely nasty
        # and applicable only to a particular version of mkinitrd.
        if (_is_rhel_family($g, $root) &&
            $g->inspect_get_major_version($root) eq '4')
        {
            push(@env, 'root_lvm=1') if ($g->is_lv($root));
        }

        $g->sh(join(' ', @env).' /sbin/mkinitrd '.join(' ', @module_args).
               " $grub_initrd $version");
    }

    else {
        v2vdie __('Didn\'t find mkinitrd or dracut. Unable to update initrd.');
    }

    # Disable kudzu in the guest
    # Kudzu will detect the changed network hardware at boot time and either:
    #   require manual intervention, or
    #   disable the network interface
    # Neither of these behaviours is desirable.
    if ($g->exists('/etc/init.d/kudzu')) {
        $g->command(['/sbin/chkconfig', 'kudzu', 'off']);
    }
}

# Return 1 if the guest supports ACPI, 0 otherwise
sub _supports_acpi
{
    my ($g, $root, $arch) = @_;

    # Blacklist configurations which are known to fail
    # RHEL 3, x86_64
    if (_is_rhel_family($g, $root) && $g->inspect_get_major_version($root) == 3 &&
        $arch eq 'x86_64') {
        return 0;
    }

    return 1;
}

sub _supports_virtio
{
    my ($kernel, $g) = @_;

    my %checklist = (
        "virtio_net" => 0,
        "virtio_blk" => 0
    );

    # Search the installed kernel's modules for the virtio drivers
    foreach my $module ($g->find("/lib/modules/$kernel")) {
        foreach my $driver (keys(%checklist)) {
            if($module =~ m{/$driver\.(?:o|ko)$}) {
                $checklist{$driver} = 1;
            }
        }
    }

    # Check we've got all the drivers in the checklist
    foreach my $driver (keys(%checklist)) {
        if(!$checklist{$driver}) {
            return 0;
        }
    }

    return 1;
}

=back

=head1 COPYRIGHT

Copyright (C) 2009-2012 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtConvert::Converter(3pm)>,
L<Sys::VirtConvert(3pm)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
