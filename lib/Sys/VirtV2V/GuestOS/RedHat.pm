# Sys::VirtV2V::GuestOS::RedHat
# Copyright (C) 2009,2010 Red Hat Inc.
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

package Sys::VirtV2V::GuestOS::RedHat;

use strict;
use warnings;

use File::Spec;

use Sys::Guestfs::Lib qw(inspect_linux_kernel);
use Sys::VirtV2V::GuestOS;
use Sys::VirtV2V::Util qw(augeas_error user_message);

use Locale::TextDomain 'virt-v2v';

@Sys::VirtV2V::GuestOS::RedHat::ISA = qw(Sys::VirtV2V::GuestOS);

=pod

=head1 NAME

Sys::VirtV2V::GuestOS::RedHat - Manipulate and query a Red Hat based Linux guest

=head1 SYNOPSIS

 use Sys::VirtV2V::GuestOS;

 $guestos = Sys::VirtV2V::GuestOS->instantiate($g, $desc);

=head1 DESCRIPTION

Sys::VirtV2V::GuestOS::RedHat provides an interface for manipulating and
querying a Red Hat based Linux guest. Specifically it handles any Guest OS which
Sys::Guestfs::Lib has identified as 'linux', which uses rpm as a package format.

=head1 METHODS

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for a detailed description of
exported methods.

=over

=item Sys::VirtV2V::GuestOS::RedHat->can_handle(desc)

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

sub can_handle
{
    my $class = shift;

    my $desc = shift;

    return ($desc->{os} eq 'linux') && ($desc->{package_format} eq 'rpm');
}

=item Sys::VirtV2V::GuestOS::RedHat->new(self)

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

sub new
{
    my $class = shift;

    # Self object
    my $self = shift;
    carp("new called without self object") unless defined($self);

    bless($self, $class);

    $self->_init_selinux();
    $self->_init_modules();
    $self->_init_augeas();

    return $self;
}

# Handle SELinux for the guest
sub _init_selinux
{
    my $self = shift;

    my $g = $self->{g};

    # Assume SELinux isn't in use if load_policy isn't available
    return if(!$g->exists('/usr/sbin/load_policy'));

    # Actually loading the policy has proven to be problematic. We make whatever
    # changes are necessary, and make the guest relabel on the next boot.
    $g->touch('/.autorelabel');
}

sub _init_modules
{
    my $self = shift;
    my $g = $self->{g};

    # Check how new modules should be configured. Possibilities, in descending
    # order of preference, are:
    #   modprobe.d/
    #   modprobe.conf
    #   modules.conf
    #   conf.modules

    # Note that we're checking in ascending order of preference so that the last
    # discovered method will be chosen

    # Files which the augeas Modprobe lens doesn't look for by default
    foreach my $file qw(/etc/conf.modules /etc/modules.conf) {
        if($g->exists($file)) {
            $self->{modules} = $file;
        }
    }

    if($g->exists("/etc/modprobe.conf")) {
        $self->{modules} = "modprobe.conf";
    }

    # If the modprobe.d directory exists, create new entries in
    # modprobe.d/virtv2v-added.conf
    if($g->exists("/etc/modprobe.d")) {
        $self->{modules} = "modprobe.d/virtv2v-added.conf";
    }

    die(user_message(__"Unable to find any valid modprobe configuration"))
        unless(defined($self->{modules}));
}

sub _init_augeas
{
    my $self = shift;
    my $g = $self->{g};

    # Initialise augeas
    eval {
        $g->aug_close();
        $g->aug_init("/", 1);

        # Check if /boot/grub/menu.lst is included by the Grub lens
        my $found = 0;
        foreach my $incl ($g->aug_match("/augeas/load/Grub/incl")) {
            if ($g->aug_get($incl) eq '/boot/grub/menu.lst') {
                $found = 1;
                last;
            }
        }

        # If it wasn't there, add it
        unless ($found) {
            $g->aug_set("/augeas/load/Grub/incl[last()+1]",
                        "/boot/grub/menu.lst");
        }

        # If we have XF86Config instead of xorg.conf, use that instead.
        if (! $g->exists('/etc/X11/xorg.conf') &&
            $g->exists('/etc/X11/XF86Config'))
        {
            $g->aug_set('/augeas/load/Xorg/incl[last()+1]',
                        '/etc/X11/XF86Config');
            $self->{xorg} = '/etc/X11/XF86Config';
        } else {
            $self->{xorg} = '/etc/X11/xorg.conf';
        }

        # Make augeas pick up the new configuration
        $g->aug_load();
    };

    # The augeas calls will die() on any error.
    augeas_error($g, $@) if ($@);
}

=item enable_kernel_module(device, module)

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

sub enable_kernel_module
{
    my $self = shift;
    my ($device, $module) = @_;

    my $g = $self->{g};

    eval {
        $g->aug_set("/files".$self->{modules}."/alias[last()+1]", $device);
        $g->aug_set("/files".$self->{modules}."/alias[last()]/modulename",
                    $module);
        $g->aug_save();
    };

    # Propagate augeas errors
    augeas_error($g, $@) if ($@);
}

=item update_kernel_module(device, module)

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

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

    # If the module has mysteriously disappeared, just add a new one
    return $self->enable_kernel_module($device, $module) if (!defined($augeas));

    eval {
        $g->aug_set($augeas."/modulename", $module);
        $g->aug_save();
    };

    # Propagate augeas errors
    augeas_error($g, $@) if ($@);
}

=item disable_kernel_module(device)

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

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

    # Nothing to do if the module has gone away
    return if (!defined($augeas));

    eval {
        $g->aug_rm($augeas);
    };

    # Propagate augeas errors
    augeas_error($g, $@) if ($@);
}

=item update_display_driver(driver)

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

sub update_display_driver
{
    my $self = shift;
    my $driver = shift;

    my $g = $self->{g};

    # Update the display driver if it exists
    eval {
        foreach my $path
            ($g->aug_match('/files'.$self->{xorg}.'/Device/Driver'))
        {
            $g->aug_set($path, $driver);
        }

        $g->aug_save();
    };

    # Propagate augeas errors
    augeas_error($g, $@) if ($@);
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
    augeas_error($g, $@) if ($@);

    return $augeas;
}

=item list_kernels()

List all installed kernels. List the default kernel first, followed by the
remaining kernels in the order they are listed in grub.

=cut

sub list_kernels
{
    my $self = shift;

    my $g = $self->{g};

    # Get the default kernel from grub if it's set
    my $default;
    eval {
        $default = $g->aug_get('/files/boot/grub/menu.lst/default');
    };
    # Doesn't matter if get fails

    # Get the grub filesystem
    my $grub = $self->{desc}->{boot}->{grub_fs};

    # Look for a kernel, starting with the default
    my @paths;
    eval {
        push(@paths, $g->aug_match("/files/boot/grub/menu.lst/".
                                   "title[$default]/kernel"))
            if defined($default);
        push(@paths, $g->aug_match('/files/boot/grub/menu.lst/title/kernel'));
    };
    augeas_error($g, $@) if ($@);

    my @kernels;
    my %checked;
    foreach my $path (@paths) {
        next if ($checked{$path});
        $checked{$path} = 1;

        my $kernel;
        eval {
            $kernel = $g->aug_get($path);
        };
        augeas_error($g, $@) if ($@);

        # Prepend the grub filesystem to the kernel path
        $kernel = "$grub$kernel" if(defined($grub));

        # Check the kernel exists
        if ($g->exists($kernel)) {
            # Work out it's version number
            my $kernel_desc = inspect_linux_kernel($g, $kernel, 'rpm');

            push(@kernels, $kernel_desc->{version});
        }

        else {
            warn user_message(__x("WARNING: grub refers to {path}, which ".
                                  "doesn't exist.",
                                  path => $kernel));
        }
    }

    return @kernels;
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

=item install_capability(name)

Install a capability specified in the configuration file.

=cut

sub install_capability
{
    my $self = shift;
    my ($name) = @_;

    my $desc = $self->{desc};
    my $config = $self->{config};

    my $cap;
    eval {
        $cap = $config->match_capability($desc, $name);
    };
    if ($@) {
        warn($@);
        return 0;
    }

    if (!defined($cap)) {
        warn(user_message(__x("{name} capability not found in configuration",
                               name => $name)));
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
        eval {
            ($min_epoch, $min_version, $min_release) =
                _parse_evr($props->{minversion});
        };
        if ($@) {
            die(user_message(__x("Unrecognised format for {field} in config: ".
                                 "{value}. {field} must be in the format ".
                                 "[epoch:]version[-release].",
                                 field => 'minversion',
                                 value => $props->{minversion})));
        }

        # Kernels are special
        if ($name eq 'kernel') {
            my ($kernel_pkg, $kernel_rpmver, $kernel_arch) =
                $self->_discover_kernel();

            my ($kernel_epoch, $kernel_ver, $kernel_release);
            eval {
                ($kernel_epoch, $kernel_ver, $kernel_release) =
                    _parse_evr($kernel_rpmver);
            };
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
            if ($kernel_pkg eq "kernel-xen" || $kernel_pkg eq "kernel-xenU") {
                $kernel_pkg = $self->_get_replacement_kernel_name($kernel_arch);

                # filter out xen/xenU from release field
                if (defined($kernel_release) &&
                    $kernel_release =~ /^(\S+?)(xen)?(U)?$/)
                {
                    $kernel_release = $1;
                }

                # If the guest kernel is new enough, but PV, try to replace it
                # with an equivalent version FV kernel
                if (_evr_cmp($kernel_epoch, $kernel_ver, $kernel_release,
                             $min_epoch, $min_version, $min_release) >= 0) {
                    $kernel = [$kernel_pkg, $kernel_arch,
                               $kernel_epoch, $kernel_ver, $kernel_release];
                }

                # Otherwise, just grab the latest
                else {
                    $kernel = [$kernel_pkg, $kernel_arch];
                }
            }

            # If the kernel is too old, grab the latest replacement
            elsif (_evr_cmp($kernel_epoch, $kernel_ver, $kernel_release,
                            $min_epoch, $min_version, $min_release) < 0) {
                $kernel = [$kernel_pkg, $kernel_arch];
            }
        }

        else {
            my @installed = $self->_get_installed($name);

            # Ignore an 'ifinstalled' dep if it's not currently installed
            next if (@installed == 0 && $ifinstalled);

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

            # Install the latest available version of the dep if it wasn't found
            if (!$found) {
                if (@installed == 0) {
                    push(@install, [$name]);
                } else {
                    push(@upgrade, [$name]);
                }
            }
        }
    }

    # Capability is already installed
    if (!defined($kernel) && @install == 0 && @upgrade == 0) {
        return 1;
    }

    my $g = $self->{g};

    # List of kernels before the new kernel installation
    my @k_before = $g->glob_expand('/boot/vmlinuz-*');

    my $success = $self->_install_any($kernel, \@install, \@upgrade);

    # Check to see if we installed a new kernel, and check grub if we did
    $self->_find_new_kernel(@k_before);

    return $success;
}

sub _get_replacement_kernel_name
{
    my $self = shift;
    my ($arch) = @_;

    my $desc = $self->{desc};

    # Make an informed choice about a replacement kernel for distros we know
    # about

    # RHEL 5
    if ($desc->{distro} eq 'rhel' && $desc->{major_version} eq '5') {
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
    elsif ($desc->{distro} eq 'rhel' && $desc->{major_version} eq '4') {
        my $ncpus = $self->get_ncpus();

        if ($arch eq 'i686') {
            # If the guest has > 10G RAM, give it a hugemem kernel
            if ($self->get_memory_kb() > 10 * 1024 * 1024) {
                return 'kernel-hugemem';
            }

            # SMP kernel for guests with >1 CPU
            elsif ($ncpus > 1) {
                return 'kernel-smp';
            }

            else {
                return 'kernel';
            }
        }

        else {
            if ($ncpus > 8) {
                return 'kernel-largesmp';
            }

            elsif ($ncpus > 1) {
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

sub _install_any
{
    my $self = shift;
    my ($kernel, $install, $upgrade) = @_;

    my $g = $self->{g};

    my $resolv_bak = $g->exists('/etc/resolv.conf');
    $g->mv('/etc/resolv.conf', '/etc/resolv.conf.v2vtmp') if ($resolv_bak);

    # XXX We should get the nameserver from the appliance here. However,
    # there's no current api other than debug to do this, and in any case
    # resolv.conf in the appliance is both hardcoded and currently wrong.
    $g->write_file('/etc/resolv.conf', "nameserver 169.254.2.3", 0);

    my $success;
    eval {
        # Try to fetch these dependencies using the guest's native update tool
        $success = $self->_install_up2date($kernel, $install, $upgrade);
        $success = $self->_install_yum($kernel, $install, $upgrade)
            unless ($success);

        # Fall back to local config if the above didn't work
        $success = $self->_install_config($kernel, $install, $upgrade)
            unless ($success);
    };
    if ($@) {
        warn($@);
        $success = 0;
    }

    $g->mv('/etc/resolv.conf.v2vtmp', '/etc/resolv.conf') if ($resolv_bak);

    # Make augeas reload to pick up any altered configuration
    eval {
        $g->aug_load();
    };
    augeas_error($g, $@) if ($@);

    return $success;
}

sub _install_up2date
{
    my $self = shift;
    my ($kernel, $install, $upgrade) = @_;

    my $g = $self->{g};

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
        warn(user_message(__x("Failed to install packages using up2date. ".
                              "Error message was: {error}",
                              error => $@)));
        return 0;
    }

    return 1;
}

sub _install_yum
{
    my $self = shift;
    my ($kernel, $install, $upgrade) = @_;

    my $g = $self->{g};

    # Check this system has yum installed
    return 0 unless ($g->exists('/usr/bin/yum'));

    # Install or upgrade the kernel?
    # If it isn't installed (because we're replacing a PV kernel), we need to
    # install
    # If we're installing a specific version, we need to install
    # If the kernel package we're installing is already installed and we're
    # just upgrading to the latest version, we need to upgrade
    if (defined($kernel)) {
        my @installed = $self->_get_installed($kernel->[0]);

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

            my @output;
            eval {
                @output = $g->sh_lines("LANG=C /usr/bin/yum -y $action $pkg");
            };
            if ($@) {
                warn(user_message(__x("Failed to install packages using yum. ".
                                      "Output was: {output}",
                                      error => $@)));
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
    my $self = shift;
    my ($kernel_naevr, $install, $upgrade) = @_;

    my $g = $self->{g};
    my $desc = $self->{desc};

    my ($kernel, $user);
    if (defined($kernel_naevr)) {
        my ($kernel_pkg, $kernel_arch) = @$kernel_naevr;

        ($kernel, $user) =
            $self->{config}->match_app($desc, $kernel_pkg, $kernel_arch);
    } else {
        $user = [];
    }

    foreach my $pkg (@$install, @$upgrade) {
        push(@$user, $pkg->[0]);
    }

    my @missing;
    if (defined($kernel) &&
        !$g->exists($self->{config}->get_transfer_path($g, $kernel)))
    {
        push(@missing, $kernel);
    }

    my @user_paths = $self->_get_deppaths(\@missing, $desc->{arch}, @$user);

    # We can't proceed if there are any files missing
    die(user_message(__x("Installation failed because the following ".
                         "files referenced in the configuration file are ".
                         "required, but missing: {list}",
                         list => join(' ', @missing)))) if (@missing > 0);

    # Install any non-kernel requirements
    $self->_install_rpms(1, @user_paths);

    if (defined($kernel)) {
        $self->_install_rpms(0, ($kernel));
    }

    return 1;
}

=item install_good_kernel

Attempt to install a known-good kernel

=cut

sub install_good_kernel
{
    my $self = shift;

    my $g = $self->{g};

    my ($kernel_pkg, $kernel_rpmver, $kernel_arch) = $self->_discover_kernel();

    # If the guest is using a Xen PV kernel, choose an appropriate
    # normal kernel replacement
    if ($kernel_pkg eq "kernel-xen" || $kernel_pkg eq "kernel-xenU") {
        $kernel_pkg = $self->_get_replacement_kernel_name($kernel_arch);
    }

    # List of kernels before the new kernel installation
    my @k_before = $g->glob_expand('/boot/vmlinuz-*');

    return undef unless ($self->_install_any([$kernel_pkg, $kernel_arch]));

    my $version = $self->_find_new_kernel(@k_before);
    die("Couldn't determine version of installed kernel")
        unless (defined($version));

    return $version;
}

sub _find_new_kernel
{
    my $self = shift;

    my $g = $self->{g};

    # Figure out which kernel has just been installed
    foreach my $k ($g->glob_expand('/boot/vmlinuz-*')) {
        if (!grep(/^$k$/, @_)) {
            # Check which directory in /lib/modules the kernel rpm creates
            foreach my $file ($g->command_lines (['rpm', '-qlf', $k])) {
                next unless ($file =~ m{^/lib/modules/([^/]+)$});

                my $version = $1;
                if ($g->is_dir("/lib/modules/$version")) {
                    $self->_check_grub($version, $k);
                    return $version;
                }
            }
        }
    }
    return undef;
}

# grubby can sometimes fail to correctly update grub.conf when run from
# libguestfs. If it looks like this happened, install a new grub config here.
sub _check_grub
{
    my $self = shift;
    my ($version, $kernel) = @_;

    my $g = $self->{g};

    # Nothing to do if there's already a grub entry
    eval {
        foreach my $augpath
            ($g->aug_match('/files/boot/grub/menu.lst/title/kernel'))
        {
            return if ($g->aug_get($augpath) eq $kernel);
        }
    };
    augeas_error($g, $@) if ($@);

    my $prefix;
    if ($self->{desc}->{boot}->{grub_fs} eq "/boot") {
        $prefix = '';
    } else {
        $prefix = '/boot';
    }

    my $initrd = "$prefix/initrd-$version.img";
    $kernel =~ m{^/boot/(.*)$} or die("kernel in unexpected location: $kernel");
    my $vmlinuz = "$prefix/$1";

    my $title;
    # No point in dying if /etc/redhat-release can't be read
    eval {
        ($title) = $g->read_lines('/etc/redhat-release');
    };
    $title ||= 'Linux';

    # This is how new-kernel-pkg does it
    $title =~ s/ release.*//;
    $title .= " ($version)";

    my $default;
    # Doesn't matter if there's no default
    eval {
        $default = $g->aug_get('/files/boot/grub/menu.lst/default');
    };

    eval {
        if (defined($default)) {
            $g->aug_defvar('template',
                 '/files/boot/grub/menu.lst/title['.($default + 1).']');
        }

        # If there's no default, take the first entry with a kernel
        else {
            my ($match) =
                $g->aug_match('/files/boot/grub/menu.lst/title/kernel');

            die("No template kernel found in grub.") unless(defined($match));

            $match =~ s/\/kernel$//;
            $g->aug_defvar('template', $match);
        }

        # Add a new title node at the end
        $g->aug_defnode('new',
                        '/files/boot/grub/menu.lst/title[last()+1]',
                        $title);

        # N.B. Don't change the order of root, kernel and initrd below, or the
        # guest will not boot.

        # Copy root from the template
        $g->aug_set('$new/root', $g->aug_get('$template/root'));

        # Set kernel and initrd to the new values
        $g->aug_set('$new/kernel', $vmlinuz);
        $g->aug_set('$new/initrd', $initrd);

        # Copy all kernel command-line arguments
        foreach my $arg ($g->aug_match('$template/kernel/*')) {
            # kernel arguments don't necessarily have values
            my $val;
            eval {
                $val = $g->aug_get($arg);
            };

            $arg =~ /([^\/]*)$/;
            $arg = $1;

            if (defined($val)) {
                $g->aug_set('$new/kernel/'.$arg, $val);
            } else {
                $g->aug_clear('$new/kernel/'.$arg);
            }
        }

        my ($new) = $g->aug_match('$new');
        $new =~ /\[(\d+)\]$/;

        $g->aug_set('/files/boot/grub/menu.lst/default',
                    defined($1) ? $1 - 1 : 0);

        $g->aug_save();
    };
    augeas_error($g, $@) if ($@);
}

# Inspect the guest description to work out what kernel package is in use
# Returns ($kernel_pkg, $kernel_arch)
sub _discover_kernel
{
    my $self = shift;

    my $desc = $self->{desc};
    my $boot = $desc->{boot};

    # Check the default first
    my @configs;
    push(@configs, $boot->{default}) if(defined($boot->{default}));

    # Then check the rest. Default will get checked twice. Shouldn't be a
    # problem, though.
    push(@configs, (0..$#{$boot->{configs}}));

    # Get a current bootable kernel, preferrably the default
    my $kernel_pkg;
    my $kernel_ver;
    my $kernel_arch;

    foreach my $i (@configs) {
        my $config = $boot->{configs}->[$i];

        # Check the entry has a kernel
        my $kernel = $config->{kernel};
        next unless(defined($kernel));

        # Check its architecture is known
        $kernel_arch = $kernel->{arch};
        next unless(defined($kernel_arch));

        # Get the kernel package name
        $kernel_pkg = $kernel->{package};

        # Get the kernel package version
        $kernel_ver = $kernel->{version};

        last;
    }

    # Default to 'kernel' if package name wasn't discovered
    $kernel_pkg = "kernel" if(!defined($kernel_pkg));

    # Default the kernel architecture to the userspace architecture if it wasn't
    # directly detected
    $kernel_arch = $desc->{arch} if(!defined($kernel_arch));

    die(user_message(__"Unable to determine a kernel architecture for this ".
                       "guest"))
        unless(defined($kernel_arch));

    # We haven't supported anything other than i686 for the kernel on 32 bit for
    # a very long time.
    $kernel_arch = 'i686' if('i386' eq $kernel_arch);

    return ($kernel_pkg, $kernel_ver, $kernel_arch);
}

=item remove_kernel(version)

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

sub remove_kernel
{
    my $self = shift;
    my ($version) = @_;
    carp("remove_kernel called without version argument")
        unless(defined($version));

    my $g = $self->{g};
    # Work out which rpm contains the kernel
    my @output = $g->command_lines(['rpm', '-qf', "/boot/vmlinuz-$version"]);
    $g->command(['rpm', '-e', $output[0]]);

    # Make augeas reload so it knows the kernel's gone
    eval {
        $g->aug_load();
    };
    augeas_error($g, $@) if ($@);
}

sub _get_nevra
{
    my $self = shift;
    my ($rpm) = @_;

    my $g = $self->{g};

    $rpm = $self->{config}->get_transfer_path($g, $rpm);

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

sub _get_installed
{
    my $self = shift;
    my ($name) = @_;

    my $g = $self->{g};

    my $rpmcmd = ['rpm', '-q', '--qf', '%{EPOCH} %{VERSION} %{RELEASE}\n',
                  $name];
    my @output;
    eval {
        @output = $g->command_lines($rpmcmd);
    };

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

        die(user_message(__x("Error running {command}: {error}",
                             command => join(' ', @$rpmcmd),
                             error => $error)));
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

    return @installed;
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


# Return 1 if the requested rpm, or a newer version, is installed
# Return 0 otherwise
sub _newer_installed
{
    my $self = shift;
    my ($rpm) = @_;

    my $g = $self->{g};

    my ($name, $epoch, $version, $release, $arch) = $self->_get_nevra($rpm);

    my @installed = $self->_get_installed("$name.$arch");

    # Search installed rpms matching <name>.<arch>
    my $found = 0;
    foreach my $pkg (@installed) {
        next if _evr_cmp($pkg->[0], $pkg->[1], $pkg->[2],
                         $epoch, $version, $release) < 0;
        $found = 1;
    }

    return $found;
}

# Return a list of dependency paths which need to be installed for the given
# apps
sub _get_deppaths
{
    my $self = shift;
    my ($missing, $arch, @apps) = @_;

    my $desc = $self->{desc};
    my $config = $self->{config};

    my %required;
    foreach my $app (@apps) {
        my ($path, $deps) = $config->match_app($desc, $app, $arch);

        my $g = $self->{g};
        my $exists = $g->exists($self->{config}->get_transfer_path($g, $path));

        if (!$exists) {
            push(@$missing, $path);
        }

        if (!$exists || !$self->_newer_installed($path)) {
            $required{$path} = 1;

            foreach my $deppath ($self->_get_deppaths($missing,
                                                      $arch, @$deps)) {
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
                ($path, $deps) = $config->match_app($desc, $app, 'i386');
            };

            if (defined($path)) {
                my $g = $self->{g};

                if (!$g->exists($self->{config}->get_transfer_path($g, $path)))
                {
                    push(@$missing, $path);

                    foreach my $deppath ($self->_get_deppaths($missing,
                                                              'i386', @$deps))
                    {
                        $required{$deppath} = 1;
                    }
                }

                elsif (!$self->_newer_installed($path)) {
                    my ($name, undef, undef, undef, $arch) =
                        $self->_get_nevra($path);

                    my @installed = $self->_get_installed("$name.$arch");

                    if (@installed > 0) {
                        $required{$path} = 1;

                        foreach my $deppath
                            ($self->_get_deppaths($missing, 'i386', @$deps))
                        {
                            $required{$deppath} = 1;
                        }
                    }
                }
            }
        }
    }

    return keys(%required);
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

=item remove_application(name)

Uninstall an application.

=cut

sub remove_application
{
    my $self = shift;
    my $name = shift;

    my $g = $self->{g};
    $g->command(['rpm', '-e', $name]);

    # Make augeas reload in case the removal changed anything
    eval {
        $g->aug_load();
    };

    augeas_error($g, $@) if ($@);
}

=item get_application_owner(file)

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

sub get_application_owner
{
    my $self = shift;
    my ($file) = @_;

    my $g = $self->{g};
    eval {
        return $g->command(['rpm', '-qf', $file]);
    };
    die($@) if($@);
}

# Install a set of rpms
sub _install_rpms
{
    my $self = shift;

    my ($upgrade, @rpms) = @_;

    # Nothing to do if we got an empty set
    return if(scalar(@rpms) == 0);

    my $g = $self->{g};

    # All paths are relative to the transfer mount. Need to make them absolute.
    @rpms = map { $_ = $self->{config}->get_transfer_path($g, $_) } @rpms;

    $g->command(['rpm', $upgrade == 1 ? '-U' : '-i', @rpms]);

    # Reload augeas in case the rpm installation changed anything
    eval {
        $g->aug_load();
    };

    augeas_error($g, $@) if($@);
}

=item remap_block_devices(devices, virtio)

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

sub remap_block_devices
{
    my $self = shift;
    my ($devices, $virtio) = @_;

    my $g    = $self->{g};
    my $desc = $self->{desc};

    # $devices contains an order list of devices, as named by the host. Because
    # libvirt uses a similar naming scheme to Linux, these will mostly be the
    # same names as used by the guest. However, if the guest is using libata,
    # IDE drives could be renamed.

    # Modern distros use libata, and IDE devices are presented as sdX
    my $libata = 1;

    # RHEL 2, 3 and 4 didn't use libata
    # RHEL 5 does use libata, but udev rules call IDE devices hdX anyway
    if ($desc->{distro} eq 'rhel') {
        if ($desc->{major_version} eq '2' ||
            $desc->{major_version} eq '3' ||
            $desc->{major_version} eq '4' ||
            $desc->{major_version} eq '5')
        {
            $libata = 0;
        }
    }
    # Fedora has used libata since FC7, which is long out of support. We assume
    # that all Fedora distributions in use use libata.

    if ($libata) {
        # Look for IDE and SCSI devices in guest config files
        my %guestif;
        eval {
            my @guestdevs;

            foreach my $spec ($g->aug_match('/files/etc/fstab/*/spec')) {
                my $device = $g->aug_get($spec);

                push(@guestdevs, $device);
            }

            foreach my $key ($g->aug_match('/files/boot/grub/device.map/*')) {
                $key =~ m{/files/boot/grub/device.map/(.*)} or die;
                my $gdev = $1;

                next if ($gdev =~ /^#comment/);

                my $odev = $g->aug_get($key);
                push(@guestdevs, $odev);
            }

            foreach my $dev (@guestdevs) {
                next unless($dev =~ m{^/dev/(sd|hd)([a-z]+)});
                $guestif{$1} ||= {};
                $guestif{$1}->{$1.$2} = 1;
            }
        };

        augeas_error($g, $@) if ($@);

        # If guest config contains references to sdX, these could refer to IDE
        # or SCSI devices. We may need to update them.
        if (exists($guestif{sd})) {
            # Look for IDE and SCSI devices from the domain definition
            my %domainif;
            foreach my $device (@$devices) {
                foreach my $type ('hd', 'sd') {
                    if ($device =~ m{^$type([a-z]+)}) {
                        $domainif{$type} ||= {};
                        $domainif{$type}->{$device} = 1;
                    }
                }
            }

            my %map;

            my $letter = 'a';
            # IDE drives are presented first
            foreach my $old (sort { _drivecmp('hd', $a, $b) }
                                  keys(%{$domainif{hd}}))
            {
                $map{$old} = "sd$letter";
                $letter++;
            }

            # Followed by SCSI drives
            foreach my $old (sort { _drivecmp('sd', $a, $b) }
                                  keys(%{$domainif{sd}}))
            {
                $map{$old} = "sd$letter";
                $letter++;
            }

            map { $_ = $map{$_} } @$devices;
        }
    }

    # We now assume that $devices contains an ordered list of device names, as
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
    foreach my $device (@$devices) {
        $map{$device} = $prefix.$letter;
        $letter++;
    }

    eval {
        # Update bare device references in fstab
        foreach my $spec ($g->aug_match('/files/etc/fstab/*/spec')) {
            my $device = $g->aug_get($spec);

            next unless($device =~ m{^/dev/([a-z]+)(\d*)});
            my $name = $1;
            my $part = $2;

            next unless(exists($map{$name}));

            $g->aug_set($spec, "/dev/".$map{$name}.$part);
        }
        # Update device.map
        foreach my $key ($g->aug_match('/files/boot/grub/device.map/*')) {
            $key =~ m{/files/boot/grub/device.map/(.*)} or die;
            my $gdev = $1;

            next if ($gdev =~ /^#comment/);

            my $odev = $g->aug_get($key);

            next unless($odev =~ m{^/dev/([a-z]+)(\d*)});
            my $name = $1;
            my $part = $2;

            next unless(exists($map{$name}));

            $g->aug_set($key, "/dev/".$map{$name}.$part);
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

=item prepare_bootable(version [, module, module, ...])

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

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
        my $prefix;
        if ($self->{desc}->{boot}->{grub_fs} eq "/boot") {
            $prefix = '';
        } else {
            $prefix = '/boot';
        }

        foreach my $kernel
                ($g->aug_match('/files/boot/grub/menu.lst/title/kernel')) {

            if($g->aug_get($kernel) eq "$prefix/vmlinuz-$version") {
                # Ensure it's the default
                $kernel =~ m{/files/boot/grub/menu.lst/title(?:\[(\d+)\])?/kernel}
                    or die($kernel);

                my $aug_index;
                if(defined($1)) {
                    $aug_index = $1;
                } else {
                    $aug_index = 1;
                }

                $g->aug_set('/files/boot/grub/menu.lst/default',
                            $aug_index - 1);

                # Get the initrd for this kernel
                $initrd = $g->aug_get("/files/boot/grub/menu.lst/title[$aug_index]/initrd");

                $found = 1;
                last;
            }
        }

        $g->aug_save();
    };

    # Propagate augeas failure
    augeas_error($g, $@) if ($@);

    if(!defined($initrd)) {
        warn user_message(__x("WARNING: Kernel version {version} ".
                              "doesn't have an initrd entry in grub.",
                              version => $version));
    } else {
        # Initrd as returned by grub may be relative to /boot
        $initrd = $self->{desc}->{boot}->{grub_fs}.$initrd;

        # Backup the original initrd
        $g->mv($initrd, "$initrd.pre-v2v") if ($g->exists($initrd));

        if ($g->exists('/sbin/dracut')) {
            $g->command(['/sbin/dracut', '--add-drivers', join(" ", @modules),
                         $initrd, $version]);
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
            eval {
                $g->modprobe('ext2');
            };

            # loop is a module in RHEL 5. Try to load it. Doesn't matter for
            # other OSs if it doesn't exist, but RHEL 5 will complain:
            #   All of your loopback devices are in use.
            eval {
                $g->modprobe('loop');
            };

            $g->command(['/sbin/mkinitrd', @module_args, $initrd, $version]);
        }

        else {
            die user_message(__"Didn't find mkinitrd or dracut. Unable to ".
                               "update initrd");
        }
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

=item supports_virtio(kernel)

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

sub supports_virtio
{
    my $self = shift;
    my ($kernel) = @_;

    my %checklist = (
        "virtio_net" => 0,
        "virtio_pci" => 0,
        "virtio_blk" => 0
    );

    my $g = $self->{g};

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

Copyright (C) 2009,2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-v2v(1)>,
L<Sys::VirtV2V::GuestOS(3pm)>,
L<Sys::Guestfs(3pm)>,
L<Sys::Guestfs::Lib(3pm)>,
L<http://libguestfs.org/>.

=cut

1;
