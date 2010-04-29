# Sys::VirtV2V::GuestOS::RedHat
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

package Sys::VirtV2V::GuestOS::RedHat;

use strict;
use warnings;

use File::Spec;

use Sys::Guestfs::Lib qw(inspect_linux_kernel);
use Sys::VirtV2V::GuestOS;
use Sys::VirtV2V::UserMessage qw(user_message);

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
    $self->_init_augeas_modprobe();

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

sub _init_augeas_modprobe
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
    # modprobe.d/virtv2v-added.conf
    if($g->exists("/etc/modprobe.d")) {
        $self->{modules} = "modprobe.d/virtv2v-added.conf";
    }

    die(user_message(__"Unable to find any valid modprobe configuration"))
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
        }

        # Remove all includes for the Grub lens, and add only
        # /boot/grub/menu.lst
        foreach my $incl ($g->aug_match("/augeas/load/Grub/incl")) {
            $g->aug_rm($incl);
        }
        $g->aug_set("/augeas/load/Grub/incl[last()+1]", "/boot/grub/menu.lst");

        # Make augeas pick up the new configuration
        $g->aug_load();
    };

    # The augeas calls will die() on any error.
    die($@) if($@);
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
        $g->aug_set("/files/".$self->{modules}."/alias[last()+1]", $device);
        $g->aug_set("/files/".$self->{modules}."/alias[last()]/modulename",
                    $module);
        $g->aug_save();
    };

    # Propagate augeas errors
    die($@) if($@);
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
    die($@) if($@);
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
    die($@) if($@);
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

    return $augeas;
}

=item get_default_kernel()

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

sub get_default_kernel
{
    my $self = shift;

    my $g = $self->{g};

    # Get the default kernel from grub if it's set
    my $default;
    eval {
        $default = $g->aug_get('/files/boot/grub/menu.lst/default');
    };

    my $kernel;
    if(defined($default)) {
        # Grub's default is zero-based, but augeas arrays are 1-based.
        $default += 1;

        # Check it's got a kernel entry
        eval {
            $kernel =
                $g->aug_get("/files/boot/grub/menu.lst/title[$default]/kernel");
        };
    }

    # If we didn't find a default, find the first listed kernel
    if(!defined($kernel)) {
        eval {
            my @paths = $g->aug_match('/files/boot/grub/menu.lst/title/kernel');

            $kernel = $g->aug_get($paths[0]) if(@paths > 0);
        };
    }

    # If we got here, grub doesn't contain any kernels. Give up.
    die(user_message(__"Unable to find a default kernel"))
        unless(defined($kernel));

    my $desc = $self->{desc};

    # Get the grub filesystem
    my $grub = $desc->{boot}->{grub_fs};

    # Prepend the grub filesystem to the kernel path to get an absolute path
    $kernel = "$grub$kernel" if(defined($grub));

    # Work out it's version number
    my $kernel_desc = inspect_linux_kernel ($g, $kernel, 'rpm');

    return $kernel_desc->{version};
}

=item add_kernel()

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

sub add_kernel
{
    my $self = shift;

    my ($kernel_pkg, $kernel_arch) = $self->_discover_kernel();

    # If the guest is using a Xen PV kernel, choose an appropriate normal kernel
    # replacement
    if ($kernel_pkg eq "kernel-xen" || $kernel_pkg eq "kernel-xenU") {
        my $desc = $self->{desc};

        # Make an informed choice about a replacement kernel for distros we know
        # about

        # RHEL 5
        if ($desc->{distro} eq 'rhel' && $desc->{major_version} eq '5') {
            if ($kernel_arch eq 'i686') {
                # XXX: This assumes that PAE will be available in the
                # hypervisor. While this is almost certainly true, it's
                # theoretically possible that it isn't. The information we need
                # is available in the capabilities XML.
                # If PAE isn't available, we should choose 'kernel'.
                $kernel_pkg = 'kernel-PAE';
            }

            # There's only 1 kernel package on RHEL 5 x86_64
            else {
                $kernel_pkg = 'kernel';
            }
        }

        # RHEL 4
        elsif ($desc->{distro} eq 'rhel' && $desc->{major_version} eq '4') {
            my $ncpus = $self->get_ncpus();

            if ($kernel_arch eq 'i686') {
                # If the guest has > 10G RAM, give it a hugemem kernel
                if ($self->get_memory_kb() > 10 * 1024 * 1024) {
                    $kernel_pkg = 'kernel-hugemem';
                }

                # SMP kernel for guests with >1 CPU
                elsif ($ncpus > 1) {
                    $kernel_pkg = 'kernel-smp';
                }

                else {
                    $kernel_pkg = 'kernel';
                }
            }

            else {
                if ($ncpus > 8) {
                    $kernel_pkg = 'kernel-largesmp';
                }

                elsif ($ncpus > 1) {
                    $kernel_pkg = 'kernel-smp';
                }

                else {
                    $kernel_pkg = 'kernel';
                }
            }
        }

        # RHEL 3 didn't have a xen kernel

        # XXX: Could do with a history of Fedora kernels in here

        # For other distros, be conservative and just return 'kernel'
        else {
            $kernel_pkg = 'kernel';
        }
    }

    my ($app, $depnames);
    eval {
        my $desc = $self->{desc};

        my $config = $self->{config};
        unless (defined($config)) {
            my $search = Sys::VirtV2V::Config::get_app_search
                            ($desc, $kernel_pkg, $kernel_arch);
            die(user_message(__x("No config specified. No app match for ".
                                 "{search}",
                                 search => $search)));
        }

        ($app, $depnames) =
            $config->match_app($desc, $kernel_pkg, $kernel_arch);
    };
    # Return undef if we didn't find a kernel
    if ($@) {
        print STDERR $@;
        return undef;
    }

    return undef if ($self->_newer_installed($app));

    my $user_arch = $kernel_arch eq 'i686' ? 'i386' : $kernel_arch;

    # Install any required kernel dependencies
    $self->_install_rpms(1, $self->_get_deppaths($user_arch, @$depnames));

    # Inspect the rpm to work out what kernel version it contains
    my $version;
    my $g = $self->{g};
    foreach my $file ($g->command_lines
        (["rpm", "-qlp", $self->_transfer_path($app)]))
    {
        if($file =~ m{^/boot/vmlinuz-(.*)$}) {
            $version = $1;
            last;
        }
    }

    die(user_message(__x("{path} doesn't contain a valid kernel",
                         path => $app))) if(!defined($version));

    $self->_install_rpms(0, ($app));

    # Make augeas reload so it'll find the new kernel
    $g->aug_load();

    return $version;
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

        # Default to 'kernel' if package name wasn't discovered
        $kernel_pkg = "kernel" if(!defined($kernel_pkg));
    }

    # Default the kernel architecture to the userspace architecture if it wasn't
    # directly detected
    $kernel_arch = $desc->{arch} if(!defined($kernel_arch));

    die(user_message(__"Unable to determine a kernel architecture for this ".
                       "guest"))
        unless(defined($kernel_arch));

    # We haven't supported anything other than i686 for the kernel on 32 bit for
    # a very long time.
    $kernel_arch = 'i686' if('i386' eq $kernel_arch);

    return ($kernel_pkg, $kernel_arch);
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
    eval {
        # Work out which rpm contains the kernel
        my @output =
            $g->command_lines(['rpm', '-qf', "/boot/vmlinuz-$version"]);
        $g->command(['rpm', '-e', $output[0]]);
    };

    die($@) if($@);

    # Make augeas reload so it knows the kernel's gone
    $g->aug_load();
}

=item add_application(label)

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

sub add_application
{
    my $self = shift;
    my $label = shift;

    my $desc = $self->{desc};
    my $user_arch = $desc->{arch};

    my $config = $self->{config};
    unless (defined($config)) {
        my $search = Sys::VirtV2V::Config::get_app_search($desc, $label,
                                                          $user_arch);
        die(user_message(__x("No config specified. No app match for {search}",
                             search => $search)));
    }

    my ($app, $deps) = $config->match_app($self->{desc}, $label, $user_arch);

    # Nothing to do if it's already installed
    return if ($self->_newer_installed($app));

    my @install = ($app);

    # Add any dependencies which aren't already installed to the install set
    push(@install, $self->_get_deppaths($user_arch, @$deps));

    $self->_install_rpms(1, @install);
}

sub _get_nevra
{
    my $self = shift;
    my ($rpm) = @_;

    my $g = $self->{g};

    $rpm = $self->_transfer_path($rpm);

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
    my ($name, $arch) = @_;

    my $g = $self->{g};

    my $rpmcmd = ['rpm', '-q', '--qf', '%{EPOCH} %{VERSION} %{RELEASE}\n',
                  "$name.$arch"];
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


# Return 1 if the requested rpm, or a newer version, is installed
# Return 0 otherwise
sub _newer_installed
{
    my $self = shift;
    my ($rpm) = @_;

    my $g = $self->{g};

    my ($name, $epoch, $version, $release, $arch) = $self->_get_nevra($rpm);

    my @installed = $self->_get_installed($name, $arch);

    # Search installed rpms matching <name>.<arch>
    my $found = 0;
    foreach my $pkg (@installed) {
        my $iepoch   = $pkg->[0];
        my $iversion = $pkg->[1];
        my $irelease = $pkg->[2];

        # Skip if installed epoch less than requested version
        next if ($iepoch < $epoch);

        if ($iepoch eq $epoch) {
            # Skip if installed version less than requested version
            next if (_rpmvercmp($iversion, $version) < 0);

            # Skip if install version == requested version, but release less
            # than requested release
            if ($iversion eq $version) {
                next if (_rpmvercmp($irelease, $release) < 0);
            }
        }

        $found = 1;
    }

    return $found;
}

# Return a list of dependency paths which need to be installed for the given
# apps
sub _get_deppaths
{
    my $self = shift;
    my ($arch, @apps) = @_;

    my $desc = $self->{desc};
    my $config = $self->{config};

    my %required;
    foreach my $app (@apps) {
        my ($path, $deps) = $config->match_app($desc, $app, $arch);

        if (!$self->_newer_installed($path)) {
            $required{$path} = 1;

            foreach my $deppath ($self->_get_deppaths($arch, @$deps)) {
                $required{$deppath} = 1;
            }
        }

        # For x86_64, also check if there is any i386 version installed. If
        # there is, check if it needs to be upgraded.
        if ($arch eq 'x86_64') {
            $path = undef;
            $deps = undef;

            # It's not an error if no i386 package is available
            eval {
                ($path, $deps) = $config->match_app($desc, $app, 'i386');
            };

            if (defined($path) && !$self->_newer_installed($path)) {
                my ($name, undef, undef, undef, $arch) =
                    $self->_get_nevra($path);

                my @installed = $self->_get_installed($name, $arch);
                if (@installed > 0) {
                    $required{$path} = 1;

                    foreach my $deppath ($self->_get_deppaths('i386', @$deps)) {
                        $required{$deppath} = 1;
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

See BACKEND INTERFACE in L<Sys::VirtV2V::GuestOS> for details.

=cut

sub remove_application
{
    my $self = shift;
    my $name = shift;

    my $g = $self->{g};
    eval {
        $g->command(['rpm', '-e', $name]);
    };
    die($@) if($@);

    # Make augeas reload in case the removal changed anything
    $g->aug_load();
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

    # All paths are relative to the transfer mount. Need to make them absolute.
    @rpms = map { $_ = $self->_transfer_path($_) } @rpms;

    my $g = $self->{g};
    eval {
        $g->command(['rpm', $upgrade == 1 ? '-U' : '-i', @rpms]);
    };

    # Propagate command failure
    die($@) if($@);

    # Reload augeas in case the rpm installation changed anything
    $g->aug_load();
}

# Get full, local path of a file on the transfer mount
sub _transfer_path
{
    my $self = shift;

    my ($path) = @_;

    $self->_ensure_transfer_mounted();

    return File::Spec->catfile($self->{transfer_mount}, $path);
}

# Ensure that the transfer device is mounted. If not, mount it.
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
        # Look for IDE and SCSI devices in fstab for the guest
        my %guestif;
        foreach my $spec ($g->aug_match('/files/etc/fstab/*/spec')) {
            my $device = $g->aug_get($spec);

            next unless($device =~ m{^/dev/(sd|hd)([a-z]+)});
            $guestif{$1} ||= {};
            $guestif{$1}->{$1.$2} = 1;
        }

        # If fstab contains references to sdX, these could refer to IDE or SCSI
        # devices. We may need to update them.
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

    # Go through fstab again, updating bare device references
    foreach my $spec ($g->aug_match('/files/etc/fstab/*/spec')) {
        my $device = $g->aug_get($spec);

        next unless($device =~ m{^/dev/([a-z]+)(\d*)});
        my $name = $1;
        my $part = $2;

        next unless(exists($map{$name}));

        $g->aug_set($spec, "/dev/".$map{$name}.$part);
    }
    $g->aug_save();
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

        # grubby can sometimes fail to correctly update grub.conf when run from
        # libguestfs. If it looks like this happened, install a new grub config
        # here.
        if (!$found) {
            # Check that an appropriately named kernel and initrd exist
            if ($g->exists("/boot/vmlinuz-$version") &&
                $g->exists("/boot/initrd-$version.img"))
            {
                $initrd = "$prefix/initrd-$version.img";

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
                eval {
                    $default = $g->aug_get('/files/boot/grub/menu.lst/default');
                };

                if (defined($default)) {
                    $g->aug_defvar('template',
                         '/files/boot/grub/menu.lst/title['.($default + 1).']');
                }

                # If there's no default, take the first entry with a kernel
                else {
                    my ($match) =
                        $g->aug_match('/files/boot/grub/menu.lst/title/kernel');

                    die("No template kernel found in grub")
                        unless(defined($match));

                    $match =~ s/\/kernel$//;
                    $g->aug_defvar('template', $match);
                }

                # Add a new title node at the end
                $g->aug_defnode('new',
                                '/files/boot/grub/menu.lst/title[last()+1]',
                                $title);

                # N.B. Don't change the order of root, kernel and initrd below,
                # or the guest will not boot.

                # Copy root from the template
                $g->aug_set('$new/root', $g->aug_get('$template/root'));

                # Set kernel and initrd to the new values
                $g->aug_set('$new/kernel', "$prefix/vmlinuz-$version");
                $g->aug_set('$new/initrd', "$prefix/initrd-$version.img");

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
            }

            else {
                die("Didn't find a grub entry for kernel version $version");
            }
        }

        eval {
            $g->aug_save();
        };

        if ($@) {
            my $msg = '';
            foreach my $error ($g->aug_match('/augeas//error')) {
                $msg .= $error.': '.$g->aug_get($error)."\n";
            }
            die($msg);
        }
    };

    # Propagate augeas failure
    die($@) if($@);

    if(!defined($initrd)) {
        print STDERR user_message(__x("WARNING: Kernel version {version} ".
                                      "doesn't have an initrd entry in grub",
                                      version => $version));
    } else {
        # Initrd as returned by grub may be relative to /boot
        $initrd = $self->{desc}->{boot}->{grub_fs}.$initrd;

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

        # We explicitly modprobe ext2 here. This is required by mkinitrd on RHEL
        # 3, and shouldn't hurt on other OSs. We don't care if this fails.
        eval {
            $g->modprobe("ext2");
        };

        # loop is a module in RHEL 5. Try to load it. Doesn't matter for other
        # OSs if it doesn't exist, but RHEL 5 will complain:
        #   All of your loopback devices are in use.
        eval {
            $g->modprobe("loop");
        };

        $g->command(["/sbin/mkinitrd", @preload_args, $initrd, $version]);
    }

    # Disable kudzu in the guest
    # Kudzu will detect the changed network hardware at boot time and either:
    #   require manual intervention, or
    #   disable the network interface
    # Neither of these behaviours is desirable.
    $g->command(['/sbin/chkconfig', 'kudzu', 'off']);
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

=back

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

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
