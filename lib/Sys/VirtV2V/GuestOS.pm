# Sys::VirtV2V::GuestOS
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

package Sys::VirtV2V::GuestOS;

use strict;
use warnings;

use Carp;
use File::Spec;
use File::Temp;

use Sys::VirtV2V::ExecHelper;
use Sys::VirtV2V::UserMessage qw(user_message);

use Module::Pluggable sub_name => 'modules',
                      search_path => 'Sys::VirtV2V::GuestOS',
                      require => 1;

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::GuestOS - Manipulate and query a Guest OS

=head1 SYNOPSIS

 use Sys::VirtV2V::GuestOS;

 $guestos = Sys::VirtV2V::GuestOS->instantiate($g, $desc);

=head1 DESCRIPTION

Sys::VirtV2V::GuestOS provides a mechanism for querying and manipulating a
specific guest operating system.

Sys::VirtV2V::GuestOS is an interface to various backends, each of
which implement a consistent API. Sys::VirtV2V::GuestOS itself only
implements methods to access backends.

Sys::VirtV2V::GuestOS uses L<Module::Pluggable> to automatically discover
backends under Sys::VirtV2V::GuestOS.

=cut

# A map of file labels to their paths relative to the transfer device
my %files;

# A map of file labels to their dependencies
my %deps;

# A map of file labels aliases
my %aliases;

# The path (on the host) to the transfer iso
my $transferiso;

=head1 METHODS

=over

=item instantiate(g, desc)

Instantiate a GuestOS object capable of manipulating the target OS.

=over

=item g

A L<Sys::Guestfs> handle.

=item desc

An OS description created by L<Sys::Guestfs::Lib>.

=back

Returns a capable Sys::VirtV2V::GuestOS backend if one is found.

Returns undef otherwise.

=cut

sub instantiate
{
    my $class = shift;

    my ($g, $desc) = @_;
    defined($g) or carp("get_instance called without g argument");
    defined($desc) or carp("get_instance called without desc argument");

    foreach my $module ($class->modules()) {
        return $module->new($g, $desc, \%files, \%deps, \%aliases)
                if($module->can_handle($desc));
    }

    return undef;
}

=item configure(config)

=over

=item config

The parsed virt-v2v config file, as returned by Config::Tiny.

=back

Read the [files], [deps] and [aliases] sections of the virt-v2v config file.
Create the transfer iso from the contents of [files].

=cut

sub configure
{
    my $class = shift;

    my $config = shift;

    carp("configure called without config argument") unless(defined($config));

    # Lookup the [files] config section
    my $files_conf = $config->{files};

    # Do nothing if there is no [files] config section
    return unless(defined($files_conf));

    # A hash, whose labels are filenames to be added to the transfer iso. We use
    # a hash here to remove duplicates.
    my %paths = ();
    foreach my $label (keys(%$files_conf)) {
        my $path = $files_conf->{$label};

        unless(-f $path && -r $path) {
            print STDERR user_message(__x("WARNING: unable to access {path}.",
                                          path => $path));
            next;
        }

        $paths{$path} = 1;

        # As transfer directory hierarchy is flat, remove all directory
        # components from paths
        my (undef, undef, $filename) = File::Spec->splitpath($path);
        $files{$label} = $filename;
    }

    # Do nothing if there are no files defined
    return if(keys(%paths) == 0);

    $transferiso = File::Temp->new(UNLINK => 1, SUFFIX => '.iso');
    my $eh = Sys::VirtV2V::ExecHelper->run
        ('mkisofs', '-o', $transferiso, '-r', '-J',
         '-V', '__virt-v2v_transfer__', keys(%paths));
    if($eh->status() != 0) {
        print STDERR user_message(__x("Failed to create transfer iso. Command ".
                                      "output was:\n{output}",
                                      output => $eh->output()));
    }

    # Populate deps from the [deps] config section
    my $deps_conf = $config->{deps};

    if(defined($deps_conf)) {
        # Copy the deps_conf hash into %deps
        foreach my $label (keys(%$deps_conf)) {
            $deps{$label} = $deps_conf->{$label};
        }
    }

    # Populate aliases from the [aliases] config section
    my $aliases_conf = $config->{aliases};

    if(defined($aliases_conf)) {
        # Copy the aliases_conf hash into %aliases
        foreach my $label (keys(%$aliases_conf)) {
            $aliases{$label} = $aliases_conf->{$label};
        }
    }
}

=item get_transfer_iso

Return the path (on the host) to the transfer iso image. L</configure> must have
been called first.

=cut

sub get_transfer_iso
{
    return $transferiso;
}

=back

=head1 BACKEND INTERFACE

All GuestOS backends are required to implement the following methods:

=over

=item CLASS->can_handle(desc)

=over

=item desc

An OS description as returned by L<Sys::Guestfs::Lib>.

=back

Returns true if the backend can handle the guest described by L<desc>.

can_handle is only intended to be called by Sys::VirtV2V::GuestOS.

=item CLASS->new(g, desc, files, deps, aliases)

=over

=item g

A L<Sys::Guestfs> handle.

=item desc

An OS description created by L<Sys::Guestfs::Lib>.

=item files

A hash containing 'label => filename' mappings. These mappings are consulted
when a guest needs to install a specific application.

=item deps

A hash containing 'label => C<space separated dependency list>'. The
dependencies are given as labels rather than specific files. This is used to
install dependencies when installing an application in the guest.

=item aliases

A hack containing 'label => alias'. Aliases are given as labels rather than
specific files. This is used to substitute packages during installation.

=back

Construct a new backend object. Assumes can_handle has previously returned true.

new is only intended to be called by Sys::VirtV2V::GuestOS.

=item get_handle()

Return the libguestfs handle used by this Sys::VirtV2V::GuestOS object.

=item enable_kernel_module(device, module)

=over

=item device

The name of the device as it is known to modprobe. Examples are I<eth0> or
I<scsi_hostadapter>.

=item module

The name of the kernel module which should be loaded for this device. An example
is I<virtio_blk>.

=back

enable_kernel_module adds a new kernel module to the modprobe configuration.

=item update_kernel_module(device, module)

See enable_kernel_module for argument descriptions.

update_kernel_module changes an existing modprobe configuration in place.

=item disable_kernel_module(device)

See enable_kernel_module for argument description.

disable_kernel_module removes the modprobe configuration for the given device.

=item update_display_driver(driver)

=over

=item driver

The name of the new disply driver. An example is I<cirrus>.

=back

Update the display driver, if defined, to the given driver.

=item get_default_kernel

get_default_kernel returns the version number of the kernel which will be booted
according to the current configuration. It examines the guest directly rather
than relying on the output from Sys::Guestfs::Lib, which may be out of date.

=item add_kernel

add_kernel installs a new kernel. It chooses a kernel label based on the name of
the default kernel installed in the guest. See L<virt-v2v(5)> for details of how
files are selected for installation.

add_kernel will also install dependencies of the chosen kernel.

add_kernel returns the version number of the kernel it installed, or undef if it
did not find a kernel to install.

=item remove_kernel(version)

=over

=item version

The version number of the kernel to be removed.

=back

remove_kernel uninstalls a kernel from the guest.

=item add_application(label)

=over

=item label

The label of the application to be installed. See L<virt-v2v.conf(5)> for more
details.

=back

add_application installs an application and its dependencies into the guest.

=item remove_application(name)

=over

=item name

The name of the the application, as it is known to the underlying package
manager.

=back

remove an application from the guest.

=item get_application_owner(file)

=over

=item file

The absolute path to a file or directory on the guest.

=back

Return the name of the application which owns the given file.

get_application_owner() will throw an exception if the file is not owned by any
application.

=item remap_block_devices(map)

=over

=item map

A map of old names to new names specified as (old1 => new1, old2 => new2...).

=back

Update the guest to reflect the renaming of block devices.

=item prepare_bootable(version [, module, module, ...])

=over

=item version

The version number of the kernel which we want to boot.

=item module

A module which must be available at boot time.

=back

Ensure that the guest will boot the given kernel version by default, and it will
make available the given modules

=item supports_virtio(kernel)

=over

=item kernel

The version number of the kernel to be inspected

=back

Returns 1 if the target kernel supports virtio, 0 otherwise.

=back

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-v2v(1)>,
L<Sys::VirtV2V::GuestOS::RedHat(3pm)>,
L<Sys::Guestfs(3pm)>,
L<Sys::Guestfs::Lib(3pm)>,
L<http://libguestfs.org/>.

=cut

1;
