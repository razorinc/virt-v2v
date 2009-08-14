# Sys::Guestfs::GuestOS
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

package Sys::Guestfs::GuestOS;

use strict;
use warnings;

use Carp;
use File::Spec;
use File::Temp;

use Module::Pluggable::Ordered sub_name => 'modules',
                               search_path => 'Sys::Guestfs::GuestOS',
                               require => 1;

use Locale::TextDomain 'libguestfs';

=pod

=head1 NAME

Sys::Guestfs::GuestOS - Manipulate and query a Guest OS

=head1 SYNOPSIS

 use Sys::Guestfs::GuestOS;

 $guestos = Sys::Guestfs::GuestOS->instantiate($g, $desc, $files, $deps)

=head1 DESCRIPTION

Sys::Guestfs::GuestOS provides a mechanism for querying and manipulating a
specific guest operating system.

Sys::Guestfs::GuestOS is an interface to various backends, each of
which implement a consistent API. Sys::Guestfs::GuestOS itself only
implements methods to access backends.

Sys::Guestfs::GuestOS uses L<Module::Pluggable::Ordered> to automatically
discover backends under Sys::Guestfs::GuestOS.

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

=item instantiate(g, desc, files, deps)

Instantiate a GuestOS object capable of manipulating the target OS.

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

=back

Returns a capable Sys::Guestfs::GuestOS backend if one is found.

Returns undef otherwise.

=back

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

Read the [files] and [deps] section of the virt-v2v config file. Create the
transfer iso from the contents of [files].

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
            print STDERR "virt-v2v: ".__x("WARNING unable to access {path}.",
                                           path => $path)."\n";
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
    system('genisoimage', '-o', $transferiso, '-r', '-J',
           '-V', '__virt-v2v_transfer__', keys(%paths));

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

    if(defined($deps_conf)) {
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

1;

=head1 BACKEND METHODS

All GuestOS backends are required to implement the following methods:

=over

=item CLASS->can_handle(desc)

=over

=item desc

An OS description as returned by L<Sys::Guestfs::Lib>.

=back

Returns true if the backend can handle the guest described by L<desc>.

can_handle is only intended to be called by Sys::Guestfs::GuestOS.

=item CLASS->new(g, desc, files, deps)

See instantiate above for a description of the arguments.

Instantiate a new backend object. Assumes can_handle has previously returned
true.

new is only intended to be called by Sys::Guestfs::GuestOS.

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

=item add_kernel

add_kernel installs a new kernel. It chooses a kernel label based on the name of
the default kernel installed in the guest. See L</INSTALLING FILES> for more
details of how files are selected for installation.

add_kernel will also install dependencies of the chosen kernel.

add_kernel returns the version number of the kernel it installed.

=item remove_kernel(version)

=over

=item version

The version number of the kernel to be removed.

=back

remove_kernel uninstalls a kernel from the guest.

=item add_application(label)

=over

=item label

The label of the application to be installed. See L</INSTALLING FILES> for
more details.

=back

add_application installs an application and its dependencies into the guest.

=item remove_application(name)

=over

=item name

The name of the the application, as it is known to the underlying package
manager.

=back

remove an application from the guest.

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

=back

=head1 INSTALLING FILES

Because different guests may need different files to be installed to satisfy
a given requirement, files are installed by I<label> rather than by file name.
Labels are given on the virt-v2v command line with the I<--file> option.

When choosing which file to install, the requested label name will be considered
along with 4 aspects of the guest:

=over

=item distro

The distribution name discovered by L<Sys::Guestfs::Lib>, e.g. 'rhel'.

=item major

The major version number of the distribution.

=item minor

The minor version number of the distribution.

=item arch

The required architecture of the file to be installed.

=back

GuestOS will search for a matching label in the following order:

1. distro.major.minor.arch.label
2. distro.major.minor.label
3. distro.major.arch.label
4. distro.major.label
5. distro.arch.label
6. distro.label

So, if the guest is RHEL 5.3 x86_64 and the given label is 'udev', you can
specify any of the following on the command line:

 --file rhel.5.3.x86_64.ecryptfs-utils=<ecryptfs-utils rpm>
 --file rhel.5.3.ecryptfs-utils=<ecryptfs-utils rpm>
 --file rhel.5.x86_64.ecryptfs-utils=<ecryptfs-utils rpm>
 --file rhel.5.ecryptfs-utils=<ecryptfs-utils rpm>
 --file rhel.x86_64.ecryptfs-utils=<ecryptfs-utils rpm>
 --file rhel.ecryptfs-utils=<ecryptfs-utils rpm>

Which I<should> be specified depends on the applicability of the target file. In
this case it would be I<rhel.5.x86_64.ecryptfs-utils>.

=head1 INSTALLING DEPENDENCIES

virt-v2v requires that all necessary files are made available before it is
invoked. This includes dependencies of new files which are to be installed into
a guest. Dependencies must be specified manually with the I<--dep> command line
option.

Dependencies are specified on labels, and define new labels. Labels are resolved
as described in L</INSTALLING FILES>.

So, for example, to specify that when installing a new kernel on RHEL 5.2 x86_64
you also need to install new versions of ecryptfs-utils and lvm2, add the
following command line argument:

 --dep rhel.5.2.kernel="ecryptfs-utils lvm2"

This will cause GuestOS to first resolve both labels ecryptfs-utils and lvm2 for
the current guest, then check that the requested package is both installed, and
at the same or a greater version number that the given package. If this is not
the case the package will be installed or upgraded.

Dependencies can be specified recursively to any depth.

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::Guestfs>,
L<Sys::Guestfs::Lib>,
L<guestfs(3)>,
L<http://libguestfs.org/>,
L<virt-v2v(1)>

=cut
