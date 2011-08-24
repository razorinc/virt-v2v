#!/usr/bin/perl
# virt-v2v
# Copyright (C) 2009-2011 Red Hat Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

use warnings;
use strict;

use Pod::Usage;
use Getopt::Long;

use Locale::TextDomain 'virt-v2v';

use Sys::Guestfs;

use Sys::VirtConvert;
use Sys::VirtConvert::Config;
use Sys::VirtConvert::Converter;
use Sys::VirtConvert::Connection::LibVirtSource;
use Sys::VirtConvert::Connection::LibVirtTarget;
use Sys::VirtConvert::Connection::LibVirtXMLSource;
use Sys::VirtConvert::Connection::RHEVTarget;
use Sys::VirtConvert::GuestfsHandle;
use Sys::VirtConvert::Util qw(:DEFAULT logmsg_init);

=encoding utf8

=head1 NAME

virt-v2v - Convert a guest to use KVM

=head1 SYNOPSIS

 virt-v2v -i libvirtxml -os imported --network default guest-domain.xml

 virt-v2v -ic esx://esx.server/ -os imported --network default esx_guest

 virt-v2v -ic esx://esx.server/ \
          -o rhev -os rhev.nfs.storage:/export_domain --network rhevm \
          esx_guest

=head1 DESCRIPTION

virt-v2v converts guests from a foreign hypervisor to run on KVM, managed by
libvirt or Red Hat Enterprise Virtualisation (RHEV) version 2.2 or later. It can
currently convert Red Hat Enterprise Linux and Windows guests running on Xen and
VMware ESX. It will enable VirtIO drivers in the converted guest if possible.

=head1 OPTIONS

=over 4

=cut

my $input_method = "libvirt";

=item B<-i input>

Specifies what input method to use to obtain the guest for conversion. The
default is C<libvirt>.  Supported options are:

=over

=item I<libvirt>

Guest argument is the name of a libvirt domain.

=item I<libvirtxml>

Guest argument is the path to an XML file containing a libvirt domain.

=back

=cut

my $input_uri = "qemu:///system";

=item B<-ic URI>

Specifies the connection to use when using the libvirt input method. If omitted,
this defaults to qemu:///system when virt-v2v runs as root, or qemu:///session
when virt-v2v runs as a regular user.

B<N.B.> virt-v2v can currently automatically obtain guest storage from local
libvirt connections, ESX connections, and connections over SSH. Other types of
connection are not supported.

=cut

my $output_method = "libvirt";

=item B<-o method>

Specifies the output method. Supported output methods are:

=over

=item libvirt

Create a libvirt guest. I<-os> must specify a libvirt storage pool for the
libvirt output method.

Also see the I<-oc> option.

=item rhev

Create a guest on a RHEV 'Export' storage domain, which can later be imported
into RHEV using the UI. I<-os> must specify the location of a RHEV export
storage domain for the RHEV output method.

=back

If no output type is specified, it defaults to libvirt.

=cut

my $output_uri = $> == 0 ? 'qemu:///system' : 'qemu:///session';

=item B<-oc URI>

Specifies the libvirt connection to use to create the converted guest. If
ommitted, this defaults to qemu:///system.

B<N.B.> virt-v2v must be able to write directly to storage described by this
libvirt connection. This makes writing to a remote connection impractical at
present.

=cut

my $output_storage;

=item B<-os storage>

The output method dependent location where new storage will be created for the
converted guest.

For the I<libvirt> output method, this must be the name of a storage pool.

For the I<rhev> output method, this specifies the NFS path to a RHEV Export
storage domain. Note that the storage domain must have been previously
initialised by RHEV. The domain must be in the format <host>:<path>, eg:

 rhev-storage.example.com:/rhev/export

The nfs export must be mountable and writable by the machine running virt-v2v.

=item B<-op pool>

See I<-os> for the I<libvirt> output method.

B<DEPRECATED> Use I<-os> instead.

=item B<-osd domain>

See I<-os> for the I<rhev> output method.

B<DEPRECATED> Use I<-os> instead.

=cut

my $output_format;

=item B<-of format>

Specifies the on-disk format which will be used for the converted guest.
Currently supported options are I<raw> and I<qcow2>. If not specified, the
converted guest will use the same format as the source guest.

=cut

my $output_sparse; # 1: sparse, 0: prealloc, undef: copy source

=item B<-oa allocation>

Specifies whether the converted guest should be I<sparse> or I<preallocated>. If
not specified, the converted guest will use the same allocation scheme as the
source.

=cut

my $output_name;

=item B<-on outputname>

Rename the guest.

If this option is not given, then the output name is the same
as the input name.

=cut

my @config_files;

=item B<-f file> | B<--config file>

Load a virt-v2v configuration from I<file>. Multiple configuration files can be
specified, which will be searched in the order they are specified on the command
line. If no configuration is specified, defaults to /etc/virt-v2v.conf and
/var/lib/virt-v2v/virt-v2v.db in that order.

When overriding the default config file it is recommended that
/var/lib/virt-v2v/virt-v2v.db is also specified, as it contains default
configuration data required for conversions.

=cut

my $network;

=item B<-n network> | B<--network network>

Map all guest bridges or networks which don't have a mapping in the
configuration file to I<network>.

This option cannot be used in conjunction with I<--bridge>.

=cut

my $bridge;

=item B<-b bridge> | B<--bridge bridge>

Map all guest bridges or networks which don't have a mapping in the
configuration file to I<bridge>.

This option cannot be used in conjunction with I<--network>.

=cut

my $profile;

=item B<-p profile> | B<--profile profile>

Take default values for output method, output storage and network mappings from
I<profile> in the configuration file.

=cut

my $root_choice = "ask";

=item B<--root=ask>

=item B<--root=single>

=item B<--root=first>

=item B<--root=/dev/sdX>

Choose the root filesystem to be converted.

In the case where the virtual machine is dual-boot or multi-boot, or
where the VM has other filesystems that look like operating systems,
this option can be used to select the root filesystem (a.k.a. "C:
drive" or "/") of the operating system that is to be converted. The
Windows Recovery Console, certain attached DVD drives, and bugs in
libguestfs inspection heuristics, can make a guest look like a
multi-boot operating system.

The default in virt-v2v E<le> 0.7.1 was I<--root=single>, which
causes virt-v2v to die if a multi-boot operating system is found.

Since virt-v2v E<ge> 0.7.2 the default is now I<--root=ask>: If the VM
is found to be multi-boot, then virt-v2v will stop and list the
possible root filesystems and ask the user which to use. This
requires that virt-v2v is run interactively.

I<--root=first> means to choose the first root device in the case of a
multi-boot operating system. Since this is a heuristic, it may
sometimes choose the wrong one.

You can also name a specific root device, eg. I<--root=/dev/sda2>
would mean to use the second partition on the first hard drive. If
the named root device does not exist or was not detected as a root
device, then virt-v2v will fail.

Note that there is a bug in grub which prevents it from successfully booting a
multiboot system if VirtIO is enabled. Grub is only able to boot an operating
system from the first VirtIO disk. Specifically, /boot must be on the first
VirtIO disk, and it cannot chainload an OS which is not in the first VirtIO
disk.

=cut

my $list_profiles = 0;

=item B<--list-profiles>

Display a list of target profile names specified in the configuration file.

=item B<--help>

Display brief help.

=item B<--version>

Display version number and exit.

=back

=cut

# Set to 1 if we're exiting due to a signal so cleanup handlers can potentially
# modify their behaviour accordingly
our $signal_exit = 0;

# Send log messages to STDOUT by default
logmsg_init(*STDOUT);

$SIG{'INT'} = \&signal_exit;
$SIG{'QUIT'} = \&signal_exit;

# SIGPIPE will cause an untidy exit of the perl process, without calling
# destructors. We don't rely on it anywhere, as we check for errors when reading
# from or writing to a pipe.
$SIG{'PIPE'} = 'IGNORE';

GetOptions ("help|?"      => sub {
                pod2usage(0);
            },
            "version"     => sub {
                print "$Sys::VirtConvert::VERSION\n";
                exit(0);
            },
            "c|connect"   => sub {
                # -c|--connect is the default for other virt tools. Be nice to
                # the user and point out that virt-v2v is different.
                pod2usage({ -message => __("Use -ic or -oc to specify an ".
                                           "input or an output connection"),
                            -exitval => 1 });
            },
            "i=s"         => \$input_method,
            "ic=s"        => \$input_uri,
            "o=s"         => \$output_method,
            "oc=s"        => \$output_uri,
            "os=s"        => \$output_storage,
            "op=s"        => \$output_storage, # Deprecated
            "osd=s"       => \$output_storage, # Deprecated
            "of=s"        => \$output_format,
            "oa=s"        => sub {
                my (undef, $value) = @_;
                $output_sparse = parse_allocation($value);
            },
            "on=s"        => \$output_name,
            "f|config=s"  => \@config_files,
            "n|network=s" => sub {
                my (undef, $value) = @_;

                pod2usage({ -message => __("--network may only be specified ".
                                           "once"),
                            -exitval => 1 }) if defined($network);
                pod2usage({ -message => __("--network and --bridge are ".
                                           "mutually exclusive"),
                            -exitval => 1 }) if defined($bridge);
                $network = $value;
            },
            "b|bridge=s"  => sub {
                my (undef, $value) = @_;

                pod2usage({ -message => __("--bridge may only be specified ".
                                           "once"),
                            -exitval => 1 }) if defined($bridge);
                pod2usage({ -message => __("--network and --bridge are ".
                                           "mutually exclusive"),
                            -exitval => 1 }) if defined($network);
                $bridge = $value;
            },
            "root=s"      => \$root_choice,
            "p|profile=s" => \$profile,
            "list-profiles" => \$list_profiles
) or pod2usage(2);

# Set the default configuration files if none are specified
if (@config_files == 0) {
    push(@config_files, '/etc/virt-v2v.conf') if -r '/etc/virt-v2v.conf';
    push(@config_files, '/var/lib/virt-v2v/virt-v2v.db')
        if -r '/var/lib/virt-v2v/virt-v2v.db';
}

sub parse_allocation
{
    my $allocation = shift;
    if ($allocation eq 'sparse') {
        return 1;
    } elsif ($allocation eq 'preallocated') {
        return 0;
    } else {
        pod2usage({ -message => __x("allocation scheme must be ".
                                    "{sparse} or {preallocated}",
                                    sparse => 'sparse',
                                    preallocated => 'preallocated'),
                    -exitval => 1 });
    }
}

# Set the umask to a reasonable default for virt-v2v
umask(0022);

# Read the config file
my $config = Sys::VirtConvert::Config->new(@config_files);

if ($list_profiles) {
    print STDOUT (__"Defined target profiles:")."\n";
    foreach my $profile ($config->list_profiles()) {
        print "  $profile\n";
    }
    exit(0);
}

if (defined($network)) {
    $config->set_default_net_mapping($network, 'network');
} elsif (defined($bridge)) {
    $config->set_default_net_mapping($bridge, 'bridge');
}

if (defined($profile)) {
    $config->use_profile($profile);

    $output_method = $config->get_method();

    $output_storage = $config->get_storage();
    my $opts = $config->get_storage_opts();

    my $allocation = $opts->{allocation};
    $output_sparse = parse_allocation($allocation) if defined($allocation);

    $output_format = $opts->{format};
}

pod2usage({ -message => __("You must specify an output storage location"),
            -exitval => 1 }) unless defined($output_storage);

my $target;
if ($output_method eq "libvirt") {
    $target = new Sys::VirtConvert::Connection::LibVirtTarget($output_uri,
                                                              $output_storage);
}

elsif ($output_method eq "rhev") {
    $target = new Sys::VirtConvert::Connection::RHEVTarget($output_storage);
}

else {
    v2vdie __x('{output} is not a valid output method.',
               output => $output_method);
}

# Get an appropriate Source
my $source;
if ($input_method eq "libvirtxml") {
    my $path = shift(@ARGV) or
        pod2usage({ -message => __"You must specify a filename",
                    -exitval => 1 });

    # Warn if we were given more than 1 argument
    if(scalar(@ARGV) > 0) {
        logmsg WARN, __x('{modulename} only takes a single filename.',
                         modulename => 'libvirtxml');
    }

    $source = Sys::VirtConvert::Connection::LibVirtXMLSource->new($path);
}

elsif ($input_method eq "libvirt") {
    my $name = shift(@ARGV) or
        pod2usage({ -message => __"You must specify a guest",
                    -exitval => 1 });

    $source = Sys::VirtConvert::Connection::LibVirtSource->new($input_uri,
                                                               $name);

    # Warn if we were given more than 1 argument
    if(scalar(@ARGV) > 0) {
        logmsg WARN, __x('{modulename} only takes a single domain name.',
                          modulename => 'libvirt');
    }
}

else {
    v2vdie __x('{input} is not a valid input method.', input => $input_method);
}


###############################################################################
## Start of processing

# Decide the name of the guest target.
$output_name = $source->get_name() unless defined $output_name;

# Check that the guest doesn't already exist on the target
v2vdie __x('Domain {name} already exists on the target.',
           name => $output_name)
    if $target->guest_exists($output_name);

# Create the transfer iso if required
my $transferiso = $config->get_transfer_iso();

# Get a libvirt configuration for the guest
my $meta = $source->get_meta();
exit(1) unless(defined($meta));

v2vdie __('Guest doesn\'t define any storage devices')
    unless @{$meta->{disks}} > 0;

# Copy source storage to target
$source->copy_storage($target, $output_format, $output_sparse);

# Open a libguestfs handle on the guest's storage devices
my @localpaths = map { $_->{dst}->get_local_path() } @{$meta->{disks}};
my $g = new Sys::VirtConvert::GuestfsHandle(
    \@localpaths,
    $transferiso,
    $output_method eq 'rhev'
);

# Get the name of the appliance's transfer device, if it has one
my $transferdev;
if (defined($transferiso)) {
    my @devices = $g->list_devices();
    $transferdev = pop(@devices);
}

my $guestcaps;
my $root;
eval {
    # Inspect the guest
    $root = inspect_guest($g, $transferdev);

    # Modify the guest and its metadata
    $guestcaps =
        Sys::VirtConvert::Converter->convert($g, $config, $root, $meta);

    $target->create_guest($g, $root, $meta, $config, $guestcaps, $output_name);
};

# If any of the above commands result in failure, we need to ensure that the
# guestfs qemu process is cleaned up before further cleanup. Failure to do this
# can result in failure to umount RHEV export's temporary mount point.
if ($@) {
    my $err = $@;
    $g->close();
    die($err);
}

$g->close();

if($guestcaps->{block} eq 'virtio' && $guestcaps->{net} eq 'virtio') {
    logmsg NOTICE, __x('{name} configured with virtio drivers.',
                       name => $output_name);
} elsif ($guestcaps->{block} eq 'virtio') {
    logmsg NOTICE, __x('{name} configured with virtio storage only.',
                       name => $output_name);
} elsif ($guestcaps->{net} eq 'virtio') {
    logmsg NOTICE, __x('{name} configured with virtio networking only.',
                       name => $output_name);
} else {
    logmsg NOTICE, __x('{name} configured without virtio drivers.',
                       name => $output_name);
}

exit(0);

# die() sets $? to 255, which is untidy.
END {
    $? = $? == 255 ? 1 : $?;
}

###############################################################################
## Helper functions

sub signal_exit
{
    # Tell cleanup handlers that we're exiting due to a signal
    $signal_exit = 1;

    $g->close() if (defined($g));
    v2vdie __x('Received signal {sig}. Exiting.', sig => shift);
}

# Perform guest inspection using the libguestfs core inspection API.
# Returns the root device of the os to be converted.
sub inspect_guest
{
    my $g = shift;
    my $transferdev = shift;

    # Get list of roots, sorted.
    my @roots = $g->inspect_os();

    # Filter out the transfer device from the results of inspect_os
    # There's a libguestfs bug (fixed upstream) which meant the transfer ISO
    # could be erroneously detected as an unknown Windows OS. As we know what it
    # is, we can filter out the transfer device here. Even when the fix is
    # released this is reasonable belt & braces.
    @roots = grep(!/^\Q$transferdev\E$/, @roots);

    @roots = sort @roots;

    my $root_dev;

    if(@roots == 0) {
        v2vdie __('No root device found in this operating system image.');
    }

    if (@roots == 1) {
        $root_dev = $roots[0];
    } else {
        # > 1 roots found. Depends on the --root option / $root_choice.
        if ($root_choice eq "ask") {
            # List out the roots and ask user to choose.
            print "\n***\n";
            print __("Dual- or multi-boot operating system detected. Choose the root filesystem\nthat contains the main operating system from the list below:\n");
            print "\n";
            my $i = 1;
            foreach (@roots) {
                print " [$i] $_";
                my $prod;
                eval { $prod = $g->inspect_get_product_name ($_) };
                print " ($prod)" if defined $prod;
                print "\n";
                $i++;
            }
            $i--;
            print "\n";
            my $j = 0;
            while ($j < 1 || $j > $i) {
                print __x("Enter number between 1 and {i}: ", i => $i);
                $j = int (<STDIN>);
            }
            $root_dev = $roots[$j-1];
        }
        elsif ($root_choice eq "single") {
            v2vdie __('Multi-boot operating systems are not supported by virt-v2v. Use the --root option to change how virt-v2v handles this.')
        }
        elsif ($root_choice eq "first") {
            # Choose the first one.
            $root_dev = $roots[0];
        }
        elsif ($root_choice =~ m|^/dev/[hsv]d([a-z]+[0-9]*)$|) {
            # Choose the named root.
            my $partnum = $1;
            foreach (@roots) {
                if ($_ =~ m|^/dev/.d$partnum$|) {
                    $root_dev = $_;
                    last;
                }
            }
            unless (defined ($root_dev)) {
                v2vdie __x('Root device "{choice}" not found. Roots found were: {roots}.',
                           choice => $root_choice,
                           roots => join ' ', @roots)
            }
        }
        elsif ($root_choice =~ m|^/dev/|) {
            # Check the chosen root exists.
            foreach (@roots) {
                if ($root_choice eq $_) {
                    $root_dev = $_;
                    last;
                }
            }
            unless (defined $root_dev) {
                v2vdie __x('Root device "{choice}" not found. Roots found were: {roots}.',
                           choice => $root_choice,
                           roots => join ' ', @roots)
            }
        }
        else {
            v2vdie __x('Unknown --root option "{choice}".',
                       choice => $root_choice)
        }
    }

    return $root_dev;
}

=head1 PREPARING TO CONVERT A GUEST

=head2 Local storage requirements

Whenever possible, virt-v2v copies a guest's storage directly from the source
hypervisor to the target hypervisor without using any local storage. However,
this is not possible in all circumstances. Specifically when transferring a
guest's storage over SSH and also either doing a format conversion, or changing
the allocation policy of qcow2 storage, virt-v2v will cache a local copy of the
guest's storage. By default, this local cache will be created in /tmp.  If /tmp
does not have sufficient storage space, it can be written to another directory
by setting the I<TMPDIR> environment variable.

=head2 Local Xen guests

B<N.B.> The following is required when converting guests on a host which used to
run Xen, but has been updated to run KVM. It is not required when converting a
Xen guest imported directly from a running libvirt/Xen instance.

virt-v2v uses a libvirt domain description to determine the current
configuration of the guest, including the location of its storage. This should
be obtained from the host running the guest pre-conversion by running:

 virsh dumpxml <domain> > <domain>.xml

This will require a reboot if the host running Xen is the same host that will
run KVM. This is because libvirt needs to connect to a running xen hypervisor to
obtain its metadata.

=head2 Converting to run on libvirt/KVM

=head3 Create a local storage pool for transferred storage

virt-v2v copies the guest storage to the local machine during import. When
converting to run on libvirt, it creates new storage in a locally defined
libvirt pool. This pool can be defined using any libvirt tool, and can be of any
type.

The simplest way to create a new pool is with L<virt-manager(1)>. Pools can be
defined from the Storage tab under Host Details.

=head3 Create local network interfaces

The local machine must have an appropriate network for the converted guest
to connect to. This is likely to be a bridge interface. A bridge interface can
be created using standard tools on the host.

Since version 0.8.3, L<virt-manager(1)> can also create and manage bridges.

=head2 Converting to run on RHEV

=head3 Create an NFS export domain

virt-v2v can convert guests to run on RHEV 2.2 or later. It does this by writing
the converted guest directly to an 'Export' NFS storage domain. The guest can
later be imported into a RHEV Data Center through the UI.

In RHEV 2.2, a new Export storage domain is created by clicking on 'New Domain'
in the Storage tab. Ensure that the Domain function is 'Export' and the Storage
type is 'NFS'. See the RHEV documentation for details. The NFS storage domain must be mountable by the machine running virt-v2v.

B<N.B.> When exporting to RHEV, virt-v2v must run as root.

=head3 Import the appropriate Guest Tools ISO

When converting Windows guests, it is strongly recommended that the Guest Tools
ISO is installed before the guest is converted. This must be done using the ISO
Uploader, which can be found on your RHEV-M system under Start->Red Hat->RHEV
Manager->ISO Uploader. This will allow RHEV to automatically update the guest's
drivers to the latest versions and install any required agents.

=head1 CONVERTING A LOCAL XEN GUEST

The following requires that the domain XML is available locally, and that the
storage referred to in the domain XML is available locally at the same paths.

To perform the conversion, run:

 virt-v2v -i libvirtxml -os <pool> [--network <network name>] \
          <domain>.xml

where C<< <domain>.xml >> is the path to the exported guest domain's xml, and
C<< <pool> >> is the local storage pool where copies of the guest's disks will
be created.

The I<--network> option may be provided for simple network mappings. For more
complex mappings, see L<virt-v2v.conf(5)>.

If it is not possible to provide software updates over the network in your
environment, software will be installed as specified in virt-v2v.conf. See
L<virt-v2v.conf(5)> for a details.

It is possible to avoid specifying replacement kernels in the virt-v2v config
file by ensuring that the guest has an appropriate kernel installed prior to
conversion. If your guest uses a Xen paravirtualised kernel (it would be called
something like kernel-xen or kernel-xenU), you can install a regular kernel,
which won't reference a hypervisor in its name, alongside it.  You shouldn't
make this newly installed kernel your default kernel because Xen may not boot
it. virt-v2v will make it the default during conversion.

=head1 CONVERTING A GUEST FROM VMWARE ESX

B<N.B.> libvirt version 0.7.0 or greater is required to connect to ESX.

virt-v2v can convert a guest from VMware ESX, including transferring its
storage.

B<N.B.> virt-v2v does not transfer snapshots from ESX. Only the latest flat
storage is transferred.

The guest MUST be shut down in ESX before conversion starts. virt-v2v will not
proceed if the guest is still running. To convert the guest, run:

 virt-v2v -ic esx://<esx.server>/ -os <pool> [--network <network name>] \
          <domain>

where:

=over

=item *

E<lt>esx.serverE<gt> is the hostname of the ESX server hosting the guest to be
converted.

B<N.B.> This hostname must match the hostname reported in the ESX server's SSL
certificate, or verification will fail.

=item *

E<lt>poolE<gt> is the name of the local storage pool where copies of the guest's
storage will be created.

=item *

E<lt>domainE<gt> is the name of the guest on the ESX server which is to be
converted.

=back

The I<--network> option may be provided for simple network mappings. For more
complex mappings, see L<virt-v2v.conf(5)>.

=head2 Authenticating to the ESX server

Connecting to the ESX server will require authentication. virt-v2v supports
password authentication when connecting to ESX. It reads passwords from
$HOME/.netrc. The format of this file is described in L<netrc(5)>. An example
entry is:

 machine esx01.example.com login root password s3cr3t

B<N.B.> The permissions of .netrc MUST be set to 0600, or it will be ignored.

=head2 Connecting to an ESX server with an invalid certificate

In non-production environments, the ESX server may have an invalid certificate,
for example a self-signed certificate. In this case, certificate checking can be
explicitly disabled by adding '?no_verify=1' to the connection URI as shown
below:

 ... -ic esx://<esx.server>/?no_verify=1 ...

=head1 EXPORTING A GUEST TO RHEV

virt-v2v can export to RHEV any guest that it can convert. This includes:

=over

=item *

Local Xen guests

=item *

ESX guests

=item *

Local libvirt/KVM guests

=back

To export to RHEV, specify I<-o rhev> on the command line, and ensure I<-os>
specifies the location of a RHEV export storage domain as in the following
examples:

=over

=item Exporting a local Xen guest to RHEV

 virt-v2v -i libvirtxml -o rhev -os <export_sd> \
          [--network <network name>] <domain>.xml

=item Export a VMWare ESX guest to RHEV

 virt-v2v -ic esx://<esx.server>/ -o rhev -os <export_sd> \
          [--network <network name>] <domain>

=item Export a local libvirt/KVM guest to RHEV

 virt-v2v -o rhev -os <export_sd> [--network <network name>] \
          <domain>

=back

=head1 RUNNING THE CONVERTED GUEST

=head2 Libvirt output method

On successful completion, virt-v2v will create a new libvirt domain for the
converted guest with the same name as the original guest. It can be started as
usual using libvirt tools, for example L<virt-manager(1)>.

=head2 RHEV output method

On successful completion virt-v2v will have written the new guest to the export
storage domain, but it will not yet be ready to run. It must be imported into
RHEV using the UI before it can be used.

In RHEV 2.2 this is done from the Storage tab. Select the export domain the
guest was written to. A pane will appear underneath the storage domain list
displaying several tabs, one of which is 'VM Import'. The converted guest will
be listed here. Select the appropriate guest an click 'Import'. See the RHEV
documentation for additional details.

=head1 POST-CONVERSION TASKS

=head2 Guest network configuration

virt-v2v cannot currently reconfigure a guest's network configuration. If the
converted guest is not connected to the same subnet as the source, its network
configuration may have to be updated.

=head2 Converting a Windows guest

When converting a Windows guests, the conversion process is split into 2 stages:

=over

=item 1

Offline conversion.

=item 2

First boot.

=back

The guest will be bootable after the offline conversion stage, but will not yet
have all necessary drivers installed to work correctly. These will be installed
automatically the first time the guest boots.

B<N.B.> Take care not to interrupt the automatic driver installation process
when logging in to the guest for the first time, as this may prevent the guest
from subsequently booting correctly.

=head2 Windows Recovery Console

virt-v2v does not support conversion of the Windows Recovery Console. If a guest
has a recovery console installed and VirtIO was enabled during conversion,
attempting to boot the recovery console will result in a BSOD.

Windows XP x86 does not support the Windows Recovery Console on VirtIO systems,
so there is no resolution to this. However, on Windows XP AMD64 and Windows 2003
(x86 and AMD64), the recovery console can be re-installed after conversion. The
re-installation procedure is the same as the initial installation procedure. It
is not necessary to remove the recovery console first.  Following
re-installation, the recovery console will work as intended.

=head1 GUEST CONFIGURATION CHANGES

As well as configuring libvirt appropriately, virt-v2v will make certain changes
to a guest to enable it support running under a KVM host either with or without
virtio driver. These changes are guest OS specific. Currently only Red Hat based
Linux distributions are supported.

=head2 Linux

virt-v2v will make the following changes to a Linux guest:

=over

=item Kernel

Un-bootable, i.e. xen paravirtualised, kernels will be uninstalled. No new
kernel will be installed if there is a remaining kernel which supports virtio.
If no remaining kernel supports virtio and the configuration file specifies a
new kernel it will be installed and configured as the default.

=item X reconfiguration

If the guest has X configured, its display driver will be updated. See L</GUEST
DRIVERS> for which driver will be used.

=item Rename block devices

If changes have caused block devices to change name, these changes will be
reflected in /etc/fstab.

=item Configure device drivers

Whether virtio or non-virtio drivers are configured, virt-v2v will ensure that
the correct network and block drivers are specified in the modprobe
configuration.

=item initrd

virt-v2v will ensure that the initrd for the default kernel supports booting the
root device, whether it is using virtio or not.

=item SELinux

virt-v2v will initiate a relabel of the guest on the next boot. This ensures
that any changes it has made are correctly labelled according to the guest's
local policy.

=back

=head1 LINUX GUEST DRIVERS

Virt-v2v will configure the following drivers in a Linux guest:

=head2 VirtIO

 X display      cirrus
 Block          virtio_blk
 Network        virtio_net

Additionally, initrd will preload the virtio_pci driver.

=head2 Non-VirtIO

 X display      cirrus
 Block          IDE
 Network        e1000

=head1 WINDOWS GUEST DRIVERS

Virt-v2v will configure the following drivers in a Windows guest:

=head2 VirtIO

 X display      cirrus
 Block          viostor
 Network        netkvm

=head2 Non-VirtIO

 X display      cirrus
 Block          IDE
 Network        rtl8139

=head1 BUGS

To get a list of bugs against virt-v2v use this link:

L<https://bugzilla.redhat.com/buglist.cgi?component=virt-v2v&product=Virtualization+Tools>

To report a new bug against virt-v2v use this link:

L<https://bugzilla.redhat.com/enter_bug.cgi?component=virt-v2v&product=Virtualization+Tools>

When reporting a bug, please check:

=over

=item *

That the bug hasn't been reported already.

=item *

That you are testing a recent version.

=item *

Describe the bug accurately, and give a way to reproduce it.

=back

=head1 SEE ALSO

L<virt-v2v.conf(5)>,
L<virt-manager(1)>,
L<http://libguestfs.org/>.

=head1 AUTHOR

Richard W.M. Jones L<http://et.redhat.com/~rjones/>

Matthew Booth <mbooth@redhat.com>

=head1 COPYRIGHT

Copyright (C) 2009-2011 Red Hat Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
