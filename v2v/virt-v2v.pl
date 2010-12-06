#!/usr/bin/perl
# virt-v2v
# Copyright (C) 2009 Red Hat Inc.
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
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

use warnings;
use strict;

use Pod::Usage;
use Getopt::Long;

use Locale::TextDomain 'virt-v2v';

use Sys::Guestfs;
use Sys::Guestfs::Lib qw(get_partitions inspect_all_partitions
                         inspect_operating_systems mount_operating_system
                         inspect_in_detail);

use Sys::VirtV2V;
use Sys::VirtV2V::Config;
use Sys::VirtV2V::Converter;
use Sys::VirtV2V::Connection::LibVirtSource;
use Sys::VirtV2V::Connection::LibVirtTarget;
use Sys::VirtV2V::Connection::LibVirtXMLSource;
use Sys::VirtV2V::Connection::RHEVTarget;
use Sys::VirtV2V::ExecHelper;
use Sys::VirtV2V::GuestfsHandle;
use Sys::VirtV2V::Util qw(user_message);

=encoding utf8

=head1 NAME

virt-v2v - Convert a guest to use KVM

=head1 SYNOPSIS

 virt-v2v -i libvirtxml -op imported --network default guest-domain.xml

 virt-v2v -ic esx://esx.server/ -op imported --network default esx_guest

 virt-v2v -ic esx://esx.server/ \
          -o rhev -osd rhev.nfs.storage:/export_domain --network rhevm \
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
this defaults to qemu:///system.

B<N.B.> virt-v2v can currently automatically obtain guest storage from local
libvirt connections, ESX connections, and connections over SSH. Other types of
connection are not supported.

=cut

my $output_method = "libvirt";

=item B<-o method>

Specifies the output method. Supported output methods are:

=over

=item libvirt

Create a libvirt guest. See the I<-oc> and I<-op> options. I<-op> must be
specified for the libvirt output method.

=item rhev

Create a guest on a RHEV 'Export' storage domain, which can later be imported
into RHEV using the UI. I<-osd> must be specified for the rhev output method.

=back

If no output type is specified, it defaults to libvirt.

=cut

my $output_uri = "qemu:///system";

=item B<-oc URI>

Specifies the libvirt connection to use to create the converted guest. If
ommitted, this defaults to qemu:///system.

B<N.B.> virt-v2v must be able to write directly to storage described by this
libvirt connection. This makes writing to a remote connection impractical at
present.

=cut

my $output_pool;

=item B<-op pool>

Specifies the pool which will be used to create new storage for the converted
guest.

=cut

my $output_storage_domain;

=item B<-osd domain>

Specifies the NFS path to a RHEV Export storage domain. Note that the storage
domain must have been previously initialised by RHEV.

The domain must be in the format <host>:<path>, eg:

 rhev-storage.example.com:/rhev/export

The nfs export must be mountable and writable by the machine running virt-v2v.

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

my $config_file;
$config_file = '/etc/virt-v2v.conf' if (-r '/etc/virt-v2v.conf');

=item B<-f file> | B<--config file>

Load the virt-v2v configuration from I<file>. Defaults to /etc/virt-v2v.conf if
it exists;

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

=item B<--help>

Display brief help.

=item B<--version>

Display version number and exit.

=back

=cut

# Set to 1 if we're exiting due to a signal so cleanup handlers can potentially
# modify their behaviour accordingly
our $signal_exit = 0;

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
                print "$Sys::VirtV2V::VERSION\n";
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
            "op=s"        => \$output_pool,
            "osd=s"       => \$output_storage_domain,
            "of=s"        => \$output_format,
            "oa=s"        => sub {
                my (undef, $value) = @_;

                if ($value eq 'sparse') {
                    $output_sparse = 1;
                } elsif ($value eq 'preallocated') {
                    $output_sparse = 0;
                } else {
                    pod2usage({ -message => __x("allocation scheme must be ".
                                                "{sparse} or {preallocated}",
                                                sparse => 'sparse',
                                                preallocated => 'preallocated'),
                                -exitval => 1 });

                }
            },
            "f|config=s"  => \$config_file,
            "n|network=s" => sub {
                my (undef, $value) = @_;

                pod2usage({ -message => __("--network may only be specified ".
                                           "once"),
                            -exitval => 1 }) if (defined($network));
                $network = $value;
            },
            "b|bridge=s"  => sub {
                my (undef, $value) = @_;

                pod2usage({ -message => __("--bridge may only be specified ".
                                           "once"),
                            -exitval => 1 }) if (defined($bridge));
                $bridge = $value;
            },
) or pod2usage(2);

# Set the umask to a reasonable default for virt-v2v
umask(0022);

# Read the config file if one was given
my $config = Sys::VirtV2V::Config->new($config_file);

if (defined($network)) {
    $config->set_default_net_mapping($network, 'network');
} elsif (defined($bridge)) {
    $config->set_default_net_mapping($bridge, 'bridge');
}

my $target;
if ($output_method eq "libvirt") {
    pod2usage({ -message => __("You must specify an output storage pool ".
                               "when using the libvirt output method"),
                -exitval => 1 })
        unless (defined($output_pool));

    $target = new Sys::VirtV2V::Connection::LibVirtTarget($output_uri,
                                                          $output_pool);
}

elsif ($output_method eq "rhev") {
    pod2usage({ -message => __("You must specify an output storage domain ".
                               "when using the rhev output method"),
                -exitval => 1 })
        unless (defined($output_storage_domain));

    $target = new Sys::VirtV2V::Connection::RHEVTarget($output_storage_domain);
}

else {
    die(user_message(__x("{output} is not a valid output method",
                         output => $output_method)));
}

# Get an appropriate Source
my $source;
if ($input_method eq "libvirtxml") {
    my $path = shift(@ARGV) or
        pod2usage({ -message => user_message(__"You must specify a filename"),
                    -exitval => 1 });

    # Warn if we were given more than 1 argument
    if(scalar(@_) > 0) {
        warn user_message
            (__x("WARNING: {modulename} only takes a single filename.",
                 modulename => 'libvirtxml'));
    }

    $source = Sys::VirtV2V::Connection::LibVirtXMLSource->new($path);
}

elsif ($input_method eq "libvirt") {
    my $name = shift(@ARGV) or
        pod2usage({ -message => user_message(__"You must specify a guest"),
                    -exitval => 1 });

    $source = Sys::VirtV2V::Connection::LibVirtSource->new($input_uri, $name);

    # Warn if we were given more than 1 argument
    if(scalar(@_) > 0) {
        warn user_message
            (__x("WARNING: {modulename} only takes a single domain name.",
                 modulename => 'libvirt'));
    }
}

else {
    warn user_message(__x("{input} is not a valid input method",
                                  input => $input_method));
    exit(1);
}


###############################################################################
## Start of processing

# Check that the guest doesn't already exist on the target
die(user_message(__x("Domain {name} already exists on the target.",
                     name => $source->get_name)))
    if ($target->guest_exists($source->get_name()));

# Copy source storage to target
$source->copy_storage($target, $output_format, $output_sparse);

# Get a libvirt configuration for the guest
my $dom = $source->get_dom();
exit(1) unless(defined($dom));

# Get a list of the guest's transfered storage devices
my $storage = $source->get_storage_paths();

# Create the transfer iso if required
my $transferiso;
$transferiso = $config->get_transfer_iso();

# Open a libguestfs handle on the guest's storage devices
my $g = new Sys::VirtV2V::GuestfsHandle($storage, $transferiso,
                                        $output_method eq 'rhev');

my $os;
my $guestcaps;
eval {
    # Inspect the guest
    $os = inspect_guest($g);

    # Modify the guest and its metadata
    $guestcaps = Sys::VirtV2V::Converter->convert($g, $config, $os, $dom,
                                                $source->get_storage_devices());
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

$target->create_guest($os, $dom, $guestcaps);

my ($name) = $dom->findnodes('/domain/name/text()');
$name = $name->getNodeValue();
if($guestcaps->{virtio}) {
    print user_message
        (__x("{name} configured with virtio drivers", name => $name));
} else {
    print user_message
        (__x("{name} configured without virtio drivers", name => $name));
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
    warn user_message(__x("Received signal {sig}. Exiting.", sig => shift));
    exit(1);
}

# Inspect the guest's storage. Returns an OS hashref as returned by
# inspect_in_detail.
sub inspect_guest
{
    my $g = shift;

    # List of possible filesystems.
    my @partitions = get_partitions ($g);

    # Now query each one to build up a picture of what's in it.
    my %fses =
        inspect_all_partitions ($g, \@partitions);

    my $oses = inspect_operating_systems ($g, \%fses);

    # Only work on single-root operating systems.
    my $root_dev;
    my @roots = keys %$oses;

    if(@roots == 0) {
        print STDERR user_message(__"no root device found in this operating ".
                                    "system image");
        exit(1);
    }

    if(@roots > 1) {
        print STDERR user_message(__"multiboot operating systems are not ".
                                    "supported by virt-v2v");
        exit(1);
    }

    $root_dev = $roots[0];

    # Mount up the disks and check for applications.

    my $os = $oses->{$root_dev};
    mount_operating_system ($g, $os, 0);
    inspect_in_detail ($g, $os);

    return $os;
}

=head1 PREPARING TO CONVERT A GUEST

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

Conversion of Windows guests requires that the Guest Tools ISO has been
installed B<before> the guest is converted. This must be done using the ISO
Uploader, which can be found on your RHEV-M system under Start->Red Hat->RHEV
Manager->ISO Uploader.

Failure to do this will result in drivers not being correctly installed in the
guest after conversion.

=head1 CONVERTING A LOCAL XEN GUEST

The following requires that the domain XML is available locally, and that the
storage referred to in the domain XML is available locally at the same paths.

To perform the conversion, run:

 virt-v2v -i libvirtxml -op <pool> [--network <network name>] \
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

 virt-v2v -ic esx://<esx.server>/ -op <pool> [--network <network name>] \
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

To export to RHEV, specify:

 ... -o rhev -osd <export_sd> ...

on the command line in place of I<-op> as in the following examples:

=over

=item Exporting a local Xen guest to RHEV

 virt-v2v -i libvirtxml -o rhev -osd <export_sd> \
          [--network <network name>] <domain>.xml

=item Export a VMWare ESX guest to RHEV

 virt-v2v -ic esx://<esx.server>/ -o rhev -osd <export_sd> \
          [--network <network name>] <domain>

=item Export a local libvirt/KVM guest to RHEV

 virt-v2v -o rhev -osd <export_sd> [--network <network name>] \
          <domain>

=back

=head2 CONVERTING A WINDOWS GUEST

When exporting to RHEV, virt-v2v can additionally convert Windows guests. In
this case, the conversion process is split into 2 stages:

=over

=item 1

Offline conversion.

=item 2

First boot.

=back

The guest will be bootable after the offline conversion stage, but will not yet
have all necessary drivers installed to work correctly in RHEV. These will be
installed automatically the first time the guest boots.

B<N.B.> The first boot stage can take several minutes to run, and must not be
interrupted. It will run automatically without any administrator intervention
other than powering on the guest. To ensure the process is not interrupted, we
strongly recommend that nobody logs in to the machine until it has quiesced the
first time it is booted after conversion. You can check for this in the RHEV
Manager UI.

B<N.B.> Driver installation on first boot requires that the Guest Tools ISO has
been previously uploaded using RHEV Manager's ISO Uploader tool. RHEV will
present this ISO to the guest automatically the first time it is booted.

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

=head1 GUEST DRIVERS

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

Copyright (C) 2009,2010 Red Hat Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
