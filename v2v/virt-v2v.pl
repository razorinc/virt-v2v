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
#use Data::Dumper;
use File::Spec;
use File::stat;

use Locale::TextDomain 'virt-v2v';

use Sys::Guestfs;
use Sys::Guestfs::Lib qw(open_guest get_partitions inspect_all_partitions
                         inspect_operating_systems mount_operating_system
                         inspect_in_detail);

use Sys::VirtV2V;
use Sys::VirtV2V::Converter;
use Sys::VirtV2V::Connection::LibVirt;
use Sys::VirtV2V::Connection::LibVirtXML;
use Sys::VirtV2V::ExecHelper;
use Sys::VirtV2V::GuestOS;
use Sys::VirtV2V::UserMessage qw(user_message);

=encoding utf8

=head1 NAME

virt-v2v - Convert a guest to use KVM

=head1 SYNOPSIS

 virt-v2v -f virt-v2v.conf -i libvirtxml guest-domain.xml

 virt-v2v -f virt-v2v.conf -ic esx://esx.server/ -op transfer guest-domain

=head1 DESCRIPTION

virt-v2v converts guests from a foreign hypervisor to run on KVM, managed by
libvirt. It can currently convert Red Hat Enterprise Linux and Fedora guests
running on Xen and VMware ESX. It will enable VirtIO drivers in the converted
guest if possible.

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

=cut

my $input_transport;

=item B<-it method>

Species the transport method used to obtain raw storage from the source guest.
This is currently only a placeholder, and does nothing.

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

my $config_file;

=item B<-f file> | B<--config file>

Load the virt-v2v configuration from I<file>. There is no default.

=item B<--help>

Display brief help.

=item B<--version>

Display version number and exit.

=back

=cut

# Initialise the message output prefix
Sys::VirtV2V::UserMessage->set_identifier('virt-v2v');

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
            "oc=s"        => \$output_uri,
            "op=s"        => \$output_pool,
            "f|config=s"  => \$config_file
) or pod2usage(2);

# Read the config file if one was given
my $config;
if(defined($config_file)) {
    # Check we can access the config file
    die(user_message(__x("Config file {path} doesn't exist",
                         path => $config_file))) unless (-e $config_file);

    die(user_message(__x("Don't have permissions to read {path}",
                         path => $config_file))) unless (-r $config_file);

    eval {
        $config = new XML::DOM::Parser->parsefile($config_file);
    };

    die(user_message(__x("Unable to parse config file {path}: {error}",
                         path => $config_file, error => $@))) if ($@);
}

# Connect to target libvirt
my $vmm = Sys::Virt->new(
    auth => 1,
    uri => $output_uri
);

# Get an appropriate Connection
my $conn;
eval {
    if ($input_method eq "libvirtxml") {
        my $path = shift(@ARGV) or
            pod2usage({ -message => user_message(__"You must specify a filename"),
                        -exitval => 1 });

        # Warn if we were given more than 1 argument
        if(scalar(@_) > 0) {
            print STDERR user_message
                (__x("WARNING: {modulename} only takes a single filename.",
                     modulename => 'libvirtxml'));
        }

        $conn = Sys::VirtV2V::Connection::LibVirtXML->new($path);
    }

    elsif ($input_method eq "libvirt") {
        my $name = shift(@ARGV) or
            pod2usage({ -message => user_message(__"You must specify a guest"),
                        -exitval => 1 });

        # Get a handle to the output pool if one is defined
        my $pool;
        if (defined($output_pool)) {
            eval {
                $pool = $vmm->get_storage_pool_by_name($output_pool);
            };

            if ($@) {
                print STDERR user_message
                    (__x("Output pool {poolname} is not a valid local ".
                         "storage pool",
                         poolname => $output_pool));
                exit(1);
            }
        }

        $conn = Sys::VirtV2V::Connection::LibVirt->new($input_uri, $name,
                                                       $pool);

        # Warn if we were given more than 1 argument
        if(scalar(@_) > 0) {
            print STDERR user_message
                (__x("WARNING: {modulename} only takes a single domain name.",
                     modulename => 'libvirt'));
        }
    }

    else {
        print STDERR user_message __x("{input} is not a valid input method",
                                      input => $input_method);
        exit(1);
    }
};
if ($@) {
    print STDERR $@;
    exit(1);
}


###############################################################################
## Start of processing

# Get a libvirt configuration for the guest
my $dom = $conn->get_dom();
exit(1) unless(defined($dom));

# Get a list of the guest's transfered storage devices
my @storage = $conn->get_local_storage();

# Create the transfer iso if required
my $transferiso = get_transfer_iso($config, $config_file);

# Open a libguestfs handle on the guest's storage devices
my $g = get_guestfs_handle(\@storage, $transferiso);

$SIG{'INT'} = \&close_guest_handle;
$SIG{'QUIT'} = \&close_guest_handle;

# Inspect the guest
my $os = inspect_guest($g);

# Instantiate a GuestOS instance to manipulate the guest
my $guestos = Sys::VirtV2V::GuestOS->new($g, $os, $dom, $config);

# Modify the guest and its metadata for the target hypervisor
Sys::VirtV2V::Converter->convert($vmm, $guestos, $config, $dom, $os);

$vmm->define_domain($dom->toString());

exit(0);

# We should always attempt to shut down the guest gracefully
END {
    close_guest_handle();
}

###############################################################################
## Helper functions

sub close_guest_handle
{
    if (defined($g)) {
        $g->umount_all();
        $g->sync();
    }
}

sub get_transfer_iso
{
    my ($config, $config_file) = @_;

    # Nothing to do if there's no config
    return undef unless (defined($config));

    # path-root doesn't have to be defined
    my ($root) = $config->findnodes('/virt-v2v/path-root/text()');
    $root = $root->getData() if (defined($root));

    # Construct a list of path arguments to mkisofs from paths referenced in the
    # config file
    # We actually use a hash here to avoid duplicates
    my %path_args;
    foreach my $path ($config->findnodes('/virt-v2v/app/path/text() | '.
                                         '/virt-v2v/app/dep/text()')) {
        $path = $path->getData();

        # Get the absolute path if iso-root was defined
        my $abs;
        if (defined($root)) {
            $abs = File::Spec->catfile($root, $path);
        } else {
            $abs = $path;
        }

        # Check the referenced path is accessible
        die(user_message(__x("Unable to access {path} referenced in ".
                             "the config file",
                             path => $path))) unless (-r $abs);

        $path_args{"$path=$abs"} = 1;
    }

    # Nothing further to do if there are no paths
    return if (keys(%path_args) == 0);

    # Get the path of the transfer iso
    my ($iso_path) = $config->findnodes('/virt-v2v/iso-path/text()');

    # We need this
    die(user_message(__"<iso-path> must be specified in the configuration ".
                       "file")) unless (defined($iso_path));
    $iso_path = $iso_path->getData();

    # Check that the transfer iso exists, and is newer than the config file
    if (-e $iso_path) {
        my $iso_st = stat($iso_path)
            or die(user_message(__x("Unable to stat iso file {path}: {error}",
                                    path => $iso_path, error => $!)));

        my $config_st = stat($config_file)
            or die(user_message(__x("Unable to stat config file {path}: ".
                                    "{error}",
                                    path => $config_file, error => $!)));

        # Don't need to re-create if the iso file is newer than the config file
        return $iso_path if ($iso_st->mtime > $config_st->mtime);
    }

    # Re-create the transfer iso
    my $eh = Sys::VirtV2V::ExecHelper->run
        ('mkisofs', '-o', $iso_path,
         '-r', '-J',
         '-V', '__virt-v2v_transfer__',
         '-graft-points', keys(%path_args));
    die(user_message(__x("Failed to create transfer iso. ".
                         "Command output was:\n{output}",
                         output => $eh->output()))) unless ($eh->status() == 0);

    return $iso_path;
}

sub get_guestfs_handle
{
    my ($storage, $transferiso) = @_;
    my $interface = "ide";

    my $g = open_guest($storage, rw => 1, interface => $interface);

    # Add the transfer iso if there is one
    $g->add_drive_ro_with_if($transferiso, $interface)
        if(defined($transferiso));

    # Enable selinux in the guest
    $g->set_selinux(1);

    # Enable autosync to defend against data corruption on unclean shutdown
    $g->set_autosync(1);

    $g->launch ();

    return $g;
}

# Inspect the guest's storage. Returns an OS hashref as returned by
# inspect_in_detail.
sub inspect_guest
{
    my $g = shift;

    my $use_windows_registry;

    # List of possible filesystems.
    my @partitions = get_partitions ($g);

    # Now query each one to build up a picture of what's in it.
    my %fses =
        inspect_all_partitions ($g, \@partitions,
                                use_windows_registry => $use_windows_registry);

    #print "fses -----------\n";
    #print Dumper(\%fses);

    my $oses = inspect_operating_systems ($g, \%fses);

    #print "oses -----------\n";
    #print Dumper($oses);

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

=head2 Xen guests

The following steps are required before converting a Xen guest. Note that only
local Xen guests are currently supported. These steps are not required for
conversions from ESX, and will not be required for remote Xen guests when we
support that.

=head3 Backup the guest

If converting a local guest using the libvirtxml input method, the guest will be
converted in place: it will make changes to a guest directly without creating a
backup. It is recommended that virt-v2v be run against a copy.

The L<v2v-snapshot(1)> tool can be used to convert a guest to use a snapshot
for storage prior to running virt-v2v against it. This snapshot can then be
committed to the original storage after the conversion is confirmed as
successful.

=head3 Obtain domain XML for the guest domain

virt-v2v uses a libvirt domain description to determine the current
configuration of the guest, including the location of its storage. This should
be obtained from the host running the guest pre-conversion by running:

 virsh dumpxml <domain> > <domain>.xml

This will require a reboot if the host running Xen is the same host that will
run KVM. This is because libvirt needs to connect to a running xen hypervisor to
obtain its metadata.

=head2 ESX guests

=head3 Create a local storage pool for transferred storage

virt-v2v copies the guest storage to the local machine during import from an ESX
server. It creates new storage in a locally defined libvirt pool. This pool can
be defined using any libvirt tool, and can be of any type.

The simplest way to create a new pool is with L<virt-manager(1)>. Pools can be
defined from the Storage tab under Host Details.

=head2 All guests

=head3 Create local network interfaces

The local machine must have an appropriate network for the converted guest
to connect to. This is likely to be a bridge interface. A bridge interface can
be created using standard tools on the host.

Since version 0.8.3, L<virt-manager(1)> can also create and manage bridges.

=head1 CONVERTING A LOCAL XEN GUEST

The following requires that the domain XML is available locally, and that the
storage referred to in the domain XML is available locally at the same paths.

To perform the conversion, run:

 virt-v2v -f virt-v2v.conf -i libvirtxml <domain>.xml

where C<< <domain>.xml >> is the path to the exported guest domain's xml. virt-v2v.conf should specify:

=over

=item *

a mapping for the guest's network configuration.

=item *

app definitions for any required replacement kernels.

=back

See L<virt-v2v.conf(5)> for details.

It is possible to avoid specifying replacement kernels in the virt-v2v config
file by ensuring that the guest has an appropriate kernel installed prior to
conversion. If your guest uses a Xen paravirtualised kernel (it would be called
something like kernel-xen or kernel-xenU), you can install a regular kernel,
which won't reference a hypervisor in its name, alongside it. You shouldn't
make this newly installed kernel your default kernel because the chances are Xen
will not boot it. virt-v2v will make it the default during conversion.

=head2 CONVERTING A GUEST FROM VMWARE ESX

virt-v2v can convert a guest from VMware ESX, including transferring its
storage.

B<N.B.> virt-v2v does not transfer snapshots from ESX. Only the latest flat
storage is transferred.

The guest MUST be shut down in ESX before conversion starts. virt-v2v will not
proceed if the guest is still running. To convert the guest, run:

 virt-v2v -f virt-v2v.conf -ic esx://<esx.server>/ -op <pool> <domain>

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

virt-v2v.conf should specify a mapping for the guest's network configuration.
See L<virt-v2v.conf(5)> for details.

=head3 Authenticating to the ESX server

Connecting to the ESX server will require authentication. virt-v2v supports
password authentication when connecting to ESX. It reads passwords from
$HOME/.netrc. The format of this file is described in L<netrc(5)>. An example
entry is:

 machine esx01.example.com login root password s3cr3t

=head3 Connecting to an ESX server with an invalid certificate

In non-production environments, the ESX server may have a non-valid certificate,
for example a self-signed certificate. In this case, certificate checking can be
explicitly disabled by adding '?no_verify=1' to the connection URI as shown
below:

 ... -ic esx://<esx.server>/?no_verify=1 ...

=head1 RUNNING THE CONVERTED GUEST

On successful completion, virt-v2v will create a new libvirt domain for the
converted guest with the same name as the original guest. It can be started as
usual using libvirt tools, for example L<virt-manager(1)>.

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
 Block          sym53c8xx (scsi)
 Network        e1000

=head1 SEE ALSO

L<virt-manager(1)>,
L<v2v-snapshot(1)>,
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
