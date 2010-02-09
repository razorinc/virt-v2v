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
use Config::Tiny;
use Locale::TextDomain 'virt-v2v';

use Sys::Guestfs;
use Sys::Guestfs::Lib qw(open_guest get_partitions inspect_all_partitions
                         inspect_operating_systems mount_operating_system
                         inspect_in_detail);

use Sys::VirtV2V;
use Sys::VirtV2V::GuestOS;
use Sys::VirtV2V::Converter;
use Sys::VirtV2V::Connection::LibVirt;
use Sys::VirtV2V::Connection::LibVirtXML;
use Sys::VirtV2V::UserMessage qw(user_message);

=encoding utf8

=head1 NAME

virt-v2v - Convert a guest to use KVM

=head1 SYNOPSIS

 virt-v2v guest-domain.xml

 virt-v2v -f virt-v2v.conf guest-domain.xml

 virt-v2v -ic qemu:///system guest-domain

=head1 DESCRIPTION

Virt-v2v converts guests from one virtualization hypervisor to
another.  Currently it is limited in what it can convert.  See the
table below.

=begin html

<table border="1" style="border-collapse: collapse">
    <tr>
        <th>Source</th>
        <th>Target</th>
    </tr>

    <tr>
        <td valign="top" style="padding: 0 0.5em 0 0.5em">
            <p>Xen domain managed by libvirt</p>
            <p>Guest
                <ul>
                    <li>PV or FV Kernel</li>
                    <li>with or without PV drivers</li>
                    <li>RHEL 3.x, 4.x, 5.x</li>
                </ul>
            </p>
        </td>
        <td valign="top" style="padding: 0 0.5em 0 0.5em">
            <p>KVM domain managed by libvirt</p>
            <p>Guest
                <ul>
                    <li>with virtio drivers if support by guest</li>
                </ul>
            </p>
        </td>
    </tr>
</table>

=end html

=begin :man

 -------------------------------+----------------------------
 SOURCE                         | TARGET
 -------------------------------+----------------------------
 Xen domain managed by          | KVM domain managed by
 libvirt                        | libvirt
                                |
 Guest:                         | Guest:
   - PV or FV kernel            |   - with virtio drivers
   - with or without PV drivers |     if supported by guest
   - RHEL 3.x, 4.x, 5.x         |
                                |
                                |
 -------------------------------+----------------------------

=end :man

=head1 OPTIONS

=over 4

=cut

my $input_method = "libvirt";

=item B<-i input>

Specifies how the conversion source metadata can be obtained. The default is
C<libvirt>.  Supported options are:

=over

=item I<libvirt>

Guest argument is the name of a libvirt domain.

=item I<libvirtxml>

Guest argument is the path to an XML file describing a libvirt domain.

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

=cut

my $output_uri = "qemu:///system";

=item B<-oc URI>

Specifies the libvirt connection to use to create the converted guest. If
ommitted, this defaults to qemu:///system.

=cut

my $output_pool;

=item B<-op pool>

Specifies the pool which will be used to create new storage for the converted
guest.

=cut

my $config_file;

=item B<-f file>

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

# Configure GuestOS ([files] and [deps] sections)
# Need to fix GuestOS's usage of config for installing applications
Sys::VirtV2V::GuestOS->configure({});


###############################################################################
## Start of processing

# Get a libvirt configuration for the guest
my $dom = $conn->get_dom();
exit(1) unless(defined($dom));

# Get a list of the guest's transfered storage devices
my @storage = $conn->get_local_storage();

# Open a libguestfs handle on the guest's storage devices
my $g = get_guestfs_handle(\@storage, $transferiso);

$SIG{'INT'} = \&close_guest_handle;
$SIG{'QUIT'} = \&close_guest_handle;

# Inspect the guest
my $os = inspect_guest($g);

# Instantiate a GuestOS instance to manipulate the guest
my $guestos = Sys::VirtV2V::GuestOS->instantiate($g, $os);

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

sub get_guestfs_handle
{
    my $g = open_guest(\@_, rw => 1);

    # Add the transfer iso if there is one
    $g->add_drive($transferiso) if(defined($transferiso));

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

=head1 PREPARING TO RUN VIRT-V2V

=head2 Backup the guest

Virt-v2v converts guests 'in-place': it will make changes to a guest directly
without creating a backup. It is recommended that virt-v2v be run against a
copy.

The L<v2v-snapshot(1)> tool can be used to convert a guest to use a snapshot
for storage prior to running virt-v2v against it. This snapshot can then be
committed to the original storage after the conversion is confirmed as
successful.

The L<virt-clone(1)> tool can make a complete copy of a guest, including all its
storage.

=head2 Obtain domain XML for the guest domain

Virt-v2v uses a libvirt domain description to determine the current
configuration of the guest, including the location of its storage. This should
be obtained from the host running the guest pre-conversion by running:

 virsh dumpxml <domain> > <domain>.xml

=head1 CONVERTING A GUEST

In the simplest case, virt-v2v can be run as follows:

 virt-v2v <domain>.xml

where C<< <domain>.xml >> is the path to the exported guest domain's xml. This
is the simplest form of conversion. It can only be used when the guest has an
installed kernel which will boot on KVM, i.e. a guest with only paravirtualised
Xen kernels installed will not work. Virtio will be configured if it is
supported, otherwise the guest will be configured to use non-virtio drivers. See
L</GUEST DRIVERS> for details of which drivers will be used.

Virt-v2v can also be configured to install new software into a guest. This might
be necessary if the guest will not boot on KVM without modification, or if you
want to upgrade it to support virtio during conversion. Doing this requires
specifying a configuration file describing where to find the new software. In
this case, virt-v2v is called as:

 virt-v2v -s <virt-v2v.conf> <domain>.xml

See L<virt-v2v.conf(5)> for details of this configuration file. During the
conversion process, if virt-v2v does not detect that the guest is capable of
supporting virtio it will try to upgrade components to resolve this.  On Linux
guests this will involve upgrading the kernel, and may involve upgrading
dependent parts of userspace.

To text boot the new guest in KVM, run:

 virsh start <domain>
 virt-viewer <domain>

If you have created a guest snapshot using L<v2v-snapshot(1)>, it can be
committed or rolled back at this stage.

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

Virt-v2v will install the following drivers in a Linux guest:

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

L<v2v-snapshot(1)>
L<http://libguestfs.org/>.

For Windows registry parsing we require the C<reged> program
from L<http://home.eunet.no/~pnordahl/ntpasswd/>.

=head1 AUTHOR

Richard W.M. Jones L<http://et.redhat.com/~rjones/>

Matthew Booth <mbooth@redhat.com>

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

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
