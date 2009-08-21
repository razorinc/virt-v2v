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
use Data::Dumper;
use XML::Writer;
use Config::Tiny;
use Locale::TextDomain 'virt-v2v';

use Sys::Guestfs;
use Sys::Guestfs::Lib qw(open_guest get_partitions resolve_windows_path
  inspect_all_partitions inspect_partition
  inspect_operating_systems mount_operating_system inspect_in_detail);

use Sys::VirtV2V::MetadataReader;
use Sys::VirtV2V::Storage;
use Sys::VirtV2V::GuestOS;
use Sys::VirtV2V::HVTarget;

=encoding utf8

=head1 NAME

virt-v2v - Convert Xen or VMWare guests to KVM

=head1 SYNOPSIS

 virt-v2v xen_name -o kvm_name

 virt-v2v guest.ovf.zip -o kvm_name

 virt-v2v guest.img [guest.img ...]

=head1 DESCRIPTION

Virt-v2v converts guests from one virtualization hypervisor to
another.  Currently it is limited in what it can convert.  See the
table below.

 -------------------------------+----------------------------
 SOURCE                         | TARGET
 -------------------------------+----------------------------
 Xen domain managed by          |
 libvirt                        |
                                |
 Xen compatibility:             |
   - PV or FV kernel            |  KVM guest managed by
   - with or without PV drivers |  libvirt
   - RHEL 3.9+, 4.8+, 5.3+      |    - with virtio drivers
   - Windows XP, 2003           |
                                |
 -------------------------------+
                                |
 VMWare VMDK image with         |
 OVF metadata, exported from    |
 vSphere                        |
                                |
 VMWare compatibility:          |
   - RHEL 3.9+, 4.8+, 5.3+      |
   - VMWare tools               |
                                |
 -------------------------------+----------------------------

=head2 CONVERTING XEN DOMAINS

For Xen domains managed by libvirt, perform the initial conversion
using:

 virt-v2v xen_name -o kvm_name

where C<xen_name> is the libvirt Xen domain name, and C<kvm_name> is
the (new) name for the converted KVM guest.

Then test boot the new guest in KVM:

 virsh start kvm_name
 virt-viewer kvm_name

When you have verified that this works, shut down the new KVM domain
and I<commit> the changes by doing:

 virt-v2v --commit kvm_name

I<This command will destroy the original Xen domain>.

Or you can I<rollback> to the original Xen domain by doing:

 virt-v2v --rollback kvm_name

B<Very important note:> Do I<not> try to run both the original Xen
domain and the KVM domain at the same time!  This will cause guest
corruption.

=head2 CONVERTING VMWARE GUESTS

I<This section to be written>





=head1 OPTIONS

=over 4

=cut

my $help;

=item B<--help>

Display brief help.

=cut

my $version;

=item B<--version>

Display version number and exit.

=cut

my $uri;

=item B<--connect URI> | B<-c URI>

Connect to libvirt using the given I<URI>. If omitted, then we connect to the
default libvirt hypervisor.

=cut

my $output;

=item B<--output name> | B<-o name>

Set the output guest name.

=cut

=back

=cut

# Option defaults
my $format_opt = "libvirtxml"; # Metadata format
my $storage_opt = "qcow2"; # storage modifier

# The path to the virt-v2v config file
my $config_file;

GetOptions ("help|?"      => \$help,
            "version"     => \$version,
            "connect|c=s" => \$uri,
            "format|f=s"  => \$format_opt,
            "storage|s=s" => \$storage_opt,
            "config|c=s"  => \$config_file
    ) or pod2usage (2);
pod2usage (1) if $help;
if ($version) {
    my $g = Sys::Guestfs->new ();
    my %h = $g->version ();
    print "$h{major}.$h{minor}.$h{release}$h{extra}\n";
    exit
}
pod2usage (__"virt-v2v: no image or VM names given") if @ARGV == 0;

# Read the config file if one was given
my $config = {};
if(defined($config_file)) {
    $config = Config::Tiny->read($config_file);

    # Check we were able to read it
    if(!defined($config)) {
        print STDERR Config::Tiny->errstr."\n";
        exit(1);
    }
}

# Get an appropriate MetadataReader
my $mdr = Sys::VirtV2V::MetadataReader->instantiate($format_opt, $config);
if(!defined($mdr)) {
    print STDERR __x("virt-v2v: {format} is not a valid metadata format",
                     format => $format_opt)."\n";
    exit(1);
}

my $storage = Sys::VirtV2V::Storage->instantiate($storage_opt, $config);
if(!defined($storage)) {
    print STDERR __x("{virt-v2v: storage} is not a valid storage option\n",
                     storage => $storage)."\n";
    exit(1);
}

$mdr->handle_arguments(@ARGV);

# Check all modules are properly initialised
my $ready = 1;
foreach my $module ($mdr, $storage) {
    $ready = 0 if(!$module->is_configured());
}
exit 1 if(!$ready);

# Configure GuestOS ([files] and [deps] sections)
Sys::VirtV2V::GuestOS->configure($config);

# Connect to libvirt
my @vmm_params = (auth => 1);
push(@vmm_params, uri => $uri) if(defined($uri));
my $vmm = Sys::Virt->new(@vmm_params);

###############################################################################
## Start of processing

# Get a libvirt configuration for the guest
my $dom = $mdr->get_dom();
exit(1) if(!defined($dom));

# Modify the storage in the guest according to configured options
$storage->update_guest($dom);

# Get a list of the guest's storage devices
my @devices = get_guest_devices($dom);

# Open a libguestfs handle on the guest's devices
my $g = get_guestfs_handle(@devices);

# Inspect the guest
my $os = inspect_guest($g);

# Instantiate a GuestOS instance to manipulate the guest
my $guestos = Sys::VirtV2V::GuestOS->instantiate($g, $os);

# Modify the guest and its metadata for the target hypervisor
Sys::VirtV2V::HVTarget->configure($vmm, $guestos, $dom, $os);

$g->umount_all();
$g->sync();

$vmm->define_domain($dom->toString());


###############################################################################
## Helper functions

sub get_guestfs_handle
{
    my @params = \@_; # Initialise parameters with list of devices

    my $g = open_guest(@params, rw => 1);

    # Mount the transfer iso if GuestOS needs it
    my $transferiso = Sys::VirtV2V::GuestOS->get_transfer_iso();
    $g->add_cdrom($transferiso) if(defined($transferiso));

    # Enable selinux in the guest
    $g->set_selinux(1);

    $g->launch ();
    $g->wait_ready ();

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
    die __"no root device found in this operating system image" if @roots == 0;
    die __"multiboot operating systems are not supported by v2v" if @roots > 1;
    $root_dev = $roots[0];

    # Mount up the disks and check for applications.

    my $os = $oses->{$root_dev};
    mount_operating_system ($g, $os, 0);
    inspect_in_detail ($g, $os);

    return $os;
}

sub get_guest_devices
{
    my $dom = shift;

    my @devices;
    foreach my $source ($dom->findnodes('/domain/devices/disk/source')) {
        my $attrs = $source->getAttributes();

        # Get either dev or file, whichever is defined
        my $attr = $attrs->getNamedItem("dev");
        $attr = $attrs->getNamedItem("file") if(!defined($attr));

        defined($attr) or die("source element has neither dev nor file: ".
                              $source.toString());

        push(@devices, $attr->getValue());
    }

    return @devices;
}

=head1 SEE ALSO

L<virt-inspector(1)>,
L<guestfs(3)>,
L<guestfish(1)>,
L<Sys::Guestfs(3)>,
L<Sys::Guestfs::Lib(3)>,
L<Sys::Virt(3)>,
L<http://libguestfs.org/>.

For Windows registry parsing we require the C<reged> program
from L<http://home.eunet.no/~pnordahl/ntpasswd/>.

=head1 AUTHOR

Richard W.M. Jones L<http://et.redhat.com/~rjones/>

Matthew Booth L<mbooth@redhat.com>

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
