# Sys::Guestfs::HVTarget::Linux
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

package Sys::Guestfs::HVTarget::Linux;

use strict;
use warnings;

use Data::Dumper;
use Locale::TextDomain 'libguestfs';

use XML::DOM;
use XML::DOM::XPath;

use Sys::Guestfs::HVSource;

use Carp;

=pod

=head1 NAME

Sys::Guestfs::HVTarget::Linux - Configure a Linux guest for a target hypervisor

=head1 SYNOPSIS

 use Sys::Guestfs::HVTarget;

=head1 DESCRIPTION

=head1 METHODS

=cut

sub can_handle
{
    my $class = shift;

    my $desc = shift;
    carp("can_handle called without desc argument") unless defined($desc);

    return ($desc->{os} eq 'linux');
}

sub configure
{
    my $class = shift;

    my ($vmm, $guestos, $dom, $desc) = @_;
    carp("configure called without vmm argument") unless defined($vmm);
    carp("configure called without guestos argument") unless defined($guestos);
    carp("configure called without dom argument") unless defined($dom);
    carp("configure called without desc argument") unless defined($desc);

    _remap_block_devices($guestos, $dom, $desc);
    _configure_drivers($guestos, $desc);
    _configure_applications($guestos, $desc);
    _configure_kernels($guestos, $desc);
    _configure_metadata($vmm, $dom, $desc);
}

sub _remap_block_devices
{
    my ($guestos, $dom, $desc) = @_;

    my %map = ();

    # Look for devices specified in the device metadata
    foreach my $dev ($dom->findnodes('/domain/devices/disk/target/@dev')) {
        if($dev->getNodeValue() =~ m{^(sd|hd|xvd)([a-z]+)\d*$}) {
            $map{"$1$2"} = "vd$2";

            # A guest might present an IDE disk as SCSI
            if($1 eq 'hd') {
                $map{"sd$2"} = "vd$2";
            }

            $dev->setNodeValue("vd$2");
        }
    }

    $guestos->remap_block_devices(\%map);
}

sub _configure_drivers
{
    my ($guestos, $desc) = @_;
    die("configure_drivers called without guestos argument")
        unless defined($guestos);
    die("configure_drivers called without desc argument")
        unless defined($desc);

    # Get a list of all old-hypervisor specific drivers which need to be
    # replaced or removed
    my %hvs_drivers;
    foreach my $driver (Sys::Guestfs::HVSource->find_drivers($guestos)) {
        $hvs_drivers{$driver} = undef;
    }

    # Go through all drivers looking for network or scsi devices
    my $drivers = $desc->{modprobe_aliases};

    foreach my $driver (keys(%$drivers)) {
        # Replace network drivers with virtio_net
        if($driver =~ /^eth\d+$/) {
            # Make a note that we updated an old-HV specific driver
            if(exists($hvs_drivers{$driver})) {
                $hvs_drivers{$driver} = "virtio_net";
            }

            $guestos->update_driver($driver, "virtio_net");

            print STDERR __x("Replaced {driver} driver with virtio_net\n",
                      driver => $driver);
        }

        # Replace block drivers with virtio_blk
        if($driver =~ /^scsi_hostadapter/) {
            # Make a note that we updated an old-HV specific driver
            if(exists($hvs_drivers{$driver})) {
                $hvs_drivers{$driver} = "virtio_blk";
            }

            $guestos->update_driver($driver, "virtio_blk");

            print STDERR __x("Replaced {driver} driver with virtio_blk\n",
                      driver => $driver);
        }
    }

    # Warn if any old-HV specific drivers weren't updated
    foreach my $driver (keys(%hvs_drivers)) {
        if(!defined($hvs_drivers{$driver})) {
            print STDERR __x("WARNING: Don't know how to update {driver}, ".
                             "which loads the {module} module.\n",
                             driver => $driver,
                             module => $drivers->{$driver}->{modulename});
        }
    }
}

sub _configure_applications
{
    my ($guestos, $desc) = @_;
    die("configure_applications called without guestos argument")
        unless defined($guestos);
    die("configure_applications called without desc argument")
        unless defined($desc);

    my @hvs_apps = Sys::Guestfs::HVSource->find_applications($guestos);
}

sub _configure_kernels
{
    my ($guestos, $desc) = @_;
    die("configure_kernels called without guestos argument")
        unless defined($guestos);
    die("configure_kernels called without desc argument")
        unless defined($desc);

    my %kernels;

    # Look for installed kernels with virtio support
    foreach my $kernel (@{$desc->{kernels}}) {
        my %checklist = (
            "virtio_blk" => undef,
            "virtio_pci" => undef,
            "virtio_net" => undef
        );

        foreach my $driver ($kernel->{modules}) {
            if(exists($checklist{$driver})) {
                $checklist{$driver} = 1;
            }
        }

        my $virtio = 1;
        foreach my $driver (keys(%checklist)) {
            if(!defined($checklist{$driver})) {
                $virtio = 0;
                last;
            }
        }

        if($virtio) {
            $kernels{$kernel->{version}} = 1;
        } else {
            $kernels{$kernel->{version}} = 0;
        }
    }

    # Remove old-HV kernels
    foreach my $kernel (Sys::Guestfs::HVSource->find_kernels($guestos)) {
        # Remove the kernel from our cache
        delete($kernels{$kernel});

        # Uninstall the kernel from the guest
        $guestos->remove_kernel($kernel);
    }

    # Find the highest versioned, virtio capable, installed kernel
    my $boot_kernel;
    foreach my $kernel (sort {$b cmp $a} (keys(%kernels))) {
        if($kernels{$kernel}) {
            if($kernels{$kernel}) {
                $boot_kernel = $kernel;
                last;
            }
        }
    }

    # If none of the installed kernels are appropriate, install a new one
    if(!defined($boot_kernel)) {
        $boot_kernel = $guestos->add_kernel();
    }

    $guestos->prepare_bootable($boot_kernel,
                               "virtio_pci", "virtio_blk", "virtio_net");
}

sub _configure_metadata
{
    my ($vmm, $dom, $desc) = @_;

    die("configure_metadata called without vmm argument")
        unless defined($vmm);
    die("configure_metadata called without dom argument")
        unless defined($dom);
    die("configure_metadata called without desc argument")
        unless defined($desc);

    # Replace source hypervisor metadata with KVM defaults
    _unconfigure_hvs($dom);

    # Configure guest according to local hypervisor's capabilities
    _configure_capabilities($dom, $vmm);

    # Remove any configuration related to a PV kernel bootloader
    _unconfigure_bootloaders($dom);

    # Configure virtio drivers
    _configure_virtio($dom);
}

# Default values for a KVM configuration
use constant KVM_XML => "
<domain type='kvm'>
  <os>
    <type machine='pc'>hvm</type>
    <boot dev='hd'/>
  </os>
  <devices>
    <disk device='disk'>
      <target bus='virtio'/>
    </disk>
    <interface type='network'>
      <model type='virtio'/>
    </interface>
    <input type='mouse' bus='ps2'/>
    <graphics type='vnc' port='-1' listen='127.0.0.1'/>
  </devices>
</domain>
";

sub _unconfigure_hvs
{
    my ($dom) = @_;

    # Parse the defaults
    my $parser = new XML::DOM::Parser;
    my $default_dom = $parser->parse(KVM_XML);

    # Get a list of source HV specific metadata nodes
    my @nodeinfo = Sys::Guestfs::HVSource->find_metadata($dom);

    for(my $i = 0; $i < $#nodeinfo; $i += 2) {
        my $node = $nodeinfo[$i];
        my $xpath = $nodeinfo[$i + 1]->[0];
        my $required = $nodeinfo[$i + 1]->[1];

        # Look for a replacement in the defaults
        my ($default) = $default_dom->findnodes($xpath);
        if(defined($default)) {
            if($node->isa('XML::DOM::Attr')) {
                $node->setNodeValue($default->getNodeValue());
            } else {
                my $replacement = $default->cloneNode(1);
                $replacement->setOwnerDocument($dom);

                $node->getParentNode()->replaceChild($replacement, $node);
            }
        }

        else {
            # Warn if a replacement is required, but none was found
            print STDERR __x("WARNING: No replacement found for {xpath} in ".
                             "domain XML. The node was removed.",
                             xpath => $xpath) if($required);

            $node->getParentNode()->removeChild($node);
        }
    }
}

sub _configure_capabilities
{
    my ($dom, $vmm) = @_;

    # Parse the capabilities of the connected libvirt
    my $caps = new XML::DOM::Parser->parse($vmm->get_capabilities());

    my @kernel_archs = ('i586', 'i686'); # XXX: Get from inspection!
    my $arch;

    # Find a guest capability description for the current guest architecture
    my $guestcap;
    foreach $arch (@kernel_archs) {
        ($guestcap) = $caps->findnodes
            ("/capabilities/guest[arch[\@name='$arch']/domain/\@type='kvm']");

        last if(defined($guestcap));
    }
    # Note that this leaves $arch set to the specific discovered arch

    die(__x("The connected hypervisor does not support a {arch} kvm guest",
        arch => $kernel_archs[0])) unless(defined($guestcap));

    # Ensure that /domain/@type = 'kvm'
    my ($type) = $dom->findnodes('/domain/@type');
    $type->setNodeValue('kvm');

    # Set /domain/os/type to the value taken from capabilities
    my ($os_type) = $dom->findnodes('/domain/os/type/text()');
    if(defined($os_type)) {
        my ($caps_os_type) = $guestcap->findnodes('os_type/text()');
        $os_type->setNodeValue($caps_os_type->getNodeValue());
    }

    # Check that /domain/os/type/@machine, if set, is listed in capabilities
    my ($machine) = $dom->findnodes('/domain/os/type/@machine');
    if(defined($machine)) {
        my @machine_caps = $guestcap->findnodes
            ("arch[\@name='$arch']/machine/text()");
        
        my $found = 0;
        foreach my $machine_cap (@machine_caps) {
            if($machine eq $machine_cap) {
                $found = 1;
                last;
            }
        }

        # If the machine isn't listed as a capability, warn and remove it
        if(!$found) {
            print STDERR __x("The connected hypervisor does not support a ".
                             "machine type of {machine}.",
                             machine => $machine)."\n";
            
            my ($type) = $dom->findnodes('/domain/os/type');
            $type->getAtributes()->removeNamedItem('machine');
        }
    }

    # Check that /domain/features are listed in capabilities
    # Get a list of supported features
    my %features;
    foreach my $feature ($guestcap->findnodes('features/*')) {
        $features{$feature->getNodeName()} = 1;
    }

    foreach my $feature ($dom->findnodes('/domain/features/*')) {
        if(!exists($features{$feature->getNodeName()})) {
            print STDERR __x("The connected hypervisor does not support ".
                             "feature {feature}", feature => $feature)."\n";
            $feature->getParentNode()->removeChild($feature);
        }
    }
}

sub _unconfigure_bootloaders
{
    my ($dom) = @_;

    # A list of paths which relate to assisted booting of a kernel on hvm
    my @bootloader_paths = (
        '/domain/os/loader',
        '/domain/os/kernel',
        '/domain/os/initrd',
        '/domain/os/root',
        '/domain/os/cmdline'
    );

    foreach my $path (@bootloader_paths) {
        my ($node) = $dom->findnodes($path);
        $node->getParentNode()->removeChild($node) if defined($node);
    }
}

sub _configure_virtio
{
    my ($dom) = @_;

    # Convert disks
    # N.B. <disk> is required to have a <target> element

    # Convert alternate bus specifications
    foreach my $bus ($dom->findnodes('/domain/devices/disk/target/@bus')) {
        $bus->setNodeValue('virtio');
    }

    # Add an explicit bus specification to targets without one
    foreach my $target
        ($dom->findnodes('/domain/devices/disk/target[not(@bus)]'))
    {
        $target->setAttribute('bus', 'virtio');
    }

    # Convert network adapters
    # N.B. <interface> is not required to have a <model> element, but <model>
    # is required to have a type attribute

    # Convert interfaces which already have a model element
    foreach my $type
        ($dom->findnodes('/domain/devices/interface/model/@type'))
    {
        $type->setNodeValue('virtio');
    }

    # Add a model element to interfaces which don't have one
    foreach my $interface
        ($dom->findnodes('/domain/devices/interface[not(model)]'))
    {
        my $model = $dom->createElement('model');
        $model->setAttribute('type', 'virtio');
        $interface->appendChild($model);
    }
}

1;

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-inspector(1)>,
L<Sys::Guestfs(3)>,
L<guestfs(3)>,
L<http://libguestfs.org/>,
L<Sys::Virt(3)>,
L<http://libvirt.org/>,
L<guestfish(1)>.

=cut
