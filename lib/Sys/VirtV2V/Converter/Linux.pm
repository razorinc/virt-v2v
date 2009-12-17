# Sys::VirtV2V::Converter::Linux
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

package Sys::VirtV2V::Converter::Linux;

use strict;
use warnings;

use Data::Dumper;
use Locale::TextDomain 'virt-v2v';

use XML::DOM;
use XML::DOM::XPath;

use Sys::VirtV2V::HVSource;
use Sys::VirtV2V::UserMessage qw(user_message);

use Carp;

=pod

=head1 NAME

Sys::VirtV2V::Converter::Linux - Convert a Linux guest to run on KVM

=head1 SYNOPSIS

 use Sys::VirtV2V::GuestOS;
 use Sys::VirtV2V::Converter;

 my $guestos = Sys::VirtV2V::GuestOS->instantiate($g, $os);
 Sys::VirtV2V::Converter->convert($vmm, $guestos, $dom, $os);

=head1 DESCRIPTION

Sys::VirtV2V::Converter::Linux convert a Linux guest to use KVM. It is an
implementation of the Sys::VirtV2V::Converter interface.

=head1 METHODS

=over

=cut

# Default values for a KVM configuration
use constant KVM_XML_VIRTIO => "
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

use constant KVM_XML_NOVIRTIO => "
<domain type='kvm'>
  <os>
    <type machine='pc'>hvm</type>
    <boot dev='hd'/>
  </os>
  <devices>
    <disk device='disk'>
      <target bus='scsi'/>
    </disk>
    <interface type='network'>
      <model type='e1000'/>
    </interface>
    <input type='mouse' bus='ps2'/>
    <graphics type='vnc' port='-1' listen='127.0.0.1'/>
  </devices>
</domain>
";

=item Sys::VirtV2V::Converter::Linux->can_handle(desc)

See BACKEND INTERFACE in L<Sys::VirtV2V::Converter> for details.

=cut

sub can_handle
{
    my $class = shift;

    my $desc = shift;
    carp("can_handle called without desc argument") unless defined($desc);

    return ($desc->{os} eq 'linux');
}

=item Sys::VirtV2V::Converter::Linux->convert(vmm, guestos, dom, desc)

See BACKEND INTERFACE in L<Sys::VirtV2V::Converter> for details.

=cut

sub convert
{
    my $class = shift;

    my ($vmm, $guestos, $dom, $desc) = @_;
    carp("convert called without vmm argument") unless defined($vmm);
    carp("convert called without guestos argument") unless defined($guestos);
    carp("convert called without dom argument") unless defined($dom);
    carp("convert called without desc argument") unless defined($desc);

    # Un-configure HV specific attributes which don't require a direct
    # replacement
    Sys::VirtV2V::HVSource->unconfigure($guestos, $desc);

    # Get the best available kernel
    my $kernel = _configure_kernel($guestos, $desc);

    # Check if the resulting kernel will support virtio
    my $virtio = $guestos->supports_virtio($kernel);

    # Configure the rest of the system
    _configure_display_driver($guestos, $virtio);
    _remap_block_devices($guestos, $dom, $desc, $virtio);
    _configure_kernel_modules($guestos, $desc, $virtio);
    _configure_boot($guestos, $kernel, $virtio);

    # Configure libvirt
    _configure_metadata($vmm, $dom, $desc, $virtio);

    my ($name) = $dom->findnodes('/domain/name/text()');
    $name = $name->getNodeValue();

    if($virtio) {
        print user_message
            (__x("{name} configured with virtio drivers", name => $name));
    } else {
        print user_message
            (__x("{name} configured without virtio drivers", name => $name));
    }
}

sub _remap_block_devices
{
    my ($guestos, $dom, $desc, $virtio) = @_;
    die("remap_block_devices called without guestos argument")
        unless defined($guestos);
    die("remap_block_devices called without dom argument")
        unless defined($dom);
    die("remap_block_devices called without desc argument")
        unless defined($desc);
    die("remap_block_devices called without virtio argument")
        unless defined($virtio);

    my %map = ();

    # Prefix is vd for virtio or sd for scsi
    my $prefix = $virtio ? 'vd' : 'sd';

    # Look for devices specified in the device metadata
    foreach my $dev ($dom->findnodes('/domain/devices/disk/target/@dev')) {
        if($dev->getNodeValue() =~ m{^(sd|hd|xvd)([a-z]+)\d*$}) {
            $map{"$1$2"} = "$prefix$2";

            # A guest might present an IDE disk as SCSI
            if($1 eq 'hd') {
                $map{"sd$2"} = "$prefix$2";
            }

            $dev->setNodeValue("$prefix$2");
        }
    }

    $guestos->remap_block_devices(\%map);
}

sub _configure_kernel_modules
{
    my ($guestos, $desc, $virtio) = @_;
    die("configure_kernel_modules called without guestos argument")
        unless defined($guestos);
    die("configure_kernel_modules called without desc argument")
        unless defined($desc);
    die("configure_kernel_modules called without virtio argument")
        unless defined($virtio);

    # Get a list of all old-hypervisor specific kernel modules which need to be
    # replaced or removed
    my %hvs_modules;
    foreach my $module (Sys::VirtV2V::HVSource->find_kernel_modules($guestos)) {
        $hvs_modules{$module} = undef;
    }

    # Go through all kernel modules looking for network or scsi devices
    my $modules = $desc->{modprobe_aliases};

    # Make a note of whether we've added scsi_hostadapter
    # We need this on RHEL 4/virtio because mkinitrd can't detect root on
    # virtio. For simplicity we always ensure this is set.
    my $scsi_hostadapter = 0;

    foreach my $module (keys(%$modules)) {
        # Replace network modules with virtio_net
        if($module =~ /^eth\d+$/) {
            # Make a note that we updated an old-HV specific kernel module
            if(exists($hvs_modules{$module})) {
                $hvs_modules{$module} = 1;
            }

            $guestos->update_kernel_module($module,
                                           $virtio ? "virtio_net" : "e1000");
        }

        # Replace block drivers with virtio_blk
        if($module =~ /^scsi_hostadapter/) {
            # Make a note that we updated an old-HV specific kernel module
            if(exists($hvs_modules{$module})) {
                $hvs_modules{$module} = 1;
            }

            $guestos->update_kernel_module($module,
                                          $virtio ? "virtio_blk" : "sym53c8xx");

            $scsi_hostadapter = 1;
        }
    }

    # Add an explicit scsi_hostadapter if it wasn't there before
    $guestos->enable_kernel_module('scsi_hostadapter',
                                $virtio ? "virtio_blk" : "sym53c8xx")
        unless($scsi_hostadapter);

    # Warn if any old-HV specific kernel modules weren't updated
    foreach my $module (keys(%hvs_modules)) {
        if(!defined($hvs_modules{$module})) {
            print STDERR user_message
                (__x("WARNING: Don't know how to update {module}, which loads ".
                     "the {modulename} module.",
                     module => $module,
                     modulename => $modules->{$module}->{modulename}));
        }
    }
}

sub _configure_display_driver
{
    my ($guestos, $virtio) = @_;
    die("configure_display_driver called without guestos argument")
        unless defined($guestos);
    die("configure_display_driver called without virtio argument")
        unless defined($virtio);

    $guestos->update_display_driver("cirrus");
}

sub _configure_kernel
{
    my ($guestos, $desc) = @_;
    die("configure_kernel called without guestos argument")
        unless defined($guestos);
    die("configure_kernel called without desc argument")
        unless defined($desc);

    my %kernels;

    # Look for installed kernels with virtio support
    foreach my $kernel (@{$desc->{kernels}}) {
        my %checklist = (
            "virtio_blk" => undef,
            "virtio_pci" => undef,
            "virtio_net" => undef
        );

        foreach my $module ($kernel->{modules}) {
            if(exists($checklist{$module})) {
                $checklist{$module} = 1;
            }
        }

        my $virtio = 1;
        foreach my $module (keys(%checklist)) {
            if(!defined($checklist{$module})) {
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

    my @remove_kernels = ();

    # Remove old-HV kernels
    foreach my $kernel (Sys::VirtV2V::HVSource->find_kernels($desc)) {
        # Remove the kernel from our cache
        delete($kernels{$kernel});

        push(@remove_kernels, $kernel);
    }

    # Find the highest versioned, virtio capable, installed kernel
    my $boot_kernel;
    foreach my $kernel (sort {$b cmp $a} (keys(%kernels))) {
        if($kernels{$kernel}) {
            $boot_kernel = $kernel;
            last;
        }
    }

    # If none of the installed kernels are appropriate, install a new one
    if(!defined($boot_kernel)) {
        $boot_kernel = $guestos->add_kernel();
    }

    # Check that either there are still kernels in the cache, or we just added a
    # kernel. If neither of these is the case, we're about to try to remove all
    # kernels, which will fail unpleasantly. Fail nicely instead.
    die(user_message(__"No bootable kernels installed, and no replacement ".
                       "specified in configuration.\nUnable to continue."))
        unless(keys(%kernels) > 0 || defined($boot_kernel));

    # Remove old kernels. We do this after installing a new kernel to keep rpm
    # happy
    foreach my $kernel (@remove_kernels) {
        # Uninstall the kernel from the guest
        $guestos->remove_kernel($kernel);
    }

    # If we didn't install a new kernel, pick the default kernel
    $boot_kernel ||= $guestos->get_default_kernel();

    return $boot_kernel;
}

sub _configure_boot
{
    my ($guestos, $kernel, $virtio) = @_;
    die("configure_boot called without guestos argument")
        unless defined($guestos);
    die("configure_boot called without kernel argument")
        unless defined($kernel);
    die("configure_boot called without virtio argument")
        unless defined($virtio);

    if($virtio) {
        $guestos->prepare_bootable($kernel, "virtio_pci", "virtio_blk");
    } else {
        $guestos->prepare_bootable($kernel);
    }
}

# Get the target architecture from the default boot kernel
sub _get_os_arch
{
    my ($desc) = @_;

    my $boot = $desc->{boot};
    my $default_boot = $boot->{default} if(defined($boot));

    my $arch;
    if(defined($default_boot)) {
        my $config = $boot->{configs}->[$default_boot];

        if(defined($config->{kernel})) {
            $arch = $config->{kernel}->{arch};
        }
    }

    # Default to i686 if we didn't find an architecture
    return 'i686' if(!defined($arch));

    # i386 should really be i686
    return 'i686' if($arch eq 'i386');

    return $arch;
}

sub _configure_metadata
{
    my ($vmm, $dom, $desc, $virtio) = @_;
    die("configure_metadata called without vmm argument")
        unless defined($vmm);
    die("configure_metadata called without dom argument")
        unless defined($dom);
    die("configure_metadata called without desc argument")
        unless defined($desc);
    die("configure_metadata called without virtio argument")
        unless defined($virtio);

    my $default_dom;
    if($virtio) {
        $default_dom = new XML::DOM::Parser->parse(KVM_XML_VIRTIO);
    } else {
        $default_dom = new XML::DOM::Parser->parse(KVM_XML_NOVIRTIO);
    }

    my $arch = _get_os_arch($desc);

    # Replace source hypervisor metadata with KVM defaults
    _unconfigure_hvs($dom, $default_dom);

    # Configure guest according to local hypervisor's capabilities
    _configure_capabilities($dom, $vmm, $arch);

    # Remove any configuration related to a PV kernel bootloader
    _unconfigure_bootloaders($dom);

    # Configure network and block drivers in the guest
    _configure_drivers($dom, $virtio);

    # Add a default os section if none exists
    _configure_os($dom, $default_dom, $arch);
}

sub _unconfigure_hvs
{
    my ($dom, $default_dom) = @_;
    die("unconfigure_hvs called without dom argument")
        unless defined($dom);
    die("unconfigure_hvs called without default_dom argument")
        unless defined($default_dom);

    # Get a list of source HV specific metadata nodes
    my @nodeinfo = Sys::VirtV2V::HVSource->find_metadata($dom);

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
            print STDERR user_message
                (__x("WARNING: No replacement found for {xpath} in ".
                     "domain XML. The node was removed.",
                     xpath => $xpath)) if($required);

            $node->getParentNode()->removeChild($node);
        }
    }
}

sub _configure_os
{
    my ($dom, $default_dom, $arch) = @_;

    my ($os) = $dom->findnodes('/domain/os');

    # If there's no os element, copy one from the default
    if(!defined($os)) {
        ($os) = $default_dom->findnodes('/domain/os');
        $os = $os->cloneNode(1);
        $os->setOwnerDocument($dom);

        my ($domain) = $dom->findnodes('/domain');
        $domain->appendChild($os);
    }

    my ($type) = $os->findnodes('type');

    # If there's no type element, copy one from the default
    if(!defined($type)) {
        ($type) = $default_dom->findnodes('/domain/os/type');
        $type = $type->cloneNode(1);
        $type->setOwnerDocument($dom);

        $os->appendChild($type);
    }

    # Set type/@arch unless it's already set
    my $arch_attr = $type->getAttributes()->getNamedItem('arch');
    $type->setAttribute('arch', $arch) unless(defined($arch_attr));
}

sub _configure_capabilities
{
    my ($dom, $vmm, $arch) = @_;

    # Parse the capabilities of the connected libvirt
    my $caps = new XML::DOM::Parser->parse($vmm->get_capabilities());

    (my $guestcap) = $caps->findnodes
        ("/capabilities/guest[arch[\@name='$arch']/domain/\@type='kvm']");

    die(__x("The connected hypervisor does not support a {arch} kvm guest",
        arch => $arch)) unless(defined($guestcap));

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
            print STDERR user_message
                (__x("The connected hypervisor does not support a ".
                     "machine type of {machine}.",
                     machine => $machine->getValue()));

            my ($type) = $dom->findnodes('/domain/os/type');
            $type->getAttributes()->removeNamedItem('machine');
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
            print STDERR user_message
                (__x("The connected hypervisor does not support ".
                     "feature {feature}", feature => $feature->getNodeName()));
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

sub _configure_drivers
{
    my ($dom, $virtio) = @_;

    # Convert disks
    # N.B. <disk> is required to have a <target> element

    # Convert alternate bus specifications
    foreach my $bus ($dom->findnodes('/domain/devices/disk/target/@bus')) {
        $bus->setNodeValue($virtio ? 'virtio' : 'scsi');
    }

    # Add an explicit bus specification to targets without one
    foreach my $target
        ($dom->findnodes('/domain/devices/disk/target[not(@bus)]'))
    {
        $target->setAttribute('bus', $virtio ? 'virtio' : 'scsi');
    }

    # Convert network adapters
    # N.B. <interface> is not required to have a <model> element, but <model>
    # is required to have a type attribute

    # Convert interfaces which already have a model element
    foreach my $type
        ($dom->findnodes('/domain/devices/interface/model/@type'))
    {
        $type->setNodeValue($virtio ? 'virtio' : 'e1000');
    }

    # Add a model element to interfaces which don't have one
    foreach my $interface
        ($dom->findnodes('/domain/devices/interface[not(model)]'))
    {
        my $model = $dom->createElement('model');
        $model->setAttribute('type', $virtio ? 'virtio' : 'e1000');
        $interface->appendChild($model);
    }
}

=back

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Converter(3pm)>,
L<Sys::VirtV2V(3pm)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
