# Sys::VirtV2V::Converter
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

package Sys::VirtV2V::Converter;

use strict;
use warnings;

use Module::Pluggable sub_name => 'modules',
                      search_path => ['Sys::VirtV2V::Converter'],
                      require => 1;

use Locale::TextDomain 'virt-v2v';

use Sys::VirtV2V::UserMessage qw(user_message);

=pod

=head1 NAME

Sys::VirtV2V::Converter - Convert a guest to run on KVM

=head1 SYNOPSIS

 use Sys::VirtV2V::GuestOS;
 use Sys::VirtV2V::Converter;

 my $guestos = Sys::VirtV2V::GuestOS->instantiate($g, $os);
 Sys::VirtV2V::Converter->convert($vmm, $guestos, $dom, $os);

=head1 DESCRIPTION

Sys::VirtV2V::Converter instantiates an appropriate backend for the target guest
OS, and uses it to convert the guest to run on KVM.

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

=item Sys::VirtV2V::Converter->convert(vmm, guestos, dom, desc)

Instantiate an appropriate backend and call convert on it.

=over

=item vmm

A Sys::Virt connection.

=item guestos

An initialised Sys::VirtV2V::GuestOS object for the guest.

=item dom

An XML::DOM object resulting from parsing the guests's libvirt domain XML.

=item desc

The OS description returned by Sys::Guestfs::Lib.

=back

=cut

sub convert
{
    my $class = shift;

    my ($vmm, $guestos, $dom, $desc) = @_;
    carp("convert called without vmm argument") unless defined($vmm);
    carp("convert called without guestos argument") unless defined($guestos);
    carp("convert called without dom argument") unless defined($dom);
    carp("convert called without desc argument") unless defined($desc);

    my $guestcaps;

    # Find a module which can convert the guest and run it
    foreach my $module ($class->modules()) {
        if($module->can_handle($desc)) {
            $guestcaps = $module->convert($vmm, $guestos, $dom, $desc);
            last;
        }
    }

    die(__"Unable to find a module to convert this guest")
        unless (defined($guestcaps));

    # Convert the metadata
    _configure_metadata($vmm, $dom, $desc, $guestcaps);

    my ($name) = $dom->findnodes('/domain/name/text()');
    $name = $name->getNodeValue();

    if($guestcaps->{virtio}) {
        print user_message
            (__x("{name} configured with virtio drivers", name => $name));
    } else {
        print user_message
            (__x("{name} configured without virtio drivers", name => $name));
    }
}

sub _configure_metadata
{
    my ($vmm, $dom, $desc, $guestcaps) = @_;

    my $arch   = $guestcaps->{arch};
    my $virtio = $guestcaps->{virtio};

    my $default_dom;
    if($virtio) {
        $default_dom = new XML::DOM::Parser->parse(KVM_XML_VIRTIO);
    } else {
        $default_dom = new XML::DOM::Parser->parse(KVM_XML_NOVIRTIO);
    }

    # Replace source hypervisor metadata with KVM defaults
    _unconfigure_hvs($dom, $default_dom);

    # Configure guest according to local hypervisor's capabilities
    _configure_capabilities($dom, $vmm, $guestcaps);

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
    my @nodeinfo = _find_hv_metadata($dom);

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
    my ($dom, $vmm, $guestcaps) = @_;

    # Parse the capabilities of the connected libvirt
    my $caps = new XML::DOM::Parser->parse($vmm->get_capabilities());

    my $arch = $guestcaps->{arch};

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
        my $name = $feature->getNodeName();

        if (!exists($features{$name})) {
            print STDERR user_message
                (__x("The connected hypervisor does not support ".
                     "feature {feature}", feature => $name));
            $feature->getParentNode()->removeChild($feature);
        }

        if ($name eq 'acpi' && !$guestcaps->{acpi}) {
            print STDERR user_message
                (__"The target guest does not support acpi under KVM. ACPI ".
                   "will be disabled.");
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

sub _find_hv_metadata
{
    my ($dom) = @_;

    return _find_xen_metadata($dom);
}

sub _find_xen_metadata
{
    my $dom = shift;
    defined($dom) or carp("find_metadata called without dom argument");

    # List of nodes requiring changes if they exist and match a particular
    # pattern, and whether they need to be replaced for a guest to function
    # Most of this is taken from inspection of domain.rng
    my @check_nodes = (
        [ '/domain/@type', 'xen', 1 ],
        [ '/domain/devices/emulator', 'xen', 0 ],
        [ '/domain/devices/input/@bus', 'xen', 1 ],
        [ '/domain/devices/interface/script/@path', 'vif-bridge', 0],
        [ '/domain/os/loader', 'xen', 0 ],
        [ '/domain/os/type/@machine', '(xenpv|xenfv|xenner)', 0 ],
        [ '/domain/devices/disk/target/@bus', 'xen', 0 ],
        [ '/domain/bootloader', undef, 0],
        [ '/domain/bootloader_args', undef, 0]
    );

    my @nodeinfo = ();
    foreach my $check_node (@check_nodes) {
        my $xpath = $check_node->[0];
        my $pattern = $check_node->[1];
        my $required = $check_node->[2];

        foreach my $node ($dom->findnodes($xpath)) {
            if(defined($pattern)) {
                my $value;
                if($node->isa('XML::DOM::Attr')) {
                    $value = $node->getNodeValue();
                } else {
                    my ($text) = $node->findnodes('text()');
                    $value = $text->getNodeValue();
                }

                next unless($value =~ m{$pattern});
            }

            push(@nodeinfo, $node => [ $xpath, $required ]);
        }
    }

    return @nodeinfo;
}

=back

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Converter::Linux(3pm)>,
L<Sys::VirtV2V::GuestOS(3pm)>,
L<Sys::Guestfs::Lib(3pm)>,
L<Sys::Virt(3pm)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
