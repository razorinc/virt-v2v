# Sys::VirtV2V::Target::LibVirt
# Copyright (C) 2010 Red Hat Inc.
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

use strict;
use warnings;

package Sys::VirtV2V::Target::LibVirt::Vol;

use POSIX;

use Sys::VirtV2V::Util qw(user_message);

use Locale::TextDomain 'virt-v2v';

sub _new
{
    my $class = shift;
    my ($vol) = @_;

    my $self = {};
    bless($self, $class);

    $self->{vol} = $vol;

    return $self;
}

sub _create
{
    my $class = shift;
    my ($pool, $name, $size) = @_;

    my $vol_xml = "
        <volume>
            <name>$name</name>
            <capacity>$size</capacity>
        </volume>
    ";

    my $vol;
    eval {
        $vol = $pool->create_volume($vol_xml);
    };
    die(user_message(__x("Failed to create storage volume: {error}",
                         error => $@->stringify()))) if ($@);

    return $class->_new($vol);
}

sub _get
{
    my $class = shift;
    my ($pool, $name) = @_;

    my $vol;
    eval {
        $vol = $pool->get_volume_by_name($name);
    };
    die(user_message(__x("Failed to get storage volume: {error}",
                          error => $@->stringify()))) if ($@);

    return $class->_new($vol);
}

sub get_path
{
    my $self = shift;

    return $self->{vol}->get_path();
}

sub get_format
{
    my $self = shift;

    my $vol = $self->{vol};
    my $voldom = new XML::DOM::Parser->parse($vol->get_xml_description());

    my ($format) = $voldom->findnodes('/volume/target/format/@type');
    $format = $format->getValue() if (defined($format));
    $format ||= 'auto';

    return $format;
}

sub is_block
{
    my $self = shift;

    my $type = $self->{vol}->get_info()->{type};
    return $type == Sys::Virt::StorageVol::TYPE_BLOCK;
}

sub open
{
    my $self = shift;

    my $path = $self->get_path();

    # We want to open the existing volume without truncating it
    sysopen(my $fd, $path, O_WRONLY)
        or die(user_message(__x("Error opening storage volume {path} ".
                                "for writing: {error}", error => $!)));

    $self->{fd} = $fd;
}

sub write
{
    my $self = shift;
    my ($data) = @_;

    defined($self->{fd}) or die("write called without open");

    syswrite($self->{fd}, $data)
        or die(user_message(__x("Error writing to {path}: {error}",
                                path => $self->get_path(),
                                error => $!)));
}

sub close
{
    my $self = shift;

    close($self->{fd})
        or die(user_message(__x("Error closing volume handle: {error}",
                                error => $!)));

    delete($self->{fd});
}

package Sys::VirtV2V::Target::LibVirt;

use Sys::Virt;
use Sys::Virt::Error;

use Sys::VirtV2V::Util qw(user_message);
use Locale::TextDomain 'virt-v2v';

=head1 NAME

Sys::VirtV2V::Target::LibVirt - Output to libvirt

=head1 SYNOPSIS

 use Sys::VirtV2V::Target::LibVirt;

 my $target = new Sys::VirtV2V::Target::LibVirt($uri, $poolname);

=head1 DESCRIPTION

Sys::VirtV2V::Target::LibVirt creates a new libvirt domain using the given
target URI. New storage will be created in the target pool.

=head1 METHODS

=over

=item Sys::VirtV2V::Target::LibVirt->new(uri, poolname)

Create a new Sys::VirtV2V::Target::LibVirt object.

=over

=item uri

A libvirt connection URI

=item poolname

The name of a storage pool managed by the target libvirt daemon.

=back

=cut

sub new
{
    my $class = shift;
    my ($uri, $poolname) = @_;

    my $self = {};
    bless($self, $class);

    $self->{vmm} = Sys::Virt->new(auth => 1, uri => $uri);

    eval {
        $self->{pool} = $self->{vmm}->get_storage_pool_by_name($poolname);
    };

    if ($@) {
        die(user_message(__x("Output pool {poolname} is not a valid ".
                             "storage pool",
                             poolname => $poolname)));
    }

    return $self;
}

=item create_volume(name, size)

Create a new volume in the pool whose name was passed to new().

=over

=item name

The name of the volume which is being created.

=item size

The size of the volume which is being created in bytes.

=back

create_volume() returns a Sys::VirtV2V::Target::LibVirt::Vol object.

=cut

sub create_volume
{
    my $self = shift;
    my ($name, $size) = @_;

    return Sys::VirtV2V::Target::LibVirt::Vol->_create($self->{pool},
                                                       $name, $size);
}

=item volume_exists (name)

Check if volume I<name> exists in the target pool.

Returns 1 if it exists, 0 otherwise.

=cut

sub volume_exists
{
    my $self = shift;
    my ($name) = @_;

    my $pool = $self->{pool};
    my $vol;
    eval {
        $vol = $pool->get_volume_by_name($name);
    };

    # The above command will generate VIR_ERR_NO_STORAGE_VOL if the
    # volume doesn't exist
    if ($@) {
        # Warn if we got any other error
        if ($@->code != Sys::Virt::Error::ERR_NO_STORAGE_VOL) {
            warn user_message(__x("WARNING: Unexpected error accessing ".
                                  "storage pool {name}: {error}",
                                  name => $pool->get_name(),
                                  error => $@->stringify()));
        }

        return 0;
    }

    return 1;
}

=item get_volume (name)

Get a reference to an existing volume. See L<create_volume> for return value.

=cut

sub get_volume
{
    my $self = shift;
    my ($name) = @_;

    return Sys::VirtV2V::Target::LibVirt::Vol->_get($self->{pool}, $name);
}

=item guest_exists(name)

Return 1 if a guest with I<name> already exists, 0 otherwise.

=cut

sub guest_exists
{
    my $self = shift;
    my ($name) = @_;

    eval {
        $self->{vmm}->get_domain_by_name($name);
    };

    if ($@) {
        if ($@->code == Sys::Virt::Error::ERR_NO_DOMAIN) {
            return 0;
        }

        die(user_message(__x("Error checking for domain: {error}",
                             error => $@->stringify())));
    }

    return 1;
}

=item create_guest(dom)

Create the guest in the target

=cut

sub create_guest
{
    my $self = shift;
    my ($desc, $dom, $guestcaps) = @_;

    my $vmm = $self->{vmm};

    _unconfigure_incompatible_devices($dom);
    _configure_capabilities($vmm, $dom, $guestcaps);

    $vmm->define_domain($dom->toString());
}

sub _unconfigure_incompatible_devices
{
    my ($dom) = @_;

    foreach my $path (
        # We have replaced the SCSI controller with either VirtIO or IDE.
        # Additionally, attempting to start a guest converted from ESX, which
        # has an lsilogic SCSI controller, will fail on RHEL 5.
        $dom->findnodes("/domain/devices/controller[\@type='scsi']")
    )
    {
        $path->getParentNode()->removeChild($path);
    }
}

# Configure guest according to target hypervisor's capabilities
sub _configure_capabilities
{
    my ($vmm, $dom, $guestcaps) = @_;

    # Parse the capabilities of the connected libvirt
    my $caps = new XML::DOM::Parser->parse($vmm->get_capabilities());

    my $arch = $guestcaps->{arch};

    (my $guestcap) = $caps->findnodes
        ("/capabilities/guest[arch[\@name='$arch']/domain/\@type='kvm']");

    die(user_message(__x("The connected hypervisor does not support a {arch} ".
                         "kvm guest.",
                         arch => $arch))) unless(defined($guestcap));

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
            warn user_message(__x("The connected hypervisor does not support ".
                                  "a machine type of {machine}. It will be ".
                                  "set to the current default.",
                                  machine => $machine->getValue()));

            my ($type) = $dom->findnodes('/domain/os/type');
            $type->getAttributes()->removeNamedItem('machine');
        }
    }

    # Get the domain features node
    my ($domfeatures) = $dom->findnodes('/domain/features');
    # Check existing features are supported by the hypervisor
    if (defined($domfeatures)) {
        # Check that /domain/features are listed in capabilities
        # Get a list of supported features
        my %features;
        foreach my $feature ($guestcap->findnodes('features/*')) {
            $features{$feature->getNodeName()} = 1;
        }

        foreach my $feature ($domfeatures->findnodes('*')) {
            my $name = $feature->getNodeName();

            if (!exists($features{$name})) {
                warn user_message(__x("The connected hypervisor does not ".
                                      "support feature {feature}.",
                                      feature => $name));
                $feature->getParentNode()->removeChild($feature);
            }

            if ($name eq 'acpi' && !$guestcaps->{acpi}) {
                warn user_message (__"The target guest does not support acpi ".
                                     "under KVM. ACPI will be disabled.");
                $feature->getParentNode()->removeChild($feature);
            }
        }
    }

    # Add a features element if there isn't one already
    else {
        $domfeatures = $dom->createElement('features');
        my ($root) = $dom->findnodes('/domain');
        $root->appendChild($domfeatures);
    }

    # Add acpi support if the guest supports it
    if ($guestcaps->{acpi}) {
        $domfeatures->appendChild($dom->createElement('acpi'));
    }

    # Add apic and pae if they're supported by the hypervisor and not already
    # there
    foreach my $feature ('apic', 'pae') {
        my ($d) = $domfeatures->findnodes($feature);
        next if (defined($d));

        my ($c) = $guestcap->findnodes("features/$feature");
        if (defined($c)) {
            $domfeatures->appendChild($dom->createElement($feature));
        }
    }
}

=back

=head1 COPYRIGHT

Copyright (C) 2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING for the full license.

=cut

1;
