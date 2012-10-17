# Sys::VirtConvert::Connection::LibVirtTarget
# Copyright (C) 2010-2011 Red Hat Inc.
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

package Sys::VirtConvert::Connection::LibVirtTarget;

use Sys::Virt;
use Sys::Virt::Error;
use Sys::Virt::StorageVol;

use Sys::VirtConvert::Connection::LibVirt;
use Sys::VirtConvert::Connection::Volume;
use Sys::VirtConvert::Util qw(:DEFAULT parse_libvirt_volinfo);

use XML::DOM;

use Locale::TextDomain 'virt-v2v';

@Sys::VirtConvert::Connection::LibVirtTarget::ISA =
    qw(Sys::VirtConvert::Connection::LibVirt);

=head1 NAME

Sys::VirtConvert::Connection::LibVirtTarget - Output to libvirt

=head1 SYNOPSIS

 use Sys::VirtConvert::Connection::LibVirtTarget;

 my $target = new Sys::VirtConvert::Connection::LibVirtTarget($uri, $poolname);

=head1 DESCRIPTION

Sys::VirtConvert::Connection::LibVirtTarget creates a new libvirt domain using
the given target URI. New storage will be created in the target pool.

=cut

# All volumes which have been created for this target. These will be
# automatically deleted if the guest is not created successfully.
our @cleanup_vols;

=head1 METHODS

=over

=item Sys::VirtConvert::Connection::LibVirtTarget->new(uri, poolname)

Create a new LibVirtTarget object.

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

    my $self = $class->SUPER::_libvirt_new($uri);

    eval {
        $self->{pool} = $self->{vmm}->get_storage_pool_by_name($poolname);
    };
    v2vdie __x('Output pool {poolname} is not a valid storage pool.',
               poolname => $poolname) if $@;

    return $self;
}

=item create_volume(name, format, size, sparse)

Create a new volume in the pool whose name was passed to new().

=over

=item name

The name of the volume which is being created.

=item format

The file format of the new volume.

=item size

The size of the volume which is being created in bytes.

=item sparse

1 if the output should be sparse if possible, 0 otherwise.

=back

create_volume() returns a Sys::VirtConvert::Connection::Volume object.

=cut

sub create_volume
{
    my $self = shift;
    my ($name, $format, $size, $sparse) = @_;

    # If we're create a volume in an LVM pool, libvirt can silently create a
    # volume which is smaller than we expected: RHBZ#670529
    # To work round this, we round up to a 1k boundary here
    my $over = $size % 1024;
    $size += 1024 - $over if $over != 0;

    my $pool = $self->{pool};

    # XXX: This is a heisenbug. Without the line below, $pool is undef when
    # retrieved from @cleanup_vols in DESTROY. The use of Dumper is essential. I
    # have absolutely no idea why this should be. mbooth@redhat.com 28/03/2011
    use Data::Dumper; logmsg DEBUG, "Pool: ".Dumper($pool);

    # Store the volume before creation so we can catch an interruption during or
    # very shortly after volume creation
    push(@cleanup_vols, [$pool, $name]);

    my $allocation = $sparse ? 0 : $size;

    my $vol_xml_format = "
        <volume>
            <name>$name</name>
            <capacity>$size</capacity>
            <allocation>$allocation</allocation>
            <target>
                <format type='$format'/>
            </target>
        </volume>
    ";

    my $vol_xml_noformat = "
        <volume>
            <name>$name</name>
            <capacity>$size</capacity>
            <allocation>$allocation</allocation>
        </volume>
    ";

    # Only certain pool types support different file formats. Check the pool
    # type to determine whether we should create the volume with an explicit
    # format, or rely on the pool default.
    my $vol_xml;
    my $pooldom = new XML::DOM::Parser->parse($pool->get_xml_description());
    my ($pooltype) = $pooldom->findnodes('/pool/@type');
    if (defined($pooltype)) {
        $pooltype = $pooltype->getNodeValue();

        if ($Sys::VirtConvert::Connection::LibVirt::format_pools{$pooltype}) {
            $vol_xml = $vol_xml_format;
        } else {
            $vol_xml = $vol_xml_noformat;

            # If the target format type isn't raw, warn the user that they're
            # not getting what they expected
            logmsg WARN, __x('Target pool type {pooltype} doesn\'t support '.
                             'format {format}',
                             pooltype => $pooltype, format => $format)
                unless $format eq 'raw';

            $format = 'raw';
        }
    } else { # Should be impossible
        logmsg WARN, __x('Pool XML has no type attribute: {xml}',
                         xml => $pool->get_xml_description());

        $vol_xml = $vol_xml_noformat;
    }

    my $vol;
    eval {
        $vol = $pool->create_volume($vol_xml);
    };
    v2vdie __x('Failed to create storage volume: {error}',
               error => $@->stringify()) if $@;

    my $info = $vol->get_info();
    my $is_block = $info->{type} == Sys::Virt::StorageVol::TYPE_BLOCK ? 1 : 0;

    my $transfer = $self->_get_transfer($name, $vol->get_path(), $format,
                                        $sparse, $is_block);
    return new Sys::VirtConvert::Connection::Volume($name, $format,
                                                $vol->get_path(),
                                                $size, $allocation,
                                                $sparse, $is_block,
                                                $transfer);
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
        logmsg WARN, __x('Unexpected error accessing '.
                         'storage pool {name}: {error}',
                         name => $pool->get_name(),
                         error => $@->stringify())
            if ($@->code != Sys::Virt::Error::ERR_NO_STORAGE_VOL);

        return 0;
    }

    return 1;
}

=item get_volume (name)

Get a reference to an existing volume. get_volume returns a
Sys::VirtConvert::Connection::Volume object.

=cut

sub get_volume
{
    my $self = shift;
    my ($name) = @_;

    my $uri = $self->{uri};
    my $pool = $self->{pool};

    my $vol;
    eval {
        $vol = $pool->get_volume_by_name($name);
    };
    v2vdie __x('Failed to get storage volume: {error}',
               error => $@->stringify()) if $@;

    my (undef, $format, $size, $usage, $is_sparse, $is_block) =
        parse_libvirt_volinfo($vol);

    my $transfer = $self->_get_transfer($name, $vol->get_path(), $format,
                                        $is_sparse, $is_block);
    return new Sys::VirtConvert::Connection::Volume($name, $format,
                                                $vol->get_path(),
                                                $size, $usage,
                                                $is_sparse, $is_block,
                                                $transfer);
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

        v2vdie __x('Error checking for domain: {error}',
                   error => $@->stringify());
    }

    return 1;
}

=item create_guest(g, root, meta, config, guestcaps, output_name)

Create the guest in the target

=cut

sub create_guest
{
    my $self = shift;
    my (undef, undef, $meta, $config, $guestcaps, $output_name) = @_;

    my $vmm = $self->{vmm};

    $meta->{name} = $output_name;
    _configure_capabilities($vmm, $meta, $guestcaps);

    $vmm->define_domain(_meta_to_domxml($meta, $config, $guestcaps));

    # Guest is successfully created, don't remove its volumes
    @cleanup_vols = ();
}

sub _meta_to_domxml
{
    my ($meta, $config, $guestcaps) = @_;

    my $dom = new XML::DOM::Parser->parse(<<DOM);
<domain type='kvm'>
  <os>
    <type>hvm</type>
    <boot dev='hd'/>
  </os>
  <on_poweroff>destroy</on_poweroff>
  <on_reboot>restart</on_reboot>
  <on_crash>restart</on_crash>
  <devices>
    <input type='tablet' bus='usb'/>
    <input type='mouse' bus='ps2'/>
    <video>
      <model type='cirrus' vram='9216' heads='1'/>
    </video>
    <console type='pty'/>
  </devices>
</domain>
DOM

    my $root = $dom->getDocumentElement();

    _append_elem($root, 'name', $meta->{name});
    _append_elem($root, 'memory', $meta->{memory} / 1024);
    _append_elem($root, 'vcpu', $meta->{cpus});

    my ($ostype) = $root->findnodes('os/type');
    $ostype->setAttribute('arch', $guestcaps->{arch});

    my $features = _append_elem($root, 'features');
    foreach my $feature (@{$meta->{features}}) {
        _append_elem($features, $feature);
    }

    my $virtio = $guestcaps->{block} eq 'virtio' ? 1 : 0;
    my $prefix = $virtio == 1 ? 'vd' : 'hd';
    my $suffix = 'a';

    my $nide = 0;

    my ($devices) = $root->findnodes('devices');

    my $display_type;
    my $display_keymap;
    my $display_password;
    if (defined($meta->{display})) {
        $display_type       = $meta->{display}->{type};
        $display_keymap     = $meta->{display}->{keymap};
        $display_password   = $meta->{display}->{password};
    } else {
        $display_type = 'vnc';
    }

    my $graphics = _append_elem($devices, 'graphics');
    $graphics->setAttribute('type', $display_type);
    $graphics->setAttribute('keymap', $display_keymap)
        if defined($display_keymap);
    $graphics->setAttribute('passwd', $display_password)
        if defined($display_password);

    foreach my $disk (sort { $a->{device} cmp $b->{device} } @{$meta->{disks}})
    {
        my $is_block = $disk->{dst}->is_block();

        my $diskE = _append_elem($devices, 'disk');
        $diskE->setAttribute('device', 'disk');
        $diskE->setAttribute('type', $is_block ? 'block' : 'file');

        my $driver = _append_elem($diskE, 'driver');
        $driver->setAttribute('name', 'qemu');
        $driver->setAttribute('type', $disk->{dst}->get_format());
        $driver->setAttribute('cache', 'none');

        my $source = _append_elem($diskE, 'source');
        $source->setAttribute($is_block ? 'dev' : 'file',
                              $disk->{dst}->get_path());

        my $target = _append_elem($diskE, 'target');
        $target->setAttribute('dev', $prefix.$suffix); $suffix++;
        $target->setAttribute('bus', $guestcaps->{block});

        $nide++ unless $virtio;
    }

    # Add the correct number of cdrom and floppy drives with appropriate new
    # names
    $suffix = 'a' if ($virtio);
    my $fdn = 0;
    foreach my $removable (@{$meta->{removables}}) {
        my $name;
        my $bus;
        if ($removable->{type} eq 'cdrom') {
            $bus = 'ide';
            $name = 'hd'.$suffix; $suffix++;
            $nide++;
        } elsif ($removable->{type} eq 'floppy') {
            $bus = 'fdc';
            $name = 'fd'.$fdn; $fdn++;
        } else {
            logmsg WARN, __x('Ignoring removable device {device} with unknown '.
                             'type {type}.',
                             device => $removable->{device},
                             type => $removable->{type});
            next;
        }

        my $diskE = _append_elem($devices, 'disk');
        $diskE->setAttribute('device', $removable->{type});
        $diskE->setAttribute('type', 'file');

        my $driver = _append_elem($diskE, 'driver');
        $driver->setAttribute('name', 'qemu');
        $driver->setAttribute('type', 'raw');

        my $target = _append_elem($diskE, 'target');
        $target->setAttribute('dev', $name);
        $target->setAttribute('bus', $bus);

        _append_elem($diskE, 'readonly') if ($removable->{type} eq 'cdrom');
    }

    logmsg WARN, __x('Only 4 IDE devices are supported, but this guest has '.
                     '{number}. The guest will not operate correctly without '.
                     'manual reconfiguration.', number => $nide) if $nide > 4;

    foreach my $nic (@{$meta->{nics}}) {
        # Find an appropriate mapped network
        my ($vnet, $vnet_type) =
            $config->map_network($nic->{vnet}, $nic->{vnet_type});
        $vnet ||= $nic->{vnet};
        $vnet_type ||= $nic->{vnet_type};

        my $interface = _append_elem($devices, 'interface');
        $interface->setAttribute('type', $vnet_type);

        my $mac = _append_elem($interface, 'mac');
        $mac->setAttribute('address', $nic->{mac});

        my $source = _append_elem($interface, 'source');
        $source->setAttribute($vnet_type, $vnet);

        my $model = _append_elem($interface, 'model');
        $model->setAttribute('type', $guestcaps->{net});
    }

    return $dom->toString();
}

sub _append_elem
{
    my ($parent, $name, $text) = @_;

    my $doc = $parent->getOwnerDocument();
    my $e = $doc->createElement($name);
    my $textE = $doc->createTextNode($text) if defined($text);

    $parent->appendChild($e);
    $e->appendChild($textE) if defined($text);

    return $e;
}

sub DESTROY
{
    my $self = shift;

    while (my $i = shift(@cleanup_vols)) {
        my $pool = $i->[0];
        my $name = $i->[1];

        logmsg INFO, __x('Automatically cleaning up volume {vol} from pool '.
                         '{pool}.', vol => $name, pool => $pool);

        # Lookup the volume in the pool
        my $vol;
        eval {
            $vol = $pool->get_volume_by_name($name);
        };
        # If the volume isn't present there's no need to delete it
        next if ($@);

        $vol->delete(Sys::Virt::StorageVol::DELETE_NORMAL);
    }
}

# Configure guest according to target hypervisor's capabilities
sub _configure_capabilities
{
    my ($vmm, $meta, $guestcaps) = @_;

    # Parse the capabilities of the connected libvirt
    my $caps = new XML::DOM::Parser->parse($vmm->get_capabilities());

    my $arch = $guestcaps->{arch};

    (my $guestcap) = $caps->findnodes
        ("/capabilities/guest[arch[\@name='$arch']/domain/\@type='kvm']");

    v2vdie __x('The connected hypervisor does not support a {arch} kvm guest.',
               arch => $arch) unless defined($guestcap);

    # Check existing features are supported by the hypervisor
    if (exists($meta->{features})) {
        # Check that requested features are listed in capabilities
        # Get a list of supported features
        my %features;
        foreach my $feature ($guestcap->findnodes('features/*')) {
            $features{$feature->getNodeName()} = 1;
        }

        my @new_features = ();
        foreach my $feature (@{$meta->{features}}) {
            if (!exists($features{$feature})) {
                logmsg WARN, __x('The connected hypervisor does not '.
                                 'support feature {feature}.',
                                 feature => $feature);
            }

            elsif ($feature eq 'acpi' && !$guestcaps->{acpi}) {
                logmsg WARN, __('The target guest does not support acpi '.
                                'under KVM. ACPI will be disabled.');
            }

            else {
                push(@new_features, $feature);
            }

            $meta->{features} = \@new_features;
        }
    }

    # Add acpi support if the guest supports it
    if ($guestcaps->{acpi}) {
        push(@{$meta->{features}}, 'acpi') unless $meta->{features} ~~ 'acpi';
    }

    # Add apic and pae if they're supported by the hypervisor and not already
    # there
    foreach my $feature ('apic', 'pae') {
        next if $meta->{features} ~~ $feature;

        my ($c) = $guestcap->findnodes("features/$feature");
        push(@{$meta->{features}}, $feature) if defined($c);
    }
}

=back

=head1 COPYRIGHT

Copyright (C) 2010-2011 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING for the full license.

=cut

1;
