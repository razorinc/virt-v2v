# Sys::VirtConvert::Connection::LibVirtXMLSource
# Copyright (C) 2012 Red Hat Inc.
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

package Sys::VirtConvert::Connection::VMWareOVASource;

use strict;
use warnings;

use File::Basename;
use File::Temp qw(tempdir);
use XML::DOM;
use XML::DOM::XPath;
use Archive::Extract;
use Digest::SHA1;

use Sys::VirtConvert::Connection::Source;
use Sys::VirtConvert::Transfer::Local;
use Sys::VirtConvert::Util;

use Locale::TextDomain 'virt-v2v';

@Sys::VirtConvert::Connection::VMWareOVASource::ISA =
    qw(Sys::VirtConvert::Connection::Source);

# Hardware profiles for Hardware Resources in OVF file

our %hw_families = (
    'Processor'         => 3,
    'Memory'            => 4,
    'IDE Controller'    => 5,
    'SCSI Controller'   => 6,
    'Ethernet'          => 10,
    'Floppy'            => 14,
    'CD Drive'          => 15,
    'DVD Drive'         => 16,
    'Disk Drive'        => 17,
    'USB Controller'    => 23
);

# Support for uncompressing big files

$Archive::Extract::PREFER_BIN = 1;


sub new
{
    my $class = shift;
    my($ova, $target) = @_;

    my $self = {};
    bless($self, $class);

    $self->{extractdir} = tempdir(CLEANUP => 1);

    $self->_uncompress_archive($ova);
    $self->_verify_manifest();
    $self->_get_meta();
    return $self;
}

=item get_name

Return the name of the domain.

=cut

sub get_name
{
    my $meta = shift->{meta};

    return $meta->{name};
}


sub _uncompress_archive
{
    my $self = shift;
    my ($ova) = @_;

    my $ae = Archive::Extract->new(archive => $ova, type => 'tar');
    $ae->extract(to => $self->{extractdir});
}

sub _get_meta
{
    my $self = shift;

    my ($ovf) = glob($self->{extractdir}.'/*.ovf');
    open(my $xml, '<', $ovf)
        or v2vdie __x('Failed to open {ovf} for reading', ovf => $ovf);
}

sub _parse_dom
{
    my ($source, $dom) = @_;

    my %meta;

    my $root=$dom->getDocumentElement();
    $meta{name} = _node_val($root, '/Envelope/VirtualSystem/Name/text()');
    $meta{memory} = _node_val($root, "/Envelope/VirtualSystem/VirtualHardwareSection/Item/VirtualQuantity[../rasd:ResourceType = $hw_families{Memory}");
    $meta{cpus} = _node_val($root, "/Envelope/VirtualSystem/VirtualHardwareSection/Item/VirtualQuantity[../rasd:ResourceType = $hw_families{Cpu}");

    # return vmx-08 that is vmware esxi 5.0
    $meta{src_type} = _node_val($root, "/Envelope/VirtualSystem/VirtualHardwareSection/System/vssd:VirtualSystemType/text()");

    $meta{features} = []; # TBD: not sure how to gather them from OVF yet

    $meta{disks} = [];

    #iterate on scsi controllers Id

    my $scsi_controllers = _collect_controllers($root, $hw_families{'SCSI Controller'});

    for my $scsi_controller (sort keys %$scsi_controllers) {
        my %info;
        foreach my $disk (_get_resources($root, $scsi_controllers->{$scsi_controller}, $hw_families{'Disk Drive'})){
            my $hd_number = _node_val($disk, "rasd:AddressOnParent/text()") ;
            $info{device} = "sd".numbers_to_letters($hd_number); #transformation from numbers to letters
            my $disk_reference=(split("/", _node_val($disk, "rasd:HostResource/text()")))[-1];
            my $disk_name=_node_val($root, '/Envelope/References/File[contains(@ovf:id, /Envelope/DiskSection/Disk[contains(@ovf:diskId, "'.$disk_reference.'")]/@ovf:fileRef )]/@ovf:href');
            my $path = $ENV{'TEMP_DIR'}.$disk_name;
            $info{src} = $source->get_volume($path);

            push(@{$meta{disks}}, \%info);
        }

    }

    my $ide_peripherals = 0;
    my $ide_controllers = _collect_controllers($root, $hw_families{'IDE Controller'});
    for my $ide_controller (sort keys %$ide_controllers) {
        my %info;
        foreach my $disk (_get_resources($root, $ide_controllers->{$ide_controller}, $hw_families{'Disk Drive'})) {
            my $hd_number = _node_val($disk, "rasd:AddressOnParent/text()") ;
            $info{device} = "sd".numbers_to_letters($hd_number); #transformation from numbers to letters
            my $disk_reference=(split("/", _node_val($disk, "rasd:HostResource/text()")))[-1];
            my $disk_name=_node_val($root, '/Envelope/References/File[contains(@ovf:id, /Envelope/DiskSection/Disk[contains(@ovf:diskId, "'.$disk_reference.'")]/@ovf:fileRef )]/@ovf:href');
            my $path = $ENV{'TEMP_DIR'}.$disk_name;
            $info{src} = $source->get_volume($path);
            $ide_peripherals++;
            push(@{$meta{disks}}, \%info);
        }
    }

    $meta{removables} = [];

    for my $ide_controller (sort keys %$ide_controllers) {
        my %info;
        foreach my $removable (_get_resources($root, $ide_controllers->{$ide_controller}, $hw_families{'CD Drive'})) {
            my $cd_number = _node_val($removable, "rasd:AddressOnParent/text()") ;
            $info{device} = "hd"._numbers_to_letters($cd_number+$ide_peripherals);
            $info{type} = "cdrom";
            $ide_peripherals++;
            push(@{$meta{removables}}, \%info);
        }
    }

    $meta{nics} = [];

    foreach my $nic (_collect_controllers($root, $hw_families{'Ethernet'} ) ) {
        my %info;

        $info{mac} = ""; # it's assigned automatically by the hypervisor
        $info{vnet} = "";    # not clear how to get it from the ovf
        $info{vnet_type} = ""; #not clear how to get it from the ovf
        push(@{$meta{nics}}, \%info);
    }


}

sub _collect_controllers
{
    my ($root, $kind) = @_;

    my %controllers;

    foreach my $controller ($root->findnodes("/Envelope/VirtualSystem/VirtualHardwareSection/Item[rasd:ResourceType = $kind]")) {
        $controllers{_node_val($controller, 'rasd:Address/text()')} = _node_val($controller, 'rasd:InstanceID/text()');
    }

    return \%controllers;
}

sub _get_resources
{
    my ($root, $parent, $kind) = @_;
    return $root->findnodes("/Envelope/VirtualSystem/VirtualHardwareSection/Item[rasd:Parent = $parent and rasdResourceType = $kind");
}

sub _node_val
{
        my ($root, $xpath) = @_;

        my ($node) = $root->findnodes($xpath);
        return defined($node) ? $node->getNodeValue() : undef;
}

sub _numbers_to_letters
{
    my $input_number = shift;
    return 1 unless $input_number < 26;
    $input_number =~s/(\d+)/chr(97+$1)/e;
    return $input_number;
}


# to double check for errors

sub _verify_manifest
{
    my $self = shift;

    my ($mf_path) = glob($self->{extractdir}.'/*.mf');
    open(my $manifest, '<', $mf_path)
        or v2vdie __x('Failed to open {path}: {error}',
                      path => $mf_path, error => $!);

    my %files;
    while(my $line = <$manifest>) {
        my ($file, $sha1) = ($line=~/SHA1\((.*?)\)=\s*(.*?)\s*$/);
        $files{$file} = $sha1;
    }
    close($manifest);

    while (my ($file, $sha1_mf) = each(%files)) {
        open(my $fh, $self->{extractdir}.'/'.$file)
            or v2vdie __x('Manifest references non-existant file {name}',
                          name => $file);

        my $sha1 = Digest::SHA1->new;
        $sha1->addfile($fh);
        my $sha1_calc = $sha1->hexdigest;

        v2vdie __x('Checksum of {file} does not match manifest', file => $file)
            unless ($sha1_calc eq $sha1_mf);

        close $fh;
    }
}

1;
