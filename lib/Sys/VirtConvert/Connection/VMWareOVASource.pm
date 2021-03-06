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

use File::stat;
use Sys::Virt;
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
    my($filename, $target) = @_;

    my $self = {};
    $self->{filename} = $filename;

    my $virtual_machine = (split('.',basename($filename),2))[0];
    $self->{path} = ENV{"TEMP_DIR"}.$virtual_machine;
    bless($self, $class);

    $self->_uncompress_archive($filename);
    $self->_verify_md5();
    $self->_get_meta();
    return $self;
}

sub _uncompress_archive
{
    my $self = shift;
    my $path = @_;
    my $tmp_directory = $ENV{"TEMP_DIR"};
    my $ae = Archive::Extract(archive => $path);
    unless(-d $tmp_directory) {
        mkdir $tmp_directory or die;
    }
    $ae->extract(to=> $tmp_directory.basename($self->path) );

#    return
}

sub _get_meta
{
    my $self = shift;

    my $ovf = $self->{path}.'.ovf';
    my $xml;
    open($xml, '<', $ovf)
        or v2vdie __x('Failed to open {ovf} for reading', ovf=>$ovf);
}

sub _parse_dom
{
    my ($source, $dom) = @_;
    my %meta;

    my $root=$dom->getDocumentElement();
    $meta{name} = _node_val($root,'/Envelope/VirtualSystem/Name/text()');
    $meta{memory} = _node_val($root,"/Envelope/VirtualSystem/VirtualHardwareSection/Item/VirtualQuantity[../rasd:ResourceType = $hw_families{Memory}");
    $meta{cpus} = _node_val($root, "/Envelope/VirtualSystem/VirtualHardwareSection/Item/VirtualQuantity[../rasd:ResourceType = $hw_families{Cpu}");

    # return vmx-08 that is vmware esxi 5.0
    $meta{src_type} = _node_val($root, "/Envelope/VirtualSystem/VirtualHardwareSection/System/vssd:VirtualSystemType/text()");

    $meta{features} = []; # TBD: not sure how to gather them from OVF yet

    $meta{disks} = [];

    #iterate on scsi controllers Id

    my $scsi_controllers = _collect_controllers($root, $hw_families{'SCSI Controller'});

    for my $scsi_controller (sort keys %$scsi_controllers) {
        my %info;
        foreach my $disk (_get_resources($root,$scsi_controllers->{$scsi_controller}, $hw_families{'Disk Drive'})){
            my $hd_number = _node_val($disk,"rasd:AddressOnParent/text()") ;
            $info{device} = "sd".numbers_to_letters($hd_number); #transformation from numbers to letters
            my $disk_reference=(split("/", _node_val($disk,"rasd:HostResource/text()")))[-1];
            my $disk_name=_node_val($root,'/Envelope/References/File[contains(@ovf:id,/Envelope/DiskSection/Disk[contains(@ovf:diskId,"'.$disk_reference.'")]/@ovf:fileRef )]/@ovf:href');
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
            my $hd_number = _node_val($disk,"rasd:AddressOnParent/text()") ;
            $info{device} = "sd".numbers_to_letters($hd_number); #transformation from numbers to letters
            my $disk_reference=(split("/", _node_val($disk,"rasd:HostResource/text()")))[-1];
            my $disk_name=_node_val($root,'/Envelope/References/File[contains(@ovf:id,/Envelope/DiskSection/Disk[contains(@ovf:diskId,"'.$disk_reference.'")]/@ovf:fileRef )]/@ovf:href');
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

    foreach my $nic (_collect_controllers($root,$hw_families{'Ethernet'} ) ) {
        my %info;

        $info{mac} = ""; # it's assigned automatically by the hypervisor
        $info{vnet} = "";    # not clear how to get it from the ovf
        $info{vnet_type} = ""; #not clear how to get it from the ovf
        push(@{$meta{nics}}, \%info);
    }


}

sub _collect_controllers
{
    my ($root,$kind) = @_;

    my %controllers;

    foreach my $controller ($root->findnodes("/Envelope/VirtualSystem/VirtualHardwareSection/Item[rasd:ResourceType = $kind ] ")) {
        $controllers{_node_val($controller, 'rasd:Address/text()')}=_node_val($controller, 'rasd:InstanceID/text()');
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

    my $mf_path = $self->{path}.$self->{filename}.'.mf';
    my $manifest;
    open($manifest, '<', $mf_path)
        or v2vdie __x('Failed to open {path} {filename}: {error}',
                      path => $self->{path}, filename => $self->{filename},
                      error => $!);

    my %files;
    while(my $line=<$manifest>) {
        my ($file,$sha1) = ($line=~/\((.*?)\)=\s(.*?)$/);
        $files{$file}=$sha1;
    }
    close($manifest);

    while (my ($file,$checksum) = each(%files)) {
        my $fh;
        open($fh, $file);
        my $sha1 = Digest::SHA1->new;
        $sha1->addfile($fh);

        unless ($sha1->hexdigest == $checksum) {
            v2vdie("The checksum on {file} as failed", file=>$file);
        }
        close $fh;
    }
}

1;
