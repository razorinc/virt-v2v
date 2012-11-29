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

=pod

=head1 NAME

Sys::VirtConvert::Connection::VMWareOVASource - Read from a VMware OVA file

=head1 METHODS

=over

=cut

# Hardware profiles for Hardware Resources in OVF file

our %hw_families = (
    'CPU'               => 3,
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

=item new(path)

Create a new VMWareOVASource connection. I<path> must be the path to an OVA
file.

=cut


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
    my $dom = new XML::DOM::Parser->parsefile($ovf)
        or v2vdie __x('Failed to open {ovf} for reading', ovf => $ovf);
    my $root = $dom->getDocumentElement();

    my %meta;
    $self->{meta} = \%meta;

    $meta{name} = _node_val($root, '/Envelope/VirtualSystem/Name/text()');
    my ($memory) = $root->findnodes("/Envelope/VirtualSystem/VirtualHardwareSection/Item[rasd:ResourceType = $hw_families{Memory}]");
    if (defined($memory)) {
        my $units = _node_val($memory, 'rasd:AllocationUnits/text()');
        $units =~ /^byte \* 2\^(\d+)$/ or die "Unexpected memory units: $units";
        $units = 2 ** $1;
        $memory = _node_val($memory, 'rasd:VirtualQuantity/text()') * $units;
    } else {
        $memory = 1 * 1024 * 1024 * 1024; # Shouldn't happen, but default to 1GB RAM
    }
    $meta{memory} = $memory;

    $meta{cpus} = _node_val($root, "/Envelope/VirtualSystem/VirtualHardwareSection/Item[rasd:ResourceType = $hw_families{CPU}]/VirtualQuantity");
    $meta{cpus} ||= 1; # Shouldn't happen, but default to 1 CPU

    # return vmx-08 that is vmware esxi 5.0
    $meta{src_type} = _node_val($root, "/Envelope/VirtualSystem/VirtualHardwareSection/System/vssd:VirtualSystemType/text()");

    $meta{features} = []; # TBD: not sure how to gather them from OVF yet

    $meta{disks} = [];
    $meta{removables} = [];

    #iterate on scsi controllers Id

    my $scsi_controllers = _collect_controllers($root, $hw_families{'SCSI Controller'});
    my $scsi_letter = 'a';
    for my $scsi_controller (sort keys %$scsi_controllers) {
        foreach my $disk (_get_resources($root, $scsi_controllers->{$scsi_controller})) {
            my %info;
            $info{device} = 'sd'.$scsi_letter++;

            my $resource_type = _node_val($disk, 'rasd:ResourceType/text()');
            if ($resource_type eq $hw_families{'Disk Drive'}) {
                $info{src} = $self->_get_volume($root, $disk);
                push(@{$meta{disks}}, \%info);
            }

            elsif ($resource_type eq $hw_families{'CD Drive'}) {
                $info{type} = 'cdrom';
                push(@{$meta{removables}}, \%info);
            }
        }
    }

    my $ide_controllers = _collect_controllers($root, $hw_families{'IDE Controller'});
    my $ide_letter = 'a';
    for my $ide_controller (sort keys %$ide_controllers) {
        foreach my $disk (_get_resources($root, $ide_controllers->{$ide_controller}, $hw_families{'Disk Drive'})) {
            my %info;
            $info{device} = 'hd'.$ide_letter++;

            my $resource_type = _node_val($disk, 'rasd:ResourceType/text()');
            if ($resource_type eq $hw_families{'Disk Drive'}) {
                $info{src} = $self->_get_volume($root, $disk);
                push(@{$meta{disks}}, \%info);
            }

            elsif ($resource_type eq $hw_families{'CD Drive'}) {
                $info{type} = 'cdrom';
                push(@{$meta{removables}}, \%info);
            }
        }
    }

    $meta{nics} = [];

    foreach my $nic ($root->findnodes("/Envelope/VirtualSystem/VirtualHardwareSection/Item[rasd:ResourceType = $hw_families{Ethernet}]")) {
        my %info;

        $info{mac} = undef; # it's assigned automatically by the hypervisor
        $info{vnet} = _node_val($nic, 'rasd:Connection/text()');    # not clear how to get it from the ovf
        $info{vnet_type} = _node_val($nic, 'rasd:ResourceSubType/text()');
        push(@{$meta{nics}}, \%info);
    }
}

sub _collect_controllers
{
    my ($root, $kind) = @_;

    my %controllers;

    foreach my $controller ($root->findnodes("/Envelope/VirtualSystem/VirtualHardwareSection/Item[rasd:ResourceType = $kind]")) {
        my $address = _node_val($controller, 'rasd:Address/text()');
        my $instanceID = _node_val($controller, 'rasd:InstanceID/text()');
        $address ||= '0'; # Item doesn't have rasd:Address if there's only 1

        $controllers{$address} = $instanceID;
    }

    return \%controllers;
}

sub _get_resources
{
    my ($root, $parent) = @_;
    return sort {_node_val($a, 'rasd:AddressOnParent/text()') <=> _node_val($b, 'rasd:AddressOnParent/text()')}
                $root->findnodes("/Envelope/VirtualSystem/VirtualHardwareSection/Item[rasd:Parent = $parent");
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

sub _get_volume
{
    my $self = shift;
    my ($root, $item) = @_;

    my $host_resource = _node_val($item, 'rasd:HostResource/text()');
    $host_resource =~ m{^ovf:/disk/(.*)} or die "Unexpected host_resource: $host_resource";
    my $disk_name = $1;

    my ($disk) = $root->findnodes("/Envelope/DiskSection/Disk[\@ovf:diskId = '$disk_name']");
    die "No Disk section for $disk_name" unless defined($disk);
    my $fileRef = _node_val($disk, '@ovf:fileRef');
    die "No fileRef for Disk $disk_name" unless defined($fileRef);

    my ($file) = $root->findnodes("/Envelope/References/File[\@ovf:id = '$fileRef']");
    die "No File section for fileRef $fileRef" unless defined($file);

    my $name = _node_val($file, '@ovf:href');
    die "No href for file $fileRef" unless defined($name);

    my $path = $self->{extractdir}."/$name";

    die __x("Guest disk image {path} is not readable.\n", path => $path)
        unless (-r $path);

    my $format = 'vmdk';
    my $usage = _node_val($disk, '@ovf:populatedSize');

    my $units = _node_val($disk, '@ovf:capacityAllocationUnits');
    $units =~ /^byte \* 2\^(\d+)$/
        or die "Unexpected ovf:capacityAllocationUnits: $units";
    $units = 2 ** $1;
    my $size = _node_val($disk, '@ovf:capacity') * $units;

    my $is_block = 0;
    my $is_sparse = $size > $usage ? 1 : 0;

    my $transfer = new Sys::VirtConvert::Transfer::Local($path, $is_block,
                                                         $format, $is_sparse);

    return new Sys::VirtConvert::Connection::Volume($name, $format, $path,
                                                    $size, $usage,
                                                    $is_sparse, $is_block,
                                                    $transfer);
}

=back

=head1 COPYRIGHT

Copyright (C) 2012 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtConvert::Connection::Source(3pm)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
