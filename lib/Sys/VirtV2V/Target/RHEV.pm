# Sys::VirtV2V::Target::RHEV
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

package Sys::VirtV2V::Target::RHEV::UUIDHelper;

sub get_uuid
{
    my $uuidgen;
    open($uuidgen, '-|', 'uuidgen') or die("Unable to execute uuidgen: $!");

    my $uuid;
    while(<$uuidgen>) {
        chomp;
        $uuid = $_;
    }
    close($uuidgen) or die("uuidgen returned an error");

    return $uuid;
}

package Sys::VirtV2V::Target::RHEV::NFSHelper;

use Carp;
use File::Temp qw(tempfile);
use POSIX qw(:sys_wait_h setuid setgid);

use Sys::VirtV2V::UserMessage qw(user_message);

use Locale::TextDomain 'virt-v2v';

sub new
{
    my $class = shift;
    my ($sub) = @_;

    my $self = {};
    bless($self, $class);

    my ($tochild_read, $tochild_write);
    my ($fromchild_read, $fromchild_write);

    pipe($tochild_read, $tochild_write);
    pipe($fromchild_read, $fromchild_write);

    # Capture stderr to a file
    my ($stderr, undef) = tempfile(UNLINK => 1, SUFFIX => '.virt-v2v');
    $self->{stderr} = $stderr;

    my $pid = fork();
    if ($pid == 0) {
        # Close the ends of the pipes we don't need
        close($tochild_write);
        close($fromchild_read);

        # dup2() stdin and stdout with the communication pipes
        open(STDIN, "<&".fileno($tochild_read))
            or die("dup stdin failed: $!");
        open(STDOUT, ">&".fileno($fromchild_write))
            or die("dup stdout failed: $!");

        # Write stderr to our temp file
        open(STDERR, ">&".fileno($stderr))
            or die("dup stderr failed: $!");

        # Close the original file handles
        close($tochild_read);
        close($fromchild_write);

        # Set EUID and EGID to RHEV magic values 36:36
        # execute the wrapped function, trapping errors
        eval {
            setgid(36) or die("setgid failed: $!");
            setuid(36) or die("setuid failed: $!");

            &$sub();
        };

        # Don't exit, which would cause destructors to be called in the child.
        # Instead exec /bin/true or /bin/false as appropriate
        if ($@) {
            print $stderr $@;
            close($stderr);
            exec('/bin/false');
        }

        exec('/bin/true');
    } else {
        close($tochild_read);
        close($fromchild_write);
    }

    $self->{tochild} = $tochild_write;
    $self->{fromchild} = $fromchild_read;

    $self->{pid} = $pid;
    return $self;
}

sub check_exit
{
    my $self = shift;

    my $ret = waitpid($self->{pid}, 0);

    # If the process terminated normally, check the exit status and stderr
    if ($ret == $self->{pid}) {
        delete($self->{pid});

        # No error if the exit status was 0
        return if ($? == 0);

        # Otherwise return whatever went to stderr
        my $stderr = $self->{stderr};
        my $error = "";
        seek($stderr, 0, 0);
        while(<$stderr>) {
            $error .= $_;
        }
        die($error);
    }

    confess("Error waiting for child process");
}

sub DESTROY
{
    my $self = shift;

    # Make certain the child process dies with the object
    if (defined($self->{pid})) {
        kill(9, $self->{pid});
        waitpid($self->{pid}, WNOHANG);
    }
}

package Sys::VirtV2V::Target::RHEV::Vol;

use POSIX;

use Sys::VirtV2V::UserMessage qw(user_message);

use Locale::TextDomain 'virt-v2v';

our %vols_by_path;

sub _new
{
    my $class = shift;
    my ($mountdir, $domainuuid, $size) = @_;

    my $self = {};
    bless($self, $class);

    $self->{size} = $size;

    my $imageuuid = Sys::VirtV2V::Target::RHEV::UUIDHelper::get_uuid();
    my $voluuid   = Sys::VirtV2V::Target::RHEV::UUIDHelper::get_uuid();
    $self->{imageuuid}  = $imageuuid;
    $self->{voluuid}    = $voluuid;
    $self->{domainuuid} = $domainuuid;

    $self->{dir}  = "$mountdir/$domainuuid/images/$imageuuid";
    $self->{path} = $self->{dir}."/$voluuid";

    $self->{creation} = time();

    $vols_by_path{$self->{path}} = $self;

    return $self;
}

sub _get_by_path
{
    my $class = shift;
    my ($path) = @_;

    return $vols_by_path{$path};
}

sub _get_size
{
    my $self = shift;

    return $self->{size};
}

sub _get_imageuuid
{
    my $self = shift;

    return $self->{imageuuid};
}

sub _get_voluuid
{
    my $self = shift;

    return $self->{voluuid};
}

sub _get_creation
{
    my $self = shift;

    return $self->{creation};
}

sub get_path
{
    my $self = shift;

    return $self->{path};
}

sub get_format
{
    my $self = shift;

    return "raw";
}

sub is_block
{
    my $self = shift;

    return 0;
}

sub open
{
    my $self = shift;

    my $now = $self->{creation};

    $self->{writer} = Sys::VirtV2V::Target::RHEV::NFSHelper->new(sub {
        my $dir = $self->{dir};
        my $path = $self->{path};

        mkdir($dir)
            or die(user_message(__x("Failed to create directory {dir}: {error}",
                                    dir => $dir,
                                    error => $!)));

        # Write out the .meta file
        my $meta;
        open($meta, '>', "$path.meta")
            or die(__x("Unable to open {path} for writing: {error}",
                       path => "$path.meta",
                       error => $!));

        print $meta "DOMAIN=".$self->{domainuuid}."\n";
        print $meta "VOLTYPE=LEAF\n";
        print $meta "CTIME=$now\n";
        print $meta "FORMAT=RAW\n";
        print $meta "IMAGE=".$self->{imageuuid}."\n";
        print $meta "DISKTYPE=1\n";
        print $meta "PUUID=00000000-0000-0000-0000-000000000000\n";
        print $meta "LEGALITY=LEGAL\n";
        print $meta "MTIME=$now\n";
        print $meta "POOL_UUID=00000000-0000-0000-0000-000000000000\n";
        print $meta "SIZE=".ceil($self->{size}/1024)."\n";
        print $meta "TYPE=SPARSE\n";
        print $meta "DESCRIPTION=Exported by virt-v2v\n";
        print $meta "EOF\n";

        close($meta)
            or die(user_message(__x("Error closing {path}: {error}",
                                    path => "$path.meta",
                                    error => $!)));

        # Write the remainder of the data using dd in 2MB chunks
        # XXX - mbooth@redhat.com 06/04/2010 (Fedora 12 writing to RHEL 5 NFS)
        # Use direct IO as writing a large amount of data to NFS regularly
        # crashes my machine.  Using direct io crashes less.
        exec('dd', 'obs='.1024*1024*2, 'oflag=direct', 'of='.$path)
            or die("Unable to execute dd: $!");
    });
}

sub write
{
    my $self = shift;
    my ($data) = @_;

    defined($self->{writer}) or die("write called without open");

    unless(print {$self->{writer}->{tochild}} $data) {
        # This should only have failed if there was an error from the helper
        $self->{writer}->check_exit();

        # die() explicitly in case the above didn't
        die("Error writing to helper: $!");
    }
}

sub close
{
    my $self = shift;

    # Close the writer pipe, which will cause the child to exit
    close($self->{writer}->{tochild})
        or die("Error closing tochild pipe");

    # Wait for the child to exit
    $self->{writer}->check_exit();

    delete($self->{writer});
}

package Sys::VirtV2V::Target::RHEV;

use File::Temp qw(tempdir);
use Time::gmtime;

use Sys::VirtV2V::ExecHelper;
use Sys::VirtV2V::UserMessage qw(user_message);

use Locale::TextDomain 'virt-v2v';

=head1 NAME

Sys::VirtV2V::Target::RHEV - Output to a RHEV Export storage domain

=head1 SYNOPSIS

 use Sys::VirtV2V::Target::RHEV;

 my $target = new Sys::VirtV2V::Target::RHEV($domain_path);

=head1 DESCRIPTION

Sys::VirtV2V::Target::RHEV write the converted guest to a RHEV Export storage
domain. This can later be imported to RHEV by the user.

=head1 METHODS

=over

=item Sys::VirtV2V::Target::RHEV->new(domain_path)

Create a new Sys::VirtV2V::Target::RHEV object.

=over

=item domain_path

The NFS path to an initialised RHEV Export storage domain.

=back

=cut

sub new
{
    my $class = shift;
    my ($domain_path) = @_;

    my $self = {};
    bless($self, $class);

    die(user_message(__"You must be root to output to RHEV"))
        unless ($> == 0);

    my $mountdir = tempdir();

    # Needs to be read by 36:36
    chown(36, 36, $mountdir)
        or die(user_message(__x("Unable to change ownership of {mountdir} to ".
                                "36:36",
                                mountdir => $mountdir)));

    $self->{mountdir} = $mountdir;
    $self->{domain_path} = $domain_path;

    my $eh = Sys::VirtV2V::ExecHelper->run('mount', $domain_path, $mountdir);
    if ($eh->status() != 0) {
        die(user_message(__x("Failed to mount {path}. Command exited with ".
                             "status {status}. Output was: {output}",
                             path => $domain_path,
                             status => $eh->status(),
                             output => $eh->output())));
    }

    my $nfs = Sys::VirtV2V::Target::RHEV::NFSHelper->new(sub {
        opendir(my $dir, $mountdir)
            or die(user_message(__x("Unable to open {mountdir}: {error}",
                                    mountdir => $mountdir,
                                    error => $!)));

        foreach my $entry (readdir($dir)) {
            # return entries which look like uuids
            print "$entry\n"
                if ($entry =~ /^[0-9a-z]{8}-(?:[0-9a-z]{4}-){3}[0-9a-z]{12}$/);
        }
    });

    # Get the UUID of the storage domain
    my $domainuuid;
    my $fromchild = $nfs->{fromchild};
    while (<$fromchild>) {
        if (defined($domainuuid)) {
            die(user_message(__x("{mountdir} contains multiple possible ".
                                 "domains. It may only contain one.",
                                 mountdir => $mountdir)));
        }
        chomp;
        $domainuuid = $_;
    }

    $nfs->check_exit();

    if (!defined($domainuuid)) {
        die(user_message(__x("{mountdir} does not contain an initialised ".
                             "storage domain",
                             mountdir => $mountdir)));
    }

    $self->{domainuuid} = $domainuuid;

    return $self;
}

sub DESTROY
{
    my $self = shift;

    my $eh = Sys::VirtV2V::ExecHelper->run('umount', $self->{mountdir});
    if ($eh->status() != 0) {
        print STDERR user_message(__x("Failed to unmount {path}. Command ".
                                      "exited with status {status}. Output ".
                                      "was: {output}",
                                      path => $self->{domain_path},
                                      status => $eh->status(),
                                      output => $eh->output()));
    }

    rmdir($self->{mountdir})
        or print STDERR user_message(__x("Failed to remove mount directory ".
                                         "{dir}: {error}",
                                         dir => $self->{mountdir},
                                         error => $!));
}

=item create_volume(name, size)

Create a new volume in the export storage domain

=over

=item name

The name of the volume which is being created.

=item size

The size of the volume which is being created in bytes.

=back

create_volume() returns a Sys::VirtV2V::Target::RHEV::Vol object.

=cut

sub create_volume
{
    my $self = shift;
    my ($name, $size) = @_;

    return Sys::VirtV2V::Target::RHEV::Vol->_new($self->{mountdir},
                                                 $self->{domainuuid},
                                                 $size);
}

=item volume_exists (name)

Check if volume I<name> exists in the target storage domain.

Always returns 0, as RHEV storage domains don't have names

=cut

sub volume_exists
{
    my $self = shift;
    my ($name) = @_;

    return 0;
}

=item get_volume (name)

Not defined for RHEV output

=cut

sub get_volume
{
    my $self = shift;
    my ($name) = @_;

    die("Cannot retrieve an existing RHEV storage volume by name");
}

=item create_guest(dom)

Create the guest in the target

=cut

sub create_guest
{
    my $self = shift;
    my ($dom, $guestcaps) = @_;

    # Get the name of the guest
    my ($name) = $dom->findnodes('/domain/name/text()');
    $name = $name->getNodeValue();

    # Get the number of virtual cpus
    my ($ncpus) = $dom->findnodes('/domain/vcpu/text()');
    $ncpus = $ncpus->getNodeValue();

    # Get the amount of memory in MB
    my ($memsize) = $dom->findnodes('/domain/memory/text()');
    $memsize = $memsize->getNodeValue();
    $memsize = int($memsize / 1024);

    # Generate a creation date
    my $now = gmtime();
    my $vmcreation = sprintf("%02d/%02d/%d %02d:%02d:%02d",
                             $now->mday(), $now->mon() + 1, $now->year() + 1900,
                             $now->hour(), $now->min(), $now->sec());

    my $osuuid = Sys::VirtV2V::Target::RHEV::UUIDHelper::get_uuid();

    my $ovf = new XML::DOM::Parser->parse(<<EOF);
<ovf:Envelope
    xmlns:rasd="http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/CIM_ResourceAllocationSettingData"
    xmlns:vssd="http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/CIM_VirtualSystemSettingData"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:ovf="http://schemas.dmtf.org/ovf/envelope/1/"
    ovf:version="0.9">

    <References/>

    <Section xsi:type="ovf:NetworkSection_Type">
        <Info>List of networks</Info>
    </Section>

    <Section xsi:type="ovf:DiskSection_Type">
        <Info>List of Virtual Disks</Info>
    </Section>

    <Content ovf:id="out" xsi:type="ovf:VirtualSystem_Type">
        <Name>$name</Name>
        <TemplateId>00000000-0000-0000-0000-000000000000</TemplateId>
        <TemplateName>Blank</TemplateName>
        <Description>Imported with virt-v2v</Description>
        <Domain/>
        <CreationDate>$vmcreation</CreationDate>
        <IsInitilized>True</IsInitilized>
        <IsAutoSuspend>False</IsAutoSuspend>
        <TimeZone/>
        <IsStateless>False</IsStateless>
        <Origin>0</Origin>
        <VmType>1</VmType>
        <DefaultDisplayType>0</DefaultDisplayType>

        <Section ovf:id="$osuuid" ovf:required="false" xsi:type="ovf:OperatingSystemSection_Type">
          <Info>Guest Operating System</Info>
          <Description>Unassigned</Description>
        </Section>

        <Section xsi:type="ovf:VirtualHardwareSection_Type">
            <Info>$ncpus CPU, $memsize Memory</Info>
            <Item>
                <rasd:Caption>$ncpus virtual cpu</rasd:Caption>
                <rasd:Description>Number of virtual CPU</rasd:Description>
                <rasd:InstanceId>1</rasd:InstanceId>
                <rasd:ResourceType>3</rasd:ResourceType>
                <rasd:num_of_sockets>$ncpus</rasd:num_of_sockets>
                <rasd:cpu_per_socket>1</rasd:cpu_per_socket>
            </Item>
            <Item>
                <rasd:Caption>$memsize MB of memory</rasd:Caption>
                <rasd:Description>Memory Size</rasd:Description>
                <rasd:InstanceId>2</rasd:InstanceId>
                <rasd:ResourceType>4</rasd:ResourceType>
                <rasd:AllocationUnits>MegaBytes</rasd:AllocationUnits>
                <rasd:VirtualQuantity>$memsize</rasd:VirtualQuantity>
            </Item>
            <Item>
                <rasd:Caption>USB Controller</rasd:Caption>
                <rasd:InstanceId>4</rasd:InstanceId>
                <rasd:ResourceType>23</rasd:ResourceType>
                <rasd:UsbPolicy>Disabled</rasd:UsbPolicy>
            </Item>
            <Item>
                <rasd:Caption>Graphical Controller</rasd:Caption>
                <rasd:InstanceId>5</rasd:InstanceId>
                <rasd:ResourceType>20</rasd:ResourceType>
                <rasd:VirtualQuantity>1</rasd:VirtualQuantity>
            </Item>
        </Section>
    </Content>
</ovf:Envelope>
EOF

    $self->_disks($ovf, $dom);
    $self->_networks($ovf, $dom);

    my $nfs = Sys::VirtV2V::Target::RHEV::NFSHelper->new(sub {
        my $mountdir = $self->{mountdir};
        my $domainuuid = $self->{domainuuid};

        my $vmuuid = Sys::VirtV2V::Target::RHEV::UUIDHelper::get_uuid();

        my $dir = $mountdir.'/'.$domainuuid.'/master/vms/'.$vmuuid;
        mkdir($dir)
            or die(user_message(__x("Failed to create directory {dir}: {error}",
                                    dir => $dir,
                                    error => $!)));

        my $vm;
        my $ovfpath = $dir.'/'.$vmuuid.'.ovf';
        open($vm, '>', $ovfpath)
            or die(user_message(__x("Unable to open {path} for writing: ".
                                    "{error}",
                                    path => $ovfpath,
                                    error => $!)));

        print $vm $ovf->toString();
    });
    $nfs->check_exit();
}

sub _disks
{
    my $self = shift;
    my ($ovf, $dom) = @_;

    my ($references) = $ovf->findnodes('/ovf:Envelope/References');
    die("no references") unless (defined($references));

    my ($disksection) = $ovf->findnodes("/ovf:Envelope/Section".
                                      "[\@xsi:type = 'ovf:DiskSection_Type']");
    die("no disksection") unless (defined($disksection));

    my ($virtualhardware) =
        $ovf->findnodes("/ovf:Envelope/Content/Section".
                        "[\@xsi:type = 'ovf:VirtualHardwareSection_Type'");
    die("no virtualhardware") unless (defined($virtualhardware));

    my $driveno = 1;

    foreach my $disk
        ($dom->findnodes("/domain/devices/disk[\@device='disk']"))
    {
        my ($path) = $disk->findnodes('source/@file');
        $path = $path->getNodeValue();

        my ($bus) = $disk->findnodes('target/@bus');
        $bus = $bus->getNodeValue();

        my $vol = Sys::VirtV2V::Target::RHEV::Vol->_get_by_path($path);

        die("dom contains path not written by virt-v2v: $path\n".
            $dom->toString()) unless (defined($vol));

        my $fileref = $vol->_get_imageuuid().'/'.$vol->_get_voluuid();
        my $size_gb = int($vol->_get_size()/1024/1024/1024);

        # Add disk to References
        my $file = $ovf->createElement("File");
        $references->appendChild($file);

        $file->setAttribute('ovf:href', $fileref);
        $file->setAttribute('ovf:id', $vol->_get_voluuid());
        $file->setAttribute('ovf:size', $vol->_get_size());
        $file->setAttribute('ovf:description', 'imported by virt-v2v');

        # Add disk to DiskSection
        my $diske = $ovf->createElement("Disk");
        $disksection->appendChild($diske);

        $diske->setAttribute('ovf:diskId', $vol->_get_voluuid());
        $diske->setAttribute('ovf:size', $size_gb);
        $diske->setAttribute('ovf:actual_size', $size_gb);
        $diske->setAttribute('ovf:fileRef', $fileref);
        $diske->setAttribute('ovf:parentRef', '');
        $diske->setAttribute('ovf:vm_snapshot_id',
                             '00000000-0000-0000-0000-000000000000');
        $diske->setAttribute('ovf:volume-format', 'RAW');
        $diske->setAttribute('ovf:volume-type', 'Sparse');
        $diske->setAttribute('ovf:format', 'http://en.wikipedia.org/wiki/Byte');
        # IDE = 0, SCSI = 1, VirtIO = 2
        $diske->setAttribute('ovf:disk-interface', $bus eq 'virtio' ? 2 : 0);

        # Add disk to VirtualHardware
        my $item = $ovf->createElement('Item');
        $virtualhardware->appendChild($item);

        my $e;
        $e = $ovf->createElement('rasd:Caption');
        $e->addText("Drive $driveno"); # This text MUST begin with the string
                                       # 'Drive ' or the file will not parse
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:InstanceId');
        $e->addText($vol->_get_voluuid());
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:ResourceType');
        $e->addText('17');
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:HostResource');
        $e->addText($fileref);
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:Parent');
        $e->addText('00000000-0000-0000-0000-000000000000');
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:Template');
        $e->addText('00000000-0000-0000-0000-000000000000');
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:ApplicationList');
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:StorageId');
        $e->addText($self->{domainuuid});
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:StoragePoolId');
        $e->addText('00000000-0000-0000-0000-000000000000');
        $item->appendChild($e);

        my $volcreation = gmtime($vol->_get_creation());
        my $voldate = sprintf("%02d/%02d/%d %02d:%02d:%02d",
                              $volcreation->mday(), $volcreation->mon() + 1,
                              $volcreation->year() + 1900, $volcreation->hour(),
                              $volcreation->min(), $volcreation->sec());

        $e = $ovf->createElement('rasd:CreationDate');
        $e->addText($voldate);
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:LastModified');
        $e->addText($voldate);
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:last_modified_date');
        $e->addText($voldate);
        $item->appendChild($e);

        $driveno++;
    }
}

sub _networks
{
    my $self = shift;
    my ($ovf, $dom) = @_;

    my ($networksection) = $ovf->findnodes("/ovf:Envelope/Section".
                                    "[\@xsi:type = 'ovf:NetworkSection_Type']");

    my ($virtualhardware) =
        $ovf->findnodes("/ovf:Envelope/Content/Section".
                        "[\@xsi:type = 'ovf:VirtualHardwareSection_Type'");
    die("no virtualhardware") unless (defined($virtualhardware));

    my $i = 0;

    foreach my $if
        ($dom->findnodes('/domain/devices/interface'))
    {
        # Extract relevant info about this NIC
        my $type = $if->getAttribute('type');

        my $name;
        if ($type eq 'bridge') {
            ($name) = $if->findnodes('source/@bridge');
        } elsif ($type eq 'network') {
            ($name) = $if->findnodes('source/@network');
        } else {
            # Should have been picked up in Converter
            die("Unknown interface type");
        }
        $name = $name->getNodeValue();

        my ($driver) = $if->findnodes('model/@type');
        $driver &&= $driver->getNodeValue();

        my ($mac) = $if->findnodes('mac/@address');
        $mac &&= $mac->getNodeValue();

        my $dev = "eth$i";

        my $e = $ovf->createElement("Network");
        $e->setAttribute('ovf:name', $name);
        $networksection->appendChild($e);

        my $item = $ovf->createElement('Item');
        $virtualhardware->appendChild($item);

        $e = $ovf->createElement('rasd:Caption');
        $e->addText("Ethernet adapter on $name");
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:InstanceId');
        $e->addText('3');
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:ResourceType');
        $e->addText('10');
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:ResourceSubType');
        if ($driver eq 'rtl8139') {
            $e->addText('1');
        } elsif ($driver eq 'e1000') {
            $e->addText('2');
        } elsif ($driver eq 'virtio') {
            $e->addText('3');
        } else {
            print STDERR (user_message(__x("Unknown NIC model {driver} for ".
                                           "{dev}. NIC will be {default} ".
                                           "when imported",
                                           driver => $driver,
                                           dev => $dev,
                                           default => 'e1000')));
            $e->addText('1');
        }
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:Connection');
        $e->addText($name);
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:Name');
        $e->addText($dev);
        $item->appendChild($e);

        $e = $ovf->createElement('rasd:MACAddress');
        $e->addText($mac) if (defined($mac));
        $item->appendChild($e);

        $i++;
    }
}

=back

=head1 COPYRIGHT

Copyright (C) 2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING for the full license.

=cut

1;
