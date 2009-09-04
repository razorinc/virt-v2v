#!/usr/bin/perl
# virt-snapshot
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

use File::Temp qw(tempfile);
use Getopt::Long;
use Pod::Usage;

use Locale::TextDomain 'virt-v2v';

use Sys::Virt;

use Sys::VirtV2V;
use Sys::VirtV2V::MetadataReader;

=encoding utf8

=head1 NAME

virt-snapshot - Convert a guest to use a qcow2 snapshot for storage

=head1 SYNOPSIS

 virt-snapshot guest-domain

 virt-snapshot --rollback guest-domain

 virt-snapshot --commit guest-domain

=head1 DESCRIPTION

Virt-snapshot creates a qcow2 snapshot for all a guest's block devices and
updates the guest's libvirt domain to use them.

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

my $input = "libvirt";

=item B<--input input> | B<-i input>

The specified guest description uses the given I<input format>. The default is
C<libvirt>. Supported options are:

=over

=item I<libvirt>

Guest argument is the name of a libvirt domain.

=item I<libvirtxml>

Guest argument is the path to an XML file describing a libvirt domain.

=back

=cut

my $outputxml;

=item B<--outputxml file> | B<-o file>

Write the updated domain xml to I<file> instead of directly creating the domain.
If I<file> is '-' output will be to standard out.

This option can be useful when snapshotting or rolling back to domain XML which
can't be created on the local hypervisor, for example because Xen is not
available locally.

=cut

use constant DEFAULT_LIBDIR => '/var/lib/virt-snapshot';

my $snapshotdir = DEFAULT_LIBDIR.'/snapshots';

=item B<--snapshotdir dir> | B<-s dir>

The directory where snapshot files will be created. This defaults to
I</var/lib/virt-snapshot/snapshots>.

=cut

my $xmldir = DEFAULT_LIBDIR.'/xml';

=item B<--xmldir dir> | B<-x dir>

The directory where XML backups will be stored. This defaults to
I</var/lib/virt-snapshot/xml>.

=cut

my $force = 0;

=item B<--force> | B<-f>

Force an action to complete which might not be safe. Force is required to:

=over

Create a snapshot of a guest which already has an active snapshot (overwrites
the XML backup).

Rollback a guest which has no XML backup (all guest metadata is lost).

=back

=cut

my $commit = 0;

=item B<--commit>

Commit the existing snapshot to its backing store and update the guest to using
the original storage.

=cut

my $rollback = 0;

=item B<--rollback>

Remove the snapshot and restore the guest to its previous, unmodified storage.

=back

=cut

GetOptions ("help|?"          => \$help,
            "version"         => \$version,
            "connect|c=s"     => \$uri,
            "input|i=s"       => \$input,
            "outputxml|o=s"   => \$outputxml,
            "snapshotdir|s=s" => \$snapshotdir,
            "xmldir|x=s"      => \$xmldir,
            "force|f"         => \$force,
            "commit"          => \$commit,
            "rollback"        => \$rollback
    ) or pod2usage(2);
pod2usage(0) if($help);
pod2usage({
    -message => __"--commit and --rollback are mutually exclusive",
    -exitval => 1
}) if($commit && $rollback);

if ($version) {
    print "$Sys::VirtV2V::VERSION\n";
    exit(0);
}

pod2usage(_user_msg(__"no guest argument given")) if @ARGV == 0;

# Get an appropriate MetadataReader
my $mdr = Sys::VirtV2V::MetadataReader->instantiate($input, {});
if(!defined($mdr)) {
    print STDERR _user_msg(__x("{input} is not a valid input format",
                                input => $input));
    exit(1);
}

$mdr->handle_arguments(@ARGV);

exit(1) unless($mdr->is_configured());

###############################################################################
## Start of processing

my @vmm_params = (auth => 1);
push(@vmm_params, uri => $uri) if(defined($uri));
my $vmm = Sys::Virt->new(@vmm_params);

# Get a libvirt configuration for the guest
my $dom = $mdr->get_dom($vmm);
exit(1) unless(defined($dom));

my $pool = _get_pool($vmm);

if($commit) {
    _commit_guest($dom, $vmm, $pool) == 0 or exit(1);
}

elsif($rollback) {
    my $retval;
    ($retval, $dom) = _rollback_guest($dom, $vmm, $pool);

    exit(1) unless($retval == 0);
}

# No commit or rollback. Snapshot the guest
else {
    _snapshot_guest($dom, $vmm, $pool) == 0 or exit(1);
}

# Don't try to output anything if the domain is no longer defined
if(!defined($dom)) {
    print _user_msg(__"No resulting domain");
    exit($force == 1 ? 0 : 1);
}

# If --outputxml was given, just write the xml instead of creating the domain
if($outputxml) {
    my $out;
    my $error = 0;

    # Write output to a file
    if('-' ne $outputxml) {
        unless(open($out, '>', $outputxml)) {
            print STDERR _user_msg(__x("Unable to open {file}: {error}",
                                       file => $outputxml,
                                       error => $!));
            ($out, $outputxml) = tempfile(_get_guest_name($dom).'-XXXXXX',
                                          SUFFIX => '.xml');
            $error = 1;
        }

        print $out $dom->toString();

        close($out) or die(__x("Error closing {file}: {error}",
                               file => $outputxml, error => $!));

        if($error) {
            print STDERR _user_msg(__x("Wrote output to {file}",
                                       file => $outputxml));
            exit(1);
        }
    }

    # Write output to STDOUT
    else {
        print $dom->toString();
    }
}

else {
    eval {
        $vmm->define_domain($dom->toString());
    };

    if($@) {
        print STDERR _user_msg(__x("Unable to create guest: {error}",
                                   error => $@->stringify()));
        print STDERR _user_msg(__"Consider using the --outputxml option");

        # Write the output to a temporary file
        my ($out, $outputxml) = tempfile(_get_guest_name($dom).'-XXXXXX',
                                         SUFFIX => '.xml');

        print $out $dom->toString();

        close($out) or die(__x("Error closing {file}: {error}",
                               file => $outputxml, error => $!));

        print STDERR _user_msg(__x("Wrote output to {file}",
                                   file => $outputxml));

        exit(1);
    }
}

###############################################################################
## Helper functions

sub _get_pool
{
    my ($vmm) = @_;

    # Look for the virt-snapshot storage pool
    my $pool;
    eval {
        $pool = $vmm->get_storage_pool_by_name('virt-snapshot');
    };

    # If it wasn't there, try creating it
    if($@) {
        eval {
            $pool = $vmm->create_storage_pool("
                <pool type='dir'>
                    <name>virt-snapshot</name>
                    <target>
                        <path>/var/lib/virt-snapshot/images</path>
                    </target>
                </pool>
            ");
        };

        # If that didn't work, give up
        if($@) {
            print STDERR _user_msg(__x("Unable to create virt-snapshot ".
                                       "storage pool: {error}",
                                       error => $@->stringify()))
                unless(defined($pool));
            exit(1);
        }
    }

    # Check that the pool is usable
    my $pool_info = $pool->get_info();

    # If it's inactive, start it
    if($pool_info->{state} == Sys::Virt::StoragePool::STATE_INACTIVE) {
        eval {
            $pool->create();
        };

        if($@) {
            print STDERR _user_msg(__x("Unable to start virt-snapshot ".
                                       "storage pool: {error}",
                                       error => $@->stringify()))
                unless(defined($pool));
            exit(1);
        }
    }

    # If it's building, there's nothing to do but wait
    elsif($pool_info->{state} == Sys::Virt::StoragePool::STATE_BUILDING) {
        print STDERR _user_msg(__("virt-snapshot storage pool is temporarily ".
                                  "unavailable"));
        exit(1);
    }

    return $pool;
}

sub _commit_guest
{
    my ($dom, $vmm, $pool) = @_;

    # First, get a list of existing disks
    foreach my $disk ($dom->findnodes('/domain/devices/disk')) {
        my ($source) = $disk->findnodes('source');
        my ($target) = $disk->findnodes('target/@dev');

        # Look for the source location
        my $path;
        my $src_attrs = $source->getAttributes();
        foreach my $attr qw(dev file) {
            my $item = $src_attrs->getNamedItem($attr);
            if(defined($item)) {
                $path = $item->getNodeValue();

                # Remove the attribute. We'll add a new one in below.
                $src_attrs->removeNamedItem($attr);

                last;
            }
        }

        # Find the storage volume, which will include information on the backing
        # store
        my $vol;
        my $refreshed = 0;
        do {
            # XXX: Shouldn't be using an undocumented API
            # See RHBZ 519647. Replace with lookupByPath when it's available.
            eval {
                $vol = Sys::Virt::StorageVol->_new(path => $path,
                                                   connection => $vmm);
            };

            if($@) {
                if($refreshed) {
                    my $pool_xml = $pool->get_xml_description();
                    my $pool_dom = new XML::DOM::Parser->parse($pool_xml);

                    my ($pool_path) =
                        $pool_dom->findnodes('/pool/target/path/text()');

                    my $msg = __x("Unable to find {path} in the virt-snapshot ".
                                 "storage pool.", path => $path)."\n";

                    if($pool_path) {
                        $msg .= __x("Try moving it to {path} and retrying",
                                   path => $pool_path->toString());
                    } else {
                        $msg .= __x("Try deleting the virt-snapshot storage ".
                                   "pool and moving it to {path}.",
                                   path => '/var/lib/virt-snapshot');
                    }

                    print STDERR _user_msg($msg);
                    return -1;
                } else {
                    $pool->refresh(0);
                    $refreshed = 1;
                }
            }
        } until(defined($vol));

        # Get the volume's backing store
        my $vol_xml = $vol->get_xml_description();
        my $vol_dom = new XML::DOM::Parser->parse($vol_xml);
        my ($backing_store) = $vol_dom->findnodes('/volume/backingStore');

        # Skip it if it doesn't have a backing store
        unless($backing_store) {
            print STDERR _user_msg(_x("Skipping device {target} as it doesn't ".
                                      "have a backing store",
                                      target => $target->getNodeValue()));
            next;
        }

        my ($backing_path) = $backing_store->findnodes('path/text()');
        $backing_path = $backing_path->getNodeValue();
        my ($backing_format) = $backing_store->findnodes('format/@type');
        $backing_format = $backing_format->getNodeValue();

        # Try to work out if the backing store is a file or a block device by
        # interrogating its storage volume object
        my $backing_type;
        eval {
            # XXX: See comment above about this usage of _new
            my $backing_vol = Sys::Virt::StorageVol->_new(path => $path,
                                                          connection => $vmm);
            $backing_type = $backing_vol->get_info()->{type};
        };

        # Backing store isn't in a storage pool
        if($@) {
            # Guess based on path name
            # N.B. We could stat it, but that wouldn't work for a remote
            # connection
            if($backing_path =~ m{^/dev/}) {
                $backing_type = Sys::Virt::StorageVol::TYPE_BLOCK;
            } else {
                $backing_type = Sys::Virt::StorageVol::TYPE_FILE;
            }
        }

        # Update the domain XML with the location of the backing store
        if($backing_type == Sys::Virt::StorageVol::TYPE_BLOCK) {
            $source->setAttribute('dev', $backing_path);
        } else {
            $source->setAttribute('file', $backing_path);
        }

        # Update the domain XML with with a driver appropriate to the backing
        # store
        my ($driver) = $disk->findnodes('driver');

        # Initialise the driver if it's not set
        $driver ||= $disk->appendChild($dom->createElement('driver'));

        $driver->setAttribute('name', 'qemu');
        $driver->setAttribute('type', $backing_format);

        # Commit snapshot to its backing store
        # XXX: There should be a libvirt API to do this
        system('/usr/bin/qemu-img', 'commit', '-f', 'qcow2', $path);

        # Check commit succeeded
        if($? >> 8 != 0) {
            print STDERR _user_msg(__x("Failed to commit snapshot '{path}' to ".
                                       "backing store '{backingstore}'",
                                       path => $path,
                                       backingstore => $backing_path));
            return -1;
        }

        # Delete the snapshot volume
        $vol->delete(0);
    }

    # Remove the XML backup if it exists
    my $xmlpath = _get_xml_path($dom);
    unlink($xmlpath) if(-e $xmlpath);

    return 0;
}

sub _snapshot_guest
{
    my ($dom, $vmm, $pool) = @_;

    my $name = _get_guest_name($dom);

    # Store a backup of the domain XML before modification
    my $xmlpath = _get_xml_path($dom);

    # Error if the xml backup already exists and force not specified
    if(-e $xmlpath && !$force) {
        print STDERR
            _user_msg(__x("A snapshot already exists for {guest}. You must ".
                          "commit it or roll it back back before creating a ".
                          "new snapshot.", guest => $name));
        return -1;
    }

    # Write the backup
    my $xmlbackup;
    open($xmlbackup, '>', $xmlpath)
        or die(__x("Unable to write to {path}: {error}",
                   path => $xmlpath, error => $!));

    print $xmlbackup $dom->toString();

    close($xmlbackup)
        or die(__x("Error closing {path}: {error}",
                   path => $xmlpath, error => $!));

    # Get a timestamp for use in naming snapshot volumes
    my $time = time();

    return _foreach_disk($dom, sub {
        my ($disk, $source, $target, $path) = @_;

        # Create a new qcow2 volume in the virt-snapshot storage pool
        my $target_name = $target->getNodeValue();
        my $vol_name = "$name-$target_name-$time.qcow2";
        my $vol_xml = "
            <volume>
                <name>$vol_name</name>
                <capacity>0</capacity>
                <target>
                    <format type='qcow2'/>
                </target>
                <backingStore>
                    <path>$path</path>
                </backingStore>
            </volume>
        ";

        my $vol;
        eval {
            $vol = $pool->create_volume($vol_xml);
        };

        if($@) {
            print STDERR
                _user_msg(__x("Unable to create storage volume: {error}",
                           error => $@->stringify()));
            return -1;
        }

        # Update the source to be a "file" with the new path
        $source->setAttribute("file", $vol->get_path());

        # Also update the disk element to be a "file"
        $source->getParentNode()->setAttribute('type', 'file');

        # Replace the driver element with one which describes the qcow2 file
        my ($driver) = $disk->findnodes('driver');

        # Initialise the driver if it's not set
        $driver ||= $disk->appendChild($dom->createElement('driver'));

        $driver->setAttribute('name', 'qemu');
        $driver->setAttribute('type', 'qcow2');

        return 0;
    });
}

sub _rollback_guest
{
    my ($dom, $vmm, $pool) = @_;

    my $name = _get_guest_name($dom);
    my $xmlpath = _get_xml_path($dom);

    # Only rollback a guest without stored XML if force was specified
    if(! -e $xmlpath && !$force) {
        print STDERR _user_msg(__x("Refusing to rollback guest {name} without ".
                                   "backed-up xml",
                                   name => _get_guest_name($dom)));
        return -1;
    }

    # Delete all snapshots
    _foreach_disk($dom, sub {
        my ($disk, $source, $target, $path) = @_;

        if(-e $path && unlink($path) != 1) {
            print STDERR _user_msg(__x("Failed to delete {file}: {error}",
                                       file => $path, error => $!));
            return -1;
        }
        return 0;
    }) == 0 or return -1;

    # Load the backed-up XML
    if(-e $xmlpath) {
        $dom = new XML::DOM::Parser->parsefile($xmlpath);
        if(unlink($xmlpath) != 1) {
            print STDERR _user_msg(__x("Unable to delete backup xml file ".
                                       "{file}: {error}",
                                       file => $xmlpath, error => $!));
            return -1;
        }
    }

    # Or there is no backed-up XML (force must have been given)
    else {
        $dom = undef;
    }

    # undefined the guest if it is defined
    my $domain;
    eval {
        $domain = $vmm->get_domain_by_name($name);
    };

    # Nothing to do if the guest isn't defined
    if(defined($domain)) {
        my $state = $domain->get_info()->{state};

        # Check the domain is shutoff
        unless($state == Sys::Virt::Domain::STATE_SHUTOFF) {
            $domain->destroy();

            # $domain is now undefined. Get it back again
            $domain = $vmm->get_domain_by_name($name);
        }

        $domain->undefine();
    }

    return (0, $dom);
}

sub _foreach_disk
{
    my ($dom, $func) = @_;

    # Get a list of existing disks
    foreach my $disk ($dom->findnodes('/domain/devices/disk')) {
        my ($source) = $disk->findnodes('source');
        my ($target) = $disk->findnodes('target/@dev');

        # Look for the source location
        my $path;
        my $src_attrs = $source->getAttributes();
        foreach my $attr qw(dev file) {
            my $item = $src_attrs->getNamedItem($attr);
            if(defined($item)) {
                $path = $item->getNodeValue();

                # Remove the attribute. We'll add a new one in below.
                $src_attrs->removeNamedItem($attr);

                last;
            }
        }

        # Warn and ignore this source if we didn't find either
        if(!defined($path)) {
            print STDERR _user_msg(__x("invalid source element: {element}",
                                        element => $source->toString()));
            next;
        }

        $func->($disk, $source, $target, $path) == 0 or return -1;
    }
}

sub _get_guest_name
{
    my ($dom) = @_;

    # Get the name of the guest from the domain description
    my ($name_elem) = $dom->findnodes('/domain/name/text()');
    return $name_elem->toString();
}

sub _get_xml_path
{
    my ($dom) = @_;

    return $xmldir.'/'._get_guest_name($dom).'.xml';
}

sub _user_msg
{
    my $msg = shift;

    return "virt-snapshot: ".$msg."\n";
}

=head1 SEE ALSO

L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=head1 AUTHOR

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
