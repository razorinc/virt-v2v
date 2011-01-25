# Sys::VirtV2V::Converter::Windows
# Copyright (C) 2009-2010 Red Hat Inc.
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

package Sys::VirtV2V::Converter::Windows;

use strict;
use warnings;

use Carp qw(carp);
use File::Temp qw(tempdir);
use Data::Dumper;
use Encode qw(encode decode);
use IO::String;
use XML::DOM;
use XML::DOM::XPath;

use Sys::Guestfs;
use Win::Hivex;
use Win::Hivex::Regedit qw(reg_import);

use Locale::TextDomain 'virt-v2v';
use Sys::VirtV2V::Util qw(user_message);


=pod

=head1 NAME

Sys::VirtV2V::Converter::Windows - Pre-convert a Windows guest to run on KVM

=head1 SYNOPSIS

 use Sys::VirtV2V::Converter;

 Sys::VirtV2V::Converter->convert($g, $config, $desc, $dom, $devices);

=head1 DESCRIPTION

Sys::VirtV2V::Converter::Windows does the "pre-conversion" steps
required to get a Windows guest to boot on KVM.  Unlike the associated
L<Sys::VirtV2V::Converter::Linux(3)> module, this doesn't do a full
conversion of Windows.  Instead it just installs the viostor (Windows
virtio block) driver, so that the Windows guest will be able to boot
on the target.  A "RunOnce" script is also added to the VM which does
all the rest of the conversion the first time the Windows VM is booted
on KVM.

=head1 METHODS

=over

=item Sys::VirtV2V::Converter::Windows->can_handle(desc)

Return 1 if Sys::VirtV2V::Converter::Windows can convert the guest
described by I<desc>, 0 otherwise.

=cut

sub can_handle
{
    my $class = shift;

    my $desc = shift;
    carp("can_handle called without desc argument") unless defined($desc);

    return ($desc->{os} eq 'windows');
}

=item Sys::VirtV2V::Converter::Windows->convert($g, $guestos, $desc, $devices, $config)

(Pre-)convert a Windows guest. Assume that can_handle has previously
returned 1.

=over

=item g

A libguestfs handle to the target.

=item config

An initialised Sys::VirtV2V::Config object.

=item desc

A description of the guest OS as returned by Sys::Guestfs::Lib.

=item dom

A DOM representation of the guest's libvirt domain metadata

=item devices

An arrayref of libvirt storage device names, in the order they will be
presented to the guest.

=back

=cut

sub convert
{
    my $class = shift;

    my ($g, $config, $desc, undef, $devices) = @_;
    croak("convert called without g argument") unless defined($g);
    croak("convert called without config argument") unless defined($config);
    croak("convert called without desc argument") unless defined($desc);
    croak("convert called without devices argument") unless defined($devices);

    _preconvert ($g, $desc, $devices, $config);

    # Return guest capabilities.
    my %guestcaps;

    $guestcaps{virtio} = 1;
    $guestcaps{arch}   = $desc->{arch};
    $guestcaps{acpi}   = 1; # XXX

    # We want an i686 guest for i[345]86
    $guestcaps{arch} =~ s/^i[345]86/i686/;

    return \%guestcaps;
}

=item _preconvert

Preconvert a Windows guest so that it can boot on KVM with virtio.  We
do the minimum changes necessary here: Installing the viostor driver,
then installing a service which will complete the conversion when
Windows is booted first time on KVM.

To install the viostor driver, we use L<Sys::Guestfs(3)> (libguestfs)
and L<Win::Hivex::Regedit(3)> (hivex) to drop the correct files and
registry changes in place.  We don't have access to the Win32 API, so
we cannot do this using the normal methods.

Similarly, we make registry changes to ensure the service runs at next
boot.

=cut

sub _preconvert
{
    my $g = shift;
    my $desc = shift;
    my $devices = shift;
    my $config = shift;

    carp "preconvert called without g argument" unless defined $g;
    carp "preconvert called without desc argument" unless defined $desc;
    carp "preconvert called without devices argument" unless defined $devices;

    # $config can be undef, but we require the configuration file when
    # converting Windows guests.
    unless (defined $config) {
        die (user_message (__x"You must specify the configuration file (-f) when converting Windows guests."));
    }

    my $tmpdir = tempdir (CLEANUP => 1);

    # Note: disks are already mounted by main virt-v2v script.

    _upload_files ($g, $tmpdir, $desc, $devices, $config);
    _add_viostor_to_registry ($g, $tmpdir, $desc, $devices, $config);
    _add_service_to_registry ($g, $tmpdir, $desc, $devices, $config);
    _prepare_virtio_drivers ($g, $tmpdir, $desc, $devices, $config);
}

# See http://rwmj.wordpress.com/2010/04/30/tip-install-a-device-driver-in-a-windows-vm/
sub _add_viostor_to_registry
{
    my $g = shift;
    my $tmpdir = shift;
    my $desc = shift;
    my $devices = shift;
    my $config = shift;

    # Locate and download the system registry.
    my $system_filename;
    eval {
        $system_filename = "/windows/system32/config/system";
        $system_filename = $g->case_sensitive_path ($system_filename);
        $g->download ($system_filename, $tmpdir . "/system");
    };
    if ($@) {
        die (user_message (__x"Could not download the SYSTEM registry from this Windows guest.  The exact error message was: {errmsg}",
                           errmsg => $@));
    }

    # Open the registry hive.
    my $h = Win::Hivex->open ($tmpdir . "/system", write => 1)
        or die "open system hive: $!";

    # Get the 'Current' ControlSet. This is normally 001, but not always.
    my $select = $h->node_get_child($h->root(), 'Select');
    my $current_cs = $h->node_get_value($select, 'Current');
    $current_cs = sprintf("ControlSet%03i", $h->value_dword($current_cs));

    # Make the changes.
    my $regedits = <<REGEDITS;
; Edits to be made to a Windows guest to have
; it boot from viostor.

[HKEY_LOCAL_MACHINE\\SYSTEM\\$current_cs\\Control\\CriticalDeviceDatabase\\pci#ven_1af4&dev_1001&subsys_00000000]
"Service"="viostor"
"ClassGUID"="{4D36E97B-E325-11CE-BFC1-08002BE10318}"

[HKEY_LOCAL_MACHINE\\SYSTEM\\$current_cs\\Control\\CriticalDeviceDatabase\\pci#ven_1af4&dev_1001&subsys_00020000]
"Service"="viostor"
"ClassGUID"="{4D36E97B-E325-11CE-BFC1-08002BE10318}"

[HKEY_LOCAL_MACHINE\\SYSTEM\\$current_cs\\Control\\CriticalDeviceDatabase\\pci#ven_1af4&dev_1001&subsys_00021af4]
"Service"="viostor"
"ClassGUID"="{4D36E97B-E325-11CE-BFC1-08002BE10318}"

[HKEY_LOCAL_MACHINE\\SYSTEM\\$current_cs\\Services\\viostor]
"Type"=dword:00000001
"Start"=dword:00000000
"Group"="SCSI miniport"
"ErrorControl"=dword:00000001
"ImagePath"="system32\\\\drivers\\\\viostor.sys"
"Tag"=dword:00000021

[HKEY_LOCAL_MACHINE\\SYSTEM\\$current_cs\\Services\\viostor\\Parameters]
"BusType"=dword:00000001

[HKEY_LOCAL_MACHINE\\SYSTEM\\$current_cs\\Services\\viostor\\Parameters\\MaxTransferSize]
"ParamDesc"="Maximum Transfer Size"
"type"="enum"
"default"="0"

[HKEY_LOCAL_MACHINE\\SYSTEM\\$current_cs\\Services\\viostor\\Parameters\\MaxTransferSize\\enum]
"0"="64  KB"
"1"="128 KB"
"2"="256 KB"

[HKEY_LOCAL_MACHINE\\SYSTEM\\$current_cs\\Services\\viostor\\Parameters\\PnpInterface]
"5"=dword:00000001

[HKEY_LOCAL_MACHINE\\SYSTEM\\$current_cs\\Services\\viostor\\Enum]
"0"="PCI\\\\VEN_1AF4&DEV_1001&SUBSYS_00021AF4&REV_00\\\\3&13c0b0c5&0&20"
"Count"=dword:00000001
"NextInstance"=dword:00000001
REGEDITS

    my $io;
    if ($desc->{major_version} == 5 || $desc->{major_version} == 6) {
        $io = IO::String->new ($regedits);
    } else {
        die (user_message (__x"Guest is not a supported version of Windows ({major}.{minor})",
                           major => $desc->{major_version},
                           minor => $desc->{minor_version}))
    }

    local *_map = sub {
        if ($_[0] =~ /^HKEY_LOCAL_MACHINE\\SYSTEM(.*)/i) {
            return ($h, $1);
        } else {
            die "can only make updates to the SYSTEM hive (key was: $_[0])\n"
        }
    };

    reg_import ($io, \&_map);

    $h->commit (undef);
    undef $h;

    # Upload the new registry.
    $g->upload ($tmpdir . "/system", $system_filename);
}

# See http://rwmj.wordpress.com/2010/04/29/tip-install-a-service-in-a-windows-vm/
sub _add_service_to_registry
{
    my $g = shift;
    my $tmpdir = shift;
    my $desc = shift;
    my $devices = shift;
    my $config = shift;

    # Locate and download the system registry.
    my $system_filename;
    eval {
        $system_filename = "/windows/system32/config/system";
        $system_filename = $g->case_sensitive_path ($system_filename);
        $g->download ($system_filename, $tmpdir . "/system");
    };
    if ($@) {
        die (user_message (__x"Could not download the SYSTEM registry from this Windows guest.  The exact error message was: {errmsg}",
                           errmsg => $@));
    }

    # Open the registry hive.
    my $h = Win::Hivex->open ($tmpdir . "/system", write => 1)
        or die "open system hive: $!";

    # Make the changes.
    my $regedits = '
[HKEY_LOCAL_MACHINE\SYSTEM\ControlSet001\services\rhev-apt]
"Type"=dword:00000010
"Start"=dword:00000002
"ErrorControl"=dword:00000001
"ImagePath"="c:\\\\Temp\\\\V2V\\\\rhsrvany.exe"
"DisplayName"="RHSrvAny"
"ObjectName"="LocalSystem"

[HKEY_LOCAL_MACHINE\SYSTEM\ControlSet001\services\rhev-apt\Parameters]
"CommandLine"="cmd /c \"c:\\\\Temp\\\\V2V\\\\firstboot.bat\""
"PWD"="c:\\\\Temp\\\\V2V"
';
    my $io = IO::String->new ($regedits);

    local *_map = sub {
        if ($_[0] =~ /^HKEY_LOCAL_MACHINE\\SYSTEM(.*)/i) {
            return ($h, $1);
        } else {
            die "can only make updates to the SYSTEM hive (key was: $_[0])\n"
        }
    };

    reg_import ($io, \&_map);

    $h->commit (undef);
    undef $h;

    # Upload the new registry.
    $g->upload ($tmpdir . "/system", $system_filename);
}

# We copy the VirtIO drivers to a directory on the guest and add this directory
# to HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\DevicePath so that it will
# be searched automatically when automatically installing drivers.
sub _prepare_virtio_drivers
{
    my $g = shift;
    my $tmpdir = shift;
    my $desc = shift;
    my $devices = shift;
    my $config = shift;

    # Copy the target VirtIO drivers to the guest
    my $driverdir = File::Spec->catdir($g->case_sensitive_path("/windows"), "Drivers/VirtIO");

    $g->mkdir_p($driverdir);

    my ($virtio) = $config->match_app ($desc, 'virtio', $desc->{arch});
    $virtio = $config->get_transfer_path($g, $virtio);

    foreach my $src ($g->ls($virtio)) {
        my $name = $src;
        $src = File::Spec->catfile($virtio, $src);
        my $dst = File::Spec->catfile($driverdir, $name);
        $g->cp($src, $dst);
    }

    # Locate and download the SOFTWARE hive
    my $sw_local = File::Spec->catfile($tmpdir, 'software');
    my $sw_guest = $g->case_sensitive_path('/windows/system32/config/software');

    $g->download($sw_guest, $sw_local);

    # Open the registry hive.
    my $h = Win::Hivex->open($sw_local, write => 1)
        or die "open hive $sw_local: $!";

    # Find the node \Microsoft\Windows\CurrentVersion
    my $node = $h->root();
    foreach ('Microsoft', 'Windows', 'CurrentVersion') {
        $node = $h->node_get_child($node, $_);
    }

    # Update DevicePath, but leave everything else as is
    my @new;
    my $append = ';%SystemRoot%\Drivers\VirtIO';
    foreach my $v ($h->node_values($node)) {
        my $key = $h->value_key($v);
        my ($type, $data) = $h->value_value($v);

        # Decode the string from utf16le to perl native
        my $value = decode('UTF-16LE', $data);

        # Append the driver location if it's not there already
        if ($key eq 'DevicePath' && index($value, $append) == -1) {
            # Remove the explicit trailing NULL
            chop($value);

            # Append the new path and a new explicit trailing NULL
            $value .= $append."\0";

            # Re-encode the string back to utf16le
            $data = encode('UTF-16LE', $value);
        }

        push (@new, { key => $key, t => $type, value => $data });
    }
    $h->node_set_values($node, \@new);

    $h->commit(undef);
    undef $h;

    # Upload the new registry.
    $g->upload($sw_local, $sw_guest);
}

sub _upload_files
{
    my $g = shift;
    my $tmpdir = shift;
    my $desc = shift;
    my $devices = shift;
    my $config = shift;

    # Check we have all required files
    my @missing;
    my %files;

    for my $file ("virtio", "firstboot", "firstbootapp", "rhsrvany") {
        my ($path) = $config->match_app ($desc, $file, $desc->{arch});
        my $local = $config->get_transfer_path ($g, $path);
        push (@missing, $path) unless ($g->exists($local));

        $files{$file} = $local;
    }

    # We can't proceed if there are any files missing
    die(user_message(__x("Installation failed because the following ".
                         "files referenced in the configuration file are ".
                         "required, but missing: {list}",
                         list => join(' ', @missing)))) if (@missing > 0);

    # Copy viostor directly into place as it's a critical boot device
    $g->cp (File::Spec->catfile($files{virtio}, 'viostor.sys'),
            $g->case_sensitive_path ("/windows/system32/drivers"));

    # Copy other files into a temp directory on the guest
    # N.B. This directory must match up with the configuration of rhsrvany
    my $path = '';
    foreach my $d ('Temp', 'V2V') {
        $path .= '/'.$d;

        eval { $path = $g->case_sensitive_path($path) };

        # case_sensitive_path will fail if the path doesn't exist
        if ($@) {
            $g->mkdir($path);
        }
    }

    $g->cp ($files{firstboot}, $path);
    $g->cp ($files{firstbootapp}, $path);
    $g->cp ($files{rhsrvany}, $path);
}

=back

=head1 COPYRIGHT

Copyright (C) 2009-2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Converter(3pm)>,
L<Sys::VirtV2V(3pm)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
