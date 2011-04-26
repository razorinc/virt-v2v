#!/usr/bin/perl
# virt-p2v-server
# Copyright (C) 2011 Red Hat Inc.
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

use IO::Handle;
use YAML::Any;

use Locale::TextDomain 'virt-v2v';

use Sys::Guestfs;

use Sys::VirtConvert;
use Sys::VirtConvert::Config;
use Sys::VirtConvert::Converter;
use Sys::VirtConvert::Connection::LibVirtTarget;
use Sys::VirtConvert::Connection::RHEVTarget;
use Sys::VirtConvert::GuestfsHandle;
use Sys::VirtConvert::Util qw(:DEFAULT logmsg_init logmsg_level);

=encoding utf8

=head1 NAME

virt-p2v-server - Receive data from virt-p2v

=head1 DESCRIPTION

virt-p2v-server is invoked over SSH by virt-p2v. It is not intended to be run
manually.

=cut

# SIGPIPE will cause an untidy exit of the perl process, without calling
# destructors. We don't rely on it anywhere, as we check for errors when reading
# from or writing to a pipe.
$SIG{'PIPE'} = 'IGNORE';

# The protocol version we support
use constant VERSION => 0;

# Message types
use constant MSG_VERSION        => 'VERSION';
use constant MSG_LANG           => 'LANG';
use constant MSG_METADATA       => 'METADATA';
use constant MSG_PATH           => 'PATH';
use constant MSG_CONVERT        => 'CONVERT';
use constant MSG_LIST_PROFILES  => 'LIST_PROFILES';
use constant MSG_SET_PROFILE    => 'SET_PROFILE';
use constant MSG_CONTAINER      => 'CONTAINER';
use constant MSG_DATA           => 'DATA';

# Container types
use constant CONT_RAW => 'RAW';

# Global state
my $config;
my $meta;
my $target;

# Initialize logging
logmsg_init('syslog');
#logmsg_level(DEBUG);

logmsg NOTICE, __x("{program} started.", program => 'p2v-server');

# Wrap everything in a big eval to catch any die(). N.B. $SIG{__DIE__} is no
# good for this, as it catches every die(), even those inside an eval
eval {
    # Set the umask to a reasonable default
    umask(0022);

    # Don't buffer output
    # While perl will use line buffering when STDOUT is connected to a tty, when
    # not connected to a tty, for example when invoked directly over ssh, it
    # will use a regular, large output buffer. This results in messages being
    # held in the buffer indefinitely.
    STDOUT->autoflush(1);

    # Read the config file
    eval {
        $config = Sys::VirtConvert::Config->new('/etc/virt-v2v.conf');
    };
    v2vdie $@ if $@;

    my $msg;
    while ($msg = p2v_receive()) {
        my $type = $msg->{type};

        # VERSION n
        if ($type eq MSG_VERSION) {
            my $version = $msg->{args}[0];
            if ($version <= VERSION) {
                p2v_return_ok();
            }

            else {
                err_and_die(__x('This version of virt-p2v-server does not '.
                                'support protocol version {version}.',
                                version => $version));
            }
        }

        # LANG lang
        elsif ($type eq MSG_LANG) {
            $ENV{LANG} = $msg->{args}[0];
            p2v_return_ok();
        }

        # METADATA length
        #  length bytes of YAML
        elsif ($type eq MSG_METADATA) {
            my $yaml = p2v_read($msg->{args}[0]);
            eval { $meta = Load($yaml); };
            err_and_die('Error parsing metadata: '.$@) if $@;

            p2v_return_ok();
        }

        # PATH length path
        #   N.B. path could theoretically include spaces
        elsif ($type eq MSG_PATH) {
            my $length = $msg->{args}[0];

            my $path = join(' ', @{$msg->{args}}[1..$#{$msg->{args}}]);
            receive_path($path, $length);
        }

        # CONVERT
        elsif ($type eq MSG_CONVERT) {
            convert();
        }

        # LIST_PROFILES
        elsif ($type eq MSG_LIST_PROFILES) {
            p2v_return_list($config->list_profiles());
        }

        # SET_PROFILE profile
        elsif ($type eq MSG_SET_PROFILE) {
            set_profile($msg->{args}[0]);
        }

        else {
            unexpected_msg($type);
        }
    }
};
logmsg FATAL, $@ if $@;

exit(0);

# Receive an image file
sub receive_path
{
    my ($path, $length) = @_;

    err_and_die('PATH without prior SET_PROFILE command')
        unless defined($target);
    err_and_die('PATH without prior METADATA command')
        unless defined($meta);

    my ($disk) = grep { $_->{path} eq $path } @{$meta->{disks}};
    err_and_die("$path not found in metadata") unless defined($disk);

    # Construct a volume name based on the path and hostname
    my $name = $meta->{name}.'-'.$disk->{device};
    $name =~ s,/,_,g;       # e.g. cciss devices have a directory structure

    my $sopts = $config->get_storage_opts();

    my $convert = 0;
    my $format;
    my $sparse;

    # Default to raw. Conversion required for anything else.
    if (!exists($sopts->{format}) || $sopts->{format} eq 'raw') {
        $format = 'raw';
    } else {
        $format = $sopts->{format};
        $convert = 1;
    }

    # Default to non-sparse
    my $allocation = $sopts->{allocation};
    if (!defined($allocation) || $allocation eq 'preallocated') {
        $sparse = 0;
    } elsif ($allocation eq 'sparse') {
        $sparse = 1;
    } else {
        err_and_die(__x('Invalid allocation policy {policy} in profile.',
                        policy => $allocation));
    }

    # Create the target volume
    my $vol;
    eval {
        $vol = $target->create_volume(
            $name,
            $format,
            $length,
            $sparse
        );
    };
    err_and_die($@) if $@;
    p2v_return_ok();

    # Receive an initial container
    my $msg = p2v_receive();
    unexpected_msg($msg->{type}) unless $msg->{type} eq MSG_CONTAINER;

    # We only support RAW container
    my $ctype = $msg->{args}[0];
    err_and_die("Received unknown container type: $ctype")
        unless $ctype eq CONT_RAW;
    p2v_return_ok();

    # Update the disk entry with the new volume details
    $disk->{local_path} = $vol->get_local_path();
    $disk->{path} = $vol->get_path();
    $disk->{is_block} = $vol->is_block();

    my $writer = $vol->get_write_stream($convert);

    # Receive volume data in chunks
    my $received = 0;
    while ($received < $length) {
        my $data = p2v_receive();

        unexpected_msg($data->command) unless $data->{type} eq MSG_DATA;

        # Read the data message in chunks of up to 4M
        my $remaining = $data->{args}[0];
        while ($remaining > 0) {
            my $chunk = $remaining > 4*1024*1024 ? 4*1024*1024 : $remaining;
            my $buf = p2v_read($chunk);

            $received += $chunk;
            $remaining -= $chunk;

            eval { $writer->write($buf); };
            err_and_die($@) if $@;
        }

        p2v_return_ok();
    }
}

# Use the specified profile
sub set_profile
{
    my ($profile) = @_;

    # Check the profile is in our list
    my $found = 0;
    for my $i ($config->list_profiles()) {
        if ($i eq $profile) {
            $found = 1;
            last;
        }
    }
    err_and_die(__x('Invalid profile: {profile}', profile => $profile))
        unless ($found);

    $config->use_profile($profile);

    my $storage = $config->get_storage();
    my $method = $config->get_method();
    if ($method eq 'libvirt') {
        $target = new Sys::VirtConvert::Connection::LibVirtTarget
            ('qemu:///system', $storage);
    } elsif ($method eq 'rhev') {
        $target = new Sys::VirtConvert::Connection::RHEVTarget($storage);
    } else {
        err_and_die(__x('Profile {profile} specifies invalid method {method}.',
                        profile => $profile, method => $method));
    }

    p2v_return_ok();
}

sub convert
{
    err_and_die('CONVERT without prior SET_PROFILE command')
        unless (defined($target));

    err_and_die('CONVERT without prior METADATA command')
        unless defined($meta);

    my @localpaths = map { $_->{local_path} } @{$meta->{disks}};

    my $g;
    eval {
        my $transferiso = $config->get_transfer_iso();

        $g = new Sys::VirtConvert::GuestfsHandle(
            \@localpaths,
            $transferiso,
            $target->isa('Sys::VirtConvert::Connection::RHEVTarget')
        );

        my $transferdev;
        if (defined($transferiso)) {
            my @devices = $g->list_devices();
            $transferdev = pop(@devices);
        }

        my $root = inspect_guest($g, $transferdev);
        my $guestcaps =
            Sys::VirtConvert::Converter->convert($g, $config, $root, $meta);
        $target->create_guest($g, $root, $meta, $config, $guestcaps,
                              $meta->{name});

        if($guestcaps->{block} eq 'virtio' && $guestcaps->{net} eq 'virtio') {
            logmsg NOTICE, __x('{name} configured with virtio drivers.',
                               name => $meta->{name});
        } elsif ($guestcaps->{block} eq 'virtio') {
            logmsg NOTICE, __x('{name} configured with virtio storage only.',
                               name => $meta->{name});
        } elsif ($guestcaps->{net} eq 'virtio') {
            logmsg NOTICE, __x('{name} configured with virtio networking only.',
                               name => $meta->{name});
        } else {
            logmsg NOTICE, __x('{name} configured without virtio drivers.',
                               name => $meta->{name});
        }
    };

    # If any of the above commands result in failure, we need to ensure that
    # the guestfs qemu process is cleaned up before further cleanup. Failure to
    # do this can result in failure to umount RHEV export's temporary mount
    # point.
    if ($@) {
        my $err = $@;
        $g->close();

        # We trust the error was already logged
        p2v_return_err($err);
        die($@);
    }

    p2v_return_ok();
}

sub unexpected_msg
{
    err_and_die('Received unexpected command: '.shift);
}

sub err_and_die
{
    my $err = shift;
    p2v_return_err($err);
    v2vdie $err;
}

END {
    my $err = $?;

    logmsg NOTICE, __x("{program} exited.", program => 'p2v-server');

    # die() sets $? to 255, which is untidy.
    $? = $err == 255 ? 1 : $err;
}

# Perform guest inspection using the libguestfs core inspection API.
# Returns the root device of the os to be converted.
sub inspect_guest
{
    my $g = shift;
    my $transferdev = shift;

    # Get list of roots, sorted
    my @roots = $g->inspect_os();

    # Filter out the transfer device from the results of inspect_os
    # There's a libguestfs bug (fixed upstream) which meant the transfer ISO
    # could be erroneously detected as an unknown Windows OS. As we know what it
    # is, we can filter out the transfer device here. Even when the fix is
    # released this is reasonable belt & braces.
    @roots = grep(!/^\Q$transferdev\E$/, @roots);

    @roots = sort @roots;

    # Only work on single-root operating systems.
    v2vdie __('No root device found in this operating system image.')
        if @roots == 0;

    v2vdie __('Multiboot operating systems are not supported.')
        if @roots > 1;

    return $roots[0];
}

sub p2v_receive
{
    my $in = <>;
    v2vdie __('Client closed connection unexpectedly') unless defined($in);

    # Messages consist of the message type followed by 0 or more arguments,
    # terminated by a newline
    chomp($in);
    $in =~ /^([A-Z_]+)( .+)?$/ or err_and_die("Received invalid message: $in");

    my %msg;
    $msg{type} = $1;
    if (defined($2)) {
        my @args = split(' ', $2);
        $msg{args} =  \@args;
    } else {
        $msg{args} = [];
    }

    logmsg DEBUG, __x('Received: {command} {args}',
                      command => $msg{type},
                      args => join(' ', @{$msg{args}}));

    return \%msg;
}

sub p2v_read
{
    my ($length) = @_;

    my $buf;
    my $total = 0;

    while($total < $length) {
        my $in = read(STDIN, $buf, $length, $total)
            or err_and_die(__x('Error receiving data: {error}', error => $@));
        logmsg DEBUG, "Read $in bytes";
        $total += $in;
    }

    return $buf;
}

sub p2v_return_ok
{
    my $msg = "OK";
    logmsg DEBUG, __x('Sent: {msg}', msg => $msg);
    print $msg,"\n";
}

sub p2v_return_list
{
    my @values = @_;

    my $msg = 'LIST '.scalar(@values);
    foreach my $value (@values) {
        $msg .= "\n$value";
    }
    logmsg DEBUG, __x('Sent: {msg}', msg => $msg);
    print $msg,"\n";
}

sub p2v_return_err
{
    my $msg = 'ERROR '.shift;
    logmsg DEBUG, __x('Sent: {msg}', msg => $msg);
    print $msg,"\n";
}

=head1 SEE ALSO

L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=head1 AUTHOR

Matthew Booth <mbooth@redhat.com>

=head1 COPYRIGHT

Copyright (C) 2011 Red Hat Inc.

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
