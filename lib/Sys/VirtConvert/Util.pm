# Sys::VirtConvert::Util
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

package Sys::VirtConvert::Util;

use strict;
use warnings;

use Carp;
use Sys::Virt;
use XML::DOM;

use Locale::TextDomain 'virt-v2v';

require Exporter;
use vars qw(@ISA @EXPORT @EXPORT_OK);

@ISA = qw(Exporter);
@EXPORT = qw(v2vdie logmsg DEBUG INFO NOTICE WARN FATAL);
@EXPORT_OK = qw(augeas_error parse_libvirt_volinfo rhev_helper
                    logmsg_init logmsg_level);

use constant DEBUG  => 0;
use constant INFO   => 1;
use constant NOTICE => 2;
use constant WARN   => 3;
use constant FATAL  => 4;

=pod

=head1 NAME

Sys::VirtConvert::Util - Utility functions for virt-v2v

=head1 SYNOPSIS

 use Sys::VirtConvert::Util qw(augeas_error);

 augeas_error($g, $@) if ($@);

 v2vdie __x("Couldn't open {file}: {error}", file => $file, error => $error);

=head1 DESCRIPTION

Sys::VirtConvert::Util contains utility functions used throughout virt-v2v.

=head1 METHODS

=over

=item augeas_error($g, $@)

Output an error message which includes any augeas errors in full, in addition to
the given error message.

=cut

sub augeas_error
{
    my ($g, $err) = @_;

    my $msg = "";
    eval {
        foreach my $error ($g->aug_match('/augeas/files//error')) {
            $error =~ /^\/augeas\/files(\/.*)\/error$/
                or die("Unexpected return from aug_match: $error");
            my $file = $1;

            my %detail;
            foreach my $detail_path ($g->aug_match("$error//*")) {
                $detail_path =~ /^$error\/(.*)$/
                    or die("Unexpected return from aug_match: $detail_path");
                $detail{$1} = $g->aug_get($detail_path);
            }

            if (defined($detail{message})) {
                $msg .= __x("augeas error for {file}: {error}",
                           file => $file,
                           error => $detail{message})."\n";
            } else {
                $msg .= __x("augeas error for {file}",
                           file => $file)."\n";
            }

            if (defined($detail{pos}) && defined($detail{line}) &&
                defined($detail{char}))
            {
                $msg .= __x("error at line {line}, char {char}, file ".
                                 "position {pos}",
                                 line => $detail{line},
                                 char => $detail{char},
                                 pos => $detail{pos})."\n";
            }

            if (defined($detail{lens})) {
                $msg .= __x("augeas lens: {lens}",
                            lens => $detail{lens})."\n";
            }
        }
    };

    # Check for failures above
    if ($@) {
        die("error generating pretty augeas error: $@\n".
            "Original error was: $err");
    }

    chomp($msg);

    v2vdie($msg) if length($msg) > 0;
    v2vdie($err);
}


=item parse_libvirt_volinfo(vol)

Return name, format, size, is_sparse, is_block for a given a libvirt volume.

=cut

sub parse_libvirt_volinfo
{
    my ($vol) = @_;

    my $voldom = new XML::DOM::Parser->parse($vol->get_xml_description());

    my ($name, $format, $size, $is_sparse, $is_block);

    ($name) = $voldom->findnodes('/volume/name/text()');
    $name = $name->getData();

    ($format) = $voldom->findnodes('/volume/target/format/@type');
    $format = $format->getValue() if (defined($format));
    $format ||= 'raw';

    my $info = $vol->get_info();

    $size = $info->{capacity};

    my $allocation = $info->{allocation};
    if ($allocation < $size) {
        $is_sparse = 1;
    } else {
        $is_sparse = 0;
    }

    $is_block = $info->{type} == Sys::Virt::StorageVol::TYPE_BLOCK ? 1 : 0;

    return ($name, $format, $size, $allocation, $is_sparse, $is_block);
}

=item rhev_helper(sub)

Execute I<sub> sete(u|g)id 36:36. Signals will be deferred until after I<sub>
exits, and if it die()s, the die() will be thrown after resetting permissions to
root.

=cut

sub rhev_helper
{
    my ($sub) = @_;

    # Don't respond to signals while we're running setuid. Cleanup operations
    # can fail if they run as the wrong user.
    my $sigint  = $SIG{'INT'};
    my $sigquit = $SIG{'QUIT'};
    my $sig_received;

    $SIG{'INT'} = $SIG{'QUIT'} = sub {
        $sig_received = shift;
    };

    my $egid = $);
    $) = "36 36";
    $> = "36";

    eval {
        &$sub();
    };
    my $err = $@;

    $) = $egid;
    $> = "0";

    die($err) if ($err);

    # Restore the signal handlers
    $SIG{'INT'}  = $sigint;
    $SIG{'QUIT'} = $sigquit;

    # Run any deferred signal handlers
    if (defined($sig_received)) {
        &$sigint($sig_received)  if ($sig_received eq 'INT');
        &$sigquit($sig_received) if ($sig_received eq 'QUIT');
    }
}

my $logmsg;
my $logmsg_level = NOTICE;

=item logmsg_init(method)

Initialise the log output method. I<method> can be an open filehandle, one of
*STDOUT or *STDERR, or the string 'syslog'. This method must be called before
calling logmsg().

=cut

sub logmsg_init
{
    my ($method) = @_;

    my $annotate = sub {
        my ($level, $msg) = @_;

        return __x("WARNING: {msg}", msg => $msg) if $level == WARN;
        return __x("FATAL: {msg}", msg => $msg) if $level == FATAL;
        return $msg;
    };

    # Check if we were passed a file handle
    if (ref($method) eq 'GLOB' || $method eq *STDOUT || $method eq *STDERR) {
        $logmsg = sub {
            my ($level, $msg, $willdie) = @_;

            my $lmethod = $method;

            # Send warnings to STDERR if regular messages are going to STDOUT
            $lmethod = *STDERR if $method eq *STDOUT && $level >= WARN;

            $msg = &$annotate($level, $msg);

            # Don't log the message if it would go to STDOUT or STDERR, and
            # we're about to die to STDERR anyway
            print $lmethod "virt-v2v: $msg\n"
                unless $willdie && ($method eq *STDOUT || $method eq *STDERR);
            return $msg;
        }
    }

    elsif ($method eq 'syslog') {
        use Sys::Syslog qw(:standard :macros);

        openlog('virt-v2v', 'pid', LOG_USER);
        $logmsg = sub {
            my ($level, $msg) = @_;

            $msg = &$annotate($level, $msg);

            # syslog is commonly configured to drop DEBUG messages on the floor.
            # We don't want to drop these on the floor in 2 different places, so
            # we send DEBUG messages with INFO priority if they're generated.
            if ($level == DEBUG) {
                syslog(LOG_INFO, $msg);
            } elsif ($level == INFO) {
                syslog(LOG_INFO, $msg);
            } elsif ($level == NOTICE) {
                syslog(LOG_NOTICE, $msg);
            } elsif ($level == WARN) {
                syslog(LOG_WARNING, $msg);
            } elsif ($level == FATAL) {
                syslog(LOG_ERR, $msg);
            }

            return $msg;
        }
    }

    else {
        croak "Unrecognised log method $method";
    }
}

=item logmsg_level(level)

Set the level at which messages will be logged. Options are DEBUG, INFO, NOTICE,
WARN and FATAL.

=cut

sub logmsg_level
{
    $logmsg_level = shift;
}

=item logmsg(level, msg)

Send I<msg> to the previously configured log destination. I<level> can be DEBUG,
INFO, NOTICE, WARN or FATAL.

=cut

sub logmsg
{
    # willdie is a private argument
    my ($level, $msg, $willdie) = @_;

    return unless $level >= $logmsg_level;

    croak "logmsg called without logmsg_init" unless defined($logmsg);
    &$logmsg($level, $msg, $willdie);
}

=item v2vdie(msg)

Log I<msg> at FATAL priority, and die().

=cut

sub v2vdie
{
    my ($msg) = @_;

    logmsg FATAL, $msg, 1;
    die "virt-v2v: $msg\n";
}

=back

=head1 COPYRIGHT

Copyright (C) 2010-2011 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
