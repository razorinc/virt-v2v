# Sys::VirtConvert::Transfer::ESX
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

package Sys::VirtConvert::Transfer::ESX::UA;

use DateTime;
use MIME::Base64;

use Sys::VirtConvert;
use Sys::VirtConvert::Util;
use Locale::TextDomain 'virt-v2v';

# This is a gross hack to bring sanity to Net::HTTPS's SSL handling. Net::HTTPS
# can use either Net::SSL or IO::Socket::SSL, depending on which is available at
# runtime. It does not expose which of these it is using, or provide any common
# interface for configuring them. Neither of these libraries will verify a peer
# certificate by default. The configuration required to ensure certificates are
# verified is custom to the driver in use. If the wrong driver is configured, it
# will silently do nothing.
#
# To try to fix this situation, we hardcode here that we want Net::SSL. In the
# constructor, we check that Net::SSL was actually used, and die() if it wasn't.
# We subsequently only include configuration for Net::SSL.

# The initialisation code below is extremely dark magic. Do not mess with it
# unless you already have an old priest and a young priest.
# A simpler version used to work, but broke some time around F16 for 'reasons
# unknown'. This version is intended to do the same as the old version for
# compatibility, but also work in the new environment.
BEGIN {
    $ENV{PERL_NET_HTTPS_SSL_SOCKET_CLASS} = "Net::SSL";

    require Net::SSL;
    require Net::HTTPS;

    $Net::HTTPS::SSL_SOCKET_CLASS = "Net::SSL";

    import Net::HTTPS;
}

sub new {
    my $class = shift;
    my ($username, $password, $noverify) = @_;

    my $self = {};
    bless($self, $class);

    $self->{noverify} = $noverify;
    $self->{agent}    = 'virt-v2v/'.$Sys::VirtConvert::VERSION;
    $self->{auth}     = 'Basic '.encode_base64("$username:$password");

    if ($noverify) {
        # Unset HTTPS_CA_DIR if it is already set
        delete($ENV{HTTPS_CA_DIR});
    } else {
        # Leave HTTPS_CA_DIR alone if it is already set
        # Setting HTTPS_CA_DIR to the empty string results in it using the
        # compiled-in default paths
        $ENV{HTTPS_CA_DIR} = "" unless (exists($ENV{HTTPS_CA_DIR}));
    }

    # Die if we need certificate verification but we're using the wrong SSL
    # library
    die('Invalid configuration of Net::HTTPS')
        unless $noverify || Net::HTTPS->isa('Net::SSL');

    return $self;
}

sub _request
{
    my $self = shift;
    my ($method, $uri) = @_;

    my $base = URI->new($uri->scheme.'://'.$uri->host);
    my $conn = new Net::HTTPS(Host => $uri->host,
                              MaxLineLength => 0)
        or v2vdie __x('Failed to connect to {host}: {error}',
                      host => $uri->host, error => $@);

    $conn->write_request($method => '/'.$uri->rel($base),
                         'User-Agent' => $self->{agent},
                         'Authorization' => $self->{auth})
        or v2vdie __x('Failed to send request to {host}: {error}',
                      host => $uri->host, error => $@);

    my ($code, $msg, %h) = $conn->read_response_headers();
    die([$code, $msg]) unless ($code == 200);

    $self->_verify_certificate($conn, $uri->host) unless ($self->{noverify});

    return ($conn, \%h);
}

sub get_content_length
{
    my $self = shift;
    my ($uri) = @_;

    my ($conn, $h) = $self->_request('HEAD', $uri);

    my $length = $h->{'Content-Length'};
    v2vdie __x('ESX Server didn\'t return content length for {uri}',
               uri => $uri) unless defined($length);

    return $length;
}

sub request_content
{
    my $self = shift;
    my ($uri) = @_;

    my ($conn) = $self->_request('GET', $uri);

    $self->{conn} = $conn;
    $self->{hostname} = $uri->host;
}

sub read_content
{
    my $self = shift;
    my ($size) = @_;

    my $conn = $self->{conn};
    die("read_content called without request_content") unless (defined($conn));

    my $buf;
    my $rv;
    do {
        $rv = $conn->read_entity_body($buf, $size);
    } while (defined($rv) && $rv == -1);
    # We want to clean up and exit immediately on signals, and we don't set
    # nonblocking on any socket, so EINTR and EAGAIN don't need to be handled
    # here

    v2vdie __x('Error reading data from {host}', host => $self->{hostname})
        unless defined($rv);

    return $buf;
}

sub _verify_certificate
{
    my $self = shift;
    my ($conn, $hostname) = @_;

    my $cert = $conn->get_peer_certificate();

    my $cn;
    foreach my $i (split(/\//, $cert->subject_name)) {
        next unless(length($i) > 0);
        my ($key, $value) = split(/=/, $i);
        $cn = lc($value) if (lc($key) eq 'cn');
    }
    v2vdie __x('SSL Certificate Subject from {host} doesn\'t contain a CN.',
               host => $hostname) unless defined($cn);

    $hostname = lc($hostname);
    v2vdie __x('Server {server} presented an SSL certificate '.
               'for {commonname}',
               server => $hostname, commonname => $cn)
        unless $hostname eq $cn or $hostname !~ /\Q.$cn\E$/;

    my $not_before = _parse_nottime($cert->not_before);
    my $not_after  = _parse_nottime($cert->not_after);

    my $now = DateTime->now;
    if (DateTime->compare($now, $not_before) < 0) {
        v2vdie __x('SSL Certificate presented by {host} will not '.
                   'be valid until {date}.',
                   host => $hostname, date => $not_before);
    }

    if (DateTime->compare($now, $not_after) > 0) {
        v2vdie __x('SSL Certificate presented by {host} expired on {date}.',
                   host => $hostname, date => $not_after);
    }

    # This should never happen, because we should instead get an SSL connection
    # error Net::HTTPS
    if (!$conn->get_peer_verify()) {
        die("SSL Certificate not verified");
    }
}

sub _parse_nottime
{
    my ($date) = @_;

    $date =~ /^\s*(\d{4})-(\d\d)-(\d\d)\s+(\d\d):(\d\d):(\d\d)\s+(\S+)\s*$/
        or die("Unrecognised date format: $date");

    return new DateTime(
        year      => $1,
        month     => $2,
        day       => $3,
        hour      => $4,
        minute    => $5,
        second    => $6,
        time_zone => $7
    );
}


package Sys::VirtConvert::Transfer::ESX::ReadStream;

sub new
{
    my $class = shift;
    my ($uri, $username, $password, $noverify, $error) = @_;

    my $self = {};
    bless($self, $class);

    $self->{ua} = new Sys::VirtConvert::Transfer::ESX::UA(
        $username,
        $password,
        $noverify
    );

    eval {
        $self->{ua}->request_content($uri);
    };
    if ($@) {
        if (ref($@) eq 'ARRAY') {
            &$error($@->[0], $@->[1]);
        } else {
            die($@);
        }
    }

    return $self;
}

sub read
{
    my $self = shift;
    my ($size) = @_;

    return $self->{ua}->read_content($size);
}

sub close
{
    # Nothing required
}

sub DESTROY
{
    shift->close();
}


package Sys::VirtConvert::Transfer::ESX;

use Sys::Virt;
use URI;

use Sys::VirtConvert::Util;
use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtConvert::Transfer::ESX - Transfer guest storage from an ESX server

=head1 METHODS

=over

=item new(path, hostname, username, password, noverify, is_sparse)

Return a new ESX Transfer object

=cut

sub new
{
    my $class = shift;
    my ($path, $hostname, $username, $password, $noverify, $is_sparse) = @_;

    v2vdie __x('Authentication is required to connect to '.
               '{server} and no credentials were found in '.
               '.netrc.', server => $hostname) unless defined($username);

    my $self = {};
    bless($self, $class);

    $self->{hostname} = $hostname;
    $self->{username} = $username;
    $self->{password} = $password;
    $self->{noverify} = $noverify;

    my $ua = new Sys::VirtConvert::Transfer::ESX::UA(
        $username,
        $password,
        $noverify
    );

    # ESX path looks like this:
    #  [yellow:storage1] win2k3r2-32/win2k3r2-32.vmdk

    # Strip out datastore and vmdk name
    $path =~ /^\[(.*)\]\s+(.*)\.vmdk$/
        or die("Failed to parse ESX path: $path");
    my $datastore = $1;
    my $vmdk = $2;

    $self->{uri} = _get_vol_uri($hostname, $vmdk, $datastore);

    # Get the size of the volume. At the same time, verify we have the correct
    # URI.
    my $retried = 0;
    for(;;) {
        eval {
            $self->{size} = $ua->get_content_length($self->{uri});
        };

        last if defined($self->{size});

        if ($@) {
            # Re-throw an unstructured error
            die ($@) if (ref($@) ne 'ARRAY');

            my $code = $@->[0];
            my $msg  = $@->[1];

            my $r = $@;
            # If a disk is actually a snapshot image it will have '-00000n'
            # appended to its name, e.g.:
            #   [yellow:storage1] RHEL4-X/RHEL4-X-000003.vmdk
            # The flat storage is still called RHEL4-X-flat, however. If we got
            # a 404 and the vmdk name looks like it might be a snapshot, try
            # again without the snapshot suffix.
            # XXX: We're in flaky heuristic territory here. When the libvirt ESX
            # driver implements the volume apis we should look for this
            # information there instead.
            if ($code == 404 && !$retried && $vmdk =~ /^(.*)-\d+$/) {
                $vmdk = $1;
                $self->{uri} = _get_vol_uri($hostname, $vmdk, $datastore);
            }

            else {
                $self->_report_error($code, $msg);
            }

            $retried = 1;
        }
    }

    # Create a libvirt-friendly volume name
    $self->{name} = $vmdk;
    $self->{name} =~ s,/,_,g;

    return $self;
}

# Volume path looks like this:
#  https://yellow.rhev.marston/folder/win2k3r2-32/win2k3r2-32-flat.vmdk? \
#  dcPath=ha-datacenter&dsName=yellow:storage1
sub _get_vol_uri
{
    my ($server, $vmdk, $datastore) = @_;

    my $uri = URI->new("https://".$server);
    $uri->path("/folder/$vmdk-flat.vmdk");
    $uri->query_form(dcPath => "ha-datacenter", dsName => $datastore);

    return $uri;
}

=item local_path

ESX cannot return a local path. This function will die().

=cut

sub local_path
{
    v2vdie __('virt-v2v cannot write to an ESX connection');
}

=item get_read_stream(convert).

Get a ReadStream for this volume. Data will be converted to raw format if
I<convert> is 1.

=cut

sub get_read_stream
{
    my $self = shift;
    my ($convert) = @_; # Not required, as ESX connection always returns raw

    return new Sys::VirtConvert::Transfer::ESX::ReadStream(
        $self->{uri},
        $self->{username},
        $self->{password},
        $self->{noverify},
        sub { $self->_report_error(@_) }
    );
}

=item get_write_stream

get_write_stream is not implemented for ESX. This function will die with an
error message if called.

=cut

sub get_write_stream
{
    v2vdie __('Unable to write to an ESX connection');
}

=item esx_get_name

Return a libvirt-friendly name for this ESX path.

=cut

sub esx_get_name
{
    return shift->{name};
}

=item esx_get_size

Return the size of the volume which will be returned.

=cut

sub esx_get_size
{
    return shift->{size};
}

sub _report_error
{
    my $self = shift;
    my ($code, $msg) = @_;

    v2vdie __x('Authentication error connecting to {server}. Used '.
               'credentials for {username} from .netrc.',
               server => $self->{hostname}, username => $self->{username})
        if ($code == 401);

    v2vdie __x('Failed to connect to ESX server: {error}', error => $msg);
}

=back

=head1 COPYRIGHT

Copyright (C) 2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
