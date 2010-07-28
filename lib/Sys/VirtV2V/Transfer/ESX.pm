# Sys::VirtV2V::Transfer::ESX
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

package Sys::VirtV2V::Transfer::ESX::UA;

use strict;
use warnings;

use Sys::Virt::Error;

use Sys::VirtV2V;

use Sys::VirtV2V::Util qw(user_message);

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
# _new constructor, we check that Net::SSL was actually used, and die() if it
# wasn't. We subsequently only include configuration for Net::SSL.
BEGIN {
    use Net::HTTPS;

    $Net::HTTPS::SSL_SOCKET_CLASS = "Net::SSL";
}

use LWP::UserAgent;
@Sys::VirtV2V::Transfer::ESX::UA::ISA = qw(LWP::UserAgent);

our %handles;

sub new {
    my $class = shift;

    my ($server, $username, $password, $target, $noverify) = @_;

    my $self = $class->SUPER::new(
        agent => 'virt-v2v/'.$Sys::VirtV2V::VERSION,
        protocols_allowed => [ 'https' ]
    );

    # Older versions of LWP::UserAgent don't support show_progress
    $self->show_progress(1) if ($self->can('show_progress'));

    $self->{_v2v_server}   = $server;
    $self->{_v2v_target}   = $target;
    $self->{_v2v_username} = $username;
    $self->{_v2v_password} = $password;
    $self->{_v2v_noverify} = $noverify;

    if ($noverify) {
        # Unset HTTPS_CA_DIR if it is already set
        delete($ENV{HTTPS_CA_DIR});
    } else {
        # Leave HTTPS_CA_DIR alone if it is already set
        # Setting HTTPS_CA_DIR to the empty string results in it using the
        # compiled-in default paths
        $ENV{HTTPS_CA_DIR} = "" unless (exists($ENV{HTTPS_CA_DIR}));
    }

    die("Invalid configuration of Net::HTTPS")
        unless(Net::HTTPS->isa('Net::SSL'));

    return $self;
}

sub get_volume
{
    my $self = shift;

    my ($path) = @_;

    # Need to turn this:
    #  [yellow:storage1] win2k3r2-32/win2k3r2-32.vmdk
    # into this:
    #  https://yellow.rhev.marston/folder/win2k3r2-32/win2k3r2-32-flat.vmdk? \
    #  dcPath=ha-datacenter&dsName=yellow:storage1

    $path =~ /^\[(.*)\]\s+(.*)\.vmdk$/
        or die("Failed to parse ESX path: $path");
    my $datastore = $1;
    my $vmdk = $2;

    my $url = _get_vol_url($self->{_v2v_server}, $vmdk, $datastore);

    # Replace / with _ so the vmdk name can be used as a volume name
    my $volname = $vmdk;
    $volname =~ s,/,_,g;
    $self->{_v2v_volname} = $volname;

    my $target = $self->{_v2v_target};
    if ($target->volume_exists($volname)) {
        warn user_message(__x("WARNING: storage volume {name} already exists ".
                              "on the target. NOT fetching it again. Delete ".
                              "the volume and retry to download again.",
                              name => $volname));
        return $target->get_volume($volname);
    }

    # Head request to get the size and create the volume
    # We could do this with a single GET request. The problem with this is that
    # you have to create the volume before writing to it. If the volume creation
    # takes a very long time, the transfer may fail in the mean time.
    my $retried = 0;
    SIZE: for(;;) {
        my $r = $self->head($url);
        if ($r->is_success) {
            $self->verify_certificate($r) unless ($self->{_v2v_noverify});
            $self->create_volume($r);
            last SIZE;
        }

        # If a disk is actually a snapshot image it will have '-00000n'
        # appended to its name, e.g.:
        #  [yellow:storage1] RHEL4-X/RHEL4-X-000003.vmdk
        # The flat storage is still called RHEL4-X-flat, however.
        # If we got a 404 and the vmdk name looks like it might be a snapshot,
        # try again without the snapshot suffix.
        # XXX: We're in flaky heuristic territory here. When the libvirt ESX
        # driver implements the volume apis we should look for this information
        # there instead.
        elsif ($r->code == 404 && $retried == 0) {
            $retried = 1;
            if ($vmdk =~ /^(.*)-\d+$/) {
                $vmdk = $1;
                $url = _get_vol_url($self->{_v2v_server}, $vmdk, $datastore);
            }
        }

        else {
            $self->report_error($r);
        }
    }

    $self->{_v2v_received} = 0;
    my $r = $self->get($url,
                    ':content_cb' => sub { $self->handle_data(@_); },
                    ':read_size_hint' => 64 * 1024);

    if ($r->is_success) {
        # It reports success even if one of the callbacks died
        my $died = $r->header('X-Died');
        die($died) if (defined($died));

        $self->verify_certificate($r) unless ($self->{_v2v_noverify});

        # It reports success even if we didn't receive the whole file
        die(user_message(__x("Didn't receive full volume. Received {received} ".
                             "of {total} bytes.",
                             received => $self->{_v2v_received},
                             total => $self->{_v2v_volsize})))
            unless ($self->{_v2v_received} == $self->{_v2v_volsize});

        my $vol = $self->{_v2v_vol};
        $vol->close();
        return $vol;
    }

    $self->report_error($r);
}

sub _get_vol_url
{
    my ($server, $vmdk, $datastore) = @_;

    my $url = URI->new("https://".$server);
    $url->path("/folder/$vmdk-flat.vmdk");
    $url->query_form(dcPath => "ha-datacenter", dsName => $datastore);

    return $url;
}

sub report_error
{
    my $self = shift;
    my ($r) = @_;

    if ($r->code == 401) {
        die(user_message(__x("Authentication error connecting to ".
                             "{server}. Used credentials for {username} ".
                             "from .netrc.",
                             server => $self->{_v2v_server},
                             username => $self->{_v2v_username})))
    }

    die(user_message(__x("Failed to connect to ESX server: {error}",
                         error => $r->status_line)));
}

sub get_basic_credentials
{
    my $self = shift;

    my ($realm, $uri, $isproxy) = @_; # Not interested in any of these things
                                      # because we only ever contact a single
                                      # server in a single context

    return ($self->{_v2v_username}, $self->{_v2v_password});
}

sub handle_data
{
    my $self = shift;

    my ($data, $response) = @_;

    # Verify the certificate of the get request the first time we're called
    if ($self->{_v2v_received} == 0) {
        $self->verify_certificate($response) unless ($self->{_v2v_noverify});
    }

    $self->{_v2v_received} += length($data);
    $self->{_v2v_vol}->write($data);
}

sub create_volume
{
    my $self = shift;

    my ($response) = @_;

    my $target = $self->{_v2v_target};

    my $name = $self->{_v2v_volname};
    die("create_volume called, but _v2v_volname is not set")
        unless (defined($name));

    my $size = $response->content_length();
    $self->{_v2v_volsize} = $size;

    my $vol = $target->create_volume($name, $size);
    $vol->open();
    $self->{_v2v_vol} = $vol;
}

sub verify_certificate
{
    my $self = shift;

    my ($r) = @_;

    # No point in trying to verify headers if the request failed anyway
    return unless ($r->is_success);

    my $subject = $r->header('Client-SSL-Cert-Subject');
    die(user_message(__"Server response didn't include an SSL subject"))
        unless ($subject);

    $subject =~ /\/CN=([^\/]*)/
        or die(user_message(__x("SSL Certification Subject doesn't contain a ".
                                "common name: {subject}",
                                subject => $subject)));
    my $cn = $1;

    $self->{_v2v_server} =~ /(^|\.)\Q$cn\E$/
        or die(user_message(__x("Server {server} presented an SSL certificate ".
                                "for {commonname}",
                                server => $self->{_v2v_server},
                                commonname => $cn)));
}

package Sys::VirtV2V::Transfer::ESX;

use Sys::Virt;

use Sys::VirtV2V::Util qw(user_message);

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::Transfer::ESX - Transfer guest storage from an ESX server

=head1 SYNOPSIS

 use Sys::VirtV2V::Transfer::ESX;

 $vol = Sys::VirtV2V::Transfer::ESX->transfer($conn, $path, $target);

=head1 DESCRIPTION

Sys::VirtV2V::Transfer::ESX retrieves guest storage devices from an ESX server.

=head1 METHODS

=over

=item transfer(conn, path, target)

Transfer <path> from a remote ESX server. Server and authentication details will
be taken from <conn>. Storage will be created using <target>.

=cut

sub transfer
{
    my $class = shift;

    my ($conn, $path, $target) = @_;

    my $uri      = $conn->{uri};
    my $username = $conn->{username};
    my $password = $conn->{password};

    die("URI not defined for connection") unless (defined($uri));

    die(user_message(__x("Authentication is required to connect to ".
                         "{server} and no credentials were found in ".
                         ".netrc.",
                         server => $conn->{hostname})))
        unless (defined($username));

    # Look for no_verify in the URI
    my %query = $uri->query_form;

    my $noverify = 0;
    $noverify = 1 if (exists($query{no_verify}) && $query{no_verify} eq "1");

    # Initialise a user agent
    my $ua = Sys::VirtV2V::Transfer::ESX::UA->new($conn->{hostname},
                                                  $username,
                                                  $password,
                                                  $target,
                                                  $noverify);

    return $ua->get_volume($path);
}

=back

=head1 COPYRIGHT

Copyright (C) 2009,2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Converter(3pm)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
