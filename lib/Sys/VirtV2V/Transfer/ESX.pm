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

use Sys::VirtV2V::UserMessage qw(user_message);

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
our @ISA = ("LWP::UserAgent");

our %handles;

sub new {
    my $class = shift;

    my ($server, $username, $password, $pool, $verify) = @_;

    my $self = $class->SUPER::new(
        agent => 'virt-v2v/'.$Sys::VirtV2V::VERSION,
        protocols_allowed => [ 'https' ]
    );
    $self->show_progress(1);

    $self->add_handler(response_header => sub {
        my ($response, $self, $h) = @_;

        if ($response->is_success) {
            $self->verify_certificate($response) if ($verify);
            $self->create_volume($response);
        }
    });

    $self->{_v2v_server}   = $server;
    $self->{_v2v_pool}     = $pool;
    $self->{_v2v_username} = $username;
    $self->{_v2v_password} = $password;

    if ($verify) {
        # Leave HTTPS_CA_DIR alone if it is already set
        # Setting HTTPS_CA_DIR to the empty string results in it using the
        # compiled-in default paths
        $ENV{HTTPS_CA_DIR} = "" unless (exists($ENV{HTTPS_CA_DIR}));
    } else {
        # Unset HTTPS_CA_DIR if it is already set
        delete($ENV{HTTPS_CA_DIR});
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

    my $url = URI->new("https://".$self->{_v2v_server});
    $url->path("/folder/$vmdk-flat.vmdk");
    $url->query_form(dcPath => "ha-datacenter", dsName => $datastore);

    # Replace / with _ so the vmdk name can be used as a volume name
    $self->{_v2v_volname} = $vmdk;
    $self->{_v2v_volname} =~ s,/,_,g;

    # Check to see if this volume already exists
    eval {
        my $pool = $self->{_v2v_pool};
        $self->{_v2v_vol} = $pool->get_volume_by_name($self->{_v2v_volname});
    };

    # The above command should generate VIR_ERR_NO_STORAGE_VOL because the
    # volume doesn't exist
    unless($@ && $@->code == Sys::Virt::Error::ERR_NO_STORAGE_VOL) {
        unless ($@) {
            print STDERR user_message(__x("WARNING: storage volume {name} ".
                                          "already exists in the target ".
                                          "pool. NOT fetching it again. ".
                                          "Delete the volume and retry to ".
                                          "download again.",
                                          name => $self->{_v2v_volname}));
            return $self->{_v2v_vol};
        }

        # We got an error, but not the one we expected
        die(user_message(__x("Unexpected error accessing storage pool: ",
                             "{error}", error => $@->stringify())));
    }

    my $r = $self->SUPER::get($url,
                              ':content_cb' => sub { $self->handle_data(@_); },
                              ':read_size_hint' => 64 * 1024);

    if ($r->is_success) {
        # It reports success even if one of the callbacks died
        my $died = $r->header('X-Died');
        die($died) if (defined($died));

        # Close the volume file descriptor
        close($self->{_v2v_volfh});
        return $self->{_v2v_vol};
    }

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

    my $volfh = $self->{_v2v_volfh};

    syswrite($volfh, $data)
        or die(user_message(__x("Error writing to {path}: {error}",
                                path => $self->{_v2v_volpath},
                                error => $!)));
}

sub create_volume
{
    my $self = shift;

    my ($response) = @_;

    my $pool = $self->{_v2v_pool};

    # Create a volume in the target storage pool of the correct size
    my $name = $self->{_v2v_volname};
    die("create_volume called, but _v2v_volname is not set")
        unless (defined($name));

    my $size = $response->content_length();

    my $vol_xml = "
        <volume>
            <name>$name</name>
            <capacity>$size</capacity>
        </volume>
    ";

    my $volume;
    eval {
        $volume = $pool->create_volume($vol_xml);
    };
    die(user_message(__x("Failed to create storage volume: {error}",
                         error => $@->stringify()))) if ($@);
    $self->{_v2v_vol} = $volume;

    # Open the volume for writing
    open(my $volfh, '>', $volume->get_path())
        or die(user_message(__x("Error opening storage volume {path} ".
                                "for writing: {error}", error => $!)));

    $self->{_v2v_volfh} = $volfh;
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

use Sys::VirtV2V::UserMessage qw(user_message);

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::Transfer::ESX - Transfer guest storage from an ESX server

=head1 SYNOPSIS

 use Sys::VirtV2V::Transfer::ESX;

 $vol = Sys::VirtV2V::Transfer::ESX->transfer($conn, $path, $pool);

=head1 DESCRIPTION

Sys::VirtV2V::Transfer::ESX retrieves guest storage devices from an ESX server.

=head1 METHODS

=over

=item transfer(conn, path, pool)

Transfer <path> from a remote ESX server. Server and authentication details will
be taken from <conn>. Storage will be copied to a new volume created in <pool>.

=cut

sub transfer
{
    my $class = shift;

    my ($conn, $path, $pool) = @_;

    my $uri      = $conn->{uri};
    my $username = $conn->{username};
    my $password = $conn->{password};

    die("URI not defined for connection")      unless (defined($uri));

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
                                                  $pool);

    return $ua->get_volume($path);
}

=back

=head1 COPYRIGHT

Copyright (C) 2009, 2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-v2v(1)>,
L<v2v-snapshot(1)>,
L<http://libguestfs.org/>.

=cut

1;
