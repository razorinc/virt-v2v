# Sys::VirtV2V::Connection::LibVirt
# Copyright (C) 2009,2010 Red Hat Inc.
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

package Sys::VirtV2V::Connection::LibVirtSource;

use strict;
use warnings;

use URI;
use XML::DOM;

use Sys::Virt;

use Sys::VirtV2V;
use Sys::VirtV2V::Connection::Source;
use Sys::VirtV2V::Connection::LibVirt;
use Sys::VirtV2V::Connection::Volume;
use Sys::VirtV2V::Transfer::ESX;
use Sys::VirtV2V::Util qw(user_message parse_libvirt_volinfo);

use Locale::TextDomain 'virt-v2v';

@Sys::VirtV2V::Connection::LibVirtSource::ISA =
    qw(Sys::VirtV2V::Connection::Source Sys::VirtV2V::Connection::LibVirt);

=pod

=head1 NAME

Sys::VirtV2V::Connection::LibVirtSource - Get storage and metadata from libvirt

=head1 SYNOPSIS

 use Sys::VirtV2V::Connection::LibVirtSource;

 $conn = Sys::VirtV2V::Connection::LibVirtSource->new
    ("xen+ssh://xenserver.example.com/", $name);
 $dom = $conn->get_dom();

=head1 DESCRIPTION

Sys::VirtV2V::Connection::LibVirtSource reads a guest's libvirt XML directly
from a libvirt connection. It accesses the guest's storage over the same
transport as the libvirt connection.

=head1 METHODS

=over

=item new(uri, name)

Create a new libvirt source connection. Domain I<name> will be obtained from
I<uri>.

=cut

sub new
{
    my $class = shift;
    my ($uri, $name) = @_;

    my $self = $class->SUPER::_libvirt_new($uri);

    $self->{name} = $name;

    $self->_check_shutdown();
    $self->_get_dom();

    return $self;
}

=item get_name

Return the name of the domain.

=cut

sub get_name
{
    return shift->{name};
}

=item get_volume(path)

Return a volume object for I<path>, where I<path> is the path of a volume on the
connected hypervisor.

=cut

sub get_volume
{
    my $self = shift;
    my ($path) = @_;

    my $uri = $self->{uri};

    my ($name, $format, $size, $is_sparse, $is_block);

    # The libvirt storage APIs aren't yet reliably implemented for ESX, so we
    # need to get volume metadata some other way
    my $transfer;
    if ($uri->scheme eq "esx") {
        $transfer = $self->_get_transfer($path, 0);

        $name = $transfer->esx_get_name();
        $format = "raw";
        $size = $transfer->esx_get_size();
        $is_sparse = 0;
        $is_block = 0;
    }

    else {
        my $vol;
        eval {
            $vol = $self->{vmm}->get_storage_volume_by_path($path);
        };
        die(user_message(__x("Failed to retrieve storage volume {path}:".
                             "{error}",
                             path => $path,
                             error => $@->stringify()))) if($@);

        ($name, $format, $size, $is_sparse, $is_block) =
            parse_libvirt_volinfo($vol);

        $transfer = $self->_get_transfer($path, $is_sparse);
    }

    return new Sys::VirtV2V::Connection::Volume($name, $format, $path, $size,
                                                $is_sparse, $is_block,
                                                $transfer);
}

sub _check_shutdown
{
    my $self = shift;

    my $vmm = $self->{vmm};
    my $domain = $self->_get_domain();

    # Check the domain is shutdown
    die(user_message(__x("Guest {name} is currently {state}. It must be ".
                         "shut down first.",
                         state => _state_string($domain->get_info()->{state}),
                         name => $self->{name})))
        unless ($domain->get_info()->{state} ==
                Sys::Virt::Domain::STATE_SHUTOFF);
}

sub _state_string
{
    my ($state) = @_;

    if ($state == Sys::Virt::Domain::STATE_NOSTATE) {
        return __"idle";
    } elsif ($state == Sys::Virt::Domain::STATE_RUNNING) {
        return __"running";
    } elsif ($state == Sys::Virt::Domain::STATE_BLOCKED) {
        return __"blocked";
    } elsif ($state == Sys::Virt::Domain::STATE_PAUSED) {
        return __"paused";
    } elsif ($state == Sys::Virt::Domain::STATE_SHUTDOWN) {
        return __"shutting down";
    } elsif ($state == Sys::Virt::Domain::STATE_SHUTOFF) {
        return __"shut down";
    } elsif ($state == Sys::Virt::Domain::STATE_CRASHED) {
        return __"crashed";
    } else {
        return "unknown state ($state)";
    }
}

sub _get_domain
{
    my $self = shift;

    return $self->{domain} if (defined($self->{domain}));

    my $vmm = $self->{vmm};
    my $name = $self->{name};

    # Lookup the domain
    my $domain;
    eval {
        $domain = $vmm->get_domain_by_name($name);
    };
    die($@) if ($@);

    # Check we found it
    die(user_message(__x("{name} isn't a valid guest name", name => $name)))
        unless($domain);

    $self->{domain} = $domain;

    return $domain;
}

sub _get_dom
{
    my $self = shift;

    my $vmm = $self->{vmm};
    my $name = $self->{name};

    # Lookup the domain
    my $domain = $self->_get_domain();

    # Warn and exit if we didn't find it
    return undef unless(defined($domain));

    my $xml = $domain->get_xml_description();

    my $dom = new XML::DOM::Parser->parse($xml);
    $self->{dom} = $dom;
}

=back

=head1 COPYRIGHT

Copyright (C) 2009,2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtV2V::Connection::Source(3)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
