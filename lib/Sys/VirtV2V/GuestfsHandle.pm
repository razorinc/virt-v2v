# Sys::VirtV2V::GuestfsHandle
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

package Sys::VirtV2V::GuestfsHandle;

use strict;
use warnings;

use Carp;

use Sys::Guestfs::Lib qw(open_guest);
use Sys::VirtV2V::UserMessage qw(user_message);

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::GuestfsHandle - Proxy Sys::Guestfs with custom close behaviour

=head1 SYNOPSIS

 use Sys::VirtV2V::GuestfsHandle;

 my $g = new Sys::VirtV2V::GuestfsHandle($storage, $transferiso);

 # GuestfsHandle proxies all Sys::Guestfs methods
 print join("\n", $g->list_devices());

 # GuestfsHandle adds 2 new methods
 $g->add_on_close(sub { print "Bye!\n"; });
 $g->close();

=head1 DESCRIPTION

Sys::VirtV2V::GuestfsHandle is a proxy to Sys::Guestfs which adds a custom
close() method, and the ability to register pre-close callbacks.

=head1 METHODS

=over

=item new(storage, transferiso, isrhev)

Create a new object. Open a new Sys::Guestfs handle to proxy, using the disks
defined in the array I<storage>. Add I<transferiso> as a read-only drive if it
is given. If I<isrhev> is true, the handle will use user and group 36:36.

=cut

sub new
{
    my $class = shift;
    my ($storage, $transfer, $isrhev) = @_;

    my $self = {};

    my $interface = "ide";

    # Don't respond to signals while we're running setuid. Cleanup operations
    # can fail if they run as the wrong user.
    my $sigint  = $SIG{'INT'};
    my $sigquit = $SIG{'QUIT'};
    my $sig_received;

    $SIG{'INT'} = $SIG{'QUIT'} = sub {
        $sig_received = shift;
    };

    if ($isrhev) {
        $) = "36 36";
        $> = "36";
    }

    # Open a guest handle
    my $g;

    eval {
        $g = open_guest($storage, rw => 1, interface => $interface);

        # Add the transfer iso if there is one
        $g->add_drive_ro_with_if($transfer, $interface) if (defined($transfer));

        $g->launch();
    };
    my $err = $@;

    if ($isrhev) {
        $) = "0 0";
        $> = "0";
    }

    die($err) if ($err);

    if (defined($sig_received)) {
        &$sigint($sig_received)  if ($sig_received eq 'INT');
        &$sigquit($sig_received) if ($sig_received eq 'QUIT');
    }
    $SIG{'INT'}  = $sigint;
    $SIG{'QUIT'} = $sigquit;

    # Enable autosync to defend against data corruption on unclean shutdown
    $g->set_autosync(1);

    $self->{g} = $g;

    $self->{onclose} = [];

    bless($self, $class);
    return $self;
}

=item add_on_close

Register a callback to be called before closing the underlying Sys::Guestfs
handle.

=cut

sub add_on_close
{
    my $self = shift;

    push(@{$self->{onclose}}, shift);
}

=item close

Call all registered close callbacks, then close the Sys::Guestfs handle.

=cut

sub close
{
    my $self = shift;

    my $g = $self->{g};

    # Nothing to do if handle is already closed
    return unless (defined($g));

    foreach my $onclose (@{$self->{onclose}}) {
        &$onclose();
    }

    my $retval = $?;
    $? = 0;

    # This will close the underlying libguestfs handle, which may affect $?
    $self->{g} = undef;

    warn(user_message(__x("libguestfs did not shut down cleanly")))
        if ($? != 0);

    $? = $retval;
}

our $AUTOLOAD;
sub AUTOLOAD
{
    (my $methodname) = $AUTOLOAD =~ m/.*::(\w+)$/;

    # We don't want to call DESTROY explicitly
    return if ($methodname eq "DESTROY");

    my $self = shift;
    my $g = $self->{g};

    croak("$methodname called on guestfs handle after handle was closed")
        unless (defined($g));

    return $g->$methodname(@_);
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
