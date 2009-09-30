# Sys::VirtV2V::ExecHelper
# Copyright (C) 2009 Red Hat Inc.
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

package Sys::VirtV2V::ExecHelper;

use strict;
use warnings;

use File::Temp;
use IPC::Open3;
use POSIX ":sys_wait_h";

=pod

=head1 NAME

Sys::VirtV2V::ExecHelper - Execute a command with output suppression

=head1 SYNOPSIS

 use Sys::VirtV2V::ExecHelper;

 $eh = Sys::VirtV2V::ExecHelper->run('rpm', '-V', 'bash');

 if($eh->status() != 0) {
     print STDERR $eh->output();
 }

=head1 DESCRIPTION

Sys::VirtV2V::ExecHelper is a substitute for system() when you don't want any
command output. Sys::VirtV2V::ExecHelper does, however, make STDOUT and STDERR
available in a combined stream if it is required, for example because the
executed command failed.

=head1 METHODS

=over

=item run(@command)

Run @command, which is an array containing the command and its arguments.

The command will be executed immediately. ExecHelper will block until the
command exits.

=cut

sub run
{
    my $class = shift;
    my @command = @_;

    my $self = {};
    bless($self, $class);

    $self->{output} = File::Temp->new();
    $self->{status} = $self->_run(@command);

    return $self;
}

sub _run
{
    my $self = shift;
    my @command = @_;

    my $output = $self->{output};

    my $null;
    open($null, '<', '/dev/null') or die("Failed to open /dev/null: $!");

    my $pid = open3($null, $output, $output, @command);
    waitpid($pid, 0);
    $self->{status} = $? >> 8;
}

=item status

Return the exit status of the executed command.

=cut

sub status
{
    my $self = shift;

    return $self->{status};
}

=item output

Return the combined stdout and stderr of the command.

B<WARNING:> This command puts the output in a string in memory. Don't use this
if the output could be large.

=cut

sub output
{
    my $self = shift;

    my $fh = $self->{output};
    # XXX: This fails for some reason.
    # Don't call output() twice and expect it to work.
    #seek($fh, 0, 0) or die("Seek on command output failed: $!");

    my $output;
    while(<$fh>) {
        $output .= $_;
    }

    return $output;
}

=back

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-v2v(1)>,
L<v2v-snapshot(1)>,
L<http://libguestfs.org/>.

=cut

1;
