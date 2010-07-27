# Sys::VirtV2V::Transfer::SSH
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

package Sys::VirtV2V::Transfer::SSH;

use POSIX;
use File::Spec;
use File::stat;

use Sys::VirtV2V::UserMessage qw(user_message);

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::Transfer::SSH - Copy a remote guest's storage via ssh

=head1 SYNOPSIS

 use Sys::VirtV2V::Transfer::SSH;

 $vol = Sys::VirtV2V::Transfer::SSH->transfer($conn, $path, $target);

=head1 DESCRIPTION

Sys::VirtV2V::Transfer::SSH retrieves guest storage devices from a remote server
via SSH.

=head1 METHODS

=over

=item transfer(conn, path, target)

Transfer <path> from the remove server. Storage will be created using <target>.

=cut

sub transfer
{
    my $class = shift;

    my ($conn, $path, $target) = @_;

    my (undef, undef, $name) = File::Spec->splitpath($path);

    if ($target->volume_exists($name)) {
        warn user_message(__x("WARNING: storage volume {name} already exists ".
                              "on the target. NOT copying it again. Delete ".
                              "the volume and retry to copy again.",
                              name => $name));
        return $target->get_volume($name);
    }

    my $uri      = $conn->{uri};
    my $username = $conn->{username};
    my $password = $conn->{password};
    my $host     = $conn->{hostname};

    die("URI not defined for connection") unless (defined($uri));

    my ($pid, $size, $fh, $error) =
        _connect($host, $username, $path);

    my $vol = $target->create_volume($name, $size);
    $vol->open();

    my $written = 0;
    for (;;) {
        my $buffer;
        # Transfer in 8k chunks
        my $in = read($fh, $buffer, 8 * 1024);
        die(user_message(__x("Error reading data from {path}: {error}",
                             path => $path,
                             error => $!))) if (!defined($in));

        last if ($in == 0);

        $vol->write($buffer);
        $written += length($buffer);
    }

    $vol->close();

    die(user_message(__x("Didn't receive full volume. Received {received} ".
                         "of {total} bytes.",
                         received => $written,
                         total => $size))) unless ($written == $size);

    waitpid($pid, 0) == $pid or die("error reaping child: $!");
    # If the child returned an error, check for anything on its stderr
    if ($? != 0) {
        my $msg = "";
        while (<$error>) {
            $msg .= $_;
        }
        die(user_message(__x("Unexpected error copying {path} from {host}. ".
                             "Command output: {output}",
                             path => $path,
                             host => $uri->host,
                             output => $msg)));
    }

    return $vol;
}

sub _connect
{
    my ($host, $username, $path) = @_;

    my ($stdin_read, $stdin_write);
    my ($stdout_read, $stdout_write);
    my ($stderr_read, $stderr_write);

    pipe($stdin_read, $stdin_write);
    pipe($stdout_read, $stdout_write);
    pipe($stderr_read, $stderr_write);

    my $pid = fork();
    if ($pid == 0) {
        my @command;
        push(@command, 'ssh');
        push(@command, '-l', $username) if (defined($username));
        push(@command, $host);
        push(@command, "stat -c %s $path; cat $path");

        # Close the ends of the pipes we don't need
        close($stdin_write);
        close($stdout_read);
        close($stderr_read);

        # dup2() stdin, stdout and stderr to pipes
        open(STDIN, "<&".fileno($stdin_read))
            or die("dup stdin failed: $!");
        open(STDOUT, "<&".fileno($stdout_write))
            or die("dup stdout failed: $!");
        open(STDERR, "<&".fileno($stderr_write))
            or die("dup stderr failed: $!");

        # We parse stderr, so make sure it's in English
        $ENV{LANG} = 'C';
        exec(@command);
    }

    close($stdin_read);
    close($stdout_write);
    close($stderr_write);

    # Check that we don't get output on stderr before we read the file size
    for(;;) {
        my ($rin, $rout);
        $rin = '';
        vec($rin, fileno($stdout_read), 1) = 1;
        vec($rin, fileno($stderr_read), 1) = 1;

        my $nfound = select($rout=$rin, undef, undef, undef);
        die("select failed: $!") if ($nfound < 0);

        if (vec($rout, fileno($stderr_read), 1) == 1) {
            my $stderr = '';
            while(<$stderr_read>) {
                $stderr .= $_;
            }

            die(user_message(__x("Unexpected error getting {path}: ".
                                 "{output}",
                                 path => $path, output => $stderr)));
        }

        if (vec($rout, fileno($stdout_read), 1) == 1) {
            last;
        }
    }

    # First line returned is the output of stat
    my $size = <$stdout_read>;

    return ($pid, $size, $stdout_read, $stderr_read);
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
