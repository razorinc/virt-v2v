# Sys::VirtV2V::Util
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

package Sys::VirtV2V::Util;

use strict;
use warnings;

use Sys::Virt;
use XML::DOM;

use Locale::TextDomain 'virt-v2v';

require Exporter;
use vars qw(@EXPORT_OK @ISA);

@ISA = qw(Exporter);
@EXPORT_OK = qw(augeas_error user_message parse_libvirt_volinfo);

=pod

=head1 NAME

Sys::VirtV2V::Util - Utility functions for virt-v2v

=head1 SYNOPSIS

 use Sys::VirtV2V::Util qw(augeas_error user_message);

 augeas_error($g, $@) if ($@);

 warn user_message(__x("Couldn't open {file}: {error}",
                       file => $file, error => $error));

=head1 DESCRIPTION

Sys::VirtV2V::Util contains utility functions used throughout virt-v2v.

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

    die(user_message($msg)) if (length($msg) > 0);
    die($err);
}

=item user_message(message)

Return a formatted user message. I<message> should not contain a prefix or a
trailing newline.

=cut

sub user_message
{
    my ($msg) = (@_);

    return __x("virt-v2v: {message}\n", message => $msg);
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
    $format = $format->getValue();

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
