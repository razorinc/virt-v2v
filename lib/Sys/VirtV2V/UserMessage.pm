# Sys::VirtV2V::UserMessage
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

package Sys::VirtV2V::UserMessage;

use strict;
use warnings;

use Locale::TextDomain 'virt-v2v';

require Exporter;

use vars qw(@EXPORT_OK @ISA);

@ISA = qw(Exporter);
@EXPORT_OK = qw(user_message);

=pod

=head1 NAME

Sys::VirtV2V::UserMessage - Create consistent user messages

=head1 SYNOPSIS

 use Sys::VirtV2V::UserMessage qw(user_message);

 Sys::VirtV2V::UserMessage->set_identifier(__'virt-v2v');

 print STDERR user_message(__x("Couldn't open {file}: {error}",
                               file => $file, error => $error));

=head1 DESCRIPTION

Sys::VirtV2V::UserMessage provides an interface for displaying consistently
formatted messages to a user. The string passed to user_message() should not
contain a trailing newline. In English, the output will have the identifier
prefixed if one was specified, and a newline appended.

=head1 METHODS

=over

=item Sys::VirtV2V::UserMessage->set_identifier(identifier)

Set an identifier which will identify messages from this source from other
sources. In English, this identifier will be prefixed to all messages with a
colon and trailing space.

=cut

our $identifier;

sub set_identifier
{
    my $class = shift;
    my ($new_identifier) = @_;

    $identifier = $new_identifier;
}

=item user_message(message)

Return a formatted user message.

I<message> should not contain a prefix or a trailing newline.

=cut

sub user_message
{
    my ($msg) = (@_);

    return __x("{identifier}: {message}\n",
               identifier => $identifier, message => $msg);
}

=back

=head1 COPYRIGHT

Copyright (C) 2009 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=cut

1;
