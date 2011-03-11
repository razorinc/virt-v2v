# Sys::VirtConvert::Converter
# Copyright (C) 2009-2011 Red Hat Inc.
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

package Sys::VirtConvert::Converter;

use strict;
use warnings;

use Carp;

use Module::Pluggable sub_name => 'modules',
                      search_path => ['Sys::VirtConvert::Converter'],
                      require => 1;

use Locale::TextDomain 'virt-v2v';

use Sys::VirtConvert::Util;

=pod

=head1 NAME

Sys::VirtConvert::Converter - Convert a guest to run on KVM

=head1 SYNOPSIS

 use Sys::VirtConvert::Converter;

 Sys::VirtConvert::Converter->convert($g, $config, $desc, $meta);

=head1 DESCRIPTION

Sys::VirtConvert::Converter instantiates an appropriate backend for the target
guest OS, and uses it to convert the guest to run on KVM.

=head1 METHODS

=over

=item Sys::VirtConvert::Converter->convert(g, config, desc, meta)

Instantiate an appropriate backend and call convert on it.

=over

=item g

A libguestfs handle to the target.

=item config

An initialised Sys::VirtConvert::Config object.

=item desc

The OS description returned by Sys::Guestfs::Lib.

=item meta

Guest metadata.

=back

=cut

sub convert
{
    my $class = shift;

    my ($g, $config, $desc, $meta) = @_;
    croak("convert called without g argument") unless defined($g);
    croak("convert called without config argument") unless defined($config);
    croak("convert called without desc argument") unless defined($desc);
    croak("convert called without meta argument") unless defined($meta);

    my $guestcaps;

    # Find a module which can convert the guest and run it
    foreach my $module ($class->modules()) {
        if($module->can_handle($desc)) {
            $guestcaps = $module->convert($g, $config, $desc, $meta);
            last;
        }
    }

    unless (defined($guestcaps)) {
        my $block = 'ide';
        my $net   = 'rtl8139';
        my $arch  = 'x86_64';

        logmsg WARN, __x('Unable to convert this guest operating system. Its '.
                         'storage will be transfered and a domain created for '.
                         'it, but it may not operate correctly without manual '.
                         'reconfiguration. The domain will present all '.
                         'storage devices as {block}, all network interfaces '.
                         'as {net} and the host as {arch}.',
                         block => $block, net => $net, arch => $arch);

        # Set some conservative defaults
        $guestcaps = {
            'arch'  => $arch,
            'block' => $block,
            'net'   => $net,
            'acpi'  => 1
        };
    }

    return $guestcaps;
}

=back

=head1 COPYRIGHT

Copyright (C) 2009-2011 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<Sys::VirtConvert::Converter::Linux(3pm)>,
L<Sys::Guestfs::Lib(3pm)>,
L<Sys::Virt(3pm)>,
L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
