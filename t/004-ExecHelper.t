#!/usr/bin/perl
# virt-v2v
# Copyright (C) 2009 Red Hat Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

use strict;
use warnings;

use Test::More tests => 4;

use Sys::VirtV2V::ExecHelper;

my $eh;

# Check it executes a program
eval {
    $eh = Sys::VirtV2V::ExecHelper->run('/bin/echo', 'Foo');
};
ok(defined($eh)) or diag($@);

# Check /bin/echo reports exit status 0
is($eh->status(), 0);

# Check output is "Foo\n"
is($eh->output(), "Foo\n");

# Check /bin/false reports exit status 1
$eh = Sys::VirtV2V::ExecHelper->run('/bin/false');

is($eh->status(), 1);
