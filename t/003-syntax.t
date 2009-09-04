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

use Test::More;

use Module::Build;
use Module::Find;

# Add the Sys::VirtV2V module
my @modules = ('Sys::VirtV2V');

# Add all modules under Sys::VirtV2V
push(@modules, findallmod(Sys::VirtV2V));

plan tests => scalar(@modules) + 1;

foreach my $module (@modules) {
    use_ok($module);
}

my $build = Module::Build->current();
my $error = $build->dispatch('syntaxcheck');
is($error, 0, 'Build syntaxcheck');
