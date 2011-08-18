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
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

use strict;
use warnings;

use Test::More;

use Module::Build;

eval "use Module::Find";
plan skip_all => "Module::Find required for testing syntax" if $@;

# Add the Sys::VirtConvert module
my @modules = ('Sys::VirtConvert');

# Add all modules under Sys::VirtConvert
push(@modules, findallmod('Sys::VirtConvert'));

plan tests => scalar(@modules) + 1;

foreach my $module (@modules) {
    use_ok($module);
}

my $build = Module::Build->current();
my $error = $build->dispatch('syntaxcheck');
is($error, 0, 'Build syntaxcheck');
