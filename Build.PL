#!/usr/bin/perl

use strict;
use warnings;

use Module::Build;

my $build = Module::Build->new (
    license      => 'gpl',
    dist_name    => 'virt-v2v',
    dist_version_from => 'lib/Sys/VirtV2V.pm',
    script_files => [ 'snapshot/virt-snapshot.pl', 'v2v/virt-v2v.pl' ]
);

$build->create_build_script();
