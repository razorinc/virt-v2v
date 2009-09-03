#!/usr/bin/perl

use strict;
use warnings;

use Module::Build;

my $class = Module::Build->subclass(
    class => 'Module::Build::VirtV2V',
    code => <<'SUBCLASS' );

# Run 'make -C pot update-po' before the dist action
sub ACTION_dist
{
    my $self = shift;

    # Run make pot in the po directory
    system($self->config('make'), '-C', 'po', 'pot', 'update-po');

    # Run the regular dist action
    $self->SUPER::ACTION_dist;
}

# Generate locale files and install them to blib/locale
sub process_locale_files
{
    my $self = shift;

    # Make them first
    system($self->config('make'), '-C', 'po', 'install',
           'LOCALEDIR=../blib/locale');
}

SUBCLASS

my $build = $class->new (
    license      => 'gpl',
    dist_name    => 'virt-v2v',
    dist_version_from => 'lib/Sys/VirtV2V.pm',
    script_files => [ 'snapshot/virt-snapshot.pl', 'v2v/virt-v2v.pl' ],
    install_path => { 'locale' => '/usr/local/share/locale' }
);

$build->add_build_element('locale');
$build->create_build_script();
