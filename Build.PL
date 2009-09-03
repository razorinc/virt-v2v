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

# Remove the .pl extension from installed scripts
sub process_script_files
{
    my $self = shift;

    # Run the regular process_script_files action
    $self->SUPER::process_script_files;

    foreach my $script (<blib/script/*>) {
        if($script =~ /^(.*)\.pl$/) {
            rename($script, $1) or die("rename $script to $1 failed: $!");
        }
    }
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
