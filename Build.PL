#!/usr/bin/perl

use strict;
use warnings;

use Module::Build;

my $class = Module::Build->subclass(
    class => 'Module::Build::VirtV2V',
    code => <<'SUBCLASS' );

__PACKAGE__->add_property('confdoc_files');

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

# Also process confdoc files
sub process_confdoc_files
{
    my $self = shift;

    # The following substantially cut/paste from
    # Module::Build::Base->manify_bin_pods

    my $mandir = File::Spec->catdir($self->blib(), 'confdoc' );
    File::Path::mkpath($mandir, 0, oct(777));

    require Pod::Man;
    foreach my $pod (@{$self->confdoc_files()}) {
        # Pod::Simple based parsers only support one document per instance.
        # This is expected to change in a future version (Pod::Simple > 3.03).
        my $parser  = Pod::Man->new( section => 5 ); # config goes in section 5

        my (undef, undef, $manpage) = File::Spec->splitpath($pod);
        $manpage =~ s/\.pod\z//i;
        $manpage .= '.5';

        my $outfile = File::Spec->catfile($mandir, $manpage);
        next if $self->up_to_date( $pod, $outfile );
        $self->log_info("Manifying $pod -> $outfile\n");
        $parser->parse_from_file( $pod, $outfile );
    }
}

SUBCLASS

my $build = $class->new (
    license      => 'gpl',
    dist_name    => 'virt-v2v',
    dist_version_from => 'lib/Sys/VirtV2V.pm',
    confdoc_files => [ 'v2v/virt-v2v.conf.pod' ],
    install_path => { 'locale' => '/usr/local/share/locale' },
    script_files => [ 'snapshot/virt-snapshot.pl', 'v2v/virt-v2v.pl' ],
);

$build->add_build_element('confdoc');
$build->add_build_element('locale');
$build->create_build_script();
