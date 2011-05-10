#!/usr/bin/perl

use strict;
use warnings;

use Module::Build;

my $class = Module::Build->subclass(
    class => 'Module::Build::VirtConvert',
    code => <<'SUBCLASS' );

__PACKAGE__->add_property('confdoc_files');
__PACKAGE__->add_property('syntaxcheck_exclude');

# Define a 'po' action which runs all the make targets under po/
sub ACTION_po
{
    my $self = shift;

    system($self->config('make'), '-C', 'po', 'pot', 'update-po') == 0
        or return 1;
}

# Define a 'p2vimagebuilder' which runs make under p2v_image_builder
sub ACTION_p2vimagebuilder
{
    my $self = shift;

    system($self->config('make'), '-C', 'p2v/image-builder') == 0
        or return 1;
}

# Generate locale files and install them to blib/locale
sub process_locale_files
{
    my $self = shift;

    # Make them first
    $self->log_info("process_locale_files\n");
    system($self->config('make'), '-C', 'po', 'install',
           'LOCALEDIR=../blib/locale') == 0
        or die(); # We need to die() here because process_foo_files return isn't
                  # checked.
}

# Remove the .pl extension from installed scripts
sub process_script_files
{
    my $self = shift;

    # Run the regular process_script_files action
    $self->SUPER::process_script_files();

    foreach my $script (<blib/script/*>) {
        if($script =~ /^(.*)\.pl$/) {
            unless(rename($script, $1)) {
                $self->log_info("rename $script to $1 failed: $1\n");
                die(); # Return isn't checked
            }
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

# Add syntaxcheck target
sub ACTION_syntaxcheck
{
    my $self = shift;

    # Populate %exclude with files we won't syntax check
    my %excludes;

    # Glob expand syntaxcheck_excludes into %excludes
    if(defined($self->syntaxcheck_exclude())) {
        foreach my $exclude (@{$self->syntaxcheck_exclude()}) {
            foreach my $match (glob($exclude)) {
                $excludes{$match} = 1;
            }
        }
    }

    my $error = 0;

    # Check for trailing whitespace in all files in the manifest
    require ExtUtils::Manifest;
    foreach my $file (keys(%{ExtUtils::Manifest::maniread()})) {
        # Skip file if it's excluded
        next if($excludes{$file});

        my $fh;
        unless(open($fh, '<', $file)) {
            $self->log_info("Unable to read manifest file $file: $!\n");
            return 1;
        }

        # Check for leading tabs by default
        my $checktabs = 1;

        # Don't check for leading tabs in Makefiles
        my (undef, undef, $name) = File::Spec->splitpath($file);
        $checktabs = 0 if($name =~ /^Makefile(\..*)?$/);

        while(<$fh>) {
            if(/\s\n$/) {
                $self->log_info("$file: trailing whitespace on line $.\n");
                $error = 1;
            }

            if($checktabs && /^\s*\t/) {
                $self->log_info("$file: indentation uses tabs on line $.\n");
                $error = 1;
            }
        }
    }

    return $error;
}

# Add changelog target
sub ACTION_changelog
{
    my $self = shift;

    # Check that this is a git repository
    unless(-d '.git') {
        $self->log_info('ChangeLog can only be generated from a git '.
                         "repository\n");
        return 1;
    }

    unless(system("git", "submodule", "update") == 0) {
        $self->log_info("Failed to initialise git2cl: $@\n");
        return 1;
    }

    my $git2cl;
    unless(open($git2cl, '-|', './git2cl/git2cl')) {
        $self->log_info("Failed to execute git2cl: $!");
        return 1;
    }

    $self->log_info("Creating ChangeLog\n");

    my $changelog;
    unless(open($changelog, '>', 'ChangeLog')) {
        $self->log_info("Failed to open ChangeLog for writing: $!\n");
        return 1;
    }

    while(<$git2cl>) {
        # Replace leading tabs with spaces
        s/^(\t+)/' ' x (length($1) * 8)/e;

        # Remove trailing whitespace
        s/\s+$//;

        # Write the output to the ChangeLog
        print $changelog $_."\n";
    }

    return 0;
}

# ChangeLog is part of distmeta
sub ACTION_distmeta
{
    my $self = shift;

    # Add a dependency on changelog
    $self->depends_on('changelog') == 0 or return 1;

    # Run the regular distmeta action
    return $self->SUPER::ACTION_distmeta;
}

# Include version.ks in p2v-image-builder in dist
sub ACTION_dist
{
    my $self = shift;

    # Add a dependency on p2vimagebuilder
    $self->depends_on('p2vimagebuilder') == 0 or return 1;

    return $self->SUPER::ACTION_dist;
}

# Make Build test depend on distmeta so syntax test succeeds
sub ACTION_test
{
    my $self = shift;

    # Ensure distmeta runs first
    $self->depends_on('distmeta') == 0 or return 1;

    # Run the regular test action
    return $self->SUPER::ACTION_test;
}

# Display the version number
sub ACTION_version
{
    my $self = shift;

    my $version = $self->{properties}->{dist_version};
    $version =~ /^v(.*)$/ and $version = $1;

    print "$version\n";
    return 0;
}

SUBCLASS

my $build = $class->new (
    license      => 'gpl',
    dist_name    => 'virt-v2v',
    dist_version_from => 'lib/Sys/VirtConvert.pm',
    confdoc_files => [ 'v2v/virt-v2v.conf.pod' ],
    install_path => { 'locale' => '/usr/local/share/locale' },
    script_files => [ 'v2v/virt-v2v.pl', 'p2v/server/virt-p2v-server.pl' ],
    meta_add => {
        resources => {
          license => "http://www.gnu.org/licenses/gpl.html",
          homepage =>  "http://people.redhat.com/mbooth/virt-v2v/",
          repository => "git://git.fedorahosted.org/virt-v2v.git",
          MailingList => "http://www.redhat.com/mailman/listinfo/libguestfs",
        },
    },
    syntaxcheck_exclude => [ "COPYING", "COPYING.LIB", "README-NLS" ],
    PL_files => [ 'virt-v2v.spec.PL' ],
);

$build->add_build_element('confdoc');
$build->add_build_element('locale');
$build->create_build_script();
