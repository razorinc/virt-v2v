#!/usr/bin/perl

use strict;
use warnings;

use Module::Build;

my $class = Module::Build->subclass(
    class => 'Module::Build::VirtV2V',
    code => <<'SUBCLASS' );

__PACKAGE__->add_property('confdoc_files');
__PACKAGE__->add_property('syntaxcheck_exclude');

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
        open($fh, '<', $file) or die("Unable to read manifest file $file: $!");

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

    # Check that git is available
    eval "use Git;";
    if($@) {
        $self->log_info("Git is not installed\n");
        return 1;
    }

    # Update submodules to get git2cl
    my $repo = Git->repository(Directory => '.');

    eval {
        $repo->command_noisy('submodule', 'update');
    };

    if($@) {
        $self->log_info("Failed to initialise git2cl\n");
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
    $self->depends_on('changelog');

    # Run the regular distmeta action
    $self->SUPER::ACTION_distmeta;
}

# Make Build test depend on distmeta so syntax test succeeds
sub ACTION_test
{
    my $self = shift;

    # Ensure distmeta runs first
    $self->depends_on('distmeta');

    # Run the regular test action
    $self->SUPER::ACTION_test;
}

SUBCLASS

my $build = $class->new (
    license      => 'gpl',
    dist_name    => 'virt-v2v',
    dist_version_from => 'lib/Sys/VirtV2V.pm',
    confdoc_files => [ 'v2v/virt-v2v.conf.pod' ],
    install_path => { 'locale' => '/usr/local/share/locale' },
    script_files => [ 'snapshot/virt-snapshot.pl', 'v2v/virt-v2v.pl' ],
    meta_add => {
        resources => {
          license => "http://www.gnu.org/licenses/gpl.html",
          homepage =>  "http://people.redhat.com/mbooth/virt-v2v/",
          repository => "http://gitorious.org/virt-v2v/virt-v2v",
          MailingList => "http://www.redhat.com/mailman/listinfo/libguestfs",
        },
    },
    syntaxcheck_exclude => [ "COPYING", "COPYING.LIB", "README-NLS" ]
);

$build->add_build_element('confdoc');
$build->add_build_element('locale');
$build->create_build_script();
