# Sys::VirtV2V::Config
# Copyright (C) 2009 Red Hat Inc.
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

package Sys::VirtV2V::Config;

use strict;
use warnings;

use File::Spec;
use File::stat;
use XML::DOM;
use XML::DOM::XPath;

use Sys::VirtV2V::ExecHelper;
use Sys::VirtV2V::UserMessage qw(user_message);

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtV2V::Config - Manage virt-v2v's configuration file

=head1 SYNOPSIS

 use Sys::VirtV2V::Config;

 $eh = Sys::VirtV2V::Config->new($config_path);

 my $isopath = $config->get_transfer_iso();
 my ($path, $deps) = $config->match_app($desc, $name, $arch);
 my ($name, $type) = $config->map_network($oldname, $oldtype);

=head1 DESCRIPTION

Sys::VirtV2V::Config parses and queries the virt-v2v config file.

=head1 METHODS

=over

=item new(path)

Create a new Sys::VirtV2V::Config object to operate on the config file at
I<path>.

=cut

sub new
{
    my $class = shift;
    my ($path) = @_;

    my $self = {};
    bless($self, $class);

    # No further config required if no config path was specified
    return $self if (!defined($path));

    die(user_message(__x("Config file {path} doesn't exist",
                         path => $path))) unless (-e $path);

    die(user_message(__x("Don't have permissions to read {path}",
                         path => $path))) unless (-r $path);

    eval {
        $self->{dom} = new XML::DOM::Parser->parsefile($path);
    };

    die(user_message(__x("Unable to parse config file {path}: {error}",
                         path => $path, error => $@))) if ($@);

    $self->{path} = $path;
    return $self;
}

=item get_transfer_iso

Return the path to an iso image containing all software defined in the config
file. Returns undef if no transfer iso is required.

=cut

sub get_transfer_iso
{
    my $self = shift;

    my $dom = $self->{dom};

    return undef unless (defined($dom));

    # path-root doesn't have to be defined
    my ($root) = $dom->findnodes('/virt-v2v/path-root/text()');
    $root = $root->getData() if (defined($root));

    # Construct a list of path arguments to mkisofs from paths referenced in the
    # config file
    # We use a hash here to avoid duplicates
    my %path_args;
    my %paths;
    foreach my $path ($dom->findnodes('/virt-v2v/app/path/text()')) {
        $path = $path->getData();

        # Get the absolute path if iso-root was defined
        my $abs;
        if (defined($root)) {
            $abs = File::Spec->catfile($root, $path);
        } else {
            $abs = $path;
        }

        if (-r $abs) {
            $path_args{"$path=$abs"} = 1;
            $paths{$abs} = 1;
        }
    }

    # Nothing further to do if there are no paths
    return if (keys(%path_args) == 0);

    # Get the path of the transfer iso
    my ($iso_path) = $dom->findnodes('/virt-v2v/iso-path/text()');

    # We need this
    die(user_message(__"<iso-path> must be specified in the configuration ".
                       "file")) unless (defined($iso_path));
    $iso_path = $iso_path->getData();

    # Check if the transfer iso exists, and is newer than the config file
    if (-e $iso_path) {
        my $iso_st = stat($iso_path)
            or die(user_message(__x("Unable to stat {path}: {error}",
                                    path => $iso_path, error => $!)));

        my $config_st = stat($self->{path})
            or die(user_message(__x("Unable to stat {path}: {error}",
                                    path => $self->{path}, error => $!)));

        if ($iso_st->mtime > $config_st->mtime) {
            my $rebuild = 0;

            my %dirs;
            foreach my $path (keys(%paths)) {
                my $path_st = stat($path);

                if ($path_st->mtime > $iso_st->mtime) {
                    $rebuild = 1;
                    last;
                }

                # Also check if the containing directory has been updated. This
                # will pick up the case where a file with an old timestamp has
                # been moved into a directory.
                my (undef, $dir, undef) = File::Spec->splitpath($path);
                if (!exists($dirs{$dir})) {
                    my $dir_st = stat($dir);
                    if ($dir_st->mtime > $iso_st->mtime) {
                        $rebuild = 1;
                        last;
                    }
                    $dirs{$dir} = 1;
                }
            }

            return $iso_path if (!$rebuild);
        }
    }

    # Re-create the transfer iso
    my $eh = Sys::VirtV2V::ExecHelper->run
        ('mkisofs', '-o', $iso_path,
         '-r', '-J',
         '-V', '__virt-v2v_transfer__',
         '-graft-points', keys(%path_args));
    die(user_message(__x("Failed to create transfer iso. ".
                         "Command output was:\n{output}",
                         output => $eh->output()))) unless ($eh->status() == 0);

    return $iso_path;
}

sub _get_app_search
{
    my ($desc, $name, $arch) = @_;

    my $os     = $desc->{os};
    my $distro = $desc->{distro};
    my $major  = $desc->{major_version};
    my $minor  = $desc->{minor_version};

    my $search = "os='$os'";
    $search .= " name='$name'";
    $search .= " distro='$distro'" if (defined ($distro));
    $search .= " major='$major'" if (defined($major));
    $search .= " minor='$minor'" if (defined($minor));
    $search .= " arch='$arch'";

    return $search;
}

=item match_app

Return a matching app entry from the virt-v2v configuration. The entry is
returned as a list containing 2 values. The first contains the path to the
application itself. The second contains an arrayref containing the paths of all
the app's listed dependencies.

=cut

sub match_app
{
    my $self = shift;

    my ($desc, $name, $arch) = @_;

    my $dom = $self->{dom};

    die(user_message(__x("No config specified. No app match for {search}",
                         search => _get_app_search($desc, $name, $arch))))
        unless (defined($dom));

    my $os     = $desc->{os};
    my $distro = $desc->{distro};
    my $major  = $desc->{major_version};
    my $minor  = $desc->{minor_version};

    # Check we've got at least the {os} field from OS detection.
    die(user_message(__"Didn't detect operating system"))
        unless defined $os;

    # Create a list of xpath queries against the config which look for a
    # matching <app> config entry in descending order of specificity
    my @queries;
    if (defined($major)) {
        if (defined($minor)) {
            push(@queries, _app_query($name, $os, $distro, $major, $minor, $arch));
            push(@queries, _app_query($name, $os, $distro, $major, $minor, undef));
        }

        push(@queries, _app_query($name, $os, $distro, $major, undef, $arch));
        push(@queries, _app_query($name, $os, $distro, $major, undef, undef));
    }

    push(@queries, _app_query($name, $os, $distro, undef, undef, $arch));
    push(@queries, _app_query($name, $os, $distro, undef, undef, undef));

    # Use the results of the first query which returns a result
    my $app;
    foreach my $query (@queries) {
        ($app) = $dom->findnodes($query);
        last if (defined($app));
    }

    die(user_message(__x("No app in config matches {search}",
                         search => _get_app_search($desc, $name, $arch))))
        unless (defined($app));

    my %app;
    my ($path) = $app->findnodes('path/text()');
    die(user_message(__x("app entry in config doesn't contain a path: {xml}",
                         xml => $app->toString()))) unless (defined($path));
    $path = $path->getData();

    my @deps;
    foreach my $dep ($app->findnodes('dep/text()')) {
        push(@deps, $dep->getData());
    }

    return ($path, \@deps);
}

sub _app_query
{
    my ($name, $os, $distro, $major, $minor, $arch) = @_;

    my $query = "/virt-v2v/app[\@name='$name' and \@os='$os' and ";
    $query .= defined($distro) ? "\@distro='$distro'" : 'not(@distro)';
    $query .= ' and ';
    $query .= defined($major) ? "\@major='$major'" : 'not(@major)';
    $query .= ' and ';
    $query .= defined($minor) ? "\@minor='$minor'" : 'not(@minor)';
    $query .= ' and ';
    $query .= defined($arch) ? "\@arch='$arch'" : 'not(@arch)';
    $query .= ']';

    return $query;
}

=item map_network(oldname, oldtype)

Return a new network name/type for I<oldname> and I<oldtype> from the config.
Returns a list of 2 values: (I<name>, I<type>)

=cut

sub map_network
{
    my $self = shift;
    my ($oldname, $oldtype) = @_;

    my $dom = $self->{dom};

    my $mapping;
    if (defined($dom)) {
        ($mapping) = $dom->findnodes
            ("/virt-v2v/network[\@type='$oldtype' and \@name='$oldname']".
             "/network");
    }

    unless (defined($mapping)) {
        # Return the default if it was specified
        return @{$self->{default_net_mapping}}
            if (defined($self->{default_net_mapping}));

        print STDERR user_message(__x("WARNING: No mapping found for ".
                                      "{type} interface {name} in config ".
                                      "file. The converted guest may not ".
                                      "start until its network interface is ".
                                      "updated.",
                                      type => $oldtype,
                                      name => $oldname));
        return;
    }

    my $newtype = $mapping->getAttributeNode('type');
    $newtype &&= $newtype->getValue();
    my $newname = $mapping->getAttributeNode('name');
    $newname &&= $newname->getValue();

    # Check type and name are defined for the mapping
    unless (defined($newtype) && defined($newname)) {
        print STDERR user_message(__x("WARNING: Invalid network ".
                                      "mapping in config: {config}",
                                      config => $mapping->toString()));
        return;
    }

    # Check type is something we recognise
    unless ($newtype eq 'network' || $newtype eq 'bridge') {
        print STDERR user_message(__x("WARNING: Unknown interface type ".
                                      "{type} in network mapping: {config}",
                                      type => $newtype,
                                      config => $mapping->toString()));
    }

    return ($newname, $newtype);
}

=item set_default_net_mapping(name, type)

Set the default network name and type which will be used if no specific mapping
can be found in a config file.

=cut

sub set_default_net_mapping
{
    my $self = shift;

    my ($name, $type) = @_;

    $self->{default_net_mapping} = [ $name, $type ];
}

=back

=head1 COPYRIGHT

Copyright (C) 2010 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
