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

use Carp;
use File::Spec;
use File::stat;
use XML::DOM;
use XML::DOM::XPath;

use Sys::VirtV2V::ExecHelper;
use Sys::VirtV2V::Util qw(user_message);

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

    die user_message(__x("Config file {path} doesn't exist", path => $path))
        unless (-e $path);
    die user_message(__x("Don't have permissions to read {path}",
                         path => $path))
        unless (-r $path);

    eval {
        $self->{dom} = new XML::DOM::Parser->parsefile($path);
    };
    die user_message(__x("Unable to parse config file {path}: {error}",
            path => $path, error => $@)) if $@;

    my ($net_default) = $self->{dom}->findnodes
        ('/virt-v2v/network[@type=\'default\']');
    $self->_parse_net_default($net_default) if defined($net_default);

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

    return $self->{iso} if (exists($self->{iso}));

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

        my $abs;
        if (File::Spec->file_name_is_absolute($path) || !defined($root)) {
            $abs = $path;
        }

        # Make relative paths relative to iso-root if it was defined
        else {
            $abs = File::Spec->catfile($root, $path);
        }

        if (-r $abs) {
            $path_args{"$path=$abs"} = 1;
            $paths{$abs} = 1;
        }
    }

    # Nothing further to do if there are no paths
    if (keys(%path_args) == 0) {
        $self->{iso} = undef;
        return undef;
    }

    # Get the path of the transfer iso
    my ($iso_path) = $dom->findnodes('/virt-v2v/iso-path/text()');

    # We need this
    die user_message(__"<iso-path> must be specified in the configuration file")
        unless defined($iso_path);
    $iso_path = $iso_path->getData();

    # Create the transfer iso
    my $eh = Sys::VirtV2V::ExecHelper->run
        ('mkisofs', '-o', $iso_path,
         '-r', '-J',
         '-V', '__virt-v2v_transfer__',
         '-graft-points', keys(%path_args));
    die user_message(__x("Failed to create transfer iso. ".
                         "Command output was:\n{output}",
                         output => $eh->output())) unless $eh->status() == 0;

    $self->{iso} = $iso_path;
    return $iso_path;
}

=item get_transfer_path(path)

Return the path to I<path> as accessible by the libguestfs appliance. This
function will also ensure that the transfer iso is mounted.

=cut

sub get_transfer_path
{
    my $self = shift;
    my ($g, $path) = @_;

    # Check that the transfer iso is mounted
    if (!exists($self->{transfer_mount})) {
        # Existing code expects the mount to exist, but handles the case where
        # files in it don't exist. Therefore we always create the mount point,
        # but only mount anything on it if there's actually a transfer iso.

        # Create the transfer mount point
        # We create this under / because it's guaranteed to exist in the
        # appliance, regardless of the guest OS.
        $self->{transfer_mount} = $g->mkdtemp("/transferXXXXXX");

        # Only mount the transfer iso if there is one
        if (defined($self->get_transfer_iso())) {
            # Find the transfer device
            my @devices = $g->list_devices();
            my $transfer = $devices[$#devices];

            $g->mount_ro($transfer, $self->{transfer_mount});

            # Umount and remove the transfer mount point before the guestfs
            # handle is closed
            $g->add_on_close(sub {
                $g->umount($self->{transfer_mount});
                $g->rmdir($self->{transfer_mount});
            });
        }
    }

    return File::Spec->catfile($self->{transfer_mount}, $path);
}

sub _get_search
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
    $search .= " arch='$arch'" if (defined($arch));

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

    my $app = $self->_match_element('app', $desc, $name, $arch);

    my %app;
    my ($path) = $app->findnodes('path/text()');
    die user_message(__x("app entry in config doesn't contain a path: {xml}",
                         xml => $app->toString())) unless (defined($path));
    $path = $path->getData();

    my @deps;
    foreach my $dep ($app->findnodes('dep/text()')) {
        push(@deps, $dep->getData());
    }

    return ($path, \@deps);
}

sub _match_query
{
    my ($type, $name, $os, $distro, $major, $minor, $arch) = @_;

    my $query = "/virt-v2v/".$type."[\@name='$name' and \@os='$os' and ";
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

=item match_capability

Match a capability from the configuration. Returned as a hashref containing
dependencies, where each dependency is a hashref containing:

  {capability} ->
    {name} ->       : package name
      {minversion}  : minimum required version
      {ifinstalled} : 1 if the package should be upgraded if necessary, but
                      not installed if it is not already, 0 otherwise

Returns undef if the capability was not found.

=cut

sub match_capability
{
    my $self = shift;
    my ($desc, $name, $arch) = @_;

    my $cap = $self->_match_element('capability', $desc, $name, $arch);

    my %out;
    foreach my $dep ($cap->findnodes('dep')) {
        my %props;
        foreach my $prop ('name', 'minversion') {
            my ($val) = $dep->findnodes('@'.$prop);
            $val &&= $val->getData();
            die user_message(__x("Capability in config contains a dependency ".
                                 "with no {property} attribute: {xml}",
                                 property => $prop,
                                 xml => $cap->toString())) unless defined($val);
            $props{$prop} = $val;
        }

        my ($ifinstalled) = $dep->findnodes('@ifinstalled');
        $ifinstalled &&= $ifinstalled->getData();
        if (defined($ifinstalled) &&
            ($ifinstalled eq "1" || $ifinstalled eq "yes"))
        {
            $props{ifinstalled} = 1;
        } else {
            $props{ifinstalled} = 0;
        }

        my $depname = $props{name};
        delete($props{name});

        $out{$depname} = \%props;
    }
    return \%out;
}

sub _match_element
{
    my $self = shift;
    my ($type, $desc, $name, $arch) = @_;

    my $dom = $self->{dom};

    die user_message(__x("No config specified. No {type} match for {search}",
                         type => $type,
                         search => _get_search($desc, $name, $arch)))
        unless (defined($dom));

    my $os     = $desc->{os};
    my $distro = $desc->{distro};
    my $major  = $desc->{major_version};
    my $minor  = $desc->{minor_version};

    # Check we've got at least the {os} field from OS detection.
    die user_message(__"Didn't detect operating system")
        unless (defined $os);

    # Create a list of xpath queries against the config which look for a
    # matching <app> config entry in descending order of specificity
    my @queries;
    if (defined($major)) {
        if (defined($minor)) {
            push(@queries, _match_query($type, $name, $os, $distro,
                                        $major, $minor, $arch))
                if (defined($arch));
            push(@queries, _match_query($type, $name, $os, $distro,
                                        $major, $minor, undef));
        }

        push(@queries, _match_query($type, $name, $os, $distro,
                                    $major, undef, $arch))
            if (defined($arch));
        push(@queries, _match_query($type, $name, $os, $distro,
                                    $major, undef, undef));
    }

    push(@queries, _match_query($type, $name, $os, $distro,
                                undef, undef, $arch))
        if (defined($arch));
    push(@queries, _match_query($type, $name, $os, $distro,
                                undef, undef, undef));

    # Use the results of the first query which returns a result
    foreach my $query (@queries) {
        my ($element) = $dom->findnodes($query);
        return $element if (defined($element));
    }

    die user_message(__x("No {type} in config matches {search}",
                         type => $type,
                         search => _get_search($desc, $name, $arch)));
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

    my @search = $dom->findnodes('/virt-v2v');
    # Search profile first if it's defined
    unshift(@search, $self->{profile}) if defined($self->{profile});

    my $mapping;
    foreach my $root (@search) {
        ($mapping) = $root->findnodes
            ("network[\@type='$oldtype' and \@name='$oldname']/network");

        last if defined($mapping);
    }

    unless (defined($mapping)) {
        # Return the default if it was specified
        return @{$self->{default_net_mapping}}
            if (defined($self->{default_net_mapping}));

        warn user_message(__x("WARNING: No mapping found for {type} interface ".
                              "{name} in config file. The converted guest may ".
                              "not start until its network interface is ".
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
        warn user_message(__x("WARNING: Invalid network ".
                              "mapping in config: {config}",
                              config => $mapping->toString()));
        return;
    }

    # Check type is something we recognise
    unless ($newtype eq 'network' || $newtype eq 'bridge') {
        warn user_message(__x("WARNING: Unknown interface type ".
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

=item use_profile(name)

Use the profile I<name> defined in the configuration file. Output method and
storage will be read from this profile, and any network mappings defined in it
will be used in preference to those defined at the top level of the
configuration file.

=cut

sub use_profile
{
    my $self = shift;
    my ($name) = @_;

    my ($profile) = $self->{dom}->findnodes
        ("/virt-v2v/profile[\@name='$name']");
    die user_message(__x("No profile {name} defined in {path}",
                          name => $name,
                          path => $self->{path})) unless defined($profile);
    $self->{profile} = $profile;

    my ($method) = $profile->findnodes('method/text()');
    die user_message(__x("Profile {name} doesn't specify an output method",
                         name => $name)) unless defined($method);
    $self->{output_method} = $method->getData();

    my ($storage) = $profile->findnodes('storage');
    if (defined($storage)) {
        my ($location) = $storage->findnodes('text()');
        $self->{output_storage} = $location->getData() if defined($location);

        my %opts;
        $self->{output_storage_opts} = \%opts;

        my ($format) = $storage->getAttributeNode('format');
        $opts{format} = $format->getValue() if defined($format);

        my ($allocation) = $storage->getAttributeNode('allocation');
        $opts{allocation} = $allocation->getValue() if defined($allocation);
    }
    die user_message(__x("Profile {name} doesn't specify output storage",
                          name => $name))
        unless defined($self->{output_storage});

    my ($net_default) = $profile->findnodes('network[@type=\'default\']');
    $self->_parse_net_default($net_default) if defined($net_default);
}

sub _parse_net_default
{
    my $self = shift;
    my ($default) = @_;

    my ($mapping) = $default->findnodes('network');
    die user_message(__x("Default network doesn't contain a mapping: {config}",
                          config => $default->toString()))
        unless defined($mapping);

    my ($map_name) = $mapping->getAttributeNode('name');
    $map_name &&= $map_name->getValue();
    my ($map_type) = $mapping->getAttributeNode('type');
    $map_type &&= $map_type->getValue();

    # Check type and name are defined for the mapping
    unless (defined($map_name) && defined($map_type)) {
        warn user_message(__x("WARNING: Invalid network mapping: {config}",
                              config => $default->toString()));
        return;
    }

    $self->set_default_net_mapping($map_name, $map_type);
}

=item get_method

Return the output method specified in the selected profile.

I<use_profile> must have been called previously.

=cut

sub get_method
{
    my $self = shift;

    croak "get_method called without profile" unless defined($self->{profile});

    return $self->{output_method};
}

=item get_storage

Return the output storage location and a hashref of storage options from the
selected profile.

I<use_profile> must have been called previously.

=cut

sub get_storage
{
    my $self = shift;

    croak "get_storage called without profile" unless defined($self->{profile});

    return ($self->{output_storage}, $self->{output_storage_opts});
}

=item list_profiles

Return a list of defined profile names

=cut

sub list_profiles
{
    my $self = shift;

    return map { $_->getValue() } $self->{dom}->findnodes
        ('/virt-v2v/profile/@name');
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
