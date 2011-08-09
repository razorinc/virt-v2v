# Sys::VirtConvert::Config
# Copyright (C) 2009-2011 Red Hat Inc.
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

package Sys::VirtConvert::Config;

use strict;
use warnings;

use Carp;
use File::Spec;
use File::stat;
use File::Temp;
use XML::DOM;
use XML::DOM::XPath;

use Sys::VirtConvert::ExecHelper;
use Sys::VirtConvert::Util;

use Locale::TextDomain 'virt-v2v';

=pod

=head1 NAME

Sys::VirtConvert::Config - Manage virt-v2v's configuration file

=head1 SYNOPSIS

 use Sys::VirtConvert::Config;

 $eh = Sys::VirtConvert::Config->new(@config_paths);

 my $isopath = $config->get_transfer_iso();
 my ($path, $deps) = $config->match_app($desc, $name, $arch);
 my ($name, $type) = $config->map_network($oldname, $oldtype);

=head1 DESCRIPTION

Sys::VirtConvert::Config parses and queries virt-v2v configuration files.

=head1 METHODS

=over

=item new(paths)

Create a new Sys::VirtConvert::Config object to operate on the configuration
files I<paths>.

=cut

sub new
{
    my $class = shift;
    my (@paths) = @_;

    my $self = {};
    bless($self, $class);

    # No further config required if no config paths were specified
    return $self if (@paths < 0);

    my @doms;
    foreach my $path (@paths) {
        v2vdie __x('Config file {path} doesn\'t exist', path => $path)
            unless -e $path;
        v2vdie __x('Don\'t have permissions to read {path}', path => $path)
            unless -r $path;

        eval {
            push(@doms, new XML::DOM::Parser->parsefile($path));
        };
        v2vdie __x('Unable to parse config file {path}: {error}',
                   path => $path, error => $@) if $@;
    }

    $self->{doms} = \@doms;

    foreach my $dom (@doms) {
        my ($net_default) = $dom->findnodes
            ('/virt-v2v/network[@type=\'default\']');
        if (defined($net_default)) {
            $self->_parse_net_default($net_default);
            last;
        }
    }

    return $self;
}

sub _trim
{
    my $s = shift;

    $s =~ s/^\s+//;
    $s =~ s/\s+$//;

    return $s;
}

=item get_transfer_iso

Return the path to an iso image containing all software defined in the config
file. Returns undef if no transfer iso is required.

=cut

sub get_transfer_iso
{
    my $self = shift;

    if (exists($self->{iso})) {
        my $iso = $self->{iso};
        return defined($iso) ? $iso->filename() : undef;
    }

    # Construct a list of path arguments to mkisofs from paths referenced in the
    # config file
    # We use a hash here to avoid duplicates
    my %path_args;
    my %paths;

    foreach my $dom (@{$self->{doms}}) {
        _get_transfer_iso_dom($dom, \%path_args, \%paths);
    }

    # Nothing further to do if there are no paths
    if (keys(%path_args) == 0) {
        $self->{iso} = undef;
        return undef;
    }

    # Create a temporary file for the transfer ISO
    my $iso = File::Temp->new();
    chmod(0644, $iso->filename());

    # Create the transfer iso
    my $eh = Sys::VirtConvert::ExecHelper->run
        ('mkisofs', '-o', $iso->filename(),
         '-r', '-J',
         '-V', '__virt-v2v_transfer__',
         '-graft-points', keys(%path_args));
    v2vdie __x("Failed to create transfer iso. Command output was:\n{output}",
               output => $eh->output()) unless $eh->status() == 0;

    $self->{iso} = $iso;
    return $iso->filename();
}

sub _get_transfer_iso_dom
{
    my ($dom, $path_args, $paths) = @_;

    # path-root doesn't have to be defined
    my ($root) = $dom->findnodes('/virt-v2v/path-root/text()');
    $root = _trim($root->getData()) if (defined($root));

    foreach my $path ($dom->findnodes('/virt-v2v/app/path/text()')) {
        $path = _trim($path->getData());

        my $abs;
        if (File::Spec->file_name_is_absolute($path) || !defined($root)) {
            $abs = $path;
        }

        # Make relative paths relative to iso-root if it was defined
        else {
            $abs = File::Spec->catfile($root, $path);
        }

        if (-r $abs) {
            $path_args->{"$path=$abs"} = 1;
            $paths->{$abs} = 1;
        }
    }
}

=item get_transfer_path(path)

Return the path to I<path> as accessible by the libguestfs appliance. This
function will also ensure that the transfer iso is mounted.

=cut

sub get_transfer_path
{
    my $self = shift;
    my ($path) = @_;

    if (!exists($self->{transfer_mount})) {
        use Carp 'confess';
        confess 'get_transfer_path with no transfer mount';
    }

    return File::Spec->catfile($self->{transfer_mount}, $path);
}

=item mount_transfer(g)

Mount the transfer iso if it is not already mounted, and return the path where
it was mounted.

=cut

sub mount_transfer
{
    my $self = shift;
    my ($g) = @_;

    return $self->{transfer_mount} if exists($self->{transfer_mount});

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
            $self->unmount_transfer($g);
        });
    }

    return $self->{transfer_mount};
}

=item unmount_transfer(g)

Unmount the transfer iso if it is currently mounted.

=cut

sub unmount_transfer
{
    my $self = shift;
    my ($g) = @_;

    return unless exists($self->{transfer_mount});

    $g->umount($self->{transfer_mount});
    $g->rmdir($self->{transfer_mount});

    delete($self->{transfer_mount});
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

    my $app = $self->_match_element('app', $desc, $name, $arch);

    my %app;
    my ($path) = $app->findnodes('path/text()');
    v2vdie __x('app entry in config doesn\'t contain a path: {xml}',
               xml => $app->toString()) unless defined($path);
    $path = _trim($path->getData());

    my @deps;
    foreach my $dep ($app->findnodes('dep/text()')) {
        push(@deps, _trim($dep->getData()));
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
            $props{$prop} = $val->getData() if defined($val);
        }

        v2vdie __x('Capability in config contains a dependency '.
                   'with no {property} attribute: {xml}',
                   property => 'name', xml => $cap->toString())
            unless exists($props{name});

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

    v2vdie __x('No config specified. No {type} match for {search}.',
               type => $type, search => _get_search($desc, $name, $arch))
        if @{$self->{doms}} == 0;

    foreach my $dom (@{$self->{doms}}) {
        my $match = _match_element_dom($dom, $type, $desc, $name, $arch);
        return $match if defined($match);
    }

    v2vdie __x('No {type} in config matches {search}',
               type => $type, search => _get_search($desc, $name, $arch));
}

sub _match_element_dom
{
    my ($dom, $type, $desc, $name, $arch) = @_;

    my $os     = $desc->{os};
    my $distro = $desc->{distro};
    my $major  = $desc->{major_version};
    my $minor  = $desc->{minor_version};

    # Check we've got at least the {os} field from OS detection.
    v2vdie __('Didn\'t detect operating system') unless defined $os;

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

    return undef;
}

=item map_network(oldname, oldtype)

Return a new network name/type for I<oldname> and I<oldtype> from the config.
Returns a list of 2 values: (I<name>, I<type>)

=cut

sub map_network
{
    my $self = shift;
    my ($oldname, $oldtype) = @_;

    my @search = ();
    foreach my $dom (@{$self->{doms}}) {
        push(@search, $dom->findnodes('/virt-v2v'));
    }
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

        logmsg WARN, __x('No mapping found for {type} interface '.
                         '{name} in config file. The converted guest may '.
                         'not start until its network interface is updated.',
                         type => $oldtype, name => $oldname);
        return;
    }

    my $newtype = $mapping->getAttributeNode('type');
    $newtype &&= $newtype->getValue();
    my $newname = $mapping->getAttributeNode('name');
    $newname &&= $newname->getValue();

    # Check type and name are defined for the mapping
    unless (defined($newtype) && defined($newname)) {
        logmsg WARN, __x('Invalid network mapping in config: {config}.',
                         config => $mapping->toString());
        return;
    }

    # Check type is something we recognise
    unless ($newtype eq 'network' || $newtype eq 'bridge') {
        logmsg WARN, __x('Unknown interface type '.
                         '{type} in network mapping: {config}',
                         type => $newtype, config => $mapping->toString());
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

    my $profile;
    foreach my $dom (@{$self->{doms}}) {
        ($profile) = $dom->findnodes("/virt-v2v/profile[\@name='$name']");
        last if defined($profile);
    }
    v2vdie __x('There is no profile named {name}',
               name => $name) unless defined($profile);
    $self->{profile} = $profile;

    my ($method) = $profile->findnodes('method/text()');
    v2vdie __x('Profile {name} doesn\'t specify an output method.',
               name => $name) unless defined($method);
    $self->{output_method} = _trim($method->getData());

    my ($storage) = $profile->findnodes('storage');
    if (defined($storage)) {
        my ($location) = $storage->findnodes('text()');
        $self->{output_storage} = _trim($location->getData())
            if defined($location);

        my %opts;
        $self->{output_storage_opts} = \%opts;

        my ($format) = $storage->getAttributeNode('format');
        $opts{format} = $format->getValue() if defined($format);

        my ($allocation) = $storage->getAttributeNode('allocation');
        $opts{allocation} = $allocation->getValue() if defined($allocation);
    }
    v2vdie __x('Profile {name} doesn\'t specify output storage.', name => $name)
        unless defined($self->{output_storage});

    my ($net_default) = $profile->findnodes('network[@type=\'default\']');
    $self->_parse_net_default($net_default) if defined($net_default);
}

sub _parse_net_default
{
    my $self = shift;
    my ($default) = @_;

    my ($mapping) = $default->findnodes('network');
    v2vdie __x('Default network doesn\'t contain a mapping: {config}.',
               config => $default->toString()) unless defined($mapping);

    my ($map_name) = $mapping->getAttributeNode('name');
    $map_name &&= $map_name->getValue();
    my ($map_type) = $mapping->getAttributeNode('type');
    $map_type &&= $map_type->getValue();

    # Check type and name are defined for the mapping
    unless (defined($map_name) && defined($map_type)) {
        logmsg WARN, __x('Invalid network mapping: {config}',
                         config => $default->toString());
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

    return $self->{output_storage};
}

=item get_storage_opts

Return a hashref of storage options from the selected profile.

I<use_profile> must have been called previously.

=cut

sub get_storage_opts
{
    my $self = shift;

    croak "get_storage_opts called without profile"
        unless defined($self->{profile});

    return $self->{output_storage_opts};
}

=item list_profiles

Return a list of defined profile names

=cut

sub list_profiles
{
    my $self = shift;

    return map { $_->getValue() }
           map { $_->findnodes('/virt-v2v/profile/@name') } @{$self->{doms}};
}

=back

=head1 COPYRIGHT

Copyright (C) 2009-2011 Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<virt-v2v(1)>,
L<http://libguestfs.org/>.

=cut

1;
