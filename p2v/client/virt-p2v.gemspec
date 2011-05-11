# Copyright (C) 2011 Red Hat Inc.
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
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

V2V_VERSION = `../../Build version`
abort "Failed to get version" unless $? == 0

GEMSPEC = Gem::Specification.new do |s|
    s.name = %q{virt-p2v}
    s.version = V2V_VERSION

    s.authors = ["Matthew Booth"]
    s.date = %q{2011-05-10}
    s.summary = %q{Send a machine's storage and metadata to virt-p2v-server}
    s.description = %q{
        virt-p2v is a client which connects to a virt-p2v-server and transfer's
        the host machine's storage and metadata. virt-p2v is intended to be run
        from a live image, so it is unlikely you want to install it.
    }
    s.email = %q{libguestfs@redhat.com}
    s.homepage = %q{http://libguestfs.org}

    s.default_executable = %q{virt-p2v}
    s.executables = ["virt-p2v"]
    s.files = [
        "Rakefile",
        "bin/virt-p2v",
        "lib/virt-p2v/blockdevice.rb",
        "lib/virt-p2v/connection.rb",
        "lib/virt-p2v/converter.rb",
        "lib/virt-p2v/gtk-queue.rb",
        "lib/virt-p2v/netdevice.rb",
        "lib/virt-p2v/ui/connect.rb",
        "lib/virt-p2v/ui/convert.rb",
        "lib/virt-p2v/ui/main.rb",
        "lib/virt-p2v/ui/network.rb",
        "lib/virt-p2v/ui/p2v.ui",
        "lib/virt-p2v/ui/success.rb",
        "virt-p2v.gemspec",
        "Manifest"
    ]
    s.require_paths = ["lib"]

    if s.respond_to? :specification_version then
        current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
        s.specification_version = 3

        if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
            s.add_runtime_dependency(%q<gtk2>, [">= 0"])
            s.add_runtime_dependency(%q<gettext>, [">= 0"])
            s.add_runtime_dependency(%q<net-ssh>, [">= 0"])
        else
            s.add_dependency(%q<gtk2>, [">= 0"])
            s.add_dependency(%q<gettext>, [">= 0"])
            s.add_dependency(%q<net-ssh>, [">= 0"])
        end
    else
        s.add_dependency(%q<gtk2>, [">= 0"])
        s.add_dependency(%q<gettext>, [">= 0"])
        s.add_dependency(%q<net-ssh>, [">= 0"])
    end

# Source doesn't contain any rdoc
#    s.extra_rdoc_files = [
#        "bin/virt-p2v",
#        "lib/virt-p2v/blockdevice.rb",
#        "lib/virt-p2v/connection.rb",
#        "lib/virt-p2v/converter.rb",
#        "lib/virt-p2v/gtk-queue.rb",
#        "lib/virt-p2v/netdevice.rb",
#        "lib/virt-p2v/ui/connect.rb",
#        "lib/virt-p2v/ui/convert.rb",
#        "lib/virt-p2v/ui/main.rb",
#        "lib/virt-p2v/ui/network.rb",
#        "lib/virt-p2v/ui/p2v.ui",
#        "lib/virt-p2v/ui/success.rb"
#    ]
#    s.rdoc_options = [
#        "--line-numbers",
#        "--inline-source",
#        "--title",
#        "Virt-p2v"
#    ]
end
