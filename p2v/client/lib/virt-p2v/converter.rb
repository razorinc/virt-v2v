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

require 'gettext'
require 'rexml/document'
include REXML

require 'virt-p2v/netdevice'
require 'virt-p2v/blockdevice'

module VirtP2V

# NIC
# hostname
# username
# password

# name          User entry
# memory        Editable
# cpus          Editable
# arch          Detected: cpuflags contains lm (long mode)
# features      Detected: apic, acpi, pae
# disks         Editable, default to all
#   device        Detected
#   path          Detected
#   is_block      1
#   format        raw
# removables    Editable, default to all
#   device        Detected
#   type          Detected
# nics          Editable, default to all connected
#   mac           Detected, option to generate new
#   vnet          Set to nic name
#   vnet_type     bridge

class Converter
    include GetText

    attr_accessor :profile, :name, :cpus, :memory, :arch
    attr_reader :features, :disks, :removables, :nics

    attr_reader :connection

    def on_connection(&cb)
        @connection_listeners << cb
    end

    def connection=(connection)
        @connection = connection
        @connection_listeners.each { |cb|
            cb.call(connection)
        }
    end

    def convert(status, progress, &completion)
        iterate([
            lambda { |cb| @connection.set_profile(@profile, &cb) },
            lambda { |cb| @connection.metadata(meta, &cb) },
            lambda { |cb|
                iterate(@disks.map { |dev|
                    lambda { |cb2|
                        disk(dev, status, progress, cb2)
                    }
                }, cb)
            },
            lambda { |cb|
                status.call(_('Converting'))
                @connection.convert(&cb)
            }
        ], completion)
    end

    private

    def initialize()
        @profile = nil
        @connection = nil
        @connection_listeners = []

        # Initialize basic system information
        @name = '' # There's no reasonable default for this

        # Get total memory from /proc/meminfo
        File.open('/proc/meminfo', 'r') do |fd|
            fd.each { |line|
                next unless line =~ /^MemTotal:\s+(\d+)\b/

                @memory = Integer($~[1]) * 1024
                break
            }
        end

        # Get the total number of cpu threads from hwloc-info
        hwloc = Document.new `hwloc-info --of xml`
        @cpus = XPath.match(hwloc, "//object[@type='PU']").length

        # Get cpu architecture and features from the first flags entry in
        # /proc/cpuinfo
        File.open('/proc/cpuinfo', 'r') do |fd|
            fd.each { |line|
                next unless line =~ /^flags\s*:\s(.*)$/

                flags = $~[1]

                # x86_64 if flags contains lm (long mode), i686 otherwise. We
                # don't support anything else.
                @arch = flags =~ /\blm\b/ ? 'x86_64' : 'i686'

                # Pull some select features from cpu flags
                @features = []
                [ 'apic', 'acpi', 'pae' ].each { |f|
                    @features << f if flags =~ /\b#{f}\b/
                }
                break
            }
        end

        # Initialise empty lists for optional devices. These will be added
        # according to the user's selection
        @disks = []
        @removables = []
        @nics = []
    end

    def disk(dev, status, progress, completion)
        path = "/dev/#{dev}"
        # XXX: No error checking of blockdev execution
        size = Integer(`blockdev --getsize64 #{path}`.chomp)
        status.call(_("Transferring #{dev}"))
        iterate([
            lambda { |cb| @connection.path(size, path, &cb) },
            lambda { |cb| @connection.container('RAW', &cb) },
            lambda { |cb|
                io = nil
                begin
                    io = File.new(path, 'r')
                rescue => ex
                    cb.call(ex)
                end
                pc = 0
                @connection.send_data(io, size, lambda { |total|
                    npc = Float(total) * 100 / size
                    # Only update the progress if it has increased by
                    # at least 1%
                    if Integer(npc) > pc then
                        pc += 1
                        progress.call(dev, pc)
                    end
                }, &cb)
            }
        ], completion)
    end

    def iterate(stages, completion)
        i = 0
        cb = lambda { |result|
            if result.kind_of?(Exception) then
                completion.call(result)
            else
                i += 1
                if i == stages.length then
                    completion.call(true)
                else
                    stages[i].call(cb)
                end
            end
        }
        stages[0].call(cb)
    end

    def meta
        {
            'name'      => @name,
            'cpus'      => @cpus,
            'memory'    => @memory,
            'arch'      => @arch,
            'features'  => @features,
            'disks'     => @disks.map { |device|
                {
                    'device'    => device,
                    'path'      => "/dev/#{device}",
                    'is_block'  => '1',
                    'format'    => 'raw'
                }
            },
            'removables' => @removables.map { |device|
                removable = RemovableBlockDevice[device]
                {
                    'device'    => removable.device,
                    'type'      => removable.type
                }
            },
            'nics'      => @nics.map { |device|
                nic = NetworkDevice[device]
                {
                    'mac'       => nic.mac,
                    'vnet'      => nic.name,
                    'vnet_type' => 'bridge'
                }
            }
        }
    end
end

end
