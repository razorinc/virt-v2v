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
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

module VirtP2V

class NoSuchDeviceError < StandardError; end

class FixedBlockDevice
    @@devices = {}

    def self.all_devices
        @@devices.values
    end

    def self.[](device)
        raise NoSuchDeviceError unless @@devices.has_key?(device)

        @@devices[device]
    end

    attr_reader :device

    def initialize(device)
        @device = device
        @@devices[device] = self
    end
end

class RemovableBlockDevice
    @@devices = {}

    def self.all_devices
        @@devices.values
    end

    def self.[](device)
        raise NoSuchDeviceError unless @@devices.has_key?(device)

        @@devices[device]
    end

    attr_reader :device, :type

    def initialize(device, type)
        @device = device
        @type = type

        @@devices[device] = self
    end
end

def ignore_unknown_device(device)
    print "Ignoring unknown block device #{device}\n"
end

# Detect and instantiate all fixed and removable block devices in the system
begin
    # Look for block devices
    # Specifically, we look for entries in /sys/block which have a device
    # symlink and no entries in their slaves subdirectory
    Dir.foreach('/sys/block') { |dev|
        next if dev == '.' || dev == '..'

        # Skip if there's no device link
        next unless File.exists?("/sys/block/#{dev}/device")

        # Skip if the slaves subdirectory contains anything other than . and
        # ..
        begin
            next if Dir.entries("/sys/block/#{dev}/slaves").length > 2
        rescue Errno::ENOENT => ex
            # This shouldn't happen, but if it did I guess it would mean
            # there are no slave devices
        end

        # We've got a real block device. Check if it's removable or not
        File.open("/sys/block/#{dev}/removable") { |fd|
            removable = fd.gets.chomp

            # cciss device /dev/cciss/c0d0 will be cciss!c0d0 under /sys/block
            devpath = dev.gsub("!", "/")

            if removable == "0" then
                FixedBlockDevice.new(devpath)
            elsif File.exist?("/sys/block/#{dev}/device/modalias")
                # Look in device/modalias to work out what kind of removable
                # device this is
                File.open(
                    "/sys/block/#{dev}/device/modalias") \
                { |modalias_f|
                    modalias = modalias_f.gets.chomp
                    if modalias =~ /floppy/ then
                        RemovableBlockDevice.new(devpath, 'floppy')
                    elsif modalias =~ /cdrom/ then
                        RemovableBlockDevice.new(devpath, 'cdrom')
                    elsif modalias =~ /^scsi:t-/ then
                        # All this tells us is that we have a SCSI device: it
                        # could still be anything. Look at the device type to
                        # find out what it is. These values are defined in
                        # /usr/include/scsi/scsi.h
                        begin
                            File.open("/sys/block/#{dev}/device/type") \
                            { |type_f|
                                type = type_f.gets.chomp
                                # DISK or MOD
                                if type == "0" || type == "7"
                                    FixedBlockDevice.new(devpath)

                                # WORM or ROM
                                elsif type == "4" || type == "5"
                                    RemovableBlockDevice.new(devpath, 'cdrom')
                                else
                                    ignore_device(devpath)
                                end
                            }
                        rescue Errno::ENOENT
                            ignore_unknown_device(devpath)
                        end
                    else
                        ignore_unknown_device(devpath)
                    end
                }
            else
                ignore_unknown_device(devpath)
            end
        }
    }
end

end
