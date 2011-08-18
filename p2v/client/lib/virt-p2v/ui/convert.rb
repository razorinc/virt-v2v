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

require 'gtk2'

require 'virt-p2v/blockdevice'
require 'virt-p2v/netdevice'

module VirtP2V::UI::Convert
    CONVERT_PROFILE_NAME    = 0

    CONVERT_NETWORK_CONVERT = 0
    CONVERT_NETWORK_DEVICE  = 1

    CONVERT_FIXED_CONVERT   = 0
    CONVERT_FIXED_DEVICE    = 1
    CONVERT_FIXED_PROGRESS  = 2

    CONVERT_REMOVABLE_CONVERT   = 0
    CONVERT_REMOVABLE_DEVICE    = 1
    CONVERT_REMOVABLE_TYPE      = 2

    UI_STATE_INVALID    = 0
    UI_STATE_VALID      = 1
    UI_STATE_CONNECTING = 2
    UI_STATE_CONVERTING = 3
    UI_STATE_COMPLETE   = 4

    EV_VALID        = 0
    EV_BUTTON       = 1
    EV_CONNECTION   = 2
    EV_CONVERTED    = 3

    def self.init(ui, converter)
        # ListStores
        @profiles   = ui.get_object('convert_profile_list')
        @nics       = ui.get_object('convert_network_list')
        @fixeds     = ui.get_object('convert_fixed_list')
        @removables = ui.get_object('convert_removable_list')

        # Widgets
        @profile    = ui.get_object('convert_profile')
        @name       = ui.get_object('convert_name')
        @cpus       = ui.get_object('convert_cpus')
        @memory     = ui.get_object('convert_memory')
        @editable   = ui.get_object('convert_editable')
        @button     = ui.get_object('convert_button')
        @status     = ui.get_object('convert_status')

        # Get initial values from converter
        @name.text = converter.name
        @cpus.text = converter.cpus.to_s
        @memory.text = (converter.memory / 1024 / 1024).to_s

        # Populate profiles on connection
        converter.on_connection { |conn|
            conn.on_connect { |cb|
                conn.list_profiles { |profiles|
                    cb.call(RuntimeError.new('Remote server does not ' +
                                             'define any profiles in ' +
                                             '/etc/virt-v2v.conf')) \
                        if profiles.kind_of?(Exception) or profiles.empty?

                    selected = @profile.active_iter
                    selected = selected[CONVERT_PROFILE_NAME] \
                        unless selected.nil?

                    @profiles.clear
                    profiles.each { |i|
                        profile = @profiles.append
                        profile[CONVERT_PROFILE_NAME] = i
                        @profile.active_iter = profile if i == selected
                    }

                    cb.call(true)
                }
            }
        }

        VirtP2V::FixedBlockDevice.all_devices.each { |dev|
            fixed = @fixeds.append
            fixed[CONVERT_FIXED_CONVERT]    = true
            fixed[CONVERT_FIXED_DEVICE]     = dev.device
            fixed[CONVERT_FIXED_PROGRESS]   = 0
        }

        VirtP2V::RemovableBlockDevice.all_devices.each { |dev|
            rem = @removables.append
            rem[CONVERT_REMOVABLE_CONVERT]  = true
            rem[CONVERT_REMOVABLE_DEVICE]   = dev.device
            rem[CONVERT_REMOVABLE_TYPE]     = dev.type
        }

        VirtP2V::NetworkDevice.all_devices.each { |dev|
            nic = @nics.append
            nic[CONVERT_NETWORK_CONVERT]    = dev.connected
            nic[CONVERT_NETWORK_DEVICE]     = dev.name
        }

        # Event handlers
        ui.register_handler('convert_profile_changed',
                            method(:update_values))
        ui.register_handler('convert_name_changed',
                            method(:update_values))
        ui.register_handler('convert_cpus_changed',
                            method(:convert_cpus_changed))
        ui.register_handler('convert_memory_changed',
                            method(:convert_memory_changed))
        ui.register_handler('convert_fixed_list_row_changed',
                            method(:convert_fixed_list_row_changed))
        ui.register_handler('convert_removable_list_row_changed',
                            method(:update_values))
        ui.register_handler('convert_network_list_row_changed',
                            method(:update_values))
        ui.register_handler('convert_fixed_select_toggled',
                            method(:convert_fixed_select_toggled))
        ui.register_handler('convert_removable_select_toggled',
                            method(:convert_removable_select_toggled))
        ui.register_handler('convert_network_select_toggled',
                            method(:convert_network_select_toggled))
        ui.register_handler('convert_button_clicked',
                            method(:convert_button_clicked))

        @state = nil
        set_state(UI_STATE_INVALID)
        update_values

        @ui = ui
        @converter = converter
    end

    def self.event(event, status)
        case @state
        when UI_STATE_INVALID
            case event
            when EV_VALID
                set_state(UI_STATE_VALID) if status
            else
                raise "Unexpected event: #{@state} #{event}"
            end
        when UI_STATE_VALID
            case event
            when EV_VALID
                set_state(UI_STATE_INVALID) if !status
            when EV_BUTTON
                if @converter.connection.connected? then
                    set_state(UI_STATE_CONVERTING)
                    convert
                else
                    set_state(UI_STATE_CONNECTING)
                    reconnect
                end
            else
                raise "Unexpected event: #{@state} #{event}"
            end
        when UI_STATE_CONNECTING
            case event
            when EV_CONNECTION
                if status then
                    set_state(UI_STATE_CONVERTING)
                    convert
                else
                    set_state(UI_STATE_VALID)
                end
            when EV_VALID
                # update_values will be called when the profile list is cleared
                # and repopulated during connection. Untidy, but ignore it.
            else
                raise "Unexpected event: #{@state} #{event}" \
                    unless event == EV_CONNECTION
            end
        when UI_STATE_CONVERTING
            case event
            when EV_CONVERTED
                if status then
                    set_state(UI_STATE_COMPLETE)
                else
                    set_state(UI_STATE_VALID)
                end
            when EV_VALID
                # update_values will be called when the list stores are updated.
                # Untidy, but ignore it
            else
                raise "Unexpected event: #{@state} #{event}"
            end
        else
            raise "Unexpected UI state: #{@state}"
        end
    end

    def self.set_state(state)
        # Don't do anything if state hasn't changed
        return if state == @state
        @state = state

        case @state
        when UI_STATE_INVALID
            @editable.sensitive = true
            @button.sensitive = false
        when UI_STATE_VALID
            @editable.sensitive = true
            @button.sensitive = true
        when UI_STATE_CONNECTING
            @status.text = 'Failed to start virt-p2v-server on remote server'
            @editable.sensitive = false
            @button.sensitive = false
        when UI_STATE_CONVERTING
            @editable.sensitive = false
            @button.sensitive = false
        when UI_STATE_COMPLETE
            @ui.active_page = 'success_win'

            # ... then leave this one as we hope to find it if we come back here
            set_state(UI_STATE_VALID)
        else
            raise "Attempt to set unexpected UI state: #{@state}"
        end
    end

    def self.convert
        @converter.convert(
            # status
            lambda { |msg|
                @status.text = msg
            },
            # progress
            lambda { |dev, progress|
                @fixeds.each { |model, path, iter|
                    next unless iter[CONVERT_FIXED_DEVICE] == dev

                    iter[CONVERT_FIXED_PROGRESS] = progress
                    break
                }
            }
        ) { |result|
            # N.B. Explicit test against true is required here, as result may be
            # an Exception, which would also return true if evaluated alone
            if result == true then
                @status.text = ''
                event(EV_CONVERTED, true)
            else
                @status.text = result.message
                event(EV_CONVERTED, false)
            end
        }
    end

    def self.reconnect
        @status.text = 'Reconnecting'
        @converter.connection.connect { |result|
            if result == true then
                event(EV_CONNECTION, true)
            else
                @status.text =
                    'Failed to start virt-p2v-server on remote server'
                event(EV_CONNECTION, false)
            end
        }
    end

    def self.convert_fixed_list_row_changed(model, path, iter)
        update_values
    end

    class InvalidUIState < StandardError; end

    def self.update_values
        valid = nil
        begin
            # Check there's a profile selected
            profile = @profile.active_iter
            raise InvalidUIState if profile.nil?
            @converter.profile = profile[CONVERT_PROFILE_NAME]

            # Check there's a name set
            name = @name.text
            raise InvalidUIState if name.nil? || name.strip.length == 0
            @converter.name = name

            # Check cpus and memory are set and numeric
            cpus = @cpus.text
            raise InvalidUIState if cpus.nil?
            cpus = Integer(cpus) rescue nil
            raise InvalidUIState if cpus.nil?
            @converter.cpus = cpus

            memory = @memory.text
            raise InvalidUIState if memory.nil?
            memory = Integer(memory) rescue nil
            raise InvalidUIState if memory.nil?
            @converter.memory = memory * 1024 * 1024

            # Check that at least 1 fixed storage device is selected
            fixed = false
            @converter.disks.clear
            @fixeds.each { |model, path, iter|
                if iter[CONVERT_FIXED_CONVERT] then
                    fixed = true
                    @converter.disks << iter[CONVERT_FIXED_DEVICE]
                end
            }
            raise InvalidUIState unless fixed

            # Populate removables and nics, although these aren't required to be
            # selected for the ui state to be valid
            @converter.removables.clear
            @removables.each { |model, path, iter|
                if iter[CONVERT_REMOVABLE_CONVERT] then
                    @converter.removables << iter[CONVERT_REMOVABLE_DEVICE]
                end
            }
            @converter.nics.clear
            @nics.each { |model, path, iter|
                if iter[CONVERT_NETWORK_CONVERT] then
                    @converter.nics << iter[CONVERT_NETWORK_DEVICE]
                end
            }
        rescue InvalidUIState
            valid = false
        end
        valid = true if valid.nil?

        event(EV_VALID, valid)
    end

    def self.valid?
        # Check there's a profile selected
        profile = @profile.active_iter
        return false if profile.nil?

        # Check there's a name set
        name = @name.text
        return false if name.nil?
        return false unless name.strip.length > 0

        # Check cpus and memory are set and numeric
        cpus = @cpus.text
        return false if cpus.nil?
        cpus = Integer(cpus) rescue nil
        return false if cpus.nil?

        memory = @memory.text
        return false if memory.nil?
        memory = Integer(memory) rescue nil
        return false if memory.nil?

        # Check that at least 1 fixed storage device is selected
        fixed = false
        @fixeds.each { |model, path, iter|
            if iter[CONVERT_FIXED_CONVERT] then
                fixed = true
                break
            end
        }
        return false unless fixed

        return true
    end

    def self.convert_cpus_changed
        check_numeric(@cpus)
    end

    def self.convert_memory_changed
        check_numeric(@memory)
    end

    def self.check_numeric(widget)
        value = widget.text
        if value.nil? ? false : begin
            value = Integer(value)
            value > 0
        rescue
            false
        end
        then
            widget.secondary_icon_name = nil
        else
            widget.secondary_icon_name = 'gtk-dialog-warning'
            widget.secondary_icon_tooltip_text =
                'Value must be an integer greater than 0'
        end

        update_values
    end

    def self.convert_fixed_select_toggled(widget, path)
        iter = @fixeds.get_iter(path)
        iter[CONVERT_FIXED_CONVERT] = !iter[CONVERT_FIXED_CONVERT]
    end

    def self.convert_removable_select_toggled(widget, path)
        iter = @removables.get_iter(path)
        iter[CONVERT_REMOVABLE_CONVERT] = !iter[CONVERT_REMOVABLE_CONVERT]
    end

    def self.convert_network_select_toggled(widget, path)
        iter = @nics.get_iter(path)
        iter[CONVERT_NETWORK_CONVERT] = !iter[CONVERT_NETWORK_CONVERT]
    end

    def self.convert_button_clicked
        event(EV_BUTTON, true)
    end

end # module
