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
require 'ipaddr'
require 'virt-p2v/netdevice'

module VirtP2V::UI::Network
    # The indices of Device List colums, taken from glade
    DEVCOL_NAME      = 0
    DEVCOL_MAC       = 1
    DEVCOL_STATUS    = 2
    DEVCOL_AVAILABLE = 3

    UI_STATE_INVALID    = 0
    UI_STATE_VALID      = 1
    UI_STATE_ACTIVATING = 2
    UI_STATE_COMPLETE   = 3

    EV_IP_CONFIG    = 0
    EV_SELECTION    = 1
    EV_BUTTON       = 2
    EV_ACTIVATION   = 3

    def self.event(event, status)
        case event
        when EV_IP_CONFIG
            @ip_config = status
        when EV_SELECTION
            @selected = status
        when EV_BUTTON, EV_ACTIVATION
            # Persistent state not required
        else
            raise "Unexpected NetworkConfig event: #{event}"
        end

        case @state
        when UI_STATE_INVALID
            if @ip_config && @selected then
                set_state(UI_STATE_VALID)
            end
        when UI_STATE_VALID
            if !@ip_config || !@selected then
                set_state(UI_STATE_INVALID)
            elsif event == EV_BUTTON
                set_state(UI_STATE_ACTIVATING)
            end
        when UI_STATE_ACTIVATING
            # UI is disabled and we're waiting for EV_ACTIVATION, but we could
            # also get events triggered by NetworkManager signals.

            if event == EV_ACTIVATION then
                if status then
                    set_state(UI_STATE_COMPLETE)
                else
                    set_state(UI_STATE_VALID)
                end
            elsif !@ip_config || !@selected then
                set_state(UI_STATE_INVALID)
            end
        else
            raise "Unexpected NetworkConfig UI state: #{@state}"
        end
    end

    def self.init(ui)
        # Configure initial defaults
        @manual_mode = false
        @ip_address = nil
        @ip_prefix = nil
        @ip_gateway = nil
        @ip_dns = nil
        @state = UI_STATE_INVALID
        @ip_config = false
        @selected = false

        @network_button     = ui.get_object('network_button')
        @device_list_frame  = ui.get_object('device_list_frame')
        @ipv4_config_frame  = ui.get_object('ipv4_config_frame')
        @dl_selection       = ui.get_object('network_device_list_view').
                              selection
        @device_list        = ui.get_object('network_device_list')
        @manual_ui          = ui.get_object('ip_manual')
        @ip_address_ui      = ui.get_object('ip_address')
        @ip_prefix_ui       = ui.get_object('ip_prefix')
        @ip_gateway_ui      = ui.get_object('ip_gateway')
        @ip_dns_ui          = ui.get_object('ip_dns')

        ui.register_handler('network_button_clicked',
                            method(:network_button_clicked))
        ui.register_handler('ip_auto_toggled',
                            method(:ip_auto_toggled))
        ui.register_handler('ip_address_changed',
                            method(:ip_address_changed))
        ui.register_handler('ip_prefix_changed',
                            method(:ip_prefix_changed))
        ui.register_handler('ip_gateway_changed',
                            method(:ip_gateway_changed))
        ui.register_handler('ip_dns_changed',
                            method(:ip_dns_changed))

        check_config_valid()

        # The user may only select a single device
        @dl_selection.mode = Gtk::SELECTION_SINGLE

        @dl_selection.set_select_function { |selection, model, path, current|
            iter = model.get_iter(path)

            # This is a toggle event. The new state is the opposite of the
            # current state
            new_state = !current

            # Don't allow the user to select an unavailable device
            if new_state then
                # Notify the config UI if we're selecting a device
                if iter[DEVCOL_AVAILABLE] then
                    event(EV_SELECTION, true)
                end

                iter[DEVCOL_AVAILABLE]

            # Always allow the user to unselect a device
            else
                # Notify the UI that we're unselecting the device
                event(EV_SELECTION, false)
                true
            end
        }

        # Store a map of device names to row references
        refs = {}

        # Populate the device list with all detected network devices
        VirtP2V::NetworkDevice.all_devices.each { |device|
            iter = @device_list.append()

            iter[DEVCOL_NAME]       = device.name
            iter[DEVCOL_MAC]        = device.mac
            iter[DEVCOL_STATUS]     = device.state
            iter[DEVCOL_AVAILABLE]  = device.connected

            # Store a stable reference to this row in the TreeModel
            refs[device.name] =
                Gtk::TreeRowReference.new(@device_list, iter.path)
        }

        # Listen for updates to device states
        VirtP2V::NetworkDevice.add_listener( lambda { |device|
            path = refs[device.name].path

            iter = @device_list.get_iter(path)
            iter[DEVCOL_STATUS]     = device.state
            iter[DEVCOL_AVAILABLE]  = device.connected

            # Notify the UI that a device was activated
            event(EV_ACTIVATION, device.activated) \
                unless device.activated.nil?

            # Unselect the path if it was previously selected and is no
            # longer available
            if !device.connected && @dl_selection.iter_is_selected?(iter)
            then
                @dl_selection.unselect_all()
                event(EV_SELECTION, false)
            end
        } )

        @ui = ui
    end

    def self.set_state(state)
        # Don't do anything if state hasn't changed
        return if state == @state

        case state
        when UI_STATE_INVALID
            @network_button.sensitive = false
            @device_list_frame.sensitive = true
            @ipv4_config_frame.sensitive = true

            @state = UI_STATE_INVALID
        when UI_STATE_VALID
            @network_button.sensitive = true
            @device_list_frame.sensitive = true
            @ipv4_config_frame.sensitive = true

            @state = UI_STATE_VALID
        when UI_STATE_ACTIVATING
            @network_button.sensitive = false
            @device_list_frame.sensitive = false
            @ipv4_config_frame.sensitive = false

            @state = UI_STATE_ACTIVATING
        when UI_STATE_COMPLETE
            # Activate the next page
            @ui.active_page = 'server_win'

            # ... then leave this one as we hope to find it if we come back here
            set_state(UI_STATE_VALID)
        else
            raise "Attempt to set unexected NetworkConfig UI state: #{@state}"
        end
    end

    def self.network_button_clicked
        event(EV_BUTTON, true)

        iter = @dl_selection.selected
        return if iter.nil? # Shouldn't be possible
        name = iter[DEVCOL_NAME]

        VirtP2V::NetworkDevice[name].activate(!@manual_mode, @ip_address,
                                              @ip_prefix, @ip_gateway, @ip_dns)
    end

    def self.ip_auto_toggled
        @manual_mode = !@manual_mode
        @manual_ui.sensitive = @manual_mode

        check_config_valid()
    end

    def self.ip_address_changed
        @ip_address = parse_ip(@ip_address_ui)

        check_config_valid()
    end

    # Check IP prefix is a positive integer
    # We check that it's appropriate to the address class in use elsewhere
    def self.ip_prefix_changed
        begin
            @ip_prefix = Integer(@ip_prefix_ui.text)
        rescue ArgumentError => e
            # Ignore the result if it didn't parse
            @ip_prefix = nil
            return
        end

        if @ip_prefix < 0 then
            @ip_prefix = nil
        end

        check_config_valid()
    end

    def self.ip_gateway_changed
        @ip_gateway = parse_ip(@ip_gateway_ui)

        check_config_valid()
    end

    # Parse an IP address understood by IPAddr
    def self.parse_ip(entry)
        a = entry.text.strip

        begin
            ip = IPAddr.new(a)
        rescue ArgumentError => e
            # Ignore the result if it didn't parse
            ip = nil
        end

        return ip
    end

    def self.ip_dns_changed
        dns = @ip_dns_ui.text

        @ip_dns = []
        dns.split(/\s*,+\s*/).each { |entry|
            begin
                @ip_dns << IPAddr.new(entry)
            rescue ArgumentError => e
                @ip_dns = ()
                break
            end
        }
    end

    def self.check_config_valid
        if !@manual_mode || (!@ip_address.nil? &&
                             !@ip_prefix.nil? &&
                             !@ip_gateway.nil?) then
            if @manual_mode then
                # Check that IPv4/IPv6 is used consistently
                if @ip_address.ipv4? then
                    event(EV_IP_CONFIG, @ip_gateway.ipv4? && @ip_prefix < 32)
                else
                    event(EV_IP_CONFIG, @ip_gateway.ipv6? && @ip_prefix < 128)
                end
            else
                event(EV_IP_CONFIG, true)
            end
        else
            event(EV_IP_CONFIG, false)
        end
    end

end # module
