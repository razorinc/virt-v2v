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
require 'gtk2'

require 'virt-p2v/connection'

module VirtP2V::UI::Connect
    include GetText

    UI_STATE_INVALID    = 0
    UI_STATE_VALID      = 1
    UI_STATE_ACTIVATING = 2
    UI_STATE_COMPLETE   = 3

    EV_HOSTNAME     = 0
    EV_USERNAME     = 1
    EV_PASSWORD     = 2
    EV_BUTTON       = 3
    EV_ACTIVATION   = 4

    def self.event(event, status)
        case event
        when EV_HOSTNAME
            @hostname = status
        when EV_USERNAME
            @username = status
        when EV_PASSWORD
            @password = status
        when EV_BUTTON, EV_ACTIVATION
            # Persistent state not required
        else
            raise "Unexpected event: #{event}"
        end

        valid = @hostname && @username && @password

        case @state
        when UI_STATE_INVALID
            set_state(UI_STATE_VALID) if valid
        when UI_STATE_VALID
            if !valid then
                set_state(UI_STATE_INVALID)
            elsif event == EV_BUTTON
                set_state(UI_STATE_ACTIVATING)
            end
        when UI_STATE_ACTIVATING
            # UI is disabled, so we shouldn't be getting any events other than
            # EV_ACTIVATION
            raise "Unexpected event: #{event}" unless event == EV_ACTIVATION

            if status then
                set_state(UI_STATE_COMPLETE)
            else
                set_state(UI_STATE_VALID)
            end
        else
            raise "Unexpected UI state: #{@state}"
        end
    end

    def self.init(ui, converter)
        @hostname_ui    = ui.get_object('server_hostname')
        @username_ui    = ui.get_object('server_username')
        @password_ui    = ui.get_object('server_password')
        @connect_frame  = ui.get_object('connect_frame')
        @connect_button = ui.get_object('connect_button')
        @connect_error  = ui.get_object('connect_error')

        ui.register_handler('server_hostname_changed',
                            method(:server_hostname_changed))
        ui.register_handler('server_username_changed',
                            method(:server_username_changed))
        ui.register_handler('server_password_changed',
                            method(:server_password_changed))
        ui.register_handler('connect_button_clicked',
                            method(:connect_button_clicked))

        @hostname = @hostname_ui.text.strip.length > 0
        @username = @username_ui.text.strip.length > 0
        @password = @password_ui.text.length > 0 # Allow spaces in passwords
        @state = UI_STATE_INVALID

        @ui = ui
        @converter = converter
    end

    def self.set_state(state)
        # Don't do anything if state hasn't changed
        return if state == @state

        case state
        when UI_STATE_INVALID
            @connect_frame.sensitive = true
            @connect_button.sensitive = false

            @state = UI_STATE_INVALID
        when UI_STATE_VALID
            @connect_frame.sensitive = true
            @connect_button.sensitive = true

            @state = UI_STATE_VALID
        when UI_STATE_ACTIVATING
            @connect_frame.sensitive = false
            @connect_button.sensitive = false
            @connect_error.text = ''

            @state = UI_STATE_ACTIVATING
        when UI_STATE_COMPLETE
            # Activate the next page
            @ui.active_page = 'conversion_win'

            # ... then leave this one as we hope to find it if we come back here
            set_state(UI_STATE_VALID)
        else
            raise "Attempt to set unexpected UI state: #{@state}"
        end
    end

    def self.server_hostname_changed
        event(EV_HOSTNAME, @hostname_ui.text.strip.length > 0)
    end

    def self.server_username_changed
        event(EV_USERNAME, @username_ui.text.strip.length > 0)
    end

    def self.server_password_changed
        event(EV_PASSWORD, @password_ui.text.length > 0)
    end

    def self.connect_button_clicked
        event(EV_BUTTON, true)

        hostname = @hostname_ui.text.strip
        username = @username_ui.text.strip
        password = @password_ui.text
        connection = VirtP2V::Connection.new(hostname, username, password) \
        { |result|
            case result
            when true
                @converter.connection = connection
                connection.connect { |result|
                    case result
                    when true
                        event(EV_ACTIVATION, true)
                    when VirtP2V::Connection::RemoteError
                        @connect_error.text =
                            _'Failed to start virt-p2v-server on remote server'
                        event(EV_ACTIVATION, false)
                    else
                        @connect_error.text = result.message
                        event(EV_ACTIVATION, false)
                    end
                }
            when VirtP2V::Connection::InvalidHostnameError
                @connect_error.text = _"Unable to connect to #{hostname}"
                event(EV_ACTIVATION, false)
            when VirtP2V::Connection::InvalidCredentialsError
                @connect_error.text = _"Invalid username/password"
                event(EV_ACTIVATION, false)
            else
                raise result
            end
        }
    end

end # module
