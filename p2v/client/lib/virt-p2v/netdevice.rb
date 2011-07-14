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

require 'dbus'

module VirtP2V

class NetworkDevice
    attr_reader :name, :mac, :connected, :activated, :state

    # Some NetworkManager names, for convenience
    CONNECTION      = 'org.freedesktop.NetworkManagerSettings.Connection'.freeze
    DEVICE          = 'org.freedesktop.NetworkManager.Device'.freeze
    NETWORKMANAGER  = 'org.freedesktop.NetworkManager'.freeze
    PROPERTIES      = 'org.freedesktop.DBus.Properties'.freeze
    SETTINGS        = 'org.freedesktop.NetworkManagerSettings'.freeze
    WIRED           = 'org.freedesktop.NetworkManager.Device.Wired'.freeze

    # NetworkManager device types
    # http://projects.gnome.org/NetworkManager/developers/spec-08.html
    TYPE_UNKNOWN  = 0
    TYPE_ETHERNET = 1
    TYPE_WIFI     = 2
    TYPE_GSM      = 3
    TYPE_CDMA     = 4

    # NetworkManager device states
    STATE_UNKNOWN         = 0
    STATE_UNMANAGED       = 1
    STATE_UNAVAILABLE     = 2
    STATE_DISCONNECTED    = 3
    STATE_PREPARE         = 4
    STATE_CONFIG          = 5
    STATE_NEED_AUTH       = 6
    STATE_IP_CONFIG       = 7
    STATE_ACTIVATED       = 8
    STATE_FAILED          = 9

    # Human readable descriptions of NetworkManager Device States
    STATES = {
        0 => 'Unknown'.freeze,           # For completeness
        1 => 'Unmanaged'.freeze,         # For completeness
        2 => 'No cable connected'.freeze,
        3 => 'Not connected'.freeze,
        4 => 'Preparing to connect'.freeze,
        5 => 'Configuring'.freeze,
        6 => 'Waiting for authentication'.freeze,
        7 => 'Obtaining an IP address'.freeze,
        8 => 'Connected'.freeze,
        9 => 'Connection failed'.freeze
    }.freeze

    def initialize(obj, device, props)
        device.default_iface = WIRED

        @nm_obj = obj
        @name   = props.Get(DEVICE, 'Interface')[0]
        @mac    = props.Get(WIRED, 'HwAddress')[0]
        state   = props.Get(WIRED, 'State')[0]

        # Lookup by name
        @@devices[@name] = self

        state_updated(state)

        # Register a listener for state changes
        device.on_signal('PropertiesChanged') { |props|
            if props.has_key?('State') then
                state_updated(props['State'])

                # Notify registered state change handlers
                @@listeners.each { |cb| cb.call(self) }
            end
        }
    end

    def self.all_devices()
        @@devices.values
    end

    def self.add_listener(cb)
        @@listeners.push(cb)
    end

    def self.[](name)
        @@devices[name]
    end

    def activate(auto, ip, prefix, gateway, dns)
        # Get an IP config dependent on whether @ip_address is IPv4 or IPv6
        ip_config = auto ? get_config_auto :
                           ip.ipv4? ? get_config_ipv4() : get_config_ipv6()

        # Create a new NetworkManager connection object
        settings = @@nm_service.object(
            '/org/freedesktop/NetworkManagerSettings')
        settings.introspect()
        settings.default_iface = SETTINGS

        uuid = `uuidgen`.chomp
        settings.AddConnection(
            'connection' => {
                'uuid' => uuid,
                'id' => 'P2V',
                'type' => '802-3-ethernet',
                'autoconnect' => false
            },
            '802-3-ethernet' => {},
            'ipv4' => ip_config['ipv4'],
            'ipv6' => ip_config['ipv6']
        )

        # Find the connection we just created
        # XXX: There must be a better way to get this!
        conn = settings.ListConnections()[0].each { |i|
            conn = @@nm_service.object(i)
            conn.introspect
            conn.default_iface = CONNECTION

            break i if conn.GetSettings()[0]['connection']['uuid'] == uuid
        }

        nm = @@nm_service.object('/org/freedesktop/NetworkManager')
        nm.introspect
        nm.default_iface = NETWORKMANAGER
        nm.ActivateConnection('org.freedesktop.NetworkManagerSystemSettings',
                             conn, @nm_obj, '/')
    end

    private

    def state_updated(state)
        @connected = state > 2
        @state  = STATES[state]

        if state == STATE_ACTIVATED then
            @activated = true
        elsif state == STATE_FAILED then
            @activated = false
        else
            @activated = nil
        end
    end

    def get_config_auto
        {
            'ipv4' => {
                'method' => 'auto'
            },
            'ipv6' => {
                'method' => 'ignore'
            }
        }
    end

    def ipv4_to_nm(ipaddr)
        ipaddr.hton().unpack("I")[0]
    end

    def get_config_ipv4
        addresses = [[ ipv4_to_nm(@ip_address), @ip_prefix,
                       ipv4_to_nm(@ip_gateway) ]]

        dns = []
        @ip_dns.each{ |ipaddr|
            # Only use IPv4 DNS servers
            next unless ipaddr.ipv4?
            dns.push(ipv4_to_nm(ipaddr))
        }

        {
            'ipv4' => {
                'method' => 'manual',
                'addresses' => [ 'aau', addresses ],
                'dns' => [ 'au', dns ]
            },
            'ipv6' => {
                'method' => 'ignore'
            }
        }
    end

    def ipv6_to_nm(ipaddr)
        ipaddr.hton().unpack("c*")
    end

    def get_config_ipv6
        dns = []
        @ip_dns.each { |ipaddr|
            # Only use IPv6 DNS servers
            next unless ipaddr.ipv6?
            dns.push(ipv6_to_nm(ipaddr))
        }

        {
            'ipv4' => {
                'method' => 'disabled'
            },
            'ipv6' => {
                'method' => 'manual',
                'addresses' => [ 'a(ayu)', [[
                    ipv6_to_nm(@ip_address),
                    @ip_prefix
                ]] ],
                'routes' => [ 'a(ayuayu)', [[
                    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 0,
                    ipv6_to_nm(@ip_gateway), 1024
                ]] ],
                'dns' => [ 'aay', dns ]
            }
        }
    end

    # Class initialization
    begin
        dbus = DBus::SystemBus.instance()
        dbus.glibize()
        @@nm_service = dbus.service(NETWORKMANAGER)

        nm = @@nm_service.object('/org/freedesktop/NetworkManager')
        nm.introspect
        nm.default_iface = NETWORKMANAGER

        @@devices = {}
        nm.GetDevices()[0].each { |obj|
            device = @@nm_service.object(obj)
            device.introspect

            props = device[PROPERTIES]
            type = props.Get(DEVICE, 'DeviceType')[0]

            # We only support ethernet devices
            next unless type == TYPE_ETHERNET

            # Constructor will add it to @@devices
            self.new(obj, device, props)
        }

        @@listeners = []
    end
end

end #module
