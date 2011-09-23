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

require 'dbus'

module VirtP2V

class NetworkDevice
    attr_reader :name, :mac, :connected, :activated, :state

    # Some NetworkManager names, for convenience
    DEVICE          = 'org.freedesktop.NetworkManager.Device'.freeze
    NETWORKMANAGER  = 'org.freedesktop.NetworkManager'.freeze
    PROPERTIES      = 'org.freedesktop.DBus.Properties'.freeze
    WIRED           = 'org.freedesktop.NetworkManager.Device.Wired'.freeze

    # NetworkManager device types
    # http://projects.gnome.org/NetworkManager/developers/spec-08.html
    TYPE_UNKNOWN  = 0
    TYPE_ETHERNET = 1
    TYPE_WIFI     = 2
    TYPE_GSM      = 3
    TYPE_CDMA     = 4

    def initialize(obj, device, props)
        device.default_iface = WIRED

        @nm_obj = obj
        @name   = props.Get(DEVICE, 'Interface')[0]
        @mac    = props.Get(WIRED, 'HwAddress')[0]
        state   = props.Get(DEVICE, 'State')[0]

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
        # Get an IP config dependent on whether ip is IPv4 or IPv6
        ip_config = auto ? get_config_auto :
                         ip.ipv4? ? get_config_ipv4(ip, prefix, gateway, dns) :
                                    get_config_ipv6(ip, prefix, gateway, dns)

        # Create a new NetworkManager connection object
        settings = @@nm_service.object(SETTINGS_PATH)
        settings.introspect()
        settings.default_iface = SETTINGS

        uuid = `uuidgen`.chomp
        conn = settings.AddConnection(
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

        nm = @@nm_service.object('/org/freedesktop/NetworkManager')
        nm.introspect
        nm.default_iface = NETWORKMANAGER

        if NM_API_09
            nm.ActivateConnection(conn[0], @nm_obj, '/')
        else
            # Find the connection we just created
            # NM before version 0.9 didn't provide a sensible way to get the
            # path of the connection object we just created
            conn = settings.ListConnections()[0].each { |i|
                conn = @@nm_service.object(i)
                conn.introspect
                conn.default_iface = CONNECTION
                break i if conn.GetSettings()[0]['connection']['uuid'] == uuid
            }

            # XXX: mbooth@redhat.com - 22/7/2011 The first time this code runs
            # on a RHEL 6 system (NetworkManager-0.8.1-9.el6_1.1.i686), conn
            # will be an array containing a single element: the connection. This
            # will cause ActivateConnection below to return an error, and the
            # p2v client to crash. If you run p2v client a second time, conn
            # will be a simple value, not a single element array, and
            # ActivateConnection works fine.  I assume this is a bug in
            # NetworkManager. I don't see this behaviour in F14.
            conn = conn[0] if conn.kind_of?(Array)

            nm.ActivateConnection(
                'org.freedesktop.NetworkManagerSystemSettings',
                conn, @nm_obj, '/'
            )
        end
    end

    private

    def state_updated(state)
        @connected = state > STATE_UNAVAILABLE
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

    def get_config_ipv4(ip, prefix, gateway, dns)
        addresses = [[ ipv4_to_nm(ip), prefix, ipv4_to_nm(gateway) ]]

        dns_nm = []
        dns.each{ |ipaddr|
            # Only use IPv4 DNS servers
            next unless ipaddr.ipv4?
            dns_nm.push(ipv4_to_nm(ipaddr))
        }

        {
            'ipv4' => {
                'method' => 'manual',
                'addresses' => [ 'aau', addresses ],
                'dns' => [ 'au', dns_nm ]
            },
            'ipv6' => {
                'method' => 'ignore'
            }
        }
    end

    def ipv6_to_nm(ipaddr)
        ipaddr.hton().unpack("c*")
    end

    def get_config_ipv6(ip, prefix, gateway, dns)
        dns_nm = []
        dns.each { |ipaddr|
            # Only use IPv6 DNS servers
            next unless ipaddr.ipv6?
            dns_nm.push(ipv6_to_nm(ipaddr))
        }

        {
            'ipv4' => {
                'method' => 'disabled'
            },
            'ipv6' => {
                'method' => 'manual',
                'addresses' => [ 'a(ayu)', [[
                    ipv6_to_nm(ip), prefix
                ]] ],
                'routes' => [ 'a(ayuayu)', [[
                    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 0,
                    ipv6_to_nm(gateway), 1024
                ]] ],
                'dns' => [ 'aay', dns_nm ]
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

        # API differences between versions 0.8 and 0.9
        version = nil
        begin
            version = nm[PROPERTIES].Get(NETWORKMANAGER, 'Version')[0]
            version = version.split(/\./).map{ |n| Integer(n) }
        rescue DBus::Error
            # Versions prior to 0.8 didn't have Version
            version = [ 0, 7, 0 ]
        end

        self.const_set('STATE_UNKNOWN', 0)
        if (version <=> [ 0, 8, 9]) > 0
            self.const_set('STATE_UNMANAGED', 10)
            self.const_set('STATE_UNAVAILABLE', 20)
            self.const_set('STATE_DISCONNECTED', 30)
            self.const_set('STATE_PREPARE', 40)
            self.const_set('STATE_CONFIG', 50)
            self.const_set('STATE_NEED_AUTH', 60)
            self.const_set('STATE_IP_CONFIG', 70)
            self.const_set('STATE_IP_CHECK', 80)
            self.const_set('STATE_IP_SECONDARIES', 90)
            self.const_set('STATE_ACTIVATED', 100)
            self.const_set('STATE_DEACTIVATING', 110)
            self.const_set('STATE_FAILED', 120)

            self.const_set('CONNECTION',
                'org.freedesktop.NetworkManager.Settings.Connection'.freeze)
            self.const_set('SETTINGS',
                'org.freedesktop.NetworkManager.Settings'.freeze)
            self.const_set('SETTINGS_PATH',
                '/org/freedesktop/NetworkManager/Settings'.freeze)

            self.const_set('NM_API_09', true)
        else
            self.const_set('STATE_UNMANAGED', 1)
            self.const_set('STATE_UNAVAILABLE', 2)
            self.const_set('STATE_DISCONNECTED', 3)
            self.const_set('STATE_PREPARE', 4)
            self.const_set('STATE_CONFIG', 5)
            self.const_set('STATE_NEED_AUTH', 6)
            self.const_set('STATE_IP_CONFIG', 7)
            self.const_set('STATE_IP_CHECK', nil)
            self.const_set('STATE_IP_SECONDARIES', nil)
            self.const_set('STATE_ACTIVATED', 8)
            self.const_set('STATE_DEACTIVATING', nil)
            self.const_set('STATE_FAILED', 9)

            self.const_set('CONNECTION',
                'org.freedesktop.NetworkManagerSettings.Connection'.freeze)
            self.const_set('SETTINGS',
                'org.freedesktop.NetworkManagerSettings'.freeze)
            self.const_set('SETTINGS_PATH',
                '/org/freedesktop/NetworkManagerSettings'.freeze)

            self.const_set('NM_API_09', false)
        end

        # Human readable descriptions of NetworkManager Device States
        self.const_set('STATES',
            {
                STATE_UNKNOWN           => 'Unknown'.freeze,
                STATE_UNMANAGED         => 'Unmanaged'.freeze,
                STATE_UNAVAILABLE       => 'No cable connected'.freeze,
                STATE_DISCONNECTED      => 'Not connected'.freeze,
                STATE_PREPARE           => 'Preparing to connect'.freeze,
                STATE_CONFIG            => 'Configuring'.freeze,
                STATE_NEED_AUTH         => 'Waiting for authentication'.freeze,
                STATE_IP_CONFIG         => 'Obtaining an IP address'.freeze,
                STATE_IP_CHECK          => 'Testing'.freeze,
                STATE_IP_SECONDARIES    => 'Waiting for secondary connection'.freeze,
                STATE_ACTIVATED         => 'Connected'.freeze,
                STATE_DEACTIVATING      => 'Deactivating'.freeze,
                STATE_FAILED            => 'Connection failed'.freeze
            }.freeze
        )

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
