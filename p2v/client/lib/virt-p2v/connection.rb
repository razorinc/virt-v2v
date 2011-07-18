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

require 'rubygems'
require 'thread'
require 'yaml'

require 'virt-p2v/gtk-queue'
require 'rblibssh2'

module VirtP2V

class Connection
    class InvalidHostnameError < StandardError; end
    class InvalidCredentialsError < StandardError; end
    class RemoteError < StandardError; end
    class ProtocolError < StandardError; end
    class NotConnectedError < StandardError; end

    def on_connect(&cb)
        @connection_listeners << cb
    end

    def initialize(hostname, username, password)
        @mutex = Mutex.new
        @connection_listeners = []

        @hostname = hostname
        @username = username
        @password = password

        @buffer = ""

        @session = nil
        @channel = nil
    end

    def connect(&cb)
        run(cb) {
            begin
                @session = Libssh2.connect(@hostname, @username, @password) \
                    if @session.nil?

                @channel = @session.exec('LANG=C virt-p2v-server') \
                    if @channel.nil?
            #rescue Libssh2::Session::InvalidHostnameError
            #    raise InvalidHostnameError
            #rescue Libssh2::Session::InvalidCredentialsError
            #    raise InvalidCredentialsError
            #rescue => ex
            #    raise RemoteError.new(ex.message)
            end

            begin
                line = @channel.read(64)

                if line !~ /^VIRT_P2V_SERVER /
                    raise RemoteError.new("Unexpected response: #{line}")
                end
            rescue Libssh2::Channel::ApplicationError => ex
                if ex.message =~ /command not found/
                    raise RemoteError.new(
                        "virt-p2v-server is not installed on #{@hostname}")
                else
                    raise RemoteError.new(ex.message)
                end
            end

            begin
                i = 0;
                listener_result = lambda { |result|
                    if result.kind_of?(Exception)
                        cb.call(result)
                    else
                        i += 1
                        if i == @connection_listeners.length
                            cb.call(true)
                        else
                            Gtk.queue {
                                @connection_listeners[i].call(listener_result)
                            }
                        end
                    end
                }
                Gtk.queue { @connection_listeners[0].call(listener_result) }
            rescue => ex
                @channel.close unless @channel.nil?
                raise ex
            end
        }
    end

    def connected?
        return !@channel.nil?
    end

    def close
        unless @channel.nil?
            @channel.close
            @channel = nil
        end
        unless @session.nil?
            @session.close
            @session = nil
        end

        @buffer = ''
    end

    def lang(lang, &cb)
        raise NotConnectedError if @channel.nil?

        run(cb) {
            @channel.write("LANG #{lang}\n")
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def metadata(meta, &cb)
        raise NotConnectedError if @channel.nil?

        run(cb) {
            payload = YAML::dump(meta)
            @channel.write("METADATA #{payload.length}\n");
            @channel.write(payload)
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def path(length, path, &cb)
        raise NotConnectedError if @channel.nil?

        run(cb) {
            @channel.write("PATH #{length} #{path}\n")
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def convert(&cb)
        raise NotConnectedError if @channel.nil?

        run(cb) {
            @channel.write("CONVERT\n")
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def list_profiles(&cb)
        raise NotConnectedError if @channel.nil?

        run(cb) {
            @channel.write("LIST_PROFILES\n")
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def set_profile(profile, &cb)
        raise NotConnectedError if @channel.nil?

        run(cb) {
            @channel.write("SET_PROFILE #{profile}\n")
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def container(type, &cb)
        raise NotConnectedError if @channel.nil?

        run(cb) {
            @channel.write("CONTAINER #{type}\n")
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def send_data(io, length, progress, &completion)
        raise NotConnectedError if @channel.nil?

        run(completion) {
            @channel.write("DATA #{length}\n")
            @channel.send_data(io) { |total|
                Gtk.queue { progress.call(total) }
            }

            result = parse_return

            Gtk.queue { progress.call(length); completion.call(result) }
        }
    end

    private

    def run(cb)
        # Run the given block in a new thread
        t = Thread.new {
            begin
                # We can't run more than 1 command simultaneously
                @mutex.synchronize { yield }
            rescue => ex
                # Deliver exceptions to the caller, then re-raise them
                Gtk.queue { cb.call(ex) }
                raise ex
            end
        }
    end

    # Return a single line of output from the remote server
    def readline
        index = nil
        loop {
            index = @buffer.index("\n")
            break unless index.nil?

            begin
                @buffer << @channel.read(64)
            rescue IOError => ex
                @channel.close
                @channel = nil
                raise RemoteError,
                    "Server closed connection unexpectedly: #{ex.message}"
            rescue Libssh2::Channel::ApplicationError => ex
                @channel.close
                @channel = nil
                raise RemoteError,
                    "virt-p2v-server returned an error: #{ex.message}"
            end
        }

        # Remove the line from the buffer and return it with the trailing
        # newline removed
        @buffer.slice!(0..index).chomp!
    end

    def parse_return
        line = readline
        line =~ /^(OK|ERROR|LIST)(?:\s(.*))?$/ or
            raise ProtocolError, "Invalid server response: #{line}"

        return true if $~[1] == 'OK'
        if $~[1] == 'ERROR' then
            close
            raise RemoteError, $~[2]
        end

        # LIST response. Get the number of items, and read that many lines
        n = Integer($~[2])
        ret = []
        while n > 0 do
            n -= 1
            ret.push(readline)
        end

        ret
    end
end

end
