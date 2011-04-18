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
require 'rubygems'
require 'net/ssh'
require 'thread'
require 'yaml'

require 'virt-p2v/gtk-queue'

module VirtP2V

class Connection
    include GetText

    attr_reader :connected

    class InvalidHostnameError < StandardError; end
    class InvalidCredentialsError < StandardError; end
    class TransportError < StandardError; end
    class NoP2VError < StandardError; end
    class RemoteError < StandardError; end
    class ProtocolError < StandardError; end
    class NotConnectedError < StandardError; end

    def on_connect(&cb)
        @connection_listeners << cb
    end

    def initialize(hostname, username, password, &cb)
        @mutex = Mutex.new
        @connection_listeners = []
        
        # Always send our version number on connection
        @connection_listeners << Proc.new { |cb|
            self.version { |result| cb.call(result) }
        }

        run(cb) {
            error = nil
            begin
                @ssh = Net::SSH.start(hostname, username, :password => password)
            rescue SocketError, Errno::EHOSTUNREACH => ex
                raise InvalidHostnameError
                raise ex
            rescue Net::SSH::AuthenticationFailed => ex
                raise InvalidCredentialsError
                raise ex
            end

            @buffer = ""
            @connected = false

            Gtk.queue { cb.call(true) }
        }
    end

    def connect(&cb)
        run(cb) {
            @ch = @ssh.open_channel do |ch|
                ch.exec("virt-p2v-server") do |ch, success|
                    raise RemoteError,
                          "could not execute a remote command" unless success

                    ch.on_data do |ch, data|
                        @buffer << data
                    end

                    # If we get anything on stderr, raise it as a RemoteError
                    ch.on_extended_data do |ch, type, data|
                        close
                        raise RemoteError, data
                    end

                    # Clean up local resources if we get eof from the other end
                    ch.on_eof do |ch|
                        close
                    end

                    @connected = true
                end

            end

            # Wait until we're connected
            @ssh.loop do
                !@connected
            end

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
        }
    end

    def close
        @connected = false
        @buffer = ""
        @ch.close
    end

    def version(&cb)
        raise NotConnectedError unless @connected

        run(cb) {
            @ch.send_data("VERSION 0\n")
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def lang(lang, &cb)
        raise NotConnectedError unless @connected

        run(cb) {
            @ch.send_data("LANG #{lang}\n")
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def metadata(meta, &cb)
        raise NotConnectedError unless @connected

        run(cb) {
            payload = YAML::dump(meta)
            @ch.send_data("METADATA #{payload.length}\n");
            @ch.send_data(payload)
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def path(length, path, &cb)
        raise NotConnectedError unless @connected

        run(cb) {
            @ch.send_data("PATH #{length} #{path}\n")
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def convert(&cb)
        raise NotConnectedError unless @connected

        run(cb) {
            @ch.send_data("CONVERT\n")
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def list_profiles(&cb)
        raise NotConnectedError unless @connected

        run(cb) {
            @ch.send_data("LIST_PROFILES\n")
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def set_profile(profile, &cb)
        raise NotConnectedError unless @connected

        run(cb) {
            @ch.send_data("SET_PROFILE #{profile}\n")
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def container(type, &cb)
        raise NotConnectedError unless @connected

        run(cb) {
            @ch.send_data("CONTAINER #{type}\n")
            result = parse_return

            Gtk.queue { cb.call(result) }
        }
    end

    def send_data(io, length, progress, &completion)
        raise NotConnectedError unless @connected

        run(completion) {
            @ch.send_data("DATA #{length}\n")
            total = 0
            buffer = ''
            begin
                # This loop is in the habit of hanging in Net::SSH when sending
                # a chunk larger than about 2M. Putting the 1 second wait
                # timeout here kickstarts it if it stops.
                @ssh.loop(1) {
                    if io.eof? || total == length then
                        false
                    else
                        if @ch.remote_window_size > 0 then
                            out = length - total
                            out = @ch.remote_window_size \
                                if out > @ch.remote_window_size

                            io.read(out, buffer)
                            @ch.send_data(buffer)

                            total += buffer.length

                            # Send a progress callback
                            Gtk.queue { progress.call(total) }
                        end

                        true
                    end
                }
            rescue => ex
                Gtk.queue { completion.call(ex) }
            end

            result = parse_return

            Gtk.queue { completion.call(result) }
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
        t.priority = 1
    end

    # Return a single line of output from the remote server
    def readline
        # Run the event loop until the buffer contains a newline
        index = nil
        @ssh.loop do
            if !@ch.eof? then
                index = @buffer.index("\n")
                index.nil?
            else
                close
                raise RemoteError, _('Server closed connection unexpectedly')
            end
        end

        # Remove the line from the buffer and return it with the trailing
        # newline removed
        @buffer.slice!(0..index).chomp
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
