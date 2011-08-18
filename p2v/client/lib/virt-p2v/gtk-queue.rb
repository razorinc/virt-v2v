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

# This code is taken from:
#   http://ruby-gnome2.sourceforge.jp/hiki.cgi?tips_threads
# The author of the above page is given as Tal Liron
# The above page is distributed under the terms of the GNU FDL, although I
# consider this code to be too trivial to be copyrightable

require 'gtk2'
require 'thread'

module Gtk
    GTK_PENDING_BLOCKS = []
    GTK_PENDING_BLOCKS_LOCK = Mutex.new

    def Gtk.queue &block
        if Thread.current == Thread.main
            block.call
        else
            GTK_PENDING_BLOCKS_LOCK.synchronize do
                GTK_PENDING_BLOCKS << block
            end
        end
    end

    def Gtk.main_with_queue timeout
        Gtk.timeout_add timeout do
            GTK_PENDING_BLOCKS_LOCK.synchronize do
                for block in GTK_PENDING_BLOCKS
                    block.call
                end
                GTK_PENDING_BLOCKS.clear
            end
            true
        end
        Gtk.main
    end
end
