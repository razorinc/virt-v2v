# virt-v2v
# Copyright (C) 2009 Red Hat Inc.
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

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = Makefile.PL.in

SUBDIRS = po v2v snapshot

CLEANFILES = pod2htmd.tmp pod2htmi.tmp html/$(PACKAGE_NAME).1.html po/stamp-po

INSTALLDIRS = site

all: Makefile-pl
	$(MAKE) -f Makefile-pl

Makefile-pl: Makefile.PL
	perl Makefile.PL INSTALLDIRS=$(INSTALLDIRS) PREFIX=$(prefix)

# No!  Otherwise it is deleted before the clean-local rule runs.
#CLEANFILES = Makefile-pl

clean-local:
	-$(MAKE) -f Makefile-pl realclean
	rm -f Makefile-pl

# Don't do this: it causes "make distcheck" to fail.
#install-data-hook:
#	$(MAKE) -f Makefile-pl DESTDIR=$(DESTDIR) install
