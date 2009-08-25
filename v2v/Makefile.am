# libguestfs virt-v2v
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

EXTRA_DIST = \
	run-v2v-locally \
	virt-v2v.pl \
	virt-v2v.conf \
	virt-v2v.conf.pod

man_MANS = virt-v2v.1 virt-v2v.conf.5
CLEANFILES = $(man_MANS)

noinst_DATA =
	@top_builddir@/html/virt-v2v.1.html \
	@top_builddir@/html/virt-v2v.conf.5.html

virt-v2v.1: virt-v2v.pl
	rm -f $@-t $@
	$(POD2MAN) \
	  --section 1 \
	  -c "Virtualization Support" \
	  --release "$(PACKAGE_NAME)-$(PACKAGE_VERSION)" \
	  $< > $@-t && mv $@-t $@

@top_builddir@/html/virt-v2v.1.html: virt-v2v.pl
	mkdir -p @top_builddir@/html
	cd @top_builddir@ && pod2html \
	  --css 'pod.css' \
	  --title 'virt-v2v: convert a guest to use KVM' \
	  --htmldir html \
	  --outfile html/virt-v2v.1.html \
	  v2v/$<

virt-v2v.conf.5: virt-v2v.conf.pod
	rm -f $@-t $@
	$(POD2MAN) \
	  --section 5 \
	  -c "Virtualization Support" \
	  --release "$(PACKAGE_NAME)-$(PACKAGE_VERSION)" \
	  $< > $@-t && mv $@-t $@

@top_builddir@/html/virt-v2v.conf.5.html: virt-v2v.conf.pod
	mkdir -p @top_builddir@/html
	cd @top_builddir@ && pod2html \
	  --css 'pod.css' \
	  --title 'virt-v2v.conf: configuration file for virt-v2v' \
	  --htmldir html \
	  --outfile html/virt-v2v.conf.5.html \
	  v2v/$<

install-data-hook:
	mkdir -p $(DESTDIR)$(bindir)
	install -m 0755 $(srcdir)/virt-v2v.pl $(DESTDIR)$(bindir)/virt-v2v

uninstall-hook:
	rm -f $(DESTDIR)$(bindir)/virt-v2v
	-rmdir $(DESTDIR)$(bindir)
