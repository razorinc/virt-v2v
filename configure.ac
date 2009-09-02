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

# major/minor/release must be numbers
m4_define([virtv2v_major],   [0])
m4_define([virtv2v_minor],   [1])
m4_define([virtv2v_release], [0])
# extra can be any string
m4_define([virtv2v_extra],   [])

AC_INIT([virt-v2v],virtv2v_major.virtv2v_minor.virtv2v_release[]virtv2v_extra)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([foreign])

AC_CONFIG_MACRO_DIR([m4])

AC_PROG_LIBTOOL

dnl Split up the version string.
AC_DEFINE([PACKAGE_VERSION_MAJOR],[virtv2v_major],[Major version number])
AC_DEFINE([PACKAGE_VERSION_MINOR],[virtv2v_minor],[Minor version number])
AC_DEFINE([PACKAGE_VERSION_RELEASE],[virtv2v_release],[Release number])
AC_DEFINE([PACKAGE_VERSION_EXTRA],["virtv2v_extra"],[Extra version string])

dnl Check for basic C environment.
AC_PROG_INSTALL

dnl Check for pod2man and pod2text.
AC_CHECK_PROG([POD2MAN],[pod2man],[pod2man],[no])
test "x$POD2MAN" = "xno" &&
     AC_MSG_ERROR([pod2man must be installed])
AC_CHECK_PROG([POD2TEXT],[pod2text],[pod2text],[no])
test "x$POD2TEXT" = "xno" &&
     AC_MSG_ERROR([pod2text must be installed])

dnl Check for mkisofs.
AC_PATH_PROGS([MKISOFS],[mkisofs],[no],
        [$PATH$PATH_SEPARATOR/usr/sbin$PATH_SEPARATOR/sbin])
test "x$MKISOFS" = "xno" && AC_MSG_ERROR([mkisofs must be installed])

dnl Check for Perl
AC_CHECK_PROG([PERL],[perl],[perl],[no])
test "x$PERL" = "xno" &&
     AC_MSG_ERROR([perl must be installed])

dnl Check for Perl modules needed by V2V
missing_perl_modules=no
for pm in Pod::Usage Getopt::Long Sys::Virt Data::Dumper XML::Writer Locale::TextDomain; do
    AC_MSG_CHECKING([for $pm])
    if ! perl -M$pm -e1 >/dev/null 2>&1; then
        AC_MSG_RESULT([no])
        missing_perl_modules=yes
    else
        AC_MSG_RESULT([yes])
    fi
done
if test "x$missing_perl_modules" = "xyes"; then
    AC_MSG_WARN([some Perl modules required to compile virt-cat, virt-df, virt-inspector and virt-v2v are missing])
fi

AC_CONFIG_FILES([Makefile
                 perl/Makefile
                 perl/Makefile.PL
                 snapshot/Makefile
                 snapshot/virt-snapshot.pl
                 v2v/Makefile
                 v2v/virt-v2v.pl])
AC_OUTPUT
