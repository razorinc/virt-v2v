#!/bin/sh
#
# This script is used to Test::AutoBuild (http://www.autobuild.org)
# to perform automated builds of the virt-v2v module
#
# Copyright (C) 2009 Red Hat, Inc.
# Copyright (C) 2009 Daniel P. Berrange
#
# This program is free software; You can redistribute it and/or modify
# it under the GNU General Public License as published by the Free
# Software Foundation; either version 2, or (at your option) any
# later version
#
# The file "LICENSE" distributed along with this file provides full
# details of the terms and conditions
#

NAME=virt-v2v

set -e

rm -rf blib _build Build

perl Build.PL install_base=$AUTOBUILD_INSTALL_ROOT

./Build
./Build changelog

if [ -z "$USE_COVER" ]; then
  perl -MDevel::Cover -e '' 1>/dev/null 2>&1 && USE_COVER=1 || USE_COVER=0
fi

if [ -z "$SKIP_TESTS" -o "$SKIP_TESTS" = "0" ]; then
  if [ "$USE_COVER" = "1" ]; then
    cover -delete
    HARNESS_PERL_SWITCHES=-MDevel::Cover ./Build test
    cover
    mkdir blib/coverage
    cp -a cover_db/*.html cover_db/*.css blib/coverage
    mv blib/coverage/coverage.html blib/coverage/index.html
  else
    ./Build test
  fi
fi

./Build install \
  --install_path locale=$AUTOBUILD_INSTALL_ROOT/share/locale \
  --install_path confdoc=$AUTOBUILD_INSTALL_ROOT/share/man/man5

rm -f $NAME-*.tar.gz
./Build dist

if [ -z "$ARCH" ]; then
    ARCH=`uname -p`
fi

if [ -f /usr/bin/rpmbuild ]; then
  if [ -n "$AUTOBUILD_COUNTER" ]; then
    EXTRA_RELEASE=".auto$AUTOBUILD_COUNTER"
  else
    NOW=`date +"%s"`
    EXTRA_RELEASE=".$USER$NOW"
  fi

  rpmbuild --nodeps -ba \
           --define "_sourcedir `pwd`" \
           --define "extra_release $EXTRA_RELEASE" \
           --clean virt-v2v.spec

  # virt-p2v only can be built on i686 platform now
  if [ "$ARCH" = "i686" ]; then
      rpmbuild --nodeps -ba --target $ARCH \
               --define "_sourcedir `pwd`" \
               --define "extra_release $EXTRA_RELEASE" \
               --clean rubygem-virt-p2v.spec
  fi
fi

exit 0
