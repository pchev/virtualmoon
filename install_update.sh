#!/bin/bash

# put here only the files that need to be changed since last release

function InstData {
  pkg=$1.tgz
  ddir=$2
  pkgz=BaseData/$pkg
  if [ ! -e $pkgz ]; then
     wget http://sourceforge.net/projects/virtualmoon/files/6-Source_Data/$pkg/download -O $pkgz
  fi
  tar xvzf $pkgz -C $ddir
}

destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/virtualmoon
fi

echo Install virtualmoon update to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/bin
install -m 755 -d $destdir/lib
install -m 755 -d $destdir/share
install -m 755 -d $destdir/share/applications
install -m 755 -d $destdir/share/doc
install -m 755 -d $destdir/share/doc/virtualmoon
install -m 755 -d $destdir/share/pixmaps
install -v -m 755 -s virtualmoon/atlun  $destdir/bin/atlun
install -v -m 644 Installer/Linux/vmapro/share/applications/virtualmoon.desktop $destdir/share/applications/virtualmoon.desktop
install -v -m 644 Installer/Linux/vmapro/share/doc/virtualmoon/changelog $destdir/share/doc/virtualmoon/changelog
install -v -m 644 Installer/Linux/vmapro/share/doc/virtualmoon/copyright $destdir/share/doc/virtualmoon/copyright
install -v -m 644 Installer/Linux/vmapro/share/pixmaps/virtualmoon.xpm $destdir/share/pixmaps/virtualmoon.xpm

