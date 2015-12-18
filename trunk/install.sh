#!/bin/bash

# install the software 

destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/virtualmoon
fi

echo Install virtualmoon to $destdir

# software
install -m 755 -d $destdir
install -m 755 -d $destdir/bin
install -m 755 -d $destdir/share
install -m 755 -d $destdir/share/applications
install -m 755 -d $destdir/share/doc
install -m 755 -d $destdir/share/doc/virtualmoon
install -m 755 -d $destdir/share/pixmaps
install -v -m 755 -s virtualmoon/atlun  $destdir/bin/atlun
install -v -m 755 -s datlun/datlun  $destdir/bin/datlun
install -v -m 755 -s photlun/photlun  $destdir/bin/photlun
install -v -m 755 -s weblun/weblun  $destdir/bin/weblun
install -v -m 755 -s cclun/cclun  $destdir/bin/cclun
install -v -m 644 Installer/Linux/vmapro/share/applications/virtualmoon.desktop $destdir/share/applications/virtualmoon.desktop
install -v -m 644 Installer/Linux/vmapro/share/applications/cclun.desktop $destdir/share/applications/cclun.desktop
install -v -m 644 Installer/Linux/vmapro/share/doc/virtualmoon/changelog $destdir/share/doc/virtualmoon/changelog
install -v -m 644 Installer/Linux/vmapro/share/doc/virtualmoon/copyright $destdir/share/doc/virtualmoon/copyright
install -v -m 644 Installer/Linux/vmapro/share/pixmaps/virtualmoon.xpm $destdir/share/pixmaps/virtualmoon.xpm
install -v -m 644 Installer/Linux/vmapro/share/pixmaps/cclun.xpm $destdir/share/pixmaps/cclun.xpm



