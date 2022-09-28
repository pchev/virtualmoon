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
install -m 755 -d $destdir/lib
install -m 755 -d $destdir/share
install -m 755 -d $destdir/share/virtualmoon
install -m 755 -d $destdir/share/applications
install -m 755 -d $destdir/share/doc
install -m 755 -d $destdir/share/doc/virtualmoon
install -m 755 -d $destdir/share/pixmaps
install -v -m 755 -s virtualmoon/atlun  $destdir/bin/atlun
install -v -m 755 -s datlun/datlun  $destdir/bin/datlun
install -v -m 755 -s photlun/photlun  $destdir/bin/photlun
install -v -m 755 -s weblun/weblun  $destdir/bin/weblun
install -v -m 755 -s cclun/cclun  $destdir/bin/cclun
install -v -m 755 -s calclun/calclun  $destdir/bin/calclun
install -v -m 755 -s notelun/notelun  $destdir/bin/notelun
install -v -m 644 calclun/cspice/libcspice.so  $destdir/lib/libcspicevma.so
install -v -m 644 Installer/Linux/vmapro/share/applications/virtualmoon.desktop $destdir/share/applications/virtualmoon.desktop
install -v -m 644 Installer/Linux/vmapro/share/applications/cclun.desktop $destdir/share/applications/cclun.desktop
install -v -m 644 Installer/Linux/vmapro/share/applications/calclun.desktop $destdir/share/applications/calclun.desktop
install -v -m 644 Installer/Linux/vmapro/share/applications/photlun.desktop $destdir/share/applications/photlun.desktop
install -v -m 644 Installer/Linux/vmapro/share/applications/datlun.desktop $destdir/share/applications/datlun.desktop
install -v -m 644 Installer/Linux/vmapro/share/applications/notelun.desktop $destdir/share/applications/notelun.desktop
install -v -m 644 Installer/Linux/vmapro/share/doc/virtualmoon/changelog $destdir/share/doc/virtualmoon/changelog
install -v -m 644 Installer/Linux/vmapro/share/doc/virtualmoon/copyright $destdir/share/doc/virtualmoon/copyright
install -v -m 644 Installer/Linux/vmapro/share/pixmaps/virtualmoon.xpm $destdir/share/pixmaps/virtualmoon.xpm
install -v -m 644 Installer/Linux/vmapro/share/pixmaps/cclun.xpm $destdir/share/pixmaps/cclun.xpm
install -v -m 644 Installer/Linux/vmapro/share/pixmaps/calclun.xpm $destdir/share/pixmaps/calclun.xpm
install -v -m 644 Installer/Linux/vmapro/share/pixmaps/photlun.xpm $destdir/share/pixmaps/photlun.xpm
install -v -m 644 Installer/Linux/vmapro/share/pixmaps/datlun.xpm $destdir/share/pixmaps/datlun.xpm
install -v -m 644 Installer/Linux/vmapro/share/pixmaps/notelun.xpm $destdir/share/pixmaps/notelun.xpm

# translation
install -m 755 -d $destdir/share/virtualmoon/language
install -v -m 644 virtualmoon/language/maplun.en.po $destdir/share/virtualmoon/language/
install -v -m 644 virtualmoon/language/maplun.fr.po $destdir/share/virtualmoon/language/
install -v -m 644 datlun/language/datlun.en.po $destdir/share/virtualmoon/language/
install -v -m 644 datlun/language/datlun.fr.po $destdir/share/virtualmoon/language/
install -v -m 644 datlun/language/vmadatabase.en.po $destdir/share/virtualmoon/language/
install -v -m 644 datlun/language/vmadatabase.fr.po $destdir/share/virtualmoon/language/
install -v -m 644 photlun/language/photlun.en.po $destdir/share/virtualmoon/language/
install -v -m 644 photlun/language/photlun.fr.po $destdir/share/virtualmoon/language/
install -v -m 644 weblun/language/weblun.en.po $destdir/share/virtualmoon/language/
install -v -m 644 weblun/language/weblun.fr.po $destdir/share/virtualmoon/language/
install -v -m 644 cclun/language/cclun.en.po $destdir/share/virtualmoon/language/
install -v -m 644 cclun/language/cclun.fr.po $destdir/share/virtualmoon/language/
install -v -m 644 calclun/language/calclun.en.po $destdir/share/virtualmoon/language/
install -v -m 644 calclun/language/calclun.fr.po $destdir/share/virtualmoon/language/
install -v -m 644 notelun/language/notelun.en.po $destdir/share/virtualmoon/language/
install -v -m 644 notelun/language/notelun.fr.po $destdir/share/virtualmoon/language/
