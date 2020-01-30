#!/bin/bash

# put here only the files that need to be changed since last release

function InstData {
  pkg=$1.tgz
  ddir=$2
  pkgz=BaseData/$pkg
  if [ ! -e $pkgz ]; then
     wget http://sourceforge.net/projects/virtualmoon/files/OldFiles/6-Source_Data/$pkg/download -O $pkgz
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
install -m 755 -d $destdir/share/virtualmoon
install -v -m 755 -s virtualmoon/atlun  $destdir/bin/atlun
install -v -m 755 -s datlun/datlun  $destdir/bin/datlun
install -v -m 644 Installer/Linux/vmapro/share/applications/virtualmoon.desktop $destdir/share/applications/virtualmoon.desktop
install -v -m 644 Installer/Linux/vmapro/share/doc/virtualmoon/changelog $destdir/share/doc/virtualmoon/changelog
install -v -m 644 Installer/Linux/vmapro/share/doc/virtualmoon/copyright $destdir/share/doc/virtualmoon/copyright
install -v -m 644 Installer/Linux/vmapro/share/pixmaps/virtualmoon.xpm $destdir/share/pixmaps/virtualmoon.xpm

# database
install -m 755 -d $destdir/share/virtualmoon/Database
install -v -m 644 Database/AVL\ Named\ EN_utf8.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Satellite\ EN_utf8.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Registered\ EN_utf8.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ EN_utf8.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Named\ FR_utf8.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Satellite\ FR_utf8.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Registered\ FR_utf8.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ FR_utf8.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-0 $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-1 $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-2 $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-3 $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-4 $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-5 $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-6 $destdir/share/virtualmoon/Database/

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
