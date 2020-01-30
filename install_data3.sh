#!/bin/bash

# install complement data for the DVD version
# must be installed after the software and data part

function InstTexture {
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

echo Install virtualmoon data3 to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/share
install -m 755 -d $destdir/share/virtualmoon
install -m 755 -d $destdir/share/virtualmoon/Database
install -v -m 644 Database/AVL\ Unnamed\ 2-0 $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-1 $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-2 $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-3 $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-4 $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-5 $destdir/share/virtualmoon/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-6 $destdir/share/virtualmoon/Database/

InstTexture TexturesChange_L5 $destdir
InstTexture TexturesLopam_L5 $destdir
InstTexture TexturesWAC_L5 $destdir
InstTexture Texture_LOLA_Kaguya_Shade_L5 $destdir
