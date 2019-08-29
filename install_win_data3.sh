#!/bin/bash

# install complement data for the DVD version
# must be installed after the software and data part

function InstTexture {
  pkg=$1.tgz
  ddir=$2
  tmpdir=$(mktemp -d)
  pkgz=BaseData/$pkg
  if [ ! -e $pkgz ]; then
     wget http://sourceforge.net/projects/virtualmoon/files/3-%20data/Textures/Linux/$pkg/download -O $pkgz
  fi
  tar xvzf $pkgz -C $tmpdir
  cp -a $tmpdir/share/virtualmoon/* $ddir/
  rm -rf $tmpdir/share/virtualmoon/*
  rmdir $tmpdir/share/virtualmoon
  rmdir $tmpdir/share
  rmdir $tmpdir
}


destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/virtualmoon
fi

echo Install virtualmoon data3 to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/Database
install -v -m 644 Database/AVL\ Unnamed\ 2-0 $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-1 $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-2 $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-3 $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-4 $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-5 $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-6 $destdir/Database/

InstTexture TexturesChange_L5 $destdir
InstTexture TexturesLopam_L5 $destdir
InstTexture TexturesWAC_L5 $destdir

