#!/bin/bash

# install minimal data for the Pro version
# must be installed after the software part

function InstData {
  pkg=$1.tgz
  ddir=$2
  tmpdir=$(mktemp -d)
  pkgz=BaseData/$pkg
  if [ ! -e $pkgz ]; then
     wget http://sourceforge.net/projects/virtualmoon/files/6-Source_Data/$pkg/download -O $pkgz
  fi
  tar xvzf $pkgz -C $tmpdir
  cp -a $tmpdir/share/virtualmoon/* $ddir/
  rm -rf $tmpdir/share/virtualmoon/*
  rmdir $tmpdir/share/virtualmoon
  rmdir $tmpdir/share
  rmdir $tmpdir
}

OS_TARGET=$1
destdir=$2

if [ -z "$OS_TARGET=" ]; then
   export OS_TARGET==win32
fi

if [ -z "$destdir" ]; then
   export destdir=/tmp/virtualmoon
fi

echo Install virtualmoon $OS_TARGET to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/Database
install -v -m 644 Database/* $destdir/Database/

InstData Base_JPLeph $destdir
InstData Base_Airbrush $destdir
InstData Base_Bumpmap $destdir
InstData Base_Clementine $destdir
InstData Base_Overlay $destdir
