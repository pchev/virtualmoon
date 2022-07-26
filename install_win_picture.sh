#!/bin/bash

# install complement pictures
# must be installed after the software and data part

function InstPicture {
  pkg=$1.tgz
  ddir=$2
  tmpdir=$(mktemp -d)
  pkgz=BaseData/$pkg
  if [ ! -e $pkgz ]; then
     wget http://sourceforge.net/projects/virtualmoon/files/OldFiles/6-Source_Data/$pkg/download -O $pkgz
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

echo Install virtualmoon pictures to $destdir

install -m 755 -d $destdir

InstPicture PictureBestOfAmateurs $destdir
InstPicture PictureBestOfCathala $destdir
InstPicture PictureBest_of_Peach $destdir
InstPicture PictureBest_Pic_du_Midi $destdir
InstPicture PictureBest_of_Brahic $destdir
InstPicture PictureBest_of_Viladrich $destdir
