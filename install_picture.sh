#!/bin/bash

# install complement pictures
# must be installed after the software and data part


function InstPicture {
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

echo Install virtualmoon pictures to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/share
install -m 755 -d $destdir/share/virtualmoon

InstPicture PictureBestOfAmateurs $destdir
InstPicture PictureBestOfCathala $destdir
InstPicture PictureBest_of_Peach $destdir
InstPicture PictureBest_Pic_du_Midi $destdir
