#!/bin/bash

# install minimal data for the Pro version
# must be installed after the software part

function InstData {
  pkg=$1.tgz
  ddir=$2
  pkgz=BaseData/$pkg
  if [ ! -e $pkgz ]; then
     wget http://sourceforge.net/projects/virtualmoon/files/OldFiles/6-Source_Data/$pkg/download -O $pkgz
  fi
  tar xvzf $pkgz -C $ddir
}

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

echo Install virtualmoon data to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/share
install -m 755 -d $destdir/share/virtualmoon
install -m 755 -d $destdir/share/virtualmoon/Textures

install -m 755 -d $destdir/share/virtualmoon/data
install -v -m 644 data/country.tab $destdir/share/virtualmoon/data/
install -v -m 644 data/retic.cur $destdir/share/virtualmoon/data/

# documentation
install -m 755 -d "$destdir/share/virtualmoon/My Images"
install -m 755 -d $destdir/share/virtualmoon/doc
install -m 755 -d $destdir/share/virtualmoon/Encyclopedia
install -v -m 644 doc/* $destdir/share/virtualmoon/doc/
install -v -m 644 Encyclopedia/* $destdir/share/virtualmoon/Encyclopedia/

# database
install -m 755 -d $destdir/share/virtualmoon/Database
install -v -m 644 Database/glossary_uEN.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/glossary_uFR.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/licence.txt $destdir/share/virtualmoon/Database/
install -v -m 644 Database/lopamidx.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/lopamidx.txt $destdir/share/virtualmoon/Database/
install -v -m 644 Database/weblun.csv $destdir/share/virtualmoon/Database/

# big data
InstData Base_Database $destdir
InstData Base_JPLeph $destdir
InstData Base_Kernels $destdir
InstData Base_Dem $destdir
InstData Base_WAC_LOWSUN $destdir
InstData Base_Change $destdir
InstData Base_Overlay $destdir
InstData Base_Historical $destdir
InstPicture PictureApollo $destdir
InstPicture PictureProbes $destdir
