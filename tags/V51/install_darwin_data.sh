#!/bin/bash

# install minimal data for the Pro version
# must be installed after the software part

function InstData {
  pkg=$1.tgz
  ddir=$2
  tmpdir=$(mktemp -d -t tmp)
  pkgz=BaseData/$pkg
  if [ ! -e $pkgz ]; then
     curl -L -o $pkgz http://sourceforge.net/projects/virtualmoon/files/6-Source_Data/$pkg/download
  fi
  tar xvzf $pkgz -C $tmpdir
  cp -R -p $tmpdir/share/virtualmoon/* $ddir/
  rm -rf $tmpdir/share/virtualmoon/*
  rmdir $tmpdir/share/virtualmoon
  rmdir $tmpdir/share
  rmdir $tmpdir
}

destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/virtualmoon
fi

echo Install virtualmoon data to $destdir

# database
install -m 755 -d $destdir/Database
install -v -m 644 Database/Domes_uEN.csv $destdir/Database/
install -v -m 644 Database/Domes_uFR.csv $destdir/Database/
install -v -m 644 Database/Farside_Named_uEN.csv $destdir/Database/
install -v -m 644 Database/Farside_Named_uFR.csv $destdir/Database/
install -v -m 644 Database/Farside_Satellite_uEN.csv $destdir/Database/
install -v -m 644 Database/Farside_Satellite_uFR.csv $destdir/Database/
install -v -m 644 Database/glossary_uEN.csv $destdir/Database/
install -v -m 644 Database/glossary_uFR.csv $destdir/Database/
install -v -m 644 Database/Historical_uEN.csv $destdir/Database/
install -v -m 644 Database/Historical_uFR.csv $destdir/Database/
install -v -m 644 Database/licence.txt $destdir/Database/
install -v -m 644 Database/lopamidx.csv $destdir/Database/
install -v -m 644 Database/lopamidx.txt $destdir/Database/
install -v -m 644 Database/Nearside_Named_uEN.csv $destdir/Database/
install -v -m 644 Database/Nearside_Named_uFR.csv $destdir/Database/
install -v -m 644 Database/Nearside_Satellite_uEN.csv $destdir/Database/
install -v -m 644 Database/Nearside_Satellite_uFR.csv $destdir/Database/
install -v -m 644 Database/Pyroclastic_uEN.csv $destdir/Database/
install -v -m 644 Database/Pyroclastic_uFR.csv $destdir/Database/

# documentation
install -m 755 -d "$destdir/My Images"
install -m 755 -d $destdir/doc
install -m 755 -d $destdir/Encyclopedia
install -v -m 644 doc/* $destdir/doc/
install -v -m 644 Encyclopedia/* $destdir/Encyclopedia/


InstData Base_JPLeph $destdir
InstData Base_Airbrush $destdir
InstData Base_Bumpmap $destdir
InstData Base_Clementine $destdir
InstData Base_Overlay $destdir

