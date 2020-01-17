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

destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/virtualmoon
fi

echo Install virtualmoon data to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/Database
install -v -m 644 "Database/1_Formations_nommées_FR.csv" $destdir/Database/
install -v -m 644 "Database/1_Named_formations_EN.csv" $destdir/Database/
install -v -m 644 "Database/2_Formations_satellites_FR.csv" $destdir/Database/
install -v -m 644 "Database/2_Satellite_formations_EN.csv" $destdir/Database/
install -v -m 644 "Database/3_Formations_non_nommées_FR.csv" $destdir/Database/
install -v -m 644 "Database/3_Unnamed_formations_EN.csv" $destdir/Database/
install -v -m 644 "Database/4_Historical_sites_EN.csv" $destdir/Database/
install -v -m 644 "Database/4_Sites_historiques_FR.csv" $destdir/Database/
install -v -m 644 "Database/5_Domes_EN.csv" $destdir/Database/
install -v -m 644 "Database/5_Dômes_FR.csv" $destdir/Database/
install -v -m 644 "Database/6_Formations_pyroclastiques_FR.csv" $destdir/Database/
install -v -m 644 "Database/6_Pyroclastic_formations_EN.csv" $destdir/Database/
install -v -m 644 Database/glossary_uEN.csv $destdir/Database/
install -v -m 644 Database/glossary_uFR.csv $destdir/Database/
install -v -m 644 Database/licence.txt $destdir/Database/
install -v -m 644 Database/lopamidx.csv $destdir/Database/
install -v -m 644 Database/lopamidx.txt $destdir/Database/
install -v -m 644 Database/weblun.csv $destdir/Database/

# big data
InstData Base_JPLeph $destdir
InstData Base_Airbrush $destdir
InstData Base_Bumpmap $destdir
InstData Base_Clementine $destdir
InstData Base_WAC $destdir
InstData Base_Overlay $destdir
