#!/bin/bash

# install minimal data for the Pro version
# must be installed after the software part

function InstData {
  pkg=$1.tgz
  ddir=$2
  pkgz=BaseData/$pkg
  if [ ! -e $pkgz ]; then
     wget http://sourceforge.net/projects/virtualmoon/files/6-Source_Data/$pkg/download -O $pkgz
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

# documentation
install -m 755 -d "$destdir/share/virtualmoon/My Images"
install -m 755 -d $destdir/share/virtualmoon/doc
install -m 755 -d $destdir/share/virtualmoon/Encyclopedia
install -v -m 644 doc/* $destdir/share/virtualmoon/doc/
install -v -m 644 Encyclopedia/* $destdir/share/virtualmoon/Encyclopedia/

# database
install -m 755 -d $destdir/share/virtualmoon/Database
install -v -m 644 Database/Domes_uEN.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Domes_uFR.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Farside_Named_uEN.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Farside_Named_uFR.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Farside_Satellite_uEN.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Farside_Satellite_uFR.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/glossary_uEN.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/glossary_uFR.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Historical_uEN.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Historical_uFR.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/licence.txt $destdir/share/virtualmoon/Database/
install -v -m 644 Database/lopamidx.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/lopamidx.txt $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Nearside_Named_uEN.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Nearside_Named_uFR.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Nearside_Satellite_uEN.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Nearside_Satellite_uFR.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Pyroclastic_uEN.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Pyroclastic_uFR.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Farside_Unnamed_uEN.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Farside_Unnamed_uFR.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Nearside_Unnamed_uEN.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/Nearside_Unnamed_uFR.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/weblun.csv $destdir/share/virtualmoon/Database/

# big data
InstData Base_JPLeph $destdir
InstData Base_Airbrush $destdir
InstData Base_Bumpmap $destdir
InstData Base_Clementine $destdir
InstData Base_WAC $destdir
InstData Base_Overlay $destdir
