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

function InstPicture {
  pkg=$1.tgz
  ddir=$2
  pkgz=BaseData/$pkg
  if [ ! -e $pkgz ]; then
     wget http://sourceforge.net/projects/virtualmoon/files/3-%20data/Lunar%20Pictures%20Library/Linux/$pkg/download -O $pkgz
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
install -v -m 644 "Database/1_Formations_nommées_FR.csv" $destdir/share/virtualmoon/Database/
install -v -m 644 "Database/1_Named_formations_EN.csv" $destdir/share/virtualmoon/Database/
install -v -m 644 "Database/2_Formations_satellites_FR.csv" $destdir/share/virtualmoon/Database/
install -v -m 644 "Database/2_Satellite_formations_EN.csv" $destdir/share/virtualmoon/Database/
install -v -m 644 "Database/3_Formations_non_nommées_FR.csv" $destdir/share/virtualmoon/Database/
install -v -m 644 "Database/3_Unnamed_formations_EN.csv" $destdir/share/virtualmoon/Database/
install -v -m 644 "Database/4_Historical_sites_EN.csv" $destdir/share/virtualmoon/Database/
install -v -m 644 "Database/4_Sites_historiques_FR.csv" $destdir/share/virtualmoon/Database/
install -v -m 644 "Database/5_Domes_EN.csv" $destdir/share/virtualmoon/Database/
install -v -m 644 "Database/5_Dômes_FR.csv" $destdir/share/virtualmoon/Database/
install -v -m 644 "Database/6_Formations_pyroclastiques_FR.csv" $destdir/share/virtualmoon/Database/
install -v -m 644 "Database/6_Pyroclastic_formations_EN.csv" $destdir/share/virtualmoon/Database/
install -v -m 644 Database/glossary_uEN.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/glossary_uFR.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/licence.txt $destdir/share/virtualmoon/Database/
install -v -m 644 Database/lopamidx.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/lopamidx.txt $destdir/share/virtualmoon/Database/
install -v -m 644 Database/weblun.csv $destdir/share/virtualmoon/Database/

# big data
InstData Base_JPLeph $destdir
InstData Base_Airbrush $destdir
InstData Base_Bumpmap $destdir
InstData Base_Clementine $destdir
InstData Base_WAC $destdir
InstData Base_LOLA_Kaguya_Shade $destdir
InstData Base_Overlay $destdir
InstPicture PictureApollo $destdir

