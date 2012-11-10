#!/bin/bash

# put here only the files that need to be changed since last release

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

function InstPicture {
  pkg=$1.tgz
  ddir=$2
  tmpdir=$(mktemp -d)
  pkgz=BaseData/$pkg
  if [ ! -e $pkgz ]; then
     wget http://sourceforge.net/projects/virtualmoon/files/3-%20data/Lunar%20Pictures%20Library/Linux/$pkg/download -O $pkgz
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

if [ $OS_TARGET = win32 ]; then
  i386-win32-strip -v -o $destdir/atlun.exe virtualmoon/atlun.exe 
  i386-win32-strip -v -o $destdir/datlun.exe datlun/datlun.exe
  i386-win32-strip -v -o $destdir/photlun.exe photlun/photlun.exe
  i386-win32-strip -v -o $destdir/weblun.exe weblun/weblun.exe
  i386-win32-strip -v -o $destdir/cclun.exe cclun/cclun.exe
  install -v -m 644 virtualmoon/library/plan404/libplan404.dll  $destdir/
  unzip -d $destdir Installer/Windows/Data/sqlite3.zip
  unzip -d $destdir Installer/Windows/Data/plugins.zip
fi
if [ $OS_TARGET = win64 ]; then
  x86_64-win64-strip -v -o $destdir/atlun.exe virtualmoon/atlun.exe 
  x86_64-win64-strip -v -o $destdir/datlun.exe datlun/datlun.exe
  x86_64-win64-strip -v -o $destdir/photlun.exe photlun/photlun.exe
  x86_64-win64-strip -v -o $destdir/weblun.exe weblun/weblun.exe
  x86_64-win64-strip -v -o $destdir/cclun.exe cclun/cclun.exe
  install -v -m 644 virtualmoon/library/plan404/libplan404_x64.dll  $destdir/libplan404.dll
  unzip -d $destdir Installer/Windows/Data/sqlite3_x64.zip
fi
install -v -m 644 Installer/Windows/Data/readme.txt $destdir/
install -v -m 644 Installer/Windows/Data/lisezmoi.txt $destdir/
install -v -m 644 Installer/Windows/Data/licence.txt $destdir/
install -v -m 644 Installer/Windows/Data/licence_fr.txt $destdir/

# translation
install -m 755 -d $destdir/language
install -v -m 644 virtualmoon/language/maplun.en.po $destdir/language/
install -v -m 644 virtualmoon/language/maplun.fr.po $destdir/language/
install -v -m 644 datlun/language/datlun.en.po $destdir/language/
install -v -m 644 datlun/language/datlun.fr.po $destdir/language/
install -v -m 644 datlun/language/vmadatabase.en.po $destdir/language/
install -v -m 644 datlun/language/vmadatabase.fr.po $destdir/language/
install -v -m 644 photlun/language/photlun.en.po $destdir/language/
install -v -m 644 photlun/language/photlun.fr.po $destdir/language/
install -v -m 644 weblun/language/weblun.en.po $destdir/language/
install -v -m 644 weblun/language/weblun.fr.po $destdir/language/
install -v -m 644 cclun/language/cclun.en.po $destdir/language/
install -v -m 644 cclun/language/cclun.fr.po $destdir/language/

install -m 755 -d $destdir/data
install -v -m 644 data/country.tab $destdir/data/
install -v -m 644 data/retic.cur $destdir/data/
cp -a  data/zoneinfo $destdir/data/

# documentation
install -m 755 -d "$destdir/My Images"
install -m 755 -d $destdir/doc
install -m 755 -d $destdir/Encyclopedia
install -v -m 644 Encyclopedia/* $destdir/Encyclopedia/

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
install -v -m 644 Database/Farside_Unnamed_uEN.csv $destdir/Database/
install -v -m 644 Database/Farside_Unnamed_uFR.csv $destdir/Database/
install -v -m 644 Database/Nearside_Unnamed_uEN.csv $destdir/Database/
install -v -m 644 Database/Nearside_Unnamed_uFR.csv $destdir/Database/
install -v -m 644 Database/weblun.csv $destdir/Database/

InstData Base_Bumpmap $destdir
InstData Base_Overlay $destdir

InstTexture TextureHistorical $destdir

