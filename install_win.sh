#!/bin/bash

# install the software 

OS_TARGET=$1
destdir=$2

if [ -z "$OS_TARGET" ]; then
   export OS_TARGET=win32
fi

if [ -z "$destdir" ]; then
   export destdir=/tmp/virtualmoon
fi

echo Install virtualmoon $OS_TARGET to $destdir

# software
install -m 755 -d $destdir
if [ $OS_TARGET = win32 ]; then
  strip -v -o $destdir/atlun.exe virtualmoon/atlun.exe 
  strip -v -o $destdir/datlun.exe datlun/datlun.exe
  strip -v -o $destdir/photlun.exe photlun/photlun.exe
  strip -v -o $destdir/weblun.exe weblun/weblun.exe
  strip -v -o $destdir/cclun.exe cclun/cclun.exe
  strip -v -o $destdir/calclun.exe calclun/calclun.exe
  strip -v -o $destdir/notelun.exe notelun/notelun.exe
  install -v -m 644 virtualmoon/library/plan404/libplan404.dll  $destdir/
  install -v -m 644 calclun/cspice/libcspice32.dll  $destdir/libcspice32.dll
  unzip -o -d $destdir Installer/Windows/Data/sqlite3.zip
  unzip -o -d $destdir Installer/Windows/Data/openssl-win32.zip
fi
if [ $OS_TARGET = win64 ]; then
  strip -v -o $destdir/atlun.exe virtualmoon/atlun.exe 
  strip -v -o $destdir/datlun.exe datlun/datlun.exe
  strip -v -o $destdir/photlun.exe photlun/photlun.exe
  strip -v -o $destdir/weblun.exe weblun/weblun.exe
  strip -v -o $destdir/cclun.exe cclun/cclun.exe
  strip -v -o $destdir/calclun.exe calclun/calclun.exe
  strip -v -o $destdir/notelun.exe notelun/notelun.exe
  install -v -m 644 virtualmoon/library/plan404/libplan404_x64.dll  $destdir/libplan404.dll
  install -v -m 644 calclun/cspice/libcspice64.dll  $destdir/libcspice64.dll
  unzip -o -d $destdir Installer/Windows/Data/sqlite3_x64.zip
  unzip -o -d $destdir Installer/Windows/Data/openssl-win64.zip
fi
install -v -m 644 Installer/Windows/Data/readme.txt $destdir/
install -v -m 644 Installer/Windows/Data/lisezmoi.txt $destdir/
install -v -m 644 Installer/Windows/Data/licence.txt $destdir/
install -v -m 644 Installer/Windows/Data/licence_fr.txt $destdir/

# translation
install -m 755 -d $destdir/language
install -v -m 644 language/maplun.en.po $destdir/language/
install -v -m 644 language/maplun.fr.po $destdir/language/
install -v -m 644 language/datlun.en.po $destdir/language/
install -v -m 644 language/datlun.fr.po $destdir/language/
install -v -m 644 language/vmadatabase.en.po $destdir/language/
install -v -m 644 language/vmadatabase.fr.po $destdir/language/
install -v -m 644 language/photlun.en.po $destdir/language/
install -v -m 644 language/photlun.fr.po $destdir/language/
install -v -m 644 language/weblun.en.po $destdir/language/
install -v -m 644 language/weblun.fr.po $destdir/language/
install -v -m 644 language/cclun.en.po $destdir/language/
install -v -m 644 language/cclun.fr.po $destdir/language/
install -v -m 644 language/calclun.en.po $destdir/language/
install -v -m 644 language/calclun.fr.po $destdir/language/
install -v -m 644 language/notelun.en.po $destdir/language/
install -v -m 644 language/notelun.fr.po $destdir/language/

install -m 755 -d $destdir/data
install -v -m 644 data/country.tab $destdir/data/
install -v -m 644 data/retic.cur $destdir/data/
cp -a  data/zoneinfo $destdir/data/

# documentation
install -m 755 -d "$destdir/My Images"
install -m 755 -d $destdir/doc
install -m 755 -d $destdir/Encyclopedia
install -v -m 644 Encyclopedia/* $destdir/Encyclopedia/
