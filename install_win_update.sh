#!/bin/bash

# put here only the files that need to be changed since last release

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
  install -v -m 644 virtualmoon/library/plan404/libplan404.dll  $destdir/
  unzip -d $destdir Installer/Windows/Data/sqlite3.zip
  unzip -d $destdir Installer/Windows/Data/plugins.zip
fi
if [ $OS_TARGET = win64 ]; then
  x86_64-win64-strip -v -o $destdir/atlun.exe virtualmoon/atlun.exe 
  x86_64-win64-strip -v -o $destdir/datlun.exe datlun/datlun.exe
  x86_64-win64-strip -v -o $destdir/photlun.exe photlun/photlun.exe
  install -v -m 644 virtualmoon/library/plan404/libplan404_x64.dll  $destdir/libplan404.dll
  unzip -d $destdir Installer/Windows/Data/sqlite3_x64.zip
fi
install -v -m 644 Installer/Windows/Data/readme.txt $destdir/
install -v -m 644 Installer/Windows/Data/lisezmoi.txt $destdir/
install -v -m 644 Installer/Windows/Data/licence.txt $destdir/
install -v -m 644 Installer/Windows/Data/licence_fr.txt $destdir/

install -m 755 -d $destdir/language
install -m 755 -d $destdir/Database
install -m 755 -d $destdir/doc
install -v -m 644 virtualmoon/language/maplun.en.po $destdir/language/
install -v -m 644 virtualmoon/language/maplun.fr.po $destdir/language/
install -v -m 644 datlun/language/datlun.en.po $destdir/language/
install -v -m 644 datlun/language/datlun.fr.po $destdir/language/
install -v -m 644 datlun/language/vmadatabase.en.po $destdir/language/
install -v -m 644 datlun/language/vmadatabase.fr.po $destdir/language/
install -v -m 644 photlun/language/photlun.en.po $destdir/language/
install -v -m 644 photlun/language/photlun.fr.po $destdir/language/

install -v -m 644 Database/Nearside_Named_uEN.csv $destdir/Database/
install -v -m 644 Database/Nearside_Named_uFR.csv $destdir/Database/

install -v -m 644 doc/* $destdir/doc/
