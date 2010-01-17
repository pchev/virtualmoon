#!/bin/bash

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
  unzip -d $destdir Installer/Windows/Data/fiximg.zip
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

install -m 755 -d $destdir/data
install -m 755 -d $destdir/data/jpleph
install -m 755 -d $destdir/language
install -m 755 -d "$destdir/My Images"
install -m 755 -d $destdir/Database
install -m 755 -d $destdir/doc
install -m 755 -d $destdir/Encyclopedia
install -m 755 -d $destdir/Textures
install -m 755 -d $destdir/Textures/Airbrush
install -m 755 -d $destdir/Textures/Airbrush/L1
install -m 755 -d $destdir/Textures/Bumpmap
install -m 755 -d $destdir/Textures/Overlay
install -m 755 -d $destdir/Textures/Overlay/caption
install -v -m 644 virtualmoon/language/maplun.en.po $destdir/language/
install -v -m 644 virtualmoon/language/maplun.fr.po $destdir/language/
install -v -m 644 datlun/language/datlun.en.po $destdir/language/
install -v -m 644 datlun/language/datlun.fr.po $destdir/language/
install -v -m 644 datlun/language/vmadatabase.en.po $destdir/language/
install -v -m 644 datlun/language/vmadatabase.fr.po $destdir/language/
install -v -m 644 photlun/language/photlun.en.po $destdir/language/
install -v -m 644 photlun/language/photlun.fr.po $destdir/language/
install -v -m 644 data/country.tab $destdir/data/
install -v -m 644 data/retic.cur $destdir/data/
install -v -m 644 data/jpleph/unxp1900.421 $destdir/data/jpleph/
install -v -m 644 textures/Airbrush/L1/* $destdir/Textures/Airbrush/L1/
install -v -m 644 textures/Bumpmap/*.jpg $destdir/Textures/Bumpmap/
install -v -m 644 textures/Bumpmap/kaguya.txt $destdir/Textures/Bumpmap/
install -v -m 644 textures/Overlay/*.jpg $destdir/Textures/Overlay/
install -v -m 644 textures/Overlay/caption/*.jpg $destdir/Textures/Overlay/caption/
install -v -m 644 Database/* $destdir/Database/
install -v -m 644 doc/* $destdir/doc/
install -v -m 644 Encyclopedia/* $destdir/Encyclopedia/
