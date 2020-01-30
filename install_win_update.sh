#!/bin/bash

# put here only the files that need to be changed since last release

function InstData {
  pkg=$1.tgz
  ddir=$2
  tmpdir=$(mktemp -d)
  pkgz=BaseData/$pkg
  if [ ! -e $pkgz ]; then
     wget http://sourceforge.net/projects/virtualmoon/files/OldFiles/6-Source_Data/$pkg/download -O $pkgz
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
     wget http://sourceforge.net/projects/virtualmoon/files/OldFiles/6-Source_Data/$pkg/download -O $pkgz
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
     wget http://sourceforge.net/projects/virtualmoon/files/OldFiles/6-Source_Data/$pkg/download -O $pkgz
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
fi
if [ $OS_TARGET = win64 ]; then
  x86_64-win64-strip -v -o $destdir/atlun.exe virtualmoon/atlun.exe 
  x86_64-win64-strip -v -o $destdir/datlun.exe datlun/datlun.exe 
fi
install -v -m 644 Installer/Windows/Data/readme.txt $destdir/
install -v -m 644 Installer/Windows/Data/lisezmoi.txt $destdir/
install -v -m 644 Installer/Windows/Data/licence.txt $destdir/
install -v -m 644 Installer/Windows/Data/licence_fr.txt $destdir/

# database
install -m 755 -d $destdir/Database
install -v -m 644 Database/AVL\ Named\ EN_utf8.csv $destdir/Database/
install -v -m 644 Database/AVL\ Satellite\ EN_utf8.csv $destdir/Database/
install -v -m 644 Database/AVL\ Registered\ EN_utf8.csv $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ EN_utf8.csv $destdir/Database/
install -v -m 644 Database/AVL\ Named\ FR_utf8.csv $destdir/Database/
install -v -m 644 Database/AVL\ Satellite\ FR_utf8.csv $destdir/Database/
install -v -m 644 Database/AVL\ Registered\ FR_utf8.csv $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ FR_utf8.csv $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-0 $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-1 $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-2 $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-3 $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-4 $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-5 $destdir/Database/
install -v -m 644 Database/AVL\ Unnamed\ 2-6 $destdir/Database/

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
cp -a  data/zoneinfo $destdir/data/

