#!/bin/bash

# install the translation
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

echo Install virtualmoon translation to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/language

lg='ca cs de el es hu it lt nl sk'
for lang in $lg; do
  install -v -m 644 virtualmoon/language/maplun.$lang.po $destdir/language/
  install -v -m 644 datlun/language/datlun.$lang.po $destdir/language/
  install -v -m 644 datlun/language/vmadatabase.$lang.po $destdir/language/
  install -v -m 644 photlun/language/photlun.$lang.po $destdir/language/
  install -v -m 644 weblun/language/weblun.$lang.po $destdir/language/
  install -v -m 644 cclun/language/cclun.$lang.po $destdir/language/
done

InstData Translation_Database $destdir
InstData Translation_Doc $destdir


