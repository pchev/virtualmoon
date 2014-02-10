#!/bin/bash

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

echo Install virtualmoon update to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/atlun.app
install -m 755 -d $destdir/atlun.app/Contents
install -m 755 -d $destdir/atlun.app/Contents/MacOS
install -m 755 -d $destdir/atlun.app/Contents/Resources
install -v -m 644 Installer/Mac/vmapro/atlun.app/Contents/Info.plist $destdir/atlun.app/Contents/
install -v -m 644 Installer/Mac/vmapro/atlun.app/Contents/PkgInfo $destdir/atlun.app/Contents/
install -v -m 644 Installer/Mac/vmapro/atlun.app/Contents/Resources/atlun.icns $destdir/atlun.app/Contents/Resources/
install -m 755 -d $destdir/datlun.app
install -m 755 -d $destdir/datlun.app/Contents
install -m 755 -d $destdir/datlun.app/Contents/MacOS
install -m 755 -d $destdir/datlun.app/Contents/Resources
install -v -m 644 Installer/Mac/vmapro/datlun.app/Contents/Info.plist $destdir/datlun.app/Contents/
install -v -m 644 Installer/Mac/vmapro/datlun.app/Contents/PkgInfo $destdir/datlun.app/Contents/
install -v -m 644 Installer/Mac/vmapro/datlun.app/Contents/Resources/datlun.icns $destdir/datlun.app/Contents/Resources/

install -v -m 755 -s virtualmoon/atlun  $destdir/atlun.app/Contents/MacOS/atlun
install -v -m 755 -s datlun/datlun  $destdir/datlun.app/Contents/MacOS/datlun
install -v -m 644 Installer/Mac/vmapro/licence.txt $destdir/
install -v -m 644 Installer/Mac/vmapro/readme.txt $destdir/

