#!/bin/bash 

# script to make an additional data package on a Mac OS X system

function InstTar {
  pkg=$1.tgz
  ddir=$2
  tmpdir=$(mktemp -d -t tmp)
  pkgz=BaseData/$pkg
echo  tar xzf $pkgz -C $tmpdir
  tar xzf $pkgz -C $tmpdir
  if [[ $? -ne 0 ]]; then exit 1;fi
  cp -R -p $tmpdir/share/virtualmoon/* $ddir/
  if [[ $? -ne 0 ]]; then exit 1;fi
  rm -rf $tmpdir/share/virtualmoon/*
  rmdir $tmpdir/share/virtualmoon
  rmdir $tmpdir/share
  rmdir $tmpdir
}

basedir=/Volumes/Data/tmp/vmadata  # Be sure this is set to a non existent directory, it is removed after the run!
builddir=$basedir/Virtual_Moon_Atlas

sfile=$1  # TextureWAC
pkgp=$sfile.packproj

wd=`pwd`

mkdir -p $builddir

echo  InstTar $sfile $builddir
  InstTar $sfile $builddir
  if [[ $? -ne 0 ]]; then exit 1;fi
  cp Installer/Mac/$pkgp $basedir
  cd $basedir
  freeze -v $pkgp
  if [[ $? -ne 0 ]]; then exit 1;fi
  hdiutil create -anyowners -volname $sfile -imagekey zlib-level=9 -format UDZO -srcfolder ./build $sfile.dmg
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $sfile.dmg $wd/
  if [[ $? -ne 0 ]]; then exit 1;fi

  rm -rf $basedir

