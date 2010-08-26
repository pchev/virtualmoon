#!/bin/bash

# install complement data for the CDrom version
# must be installed after the software and data part

function InstTexture {
  pkg=$1.tgz
  ddir=$2
  tmpdir=$(mktemp -d -t tmp)
  pkgz=BaseData/$pkg
  if [ ! -e $pkgz ]; then
     curl -L -o $pkgz http://sourceforge.net/projects/virtualmoon/files/3-%20data/Textures/Linux/$pkg/download
  fi
  tar xvzf $pkgz -C $tmpdir
  cp -R -p $tmpdir/share/virtualmoon/* $ddir/
  rm -rf $tmpdir/share/virtualmoon/*
  rmdir $tmpdir/share/virtualmoon
  rmdir $tmpdir/share
  rmdir $tmpdir
}

function InstPicture {
  pkg=$1.tgz
  ddir=$2
  tmpdir=$(mktemp -d -t tmp)
  pkgz=BaseData/$pkg
  if [ ! -e $pkgz ]; then
     curl -L -o $pkgz http://sourceforge.net/projects/virtualmoon/files/3-%20data/Lunar%20Pictures%20Library/Linux/$pkg/download
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

echo Install virtualmoon data2 to $destdir

install -m 755 -d $destdir

InstTexture TexturesAirbrush_na $destdir
InstTexture TexturesClementine $destdir
InstTexture TexturesLopam $destdir

InstPicture PictureApollo $destdir
InstPicture PictureApolloMapping $destdir
InstPicture PictureCLA $destdir
InstPicture PictureClementine $destdir
InstPicture PictureLAC_LM $destdir
InstPicture PictureLopam $destdir
InstPicture PictureProbes $destdir

