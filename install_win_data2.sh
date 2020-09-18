#!/bin/bash

# install complement data for the CDrom version
# must be installed after the software and data part

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

destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/virtualmoon
fi

echo Install virtualmoon data2 to $destdir

install -m 755 -d $destdir

InstData Base_Database2 $destdir

InstTexture TexturesAirbrush_na $destdir
InstTexture TexturesClementine $destdir
InstTexture TexturesClementineNegative $destdir
InstTexture TexturesLopam $destdir
InstTexture TexturesWAC $destdir
InstTexture Texture_LOLA_Kaguya_Shade $destdir
InstTexture TexturesChange $destdir
InstTexture TextureHistorical $destdir
InstTexture TextureOverlay $destdir
InstTexture TextureGeological $destdir

InstPicture PictureApolloMapping $destdir
InstPicture PictureCLA $destdir
InstPicture PictureClementine $destdir
InstPicture PictureLAC_LM $destdir
InstPicture PictureLopam $destdir
InstPicture PictureProbes $destdir
InstPicture PictureSmart1 $destdir
