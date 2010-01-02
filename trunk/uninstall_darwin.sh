#!/bin/bash

destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/virtualmoon
fi

echo Uninstall virtualmoon from $destdir

rm -fv $destdir/atlun.app/Contents/MacOS/atlun
rm -fv $destdir/datlun.app/Contents/MacOS/datlun
rm -fv $destdir/photlun.app/Contents/MacOS/photlun
rm -fv $destdir/libplan404.dylib

rm -rf $destdir/atlun.app 
rm -rf $destdir/datlun.app
rm -rf $destdir/photlun.app

rm -fv $destdir/licence.txt
rm -fv $destdir/readme.txt
rm -fv $destdir/Textures/Overlay/caption/*.jpg
rm -fv $destdir/Textures/Overlay/*.jpg
rm -fv $destdir/Textures/Bumpmap/*
rm -fv $destdir/Textures/Airbrush/L1/*.jpg
rm -fv $destdir/Encyclopedia/*
rm -fv $destdir/doc/*
rm -fv $destdir/Database/*
rm -fv $destdir/language/*.po
rm -fv $destdir/data/jpleph/*
rm -fv $destdir/data/*.cur
rm -fv $destdir/data/*.tab

rmdir  $destdir/Textures/Overlay/caption
rmdir  $destdir/Textures/Overlay
rmdir  $destdir/Textures/Bumpmap
rmdir  $destdir/Textures/Airbrush/L1
rmdir  $destdir/Textures/Airbrush
rmdir  $destdir/Encyclopedia
rmdir  $destdir/doc
rmdir  $destdir/Database
rmdir  $destdir/language
rmdir  $destdir/data/jpleph
rmdir  $destdir/data
rmdir  $destdir/Textures
rmdir  "$destdir/My Images"
rmdir  $destdir