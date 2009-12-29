#!/bin/bash

destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/virtualmoon
fi

echo Uninstall virtualmoon from $destdir

rm -fv $destdir/bin/atlun
rm -fv $destdir/bin/datlun
rm -fv $destdir/bin/photlun
rm -fv $destdir/lib/libplan404.so
rm -fv $destdir/share/applications/virtualmoon.desktop
rm -fv $destdir/share/doc/virtualmoon/changelog
rm -fv $destdir/share/doc/virtualmoon/copyright
rm -fv $destdir/share/pixmaps/virtualmoon.xpm
rmdir -v $destdir/share/doc/virtualmoon
rm -fv $destdir/share/virtualmoon/Textures/Overlay/caption/*.jpg
rm -fv $destdir/share/virtualmoon/Textures/Overlay/*.jpg
rm -fv $destdir/share/virtualmoon/Textures/Bumpmap/*
rm -fv $destdir/share/virtualmoon/Textures/Airbrush/L1/*.jpg
rm -fv $destdir/share/virtualmoon/Encyclopedia/*
rm -fv $destdir/share/virtualmoon/doc/*
rm -fv $destdir/share/virtualmoon/Database/*
rm -fv $destdir/share/virtualmoon/language/*.po
rm -fv $destdir/share/virtualmoon/data/jpleph/*
rm -fv $destdir/share/virtualmoon/data/*

rmdir -v $destdir/share/virtualmoon/Textures/Overlay/caption
rmdir -v $destdir/share/virtualmoon/Textures/Overlay
rmdir -v $destdir/share/virtualmoon/Textures/Bumpmap
rmdir -v $destdir/share/virtualmoon/Textures/Airbrush/L1
rmdir -v $destdir/share/virtualmoon/Textures/Airbrush
rmdir -v $destdir/share/virtualmoon/Encyclopedia
rmdir -v $destdir/share/virtualmoon/doc
rmdir -v $destdir/share/virtualmoon/Database
rmdir -v $destdir/share/virtualmoon/language
rmdir -v $destdir/share/virtualmoon/data/jpleph
rmdir -v $destdir/share/virtualmoon/data
rmdir -v $destdir/share/virtualmoon/Textures
rmdir -v "$destdir/share/virtualmoon/My Images"
rmdir -v $destdir/share/virtualmoon
