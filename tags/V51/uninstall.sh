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
rm -rfv $destdir/share/virtualmoon/Textures
rm -fv $destdir/share/virtualmoon/Encyclopedia/*
rm -fv $destdir/share/virtualmoon/doc/*
rm -fv $destdir/share/virtualmoon/Database/*
rm -fv $destdir/share/virtualmoon/language/*.po
rm -fv $destdir/share/virtualmoon/data/jpleph/*
rm -fv $destdir/share/virtualmoon/data/*
rm -rfv $destdir/share/virtualmoon
