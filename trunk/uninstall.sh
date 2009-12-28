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
