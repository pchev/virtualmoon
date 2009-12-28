#!/bin/bash

destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/virtualmoon
fi

echo Install virtualmoon to $destdir

install -m 755 -d $destdir
cp -R -p Installer/Mac/vmapro/atlun.app $destdir/
cp -R -p Installer/Mac/vmapro/datlun.app $destdir/
cp -R -p Installer/Mac/vmapro/photlun.app $destdir/

install -v -m 755 -s virtualmoon/atlun  $destdir/atlun.app/Contents/MacOS/atlun
install -v -m 755 -s datlun/datlun  $destdir/datlun.app/Contents/MacOS/datlun
install -v -m 755 -s photlun/photlun  $destdir/photlun.app/Contents/MacOS/photlun
install -v -m 755 virtualmoon/library/plan404/libplan404.dylib  $destdir/libplan404.dylib

