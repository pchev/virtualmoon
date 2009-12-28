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
