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
rm -rfv $destdir/Textures
rm -rfv $destdir/Textures
rm -rfv $destdir/Apollo
rm -rfv $destdir/ApolloMapping
rm -rfv $destdir/CLA
rm -rfv $destdir/Clementine
rm -rfv $destdir/data
rm -rfv $destdir/Database
rm -rfv $destdir/doc
rm -rfv $destdir/Encyclopedia
rm -rfv $destdir/LAC_LM
rm -rfv $destdir/language
rm -rfv $destdir/Lopam
rm -rfv $destdir/My\ Images
rm -rfv $destdir/Probes
rmdir  $destdir