#!/bin/bash

destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/virtualmoon
fi

echo Uninstall virtualmoon from $destdir

rm -fv $destdir/atlun.exe
rm -fv $destdir/datlun.exe
rm -fv $destdir/photlun.exe
rm -fv $destdir/libplan404.dll
rm -fv $destdir/sqlite3.dll
rm -fv $destdir/Ascom.tid
rm -fv $destdir/encoder.tid
rm -fv $destdir/Meade.tid
rm -fv $destdir/licence_fr.txt
rm -fv $destdir/licence.txt
rm -fv $destdir/lisezmoi.txt
rm -fv $destdir/readme.txt
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
rmdir $destdir