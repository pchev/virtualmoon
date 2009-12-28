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
