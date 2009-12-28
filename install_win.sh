#!/bin/bash

destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/virtualmoon
fi

echo Install virtualmoon to $destdir

install -m 755 -d $destdir
fpc-i386-win32-strip -v -o $destdir/atlun.exe virtualmoon/atlun.exe 
fpc-i386-win32-strip -v -o $destdir/datlun.exe datlun/datlun.exe
fpc-i386-win32-strip -v -o $destdir/photlun.exe photlun/photlun.exe
install -v -m 644 virtualmoon/library/plan404/libplan404.dll  $destdir/
install -v -m 644 virtualmoon/library/sqlite/sqlite3.dll  $destdir/
