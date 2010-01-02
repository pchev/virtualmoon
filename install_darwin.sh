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
install -v -m 644 Installer/Mac/vmapro/licence.txt $destdir/
install -v -m 644 Installer/Mac/vmapro/readme.txt $destdir/

install -m 755 -d $destdir/data
install -m 755 -d $destdir/data/jpleph
install -m 755 -d $destdir/language
install -m 755 -d "$destdir/My Images"
install -m 755 -d $destdir/Database
install -m 755 -d $destdir/doc
install -m 755 -d $destdir/Encyclopedia
install -m 755 -d $destdir/Textures
install -m 755 -d $destdir/Textures/Airbrush
install -m 755 -d $destdir/Textures/Airbrush/L1
install -m 755 -d $destdir/Textures/Bumpmap
install -m 755 -d $destdir/Textures/Overlay
install -m 755 -d $destdir/Textures/Overlay/caption

install -v -m 644 virtualmoon/language/maplun.en.po $destdir/language/
install -v -m 644 virtualmoon/language/maplun.fr.po $destdir/language/
install -v -m 644 datlun/language/datlun.en.po $destdir/language/
install -v -m 644 datlun/language/datlun.fr.po $destdir/language/
install -v -m 644 datlun/language/vmadatabase.en.po $destdir/language/
install -v -m 644 datlun/language/vmadatabase.fr.po $destdir/language/
install -v -m 644 photlun/language/photlun.en.po $destdir/language/
install -v -m 644 photlun/language/photlun.fr.po $destdir/language/
install -v -m 644 data/country.tab $destdir/data/
install -v -m 644 data/retic.cur $destdir/data/
install -v -m 644 data/jpleph/unxp1900.421 $destdir/data/jpleph/
install -v -m 644 textures/Airbrush/L1/* $destdir/Textures/Airbrush/L1/
install -v -m 644 textures/Bumpmap/*.jpg $destdir/Textures/Bumpmap/
install -v -m 644 textures/Bumpmap/kaguya.txt $destdir/Textures/Bumpmap/
install -v -m 644 textures/Overlay/*.jpg $destdir/Textures/Overlay/
install -v -m 644 textures/Overlay/caption/*.jpg $destdir/Textures/Overlay/caption/
install -v -m 644 Database/* $destdir/Database/
install -v -m 644 doc/* $destdir/doc/
install -v -m 644 Encyclopedia/* $destdir/Encyclopedia/
