#!/bin/bash

# install the translation
# must be installed after the software part

function InstData {
  pkg=$1.tgz
  ddir=$2
  pkgz=BaseData/$pkg
  if [ ! -e $pkgz ]; then
     wget http://sourceforge.net/projects/virtualmoon/files/OldFiles/6-Source_Data/$pkg/download -O $pkgz
  fi
  tar xvzf $pkgz -C $ddir
}


destdir=$1

if [ -z "$destdir" ]; then
   export destdir=/tmp/virtualmoon
fi

echo Install virtualmoon translation to $destdir

install -m 755 -d $destdir
install -m 755 -d $destdir/share
install -m 755 -d $destdir/share/virtualmoon

# translation
install -m 755 -d $destdir/share/virtualmoon/language
lg='ca cs de el es hu it lt nl sk'
for lang in $lg; do
  install -v -m 644 virtualmoon/language/maplun.$lang.po $destdir/share/virtualmoon/language/
  install -v -m 644 datlun/language/datlun.$lang.po $destdir/share/virtualmoon/language/
  install -v -m 644 datlun/language/vmadatabase.$lang.po $destdir/share/virtualmoon/language/
  install -v -m 644 photlun/language/photlun.$lang.po $destdir/share/virtualmoon/language/
  install -v -m 644 weblun/language/weblun.$lang.po $destdir/share/virtualmoon/language/
  install -v -m 644 cclun/language/cclun.$lang.po $destdir/share/virtualmoon/language/
  install -v -m 644 notelun/language/notelun.$lang.po $destdir/share/virtualmoon/language/
  install -v -m 644 calclun/language/calclun.$lang.po $destdir/share/virtualmoon/language/
done

install -v -m 644 Database/translation/01_IAU_NAMED_CA.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/translation/01_IAU_NAMED_DE.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/translation/01_IAU_NAMED_ES.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/translation/01_IAU_NAMED_IT.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/translation/02_IAU_SATELLITE_CA.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/translation/02_IAU_SATELLITE_ES.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/translation/02_IAU_SATELLITE_IT.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/translation/glossary_uCA.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/translation/glossary_uDE.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/translation/glossary_uES.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/translation/glossary_uIT.csv $destdir/share/virtualmoon/Database/
install -v -m 644 Database/translation/glossary_uSK.csv $destdir/share/virtualmoon/Database/

InstData Translation_Doc $destdir
