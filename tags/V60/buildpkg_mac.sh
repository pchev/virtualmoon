#!/bin/bash 

# script to build virtualmoon on a Mac OS X system

version=6.0

unset make_darwin_i386
make_darwin_i386=1
unset make_darwin_ppc
#make_darwin_ppc=1

basedir=/Volumes/Data/tmp/virtualmoon  # Be sure this is set to a non existent directory, it is removed after the run!
builddir=$basedir/Virtual_Moon_Atlas

if [[ -n $1 ]]; then
  configopt="fpc=$1"
fi
if [[ -n $2 ]]; then
  configopt=$configopt" lazarus=$2"
fi

unset upd
unset cdrom
unset pro
unset updname
if [[ -n $3 ]]; then
  if [[ $3 == update ]]; then upd=1; fi
  if [[ $3 == cdrom ]]; then cdrom=1; fi
  if [[ $3 == pro ]]; then pro=1; fi
fi
if [[ $upd ]]; then
  echo make update
  updname=_update
  outdir='UPD';
fi
if [[ $cdrom ]]; then
  echo make cdrom
  updname=_cdrom
  unset outdir;
fi
if [[ $pro ]]; then
  echo make pro
  updname=_pro
  outdir='PRO';
fi
if [[ -z $updname ]]; then
  echo "Syntaxe : daily_build.sh freepascal_path lazarus_path [update|pro|cdrom]"
  exit 1;
fi

wd=`pwd`
mkdir $wd/$outdir

# check if new revision since last run
read lastrev <last.build
lang=LANG
LANG=C
currentrev=`svn info . | grep Revision: | sed 's/Revision: //'`
LANG=$lang
if [[ $upd ]]; then
  echo $lastrev ' - ' $currentrev
  if [[ $lastrev -eq $currentrev ]]; then
    echo Already build at revision $currentrev
    exit 4
  fi
fi

# delete old files
  if [[ -z $outdir ]] ; then 
    deldir="CD_*";
  else
    deldir=$outdir; 
  fi 
  rm $deldir/virtualmoon*.dmg
  rm $deldir/virtualmoon-bin*.tgz
  rm $deldir/virtualmoon_update-bin*.tgz
  rm -rf $basedir

# make i386 Mac version
if [[ $make_darwin_i386 ]]; then
  ./configure $configopt prefix=$builddir target=i386-darwin
  if [[ $? -ne 0 ]]; then exit 1;fi
  make clean
  make 
  make 
  if [[ $? -ne 0 ]]; then exit 1;fi
  if [[ $upd ]]; then
    make install_update
    if [[ $? -ne 0 ]]; then exit 1;fi
  else 
    make install
    if [[ $? -ne 0 ]]; then exit 1;fi
    make install_data
    if [[ $? -ne 0 ]]; then exit 1;fi
    if [[ $cdrom ]]; then
       make install_data2
       if [[ $? -ne 0 ]]; then exit 1;fi
    fi   
  fi 
  # pkg
  if [[ $upd ]]; then
    cp Installer/Mac/vmaupdate.packproj $basedir
    cd $basedir
    freeze -v vmaupdate.packproj
    if [[ $? -ne 0 ]]; then exit 1;fi
    hdiutil create -anyowners -volname virtualmoon$updname-$version-$currentrev-macosx-i386 -imagekey zlib-level=9 -format UDZO -srcfolder ./build virtualmoon$updname-$version-$currentrev-macosx-i386.dmg
    if [[ $? -ne 0 ]]; then exit 1;fi
    mv virtualmoon*.dmg $wd/$outdir/
    if [[ $? -ne 0 ]]; then exit 1;fi
  fi
  if [[ $pro ]]; then
    cp Installer/Mac/vmapro.packproj $basedir
    cd $basedir
    freeze -v vmapro.packproj
    if [[ $? -ne 0 ]]; then exit 1;fi
    hdiutil create -anyowners -volname virtualmoon-$version-macosx-i386 -imagekey zlib-level=9 -format UDZO -srcfolder ./build virtualmoon-$version-macosx-i386.dmg
    if [[ $? -ne 0 ]]; then exit 1;fi
    mv virtualmoon*.dmg $wd/$outdir/
    if [[ $? -ne 0 ]]; then exit 1;fi
  fi
  if [[ $cdrom ]]; then
    mkdir $wd/CD_Mac
    cp Installer/Mac/CDvmapro.packproj $basedir
    cd $basedir
    freeze -v CDvmapro.packproj
    if [[ $? -ne 0 ]]; then exit 1;fi
    cp Virtual_Moon_Atlas/licence.txt vmapro6/
    cp Virtual_Moon_Atlas/readme.txt vmapro6/
    rm *.cdr *.iso
    hdiutil create -anyowners -volname virtualmoon-$version-macosx-i386 -format UDTO -srcfolder ./vmapro6 virtualmoon-$version-macosx.cdr
    hdiutil makehybrid -o virtualmoon-$version-macosx.iso virtualmoon-$version-macosx.cdr -iso -joliet   
    if [[ $? -ne 0 ]]; then exit 1;fi
    mv virtualmoon*.iso $wd/CD_Mac/
    if [[ $? -ne 0 ]]; then exit 1;fi
  fi
  #debug
  cd $wd
  mkdir $basedir/debug
  cp virtualmoon/atlun $basedir/debug/
  cp photlun/photlun $basedir/debug/
  cp datlun/datlun $basedir/debug/
  cd $basedir/debug/
  if [[ $? -ne 0 ]]; then exit 1;fi
  tar cvzf virtualmoon$updname-bin-macosx-i386-debug-$currentrev.tgz *
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon$updname-bin-*.tgz $wd/$outdir/
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $basedir
fi

# make ppc Mac version
if [[ $make_darwin_ppc ]]; then
  echo I not have a ppc system to test so you may have some work to do!
  #./configure $configopt prefix=$builddir target=ppc-darwin
fi

# store revision 
  echo $currentrev > last.build


