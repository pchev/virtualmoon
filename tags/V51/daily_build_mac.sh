#!/bin/bash 

# script to build virtualmoon on a Mac OS X system

version=5.1

basedir=/tmp/virtualmoon  # Be sure this is set to a non existent directory, it is removed after the run!
builddir=$basedir/Virtual_Moon_Atlas

if [[ -n $1 ]]; then
  configopt="fpc=$1"
fi
if [[ -n $2 ]]; then
  configopt=$configopt" lazarus=$2"
fi
unset upd
if [[ -n $3 ]]; then
  if [[ $3 == update ]]; then upd=1; fi
fi

if [[ $upd ]]; then
  echo make update
  updname=_update
else 
  echo make full
  updname=
fi

wd=`pwd`

# update to last revision
#svn up --force --non-interactive --accept theirs-full    # svn 1.5 only
svn -R revert .
svn up --non-interactive

# check if new revision since last run
read lastrev <last.build
lang=LANG
LANG=C
currentrev=`svn info . | grep Revision: | sed 's/Revision: //'`
LANG=$lang
echo $lastrev ' - ' $currentrev
if [[ $lastrev -ne $currentrev ]]; then

# delete old files
  rm virtualmoon*.dmg
  rm virtualmoon-bin*.tgz
  rm virtualmoon_update-bin*.tgz
  rm -rf $basedir

# make i386 Mac version
  ./configure $configopt prefix=$builddir target=i386-darwin
  if [[ $? -ne 0 ]]; then exit 1;fi
  make clean
  make 
  if [[ $? -ne 0 ]]; then exit 1;fi
  if [[ $upd ]]; then
    make install_update
  else 
    make install
  fi 
  if [[ $? -ne 0 ]]; then exit 1;fi
  # pkg
  cp Installer/Mac/vmaupdate.packproj $basedir
  cd $basedir
  freeze -v vmaupdate.packproj
  if [[ $? -ne 0 ]]; then exit 1;fi
  hdiutil create -anyowners -volname virtualmoon$updname-$version-$currentrev-macosx-i386 -imagekey zlib-level=9 -format UDZO -srcfolder ./build virtualmoon$updname-$version-$currentrev-macosx-i386.dmg
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon*.dmg $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
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
  mv virtualmoon$updname-bin-*.tgz $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $basedir

# store revision 
  echo $currentrev > last.build
else
  echo Already build at revision $currentrev
  exit 4
fi

