#!/bin/bash 

# script to build virtualmoon on a 64bit Linux system

version=5.1

builddir=/tmp/virtualmoon  # Be sure this is set to a non existent directory, it is removed after the run!
innosetup="C:\Program Files\Inno Setup 5\ISCC.exe"  # Install under Wine from http://www.jrsoftware.org/isinfo.php
wine_build="Z:\tmp\virtualmoon" # Change to match builddir, Z: is defined in ~/.wine/dosdevices

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
  rm virtualmoon*.tgz
  rm virtualmoon*.deb
  rm virtualmoon*.rpm
  rm virtualmoon*.zip
  rm virtualmoon*.exe
  rm bin-*.zip
  rm bin-*.bz2
  rm -rf $builddir

# make Linux i386 version
  ./configure $configopt prefix=$builddir target=i386-linux,x86_64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=i386 OS_TARGET=linux clean
  make CPU_TARGET=i386 OS_TARGET=linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  if [[ $upd ]]; then
    make install_update
  else 
    make install
  fi 
  if [[ $? -ne 0 ]]; then exit 1;fi
#  make install_data
#  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  tar cvzf virtualmoon$updname-$version-$currentrev-linux_i386.tgz --owner=root --group=root *
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon*.tgz $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  if [[ ! $upd ]]; then
  # deb
  cd $wd
  rsync -a --exclude=.svn Installer/Linux/debian $builddir
  cd $builddir
  mv bin debian/virtualmoon/usr/
  mv lib debian/virtualmoon/usr/
  mv share debian/virtualmoon/usr/
  cd debian
  sed -i "/Version:/ s/5/$version-$currentrev/" virtualmoon/DEBIAN/control
  fakeroot dpkg-deb --build virtualmoon .
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon*.deb $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # rpm
  cd $wd
  rsync -a --exclude=.svn Installer/Linux/rpm $builddir
  cd $builddir
  mv debian/virtualmoon/usr/* rpm/virtualmoon/usr/
  cd rpm
  sed -i "/Version:/ s/5/$version/"  SPECS/virtualmoon.spec
  sed -i "/Release:/ s/1/$currentrev/" SPECS/virtualmoon.spec
  setarch i386 fakeroot rpmbuild  --buildroot "$builddir/rpm/virtualmoon" --define "_topdir $builddir/rpm/" -bb SPECS/virtualmoon.spec
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv RPMS/i386/virtualmoon*.rpm $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  fi # if [[ ! $upd ]]
  #debug
  cd $wd
  mkdir $builddir/debug
  cp virtualmoon/atlun $builddir/debug/
  cp photlun/photlun $builddir/debug/
  cp datlun/datlun $builddir/debug/
  cd $builddir/debug/
  tar cvzf virtualmoon$updname-bin-linux_i386-debug-$currentrev.tgz --owner=root --group=root *
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon$updname-bin-*.tgz $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $builddir

# make Linux x86_64 version
  ./configure $configopt prefix=$builddir target=x86_64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=x86_64 OS_TARGET=linux clean
  make CPU_TARGET=x86_64 OS_TARGET=linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  if [[ $upd ]]; then
    make install_update
  else 
    make install
  fi 
  if [[ $? -ne 0 ]]; then exit 1;fi
#  make install_data
#  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  tar cvzf virtualmoon$updname-$version-$currentrev-linux_x86_64.tgz --owner=root --group=root *
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon*.tgz $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  if [[ ! $upd ]]; then
  # deb
  cd $wd
  rsync -a --exclude=.svn Installer/Linux/debian $builddir
  cd $builddir
  mv bin debian/virtualmoon64/usr/
  mv lib debian/virtualmoon64/usr/
  mv share debian/virtualmoon64/usr/
  cd debian
  sed -i "/Version:/ s/5/$version-$currentrev/" virtualmoon64/DEBIAN/control
  fakeroot dpkg-deb --build virtualmoon64 .
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon*.deb $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # rpm
  cd $wd
  rsync -a --exclude=.svn Installer/Linux/rpm $builddir
  cd $builddir
  mv debian/virtualmoon64/usr/* rpm/virtualmoon/usr/
  # Redhat 64bits lib is lib64 
  mv rpm/virtualmoon/usr/lib rpm/virtualmoon/usr/lib64
  cd rpm
  sed -i "/Version:/ s/5/$version/"  SPECS/virtualmoon64.spec
  sed -i "/Release:/ s/1/$currentrev/" SPECS/virtualmoon64.spec
# rpm 4.7
  fakeroot rpmbuild  --buildroot "$builddir/rpm/virtualmoon" --define "_topdir $builddir/rpm/" -bb SPECS/virtualmoon64.spec
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv RPMS/x86_64/virtualmoon*.rpm $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  fi # if [[ ! $upd ]]
  #debug
  cd $wd
  mkdir $builddir/debug
  cp virtualmoon/atlun $builddir/debug/
  cp photlun/photlun $builddir/debug/
  cp datlun/datlun $builddir/debug/
  cd $builddir/debug/
  tar cvzf virtualmoon$updname-bin-linux_x86_64-debug-$currentrev.tgz --owner=root --group=root *
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon$updname-bin-*.tgz $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $builddir

# make Windows i386 version
  cd $wd/data
  ./mkzoneinfo.sh
  cd $wd
  rsync -a --exclude=.svn Installer/Windows/* $builddir
  ./configure $configopt prefix=$builddir/vmapro/Data target=i386-win32,x86_64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make OS_TARGET=win32 CPU_TARGET=i386 clean
  make OS_TARGET=win32 CPU_TARGET=i386
  if [[ $? -ne 0 ]]; then exit 1;fi
  if [[ $upd ]]; then
    make install_win_update
  else 
    make install_win
  fi 
  if [[ $? -ne 0 ]]; then exit 1;fi
#  make install_win_data
#  if [[ $? -ne 0 ]]; then exit 1;fi
  # zip
  cd $builddir/vmapro/Data
  zip -r  virtualmoon$updname-$version-$currentrev-windows.zip *
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon*.zip $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # exe
  cd $builddir
  sed -i "/AppVerName/ s/V5/V$version/" vmapro.iss
  sed -i "/OutputBaseFilename/ s/-windows/$updname-$version-$currentrev-windows/" vmapro.iss
  wine "$innosetup" "$wine_build\vmapro.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/virtualmoon*.exe $wd
  #debug
  cd $wd
  mkdir $builddir/debug
  cp virtualmoon/atlun.exe $builddir/debug/
  cp photlun/photlun.exe $builddir/debug/
  cp datlun/datlun.exe $builddir/debug/
  cd $builddir/debug/
  zip virtualmoon$updname-bin-windows_i386-debug-$currentrev.zip *
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon$updname-bin-*.zip $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  cd $wd
  rm -rf $builddir

# make Windows x86_64 version
  rsync -a --exclude=.svn Installer/Windows/* $builddir
  ./configure $configopt prefix=$builddir/vmapro/Data target=x86_64-win64,x86_64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make OS_TARGET=win64 CPU_TARGET=x86_64 clean
  make OS_TARGET=win64 CPU_TARGET=x86_64
  if [[ $? -ne 0 ]]; then exit 1;fi
   if [[ $upd ]]; then
    make install_win64_update
  else 
    make install_win64
  fi 
  if [[ $? -ne 0 ]]; then exit 1;fi
#  make install_win_data
#  if [[ $? -ne 0 ]]; then exit 1;fi
  # zip
  cd $builddir/vmapro/Data
  zip -r  virtualmoon$updname-$version-$currentrev-windows-x64.zip *
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon*.zip $wd
  if [[ $? -ne 0 ]]; then exit 1;fi
  # exe
  cd $builddir
  sed -i "/AppVerName/ s/V5/V$version/" vmapro_64.iss
  sed -i "/OutputBaseFilename/ s/-windows-x64/$updname-$version-$currentrev-windows-x64/" vmapro_64.iss
  wine "$innosetup" "$wine_build\vmapro_64.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/virtualmoon*.exe $wd
  #debug
  cd $wd
  mkdir $builddir/debug
  cp virtualmoon/atlun.exe $builddir/debug/
  cp photlun/photlun.exe $builddir/debug/
  cp datlun/datlun.exe $builddir/debug/
  cd $builddir/debug/
  zip virtualmoon$updname-bin-windows-x64-debug-$currentrev.zip *
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon$updname-bin-*.zip $wd
  if [[ $? -ne 0 ]]; then exit 1;fi

  cd $wd
  rm -rf $builddir

# store revision 
  echo $currentrev > last.build
else
  echo Already build at revision $currentrev
  exit 4
fi

