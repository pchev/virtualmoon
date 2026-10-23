#!/bin/bash

# script to build virtualmoon for Linux and Windows on a Linux system
# for Macos build see tools/macos/

Syntaxe="Syntaxe: buildpkg.sh freepascal_path lazarus_path [linux|win|]"

version=9.2

builddir=/tmp/virtualmoon  # Be sure this is set to a non existent directory, it is removed after the run!
export WINEPREFIX=~/.wine
innosetup="C:\Program Files\Inno Setup 6\ISCC.exe"  # Install under Wine from http://www.jrsoftware.org/isinfo.php
wine_build="Z:\tmp\virtualmoon" # Change to match builddir, Z: is defined in ~/.wine/dosdevices

# not enough space on /
builddir=/home/pch/tmp/virtualmoon
wine_build="Z:\home\pch\tmp\virtualmoon"

# optionaly build the RPM
unset buildrpm

arch=$(arch)
unset extratarget

# adjuste here the target you want to crossbuild
# You MUST crosscompile Freepascal and Lazarus for this targets! 

if [[ -z $3 ]]; then
  echo $Syntaxe
  exit 1;
fi

buildname=$3
echo Make $buildname

unset make_linux64
unset make_win32
unset outdir
if [[ $buildname == linux ]]; then 
  if [[ $arch == x86_64 ]]; then 
     make_linux64=1
     extratarget=",x86_64-linux"
     outdir="BUILD_LINUX64"
  fi
fi

if [[ $buildname == win ]]; then 
  make_win32=1
  extratarget=",x86_64-linux"
  outdir="BUILD_WIN"
fi

if [[ -z $outdir ]]; then
  echo $Syntaxe
  exit 1;
fi

if [[ -n $1 ]]; then
  configopt="fpc=$1"
fi
if [[ -n $2 ]]; then
  configopt=$configopt" lazarus=$2"
fi

wd=`pwd`
mkdir $outdir
rm $outdir/virtualmoon*
rm -rf $builddir

currentrev=$(git rev-list --count --first-parent HEAD)

# make Linux x86_64 version
if [[ $make_linux64 ]]; then 
  ./configure $configopt prefix=$builddir target=x86_64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=x86_64 OS_TARGET=linux clean
  make CPU_TARGET=x86_64 OS_TARGET=linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install
  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  tar cvzf virtualmoon-$version-linux_x86_64.tgz --owner=root --group=root *
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon*.tgz $wd/$outdir/
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn Installer/Linux/debian $builddir
  cd $builddir
  mkdir debian/virtualmoon64/usr/
  mv bin debian/virtualmoon64/usr/
  mv lib debian/virtualmoon64/usr/
  mv share debian/virtualmoon64/usr/
  cd debian
  sed -i "/Version:/ s/5/$version/" virtualmoon64/DEBIAN/control
  fakeroot dpkg-deb --build virtualmoon64 .
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon*.deb $wd/$outdir/
  if [[ $? -ne 0 ]]; then exit 1;fi
  if [[ $buildrpm ]]; then
    # rpm
    cd $wd
    rsync -a --exclude=.svn Installer/Linux/rpm $builddir
    cd $builddir
    mkdir -p rpm/virtualmoon/usr/
    mv debian/virtualmoon64/usr/* rpm/virtualmoon/usr/
    cd rpm
    sed -i "/Version:/ s/5/$version/"  SPECS/virtualmoon64.spec
    sed -i "/Release:/ s/1/1/" SPECS/virtualmoon64.spec
    # rpm 4.7
    fakeroot rpmbuild  --buildroot "$builddir/rpm/virtualmoon" --define "_topdir $builddir/rpm/" -bb SPECS/virtualmoon64.spec
    if [[ $? -ne 0 ]]; then exit 1;fi
    mv RPMS/x86_64/virtualmoon*.rpm $wd/$outdir/
    if [[ $? -ne 0 ]]; then exit 1;fi
  fi  
  cd $wd
  rm -rf $builddir
fi

# make Windows i386 version
if [[ $make_win32 ]]; then 
  cd $wd/data
  ./mkzoneinfo.sh
  cd $wd
  rsync -a --exclude=.svn Installer/Windows/* $builddir
  ./configure $configopt prefix=$builddir/vmapro/Data target=i386-win32$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make OS_TARGET=win32 CPU_TARGET=i386 clean
  make OS_TARGET=win32 CPU_TARGET=i386
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_win
  if [[ $? -ne 0 ]]; then exit 1;fi
  # exe
  cd $builddir
  sed -i "/AppVerName/ s/V5/V$version/" vmapro.iss
  sed -i "/OutputBaseFilename/ s/-windows/-$version-windows/" vmapro.iss
  wine "$innosetup" "$wine_build\vmapro.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/virtualmoon*.exe $wd/$outdir/
  cd $wd
  rm -rf $builddir
fi

