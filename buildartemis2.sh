#!/bin/bash

# script to build virtualmoon on a Linux system

Syntaxe="Syntaxe: buildartemis2.sh freepascal_path lazarus_path"

version=9.0

builddir=/tmp/virtualmoon  # Be sure this is set to a non existent directory, it is removed after the run!
export WINEPREFIX=~/.wineinno6
innosetup="C:\Program Files (x86)\Inno Setup 6\ISCC.exe"  # Install under Wine from http://www.jrsoftware.org/isinfo.php
wine_build="Z:\tmp\virtualmoon" # Change to match builddir, Z: is defined in ~/.wine/dosdevices

# not enough space on /
builddir=/home/pch/tmp/virtualmoon
wine_build="Z:\home\pch\tmp\virtualmoon"

# optionaly build the RPM
unset buildrpm

outdir="BUILD_PICTURE"
extratarget=",x86_64-linux"

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

# make Linux Data for both architectures
function datapkg {
  pkg=$1
  cd $builddir
  tar cvzf virtualmoon-$pkg-$version-linux_all.tgz --owner=root --group=root *
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon*.tgz $wd/$outdir/
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn Installer/Linux/debian $builddir
  cd $builddir
  mkdir debian/virtualmoon-$pkg/usr/
  mv share debian/virtualmoon-$pkg/usr/
  cd debian
  sed -i "/Version:/ s/5/$version/" virtualmoon-$pkg/DEBIAN/control
  fakeroot dpkg-deb --build virtualmoon-$pkg .
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon*.deb $wd/$outdir/
  if [[ $? -ne 0 ]]; then exit 1;fi
  if [[ $buildrpm ]]; then
    # rpm
    cd $wd
    rsync -a --exclude=.svn Installer/Linux/rpm $builddir
    cd $builddir
    mkdir -p rpm/virtualmoon-$pkg/usr/
    mv debian/virtualmoon-$pkg/usr/* rpm/virtualmoon-$pkg/usr/
    cd rpm
    sed -i "/Version:/ s/5/$version/"  SPECS/virtualmoon-$pkg.spec
    sed -i "/Release:/ s/1/1/" SPECS/virtualmoon-$pkg.spec
    fakeroot rpmbuild  --buildroot "$builddir/rpm/virtualmoon-"$pkg --define "_topdir $builddir/rpm/" -bb SPECS/virtualmoon-$pkg.spec
    if [[ $? -ne 0 ]]; then exit 1;fi
    mv RPMS/noarch/virtualmoon*.rpm $wd/$outdir/
    if [[ $? -ne 0 ]]; then exit 1;fi
  fi  
  cd $wd
  rm -rf $builddir
}


# make linux
  ./configure $configopt prefix=$builddir target=x86_64-linux
  make install_artemis2
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg artemis2
  cd $wd
  rm -rf $builddir

# make Windows
  cd $wd
  rsync -a --exclude=.svn Installer/Windows/* $builddir
  ./configure $configopt prefix=$builddir/vmapro/Data target=i386-win32$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_win_artemis2
  if [[ $? -ne 0 ]]; then exit 1;fi
  # exe
  cd $builddir
  sed -i "/AppVerName/ s/V5/V$version/" vmaartemis2.iss
  sed -i "/OutputBaseFilename/ s/-picture/-picture-$version/" vmaartemis2.iss
  wine "$innosetup" "$wine_build\vmaartemis2.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/virtualmoon*.exe $wd/$outdir/
  cd $wd
  rm -rf $builddir
