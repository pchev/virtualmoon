#!/bin/bash 

# script to build virtualmoon on a Mac OS X system

version=6.0

basedir=/Volumes/Data/tmp/vmadvd  # Be sure this is set to a non existent directory, it is removed after the run!
builddir=$basedir/Virtual_Moon_Atlas

if [[ -n $1 ]]; then
  configopt="fpc=$1"
fi
if [[ -n $2 ]]; then
  configopt=$configopt" lazarus=$2"
fi

wd=`pwd`
mkdir $wd/$outdir

./configure $configopt prefix=$builddir target=i386-darwin
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_data3
  if [[ $? -ne 0 ]]; then exit 1;fi
  cp Installer/Mac/vmapro/readme_120m.txt $basedir/readme.txt
  cp Installer/Mac/vmapro/licence.txt $basedir/
  # pkg
  mkdir $wd/CD_Mac
  cp Installer/Mac/Textures_120m.packproj $basedir
  cd $basedir
  freeze -v Textures_120m.packproj
  rm -rf $builddir
  rm $basedir/readme.txt
  rm $basedir/licence.txt
  rm $basedir/Textures_120m.packproj
