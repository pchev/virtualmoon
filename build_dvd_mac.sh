#!/bin/bash 

# script to build virtualmoon on a Mac OS X system

version=6.1

unset make_darwin_i386
make_darwin_i386=1
unset make_darwin_ppc
#make_darwin_ppc=1

basedir=/Volumes/Data/tmp/vmadvd  # Be sure this is set to a non existent directory, it is removed after the run!
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
cdrom=1;
updname=_cdrom
unset outdir;

wd=`pwd`

mkdir -p $builddir

# delete old files
  deldir=$basedir/DVD;
  rm $deldir/virtualmoon*.dmg
  mkdir $basedir/DVD

# make i386 Mac version
  ./configure $configopt prefix=$builddir target=i386-darwin
  if [[ $? -ne 0 ]]; then exit 1;fi
  make clean
  make 
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_data
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_data2
  if [[ $? -ne 0 ]]; then exit 1;fi
  # pkg
    cp Installer/Mac/DVDvmapro.packproj $basedir
    cp Installer/Mac/vmapro/readme_dvd.txt $basedir/build/readme.txt
    cd $basedir
    freeze -v DVDvmapro.packproj
    if [[ $? -ne 0 ]]; then exit 1;fi
    rm *.cdr *.iso
    hdiutil create -anyowners -volname virtualmoon-$version-macosx-i386 -format UDTO -srcfolder ./build virtualmoon-$version-macosx.cdr
    hdiutil makehybrid -o virtualmoon-$version-macosx.iso virtualmoon-$version-macosx.cdr -iso -joliet   
    if [[ $? -ne 0 ]]; then exit 1;fi
  cd $wd

