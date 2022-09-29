#!/bin/bash

# script to build virtualmoon on a Linux system

Syntaxe="Syntaxe: buildpkg.sh freepascal_path lazarus_path [linux|linuxdata|linuxhires|linuxpicture|linuxtranslation|win|windata|winhires|winpicture|wintranslation]"

version=8.0

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

unset make_linux32
unset make_linux64
unset make_linux_data
unset make_linux_hires
unset make_linux_picture
unset make_linux_translation
unset make_win32
unset make_win32_data
unset make_win32_hires
unset make_win32_picture
unset make_win32_translation
unset outdir
if [[ $buildname == linux ]]; then 
  if [[ $arch == i686 ]]; then 
     make_linux32=1
     outdir="BUILD_LINUX32"
  fi
  if [[ $arch == x86_64 ]]; then 
     make_linux64=1
     extratarget=",x86_64-linux"
     outdir="BUILD_LINUX64"
  fi
fi

if [[ $buildname == linuxdata ]]; then 
  make_linux_data=1
  outdir="BUILD_LINUXDATA"
fi  
if [[ $buildname == linuxhires ]]; then 
  make_linux_hires=1
  outdir="BUILD_LINUXDATA"
fi  
if [[ $buildname == linuxpicture ]]; then 
  make_linux_picture=1
  outdir="BUILD_LINUXPICTURE"
fi  
if [[ $buildname == linuxtranslation ]]; then 
  make_linux_translation=1
  outdir="BUILD_LINUXTRANSLATION"
fi  

if [[ $buildname == win ]]; then 
  make_win32=1
  extratarget=",x86_64-linux"
  outdir="BUILD_WIN"
fi
if [[ $buildname == windata ]]; then 
  make_win32_data=1
  extratarget=",x86_64-linux"
  outdir="BUILD_WINDATA"
fi  
if [[ $buildname == winhires ]]; then 
  make_win32_hires=1
  extratarget=",x86_64-linux"
  outdir="BUILD_WINDATA"
fi  
if [[ $buildname == winpicture ]]; then 
  make_win32_picture=1
  extratarget=",x86_64-linux"
  outdir="BUILD_WINPICTURE"
fi  
if [[ $buildname == wintranslation ]]; then 
  make_win32_translation=1
  extratarget=",x86_64-linux"
  outdir="BUILD_WINTRANSLATION"
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

# make Linux i386 version
if [[ $make_linux32 ]]; then 
  ./configure $configopt prefix=$builddir target=i386-linux$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=i386 OS_TARGET=linux clean
  make CPU_TARGET=i386 OS_TARGET=linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install
  if [[ $? -ne 0 ]]; then exit 1;fi
  # tar
  cd $builddir
  tar cvzf virtualmoon-$version-linux_i386.tgz --owner=root --group=root *
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon*.tgz $wd/$outdir/
  if [[ $? -ne 0 ]]; then exit 1;fi
  # deb
  cd $wd
  rsync -a --exclude=.svn Installer/Linux/debian $builddir
  cd $builddir
  mkdir debian/virtualmoon/usr/
  mv bin debian/virtualmoon/usr/
  mv share debian/virtualmoon/usr/
  cd debian
  sed -i "/Version:/ s/5/$version/" virtualmoon/DEBIAN/control
  fakeroot dpkg-deb --build virtualmoon .
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv virtualmoon*.deb $wd/$outdir/
  if [[ $? -ne 0 ]]; then exit 1;fi
  if [[ $buildrpm ]]; then
    # rpm
    cd $wd
    rsync -a --exclude=.svn Installer/Linux/rpm $builddir
    cd $builddir
    mkdir -p rpm/virtualmoon/usr/
    mv debian/virtualmoon/usr/* rpm/virtualmoon/usr/
    cd rpm
    sed -i "/Version:/ s/5/$version/"  SPECS/virtualmoon.spec
    sed -i "/Release:/ s/1/1/" SPECS/virtualmoon.spec
    fakeroot rpmbuild  --buildroot "$builddir/rpm/virtualmoon" --define "_topdir $builddir/rpm/" -bb SPECS/virtualmoon.spec
    if [[ $? -ne 0 ]]; then exit 1;fi
    mv RPMS/i386/virtualmoon*.rpm $wd/$outdir/
  if [[ $? -ne 0 ]]; then exit 1;fi
  fi
  cd $wd
  rm -rf $builddir
fi

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
if [[ $make_linux_data ]]; then 
  ./configure $configopt prefix=$builddir target=x86_64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_data
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg basedata 
  make install_data2
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg data
  cd $wd
  rm -rf $builddir
fi

if [[ $make_linux_hires ]]; then 
  ./configure $configopt prefix=$builddir target=x86_64-linux
  make install_data3
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg hires
  make install_data4-1
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg vhres-change
  make install_data4-2
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg vhres-lopam
  make install_data4-3
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg vhres-lrowac
  make install_data4-4
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg vhres-lolakaguya
  make install_data4-5
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg vhres-waclowsun
  make install_data5
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg hires-lac
  cd $wd
  rm -rf $builddir
fi

if [[ $make_linux_picture ]]; then 
  ./configure $configopt prefix=$builddir target=x86_64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_picture
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg picture 
  cd $wd
  rm -rf $builddir
fi

if [[ $make_linux_translation ]]; then 
  ./configure $configopt prefix=$builddir target=x86_64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_translation
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg translation 
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
  make install_win_data
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


# make Windows data
if [[ $make_win32_data ]]; then 
  cd $wd
  rsync -a --exclude=.svn Installer/Windows/* $builddir
  ./configure $configopt prefix=$builddir/vmapro/Data target=i386-win32$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_win_data2
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/vmapro/Data $builddir/vmapro/Data2
  # exe
  cd $builddir
  wine "$innosetup" "$wine_build\vmadata2.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/virtualmoon*.exe $wd/$outdir/
  cd $wd
  rm -rf $builddir
fi

if [[ $make_win32_hires ]]; then 
  cd $wd
  rsync -a --exclude=.svn Installer/Windows/* $builddir
  ./configure $configopt prefix=$builddir/vmapro/Data target=i386-win32$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_win_data3
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/vmapro/Data $builddir/vmapro/Data3
  make install_win_data4-1
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/vmapro/Data $builddir/vmapro/Data4-1
  make install_win_data4-2
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/vmapro/Data $builddir/vmapro/Data4-2
  make install_win_data4-3
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/vmapro/Data $builddir/vmapro/Data4-3
  make install_win_data4-4
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/vmapro/Data $builddir/vmapro/Data4-4
  make install_win_data4-5
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/vmapro/Data $builddir/vmapro/Data4-5
  make install_win_data5
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/vmapro/Data $builddir/vmapro/Data5
  # exe
  cd $builddir
  wine "$innosetup" "$wine_build\vmadata3.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  wine "$innosetup" "$wine_build\vmadata4-1.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  wine "$innosetup" "$wine_build\vmadata4-2.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  wine "$innosetup" "$wine_build\vmadata4-3.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  wine "$innosetup" "$wine_build\vmadata4-4.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  wine "$innosetup" "$wine_build\vmadata4-5.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  wine "$innosetup" "$wine_build\vmadata5.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/virtualmoon*.exe $wd/$outdir/
  cd $wd
  rm -rf $builddir
fi

# make Windows pictures
if [[ $make_win32_picture ]]; then 
  cd $wd
  rsync -a --exclude=.svn Installer/Windows/* $builddir
  ./configure $configopt prefix=$builddir/vmapro/Data target=i386-win32$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_win_picture
  if [[ $? -ne 0 ]]; then exit 1;fi
  # exe
  cd $builddir
  wine "$innosetup" "$wine_build\vmapicture.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/virtualmoon*.exe $wd/$outdir/
  cd $wd
  rm -rf $builddir
fi

# make Windows translation
if [[ $make_win32_translation ]]; then 
  cd $wd
  rsync -a --exclude=.svn Installer/Windows/* $builddir
  ./configure $configopt prefix=$builddir/vmapro/Data target=i386-win32$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_win_translation
  if [[ $? -ne 0 ]]; then exit 1;fi
  # exe
  cd $builddir
  wine "$innosetup" "$wine_build\vmatranslation.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/virtualmoon*.exe $wd/$outdir/
  cd $wd
  rm -rf $builddir
fi

