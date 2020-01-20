#!/bin/bash

# script to build virtualmoon on a Linux system

Syntaxe="Syntaxe: buildpkg.sh freepascal_path lazarus_path [linux|linuxdata|win|windata|windvd]"

version=7.0

builddir=/tmp/virtualmoon  # Be sure this is set to a non existent directory, it is removed after the run!
export WINEPREFIX=~/.wineinno6
innosetup="C:\Program Files (x86)\Inno Setup 6\ISCC.exe"  # Install under Wine from http://www.jrsoftware.org/isinfo.php
wine_build="Z:\tmp\virtualmoon" # Change to match builddir, Z: is defined in ~/.wine/dosdevices

# not enough space on /
builddir=/home/pch/tmp/virtualmoon
wine_build="Z:\home\pch\tmp\virtualmoon"


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
unset make_win32
unset make_win32_data
unset make_win32_dvd
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
if [[ $buildname == windvd ]]; then 
  make_win32_dvd=1
  extratarget=",x86_64-linux"
  outdir="BUILD_WINDVD"
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
  cd $wd
  rm -rf $builddir
fi

# make Linux Data for both architectures

if [[ $make_linux_data ]]; then 
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
  mv virtualmoon-data*.deb $wd/$outdir/
  if [[ $? -ne 0 ]]; then exit 1;fi
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
  cd $wd
  rm -rf $builddir
}
  ./configure $configopt prefix=$builddir target=x86_64-linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_data
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg data1 
  make install_data2
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg data2
  make install_data3
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg data3
  make install_data4
  if [[ $? -ne 0 ]]; then exit 1;fi
  datapkg data4
  mkdir $outdir/Linux_CD
  mkdir $outdir/Linux_download
  cp Installer/Linux/licence $outdir/Linux_CD/
  cp Installer/Linux/readme_CD $outdir/Linux_CD/readme
  cp Installer/Linux/install_CD.sh $outdir/Linux_CD/install.sh
  cp Installer/Linux/vmapro_install.sh $outdir/Linux_download/
  cp Installer/Linux/licence $outdir/Linux_download/
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
  # exe
  cd $builddir
  wine "$innosetup" "$wine_build\vmadata1.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  wine "$innosetup" "$wine_build\vmadata2.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  wine "$innosetup" "$wine_build\vmadata3.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  wine "$innosetup" "$wine_build\vmadata4.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  wine "$innosetup" "$wine_build\vmadata5.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  wine "$innosetup" "$wine_build\vmadata6.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/virtualmoon*.exe $wd/$outdir/
  cd $wd
  rm -rf $builddir
fi

# make Windows i386 DVD version
if [[ $make_win32_dvd ]]; then 
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
  mv $builddir/vmapro/Data $builddir/vmapro/Data1
  make install_win_data2
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/vmapro/Data $builddir/vmapro/Data2
  make install_win_data3
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/vmapro/Data $builddir/vmapro/Data3
  make install_win_data4
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/vmapro/Data $builddir/vmapro/Data4
  # exe
  cd $builddir
  sed -i "/AppVerName/ s/V5/V$version/" vmapro_CD.iss
  wine "$innosetup" "$wine_build\vmapro_CD.iss"
  if [[ $? -ne 0 ]]; then exit 1;fi
  mv $builddir/setup*.exe $outdir/
  mv $builddir/setup*.bin $outdir/
  cp $builddir/Data/licence.txt  $outdir/
  cp $builddir/Data/licence_fr.txt  $outdir/
  cp $builddir/Data/readme.txt  $outdir/
  cp $builddir/Data/lisezmoi.txt  $outdir/
  cp $builddir/Data/autorun.inf  $outdir/
  cp $builddir/Data/vma.ico  $outdir/
  cd $wd 
  mkisofs -R -r -l -J -quiet -Vvirtualmoon -ovirtualmoon-$version-windows.iso $outdir
  cd $wd
  rm -rf $builddir
fi

