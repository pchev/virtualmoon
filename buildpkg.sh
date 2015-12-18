#!/bin/bash

# script to build virtualmoon on a Linux system

version=6.1

arch=$(arch)
unset extratarget

# adjuste here the target you want to crossbuild
# You MUST crosscompile Freepascal and Lazarus for this targets! 

unset make_debug
unset make_linux32

if [[ $arch == i386 ]]; then 
   make_linux32=1
fi
unset make_linux64
if [[ $arch == x86_64 ]]; then 
   make_linux64=1
   extratarget=",x86_64-linux"
fi
unset make_linux_data
make_linux_data=1
unset make_win32
make_win32=1

builddir=/tmp/virtualmoon  # Be sure this is set to a non existent directory, it is removed after the run!
innosetup="C:\Program Files\Inno Setup 5\ISCC.exe"  # Install under Wine from http://www.jrsoftware.org/isinfo.php
wine_build="Z:\tmp\virtualmoon" # Change to match builddir, Z: is defined in ~/.wine/dosdevices

builddir=/home/pch/tmp/virtualmoon
wine_build="F:\pch\tmp\virtualmoon"

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
release=RELEASE
mkdir $wd/$release

# check if new revision since last run
read lastrev <last.build
currentrev=$(LC_ALL=C svn info . | grep Revision: | sed 's/Revision: //')
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
  rm $deldir/virtualmoon*.tgz
  rm $deldir/virtualmoon*.deb
  rm $deldir/virtualmoon*.rpm
  rm $deldir/virtualmoon*.zip
  rm $deldir/virtualmoon*.exe
  rm $deldir/bin-*.zip
  rm $deldir/bin-*.bz2
  rm -rf $builddir

# make Linux i386 version
if [[ $make_linux32 ]]; then 
  ./configure $configopt prefix=$builddir target=i386-linux$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make CPU_TARGET=i386 OS_TARGET=linux clean
  make CPU_TARGET=i386 OS_TARGET=linux
  make CPU_TARGET=i386 OS_TARGET=linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  if [[ $upd ]]; then
    make install_update
    if [[ $? -ne 0 ]]; then exit 1;fi
  else 
    make install
    if [[ $? -ne 0 ]]; then exit 1;fi
  fi 
  # tar
  cd $builddir
  if [[ $upd ]]; then
    tar cvzf virtualmoon$updname-$version-$currentrev-linux_i386.tgz --owner=root --group=root *
    if [[ $? -ne 0 ]]; then exit 1;fi
  else
    tar cvzf virtualmoon-$version-linux_i386.tgz --owner=root --group=root *
    if [[ $? -ne 0 ]]; then exit 1;fi
  fi
  if [[ $cdrom ]]; then
    mkdir $wd/CD_Linux
    mv virtualmoon*.tgz $wd/CD_Linux/
    if [[ $? -ne 0 ]]; then exit 1;fi
  else
    mv virtualmoon*.tgz $wd/$outdir/
    if [[ $? -ne 0 ]]; then exit 1;fi
  fi
#  if [[ $pro ]]; then
    # deb
#    cd $wd
#    rsync -a --exclude=.svn Installer/Linux/debian $builddir
#    cd $builddir
#    mv bin debian/virtualmoon/usr/
#    mv share debian/virtualmoon/usr/
#    cd debian
#    sed -i "/Version:/ s/5/$version/" virtualmoon/DEBIAN/control
#    fakeroot dpkg-deb --build virtualmoon .
#    if [[ $? -ne 0 ]]; then exit 1;fi
#    mv virtualmoon*.deb $wd/$outdir/
#    if [[ $? -ne 0 ]]; then exit 1;fi
    # rpm
#    cd $wd
#    rsync -a --exclude=.svn Installer/Linux/rpm $builddir
#    cd $builddir
#    mv debian/virtualmoon/usr/* rpm/virtualmoon/usr/
#    cd rpm
#    sed -i "/Version:/ s/5/$version/"  SPECS/virtualmoon.spec
#    sed -i "/Release:/ s/1/1/" SPECS/virtualmoon.spec
#    setarch i386 fakeroot rpmbuild  --buildroot "$builddir/rpm/virtualmoon" --define "_topdir $builddir/rpm/" -bb SPECS/virtualmoon.spec
#    if [[ $? -ne 0 ]]; then exit 1;fi
#    mv RPMS/i386/virtualmoon*.rpm $wd/$outdir/
#    if [[ $? -ne 0 ]]; then exit 1;fi
#  fi # if [[ $pro ]]
  if [[ $make_debug ]]; then 
    #debug
    cd $wd
    mkdir $builddir/debug
    cp virtualmoon/atlun $builddir/debug/
    cp photlun/photlun $builddir/debug/
    cp datlun/datlun $builddir/debug/
    cd $builddir/debug/
    tar cvzf virtualmoon$updname-bin-linux_i386-debug-$currentrev.tgz --owner=root --group=root *
    if [[ $? -ne 0 ]]; then exit 1;fi
    mv virtualmoon$updname-bin-*.tgz $wd/$outdir/
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
  make CPU_TARGET=x86_64 OS_TARGET=linux
  if [[ $? -ne 0 ]]; then exit 1;fi
  if [[ $upd ]]; then
    make install_update
    if [[ $? -ne 0 ]]; then exit 1;fi
  else 
    make install
    if [[ $? -ne 0 ]]; then exit 1;fi
  fi 
  # tar
  cd $builddir
  if [[ $upd ]]; then
    tar cvzf virtualmoon$updname-$version-$currentrev-linux_x86_64.tgz --owner=root --group=root *
    if [[ $? -ne 0 ]]; then exit 1;fi
  else
    tar cvzf virtualmoon-$version-linux_x86_64.tgz --owner=root --group=root *
    if [[ $? -ne 0 ]]; then exit 1;fi
  fi
  if [[ $cdrom ]]; then
    mkdir $wd/CD_Linux
    mv virtualmoon*.tgz $wd/CD_Linux/
    if [[ $? -ne 0 ]]; then exit 1;fi
    cp /home/transfert/avl_release/virtualmoon-$version-linux_i386.tgz $wd/CD_Linux/
  else
    mv virtualmoon*.tgz $wd/$outdir/
    if [[ $? -ne 0 ]]; then exit 1;fi
    cp /home/transfert/avl_release/virtualmoon-$version-linux_i386.tgz $wd/$outdir/
  fi
#  if [[ $pro ]]; then
    # deb
#    cd $wd
#    rsync -a --exclude=.svn Installer/Linux/debian $builddir
#    cd $builddir
#    mv bin debian/virtualmoon64/usr/
#    mv share debian/virtualmoon64/usr/
#    cd debian
#    sed -i "/Version:/ s/5/$version/" virtualmoon64/DEBIAN/control
#    fakeroot dpkg-deb --build virtualmoon64 .
#    if [[ $? -ne 0 ]]; then exit 1;fi
#    mv virtualmoon*.deb $wd/$outdir/
#    if [[ $? -ne 0 ]]; then exit 1;fi
    # rpm
#    cd $wd
#    rsync -a --exclude=.svn Installer/Linux/rpm $builddir
#    cd $builddir
#    mv debian/virtualmoon64/usr/* rpm/virtualmoon/usr/
#    cd rpm
#    sed -i "/Version:/ s/5/$version/"  SPECS/virtualmoon64.spec
#    sed -i "/Release:/ s/1/1/" SPECS/virtualmoon64.spec
#    # rpm 4.7
#    fakeroot rpmbuild  --buildroot "$builddir/rpm/virtualmoon" --define "_topdir $builddir/rpm/" -bb SPECS/virtualmoon64.spec
#    if [[ $? -ne 0 ]]; then exit 1;fi
#    mv RPMS/x86_64/virtualmoon*.rpm $wd/$outdir/
#    if [[ $? -ne 0 ]]; then exit 1;fi
#  fi # if [[ $pro ]]
  if [[ $make_debug ]]; then
    #debug
    cd $wd
    mkdir $builddir/debug
    cp virtualmoon/atlun $builddir/debug/
    cp photlun/photlun $builddir/debug/
    cp datlun/datlun $builddir/debug/
    cd $builddir/debug/
    tar cvzf virtualmoon$updname-bin-linux_x86_64-debug-$currentrev.tgz --owner=root --group=root *
    if [[ $? -ne 0 ]]; then exit 1;fi
    mv virtualmoon$updname-bin-*.tgz $wd/$outdir/
    if [[ $? -ne 0 ]]; then exit 1;fi
  fi
  cd $wd
  rm -rf $builddir
fi

# make Linux Data for both architectures
if [[ $make_linux_data ]]; then 
if [[ ! $upd ]]; then
  ./configure $configopt prefix=$builddir target=i386-linux$extratarget
  if [[ $? -ne 0 ]]; then exit 1;fi
  make install_data
  if [[ $? -ne 0 ]]; then exit 1;fi
  if [[ $cdrom ]]; then
     make install_data2
     if [[ $? -ne 0 ]]; then exit 1;fi
  fi
  # tar
  cd $builddir
  if [[ $cdrom ]]; then
    tar cvzf virtualmoon-data1-$version-linux_all.tgz --owner=root --group=root *
    if [[ $? -ne 0 ]]; then exit 1;fi
    mkdir $wd/CD_Linux
    mv virtualmoon*.tgz $wd/CD_Linux/
    if [[ $? -ne 0 ]]; then exit 1;fi
    # data3 
    cd $wd
    rm -rf $builddir
    make install_data3
    if [[ $? -ne 0 ]]; then exit 1;fi
    cd $builddir
    tar cvzf virtualmoon-data2-$version-linux_all.tgz --owner=root --group=root *
    if [[ $? -ne 0 ]]; then exit 1;fi
    mv virtualmoon*.tgz $wd/CD_Linux/
    if [[ $? -ne 0 ]]; then exit 1;fi
    # data4
    cd $wd
    rm -rf $builddir
    make install_data4
    if [[ $? -ne 0 ]]; then exit 1;fi
    cd $builddir
    tar cvzf virtualmoon-data3-$version-linux_all.tgz --owner=root --group=root *
    if [[ $? -ne 0 ]]; then exit 1;fi
    mv virtualmoon*.tgz $wd/CD_Linux/
    if [[ $? -ne 0 ]]; then exit 1;fi
    cd $wd
    cp Installer/Linux/licence $wd/CD_Linux/
    cp Installer/Linux/readme_CD $wd/CD_Linux/readme
    cp Installer/Linux/install_CD.sh $wd/CD_Linux/install.sh
#    mkisofs -R -r -l -J -quiet -Vvirtualmoon -o$release/virtualmoon-$version-linux.iso CD_Linux
  else
    tar cvzf virtualmoon-data-$version-linux_all.tgz --owner=root --group=root *
    if [[ $? -ne 0 ]]; then exit 1;fi
    mv virtualmoon*.tgz $wd/$outdir/
    if [[ $? -ne 0 ]]; then exit 1;fi
    cd $wd
    cp Installer/Linux/vmapro_install.sh $wd/$outdir/
    cp Installer/Linux/licence $wd/$outdir/
    if [[ $pro ]]; then
       cd $wd/$outdir/
       tar cf $wd/$release/virtualmoon-$version-linux.tar vmapro_install.sh licence virtualmoon-data-$version-linux_all.tgz virtualmoon-$version-linux_x86_64.tgz virtualmoon-$version-linux_i386.tgz
    fi
  fi
#  if [[ $pro ]]; then 
    # deb
#    cd $wd
#    rsync -a --exclude=.svn Installer/Linux/debian $builddir
#    cd $builddir
#    mv share debian/virtualmoon-data/usr/
#    cd debian
#    sed -i "/Version:/ s/5/$version/" virtualmoon-data/DEBIAN/control
#    fakeroot dpkg-deb --build virtualmoon-data .
#    if [[ $? -ne 0 ]]; then exit 1;fi
#    mv virtualmoon-data*.deb $wd/$outdir/
#    if [[ $? -ne 0 ]]; then exit 1;fi
#    # rpm
#    cd $wd
#    rsync -a --exclude=.svn Installer/Linux/rpm $builddir
#    cd $builddir
#    mv debian/virtualmoon-data/usr/* rpm/virtualmoon-data/usr/
#    cd rpm
#    sed -i "/Version:/ s/5/$version/"  SPECS/virtualmoon-data.spec
#    sed -i "/Release:/ s/1/1/" SPECS/virtualmoon-data.spec
#    fakeroot rpmbuild  --buildroot "$builddir/rpm/virtualmoon-data" --define "_topdir $builddir/rpm/" -bb SPECS/virtualmoon-data.spec
#    if [[ $? -ne 0 ]]; then exit 1;fi
#    mv RPMS/noarch/virtualmoon*.rpm $wd/$outdir/
#    if [[ $? -ne 0 ]]; then exit 1;fi
#  fi

  cd $wd
  rm -rf $builddir
fi
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
  make OS_TARGET=win32 CPU_TARGET=i386
  if [[ $? -ne 0 ]]; then exit 1;fi
  if [[ $upd ]]; then
    make install_win_update
    if [[ $? -ne 0 ]]; then exit 1;fi
  else 
    make install_win
    if [[ $? -ne 0 ]]; then exit 1;fi
    make install_win_data
    if [[ $? -ne 0 ]]; then exit 1;fi
    if [[ $cdrom ]]; then
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
    fi
  fi 
  # zip
  if [[ $upd ]]; then
    cd $builddir/vmapro/Data
    zip -r  virtualmoon$updname-$version-$currentrev-windows.zip *
    if [[ $? -ne 0 ]]; then exit 1;fi
    mv virtualmoon*.zip $wd/$outdir/
    if [[ $? -ne 0 ]]; then exit 1;fi
  fi
  # exe
  cd $builddir
  if [[ $cdrom ]]; then
    sed -i "/AppVerName/ s/V5/V$version/" vmapro_CD.iss
    wine "$innosetup" "$wine_build\vmapro_CD.iss"
    if [[ $? -ne 0 ]]; then exit 1;fi
    mkdir $wd/CD_Win32/
    mv $builddir/setup*.exe $wd/CD_Win32/
    mv $builddir/setup*.bin $wd/CD_Win32/
    cp $builddir/Data/licence.txt  $wd/CD_Win32/
    cp $builddir/Data/licence_fr.txt  $wd/CD_Win32/
    cp $builddir/Data/readme.txt  $wd/CD_Win32/
    cp $builddir/Data/lisezmoi.txt  $wd/CD_Win32/
    cp $builddir/Data/autorun.inf  $wd/CD_Win32/
    cp $builddir/Data/vma.ico  $wd/CD_Win32/
    cd $wd 
    mkisofs -R -r -l -J -quiet -Vvirtualmoon -o$release/virtualmoon-$version-windows.iso CD_Win32
  else
    sed -i "/AppVerName/ s/V5/V$version/" vmapro.iss
    if [[ $upd ]]; then
      sed -i "/OutputBaseFilename/ s/-windows/$updname-$version-$currentrev-windows/" vmapro.iss
    else
      sed -i "/OutputBaseFilename/ s/-windows/-$version-windows/" vmapro.iss
    fi
    wine "$innosetup" "$wine_build\vmapro.iss"
    if [[ $? -ne 0 ]]; then exit 1;fi
    if [[ $pro ]]; then
      mv $builddir/virtualmoon*.exe $wd/$release
    else
      mv $builddir/virtualmoon*.exe $wd/$outdir/
    fi
  fi
  if [[ $make_debug ]]; then
    #debug
    cd $wd
    mkdir $builddir/debug
    cp virtualmoon/atlun.exe $builddir/debug/
    cp photlun/photlun.exe $builddir/debug/
    cp datlun/datlun.exe $builddir/debug/
    cd $builddir/debug/
    zip virtualmoon$updname-bin-windows_i386-debug-$currentrev.zip *
    if [[ $? -ne 0 ]]; then exit 1;fi
    mv virtualmoon$updname-bin-*.zip $wd/$outdir/
    if [[ $? -ne 0 ]]; then exit 1;fi
  fi
  cd $wd
  rm -rf $builddir
fi

if [[ $upd ]]; then
  # store revision 
  echo $currentrev > last.build
fi
