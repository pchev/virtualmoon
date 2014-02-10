#!/bin/bash

my_id=$(id -u)
if [ $my_id != 0 ] ; then
  echo ""
  echo "You must run this script as root to install Virtual Moon Atlas at the default location."
  echo "For example: sudo ./vmapro_install.sh"
  echo ""
  echo "You can also install the program at any other location you want as root or as normal user."
  echo "For example: cd ~; mkdir vma6; cd vma6; tar xzf ~/Download/virtualmoon-6.1-linux_i386.tgz"
  echo "But in this case you must ensure the files in lib folder can be loaded."
  echo "See man ldconfig for more information."
  echo ""
  echo "Installation unsuccessful"
  echo ""
  exit 1
fi

echo ""
echo "Virtual Moon Atlas"
echo ""
echo "You can use this script without parameter for the initial installation of virtualmoon-6.1"
echo "or give the name of the additional file to install: sudo ./vmapro_install.sh PictureApollo.tgz"
echo ""

ARCH=$(uname -m)
if [ $ARCH != x86_64 ] ; then ARCH=i386; fi   # handle i686, i586, ...

# read options 
current_dir=$(pwd)
defaultfile=virtualmoon-6.1-linux_$ARCH.tgz
tarfile2="$current_dir"/virtualmoon-data-6.1-linux_all.tgz

tarfile1=$defaultfile;
if [ "$1" ] ; then 
  tarfile1="$1"
  unset tarfile2;
fi

# check if file install need ldconfig
if [[ $tarfile1 == $defaultfile ]]; then 
  initial_install=1
fi
if [[ $tarfile1 == virtualmoon_update-* ]]; then 
  initial_install=1
fi

# check architecture
if [ "$initial_install" ]; then 
filearch=$(echo $tarfile1 |sed "s/\(.*\)linux_//"|sed "s/.tgz//")
if [ $filearch == vmapro5 ]; then filearch=i386; fi
if [ $filearch != $ARCH ]; then
  echo "Your system is running with $ARCH architecture,"
  echo "but you try to install a file for $filearch"   
  read -p "Are you sure? [y,n] :"
  if [ "$REPLY" != "y" ]; then echo "Installation aborted"; exit 1; fi
fi
fi

tarfile1="$current_dir"/$tarfile1

#check file exist
if [ ! -e $tarfile1 ]; then
  echo "File not found: "$tarfile1
  echo "Installation aborted" 
  exit 1  
fi

echo "Installing "$tarfile1
echo ""

# get install dir
d=$(which atlun)
d=${d/\/bin\/atlun}
if [[ -n $d ]] 
  then install_dir=$d
  else install_dir=/usr/local
fi
read -p "Select installation directory [$install_dir] :" 
if [ "$REPLY" ] ; then install_dir=$REPLY; fi

# final confirmation
echo ""
echo Now installing Virtual Moon Atlas to $install_dir
read -p "Are you sure? [y,n] :"
if [ "$REPLY" != "y" ]; then echo "Installation aborted"; exit 1; fi

# create directory
mkdir -p "$install_dir"
# extract tar
cd "$install_dir"
tar xzf "$tarfile1" 
rc=$?
# try to move lib to lib64 for redhat 64
if [ "$initial_install" ]; then 
  if [ $filearch = x86_64 ]; then
    mv lib/libvma404.so lib64/ 2>/dev/null
  fi
fi

if [[ -n $tarfile2 ]] ; then
  if [ ! -e $tarfile2 ]; then
    echo "File not found: "$tarfile2
    echo "Installation aborted" 
    exit 1  
  fi
  echo "Installing "$tarfile2
  echo ""
  tar xzf "$tarfile2" 
  rc1=$?
  ((rc=rc+rc1))
fi

# check result
cd "$current_dir"
if [ $rc != 0 ] ; then 
  echo "Errors occured during installation"
  echo "tar exit with "$rc
  echo "Installation aborted" 
  exit $rc 
fi

# ensure lib are accessible
if [ "$initial_install" ]; then 
ldconfig
ldconfig -p | grep libvma404
rc=$?
if [ $rc = 0 ] ; then 
  echo "Installation successful" 
  echo "Then run Virtual Moon Atlas with the following command:"
  echo "$install_dir/bin/atlun"
  echo "Or the Command Center with the following command:"
  echo "$install_dir/bin/cclun"
else 
  echo "Installation successful" 
  echo "Then run Virtual Moon Atlas with the following command:"
  echo "export LD_LIBRARY_PATH=$install_dir/lib && $install_dir/bin/atlun"
fi
echo ""
fi

# end

