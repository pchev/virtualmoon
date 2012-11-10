#!/bin/bash

my_id=$(id -u)
if [ $my_id != 0 ] ; then
  echo ""
  echo "You must run this script as root to install Virtual Moon Atlas at the default location."
  echo "For example: sudo ./install.sh"
  echo ""
  echo "You can also install the program at any other location you want as root or as normal user."
  echo "For example: cd ~; mkdir vma6; cd vma6; tar xzf /media/cdrom/virtualmoon-6.0-linux_i386.tgz"
  echo "But in this case you must ensure the files in lib folder can be loaded."
  echo "See man ldconfig for more information."
  echo ""
  echo "Installation unsuccessful"
  echo ""
  exit 1
fi

# check architecture
ARCH=$(uname -m)
if [ $ARCH != x86_64 ] ; then ARCH=i386; fi   # handle i686, i586, ...

echo ""
echo "Installing Virtual Moon Atlas "$ARCH
echo ""

current_dir=$(pwd)

tarfile1="$current_dir"/virtualmoon-6.0-linux_$ARCH.tgz
tarfile2="$current_dir"/virtualmoon-data1-6.0-linux_all.tgz
tarfile3="$current_dir"/virtualmoon-data2-6.0-linux_all.tgz
tarfile4="$current_dir"/virtualmoon-data3-6.0-linux_all.tgz

#check file exist
if [ ! -e $tarfile1 ]; then
  echo "File not found: "$tarfile1
  echo "Installation aborted" 
  exit 1  
fi
if [ ! -e $tarfile2 ]; then
  echo "File not found: "$tarfile2
  echo "Installation aborted" 
  exit 1  
fi
if [ ! -e $tarfile3 ]; then
  echo "File not found: "$tarfile3
  echo "Installation aborted" 
  exit 1  
fi
if [ ! -e $tarfile4 ]; then
  echo "File not found: "$tarfile4
  echo "Installation aborted" 
  exit 1  
fi

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
echo "Installing software ..."
tar xzf "$tarfile1" 
rc=$?
echo "Installing data, please wait ..."
tar xzf "$tarfile2" 
rc1=$?
((rc=rc+rc1))
# data option
echo ""
echo "Do you want to install the high resolution 120m textures (+1GB disk space)"
read -p "? [y,n] :"
if [ "$REPLY" = "y" ]; then 
  echo "Installing data, please wait ..."
  tar xzf "$tarfile3" 
  rc1=$?
  ((rc=rc+rc1))
  echo ""
  echo "Do you want to install the high resolution 60m textures (+2.2GB disk space)"
  read -p "? [y,n] :"
  if [ "$REPLY" = "y" ]; then 
    echo "Installing data, please wait ..."
    tar xzf "$tarfile4" 
    rc1=$?
    ((rc=rc+rc1))
  fi
fi
cd "$current_dir"
if [ $rc != 0 ] ; then 
  echo "Errors occured during installation"
  echo "tar exit with "$rc
  echo "Installation aborted" 
  exit $rc 
fi

# ensure lib are accessible
ldconfig
ldconfig -p | grep libvma404
rc=$?
if [ $rc = 0 ] ; then 
  echo "Installation successful" 
  echo "You can now run Virtual Moon Atlas with the following command:"
  echo "$install_dir/bin/atlun"
  echo "Or the Command Center with the following command:"
  echo "$install_dir/bin/cclun"
else 
  echo "Installation successful" 
  echo "You can now run Virtual Moon Atlas with the following command:"
  echo "export LD_LIBRARY_PATH=$install_dir/lib && $install_dir/bin/atlun"
fi
echo ""