#!/bin/bash

my_id=$(id -u)
if [ $my_id != 0 ] ; then
  echo ""
  echo "You must run this script as root to install Virtual Moon Atlas at the default location."
  echo "For example: sudo ./vmapro_install.sh"
  echo ""
  echo "You can also install the program at any other location you want as root or as normal user."
  echo "For example: cd ~; mkdir vma5; cd vma5; tar xzf ~/Download/vmapro5.tgz"
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
echo "You can use this script without parameter for the initial installation of vmapro5.tgz"
echo "or give the name of the additional file to install: sudo ./vmapro_install.sh PictureApollo.tgz"
echo ""

current_dir=$(pwd)
tarfile="$current_dir"/vmapro5.tgz
initial_install=1
if [ "$1" ] ; then 
  tarfile="$current_dir/$1"
  unset initial_install
fi

if [ ! -e $tarfile ]; then
  echo "File not found: "$tarfile
  echo "Installation aborted" 
  exit 1  
fi

echo "Installing "$tarfile
echo ""

install_dir=/usr/local
read -p "Select installation directory [$install_dir] :" 
if [ "$REPLY" ] ; then install_dir=$REPLY; fi

echo ""
echo Now installing Virtual Moon Atlas to $install_dir
read -p "Are you sure? [y,n] :"
if [ "$REPLY" != "y" ]; then echo "Installation aborted"; exit 1; fi

mkdir -p "$install_dir"
cd "$install_dir"
tar xzf "$tarfile" 
rc=$?
cd "$current_dir"
if [ $rc != 0 ] ; then 
  echo "Errors occured during installation"
  echo "tar exit with "$rc
  echo "Installation aborted" 
  exit $rc 
fi

if [ "$initial_install" ]; then 
ldconfig
ldconfig -p | grep libplan404
rc=$?
if [ $rc = 0 ] ; then 
  echo "Installation successful" 
  echo "You can now run Virtual Moon Atlas with the following command:"
  echo "$install_dir/bin/atlun"
else 
  echo "Installation successful" 
  echo "You can now run Virtual Moon Atlas with the following command:"
  echo "export LD_LIBRARY_PATH=$install_dir/lib && $install_dir/bin/atlun"
fi
echo ""
fi