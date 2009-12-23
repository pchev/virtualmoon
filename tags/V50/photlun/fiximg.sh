#!/bin/bash
#
# Fix buggy 8 bit jpeg format by saving the picture again using 24 bit.
# 
# Version for Linux because the Delphi fiximg program is not cross platform.
#
# Require : djpeg and cjpeg commands
#           from libjpeg-progs package 
#
# Use : fiximg.sh filename.jpg
#
if [ -z $1 ] ; then exit 1 ; fi
djpeg -bmp "$1" | cjpeg -quality 50 > "$1.tmp"
rc=$?
if [ $rc != 0 ] ; then exit $rc ; fi
if [ -s "$1".tmp ] ; then
  rm "$1"
  mv "$1.tmp" "$1"
fi

