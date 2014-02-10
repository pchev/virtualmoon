#!/bin/bash

mkdir /tmp/glscene
svn export http://svn.code.sf.net/p/glscene/code/trunk/Lazarus /tmp/glscene/Lazarus
svn export http://svn.code.sf.net/p/glscene/code/trunk/Source /tmp/glscene/SourceFull

mkdir /tmp/glscene/Source
cp /tmp/glscene/SourceFull/* /tmp/glscene/Source/

rm -rf /tmp/glscene/SourceFull

find /tmp/glscene/Source -name *.obj -delete
find /tmp/glscene/Source -name *.OBJ -delete
find /tmp/glscene/Source -name *.dll -delete
find /tmp/glscene/Source -name *.DLL -delete
find /tmp/glscene/Source -name *.exe -delete
find /tmp/glscene/Source -name *.EXE -delete

fromdos /tmp/glscene/Source/*
fromdos /tmp/glscene/Lazarus/*

# cp -a /tmp/glscene/Lazarus /home/vma/src/virtualmoon/component/glscene/
# cp -a /tmp/glscene/Source /home/vma/src/virtualmoon/component/glscene/
# rm -rf /tmp/glscene
