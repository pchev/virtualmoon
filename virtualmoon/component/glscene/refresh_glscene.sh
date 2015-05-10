#!/bin/bash

rm -rf /tmp/glscene
svn export http://svn.code.sf.net/p/glscene/code/branches/GLScene_LCL /tmp/glscene

rm -rf /tmp/glscene/Samples
rm -rf /tmp/glscene/Locale

find /tmp/glscene/Source -name *.obj -delete
find /tmp/glscene/Source -name *.OBJ -delete
find /tmp/glscene/Source -name *.dll -delete
find /tmp/glscene/Source -name *.DLL -delete
find /tmp/glscene/Source -name *.exe -delete
find /tmp/glscene/Source -name *.EXE -delete

fromdos /tmp/glscene/Source/*
fromdos /tmp/glscene/Packages/*

