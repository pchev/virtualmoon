#!/bin/bash

mkdir /tmp/glscene
svn export https://glscene.svn.sourceforge.net/svnroot/glscene/trunk/Lazarus /tmp/glscene/Lazarus
svn export https://glscene.svn.sourceforge.net/svnroot/glscene/trunk/Source /tmp/glscene/SourceFull

mkdir /tmp/glscene/Source
cp /tmp/glscene/SourceFull/* /tmp/glscene/Source/

cp -a /tmp/glscene/SourceFull/Base /tmp/glscene/Source/
cp -a /tmp/glscene/SourceFull/DesignTime /tmp/glscene/Source/
cp -a /tmp/glscene/SourceFull/FileFormats /tmp/glscene/Source/
cp -a /tmp/glscene/SourceFull/GLSLShaders /tmp/glscene/Source/
cp -a /tmp/glscene/SourceFull/Platform /tmp/glscene/Source/
cp -a /tmp/glscene/SourceFull/PropertyEditors /tmp/glscene/Source/
cp -a /tmp/glscene/SourceFull/Shaders /tmp/glscene/Source/

rm -rf /tmp/glscene/SourceFull

find /tmp/glscene/Source -name *.obj -delete
find /tmp/glscene/Source -name *.OBJ -delete
find /tmp/glscene/Source -name *.dll -delete
find /tmp/glscene/Source -name *.DLL -delete
find /tmp/glscene/Source -name *.exe -delete
find /tmp/glscene/Source -name *.EXE -delete

# cp /tmp/glscene/Lazarus/* /home/pch/appli/sources/vma/virtualmoon/component/glscene/Lazarus/
# cp -a /tmp/glscene/Source /home/pch/appli/sources/vma/virtualmoon/component/glscene/
# rm -rf /tmp/glscene
