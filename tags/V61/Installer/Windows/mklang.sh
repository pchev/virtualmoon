#!/bin/bash

# put the translation in subfolder $lg/share/virtualmoon/

innosetup="C:\Program Files\Inno Setup 5\ISCC.exe"  # Install under Wine from http://www.jrsoftware.org/isinfo.php
wine_build="F:\vma\translation"

curd=$(pwd)
lg=$1 
lgu=$(echo $lg | tr [a-z] [A-Z] );

if [ -z $lg ]; then exit 1 ; fi


# Linux
cd $lg; tar cvzf $curd/setup/lang_$lg.tgz  --owner=root --group=root *
cd $curd

# Windows
wine "$innosetup" "$wine_build\Translation_$lgu.iss"
