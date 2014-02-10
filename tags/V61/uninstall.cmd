echo This script is incomplete!
echo Please look at uninstall_win.sh 

@echo off

set destdir=%1%

if not defined destdir ( 
  echo Specify the install directory 
  exit 1
)

if not exist %destdir% ( 
  echo directory %destdir% do not exist 
  exit 1
)

echo uninstall virtualmoon from %destdir%

del /F %destdir%\atlun.exe 
del /F %destdir%\datlun.exe 
del /F %destdir%\photlun.exe
del /F %destdir%\libplan404.dll  
del /F %destdir%\sqlite3.dll  

