@echo off

set destdir=%1%

if not defined destdir ( 
  echo Specify the install directory 
  exit 1
)

if not exist %destdir% ( 
  echo create directory %destdir% 
  mkdir %destdir%
)

if not exist %destdir% ( 
  echo directory %destdir% do not exist 
  exit 1
)

echo Install virtualmoon to %destdir%

xcopy /Y /F virtualmoon\atlun.exe %destdir%\
xcopy /Y /F datlun\datlun.exe %destdir%\
xcopy /Y /F photlun\photlun.exe %destdir%\

strip -v %destdir%\atlun.exe
strip -v %destdir%\datlun.exe
strip -v %destdir%\photlun.exe

xcopy /Y /F virtualmoon\library\plan404\libplan404.dll  %destdir%\
xcopy /Y /F virtualmoon\library\sqlite\sqlite3.dll  %destdir%\
