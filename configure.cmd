@echo off
rem
rem create freepascal Makefile to build virtualmoon
rem
rem set the variable below before to run
rem
rem To compile the C libraries plan404 
rem you need to install Mingw and Msys from http://www.mingw.org/
rem and add them to your PATH
rem For Windows 64 bits I have success with WPG System64 http://www.cadforte.com/system64.html

rem ################################################################
rem start parameters

rem where you install sed  ( http://gnuwin32.sourceforge.net/packages/sed.htm )
set sed=C:\GnuWin32\bin

rem where you install fpc 
set fpc=C:\pp\bin\x86_64-win64

rem where you install lazarus (keep double \\ for path) 
set lazarus=C:\\lazarus

rem where you want to install vma (keep double \\ for path)
set prefix=C:\\appli\\virtualmoon

rem end of parameters
rem ################################################################

set basedir=%CD%
set PATH=%basedir%;%fpc%;%sed%;%PATH%

rem test sed
sed --version
if %ERRORLEVEL% NEQ 0 (
  echo .
  echo Please edit configure.cmd to set the path to fpc, lazarus and sed
  goto :EOF
)
rem test fpc
fpc -iV
if %ERRORLEVEL% NEQ 0 (
  echo .
  echo Please edit configure.cmd to set the path to fpc, lazarus and sed
  goto :EOF
)

echo virtualmoon\component\libsql > dirs.lst
echo virtualmoon\component\uniqueinstance >> dirs.lst
echo virtualmoon\component\enhedits >> dirs.lst
echo virtualmoon\component >> dirs.lst
echo virtualmoon\library >> dirs.lst
echo virtualmoon >> dirs.lst
echo datlun >> dirs.lst
echo photlun >> dirs.lst
echo . >> dirs.lst

echo using fpc in %fpc%
echo using Lazarus in %lazarus%
echo installing in %prefix%

for /F  %%d in (dirs.lst) do  (
   echo creating %%d\Makefile 
   cd %%d
   sed "s/\%%LAZDIR\%%/%lazarus%/" Makefile.in > Makefile.fpc
   sed -i "s/\%%PREFIX\%%/%prefix%/" Makefile.fpc
   fpcmake -q Makefile.fpc
   cd %basedir%
)
del dirs.lst

rem cd tools
rem sed "s/\%%PREFIX\%%/%prefix%/" Makefile.in > Makefile
rem cd %basedir%

echo You can now run make 
echo then make install 
echo and make install_data
