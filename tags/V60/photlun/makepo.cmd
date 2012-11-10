REM Run this script to update all the translations after modification of a
REM resource string in u_translation.pas and compilation of the program.

REM Update first the path to your Lazarus installation and run "make" in lazarus/tools

D:\lazarus\fpc\2.2.0\bin\i386-win32\rstconv -i units\i386-win32-win32\u_translation.rst -o language\photlun.po
D:\lazarus\tools\updatepofiles language\photlun.po

pause
