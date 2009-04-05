REM Run this script to update all the translations after modification of a
REM resource string in u_translation.pas and compilation of the program.

REM Update first the path to your Lazarus installation and run "make" in lazarus/tools

D:\appli\lazarus\fpc\2.1.5\bin\i386-win32\rstconv -i D:\source\WinCE\pocketlun\u_translation.rst -o language\pocketlun.po
D:\appli\lazarus\tools\updatepofiles language\pocketlun.po

pause
