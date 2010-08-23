@echo off
echo Copying ode.dll to the windows system32 directory

@echo Deploying for NT
copy ode.dll %SystemRoot%\System32\

rem @echo Deploying for Windows95 / Windows98 / WindowsME
rem copy ode.dll %Windir%\System\


pause