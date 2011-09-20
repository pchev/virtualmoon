@echo off
echo Copying ODE's dll to the windows system32 directory

@echo Deploying for NT
copy ode_single.dll %SystemRoot%\System32\
copy ode_singled.dll %SystemRoot%\System32\
copy ode_double.dll %SystemRoot%\System32\
copy ode_doubled.dll %SystemRoot%\System32\

rem @echo Deploying for Windows95 / Windows98 / WindowsME
rem copy ode_single.dll %Windir%\System\
rem copy ode_singled.dll %Windir%\System\
rem copy ode_double.dll %Windir%\System\
rem copy ode_doubled.dll %Windir%\System\


pause