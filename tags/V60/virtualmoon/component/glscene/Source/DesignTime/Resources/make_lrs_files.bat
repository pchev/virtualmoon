@echo off
del *.lrs
lazres.exe GLSceneObjectsLCL.lrs @GLSceneObjectsLCL.rc
lazres.exe GLSceneLCL.lrs @GLSceneLCL.rc
lazres.exe nonGLSceneLCL.lrs @nonGLSceneLCL.rc
lazres.exe GLSceneRunTimeLCL.lrs @GLSceneRunTimeLCL.rc
pause