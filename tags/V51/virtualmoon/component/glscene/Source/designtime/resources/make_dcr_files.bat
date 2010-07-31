@echo off
del *.dcr
BRCC32 -r -foGLSceneVCL.dcr GLSceneVCL.rc
BRCC32 -r -foGLSceneObjects.dcr GLSceneObjects.rc
BRCC32 -r -fononGLSceneVCL.dcr nonGLSceneVCL.rc
BRCC32 -r -foGLSceneVCLBass.dcr GLSceneVCLBass.rc
BRCC32 -r -foGLSceneVCLFMod.dcr GLSceneVCLFMod.rc
BRCC32 -r -foGLSceneVCLODE.dcr GLSceneVCLODE.rc
BRCC32 -r -foGLSceneVCLSDL.dcr GLSceneVCLSDL.rc
BRCC32 -r -foGLSceneVCLCg.dcr GLSceneVCLCg.rc
pause