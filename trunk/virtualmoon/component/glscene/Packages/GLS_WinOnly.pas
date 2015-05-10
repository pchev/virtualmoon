{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLS_WinOnly; 

interface

uses
    GLJoystick, GLScreenSaver, GLSMWaveOut, GLAVIRecorder, 
  GLSceneRegisterWinOnlyLCL, GLSVfw, GLSpaceText, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSceneRegisterWinOnlyLCL', @GLSceneRegisterWinOnlyLCL.Register
    ); 
end; 

initialization
  RegisterPackage('GLS_WinOnly', @Register); 
end.
