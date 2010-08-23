{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit glscenecgshader; 

interface

uses
  GLCgShader, cg, cgGL, GLCgRegister, GLCgBombShader, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('GLCgRegister', @GLCgRegister.Register); 
end; 

initialization
  RegisterPackage('glscenecgshader', @Register); 
end.
