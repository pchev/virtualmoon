{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit glsceneode; 

interface

uses
  GLODERegister, GLODECustomColliders, GLODEManager, GLODESkeletonColliders, 
    gloxode, ODEGL, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('GLODERegister', @GLODERegister.Register); 
end; 

initialization
  RegisterPackage('glsceneode', @Register); 
end.
