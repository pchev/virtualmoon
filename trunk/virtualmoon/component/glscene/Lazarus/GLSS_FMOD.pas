{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLSS_FMOD; 

interface

uses
    GLSMFMOD, fmodpresets, fmodtypes, fmoderrors, fmoddyn, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSMFMOD', @GLSMFMOD.Register); 
end; 

initialization
  RegisterPackage('GLSS_FMOD', @Register); 
end.
