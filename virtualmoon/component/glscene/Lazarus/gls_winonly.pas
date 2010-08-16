{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit gls_winonly; 

interface

uses
  GLSMBASS, Bass, GLSMWaveOut, ScreenSaver, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSMBASS', @GLSMBASS.Register); 
end; 

initialization
  RegisterPackage('gls_winonly', @Register); 
end.
