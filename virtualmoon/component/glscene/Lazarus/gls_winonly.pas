{ This file was automatically created by Lazarus. Do not edit!
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
  RegisterUnit('GLSMWaveOut', @GLSMWaveOut.Register); 
  RegisterUnit('ScreenSaver', @ScreenSaver.Register); 
end; 

initialization
  RegisterPackage('gls_winonly', @Register); 
end.
