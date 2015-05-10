unit GLSS_BASS; 

interface

uses
  GLSMBASS, Bass, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSMBASS', @GLSMBASS.Register); 
end; 

initialization
  RegisterPackage('GLSS_BASS', @Register); 
end.
