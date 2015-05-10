unit GLSS_OpenAL; 

interface

uses
  GLSMOpenAL, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSMOpenAL', @GLSMOpenAL.Register); 
end; 

initialization
  RegisterPackage('GLSS_OpenAL', @Register); 
end.
