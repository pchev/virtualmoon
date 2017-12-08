unit GLScene_OpenAL; 

interface

uses
  GLSMOpenAL, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSMOpenAL', @GLSMOpenAL.Register); 
end; 

initialization
  RegisterPackage('GLScene_OpenAL', @Register); 
end.
