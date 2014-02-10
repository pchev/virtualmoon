unit GLScene_ODE; 

interface

uses
    GLODERegister, GLODECustomColliders, GLODEManager, dynodegl, ODEImport, 
  ODEGL, GLODERagdoll, GLODESkeletonColliders, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLODERegister', @GLODERegister.Register); 
end; 

initialization
  RegisterPackage('GLScene_ODE', @Register); 
end.
