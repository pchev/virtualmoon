{
  GLODERegister - Design time registration code for the ODE Manager

  History:

    18/06/03 - SG - Creation.
}
unit GLODERegister;

interface

uses
  Classes, GLSceneRegister, GLODEManager, gloxode, glstrings;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// Register
//
procedure Register;
begin
  RegisterClasses([TGLODEManager, TGLODEJointList, TODEJoints, TODEElements]);
  RegisterComponents('GLScene',[TGLODEManager,TGLODEJointList]);
end;

initialization
  with ObjectManager do begin
      //////////////////////////////////////////////////////////////////////////
      // GLOxOde
      RegisterSceneObject(TGLSizableDummyCube, 'GLSizableDummyCube', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXOdeEngine, 'GLOXOdeEngine', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXStaBall, 'GLOXStaBall', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXStaCylinder, 'GLOXStaCylinder', glsOCSpecialObjects);
//      RegisterSceneObject(TGLOXStaCCylinder, 'GLOXStaCCylinder', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXStaBox, 'GLOXStaBox', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXStaMesh, 'GLOXStaMesh', glsOCSpecialObjects);
//      RegisterSceneObject(TGLOXZStaTerrain, 'GLOXZStaTerrain', glsOCSpecialObjects);
//      RegisterSceneObject(TGLOXStaCone, 'GLOXStaCone', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXDynMesh , 'GLOXDynMesh ', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXDynBall, 'GLOXDynBall', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXDynBox, 'GLOXDynBox', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXDynCylinder, 'GLOXDynCylinder', glsOCSpecialObjects);
//      RegisterSceneObject(TGLOXDynCCylinder, 'GLOXDynCCylinder', glsOCSpecialObjects);
//      RegisterSceneObject(TGLOXDynCone, 'GLOXDynCone', glsOCSpecialObjects);
//      RegisterSceneObject(TGLOXDynCar, 'GLOXDynCar', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXRagdoll, 'GLOXRagdoll', glsOCSpecialObjects);
      RegisterSceneObject(TGLOXAMotor, 'GLOXAMotor', glsOCSpecialObjects);
      //////////////////////////////////////////////////////////////////////////
  end;

end.
