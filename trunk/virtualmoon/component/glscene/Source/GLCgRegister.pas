//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLCgRegister<p>

   Registration unit for CG shader.<p>

   <b>History :</b><font size=-1><ul>
      <li>11/11/09 - DaStr - Improved FPC compatibility
                             (thanks Predator) (BugtrackerID = 2893580)
      <li>23/02/07 - DaStr - Initial version

}
unit GLCgRegister;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes,
  {$IFDEF FPC}
    propedits, GLSceneRegisterLCL,
  {$ELSE}
    DesignIntf, DesignEditors, VCLEditors,
    GLSceneRegister,
  {$ENDIF}

  // GLScene
  GLMaterial,

  // CG
  Cg, CgGL, GLCgShader, GLCgBombShader;

procedure Register;

implementation

procedure Register;
begin
  // Register components.
  RegisterComponents('GLScene Shaders', [TCgShader, TGLCgBombShader]);

  // Register property editors.
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLCgBombShader, '', TGLLibMaterialNameProperty);
end;

end.
