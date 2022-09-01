{:
  GLSL Diffuse Specular Shader Demo<p>

  A demo that shows how to use the TGLSLDiffuseSpecularShader component.

  Version history:
    24/07/09 - DaStr - Added fog support
    02/07/07 - DaStr - Removed old Timer leftovers
                       (GLSimpleNavigation component now does this stuff)
    20/03/07 - DaStr - Initial version


}
unit umainform;

interface

uses
  // VCL
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,

  // GLScene
  GLTexture, GLCadencer, GLViewer, GLScene, GLObjects, GLGraph,
  GLVectorTypes, GLUserShader, GLContext, GLVectorGeometry, GLGeomObjects,
  GLVectorFileObjects, GLSLDiffuseSpecularShader, GLSLShader, GLCustomShader,
  GLSimpleNavigation, GLCrossPlatform, GLMaterial, GLCoordinates, GLBaseClasses,

  // FileFormats
  GLFileMD2, GLFileMS3D, GLFile3DS;

type
  TGLSLTestForm = class(TForm)
    Scene: TGLScene;
    Viewer: TGLSceneViewer;
    Cadencer: TGLCadencer;
    Camera: TGLCamera;
    Light:  TGLLightSource;
    LightCube: TGLDummyCube;
    GLSphere1: TGLSphere;
    GLXYZGrid1: TGLXYZGrid;
    GLArrowLine1: TGLArrowLine;
    Panel1: TPanel;
    LightMovingCheckBox: TCheckBox;
    GUICube: TGLDummyCube;
    WorldCube: TGLDummyCube;
    Fighter: TGLActor;
    Teapot: TGLActor;
    Sphere_big: TGLActor;
    Sphere_little: TGLActor;
    MaterialLibrary: TGLMaterialLibrary;
    ShaderEnabledCheckBox: TCheckBox;
    TurnPitchrollCheckBox: TCheckBox;
    RealisticSpecularCheckBox: TCheckBox;
    LightCube2: TGLDummyCube;
    Light2: TGLLightSource;
    GLSphere2: TGLSphere;
    MultiLightShaderCheckBox: TCheckBox;

    DiffuseSpecularShader: TGLSLDiffuseSpecularShader;
    GLSimpleNavigation1: TGLSimpleNavigation;
    EnableFogCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: double);
    procedure LightCubeProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure ShaderEnabledCheckBoxClick(Sender: TObject);
    procedure RealisticSpecularCheckBoxClick(Sender: TObject);
    procedure MultiLightShaderCheckBoxClick(Sender: TObject);
    procedure EnableFogCheckBoxClick(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  GLSLTestForm:  TGLSLTestForm;
  MultiLightShader: TGLSLMLDiffuseSpecularShader;

implementation

{$R *.lfm}

uses
  GLUtils;

procedure TGLSLTestForm.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  // First load models.
  Fighter.LoadFromFile('waste.md2'); //Fighter
  Fighter.SwitchToAnimation(0, True);
  Fighter.AnimationMode := aamLoop;
  Fighter.Scale.Scale(3);

  Teapot.LoadFromFile('teapot.3ds'); //Teapot (no texture coordinates)
  Teapot.Scale.Scale(0.8);

  Sphere_big.LoadFromFile('Sphere_big.3DS'); //Sphere_big
  Sphere_big.Scale.Scale(70);

  Sphere_little.LoadFromFile('Sphere_little.3DS'); //Sphere_little
  Sphere_little.Scale.Scale(4);

  MaterialLibrary.LibMaterialByName('Earth').Material.Texture.Image.LoadFromFile('Earth.jpg');
  MaterialLibrary.LibMaterialByName('Fighter').Material.Texture.Image.LoadFromFile('waste.jpg');

  MaterialLibrary.LibMaterialByName('Earth').Shader := DiffuseSpecularShader;
  MaterialLibrary.LibMaterialByName('Fighter').Shader := DiffuseSpecularShader;

  // This is how a shader is created in runtime.
  MultiLightShader := TGLSLMLDiffuseSpecularShader.Create(Self);
  MultiLightShader.LightCompensation := 0.7;
  MultiLightShader.LightCount := 2;

  // Disable fog.
  EnableFogCheckBoxClick(nil);
end;

procedure TGLSLTestForm.CadencerProgress(Sender: TObject; const deltaTime, newTime: double);
begin
  Viewer.Invalidate;

  if TurnPitchrollCheckBox.Checked then
  begin
    Sphere_big.Pitch(40 * deltaTime);
    Fighter.Turn(40 * deltaTime);
    Sphere_little.Roll(40 * deltaTime);
    Teapot.Roll(-20 * deltaTime);
  end;
end;


procedure TGLSLTestForm.LightCubeProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if LightMovingCheckBox.Checked then
    LightCube.MoveObjectAround(Camera.TargetObject, sin(NewTime) * deltaTime * 10, deltaTime * 20);
end;


procedure TGLSLTestForm.ShaderEnabledCheckBoxClick(Sender: TObject);
begin
  DiffuseSpecularShader.Enabled := ShaderEnabledCheckBox.Checked;
  MultiLightShader.Enabled := ShaderEnabledCheckBox.Checked;
end;

procedure TGLSLTestForm.RealisticSpecularCheckBoxClick(Sender: TObject);
begin
  DiffuseSpecularShader.RealisticSpecular := RealisticSpecularCheckBox.Checked;
  MultiLightShader.RealisticSpecular := RealisticSpecularCheckBox.Checked;
  if DiffuseSpecularShader.RealisticSpecular then
  begin
    DiffuseSpecularShader.SpecularPower := 20;
    MultiLightShader.SpecularPower := 20;
  end
  else
  begin
    DiffuseSpecularShader.SpecularPower := 8;
    MultiLightShader.SpecularPower := 8;
  end;
end;

procedure TGLSLTestForm.MultiLightShaderCheckBoxClick(Sender: TObject);
begin
  if MultiLightShaderCheckBox.Checked then
  begin
    MaterialLibrary.LibMaterialByName('Earth').Shader := MultiLightShader;
    MaterialLibrary.LibMaterialByName('Fighter').Shader := MultiLightShader;
  end
  else
  begin
    MaterialLibrary.LibMaterialByName('Earth').Shader := DiffuseSpecularShader;
    MaterialLibrary.LibMaterialByName('Fighter').Shader := DiffuseSpecularShader;
  end;

  Light2.Shining := MultiLightShaderCheckBox.Checked;
  LightCube2.Visible := MultiLightShaderCheckBox.Checked;
end;

procedure TGLSLTestForm.EnableFogCheckBoxClick(Sender: TObject);
begin
  if EnableFogCheckBox.Checked then
  begin
    Viewer.Buffer.FogEnable := True;
    
    DiffuseSpecularShader.NotifyChange(Self);
    MultiLightShader.NotifyChange(Self);
  end
  else
  begin
    Viewer.Buffer.FogEnable := False;

    DiffuseSpecularShader.NotifyChange(Self);
    MultiLightShader.NotifyChange(Self);
  end;
end;

end.
