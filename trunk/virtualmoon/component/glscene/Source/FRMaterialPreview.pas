//
// This unit is part of the GLScene Project, http://glscene.org
//
{: FRMaterialPreview<p>

   Material Preview frame.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>12/07/07 - DaStr - Improved Cross-Platform compatibility
                             (Bugtracker ID = 1684432)
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
      <li>16/12/06 - DaStr - Editor enhanced
      <li>03/07/04 - LR  - Make change for Linux
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit FRMaterialPreview;

interface

{$i GLScene.inc}

uses
{$IFDEF GLS_DELPHI_XE2_UP}
  VCL.Forms, VCL.StdCtrls, VCL.ComCtrls, Vcl.Controls,
{$ELSE}
  Forms, StdCtrls, ComCtrls, Controls,
{$ENDIF}
  Classes,
  GLScene, GLObjects, GLTexture, GLHUDObjects, GLViewer, GLTeapot,
  GLGeomObjects, GLColor, GLWin32Viewer, GLCoordinates,
  GLCrossPlatform, BaseClasses, GLMaterial;


type
  TRMaterialPreview = class(TFrame)
    GLScene1: TGLScene;
    SceneViewer: TGLSceneViewer;
    CBObject: TComboBox;
    Camera: TGLCamera;
    Cube: TGLCube;
    Sphere: TGLSphere;
    LightSource: TGLLightSource;
    CBBackground: TComboBox;
    BackGroundSprite: TGLHUDSprite;
    Cone: TGLCone;
    Teapot: TGLTeapot;
    World: TGLDummyCube;
    Light: TGLDummyCube;
    FireSphere: TGLSphere;
    GLMaterialLibrary: TGLMaterialLibrary;
    procedure CBObjectChange(Sender: TObject);
    procedure CBBackgroundChange(Sender: TObject);
    procedure SceneViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SceneViewerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

  private
    FLibMaterial: TGLAbstractLibMaterial;
    function GetMaterial: TGLMaterial;
    procedure SetMaterial(const Value: TGLMaterial);
    function GetLibMaterial: TGLAbstractLibMaterial;
    procedure SetLibMaterial(const Value: TGLAbstractLibMaterial);
    { Déclarations privées }
  public
    { Déclarations publiques }
    constructor Create(AOwner : TComponent); override;

    property Material : TGLMaterial read GetMaterial
      write SetMaterial;
    property LibMaterial : TGLAbstractLibMaterial read GetLibMaterial
      write SetLibMaterial;

  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R *.dfm}


uses
{$IFDEF GLS_DELPHI_XE2_UP}
  VCL.Graphics;
{$ELSE}
  Graphics;
{$ENDIF}

var
  MX, MY: Integer;

constructor TRMaterialPreview.Create(AOwner : TComponent);
begin
   inherited;
   BackGroundSprite.Position.X := SceneViewer.Width div 2;
   BackGroundSprite.Position.Y := SceneViewer.Height div 2;
   BackGroundSprite.Width := SceneViewer.Width;
   BackGroundSprite.Height := SceneViewer.Height;

   CBObject.ItemIndex:=0;     CBObjectChange(Self);
   CBBackground.ItemIndex:=0; CBBackgroundChange(Self);
end;

procedure TRMaterialPreview.CBObjectChange(Sender: TObject);
var
   i : Integer;
begin
   i:=CBObject.ItemIndex;
   Cube.Visible   := I = 0;
   Sphere.Visible := I = 1;
   Cone.Visible   := I = 2;
   Teapot.Visible := I = 3;
end;

procedure TRMaterialPreview.CBBackgroundChange(Sender: TObject);
var
   bgColor : TColor;
begin
   case CBBackground.ItemIndex of
      1 : bgColor:=clWhite;
      2 : bgColor:=clBlack;
      3 : bgColor:=clBlue;
      4 : bgColor:=clRed;
      5 : bgColor:=clGreen;
   else
      bgColor:=clNone;
   end;
   with BackGroundSprite.Material do begin
      Texture.Disabled:=(bgColor<>clNone);
      FrontProperties.Diffuse.Color:=ConvertWinColor(bgColor);
   end;
end;

procedure TRMaterialPreview.SceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in Shift) and (ssLeft in Shift) then
    Camera.AdjustDistanceToTarget(1 - 0.01 * (MY - Y))
  else
  if (ssRight in Shift) or (ssLeft in Shift) then
    Camera.MoveAroundTarget(Y - MY, X - MX);

  MX := X;
  MY := Y;
end;

procedure TRMaterialPreview.SceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MX := X;
  MY := Y;
end;

procedure TRMaterialPreview.SceneViewerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(1 - 0.1 * (Abs(WheelDelta) / WheelDelta));
end;

function TRMaterialPreview.GetMaterial: TGLMaterial;
begin
  Result := GLMaterialLibrary.Materials[0].Material;
end;

procedure TRMaterialPreview.SetMaterial(const Value: TGLMaterial);
begin
  GLMaterialLibrary.Materials[0].Material.Assign(Value.GetActualPrimaryMaterial);
end;

function TRMaterialPreview.GetLibMaterial: TGLAbstractLibMaterial;
begin
  Result := FLibMaterial;
end;

procedure TRMaterialPreview.SetLibMaterial(const Value: TGLAbstractLibMaterial);
begin
  FLibMaterial := Value;
  if Assigned(FLibMaterial) then
  begin
    with GLMaterialLibrary.Materials[0] do
    begin
      Material.MaterialLibrary := FLibMaterial.MaterialLibrary;
      Material.LibMaterialName := FLibMaterial.Name
    end;
  end
  else
  with GLMaterialLibrary.Materials[0] do
  begin
    Material.MaterialLibrary := nil;
    Material.LibMaterialName := '';
  end;
end;

end.



