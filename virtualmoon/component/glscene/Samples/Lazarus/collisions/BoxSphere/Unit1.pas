{
  SphereBoxIntersect Demo

  History:
  29/01/07 - DaStr - Initial version (by dikoe Kenguru)
}
unit Unit1;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLCadencer, GLVectorFileObjects, ExtCtrls,
  StdCtrls, GLLCLViewer, GLVectorGeometry, GLGraph, GLGeomObjects, Spin,
  ComCtrls, GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    Viewer: TGLSceneViewer;
    GLScene: TGLScene;
    GLCadencer: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLLightSource2: TGLLightSource;
    DCCamTarget: TGLDummyCube;
    Panel2: TPanel;
    GLCube1: TGLCube;
    GLXYZGrid1: TGLXYZGrid;
    GLLines1: TGLLines;
    CheckBox06: TCheckBox;
    CheckBox04: TCheckBox;
    CheckBox05: TCheckBox;
    GLSphere1: TGLSphere;
    DCCube1: TGLDummyCube;
    CheckBox07: TCheckBox;
    Label5: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    UpDown3: TUpDown;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    UpDown4: TUpDown;
    UpDown5: TUpDown;
    UpDown6: TUpDown;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    UpDown7: TUpDown;
    UpDown8: TUpDown;
    UpDown9: TUpDown;
    Label1: TLabel;
    Edit10: TEdit;
    UpDown10: TUpDown;
    Label7: TLabel;
    Button3: TButton;
    Button4: TButton;
    GLLines3: TGLLines;
    GLSphere2: TGLSphere;
    procedure GLCadencerProgress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure CheckBox04Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    mdx, mdy: Integer;
    intersPoint, ResNormal, BoxScale, SpherePos: TAffineVector;
    BoxMatrix: TMatrix;
    SphereRadius: Single;
  end;

// Generates random rotation for matrix. It remains a scale.
function RandomRotation(const aMatrix: TMatrix): TMatrix;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  BoxScale := XYZVector;
  SphereRadius := 1;
  BoxMatrix := IdentityHmgMatrix;
end;

procedure TForm1.CheckBox04Click(Sender: TObject);
begin
  GLCube1.Visible := CheckBox04.Checked;
  GLLines1.Visible := CheckBox05.Checked;
  GLXYZGrid1.Visible := CheckBox06.Checked;
  GLSphere1.Visible := CheckBox07.Checked;
end;

procedure TForm1.Edit1Change(Sender: TObject);
const
  EditorsScale = 0.1;
var
  Res1: Boolean;
begin
  if not Form1.Visible then
    Exit;
  GLLines3.Nodes.Clear;

  // Calc data.
  BoxMatrix.V[3].V[0] := UpDown1.Position * EditorsScale;
  BoxMatrix.V[3].V[1] := UpDown2.Position * EditorsScale;
  BoxMatrix.V[3].V[2] := UpDown3.Position * EditorsScale;
  BoxMatrix.V[3].V[3] := 1;

  BoxScale.V[0] := UpDown4.Position * EditorsScale;
  BoxScale.V[1] := UpDown5.Position * EditorsScale;
  BoxScale.V[2] := UpDown6.Position * EditorsScale;

  SpherePos.V[0] := UpDown7.Position * EditorsScale;
  SpherePos.V[1] := UpDown8.Position * EditorsScale;
  SpherePos.V[2] := UpDown9.Position * EditorsScale;

  SphereRadius := UpDown10.Position * EditorsScale;


  // dCollideSphereBox function !
  Res1 := IntersectSphereBox(VectorMake(SpherePos, 1), SphereRadius, BoxMatrix,
    BoxScale, @intersPoint, @ResNormal);

  if Res1 then
  begin
    // Intersected.
    Label1.Caption := 'Intersected';
    DCCamTarget.Position.SetPoint(intersPoint);

    // Draw normal
    GLLines3.Nodes.AddNode(intersPoint);
    GLLines3.Nodes.AddNode(VectorAdd(intersPoint, VectorScale(
      ResNormal, SphereRadius * 3)));
  end
  else
  begin
    // Not intersected.
    beep;
    Label1.Caption := '';

  end;
  DCCamTarget.Visible := Res1;

  // Draw GLCube1 and GLSphere1.
  GLCube1.Matrix := BoxMatrix;
  GLCube1.CubeWidth := BoxScale.V[0];
  GLCube1.CubeHeight := BoxScale.V[1];
  GLCube1.CubeDepth := BoxScale.V[2];
  DCCube1.Matrix := GLCube1.Matrix;
  DCCube1.Scale.SetVector(BoxScale);
  GLSphere1.Position.SetPoint(SpherePos);
  GLSphere1.Radius := SphereRadius;
  GLSphere2.Position.SetPoint(SpherePos);
  GLSphere2.Radius := SphereRadius;
end;

// Recalc.
procedure TForm1.Button3Click(Sender: TObject);
begin
  Edit1Change(Self);
end;

// Generates random rotation for matrix. It remains a scale.
function RandomRotation(const aMatrix: TMatrix): TMatrix;
var
  aScale: TAffineVector;
  I:      Integer;
begin
  // Save scale.
  for I := 0 to 2 do
    aScale.V[I] := VectorLength(aMatrix.V[I]);
  // Generate two not equal random vectors.
  Result.V[3] := aMatrix.V[3];
  repeat
    repeat
      Result.V[0] := VectorMake(Random * 2 - 1, Random * 2 - 1, Random * 2 - 1);
    until VectorNorm(Result.V[0]) > 10e-6;
    repeat
      Result.V[1] := VectorMake(Random * 2 - 1, Random * 2 - 1, Random * 2 - 1);
    until VectorNorm(Result.V[1]) > 10e-6;
  until VectorNorm(VectorSubtract(Result.V[0], Result.V[1])) > 10e-6;
  // Calculate two perpendicular vectors.
  Result.V[2] := VectorCrossProduct(Result.V[0], Result.V[1]);
  Result.V[1] := VectorCrossProduct(Result.V[0], Result.V[2]);
  // Restore scale.
  for I := 0 to 2 do
  begin
    NormalizeVector(Result.V[I]);
    ScaleVector(Result.V[I], aScale.V[I]);
  end;
end;

// Random matrix.
procedure TForm1.Button4Click(Sender: TObject);
begin
  BoxMatrix := RandomRotation(BoxMatrix);
  Edit1Change(Self);
end;

procedure TForm1.ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Viewer.SetFocus;
end;

procedure TForm1.GLCadencerProgress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  if Form1.Active then
    Viewer.Invalidate;
end;

procedure TForm1.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Shift = [ssLeft] then
    GLCamera1.MoveAroundTarget(mdy - Y, mdx - X);
  mdx := X;
  mdy := Y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Viewer.Focused then
    GLCamera1.AdjustDistanceToTarget(Power(1.02, WheelDelta / 120));
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  GLCamera1.FocalLength := MinInteger(Height, Width) / 10;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
end;

end.

