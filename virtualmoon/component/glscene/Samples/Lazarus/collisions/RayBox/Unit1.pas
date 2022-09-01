{: RayCastBoxIntersect example demo <p>

  History:
  22/01/07 - DaStr - Initial version (by dikoe Kenguru)
 }
unit Unit1;

{$MODE Delphi}

interface

uses
  //VCL
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls,
  //GLScene
  GLScene, GLObjects, GLCadencer, GLVectorFileObjects,
  GLLCLViewer, GLTexture, GLVectorGeometry, GLCrossPlatform, GLMaterial,
  GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    Viewer: TGLSceneViewer;
    GLScene: TGLScene;
    GLCadencer: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DCCamTarg: TGLDummyCube;
    Timer1: TTimer;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLLightSource2: TGLLightSource;
    Panel1: TPanel;
    Button1: TButton;
    GLLines1: TGLLines;
    GLPoints1: TGLPoints;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    GLCube1: TGLCube;
    CheckBox2: TCheckBox;
    GLDummyCube1: TGLDummyCube;
    DCCube1: TGLDummyCube;
    procedure GLCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    mdx, mdy : Integer;
  public
  end;

var
  BoxPos, BoxScale,
  RayStart, RayDir : TAffineVector;
  Form1 : TForm1;

implementation

{$R *.lfm}


procedure TForm1.Button1Click(Sender: TObject);
var
  iPnt, afScale : TAffineVector;
begin
    // Change pos.
  if CheckBox2.Checked then begin
    BoxPos   := AffineVectorMake(Random*2 -1, Random *2 -1, Random *2 -1);
    DCCamTarg.Position.SetPoint(BoxPos);

    BoxScale := AffineVectorMake(Random*1 +0.5, Random*1 +0.5, Random*1 +0.5);
    DCCube1.Scale.SetVector(BoxScale);
    afScale := VectorScale(BoxScale, 0.5);

    RayStart := AffineVectorMake(Random*3 -1.5, Random *3 -1.5, Random *3 -1.5);
  end;
  RayDir := AffineVectorMake(Random*2 -1, Random *2 -1, Random *2 -1);
  NormalizeVector(RayDir);

  GLLines1.Nodes.Clear;
  GLLines1.Nodes.AddNode(RayStart);
  GLLines1.Nodes.AddNode( VectorAdd(RayStart, VectorScale(RayDir, 8)) );
  GLPoints1.Positions.Clear;
  GLPoints1.Positions.Add(RayStart);
  GLPoints1.Positions.Add(BoxPos);
  GLPoints1.Positions.Add(VectorSubtract(BoxPos, afScale));
  GLPoints1.Positions.Add(VectorAdd(     BoxPos, afScale));

  if RayCastBoxIntersect(
    RayStart, RayDir,
    VectorSubtract(BoxPos, afScale),
    VectorAdd(     BoxPos, afScale),
    @iPnt)
  then begin
    Label1.Caption :=
       Format('Intersect point: %.3f %.3f %.3f', [iPnt.V[0], iPnt.V[1], iPnt.V[2]]);
    GLPoints1.Positions.Add(iPnt);
    beep;
  end else begin
    Label1.Caption := 'no intersection';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  RayStart := AffineVectorMake(Random*2 -1, Random *2 -1, Random *2 -1);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  GLCube1.Visible := CheckBox1.Checked;
end;

procedure TForm1.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if Form1.Active then Viewer.Invalidate
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.1f FPS', [Viewer.FramesPerSecond]);
  Viewer.ResetPerformanceMonitor;
end;

procedure TForm1.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Shift = [ssLeft] then GLCamera1.MoveAroundTarget(mdy -y, mdx -x);
  mdx := x;
  mdy := y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.02, WheelDelta/120));
end;

procedure TForm1.ViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Viewer.SetFocus;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  GLCamera1.FocalLength := MinInteger(Height, Width) / 10;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then close;
end;

end.
