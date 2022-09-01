{
 Demo showing GLTrail object
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLCadencer, GLScene, GLObjects, GLGeomObjects,
  GLLCLViewer, GLTrail, GLTexture, GLVectorgeometry, StdCtrls,
  ComCtrls, ExtCtrls, GLMesh, GLVectorTypes, GLCrossPlatform, GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    GLSphere1: TGLSphere;
    Room: TGLSphere;
    GLLightSource1: TGLLightSource;
    Panel1: TPanel;
    TrackBarSpeed: TTrackBar;
    Label1: TLabel;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my: integer;
    Trail: TGLTrail;
    Direction: TVector3f;
    LastTimeDirectionChange: double;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  Direction := VectorScale(AffineVectormake(random - 0.5, random - 0.5, random - 0.5), 0.5);
  with TGLTrail(GLScene1.Objects.AddNewChild(TGLTrail)) do
  begin
    TrailObject := GLSphere1;     // Trail will follow this sphere
    TimeLimit := 0.2;           // 0.2 seconds trail
    VertLimit := 200;           // max 200 vertices for trail
    MinDistance := 0.1;
    // minimal distance before adding a segment to the trail
    MarkStyle := msFaceCamera;
    // Trail will be facing the camera (ideal for bullets trails or similar)
    MarkWidth := 0.5;
    Material.FrontProperties.Diffuse.AsWinColor := clWhite;
  end;
end;


procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin

  GLSphere1.Position.AsAffineVector :=
    VectorAdd(GLSphere1.Position.AsAffineVector, VectorScale(
    Direction, TrackBarSpeed.Position));

  // Keep sphere in place
  if VectorLength(GLSphere1.Position.AsAffineVector) > 6 then
  begin
    // move to previous position
    GLSphere1.Position.AsAffineVector :=
      VectorSubtract(GLSphere1.Position.AsAffineVector, VectorScale(
      Direction, TrackBarSpeed.Position));
    // set opposite direction
    Direction := VectorNegate(Direction);
    // Add some randomness
    Direction := VectorScale(AffineVectormake(random - 0.5, random - 0.5, random - 0.5),
      deltatime * 10);
  end;

  GLSceneViewer1.Invalidate;
end;


procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  mx := x;
  my := y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(my - y, mx - x);
  end;

  mx := x;
  my := y;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := IntToStr(Round(GLSceneViewer1.FramesPerSecond));
  GLSceneViewer1.ResetPerformanceMonitor;
end;


end.
