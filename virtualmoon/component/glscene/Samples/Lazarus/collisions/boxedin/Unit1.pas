{: Basic Octree/Collision demo.<p>

Robert Hayes, March 2002.

This demo uses the new Octree class to quickly detect triangle-level collisions with a sphere.

See Octree.SphereIntersect() for detailed comments.

}

unit Unit1;

interface

uses
  Classes, SysUtils, Controls, Forms, GLKeyboard, GLVectorGeometry,
  GLScene, GLVectorFileObjects, GLObjects, GLLCLViewer,
  GLCadencer, ExtCtrls, StdCtrls, GLNavigator, ComCtrls, GLGeomObjects,
  GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    FreeForm1: TGLFreeForm;
    Sphere1: TGLSphere;
    ArrowLine1: TGLArrowLine;
    GLSceneViewer2: TGLSceneViewer;
    GLCamera2: TGLCamera;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    DummyCube2: TGLDummyCube;
    Sphere2: TGLSphere;
    GLLightSource2: TGLLightSource;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TrackBar1: TTrackBar;
    Button1: TButton;
    Lines1: TGLLines;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    colTotalTime: single; // for timing collision detection
    colCount: integer;
    procedure AddToTrail(const p: TVector);
  public
    { Public declarations }
    mousex, mousey: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses GLFile3DS, GLUtils, LCLType;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  FreeForm1.LoadFromFile(MediaPath+'BoxedIn.3ds');

  FreeForm1.BuildOctree;
  Label1.Caption := 'Octree Nodes    : ' + IntToStr(FreeForm1.Octree.NodeCount);
  Label2.Caption := 'Tri Count Octree: ' + IntToStr(FreeForm1.Octree.TriCountOctree);
  Label3.Caption := 'Tri Count Mesh  : ' + IntToStr(FreeForm1.Octree.TriCountMesh);

  Lines1.AddNode(0, 0, 0);
  Lines1.ObjectStyle := Lines1.ObjectStyle + [osDirectDraw];
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
var
  rayStart, rayVector: TVector;
  velocity: single;
  pPoint: TVector;
  pNormal: TVector;
  t: int64;
begin
  if IsKeyDown(VK_ESCAPE) then
    Close;

  Velocity := Trackbar1.Position * deltaTime * 50;

  t := StartPrecisionTimer;

  with FreeForm1 do
  begin
    SetVector(rayStart, Sphere2.AbsolutePosition);
    SetVector(rayVector, Sphere2.AbsoluteDirection);
    NormalizeVector(rayVector);
    //Note: since collision may be performed on multiple meshes, we might need to know which hit
    //      is closest (ie: d:=raystart - pPoint).
    if OctreeSphereSweepIntersect(raystart, rayvector, velocity,
      Sphere2.Radius, @pPoint, @pNormal) then
    begin
      // Show the polygon intersection point
      NormalizeVector(pNormal);
      Sphere1.Position.AsVector := pPoint;
      Sphere1.Direction.AsVector := pNormal;

      // Make it rebound...
      with Sphere2.Direction do
        AsAffineVector := VectorReflect(AsAffineVector, AffineVectorMake(pNormal));
      // Add some "english"...
      with Sphere2.Direction do
      begin
        X := x + random / 10;
        Y := y + random / 10;
        Z := z + random / 10;
      end;
      // Add intersect point to trail
      AddToTrail(pPoint);
    end
    else
    begin
      Sphere2.Move(velocity); //No collision, so just move the ball.
    end;
  end;
  // Last trail point is always the sphere's current position
  Lines1.Nodes.Last.AsVector := Sphere2.Position.AsVector;

  colTotalTime := colTotalTime + StopPrecisionTimer(t);
  Inc(colCount);
end;

procedure TForm1.AddToTrail(const p: TVector);
var
  i, k: integer;
begin
  Lines1.Nodes.Last.AsVector := p;
  Lines1.AddNode(0, 0, 0);
  if Lines1.Nodes.Count > 20 then // limit trail to 20 points
    Lines1.Nodes[0].Free;

  for i := 0 to 19 do
  begin
    k := Lines1.Nodes.Count - i - 1;
    if k >= 0 then
      TGLLinesNode(Lines1.Nodes[k]).Color.Alpha := 0.95 - i * 0.05;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  t: single;
begin
  if colCount > 0 then
    t := colTotalTime * 1000 / colCount
  else
    t := 0;
  Caption := Format('%.2f FPS - %.3f ms for collisions/frame',
    [GLSceneViewer2.FramesPerSecond, t]);
  GLSceneViewer2.ResetPerformanceMonitor;
  colTotalTime := 0;
  colCount := 0;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  //If the ball gets stuck in a pattern, hit the reset button.
  with Sphere2.Position do
  begin
    X := random;
    Y := random;
    Z := random;
  end;

  with Sphere2.Direction do
  begin
    X := random;
    if random > 0.5 then
      x := -x;
    Y := random;
    if random > 0.5 then
      y := -y;
    Z := random;
    if random > 0.5 then
      z := -z;
  end;
end;

end.

