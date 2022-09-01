{: Using Verlet Hair with ODE; Fur Balls<p>
}

unit fFurBall;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLLCLViewer, GLScene, GLObjects, GLCadencer, ODEImport,
  StdCtrls, GLTexture, GLExtrusion, GLVectorGeometry, GLShadowPlane, GLNavigator,
  GLVerletTypes, GLVerletHairClasses, GLKeyboard, ExtCtrls, ComCtrls,GLColor,
  GLCrossPlatform, GLCoordinates, GLBaseClasses;

const
  cMaxWindMag = 8;

type
  TfrmFurBall = class(TForm)
    GLCadencer1: TGLCadencer;
    GLScene1: TGLScene;
    DC_LightHolder: TGLDummyCube;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLSceneViewer1: TGLSceneViewer;
    GLShadowPlane_Floor: TGLShadowPlane;
    GLShadowPlane_Wall: TGLShadowPlane;
    Sphere1: TGLSphere;
    DCShadowCaster: TGLDummyCube;
    FurBall: TGLSphere;
    CheckBox_LockBall: TCheckBox;
    Label1: TLabel;
    CheckBox_FurGravity: TCheckBox;
    CheckBox_WindResistence: TCheckBox;
    GLShadowPlane_Floor2: TGLShadowPlane;
    GLLines1: TGLLines;
    GLShadowPlane_Wall2: TGLShadowPlane;
    GLShadowPlane_Wall3: TGLShadowPlane;
    CheckBox_Bald: TCheckBox;
    Label_FPS: TLabel;
    Timer1: TTimer;
    CheckBox_Shadows: TCheckBox;
    CheckBox_Inertia: TCheckBox;
    TrackBar_WindForce: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure DC_LightHolderProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure CheckBox_FurGravityClick(Sender: TObject);
    procedure CheckBox_WindResistenceClick(Sender: TObject);
    procedure CheckBox_BaldClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox_ShadowsClick(Sender: TObject);
    procedure CheckBox_InertiaClick(Sender: TObject);
    procedure TrackBar_WindForceChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    odeFurBallBody : PdxBody;
    odeFurBallGeom : PdxGeom;

    world : PdxWorld;
    space : PdxSpace;
    contactgroup : TdJointGroupID;

    VerletWorld : TVerletWorld;
    HairList : TList;
    VCSphere : TVCSphere;
    PhysicsTime : single;

    Gravity : TVFGravity;
    AirResistance : TVFAirResistance;

    procedure CreateBall;
    procedure CreateFur;
  end;

var
  frmFurBall: TfrmFurBall;


implementation

uses ODEGL, GLVerletClasses;

{$R *.lfm}

procedure nearCallback (data : pointer; o1, o2 : PdxGeom); cdecl;
const
  cCOL_MAX = 1;
var
  i, numc : integer;
  b1,b2 : PdxBody;
  contact : array[0..cCOL_MAX-1] of TdContact;
  c : TdJointID;
begin
  // exit without doing anything if the two bodies are connected by a joint
  b1 := dGeomGetBody(o1);
  b2 := dGeomGetBody(o2);
  if (Assigned(b1) and Assigned(b2) and (dAreConnected (b1,b2)<>0)) then
    exit;


  for i :=0 to cCOL_MAX-1 do
  begin
    contact[i].surface.mode := dContactBounce;

    // This determines friction, play around with it!
    contact[i].surface.mu := 3;//10e9; //dInfinity; SHOULD BE INFINITY!
    contact[i].surface.mu2 := 0;
    contact[i].surface.bounce := 0.5;//0.5;
    contact[i].surface.bounce_vel := 0.1;
  end;


  numc := dCollide (o1,o2,cCOL_MAX,contact[0].geom,sizeof(TdContact));
  if (numc>0) then
  begin
    for i := 0 to numc-1 do
    begin
      c := dJointCreateContact (frmFurBall.world,frmFurBall.contactgroup, @contact[i]);
      dJointAttach (c,b1,b2);
    end;
  end;
end;

const
  cOffset = 0.03;
procedure TfrmFurBall.FormCreate(Sender: TObject);
begin
  Show;

  Randomize;

  world := dWorldCreate();
  space := dHashSpaceCreate(nil);
  contactgroup := dJointGroupCreate (1000000);
  dWorldSetGravity (world,0,0,-9.81);

  CreateODEPlaneFromGLPlane(GLShadowPlane_Floor, space);
  CreateODEPlaneFromGLPlane(GLShadowPlane_Floor2, space);
  CreateODEPlaneFromGLPlane(GLShadowPlane_Wall, space);
  CreateODEPlaneFromGLPlane(GLShadowPlane_Wall2, space);
  CreateODEPlaneFromGLPlane(GLShadowPlane_Wall3, space);
  // dCreatePlane (space,0,0,1,0);

  VerletWorld := TVerletWorld.Create;
  VerletWorld.Iterations := 2;
  VerletWorld.VerletNodeClass := TGLVerletNode;

  CheckBox_FurGravityClick(Sender);
  CheckBox_WindResistenceClick(Sender);

  CreateVCPlaneFromGLPlane(GLShadowPlane_Floor, VerletWorld, cOffset);
  CreateVCPlaneFromGLPlane(GLShadowPlane_Floor2, VerletWorld, cOffset);
  CreateVCPlaneFromGLPlane(GLShadowPlane_Wall, VerletWorld, cOffset);
  CreateVCPlaneFromGLPlane(GLShadowPlane_Wall2, VerletWorld, cOffset);
  CreateVCPlaneFromGLPlane(GLShadowPlane_Wall3, VerletWorld, cOffset);

  HairList := TList.Create;

  CreateBall;
end;

procedure TfrmFurBall.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled := false;
  dJointGroupDestroy (contactgroup);
  dSpaceDestroy (space);
  dWorldDestroy (world);
end;

var
  angle : double=0;
procedure TfrmFurBall.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
const
  cTIME_STEP = 0.01;
var
  i,j : integer;
  Delta : single;
  Hair : TVerletHair;
  GLLines : TGLLines;
begin
  Delta := deltaTime;
  angle := angle + Delta*3;

  while PhysicsTime<newTime do
  begin
    PhysicsTime := PhysicsTime + cTIME_STEP;

    if not CheckBox_LockBall.Checked then
    begin
      dSpaceCollide (space,nil,nearCallback);
      dWorldStep (world, cTIME_STEP);//}
      // remove all contact joints
      dJointGroupEmpty (contactgroup);

      if IsKeyDown(VK_UP) then
        dBodyAddForce(odeFurBallBody, 0,0,2.5)

      else if IsKeyDown(VK_DOWN) then
        dBodyAddForce(odeFurBallBody, 0,0,-2.5);

      if IsKeyDown('A') then
        dBodyAddForce(odeFurBallBody, 0,-1,0)

      else if IsKeyDown('D') then
        dBodyAddForce(odeFurBallBody, 0,1,0);

      if IsKeyDown('W') then
        dBodyAddForce(odeFurBallBody, -1,0,0)

      else if IsKeyDown('S') then
        dBodyAddForce(odeFurBallBody, 1,0,0);
    end;

    PositionSceneObject(FurBall, odeFurBallGeom);
    VCSphere.Location := FurBall.Position.AsAffineVector;
    VerletWorld.Progress(cTIME_STEP, PhysicsTime);
  end;

  for i := 0 to HairList.Count -1 do
  begin
    Hair := TVerletHair(HairList[i]);
    GLLines := TGLLines(Hair.Data);
    for j := 1 to Hair.NodeList.Count-1 do
      GLLines.Nodes[j-1].AsAffineVector := Hair.NodeList[j].Location;
  end;
end;

procedure TfrmFurBall.DC_LightHolderProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  // DC_LightHolder.Roll(deltaTime*pi*2*8);
end;

var
  FoldMouseX : integer;
  FoldMouseY : integer;
procedure TfrmFurBall.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(FoldMouseY-Y, FoldMouseX-X);

  FoldMouseX := X;
  FoldMouseY := Y;
end;

procedure TfrmFurBall.CreateBall;
var
  m : TdMass;
begin
  dMassSetSphere (m,1,FurBall.Radius);

  odeFurBallGeom := dCreateSphere (space,FurBall.Radius);
  odeFurBallBody := dBodyCreate(World);

  dGeomSetBody (odeFurBallGeom,odeFurBallBody);
  dBodySetMass (odeFurBallBody, @m);
  dBodySetLinearVel(odeFurBallBody, 0, 14, 0);

  dBodyAddTorque(odeFurBallBody, 0.1,0.1,0.1);

  // Add the GLScene object
  odeFurBallGeom.Data:=FurBall;

  CopyPosFromGeomToGL(odeFurBallGeom, FurBall);

  VCSphere := TVCSphere.Create(VerletWorld);
  VCSphere.Radius := FurBall.Radius * 1.1;
  VCSphere.Location := AffineVectorMake(FurBall.AbsolutePosition);

  CreateFur;
end;

const
  cRadiusMultiplier = 5;
  cSegmentCount = 4;
  cHairCount = 200;
  cRootDepth = 4;
procedure TfrmFurBall.CreateFur;
  // Much, MUCH easier that uniform distribution, and it looks fun.
  procedure CreateRandomHair;
  var
    i : integer;
    Dir : TAffineVector;
    Hair : TVerletHair;
    GLLines : TGLLines;
  begin
    Dir := AffineVectorMake(random-0.5,random-0.5,random-0.5);
    NormalizeVector(Dir);

    Hair := TVerletHair.Create(VerletWorld, FurBall.Radius * cRootDepth,
      FurBall.Radius*cRadiusMultiplier, cSegmentCount,
      VectorAdd(AffineVectorMake(FurBall.AbsolutePosition), VectorScale(Dir, FurBall.Radius)),
      Dir, [vhsSkip1Node]);

    //GLLines := TGLLines(GLScene1.Objects.AddNewChild(TGLLines));
    GLLines := TGLLines(DCShadowCaster.AddNewChild(TGLLines));
    GLLines.NodesAspect := lnaInvisible;
    GLLines.LineWidth := 2;
    GLLines.LineColor.Color := clrBlack;

    for i := 0 to Hair.NodeList.Count-1 do
      TGLVerletNode(Hair.NodeList[i]).GLBaseSceneObject := FurBall;

    for i := 1 to Hair.NodeList.Count-1 do
       GLLines.AddNode(Hair.NodeList[i].Location);

    for i := 0 to GLLines.Nodes.Count-1 do
      TGLLinesNode(GLLines.Nodes[i]).Color.Color := clrBlack;

    GLLines.ObjectStyle:=GLLines.ObjectStyle+[osDirectDraw];
    GLLines.SplineMode := lsmCubicSpline;

    Hair.Data := GLLines;
    HairList.Add(Hair);
  end;
var
  Hair : TVerletHair;
  i : integer;
begin
  for i := 0 to HairList.Count-1 do
  begin
    Hair := TVerletHair(HairList[i]);
    TGLLines(Hair.Data).Free;
    Hair.Free;
  end;

  HairList.Clear;

  for i := 0 to cHairCount-1 do
    CreateRandomHair;
end;

procedure TfrmFurBall.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
	GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

procedure TfrmFurBall.CheckBox_FurGravityClick(Sender: TObject);
begin
  if not CheckBox_FurGravity.Checked then
    FreeAndNil(Gravity)
  else
  begin
    Gravity := TVFGravity.Create(VerletWorld);
    Gravity.Gravity := AffineVectorMake(0,0,-9.81);
  end;
end;

procedure TfrmFurBall.CheckBox_WindResistenceClick(Sender: TObject);
begin
  if not CheckBox_WindResistence.Checked then
    FreeAndNil(AirResistance)
  else
  begin
    AirResistance := TVFAirResistance.Create(VerletWorld);
    AirResistance.DragCoeff := 0.01;
    AirResistance.WindDirection := AffineVectorMake(1,0,0);
    AirResistance.WindMagnitude := TrackBar_WindForce.Position/100 * cMaxWindMag;
    AirResistance.WindChaos := 0.4;
  end;

  TrackBar_WindForce.Enabled := CheckBox_WindResistence.Checked;
end;

procedure TfrmFurBall.TrackBar_WindForceChange(Sender: TObject);
begin

  if Assigned(AirResistance) then
    AirResistance.WindMagnitude := TrackBar_WindForce.Position/100 * cMaxWindMag;
end;

procedure TfrmFurBall.CheckBox_BaldClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to HairList.Count -1 do
  begin
    with TVerletHair(HairList[i]) do
    begin
      Anchor.NailedDown := not CheckBox_Bald.Checked;
      Anchor.OldLocation := Anchor.Location;
      Root.NailedDown := not CheckBox_Bald.Checked;
      Root.OldLocation := Root.Location;
    end;
  end;

  if not CheckBox_Bald.Checked then
    VerletWorld.PauseInertia(5);
end;

procedure TfrmFurBall.Timer1Timer(Sender: TObject);
begin
  Label_FPS.Caption := GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TfrmFurBall.CheckBox_ShadowsClick(Sender: TObject);
var
  light : TGLLightSource;
begin
  if CheckBox_Shadows.Checked then
    light := GLLightSource1
  else
    light := nil;

  GLShadowPlane_Floor.ShadowedLight := light;
  GLShadowPlane_Floor2.ShadowedLight := light;
  GLShadowPlane_Wall.ShadowedLight := light;
  GLShadowPlane_Wall2.ShadowedLight := light;
  GLShadowPlane_Wall3.ShadowedLight := light;
end;

procedure TfrmFurBall.CheckBox_InertiaClick(Sender: TObject);
begin
  VerletWorld.Inertia := CheckBox_Inertia.Checked;
end;
end.
