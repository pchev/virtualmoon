{
  GLRagdoll Demo by Lucas Goraieb

  This demo shows how to use ragdolls in GLScene.
  To use it, you must set every bone of your model, otherwise some
  vertices won't be aligned to the ragdoll causing weird effects.
  Also this should not be the official demo, it really need some cleanup and
  some improvements, I hope someone do this soon.

  This demo is based on Dave Gravel's ODE Ragdoll.

  Version history:

  21-November-2009 - DaStr - Added to the CVS  
}

unit fRagdoll;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLLCLViewer, GLCadencer, ODEImport,
  GLShadowPlane, GLVectorGeometry, GLGeomObjects, ExtCtrls, ComCtrls,
  GLBitmapFont, GLWindowsFont, GLHUDObjects, GLKeyboard, GLVectorFileObjects,
  GLRagdoll, GLODERagdoll, GLTexture, GLMaterial, GLCoordinates,
  GLCrossPlatform, GLBaseClasses;

//Physic World ODE
type
  TWorld_ODE = class
    world: PdxWorld;
    space: PdxSpace;
    contactgroup: TdJointGroupID;
    ground_box : PdxGeom;
    ground_box2 : PdxGeom;
    cube: TGLCube;
    cube2: TGLCube;
    ODEEnable : boolean;
    physTime : double;
    destructor destroy; override;
    constructor create;
    procedure WorldUpdate;
  end;

//Form
type
  TfrmRagdoll = class(TForm)
    GLScene1: TGLScene;
    GLCadencer1: TGLCadencer;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    ODEScene: TGLDummyCube;
    GLShadowPlane1: TGLShadowPlane;
    Targetrag: TGLDummyCube;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    Actor1: TGLActor;
    GLMaterialLibrary1: TGLMaterialLibrary;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    my,mx: integer;
  public
    { Public declarations }
  end;

var
  frmRagdoll: TfrmRagdoll;
  WorldODE: TWorld_ODE;
  Rag: TODERagdoll;
  RagWorld: TODERagdollWorld;
  HeadBone, spine, torso, rightleg : TODERagdollBone;

implementation

{$R *.lfm}
uses ODEGL, GLFileSMD;

constructor TWorld_ODE.create;
var
  R : TdMatrix3;
begin
  ODEEnable := False;
  PhysTime := 0;
  //Create physic
  world := dWorldCreate();
  dWorldSetQuickStepNumIterations(world, 8);
  space := dHashSpaceCreate (nil);
  contactgroup := dJointGroupCreate ( 0 );
  dWorldSetGravity (world, 0, 0, -0.81);
  dWorldSetCFM (world, 1e-5);
  //Floor
  dCreatePlane (space, 0, 0, 1, 0);
  //Box wall limit
  dCreatePlane (space,  0, 1, 0, -50.00);
  dCreatePlane (space,  1, 0, 0, -50.00);
  dCreatePlane (space,  0,-1, 0, -50.00);
  dCreatePlane (space, -1, 0, 0, -50.00);
  // Create 1 GLSCube and a box space.
  ground_box := dCreateBox (space,25,50,50);
  dRFromAxisAndAngle (R,0,1,0,0.95);
  dGeomSetPosition (ground_box,32,5,0.5);
  dGeomSetRotation (ground_box,R);
  Cube := TGLCube(frmRagdoll.ODEScene.AddNewChild(TGLCube));
  PdxGeom(ground_box).data := Cube;
  CopyCubeSizeFromBox(Cube, ground_box);
  PositionSceneObject(TGLBaseSceneObject(PdxGeom(ground_box).data), ground_box);
  // Same Create 1 GLSCube and a box space.
  ground_box2 := dCreateBox (space,5,10,5);
  dRFromAxisAndAngle (R,0,1,0,0);
  dGeomSetPosition (ground_box2,-12,-5,2.5);
  dGeomSetRotation (ground_box2,R);
  Cube2 := TGLCube(frmRagdoll.ODEScene.AddNewChild(TGLCube));
  PdxGeom(ground_box2).data := Cube2;
  CopyCubeSizeFromBox(Cube2, ground_box2);
  PositionSceneObject(TGLBaseSceneObject(PdxGeom(ground_box2).data), ground_box2);

  // Create now a sphere
  ground_box2 := dCreateSphere (space,5);
  dGeomSetPosition (ground_box2,0,-15,2.5);
  PdxGeom(ground_box2).data := TGLSphere(frmRagdoll.ODEScene.AddNewChild(TGLSphere));
  TGLSphere(PdxGeom(ground_box2).data).Radius := 5;
  PositionSceneObject(TGLSphere(PdxGeom(ground_box2).data), ground_box2);
end;

destructor TWorld_ODE.Destroy;
begin
  //Destroy the physic
  dJointGroupDestroy (contactgroup);
  dSpaceDestroy (space);
  dWorldDestroy (world);
  inherited;
end;

procedure TWorld_ODE.WorldUpdate;
const
  cDeltaTime = 1/50;
begin
  physTime := physTime + cDeltaTime;

  RagWorld.WorldUpdate;
  frmRagdoll.GLSceneViewer1.Invalidate;
end;

  // NEW1
var
  rootBone : TODERagdollBone;
   hjoint: TODERagdollHingeJoint;
   JLeg: TODERagdollUniversalJoint;
   JTorso: TODERagdollUniversalJoint;
   JKnee: TODERagdollHingeJoint;
   JLArm,JRarm: TODERagdollHingeJoint;
   JShoulder: TODERagdollUniversalJoint;
   ujoint: TODERagdollUniversalJoint;
   JDummy: TODERagdollDummyJoint;

procedure TfrmRagdoll.FormCreate(Sender: TObject);
var // rootBone,
   tb: TODERagdollBone;
   {
   hjoint: TODERagdollHingeJoint;
   JLeg: TODERagdollUniversalJoint;
   JTorso: TODERagdollUniversalJoint;
   JKnee: TODERagdollHingeJoint;
   JLArm,JRarm: TODERagdollHingeJoint;
   JShoulder: TODERagdollUniversalJoint;
   ujoint: TODERagdollUniversalJoint;
   JDummy: TODERagdollDummyJoint;
   }

  function bone(oBone: TODERagdollBone; id: String; j: TGLRagdolJoint): TODERagdollBone;
  var b: TODERagdollBone;
  begin
    b := TODERagdollBone.CreateOwned(oBone);
    with b do
    begin
      Name := id;
      BoneID := Actor1.Skeleton.BoneByName('Bip01 '+id).BoneID;
      Joint := j;
    end;
    Result := b;
  end;

begin
  SetCurrentDirectory('..\..\media\');

  //Execute Create physic and RagdollPlayer
  WorldODE := TWorld_ODE.create;

  Actor1.LoadFromFile('trinityRage.smd');
  Actor1.Scale.SetVector(0.2,0.2,0.2);
  Actor1.AddDataFromFile('walk.smd');
  Actor1.Animations[1].MakeSkeletalTranslationStatic;

  Actor1.AddDataFromFile('jump.smd');
  Actor1.Animations[2].MakeSkeletalTranslationStatic;

  RagWorld := TODERagdollWorld.CreateFrom(WorldODE.world, WorldODE.space, WorldODE.contactgroup);

  Rag := TODERagdoll.Create(Actor1);
  Rag.ODEWorld := RagWorld;
  Rag.GLSceneRoot := GLScene1.Objects;
  Rag.ShowBoundingBoxes := False;

  hjoint := TODERagdollHingeJoint.Create(AffineVectorMake(0,0,1), -0.3, 0.3);
  JLeg := TODERagdollUniversalJoint.Create(AffineVectorMake(0,1,0), -1, 1, AffineVectorMake(0,0,1), 0, 2);
  JTorso := TODERagdollUniversalJoint.Create(AffineVectorMake(0,1,0), -1, 1, AffineVectorMake(0,0,1), -1, 1);
  JKnee := TODERagdollHingeJoint.Create(AffineVectorMake(0,0,-1), -2, 0);
  JLArm := TODERagdollHingeJoint.Create(AffineVectorMake(0,1,0), -0.1, 2);
  JRArm := TODERagdollHingeJoint.Create(AffineVectorMake(0,1,0), -2, 0.1);
  JShoulder := TODERagdollUniversalJoint.Create(AffineVectorMake(0,0,1), -0.5, 0.5, AffineVectorMake(-1,0,0), -1.5, 1.5);
  ujoint := TODERagdollUniversalJoint.Create(AffineVectorMake(0,1,0), -0.5, 0.5,
         AffineVectorMake(0,0,-1), -0.5, 0.5);
  JDummy := TODERagdollDummyJoint.Create;

  rootBone := TODERagdollBone.Create(Rag);
  rootBone.Joint := hjoint;
  rootBone.Name := 'Spine';
  rootBone.BoneID := Actor1.Skeleton.BoneByName('Bip01 Spine').BoneID;
  Rag.SetRootBone(rootBone);

  tb := TODERagdollBone.CreateOwned(rootBone);
  tb.Joint := JDummy;
  tb.Name := '0';
  tb.BoneID := Actor1.Skeleton.BoneByName('Bip01').BoneID;

  bone(rootbone, 'Pelvis', JDummy);
  torso := bone(rootbone, 'Spine1', JTorso);
  tb := bone(torso, 'Spine2', hjoint);
  spine := bone(tb, 'Spine3', hjoint);

  HeadBone := bone(spine, 'Head', ujoint);
  bone(spine, 'Neck', JDummy);


  tb := bone(spine, 'L Arm', JShoulder);
  tb := bone(tb, 'L Arm1', JLArm);
  tb := bone(tb, 'L Arm2', hjoint);
  tb := bone(tb, 'L Hand', hjoint);
  bone(tb, 'L Finger0', JDummy);
  bone(tb, 'L Finger01', JDummy);
  bone(tb, 'L Finger02', JDummy);
  bone(tb, 'L Finger1', JDummy);
  bone(tb, 'L Finger11', JDummy);
  bone(tb, 'L Finger12', JDummy);

  tb := bone(spine, 'R Arm', JShoulder);
  tb := bone(tb, 'R Arm1', JRArm);
  tb := bone(tb, 'R Arm2', hjoint);
  tb := bone(tb, 'R Hand', hjoint);
  bone(tb, 'R Finger0', JDummy);
  bone(tb, 'R Finger01', JDummy);
  bone(tb, 'R Finger02', JDummy);
  bone(tb, 'R Finger1', JDummy);
  bone(tb, 'R Finger11', JDummy);
  bone(tb, 'R Finger12', JDummy);

  tb := bone(rootbone, 'L Leg', JLeg);
  tb := bone(tb, 'L Leg1', JKnee);
  tb := bone(tb, 'L Foot', hjoint);
  bone(tb, 'L Toe0', JDummy);
  bone(tb, 'L Toe01', JDummy);
  bone(tb, 'L Toe02', JDummy);

  tb := bone(rootbone, 'R Leg', JLeg);
  rightleg := bone(tb, 'R Leg1', JKnee);
  tb := bone(rightleg, 'R Foot', hjoint);
  bone(tb, 'R Toe0', JDummy);
  bone(tb, 'R Toe01', JDummy);
  bone(tb, 'R Toe02', JDummy);

  Rag.BuildRagdoll;

  WorldODE.ODEEnable := True;

end;

procedure TfrmRagdoll.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  my := y;
  mx := x;
end;

procedure TfrmRagdoll.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssleft in shift then
  begin
    GLScene1.CurrentGLCamera.MoveAroundTarget(my-y,mx-x);
    my := y;
    mx := x;
  end;
end;

procedure TfrmRagdoll.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
  if WorldODE.ODEEnable then
  begin

    while WorldODE.physTime<newtime*10 do
    begin
      WorldODE.WorldUpdate;
      Rag.Update;
    end;

    if (Rag.Enabled) then
    begin
      if (IsKeyDown('w')) then
        dBodyAddForce(rightleg.Body, 0, 0, 100);
      if (IsKeyDown('s')) then
        dBodyAddForce(HeadBone.Body, 0, 0, 150);
      if (IsKeyDown('a')) then
        dBodyAddForce(HeadBone.Body, 0, 100, 0);
      if (IsKeyDown('d')) then
        dBodyAddForce(HeadBone.Body, 0, -100, 0);
      if (IsKeyDown('q')) then
        dBodyAddForce(HeadBone.Body, 100, 0, 0);
      if (IsKeyDown('e')) then
        dBodyAddForce(HeadBone.Body, -100, 0, 0);
      if (IsKeyDown('c')) then
      begin
        Rag.Stop;
        Actor1.AnimationMode := aamLoop;
      end;
    end;
    if (IsKeyDown('x')) then
    begin
      Rag.Start;
      Actor1.AnimationMode := aamNone;
    end;
    if (IsKeyDown(VK_LEFT)) then
      Actor1.Roll(deltaTime * -130);
    if (IsKeyDown(VK_RIGHT)) then
      Actor1.Roll(deltaTime * 130);
    if (IsKeyDown(VK_UP)) then
      Actor1.Lift(deltaTime * -20);
    if (IsKeyDown(VK_DOWN)) then
      Actor1.Lift(deltaTime * 20);
  end;
end;

procedure TfrmRagdoll.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //Execute Destroy Physic RagdollPlayer
  GLCadencer1.Enabled := False;
  WorldODE.ODEEnable := False;

   // NEW1
  Rag.Stop;

  rootBone.Free;
  hjoint.Free;
  JLeg.Free;
  JTorso.Free;
  JKnee.Free;
  JLArm.Free;
  JRarm.Free;
  JShoulder.Free;
  ujoint.Free;
  JDummy.Free;
   // NEW1 end.

  Rag.Destroy;

  RagWorld.Free; // NEW1


  WorldODE.destroy;
end;

procedure TfrmRagdoll.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  //Mouse wheel zoom + -
  GLScene1.CurrentGLCamera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TfrmRagdoll.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
 cForce = 3000;
begin
  if (Key = VK_F1) then
  begin
    Actor1.SwitchToAnimation(0);
    Actor1.Position.z := 0;
  end;
  if (Key = VK_F2) then
  begin
    Actor1.SwitchToAnimation('walk');
    Actor1.Position.z := 7;
  end;
  if (Key = VK_F3) then
  begin
    Actor1.SwitchToAnimation('jump');
    Actor1.Position.z := 7;
  end;

  if (Key = VK_RETURN) then
  begin
    Randomize;
    if not (Rag.Enabled) then Rag.Start;
    dBodyAddForce(HeadBone.Body, random(cForce)-random(cForce), random(cForce)-random(cForce), random(cForce)-random(cForce)+1500);
    dBodyAddForce(Torso.Body, random(cForce)-random(cForce),random(cForce)-random(cForce),random(cForce)-random(cForce)+800);
  end;

  if (Key = VK_F5) then
    Rag.ShowBoundingBoxes := not Rag.ShowBoundingBoxes;

end;

end.
