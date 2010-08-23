{: GLODERagdoll<p>

	TGLRagdoll extended using Open Dynamics Engine (ODE). <p>

	<b>History :</b><font size=-1><ul>
    <li>09/11/05 - LucasG - Fixed joints to be relative to the body
    <li>07/11/05 - LucasG - Alignment (Using Stuart's AlignToMatrix function)
    <li>02/11/05 - LucasG - First version created.
  </ul></font>
}

unit GLODERagdoll;

interface

uses GLRagdoll, ODEImport, GLScene, GLObjects, VectorGeometry, ODEGL, GLTexture,
  GLVectorFileObjects;

const
  cDensity = 20;
  cMass = 1;
  cMaxContacts = 4;

type                       
  TODERagdoll = class;

  TODERagdollWorld = class
  private
    FSpace: PdxSpace;
    FWorld: PdxWorld;
    FContactGroup: TdJointGroupID;
    FRagdoll: TODERagdoll;

    isWorldCreated : Boolean; // NEW1

  public
    constructor Create;
    {: Create the world from any existing ODE world }
    constructor CreateFrom(World: PdxWorld; Space: PdxSpace; ContactGroup: TdJointGroupID);
    destructor Destroy; override;
    procedure WorldUpdate;
    property World: PdxWorld read FWorld;
    property Space: PdxSpace read FSpace;
    property ContactGroup: TdJointGroupID read FContactGroup;
    property Ragdoll: TODERagdoll read FRagdoll;
  end;

  TODERagdollDummyJoint = class(TRagdollJoint)
  end;


  TODERagdollHingeJoint = class(TRagdollJoint)
  private
    FParamHiStop: Single;
    FParamLoStop: Single;
    FAxis: TAffineVector;
  public
    constructor Create(Axis: TAffineVector;
      ParamLoStop: Single; ParamHiStop: Single);
    property Axis : TAffineVector read FAxis;
    property ParamLoStop: Single read FParamLoStop write FParamLoStop;
    property ParamHiStop: Single read FParamHiStop write FParamHiStop;
  end;

  TODERagdollUniversalJoint = class(TODERagdollHingeJoint)
  private
    FParamHiStop2: Single;
    FParamLoStop2: Single;
    FAxis2: TAffineVector;
  public
    constructor Create(Axis: TAffineVector; ParamLoStop: Single; ParamHiStop: Single;
                       Axis2: TAffineVector; ParamLoStop2: Single; ParamHiStop2: Single);
    property Axis2 : TAffineVector read FAxis2;
    property ParamLoStop2: Single read FParamLoStop2 write FParamLoStop2;
    property ParamHiStop2: Single read FParamHiStop2 write FParamHiStop2;
  end;

	TODERagdollBone = class (TRagdollBone)
  private
    FOwner: TODERagdollBone;
    FRagdoll: TODERagdoll;
    FBody: PdxBody;
    FGeom: PdxGeom;
    FJointId: Cardinal;
    procedure AlignBodyToMatrix(Mat: TMatrix);
  protected
    procedure Start; override;
    procedure Align; override;
    procedure Update; override;
    procedure Stop; override;
  public
    constructor CreateOwned(aOwner : TODERagdollBone);
    constructor Create(Ragdoll: TODERagdoll);
    property Body: PdxBody read FBody;
    property Geom: PdxGeom read FGeom;
  end;

  TODERagdoll = class(TGLRagdoll)
  private
    FODEWorld: TODERagdollWorld;
    FGLSceneRoot: TGLBaseSceneObject;
    FShowBoundingBoxes: Boolean;
  public
    constructor Create(AOwner : TGLBaseMesh);
    property ODEWorld: TODERagdollWorld read FODEWorld write FODEWorld;
    property GLSceneRoot: TGLBaseSceneObject read FGLSceneRoot write FGLSceneRoot;
    property ShowBoundingBoxes: Boolean read FShowBoundingBoxes write FShowBoundingBoxes;
  end;

implementation

{ TODERagdollWorld }

constructor TODERagdollWorld.Create;
begin
  //Create default physics
  FWorld := dWorldCreate();
  dWorldSetQuickStepNumIterations(FWorld, 8);
  FSpace := dHashSpaceCreate (nil);
  FContactGroup := dJointGroupCreate(0);
  dWorldSetGravity(FWorld, 0, 0, -0.81);
  dWorldSetCFM(FWorld, 1e-5);
  isWorldCreated := True; // NEW1
end;

constructor TODERagdollWorld.CreateFrom(World: PdxWorld; Space: PdxSpace;
  ContactGroup: TdJointGroupID);
begin
  FWorld := World;
  FSpace := Space;
  FContactGroup := ContactGroup;
  isWorldCreated := False; // NEW1
end;

destructor TODERagdollWorld.Destroy;
begin
  if isWorldCreated then begin
    dJointGroupDestroy (FContactGroup);
    dSpaceDestroy(FSpace);
    dWorldDestroy(FWorld);
  end;
  inherited;
end;

procedure ODERagdollCallback(data : pointer; o1, o2 : PdxGeom); cdecl;
var
 i,n     :integer;
 b1,b2   :PdxBody;
 c       :TdJointID;
 contact :Array[0..cMaxContacts-1] of TdContact;
begin
 b1:=dGeomGetBody(o1);
 b2:=dGeomGetBody(o2);

 if (assigned(b1) and assigned(b2) and (dAreConnected (b1,b2)<>0)) then exit;

 n:=dCollide(o1,o2,cMaxContacts,contact[0].geom,sizeof(TdContact));
 if (n > 0) then
 begin
  for i := 0 to n-1 do
  begin
    contact[i].surface.mode := ord(dContactBounce) or ord(dContactSoftCFM) or
      ord(dContactSlip1) or ord(dContactSlip2);
    contact[i].surface.mu := 10e9;
    contact[i].surface.mu2 := 0;
    contact[i].surface.soft_cfm := 0.001;
    contact[i].surface.bounce := 0.15;
    contact[i].surface.bounce_vel := 0.2;
    contact[i].surface.slip1 := 0.1;
    contact[i].surface.slip2 := 0.1;

   c:=dJointCreateContact(TODERagdollWorld(Data).World,TODERagdollWorld(Data).Contactgroup,@contact[i]);
   dJointAttach(c,dGeomGetBody(contact[i].geom.g1),dGeomGetBody(contact[i].geom.g2));
  end;
 end;

end;

procedure TODERagdollWorld.WorldUpdate;
const
  cDeltaTime = 1/50;
begin
  //Update the physic
  dSpaceCollide(FSpace, Self, ODERagdollCallback);
  dWorldQuickStep(FWorld, cDeltaTime);
  // remove all contact joints
  dJointGroupEmpty (FContactGroup);
end;

{ TODERagdollHingeJoint }

constructor TODERagdollHingeJoint.Create(Axis: TAffineVector;
  ParamLoStop, ParamHiStop: Single);
begin
  inherited Create;
  FAxis := Axis;
  FParamLoStop := ParamLoStop;
  FParamHiStop := ParamHiStop;
end;

{ TODERagdollUniversalJoint }

constructor TODERagdollUniversalJoint.Create(Axis: TAffineVector;
  ParamLoStop, ParamHiStop: Single; Axis2: TAffineVector; ParamLoStop2,
  ParamHiStop2: Single);
begin
  inherited Create(Axis, ParamLoStop, ParamHiStop);
  FAxis2 := Axis2;
  FParamLoStop := ParamLoStop;
  FParamHiStop := ParamHiStop;
end;


{ TODERagdollBone }

constructor TODERagdollBone.Create(Ragdoll: TODERagdoll);
begin
  inherited Create(Ragdoll);
  FRagdoll := Ragdoll;
end;

constructor TODERagdollBone.CreateOwned(aOwner: TODERagdollBone);
begin
  inherited CreateOwned(aOwner);
  FOwner := aOwner;
  FRagdoll := aOwner.FRagdoll;
end;

procedure TODERagdollBone.AlignBodyToMatrix(Mat:TMatrix); //By Stuart Gooding
var
  R : TdMatrix3;
begin
  if not Assigned(FBody) then exit;
  R[0]:=Mat[0][0]; R[1]:=Mat[1][0]; R[2]:= Mat[2][0]; R[3]:= 0;
  R[4]:=Mat[0][1]; R[5]:=Mat[1][1]; R[6]:= Mat[2][1]; R[7]:= 0;
  R[8]:=Mat[0][2]; R[9]:=Mat[1][2]; R[10]:=Mat[2][2]; R[11]:=0;
  dBodySetRotation(FBody,R);
  dBodySetPosition(FBody,Mat[3][0],Mat[3][1],Mat[3][2]);
end;

procedure TODERagdollBone.Start;
var
  mass: TdMass;
  boneSize, vAxis, vAxis2: TAffineVector;

  function RotateAxis(Axis: TAffineVector): TAffineVector;
  var absMat: TMatrix;
  begin
    absMat := ReferenceMatrix;
    absMat[3] := NullHmgVector;
    Result := VectorNormalize(VectorTransform(Axis, absMat));
  end;

begin
  FBody := dBodyCreate(FRagdoll.ODEWorld.World);

  boneSize[0] := Size[0]*VectorLength(BoneMatrix[0]);
  boneSize[1] := Size[1]*VectorLength(BoneMatrix[1]);
  boneSize[2] := Size[2]*VectorLength(BoneMatrix[2]);

  dMassSetBox(mass, cDensity, BoneSize[0], BoneSize[1], BoneSize[2]);
  dMassAdjust(mass, cMass);
  dBodySetMass(FBody, @mass);

  AlignBodyToMatrix(ReferenceMatrix);

  FGeom := dCreateBox(FRagdoll.ODEWorld.Space, BoneSize[0], BoneSize[1], BoneSize[2]);
  FGeom.data := FRagdoll.GLSceneRoot.AddNewChild(TGLCube);
  if (Joint is TODERagdollDummyJoint) then
    dGeomSetBody(FGeom, FOwner.Body)
  else
    dGeomSetBody(FGeom, FBody);

  if (Owner <> nil) then
  begin

    if (Joint is TODERagdollHingeJoint) then
    with (Joint as TODERagdollHingeJoint) do
    begin
      vAxis := RotateAxis(Axis);
      FJointId := dJointCreateHinge(FRagdoll.ODEWorld.World, 0);
      dJointAttach(FJointId, TODERagdollBone(Owner).Body, FBody);
      dJointSetHingeAnchor(FJointId, Anchor[0], Anchor[1], Anchor[2]);
      dJointSetHingeAxis (FJointId, vAxis[0], vAxis[1], vAxis[2]);
   	  dJointSetHingeParam (FJointId, dParamLoStop, ParamLoStop);
      dJointSetHingeParam (FJointId, dParamHiStop, ParamHiStop);
    end;

    if (Joint is TODERagdollUniversalJoint) then
    with (Joint as TODERagdollUniversalJoint) do
    begin
      vAxis := RotateAxis(Axis);
      vAxis2 := RotateAxis(Axis2);
      FJointId := dJointCreateUniversal(FRagdoll.ODEWorld.World, 0);
      dJointAttach(FJointId, FBody, TODERagdollBone(Owner).Body);
      dJointSetUniversalAnchor(FJointId, Anchor[0], Anchor[1], Anchor[2]);
      dJointSetUniversalAxis1(FJointId, vAxis[0], vAxis[1], vAxis[2]);
      dJointSetUniversalAxis2(FJointId, vAxis2[0], vAxis2[1], vAxis2[2]);
   	  dJointSetUniversalParam(FJointId, dParamLoStop, ParamLoStop);
   	  dJointSetUniversalParam(FJointId, dParamHiStop, ParamHiStop);
   	  dJointSetUniversalParam(FJointId, dParamLoStop2, ParamLoStop2);
   	  dJointSetUniversalParam(FJointId, dParamHiStop2, ParamHiStop2);
    end;


  end;

  with TGLCube(FGeom.data) do
  begin
    Visible := FRagdoll.ShowBoundingBoxes;
    Material.FrontProperties.Diffuse.SetColor(1,0,0,0.4);
    CubeWidth := BoneSize[0];
    CubeHeight := BoneSize[1];
    CubeDepth := BoneSize[2];
  end;

end;

procedure TODERagdollBone.Stop;
begin
  inherited;
  dBodyDestroy(FBody);
  if Assigned(FGeom.data) then
    FRagdoll.GLSceneRoot.Remove(TGLBaseSceneObject(FGeom.data), false);
  dGeomDestroy(FGeom);
end;

procedure TODERagdollBone.Update;
begin
  PositionSceneObject(TGLBaseSceneObject(PdxGeom(FGeom.data)), FGeom);
  RagDoll.Owner.Skeleton.BoneByID(BoneID).SetGlobalMatrixForRagDoll(TGLBaseSceneObject(PdxGeom(FGeom.data)).AbsoluteMatrix);
end;

procedure TODERagdollBone.Align;
begin
  inherited;
  AlignBodyToMatrix(BoneMatrix);
end;

{ TODERagdoll }

constructor TODERagdoll.Create(AOwner : TGLBaseMesh);
begin
  inherited Create(AOwner);
  FShowBoundingBoxes := False;
end;


end.
