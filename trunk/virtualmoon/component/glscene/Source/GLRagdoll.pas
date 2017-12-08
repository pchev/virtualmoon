//
// This unit is part of the GLScene Project, http://glscene.org
//
{
    Base abstract ragdoll class. Should be extended to use any physics system. 

	 History : 
     10/11/12 - PW - Added CPP compatibility: changed vector arrays to arrays of records
     23/08/10 - Yar - Added GLVectorTypes to uses
     09/11/05 - LucasG - Fixed joint and few small things
     07/11/05 - LucasG - Fixed bone position and rotation (Align to animation)
     02/11/05 - LucasG - First version created.
   
}

unit GLRagdoll;

interface

uses
  GLScene, GLPersistentClasses, GLVectorGeometry, GLVectorFileObjects,
  GLVectorLists, GLObjects;

type
  TGLRagdoll = class;
  TGLRagdolBone = class;

  TGLRagdolJoint = class
  end;

  TGLRagdolBoneList = class (TPersistentObjectList)
  private
     
     FRagdoll : TGLRagdoll;
  protected
     
    function GetRagdollBone(Index: Integer) : TGLRagdolBone;
  public
     
    constructor Create(Ragdoll: TGLRagdoll); reintroduce;
    destructor Destroy; override;

    procedure WriteToFiler(writer : TVirtualWriter); override;
    procedure ReadFromFiler(reader : TVirtualReader); override;

    property Ragdoll : TGLRagdoll read FRagdoll;
    property Items[Index: Integer] : TGLRagdolBone read GetRagdollBone; default;
	end;

	TGLRagdolBone = class (TGLRagdolBoneList)
  private
     
    FOwner : TGLRagdolBoneList;
    FName : String;
    FBoneID : Integer; //Refering to TGLActor Bone
    FBoundMax: TAffineVector;
    FBoundMin: TAffineVector;
    FBoundBoneDelta: TAffineVector; //Stores the diference from the bone.GlobalMatrix to the center of the bone's bounding box
    FOrigin: TAffineVector;
    FSize: TAffineVector;
    FBoneMatrix: TMatrix;
    FJoint: TGLRagdolJoint;
    FOriginalMatrix: TMatrix; //Stores the Bone.GlobalMatrix before the ragdoll start
    FReferenceMatrix: TMatrix; //Stores the first bone matrix to be used as reference
    FAnchor: TAffineVector; //The position of the joint
    procedure CreateBoundingBox;
    procedure SetAnchor(Anchor: TAffineVector);
    procedure AlignToSkeleton;
    procedure CreateBoundsChild;
    procedure StartChild;
    procedure AlignChild;
    procedure UpdateChild;
    procedure StopChild;
  protected
     
    function GetRagdollBone(Index: Integer) : TGLRagdolBone;
    procedure Start; virtual; abstract;
    procedure Align; virtual; abstract;
    procedure Update; virtual; abstract;
    procedure Stop; virtual; abstract;
  public
     
    constructor CreateOwned(aOwner : TGLRagdolBoneList);
    constructor Create(Ragdoll: TGLRagdoll);
    destructor Destroy; override;

    procedure WriteToFiler(writer : TVirtualWriter); override;
    procedure ReadFromFiler(reader : TVirtualReader); override;

    property Owner : TGLRagdolBoneList read FOwner;
    property Name : String read FName write FName;
    property BoneID : Integer read FBoneID write FBoneID;
    property Origin : TAffineVector read FOrigin;
    property Size : TAffineVector read FSize;
    property BoneMatrix : TMatrix read FBoneMatrix;
    property ReferenceMatrix : TMatrix read FReferenceMatrix;
    property Anchor : TAffineVector read FAnchor;
    property Joint : TGLRagdolJoint read FJoint write FJoint;
    property Items[Index: Integer] : TGLRagdolBone read GetRagdollBone; default;
	end;

  TGLRagdoll = class(TPersistentObject)
	private
     
    FOwner : TGLBaseMesh;
    FRootBone : TGLRagdolBone;
    FEnabled: Boolean;
    FBuilt: Boolean;
  protected
     
  public
     
    constructor Create(AOwner : TGLBaseMesh); reintroduce;
    destructor Destroy; override;

    procedure WriteToFiler(writer : TVirtualWriter); override;
    procedure ReadFromFiler(reader : TVirtualReader); override;

    { Must be set before build the ragdoll }
    procedure SetRootBone(RootBone: TGLRagdolBone);
    { Create the bounding box and setup the ragdoll do be started later }
    procedure BuildRagdoll;

    procedure Start;
    procedure Update;
    procedure Stop;

    property Owner : TGLBaseMesh read FOwner;
    property RootBone : TGLRagdolBone read FRootBone;
    property Enabled : Boolean read FEnabled;
	end;

implementation

uses
  GLVectorTypes;

{ TGLRagdolBoneList }

constructor TGLRagdolBoneList.Create(Ragdoll: TGLRagdoll);
begin
  inherited Create;
  FRagdoll := Ragdoll;
end;

destructor TGLRagdolBoneList.Destroy;
var i: integer;
begin
  for i:=0 to Count-1 do Items[i].Destroy;
  inherited;
end;

function TGLRagdolBoneList.GetRagdollBone(Index: Integer): TGLRagdolBone;
begin
  Result:=TGLRagdolBone(List^[Index]);
end;

procedure TGLRagdolBoneList.ReadFromFiler(reader: TVirtualReader);
begin
  inherited;
  //Not implemented
end;

procedure TGLRagdolBoneList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited;
  //Not implemented
end;

{ TGLRagdolBone }

constructor TGLRagdolBone.Create(Ragdoll: TGLRagdoll);
begin
  inherited Create(Ragdoll);
end;

procedure TGLRagdolBone.CreateBoundingBox;
var
  bone: TGLSkeletonBone;
  i, j: integer;
  BoneVertices : TAffineVectorList;
  BoneVertex, max,min: TAffineVector;
  invMat, mat: TMatrix;
begin
  bone := Ragdoll.Owner.Skeleton.BoneByID(FBoneID);

  //Get all vertices weighted to this bone
  BoneVertices:=TAffineVectorList.Create;
  for i:=0 to Ragdoll.Owner.MeshObjects.Count-1 do
  with TGLSkeletonMeshObject(Ragdoll.Owner.MeshObjects[i]) do
    for j:=0 to Vertices.Count-1 do
      if bone.BoneID = VerticesBonesWeights[j][0].BoneID then
        BoneVertices.FindOrAdd(Vertices[j]);

  invMat := bone.GlobalMatrix;
  InvertMatrix(invMat);

  //For each vertex, get the max and min XYZ (Bounding box)
  if BoneVertices.Count > 0 then
  begin
    BoneVertex := VectorTransform(BoneVertices[0], invMat);
    max := BoneVertex;
    min := BoneVertex;
    for i:=1 to BoneVertices.Count-1 do begin
      BoneVertex := VectorTransform(BoneVertices[i], invMat);
      if (BoneVertex.V[0] > max.V[0]) then max.V[0] := BoneVertex.V[0];
      if (BoneVertex.V[1] > max.V[1]) then max.V[1] := BoneVertex.V[1];
      if (BoneVertex.V[2] > max.V[2]) then max.V[2] := BoneVertex.V[2];

      if (BoneVertex.V[0] < min.V[0]) then min.V[0] := BoneVertex.V[0];
      if (BoneVertex.V[1] < min.V[1]) then min.V[1] := BoneVertex.V[1];
      if (BoneVertex.V[2] < min.V[2]) then min.V[2] := BoneVertex.V[2];
    end;

    FBoundMax := max;
    FBoundMin := min;
    //Get the origin and subtract from the bone matrix
    FBoundBoneDelta := VectorScale(VectorAdd(FBoundMax, FBoundMin), 0.5);
  end else begin
    FBoundMax := NullVector;
    FBoundMin := NullVector;
  end;

  AlignToSkeleton;
  FReferenceMatrix := FBoneMatrix;
  mat := MatrixMultiply(bone.GlobalMatrix,FRagdoll.Owner.AbsoluteMatrix);
  //Set Joint position
  SetAnchor(AffineVectorMake(mat.V[3]));

  BoneVertices.Free; // NEW1
end;

constructor TGLRagdolBone.CreateOwned(aOwner: TGLRagdolBoneList);
begin
	Create(aOwner.Ragdoll);
  FOwner:=aOwner;
  aOwner.Add(Self);
end;

destructor TGLRagdolBone.Destroy;
begin
  inherited;
end;

procedure TGLRagdolBone.AlignToSkeleton;
var
  o: TAffineVector;
  bone: TGLSkeletonBone;
  mat, posMat: TMatrix;
  noBounds: Boolean;
begin
  bone := Ragdoll.Owner.Skeleton.BoneByID(FBoneID);
  noBounds := VectorIsNull(FBoundMax) and VectorIsNull(FBoundMin);
  //Get the bone matrix relative to the Actor matrix
  mat := MatrixMultiply(bone.GlobalMatrix,FRagdoll.Owner.AbsoluteMatrix);
  //Set Rotation
  FBoneMatrix := mat;
  NormalizeMatrix(FBoneMatrix);

  if (noBounds) then
  begin
    FOrigin := AffineVectorMake(mat.V[3]);
    FSize := AffineVectorMake(0.1,0.1,0.1);
  end else begin
    //Set Origin
    posMat := mat;
    posMat.V[3] := NullHmgVector;
    o := VectorTransform(FBoundBoneDelta, posMat);
    FOrigin := VectorAdd(AffineVectorMake(mat.V[3]), o);
    //Set Size
    FSize := VectorScale(VectorSubtract(FBoundMax, FBoundMin),0.9);
    FSize.V[0] := FSize.V[0]*VectorLength(mat.V[0]);
    FSize.V[1] := FSize.V[1]*VectorLength(mat.V[1]);
    FSize.V[2] := FSize.V[2]*VectorLength(mat.V[2]);
  end;
  //Put the origin in the BoneMatrix
  FBoneMatrix.V[3] := VectorMake(FOrigin,1);
end;

function TGLRagdolBone.GetRagdollBone(Index: Integer): TGLRagdolBone;
begin
  Result:=TGLRagdolBone(List^[Index]);
end;

procedure TGLRagdolBone.ReadFromFiler(reader: TVirtualReader);
begin
  inherited;

end;

procedure TGLRagdolBone.StartChild;
var i: integer;
begin
  FOriginalMatrix := Ragdoll.Owner.Skeleton.BoneByID(FBoneID).GlobalMatrix;
  AlignToSkeleton;
  Start;
  for i := 0 to Count-1 do items[i].StartChild;
end;

procedure TGLRagdolBone.UpdateChild;
var i: integer;
begin
  Update;
  for i := 0 to Count-1 do items[i].UpdateChild;
end;

procedure TGLRagdolBone.WriteToFiler(writer: TVirtualWriter);
begin
  inherited;

end;

procedure TGLRagdolBone.StopChild;
var i: integer;
begin
  Stop;
  Ragdoll.Owner.Skeleton.BoneByID(FBoneID).SetGlobalMatrix(FOriginalMatrix);
  for i := 0 to Count-1 do items[i].StopChild;
end;

procedure TGLRagdolBone.CreateBoundsChild;
var i: integer;
begin
  CreateBoundingBox;
  for i := 0 to Count-1 do items[i].CreateBoundsChild;
end;

procedure TGLRagdolBone.SetAnchor(Anchor: TAffineVector);
begin
  FAnchor := Anchor;
end;

procedure TGLRagdolBone.AlignChild;
var i: integer;
begin
  Align;
  Update;
  for i := 0 to Count-1 do items[i].AlignChild;
end;

{ TGLRagdoll }

constructor TGLRagdoll.Create(AOwner : TGLBaseMesh);
begin
  FOwner := AOwner;
  FEnabled := False;
  FBuilt := False;
end;

destructor TGLRagdoll.Destroy;
begin
  if FEnabled then Stop;
  inherited Destroy;
end;

procedure TGLRagdoll.ReadFromFiler(reader: TVirtualReader);
begin
  inherited;
end;

procedure TGLRagdoll.SetRootBone(RootBone: TGLRagdolBone);
begin
  FRootBone := RootBone;
end;

procedure TGLRagdoll.Start;
begin
  Assert(FBuilt, 'First you need to build the ragdoll. BuildRagdoll;');
  if (FEnabled) then Exit;
  FEnabled:= True;
  //First start the ragdoll in the reference position
  RootBone.StartChild;
  //Now align it to the animation
  RootBone.AlignChild;
  //Now it recalculate the vertices to use as reference
  FOwner.Skeleton.StartRagDoll;
end;

procedure TGLRagdoll.Update;
begin
  if FEnabled then
  begin
    RootBone.UpdateChild;
    FOwner.Skeleton.MorphMesh(true);
  end;
end;

procedure TGLRagdoll.Stop;
begin
  if not FEnabled then Exit;
  FEnabled := False;
  RootBone.StopChild;
  //Restore the old information
  FOwner.Skeleton.StopRagDoll;
  FOwner.Skeleton.MorphMesh(true);
end;

procedure TGLRagdoll.WriteToFiler(writer: TVirtualWriter);
begin
  inherited;

end;

procedure TGLRagdoll.BuildRagdoll;
begin
  Assert(RootBone <> nil, 'First you need to set the root bone. SetRootBone();');
  RootBone.CreateBoundsChild;
  FBuilt := True;
end;

end.
