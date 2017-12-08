//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  Nodes are used to describe lines, polygons + more.

   History :  
   01/03/11 - Vincent - Fix a bug in TGLNodes.Vector
   17/10/10 - Yar - Added TagObject property to TGLNode (thanks �Alexx)
   23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
   26/11/09 - DaStr - Improved Lazarus compatibility
  (thanks Predator) (BugtrackerID = 2893580)
   22/11/09 - DaStr - Improved Unix compatibility
  (thanks Predator) (BugtrackerID = 2893580)
   14/07/09 - DaStr - Added $I GLScene.inc
   05/10/08 - DanB - Created from GLMisc.pas split
   
}
unit GLNodes;

interface

uses
  Classes, SysUtils,
   
  GLVectorGeometry, OpenGLTokens, OpenGLAdapter, GLContext, GLBaseClasses,
  GLCoordinates, GLSpline,
  XOpenGL, GLVectorTypes;


{$I GLScene.inc}

type
  // TGLNode
  //
  TGLNode = class(TCollectionItem)
  private
     
    FCoords: TVector;
    FTagObject: TObject;
    procedure SetAsVector(const Value: TVector);
    procedure SetAsAffineVector(const Value: TAffineVector);
    function GetAsAffineVector: TAffineVector;
    procedure SetCoordinate(AIndex: Integer; AValue: TGLFloat);
    function GetCoordinate(const Index: Integer): TGLFloat;

  protected
     
    function StoreCoordinate(AIndex: Integer): Boolean;

    function GetDisplayName: string; override;

  public
     
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function AsAddress: PGLFloat;
    { : The coordinates viewed as a vector.
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. }
    property AsVector: TVector read FCoords write SetAsVector;
    { : The coordinates viewed as an affine vector.
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. 
      The W component is automatically adjustes depending on style. }
    property AsAffineVector: TAffineVector read GetAsAffineVector
      write SetAsAffineVector;

    property W: TGLFloat index 3 read GetCoordinate write SetCoordinate
      stored StoreCoordinate;

    property TagObject: TObject read FTagObject write FTagObject;
  published
     
    property X: TGLFloat index 0 read GetCoordinate write SetCoordinate
      stored StoreCoordinate;
    property Y: TGLFloat index 1 read GetCoordinate write SetCoordinate
      stored StoreCoordinate;
    property Z: TGLFloat index 2 read GetCoordinate write SetCoordinate
      stored StoreCoordinate;
  end;

  // TGLNodes
  //
  TGLNodes = class(TOwnedCollection)
  private
     

  protected
     
    procedure SetItems(Index: Integer; const Val: TGLNode);
    function GetItems(Index: Integer): TGLNode;
    procedure Update(Item: TCollectionItem); override;

  public
     
    constructor Create(AOwner: TPersistent;
      AItemClass: TCollectionItemClass = nil);
    function CreateCopy(AOwner: TPersistent): TGLNodes;

    function Add: TGLNode;
    function FindItemID(ID: Integer): TGLNode;
    property Items[index: Integer]: TGLNode read GetItems
      write SetItems; default;
    function First: TGLNode;
    function Last: TGLNode;

    procedure NotifyChange; virtual;
    procedure EndUpdate; override;

    procedure AddNode(const Coords: TGLCustomCoordinates); overload;
    procedure AddNode(const X, Y, Z: TGLfloat); overload;
    procedure AddNode(const Value: TVector); overload;
    procedure AddNode(const Value: TAffineVector); overload;
    procedure AddXYArc(XRadius, YRadius: Single; StartAngle, StopAngle: Single;
      NbSegments: Integer; const Center: TAffineVector);

    // : Calculates and returns the barycenter of the nodes
    function Barycenter: TAffineVector;
    { : Computes normal based on the 1st three nodes.
      Returns NullVector if there are less than 3 nodes. }
    function Normal: TAffineVector;
    // : Returns normalized vector Nodes[i+1]-Nodes[i]
    function Vector(I: Integer): TAffineVector;

    { : Calculates the extents of the nodes (min-max for all coordinates).
      The returned values are also the two corners of the axis-aligned
      bounding box. }
    procedure GetExtents(var Min, Max: TAffineVector);
    // : Translate all nodes
    procedure Translate(const Tv: TAffineVector);
    // : Scale all node coordinates
    procedure Scale(const Fv: TAffineVector); overload;
    // : Scale all node coordinates
    procedure Scale(F: Single); overload;
    // : Rotate nodes around Y axis by the given angle (degrees)
    procedure RotateAroundX(Angle: Single);
    // : Rotate nodes around Y axis by the given angle (degrees)
    procedure RotateAroundY(Angle: Single);
    // : Rotate nodes around Y axis by the given angle (degrees)
    procedure RotateAroundZ(Angle: Single);

    procedure RenderTesselatedPolygon(ATextured: Boolean;
      ANormal: PAffineVector = nil; ASplineDivisions: Integer = 1;
      AInvertNormals: Boolean = False);

    function CreateNewCubicSpline: TCubicSpline;

  end;

  TGLNodesClass = class of TGLNodes;

implementation

// ------------------
// ------------------ TGLNode ------------------
// ------------------

// Create
//

constructor TGLNode.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  // nothing, yet
end;

// Destroy
//

destructor TGLNode.Destroy;
begin
  // nothing, yet
  inherited Destroy;
end;

 
//

procedure TGLNode.Assign(Source: TPersistent);
begin
  if Source is TGLNode then
  begin
    FCoords := TGLNode(Source).FCoords;
  end
  else
    inherited;
end;

// GetDisplayName
//

function TGLNode.GetDisplayName: string;
begin
  Result := Format('%.4f; %.4f; %.4f', [X, Y, Z]);
end;

// AsAddress
//

function TGLNode.AsAddress: PGLFloat;
begin
  Result := @FCoords;
end;

// SetAsVector
//

procedure TGLNode.SetAsVector(const Value: TVector);
begin
  FCoords := Value;
  (Collection as TGLNodes).NotifyChange;
end;

// SetAsAffineVector
//

procedure TGLNode.SetAsAffineVector(const Value: TAffineVector);
begin
  GLVectorGeometry.SetVector(FCoords, Value);
  (Collection as TGLNodes).NotifyChange;
end;

// GetAsAffineVector
//

function TGLNode.GetAsAffineVector: TAffineVector;
begin
  GLVectorGeometry.SetVector(Result, FCoords);
end;

function TGLNode.GetCoordinate(const Index: Integer): TGLFloat;
begin
  Result := FCoords.V[Index];
end;

// SetCoordinate
//

procedure TGLNode.SetCoordinate(AIndex: Integer; AValue: TGLFloat);
begin
  FCoords.V[AIndex] := AValue;
  (Collection as TGLNodes).NotifyChange;
end;

// StoreCoordinate
//

function TGLNode.StoreCoordinate(AIndex: Integer): Boolean;
begin
  Result := (FCoords.V[AIndex] <> 0);
end;

// ------------------
// ------------------ TGLNodes ------------------
// ------------------

// Create
//

constructor TGLNodes.Create(AOwner: TPersistent;
  AItemClass: TCollectionItemClass = nil);
begin
  if not Assigned(AItemClass) then
    inherited Create(AOwner, TGLNode)
  else
    inherited Create(AOwner, AItemClass);
end;

// CreateCopy
//

function TGLNodes.CreateCopy(AOwner: TPersistent): TGLNodes;
begin
  if Self <> nil then
  begin
    Result := TGLNodesClass(Self.ClassType).Create(AOwner);
    Result.Assign(Self);
  end
  else
    Result := nil;
end;

// SetItems
//

procedure TGLNodes.SetItems(Index: Integer; const Val: TGLNode);
begin
  inherited Items[index] := Val;
end;

// GetItems
//

function TGLNodes.GetItems(Index: Integer): TGLNode;
begin
  Result := TGLNode(inherited Items[index]);
end;

// First
//

function TGLNodes.First: TGLNode;
begin
  if Count > 0 then
    Result := TGLNode(inherited Items[0])
  else
    Result := nil;
end;

// Last
//

function TGLNodes.Last: TGLNode;
var
  N: Integer;
begin
  N := Count - 1;
  if N >= 0 then
    Result := TGLNode(inherited Items[N])
  else
    Result := nil;
end;

// Update
//

procedure TGLNodes.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

// Add
//

function TGLNodes.Add: TGLNode;
begin
  Result := (inherited Add) as TGLNode;
end;

// FindItemID
//

function TGLNodes.FindItemID(ID: Integer): TGLNode;
begin
  Result := (inherited FindItemID(ID)) as TGLNode;
end;

// NotifyChange
//

procedure TGLNodes.NotifyChange;
begin
  if (UpdateCount = 0) and (GetOwner <> nil) and
    (GetOwner is TGLUpdateAbleComponent) then
    TGLUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

// EndUpdate
//

procedure TGLNodes.EndUpdate;
begin
  inherited EndUpdate;
  // Workaround for a bug in VCL's EndUpdate
  if UpdateCount = 0 then
    NotifyChange;
end;

// AddNode (TGLCustomCoordinates)
//

procedure TGLNodes.AddNode(const Coords: TGLCustomCoordinates);
begin
  Add.AsVector := Coords.AsVector;
end;

// AddNode (floats)
//

procedure TGLNodes.AddNode(const X, Y, Z: Single);
begin
  Add.AsVector := PointMake(X, Y, Z);
end;

// AddNode (TVector)
//

procedure TGLNodes.AddNode(const Value: TVector);
begin
  Add.AsVector := Value;
end;

// AddNode (TAffineVector)
//

procedure TGLNodes.AddNode(const Value: TAffineVector);
begin
  Add.AsAffineVector := Value;
end;

// AddXYArc
//

procedure TGLNodes.AddXYArc(XRadius, YRadius: Single;
  StartAngle, StopAngle: Single; NbSegments: Integer;
  const Center: TAffineVector);
var
  I: Integer;
  F: Single;
  S, C: Single;
begin
  BeginUpdate;
  try
    StartAngle := DegToRad(StartAngle);
    StopAngle := DegToRad(StopAngle);
    F := (StopAngle - StartAngle) / NbSegments;
    for I := 0 to NbSegments do
    begin
      SinCos(I * F + StartAngle, S, C);
      SetVector(Add.FCoords, Center.V[0] + XRadius * C,
                             Center.V[1] + YRadius * S,
                             Center.V[2], 1);
    end;
  finally
    EndUpdate;
  end;
end;

// Barycenter
//

function TGLNodes.Barycenter: TAffineVector;
var
  I: Integer;
begin
  Result := NullVector;
  if Count > 0 then
  begin
    for I := 0 to Count - 1 do
      AddVector(Result, PAffineVector(Items[I].AsAddress)^);
    ScaleVector(Result, 1.0 / Count);
  end;
end;

// Normal
//

function TGLNodes.Normal: TAffineVector;
begin
  if Count >= 3 then
    CalcPlaneNormal(Items[0].FCoords, Items[1].FCoords,
      Items[2].FCoords, Result)
  else
    Result := NullVector;
end;

// Vector
//

function TGLNodes.Vector(I: Integer): TAffineVector;

procedure CalcUsingPrev; forward;

  procedure CalcUsingNext;
  begin
    if I < Count - 1 then
      VectorSubtract(Items[I].AsVector, Items[I + 1].AsVector, Result)
    else
      CalcUsingPrev;
  end;

  procedure CalcUsingPrev;
  begin
    if I > 0 then
      VectorSubtract(Items[I - 1].AsVector, Items[I].AsVector, Result)
    else
      CalcUsingNext;
  end;

var
  J: Integer;
  Vecnull: Boolean;
begin
  Assert((I >= 0) and (I < Count));
  if I = 0 then
    if I = Count - 1 then
      SetVector(Result, NullVector)
    else
      VectorSubtract(Items[I + 1].AsVector, Items[I].AsVector, Result)
  else if I = Count - 1 then
    VectorSubtract(Items[I].AsVector, Items[I - 1].AsVector, Result)
  else
    VectorSubtract(Items[I + 1].AsVector, Items[I - 1].AsVector, Result);
  if VectorNorm(Result) < 1E-5 then
  begin
    // avoid returning null vector which generates display bugs in geometry
    J := 1;
    Vecnull := True;
    while (I + J < Count) and (Vecnull) do
    begin
      VectorSubtract(Items[I + J].AsVector, Items[I].AsVector, Result);
      if (VectorNorm(Result) > 1E-5) then
        Vecnull := False
      else
        Inc(J);
    end;
    J := 1;
    while (I - J > 0) and (Vecnull) do
    begin
      VectorSubtract(Items[I].AsVector, Items[I - J].AsVector, Result);
      if (VectorNorm(Result) > 1E-5) then
        Vecnull := False
      else
        Inc(J);
    end;
    if Vecnull then
      SetVector(Result, NullVector)
    else
      NormalizeVector(Result);
  end
  else
    NormalizeVector(Result);
end;

// GetExtents
//

procedure TGLNodes.GetExtents(var Min, Max: TAffineVector);
var
  I, K: Integer;
  F: Single;
const
  CBigValue: Single = 1E50;
  CSmallValue: Single = -1E50;
begin
  SetVector(Min, CBigValue, CBigValue, CBigValue);
  SetVector(Max, CSmallValue, CSmallValue, CSmallValue);
  for I := 0 to Count - 1 do
  begin
    for K := 0 to 2 do
    begin
      F := PAffineVector(Items[I].AsAddress)^.V[K];
      if F < Min.V[K] then
        Min.V[K] := F;
      if F > Max.V[K] then
        Max.V[K] := F;
    end;
  end;
end;

// Translate
//

procedure TGLNodes.Translate(const Tv: TAffineVector);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AddVector(PAffineVector(Items[I].AsAddress)^, Tv);
  NotifyChange;
end;

// Scale (vector)
//

procedure TGLNodes.Scale(const Fv: TAffineVector);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ScaleVector(PAffineVector(Items[I].AsAddress)^, Fv);
  NotifyChange;
end;

// Scale (single)
//

procedure TGLNodes.Scale(F: Single);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ScaleVector(PAffineVector(Items[I].AsAddress)^, F);
  NotifyChange;
end;

// RotateAroundX
//

procedure TGLNodes.RotateAroundX(Angle: Single);
var
  I: Integer;
  C, S, V2: Single;
  V: PAffineVector;
begin
  SinCos(CPIDiv180 * Angle, S, C);
  for I := 0 to Count - 1 do
  begin
    V := PAffineVector(Items[I].AsAddress);
    V2 := V^.V[2];
    V^.V[1] := C * V^.V[1] + S * V2;
    V^.V[2] := C * V2 - S * V^.V[1];
  end;
  NotifyChange;
end;

// RotateAroundY
//

procedure TGLNodes.RotateAroundY(Angle: Single);
var
  I: Integer;
  C, S, V0: Single;
  V: PAffineVector;
begin
  SinCos(CPIDiv180 * Angle, S, C);
  for I := 0 to Count - 1 do
  begin
    V := PAffineVector(Items[I].AsAddress);
    V0 := V^.V[0];
    V^.V[0] := C * V0 + S * V^.V[2];
    V^.V[2] := C * V^.V[2] - S * V0;
  end;
  NotifyChange;
end;

// RotateAroundZ
//

procedure TGLNodes.RotateAroundZ(Angle: Single);
var
  I: Integer;
  C, S, V1: Single;
  V: PAffineVector;
begin
  SinCos(CPIDiv180 * Angle, S, C);
  for I := 0 to Count - 1 do
  begin
    V := PAffineVector(Items[I].AsAddress);
    V1 := V^.V[1];
    V^.V[1] := C * V1 + S * V^.V[0];
    V^.V[0] := C * V^.V[0] - S * V1;
  end;
  NotifyChange;
end;

// CreateNewCubicSpline
//

function TGLNodes.CreateNewCubicSpline: TCubicSpline;
var
  I: Integer;
  Xa, Ya, Za: PFloatArray;
begin
  GetMem(Xa, SizeOf(TGLFloat) * Count);
  GetMem(Ya, SizeOf(TGLFloat) * Count);
  GetMem(Za, SizeOf(TGLFloat) * Count);
  for I := 0 to Count - 1 do
    with Items[I] do
    begin
      Xa^[I] := X;
      Ya^[I] := Y;
      Za^[I] := Z;
    end;
  Result := TCubicSpline.Create(Xa, Ya, Za, nil, Count);
  FreeMem(Xa);
  FreeMem(Ya);
  FreeMem(Za);
end;

// RenderTesselatedPolygon
//
var
  NbExtraVertices: Integer;
  NewVertices: PAffineVectorArray;

function AllocNewVertex: PAffineVector;
begin
  Inc(NbExtraVertices);
  Result := @NewVertices[NbExtraVertices - 1];
end;

procedure TessError(Errno: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Assert(False, IntToStr(Errno) + ': ' + string(GluErrorString(Errno)));
end;

procedure TessIssueVertex(VertexData: Pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Xgl.TexCoord2fv(VertexData);
  GL.Vertex3fv(VertexData);
end;

procedure TessCombine(Coords: PDoubleVector; Vertex_data: Pointer;
  Weight: PGLFloat; var OutData: Pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  OutData := AllocNewVertex;
  SetVector(PAffineVector(OutData)^, Coords^[0], Coords^[1], Coords^[2]);
end;

procedure TGLNodes.RenderTesselatedPolygon(ATextured: Boolean;
  ANormal: PAffineVector = nil; ASplineDivisions: Integer = 1;
  AInvertNormals: Boolean = False);
var
  I: Integer;
  Tess: PGLUTesselator;
  DblVector: TAffineDblVector;
  Spline: TCubicSpline;
  SplinePos: PAffineVector;
  F: Single;

begin
  if Count > 2 then
  begin
    // Create and initialize the GLU tesselator
    Tess := GluNewTess;
    GluTessCallback(Tess, GLU_TESS_BEGIN, @GL.Begin_);
    if ATextured then
      GluTessCallback(Tess, GLU_TESS_VERTEX, @TessIssueVertex)
    else
      GluTessCallback(Tess, GLU_TESS_VERTEX, @GL.Vertex3fv);
    GluTessCallback(Tess, GLU_TESS_END, @GL.End_);
    GluTessCallback(Tess, GLU_TESS_ERROR, @TessError);
    GluTessCallback(Tess, GLU_TESS_COMBINE, @TessCombine);
    NbExtraVertices := 0;
    // Issue normal
    if Assigned(ANormal) then
    begin
      GL.Normal3fv(PGLFloat(ANormal));
      GluTessNormal(Tess, ANormal^.V[0], ANormal^.V[1], ANormal^.V[2]);
    end;
    // Issue polygon
    GluTessBeginPolygon(Tess, nil);
    GluTessBeginContour(Tess);
    if ASplineDivisions <= 1 then
    begin
      // no spline, use direct coordinates
      GetMem(NewVertices, Count * SizeOf(TAffineVector));
      if AInvertNormals then
      begin
        for I := Count - 1 downto 0 do
        begin
          SetVector(DblVector, PAffineVector(Items[I].AsAddress)^);
          GluTessVertex(Tess, DblVector, Items[I].AsAddress);
        end;
      end
      else
      begin
        for I := 0 to Count - 1 do
        begin
          SetVector(DblVector, PAffineVector(Items[I].AsAddress)^);
          GluTessVertex(Tess, DblVector, Items[I].AsAddress);
        end;
      end;
    end
    else
    begin
      // cubic spline
      GetMem(NewVertices, 2 * ASplineDivisions * Count * SizeOf(TAffineVector));
      Spline := CreateNewCubicSpline;
      F := 1.0 / ASplineDivisions;
      if AInvertNormals then
      begin
        for I := ASplineDivisions * (Count - 1) downto 0 do
        begin
          SplinePos := AllocNewVertex;
          Spline.SplineAffineVector(I * F, SplinePos^);
          SetVector(DblVector, SplinePos^);
          GluTessVertex(Tess, DblVector, SplinePos);
        end;
      end
      else
      begin
        for I := 0 to ASplineDivisions * (Count - 1) do
        begin
          SplinePos := AllocNewVertex;
          Spline.SplineAffineVector(I * F, SplinePos^);
          SetVector(DblVector, SplinePos^);
          GluTessVertex(Tess, DblVector, SplinePos);
        end;
      end;
      Spline.Free;
    end;
    GluTessEndContour(Tess);
    GluTessEndPolygon(Tess);
    // release stuff
    if Assigned(NewVertices) then
      FreeMem(NewVertices);
    GluDeleteTess(Tess);
  end;
end;

end.
