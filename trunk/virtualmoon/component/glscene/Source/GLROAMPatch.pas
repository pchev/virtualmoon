
// This unit is part of the GLScene Project, http://glscene.org

{: GLROAMPatch<p>

   Class for managing a ROAM (square) patch.<p>

  <b>History : </b><font size=-1><ul>
      <li>22/08/10 - DaStr - Fixed compiler warning
      <li>27/07/10 - YP - Safe tesselation operation to avoid AV after a memory shift
      <li>26/07/10 - YP - Invalid range test when splitting, we need to check space for n and n+1
      <li>20/05/10 - Yar - Fixes for Linux x64
      <li>16/10/08 - UweR - Compatibility fix for Delphi 2009
      <li>30/03/07 - DaStr - Added $I GLScene.inc
      <li>19/10/06 - LC - Added code to gracefully handle the case when MaxCLODTriangles is reached.
                          It will now increase the buffer instead of not splitting. Bugtracker ID=1574111
      <li>09/10/06 - Lin - Added OnMaxCLODTrianglesReached event.
      <li>09/06/06 - Lin - Bugfix: Stop splitting Triangles when MaxCLODTriangles is reached (prevents Access Violations)
      <li>10/06/05 - Mathx - Protection against cards that have GL_EXT_compiled_vertex_array
                             but not GL_EXT_draw_range_elements
      <li>25/04/04 - EG - Occlusion testing support
      <li>06/02/03 - EG - Adaptative variance computation
      <li>03/12/02 - EG - Minor ROAM tessel/render optimizations
      <li>15/06/02 - EG - Fixed patch rendering bug "introduced" by TBaseList fix
      <li>24/02/02 - EG - Hybrid ROAM-stripifier engine
      <li>10/09/01 - EG - Creation
  </ul></font>
}
unit GLROAMPatch;

interface

{$I GLScene.inc}

uses VectorGeometry, GLHeightData, VectorLists, GLCrossPlatform, GLContext, SysUtils;

type

  // Exception use by Split for SafeTesselate
  EGLROAMException = class(Exception);

  // TROAMTriangleNode

  PROAMTriangleNode = ^TROAMTriangleNode;

  TROAMTriangleNode = packed record
    base, left, right: PROAMTriangleNode;
    leftChild, rightChild: PROAMTriangleNode;
  end;

  // TROAMRenderPoint

  TROAMRenderPoint = packed record
    X, Y: integer;
    idx: integer;
  end;

  // TGLROAMPatch

  TGLROAMPatch = class(TObject)
  private
    { Private Declarations }
    FID: integer;
    FHeightData: THeightData; // Referred, not owned
    FHeightRaster: PSmallIntRaster;
    FTLNode, FBRNode: integer;
    FTLVariance, FBRVariance: array of cardinal;
    FPatchSize, FTriangleCount: integer;
    FListHandle: TGLListHandle;
    FTag: integer;
    FObserverPosition: TAffineVector;
    FNorth, FSouth, FWest, FEast: TGLROAMPatch; // neighbours
    FHighRes: boolean;
    FMaxDepth: integer;
    FVertexScale, FVertexOffset: TAffineVector;
    FTextureScale, FTextureOffset: TAffineVector;
    FMaxTLVarianceDepth, FMaxBRVarianceDepth: integer;

    FOcclusionQuery: TGLOcclusionQueryHandle;
    FOcclusionSkip, FOcclusionCounter: integer;
    FLastOcclusionTestPassed: boolean;



  protected
    { Protected Declarations }
    procedure SetHeightData(val: THeightData);
    procedure SetOcclusionSkip(val: integer);

    procedure RenderROAM(vertices: TAffineVectorList;
      vertexIndices: TIntegerList;
      texCoords: TTexPointList);
    procedure RenderAsStrips(vertices: TAffineVectorList;
      vertexIndices: TIntegerList;
      texCoords: TTexPointList);

    function Tesselate: boolean;
    // Returns false if MaxCLODTriangles limit is reached(Lin)
  public
    { Public Declarations }
    constructor Create;
    destructor Destroy; override;

    procedure ComputeVariance(variance: integer);

    procedure ResetTessellation;
    procedure ConnectToTheWest(westPatch: TGLROAMPatch);
    procedure ConnectToTheNorth(northPatch: TGLROAMPatch);

         {: AV free version of Tesselate.<p>
            When IncreaseTrianglesCapacity is called, all PROAMTriangleNode
            values in higher function became invalid due to the memory shifting.
            Recursivity is the main problem, that's why SafeTesselate is calling
            Tesselate in a try..except .}
    function SafeTesselate: boolean;

         {: Render the patch in high-resolution.<p>
            The lists are assumed to have enough capacity to allow AddNC calls
            (additions without capacity check). High-resolution renders use
            display lists, and are assumed to be made together. }
    procedure RenderHighRes(vertices: TAffineVectorList;
      vertexIndices: TIntegerList;
      texCoords: TTexPointList;
      forceROAM: boolean);
         {: Render the patch by accumulating triangles.<p>
            The lists are assumed to have enough capacity to allow AddNC calls
            (additions without capacity check).<br>
            Once at least autoFlushVertexCount vertices have been accumulated,
            perform a FlushAccum }
    procedure RenderAccum(vertices: TAffineVectorList;
      vertexIndices: TIntegerList;
      texCoords: TTexPointList;
      autoFlushVertexCount: integer);
         {: Render all vertices accumulated in the arrays and set their count
            back to zero. }
    class procedure FlushAccum(vertices: TAffineVectorList;
      vertexIndices: TIntegerList;
      texCoords: TTexPointList);

    property HeightData: THeightData read FHeightData write SetHeightData;
    property VertexScale: TAffineVector read FVertexScale write FVertexScale;
    property VertexOffset: TAffineVector read FVertexOffset write FVertexOffset;

    property ObserverPosition: TAffineVector
      read FObserverPosition write FObserverPosition;

    property TextureScale: TAffineVector read FTextureScale write FTextureScale;
    property TextureOffset: TAffineVector read FTextureOffset write FTextureOffset;

    property HighRes: boolean read FHighRes write FHighRes;

    {: Number of frames to skip after an occlusion test returned zero pixels. }
    property OcclusionSkip: integer read FOcclusionSkip write SetOcclusionSkip;
    {: Number of frames remaining to next occlusion test. }
    property OcclusionCounter: integer read FOcclusionCounter
      write FOcclusionCounter;
         {: Result for the last occlusion test.<p>
            Note that this value is updated upon rendering the tile in
            non-high-res mode only. }
    property LastOcclusionTestPassed: boolean read FLastOcclusionTestPassed;

    property ID: integer read FID;
    property TriangleCount: integer read FTriangleCount;
    property Tag: integer read FTag write FTag;
  end;


{: Specifies the maximum number of ROAM triangles that may be allocated. }
procedure SetROAMTrianglesCapacity(nb: integer);
function GetROAMTrianglesCapacity: integer;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses OpenGLTokens, XOpenGL;

var
  FVBOVertHandle, FVBOTexHandle: TGLVBOArrayBufferHandle;
  FVBOIndicesHandle: TGLVBOElementArrayHandle;

type

  // TROAMVariancePoint

  TROAMVariancePoint = packed record
    X, Y: integer;
    Z: integer;
  end;

var
  vNextPatchID: integer;
  vNbTris, vTriangleNodesCapacity: integer;
  vTriangleNodes: array of TROAMTriangleNode;

// SetROAMTrianglesCapacity

procedure SetROAMTrianglesCapacity(nb: integer);
begin
  vNbTris := 0;
  if vTriangleNodesCapacity <> nb then
  begin
    SetLength(vTriangleNodes, nb);
    vTriangleNodesCapacity := nb;
  end;
end;

function GetROAMTrianglesCapacity: integer;
begin
  Result := vTriangleNodesCapacity;
end;


// The result is the delta between the old address of the array and the new one
function IncreaseTrianglesCapacity(NewCapacity: integer): int64;

  procedure FixNodePtr(var p: PROAMTriangleNode; const delta: int64);
  begin
    if p = nil then
      exit;

    Inc(PByte(p), delta);
  end;

var
  oldbase, newbase: pointer;
  node: PROAMTriangleNode;
  i, oldsize: integer;
begin
  Result := 0;
  if NewCapacity <= vTriangleNodesCapacity then
    exit;

  oldsize := vTriangleNodesCapacity;

  oldbase := pointer(vTriangleNodes);
  SetLength(vTriangleNodes, NewCapacity);

  vTriangleNodesCapacity := NewCapacity;

  newbase := pointer(vTriangleNodes);

  // Array has not been relocated, no need to fix
  if oldbase = newbase then
    exit;

  // go through all the old nodes and fix the pointers
  // YP: Delphi needs int64 dual casting to avoid overflow exception
  Result := int64(PtrUInt(newbase)) - int64(PtrUInt(oldbase));
  for i := 0 to oldsize - 1 do
  begin
    node := @vTriangleNodes[i];

    FixNodePtr(node^.base, Result);
    FixNodePtr(node^.left, Result);
    FixNodePtr(node^.right, Result);
    FixNodePtr(node^.leftChild, Result);
    FixNodePtr(node^.rightChild, Result);
  end;
end;

// AllocTriangleNode

function AllocTriangleNode: integer;
var
  nilNode: PROAMTriangleNode;
begin
  if vNbTris >= vTriangleNodesCapacity then
  begin
    // grow by 50%
    IncreaseTrianglesCapacity(vTriangleNodesCapacity + (vTriangleNodesCapacity shr 1));
  end;
  Result := vNbTris;
  with vTriangleNodes[vNbTris] do
  begin
    nilNode := nil;
    left := nilNode;
    right := nilNode;
    leftChild := nilNode;
    rightChild := nilNode;
  end;
  Inc(vNbTris);
end;

// Split

function Split(tri: PROAMTriangleNode): boolean;
var
  n: integer;
  lc, rc: PROAMTriangleNode;
  Shift: int64;
begin
  Result := Assigned(tri.leftChild);
  if Result then
    Exit;                            //dont split if tri already has a left child
  with tri^ do
  begin
    if Assigned(base) and (base.base <> tri) then
      Split(base); // If this triangle is not in a proper diamond, force split our base neighbor
    n := vNbTris;
  end;

  if n >= vTriangleNodesCapacity - 1 then
  begin
    // grow by 50%
    Shift := IncreaseTrianglesCapacity(vTriangleNodesCapacity +
      (vTriangleNodesCapacity shr 1));
    if Shift <> 0 then
    begin
      raise EGLROAMException.Create('PROAMTriangleNode addresses are invalid now');
    end;
  end;

  with tri^ do
  begin

    // Create children and cross-link them
    lc := @vTriangleNodes[n];           //left child
    rc := @vTriangleNodes[n + 1];         //right child

    leftChild := lc;
    rightChild := rc;

    rc.base := right;         //right child
    rc.leftChild := nil;
    rc.rightChild := leftChild;
    rc.right := leftChild;

    lc.base := left;          //left child
    lc.leftChild := nil;
    lc.rightChild := leftChild;
    lc.left := rightChild;

    Inc(vNbTris, 2);

    if Assigned(left) then   // Link our Left Neighbour to the new children
      if left.base = tri then
        left.base := lc
      else if left.left = tri then
        left.left := lc
      else
        left.right := lc;
    if Assigned(right) then  // Link our Right Neighbour to the new children
      if right.base = tri then
        right.base := rc
      else if right.left = tri then
        right.left := rc
      else
        right.right := rc;
    // Link our Base Neighbor to the new children
    if Assigned(base) then
    begin
      if Assigned(base.leftChild) then
      begin
        base.leftChild.right := rightChild;
        rightChild.left := base.leftChild;
        base.rightChild.left := leftChild;
        leftChild.right := base.rightChild;
      end
      else
      begin
        Split(base);
      end;
    end
    else
    begin // An edge triangle, trivial case.
      leftChild.right := nil;
      rightChild.left := nil;
    end;
  end;
  Result := True;
end;


// ------------------
// ------------------ TGLROAMPatch ------------------
// ------------------

// Create

constructor TGLROAMPatch.Create;
begin
  inherited Create;
  FID := vNextPatchID;
  Inc(vNextPatchID);
  FListHandle := TGLListHandle.Create;
  FOcclusionQuery := TGLOcclusionQueryHandle.Create;
end;

// Destroy

destructor TGLROAMPatch.Destroy;
begin
  FListHandle.Free;
  FOcclusionQuery.Free;
  inherited Destroy;
end;

// SetHeightData

procedure TGLROAMPatch.SetHeightData(val: THeightData);
begin
  FHeightData := val;
  FPatchSize := FHeightData.Size - 1;
  FHeightRaster := val.SmallIntRaster;
end;

// SetOcclusionSkip

procedure TGLROAMPatch.SetOcclusionSkip(val: integer);
begin
  if val < 0 then
    val := 0;
  if FOcclusionSkip <> val then
  begin
    FOcclusionSkip := val;
    FOcclusionQuery.DestroyHandle;
  end;
end;

// ConnectToTheWest

procedure TGLROAMPatch.ConnectToTheWest(westPatch: TGLROAMPatch);
begin
  if Assigned(westPatch) then
  begin
    if not (westPatch.HighRes or HighRes) then
    begin
      vTriangleNodes[FTLNode].left := @vTriangleNodes[westPatch.FBRNode];
      vTriangleNodes[westPatch.FBRNode].left := @vTriangleNodes[FTLNode];
    end;
    FWest := westPatch;
    westPatch.FEast := Self;
  end;
end;

// ConnectToTheNorth

procedure TGLROAMPatch.ConnectToTheNorth(northPatch: TGLROAMPatch);
begin
  if Assigned(northPatch) then
  begin
    if not (northPatch.HighRes or HighRes) then
    begin
      vTriangleNodes[FTLNode].right := @vTriangleNodes[northPatch.FBRNode];
      vTriangleNodes[northPatch.FBRNode].right := @vTriangleNodes[FTLNode];
    end;
    FNorth := northPatch;
    northPatch.FSouth := Self;
  end;
end;

// ComputeVariance

procedure TGLROAMPatch.ComputeVariance(variance: integer);
var
  raster: PSmallIntRaster;
  currentVariance: PIntegerArray;
  maxVarianceDepth: integer;
  maxNonNullIndex: integer;
  invVariance: single;

  function ROAMVariancePoint(anX, anY: integer): TROAMVariancePoint;
  begin
    Result.X := anX;
    Result.Y := anY;
    Result.Z := (integer(FHeightRaster[anY][anX]) shl 8);
  end;

  function RecursComputeVariance(const left, right, apex: TROAMVariancePoint;
    node: integer): cardinal;
  var
    half: TROAMVariancePoint;
    v: cardinal;
    n2: integer;
  begin
    with half do
    begin
      X := (left.X + right.X) shr 1;
      Y := (left.Y + right.Y) shr 1;
      Z := integer(raster[Y][X]) shl 8;
      Result := ScaleAndRound(Abs(((left.Z + right.Z) div 2) - Z), invVariance);
    end;

    n2 := node shl 1;
    if n2 < maxVarianceDepth then
    begin
      v := RecursComputeVariance(apex, left, half, n2);
      if v > Result then
        Result := v;
      v := RecursComputeVariance(right, apex, half, 1 + n2);
      if v > Result then
        Result := v;
    end;
    currentVariance[node] := Result;
  end;

  procedure ScaleVariance(n, d: integer);
  var
    newVal: integer;
  begin
    if d >= 0 then
      newVal := (currentVariance[n] shl (d shr 1))
    else
      newVal := (currentVariance[n] shr (-d shr 1));
    currentVariance[n] := newVal;
    if newVal > 0 then
      if n > maxNonNullIndex then
        maxNonNullIndex := n;
    n := n shl 1;
    if n < maxVarianceDepth then
    begin
      Dec(d);
      ScaleVariance(n, d);
      ScaleVariance(n + 1, d);
    end;
  end;

var
  s, p: integer;
begin
  invVariance := 1 / variance;
  s := Sqr(FPatchSize);
  raster := FHeightRaster;
  FMaxDepth := 1;
  p := -1 - 8;
  repeat
    FMaxDepth := FMaxDepth shl 2;
    Inc(p);
  until FMaxDepth >= s;
  maxVarianceDepth := FMaxDepth;
  SetLength(FTLVariance, maxVarianceDepth);
  SetLength(FBRVariance, maxVarianceDepth);

  s := FPatchSize;
  currentVariance := @FTLVariance[0];
  maxNonNullIndex := 1;
  RecursComputeVariance(ROAMVariancePoint(0, s), ROAMVariancePoint(s, 0),
    ROAMVariancePoint(0, 0), 1);
  ScaleVariance(1, p);
  FMaxTLVarianceDepth := maxNonNullIndex + 1;
  SetLength(FTLVariance, FMaxTLVarianceDepth);
  currentVariance := @FBRVariance[0];
  maxNonNullIndex := 1;
  RecursComputeVariance(ROAMVariancePoint(s, 0), ROAMVariancePoint(0, s),
    ROAMVariancePoint(s, s), 1);
  ScaleVariance(1, p);
  FMaxBRVarianceDepth := maxNonNullIndex + 1;
  SetLength(FBRVariance, FMaxBRVarianceDepth);
end;

// ResetTessellation

procedure TGLROAMPatch.ResetTessellation;
begin
  FTLNode := AllocTriangleNode;
  FBRNode := AllocTriangleNode;
  vTriangleNodes[FTLNode].base := @vTriangleNodes[FBRNode];
  vTriangleNodes[FBRNode].base := @vTriangleNodes[FTLNode];
  FNorth := nil;
  FSouth := nil;
  FWest := nil;
  FEast := nil;
end;

// Tessellate

var
  tessMaxVariance: cardinal;
  tessMaxDepth: cardinal;
  tessCurrentVariance: PIntegerArray;
  tessObserverPosX, tessObserverPosY: integer;

function RecursTessellate(tri: PROAMTriangleNode; n: cardinal;
  const left, right, apex: cardinal): boolean;
  //returns false if tessellation failed due to MaxCLODTriangles limit
var
  d: integer;
begin
  Result := True;
  d := ((left + right) shr 1);
  if tessCurrentVariance[n] > d then
  begin
    Result := False;
    if Split(tri) then
    begin
      n := n shl 1;
      if n < tessMaxVariance then
      begin
        RecursTessellate(tri.leftChild, n, apex, left, d);
        Result :=
          RecursTessellate(tri.rightChild, n + 1, right, apex, d);
      end;
    end;
  end;
end;

function TGLROAMPatch.Tesselate: boolean;
  //Returns false if MaxCLODTriangles limit is reached.
var
  tessFrameVarianceDelta: integer;

  function VertexDist(x, y: integer): cardinal;
  var
    f: single;
  const
    c1Div100: single = 0.01;
  begin
    if HighRes then
      f := 0.2 * Sqr(FPatchSize)
    else
      f := Sqr(x - tessObserverPosX) + Sqr(y - tessObserverPosY) + tessFrameVarianceDelta;
    Result := Round(Sqrt(f) + f * c1Div100);
  end;

  procedure FullBaseTess(tri: PROAMTriangleNode; n: cardinal); forward;

  procedure FullLeftTess(tri: PROAMTriangleNode; n: cardinal);
  begin
    if Split(tri) then
    begin
      n := n shl 1;
      if n < tessMaxDepth then
        FullBaseTess(tri.leftChild, n);
    end;
  end;

  procedure FullRightTess(tri: PROAMTriangleNode; n: cardinal);
  begin
    if Split(tri) then
    begin
      n := n shl 1;
      if n < tessMaxDepth then
        FullBaseTess(tri.rightChild, n);
    end;
  end;

  procedure FullBaseTess(tri: PROAMTriangleNode; n: cardinal);
  begin
    if Split(tri) then
    begin
      n := n shl 1;
      if n < tessMaxDepth then
      begin
        FullRightTess(tri.leftChild, n);
        FullLeftTess(tri.rightChild, n);
      end;
    end;
  end;

var
  s: integer;
begin
  tessMaxDepth := FMaxDepth;
  tessObserverPosX := Round(FObserverPosition[0]);
  tessObserverPosY := Round(FObserverPosition[1]);

  if HighRes then
  begin
    FullRightTess(@vTriangleNodes[FTLNode], 1);
    FullRightTess(@vTriangleNodes[FBRNode], 1);
    FullLeftTess(@vTriangleNodes[FBRNode], 1);
    FullLeftTess(@vTriangleNodes[FTLNode], 1);
    tessFrameVarianceDelta := 0;
  end
  else
  begin
    if Assigned(FNorth) and FNorth.HighRes then
      FullRightTess(@vTriangleNodes[FTLNode], 1);
    if Assigned(FSouth) and FSouth.HighRes then
      FullRightTess(@vTriangleNodes[FBRNode], 1);
    if Assigned(FEast) and FEast.HighRes then
      FullLeftTess(@vTriangleNodes[FBRNode], 1);
    if Assigned(FWest) and FWest.HighRes then
      FullLeftTess(@vTriangleNodes[FTLNode], 1);
    if FObserverPosition[2] > 0 then
      tessFrameVarianceDelta := Round(Sqr(FObserverPosition[2] * (1 / 16)))
    else
      tessFrameVarianceDelta := 0;
  end;
  s := FPatchSize;
  tessCurrentVariance := @FTLVariance[0];
  tessMaxVariance := FMaxTLVarianceDepth;
  Result := RecursTessellate(@vTriangleNodes[FTLNode], 1, VertexDist(0, s),
    VertexDist(s, 0), VertexDist(0, 0));
  tessCurrentVariance := @FBRVariance[0];
  tessMaxVariance := FMaxBRVarianceDepth;
  if Result then
    Result := RecursTessellate(@vTriangleNodes[FBRNode], 1, VertexDist(s, 0),
      VertexDist(0, s), VertexDist(s, s));
end;


// SafeTesselate

function TGLROAMPatch.SafeTesselate: boolean;
var
  Fail: boolean;
begin
  Result := False;
  Fail := True;
  repeat
    try
      ResetTessellation;
      Result := Tesselate;
      Fail := False;
    except
      on e: EGLROAMException do
      begin
        //Nothing to do, just wait the next iteration
        Fail := True;
      end;
    end;
  until not Fail;
end;

// RenderHighRes

procedure TGLROAMPatch.RenderHighRes(vertices: TAffineVectorList;
  vertexIndices: TIntegerList;
  texCoords: TTexPointList;
  forceROAM: boolean);
var
  primitive: TGLEnum;
begin
  // Prepare display list if needed
  if FListHandle.Handle = 0 then
  begin

    // either use brute-force strips or a high-res static tesselation
    if forceROAM then
    begin
      SafeTesselate;
      RenderROAM(vertices, vertexIndices, texCoords);
      primitive := GL_TRIANGLES;
      FTriangleCount := vertexIndices.Count div 3;
    end
    else
    begin
      RenderAsStrips(vertices, vertexIndices, texCoords);
      primitive := GL_TRIANGLE_STRIP;
      FTriangleCount := vertexIndices.Count - 2 * FPatchSize;
    end;

    vertices.Translate(VertexOffset);
    texCoords.ScaleAndTranslate(PTexPoint(@TextureScale)^,
      PTexPoint(@TextureOffset)^);

    GL.VertexPointer(3, GL_FLOAT, 0, vertices.List);
    xgl.TexCoordPointer(2, GL_FLOAT, 0, texCoords.List);

    FListHandle.AllocateHandle;
    GL.NewList(FListHandle.Handle, GL_COMPILE);
    GL.DrawElements(primitive, vertexIndices.Count,
      GL_UNSIGNED_INT, vertexIndices.List);
    GL.EndList;

    vertices.Count := 0;
    texCoords.Count := 0;
    vertexIndices.Count := 0;
  end;
  // perform the render
  GL.CallList(FListHandle.Handle);
end;

// RenderAccum

procedure TGLROAMPatch.RenderAccum(vertices: TAffineVectorList;
  vertexIndices: TIntegerList;
  texCoords: TTexPointList;
  autoFlushVertexCount: integer);
var
  occlusionPassed: boolean;
  n, nb, nvi: integer;
begin
  // CLOD tiles are rendered via ROAM
  if (FOcclusionSkip > 0) and FOcclusionQuery.IsSupported then
  begin
    if FOcclusionQuery.Handle = 0 then
    begin
      FOcclusionQuery.AllocateHandle;
      FOcclusionCounter := -(ID mod (FOcclusionSkip));
    end;
    occlusionPassed := (FOcclusionCounter <= 0) or (FOcclusionQuery.PixelCount > 0);
    Dec(FOcclusionCounter);
    if occlusionPassed then
    begin
      if FOcclusionCounter <= 0 then
        Inc(FOcclusionCounter, FOcclusionSkip);
      FOcclusionQuery.BeginQuery;
    end;
  end
  else
    occlusionPassed := True;
  FLastOcclusionTestPassed := occlusionPassed;
  if occlusionPassed then
  begin
    nvi := vertexIndices.Count;
    n := vertices.Count;
    RenderROAM(vertices, vertexIndices, texCoords);
    nb := vertices.Count - n;
    FTriangleCount := (vertexIndices.Count - nvi) div 3;

    vertices.Translate(VertexOffset, n, nb);
    texCoords.ScaleAndTranslate(PTexPoint(@TextureScale)^,
      PTexPoint(@TextureOffset)^, n, nb);

    if FOcclusionQuery.Active then
    begin
      FlushAccum(vertices, vertexIndices, texCoords);
      FOcclusionQuery.EndQuery;
    end
    else if vertexIndices.Count > autoFlushVertexCount then
      FlushAccum(vertices, vertexIndices, texCoords);
  end
  else
    FTriangleCount := 0;
end;

// FlushAccum

class procedure TGLROAMPatch.FlushAccum(vertices: TAffineVectorList;
  vertexIndices: TIntegerList;
  texCoords: TTexPointList);
begin
  if vertexIndices.Count = 0 then
    Exit;

  if GL.ARB_vertex_buffer_object then
  begin
    FVBOVertHandle.AllocateHandle;
    FVBOVertHandle.BindBufferData(vertices.List, vertices.DataSize,
      GL_STREAM_DRAW_ARB);
    GL.VertexPointer(3, GL_FLOAT, 0, nil);

    FVBOTexHandle.AllocateHandle;
    FVBOTexHandle.BindBufferData(texCoords.List, texCoords.DataSize,
      GL_STREAM_DRAW_ARB);
    xgl.TexCoordPointer(2, GL_FLOAT, 0, nil);

    GL.DrawRangeElements(GL_TRIANGLES, 0, vertices.Count - 1, vertexIndices.Count,
      GL_UNSIGNED_INT, vertexIndices.List);
    GL.BindBuffer(GL_ARRAY_BUFFER_ARB, 0);
    GL.BindBuffer(GL_ELEMENT_ARRAY_BUFFER_ARB, 0);
  end
  else if GL.EXT_compiled_vertex_array and GL.EXT_draw_range_elements then
  begin
    GL.LockArrays(0, vertices.Count);
    GL.DrawRangeElements(GL_TRIANGLES, 0, vertices.Count - 1, vertexIndices.Count,
      GL_UNSIGNED_INT, vertexIndices.List);
    GL.UnLockArrays;
  end
  else
  begin
    GL.DrawElements(GL_TRIANGLES, vertexIndices.Count, GL_UNSIGNED_INT,
      vertexIndices.List);
  end;
  vertices.Count := 0;
  texCoords.Count := 0;
  vertexIndices.Count := 0;
end;

// RenderROAM

var
  renderRaster: PSmallIntRaster;
  renderIndices: PIntegerArray;
  renderVertices: TAffineVectorList;
  renderTexCoords: TTexPointList;

procedure RecursRender(const tri: PROAMTriangleNode;
  const left, right, apex: TROAMRenderPoint);
var
  half: TROAMRenderPoint;
  localIndices: PIntegerArray;
begin
  if Assigned(tri.leftChild) then
  begin  // = if node is split
    half.Y := (left.Y + right.Y) shr 1;
    half.X := (left.X + right.X) shr 1;
    renderTexCoords.AddNC(@half.X);
    half.Idx := renderVertices.AddNC(@half.X, renderRaster[half.Y][half.X]);
    RecursRender(tri.leftChild, apex, left, half);
    RecursRender(tri.rightChild, right, apex, half);
  end
  else
  begin
    localIndices := renderIndices;
    localIndices[0] := left.Idx;
    localIndices[1] := apex.Idx;
    localIndices[2] := right.Idx;
    renderIndices := PIntegerArray(@localIndices[3]);
  end;
end;

procedure TGLROAMPatch.RenderROAM(vertices: TAffineVectorList;
  vertexIndices: TIntegerList;
  texCoords: TTexPointList);

  procedure ROAMRenderPoint(var p: TROAMRenderPoint; anX, anY: integer);
  begin
    p.X := anX;
    p.Y := anY;
    p.Idx := vertices.Add(anX, anY, renderRaster[anY][anX]);
    texCoords.Add(anX, anY);
  end;

var
  rtl, rtr, rbl, rbr: TROAMRenderPoint;
begin
  renderVertices := vertices;
  renderTexCoords := texCoords;
  vertexIndices.AdjustCapacityToAtLeast(Sqr(FPatchSize) * 6 + 15000);
  // this is required, the actual item count is maintained out of the list scope
  vertexIndices.SetCountResetsMemory := False;
  renderIndices := @vertexIndices.List[vertexIndices.Count];

  renderRaster := FHeightData.SmallIntRaster;

  ROAMRenderPoint(rtl, 0, 0);
  ROAMRenderPoint(rtr, FPatchSize, 0);
  ROAMRenderPoint(rbl, 0, FPatchSize);
  ROAMRenderPoint(rbr, FPatchSize, FPatchSize);

  RecursRender(@vTriangleNodes[FTLNode], rbl, rtr, rtl);
  RecursRender(@vTriangleNodes[FBRNode], rtr, rbl, rbr);

  vertexIndices.Count := (PtrUInt(renderIndices) - PtrUInt(vertexIndices.List)) div
    SizeOf(integer);
end;

// RenderAsStrips

procedure TGLROAMPatch.RenderAsStrips(vertices: TAffineVectorList;
  vertexIndices: TIntegerList;
  texCoords: TTexPointList);

var
  x, y, baseTop, rowLength: integer;
  p: TAffineVector;
  row: PSmallIntArray;
  raster: PSmallIntRaster;
  tex: TTexPoint;
  verticesList: PAffineVector;
  texCoordsList: PTexPoint;
  indicesList: PInteger;
begin
  raster := FHeightData.SmallIntRaster;
  rowLength := FPatchSize + 1;
  // prepare vertex data
  vertices.Count := Sqr(rowLength);
  verticesList := PAffineVector(vertices.List);
  texCoords.Count := Sqr(rowLength);
  texCoordsList := PTexPoint(texCoords.List);
  for y := 0 to FPatchSize do
  begin
    p[1] := y;
    tex.T := p[1];
    row := raster[y];
    for x := 0 to FPatchSize do
    begin
      p[0] := x;
      tex.S := p[0];
      p[2] := row[x];
      verticesList^ := p;
      Inc(verticesList);
      texCoordsList^ := tex;
      Inc(texCoordsList);
    end;
  end;
  // build indices list
  baseTop := 0;
  vertexIndices.Count := (rowLength * 2 + 2) * FPatchSize - 1;
  indicesList := PInteger(vertexIndices.List);
  y := 0;
  while y < FPatchSize do
  begin
    if y > 0 then
    begin
      indicesList^ := baseTop + FPatchSize;
      Inc(indicesList);
    end;
    for x := baseTop + FPatchSize downto baseTop do
    begin
      indicesList^ := x;
      PIntegerArray(indicesList)[1] := rowLength + x;
      Inc(indicesList, 2);
    end;
    indicesList^ := baseTop + rowLength;
    Inc(baseTop, rowLength);
    PIntegerArray(indicesList)[1] := baseTop + rowLength;
    Inc(indicesList, 2);
    for x := baseTop to baseTop + FPatchSize do
    begin
      indicesList^ := rowLength + x;
      PIntegerArray(indicesList)[1] := x;
      Inc(indicesList, 2);
    end;
    indicesList^ := baseTop + FPatchSize;
    Inc(indicesList);
    Inc(baseTop, rowLength);
    Inc(y, 2);
  end;
  vertexIndices.Count := vertexIndices.Count - 1;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  FVBOVertHandle := TGLVBOArrayBufferHandle.Create;
  FVBOTexHandle := TGLVBOArrayBufferHandle.Create;
  FVBOIndicesHandle := TGLVBOElementArrayHandle.Create;

finalization

  FVBOVertHandle.Free;
  FVBOTexHandle.Free;
  FVBOIndicesHandle.Free;

  SetROAMTrianglesCapacity(0);

end.

