//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GeometryBB<p>

	Calculations and manipulations on Bounding Boxes.<p>

	<b>History : </b><font size=-1><ul>
      <li>20/04/08 - DaStr - Added a NullBoundingBox constant and
                              BoundingBoxesAreEqual() function (thanks Pascal)
      <li>19/09/07 - DaStr - Added OffsetBB(Point) procedures
      <li>31/08/07 - LC - Replaced TriangleIntersectAABB with a working (and faster) version
      <li>23/08/07 - LC - Added RayCastAABBIntersect
      <li>24/03/07 - DaStr - Added explicit pointer dereferencing
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
      <li>22/06/03 - MF - Added TBSphere for bounding spheres and classes to
                          determine whether one aabb/bsphere contains another
                          aabb/bsphere
      <li>21/06/03 - MF - Added IntersectAABBsAbsolute
      <li>08/05/03 - DanB - Added Plane/Triangle-AABB collisions (Matheus Degiovani)
      <li>07/02/03 - EG - Added IntersectAABBsAbsoluteXY (Dan Bartlett)
      <li>22/01/03 - EG - IntersectAABBs moved in (Bernd Klaiber)
      <li>04/09/03 - EG - New AABB functions
      <li>17/08/01 - EG - Removed "math" dependency
      <li>09/07/01 - EG - Added AABB types and functions
	    <li>31/03/01 - EG - Original Unit by Jacques Tur
	</ul></font>
}
unit GeometryBB;

interface

{$i GLScene.inc}

uses
  VectorGeometry;

type

   {: Structure for storing Bounding Boxes }
   THmgBoundingBox = array [0..7] of TVector;
   PHmgBoundingBox = ^THmgBoundingBox;

   {: Structure for storing Axis Aligned Bounding Boxes }
   TAABB = record
      min, max : TAffineVector;
   end;
   PAABB = ^TAABB;

   // TBSphere
   //
   {: Structure for storing BoundingSpheres. Similar to TAABB}
   TBSphere = record
      {: Center of Bounding Sphere }
      Center : TAffineVector;
      {: Radius of Bounding Sphere }
      Radius : single;
   end;

   // TClipRect
   //
   TClipRect = record
      Left, Top : Single;
      Right, Bottom : Single;
   end;

   {: Result type for space intersection tests, like AABBContainsAABB or
   BSphereContainsAABB }
   TSpaceContains = (scNoOverlap, scContainsFully, scContainsPartially);
   {: Structure for storing the corners of an AABB, used with ExtractAABBCorners}
   TAABBCorners = array[0..7] of TAffineVector;

const
  NullBoundingBox : THmgBoundingBox = ((0, 0, 0, 1),(0, 0, 0, 1),(0, 0, 0, 1),(0, 0, 0, 1),
                                       (0, 0, 0, 1),(0, 0, 0, 1),(0, 0, 0, 1),(0, 0, 0, 1)) ;

//------------------------------------------------------------------------------
// Bounding Box functions
//------------------------------------------------------------------------------


function BoundingBoxesAreEqual(const ABoundingBox1, ABoundingBox2: THmgBoundingBox): Boolean; overload;
function BoundingBoxesAreEqual(const ABoundingBox1, ABoundingBox2: PHmgBoundingBox): Boolean; overload;

{: Adds a BB into another BB.<p>
   The original BB (c1) is extended if necessary to contain c2. }
function AddBB(var c1 : THmgBoundingBox; const c2 : THmgBoundingBox) : THmgBoundingBox;
procedure AddAABB(var aabb : TAABB; const aabb1 : TAABB);

procedure SetBB(var c : THmgBoundingBox; const v : TVector);
procedure SetAABB(var bb : TAABB; const v : TVector);

procedure BBTransform(var c : THmgBoundingBox; const m : TMatrix);
procedure AABBTransform(var bb : TAABB; const m : TMatrix);
procedure AABBScale(var bb : TAABB; const v : TAffineVector);

function BBMinX(const c : THmgBoundingBox) : Single;
function BBMaxX(const c : THmgBoundingBox) : Single;
function BBMinY(const c : THmgBoundingBox) : Single;
function BBMaxY(const c : THmgBoundingBox) : Single;
function BBMinZ(const c : THmgBoundingBox) : Single;
function BBMaxZ(const c : THmgBoundingBox) : Single;

{: Resize the AABB if necessary to include p. }
procedure AABBInclude(var bb : TAABB; const p : TAffineVector);
{: Make an AABB that is formed by sweeping a sphere (or AABB) from Start to Dest}
procedure AABBFromSweep(var SweepAABB:TAABB; const Start,Dest:TVector; const Radius:Single);
{: Returns the intersection AABB of two AABBs.<p>
   If the AABBs don't intersect, will return a degenerated AABB (plane, line or point). }
function AABBIntersection(const aabb1, aabb2 : TAABB) : TAABB;

{: Extract AABB information from a BB. }
function BBToAABB(const aBB : THmgBoundingBox) : TAABB;
{: Converts an AABB to its canonical BB. }
function AABBToBB(const anAABB : TAABB) : THmgBoundingBox; overload;
{: Transforms an AABB to a BB. }
function AABBToBB(const anAABB : TAABB; const m : TMatrix) : THmgBoundingBox; overload;

{: Adds delta to min and max of the AABB. }
procedure OffsetAABB(var aabb : TAABB; const delta : TAffineVector); overload;
procedure OffsetAABB(var aabb : TAABB; const delta : TVector); overload;

{: Adds delta to min and max of the BB. }
procedure OffsetBB(var bb : THmgBoundingBox; const delta : TAffineVector); overload;
procedure OffsetBB(var bb : THmgBoundingBox; const delta : TVector); overload;
{: The same as above but uses AddPoint() instead of AddVector(). }
procedure OffsetBBPoint(var bb : THmgBoundingBox; const delta : TVector); overload;

{: Determines if two AxisAlignedBoundingBoxes intersect.<p>
   The matrices are the ones that convert one point to the other's AABB system }
function IntersectAABBs(const aabb1, aabb2 : TAABB; const m1To2, m2To1 : TMatrix) : Boolean; overload;
{: Checks whether two Bounding boxes aligned with the world axes collide in the XY plane.<p> }
function IntersectAABBsAbsoluteXY(const aabb1, aabb2 : TAABB) : Boolean;
{: Checks whether two Bounding boxes aligned with the world axes collide in the XZ plane.<p> }
function IntersectAABBsAbsoluteXZ(const aabb1, aabb2 : TAABB) : Boolean;
{: Checks whether two Bounding boxes aligned with the world axes collide.<p> }
function IntersectAABBsAbsolute(const aabb1, aabb2 : TAABB) : Boolean;
{: Checks whether one Bounding box aligned with the world axes fits within
another Bounding box.<p> }
function AABBFitsInAABBAbsolute(const aabb1, aabb2 : TAABB) : Boolean;

{: Checks if a point "p" is inside an AABB}
function PointInAABB(const p : TAffineVector; const aabb : TAABB) : Boolean; overload;
function PointInAABB(const p : TVector; const aabb : TAABB) : Boolean; overload;

{: Checks if a plane (given by the normal+d) intersects the AABB}
function PlaneIntersectAABB(Normal: TAffineVector; d: single; aabb: TAABB): boolean;
{: Checks if a triangle (given by vertices v1, v2 and v3) intersects an AABB}
function TriangleIntersectAABB(const aabb: TAABB; const v1, v2, v3: TAffineVector): boolean;

{: Extract the corners from an AABB}
procedure ExtractAABBCorners(const AABB: TAABB; var AABBCorners : TAABBCorners);

{: Convert an AABB to a BSphere}
procedure AABBToBSphere(const AABB : TAABB; var BSphere : TBSphere);
{: Convert a BSphere to an AABB }
procedure BSphereToAABB(const BSphere : TBSphere; var AABB : TAABB); overload;
function BSphereToAABB(const center : TAffineVector; radius : Single) : TAABB; overload;
function BSphereToAABB(const center : TVector; radius : Single) : TAABB; overload;

{: Determines to which extent one AABB contains another AABB}
function AABBContainsAABB(const mainAABB, testAABB : TAABB) : TSpaceContains;
{: Determines to which extent a BSphere contains an AABB}
function BSphereContainsAABB(const mainBSphere : TBSphere; const testAABB : TAABB) : TSpaceContains;
{: Determines to which extent one BSphere contains another BSphere}
function BSphereContainsBSphere(const mainBSphere, testBSphere : TBSphere) : TSpaceContains;
{: Determines to which extent an AABB contains a BSpher}
function AABBContainsBSphere(const mainAABB : TAABB; const testBSphere : TBSphere) : TSpaceContains;
{: Determines to which extent a plane contains a BSphere}
function PlaneContainsBSphere(const Location, Normal : TAffineVector; const testBSphere : TBSphere) : TSpaceContains;
{: Determines to which extent a frustum contains a BSphere}
function FrustumContainsBSphere(const Frustum : TFrustum; const testBSphere : TBSphere) : TSpaceContains;
{: Determines to which extent a frustum contains an AABB}
function FrustumContainsAABB(const Frustum : TFrustum; const testAABB : TAABB) : TSpaceContains;
{: Clips a position to an AABB }
function ClipToAABB(const v : TAffineVector; const AABB : TAABB) : TAffineVector;
{: Determines if one BSphere intersects another BSphere}
function BSphereIntersectsBSphere(const mainBSphere, testBSphere : TBSphere) : boolean;

{: Extend the clip rect to include given coordinate. }
procedure IncludeInClipRect(var clipRect : TClipRect; x, y : Single);
{: Projects an AABB and determines the extent of its projection as a clip rect. }
function AABBToClipRect(const aabb : TAABB; modelViewProjection : TMatrix;
                        viewportSizeX, viewportSizeY : Integer) : TClipRect;

{: Finds the intersection between a ray and an axis aligned bounding box. }
function RayCastAABBIntersect(const rayOrigin, rayDirection: TVector; const aabb: TAABB;
  out tNear, tFar: single): boolean; overload;
function RayCastAABBIntersect(const rayOrigin, rayDirection: TVector;
  const aabb: TAABB; intersectPoint: PVector = nil): boolean; overload;

type
   TPlanIndices = array [0..3] of Integer;
   TPlanBB = array [0..5] of TPlanIndices;
   TDirPlan = array [0..5] of Integer;

const
   cBBFront :  TPlanIndices = (0, 1, 2, 3);
   cBBBack :   TPlanIndices = (4, 5, 6, 7);
   cBBLeft :   TPlanIndices = (0, 4, 7, 3);
   cBBRight :  TPlanIndices = (1, 5, 6, 2);
   cBBTop :    TPlanIndices = (0, 1, 5, 4);
   cBBBottom : TPlanIndices = (2, 3, 7, 6);
   cBBPlans : TPlanBB = ( (0, 1, 2, 3),
                          (4, 5, 6, 7),
                          (0, 4, 7, 3),
                          (1, 5, 6, 2),
                          (0, 1, 5, 4),
                          (2, 3, 7, 6) );
   cDirPlan : TDirPlan = ( 0, 0, 1, 1, 2, 2 );

//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------
implementation
//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------
uses SysUtils;
//------------------------------------------------------------------------------
//----------------- BB functions -------------------------------------------
//------------------------------------------------------------------------------

// SetPlanBB
//
procedure SetPlanBB(var BB : THmgBoundingBox; const NumPlan : Integer; const Valeur : Double);
var
   i : Integer;
begin
   for i := 0 to 3 do
   begin
       BB[cBBPlans[NumPlan][i]][cDirPlan[NumPlan]] := Valeur;
       BB[cBBPlans[NumPlan][i]][3] := 1;
   end;
end;

// BoundingBoxesAreEqual (copy)
//
function BoundingBoxesAreEqual(const ABoundingBox1, ABoundingBox2: THmgBoundingBox): Boolean;
begin
  Result := CompareMem(@ABoundingBox1, @ABoundingBox2, SizeOf(THmgBoundingBox));
end;

// BoundingBoxesAreEqual (direct)
//
function BoundingBoxesAreEqual(const ABoundingBox1, ABoundingBox2: PHmgBoundingBox): Boolean;
begin
  Result := CompareMem(ABoundingBox1, ABoundingBox2, SizeOf(THmgBoundingBox));
end;

// AddBB
//
function AddBB(var c1 : THmgBoundingBox; const c2 : THmgBoundingBox) : THmgBoundingBox;

var
   i, j : Integer;
begin
   for i:=0 to 7 do begin
      for j:=0 to 3 do
          if c1[cBBFront[j]][0]<c2[i][0] then SetPlanBB(c1, 0, c2[i][0]);
      for j:=0 to 3 do
          if c1[cBBBack[j]][0]>c2[i][0] then SetPlanBB(c1, 1, c2[i][0]);
      for j:=0 to 3 do
          if c1[cBBLeft[j]][1]<c2[i][1] then SetPlanBB(c1, 2, c2[i][1]);
      for j:=0 to 3 do
          if c1[cBBRight[j]][1]>c2[i][1] then SetPlanBB(c1, 3, c2[i][1]);
      for j:=0 to 3 do
          if c1[cBBTop[j]][2]<c2[i][2] then SetPlanBB(c1, 4, c2[i][2]);
      for j:=0 to 3 do
          if c1[cBBBottom[j]][2]>c2[i][2] then SetPlanBB(c1, 5, c2[i][2]);
   end;
   Result:=c1;
end;

// AddAABB
//
procedure AddAABB(var aabb : TAABB; const aabb1 : TAABB);
begin
   if aabb1.min[0]<aabb.min[0] then aabb.min[0]:=aabb1.min[0];
   if aabb1.min[1]<aabb.min[1] then aabb.min[1]:=aabb1.min[1];
   if aabb1.min[2]<aabb.min[2] then aabb.min[2]:=aabb1.min[2];
   if aabb1.max[0]>aabb.max[0] then aabb.max[0]:=aabb1.max[0];
   if aabb1.max[1]>aabb.max[1] then aabb.max[1]:=aabb1.max[1];
   if aabb1.max[2]>aabb.max[2] then aabb.max[2]:=aabb1.max[2];
end;

// SetBB
//
procedure SetBB( var c : THmgBoundingBox; const v : TVector );
begin
   SetPlanBB( c, 0, v[0] );
   SetPlanBB( c, 1, -v[0] );
   SetPlanBB( c, 2, v[1] );
   SetPlanBB( c, 3, -v[1] );
   SetPlanBB( c, 4, v[2] );
   SetPlanBB( c, 5, -v[2] );
end;

// SetAABB
//
procedure SetAABB(var bb : TAABB; const v : TVector);
begin
   bb.max[0]:=Abs(v[0]);
   bb.max[1]:=Abs(v[1]);
   bb.max[2]:=Abs(v[2]);
   bb.min[0]:=-bb.max[0];
   bb.min[1]:=-bb.max[1];
   bb.min[2]:=-bb.max[2];
end;

// BBTransform
//
procedure BBTransform( var c : THmgBoundingBox; const m : TMatrix );
var
   i : Integer;
begin
   for i:=0 to 7 do
      c[i]:=VectorTransform(c[i], m);
end;

// AABBTransform
//
procedure AABBTransform(var bb : TAABB; const m : TMatrix);
var
   oldMin, oldMax : TAffineVector;
begin
   oldMin:=bb.min;
   oldMax:=bb.max;
   bb.min:=VectorTransform(oldMin , m);
   bb.max:=bb.min;
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMin[0], oldMin[1], oldMax[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMin[0], oldMax[1], oldMin[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMin[0], oldMax[1], oldMax[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMax[0], oldMin[1], oldMin[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMax[0], oldMin[1], oldMax[2]), m));
   AABBInclude(bb, VectorTransform(AffineVectorMake(oldMax[0], oldMax[1], oldMin[2]), m));
   AABBInclude(bb, VectorTransform(oldMax , m));
end;

//AABBScale
//
procedure AABBScale(var bb : TAABB; const v : TAffineVector);
begin
  ScaleVector(bb.min,v);
  ScaleVector(bb.max,v);
end;

//BBMinX
//
function BBMinX(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   Result:=c[0][0];
   for i:=1 to 7 do
      Result:=MinFloat(Result, c[i][0]);
end;

//BBMaxX
//
function BBMaxX(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   result := c[0][0];
   for i := 1 to 7 do
      result := MaxFloat( Result, c[i][0] );
end;

//BBMinY
//
function BBMinY(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   result := c[0][1];
   for i := 1 to 7 do
      Result := MinFloat( Result, c[i][1] );
end;

//BBMaxY
//
function BBMaxY(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   Result := c[0][1];
   for i := 1 to 7 do
      Result := MaxFloat( Result, c[i][1] );
end;

//BBMinZ
//
function BBMinZ(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   Result := c[0][2];
   for i := 1 to 7 do
      Result := MinFloat( Result, c[i][2] );
end;

// BBMaxZ
//
function BBMaxZ(const c : THmgBoundingBox ) : Single;
var
   i : Integer;
begin
   Result := c[0][2];
   for i := 1 to 7 do
      Result := MaxFloat( Result, c[i][2] );
end;

// AABBInclude
//
procedure AABBInclude(var bb : TAABB; const p : TAffineVector);
begin
   if p[0]<bb.min[0] then bb.min[0]:=p[0];
   if p[0]>bb.max[0] then bb.max[0]:=p[0];
   if p[1]<bb.min[1] then bb.min[1]:=p[1];
   if p[1]>bb.max[1] then bb.max[1]:=p[1];
   if p[2]<bb.min[2] then bb.min[2]:=p[2];
   if p[2]>bb.max[2] then bb.max[2]:=p[2];
end;

// AABBFromSweep
//
procedure AABBFromSweep(var SweepAABB:TAABB; const Start,Dest:TVector; const Radius:Single);
begin
   if Start[0]<Dest[0] then
   begin
     SweepAABB.min[0]:=Start[0]-radius;
     SweepAABB.max[0]:=Dest[0]+radius;
   end
   else
   begin
     SweepAABB.min[0]:=Dest[0]-radius;
     SweepAABB.max[0]:=Start[0]+radius;
   end;

   if Start[1]<Dest[1] then
   begin
     SweepAABB.min[1]:=Start[1]-radius;
     SweepAABB.max[1]:=Dest[1]+radius;
   end
   else
   begin
     SweepAABB.min[1]:=Dest[1]-radius;
     SweepAABB.max[1]:=Start[1]+radius;
   end;

   if Start[2]<Dest[2] then
   begin
     SweepAABB.min[2]:=Start[2]-radius;
     SweepAABB.max[2]:=Dest[2]+radius;
   end
   else
   begin
     SweepAABB.min[2]:=Dest[2]-radius;
     SweepAABB.max[2]:=Start[2]+radius;
   end;
end;

// AABBIntersection
//
function AABBIntersection(const aabb1, aabb2 : TAABB) : TAABB;
var
   i : Integer;
begin
   for i:=0 to 2 do begin
      Result.min[i]:=MaxFloat(aabb1.min[i], aabb2.min[i]);
      Result.max[i]:=MinFloat(aabb1.max[i], aabb2.max[i]);
   end;
end;

// BBToAABB
//
function BBToAABB(const aBB : THmgBoundingBox) : TAABB;
var
   i : Integer;
begin
   SetVector(Result.min, aBB[0]);
   SetVector(Result.max, aBB[0]);
   for i:=1 to 7 do begin
      if aBB[i][0]<Result.min[0] then
         Result.min[0]:=aBB[i][0];
      if aBB[i][0]>Result.max[0] then
         Result.max[0]:=aBB[i][0];
      if aBB[i][1]<Result.min[1] then
         Result.min[1]:=aBB[i][1];
      if aBB[i][1]>Result.max[1] then
         Result.max[1]:=aBB[i][1];
      if aBB[i][2]<Result.min[2] then
         Result.min[2]:=aBB[i][2];
      if aBB[i][2]>Result.max[2] then
         Result.max[2]:=aBB[i][2];
   end;
end;

// AABBToBB
//
function AABBToBB(const anAABB : TAABB) : THmgBoundingBox;
begin
   with anAABB do begin
      SetPlanBB( Result, 0, max[0] );
      SetPlanBB( Result, 1, min[0] );
      SetPlanBB( Result, 2, max[1] );
      SetPlanBB( Result, 3, min[1] );
      SetPlanBB( Result, 4, max[2] );
      SetPlanBB( Result, 5, min[2] );
   end;
end;

// AABBToBB
//
function AABBToBB(const anAABB : TAABB; const m : TMatrix) : THmgBoundingBox;
begin
   Result:=AABBToBB(anAABB);
   BBTransform(Result, m);
end;

// OffsetAABB
//
procedure OffsetAABB(var aabb : TAABB; const delta : TAffineVector);
begin
   AddVector(aabb.min, delta);
   AddVector(aabb.max, delta);
end;

// OffsetAABB
//
procedure OffsetAABB(var aabb : TAABB; const delta : TVector);
begin
   AddVector(aabb.min, delta);
   AddVector(aabb.max, delta);
end;

// OffsetBB
//
procedure OffsetBB(var bb : THmgBoundingBox; const delta : TAffineVector);
var
  I: Integer;
  TempVector: TVector;
begin
  TempVector := VectorMake(delta, 0);
  for I := 0 to 7 do
    AddVector(bb[I], TempVector);
end;

// OffsetAABB
//
procedure OffsetBB(var bb : THmgBoundingBox; const delta : TVector);
var
  I: Integer;
begin
  for I := 0 to 7 do
    AddVector(bb[I], delta);
end;

// OffsetBBPoint
//
procedure OffsetBBPoint(var bb : THmgBoundingBox; const delta : TVector);
var
  I: Integer;
begin
  for I := 0 to 7 do
    AddPoint(bb[I], delta);
end;

// IntersectCubes (AABBs)
//
function IntersectAABBs(const aabb1, aabb2 : TAABB;
                        const m1To2, m2To1 : TMatrix) : boolean;
const
  cWires : array[0..11,0..1] of Integer //Points of the wire
         = ((0,1),(1,2),(2,3),(3,0),
            (4,5),(5,6),(6,7),(7,4),
            (0,4),(1,5),(2,6),(3,7));
  cPlanes : array[0..5,0..3] of Integer //points of the planes
         = ((1,2,6,5), (2,3,7,6), (0,1,2,3), (0,3,7,4), (0,1,5,4), (5,6,7,4));

   procedure MakeAABBPoints(const AABB : TAABB; var pt : array of TVertex);
   begin
      with AABB do begin
         SetVector(pt[0], min[0], min[1], min[2]);
         SetVector(pt[1], max[0], min[1], min[2]);
         SetVector(pt[2], max[0], max[1], min[2]);
         SetVector(pt[3], min[0], max[1], min[2]);
         SetVector(pt[4], min[0], min[1], max[2]);
         SetVector(pt[5], max[0], min[1], max[2]);
         SetVector(pt[6], max[0], max[1], max[2]);
         SetVector(pt[7], min[0], max[1], max[2]);
      end;
   end;

   procedure MakePlanes(const pt : array of TVertex; var planes : array of THmgPlane);
   var
      i : Integer;
   begin
      for i:=0 to 5 do
         planes[i]:=PlaneMake(pt[cPlanes[i, 0]], pt[cPlanes[i, 1]], pt[cPlanes[i, 2]]);
   end;

var
  pt1, pt2: array[0..7] of TVertex;
  pt:TVertex;
  Planes2: array[0..5] of THmgPlane;
  i, t: integer;
  V: TVertex;
  P: TVector;
begin
  result:= false;

  //Build Points
  MakeAABBPoints(AABB1, pt1);
  MakeAABBPoints(AABB2, pt2);
  for i:=0 to 7 do
  begin
    pt:= VectorTransform(pt2[i], m2To1);
    //check for inclusion (points of Obj2 in Obj1)
    if IsInRange(pt[0], AABB1.Min[0], AABB1.Max[0]) and
      IsInRange(pt[1], AABB1.Min[1], AABB1.Max[1]) and
      IsInRange(pt[2], AABB1.Min[2], AABB1.Max[2]) then
    begin
      result:= true;
      exit;
    end;
  end;

  for i:=0 to 7 do
  begin
    pt1[i]:= VectorTransform(pt1[i], m1To2);
    //check for inclusion (points of Obj1 in Obj2)
    if IsInRange(pt1[i][0], AABB2.Min[0], AABB2.Max[0]) and
      IsInRange(pt1[i][1], AABB2.Min[1], AABB2.Max[1]) and
      IsInRange(pt1[i][2], AABB2.Min[2], AABB2.Max[2]) then
    begin
      result:= true;
      exit;
    end;
  end;

  //Build Planes
  MakePlanes(pt2, Planes2);

  //Wire test
  for i:=0 to 11 do
  begin
    for t:=0 to 5 do
    begin
      //Build Vector of Ray
      V:= VectorSubtract(pt1[cWires[i,0]], pt1[cWires[i,1]]);
      if IntersectLinePlane(VectorMake(pt1[cWires[i,0]]), VectorMake(V), Planes2[t], @P) = 1 then
      begin
        //check point in Wire
        if IsInRange(P[0], pt1[cWires[i,0]][0], pt1[cWires[i,1]][0]) and
          IsInRange(P[1], pt1[cWires[i,0]][1], pt1[cWires[i,1]][1]) and
          IsInRange(P[2], pt1[cWires[i,0]][2], pt1[cWires[i,1]][2]) then
        begin
          //check point in Plane
          if IsInRange(P[0], pt2[cPlanes[t, 0]][0], pt2[cPlanes[t, 2]][0]) and
            IsInRange(P[1], pt2[cPlanes[t, 0]][1], pt2[cPlanes[t, 2]][1]) and
            IsInRange(P[2], pt2[cPlanes[t, 0]][2], pt2[cPlanes[t, 2]][2]) then
          begin
            result:= true;
            exit;
          end;
        end;
      end;
    end;
  end;
end;

// IntersectAABBsAbsoluteXY (AABBs)
//
function IntersectAABBsAbsoluteXY(const aabb1, aabb2 : TAABB) : Boolean;
begin
  result:= false;

  if (AABB2.min[0]>AABB1.max[0])or (AABB2.min[1]>AABB1.max[1]) then Exit
  else if (AABB2.max[0]<AABB1.min[0])or (AABB2.max[1]<AABB1.min[1]) then Exit
  else Result:=true;

end;

function IntersectAABBsAbsoluteXZ(const aabb1, aabb2 : TAABB) : Boolean;
begin
  result := 
   ((AABB1.min[0]<AABB2.max[0]) and
    (AABB1.min[2]<AABB2.max[2]) and

    (AABB2.min[0]<AABB1.max[0]) and
    (AABB2.min[2]<AABB1.max[2]));
end;

// IntersectAABBsAbsolute
//
function IntersectAABBsAbsolute(const aabb1, aabb2 : TAABB) : Boolean;
begin
  result := not
   ((AABB1.min[0]>AABB2.max[0]) or
    (AABB1.min[1]>AABB2.max[1]) or
    (AABB1.min[2]>AABB2.max[2]) or

    (AABB2.min[0]>AABB1.max[0]) or
    (AABB2.min[1]>AABB1.max[1]) or
    (AABB2.min[2]>AABB1.max[2]));
end;

// IntersectAABBsAbsolute
//
function AABBFitsInAABBAbsolute(const aabb1, aabb2 : TAABB) : Boolean;
begin
  // AABB1 fits completely inside AABB2?
  // AABB1 min must be >= to AABB2 min
  // AABB1 max must be <= to AABB2 max

  result :=
    (AABB1.min[0]>=AABB2.min[0]) and
    (AABB1.min[1]>=AABB2.min[1]) and
    (AABB1.min[2]>=AABB2.min[2]) and

    (AABB1.max[0]<=AABB2.max[0]) and
    (AABB1.max[1]<=AABB2.max[1]) and
    (AABB1.max[2]<=AABB2.max[2]);
end;

// PointInAABB (affine)
//
function PointInAABB(const p : TAffineVector; const aabb : TAABB) : Boolean;
begin
   Result:=    (p[0]<=aabb.max[0]) and (p[0]>=aabb.min[0])
           and (p[1]<=aabb.max[1]) and (p[1]>=aabb.min[1])
           and (p[2]<=aabb.max[2]) and (p[2]>=aabb.min[2]);
end;

// PointInAABB (hmg)
//
function PointInAABB(const p : TVector; const aabb : TAABB) : Boolean;
begin
   Result:=    (p[0]<=aabb.max[0]) and (p[0]>=aabb.min[0])
           and (p[1]<=aabb.max[1]) and (p[1]>=aabb.min[1])
           and (p[2]<=aabb.max[2]) and (p[2]>=aabb.min[2]);
end;

// PlaneIntersectAABB
//
function PlaneIntersectAABB(Normal: TAffineVector; d: single; aabb: TAABB): boolean;
var
vmax, vmin: TAffineVector;
i: integer;
begin
     result:= false;
     for i:= 0 to 2 do
          if normal[i] > 0.0 then begin
               vMin[i]:= aabb.min[i];
               vMax[i]:= aabb.max[i];
          end else begin
               vMin[i]:= aabb.max[i];
               vMax[i]:= aabb.min[i];
          end;

     if VectorDotProduct(normal, vmin) + d > 0 then Exit;
     if VectorDotProduct(normal, vmax) + d >= 0 then result:= true;
end;

procedure FindMinMax(x0,x1,x2: single; out min, max: single);
begin
  min:= x0;
  max:= x0;
  if (x1 < min) then
    min:= x1;
  if (x1 > max) then
    max:= x1;
  if (x2 < min) then
    min:= x2;
  if (x2 > max) then
    max:= x2;
end;

function planeBoxOverlap(const normal: TAffineVector; d: single; const maxbox: TAffineVector): boolean;
var
  q: integer;
  vmin, vmax: TAffineVector;
begin
  result:= false;

  for q := 0 to 2 do
  begin
    if (normal[q] > 0.0) then
    begin
      vmin[q]:= -maxbox[q];
      vmax[q]:=  maxbox[q];
    end
    else
    begin
      vmin[q]:=  maxbox[q];
      vmax[q]:= -maxbox[q];
    end;
  end;

  if (VectorDotProduct(normal, vmin)+d) > 0 then
    exit;

  if (VectorDotProduct(normal, vmax)+d) >= 0 then
    result:= true;
end;

// TriangleIntersectAABB
//
function TriangleIntersectAABB(const aabb: TAABB;
  const v1, v2, v3: TAffineVector): boolean;
// Original source code by Tomas Akenine-M�ller
// Based on the paper "Fast 3D Triangle-Box Overlap Testing"
// http://www.cs.lth.se/home/Tomas_Akenine_Moller/pubs/tribox.pdf
// http://jgt.akpeters.com/papers/AkenineMoller01/ (code)

//    use separating axis theorem to test overlap between triangle and box
//    need to test for overlap in these directions:
//    1) the (x,y,z)-directions (actually, since we use the AABB of the triangle
//       we do not even need to test these)
//    2) normal of the triangle
//    3) crossproduct(edge from tri, {x,y,z}-directin)
//       this gives 3x3=9 more tests
var
  boxcenter, boxhalfsize: TAffineVector;
  tv0, tv1, tv2: TAffineVector;
  min, max, d, p0, p1, p2, rad, fex, fey, fez: single;
  normal, e0, e1, e2: TAffineVector;
begin
  result:= false;

  boxhalfsize:= VectorSubtract(VectorScale(aabb.max, 0.5), VectorScale(aabb.min, 0.5));
  boxcenter:= VectorAdd(VectorScale(aabb.max, 0.5), VectorScale(aabb.min, 0.5));
  // move everything so that the boxcenter is in (0,0,0)
  VectorSubtract(v1, boxcenter, tv0);
  VectorSubtract(v2, boxcenter, tv1);
  VectorSubtract(v3, boxcenter, tv2);

   // compute triangle edges
  VectorSubtract(tv1, tv0, e0);
  VectorSubtract(tv2, tv1, e1);
  VectorSubtract(tv0, tv2, e2);

  // Bullet 3:
  //  test the 9 tests first (this was faster)
  fex:= abs(e0[0]);
  fey:= abs(e0[1]);
  fez:= abs(e0[2]);

  //  AXISTEST_X01(e0[Z], e0[Y], fez, fey);
  p0:= e0[2]*tv0[1] - e0[1]*tv0[2];
  p2:= e0[2]*tv2[1] - e0[1]*tv2[2];
  min:= MinFloat(p0, p2);
  max:= MaxFloat(p0, p2);
  rad:= fez * boxhalfsize[1] + fey * boxhalfsize[2];
  if (min > rad) or (max < -rad) then
    exit;

  //  AXISTEST_Y02(e0[Z], e0[X], fez, fex);
  p0:= -e0[2]*tv0[0] + e0[0]*tv0[2];
  p2:= -e0[2]*tv2[0] + e0[0]*tv2[2];
  min:= MinFloat(p0, p2);
  max:= MaxFloat(p0, p2);
  rad:= fez * boxhalfsize[0] + fex * boxhalfsize[2];
  if (min > rad) or (max < -rad) then
    exit;

  //  AXISTEST_Z12(e0[Y], e0[X], fey, fex);
  p1:= e0[1]*tv1[0] - e0[0]*tv1[1];
  p2:= e0[1]*tv2[0] - e0[0]*tv2[1];
  min:= MinFloat(p1, p2);
  max:= MaxFloat(p1, p2);
  rad:= fey * boxhalfsize[0] + fex * boxhalfsize[1];
  if (min > rad) or (max < -rad) then
    exit;

  fex:= abs(e1[0]);
  fey:= abs(e1[1]);
  fez:= abs(e1[2]);
  //  AXISTEST_X01(e1[Z], e1[Y], fez, fey);
  p0:= e1[2]*tv0[1] - e1[1]*tv0[2];
  p2:= e1[2]*tv2[1] - e1[1]*tv2[2];
  min:= MinFloat(p0, p2);
  max:= MaxFloat(p0, p2);
  rad:= fez * boxhalfsize[1] + fey * boxhalfsize[2];
  if (min > rad) or (max < -rad) then
    exit;

  //  AXISTEST_Y02(e1[Z], e1[X], fez, fex);
  p0:= -e1[2]*tv0[0] + e1[0]*tv0[2];
  p2:= -e1[2]*tv2[0] + e1[0]*tv2[2];
  min:= MinFloat(p0, p2);
  max:= MaxFloat(p0, p2);
  rad:= fez * boxhalfsize[0] + fex * boxhalfsize[2];
  if (min > rad) or (max < -rad) then
    exit;

  //  AXISTEST_Z0(e1[Y], e1[X], fey, fex);
  p0:= e1[1]*tv0[0] - e1[0]*tv0[1];
  p1:= e1[1]*tv1[0] - e1[0]*tv1[1];
  min:= MinFloat(p0, p1);
  max:= MaxFloat(p0, p1);
  rad:= fey * boxhalfsize[0] + fex * boxhalfsize[1];
  if (min > rad) or (max < -rad) then
    exit;

  fex:= abs(e2[0]);
  fey:= abs(e2[1]);
  fez:= abs(e2[2]);
  //  AXISTEST_X2(e2[Z], e2[Y], fez, fey);
  p0:= e2[2]*tv0[1] - e2[1]*tv0[2];
  p1:= e2[2]*tv1[1] - e2[1]*tv1[2];
  min:= MinFloat(p0, p1);
  max:= MaxFloat(p0, p1);
  rad:= fez * boxhalfsize[1] + fey * boxhalfsize[2];
  if (min > rad) or (max < -rad) then
    exit;

//  AXISTEST_Y1(e2[Z], e2[X], fez, fex);
  p0:= -e2[2]*tv0[0] + e2[0]*tv0[2];
  p1:= -e2[2]*tv1[0] + e2[0]*tv1[2];
  min:= MinFloat(p0, p1);
  max:= MaxFloat(p0, p1);
  rad:= fez * boxhalfsize[0] + fex * boxhalfsize[2];
  if (min > rad) or (max < -rad) then
    exit;

  //  AXISTEST_Z12(e2[Y], e2[X], fey, fex);
  p1:= e2[1]*tv1[0] - e2[0]*tv1[1];
  p2:= e2[1]*tv2[0] - e2[0]*tv2[1];
  min:= MinFloat(p1, p2);
  max:= MaxFloat(p1, p2);
  rad:= fey * boxhalfsize[0] + fex * boxhalfsize[1];
  if (min > rad) or (max < -rad) then
    exit;

  // Bullet 1:
  //  first test overlap in the {x,y,z}-directions
  //  find min, max of the triangle each direction, and test for overlap in
  //  that direction -- this is equivalent to testing a minimal AABB around 
  //  the triangle against the AABB

  // test in X-direction
  FindMinMax(tv0[0], tv1[0], tv2[0], min, max);
  if (min > boxhalfsize[0]) or (max < -boxhalfsize[0]) then
    exit;

  // test in Y-direction
  FindMinMax(tv0[1], tv1[1], tv2[1], min, max);
  if (min > boxhalfsize[1]) or (max < -boxhalfsize[1]) then
    exit;

  // test in Z-direction
  FindMinMax(tv0[2], tv1[2], tv2[2], min, max);
  if (min > boxhalfsize[2]) or (max < -boxhalfsize[2]) then
    exit;

  // Bullet 2:
  //  test if the box intersects the plane of the triangle
  //  compute plane equation of triangle: normal * x + d = 0
  VectorCrossProduct(e0, e1, normal);
  d:= -VectorDotProduct(normal, tv0); // plane eq: normal.x + d = 0
  if not planeBoxOverlap(normal, d, boxhalfsize) then
    exit;

  // box and triangle overlaps
  result:= true;
end;

// ExtractAABBCorners
//
procedure ExtractAABBCorners(const AABB: TAABB; var AABBCorners : TAABBCorners);
begin
  MakeVector(AABBCorners[0], AABB.min[0], AABB.min[1], AABB.min[2]);
  MakeVector(AABBCorners[1], AABB.min[0], AABB.min[1], AABB.max[2]);
  MakeVector(AABBCorners[2], AABB.min[0], AABB.max[1], AABB.min[2]);
  MakeVector(AABBCorners[3], AABB.min[0], AABB.max[1], AABB.max[2]);

  MakeVector(AABBCorners[4], AABB.max[0], AABB.min[1], AABB.min[2]);
  MakeVector(AABBCorners[5], AABB.max[0], AABB.min[1], AABB.max[2]);
  MakeVector(AABBCorners[6], AABB.max[0], AABB.max[1], AABB.min[2]);
  MakeVector(AABBCorners[7], AABB.max[0], AABB.max[1], AABB.max[2]);
end;

//  AABBToBSphere
//
procedure AABBToBSphere(const AABB : TAABB; var BSphere : TBSphere);
begin
  BSphere.Center := VectorScale(VectorAdd(AABB.min, AABB.max), 0.5);
  BSphere.Radius := VectorDistance(AABB.min, AABB.max) * 0.5;
end;

//  BSphereToAABB (bsphere)
//
procedure BSphereToAABB(const BSphere : TBSphere; var AABB : TAABB);
begin
   AABB.min:=VectorSubtract(BSphere.Center, BSphere.Radius);
   AABB.max:=VectorAdd(BSphere.Center, BSphere.Radius);
end;

// BSphereToAABB (affine center, radius)
//
function BSphereToAABB(const center : TAffineVector; radius : Single) : TAABB;
begin
   Result.min:=VectorSubtract(center, radius);
   Result.max:=VectorAdd(center, radius);
end;

// BSphereToAABB (hmg center, radius)
//
function BSphereToAABB(const center : TVector; radius : Single) : TAABB;
begin
   SetVector(Result.min, VectorSubtract(center, radius));
   SetVector(Result.max, VectorAdd(center, radius));
end;

//  AABBContainsAABB
//
function AABBContainsAABB(const mainAABB, testAABB : TAABB) : TSpaceContains;
begin
  // AABB1 fits completely inside AABB2?
  // AABB1 min must be >= to AABB2 min
  // AABB1 max must be <= to AABB2 max

  if
   ((mainAABB.min[0]<testAABB.max[0]) and
    (mainAABB.min[1]<testAABB.max[1]) and
    (mainAABB.min[2]<testAABB.max[2]) and

    (testAABB.min[0]<mainAABB.max[0]) and
    (testAABB.min[1]<mainAABB.max[1]) and
    (testAABB.min[2]<mainAABB.max[2])) then
  begin
    if(testAABB.min[0]>=mainAABB.min[0]) and
      (testAABB.min[1]>=mainAABB.min[1]) and
      (testAABB.min[2]>=mainAABB.min[2]) and

      (testAABB.max[0]<=mainAABB.max[0]) and
      (testAABB.max[1]<=mainAABB.max[1]) and
      (testAABB.max[2]<=mainAABB.max[2]) then
      result := scContainsFully
    else
      result := scContainsPartially;
  end else
    result := scNoOverlap;
end;

//  AABBContainsBSphere
//
function AABBContainsBSphere(const mainAABB : TAABB; const testBSphere : TBSphere) : TSpaceContains;
var
  testAABB : TAABB;
begin
  BSphereToAABB(testBSphere, testAABB);
  result := AABBContainsAABB(mainAABB, testAABB);
end;

function PlaneContainsBSphere(const Location, Normal : TAffineVector; const testBSphere : TBSphere) : TSpaceContains;
var
  Dist : single;
begin
  Dist := PointPlaneDistance(testBSphere.Center, Location, Normal);

  if Dist > testBSphere.Radius then
    result := scNoOverlap
  else if abs(Dist)<= testBSphere.Radius then
    result := scContainsPartially
  else
    result := scContainsFully;
end;

// FrustumContainsBSphere
//
function FrustumContainsBSphere(const Frustum : TFrustum; const testBSphere : TBSphere) : TSpaceContains;
var
  negRadius : Single;
  HitCount : integer;
  Distance : single;
  i : integer;
type
  TPlaneArray = array[0..5] of THmgPlane;
begin
  negRadius:=-testBSphere.Radius;

  HitCount := 0;

  // This would be fractionally faster to unroll, but oh so ugly!?
  for i := 0 to 5 do
  begin
    Distance := PlaneEvaluatePoint(TPlaneArray(frustum)[i], testBSphere.Center);
    if Distance<negRadius then begin
      result := scNoOverlap;
      exit;
    end else if Distance >= testBSphere.Radius then
      inc(HitCount);
  end;//}

  if HitCount=6 then
    result := scContainsFully
  else
    result := scContainsPartially;
end;

// FrustumContainsBSphere
// see http://www.flipcode.com/articles/article_frustumculling.shtml
function FrustumContainsAABB(const Frustum : TFrustum; const testAABB : TAABB) : TSpaceContains;
type
  TPlaneArray = array[0..5] of THmgPlane;
var
  iPlane, iCorner : integer;
  PointIn : boolean;
  AABBCorners : TAABBCorners;
  InCount : integer;
  TotalIn : integer;
begin
  ExtractAABBCorners(testAABB, AABBCorners);

  TotalIn := 0;
  // test all 8 corners against the 6 sides
	// if all points are behind 1 specific plane, we are out
	// if we are in with all points, then we are fully in

  // For each plane
  for iPlane := Low(TPlaneArray) to High(TPlaneArray) do begin
    // We're about to test 8 corners
    InCount := 8;
    PointIn := true;

    // For each corner
    for iCorner := Low(AABBCorners) to High(AABBCorners) do begin
      if PlaneEvaluatePoint(TPlaneArray(Frustum)[iPlane], AABBCorners[iCorner])<0 then begin
        PointIn := false;
        dec(InCount);
      end;
    end;

    if InCount=0 then begin
      result := scNoOverlap;
      exit;
    end

    else if PointIn then
      inc(TotalIn);
  end;

  if TotalIn=6 then
    result := scContainsFully
  else
    result := scContainsPartially;
end;

//  BSphereContainsAABB
//
function BSphereContainsAABB(const mainBSphere : TBSphere; const testAABB : TAABB) : TSpaceContains;
var
  r2: single;
  ClippedCenter : TAffineVector;

  AABBCorners : TAABBCorners;
  CornerHitCount : integer;
begin
  r2 := sqr(mainBSphere.Radius);

  ClippedCenter := ClipToAABB(mainBSphere.Center, testAABB);

  if VectorDistance2(ClippedCenter, mainBSphere.Center) < r2 then
  begin
    ExtractAABBCorners(testAABB, AABBCorners);

    CornerHitCount := 0;
    // BSphere fully contains aabb if all corners of aabb are within bsphere.
    if (VectorDistance2(mainBSphere.Center, AABBCorners[0]) < r2) then inc(CornerHitCount);
    if (VectorDistance2(mainBSphere.Center, AABBCorners[1]) < r2) then inc(CornerHitCount);
    if (VectorDistance2(mainBSphere.Center, AABBCorners[2]) < r2) then inc(CornerHitCount);
    if (VectorDistance2(mainBSphere.Center, AABBCorners[3]) < r2) then inc(CornerHitCount);
    if (VectorDistance2(mainBSphere.Center, AABBCorners[4]) < r2) then inc(CornerHitCount);
    if (VectorDistance2(mainBSphere.Center, AABBCorners[5]) < r2) then inc(CornerHitCount);
    if (VectorDistance2(mainBSphere.Center, AABBCorners[6]) < r2) then inc(CornerHitCount);
    if (VectorDistance2(mainBSphere.Center, AABBCorners[7]) < r2) then inc(CornerHitCount);

    if CornerHitCount=7 then
      result := scContainsFully
    else
      result := scContainsPartially;
  end else
    result := scNoOverlap;
end;

//  BSphereContainsBSphere
//
function BSphereContainsBSphere(const mainBSphere, testBSphere : TBSphere) : TSpaceContains;
var
  d2 : single;
begin
  d2 := VectorDistance2(mainBSphere.Center, testBSphere.Center);

  if d2<sqr(mainBSphere.Radius+testBSphere.Radius) then
  begin
    if d2<sqr(mainBSphere.Radius-testBSphere.Radius) then
      result := scContainsFully
    else
      result := scContainsPartially;
  end else
    result := scNoOverlap;
end;

//  BSphereIntersectsBSphere
//
function BSphereIntersectsBSphere(const mainBSphere, testBSphere : TBSphere) : boolean;
begin
  result := VectorDistance2(mainBSphere.Center, testBSphere.Center)<sqr(mainBSphere.Radius+testBSphere.Radius);
end;

//  ClipToAABB
//
function ClipToAABB(const v : TAffineVector; const AABB : TAABB) : TAffineVector;
begin
  result := v;

  if result[0]<AABB.min[0] then result[0] := AABB.min[0];
  if result[1]<AABB.min[1] then result[1] := AABB.min[1];
  if result[2]<AABB.min[2] then result[2] := AABB.min[2];

  if result[0]>AABB.max[0] then result[0] := AABB.max[0];
  if result[1]>AABB.max[1] then result[1] := AABB.max[1];
  if result[2]>AABB.max[2] then result[2] := AABB.max[2];
end;

// IncludeInClipRect
//
procedure IncludeInClipRect(var clipRect : TClipRect; x, y : Single);
begin
   with clipRect do begin
      if x<Left then Left:=x;
      if x>Right then Right:=x;
      if y<Top then Top:=y;
      if y>Bottom then Bottom:=y;
   end;
end;

// AABBToClipRect
//
function AABBToClipRect(const aabb : TAABB; modelViewProjection : TMatrix;
                        viewportSizeX, viewportSizeY : Integer) : TClipRect;
var
   i : Integer;
   v, vt : TVector;
   minmax : array [0..1] of PAffineVector;
begin
   minmax[0]:=@aabb.min;
   minmax[1]:=@aabb.max;
   v[3]:=1;
   for i:=0 to 7 do begin
      v[0]:=minmax[i and 1]^[0];
      v[1]:=minmax[(i shr 1) and 1]^[1];
      v[2]:=minmax[(i shr 2) and 1]^[2];

      // Project
      vt:=VectorTransform(v, modelViewProjection);
      ScaleVector(vt, 1/vt[3]);

      // Convert to screen coordinates
      if i>0 then
         IncludeInClipRect(Result, viewportSizeX*(vt[0]+1)*0.5, viewportSizeY*(vt[1]+1)*0.5)
      else begin
         Result.Left:=viewportSizeX*(vt[0]+1)*0.5;
         Result.Top:=viewportSizeY*(vt[1]+1)*0.5;
         Result.Right:=Result.Left;
         Result.Bottom:=Result.Top;
      end;
   end;
end;

function RayCastAABBIntersect(const rayOrigin, rayDirection: TVector; const aabb: TAABB;
  out tNear, tFar: single): boolean; overload;
const
  Infinity    =  1.0 / 0.0;
var
  p: integer;
  invDir: double;
  t0, t1, tmp: single;
begin
  result:= false;

  tNear:= -Infinity;
  tFar:= Infinity;

  for p:= 0 to 2 do
  begin
    if (rayDirection[p] = 0) then
    begin
      if ((rayOrigin[p] < aabb.min[p]) or (rayOrigin[p] > aabb.max[p])) then
        exit;
    end
    else
    begin
      invDir:= 1 / rayDirection[p];
      t0:= (aabb.min[p] - rayOrigin[p]) * invDir;
      t1:= (aabb.max[p] - rayOrigin[p]) * invDir;

      if (t0 > t1) then
      begin
        tmp:= t0;
        t0:= t1;
        t1:= tmp;
      end;

      if (t0 > tNear) then
        tNear:= t0;
      if (t1 < tFar) then
        tFar:= t1;

      if ((tNear > tFar) or (tFar < 0)) then
        exit;
    end;
  end;

  result:= true;
end;

function RayCastAABBIntersect(const rayOrigin, rayDirection: TVector;
  const aabb: TAABB; intersectPoint: PVector = nil): boolean; overload;
var
  tNear, tFar: single;
begin
  result:= RayCastAABBIntersect(rayOrigin, rayDirection, aabb, tNear, tFar);

  if result and assigned(intersectPoint) then
  begin
    if tNear >= 0 then
      // origin outside the box
      intersectPoint^:= VectorCombine(rayOrigin, rayDirection, 1, tNear)
    else
      // origin inside the box, near is "behind", use far
      intersectPoint^:= VectorCombine(rayOrigin, rayDirection, 1, tFar);
  end;
end;


end.
