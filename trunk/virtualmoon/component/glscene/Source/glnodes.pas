//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLNodes<p>

   Nodes are used to describe lines, polygons + more.<p>

	<b>History : </b><font size=-1><ul>
      <li>26/11/09 - DaStr - Improved Lazarus compatibility (merged from gls4laz)
                             (thanks Predator) (BugtrackerID = 2893580)
      <li>22/11/09 - DaStr - Improved Unix compatibility (merged from gls4laz)
                             (thanks Predator) (BugtrackerID = 2893580)
      <li>14/07/09 - DaStr - Added $I GLScene.inc
      <li>05/10/08 - DanB - Created from GLMisc.pas split
   </ul></font>
}
unit GLNodes;

interface

uses Classes, VectorGeometry, OpenGL1x, BaseClasses, GLCoordinates, Spline;

{$I GLScene.inc}

type
	// TGLNode
	//
	TGLNode = class (TCollectionItem)
	   private
	      { Private Declarations }
			FCoords : TVector;
			procedure SetAsVector(const value: TVector);
			procedure SetAsAffineVector(const value : TAffineVector);
         function GetAsAffineVector : TAffineVector;
			procedure SetCoordinate(AIndex: Integer; AValue: TGLFloat);

	   protected
	      { Protected Declarations }
         function StoreCoordinate(AIndex: Integer) : Boolean;

         function GetDisplayName : String; override;

      public
	      { Public Declarations }
	      constructor Create(ACollection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         function AsAddress : PGLFloat;
         {: The coordinates viewed as a vector.<p>
            Assigning a value to this property will trigger notification events,
            if you don't want so, use DirectVector instead. }
			property AsVector : TVector read FCoords write SetAsVector;
         {: The coordinates viewed as an affine vector.<p>
            Assigning a value to this property will trigger notification events,
            if you don't want so, use DirectVector instead.<br>
            The W component is automatically adjustes depending on style. }
			property AsAffineVector : TAffineVector read GetAsAffineVector write SetAsAffineVector;

			property W: TGLFloat index 3 read FCoords[3] write SetCoordinate stored StoreCoordinate;

	   published
	      { Published Declarations }
			property X: TGLFloat index 0 read FCoords[0] write SetCoordinate stored StoreCoordinate;
			property Y: TGLFloat index 1 read FCoords[1] write SetCoordinate stored StoreCoordinate;
			property Z: TGLFloat index 2 read FCoords[2] write SetCoordinate stored StoreCoordinate;
	end;


	// TGLNodes
	//
	TGLNodes = class (TOwnedCollection)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }
         procedure SetItems(index : Integer; const val : TGLNode);
	      function GetItems(index : Integer) : TGLNode;
         procedure Update(Item: TCollectionItem); override;

      public
	      { Public Declarations }
         constructor Create(AOwner : TPersistent; AItemClass: TCollectionItemClass = nil);
         function CreateCopy(AOwner : TPersistent) : TGLNodes;

         function Add : TGLNode;
	      function FindItemID(ID : Integer) : TGLNode;
	      property Items[index : Integer] : TGLNode read GetItems write SetItems; default;
         function First : TGLNode;
         function Last : TGLNode;

         procedure NotifyChange; virtual;
         procedure EndUpdate; override;

         procedure AddNode(const coords : TGLCustomCoordinates); overload;
         procedure AddNode(const X, Y, Z: TGLfloat); overload;
         procedure AddNode(const value : TVector); overload;
         procedure AddNode(const value : TAffineVector); overload;
         procedure AddXYArc(xRadius, yRadius : Single;
                            startAngle, stopAngle : Single;
                            nbSegments : Integer;
                            const center : TAffineVector);

         //: Calculates and returns the barycenter of the nodes
         function Barycenter : TAffineVector;
         {: Computes normal based on the 1st three nodes.<p>
            Returns NullVector if there are less than 3 nodes. }
         function Normal : TAffineVector;
         //: Returns normalized vector Nodes[i+1]-Nodes[i]
         function Vector(i : Integer) : TAffineVector;

         {: Calculates the extents of the nodes (min-max for all coordinates).<p>
            The returned values are also the two corners of the axis-aligned
            bounding box. }
         procedure GetExtents(var min, max : TAffineVector);
         //: Translate all nodes
         procedure Translate(const tv : TAffineVector);
         //: Scale all node coordinates
         procedure Scale(const fv : TAffineVector); overload;
         //: Scale all node coordinates
         procedure Scale(f : Single); overload;
         //: Rotate nodes around Y axis by the given angle (degrees)
         procedure RotateAroundX(angle : Single);
         //: Rotate nodes around Y axis by the given angle (degrees)
         procedure RotateAroundY(angle : Single);
         //: Rotate nodes around Y axis by the given angle (degrees)
         procedure RotateAroundZ(angle : Single);

         procedure RenderTesselatedPolygon(ATextured : Boolean;
                                           ANormal : PAffineVector = nil;
                                           ASplineDivisions : Integer = 1;
                                           AInvertNormals : Boolean = False);

         function CreateNewCubicSpline : TCubicSpline;

   end;

   TGLNodesClass = class of TGLNodes;


implementation

uses SysUtils, XOpenGL;

// ------------------
// ------------------ TGLNode ------------------
// ------------------

// Create
//
constructor TGLNode.Create(ACollection : TCollection);
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

// Assign
//
procedure TGLNode.Assign(Source: TPersistent);
begin
	if Source is TGLNode then begin
      FCoords:=TGLNode(Source).FCoords;
	end else inherited;
end;

// GetDisplayName
//
function TGLNode.GetDisplayName : String;
begin
	Result:=Format('%.4f; %.4f; %.4f', [X, Y, Z]);
end;

// AsAddress
//
function TGLNode.AsAddress : PGLFloat;
begin
   Result:=@FCoords;
end;

// SetAsVector
//
procedure TGLNode.SetAsVector(const value: TVector);
begin
	FCoords:=Value;
   (Collection as TGLNodes).NotifyChange;
end;

// SetAsAffineVector
//
procedure TGLNode.SetAsAffineVector(const value : TAffineVector);
begin
   VectorGeometry.SetVector(FCoords, value);
   (Collection as TGLNodes).NotifyChange;
end;

// GetAsAffineVector
//
function TGLNode.GetAsAffineVector : TAffineVector;
begin
   VectorGeometry.SetVector(Result, FCoords);
end;

// SetCoordinate
//
procedure TGLNode.SetCoordinate(AIndex: Integer; AValue: TGLFloat);
begin
	FCoords[AIndex]:=AValue;
   (Collection as TGLNodes).NotifyChange;
end;

// StoreCoordinate
//
function TGLNode.StoreCoordinate(AIndex: Integer) : Boolean;
begin
   Result:=(FCoords[AIndex]<>0);
end;

// ------------------
// ------------------ TGLNodes ------------------
// ------------------

// Create
//
constructor TGLNodes.Create(AOwner : TPersistent; AItemClass: TCollectionItemClass = nil);
begin
   if not Assigned(AItemClass) then
      inherited Create(AOwner, TGLNode)
   else inherited Create(AOwner, AItemClass);
end;

// CreateCopy
//
function TGLNodes.CreateCopy(AOwner : TPersistent) : TGLNodes;
begin
   if Self<>nil then begin
      Result:=TGLNodesClass(Self.ClassType).Create(AOwner);
      Result.Assign(Self);
   end else Result:=nil;
end;

// SetItems
//
procedure TGLNodes.SetItems(index : Integer; const val : TGLNode);
begin
	inherited Items[index]:=val;
end;

// GetItems
//
function TGLNodes.GetItems(index : Integer) : TGLNode;
begin
	Result:=TGLNode(inherited Items[index]);
end;

//First
//
function TGLNodes.First : TGLNode;
begin
   if Count>0 then
      Result:=TGLNode(inherited Items[0])
   else Result:=nil;
end;

// Last
//
function TGLNodes.Last : TGLNode;
var
   n : Integer;
begin
   n:=Count-1;
   if n>=0 then
      Result:=TGLNode(inherited Items[n])
   else Result:=nil;
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
	Result:=(inherited Add) as TGLNode;
end;

// FindItemID
//
function TGLNodes.FindItemID(ID: Integer): TGLNode;
begin
	Result:=(inherited FindItemID(ID)) as TGLNode;
end;

// NotifyChange
//
procedure TGLNodes.NotifyChange;
begin
   if (UpdateCount=0) and (GetOwner<>nil) and (GetOwner is TGLUpdateAbleComponent) then
      TGLUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

// EndUpdate
//
procedure TGLNodes.EndUpdate;
begin
   inherited EndUpdate;
   // Workaround for a bug in VCL's EndUpdate
   if UpdateCount=0 then NotifyChange;
end;

// AddNode (TGLCustomCoordinates)
//
procedure TGLNodes.AddNode(const coords : TGLCustomCoordinates);
begin
   Add.AsVector:=coords.AsVector;
end;

// AddNode (floats)
//
procedure TGLNodes.AddNode(const x, y, z : Single);
begin
   Add.AsVector:=PointMake(x, y, z);
end;

// AddNode (TVector)
//
procedure TGLNodes.AddNode(const value : TVector);
begin
   Add.AsVector:=value;
end;

// AddNode (TAffineVector)
//
procedure TGLNodes.AddNode(const value : TAffineVector);
begin
   Add.AsAffineVector:=value;
end;

// AddXYArc
//
procedure TGLNodes.AddXYArc(xRadius, yRadius : Single;
                            startAngle, stopAngle : Single;
                            nbSegments : Integer;
                            const center : TAffineVector);
var
   i : Integer;
   f : Single;
   s, c : Single;
begin
   BeginUpdate;
   try
      startAngle:=DegToRad(startAngle);
      stopAngle :=DegToRad(stopAngle);
      f:=(stopAngle-startAngle)/nbSegments;
      for i:=0 to nbSegments do begin
         SinCos(i*f+startAngle, s, c);
         SetVector(Add.FCoords, center[0]+xRadius*c, center[1]+yRadius*s, center[2], 1);
      end;
   finally
      EndUpdate;
   end;
end;

// Barycenter
//
function TGLNodes.Barycenter : TAffineVector;
var
   i : Integer;
begin
   Result:=NullVector;
   if Count>0 then begin
      for i:=0 to Count-1 do
         AddVector(Result, PAffineVector(Items[i].AsAddress)^);
      ScaleVector(Result, 1.0/Count);
   end;
end;

// Normal
//
function TGLNodes.Normal : TAffineVector;
begin
   if Count>=3 then
      CalcPlaneNormal(Items[0].FCoords, Items[1].FCoords, Items[2].FCoords, Result)
   else Result:=NullVector;
end;

// Vector
//
function TGLNodes.Vector(i : Integer) : TAffineVector;

   procedure CalcUsingPrev; forward;

   procedure CalcUsingNext;
   begin
      if i<Count-1 then
         VectorSubtract(Items[i].AsVector, Items[i+1].AsVector, Result)
      else CalcUsingPrev;
   end;

   procedure CalcUsingPrev;
   begin
      if i>0 then
         VectorSubtract(Items[i-1].AsVector, Items[i].AsVector, Result)
      else CalcUsingNext;
   end;

begin
   Assert((i>=0) and (i<Count));
   if i=0 then
      if i=Count-1 then
         SetVector(Result, NullVector)
      else VectorSubtract(Items[i+1].AsVector, Items[i].AsVector, Result)
   else if i=Count-1 then
      VectorSubtract(Items[i].AsVector, Items[i-1].AsVector, Result)
   else VectorSubtract(Items[i+1].AsVector, Items[i-1].AsVector, Result);
   if VectorNorm(Result)<1e-5 then
      SetVector(Result, NullVector)
   else NormalizeVector(Result);
end;

// GetExtents
//
procedure TGLNodes.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   f : Single;
const
   cBigValue : Single = 1e50;
   cSmallValue : Single = -1e50;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to Count-1 do begin
      for k:=0 to 2 do begin
         f:=PAffineVector(Items[i].AsAddress)^[k];
         if f<min[k] then min[k]:=f;
         if f>max[k] then max[k]:=f;
      end;
   end;
end;

// Translate
//
procedure TGLNodes.Translate(const tv : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      AddVector(PAffineVector(Items[i].AsAddress)^, tv);
   NotifyChange;
end;

// Scale (vector)
//
procedure TGLNodes.Scale(const fv : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      ScaleVector(PAffineVector(Items[i].AsAddress)^, fv);
   NotifyChange;
end;

// Scale (single)
//
procedure TGLNodes.Scale(f : Single);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      ScaleVector(PAffineVector(Items[i].AsAddress)^, f);
   NotifyChange;
end;

// RotateAroundX
//
procedure TGLNodes.RotateAroundX(angle : Single);
var
   i : Integer;
   c, s, v2 : Single;
   v : PAffineVector;
begin
   SinCos(cPIDiv180*angle, s, c);
   for i:=0 to Count-1 do begin
      v:=PAffineVector(Items[i].AsAddress);
      v2:=v^[2];
      v^[1]:=c*v^[1]+s*v2;
      v^[2]:=c*v2-s*v^[1];
   end;
   NotifyChange;
end;

// RotateAroundY
//
procedure TGLNodes.RotateAroundY(angle : Single);
var
   i : Integer;
   c, s, v0 : Single;
   v : PAffineVector;
begin
   SinCos(cPIDiv180*angle, s, c);
   for i:=0 to Count-1 do begin
      v:=PAffineVector(Items[i].AsAddress);
      v0:=v^[0];
      v^[0]:=c*v0+s*v^[2];
      v^[2]:=c*v^[2]-s*v0;
   end;
   NotifyChange;
end;

// RotateAroundZ
//
procedure TGLNodes.RotateAroundZ(angle : Single);
var
   i : Integer;
   c, s, v1 : Single;
   v : PAffineVector;
begin
   SinCos(cPIDiv180*angle, s, c);
   for i:=0 to Count-1 do begin
      v:=PAffineVector(Items[i].AsAddress);
      v1:=v^[1];
      v^[1]:=c*v1+s*v^[0];
      v^[0]:=c*v^[0]-s*v1;
   end;
   NotifyChange;
end;

// CreateNewCubicSpline
//
function TGLNodes.CreateNewCubicSpline : TCubicSpline;
var
   i : Integer;
   xa, ya, za : PFloatArray;
begin
   GetMem(xa, SizeOf(TGLFloat)*Count);
   GetMem(ya, SizeOf(TGLFloat)*Count);
   GetMem(za, SizeOf(TGLFloat)*Count);
   for i:=0 to Count-1 do with Items[i] do begin
      xa^[i]:=X;
      ya^[i]:=Y;
      za^[i]:=Z;
   end;
   Result:=TCubicSpline.Create(xa, ya, za, nil, Count);
   FreeMem(xa);
   FreeMem(ya);
   FreeMem(za);
end;

// RenderTesselatedPolygon
//
var
   nbExtraVertices : Integer;
   newVertices : PAffineVectorArray;

  function AllocNewVertex : PAffineVector;
  begin
     Inc(nbExtraVertices);
     Result:=@newVertices[nbExtraVertices-1];
  end;

  procedure tessError(errno : TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  begin
     Assert(False, IntToStr(errno)+': '+gluErrorString(errno));
  end;

  procedure tessIssueVertex(vertexData : Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  begin
     xglTexCoord2fv(vertexData);
     glVertex3fv(vertexData);
  end;

  procedure tessCombine(coords : PDoubleVector; vertex_data : Pointer;
                        weight : PGLFloat; var outData : Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  begin
     outData:=AllocNewVertex;
     SetVector(PAffineVector(outData)^, coords^[0], coords^[1], coords^[2]);
  end;

procedure TGLNodes.RenderTesselatedPolygon(ATextured : Boolean;
                                           ANormal : PAffineVector = nil;
                                           ASplineDivisions : Integer = 1;
                                           AInvertNormals : Boolean = False);
var
   i : Integer;
   tess : PGLUTesselator;
   dblVector : TAffineDblVector;
   spline : TCubicSpline;
   splinePos : PAffineVector;
   f : Single;

begin
   if Count>2 then begin
      // Create and initialize the GLU tesselator
      tess:=gluNewTess;
      gluTessCallback(tess, GLU_TESS_BEGIN, @glBegin);
      if ATextured then
         gluTessCallback(tess, GLU_TESS_VERTEX, @tessIssueVertex)
      else gluTessCallback(tess, GLU_TESS_VERTEX, @glVertex3fv);
      gluTessCallback(tess, GLU_TESS_END, @glEnd);
      gluTessCallback(tess, GLU_TESS_ERROR, @tessError);
      gluTessCallback(tess, GLU_TESS_COMBINE, @tessCombine);
      nbExtraVertices:=0;
      // Issue normal
      if Assigned(ANormal) then begin
         glNormal3fv(PGLFloat(ANormal));
         gluTessNormal(tess, ANormal^[0], ANormal^[1], ANormal^[2]);
      end;
      // Issue polygon
      gluTessBeginPolygon(tess, nil);
      gluTessBeginContour(tess);
      if ASplineDivisions<=1 then begin
         // no spline, use direct coordinates
         GetMem(newVertices, Count*SizeOf(TAffineVector));
         if AInvertNormals then begin
            for i:=Count-1 downto 0 do begin
               SetVector(dblVector, PAffineVector(Items[i].AsAddress)^);
               gluTessVertex(tess, dblVector, Items[i].AsAddress);
            end;
         end else begin
            for i:=0 to Count-1 do begin
               SetVector(dblVector, PAffineVector(Items[i].AsAddress)^);
               gluTessVertex(tess, dblVector, Items[i].AsAddress);
            end;
         end;
      end else begin
         // cubic spline
         GetMem(newVertices, 2*ASplineDivisions*Count*SizeOf(TAffineVector));
         spline:=CreateNewCubicSpline;
         f:=1.0/ASplineDivisions;
         if AInvertNormals then begin
            for i:=ASplineDivisions*(Count-1) downto 0 do begin
               splinePos:=AllocNewVertex;
               spline.SplineAffineVector(i*f, splinePos^);
               SetVector(dblVector, splinePos^);
               gluTessVertex(tess, dblVector, splinePos);
            end;
         end else begin
            for i:=0 to ASplineDivisions*(Count-1) do begin
               splinePos:=AllocNewVertex;
               spline.SplineAffineVector(i*f, splinePos^);
               SetVector(dblVector, splinePos^);
               gluTessVertex(tess, dblVector, splinePos);
            end;
         end;
         spline.Free;
      end;
      gluTessEndContour(tess);
      gluTessEndPolygon(tess);
      // release stuff
      if Assigned(newVertices) then
         FreeMem(newVertices);
      gluDeleteTess(tess);
   end;
end;

end.
