//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   History :  

   10/11/12 - PW - Added CPP compatibility: changed vector arrays to records
   10/08/10 - Yar - Replaced OpenGL1x to OpenGLTokens
   05/04/10 - Yar - Added GLSceneMatrixToODER (thanks Vovik)
   22/11/09 - DaStr - Improved Unix compatibility (thanks Predator)
                 (BugtrackerID = 2893580)
   08/12/08 - PR dBodySetMass no longer accepts zero mass. check added
   06/02/08 - Mrqzzz - Upgrade to ODE 0.9 (upgrade by Paul Robello)
   11/09/07 - Mrqzzz - added reference to odeimport
   02/08/04 - LR, YHC - BCB corrections: use record instead array
   03/02/03 - EG - Fixed CopyPosFromGeomToGL
   11/02/03 - MF - Added a couple of vector functions for copying between
    ODE formats and GLScene formats
}

unit ODEGL;

interface

{
  Here I collect random functions and procedures I've found useful when
  integrating ODE into GLScene. If you don't use GLScene, this unit won't be
  very useful to you. The unit is not intended as a sorted toolbox, but more
  as a place to put stuff until we figure out how to organize the integration.

  Mattias Fagerlund ( mattias@cambrianlabs.com ), 2002-09-26

}

uses
  SysUtils,
  OpenGLTokens,
  GLContext,
  GLVectorGeometry,
  ODEImport,
  GLScene,
  GLVectorTypes,
  GLVectorLists,
  GLObjects,
  GLVerletClothify,
  GLVectorFileObjects;

procedure DrawBox(Sides: TdVector3);
procedure setTransform(pos: TdVector3; R: TdMatrix3);
procedure dsDrawBox(pos: PdVector3; R: PdMatrix3; Sides: TdVector3); overload;
procedure dsDrawBox(pos: TdVector3; R: TdMatrix3; Sides: TdVector3); overload;

procedure ODERToGLSceneMatrix(var m: TMatrix; R: TdMatrix3; pos: TdVector3);
overload;
procedure ODERToGLSceneMatrix(var m: TMatrix; R: PdMatrix3; pos: PdVector3);
overload;
procedure ODERToGLSceneMatrix(var m: TMatrix; R: TdMatrix3_As3x4; pos:  TdVector3); overload;
function GLSceneMatrixToODER(m: TMatrix): TdMatrix3;

// Converting between ODE and GLScene formats
function ConvertdVector3ToVector3f(R: TdVector3): TVector3f; overload;
function ConvertdVector3ToVector3f(R: PdVector3): TVector3f; overload;
function ConvertdVector3ToVector4f(R: TdVector3): TVector4f; overload;
function ConvertdVector3ToVector4f(R: PdVector3): TVector4f; overload;

function ConvertdVector3ToAffineVector(R: PdVector3): TAffineVector; overload;
function ConvertdVector3ToAffineVector(R: TdVector3): TAffineVector; overload;

function GetBodyPositionAsAffineVector(Body: PdxBody): TAffineVector;

// Converting between GLScene and ODE formats
function ConvertVector3fTodVector3(R: TVector3f): TdVector3;
function ConvertVector3fToPdVector3(R: TVector3f): PdVector3;
function ConvertVector4fTodVector3(R: TVector4f): TdVector3;
function ConvertVector4fToPdVector3(R: TVector4f): PdVector3;

function dVector3Length(R: TdVector3): single; overload;
function dVector3Length(R: PdVector3): single; overload;

function dBodyToBodyDistance(Body1, Body2: PdxBody): TdReal;

procedure CopyPosFromGeomToGL(Geom: PdxGeom; GLBaseSceneObject:  TGLBaseSceneObject);
procedure PositionSceneObject(GLBaseSceneObject: TGLBaseSceneObject; Geom: PdxGeom);
procedure PositionSceneObjectForGeom(Geom: PdxGeom);

procedure CopyCubeSizeFromBox(Cube: TGLCube; Geom: PdxGeom);
procedure CopyBodyFromCube(Body: PdxBody; var Geom: PdxGeom; Cube: TGLCube;  Space: PdxSpace);

function CreateGeomFromCube(Cube: TGLCube; Space: PdxSpace): PdxGeom;
function CreateBodyFromCube(var Geom: PdxGeom; Cube: TGLCube; World: PdxWorld;  Space: PdxSpace): PdxBody;

{ Note that this method requires you to manually deallocate vertices and
  indices when you're done with the trimesh }
function CreateTriMeshFromBaseMesh(
  GLBaseMesh: TGLBaseMesh;
  Space: PdxSpace;
  var Vertices: PdVector3Array;
  var Indices: PdIntegerArray): PdxGeom;

function GLMatrixFromGeom(Geom: PdxGeom): TMatrix;
function GLDirectionFromGeom(Geom: PdxGeom): TVector;

function CreateODEPlaneFromGLPlane(Plane: TGLPlane; Space: PdxSpace): PdxGeom;

procedure RenderGeomList(GeomList: TGeomList);

function RandomColorVector: TVector;

// { $ EXTERNALSYM GL_ZERO} ?

implementation

procedure ODERToGLSceneMatrix(var m: TMatrix; R: TdMatrix3_As3x4; pos: TdVector3); overload;
begin
  m.V[0].V[0] := r[0][0];
  m.V[0].V[1] := r[0][1];
  m.V[0].V[2] := r[0][2];
  m.V[0].V[3] := 0;
  m.V[1].V[0] := r[1][0];
  m.V[1].V[1] := r[1][1];
  m.V[1].V[2] := r[1][2];
  m.V[1].V[3] := 0;
  m.V[2].V[0] := r[2][0];
  m.V[2].V[1] := r[2][1];
  m.V[2].V[2] := r[2][2];
  m.V[2].V[3] := 0;
  m.V[3] := NullHmgPoint;

  TransposeMatrix(m);
  m.V[3].V[0] := pos[0];
  m.V[3].V[1] := pos[1];
  m.V[3].V[2] := pos[2];
  m.V[3].V[3] := 1; //}
end;

procedure ODERToGLSceneMatrix(var m: TMatrix; R: PdMatrix3; pos: PdVector3);
begin
  ODERToGLSceneMatrix(m, TdMatrix3_As3x4(R^), pos^);
end;

procedure ODERToGLSceneMatrix(var m: TMatrix; R: TdMatrix3; pos: TdVector3);
begin
  ODERToGLSceneMatrix(m, TdMatrix3_As3x4(R), pos);
end;

procedure DrawBox(Sides: TdVector3);
var
  lx, ly, lz: single;
begin
  lx := Sides[0] * 0.5;
  ly := Sides[1] * 0.5;
  lz := Sides[2] * 0.5;

  // sides
  GL.Begin_(GL_TRIANGLE_STRIP);
  GL.Normal3f(-1, 0, 0);
  GL.Vertex3f(-lx, -ly, -lz);
  GL.Vertex3f(-lx, -ly, lz);
  GL.Vertex3f(-lx, ly, -lz);
  GL.Vertex3f(-lx, ly, lz);
  GL.Normal3f(0, 1, 0);
  GL.Vertex3f(lx, ly, -lz);
  GL.Vertex3f(lx, ly, lz);
  GL.Normal3f(1, 0, 0);
  GL.Vertex3f(lx, -ly, -lz);
  GL.Vertex3f(lx, -ly, lz);
  GL.Normal3f(0, -1, 0);
  GL.Vertex3f(-lx, -ly, -lz);
  GL.Vertex3f(-lx, -ly, lz);
  GL.End_();

  // top face
  GL.Begin_(GL_TRIANGLE_FAN);
  GL.Normal3f(0, 0, 1);
  GL.Vertex3f(-lx, -ly, lz);
  GL.Vertex3f(lx, -ly, lz);
  GL.Vertex3f(lx, ly, lz);
  GL.Vertex3f(-lx, ly, lz);
  GL.End_();

  // bottom face
  GL.Begin_(GL_TRIANGLE_FAN);
  GL.Normal3f(0, 0, -1);
  GL.Vertex3f(-lx, -ly, -lz);
  GL.Vertex3f(-lx, ly, -lz);
  GL.Vertex3f(lx, ly, -lz);
  GL.Vertex3f(lx, -ly, -lz);
  GL.End_();
end;

function GLSceneMatrixToODER(m: TMatrix): TdMatrix3;
begin
  TransposeMatrix(m);
  Result[0] := m.V[0].V[0];
  Result[1] := m.V[0].V[1];
  Result[2] := m.V[0].V[2];
  Result[4] := m.V[1].V[0];
  Result[5] := m.V[1].V[1];
  Result[6] := m.V[1].V[2];
  Result[8] := m.V[2].V[0];
  Result[9] := m.V[2].V[1];
  Result[10] := m.V[2].V[2];
end;

procedure dsDrawBox(pos: PdVector3; R: PdMatrix3; Sides: TdVector3);
begin
  dsDrawBox(pos^, r^, Sides);
end;

procedure dsDrawBox(pos: TdVector3; R: TdMatrix3; Sides: TdVector3);
begin
  setTransform(pos, R);
  drawBox(sides);
  GL.PopMatrix();
end;

procedure setTransform(pos: TdVector3; R: TdMatrix3);
var
  matrix: array[0..15] of GLfloat;
begin
  matrix[0] := R[0];
  matrix[1] := R[4];
  matrix[2] := R[8];
  matrix[3] := 0;
  matrix[4] := R[1];
  matrix[5] := R[5];
  matrix[6] := R[9];
  matrix[7] := 0;
  matrix[8] := R[2];
  matrix[9] := R[6];
  matrix[10] := R[10];
  matrix[11] := 0;
  matrix[12] := pos[0];
  matrix[13] := pos[1];
  matrix[14] := pos[2];
  matrix[15] := 1;
  GL.PushMatrix();
  GL.MultMatrixf(@matrix);
end;

(*$WARNINGS OFF*)

function ConvertdVector3ToVector3f(R: TdVector3): TVector3f;
begin
  result.V[0] := R[0];
  result.V[1] := R[1];
  result.V[2] := R[2];
end;

function ConvertdVector3ToVector3f(R: PdVector3): TVector3f;
begin
  result.V[0] := R[0];
  result.V[1] := R[1];
  result.V[2] := R[2];
end;

function ConvertdVector3ToVector4f(R: TdVector3): TVector4f; overload;
begin
  result.V[0] := R[0];
  result.V[1] := R[1];
  result.V[2] := R[2];
  result.V[3] := 0;
end;

function ConvertdVector3ToVector4f(R: PdVector3): TVector4f; overload;
begin
  result.V[0] := R[0];
  result.V[1] := R[1];
  result.V[2] := R[2];
  result.V[3] := 0;
end;

function ConvertdVector3ToAffineVector(R: PdVector3): TAffineVector; overload;
begin
  result.V[0] := R[0];
  result.V[1] := R[1];
  result.V[2] := R[2];
end;

function ConvertdVector3ToAffineVector(R: TdVector3): TAffineVector; overload;
begin
  result.V[0] := R[0];
  result.V[1] := R[1];
  result.V[2] := R[2];
end;

function ConvertVector3fTodVector3(R: TVector3f): TdVector3;
begin
  result[0] := R.V[0];
  result[1] := R.V[1];
  result[2] := R.V[2];
end;

function ConvertVector3fToPdVector3(R: TVector3f): PdVector3;
begin
  result[0] := R.V[0];
  result[1] := R.V[1];
  result[2] := R.V[2];
end;

function ConvertVector4fTodVector3(R: TVector4f): TdVector3;
begin
  result[0] := R.V[0];
  result[1] := R.V[1];
  result[2] := R.V[2];
  result[3] := 0;
end;

function ConvertVector4fToPdVector3(R: TVector4f): PdVector3;
begin
  result[0] := R.V[0];
  result[1] := R.V[1];
  result[2] := R.V[2];
  result[3] := 0;
end;

(*$WARNINGS ON*)

function GetBodyPositionAsAffineVector(Body: PdxBody): TAffineVector;
begin
  result := ConvertdVector3ToVector3f(dBodyGetPosition(Body));
end;

procedure PositionSceneObjectForGeom(Geom: PdxGeom);
begin
  if Assigned(Geom.Data) then
    PositionSceneObject(TGLBaseSceneObject(Geom.Data), Geom);
end;

function GLMatrixFromGeom(Geom: PdxGeom): TMatrix;
var
  pos, Pos2: PdVector3;
  R, R2: PdMatrix3;

  actual_pos: TdVector3;
  actual_R: TdMatrix3;

  TransformedGeom: PdxGeom;
  GeomClass: integer;
begin
  // Retrieve the position and rotation of the geom
  pos := dGeomGetPosition(geom);
  R := dGeomGetRotation(geom);

  // if the geom is a transform geom, it should be treated differently
  GeomClass := dGeomGetClass(Geom);

  if GeomClass = dGeomTransformClass then
  begin
    TransformedGeom := dGeomTransformGetGeom(Geom);

    // No transformed geom!?
    if TransformedGeom = nil then
      exit;

    // Retrieve the position and rotation of the transformed geom
    pos2 := dGeomGetPosition(TransformedGeom);
    R2 := dGeomGetRotation(TransformedGeom);

    dMULTIPLY0_331(actual_pos, R^, pos2^);
    actual_pos := Vector3ADD(actual_pos, pos^);
    dMULTIPLY0_333(actual_R, R^, R2^);

    ODERToGLSceneMatrix(result, actual_R, actual_pos);
  end
  else
  begin
    ODERToGLSceneMatrix(result, R, pos);
  end;
end;

function GLDirectionFromGeom(Geom: PdxGeom): TVector;
var
  m: TMatrix;
begin
  m := GLMatrixFromGeom(Geom);

  result := VectorNormalize(m.V[2]);
end;

procedure PositionSceneObject(GLBaseSceneObject: TGLBaseSceneObject; Geom: PdxGeom);
var
  Scale: TAffineVector;
begin
  Scale := GLBaseSceneObject.Scale.AsAffineVector;
  GLBaseSceneObject.Matrix := GLMatrixFromGeom(Geom);
  GLBaseSceneObject.Scale.AsAffineVector := Scale;
end;

procedure CopyCubeSizeFromBox(Cube: TGLCube; Geom: PdxGeom);
var
  Sides: TdVector3;
begin
  dGeomBoxGetLengths(Geom, Sides);

  Cube.CubeWidth := Sides[0]; // 0
  Cube.CubeHeight := Sides[1]; // 1
  Cube.CubeDepth := Sides[2]; // 2
end;

procedure CopyPosFromGeomToGL(Geom: PdxGeom; GLBaseSceneObject: TGLBaseSceneObject);
var
  v: TVector;
  m: TMatrix;

  R: PdMatrix3;
  pos: PdVector3;
begin
  v := GLBaseSceneObject.AbsolutePosition;

  dGeomSetPosition(Geom, v.V[0], v.V[1], v.V[2]);

  R := dGeomGetRotation(Geom);
  pos := dgeomGetPosition(Geom);

  m := GLBaseSceneObject.AbsoluteMatrix;
  R[0] := m.V[0].V[0];
  R[4] := m.V[0].V[1];
  R[8] := m.V[0].V[2];
  R[1] := m.V[1].V[0];
  R[5] := m.V[1].V[1];
  R[9] := m.V[1].V[2];
  R[2] := m.V[2].V[0];
  R[6] := m.V[2].V[1];
  R[10] := m.V[2].V[2];
  pos[0] := m.V[3].V[0];
  pos[1] := m.V[3].V[1];
  pos[2] := m.V[3].V[2]; //}

  dGeomSetRotation(Geom, R^);
end;

function CreateGeomFromCube(Cube: TGLCube; Space: PdxSpace): PdxGeom;
var
  Geom: PdxGeom;
begin
  Geom := dCreateBox(Space, Cube.CubeWidth, Cube.CubeHeight, Cube.CubeDepth);
  CopyPosFromGeomToGL(Geom, Cube);

  result := Geom;
end;

function CreateBodyFromCube(var Geom: PdxGeom; Cube: TGLCube; World: PdxWorld; Space: PdxSpace): PdxBody;
var
  Body: PdxBody;
begin
  Body := dBodyCreate(World);

  try
    dBodySetLinearVel(Body, 0, 0, 0);

    CopyBodyFromCube(Body, Geom, Cube, Space);
  finally
    result := Body;
  end;
end;

function CreateTriMeshFromBaseMesh(
  GLBaseMesh: TGLBaseMesh;
  Space: PdxSpace;
  var Vertices: PdVector3Array;
  var Indices: PdIntegerArray): PdxGeom;
var
  i, j, p: integer;
  FaceExtractor: TFaceExtractor;
  VertexCount: integer;
  Vertex: TAffineVector;
  OffsetList: TIntegerList;
  Face: TFace;
  iMO: integer;
  TriMeshData: PdxTriMeshData;
begin
  OffsetList := nil;
  FaceExtractor := TFaceExtractor.Create(GLBaseMesh);

  try
    OffsetList := TIntegerList.Create;

    FaceExtractor.ProcessMesh;

    VertexCount := 0;
    for i := 0 to GLBaseMesh.MeshObjects.Count - 1 do
      VertexCount := VertexCount + GLBaseMesh.MeshObjects[i].Vertices.Count;

    Vertices := AllocMem(sizeOf(TdVector3) * VertexCount);
    Indices := AllocMem(sizeOf(integer) * FaceExtractor.FaceList.Count * 3);

    // Copy all vertices
    p := 0;
    for i := 0 to GLBaseMesh.MeshObjects.Count - 1 do
    begin
      OffsetList.Add(p);
      for j := 0 to GLBaseMesh.MeshObjects[i].Vertices.Count - 1 do
      begin
        Vertex :=
          GLBaseMesh.LocalToAbsolute(GLBaseMesh.MeshObjects[i].Vertices[j]);
        Vertices^[p, 0] := Vertex.V[0];
        Vertices^[p, 1] := Vertex.V[1];
        Vertices^[p, 2] := Vertex.V[2];
        Vertices^[p, 3] := 0;
        inc(p);
      end;
    end;

    // Copy all triangles
    p := 0;
    for i := 0 to FaceExtractor.FaceList.Count - 1 do
    begin
      Face := FaceExtractor.FaceList[i];
      iMO := GLBaseMesh.MeshObjects.IndexOf(Face.MeshObject);

      Indices^[p] := Face.Vertices[0] + OffsetList[iMO];
      inc(p);
      Indices^[p] := Face.Vertices[1] + OffsetList[iMO];
      inc(p);
      Indices^[p] := Face.Vertices[2] + OffsetList[iMO];
      inc(p);
    end;

    TriMeshData := dGeomTriMeshDataCreate;

    dGeomTriMeshDataBuildSimple(
      TriMeshData,
      Vertices, VertexCount,
      Indices, FaceExtractor.FaceList.Count * 3);

    result := dCreateTriMesh(space, TriMeshData, nil, nil, nil);
  finally
    FaceExtractor.Free;

    if OffsetList <> nil then
      OffsetList.Free;
  end;
end;

procedure CopyBodyFromCube(Body: PdxBody; var Geom: PdxGeom; Cube: TGLCube;
  Space: PdxSpace);
var
  m: TdMass;
begin
  // Stup the body
  dMassSetBox(m, 1, Cube.CubeWidth, Cube.CubeHeight, Cube.CubeDepth);
  if m.mass>0 then dBodySetMass(Body, @m);

  // Setup the geom
  Geom := CreateGeomFromCube(Cube, Space);
  dGeomSetBody(Geom, Body);

  CopyPosFromGeomToGL(Geom, Cube);

  Geom.data := Cube;
end;

function dBodyToBodyDistance(Body1, Body2: PdxBody): TdReal;
begin
  result := dVector3Length(Vector3SUB(Body1.posr.pos, Body2.posr.pos));
end;

function dVector3Length(R: TdVector3): single;
begin
  result := Sqrt(sqr(R[0]) + sqr(R[1]) + sqr(R[2]));
end;

function dVector3Length(R: PdVector3): single;
begin
  result := Sqrt(sqr(R[0]) + sqr(R[1]) + sqr(R[2]));
end;

procedure RenderGeomList(GeomList: TGeomList);
var
  i: integer;
begin
  for i := 0 to GeomList.Count - 1 do
    if Assigned(GeomList[i].data) then
      PositionSceneObject(TGLBaseSceneObject(GeomList[i].data), GeomList[i]);
end;

function CreateODEPlaneFromGLPlane(Plane: TGLPlane; Space: PdxSpace): PdxGeom;
var
  Pos, Direction: TVector;
  d: single;
begin
  Direction := Plane.AbsoluteDirection;
  Pos := Plane.AbsolutePosition;

  d := (Direction.V[0] * Pos.V[0] +
        Direction.V[1] * Pos.V[1] +
        Direction.V[2] * Pos.V[2]);

  result := dCreatePlane(space, Direction.V[0], Direction.V[1], Direction.V[2], d);
end;

function RandomColorVector: TVector;
begin
  result := VectorMake(Random, Random, Random, 1);
end;
end.

