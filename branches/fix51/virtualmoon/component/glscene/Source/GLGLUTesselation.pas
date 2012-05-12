//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLGLUTesselation<p>

    Code to generate triangle strips and fans for polygons.

 <b>History : </b><font size=-1><ul>
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>06/06/10 - Yar - Fixed warnings
      <li>26/11/09 - DaStr - Improved Lazarus compatibility (BugtrackerID = 2893580)
      <li>10/03/09 - DanB - DoTesselate now accepts TGLBaseMesh instead of
                            TGLFreeform, so can now use TGLActor with it too
      <li>29/05/08 - DaStr - Added $I GLScene.inc
      <li>08/09/03 - Jaj - Added single outline polygon support

   </ul><p>

  License:<br>

    Contributed to GLScene.<p>
}
unit GLGLUTesselation;

interface

{$I GLScene.inc}

uses
  GLVectorFileObjects,
  VectorLists,
  VectorGeometry;

{: Tesselates the polygon outlined by the Vertexes. And addeds them to the first facegroup of the Mesh. }
procedure DoTesselate(Vertexes: TAffineVectorList; Mesh: TGLBaseMesh; normal: PAffineVector = nil; invertNormals: Boolean = False);

implementation

uses
  SysUtils,
  OpenGLAdapter,
  OpenGLTokens,
  VectorTypes;

var
  TessMesh: TMeshObject;
  TessFace: TFGIndexTexCoordList;
  TessExtraVertices: Integer;
  TessVertices: PAffineVectorArray;

procedure DoTessBegin(mode: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  TessFace := TFGIndexTexCoordList.CreateOwned(TessMesh.FaceGroups);
  case mode of
    GL_TRIANGLES: TessFace.Mode := fgmmTriangles;
    GL_TRIANGLE_STRIP: TessFace.Mode := fgmmTriangleStrip;
    GL_TRIANGLE_FAN: TessFace.Mode := fgmmTriangleFan;
  end;
end;

procedure DoTessVertex3fv(v: PAffineVector);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  TessFace.Add(TessMesh.Vertices.Add(v^), 0, 0);
end;

procedure DoTessEnd;
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
end;

procedure DoTessError(errno: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Assert(False, IntToStr(errno) + ': ' + string(gluErrorString(errno)));
end;

function AllocNewVertex: PAffineVector;
begin
  Inc(TessExtraVertices);
  Result := @TessVertices[TessExtraVertices - 1];
end;

procedure DoTessCombine(coords: PDoubleVector; vertex_data: Pointer; weight: PGLFloat; var outData: Pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  outData := AllocNewVertex;
  SetVector(PAffineVector(outData)^, coords[0], coords[1], coords[2]);
end;

procedure DoTesselate(Vertexes: TAffineVectorList; Mesh: TGLBaseMesh; normal: PAffineVector = nil; invertNormals: Boolean = False);
var
  Tess: PGLUTesselator;
  i: Integer;
  dblVector: TAffineDblVector;
begin
  // Select or Create FaceGroup
  if Mesh.MeshObjects.Count = 0 then
  begin
    TessMesh := TMeshObject.CreateOwned(Mesh.MeshObjects);
    Mesh.MeshObjects[0].Mode := momFaceGroups;
  end
  else
    TessMesh := Mesh.MeshObjects[0];

  // allocate extra buffer used by GLU in complex polygons.
  GetMem(TessVertices, Vertexes.Count * SizeOf(TAffineVector));

  // make a Tessellation GLU object.
  Tess := gluNewTess;

  // set up callback events
  gluTessCallback(Tess, GLU_TESS_BEGIN, @DoTessBegin);
  gluTessCallback(tess, GLU_TESS_VERTEX, @DoTessVertex3fv);
  gluTessCallback(tess, GLU_TESS_END, @DoTessEnd);
  gluTessCallback(tess, GLU_TESS_ERROR, @DoTessError);
  gluTessCallback(tess, GLU_TESS_COMBINE, @DoTessCombine);

  if Assigned(normal) then
    gluTessNormal(tess, normal^[0], normal^[1], normal^[2])
  else
    gluTessNormal(tess, 0, 1, 0);

  // start tesselation of polygon
  gluTessBeginPolygon(tess, nil);

  // build outline, a polygon can have multiple outlines.
  gluTessBeginContour(tess);
  TessExtraVertices := 0;
  if invertNormals then
  begin
    for i := Vertexes.Count - 1 downto 0 do
    begin
      SetVector(dblVector, Vertexes.Items[i]);
      gluTessVertex(tess, dblVector, Vertexes.ItemAddress[i]);
    end;
  end
  else
  begin
    for i := 0 to Vertexes.Count - 1 do
    begin
      SetVector(dblVector, Vertexes.Items[i]);
      gluTessVertex(tess, dblVector, Vertexes.ItemAddress[i]);
    end;
  end;
  gluTessEndContour(tess);

  // End Tesselation of polygon, THIS is where the data is processed! (And all the events triggered!)
  gluTessEndPolygon(tess);

  // Destroy the Tessellation GLU object.
  gluDeleteTess(tess);

  // deallocate extra buffer used by GLU in complex polygons.
  FreeMem(TessVertices, Vertexes.Count * SizeOf(TAffineVector));
end;

end.

