// GLPolyhedron
{: Standard polyhedrons.<p>

 <b>History : </b><font size=-1><ul>
      <li>10/03/13 - PW - Added TGLTetrahedron and TGLOctahedron classes
      <li>23/08/10 - Yar - Added OpenGLTokens to uses
      <li>20/01/04 - SG - Added TGLIcosahedron
      <li>21/07/03 - EG - Creation from GLObjects split
   </ul></font>
}
unit GLPolyhedron;

interface

uses
  Classes,
  GLScene,
  VectorGeometry,
  GLRenderContextInfo;

type

  // TGLDodecahedron
  //
  {: A Dodecahedron.<p>
     The dodecahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TGLDodecahedron = class(TGLSceneObject)
  public
    { Public Declarations }
    procedure BuildList(var rci: TRenderContextInfo); override;
  end;

  // TGLIcosahedron
  //
  {: A Icosahedron.<p>
     The icosahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TGLIcosahedron = class(TGLSceneObject)
  public
    { Public Declarations }
    procedure BuildList(var rci: TRenderContextInfo); override;
  end;

  // TGLOctahedron
  //
  {: A Octahedron.<p>
     The octahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TGLOctahedron = class(TGLSceneObject)
  public
    { Public Declarations }
    procedure BuildList(var rci: TRenderContextInfo); override;
  end;

  // TGLTetrahedron
  //
  {: A Tetrahedron.<p>
     The tetrahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TGLTetrahedron = class(TGLSceneObject)
  public
    { Public Declarations }
    procedure BuildList(var rci: TRenderContextInfo); override;
  end;


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses GLObjects;

// ------------------
// ------------------ TGLDodecahedron ------------------
// ------------------

// BuildList
//

procedure TGLDodecahedron.BuildList(var rci: TRenderContextInfo);
begin
  DodecahedronBuildList;
end;

// ------------------
// ------------------ TGLIcosahedron ------------------
// ------------------

// BuildList
//

procedure TGLIcosahedron.BuildList(var rci: TRenderContextInfo);
begin
  IcosahedronBuildList;
end;

//--------------------
//--------------------  TGLOctahedron ------------------------
//--------------------

// BuildList
//
procedure TGLOctahedron.BuildList(var rci: TRenderContextInfo);
begin
  OctahedronBuildList;
end;

//--------------------
//--------------------  TGLTetrahedron ------------------------
//--------------------

// BuildList
//
procedure TGLTetrahedron.BuildList(var rci: TRenderContextInfo);
begin
  TetrahedronBuildList;
end;

initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TGLDodecahedron, TGLIcosahedron, TGLOctahedron, TGLTetrahedron]);

end.

