//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLParametricSurfaces<p>

   Parametric surface implementation (like Bezier and BSpline surfaces)
   for GLScene.<p>

   Notes:
   The MOParametricSurface is a TMeshObject descendant that can be used
   to render parametric surfaces. The Renderer property defines if the
   surface should be rendered using OpenGL mesh evaluators (through GLU
   Nurbs for BSplines) or through GLScene using the CurvesAndSurfaces.pas
   routines to generate the mesh vertices and then rendered through the
   standard TMeshObject render routine. Please note that BSplines aren't
   correctly handled yet in the CurvesAndSurfaces unit so the output mesh
   in GLScene rendering mode is wrong. I'll have it fixed when I know
   what's going wrong. The GLU Nurbs and glMeshEval Beziers work well
   though.<p>

   The FGBezierSurface is a face group decendant that renders the surface
   using mesh evaluators. The ControlPointIndices point to the mesh object
   vertices much the same as vertex indices for other face group flavours.
   The MinU, MaxU, MinV and MaxV properties allow for drawing specific
   parts of the bezier surface, which can be used to blend a patch with
   other patches.<p>

   <b>History : </b><font size=-1><ul>
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>31/03/07 - DaStr - Added $I GLScene.inc
      <li>11/05/04 - SG - Mesh building and texture coord fixes.
      <li>05/02/04 - SG - Added FGBezierSurface facegroup descendant.
      <li>20/08/03 - SG - Weighted control points.
      <li>18/07/03 - SG - Creation.
   </ul></font>
}
unit GLParametricSurfaces;

interface

{$I GLScene.inc}

uses
  GLVectorFileObjects,
  CurvesAndSurfaces,
  VectorGeometry,
  VectorLists,
  PersistentClasses,
  GLTexture,
  OpenGLTokens,
  OpenGLAdapter,
  GLState,
  GLRenderContextInfo;

type

  {: psrGLScene tells the surface to render using GLScene code to build
     the mesh, whereas, psrOpenGL uses glEvalMesh2 or gluNurbsRenderer
     calls to render the surface. }
  TParametricSurfaceRenderer = (psrGLScene, psrOpenGL);

  {: psbBezier indicates building the surface with Bernstein basis
     functions, no knot or order properties are used.
     psbBSpline indicates building the surface using BSpline basis
     functions, these require orders and knot vectors to define the
     control point influences on the surface. }
  TParametricSurfaceBasis = (psbBezier, psbBSpline);

  TMOParametricSurface = class(TMeshObject)
  private
    FControlPoints,
      FWeightedControlPoints: TAffineVectorList;
    FKnotsU,
      FKnotsV,
      FWeights: TSingleList;
    FOrderU,
      FOrderV,
      FCountU,
      FCountV,
      FResolution: Integer;
    FAutoKnots: Boolean;
    FContinuity: TBSplineContinuity;
    FRenderer: TParametricSurfaceRenderer;
    FBasis: TParametricSurfaceBasis;

    procedure SetControlPoints(Value: TAffineVectorList);
    procedure SetKnotsU(Value: TSingleList);
    procedure SetKnotsV(Value: TSingleList);
    procedure SetWeights(Value: TSingleList);
    procedure SetRenderer(Value: TParametricSurfaceRenderer);
    procedure SetBasis(Value: TParametricSurfaceBasis);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    procedure BuildList(var mrci: TRenderContextInfo); override;
    procedure Prepare; override;
    procedure Clear; override;
    {: Generates a mesh approximation of the surface defined by the
       properties below. This is used to construct the mesh when using
       Renderer = psrGLScene. If you want to render using OpenGL calls
       but would like to obtain the mesh data also use this call to
       generate the mesh data. Fills in Vertices, Normals, etc. }
    procedure GenerateMesh;

    //: Control points define the parametric surface.
    property ControlPoints: TAffineVectorList read FControlPoints write SetControlPoints;
    {: KnotsU and KnotsV are the knot vectors in the U and V direction. Knots
       define the continuity of curves and how control points influence the
       parametric values to build the surface. }
    property KnotsU: TSingleList read FKnotsU write SetKnotsU;
    property KnotsV: TSingleList read FKnotsV write SetKnotsV;
    {: Weights define how much a control point effects the surface. }
    property Weights: TSingleList read FWeights write SetWeights;
    //: OrderU and OrderV defines the curve order in the U and V direction
    property OrderU: Integer read FOrderU write FOrderU;
    property OrderV: Integer read FOrderV write FOrderV;
    {: CountU and CountV describe the number of control points in the
       U and V direciton. Basically a control point width and height
       in (u,v) space. }
    property CountU: Integer read FCountU write FCountU;
    property CountV: Integer read FCountV write FCountV;
    {: Defines how fine the resultant mesh will be. Higher values create
       finer meshes. Resolution = 50 would produce a 50x50 mesh.
       The GLU Nurbs rendering uses resolution as the U_STEP and V_STEP
       using the sampling method GLU_DOMAIN_DISTANCE, so the resolution
       works a little differently there. }
    property Resolution: Integer read FResolution write FResolution;
    {: Automatically generate the knot vectors based on the Continuity.
       Only applies to BSpline surfaces. }
    property AutoKnots: Boolean read FAutoKnots write FAutoKnots;
    property Continuity: TBSplineContinuity read FContinuity write FContinuity;
    {: Determines whether to use OpenGL calls (psrOpenGL) or the GLScene
       mesh objects (psrGLScene) to render the surface. }
    property Renderer: TParametricSurfaceRenderer read FRenderer write SetRenderer;
    //: Basis determines the style of curve, psbBezier or psbBSpline
    property Basis: TParametricSurfaceBasis read FBasis write SetBasis;
  end;

  // TFGBezierSurface
  //
  {: A 3d bezier surface implemented through facegroups. The ControlPointIndices
     is an index to control points stored in the MeshObject.Vertices affine
     vector list. Similarly the TexCoordIndices point to the owner
     MeshObject.TexCoords, one for each control point.
     CountU and CountV define the width and height of the surface.
     Resolution sets the detail level of the mesh evaluation.
     MinU, MaxU, MinV and MaxV define the region of the surface to be rendered,
     this is especially useful for blending with neighbouring patches. }
  TFGBezierSurface = class(TFaceGroup)
  private
    FCountU, FCountV: Integer;
    FControlPointIndices,
      FTexCoordIndices: TIntegerList;
    FResolution: Integer;
    FMinU, FMaxU,
      FMinV, FMaxV: Single;
    FTempControlPoints,
      FTempTexCoords: TAffineVectorList;

  protected
    procedure SetControlPointIndices(const Value: TIntegerList);
    procedure SetTexCoordIndices(const Value: TIntegerList);

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    procedure BuildList(var mrci: TRenderContextInfo); override;
    procedure Prepare; override;

    property CountU: Integer read FCountU write FCountU;
    property CountV: Integer read FCountV write FCountV;
    property Resolution: Integer read FResolution write FResolution;
    property MinU: Single read FMinU write FMinU;
    property MaxU: Single read FMaxU write FMaxU;
    property MinV: Single read FMinV write FMinV;
    property MaxV: Single read FMaxV write FMaxV;
    property ControlPointIndices: TIntegerList read FControlPointIndices write SetControlPointIndices;
    property TexCoordIndices: TIntegerList read FTexCoordIndices write SetTexCoordIndices;

  end;

  // ----------------------------------------------------------------------
  // ----------------------------------------------------------------------
  // ----------------------------------------------------------------------
implementation
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
uses
  GLContext;
// ------------------
// ------------------ TMOParametricSurface ------------------
// ------------------

// Create
//

constructor TMOParametricSurface.Create;
begin
  inherited;

  FControlPoints := TAffineVectorList.Create;
  FWeightedControlPoints := TAffineVectorList.Create;
  FKnotsU := TSingleList.Create;
  FKnotsV := TSingleList.Create;
  FWeights := TSingleList.Create;

  Resolution := 20;
end;

// Destroy
//

destructor TMOParametricSurface.Destroy;
begin
  FControlPoints.Free;
  FWeightedControlPoints.Free;
  FKnotsU.Free;
  FKnotsV.Free;
  FWeights.Free;
  inherited;
end;

// WriteToFiler
//

procedure TMOParametricSurface.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version
    FControlPoints.WriteToFiler(writer);
    FKnotsU.WriteToFiler(writer);
    FKnotsV.WriteToFiler(writer);
    FWeights.WriteToFiler(writer);
    WriteInteger(FOrderU);
    WriteInteger(FOrderV);
    WriteInteger(FCountU);
    WriteInteger(FCountV);
    WriteInteger(FResolution);
    WriteBoolean(FAutoKnots);
    WriteInteger(Integer(FContinuity));
    WriteInteger(Integer(FRenderer));
    WriteInteger(Integer(FBasis));
  end;
end;

// ReadFromFiler
//

procedure TMOParametricSurface.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FControlPoints.ReadFromFiler(reader);
      FKnotsU.ReadFromFiler(reader);
      FKnotsV.ReadFromFiler(reader);
      FWeights.ReadFromFiler(reader);
      FOrderU := ReadInteger;
      FOrderV := ReadInteger;
      FCountU := ReadInteger;
      FCountV := ReadInteger;
      FResolution := ReadInteger;
      FAutoKnots := ReadBoolean;
      FContinuity := TBSplineContinuity(ReadInteger);
      FRenderer := TParametricSurfaceRenderer(ReadInteger);
      FBasis := TParametricSurfaceBasis(ReadInteger);
    end
  else
    RaiseFilerException(archiveVersion);
end;

// BuildList
//

procedure TMOParametricSurface.BuildList(var mrci: TRenderContextInfo);
var
  NurbsRenderer: PGLUNurbs;
begin
  case FRenderer of
    psrGLScene: inherited;
    psrOpenGL:
      begin
        mrci.GLStates.PushAttrib([sttEnable, sttEval]);
        //GL.Enable(GL_MAP2_TEXTURE_COORD_3);
        GL.Enable(GL_MAP2_VERTEX_3);
        GL.Enable(GL_AUTO_NORMAL);
        GL.Enable(GL_NORMALIZE);

        case FBasis of
          psbBezier:
            begin
              GL.MapGrid2f(FResolution, 1, 0, FResolution, 0, 1);
              GL.Map2f(GL_MAP2_TEXTURE_COORD_3,
                0, 1, 3, FOrderU,
                0, 1, 3 * FCountU, FOrderV,
                @FWeightedControlPoints.List[0]);
              GL.Map2f(GL_MAP2_VERTEX_3, 0, 1, 3, FCountU, 0, 1, 3 * FCountU, FCountV, @FWeightedControlPoints.List[0]);
              GL.EvalMesh2(GL_FILL, 0, FResolution, 0, FResolution);
            end;

          psbBSpline:
            begin
              NurbsRenderer := gluNewNurbsRenderer;
              gluNurbsProperty(NurbsRenderer, GLU_DISPLAY_MODE, GLU_FILL);

              gluNurbsProperty(NurbsRenderer, GLU_SAMPLING_METHOD, GLU_DOMAIN_DISTANCE);
              gluNurbsProperty(NurbsRenderer, GLU_U_STEP, FResolution);
              gluNurbsProperty(NurbsRenderer, GLU_V_STEP, FResolution);

              gluBeginSurface(NurbsRenderer);
              gluNurbsSurface(NurbsRenderer,
                FKnotsU.Count, @FKnotsU.List[0],
                FKnotsV.Count, @FKnotsV.List[0],
                3, FCountU * 3,
                @FWeightedControlPoints.List[0],
                FOrderU, FOrderV,
                GL_MAP2_TEXTURE_COORD_3);
              gluNurbsSurface(NurbsRenderer,
                FKnotsU.Count, @FKnotsU.List[0],
                FKnotsV.Count, @FKnotsV.List[0],
                3, FCountU * 3,
                @FWeightedControlPoints.List[0],
                FOrderU, FOrderV,
                GL_MAP2_VERTEX_3);
              gluEndSurface(NurbsRenderer);
              gluDeleteNurbsRenderer(NurbsRenderer);
            end;

        end;
        mrci.GLStates.PopAttrib;
      end;
  end;
end;

// Prepare
//

procedure TMOParametricSurface.Prepare;
var
  i: integer;
begin
  // We want to clear everything but the parametric surface
  // data (control points and knot vectors).
  inherited Clear;

  // Apply weights to control points
  FWeightedControlPoints.Assign(FControlPoints);
  if FWeights.Count = FControlPoints.Count then
    for i := 0 to FWeightedControlPoints.Count - 1 do
      FWeightedControlPoints[i] := VectorScale(FWeightedControlPoints[i], FWeights[i]);

  case FRenderer of
    psrGLScene:
      begin
        GenerateMesh;
      end;
    psrOpenGL:
      begin
        if (FAutoKnots) and (FBasis = psbBSpline) then
        begin
          GenerateKnotVector(FKnotsU, FCountU, FOrderU, FContinuity);
          GenerateKnotVector(FKnotsV, FCountV, FOrderV, FContinuity);
        end;
      end;
  end;
end;

// Clear
//

procedure TMOParametricSurface.Clear;
begin
  inherited;
  FControlPoints.Clear;
  FKnotsU.Clear;
  FKnotsV.Clear;
  FWeights.Clear;
end;

// GenerateMesh
//

procedure TMOParametricSurface.GenerateMesh;
var
  i, j: Integer;
  fg: TFGVertexIndexList;
begin
  case FBasis of
    psbBezier:
      begin
        if FAutoKnots then
        begin
          FKnotsU.Clear;
          FKnotsV.Clear;
        end;
        GenerateBezierSurface(FResolution, FCountU, FCountV, FControlPoints, Vertices);
      end;
    psbBSpline:
      begin
        if FAutoKnots then
        begin
          GenerateKnotVector(FKnotsU, FCountU, FOrderU, FContinuity);
          GenerateKnotVector(FKnotsV, FCountV, FOrderV, FContinuity);
        end;
        GenerateBSplineSurface(FResolution, FOrderU, FOrderV, FCountU, FCountV, FKnotsU, FKnotsV, FControlPoints, Vertices);
      end;
  end;

  Mode := momFaceGroups;
  fg := TFGVertexIndexList.CreateOwned(FaceGroups);
  fg.Mode := fgmmTriangles;
  for j := 0 to FResolution - 2 do
    with fg do
      for i := 0 to FResolution - 2 do
      begin
        VertexIndices.Add(i + FResolution * j);
        VertexIndices.Add((i + 1) + FResolution * j);
        VertexIndices.Add(i + FResolution * (j + 1));

        VertexIndices.Add(i + FResolution * (j + 1));
        VertexIndices.Add((i + 1) + FResolution * j);
        VertexIndices.Add((i + 1) + FResolution * (j + 1));
      end;
  BuildNormals(fg.VertexIndices, momTriangles);

end;

// SetControlPoints
//

procedure TMOParametricSurface.SetControlPoints(Value: TAffineVectorList);
begin
  FControlPoints.Assign(Value);
end;

// SetKnotsU
//

procedure TMOParametricSurface.SetKnotsU(Value: TSingleList);
begin
  FKnotsU.Assign(Value);
end;

// SetKnotsV
//

procedure TMOParametricSurface.SetKnotsV(Value: TSingleList);
begin
  FKnotsV.Assign(Value);
end;

// SetWeights
//

procedure TMOParametricSurface.SetWeights(Value: TSingleList);
begin
  FWeights.Assign(Value);
end;

// SetRenderer
//

procedure TMOParametricSurface.SetRenderer(
  Value: TParametricSurfaceRenderer);
begin
  if Value <> FRenderer then
  begin
    FRenderer := Value;
    Owner.Owner.StructureChanged;
  end;
end;

// SetBasis
//

procedure TMOParametricSurface.SetBasis(Value: TParametricSurfaceBasis);
begin
  if Value <> FBasis then
  begin
    FBasis := Value;
    Owner.Owner.StructureChanged;
  end;
end;

// ------------------
// ------------------ TFGBezierSurface ------------------
// ------------------

// Create
//

constructor TFGBezierSurface.Create;
begin
  inherited;
  FControlPointIndices := TIntegerList.Create;
  FTexCoordIndices := TIntegerList.Create;
  FTempControlPoints := TAffineVectorList.Create;
  FTempTexCoords := TAffineVectorList.Create;

  // Default values
  FCountU := 4;
  FCountV := 4;
  FResolution := 20;
  FMinU := 0;
  FMaxU := 1;
  FMinV := 0;
  FMaxV := 1;
end;

// Destroy
//

destructor TFGBezierSurface.Destroy;
begin
  FControlPointIndices.Free;
  FTexCoordIndices.Free;
  FTempControlPoints.Free;
  FTempTexCoords.Free;
  inherited;
end;

// WriteToFiler
//

procedure TFGBezierSurface.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FControlPointIndices.WriteToFiler(writer);
    FTexCoordIndices.WriteToFiler(writer);
    WriteInteger(FCountU);
    WriteInteger(FCountV);
    WriteInteger(FResolution);
    WriteFloat(FMinU);
    WriteFloat(FMaxU);
    WriteFloat(FMinV);
    WriteFloat(FMaxV);
  end;
end;

// ReadFromFiler
//

procedure TFGBezierSurface.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FControlPointIndices.ReadFromFiler(reader);
      FTexCoordIndices.ReadFromFiler(reader);
      FCountU := ReadInteger;
      FCountV := ReadInteger;
      FResolution := ReadInteger;
      FMinU := ReadFloat;
      FMaxU := ReadFloat;
      FMinV := ReadFloat;
      FMaxV := ReadFloat;
    end
  else
    RaiseFilerException(archiveVersion);
end;

// BuildList
//

procedure TFGBezierSurface.BuildList(var mrci: TRenderContextInfo);
begin
  if (FTempControlPoints.Count = 0)
    or (FTempControlPoints.Count <> FControlPointIndices.Count) then
    Exit;

  AttachOrDetachLightmap(mrci);

  mrci.GLStates.PushAttrib([sttEnable, sttEval]);
  mrci.GLStates.Enable(stAutoNormal);
  mrci.GLStates.Enable(stNormalize);

  GL.MapGrid2f(FResolution, MaxU, MinU, FResolution, MinV, MaxV);

  if FTempTexCoords.Count > 0 then
  begin
    GL.Enable(GL_MAP2_TEXTURE_COORD_3);
    GL.Map2f(GL_MAP2_TEXTURE_COORD_3,
      0, 1, 3, FCountU,
      0, 1, 3 * FCountU, FCountV,
      @FTempTexCoords.List[0]);
  end;

  GL.Enable(GL_MAP2_VERTEX_3);
  GL.Map2f(GL_MAP2_VERTEX_3,
    0, 1, 3, FCountU,
    0, 1, 3 * FCountU, FCountV,
    @FTempControlPoints.List[0]);

  GL.EvalMesh2(GL_FILL, 0, FResolution, 0, FResolution);

  mrci.GLStates.PopAttrib;
end;

// SetControlPointIndices
//

procedure TFGBezierSurface.SetControlPointIndices(const Value: TIntegerList);
begin
  FControlPointIndices.Assign(Value);
end;

// SetTexCoordIndices
//

procedure TFGBezierSurface.SetTexCoordIndices(const Value: TIntegerList);
begin
  FTexCoordIndices.Assign(Value);
end;

// Prepare
//

procedure TFGBezierSurface.Prepare;
var
  i, j: Integer;
begin
  inherited;
  FTempControlPoints.Clear;
  FTempTexCoords.Clear;
  for j := 0 to CountV - 1 do
    for i := CountU - 1 downto 0 do
    begin
      FTempControlPoints.Add(Owner.Owner.Vertices[ControlPointIndices[i + CountU * j]]);
      if TexCoordIndices.Count = ControlPointIndices.Count then
        FTempTexCoords.Add(Owner.Owner.TexCoords[TexCoordIndices[i + CountU * j]]);
    end;
end;

end.

