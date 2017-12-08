//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Preliminary VRML vector file support for GLScene.
    10/11/12 - PW - Added CPP compatibility: changed vector arrays to records
    History : 
       29/03/07 - DaStr - RecursNodes bugfixed (thanks Burkhard Carstens)
       25/01/05 - SG - Improved auto-normal generation using creaseAngle,
                          Added Normal and TexCoord reading,
                          Fixes for the polygon tessellation routine (Carsten Pohl)
       18/01/05 - SG - Added polygon tessellation routine to decompose 
                          a polygon mesh to a triangle mesh
       14/01/05 - SG - Added to CVS
    
}
unit GLFileVRML;

interface

uses
  Classes, SysUtils, GLVectorFileObjects, GLMaterial, GLApplicationFileIO,
  GLVectorTypes, GLVectorGeometry, GLVectorLists, VRMLParser, GLMeshUtils;

type

  TGLVRMLVectorFile = class (TGLVectorFile)
    public
      class function Capabilities : TGLDataFileCapabilities; override;
      procedure LoadFromStream(aStream : TStream); override;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// TessellatePolygon
//
procedure TessellatePolygon(PolyVerts : TAffineVectorList;
  PolyIndices, TriIndices : TIntegerList);

  function IsPolyClockWise : Boolean;
  var
    i,j : Integer;
    det : Single;
    mat : TAffineMatrix;
  begin
    det:=0;
    for i:=0 to PolyIndices.Count-1 do begin
      for j:=0 to 2 do
        if (i+j)>=PolyIndices.Count then
          mat.V[j]:=PolyVerts[PolyIndices[i+j-PolyIndices.Count]]
        else
          mat.V[j]:=PolyVerts[PolyIndices[i+j]];
      det:=det+MatrixDeterminant(mat);
    end;
    Result:=(det<0);
  end;

  function IsTriClockWise(v0, v1, v2 : TAffineVector) : Boolean;
  var
    mat : TAffineMatrix;
  begin
    mat.V[0]:=v0;
    mat.V[1]:=v1;
    mat.V[2]:=v2;
    Result:=(MatrixDeterminant(mat)<0);
  end;

  function PointInTriangle(p,v0,v1,v2 : TAffineVector;
    IsClockWise : Boolean = False) : Boolean;
  begin
    Result:=not ((IsTriClockWise(v1,v0,p) = IsClockWise) or
                 (IsTriClockWise(v0,v2,p) = IsClockWise) or
                 (IsTriClockWise(v2,v1,p) = IsClockWise));
  end;

var
  i, j,
  prev, next,
  min_vert, min_prev, min_next : Integer;
  PolyCW, NoPointsInTriangle : Boolean;
  v : TAffineMatrix;
  temp : TIntegerList;
  min_dist, d, area : Single;
begin
  temp:=TIntegerList.Create;
  try
    PolyCW:=IsPolyClockWise;
    temp.Assign(PolyIndices);
    while temp.Count>3 do begin
      min_dist:=10e7;
      min_vert:=-1;
      min_prev:=-1;
      min_next:=-1;
      for i:=0 to temp.Count-1 do begin
        prev:=i-1;
        next:=i+1;
        if prev<0 then prev:=temp.Count-1;
        if next>temp.Count-1 then next:=0;
        v.V[0]:=PolyVerts[temp[prev]];
        v.V[1]:=PolyVerts[temp[i]];
        v.V[2]:=PolyVerts[temp[next]];
        if IsTriClockWise(v.V[0], v.V[1], v.V[2]) = PolyCW then begin
          NoPointsInTriangle:=True;
          for j:=0 to temp.Count-1 do begin
            if (j<>i) and (j<>prev) and (j<>next) then begin
              if PointInTriangle(PolyVerts[temp[j]], v.V[0], v.V[1], v.V[2], PolyCW) then begin
                NoPointsInTriangle:=False;
                Break;
              end;
            end;
          end;

          area:=TriangleArea(v.V[0],v.V[1],v.V[2]);

          if NoPointsInTriangle and (area>0) then begin
            d:=VectorDistance2(v.V[0], v.V[2]);
            if d<min_dist then begin
              min_dist:=d;
              min_prev:=prev;
              min_vert:=i;
              min_next:=next;
            end;
          end;
        end;
      end;
      if min_vert = -1 then begin
        raise Exception.Create('Failed to tessellate polygon.');
      end else begin
        TriIndices.Add(temp[min_prev], temp[min_vert], temp[min_next]);
        temp.Delete(min_vert);
      end;
    end;
    TriIndices.Add(temp[0], temp[1], temp[2]);
  finally
    temp.Free;
  end;
end;


// ------------------
// ------------------ TGLVRMLVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLVRMLVectorFile.Capabilities : TGLDataFileCapabilities;
begin
  Result:=[dfcRead];
end;

// LoadFromStream
//
procedure TGLVRMLVectorFile.LoadFromStream(aStream : TStream);
var
  mesh : TGLMeshObject;
  uniqueMatID : Integer;
  currentMaterial : TGLLibMaterial;
  currentTransform : TMatrix;
  creaseAngle : Single;

  function GetUniqueMaterialName : String;
  var
    libMat : TGLLibMaterial;
  begin
    repeat
      Result:='UntitledMaterial'+IntToStr(uniqueMatID);
      Inc(uniqueMatID);
      libMat:=Owner.MaterialLibrary.Materials.GetLibMaterialByName(Result);
    until not Assigned(libMat);
  end;

  function AddMaterialToLibrary(VRMLMaterial : TVRMLMaterial) : TGLLibMaterial;
  var
    matname : String;
  begin
    Result:=nil;
    if not Assigned(Owner.MaterialLibrary) then Exit;

    if VRMLMaterial.DefName = '' then
      matname:=GetUniqueMaterialName
    else
      matname:=VRMLMaterial.DefName;

    Result:=Owner.MaterialLibrary.Materials.GetLibMaterialByName(matname);
    if not Assigned(Result) then begin
      Result:=Owner.MaterialLibrary.Materials.Add;
      Result.Name:=matname;
    end;

    //Assigned values from the current material
    if Assigned(currentMaterial) then
      Result.Material.FrontProperties.Assign(currentMaterial.Material.FrontProperties);

    with Result.Material.FrontProperties do begin
      if VRMLMaterial.HasDiffuse then
        Diffuse.Color:=VectorMake(VRMLMaterial.DiffuseColor, Diffuse.Color.V[3]);
      if VRMLMaterial.HasAmbient then
        Ambient.Color:=VectorMake(VRMLMaterial.AmbientColor, Ambient.Color.V[3]);
      if VRMLMaterial.HasSpecular then
        Specular.Color:=VectorMake(VRMLMaterial.SpecularColor, Specular.Color.V[3]);
      if VRMLMaterial.HasEmissive then
        Emission.Color:=VectorMake(VRMLMaterial.EmissiveColor, Emission.Color.V[3]);
      if Shininess = 0 then Shininess:=16;
      if VRMLMaterial.HasShininess then
        Shininess:=Floor(128*VRMLMaterial.Shininess);
      if VRMLMaterial.HasTransparency then begin
        Diffuse.Color:=VectorMake(AffineVectorMake(Diffuse.Color),
                                  1-VRMLMaterial.Transparency);
        Ambient.Color:=VectorMake(AffineVectorMake(Ambient.Color),
                                  1-VRMLMaterial.Transparency);
        Specular.Color:=VectorMake(AffineVectorMake(Specular.Color),
                                  1-VRMLMaterial.Transparency);
        Emission.Color:=VectorMake(AffineVectorMake(Emission.Color),
                                  1-VRMLMaterial.Transparency);
      end;
    end;
    if VRMLMaterial.HasTransparency then
      Result.Material.BlendingMode:=bmTransparency;
  end;

  procedure RebuildMesh;
  var
    i,j,k,l : Integer;
    newfg : TFGVertexIndexList;
    fg : TFGVertexNormalTexIndexList;
    vertices, normals, texcoords, triNormals,
    newVertices, newNormals, newTexCoords : TAffineVectorList;
    optimized : TIntegerList;
    cosAngle : Single;
    normal : TAffineVector;
    s, t : array[0..2] of Integer;
    n : array[0..2] of TIntegerList;
    smooth,
    hasVertices,
    hasNormals, hasNormalIndices,
    hasTexCoords, hasTexCoordIndices : Boolean;
  begin
    if not Assigned(mesh) then Exit;

    hasVertices:=mesh.Vertices.Count>0;
    hasNormals:=mesh.Normals.Count>0;
    hasTexCoords:=mesh.TexCoords.Count>0;

    if not hasVertices then Exit;

    vertices:=TAffineVectorList.Create;
    normals:=TAffineVectorList.Create;
    texcoords:=TAffineVectorList.Create;
    newVertices:=TAffineVectorList.Create;
    newNormals:=TAffineVectorList.Create;
    newTexCoords:=TAffineVectorList.Create;
    triNormals:=TAffineVectorList.Create;
    n[0]:=TIntegerList.Create;
    n[1]:=TIntegerList.Create;
    n[2]:=TIntegerList.Create;
    for i:=0 to mesh.FaceGroups.Count-1 do begin
      fg:=TFGVertexNormalTexIndexList(mesh.FaceGroups[i]);

      hasNormalIndices:=fg.NormalIndices.Count>0;
      hasTexCoordIndices:=fg.TexCoordIndices.Count>0;

      vertices.Clear;
      normals.Clear;
      texcoords.Clear;
      triNormals.Clear;

      if not hasNormals then begin
        for j:=0 to (fg.VertexIndices.Count div 3)-1 do begin
          normal:=VectorCrossProduct(
            VectorNormalize(VectorSubtract(
              mesh.Vertices[fg.VertexIndices[3*j+1]],
              mesh.Vertices[fg.VertexIndices[3*j]])),
            VectorNormalize(VectorSubtract(
              mesh.Vertices[fg.VertexIndices[3*j+2]],
              mesh.Vertices[fg.VertexIndices[3*j]])));
          triNormals.Add(VectorNormalize(normal));
        end;
      end;

      for j:=0 to (fg.VertexIndices.Count div 3)-1 do begin
        vertices.Add(
          mesh.Vertices[fg.VertexIndices[3*j]],
          mesh.Vertices[fg.VertexIndices[3*j+1]],
          mesh.Vertices[fg.VertexIndices[3*j+2]]);

        if hasNormals then begin
          if hasNormalIndices then begin
            normals.Add(
              mesh.Normals[fg.NormalIndices[3*j]],
              mesh.Normals[fg.NormalIndices[3*j+1]],
              mesh.Normals[fg.NormalIndices[3*j+2]]);
          end else begin
            normals.Add(
              mesh.Normals[fg.VertexIndices[3*j]],
              mesh.Normals[fg.VertexIndices[3*j+1]],
              mesh.Normals[fg.VertexIndices[3*j+2]]);
          end;
        end else begin
          // No normal data, generate the normals
          n[0].Clear;
          n[1].Clear;
          n[2].Clear;
          s[0]:=fg.VertexIndices[3*j];
          s[1]:=fg.VertexIndices[3*j+1];
          s[2]:=fg.VertexIndices[3*j+2];
          for k:=0 to (fg.VertexIndices.Count div 3)-1 do
          if j<>k then begin
            t[0]:=fg.VertexIndices[3*k];
            t[1]:=fg.VertexIndices[3*k+1];
            t[2]:=fg.VertexIndices[3*k+2];
            if (s[0]=t[0]) or (s[0]=t[1]) or (s[0]=t[2]) then
              n[0].Add(k);
            if (s[1]=t[0]) or (s[1]=t[1]) or (s[1]=t[2]) then
              n[1].Add(k);
            if (s[2]=t[0]) or (s[2]=t[1]) or (s[2]=t[2]) then
              n[2].Add(k);
          end;

          for k:=0 to 2 do begin
            if n[k].Count>0 then begin
              smooth:=True;
              for l:=0 to n[k].Count-1 do begin
                cosAngle:=VectorAngleCosine(triNormals[j], triNormals[n[k][l]]);
                smooth:=smooth and (cosAngle>Cos(creaseAngle));
                if not Smooth then Break;
              end;
              if smooth then begin
                normal:=triNormals[j];
                for l:=0 to n[k].Count-1 do
                  AddVector(normal, triNormals[n[k][l]]);
                ScaleVector(normal, 1/(n[k].Count+1));
                normals.Add(VectorNormalize(normal));
              end else
                normals.Add(triNormals[j]);
            end else begin
              normals.Add(triNormals[j]);
            end;
          end;
        end;

        if hasTexCoords then begin
          if hasTexCoordIndices then begin
            texcoords.Add(
              mesh.TexCoords[fg.TexCoordIndices[3*j]],
              mesh.TexCoords[fg.TexCoordIndices[3*j+1]],
              mesh.TexCoords[fg.TexCoordIndices[3*j+2]]);
          end else begin
            texcoords.Add(
              mesh.TexCoords[fg.VertexIndices[3*j]],
              mesh.TexCoords[fg.VertexIndices[3*j+1]],
              mesh.TexCoords[fg.VertexIndices[3*j+2]]);
          end;
        end;

      end;

      // Optimize the mesh
      if hasTexCoords then begin
        optimized:=BuildVectorCountOptimizedIndices(vertices, normals, texcoords);
        RemapReferences(texcoords, optimized);
      end else
        optimized:=BuildVectorCountOptimizedIndices(vertices, normals);
      RemapReferences(normals, optimized);
      RemapAndCleanupReferences(vertices, optimized);
      optimized.Offset(newVertices.Count);

      // Replace the facegroup with a vertex-only index list
      newfg:=TFGVertexIndexList.Create;
      newfg.Owner:=mesh.FaceGroups;
      newfg.Mode:=fg.Mode;
      newfg.MaterialName:=fg.MaterialName;
      newfg.VertexIndices.Assign(optimized);
      mesh.FaceGroups.Insert(i, newfg);
      mesh.FaceGroups.RemoveAndFree(fg);
      optimized.Free;

      newVertices.Add(vertices);
      newNormals.Add(normals);
      newTexCoords.Add(texcoords);
    end;
    vertices.Free;
    normals.Free;
    texcoords.Free;
    n[0].Free;
    n[1].Free;
    n[2].Free;
    triNormals.Free;

    if newVertices.Count>0 then
      mesh.Vertices.Assign(newVertices);
    if newNormals.Count>0 then
      mesh.Normals.Assign(newNormals);
    if newTexCoords.Count>0 then
      mesh.TexCoords.Assign(newTexCoords);

    newVertices.Free;
    newNormals.Free;
    newTexCoords.Free;
  end;

  procedure RecursNodes(node : TVRMLNode);
  var
    i,j,n : Integer;
    points : TSingleList;
    indices, fgindices : TIntegerList;
    fg : TFGVertexNormalTexIndexList;
    face : TIntegerList;
    tempLibMat : TGLLibMaterial;
    saveTransform, mat : TMatrix;
    saveMaterial : TGLLibMaterial;
    axis : TAffineVector;
    angle : Single;
  begin
    // Store current transform and material
    saveTransform:=currentTransform;
    saveMaterial:=currentMaterial;

    // Look for a child node data (transforms and materials)
    for i:=0 to node.Count-1 do
      if node[i] is TVRMLTransform then begin
        if not VectorEquals(TVRMLTransform(node[i]).Rotation, NullHMGVector) then begin
          axis:=AffineVectorMake(TVRMLTransform(node[i]).Rotation);
          angle:=TVRMLTransform(node[i]).Rotation.V[3];
          mat:=MatrixMultiply(CreateRotationMatrix(axis, angle),
                              CreateRotationMatrixZ(Pi/2));
        end else
          mat:=IdentityHMGMatrix;
        for j:=0 to 2 do
          mat.V[j]:=VectorScale(mat.V[j], TVRMLTransform(node[i]).ScaleFactor.V[j]);
        mat.V[3]:=PointMake(TVRMLTransform(node[i]).Center);
        currentTransform:=MatrixMultiply(mat, currentTransform);
      end else if node[i] is TVRMLMaterial then begin
        currentMaterial:=AddMaterialToLibrary(TVRMLMaterial(node[i]));
      end else if node[i] is TVRMLShapeHints then begin
        creaseAngle:=TVRMLShapeHints(node[i]).CreaseAngle;
      end else if node[i] is TVRMLUse then begin
       if Assigned(Owner.MaterialLibrary) then begin
        tempLibMat:=Owner.MaterialLibrary.Materials.GetLibMaterialByName(TVRMLUse(node[i]).Value);
        if Assigned(tempLibMat) then
          currentMaterial:=tempLibMat;
        end;
      end;

    // Read node data
    if (node.Name = 'Coordinate3') and (node.Count>0) then begin
      RebuildMesh;
      mesh:=TGLMeshObject.CreateOwned(Owner.MeshObjects);
      points:=TVRMLSingleArray(node[0]).Values;
      for i:=0 to (points.Count div 3) - 1 do
        mesh.Vertices.Add(points[3*i], points[3*i+1], points[3*i+2]);
      mesh.Vertices.TransformAsPoints(currentTransform);

    end else if (node.Name = 'Normal') and (node.Count>0) and Assigned(mesh) then begin
      points:=TVRMLSingleArray(node[0]).Values;
      for i:=0 to (points.Count div 3) - 1 do
        mesh.Normals.Add(points[3*i], points[3*i+1], points[3*i+2]);
      mesh.Normals.TransformAsVectors(currentTransform);

    end else if (node.Name = 'TextureCoordinate2') and (node.Count>0) and Assigned(mesh) then begin
      points:=TVRMLSingleArray(node[0]).Values;
      for i:=0 to (points.Count div 2) - 1 do
        mesh.TexCoords.Add(points[2*i], points[2*i+1], 0);

    end else if (node.Name = 'IndexedFaceSet') and (node.Count>0) and Assigned(mesh) then begin
      fg:=TFGVertexNormalTexIndexList.CreateOwned(mesh.FaceGroups);
      mesh.Mode:=momFaceGroups;
      face:=TIntegerList.Create;
      if Assigned(currentMaterial) then
        fg.MaterialName:=currentMaterial.Name;
      for n:=0 to node.Count-1 do begin
        if node[n].Name = 'CoordIndexArray' then
          fgindices:=fg.VertexIndices
        else if node[n].Name = 'NormalIndexArray' then
          fgindices:=fg.NormalIndices
        else if node[n].Name = 'TextureCoordIndexArray' then
          fgindices:=fg.TexCoordIndices
        else
          fgindices:=nil;

        if not Assigned(fgindices) then Continue;

        indices:=TVRMLIntegerArray(node[n]).Values;
        i:=0;
        while i<indices.Count do begin
          if indices[i] = -1 then begin
            if face.Count<=4 then begin
              for j:=0 to face.Count-3 do
                fgindices.Add(face[0], face[j+1], face[j+2]);
            end else begin
              TessellatePolygon(mesh.Vertices, face, fgindices);
            end;
            face.Clear;
          end else begin
            face.Add(indices[i]);
          end;
          i:=i+1;
        end;
      end;

      face.Free;

    end else begin
      // Continue recursion through child nodes
      for i:=0 to node.Count-1 do
        RecursNodes(node[i]);
    end;

    // Load transform and material from stored values
    currentTransform:=saveTransform;
    currentMaterial:=saveMaterial;
  end;

var
  str : TStringList;
  parser : TVRMLParser;
begin
  str:=TStringList.Create;
  parser:=TVRMLParser.Create;
  currentMaterial:=nil;
  currentTransform:=IdentityHMGMatrix;
  creaseAngle:=0.5;
  mesh:=nil;
  uniqueMatID:=0;
  try
    str.LoadFromStream(aStream);
    parser.Parse(str.Text);
    currentMaterial:=nil;
    RecursNodes(parser.RootNode);
    RebuildMesh;
  finally
    str.Free;
    parser.Free;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('wrl', 'VRML files', TGLVRMLVectorFile);

end.
