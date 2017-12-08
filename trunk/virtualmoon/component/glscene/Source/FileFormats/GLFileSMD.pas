//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   SMD vector file format implementation.

	 History : 
       24/03/07 - DaStr - Added explicit pointer dereferencing
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
       28/01/07 - DaStr - Optimized bone weights loading a bit
       14/01/07 - DaStr - Fixed bone weights for HL2 models (thanks DIVON)
       24/01/05 - SG - Fix for comma decimal separator in save function (dikoe Kenguru)
       30/03/04 - EG - Basic Half-Life2/XSI support
       05/06/03 - SG - Separated from GLVectorFileObjects.pas
	 
}
unit GLFileSMD;

interface

uses
  Classes, SysUtils,
  GLVectorFileObjects, GLTexture, GLApplicationFileIO,
  GLVectorTypes, GLVectorGeometry, GLMaterial;

type
   // TGLSMDVectorFile
   //
   { The SMD vector file is Half-life's skeleton format.
      The SMD is a text-based file format. They come in two flavors: one that
      old Skeleton and triangle (mesh) data, and animation files that store
      Skeleton frames.
      This reader curently reads both, but requires that the main file
      (the one with mesh data) be read first. }
   TGLSMDVectorFile = class(TGLVectorFile)
      public
          
         class function Capabilities : TGLDataFileCapabilities; override;

         procedure LoadFromStream(aStream : TStream); override;
         procedure SaveToStream(aStream : TStream); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses GLUtils;

// ------------------
// ------------------ TGLSMDVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLSMDVectorFile.Capabilities : TGLDataFileCapabilities;
begin
   Result:=[dfcRead, dfcWrite];
end;

// LoadFromStream
//
procedure TGLSMDVectorFile.LoadFromStream(aStream : TStream);

   procedure AllocateMaterial(const name : String);
   var
      matLib : TGLMaterialLibrary;
   begin
      if Owner is TGLBaseMesh then begin
         matLib:=TGLBaseMesh(GetOwner).MaterialLibrary;
         if Assigned(matLib) then begin
            if matLib.Materials.GetLibMaterialByName(name)=nil then begin
               if CompareText(name, 'null.bmp')<>0 then begin
                  try
                     matLib.AddTextureMaterial(name, name)
                  except
                     on E: ETexture do begin
                        if not Owner.IgnoreMissingTextures then
                           raise;
                     end;
                  end;
               end else matLib.AddTextureMaterial(name, '');
            end;
         end;
      end;
   end;

var
   i, j, k, nVert, nTex, firstFrame : Integer;
   nbBones, boneID : Integer;
   mesh : TGLSkeletonMeshObject;
   sl, tl : TStringList;
   bone : TGLSkeletonBone;
   frame : TGLSkeletonFrame;
   faceGroup : TFGVertexNormalTexIndexList;
   v : TAffineVector;

   boneIDs : TVertexBoneWeightDynArray;
   weightCount: Integer;
begin
   sl:=TStringList.Create;
   tl:=TStringList.Create;
   try
      sl.LoadFromStream(aStream);
      if sl[0]<>'version 1' then
         raise Exception.Create('SMD version 1 required');
      if sl[1]<>'nodes' then
         raise Exception.Create('nodes not found');
      if sl.IndexOf('triangles')>=0 then begin
         mesh:=TGLSkeletonMeshObject.CreateOwned(Owner.MeshObjects);
         mesh.Mode:=momFaceGroups;
      end else if Owner.MeshObjects.Count>0 then
         mesh:=(Owner.MeshObjects[0] as TGLSkeletonMeshObject)
      else raise Exception.Create('SMD is an animation, load model SMD first.');
      // read skeleton nodes
      i:=2;
      if Owner.Skeleton.RootBones.Count=0 then begin
         // new bone structure
         while sl[i]<>'end' do begin
            tl.CommaText:=sl[i];
            with Owner.Skeleton do
               if (tl[2]<>'-1') then
                  bone:=TGLSkeletonBone.CreateOwned(RootBones.BoneByID(StrToInt(tl[2])))
               else bone:=TGLSkeletonBone.CreateOwned(RootBones);
            if Assigned(bone) then begin
               bone.BoneID:=StrToInt(tl[0]);
               bone.Name:=tl[1];
            end;
            Inc(i);
         end;
      end else begin
         // animation file, skip structure
         while sl[i]<>'end' do Inc(i);
      end;
      Inc(i);
      if sl[i]<>'skeleton' then
         raise Exception.Create('skeleton not found');
      Inc(i);
      // read animation time frames
      nbBones:=Owner.Skeleton.RootBones.BoneCount-1;
      firstFrame:=Owner.Skeleton.Frames.Count;
      while sl[i]<>'end' do begin
         if Copy(sl[i], 1, 5)<>'time ' then
            raise Exception.Create('time not found, got: '+sl[i]);
         frame:=TGLSkeletonFrame.CreateOwned(Owner.Skeleton.Frames);
         frame.Name:=ResourceName+' '+sl[i];
         Inc(i);
         while Pos(Copy(sl[i], 1, 1), ' 1234567890')>0 do begin
            tl.CommaText:=sl[i];
            while StrToInt(tl[0])>frame.Position.Count do begin
               frame.Position.Add(NullVector);
               frame.Rotation.Add(NullVector);
            end;
            frame.Position.Add(GLUtils.StrToFloatDef(tl[1]), GLUtils.StrToFloatDef(tl[2]), GLUtils.StrToFloatDef(tl[3]));
            v:=AffineVectorMake(GLUtils.StrToFloatDef(tl[4]), GLUtils.StrToFloatDef(tl[5]), GLUtils.StrToFloatDef(tl[6]));
            frame.Rotation.Add(v);
            Inc(i);
         end;
         while frame.Position.Count<nbBones do begin
            frame.Position.Add(NullVector);
            frame.Rotation.Add(NullVector);
         end;
         Assert(frame.Position.Count=nbBones,
                'Invalid number of bones in frame '+IntToStr(Owner.Skeleton.Frames.Count));
      end;
      if Owner is TGLActor then with TGLActor(Owner).Animations.Add do begin
         k:=Pos('.', ResourceName);
         if k>0 then
            Name:=Copy(ResourceName, 1, k-1)
         else Name:=ResourceName;
         Reference:=aarSkeleton;
         StartFrame:=firstFrame;
         EndFrame:=Self.Owner.Skeleton.Frames.Count-1;
      end;
      Inc(i);
      if (i<sl.Count) and (sl[i]='triangles') then begin
         // read optional mesh data
         Inc(i);
         if mesh.BonesPerVertex<1 then
            mesh.BonesPerVertex:=1;
         faceGroup:=nil;

         while sl[i]<>'end' do begin
            if (faceGroup=nil) or (faceGroup.MaterialName<>sl[i]) then begin
               faceGroup:=TFGVertexNormalTexIndexList.CreateOwned(mesh.FaceGroups);
               faceGroup.Mode:=fgmmTriangles;
               faceGroup.MaterialName:=sl[i];
               AllocateMaterial(sl[i]);
            end;
            Inc(i);

            for k:=1 to 3 do with mesh do begin
                 tl.CommaText:=sl[i];

                 if tl.Count>9
                 then begin
                   // Half-Life 2 SMD, specifies bones and weights
                   weightCount := StrToInt(tl[9]);
                   SetLength(boneIDs,weightCount);
                   for j := 0 to weightCount - 1 do begin
                     BoneIDs[j].BoneID := StrToInt(tl[10+j*2]);
                     BoneIDs[j].Weight := GLUtils.StrToFloatDef(tl[11+j*2]);
                   end;

                   nVert:=FindOrAdd(boneIDs, AffineVectorMake(GLUtils.StrToFloatDef(tl[1]), GLUtils.StrToFloatDef(tl[2]), GLUtils.StrToFloatDef(tl[3])),                                AffineVectorMake(GLUtils.StrToFloatDef(tl[4]), GLUtils.StrToFloatDef(tl[5]), GLUtils.StrToFloatDef(tl[6])));
                   nTex:=TexCoords.FindOrAdd(AffineVectorMake(GLUtils.StrToFloatDef(tl[7]), GLUtils.StrToFloatDef(tl[8]), 0));
                   faceGroup.Add(nVert, nVert, nTex);
                   Inc(i);
                 end
                 else
                 begin
                   // Half-Life 1 simple format
                   boneID:=StrToInt(tl[0]);
                   nVert:=FindOrAdd(boneID,AffineVectorMake(GLUtils.StrToFloatDef(tl[1]), GLUtils.StrToFloatDef(tl[2]), GLUtils.StrToFloatDef(tl[3])),                                AffineVectorMake(GLUtils.StrToFloatDef(tl[4]), GLUtils.StrToFloatDef(tl[5]), GLUtils.StrToFloatDef(tl[6])));
                   nTex:=TexCoords.FindOrAdd(AffineVectorMake(GLUtils.StrToFloatDef(tl[7]), GLUtils.StrToFloatDef(tl[8]), 0));
                   faceGroup.Add(nVert, nVert, nTex);
                   Inc(i);
                 end;
            end;
         end;
         Owner.Skeleton.RootBones.PrepareGlobalMatrices;
         mesh.PrepareBoneMatrixInvertedMeshes;
      end;
   finally
      tl.Free;
      sl.Free;
   end;
end;

// SaveToStream
//
procedure TGLSMDVectorFile.SaveToStream(aStream : TStream);
var
   str,
   nodes     : TStrings;
   i,j,k,l,b : Integer;
   p,r,v,n,t : TAffineVector;

   procedure GetNodesFromBonesRecurs(bone : TGLSkeletonBone; ParentID : Integer; bl : TStrings);
   var
      i : Integer;
   begin
      bl.Add(Format('%3d "%s" %3d',[bone.BoneID,bone.Name,ParentID]));
      for i:=0 to bone.Count-1 do
         GetNodesFromBonesRecurs(bone.Items[i],bone.BoneID,bl);
   end;

begin
   str:=TStringList.Create;
   nodes:=TStringList.Create;
   try
      str.Add('version 1');

      // Add the bones
      str.Add('nodes');
      for i:=0 to Owner.Skeleton.RootBones.Count-1 do begin
         GetNodesFromBonesRecurs(Owner.Skeleton.RootBones[i],-1,nodes);
      end;
      str.AddStrings(nodes);
      str.Add('end');

      // Now add the relavent frames
      if Owner.Skeleton.Frames.Count>0 then begin
         str.Add('skeleton');
         for i:=0 to Owner.Skeleton.Frames.Count-1 do begin
            str.Add(Format('time %d',[i]));
            for j:=0 to Owner.Skeleton.Frames[i].Position.Count-1 do begin
               p:=Owner.Skeleton.Frames[i].Position[j];
               r:=Owner.Skeleton.Frames[i].Rotation[j];
               str.Add(StringReplace(
                 Format('%3d %.6f %.6f %.6f %.6f %.6f %.6f',
                        [j,p.V[0],p.V[1],p.V[2],r.V[0],r.V[1],r.V[2]]),
                 ',', '.', [rfReplaceAll]));
            end;
         end;
         str.Add('end');
      end;

      // Add the mesh data
      if Owner.MeshObjects.Count>0 then begin
         str.Add('triangles');
         for i:=0 to Owner.MeshObjects.Count-1 do
         if Owner.MeshObjects[i] is TGLSkeletonMeshObject then
         with TGLSkeletonMeshObject(Owner.MeshObjects[i]) do begin
            for j:=0 to FaceGroups.Count-1 do 
            with TFGVertexNormalTexIndexList(FaceGroups[j]) do begin
               for k:=0 to (VertexIndices.Count div 3)-1 do begin
                  str.Add(MaterialName);
                  for l:=0 to 2 do begin
                     v:=Vertices[VertexIndices[3*k+l]];
                     n:=Normals[NormalIndices[3*k+l]];
                     t:=TexCoords[TexCoordIndices[3*k+l]];
                     b:=VerticesBonesWeights^[VertexIndices[3*k+l]]^[0].BoneID;
                     str.Add(StringReplace(
                       Format('%3d %.4f %.4f %.4f %.4f %.4f %.4f %.4f %.4f',
                              [b,v.V[0],v.V[1],v.V[2],n.V[0],n.V[1],n.V[2],t.V[0],t.V[1]]),
                       ',', '.', [rfReplaceAll]));
                  end;
               end;
            end;
         end;
         str.Add('end');
      end;
    
      str.SaveToStream(aStream);
   finally
      str.Free;
      nodes.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('smd', 'Half-Life SMD files', TGLSMDVectorFile);

end.