//
// This unit is part of the GLScene Project, http://glscene.org
//
{   TIN (Triangular Irregular Network) vector file format implementation.
   History : 
       08/04/13 - PW - Fixed a bug in VertArr: decremented element numbers by one
       17/11/05 - PW - Added support for ascii TIN files with materials
       05/06/03 - SG - Separated from GLVectorFileObjects.pas
	 
}
unit GLFileTIN;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,
  GLVectorFileObjects, GLApplicationFileIO;

type
   // TGLTINVectorFile
   //
   { The TIN vector file (triangle irregular network).
      It is a simple text format, with one triangle record per line, no materials,
      no texturing (there may be more, but I never saw anything in this files).
      This format is encountered in the DEM/DTED world and used in place of grids. }
   TGLTINVectorFile = class(TGLVectorFile)
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

uses
  GLVectorGeometry, GLUtils, GLTypes;
// ------------------
// ------------------ TGLTINVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLTINVectorFile.Capabilities : TGLDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

// LoadFromStream
//
procedure TGLTINVectorFile.LoadFromStream(aStream : TStream);
var
   i, j : Integer;
   sl, tl : TStringList;
   mesh : TGLMeshObject;
   v1, v2, v3, n : TAffineVector;
   ActiveTin : Boolean;
   Id_Tin : Integer;
   Tnam, S : string;
   Id_Mat, NVert, NTri : Integer;

   VertArr :  TGLPoint3DArray;
   n1, n2, n3 : Integer;


begin
  sl := TStringList.Create;
  tl := TStringList.Create;
  try
    sl.LoadFromStream(aStream);
    mesh      := TGLMeshObject.CreateOwned(Owner.MeshObjects);
    mesh.Mode := momTriangles;
    if sl[0]<>'TIN' then    // the file with single TIN described by vertices only
    begin
      for i := 0 to sl.Count - 1 do
      begin
        tl.CommaText := Copy(sl[i], 0, MaxInt);
        Trim(tl.CommaText);
        if tl.Count = 9 then
        begin
            SetVector(v1, GLUtils.StrToFloatDef(tl[0],0), GLUtils.StrToFloatDef(tl[1],0), GLUtils.StrToFloatDef(tl[2],0));
            SetVector(v2, GLUtils.StrToFloatDef(tl[3],0), GLUtils.StrToFloatDef(tl[4],0), GLUtils.StrToFloatDef(tl[5],0));
            SetVector(v3, GLUtils.StrToFloatDef(tl[6],0), GLUtils.StrToFloatDef(tl[7],0), GLUtils.StrToFloatDef(tl[8],0));
            mesh.Vertices.Add(v1, v2, v3);
            n := CalcPlaneNormal(v1, v2, v3);
            mesh.Normals.Add(n, n, n);
        end;
      end
    end
    else  // the file with multiple TINs described by triangles and materials
    while i < sl.Count - 1  do
    begin
      Inc(i);
      tl.DelimitedText := sl[i];
      if (tl.CommaText = 'BEGT') then // the beginning of new tin
      begin
        repeat
          Inc(i); tl.DelimitedText := sl[i];
          if (tl[0] = 'ACTIVETIN') then
            ActiveTin := True;
          if (tl[0] = 'ID') then
            Id_Tin := StrToInt(tl[1]);
          if (tl[0] = 'TNAM') then
            Tnam := tl[1];
          if (tl[0] = 'MAT') then
            Id_Mat := StrToInt(tl[1]);
          if (tl[0] = 'VERT') then
            NVert := StrToInt(tl[1]);
        until tl[0]='VERT';
        SetLength(VertArr, NVert);
        j := 0;
        repeat
          Inc(i);
          tl.DelimitedText := sl[i];
          VertArr[j].X := StrToFloat(tl[0]);
          VertArr[j].Y := StrToFloat(tl[1]);
          VertArr[j].Z := StrToFloat(tl[2]);
          Inc(j);
        until (j = NVert);
        Inc(i);  tl.DelimitedText := sl[i];
        NTri := StrToInt(tl[1]);
        j := 0;
        repeat
          Inc(i); Inc(j);
          tl.DelimitedText := sl[i];
          n1 := StrToInt(tl[0]); n2 := StrToInt(tl[1]); n3 := StrToInt(tl[2]);
          SetVector(v1, VertArr[n1-1].X, VertArr[n1-1].Y, VertArr[n1-1].Z);
          SetVector(v2, VertArr[n2-1].X, VertArr[n2-1].Y, VertArr[n2-1].Z);
          SetVector(v3, VertArr[n3-1].X, VertArr[n3-1].Y, VertArr[n3-1].Z);
          mesh.Vertices.Add(v1, v2, v3);
          n := CalcPlaneNormal(v1, v2, v3);
          mesh.Normals.Add(n, n, n);
        until (j = NTri);
        Inc(i); tl.DelimitedText := sl[i]; //tl.DelimitedText = 'ENDT';
      end;
    end;
  finally
    tl.Free;
    sl.Free;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('tin', 'Triangular Irregular Network', TGLTINVectorFile);

end.