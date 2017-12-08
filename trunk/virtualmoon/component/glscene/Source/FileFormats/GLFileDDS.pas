//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   DDS File support for GLScene.

  History :  
         04/11/10 - DaStr - Added Delphi5 and Delphi6 compatibility 
         23/08/10 - Yar - Replaced OpenGL1x to OpenGLTokens
         06/06/10 - Yar - Fixes for Linux x64
         08/05/10 - Yar - Removed check for residency in AssignFromTexture
         22/04/10 - Yar - Fixes after GLState revision
         01/03/10 - Yar - Added control of texture detail level
         27/01/10 - Yar - Bugfix in BlockOffset with negative result
         23/11/10 - DaStr - Added $I GLScene.inc
         23/01/10 - Yar - Added to AssignFromTexture CurrentFormat parameter
                             Fixed cube map saving bug
         20/01/10 - Yar - Creation
    
}
unit GLFileDDS;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,
   
  GLCrossPlatform, OpenGLTokens, GLContext, GLGraphics, GLTextureFormat,
  GLSRGBE, GLApplicationFileIO, GLVectorGeometry, GLStrings;

type

  // TGLDDSResolutions
  //

  TGLDDSDetailLevels = (ddsHighDet, ddsMediumDet, ddsLowDet);

  // TGLDDSImage
  //

  TGLDDSImage = class(TGLBaseImage)
  private
    procedure flipSurface(chgData: PGLubyte; w, h, d: integer);
  public
    class function Capabilities: TGLDataFileCapabilities; override;

    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;

    { Assigns from any Texture.}
    procedure AssignFromTexture(textureContext: TGLContext;
      const textureHandle: TGLuint;
      textureTarget: TGLTextureTarget;
      const CurrentFormat: Boolean;
      const intFormat: TGLInternalFormat); reintroduce;
  end;

var
  { Variable determines which resolution to use textures,
     high - it loads all levels,
     midle - skipped the first level,
     low - skipped the first two levels. }
  vDDSDetailLevel: TGLDDSDetailLevels = ddsHighDet;

implementation

uses
  DXTC;

// ------------------
// ------------------ TGLDDSImage ------------------
// ------------------

 
//

procedure TGLDDSImage.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  if FileStreamExists(fileName) then
  begin
    fs := CreateFileStream(fileName, fmOpenRead);
    try
      LoadFromStream(fs);
    finally
      fs.Free;
      ResourceName := filename;
    end;
  end
  else
    raise EInvalidRasterFile.CreateFmt(glsFileNotFound, [filename]);
end;

// SaveToFile
//

procedure TGLDDSImage.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenWrite or fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
  ResourceName := filename;
end;

// LoadFromStream
//

procedure TGLDDSImage.LoadFromStream(stream: TStream);
var
  header: TDDSHeader;
  DX10header: TDDS_HEADER_DXT10;
  btcCompressed: Boolean;
  face, faceCount, level: Integer;
  w, h, d, bw, bh, size, offset: Integer;
  bDXT10Header: Boolean;

  procedure CalcSize;
  begin
    if btcCompressed then
    begin
      bw := (w + 3) div 4;
      bh := (h + 3) div 4;
    end
    else
    begin
      bw := w;
      bh := h;
    end;
    if d = 0 then
      d := 1;
    size := bw * bh * d * fElementSize;
  end;

begin
  stream.Read(header, Sizeof(TDDSHeader));
  // DDS files always start with the same magic number ("DDS ")
  if TFOURCC(header.Magic) <> 'DDS ' then
    raise EInvalidRasterFile.Create('Invalid DDS file');

  // Verify header to validate DDS file
  if (header.SurfaceFormat.dwSize <> sizeof(TDDSURFACEDESC2))
    or (header.SurfaceFormat.ddpf.dwSize <> sizeof(TDDPIXELFORMAT)) then
    raise EInvalidRasterFile.Create('Invalid DDS file');

  // Check for DX10 extension
  bDXT10Header := (header.SurfaceFormat.ddpf.dwFlags and DDPF_FOURCC <> 0)
    and (header.SurfaceFormat.ddpf.dwFourCC = FOURCC_DX10);
  if bDXT10Header then
    stream.Read(DX10header, Sizeof(TDDS_HEADER_DXT10));

  with header.SurfaceFormat do
  begin
    { There are flags that are supposed to mark these fields as valid,
       but some dds files don't set them properly }
    UnMipmap;
    FLOD[0].Width := dwWidth;
    FLOD[0].Height := dwHeight;
    // check if image is a volume texture
    if ((dwCaps2 and DDSCAPS2_VOLUME) <> 0) and (dwDepth > 0) then
      FLOD[0].Depth := dwDepth
    else
      FLOD[0].Depth := 0;

    if (dwFlags and DDSD_MIPMAPCOUNT) <> 0 then
      fLevelCount := MaxInteger(dwMipMapCount, 1)
    else
      fLevelCount := 1;

    //check cube-map faces
    fCubeMap := false;
    faceCount := 0;
    if (dwCaps2 and DDSCAPS2_CUBEMAP) <> 0 then
    begin
      //this is a cubemap, count the faces
      if (dwCaps2 and DDSCAPS2_CUBEMAP_POSITIVEX) <> 0 then
        Inc(faceCount);
      if (dwCaps2 and DDSCAPS2_CUBEMAP_NEGATIVEX) <> 0 then
        Inc(faceCount);
      if (dwCaps2 and DDSCAPS2_CUBEMAP_POSITIVEY) <> 0 then
        Inc(faceCount);
      if (dwCaps2 and DDSCAPS2_CUBEMAP_NEGATIVEY) <> 0 then
        Inc(faceCount);
      if (dwCaps2 and DDSCAPS2_CUBEMAP_POSITIVEZ) <> 0 then
        Inc(faceCount);
      if (dwCaps2 and DDSCAPS2_CUBEMAP_NEGATIVEZ) <> 0 then
        Inc(faceCount);
      //check for a complete cubemap
      if (faceCount <> 6) or (GetWidth <> GetHeight) then
        raise EInvalidRasterFile.Create('Invalid cubemap');
      fCubeMap := true;
    end;
    fTextureArray := false;

    if not DDSHeaderToGLEnum(header,
      DX10header,
      bDXT10Header,
      fInternalFormat,
      fColorFormat,
      fDataType,
      fElementSize) then
      raise EInvalidRasterFile.Create('DDS errorneus format');
    btcCompressed := IsCompressedFormat(fInternalFormat);
  end; // of with

  offset := 0;
  case vDDSDetailLevel of
    ddsHighDet: ; // Do nothing..
    ddsMediumDet:
      if fLevelCount > 1 then
      begin
        w := FLOD[0].Width;
        h := FLOD[0].Height;
        d := FLOD[0].Depth;
        CalcSize;
        offset := size;
        FLOD[0].Width := FLOD[0].Width div 2;
        FLOD[0].Height := FLOD[0].Height div 2;
        FLOD[0].Depth := FLOD[0].Depth div 2;
        Dec(fLevelCount);
      end;
    ddsLowDet:
      if fLevelCount > 2 then
      begin
        w := FLOD[0].Width;
        h := FLOD[0].Height;
        d := FLOD[0].Depth;
        CalcSize;
        offset := size;
        Div2(w);
        Div2(h);
        Div2(d);
        CalcSize;
        offset := offset + size;
        FLOD[0].Width := FLOD[0].Width div 4;
        FLOD[0].Height := FLOD[0].Height div 4;
        FLOD[0].Depth := FLOD[0].Depth div 4;
        Dec(fLevelCount, 2);
      end;
  else
    Assert(False, glsErrorEx + glsUnknownType);
  end;

  ReallocMem(fData, DataSize);

  if not fCubeMap then
    faceCount := 1;
  for face := 0 to faceCount - 1 do
  begin
    if offset > 0 then
      stream.Seek(offset, soCurrent);
    for level := 0 to fLevelCount - 1 do
    begin
      stream.Read(GetLevelAddress(level, face)^, GetLevelSizeInByte(level) div faceCount);
      if not fCubeMap and vVerticalFlipDDS then
        flipSurface(GetLevelAddress(level, face), FLOD[level].Width, FLOD[level].Height, FLOD[level].Depth);
    end;
  end; // for level
end;

procedure TGLDDSImage.SaveToStream(stream: TStream);
const
  Magic: array[0..3] of AnsiChar = 'DDS ';
var
  header: TDDSHeader;
  DX10header: TDDS_HEADER_DXT10;
  buffer: PGLubyte;
  level, size: Integer;
begin
  FillChar(header, SizeOf(TDDSHeader), 0);
  header.Magic := Cardinal(Magic);
  header.SurfaceFormat.dwSize := sizeof(TDDSURFACEDESC2);
  header.SurfaceFormat.ddpf.dwSize := sizeof(TDDPIXELFORMAT);
  header.SurfaceFormat.dwWidth := GetWidth;
  header.SurfaceFormat.dwHeight := GetHeight;
  header.SurfaceFormat.dwDepth := GetDepth;
  header.SurfaceFormat.dwPitchOrLinearSize := fElementSize * GetWidth;
  header.SurfaceFormat.dwFlags := DDSD_CAPS or
    DDSD_HEIGHT or
    DDSD_WIDTH or
    DDSD_PIXELFORMAT;
  if IsCompressed then
  begin
    header.SurfaceFormat.dwPitchOrLinearSize :=
      header.SurfaceFormat.dwPitchOrLinearSize * Cardinal(GetHeight) *
      Cardinal(GetDepth);
    header.SurfaceFormat.dwFlags := header.SurfaceFormat.dwFlags or DDSD_PITCH;
  end
  else
    header.SurfaceFormat.dwFlags := header.SurfaceFormat.dwFlags or
      DDSD_LINEARSIZE;

  header.SurfaceFormat.dwCaps := DDSCAPS_TEXTURE;
  header.SurfaceFormat.dwCaps2 := 0;

  if IsVolume then
  begin
    header.SurfaceFormat.dwFlags := header.SurfaceFormat.dwFlags or DDSD_DEPTH;
    header.SurfaceFormat.dwCaps := header.SurfaceFormat.dwCaps or
      DDSCAPS_COMPLEX;
    header.SurfaceFormat.dwCaps2 := header.SurfaceFormat.dwCaps2 or
      DDSCAPS2_VOLUME;
  end;

  if fLevelCount > 1 then
  begin
    header.SurfaceFormat.dwCaps := header.SurfaceFormat.dwCaps or DDSCAPS_COMPLEX
      or DDSCAPS_MIPMAP;
    header.SurfaceFormat.dwFlags := header.SurfaceFormat.dwFlags or
      DDSD_MIPMAPCOUNT;
    header.SurfaceFormat.dwMipMapCount := fLevelCount;
  end
  else
    header.SurfaceFormat.dwMipMapCount := 0;

  if fCubeMap then
  begin
    header.SurfaceFormat.dwCaps := header.SurfaceFormat.dwCaps or
      DDSCAPS_COMPLEX;
    header.SurfaceFormat.dwCaps2 := header.SurfaceFormat.dwCaps2 or
      DDSCAPS2_CUBEMAP or
      DDSCAPS2_CUBEMAP_POSITIVEX or
      DDSCAPS2_CUBEMAP_NEGATIVEX or
      DDSCAPS2_CUBEMAP_POSITIVEY or
      DDSCAPS2_CUBEMAP_NEGATIVEY or
      DDSCAPS2_CUBEMAP_POSITIVEZ or
      DDSCAPS2_CUBEMAP_NEGATIVEZ;
  end;

  if not GLEnumToDDSHeader(header,
    DX10header,
    false,
    fInternalFormat,
    fColorFormat,
    fDataType,
    fElementSize) then
    raise
      EInvalidRasterFile.Create('These image format do not match the DDS format specification.');

  stream.Write(header, Sizeof(TDDSHeader));
  //  stream.Write(DX10header, Sizeof(TDDS_HEADER_DXT10));
  if fCubeMap or not vVerticalFlipDDS then
  begin
    stream.Write(fData[0], DataSize);
    Exit;
  end
  else
  begin
    GetMem(buffer, GetLevelSizeInByte(0));
    try
      for level := 0 to fLevelCount - 1 do
      begin
        size := GetLevelSizeInByte(level);
        Move(GetLevelAddress(level)^, buffer^, size);
        flipSurface(buffer, LevelWidth[level], LevelHeight[level], LevelDepth[level]);
        stream.Write(buffer^, size);
      end;
    finally
      FreeMem(buffer);
    end;
  end;
end;

// AssignFromTexture
//

procedure TGLDDSImage.AssignFromTexture(textureContext: TGLContext;
  const textureHandle: TGLuint;
  textureTarget: TGLTextureTarget;
  const CurrentFormat: Boolean;
  const intFormat: TGLInternalFormat);
var
  oldContext: TGLContext;
  contextActivate: Boolean;
  texFormat, texLod, optLod: Cardinal;
  level, faceCount, face: Integer;
  residentFormat: TGLInternalFormat;
  bCompressed: Boolean;
  vtcBuffer, top, bottom: PGLubyte;
  i, j, k: Integer;
  cw, ch: Integer;
  glTarget: TGLEnum;

  function blockOffset(x, y, z: Integer): Integer;
  begin

    if z >= (FLOD[level].Depth and -4) then
      Result := fElementSize * (cw * ch * (FLOD[level].Depth and -4) + x +
        cw * (y + ch * (z - 4 * ch)))
    else
      Result := fElementSize * (4 * (x + cw * (y + ch * floor(z / 4))) + (z and
        3));
    if Result < 0 then
      Result := 0;
  end;

begin
  oldContext := CurrentGLContext;
  contextActivate := (oldContext <> textureContext);
  if contextActivate then
  begin
    if Assigned(oldContext) then
      oldContext.Deactivate;
    textureContext.Activate;
  end;
  glTarget := DecodeGLTextureTarget(textureTarget);

  try
    textureContext.GLStates.TextureBinding[0, textureTarget] := textureHandle;
    fLevelCount := 0;
    GL.GetTexParameteriv(glTarget, GL_TEXTURE_MAX_LEVEL, @texLod);
    if glTarget = GL_TEXTURE_CUBE_MAP then
    begin
      fCubeMap := true;
      faceCount := 6;
      glTarget := GL_TEXTURE_CUBE_MAP_POSITIVE_X;
    end
    else
    begin
      fCubeMap := false;
      faceCount := 1;
    end;
    fTextureArray := (glTarget = GL_TEXTURE_1D_ARRAY)
      or (glTarget = GL_TEXTURE_2D_ARRAY)
      or (glTarget = GL_TEXTURE_CUBE_MAP_ARRAY);

    repeat
      // Check level existence
      GL.GetTexLevelParameteriv(glTarget, fLevelCount,
        GL_TEXTURE_INTERNAL_FORMAT,
        @texFormat);
      if texFormat = 1 then
        Break;
      Inc(fLevelCount);
      if fLevelCount = 1 then
      begin
        GL.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_WIDTH, @FLOD[0].Width);
        GL.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_HEIGHT, @FLOD[0].Height);
        FLOD[0].Depth := 0;
        if (glTarget = GL_TEXTURE_3D)
          or (glTarget = GL_TEXTURE_2D_ARRAY)
          or (glTarget = GL_TEXTURE_CUBE_MAP_ARRAY) then
          GL.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_DEPTH, @FLOD[0].Depth);
        residentFormat := OpenGLFormatToInternalFormat(texFormat);
        if CurrentFormat then
          fInternalFormat := residentFormat
        else
          fInternalFormat := intFormat;
        if not FindDDSCompatibleDataFormat(fInternalFormat,
          fColorFormat,
          fDataType) then
          FindCompatibleDataFormat(fInternalFormat,
            fColorFormat,
            fDataType);

        // Get optimal number or MipMap levels
        optLod := GetImageLodNumber(FLOD[0].Width, FLOD[0].Height, FLOD[0].Depth, glTarget = GL_TEXTURE_3D);
        if texLod > optLod then
          texLod := optLod;
        // Check for MipMap posibility
        if ((fInternalFormat >= tfFLOAT_R16)
          and (fInternalFormat <= tfFLOAT_RGBA32)) then
          texLod := 1;
      end;
    until fLevelCount = Integer(texLod);

    if fLevelCount > 0 then
    begin
      fElementSize := GetTextureElementSize(fColorFormat, fDataType);
      ReallocMem(FData, DataSize);
      bCompressed := IsCompressed;
      vtcBuffer := nil;

      for face := 0 to faceCount - 1 do
      begin
        if fCubeMap then
          glTarget := face + GL_TEXTURE_CUBE_MAP_POSITIVE_X;
        for level := 0 to fLevelCount - 1 do
        begin
          if bCompressed then
          begin

            if GL.NV_texture_compression_vtc and (FLOD[level].Depth > 0)
              and not fTextureArray then
            begin
              if level = 0 then
                GetMem(vtcBuffer, GetLevelSizeInByte(0));
              GL.GetCompressedTexImage(glTarget, level, vtcBuffer);
              // Shufle blocks from VTC to S3TC
              cw := (FLOD[level].Width + 3) div 4;
              ch := (FLOD[level].Height + 3) div 4;
              top := GetLevelAddress(level);
              for k := 0 to FLOD[level].Depth - 1 do
                for i := 0 to ch - 1 do
                  for j := 0 to cw - 1 do
                  begin
                    bottom := vtcBuffer;
                    Inc(bottom, blockOffset(j, i, k));
                    Move(bottom^, top^, fElementSize);
                    Inc(top, fElementSize);
                  end;
            end
            else
              GL.GetCompressedTexImage(glTarget, level, GetLevelAddress(level));
          end
          else
            GL.GetTexImage(glTarget, level, fColorFormat, fDataType, GetLevelAddress(level));

        end; // for level
      end; // for face
      if Assigned(vtcBuffer) then
        FreeMem(vtcBuffer);
      // Check memory corruption
      ReallocMem(FData, DataSize);
    end;

    if fLevelCount < 1 then
      fLevelCount := 1;
    GL.CheckError;
  finally
    if contextActivate then
    begin
      textureContext.Deactivate;
      if Assigned(oldContext) then
        oldContext.Activate;
    end;
  end;
end;

procedure TGLDDSImage.flipSurface(chgData: PGLubyte; w, h, d: integer);
var
  lineSize: integer;
  sliceSize: integer;
  tempBuf: PGLubyte;
  i, j: integer;
  top, bottom: PGLubyte;
  flipblocks: procedure(data: PGLubyte; size: integer);

begin
  if d = 0 then
    d := 1;

  if not isCompressed then
  begin
    lineSize := fElementSize * w;
    sliceSize := lineSize * h;
    GetMem(tempBuf, lineSize);

    for i := 0 to d - 1 do
    begin
      top := chgData;
      Inc(top, i * sliceSize);
      bottom := top;
      Inc(bottom, sliceSize - lineSize);

      for j := 0 to (h div 2) - 1 do
      begin
        Move(top^, tempBuf^, lineSize);
        Move(bottom^, top^, lineSize);
        Move(tempBuf^, bottom^, lineSize);
        Inc(top, lineSize);
        Dec(bottom, lineSize);
      end;
    end;
    FreeMem(tempBuf);
  end
  else
  begin

    w := (w + 3) div 4;
    h := (h + 3) div 4;

    case fColorFormat of
      GL_COMPRESSED_RGBA_S3TC_DXT1_EXT: flipblocks := flip_blocks_dxtc1;
      GL_COMPRESSED_RGBA_S3TC_DXT3_EXT: flipblocks := flip_blocks_dxtc3;
      GL_COMPRESSED_RGBA_S3TC_DXT5_EXT: flipblocks := flip_blocks_dxtc5;
    else
      exit;
    end;

    lineSize := fElementSize * w;
    sliceSize := lineSize * h;
    GetMem(tempBuf, lineSize);
    for i := 0 to d - 1 do
    begin
      top := chgData;
      Inc(top, i * sliceSize);
      bottom := top;
      Inc(bottom, sliceSize - lineSize);

      for j := 0 to (h div 2) - 1 do
      begin
        if top = bottom then
        begin
          flipblocks(top, w);
          break;
        end;

        flipblocks(top, w);
        flipblocks(bottom, w);

        Move(top^, tempBuf^, lineSize);
        Move(bottom^, top^, lineSize);
        Move(tempBuf^, bottom^, lineSize);

        Inc(top, lineSize);
        Dec(bottom, lineSize);
      end;
    end;
    FreeMem(tempBuf);
  end;
end;

// Capabilities
//

class function TGLDDSImage.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

initialization
  { Register this Fileformat-Handler with GLScene }
  RegisterRasterFormat('dds', 'Direct Draw Surface', TGLDDSImage);

end.

