//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFileDDS<p>

 <b>History : </b><font size=-1><ul>
        <li>01/03/10 - Yar - Added control of texture detail level
        <li>27/01/10 - Yar - Bugfix in BlockOffset with negative result
        <li>23/11/10 - DaStr - Added $I GLScene.inc
        <li>23/01/10 - Yar - Added to AssignFromTexture CurrentFormat parameter
                             Fixed cube map saving bug
        <li>20/01/10 - Yar - Creation
   </ul><p>
}
unit GLFileDDS;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils,
  OpenGL1x, GLContext, GLGraphics, GLTextureFormat, RGBE,
  ApplicationFileIO;

type

  // TGLDDSResolutions
  //

  TGLDDSDetailLevels = (ddsHighDet, ddsMediumDet, ddsLowDet);

  // TGLDDSImage
  //

  TGLDDSImage = class(TGLBaseImage)
  private
    fTransparent: Boolean;
    procedure flipSurface(chgData: PGLubyte; w, h, d: integer);
  public
    class function Capabilities: TDataFileCapabilities; override;

    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;

    {: Assigns from any Texture.}
    procedure AssignFromTexture(textureContext: TGLContext;
      const textureHandle: TGLenum;
      textureTarget: TGLenum;
      const CurrentFormat: Boolean;
      const intFormat: TGLInternalFormat); override;

    property Data: PGLPixel32Array read FData;
    property Width: Integer read fWidth;
    property Height: Integer read fHeight;
    property Depth: Integer read fDepth;
    property MipLevels: Integer read fMipLevels;
    property ColorFormat: GLenum read fColorFormat;
    property InternalFormat: TGLInternalFormat read fInternalFormat;
    property DataType: GLenum read fDataType;
    property ElementSize: Integer read fElementSize;
    property CubeMap: Boolean read fCubeMap;
    property TextureArray: Boolean read fTextureArray;
    property Transparent: Boolean read fTransparent;
  end;

var
  {: Variable determines which resolution to use textures,
     high - it loads all levels,
     midle - skipped the first level,
     low - skipped the first two levels. }
  vDDSDetailLevel: TGLDDSDetailLevels = ddsHighDet;

implementation

uses
  DXTC, VectorGeometry, GLStrings;

// ------------------
// ------------------ TGLDDSImage ------------------
// ------------------

// LoadFromFile
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
    raise EInvalidRasterFile.CreateFmt('File %s not found', [filename]);
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
  lData: PGLubyte;
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

  procedure DownSizeBy2;
  begin
    if w > 1 then
      w := w div 2
    else
      w := 1;
    if h > 1 then
      h := h div 2
    else
      h := 1;
    if d > 1 then
      d := d div 2
    else
      d := 1;
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
    {: There are flags that are supposed to mark these fields as valid,
       but some dds files don't set them properly }
    fWidth := dwWidth;
    fHeight := dwHeight;
    // check if image is a volume texture
    if ((dwCaps2 and DDSCAPS2_VOLUME) <> 0) and (dwDepth > 0) then
      fDepth := dwDepth
    else
      fDepth := 0;

    // check alpha flag
    fTransparent := (dwFlags and DDPF_ALPHAPIXELS) <> 0;

    if (dwFlags and DDSD_MIPMAPCOUNT) <> 0 then
      fMipLevels := dwMipMapCount
    else
      fMipLevels := 1;

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
      if (faceCount <> 6) or (Width <> Height) then
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
      if fMipLevels > 1 then
      begin
        w := fWidth;
        h := fHeight;
        d := fDepth;
        CalcSize;
        offset := size;
        fWidth := fWidth div 2;
        fHeight := fHeight div 2;
        fDepth := fDepth div 2;
        Dec(fMipLevels);
      end;
    ddsLowDet:
      if fMipLevels > 2 then
      begin
        w := fWidth;
        h := fHeight;
        d := fDepth;
        CalcSize;
        offset := size;
        DownSizeBy2;
        CalcSize;
        offset := offset + size;
        fWidth := fWidth div 4;
        fHeight := fHeight div 4;
        fDepth := fDepth div 4;
        Dec(fMipLevels, 2);
      end;
  else
    Assert(False, glsErrorEx + glsUnknownType);
  end;

  ReallocMem(fData, DataSize);
  lData := PGLubyte(fData);
  fLevels.Clear;

  if not fCubeMap then
    faceCount := 1;
  for face := 0 to faceCount - 1 do
  begin
    w := Width;
    h := Height;
    d := Depth;
    if offset > 0 then
      stream.Seek(offset, soCurrent);
    for level := 0 to MipLevels - 1 do
    begin
      fLevels.Add(pointer(integer(lData) - integer(fData)));
      CalcSize;
      stream.Read(lData^, size);
      if not fCubeMap and vVerticalFlipDDS then
        flipSurface(lData, w, h, d);
      DownSizeBy2;
      Inc(lData, size);
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
  w, h, d, level, size: Integer;
begin
  FillChar(header, SizeOf(TDDSHeader), 0);
  header.Magic := Cardinal(Magic);
  header.SurfaceFormat.dwSize := sizeof(TDDSURFACEDESC2);
  header.SurfaceFormat.ddpf.dwSize := sizeof(TDDPIXELFORMAT);
  header.SurfaceFormat.dwWidth := fWidth;
  header.SurfaceFormat.dwHeight := fHeight;
  header.SurfaceFormat.dwDepth := fDepth;
  header.SurfaceFormat.dwPitchOrLinearSize := fElementSize * fWidth;
  header.SurfaceFormat.dwFlags := DDSD_CAPS or
    DDSD_HEIGHT or
    DDSD_WIDTH or
    DDSD_PIXELFORMAT;
  if IsCompressed then
  begin
    header.SurfaceFormat.dwPitchOrLinearSize :=
      header.SurfaceFormat.dwPitchOrLinearSize * Cardinal(fHeight) *
      Cardinal(fDepth);
    header.SurfaceFormat.dwFlags := header.SurfaceFormat.dwFlags or DDSD_PITCH;
  end
  else
    header.SurfaceFormat.dwFlags := header.SurfaceFormat.dwFlags or
      DDSD_LINEARSIZE;

  header.SurfaceFormat.dwCaps := DDSCAPS_TEXTURE;
  header.SurfaceFormat.dwCaps2 := 0;

  if fDepth > 0 then
  begin
    header.SurfaceFormat.dwFlags := header.SurfaceFormat.dwFlags or DDSD_DEPTH;
    header.SurfaceFormat.dwCaps := header.SurfaceFormat.dwCaps or
      DDSCAPS_COMPLEX;
    header.SurfaceFormat.dwCaps2 := header.SurfaceFormat.dwCaps2 or
      DDSCAPS2_VOLUME;
  end;

  if fMipLevels > 1 then
  begin
    header.SurfaceFormat.dwCaps := header.SurfaceFormat.dwCaps or DDSCAPS_COMPLEX
      or DDSCAPS_MIPMAP;
    header.SurfaceFormat.dwFlags := header.SurfaceFormat.dwFlags or
      DDSD_MIPMAPCOUNT;
    header.SurfaceFormat.dwMipMapCount := fMipLevels;
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
    GetMem(buffer, LevelSize(0));
    w := fWidth;
    h := fHeight;
    d := fDepth;
    if d = 0 then
      d := 1;
    try
      for level := 0 to fMipLevels - 1 do
      begin
        size := LevelSize(level);
        Move(GetLevelData(level)[0], buffer^, size);
        flipSurface(buffer, w, h, d);
        stream.Write(buffer^, size);
        if w > 1 then
          w := w div 2
        else
          w := 1;
        if h > 1 then
          h := h div 2
        else
          h := 1;
        if d > 1 then
          d := d div 2
        else
          d := 1;
      end;
    finally
      FreeMem(buffer);
    end;
  end;
end;

// AssignFromTexture
//

procedure TGLDDSImage.AssignFromTexture(textureContext: TGLContext;
  const textureHandle: TGLenum;
  textureTarget: TGLenum;
  const CurrentFormat: Boolean;
  const intFormat: TGLInternalFormat);
var
  oldContext: TGLContext;
  contextActivate: Boolean;
  texFormat, texLod, texResident, optLod: Cardinal;
  level, faceCount, face: Integer;
  lData: PGLubyte;
  residentFormat: TGLInternalFormat;
  bCompressed: Boolean;
  vtcBuffer, top, bottom: PGLubyte;
  i, j, k: Integer;
  w, d, h, cw, ch: Integer;

  function blockOffset(x, y, z: Integer): Integer;
  begin

    if z >= (d and -4) then
      Result := fElementSize * (cw * ch * (d and -4) + x +
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

  try
    textureContext.GLStates.SetGLCurrentTexture(0, textureTarget, textureHandle);
    //Check for texture is resident in texture memory
    glGetTexParameteriv(textureTarget, GL_TEXTURE_RESIDENT, @texResident);
    fMipLevels := 0;
    if texResident = GL_TRUE then
    begin
      glGetTexParameteriv(textureTarget, GL_TEXTURE_MAX_LEVEL, @texLod);
      if textureTarget = GL_TEXTURE_CUBE_MAP then
      begin
        fCubeMap := true;
        faceCount := 6;
        textureTarget := GL_TEXTURE_CUBE_MAP_POSITIVE_X;
      end
      else
      begin
        fCubeMap := false;
        faceCount := 1;
      end;
      fTextureArray := (textureTarget = GL_TEXTURE_1D_ARRAY)
        or (textureTarget = GL_TEXTURE_2D_ARRAY)
        or (textureTarget = GL_TEXTURE_CUBE_MAP_ARRAY);

      repeat
        // Check level existence
        glGetTexLevelParameteriv(textureTarget, fMipLevels,
          GL_TEXTURE_INTERNAL_FORMAT,
          @texFormat);
        if texFormat = 1 then
          Break;
        Inc(fMipLevels);
        if fMipLevels = 1 then
        begin
          glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_WIDTH, @fWidth);
          glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_HEIGHT,
            @fHeight);
          fDepth := 0;
          if (textureTarget = GL_TEXTURE_3D)
            or (textureTarget = GL_TEXTURE_2D_ARRAY)
            or (textureTarget = GL_TEXTURE_CUBE_MAP_ARRAY) then
            glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_DEPTH,
              @fDepth);
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
          optLod := GetImageLodNumber(fWidth, fHeight, fDepth);
          if texLod > optLod then
            texLod := optLod;
          // Check for MipMap posibility
          if ((fInternalFormat >= tfFLOAT_R16)
            and (fInternalFormat <= tfFLOAT_RGBA32)) then
            texLod := 1;
        end;
      until fMipLevels = Integer(texLod);

      if fMipLevels > 0 then
      begin
        fElementSize := GetTextureElementSize(fColorFormat, fDataType);
        ReallocMem(FData, DataSize);
        fLevels.Clear;
        lData := PGLubyte(fData);
        bCompressed := IsCompressed;
        vtcBuffer := nil;
        w := fWidth;
        h := fHeight;
        d := fDepth;

        for face := 0 to faceCount - 1 do
        begin
          if fCubeMap then
            textureTarget := face + GL_TEXTURE_CUBE_MAP_POSITIVE_X;
          for level := 0 to fMipLevels - 1 do
          begin
            fLevels.Add(Pointer(Integer(lData) - Integer(fData)));
            if bCompressed then
            begin

              if GL_NV_texture_compression_vtc and (d > 0) and not fTextureArray
                then
              begin
                if level = 0 then
                  GetMem(vtcBuffer, LevelSize(0));
                glGetCompressedTexImage(textureTarget, level, vtcBuffer);
                // Shufle blocks from VTC to S3TC
                cw := (w + 3) div 4;
                ch := (h + 3) div 4;
                top := lData;
                for k := 0 to d - 1 do
                  for i := 0 to ch - 1 do
                    for j := 0 to cw - 1 do
                    begin
                      bottom := vtcBuffer;
                      Inc(bottom, blockOffset(j, i, k));
                      Move(bottom^, top^, fElementSize);
                      Inc(top, fElementSize);
                    end;
                if w > 1 then
                  w := w div 2
                else
                  w := 1;
                if h > 1 then
                  h := h div 2
                else
                  h := 1;
                if d > 1 then
                  d := d div 2
                else
                  d := 1;
              end
              else
                glGetCompressedTexImage(textureTarget, level, lData);
            end
            else
              glGetTexImage(textureTarget, level, fColorFormat, fDataType,
                lData);

            Inc(lData, LevelSize(level));
          end; // for level
        end; // for face
        if Assigned(vtcBuffer) then
          FreeMem(vtcBuffer);
        // Check memory corruption
        ReallocMem(FData, DataSize);
      end;
    end;
    if fMipLevels = 0 then
      fMipLevels := 1;
    CheckOpenGLError;
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

class function TGLDDSImage.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

initialization
  { Register this Fileformat-Handler with GLScene }
  RegisterRasterFormat('dds', 'Direct Draw Surface', TGLDDSImage);

end.

