//
// This unit is part of the GLScene Project, http://glscene.org
//
{: DDSImage<p>
    Alternative for DDS unit with more supported formats of flat image:
    Alpha8, Luminance8, R3G3B2, RGB5A1, RGBA4, Alpha8Luminance8, Luminance16, R5G6B5, 
    RGB8, R10G10B10A2, RGBA8, RGBA16, R16F, RGBA16F, R32F, RGBA32F, GR16, GR16F, GR32F, 
    Compressed RGB S3TC DXT1, Compressed RGBA S3TC DXT1, Compressed RGBA S3TC DXT3, 
    Compressed RGBA S3TC DXT5
    But it down color to RGBA8 because becomes to TGLBitmap
    Good for preview picture in OpenDialog, 
    so you may include both DDSImage (preview) and GLFileDDS (loading)

 <b>History : </b><font size=-1><ul>
        <li>24/01/10 - Yar - Improved FPC compatibility
        <li>21/01/10 - Yar - Creation
   </ul><p>
}

unit DDSImage;

interface

{$i GLScene.inc}

uses
  {$IFDEF MSWINDOWS} Windows, GLPBuffer, {$ENDIF}
  Classes, SysUtils, GLCrossPlatform, VectorGeometry, GLGraphics,
  OpenGL1x;

type

  TDDSImage = class(TGLBitmap)
  public
    { Public Declarations }
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
  end;

  EDDSException = class(Exception);

implementation

uses
  {$IFDEF FPC} graphtype, {$ENDIF}
  DXTC, GLFileDDS, GLTextureFormat;

// ------------------
// ------------------ TDDSImage ------------------
// ------------------

// LoadFromStream

procedure TDDSImage.LoadFromStream(stream: TStream);
var
  FullDDS: TGLDDSImage;
  PBuf: TGLPixelBuffer;
  size: integer;
  tempBuff: PGLubyte;
  tempTex: GLuint;
  DC: HDC;
  RC: HGLRC;
  {$IFNDEF FPC}
  src, dst: PGLubyte;
  y: integer;
  {$ELSE}
  RIMG: TRawImage;
  {$ENDIF}
begin
  FullDDS := TGLDDSImage.Create;
  try
    FullDDS.LoadFromStream(stream);
  except
    FullDDS.Free;
    raise;
  end;

  // Copy surface as posible to TBitmap
  DC := wglGetCurrentDC;
  RC := wglGetCurrentContext;

  // Create minimal pixel buffer
  if (DC = 0) or (RC = 0) then
  begin
    PBuf := TGLPixelBuffer.Create;
    try
      PBuf.Initialize(1, 1);
    except
      FullDDS.Free;
      raise;
    end;
    tempTex := PBuf.TextureID;
  end
  else
  begin
    Pbuf := nil;
    glPushAttrib(GL_TEXTURE_BIT);
    glGenTextures(1, @tempTex);
  end;

  if IsFormatSupported(FullDDS.InternalFormat) then
  begin
    // Setup texture
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, tempTex);
    // copy texture to video memory
    if FullDDS.isCompressed then
    begin
      size := ((FullDDS.Width + 3) div 4) * ((FullDDS.Height + 3) div 4) *
        FullDDS.ElementSize;
      glCompressedTexImage2DARB(GL_TEXTURE_2D, 0,
        InternalFormatToOpenGLFormat(FullDDS.InternalFormat),
        FullDDS.Width, FullDDS.Height, 0, size,
        FullDDS.GetLevelData(0));
    end
    else
      glTexImage2D(GL_TEXTURE_2D, 0,
        InternalFormatToOpenGLFormat(FullDDS.InternalFormat), FullDDS.Width,
        FullDDS.Height, 0, FullDDS.ColorFormat, FullDDS.DataType,
        FullDDS.GetLevelData(0));

    CheckOpenGLError;

    GetMem(tempBuff, FullDDS.Width * FullDDS.Height * 4);
    // get texture from video memory in simple format
    glGetTexImage(GL_TEXTURE_2D, 0, GL_BGRA, GL_UNSIGNED_BYTE, tempBuff);
    PixelFormat := glpf32bit;
    Transparent := FullDDS.Transparent;
    Width := FullDDS.Width;
    Height := FullDDS.Height;

{$IFNDEF FPC}
    src := tempBuff;
    if FullDDS.CubeMap then
      for y := 0 to Height - 1 do
      begin
        dst := ScanLine[y];
        Move(src^, dst^, Width * 4);
        Inc(src, Width * 4);
      end
    else
      for y := 0 to Height - 1 do
      begin
        dst := ScanLine[Height - 1 - y];
        Move(src^, dst^, Width * 4);
        Inc(src, Width * 4);
      end;
{$ELSE}
    RIMG.Init;
    rimg.Description.Init_BPP32_B8G8R8A8_BIO_TTB(Width, Height);
    rimg.Description.RedShift := 16;
    rimg.Description.BlueShift := 0;
    rimg.Description.LineOrder := riloBottomToTop;
    RIMG.DataSize := Width*Height*4;
    rimg.Data := PByte(tempBuff);
    LoadFromRawImage(rimg, false);
{$ENDIF}
    FullDDS.Free;
    FreeMem(tempBuff);

    CheckOpenGLError;
  end;
  if Assigned(pBuf) then
    pBuf.Destroy
  else begin
    glDeleteTextures(1, @tempTex);
    glPopAttrib;
  end;
end;

// SaveToStream

procedure TDDSImage.SaveToStream(stream: TStream);
const
  Magic: array[0..3] of AnsiChar = 'DDS ';
var
  header: TDDSHeader;
  i, rowSize: integer;
begin
  FillChar(header, SizeOf(TDDSHeader), 0);
  header.magic := cardinal(Magic);
  with header.SurfaceFormat do
  begin
    dwSize := 124;
    dwFlags := DDSD_CAPS + DDSD_PIXELFORMAT + DDSD_WIDTH + DDSD_HEIGHT + DDSD_PITCH;
    dwWidth := Width;
    dwHeight := Height;
    case PixelFormat of
         {$IFDEF MSWINDOWS}
      glpf24bit:
      begin
        ddpf.dwFlags := DDPF_RGB;
        ddpf.dwRGBBitCount := 24;
        ddpf.dwRBitMask := $00FF0000;
        ddpf.dwGBitMask := $0000FF00;
        ddpf.dwBBitMask := $000000FF;
      end;
         {$ENDIF}
      glpf32bit:
      begin
        ddpf.dwFlags := DDPF_RGB;
        ddpf.dwRGBBitCount := 32;
        ddpf.dwRBitMask := $00FF0000;
        ddpf.dwGBitMask := $0000FF00;
        ddpf.dwBBitMask := $000000FF;
        if Transparent then
        begin
          ddpf.dwFlags := ddpf.dwFlags + DDPF_ALPHAPIXELS;
          ddpf.dwRGBAlphaBitMask := $FF000000;
        end;
      end;
      else
        raise EDDSException.Create('Unsupported pixel format format');
    end;
    rowSize := (ddpf.dwRGBBitCount div 8) * dwWidth;
    dwPitchOrLinearSize := dwHeight * cardinal(rowSize);
    dwCaps := DDSCAPS_TEXTURE;
    stream.Write(header, SizeOf(TDDSHeader));
{$IFNDEF FPC}
    for i := 0 to Height - 1 do
      stream.Write(ScanLine[i]^, rowSize);
{$ELSE}
      stream.Write(RawImage.Data^, Width*Height*header.SurfaceFormat.ddpf.dwRGBBitCount div 4);
{$ENDIF}
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  TGLPicture.RegisterFileFormat(
    'dds', 'Microsoft DirectDraw Surface', TDDSImage);

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
finalization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  TGLPicture.UnregisterGraphicClass(TDDSImage);

end.

