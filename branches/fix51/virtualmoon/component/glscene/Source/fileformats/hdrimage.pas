//
// This unit is part of the GLScene Project, http://glscene.org
//
{: HDRImage<p>
    Good for preview picture in OpenDialog, 
    so you may include both HDRImage (preview) and GLFileHDR (loading)

      <li>24/01/10 - Yar - Improved FPC compatibility
      <li>21/01/10 - Yar - Creation 
   </ul></font>
}

unit HDRImage;

interface

{$i GLScene.inc}

uses
  Windows, Classes, SysUtils, GLCrossPlatform, VectorGeometry, GLGraphics,
  OpenGL1x, GLPBuffer;

type

  THDRImage = class (TGLBitmap)
  public
   { Public Declarations }
   procedure LoadFromStream(stream : TStream); override;
   procedure SaveToStream(stream : TStream); override;
	end;

implementation

uses
  {$IFDEF FPC} graphtype, {$ENDIF}
  GLFileHDR, GLTextureFormat;

// ------------------
// ------------------ THDRImage ------------------
// ------------------

// LoadFromStream
//
procedure THDRImage.LoadFromStream(stream : TStream);
var
  FullHDR : TGLHDRImage;
  PBuf : TGLPixelBuffer;
  tempBuff: PGLubyte;
  tempTex : GLuint;
  DC : HDC;
  RC : HGLRC;
  {$IFNDEF FPC}
  src, dst: PGLubyte;
  y: integer;
  {$ELSE}
  RIMG: TRawImage;
  {$ENDIF}
begin
  FullHDR := TGLHDRImage.Create;
  try
    FullHDR.LoadFromStream( stream );
  except
    FullHDR.Free;
    raise;
  end;
  // Copy surface as posible to TBitmap
  DC := wglGetCurrentDC;
  RC := wglGetCurrentContext;

  // Create minimal pixel buffer
  if (DC=0) or (RC=0) then
  begin
    PBuf := TGLPixelBuffer.Create;
    try
      PBuf.Initialize(1, 1);
    except
      FullHDR.Free;
      raise;
    end;
    tempTex := PBuf.TextureID;
  end
  else begin
    Pbuf := nil;
    glPushAttrib(GL_TEXTURE_BIT);
    glGenTextures(1, @tempTex);
  end;
  // Setup texture
  glEnable       ( GL_TEXTURE_2D );
  glBindTexture  ( GL_TEXTURE_2D, tempTex);
  // copy texture to video memory
  glTexImage2D( GL_TEXTURE_2D, 0,
    InternalFormatToOpenGLFormat(FullHDR.InternalFormat), FullHDR.Width,
    FullHDR.Height, 0, FullHDR.ColorFormat, FullHDR.DataType,
    FullHDR.GetLevelData(0));

  CheckOpenGLError;

  GetMem( tempBuff, FullHDR.Width*FullHDR.Height*3 );
  // get texture from video memory in simple format
  glGetTexImage( GL_TEXTURE_2D, 0, GL_BGR, GL_UNSIGNED_BYTE, tempBuff);

  Width       := FullHDR.Width;
  Height      := FullHDR.Height;
  Transparent := false;
  PixelFormat := glpf24bit;

{$IFNDEF FPC}
  src := tempBuff;
  for y := 0 to Height - 1 do
  begin
    dst := ScanLine[Height - 1 - y];
    Move(src^, dst^, Width*3);
    Inc(src, Width*3);
  end;
{$ELSE}
  RIMG.Init;
  rimg.Description.Init_BPP24_B8G8R8_BIO_TTB(Width, Height);
  rimg.Description.RedShift := 16;
  rimg.Description.BlueShift := 0;
  rimg.Description.LineOrder := riloBottomToTop;
  RIMG.DataSize := Width*Height*3;
  rimg.Data := PByte(tempBuff);
  LoadFromRawImage(rimg, false);
{$ENDIF}
  FullHDR.Free;
  FreeMem( tempBuff );

  CheckOpenGLError;
  if Assigned( pBuf ) then
    pBuf.Destroy
  else begin
    glDeleteTextures(1, @tempTex);
    glPopAttrib;
  end;
end;

// SaveToStream
//
procedure THDRImage.SaveToStream(stream : TStream);
begin

end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TGLPicture.RegisterFileFormat(
     'HDR', 'High Dynamic Range Image', THDRImage);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TGLPicture.UnregisterGraphicClass(THDRImage);

end.
