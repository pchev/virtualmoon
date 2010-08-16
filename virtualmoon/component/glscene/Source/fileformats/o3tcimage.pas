//
// This unit is part of the GLScene Project, http://glscene.org
//
{: O3TCImage<p>
    Good for preview picture in OpenDialog, 
    so you may include both O3TCImage (preview) and GLFileO3TC (loading)

      <li>24/01/10 - Yar - Improved FPC compatibility
      <li>21/01/10 - Yar - Creation 
   </ul></font>
}

unit O3TCImage;

interface

{$i GLScene.inc}

uses
  Windows, Classes, SysUtils, GLCrossPlatform, VectorGeometry, GLGraphics,
  OpenGL1x, GLPBuffer;

type

  TO3TCImage = class (TGLBitmap)
  public
   { Public Declarations }
   procedure LoadFromStream(stream : TStream); override;
   procedure SaveToStream(stream : TStream); override;
	end;

implementation

uses
  {$IFDEF FPC} graphtype, {$ENDIF}
  GLFileO3TC, GLTextureFormat;

// ------------------
// ------------------ TO3TCImage ------------------
// ------------------

// LoadFromStream
//
procedure TO3TCImage.LoadFromStream(stream : TStream);
var
  FullO3TC : TGLO3TCImage;
  PBuf : TGLPixelBuffer;
  size: integer;
  tempBuff: PGLubyte;
  tempTex : GLuint;
  DC : HDC;
  RC : HGLRC;
  {$IFNDEF FPC}
  src, dst: PGLubyte;
  y: Integer;
  {$ELSE}
  RIMG: TRawImage;
  {$ENDIF}
begin
  FullO3TC := TGLO3TCImage.Create;
  try
    FullO3TC.LoadFromStream( stream );
  except
    FullO3TC.Free;
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
      FullO3TC.Free;
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
  size := ((FullO3TC.Width + 3) div 4)
        * ((FullO3TC.Height + 3) div 4)
        * FullO3TC.ElementSize;
  glCompressedTexImage2DARB( GL_TEXTURE_2D, 0,
    InternalFormatToOpenGLFormat(FullO3TC.InternalFormat),
    FullO3TC.Width, FullO3TC.Height, 0, size,
    FullO3TC.GetLevelData(0));

  CheckOpenGLError;

  GetMem( tempBuff, FullO3TC.Width*FullO3TC.Height*4 );
  // get texture from video memory in simple format
  glGetTexImage( GL_TEXTURE_2D, 0, GL_BGRA, GL_UNSIGNED_BYTE, tempBuff);

  Width       := FullO3TC.Width;
  Height      := FullO3TC.Height;
  Transparent := true;
  PixelFormat := glpf32bit;


{$IFNDEF FPC}
  src := tempBuff;
  for y := 0 to Height - 1 do begin
    dst := ScanLine[Height - 1 - y];
    Move(src^, dst^, Width*4);
    Inc(src, Width*4);
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
  FullO3TC.Free;
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
procedure TO3TCImage.SaveToStream(stream : TStream);
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
     'o3tc', 'oZone3D Texture Compression', TO3TCImage);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TGLPicture.UnregisterGraphicClass(TO3TCImage);

end.
