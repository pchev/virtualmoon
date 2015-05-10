//
// This unit is part of the GLScene Project, http://glscene.org
//
{: O3TCImage<p>
    Good for preview picture in OpenDialog,
    so you may include both O3TCImage (preview) and GLFileO3TC (loading)

      <li>23/10/10 - Yar - Removed PBuffer    
      <li>23/08/10 - Yar - Changes after PBuffer upgrade
      <li>21/03/10 - Yar - Added Linux support
                           (thanks to Rustam Asmandiarov aka Predator)
      <li>24/01/10 - Yar - Improved FPC compatibility
      <li>21/01/10 - Yar - Creation
   </ul></font>
}

unit O3TCImage;

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes,
  SysUtils,
  GLCrossPlatform,
  GLVectorGeometry,
  GLGraphics,
  OpenGLTokens;

type

  TO3TCImage = class(TGLBitmap)
  public
    { Public Declarations }
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
  end;

implementation

uses
{$IFDEF FPC}graphtype,
{$ENDIF}
  GLFileO3TC,
  GLTextureFormat;

// ------------------
// ------------------ TO3TCImage ------------------
// ------------------

// LoadFromStream
//

procedure TO3TCImage.LoadFromStream(stream: TStream);
var
  FullO3TC: TGLO3TCImage;
{$IFNDEF FPC}
  src, dst: PGLubyte;
  y: Integer;
{$ELSE}
  RIMG: TRawImage;
{$ENDIF}
begin
  FullO3TC := TGLO3TCImage.Create;
  try
    FullO3TC.LoadFromStream(stream);
  except
    FullO3TC.Free;
    raise;
  end;

  FullO3TC.Narrow;

  Width := FullO3TC.LevelWidth[0];
  Height := FullO3TC.LevelHeight[0];
  Transparent := true;
  PixelFormat := glpf32bit;

{$IFNDEF FPC}
  src := PGLubyte(FullO3TC.Data);
  for y := 0 to Height - 1 do
  begin
    dst := ScanLine[Height - 1 - y];
    BGRA32ToRGBA32(src, dst, Width);
    Inc(src, Width * 4);
  end;
{$ELSE}
  RIMG.Init;
  rimg.Description.Init_BPP32_B8G8R8A8_BIO_TTB(Width, Height);
  rimg.Description.RedShift := 16;
  rimg.Description.BlueShift := 0;
  rimg.Description.LineOrder := riloBottomToTop;
  RIMG.DataSize := Width * Height * 4;
  rimg.Data := PByte(FullO3TC.Data);
  LoadFromRawImage(rimg, false);
{$ENDIF}
  FullO3TC.Free;
end;

// SaveToStream
//

procedure TO3TCImage.SaveToStream(stream: TStream);
begin
  Assert(False, 'Not supported');
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

