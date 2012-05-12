//
// This unit is part of the GLScene Project, http://glscene.org
//
{: HDRImage<p>
    Good for preview picture in OpenDialog,
    so you may include both HDRImage (preview) and GLFileHDR (loading)

      <li>23/10/10 - Yar - Removed PBuffer    
      <li>23/08/10 - Yar - Changes after PBuffer upgrade
      <li>21/03/10 - Yar - Added Linux support
                           (thanks to Rustam Asmandiarov aka Predator)
      <li>24/01/10 - Yar - Improved FPC compatibility
      <li>21/01/10 - Yar - Creation
   </ul></font>
}

unit HDRImage;

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS}Windows,
{$ENDIF}Classes,
  SysUtils,
  GLCrossPlatform,
  VectorGeometry,
  GLGraphics,
  OpenGLTokens;

type

  THDRImage = class(TGLBitmap)
  public
    { Public Declarations }
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
  end;

implementation

uses
{$IFDEF FPC}graphtype,
{$ENDIF}
  GLFileHDR,
  GLTextureFormat;

// ------------------
// ------------------ THDRImage ------------------
// ------------------

// LoadFromStream
//

procedure THDRImage.LoadFromStream(stream: TStream);
var
  FullHDR: TGLHDRImage;
{$IFNDEF FPC}
  src, dst: PGLubyte;
  y: integer;
{$ELSE}
  RIMG: TRawImage;
{$ENDIF}
begin
  FullHDR := TGLHDRImage.Create;
  try
    FullHDR.LoadFromStream(stream);
  except
    FullHDR.Free;
    raise;
  end;

  FullHDR.Narrow;

  Width := FullHDR.LevelWidth[0];
  Height := FullHDR.LevelHeight[0];
  Transparent := false;
  PixelFormat := glpf32bit;

{$IFNDEF FPC}
  src := PGLubyte(FullHDR.Data);
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
  RIMG.DataSize := Width * Height * 4;
  rimg.Data := PByte(FullHDR.Data);
  LoadFromRawImage(rimg, false);
{$ENDIF}
  FullHDR.Free;
end;

// SaveToStream
//

procedure THDRImage.SaveToStream(stream: TStream);
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

