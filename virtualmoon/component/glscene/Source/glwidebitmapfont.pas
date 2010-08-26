//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLWideBitmapFont<p>

  TFont Import into a BitmapFont using variable width... With Wide Support... <p>
  Windows only!

  JAJ: Credits to the UniCode Version of SynEdit! I located the needed API calls from them. GPL/MPL as GLScene

	<b>History : </b><font size=-1><ul>
      <li>24/03/07 - DaStr - Got rid of Types dependancy
                             Removed unused variables
      <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTrackerID=1681585)
      <li>18/01/07 - JAJ - Made into a standalone unit...
	</ul></font>
}
unit GLWideBitmapFont;

interface

{$include GLScene.inc}

uses
  GLBitmapFont, Classes, GLScene, GLTexture, GLCrossPlatform, VectorGeometry,
  {$IFDEF MSWINDOWS}
  Windows,
  Graphics
  {$ENDIF}
  {$IFDEF UNIX}
  QGraphics
  {$ENDIF}
  ;

type

   // TGLWideBitmapFont
   //
   {: A bitmap font automatically built from a TFont.<p>
      It works like a TGLBitmapfont, you set ranges and which chars are assigned
      to which indexes, however here you also set the Font property to any TFont
      available to the system and it renders in GLScene as close to that font
      as posible, on some font types this is 100% on some a slight difference
      in spacing can occur at most 1 pixel per char on some char combinations.<p>
      Ranges must be sorted in ascending ASCII order and should not overlap.
      As the font texture is automatically layed out, the Ranges StartGlyphIdx
      property is ignored and replaced appropriately. }
   TGLWideBitmapFont = class (TGLCustomBitmapFont)
      private
	      { Private Declarations }
         FFont : TFont;
        FWideCharacters: WideString;
        FWideChars : Array of WideChar; // array of Widechars stored in string for multibyte support.
        procedure SetWideCharacters(const Value: WideString);

      protected
	      { Protected Declarations }
         procedure SetFont(value : TFont);
         procedure LoadWideBitmapFont; virtual;

         procedure PrepareImage; override;
         function TextureFormat : Integer; override;

      public
	      { Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor  Destroy; override;

         procedure NotifyChange(Sender : TObject); override;
         Function Encode(const AWideString : WideString) : String; overload;
         Function Encode(const AWideChar : WideChar) : Char; overload;

         function FontTextureWidth : Integer;
         function FontTextureHeight : Integer;

         property Glyphs;

      published
	      { Published Declarations }
         {: The font used to prepare the texture.<p>
            Note: the font color is ignored. }
         property Font : TFont read FFont write SetFont;

			property MagFilter;
			property MinFilter;
      property WideCharacters : WideString read FWideCharacters write SetWideCharacters;
   end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x;

Var
  Win32PlatformIsUnicode : Boolean;

// ------------------
// ------------------ TGLWideBitmapFont ------------------
// ------------------

// Create
//
constructor TGLWideBitmapFont.Create(AOwner: TComponent);
begin
   inherited;

   FFont:=TFont.Create;
   FFont.Color:=clWhite;
   FFont.OnChange:=NotifyChange;
   GlyphsAlpha:=tiaAlphaFromIntensity;
end;

// Destroy
//
destructor TGLWideBitmapFont.Destroy;
begin
   FFont.Free;
   inherited;
end;

// FontTextureWidth
//
function TGLWideBitmapFont.FontTextureWidth : Integer;
begin
   Result:=Glyphs.Width;
end;

// FontTextureHeight
//
function TGLWideBitmapFont.FontTextureHeight : Integer;
begin
   Result:=Glyphs.Height;
end;

// SetFont
//
procedure TGLWideBitmapFont.SetFont(value: TFont);
begin
   FFont.Assign(value);
end;

// NotifyChange
//
procedure TGLWideBitmapFont.NotifyChange(Sender : TObject);
begin
   FreeTextureHandle;
   InvalidateUsers;
   inherited;
end;

// LoadWideBitmapFont
//
procedure TGLWideBitmapFont.LoadWideBitmapFont;
var
   textureWidth, textureHeight : Integer;

   function ComputeCharRects(x, y : Integer; canvas : TCanvas) : Integer;
   var
      px, py, cw, n : Integer;
      rect : TGLRect;
      buffer : array[0..2] of WideChar;
   begin
      buffer[1] := WideChar(#32);
      buffer[2] := WideChar(#0);
      Result:=0;
      n:=0;
      px:=0;
      py:=0;

      while n<256 do begin
         cw:=CharWidths[n];
         if cw>0 then begin
            Inc(cw, 2);
            if Assigned(canvas) then begin
               SetCharRects(n, VectorMake((px+1.05)/textureWidth,
                                          (textureHeight-(py+0.05))/textureHeight,
                                          (px+cw-1.05)/textureWidth,
                                          (textureHeight-(py+CharHeight-0.05))/textureHeight));
               rect.Left:=px;
               rect.Top:=py;
               rect.Right:=px+cw;
               rect.Bottom:=py+CharHeight;
               // Draw the Char, the trailing space is to properly handle the italics.
//               canvas.TextRect(rect, px+1, py+1, FWideChars[n]+WideString(' '));
               buffer[0] := FWideChars[n];

               // credits to the Unicode version of SynEdit for this function call. GPL/MPL as GLScene
               Windows.ExtTextOutW(canvas.Handle, px+1, py+1, ETO_CLIPPED, @rect, buffer, 2, nil);
            end;
            if ((n<255) and (px+cw+CharWidths[n+1]+2<=x)) or (n=255) then
               Inc(px, cw)
            else begin
               px:=0;
               Inc(py, CharHeight);
               if py+CharHeight>y then Break;
            end;
            Inc(Result);
         end;
         Inc(n);
      end;
   end;

  // credits to the Unicode version of SynEdit for this function. GPL/MPL as GLScene
  function GetTextSize(DC: HDC; Str: PWideChar; Count: Integer): TSize;
  var
    tm: TTextMetricA;
  begin
    Result.cx := 0;
    Result.cy := 0;
    GetTextExtentPoint32W(DC, Str, Count, Result);
    if not Win32PlatformIsUnicode then
    begin
      GetTextMetricsA(DC, tm);
      if tm.tmPitchAndFamily and TMPF_TRUETYPE <> 0 then
        Result.cx := Result.cx - tm.tmOverhang
      else
        Result.cx := tm.tmAveCharWidth * Count;
    end;
  end;

var
   bitmap : TGLBitMap;
   wChar : WideChar;
   x, y, i, cw : Integer;
   nbChars, n : Integer;
   texMem, bestTexMem : Integer;
begin
   InvalidateUsers;
   bitmap:=Glyphs.Bitmap;
   Glyphs.OnChange:=nil;

   bitmap.PixelFormat:=glpf32bit;
   with bitmap.Canvas do begin
      Font:=Self.Font;
      Font.Color:=clWhite;
      // get characters dimensions for the font
      CharWidth:=Round(2+MaxFloat(TextWidth('M'), TextWidth('W'), TextWidth('_')));
      CharHeight:=2+TextHeight('"_pI|,');
      if fsItalic in Font.Style then begin
         // italics aren't properly acknowledged in font width
         HSpaceFix:=-(CharWidth div 3);
         CharWidth:=CharWidth-HSpaceFix;
      end else HSpaceFix:=0;
   end;

   nbChars:=0;
   // Set width of all characters (texture width)
   For I := 0 to 255 do
   Begin
     wChar := FWideChars[I];
     If wChar <> WideChar(#0) then
     Begin
       cw := GetTextSize(bitmap.canvas.Handle, @wChar, 1).cx-HSpaceFix;
       SetCharWidths(I, cw);
       inc( nbChars );
     End else
     Begin
       SetCharWidths(I, 0);
     End;
   End;

   // compute texture size: look for best fill ratio
   // and as square a texture as possible
   bestTexMem:=MaxInt;
   textureWidth:=0;
   textureHeight:=0;
   y:=64; while y<=512 do begin
      x:=64; while x<=512 do begin
         // compute the number of characters that fit
         n:=ComputeCharRects(x, y, nil);
         if n=nbChars then begin
            texMem:=x*y;
            if (texMem<bestTexMem) or ((texMem=bestTexMem) and (Abs(x-y)<Abs(textureWidth-textureHeight))) then begin
               textureWidth:=x;
               textureHeight:=y;
               bestTexMem:=texMem;
            end;
         end;
         x:=2*x;
      end;
      y:=y*2;
   end;

   if bestTexMem=MaxInt then begin
      Font.Size:=6;
      raise Exception.Create('Characters are too large or too many. Unable to create font texture.');
   end;

   bitmap.Width:=textureWidth;
   bitmap.Height:=textureHeight;

   with bitmap.Canvas do
   begin
      Brush.Style:=bsSolid;
      Brush.Color:=clBlack;
      FillRect(Rect(0, 0, textureWidth, textureHeight));
   end;

   ComputeCharRects(textureWidth, textureHeight, bitmap.Canvas);

   Glyphs.OnChange:=OnGlyphsChanged;
end;

// PrepareImage
//
procedure TGLWideBitmapFont.PrepareImage;
begin
   LoadWideBitmapFont;
   inherited;
end;

// TextureFormat
//
function TGLWideBitmapFont.TextureFormat : Integer;
begin
   Result:=GL_ALPHA;
end;

procedure TGLWideBitmapFont.SetWideCharacters(const Value: WideString);
Var
  I, L : Integer;
//  CL   : Integer;
  wChar : WideChar;
//  Loader : TMemoryStream;
//  S : String;

begin
  if Length(FWideChars) = 0 then
  Begin
    SetLength(FWideChars, 256);
  End;

  For I := 0 to 255 do
  Begin
    FWideChars[I] := WideChar(#0);
  End;

  FWideCharacters := Value;

(*
//  Just a swift override hack to test loading the widestring
  Loader := TMemoryStream.Create;
  Loader.LoadFromFile('document.txt');
  Loader.Seek(2,soFromBeginning	); // does unicode strings allways have a prefix?
  SetLength(FWideCharacters, (Loader.Size-2) div 2);
  Loader.Read(FWideCharacters[1], Loader.Size-2);
  Loader.Free;
*)
//  I := 1;
  L := Length(WideCharacters);
  assert(L + 32 <= 255, 'You cannot have more than 223 characters in an WideBitmapFont!');

  For I := 1 to L do
  Begin
    wChar := WideCharacters[I];
    FWideChars[31+i] := wChar;
  End;

  LoadWideBitmapFont;
  NotifyChange(self);
end;

function TGLWideBitmapFont.Encode(const AWideString: WideString): String;
Var
  I : Integer;
begin
  Result := '';
  For I := 1 to Length(AWideString) do
  Begin
    Result := Result + Encode(AWideString[I]);
  End;
end;

function TGLWideBitmapFont.Encode(const AWideChar: WideChar): Char;
Var
  I : Integer;
begin
  For I := 0 to 255 do
  Begin
    If FWideChars[I] = AWideChar then
    Begin
      result := char(I);
      exit;
    End;
  End;
  // JAJ: if unknown let Delphi convert it, for better or worse... (Should handle linebreaks, I hope)
  Result := Char(AWideChar);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  Win32PlatformIsUnicode := (Win32Platform = VER_PLATFORM_WIN32_NT);

	// class registrations
   RegisterClasses([TGLWideBitmapFont]);


end.
