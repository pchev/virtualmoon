//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLColor<p>

   All color types, constants and utilities should go here<p>

  <b>History : </b><font size=-1><ul>
    <li>06/06/07 - DaStr - Initial version (BugtrackerID = 1732211)
                          (separated from GLTexture.pas and GLCrossPlatform.pas)
  </ul>
}
unit GLColor;

interface

{$i GLScene.inc}

uses
  // GLScene
  VectorTypes, Graphics, GLCrossPlatform;

type
  PColorVector = ^TColorVector;
  TColorVector = TVector4f;

  PRGBColor = ^TRGBColor;
  TRGBColor = TVector3b;

function GetRValue(rgb: DWORD): Byte;
function GetGValue(rgb: DWORD): Byte;
function GetBValue(rgb: DWORD): Byte;

procedure InitGLSceneColors;
{: Converts a delphi color into its RGB fragments and correct range. }
function ConvertWinColor(aColor: TColor; alpha : Single = 1) : TColorVector;

//: Converts a color vector (containing float values)
function ConvertColorVector(const AColor: TColorVector): TColor; overload;
{: Converts a color vector (containing float values) and alter intensity.<p>
   intensity is in [0..1] }
function ConvertColorVector(const AColor: TColorVector; intensity: Single): TColor; overload;
//: Converts RGB components into a color vector with correct range
function ConvertRGBColor(const aColor: array of Byte): TColorVector;

// color definitions

// Window's colors (must be filled at program
// startup, since they depend on the desktop scheme)
const
   {$J+ - allow change of the following typed constants}
   clrScrollBar           : TColorVector = (0,0,0,1);
   clrBackground          : TColorVector = (0,0,0,1);
   clrActiveCaption       : TColorVector = (0,0,0,1);
   clrInactiveCaption     : TColorVector = (0,0,0,1);
   clrMenu                : TColorVector = (0,0,0,1);
   clrWindow              : TColorVector = (0,0,0,1);
   clrWindowFrame         : TColorVector = (0,0,0,1);
   clrMenuText            : TColorVector = (0,0,0,1);
   clrWindowText          : TColorVector = (0,0,0,1);
   clrCaptionText         : TColorVector = (0,0,0,1);
   clrActiveBorder        : TColorVector = (0,0,0,1);
   clrInactiveBorder      : TColorVector = (0,0,0,1);
   clrAppWorkSpace        : TColorVector = (0,0,0,1);
   clrHighlight           : TColorVector = (0,0,0,1);
   clrHighlightText       : TColorVector = (0,0,0,1);
   clrBtnFace             : TColorVector = (0,0,0,1);
   clrBtnShadow           : TColorVector = (0,0,0,1);
   clrGrayText            : TColorVector = (0,0,0,1);
   clrBtnText             : TColorVector = (0,0,0,1);
   clrInactiveCaptionText : TColorVector = (0,0,0,1);
   clrBtnHighlight        : TColorVector = (0,0,0,1);
   clr3DDkShadow          : TColorVector = (0,0,0,1);
   clr3DLight             : TColorVector = (0,0,0,1);
   clrInfoText            : TColorVector = (0,0,0,1);
   clrInfoBk              : TColorVector = (0,0,0,1);
      
   {$J- - disable change of other typed constants}

   // 'static' color definitions
   // sort of grays
   clrTransparent         : TColorVector = (0,    0,    0,    0);
   clrBlack               : TColorVector = (0,    0,    0,    1);
   clrGray05              : TColorVector = (0.05, 0.05, 0.05, 1);
   clrGray10              : TColorVector = (0.10, 0.10, 0.10, 1);
   clrGray15              : TColorVector = (0.15, 0.15, 0.15, 1);
   clrGray20              : TColorVector = (0.20, 0.20, 0.20, 1);
   clrGray25              : TColorVector = (0.25, 0.25, 0.25, 1);
   clrGray30              : TColorVector = (0.30, 0.30, 0.30, 1);
   clrGray35              : TColorVector = (0.35, 0.35, 0.35, 1);
   clrGray40              : TColorVector = (0.40, 0.40, 0.40, 1);
   clrGray45              : TColorVector = (0.45, 0.45, 0.45, 1);
   clrGray50              : TColorVector = (0.50, 0.50, 0.50, 1);
   clrGray55              : TColorVector = (0.55, 0.55, 0.55, 1);
   clrGray60              : TColorVector = (0.60, 0.60, 0.60, 1);
   clrGray65              : TColorVector = (0.65, 0.65, 0.65, 1);
   clrGray70              : TColorVector = (0.70, 0.70, 0.70, 1);
   clrGray75              : TColorVector = (0.75, 0.75, 0.75, 1);
   clrGray80              : TColorVector = (0.80, 0.80, 0.80, 1);
   clrGray85              : TColorVector = (0.85, 0.85, 0.85, 1);
   clrGray90              : TColorVector = (0.90, 0.90, 0.90, 1);
   clrGray95              : TColorVector = (0.95, 0.95, 0.95, 1);
   clrWhite               : TColorVector = (1,    1,    1,    1);

   // other grays
   clrDimGray             : TColorVector = (0.329412, 0.329412, 0.329412, 1);
   clrGray                : TColorVector = (0.752941, 0.752941, 0.752941, 1);
   clrLightGray           : TColorVector = (0.658824, 0.658824, 0.658824, 1);

   // colors en masse
   clrAquamarine          : TColorVector = (0.439216, 0.858824, 0.576471, 1);
   clrBlueViolet          : TColorVector = (0.62352,  0.372549, 0.623529, 1);
   clrBrown               : TColorVector = (0.647059, 0.164706, 0.164706, 1);
   clrCadetBlue           : TColorVector = (0.372549, 0.623529, 0.623529, 1);
   clrCoral               : TColorVector = (1,        0.498039, 0.0,      1);
   clrCornflowerBlue      : TColorVector = (0.258824, 0.258824, 0.435294, 1);
   clrDarkGreen           : TColorVector = (0.184314, 0.309804, 0.184314, 1);
   clrDarkOliveGreen      : TColorVector = (0.309804, 0.309804, 0.184314, 1);
   clrDarkOrchid          : TColorVector = (0.6,      0.196078, 0.8,      1);
   clrDarkSlateBlue       : TColorVector = (0.419608, 0.137255, 0.556863, 1);
   clrDarkSlateGray       : TColorVector = (0.184314, 0.309804, 0.309804, 1);
   clrDarkSlateGrey       : TColorVector = (0.184314, 0.309804, 0.309804, 1);
   clrDarkTurquoise       : TColorVector = (0.439216, 0.576471, 0.858824, 1);
   clrFirebrick           : TColorVector = (0.556863, 0.137255, 0.137255, 1);
   clrForestGreen         : TColorVector = (0.137255, 0.556863, 0.137255, 1);
   clrGold                : TColorVector = (0.8,      0.498039, 0.196078, 1);
   clrGoldenrod           : TColorVector = (0.858824, 0.858824, 0.439216, 1);
   clrGreenYellow         : TColorVector = (0.576471, 0.858824, 0.439216, 1);
   clrIndian              : TColorVector = (0.309804, 0.184314, 0.184314, 1);
   clrKhaki               : TColorVector = (0.623529, 0.623529, 0.372549, 1);
   clrLightBlue           : TColorVector = (0.74902,  0.847059, 0.847059, 1);
   clrLightSteelBlue      : TColorVector = (0.560784, 0.560784, 0.737255, 1);
   clrLimeGreen           : TColorVector = (0.196078, 0.8,      0.196078, 1);
   clrMaroon              : TColorVector = (0.556863, 0.137255, 0.419608, 1);
   clrMediumAquamarine    : TColorVector = (0.196078, 0.8,      0.6,      1);
   clrMediumBlue          : TColorVector = (0.196078, 0.196078, 0.8,      1);
   clrMediumForestGreen   : TColorVector = (0.419608, 0.556863, 0.137255, 1);
   clrMediumGoldenrod     : TColorVector = (0.917647, 0.917647, 0.678431, 1);
   clrMediumOrchid        : TColorVector = (0.576471, 0.439216, 0.858824, 1);
   clrMediumSeaGreen      : TColorVector = (0.258824, 0.435294, 0.258824, 1);
   clrMediumSlateBlue     : TColorVector = (0.498039, 0,        1,        1);
   clrMediumSpringGreen   : TColorVector = (0.498039, 1,        0,        1);
   clrMediumTurquoise     : TColorVector = (0.439216, 0.858824, 0.858824, 1);
   clrMediumViolet        : TColorVector = (0.858824, 0.439216, 0.576471, 1);
   clrMidnightBlue        : TColorVector = (0.184314, 0.184314, 0.309804, 1);
   clrNavy                : TColorVector = (0.137255, 0.137255, 0.556863, 1);
   clrNavyBlue            : TColorVector = (0.137255, 0.137255, 0.556863, 1);
   clrOrange              : TColorVector = (1,        0.5,      0.0,      1);
   clrOrangeRed           : TColorVector = (1,        0.25,     0,        1);
   clrOrchid              : TColorVector = (0.858824, 0.439216, 0.858824, 1);
   clrPaleGreen           : TColorVector = (0.560784, 0.737255, 0.560784, 1);
   clrPink                : TColorVector = (0.737255, 0.560784, 0.560784, 1);
   clrPlum                : TColorVector = (0.917647, 0.678431, 0.917647, 1);
   clrSalmon              : TColorVector = (0.435294, 0.258824, 0.258824, 1);
   clrSeaGreen            : TColorVector = (0.137255, 0.556863, 0.419608, 1);
   clrSienna              : TColorVector = (0.556863, 0.419608, 0.137255, 1);
   clrSkyBlue             : TColorVector = (0.196078, 0.6,      0.8,      1);
   clrSlateBlue           : TColorVector = (0,        0.498039, 1,        1);
   clrSpringGreen         : TColorVector = (0,        1,        0.498039, 1);
   clrSteelBlue           : TColorVector = (0.137255, 0.419608, 0.556863, 1);
   clrTan                 : TColorVector = (0.858824, 0.576471, 0.439216, 1);
   clrThistle             : TColorVector = (0.847059, 0.74902,  0.847059, 1);
   clrTurquoise           : TColorVector = (0.678431, 0.917647, 0.917647, 1);
   clrViolet              : TColorVector = (0.309804, 0.184314, 0.309804, 1);
   clrVioletRed           : TColorVector = (0.8,      0.196078, 0.6,      1);
   clrWheat               : TColorVector = (0.847059, 0.847059, 0.74902,  1);
   clrYellowGreen         : TColorVector = (0.6,      0.8,      0.196078, 1);
   clrSummerSky           : TColorVector = (0.22,     0.69,     0.87,     1);
   clrRichBlue            : TColorVector = (0.35,     0.35,     0.67,     1);
   clrBrass               : TColorVector = (0.71,     0.65,     0.26,     1);
   clrCopper              : TColorVector = (0.72,     0.45,     0.20,     1);
   clrBronze              : TColorVector = (0.55,     0.47,     0.14,     1);
   clrBronze2             : TColorVector = (0.65,     0.49,     0.24,     1);
   clrSilver              : TColorVector = (0.90,     0.91,     0.98,     1);
   clrBrightGold          : TColorVector = (0.85,     0.85,     0.10,     1);
   clrOldGold             : TColorVector = (0.81,     0.71,     0.23,     1);
   clrFeldspar            : TColorVector = (0.82,     0.57,     0.46,     1);
   clrQuartz              : TColorVector = (0.85,     0.85,     0.95,     1);
   clrNeonPink            : TColorVector = (1.00,     0.43,     0.78,     1);
   clrDarkPurple          : TColorVector = (0.53,     0.12,     0.47,     1);
   clrNeonBlue            : TColorVector = (0.30,     0.30,     1.00,     1);
   clrCoolCopper          : TColorVector = (0.85,     0.53,     0.10,     1);
   clrMandarinOrange      : TColorVector = (0.89,     0.47,     0.20,     1);
   clrLightWood           : TColorVector = (0.91,     0.76,     0.65,     1);
   clrMediumWood          : TColorVector = (0.65,     0.50,     0.39,     1);
   clrDarkWood            : TColorVector = (0.52,     0.37,     0.26,     1);
   clrSpicyPink           : TColorVector = (1.00,     0.11,     0.68,     1);
   clrSemiSweetChoc       : TColorVector = (0.42,     0.26,     0.15,     1);
   clrBakersChoc          : TColorVector = (0.36,     0.20,     0.09,     1);
   clrFlesh               : TColorVector = (0.96,     0.80,     0.69,     1);
   clrNewTan              : TColorVector = (0.92,     0.78,     0.62,     1);
   clrNewMidnightBlue     : TColorVector = (0.00,     0.00,     0.61,     1);
   clrVeryDarkBrown       : TColorVector = (0.35,     0.16,     0.14,     1);
   clrDarkBrown           : TColorVector = (0.36,     0.25,     0.20,     1);
   clrDarkTan             : TColorVector = (0.59,     0.41,     0.31,     1);
   clrGreenCopper         : TColorVector = (0.32,     0.49,     0.46,     1);
   clrDkGreenCopper       : TColorVector = (0.29,     0.46,     0.43,     1);
   clrDustyRose           : TColorVector = (0.52,     0.39,     0.39,     1);
   clrHuntersGreen        : TColorVector = (0.13,     0.37,     0.31,     1);
   clrScarlet             : TColorVector = (0.55,     0.09,     0.09,     1);
   clrMediumPurple        : TColorVector = (0.73,     0.16,     0.96,     1);
   clrLightPurple         : TColorVector = (0.87,     0.58,     0.98,     1);
   clrVeryLightPurple     : TColorVector = (0.94,     0.81,     0.99,     1);
   clrGreen               : TColorVector = (0,        0.5,      0,        1);
   clrOlive               : TColorVector = (0.5,      0.5,      1,        1);
   clrPurple              : TColorVector = (1,        0,        1,        1);
   clrTeal                : TColorVector = (0,        0.5,      0.5,      1);
   clrRed                 : TColorVector = (1,        0,        0,        1);
   clrLime                : TColorVector = (0,        1,        0,        1);
   clrYellow              : TColorVector = (1,        1,        0,        1);
   clrBlue                : TColorVector = (0,        0,        1,        1);
   clrFuchsia             : TColorVector = (1,        0,        1,        1);
   clrAqua                : TColorVector = (0,        1,        1,        1);
{$J- - disallow change of the following typed constants}

implementation

// ConvertWinColor
//
function ConvertWinColor(aColor: TColor; alpha: Single = 1): TColorVector;
var
  winColor: Integer;
begin
  // Delphi color to Windows color
  winColor := ColorToRGB(aColor);
  // convert 0..255 range into 0..1 range
  Result[0] := (winColor and $FF) * (1 / 255);
  Result[1] := ((winColor shr 8) and $FF) * (1 / 255);
  Result[2] := ((winColor shr 16) and $FF) * (1 / 255);
  Result[3] := alpha;
end;

// GetRValue
//
function GetRValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb);
end;

// GetGValue
//
function GetGValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 8);
end;

// GetBValue
//
function GetBValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 16);
end;

// InitGLSceneColors
//
procedure InitGLSceneColors;
begin
  clrScrollBar := ConvertWinColor(clScrollBar);
  clrBackground := ConvertWinColor(clBackground);
  clrActiveCaption := ConvertWinColor(clActiveCaption);
  clrInactiveCaption := ConvertWinColor(clInactiveCaption);
  clrMenu := ConvertWinColor(clMenu);
  clrWindow := ConvertWinColor(clWindow);
  clrWindowFrame := ConvertWinColor(clWindowFrame);
  clrMenuText := ConvertWinColor(clMenuText);
  clrWindowText := ConvertWinColor(clWindowText);
  clrCaptionText := ConvertWinColor(clCaptionText);
  clrActiveBorder := ConvertWinColor(clActiveBorder);
  clrInactiveBorder := ConvertWinColor(clInactiveBorder);
  clrAppWorkSpace := ConvertWinColor(clAppWorkSpace);
  clrHighlight := ConvertWinColor(clHighlight);
  clrHighlightText := ConvertWinColor(clHighlightText);
  clrBtnFace := ConvertWinColor(clBtnFace);
  clrBtnShadow := ConvertWinColor(clBtnShadow);
  clrGrayText := ConvertWinColor(clGrayText);
  clrBtnText := ConvertWinColor(clBtnText);
  clrInactiveCaptionText := ConvertWinColor(clInactiveCaptionText);
  clrBtnHighlight := ConvertWinColor(clBtnHighlight);
  clr3DDkShadow := ConvertWinColor(cl3DDkShadow);
  clr3DLight := ConvertWinColor(cl3DLight);
  clrInfoText := ConvertWinColor(clInfoText);
  clrInfoBk := ConvertWinColor(clInfoBk);
end;

// ConvertColorVector
//
function ConvertColorVector(const aColor: TColorVector): TColor;
begin
  Result := RGB(
    Round(255 * aColor[0]),
    Round(255 * aColor[1]),
    Round(255 * aColor[2]));
end;

// ConvertColorVector
//
function ConvertColorVector(const aColor: TColorVector; intensity: Single): TColor;
begin
  intensity := 255 * intensity;
  Result := RGB(Round(intensity * aColor[0]),
    Round(intensity * aColor[1]),
    Round(intensity * aColor[2]));
end;

// ConvertRGBColor
//
function ConvertRGBColor(const aColor: array of Byte): TColorVector;
var
  n: Integer;
begin
  // convert 0..255 range into 0..1 range
  n := High(AColor);
  Result[0] := AColor[0] * (1 / 255);
  if n > 0 then
    Result[1] := AColor[1] * (1 / 255)
  else
    Result[1] := 0;
  if n > 1 then
    Result[2] := AColor[2] * (1 / 255)
  else
    Result[2] := 0;
  if n > 2 then
    Result[3] := AColor[3] * (1 / 255)
  else
    Result[3] := 1;
end;

initialization
  InitGLSceneColors;

end.
