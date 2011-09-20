//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLColor<p>

   All color types, constants and utilities should go here<p>

  <b>History : </b><font size=-1><ul>
    <li>04/11/10 - DaStr - Removed dependancy from OpenGL (this time for good)
    <li>24/10/10 - DaStr - Removed dependancy from OpenGL
    <li>23/08/10 - Yar - Added OpenGLTokens to uses
    <li>31/05/10 - Yar - Fixed warnings for Delhi2009/2010
    <li>04/03/10 - DanB - TGLColorManager.GetColor now uses CharInSet
    <li>05/10/08 - DanB - Moved TGLColor/ TGLColorManager in from GLTexture.pas
    <li>06/06/07 - DaStr - Initial version (BugtrackerID = 1732211)
                          (separated from GLTexture.pas and GLCrossPlatform.pas)
  </ul>
}
unit GLColor;

interface

{$i GLScene.inc}

uses
  // GLScene
  Classes,
{$IFDEF GLS_DELPHI_XE2_UP}
  VCL.Graphics,
{$ELSE}
  Graphics,
{$ENDIF}
  VectorTypes, VectorGeometry,
  GLCrossPlatform,
  PersistentClasses, BaseClasses;

type
  PColorVector = ^TColorVector;
  TColorVector = TVector4f;

  PRGBColor = ^TRGBColor;
  TRGBColor = TVector3b;

   // TGLColor
	//
   {: Wraps an OpenGL color. }
   TGLColor = class(TGLUpdateAbleObject)
      private
         { Private Properties }
			FColor : TColorVector;
         FPDefaultColor : PColorVector;
			procedure SetColorVector(const aColor : TColorVector); overload;
			procedure SetColorComponent(index : Integer; value : Single);
			procedure SetAsWinColor(const val : TColor);
			function GetAsWinColor : TColor;
      procedure SetDirectColorVector(const AColor: TColorVector);

		protected
         { Protected Properties }
			procedure DefineProperties(Filer: TFiler); override;
			procedure ReadData(Stream: TStream);
			procedure WriteData(Stream: TStream);

         function GetHSVA : TVector;
         procedure SetHSVA(const hsva : TVector);

		public
         { Public Properties }
			constructor Create(AOwner : TPersistent); override;
			constructor CreateInitialized(AOwner : TPersistent; const color : TColorVector;
                                       changeEvent : TNotifyEvent = nil);
         destructor Destroy; override;
         procedure NotifyChange(Sender : TObject); override;
			procedure Assign(Source : TPersistent); override;
			procedure Initialize(const color : TColorVector);
			function AsAddress : PSingle;

         procedure RandomColor;
         procedure SetColor(red, green, blue : Single; alpha : Single = 1); overload;

			property Color : TColorVector read FColor write SetColorVector;
      property DirectColor : TColorVector read FColor write SetDirectColorVector;
			property AsWinColor : TColor read GetAsWinColor write SetAsWinColor;
         property HSVA : TVector read GetHSVA write SetHSVA;

         property DefaultColor : TColorVector read FColor;

		published
         { Published Properties }
			property Red :   Single index 0 read FColor[0] write SetColorComponent stored False;
			property Green : Single index 1 read FColor[1] write SetColorComponent stored False;
			property Blue :  Single index 2 read FColor[2] write SetColorComponent stored False;
			property Alpha : Single index 3 read FColor[3] write SetColorComponent stored False;
	end;

   PColorEntry = ^TColorEntry;
   TColorEntry = record
                   Name  : String[31];
                   Color : TColorVector;
                 end;

   // TGLColorManager
   //
   TGLColorManager = class (TList)
      public
         destructor Destroy; override;

         procedure AddColor(const aName: String; const aColor: TColorVector);
         procedure EnumColors(Proc: TGetStrProc); overload;
         procedure EnumColors(AValues: TStrings); overload;

         function  FindColor(const aName: String): TColorVector;
         {: Convert a clrXxxx or a '<red green blue alpha> to a color vector }
         function  GetColor(const aName: String): TColorVector;
         function  GetColorName(const aColor: TColorVector): String;
         procedure RegisterDefaultColors;
         procedure RemoveColor(const aName: String);
   end;

function ColorManager: TGLColorManager;

procedure RegisterColor(const aName : String; const aColor : TColorVector);
procedure UnRegisterColor(const aName : String);


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
const
  // Some extra colors, not declared in Graphics.pas
  clForeground = TColor(-1);
  clButton = TColor(-2);
  clLight = TColor(-3);
  clMidlight = TColor(-4);
  clDark = TColor(-5);
  clMid = TColor(-6);
  clText = TColor(-7);
  clBrightText = TColor(-8);
  clButtonText = TColor(-9);
  clBase = TColor(-10);
  clBackground = TColor(-11);
  clShadow = TColor(-12);
  clHighlight = TColor(-13);
  clHighlightedText = TColor(-14);

  { Mapped role offsets }
  cloNormal = 32;
  cloDisabled = 64;
  cloActive = 96;

  { Normal, mapped, pseudo, rgb values }
  clNormalForeground = TColor(clForeground - cloNormal);
  clNormalButton = TColor(clButton - cloNormal);
  clNormalLight = TColor(clLight - cloNormal);
  clNormalMidlight = TColor(clMidlight - cloNormal);
  clNormalDark = TColor(clDark - cloNormal);
  clNormalMid = TColor(clMid - cloNormal);
  clNormalText = TColor(clText - cloNormal);
  clNormalBrightText = TColor(clBrightText - cloNormal);
  clNormalButtonText = TColor(clButtonText - cloNormal);
  clNormalBase = TColor(clBase - cloNormal);
  clNormalBackground = TColor(clBackground - cloNormal);
  clNormalShadow = TColor(clShadow - cloNormal);
  clNormalHighlight = TColor(clHighlight - cloNormal);
  clNormalHighlightedText = TColor(clHighlightedText - cloNormal);

  { Disabled, mapped, pseudo, rgb values }
  clDisabledForeground = TColor(clForeground - cloDisabled);
  clDisabledButton = TColor(clButton - cloDisabled);
  clDisabledLight = TColor(clLight - cloDisabled);
  clDisabledMidlight = TColor(clMidlight - cloDisabled);
  clDisabledDark = TColor(clDark - cloDisabled);
  clDisabledMid = TColor(clMid - cloDisabled);
  clDisabledText = TColor(clText - cloDisabled);
  clDisabledBrightText = TColor(clBrightText - cloDisabled);
  clDisabledButtonText = TColor(clButtonText - cloDisabled);
  clDisabledBase = TColor(clBase - cloDisabled);
  clDisabledBackground = TColor(clBackground - cloDisabled);
  clDisabledShadow = TColor(clShadow - cloDisabled);
  clDisabledHighlight = TColor(clHighlight - cloDisabled);
  clDisabledHighlightedText = TColor(clHighlightedText - cloDisabled);

  { Active, mapped, pseudo, rgb values }
  clActiveForeground = TColor(clForeground - cloActive);
  clActiveButton = TColor(clButton - cloActive);
  clActiveLight = TColor(clLight - cloActive);
  clActiveMidlight = TColor(clMidlight - cloActive);
  clActiveDark = TColor(clDark - cloActive);
  clActiveMid = TColor(clMid - cloActive);
  clActiveText = TColor(clText - cloActive);
  clActiveBrightText = TColor(clBrightText - cloActive);
  clActiveButtonText = TColor(clButtonText - cloActive);
  clActiveBase = TColor(clBase - cloActive);
  clActiveBackground = TColor(clBackground - cloActive);
  clActiveShadow = TColor(clShadow - cloActive);
  clActiveHighlight = TColor(clHighlight - cloActive);
  clActiveHighlightedText = TColor(clHighlightedText - cloActive);

  clFirstSpecialColor = clActiveHighlightedText;
  clMask = clWhite;
  clDontMask = clBlack;

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

var
   // Specifies if TGLColor should allocate memory for
   // their default values (ie. design-time) or not (run-time)
   vUseDefaultColorSets : Boolean = False;

implementation

uses
  SysUtils,
{$IFDEF GLS_DELPHI_XE2_UP}
  VCL.Dialogs;
{$ELSE}
  Dialogs;
{$ENDIF}

var
	vColorManager : TGLColorManager;

// ColorManager
//
function ColorManager : TGLColorManager;
begin
	if not Assigned(vColorManager) then begin
		vColorManager:=TGLColorManager.Create;
		vColorManager.RegisterDefaultColors;
	end;
	Result:=vColorManager;
end;


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
//  clrBackground := ConvertWinColor(clBackground);
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
//  clrHighlight := ConvertWinColor(clHighlight);
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

// ------------------
// ------------------ TGLColor ------------------
// ------------------

// Create
//
constructor TGLColor.Create(AOwner: TPersistent);
begin
   inherited;
   Initialize(clrBlack);
end;

// CreateInitialized
//
constructor TGLColor.CreateInitialized(AOwner : TPersistent; const color : TColorVector;
                                       changeEvent : TNotifyEvent = nil);
begin
   Create(AOwner);
   Initialize(color);
   OnNotifyChange:=changeEvent;
end;

// Destroy
//
destructor TGLColor.Destroy;
begin
   if assigned(FPDefaultColor) then Dispose(FPDefaultColor);
   inherited;
end;

// Initialize
//
procedure TGLColor.Initialize(const color : TColorVector);
begin
   SetVector(FColor, color);
   if vUseDefaultColorSets then
   begin
      if not assigned(FPDefaultColor) then
         New(FPDefaultColor);
      SetVector(FPDefaultColor^, color);
   end;
end;

// SetColorVector
//
procedure TGLColor.SetColorVector(const aColor : TColorVector);
begin
   SetVector(FColor, AColor);
	NotifyChange(Self);
end;

procedure TGLColor.SetDirectColorVector(const AColor: TColorVector);
begin
  SetVector(FColor, AColor);
end;

// SetColorComponent
//
procedure TGLColor.SetColorComponent(index : Integer; value : Single);
begin
	if FColor[index]<>value then begin
		FColor[index]:=value;
		NotifyChange(Self);
	end;
end;

// SetAsWinColor
//
procedure TGLColor.SetAsWinColor(const val : TColor);
begin
	FColor:=ConvertWinColor(val);
	NotifyChange(Self);
end;

// GetAsWinColor
//
function TGLColor.GetAsWinColor : TColor;
begin
	Result:=ConvertColorVector(FColor);
end;

// Assign
//
procedure TGLColor.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLColor) then begin
		FColor:=TGLColor(Source).FColor;
      NotifyChange(Self);
   end else inherited;
end;

// DefineProperties
//
procedure TGLColor.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Color', ReadData, WriteData,
                             not (Assigned(FPDefaultColor) and VectorEquals(FColor, FPDefaultColor^)));
end;

// ReadData
//
procedure TGLColor.ReadData(Stream: TStream);
begin
   Stream.Read(FColor, SizeOf(FColor));
end;

// WriteData
//
procedure TGLColor.WriteData(Stream: TStream);
begin
   Stream.Write(FColor, SizeOf(FColor));
end;

// NotifyChange
//
procedure TGLColor.NotifyChange(Sender : TObject);
var
  intf: IGLNotifyable;
begin
   if Assigned(Owner) then begin
      if Supports(Owner,IGLNotifyable, intf) then
        intf.NotifyChange(Self);
//      if Owner is TGLBaseSceneObject then
//         TGLBaseSceneObject(Owner).StructureChanged;
      inherited;
   end;
end;

// AsAddress
//
function TGLColor.AsAddress: PSingle;
begin
	Result:=@FColor;
end;

// RandomColor
//
procedure TGLColor.RandomColor;
begin
   Red:=Random;
   Green:=Random;
   Blue:=Random;
end;

// SetColor
//
procedure TGLColor.SetColor(red, green, blue : Single; alpha : Single = 1);
begin
   FColor[0]:=red;
   FColor[1]:=Green;
   FColor[2]:=blue;
   FColor[3]:=alpha;
   NotifyChange(Self);
end;

// GetHSVA
//
function TGLColor.GetHSVA : TVector;
var
   delta, min : Single;
const
   H = 0;
   S = 1;
   V = 2;
begin
   min:=MinFloat(PFloatVector(@FColor), 3);
   Result[V]:=MaxFloat(PFloatVector(@FColor), 3);
   delta:=Result[V]-min;

  // saturation is zero if R, G & B are zero
  // hue undefined (zero) if saturation is zero or color is gray (delta=zero)
   if (Result[V]=0) or (delta=0) then begin
      Result[S]:=0;
      Result[H]:=0;
   end else begin
      Result[S]:=delta/Result[V];
      if Red=Result[V] then
         // between yellow and magenta
         Result[H]:=60*(Green-Blue)/delta
      else if Green=Result[V] then
         // between cyan and yellow
         Result[H]:=120+60*(Blue-Red)/delta
      else // between magenta and cyan
         Result[H]:=240+60*(Red-Green)/delta;
      if Result[H]<0 then  // normalize H
         Result[H]:=Result[H]+360;
   end;
   Result[3]:=Alpha;
end;

// SetHSVA
//
procedure TGLColor.SetHSVA(const hsva : TVector);
var
   f, hTemp, p, q, t : Single;
const
   H = 0;
   S = 1;
   V = 2;
begin
   if hsva[S]=0 then begin
      // gray (ignore hue)
      FColor[0]:=hsva[V];
      FColor[1]:=hsva[V];
      FColor[2]:=hsva[V];
   end else begin
      hTemp:=hsva[H]*(1/60);
      f:=Frac(hTemp);

      p:=hsva[V]*(1-hsva[S]);
      q:=hsva[V]*(1-(hsva[S]*f));
      t:=hsva[V]*(1-(hsva[S]*(1-f)));

      case Trunc(hTemp) mod 6 of
         0 : begin
            FColor[0]:=hsva[V];
            FColor[1]:=t;
            FColor[2]:=p;
         end;
         1 : begin
            FColor[0]:=q;
            FColor[1]:=hsva[V];
            FColor[2]:=p;
         end;
         2 : begin
            FColor[0]:=p;
            FColor[1]:=hsva[V];
            FColor[2]:=t;
         end;
         3 : begin
            FColor[0]:=p;
            FColor[1]:=q;
            FColor[2]:=hsva[V];
         end;
         4 : begin
            FColor[0]:=t;
            FColor[1]:=p;
            FColor[2]:=hsva[V];
         end;
         5 : begin
            FColor[0]:=hsva[V];
            FColor[1]:=p;
            FColor[2]:=q;
         end;
      end
   end;
   FColor[3]:=hsva[3];
   NotifyChange(Self);
end;

// ------------------
// ------------------ TGLColorManager ------------------
// ------------------

// Find Color
//
function TGLColorManager.FindColor(const aName: String): TColorVector;
var
   i : Integer;
begin
   Result:=clrBlack;
   for i:=0 to Count-1 do
      if CompareText(string(TColorEntry(Items[i]^).Name), AName)=0 then begin
         SetVector(Result, TColorEntry(Items[i]^).Color);
         Break;
      end;
end;

// GetColor
//
function TGLColorManager.GetColor(const aName : String): TColorVector;
var
   workCopy  : String;
   delimiter : Integer;
begin
   if aName='' then
      Result:=clrBlack
   else begin
      workCopy:=Trim(AName);
      if CharInSet(AName[1], ['(','[','<']) then
         workCopy:=Copy(workCopy, 2, Length(AName)-2);
      if CompareText(Copy(workCopy,1,3),'clr')=0 then
         SetVector(Result, FindColor(workCopy))
      else try
         // initialize result
         Result:=clrBlack;
         workCopy:=Trim(workCopy);
         delimiter:=Pos(' ', workCopy);
         if (Length(workCopy)>0) and (delimiter>0) then begin
            Result[0]:=StrToFloat(Copy(workCopy, 1, delimiter-1));
            System.Delete(workCopy, 1, delimiter);
            workCopy:=TrimLeft(workCopy);
            delimiter:=Pos(' ',workCopy);
            if (Length(workCopy)>0) and (delimiter>0) then begin
               Result[1]:=StrToFloat(Copy(workCopy, 1, delimiter-1));
               System.Delete(workCopy, 1, delimiter);
               workCopy:=TrimLeft(workCopy);
               delimiter:=Pos(' ', workCopy);
               if (Length(workCopy)>0) and (delimiter>0) then begin
                  Result[2]:=StrToFloat(Copy(workCopy, 1, delimiter-1));
                  System.Delete(workCopy, 1, delimiter);
                  workCopy:=TrimLeft(workCopy);
                  Result[3]:=StrToFloat(workCopy);
               end else Result[2]:=StrToFloat(workCopy);
            end else Result[1]:=StrToFloat(workCopy);
         end else Result[0]:=StrToFloat(workCopy);
      except
         ShowMessage('Wrong vector format. Use: ''<red green blue alpha>''!');
         Abort;
      end;
   end;
end;

//------------------------------------------------------------------------------

function TGLColorManager.GetColorName(const aColor: TColorVector): String;

const MinDiff = 1e-6;

var I : Integer;

begin
  for I:=0 to Count-1 do
    with TColorEntry(Items[I]^) do
      if (Abs(Color[0]-AColor[0]) < MinDiff) and
         (Abs(Color[1]-AColor[1]) < MinDiff) and
         (Abs(Color[2]-AColor[2]) < MinDiff) and
         (Abs(Color[3]-AColor[3]) < MinDiff) then Break;
  if I < Count then
    Result:=string(TColorEntry(Items[I]^).Name)
  else
    Result:=Format('<%.3f %.3f %.3f %.3f>',[AColor[0],AColor[1],AColor[2],AColor[3]]);
end;

// Destroy
//
destructor TGLColorManager.Destroy;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      FreeMem(Items[i], SizeOf(TColorEntry));
   inherited Destroy;
end;

// AddColor
//
procedure TGLColorManager.AddColor(const aName: String; const aColor: TColorVector);
var
   newEntry : PColorEntry;
begin
   New(newEntry);
   if newEntry = nil then
      raise Exception.Create('Could not allocate memory for color registration!');
   with newEntry^ do begin
     Name:=shortstring(AName);
     SetVector(Color, aColor);
   end;
   Add(newEntry);
end;

// EnumColors
//
procedure TGLColorManager.EnumColors(Proc: TGetStrProc);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Proc(string(TColorEntry(Items[i]^).Name));
end;

// EnumColors
//
procedure TGLColorManager.EnumColors(AValues: TStrings);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      AValues.Add(string(TColorEntry(Items[i]^).Name));
end;


// RegisterDefaultColors
//
procedure TGLColorManager.RegisterDefaultColors;
begin
   Capacity:=150;
   AddColor('clrTransparent',clrTransparent);
   AddColor('clrBlack',clrBlack);
   AddColor('clrGray05',clrGray05);
   AddColor('clrGray10',clrGray10);
   AddColor('clrGray15',clrGray15);
   AddColor('clrGray20',clrGray20);
   AddColor('clrGray25',clrGray25);
   AddColor('clrGray30',clrGray30);
   AddColor('clrGray35',clrGray35);
   AddColor('clrGray40',clrGray40);
   AddColor('clrGray45',clrGray45);
   AddColor('clrGray50',clrGray50);
   AddColor('clrGray55',clrGray55);
   AddColor('clrGray60',clrGray60);
   AddColor('clrGray65',clrGray65);
   AddColor('clrGray70',clrGray70);
   AddColor('clrGray75',clrGray75);
   AddColor('clrGray80',clrGray80);
   AddColor('clrGray85',clrGray85);
   AddColor('clrGray90',clrGray90);
   AddColor('clrGray95',clrGray95);
   AddColor('clrWhite',clrWhite);
   AddColor('clrDimGray',clrDimGray);
   AddColor('clrGray',clrGray);
   AddColor('clrLightGray',clrLightGray);
   AddColor('clrAquamarine',clrAquamarine);
   AddColor('clrBakersChoc',clrBakersChoc);
   AddColor('clrBlueViolet',clrBlueViolet);
   AddColor('clrBrass',clrBrass);
   AddColor('clrBrightGold',clrBrightGold);
   AddColor('clrBronze',clrBronze);
   AddColor('clrBronze2',clrBronze2);
   AddColor('clrBrown',clrBrown);
   AddColor('clrCadetBlue',clrCadetBlue);
   AddColor('clrCoolCopper',clrCoolCopper);
   AddColor('clrCopper',clrCopper);
   AddColor('clrCoral',clrCoral);
   AddColor('clrCornflowerBlue',clrCornflowerBlue);
   AddColor('clrDarkBrown',clrDarkBrown);
   AddColor('clrDarkGreen',clrDarkGreen);
   AddColor('clrDarkOliveGreen',clrDarkOliveGreen);
   AddColor('clrDarkOrchid',clrDarkOrchid);
   AddColor('clrDarkPurple',clrDarkPurple);
   AddColor('clrDarkSlateBlue',clrDarkSlateBlue);
   AddColor('clrDarkSlateGray',clrDarkSlateGray);
   AddColor('clrDarkSlateGrey',clrDarkSlateGrey);
   AddColor('clrDarkTan',clrDarkTan);
   AddColor('clrDarkTurquoise',clrDarkTurquoise);
   AddColor('clrDarkWood',clrDarkWood);
   AddColor('clrDkGreenCopper',clrDkGreenCopper);
   AddColor('clrDustyRose',clrDustyRose);
   AddColor('clrFeldspar',clrFeldspar);
   AddColor('clrFirebrick',clrFirebrick);
   AddColor('clrFlesh',clrFlesh);
   AddColor('clrForestGreen',clrForestGreen);
   AddColor('clrGold',clrGold);
   AddColor('clrGoldenrod',clrGoldenrod);
   AddColor('clrGreenCopper',clrGreenCopper);
   AddColor('clrGreenYellow',clrGreenYellow);
   AddColor('clrHuntersGreen',clrHuntersGreen);
   AddColor('clrIndian',clrIndian);
   AddColor('clrKhaki',clrKhaki);
   AddColor('clrLightBlue',clrLightBlue);
   AddColor('clrLightPurple',clrLightPurple);
   AddColor('clrLightSteelBlue',clrLightSteelBlue);
   AddColor('clrLightWood',clrLightWood);
   AddColor('clrLimeGreen',clrLimeGreen);
   AddColor('clrMandarinOrange',clrMandarinOrange);
   AddColor('clrMaroon',clrMaroon);
   AddColor('clrMediumAquamarine',clrMediumAquamarine);
   AddColor('clrMediumBlue',clrMediumBlue);
   AddColor('clrMediumForestGreen',clrMediumForestGreen);
   AddColor('clrMediumGoldenrod',clrMediumGoldenrod);
   AddColor('clrMediumOrchid',clrMediumOrchid);
   AddColor('clrMediumPurple',clrMediumPurple);
   AddColor('clrMediumSeaGreen',clrMediumSeaGreen);
   AddColor('clrMediumSlateBlue',clrMediumSlateBlue);
   AddColor('clrMediumSpringGreen',clrMediumSpringGreen);
   AddColor('clrMediumTurquoise',clrMediumTurquoise);
   AddColor('clrMediumViolet',clrMediumViolet);
   AddColor('clrMediumWood',clrMediumWood);
   AddColor('clrMidnightBlue',clrMidnightBlue);
   AddColor('clrNavy',clrNavy);
   AddColor('clrNavyBlue',clrNavyBlue);
   AddColor('clrNeonBlue',clrNeonBlue);
   AddColor('clrNeonPink',clrNeonPink);
   AddColor('clrNewMidnightBlue',clrNewMidnightBlue);
   AddColor('clrNewTan',clrNewTan);
   AddColor('clrOldGold',clrOldGold);
   AddColor('clrOrange',clrOrange);
   AddColor('clrOrangeRed',clrOrangeRed);
   AddColor('clrOrchid',clrOrchid);
   AddColor('clrPaleGreen',clrPaleGreen);
   AddColor('clrPink',clrPink);
   AddColor('clrPlum',clrPlum);
   AddColor('clrQuartz',clrQuartz);
   AddColor('clrRichBlue',clrRichBlue);
   AddColor('clrSalmon',clrSalmon);
   AddColor('clrScarlet',clrScarlet);
   AddColor('clrSeaGreen',clrSeaGreen);
   AddColor('clrSemiSweetChoc',clrSemiSweetChoc);
   AddColor('clrSienna',clrSienna);
   AddColor('clrSilver',clrSilver);
   AddColor('clrSkyBlue',clrSkyBlue);
   AddColor('clrSlateBlue',clrSlateBlue);
   AddColor('clrSpicyPink',clrSpicyPink);
   AddColor('clrSpringGreen',clrSpringGreen);
   AddColor('clrSteelBlue',clrSteelBlue);
   AddColor('clrSummerSky',clrSummerSky);
   AddColor('clrTan',clrTan);
   AddColor('clrThistle',clrThistle);
   AddColor('clrTurquoise',clrTurquoise);
   AddColor('clrViolet',clrViolet);
   AddColor('clrVioletRed',clrVioletRed);
   AddColor('clrVeryDarkBrown',clrVeryDarkBrown);
   AddColor('clrVeryLightPurple',clrVeryLightPurple);
   AddColor('clrWheat',clrWheat);
   AddColor('clrYellowGreen',clrYellowGreen);
   AddColor('clrGreen',clrGreen);
   AddColor('clrOlive',clrOlive);
   AddColor('clrPurple',clrPurple);
   AddColor('clrTeal',clrTeal);
   AddColor('clrRed',clrRed);
   AddColor('clrLime',clrLime);
   AddColor('clrYellow',clrYellow);
   AddColor('clrBlue',clrBlue);
   AddColor('clrFuchsia',clrFuchsia);
   AddColor('clrAqua',clrAqua);

   AddColor('clrScrollBar',clrScrollBar);
   AddColor('clrBackground',clrBackground);
   AddColor('clrActiveCaption',clrActiveCaption);
   AddColor('clrInactiveCaption',clrInactiveCaption);
   AddColor('clrMenu',clrMenu);
   AddColor('clrWindow',clrWindow);
   AddColor('clrWindowFrame',clrWindowFrame);
   AddColor('clrMenuText',clrMenuText);
   AddColor('clrWindowText',clrWindowText);
   AddColor('clrCaptionText',clrCaptionText);
   AddColor('clrActiveBorder',clrActiveBorder);
   AddColor('clrInactiveBorder',clrInactiveBorder);
   AddColor('clrAppWorkSpace',clrAppWorkSpace);
   AddColor('clrHighlight',clrHighlight);
   AddColor('clrHighlightText',clrHighlightText);
   AddColor('clrBtnFace',clrBtnFace);
   AddColor('clrBtnShadow',clrBtnShadow);
   AddColor('clrGrayText',clrGrayText);
   AddColor('clrBtnText',clrBtnText);
   AddColor('clrInactiveCaptionText',clrInactiveCaptionText);
   AddColor('clrBtnHighlight',clrBtnHighlight);
   AddColor('clr3DDkShadow',clr3DDkShadow);
   AddColor('clr3DLight',clr3DLight);
   AddColor('clrInfoText',clrInfoText);
   AddColor('clrInfoBk',clrInfoBk);
end;

// RemoveColor
//
procedure TGLColorManager.RemoveColor(const aName: String);
var
   i : Integer;
begin
   for i:=0 to Count-1 do begin
      if CompareText(string(TColorEntry(Items[i]^).Name), aName)=0 then begin
         Delete(i);
         Break;
	   end;
   end;
end;

// RegisterColor
//
procedure RegisterColor(const aName : String; const aColor : TColorVector);
begin
   ColorManager.AddColor(AName, AColor);
end;

// UnregisterColor
//
procedure UnregisterColor(const aName : String);
begin
   ColorManager.RemoveColor(AName);
end;


initialization
  InitGLSceneColors;

finalization
	vColorManager.Free;

end.
