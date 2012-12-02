//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLCrossPlatform<p>

   Cross platform support functions and types for GLScene.<p>

   Ultimately, *no* cross-platform or cross-version defines should be present
   in the core GLScene units, and have all moved here instead.<p>

      $Log: glcrossplatform.pas,v $
      Revision 1.2  2006/01/11 05:28:33  z0m3ie
      *** empty log message ***

      Revision 1.1  2006/01/10 20:50:46  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.1  2006/01/09 21:02:33  z0m3ie
      *** empty log message ***

      Revision 1.4  2005/12/04 16:53:04  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.3  2005/08/03 00:41:39  z0m3ie
      - added automatical generated History from CVS

	<b>Historique : </b><font size=-1><ul>
      <li>29/05/08 - DaStr - Added StrToFloatDef(), TryStrToFloat()
      <li>10/04/08 - DaStr - Added TGLComponent (BugTracker ID = 1938988)
      <li>07/04/08 - DaStr - Added IsInfinite, IsNan
      <li>18/11/07 - DaStr - Added ptrInt and PtrUInt types (BugtrackerID = 1833830)
                              (thanks Dje and Burkhard Carstens)
      <li>06/06/07 - DaStr - Added WORD type
                             Got rid of GLTexture.pas dependancy
                             Moved GetRValue, GetGValue, GetBValue, InitWinColors
                               to GLColor.pas (BugtrackerID = 1732211)
      <li>02/04/07 - DaStr - Added MakeSubComponent
                             Fixed some IFDEFs to separate FPC from Kylix
      <li>25/03/07 - DaStr - Replaced some UNIX IFDEFs with KYLIX
                             Added IdentToColor, ColorToIdent, ColorToString,
                                   AnsiStartsText, IsSubComponent
                             Added TPoint, PPoint, TRect, PRect, TPicture, TGraphic,
                                   TBitmap, TTextLayout, TMouseButton, TMouseEvent,
                                   TKeyEvent, TKeyPressEvent
                             Added IInterface, S_OK, E_NOINTERFACE,
                                   glKey_PRIOR, glKey_NEXT, glKey_CONTROL
      <li>24/03/07 - DaStr - Added TPenStyle, TPenMode, TBrushStyle, more color constants,
                             Added "Application" function
      <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTracekrID=1681585)
      <li>08/07/04 - LR - Added clBlack
      <li>03/07/04 - LR - Added constant for Keyboard (glKey_TAB, ...)
                          Added function GLOKMessageBox to avoid the uses of Forms
                          Added other abstraction calls
                          Added procedure ShowHTMLUrl for unit Info.pas
                          Added GLShowCursor, GLSetCursorPos, GLGetCursorPos,
                          GLGetScreenWidth, GLGetScreenHeight for GLNavigation
                          Added GLGetTickCount for GLFPSMovement
      <li>28/06/04 - LR - Added TGLTextLayout, GLLoadBitmapFromInstance
                          Added GetDeviceCapabilities to replace the old function
      <li>30/05/03 - EG - Added RDTSC and RDTSC-based precision timing for non-WIN32
      <li>22/01/02 - EG - Added OpenPictureDialog, ApplicationTerminated
      <li>07/01/02 - EG - Added QuestionDialog and SavePictureDialog,
                          Added PrecisionTimer funcs
      <li>06/12/01 - EG - Added several abstraction calls
      <li>31/08/01 - EG - Creation
	</ul></font>
}
unit GLCrossPlatform;

interface

{$include GLScene.inc}

//**************************************
//**  FPC uses section
{$IFDEF FPC}
uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, Graphics, Controls, Forms, VectorTypes,
  Dialogs, StdCtrls, ExtDlgs, Math, strutils, LCLType, LCLIntf, types, ComponentEditors
  {$IFDEF UNIX}
  , Buttons, unix
  {$ENDIF}
;
{$ENDIF}
//**  end of FPC uses section
//**************************************

//**************************************
//**  Delphi uses section
{$IFNDEF FPC} //Not FPC

{$IFDEF MSWINDOWS}
uses
  Windows, Classes, SysUtils, Graphics, Controls, Forms, VectorTypes,
  Dialogs, StdCtrls, ExtDlgs, Consts
  {$IFDEF GLS_COMPILER_6_UP}, Math, StrUtils{$ENDIF}
  ;
{$ENDIF}
{$ENDIF}
//**  end of Delphi uses section
//**************************************

type
{$IFNDEF FPC}
  // These new types were added to be able to cast pointers to integers
  // in 64 bit mode, because in FPC "Integer" type is always 32 bit 
  // (or 16 bit in Pascal mode), but in Delphi it is platform-specific and
  // can be 16, 32 or 64 bit.
  ptrInt  = Integer;
  PtrUInt = Cardinal;
{$ENDIF}

   // Several aliases to shield us from the need of ifdef'ing between
   // the "almost cross-platform" units like Graphics/QGraphics etc.
   // Gives a little "alien" look to names, but that's the only way around :(

   // DaStr: Actually, there is a way around, see TPenStyle for example.

   TGLPoint = TPoint;
   PGLPoint = ^TGLPoint;
   TGLRect = TRect;
   PGLRect = ^TGLRect;
   TDelphiColor = TColor;

   TGLPicture = TPicture;
   TGLGraphic = TGraphic;
   TGLBitmap = TBitmap;
   TGraphicClass = class of TGraphic;

   TGLTextLayout = (tlTop, tlCenter, tlBottom); // idem TTextLayout;

   TGLMouseButton = (mbLeft, mbRight, mbMiddle); // idem TMouseButton;
   TGLMouseEvent = procedure(Sender: TObject; Button: TGLMouseButton;
    Shift: TShiftState; X, Y: Integer) of object;
   TGLMouseMoveEvent = TMouseMoveEvent;
   TGLKeyEvent = TKeyEvent;
   TGLKeyPressEvent = TKeyPressEvent;

{$IFDEF GLS_DELPHI_5}
   EGLOSError = EWin32Error;
{$ELSE}
   {$IFDEF FPC}
   {$IFDEF unix}
      EGLOSError = EOSError;
   {$ELSE}
      EGLOSError = EWin32Error;
   {$ENDIF}
   {$ELSE}
      EGLOSError = EOSError;
   {$ENDIF}
{$ENDIF}

{$IFDEF GLS_DELPHI_5_DOWN}
  IInterface = IUnknown;
{$ENDIF}

// A work-around a Delphi5 interface bug.
{$IFDEF GLS_DELPHI_5_DOWN}
  TGLComponent = class(TComponent, IInterface);
{$ELSE}
  TGLComponent = class(TComponent);
{$ENDIF}

{$IFDEF FPC}
   TGLDesigner = TComponentEditorDesigner;
{$ELSE}
  {$ifdef GLS_DELPHI_6_UP}
    TGLDesigner = IDesigner;
  {$else}
    TGLDesigner = IFormDesigner;
  {$endif}
{$ENDIF}

  {$IFDEF FPC}
  TPoint = Types.TPoint;
  PPoint = Types.PPoint;
  TRect = Types.TRect;
  PRect = Types.PRect;

  TTextLayout = Graphics.TTextLayout;
  HDC         = LCLType.HDC;
  {$ELSE}
  TPoint = Windows.TPoint;
  PPoint = Windows.PPoint;
  TRect = Windows.TRect;
  PRect = Windows.PRect;

  TTextLayout = StdCtrls.TTextLayout;

  {$ENDIF}
  TPicture = Graphics.TPicture;
  TGraphic = Graphics.TGraphic;
  TBitmap = Graphics.TBitmap;


  TMouseButton = Controls.TMouseButton;
  TMouseEvent = Controls.TMouseEvent;
  TKeyEvent = Controls.TKeyEvent;
  TKeyPressEvent = Controls.TKeyPressEvent;

  TColor      = Graphics.TColor;
  TPenStyle   = Graphics.TPenStyle;
  TPenMode    = Graphics.TPenMode;
  TBrushStyle = Graphics.TBrushStyle;

const
{$IFDEF GLS_DELPHI_5_DOWN}
  S_OK = Windows.S_OK;
  E_NOINTERFACE = Windows.E_NOINTERFACE;
{$ENDIF}

   glpf8Bit = pf8bit;
   glpf24bit = pf24bit;
   glpf32Bit = pf32bit;
   glpfDevice = pfDevice;

   // standard colors
   { Raw rgb values }
  clBlack = Graphics.clBlack;
  clMaroon = Graphics.clMaroon;
  clGreen = Graphics.clGreen;
  clOlive = Graphics.clOlive;
  clNavy = Graphics.clNavy;
  clPurple = Graphics.clPurple;
  clTeal = Graphics.clTeal;
  clGray = Graphics.clGray;
  clSilver = Graphics.clSilver;
  clRed = Graphics.clRed;
  clLime = Graphics.clLime;
  clYellow = Graphics.clYellow;
  clBlue = Graphics.clBlue;
  clFuchsia = Graphics.clFuchsia;
  clAqua = Graphics.clAqua;
  clLtGray = Graphics.clLtGray;
  clDkGray = Graphics.clDkGray;
  clWhite = Graphics.clWhite;
  clNone = Graphics.clNone;
  clDefault = Graphics.clDefault;

// Not declared in Graphics.pas
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
  clBackground = {$IFDEF LCL}graphics.clBackground;{$ELSE}TColor(-11);{$ENDIF}
  clShadow = TColor(-12);
  clHighlight = {$IFDEF LCL}graphics.clHighlight;{$ELSE}TColor(-13);{$ENDIF}
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

// Not declared in Graphics.pas
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


  { Compatiblity colors }
  clScrollBar = Graphics.clScrollBar;
  clActiveCaption = Graphics.clActiveCaption;
  clInactiveCaption = Graphics.clInactiveCaption;
  clMenu = Graphics.clMenu;
  clWindow = Graphics.clWindow;
  clWindowFrame = Graphics.clWindowFrame;
  clMenuText = Graphics.clMenuText;
  clWindowText = Graphics.clWindowText;
  clCaptionText = Graphics.clCaptionText;
  clActiveBorder = Graphics.clActiveBorder;
  clInactiveBorder = Graphics.clInactiveBorder;
  clAppWorkSpace = Graphics.clAppWorkSpace;
  clBtnFace = Graphics.clBtnFace;
  clBtnShadow = Graphics.clBtnShadow;
  clGrayText = Graphics.clGrayText;
  clBtnText = Graphics.clBtnText;
  clInactiveCaptionText = Graphics.clInactiveCaptionText;
  clBtnHighlight = Graphics.clBtnHighlight;
  cl3DDkShadow = Graphics.cl3DDkShadow;
  cl3DLight = Graphics.cl3DLight;
  clInfoText = Graphics.clInfoText;
  clInfoBk = Graphics.clInfoBk;
  clHighlightText = Graphics.clHighlightText;


 // Not declared in Graphics.pas
  clFirstSpecialColor = clActiveHighlightedText;
  clMask = clWhite;
  clDontMask = clBlack;

// standard keyboard
  glKey_ESCAPE = VK_ESCAPE;
  glKey_SHIFT = VK_SHIFT;
  glKey_TAB = VK_TAB;
  glKey_SPACE = VK_SPACE;
  glKey_RETURN = VK_RETURN;
  glKey_DELETE = VK_DELETE;
  glKey_LEFT = VK_LEFT;
  glKey_RIGHT = VK_RIGHT;
  glKey_HOME = VK_HOME;
  glKey_END = VK_END;
  glKey_CANCEL = VK_CANCEL;
  glKey_UP = VK_UP;
  glKey_DOWN = VK_DOWN;
  glKey_PRIOR = VK_PRIOR;
  glKey_NEXT = VK_NEXT;
  glKey_CONTROL = VK_CONTROL;

// TPenStyle.
  psSolid = Graphics.psSolid;
  psDash = Graphics.psDash;
  psDot = Graphics.psDot;
  psDashDot = Graphics.psDashDot;
  psDashDotDot = Graphics.psDashDotDot;
  psClear = Graphics.psClear;
  {.$IFNDEF FPC}
  psInsideFrame = Graphics.psInsideFrame;
  {.$ENDIF}

// TPenMode.
  pmBlack = Graphics.pmBlack;
  pmWhite = Graphics.pmWhite;
  pmNop = Graphics.pmNop;
  pmNot = Graphics.pmNot;
  pmCopy = Graphics.pmCopy;
  pmNotCopy = Graphics.pmNotCopy;

  pmMergePenNot = Graphics.pmMergePenNot;
  pmMaskPenNot = Graphics.pmMaskPenNot;
  pmMergeNotPen = Graphics.pmMergeNotPen;
  pmMaskNotPen = Graphics.pmMaskNotPen;
  pmMerge = Graphics.pmMerge;

  pmNotMerge = Graphics.pmNotMerge;
  pmMask = Graphics.pmMask;
  pmNotMask = Graphics.pmNotMask;
  pmXor = Graphics.pmXor;
  pmNotXor = Graphics.pmNotXor;

// TBrushStyle.
  bsSolid = Graphics.bsSolid;
  bsClear = Graphics.bsClear;
  bsHorizontal = Graphics.bsHorizontal;
  bsVertical = Graphics.bsVertical;

  bsFDiagonal = Graphics.bsFDiagonal;
  bsBDiagonal = Graphics.bsBDiagonal;
  bsCross = Graphics.bsCross;
  bsDiagCross = Graphics.bsDiagCross;

// Several define from unit Consts
const
  glsAllFilter: string = {$ifndef FPC}sAllFilter{$else}'all'{$endif};
  {$IFDEF FPC}
  SMsgDlgWarning = 'Warnung';
  SMsgDlgError = 'Fehler';
  SMsgDlgInformation = 'Information';
  SMsgDlgConfirm = 'Best�igen';
  SMsgDlgYes = '&Ja';
  SMsgDlgNo = '&Nein';
  SMsgDlgOK = 'OK';
  SMsgDlgCancel = 'Abbrechen';
  SMsgDlgHelp = '&Hilfe';
  SMsgDlgHelpNone = 'Keine Hilfe verfgbar';
  SMsgDlgHelpHelp = 'Hilfe';
  SMsgDlgAbort = '&Abbrechen';
  SMsgDlgRetry = '&Wiederholen';
  SMsgDlgIgnore = '&Ignorieren';
  SMsgDlgAll = '&Alles';
  SMsgDlgNoToAll = '&Alle Nein';
  SMsgDlgYesToAll = 'A&lle Ja';
  {$ENDIF}

function GLPoint(const x, y : Integer) : TGLPoint;

{: Builds a TColor from Red Green Blue components. }
function RGB(const r, g, b : Byte) : TColor;

function GLRect(const aLeft, aTop, aRight, aBottom : Integer) : TGLRect;
{: Increases or decreases the width and height of the specified rectangle.<p>
   Adds dx units to the left and right ends of the rectangle and dy units to
   the top and bottom. }
procedure InflateGLRect(var aRect : TGLRect; dx, dy : Integer);
procedure IntersectGLRect(var aRect : TGLRect; const rect2 : TGLRect);

{: Pops up a simple dialog with msg and an Ok button. }
procedure InformationDlg(const msg : String);
{: Pops up a simple question dialog with msg and yes/no buttons.<p>
   Returns True if answer was "yes". }
function QuestionDlg(const msg : String) : Boolean;
{: Posp a simple dialog with a string input. }
function InputDlg(const aCaption, aPrompt, aDefault : String) : String;

{: Pops up a simple save picture dialog. }
function SavePictureDialog(var aFileName : String; const aTitle : String = '') : Boolean;
{: Pops up a simple open picture dialog. }
function OpenPictureDialog(var aFileName : String; const aTitle : String = '') : Boolean;

{: Returns True if the application has been terminated. }
function ApplicationTerminated : Boolean;

procedure RaiseLastOSError;

{$IFDEF GLS_DELPHI_4_DOWN}
procedure FreeAndNil(var anObject);
{$ENDIF}

{: Number of pixels per logical inch along the screen width for the device.<p>
   Under Win32 awaits a HDC and returns its LOGPIXELSX. }
function GetDeviceLogicalPixelsX(device : Cardinal) : Integer;
{: Number of bits per pixel for the current desktop resolution. }
function GetCurrentColorDepth : Integer;
{: Returns the number of color bits associated to the given pixel format. }
function PixelFormatToColorBits(aPixelFormat : TPixelFormat) : Integer;

{: Returns the bitmap's scanline for the specified row. }
function BitmapScanLine(aBitmap : TGLBitmap; aRow : Integer) : Pointer;

{: Suspends thread execution for length milliseconds.<p>
   If length is zero, only the remaining time in the current thread's time
   slice is relinquished. }
procedure Sleep(length : Cardinal);

{: Returns the current value of the highest-resolution counter.<p>
   If the platform has none, should return a value derived from the highest
   precision time reference available, avoiding, if possible, timers that
   allocate specific system resources. }
procedure QueryPerformanceCounter(var val : Int64);
{: Returns the frequency of the counter used by QueryPerformanceCounter.<p>
   Return value is in ticks per second (Hz), returns False if no precision
   counter is available. }
function QueryPerformanceFrequency(var val : Int64) : Boolean;

{: Starts a precision timer.<p>
   Returned value should just be considered as 'handle', even if it ain't so.
   Default platform implementation is to use QueryPerformanceCounter and
   QueryPerformanceFrequency, if higher precision references are available,
   they should be used. The timer will and must be stopped/terminated/released
   with StopPrecisionTimer. }
function StartPrecisionTimer : Int64;
{: Computes time elapsed since timer start.<p>
   Return time lap in seconds. }
function PrecisionTimerLap(const precisionTimer : Int64) : Double;
{: Computes time elapsed since timer start and stop timer.<p>
   Return time lap in seconds. }
function StopPrecisionTimer(const precisionTimer : Int64) : Double;
{: Returns the number of CPU cycles since startup.<p>
   Use the similarly named CPU instruction. }
function RDTSC : Int64;

procedure GLLoadBitmapFromInstance(ABitmap: TBitmap; AName: string);
function GLOKMessageBox(const Text, Caption: string): Integer;
procedure ShowHTMLUrl(Url: String);
procedure GLShowCursor(AShow: boolean);
procedure GLSetCursorPos(AScreenX, AScreenY: integer);
procedure GLGetCursorPos(var point: TGLPoint);
function GLGetScreenWidth:integer;
function GLGetScreenHeight:integer;
function GLGetTickCount:int64;

function ColorToString(Color: TColor): string;

// StrUtils.pas
function AnsiStartsText(const ASubText, AText: string): Boolean;

// Classes.pas
function IsSubComponent(const AComponent: TComponent): Boolean;
procedure MakeSubComponent(const AComponent: TComponent; const Value: Boolean);

// SysUtils.pas
function StrToFloatDef(const S: string; const Default: Extended): Extended; overload;
{$IFDEF FPC_HAS_TYPE_EXTENDED}
function TryStrToFloat(const S: string; out Value: Extended): Boolean; overload;
{$ENDIF}
function TryStrToFloat(const S: string; out Value: Double): Boolean; overload;
function TryStrToFloat(const S: string; out Value: Single): Boolean; overload;

// Math.pas
function IsNan(const AValue: Double): Boolean; overload;
function IsNan(const AValue: Single): Boolean; overload;
{$IFDEF FPC_HAS_TYPE_EXTENDED}
function IsNan(const AValue: Extended): Boolean; overload;
{$ENDIF}
function IsInfinite(const AValue: Double): Boolean;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
uses
  ShellApi;
{$ENDIF}
{$IFDEF UNIX}

{$ENDIF}

var
   vInvPerformanceCounterFrequency : Double;
   vInvPerformanceCounterFrequencyReady : Boolean = False;

function IsSubComponent(const AComponent: TComponent): Boolean;
begin
{$IFDEF GLS_DELPHI_5_DOWN}
  Result := False; // AFAIK Delphi 5 does not know what is a SubComponent.
{$ELSE}
  Result := (csSubComponent in AComponent.ComponentStyle);
{$ENDIF}
end;

procedure MakeSubComponent(const AComponent: TComponent; const Value: Boolean);
begin
{$IFDEF GLS_DELPHI_5_DOWN}
 // AFAIK Delphi 5 does not know what is a SubComponent, so ignore this.
{$ELSE}
  AComponent.SetSubComponent(Value);
{$ENDIF}
end;

function StrToFloatDef(const S: string; const Default: Extended): Extended;
begin
{$IFDEF GLS_DELPHI_5_DOWN}
  Removed code, because we don't need it AND it's probably copyrighted
  regards, crossbuilder
{$ELSE}
  Result := SysUtils.StrToFloatDef(S, Default);
{$ENDIF}
end;

{$IFDEF FPC_HAS_TYPE_EXTENDED}
function TryStrToFloat(const S: string; out Value: Extended): Boolean;
begin
{$IFDEF GLS_DELPHI_5_DOWN}
  Removed code, because we don't need it AND it's probably copyrighted
  regards, crossbuilder
{$ELSE}
  Result := SysUtils.TryStrToFloat(S, Value);
{$ENDIF}
end;
{$ENDIF}

function TryStrToFloat(const S: string; out Value: Double): Boolean;
{$IFDEF GLS_DELPHI_5_DOWN}
  Removed code, because we don't need it AND it's probably copyrighted
  regards, crossbuilder
{$ELSE}
begin
  Result := SysUtils.TryStrToFloat(S, Value);
end;
{$ENDIF}


function TryStrToFloat(const S: string; out Value: Single): Boolean;
{$IFDEF GLS_DELPHI_5_DOWN}
  Removed code, because we don't need it AND it's probably copyrighted
  regards, crossbuilder
{$ELSE}
begin
  Result := SysUtils.TryStrToFloat(S, Value);
end;
{$ENDIF}

function IsNan(const AValue: Single): Boolean;
begin
{$IFDEF GLS_DELPHI_5_DOWN}
  Removed code, because we don't need it AND it's probably copyrighted
  regards, crossbuilder
{$ELSE}
  Result := Math.IsNan(AValue);
{$ENDIF}
end;

function IsNan(const AValue: Double): Boolean;
begin
{$IFDEF GLS_DELPHI_5_DOWN}
  Removed code, because we don't need it AND it's probably copyrighted
  regards, crossbuilder
{$ELSE}
  Result := Math.IsNan(AValue);
{$ENDIF}
end;

{$IFDEF FPC_HAS_TYPE_EXTENDED}
function IsNan(const AValue: Extended): Boolean;
{$IFDEF GLS_DELPHI_5_DOWN}
  Removed code, because we don't need it AND it's probably copyrighted
  regards, crossbuilder
{$ELSE}
begin
  Result := Math.IsNan(AValue);
{$ENDIF}
end;
{$ENDIF}

function IsInfinite(const AValue: Double): Boolean;
begin
{$IFDEF GLS_DELPHI_5_DOWN}
  Removed code, because we don't need it AND it's probably copyrighted
  regards, crossbuilder
{$ELSE}
  Result := Math.IsInfinite(AValue);
{$ENDIF}
end;

function ColorToString(Color: TColor): string;
begin
  // Taken from Delphi7 Graphics.pas
  if not ColorToIdent(Color, Result) then
    FmtStr(Result, '%s%.8x', [HexDisplayPrefix, Color]);
end;

function AnsiStartsText(const ASubText, AText: string): Boolean;
{$IFNDEF GLS_DELPHI_5_DOWN}
begin
  Result := StrUtils.AnsiStartsText(ASubText, AText);
end;
{$ELSE}
var
{$IFDEF MSWINDOWS}
  P: PChar;
{$ENDIF}
  L, L2: Integer;
begin
{$IFDEF MSWINDOWS}
  P := PChar(AText);
{$ENDIF}
  L := Length(ASubText);
  L2 := Length(AText);
  if L > L2 then
    Result := False
  else
{$IFDEF MSWINDOWS}
    Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
      P, L, PChar(ASubText), L) = 2;
{$ENDIF}
{$IFDEF UNIX}
    Result := WideSameText(ASubText, Copy(AText, 1, L));
{$ENDIF}
end;
{$ENDIF}


procedure GLLoadBitmapFromInstance(ABitmap: TBitmap; AName: string);
begin
{$IFDEF MSWINDOWS}
  ABitmap.Handle := LoadBitmap(HInstance, PChar(AName));
{$ENDIF}
{$IFDEF UNIX}
  {$IFNDEF FPC}
  ABitmap.LoadFromResourceName(HInstance, PChar(AName));
  {$ELSE}
  ABitmap.LoadFromLazarusResource(AName);
  {$ENDIF}
{$ENDIF}
end;

function GLOKMessageBox(const Text, Caption: string): Integer;
begin
  result := Application.MessageBox(PChar(Text),PChar(Caption),MB_OK);
end;

procedure GLShowCursor(AShow: boolean);
begin
{$IFDEF MSWINDOWS}
  ShowCursor(AShow);
{$ENDIF}
{$IFDEF UNIX}
  {$MESSAGE Warn 'ShowCursor: Needs to be implemented'}
{$ENDIF}
end;

procedure GLSetCursorPos(AScreenX, AScreenY: integer);
begin
{$IFDEF MSWINDOWS}
  SetCursorPos(AScreenX, AScreenY);
{$ENDIF}
{$IFDEF UNIX}
  {$MESSAGE Warn 'SetCursorPos: Needs to be implemented'}
{$ENDIF}
end;

procedure GLGetCursorPos(var point: TGLPoint);
begin
{$IFDEF MSWINDOWS}
  GetCursorPos(point);
{$ENDIF}
{$IFDEF UNIX}
  {$MESSAGE Warn 'GetCursorPos: Needs to be implemented'}
{$ENDIF}
end;

function GLGetScreenWidth:integer;
begin
  result := Screen.Width;
end;

function GLGetScreenHeight:integer;
begin
  result := Screen.Height;
end;

function GLGetTickCount:int64;
begin
  result:=GetTickCount;
end;

{$IFDEF UNIX}
function QueryCombo(const ACaption, APrompt: string; Alist:TStringList;
                          var Index: integer; var Value: string): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Combo: TComboBox;
  Dialogfrms: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      Dialogfrms := Point(Canvas.TextWidth('L'),Canvas.TextHeight('R'));
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, Dialogfrms.X, 4);
      ClientHeight := MulDiv(63, Dialogfrms.Y, 8);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        AutoSize := True;
        Left := MulDiv(8, Dialogfrms.X, 4);
        Top := MulDiv(8, Dialogfrms.Y, 8);
        Caption := APrompt;
      end;
      Combo := TComboBox.Create(Form);
      with Combo do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := MulDiv(19, Dialogfrms.Y, 8);
        Width := MulDiv(164, Dialogfrms.X, 4);
        DropDownCount := 3;
        Items.AddStrings(AList);
        Combo.ItemIndex := index;
      end;
      ButtonTop := MulDiv(41, Dialogfrms.Y, 8);
      ButtonWidth := MulDiv(50, Dialogfrms.X, 4);
      ButtonHeight := MulDiv(14, Dialogfrms.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, Dialogfrms.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
        TabOrder := 0;
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, Dialogfrms.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      if ShowModal = mrOk then
      begin
        Value := Combo.Text;
        index := Combo.ItemIndex;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

resourcestring
  sFileName = '/tmp/delete-me.txt';

// Code inspired from unit Misc.pas of TPlot component of Mat Ballard
function CheckForRPM(AnRPM: String): String;
var
  TmpFile: TStringList;
begin
  Result := '';
  TmpFile := TStringList.Create;
  FPSystem(PChar('rpm -ql ' + AnRPM + ' > ' + sFileName));
  TmpFile.LoadFromFile(sFileName);
  if (Length(TmpFile.Strings[0]) > 0) then
    if (Pos('not installed', TmpFile.Strings[0]) = 0) then
      Result := TmpFile.Strings[0];
  DeleteFile(sFileName);
  TmpFile.Free;
end;

function GetBrowser: String;
var
  Index: Integer;
  AProgram,
  ExeName: String;
  BrowserList: TStringList;
begin
{Get the $BROWSER environment variable:}
  ExeName := GetEnvironmentVariable('BROWSER');

  if (Length(ExeName) = 0) then
  begin
{Get the various possible browsers:}
    BrowserList := TStringList.Create;

    try
      if (FileExists('/usr/bin/konqueror')) then
        BrowserList.Add('/usr/bin/konqueror');

      AProgram := CheckForRPM('mozilla');
      if (Length(AProgram) > 0) then
        BrowserList.Add(AProgram);
      AProgram := CheckForRPM('netscape-common');
      if (Length(AProgram) > 0) then
        BrowserList.Add(AProgram);
      AProgram := CheckForRPM('opera');
      if (Length(AProgram) > 0) then
        BrowserList.Add(AProgram);
      AProgram := CheckForRPM('lynx');
      if (Length(AProgram) > 0) then
        BrowserList.Add(AProgram);
      AProgram := CheckForRPM('links');
      if (Length(AProgram) > 0) then
        BrowserList.Add(AProgram);

      Index := 0;
      if QueryCombo('Browser Selection', 'Which Web Browser Program To Use ?',
        BrowserList, Index, AProgram) then
      begin
        ExeName := AProgram;
        //Libc.putenv(PChar('BROWSER=' + ExeName));
      end;

    finally
      BrowserList.Free;
    end;
  end;

  Result := ExeName;
end;
{$ENDIF}

procedure ShowHTMLUrl(Url: String);
{$IFDEF UNIX}
var
  TheBrowser: String;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(Url), Nil, Nil, SW_SHOW);
{$ENDIF}
{$IFDEF UNIX}
  TheBrowser := GetBrowser;
{the ' &' means immediately continue:}
  if (Length(TheBrowser) > 0) then
    fpsystem(PChar(TheBrowser + ' ' + Url + ' &'));
{$ENDIF}
end;

// GLPoint
//
function GLPoint(const x, y : Integer) : TGLPoint;
begin
   Result.X:=x;
   Result.Y:=y;
end;

// RGB
//
function RGB(const r, g, b : Byte) : TColor;
begin
   Result:=(b shl 16) or (g shl 8) or r;
end;

// GLRect
//
function GLRect(const aLeft, aTop, aRight, aBottom : Integer) : TGLRect;
begin
   Result.Left:=aLeft;
   Result.Top:=aTop;
   Result.Right:=aRight;
   Result.Bottom:=aBottom;
end;

// InflateRect
//
procedure InflateGLRect(var aRect : TGLRect; dx, dy : Integer);
begin
   aRect.Left:=aRect.Left-dx;
   aRect.Right:=aRect.Right+dx;
   if aRect.Right<aRect.Left then
      aRect.Right:=aRect.Left;
   aRect.Top:=aRect.Top-dy;
   aRect.Bottom:=aRect.Bottom+dy;
   if aRect.Bottom<aRect.Top then
      aRect.Bottom:=aRect.Top;
end;

// IntersectGLRect
//
procedure IntersectGLRect(var aRect : TGLRect; const rect2 : TGLRect);
var
   a : Integer;
begin
   if (aRect.Left>rect2.Right) or (aRect.Right<rect2.Left)
      or (aRect.Top>rect2.Bottom) or (aRect.Bottom<rect2.Top) then begin
      // no intersection
      a:=0;
      aRect.Left:=a;
      aRect.Right:=a;
      aRect.Top:=a;
      aRect.Bottom:=a;
   end else begin
      if aRect.Left<rect2.Left then
         aRect.Left:=rect2.Left;
      if aRect.Right>rect2.Right then
         aRect.Right:=rect2.Right;
      if aRect.Top<rect2.Top then
         aRect.Top:=rect2.Top;
      if aRect.Bottom>rect2.Bottom then
         aRect.Bottom:=rect2.Bottom;
   end;
end;

// InformationDlg
//
procedure InformationDlg(const msg : String);
begin
   ShowMessage(msg);
end;

// QuestionDlg
//
function QuestionDlg(const msg : String) : Boolean;
begin
   Result:=(MessageDlg(msg, mtConfirmation, [mbYes, mbNo], 0)=mrYes);
end;

// InputDlg
//
function InputDlg(const aCaption, aPrompt, aDefault : String) : String;
begin
   Result:=InputBox(aCaption, aPrompt, aDefault);
end;

// SavePictureDialog
//
function SavePictureDialog(var aFileName : String; const aTitle : String = '') : Boolean;
var
   saveDialog : TSavePictureDialog;
begin
   saveDialog:=TSavePictureDialog.Create(Application);
   try
      with saveDialog do begin
         Options:=[ofHideReadOnly, ofNoReadOnlyReturn];
         if aTitle<>'' then
            Title:=aTitle;
         FileName:=aFileName;
         Result:=Execute;
         if Result then
            aFileName:=FileName;
      end;
   finally
      saveDialog.Free;
   end;
end;

// OpenPictureDialog
//
function OpenPictureDialog(var aFileName : String; const aTitle : String = '') : Boolean;
var
   openDialog : TOpenPictureDialog;
begin
   openDialog:=TOpenPictureDialog.Create(Application);
   try
      with openDialog do begin
         Options:=[ofHideReadOnly, ofNoReadOnlyReturn];
         if aTitle<>'' then
            Title:=aTitle;
         FileName:=aFileName;
         Result:=Execute;
         if Result then
            aFileName:=FileName;
      end;
   finally
      openDialog.Free;
   end;
end;

// ApplicationTerminated
//
function ApplicationTerminated : Boolean;
begin
   Result:=Application.Terminated;
end;

// RaiseLastOSError
//
{ TODO : Fix this. Currently it does nothing in lazarus. }
procedure RaiseLastOSError;
{$IFNDEF FPC}
var
   e : EGLOSError;
{$ENDIF}
begin
  {$IFNDEF FPC}
   e:=EGLOSError.Create('OS Error : '+SysErrorMessage(GetLastError));
   raise e;
   {$ENDIF}
end;

{$IFDEF GLS_DELPHI_4_DOWN}
// FreeAndNil
//
procedure FreeAndNil(var anObject);
var
  buf : TObject;
begin
  buf:=TObject(anObject);
  TObject(anObject):=nil;  // clear the reference before destroying the object
  buf.Free;
end;
{$ENDIF}

type
  TDeviceCapabilities = record
    Xdpi, Ydpi: integer;        // Number of pixels per logical inch.
    Depth: integer;             // The bit depth.
    NumColors: integer;         // Number of entries in the device's color table.
  end;

function GetDeviceCapabilities: TDeviceCapabilities;
{$IFDEF MSWINDOWS}
var
  Device: HDC;
begin
  Device := GetDC(0);
  try
    result.Xdpi := GetDeviceCaps(Device,LOGPIXELSX);
    result.Ydpi := GetDeviceCaps(Device,LOGPIXELSY);
    result.Depth := GetDeviceCaps(Device,BITSPIXEL);
    result.NumColors := GetDeviceCaps(Device,NUMCOLORS);
  finally
    ReleaseDC(0, Device);
  end;
end;
{$ENDIF}
{$IFDEF UNIX}
var
  Device: HDC;
const
  NUMCOLORS=24;
begin

  Device := GetDC(0);
  try
    result.Xdpi := GetDeviceCaps(Device,LOGPIXELSX);
    result.Ydpi := GetDeviceCaps(Device,LOGPIXELSY);
    result.Depth := GetDeviceCaps(Device,BITSPIXEL);
    result.NumColors := GetDeviceCaps(Device,NUMCOLORS);
  finally
    ReleaseDC(0, Device);
  end;
end;
{$ENDIF}

// GetDeviceLogicalPixelsX
//
function GetDeviceLogicalPixelsX(device : Cardinal) : Integer;
begin
  result := GetDeviceCapabilities().Xdpi;
end;

// GetCurrentColorDepth
//

function GetCurrentColorDepth : Integer;
begin
  result := GetDeviceCapabilities().Depth;
end;

// PixelFormatToColorBits
//
function PixelFormatToColorBits(aPixelFormat : TPixelFormat) : Integer;
begin
   case aPixelFormat of
      pfCustom {$IFDEF WINDOWS}, pfDevice{$ENDIF} :  // use current color depth
         Result:=GetCurrentColorDepth;
      pf1bit  : Result:=1;
{$IFDEF WINDOWS}
      pf4bit  : Result:=4;
      pf15bit : Result:=15;
{$ENDIF}
      pf8bit  : Result:=8;
      pf16bit : Result:=16;
      pf32bit : Result:=32;
   else
      Result:=24;
   end;
end;

// BitmapScanLine
//
function BitmapScanLine(aBitmap : TGLBitmap; aRow : Integer) : Pointer;
begin
{$IFDEF FPC}
   //Assert(False, 'BitmapScanLine unsupported');
   Result:=nil;
   raise exception.create('BitmapScanLine unsupported');
{$ELSE}
   Result:=aBitmap.ScanLine[aRow];
{$ENDIF}
end;


// Sleep
//
procedure Sleep(length : Cardinal);
begin
  Sleep(Length);
end;

// QueryPerformanceCounter
//
{$IFDEF UNIX}
  {$IFDEF FPC}
   var
     vProgStartSecond : int64;

   procedure Init_vProgStartSecond;
   var
     tz:timeval;
   begin
     fpgettimeofday(@tz,nil);
     vProgStartSecond:=tz.tv_sec;
   end;
  {$ENDIF}
{$ENDIF}

procedure QueryPerformanceCounter(var val : Int64);
{$IFDEF WINDOWS}
begin
   Windows.QueryPerformanceCounter(val);
{$ELSE}
   {$IFDEF fpc}
   var
     tz:timeval;
   begin
     //val:=round(now*MSecsPerDay);
     fpgettimeofday(@tz,nil);
     val:=tz.tv_sec-vProgStartSecond;
     val:=val*1000000;
     val:=val+tz.tv_usec;
   {$ELSE}
   begin
     val:=RDTSC;
   {$ENDIF}
{$ENDIF}
end;

// QueryPerformanceFrequency
//
function QueryPerformanceFrequency(var val : Int64) : Boolean;
{$IFDEF WINDOWS}
begin
   Result:=Boolean(Windows.QueryPerformanceFrequency(val));
{$ELSE}
  {$IFDEF FPC}
  begin
    val:=1000000;
  {$ELSE}
  var
    startCycles, endCycles : Int64;
    aTime, refTime : TDateTime;
  begin
   aTime:=Now;
   while aTime=Now do ;
   startCycles:=RDTSC;
   refTime:=Now;
   while refTime=Now do ;
   endCycles:=RDTSC;
   aTime:=Now;
   val:=Round((endCycles-startCycles)/((aTime-refTime)*(3600*24)));
  {$ENDIF}
  Result:=True;
{$ENDIF}
end;

// StartPrecisionTimer
//
function StartPrecisionTimer : Int64;
begin
   QueryPerformanceCounter(Result);
end;

// PrecisionTimeLap
//
function PrecisionTimerLap(const precisionTimer : Int64) : Double;
begin
   // we can do this, because we don't really stop anything
   Result:=StopPrecisionTimer(precisionTimer);
end;

// StopPrecisionTimer
//
function StopPrecisionTimer(const precisionTimer : Int64) : Double;
var
   cur, freq : Int64;
begin
   QueryPerformanceCounter(cur);
   if not vInvPerformanceCounterFrequencyReady then begin
      QueryPerformanceFrequency(freq);
      vInvPerformanceCounterFrequency:=1.0/freq;
      vInvPerformanceCounterFrequencyReady:=True;
   end;
   Result:=(cur-precisionTimer)*vInvPerformanceCounterFrequency;
end;

// RDTSC
//
function RDTSC : Int64;
{$IFDEF FPC}
begin
  raise exception.create('Using GLCrossPlatform.RDTSC is a bad idea!');
end;
{$ELSE}
asm
   db $0f, $31
end;
{$ENDIF}


initialization
{$IFDEF UNIX}
  {$IFDEF FPC}
  Init_vProgStartSecond;
  {$ENDIF}
{$ENDIF}
end.
