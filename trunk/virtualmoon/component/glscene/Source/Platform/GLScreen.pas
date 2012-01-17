//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLScreen<p>

   Routines to interact with the screen/desktop.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>04/11/10 - DaStr - Added Delphi5 and Delphi6 compatibility   
      <li>06/06/10 - Yar - Fixed warnings
      <li>13/04/10 - Yar - Fixed conditional for delphi (thanks mif) 
      <li>07/01/10 - DaStr - Enhanced cross-platform compatibility (thanks Predator)
      <li>17/12/09 - DaStr - Added screen utility functions from
                              GLCrossPlatform.pas (thanks Predator)
      <li>07/11/09 - DaStr - Improved FPC compatibility and moved to the /Source/Platform/
                             directory (BugtrackerID = 2893580) (thanks Predator)
      <li>23/03/07 - DaStr - Added explicit pointer dereferencing
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
      <li>03/07/04 - LR - Suppress CurrentScreenColorDepth because there are in GLCrossPlatform
      <li>24/07/03 - EG - Video modes now read on request only, removed
                          the non-standard low-res video modes
      <li>27/09/02 - EG - Added Ability to set display frequency
      <li>27/07/01 - EG - Removed the "absolute" in RestoreDefaultMode
      <li>08/02/00 - EG - TLowResMode & TVideoMode packed (wins 5 kb)
      <li>06/02/00 - EG - Javadocisation, added "default"s to properties
   </ul></font>
}
unit GLScreen;

// GLScreen    - This units contains routines to interact with the screen/desktop.
// Version     - 0.0.8
// Last Change - 30. September 1998
// for more information see help file

interface

{$i GLScene.inc}

uses
   {$IFDEF MSWINDOWS} Windows,{$ENDIF}
   {$IFDEF GLS_X11_SUPPORT} x,xlib,xf86vmode,{$ENDIF}
   {$IFDEF FPC} LCLVersion, {$ENDIF}
   Classes, VectorGeometry, GLCrossPlatform;

const
   MaxVideoModes = 200;
{$IFNDEF FPC}
   lcl_release = 0;
{$ENDIF}

type

   TResolution = 0..MaxVideoModes;

   // window attributes
   TWindowAttribute  = (woDesktop, woStayOnTop, woTransparent);
   TWindowAttributes = set of TWindowAttribute;

   // window-to-screen fitting
   TWindowFitting = (wfDefault, wfFitWindowToScreen, wfFitScreenToWindow);

   // TDisplayOptions
   //
   TDisplayOptions = class(TPersistent)
      private
         FFullScreen       : Boolean;
         FScreenResolution : TResolution;
         FWindowAttributes : TWindowAttributes;
         FWindowFitting    : TWindowFitting;
      public
         procedure Assign(Source: TPersistent); override;
      published
         property FullScreen: Boolean read FFullScreen write FFullScreen default False;
         property ScreenResolution: TResolution read FScreenResolution write FScreenResolution default 0;
         property WindowAttributes: TWindowAttributes read FWindowAttributes write FWindowAttributes default [];
         property WindowFitting: TWindowFitting read FWindowFitting write FWindowFitting default wfDefault;
   end;

   TVideoMode = packed record
      Width : Word;
      Height : Word;
      ColorDepth  : Byte;
      MaxFrequency : Byte;
      Description : String;
   end;
   PVideoMode = ^TVideoMode;

function GetIndexFromResolution(XRes,YRes,BPP: Integer): TResolution;


procedure ReadVideoModes;

//: Changes to the video mode given by 'Index'
function SetFullscreenMode(modeIndex : TResolution; displayFrequency : Integer = 0) : Boolean;

{$IFDEF MSWINDOWS}
procedure ReadScreenImage(Dest: HDC; DestLeft, DestTop: Integer; SrcRect: TRectangle);
{$ENDIF}
procedure RestoreDefaultMode;


procedure GLShowCursor(AShow: boolean);
procedure GLSetCursorPos(AScreenX, AScreenY: integer);
procedure GLGetCursorPos(var point: TGLPoint);
function GLGetScreenWidth:integer;
function GLGetScreenHeight:integer;

var
   vNumberVideoModes  : Integer = 0;
   vCurrentVideoMode  : Integer = 0;
   {$IFDEF MSWINDOWS}
   vVideoModes        : array of TVideoMode;
   {$ENDIF} //Unix
   {$IFDEF GLS_X11_SUPPORT}
   vDisplay: PDisplay;
   vScreenModeChanged : boolean;
   vVideoModes        : array of PXF86VidModeModeInfo;
   vDesktop           : TXF86VidModeModeInfo;
   {$ENDIF}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

uses
{$IFDEF GLS_DELPHI_XE2_UP}
  VCL.Forms,
{$ELSE}
  Forms,
{$ENDIF}
  SysUtils;

type TLowResMode = packed record
                     Width : Word;
                     Height : Word;
                     ColorDepth : Byte;
                   end;

const NumberLowResModes = 15;
{$IFDEF MSWINDOWS}
      LowResModes       : array [0..NumberLowResModes-1] of TLowResMode =
      (
       (Width:320;Height:200;ColorDepth: 8),(Width:320;Height:200;ColorDepth:15),(Width:320;Height:200;ColorDepth:16),
       (Width:320;Height:200;ColorDepth:24),(Width:320;Height:200;ColorDepth:32),
       (Width:400;Height:300;ColorDepth: 8),(Width:400;Height:300;ColorDepth:15),(Width:400;Height:300;ColorDepth:16),
       (Width:400;Height:300;ColorDepth:24),(Width:400;Height:300;ColorDepth:32),
       (Width:512;Height:384;ColorDepth: 8),(Width:512;Height:384;ColorDepth:15),(Width:512;Height:384;ColorDepth:16),
       (Width:512;Height:384;ColorDepth:24),(Width:512;Height:384;ColorDepth:32)
      );
{$ENDIF}
// Assign
//
procedure TDisplayOptions.Assign(Source: TPersistent);
begin
   if Source is TDisplayOptions then begin
      FFullScreen       :=TDisplayOptions(Source).FFullScreen;
      FScreenResolution :=TDisplayOptions(Source).FScreenResolution;
      FWindowAttributes :=TDisplayOptions(Source).FWindowAttributes;
      FWindowFitting    :=TDisplayOptions(Source).FWindowFitting;
   end else inherited Assign(Source);
end;

// GetIndexFromResolution
//
function GetIndexFromResolution(XRes,YRes,BPP: Integer): TResolution;

// Determines the index of a screen resolution nearest to the
// given values. The returned screen resolution is always greater
// or equal than XRes and YRes or, in case the resolution isn't
// supported, the value 0, which indicates the default mode.

var
   I : Integer;
   XDiff, YDiff: Integer;
   {$IFDEF MSWINDOWS}
   CDiff : Integer;
   {$ENDIF}
begin
   ReadVideoModes;
   // prepare result in case we don't find a valid mode
   Result:=0;
   // set differences to maximum
   XDiff:=9999; YDiff:=9999;
   {$IFDEF MSWINDOWS}
   CDiff:=99;
   {$ENDIF}
   for I:=1 to vNumberVideomodes-1 do
   {$IFDEF MSWINDOWS}
   with vVideoModes[I] do begin
     if     (Width  >= XRes) and ((Width-XRes)  <= XDiff)
        and (Height >= YRes) and ((Height-YRes) <= YDiff)
        and (ColorDepth >= BPP) and ((ColorDepth-BPP) <= CDiff) then begin
         XDiff:=Width-XRes;
         YDiff:=Height-YRes;
         CDiff:=ColorDepth-BPP;
         Result:=I;
        end;
     end;
     {$ENDIF}
   {$IFDEF GLS_X11_SUPPORT}
    with vVideoModes[ i ]^ do begin
     if     (hDisplay  >= XRes) and ((hDisplay-XRes)  <= XDiff)
        and (vDisplay >= YRes) and ((vDisplay-YRes) <= YDiff)
        then begin
         XDiff:=hDisplay-XRes;
         YDiff:=vDisplay-YRes;
         Result:=I;
        end;
     end;
    {$ENDIF}
    {$IFDEF Darwin}
      begin
       {$MESSAGE Warn 'Needs to be implemented'}
      end;
     {$ENDIF}
end;


// TryToAddToList
//
{$IFDEF MSWINDOWS}
procedure TryToAddToList(deviceMode : TDevMode);
// Adds a video mode to the list if it's not a duplicate and can actually be set.
var
   i : Integer;
   vm : PVideoMode;
begin
   // See if this is a duplicate mode (can happen because of refresh
   // rates, or because we explicitly try all the low-res modes)
   for i:=1 to vNumberVideoModes-1 do with DeviceMode do begin
      vm:=@vVideoModes[i];
      if (    (dmBitsPerPel=vm^.ColorDepth)
          and (dmPelsWidth =vm^.Width)
          and (dmPelsHeight=vm^.Height)) then begin
         // it's a duplicate mode, higher frequency?
         if dmDisplayFrequency>vm^.MaxFrequency then
            vm^.MaxFrequency:=dmDisplayFrequency;
         Exit;
      end;
   end;

   // do a mode set test (doesn't actually do the mode set, but reports whether it would have succeeded).
   if ChangeDisplaySettings(DeviceMode, CDS_TEST or CDS_FULLSCREEN)<>DISP_CHANGE_SUCCESSFUL then Exit;

   // it's a new, valid mode, so add this to the list
   vm:=@vVideoModes[vNumberVideomodes];
   with DeviceMode do begin
      vm^.ColorDepth:=dmBitsPerPel;
      vm^.Width:=dmPelsWidth;
      vm^.Height:=dmPelsHeight;
      vm^.MaxFrequency:=dmDisplayFrequency;
      vm^.Description:=Format('%d x %d, %d bpp',
                             [dmPelsWidth, dmPelsHeight, dmBitsPerPel]);
   end;
   Inc(vNumberVideomodes);
   {$ENDIF}
{$IFDEF GLS_X11_SUPPORT}
procedure TryToAddToList(); // Without input parameters.
begin
  XF86VidModeGetAllModeLines( vDisplay, vCurrentVideoMode, @vNumberVideoModes, @vVideoModes[0] );
{$ENDIF}
{$IFDEF Darwin}
procedure TryToAddToList(); // Without input parameters.
begin
  {$MESSAGE Warn 'Needs to be implemented'}
{$ENDIF}
end;


// ReadVideoModes
//
procedure ReadVideoModes;
{$IFDEF MSWINDOWS}
var
   I, ModeNumber : Integer;
   done          : Boolean;
   DeviceMode    : TDevMode;
   DeskDC        : HDC;
begin
   if vNumberVideoModes>0 then Exit;

   SetLength(vVideoModes, MaxVideoModes);
   vNumberVideoModes:=1;

   // prepare 'default' entry
   DeskDC:=GetDC(0);
   with vVideoModes[0] do try
      ColorDepth:=GetDeviceCaps(DeskDC, BITSPIXEL)*GetDeviceCaps(DeskDC,PLANES);
      Width:=Screen.Width;
      Height:=Screen.Height;
      Description:='default';
   finally
      ReleaseDC(0, DeskDC);
   end;

   // enumerate all available video modes
   ModeNumber:=0;
   repeat
      done:=not EnumDisplaySettings(nil,ModeNumber,DeviceMode);
      TryToAddToList(DeviceMode);
      Inc(ModeNumber);
   until (done or (vNumberVideoModes >= MaxVideoModes));

  // low-res modes don't always enumerate, ask about them explicitly
  with DeviceMode do begin
    dmBitsPerPel:=8;
    dmPelsWidth:=42;
    dmPelsHeight:=37;
    dmFields:=DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
    // make sure the driver doesn't just answer yes to all tests
    if ChangeDisplaySettings(DeviceMode,CDS_TEST or CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL then begin
      I:=0;
      while (I < NumberLowResModes-1) and (vNumberVideoModes < MaxVideoModes) do begin
        dmSize:=Sizeof(DeviceMode);
        with LowResModes[I] do begin
           dmBitsPerPel:=ColorDepth;
           dmPelsWidth:=Width;
           dmPelsHeight:=Height;
        end;
        dmFields:=DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
        TryToAddToList(DeviceMode);
        Inc(I);
      end;
    end;
  end;
{$ENDIF}
{$IFDEF GLS_X11_SUPPORT}
var
   i,j:Integer;
begin
  SetLength(vVideoModes, MaxVideoModes);
  //if error usr/bin/ld: cannot find -lXxf86vm
  //then sudo apt-get install libXxf86vm-dev

  // Connect to XServer
  vDisplay := XOpenDisplay( nil );
  if not Assigned( vDisplay ) Then
     Assert(False, 'Not conected with X Server');
  vCurrentVideoMode := DefaultScreen( vDisplay );

  // Check support XF86VidMode Extension
  {$IFNDEF GLS_DELPHI_5}
    {$IF (FPC_VERSION = 2) and (FPC_RELEASE < 7)}
    if not XF86VidModeQueryExtension( vDisplay, @i, @j ) then
    {$ELSE}
    if XF86VidModeQueryExtension( vDisplay, @i, @j )=0 then
    {$IFEND}
      Assert(False, 'XF86VidMode Extension not support');
  {$ENDIF}
  
  // Get Current Settings
  if not vScreenModeChanged then
    if XF86VidModeGetModeLine( vDisplay, vCurrentVideoMode, @vDesktop.dotclock,
      PXF86VidModeModeLine( PtrUInt(@vDesktop) + SizeOf( vDesktop.dotclock ) ) ) then
    TryToAddToList;
  XCloseDisplay(vDisplay);
{$ENDIF}
{$IFDEF Darwin}
begin
  {$MESSAGE Warn 'ReadVideoModes not yet implemented for Darwin platforms'}
{$ENDIF}
end;

// SetFullscreenMode
//
function SetFullscreenMode(modeIndex : TResolution; displayFrequency : Integer = 0) : Boolean;
{$IFDEF MSWINDOWS}
var
   deviceMode : TDevMode;
begin
   ReadVideoModes;
   FillChar(deviceMode, SizeOf(deviceMode), 0);
   with deviceMode do begin
      dmSize:=SizeOf(DeviceMode);
      dmBitsPerPel:=vVideoModes[ModeIndex].ColorDepth;
      dmPelsWidth:=vVideoModes[ModeIndex].Width;
      dmPelsHeight:=vVideoModes[ModeIndex].Height;
      dmFields:=DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
      if displayFrequency>0 then begin
         dmFields:=dmFields or DM_DISPLAYFREQUENCY;
         if displayFrequency>vVideoModes[ModeIndex].MaxFrequency then
            displayFrequency:=vVideoModes[ModeIndex].MaxFrequency;
         dmDisplayFrequency:=displayFrequency;
      end;
   end;
   Result:=ChangeDisplaySettings(deviceMode, CDS_FULLSCREEN) = DISP_CHANGE_SUCCESSFUL;
   if Result then
      vCurrentVideoMode:=ModeIndex;
{$ENDIF}
{$IFDEF GLS_X11_SUPPORT}
var
  vSettings : TXF86VidModeModeInfo;
  wnd:TWindow;
begin
  ReadVideoModes;
  vDisplay := XOpenDisplay( nil );
  vSettings := vVideoModes[modeIndex]^;
    if ( vSettings.hDisplay <> vDesktop.hDisplay ) and
       ( vSettings.vDisplay <> vDesktop.vDisplay ) then
    begin

      //vsettings.vtotal:=vsettings.vdisplay;
      XF86VidModeSwitchToMode( vDisplay, vCurrentVideoMode, @vSettings );
      XF86VidModeSetViewPort( vDisplay, vCurrentVideoMode, 0, 0 );
      wnd:=XDefaultRootWindow( vDisplay);
      XGrabPointer(vDisplay,wnd,  true,PointerMotionMask+ButtonReleaseMask,GrabModeAsync,GrabModeAsync, wnd,none,0);
      vScreenModeChanged:=true;
    end else
    begin
    // Restore
       XF86VidModeSwitchToMode(vDisplay, vCurrentVideoMode, @vDesktop);
       vScreenModeChanged:=false;
    end;
    // Disconnect to XServer else settings not accept
    XCloseDisplay(vDisplay);
    Result:= vScreenModeChanged;
{$ENDIF}
{$IFDEF Darwin}
begin
  {$MESSAGE Warn 'Needs to be implemented'}
{$ENDIF}
end;

// ReadScreenImage
//
{$IFDEF MSWINDOWS}
procedure ReadScreenImage(Dest: HDC; DestLeft, DestTop: Integer; SrcRect: TRectangle);
var
   screenDC : HDC;
begin
   screenDC:=GetDC(0);
   try
      GDIFlush;
      BitBlt(Dest,DestLeft,DestTop,SrcRect.Width,SrcRect.Height,ScreenDC,SrcRect.Left,SrcRect.Top,SRCCOPY);
   finally
      ReleaseDC(0,ScreenDC);
   end;
end;
{$ENDIF}

// RestoreDefaultMode
//
procedure RestoreDefaultMode;
{$IFDEF MSWINDOWS}
var
   t : PDevMode;
begin
   t:=nil;
   ChangeDisplaySettings(t^, CDS_FULLSCREEN);
{$ENDIF}
{$IFDEF GLS_X11_SUPPORT}
begin
  //if vCurrentVideoMode=0 then
  ReadVideoModes;
  vDisplay := XOpenDisplay( nil );
  XF86VidModeSwitchToMode(vDisplay, vCurrentVideoMode, @vDesktop);
  vScreenModeChanged:=false;
  XCloseDisplay(vDisplay);
{$ENDIF}
{$IFDEF Darwin}
begin
  {$MESSAGE Warn 'Needs to be implemented'}
{$ENDIF}
end;

procedure GLShowCursor(AShow: boolean);
begin
{$IFDEF MSWINDOWS}
  ShowCursor(AShow);
{$ENDIF}
{$IFDEF UNIX}
  {$MESSAGE Warn 'ShowCursor: Needs to be implemented'}
  //Use Form.Cursor:=crNone
{$ENDIF}
end;

procedure GLSetCursorPos(AScreenX, AScreenY: integer);
{$IFDEF MSWINDOWS}
begin
  SetCursorPos(AScreenX, AScreenY);
{$ENDIF}
{$IFDEF GLS_X11_SUPPORT}
var
  dpy: PDisplay;
  root: TWindow;
begin
  dpy := XOpenDisplay(nil);
  root := RootWindow(dpy, DefaultScreen(dpy));
  XWarpPointer(dpy, none, root, 0, 0, 0, 0, AScreenX, AScreenY);
  XCloseDisplay(dpy);
{$ENDIF}
{$IFDEF Darwin}
begin
  {$MESSAGE Warn 'Needs to be implemented'}
{$ENDIF}
end;

procedure GLGetCursorPos(var point: TGLPoint);
{$IFDEF MSWINDOWS}
begin
  GetCursorPos(point);
{$ENDIF}
{$IFDEF GLS_X11_SUPPORT}
var
  dpy: PDisplay;
  root, child : TWindow;
  rootX, rootY, winX, winY : Integer;
  xstate : Word;
  Result:Boolean;
begin
  point.x := 0;
  point.y := 0;
  dpy := XOpenDisplay(nil);
  Result := LongBool(XQueryPointer(dpy, XDefaultRootWindow( dpy), @root, @child,
     @rootX, @rootY, @winX, @winY, @xstate));
  If Result then begin
    point.x := rootX;
    point.y := rootY;
  end;
    XCloseDisplay(dpy);
{$ENDIF}
{$IFDEF Darwin}
begin
  {$MESSAGE Warn 'Needs to be implemented'}
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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

finalization
{$IFDEF MSWINDOWS}
   if vCurrentVideoMode<>0 then
{$ENDIF}
{$IFDEF GLS_X11_SUPPORT}
   if vScreenModeChanged then
{$ENDIF}
      RestoreDefaultMode;  // set default video mode
end.

