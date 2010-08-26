//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLScreen<p>

   Routines to interact with the screen/desktop.<p>

      $Log: glscreen.pas,v $
      Revision 1.1  2006/01/10 20:50:45  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.3  2006/01/09 20:45:50  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/12/04 16:53:06  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.1  2005/12/01 21:24:11  z0m3ie
      *** empty log message ***

      Revision 1.5  2005/08/15 19:36:47  z0m3ie
      *** empty log message ***

      Revision 1.4  2005/08/15 18:39:11  z0m3ie
      - Linux compatibility issues

      Revision 1.3  2005/08/03 00:41:39  z0m3ie
      - added automatical generated History from CVS

   <b>Historique : </b><font size=-1><ul>
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

{$include GLScene.inc}

uses
   {$IFDEF WINDOWS}Windows,{$ENDIF} Classes, Graphics, VectorGeometry;

const
   MaxVideoModes = 200;

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

{$IFDEF MSWINDOWS}
procedure ReadVideoModes;
{$ENDIF}
function GetIndexFromResolution(XRes,YRes,BPP: Integer): TResolution;
//: Changes to the video mode given by 'Index'
{$IFDEF MSWINDOWS}
function SetFullscreenMode(modeIndex : TResolution; displayFrequency : Integer = 0) : Boolean;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure ReadScreenImage(Dest: HDC; DestLeft, DestTop: Integer; SrcRect: TRectangle);
procedure RestoreDefaultMode;
{$ENDIF}

var
   vVideoModes        : array of TVideoMode;
   vNumberVideoModes  : Integer = 0;
   vCurrentVideoMode  : Integer = 0;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

uses GLScene, SysUtils, Forms;

type TLowResMode = packed record
                     Width : Word;
                     Height : Word;
                     ColorDepth : Byte;
                   end;

const NumberLowResModes = 15;
      LowResModes       : array [0..NumberLowResModes-1] of TLowResMode =
      (
       (Width:320;Height:200;ColorDepth: 8),(Width:320;Height:200;ColorDepth:15),(Width:320;Height:200;ColorDepth:16),
       (Width:320;Height:200;ColorDepth:24),(Width:320;Height:200;ColorDepth:32),
       (Width:400;Height:300;ColorDepth: 8),(Width:400;Height:300;ColorDepth:15),(Width:400;Height:300;ColorDepth:16),
       (Width:400;Height:300;ColorDepth:24),(Width:400;Height:300;ColorDepth:32),
       (Width:512;Height:384;ColorDepth: 8),(Width:512;Height:384;ColorDepth:15),(Width:512;Height:384;ColorDepth:16),
       (Width:512;Height:384;ColorDepth:24),(Width:512;Height:384;ColorDepth:32)
      );

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
end;
{$ENDIF}

// ReadVideoModes
//
{$IFDEF MSWINDOWS}
procedure ReadVideoModes;
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
end;
{$ENDIF}

// GetIndexFromResolution
//
function GetIndexFromResolution(XRes,YRes,BPP: Integer): TResolution;

// Determines the index of a screen resolution nearest to the
// given values. The returned screen resolution is always greater
// or equal than XRes and YRes or, in case the resolution isn't
// supported, the value 0, which indicates the default mode.

var
   I : Integer;
   XDiff, YDiff, CDiff : Integer;

begin
{$IFDEF MSWINDOWS}
   ReadVideoModes;
{$ENDIF}
   // prepare result in case we don't find a valid mode
   Result:=0;
   // set differences to maximum
   XDiff:=9999; YDiff:=9999; CDiff:=99;
   for I:=1 to vNumberVideomodes-1 do with vVideoModes[I] do begin
     if     (Width  >= XRes) and ((Width-XRes)  <= XDiff)
        and (Height >= YRes) and ((Height-YRes) <= YDiff)
        and (ColorDepth >= BPP) and ((ColorDepth-BPP) <= CDiff) then begin
         XDiff:=Width-XRes;
         YDiff:=Height-YRes;
         CDiff:=ColorDepth-BPP;
         Result:=I;
     end;
   end;
end;

// SetFullscreenMode
//
{$IFDEF MSWINDOWS}
function SetFullscreenMode(modeIndex : TResolution; displayFrequency : Integer = 0) : Boolean;
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
end;
{$ENDIF}

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
{$IFDEF MSWINDOWS}
procedure RestoreDefaultMode;
var
   t : PDevMode;
begin
   t:=nil;
   ChangeDisplaySettings(t^, CDS_FULLSCREEN);
end;
{$ENDIF}

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
      RestoreDefaultMode;  // set default video mode
{$ENDIF}

end.
