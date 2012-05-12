//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLSceneForm<p>

  <b>History : </b><font size=-1><ul>
  <li>05/04/11 - Yar - Added property FullScreenVideoMode (thanks to ltyrosine)
  <li>08/12/10 - Yar - Added code for Lazarus (thanks Rustam Asmandiarov aka Predator)
  <li>23/08/10 - Yar - Creation
  </ul></font>
}

unit GLSceneForm;

interface

{$I GLScene.inc}

uses
{$IFDEF GLS_DELPHI_OR_CPPB}
  Windows,
  Messages,
{$ELSE}
  LCLIntf,
  LCLType,
  LMessages,
{$IF DEFINED(LCLwin32) or DEFINED(LCLwin64)}
  Windows, // need
  WSLCLClasses, Win32Int, Win32WSForms,
  Win32Proc, LCLMessageGlue, Win32WSControls, LCLVersion,
{$IFEND}
{$ENDIF}
  Classes,
{$IFDEF GLS_DELPHI_XE2_UP}
  VCL.Controls,
  VCL.Forms,
{$ELSE}
  Controls,
  Forms,
{$ENDIF}

  GLScene,
  GLContext,
  GLCrossPlatform,
  GLScreen;

type

  TGLSceneForm = class;

  // TGLFullScreenResolution
  //
  {: Defines how GLSceneForm will handle fullscreen request
     fcWindowMaximize: Use current resolution (just maximize form and hide OS bars)
     fcNearestResolution: Change to nearest valid resolution from current window size
     fcManualResolution: Use FFullScreenVideoMode settings }
  TGLFullScreenResolution = (
    fcUseCurrent,
    fcNearestResolution,
    fcManualResolution);

  // TGLFullScreenVideoMode
  //
  {: Screen mode settings }
  TGLFullScreenVideoMode = class(TPersistent)
  private
    FOwner: TGLSceneForm;
    FEnabled: Boolean;
    FAltTabSupportEnable: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FColorDepth: Integer;
    FFrequency: Integer;
    FResolutionMode: TGLFullScreenResolution;
    procedure SetEnabled(aValue: Boolean);
    procedure SetAltTabSupportEnable(aValue: Boolean);
  public
    constructor Create(AOwner: TGLSceneForm);
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property AltTabSupportEnable: Boolean read FAltTabSupportEnable
      write SetAltTabSupportEnable default False;
    property ResolutionMode: TGLFullScreenResolution read FResolutionMode
      write FResolutionMode default fcUseCurrent;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property ColorDepth: Integer read FColorDepth write FColorDepth;
    property Frequency: Integer read FFrequency write FFrequency;
  end;

  { TGLSceneForm }

  TGLSceneForm = class(TForm)
  private
    { Private Declarations }
    FBuffer: TGLSceneBuffer;
    FVSync: TVSyncMode;
    FOwnDC: HDC;
    FFullScreenVideoMode: TGLFullScreenVideoMode;
    procedure SetBeforeRender(const val: TNotifyEvent);
    function GetBeforeRender: TNotifyEvent;
    procedure SetPostRender(const val: TNotifyEvent);
    function GetPostRender: TNotifyEvent;
    procedure SetAfterRender(const val: TNotifyEvent);
    function GetAfterRender: TNotifyEvent;
    procedure SetCamera(const val: TGLCamera);
    function GetCamera: TGLCamera;
    procedure SetBuffer(const val: TGLSceneBuffer);

    function GetFieldOfView: single;
    procedure SetFieldOfView(const Value: single);
    function GetIsRenderingContextAvailable: Boolean;
{$IFDEF GLS_DELPHI_OR_CPPB}
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure LastFocus(var Mess: TMessage); message WM_ACTIVATE;
{$ENDIF}
{$IFDEF FPC}
    procedure LMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure LMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure LMSize(var Message: TLMSize); message LM_SIZE;
    procedure LMDestroy(var Message: TLMDestroy); message LM_DESTROY;
    procedure GetFocus(var Mess: TLMessage); message LM_ACTIVATE;
    procedure LastFocus(var Mess: TLMessage); message LM_DEACTIVATE;
{$ENDIF}
    procedure SetFullScreenVideoMode(AValue: TGLFullScreenVideoMode);
    procedure StartupFS;
    procedure ShutdownFS;
  protected
    { Protected Declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure CreateWnd; override;
    procedure Loaded; override;

    procedure DoBeforeRender(Sender: TObject); dynamic;
    procedure DoBufferChange(Sender: TObject); virtual;
    procedure DoBufferStructuralChange(Sender: TObject); dynamic;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyWnd; override;

    property IsRenderingContextAvailable: Boolean read
      GetIsRenderingContextAvailable;
    property RenderDC: HDC read FOwnDC;
  published
    { Published Declarations }
    { : Camera from which the scene is rendered. }
    property Camera: TGLCamera read GetCamera write SetCamera;

    { : Specifies if the refresh should be synchronized with the VSync signal.<p>
      If the underlying OpenGL ICD does not support the WGL_EXT_swap_control
      extension, this property is ignored. }
    property VSync: TVSyncMode read FVSync write FVSync default vsmNoSync;

    { : Triggered before the scene's objects get rendered.<p>
      You may use this event to execute your own OpenGL rendering. }
    property BeforeRender: TNotifyEvent read GetBeforeRender write
      SetBeforeRender;
    { : Triggered just after all the scene's objects have been rendered.<p>
      The OpenGL context is still active in this event, and you may use it
      to execute your own OpenGL rendering.<p> }
    property PostRender: TNotifyEvent read GetPostRender write SetPostRender;
    { : Called after rendering.<p>
      You cannot issue OpenGL calls in this event, if you want to do your own
      OpenGL stuff, use the PostRender event. }
    property AfterRender: TNotifyEvent read GetAfterRender write SetAfterRender;

    { : Access to buffer properties. }
    property Buffer: TGLSceneBuffer read FBuffer write SetBuffer;

    { : Returns or sets the field of view for the viewer, in degrees.<p>
      This value depends on the camera and the width and height of the scene.
      The value isn't persisted, if the width/height or camera.focallength is
      changed, FieldOfView is changed also. }
    property FieldOfView: single read GetFieldOfView write SetFieldOfView;

    property FullScreenVideoMode: TGLFullScreenVideoMode read
      FFullScreenVideoMode
      write SetFullScreenVideoMode;
  end;
{$IFDEF FPC}
  // Code created to workaround black screen and blinking when Manifest is enabled
  // Код создан для обхода черного экрана и мерцания при включенном Manifest'е
{$IF DEFINED(LCLwin32) or DEFINED(LCLwin64)}

  TGLSOpenGLForm = class(TWin32WSForm)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams:
      TCreateParams): HWND; override;
  end;

procedure GLRegisterWSComponent(aControl: TComponentClass);
{$IFEND}
{$ENDIF}

implementation

uses
  GLViewer;

constructor TGLSceneForm.Create(AOwner: TComponent);
begin
  FBuffer := TGLSceneBuffer.Create(Self);
  FVSync := vsmNoSync;
  FBuffer.ViewerBeforeRender := DoBeforeRender;
  FBuffer.OnChange := DoBufferChange;
  FBuffer.OnStructuralChange := DoBufferStructuralChange;
  FFullScreenVideoMode := TGLFullScreenVideoMode.Create(Self);
  inherited Create(AOwner);
end;

destructor TGLSceneForm.Destroy;
begin
  FBuffer.Free;
  FBuffer := nil;
  FFullScreenVideoMode.Destroy;
  inherited Destroy;
end;

// Notification
//

procedure TGLSceneForm.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (Operation = opRemove) and (FBuffer <> nil) then
  begin
    if (AComponent = FBuffer.Camera) then
      FBuffer.Camera := nil;
  end;
  inherited;
end;

// CreateWnd
//

procedure TGLSceneForm.CreateWnd;
begin
  inherited CreateWnd;
  // initialize and activate the OpenGL rendering context
  // need to do this only once per window creation as we have a private DC
  FBuffer.Resize(0, 0, Self.Width, Self.Height);
  FOwnDC := GetDC(Handle);
  FBuffer.CreateRC(FOwnDC, false);
end;

// DestroyWnd
//

procedure TGLSceneForm.DestroyWnd;
begin
  if Assigned(FBuffer) then
  begin
    FBuffer.DestroyRC;
    if FOwnDC <> 0 then
    begin
      ReleaseDC(Handle, FOwnDC);
      FOwnDC := 0;
    end;
  end;
  inherited;
end;

// Loaded
//

procedure TGLSceneForm.Loaded;
begin
  inherited Loaded;
  // initiate window creation
  HandleNeeded;
  if not (csDesigning in ComponentState) then
  begin
    if FFullScreenVideoMode.FEnabled then
      StartupFS;
  end;
end;

{$IFDEF GLS_DELPHI_OR_CPPB}
// WMEraseBkgnd
//

procedure TGLSceneForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if GetIsRenderingContextAvailable then
    Message.Result := 1
  else
    inherited;
end;

// WMSize
//

procedure TGLSceneForm.WMSize(var Message: TWMSize);
begin
  inherited;
  if Assigned(FBuffer) then
    FBuffer.Resize(0, 0, Message.Width, Message.Height);
end;

// WMPaint
//

procedure TGLSceneForm.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
begin
  BeginPaint(Handle, PS);
  try
    if GetIsRenderingContextAvailable and (Width > 0) and (Height > 0) then
      FBuffer.Render;
  finally
    EndPaint(Handle, PS);
    Message.Result := 0;
  end;
end;

// WMDestroy
//

procedure TGLSceneForm.WMDestroy(var Message: TWMDestroy);
begin
  if Assigned(FBuffer) then
  begin
    FBuffer.DestroyRC;
    if FOwnDC <> 0 then
    begin
      ReleaseDC(Handle, FOwnDC);
      FOwnDC := 0;
    end;
  end;
  inherited;
end;

// LastFocus
//

procedure TGLSceneForm.LastFocus(var Mess: TMessage);
begin
  if not (csDesigning in ComponentState)
    and FFullScreenVideoMode.FEnabled
    and FFullScreenVideoMode.FAltTabSupportEnable then
    begin
      if Mess.wParam = WA_INACTIVE then
      begin
        ShutdownFS;
      end
      else
      begin
        StartupFS;
      end;
    end;
  inherited;
end;

{$ENDIF GLS_DELPHI_OR_CPPB}

{$IFDEF FPC}

procedure TGLSceneForm.LMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  if IsRenderingContextAvailable then
    Message.Result := 1
  else
    inherited;
end;

procedure TGLSceneForm.LMPaint(var Message: TLMPaint);
var
  PS: TPaintStruct;
begin
  BeginPaint(Handle, PS);
  try
    if IsRenderingContextAvailable and (Width > 0) and (Height > 0) then
      FBuffer.Render;
  finally
    EndPaint(Handle, PS);
    Message.Result := 0;
  end;
end;

procedure TGLSceneForm.LMSize(var Message: TLMSize);
begin
  inherited;
  if Assigned(FBuffer) then
    FBuffer.Resize(0, 0, Message.Width, Message.Height);
end;

procedure TGLSceneForm.LMDestroy(var Message: TLMDestroy);
begin
  if Assigned(FBuffer) then
  begin
    FBuffer.DestroyRC;
    if FOwnDC <> 0 then
    begin
      ReleaseDC(Handle, FOwnDC);
      FOwnDC := 0;
    end;
  end;
  inherited;
end;

procedure TGLSceneForm.GetFocus(var Mess: TLMessage);
begin
  if not (csDesigning in ComponentState)
    and FFullScreenVideoMode.FEnabled
    and FFullScreenVideoMode.FAltTabSupportEnable then
    begin
      StartupFS;
    end;
  inherited;
end;

procedure TGLSceneForm.LastFocus(var Mess: TLMessage);
begin
  if not (csDesigning in ComponentState)
    and FFullScreenVideoMode.FEnabled
    and FFullScreenVideoMode.FAltTabSupportEnable then
    begin
      ShutdownFS;
    end;
  inherited;
end;

{$ENDIF FPC}

procedure TGLFullScreenVideoMode.SetEnabled(aValue: Boolean);
begin
  if FEnabled <> aValue then
  begin
    FEnabled := aValue;
    if not ((csDesigning in FOwner.ComponentState)
      or (csLoading in FOwner.ComponentState)) then
    begin
      if FEnabled then
        FOwner.StartupFS
      else
        FOwner.ShutdownFS;
    end;
  end;
end;

constructor TGLFullScreenVideoMode.Create(AOwner: TGLSceneForm);
begin
  inherited Create;
  FOwner := AOwner;
  FEnabled := False;
  FAltTabSupportEnable := False;
  ReadVideoModes;
{$IFDEF MSWINDOWS}
  FWidth := vVideoModes[0].Width;
  FHeight := vVideoModes[0].Height;
  FColorDepth := vVideoModes[0].ColorDepth;
  FFrequency := vVideoModes[0].MaxFrequency;
{$ENDIF}
{$IFDEF GLS_X11_SUPPORT}
  FWidth := vVideoModes[0].vdisplay;
  FHeight := vVideoModes[0].hdisplay;
  FColorDepth := 32;
  FFrequency := 0;
{$ENDIF}
{$IFDEF DARWIN}
  FWidth := 1280;
  FHeight := 1024;
  FColorDepth := 32;
  FFrequency := 0;
  {$Message Hint 'Fullscreen mode not yet implemented for Darwin OSes' }
{$ENDIF}
  if FFrequency = 0 then
    FFrequency := 50;
  FResolutionMode := fcUseCurrent;
end;

procedure TGLFullScreenVideoMode.SetAltTabSupportEnable(aValue: Boolean);
begin
  if FAltTabSupportEnable <> aValue then
    FAltTabSupportEnable := aValue;
end;

procedure TGLSceneForm.StartupFS;
begin
  case FFullScreenVideoMode.FResolutionMode of
    fcNearestResolution:
      begin
        SetFullscreenMode(GetIndexFromResolution(ClientWidth, ClientHeight,
{$IFDEF MSWINDOWS}
        vVideoModes[0].ColorDepth));
{$ELSE}
        32));
{$ENDIF}
      end;
    fcManualResolution:
      begin
        SetFullscreenMode(GetIndexFromResolution(FFullScreenVideoMode.Width , FFullScreenVideoMode.Height, FFullScreenVideoMode.ColorDepth), FFullScreenVideoMode.Frequency);
      end;
  end;

  Left := 0;
  Top := 0;
  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;
  BringToFront;
  WindowState := wsMaximized;

{$IFDEF FPC}
  ShowInTaskBar := stAlways;
{$ELSE}
{$IFDEF GLS_DELPHI_2009_UP}
  Application.MainFormOnTaskBar := True;
{$ENDIF}
{$ENDIF}
end;

procedure TGLSceneForm.ShutdownFS;
begin
  RestoreDefaultMode;
  SendToBack;
  WindowState := wsNormal;
  BorderStyle := bsSingle;
  FormStyle := fsNormal;
  Left := (Screen.Width div 2) - (Width div 2);
  Top := (Screen.Height div 2) - (Height div 2);
end;

// DoBeforeRender
//

procedure TGLSceneForm.DoBeforeRender(Sender: TObject);
begin
  SetupVSync(VSync);
end;

// DoBufferChange
//

procedure TGLSceneForm.DoBufferChange(Sender: TObject);
begin
  if (not Buffer.Rendering) and (not Buffer.Freezed) then
    Invalidate;
end;

// DoBufferStructuralChange
//

procedure TGLSceneForm.DoBufferStructuralChange(Sender: TObject);
begin
{$IFNDEF FPC}
  RecreateWnd;
{$ELSE}
  DestroyWnd;
  CreateWnd;
{$ENDIF}
end;

procedure TGLSceneForm.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if csDesignInteractive in ControlStyle then
    FBuffer.NotifyMouseMove(Shift, X, Y);
end;

// SetBeforeRender
//

procedure TGLSceneForm.SetBeforeRender(const val: TNotifyEvent);
begin
  FBuffer.BeforeRender := val;
end;

// GetBeforeRender
//

function TGLSceneForm.GetBeforeRender: TNotifyEvent;
begin
  Result := FBuffer.BeforeRender;
end;

// SetPostRender
//

procedure TGLSceneForm.SetPostRender(const val: TNotifyEvent);
begin
  FBuffer.PostRender := val;
end;

// GetPostRender
//

function TGLSceneForm.GetPostRender: TNotifyEvent;
begin
  Result := FBuffer.PostRender;
end;

// SetAfterRender
//

procedure TGLSceneForm.SetAfterRender(const val: TNotifyEvent);
begin
  FBuffer.AfterRender := val;
end;

// GetAfterRender
//

function TGLSceneForm.GetAfterRender: TNotifyEvent;
begin
  Result := FBuffer.AfterRender;
end;

// SetCamera
//

procedure TGLSceneForm.SetCamera(const val: TGLCamera);
begin
  FBuffer.Camera := val;
end;

// GetCamera
//

function TGLSceneForm.GetCamera: TGLCamera;
begin
  Result := FBuffer.Camera;
end;

// SetBuffer
//

procedure TGLSceneForm.SetBuffer(const val: TGLSceneBuffer);
begin
  FBuffer.Assign(val);
end;

// GetFieldOfView
//

function TGLSceneForm.GetFieldOfView: single;
begin
  if not Assigned(Camera) then
    Result := 0
  else if Width < Height then
    Result := Camera.GetFieldOfView(Width)
  else
    Result := Camera.GetFieldOfView(Height);
end;

// SetFieldOfView
//

procedure TGLSceneForm.SetFieldOfView(const Value: single);
begin
  if Assigned(Camera) then
  begin
    if Width < Height then
      Camera.SetFieldOfView(Value, Width)
    else
      Camera.SetFieldOfView(Value, Height);
  end;
end;

procedure TGLSceneForm.SetFullScreenVideoMode(AValue: TGLFullScreenVideoMode);
begin
end;

// GetIsRenderingContextAvailable
//

function TGLSceneForm.GetIsRenderingContextAvailable: Boolean;
begin
  Result := FBuffer.RCInstantiated and FBuffer.RenderingContext.IsValid;
end;

{$IFDEF FPC}
{$IF DEFINED(LCLwin32) or DEFINED(LCLwin64)}
// FixBSod

function GlWindowProc(Window: HWND; Msg: UInt; wParam: Windows.wParam; LParam:
  Windows.LParam): LResult; stdcall;
var
  PaintMsg: TLMPaint;
  winctrl: TWinControl;
begin
  case Msg of
    WM_ERASEBKGND:
      begin
        Result := 0;
      end;
    WM_PAINT:
      begin
        winctrl := GetWin32WindowInfo(Window)^.WinControl;
        if Assigned(winctrl) then
        begin
          FillChar(PaintMsg, SizeOf(PaintMsg), 0);
          PaintMsg.Msg := LM_PAINT;
          PaintMsg.DC := wParam;
          DeliverMessage(winctrl, PaintMsg);
          Result := PaintMsg.Result;
        end
        else
          Result := WindowProc(Window, Msg, wParam, LParam);
      end;
  else
    Result := WindowProc(Window, Msg, wParam, LParam);
  end;
end;

function GetDesigningBorderStyle(const AForm: TCustomForm): TFormBorderStyle;
begin
  if csDesigning in AForm.ComponentState then
    Result := bsSizeable
  else
    Result := AForm.BorderStyle;
end;

function CalcBorderIconsFlags(const AForm: TCustomForm): DWORD;
var
  BorderIcons: TBorderIcons;
begin
  Result := 0;
  BorderIcons := AForm.BorderIcons;
  if (biSystemMenu in BorderIcons) or (csDesigning in AForm.ComponentState) then
    Result := Result or WS_SYSMENU;

  if GetDesigningBorderStyle(AForm) in [bsNone, bsSingle, bsSizeable] then
  begin
    if biMinimize in BorderIcons then
      Result := Result or WS_MINIMIZEBOX;
    if biMaximize in BorderIcons then
      Result := Result or WS_MAXIMIZEBOX;
  end;
end;

function CalcBorderIconsFlagsEx(const AForm: TCustomForm): DWORD;
var
  BorderIcons: TBorderIcons;
begin
  Result := 0;
  BorderIcons := AForm.BorderIcons;
  if GetDesigningBorderStyle(AForm) in [bsSingle, bsSizeable, bsDialog] then
  begin
    if biHelp in BorderIcons then
      Result := Result or WS_EX_CONTEXTHELP;
  end;
end;

function CalcBorderStyleFlags(const AForm: TCustomForm): DWORD;
begin
  Result := WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  case GetDesigningBorderStyle(AForm) of
    bsSizeable, bsSizeToolWin:
      Result := Result or (WS_OVERLAPPED or WS_THICKFRAME or WS_CAPTION);
    bsSingle, bsToolWindow:
      Result := Result or (WS_OVERLAPPED or WS_BORDER or WS_CAPTION);
    bsDialog:
      Result := Result or (WS_POPUP or WS_BORDER or WS_CAPTION);
    bsNone:
      if (AForm.Parent = nil) and (AForm.ParentWindow = 0) then
        Result := Result or WS_POPUP;
  end;
end;

function CalcBorderStyleFlagsEx(const AForm: TCustomForm): DWORD;
begin
  Result := 0;
  case GetDesigningBorderStyle(AForm) of
    bsDialog:
      Result := WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
    bsToolWindow, bsSizeToolWin:
      Result := WS_EX_TOOLWINDOW;
  end;
end;

procedure CalcFormWindowFlags(const AForm: TCustomForm; var Flags, FlagsEx:
  DWORD);
begin
  Flags := CalcBorderStyleFlags(AForm);
  if AForm.Parent <> nil then
    Flags := (Flags or WS_CHILD) and not WS_POPUP;
  // clear border style flags
  FlagsEx := FlagsEx and not (WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE or
    WS_EX_TOOLWINDOW);
  // set border style flags
  FlagsEx := FlagsEx or CalcBorderStyleFlagsEx(AForm);
  if (AForm.FormStyle in fsAllStayOnTop) and not (csDesigning in
    AForm.ComponentState) then
    FlagsEx := FlagsEx or WS_EX_TOPMOST;
  Flags := Flags or CalcBorderIconsFlags(AForm);
  FlagsEx := FlagsEx or CalcBorderIconsFlagsEx(AForm);
end;

procedure AdjustFormBounds(const AForm: TCustomForm; var SizeRect: TRect);
begin
  // the LCL defines the size of a form without border, win32 with.
  // -> adjust size according to BorderStyle
  SizeRect := AForm.BoundsRect;
  Windows.AdjustWindowRectEx(@SizeRect, CalcBorderStyleFlags(AForm),
    false, CalcBorderStyleFlagsEx(AForm));
end;

class function TGLSOpenGLForm.CreateHandle(const AWinControl: TWinControl; const
  AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  lForm: TCustomForm absolute AWinControl;
  Bounds: TRect;
  SystemMenu: HMenu;
begin
  // general initialization of Params
{$IF (LCL_RELEASE <= 28) }
  PrepareCreateWindow(AWinControl, Params);
{$ELSE}
  PrepareCreateWindow(AWinControl, AParams, Params);
{$IFEND}
  // customization of Params
  with Params do
  begin
    CalcFormWindowFlags(lForm, Flags, FlagsEx);
    pClassName := @ClsName;
    WindowTitle := StrCaption;
    AdjustFormBounds(lForm, Bounds);
    if (lForm.Position in [poDefault, poDefaultPosOnly]) and not (csDesigning in
      lForm.ComponentState) then
    begin
      Left := CW_USEDEFAULT;
      Top := CW_USEDEFAULT;
    end
    else
    begin
      Left := Bounds.Left;
      Top := Bounds.Top;
    end;
    if (lForm.Position in [poDefault, poDefaultSizeOnly]) and not (csDesigning in
      lForm.ComponentState) then
    begin
      Width := CW_USEDEFAULT;
      Height := CW_USEDEFAULT;
    end
    else
    begin
      Width := Bounds.Right - Bounds.Left;
      Height := Bounds.Bottom - Bounds.Top;
    end;
    SubClassWndProc := @GlWindowProc;
    if not (csDesigning in lForm.ComponentState) and lForm.AlphaBlend then
      FlagsEx := FlagsEx or WS_EX_LAYERED;
  end;
  SetStdBiDiModeParams(AWinControl, Params);
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;

  // remove system menu items for bsDialog
  if (lForm.BorderStyle = bsDialog) and not (csDesigning in lForm.ComponentState)
    then
  begin
    SystemMenu := GetSystemMenu(Result, false);
    DeleteMenu(SystemMenu, SC_RESTORE, MF_BYCOMMAND);
    DeleteMenu(SystemMenu, SC_SIZE, MF_BYCOMMAND);
    DeleteMenu(SystemMenu, SC_MINIMIZE, MF_BYCOMMAND);
    DeleteMenu(SystemMenu, SC_MAXIMIZE, MF_BYCOMMAND);
    DeleteMenu(SystemMenu, 1, MF_BYPOSITION);
      // remove the separator between move and close
  end;

  // Beginning with Windows 2000 the UI in an application may hide focus
  // rectangles and accelerator key indication. According to msdn we need to
  // initialize all root windows with this message
  if WindowsVersion >= wv2000 then
    Windows.SendMessage(Result, WM_CHANGEUISTATE, MakeWParam(UIS_INITIALIZE,
      UISF_HIDEFOCUS or UISF_HIDEACCEL), 0)
end;

procedure GLRegisterWSComponent(aControl: TComponentClass);
begin
  RegisterWSComponent(aControl, TGLSOpenGLForm);
end;
{$IFEND}
{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterClass(TGLSceneForm);
{$IFDEF FPC}
{$IF DEFINED(LCLwin32) or DEFINED(LCLwin64)}
  // Code created to workaround black screen and blinking when Manifest is enabled
  // You may comment it for Win2000\98
  // Код создан для обхода черного экрана и мерцания при включенном Manifest'е
  // Можно закоментировать для Win2000\98
  GLRegisterWSComponent(TGLSceneForm);
{$IFEND}
{$ENDIF}

end.
