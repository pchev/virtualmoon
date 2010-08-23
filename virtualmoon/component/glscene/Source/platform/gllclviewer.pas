//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLLCLViewer<p>

  A FPC specific Scene viewer.

	<b>History : </b><font size=-1><ul>
      <li>10/04/08 - DaStr - Bugfixed TGLSceneViewer.Notification()
                              (thanks z80maniac) (Bugtracker ID = 1936108)
      <li>12/09/07 - DaStr - Removed old IFDEFs. Moved SetupVSync()
                              to GLViewer.pas (Bugtracker ID = 1786279)
      <li>04/12/04 - DaS - OnMouseWheel, OnMouseWheelDown, OnMouseWheelUp
                           published in TGLSceneViewer
      <li>04/12/04 - MF - Added FieldOfView, formula by Ivan Sivak Jr.
      <li>24/07/03 - EG - FullScreen Viewer moved to GLWin32FullScreenViewer
      <li>11/06/03 - EG - Now uses ViewerBeforeChange to adjust VSync
      <li>29/10/02 - EG - Added MouseEnter/Leave/InControl
      <li>27/09/02 - EG - Added Ability to set display frequency
      <li>22/08/02 - EG - Added TGLSceneViewer.RecreateWnd
      <li>19/08/02 - EG - Added GetHandle
      <li>14/03/02 - EG - No longer invalidates while rendering
      <li>11/02/02 - EG - Fixed BeforeRender
      <li>29/01/02 - EG - New StayOnTop/Maximize logic (Richard Smuts)
      <li>22/01/02 - EG - Added TGLFullScreenViewer
      <li>28/12/01 - EG - Event persistence change (GliGli / Dephi bug)
	    <li>12/12/01 - EG - Creation (split from GLScene.pas)
	</ul></font>
}
unit GLLCLViewer;

interface

{$i GLScene.inc}

uses
  LCLType,
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Messages, Graphics, Forms, Classes, Controls, Menus,
  LMessages,

  // GLScene
  GLScene, GLContext;


type
   // TVSyncMode
   //
   TVSyncMode = (vsmSync, vsmNoSync);

   // TGLSceneViewer
   //
   {: Component where the GLScene objects get rendered.<p>
      This component delimits the area where OpenGL renders the scene,
      it represents the 3D scene viewed from a camera (specified in the
      camera property). This component can also render to a file or to a bitmap.<p>
      It is primarily a windowed component, but it can handle full-screen
      operations : simply make this component fit the whole screen (use a
      borderless form).<p>
      This viewer also allows to define rendering options such a fog, face culling,
      depth testing, etc. and can take care of framerate calculation.<p> }
   TGLSceneViewerLCL = class (TWinControl)
      private
         { Private Declarations }
         FBuffer : TGLSceneBuffer;
         FVSync : TVSyncMode;
         FOwnDC : HDC;
         FOnMouseEnter, FOnMouseLeave : TNotifyEvent;
         FMouseInControl : Boolean;
         FIsOpenGLAvailable : Boolean;
         FLastScreenPos : TPoint;

         {$IFDEF MSWINDOWS}
         procedure EraseBackground(DC: HDC); override;
         {$ENDIF}
         procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); Message LM_ERASEBKGND;
         procedure WMPaint(var Message: TLMPaint); Message LM_PAINT;
         procedure WMSize(var Message: TLMSize); Message LM_SIZE;
         procedure WMDestroy(var Message: TLMDestroy); message LM_DESTROY;

         procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
         procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;

         function GetFieldOfView: single;
         procedure SetFieldOfView(const Value: single);

      protected
         { Protected Declarations }
         procedure SetBeforeRender(const val : TNotifyEvent);
         function GetBeforeRender : TNotifyEvent;
         procedure SetPostRender(const val : TNotifyEvent);
         function GetPostRender : TNotifyEvent;
         procedure SetAfterRender(const val : TNotifyEvent);
         function GetAfterRender : TNotifyEvent;
         procedure SetCamera(const val : TGLCamera);
         function GetCamera : TGLCamera;
         procedure SetBuffer(const val : TGLSceneBuffer);

         {$IFDEF WINDOWS}
         procedure CreateParams(var Params: TCreateParams); override;
         {$ENDIF}
         procedure CreateWnd; override;
         procedure DestroyWnd; override;
         procedure Loaded; override;
         procedure DoBeforeRender(Sender : TObject); dynamic;
         procedure DoBufferChange(Sender : TObject); virtual;
         procedure DoBufferStructuralChange(Sender : TObject); dynamic;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor  Destroy; override;

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         {: Makes TWinControl's RecreateWnd public.<p>
            This procedure allows to work around limitations in some OpenGL
            drivers (like MS Software OpenGL) that are not able to share lists
            between RCs that already have display lists. }
         procedure RecreateWnd;

         property IsOpenGLAvailable : Boolean read FIsOpenGLAvailable;

         function LastFrameTime : Single;
         function FramesPerSecond : Single;
         function FramesPerSecondText(decimals : Integer = 1) : String;
         procedure ResetPerformanceMonitor;

         function CreateSnapShotBitmap : TBitmap;

         property RenderDC : HDC read FOwnDC;
         property MouseInControl : Boolean read FMouseInControl;
         Procedure Invalidate; override;
      published
         { Published Declarations }
         {: Camera from which the scene is rendered. }
         property Camera : TGLCamera read GetCamera write SetCamera;

         {: Specifies if the refresh should be synchronized with the VSync signal.<p>
            If the underlying OpenGL ICD does not support the WGL_EXT_swap_control
            extension, this property is ignored.  }
         property VSync : TVSyncMode read FVSync write FVSync default vsmNoSync;

         {: Triggered before the scene's objects get rendered.<p>
            You may use this event to execute your own OpenGL rendering. }
         property BeforeRender : TNotifyEvent read GetBeforeRender write SetBeforeRender;
         {: Triggered just after all the scene's objects have been rendered.<p>
            The OpenGL context is still active in this event, and you may use it
            to execute your own OpenGL rendering.<p> }
         property PostRender : TNotifyEvent read GetPostRender write SetPostRender;
         {: Called after rendering.<p>
            You cannot issue OpenGL calls in this event, if you want to do your own
            OpenGL stuff, use the PostRender event. }
         property AfterRender : TNotifyEvent read GetAfterRender write SetAfterRender;

         {: Access to buffer properties. }
         property Buffer : TGLSceneBuffer read FBuffer write SetBuffer;

         {: Returns or sets the field of view for the viewer, in degrees.<p>
         This value depends on the camera and the width and height of the scene.
         The value isn't persisted, if the width/height or camera.focallength is
         changed, FieldOfView is changed also. }
         property FieldOfView : single read GetFieldOfView write SetFieldOfView;

         property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
         property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
         
         property Align;
         property Anchors;
         property DragCursor;
         property DragMode;
         property Enabled;
         property HelpContext;
         property Hint;
         property PopupMenu;
         property Visible;

         property OnClick;
         property OnDblClick;
         property OnDragDrop;
         property OnDragOver;
         property OnStartDrag;
         property OnEndDrag;
         property OnMouseDown;
         property OnMouseMove;
         property OnMouseUp;

         property OnMouseWheel;
         property OnMouseWheelDown;
         property OnMouseWheelUp;
{$ifdef GLS_COMPILER_5_UP}
         property OnContextPopup;
{$endif}
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses OpenGL1x, sysutils, GLViewer
     {$ifndef fpc} // delphi
     ,GLWin32Context
     {$else}
     ,LCLIntf
       {$ifdef LCLWIN32}
         {$ifndef CONTEXT_INCLUDED}
     ,GLWin32Context
         {$define CONTEXT_INCLUDED}
         {$endif}
       {$endif}
       
       {$ifdef LCLGTK}
         {$ifndef CONTEXT_INCLUDED}
     ,GLLinGTKContext
         {$define CONTEXT_INCLUDED}
         {$endif}
       {$endif}
       
       {$ifdef LCLGTK2}
         {$ifndef CONTEXT_INCLUDED}
     ,GLLinGTKContext
         {$define CONTEXT_INCLUDED}
         {$endif}
       {$endif}

       {$ifdef LCLCARBON}
     ,GLCarbonContext
       {$endif}
       
       {$ifdef LCLQT}
         {$error unimplemented QT context}
       {$endif}
       
     {$endif}
     ;

// ------------------
// ------------------ TGLSceneViewer ------------------
// ------------------

// Create
//
constructor TGLSceneViewerLCL.Create(AOwner: TComponent);
begin
   FIsOpenGLAvailable:=InitOpenGL;
   inherited Create(AOwner);
   ControlStyle:=[csClickEvents, csDoubleClicks, csOpaque, csCaptureMouse];
   if csDesigning in ComponentState then
      ControlStyle:=ControlStyle+[csFramed];
   Width:=100;
   Height:=100;
   FVSync:=vsmNoSync;
   FBuffer:=TGLSceneBuffer.Create(Self);
   FBuffer.ViewerBeforeRender:=DoBeforeRender;
   FBuffer.OnChange:=DoBufferChange;
   FBuffer.OnStructuralChange:=DoBufferStructuralChange;
end;

// Destroy
//
destructor TGLSceneViewerLCL.Destroy;
begin
   FreeAndNil(FBuffer);
   inherited Destroy;
end;

// Notification
//
procedure TGLSceneViewerLCL.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation = opRemove) and (FBuffer<>nil) then
   begin
      if (AComponent = FBuffer.Camera) then
         FBuffer.Camera := nil;
   end;
   inherited;
end;

// RecreateWnd
//
procedure TGLSceneViewerLCL.RecreateWnd;
begin
   inherited;
end;

// SetBeforeRender
//
procedure TGLSceneViewerLCL.SetBeforeRender(const val : TNotifyEvent);
begin
   if assigned(FBuffer) then
     FBuffer.BeforeRender:=val;
end;

// GetBeforeRender
//
function TGLSceneViewerLCL.GetBeforeRender : TNotifyEvent;
begin
   if assigned(FBuffer) then
     Result:=FBuffer.BeforeRender;
end;

// SetPostRender
//
procedure TGLSceneViewerLCL.SetPostRender(const val : TNotifyEvent);
begin
   if assigned(FBuffer) then
     FBuffer.PostRender:=val;
end;

// GetPostRender
//
function TGLSceneViewerLCL.GetPostRender : TNotifyEvent;
begin
   if assigned(FBuffer) then
     Result:=FBuffer.PostRender;
end;

// SetAfterRender
//
procedure TGLSceneViewerLCL.SetAfterRender(const val : TNotifyEvent);
begin
   if assigned(FBuffer) then
     FBuffer.AfterRender:=val;
end;

// GetAfterRender
//
function TGLSceneViewerLCL.GetAfterRender : TNotifyEvent;
begin
   if assigned(FBuffer) then
     Result:=FBuffer.AfterRender;
end;

// SetCamera
//
procedure TGLSceneViewerLCL.SetCamera(const val : TGLCamera);
begin
   if assigned(FBuffer) then
     FBuffer.Camera:=val;
end;

// GetCamera
//
function TGLSceneViewerLCL.GetCamera : TGLCamera;
begin
   if assigned(FBuffer) then
     Result:=FBuffer.Camera;
end;

// SetBuffer
//
procedure TGLSceneViewerLCL.SetBuffer(const val : TGLSceneBuffer);
begin
   if assigned(FBuffer) then
     FBuffer.Assign(val);
end;

{$ifdef MSWINDOWS}
// CreateParams
//
procedure TGLSceneViewerLCL.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   with Params do begin
      Style:=Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
      WindowClass.Style:=WindowClass.Style or CS_OWNDC;
   end;
end;
{$ENDIF}
// CreateWnd
//
procedure TGLSceneViewerLCL.CreateWnd;
begin
   inherited CreateWnd;
   if ((IsOpenGLAvailable) and (assigned(FBuffer))) then begin
      // initialize and activate the OpenGL rendering context
      // need to do this only once per window creation as we have a private DC
      FBuffer.Resize(Self.Width, Self.Height);
      {.$ifdef MSWINDOWS}
      FOwnDC:=GetDC(Handle);
      {.$ELSE}
      //FOwnDC := self;
      {.$ENDIF}
      FBuffer.CreateRC(FOwnDC, False);
   end;
end;

// DestroyWnd
//
procedure TGLSceneViewerLCL.DestroyWnd;
begin
   if assigned(FBuffer) then
     FBuffer.DestroyRC;
   if FOwnDC<>0 then begin
      {.$IFDEF MSWINDIOWS}
      ReleaseDC(Handle, FOwnDC);
      {.$ELSE}
      //FOwnDC := 0;
      {.$ENDIF}
      FOwnDC:=0;
   end;
   inherited;
end;

// WMEraseBkgnd
//
procedure TGLSceneViewerLCL.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
   if IsOpenGLAvailable then
      Message.Result:=1
   else inherited;
end;

// WMSize
//
procedure TGLSceneViewerLCL.WMSize(var Message: TLMSize);
begin
   inherited;
   if assigned(FBuffer) then
     FBuffer.Resize(Message.Width, Message.Height);
end;

// WMPaint
//
procedure TGLSceneViewerLCL.WMPaint(var Message: TLMPaint);
{$IFNDEF MSWINDOWS}
  begin
    Include(FControlState,csCustomPaint);
    inherited WMPaint(Message);
    if ((IsOpenGLAvailable) and (assigned(FBuffer))) then
      FBuffer.Render;
    Exclude(FControlState,csCustomPaint);
  end;
{$ELSE}
var
   PS : LCLType.TPaintStruct;
   p : TPoint;
begin
   p:=ClientToScreen(Point(0, 0));
   if (FLastScreenPos.X<>p.X) or (FLastScreenPos.Y<>p.Y) then begin
      // Workaround for MS OpenGL "black borders" bug
      if ((assigned(FBuffer)) and (FBuffer.RCInstantiated)) then
         PostMessage(Handle, WM_SIZE, SIZE_RESTORED,
                     Width+(Height shl 16));
      FLastScreenPos:=p;
   end;
   BeginPaint(Handle, PS);
   try
      if assigned(FBuffer) and IsOpenGLAvailable and (Width>0) and (Height>0) then
         FBuffer.Render;
   finally
      EndPaint(Handle, PS);
      Message.Result:=0;
   end;
end;
{$ENDIF}

// WMDestroy
//
procedure TGLSceneViewerLCL.WMDestroy(var Message: TLMDestroy);
begin
   if assigned(FBuffer) then
     FBuffer.DestroyRC;
   if FOwnDC<>0 then begin
     {.$ifdef MSWINDOWS}
      ReleaseDC(Handle, FOwnDC);
      {.$else}
      //FOwnDC := 0;
      {.$endif}
      FOwnDC:=0;
   end;
   inherited;
end;

// CMMouseEnter
//
procedure TGLSceneViewerLCL.CMMouseEnter(var msg: TMessage);
begin
   inherited;
   FMouseInControl:=True;
   if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

// CMMouseLeave
//
procedure TGLSceneViewerLCL.CMMouseLeave(var msg: TMessage);
begin
   inherited;
   FMouseInControl:=False;
   if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

// Loaded
//
procedure TGLSceneViewerLCL.Loaded;
begin
   inherited Loaded;
   // initiate window creation
   HandleNeeded;
end;

// DoBeforeRender
//
procedure TGLSceneViewerLCL.DoBeforeRender(Sender : TObject);
begin
   SetupVSync(VSync);
end;

// DoBufferChange
//
procedure TGLSceneViewerLCL.DoBufferChange(Sender : TObject);
begin
   if (not Buffer.Rendering) and (not Buffer.Freezed) then
      Invalidate;
end;

// DoBufferStructuralChange
//
procedure TGLSceneViewerLCL.DoBufferStructuralChange(Sender : TObject);
begin
   RecreateWnd;
end;

// LastFrameTime
//
function TGLSceneViewerLCL.LastFrameTime : Single;
begin
   if assigned(FBuffer) then
     Result:=FBuffer.LastFrameTime
   else result:=0.0;
end;

// FramesPerSecond
//
function TGLSceneViewerLCL.FramesPerSecond : Single;
begin
   if assigned(FBuffer) then
     Result:=FBuffer.FramesPerSecond
   else
     result:=0.0;
end;

// FramesPerSecondText
//
function TGLSceneViewerLCL.FramesPerSecondText(decimals : Integer = 1) : String;
begin
   if assigned(FBuffer) then
     Result:=Format('%.*f FPS', [decimals, FBuffer.FramesPerSecond])
   else result:='';
end;

// ResetPerformanceMonitor
//
procedure TGLSceneViewerLCL.ResetPerformanceMonitor;
begin
   if assigned(FBuffer) then
     FBuffer.ResetPerformanceMonitor;
end;

// CreateSnapShotBitmap
//
function TGLSceneViewerLCL.CreateSnapShotBitmap : TBitmap;
begin
{$ifdef MSWINDOWS}
   Result:=TBitmap.Create;
   Result.PixelFormat:=pf24bit;
   Result.Width:=Width;
   Result.Height:=Height;
   BitBlt(Result.Canvas.Handle, 0, 0, Width, Height,
          RenderDC, 0, 0, SRCCOPY);
{$endif}
end;

// GetFieldOfView
//
function TGLSceneViewerLCL.GetFieldOfView: single;
begin
  if not Assigned(Camera) then
    result := 0

  else if Width<Height then
    result := Camera.GetFieldOfView(Width)

  else
    result := Camera.GetFieldOfView(Height);
end;

procedure TGLSceneViewerLCL.SetFieldOfView(const Value: single);
begin
  if Assigned(Camera) then
  begin
    if Width<Height then
      Camera.SetFieldOfView(Value, Width)

    else
      Camera.SetFieldOfView(Value, Height);
  end;
end;

Procedure TGLSceneViewerLCL.Invalidate;
begin
  inherited;
end;

{$IFDEF MSWINDOWS}
procedure TGLSceneViewerLCL.EraseBackground(DC: HDC);
begin

end;
{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------


end.

