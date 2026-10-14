//
// This unit is part of the GLScene Project, http://glscene.org
//
{ ============================================================================
  Cocoa / NSOpenGLView OpenGL context backend for GLScene.

  PURPOSE
  -------
  Drop-in replacement for GLCarbonContext.pas on macOS. The Carbon backend is
  written against the Carbon API and the Lazarus Carbon widgetset - both 32-bit
  only and removed from macOS after 10.14 (Catalina, 2019) - so GLScene's 3D
  view has not compiled, let alone rendered, on any recent Mac. This unit
  implements the same abstract TGLContext interface using the modern Cocoa
  OpenGL classes (NSOpenGLView / NSOpenGLContext / NSOpenGLPixelFormat) through
  Free Pascal's Objective-C bridge (mode switch objectivec1; see the compiler
  directives below the unit clause).

  Tested with FPC 3.2.2 + Lazarus 3.6 on macOS 26 / Apple Silicon: renders on a
  hardware, Metal-backed OpenGL 2.1 (compatibility) context.

  DESIGN: why an NSOpenGLView child, not a bare NSOpenGLContext on the LCL view
  ----------------------------------------------------------------------------
  The obvious approach - create an NSOpenGLContext and point it at the LCL
  viewer's own NSView with -setView: - DOES create a valid context (glGetString
  reports the real renderer) but nothing is ever composited to the screen on
  macOS 11+. Every AppKit view is layer-backed there, and a raw NSOpenGLContext
  surface does not participate in that layer tree; -setWantsLayer:NO on a view
  that is already inside a layer-backed hierarchy does not reliably undo it.

  Lazarus' own TOpenGLControl solves this by BEING an NSOpenGLView. We do the
  same, one level down: DoCreateContext builds a dedicated NSOpenGLView
  (TGLCocoaGLView) that fills the LCL viewer's content view and owns the
  drawable. GLScene keeps driving rendering explicitly through DoActivate /
  SwapBuffers; the child view's own -drawRect: is a no-op. Mouse events that
  land on the child are forwarded to its superview so the LCL control (and thus
  VMA's feature picking) still receives them.

  Reference material this was adapted from:
    * <lazarus>/components/opengl/glcocoanscontext.pas  (Lazarus' working
      NSOpenGLView binding; primary template for the ObjC-bridge calls)
    * GLCarbonContext.pas                               (the retired backend;
      structural template - same method shapes)

  OBJECTIVE-C MEMORY MANAGEMENT
  ----------------------------
  The bridge exposes manual retain/release. Every object this unit creates with
  <Class>.alloc.init... carries a +1 retain count and is released exactly once
  in DoDestroyContext (FRC, FPixelFormat, FGLView). FHost is borrowed from LCL
  and never released here. No autorelease pool is created - the LCL Cocoa
  widgetset installs one around the run loop, and DoCreateContext runs on the
  main thread inside it.

  History :
     2026-08-30 - Creation. Native macOS (Cocoa) OpenGL context backend for
                  the GLScene viewer, replacing the retired Carbon path.
  ============================================================================ }

unit GLCocoaContext;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}   // enables objcclass / message-send syntax / CocoaAll

{$I GLScene.inc}

interface

uses
  MacOSAll, CocoaAll,                          // NSOpenGL*, NSView, NSEvent, ...
  Classes, SysUtils, LCLType, LCLProc, Controls, Forms,
  CocoaPrivate,                                // NSObject.lclContentView category
  GLCrossPlatform, GLContext,                  // TGLContext + the abstract contract
  OpenGLAdapter, OpenGLTokens;                 // InitOpenGL, GL entry-point loader

type

  { TGLCocoaGLView
    ---------------
    The actual on-screen GL surface. It is added as a child of the LCL viewer's
    NSView and made to fill it.

    * drawRect: is deliberately empty. GLScene renders on its own schedule
      (timer / Invalidate -> LM_PAINT -> TGLSceneBuffer.Render), calling this
      backend's DoActivate + SwapBuffers. Letting AppKit also paint here would
      only fight that.
    * The mouse handlers forward to -superview so the LCL host control keeps
      getting events. Without this, an NSOpenGLView child silently swallows
      every click that lands on the globe and VMA's crater picking stops
      working. }
  TGLCocoaGLView = objcclass(NSOpenGLView)
  public
    procedure drawRect(dirtyRect: NSRect); override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure rightMouseDragged(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;
  end;

  { TGLCocoaContext
    ---------------
    GLScene context driver backed by Cocoa. Implements every abstract method of
    TGLContext. Mirrors the structure of the retired TGLCarbonContext so the two
    can be compared hunk for hunk. }
  TGLCocoaContext = class(TGLContext)
  private
    FRC: NSOpenGLContext;              // the rendering context. OWNED (alloc/release).
    FPixelFormat: NSOpenGLPixelFormat; // OWNED. Kept alive for the life of FRC / FGLView.
    FGLView: TGLCocoaGLView;           // on-screen surface, child of FHost. OWNED.
    FHost: NSView;                     // LCL viewer's content view. BORROWED - never released.
    FShareContext: TGLCocoaContext;    // sibling context to share lists with, or nil.
    FMemoryContext: Boolean;           // True when built by DoCreateMemoryContext (no FGLView).

    { Build FPixelFormat from the GLScene buffer settings (ColorBits, DepthBits,
      AntiAliasing, ...). AMemory omits the double buffer for the offscreen path.
      Releases any previous FPixelFormat first. Raises EGLContext on failure. }
    procedure ChoosePixelFormat(AMemory: Boolean);

    { The NSOpenGLContext to share display lists / textures with, resolved from
      the GLScene ServiceContext or an explicit DoShareLists partner. nil = none. }
    function  ShareRC: NSOpenGLContext;
  protected
    // ---- TGLContext abstract interface -------------------------------------
    procedure DoCreateContext(ADeviceHandle: HDC); override;
    procedure DoCreateMemoryContext(outputDevice: HWND; width, height: Integer;
      BufferCount: integer); override;
    function  DoShareLists(aContext: TGLContext): Boolean; override;
    procedure DoDestroyContext; override;
    procedure DoActivate; override;
    procedure DoDeactivate; override;

    property  RenderingContext: NSOpenGLContext read FRC;
  public
    constructor Create; override;
    destructor Destroy; override;

    function IsValid: Boolean; override;
    procedure SwapBuffers; override;
    function RenderOutputDevice: Pointer; override;
  end;

implementation

uses
  GLState, GLSLog;

resourcestring
  cCtxNoView          = 'GLCocoaContext: unable to resolve an NSView for the output device';
  cCtxPixelFmtFailed  = 'GLCocoaContext: failed to create NSOpenGLPixelFormat';
  cCtxCreateFailed    = 'GLCocoaContext: failed to create NSOpenGLContext';
  cCtxViewFailed      = 'GLCocoaContext: failed to create NSOpenGLView';
  cCtxActivateFailed  = 'GLCocoaContext: context activation failed';
  cCtxIncompatible    = 'GLCocoaContext: incompatible contexts';

const
  // FPC's CocoaAll spells the legacy (2.1 compatibility) profile value
  // NSOpenGLProfileVersionLegacy; older headers called it NSOpenGLProfileLegacy.
  cGLProfileLegacy     = NSOpenGLPixelFormatAttribute(NSOpenGLProfileVersionLegacy);
  // NSAutoresizingMaskOptions bits (not exported by every CocoaAll revision).
  cNSViewWidthSizable  = 2;
  cNSViewHeightSizable = 16;

// ===========================================================================
{ TGLCocoaGLView }
// ===========================================================================

procedure TGLCocoaGLView.drawRect(dirtyRect: NSRect);
begin
  // Intentionally empty - GLScene renders explicitly via the context backend.
end;

// -- mouse forwarding --------------------------------------------------------
// Each handler passes the event up to the LCL host view (our superview) so the
// LCL control's own tracking / OnMouseXxx / hit-testing keep working. The
// "inherited" fallbacks only fire if we are somehow un-parented.

procedure TGLCocoaGLView.mouseDown(event: NSEvent);
begin
  if superview <> nil then superview.mouseDown(event) else inherited mouseDown(event);
end;

procedure TGLCocoaGLView.mouseUp(event: NSEvent);
begin
  if superview <> nil then superview.mouseUp(event) else inherited mouseUp(event);
end;

procedure TGLCocoaGLView.mouseDragged(event: NSEvent);
begin
  if superview <> nil then superview.mouseDragged(event) else inherited mouseDragged(event);
end;

procedure TGLCocoaGLView.rightMouseDown(event: NSEvent);
begin
  if superview <> nil then superview.rightMouseDown(event) else inherited rightMouseDown(event);
end;

procedure TGLCocoaGLView.rightMouseUp(event: NSEvent);
begin
  if superview <> nil then superview.rightMouseUp(event) else inherited rightMouseUp(event);
end;

procedure TGLCocoaGLView.rightMouseDragged(event: NSEvent);
begin
  if superview <> nil then superview.rightMouseDragged(event) else inherited rightMouseDragged(event);
end;

procedure TGLCocoaGLView.otherMouseDown(event: NSEvent);
begin
  if superview <> nil then superview.otherMouseDown(event) else inherited otherMouseDown(event);
end;

procedure TGLCocoaGLView.otherMouseUp(event: NSEvent);
begin
  if superview <> nil then superview.otherMouseUp(event) else inherited otherMouseUp(event);
end;

procedure TGLCocoaGLView.scrollWheel(event: NSEvent);
begin
  if superview <> nil then superview.scrollWheel(event) else inherited scrollWheel(event);
end;

// ===========================================================================
{ TGLCocoaContext }
// ===========================================================================

constructor TGLCocoaContext.Create;
begin
  inherited Create;
  FRC := nil;
  FPixelFormat := nil;
  FGLView := nil;
  FHost := nil;
  FShareContext := nil;
  FMemoryContext := False;
end;

destructor TGLCocoaContext.Destroy;
begin
  // TGLContext.DestroyContext (hence DoDestroyContext) has already run by the
  // time the base destructor finishes its own teardown; nothing extra here.
  inherited Destroy;
end;

procedure TGLCocoaContext.ChoosePixelFormat(AMemory: Boolean);

  { Build one NSOpenGLPixelFormat. WantAccelerated demands a hardware renderer
    (the normal case); pass False for the software-renderer fallback. Returns
    nil if the window server has no format matching the request. }
  function TryFormat(WantAccelerated: Boolean): NSOpenGLPixelFormat;
  var
    attrs: array[0..31] of NSOpenGLPixelFormatAttribute;
    n: Integer;

    procedure Add(a: NSOpenGLPixelFormatAttribute);
    begin
      attrs[n] := a;    // 32 slots is ample for the fixed set below (~20 used)
      Inc(n);
    end;

  begin
    n := 0;

    // GLScene's default pipeline is fixed-function (glBegin/glMatrixMode/...),
    // which only exists in the legacy 2.1 compatibility profile. Requesting a
    // 3.2+ core profile here would break CULL_FACE / DEPTH_BUFFER handling and
    // every immediate-mode call GLScene makes.
    Add(NSOpenGLPFAOpenGLProfile); Add(cGLProfileLegacy);

    Add(NSOpenGLPFAColorSize);     Add(24);

    if DepthBits > 0 then                     // GLScene TGLSceneBuffer.DepthBits
    begin
      Add(NSOpenGLPFADepthSize);
      if DepthBits <= 16 then Add(16) else Add(24);
    end;

    if AlphaBits > 0 then
    begin
      Add(NSOpenGLPFAAlphaSize);   Add(NSOpenGLPixelFormatAttribute(AlphaBits));
    end;

    if StencilBits > 0 then
    begin
      Add(NSOpenGLPFAStencilSize); Add(NSOpenGLPixelFormatAttribute(StencilBits));
    end;

    if (AntiAliasing <> aaDefault) and (AntiAliasing <> aaNone) then
    begin
      // A single MSAA buffer with 4 samples; GLScene's finer aaXxHQ hints are
      // mapped to the filter hint in DoCreateContext, not to distinct formats.
      Add(NSOpenGLPFAMultisample);
      Add(NSOpenGLPFASampleBuffers); Add(1);
      Add(NSOpenGLPFASamples);       Add(4);
    end;

    if not AMemory then
      Add(NSOpenGLPFADoubleBuffer);           // on-screen path is double buffered

    // Offline (non-display) GPUs are acceptable on either path: an eGPU or a
    // headless Mac's discrete GPU.
    Add(NSOpenGLPFAAllowOfflineRenderers);

    if WantAccelerated then
    begin
      // Normal case: demand a real hardware renderer, and NoRecovery so the
      // window server does not silently swap in software behind our back.
      Add(NSOpenGLPFAAccelerated);
      Add(NSOpenGLPFANoRecovery);
    end;
    // else: no NSOpenGLPFAAccelerated requirement, so MaximumPolicy is free to
    // return the Apple Software Renderer -- all a GPU-less VM or headless CI
    // box (UTM, GitHub Actions, ...) can offer. Slow, but it renders.

    Add(NSOpenGLPFAMaximumPolicy);            // pick the closest match, not exact
    Add(0);                                   // attribute list terminator

    Result := NSOpenGLPixelFormat(NSOpenGLPixelFormat.alloc).initWithAttributes(@attrs[0]);
  end;

begin
  if Assigned(FPixelFormat) then              // DoShareLists can call us twice
  begin
    FPixelFormat.release;
    FPixelFormat := nil;
  end;

  FPixelFormat := TryFormat(True);
  if not Assigned(FPixelFormat) then
  begin
    // No accelerated format -- almost always a VM or headless host with no
    // GL-capable GPU. Retry allowing the software renderer.
    GLSLogger.LogWarning('GLCocoaContext: no hardware-accelerated pixel '
      + 'format available; falling back to the software renderer');
    FPixelFormat := TryFormat(False);
  end;
  if not Assigned(FPixelFormat) then
    raise EGLContext.Create(cCtxPixelFmtFailed);
end;

function TGLCocoaContext.ShareRC: NSOpenGLContext;
begin
  Result := nil;
  // GLScene's cross-thread ServiceContext takes precedence (it owns the master
  // list of shared GL objects); otherwise an explicit DoShareLists partner.
  if (ServiceContext <> nil) and (Self <> ServiceContext)
     and (ServiceContext is TGLCocoaContext) then
    Result := TGLCocoaContext(ServiceContext).FRC
  else if Assigned(FShareContext) then
    Result := FShareContext.FRC;
end;

{ DoCreateContext
  ---------------
  Contract (from TGLContext): create a live, on-screen rendering context for the
  given device handle, load the GL entry points into FGL, and leave the context
  deactivated.

  ADeviceHandle: on this backend it is NOT an LCL HDC. GLLCLViewer.CreateWnd is
  patched so that, on Darwin, it passes the LCL widget Handle straight through
  (FOwnDC := Handle). During CreateWnd the LCL device context has no
  owning-control association yet, so there is nothing useful to extract from a
  real DC; the widget handle is an NSView subclass and that is what we need. }
procedure TGLCocoaContext.DoCreateContext(ADeviceHandle: HDC);
var
  obj: NSObject;
  shared: NSOpenGLContext;
begin
  // Loads /System/.../OpenGL.framework's libGL and populates GLScene's function
  // pointers. Cheap and idempotent after the first call.
  if not InitOpenGL then
    RaiseLastOSError;

  // Resolve the host NSView. isKindOfClass guards against ever being handed a
  // real TCocoaContext (HDC) here - calling lclContentView on that would
  // dereference a non-NSObject and crash.
  FHost := nil;
  if ADeviceHandle <> 0 then
  begin
    obj := NSObject(ADeviceHandle);
    if obj.isKindOfClass(NSView) then
      FHost := obj.lclContentView;            // LCL category: the drawable subview
  end;
  if not Assigned(FHost) then
  begin
    GLSLogger.LogError(Format('GLCocoaContext: no NSView from handle %d', [PtrInt(ADeviceHandle)]));
    raise EGLContext.Create(cCtxNoView);
  end;

  ChoosePixelFormat(False);

  // Create our own NSOpenGLContext (rather than letting the NSOpenGLView make
  // one implicitly) so that list sharing actually works: an implicitly created
  // context ignores the shareContext argument.
  shared := ShareRC;
  FRC := NSOpenGLContext(NSOpenGLContext.alloc).initWithFormat_shareContext(FPixelFormat, shared);
  if not Assigned(FRC) and Assigned(shared) then
  begin
    // Sharing refused (e.g. incompatible pixel format) - fall back to unshared.
    GLSLogger.LogWarning(glsFailedToShare);
    FRC := NSOpenGLContext(NSOpenGLContext.alloc).initWithFormat_shareContext(FPixelFormat, nil);
  end;
  if not Assigned(FRC) then
    raise EGLContext.Create(cCtxCreateFailed);

  // The dedicated on-screen GL surface (see the unit header for why a child
  // NSOpenGLView instead of -setView: on FHost directly).
  FGLView := TGLCocoaGLView(TGLCocoaGLView.alloc).initWithFrame_pixelFormat(FHost.bounds, FPixelFormat);
  if not Assigned(FGLView) then
    raise EGLContext.Create(cCtxViewFailed);
  FGLView.setOpenGLContext(FRC);              // bind our shared context to the view
  FRC.setView(FGLView);                       // ... and the view as the drawable
  FGLView.setAutoresizingMask(cNSViewWidthSizable or cNSViewHeightSizable);
  FHost.addSubview(FGLView);
  FRC.update;                                 // pick up the initial geometry

  FAcceleration := chaHardware;
  FMemoryContext := False;

  // TGLContext.Activate -> our DoActivate: makes the context current and
  // triggers FGL.Initialize the first time. Explicit FGL.Initialize afterwards
  // matches TGLCarbonContext and is harmless if already done.
  Activate;
  FGL.Initialize;

  // Map GLScene's antialiasing quality hint onto the multisample filter hint
  // (the pixel format above already carries the sample count).
  if AntiAliasing in [aa2xHQ, aa4xHQ, csa8xHQ, csa16xHQ] then
    GLStates.MultisampleFilterHint := hintNicest
  else if AntiAliasing in [aa2x, aa4x, csa8x, csa16x] then
    GLStates.MultisampleFilterHint := hintFastest
  else
    GLStates.MultisampleFilterHint := hintDontCare;

  if Active then
    Deactivate;                               // leave it deactivated per contract

  // Register with the ServiceContext's shared-context set so GLScene can
  // propagate shared resources to/from the background service context.
  if (ServiceContext <> nil) and (Self <> ServiceContext) then
  begin
    FSharedContexts.Add(ServiceContext);
    PropagateSharedContext;
  end;

  GLSLogger.LogInfo('GLCocoaContext: on-screen NSOpenGLView context created');
end;

{ DoCreateMemoryContext
  ---------------------
  Contract: create an off-screen rendering context (GLScene uses it for
  thumbnail / render-to-texture work) with no on-screen drawable. GLScene binds
  an FBO to it after creation, so a plain view-less NSOpenGLContext is enough.

  NOTE: implemented for completeness and to satisfy the abstract method, but VMA
  does not currently exercise this path on macOS - treat as lightly tested.
  outputDevice / width / height / BufferCount are advisory only for an FBO-based
  offscreen context and are not used here beyond BufferCount's MRT hint being
  left to GLScene. }
procedure TGLCocoaContext.DoCreateMemoryContext(outputDevice: HWND;
  width, height: Integer; BufferCount: integer);
var
  shared: NSOpenGLContext;
begin
  if not InitOpenGL then
    RaiseLastOSError;

  FHost := nil;
  FGLView := nil;
  ChoosePixelFormat(True);                    // no double buffer for offscreen

  shared := ShareRC;
  FRC := NSOpenGLContext(NSOpenGLContext.alloc).initWithFormat_shareContext(FPixelFormat, shared);
  if not Assigned(FRC) and Assigned(shared) then
  begin
    GLSLogger.LogWarning(glsFailedToShare);
    FRC := NSOpenGLContext(NSOpenGLContext.alloc).initWithFormat_shareContext(FPixelFormat, nil);
  end;
  if not Assigned(FRC) then
    raise EPBuffer.Create(cCtxCreateFailed);  // EPBuffer: matches the Carbon backend

  FAcceleration := chaHardware;
  FMemoryContext := True;

  Activate;
  FGL.Initialize;

  if AntiAliasing in [aa2xHQ, aa4xHQ, csa8xHQ, csa16xHQ] then
    GLStates.MultisampleFilterHint := hintNicest
  else if AntiAliasing in [aa2x, aa4x, csa8x, csa16x] then
    GLStates.MultisampleFilterHint := hintFastest
  else
    GLStates.MultisampleFilterHint := hintDontCare;

  if Active then
    Deactivate;

  if (ServiceContext <> nil) and (Self <> ServiceContext) then
  begin
    FSharedContexts.Add(ServiceContext);
    PropagateSharedContext;
  end;

  GLSLogger.LogInfo('GLCocoaContext: off-screen (memory) rendering context created');
end;

{ DoShareLists
  -----------
  Contract: arrange for this context to share GL objects with aContext.
  NSOpenGLContext sharing is fixed at creation time, so if we already have a
  live FRC we must drop it and let the next DoCreateContext rebuild it with the
  share partner recorded here. Returns True when a rebuild was forced. }
function TGLCocoaContext.DoShareLists(aContext: TGLContext): Boolean;
begin
  Result := False;
  if aContext is TGLCocoaContext then
  begin
    if Assigned(FRC) and (FRC <> TGLCocoaContext(aContext).FRC) then
    begin
      DestroyContext;                         // -> DoDestroyContext
      FShareContext := TGLCocoaContext(aContext);
      Result := True;
    end
    else
      FShareContext := TGLCocoaContext(aContext);
  end
  else
    raise Exception.Create(cCtxIncompatible);
end;

{ DoDestroyContext
  ---------------
  Contract: fully tear down the context. Safe to call when partially
  constructed (any of the fields may be nil) and safe to call more than once.
  Release order: current-context guard, then the view (which references FRC),
  then FRC, then the pixel format. }
procedure TGLCocoaContext.DoDestroyContext;
begin
  if NSOpenGLContext.currentContext = FRC then
    NSOpenGLContext.clearCurrentContext;

  if Assigned(FGLView) then
  begin
    FGLView.clearGLContext;                   // detach the context from the view
    FGLView.removeFromSuperview;
    FGLView.release;                          // balances TGLCocoaGLView.alloc
    FGLView := nil;
  end;

  if Assigned(FRC) then
  begin
    FRC.clearDrawable;
    FRC.release;                              // balances NSOpenGLContext.alloc
    FRC := nil;
  end;
  if Assigned(FPixelFormat) then
  begin
    FPixelFormat.release;                     // balances NSOpenGLPixelFormat.alloc
    FPixelFormat := nil;
  end;

  FHost := nil;                               // borrowed, do not release
  FShareContext := nil;
end;

{ DoActivate
  ---------
  Contract: make this context the current GL context. GLScene calls it before
  every render pass and expects FGL to be usable on return.

  Two macOS-specific chores happen here first:
    * Resize sync. LCL resizes the host view (Align=alClient) but knows nothing
      about our NSOpenGLView child, so we match the child's frame to the host
      on every activation. Cheap; -setFrame is only issued on an actual change.
    * -update. NSOpenGLContext must be told when its view's geometry or the
      surrounding window changes; doing it here covers move/resize/display
      changes without hooking AppKit notifications. }
procedure TGLCocoaContext.DoActivate;
var
  hb: NSRect;
begin
  if not Assigned(FRC) then
    raise EGLContext.Create(cCtxActivateFailed);

  if (not FMemoryContext) and Assigned(FGLView) and Assigned(FHost) then
  begin
    hb := FHost.bounds;
    if (FGLView.frame.size.width  <> hb.size.width)
       or (FGLView.frame.size.height <> hb.size.height)
       or (FGLView.frame.origin.x    <> hb.origin.x)
       or (FGLView.frame.origin.y    <> hb.origin.y) then
      FGLView.setFrame(hb);
    if FGLView.window <> nil then             // -update is only valid once on a window
      FRC.update;
  end;

  FRC.makeCurrentContext;

  if not FGL.IsInitialized then
    FGL.Initialize;                           // first activation loads GL entry points
end;

{ DoDeactivate - contract: no context current on this thread afterwards. }
procedure TGLCocoaContext.DoDeactivate;
begin
  NSOpenGLContext.clearCurrentContext;
end;

function TGLCocoaContext.IsValid: Boolean;
begin
  Result := Assigned(FRC);
end;

{ SwapBuffers
  ----------
  Contract: present the rendered frame. Apple documents -flushBuffer as
  equivalent to glFlush() on a single-buffered context, so this is correct
  whether or not rcoDoubleBuffered was honoured by the pixel format. Before a
  drawable is attached it is a harmless no-op. }
procedure TGLCocoaContext.SwapBuffers;
begin
  if not Assigned(FRC) then
    Exit;
  FRC.flushBuffer;
end;

{ RenderOutputDevice - contract: platform handle for advanced callers. GLScene
  core never dereferences it on this backend; nil, as in TGLCarbonContext. }
function TGLCocoaContext.RenderOutputDevice: Pointer;
begin
  Result := nil;
end;

end.
