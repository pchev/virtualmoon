{: gllingtkcontext<p>

   GTK specific Context.<p>

   <b>History : </b><font size=-1><ul>
      <li>14/01/05 - CU - Creation
   </ul></font>
}
unit GLLinGTKContext;

interface

{$i ../GLScene.inc}

uses
  xlib, Classes, sysutils, GLCrossPlatform, GLContext, LCLProc, Forms, Controls, OpenGL1x,
  x, xutil, GTKProc,
{$ifdef LCLGTK2}
  gtk2, gdk2, gdk2x, gtkdef;
{$else}
  gtk, gtkdef, gdk;
{$endif}

type
   // TGLGTKContext
   //
   {: A context driver for standard XOpenGL. }
   TGLGTKContext = class (TGLContext)
      private
         { Private Declarations }
         FRenderingContext: GLXContext;
         FGTKWidget : PGTKWidget;
         CurXDisplay: Pointer;
         CurXWindow: LongInt;
         FiAttribs : packed array of Integer;
      protected
         { Protected Declarations }
         procedure ClearIAttribs;
         procedure AddIAttrib(attrib, value : Integer);
         procedure ChangeIAttrib(attrib, newValue : Integer);
         procedure DropIAttrib(attrib : Integer);

         procedure DestructionEarlyWarning(sender : TObject);

         procedure DoCreateContext(outputDevice : HDC); override;
         procedure DoCreateMemoryContext(outputDevice : HDC;width, height : Integer; BufferCount : integer); override;
         procedure DoShareLists(aContext : TGLContext); override;
         procedure DoDestroyContext; override;
         procedure DoActivate; override;
         procedure DoDeactivate; override;

         property RenderingContext: GLXContext read FRenderingContext;
      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

         function IsValid : Boolean; override;
         procedure SwapBuffers; override;

         function RenderOutputDevice : Integer; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------


resourcestring
   cIncompatibleContexts =       'Incompatible contexts';
   cDeleteContextFailed =        'Delete context failed';
   cContextActivationFailed =    'Context activation failed: %X, %s';
   cContextDeactivationFailed =  'Context deactivation failed';
   cUnableToCreateLegacyContext= 'Unable to create legacy context';

// ------------------
// ------------------ TGLGTKContext ------------------
// ------------------

var
//   vLastPixelFormat : Integer;
   vLastVendor : String;


procedure TGLGTKContext.ClearIAttribs;
begin
   SetLength(FiAttribs, 1);
   FiAttribs[0]:=0;
end;

procedure TGLGTKContext.AddIAttrib(attrib, value: Integer);
var
   n : Integer;
begin
   n:=Length(FiAttribs);
   SetLength(FiAttribs, n+2);
   FiAttribs[n-1]:=attrib;
   FiAttribs[n]:=value;
   FiAttribs[n+1]:=0;
end;

procedure TGLGTKContext.ChangeIAttrib(attrib, newValue: Integer);
var
   i : Integer;
begin
   i:=0;
   while i<Length(FiAttribs) do begin
      if FiAttribs[i]=attrib then begin
         FiAttribs[i+1]:=newValue;
         Exit;
      end;
      Inc(i, 2);
   end;
   AddIAttrib(attrib, newValue);
end;

procedure TGLGTKContext.DropIAttrib(attrib: Integer);
var
   i : Integer;
begin
   i:=0;
   while i<Length(FiAttribs) do begin
      if FiAttribs[i]=attrib then begin
         Inc(i, 2);
         while i<Length(FiAttribs) do begin
            FiAttribs[i-2]:=FiAttribs[i];
            Inc(i);
         end;
         SetLength(FiAttribs, Length(FiAttribs)-2);
         Exit;
      end;
      Inc(i, 2);
   end;
end;

procedure TGLGTKContext.DestructionEarlyWarning(sender: TObject);
begin
   DestroyContext;
end;

// DoCreateContext
//
procedure TGLGTKContext.DoCreateContext(outputDevice : HDC);
var
  winattr: TXWindowAttributes;
  vitemp: TXVisualInfo;
  nret: Integer;
  vi: PXvisualInfo;
  //LCLObject: TObject;
  //CurWinControl: TWinControl;
  CurScreen : Integer;
begin
   // Just in case it didn't happen already.
   if not InitOpenGL then RaiseLastOSError;
    { Create OpenGL context }
    //LCLObject:=TObject(outputDevice);
    //CurWinControl:=TWinControl(LCLObject);
    //fGTKWidget := GetFixedWidget(pgtkwidget(CurWinControl.Handle));
    fGTKWidget := GetFixedWidget(TGtkDeviceContext(outputDevice).Widget);
    // Dirty workaround: force realize
    gtk_widget_realize(FGTKWidget);
    
    // GTK1 -- old
    {$ifdef LCLGTK2}
    gtk_widget_set_double_buffered(FGTKWidget, False);
    CurXDisplay:=GDK_WINDOW_XDISPLAY (PGdkDrawable(fGTKWidget^.window));
    CurXWindow:=GDK_WINDOW_XWINDOW (PGdkDrawable(fGTKWidget^.window));
    {$else}
    CurXDisplay:=GDK_WINDOW_XDISPLAY (PGdkWindowPrivate(fGTKWidget^.window));
    CurXWindow:=GDK_WINDOW_XWINDOW (PGdkWindowPrivate(fGTKWidget^.window));
    {$endif}

    XGetWindowAttributes(CurXDisplay, CurXWindow, @winattr);
    vitemp.visual := winattr.visual;
    vitemp.visualid := XVisualIDFromVisual(vitemp.visual);
    vi := XGetVisualInfo(CurXDisplay, VisualIDMask, @vitemp, @nret);
    CurScreen := vi^.screen;

    {crossbuilder: better free vi instead of just nil it to avoid leak
    vi := nil;}
    XFree(vi);

    AddIAttrib(GLX_RGBA,GL_TRUE);
    
    AddIAttrib(GLX_RED_SIZE, round(ColorBits/4));
    AddIAttrib(GLX_GREEN_SIZE, round(ColorBits/4));
    AddIAttrib(GLX_BLUE_SIZE, round(ColorBits/4));
    if AlphaBits>0 then
     AddIAttrib(GLX_ALPHA_SIZE, AlphaBits);
    AddIAttrib(GLX_DEPTH_SIZE, DepthBits);
    if StencilBits>0 then
       AddIAttrib(GLX_STENCIL_SIZE, StencilBits);
    if AccumBits>0 then
      begin
       AddIAttrib(GLX_ACCUM_RED_SIZE, round(AccumBits/4));
       AddIAttrib(GLX_ACCUM_GREEN_SIZE, round(AccumBits/4));
       AddIAttrib(GLX_ACCUM_BLUE_SIZE, round(AccumBits/4));
      end;
    if AuxBuffers>0 then
       AddIAttrib(GLX_AUX_BUFFERS, AuxBuffers);
    if (rcoDoubleBuffered in Options) then
       AddIAttrib(GLX_DOUBLEBUFFER,GL_TRUE);
    vi := glXChooseVisual(CurXDisplay,CurScreen, @FiAttribs[0]);
    if not Assigned(vi) and (DepthBits>=32) then
      ChangeIAttrib(WGL_DEPTH_BITS_ARB, 24);
    vi := glXChooseVisual(CurXDisplay,CurScreen, @FiAttribs[0]);
    if not Assigned(vi) and (DepthBits>=24) then
      ChangeIAttrib(WGL_DEPTH_BITS_ARB, 16);
    vi := glXChooseVisual(CurXDisplay,CurScreen, @FiAttribs[0]);
    if not Assigned(vi) and (ColorBits>=24) then
      begin
        AddIAttrib(GLX_RED_SIZE, 4);
        AddIAttrib(GLX_GREEN_SIZE, 4);
        AddIAttrib(GLX_BLUE_SIZE, 4);
      end;
    if not Assigned(vi) then
      vi := XGetVisualInfo(CurXDisplay, VisualIDMask, @vitemp, @nret);
    //FRenderingContext := glXCreateContext(CurXDisplay, vi, nil, false); //Last Param (Direct Draw) schoud be true but guves some strange errors
    FRenderingContext := glXCreateContext(CurXDisplay, vi, nil, true); //Last Param (Direct Draw) schoud MUST true, otherwise I get
                                                                       //GdK-Error on machines with HW-Accel. Not at this line, but somwhere latere (didn't find where)
    XFree(vi);
    if RenderingContext = nil then
      raise Exception.Create('Failed to create rendering context');
    if PtrUInt(RenderingContext) = GLX_BAD_CONTEXT then
      raise Exception.Create('bad context');
end;

// DoCreateMemoryContext
//
procedure TGLGTKContext.DoCreateMemoryContext(outputDevice : HDC; width, height : Integer; BufferCount : integer);
begin
  {$MESSAGE Warn 'DoCreateMemoryContext: Needs to be implemented'}
end;

// DoShareLists
//
procedure TGLGTKContext.DoShareLists(aContext : TGLContext);
var
   otherRC : GLXContext;
begin
  {$MESSAGE Warn 'DoShareLists: Needs to be implemented'}
   if aContext is TGLGTKContext then begin
      otherRC:=TGLGTKContext(aContext).RenderingContext;
      // some drivers fail (access violation) when requesting to share
      // a context with itself
      if FRenderingContext<>otherRC then
         //Can't find such a function.
         //glXShareLists(FRC, otherRC);
         //Seems, a sharedList context must be given when creating the context (3. parameter of glXCeateContext)
   end else raise Exception.Create(cIncompatibleContexts);
end;

// DoDestroyContext
//
{procedure TGLGTKContext.DoDestroyContext; // GTK1 - old
begin
  if (glXGetCurrentContext() = RenderingContext) and
     (not glXMakeCurrent(PGDKWindowPrivate(fGTKWidget^.window)^.xdisplay, 0, nil)) then
    raise Exception.Create('Failed to deselect rendering context');
  glXDestroyContext(PGDKWindowPrivate(fGTKWidget^.window)^.xdisplay, RenderingContext);
  FRenderingContext := nil;
//  FOutputDevice := nil;
end;}

procedure TGLGTKContext.DoDestroyContext;
begin
  if (glXGetCurrentContext() = RenderingContext) and
     (not glXMakeCurrent(CurXDisplay, 0, nil)) then
    raise Exception.Create('Failed to deselect rendering context');
  glXDestroyContext(CurXDisplay, RenderingContext);
  FRenderingContext := nil;
//  FOutputDevice := nil;
end;

// DoActivate
//
procedure TGLGTKContext.DoActivate;
begin
  if not glXMakeCurrent(CurXDisplay,CurXWindow,FRenderingContext) then
    raise EGLContext.Create(cContextActivationFailed);

   // The extension function addresses are unique for each pixel format. All rendering
   // contexts of a given pixel format share the same extension function addresses.
  if glGetString(GL_VENDOR) <> vLastVendor then
    begin
      ReadExtensions;
      ReadImplementationProperties;
      vLastVendor:=glGetString(GL_VENDOR);
    end
end;

// Deactivate
//
procedure TGLGTKContext.DoDeactivate;
begin
  if not glXMakeCurrent(CurXDisplay,0,nil) then
    raise Exception.Create(cContextDeactivationFailed);
end;

constructor TGLGTKContext.Create;
begin
   inherited Create;
   ClearIAttribs;
end;

destructor TGLGTKContext.Destroy;
begin
  inherited Destroy;
end;

// IsValid
//
function TGLGTKContext.IsValid : Boolean;
begin
   Result:=(FRenderingContext<>nil);
end;

// SwapBuffers
//
procedure TGLGTKContext.SwapBuffers;
begin
   if (FRenderingContext<>nil) and (rcoDoubleBuffered in Options) then
    glXSwapBuffers(CurXDisplay,CurXWindow);
end;

function TGLGTKContext.RenderOutputDevice: Integer;
begin
  Result:=0;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterGLContextClass(TGLGTKContext);

end.
   
end.
