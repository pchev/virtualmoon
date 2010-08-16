//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLLinuxContext<p>

   Linux specific Context.<p>

   <b>History : </b><font size=-1><ul>
      <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTracekrID=1681585)
      <li>15/02/07 - DaStr - Integer -> Cardinal because $R- was removed in GLScene.pas
                             Added a header and restored version history  
      <li>05/07/04 - lirey - More corrections for Linux
      <li>03/07/04 - lirey - Corrections for Linux
      <li>23/07/03 - EG - OpenGL12 references changed to OpenGL1x
      <li>30/05/03 - EG - Kylix compatibility changes (compiles, but untested)
      <li>29/11/02 - EG - Access to outputDevice
      <li>24/01/02 - EG - Initial version
}
unit GLLinuxContext;

interface

{$i GLScene.inc}

{$IFNDEF UNIX} {$MESSAGE Error 'Unit is Linux specific'} {$ENDIF}

uses
  XLib, Classes, SysUtils, GLContext, QForms, OpenGL1x, Qt;

type

   // TGLLinuxContext
   //
   {: A context driver for standard XOpenGL (via MS OpenGL). }
   TGLLinuxContext = class (TGLContext)
      private
         { Private Declarations }
         FRenderingContext: GLXContext;
         FOutputDevice: QWidgetH;
      protected
         { Protected Declarations }
         procedure DoCreateContext(outputDevice : Cardinal); override;
         procedure DoCreateMemoryContext(outputDevice: Cardinal; width, height : Integer; BufferCount : integer); override;
         procedure DoShareLists(aContext : TGLContext); override;
         procedure DoDestroyContext; override;
         procedure DoActivate; override;
         procedure DoDeactivate; override;

         property RenderingContext: GLXContext read FRenderingContext;
      public
         { Public Declarations }
         function IsValid : Boolean; override;
         procedure SwapBuffers; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  GLCrossPlatform;

resourcestring
   cContextActivationFailed =    'Context activation failed: %X';

// ------------------
// ------------------ TGLLinuxContext ------------------
// ------------------

var
//   vLastPixelFormat : Integer;
   vLastVendor : String;

// DoCreateContext
//
procedure TGLLinuxContext.DoCreateContext(outputDevice : Cardinal);
var
  winattr: XWindowAttributes;
  vitemp: xlib.XVisualInfo;
  nret: Integer;
  glwin: Cardinal;
  vi: xlib.PXvisualInfo;
begin
   // Just in case it didn't happen already.
   if not InitOpenGL then RaiseLastOSError;
    { Create OpenGL context }
    FOutputDevice := QWidgetH(outputDevice);
    glwin := QWidget_winId(FOutputDevice);
    xlib.XGetWindowAttributes(Application.Display, glwin, @winattr);
    vitemp.visual := winattr.visual;
    vitemp.visualid := XVisualIDFromVisual(vitemp.visual);
    vi := XGetVisualInfo(Application.Display, VisualIDMask, @vitemp, @nret);
    FRenderingContext := glXCreateContext(Application.Display, vi, nil, True);
    if RenderingContext = nil then
      raise Exception.Create('Failed to create rendering context');
    if RenderingContext = GLX_BAD_CONTEXT then
      raise Exception.Create('bad context');
end;

// DoCreateMemoryContext
//
procedure TGLLinuxContext.DoCreateMemoryContext(outputDevice: Cardinal; width, height : Integer; BufferCount : integer);
begin
  {$MESSAGE Warn 'DoCreateMemoryContext: Needs to be implemented'}
end;

// DoShareLists
//
procedure TGLLinuxContext.DoShareLists(aContext : TGLContext);
begin
  {$MESSAGE Warn 'DoShareLists: Needs to be implemented'}
end;

// DoDestroyContext
//
procedure TGLLinuxContext.DoDestroyContext;
begin
  if (glXGetCurrentContext() = RenderingContext) and
     (not glXMakeCurrent(Application.Display, 0, nil)) then
    raise Exception.Create('Failed to deselect rendering context');
  glXDestroyContext(Application.Display, RenderingContext);
  FRenderingContext := nil;
  FOutputDevice := nil;
end;

// DoActivate
//
procedure TGLLinuxContext.DoActivate;
begin
   if not glXMakeCurrent(Application.Display, QWidget_winId(FoutputDevice), RenderingContext) then
     raise EGLContext.Create(Format(cContextActivationFailed, [GetLastError]));

   // The extension function addresses are unique for each pixel format. All rendering
   // contexts of a given pixel format share the same extension function addresses.
//   pixelFormat:=GetPixelFormat(FDC);
//   if PixelFormat<>vLastPixelFormat then begin
      if glGetString(GL_VENDOR)<>vLastVendor then begin
         ReadExtensions;
         ReadImplementationProperties;
         vLastVendor:=glGetString(GL_VENDOR);
      end
//      else begin
//         ReadWGLExtensions;
//         ReadWGLImplementationProperties;
//      end;
//      vLastPixelFormat:=pixelFormat;
//   end;
end;

// Deactivate
//
procedure TGLLinuxContext.DoDeactivate;
begin
  glXMakeCurrent(Application.Display, 0, nil);
end;

// IsValid
//
function TGLLinuxContext.IsValid : Boolean;
begin
   Result:=(FRenderingContext<>nil);
end;

// SwapBuffers
//
procedure TGLLinuxContext.SwapBuffers;
begin
   if (FRenderingContext<>nil) and (rcoDoubleBuffered in Options) then
    glXSwapBuffers(Application.Display, QWidget_winId(FOutputDevice));
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterGLContextClass(TGLLinuxContext);

end.
