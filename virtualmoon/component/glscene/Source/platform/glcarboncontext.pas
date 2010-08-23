{: glcarboncontext<p>

   Carbon specific Context.<p>

   <b>History : </b><font size=-1><ul>
      <li>14/11/08 - Creation
   </ul></font>
}
unit GLCarbonContext;

{$i ../GLScene.inc}

interface

uses
  MacOSAll,
  Classes, sysutils, GLCrossPlatform, GLContext, LCLProc, Forms, Controls,
  OpenGL1x, agl, CarbonDef, CarbonCanvas, CarbonProc, CarbonPrivate;

type
   // TGLCarbonContext
   //
   {: A context driver for standard XOpenGL. }
   TGLCarbonContext = class (TGLContext)
      private
         { Private Declarations }
         FContext: TAGLContext;
         FBounds: TRect;
         FViewer, FForm: TControl;
         FIAttribs : packed array of Integer;

         function GetFormBounds: TRect;
         procedure BoundsChanged;
      protected
         { Protected Declarations }
         procedure ClearIAttribs;
         procedure AddIAttrib(attrib, value : Integer);
         procedure ChangeIAttrib(attrib, newValue : Integer);
         procedure DropIAttrib(attrib : Integer);

         procedure DoCreateContext(outputDevice : HDC); override;
         procedure DoCreateMemoryContext(outputDevice : HDC;width, height : Integer; BufferCount : integer); override;
         procedure DoShareLists(aContext : TGLContext); override;
         procedure DoDestroyContext; override;
         procedure DoActivate; override;
         procedure DoDeactivate; override;
      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

         function IsValid : Boolean; override;
         procedure SwapBuffers; override;

         function RenderOutputDevice : Integer; override;
   end;

implementation

var
  vLastVendor : String;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

resourcestring
   cIncompatibleContexts =       'Incompatible contexts';
   cDeleteContextFailed =        'Delete context failed';
   cContextActivationFailed =    'Context activation failed: %X, %s';
   cContextDeactivationFailed =  'Context deactivation failed';
   cUnableToCreateLegacyContext= 'Unable to create legacy context';

{ TGLCarbonContext }

function TGLCarbonContext.GetFormBounds: TRect;
begin
  Result.TopLeft := FForm.ScreenToClient(FViewer.ControlToScreen(Point(0, 0)));
  Result.Right := Result.Left + FViewer.Width;
  Result.Bottom := Result.Top + FViewer.Height;
end;

procedure TGLCarbonContext.BoundsChanged;
var
  Bounds: Array [0..3] of GLint;
begin
  Bounds[0] := FBounds.Left;
  Bounds[1] := FForm.Height - FBounds.Bottom;
  Bounds[2] := FBounds.Right - FBounds.Left;
  Bounds[3] := FBounds.Bottom - FBounds.Top;

  aglSetInteger(FContext, AGL_BUFFER_RECT, @Bounds[0]);
  aglEnable(FContext, AGL_BUFFER_RECT);

  {$MESSAGE Warn 'Removing child controls from clip region needs to be implemented'}
(*BoundsRGN := NewRgn;
  RectRgn(BoundsRGN, GetCarbonRect(TCarbonControlContext(DC).Owner.LCLObject.BoundsRect));

  aglSetInteger(FContext, AGL_CLIP_REGION, PGLInt(BoundsRGN));
  aglEnable(FContext, AGL_CLIP_REGION);*)

  aglUpdateContext(FContext);
end;

procedure TGLCarbonContext.ClearIAttribs;
begin
  SetLength(FIAttribs, 1);
  FiAttribs[0]:=0;
end;

procedure TGLCarbonContext.AddIAttrib(attrib, value: Integer);
var
  N: Integer;
begin
  N := Length(FIAttribs);
  SetLength(FIAttribs, N+2);
  FiAttribs[N-1]:=attrib;
  FiAttribs[N]:=value;
  FiAttribs[N+1]:=0;
end;

procedure TGLCarbonContext.ChangeIAttrib(attrib, newValue: Integer);
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

procedure TGLCarbonContext.DropIAttrib(attrib: Integer);
var
  i: Integer;
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

procedure TGLCarbonContext.DoCreateContext(outputDevice: HDC);
var
  DC: TCarbonDeviceContext absolute outputDevice;
  Window: WindowRef;
  Disp: GDHandle;
  PixelFmt: TAGLPixelFormat;
begin
  if not (CheckDC(outputDevice, 'DoCreateContext') or (DC is TCarbonControlContext)) then
    raise EGLContext.Create('Creating context failed: invalid device context!');

  FViewer := TCarbonControlContext(DC).Owner.LCLObject;
  FForm := FViewer.GetTopParent;
  if not (FForm is TCustomForm) then
    raise EGLContext.Create('Creating context failed: control not on the form!');

  Window := TCarbonWindow((FForm as TWinControl).Handle).Window;

  // create the AGL context
  Disp := GetMainDevice();

  AddIAttrib(AGL_WINDOW, GL_TRUE);
  AddIAttrib(AGL_RGBA, GL_TRUE);

  AddIAttrib(AGL_RED_SIZE, Round(ColorBits / 3));
  AddIAttrib(AGL_GREEN_SIZE, Round(ColorBits / 3));
  AddIAttrib(AGL_BLUE_SIZE, Round(ColorBits / 3));
  AddIAttrib(AGL_DEPTH_SIZE, DepthBits);

  if AlphaBits > 0 then AddIAttrib(GLX_ALPHA_SIZE, AlphaBits);

  AddIAttrib(AGL_DEPTH_SIZE, DepthBits);

  if StencilBits > 0 then AddIAttrib(GLX_STENCIL_SIZE, StencilBits);
  if AccumBits > 0 then
  begin
    AddIAttrib(AGL_ACCUM_RED_SIZE, round(AccumBits/4));
    AddIAttrib(AGL_ACCUM_GREEN_SIZE, round(AccumBits/4));
    AddIAttrib(AGL_ACCUM_BLUE_SIZE, round(AccumBits/4));
  end;
  if AuxBuffers > 0 then AddIAttrib(AGL_AUX_BUFFERS, AuxBuffers);
  if (rcoDoubleBuffered in Options) then AddIAttrib(AGL_DOUBLEBUFFER, GL_TRUE);

  // choose the best compatible pixel format
  PixelFmt := aglChoosePixelFormat(@Disp, 1, @FIAttribs[0]);
  if PixelFmt = nil then
  begin
    if DepthBits >= 32 then ChangeIAttrib(AGL_DEPTH_SIZE, 24);
    PixelFmt := aglChoosePixelFormat(@Disp, 1, @FIAttribs[0]);
    if PixelFmt = nil then
    begin
      if DepthBits >= 24 then ChangeIAttrib(AGL_DEPTH_SIZE, 16);
      PixelFmt := aglChoosePixelFormat(@Disp, 1, @FIAttribs[0]);
      if PixelFmt = nil then
      begin
        AddIAttrib(AGL_RED_SIZE, 4);
        AddIAttrib(AGL_GREEN_SIZE, 4);
        AddIAttrib(AGL_BLUE_SIZE, 4);
        PixelFmt := aglChoosePixelFormat(@Disp, 1, @FIAttribs[0]);
      end;
    end;
  end;

  FContext := aglCreateContext(PixelFmt, nil);
  aglDestroyPixelFormat(PixelFmt);

  aglSetDrawable(FContext, GetWindowPort(Window));

  FBounds := GetFormBounds;
  BoundsChanged;

  if FContext = nil then
    raise EGLContext.Create('Failed to create rendering context!');
  if PtrUInt(FContext) = AGL_BAD_CONTEXT then
    raise EGLContext.Create('Created bad context!');
end;

procedure TGLCarbonContext.DoCreateMemoryContext(outputDevice: HDC; width,
  height: Integer; BufferCount: integer);
begin
  {$MESSAGE Warn 'DoCreateMemoryContext: Needs to be implemented'}
end;

procedure TGLCarbonContext.DoShareLists(aContext: TGLContext);
begin
  {$MESSAGE Warn 'DoShareLists: Needs to be implemented'}
end;

procedure TGLCarbonContext.DoDestroyContext;
begin
  if (aglGetCurrentContext = FContext) and
     (aglSetCurrentContext(nil) = GL_FALSE) then
    raise EGLContext.Create('Failed to deselect rendering context');

  aglDestroyContext(FContext);
  FContext := nil;
end;

procedure TGLCarbonContext.DoActivate;
var
  B: TRect;
begin
  B := GetFormBounds;
  if (B.Left <> FBounds.Left) or (B.Top <> FBounds.Top) or
    (B.Right <> FBounds.Right) or (B.Bottom <> FBounds.Bottom) then
  begin
    FBounds := B;
    BoundsChanged;
  end;

  if aglSetCurrentContext(FContext) = GL_FALSE then
    raise EGLContext.Create(cContextActivationFailed);

  if glGetString(GL_VENDOR) <> vLastVendor then
  begin
    ReadExtensions;
    ReadImplementationProperties;
    vLastVendor:=glGetString(GL_VENDOR);
  end;
end;

procedure TGLCarbonContext.DoDeactivate;
begin
  if aglSetCurrentContext(nil) = GL_FALSE then
    raise EGLContext.Create(cContextDeactivationFailed);
end;

constructor TGLCarbonContext.Create;
begin
  inherited Create;
  ClearIAttribs;
end;

destructor TGLCarbonContext.Destroy;
begin
  inherited Destroy;
end;

function TGLCarbonContext.IsValid: Boolean;
begin
  Result := (FContext <> nil);
end;

procedure TGLCarbonContext.SwapBuffers;
begin
  if (FContext <> nil) and (rcoDoubleBuffered in Options) then
    aglSwapBuffers(FContext);
end;

function TGLCarbonContext.RenderOutputDevice: Integer;
begin
  Result := 0;
end;

initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterGLContextClass(TGLCarbonContext);

end.

