//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLPBuffer<p>

  Simple handling of pixelbuffers.<p>
  
  TGLPixelBuffer can be used for offscreen rendering.<p>
  It does not require a fully-functional rendering context.<p>

  <b>Historique : </b><font size=-1><ul>
      <li>08/03/10 - Yar - Added more conditional brackets for unix systems
      <li>27/01/10 - Yar - Updated header and moved to the /Source/Base/ folder
      <li>26/01/10 - DaStr - Bugfixed range check error, for real ;)
                             Enhanced TGLPixelBuffer.IsLost()
      <li>24/01/10 - Yar - Removed initialization from constructor,
                           changes of use of desktop windows on foreground
                           (improved work on Delphi7 and Lazarus)
      <li>21/01/10 - DaStr - Bugfixed range check error
      <li>22/01/10 - Yar - Added to GLScene (contributed by Sascha Willems)
  </ul></font>

   Copyright � 2003-2009 by Sascha Willems - http://www.saschawillems.de

   The contents of this file are subject to the Mozilla Public License
   Version 1.1 (the "License"; you may not use this file except in
   compliance with the License. You may obtain a copy of the License at
   http://www.mozilla.org/MPL/

   Software distributed under the License is distributed on an "AS IS"
   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
   License for the specific language governing rights and limitations
   under the License.
}

unit GLPBuffer;

{$i GLScene.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
  Classes,
{$ENDIF}
  SysUtils,
  OpenGL1x;

type

  TGLPixelBuffer = class
  private
{$IFDEF MSWINDOWS}
    DC: HDC;
    RC: HGLRC;
    ParentDC: HDC;
    ParentRC: HGLRC;
    fHandle: HPBUFFERARB;
{$ELSE}
    DC: LongWord;
    RC: LongWord;
    ParentDC: LongWord;
    ParentRC: LongWord;
    fHandle: LongInt;
{$ENDIF}
    fWidth: GLint;
    fHeight: GLint;
    fTextureID: GLuint;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize(pWidth, pHeight: integer);
    function  IsLost: boolean;
    procedure Enable;
    procedure Disable;
    procedure Bind;
    procedure Release;

    property Handle: GLint read fHandle;
    property Width: GLint read fWidth;
    property Height: GLint read fHeight;
    property TextureID: GLuint read fTextureID;
  end;

  EGLPixelBuffer = class(Exception);

implementation

constructor TGLPixelBuffer.Create;
begin
  inherited Create;
  ParentDC := 0;
  ParentRC := 0;
end;

procedure TGLPixelBuffer.Initialize(pWidth, pHeight: integer);
{$IFDEF MSWINDOWS}
const
  PixelFormatAttribs: array[0..12] of TGLInt =
   (WGL_SUPPORT_OPENGL_ARB, GL_TRUE,
    WGL_DRAW_TO_PBUFFER_ARB, GL_TRUE,
    WGL_COLOR_BITS_ARB, 24,
    WGL_ALPHA_BITS_ARB, 8,
    WGL_DEPTH_BITS_ARB, 24,
    WGL_DOUBLE_BUFFER_ARB, GL_FALSE, 0);

  PixelBufferAttribs: array[0..4] of TGLInt =
    (WGL_TEXTURE_FORMAT_ARB, WGL_TEXTURE_RGBA_ARB,
    WGL_TEXTURE_TARGET_ARB,
    WGL_TEXTURE_2D_ARB, 0);

  EmptyF: TGLFLoat = 0;
var
  PFormat: array[0..64] of TGLInt;
  NumPFormat: TGLenum;
  TempW, TempH: TGLInt;
{$ELSE}

{$ENDIF}
begin
  fWidth := pWidth;
  fHeight := pHeight;
{$IFDEF MSWINDOWS}
  ParentDC := wglGetCurrentDC;
  ParentRC := wglGetCurrentContext;
  if ParentDC = 0 then
  begin
    ParentDC := GetDC(GetForegroundWindow);
    if ParentDC = 0 then
      raise EGLPixelBuffer.Create(
        'PixelBuffer->wglGetCurrentDC->Couldn''t obtain valid device context');
  end;

  if not wglChoosePixelFormatARB(ParentDC, @PixelFormatAttribs, @EmptyF,
    Length(PFormat), @PFormat, @NumPFormat) then
    raise EGLPixelBuffer.Create(
      'PixelBuffer->wglChoosePixelFormatARB->No suitable pixelformat found');

  fHandle := wglCreatePBufferARB(ParentDC, PFormat[0], fWidth, fHeight,
    @PixelBufferAttribs);
  if fHandle <> 0 then
  begin
    wglQueryPbufferARB(fHandle, WGL_PBUFFER_WIDTH_ARB, @TempW);
    wglQueryPbufferARB(fHandle, WGL_PBUFFER_HEIGHT_ARB, @TempH);
  end
  else
    raise EGLPixelBuffer.Create(
      'PixelBuffer->wglCreatePBufferARB->Couldn''t obtain valid handle');

  DC := wglGetPBufferDCARB(fHandle);
  if DC = 0 then
    raise EGLPixelBuffer.Create(
      'PixelBuffer->wglGetPBufferDCARB->Couldn''t obtain valid DC for PBuffer');

  RC := wglCreateContext(DC);
  if RC = 0 then
    raise EGLPixelBuffer.Create(
      'PixelBuffer->wglGetPBufferDCARB->Couldn''t create rendercontext for PBuffer');

  wglMakeCurrent(DC, RC);
  glGenTextures(1, @fTextureID);
{$ELSE}

{$ENDIF}
end;

destructor TGLPixelBuffer.Destroy;
begin
{$IFDEF MSWINDOWS}
  Disable;
  if (fHandle <> 0) then
  begin
    wglDeleteContext(RC);
    wglReleasePbufferDCARB(fHandle, DC);
    wglDestroyPbufferARB(fHandle);
  end;
{$ELSE}

{$ENDIF}
  inherited;
end;

function TGLPixelBuffer.IsLost: boolean;
var
  Flag: TGLUInt;
begin
{$IFDEF MSWINDOWS}
  Assert(fHandle <> 0);
  if wglQueryPbufferARB(fHandle, WGL_PBUFFER_LOST_ARB, @Flag) then
  begin
    Result := (Flag <> 0);
  end
  else
    Result := False;
{$ELSE}

{$ENDIF}
end;

procedure TGLPixelBuffer.Enable;
begin
{$IFDEF MSWINDOWS}
  ParentDC := wglGetCurrentDC;
  ParentRC := wglGetCurrentContext;
  wglMakeCurrent(DC, RC);
{$ELSE}

{$ENDIF}
end;

procedure TGLPixelBuffer.Disable;
begin
{$IFDEF MSWINDOWS}
  if (ParentDC = 0) or (ParentRC = 0) then
    wglMakeCurrent(0, 0)
  else
    wglMakeCurrent(ParentDC, ParentRC);
{$ELSE}

{$ENDIF}
end;

procedure TGLPixelBuffer.Bind;
begin
{$IFDEF MSWINDOWS}
  Assert(fHandle <> 0);
  wglBindTexImageARB(fHandle, WGL_FRONT_LEFT_ARB);
{$ELSE}

{$ENDIF}
end;

procedure TGLPixelBuffer.Release;
begin
{$IFDEF MSWINDOWS}
  Assert(fHandle <> 0);
  wglReleaseTexImageARB(fHandle, WGL_FRONT_LEFT_ARB);
{$ELSE}

{$ENDIF}
end;

end.

