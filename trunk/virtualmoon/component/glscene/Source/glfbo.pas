//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFBO<p>

   Implements FBO support for GLScene.

   Original author of the unit is Riz.
   Modified by C4 and YarUnderoaker (hope, I didn't miss anybody).

   <b>History : </b><font size=-1><ul>
      <li>23/01/10 - Yar   - Replaced TextureFormat to TextureFormatEx
      <li>22/01/10 - Yar   - Adapted to Handles of GLContext,
                             texture target unification, level and layer control
      <li>11/11/09 - DaStr - Added $I GLScene.inc
      <li>09/11/09 - DaStr - Initial version (contributed to GLScene)
   </ul></font>
}
unit GLFBO;

interface

{$I GLScene.inc}

uses
  OpenGL1x,
  GLScene, GLContext, GLTexture, GLColor, GLRenderContextInfo;

const
  MaxColorAttachments = 32;

type
  TGLRenderbuffer = class
  private
    FRenderbufferHandle: TGLRenderbufferHandle;
    FWidth: Integer;
    FHeight: Integer;
    FStorageValid: Boolean;
    function GetHandle: TGLuint;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected

    function GetInternalFormat: cardinal; virtual; abstract;

    procedure InvalidateStorage;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Bind;
    procedure Unbind;
    {: Handle to the OpenGL render buffer object.<p>
      If the handle hasn't already been allocated, it will be allocated
      by this call (ie. do not use if no OpenGL context is active!) }
    property Handle: TGLuint read GetHandle;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

  TGLDepthRBO = class(TGLRenderbuffer)
  private
    FDepthPrecision: TGLDepthPrecision;
    procedure SetDepthPrecision(const Value: TGLDepthPrecision);
  protected
    function GetInternalFormat: cardinal; override;
  public
    constructor Create;

    property DepthPrecision: TGLDepthPrecision read FDepthPrecision write
      SetDepthPrecision;
  end;

  TGLStencilPrecision = (spDefault, sp1bit, sp4bits, sp8bits, sp16bits);

  TGLStencilRBO = class(TGLRenderbuffer)
  private
    FStencilPrecision: TGLStencilPrecision;
    procedure SetStencilPrecision(const Value: TGLStencilPrecision);
  protected
    function GetInternalFormat: cardinal; override;
  public
    constructor Create;

    property StencilPrecision: TGLStencilPrecision read FStencilPrecision write
      SetStencilPrecision;
  end;

  TGLFramebufferStatus = (fsComplete, fsIncompleteAttachment,
    fsIncompleteMissingAttachment,
    fsIncompleteDuplicateAttachment, fsIncompleteDimensions,
    fsIncompleteFormats,
    fsIncompleteDrawBuffer, fsIncompleteReadBuffer, fsUnsupported,
    fsStatusError);

  TGLFrameBuffer = class
  private
    FFrameBufferHandle: TGLFramebufferHandle;
    FBinded: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FLayer: Integer;
    FLevel: Integer;
    FTextureMipmap: cardinal;
    FAttachedTexture: array[0..MaxColorAttachments - 1] of TGLTexture;
    FDepthTexture: TGLTexture;

    function GetHandle: GLuint;
    function GetStatus: TGLFramebufferStatus;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetLayer(const Value: Integer);
    procedure SetLevel(const Value: Integer);
  protected
    procedure AttachTexture(const target: TGLenum;
      const attachment: TGLenum;
      const textarget: TGLenum;
      const texture: TGLuint;
      const level: TGLint;
      const layer: TGLint); overload;
    procedure ReattachTextures;
  public
    constructor Create;
    destructor Destroy; override;

    // attaches a depth rbo to the fbo
    // the depth buffer must have the same dimentions as the fbo
    procedure AttachDepthBuffer(DepthBuffer: TGLDepthRBO); overload;
    // detaches depth attachment from the fbo
    procedure DetachDepthBuffer;

    // attaches a stencil rbo to the fbo
    // the stencil buffer must have the same dimentions as the fbo
    procedure AttachStencilBuffer(StencilBuffer: TGLStencilRBO); overload;
    // detaches stencil attachment from the fbo
    procedure DetachStencilBuffer;

    // attaches a depth texture to the fbo
    // the depth texture must have the same dimentions as the fbo
    procedure AttachDepthTexture(Texture: TGLTexture); overload;
    procedure DetachDepthTexture;

    procedure AttachTexture(n: Integer; Texture: TGLTexture); overload;
    procedure DetachTexture(n: Integer);

    procedure Bind;
    procedure Unbind;

    procedure PreRender;
    procedure Render(var rci: TRenderContextInfo; baseObject:
      TGLBaseSceneObject);
    procedure PostRender(const PostGenerateMipmap: Boolean);

    property Handle: GLuint read GetHandle;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Layer: Integer read FLayer write SetLayer;
    property Level: Integer read FLevel write SetLevel;

    property Status: TGLFramebufferStatus read GetStatus;
  end;

implementation

uses
  GLUtils, GLGraphics, GLTextureFormat;

{ TGLRenderbuffer }

constructor TGLRenderbuffer.Create;
begin
  inherited Create;
  FRenderbufferHandle := TGLRenderbufferHandle.Create;
  FWidth := 256;
  FHeight := 256;
end;

destructor TGLRenderbuffer.Destroy;
begin
  FRenderbufferHandle.DestroyHandle;
  FRenderbufferHandle.Free;
  inherited Destroy;
end;

function TGLRenderbuffer.GetHandle: GLuint;
begin
  if FRenderbufferHandle.Handle = 0 then
    FRenderbufferHandle.AllocateHandle;
  Result := FRenderbufferHandle.Handle;
end;

procedure TGLRenderbuffer.InvalidateStorage;
begin
  FStorageValid := False;
end;

procedure TGLRenderbuffer.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    InvalidateStorage;
  end;
end;

procedure TGLRenderbuffer.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    InvalidateStorage;
  end;
end;

procedure TGLRenderbuffer.Bind;
var
  internalFormat: cardinal;
begin
  glBindRenderbuffer(GL_RENDERBUFFER, Handle);
  if not FStorageValid then
  begin
    internalFormat := GetInternalFormat;
    FRenderbufferHandle.SetStorage(internalFormat, FWidth, FHeight);
  end;
end;

procedure TGLRenderbuffer.Unbind;
begin
  glBindRenderbuffer(GL_RENDERBUFFER, 0);
end;

{ TGLDepthRBO }

constructor TGLDepthRBO.Create;
begin
  inherited Create;
  FDepthPrecision := dpDefault;
end;

function TGLDepthRBO.GetInternalFormat: cardinal;
begin
  case DepthPrecision of
    dp24bits: Result := GL_DEPTH_COMPONENT24;
    dp16bits: Result := GL_DEPTH_COMPONENT16;
    dp32bits: Result := GL_DEPTH_COMPONENT32;
  else
    // dpDefault
    Result := GL_DEPTH_COMPONENT24_ARB;
  end;
end;

procedure TGLDepthRBO.SetDepthPrecision(const Value: TGLDepthPrecision);
begin
  if FDepthPrecision <> Value then
  begin
    FDepthPrecision := Value;
    InvalidateStorage;
  end;
end;

{ TGLStencilRBO }

constructor TGLStencilRBO.Create;
begin
  inherited Create;
  FStencilPrecision := spDefault;
end;

function TGLStencilRBO.GetInternalFormat: cardinal;
begin
  case StencilPrecision of
    spDefault: Result := GL_STENCIL_INDEX;
    sp1bit: Result := GL_STENCIL_INDEX1_EXT;
    sp4bits: Result := GL_STENCIL_INDEX4_EXT;
    sp8bits: Result := GL_STENCIL_INDEX8_EXT;
    sp16bits: Result := GL_STENCIL_INDEX16_EXT;
  else
    // spDefault
    Result := GL_STENCIL_INDEX;
  end;
end;

procedure TGLStencilRBO.SetStencilPrecision(const Value: TGLStencilPrecision);
begin
  if FStencilPrecision <> Value then
  begin
    FStencilPrecision := Value;
    InvalidateStorage;
  end;
end;

{ TGLFrameBuffer }

constructor TGLFrameBuffer.Create;
begin
  inherited;
  FFrameBufferHandle := TGLFrameBufferHandle.Create;
  FWidth := 256;
  FHeight := 256;
  FLayer := 0;
  FLevel := 0;
  FTextureMipmap := 0;
  FBinded := false;
end;

destructor TGLFrameBuffer.Destroy;
begin
  FFrameBufferHandle.DestroyHandle;
  FFrameBufferHandle.Free;
  inherited Destroy;
end;

procedure TGLFrameBuffer.AttachTexture(n: Integer; Texture: TGLTexture);
var
  textarget: GLenum;
begin
  Assert(n < MaxColorAttachments);
  Texture.Handle;
  FAttachedTexture[n] := Texture;
  textarget := Texture.Image.NativeTextureTarget;
  // Store mipmaping requires
  if not ((Texture.MinFilter in [miNearest, miLinear])
    or (textarget = GL_TEXTURE_RECTANGLE)) then
    FTextureMipmap := FTextureMipmap or (1 shl n);

  AttachTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0_EXT + n,
    textarget, Texture.Handle,
    FLevel, FLayer);
end;

procedure TGLFrameBuffer.AttachDepthBuffer(DepthBuffer: TGLDepthRBO);

  procedure AttachDepthRB;
  begin
    // forces initialization
    DepthBuffer.Bind;
    DepthBuffer.Unbind;
    glFramebufferRenderbufferEXT(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT_EXT,
      GL_RENDERBUFFER_EXT, DepthBuffer.Handle);
    CheckOpenGLError;
  end;

var
  dp: TGLDepthPrecision;
begin
  Bind;
  AttachDepthRB;

  // if default format didn't work, try something else
  // crude, but might work
  if (Status = fsUnsupported) and (DepthBuffer.DepthPrecision = dpDefault) then
  begin
    // try the other formats
    // best quality first
    for dp := high(dp) downto low(dp) do
    begin
      if dp = dpDefault then
        Continue;

      DepthBuffer.DepthPrecision := dp;

      AttachDepthRB;

      if not (Status = fsUnsupported) then
        Break;
    end;
  end;
  Status;
  Unbind;
end;

procedure TGLFrameBuffer.AttachDepthTexture(Texture: TGLTexture);
begin
  FDepthTexture := Texture;
  // Force texture properties to depth compatibility
  if not IsDepthFormat(FDepthTexture.TextureFormatEx) then
  begin
    FDepthTexture.ImageClassName := 'TGLBlankImage';
    FDepthTexture.TextureFormatEx := tfDEPTH_COMPONENT24;
    TGLBlankImage(FDepthTexture.Image).Width := Width;
    TGLBlankImage(FDepthTexture.Image).Height := Height;
  end;
  TGLBlankImage(FDepthTexture.Image).ColorFormat := GL_DEPTH_COMPONENT;
  // Depth texture mipmaping
  if not ((FDepthTexture.MinFilter in [miNearest, miLinear])) then
    FTextureMipmap := FTextureMipmap or (1 shl MaxColorAttachments);

  AttachTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
    FDepthTexture.Image.NativeTextureTarget, FDepthTexture.Handle,
    FLevel, FLayer);
end;

procedure TGLFrameBuffer.DetachDepthTexture;
begin
  if Assigned(FDepthTexture) then
  begin
    FTextureMipmap := FTextureMipmap and (not (1 shl MaxColorAttachments));
    AttachTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
      FDepthTexture.Image.NativeTextureTarget, 0, 0, 0);
    FDepthTexture := nil;
  end;
end;

procedure TGLFrameBuffer.AttachStencilBuffer(StencilBuffer: TGLStencilRBO);
begin
  Bind;
  // forces initialization
  StencilBuffer.Bind;
  StencilBuffer.Unbind;
  glFramebufferRenderbufferEXT(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT,
    GL_RENDERBUFFER, StencilBuffer.Handle);
  CheckOpenGLError;
  Status;
  Unbind;
end;

procedure TGLFrameBuffer.AttachTexture(const target: TGLenum;
  const attachment: TGLenum;
  const textarget: TGLenum;
  const texture: TGLuint;
  const level: TGLint;
  const layer: TGLint);
var
  NeedBind: Boolean;
begin
  NeedBind := not FBinded;
  if NeedBind then
    Bind;

  with FFrameBufferHandle do
    case textarget of
      GL_TEXTURE_1D: Attach1DTexture(target, attachment, textarget, texture,
          level);
      GL_TEXTURE_2D: Attach2DTexture(target, attachment, textarget, texture,
          level);
      GL_TEXTURE_RECTANGLE: // Rectangle texture can't be leveled
        Attach2DTexture(target, attachment, textarget, texture, 0);
      GL_TEXTURE_3D: Attach3DTexture(target, attachment, textarget, texture,
          level, layer);

      GL_TEXTURE_CUBE_MAP: Attach2DTexture(target, attachment,
          GL_TEXTURE_CUBE_MAP_POSITIVE_X + layer, texture, level);

      GL_TEXTURE_CUBE_MAP_POSITIVE_X,
        GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
        GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
        GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
        GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
        GL_TEXTURE_CUBE_MAP_NEGATIVE_Z: Attach2DTexture(target, attachment,
          textarget, texture, level);

      GL_TEXTURE_CUBE_MAP_ARRAY,
        GL_TEXTURE_1D_ARRAY,
        GL_TEXTURE_2D_ARRAY: AttachLayer(target, attachment, texture, level,
          layer);

    end;

  CheckOpenGLError;
  if NeedBind then
    Unbind;
end;

procedure TGLFrameBuffer.Bind;
begin
  FBinded := true;
  glBindFramebufferEXT(GL_FRAMEBUFFER, Handle);
end;

procedure TGLFrameBuffer.Unbind;
begin
  FBinded := false;
  glBindFramebufferEXT(GL_FRAMEBUFFER, 0);
end;

procedure TGLFrameBuffer.DetachTexture(n: Integer);
begin
  // textarget ignored when binding 0
  if Assigned(FAttachedTexture[n]) then
  begin
    Bind;
    AttachTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + n,
      GL_TEXTURE_2D, // target does not matter
      0, 0, 0);

    FTextureMipmap := FTextureMipmap and (not (1 shl n));
    FAttachedTexture[n] := nil;
    Unbind;
    CheckOpenGLError;
  end;
end;

procedure TGLFrameBuffer.DetachDepthBuffer;
begin
  Bind;
  glFramebufferRenderbufferEXT(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
    GL_RENDERBUFFER, 0);
  Unbind;
end;

procedure TGLFrameBuffer.DetachStencilBuffer;
begin
  Bind;
  glFramebufferRenderbufferEXT(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT,
    GL_RENDERBUFFER, 0);
  Unbind;
end;

function TGLFrameBuffer.GetStatus: TGLFramebufferStatus;
var
  status: cardinal;
begin
  status := glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);

  case status of
    GL_FRAMEBUFFER_COMPLETE_EXT: Result := fsComplete;
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT: Result := fsIncompleteAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT: Result :=
      fsIncompleteMissingAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT: Result :=
      fsIncompleteDuplicateAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT: Result := fsIncompleteDimensions;
    GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT: Result := fsIncompleteFormats;
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT: Result := fsIncompleteDrawBuffer;
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT: Result := fsIncompleteReadBuffer;
    GL_FRAMEBUFFER_UNSUPPORTED_EXT: Result := fsUnsupported;
  else
    Result := fsStatusError;
  end;
end;

procedure TGLFrameBuffer.PostRender(const PostGenerateMipmap: Boolean);
var
  n: Integer;
  textarget: TGLEnum;
begin
  if (FTextureMipmap > 0) and PostGenerateMipmap then
  begin
    glPushAttrib(GL_TEXTURE_BIT);
    for n := 0 to MaxColorAttachments - 1 do
      if Assigned(FAttachedTexture[n]) then
      begin
        if FTextureMipmap and (1 shl n) = 0 then
          Continue;
        textarget := FAttachedTexture[n].Image.NativeTextureTarget;
        glBindTexture(textarget, FAttachedTexture[n].Handle);
        glGenerateMipmapEXT(textarget);
      end;
    glPopAttrib;
  end;
end;

procedure TGLFrameBuffer.PreRender;
begin

end;

procedure TGLFrameBuffer.Render(var rci: TRenderContextInfo; baseObject:
  TGLBaseSceneObject);
var
  backColor: TColorVector;
  buffer: TGLSceneBuffer;
begin
  Bind;
  Assert(Status = fsComplete, 'Framebuffer not complete');

  buffer := TGLSceneBuffer(rci.buffer);

  backColor := ConvertWinColor(buffer.BackgroundColor);
  glClearColor(backColor[0], backColor[1], backColor[2],
    buffer.BackgroundAlpha);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  baseObject.Render(rci);

  CheckOpenGLError;

  Unbind;
end;

function TGLFrameBuffer.GetHandle: GLuint;
begin
  if FFrameBufferHandle.Handle = 0 then
    FFrameBufferHandle.AllocateHandle;
  Result := FFrameBufferHandle.Handle;
end;

procedure TGLFrameBuffer.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
  end;
end;

procedure TGLFrameBuffer.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
  end;
end;

procedure TGLFrameBuffer.ReattachTextures;
var
  n: Integer;
begin
  // Reattach layered textures
  for n := 0 to MaxColorAttachments-1 do
    if Assigned(FAttachedTexture[n]) then
      AttachTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0_EXT + n,
        FAttachedTexture[n].Image.NativeTextureTarget,
        FAttachedTexture[n].Handle,
        FLevel, FLayer);
  if Assigned(FDepthTexture) then
    AttachTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
      FDepthTexture.Image.NativeTextureTarget, FDepthTexture.Handle,
      FLevel, FLayer);
  Assert(Status = fsComplete, 'Framebuffer not complete');
  CheckOpenGLError;
end;

procedure TGLFrameBuffer.SetLayer(const Value: Integer);
begin
  if FLayer <> Value then
  begin
    FLayer := Value;
    if not FBinded then
      Exit;
    ReattachTextures;
  end;
end;

procedure TGLFrameBuffer.SetLevel(const Value: Integer);
begin
  if FLevel <> Value then
  begin
    FLevel := Value;
    if not FBinded then
      Exit;
    ReattachTextures;
  end;
end;

end.

