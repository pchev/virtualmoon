//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  Adds a dynamic texture image, which allows for easy updating of
  texture data.

	 History :  
       23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
       20/02/10 - DanB - Fix for TGLDynamicTextureImage.GetTexSize
       23/01/10 - Yar - Replaced TextureFormat to TextureFormatEx
                           simplify GetBitsPerPixel and GetDataFormat
       22/01/10 - Yar - Added GLTextureFormat to uses 
       08/10/08 - DanB - added FriendlyName/FriendlyDescription
       16/10/07 - LC - Added DirtyRectangle to allow partial updates.
       12/07/07 - DaStr - Added $I GLScene.inc
       25/06/07 - LC - Added SysUtils (needed for AllocMem on D7 and down).
       25/06/07 - LC - Fixed a bug where resizing a texture would fail. Introduced
                          new methods for freeing PBO and buffer.
       24/06/07 - LC - Creation
    
}

unit GLDynamicTexture;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils, OpenGLTOkens, GLContext, GLTexture, GLTextureFormat,
  GLGraphics, GLCrossPlatform;

type
  // TGLDynamicTextureImage
  //
  { Allows for fast updating of the texture at runtime. }
  TGLDynamicTextureImage = class(TGLBlankImage)
  private
    FUpdating: integer;
    FTexSize: integer;
    FBuffer: pointer;
    FPBO: TGLBufferObjectHandle;
    FData: pointer;
    FDirtyRect: TGLRect;
    FUseBGR: boolean;
    FUsePBO: boolean;
    procedure SetDirtyRectangle(const Value: TGLRect);
    procedure SetUsePBO(const Value: boolean);
  protected
    function GetTexSize: integer;
    function GetBitsPerPixel: integer;
    function GetDataFormat: integer;
    function GetTextureFormat: integer;

    procedure FreePBO;
    procedure FreeBuffer;

    property BitsPerPixel: integer read GetBitsPerPixel;
    property DataFormat: integer read GetDataFormat;
    property TextureFormat: integer read GetTextureFormat;
  public
    constructor Create(AOwner: TPersistent); override;

    class function FriendlyName : String; override;
    class function FriendlyDescription : String; override;

    procedure NotifyChange(Sender: TObject); override;

    { Must be called before using the Data pointer.
      Rendering context must be active! }
    procedure BeginUpdate;

    { Must be called after data is changed.
       This will upload the new data. }
    procedure EndUpdate;

    { Pointer to buffer data. Will be nil
       outside a BeginUpdate / EndUpdate block. }
    property Data: pointer read FData;

    { Marks the dirty rectangle inside the texture. BeginUpdate sets
       it to ((0, 0), (Width, Height)), ie the entire texture.
       Override it if you're only changing a small piece of the texture.
       Note that the Data pointer is relative to the DirtyRectangle,
       NOT the entire texture. }
    property DirtyRectangle: TGLRect read FDirtyRect write SetDirtyRectangle;

    { Indicates that the data is stored as BGR(A) instead of
       RGB(A). The default is to use BGR(A). }
    property UseBGR: boolean read FUseBGR write FUseBGR;

    { Enables or disables use of a PBO. Default is true. }
    property UsePBO: boolean read FUsePBO write SetUsePBO;
  end;

implementation

uses
  GLVectorGeometry;

{ TGLDynamicTextureImage }

procedure TGLDynamicTextureImage.BeginUpdate;
var
  LTarget: TGLTextureTarget;
begin
  Assert(FUpdating >= 0, 'Unbalanced begin/end update');

  FUpdating:= FUpdating + 1;

  if FUpdating > 1 then
    exit;

  // initialization
  if not (assigned(FPBO) or assigned(FBuffer)) then
  begin
    // cache so we know if it's changed
    FTexSize:= GetTexSize;
    
    if FUsePBO and TGLUnpackPBOHandle.IsSupported then
    begin
      FPBO:= TGLUnpackPBOHandle.CreateAndAllocate;
      // initialize buffer
      FPBO.BindBufferData(nil, FTexSize, GL_STREAM_DRAW_ARB);
      // unbind so we don't upload the data from it, which is unnecessary
      FPBO.UnBind;
    end
    else
    begin
      // fall back to regular memory buffer if PBO's aren't supported
      FBuffer:= AllocMem(FTexSize);
    end;

    // Force creation of texture
    // This is a bit of a hack, should be a better way...
    LTarget := TGLTexture(OwnerTexture).TextureHandle.Target;
    CurrentGLContext.GLStates.TextureBinding[0, LTarget] := TGLTexture(OwnerTexture).Handle;
    case LTarget of
      ttNoShape: ;
      ttTexture1D: ;
      ttTexture2D:
        GL.TexImage2D(GL_TEXTURE_2D, 0, TGLTexture(OwnerTexture).OpenGLTextureFormat, Width, Height, 0, TextureFormat, GL_UNSIGNED_BYTE, nil);
      ttTexture3D: ;
      ttTexture1DArray: ;
      ttTexture2DArray: ;
      ttTextureRect: ;
      ttTextureBuffer: ;
      ttTextureCube: ;
      ttTexture2DMultisample: ;
      ttTexture2DMultisampleArray: ;
      ttTextureCubeArray: ;
    end;
  end;

  GL.CheckError;
  
  if assigned(FPBO) then
  begin
    FPBO.Bind;

    FData:= FPBO.MapBuffer(GL_WRITE_ONLY_ARB);
  end
  else
  begin
    FData:= FBuffer;
  end;

  GL.CheckError;

  FDirtyRect:= GLRect(0, 0, Width, Height);
end;

constructor TGLDynamicTextureImage.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);

  FUseBGR:= true;
  FUsePBO:= true;
end;

procedure TGLDynamicTextureImage.EndUpdate;
var
  d: pointer;
  LTarget: TGLTextureTarget;
begin
  Assert(FUpdating > 0, 'Unbalanced begin/end update');

  FUpdating:= FUpdating - 1;

  if FUpdating > 0 then
    exit;

  if assigned(FPBO) then
  begin
    FPBO.UnmapBuffer;
    // pointer will act as an offset when using PBO
    d:= nil;
  end
  else
  begin
    d:= FBuffer;
  end;

  LTarget := TGLTexture(OwnerTexture).TextureHandle.Target;
  CurrentGLContext.GLStates.TextureBinding[0, LTarget] := TGLTexture(OwnerTexture).Handle;

  case LTarget of
    ttNoShape: ;
    ttTexture1D: ;
    ttTexture2D:
      begin
        GL.TexSubImage2D(GL_TEXTURE_2D, 0,
          FDirtyRect.Left, FDirtyRect.Top,
          FDirtyRect.Right-FDirtyRect.Left,
          FDirtyRect.Bottom-FDirtyRect.Top,
          TextureFormat, DataFormat, d);
      end;
    ttTexture3D: ;
    ttTexture1DArray: ;
    ttTexture2DArray: ;
    ttTextureRect: ;
    ttTextureBuffer: ;
    ttTextureCube: ;
    ttTexture2DMultisample: ;
    ttTexture2DMultisampleArray: ;
    ttTextureCubeArray: ;
  end;

  if assigned(FPBO) then
    FPBO.UnBind;

  FData:= nil;

  GL.CheckError;
end;

procedure TGLDynamicTextureImage.FreeBuffer;
begin
  if assigned(FBuffer) then
  begin
    FreeMem(FBuffer);
    FBuffer:= nil;
  end;
end;

procedure TGLDynamicTextureImage.FreePBO;
begin
  if assigned(FPBO) then
  begin
    FPBO.Free;
    FPBO:= nil;
  end;
end;

 
//
class function TGLDynamicTextureImage.FriendlyName : String;
begin
   Result:='Dynamic Texture';
end;

// FriendlyDescription
//
class function TGLDynamicTextureImage.FriendlyDescription : String;
begin
   Result:='Dynamic Texture - optimised for changes at runtime';
end;

function TGLDynamicTextureImage.GetBitsPerPixel: integer;
begin
  Result := 8 * GetTextureElementSize( TGLTexture(OwnerTexture).TextureFormatEx );
end;

function TGLDynamicTextureImage.GetDataFormat: integer;
var
  data, color: TGLEnum;
begin
  FindCompatibleDataFormat(TGLTexture(OwnerTexture).TextureFormatEx, color, data);
  Result := data;
end;

function TGLDynamicTextureImage.GetTexSize: integer;
begin
  result:= Width * Height * BitsPerPixel div 8;
end;

function TGLDynamicTextureImage.GetTextureFormat: integer;
var
  data, color: TGLEnum;
begin
  FindCompatibleDataFormat(TGLTexture(OwnerTexture).TextureFormatEx, color, data);
  if FUseBGR then
    case color of
      GL_RGB: color := GL_BGR;
      GL_RGBA: color := GL_BGRA;
    end;
  Result := color;
end;

procedure TGLDynamicTextureImage.NotifyChange(Sender: TObject);
begin
  if FTexSize <> GetTexSize then
  begin
    FreePBO;
    FreeBuffer;
  end;

  inherited;
end;

procedure TGLDynamicTextureImage.SetDirtyRectangle(const Value: TGLRect);
begin
  FDirtyRect.Left:= MaxInteger(Value.Left, 0);
  FDirtyRect.Top:= MaxInteger(Value.Top, 0);
  FDirtyRect.Right:= MinInteger(Value.Right, Width);
  FDirtyRect.Bottom:= MinInteger(Value.Bottom, Height);
end;

procedure TGLDynamicTextureImage.SetUsePBO(const Value: boolean);
begin
  Assert(FUpdating = 0, 'Cannot change PBO settings while updating');
  if FUsePBO <> Value then
  begin
    FUsePBO := Value;
    if not FUsePBO then
      FreePBO
    else
      FreeBuffer;
  end;
end;

initialization
  RegisterGLTextureImageClass(TGLDynamicTextureImage);

end.
