//
// This unit is part of the GLScene Project, http://glscene.org
//
{
    This unit provides support for two new types of "multisample
    textures" - two-dimensional and two-dimensional array - as well as
    mechanisms to fetch a specific sample from such a texture in a shader,
    and to attach such textures to FBOs for rendering.

    History :  
       04/11/10- DaStr - Added $I GLScene.inc   
       23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
       16/05/10 - Yar - Creation (thanks to C4)
    
}
unit GLMultisampleImage;

interface

{$I GLScene.inc}

uses
  Classes,
  OpenGLTokens,
  GLContext,
  GLTexture,
  GLGraphics,
  GLTextureFormat;

type

  // TGLMultisampleImage
  //
  TGLMultisampleImage = class(TGLTextureImage)
  private
     
    FBitmap: TGLBitmap32;
    FSamplesCount: Integer;
    FWidth, FHeight, FDepth: Integer;
    FFixedSamplesLocation: Boolean;
    procedure SetWidth(val: Integer);
    procedure SetHeight(val: Integer);
    procedure SetDepth(val: Integer);
    procedure SetSamplesCount(val: Integer);
    procedure SetFixedSamplesLocation(val: Boolean);
  protected
     
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
    function GetTextureTarget: TGLTextureTarget; override;
  public
     
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    class function IsSelfLoading: Boolean; override;
    procedure LoadTexture(AInternalFormat: TGLInternalFormat); override;
    function GetBitmap32: TGLBitmap32; override;
    procedure ReleaseBitmap32; override;

    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;

    property NativeTextureTarget;

  published
     
    { Width of the blank image (for memory allocation). }
    property Width: Integer read GetWidth write SetWidth default 256;
    { Width of the blank image (for memory allocation). }
    property Height: Integer read GetHeight write SetHeight default 256;
    property Depth: Integer read GetDepth write SetDepth default 0;
    property SamplesCount: Integer read FSamplesCount write SetSamplesCount
      default 0;
    property FixedSamplesLocation: Boolean read FFixedSamplesLocation write
      SetFixedSamplesLocation;
  end;

implementation

// ------------------
// ------------------ TGLMultisampleImage ------------------
// ------------------

{$IFDEF GLS_REGIONS}{$REGION 'TGLMultisampleImage'}{$ENDIF}

// Create
//

constructor TGLMultisampleImage.Create(AOwner: TPersistent);
begin
  inherited;
  FWidth := 256;
  FHeight := 256;
  FDepth := 0;
  FSamplesCount := 0;
end;

// Destroy
//

destructor TGLMultisampleImage.Destroy;
begin
  ReleaseBitmap32;
  inherited Destroy;
end;

 
//

procedure TGLMultisampleImage.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if (Source is TGLMultisampleImage) then
    begin
      FWidth := TGLMultisampleImage(Source).FWidth;
      FHeight := TGLMultisampleImage(Source).FHeight;
      FDepth := TGLMultisampleImage(Source).FDepth;
      FSamplesCount := TGLMultisampleImage(Source).FSamplesCount;
      Invalidate;
    end
    else
      inherited;
  end
  else
    inherited;
end;

// SetWidth
//

procedure TGLMultisampleImage.SetWidth(val: Integer);
begin
  if val <> FWidth then
  begin
    FWidth := val;
    if FWidth < 1 then
      FWidth := 1;
    Invalidate;
  end;
end;

// GetWidth
//

function TGLMultisampleImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

// SetHeight
//

procedure TGLMultisampleImage.SetHeight(val: Integer);
begin
  if val <> FHeight then
  begin
    FHeight := val;
    if FHeight < 1 then
      FHeight := 1;
    Invalidate;
  end;
end;

// GetHeight
//

function TGLMultisampleImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

// GetDepth
//

function TGLMultisampleImage.GetDepth: Integer;
begin
  Result := FDepth;
end;

// SetHeight
//

procedure TGLMultisampleImage.SetDepth(val: Integer);
begin
  if val <> FDepth then
  begin
    FDepth := val;
    if FDepth < 0 then
      FDepth := 0;
    Invalidate;
  end;
end;

// SetSamplesCount
//

procedure TGLMultisampleImage.SetSamplesCount(val: Integer);
begin
  if val < 0 then
    val := 0;

  if val <> FSamplesCount then
  begin
    FSamplesCount := val;
    Invalidate;
  end;
end;

// SetFixedSamplesLocation
//

procedure TGLMultisampleImage.SetFixedSamplesLocation(val: Boolean);
begin
  if val <> FFixedSamplesLocation then
  begin
    FFixedSamplesLocation := val;
    Invalidate;
  end;
end;

// GetBitmap32
//

function TGLMultisampleImage.GetBitmap32: TGLBitmap32;
begin
  if not Assigned(FBitmap) then
  begin
    FBitmap := TGLBitmap32.Create;
    FBitmap.Blank := true;
    FBitmap.Width := FWidth;
    FBitmap.Height := FHeight;
  end;
  Result := FBitmap;
end;

// ReleaseBitmap32
//

procedure TGLMultisampleImage.ReleaseBitmap32;
begin
  FBitmap.Free;
  FBitmap := nil;
end;

// SaveToFile
//

procedure TGLMultisampleImage.SaveToFile(const fileName: string);
begin
end;

 
//

procedure TGLMultisampleImage.LoadFromFile(const fileName: string);
begin
end;

 
//

class function TGLMultisampleImage.FriendlyName: string;
begin
  Result := 'Multisample Image';
end;

// FriendlyDescription
//

class function TGLMultisampleImage.FriendlyDescription: string;
begin
  Result := 'Image for rendering to texture with antialiasing';
end;

// GetTextureTarget
//

function TGLMultisampleImage.GetTextureTarget: TGLTextureTarget;
begin
  if fDepth > 0 then
    Result := ttTexture2DMultisampleArray
  else
    Result := ttTexture2DMultisample;
end;

class function TGLMultisampleImage.IsSelfLoading: Boolean;
begin
  Result := True;
end;

procedure TGLMultisampleImage.LoadTexture(AInternalFormat: TGLInternalFormat);
var
  target: TGLTextureTarget;
  maxSamples, maxSize: TGLint;
begin
  // Check smaples count range
  GL.GetIntegerv(GL_MAX_SAMPLES, @maxSamples);
  if FSamplesCount > maxSamples then
    FSamplesCount := maxSamples;
  if IsDepthFormat(AInternalFormat) then
  begin
    GL.GetIntegerv(GL_MAX_DEPTH_TEXTURE_SAMPLES, @maxSamples);
    if FSamplesCount > maxSamples then
      FSamplesCount := maxSamples;
  end
  else
  begin
    GL.GetIntegerv(GL_MAX_COLOR_TEXTURE_SAMPLES, @maxSamples);
    if FSamplesCount > maxSamples then
      FSamplesCount := maxSamples;
  end;
  // Check texture size
  GL.GetIntegerv(GL_MAX_TEXTURE_SIZE, @maxSize);
  if FWidth > maxSize then
    FWidth := maxSize;
  if FHeight > maxSize then
    FHeight := maxSize;

  target := NativeTextureTarget;
  case target of

    ttTexture2DMultisample:
      GL.TexImage2DMultisample(
        DecodeGLTextureTarget(target),
        SamplesCount,
        InternalFormatToOpenGLFormat(AInternalFormat),
        Width,
        Height,
        FFixedSamplesLocation);

    ttTexture2DMultisampleArray:
      GL.TexImage3DMultisample(
        DecodeGLTextureTarget(target),
        SamplesCount,
        InternalFormatToOpenGLFormat(AInternalFormat),
        Width,
        Height,
        Depth,
        FFixedSamplesLocation);
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

initialization
  RegisterGLTextureImageClass(TGLMultisampleImage);

end.