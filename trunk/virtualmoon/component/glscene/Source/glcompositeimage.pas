//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLCompositeImage<p>

    This class is required for loading images such classes as TGLDDSImage,
    TGLO3TCImage, TGLHDRImage etc.

 <b>History : </b><font size=-1><ul>
        <li>22/02/10 - Yar - Added LoadFromStream (thanks to mif)
        <li>23/01/10 - Yar - Replaced TextureFormat to TextureFormatEx
        <li>21/01/10 - Yar - Creation
   </ul><p>
}

unit GLCompositeImage;

interface

uses
  Classes, OpenGL1x, GLGraphics, GLTexture;

type

  // TGLCompositeImage
  //

  TGLCompositeImage = class(TGLTextureImage)
  private
    FBitmap: TGLBitmap32;
    FWidth, FHeight, FDepth: integer;
  protected
    procedure SetWidth(val: Integer);
    procedure SetHeight(val: Integer);
    procedure SetDepth(val: Integer);
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
    function GetTextureTarget: GLenum; override;
  public

    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function GetBitmap32(target: TGLUInt = GL_TEXTURE_2D): TGLBitmap32;
      override;
    procedure ReleaseBitmap32; override;

    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    procedure LoadFromStream(const AStream: TStream);
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property NativeTextureTarget;

  published
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Depth: Integer read GetDepth write SetDepth;
  end;

implementation

uses
  ApplicationFileIO, GLTextureFormat;

// ------------------
// ------------------ TGLCompositeImage ------------------
// ------------------

// Create
//

constructor TGLCompositeImage.Create(AOwner: TPersistent);
begin
  inherited;
  FWidth := 256;
  FHeight := 256;
  FDepth := 0;
end;

// Destroy
//

destructor TGLCompositeImage.Destroy;
begin
  ReleaseBitmap32;
  inherited Destroy;
end;

// Assign
//

procedure TGLCompositeImage.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if not Assigned(FBitmap) then
      FBitmap := TGLBitmap32.Create;
    if (Source is TGLCompositeImage) then
    begin
      FBitmap.Assign(TGLCompositeImage(Source).FBitmap);
    end
    else
      FBitmap.Assign(Source);

    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    FDepth := FBitmap.Depth;
    FResourceFile := FBitmap.ResourceName;
    // Composite image always rewrite texture format
    if Assigned(FOwnerTexture) then
      FOwnerTexture.TextureFormatEx := FBitmap.InternalFormat;
    NotifyChange(Self);
  end
  else
    inherited;
end;

// SetWidth
//

procedure TGLCompositeImage.SetWidth(val: Integer);
begin
  if val <> FWidth then
  begin
    if val < 1 then
      val := 1;
    FWidth := val;
    Invalidate;
  end;
end;

// GetWidth
//

function TGLCompositeImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

// SetHeight
//

procedure TGLCompositeImage.SetHeight(val: Integer);
begin
  if val <> FHeight then
  begin
    if val < 1 then
      val := 1;
    FHeight := val;
    Invalidate;
  end;
end;

// GetHeight
//

function TGLCompositeImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

// SetDepth
//

procedure TGLCompositeImage.SetDepth(val: Integer);
begin
  if val <> FDepth then
  begin
    if val < 0 then
      val := 0;
    FDepth := val;
    Invalidate;
  end;
end;

// GetDepth
//

function TGLCompositeImage.GetDepth: Integer;
begin
  Result := FDepth;
end;

// GetBitmap32
//

function TGLCompositeImage.GetBitmap32(target: TGLUInt): TGLBitmap32;
begin
  if not Assigned(FBitmap) then
  begin
    FBitmap := TGLBitmap32.Create;
    FBitmap.Blank := true;
    FWidth := 256;
    FHeight := 256;
    FDepth := 0;
    FBitmap.Width := FWidth;
    FBitmap.Height := FHeight;
    FBitmap.Depth := FDepth;
  end;
  Result := FBitmap;
end;

// ReleaseBitmap32
//

procedure TGLCompositeImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

// SaveToFile
//

procedure TGLCompositeImage.SaveToFile(const fileName: string);
var
  BaseImageClass: TGLBaseImageClass;
  tempImage: TGLBaseImage;
begin
  if (filename = '') and Assigned(FBitmap) then
    EXIT;
  BaseImageClass := GetRasterFileFormats.FindFromFileName(filename);
  tempImage := BaseImageClass.Create;
  if Assigned(FOwnerTexture) then
  begin
    if FOwnerTexture.IsHandleAllocated then
      tempImage.AssignFromTexture(FOwnerTexture.RenderingContext,
        FOwnerTexture.Handle,
        NativeTextureTarget,
        false,
        FOwnerTexture.TextureFormatEx)
    else
      tempImage.Assign(fBitmap);
  end
  else
    tempImage.Assign(fBitmap);
  try
    tempImage.SaveToFile(fileName);
    FResourceFile := fileName;
  finally
    tempImage.Free;
  end;
end;

// LoadFromFile
//

procedure TGLCompositeImage.LoadFromFile(const fileName: string);
var
  BaseImageClass: TGLBaseImageClass;
  tempImage: TGLBaseImage;
begin
  if filename = '' then
    exit;
  BaseImageClass := GetRasterFileFormats.FindFromFileName(filename);
  tempImage := BaseImageClass.Create;
  try
    tempImage.LoadFromFile(fileName);
    if not Assigned(FBitmap) then
      FBitmap := TGLBitmap32.Create;
    FBitmap.Assign(tempImage);
    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    FDepth := FBitmap.Depth;
    FResourceFile := FBitmap.ResourceName;
    // Internal image always rewrite texture format
    if Assigned(FOwnerTexture) then
      FOwnerTexture.TextureFormatEx := FBitmap.InternalFormat;
    NotifyChange(Self);
  finally
    tempImage.Free;
  end;
end;

// LoadFromStream
//

procedure TGLCompositeImage.LoadFromStream(const AStream: TStream);
var
  tempImage: TGLBaseImage;
begin
  if (not Assigned(AStream)) or (AStream.Size - AStream.Position < 200) then
    exit;
  with GetRasterFileFormats do
    tempImage := FindFromStream(AStream).Create;
  try
    tempImage.LoadFromStream(AStream);
    if not Assigned(FBitmap) then
      FBitmap := TGLBitmap32.Create;
    FBitmap.Assign(tempImage);
    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    FDepth := FBitmap.Depth;
    FResourceFile := '';
    if Assigned(FOwnerTexture) then
      FOwnerTexture.TextureFormatEx := FBitmap.InternalFormat;
    NotifyChange(Self);
  finally
    tempImage.Free;
  end;
end;

// FriendlyName
//

class function TGLCompositeImage.FriendlyName: string;
begin
  Result := 'Composite Image';
end;

// FriendlyDescription
//

class function TGLCompositeImage.FriendlyDescription: string;
begin
  Result := 'Image contained any internal formats of OpenGL textures';
end;

// GetTextureTarget
//

function TGLCompositeImage.GetTextureTarget: GLenum;
begin
  if Assigned(fBitmap) then
  begin
    Result := GL_TEXTURE_2D;
    // Choose a texture target
    if fBitmap.Height = 1 then
      Result := GL_TEXTURE_1D;
    if fBitmap.CubeMap then
      Result := GL_TEXTURE_CUBE_MAP;
    if fBitmap.IsVolume then
      Result := GL_TEXTURE_3D;
    if fBitmap.TextureArray then
    begin
      if (fBitmap.Depth < 2) then
        Result := GL_TEXTURE_1D_ARRAY
      else
        Result := GL_TEXTURE_2D_ARRAY;
      if fBitmap.CubeMap then
        Result := GL_TEXTURE_CUBE_MAP_ARRAY;
    end;

    if ((fBitmap.InternalFormat >= tfFLOAT_R16)
      and (fBitmap.InternalFormat <= tfFLOAT_RGBA32)) then
      Result := GL_TEXTURE_RECTANGLE;

    if Result = fPreviousTarget then
      Exit;
    fPreviousTarget := Result;
    // update texture target
    if Assigned(FOwnerTexture) then
      FOwnerTexture.NotifyTargetChange;
  end
    // if bitmap is already realised then give a previous target
  else
    Result := fPreviousTarget;
end;

initialization
  RegisterGLTextureImageClass(TGLCompositeImage);

end.

