//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLGraphics<p>

   Utility class and functions to manipulate a bitmap in OpenGL's default
   byte order (GL_RGBA vs TBitmap's GL_BGRA)<p>

   Note: TGLBitmap32 has support for Alex Denissov's Graphics32 library
   (http://www.g32.org), just make sure the GLS_Graphics32_SUPPORT conditionnal
   is active in GLScene.inc and recompile.<p>

 <b>Historique : </b><font size=-1><ul>
      <li>08/03/10 - Yar - Added in TRasterFileFormatsList.FindFromStream PNG singnature
                           Added forward context cheking to circumvent deprecations
      <li>01/03/10 - Yar - Bugfix when texture, which has lower mip-levels than the standard number is not rendered
                           (thanks to Controller)
      <li>23/02/10 - Yar - Solved problem of TGLBitmap with width of which is not a multiple of four.
                           Added in AssignFrom24BitsBitmap, AssignFrom32BitsBitmap using extension GL_EXT_bgra
      <li>22/02/10 - Yar - Added FindFromStream to TRasterFileFormatsList
                           (thanks to mif)
      <li>10/02/10 - Yar   - Bugfix in RegisterAsOpenGLTexture with Cubemap mipmaping
      <li>27/01/10 - Yar   - Bugfix in BlockOffset with negative result
                             Return to GL_SGIS_generate_mipmap
      <li>23/01/10 - Yar   - Added to AssignFromTexture CurrentFormat parameter
      <li>22/01/10 - Yar   - Added TRasterFileFormat, TGLBaseImage classes
                             TGLBitmap32 now derived from TGLBaseImage
                             and can contain all types of image
                             (with mipmap level, texture array, volume texture)
                             any kind of color and data formats of OpenGL
                             remake RegisterAsOpenGLTexture
      <li>10/11/09 - DaStr - Updated TGLBitmap32.RegisterAsOpenGLTexture() - fixed
                               TextureFormat and ColorFormat (thanks YarUnderoaker)
                             Improved FPC compatibility by merging from gls4laz
                              (BugtrackerID = 2893580) (thanks Predator)
      <li>24/03/07 - DaStr - Moved TGLMinFilter and TGLMagFilter from GLUtils.pas
                              to GLGraphics.pas (BugTracker ID = 1923844)
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>14/03/07 - DaStr - Added explicit pointer dereferencing
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
      <li>23/01/07 - LIN- Added TGLBitmap32.AssignToBitmap : Converts a TGLBitmap32 back into a TBitmap
      <li>12/09/06 - NC - Added TGLBitmap32.Blank
      <li>18/10/05 - NC - GL_ARB_texture_non_power_of_two, GL_TEXTURE_2D for float
                          texture
      <li>06/10/04 - NC - Now uses GL_TEXTURE_RECTANGLE_NV for all float texture types
      <li>04/10/04 - NC - Added support for float texture
      <li>05/09/03 - EG - Added TGLBitmap32.DownSampleByFactor2
      <li>04/07/03 - EG - Added RGBA brightness/gamma correction support
      <li>13/05/03 - EG - Added GrayScaleToNormalMap
      <li>26/08/02 - EG - Fixed loading of 1D horiz textures from 24 bits bitmaps
      <li>16/06/02 - EG - AssignFrom32BitsBitmap fix for single-line bitmaps
      <li>29/01/02 - EG - Fixed ScanLine Index bug with empty bitmaps
      <li>20/01/02 - EG - Fixed BGR24/RGB24 last pixel transfer
      <li>17/01/02 - EG - Faster assignments from bitmaps (approx. x2),
                          Added AssignFromBitmap24WithoutRGBSwap
      <li>28/12/01 - EG - Graphics32 support added
      <li>15/12/01 - EG - Texture target support
      <li>14/09/01 - EG - Use of vFileStreamClass
      <li>31/08/01 - EG - 24bits Bitmaps are now made opaque by default
      <li>12/08/01 - EG - Now detects and uses GL_SGIS_generate_mipmap
      <li>20/02/01 - EG - Fixed SetHeight & SetWidth (thx Nelson Chu)
      <li>14/02/01 - EG - Simplified RegisterAsOpenGLTexture
      <li>15/01/01 - EG - Fixed RegisterAsOpenGLTexture (clamping)
      <li>14/01/01 - EG - Fixed isEmpty (was invalid for rectangles)
      <li>08/10/00 - EG - Fixed RegisterAsOpenGLTexture and Assign(nil)
      <li>25/09/00 - EG - First operational code
      <li>19/08/00 - EG - Creation
 </ul></font>
}
unit GLGraphics;

interface

{$I GLScene.inc}

uses Classes, PersistentClasses, Graphics, ApplicationFileIO, SysUtils,
{$IFDEF GLS_Graphics32_SUPPORT}
  GR32,
{$ENDIF}
{$IFDEF FPC}
  fpimage, intfgraphics, GraphType,
{$ENDIF}
  OpenGL1x, GLUtils, GLCrossPlatform, GLContext, GLColor, GLTextureFormat;

type
  TGLMinFilter = (miNearest, miLinear, miNearestMipmapNearest,
    miLinearMipmapNearest, miNearestMipmapLinear,
    miLinearMipmapLinear);

  TGLMagFilter = (maNearest, maLinear);

  // TGLPixel24
  //
  TGLPixel24 = packed record
    r, g, b: Byte;
  end;
  PGLPixel24 = ^TGLPixel24;

  // TGLPixel32
  //
  TGLPixel32 = packed record
    r, g, b, a: Byte;
  end;
  PGLPixel32 = ^TGLPixel32;

  TGLPixel32Array = array[0..MaxInt shr 3] of TGLPixel32;
  PGLPixel32Array = ^TGLPixel32Array;

  // TGLBaseImage
  //
  TGLBaseImage = class(TDataFile)
  protected
    fData: PGLPixel32Array;
    fWidth: Integer;
    fHeight: Integer;
    fDepth: Integer;
    fMipLevels: Integer;
    fColorFormat: GLenum;
    fInternalFormat: TGLInternalFormat;
    fDataType: GLenum;
    fElementSize: Integer;
    fLevels: TList;
    fCubeMap: Boolean;
    fTextureArray: Boolean;

    function GetData: PGLPixel32Array; virtual;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    {: Assigns from any Texture.}
    procedure AssignFromTexture(textureContext: TGLContext;
      const textureHandle: TGLenum;
      textureTarget: TGLenum;
      const CurrentFormat: Boolean;
      const intFormat: TGLInternalFormat); virtual; abstract;

    procedure Assign(Source: TPersistent); override;
    {: Convert vertical cross format of non compressed, non mipmaped image
       to six face of cube map }
    function ConvertCrossToCubeMap: Boolean;
    {: Convert flat image to volume by dividing it into slice. }
    function ConvertToVolume(const col, row: Integer; const MakeArray: Boolean):
      Boolean;
    {: Return mipmap level size in byte }
    function LevelSize(const level: integer): Integer;
    {: Return pointer to mipmap level }
    function GetLevelData(const level: integer; face: integer =
      GL_TEXTURE_CUBE_MAP_POSITIVE_X): PGLPixel32Array;
    {: Return size in byte of all image }
    function DataSize: Integer;
    {: True if the bitmap is empty (ie. width or height is zero). }
    function IsEmpty: Boolean;
    function IsCompressed: Boolean;
    function IsVolume: Boolean;
    {: Narrow image data to simple RGBA8 ubyte }
    procedure Narrow;
    {: Leave top level and remove other }
    procedure UnMipmap; dynamic;
    {: Direct Access to image data}
    property Data: PGLPixel32Array read GetData;
  end;

  TGLBaseImageClass = class of TGLBaseImage;

  // TGLBitmap32
  //
    {: Contains and manipulates a 32 bits (24+8) bitmap.<p>
       This is the base class for preparing and manipulating textures in GLScene,
       this function does not rely on a windows handle and should be used for
       in-memory manipulations only.<br>
       16 bits textures are automatically converted to 24 bits and an opaque (255)
       alpha channel is assumed for all planes, the byte order is as specified
       in GL_RGBA. If 32 bits is used in this class, it can however output 16 bits texture
       data for use in OpenGL.<p>
       The class has support for registering its content as a texture, as well
       as for directly drawing/reading from the current OpenGL buffer. }
  TGLBitmap32 = class(TGLBaseImage)
  private
    { Private Declarations }
    FVerticalReverseOnAssignFromBitmap: Boolean;
    FBlank: boolean;

  protected
    { Protected Declarations }
    procedure SetWidth(val: Integer);
    procedure SetHeight(const val: Integer);
    procedure SetDepth(const val: Integer);
    procedure SetBlank(const Value: boolean);
    procedure SetCubeMap(const val: Boolean);
    procedure SetArray(const val: Boolean);
    procedure SetInternalFormat(const val: TGLInternalFormat);
    procedure SetColorFormat(const val: GLenum);
    procedure SetDataType(const val: GLenum);
    function GetScanLine(index: Integer): PGLPixel32Array;
    procedure AssignFrom24BitsBitmap(aBitmap: TGLBitmap);
    procedure AssignFrom32BitsBitmap(aBitmap: TGLBitmap);
{$IFDEF GLS_Graphics32_SUPPORT}
    procedure AssignFromBitmap32(aBitmap32: TBitmap32);
{$ENDIF}

  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;
    {: Accepts TGLBitmap32 and TGraphic subclasses. }
    procedure Assign(Source: TPersistent); override;
    {: Assigns from a 24 bits bitmap without swapping RGB.<p>
      This is faster than a regular assignment, but R and B channels
      will be reversed (from what you would view in a TImage). Suitable
      if you do your own drawing and reverse RGB on the drawing side.<br>
      If you're after speed, don't forget to set the bitmap's dimensions
      to a power of two! }
    procedure AssignFromBitmap24WithoutRGBSwap(aBitmap: TGLBitmap);
    {: Assigns from a 2D Texture.<p>
      The context which holds the texture must be active and the texture
      handle valid. }
    procedure AssignFromTexture2D(textureHandle: Cardinal); overload;
    {: Assigns from a Texture handle.<p>
      If the handle is invalid, the bitmap32 will be empty. }
    procedure AssignFromTexture2D(textureHandle: TGLTextureHandle); overload;
    {: Assigns from any Texture.}
    procedure AssignFromTexture(textureContext: TGLContext;
      const textureHandle: TGLenum;
      textureTarget: TGLenum;
      const CurrentFormat: Boolean;
      const intFormat: TGLInternalFormat = tfRGBA8;
      const colorFormat: TGLenum = 0;
      const dataType: TGLenum = 0); reintroduce;
    {: Create a 32 bits TBitmap from self content. }
    function Create32BitsBitmap: TGLBitmap;

    {: Width of the bitmap.<p> }
    property Width: Integer read fWidth write SetWidth;
    {: Height of the bitmap. }
    property Height: Integer read fHeight write SetHeight;
    {: Depth of the bitmap. }
    property Depth: Integer read fDepth write SetDepth;
    {: Number of MipMap levels }
    property MipLevels: Integer read fMipLevels;
    {: OpenGL color format }
    property ColorFormat: GLenum read fColorFormat write SetColorFormat;
    {: Recommended texture internal format }
    property InternalFormat: TGLInternalFormat read fInternalFormat write
      SetInternalFormat;
    {: OpenGL data type }
    property DataType: GLenum read fDataType write SetDataType;
    {: Size in bytes of pixel or block }
    property ElementSize: Integer read fElementSize;

    property CubeMap: Boolean read fCubeMap write SetCubeMap;

    property TextureArray: Boolean read fTextureArray write SetArray;
    {: Access to a specific Bitmap ScanLine.<p>
      index should be in the [0; Height[ range.<p>
      Warning : this function is NOT protected against invalid indexes,
      and invoking it is invalid if the bitmap is Empty. }
    property ScanLine[index: Integer]: PGLPixel32Array read GetScanLine;

    property VerticalReverseOnAssignFromBitmap: Boolean read
      FVerticalReverseOnAssignFromBitmap write
        FVerticalReverseOnAssignFromBitmap;

    {: Grants direct access to the bitmap's data.<p>
      This property is equivalent to ScanLine[0], and may be nil if the
      bitmap is empty. }
    property Data: PGLPixel32Array read FData;

    {: Set Blank to true if you actually don't need to allocate data in main
      menory.<p>
      Useful for textures that are generated by the GPU on the fly. }
    property Blank: boolean read FBlank write SetBlank;

    {: Set Alpha channel values to the pixel intensity.<p>
      The intensity is calculated as the mean of RGB components. }
    procedure SetAlphaFromIntensity;
    {: Set Alpha channel to 0 for pixels of given color, 255 for others).<p>
      This makes pixels of given color totally transparent while the others
      are completely opaque. }
    procedure SetAlphaTransparentForColor(const aColor: TColor); overload;
    procedure SetAlphaTransparentForColor(const aColor: TGLPixel32); overload;
    procedure SetAlphaTransparentForColor(const aColor: TGLPixel24); overload;
    {: Set Alpha channel values to given byte value. }
    procedure SetAlphaToValue(const aValue: Byte);
    {: Set Alpha channel values to given float [0..1] value. }
    procedure SetAlphaToFloatValue(const aValue: Single);
    {: Inverts the AlphaChannel component.<p>
      What was transparent becomes opaque and vice-versa. }
    procedure InvertAlpha;
    {: AlphaChannel components are replaced by their sqrt.<p> }
    procedure SqrtAlpha;

    {: Apply a brightness (scaled saturating) correction to the RGB components. }
    procedure BrightnessCorrection(const factor: Single);
    {: Apply a gamma correction to the RGB components. }
    procedure GammaCorrection(const gamma: Single);

    {: Downsample the bitmap by a factor of 2 in both dimensions.<p>
      If one of the dimensions is 1 or less, does nothing. }
    procedure DownSampleByFactor2;

    {: Registers the bitmap's content as an OpenGL texture map.<p>
      Legal values for bytesPerPixel are :<ul>
      <li>4 : RGB+A (32 bits)
      <li>3 : RGB (24 bits)
      <li>1 : Alpha channel only (8 bits)
      </ul>The texWidth and texHeight parameters are used to return
      the actual width and height of the texture (that can be different
      from the size of the bitmap32). }
    procedure RegisterAsOpenGLTexture(target: TGLUInt;
      minFilter: TGLMinFilter;
      texFormat: TGLEnum;
      out texWidth: integer;
      out texHeight: integer;
      out texDepth: integer); overload;

    {: Helper version of RegisterAsOpenGLTexture. }
    procedure RegisterAsOpenGLTexture(target: TGLUInt;
      minFilter: TGLMinFilter;
      texFormat: TGLEnum); overload;

    {: Reads the given area from the current active OpenGL rendering context.<p>
      The best spot for reading pixels is within a SceneViewer's PostRender
      event : the scene has been fully rendered and the OpenGL context
      is still active. }
    procedure ReadPixels(const area: TGLRect);
    {: Draws the whole bitmap at given position in the current OpenGL context.<p>
      This function must be called with a rendering context active.<p>
      Blending and Alpha channel functions are not altered by this function
      and must be adjusted separately. }
    procedure DrawPixels(const x, y: Single);

    {: Converts a grayscale 'elevation' bitmap to normal map.<p>
      Actually, only the Green component in the original bitmap is used. }
    procedure GrayScaleToNormalMap(const scale: Single;
      wrapX: Boolean = True; wrapY: Boolean = True);
    {: Assumes the bitmap content is a normal map and normalizes all pixels.<p> }
    procedure NormalizeNormalMap;
    procedure AssignToBitmap(aBitmap: TGLBitmap);
    procedure UnMipmap; override;
  end;

  // TRasterFileFormat
  //
  TRasterFileFormat = class
  public
    BaseImageClass: TGLBaseImageClass;
    Extension: string;
    Description: string;
    DescResID: Integer;
  end;

  // TRasterFileFormatsList
  //
  {: Stores registered raster file formats. }
  TRasterFileFormatsList = class(TPersistentObjectList)
  public
    { Public Declarations }
    destructor Destroy; override;

    procedure Add(const Ext, Desc: string; DescID: Integer; AClass:
      TGLBaseImageClass);
    function FindExt(ext: string): TGLBaseImageClass;
    function FindFromFileName(const fileName: string): TGLBaseImageClass;
    function FindFromStream(const AStream: TStream): TGLBaseImageClass;
    procedure Remove(AClass: TGLBaseImageClass);
    procedure BuildFilterStrings(imageFileClass: TGLBaseImageClass;
      var descriptions, filters: string;
      formatsThatCanBeOpened: Boolean = True;
      formatsThatCanBeSaved: Boolean = False);
    function FindExtByIndex(index: Integer;
      formatsThatCanBeOpened: Boolean = True;
      formatsThatCanBeSaved: Boolean = False): string;
  end;

  EInvalidRasterFile = class(Exception);

procedure BGR24ToRGB24(src, dest: Pointer; pixelCount: Integer);
procedure BGR24ToRGBA32(src, dest: Pointer; pixelCount: Integer);
procedure RGB24ToRGBA32(src, dest: Pointer; pixelCount: Integer);
procedure BGRA32ToRGBA32(src, dest: Pointer; pixelCount: Integer);

procedure GammaCorrectRGBArray(base: Pointer; pixelCount: Integer;
  gamma: Single);
procedure BrightenRGBArray(base: Pointer; pixelCount: Integer;
  factor: Single);
//: Read access to the list of registered vector file formats
function GetRasterFileFormats: TRasterFileFormatsList;
{: Returns an extension by its index
   in the internal image files dialogs filter.<p>
   Use InternalImageFileFormatsFilter to obtain the filter. }
function RasterFileFormatExtensionByIndex(index: Integer): string;

procedure RegisterRasterFormat(const AExtension, ADescription: string;
  AClass: TGLBaseImageClass);
procedure UnregisterRasterFormat(AClass: TGLBaseImageClass);
//: Return an optimal number of texture pyramid
function GetImageLodNumber(w, h, d: integer): Integer;

var
  vVerticalFlipDDS: Boolean = true;
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  VectorGeometry, GLStrings;

var
  vRasterFileFormats: TRasterFileFormatsList;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Raster File Registries'}{$ENDIF}

  // GetRasterFileFormats
  //

function GetRasterFileFormats: TRasterFileFormatsList;
begin
  if not Assigned(vRasterFileFormats) then
    vRasterFileFormats := TRasterFileFormatsList.Create;
  Result := vRasterFileFormats;
end;

// RegisterRasterFormat
//

procedure RegisterRasterFormat(const AExtension, ADescription: string;
  AClass: TGLBaseImageClass);
begin
  RegisterClass(AClass);
  GetRasterFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

// UnregisterRasterFormat
//

procedure UnregisterRasterFormat(AClass: TGLBaseImageClass);
begin
  if Assigned(vRasterFileFormats) then
    vRasterFileFormats.Remove(AClass);
end;

// RasterFileFormatExtensionByIndex
//

function RasterFileFormatExtensionByIndex(index: Integer): string;
begin
  Result := GetRasterFileFormats.FindExtByIndex(index);
end;

// TRasterFileFormatsList.Destroy
//

destructor TRasterFileFormatsList.Destroy;
begin
  Clean;
  inherited;
end;

// Add
//

procedure TRasterFileFormatsList.Add(const Ext, Desc: string; DescID: Integer;
  AClass: TGLBaseImageClass);
var
  newRec: TRasterFileFormat;
begin
  newRec := TRasterFileFormat.Create;
  with newRec do
  begin
    Extension := AnsiLowerCase(Ext);
    BaseImageClass := AClass;
    Description := Desc;
    DescResID := DescID;
  end;
  inherited Add(newRec);
end;

// FindExt
//

function TRasterFileFormatsList.FindExt(ext: string): TGLBaseImageClass;
var
  i: Integer;
begin
  ext := AnsiLowerCase(ext);
  for i := Count - 1 downto 0 do
    with TRasterFileFormat(Items[I]) do
    begin
      if Extension = ext then
      begin
        Result := BaseImageClass;
        Exit;
      end;
    end;
  Result := nil;
end;

// FindFromFileName
//

function TRasterFileFormatsList.FindFromFileName(const fileName: string):
  TGLBaseImageClass;
var
  ext: string;
begin
  ext := ExtractFileExt(Filename);
  System.Delete(ext, 1, 1);
  Result := FindExt(ext);
  if not Assigned(Result) then
    raise EInvalidRasterFile.CreateFmt(glsUnknownExtension,
      [ext, 'GLFile' + UpperCase(ext)]);
end;

// FindFromStream
//

function TRasterFileFormatsList.FindFromStream(const AStream: TStream):
  TGLBaseImageClass;
var
  ext: string;
  magic: array[0..1] of LongWord;
begin
  AStream.ReadBuffer(magic, 2*SizeOf(Longword));
  AStream.Seek(-2*SizeOf(Longword), 1);

  if magic[0] = $20534444 then
    ext := 'DDS'
  else if magic[1]=$4354334F then
    ext := 'O3TC'
  else if (magic[0] and $0000FFFF)=$00003F23 then
    ext := 'HDR'
  else if (magic[0] = $474E5089) and (magic[1] = $0A1A0A0D) then
    ext := 'PNG';

  Result := FindExt(ext);
  if not Assigned(Result) then
    raise EInvalidRasterFile.CreateFmt(glsUnknownExtension,
      [ext, 'GLFile' + UpperCase(ext)]);
end;

// Remove
//

procedure TRasterFileFormatsList.Remove(AClass: TGLBaseImageClass);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if TRasterFileFormat(Items[i]).BaseImageClass.InheritsFrom(AClass) then
      DeleteAndFree(i);
  end;
end;

// BuildFilterStrings
//

procedure TRasterFileFormatsList.BuildFilterStrings(
  imageFileClass: TGLBaseImageClass;
  var descriptions, filters: string;
  formatsThatCanBeOpened: Boolean = True;
  formatsThatCanBeSaved: Boolean = False);
var
  k, i: Integer;
  p: TRasterFileFormat;
begin
  descriptions := '';
  filters := '';
  k := 0;
  for i := 0 to Count - 1 do
  begin
    p := TRasterFileFormat(Items[i]);
    if p.BaseImageClass.InheritsFrom(imageFileClass) and (p.Extension <> '')
      and ((formatsThatCanBeOpened and (dfcRead in
      p.BaseImageClass.Capabilities))
      or (formatsThatCanBeSaved and (dfcWrite in p.BaseImageClass.Capabilities)))
        then
    begin
      with p do
      begin
        if k <> 0 then
        begin
          descriptions := descriptions + '|';
          filters := filters + ';';
        end;
        if (Description = '') and (DescResID <> 0) then
          Description := LoadStr(DescResID);
        FmtStr(descriptions, '%s%s (*.%s)|*.%2:s', [descriptions, Description,
          Extension]);
        filters := filters + '*.' + Extension;
        Inc(k);
      end;
    end;
  end;
  if (k > 1) and (not formatsThatCanBeSaved) then
    FmtStr(descriptions, '%s (%s)|%1:s|%s',
      [glsAllFilter, filters, descriptions]);
end;

// FindExtByIndex
//

function TRasterFileFormatsList.FindExtByIndex(index: Integer;
  formatsThatCanBeOpened: Boolean = True;
  formatsThatCanBeSaved: Boolean = False): string;
var
  i: Integer;
  p: TRasterFileFormat;
begin
  Result := '';
  if index > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      p := TRasterFileFormat(Items[i]);
      if (formatsThatCanBeOpened and (dfcRead in p.BaseImageClass.Capabilities))
        or (formatsThatCanBeSaved and (dfcWrite in
        p.BaseImageClass.Capabilities)) then
      begin
        if index = 1 then
        begin
          Result := p.Extension;
          Break;
        end
        else
          Dec(index);
      end;
    end;
  end;
end;

function GetImageLodNumber(w, h, d: integer): Integer;
var
  i: Integer;
begin
  i := 1;
  while ((w > 1) or (h > 1) or (d > 1)) do
  begin
    if w > 1 then
      w := w div 2
    else
      w := 1;
    if h > 1 then
      h := h div 2
    else
      h := 1;
    if d > 1 then
      d := d div 2
    else
      d := 1;
    Inc(i);
  end;
  Result := i;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'RGBA Utils'}{$ENDIF}
// GammaCorrectRGBArray
//

procedure GammaCorrectRGBArray(base: Pointer; pixelCount: Integer;
  gamma: Single);
type
  PByte = ^Byte;
var
  vGammaLUT: array[0..255] of Byte;
  invGamma: Single;
   i : PtrUInt;
begin
  if pixelCount < 1 then
    Exit;
  Assert(gamma > 0);
  // build LUT
  if gamma < 0.1 then
    invGamma := 10
  else
    invGamma := 1 / gamma;
  for i := 0 to 255 do
    vGammaLUT[i] := Round(255 * Power(i * (1 / 255), InvGamma));
  // perform correction
  for i := PtrUInt(base) to PtrUInt(base) + longword(pixelCount * 3 - 1) do
    PByte(i)^ := vGammaLUT[PByte(i)^];
end;

// GammaCorrectRGBAArray
//

procedure GammaCorrectRGBAArray(base: Pointer; pixelCount: Integer;
  gamma: Single);
type
  PByte = ^Byte;
var
  vGammaLUT: array[0..255] of Byte;
  pLUT: PByteArray;
  invGamma: Single;
   i, n : PtrUInt;
begin
  if pixelCount < 1 then
    Exit;
  Assert(gamma > 0);
  // build LUT
  if gamma < 0.1 then
    invGamma := 10
  else
    invGamma := 1 / gamma;
  for i := 0 to 255 do
    vGammaLUT[i] := Round(255 * Power(i * (1 / 255), InvGamma));
  // perform correction
  n := PtrUInt(base) + longword(pixelCount * 4);
  i := PtrUInt(base);
  pLUT := @vGammaLUT[0];
  while i < n do
  begin
    PByte(i)^ := pLUT^[PByte(i)^];
    Inc(i);
    PByte(i)^ := pLUT^[PByte(i)^];
    Inc(i);
    PByte(i)^ := pLUT^[PByte(i)^];
    Inc(i, 2);
  end;
end;

// BrightenRGBArray
//

procedure BrightenRGBArray(base: Pointer; pixelCount: Integer;
  factor: Single);
type
  PByte = ^Byte;
var
  vBrightnessLUT: array[0..255] of Byte;
   i : PtrUint;
   k : Integer;
begin
  if pixelCount < 1 then
    Exit;
  Assert(factor >= 0);
  // build LUT
  for i := 0 to 255 do
  begin
    k := Round(factor * i);
    if k > 255 then
      k := 255;
    vBrightnessLUT[i] := Byte(k);
  end;
  // perform correction
  for i := PtrUInt(base) to PtrUInt(base) + longword(pixelCount * 3 - 1) do
    PByte(i)^ := vBrightnessLUT[PByte(i)^];
end;

// BrightenRGBAArray
//

procedure BrightenRGBAArray(base: Pointer; pixelCount: Integer;
  factor: Single);
type
  PByte = ^Byte;
var
  vBrightnessLUT: array[0..255] of Byte;
  pLUT: PByteArray;
  i, n : PtrUInt;
  k : Integer;
begin
  if pixelCount < 1 then
    Exit;
  Assert(factor >= 0);
  // build LUT
  for i := 0 to 255 do
  begin
    k := Round(factor * i);
    if k > 255 then
      k := 255;
    vBrightnessLUT[i] := k;
  end;
  // perform correction
  n := PtrUInt(base) + longword(pixelCount * 4);
  i := PtrUInt(base);
  pLUT := @vBrightnessLUT[0];
  while i < n do
  begin
    PByte(i)^ := pLUT^[PByte(i)^];
    Inc(i);
    PByte(i)^ := pLUT^[PByte(i)^];
    Inc(i);
    PByte(i)^ := pLUT^[PByte(i)^];
    Inc(i, 2);
  end;
end;

// BGR24ToRGB24
//

procedure BGR24ToRGB24(src, dest: Pointer; pixelCount: Integer); register;
begin
  while pixelCount > 0 do
  begin
    PAnsiChar(dest)[0] := PAnsiChar(src)[2];
    PAnsiChar(dest)[1] := PAnsiChar(src)[1];
    PAnsiChar(dest)[2] := PAnsiChar(src)[0];
    dest := Pointer(PtrUInt(dest) + 3);
    src := Pointer(PtrUInt(src) + 3);
    Dec(pixelCount);
  end;
end;

// BGR24ToRGBA32
//
{$ifNdef NO_ASM}
procedure BGR24ToRGBA32(src, dest: Pointer; pixelCount: Integer); register;
// EAX stores src
// EDX stores dest
// ECX stores pixelCount
asm
         push  edi
         cmp   ecx, 0
         jle   @@Done
         mov   edi, eax
         dec   ecx
         jz    @@Last
@@Loop:
         mov   eax, [edi]
         shl   eax, 8
         or    eax, $FF
         bswap eax
         mov   [edx], eax
         add   edi, 3
         add   edx, 4
         dec   ecx
         jnz   @@Loop
@@Last:
         mov   cx, [edi+1]
         shl   ecx, 16
         mov   ah, [edi]
         mov   al, $FF
         and   eax, $FFFF
         or    eax, ecx
         bswap eax
         mov   [edx], eax
@@Done:
         pop   edi
end;
{$ELSE}

procedure BGR24ToRGBA32(src, dest: Pointer; pixelCount: Integer);
begin
  while pixelCount > 0 do
  begin
    PAnsiChar(dest)[0] := PAnsiChar(src)[2];
    PAnsiChar(dest)[1] := PAnsiChar(src)[1];
    PAnsiChar(dest)[2] := PAnsiChar(src)[0];
    PAnsiChar(dest)[3] := #255;
    dest := Pointer(PtrUInt(dest) + 4);
    src := Pointer(PtrUInt(src) + 3);
    Dec(pixelCount);
  end;
end;
{$ENDIF}

// RGB24ToRGBA32
//
{$ifNdef NO_ASM}
procedure RGB24ToRGBA32(src, dest: Pointer; pixelCount: Integer); register;
// EAX stores src
// EDX stores dest
// ECX stores pixelCount
asm
         push  edi
         cmp   ecx, 0
         jle   @@Done
         mov   edi, eax
         dec   ecx
         jz    @@Last
@@Loop:
         mov   eax, [edi]
         or    eax, $FF000000
         mov   [edx], eax
         add   edi, 3
         add   edx, 4
         dec   ecx
         jnz   @@Loop
@@Last:
         mov   ax, [edi+1]
         shl   eax, 8
         mov   al, [edi];
         or    eax, $FF000000
         mov   [edx], eax
@@Done:
         pop   edi
end;
{$ELSE}

procedure RGB24ToRGBA32(src, dest: Pointer; pixelCount: Integer);
begin
  while pixelCount > 0 do
  begin
    PAnsiChar(dest)[0] := PAnsiChar(src)[0];
    PAnsiChar(dest)[1] := PAnsiChar(src)[1];
    PAnsiChar(dest)[2] := PAnsiChar(src)[2];
    PAnsiChar(dest)[3] := #255;
    dest := Pointer(PtrUInt(dest) + 4);
    src := Pointer(PtrUInt(src) + 3);
    Dec(pixelCount);
  end;
end;
{$ENDIF}

// BGRA32ToRGBA32
//
{$ifNdef NO_ASM}
procedure BGRA32ToRGBA32(src, dest: Pointer; pixelCount: Integer); register;
// EAX stores src
// EDX stores dest
// ECX stores pixelCount
asm
         push  edi
         cmp   ecx, 0
         jle   @@Done
         mov   edi, eax
@@Loop:
         mov   eax, [edi]
         shl   eax, 8
         mov   al, [edi+3]
         bswap eax
         mov   [edx], eax
         add   edi, 4
         add   edx, 4
         dec   ecx
         jnz   @@Loop
@@Done:
         pop   edi
end;
{$ELSE}

procedure BGRA32ToRGBA32(src, dest: Pointer; pixelCount: Integer);
begin
  while pixelCount > 0 do
  begin
    PAnsiChar(dest)[0] := PAnsiChar(src)[2];
    PAnsiChar(dest)[1] := PAnsiChar(src)[1];
    PAnsiChar(dest)[2] := PAnsiChar(src)[0];
    PAnsiChar(dest)[3] := PAnsiChar(src)[3];
    dest := Pointer(PtrUInt(dest) + 4);
    src := Pointer(PtrUInt(src) + 4);
    Dec(pixelCount);
  end;
end;
{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TGLBaseImage ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLBaseImage'}{$ENDIF}
// Create
//

constructor TGLBaseImage.Create;
begin
  inherited Create(Self);
  fLevels := TList.Create;
  fWidth := 0;
  fHeight := 0;
  fDepth := 0;
  fMipLevels := 1; // first level always is present
  fColorFormat := GL_RGBA;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fCubeMap := false;
  fTextureArray := false;
end;

// Destroy
//

destructor TGLBaseImage.Destroy;
begin
  if Assigned(fData) then
  begin
    FreeMem(fData);
    fData := nil;
  end;
  FreeAndNil(fLevels);
  inherited Destroy;
end;

// Assign
//

procedure TGLBaseImage.Assign(Source: TPersistent);
var
  img: TGLBaseImage;
  size: integer;
begin
  if Source is TGLBaseImage then
  begin
    img := Source as TGLBaseImage;
    fLevels.Clear;
    fLevels.Assign(img.fLevels);
    fWidth := img.fWidth;
    fHeight := img.fHeight;
    fDepth := img.fDepth;
    fMipLevels := img.fMipLevels;
    fColorFormat := img.fColorFormat;
    fInternalFormat := img.fInternalFormat;
    fDataType := img.fDataType;
    fElementSize := img.fElementSize;
    fCubeMap := img.fCubeMap;
    fTextureArray := img.fTextureArray;
    size := img.DataSize;
    ReallocMem(FData, size);
    Move(img.fData^, fData^, size);
  end
  else
    inherited;
end;

// LevelSize
//

function TGLBaseImage.LevelSize(const level: Integer): integer;
var
  w, h, d, bw, bh, size: integer;
begin
  w := fWidth shr level;
  h := fHeight shr level;
  if fTextureArray then
    d := fDepth
  else
    d := fDepth shr level;
  if w = 0 then
    w := 1;
  if h = 0 then
    h := 1;
  if d = 0 then
    d := 1;
  if IsCompressed then
  begin
    bw := (w + 3) div 4;
    bh := (h + 3) div 4;
  end
  else
  begin
    bw := w;
    bh := h;
  end;
  size := bw * bh * d * fElementSize;
  // Align to Double Word
  if (size and 3) <> 0 then
  begin
    size := size and MaxInt - 3;
    Inc(size, 4);
  end;
  Result := size;
end;

// DataSize
//

function TGLBaseImage.DataSize: integer;
var
  i: integer;
  s: integer;
begin
  s := 0;
  if not IsEmpty then
  begin
    for i := 0 to fMipLevels - 1 do
      s := s + LevelSize(i);
    if fCubeMap then
      s := s * 6;
  end;
  Result := s;
end;

//  GetLevelData
//

function TGLBaseImage.GetLevelData(const level: Integer; face: Integer):
  PGLPixel32Array;
begin
  Result := fData;
  Assert(level < fMipLevels);
  face := face - GL_TEXTURE_CUBE_MAP_POSITIVE_X;
  Assert((face >= 0) and (face < 6));
  // Add level offset
  if (face * fMipLevels + level) < fLevels.Count then
    Result := PGLPixel32Array(PtrUInt(Result) + PtrUInt(fLevels.Items[face *
      fMipLevels + level]));
end;

// IsEmpty
//

function TGLBaseImage.IsEmpty: Boolean;
begin
  Result := (fWidth = 0) or (fHeight = 0);
end;

// isCompressed
//

function TGLBaseImage.isCompressed: Boolean;
begin
  Result := IsCompressedFormat(fInternalFormat);
end;

// IsVolume
//

function TGLBaseImage.IsVolume: boolean;
begin
  Result := (fDepth > 0) and not fTextureArray;
end;

// ConvertCrossToCubemap
//

function TGLBaseImage.ConvertCrossToCubemap: boolean;
var
  fW, fH, pW, pH: integer;
  lData: PByteArray;
  ptr: PGLubyte;
  i, j: integer;
begin
  Result := false;
  // Can't already be a cubemap
  if fCubeMap or fTextureArray then
    Exit;
  //this function only supports vertical cross format for now (3 wide by 4 high)
  if (fWidth div 3 <> fHeight div 4)
    or (fWidth mod 3 <> 0)
    or (fHeight mod 4 <> 0)
    or (fDepth > 0) then
    Exit;
  // Mipmaps are not supported
  fMipLevels := 1;
  fLevels.Clear;
  // Get the source data
  lData := PByteArray(fData);
  if IsCompressed then
  begin
    fW := (fWidth + 3) div 4;
    fH := (fHeight + 3) div 4;
  end
  else
  begin
    fW := fWidth;
    fH := fHeight;
  end;
  pW := fW div 3;
  pH := fH div 4;
  GetMem(fData, fW * fH * fElementSize);
  // Extract the faces
  ptr := PGLubyte(fData);
  // positive X
  fLevels.Add(Pointer(PtrInt(ptr) - PtrInt(fData)));
  for j := 0 to pH - 1 do
  begin
    Move(lData[((fH - (pH + j + 1)) * fW + 2 * pW) * fElementSize],
      ptr^, pW * fElementSize);
    Inc(ptr, pW * fElementSize);
  end;
  // negative X
  fLevels.Add(Pointer(PtrInt(ptr) - PtrInt(fData)));
  for j := 0 to pH - 1 do
  begin
    Move(lData[(fH - (pH + j + 1)) * fW * fElementSize],
      ptr^, pW * fElementSize);
    Inc(ptr, pW * fElementSize);
  end;
  // positive Y
  fLevels.Add(Pointer(PtrInt(ptr) - PtrInt(fData)));
  for j := 0 to pH - 1 do
  begin
    Move(lData[((4 * pH - j - 1) * fW + pW) * fElementSize],
      ptr^, pW * fElementSize);
    Inc(ptr, pW * fElementSize);
  end;
  // negative Y
  fLevels.Add(Pointer(PtrInt(ptr) - PtrInt(fData)));
  for j := 0 to pH - 1 do
  begin
    Move(lData[((2 * pH - j - 1) * fW + pW) * fElementSize],
      ptr^, pW * fElementSize);
    Inc(ptr, pW * fElementSize);
  end;
  // positive Z
  fLevels.Add(Pointer(PtrInt(ptr) - PtrInt(fData)));
  for j := 0 to pH - 1 do
  begin
    Move(lData[((fH - (pH + j + 1)) * fW + pW) * fElementSize],
      ptr^, pW * fElementSize);
    Inc(ptr, pW * fElementSize);
  end;
  // negative Z
  fLevels.Add(Pointer(PtrInt(ptr) - PtrInt(fData)));
  for j := 0 to pH - 1 do
    for i := 0 to pW - 1 do
    begin
      Move(lData[(j * fW + 2 * pW - (i + 1)) * fElementSize],
        ptr^, fElementSize);
      Inc(ptr, fElementSize);
    end;
  // Set the new # of faces, width and height
  fCubeMap := true;
  fWidth := pW;
  fHeight := pH;
  if IsCompressed then
  begin
    fWidth := fWidth * 4;
    fHeight := fHeight * 4;
  end;
  FreeMem(lData);
  Result := true;
end;

// ConvertToVolume
//

function TGLBaseImage.ConvertToVolume(const col, row: Integer; const MakeArray:
  Boolean): Boolean;
var
  fW, fH, sW, sH, sD: Integer;
  lData: PByteArray;
  ptr: PGLubyte;
  i, j, k: integer;
begin
  Result := false;
  if fCubeMap then
    Exit;

  if (fDepth > 0) and not fTextureArray and MakeArray then
  begin
    // Let volume be array
    fTextureArray := true;
    Result := true;
    Exit;
  end;
  if fTextureArray and not MakeArray then
  begin
    // Let array be volume
    fTextureArray := false;
    Result := true;
    Exit;
  end;
  // Check sizes
  sD := col * row;
  if sD < 1 then
    Exit;
  if IsCompressed then
  begin
    fW := (fWidth + 3) div 4;
    fH := (fHeight + 3) div 4;
  end
  else
  begin
    fW := fWidth;
    fH := fHeight;
  end;
  sW := fW div col;
  sH := fH div row;
  if (sW = 0) or (sH = 0) then
    Exit;

  // Mipmaps are not supported
  fLevels.Clear;
  fMipLevels := 1;
  // Get the source data
  lData := PByteArray(fData);
  GetMem(fData, sW * sH * sD * fElementSize);
  ptr := PGLubyte(fData);
  for i := 0 to row - 1 do
    for j := 0 to col - 1 do
      for k := 0 to sH - 1 do
      begin
        Move(lData[(i * fW * sH + j * sW + k * fW) * fElementSize],
          ptr^, sW * fElementSize);
        Inc(ptr, sW * fElementSize);
      end;

  fWidth := sW;
  fHeight := sH;
  fDepth := sD;
  fTextureArray := MakeArray;
  if IsCompressed then
  begin
    fWidth := fWidth * 4;
    fHeight := fHeight * 4;
  end;
  FreeMem(lData);
  Result := true;
end;

// Narrow
//

procedure TGLBaseImage.Narrow;
begin
  // Check for already norrow
  if (fColorFormat = GL_RGBA)
    and (fDepth = 0)
    and (fDataType = GL_UNSIGNED_BYTE)
    and (fMipLevels = 1)
    and not (fTextureArray or fCubeMap) then
    Exit;

  UnMipmap;
    fDepth := 0;
    fColorFormat := GL_RGBA;
    fInternalFormat := tfRGBA8;
    fDataType := GL_UNSIGNED_BYTE;
    fElementSize := 4;
    fCubeMap := false;
    fTextureArray := false;
    ReallocMem(fData, DataSize);
end;

// UnMipmap
//

procedure TGLBaseImage.UnMipmap;
begin
  fLevels.Clear;
  fMipLevels := 1;
end;

function TGLBaseImage.GetData: PGLPixel32Array;
begin
  Result := fData;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TGLBitmap32 ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLBitmap32'}{$ENDIF}
// Create
//

constructor TGLBitmap32.Create;
begin
  inherited Create;
  SetBlank(false);
end;

// Destroy
//

destructor TGLBitmap32.Destroy;
begin
  inherited Destroy;
end;

// Assign
//

procedure TGLBitmap32.Assign(Source: TPersistent);
var
  bmp: TGLBitmap;
  graphic: TGLGraphic;
begin
  if Source = nil then
  begin
    FWidth := 0;
    FHeight := 0;
    Narrow;
    ReallocMem(FData, 0);
  end
  else if (Source is TGLBitmap32) or (Source is TGLBaseImage) then
  begin
    // duplicate the data
    if (Source is TGLBitmap32) then
      FBlank := TGLBitmap32(Source).fBlank
    else
      FBlank := false;
    FWidth := (Source as TGLBaseImage).fWidth;
    FHeight := (Source as TGLBaseImage).fHeight;
    FDepth := (Source as TGLBaseImage).fDepth;
    fMipLevels := (Source as TGLBaseImage).fMipLevels;
    fCubeMap := (Source as TGLBaseImage).fCubeMap;
    fColorFormat := (Source as TGLBaseImage).fColorFormat;
    fInternalFormat := (Source as TGLBaseImage).fInternalFormat;
    fDataType := (Source as TGLBaseImage).fDataType;
    fElementSize := (Source as TGLBaseImage).fElementSize;
    fLevels.Clear;
    fLevels.Assign((Source as TGLBaseImage).fLevels);
    fTextureArray := (Source as TGLBaseImage).fTextureArray;
    ResourceName := (Source as TGLBaseImage).ResourceName;

    if not FBlank then
    begin
      ReallocMem(FData, DataSize);
      Move(TGLBaseImage(Source).Data^, Data^, DataSize);
    end;
  end
  else if Source is TGLGraphic then
  begin
    if (Source is TGLBitmap)
         {$ifNdef FPC}  and  (TGLBitmap(Source).PixelFormat in [glpf24bit, glpf32bit]){$ENDIF}
                        and ((TGLBitmap(Source).Width and 3)=0) then begin
{$ifdef FPC}
      // in FPC this is pixel wize and reads all pixelFormats.
      AssignFrom24BitsBitmap(TGLBitmap(Source));
{$ELSE}
      if TGLBitmap(Source).PixelFormat = glpf24bit then
        AssignFrom24BitsBitmap(TGLBitmap(Source))
      else
        AssignFrom32BitsBitmap(TGLBitmap(Source))
{$ENDIF}
    end
    else
    begin
      graphic := TGLGraphic(Source);
      bmp := TGLBitmap.Create;
      try
        // crossbuilder: useless to set pixelformat before setting the size ?
        //               or maybe just useless at all on gtk .. as soon as
        //               bmp.canvas is touched, it's the pixelformat of the device
        //               no matter what was adjusted before ??
        // bmp.PixelFormat:=glpf24bit;
        // bmp.Height:=graphic.Height;
        // crossbuilder: using setsize because setting width or height while
        // the other one is zero results in not setting with/hight
        bmp.SetSize(graphic.Width,graphic.Height);
        bmp.PixelFormat := glpf24bit;
        if (graphic.Width and 3) = 0 then
        begin
          //bmp.Width:=graphic.Width;
          bmp.Canvas.Draw(0, 0, graphic);
        end
        else
        begin
          bmp.Width := (graphic.Width and $FFFC) + 4;
          bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), graphic);
        end;
        AssignFrom24BitsBitmap(bmp);
      finally
        bmp.Free;
      end;
    end;
{$IFDEF GLS_Graphics32_SUPPORT}
  end
  else if Source is TBitmap32 then
  begin
    Narrow;
    AssignFromBitmap32(TBitmap32(Source));
{$ENDIF}
  end
  else
    inherited;
end;

// AssignFrom24BitsBitmap
//
{$IFDEF FPC}

procedure TGLBitmap32.AssignFrom24BitsBitmap(aBitmap: TGLBitmap);
var
  y, x: Integer;
  pDest: PByte;
  pixel: TFPColor;
  IntfImg: TLazIntfImage;
begin
  Assert((aBitmap.Width and 3) = 0);
  FWidth := aBitmap.Width;
  FHeight := aBitmap.Height;
  FDepth := 0;
  fMipLevels := 1;
  fColorFormat := GL_RGBA;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fLevels.Clear;
  fCubeMap := false;
  fTextureArray := false;
  ReallocMem(FData, DataSize);

  IntfImg := aBitmap.CreateIntfImage;
  try
    pDest := PByte(FData);
    if not (VerticalReverseOnAssignFromBitmap) then
    begin
      for y := FHeight - 1 downto 0 do
        for x := 0 to FWidth - 1 do
        begin
          pixel := IntfImg.Colors[x, y];
          pDest^ := pixel.red shr 8;
          inc(pDest);
          pDest^ := pixel.green shr 8;
          inc(pDest);
          pDest^ := pixel.blue shr 8;
          inc(pDest);
          pDest^ := pixel.alpha shr 8;
          inc(pDest);
        end;
    end
    else
    begin
      for y := 0 to FHeight - 1 do
        for x := 0 to FWidth - 1 do
        begin
          pixel := IntfImg.Colors[x, y];
          pDest^ := pixel.red shr 8;
          inc(pDest);
          pDest^ := pixel.green shr 8;
          inc(pDest);
          pDest^ := pixel.blue shr 8;
          inc(pDest);
          pDest^ := pixel.alpha shr 8;
          inc(pDest);
        end;
    end;
  finally
    IntfImg.Free;
  end;
end;
{$ELSE}

procedure TGLBitmap32.AssignFrom24BitsBitmap(aBitmap: TGLBitmap);
var
   y           : Integer;
   rowOffset   : PtrInt;
  pSrc, pDest: PAnsiChar;
begin
  Assert(aBitmap.PixelFormat = glpf24bit);
  FWidth := aBitmap.Width;
  FHeight := aBitmap.Height;
  FDepth := 0;
  fMipLevels := 1;
  if GL_EXT_bgra then
  begin
    fColorFormat := GL_BGR;
    fElementSize := 3;
  end
  else begin
    Assert((aBitmap.Width and 3) = 0);
    fColorFormat := GL_RGBA;
    fElementSize := 4;
  end;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fLevels.Clear;
  fCubeMap := false;
  fTextureArray := false;
  ReallocMem(FData, DataSize);
  FBlank := false;
  if Height > 0 then
  begin
    pDest := @PAnsiChar(FData)[Width * fElementSize * (Height - 1)];
    if Height = 1 then
    begin
      if GL_EXT_bgra then
      begin
        pSrc := BitmapScanLine(aBitmap, 0);
        Move(pSrc^, pDest^, Width*fElementSize);
      end
      else
        BGR24ToRGBA32(BitmapScanLine(aBitmap, 0), pDest, Width);
    end
    else
    begin
      if VerticalReverseOnAssignFromBitmap then
      begin
        pSrc := BitmapScanLine(aBitmap, Height - 1);
        rowOffset := PtrInt(BitmapScanLine(aBitmap, Height - 2)) -
          PtrInt(pSrc);
      end
      else
      begin
        pSrc := BitmapScanLine(aBitmap, 0);
        rowOffset := PtrInt(BitmapScanLine(aBitmap, 1)) - PtrInt(pSrc);
      end;
      if GL_EXT_bgra then
      begin
        for y := 0 to Height - 1 do
        begin
          Move(pSrc^, pDest^, Width*fElementSize);
          Dec(pDest, Width * fElementSize);
          Inc(pSrc, rowOffset);
        end;
      end
      else begin
        for y := 0 to Height - 1 do
        begin
          BGR24ToRGBA32(pSrc, pDest, Width);
          Dec(pDest, Width * fElementSize);
          Inc(pSrc, rowOffset);
        end;
      end;
    end;
  end;
end;
{$ENDIF}

// AssignFromBitmap24WithoutRGBSwap
//

procedure TGLBitmap32.AssignFromBitmap24WithoutRGBSwap(aBitmap: TGLBitmap);
var
   y           : Integer;
   rowOffset   : PtrInt;
  pSrc, pDest: PAnsiChar;
begin
  Assert(aBitmap.PixelFormat = glpf24bit);
  Assert((aBitmap.Width and 3) = 0);
  FWidth := aBitmap.Width;
  FHeight := aBitmap.Height;
  FDepth := 0;
  fMipLevels := 1;
  fColorFormat := GL_RGBA;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fLevels.Clear;
  fCubeMap := false;
  fTextureArray := false;
  ReallocMem(FData, DataSize);
  FBlank := false;
  if Height > 0 then
  begin
    pDest := @PAnsiChar(FData)[Width * 4 * (Height - 1)];
    if Height = 1 then
    begin
      RGB24ToRGBA32(BitmapScanLine(aBitmap, 0), pDest, Width);
    end
    else
    begin
      if VerticalReverseOnAssignFromBitmap then
      begin
        pSrc := BitmapScanLine(aBitmap, Height - 1);
        rowOffset := PtrInt(BitmapScanLine(aBitmap, Height - 2)) - PtrInt(pSrc);
      end
      else
      begin
        pSrc := BitmapScanLine(aBitmap, 0);
        rowOffset := PtrInt(BitmapScanLine(aBitmap, 1)) - PtrInt(pSrc);
      end;
      for y := 0 to Height - 1 do
      begin
        RGB24ToRGBA32(pSrc, pDest, Width);
        Dec(pDest, Width * 4);
        Inc(pSrc, rowOffset);
      end;
    end;
  end;
end;

// AssignFrom32BitsBitmap
//

procedure TGLBitmap32.AssignFrom32BitsBitmap(aBitmap: TGLBitmap);
var
   y           : Integer;
   rowOffset   : PtrInt;
  pSrc, pDest: PAnsiChar;
begin
  Assert(aBitmap.PixelFormat = glpf32bit);
  FWidth := aBitmap.Width;
  FHeight := aBitmap.Height;
  FDepth := 0;
  fMipLevels := 1;
  if GL_EXT_bgra then
    fColorFormat := GL_BGRA
  else begin
    Assert((aBitmap.Width and 3) = 0);
    fColorFormat := GL_RGBA;
  end;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fLevels.Clear;
  fCubeMap := false;
  fTextureArray := false;
  ReallocMem(FData, DataSize);
  FBlank := false;
  if Height > 0 then
  begin
    pDest := @PAnsiChar(FData)[Width * 4 * (Height - 1)];
    if VerticalReverseOnAssignFromBitmap then
    begin
      pSrc := BitmapScanLine(aBitmap, Height - 1);
      if Height > 1 then
        rowOffset := PtrInt(BitmapScanLine(aBitmap, Height - 2)) - PtrInt(pSrc)
      else
        rowOffset := 0;
    end
    else
    begin
      pSrc := BitmapScanLine(aBitmap, 0);
      if Height > 1 then
        rowOffset := PtrInt(BitmapScanLine(aBitmap, 1)) - PtrInt(pSrc)
      else
        rowOffset := 0;
    end;
    if GL_EXT_bgra then
    begin
      for y := 0 to Height - 1 do
      begin
        Move(pSrc^, pDest^, Width*4);
        Dec(pDest, Width*4);
        Inc(pSrc, rowOffset);
      end;
    end
    else begin
      for y := 0 to Height - 1 do
      begin
        BGRA32ToRGBA32(pSrc, pDest, Width);
        Dec(pDest, Width * 4);
        Inc(pSrc, rowOffset);
      end;
    end;
  end;
end;

{$IFDEF GLS_Graphics32_SUPPORT}
// AssignFromBitmap32
//

procedure TGLBitmap32.AssignFromBitmap32(aBitmap32: TBitmap32);
var
  y: Integer;
  pSrc, pDest: PAnsiChar;
begin
  Assert((aBitmap32.Width and 3) = 0);
  FWidth := aBitmap32.Width;
  FHeight := aBitmap32.Height;
  FDepth := 0;
  fMipLevels := 1;
  fColorFormat := GL_RGBA;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fLevels.Clear;
  fCubeMap := false;
  fTextureArray := false;
  ReallocMem(FData, DataSize);
  FBlank := false;
  if Height > 0 then
  begin
    pDest := @PAnsiChar(FData)[Width * 4 * (Height - 1)];
    for y := 0 to Height - 1 do
    begin
      if VerticalReverseOnAssignFromBitmap then
        pSrc := PAnsiChar(aBitmap32.ScanLine[Height - 1 - y])
      else
        pSrc := PAnsiChar(aBitmap32.ScanLine[y]);
      BGRA32ToRGBA32(pSrc, pDest, Width);
      Dec(pDest, Width * 4);
    end;
  end;
end;
{$ENDIF}

// AssignFromTexture2D
//

procedure TGLBitmap32.AssignFromTexture2D(textureHandle: Cardinal);
var
  oldTex: Cardinal;
begin
  glGetIntegerv(GL_TEXTURE_2D_BINDING_EXT, @oldTex);
  glBindTexture(GL_TEXTURE_2D, textureHandle);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @FWidth);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, @FHeight);
  FDepth := 0;
  fMipLevels := 1;
  fColorFormat := GL_RGBA;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fLevels.Clear;
  fCubeMap := false;
  fTextureArray := false;
  ReallocMem(FData, DataSize);
  glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, FData);
  FBlank := false;
  glBindTexture(GL_TEXTURE_2D, oldTex);
end;

// AssignFromTexture2D
//

procedure TGLBitmap32.AssignFromTexture2D(textureHandle: TGLTextureHandle);
var
  oldContext: TGLContext;
  contextActivate: Boolean;
begin
  if Assigned(textureHandle) and (textureHandle.Handle <> 0) then
  begin
    oldContext := CurrentGLContext;
    contextActivate := (oldContext <> textureHandle.RenderingContext);
    if contextActivate then
    begin
      if Assigned(oldContext) then
        oldContext.Deactivate;
      textureHandle.RenderingContext.Activate;
    end;

    try
      AssignFromTexture2D(textureHandle.Handle);
    finally
      if contextActivate then
      begin
        textureHandle.RenderingContext.Deactivate;
        if Assigned(oldContext) then
          oldContext.Activate;
      end;
    end;
  end
  else
  begin
    // Make image empty
    FWidth := 0;
    FHeight := 0;
    FDepth := 0;
    fMipLevels := 1;
    fColorFormat := GL_RGBA;
    fInternalFormat := tfRGBA8;
    fDataType := GL_UNSIGNED_BYTE;
    fElementSize := 4;
    fLevels.Clear;
    fCubeMap := false;
    fTextureArray := false;
    ReallocMem(FData, DataSize);
  end;
end;

// AssignFromTexture
//

procedure TGLBitmap32.AssignFromTexture(textureContext: TGLContext;
  const textureHandle: TGLenum;
  textureTarget: TGLenum;
  const CurrentFormat: Boolean;
  const intFormat: TGLInternalFormat = tfRGBA8;
  const colorFormat: TGLenum = 0;
  const dataType: TGLenum = 0);
var
  oldContext: TGLContext;
  contextActivate: Boolean;
  texFormat, texLod, texResident, optLod: Cardinal;
  level, faceCount, face: Integer;
  lData: PGLubyte;
  residentFormat: TGLInternalFormat;
  bCompressed: Boolean;
  vtcBuffer, top, bottom: PGLubyte;
  i, j, k: Integer;
  w, d, h, cw, ch: Integer;

  function blockOffset(x, y, z: Integer): Integer;
  begin

    if z >= (d and -4) then
      Result := fElementSize * (cw * ch * (d and -4) + x +
        cw * (y + ch * (z - 4 * ch)))
    else
      Result := fElementSize * (4 * (x + cw * (y + ch * floor(z / 4))) + (z and
        3));
  end;

begin
  oldContext := CurrentGLContext;
  contextActivate := (oldContext <> textureContext);
  if contextActivate then
  begin
    if Assigned(oldContext) then
      oldContext.Deactivate;
    textureContext.Activate;
  end;

  try
    textureContext.GLStates.SetGLCurrentTexture(0, textureTarget, textureHandle);

    fMipLevels := 0;
    glGetTexParameteriv(textureTarget, GL_TEXTURE_MAX_LEVEL, @texLod);
    if textureTarget = GL_TEXTURE_CUBE_MAP then
    begin
      fCubeMap := true;
      faceCount := 6;
      textureTarget := GL_TEXTURE_CUBE_MAP_POSITIVE_X;
    end
    else
    begin
      fCubeMap := false;
      faceCount := 1;
    end;
    fTextureArray := (textureTarget = GL_TEXTURE_1D_ARRAY)
      or (textureTarget = GL_TEXTURE_2D_ARRAY)
      or (textureTarget = GL_TEXTURE_CUBE_MAP_ARRAY);

    repeat
      // Check level existence
      glGetTexLevelParameteriv(textureTarget, fMipLevels,
        GL_TEXTURE_INTERNAL_FORMAT,
        @texFormat);
      if texFormat = 1 then
        Break;
      Inc(fMipLevels);
      if fMipLevels = 1 then
      begin
        glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_WIDTH, @fWidth);
        glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_HEIGHT,
          @fHeight);
        fDepth := 0;
        if (textureTarget = GL_TEXTURE_3D)
          or (textureTarget = GL_TEXTURE_2D_ARRAY)
          or (textureTarget = GL_TEXTURE_CUBE_MAP_ARRAY) then
          glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_DEPTH,
            @fDepth);
        residentFormat := OpenGLFormatToInternalFormat(texFormat);
        if CurrentFormat then
          fInternalFormat := residentFormat
        else
          fInternalFormat := intFormat;
        FindCompatibleDataFormat(fInternalFormat, fColorFormat, fDataType);
        // Substitute properties if need
        if colorFormat > 0 then
          fColorFormat := colorFormat;
        if dataType > 0 then
          fDataType := dataType;
        // Get optimal number or MipMap levels
        optLod := GetImageLodNumber(fWidth, fHeight, fDepth);
        if texLod > optLod then
          texLod := optLod;
        // Check for MipMap posibility
        if ((fInternalFormat >= tfFLOAT_R16)
          and (fInternalFormat <= tfFLOAT_RGBA32)) then
          texLod := 1;
      end;
    until fMipLevels = Integer(texLod);

    if fMipLevels > 0 then
    begin
      fElementSize := GetTextureElementSize(fColorFormat, fDataType);
      ReallocMem(FData, DataSize);
      fLevels.Clear;
      lData := PGLubyte(fData);
      bCompressed := IsCompressed;
      vtcBuffer := nil;
      w := fWidth;
      h := fHeight;
      d := fDepth;

      for face := 0 to faceCount - 1 do
      begin
        if fCubeMap then
          textureTarget := face + GL_TEXTURE_CUBE_MAP_POSITIVE_X;
        for level := 0 to fMipLevels - 1 do
        begin
          fLevels.Add(Pointer(PtrInt(lData) - PtrInt(fData)));
          if bCompressed then
          begin

            if GL_NV_texture_compression_vtc and (d > 1) and not fTextureArray
              then
            begin
              if level = 0 then
                GetMem(vtcBuffer, LevelSize(0));
              glGetCompressedTexImage(textureTarget, level, vtcBuffer);
              // Shufle blocks from VTC to S3TC
              cw := (w + 3) div 4;
              ch := (h + 3) div 4;
              top := lData;
              for k := 0 to d - 1 do
                for i := 0 to ch - 1 do
                  for j := 0 to cw - 1 do
                  begin
                    bottom := vtcBuffer;
                    Inc(bottom, blockOffset(j, i, k));
                    Move(bottom^, top^, fElementSize);
                    Inc(top, fElementSize);
                  end;
              if w > 1 then
                w := w div 2
              else
                w := 1;
              if h > 1 then
                h := h div 2
              else
                h := 1;
              if d > 1 then
                d := d div 2
              else
                d := 1;
            end
            else
              glGetCompressedTexImage(textureTarget, level, lData);
          end
          else
            glGetTexImage(textureTarget, level, fColorFormat, fDataType,
              lData);

          Inc(lData, LevelSize(level));
        end; // for level
      end; // for face
      if Assigned(vtcBuffer) then
        FreeMem(vtcBuffer);
      // Check memory corruption
      ReallocMem(FData, DataSize);
    end;

    FBlank := fMipLevels = 0;
    if FBlank then
    begin
      fMipLevels := 1;
      FreeMem(fData);
      fData := nil;
    end;
    CheckOpenGLError;
  finally
    if contextActivate then
    begin
      textureContext.Deactivate;
      if Assigned(oldContext) then
        oldContext.Activate;
    end;
  end;
end;

// Create32BitsBitmap
//

function TGLBitmap32.Create32BitsBitmap: TGLBitmap;
begin
  if FBlank then
  begin
    Result := nil;
    exit;
  end;

  Result := TGLBitmap.Create;
  AssignToBitmap(Result);
end;

// SetWidth
//

procedure TGLBitmap32.SetWidth(val: Integer);
begin
  //  if (val and 3)>0 then
  //    val:=(val and $FFFC)+4;
  if val <> FWidth then
  begin
    Assert(val >= 0);
    FWidth := val;
    FBlank := true;
  end;
end;

// SetHeight
//

procedure TGLBitmap32.SetHeight(const val: Integer);
begin
  if val <> FHeight then
  begin
    Assert(val >= 0);
    FHeight := val;
    FBlank := true;
  end;
end;

// SetDepth
//

procedure TGLBitmap32.SetDepth(const val: Integer);
begin
  if val <> FDepth then
  begin
    Assert(val >= 0);
    FDepth := val;
    FBlank := true;
  end;
end;

// SetCubeMap
//

procedure TGLBitmap32.SetCubeMap(const val: Boolean);
begin
  if val <> fCubeMap then
  begin
    fCubeMap := val;
    FBlank := true;
  end;
end;

// SetArray
//

procedure TGLBitmap32.SetArray(const val: Boolean);
begin
  if val <> fTextureArray then
  begin
    fTextureArray := val;
    FBlank := true;
  end;
end;

// SetInternalFormat
//

procedure TGLBitmap32.SetInternalFormat(const val: TGLInternalFormat);
begin
  Assert(fBlank);
  fInternalFormat := val;
  fElementSize := GetTextureElementSize(val);
end;

// SetFormat
//

procedure TGLBitmap32.SetColorFormat(const val: GLenum);
begin
  Assert(fBlank);
  fColorFormat := val;
  fElementSize := GetTextureElementSize(fColorFormat, fDataType);
end;

// SetDataType
//

procedure TGLBitmap32.SetDataType(const val: GLenum);
begin
  Assert(fBlank);
  fDataType := val;
  fElementSize := GetTextureElementSize(fColorFormat, fDataType);
end;

// GetScanLine
//

function TGLBitmap32.GetScanLine(index: Integer): PGLPixel32Array;
begin
  Narrow;
  Result := PGLPixel32Array(@FData[index * Width]);
end;

// SetAlphaFromIntensity
//

procedure TGLBitmap32.SetAlphaFromIntensity;
var
  i: Integer;
begin
  Narrow;
  for i := 0 to (DataSize div 4) - 1 do
    with FData^[i] do
      a := (Integer(r) + Integer(g) + Integer(b)) div 3;
end;

// SetAlphaTransparentForColor
//

procedure TGLBitmap32.SetAlphaTransparentForColor(const aColor: TColor);
var
  color: TGLPixel24;
begin
  color.r := GetRValue(aColor);
  color.g := GetGValue(aColor);
  color.b := GetBValue(aColor);
  SetAlphaTransparentForColor(color);
end;

// SetAlphaTransparentForColor
//

procedure TGLBitmap32.SetAlphaTransparentForColor(const aColor: TGLPixel32);
var
  color: TGLPixel24;
begin
  color.r := aColor.r;
  color.g := aColor.g;
  color.b := aColor.b;
  SetAlphaTransparentForColor(color);
end;

// SetAlphaTransparentForColor
//

procedure TGLBitmap32.SetAlphaTransparentForColor(const aColor: TGLPixel24);
var
  i: Integer;
  intCol: Integer;
begin
  Narrow;
  intCol := (PInteger(@aColor)^) and $FFFFFF;
  for i := 0 to (DataSize div 4) - 1 do
    if PInteger(@FData[i])^ and $FFFFFF = intCol then
      FData^[i].a := 0
    else
      FData^[i].a := 255;
end;

// SetAlphaToValue
//

procedure TGLBitmap32.SetAlphaToValue(const aValue: Byte);
var
  i: Integer;
begin
  Narrow;
  for i := 0 to (DataSize div 4) - 1 do
    FData^[i].a := aValue
end;

// SetAlphaToFloatValue
//

procedure TGLBitmap32.SetAlphaToFloatValue(const aValue: Single);
begin
  SetAlphaToValue(Byte(Trunc(aValue * 255) and 255));
end;

// InvertAlpha
//

procedure TGLBitmap32.InvertAlpha;
var
  i: Integer;
begin
  for i := 0 to (DataSize div 4) - 1 do
    FData^[i].a := 255 - FData^[i].a;
end;

// SqrtAlpha
//

procedure TGLBitmap32.SqrtAlpha;
var
  i: Integer;
  sqrt255Array: PSqrt255Array;
begin
  sqrt255Array := GetSqrt255Array;
  for i := 0 to (DataSize div 4) - 1 do
    with FData^[i] do
      a := sqrt255Array^[(Integer(r) + Integer(g) + Integer(b)) div 3];
end;

// BrightnessCorrection
//

procedure TGLBitmap32.BrightnessCorrection(const factor: Single);
begin
  if Assigned(FData) then
  begin
    Narrow;
    BrightenRGBAArray(Data, DataSize div 4, factor);
  end;
end;

// GammaCorrection
//

procedure TGLBitmap32.GammaCorrection(const gamma: Single);
begin
  if Assigned(FData) then
  begin
    Narrow;
    GammaCorrectRGBAArray(Data, DataSize div 4, gamma);
  end;
end;

// DownSampleByFactor2
//

procedure TGLBitmap32.DownSampleByFactor2;
type
  T2Pixel32 = packed array[0..1] of TGLPixel32;
  P2Pixel32 = ^T2Pixel32;

{$ifNdef NO_ASM}
  procedure ProcessRow3DNow(pDest: PGLPixel32; pLineA, pLineB: P2Pixel32; n: Integer);
  asm     // 3DNow! version 30% faster
      db $0F,$EF,$C0           /// pxor        mm0, mm0          // set mm0 to [0, 0, 0, 0]

@@Loop:
      db $0F,$0D,$81,$00,$01,$00,$00/// prefetch    [ecx+256]

      db $0F,$6F,$0A           /// movq        mm1, [edx]
      db $0F,$6F,$11           /// movq        mm2, [ecx]

      db $0F,$6F,$D9           /// movq        mm3, mm1
      db $0F,$6F,$E2           /// movq        mm4, mm2

      db $0F,$60,$C8           /// punpcklbw   mm1, mm0          // promote to 16 bits and add LineA pixels
      db $0F,$68,$D8           /// punpckhbw   mm3, mm0
      db $0F,$FD,$CB           /// paddw       mm1, mm3

      db $0F,$60,$D0           /// punpcklbw   mm2, mm0          // promote to 16 bits and add LineB pixels
      db $0F,$68,$E0           /// punpckhbw   mm4, mm0
      db $0F,$FD,$D4           /// paddw       mm2, mm4

      db $0F,$FD,$CA           /// paddw       mm1, mm2          // add LineA and LineB pixels

      db $0F,$71,$D1,$02       /// psrlw       mm1, 2            // divide by 4
      db $0F,$67,$C9           /// packuswb    mm1, mm1          // reduce to 8 bits and store point
      db $0F,$7E,$08           /// movd        [eax], mm1

      add         edx, 8
      add         ecx, 8
      add         eax, 4

      dec         [n]
      jnz         @@Loop

      db $0F,$0E               /// femms
  end;
{$ENDIF}

  procedure ProcessRowPascal(pDest: PGLPixel32; pLineA, pLineB: P2Pixel32; n: Integer);
  var
    i: Integer;
  begin
    for i := 0 to n - 1 do
    begin
       pDest^.r:=(pLineA^[0].r+pLineA^[1].r+pLineB^[0].r+pLineB^[1].r) shr 2;
       pDest^.g:=(pLineA^[0].g+pLineA^[1].g+pLineB^[0].g+pLineB^[1].g) shr 2;
       pDest^.b:=(pLineA^[0].b+pLineA^[1].b+pLineB^[0].b+pLineB^[1].b) shr 2;
       pDest^.a:=(pLineA^[0].a+pLineA^[1].a+pLineB^[0].a+pLineB^[1].a) shr 2;
      Inc(pLineA);
      Inc(pLineB);
      Inc(pDest);
    end;
  end; // }

var
  y, w2, h2: Integer;
  pDest: PGLPixel32;
  pLineA, pLineB: P2Pixel32;
begin
  if (FWidth <= 1) or (FHeight <= 1) then
    Exit;
  Narrow;
  w2 := FWidth shr 1;
  h2 := FHeight shr 1;
  pDest := @FData[0];
  pLineA := @FData[0];
  pLineB := @FData[Width];
{$ifNdef NO_ASM}
  if vSIMD = 1 then
  begin
    for y := 0 to h2 - 1 do
    begin
      ProcessRow3DNow(pDest, pLineA, pLineB, w2);
      Inc(pDest, w2);
      Inc(pLineA, Width);
      Inc(pLineB, Width);
    end;
  end
  else
{$ENDIF}
  begin
    for y := 0 to h2 - 1 do
    begin
      ProcessRowPascal(pDest, pLineA, pLineB, w2);
      Inc(pDest, w2);
      Inc(pLineA, Width);
      Inc(pLineB, Width);
    end;
  end;
  FWidth := w2;
  FHeight := h2;
  ReallocMem(FData, DataSize);
end;

// RegisterAsOpenGLTexture
//

procedure TGLBitmap32.RegisterAsOpenGLTexture(target: TGLUInt;
  minFilter: TGLMinFilter;
  texFormat: TGLEnum);
var
  tw, th, td: Integer;
begin
  RegisterAsOpenGLTexture(target, minFilter, texFormat, tw, th, td);
end;

// RegisterAsOpenGLTexture
//

procedure TGLBitmap32.RegisterAsOpenGLTexture(target: TGLUInt;
  minFilter: TGLMinFilter;
  texFormat: TGLEnum;
  out texWidth: integer;
  out texHeight: integer;
  out texDepth: integer);
var
  Level: integer;
  ml, face: integer;
  bCompress, bMipmapGen: boolean;
  w, h, d, cw, ch, maxSize: GLsizei;
  p, buffer: Pointer;
  vtcBuffer, top, bottom: PGLubyte;
  i, j, k: Integer;

  function blockOffset(x, y, z: Integer): Integer;
  begin

    if z >= (d and -4) then
      Result := fElementSize * (cw * ch * (d and -4) + x +
        cw * (y + ch * (z - 4 * ch)))
    else
      Result := fElementSize * (4 * (x + cw * (y + ch * floor(z / 4))) + (z and
        3));
    if Result < 0 then
      Result := 0;
  end;

begin
  // Check for Non-power-of-two
  if not GL_ARB_texture_non_power_of_two then
  begin
    w := RoundUpToPowerOf2(Width);
    h := RoundUpToPowerOf2(Height);
    d := RoundUpToPowerOf2(Depth);
    if Depth = 0 then
      d := 0;
  end
  else
  begin
    w := Width;
    h := Height;
    d := Depth;
  end;

  // Check maximum dimension
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @maxSize);
  if w > maxSize then
    w := maxSize;
  if h > maxSize then
    h := maxSize;
  texWidth := w;
  texHeight := h;
  texDepth := d;
  ml := MipLevels;
  bCompress := IsCompressed;

  // Rescale if need and can
  buffer := nil;
  if (w <> Width) or (h <> Height) then
  begin
    if not ((d > 0) // not volume
      or bCompress // not compressed
      or (ml > 1) // without LOD
      or fBlank) then // not blank
    begin
      GetMem(buffer, w * h * fElementSize);
      if gluScaleImage(ColorFormat, Width, Height, DataType, GetLevelData(0), w,
        h,
        DataType, buffer) <> 0 then
        fBlank := true;
    end
    else
      fBlank := true;
  end;

  // Hardware mipmap autogeneration
  if GL_SGIS_generate_mipmap and (target <> GL_TEXTURE_RECTANGLE) then
  begin
    bMipmapGen := (ml = 1) and not (minFilter in [miNearest, miLinear]);
    if (target >= GL_TEXTURE_CUBE_MAP_POSITIVE_X)
      and (target <= GL_TEXTURE_CUBE_MAP_NEGATIVE_Z) then
    begin
      if not CurrentGLContext.GLStates.ForwardContext then
        glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_GENERATE_MIPMAP_SGIS,
          Integer(bMipmapGen));
      if ml>1 then
        glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAX_LEVEL_SGIS, ml);
    end
    else begin
      if not CurrentGLContext.GLStates.ForwardContext then
        glTexParameteri(target, GL_GENERATE_MIPMAP_SGIS, Integer(bMipmapGen));
      if ml>1 then
        glTexParameteri(target, GL_TEXTURE_MAX_LEVEL_SGIS, ml-1);
    end;
  end;

  // if image is blank then doing only allocatation texture in videomemory
  p := nil;
  vtcBuffer := nil;
  case target of

    GL_TEXTURE_1D:
      for Level := 0 to ml - 1 do
      begin
        if not fBlank then
          if Assigned(buffer) then
            p := buffer
          else
            p := GetLevelData(Level);
        if bCompress then
          glCompressedTexImage1DARB(target, Level, texFormat, w, 0,
            LevelSize(Level), p)
        else
          glTexImage1D(target, Level, texFormat, w, 0, ColorFormat, DataType,
            p);
        if w > 1 then
          w := w div 2
        else
          w := 1;
      end;

    GL_TEXTURE_2D:
      for Level := 0 to ml - 1 do
      begin
        if not fBlank then
          if Assigned(buffer) then
            p := buffer
          else
            p := GetLevelData(Level);
        if bCompress then
          glCompressedTexImage2DARB(target, Level, texFormat, w, h, 0,
            LevelSize(Level), p)
        else
          glTexImage2D(target, Level, texFormat, w, h, 0, ColorFormat, DataType,
            p);
        if w > 1 then
          w := w div 2
        else
          w := 1;
        if h > 1 then
          h := h div 2
        else
          h := 1;
      end;

    GL_TEXTURE_RECTANGLE:
      begin
        if not fBlank then
          if Assigned(buffer) then
            p := buffer
          else
            p := GetLevelData(0);
        if bCompress then
          glCompressedTexImage2DARB(target, 0, texFormat, w, h, 0, LevelSize(0),
            p)
        else
          glTexImage2D(target, 0, texFormat, w, h, 0, ColorFormat, DataType, p);
      end;

    GL_TEXTURE_3D:
      for Level := 0 to ml - 1 do
      begin
        if not fBlank then
          if Assigned(buffer) then
            p := buffer
          else
            p := GetLevelData(Level);

        if GL_NV_texture_compression_vtc and bCompress then
        begin
          // Shufle blocks for Volume Texture Compression
          if Assigned(p) then
          begin
            cw := (w + 3) div 4;
            ch := (h + 3) div 4;
            if Level = 0 then
              GetMem(vtcBuffer, LevelSize(0));
            top := p;
            for k := 0 to d - 1 do
              for i := 0 to ch - 1 do
                for j := 0 to cw - 1 do
                begin
                  bottom := vtcBuffer;
                  Inc(bottom, blockOffset(j, i, k));
                  Move(top^, bottom^, fElementSize);
                  Inc(top, fElementSize);
                end;
          end;
          glCompressedTexImage3DARB(target, Level, texFormat, w, h, d, 0,
            LevelSize(Level), vtcBuffer);
        end
        else
        begin
          // Normal compression
          if bCompress then
            glCompressedTexImage3DARB(target, Level, texFormat, w, h, d, 0,
              LevelSize(Level), p)
          else
            glTexImage3D(target, Level, texFormat, w, h, d, 0, ColorFormat,
              DataType, p);
        end;
        if w > 1 then
          w := w div 2
        else
          w := 1;
        if h > 1 then
          h := h div 2
        else
          h := 1;
        if d > 1 then
          d := d div 2
        else
          d := 1;
      end;

    GL_TEXTURE_CUBE_MAP:
      for Level := 0 to ml - 1 do
      begin
        for face := GL_TEXTURE_CUBE_MAP_POSITIVE_X to
          GL_TEXTURE_CUBE_MAP_NEGATIVE_Z do
        begin
          if not fBlank then
            if Assigned(buffer) then
              p := buffer
            else
              p := GetLevelData(Level, face);
          if bCompress then
            glCompressedTexImage2DARB(face, Level, texFormat, w, h, 0,
              LevelSize(Level), p)
          else
            glTexImage2D(face, Level, texFormat, w, h, 0, ColorFormat, DataType,
              p);
        end;
        if w > 1 then
          w := w div 2
        else
          w := 1;
        if h > 1 then
          h := h div 2
        else
          h := 1;
      end;

    GL_TEXTURE_CUBE_MAP_POSITIVE_X,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
      GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
      GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_Z:
      for Level := 0 to ml - 1 do
      begin
        if not fBlank then
          if Assigned(buffer) then
            p := buffer
          else
            p := GetLevelData(Level, target);
        if bCompress then
          glCompressedTexImage2DARB(target, Level, texFormat, w, h, 0,
            LevelSize(Level), p)
        else
          glTexImage2D(target, Level, texFormat, w, h, 0, ColorFormat, DataType,
            p);
        if w > 1 then
          w := w div 2
        else
          w := 1;
        if h > 1 then
          h := h div 2
        else
          h := 1;
      end;

    GL_TEXTURE_1D_ARRAY:
      for Level := 0 to ml - 1 do
      begin
        if not fBlank then
          if Assigned(buffer) then
            p := buffer
          else
            p := GetLevelData(Level);
        if bCompress then
          glCompressedTexImage2DARB(target, Level, texFormat, w, h, 0,
            LevelSize(Level), p)
        else
          glTexImage2D(target, Level, texFormat, w, h, 0, ColorFormat, DataType,
            p);
        if w > 1 then
          w := w div 2
        else
          w := 1;
      end;

    GL_TEXTURE_2D_ARRAY:
      for Level := 0 to ml - 1 do
      begin
        if not fBlank then
          if Assigned(buffer) then
            p := buffer
          else
            p := GetLevelData(Level);
        if bCompress then
          glCompressedTexImage3DARB(target, Level, texFormat, w, h, d, 0,
            LevelSize(Level), p)
        else
          glTexImage3D(target, Level, texFormat, w, h, d, 0, ColorFormat,
            DataType, p);
        if w > 1 then
          w := w div 2
        else
          w := 1;
        if h > 1 then
          h := h div 2
        else
          h := 1;
      end;
    GL_TEXTURE_CUBE_MAP_ARRAY: ;

  end; // of case
  if Assigned(buffer) then
    FreeMem(buffer);
  if Assigned(vtcBuffer) then
    FreeMem(vtcBuffer);
end;

// ReadPixels
//

procedure TGLBitmap32.ReadPixels(const area: TGLRect);
begin
  FWidth := (area.Right - area.Left) and $FFFC;
  FHeight := (area.Bottom - area.Top);
  FDepth := 0;
  fMipLevels := 1;
  fColorFormat := GL_RGBA;
  fInternalFormat := tfRGBA8;
  fDataType := GL_UNSIGNED_BYTE;
  fElementSize := 4;
  fLevels.Clear;
  fCubeMap := false;
  fTextureArray := false;
  fBlank := false;
  ReallocMem(FData, DataSize);
  glReadPixels(0, 0, FWidth, FHeight, GL_RGBA, GL_UNSIGNED_BYTE, FData);
end;

// DrawPixels
//

procedure TGLBitmap32.DrawPixels(const x, y: Single);
begin
  if fBlank or IsEmpty then
    Exit;
  Assert(not CurrentGLContext.GLStates.ForwardContext);
  glRasterPos2f(x, y);
  glDrawPixels(Width, Height, fColorFormat, fDataType, FData);
end;

// TGLBitmap32
//

procedure TGLBitmap32.GrayScaleToNormalMap(const scale: Single;
  wrapX: Boolean = True; wrapY: Boolean = True);
var
  x, y: Integer;
  dcx, dcy: Single;
  invLen: Single;
  maskX, maskY: Integer;
  curRow, nextRow, prevRow: PGLPixel32Array;
  normalMapBuffer: PGLPixel32Array;
  p: PGLPixel32;
begin
  if Assigned(FData) then
  begin
    Narrow;
    normalMapBuffer := AllocMem(DataSize);
    try
      maskX := Width - 1;
      maskY := Height - 1;
      p := @normalMapBuffer[0];
      for y := 0 to Height - 1 do
      begin
        curRow := GetScanLine(y);
        if wrapY then
        begin
          prevRow := GetScanLine((y - 1) and maskY);
          nextRow := GetScanLine((y + 1) and maskY);
        end
        else
        begin
          if y > 0 then
            prevRow := GetScanLine(y - 1)
          else
            prevRow := curRow;
          if y < Height - 1 then
            nextRow := GetScanLine(y + 1)
          else
            nextRow := curRow;
        end;
        for x := 0 to Width - 1 do
        begin
          if wrapX then
            dcx := scale * (curRow^[(x - 1) and maskX].g - curRow^[(x + 1) and
              maskX].g)
          else
          begin
            if x = 0 then
              dcx := scale * (curRow^[x].g - curRow^[x + 1].g)
            else if x < Width - 1 then
              dcx := scale * (curRow^[x - 1].g - curRow^[x].g)
            else
              dcx := scale * (curRow^[x - 1].g - curRow^[x + 1].g);
          end;
          dcy := scale * (prevRow^[x].g - nextRow^[x].g);
          invLen := 127 * RSqrt(dcx * dcx + dcy * dcy + 1);
          with p^ do
          begin
            r := Integer(Round(128 + ClampValue(dcx * invLen, -128, 127)));
            g := Integer(Round(128 + ClampValue(dcy * invLen, -128, 127)));
            b := Integer(Round(128 + ClampValue(invLen, -128, 127)));
            a := 255;
          end;
          Inc(p);
        end;
      end;
      Move(normalMapBuffer^, FData^, DataSize);
    finally
      FreeMem(normalMapBuffer);
    end;
  end;
end;

// NormalizeNormalMap
//

procedure TGLBitmap32.NormalizeNormalMap;
var
  x, y: Integer;
  sr, sg, sb: Single;
  invLen: Single;
  curRow: PGLPixel32Array;
  p: PGLPixel32;
const
  cInv128: Single = 1 / 128;
begin
  if Assigned(FData) then
  begin
    Narrow;
    for y := 0 to Height - 1 do
    begin
      curRow := @FData[y * Width];
      for x := 0 to Width - 1 do
      begin
        p := @curRow[x];
        sr := (p^.r - 128) * cInv128;
        sg := (p^.g - 128) * cInv128;
        sb := (p^.b - 128) * cInv128;
        invLen := RSqrt(sr * sr + sg * sg + sb * sb);
        p^.r := Round(128 + 127 * ClampValue(sr * invLen, -1, 1));
        p^.g := Round(128 + 127 * ClampValue(sg * invLen, -1, 1));
        p^.b := Round(128 + 127 * ClampValue(sb * invLen, -1, 1));
      end;
    end;
  end;
end;

procedure TGLBitmap32.SetBlank(const Value: boolean);
begin
  if FBlank and not IsEmpty then
    ReallocMem(FData, DataSize);
  FBlank := Value;
end;

//Converts a TGLBitmap32 back into a TBitmap
//

procedure TGLBitmap32.AssignToBitmap(aBitmap: TGLBitmap); //TGLBitmap = TBitmap
var
  y: integer;
  pSrc, pDest: PAnsiChar;
{$IFDEF FPC}
  RIMG: TRawImage;
{$ENDIF}

begin
  Narrow;
  aBitmap.Width := FWidth;
  aBitmap.Height := FHeight;
  aBitmap.PixelFormat := glpf32bit;
  if Height > 0 then
  begin
{$IFDEF FPC}
    RIMG.Init;
    rimg.Description.Init_BPP32_B8G8R8A8_BIO_TTB(FWidth, FHeight);
    rimg.Description.RedShift := 0;
    rimg.Description.BlueShift := 16;
    rimg.Description.LineOrder := riloBottomToTop;
    RIMG.DataSize := DataSize;
    rimg.Data := PByte(FData);
    aBitmap.LoadFromRawImage(rimg, false);
{$ELSE}
    for y := 0 to FHeight - 1 do
    begin
      pSrc := @PAnsiChar(FData)[y * (FWidth * 4)];
      pDest := BitmapScanLine(aBitmap, FHeight - 1 - y);
      BGRA32ToRGBA32(pSrc, pDest, FWidth);
    end;
{$ENDIF}
  end;
end;

// UnMipmap
//

procedure TGLBitmap32.UnMipmap;
begin
  inherited UnMipmap;
  if not (fBlank or IsEmpty) then
    ReallocMem(FData, DataSize);
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

initialization

finalization
  FreeAndNil(vRasterFileFormats);

end.

