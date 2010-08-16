//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLTextureFormat<p>

 <b>History : </b><font size=-1><ul>
        <li>23/01/10 - Yar - Separated GLTextureFormat and GLInternalFormat
                             GLTextureFormat moved to GLTexture
        <li>21/01/10 - Yar - Creation
   </ul><p>
}
unit GLTextureFormat;

interface

uses
  OpenGL1x;

type

  // TGLInternalFormat
  //
  TGLInternalFormat = (
    tfALPHA4,
    tfALPHA8,
    tfALPHA12,
    tfALPHA16,
    tfDEPTH_COMPONENT16,
    tfDEPTH_COMPONENT24,
    tfDEPTH_COMPONENT32,
    tfLUMINANCE4,
    tfLUMINANCE8,
    tfLUMINANCE12,
    tfLUMINANCE16,
    tfLUMINANCE4_ALPHA4,
    tfLUMINANCE6_ALPHA2,
    tfLUMINANCE8_ALPHA8,
    tfLUMINANCE12_ALPHA4,
    tfLUMINANCE12_ALPHA12,
    tfLUMINANCE16_ALPHA16,
    tfINTENSITY4,
    tfINTENSITY8,
    tfINTENSITY12,
    tfINTENSITY16,
    tfR3_G3_B2,
    tfRGB4,
    tfRGB5,
    tfRGB8,
    tfRGB10,
    tfRGB12,
    tfR16G16B16,
    tfRGBA2,
    tfRGBA4,
    tfRGB5_A1,
    tfRGBA8,
    tfRGB10_A2,
    tfRGBA12,
    tfR16G16B16A16,
    tfCOMPRESSED_RGB_S3TC_DXT1,
    tfCOMPRESSED_RGBA_S3TC_DXT1,
    tfCOMPRESSED_RGBA_S3TC_DXT3,
    tfCOMPRESSED_RGBA_S3TC_DXT5,
    tfSIGNED_LUMINANCE8,
    tfSIGNED_LUMINANCE8_ALPHA8,
    tfSIGNED_RGB8,
    tfSIGNED_RGBA8,
    tfSIGNED_RGB8_UNSIGNED_ALPHA8,
    tfSIGNED_ALPHA8,
    tfSIGNED_INTENSITY8,
    tfHILO16,
    tfSIGNED_HILO16,
    tfDSDT8,
    tfDSDT8_MAG8,
    tfDSDT8_MAG8_INTENSITY8,
    tfHILO8,
    tfSIGNED_HILO8,
    tfFLOAT_R16,
    tfFLOAT_R32,
    tfFLOAT_RG16,
    tfFLOAT_RGB16,
    tfFLOAT_RGBA16,
    tfFLOAT_RG32,
    tfFLOAT_RGB32,
    tfFLOAT_RGBA32,
    tfRGBA_FLOAT32,
    tfRGB_FLOAT32,
    tfALPHA_FLOAT32,
    tfINTENSITY_FLOAT32,
    tfLUMINANCE_FLOAT32,
    tfLUMINANCE_ALPHA_FLOAT32,
    tfRGBA_FLOAT16,
    tfRGB_FLOAT16,
    tfALPHA_FLOAT16,
    tfINTENSITY_FLOAT16,
    tfLUMINANCE_FLOAT16,
    tfLUMINANCE_ALPHA_FLOAT16,
    tfDEPTH24_STENCIL8,
    tfDEPTH_COMPONENT32F,
    tfDEPTH32F_STENCIL8,
    tfSRGB8,
    tfSRGB8_ALPHA8,
    tfSLUMINANCE8,
    tfSLUMINANCE8_ALPHA8,
    tfCOMPRESSED_SRGB_S3TC_DXT1,
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT1,
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT3,
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5,
    tfRGB9_E5,
    tfR11F_G11F_B10F,
    tfCOMPRESSED_LUMINANCE_LATC1,
    tfCOMPRESSED_SIGNED_LUMINANCE_LATC1,
    tfCOMPRESSED_LUMINANCE_ALPHA_LATC2,
    tfCOMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2,
    tfCOMPRESSED_LUMINANCE_ALPHA_3DC,
    tfRGBA32UI,
    tfRGB32UI,
    tfALPHA32UI,
    tfINTENSITY32UI,
    tfLUMINANCE32UI,
    tfLUMINANCE_ALPHA32UI,
    tfRGBA16UI,
    tfRGB16UI,
    tfALPHA16UI,
    tfINTENSITY16UI,
    tfLUMINANCE16UI,
    tfLUMINANCE_ALPHA16UI,
    tfRGBA8UI,
    tfRGB8UI,
    tfALPHA8UI,
    tfINTENSITY8UI,
    tfLUMINANCE8UI,
    tfLUMINANCE_ALPHA8UI,
    tfRGBA32I,
    tfRGB32I,
    tfALPHA32I,
    tfINTENSITY32I,
    tfLUMINANCE32I,
    tfLUMINANCE_ALPHA32I,
    tfRGBA16I,
    tfRGB16I,
    tfALPHA16I,
    tfINTENSITY16I,
    tfLUMINANCE16I,
    tfLUMINANCE_ALPHA16I,
    tfRGBA8I,
    tfRGB8I,
    tfALPHA8I,
    tfINTENSITY8I,
    tfLUMINANCE8I,
    tfLUMINANCE_ALPHA8I,
    tfRG32UI,
    tfR32UI,
    tfRG16UI,
    tfR16UI,
    tfRG8UI,
    tfR8UI,
    tfRG32I,
    tfR32I,
    tfRG16I,
    tfR16I,
    tfRG8I,
    tfR8I,
    tfRG8,
    tfR8,
    tfRG16,
    tfR16,
    tfRG16F,
    tfR16F,
    tfRG32F,
    tfR32F,
    tfCOMPRESSED_RED_RGTC1,
    tfCOMPRESSED_SIGNED_RED_RGTC1,
    tfCOMPRESSED_RG_RGTC2,
    tfCOMPRESSED_SIGNED_RG_RGTC2);

  // TGLInternalCompression
  //
  {: Texture compression option.<p>
     If OpenGL supports it, this will activate a compressed texture format:<ul>
     <li>tcDefault : uses global default compression option
     <li>tcNone : do not use compression
     <li>tcStandard : use standard compression, average quality, average rate
     <li>tcHighQuality : choose a high-quality, low-speed compression
     <li>tcHighSpeed : choose a high-speed, low-quality compression
     </ul>. }
  TGLInternalCompression = (tcDefault, tcNone, tcStandard, tcHighQuality,
    tcHighSpeed);

  // Global texturing defaults
  //
var
  vDefaultTextureFormat: TGLInternalFormat = tfRGBA8;
  vDefaultTextureCompression: TGLInternalCompression = tcNone;

// Give a openGL texture format from GLScene texture format
function InternalFormatToOpenGLFormat(texFormat: TGLInternalFormat): Integer;
// Give a GLScene texture format from openGL texture format
function OpenGLFormatToInternalFormat(intFormat: Integer): TGLInternalFormat;
// Give a pixel size in bytes from texture format or data format
function GetTextureElementSize(texFormat: TGLInternalFormat): Integer; overload;
function GetTextureElementSize(colorFormat: TGLEnum; dataType: TGLEnum):
  Integer; overload;
// Give compatible openGL image format and data type
procedure FindCompatibleDataFormat(texFormat: TGLInternalFormat; out dFormat:
  GLenum; out dType: GLenum);
// Give a compressed openGL texture format from GLScene texture format
// if format is have not compression than return same openGL format
function CompressedInternalFormatToOpenGL(texFormat: TGLInternalFormat):
  Integer;
// True if texture target supported
function IsTargetSupported(target: TGLEnum): Boolean;
// True if texture format is supported by hardware or software
function IsFormatSupported(texFormat: TGLInternalFormat): Boolean;
// True if texture format is float
function IsFloatFormat(texFormat: TGLInternalFormat): Boolean; overload;
function IsFloatFormat(intFormat: TGLEnum): Boolean; overload;
// True if depth texture
function IsDepthFormat(texFormat: TGLInternalFormat): boolean; overload;
function IsDepthFormat(intFormat: TGLEnum): Boolean; overload;
// True if texture compressed
function IsCompressedFormat(texFormat: TGLInternalFormat): Boolean; overload;
function IsCompressedFormat(intFormat: TGLEnum): Boolean; overload;
// Give generic compressed OpenGL texture format
function GetGenericCompressedFormat(const texFormat: TGLInternalFormat;
  const colorFormat: TGLEnum; out internalFormat: TGLEnum): Boolean;
// Give uncompressed texture format and OpenGL color format
function GetUncompressedFormat(const texFormat: TGLInternalFormat;
  out internalFormat: TGLInternalFormat; out colorFormat: TGLEnum): Boolean;

implementation

uses
  GLStrings;

const
  //: InternalFormat, ColorFormat, DataType
  cTextureFormatToOpenGL: array[low(TGLInternalFormat)..high(TGLInternalFormat), 0..2] of Integer
    = (
    (GL_ALPHA4, GL_ALPHA, GL_UNSIGNED_BYTE),
    (GL_ALPHA8, GL_ALPHA, GL_UNSIGNED_BYTE),
    (GL_ALPHA12, GL_ALPHA, GL_UNSIGNED_SHORT),
    (GL_ALPHA16, GL_ALPHA, GL_UNSIGNED_SHORT),
    (GL_DEPTH_COMPONENT16, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE),
    (GL_DEPTH_COMPONENT24, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE),
    (GL_DEPTH_COMPONENT32, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE),
    (GL_LUMINANCE4, GL_LUMINANCE, GL_UNSIGNED_BYTE),
    (GL_LUMINANCE8, GL_LUMINANCE, GL_UNSIGNED_BYTE),
    (GL_LUMINANCE12, GL_LUMINANCE, GL_UNSIGNED_SHORT),
    (GL_LUMINANCE16, GL_LUMINANCE, GL_UNSIGNED_SHORT),
    (GL_LUMINANCE4_ALPHA4, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE),
    (GL_LUMINANCE6_ALPHA2, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE),
    (GL_LUMINANCE8_ALPHA8, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE),
    (GL_LUMINANCE12_ALPHA4, GL_LUMINANCE_ALPHA, GL_UNSIGNED_SHORT),
    (GL_LUMINANCE12_ALPHA12, GL_LUMINANCE_ALPHA, GL_UNSIGNED_SHORT),
    (GL_LUMINANCE16_ALPHA16, GL_LUMINANCE_ALPHA, GL_UNSIGNED_SHORT),
    (GL_INTENSITY4, GL_LUMINANCE, GL_UNSIGNED_BYTE),
    (GL_INTENSITY8, GL_LUMINANCE, GL_UNSIGNED_BYTE),
    (GL_INTENSITY12, GL_LUMINANCE, GL_UNSIGNED_SHORT),
    (GL_INTENSITY16, GL_LUMINANCE, GL_UNSIGNED_SHORT),
    (GL_R3_G3_B2, GL_RGB, GL_UNSIGNED_BYTE_3_3_2),
    (GL_RGB4, GL_RGB, GL_UNSIGNED_BYTE),
    (GL_RGB5, GL_RGB, GL_UNSIGNED_SHORT_5_6_5),
    (GL_RGB8, GL_RGB, GL_UNSIGNED_BYTE),
    (GL_RGB10, GL_RGBA, GL_UNSIGNED_INT_10_10_10_2),
    (GL_RGB12, GL_RGB, GL_UNSIGNED_BYTE),
    (GL_RGB16, GL_RGB, GL_UNSIGNED_SHORT),
    (GL_RGBA2, GL_RGBA, GL_UNSIGNED_BYTE),
    (GL_RGBA4, GL_RGBA, GL_UNSIGNED_SHORT_4_4_4_4),
    (GL_RGB5_A1, GL_RGBA, GL_UNSIGNED_SHORT_5_5_5_1),
    (GL_RGBA8, GL_RGBA, GL_UNSIGNED_BYTE),
    (GL_RGB10_A2, GL_RGBA, GL_UNSIGNED_INT_10_10_10_2),
    (GL_RGBA12, GL_RGBA, GL_UNSIGNED_BYTE),
    (GL_RGBA16, GL_RGBA, GL_UNSIGNED_SHORT),
    (GL_COMPRESSED_RGB_S3TC_DXT1_EXT, GL_COMPRESSED_RGB_S3TC_DXT1_EXT,
      GL_COMPRESSED_RGB_S3TC_DXT1_EXT),
    (GL_COMPRESSED_RGBA_S3TC_DXT1_EXT, GL_COMPRESSED_RGBA_S3TC_DXT1_EXT,
      GL_COMPRESSED_RGBA_S3TC_DXT1_EXT),
    (GL_COMPRESSED_RGBA_S3TC_DXT3_EXT, GL_COMPRESSED_RGBA_S3TC_DXT3_EXT,
      GL_COMPRESSED_RGBA_S3TC_DXT3_EXT),
    (GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, GL_COMPRESSED_RGBA_S3TC_DXT5_EXT,
      GL_COMPRESSED_RGBA_S3TC_DXT5_EXT),
    (GL_SIGNED_LUMINANCE8_NV, GL_LUMINANCE, GL_BYTE),
    (GL_SIGNED_LUMINANCE8_ALPHA8_NV, GL_LUMINANCE_ALPHA, GL_SHORT),
    (GL_SIGNED_RGB8_NV, GL_RGB, GL_BYTE),
    (GL_SIGNED_RGBA8_NV, GL_RGBA, GL_BYTE),
    (GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV, GL_RGBA, GL_BYTE),
    (GL_SIGNED_ALPHA8_NV, GL_ALPHA, GL_BYTE),
    (GL_SIGNED_INTENSITY8_NV, GL_INTENSITY, GL_BYTE),
    (GL_HILO16_NV, GL_RG, GL_UNSIGNED_SHORT),
    (GL_SIGNED_HILO16_NV, GL_RG, GL_SHORT),
    (GL_DSDT8_NV, GL_RED, GL_UNSIGNED_BYTE),
    (GL_DSDT8_MAG8_NV, GL_RG, GL_UNSIGNED_BYTE),
    (GL_DSDT8_MAG8_INTENSITY8_NV, GL_RGB, GL_UNSIGNED_BYTE),
    (GL_HILO8_NV, GL_RG, GL_UNSIGNED_BYTE),
    (GL_SIGNED_HILO8_NV, GL_RG, GL_BYTE),
    (GL_FLOAT_R16_NV, GL_RED, GL_HALF_FLOAT),
    (GL_FLOAT_R32_NV, GL_RED, GL_FLOAT),
    (GL_FLOAT_RG16_NV, GL_RG, GL_HALF_FLOAT),
    (GL_FLOAT_RGB16_NV, GL_RGB, GL_HALF_FLOAT),
    (GL_FLOAT_RGBA16_NV, GL_RGBA, GL_HALF_FLOAT),
    (GL_FLOAT_RG32_NV, GL_RG, GL_FLOAT),
    (GL_FLOAT_RGB32_NV, GL_RGB, GL_FLOAT),
    (GL_FLOAT_RGBA32_NV, GL_RGBA, GL_FLOAT),
    (GL_RGBA_FLOAT32_ATI, GL_RGBA, GL_FLOAT),
    (GL_RGB_FLOAT32_ATI, GL_RGB, GL_FLOAT),
    (GL_ALPHA_FLOAT32_ATI, GL_ALPHA, GL_FLOAT),
    (GL_INTENSITY_FLOAT32_ATI, GL_LUMINANCE, GL_FLOAT),
    (GL_LUMINANCE_FLOAT32_ATI, GL_LUMINANCE, GL_FLOAT),
    (GL_LUMINANCE_ALPHA_FLOAT32_ATI, GL_LUMINANCE_ALPHA, GL_FLOAT),
    (GL_RGBA_FLOAT16_ATI, GL_RGBA, GL_HALF_FLOAT),
    (GL_RGB_FLOAT16_ATI, GL_RGB, GL_HALF_FLOAT),
    (GL_ALPHA_FLOAT16_ATI, GL_ALPHA, GL_HALF_FLOAT),
    (GL_INTENSITY_FLOAT16_ATI, GL_LUMINANCE, GL_HALF_FLOAT),
    (GL_LUMINANCE_FLOAT16_ATI, GL_LUMINANCE, GL_HALF_FLOAT),
    (GL_LUMINANCE_ALPHA_FLOAT16_ATI, GL_LUMINANCE_ALPHA, GL_HALF_FLOAT),
    (GL_DEPTH24_STENCIL8, GL_DEPTH_STENCIL, GL_UNSIGNED_BYTE),
    (GL_DEPTH_COMPONENT32F, GL_DEPTH_COMPONENT, GL_FLOAT),
    (GL_DEPTH32F_STENCIL8, GL_DEPTH_STENCIL, GL_FLOAT),
    (GL_SRGB8, GL_RGB, GL_UNSIGNED_BYTE),
    (GL_SRGB8_ALPHA8, GL_RGBA, GL_UNSIGNED_BYTE),
    (GL_SLUMINANCE8, GL_LUMINANCE, GL_UNSIGNED_BYTE),
    (GL_SLUMINANCE8_ALPHA8, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE),
    (GL_COMPRESSED_SRGB_S3TC_DXT1_EXT, GL_COMPRESSED_SRGB_S3TC_DXT1_EXT,
      GL_COMPRESSED_SRGB_S3TC_DXT1_EXT),
    (GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT,
      GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT,
      GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT),
    (GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT,
      GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT,
      GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT),
    (GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT,
      GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT,
      GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT),
    (GL_RGB9_E5, GL_RGBA, GL_FLOAT),
    (GL_R11F_G11F_B10F, GL_RGB, GL_FLOAT),
    (GL_COMPRESSED_LUMINANCE_LATC1_EXT, GL_COMPRESSED_LUMINANCE_LATC1_EXT,
      GL_COMPRESSED_LUMINANCE_LATC1_EXT),
    (GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT,
      GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT,
      GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT),
    (GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT,
      GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT,
      GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT),
    (GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT,
      GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT,
      GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT),
    (GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI,
      GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI,
      GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI),
    (GL_RGBA32UI, GL_RGBA_INTEGER, GL_UNSIGNED_INT),
    (GL_RGB32UI, GL_RGB_INTEGER, GL_UNSIGNED_INT),
    (GL_ALPHA32UI_EXT, GL_ALPHA_INTEGER, GL_UNSIGNED_INT),
    (GL_INTENSITY32UI_EXT, GL_LUMINANCE_INTEGER_EXT, GL_UNSIGNED_INT),
    (GL_LUMINANCE32UI_EXT, GL_LUMINANCE_INTEGER_EXT, GL_UNSIGNED_INT),
    (GL_LUMINANCE_ALPHA32UI_EXT, GL_LUMINANCE_ALPHA_INTEGER_EXT, GL_UNSIGNED_INT),
    (GL_RGBA16UI, GL_RGBA_INTEGER, GL_UNSIGNED_SHORT),
    (GL_RGB16UI, GL_RGB_INTEGER, GL_UNSIGNED_SHORT),
    (GL_ALPHA16UI_EXT, GL_ALPHA_INTEGER, GL_UNSIGNED_SHORT),
    (GL_INTENSITY16UI_EXT, GL_LUMINANCE_INTEGER_EXT, GL_UNSIGNED_SHORT),
    (GL_LUMINANCE16UI_EXT, GL_LUMINANCE_INTEGER_EXT, GL_UNSIGNED_SHORT),
    (GL_LUMINANCE_ALPHA16UI_EXT, GL_LUMINANCE_ALPHA_INTEGER_EXT, GL_UNSIGNED_SHORT),
    (GL_RGBA8UI, GL_RGBA_INTEGER, GL_UNSIGNED_BYTE),
    (GL_RGB8UI, GL_RGB_INTEGER, GL_UNSIGNED_BYTE),
    (GL_ALPHA8UI_EXT, GL_ALPHA_INTEGER, GL_UNSIGNED_BYTE),
    (GL_INTENSITY8UI_EXT, GL_LUMINANCE_INTEGER_EXT, GL_UNSIGNED_BYTE),
    (GL_LUMINANCE8UI_EXT, GL_LUMINANCE_INTEGER_EXT, GL_UNSIGNED_BYTE),
    (GL_LUMINANCE_ALPHA8UI_EXT, GL_LUMINANCE_ALPHA_INTEGER_EXT, GL_UNSIGNED_BYTE),
    (GL_RGBA32I, GL_RGBA_INTEGER, GL_INT),
    (GL_RGB32I, GL_RGB_INTEGER, GL_INT),
    (GL_ALPHA32I_EXT, GL_ALPHA_INTEGER, GL_INT),
    (GL_INTENSITY32I_EXT, GL_LUMINANCE_INTEGER_EXT, GL_INT),
    (GL_LUMINANCE32I_EXT, GL_LUMINANCE_INTEGER_EXT, GL_INT),
    (GL_LUMINANCE_ALPHA32I_EXT, GL_LUMINANCE_ALPHA_INTEGER_EXT, GL_INT),
    (GL_RGBA16I, GL_RGBA_INTEGER, GL_SHORT),
    (GL_RGB16I, GL_RGB_INTEGER, GL_SHORT),
    (GL_ALPHA16I_EXT, GL_ALPHA_INTEGER, GL_SHORT),
    (GL_INTENSITY16I_EXT, GL_LUMINANCE_INTEGER_EXT, GL_SHORT),
    (GL_LUMINANCE16I_EXT, GL_LUMINANCE_INTEGER_EXT, GL_SHORT),
    (GL_LUMINANCE_ALPHA16I_EXT, GL_LUMINANCE_ALPHA_INTEGER_EXT, GL_SHORT),
    (GL_RGBA8I, GL_RGBA_INTEGER, GL_BYTE),
    (GL_RGB8I, GL_RGB_INTEGER, GL_BYTE),
    (GL_ALPHA8I_EXT, GL_ALPHA_INTEGER, GL_BYTE),
    (GL_INTENSITY8I_EXT, GL_INTENSITY, GL_BYTE),
    (GL_LUMINANCE8I_EXT, GL_LUMINANCE, GL_BYTE),
    (GL_LUMINANCE_ALPHA8I_EXT, GL_LUMINANCE_ALPHA, GL_BYTE),
    (GL_RG32UI, GL_RG, GL_UNSIGNED_INT),
    (GL_R32UI, GL_RED_INTEGER, GL_UNSIGNED_INT),
    (GL_RG16UI, GL_RG, GL_UNSIGNED_SHORT),
    (GL_R16UI, GL_RED_INTEGER, GL_UNSIGNED_SHORT),
    (GL_RG8UI, GL_RG, GL_UNSIGNED_BYTE),
    (GL_R8UI, GL_RED_INTEGER, GL_UNSIGNED_BYTE),
    (GL_RG32I, GL_RG, GL_INT),
    (GL_R32I, GL_RED_INTEGER, GL_INT),
    (GL_RG16I, GL_RG, GL_SHORT),
    (GL_R16I, GL_RED_INTEGER, GL_SHORT),
    (GL_RG8I, GL_RG, GL_BYTE),
    (GL_R8I, GL_RED_INTEGER, GL_BYTE),
    (GL_RG8, GL_RG, GL_BYTE),
    (GL_R8, GL_RED, GL_BYTE),
    (GL_RG16, GL_RG, GL_SHORT),
    (GL_R16, GL_RED, GL_SHORT),
    (GL_RG16F, GL_RG, GL_HALF_FLOAT),
    (GL_R16F, GL_RED, GL_HALF_FLOAT),
    (GL_RG32F, GL_RG, GL_FLOAT),
    (GL_R32F, GL_LUMINANCE, GL_FLOAT),
    (GL_COMPRESSED_RED_RGTC1, GL_COMPRESSED_RED_RGTC1, GL_COMPRESSED_RED_RGTC1),
    (GL_COMPRESSED_SIGNED_RED_RGTC1, GL_COMPRESSED_SIGNED_RED_RGTC1,
      GL_COMPRESSED_SIGNED_RED_RGTC1),
    (GL_COMPRESSED_RG_RGTC2, GL_COMPRESSED_RG_RGTC2, GL_COMPRESSED_RG_RGTC2),
    (GL_COMPRESSED_SIGNED_RG_RGTC2, GL_COMPRESSED_SIGNED_RG_RGTC2,
      GL_COMPRESSED_SIGNED_RG_RGTC2)
    );

function InternalFormatToOpenGLFormat(texFormat: TGLInternalFormat): Integer;
begin
  Result := cTextureFormatToOpenGL[texFormat, 0];
end;

function OpenGLFormatToInternalFormat(intFormat: Integer): TGLInternalFormat;
var
  i: TGLInternalFormat;
begin
  Result := tfRGBA8;
  for i := Low(cTextureFormatToOpenGL) to High(cTextureFormatToOpenGL) do
    if intFormat = cTextureFormatToOpenGL[i, 0] then
    begin
      Result := i;
      Exit;
    end;
  Assert(false);
end;

function GetTextureElementSize(texFormat: TGLInternalFormat): Integer;
begin
  Result := GetTextureElementSize(cTextureFormatToOpenGL[texFormat, 1],
    cTextureFormatToOpenGL[texFormat, 2]);
end;

function GetTextureElementSize(colorFormat: TGLEnum; dataType: TGLEnum):
  Integer;
var
  components: Byte;
begin
  case colorFormat of
    GL_RGB, GL_BGR: components := 3;
    GL_RGBA, GL_BGRA: components := 4;
    GL_ALPHA: components := 1;
    GL_LUMINANCE: components := 1;
    GL_LUMINANCE_ALPHA: components := 2;
    GL_INTENSITY: components := 1;
    GL_RED: components := 1;
    GL_GREEN: components := 1;
    GL_BLUE: components := 1;
    GL_RG: components := 2;

    GL_RGB_INTEGER: components := 3;
    GL_RGBA_INTEGER: components := 4;
    GL_ALPHA_INTEGER: components := 1;
    GL_LUMINANCE_INTEGER_EXT: components := 1;
    GL_LUMINANCE_ALPHA_INTEGER_EXT: components := 2;
    GL_RED_INTEGER: components := 1;
    GL_RG_INTEGER: components := 2;
  else
    components := 1;
  end;

  case dataType of
    GL_BITMAP,
      GL_UNSIGNED_BYTE,
      GL_BYTE: Result := components;
    GL_UNSIGNED_BYTE_3_3_2,
      GL_UNSIGNED_BYTE_2_3_3_REV: Result := 1;
    GL_UNSIGNED_SHORT,
      GL_SHORT: Result := components * 2;
    GL_UNSIGNED_SHORT_4_4_4_4,
      GL_UNSIGNED_SHORT_4_4_4_4_REV,
      GL_UNSIGNED_SHORT_5_6_5,
      GL_UNSIGNED_SHORT_5_6_5_REV,
      GL_UNSIGNED_SHORT_5_5_5_1,
      GL_UNSIGNED_SHORT_1_5_5_5_REV: Result := 2;

    GL_UNSIGNED_INT,
      GL_INT: Result := components * 4;
    GL_UNSIGNED_INT_8_8_8_8,
      GL_UNSIGNED_INT_8_8_8_8_REV,
      GL_UNSIGNED_INT_10_10_10_2,
      GL_UNSIGNED_INT_2_10_10_10_REV: Result := 4;

    GL_FLOAT: Result := components * 4;
    GL_HALF_FLOAT: Result := components * 2;

    GL_COMPRESSED_RGB_S3TC_DXT1_EXT: Result := 8;
    GL_COMPRESSED_RGBA_S3TC_DXT1_EXT: Result := 8;
    GL_COMPRESSED_RGBA_S3TC_DXT3_EXT: Result := 16;
    GL_COMPRESSED_RGBA_S3TC_DXT5_EXT: Result := 16;
    GL_COMPRESSED_SRGB_S3TC_DXT1_EXT: Result := 8;
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT: Result := 8;
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT: Result := 16;
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT: Result := 16;
    GL_COMPRESSED_LUMINANCE_LATC1_EXT: Result := 8;
    GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT: Result := 8;
    GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT: Result := 16;
    GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT: Result := 16;
    GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI: Result := 16;
    GL_COMPRESSED_RED_RGTC1: Result := 8;
    GL_COMPRESSED_SIGNED_RED_RGTC1: Result := 8;
    GL_COMPRESSED_RG_RGTC2: Result := 16;
    GL_COMPRESSED_SIGNED_RG_RGTC2: Result := 16;
  else
    Result := 1;
  end;
end;

function CompressedInternalFormatToOpenGL(texFormat: TGLInternalFormat):
  Integer;
begin
  Result := GL_COMPRESSED_RGBA;
  case texFormat of
    tfRGB8: Result := GL_COMPRESSED_RGB;
    tfRGBA8: Result := GL_COMPRESSED_RGBA;
    tfRGB5: Result := GL_COMPRESSED_RGB;
    tfRGBA4: Result := GL_COMPRESSED_RGBA;
    tfALPHA8: Result := GL_COMPRESSED_ALPHA;
    tfLUMINANCE8: Result := GL_COMPRESSED_LUMINANCE;
    tfLUMINANCE8_ALPHA8: Result := GL_COMPRESSED_LUMINANCE_ALPHA;
    tfINTENSITY8: Result := GL_COMPRESSED_INTENSITY;
    else Assert(false);
  end;
end;

procedure FindCompatibleDataFormat(texFormat: TGLInternalFormat; out dFormat:
  TGLEnum; out dType: GLenum);
begin
  dFormat := cTextureFormatToOpenGL[texFormat, 1];
  dType := cTextureFormatToOpenGL[texFormat, 2];
end;

function IsTargetSupported(target: TGLEnum): Boolean;
begin
  case target of
    GL_TEXTURE_1D: Result := GL_VERSION_1_1 or GL_EXT_texture_object;
    GL_TEXTURE_2D: Result := GL_VERSION_1_1 or GL_EXT_texture_object;
    GL_TEXTURE_3D: Result := GL_EXT_texture3D;
    GL_TEXTURE_RECTANGLE: Result := GL_ARB_texture_rectangle;
    GL_TEXTURE_CUBE_MAP,
      GL_TEXTURE_CUBE_MAP_POSITIVE_X,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
      GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
      GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_Z: Result := GL_ARB_texture_cube_map;
    GL_TEXTURE_1D_ARRAY: Result := GL_EXT_texture_array;
    GL_TEXTURE_2D_ARRAY: Result := GL_EXT_texture_array;
    GL_TEXTURE_CUBE_MAP_ARRAY: Result := GL_ARB_texture_cube_map_array;
  else
    begin
      Result := false;
      Assert(False, glsErrorEx + glsUnknownType);
    end;
  end;
end;

function IsFormatSupported(texFormat: TGLInternalFormat): Boolean;
begin
  Result := false;

  if ((texFormat >= tfALPHA4) and (texFormat <= tfALPHA16)) or
    ((texFormat >= tfLUMINANCE4) and (texFormat <= tfR16G16B16A16)) then
  begin
    Result := GL_VERSION_1_1;
    EXIT;
  end;

  if ((texFormat >= tfDEPTH_COMPONENT16) and (texFormat <= tfDEPTH_COMPONENT32))
    then
  begin
    Result := GL_ARB_depth_texture;
    EXIT;
  end;

  if ((texFormat >= tfCOMPRESSED_RGB_S3TC_DXT1) and (texFormat <=
    tfCOMPRESSED_RGBA_S3TC_DXT5)) then
  begin
    Result := GL_EXT_texture_compression_s3tc;
    EXIT;
  end;

  if ((texFormat >= tfSIGNED_LUMINANCE8) and (texFormat <=
    tfDSDT8_MAG8_INTENSITY8)) then
  begin
    Result := GL_NV_texture_shader;
    EXIT;
  end;

  if ((texFormat = tfHILO8) or (texFormat = tfSIGNED_HILO8)) then
  begin
    Result := GL_NV_texture_shader3;
    EXIT;
  end;

  if ((texFormat >= tfFLOAT_R16) and (texFormat <= tfFLOAT_RGBA32)) then
  begin
    Result := GL_NV_float_buffer;
    EXIT;
  end;

  if ((texFormat >= tfRGBA_FLOAT32)
    and (texFormat <= tfLUMINANCE_ALPHA_FLOAT16)) then
  begin
    Result := GL_ATI_texture_float;
    EXIT;
  end;

  if texFormat = tfDEPTH24_STENCIL8 then
  begin
    Result := GL_EXT_packed_depth_stencil;
    EXIT;
  end;

  if ((texFormat = tfDEPTH_COMPONENT32F) or (texFormat = tfDEPTH32F_STENCIL8))
    then
  begin
    Result := GL_NV_depth_buffer_float;
    EXIT;
  end;

  if ((texFormat >= tfSRGB8) and (texFormat <=
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5)) then
  begin
    Result := GL_EXT_texture_sRGB;
    EXIT;
  end;

  if texFormat = tfRGB9_E5 then
  begin
    Result := GL_EXT_texture_shared_exponent;
    EXIT;
  end;

  if texFormat = tfR11F_G11F_B10F then
  begin
    Result := GL_EXT_packed_float;
    EXIT;
  end;

  if ((texFormat >= tfCOMPRESSED_LUMINANCE_LATC1) and (texFormat <=
    tfCOMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2)) then
  begin
    Result := GL_EXT_texture_compression_latc;
  end;

  if texFormat = tfCOMPRESSED_LUMINANCE_ALPHA_3DC then
  begin
    Result := GL_ATI_texture_compression_3dc;
    EXIT;
  end;

  if ((texFormat >= tfRGBA32UI) and (texFormat <= tfLUMINANCE_ALPHA8I)) then
  begin
    Result := GL_EXT_texture_integer;
    EXIT;
  end;

  if ((texFormat >= tfRG32UI) and (texFormat <= tfR32F)) then
    Result := GL_ARB_texture_rg;

  if ((texFormat >= tfCOMPRESSED_RED_RGTC1) and (texFormat <=
    tfCOMPRESSED_SIGNED_RG_RGTC2)) then
  begin
    Result := GL_ARB_texture_compression_rgtc;
  end
end;

function IsFloatFormat(texFormat: TGLInternalFormat): boolean;
begin
  Result := IsFloatFormat(InternalFormatToOpenGLFormat(texFormat));
end;

function IsFloatFormat(intFormat: TGLEnum): boolean;
const
  cFloatFormat: array[0..26] of GLenum = (
    GL_RG16F,
    GL_R16F,
    GL_RG32F,
    GL_R32F,
    GL_FLOAT_R16_NV,
    GL_FLOAT_R32_NV,
    GL_FLOAT_RG16_NV,
    GL_FLOAT_RGB16_NV,
    GL_FLOAT_RGBA16_NV,
    GL_FLOAT_RG32_NV,
    GL_FLOAT_RGB32_NV,
    GL_FLOAT_RGBA32_NV,
    GL_RGBA_FLOAT32_ATI,
    GL_RGB_FLOAT32_ATI,
    GL_ALPHA_FLOAT32_ATI,
    GL_INTENSITY_FLOAT32_ATI,
    GL_LUMINANCE_FLOAT32_ATI,
    GL_LUMINANCE_ALPHA_FLOAT32_ATI,
    GL_RGBA_FLOAT16_ATI,
    GL_RGB_FLOAT16_ATI,
    GL_ALPHA_FLOAT16_ATI,
    GL_INTENSITY_FLOAT16_ATI,
    GL_LUMINANCE_FLOAT16_ATI,
    GL_LUMINANCE_ALPHA_FLOAT16_ATI,
    GL_DEPTH_COMPONENT32F,
    GL_DEPTH32F_STENCIL8,
    GL_R11F_G11F_B10F);
var
  i: GLenum;
begin
  Result := false;
  for i := 0 to High(cFloatFormat) do
    if cFloatFormat[i] = intFormat then
    begin
      Result := true;
      exit;
    end;
end;

function IsDepthFormat(texFormat: TGLInternalFormat): boolean;
begin
  Result := IsDepthFormat(InternalFormatToOpenGLFormat(texFormat));
end;

function IsDepthFormat(intFormat: TGLEnum): boolean;
const
  cDepthFormat: array[0..5] of GLenum = (
    GL_DEPTH_COMPONENT16,
    GL_DEPTH_COMPONENT24,
    GL_DEPTH_COMPONENT32,
    GL_DEPTH24_STENCIL8,
    GL_DEPTH_COMPONENT32F,
    GL_DEPTH32F_STENCIL8);
var
  i: GLenum;
begin
  Result := false;
  for i := 0 to High(cDepthFormat) do
    if cDepthFormat[i] = intFormat then
    begin
      Result := true;
      exit;
    end;
end;

function IsCompressedFormat(texFormat: TGLInternalFormat): boolean;
begin
  Result := IsCompressedFormat(InternalFormatToOpenGLFormat(texFormat));
end;

function IsCompressedFormat(intFormat: TGLEnum): boolean;
const
  cCompressedFormat: array[0..24] of GLenum = (
    GL_COMPRESSED_RGB_ARB,
    GL_COMPRESSED_RGBA_ARB,
    GL_COMPRESSED_ALPHA_ARB,
    GL_COMPRESSED_LUMINANCE_ARB,
    GL_COMPRESSED_LUMINANCE_ALPHA_ARB,
    GL_COMPRESSED_INTENSITY_ARB,
    GL_COMPRESSED_RGB_ARB,
    GL_COMPRESSED_RGB_ARB,
    GL_COMPRESSED_RGB_S3TC_DXT1_EXT,
    GL_COMPRESSED_RGBA_S3TC_DXT1_EXT,
    GL_COMPRESSED_RGBA_S3TC_DXT3_EXT,
    GL_COMPRESSED_RGBA_S3TC_DXT5_EXT,
    GL_COMPRESSED_SRGB_S3TC_DXT1_EXT,
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT,
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT,
    GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT,
    GL_COMPRESSED_LUMINANCE_LATC1_EXT,
    GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT,
    GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT,
    GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT,
    GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI,
    GL_COMPRESSED_RED_RGTC1,
    GL_COMPRESSED_SIGNED_RED_RGTC1,
    GL_COMPRESSED_RG_RGTC2,
    GL_COMPRESSED_SIGNED_RG_RGTC2);
var
  i: GLenum;
begin
  Result := false;
  for i := 0 to High(cCompressedFormat) do
    if cCompressedFormat[i] = intFormat then
    begin
      Result := true;
      exit;
    end;
end;

function GetGenericCompressedFormat(const texFormat: TGLInternalFormat;
  const colorFormat: TGLEnum; out internalFormat: TGLEnum): Boolean;

begin
  Result := false;
  if IsCompressedFormat(texFormat) then
    Exit;
  if not IsFormatSupported(texFormat) then
    Exit;
  internalFormat := 0;

  if ((texFormat >= tfSRGB8) and (texFormat <=
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5)) then
    case colorFormat of
      GL_RGB: internalFormat := GL_COMPRESSED_SRGB;
      GL_RGBA: internalFormat := GL_COMPRESSED_SRGB_ALPHA;
      GL_LUMINANCE: internalFormat := GL_COMPRESSED_SLUMINANCE;
      GL_LUMINANCE_ALPHA: internalFormat := GL_COMPRESSED_SLUMINANCE_ALPHA;
    end
  else
    case colorFormat of
      GL_RGB, GL_BGR: internalFormat := GL_COMPRESSED_RGB;
      GL_RGBA, GL_BGRA: internalFormat := GL_COMPRESSED_RGBA;
      GL_ALPHA: internalFormat := GL_COMPRESSED_ALPHA;
      GL_LUMINANCE: internalFormat := GL_COMPRESSED_LUMINANCE;
      GL_LUMINANCE_ALPHA: internalFormat := GL_COMPRESSED_LUMINANCE_ALPHA;
      GL_INTENSITY: internalFormat := GL_COMPRESSED_INTENSITY;
      GL_RED: internalFormat := GL_COMPRESSED_RED;
      GL_RG: internalFormat := GL_COMPRESSED_RG;
    end;

  if internalFormat = 0 then
    Exit;
  Result := true;
end;

function GetUncompressedFormat(const texFormat: TGLInternalFormat;
  out internalFormat: TGLInternalFormat; out colorFormat: TGLEnum): Boolean;
begin
  Result := false;
  if not IsCompressedFormat(texFormat) then
    Exit;
  if not IsFormatSupported(texFormat) then
    Exit;
  colorFormat := 0;
  case texFormat of
    tfCOMPRESSED_RGB_S3TC_DXT1:
      begin
        colorFormat := GL_RGB;
        internalFormat := tfRGB8;
      end;
    tfCOMPRESSED_RGBA_S3TC_DXT1:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfRGBA8;
      end;
    tfCOMPRESSED_RGBA_S3TC_DXT3:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfRGBA8;
      end;
    tfCOMPRESSED_RGBA_S3TC_DXT5:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfRGBA8;
      end;
    tfCOMPRESSED_SRGB_S3TC_DXT1:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfSRGB8;
      end;
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT1:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfSRGB8_ALPHA8;
      end;
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT3:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfSRGB8_ALPHA8;
      end;
    tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5:
      begin
        colorFormat := GL_RGBA;
        internalFormat := tfSRGB8_ALPHA8;
      end;
    tfCOMPRESSED_LUMINANCE_LATC1:
      begin
        colorFormat := GL_LUMINANCE;
        internalFormat := tfLUMINANCE8;
      end;
    tfCOMPRESSED_SIGNED_LUMINANCE_LATC1:
      begin
        colorFormat := GL_LUMINANCE;
        internalFormat := tfSIGNED_LUMINANCE8;
      end;
    tfCOMPRESSED_LUMINANCE_ALPHA_LATC2:
      begin
        colorFormat := GL_LUMINANCE_ALPHA;
        internalFormat := tfLUMINANCE8_ALPHA8;
      end;
    tfCOMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2:
      begin
        colorFormat := GL_LUMINANCE_ALPHA;
        internalFormat := tfSIGNED_LUMINANCE8_ALPHA8;
      end;
    tfCOMPRESSED_LUMINANCE_ALPHA_3DC:
      begin
        colorFormat := GL_LUMINANCE_ALPHA;
        internalFormat := tfLUMINANCE8_ALPHA8;
      end;
    tfCOMPRESSED_RED_RGTC1:
      begin
        colorFormat := GL_RED;
        internalFormat := tfR8;
      end;
    tfCOMPRESSED_SIGNED_RED_RGTC1:
      begin
        colorFormat := GL_RED;
        internalFormat := tfR8;
      end;
    tfCOMPRESSED_RG_RGTC2:
      begin
        colorFormat := GL_RG;
        internalFormat := tfRG8;
      end;
    tfCOMPRESSED_SIGNED_RG_RGTC2:
      begin
        colorFormat := GL_RG;
        internalFormat := tfRG8;
      end;
  end;
  Result := colorFormat<>0;
end;

end.

