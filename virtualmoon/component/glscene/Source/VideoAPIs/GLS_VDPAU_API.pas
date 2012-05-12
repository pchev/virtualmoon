//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLS_VDPAU_API<p>

  <b>History : </b><font size=-1><ul>
  <li>23/10/10 - Yar - Creation
  </ul></font>
}

// {
// * This copyright notice applies to this header file:
// *
// * Copyright (c) 2008-2010 NVIDIA Corporation
// *
// * Permission is hereby granted, free of charge, to any person
// * obtaining a copy of this software and associated documentation
// * files (the "Software"), to deal in the Software without
// * restriction, including without limitation the rights to use,
// * copy, modify, merge, publish, distribute, sublicense, and/or sell
// * copies of the Software, and to permit persons to whom the
// * Software is furnished to do so, subject to the following
// * conditions:
// *
// * The above copyright notice and this permission notice shall be
// * included in all copies or substantial portions of the Software.
// *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// * OTHER DEALINGS IN THE SOFTWARE.
// }

unit GLS_VDPAU_API;

{$I GLScene.inc}
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

interface

{$IFDEF UNIX}
uses
  Xlib, Types, LCLType, X, XUtil, dynlibs;
{$ENDIF}

{$IFDEF UNIX}
const
  libvdpau = 'libvdpau.so';
{$ENDIF}

const
  VDP_TRUE = 1;
  VDP_FALSE = 0;

type
  PVdpBool = ^TVdpBool;
  TVdpBool = longint;

const
  VDP_INVALID_HANDLE = $FFFFFFFF;

type
  PVdpChromaType = ^TVdpChromaType;
  TVdpChromaType = Tuint32_t;

type
  PVdpYCbCrFormat = ^TVdpYCbCrFormat;
  TVdpYCbCrFormat = Tuint32_t;

const
  VDP_CHROMA_TYPE_420 = TVdpChromaType(0);
  VDP_CHROMA_TYPE_422 = TVdpChromaType(1);
  VDP_CHROMA_TYPE_444 = TVdpChromaType(2);

  VDP_YCBCR_FORMAT_NV12 = TVdpYCbCrFormat(0);
  VDP_YCBCR_FORMAT_YV12 = TVdpYCbCrFormat(1);
  VDP_YCBCR_FORMAT_UYVY = TVdpYCbCrFormat(2);
  VDP_YCBCR_FORMAT_YUYV = TVdpYCbCrFormat(3);
  VDP_YCBCR_FORMAT_Y8U8V8A8 = TVdpYCbCrFormat(4);
  VDP_YCBCR_FORMAT_V8U8Y8A8 = TVdpYCbCrFormat(5);

  VDP_RGBA_FORMAT_B8G8R8A8 = TVdpRGBAFormat(0);
  VDP_RGBA_FORMAT_R8G8B8A8 = TVdpRGBAFormat(1);
  VDP_RGBA_FORMAT_R10G10B10A2 = TVdpRGBAFormat(2);
  VDP_RGBA_FORMAT_B10G10R10A2 = TVdpRGBAFormat(3);
  VDP_RGBA_FORMAT_A8 = TVdpRGBAFormat(4);

  VDP_INDEXED_FORMAT_A4I4 = TVdpIndexedFormat(0);
  VDP_INDEXED_FORMAT_I4A4 = TVdpIndexedFormat(1);
  VDP_INDEXED_FORMAT_A8I8 = TVdpIndexedFormat(2);
  VDP_INDEXED_FORMAT_I8A8 = TVdpIndexedFormat(3);

  VDP_COLOR_STANDARD_ITUR_BT_601 = TVdpColorStandard(0);
  VDP_COLOR_STANDARD_ITUR_BT_709 = TVdpColorStandard(1);
  VDP_COLOR_STANDARD_SMPTE_240M = TVdpColorStandard(2);

  VDP_COLOR_TABLE_FORMAT_B8G8R8X8 = TVdpColorTableFormat(0);

  VDP_DECODER_PROFILE_MPEG1 = TVdpDecoderProfile(0);
  VDP_DECODER_PROFILE_MPEG2_SIMPLE = TVdpDecoderProfile(1);
  VDP_DECODER_PROFILE_MPEG2_MAIN = TVdpDecoderProfile(2);
  VDP_DECODER_PROFILE_H264_BASELINE = TVdpDecoderProfile(6);
  VDP_DECODER_PROFILE_H264_MAIN = TVdpDecoderProfile(7);
  VDP_DECODER_PROFILE_H264_HIGH = TVdpDecoderProfile(8);
  VDP_DECODER_PROFILE_VC1_SIMPLE = TVdpDecoderProfile(9);
  VDP_DECODER_PROFILE_VC1_MAIN = TVdpDecoderProfile(10);
  VDP_DECODER_PROFILE_VC1_ADVANCED = TVdpDecoderProfile(11);
  VDP_DECODER_PROFILE_MPEG4_PART2_SP = TVdpDecoderProfile(12);
  VDP_DECODER_PROFILE_MPEG4_PART2_ASP = TVdpDecoderProfile(13);
  VDP_DECODER_PROFILE_DIVX4_QMOBILE = TVdpDecoderProfile(14);
  VDP_DECODER_PROFILE_DIVX4_MOBILE = TVdpDecoderProfile(15);
  VDP_DECODER_PROFILE_DIVX4_HOME_THEATER = TVdpDecoderProfile(16);
  VDP_DECODER_PROFILE_DIVX4_HD_1080P = TVdpDecoderProfile(17);
  VDP_DECODER_PROFILE_DIVX5_QMOBILE = TVdpDecoderProfile(18);
  VDP_DECODER_PROFILE_DIVX5_MOBILE = TVdpDecoderProfile(19);
  VDP_DECODER_PROFILE_DIVX5_HOME_THEATER = TVdpDecoderProfile(20);
  VDP_DECODER_PROFILE_DIVX5_HD_1080P = TVdpDecoderProfile(21);

  VDP_VIDEO_MIXER_FEATURE_DEINTERLACE_TEMPORAL = TVdpVideoMixerFeature(0);
  VDP_VIDEO_MIXER_FEATURE_DEINTERLACE_TEMPORAL_SPATIAL = TVdpVideoMixerFeature(1);
  VDP_VIDEO_MIXER_FEATURE_INVERSE_TELECINE = TVdpVideoMixerFeature(2);
  VDP_VIDEO_MIXER_FEATURE_NOISE_REDUCTION = TVdpVideoMixerFeature(3);
  VDP_VIDEO_MIXER_FEATURE_SHARPNESS = TVdpVideoMixerFeature(4);
  VDP_VIDEO_MIXER_FEATURE_LUMA_KEY = TVdpVideoMixerFeature(5);

  VDP_VIDEO_MIXER_FEATURE_HIGH_QUALITY_SCALING_L1 = TVdpVideoMixerFeature(11);
  VDP_VIDEO_MIXER_FEATURE_HIGH_QUALITY_SCALING_L2 = TVdpVideoMixerFeature(12);
  VDP_VIDEO_MIXER_FEATURE_HIGH_QUALITY_SCALING_L3 = TVdpVideoMixerFeature(13);
  VDP_VIDEO_MIXER_FEATURE_HIGH_QUALITY_SCALING_L4 = TVdpVideoMixerFeature(14);
  VDP_VIDEO_MIXER_FEATURE_HIGH_QUALITY_SCALING_L5 = TVdpVideoMixerFeature(15);
  VDP_VIDEO_MIXER_FEATURE_HIGH_QUALITY_SCALING_L6 = TVdpVideoMixerFeature(16);
  VDP_VIDEO_MIXER_FEATURE_HIGH_QUALITY_SCALING_L7 = TVdpVideoMixerFeature(17);
  VDP_VIDEO_MIXER_FEATURE_HIGH_QUALITY_SCALING_L8 = TVdpVideoMixerFeature(18);
  VDP_VIDEO_MIXER_FEATURE_HIGH_QUALITY_SCALING_L9 = TVdpVideoMixerFeature(19);

  VDP_VIDEO_MIXER_PARAMETER_VIDEO_SURFACE_WIDTH = TVdpVideoMixerParameter(0);
  VDP_VIDEO_MIXER_PARAMETER_VIDEO_SURFACE_HEIGHT = TVdpVideoMixerParameter(1);
  VDP_VIDEO_MIXER_PARAMETER_CHROMA_TYPE = TVdpVideoMixerParameter(2);
  VDP_VIDEO_MIXER_PARAMETER_LAYERS = TVdpVideoMixerParameter(3);

  VDP_VIDEO_MIXER_ATTRIBUTE_BACKGROUND_COLOR = TVdpVideoMixerAttribute(0);
  VDP_VIDEO_MIXER_ATTRIBUTE_CSC_MATRIX = TVdpVideoMixerAttribute(1);
  VDP_VIDEO_MIXER_ATTRIBUTE_NOISE_REDUCTION_LEVEL = TVdpVideoMixerAttribute(2);
  VDP_VIDEO_MIXER_ATTRIBUTE_SHARPNESS_LEVEL = TVdpVideoMixerAttribute(3);
  VDP_VIDEO_MIXER_ATTRIBUTE_LUMA_KEY_MIN_LUMA = TVdpVideoMixerAttribute(4);
  VDP_VIDEO_MIXER_ATTRIBUTE_LUMA_KEY_MAX_LUMA = TVdpVideoMixerAttribute(5);
  VDP_VIDEO_MIXER_ATTRIBUTE_SKIP_CHROMA_DEINTERLACE = TVdpVideoMixerAttribute(6);

  VDP_FUNC_ID_GET_ERROR_STRING = TVdpFuncId(0);
  VDP_FUNC_ID_GET_PROC_ADDRESS = TVdpFuncId(1);
  VDP_FUNC_ID_GET_API_VERSION = TVdpFuncId(2);
  VDP_FUNC_ID_GET_INFORMATION_STRING = TVdpFuncId(4);
  VDP_FUNC_ID_DEVICE_DESTROY = TVdpFuncId(5);
  VDP_FUNC_ID_GENERATE_CSC_MATRIX = TVdpFuncId(6);
  VDP_FUNC_ID_VIDEO_SURFACE_QUERY_CAPABILITIES = TVdpFuncId(7);
  VDP_FUNC_ID_VIDEO_SURFACE_QUERY_GET_PUT_BITS_Y_CB_CR_CAPABILITIES = TVdpFuncId(8);
  VDP_FUNC_ID_VIDEO_SURFACE_CREATE = TVdpFuncId(9);
  VDP_FUNC_ID_VIDEO_SURFACE_DESTROY = TVdpFuncId(10);
  VDP_FUNC_ID_VIDEO_SURFACE_GET_PARAMETERS = TVdpFuncId(11);
  VDP_FUNC_ID_VIDEO_SURFACE_GET_BITS_Y_CB_CR = TVdpFuncId(12);
  VDP_FUNC_ID_VIDEO_SURFACE_PUT_BITS_Y_CB_CR = TVdpFuncId(13);
  VDP_FUNC_ID_OUTPUT_SURFACE_QUERY_CAPABILITIES = TVdpFuncId(14);
  VDP_FUNC_ID_OUTPUT_SURFACE_QUERY_GET_PUT_BITS_NATIVE_CAPABILITIES = TVdpFuncId(15);
  VDP_FUNC_ID_OUTPUT_SURFACE_QUERY_PUT_BITS_INDEXED_CAPABILITIES = TVdpFuncId(16);
  VDP_FUNC_ID_OUTPUT_SURFACE_QUERY_PUT_BITS_Y_CB_CR_CAPABILITIES = TVdpFuncId(17);
  VDP_FUNC_ID_OUTPUT_SURFACE_CREATE = TVdpFuncId(18);
  VDP_FUNC_ID_OUTPUT_SURFACE_DESTROY = TVdpFuncId(19);
  VDP_FUNC_ID_OUTPUT_SURFACE_GET_PARAMETERS = TVdpFuncId(20);
  VDP_FUNC_ID_OUTPUT_SURFACE_GET_BITS_NATIVE = TVdpFuncId(21);
  VDP_FUNC_ID_OUTPUT_SURFACE_PUT_BITS_NATIVE = TVdpFuncId(22);
  VDP_FUNC_ID_OUTPUT_SURFACE_PUT_BITS_INDEXED = TVdpFuncId(23);
  VDP_FUNC_ID_OUTPUT_SURFACE_PUT_BITS_Y_CB_CR = TVdpFuncId(24);
  VDP_FUNC_ID_BITMAP_SURFACE_QUERY_CAPABILITIES = TVdpFuncId(25);
  VDP_FUNC_ID_BITMAP_SURFACE_CREATE = TVdpFuncId(26);
  VDP_FUNC_ID_BITMAP_SURFACE_DESTROY = TVdpFuncId(27);
  VDP_FUNC_ID_BITMAP_SURFACE_GET_PARAMETERS = TVdpFuncId(28);
  VDP_FUNC_ID_BITMAP_SURFACE_PUT_BITS_NATIVE = TVdpFuncId(29);
  VDP_FUNC_ID_OUTPUT_SURFACE_RENDER_OUTPUT_SURFACE = TVdpFuncId(33);
  VDP_FUNC_ID_OUTPUT_SURFACE_RENDER_BITMAP_SURFACE = TVdpFuncId(34);
  VDP_FUNC_ID_OUTPUT_SURFACE_RENDER_VIDEO_SURFACE_LUMA = TVdpFuncId(35);
  VDP_FUNC_ID_DECODER_QUERY_CAPABILITIES = TVdpFuncId(36);
  VDP_FUNC_ID_DECODER_CREATE = TVdpFuncId(37);
  VDP_FUNC_ID_DECODER_DESTROY = TVdpFuncId(38);
  VDP_FUNC_ID_DECODER_GET_PARAMETERS = TVdpFuncId(39);
  VDP_FUNC_ID_DECODER_RENDER = TVdpFuncId(40);
  VDP_FUNC_ID_VIDEO_MIXER_QUERY_FEATURE_SUPPORT = TVdpFuncId(41);
  VDP_FUNC_ID_VIDEO_MIXER_QUERY_PARAMETER_SUPPORT = TVdpFuncId(42);
  VDP_FUNC_ID_VIDEO_MIXER_QUERY_ATTRIBUTE_SUPPORT = TVdpFuncId(43);
  VDP_FUNC_ID_VIDEO_MIXER_QUERY_PARAMETER_VALUE_RANGE = TVdpFuncId(44);
  VDP_FUNC_ID_VIDEO_MIXER_QUERY_ATTRIBUTE_VALUE_RANGE = TVdpFuncId(45);
  VDP_FUNC_ID_VIDEO_MIXER_CREATE = TVdpFuncId(46);
  VDP_FUNC_ID_VIDEO_MIXER_SET_FEATURE_ENABLES = TVdpFuncId(47);
  VDP_FUNC_ID_VIDEO_MIXER_SET_ATTRIBUTE_VALUES = TVdpFuncId(48);
  VDP_FUNC_ID_VIDEO_MIXER_GET_FEATURE_SUPPORT = TVdpFuncId(49);
  VDP_FUNC_ID_VIDEO_MIXER_GET_FEATURE_ENABLES = TVdpFuncId(50);
  VDP_FUNC_ID_VIDEO_MIXER_GET_PARAMETER_VALUES = TVdpFuncId(51);
  VDP_FUNC_ID_VIDEO_MIXER_GET_ATTRIBUTE_VALUES = TVdpFuncId(52);
  VDP_FUNC_ID_VIDEO_MIXER_DESTROY = TVdpFuncId(53);
  VDP_FUNC_ID_VIDEO_MIXER_RENDER = TVdpFuncId(54);
  VDP_FUNC_ID_PRESENTATION_QUEUE_TARGET_DESTROY = TVdpFuncId(55);
  VDP_FUNC_ID_PRESENTATION_QUEUE_CREATE = TVdpFuncId(56);
  VDP_FUNC_ID_PRESENTATION_QUEUE_DESTROY = TVdpFuncId(57);
  VDP_FUNC_ID_PRESENTATION_QUEUE_SET_BACKGROUND_COLOR = TVdpFuncId(58);
  VDP_FUNC_ID_PRESENTATION_QUEUE_GET_BACKGROUND_COLOR = TVdpFuncId(59);
  VDP_FUNC_ID_PRESENTATION_QUEUE_GET_TIME = TVdpFuncId(62);
  VDP_FUNC_ID_PRESENTATION_QUEUE_DISPLAY = TVdpFuncId(63);
  VDP_FUNC_ID_PRESENTATION_QUEUE_BLOCK_UNTIL_SURFACE_IDLE = TVdpFuncId(64);
  VDP_FUNC_ID_PRESENTATION_QUEUE_QUERY_SURFACE_STATUS = TVdpFuncId(65);
  VDP_FUNC_ID_PREEMPTION_CALLBACK_REGISTER = TVdpFuncId(66);

type
  PVdpRGBAFormat = ^TVdpRGBAFormat;
  TVdpRGBAFormat = Tuint32_t;

type
  PVdpIndexedFormat = ^TVdpIndexedFormat;
  TVdpIndexedFormat = Tuint32_t;

type
  PVdpPoint = ^TVdpPoint;

  TVdpPoint = record
    x: Tuint32_t;
    y: Tuint32_t;
  end;

  PVdpRect = ^TVdpRect;

  TVdpRect = record
    x0: Tuint32_t;
    y0: Tuint32_t;
    x1: Tuint32_t;
    y1: Tuint32_t;
  end;

  PVdpColor = ^TVdpColor;

  TVdpColor = record
    red: single;
    green: single;
    blue: single;
    alpha: single;
  end;

  PVdpStatus = ^TVdpStatus;
  TVdpStatus = (VDP_STATUS_OK = 0, VDP_STATUS_NO_IMPLEMENTATION, VDP_STATUS_DISPLAY_PREEMPTED, VDP_STATUS_INVALID_HANDLE, VDP_STATUS_INVALID_POINTER, VDP_STATUS_INVALID_CHROMA_TYPE, VDP_STATUS_INVALID_Y_CB_CR_FORMAT, VDP_STATUS_INVALID_RGBA_FORMAT, VDP_STATUS_INVALID_INDEXED_FORMAT, VDP_STATUS_INVALID_COLOR_STANDARD, VDP_STATUS_INVALID_COLOR_TABLE_FORMAT, VDP_STATUS_INVALID_BLEND_FACTOR, VDP_STATUS_INVALID_BLEND_EQUATION, VDP_STATUS_INVALID_FLAG, VDP_STATUS_INVALID_DECODER_PROFILE, VDP_STATUS_INVALID_VIDEO_MIXER_FEATURE, VDP_STATUS_INVALID_VIDEO_MIXER_PARAMETER, VDP_STATUS_INVALID_VIDEO_MIXER_ATTRIBUTE, VDP_STATUS_INVALID_VIDEO_MIXER_PICTURE_STRUCTURE, VDP_STATUS_INVALID_FUNC_ID, VDP_STATUS_INVALID_SIZE, VDP_STATUS_INVALID_VALUE, VDP_STATUS_INVALID_STRUCT_VERSION, VDP_STATUS_RESOURCES, VDP_STATUS_HANDLE_DEVICE_MISMATCH, VDP_STATUS_ERROR);

  PVdpGetErrorString = ^TVdpGetErrorString;
  TVdpGetErrorString = function(status: TVdpStatus): Pchar; cdecl;

const
  VDPAU_INTERFACE_VERSION = 1;
  VDPAU_VERSION = 1;

type

  TVdpGetApiVersion = function(api_version: Puint32_t): TVdpStatus; cdecl;

  TVdpGetInformationString = function(information_string: PPchar): TVdpStatus; cdecl;

  PVdpDevice = ^TVdpDevice;
  TVdpDevice = Tuint32_t;

  TVdpDeviceDestroy = function(device: TVdpDevice): TVdpStatus; cdecl;

  PVdpCSCMatrix = ^TVdpCSCMatrix;
  TVdpCSCMatrix = array [0 .. 2] of array [0 .. 3] of single;

const
  VDP_PROCAMP_VERSION = 0;

type
  PVdpProcamp = ^TVdpProcamp;

  TVdpProcamp = record
    struct_version: Tuint32_t;
    brightness: single;
    contrast: single;
    saturation: single;
    hue: single;
  end;

  PVdpColorStandard = ^TVdpColorStandard;
  TVdpColorStandard = Tuint32_t;

type

  TVdpGenerateCSCMatrix = function(procamp: PVdpProcamp; standard: TVdpColorStandard; csc_matrix: PVdpCSCMatrix): TVdpStatus; cdecl;

  TVdpVideoSurfaceQueryCapabilities = function(device: TVdpDevice; surface_chroma_type: TVdpChromaType; is_supported: PVdpBool; max_width: Puint32_t; max_height: Puint32_t): TVdpStatus; cdecl;

  TVdpVideoSurfaceQueryGetPutBitsYCbCrCapabilities = function(device: TVdpDevice; surface_chroma_type: TVdpChromaType; bits_ycbcr_format: TVdpYCbCrFormat; is_supported: PVdpBool): TVdpStatus; cdecl;

  PVdpVideoSurface = ^TVdpVideoSurface;
  TVdpVideoSurface = Tuint32_t;

  TVdpVideoSurfaceCreate = function(device: TVdpDevice; chroma_type: TVdpChromaType; width: Tuint32_t; height: Tuint32_t; surface: PVdpVideoSurface): TVdpStatus; cdecl;

  TVdpVideoSurfaceDestroy = function(surface: TVdpVideoSurface): TVdpStatus; cdecl;

  TVdpVideoSurfaceGetParameters = function(surface: TVdpVideoSurface; chroma_type: PVdpChromaType; width: Puint32_t; height: Puint32_t): TVdpStatus; cdecl;

  TVdpVideoSurfaceGetBitsYCbCr = function(surface: TVdpVideoSurface; destination_ycbcr_format: TVdpYCbCrFormat; destination_data: Ppointer; destination_pitches: Puint32_t): TVdpStatus; cdecl;

  TVdpVideoSurfacePutBitsYCbCr = function(surface: TVdpVideoSurface; source_ycbcr_format: TVdpYCbCrFormat; source_data: Ppointer; source_pitches: Puint32_t): TVdpStatus; cdecl;

  PVdpColorTableFormat = ^TVdpColorTableFormat;
  TVdpColorTableFormat = Tuint32_t;

type

  TVdpOutputSurfaceQueryCapabilities = function(device: TVdpDevice; surface_rgba_format: TVdpRGBAFormat; is_supported: PVdpBool; max_width: Puint32_t; max_height: Puint32_t): TVdpStatus; cdecl;

  TVdpOutputSurfaceQueryGetPutBitsNativeCapabilities = function(device: TVdpDevice; surface_rgba_format: TVdpRGBAFormat; is_supported: PVdpBool): TVdpStatus; cdecl;

  TVdpOutputSurfaceQueryPutBitsIndexedCapabilities = function(device: TVdpDevice; surface_rgba_format: TVdpRGBAFormat; bits_indexed_format: TVdpIndexedFormat; color_table_format: TVdpColorTableFormat; is_supported: PVdpBool): TVdpStatus; cdecl;

  TVdpOutputSurfaceQueryPutBitsYCbCrCapabilities = function(device: TVdpDevice; surface_rgba_format: TVdpRGBAFormat; bits_ycbcr_format: TVdpYCbCrFormat; is_supported: PVdpBool): TVdpStatus; cdecl;

  PVdpOutputSurface = ^TVdpOutputSurface;
  TVdpOutputSurface = Tuint32_t;

  TVdpOutputSurfaceCreate = function(device: TVdpDevice; rgba_format: TVdpRGBAFormat; width: Tuint32_t; height: Tuint32_t; surface: PVdpOutputSurface): TVdpStatus; cdecl;

  TVdpOutputSurfaceDestroy = function(surface: TVdpOutputSurface): TVdpStatus; cdecl;

  TVdpOutputSurfaceGetParameters = function(surface: TVdpOutputSurface; rgba_format: PVdpRGBAFormat; width: Puint32_t; height: Puint32_t): TVdpStatus; cdecl;

  TVdpOutputSurfaceGetBitsNative = function(surface: TVdpOutputSurface; source_rect: PVdpRect; destination_data: Ppointer; destination_pitches: Puint32_t): TVdpStatus; cdecl;

  TVdpOutputSurfacePutBitsNative = function(surface: TVdpOutputSurface; source_data: Ppointer; source_pitches: Puint32_t; destination_rect: PVdpRect): TVdpStatus; cdecl;

  TVdpOutputSurfacePutBitsIndexed = function(surface: TVdpOutputSurface; source_indexed_format: TVdpIndexedFormat; source_data: Ppointer; source_pitch: Puint32_t; destination_rect: PVdpRect; color_table_format: TVdpColorTableFormat; color_table: pointer): TVdpStatus; cdecl;

  TVdpOutputSurfacePutBitsYCbCr = function(surface: TVdpOutputSurface; source_ycbcr_format: TVdpYCbCrFormat; source_data: Ppointer; source_pitches: Puint32_t; destination_rect: PVdpRect; csc_matrix: PVdpCSCMatrix): TVdpStatus; cdecl;

  TVdpBitmapSurfaceQueryCapabilities = function(device: TVdpDevice; surface_rgba_format: TVdpRGBAFormat; is_supported: PVdpBool; max_width: Puint32_t; max_height: Puint32_t): TVdpStatus; cdecl;

  PVdpBitmapSurface = ^TVdpBitmapSurface;
  TVdpBitmapSurface = Tuint32_t;

  TVdpBitmapSurfaceCreate = function(device: TVdpDevice; rgba_format: TVdpRGBAFormat; width: Tuint32_t; height: Tuint32_t; frequently_accessed: TVdpBool; surface: PVdpBitmapSurface): TVdpStatus; cdecl;

  TVdpBitmapSurfaceDestroy = function(surface: TVdpBitmapSurface): TVdpStatus; cdecl;

  TVdpBitmapSurfaceGetParameters = function(surface: TVdpBitmapSurface; rgba_format: PVdpRGBAFormat; width: Puint32_t; height: Puint32_t; frequently_accessed: PVdpBool): TVdpStatus; cdecl;

  TVdpBitmapSurfacePutBitsNative = function(surface: TVdpBitmapSurface; source_data: Ppointer; source_pitches: Puint32_t; destination_rect: PVdpRect): TVdpStatus; cdecl;

  PVdpOutputSurfaceRenderBlendFactor = ^TVdpOutputSurfaceRenderBlendFactor;
  TVdpOutputSurfaceRenderBlendFactor = (VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_ZERO = 0, VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_ONE = 1, VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_SRC_COLOR = 2, VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_ONE_MINUS_SRC_COLOR = 3, VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_SRC_ALPHA = 4, VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA = 5, VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_DST_ALPHA = 6, VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_ONE_MINUS_DST_ALPHA = 7, VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_DST_COLOR = 8, VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_ONE_MINUS_DST_COLOR = 9, VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_SRC_ALPHA_SATURATE = 10, VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_CONSTANT_COLOR = 11, VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR = 12, VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_CONSTANT_ALPHA = 13, VDP_OUTPUT_SURFACE_RENDER_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA = 14);

  PVdpOutputSurfaceRenderBlendEquation = ^TVdpOutputSurfaceRenderBlendEquation;
  TVdpOutputSurfaceRenderBlendEquation = (VDP_OUTPUT_SURFACE_RENDER_BLEND_EQUATION_SUBTRACT = 0, VDP_OUTPUT_SURFACE_RENDER_BLEND_EQUATION_REVERSE_SUBTRACT = 1, VDP_OUTPUT_SURFACE_RENDER_BLEND_EQUATION_ADD = 2, VDP_OUTPUT_SURFACE_RENDER_BLEND_EQUATION_MIN = 3, VDP_OUTPUT_SURFACE_RENDER_BLEND_EQUATION_MAX = 4);

const
  VDP_OUTPUT_SURFACE_RENDER_BLEND_STATE_VERSION = 0;

type
  PVdpOutputSurfaceRenderBlendState = ^TVdpOutputSurfaceRenderBlendState;

  TVdpOutputSurfaceRenderBlendState = record
    struct_version: Tuint32_t;
    blend_factor_source_color: TVdpOutputSurfaceRenderBlendFactor;
    blend_factor_destination_color: TVdpOutputSurfaceRenderBlendFactor;
    blend_factor_source_alpha: TVdpOutputSurfaceRenderBlendFactor;
    blend_factor_destination_alpha: TVdpOutputSurfaceRenderBlendFactor;
    blend_equation_color: TVdpOutputSurfaceRenderBlendEquation;
    blend_equation_alpha: TVdpOutputSurfaceRenderBlendEquation;
    blend_constant: TVdpColor;
  end;

const
  VDP_OUTPUT_SURFACE_RENDER_ROTATE_0 = 0;
  VDP_OUTPUT_SURFACE_RENDER_ROTATE_90 = 1;
  VDP_OUTPUT_SURFACE_RENDER_ROTATE_180 = 2;
  VDP_OUTPUT_SURFACE_RENDER_ROTATE_270 = 3;
  VDP_OUTPUT_SURFACE_RENDER_COLOR_PER_VERTEX = 1 shl 2;

type

  TVdpOutputSurfaceRenderOutputSurface = function(destination_surface: TVdpOutputSurface; destination_rect: PVdpRect; source_surface: TVdpOutputSurface; source_rect: PVdpRect; colors: PVdpColor; blend_state: PVdpOutputSurfaceRenderBlendState; flags: Tuint32_t): TVdpStatus; cdecl;

  TVdpOutputSurfaceRenderBitmapSurface = function(destination_surface: TVdpOutputSurface; destination_rect: PVdpRect; source_surface: TVdpBitmapSurface; source_rect: PVdpRect; colors: PVdpColor; blend_state: PVdpOutputSurfaceRenderBlendState; flags: Tuint32_t): TVdpStatus; cdecl;

  PVdpDecoderProfile = ^TVdpDecoderProfile;
  TVdpDecoderProfile = Tuint32_t;

const
  VDP_DECODER_LEVEL_MPEG1_NA = 0;
  VDP_DECODER_LEVEL_MPEG2_LL = 0;
  VDP_DECODER_LEVEL_MPEG2_ML = 1;
  VDP_DECODER_LEVEL_MPEG2_HL14 = 2;
  VDP_DECODER_LEVEL_MPEG2_HL = 3;
  VDP_DECODER_LEVEL_H264_1 = 10;
  VDP_DECODER_LEVEL_H264_1b = 9;
  VDP_DECODER_LEVEL_H264_1_1 = 11;
  VDP_DECODER_LEVEL_H264_1_2 = 12;
  VDP_DECODER_LEVEL_H264_1_3 = 13;
  VDP_DECODER_LEVEL_H264_2 = 20;
  VDP_DECODER_LEVEL_H264_2_1 = 21;
  VDP_DECODER_LEVEL_H264_2_2 = 22;
  VDP_DECODER_LEVEL_H264_3 = 30;
  VDP_DECODER_LEVEL_H264_3_1 = 31;
  VDP_DECODER_LEVEL_H264_3_2 = 32;
  VDP_DECODER_LEVEL_H264_4 = 40;
  VDP_DECODER_LEVEL_H264_4_1 = 41;
  VDP_DECODER_LEVEL_H264_4_2 = 42;
  VDP_DECODER_LEVEL_H264_5 = 50;
  VDP_DECODER_LEVEL_H264_5_1 = 51;
  VDP_DECODER_LEVEL_VC1_SIMPLE_LOW = 0;
  VDP_DECODER_LEVEL_VC1_SIMPLE_MEDIUM = 1;
  VDP_DECODER_LEVEL_VC1_MAIN_LOW = 0;
  VDP_DECODER_LEVEL_VC1_MAIN_MEDIUM = 1;
  VDP_DECODER_LEVEL_VC1_MAIN_HIGH = 2;
  VDP_DECODER_LEVEL_VC1_ADVANCED_L0 = 0;
  VDP_DECODER_LEVEL_VC1_ADVANCED_L1 = 1;
  VDP_DECODER_LEVEL_VC1_ADVANCED_L2 = 2;
  VDP_DECODER_LEVEL_VC1_ADVANCED_L3 = 3;
  VDP_DECODER_LEVEL_VC1_ADVANCED_L4 = 4;
  VDP_DECODER_LEVEL_MPEG4_PART2_SP_L0 = 0;
  VDP_DECODER_LEVEL_MPEG4_PART2_SP_L1 = 1;
  VDP_DECODER_LEVEL_MPEG4_PART2_SP_L2 = 2;
  VDP_DECODER_LEVEL_MPEG4_PART2_SP_L3 = 3;
  VDP_DECODER_LEVEL_MPEG4_PART2_ASP_L0 = 0;
  VDP_DECODER_LEVEL_MPEG4_PART2_ASP_L1 = 1;
  VDP_DECODER_LEVEL_MPEG4_PART2_ASP_L2 = 2;
  VDP_DECODER_LEVEL_MPEG4_PART2_ASP_L3 = 3;
  VDP_DECODER_LEVEL_MPEG4_PART2_ASP_L4 = 4;
  VDP_DECODER_LEVEL_MPEG4_PART2_ASP_L5 = 5;
  VDP_DECODER_LEVEL_DIVX_NA = 0;

type

  TVdpDecoderQueryCapabilities = function(device: TVdpDevice; profile: TVdpDecoderProfile; is_supported: PVdpBool; max_level: Puint32_t; max_macroblocks: Puint32_t; max_width: Puint32_t; max_height: Puint32_t): TVdpStatus; cdecl;

  PVdpDecoder = ^TVdpDecoder;
  TVdpDecoder = Tuint32_t;

  TVdpDecoderCreate = function(device: TVdpDevice; profile: TVdpDecoderProfile; width: Tuint32_t; height: Tuint32_t; max_references: Tuint32_t; decoder: PVdpDecoder): TVdpStatus; cdecl;

  TVdpDecoderDestroy = function(decoder: TVdpDecoder): TVdpStatus; cdecl;

  TVdpDecoderGetParameters = function(decoder: TVdpDecoder; profile: PVdpDecoderProfile; width: Puint32_t; height: Puint32_t): TVdpStatus; cdecl;

const
  VDP_BITSTREAM_BUFFER_VERSION = 0;

type
  PVdpBitstreamBuffer = ^TVdpBitstreamBuffer;

  TVdpBitstreamBuffer = record
    struct_version: Tuint32_t;
    bitstream: pointer;
    bitstream_bytes: Tuint32_t;
  end;

  PVdpPictureInfo = ^TVdpPictureInfo;
  TVdpPictureInfo = pointer;

  PVdpPictureInfoMPEG1Or2 = ^TVdpPictureInfoMPEG1Or2;

  TVdpPictureInfoMPEG1Or2 = record
    forward_reference: TVdpVideoSurface;
    backward_reference: TVdpVideoSurface;
    slice_count: Tuint32_t;
    picture_structure: Tuint8_t;
    picture_coding_type: Tuint8_t;
    intra_dc_precision: Tuint8_t;
    frame_pred_frame_dct: Tuint8_t;
    concealment_motion_vectors: Tuint8_t;
    intra_vlc_format: Tuint8_t;
    alternate_scan: Tuint8_t;
    q_scale_type: Tuint8_t;
    top_field_first: Tuint8_t;
    full_pel_forward_vector: Tuint8_t;
    full_pel_backward_vector: Tuint8_t;
    f_code: array [0 .. 1] of array [0 .. 1] of Tuint8_t;
    intra_quantizer_matrix: array [0 .. 63] of Tuint8_t;
    non_intra_quantizer_matrix: array [0 .. 63] of Tuint8_t;
  end;

  PVdpReferenceFrameH264 = ^TVdpReferenceFrameH264;

  TVdpReferenceFrameH264 = record
    surface: TVdpVideoSurface;
    is_long_term: TVdpBool;
    top_is_reference: TVdpBool;
    bottom_is_reference: TVdpBool;
    field_order_cnt: array [0 .. 1] of Tint32_t;
    frame_idx: Tuint16_t;
  end;

  PVdpPictureInfoH264 = ^TVdpPictureInfoH264;

  TVdpPictureInfoH264 = record
    slice_count: Tuint32_t;
    field_order_cnt: array [0 .. 1] of Tint32_t;
    is_reference: TVdpBool;
    frame_num: Tuint16_t;
    field_pic_flag: Tuint8_t;
    bottom_field_flag: Tuint8_t;
    num_ref_frames: Tuint8_t;
    mb_adaptive_frame_field_flag: Tuint8_t;
    constrained_intra_pred_flag: Tuint8_t;
    weighted_pred_flag: Tuint8_t;
    weighted_bipred_idc: Tuint8_t;
    frame_mbs_only_flag: Tuint8_t;
    transform_8x8_mode_flag: Tuint8_t;
    chroma_qp_index_offset: Tint8_t;
    second_chroma_qp_index_offset: Tint8_t;
    pic_init_qp_minus26: Tint8_t;
    num_ref_idx_l0_active_minus1: Tuint8_t;
    num_ref_idx_l1_active_minus1: Tuint8_t;
    log2_max_frame_num_minus4: Tuint8_t;
    pic_order_cnt_type: Tuint8_t;
    log2_max_pic_order_cnt_lsb_minus4: Tuint8_t;
    delta_pic_order_always_zero_flag: Tuint8_t;
    direct_8x8_inference_flag: Tuint8_t;
    entropy_coding_mode_flag: Tuint8_t;
    pic_order_present_flag: Tuint8_t;
    deblocking_filter_control_present_flag: Tuint8_t;
    redundant_pic_cnt_present_flag: Tuint8_t;
    scaling_lists_4x4: array [0 .. 5] of array [0 .. 15] of Tuint8_t;
    scaling_lists_8x8: array [0 .. 1] of array [0 .. 63] of Tuint8_t;
    referenceFrames: array [0 .. 15] of TVdpReferenceFrameH264;
  end;

  PVdpPictureInfoVC1 = ^TVdpPictureInfoVC1;

  TVdpPictureInfoVC1 = record
    forward_reference: TVdpVideoSurface;
    backward_reference: TVdpVideoSurface;
    slice_count: Tuint32_t;
    picture_type: Tuint8_t;
    frame_coding_mode: Tuint8_t;
    postprocflag: Tuint8_t;
    pulldown: Tuint8_t;
    interlace: Tuint8_t;
    tfcntrflag: Tuint8_t;
    finterpflag: Tuint8_t;
    psf: Tuint8_t;
    dquant: Tuint8_t;
    panscan_flag: Tuint8_t;
    refdist_flag: Tuint8_t;
    quantizer: Tuint8_t;
    extended_mv: Tuint8_t;
    extended_dmv: Tuint8_t;
    overlap: Tuint8_t;
    vstransform: Tuint8_t;
    loopfilter: Tuint8_t;
    fastuvmc: Tuint8_t;
    range_mapy_flag: Tuint8_t;
    range_mapy: Tuint8_t;
    range_mapuv_flag: Tuint8_t;
    range_mapuv: Tuint8_t;
    multires: Tuint8_t;
    syncmarker: Tuint8_t;
    rangered: Tuint8_t;
    maxbframes: Tuint8_t;
    deblockEnable: Tuint8_t;
    pquant: Tuint8_t;
  end;

  PVdpPictureInfoMPEG4Part2 = ^TVdpPictureInfoMPEG4Part2;

  TVdpPictureInfoMPEG4Part2 = record
    forward_reference: TVdpVideoSurface;
    backward_reference: TVdpVideoSurface;
    trd: array [0 .. 1] of Tint32_t;
    trb: array [0 .. 1] of Tint32_t;
    vop_time_increment_resolution: Tuint16_t;
    vop_coding_type: Tuint8_t;
    vop_fcode_forward: Tuint8_t;
    vop_fcode_backward: Tuint8_t;
    resync_marker_disable: Tuint8_t;
    interlaced: Tuint8_t;
    quant_type: Tuint8_t;
    quarter_sample: Tuint8_t;
    short_video_header: Tuint8_t;
    rounding_control: Tuint8_t;
    alternate_vertical_scan_flag: Tuint8_t;
    top_field_first: Tuint8_t;
    intra_quantizer_matrix: array [0 .. 63] of Tuint8_t;
    non_intra_quantizer_matrix: array [0 .. 63] of Tuint8_t;
  end;

  PVdpPictureInfoDivX4 = ^TVdpPictureInfoDivX4;
  TVdpPictureInfoDivX4 = TVdpPictureInfoMPEG4Part2;

  PVdpPictureInfoDivX5 = ^TVdpPictureInfoDivX5;
  TVdpPictureInfoDivX5 = TVdpPictureInfoMPEG4Part2;

  TVdpDecoderRender = function(decoder: TVdpDecoder; target: TVdpVideoSurface; picture_info: PVdpPictureInfo; bitstream_buffer_count: Tuint32_t; bitstream_buffers: PVdpBitstreamBuffer): TVdpStatus; cdecl;

  PVdpVideoMixerFeature = ^TVdpVideoMixerFeature;
  TVdpVideoMixerFeature = Tuint32_t;

type
  PVdpVideoMixerParameter = ^TVdpVideoMixerParameter;
  TVdpVideoMixerParameter = Tuint32_t;

type
  PVdpVideoMixerAttribute = ^TVdpVideoMixerAttribute;
  TVdpVideoMixerAttribute = Tuint32_t;

type

  TVdpVideoMixerQueryFeatureSupport = function(device: TVdpDevice; feature: TVdpVideoMixerFeature; is_supported: PVdpBool): TVdpStatus; cdecl;

  TVdpVideoMixerQueryParameterSupport = function(device: TVdpDevice; parameter: TVdpVideoMixerParameter; is_supported: PVdpBool): TVdpStatus; cdecl;

  TVdpVideoMixerQueryAttributeSupport = function(device: TVdpDevice; attribute: TVdpVideoMixerAttribute; is_supported: PVdpBool): TVdpStatus; cdecl;

  TVdpVideoMixerQueryParameterValueRange = function(device: TVdpDevice; parameter: TVdpVideoMixerParameter; min_value: pointer; max_value: pointer): TVdpStatus; cdecl;

  TVdpVideoMixerQueryAttributeValueRange = function(device: TVdpDevice; attribute: TVdpVideoMixerAttribute; min_value: pointer; max_value: pointer): TVdpStatus; cdecl;

  PVdpVideoMixer = ^TVdpVideoMixer;
  TVdpVideoMixer = Tuint32_t;

  TVdpVideoMixerCreate = function(device: TVdpDevice; feature_count: Tuint32_t; features: PVdpVideoMixerFeature; parameter_count: Tuint32_t; parameters: PVdpVideoMixerParameter; parameter_values: Ppointer; mixer: PVdpVideoMixer): TVdpStatus; cdecl;

  TVdpVideoMixerSetFeatureEnables = function(mixer: TVdpVideoMixer; feature_count: Tuint32_t; features: PVdpVideoMixerFeature; feature_enables: PVdpBool): TVdpStatus; cdecl;

  TVdpVideoMixerSetAttributeValues = function(mixer: TVdpVideoMixer; attribute_count: Tuint32_t; attributes: PVdpVideoMixerAttribute; attribute_values: Ppointer): TVdpStatus; cdecl;

  TVdpVideoMixerGetFeatureSupport = function(mixer: TVdpVideoMixer; feature_count: Tuint32_t; features: PVdpVideoMixerFeature; feature_supports: PVdpBool): TVdpStatus; cdecl;

  TVdpVideoMixerGetFeatureEnables = function(mixer: TVdpVideoMixer; feature_count: Tuint32_t; features: PVdpVideoMixerFeature; feature_enables: PVdpBool): TVdpStatus; cdecl;

  TVdpVideoMixerGetParameterValues = function(mixer: TVdpVideoMixer; parameter_count: Tuint32_t; parameters: PVdpVideoMixerParameter; parameter_values: Ppointer): TVdpStatus; cdecl;

  TVdpVideoMixerGetAttributeValues = function(mixer: TVdpVideoMixer; attribute_count: Tuint32_t; attributes: PVdpVideoMixerAttribute; attribute_values: Ppointer): TVdpStatus; cdecl;

  TVdpVideoMixerDestroy = function(mixer: TVdpVideoMixer): TVdpStatus; cdecl;

  PVdpVideoMixerPictureStructure = ^TVdpVideoMixerPictureStructure;
  TVdpVideoMixerPictureStructure = (VDP_VIDEO_MIXER_PICTURE_STRUCTURE_TOP_FIELD, VDP_VIDEO_MIXER_PICTURE_STRUCTURE_BOTTOM_FIELD, VDP_VIDEO_MIXER_PICTURE_STRUCTURE_FRAME);

const
  VDP_LAYER_VERSION = 0;

type
  PVdpLayer = ^TVdpLayer;

  TVdpLayer = record
    struct_version: Tuint32_t;
    source_surface: TVdpOutputSurface;
    source_rect: PVdpRect;
    destination_rect: PVdpRect;
  end;

  TVdpVideoMixerRender = function(mixer: TVdpVideoMixer; background_surface: TVdpOutputSurface; background_source_rect: PVdpRect; current_picture_structure: TVdpVideoMixerPictureStructure; video_surface_past_count: Tuint32_t; video_surface_past: PVdpVideoSurface; video_surface_current: TVdpVideoSurface; video_surface_future_count: Tuint32_t; video_surface_future: PVdpVideoSurface; video_source_rect: PVdpRect; destination_surface: TVdpOutputSurface; destination_rect: PVdpRect; destination_video_rect: PVdpRect; layer_count: Tuint32_t; layers: PVdpLayer): TVdpStatus; cdecl;

  PVdpTime = ^TVdpTime;
  TVdpTime = Tuint64_t;

  PVdpPresentationQueueTarget = ^TVdpPresentationQueueTarget;
  TVdpPresentationQueueTarget = Tuint32_t;

  TVdpPresentationQueueTargetDestroy = function(presentation_queue_target: TVdpPresentationQueueTarget): TVdpStatus; cdecl;

  PVdpPresentationQueue = ^TVdpPresentationQueue;
  TVdpPresentationQueue = Tuint32_t;

  TVdpPresentationQueueCreate = function(device: TVdpDevice; presentation_queue_target: TVdpPresentationQueueTarget; presentation_queue: PVdpPresentationQueue): TVdpStatus; cdecl;

  TVdpPresentationQueueDestroy = function(presentation_queue: TVdpPresentationQueue): TVdpStatus; cdecl;

  TVdpPresentationQueueSetBackgroundColor = function(presentation_queue: TVdpPresentationQueue; background_color: PVdpColor): TVdpStatus; cdecl;

  TVdpPresentationQueueGetBackgroundColor = function(presentation_queue: TVdpPresentationQueue; background_color: PVdpColor): TVdpStatus; cdecl;

  TVdpPresentationQueueGetTime = function(presentation_queue: TVdpPresentationQueue; current_time: PVdpTime): TVdpStatus; cdecl;

  TVdpPresentationQueueDisplay = function(presentation_queue: TVdpPresentationQueue; surface: TVdpOutputSurface; clip_width: Tuint32_t; clip_height: Tuint32_t; earliest_presentation_time: TVdpTime): TVdpStatus; cdecl;

  TVdpPresentationQueueBlockUntilSurfaceIdle = function(presentation_queue: TVdpPresentationQueue; surface: TVdpOutputSurface; first_presentation_time: PVdpTime): TVdpStatus; cdecl;

  PVdpPresentationQueueStatus = ^TVdpPresentationQueueStatus;
  TVdpPresentationQueueStatus = (VDP_PRESENTATION_QUEUE_STATUS_IDLE, VDP_PRESENTATION_QUEUE_STATUS_QUEUED, VDP_PRESENTATION_QUEUE_STATUS_VISIBLE);

  TVdpPresentationQueueQuerySurfaceStatus = function(presentation_queue: TVdpPresentationQueue; surface: TVdpOutputSurface; status: PVdpPresentationQueueStatus; first_presentation_time: PVdpTime): TVdpStatus; cdecl;

  TVdpPreemptionCallback = procedure(device: TVdpDevice; context: pointer); cdecl;

  TVdpPreemptionCallbackRegister = function(device: TVdpDevice; callback: TVdpPreemptionCallback; context: pointer): TVdpStatus; cdecl;

  PVdpFuncId = ^TVdpFuncId;
  TVdpFuncId = Tuint32_t;

const
  VDP_FUNC_ID_BASE_WINSYS = $1000;

type

  TVdpGetProcAddress = function(device: TVdpDevice; function_id: TVdpFuncId; function_pointer: Ppointer): TVdpStatus; cdecl;

implementation

end.
