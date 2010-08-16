//
// This unit is part of the GLScene Project, http://glscene.org
//
{: OpenGL1x<p>

	OpenGL 1.x import unit for GLScene. Unit remains "general purpose", but with
   a more "pragmatic" approach :)<p>

   This unit is based on OpenGL12.pas orginally written by Mike Lischke,
   please refer to OpenGL12.pas header.<p>

	<b>History : </b><font size=-1><ul>
      <li>04/03/10 - DanB - Organised core into relevant + deprecated sections,
                            fixed a couple of function params + misc changes.
      <li>12/02/10 - Yar -  Added GL_AMD_vertex_shader_tessellator
      <li>07/02/10 - Yar -  Added GL_NV_primitive_restart
      <li>21/01/10 - DaStr - Bugfixed wglChoosePixelFormatARB() and
                              wglCreatePbufferARB() parameters
      <li>07/01/10 - DaStr - Added WGL_COLOR_SAMPLES_NV (thanks YarUndeoaker)
      <li>25/12/09 - DaStr - Added GL_NV_copy_image, GL_LUMINANCE_INTEGER,
                              GL_LUMINANCE_ALPHA_INTEGER extentions and constants
                             Re-added $region declarations (thanks YarUndeoaker)
      <li>13/12/09 - DaStr - Added missing stdcall/cdecl modifiers
      <li>25/10/09 - DaStr - Added some texture compression extensions and updated
                              glTransformFeedbackVaryings()(thanks YarUndeoaker)
      <li>28/09/09 - DaStr - Added some NVidia-specific extensions (thanks YarUndeoaker)
      <li>30/08/09 - DanB - GLsync changed to NativeInt, fixes to glBindBufferRange calls
      <li>14/08/09 - DanB - Added missing GL_ARB_framebuffer_object extension check + fixed typo
      <li>04/08/09 - DanB - OpenGL 3.1/3.2 support + new ARB extensions added
      <li>28/07/09 - DaStr - Added GL_GEOMETRY_PROGRAM_NV and related extensions
      <li>20/01/08 - DanB - Fix due to Delphi6 not containing UInt64
      <li>05/10/08 - DanB - Moved error handling code here from GLContext.pas
                            OpenGL 3.0 support, new core features + ARB extensions
      <li>23/03/08 - DanB - Added more Vendor/EXT extensions
      <li>17/03/08 - mrqzzz - uncommented some constants "GL_NORMAL_MAP_EXT,..."
                              to keep compatibility with dws2OpenGL1x.
      <li>16/03/08 - DanB - Major rewrite of unit, including:
                            OpenGL 1.3, 1.4, 1.5, 2.0, 2.1 support.
                            removed TRCOptions (not used).
                            moved MRT_BUFFERS constant to GLContext.pas (isn't core openGL).
                            several new ARB extensions added.
                            several new Vendor/EXT exensions added.
                            new function IsOpenGLVersionMet added.
                            restructured unit so extensions are in numerical order.
      <li>17/06/07 - LC - Added GL_ARB_pixel_buffer_object, GL_EXT_pixel_buffer_object
      <li>22/03/07 - DaStr - Removed GetTextureRectangle (had many useless checks)
      <li>16/03/07 - DaStr - Dropped Kylix support in favor of FPC
                             (thanks Burkhard Carstens) (BugTracekrID=1681585)
      <li>09/03/07 - DaStr - Added GL_ARB_draw_buffers (thanks riz)
      <li>03/03/07 - DaStr - Added GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT
      <li>02/03/07 - DaStr - Added GL_[ARB/EXT]_texture_rectangle
                             Added GetTextureRectangle
      <li>10/01/07 - LC - Added GL_EXT_framebuffer_object
      <li>11/09/06 - NC - Added GL_ARB_texture_float, GL_ARB_texture_non_power_of_two
      <li>13/10/04 - NC - Added GL_ATI_draw_buffers
      <li>08/10/04 - LR - Added const in the prototype of the following function for compatibility :
                              TGLUTessCombineProc, TGLUTessCombineDataProc, gluPickMatrix
      gluProject, gluUnProject, gluTessVertex, gluLoadSamplingMatrices
      <li>04/10/04 - NC - Added GL_ATI_texture_float, WGL_ATI_pixel_format_float,
                          WGL_NV_float_buffer, GL_NV_float_buffer
      <li>08/07/04 - LR - Change case for Linux
      <li>05/07/04 - LR - Corrections for Linux. Now glX function are directly load
                          by external action (like for Windows). So i suppress
                          the function LoadLinuxOpenGL.
      <li>28/06/04 - LR - Removed ..\ from the GLScene.inc
      <li>24/06/04 - SG - Added GL_ARB_fragment_program
      <li>17/05/04 - EG - Dropped EXT_vertex_array (assumed as standard)
      <li>06/04/04 - EG - Added GL_ARB_shader_objects, GL_ARB_vertex_shader
                          and GL_ARB_fragment_shader, dropped a few oldies
      <li>13/02/04 - EG - Added GL_NV_texture_rectangle
      <li>18/11/03 - EG - Fixed binding of core extensions, added GL_ARB_depth_texture
                          and GL_ARB_shadow support
      <li>20/09/03 - EG - Added GL_NV_occlusion_query, dropped some more oldies
      <li>09/09/03 - EG - Added GL_ARB_vertex_buffer_object, dropped some oldies
      <li>04/09/03 - EG - Added GL_ARB_vertex_program
      <li>23/07/03 - EG - Creation from OpenGL12.pas "morph": classic OpenGL
                          calls made static, obsolete/rare extensions support
                          dropped
   </ul></font>
}
unit OpenGL1x;

interface

{$i GLScene.inc}

 // DaStr: MULTITHREADOPENGL is defined in GLScene.inc, but you can override it
 // manually here, though I would not reccomend it. This is because other units
 // may depend on this option too. So if you need this option, please use the
 // GLS_MULTITHREAD define in GLScene.inc.
{.$define MULTITHREADOPENGL}
{}
{$hint crossbuilder: the following defines should go into glscene.inc}
{$IFDEF WINDOWS}
  {$DEFINE SUPPORT_WGL}
{$ENDIF}

{$IFDEF UNIX}
  {$IFnDEF DARWIN}
    {$DEFINE SUPPORT_GLX}
  {$ENDIF}
{$ENDIF}



uses
  VectorTypes, SysUtils
  {$IFDEF MSWINDOWS}
    ,Windows
  {$ENDIF }

  {$IFDEF FPC}
    ,dynlibs ,ctypes ,LCLType
  {$ENDIF }

  {$IFDEF unix}
      {$IFDEF darwin}
        ,MacOSAll
      {$ELSE}
        ,X ,Xlib, XUtil
      {$ENDIF}
    , Types
  {$ENDIF}
  ;

type

   PGLChar     = PAnsiChar;
   TGLString   = AnsiString;

   GLenum      = UINT;
   TGLenum     = UINT;
   PGLenum     = ^TGLenum;

   GLboolean   = BYTEBOOL;
   TGLboolean  = BYTEBOOL;
   PGLboolean  = ^TGLboolean;

   GLbitfield  = UINT;
   TGLbitfield = UINT;
   PGLbitfield = ^TGLbitfield;

   GLbyte      = ShortInt;
   TGLbyte     = ShortInt;
   PGLbyte     = ^TGLbyte;

   GLshort     = SmallInt;
   TGLshort    = SmallInt;
   PGLshort    = ^TGLshort;

   GLint       = Integer;
   TGLint      = Integer;
   PGLint      = ^Integer;

   GLsizei     = Integer;
   TGLsizei    = Integer;
   PGLsizei    = ^TGLsizei;

   GLint64 = Int64;
   TGLint64 = Int64;
   PGLint64 = ^TGLInt64;

   GLint64EXT  = Int64;
   TGLint64EXT = Int64;
   PGLint64EXT = ^TGLint64EXT;

   {$IFNDEF GLS_DELPHI_7_DOWN}
   GLuint64 = UInt64;
   TGLuint64= UInt64;
   PGLuint64= ^TGLuint64;

   GLuint64EXT = UInt64;
   TGLuint64EXT= UInt64;
   PGLuint64EXT= ^TGLuint64EXT;
   {$ELSE}
   // fake UInt64 by using Int64 for Delphi5 + 6
   GLuint64 = Int64;
   TGLuint64= Int64;
   PGLuint64= ^TGLuint64;

   GLuint64EXT = Int64;
   TGLuint64EXT= Int64;
   PGLuint64EXT= ^TGLuint64EXT;
   {$ENDIF}

   GLubyte     = Byte;
   TGLubyte    = Byte;
   PGLubyte    = ^TGLubyte;

   GLushort    = Word;
   TGLushort   = Word;
   PGLushort   = ^TGLushort;

   GLuint      = UINT;
   TGLuint     = UINT;
   PGLuint     = ^TGLuint;

   GLfloat     = Single;
   TGLfloat    = Single;
   PGLfloat    = ^TGLfloat;

   GLclampf    = Single;
   TGLclampf   = Single;
   PGLclampf   = ^TGLclampf;

   GLdouble    = Double;
   TGLdouble   = Double;
   PGLdouble   = ^TGLdouble;

   GLclampd    = Double;
   TGLclampd   = Double;
   PGLclampd   = ^TGLclampd;

   GLhandleARB = Cardinal;
   PGLhandleARB = ^GLhandleARB;

   PGLPCharArray = ^PGLChar;

   PGLvoid = Pointer;

   TVector4p = array[0..3] of Pointer;

   PGLPointer = ^Pointer;

   // the size of these depend on platform (32bit or 64bit)
   {$IFDEF FPC}
   GLintptr = PtrInt;
   TGLintptr = PtrInt;
   GLsizeiptr = SizeInt;
   TGLsizeiptr = SizeInt;
   GLsync = PtrInt;
   TGLsync = PtrInt;
   {$ELSE}
   {$IFDEF GLS_DELPHI_2009_UP}
   GLintptr = NativeInt;
   TGLintptr = NativeInt;
   GLsizeiptr = NativeInt;
   TGLsizeiptr = NativeInt;
   GLsync = NativeInt;
   TGLsync = NativeInt;
   {$ELSE}
   GLintptr = Integer;
   TGLintptr = Integer;
   GLsizeiptr = Integer;
   TGLsizeiptr = Integer;
   GLsync = Integer;
   TGLsync = Integer;
   {$ENDIF}
   {$ENDIF}

   // Windows types
   {$IFDEF MSWINDOWS}
   PWGLSwap = ^TWGLSwap;
   _WGLSWAP = packed record
     hdc: HDC;
     uiFlags: UINT;
   end;
   TWGLSwap = _WGLSWAP;
   WGLSWAP = _WGLSWAP;
   HPBUFFERARB= Integer;

   {$ENDIF}

   // Unix types
   {$IFDEF SUPPORT_GLX}
   XPixmap        = TPixmap;
   XFont          = TFont;
   XColormap      = TColormap;
   XWindow        = TWindow;
  
   GLXContext    = Pointer;
   GLXPixmap     = TXID;
   GLXDrawable   = TXID;

   // GLX 1.3 and later
   GLXFBConfig   = Pointer;
   GLXFBConfigID = TXID;
   GLXContextID  = TXID;
   GLXWindow     = TXID;
   GLXPbuffer    = TXID;
   {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL extension feature checks'} {$ENDIF}

{$IFDEF MULTITHREADOPENGL}
threadvar
{$else}
var
{$ENDIF}
   // supported version checks
   GL_VERSION_1_0,
   GL_VERSION_1_1,
   GL_VERSION_1_2,
   GL_VERSION_1_3,
   GL_VERSION_1_4,
   GL_VERSION_1_5,
   GL_VERSION_2_0,
   GL_VERSION_2_1,
   GL_VERSION_2_2, 
   GL_VERSION_3_0,
   GL_VERSION_3_1,
   GL_VERSION_3_2,
   GLU_VERSION_1_1,
   GLU_VERSION_1_2,
   GLU_VERSION_1_3: Boolean;

   // ARB approved OpenGL extension checks
   GL_ARB_color_buffer_float,
   GL_ARB_compatibility,
   GL_ARB_copy_buffer,
   GL_ARB_depth_buffer_float,
   GL_ARB_depth_clamp,
   GL_ARB_depth_texture,
   GL_ARB_draw_buffers,
   GL_ARB_draw_buffers_blend,
   GL_ARB_draw_elements_base_vertex,
   GL_ARB_draw_instanced,
   GL_ARB_fragment_coord_conventions,
   GL_ARB_fragment_program,
   GL_ARB_fragment_program_shadow,
   GL_ARB_fragment_shader,
   GL_ARB_framebuffer_object,
   GL_ARB_framebuffer_sRGB,
   GL_ARB_geometry_shader4,
   GL_ARB_half_float_pixel,
   GL_ARB_half_float_vertex,
   GL_ARB_imaging,
   GL_ARB_instanced_arrays,
   GL_ARB_map_buffer_range,
   GL_ARB_matrix_palette,
   GL_ARB_multisample,
   GL_ARB_multitexture,
   GL_ARB_occlusion_query,
   GL_ARB_pixel_buffer_object,
   GL_ARB_point_parameters,
   GL_ARB_point_sprite,
   GL_ARB_provoking_vertex,
   GL_ARB_sample_shading,
   GL_ARB_seamless_cube_map,
   GL_ARB_shader_texture_lod,
   GL_ARB_shading_language_100,
   GL_ARB_shadow,
   GL_ARB_shadow_ambient,
   GL_ARB_shader_objects,
   GL_ARB_sync,
   GL_ARB_texture_border_clamp,
   GL_ARB_texture_buffer_object,
   GL_ARB_texture_compression,
   GL_ARB_texture_compression_rgtc,
   GL_ARB_texture_cube_map,
   GL_ARB_texture_cube_map_array,
   GL_ARB_texture_env_add,
   GL_ARB_texture_env_combine,
   GL_ARB_texture_env_crossbar,
   GL_ARB_texture_env_dot3,
   GL_ARB_texture_float,
   GL_ARB_texture_gather,
   GL_ARB_texture_mirrored_repeat,
   GL_ARB_texture_multisample,
   GL_ARB_texture_non_power_of_two,
   GL_ARB_texture_query_lod,
   GL_ARB_texture_rectangle,
   GL_ARB_texture_rg,
   GL_ARB_transpose_matrix,
   GL_ARB_uniform_buffer_object,
   GL_ARB_vertex_array_bgra,
   GL_ARB_vertex_array_object,
   GL_ARB_vertex_blend,
   GL_ARB_vertex_buffer_object,
   GL_ARB_vertex_program,
   GL_ARB_vertex_shader,
   GL_ARB_window_pos,

   // Vendor/EXT OpenGL extension checks
   GL_3DFX_multisample,
   GL_3DFX_tbuffer,
   GL_3DFX_texture_compression_FXT1,

   GL_ATI_draw_buffers,
   GL_ATI_texture_compression_3dc,
   GL_ATI_texture_float,
   GL_ATI_texture_mirror_once,

   GL_EXT_abgr,
   GL_EXT_bgra,
   GL_EXT_bindable_uniform,
   GL_EXT_blend_color,
   GL_EXT_blend_equation_separate,
   GL_EXT_blend_func_separate,
   GL_EXT_blend_logic_op,
   GL_EXT_blend_minmax,
   GL_EXT_blend_subtract,
   GL_EXT_Cg_shader,
   GL_EXT_clip_volume_hint,
   GL_EXT_compiled_vertex_array,
   GL_EXT_copy_texture,
   GL_EXT_depth_bounds_test,
   GL_EXT_draw_buffers2,
   GL_EXT_draw_instanced,
   GL_EXT_draw_range_elements,
   GL_EXT_fog_coord,
   GL_EXT_framebuffer_blit,
   GL_EXT_framebuffer_multisample,
   GL_EXT_framebuffer_object,
   GL_EXT_framebuffer_sRGB,
   GL_EXT_geometry_shader4,
   GL_EXT_gpu_program_parameters,
   GL_EXT_gpu_shader4,
   GL_EXT_multi_draw_arrays,
   GL_EXT_multisample,
   GL_EXT_packed_depth_stencil,
   GL_EXT_packed_float,
   GL_EXT_packed_pixels,
   GL_EXT_paletted_texture,
   GL_EXT_pixel_buffer_object,
   GL_EXT_polygon_offset,
   GL_EXT_rescale_normal,
   GL_EXT_secondary_color,
   GL_EXT_separate_specular_color,
   GL_EXT_shadow_funcs,
   GL_EXT_shared_texture_palette,
   GL_EXT_stencil_clear_tag,
   GL_EXT_stencil_two_side,
   GL_EXT_stencil_wrap,
   GL_EXT_texture3D,
   GL_EXT_texture_array,
   GL_EXT_texture_buffer_object,
   GL_EXT_texture_compression_latc,
   GL_EXT_texture_compression_rgtc,
   GL_EXT_texture_compression_s3tc,
   GL_EXT_texture_cube_map,
   GL_EXT_texture_edge_clamp,
   GL_EXT_texture_env_add,
   GL_EXT_texture_env_combine,
   GL_EXT_texture_env_dot3,
   GL_EXT_texture_filter_anisotropic,
   GL_EXT_texture_integer,
   GL_EXT_texture_lod,
   GL_EXT_texture_lod_bias,
   GL_EXT_texture_mirror_clamp,
   GL_EXT_texture_object,
   GL_EXT_texture_rectangle,
   GL_EXT_texture_sRGB,
   GL_EXT_texture_shared_exponent,
   GL_EXT_timer_query,
   GL_EXT_transform_feedback,
   GL_EXT_vertex_array,

   GL_HP_occlusion_test,

   GL_IBM_rasterpos_clip,

   GL_KTX_buffer_region,

   GL_MESA_resize_buffers,

   GL_NV_blend_square,
   GL_NV_conditional_render,
   GL_NV_copy_image,
   GL_NV_depth_buffer_float,
   GL_NV_fence,
   GL_NV_float_buffer,
   GL_NV_fog_distance,
   GL_NV_geometry_program4,
   GL_NV_light_max_exponent,
   GL_NV_multisample_filter_hint,
   GL_NV_occlusion_query,
   GL_NV_point_sprite,
   GL_NV_primitive_restart,
   GL_NV_register_combiners,
   GL_NV_texgen_reflection,
   GL_NV_texture_compression_vtc,
   GL_NV_texture_env_combine4,
   GL_NV_texture_rectangle,
   GL_NV_texture_shader,
   GL_NV_texture_shader2,
   GL_NV_texture_shader3,
   GL_NV_transform_feedback,
   GL_NV_vertex_array_range,
   GL_NV_vertex_array_range2,
   GL_NV_vertex_program,

   GL_SGI_color_matrix,

   GL_SGIS_generate_mipmap,
   GL_SGIS_multisample,
   GL_SGIS_texture_border_clamp,
   GL_SGIS_texture_color_mask,
   GL_SGIS_texture_edge_clamp,
   GL_SGIS_texture_lod,

   GL_SGIX_depth_texture,
   GL_SGIX_shadow,
   GL_SGIX_shadow_ambient,

   GL_AMD_vertex_shader_tessellator,

   GL_WIN_swap_hint,

   // ARB approved WGL extension checks
   WGL_ARB_buffer_region,
   WGL_ARB_create_context,
   WGL_ARB_create_context_profile,
   WGL_ARB_extensions_string,
   WGL_ARB_framebuffer_sRGB,
   WGL_ARB_make_current_read,
   WGL_ARB_multisample,
   WGL_ARB_pbuffer,
   WGL_ARB_pixel_format,
   WGL_ARB_pixel_format_float,
   WGL_ARB_render_texture,

   // Vendor/EXT WGL extension checks
   WGL_ATI_pixel_format_float,

   WGL_EXT_framebuffer_sRGB,
   WGL_EXT_pixel_format_packed_float,
   WGL_EXT_swap_control,

   // GLX extension checks
   GLX_ARB_create_context,
   GLX_ARB_create_context_profile,
   GLX_ARB_framebuffer_sRGB,
   GLX_EXT_framebuffer_sRGB,
   GLX_EXT_fbconfig_packed_float,
   GLX_SGI_swap_control,

   // OpenGL Utility (GLU) extension checks
   GLU_EXT_object_space_tess,
   GLU_EXT_nurbs_tessellator,
   GLU_EXT_Texture: Boolean;

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

const
{$IFDEF MSWINDOWS}
    opengl32 = 'OpenGL32.dll';
    glu32 = 'GLU32.dll';
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF darwin}
    opengl32 = '/System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib';
    glu32 = '/System/Library/Frameworks/OpenGL.framework/Libraries/libGLU.dylib';
  {$ELSE}
    opengl32 = 'libGL.so';
    glu32 = 'libGLU.so';
  {$ENDIF NON-Darwin UNIX}
{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL v1.1 generic constants'} {$ENDIF}
   // ********** GL generic constants **********

   // attribute bits
   GL_DEPTH_BUFFER_BIT                               = $00000100;
   GL_STENCIL_BUFFER_BIT                             = $00000400;
   GL_COLOR_BUFFER_BIT                               = $00004000;

   // boolean values
   GL_FALSE                                          = 0;
   GL_TRUE                                           = 1;

   // primitives
   GL_POINTS                                         = $0000;
   GL_LINES                                          = $0001;
   GL_LINE_LOOP                                      = $0002;
   GL_LINE_STRIP                                     = $0003;
   GL_TRIANGLES                                      = $0004;
   GL_TRIANGLE_STRIP                                 = $0005;
   GL_TRIANGLE_FAN                                   = $0006;

   // AlphaFunction
   GL_NEVER                                          = $0200;
   GL_LESS                                           = $0201;
   GL_EQUAL                                          = $0202;
   GL_LEQUAL                                         = $0203;
   GL_GREATER                                        = $0204;
   GL_NOTEQUAL                                       = $0205;
   GL_GEQUAL                                         = $0206;
   GL_ALWAYS                                         = $0207;

   // blending
   GL_ZERO                                           = 0;
   GL_ONE                                            = 1;
   GL_SRC_COLOR                                      = $0300;
   GL_ONE_MINUS_SRC_COLOR                            = $0301;
   GL_SRC_ALPHA                                      = $0302;
   GL_ONE_MINUS_SRC_ALPHA                            = $0303;
   GL_DST_ALPHA                                      = $0304;
   GL_ONE_MINUS_DST_ALPHA                            = $0305;
   GL_DST_COLOR                                      = $0306;
   GL_ONE_MINUS_DST_COLOR                            = $0307;
   GL_SRC_ALPHA_SATURATE                             = $0308;

   // buffers
   GL_NONE                                           = 0;
   GL_FRONT_LEFT                                     = $0400;
   GL_FRONT_RIGHT                                    = $0401;
   GL_BACK_LEFT                                      = $0402;
   GL_BACK_RIGHT                                     = $0403;
   GL_FRONT                                          = $0404;
   GL_BACK                                           = $0405;
   GL_LEFT                                           = $0406;
   GL_RIGHT                                          = $0407;
   GL_FRONT_AND_BACK                                 = $0408;

   // errors
   GL_NO_ERROR                                       = 0;
   GL_INVALID_ENUM                                   = $0500;
   GL_INVALID_VALUE                                  = $0501;
   GL_INVALID_OPERATION                              = $0502;
   GL_OUT_OF_MEMORY                                  = $0505;

   // FrontFaceDirection
   GL_CW                                             = $0900;
   GL_CCW                                            = $0901;

   // points
   GL_POINT_SIZE                                     = $0B11;
   GL_POINT_SIZE_RANGE                               = $0B12;
   GL_POINT_SIZE_GRANULARITY                         = $0B13;

   // lines
   GL_LINE_SMOOTH                                    = $0B20;
   GL_LINE_WIDTH                                     = $0B21;
   GL_LINE_WIDTH_RANGE                               = $0B22;
   GL_LINE_WIDTH_GRANULARITY                         = $0B23;

   // polygons
   GL_POLYGON_SMOOTH                                 = $0B41;
   GL_CULL_FACE                                      = $0B44;
   GL_CULL_FACE_MODE                                 = $0B45;
   GL_FRONT_FACE                                     = $0B46;

   // depth buffer
   GL_DEPTH_RANGE                                    = $0B70;
   GL_DEPTH_TEST                                     = $0B71;
   GL_DEPTH_WRITEMASK                                = $0B72;
   GL_DEPTH_CLEAR_VALUE                              = $0B73;
   GL_DEPTH_FUNC                                     = $0B74;

   // stenciling
   GL_STENCIL_TEST                                   = $0B90;
   GL_STENCIL_CLEAR_VALUE                            = $0B91;
   GL_STENCIL_FUNC                                   = $0B92;
   GL_STENCIL_VALUE_MASK                             = $0B93;
   GL_STENCIL_FAIL                                   = $0B94;
   GL_STENCIL_PASS_DEPTH_FAIL                        = $0B95;
   GL_STENCIL_PASS_DEPTH_PASS                        = $0B96;
   GL_STENCIL_REF                                    = $0B97;
   GL_STENCIL_WRITEMASK                              = $0B98;

   GL_MATRIX_MODE                                    = $0BA0;

   GL_VIEWPORT                                       = $0BA2;

   // miscellaneous
   GL_DITHER                                         = $0BD0;

   GL_BLEND_DST                                      = $0BE0;
   GL_BLEND_SRC                                      = $0BE1;
   GL_BLEND                                          = $0BE2;

   GL_LOGIC_OP_MODE                                  = $0BF0;
   GL_COLOR_LOGIC_OP                                 = $0BF2;

   GL_DRAW_BUFFER                                    = $0C01;
   GL_READ_BUFFER                                    = $0C02;

   GL_SCISSOR_BOX                                    = $0C10;
   GL_SCISSOR_TEST                                   = $0C11;
   GL_COLOR_CLEAR_VALUE                              = $0C22;
   GL_COLOR_WRITEMASK                                = $0C23;

   GL_DOUBLEBUFFER                                   = $0C32;
   GL_STEREO                                         = $0C33;

   GL_LINE_SMOOTH_HINT                               = $0C52;
   GL_POLYGON_SMOOTH_HINT                            = $0C53;

   // pixel mode, transfer
   GL_UNPACK_SWAP_BYTES                              = $0CF0;
   GL_UNPACK_LSB_FIRST                               = $0CF1;
   GL_UNPACK_ROW_LENGTH                              = $0CF2;
   GL_UNPACK_SKIP_ROWS                               = $0CF3;
   GL_UNPACK_SKIP_PIXELS                             = $0CF4;
   GL_UNPACK_ALIGNMENT                               = $0CF5;
   GL_PACK_SWAP_BYTES                                = $0D00;
   GL_PACK_LSB_FIRST                                 = $0D01;
   GL_PACK_ROW_LENGTH                                = $0D02;
   GL_PACK_SKIP_ROWS                                 = $0D03;
   GL_PACK_SKIP_PIXELS                               = $0D04;
   GL_PACK_ALIGNMENT                                 = $0D05;

   GL_MAX_TEXTURE_SIZE                               = $0D33;
   GL_MAX_VIEWPORT_DIMS                              = $0D3A;
   GL_SUBPIXEL_BITS                                  = $0D50;

   GL_TEXTURE_1D                                     = $0DE0;
   GL_TEXTURE_2D                                     = $0DE1;

   GL_POLYGON_OFFSET_UNITS                           = $2A00;
   GL_POLYGON_OFFSET_POINT                           = $2A01;
   GL_POLYGON_OFFSET_LINE                            = $2A02;
   GL_POLYGON_OFFSET_FILL                            = $8037;
   GL_POLYGON_OFFSET_FACTOR                          = $8038;
   GL_TEXTURE_BINDING_1D                             = $8068;
   GL_TEXTURE_BINDING_2D                             = $8069;

   // texture mapping
   GL_TEXTURE_WIDTH                                  = $1000;
   GL_TEXTURE_HEIGHT                                 = $1001;
   GL_TEXTURE_INTERNAL_FORMAT                        = $1003;
   GL_TEXTURE_BORDER_COLOR                           = $1004;
   GL_TEXTURE_BORDER                                 = $1005;
   GL_TEXTURE_RED_SIZE                               = $805C;
   GL_TEXTURE_GREEN_SIZE                             = $805D;
   GL_TEXTURE_BLUE_SIZE                              = $805E;
   GL_TEXTURE_ALPHA_SIZE                             = $805F;

   // hints
   GL_DONT_CARE                                      = $1100;
   GL_FASTEST                                        = $1101;
   GL_NICEST                                         = $1102;

   // data types
   GL_BYTE                                           = $1400;
   GL_UNSIGNED_BYTE                                  = $1401;
   GL_SHORT                                          = $1402;
   GL_UNSIGNED_SHORT                                 = $1403;
   GL_INT                                            = $1404;
   GL_UNSIGNED_INT                                   = $1405;
   GL_FLOAT                                          = $1406;
   GL_DOUBLE                                         = $140A;

   // logic operations
   GL_CLEAR                                          = $1500;
   GL_AND                                            = $1501;
   GL_AND_REVERSE                                    = $1502;
   GL_COPY                                           = $1503;
   GL_AND_INVERTED                                   = $1504;
   GL_NOOP                                           = $1505;
   GL_XOR                                            = $1506;
   GL_OR                                             = $1507;
   GL_NOR                                            = $1508;
   GL_EQUIV                                          = $1509;
   GL_INVERT                                         = $150A;
   GL_OR_REVERSE                                     = $150B;
   GL_COPY_INVERTED                                  = $150C;
   GL_OR_INVERTED                                    = $150D;
   GL_NAND                                           = $150E;
   GL_SET                                            = $150F;

   GL_TEXTURE                                        = $1702; // (for gl3.h, FBO attachment type)

   // PixelCopyType
   GL_COLOR                                          = $1800;
   GL_DEPTH                                          = $1801;
   GL_STENCIL                                        = $1802;

   // pixel formats
   GL_STENCIL_INDEX                                  = $1901;
   GL_DEPTH_COMPONENT                                = $1902;
   GL_RED                                            = $1903;
   GL_GREEN                                          = $1904;
   GL_BLUE                                           = $1905;
   GL_ALPHA                                          = $1906;
   GL_RGB                                            = $1907;
   GL_RGBA                                           = $1908;

   // PolygonMode
   GL_POINT                                          = $1B00;
   GL_LINE                                           = $1B01;
   GL_FILL                                           = $1B02;

   // StencilOp
   GL_KEEP                                           = $1E00;
   GL_REPLACE                                        = $1E01;
   GL_INCR                                           = $1E02;
   GL_DECR                                           = $1E03;

   // implementation strings
   GL_VENDOR                                         = $1F00;
   GL_RENDERER                                       = $1F01;
   GL_VERSION                                        = $1F02;
   GL_EXTENSIONS                                     = $1F03;

   GL_NEAREST                                        = $2600;
   GL_LINEAR                                         = $2601;
   GL_NEAREST_MIPMAP_NEAREST                         = $2700;
   GL_LINEAR_MIPMAP_NEAREST                          = $2701;
   GL_NEAREST_MIPMAP_LINEAR                          = $2702;
   GL_LINEAR_MIPMAP_LINEAR                           = $2703;
   GL_TEXTURE_MAG_FILTER                             = $2800;
   GL_TEXTURE_MIN_FILTER                             = $2801;
   GL_TEXTURE_WRAP_S                                 = $2802;
   GL_TEXTURE_WRAP_T                                 = $2803;
   GL_PROXY_TEXTURE_1D                               = $8063;
   GL_PROXY_TEXTURE_2D                               = $8064;
   GL_REPEAT                                         = $2901;

   // pixel formats
   GL_R3_G3_B2                                       = $2A10;
   GL_RGB4                                           = $804F;
   GL_RGB5                                           = $8050;
   GL_RGB8                                           = $8051;
   GL_RGB10                                          = $8052;
   GL_RGB12                                          = $8053;
   GL_RGB16                                          = $8054;
   GL_RGBA2                                          = $8055;
   GL_RGBA4                                          = $8056;
   GL_RGB5_A1                                        = $8057;
   GL_RGBA8                                          = $8058;
   GL_RGB10_A2                                       = $8059;
   GL_RGBA12                                         = $805A;
   GL_RGBA16                                         = $805B;

   //crossbuilder: check if these are needed or if there are GL_* versions 
   UNSIGNED_BYTE_3_3_2                               = $8032; // GL 1.2
   UNSIGNED_BYTE_2_3_3_REV                           = $8362; // GL 1.2
   UNSIGNED_SHORT_5_6_5                              = $8363; // GL 1.2
   UNSIGNED_SHORT_5_6_5_REV                          = $8364; // GL 1.2
   UNSIGNED_SHORT_4_4_4_4                            = $8033; // GL 1.2
   UNSIGNED_SHORT_4_4_4_4_REV                        = $8365; // GL 1.2
   UNSIGNED_SHORT_5_5_5_1                            = $8034; // GL 1.2
   UNSIGNED_SHORT_1_5_5_5_REV                        = $8366; // GL 1.2
   UNSIGNED_INT_8_8_8_8                              = $8035; // GL 1.2
   UNSIGNED_INT_8_8_8_8_REV                          = $8367; // GL 1.2
   UNSIGNED_INT_10_10_10_2                           = $8036; // GL 1.2
   UNSIGNED_INT_2_10_10_10_REV                       = $8368; // GL 1.2

   {$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 1.1 deprecated'} {$ENDIF}
   // attribute bits
   GL_CURRENT_BIT                                    = $00000001 {deprecated};
   GL_POINT_BIT                                      = $00000002 {deprecated};
   GL_LINE_BIT                                       = $00000004 {deprecated};
   GL_POLYGON_BIT                                    = $00000008 {deprecated};
   GL_POLYGON_STIPPLE_BIT                            = $00000010 {deprecated};
   GL_PIXEL_MODE_BIT                                 = $00000020 {deprecated};
   GL_LIGHTING_BIT                                   = $00000040 {deprecated};
   GL_FOG_BIT                                        = $00000080 {deprecated};
   GL_ACCUM_BUFFER_BIT                               = $00000200 {deprecated};
   GL_VIEWPORT_BIT                                   = $00000800 {deprecated};
   GL_TRANSFORM_BIT                                  = $00001000 {deprecated};
   GL_ENABLE_BIT                                     = $00002000 {deprecated};
   GL_HINT_BIT                                       = $00008000 {deprecated};
   GL_EVAL_BIT                                       = $00010000 {deprecated};
   GL_LIST_BIT                                       = $00020000 {deprecated};
   GL_TEXTURE_BIT                                    = $00040000 {deprecated};
   GL_SCISSOR_BIT                                    = $00080000 {deprecated};
   // changed from $000FFFFF to $FFFFFFFF in OpenGL 1.3
   GL_ALL_ATTRIB_BITS                                = $FFFFFFFF {deprecated};

   // client attribute bits
   GL_CLIENT_PIXEL_STORE_BIT                         = $00000001 {deprecated};
   GL_CLIENT_VERTEX_ARRAY_BIT                        = $00000002 {deprecated};
   GL_CLIENT_ALL_ATTRIB_BITS                         = $FFFFFFFF {deprecated};

   // primitives
   GL_QUADS                                          = $0007 {deprecated};
   GL_QUAD_STRIP                                     = $0008 {deprecated};
   GL_POLYGON                                        = $0009 {deprecated};

   // accumulation buffer
   GL_ACCUM                                          = $0100 {deprecated};
   GL_LOAD                                           = $0101 {deprecated};
   GL_RETURN                                         = $0102 {deprecated};
   GL_MULT                                           = $0103 {deprecated};
   GL_ADD                                            = $0104 {deprecated};

   // buffers
   GL_AUX0                                           = $0409 {deprecated};
   GL_AUX1                                           = $040A {deprecated};
   GL_AUX2                                           = $040B {deprecated};
   GL_AUX3                                           = $040C {deprecated};

   // errors
   GL_STACK_OVERFLOW                                 = $0503 {deprecated};
   GL_STACK_UNDERFLOW                                = $0504 {deprecated};

   // feedback types
   GL_2D                                             = $0600 {deprecated};
   GL_3D                                             = $0601 {deprecated};
   GL_3D_COLOR                                       = $0602 {deprecated};
   GL_3D_COLOR_TEXTURE                               = $0603 {deprecated};
   GL_4D_COLOR_TEXTURE                               = $0604 {deprecated};

   // feedback tokens
   GL_PASS_THROUGH_TOKEN                             = $0700 {deprecated};
   GL_POINT_TOKEN                                    = $0701 {deprecated};
   GL_LINE_TOKEN                                     = $0702 {deprecated};
   GL_POLYGON_TOKEN                                  = $0703 {deprecated};
   GL_BITMAP_TOKEN                                   = $0704 {deprecated};
   GL_DRAW_PIXEL_TOKEN                               = $0705 {deprecated};
   GL_COPY_PIXEL_TOKEN                               = $0706 {deprecated};
   GL_LINE_RESET_TOKEN                               = $0707 {deprecated};

   // fog
   GL_EXP                                            = $0800 {deprecated};
   GL_EXP2                                           = $0801 {deprecated};

   // evaluators
   GL_COEFF                                          = $0A00 {deprecated};
   GL_ORDER                                          = $0A01 {deprecated};
   GL_DOMAIN                                         = $0A02 {deprecated};

   // gets
   GL_CURRENT_COLOR                                  = $0B00 {deprecated};
   GL_CURRENT_INDEX                                  = $0B01 {deprecated};
   GL_CURRENT_NORMAL                                 = $0B02 {deprecated};
   GL_CURRENT_TEXTURE_COORDS                         = $0B03 {deprecated};
   GL_CURRENT_RASTER_COLOR                           = $0B04 {deprecated};
   GL_CURRENT_RASTER_INDEX                           = $0B05 {deprecated};
   GL_CURRENT_RASTER_TEXTURE_COORDS                  = $0B06 {deprecated};
   GL_CURRENT_RASTER_POSITION                        = $0B07 {deprecated};
   GL_CURRENT_RASTER_POSITION_VALID                  = $0B08 {deprecated};
   GL_CURRENT_RASTER_DISTANCE                        = $0B09 {deprecated};

   // points
   GL_POINT_SMOOTH                                   = $0B10 {deprecated};

   // lines
   GL_LINE_STIPPLE                                   = $0B24 {deprecated};
   GL_LINE_STIPPLE_PATTERN                           = $0B25 {deprecated};
   GL_LINE_STIPPLE_REPEAT                            = $0B26 {deprecated};

   // display lists
   GL_LIST_MODE                                      = $0B30 {deprecated};
   GL_MAX_LIST_NESTING                               = $0B31 {deprecated};
   GL_LIST_BASE                                      = $0B32 {deprecated};
   GL_LIST_INDEX                                     = $0B33 {deprecated};

   // polygons
   // DanB - not sure "GL_POLYGON_MODE" should be deprecated, but it is marked
   // deprecated in OpenGL spec, so will put it here for now
   GL_POLYGON_MODE                                   = $0B40 {deprecated};
   GL_POLYGON_STIPPLE                                = $0B42 {deprecated};
   GL_EDGE_FLAG                                      = $0B43 {deprecated};

   // lighting
   GL_LIGHTING                                       = $0B50 {deprecated};
   GL_LIGHT_MODEL_LOCAL_VIEWER                       = $0B51 {deprecated};
   GL_LIGHT_MODEL_TWO_SIDE                           = $0B52 {deprecated};
   GL_LIGHT_MODEL_AMBIENT                            = $0B53 {deprecated};
   GL_SHADE_MODEL                                    = $0B54 {deprecated};

   // color material
   GL_COLOR_MATERIAL_FACE                            = $0B55 {deprecated};
   GL_COLOR_MATERIAL_PARAMETER                       = $0B56 {deprecated};
   GL_COLOR_MATERIAL                                 = $0B57 {deprecated};

   // fog
   GL_FOG                                            = $0B60 {deprecated};
   GL_FOG_INDEX                                      = $0B61 {deprecated};
   GL_FOG_DENSITY                                    = $0B62 {deprecated};
   GL_FOG_START                                      = $0B63 {deprecated};
   GL_FOG_END                                        = $0B64 {deprecated};
   GL_FOG_MODE                                       = $0B65 {deprecated};
   GL_FOG_COLOR                                      = $0B66 {deprecated};

   GL_ACCUM_CLEAR_VALUE                              = $0B80 {deprecated};

   GL_NORMALIZE                                      = $0BA1 {deprecated};
   GL_MODELVIEW_STACK_DEPTH                          = $0BA3 {deprecated};
   GL_PROJECTION_STACK_DEPTH                         = $0BA4 {deprecated};
   GL_TEXTURE_STACK_DEPTH                            = $0BA5 {deprecated};
   GL_MODELVIEW_MATRIX                               = $0BA6 {deprecated};
   GL_PROJECTION_MATRIX                              = $0BA7 {deprecated};
   GL_TEXTURE_MATRIX                                 = $0BA8 {deprecated};
   GL_ATTRIB_STACK_DEPTH                             = $0BB0 {deprecated};
   GL_CLIENT_ATTRIB_STACK_DEPTH                      = $0BB1 {deprecated};

   // alpha testing
   GL_ALPHA_TEST                                     = $0BC0 {deprecated};
   GL_ALPHA_TEST_FUNC                                = $0BC1 {deprecated};
   GL_ALPHA_TEST_REF                                 = $0BC2 {deprecated};

   GL_INDEX_LOGIC_OP                                 = $0BF1 {deprecated};
   GL_LOGIC_OP                                       = $0BF1 {deprecated};

   GL_AUX_BUFFERS                                    = $0C00 {deprecated};

   GL_INDEX_CLEAR_VALUE                              = $0C20 {deprecated};
   GL_INDEX_WRITEMASK                                = $0C21 {deprecated};

   GL_INDEX_MODE                                     = $0C30 {deprecated};
   GL_RGBA_MODE                                      = $0C31 {deprecated};

   GL_RENDER_MODE                                    = $0C40 {deprecated};
   GL_PERSPECTIVE_CORRECTION_HINT                    = $0C50 {deprecated};
   GL_POINT_SMOOTH_HINT                              = $0C51 {deprecated};

   GL_FOG_HINT                                       = $0C54 {deprecated};
   GL_TEXTURE_GEN_S                                  = $0C60 {deprecated};
   GL_TEXTURE_GEN_T                                  = $0C61 {deprecated};
   GL_TEXTURE_GEN_R                                  = $0C62 {deprecated};
   GL_TEXTURE_GEN_Q                                  = $0C63 {deprecated};

   // pixel mode, transfer
   GL_PIXEL_MAP_I_TO_I                               = $0C70 {deprecated};
   GL_PIXEL_MAP_S_TO_S                               = $0C71 {deprecated};
   GL_PIXEL_MAP_I_TO_R                               = $0C72 {deprecated};
   GL_PIXEL_MAP_I_TO_G                               = $0C73 {deprecated};
   GL_PIXEL_MAP_I_TO_B                               = $0C74 {deprecated};
   GL_PIXEL_MAP_I_TO_A                               = $0C75 {deprecated};
   GL_PIXEL_MAP_R_TO_R                               = $0C76 {deprecated};
   GL_PIXEL_MAP_G_TO_G                               = $0C77 {deprecated};
   GL_PIXEL_MAP_B_TO_B                               = $0C78 {deprecated};
   GL_PIXEL_MAP_A_TO_A                               = $0C79 {deprecated};
   GL_PIXEL_MAP_I_TO_I_SIZE                          = $0CB0 {deprecated};
   GL_PIXEL_MAP_S_TO_S_SIZE                          = $0CB1 {deprecated};
   GL_PIXEL_MAP_I_TO_R_SIZE                          = $0CB2 {deprecated};
   GL_PIXEL_MAP_I_TO_G_SIZE                          = $0CB3 {deprecated};
   GL_PIXEL_MAP_I_TO_B_SIZE                          = $0CB4 {deprecated};
   GL_PIXEL_MAP_I_TO_A_SIZE                          = $0CB5 {deprecated};
   GL_PIXEL_MAP_R_TO_R_SIZE                          = $0CB6 {deprecated};
   GL_PIXEL_MAP_G_TO_G_SIZE                          = $0CB7 {deprecated};
   GL_PIXEL_MAP_B_TO_B_SIZE                          = $0CB8 {deprecated};
   GL_PIXEL_MAP_A_TO_A_SIZE                          = $0CB9 {deprecated};

   GL_MAP_COLOR                                      = $0D10 {deprecated};
   GL_MAP_STENCIL                                    = $0D11 {deprecated};
   GL_INDEX_SHIFT                                    = $0D12 {deprecated};
   GL_INDEX_OFFSET                                   = $0D13 {deprecated};
   GL_RED_SCALE                                      = $0D14 {deprecated};
   GL_RED_BIAS                                       = $0D15 {deprecated};
   GL_ZOOM_X                                         = $0D16 {deprecated};
   GL_ZOOM_Y                                         = $0D17 {deprecated};
   GL_GREEN_SCALE                                    = $0D18 {deprecated};
   GL_GREEN_BIAS                                     = $0D19 {deprecated};
   GL_BLUE_SCALE                                     = $0D1A {deprecated};
   GL_BLUE_BIAS                                      = $0D1B {deprecated};
   GL_ALPHA_SCALE                                    = $0D1C {deprecated};
   GL_ALPHA_BIAS                                     = $0D1D {deprecated};
   GL_DEPTH_SCALE                                    = $0D1E {deprecated};
   GL_DEPTH_BIAS                                     = $0D1F {deprecated};
   GL_MAX_EVAL_ORDER                                 = $0D30 {deprecated};
   GL_MAX_LIGHTS                                     = $0D31 {deprecated};
   GL_MAX_CLIP_PLANES                                = $0D32 {deprecated};

   GL_MAX_PIXEL_MAP_TABLE                            = $0D34 {deprecated};
   GL_MAX_ATTRIB_STACK_DEPTH                         = $0D35 {deprecated};
   GL_MAX_MODELVIEW_STACK_DEPTH                      = $0D36 {deprecated};
   GL_MAX_NAME_STACK_DEPTH                           = $0D37 {deprecated};
   GL_MAX_PROJECTION_STACK_DEPTH                     = $0D38 {deprecated};
   GL_MAX_TEXTURE_STACK_DEPTH                        = $0D39 {deprecated};

   GL_MAX_CLIENT_ATTRIB_STACK_DEPTH                  = $0D3B {deprecated};
   GL_INDEX_BITS                                     = $0D51 {deprecated};
   GL_RED_BITS                                       = $0D52 {deprecated};
   GL_GREEN_BITS                                     = $0D53 {deprecated};
   GL_BLUE_BITS                                      = $0D54 {deprecated};
   GL_ALPHA_BITS                                     = $0D55 {deprecated};
   GL_DEPTH_BITS                                     = $0D56 {deprecated};
   GL_STENCIL_BITS                                   = $0D57 {deprecated};
   GL_ACCUM_RED_BITS                                 = $0D58 {deprecated};
   GL_ACCUM_GREEN_BITS                               = $0D59 {deprecated};
   GL_ACCUM_BLUE_BITS                                = $0D5A {deprecated};
   GL_ACCUM_ALPHA_BITS                               = $0D5B {deprecated};
   GL_NAME_STACK_DEPTH                               = $0D70 {deprecated};
   GL_AUTO_NORMAL                                    = $0D80 {deprecated};
   GL_MAP1_COLOR_4                                   = $0D90 {deprecated};
   GL_MAP1_INDEX                                     = $0D91 {deprecated};
   GL_MAP1_NORMAL                                    = $0D92 {deprecated};
   GL_MAP1_TEXTURE_COORD_1                           = $0D93 {deprecated};
   GL_MAP1_TEXTURE_COORD_2                           = $0D94 {deprecated};
   GL_MAP1_TEXTURE_COORD_3                           = $0D95 {deprecated};
   GL_MAP1_TEXTURE_COORD_4                           = $0D96 {deprecated};
   GL_MAP1_VERTEX_3                                  = $0D97 {deprecated};
   GL_MAP1_VERTEX_4                                  = $0D98 {deprecated};
   GL_MAP2_COLOR_4                                   = $0DB0 {deprecated};
   GL_MAP2_INDEX                                     = $0DB1 {deprecated};
   GL_MAP2_NORMAL                                    = $0DB2 {deprecated};
   GL_MAP2_TEXTURE_COORD_1                           = $0DB3 {deprecated};
   GL_MAP2_TEXTURE_COORD_2                           = $0DB4 {deprecated};
   GL_MAP2_TEXTURE_COORD_3                           = $0DB5 {deprecated};
   GL_MAP2_TEXTURE_COORD_4                           = $0DB6 {deprecated};
   GL_MAP2_VERTEX_3                                  = $0DB7 {deprecated};
   GL_MAP2_VERTEX_4                                  = $0DB8 {deprecated};
   GL_MAP1_GRID_DOMAIN                               = $0DD0 {deprecated};
   GL_MAP1_GRID_SEGMENTS                             = $0DD1 {deprecated};
   GL_MAP2_GRID_DOMAIN                               = $0DD2 {deprecated};
   GL_MAP2_GRID_SEGMENTS                             = $0DD3 {deprecated};

   // feedback buffer
   GL_FEEDBACK_BUFFER_POINTER                        = $0DF0 {deprecated};
   GL_FEEDBACK_BUFFER_SIZE                           = $0DF1 {deprecated};
   GL_FEEDBACK_BUFFER_TYPE                           = $0DF2 {deprecated};

   GL_SELECTION_BUFFER_POINTER                       = $0DF3 {deprecated};
   GL_SELECTION_BUFFER_SIZE                          = $0DF4 {deprecated};

   GL_TEXTURE_COMPONENTS                             = $1003 {deprecated};
   GL_TEXTURE_LUMINANCE_SIZE                         = $8060 {deprecated};
   GL_TEXTURE_INTENSITY_SIZE                         = $8061 {deprecated};
   GL_TEXTURE_PRIORITY                               = $8066 {deprecated};
   GL_TEXTURE_RESIDENT                               = $8067 {deprecated};

   // lighting
   GL_AMBIENT                                        = $1200 {deprecated};
   GL_DIFFUSE                                        = $1201 {deprecated};
   GL_SPECULAR                                       = $1202 {deprecated};
   GL_POSITION                                       = $1203 {deprecated};
   GL_SPOT_DIRECTION                                 = $1204 {deprecated};
   GL_SPOT_EXPONENT                                  = $1205 {deprecated};
   GL_SPOT_CUTOFF                                    = $1206 {deprecated};
   GL_CONSTANT_ATTENUATION                           = $1207 {deprecated};
   GL_LINEAR_ATTENUATION                             = $1208 {deprecated};
   GL_QUADRATIC_ATTENUATION                          = $1209 {deprecated};

   // display lists
   GL_COMPILE                                        = $1300 {deprecated};
   GL_COMPILE_AND_EXECUTE                            = $1301 {deprecated};

   // data types
   GL_2_BYTES                                        = $1407 {deprecated};
   GL_3_BYTES                                        = $1408 {deprecated};
   GL_4_BYTES                                        = $1409 {deprecated};
   GL_DOUBLE_EXT                                     = $140A {deprecated};

   GL_EMISSION                                       = $1600 {deprecated};
   GL_SHININESS                                      = $1601 {deprecated};
   GL_AMBIENT_AND_DIFFUSE                            = $1602 {deprecated};
   GL_COLOR_INDEXES                                  = $1603 {deprecated};

   // matrix modes
   GL_MODELVIEW                                      = $1700 {deprecated};
   GL_PROJECTION                                     = $1701 {deprecated};

   // pixel formats
   GL_COLOR_INDEX                                    = $1900 {deprecated};
   GL_LUMINANCE                                      = $1909 {deprecated};
   GL_LUMINANCE_ALPHA                                = $190A {deprecated};

   // pixel type
   GL_BITMAP                                         = $1A00 {deprecated};

   // rendering modes
   GL_RENDER                                         = $1C00 {deprecated};
   GL_FEEDBACK                                       = $1C01 {deprecated};
   GL_SELECT                                         = $1C02 {deprecated};

   GL_FLAT                                           = $1D00 {deprecated};
   GL_SMOOTH                                         = $1D01 {deprecated};

   GL_S                                              = $2000 {deprecated};
   GL_T                                              = $2001 {deprecated};
   GL_R                                              = $2002 {deprecated};
   GL_Q                                              = $2003 {deprecated};
   GL_MODULATE                                       = $2100 {deprecated};
   GL_DECAL                                          = $2101 {deprecated};
   GL_TEXTURE_ENV_MODE                               = $2200 {deprecated};
   GL_TEXTURE_ENV_COLOR                              = $2201 {deprecated};
   GL_TEXTURE_ENV                                    = $2300 {deprecated};
   GL_EYE_LINEAR                                     = $2400 {deprecated};
   GL_OBJECT_LINEAR                                  = $2401 {deprecated};
   GL_SPHERE_MAP                                     = $2402 {deprecated};
   GL_TEXTURE_GEN_MODE                               = $2500 {deprecated};
   GL_OBJECT_PLANE                                   = $2501 {deprecated};
   GL_EYE_PLANE                                      = $2502 {deprecated};

   GL_CLAMP                                          = $2900 {deprecated};

   // pixel formats
   GL_ALPHA4                                         = $803B {deprecated};
   GL_ALPHA8                                         = $803C {deprecated};
   GL_ALPHA12                                        = $803D {deprecated};
   GL_ALPHA16                                        = $803E {deprecated};
   GL_LUMINANCE4                                     = $803F {deprecated};
   GL_LUMINANCE8                                     = $8040 {deprecated};
   GL_LUMINANCE12                                    = $8041 {deprecated};
   GL_LUMINANCE16                                    = $8042 {deprecated};
   GL_LUMINANCE4_ALPHA4                              = $8043 {deprecated};
   GL_LUMINANCE6_ALPHA2                              = $8044 {deprecated};
   GL_LUMINANCE8_ALPHA8                              = $8045 {deprecated};
   GL_LUMINANCE12_ALPHA4                             = $8046 {deprecated};
   GL_LUMINANCE12_ALPHA12                            = $8047 {deprecated};
   GL_LUMINANCE16_ALPHA16                            = $8048 {deprecated};
   GL_INTENSITY                                      = $8049 {deprecated};
   GL_INTENSITY4                                     = $804A {deprecated};
   GL_INTENSITY8                                     = $804B {deprecated};
   GL_INTENSITY12                                    = $804C {deprecated};
   GL_INTENSITY16                                    = $804D {deprecated};

   GL_VERTEX_ARRAY                                   = $8074 {deprecated};
   GL_NORMAL_ARRAY                                   = $8075 {deprecated};
   GL_COLOR_ARRAY                                    = $8076 {deprecated};
   GL_INDEX_ARRAY                                    = $8077 {deprecated};
   GL_TEXTURE_COORD_ARRAY                            = $8078 {deprecated};
   GL_EDGE_FLAG_ARRAY                                = $8079 {deprecated};
   GL_VERTEX_ARRAY_SIZE                              = $807A {deprecated};
   GL_VERTEX_ARRAY_TYPE                              = $807B {deprecated};
   GL_VERTEX_ARRAY_STRIDE                            = $807C {deprecated};
   GL_NORMAL_ARRAY_TYPE                              = $807E {deprecated};
   GL_NORMAL_ARRAY_STRIDE                            = $807F {deprecated};
   GL_COLOR_ARRAY_SIZE                               = $8081 {deprecated};
   GL_COLOR_ARRAY_TYPE                               = $8082 {deprecated};
   GL_COLOR_ARRAY_STRIDE                             = $8083 {deprecated};
   GL_INDEX_ARRAY_TYPE                               = $8085 {deprecated};
   GL_INDEX_ARRAY_STRIDE                             = $8086 {deprecated};
   GL_TEXTURE_COORD_ARRAY_SIZE                       = $8088 {deprecated};
   GL_TEXTURE_COORD_ARRAY_TYPE                       = $8089 {deprecated};
   GL_TEXTURE_COORD_ARRAY_STRIDE                     = $808A {deprecated};
   GL_EDGE_FLAG_ARRAY_STRIDE                         = $808C {deprecated};

   // vertex arrays
   GL_VERTEX_ARRAY_POINTER                           = $808E {deprecated};
   GL_NORMAL_ARRAY_POINTER                           = $808F {deprecated};
   GL_COLOR_ARRAY_POINTER                            = $8090 {deprecated};
   GL_INDEX_ARRAY_POINTER                            = $8091 {deprecated};
   GL_TEXTURE_COORD_ARRAY_POINTER                    = $8092 {deprecated};
   GL_EDGE_FLAG_ARRAY_POINTER                        = $8093 {deprecated};

   // interleaved arrays formats
   GL_V2F                                            = $2A20 {deprecated};
   GL_V3F                                            = $2A21 {deprecated};
   GL_C4UB_V2F                                       = $2A22 {deprecated};
   GL_C4UB_V3F                                       = $2A23 {deprecated};
   GL_C3F_V3F                                        = $2A24 {deprecated};
   GL_N3F_V3F                                        = $2A25 {deprecated};
   GL_C4F_N3F_V3F                                    = $2A26 {deprecated};
   GL_T2F_V3F                                        = $2A27 {deprecated};
   GL_T4F_V4F                                        = $2A28 {deprecated};
   GL_T2F_C4UB_V3F                                   = $2A29 {deprecated};
   GL_T2F_C3F_V3F                                    = $2A2A {deprecated};
   GL_T2F_N3F_V3F                                    = $2A2B {deprecated};
   GL_T2F_C4F_N3F_V3F                                = $2A2C {deprecated};
   GL_T4F_C4F_N3F_V4F                                = $2A2D {deprecated};

   // clip planes
   GL_CLIP_PLANE0                                    = $3000 {deprecated};
   GL_CLIP_PLANE1                                    = $3001 {deprecated};
   GL_CLIP_PLANE2                                    = $3002 {deprecated};
   GL_CLIP_PLANE3                                    = $3003 {deprecated};
   GL_CLIP_PLANE4                                    = $3004 {deprecated};
   GL_CLIP_PLANE5                                    = $3005 {deprecated};

   // lights
   GL_LIGHT0                                         = $4000 {deprecated};
   GL_LIGHT1                                         = $4001 {deprecated};
   GL_LIGHT2                                         = $4002 {deprecated};
   GL_LIGHT3                                         = $4003 {deprecated};
   GL_LIGHT4                                         = $4004 {deprecated};
   GL_LIGHT5                                         = $4005 {deprecated};
   GL_LIGHT6                                         = $4006 {deprecated};
   GL_LIGHT7                                         = $4007 {deprecated};

   {$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}    {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}    {$region 'New core constants in OpenGL v1.2'} {$ENDIF}

   // promoted to core v1.2 from GL_EXT_packed_pixels (EXT #23)
   GL_UNSIGNED_BYTE_3_3_2                            = $8032;
   GL_UNSIGNED_SHORT_4_4_4_4                         = $8033;
   GL_UNSIGNED_SHORT_5_5_5_1                         = $8034;
   GL_UNSIGNED_INT_8_8_8_8                           = $8035;
   GL_UNSIGNED_INT_10_10_10_2                        = $8036;

   // promoted to core v1.2 from GL_EXT_texture3D (EXT #6)
   GL_PACK_SKIP_IMAGES                               = $806B;
   GL_PACK_IMAGE_HEIGHT                              = $806C;
   GL_UNPACK_SKIP_IMAGES                             = $806D;
   GL_UNPACK_IMAGE_HEIGHT                            = $806E;
   GL_TEXTURE_3D                                     = $806F;
   GL_TEXTURE_BINDING_3D				                     = $806A;
   GL_PROXY_TEXTURE_3D                               = $8070;
   GL_TEXTURE_DEPTH                                  = $8071;
   GL_TEXTURE_WRAP_R                                 = $8072;
   GL_MAX_3D_TEXTURE_SIZE                            = $8073;

   // new for OpenGL 1.2
   GL_UNSIGNED_BYTE_2_3_3_REV                        = $8362;
   GL_UNSIGNED_SHORT_5_6_5                           = $8363;
   GL_UNSIGNED_SHORT_5_6_5_REV                       = $8364;
   GL_UNSIGNED_SHORT_4_4_4_4_REV                     = $8365;
   GL_UNSIGNED_SHORT_1_5_5_5_REV                     = $8366;
   GL_UNSIGNED_INT_8_8_8_8_REV                       = $8367;
   GL_UNSIGNED_INT_2_10_10_10_REV                    = $8368;
   
   // promoted to core v1.2 from GL_EXT_bgra (EXT #129)
   GL_BGR                                            = $80E0;
   GL_BGRA                                           = $80E1;

   // promoted to core v1.2 from GL_EXT_draw_range_elements (EXT #112)
   GL_MAX_ELEMENTS_VERTICES                          = $80E8;
   GL_MAX_ELEMENTS_INDICES                           = $80E9;

   // promoted to core v1.2 from GL_SGIS_texture_edge_clamp (EXT #35)
   GL_CLAMP_TO_EDGE                                  = $812F;

   // promoted to core v1.2 from GL_SGIS_texture_lod (EXT #24)
   GL_TEXTURE_MIN_LOD                                = $813A;
   GL_TEXTURE_MAX_LOD                                = $813B;
   GL_TEXTURE_BASE_LEVEL                             = $813C;
   GL_TEXTURE_MAX_LEVEL                              = $813D;

   // new 1.2 naming scheme (POINT => SMOOTH_POINT)
   GL_SMOOTH_POINT_SIZE_RANGE                        = $0B12;
   GL_SMOOTH_POINT_SIZE_GRANULARITY			             = $0B13;
   GL_SMOOTH_LINE_WIDTH_RANGE				                 = $0B22;
   GL_SMOOTH_LINE_WIDTH_GRANULARITY		               = $0B23;
   GL_ALIASED_LINE_WIDTH_RANGE                       = $846E;

   // Blending ( 1.2 ARB imaging)
   // promoted to core v1.2 from GL_EXT_blend_color (EXT #2)
   GL_CONSTANT_COLOR                                 = $8001;
   GL_ONE_MINUS_CONSTANT_COLOR                       = $8002;
   GL_CONSTANT_ALPHA                                 = $8003;
   GL_ONE_MINUS_CONSTANT_ALPHA                       = $8004;
   GL_BLEND_COLOR                                    = $8005;

   // promoted to core v1.2 from GL_EXT_blend_minmax (EXT #37)
   GL_FUNC_ADD                                       = $8006;
   GL_MIN                                            = $8007;
   GL_MAX                                            = $8008;
   GL_BLEND_EQUATION                                 = $8009;

   // promoted to core v1.2 from GL_EXT_blend_subtract (EXT #38)
   GL_FUNC_SUBTRACT                                  = $800A;
   GL_FUNC_REVERSE_SUBTRACT                          = $800B;

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 1.2 deprecated'} {$ENDIF}
   // {deprecated}
   // promoted to core v1.2 from GL_EXT_rescale_normal (EXT #27)
   GL_RESCALE_NORMAL                                 = $803A {deprecated};

   // promoted to core v1.2 from EXT_separate_specular_color (EXT #144)
   GL_LIGHT_MODEL_COLOR_CONTROL                      = $81F8 {deprecated};
   GL_SINGLE_COLOR                                   = $81F9 {deprecated};
   GL_SEPARATE_SPECULAR_COLOR                        = $81FA {deprecated};

   // new 1.2 naming scheme (POINT => SMOOTH_POINT)
   GL_ALIASED_POINT_SIZE_RANGE                       = $846D {deprecated};

   // Convolutions (GL 1.2 ARB imaging)
   // promoted to core v1.2 from GL_EXT_convolution (EXT #12)
   GL_CONVOLUTION_1D                                 = $8010 {deprecated};
   GL_CONVOLUTION_2D                                 = $8011 {deprecated};
   GL_SEPARABLE_2D                                   = $8012 {deprecated};
   GL_CONVOLUTION_BORDER_MODE                        = $8013 {deprecated};
   GL_CONVOLUTION_FILTER_SCALE                       = $8014 {deprecated};
   GL_CONVOLUTION_FILTER_BIAS                        = $8015 {deprecated};
   GL_REDUCE                                         = $8016 {deprecated};
   GL_CONVOLUTION_FORMAT                             = $8017 {deprecated};
   GL_CONVOLUTION_WIDTH                              = $8018 {deprecated};
   GL_CONVOLUTION_HEIGHT                             = $8019 {deprecated};
   GL_MAX_CONVOLUTION_WIDTH                          = $801A {deprecated};
   GL_MAX_CONVOLUTION_HEIGHT                         = $801B {deprecated};
   GL_POST_CONVOLUTION_RED_SCALE                     = $801C {deprecated};
   GL_POST_CONVOLUTION_GREEN_SCALE                   = $801D {deprecated};
   GL_POST_CONVOLUTION_BLUE_SCALE                    = $801E {deprecated};
   GL_POST_CONVOLUTION_ALPHA_SCALE                   = $801F {deprecated};
   GL_POST_CONVOLUTION_RED_BIAS                      = $8020 {deprecated};
   GL_POST_CONVOLUTION_GREEN_BIAS                    = $8021 {deprecated};
   GL_POST_CONVOLUTION_BLUE_BIAS                     = $8022 {deprecated};
   GL_POST_CONVOLUTION_ALPHA_BIAS                    = $8023 {deprecated};

   // Histogram (GL 1.2 ARB imaging)
   // promoted to core v1.2 from GL_EXT_histogram (EXT #11)
   GL_HISTOGRAM                                      = $8024 {deprecated};
   GL_PROXY_HISTOGRAM                                = $8025 {deprecated};
   GL_HISTOGRAM_WIDTH                                = $8026 {deprecated};
   GL_HISTOGRAM_FORMAT                               = $8027 {deprecated};
   GL_HISTOGRAM_RED_SIZE                             = $8028 {deprecated};
   GL_HISTOGRAM_GREEN_SIZE                           = $8029 {deprecated};
   GL_HISTOGRAM_BLUE_SIZE                            = $802A {deprecated};
   GL_HISTOGRAM_ALPHA_SIZE                           = $802B {deprecated};
   GL_HISTOGRAM_LUMINANCE_SIZE                       = $802C {deprecated};
   GL_HISTOGRAM_SINK                                 = $802D {deprecated};
   GL_MINMAX                                         = $802E {deprecated};
   GL_MINMAX_FORMAT                                  = $802F {deprecated};
   GL_MINMAX_SINK                                    = $8030 {deprecated};
   GL_TABLE_TOO_LARGE                                = $8031 {deprecated};

   // Color Matrix (GL 1.2 ARB imaging)
   // promoted to core v1.2 from SGI_color_matrix (EXT #13)
   GL_COLOR_MATRIX                                   = $80B1 {deprecated};
   GL_COLOR_MATRIX_STACK_DEPTH                       = $80B2 {deprecated};
   GL_MAX_COLOR_MATRIX_STACK_DEPTH                   = $80B3 {deprecated};
   GL_POST_COLOR_MATRIX_RED_SCALE                    = $80B4 {deprecated};
   GL_POST_COLOR_MATRIX_GREEN_SCALE                  = $80B5 {deprecated};
   GL_POST_COLOR_MATRIX_BLUE_SCALE                   = $80B6 {deprecated};
   GL_POST_COLOR_MATRIX_ALPHA_SCALE                  = $80B7 {deprecated};
   GL_POST_COLOR_MATRIX_RED_BIAS                     = $80B8 {deprecated};
   GL_POST_COLOR_MATRIX_GREEN_BIAS                   = $80B9 {deprecated};
   GL_POST_COLOR_MATRIX_BLUE_BIAS                    = $80BA {deprecated};
   GL_POST_COLOR_MATRIX_ALPHA_BIAS                   = $80BB {deprecated};

   // Color Table (GL 1.2 ARB imaging)
   // promoted to core v1.2 from GL_SGI_color_table (EXT #14)
   GL_COLOR_TABLE                                    = $80D0 {deprecated};
   GL_POST_CONVOLUTION_COLOR_TABLE                   = $80D1 {deprecated};
   GL_POST_COLOR_MATRIX_COLOR_TABLE                  = $80D2 {deprecated};
   GL_PROXY_COLOR_TABLE                              = $80D3 {deprecated};
   GL_PROXY_POST_CONVOLUTION_COLOR_TABLE             = $80D4 {deprecated};
   GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE            = $80D5 {deprecated};
   GL_COLOR_TABLE_SCALE                              = $80D6 {deprecated};
   GL_COLOR_TABLE_BIAS                               = $80D7 {deprecated};
   GL_COLOR_TABLE_FORMAT                             = $80D8 {deprecated};
   GL_COLOR_TABLE_WIDTH                              = $80D9 {deprecated};
   GL_COLOR_TABLE_RED_SIZE                           = $80DA {deprecated};
   GL_COLOR_TABLE_GREEN_SIZE                         = $80DB {deprecated};
   GL_COLOR_TABLE_BLUE_SIZE                          = $80DC {deprecated};
   GL_COLOR_TABLE_ALPHA_SIZE                         = $80DD {deprecated};
   GL_COLOR_TABLE_LUMINANCE_SIZE                     = $80DE {deprecated};
   GL_COLOR_TABLE_INTENSITY_SIZE                     = $80DF {deprecated};

   // Convolution Border Modes (GL 1.2 ARB imaging)
   // promoted to core v1.2 from GL_HP_convolution_border_modes (EXT #67)
   GL_CONSTANT_BORDER                                = $8151 {deprecated};
	 GL_REPLICATE_BORDER				                       = $8153 {deprecated};
	 GL_CONVOLUTION_BORDER_COLOR			                 = $8154 {deprecated};
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}    {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}    {$region 'New core constants in OpenGL v1.3'} {$ENDIF}
   // Multitexturing
   // promoted to core OpenGL v1.3 from GL_ARB_multitexture (ARB #1)
   GL_TEXTURE0                                       = $84C0;
   GL_TEXTURE1                                       = $84C1;
   GL_TEXTURE2                                       = $84C2;
   GL_TEXTURE3                                       = $84C3;
   GL_TEXTURE4                                       = $84C4;
   GL_TEXTURE5                                       = $84C5;
   GL_TEXTURE6                                       = $84C6;
   GL_TEXTURE7                                       = $84C7;
   GL_TEXTURE8                                       = $84C8;
   GL_TEXTURE9                                       = $84C9;
   GL_TEXTURE10                                      = $84CA;
   GL_TEXTURE11                                      = $84CB;
   GL_TEXTURE12                                      = $84CC;
   GL_TEXTURE13                                      = $84CD;
   GL_TEXTURE14                                      = $84CE;
   GL_TEXTURE15                                      = $84CF;
   GL_TEXTURE16                                      = $84D0;
   GL_TEXTURE17                                      = $84D1;
   GL_TEXTURE18                                      = $84D2;
   GL_TEXTURE19                                      = $84D3;
   GL_TEXTURE20                                      = $84D4;
   GL_TEXTURE21                                      = $84D5;
   GL_TEXTURE22                                      = $84D6;
   GL_TEXTURE23                                      = $84D7;
   GL_TEXTURE24                                      = $84D8;
   GL_TEXTURE25                                      = $84D9;
   GL_TEXTURE26                                      = $84DA;
   GL_TEXTURE27                                      = $84DB;
   GL_TEXTURE28                                      = $84DC;
   GL_TEXTURE29                                      = $84DD;
   GL_TEXTURE30                                      = $84DE;
   GL_TEXTURE31                                      = $84DF;
   GL_ACTIVE_TEXTURE                                 = $84E0;

   // Multisampling
   // promoted to core OpenGL v1.3 from GL_ARB_multisample (ARB #5)
   GL_MULTISAMPLE                                    = $809D;
   GL_SAMPLE_ALPHA_TO_COVERAGE                       = $809E;
   GL_SAMPLE_ALPHA_TO_ONE                            = $809F;
   GL_SAMPLE_COVERAGE                                = $80A0;
   GL_SAMPLE_BUFFERS                                 = $80A8;
   GL_SAMPLES                                        = $80A9;
   GL_SAMPLE_COVERAGE_VALUE                          = $80AA;
   GL_SAMPLE_COVERAGE_INVERT                         = $80AB;

   // Cube Mapping
   // promoted to core OpenGL v1.3 from GL_ARB_texture_cube_map (ARB #7)
   GL_TEXTURE_CUBE_MAP                               = $8513;
   GL_TEXTURE_BINDING_CUBE_MAP                       = $8514;
   GL_TEXTURE_CUBE_MAP_POSITIVE_X                    = $8515;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_X                    = $8516;
   GL_TEXTURE_CUBE_MAP_POSITIVE_Y                    = $8517;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Y                    = $8518;
   GL_TEXTURE_CUBE_MAP_POSITIVE_Z                    = $8519;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Z                    = $851A;
   GL_PROXY_TEXTURE_CUBE_MAP                         = $851B;
   GL_MAX_CUBE_MAP_TEXTURE_SIZE                      = $851C;

   // Texture Compression
   // promoted to core OpenGL v1.3 from GL_ARB_texture_compression (ARB #12)
   GL_COMPRESSED_RGB                                 = $84ED;
   GL_COMPRESSED_RGBA                                = $84EE;
   GL_TEXTURE_COMPRESSION_HINT                       = $84EF;
   GL_TEXTURE_COMPRESSED_IMAGE_SIZE                  = $86A0;
   GL_TEXTURE_COMPRESSED                             = $86A1;
   GL_NUM_COMPRESSED_TEXTURE_FORMATS                 = $86A2;
   GL_COMPRESSED_TEXTURE_FORMATS                     = $86A3;

   // Texture Border Clamping
   // promoted to core OpenGL v1.3 from GL_ARB_texture_border_clamp (ARB #13)
   GL_CLAMP_TO_BORDER                                = $812D;

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 1.3 deprecated'} {$ENDIF}
   // promoted to core OpenGL v1.3 from GL_ARB_multitexture (ARB #1)
   GL_CLIENT_ACTIVE_TEXTURE                          = $84E1 {deprecated};
   GL_MAX_TEXTURE_UNITS                              = $84E2 {deprecated};

   // Transpose Matrices
   // promoted to core OpenGL v1.3 from GL_ARB_transpose_matrix (ARB #3)
   GL_TRANSPOSE_MODELVIEW_MATRIX                     = $84E3 {deprecated};
   GL_TRANSPOSE_PROJECTION_MATRIX                    = $84E4 {deprecated};
   GL_TRANSPOSE_TEXTURE_MATRIX                       = $84E5 {deprecated};
   GL_TRANSPOSE_COLOR_MATRIX                         = $84E6 {deprecated};

   // promoted to core OpenGL v1.3 from GL_ARB_multisample (ARB #5)
   GL_MULTISAMPLE_BIT                                = $20000000 {deprecated};

   // promoted to core OpenGL v1.3 from GL_ARB_texture_cube_map (ARB #7)
   GL_NORMAL_MAP                                     = $8511 {deprecated};
   GL_REFLECTION_MAP                                 = $8512 {deprecated};

   // promoted to core OpenGL v1.3 from GL_ARB_texture_compression (ARB #12)
   GL_COMPRESSED_ALPHA                               = $84E9 {deprecated};
   GL_COMPRESSED_LUMINANCE                           = $84EA {deprecated};
   GL_COMPRESSED_LUMINANCE_ALPHA                     = $84EB {deprecated};
   GL_COMPRESSED_INTENSITY                           = $84EC {deprecated};

   // Texture Combine Environment Mode
   // promoted to core OpenGL v1.3 from GL_ARB_texture_env_combine (ARB #17)
   GL_COMBINE                                        = $8570 {deprecated};
   GL_COMBINE_RGB                                    = $8571 {deprecated};
   GL_COMBINE_ALPHA                                  = $8572 {deprecated};
   GL_SOURCE0_RGB                                    = $8580 {deprecated};
   GL_SOURCE1_RGB                                    = $8581 {deprecated};
   GL_SOURCE2_RGB                                    = $8582 {deprecated};
   GL_SOURCE0_ALPHA                                  = $8588 {deprecated};
   GL_SOURCE1_ALPHA                                  = $8589 {deprecated};
   GL_SOURCE2_ALPHA                                  = $858A {deprecated};
   GL_OPERAND0_RGB                                   = $8590 {deprecated};
   GL_OPERAND1_RGB                                   = $8591 {deprecated};
   GL_OPERAND2_RGB                                   = $8592 {deprecated};
   GL_OPERAND0_ALPHA                                 = $8598 {deprecated};
   GL_OPERAND1_ALPHA                                 = $8599 {deprecated};
   GL_OPERAND2_ALPHA                                 = $859A {deprecated};
   GL_RGB_SCALE                                      = $8573 {deprecated};
   GL_ADD_SIGNED                                     = $8574 {deprecated};
   GL_INTERPOLATE                                    = $8575 {deprecated};
   GL_SUBTRACT                                       = $84E7 {deprecated};
   GL_CONSTANT                                       = $8576 {deprecated};
   GL_PRIMARY_COLOR                                  = $8577 {deprecated};
   GL_PREVIOUS                                       = $8578 {deprecated};

   // Texture Dot3 Environment Mode
   // promoted to OpenGL v1.3 from GL_ARB_texture_env_dot3 (ARB #19)
   GL_DOT3_RGB                                       = $86AE {deprecated};
   GL_DOT3_RGBA                                      = $86AF {deprecated};
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core constants in OpenGL v1.4'} {$ENDIF}

   // Separate Blend Functions
   // promoted to core OpenGL v1.4 from GL_EXT_blend_func_separate (EXT #173)
   GL_BLEND_DST_RGB                                 = $80C8;
   GL_BLEND_SRC_RGB                                 = $80C9;
   GL_BLEND_DST_ALPHA                               = $80CA;
   GL_BLEND_SRC_ALPHA                               = $80CB;

   // Point Parameters
   // promoted to core OpenGL v1.4 from GL_ARB_point_parameters (ARB #14)
   GL_POINT_FADE_THRESHOLD_SIZE                      = $8128;

   // Depth Texture
   // promoted to core OpenGL v1.4 from GL_ARB_depth_texture (ARB #22)
   GL_DEPTH_COMPONENT16                              = $81A5;
   GL_DEPTH_COMPONENT24                              = $81A6;
   GL_DEPTH_COMPONENT32                              = $81A7;

   // Texture Mirrored Repeat
   // promoted to Core OpenGL v1.4 from GL_ARB_texture_mirrored_repeat (ARB #21)
   GL_MIRRORED_REPEAT					                       = $8370;

   // Texture LOD Bias
   // (promoted to core OpenGL v1.4 from GL_EXT_texture_lod_bias (EXT #186)
   GL_MAX_TEXTURE_LOD_BIAS                          = $84FD;
   GL_TEXTURE_LOD_BIAS                              = $8501;

   // Stencil Wrap
   // promoted to core OpenGL v1.4 from GL_EXT_stencil_wrap (EXT #176)
   GL_INCR_WRAP                                      = $8507;
   GL_DECR_WRAP                                      = $8508;

   // Depth Textures
   // promoted to core OpenGL v1.4 from GL_ARB_depth_texture (ARB #22)
   GL_TEXTURE_DEPTH_SIZE                             = $884A;

   // Shadows
   // promoted to core OpenGL v1.4 from GL_ARB_shadow (ARB #23)
   GL_TEXTURE_COMPARE_MODE                           = $884C;
   GL_TEXTURE_COMPARE_FUNC                           = $884D;

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 1.4 deprecated'} {$ENDIF}
   // from GL_ARB_point_parameters (ARB #14)
   GL_POINT_SIZE_MIN                                 = $8126 {deprecated};
   GL_POINT_SIZE_MAX                                 = $8127 {deprecated};
   GL_POINT_DISTANCE_ATTENUATION                     = $8129 {deprecated};

   // Automatic Mipmap Generation
   // promoted to core OpenGL v1.4 from GL_SGIS_generate_mipmap (EXT #32)
   GL_GENERATE_MIPMAP                               = $8191 {deprecated};
   GL_GENERATE_MIPMAP_HINT                          = $8192 {deprecated};

   // Fog Coordinate
   // promoted to core OpenGL v1.4 from GL_EXT_fog_coord (EXT #149)
   GL_FOG_COORDINATE_SOURCE                         = $8450 {deprecated};
   GL_FOG_COORDINATE                                = $8451 {deprecated};
   GL_FRAGMENT_DEPTH                                = $8452 {deprecated};
   GL_CURRENT_FOG_COORDINATE                        = $8453 {deprecated};
   GL_FOG_COORDINATE_ARRAY_TYPE                     = $8454 {deprecated};
   GL_FOG_COORDINATE_ARRAY_STRIDE                   = $8455 {deprecated};
   GL_FOG_COORDINATE_ARRAY_POINTER                  = $8456 {deprecated};
   GL_FOG_COORDINATE_ARRAY                          = $8457 {deprecated};

   // Secondary Color
   // promoted to core OpenGL v1.4 from GL_EXT_secondary_color (EXT #145)
   GL_COLOR_SUM                                     = $8458 {deprecated};
   GL_CURRENT_SECONDARY_COLOR                       = $8459 {deprecated};
   GL_SECONDARY_COLOR_ARRAY_SIZE                    = $845A {deprecated};
   GL_SECONDARY_COLOR_ARRAY_TYPE                    = $845B {deprecated};
   GL_SECONDARY_COLOR_ARRAY_STRIDE                  = $845C {deprecated};
   GL_SECONDARY_COLOR_ARRAY_POINTER                 = $845D {deprecated};
   GL_SECONDARY_COLOR_ARRAY                         = $845E {deprecated};

   // (promoted to core OpenGL v1.4 from GL_EXT_texture_lod_bias (EXT #186)
   GL_TEXTURE_FILTER_CONTROL                        = $8500 {deprecated};

   // promoted to core OpenGL v1.4 from GL_ARB_depth_texture (ARB #22)
   GL_DEPTH_TEXTURE_MODE                             = $884B {deprecated};

   // promoted to core OpenGL v1.4 from GL_ARB_shadow (ARB #23)
   GL_COMPARE_R_TO_TEXTURE                           = $884E {deprecated};
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core constants in OpenGL v1.5'} {$ENDIF}
   // Buffer Objects
   // promoted to core OpenGL v1.5 from GL_ARB_vertex_buffer_object (ARB #28)
   GL_BUFFER_SIZE                                    = $8764;
   GL_BUFFER_USAGE                                   = $8765;

   // Occlusion Queries
   // promoted to core OpenGL v1.5 from GL_ARB_occulsion_query (ARB #29)
   GL_QUERY_COUNTER_BITS                             = $8864;
   GL_CURRENT_QUERY                                  = $8865;
   GL_QUERY_RESULT                                   = $8866;
   GL_QUERY_RESULT_AVAILABLE                         = $8867;

   // Buffer Objects
   // promoted to core OpenGL v1.5 from GL_ARB_vertex_buffer_object (ARB #28)
   GL_ARRAY_BUFFER                                   = $8892;
   GL_ELEMENT_ARRAY_BUFFER                           = $8893;
   GL_ARRAY_BUFFER_BINDING                           = $8894;
   GL_ELEMENT_ARRAY_BUFFER_BINDING                   = $8895;
   GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING             = $889F;
   GL_READ_ONLY                                      = $88B8;
   GL_WRITE_ONLY                                     = $88B9;
   GL_READ_WRITE                                     = $88BA;
   GL_BUFFER_ACCESS                                  = $88BB;
   GL_BUFFER_MAPPED                                  = $88BC;
   GL_BUFFER_MAP_POINTER                             = $88BD;
   GL_STREAM_DRAW                                    = $88E0;
   GL_STREAM_READ                                    = $88E1;
   GL_STREAM_COPY                                    = $88E2;
   GL_STATIC_DRAW                                    = $88E4;
   GL_STATIC_READ                                    = $88E5;
   GL_STATIC_COPY                                    = $88E6;
   GL_DYNAMIC_DRAW                                   = $88E8;
   GL_DYNAMIC_READ                                   = $88E9;
   GL_DYNAMIC_COPY                                   = $88EA;

   // Occlusion Queries   
   // promoted to core OpenGL v1.5 from GL_ARB_occulsion_query (ARB #29)
   GL_SAMPLES_PASSED                                 = $8914;

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 1.5 deprecated'} {$ENDIF}
   // from GL_ARB_vertex_buffer_object (ARB #28)
   GL_VERTEX_ARRAY_BUFFER_BINDING                    = $8896 {deprecated};
   GL_NORMAL_ARRAY_BUFFER_BINDING                    = $8897 {deprecated};
   GL_COLOR_ARRAY_BUFFER_BINDING                     = $8898 {deprecated};
   GL_INDEX_ARRAY_BUFFER_BINDING                     = $8899 {deprecated};
   GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING             = $889A {deprecated};
   GL_EDGE_FLAG_ARRAY_BUFFER_BINDING                 = $889B {deprecated};
   GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING           = $889C {deprecated};
   GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING            = $889D {deprecated};
   GL_WEIGHT_ARRAY_BUFFER_BINDING                    = $889E {deprecated};

	 GL_FOG_COORD_SRC		                = GL_FOG_COORDINATE_SOURCE {deprecated};
	 GL_FOG_COORD					              = GL_FOG_COORDINATE {deprecated};
	 GL_CURRENT_FOG_COORD				        = GL_CURRENT_FOG_COORDINATE {deprecated};
	 GL_FOG_COORD_ARRAY_TYPE				    = GL_FOG_COORDINATE_ARRAY_TYPE {deprecated};
	 GL_FOG_COORD_ARRAY_STRIDE				  = GL_FOG_COORDINATE_ARRAY_STRIDE {deprecated};
	 GL_FOG_COORD_ARRAY_POINTER				  = GL_FOG_COORDINATE_ARRAY_POINTER {deprecated};
	 GL_FOG_COORD_ARRAY					        = GL_FOG_COORDINATE_ARRAY {deprecated};
	 GL_FOG_COORD_ARRAY_BUFFER_BINDING	= GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING {deprecated};

   // Changed Tokens
   // new naming scheme in OpenGL v1.5, old tokens kept for backwards compatibility
	 GL_SRC0_RGB					              = GL_SOURCE0_RGB {deprecated};
	 GL_SRC1_RGB					              = GL_SOURCE1_RGB {deprecated};
	 GL_SRC2_RGB					              = GL_SOURCE2_RGB {deprecated};
	 GL_SRC0_ALPHA				              = GL_SOURCE0_ALPHA {deprecated};
	 GL_SRC1_ALPHA					            = GL_SOURCE1_ALPHA {deprecated};
	 GL_SRC2_ALPHA					            = GL_SOURCE2_ALPHA {deprecated};
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core constants in OpenGL v2.0'} {$ENDIF}
   // OpenGL 2.0

   // Changed Tokens
   // new name in OpenGL v2.0
   GL_BLEND_EQUATION_RGB          = GL_BLEND_EQUATION;

   // promoted to core OpenGL v2.0 from GL_ARB_vertex_shader (ARB #31)
   GL_VERTEX_ATTRIB_ARRAY_ENABLED                    = $8622;
   GL_VERTEX_ATTRIB_ARRAY_SIZE                       = $8623;
   GL_VERTEX_ATTRIB_ARRAY_STRIDE                     = $8624;
   GL_VERTEX_ATTRIB_ARRAY_TYPE                       = $8625;
   GL_CURRENT_VERTEX_ATTRIB                          = $8626;
   GL_VERTEX_PROGRAM_POINT_SIZE                      = $8642;
   GL_VERTEX_ATTRIB_ARRAY_POINTER                    = $8645;

   // Separate Stencil
   // promoted to core OpenGL v2.0 from GL_ARB_stencil_two_side (ARB #unknown)
   GL_STENCIL_BACK_FUNC                              = $8800;
   GL_STENCIL_BACK_FAIL                              = $8801;
   GL_STENCIL_BACK_PASS_DEPTH_FAIL                   = $8802;
   GL_STENCIL_BACK_PASS_DEPTH_PASS                   = $8803;

   // promoted to core OpenGL v2.0 from GL_ARB_draw_buffers (ARB #37) / GL_ATI_draw_buffers (EXT #277)
   GL_MAX_DRAW_BUFFERS                               = $8824;
   GL_DRAW_BUFFER0                                   = $8825;
   GL_DRAW_BUFFER1                                   = $8826;
   GL_DRAW_BUFFER2                                   = $8827;
   GL_DRAW_BUFFER3                                   = $8828;
   GL_DRAW_BUFFER4                                   = $8829;
   GL_DRAW_BUFFER5                                   = $882A;
   GL_DRAW_BUFFER6                                   = $882B;
   GL_DRAW_BUFFER7                                   = $882C;
   GL_DRAW_BUFFER8                                   = $882D;
   GL_DRAW_BUFFER9                                   = $882E;
   GL_DRAW_BUFFER10                                  = $882F;
   GL_DRAW_BUFFER11                                  = $8830;
   GL_DRAW_BUFFER12                                  = $8831;
   GL_DRAW_BUFFER13                                  = $8832;
   GL_DRAW_BUFFER14                                  = $8833;
   GL_DRAW_BUFFER15                                  = $8834;

   // Separate Blend Equation
   // promoted to core OpenGL v2.0 from GL_EXT_blend_equation_separate (EXT #299)
   GL_BLEND_EQUATION_ALPHA                           = $883D;

   // Shader Programs
   // promoted to core OpenGL v2.0 from GL_ARB_vertex_shader (ARB #31)
   GL_MAX_VERTEX_ATTRIBS                             = $8869;
   GL_VERTEX_ATTRIB_ARRAY_NORMALIZED                 = $886A;

   // promoted to core OpenGL v2.0 from GL_ARB_vertex_shader (ARB #31) /GL_ARB_fragment_shader (ARB #32)
   GL_MAX_TEXTURE_IMAGE_UNITS                        = $8872;

   // promoted to core OpenGL v2.0 from GL_ARB_fragment_shader (ARB #32)
   GL_FRAGMENT_SHADER                                = $8B30;

   // promoted to core OpenGL v2.0 from GL_ARB_vertex_shader (ARB #31)
   GL_VERTEX_SHADER                                  = $8B31;

   // promoted to core OpenGL v2.0 from GL_ARB_fragment_shader (ARB #32)
   GL_MAX_FRAGMENT_UNIFORM_COMPONENTS                = $8B49;

   // promoted to core OpenGL v2.0 from GL_ARB_vertex_shader (ARB #31)
   GL_MAX_VERTEX_UNIFORM_COMPONENTS                  = $8B4A;
   GL_MAX_VARYING_FLOATS                             = $8B4B {deprecated}; // not yet removed
   GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS                 = $8B4C;
   GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS               = $8B4D;

   // Shader Objects
   // promoted to core OpenGL v2.0 from GL_ARB_shader_objects (ARB #30)
   GL_SHADER_TYPE                                    = $8B4F;
   GL_FLOAT_VEC2                                     = $8B50;
   GL_FLOAT_VEC3                                     = $8B51;
   GL_FLOAT_VEC4                                     = $8B52;
   GL_INT_VEC2                                       = $8B53;
   GL_INT_VEC3                                       = $8B54;
   GL_INT_VEC4                                       = $8B55;
   GL_BOOL                                           = $8B56;
   GL_BOOL_VEC2                                      = $8B57;
   GL_BOOL_VEC3                                      = $8B58;
   GL_BOOL_VEC4                                      = $8B59;
   GL_FLOAT_MAT2                                     = $8B5A;
   GL_FLOAT_MAT3                                     = $8B5B;
   GL_FLOAT_MAT4                                     = $8B5C;
   GL_SAMPLER_1D                                     = $8B5D;
   GL_SAMPLER_2D                                     = $8B5E;
   GL_SAMPLER_3D                                     = $8B5F;
   GL_SAMPLER_CUBE                                   = $8B60;
   GL_SAMPLER_1D_SHADOW                              = $8B61;
   GL_SAMPLER_2D_SHADOW                              = $8B62;
   GL_DELETE_STATUS                                  = $8B80;
   GL_COMPILE_STATUS                                 = $8B81;
   GL_LINK_STATUS                                    = $8B82;
   GL_VALIDATE_STATUS                                = $8B83;
   GL_INFO_LOG_LENGTH                                = $8B84;
   GL_ATTACHED_SHADERS                               = $8B85;
   GL_ACTIVE_UNIFORMS                                = $8B86;
   GL_ACTIVE_UNIFORM_MAX_LENGTH                      = $8B87;
   GL_SHADER_SOURCE_LENGTH                           = $8B88;

   // Shader Programs
   // promoted to core OpenGL v2.0 from GL_ARB_vertex_shader (ARB #31)
   GL_ACTIVE_ATTRIBUTES                              = $8B89;
   GL_ACTIVE_ATTRIBUTE_MAX_LENGTH                    = $8B8A;

   // promoted to core OpenGL v2.0 from GL_ARB_fragment_shader (ARB #32)
   GL_FRAGMENT_SHADER_DERIVATIVE_HINT                   = $8B8B;

   // OpenGL Shading Language
   // promoted to core OpenGL v2.0 from GL_ARB_shading_language_100 (ARB #33)
   GL_SHADING_LANGUAGE_VERSION                       = $8B8C;

   // Shader Objects
   // promoted to core OpenGL v2.0 from GL_ARB_shader_objects (ARB #30) (added for 2.0)
   GL_CURRENT_PROGRAM					                         = $8B8D;

   // Point Sprites
   // promoted to core OpenGL v2.0 from GL_ARB_point_sprite (ARB #35) (added for 2.0)
   GL_POINT_SPRITE_COORD_ORIGIN                         = $8CA0;
   GL_LOWER_LEFT                                        = $8CA1;
   GL_UPPER_LEFT                                        = $8CA2;

   // Separate Stencil
   // promoted to core OpenGL v2.0 from GL_ARB_stencil_two_side (ARB #unknown)
   GL_STENCIL_BACK_REF                                  = $8CA3;
   GL_STENCIL_BACK_VALUE_MASK                           = $8CA4;
   GL_STENCIL_BACK_WRITEMASK                            = $8CA5;

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 2.0 deprecated'} {$ENDIF}
   // from GL_ARB_vertex_shader (ARB #31)
   GL_VERTEX_PROGRAM_TWO_SIDE                        = $8643 {deprecated};

   // from GL_ARB_point_sprite (ARB #35)
   GL_POINT_SPRITE                                   = $8861 {deprecated};
   GL_COORD_REPLACE                                  = $8862 {deprecated};

   // from GL_ARB_vertex_shader (ARB #31) /GL_ARB_fragment_shader (ARB #32)
   GL_MAX_TEXTURE_COORDS                             = $8871 {deprecated};
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core constants in OpenGL v2.1'} {$ENDIF}

   // OpenGL 2.1

   // Pixel Buffer Objects
   // from GL_ARB_pixel_buffer_object (ARB #42)
   GL_PIXEL_PACK_BUFFER                                 = $88EB;
   GL_PIXEL_UNPACK_BUFFER                               = $88EC;
   GL_PIXEL_PACK_BUFFER_BINDING                         = $88ED;
   GL_PIXEL_UNPACK_BUFFER_BINDING                       = $88EF;

   // Non-Square Matrices
   // new for OpenGL 2.1
   GL_FLOAT_MAT2x3                                      = $8B65;
   GL_FLOAT_MAT2x4                                      = $8B66;
   GL_FLOAT_MAT3x2                                      = $8B67;
   GL_FLOAT_MAT3x4                                      = $8B68;
   GL_FLOAT_MAT4x2                                      = $8B69;
   GL_FLOAT_MAT4x3                                      = $8B6A;

   // sRGB Textures
   // from GL_EXT_texture_sRGB (EXT #315)
   GL_SRGB                                              = $8C40;
   GL_SRGB8                                             = $8C41;
   GL_SRGB_ALPHA                                        = $8C42;
   GL_SRGB8_ALPHA8                                      = $8C43;
   GL_COMPRESSED_SRGB                                   = $8C48;
   GL_COMPRESSED_SRGB_ALPHA                             = $8C49;

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 2.1 deprecated'} {$ENDIF}
   // new
   GL_CURRENT_RASTER_SECONDARY_COLOR                    = $845F {deprecated};
   // from GL_EXT_texture_sRGB (EXT #315)
   GL_SLUMINANCE_ALPHA                                  = $8C44 {deprecated};
   GL_SLUMINANCE8_ALPHA8                                = $8C45 {deprecated};
   GL_SLUMINANCE                                        = $8C46 {deprecated};
   GL_SLUMINANCE8                                       = $8C47 {deprecated};
   GL_COMPRESSED_SLUMINANCE                             = $8C4A {deprecated};
   GL_COMPRESSED_SLUMINANCE_ALPHA                       = $8C4B {deprecated};
{$IFDEF GLS_COMPILER_2005_UP} {$endregion'} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core constants in OpenGL v3.0'} {$ENDIF}
   // TODO: arrange these better, find where they came from
   GL_COMPARE_REF_TO_TEXTURE				= $884E;//GL_COMPARE_R_TO_TEXTURE;
   GL_CLIP_DISTANCE0					= $3000;//GL_CLIP_PLANE0;
   GL_CLIP_DISTANCE1					= $3001;//GL_CLIP_PLANE1;
   GL_CLIP_DISTANCE2					= $3002;//GL_CLIP_PLANE2;
   GL_CLIP_DISTANCE3					= $3003;//GL_CLIP_PLANE3;
   GL_CLIP_DISTANCE4					= $3004;//GL_CLIP_PLANE4;
   GL_CLIP_DISTANCE5					= $3005;//GL_CLIP_PLANE5;
   GL_CLIP_DISTANCE6					= $3006;
   GL_CLIP_DISTANCE7					= $3007;
   GL_MAX_CLIP_DISTANCES				= $0D32;//GL_MAX_CLIP_PLANES;
	 GL_MAJOR_VERSION					=$821B;
	 GL_MINOR_VERSION					=$821C;
	 GL_NUM_EXTENSIONS					=$821D;
	 GL_CONTEXT_FLAGS					=$821E;
//# Removed - replaced by per-attachment framebuffer queries
//##	  COLOR_COMPONENT_TYPE				  = 0x821F
//##	  COLOR_ENCODING_TYPE				  = 0x8220
//##	  DEPTH_COMPONENT_TYPE				  = 0x8221
//##	  TEXTURE_SHARED_TYPE				  = 0x8222
	 GL_DEPTH_BUFFER					=$8223;
	 GL_STENCIL_BUFFER					=$8224;
	 GL_COMPRESSED_RED					=$8225;
	 GL_COMPRESSED_RG					=$8226;
	 GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT		=$0001;
	 GL_RGBA32F						=$8814;
	 GL_RGB32F						=$8815;
	 GL_RGBA16F						=$881A;
	 GL_RGB16F						=$881B;
	 GL_VERTEX_ATTRIB_ARRAY_INTEGER			=$88FD;
	 GL_MAX_ARRAY_TEXTURE_LAYERS			=$88FF;
	 GL_MIN_PROGRAM_TEXEL_OFFSET			=$8904;
	 GL_MAX_PROGRAM_TEXEL_OFFSET			=$8905;
	 GL_CLAMP_VERTEX_COLOR				=$891A;
	 GL_CLAMP_FRAGMENT_COLOR				=$891B;
	 GL_CLAMP_READ_COLOR				=$891C;
	 GL_FIXED_ONLY					=$891D;
	 GL_MAX_VARYING_COMPONENTS				= GL_MAX_VARYING_FLOATS {deprecated}; // not yet removed
//	 GL_TEXTURE_RED_TYPE				=$8C10;
//	 GL_TEXTURE_GREEN_TYPE				=$8C11;
//	 GL_TEXTURE_BLUE_TYPE				=$8C12;
//	 GL_TEXTURE_ALPHA_TYPE				=$8C13;
//	 GL_TEXTURE_LUMINANCE_TYPE				=$8C14;
//	 GL_TEXTURE_INTENSITY_TYPE				=$8C15;
//	 GL_TEXTURE_DEPTH_TYPE				= $8C16;
//	 GL_UNSIGNED_NORMALIZED				= $8C17;
	 GL_TEXTURE_1D_ARRAY				= $8C18;
	 GL_PROXY_TEXTURE_1D_ARRAY				= $8C19;
	 GL_TEXTURE_2D_ARRAY				= $8C1A;
	 GL_PROXY_TEXTURE_2D_ARRAY				= $8C1B;
	 GL_TEXTURE_BINDING_1D_ARRAY			= $8C1C;
	 GL_TEXTURE_BINDING_2D_ARRAY			= $8C1D;
	 GL_R11F_G11F_B10F					= $8C3A;
	 GL_UNSIGNED_INT_10F_11F_11F_REV			= $8C3B;
	 GL_RGB9_E5						= $8C3D;
	 GL_UNSIGNED_INT_5_9_9_9_REV			= $8C3E;
	 GL_TEXTURE_SHARED_SIZE				= $8C3F;
	 GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH		= $8C76;
	 GL_TRANSFORM_FEEDBACK_BUFFER_MODE			= $8C7F;
	 GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS	= $8C80;
	 GL_TRANSFORM_FEEDBACK_VARYINGS			= $8C83;
	 GL_TRANSFORM_FEEDBACK_BUFFER_START			= $8C84;
	 GL_TRANSFORM_FEEDBACK_BUFFER_SIZE			= $8C85;
	 GL_PRIMITIVES_GENERATED				= $8C87;
	 GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN		= $8C88;
	 GL_RASTERIZER_DISCARD				= $8C89;
	 GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS	= $8C8A;
	 GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS		= $8C8B;
	 GL_INTERLEAVED_ATTRIBS				= $8C8C;
	 GL_SEPARATE_ATTRIBS				= $8C8D;
	 GL_TRANSFORM_FEEDBACK_BUFFER			= $8C8E;
	 GL_TRANSFORM_FEEDBACK_BUFFER_BINDING		= $8C8F;
	 GL_RGBA32UI					= $8D70;
	 GL_RGB32UI						= $8D71;
	 GL_RGBA16UI					= $8D76;
	 GL_RGB16UI						= $8D77;
	 GL_RGBA8UI						= $8D7C;
	 GL_RGB8UI						= $8D7D;
	 GL_RGBA32I						= $8D82;
	 GL_RGB32I						= $8D83;
	 GL_RGBA16I						= $8D88;
	 GL_RGB16I						= $8D89;
	 GL_RGBA8I						= $8D8E;
	 GL_RGB8I						  = $8D8F;
	 GL_RED_INTEGER				= $8D94;
	 GL_GREEN_INTEGER			= $8D95;
	 GL_BLUE_INTEGER			= $8D96;
	 GL_ALPHA_INTEGER			= $8D97;
	 GL_RGB_INTEGER				= $8D98;
	 GL_RGBA_INTEGER			= $8D99;
	 GL_BGR_INTEGER				= $8D9A;
	 GL_BGRA_INTEGER			= $8D9B;
   // these 2 never made it to core, only _EXT?
//	 GL_LUMINANCE_INTEGER       = $8D9C;
//	 GL_LUMINANCE_ALPHA_INTEGER = $8D9D;
	 GL_SAMPLER_1D_ARRAY				= $8DC0;
	 GL_SAMPLER_2D_ARRAY				= $8DC1;
	 GL_SAMPLER_1D_ARRAY_SHADOW				= $8DC3;
	 GL_SAMPLER_2D_ARRAY_SHADOW				= $8DC4;
	 GL_SAMPLER_CUBE_SHADOW				= $8DC5;
	 GL_UNSIGNED_INT_VEC2				= $8DC6;
	 GL_UNSIGNED_INT_VEC3				= $8DC7;
	 GL_UNSIGNED_INT_VEC4				= $8DC8;
	 GL_INT_SAMPLER_1D					= $8DC9;
	 GL_INT_SAMPLER_2D					= $8DCA;
	 GL_INT_SAMPLER_3D					= $8DCB;
	 GL_INT_SAMPLER_CUBE				= $8DCC;
	 GL_INT_SAMPLER_1D_ARRAY				= $8DCE;
	 GL_INT_SAMPLER_2D_ARRAY				= $8DCF;
	 GL_UNSIGNED_INT_SAMPLER_1D				= $8DD1;
	 GL_UNSIGNED_INT_SAMPLER_2D				= $8DD2;
	 GL_UNSIGNED_INT_SAMPLER_3D				= $8DD3;
	 GL_UNSIGNED_INT_SAMPLER_CUBE			= $8DD4;
	 GL_UNSIGNED_INT_SAMPLER_1D_ARRAY			= $8DD6;
	 GL_UNSIGNED_INT_SAMPLER_2D_ARRAY			= $8DD7;
	 GL_QUERY_WAIT					= $8E13;
	 GL_QUERY_NO_WAIT					= $8E14;
	 GL_QUERY_BY_REGION_WAIT				= $8E15;
	 GL_QUERY_BY_REGION_NO_WAIT				= $8E16;
   GL_BUFFER_ACCESS_FLAGS            = $911F;
   GL_BUFFER_MAP_LENGTH              = $9120;
   GL_BUFFER_MAP_OFFSET              = $9121;

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core constants in OpenGL v3.1'} {$ENDIF}
   GL_SAMPLER_2D_RECT                = $8B63;
   GL_SAMPLER_2D_RECT_SHADOW         = $8B64;
   GL_SAMPLER_BUFFER                 = $8DC2;
   GL_INT_SAMPLER_2D_RECT            = $8DCD;
   GL_INT_SAMPLER_BUFFER             = $8DD0;
   GL_UNSIGNED_INT_SAMPLER_2D_RECT   = $8DD5;
   GL_UNSIGNED_INT_SAMPLER_BUFFER    = $8DD8;
   GL_TEXTURE_BUFFER                 = $8C2A;
   GL_MAX_TEXTURE_BUFFER_SIZE        = $8C2B;
   GL_TEXTURE_BINDING_BUFFER         = $8C2C;
   GL_TEXTURE_BUFFER_DATA_STORE_BINDING = $8C2D;
   GL_TEXTURE_BUFFER_FORMAT          = $8C2E;
   GL_TEXTURE_RECTANGLE              = $84F5;
   GL_TEXTURE_BINDING_RECTANGLE      = $84F6;
   GL_PROXY_TEXTURE_RECTANGLE        = $84F7;
   GL_MAX_RECTANGLE_TEXTURE_SIZE     = $84F8;
   GL_RED_SNORM                      = $8F90;
   GL_RG_SNORM                       = $8F91;
   GL_RGB_SNORM                      = $8F92;
   GL_RGBA_SNORM                     = $8F93;
   GL_R8_SNORM                       = $8F94;
   GL_RG8_SNORM                      = $8F95;
   GL_RGB8_SNORM                     = $8F96;
   GL_RGBA8_SNORM                    = $8F97;
   GL_R16_SNORM                      = $8F98;
   GL_RG16_SNORM                     = $8F99;
   GL_RGB16_SNORM                    = $8F9A;
   GL_RGBA16_SNORM                   = $8F9B;
   GL_SIGNED_NORMALIZED              = $8F9C;
   GL_PRIMITIVE_RESTART              = $8F9D;
   GL_PRIMITIVE_RESTART_INDEX        = $8F9E;
   // re-use tokens from:
   // ARB_copy_buffer (ARB #59)
   // ARB_draw_instanced (ARB #44)
   // ARB_uniform_buffer_object (ARB #57)
{$IFDEF GLS_COMPILER_2005_UP} {$endregion}  {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core constants in OpenGL v3.2'} {$ENDIF}
   GL_CONTEXT_CORE_PROFILE_BIT              = $00000001;
   GL_CONTEXT_COMPATIBILITY_PROFILE_BIT     = $00000002;
   GL_LINES_ADJACENCY                       = $000A;
   GL_LINE_STRIP_ADJACENCY                  = $000B;
   GL_TRIANGLES_ADJACENCY                   = $000C;
   GL_TRIANGLE_STRIP_ADJACENCY              = $000D;
   GL_PROGRAM_POINT_SIZE                    = $8642;
   GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS      = $8C29;
   GL_FRAMEBUFFER_ATTACHMENT_LAYERED        = $8DA7;
   GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS  = $8DA8;
   GL_GEOMETRY_SHADER                       = $8DD9;
   GL_GEOMETRY_VERTICES_OUT                 = $8916;
   GL_GEOMETRY_INPUT_TYPE                   = $8917;
   GL_GEOMETRY_OUTPUT_TYPE                  = $8918;
   GL_MAX_GEOMETRY_UNIFORM_COMPONENTS       = $8DDF;
   GL_MAX_GEOMETRY_OUTPUT_VERTICES          = $8DE0;
   GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS	= $8DE1;
   GL_MAX_VERTEX_OUTPUT_COMPONENTS          = $9122;
   GL_MAX_GEOMETRY_INPUT_COMPONENTS         = $9123;
   GL_MAX_GEOMETRY_OUTPUT_COMPONENTS        = $9124;
   GL_MAX_FRAGMENT_INPUT_COMPONENTS         = $9125;
   GL_CONTEXT_PROFILE_MASK                  = $9126;
   // re-use tokens from:
   // VERSION_3_0
   // ARB_framebuffer_object
   // ARB_depth_clamp
   // ARB_draw_elements_base_vertex
   // ARB_fragment_coord_conventions
   // ARB_provoking_vertex
   // ARB_seamless_cube_map
   // ARB_sync
   // ARB_texture_multisample
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'ARB approved extensions constants, in extension number order'} {$ENDIF}
   // ARB approved extensions enumerants, in number order

   // ARB Extension #1 - GL_ARB_multitexture
   GL_ACTIVE_TEXTURE_ARB                             = $84E0;
   GL_CLIENT_ACTIVE_TEXTURE_ARB                      = $84E1;
   GL_MAX_TEXTURE_UNITS_ARB                          = $84E2;
   GL_TEXTURE0_ARB                                   = $84C0;
   GL_TEXTURE1_ARB                                   = $84C1;
   GL_TEXTURE2_ARB                                   = $84C2;
   GL_TEXTURE3_ARB                                   = $84C3;
   GL_TEXTURE4_ARB                                   = $84C4;
   GL_TEXTURE5_ARB                                   = $84C5;
   GL_TEXTURE6_ARB                                   = $84C6;
   GL_TEXTURE7_ARB                                   = $84C7;
   GL_TEXTURE8_ARB                                   = $84C8;
   GL_TEXTURE9_ARB                                   = $84C9;
   GL_TEXTURE10_ARB                                  = $84CA;
   GL_TEXTURE11_ARB                                  = $84CB;
   GL_TEXTURE12_ARB                                  = $84CC;
   GL_TEXTURE13_ARB                                  = $84CD;
   GL_TEXTURE14_ARB                                  = $84CE;
   GL_TEXTURE15_ARB                                  = $84CF;
   GL_TEXTURE16_ARB                                  = $84D0;
   GL_TEXTURE17_ARB                                  = $84D1;
   GL_TEXTURE18_ARB                                  = $84D2;
   GL_TEXTURE19_ARB                                  = $84D3;
   GL_TEXTURE20_ARB                                  = $84D4;
   GL_TEXTURE21_ARB                                  = $84D5;
   GL_TEXTURE22_ARB                                  = $84D6;
   GL_TEXTURE23_ARB                                  = $84D7;
   GL_TEXTURE24_ARB                                  = $84D8;
   GL_TEXTURE25_ARB                                  = $84D9;
   GL_TEXTURE26_ARB                                  = $84DA;
   GL_TEXTURE27_ARB                                  = $84DB;
   GL_TEXTURE28_ARB                                  = $84DC;
   GL_TEXTURE29_ARB                                  = $84DD;
   GL_TEXTURE30_ARB                                  = $84DE;
   GL_TEXTURE31_ARB                                  = $84DF;

   // ARB Extension #2 - GLX_ARB_get_proc_address
   // (no new tokens)

   // ARB Extension #3 - GL_ARB_transpose_matrix
   GL_TRANSPOSE_MODELVIEW_MATRIX_ARB                 = $84E3;
   GL_TRANSPOSE_PROJECTION_MATRIX_ARB                = $84E4;
   GL_TRANSPOSE_TEXTURE_MATRIX_ARB                   = $84E5;
   GL_TRANSPOSE_COLOR_MATRIX_ARB                     = $84E6;

   // ARB Extension #4 - WGL_ARB_buffer_region
   WGL_FRONT_COLOR_BUFFER_BIT_ARB                   = $00000001;
   WGL_BACK_COLOR_BUFFER_BIT_ARB                    = $00000002;
   WGL_DEPTH_BUFFER_BIT_ARB                         = $00000004;
   WGL_STENCIL_BUFFER_BIT_ARB                       = $00000008;


   // ARB Extension #5 - GL_ARB_multisample
   //                  - GLX_ARB_multisample 
   //                  - WGL_ARB_multisample
   GL_MULTISAMPLE_ARB                                = $809D;
   GL_SAMPLE_ALPHA_TO_COVERAGE_ARB                   = $809E;
   GL_SAMPLE_ALPHA_TO_ONE_ARB                        = $809F;
   GL_SAMPLE_COVERAGE_ARB                            = $80A0;
   GL_SAMPLE_BUFFERS_ARB                             = $80A8;
   GL_SAMPLES_ARB                                    = $80A9;
   GL_SAMPLE_COVERAGE_VALUE_ARB                      = $80AA;
   GL_SAMPLE_COVERAGE_INVERT_ARB                     = $80AB;
   GL_MULTISAMPLE_BIT_ARB                            = $20000000;
   GLX_SAMPLE_BUFFERS_ARB                            = 100000;
   GLX_SAMPLES_ARB                                   = 100001;
   WGL_SAMPLE_BUFFERS_ARB                            = $2041;
   WGL_SAMPLES_ARB                                   = $2042;

   // ARB Extension #6 - GL_ARB_texture_env_add
   // (no new tokens)

   // ARB Extension #7 - GL_ARB_texture_cube_map
   GL_NORMAL_MAP_ARB                                 = $8511;
   GL_REFLECTION_MAP_ARB                             = $8512;
   GL_TEXTURE_CUBE_MAP_ARB                           = $8513;
   GL_TEXTURE_BINDING_CUBE_MAP_ARB                   = $8514;
   GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB                = $8515;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB                = $8516;
   GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB                = $8517;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB                = $8518;
   GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB                = $8519;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB                = $851A;
   GL_PROXY_TEXTURE_CUBE_MAP_ARB                     = $851B;
   GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB                  = $851C;

   // ARB Extension #8 - WGL_ARB_extensions_string
   // (no new tokens)

   // ARB Extension #9 - WGL_ARB_pixel_format
   // (no new tokens)
   WGL_NUMBER_PIXEL_FORMATS_ARB                     = $2000;
   WGL_DRAW_TO_WINDOW_ARB                           = $2001;
   WGL_DRAW_TO_BITMAP_ARB                           = $2002;
   WGL_ACCELERATION_ARB                             = $2003;
   WGL_NEED_PALETTE_ARB                             = $2004;
   WGL_NEED_SYSTEM_PALETTE_ARB                      = $2005;
   WGL_SWAP_LAYER_BUFFERS_ARB                       = $2006;
   WGL_SWAP_METHOD_ARB                              = $2007;
   WGL_NUMBER_OVERLAYS_ARB                          = $2008;
   WGL_NUMBER_UNDERLAYS_ARB                         = $2009;
   WGL_TRANSPARENT_ARB                              = $200A;
   WGL_TRANSPARENT_RED_VALUE_ARB                    = $2037;
   WGL_TRANSPARENT_GREEN_VALUE_ARB                  = $2038;
   WGL_TRANSPARENT_BLUE_VALUE_ARB                   = $2039;
   WGL_TRANSPARENT_ALPHA_VALUE_ARB                  = $203A;
   WGL_TRANSPARENT_INDEX_VALUE_ARB                  = $203B;
   WGL_SHARE_DEPTH_ARB                              = $200C;
   WGL_SHARE_STENCIL_ARB                            = $200D;
   WGL_SHARE_ACCUM_ARB                              = $200E;
   WGL_SUPPORT_GDI_ARB                              = $200F;
   WGL_SUPPORT_OPENGL_ARB                           = $2010;
   WGL_DOUBLE_BUFFER_ARB                            = $2011;
   WGL_STEREO_ARB                                   = $2012;
   WGL_PIXEL_TYPE_ARB                               = $2013;
   WGL_COLOR_BITS_ARB                               = $2014;
   WGL_RED_BITS_ARB                                 = $2015;
   WGL_RED_SHIFT_ARB                                = $2016;
   WGL_GREEN_BITS_ARB                               = $2017;
   WGL_GREEN_SHIFT_ARB                              = $2018;
   WGL_BLUE_BITS_ARB                                = $2019;
   WGL_BLUE_SHIFT_ARB                               = $201A;
   WGL_ALPHA_BITS_ARB                               = $201B;
   WGL_ALPHA_SHIFT_ARB                              = $201C;
   WGL_ACCUM_BITS_ARB                               = $201D;
   WGL_ACCUM_RED_BITS_ARB                           = $201E;
   WGL_ACCUM_GREEN_BITS_ARB                         = $201F;
   WGL_ACCUM_BLUE_BITS_ARB                          = $2020;
   WGL_ACCUM_ALPHA_BITS_ARB                         = $2021;
   WGL_DEPTH_BITS_ARB                               = $2022;
   WGL_STENCIL_BITS_ARB                             = $2023;
   WGL_AUX_BUFFERS_ARB                              = $2024;
   WGL_NO_ACCELERATION_ARB                          = $2025;
   WGL_GENERIC_ACCELERATION_ARB                     = $2026;
   WGL_FULL_ACCELERATION_ARB                        = $2027;
   WGL_SWAP_EXCHANGE_ARB                            = $2028;
   WGL_SWAP_COPY_ARB                                = $2029;
   WGL_SWAP_UNDEFINED_ARB                           = $202A;
   WGL_TYPE_RGBA_ARB                                = $202B;
   WGL_TYPE_COLORINDEX_ARB                          = $202C;

   // ARB Extension #10 - WGL_ARB_make_current_read
   ERROR_INVALID_PIXEL_TYPE_ARB                     = $2043;
   ERROR_INCOMPATIBLE_DEVICE_CONTEXTS_ARB           = $2054;

   // ARB Extension #11 - WGL_ARB_pbuffer
   WGL_DRAW_TO_PBUFFER_ARB                          = $202D;
   WGL_MAX_PBUFFER_PIXELS_ARB                       = $202E;
   WGL_MAX_PBUFFER_WIDTH_ARB                        = $202F;
   WGL_MAX_PBUFFER_HEIGHT_ARB                       = $2030;
   WGL_PBUFFER_LARGEST_ARB                          = $2033;
   WGL_PBUFFER_WIDTH_ARB                            = $2034;
   WGL_PBUFFER_HEIGHT_ARB                           = $2035;
   WGL_PBUFFER_LOST_ARB                             = $2036;

   // ARB Extension #12 - GL_ARB_texture_compression
   GL_COMPRESSED_ALPHA_ARB                           = $84E9;
   GL_COMPRESSED_LUMINANCE_ARB                       = $84EA;
   GL_COMPRESSED_LUMINANCE_ALPHA_ARB                 = $84EB;
   GL_COMPRESSED_INTENSITY_ARB                       = $84EC;
   GL_COMPRESSED_RGB_ARB                             = $84ED;
   GL_COMPRESSED_RGBA_ARB                            = $84EE;
   GL_TEXTURE_COMPRESSION_HINT_ARB                   = $84EF;
   GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB              = $86A0;
   GL_TEXTURE_COMPRESSED_ARB                         = $86A1;
   GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB             = $86A2;
   GL_COMPRESSED_TEXTURE_FORMATS_ARB                 = $86A3;

   // ARB Extension #13 - GL_ARB_texture_border_clamp
   // (promoted from #36 GL_SGIS_texture_border_clamp)
   GL_CLAMP_TO_BORDER_ARB                            = $812D;

   // ARB Extension #14 - GL_ARB_point_parameters
   // (promoted from #54 GL_{SGIS,EXT}_point_parameters)
   GL_POINT_SIZE_MIN_ARB                             = $8126;
   GL_POINT_SIZE_MAX_ARB                             = $8127;
   GL_POINT_FADE_THRESHOLD_SIZE_ARB                  = $8128;
   GL_DISTANCE_ATTENUATION_ARB                       = $8129;

   // ARB Extension #15 - GL_ARB_vertex_blend
   GL_MAX_VERTEX_UNITS_ARB                           = $86A4;
   GL_ACTIVE_VERTEX_UNITS_ARB                        = $86A5;
   GL_WEIGHT_SUM_UNITY_ARB                           = $86A6;
   GL_VERTEX_BLEND_ARB                               = $86A7;
   GL_CURRENT_WEIGHT_ARB                             = $86A8;
   GL_WEIGHT_ARRAY_TYPE_ARB                          = $86A9;
   GL_WEIGHT_ARRAY_STRIDE_ARB                        = $86AA;
   GL_WEIGHT_ARRAY_SIZE_ARB                          = $86AB;
   GL_WEIGHT_ARRAY_POINTER_ARB                       = $86AC;
   GL_WEIGHT_ARRAY_ARB                               = $86AD;
   GL_MODELVIEW0_ARB                                 = $1700;
   GL_MODELVIEW1_ARB                                 = $850A;
   GL_MODELVIEW2_ARB                                = $8722;
   GL_MODELVIEW3_ARB                                = $8723;
   GL_MODELVIEW4_ARB                                = $8724;
   GL_MODELVIEW5_ARB                                = $8725;
   GL_MODELVIEW6_ARB                                = $8726;
   GL_MODELVIEW7_ARB                                = $8727;
   GL_MODELVIEW8_ARB                                = $8728;
   GL_MODELVIEW9_ARB                                = $8729;
   GL_MODELVIEW10_ARB                               = $872A;
   GL_MODELVIEW11_ARB                               = $872B;
   GL_MODELVIEW12_ARB                               = $872C;
   GL_MODELVIEW13_ARB                               = $872D;
   GL_MODELVIEW14_ARB                               = $872E;
   GL_MODELVIEW15_ARB                               = $872F;
   GL_MODELVIEW16_ARB                               = $8730;
   GL_MODELVIEW17_ARB                               = $8731;
   GL_MODELVIEW18_ARB                               = $8732;
   GL_MODELVIEW19_ARB                               = $8733;
   GL_MODELVIEW20_ARB                               = $8734;
   GL_MODELVIEW21_ARB                               = $8735;
   GL_MODELVIEW22_ARB                               = $8736;
   GL_MODELVIEW23_ARB                               = $8737;
   GL_MODELVIEW24_ARB                               = $8738;
   GL_MODELVIEW25_ARB                               = $8739;
   GL_MODELVIEW26_ARB                               = $873A;
   GL_MODELVIEW27_ARB                               = $873B;
   GL_MODELVIEW28_ARB                               = $873C;
   GL_MODELVIEW29_ARB                               = $873D;
   GL_MODELVIEW30_ARB                               = $873E;
   GL_MODELVIEW31_ARB                               = $873F;

   // ARB Extension #16 - GL_ARB_matrix_palette
   GL_MATRIX_PALETTE_ARB                               = $8840;
   GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB               = $8841;
   GL_MAX_PALETTE_MATRICES_ARB                         = $8842;
   GL_CURRENT_PALETTE_MATRIX_ARB                       = $8843;
   GL_MATRIX_INDEX_ARRAY_ARB                           = $8844;
   GL_CURRENT_MATRIX_INDEX_ARB                         = $8845;
   GL_MATRIX_INDEX_ARRAY_SIZE_ARB                      = $8846;
   GL_MATRIX_INDEX_ARRAY_TYPE_ARB                      = $8847;
   GL_MATRIX_INDEX_ARRAY_STRIDE_ARB                    = $8848;
   GL_MATRIX_INDEX_ARRAY_POINTER_ARB                   = $8849;

   // ARB Extension #17 - GL_ARB_texture_env_combine
   // (Shares enum values with #158 GL_EXT_texture_env_combine)
   GL_COMBINE_ARB                                    = $8570;
   GL_COMBINE_RGB_ARB                                = $8571;
   GL_COMBINE_ALPHA_ARB                              = $8572;
   GL_RGB_SCALE_ARB                                  = $8573;
   GL_ADD_SIGNED_ARB                                 = $8574;
   GL_INTERPOLATE_ARB                                = $8575;
   GL_CONSTANT_ARB                                   = $8576;
   GL_CONSTANT_COLOR_ARB                             = $8576;
   GL_PRIMARY_COLOR_ARB                              = $8577;
   GL_PREVIOUS_ARB                                   = $8578;
   GL_SOURCE0_RGB_ARB                                = $8580;
   GL_SOURCE1_RGB_ARB                                = $8581;
   GL_SOURCE2_RGB_ARB                                = $8582;
   GL_SOURCE0_ALPHA_ARB                              = $8588;
   GL_SOURCE1_ALPHA_ARB                              = $8589;
   GL_SOURCE2_ALPHA_ARB                              = $858A;
   GL_OPERAND0_RGB_ARB                               = $8590;
   GL_OPERAND1_RGB_ARB                               = $8591;
   GL_OPERAND2_RGB_ARB                               = $8592;
   GL_OPERAND0_ALPHA_ARB                             = $8598;
   GL_OPERAND1_ALPHA_ARB                             = $8599;
   GL_OPERAND2_ALPHA_ARB                             = $859A;
   GL_SUBTRACT_ARB                                   = $84E7;

   // ARB Extension #18 - GL_ARB_texture_env_crossbar
   // (no new tokens)

   // ARB Extension #19 - GL_ARB_texture_env_dot3
   // (promoted from #220 GL_EXT_texture_env_dot3; enum values changed)
   GL_DOT3_RGB_ARB                                   = $86AE;
   GL_DOT3_RGBA_ARB                                  = $86AF;

   // ARB Extension #20 - WGL_ARB_render_texture
   WGL_BIND_TO_TEXTURE_RGB_ARB                       = $2070;
   WGL_BIND_TO_TEXTURE_RGBA_ARB                      = $2071;
   WGL_TEXTURE_FORMAT_ARB                            = $2072;
   WGL_TEXTURE_TARGET_ARB                            = $2073;
   WGL_MIPMAP_TEXTURE_ARB                            = $2074;
   WGL_TEXTURE_RGB_ARB                               = $2075;
   WGL_TEXTURE_RGBA_ARB                              = $2076;
   WGL_NO_TEXTURE_ARB                                = $2077;
   WGL_TEXTURE_CUBE_MAP_ARB                          = $2078;
   WGL_TEXTURE_1D_ARB                                = $2079;
   WGL_TEXTURE_2D_ARB                                = $207A;
   WGL_MIPMAP_LEVEL_ARB                              = $207B;
   WGL_CUBE_MAP_FACE_ARB                             = $207C;
   WGL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB               = $207D;
   WGL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB               = $207E;
   WGL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB               = $207F;
   WGL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB               = $2080;
   WGL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB               = $2081;
   WGL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB               = $2082;
   WGL_FRONT_LEFT_ARB                                = $2083;
   WGL_FRONT_RIGHT_ARB                               = $2084;
   WGL_BACK_LEFT_ARB                                 = $2085;
   WGL_BACK_RIGHT_ARB                                = $2086;
   WGL_AUX0_ARB                                      = $2087;
   WGL_AUX1_ARB                                      = $2088;
   WGL_AUX2_ARB                                      = $2089;
   WGL_AUX3_ARB                                      = $208A;
   WGL_AUX4_ARB                                      = $208B;
   WGL_AUX5_ARB                                      = $208C;
   WGL_AUX6_ARB                                      = $208D;
   WGL_AUX7_ARB                                      = $208E;
   WGL_AUX8_ARB                                      = $208F;
   WGL_AUX9_ARB                                      = $2090;

   // ARB Extension #21 - GL_ARB_texture_mirrored_repeat
   GL_MIRRORED_REPEAT_ARB                            = $8370;

   // ARB Extension #22 - GL_ARB_depth_texture
   GL_DEPTH_COMPONENT16_ARB                          = $81A5;
   GL_DEPTH_COMPONENT24_ARB                          = $81A6;
   GL_DEPTH_COMPONENT32_ARB                          = $81A7;
   GL_TEXTURE_DEPTH_SIZE_ARB                         = $884A;
   GL_DEPTH_TEXTURE_MODE_ARB                         = $884B;

   // ARB Extension #23 - GL_ARB_shadow
   GL_TEXTURE_COMPARE_MODE_ARB                       = $884C;
   GL_TEXTURE_COMPARE_FUNC_ARB                       = $884D;
   GL_COMPARE_R_TO_TEXTURE_ARB                       = $884E;

   // ARB Extension #24 - GL_ARB_shadow_ambient
   // (same as #90 GL_SGIX_shadow_ambient)
   GL_TEXTURE_COMPARE_FAIL_VALUE_ARB                 = $80BF;

   // ARB Extension #25 - GL_ARB_window_pos
   // (no new tokens)

   // ARB Extension #26 - GL_ARB_vertex_program
   // GL_ARB_vertex_program enums are shared by GL_ARB_fragment_program are so marked.
   // Unfortunately, PROGRAM_BINDING_ARB does accidentally reuse 0x8677 -
   //   this was a spec editing typo that's now uncorrectable.
   GL_COLOR_SUM_ARB                                  = $8458;
   GL_VERTEX_PROGRAM_ARB                             = $8620;
   GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB                = $8622;
   GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB                   = $8623;
   GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB                 = $8624;
   GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB                   = $8625;
   GL_CURRENT_VERTEX_ATTRIB_ARB                      = $8626;
   GL_PROGRAM_LENGTH_ARB                             = $8627;  //shared
   GL_PROGRAM_STRING_ARB                             = $8628;  //shared
   GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB             = $862E;  //shared
   GL_MAX_PROGRAM_MATRICES_ARB                       = $862F;  //shared
   GL_CURRENT_MATRIX_STACK_DEPTH_ARB                 = $8640;  //shared
   GL_CURRENT_MATRIX_ARB                             = $8641;  //shared
   GL_VERTEX_PROGRAM_POINT_SIZE_ARB                  = $8642;
   GL_VERTEX_PROGRAM_TWO_SIDE_ARB                    = $8643;
   GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB                = $8645;
   GL_PROGRAM_ERROR_POSITION_ARB                     = $864B;  //shared
   GL_PROGRAM_BINDING_ARB                            = $8677;  //shared
   GL_MAX_VERTEX_ATTRIBS_ARB                         = $8869;
   GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB             = $886A;

   GL_PROGRAM_ERROR_STRING_ARB                       = $8874;  //shared
   GL_PROGRAM_FORMAT_ASCII_ARB                       = $8875;  //shared
   GL_PROGRAM_FORMAT_ARB                             = $8876;  //shared

   GL_PROGRAM_INSTRUCTIONS_ARB                       = $88A0;  //shared
   GL_MAX_PROGRAM_INSTRUCTIONS_ARB                   = $88A1;  //shared
   GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB                = $88A2;  //shared
   GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB            = $88A3;  //shared
   GL_PROGRAM_TEMPORARIES_ARB                        = $88A4;  //shared
   GL_MAX_PROGRAM_TEMPORARIES_ARB                    = $88A5;  //shared
   GL_PROGRAM_NATIVE_TEMPORARIES_ARB                 = $88A6;  //shared
   GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB             = $88A7;  //shared
   GL_PROGRAM_PARAMETERS_ARB                         = $88A8;  //shared
   GL_MAX_PROGRAM_PARAMETERS_ARB                     = $88A9;  //shared
   GL_PROGRAM_NATIVE_PARAMETERS_ARB                  = $88AA;  //shared
   GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB              = $88AB;  //shared
   GL_PROGRAM_ATTRIBS_ARB                            = $88AC;  //shared
   GL_MAX_PROGRAM_ATTRIBS_ARB                        = $88AD;  //shared
   GL_PROGRAM_NATIVE_ATTRIBS_ARB                     = $88AE;  //shared
   GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB                 = $88AF;  //shared
   GL_PROGRAM_ADDRESS_REGISTERS_ARB                  = $88B0;  //shared
   GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB              = $88B1;  //shared
   GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB           = $88B2;  //shared
   GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB       = $88B3;  //shared
   GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB               = $88B4;  //shared
   GL_MAX_PROGRAM_ENV_PARAMETERS_ARB                 = $88B5;  //shared
   GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB                = $88B6;  //shared
   GL_TRANSPOSE_CURRENT_MATRIX_ARB                   = $88B7;  //shared

   GL_MATRIX0_ARB                                    = $88C0;  //shared
   GL_MATRIX1_ARB                                    = $88C1;  //shared
   GL_MATRIX2_ARB                                    = $88C2;  //shared
   GL_MATRIX3_ARB                                    = $88C3;  //shared
   GL_MATRIX4_ARB                                    = $88C4;  //shared
   GL_MATRIX5_ARB                                    = $88C5;  //shared
   GL_MATRIX6_ARB                                    = $88C6;  //shared
   GL_MATRIX7_ARB                                    = $88C7;  //shared
   GL_MATRIX8_ARB                                    = $88C8;  //shared
   GL_MATRIX9_ARB                                    = $88C9;  //shared
   GL_MATRIX10_ARB                                   = $88CA;  //shared
   GL_MATRIX11_ARB                                   = $88CB;  //shared
   GL_MATRIX12_ARB                                   = $88CC;  //shared
   GL_MATRIX13_ARB                                   = $88CD;  //shared
   GL_MATRIX14_ARB                                   = $88CE;  //shared
   GL_MATRIX15_ARB                                   = $88CF;  //shared
   GL_MATRIX16_ARB                                   = $88D0;  //shared
   GL_MATRIX17_ARB                                   = $88D1;  //shared
   GL_MATRIX18_ARB                                   = $88D2;  //shared
   GL_MATRIX19_ARB                                   = $88D3;  //shared
   GL_MATRIX20_ARB                                   = $88D4;  //shared
   GL_MATRIX21_ARB                                   = $88D5;  //shared
   GL_MATRIX22_ARB                                   = $88D6;  //shared
   GL_MATRIX23_ARB                                   = $88D7;  //shared
   GL_MATRIX24_ARB                                   = $88D8;  //shared
   GL_MATRIX25_ARB                                   = $88D9;  //shared
   GL_MATRIX26_ARB                                   = $88DA;  //shared
   GL_MATRIX27_ARB                                   = $88DB;  //shared
   GL_MATRIX28_ARB                                   = $88DC;  //shared
   GL_MATRIX29_ARB                                   = $88DD;  //shared
   GL_MATRIX30_ARB                                   = $88DE;  //shared
   GL_MATRIX31_ARB                                   = $88DF;  //shared

   // ARB Extension #27 - GL_ARB_fragment_program
   // Some GL_ARB_fragment_program enums are shared with #26 GL_ARB_vertex_program,
   //  and are included in there for now.
   GL_FRAGMENT_PROGRAM_ARB                           = $8804;
   GL_PROGRAM_ALU_INSTRUCTIONS_ARB                   = $8805;
   GL_PROGRAM_TEX_INSTRUCTIONS_ARB                   = $8806;
   GL_PROGRAM_TEX_INDIRECTIONS_ARB                   = $8807;
   GL_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB            = $8808;
   GL_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB            = $8809;
   GL_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB            = $880A;
   GL_MAX_PROGRAM_ALU_INSTRUCTIONS_ARB               = $880B;
   GL_MAX_PROGRAM_TEX_INSTRUCTIONS_ARB               = $880C;
   GL_MAX_PROGRAM_TEX_INDIRECTIONS_ARB               = $880D;
   GL_MAX_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB        = $880E;
   GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB        = $880F;
   GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB        = $8810;
   GL_MAX_TEXTURE_COORDS_ARB                         = $8871;
   GL_MAX_TEXTURE_IMAGE_UNITS_ARB                    = $8872;

   // ARB Extension #28 - GL_ARB_vertex_buffer_object
   GL_BUFFER_SIZE_ARB                                = $8764;
   GL_BUFFER_USAGE_ARB                               = $8765;
   GL_ARRAY_BUFFER_ARB                               = $8892;
   GL_ELEMENT_ARRAY_BUFFER_ARB                       = $8893;
   GL_ARRAY_BUFFER_BINDING_ARB                       = $8894;
   GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB               = $8895;
   GL_VERTEX_ARRAY_BUFFER_BINDING_ARB                = $8896;
   GL_NORMAL_ARRAY_BUFFER_BINDING_ARB                = $8897;
   GL_COLOR_ARRAY_BUFFER_BINDING_ARB                 = $8898;
   GL_INDEX_ARRAY_BUFFER_BINDING_ARB                 = $8899;
   GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB         = $889A;
   GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB             = $889B;
   GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB       = $889C;
   GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB        = $889D;
   GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB                = $889E;
   GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB         = $889F;
   GL_READ_ONLY_ARB                                  = $88B8;
   GL_WRITE_ONLY_ARB                                 = $88B9;
   GL_READ_WRITE_ARB                                 = $88BA;
   GL_BUFFER_ACCESS_ARB                              = $88BB;
   GL_BUFFER_MAPPED_ARB                              = $88BC;
   GL_BUFFER_MAP_POINTER_ARB                         = $88BD;
   GL_STREAM_DRAW_ARB                                = $88E0;
   GL_STREAM_READ_ARB                                = $88E1;
   GL_STREAM_COPY_ARB                                = $88E2;
   GL_STATIC_DRAW_ARB                                = $88E4;
   GL_STATIC_READ_ARB                                = $88E5;
   GL_STATIC_COPY_ARB                                = $88E6;
   GL_DYNAMIC_DRAW_ARB                               = $88E8;
   GL_DYNAMIC_READ_ARB                               = $88E9;
   GL_DYNAMIC_COPY_ARB                               = $88EA;

   // ARB Extension #29 - GL_ARB_occlusion_query
   // (promoted from GL_HP_occulsion_query / GL_NV_occlusion_query)
   GL_QUERY_COUNTER_BITS_ARB                         = $8864;
   GL_CURRENT_QUERY_ARB                              = $8865;
   GL_QUERY_RESULT_ARB                               = $8866;
   GL_QUERY_RESULT_AVAILABLE_ARB                     = $8867;
   GL_SAMPLES_PASSED_ARB                             = $8914;

   // ARB Extension #30 - GL_ARB_shader_objects
   GL_PROGRAM_OBJECT_ARB                             = $8B40;
   GL_SHADER_OBJECT_ARB                              = $8B48;
   GL_OBJECT_TYPE_ARB                                = $8B4E;
   GL_OBJECT_SUBTYPE_ARB                             = $8B4F;
   GL_FLOAT_VEC2_ARB                                 = $8B50;
   GL_FLOAT_VEC3_ARB                                 = $8B51;
   GL_FLOAT_VEC4_ARB                                 = $8B52;
   GL_INT_VEC2_ARB                                   = $8B53;
   GL_INT_VEC3_ARB                                   = $8B54;
   GL_INT_VEC4_ARB                                   = $8B55;
   GL_BOOL_ARB                                       = $8B56;
   GL_BOOL_VEC2_ARB                                  = $8B57;
   GL_BOOL_VEC3_ARB                                  = $8B58;
   GL_BOOL_VEC4_ARB                                  = $8B59;
   GL_FLOAT_MAT2_ARB                                 = $8B5A;
   GL_FLOAT_MAT3_ARB                                 = $8B5B;
   GL_FLOAT_MAT4_ARB                                 = $8B5C;
   GL_SAMPLER_1D_ARB                                 = $8B5D;
   GL_SAMPLER_2D_ARB                                 = $8B5E;
   GL_SAMPLER_3D_ARB                                 = $8B5F;
   GL_SAMPLER_CUBE_ARB                               = $8B60;
   GL_SAMPLER_1D_SHADOW_ARB                          = $8B61;
   GL_SAMPLER_2D_SHADOW_ARB                          = $8B62;
   GL_SAMPLER_2D_RECT_ARB                            = $8B63;
	 GL_SAMPLER_2D_RECT_SHADOW_ARB                     = $8B64;
   GL_OBJECT_DELETE_STATUS_ARB                       = $8B80;
   GL_OBJECT_COMPILE_STATUS_ARB                      = $8B81;
   GL_OBJECT_LINK_STATUS_ARB                         = $8B82;
   GL_OBJECT_VALIDATE_STATUS_ARB                     = $8B83;
   GL_OBJECT_INFO_LOG_LENGTH_ARB                     = $8B84;
   GL_OBJECT_ATTACHED_OBJECTS_ARB                    = $8B85;
   GL_OBJECT_ACTIVE_UNIFORMS_ARB                     = $8B86;
   GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB           = $8B87;
   GL_OBJECT_SHADER_SOURCE_LENGTH_ARB                = $8B88;

   // ARB Extension #31 - GL_ARB_vertex_shader
   // (additional enums are reused from:
   //  #26 GL_ARB_vertex_program
   //  #27 GL_ARB_fragment_program
   //  #30 GL_ARB_shader_objects)
   GL_VERTEX_SHADER_ARB                              = $8B31;
   GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB              = $8B4A;
   GL_MAX_VARYING_FLOATS_ARB                         = $8B4B;
   GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB             = $8B4C;
   GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB           = $8B4D;
   GL_OBJECT_ACTIVE_ATTRIBUTES_ARB                   = $8B89;
   GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB         = $8B8A;

   // ARB Extension #32 - GL_ARB_fragment_shader
   // (additional enums are reused from #27 GL_ARB_fragment_program and #30 GL_ARB_shader_objects)
   GL_FRAGMENT_SHADER_ARB                            = $8B30;
   GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB            = $8B49;
   GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB            = $8B8B;

   // ARB Extension #33 - GL_ARB_shading_language_100
   GL_SHADING_LANGUAGE_VERSION_ARB                   = $8B8C;

   // ARB Extension #34 - GL_ARB_texture_non_power_of_two
   // (no new tokens)

   // ARB Extension #35 - GL_ARB_point_sprite
   GL_POINT_SPRITE_ARB                               = $8861;
   GL_COORD_REPLACE_ARB                              = $8862;

   // ARB Extension #36 - GL_ARB_fragment_program_shadow
   // (no new tokens)

   // ARB Extension #37 - GL_ARB_draw_buffers
   GL_MAX_DRAW_BUFFERS_ARB                           = $8824;
   GL_DRAW_BUFFER0_ARB                               = $8825;
   GL_DRAW_BUFFER1_ARB                               = $8826;
   GL_DRAW_BUFFER2_ARB                               = $8827;
   GL_DRAW_BUFFER3_ARB                               = $8828;
   GL_DRAW_BUFFER4_ARB                               = $8829;
   GL_DRAW_BUFFER5_ARB                               = $882A;
   GL_DRAW_BUFFER6_ARB                               = $882B;
   GL_DRAW_BUFFER7_ARB                               = $882C;
   GL_DRAW_BUFFER8_ARB                               = $882D;
   GL_DRAW_BUFFER9_ARB                               = $882E;
   GL_DRAW_BUFFER10_ARB                              = $882F;
   GL_DRAW_BUFFER11_ARB                              = $8830;
   GL_DRAW_BUFFER12_ARB                              = $8831;
   GL_DRAW_BUFFER13_ARB                              = $8832;
   GL_DRAW_BUFFER14_ARB                              = $8833;
   GL_DRAW_BUFFER15_ARB                              = $8834;

   // ARB Extension #38 - GL_ARB_texture_rectangle
   GL_TEXTURE_RECTANGLE_ARB                          = $84F5;
   GL_TEXTURE_BINDING_RECTANGLE_ARB                  = $84F6;
   GL_PROXY_TEXTURE_RECTANGLE_ARB                    = $84F7;
   GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB                 = $84F8;

   // ARB Extension #39 - GL_ARB_color_buffer_float
   //                   - WGL_ARB_pixel_format_float
   //                   - GLX_ARB_fbconfig_float
   GL_RGBA_FLOAT_MODE_ARB                            = $8820;
   GL_CLAMP_VERTEX_COLOR_ARB                         = $891A;
   GL_CLAMP_FRAGMENT_COLOR_ARB                       = $891B;
   GL_CLAMP_READ_COLOR_ARB                           = $891C;
   GL_FIXED_ONLY_ARB                                 = $891D;

   WGL_TYPE_RGBA_FLOAT_ARB                           = $21A0;
   GLX_RGBA_FLOAT_TYPE                               = $20B9;
   GLX_RGBA_FLOAT_BIT                                = $00000004;

   // ARB Extension #40 - GL_ARB_half_float_pixel
   GL_HALF_FLOAT_ARB                                 = $140B;

   // ARB Extension #41 - GL_ARB_texture_float
   GL_TEXTURE_RED_TYPE_ARB                           = $8C10;
   GL_TEXTURE_GREEN_TYPE_ARB                         = $8C11;
   GL_TEXTURE_BLUE_TYPE_ARB                          = $8C12;
   GL_TEXTURE_ALPHA_TYPE_ARB                         = $8C13;
   GL_TEXTURE_LUMINANCE_TYPE_ARB                     = $8C14;
   GL_TEXTURE_INTENSITY_TYPE_ARB                     = $8C15;
   GL_TEXTURE_DEPTH_TYPE_ARB                         = $8C16;
   GL_UNSIGNED_NORMALIZED_ARB                        = $8C17;
   GL_RGBA32F_ARB                                    = $8814;
   GL_RGB32F_ARB                                     = $8815;
   GL_ALPHA32F_ARB                                   = $8816;
   GL_INTENSITY32F_ARB                               = $8817;
   GL_LUMINANCE32F_ARB                               = $8818;
   GL_LUMINANCE_ALPHA32F_ARB                         = $8819;
   GL_RGBA16F_ARB                                    = $881A;
   GL_RGB16F_ARB                                     = $881B;
   GL_ALPHA16F_ARB                                   = $881C;
   GL_INTENSITY16F_ARB                               = $881D;
   GL_LUMINANCE16F_ARB                               = $881E;
   GL_LUMINANCE_ALPHA16F_ARB                         = $881F;

   // ARB Extension #42 - GL_ARB_pixel_buffer_object
   GL_PIXEL_PACK_BUFFER_ARB                          = $88EB;
   GL_PIXEL_UNPACK_BUFFER_ARB                        = $88EC;
   GL_PIXEL_PACK_BUFFER_BINDING_ARB                  = $88ED;
   GL_PIXEL_UNPACK_BUFFER_BINDING_ARB                = $88EF;

   // ARB Extension #43 - GL_ARB_depth_buffer_float
   GL_DEPTH_COMPONENT32F                             = $8CAC;
   GL_DEPTH32F_STENCIL8                              = $8CAD;
   GL_FLOAT_32_UNSIGNED_INT_24_8_REV                 = $8DAD;

   // ARB Extension #44 - GL_ARB_draw_instanced
   // (no new tokens)

   // ARB Extension #45 - GL_ARB_framebuffer_object
   // (Also went simultaneously to core 3.0, so no ARB prefix on names)
   GL_INVALID_FRAMEBUFFER_OPERATION			= $0506;
	 GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING		= $8210;
	 GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE		= $8211;
	 GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE			= $8212;
	 GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE		= $8213;
	 GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE		= $8214;
	 GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE		= $8215;
	 GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE		= $8216;
	 GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE		= $8217;
	 GL_FRAMEBUFFER_DEFAULT				= $8218;
	 GL_FRAMEBUFFER_UNDEFINED				= $8219;
	 GL_DEPTH_STENCIL_ATTACHMENT			= $821A;
	 GL_INDEX						= $8222;
	 GL_MAX_RENDERBUFFER_SIZE				= $84E8;
	 GL_DEPTH_STENCIL					= $84F9;
	 GL_UNSIGNED_INT_24_8				= $84FA;
	 GL_DEPTH24_STENCIL8				= $88F0;
	 GL_TEXTURE_STENCIL_SIZE				= $88F1;
	 GL_TEXTURE_RED_TYPE				= $8C10;
	 GL_TEXTURE_GREEN_TYPE				= $8C11;
	 GL_TEXTURE_BLUE_TYPE				= $8C12;
	 GL_TEXTURE_ALPHA_TYPE				= $8C13;
	 GL_TEXTURE_LUMINANCE_TYPE				= $8C14;
	 GL_TEXTURE_INTENSITY_TYPE				= $8C15;
	 GL_TEXTURE_DEPTH_TYPE				= $8C16;
	 GL_UNSIGNED_NORMALIZED				= $8C17;
	 GL_FRAMEBUFFER_BINDING				= $8CA6;
	 GL_DRAW_FRAMEBUFFER_BINDING			= GL_FRAMEBUFFER_BINDING;
	 GL_RENDERBUFFER_BINDING				= $8CA7;
	 GL_READ_FRAMEBUFFER				= $8CA8;
	 GL_DRAW_FRAMEBUFFER				= $8CA9;
	 GL_READ_FRAMEBUFFER_BINDING			= $8CAA;
	 GL_RENDERBUFFER_SAMPLES				= $8CAB;
	 GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE		= $8CD0;
	 GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME		= $8CD1;
	 GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL		= $8CD2;
	 GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE	= $8CD3;
	 GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER		= $8CD4;
	 GL_FRAMEBUFFER_COMPLETE				= $8CD5;
	 GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT		= $8CD6;
	 GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT	= $8CD7;
	 GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER		= $8CDB;
	 GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER		= $8CDC;
	 GL_FRAMEBUFFER_UNSUPPORTED				= $8CDD;
	 GL_MAX_COLOR_ATTACHMENTS				= $8CDF;
	 GL_COLOR_ATTACHMENT0				= $8CE0;
	 GL_COLOR_ATTACHMENT1				= $8CE1;
	 GL_COLOR_ATTACHMENT2				= $8CE2;
	 GL_COLOR_ATTACHMENT3				= $8CE3;
	 GL_COLOR_ATTACHMENT4				= $8CE4;
	 GL_COLOR_ATTACHMENT5				= $8CE5;
	 GL_COLOR_ATTACHMENT6				= $8CE6;
	 GL_COLOR_ATTACHMENT7				= $8CE7;
	 GL_COLOR_ATTACHMENT8				= $8CE8;
	 GL_COLOR_ATTACHMENT9				= $8CE9;
	 GL_COLOR_ATTACHMENT10				= $8CEA;
	 GL_COLOR_ATTACHMENT11				= $8CEB;
	 GL_COLOR_ATTACHMENT12				= $8CEC;
	 GL_COLOR_ATTACHMENT13				= $8CED;
	 GL_COLOR_ATTACHMENT14				= $8CEE;
	 GL_COLOR_ATTACHMENT15				= $8CEF;
	 GL_DEPTH_ATTACHMENT				= $8D00;
	 GL_STENCIL_ATTACHMENT				= $8D20;
	 GL_FRAMEBUFFER					= $8D40;
	 GL_RENDERBUFFER					= $8D41;
	 GL_RENDERBUFFER_WIDTH				= $8D42;
	 GL_RENDERBUFFER_HEIGHT				= $8D43;
	 GL_RENDERBUFFER_INTERNAL_FORMAT			= $8D44;
	 GL_STENCIL_INDEX1					= $8D46;
	 GL_STENCIL_INDEX4					= $8D47;
	 GL_STENCIL_INDEX8					= $8D48;
	 GL_STENCIL_INDEX16					= $8D49;
	 GL_RENDERBUFFER_RED_SIZE				= $8D50;
	 GL_RENDERBUFFER_GREEN_SIZE				= $8D51;
	 GL_RENDERBUFFER_BLUE_SIZE				= $8D52;
	 GL_RENDERBUFFER_ALPHA_SIZE				= $8D53;
	 GL_RENDERBUFFER_DEPTH_SIZE				= $8D54;
	 GL_RENDERBUFFER_STENCIL_SIZE			= $8D55;
	 GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE		= $8D56;
	 GL_MAX_SAMPLES					= $8D57;

   // ARB Extension #46 -  GL_ARB_framebuffer_sRGB
   //                      GLX_ARB_framebuffer_sRGB
   //                      WGL_ARB_framebuffer_sRGB
   GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB                  = $20B2;
   WGL_FRAMEBUFFER_SRGB_CAPABLE_ARB                  = $20A9;
   GL_FRAMEBUFFER_SRGB                               = $8DB9;
   //GL_FRAMEBUFFER_SRGB_CAPABLE                       = $8DBA;

   // ARB Extension #47 - GL_ARB_geometry_shader4
   GL_GEOMETRY_SHADER_ARB                            =$8DD9;
   GL_GEOMETRY_VERTICES_OUT_ARB                      =$8DDA;
   GL_GEOMETRY_INPUT_TYPE_ARB                        =$8DDB;
   GL_GEOMETRY_OUTPUT_TYPE_ARB                       =$8DDC;
   GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_ARB           =$8C29;
   GL_MAX_GEOMETRY_VARYING_COMPONENTS_ARB            =$8DDD;
   GL_MAX_VERTEX_VARYING_COMPONENTS_ARB              =$8DDE;
   GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_ARB              =$8DDF;
   GL_MAX_GEOMETRY_OUTPUT_VERTICES_ARB                 =$8DE0;
   GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_ARB         =$8DE1;
   GL_LINES_ADJACENCY_ARB                              =$A;
   GL_LINE_STRIP_ADJACENCY_ARB                         =$B;
   GL_TRIANGLES_ADJACENCY_ARB                          =$C;
   GL_TRIANGLE_STRIP_ADJACENCY_ARB                     =$D;
   GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_ARB         =$8DA8;
   GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_ARB           =$8DA9;
   GL_FRAMEBUFFER_ATTACHMENT_LAYERED_ARB               =$8DA7;
   //GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER             =$8CD4;
   GL_PROGRAM_POINT_SIZE_ARB                           =$8642;

   // ARB Extension #48 - GL_ARB_half_float_vertex
   GL_HALF_FLOAT                                       =$140B;

   // ARB Extension #49 - GL_ARB_instanced_arrays
   GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ARB                  =$88FE;

   // ARB Extension #50 - GL_ARB_map_buffer_range
   GL_MAP_READ_BIT                                     =$0001;
   GL_MAP_WRITE_BIT                                    =$0002;
   GL_MAP_INVALIDATE_RANGE_BIT                         =$0004;
   GL_MAP_INVALIDATE_BUFFER_BIT                        =$0008;
   GL_MAP_FLUSH_EXPLICIT_BIT                           =$0010;
   GL_MAP_UNSYNCHRONIZED_BIT                           =$0020;

   // ARB Extension #51 - GL_ARB_texture_buffer_object
   GL_TEXTURE_BUFFER_ARB                               =$8C2A;
   GL_MAX_TEXTURE_BUFFER_SIZE_ARB                      =$8C2B;
   GL_TEXTURE_BINDING_BUFFER_ARB                       =$8C2C;
   GL_TEXTURE_BUFFER_DATA_STORE_BINDING_ARB            =$8C2D;
   GL_TEXTURE_BUFFER_FORMAT_ARB                        =$8C2E;

   // ARB Extension #52 - GL_ARB_texture_compression_rgtc
   GL_COMPRESSED_RED_RGTC1                             =$8DBB;
   GL_COMPRESSED_SIGNED_RED_RGTC1                      =$8DBC;
   GL_COMPRESSED_RG_RGTC2                              =$8DBD;
   GL_COMPRESSED_SIGNED_RG_RGTC2                       =$8DBE;

   // ARB Extension #53 - GL_ARB_texture_rg
   GL_R8                      =$8229;
   GL_R16                     =$822A;
   GL_RG8                     =$822B;
   GL_RG16                    =$822C;
   GL_R16F                    =$822D;
   GL_R32F                    =$822E;
   GL_RG16F                   =$822F;
   GL_RG32F                   =$8230;
   GL_R8I                     =$8231;
   GL_R8UI                    =$8232;
   GL_R16I                    =$8233;
   GL_R16UI                   =$8234;
   GL_R32I                    =$8235;
   GL_R32UI                   =$8236;
   GL_RG8I                    =$8237;
   GL_RG8UI                   =$8238;
   GL_RG16I                   =$8239;
   GL_RG16UI                  =$823A;
   GL_RG32I                   =$823B;
   GL_RG32UI                  =$823C;
   GL_RG                      =$8227;
   GL_RG_INTEGER              =$8228;

   // ARB Extension #54 - GL_ARB_vertex_array_object
   GL_VERTEX_ARRAY_BINDING                             =$85B5;
   
   // ARB Extension #55 - WGL_ARB_create_context
   // see also WGL_ARB_create_context_profile (ARB #74)
   WGL_CONTEXT_MAJOR_VERSION_ARB                       =$2091;
   WGL_CONTEXT_MINOR_VERSION_ARB                       =$2092;
   WGL_CONTEXT_LAYER_PLANE_ARB                         =$2093;
   WGL_CONTEXT_FLAGS_ARB                               =$2094;
   WGL_CONTEXT_DEBUG_BIT_ARB                           =$0001;
   WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB              =$0002;
   ERROR_INVALID_VERSION_ARB                           =$2095;

   // ARB Extension #56 - GLX_ARB_create_context
   // see also GLX_ARB_create_context_profile (ARB #75)
   GLX_CONTEXT_MAJOR_VERSION_ARB                       = $2091;
   GLX_CONTEXT_MINOR_VERSION_ARB                       = $2092;
   GLX_CONTEXT_FLAGS_ARB                               = $2094;
   GLX_CONTEXT_DEBUG_BIT_ARB                           = $0001;
   GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB              = $0002;

   // ARB Extension #57 - GL_ARB_uniform_buffer_object
   GL_UNIFORM_BUFFER                                   = $8A11;
   GL_UNIFORM_BUFFER_BINDING                           = $8A28;
   GL_UNIFORM_BUFFER_START                             = $8A29;
   GL_UNIFORM_BUFFER_SIZE                              = $8A2A;
   GL_MAX_VERTEX_UNIFORM_BLOCKS                        = $8A2B;
   GL_MAX_GEOMETRY_UNIFORM_BLOCKS                      = $8A2C;
   GL_MAX_FRAGMENT_UNIFORM_BLOCKS                      = $8A2D;
   GL_MAX_COMBINED_UNIFORM_BLOCKS                      = $8A2E;
   GL_MAX_UNIFORM_BUFFER_BINDINGS                      = $8A2F;
   GL_MAX_UNIFORM_BLOCK_SIZE                           = $8A30;
   GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS           = $8A31;
   GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS         = $8A32;
   GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS         = $8A33;
   GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT                  = $8A34;
   GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH             = $8A35;
   GL_ACTIVE_UNIFORM_BLOCKS                            = $8A36;
   GL_UNIFORM_TYPE                                     = $8A37;
   GL_UNIFORM_SIZE                                     = $8A38;
   GL_UNIFORM_NAME_LENGTH                              = $8A39;
   GL_UNIFORM_BLOCK_INDEX                              = $8A3A;
   GL_UNIFORM_OFFSET                                   = $8A3B;
   GL_UNIFORM_ARRAY_STRIDE                             = $8A3C;
   GL_UNIFORM_MATRIX_STRIDE                            = $8A3D;
   GL_UNIFORM_IS_ROW_MAJOR                             = $8A3E;
   GL_UNIFORM_BLOCK_BINDING                            = $8A3F;
   GL_UNIFORM_BLOCK_DATA_SIZE                          = $8A40;
   GL_UNIFORM_BLOCK_NAME_LENGTH                        = $8A41;
   GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS                    = $8A42;
   GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES             = $8A43;
   GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER        = $8A44;
   GL_UNIFORM_BLOCK_REFERENCED_BY_GEOMETRY_SHADER      = $8A45;
   GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER      = $8A46;
   GL_INVALID_INDEX                                    = $FFFFFFFF;

   // ARB Extension #58 - GL_ARB_compatibility
   // (no new tokens)

   // ARB Extension #59 - GL_ARB_copy_buffer
   GL_COPY_READ_BUFFER                                 = $8F36;
   GL_COPY_WRITE_BUFFER                                = $8F37;

   // ARB Extension #60 - GL_ARB_shader_texture_lod
   // (no new tokens)

   // ARB Extension #61 - GL_ARB_depth_clamp
   GL_DEPTH_CLAMP                                      = $864F;

   // ARB Extension #62 - GL_ARB_draw_elements_base_vertex
   // (no new tokens)

   // ARB Extension #63 - GL_ARB_fragment_coord_conventions
   // (no new tokens)

   // ARB Extension #64 - GL_ARB_provoking_vertex
   GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION         = $8E4C;
   GL_FIRST_VERTEX_CONVENTION                          = $8E4D;
   GL_LAST_VERTEX_CONVENTION                           = $8E4E;
   GL_PROVOKING_VERTEX                                 = $8E4F;

   // ARB Extension #65 - GL_ARB_seamless_cube_map
   GL_TEXTURE_CUBE_MAP_SEAMLESS                        = $884F;

   // ARB Extension #66 - GL_ARB_sync
   GL_MAX_SERVER_WAIT_TIMEOUT                          = $9111;
   GL_OBJECT_TYPE                                      = $9112;
   GL_SYNC_CONDITION                                   = $9113;
   GL_SYNC_STATUS                                      = $9114;
   GL_SYNC_FLAGS                                       = $9115;
   GL_SYNC_FENCE                                       = $9116;
   GL_SYNC_GPU_COMMANDS_COMPLETE                       = $9117;
   GL_UNSIGNALED                                       = $9118;
   GL_SIGNALED                                         = $9119;
   GL_ALREADY_SIGNALED                                 = $911A;
   GL_TIMEOUT_EXPIRED                                  = $911B;
   GL_CONDITION_SATISFIED                              = $911C;
   GL_WAIT_FAILED                                      = $911D;
   GL_SYNC_FLUSH_COMMANDS_BIT                          = $00000001;
   GL_TIMEOUT_IGNORED                                  = $FFFFFFFFFFFFFFFF;

   // ARB Extension #67 - GL_ARB_texture_multisample
   GL_SAMPLE_POSITION                                  = $8E50;
   GL_SAMPLE_MASK                                      = $8E51;
   GL_SAMPLE_MASK_VALUE                                = $8E52;
   GL_MAX_SAMPLE_MASK_WORDS                            = $8E59;
   GL_TEXTURE_2D_MULTISAMPLE                           = $9100;
   GL_PROXY_TEXTURE_2D_MULTISAMPLE                     = $9101;
   GL_TEXTURE_2D_MULTISAMPLE_ARRAY                     = $9102;
   GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY               = $9103;
   GL_TEXTURE_BINDING_2D_MULTISAMPLE                   = $9104;
   GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY             = $9105;
   GL_TEXTURE_SAMPLES                                  = $9106;
   GL_TEXTURE_FIXED_SAMPLE_LOCATIONS                   = $9107;
   GL_SAMPLER_2D_MULTISAMPLE                           = $9108;
   GL_INT_SAMPLER_2D_MULTISAMPLE                       = $9109;
   GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE              = $910A;
   GL_SAMPLER_2D_MULTISAMPLE_ARRAY                     = $910B;
   GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY                 = $910C;
   GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY        = $910D;
   GL_MAX_COLOR_TEXTURE_SAMPLES                        = $910E;
   GL_MAX_DEPTH_TEXTURE_SAMPLES                        = $910F;
   GL_MAX_INTEGER_SAMPLES                              = $9110;

   // ARB Extension #68 - GL_ARB_vertex_array_bgra
   // (no new tokens)

   // ARB Extension #69 - GL_ARB_draw_buffers_blend
   // (no new tokens)

   // ARB Extension #70 - GL_ARB_sample_shading
   GL_SAMPLE_SHADING                                   = $8C36;
   GL_MIN_SAMPLE_SHADING_VALUE                         = $8C37;

   // ARB Extension #71 - GL_ARB_texture_cube_map_array
   GL_TEXTURE_CUBE_MAP_ARRAY                           = $9009;
   GL_TEXTURE_BINDING_CUBE_MAP_ARRAY                   = $900A;
   GL_PROXY_TEXTURE_CUBE_MAP_ARRAY                     = $900B;
   GL_SAMPLER_CUBE_MAP_ARRAY                           = $900C;
   GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW                    = $900D;
   GL_INT_SAMPLER_CUBE_MAP_ARRAY                       = $900E;
   GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY              = $900F;

   // ARB Extension #72 - GL_ARB_texture_gather
   GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET                = $8E5E;
   GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET                = $8E5F;
   GL_MAX_PROGRAM_TEXTURE_GATHER_COMPONENTS            = $8F9F;

   // ARB Extension #73 - GL_ARB_texture_query_lod
   // (no new tokens)

   // ARB Extension #74 - WGL_ARB_create_context_profile
   // see also WGL_ARB_create_context (ARB #55)
   WGL_CONTEXT_PROFILE_MASK_ARB                        =$9126;
   WGL_CONTEXT_CORE_PROFILE_BIT_ARB                    =$00000001;
   WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB           =$00000002;
   ERROR_INVALID_PROFILE_ARB                           =$2096;

   // ARB Extension #75 - GLX_ARB_create_context_profile
   // see also GLX_ARB_create_context (ARB #56)
   GLX_CONTEXT_PROFILE_MASK_ARB                        = $9126;
   GLX_CONTEXT_CORE_PROFILE_BIT_ARB                    = $00000001;
   GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB           = $00000002;

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'Vendor/EXT extensions constants, in extension number order'} {$ENDIF}

   // ----- extensions enumerants -----

   // EXT_texture_rectangle (can't find this extension in OpenGL registry)

   GL_TEXTURE_RECTANGLE_EXT                          = $84F5;
   GL_TEXTURE_BINDING_RECTANGLE_EXT                  = $84F6;
   GL_PROXY_TEXTURE_RECTANGLE_EXT                    = $84F7;
   GL_MAX_RECTANGLE_TEXTURE_SIZE_EXT                 = $84F8;

   // EXT_abgr (#1)
   GL_ABGR_EXT                                       = $8000;

   // EXT_blend_color (#2)
   GL_CONSTANT_COLOR_EXT                             = $8001;
   GL_ONE_MINUS_CONSTANT_COLOR_EXT                   = $8002;
   GL_CONSTANT_ALPHA_EXT                             = $8003;
   GL_ONE_MINUS_CONSTANT_ALPHA_EXT                   = $8004;
   GL_BLEND_COLOR_EXT                                = $8005;

   // EXT_polygon_offset (#3)
   GL_POLYGON_OFFSET_EXT                             = $8037;
   GL_POLYGON_OFFSET_FACTOR_EXT                      = $8038;
   GL_POLYGON_OFFSET_BIAS_EXT                        = $8039;

   // EXT_texture (#4)
   GL_ALPHA4_EXT                                     = $803B;
   GL_ALPHA8_EXT                                     = $803C;
   GL_ALPHA12_EXT                                    = $803D;
   GL_ALPHA16_EXT                                    = $803E;
   GL_LUMINANCE4_EXT                                 = $803F;
   GL_LUMINANCE8_EXT                                 = $8040;
   GL_LUMINANCE12_EXT                                = $8041;
   GL_LUMINANCE16_EXT                                = $8042;
   GL_LUMINANCE4_ALPHA4_EXT                          = $8043;
   GL_LUMINANCE6_ALPHA2_EXT                          = $8044;
   GL_LUMINANCE8_ALPHA8_EXT                          = $8045;
   GL_LUMINANCE12_ALPHA4_EXT                         = $8046;
   GL_LUMINANCE12_ALPHA12_EXT                        = $8047;
   GL_LUMINANCE16_ALPHA16_EXT                        = $8048;
   GL_INTENSITY_EXT                                  = $8049;
   GL_INTENSITY4_EXT                                 = $804A;
   GL_INTENSITY8_EXT                                 = $804B;
   GL_INTENSITY12_EXT                                = $804C;
   GL_INTENSITY16_EXT                                = $804D;
   GL_RGB2_EXT                                       = $804E;
   GL_RGB4_EXT                                       = $804F;
   GL_RGB5_EXT                                       = $8050;
   GL_RGB8_EXT                                       = $8051;
   GL_RGB10_EXT                                      = $8052;
   GL_RGB12_EXT                                      = $8053;
   GL_RGB16_EXT                                      = $8054;
   GL_RGBA2_EXT                                      = $8055;
   GL_RGBA4_EXT                                      = $8056;
   GL_RGB5_A1_EXT                                    = $8057;
   GL_RGBA8_EXT                                      = $8058;
   GL_RGB10_A2_EXT                                   = $8059;
   GL_RGBA12_EXT                                     = $805A;
   GL_RGBA16_EXT                                     = $805B;
   GL_TEXTURE_RED_SIZE_EXT                           = $805C;
   GL_TEXTURE_GREEN_SIZE_EXT                         = $805D;
   GL_TEXTURE_BLUE_SIZE_EXT                          = $805E;
   GL_TEXTURE_ALPHA_SIZE_EXT                         = $805F;
   GL_TEXTURE_LUMINANCE_SIZE_EXT                     = $8060;
   GL_TEXTURE_INTENSITY_SIZE_EXT                     = $8061;
   GL_REPLACE_EXT                                    = $8062;
   GL_PROXY_TEXTURE_1D_EXT                           = $8063;
   GL_PROXY_TEXTURE_2D_EXT                           = $8064;
   GL_TEXTURE_TOO_LARGE_EXT                          = $8065;

   // EXT_texture3D (#6)
   GL_PACK_SKIP_IMAGES_EXT                           = $806B;
   GL_PACK_IMAGE_HEIGHT_EXT                          = $806C;
   GL_UNPACK_SKIP_IMAGES_EXT                         = $806D;
   GL_UNPACK_IMAGE_HEIGHT_EXT                        = $806E;
   GL_TEXTURE_3D_EXT                                 = $806F;
   GL_PROXY_TEXTURE_3D_EXT                           = $8070;
   GL_TEXTURE_DEPTH_EXT                              = $8071;
   GL_TEXTURE_WRAP_R_EXT                             = $8072;
   GL_MAX_3D_TEXTURE_SIZE_EXT                        = $8073;

   // EXT_histogram (#11)
   GL_HISTOGRAM_EXT                                  = $8024;
   GL_PROXY_HISTOGRAM_EXT                            = $8025;
   GL_HISTOGRAM_WIDTH_EXT                            = $8026;
   GL_HISTOGRAM_FORMAT_EXT                           = $8027;
   GL_HISTOGRAM_RED_SIZE_EXT                         = $8028;
   GL_HISTOGRAM_GREEN_SIZE_EXT                       = $8029;
   GL_HISTOGRAM_BLUE_SIZE_EXT                        = $802A;
   GL_HISTOGRAM_ALPHA_SIZE_EXT                       = $802B;
   GL_HISTOGRAM_LUMINANCE_SIZE_EXT                   = $802C;
   GL_HISTOGRAM_SINK_EXT                             = $802D;
   GL_MINMAX_EXT                                     = $802E;
   GL_MINMAX_FORMAT_EXT                              = $802F;
   GL_MINMAX_SINK_EXT                                = $8030;

   // EXT_convolution (#12)
   GL_CONVOLUTION_1D_EXT                             = $8010;
   GL_CONVOLUTION_2D_EXT                             = $8011;
   GL_SEPARABLE_2D_EXT                               = $8012;
   GL_CONVOLUTION_BORDER_MODE_EXT                    = $8013;
   GL_CONVOLUTION_FILTER_SCALE_EXT                   = $8014;
   GL_CONVOLUTION_FILTER_BIAS_EXT                    = $8015;
   GL_REDUCE_EXT                                     = $8016;
   GL_CONVOLUTION_FORMAT_EXT                         = $8017;
   GL_CONVOLUTION_WIDTH_EXT                          = $8018;
   GL_CONVOLUTION_HEIGHT_EXT                         = $8019;
   GL_MAX_CONVOLUTION_WIDTH_EXT                      = $801A;
   GL_MAX_CONVOLUTION_HEIGHT_EXT                     = $801B;
   GL_POST_CONVOLUTION_RED_SCALE_EXT                 = $801C;
   GL_POST_CONVOLUTION_GREEN_SCALE_EXT               = $801D;
   GL_POST_CONVOLUTION_BLUE_SCALE_EXT                = $801E;
   GL_POST_CONVOLUTION_ALPHA_SCALE_EXT               = $801F;
   GL_POST_CONVOLUTION_RED_BIAS_EXT                  = $8020;
   GL_POST_CONVOLUTION_GREEN_BIAS_EXT                = $8021;
   GL_POST_CONVOLUTION_BLUE_BIAS_EXT                 = $8022;
   GL_POST_CONVOLUTION_ALPHA_BIAS_EXT                = $8023;

   // SGI_color_matrix (#13)
   GL_COLOR_MATRIX_SGI                               = $80B1;
   GL_COLOR_MATRIX_STACK_DEPTH_SGI                   = $80B2;
   GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI               = $80B3;
   GL_POST_COLOR_MATRIX_RED_SCALE_SGI                = $80B4;
   GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI              = $80B5;
   GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI               = $80B6;
   GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI              = $80B7;
   GL_POST_COLOR_MATRIX_RED_BIAS_SGI                 = $80B8;
   GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI               = $80B9;
   GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI                = $80BA;
   GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI               = $80BB;

   // EXT_texture_object (#20)
   GL_TEXTURE_PRIORITY_EXT                           = $8066;
   GL_TEXTURE_RESIDENT_EXT                           = $8067;
   GL_TEXTURE_1D_BINDING_EXT                         = $8068;
   GL_TEXTURE_2D_BINDING_EXT                         = $8069;
   GL_TEXTURE_3D_BINDING_EXT                         = $806A;

   // EXT_packed_pixels (#23)
   GL_UNSIGNED_BYTE_3_3_2_EXT                        = $8032;
   GL_UNSIGNED_SHORT_4_4_4_4_EXT                     = $8033;
   GL_UNSIGNED_SHORT_5_5_5_1_EXT                     = $8034;
   GL_UNSIGNED_INT_8_8_8_8_EXT                       = $8035;
   GL_UNSIGNED_INT_10_10_10_2_EXT                    = $8036;

   // GL_SGIS_texture_lod (#24)
   GL_TEXTURE_MIN_LOD_SGIS                          = $813A;
   GL_TEXTURE_MAX_LOD_SGIS                          = $813B;
   GL_TEXTURE_BASE_LEVEL_SGIS                       = $813C;
   GL_TEXTURE_MAX_LEVEL_SGIS                        = $813D;

   // GL_SGIS_multisample (#25)
   GL_MULTISAMPLE_SGIS                              = $809D;
   GL_SAMPLE_ALPHA_TO_MASK_SGIS                     = $809E;
   GL_SAMPLE_ALPHA_TO_ONE_SGIS                      = $809F;
   GL_SAMPLE_MASK_SGIS                              = $80A0;
   GL_1PASS_SGIS                                    = $80A1;
   GL_2PASS_0_SGIS                                  = $80A2;
   GL_2PASS_1_SGIS                                  = $80A3;
   GL_4PASS_0_SGIS                                  = $80A4;
   GL_4PASS_1_SGIS                                  = $80A5;
   GL_4PASS_2_SGIS                                  = $80A6;
   GL_4PASS_3_SGIS                                  = $80A7;
   GL_SAMPLE_BUFFERS_SGIS                           = $80A8;
   GL_SAMPLES_SGIS                                  = $80A9;
   GL_SAMPLE_MASK_VALUE_SGIS                        = $80AA;
   GL_SAMPLE_MASK_INVERT_SGIS                       = $80AB;
   GL_SAMPLE_PATTERN_SGIS                           = $80AC;

   // GL_EXT_rescale_normal (#27)
   GL_RESCALE_NORMAL_EXT                             = $803A;
   
   // GL_SGIS_generate_mipmap (#32)
   GL_GENERATE_MIPMAP_SGIS                          = $8191;
   GL_GENERATE_MIPMAP_HINT_SGIS                     = $8192;

   // GL_SGIX_shadow (#34)
   GL_TEXTURE_COMPARE_SGIX                          = $819A;
   GL_TEXTURE_COMPARE_OPERATOR_SGIX                 = $819B;
   GL_TEXTURE_LEQUAL_R_SGIX                         = $819C;
   GL_TEXTURE_GEQUAL_R_SGIX                         = $819D;

   // GL_SGIS_texture_edge_clamp (#35)
   GL_CLAMP_TO_EDGE_SGIS                            = $812F;

   // GL_SGIS_texture_border_clamp (#36)
   GL_CLAMP_TO_BORDER_SGIS                          = $812D;

   // EXT_blend_minmax (#37)
   GL_FUNC_ADD_EXT                                   = $8006;
   GL_MIN_EXT                                        = $8007;
   GL_MAX_EXT                                        = $8008;
   GL_BLEND_EQUATION_EXT                             = $8009;

   // EXT_blend_subtract (#38)
   GL_FUNC_SUBTRACT_EXT                              = $800A;
   GL_FUNC_REVERSE_SUBTRACT_EXT                      = $800B;

   // GL_EXT_object_space_tess (#75)
   GLU_OBJECT_PARAMETRIC_ERROR_EXT                   = 100208;
   GLU_OBJECT_PATH_LENGTH_EXT                        = 100209;

   // GL_EXT_paletted_texture (#78)
   GL_COLOR_INDEX1_EXT                               = $80E2;
   GL_COLOR_INDEX2_EXT                               = $80E3;
   GL_COLOR_INDEX4_EXT                               = $80E4;
   GL_COLOR_INDEX8_EXT                               = $80E5;
   GL_COLOR_INDEX12_EXT                              = $80E6;
   GL_COLOR_INDEX16_EXT                              = $80E7;

   // GL_EXT_paletted_texture (#78)
   GL_TEXTURE_INDEX_SIZE_EXT                        = $80ED;

   // GL_EXT_clip_volume_hint (#79)
   GL_CLIP_VOLUME_CLIPPING_HINT_EXT                 = $80F0;

   // GL_SGIX_shadow_ambient (#90)
   GL_SHADOW_AMBIENT_SGIX                           = $80BF;

   // EXT_compiled_vertex_array (#97)
   GL_ARRAY_ELEMENT_LOCK_FIRST_EXT                   = $81A8;
   GL_ARRAY_ELEMENT_LOCK_COUNT_EXT                   = $81A9;

   // EXT_nurbs_tessellator (#100)
   GLU_NURBS_MODE_EXT                                = 100160;
   GLU_NURBS_TESSELLATOR_EXT                         = 100161;
   GLU_NURBS_RENDERER_EXT                            = 100162;
   GLU_NURBS_BEGIN_EXT                               = 100164;
   GLU_NURBS_VERTEX_EXT                              = 100165;
   GLU_NURBS_NORMAL_EXT                              = 100166;
   GLU_NURBS_COLOR_EXT                               = 100167;
   GLU_NURBS_TEX_COORD_EXT                           = 100168;
   GLU_NURBS_END_EXT                                 = 100169;
   GLU_NURBS_BEGIN_DATA_EXT                          = 100170;
   GLU_NURBS_VERTEX_DATA_EXT                         = 100171;
   GLU_NURBS_NORMAL_DATA_EXT                         = 100172;
   GLU_NURBS_COLOR_DATA_EXT                          = 100173;
   GLU_NURBS_TEX_COORD_DATA_EXT                      = 100174;
   GLU_NURBS_END_DATA_EXT                            = 100175;

   // GL_IBM_rasterpos_clip (#110)
   GL_RASTER_POSITION_UNCLIPPED_IBM                 = $19262;

   // GL_EXT_draw_range_elements (#112)
   GL_MAX_ELEMENTS_VERTICES_EXT                     = $80E8;
   GL_MAX_ELEMENTS_INDICES_EXT                      = $80E9;

   // EXT_bgra (#129)
   GL_BGR_EXT                                        = $80E0;
   GL_BGRA_EXT                                       = $80E1;

   // GL_HP_occlusion_test (#137)
   GL_OCCLUSION_TEST_HP                             = $8165;
   GL_OCCLUSION_TEST_RESULT_HP                      = $8166;

   // GL_EXT_shared_texture_palette (#141)
   GL_SHARED_TEXTURE_PALETTE_EXT                     = $81FB;
   
   // GL_EXT_separate_specular_color (#144)
   GL_LIGHT_MODEL_COLOR_CONTROL_EXT                 = $81F8;
   GL_SINGLE_COLOR_EXT                              = $81F9;
   GL_SEPARATE_SPECULAR_COLOR_EXT                   = $81FA;

   // GL_EXT_secondary_color (#145)
   GL_COLOR_SUM_EXT                                 = $8458;
   GL_CURRENT_SECONDARY_COLOR_EXT                   = $8459;
   GL_SECONDARY_COLOR_ARRAY_SIZE_EXT                = $845A;
   GL_SECONDARY_COLOR_ARRAY_TYPE_EXT                = $845B;
   GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT              = $845C;
   GL_SECONDARY_COLOR_ARRAY_POINTER_EXT             = $845D;
   GL_SECONDARY_COLOR_ARRAY_EXT                     = $845E;

   // GL_EXT_fog_coord (#149)
   GL_FOG_COORDINATE_SOURCE_EXT                     = $8450;
   GL_FOG_COORDINATE_EXT                            = $8451;
   GL_FRAGMENT_DEPTH_EXT                            = $8452;
   GL_CURRENT_FOG_COORDINATE_EXT                    = $8453;
   GL_FOG_COORDINATE_ARRAY_TYPE_EXT                 = $8454;
   GL_FOG_COORDINATE_ARRAY_STRIDE_EXT               = $8455;
   GL_FOG_COORDINATE_ARRAY_POINTER_EXT              = $8456;
   GL_FOG_COORDINATE_ARRAY_EXT                      = $8457;

   // GL_EXT_texture_env_combine (#158)
   GL_COMBINE_EXT                                    = $8570;
   GL_COMBINE_RGB_EXT                                = $8571;
   GL_COMBINE_ALPHA_EXT                              = $8572;
   GL_RGB_SCALE_EXT                                  = $8573;
   GL_ADD_SIGNED_EXT                                 = $8574;
   GL_INTERPOLATE_EXT                                = $8575;
   GL_CONSTANT_EXT                                   = $8576;
   GL_PRIMARY_COLOR_EXT                              = $8577;
   GL_PREVIOUS_EXT                                   = $8578;
   GL_SOURCE0_RGB_EXT                                = $8580;
   GL_SOURCE1_RGB_EXT                                = $8581;
   GL_SOURCE2_RGB_EXT                                = $8582;
   GL_SOURCE0_ALPHA_EXT                              = $8588;
   GL_SOURCE1_ALPHA_EXT                              = $8589;
   GL_SOURCE2_ALPHA_EXT                              = $858A;
   GL_OPERAND0_RGB_EXT                               = $8590;
   GL_OPERAND1_RGB_EXT                               = $8591;
   GL_OPERAND2_RGB_EXT                               = $8592;
   GL_OPERAND0_ALPHA_EXT                             = $8598;
   GL_OPERAND1_ALPHA_EXT                             = $8599;
   GL_OPERAND2_ALPHA_EXT                             = $859A;

   // GL_EXT_texture_env_combine (#158)
   GL_SOURCE3_RGB_EXT                               = $8583;
   GL_SOURCE4_RGB_EXT                               = $8584;
   GL_SOURCE5_RGB_EXT                               = $8585;
   GL_SOURCE6_RGB_EXT                               = $8586;
   GL_SOURCE7_RGB_EXT                               = $8587;
   GL_SOURCE3_ALPHA_EXT                             = $858B;
   GL_SOURCE4_ALPHA_EXT                             = $858C;
   GL_SOURCE5_ALPHA_EXT                             = $858D;
   GL_SOURCE6_ALPHA_EXT                             = $858E;
   GL_SOURCE7_ALPHA_EXT                             = $858F;
   GL_OPERAND3_RGB_EXT                              = $8593;
   GL_OPERAND4_RGB_EXT                              = $8594;
   GL_OPERAND5_RGB_EXT                              = $8595;
   GL_OPERAND6_RGB_EXT                              = $8596;
   GL_OPERAND7_RGB_EXT                              = $8597;
   GL_OPERAND3_ALPHA_EXT                            = $859B;
   GL_OPERAND4_ALPHA_EXT                            = $859C;
   GL_OPERAND5_ALPHA_EXT                            = $859D;
   GL_OPERAND6_ALPHA_EXT                            = $859E;
   GL_OPERAND7_ALPHA_EXT                            = $859F;

   // GL_EXT_blend_func_separate (#173)
   GL_BLEND_DST_RGB_EXT                             = $80C8;
   GL_BLEND_SRC_RGB_EXT                             = $80C9;
   GL_BLEND_DST_ALPHA_EXT                           = $80CA;
   GL_BLEND_SRC_ALPHA_EXT                           = $80CB;

   // DanB : "GL_EXT_texture_cube_map (can't find this extension in OpenGL registry so removed)"
   // Mrqzzz : The following block was commented by DanB
   // But the constants are currently used in dws2openGL1x.pas, so i re-add them. If they
   // result harmful, we will remove them again.
   GL_NORMAL_MAP_EXT                                = $8511;
   GL_REFLECTION_MAP_EXT                            = $8512;
   GL_TEXTURE_CUBE_MAP_EXT                          = $8513;
   GL_TEXTURE_BINDING_CUBE_MAP_EXT                  = $8514;
   GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT               = $8515;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT               = $8516;
   GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT               = $8517;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT               = $8518;
   GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT               = $8519;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT               = $851A;
   GL_PROXY_TEXTURE_CUBE_MAP_EXT                    = $851B;
   GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT                 = $851C;



   // GL_EXT_stencil_wrap (#176)
   GL_INCR_WRAP_EXT                                  = $8507;
   GL_DECR_WRAP_EXT                                  = $8508;

   // GL_NV_texgen_reflection (#179)
   GL_NORMAL_MAP_NV                                  = $8511;
   GL_REFLECTION_MAP_NV                              = $8512;

   // GL_EXT_texture_lod_bias (#186)
   GL_MAX_TEXTURE_LOD_BIAS_EXT                      = $84FD;
   GL_TEXTURE_FILTER_CONTROL_EXT                    = $8500;
   GL_TEXTURE_LOD_BIAS_EXT                          = $8501;

   // GL_EXT_texture_filter_anisotropic (#187)
   GL_TEXTURE_MAX_ANISOTROPY_EXT                    = $84FE;
   GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT                = $84FF;

   // GL_NV_light_max_exponent (#189)
   GL_MAX_SHININESS_NV                              = $8504;
   GL_MAX_SPOT_EXPONENT_NV                          = $8505;

   // GL_NV_vertex_array_range (#190)
   GL_VERTEX_ARRAY_RANGE_NV                         = $851D;
   GL_VERTEX_ARRAY_RANGE_LENGTH_NV                  = $851E;
   GL_VERTEX_ARRAY_RANGE_VALID_NV                   = $851F;
   GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV             = $8520;
   GL_VERTEX_ARRAY_RANGE_POINTER_NV                 = $8521;

   // GL_NV_register_combiners (#191)
   GL_REGISTER_COMBINERS_NV                         = $8522;
   GL_VARIABLE_A_NV                                 = $8523;
   GL_VARIABLE_B_NV                                 = $8524;
   GL_VARIABLE_C_NV                                 = $8525;
   GL_VARIABLE_D_NV                                 = $8526;
   GL_VARIABLE_E_NV                                 = $8527;
   GL_VARIABLE_F_NV                                 = $8528;
   GL_VARIABLE_G_NV                                 = $8529;
   GL_CONSTANT_COLOR0_NV                            = $852A;
   GL_CONSTANT_COLOR1_NV                            = $852B;
   GL_PRIMARY_COLOR_NV                              = $852C;
   GL_SECONDARY_COLOR_NV                            = $852D;
   GL_SPARE0_NV                                     = $852E;
   GL_SPARE1_NV                                     = $852F;
   GL_DISCARD_NV                                    = $8530;
   GL_E_TIMES_F_NV                                  = $8531;
   GL_SPARE0_PLUS_SECONDARY_COLOR_NV                = $8532;
   GL_UNSIGNED_IDENTITY_NV                          = $8536;
   GL_UNSIGNED_INVERT_NV                            = $8537;
   GL_EXPAND_NORMAL_NV                              = $8538;
   GL_EXPAND_NEGATE_NV                              = $8539;
   GL_HALF_BIAS_NORMAL_NV                           = $853A;
   GL_HALF_BIAS_NEGATE_NV                           = $853B;
   GL_SIGNED_IDENTITY_NV                            = $853C;
   GL_SIGNED_NEGATE_NV                              = $853D;
   GL_SCALE_BY_TWO_NV                               = $853E;
   GL_SCALE_BY_FOUR_NV                              = $853F;
   GL_SCALE_BY_ONE_HALF_NV                          = $8540;
   GL_BIAS_BY_NEGATIVE_ONE_HALF_NV                  = $8541;
   GL_COMBINER_INPUT_NV                             = $8542;
   GL_COMBINER_MAPPING_NV                           = $8543;
   GL_COMBINER_COMPONENT_USAGE_NV                   = $8544;
   GL_COMBINER_AB_DOT_PRODUCT_NV                    = $8545;
   GL_COMBINER_CD_DOT_PRODUCT_NV                    = $8546;
   GL_COMBINER_MUX_SUM_NV                           = $8547;
   GL_COMBINER_SCALE_NV                             = $8548;
   GL_COMBINER_BIAS_NV                              = $8549;
   GL_COMBINER_AB_OUTPUT_NV                         = $854A;
   GL_COMBINER_CD_OUTPUT_NV                         = $854B;
   GL_COMBINER_SUM_OUTPUT_NV                        = $854C;
   GL_MAX_GENERAL_COMBINERS_NV                      = $854D;
   GL_NUM_GENERAL_COMBINERS_NV                      = $854E;
   GL_COLOR_SUM_CLAMP_NV                            = $854F;
   GL_COMBINER0_NV                                  = $8550;
   GL_COMBINER1_NV                                  = $8551;
   GL_COMBINER2_NV                                  = $8552;
   GL_COMBINER3_NV                                  = $8553;
   GL_COMBINER4_NV                                  = $8554;
   GL_COMBINER5_NV                                  = $8555;
   GL_COMBINER6_NV                                  = $8556;
   GL_COMBINER7_NV                                  = $8557;

   // GL_NV_fog_distance (#192)
   GL_FOG_DISTANCE_MODE_NV                          = $855A;
   GL_EYE_RADIAL_NV                                 = $855B;
   GL_EYE_PLANE_ABSOLUTE_NV                         = $855C;

   // GL_NV_texture_env_combine4 (#195)
   GL_COMBINE4_NV                                    = $8503;
   GL_SOURCE3_RGB_NV                                 = $8583;
   GL_SOURCE3_ALPHA_NV                               = $858B;
   GL_OPERAND3_RGB_NV                                = $8593;
   GL_OPERAND3_ALPHA_NV                              = $859B;

   // GL_EXT_texture_compression_s3tc (#198)
   GL_COMPRESSED_RGB_S3TC_DXT1_EXT                  = $83F0;
   GL_COMPRESSED_RGBA_S3TC_DXT1_EXT                 = $83F1;
   GL_COMPRESSED_RGBA_S3TC_DXT3_EXT                 = $83F2;
   GL_COMPRESSED_RGBA_S3TC_DXT5_EXT                 = $83F3;

   // GL_3DFX_texture_compression_FXT1 (#206)
   GL_COMPRESSED_RGB_FXT1_3DFX                      = $86B0;
   GL_COMPRESSED_RGBA_FXT1_3DFX                     = $86B1;

   // GL_3DFX_multisample (#207)
   GL_MULTISAMPLE_3DFX                              = $86B2;
   GL_SAMPLE_BUFFERS_3DFX                           = $86B3;
   GL_SAMPLES_3DFX                                  = $86B4;
   GL_MULTISAMPLE_BIT_3DFX                          = $20000000;

   // GL_EXT_multisample / WGL_EXT_multisample (#209)
   GL_MULTISAMPLE_EXT                               = $809D;
   GL_SAMPLE_ALPHA_TO_MASK_EXT                      = $809E;
   GL_SAMPLE_ALPHA_TO_ONE_EXT                       = $809F;
   GL_SAMPLE_MASK_EXT                               = $80A0;
   GL_1PASS_EXT                                     = $80A1;
   GL_2PASS_0_EXT                                   = $80A2;
   GL_2PASS_1_EXT                                   = $80A3;
   GL_4PASS_0_EXT                                   = $80A4;
   GL_4PASS_1_EXT                                   = $80A5;
   GL_4PASS_2_EXT                                   = $80A6;
   GL_4PASS_3_EXT                                   = $80A7;
   GL_SAMPLE_BUFFERS_EXT                            = $80A8;
   GL_SAMPLES_EXT                                   = $80A9;
   GL_SAMPLE_MASK_VALUE_EXT                         = $80AA;
   GL_SAMPLE_MASK_INVERT_EXT                        = $80AB;
   GL_SAMPLE_PATTERN_EXT                            = $80AC;
   WGL_SAMPLE_BUFFERS_EXT                           = $2041;
   WGL_SAMPLES_EXT                                  = $2042;

   // GL_SGIS_texture_color_mask (#214)
   GL_TEXTURE_COLOR_WRITEMASK_SGIS                  = $81EF;

   // GL_EXT_texture_env_dot3 (#220)
   GL_DOT3_RGB_EXT                                  = $8740;
   GL_DOT3_RGBA_EXT                                 = $8741;

   // GL_ATI_texture_mirror_once (#221)
   GL_MIRROR_CLAMP_ATI                              = $8742;
   GL_MIRROR_CLAMP_TO_EDGE_ATI                      = $8743;

   // GL_NV_fence (#222)
   GL_ALL_COMPLETED_NV                               = $84F2;
   GL_FENCE_STATUS_NV                                = $84F3;
   GL_FENCE_CONDITION_NV                             = $84F4;

   // GL_NV_texture_rectangle (#229)
   GL_TEXTURE_RECTANGLE_NV                           = $84F5;
   GL_TEXTURE_BINDING_RECTANGLE_NV                   = $84F6;
   GL_PROXY_TEXTURE_RECTANGLE_NV                     = $84F7;
   GL_MAX_RECTANGLE_TEXTURE_SIZE_NV                  = $84F8;

   // GL_NV_texture_shader (#230)
   GL_OFFSET_TEXTURE_RECTANGLE_NV                   = $864C;
   GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV             = $864D;
   GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV              = $864E;
   GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV          = $86D9;
   GL_UNSIGNED_INT_S8_S8_8_8_NV                     = $86DA;
   GL_UNSIGNED_INT_8_8_S8_S8_REV_NV                 = $86DB;
   GL_DSDT_MAG_INTENSITY_NV                         = $86DC;
   GL_SHADER_CONSISTENT_NV                          = $86DD;
   GL_TEXTURE_SHADER_NV                             = $86DE;
   GL_SHADER_OPERATION_NV                           = $86DF;
   GL_CULL_MODES_NV                                 = $86E0;
   GL_OFFSET_TEXTURE_MATRIX_NV                      = $86E1;
   GL_OFFSET_TEXTURE_SCALE_NV                       = $86E2;
   GL_OFFSET_TEXTURE_BIAS_NV                        = $86E3;
   GL_OFFSET_TEXTURE_2D_MATRIX_NV                   = GL_OFFSET_TEXTURE_MATRIX_NV;
   GL_OFFSET_TEXTURE_2D_SCALE_NV                    = GL_OFFSET_TEXTURE_SCALE_NV;
   GL_OFFSET_TEXTURE_2D_BIAS_NV                     = GL_OFFSET_TEXTURE_BIAS_NV;
   GL_PREVIOUS_TEXTURE_INPUT_NV                     = $86E4;
   GL_CONST_EYE_NV                                  = $86E5;
   GL_PASS_THROUGH_NV                               = $86E6;
   GL_CULL_FRAGMENT_NV                              = $86E7;
   GL_OFFSET_TEXTURE_2D_NV                          = $86E8;
   GL_DEPENDENT_AR_TEXTURE_2D_NV                    = $86E9;
   GL_DEPENDENT_GB_TEXTURE_2D_NV                    = $86EA;
   GL_DOT_PRODUCT_NV                                = $86EC;
   GL_DOT_PRODUCT_DEPTH_REPLACE_NV                  = $86ED;
   GL_DOT_PRODUCT_TEXTURE_2D_NV                     = $86EE;
   GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV               = $86F0;
   GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV               = $86F1;
   GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV               = $86F2;
   GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV     = $86F3;
   GL_HILO_NV                                       = $86F4;
   GL_DSDT_NV                                       = $86F5;
   GL_DSDT_MAG_NV                                   = $86F6;
   GL_DSDT_MAG_VIB_NV                               = $86F7;
   GL_HILO16_NV                                     = $86F8;
   GL_SIGNED_HILO_NV                                = $86F9;
   GL_SIGNED_HILO16_NV                              = $86FA;
   GL_SIGNED_RGBA_NV                                = $86FB;
   GL_SIGNED_RGBA8_NV                               = $86FC;
   GL_SIGNED_RGB_NV                                 = $86FE;
   GL_SIGNED_RGB8_NV                                = $86FF;
   GL_SIGNED_LUMINANCE_NV                           = $8701;
   GL_SIGNED_LUMINANCE8_NV                          = $8702;
   GL_SIGNED_LUMINANCE_ALPHA_NV                     = $8703;
   GL_SIGNED_LUMINANCE8_ALPHA8_NV                   = $8704;
   GL_SIGNED_ALPHA_NV                               = $8705;
   GL_SIGNED_ALPHA8_NV                              = $8706;
   GL_SIGNED_INTENSITY_NV                           = $8707;
   GL_SIGNED_INTENSITY8_NV                          = $8708;
   GL_DSDT8_NV                                      = $8709;
   GL_DSDT8_MAG8_NV                                 = $870A;
   GL_DSDT8_MAG8_INTENSITY8_NV                      = $870B;
   GL_SIGNED_RGB_UNSIGNED_ALPHA_NV                  = $870C;
   GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV                = $870D;
   GL_HI_SCALE_NV                                   = $870E;
   GL_LO_SCALE_NV                                   = $870F;
   GL_DS_SCALE_NV                                   = $8710;
   GL_DT_SCALE_NV                                   = $8711;
   GL_MAGNITUDE_SCALE_NV                            = $8712;
   GL_VIBRANCE_SCALE_NV                             = $8713;
   GL_HI_BIAS_NV                                    = $8714;
   GL_LO_BIAS_NV                                    = $8715;
   GL_DS_BIAS_NV                                    = $8716;
   GL_DT_BIAS_NV                                    = $8717;
   GL_MAGNITUDE_BIAS_NV                             = $8718;
   GL_VIBRANCE_BIAS_NV                              = $8719;
   GL_TEXTURE_BORDER_VALUES_NV                      = $871A;
   GL_TEXTURE_HI_SIZE_NV                            = $871B;
   GL_TEXTURE_LO_SIZE_NV                            = $871C;
   GL_TEXTURE_DS_SIZE_NV                            = $871D;
   GL_TEXTURE_DT_SIZE_NV                            = $871E;
   GL_TEXTURE_MAG_SIZE_NV                           = $871F;

   // GL_NV_texture_shader2 (#231)
   GL_DOT_PRODUCT_TEXTURE_3D_NV                     = $86EF;

   // GL_NV_vertex_array_range2 (#232)
   GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV           = $8533;

   // GL_NV_vertex_program (#233)
   GL_VERTEX_PROGRAM_NV                             = $8620;
   GL_VERTEX_STATE_PROGRAM_NV                       = $8621;
   GL_ATTRIB_ARRAY_SIZE_NV                          = $8623;
   GL_ATTRIB_ARRAY_STRIDE_NV                        = $8624;
   GL_ATTRIB_ARRAY_TYPE_NV                          = $8625;
   GL_CURRENT_ATTRIB_NV                             = $8626;
   GL_PROGRAM_LENGTH_NV                             = $8627;
   GL_PROGRAM_STRING_NV                             = $8628;
   GL_MODELVIEW_PROJECTION_NV                       = $8629;
   GL_IDENTITY_NV                                   = $862A;
   GL_INVERSE_NV                                    = $862B;
   GL_TRANSPOSE_NV                                  = $862C;
   GL_INVERSE_TRANSPOSE_NV                          = $862D;
   GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV               = $862E;
   GL_MAX_TRACK_MATRICES_NV                         = $862F;
   GL_MATRIX0_NV                                    = $8630;
   GL_MATRIX1_NV                                    = $8631;
   GL_MATRIX2_NV                                    = $8632;
   GL_MATRIX3_NV                                    = $8633;
   GL_MATRIX4_NV                                    = $8634;
   GL_MATRIX5_NV                                    = $8635;
   GL_MATRIX6_NV                                    = $8636;
   GL_MATRIX7_NV                                    = $8637;
   GL_CURRENT_MATRIX_STACK_DEPTH_NV                 = $8640;
   GL_CURRENT_MATRIX_NV                             = $8641;
   GL_VERTEX_PROGRAM_POINT_SIZE_NV                  = $8642;
   GL_VERTEX_PROGRAM_TWO_SIDE_NV                    = $8643;
   GL_PROGRAM_PARAMETER_NV                          = $8644;
   GL_ATTRIB_ARRAY_POINTER_NV                       = $8645;
   GL_PROGRAM_TARGET_NV                             = $8646;
   GL_PROGRAM_RESIDENT_NV                           = $8647;
   GL_TRACK_MATRIX_NV                               = $8648;
   GL_TRACK_MATRIX_TRANSFORM_NV                     = $8649;
   GL_VERTEX_PROGRAM_BINDING_NV                     = $864A;
   GL_PROGRAM_ERROR_POSITION_NV                     = $864B;
   GL_VERTEX_ATTRIB_ARRAY0_NV                       = $8650;
   GL_VERTEX_ATTRIB_ARRAY1_NV                       = $8651;
   GL_VERTEX_ATTRIB_ARRAY2_NV                       = $8652;
   GL_VERTEX_ATTRIB_ARRAY3_NV                       = $8653;
   GL_VERTEX_ATTRIB_ARRAY4_NV                       = $8654;
   GL_VERTEX_ATTRIB_ARRAY5_NV                       = $8655;
   GL_VERTEX_ATTRIB_ARRAY6_NV                       = $8656;
   GL_VERTEX_ATTRIB_ARRAY7_NV                       = $8657;
   GL_VERTEX_ATTRIB_ARRAY8_NV                       = $8658;
   GL_VERTEX_ATTRIB_ARRAY9_NV                       = $8659;
   GL_VERTEX_ATTRIB_ARRAY10_NV                      = $865A;
   GL_VERTEX_ATTRIB_ARRAY11_NV                      = $865B;
   GL_VERTEX_ATTRIB_ARRAY12_NV                      = $865C;
   GL_VERTEX_ATTRIB_ARRAY13_NV                      = $865D;
   GL_VERTEX_ATTRIB_ARRAY14_NV                      = $865E;
   GL_VERTEX_ATTRIB_ARRAY15_NV                      = $865F;
   GL_MAP1_VERTEX_ATTRIB0_4_NV                      = $8660;
   GL_MAP1_VERTEX_ATTRIB1_4_NV                      = $8661;
   GL_MAP1_VERTEX_ATTRIB2_4_NV                      = $8662;
   GL_MAP1_VERTEX_ATTRIB3_4_NV                      = $8663;
   GL_MAP1_VERTEX_ATTRIB4_4_NV                      = $8664;
   GL_MAP1_VERTEX_ATTRIB5_4_NV                      = $8665;
   GL_MAP1_VERTEX_ATTRIB6_4_NV                      = $8666;
   GL_MAP1_VERTEX_ATTRIB7_4_NV                      = $8667;
   GL_MAP1_VERTEX_ATTRIB8_4_NV                      = $8668;
   GL_MAP1_VERTEX_ATTRIB9_4_NV                      = $8669;
   GL_MAP1_VERTEX_ATTRIB10_4_NV                     = $866A;
   GL_MAP1_VERTEX_ATTRIB11_4_NV                     = $866B;
   GL_MAP1_VERTEX_ATTRIB12_4_NV                     = $866C;
   GL_MAP1_VERTEX_ATTRIB13_4_NV                     = $866D;
   GL_MAP1_VERTEX_ATTRIB14_4_NV                     = $866E;
   GL_MAP1_VERTEX_ATTRIB15_4_NV                     = $866F;
   GL_MAP2_VERTEX_ATTRIB0_4_NV                      = $8670;
   GL_MAP2_VERTEX_ATTRIB1_4_NV                      = $8671;
   GL_MAP2_VERTEX_ATTRIB2_4_NV                      = $8672;
   GL_MAP2_VERTEX_ATTRIB3_4_NV                      = $8673;
   GL_MAP2_VERTEX_ATTRIB4_4_NV                      = $8674;
   GL_MAP2_VERTEX_ATTRIB5_4_NV                      = $8675;
   GL_MAP2_VERTEX_ATTRIB6_4_NV                      = $8676;
   GL_MAP2_VERTEX_ATTRIB7_4_NV                      = $8677;
   GL_MAP2_VERTEX_ATTRIB8_4_NV                      = $8678;
   GL_MAP2_VERTEX_ATTRIB9_4_NV                      = $8679;
   GL_MAP2_VERTEX_ATTRIB10_4_NV                     = $867A;
   GL_MAP2_VERTEX_ATTRIB11_4_NV                     = $867B;
   GL_MAP2_VERTEX_ATTRIB12_4_NV                     = $867C;
   GL_MAP2_VERTEX_ATTRIB13_4_NV                     = $867D;
   GL_MAP2_VERTEX_ATTRIB14_4_NV                     = $867E;
   GL_MAP2_VERTEX_ATTRIB15_4_NV                     = $867F;

   // GL_NV_multisample_filter_hint (#259)
   GL_MULTISAMPLE_FILTER_HINT_NV                    = $8534;

   // GL_NV_occlusion_query (#261)
   GL_PIXEL_COUNTER_BITS_NV                          = $8864;
   GL_CURRENT_OCCLUSION_QUERY_ID_NV                  = $8865;
   GL_PIXEL_COUNT_NV                                 = $8866;
   GL_PIXEL_COUNT_AVAILABLE_NV                       = $8867;

   // GL_NV_point_sprite (#262)
   GL_POINT_SPRITE_NV                                = $8861;
   GL_COORD_REPLACE_NV                               = $8862;
   GL_POINT_SPRITE_R_MODE_NV                         = $8863;

   // GL_NV_texture_shader3 (#265)
   GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV               = $8850;
   GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV         = $8851;
   GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV        = $8852;
   GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV  = $8853;
   GL_OFFSET_HILO_TEXTURE_2D_NV                     = $8854;
   GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV              = $8855;
   GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV          = $8856;
   GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV   = $8857;
   GL_DEPENDENT_HILO_TEXTURE_2D_NV                  = $8858;
   GL_DEPENDENT_RGB_TEXTURE_3D_NV                   = $8859;
   GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV             = $885A;
   GL_DOT_PRODUCT_PASS_THROUGH_NV                   = $885B;
   GL_DOT_PRODUCT_TEXTURE_1D_NV                     = $885C;
   GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV           = $885D;
   GL_HILO8_NV                                      = $885E;
   GL_SIGNED_HILO8_NV                               = $885F;
   GL_FORCE_BLUE_TO_ONE_NV                          = $8860;

   // GL_EXT_stencil_two_side (#268)
   GL_STENCIL_TEST_TWO_SIDE_EXT                      = $8910;
   GL_ACTIVE_STENCIL_FACE_EXT                        = $8911;

   // GL_ATI_draw_buffers (#277)
   GL_MAX_DRAW_BUFFERS_ATI                          = $8824;
   GL_DRAW_BUFFER0_ATI                              = $8825;
   GL_DRAW_BUFFER1_ATI                              = $8826;
   GL_DRAW_BUFFER2_ATI                              = $8827;
   GL_DRAW_BUFFER3_ATI                              = $8828;
   GL_DRAW_BUFFER4_ATI                              = $8829;
   GL_DRAW_BUFFER5_ATI                              = $882A;
   GL_DRAW_BUFFER6_ATI                              = $882B;
   GL_DRAW_BUFFER7_ATI                              = $882C;
   GL_DRAW_BUFFER8_ATI                              = $882D;
   GL_DRAW_BUFFER9_ATI                              = $882E;
   GL_DRAW_BUFFER10_ATI                             = $882F;
   GL_DRAW_BUFFER11_ATI                             = $8830;
   GL_DRAW_BUFFER12_ATI                             = $8831;
   GL_DRAW_BUFFER13_ATI                             = $8832;
   GL_DRAW_BUFFER14_ATI                             = $8833;
   GL_DRAW_BUFFER15_ATI                             = $8834;

   // WGL_ATI_pixel_format_float (#278)
   WGL_TYPE_RGBA_FLOAT_ATI                          = $21A0;
   GL_TYPE_RGBA_FLOAT_ATI                           = $8820;
   GL_COLOR_CLEAR_UNCLAMPED_VALUE_ATI               = $8835;

   // GL_ATI_texture_float (#280)
   GL_RGBA_FLOAT32_ATI                              = $8814;
   GL_RGB_FLOAT32_ATI                               = $8815;
   GL_ALPHA_FLOAT32_ATI                             = $8816;
   GL_INTENSITY_FLOAT32_ATI                         = $8817;
   GL_LUMINANCE_FLOAT32_ATI                         = $8818;
   GL_LUMINANCE_ALPHA_FLOAT32_ATI                   = $8819;
   GL_RGBA_FLOAT16_ATI                              = $881A;
   GL_RGB_FLOAT16_ATI                               = $881B;
   GL_ALPHA_FLOAT16_ATI                             = $881C;
   GL_INTENSITY_FLOAT16_ATI                         = $881D;
   GL_LUMINANCE_FLOAT16_ATI                         = $881E;
   GL_LUMINANCE_ALPHA_FLOAT16_ATI                   = $881F;

   // GL_NV_float_buffer (#281)
   // WGL_NV_float_buffer
   // GLX_NV_float_buffer
   GL_FLOAT_R_NV                                    = $8880;
   GL_FLOAT_RG_NV                                   = $8881;
   GL_FLOAT_RGB_NV                                  = $8882;
   GL_FLOAT_RGBA_NV                                 = $8883;
   GL_FLOAT_R16_NV                                  = $8884;
   GL_FLOAT_R32_NV                                  = $8885;
   GL_FLOAT_RG16_NV                                 = $8886;
   GL_FLOAT_RG32_NV                                 = $8887;
   GL_FLOAT_RGB16_NV                                = $8888;
   GL_FLOAT_RGB32_NV                                = $8889;
   GL_FLOAT_RGBA16_NV                               = $888A;
   GL_FLOAT_RGBA32_NV                               = $888B;
   GL_TEXTURE_FLOAT_COMPONENTS_NV                   = $888C;
   GL_FLOAT_CLEAR_COLOR_VALUE_NV                    = $888D;
   GL_FLOAT_RGBA_MODE_NV                            = $888E;
   WGL_FLOAT_COMPONENTS_NV                          = $20B0;
   WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_R_NV         = $20B1;
   WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RG_NV        = $20B2;
   WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGB_NV       = $20B3;
   WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGBA_NV      = $20B4;
   WGL_TEXTURE_FLOAT_R_NV                           = $20B5;
   WGL_TEXTURE_FLOAT_RG_NV                          = $20B6;
   WGL_TEXTURE_FLOAT_RGB_NV                         = $20B7;
   WGL_TEXTURE_FLOAT_RGBA_NV                        = $20B8;
   GLX_FLOAT_COMPONENTS_NV                          = $20B0;

   // GL_NV_primitive_restart (#285)
   GL_PRIMITIVE_RESTART_NV                           = $8558;
   GL_PRIMITIVE_RESTART_INDEX_NV                     = $8559;

   // GL_EXT_depth_bounds_test (#297)
   GL_DEPTH_BOUNDS_TEST_EXT                         = $8890;
   GL_DEPTH_BOUNDS_EXT                              = $8891;

   // GL_EXT_texture_mirror_clamp (#298)
   GL_MIRROR_CLAMP_EXT                              = $8742;
   GL_MIRROR_CLAMP_TO_EDGE_EXT                      = $8743;
   GL_MIRROR_CLAMP_TO_BORDER_EXT                    = $8912;

   // GL_EXT_blend_equation_separate (EXT #299)
   GL_BLEND_EQUATION_RGB_EXT                        = $8009;
   GL_BLEND_EQUATION_ALPHA_EXT                      = $883D;

   // GL_EXT_pixel_buffer_object (EXT #302)
   GL_PIXEL_PACK_BUFFER_EXT                         = $88EB;
   GL_PIXEL_UNPACK_BUFFER_EXT                       = $88EC;
   GL_PIXEL_PACK_BUFFER_BINDING_EXT                 = $88ED;
   GL_PIXEL_UNPACK_BUFFER_BINDING_EXT               = $88EF;

   // GL_EXT_framebuffer_object (#310)
   GL_FRAMEBUFFER_EXT                               = $8D40;
   GL_RENDERBUFFER_EXT                              = $8D41;
   GL_STENCIL_INDEX1_EXT                            = $8D46;
   GL_STENCIL_INDEX4_EXT                            = $8D47;
   GL_STENCIL_INDEX8_EXT                            = $8D48;
   GL_STENCIL_INDEX16_EXT                           = $8D49;
   GL_DEPTH24_STENCIL8_EXT                          = $88F0;
   GL_RENDERBUFFER_WIDTH_EXT                        = $8D42;
   GL_RENDERBUFFER_HEIGHT_EXT                       = $8D43;
   GL_RENDERBUFFER_INTERNAL_FORMAT_EXT              = $8D44;
   GL_RENDERBUFFER_RED_SIZE_EXT                     = $8D50;
   GL_RENDERBUFFER_GREEN_SIZE_EXT                   = $8D51;
   GL_RENDERBUFFER_BLUE_SIZE_EXT                    = $8D52;
   GL_RENDERBUFFER_ALPHA_SIZE_EXT                   = $8D53;
   GL_RENDERBUFFER_DEPTH_SIZE_EXT                   = $8D54;
   GL_RENDERBUFFER_STENCIL_SIZE_EXT                 = $8D55;
   GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT        = $8CD0;
   GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT        = $8CD1;
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT      = $8CD2;
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT = $8CD3;
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT = $8CD4;
   GL_COLOR_ATTACHMENT0_EXT                         = $8CE0;
   GL_COLOR_ATTACHMENT1_EXT                         = $8CE1;
   GL_COLOR_ATTACHMENT2_EXT                         = $8CE2;
   GL_COLOR_ATTACHMENT3_EXT                         = $8CE3;
   GL_COLOR_ATTACHMENT4_EXT                         = $8CE4;
   GL_COLOR_ATTACHMENT5_EXT                         = $8CE5;
   GL_COLOR_ATTACHMENT6_EXT                         = $8CE6;
   GL_COLOR_ATTACHMENT7_EXT                         = $8CE7;
   GL_COLOR_ATTACHMENT8_EXT                         = $8CE8;
   GL_COLOR_ATTACHMENT9_EXT                         = $8CE9;
   GL_COLOR_ATTACHMENT10_EXT                        = $8CEA;
   GL_COLOR_ATTACHMENT11_EXT                        = $8CEB;
   GL_COLOR_ATTACHMENT12_EXT                        = $8CEC;
   GL_COLOR_ATTACHMENT13_EXT                        = $8CED;
   GL_COLOR_ATTACHMENT14_EXT                        = $8CEE;
   GL_COLOR_ATTACHMENT15_EXT                        = $8CEF;
   GL_DEPTH_ATTACHMENT_EXT                          = $8D00;
   GL_STENCIL_ATTACHMENT_EXT                        = $8D20;
   GL_FRAMEBUFFER_COMPLETE_EXT                      = $8CD5;
   GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT         = $8CD6;
   GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT = $8CD7;
   GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT = $8CD8;
   GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT         = $8CD9;
   GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT            = $8CDA;
   GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT        = $8CDB;
   GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT        = $8CDC;
   GL_FRAMEBUFFER_UNSUPPORTED_EXT                   = $8CDD;
   GL_FRAMEBUFFER_BINDING_EXT                       = $8CA6;
   GL_RENDERBUFFER_BINDING_EXT                      = $8CA7;
   GL_MAX_COLOR_ATTACHMENTS_EXT                     = $8CDF;
   GL_MAX_RENDERBUFFER_SIZE_EXT                     = $84E8;
   GL_INVALID_FRAMEBUFFER_OPERATION_EXT             = $0506;

   // GL_EXT_packed_depth_stencil (#312)
   GL_DEPTH_STENCIL_EXT                             = $84F9;
   GL_UNSIGNED_INT_24_8_EXT                         = $84FA;
   //GL_DEPTH24_STENCIL8_EXT                          = $88F0;
   GL_TEXTURE_STENCIL_SIZE_EXT                      = $88F1;

   // GL_EXT_stencil_clear_tag (#314)
   GL_STENCIL_TAG_BITS_EXT                          = $88F2;
   GL_STENCIL_CLEAR_TAG_VALUE_EXT                   = $88F3;

   // GL_EXT_texture_sRGB (#315)
   GL_SRGB_EXT                                          = $8C40;
   GL_SRGB8_EXT                                         = $8C41;
   GL_SRGB_ALPHA_EXT                                    = $8C42;
   GL_SRGB8_ALPHA8_EXT                                  = $8C43;
   GL_SLUMINANCE_ALPHA_EXT                              = $8C44;
   GL_SLUMINANCE8_ALPHA8_EXT                            = $8C45;
   GL_SLUMINANCE_EXT                                    = $8C46;
   GL_SLUMINANCE8_EXT                                   = $8C47;
   GL_COMPRESSED_SRGB_EXT                               = $8C48;
   GL_COMPRESSED_SRGB_ALPHA_EXT                         = $8C49;
   GL_COMPRESSED_SLUMINANCE_EXT                         = $8C4A;
   GL_COMPRESSED_SLUMINANCE_ALPHA_EXT                   = $8C4B;
   GL_COMPRESSED_SRGB_S3TC_DXT1_EXT                     = $8C4C;
   GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT               = $8C4D;
   GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT               = $8C4E;
   GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT               = $8C4F;

   // GL_EXT_framebuffer_blit (#316)
   GL_READ_FRAMEBUFFER_EXT                              = $8CA8;
   GL_DRAW_FRAMEBUFFER_EXT                              = $8CA9;
   GL_DRAW_FRAMEBUFFER_BINDING_EXT                      = $8CA6; // alias FRAMEBUFFER_BINDING_EXT
   GL_READ_FRAMEBUFFER_BINDING_EXT                      = $8CAA;

   // GL_EXT_framebuffer_multisample (#317)
   GL_RENDERBUFFER_SAMPLES_EXT                          = $8CAB;
   GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_EXT            = $8D56;
   GL_MAX_SAMPLES_EXT                                   = $8D57;

   // GL_EXT_timer_query (#319)
   GL_TIME_ELAPSED_EXT                                  = $88BF;

   // GL_EXT_gpu_program_parameters (#320)
   // (no new tokens)

   // GL_NV_geometry_program4 (#323) - this seems to be supported on NO hardware
   GL_GEOMETRY_PROGRAM_NV                              = $8C26;
   GL_MAX_PROGRAM_OUTPUT_VERTICES_NV                   = $8C27;
   GL_MAX_PROGRAM_TOTAL_OUTPUT_COMPONENTS_NV           = $8C28;

   // GL_EXT_geometry_shader4 (#324)
   GL_GEOMETRY_SHADER_EXT                           = $8DD9;
   GL_GEOMETRY_VERTICES_OUT_EXT                     = $8DDA;
   GL_GEOMETRY_INPUT_TYPE_EXT                       = $8DDB;
   GL_GEOMETRY_OUTPUT_TYPE_EXT                      = $8DDC;
   GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_EXT          = $8C29;
   GL_MAX_GEOMETRY_VARYING_COMPONENTS_EXT           = $8DDD;
   GL_MAX_VERTEX_VARYING_COMPONENTS_EXT             = $8DDE;
   GL_MAX_VARYING_COMPONENTS_EXT                    = $8B4B;
   GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_EXT           = $8DDF;
   GL_MAX_GEOMETRY_OUTPUT_VERTICES_EXT              = $8DE0;
   GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_EXT      = $8DE1;
   GL_LINES_ADJACENCY_EXT                           = $A;
   GL_LINE_STRIP_ADJACENCY_EXT                      = $B;
   GL_TRIANGLES_ADJACENCY_EXT                       = $C;
   GL_TRIANGLE_STRIP_ADJACENCY_EXT                  = $D;
   GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_EXT      = $8DA8;
   GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_EXT        = $8DA9;
   GL_FRAMEBUFFER_ATTACHMENT_LAYERED_EXT            = $8DA7;
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT      = $8CD4;
   GL_PROGRAM_POINT_SIZE_EXT                        = $8642;

   // GL_EXT_gpu_shader4 (#326)
   GL_VERTEX_ATTRIB_ARRAY_INTEGER_EXT                  = $88FD;
   GL_SAMPLER_1D_ARRAY_EXT                             = $8DC0;
   GL_SAMPLER_2D_ARRAY_EXT                             = $8DC1;
   GL_SAMPLER_BUFFER_EXT                               = $8DC2;
   GL_SAMPLER_1D_ARRAY_SHADOW_EXT                      = $8DC3;
   GL_SAMPLER_2D_ARRAY_SHADOW_EXT                      = $8DC4;
   GL_SAMPLER_CUBE_SHADOW_EXT                          = $8DC5;
   //GL_UNSIGNED_INT                                     = $1405;
   GL_UNSIGNED_INT_VEC2_EXT                            = $8DC6;
   GL_UNSIGNED_INT_VEC3_EXT                            = $8DC7;
   GL_UNSIGNED_INT_VEC4_EXT                            = $8DC8;
   GL_INT_SAMPLER_1D_EXT                               = $8DC9;
   GL_INT_SAMPLER_2D_EXT                               = $8DCA;
   GL_INT_SAMPLER_3D_EXT                               = $8DCB;
   GL_INT_SAMPLER_CUBE_EXT                             = $8DCC;
   GL_INT_SAMPLER_2D_RECT_EXT                          = $8DCD;
   GL_INT_SAMPLER_1D_ARRAY_EXT                         = $8DCE;
   GL_INT_SAMPLER_2D_ARRAY_EXT                         = $8DCF;
   GL_INT_SAMPLER_BUFFER_EXT                           = $8DD0;
   GL_UNSIGNED_INT_SAMPLER_1D_EXT                      = $8DD1;
   GL_UNSIGNED_INT_SAMPLER_2D_EXT                      = $8DD2;
   GL_UNSIGNED_INT_SAMPLER_3D_EXT                      = $8DD3;
   GL_UNSIGNED_INT_SAMPLER_CUBE_EXT                    = $8DD4;
   GL_UNSIGNED_INT_SAMPLER_2D_RECT_EXT                 = $8DD5;
   GL_UNSIGNED_INT_SAMPLER_1D_ARRAY_EXT                = $8DD6;
   GL_UNSIGNED_INT_SAMPLER_2D_ARRAY_EXT                = $8DD7;
   GL_UNSIGNED_INT_SAMPLER_BUFFER_EXT                  = $8DD8;
   GL_MIN_PROGRAM_TEXEL_OFFSET_EXT                     = $8904;
   GL_MAX_PROGRAM_TEXEL_OFFSET_EXT                     = $8905;


   // GL_EXT_packed_float (#328)
   // WGL_EXT_pixel_format_packed_float
   // GLX_EXT_fbconfig_packed_float
   GL_R11F_G11F_B10F_EXT                            = $8C3A;
   GL_UNSIGNED_INT_10F_11F_11F_REV_EXT              = $8C3B;
   GL_RGBA_SIGNED_COMPONENTS_EXT                    = $8C3C;
   WGL_TYPE_RGBA_UNSIGNED_FLOAT_EXT                 = $20A8;
   GLX_RGBA_UNSIGNED_FLOAT_TYPE_EXT                 = $20B1;
   GLX_RGBA_UNSIGNED_FLOAT_BIT_EXT                  = $00000008;

   // GL_EXT_texture_array (#329)
   GL_TEXTURE_1D_ARRAY_EXT                          = $8C18;
   GL_TEXTURE_2D_ARRAY_EXT                          = $8C1A;
   GL_PROXY_TEXTURE_2D_ARRAY_EXT                    = $8C1B;
   GL_PROXY_TEXTURE_1D_ARRAY_EXT                    = $8C19;
   GL_TEXTURE_BINDING_1D_ARRAY_EXT                  = $8C1C;
   GL_TEXTURE_BINDING_2D_ARRAY_EXT                  = $8C1D;
   GL_MAX_ARRAY_TEXTURE_LAYERS_EXT                  = $88FF;
   GL_COMPARE_REF_DEPTH_TO_TEXTURE_EXT              = $884E;
   //GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT      = $8CD4;
   //GL_SAMPLER_1D_ARRAY_EXT                          = $8DC0;
   //GL_SAMPLER_2D_ARRAY_EXT                          = $8DC1;
   //GL_SAMPLER_1D_ARRAY_SHADOW_EXT                   = $8DC3;
   //GL_SAMPLER_2D_ARRAY_SHADOW_EXT                   = $8DC4;

   // GL_EXT_texture_buffer_object (#330)
   GL_TEXTURE_BUFFER_EXT                            = $8C2A;
   GL_MAX_TEXTURE_BUFFER_SIZE_EXT                   = $8C2B;
   GL_TEXTURE_BINDING_BUFFER_EXT                    = $8C2C;
   GL_TEXTURE_BUFFER_DATA_STORE_BINDING_EXT         = $8C2D;
   GL_TEXTURE_BUFFER_FORMAT_EXT                     = $8C2E;

   // GL_EXT_texture_compression_latc (#331)
   GL_COMPRESSED_LUMINANCE_LATC1_EXT                = $8C70;
   GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT         = $8C71;
   GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT          = $8C72;
   GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT   = $8C73;

   // // GL_ATI_texture_compression_3dc
   GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI            = $8837;

   // GL_EXT_texture_compression_rgtc (#332)
   GL_COMPRESSED_RED_RGTC1_EXT                      = $8DBB;
   GL_COMPRESSED_SIGNED_RED_RGTC1_EXT               = $8DBC;
   GL_COMPRESSED_RED_GREEN_RGTC2_EXT                = $8DBD;
   GL_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT         = $8DBE;

   // GL_EXT_texture_shared_exponent (#333)
   GL_RGB9_E5_EXT                                   = $8C3D;
   GL_UNSIGNED_INT_5_9_9_9_REV_EXT                  = $8C3E;
   GL_TEXTURE_SHARED_SIZE_EXT                       = $8C3F;



   // GL_EXT_framebuffer_sRGB (#337)
   // GLX_EXT_framebuffer_sRGB
   // WGL_EXT_framebuffer_sRGB
   GLX_FRAMEBUFFER_SRGB_CAPABLE_EXT                 = $20B2;
   WGL_FRAMEBUFFER_SRGB_CAPABLE_EXT                 = $20A9;
   GL_FRAMEBUFFER_SRGB_EXT                          = $8DB9;
   GL_FRAMEBUFFER_SRGB_CAPABLE_EXT                  = $8DBA;

   // GL_NV_transform_feedback (#341)
   GL_TRANSFORM_FEEDBACK_BUFFER_NV                      =$8C8E;
   GL_TRANSFORM_FEEDBACK_BUFFER_START_NV                =$8C84;
   GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_NV                 =$8C85;
   GL_TRANSFORM_FEEDBACK_RECORD_NV                      =$8C86;
   GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_NV              =$8C8F;
   GL_INTERLEAVED_ATTRIBS_NV                            =$8C8C;
   GL_SEPARATE_ATTRIBS_NV                               =$8C8D;
   GL_PRIMITIVES_GENERATED_NV                           =$8C87;
   GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_NV          =$8C88;
   GL_RASTERIZER_DISCARD_NV                             =$8C89;
   GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_NV  =$8C8A;
   GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_NV        =$8C8B;
   GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_NV     =$8C80;
   GL_TRANSFORM_FEEDBACK_ATTRIBS_NV                     =$8C7E;
   GL_ACTIVE_VARYINGS_NV                                =$8C81;
   GL_ACTIVE_VARYING_MAX_LENGTH_NV                      =$8C82;
   GL_TRANSFORM_FEEDBACK_VARYINGS_NV                    =$8C83;
   GL_TRANSFORM_FEEDBACK_BUFFER_MODE_NV                 =$8C7F;
   GL_BACK_PRIMARY_COLOR_NV                             =$8C77;
   GL_BACK_SECONDARY_COLOR_NV                           =$8C78;
   GL_TEXTURE_COORD_NV                                  =$8C79;
   GL_CLIP_DISTANCE_NV                                  =$8C7A;
   GL_VERTEX_ID_NV                                      =$8C7B;
   GL_PRIMITIVE_ID_NV                                   =$8C7C;
   GL_GENERIC_ATTRIB_NV                                 =$8C7D;
   //GL_POINT_SIZE                                        =$0B11;
   //GL_FOG_COORDINATE                                    =$8451;
   //GL_SECONDARY_COLOR_NV                                =$852D;
   //GL_PRIMARY_COLOR                                     =$8577;
   //GL_POSITION                                          =$1203;
   GL_LAYER_NV                                          =$8DAA;
   //GL_UNSIGNED_INT_VEC2_EXT                             =$8DC6;
   //GL_UNSIGNED_INT_VEC3_EXT                             =$8DC7;
   //GL_UNSIGNED_INT_VEC4_EXT                             =$8DC8;


   // GL_EXT_bindable_uniform (#342)
   GL_MAX_VERTEX_BINDABLE_UNIFORMS_EXT              = $8DE2;
   GL_MAX_FRAGMENT_BINDABLE_UNIFORMS_EXT            = $8DE3;
   GL_MAX_GEOMETRY_BINDABLE_UNIFORMS_EXT            = $8DE4;
   GL_MAX_BINDABLE_UNIFORM_SIZE_EXT                 = $8DED;
   GL_UNIFORM_BUFFER_BINDING_EXT                    = $8DEF;
   GL_UNIFORM_BUFFER_EXT                            = $8DEE;

   // GL_EXT_texture_integer (#343)
   GL_RGBA_INTEGER_MODE_EXT                         = $8D9E;
   GL_RGBA32UI_EXT                                  = $8D70;
   GL_RGB32UI_EXT                                   = $8D71;
   GL_ALPHA32UI_EXT                                 = $8D72;
   GL_INTENSITY32UI_EXT                             = $8D73;
   GL_LUMINANCE32UI_EXT                             = $8D74;
   GL_LUMINANCE_ALPHA32UI_EXT                       = $8D75;

   GL_RGBA16UI_EXT                                  = $8D76;
   GL_RGB16UI_EXT                                   = $8D77;
   GL_ALPHA16UI_EXT                                 = $8D78;
   GL_INTENSITY16UI_EXT                             = $8D79;
   GL_LUMINANCE16UI_EXT                             = $8D7A;
   GL_LUMINANCE_ALPHA16UI_EXT                       = $8D7B;

   GL_RGBA8UI_EXT                                   = $8D7C;
   GL_RGB8UI_EXT                                    = $8D7D;
   GL_ALPHA8UI_EXT                                  = $8D7E;
   GL_INTENSITY8UI_EXT                              = $8D7F;
   GL_LUMINANCE8UI_EXT                              = $8D80;
   GL_LUMINANCE_ALPHA8UI_EXT                        = $8D81;

   GL_RGBA32I_EXT                                   = $8D82;
   GL_RGB32I_EXT                                    = $8D83;
   GL_ALPHA32I_EXT                                  = $8D84;
   GL_INTENSITY32I_EXT                              = $8D85;
   GL_LUMINANCE32I_EXT                              = $8D86;
   GL_LUMINANCE_ALPHA32I_EXT                        = $8D87;

   GL_RGBA16I_EXT                                   = $8D88;
   GL_RGB16I_EXT                                    = $8D89;
   GL_ALPHA16I_EXT                                  = $8D8A;
   GL_INTENSITY16I_EXT                              = $8D8B;
   GL_LUMINANCE16I_EXT                              = $8D8C;
   GL_LUMINANCE_ALPHA16I_EXT                        = $8D8D;

   GL_RGBA8I_EXT                                    = $8D8E;
   GL_RGB8I_EXT                                     = $8D8F;
   GL_ALPHA8I_EXT                                   = $8D90;
   GL_INTENSITY8I_EXT                               = $8D91;
   GL_LUMINANCE8I_EXT                               = $8D92;
   GL_LUMINANCE_ALPHA8I_EXT                         = $8D93;

   GL_RED_INTEGER_EXT                               = $8D94;
   GL_GREEN_INTEGER_EXT                             = $8D95;
   GL_BLUE_INTEGER_EXT                              = $8D96;
   GL_ALPHA_INTEGER_EXT                             = $8D97;
   GL_RGB_INTEGER_EXT                               = $8D98;
   GL_RGBA_INTEGER_EXT                              = $8D99;
   GL_BGR_INTEGER_EXT                               = $8D9A;
   GL_BGRA_INTEGER_EXT                              = $8D9B;
   GL_LUMINANCE_INTEGER_EXT                         = $8D9C;
   GL_LUMINANCE_ALPHA_INTEGER_EXT                   = $8D9D;

   // GL_NV_conditional_render (#346)
   GL_QUERY_WAIT_NV                                    = $8E13;
   GL_QUERY_NO_WAIT_NV                                 = $8E14;
   GL_QUERY_BY_REGION_WAIT_NV                          = $8E15;
   GL_QUERY_BY_REGION_NO_WAIT_NV                       = $8E16;

   // GL_EXT_transform_feedback (#352)
   GL_TRANSFORM_FEEDBACK_BUFFER_EXT                    = $8C8E;
   GL_TRANSFORM_FEEDBACK_BUFFER_START_EXT              = $8C84;
   GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_EXT               = $8C85;
   GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_EXT            = $8C8F;
   GL_INTERLEAVED_ATTRIBS_EXT                          = $8C8C;
   GL_SEPARATE_ATTRIBS_EXT                             = $8C8D;
   GL_PRIMITIVES_GENERATED_EXT                         = $8C87;
   GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_EXT        = $8C88;
   GL_RASTERIZER_DISCARD_EXT                           = $8C89;
   GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_EXT= $8C8A;
   GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_EXT      = $8C8B;
   GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_EXT   = $8C80;
   GL_TRANSFORM_FEEDBACK_VARYINGS_EXT                  = $8C83;
   GL_TRANSFORM_FEEDBACK_BUFFER_MODE_EXT               = $8C7F;
   GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH_EXT        = $8C76;

   // GL_AMD_vertex_shader_tessellator (#363)
   GL_SAMPLER_BUFFER_AMD                               = $9001;
   GL_INT_SAMPLER_BUFFER_AMD                           = $9002;
   GL_UNSIGNED_INT_SAMPLER_BUFFER_AMD                  = $9003;
   GL_DISCRETE_AMD                                     = $9006;
   GL_CONTINUOUS_AMD                                   = $9007;
   GL_TESSELLATION_MODE_AMD                            = $9004;
   GL_TESSELLATION_FACTOR_AMD                          = $9005;

   // unknown extension, where does it come from?
   WGL_COLOR_SAMPLES_NV                              = $20B9;

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL Utility (GLU) generic constants'} {$ENDIF}
   // ********** GLU generic constants **********

   // Errors: (return value 0= no error)
   GLU_INVALID_ENUM                                 = 100900;
   GLU_INVALID_VALUE                                = 100901;
   GLU_OUT_OF_MEMORY                                = 100902;
   GLU_INCOMPATIBLE_GL_VERSION                      = 100903;

   // StringName
   GLU_VERSION                                      = 100800;
   GLU_EXTENSIONS                                   = 100801;

   // Boolean
   GLU_TRUE                                         = GL_TRUE;
   GLU_FALSE                                        = GL_FALSE;

   // Quadric constants
   // QuadricNormal
   GLU_SMOOTH                                       = 100000;
   GLU_FLAT                                         = 100001;
   GLU_NONE                                         = 100002;

   // QuadricDrawStyle
   GLU_POINT                                        = 100010;
   GLU_LINE                                         = 100011;
   GLU_FILL                                         = 100012;
   GLU_SILHOUETTE                                   = 100013;

   // QuadricOrientation
   GLU_OUTSIDE                                      = 100020;
   GLU_INSIDE                                       = 100021;

   // Tesselation constants
   GLU_TESS_MAX_COORD                               = 1.0e150;

   // TessProperty
   GLU_TESS_WINDING_RULE                            = 100140;
   GLU_TESS_BOUNDARY_ONLY                           = 100141;
   GLU_TESS_TOLERANCE                               = 100142;

   // TessWinding
   GLU_TESS_WINDING_ODD                             = 100130;
   GLU_TESS_WINDING_NONZERO                         = 100131;
   GLU_TESS_WINDING_POSITIVE                        = 100132;
   GLU_TESS_WINDING_NEGATIVE                        = 100133;
   GLU_TESS_WINDING_ABS_GEQ_TWO                     = 100134;

   // TessCallback
   GLU_TESS_BEGIN                                   = 100100; // TGLUTessBeginProc
   GLU_TESS_VERTEX                                  = 100101; // TGLUTessVertexProc
   GLU_TESS_END                                     = 100102; // TGLUTessEndProc
   GLU_TESS_ERROR                                   = 100103; // TGLUTessErrorProc
   GLU_TESS_EDGE_FLAG                               = 100104; // TGLUTessEdgeFlagProc
   GLU_TESS_COMBINE                                 = 100105; // TGLUTessCombineProc
   GLU_TESS_BEGIN_DATA                              = 100106; // TGLUTessBeginDataProc
   GLU_TESS_VERTEX_DATA                             = 100107; // TGLUTessVertexDataProc
   GLU_TESS_END_DATA                                = 100108; // TGLUTessEndDataProc
   GLU_TESS_ERROR_DATA                              = 100109; // TGLUTessErrorDataProc
   GLU_TESS_EDGE_FLAG_DATA                          = 100110; // TGLUTessEdgeFlagDataProc
   GLU_TESS_COMBINE_DATA                            = 100111; // TGLUTessCombineDataProc

   // TessError
   GLU_TESS_ERROR1                                  = 100151;
   GLU_TESS_ERROR2                                  = 100152;
   GLU_TESS_ERROR3                                  = 100153;
   GLU_TESS_ERROR4                                  = 100154;
   GLU_TESS_ERROR5                                  = 100155;
   GLU_TESS_ERROR6                                  = 100156;
   GLU_TESS_ERROR7                                  = 100157;
   GLU_TESS_ERROR8                                  = 100158;

   GLU_TESS_MISSING_BEGIN_POLYGON                   = GLU_TESS_ERROR1;
   GLU_TESS_MISSING_BEGIN_CONTOUR                   = GLU_TESS_ERROR2;
   GLU_TESS_MISSING_END_POLYGON                     = GLU_TESS_ERROR3;
   GLU_TESS_MISSING_END_CONTOUR                     = GLU_TESS_ERROR4;
   GLU_TESS_COORD_TOO_LARGE                         = GLU_TESS_ERROR5;
   GLU_TESS_NEED_COMBINE_CALLBACK                   = GLU_TESS_ERROR6;

   // NURBS constants

   // NurbsProperty
   GLU_AUTO_LOAD_MATRIX                             = 100200;
   GLU_CULLING                                      = 100201;
   GLU_SAMPLING_TOLERANCE                           = 100203;
   GLU_DISPLAY_MODE                                 = 100204;
   GLU_PARAMETRIC_TOLERANCE                         = 100202;
   GLU_SAMPLING_METHOD                              = 100205;
   GLU_U_STEP                                       = 100206;
   GLU_V_STEP                                       = 100207;

   // NurbsSampling
   GLU_PATH_LENGTH                                  = 100215;
   GLU_PARAMETRIC_ERROR                             = 100216;
   GLU_DOMAIN_DISTANCE                              = 100217;

   // NurbsTrim
   GLU_MAP1_TRIM_2                                  = 100210;
   GLU_MAP1_TRIM_3                                  = 100211;

   // NurbsDisplay
   GLU_OUTLINE_POLYGON                              = 100240;
   GLU_OUTLINE_PATCH                                = 100241;

   // NurbsErrors
   GLU_NURBS_ERROR1                                 = 100251;
   GLU_NURBS_ERROR2                                 = 100252;
   GLU_NURBS_ERROR3                                 = 100253;
   GLU_NURBS_ERROR4                                 = 100254;
   GLU_NURBS_ERROR5                                 = 100255;
   GLU_NURBS_ERROR6                                 = 100256;
   GLU_NURBS_ERROR7                                 = 100257;
   GLU_NURBS_ERROR8                                 = 100258;
   GLU_NURBS_ERROR9                                 = 100259;
   GLU_NURBS_ERROR10                                = 100260;
   GLU_NURBS_ERROR11                                = 100261;
   GLU_NURBS_ERROR12                                = 100262;
   GLU_NURBS_ERROR13                                = 100263;
   GLU_NURBS_ERROR14                                = 100264;
   GLU_NURBS_ERROR15                                = 100265;
   GLU_NURBS_ERROR16                                = 100266;
   GLU_NURBS_ERROR17                                = 100267;
   GLU_NURBS_ERROR18                                = 100268;
   GLU_NURBS_ERROR19                                = 100269;
   GLU_NURBS_ERROR20                                = 100270;
   GLU_NURBS_ERROR21                                = 100271;
   GLU_NURBS_ERROR22                                = 100272;
   GLU_NURBS_ERROR23                                = 100273;
   GLU_NURBS_ERROR24                                = 100274;
   GLU_NURBS_ERROR25                                = 100275;
   GLU_NURBS_ERROR26                                = 100276;
   GLU_NURBS_ERROR27                                = 100277;
   GLU_NURBS_ERROR28                                = 100278;
   GLU_NURBS_ERROR29                                = 100279;
   GLU_NURBS_ERROR30                                = 100280;
   GLU_NURBS_ERROR31                                = 100281;
   GLU_NURBS_ERROR32                                = 100282;
   GLU_NURBS_ERROR33                                = 100283;
   GLU_NURBS_ERROR34                                = 100284;
   GLU_NURBS_ERROR35                                = 100285;
   GLU_NURBS_ERROR36                                = 100286;
   GLU_NURBS_ERROR37                                = 100287;

   // Contours types -- obsolete!
   GLU_CW                                           = 100120;
   GLU_CCW                                          = 100121;
   GLU_INTERIOR                                     = 100122;
   GLU_EXTERIOR                                     = 100123;
   GLU_UNKNOWN                                      = 100124;

   // Names without "TESS_" prefix
   GLU_BEGIN                                        = GLU_TESS_BEGIN;
   GLU_VERTEX                                       = GLU_TESS_VERTEX;
   GLU_END                                          = GLU_TESS_END;
   GLU_ERROR                                        = GLU_TESS_ERROR;
   GLU_EDGE_FLAG                                    = GLU_TESS_EDGE_FLAG;

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL Extension to the X Window System (GLX) generic constants'} {$ENDIF}

   GLX_VERSION_1_1                                  = 1;
   GLX_VERSION_1_2                                  = 1;
   GLX_VERSION_1_3                                  = 1;
   GLX_EXTENSION_NAME                               = 'GLX';
   GLX_USE_GL                                       = 1;
   GLX_BUFFER_SIZE                                  = 2;
   GLX_LEVEL                                        = 3;
   GLX_RGBA                                         = 4;
   GLX_DOUBLEBUFFER                                 = 5;
   GLX_STEREO                                       = 6;
   GLX_AUX_BUFFERS                                  = 7;
   GLX_RED_SIZE                                     = 8;
   GLX_GREEN_SIZE                                   = 9;
   GLX_BLUE_SIZE                                    = 10;
   GLX_ALPHA_SIZE                                   = 11;
   GLX_DEPTH_SIZE                                   = 12;
   GLX_STENCIL_SIZE                                 = 13;
   GLX_ACCUM_RED_SIZE                               = 14;
   GLX_ACCUM_GREEN_SIZE                             = 15;
   GLX_ACCUM_BLUE_SIZE                              = 16;
   GLX_ACCUM_ALPHA_SIZE                             = 17;

   // Error codes returned by glXGetConfig:
   GLX_BAD_SCREEN                                   = 1;
   GLX_BAD_ATTRIBUTE                                = 2;
   GLX_NO_EXTENSION                                 = 3;
   GLX_BAD_VISUAL                                   = 4;
   GLX_BAD_CONTEXT                                  = 5;
   GLX_BAD_VALUE                                    = 6;
   GLX_BAD_ENUM                                     = 7;

   // GLX 1.1 and later:
   GLX_VENDOR                                       = 1;
   GLX_VERSION                                      = 2;
   GLX_EXTENSIONS                                   = 3;

   // GLX 1.3 and later:
   GLX_CONFIG_CAVEAT                                = $20;
   GLX_DONT_CARE                                    = $FFFFFFFF;
   GLX_SLOW_CONFIG                                  = $8001;
   GLX_NON_CONFORMANT_CONFIG                        = $800D;
   GLX_X_VISUAL_TYPE                                = $22;
   GLX_TRANSPARENT_TYPE                             = $23;
   GLX_TRANSPARENT_INDEX_VALUE                      = $24;
   GLX_TRANSPARENT_RED_VALUE                        = $25;
   GLX_TRANSPARENT_GREEN_VALUE                      = $26;
   GLX_TRANSPARENT_BLUE_VALUE                       = $27;
   GLX_TRANSPARENT_ALPHA_VALUE                      = $28;
   GLX_MAX_PBUFFER_WIDTH                            = $8016;
   GLX_MAX_PBUFFER_HEIGHT                           = $8017;
   GLX_MAX_PBUFFER_PIXELS                           = $8018;
   GLX_PRESERVED_CONTENTS                           = $801B;
   GLX_LARGEST_BUFFER                               = $801C;
   GLX_DRAWABLE_TYPE                                = $8010;
   GLX_FBCONFIG_ID                                  = $8013;
   GLX_VISUAL_ID                                    = $800B;
   GLX_WINDOW_BIT                                   = $00000001;
   GLX_PIXMAP_BIT                                   = $00000002;
   GLX_PBUFFER_BIT                                  = $00000004;
   GLX_AUX_BUFFERS_BIT                              = $00000010;
   GLX_FRONT_LEFT_BUFFER_BIT                        = $00000001;
   GLX_FRONT_RIGHT_BUFFER_BIT                       = $00000002;
   GLX_BACK_LEFT_BUFFER_BIT                         = $00000004;
   GLX_BACK_RIGHT_BUFFER_BIT                        = $00000008;
   GLX_DEPTH_BUFFER_BIT                             = $00000020;
   GLX_STENCIL_BUFFER_BIT                           = $00000040;
   GLX_ACCUM_BUFFER_BIT                             = $00000080;
   GLX_RENDER_TYPE                                  = $8011;
   GLX_X_RENDERABLE                                 = $8012;
   GLX_NONE                                         = $8000;
   GLX_TRUE_COLOR                                   = $8002;
   GLX_DIRECT_COLOR                                 = $8003;
   GLX_PSEUDO_COLOR                                 = $8004;
   GLX_STATIC_COLOR                                 = $8005;
   GLX_GRAY_SCALE                                   = $8006;
   GLX_STATIC_GRAY                                  = $8007;
   GLX_TRANSPARENT_INDEX                            = $8009;
   GLX_COLOR_INDEX_TYPE                             = $8015;
   GLX_COLOR_INDEX_BIT                              = $00000002;
   GLX_SCREEN                                       = $800C;
   GLX_PBUFFER_CLOBBER_MASK                         = $08000000;
   GLX_DAMAGED                                      = $8020;
   GLX_SAVED                                        = $8021;
   GLX_WINDOW                                       = $8022;
   GLX_PBUFFER                                      = $8023;
   GLX_EXT_visual_info                              = 1;
   GLX_X_VISUAL_TYPE_EXT                            = $22;
   GLX_TRANSPARENT_TYPE_EXT                         = $23;
   GLX_TRANSPARENT_INDEX_VALUE_EXT                  = $24;
   GLX_TRANSPARENT_RED_VALUE_EXT                    = $25;
   GLX_TRANSPARENT_GREEN_VALUE_EXT                  = $26;
   GLX_TRANSPARENT_BLUE_VALUE_EXT                   = $27;
   GLX_TRANSPARENT_ALPHA_VALUE_EXT                  = $28;
   GLX_TRUE_COLOR_EXT                               = $8002;
   GLX_DIRECT_COLOR_EXT                             = $8003;
   GLX_PSEUDO_COLOR_EXT                             = $8004;
   GLX_STATIC_COLOR_EXT                             = $8005;
   GLX_GRAY_SCALE_EXT                               = $8006;
   GLX_STATIC_GRAY_EXT                              = $8007;
   GLX_NONE_EXT                                     = $8000;
   GLX_TRANSPARENT_RGB_EXT                          = $8008;
   GLX_TRANSPARENT_INDEX_EXT                        = $8009;
   GLX_VISUAL_CAVEAT_EXT                            = $20;
   GLX_SLOW_VISUAL_EXT                              = $8001;
   GLX_NON_CONFORMANT_VISUAL_EXT                    = $800D;
   GLX_SHARE_CONTEXT_EXT                            = $800A;
   GLX_VISUAL_ID_EXT                                = $800B;
   GLX_SCREEN_EXT                                   = $800C;
   GLX_3DFX_WINDOW_MODE_MESA                        = $1;
   GLX_3DFX_FULLSCREEN_MODE_MESA                    = $2;

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

type

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL Utility (GLU) types'} {$ENDIF}
   // GLU types
   TGLUNurbs = record end;
   TGLUQuadric = record end;
   TGLUTesselator = record end;

   PGLUNurbs = ^TGLUNurbs;
   PGLUQuadric = ^TGLUQuadric;
   PGLUTesselator=  ^TGLUTesselator;

   // backwards compatibility
   TGLUNurbsObj = TGLUNurbs;
   TGLUQuadricObj = TGLUQuadric;
   TGLUTesselatorObj = TGLUTesselator;
   TGLUTriangulatorObj = TGLUTesselator;

   PGLUNurbsObj = PGLUNurbs;
   PGLUQuadricObj = PGLUQuadric;
   PGLUTesselatorObj = PGLUTesselator;
   PGLUTriangulatorObj = PGLUTesselator;

   // Callback function prototypes
   // GLUQuadricCallback
   TGLUQuadricErrorProc = procedure(errorCode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GLUTessCallback
   TGLUTessBeginProc = procedure(AType: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessEdgeFlagProc = procedure(Flag: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessVertexProc = procedure(VertexData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessEndProc = procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessErrorProc = procedure(ErrNo: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessCombineProc = procedure(const Coords: TVector3d; const VertexData: TVector4p; const Weight: TVector4f; OutData: PGLPointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessBeginDataProc = procedure(AType: TGLEnum; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessEdgeFlagDataProc = procedure(Flag: TGLboolean; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessVertexDataProc = procedure(VertexData: Pointer; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessEndDataProc = procedure(UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessErrorDataProc = procedure(ErrNo: TGLEnum; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessCombineDataProc = procedure(const Coords: TVector3d; const VertexData: TVector4p; const Weight: TVector4f; OutData: PGLPointer; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GLUNurbsCallback
   TGLUNurbsErrorProc = procedure(ErrorCode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL v1.1 core functions and procedures'} {$ENDIF}
   procedure glBindTexture(target: TGLEnum; texture: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glBlendFunc(sfactor: TGLEnum; dfactor: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glClear(mask: TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glClearColor(red, green, blue, alpha: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glClearDepth(depth: TGLclampd); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glClearStencil(s: TGLint ); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glColorMask(red, green, blue, alpha: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glCopyTexImage1D(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glCopyTexImage2D(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width, height: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glCopyTexSubImage1D(target: TGLEnum; level, xoffset, x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glCopyTexSubImage2D(target: TGLEnum; level, xoffset, yoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glCullFace(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glDeleteTextures(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDepthFunc(func: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDepthMask(flag: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDepthRange(zNear, zFar: TGLclampd); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDisable(cap: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glDrawArrays(mode: TGLEnum; first: TGLint; count: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDrawBuffer(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDrawElements(mode: TGLEnum; count: TGLsizei; atype: TGLEnum; indices: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;   

   procedure glEnable(cap: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glFinish; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glFlush; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glFrontFace(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glGenTextures(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetBooleanv(pname: TGLEnum; params: PGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glGetDoublev(pname: TGLEnum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   function  glGetError: TGLuint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetFloatv(pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetIntegerv(pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glGetPointerv(pname: TGLEnum; var params); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   function  glGetString(name: TGLEnum): PGLChar; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;   
   procedure glGetTexImage(target: TGLEnum; level: TGLint; format, atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetTexLevelParameterfv(target: TGLEnum; level: TGLint; pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetTexLevelParameteriv(target: TGLEnum; level: TGLint; pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetTexParameterfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetTexParameteriv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glHint(target, mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   function  glIsEnabled(cap: TGLEnum): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   function  glIsTexture(texture: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glLineWidth(width: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glLogicOp(opcode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glPointSize(size: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glPolygonMode(face, mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glPolygonOffset(factor, units: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glReadBuffer(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glReadPixels(x, y: TGLint; width, height: TGLsizei; format, atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glScissor(x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glStencilFunc(func: TGLEnum; ref: TGLint; mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glStencilMask(mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glStencilOp(fail, zfail, zpass: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glTexImage1D(target: TGLEnum; level, internalformat: TGLint; width: TGLsizei; border: TGLint; format,
                          atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexImage2D(target: TGLEnum; level, internalformat: TGLint; width, height: TGLsizei; border: TGLint;
                          format, atype: TGLEnum; Pixels:Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexParameterf(target, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexParameterfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexParameteri(target, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexParameteriv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexSubImage1D(target: TGLEnum; level, xoffset: TGLint; width: TGLsizei; format, atype: TGLEnum;
                             pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexSubImage2D(target: TGLEnum; level, xoffset, yoffset: TGLint; width, height: TGLsizei; format,
                             atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glViewport(x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 1.1 deprecated'} {$ENDIF}
   procedure glAccum(op: TGLuint; value: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glAlphaFunc(func: TGLEnum; ref: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   function  glAreTexturesResident(n: TGLsizei; Textures: PGLuint; residences: PGLboolean): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glArrayElement(i: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glBegin(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glBitmap(width: TGLsizei; height: TGLsizei; xorig, yorig: TGLfloat; xmove: TGLfloat; ymove: TGLfloat; bitmap: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glCallList(list: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glCallLists(n: TGLsizei; atype: TGLEnum; lists: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glClearAccum(red, green, blue, alpha: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glClearIndex(c: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glClipPlane(plane: TGLEnum; equation: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glColor3b(red, green, blue: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3bv(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3d(red, green, blue: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3f(red, green, blue: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3i(red, green, blue: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3s(red, green, blue: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3ub(red, green, blue: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3ubv(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3ui(red, green, blue: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3uiv(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3us(red, green, blue: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3usv(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4b(red, green, blue, alpha: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4bv(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4d(red, green, blue, alpha: TGLdouble ); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4f(red, green, blue, alpha: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4i(red, green, blue, alpha: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4s(red, green, blue, alpha: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4sv(v: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4ub(red, green, blue, alpha: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4ubv(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4ui(red, green, blue, alpha: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4uiv(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4us(red, green, blue, alpha: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4usv(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glColorMaterial(face: TGLEnum; mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColorPointer(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glCopyPixels(x, y: TGLint; width, height: TGLsizei; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glDeleteLists(list: TGLuint; range: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glDisableClientState(aarray: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glDrawPixels(width, height: TGLsizei; format, atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glEdgeFlag(flag: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEdgeFlagPointer(stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEdgeFlagv(flag: PGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEnableClientState(aarray: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEnd; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEndList; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord1d(u: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord1dv(u: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord1f(u: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord1fv(u: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord2d(u: TGLdouble; v: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord2dv(u: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord2f(u, v: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord2fv(u: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalMesh1(mode: TGLEnum; i1, i2: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalMesh2(mode: TGLEnum; i1, i2, j1, j2: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalPoint1(i: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalPoint2(i, j: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glFeedbackBuffer(size: TGLsizei; atype: TGLEnum; buffer: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFogf(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFogfv(pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFogi(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFogiv(pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFrustum(left, right, bottom, top, zNear, zFar: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   function  glGenLists(range: TGLsizei): TGLuint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetClipPlane(plane: TGLEnum; equation: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetLightfv(light, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetLightiv(light, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMapdv(target, query: TGLEnum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMapfv(target, query: TGLEnum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMapiv(target, query: TGLEnum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMaterialfv(face, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMaterialiv(face, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetPixelMapfv(map: TGLEnum; values: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetPixelMapuiv(map: TGLEnum; values: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetPixelMapusv(map: TGLEnum; values: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetPolygonStipple(mask: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexEnvfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexEnviv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexGendv(coord, pname: TGLEnum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexGenfv(coord, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexGeniv(coord, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glIndexMask(mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexPointer(atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexd(c: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexdv(c: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexf(c: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexfv(c: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexi(c: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexiv(c: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexs(c: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexsv(c: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexub(c: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexubv(c: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glInitNames; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glInterleavedArrays(format: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   function  glIsList(list: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightModelf(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightModelfv(pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightModeli(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightModeliv(pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightf(light, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightfv(light, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLighti(light, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightiv(light, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLineStipple(factor: TGLint; pattern: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glListBase(base: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLoadIdentity; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLoadMatrixd(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLoadMatrixf(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLoadName(name: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glMap1d(target: TGLEnum; u1, u2: TGLdouble; stride, order: TGLint; points: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMap1f(target: TGLEnum; u1, u2: TGLfloat; stride, order: TGLint; points: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMap2d(target: TGLEnum; u1, u2: TGLdouble; ustride, uorder: TGLint; v1, v2: TGLdouble; vstride,
                     vorder: TGLint; points: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMap2f(target: TGLEnum; u1, u2: TGLfloat; ustride, uorder: TGLint; v1, v2: TGLfloat; vstride,
                     vorder: TGLint; points: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMapGrid1d(un: TGLint; u1, u2: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMapGrid1f(un: TGLint; u1, u2: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMapGrid2d(un: TGLint; u1, u2: TGLdouble; vn: TGLint; v1, v2: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMapGrid2f(un: TGLint; u1, u2: TGLfloat; vn: TGLint; v1, v2: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMaterialf(face, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMaterialfv(face, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMateriali(face, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMaterialiv(face, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMatrixMode(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMultMatrixd(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMultMatrixf(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNewList(list: TGLuint; mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3b(nx, ny, nz: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3bv(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3d(nx, ny, nz: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3f(nx, ny, nz: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3i(nx, ny, nz: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3s(nx, ny, nz: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormalPointer(atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glOrtho(left, right, bottom, top, zNear, zFar: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPassThrough(token: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelMapfv(map: TGLEnum; mapsize: TGLsizei; values: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelMapuiv(map: TGLEnum; mapsize: TGLsizei; values: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelMapusv(map: TGLEnum; mapsize: TGLsizei; values: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelStoref(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glPixelStorei(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glPixelTransferf(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelTransferi(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelZoom(xfactor, yfactor: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPolygonStipple(mask: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPopAttrib; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPopClientAttrib; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPopMatrix; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPopName; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPrioritizeTextures(n: TGLsizei; textures: PGLuint; priorities: PGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPushAttrib(mask: TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPushClientAttrib(mask: TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPushMatrix; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPushName(name: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glRasterPos2d(x, y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2f(x, y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2i(x, y: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2s(x, y: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3d(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3f(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3i(x, y, z: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3s(x, y, z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4d(x, y, z, w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4f(x, y, z, w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4i(x, y, z, w: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4s(x, y, z, w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectd(x1, y1, x2, y2: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectdv(v1, v2: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectf(x1, y1, x2, y2: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectfv(v1, v2: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRecti(x1, y1, x2, y2: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectiv(v1, v2: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRects(x1, y1, x2, y2: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectsv(v1, v2: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   function  glRenderMode(mode: TGLEnum): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRotated(angle, x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRotatef(angle, x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glScaled(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glScalef(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glSelectBuffer(size: TGLsizei; buffer: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glShadeModel(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1d(s: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1f(s: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1i(s: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1s(s: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2d(s, t: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2f(s, t: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2i(s, t: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2s(s, t: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3d(s, t, r: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3f(s, t, r: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3i(s, t, r: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3s(s, t, r: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4d(s, t, r, q: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4f(s, t, r, q: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4i(s, t, r, q: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4s(s, t, r, q: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoordPointer(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexEnvf(target, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexEnvfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexEnvi(target, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexEnviv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGend(coord, pname: TGLEnum; param: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGendv(coord, pname: TGLEnum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGenf(coord, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGenfv(coord, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGeni(coord, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGeniv(coord, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTranslated(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTranslatef(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glVertex2d(x, y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2f(x, y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2i(x, y: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2s(x, y: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3d(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3f(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3i(x, y, z: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3s(x, y, z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4d(x, y, z, w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4f(x, y, z, w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4i(x, y, z, w: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4s(x, y, z, w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertexPointer(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL utility (GLU) functions and procedures'} {$ENDIF}
   function  gluErrorString(errCode: TGLEnum): PGLChar; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluGetString(name: TGLEnum): PGLChar; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluOrtho2D(left, right, bottom, top: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluPerspective(fovy, aspect, zNear, zFar: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluPickMatrix(x, y, width, height: TGLdouble; const viewport: TVector4i); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluLookAt(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluProject(objx, objy, objz: TGLdouble; const modelMatrix: TMatrix4d; const projMatrix: TMatrix4d; const viewport: TVector4i;
                        winx, winy, winz: PGLdouble): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluUnProject(winx, winy, winz: TGLdouble; const modelMatrix: TMatrix4d; const projMatrix: TMatrix4d; const viewport: TVector4i;
                          objx, objy, objz: PGLdouble): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluScaleImage(format: TGLEnum; widthin, heightin: TGLint; typein: TGLEnum; datain: Pointer; widthout,
                           heightout: TGLint; typeout: TGLEnum; dataout: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluBuild1DMipmaps(target: TGLEnum; components, width: TGLint; format, atype: TGLEnum;
                               data: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluBuild2DMipmaps(target: TGLEnum; components, width, height: TGLint; format, atype: TGLEnum;
                               data: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluNewQuadric: PGLUquadric; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluDeleteQuadric(state: PGLUquadric); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricNormals(quadObject: PGLUquadric; normals: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricTexture(quadObject: PGLUquadric; textureCoords: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricOrientation(quadObject: PGLUquadric; orientation: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricDrawStyle(quadObject: PGLUquadric; drawStyle: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluCylinder(quadObject: PGLUquadric; baseRadius, topRadius, height: TGLdouble; slices,
                         stacks: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluDisk(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluPartialDisk(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint;
                            startAngle, sweepAngle: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluSphere(quadObject: PGLUquadric; radius: TGLdouble; slices, stacks: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricCallback(quadObject: PGLUquadric; which: TGLEnum; fn: TGLUQuadricErrorProc); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluNewTess: PGLUtesselator; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluDeleteTess(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessBeginPolygon(tess: PGLUtesselator; polygon_data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessBeginContour(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessVertex(tess: PGLUtesselator; const coords: TVector3d; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessEndContour(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessEndPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessProperty(tess: PGLUtesselator; which: TGLEnum; value: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessNormal(tess: PGLUtesselator; x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessCallback(tess: PGLUtesselator; which: TGLEnum; fn: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluGetTessProperty(tess: PGLUtesselator; which: TGLEnum; value: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluNewNurbsRenderer: PGLUnurbs; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluDeleteNurbsRenderer(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluBeginSurface(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluBeginCurve(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluEndCurve(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluEndSurface(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluBeginTrim(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluEndTrim(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluPwlCurve(nobj: PGLUnurbs; count: TGLint; points: PGLfloat; stride: TGLint; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNurbsCurve(nobj: PGLUnurbs; nknots: TGLint; knot: PGLfloat; stride: TGLint; ctlarray: PGLfloat; order: TGLint; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNurbsSurface(nobj: PGLUnurbs; sknot_count: TGLint; sknot: PGLfloat; tknot_count: TGLint; tknot: PGLfloat; s_stride, t_stride: TGLint; ctlarray: PGLfloat; sorder, torder: TGLint; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluLoadSamplingMatrices(nobj: PGLUnurbs; const modelMatrix: TMatrix4f; const projMatrix: TMatrix4f; const viewport: TVector4i); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNurbsProperty(nobj: PGLUnurbs; aproperty: TGLEnum; value: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluGetNurbsProperty(nobj: PGLUnurbs; aproperty: TGLEnum; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNurbsCallback(nobj: PGLUnurbs; which: TGLEnum; fn: TGLUNurbsErrorProc); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluBeginPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNextContour(tess: PGLUtesselator; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluEndPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'Windows OpenGL (WGL) support functions'} {$ENDIF}
   {$IFDEF SUPPORT_WGL}
   function wglGetProcAddress(ProcName: PGLChar): Pointer; stdcall; external opengl32;
   function wglCopyContext(p1: HGLRC; p2: HGLRC; p3: Cardinal): BOOL; stdcall; external opengl32;
   function wglCreateContext(DC: HDC): HGLRC; stdcall; external opengl32;
   function wglCreateLayerContext(p1: HDC; p2: Integer): HGLRC; stdcall; external opengl32;
   function wglDeleteContext(p1: HGLRC): BOOL; stdcall; external opengl32;
   function wglDescribeLayerPlane(p1: HDC; p2, p3: Integer; p4: Cardinal; var p5: TLayerPlaneDescriptor): BOOL; stdcall; external opengl32;
   function wglGetCurrentContext: HGLRC; stdcall; external opengl32;
   function wglGetCurrentDC: HDC; stdcall; external opengl32;
   function wglGetLayerPaletteEntries(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall; external opengl32;
   function wglMakeCurrent(DC: HDC; p2: HGLRC): BOOL; stdcall; external opengl32;
   function wglRealizeLayerPalette(p1: HDC; p2: Integer; p3: BOOL): BOOL; stdcall; external opengl32;
   function wglSetLayerPaletteEntries(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall; external opengl32;
   function wglShareLists(p1, p2: HGLRC): BOOL; stdcall; external opengl32;
   function wglSwapLayerBuffers(p1: HDC; p2: Cardinal): BOOL; stdcall; external opengl32;
   function wglSwapMultipleBuffers(p1: UINT; const p2: PWGLSwap): DWORD; stdcall; external opengl32;
   function wglUseFontBitmapsA(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32;
   function wglUseFontOutlinesA (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
   function wglUseFontBitmapsW(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32;
   function wglUseFontOutlinesW (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
   function wglUseFontBitmaps(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32 name 'wglUseFontBitmapsA';
   function wglUseFontOutlines(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32 name 'wglUseFontOutlinesA';
   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL Extension to the X Window System (GLX) support functions'} {$ENDIF}
   {$IFDEF SUPPORT_GLX}
   function glXChooseVisual(dpy: PDisplay; screen: TGLint; attribList: PGLint): PXVisualInfo; cdecl; external opengl32;
   function glXCreateContext(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: TGLboolean): GLXContext; cdecl; external opengl32;
   procedure glXDestroyContext(dpy: PDisplay; ctx: GLXContext); cdecl; external opengl32;
   function glXMakeCurrent(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): TGLboolean; cdecl; external opengl32;
   procedure glXCopyContext(dpy: PDisplay; src: GLXContext; dst: GLXContext; mask: TGLuint); cdecl; external opengl32;
   procedure glXSwapBuffers(dpy: PDisplay; drawable: GLXDrawable); cdecl; external opengl32;
   function glXCreateGLXPixmap(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap): GLXPixmap; cdecl; external opengl32;
   procedure glXDestroyGLXPixmap(dpy: PDisplay; pixmap: GLXPixmap); cdecl; external opengl32;
   function glXQueryExtension(dpy: PDisplay; errorb: PGLInt; event: PGLInt): TGLboolean; cdecl; external opengl32;
   function glXQueryVersion(dpy: PDisplay; maj: PGLInt; min: PGLINT): TGLboolean; cdecl; external opengl32;
   function glXIsDirect(dpy: PDisplay; ctx: GLXContext): TGLboolean; cdecl; external opengl32;
   function glXGetConfig(dpy: PDisplay; visual: PXVisualInfo; attrib: TGLInt; value: PGLInt): TGLInt; cdecl; external opengl32;
   function glXGetCurrentContext: GLXContext; cdecl; external opengl32;
   function glXGetCurrentDrawable: GLXDrawable; cdecl; external opengl32;
   procedure glXWaitGL; cdecl; external opengl32;
   procedure glXWaitX; cdecl; external opengl32;
   procedure glXUseXFont(font: XFont; first: TGLInt; count: TGLInt; list: TGLint); cdecl; external opengl32;

   // GLX 1.1 and later
   function glXQueryExtensionsString(dpy: PDisplay; screen: TGLInt): PGLChar; cdecl; external opengl32;
   function glXQueryServerString(dpy: PDisplay; screen: TGLInt; name: TGLInt): PGLChar; cdecl; external opengl32;
   function glXGetClientString(dpy: PDisplay; name: TGLInt): PGLChar; cdecl; external opengl32;

   // GLX 1.2 and later
   function glXGetCurrentDisplay: PDisplay; cdecl; external opengl32;

   // GLX 1.3 and later
   function glXChooseFBConfig(dpy: PDisplay; screen: TGLInt; attribList: PGLInt; nitems: PGLInt): GLXFBConfig; cdecl; external opengl32;
   function glXGetFBConfigAttrib(dpy: PDisplay; config: GLXFBConfig; attribute: TGLInt; value: PGLInt): TGLInt; cdecl; external opengl32;
   function glXGetFBConfigs(dpy: PDisplay; screen: TGLInt; nelements: PGLInt): GLXFBConfig; cdecl; external opengl32;
   function glXGetVisualFromFBConfig(dpy: PDisplay; config: GLXFBConfig): PXVisualInfo; cdecl; external opengl32;
   function glXCreateWindow(dpy: PDisplay; config: GLXFBConfig; win: XWindow; const attribList: PGLInt): GLXWindow; cdecl; external opengl32;
   procedure glXDestroyWindow(dpy: PDisplay; window: GLXWindow); cdecl; external opengl32;
   function glXCreatePixmap(dpy: PDisplay; config: GLXFBConfig; pixmap: XPixmap; attribList: PGLInt): GLXPixmap; cdecl; external opengl32;
   procedure glXDestroyPixmap(dpy: PDisplay; pixmap: GLXPixmap); cdecl; external opengl32;
   function glXCreatePbuffer(dpy: PDisplay; config: GLXFBConfig; attribList: PGLInt): GLXPBuffer; cdecl; external opengl32;
   procedure glXDestroyPbuffer(dpy: PDisplay; pbuf: GLXPBuffer); cdecl; external opengl32;
   procedure glXQueryDrawable(dpy: PDisplay; draw: GLXDrawable; attribute: TGLInt; value: PGLuint); cdecl; external opengl32;
   function glXCreateNewContext(dpy: PDisplay; config: GLXFBConfig; renderType: TGLInt; shareList: GLXContext; direct: TGLboolean): GLXContext; cdecl; external opengl32;
   function glXMakeContextCurrent(dpy: PDisplay; draw: GLXDrawable; read: GLXDrawable; ctx: GLXContext): TGLboolean; cdecl; external opengl32;
   function glXGetCurrentReadDrawable: GLXDrawable; cdecl; external opengl32;
   function glXQueryContext(dpy: PDisplay; ctx: GLXContext; attribute: TGLInt; value: PGLInt): TGLInt; cdecl; external opengl32;
   procedure glXSelectEvent(dpy: PDisplay; drawable: GLXDrawable; mask: TGLsizei); cdecl; external opengl32;
   procedure glXGetSelectedEvent(dpy: PDisplay; drawable: GLXDrawable; mask: TGLsizei); cdecl; external opengl32;
   //should these extensions should be loaded dynamically?
   function glXGetVideoSyncSGI(count: PGLuint): TGLInt; cdecl; external opengl32;
   function glXWaitVideoSyncSGI(divisor: TGLInt; remainder: TGLInt; count: PGLuint): TGLInt; cdecl; external opengl32;
   procedure glXFreeContextEXT(dpy: PDisplay; context: GLXContext); cdecl; external opengl32;
   function glXGetContextIDEXT(const context: GLXContext): GLXContextID; cdecl; external opengl32;
   function glXGetCurrentDisplayEXT: PDisplay; cdecl; external opengl32;
   function glXImportContextEXT(dpy: PDisplay; contextID: GLXContextID): GLXContext; cdecl; external opengl32;
   function glXQueryContextInfoEXT(dpy: PDisplay; context: GLXContext; attribute: TGLInt; value: PGLInt): TGLInt; cdecl; external opengl32;
   procedure glXCopySubBufferMESA(dpy: PDisplay; drawable: GLXDrawable; x: TGLInt; y: TGLInt; width: TGLInt; height: TGLInt); cdecl; external opengl32;
   function glXCreateGLXPixmapMESA(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap; cmap: XColormap): GLXPixmap; cdecl; external opengl32;
   function glXReleaseBuffersMESA(dpy: PDisplay; d: GLXDrawable): TGLboolean; cdecl; external opengl32;
   function glXSet3DfxModeMESA(mode: TGLint): TGLboolean; cdecl; external opengl32;
   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF MULTITHREADOPENGL}
threadvar
{$else}
var
{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL extension function/procedure definitions'} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 1.2'} {$ENDIF}
   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 1.2 Core
   //  ###########################################################

   // promoted to core v1.2 from GL_EXT_blend_color (#2)
   glBlendColor: procedure(red, green, blue, alpha: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.2 from GL_EXT_blend_minmax (#37)
   glBlendEquation: procedure(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.2 from GL_EXT_draw_range_elements (#112)
   glDrawRangeElements: procedure(mode: TGLEnum; Astart, Aend: TGLuint; count: TGLsizei; Atype: TGLEnum;
                                  indices: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.2 from GL_EXT_texture3D (#6)
   glTexImage3D: procedure(target: TGLEnum; level: TGLint; internalformat: TGLEnum; width, height, depth: TGLsizei;
                           border: TGLint; format: TGLEnum; Atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexSubImage3D: procedure(target: TGLEnum; level, xoffset, yoffset, zoffset: TGLint;  width, height, depth: TGLsizei;
                              format: TGLEnum; Atype: TGLEnum; pixels: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.2 from GL_EXT_copy_texture
   glCopyTexSubImage3D: procedure(target: TGLEnum; level, xoffset, yoffset, zoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 1.2 deprecated'} {$ENDIF}
   // promoted to core v1.2 from GL_SGI_color_table (#14)
   glColorTable: procedure(target, internalformat: TGLEnum; width: TGLsizei; format, Atype: TGLEnum;
                           table: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glColorTableParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glColorTableParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glCopyColorTable: procedure(target, internalformat: TGLEnum; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetColorTable: procedure(target, format, Atype: TGLEnum; table: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetColorTableParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetColorTableParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

   // promoted to core v1.2 from GL_EXT_color_subtable (#74)
   glColorSubTable: procedure(target: TGLEnum; start, count: TGLsizei; format, Atype: TGLEnum; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glCopyColorSubTable: procedure(target: TGLEnum; start: TGLsizei; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

   // promoted to core v1.2 from GL_EXT_convolution (#12)
   glConvolutionFilter1D: procedure(target, internalformat: TGLEnum; width: TGLsizei; format, Atype: TGLEnum;
     image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glConvolutionFilter2D: procedure(target, internalformat: TGLEnum; width, height: TGLsizei; format, Atype: TGLEnum;
     image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glConvolutionParameterf: procedure(target, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glConvolutionParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glConvolutionParameteri: procedure(target, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glConvolutionParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glCopyConvolutionFilter1D: procedure(target, internalformat: TGLEnum; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glCopyConvolutionFilter2D: procedure(target, internalformat: TGLEnum; x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetConvolutionFilter: procedure(target, internalformat, Atype: TGLEnum; image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetConvolutionParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetConvolutionParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetSeparableFilter: procedure(target, format, Atype: TGLEnum; row, column, span: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSeparableFilter2D: procedure(target, internalformat: TGLEnum; width, height: TGLsizei; format, Atype: TGLEnum; row,
     column: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

   // promoted to core v1.2 from GL_EXT_histogram (#11)
   glGetHistogram: procedure(target: TGLEnum; reset: TGLboolean; format, Atype: TGLEnum; values: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetHistogramParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetHistogramParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetMinmax: procedure(target: TGLEnum; reset: TGLboolean; format, Atype: TGLEnum; values: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetMinmaxParameterfv: procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glGetMinmaxParameteriv: procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glHistogram: procedure(target: TGLEnum; width: TGLsizei; internalformat: TGLEnum; sink: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMinmax: procedure(target, internalformat: TGLEnum; sink: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glResetHistogram: procedure(target: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glResetMinmax: procedure(target: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 1.3'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 1.3 Core
   //  ###########################################################

   // promoted to core v1.3 from GL_ARB_multitexture (#1)
   glActiveTexture: procedure(texture: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   // promoted to core v1.3 from GL_ARB_multisample (#5)
   glSampleCoverage: procedure(Value: TGLclampf; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.3 from GL_ARB_texture_compression (#12)
   glCompressedTexImage3D: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height, depth: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexImage2D: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexImage1D: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexSubImage3D: procedure(target: TGLenum; level: TGLint; xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexSubImage2D: procedure(target: TGLenum; level: TGLint; xoffset, yoffset: TGLint; width, height: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexSubImage1D: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetCompressedTexImage: procedure(target: TGLenum; level: TGLint; img: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 1.3 deprecated'} {$ENDIF}
   // promoted to core v1.3 from GL_ARB_multitexture (#1)
   glClientActiveTexture: procedure(texture: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1d: procedure(target: TGLenum; s: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1dV: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1f: procedure(target: TGLenum; s: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1fV: procedure(target: TGLenum; v: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1i: procedure(target: TGLenum; s: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1iV: procedure(target: TGLenum; v: PGLInt); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1s: procedure(target: TGLenum; s: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord1sV: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2d: procedure(target: TGLenum; s, t: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2dv: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2f: procedure(target: TGLenum; s, t: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2fv: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2i: procedure(target: TGLenum; s, t: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2iv: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2s: procedure(target: TGLenum; s, t: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord2sv: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3d: procedure(target: TGLenum; s, t, r: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3dv: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3f: procedure(target: TGLenum; s, t, r: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3fv: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3i: procedure(target: TGLenum; s, t, r: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3iv: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3s: procedure(target: TGLenum; s, t, r: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord3sv: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4d: procedure(target: TGLenum; s, t, r, q: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4dv: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4f: procedure(target: TGLenum; s, t, r, q: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4fv: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4i: procedure(target: TGLenum; s, t, r, q: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4iv: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4s: procedure(target: TGLenum; s, t, r, q: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultiTexCoord4sv: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

   // promoted to core v1.3 from GL_ARB_transpose_matrix
   glLoadTransposeMatrixf: procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glLoadTransposeMatrixd: procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultTransposeMatrixf: procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glMultTransposeMatrixd: procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 1.4'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 1.4 Core
   //  ###########################################################

   // promoted to core v1.4 from GL_EXT_blend_func_separate (#173)
   glBlendFuncSeparate: procedure(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.4 from GL_EXT_multi_draw_arrays (#148)
   glMultiDrawArrays: procedure(mode: TGLenum; First: PGLint; Count: PGLsizei; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiDrawElements: procedure(mode: TGLenum; Count: PGLsizei; AType: TGLenum; var indices; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.4 from GL_ARB_point_parameters (#14), GL_NV_point_sprite (#262)
   glPointParameterf: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPointParameterfv: procedure(pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPointParameteri: procedure(pname: TGLenum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPointParameteriv: procedure(pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL 1.4 deprecated'} {$ENDIF}
   // promoted to core v1.4 from GL_EXT_fog_coord (#149)
   glFogCoordf: procedure(coord: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glFogCoordfv: procedure(coord: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glFogCoordd: procedure(coord: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glFogCoorddv: procedure(coord: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glFogCoordPointer: procedure(AType: TGLenum; stride: TGLsizei; p: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

   // promoted to core v1.4 from GL_EXT_secondary_color (#145)
   glSecondaryColor3b: procedure(red, green, blue: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3bv: procedure(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3d: procedure(red, green, blue: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3dv: procedure(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3f: procedure(red, green, blue: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3fv: procedure(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3i: procedure(red, green, blue: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3iv: procedure(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3s: procedure(red, green, blue: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3sv: procedure(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3ub: procedure(red, green, blue: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3ubv: procedure(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3ui: procedure(red, green, blue: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3uiv: procedure(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3us: procedure(red, green, blue: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColor3usv: procedure(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glSecondaryColorPointer: procedure(Size: TGLint; Atype: TGLenum; stride: TGLsizei; p: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

   // promoted to core v1.4 from GL_ARB_window_pos (#25)
   glWindowPos2d: procedure(x,y : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos2dv: procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos2f: procedure(x,y : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos2fv: procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos2i: procedure(x,y : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos2iv: procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos2s: procedure(x,y : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos2sv: procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3d: procedure(x,y,z : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3dv: procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3f: procedure(x,y,z : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3fv: procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3i: procedure(x,y,z : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3iv: procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3s: procedure(x,y,z : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
   glWindowPos3sv: procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 1.5'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 1.5 Core
   //  ###########################################################

   // promoted to core v1.5 from GL_ARB_occlusion_query (#29)
   glGenQueries: procedure(n: TGLsizei; ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteQueries: procedure(n: TGLsizei; const ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsQuery:  function(id: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBeginQuery: procedure(target: TGLenum; id: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndQuery: procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryiv: procedure(target: TGLEnum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryObjectiv: procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryObjectuiv: procedure(id: TGLuint; pname: TGLenum; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.5 from GL_ARB_vertex_buffer_object (#28)
   glBindBuffer: procedure(target: GLenum; buffer: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteBuffers: procedure(n: GLsizei; const buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenBuffers: procedure(n: GLsizei; buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsBuffer: function(buffer: GLuint): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBufferData: procedure(target: GLenum; size: GLsizei; const data: Pointer; usage: GLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBufferSubData: procedure(target: GLenum; offset: GLuint; size: GLsizei; const data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferSubData: procedure(target: GLenum; offset: GLuint; size: GLsizei; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMapBuffer: function(target: GLenum; access: GLenum): Pointer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUnmapBuffer: function(target: GLenum): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferParameteriv: procedure(target: GLenum; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferPointerv: procedure(target: GLenum; pname: GLenum; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v1.5 from GL_EXT_shadow_funcs (#267)
   // (no functions or procedures)

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 2.0'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 2.0 Core
   //  ###########################################################

   // promoted to core v2.0 from GL_EXT_blend_equation_separate (#299)
   glBlendEquationSeparate: procedure(modeRGB: TGLenum; modeAlpha: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v2.0 from GL_ARB_draw_buffers (#37)
   glDrawBuffers: procedure(n: GLSizei; const bufs: PGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v2.0 from GL_ARB_stencil_two_side (no # found)
   glStencilOpSeparate: procedure(face, sfail, dpfail, dppass: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glStencilFuncSeparate: procedure(face, func: TGLenum; ref: TGLint; mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glStencilMaskSeparate: procedure(face: TGLenum; mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v2.0 from GL_ARB_shader_objects (#30) / GL_ARB_vertex_shader (#31) / GL_ARB_fragment_shader (#32)
   glAttachShader: procedure(_program: TGLuint; shader: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindAttribLocation: procedure(_program: TGLuint; index: TGLuint; const name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF unix} cdecl; {$ENDIF}
   glCompileShader: procedure(shader: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCreateProgram: function(): TGLuint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCreateShader: function(_type: TGLenum): TGLuint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteProgram: procedure(_program: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteShader: procedure(shader: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDetachShader: procedure(_program: TGLuint; shader: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDisableVertexAttribArray: procedure(index: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEnableVertexAttribArray: procedure(index: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveAttrib: procedure(_program: TGLuint; index: TGLuint; bufSize: TGLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveUniform: procedure(_program: TGLuint; index: TGLuint; bufSize: TGLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetAttachedShaders: procedure(_program: TGLuint; maxCount: TGLsizei; count: PGLSizei; obj: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetAttribLocation: function(_program: TGLuint; const name: PGLChar): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramiv: procedure(_program: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramInfoLog: procedure(_program: TGLuint; bufSize: TGLsizei; length: PGLsizei; infoLog: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetShaderiv: procedure(shader: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetShaderInfoLog: procedure(shader: TGLuint; bufSize: TGLsizei; length: PGLsizei; infoLog: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetShaderSource: procedure(shader:TGLuint; bufSize: TGLsizei; length: PGLsizei; source: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformLocation: function(_program: TGLuint; const name: PGLChar): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformfv: procedure(_program: TGLuint; location: TGLint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformiv: procedure(_program: TGLuint; location: TGLint; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribdv: procedure(index:TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribfv: procedure(index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribiv: procedure(index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribPointerv: procedure(index: TGLuint; pname: TGLenum; _pointer: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsProgram: function(_program: TGLuint):TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsShader: function(shader: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glLinkProgram: procedure(_program: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glShaderSource: procedure(shader: TGLuint; count: TGLsizei; const _string: PGLPCharArray; const length: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUseProgram: procedure(_program: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1f: procedure(location: GLint; v0: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2f: procedure(location: GLint; v0: GLfloat; v1: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1i: procedure(location: GLint; v0: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2i: procedure(location: GLint; v0: GLint; v1: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4fv: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1iv: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2iv: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3iv: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4iv: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glValidateProgram: procedure(_program: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1d: procedure(index: TGLuint; x: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1dv: procedure(index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1f: procedure(index: TGLuint; x: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1fv: procedure(index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1s: procedure(index: TGLuint; x: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1sv: procedure(index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2d: procedure(index: TGLuint; x,y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2dv: procedure(index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2f: procedure(index: TGLuint; x,y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2fv: procedure(index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2s: procedure(index: TGLuint; x,y: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2sv: procedure(index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3d: procedure(index: TGLuint; x,y,z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3dv: procedure(index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3f: procedure(index: TGLuint; x,y,z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3fv: procedure(index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3s: procedure(index: TGLuint; x,y,z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3sv: procedure(index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4Nbv: procedure(index: TGLuint; v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4Niv: procedure(index: TGLuint; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4Nsv: procedure(index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4Nub: procedure(index: TGLuint; x,y,z,w: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4Nubv: procedure(index: TGLuint; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4Nuiv: procedure(index: TGLuint; v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4Nusv: procedure(index: TGLuint; v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4bv: procedure(index: TGLuint; v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4d: procedure(index: TGLuint; x,y,z,w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4dv: procedure(index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4f: procedure(index: TGLuint; x,y,z,w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4fv: procedure(index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4iv: procedure(index: TGLuint; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4s: procedure(index: TGLuint; x,y,z,w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4sv: procedure(index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4ubv: procedure(index: TGLuint; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4uiv: procedure(index: TGLuint; v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4usv: procedure(index: TGLuint; v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribPointer: procedure(index: TGLuint; size: TGLint; _type: TGLenum; normalized: TGLboolean; stride: TGLsizei; _pointer: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 2.1'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 2.1 Core
   //  ###########################################################

   // promoted to core v2.1 from GL_ARB_pixel_buffer_object
   // (no functions or procedures)

   // promoted to core v2.1 from GL_EXT_texture_sRGB (#315)
   // (no functions or procedures)

   // New commands in OpenGL 2.1
   glUniformMatrix2x3fv: procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix3x2fv: procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix2x4fv: procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix4x2fv: procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix3x4fv: procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix4x3fv: procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 3.0'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 3.0 Core
   //  ###########################################################

   // promoted to core v3.0 from GL_EXT_gpu_shader4
   glVertexAttribI1i: procedure(index: TGLuint; x: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2i: procedure(index: TGLuint; x: TGLint; y: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3i: procedure(index: TGLuint; x: TGLint; y: TGLint; z: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4i: procedure(index: TGLuint; x: TGLint; y: TGLint; z: TGLint; w: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI1ui: procedure(index: TGLuint; x: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2ui: procedure(index: TGLuint; x: TGLuint; y: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3ui: procedure(index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4ui: procedure(index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint; w: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI1iv: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2iv: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3iv: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4iv: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI1uiv: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2uiv: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3uiv: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4uiv: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4bv: procedure(index: TGLuint; v:PGLbyte);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4sv: procedure(index: TGLuint; v:PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4ubv: procedure(index: TGLuint; v: PGLUbyte);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4usv: procedure(index: TGLuint; v: PGLushort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribIPointer: procedure(index: TGLuint; size: TGLint; _type: TGLenum;
                                stride: TGLsizei; _pointer: pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribIiv: procedure(index: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribIuiv: procedure(index: TGLuint; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1ui: procedure(location: TGLInt; v0: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2ui: procedure(location: TGLInt; v0: TGLuint; v1: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3ui: procedure(location: TGLInt; v0: TGLuint; v1: TGLuint; v2: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4ui: procedure(location: TGLInt; v0: TGLuint; v1: TGLuint; v2: TGLuint; v3: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1uiv: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2uiv: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3uiv: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4uiv: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformuiv: procedure(_program: TGLuint; location: TGLint; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindFragDataLocation: procedure(_program: TGLuint; colorNumber: TGLuint; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFragDataLocation: function(_program: TGLuint; name: PGLChar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v3.0 from GL_NV_conditional_render
   glBeginConditionalRender: procedure(id: TGLuint; mode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndConditionalRender: procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v3.0 from GL_ARB_color_buffer_float
   glClampColor: procedure (target: TGLenum; clamp: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v3.0 from GL_EXT_texture_integer
   //glClearColorIi: procedure(r: TGLint; g: TGLint; b: TGLint; a: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   //glClearColorIui: procedure(r: TGLuint; g: TGLuint; b: TGLuint; a: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexParameterIiv: procedure(target: TGLenum; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexParameterIuiv: procedure(target: TGLenum; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTexParameterIiv: procedure(target: TGLenum; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTexParameterIuiv: procedure(target: TGLenum; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // promoted to core v3.0 from GL_EXT_draw_buffers2
   glColorMaski: procedure(index: TGLuint; r: TGLboolean; g: TGLboolean;
                            b: TGLboolean; a: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBooleani_v: procedure(target: TGLenum; index: TGLuint; data: PGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetIntegeri_v: procedure(target: TGLenum; index: TGLuint; data: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEnablei: procedure(target: TGLenum; index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDisablei: procedure(target: TGLenum; index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsEnabledi: function(target: TGLenum; index: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   //promoted to core v3.0 from GL_EXT_transform_feedback
   glBindBufferRange: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                            offset:TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindBufferBase: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBeginTransformFeedback: procedure(primitiveMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndTransformFeedback: procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTransformFeedbackVaryings: procedure(_program: TGLuint; count: TGLsizei;
                                      const varyings: PGLPCharArray; bufferMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTransformFeedbackVarying: procedure(_program: TGLuint; index: TGLuint;
                                        location: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // New commands in OpenGL 3.0
   glClearBufferiv: procedure(buffer: TGLenum; drawbuffer: TGLint; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glClearBufferuiv: procedure(buffer: TGLenum; drawbuffer: TGLint; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glClearBufferfv: procedure(buffer: TGLenum; drawbuffer: TGLint; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glClearBufferfi: procedure(buffer: TGLenum; drawbuffer: TGLint; depth: TGLfloat; stencil: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetStringi: function(name: TGLenum; index: TGLuint): PGLChar;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 3.1'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 3.1 Core
   //  ###########################################################

   glDrawArraysInstanced: procedure(mode: TGLenum; first: TGLint; count: TGLsizei; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDrawElementsInstanced: procedure(mode: TGLenum; count: TGLsizei; _type: TGLenum; indices: PGLvoid; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexBuffer: procedure(target: TGLenum; internalformat: TGLenum; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPrimitiveRestartIndex: procedure(index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'New core function/procedure definitions in OpenGL 3.2'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 3.2 Core
   //  ###########################################################

   glGetInteger64i_v: procedure(target: TGLenum; index: TGLuint; data: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferParameteri64v: procedure(target: TGLenum; pname: TGLenum; params: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramParameteri: procedure(_program: TGLuint; pname: TGLenum; value: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTexture: procedure(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
//   glFramebufferTextureFace: procedure(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint; face: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   // OpenGL 3.2 also reuses entry points from these extensions:
   // GL_ARB_draw_elements_base_vertex
   // GL_ARB_provoking_vertex
   // GL_ARB_sync
   // GL_ARB_texture_multisample

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL Utility (GLU) function/procedure definitions'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //                     GLU extensions
   //  ###########################################################

   // GLU extensions
   gluNurbsCallbackDataEXT: procedure(nurb: PGLUnurbs; userData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   gluNewNurbsTessellatorEXT: function: PGLUnurbs; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   gluDeleteNurbsTessellatorEXT: procedure(nurb: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'Windows OpenGL (WGL) function/procedure definitions for ARB approved extensions'} {$ENDIF}
   {$IFDEF SUPPORT_WGL}
   //  ###########################################################
   //           function and procedure definitions for
   //               ARB approved WGL extensions
   //  ###########################################################

   // WGL_buffer_region (ARB #4)
   wglCreateBufferRegionARB: function(DC: HDC; iLayerPlane: Integer; uType: TGLenum) : Integer; stdcall;
   wglDeleteBufferRegionARB: procedure(hRegion: Integer); stdcall;
   wglSaveBufferRegionARB: function(hRegion: Integer; x, y, width, height: Integer): BOOL; stdcall;
   wglRestoreBufferRegionARB: function(hRegion: Integer; x, y, width, height: Integer;
     xSrc, ySrc: Integer): BOOL; stdcall;

   // WGL_ARB_extensions_string (ARB #8)
   wglGetExtensionsStringARB: function(DC: HDC): PGLChar; stdcall;

   // WGL_ARB_pixel_format (ARB #9)
   wglGetPixelFormatAttribivARB: function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: TGLenum;
     const piAttributes: PGLint; piValues : PGLint) : BOOL; stdcall;
   wglGetPixelFormatAttribfvARB: function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: TGLenum;
     const piAttributes: PGLint; piValues: PGLFloat) : BOOL; stdcall;
   wglChoosePixelFormatARB: function(DC: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLFloat;
     nMaxFormats: GLuint; piFormats: PGLint; nNumFormats: PGLenum) : BOOL; stdcall;

   // WGL_make_current_read (ARB #10)
   wglMakeContextCurrentARB: function(hDrawDC: HDC; hReadDC: HDC; _hglrc: HGLRC): BOOL; stdcall;
   wglGetCurrentReadDCARB: function(): HDC; stdcall;

   // WGL_ARB_pbuffer (ARB #11)
   wglCreatePbufferARB: function(DC: HDC; iPixelFormat: GLInt; iWidth, iHeight : GLInt;
     const piAttribList: PGLint) : HPBUFFERARB; stdcall;
   wglGetPbufferDCARB: function(hPbuffer: HPBUFFERARB) : HDC; stdcall;
   wglReleasePbufferDCARB: function(hPbuffer: HPBUFFERARB; DC: HDC) : Integer; stdcall;
   wglDestroyPbufferARB: function(hPbuffer: HPBUFFERARB): BOOL; stdcall;
   wglQueryPbufferARB: function(hPbuffer: HPBUFFERARB; iAttribute : Integer;
     piValue: PGLint) : BOOL; stdcall;

   // WGL_ARB_render_texture (ARB #20)
   wglBindTexImageARB: function(hPbuffer: HPBUFFERARB; iBuffer: Integer): BOOL; stdcall;
   wglReleaseTexImageARB: function(hpBuffer: HPBUFFERARB; iBuffer: Integer): BOOL; stdcall;
   wglSetPbufferAttribARB: function(hpBuffer: HPBUFFERARB; const piAttribList: PGLint): BOOL; stdcall;

   // WGL_ARB_create_context (ARB #55)
   wglCreateContextAttribsARB: function(DC: HDC; hShareContext: HGLRC;
				     attribList: PGLint):HGLRC; stdcall;

   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'Windows OpenGL (WGL) function/procedure definitions for Vendor/EXT extensions'} {$ENDIF}
   {$IFDEF SUPPORT_WGL}
   //  ###########################################################
   //           function and procedure definitions for
   //               Vendor/EXT WGL extensions
   //  ###########################################################

   // WGL_EXT_swap_control (EXT #172)
   wglSwapIntervalEXT: function(interval : Integer) : BOOL; stdcall;
   wglGetSwapIntervalEXT: function : Integer; stdcall;
   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

 {$IFDEF GLS_COMPILER_2005_UP} {$region 'GLX function/procedure definitions for ARB approved extensions'} {$ENDIF}
 {$IFDEF SUPPORT_GLX}
   //  ###########################################################
   //           function and procedure definitions for
   //               ARB approved GLX extensions
   //  ###########################################################

   // GLX_ARB_create_context (EXT #56)
   glXCreateContextAttribsARB: function(dpy: PDisplay; config: GLXFBConfig;
		    share_context: GLXContext; direct: TGLBoolean;
		    attrib_list: PGLint): GLXContext; cdecl;

   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'GLX function/procedure definitions for Vendor/EXT extensions'} {$ENDIF}
   {$IFDEF SUPPORT_GLX}
   //  ###########################################################
   //           function and procedure definitions for
   //               Vendor/EXT WGL extensions
   //  ###########################################################

   // GLX_SGI_swap_control (EXT #40)
   glXSwapIntervalSGI: function(interval: TGLint): TGLint; cdecl;

   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL function/procedure definitions for ARB approved extensions'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //                  ARB approved extensions
   //  ###########################################################

   // unknown ARB extension
   glSamplePassARB: procedure(pass: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   
   // GL_ARB_multitexture (ARB #1)
   glActiveTextureARB: procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glClientActiveTextureARB: procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1dARB: procedure(target: TGLenum; s: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1dVARB: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1fARB: procedure(target: TGLenum; s: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1fVARB: procedure(target: TGLenum; v: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1iARB: procedure(target: TGLenum; s: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1iVARB: procedure(target: TGLenum; v: PGLInt); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1sARB: procedure(target: TGLenum; s: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord1sVARB: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2dARB: procedure(target: TGLenum; s, t: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2dvARB: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2fARB: procedure(target: TGLenum; s, t: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2fvARB: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2iARB: procedure(target: TGLenum; s, t: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2ivARB: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2sARB: procedure(target: TGLenum; s, t: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord2svARB: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3dARB: procedure(target: TGLenum; s, t, r: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3dvARB: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3fARB: procedure(target: TGLenum; s, t, r: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3fvARB: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3iARB: procedure(target: TGLenum; s, t, r: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3ivARB: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3sARB: procedure(target: TGLenum; s, t, r: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord3svARB: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4dARB: procedure(target: TGLenum; s, t, r, q: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4dvARB: procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4fARB: procedure(target: TGLenum; s, t, r, q: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4fvARB: procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4iARB: procedure(target: TGLenum; s, t, r, q: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4ivARB: procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4sARB: procedure(target: TGLenum; s, t, r, q: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiTexCoord4svARB: procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_transpose_matrix (ARB #3)
   glLoadTransposeMatrixfARB: procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glLoadTransposeMatrixdARB: procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultTransposeMatrixfARB: procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultTransposeMatrixdARB: procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   
   // GL_ARB_multisample (ARB #5)
   glSampleCoverageARB: procedure(Value: TGLclampf; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_texture_compression (ARB #12)
   glCompressedTexImage3DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height, depth: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexImage2DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexImage1DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexSubImage3DARB: procedure(target: TGLenum; level: TGLint; xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexSubImage2DARB: procedure(target: TGLenum; level: TGLint; xoffset, yoffset: TGLint; width, height: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompressedTexSubImage1DARB: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetCompressedTexImageARB: procedure(target: TGLenum; level: TGLint; img: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_point_parameter (ARB #14)
   glPointParameterfARB: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPointParameterfvARB: procedure(pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_vertex_blend (ARB #15) {deprecated?}
   glWeightbvARB: procedure(size: TGLint; weights: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightsvARB: procedure(size: TGLint; weights: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightivARB: procedure(size: TGLint; weights: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightfvARB: procedure(size: TGLint; weights: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightdvARB: procedure(size: TGLint; weights: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightubvARB: procedure(size: TGLint; weights: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightusvARB: procedure(size: TGLint; weights: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightuivARB: procedure(size: TGLint; weights: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWeightPointerARB: procedure(size: TGLint; _type: TGLenum; stride:TGLsizei;
                                 _pointer:pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexBlendARB: procedure(count: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_matrix_palette (ARB #16) {deprecated?}
   glCurrentPaletteMatrixARB: procedure(index: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMatrixIndexubvARB: procedure(size: TGLint; indices: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMatrixIndexusvARB: procedure(size: TGLint; indices: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMatrixIndexuivARB: procedure(size: TGLint; indices: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMatrixIndexPointerARB: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; _pointer:pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_window_pos (ARB #25)
   glWindowPos2dARB: procedure(x,y : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos2dvARB: procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos2fARB: procedure(x,y : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos2fvARB: procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos2iARB: procedure(x,y : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos2ivARB: procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos2sARB: procedure(x,y : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos2svARB: procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3dARB: procedure(x,y,z : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3dvARB: procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3fARB: procedure(x,y,z : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3fvARB: procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3iARB: procedure(x,y,z : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3ivARB: procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3sARB: procedure(x,y,z : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWindowPos3svARB: procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_vertex_program (ARB #26)
   glVertexAttrib1dARB: procedure(index: GLuint; x: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1dvARB: procedure(index: GLuint; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1fARB: procedure(index: GLuint; x: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1fvARB: procedure(index: GLuint; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1sARB: procedure(index: GLuint; x: GLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1svARB: procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2dARB: procedure(index: GLuint; x: GLdouble; y: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2dvARB: procedure(index: GLuint; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2fARB: procedure(index: GLuint; x: GLfloat; y: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2fvARB: procedure(index: GLuint; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2sARB: procedure(index: GLuint; x: GLshort; y: GLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2svARB: procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3dARB: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3dvARB: procedure(index: GLuint; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3fARB: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3fvARB: procedure(index: GLuint; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3sARB: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3svARB: procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4NbvARB: procedure(index: GLuint; const v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4NivARB: procedure(index: GLuint; const v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4NsvARB: procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4NubARB: procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4NubvARB: procedure(index: GLuint; const v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4NuivARB: procedure(index: GLuint; const v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4NusvARB: procedure(index: GLuint; const v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4bvARB: procedure(index: GLuint; const v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4dARB: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4dvARB: procedure(index: GLuint; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4fARB: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4fvARB: procedure(index: GLuint; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4ivARB: procedure(index: GLuint; const v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4sARB: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4svARB: procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4ubvARB: procedure(index: GLuint; const v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4uivARB: procedure(index: GLuint; const v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4usvARB: procedure(index: GLuint; const v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribPointerARB: procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; const _pointer: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEnableVertexAttribArrayARB: procedure(index: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDisableVertexAttribArrayARB: procedure(index: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramStringARB: procedure(target: GLenum; format: GLenum; len: GLsizei; const _string: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindProgramARB: procedure(target: GLenum; _program: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteProgramsARB: procedure(n: GLsizei; const programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenProgramsARB: procedure(n: GLsizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramEnvParameter4dARB: procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramEnvParameter4dvARB: procedure(target: GLenum; index: GLuint; const params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramEnvParameter4fARB: procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramEnvParameter4fvARB: procedure(target: GLenum; index: GLuint; const params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramLocalParameter4dARB: procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramLocalParameter4dvARB: procedure(target: GLenum; index: GLuint; const params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramLocalParameter4fARB: procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramLocalParameter4fvARB: procedure(target: GLenum; index: GLuint; const params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramEnvParameterdvARB: procedure(target: GLenum; index: GLuint; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramEnvParameterfvARB: procedure(target: GLenum; index: GLuint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramLocalParameterdvARB: procedure(target: GLenum; index: GLuint; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramLocalParameterfvARB: procedure(target: GLenum; index: GLuint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramivARB: procedure(target: GLenum; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramStringARB: procedure(target: GLenum; pname: GLenum; _string: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribdvARB: procedure(index: GLuint; pname: GLenum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribfvARB: procedure(index: GLuint; pname: GLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribivARB: procedure(index: GLuint; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribPointervARB: procedure(index: GLuint; pname: GLenum; _pointer: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsProgramARB: function(_program: GLuint): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_vertex_buffer_object (ARB #28)
   glBindBufferARB: procedure(target: GLenum; buffer: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteBuffersARB: procedure(n: GLsizei; const buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenBuffersARB: procedure(n: GLsizei; buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsBufferARB: function(buffer: GLuint): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBufferDataARB: procedure(target: GLenum; size: GLsizei; const data: Pointer; usage: GLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBufferSubDataARB: procedure(target: GLenum; offset: GLuint; size: GLsizei; const data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferSubDataARB: procedure(target: GLenum; offset: GLuint; size: GLsizei; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMapBufferARB: function(target: GLenum; access: GLenum): Pointer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUnmapBufferARB: function(target: GLenum): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferParameterivARB: procedure(target: GLenum; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBufferPointervARB: procedure(target: GLenum; pname: GLenum; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_occlusion_query (ARB #29)
   glGenQueriesARB: procedure(n: TGLsizei; ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteQueriesARB: procedure(n: TGLsizei; const ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsQueryARB:  function(id: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBeginQueryARB: procedure(target: TGLenum; id: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndQueryARB: procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryivARB: procedure(target: TGLEnum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryObjectivARB: procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryObjectuivARB: procedure(id: TGLuint; pname: TGLenum; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_shader_objects (ARB #30)
   glDeleteObjectARB: procedure(obj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetHandleARB: function(pname: GLenum): GLhandleARB; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDetachObjectARB: procedure(containerObj: GLhandleARB; attachedObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCreateShaderObjectARB: function(shaderType: GLenum): GLhandleARB; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glShaderSourceARB: procedure(shaderObj: GLhandleARB; count: GLsizei; const _string: PGLPCharArray; const length: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCompileShaderARB: procedure(shaderObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCreateProgramObjectARB: function(): GLhandleARB; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glAttachObjectARB: procedure(containerObj: GLhandleARB; obj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glLinkProgramARB: procedure(programObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUseProgramObjectARB: procedure(programObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glValidateProgramARB: procedure(programObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1fARB: procedure(location: GLint; v0: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2fARB: procedure(location: GLint; v0: GLfloat; v1: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3fARB: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4fARB: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1iARB: procedure(location: GLint; v0: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2iARB: procedure(location: GLint; v0: GLint; v1: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3iARB: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4iARB: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1fvARB: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2fvARB: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3fvARB: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4fvARB: procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1ivARB: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2ivARB: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3ivARB: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4ivARB: procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix2fvARB: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix3fvARB: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformMatrix4fvARB: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetObjectParameterfvARB: procedure(obj: GLhandleARB; pname: GLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetObjectParameterivARB: procedure(obj: GLhandleARB; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetInfoLogARB: procedure(obj: GLhandleARB; maxLength: GLsizei; length: PGLsizei; infoLog: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetAttachedObjectsARB: procedure(containerObj: GLhandleARB; maxCount: GLsizei; count: PGLsizei; obj: PGLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformLocationARB: function(programObj: GLhandleARB; const name: PGLChar): GLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveUniformARB: procedure(programObj: GLhandleARB; index: GLuint; maxLength: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformfvARB: procedure(programObj: GLhandleARB; location: GLint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformivARB: procedure(programObj: GLhandleARB; location: GLint; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetShaderSourceARB: procedure(obj: GLhandleARB; maxLength: GLsizei; length: PGLsizei; source: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_vertex_shader (ARB #31)
   glBindAttribLocationARB: procedure(programObj: GLhandleARB; index: GLuint; const name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveAttribARB: procedure(programObj: GLhandleARB; index: GLuint; maxLength: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetAttribLocationARB: function(programObj: GLhandleARB; const name: PGLChar): GLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_DrawBuffers (ARB #37)
   glDrawBuffersARB: procedure (n: GLSizei; const bufs: PGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_color_buffer_float (ARB #39)
   glClampColorARB: procedure (target: TGLenum; clamp: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_draw_instanced (ARB #44)
   glDrawArraysInstancedARB: procedure(mode: TGLenum; first: TGLint; count: TGLsizei;
            primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDrawElementsInstancedARB: procedure(mode: TGLenum; count: TGLSizei; _type: TGLenum;
            indices: PGLvoid; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_framebuffer_object (ARB #45)         
   glIsRenderbuffer: function(renderbuffer: TGLuint): TGLBoolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindRenderbuffer: procedure(target: TGLenum; renderbuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteRenderbuffers: procedure(n: TGLsizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenRenderbuffers: procedure(n: TGLSizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glRenderbufferStorage: procedure(target: TGLenum; internalformat: TGLenum;
			      width: TGLsizei;  height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glRenderbufferStorageMultisample: procedure(target: TGLenum; samples: TGLsizei;
					internalformat: TGLenum;
				  width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetRenderbufferParameteriv: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsFramebuffer: function(framebuffer: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindFramebuffer: procedure(target: TGLenum; framebuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteFramebuffers: procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenFramebuffers: procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCheckFramebufferStatus: function(target: TGLenum): TGLenum; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTexture1D: procedure(target: TGLenum; attachment: TGLenum;
			      textarget: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTexture2D: procedure(target: TGLenum; attachment: TGLenum;
			      textarget: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTexture3D: procedure(target: TGLenum; attachment: TGLenum;
			      textarget: TGLenum; texture: TGLuint;
			      level: TGLint; layer: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTextureLayer: procedure(target: TGLenum; attachment: TGLenum;
				 texture: TGLuint; level: TGLint; layer: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferRenderbuffer: procedure(target: TGLenum; attachment: TGLenum;
				 renderbuffertarget: TGLenum; renderbuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFramebufferAttachmentParameteriv: procedure(target: TGLenum; attachment: TGLenum;
					     pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBlitFramebuffer: procedure(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint;
			 dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint;
			 mask: TGLbitfield; filter: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenerateMipmap: procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_geometry_shader4 (ARB #47)
   glProgramParameteriARB: procedure ( _program:TGLuint; pname:TGLenum; value: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTextureARB: procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTextureLayerARB: procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint; layer:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTextureFaceARB: procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint; face:TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_instanced_arrays (ARB #49)
   glVertexAttribDivisorARB: procedure(index: TGLuint; divisor: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_map_buffer_range (ARB #50)
   glMapBufferRange: function(target: TGLenum; offset: TGLint{ptr}; length: TGLsizei{ptr};
	            access: TGLbitfield ): Pointer;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFlushMappedBufferRange: procedure( target: TGLenum; offset: TGLint{ptr}; length: TGLsizei{ptr} );{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_texture_buffer_object (ARB #51)
   glTexBufferARB: procedure(target: TGLenum; internalformat: TGLEnum; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_vertex_array_object (ARB #54)
   glBindVertexArray: procedure(_array: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteVertexArrays: procedure(n: TGLsizei; arrays: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenVertexArrays: procedure(n: TGLsizei; arrays: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsVertexArray: function(_array: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_uniform_buffer_object (ARB #57)
   glGetUniformIndices: procedure(_program: TGLuint; uniformCount: TGLsizei; uniformNames: PGLPCharArray; uniformIndices: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveUniformsiv: procedure(_program: TGLuint; uniformCount: TGLsizei; uniformIndices: PGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveUniformName: procedure(_program: TGLuint; uniformIndex: TGLuint; bufSize: TGLsizei; length: PGLsizei; uniformName: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformBlockIndex: function(_program: TGLuint; uniformBlockName: PGLchar): TGLuint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveUniformBlockiv: procedure(_program: TGLuint; uniformBlockIndex: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveUniformBlockName: procedure(_program: TGLuint; uniformBlockIndex: TGLuint; bufSize: TGLsizei; length: PGLsizei; uniformBlockName: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniformBlockBinding: procedure(_program: TGLuint; uniformBlockIndex: TGLuint; uniformBlockBinding: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_copy_buffer (ARB #59)
   glCopyBufferSubData: procedure(readTarget: TGLenum; writeTarget: TGLenum;
          readOffset: TGLintptr; writeOffset: TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_draw_elements_base_vertex (ARB #62)
   glDrawElementsBaseVertex: procedure(mode: TGLenum; count: TGLsizei;
          _type: TGLenum; indices: PGLvoid; basevertex: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDrawRangeElementsBaseVertex: procedure(mode: TGLenum; start: TGLuint; _end: TGLuint;
          count: TGLsizei; _type: TGLenum; indices: PGLvoid; basevertex: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDrawElementsInstancedBaseVertex: procedure(mode: TGLenum; count: TGLsizei;
          _type: TGLenum; indices: PGLvoid; primcount: TGLsizei; basevertex: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiDrawElementsBaseVertex: procedure(mode: TGLenum; count: PGLsizei;
          _type: TGLenum; var indices; primcount: TGLsizei; basevertex: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_provoking_vertex (ARB #64)
   glProvokingVertex: procedure(mode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_sync (ARB #66)
   glFenceSync: function(condition: TGLenum; flags: TGLbitfield): TGLsync;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsSync: function(sync: TGLsync): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteSync: procedure(sync: TGLsync);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glClientWaitSync: function(sync: TGLsync; flags: TGLbitfield; timeout: TGLuint64): TGLenum;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glWaitSync: procedure(sync: TGLsync; flags: TGLbitfield; timeout: TGLuint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetInteger64v: procedure(pname: TGLenum; params: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetSynciv: procedure(sync: TGLsync; pname: TGLenum; bufSize: TGLsizei; length: PGLsizei; values: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_texture_multisample (ARB #67)
   glTexImage2DMultisample: procedure(target: TGLenum; samples: TGLsizei; internalformat: TGLint;
                               width: TGLsizei; height: TGLsizei;
                               fixedsamplelocations: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexImage3DMultisample: procedure(target: TGLenum; samples: TGLsizei; internalformat: TGLint;
                               width: TGLsizei; height: TGLsizei; depth: TGLsizei;
                               fixedsamplelocations: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetMultisamplefv: procedure(pname: TGLenum; index: TGLuint; val: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSampleMaski: procedure(index: TGLuint; mask: TGLbitfield);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_draw_buffers_blend (ARB #69)
   glBlendEquationiARB: procedure(buf: TGLuint; mode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBlendEquationSeparateiARB: procedure(buf: TGLuint; modeRGB: TGLenum; modeAlpha: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBlendFunciARB: procedure(buf: TGLuint; src: TGLenum; dst: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBlendFuncSeparateiARB: procedure(buf: TGLuint; srcRGB: TGLenum; dstRGB: TGLenum;
                               srcAlpha: TGLenum; dstAlpha: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ARB_sample_shading (ARB #70)
   glMinSampleShadingARB: procedure(value: TGLclampf);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'OpenGL function/procedure definitions for Vendor/EXT extensions'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //                   Vendor/EXT extensions
   //  ###########################################################

   // Unknown Vendor/EXT functions
   glArrayElementArrayEXT: procedure(mode: TGLEnum; count: TGLsizei; pi: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_WIN_swap_hint (extension # not found)
   glAddSwapHintRectWIN: procedure(x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_blend_color (EXT #2)
   glBlendColorEXT: procedure(red, green, blue: TGLclampf; alpha: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_polygon_offset (EXT #3)
   glPolygonOffsetEXT: procedure(factor, bias: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_texture3D (EXT #6)
   glTexImage3DEXT: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width, height, depth: TGLsizei; border: TGLint; Format, AType: TGLenum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_subtexture (EXT #9)
   glTexSubImage1DEXT: procedure(target: TGLEnum; level, xoffset: TGLint; width: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexSubImage2DEXT: procedure(target: TGLEnum; level, xoffset, yoffset: TGLint; width, height: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexSubImage3DEXT: procedure(target: TGLEnum; level, xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_copy_texture (EXT #10)
   glCopyTexImage1DEXT: procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCopyTexImage2DEXT: procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width, height: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCopyTexSubImage1DEXT: procedure(target: TGLEnum; level, xoffset, x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCopyTexSubImage2DEXT: procedure(target: TGLEnum; level, xoffset, yoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCopyTexSubImage3DEXT: procedure(target: TGLEnum; level, xoffset, yoffset, zoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_texture_object (EXT #20)
   glGenTexturesEXT: procedure(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteTexturesEXT: procedure(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindTextureEXT: procedure(target: TGLEnum; texture: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPrioritizeTexturesEXT: procedure(n: TGLsizei; textures: PGLuint; priorities: PGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glAreTexturesResidentEXT: function(n: TGLsizei; textures: PGLuint; residences: PGLBoolean): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsTextureEXT: function(texture: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_SGIS_multisample (EXT #25)
   glSampleMaskSGIS: procedure(Value: TGLclampf; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSamplePatternSGIS: procedure(pattern: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_blend_minmax (EXT #37)
   glBlendEquationEXT: procedure(mode: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_paletted_texture (EXT #78)
   glColorTableEXT: procedure(target, internalFormat: TGLEnum; width: TGLsizei; format, atype: TGLEnum; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glColorSubTableExt: procedure(target: TGLEnum; start, count: TGLsizei; format, atype: TGLEnum; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetColorTableEXT: procedure(target, format, atype: TGLEnum; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetColorTableParameterfvEXT: procedure(target, pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetColorTableParameterivEXT: procedure(target, pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   (* crossbuilder these are probably leftovers from a typo: *)
//   glGetColorTableParameterfvEXT: procedure(target, pname: TGLEnum; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
//   glGetColorTableParameterivEXT: procedure(target, pname: TGLEnum; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_index_material (EXT #94)
   glIndexMaterialEXT: procedure(face: TGLEnum; mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_index_func (EXT #95)
   glIndexFuncEXT: procedure(func: TGLEnum; ref: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_compiled_vertex_array (EXT #97)
   glLockArraysEXT: procedure(first: TGLint; count: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUnlockArraysEXT: procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_draw_range_elements (EXT #112)
   glDrawRangeElementsEXT: procedure(mode: TGLenum; start, Aend: TGLuint; Count: TGLsizei; Atype: TGLenum; indices: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_scene_marker (EXT #120)
   glBeginSceneEXT: procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndSceneEXT: procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_secondary_color (EXT #145)
   glSecondaryColor3bEXT: procedure(red, green, blue: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3bvEXT: procedure(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3dEXT: procedure(red, green, blue: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3dvEXT: procedure(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3fEXT: procedure(red, green, blue: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3fvEXT: procedure(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3iEXT: procedure(red, green, blue: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3ivEXT: procedure(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3sEXT: procedure(red, green, blue: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3svEXT: procedure(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3ubEXT: procedure(red, green, blue: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3ubvEXT: procedure(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3uiEXT: procedure(red, green, blue: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3uivEXT: procedure(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3usEXT: procedure(red, green, blue: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColor3usvEXT: procedure(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSecondaryColorPointerEXT: procedure(Size: TGLint; Atype: TGLenum; stride: TGLsizei; p: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_multi_draw_arrays (EXT #148)
   glMultiDrawArraysEXT: procedure(mode: TGLenum; First: PGLint; Count: PGLsizei; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glMultiDrawElementsEXT: procedure(mode: TGLenum; Count: PGLsizei; AType: TGLenum; var indices; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_fog_coord (EXT #149)
   glFogCoordfEXT: procedure(coord: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFogCoordfvEXT: procedure(coord: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFogCoorddEXT: procedure(coord: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFogCoorddvEXT: procedure(coord: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFogCoordPointerEXT: procedure(AType: TGLenum; stride: TGLsizei; p: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_blend_func_separate (EXT #173)
   glBlendFuncSeparateEXT: procedure(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_vertex_array_range (EXT #190)
   glFlushVertexArrayRangeNV: procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexArrayRangeNV: procedure(Size: TGLsizei; p: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   wglAllocateMemoryNV: function(size: TGLsizei; readFrequency, writeFrequency, priority: Single): Pointer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   wglFreeMemoryNV: procedure(ptr: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_register_combiners (EXT #191)
   glCombinerParameterfvNV: procedure(pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCombinerParameterfNV: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCombinerParameterivNV: procedure(pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCombinerParameteriNV: procedure(pname: TGLenum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCombinerInputNV: procedure(stage, portion, variable, input, mapping, componentUsage: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCombinerOutputNV: procedure(stage, portion, abOutput, cdOutput, sumOutput, scale, bias: TGLenum; abDotProduct, cdDotProduct, muxSum: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFinalCombinerInputNV: procedure(variable, input, mapping, componentUsage: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetCombinerInputParameterfvNV: procedure(stage, portion, variable, pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetCombinerInputParameterivNV: procedure(stage, portion, variable, pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetCombinerOutputParameterfvNV: procedure(stage, portion, pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetCombinerOutputParameterivNV: procedure(stage, portion, pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFinalCombinerInputParameterfvNV: procedure(variable, pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFinalCombinerInputParameterivNV: procedure(variable, pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_MESA_resize_buffers (EXT #196)
   glResizeBuffersMESA: procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_3DFX_tbuffer (EXT #208)
   glTbufferMask3DFX: procedure(mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_multisample (EXT #209)
   glSampleMaskEXT: procedure(Value: TGLclampf; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSamplePatternEXT: procedure(pattern: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_SGIS_texture_color_mask (EXT #214)
   glTextureColorMaskSGIS: procedure(red, green, blue, alpha: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_fence (EXT #222)
   glGenFencesNV: procedure(n: TGLsizei; fences: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteFencesNV: procedure(n: TGLsizei; fences: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glSetFenceNV: procedure(fence: TGLuint; condition: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTestFenceNV: function(fence: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFinishFenceNV: procedure(fence: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsFenceNV: function(fence: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFenceivNV: procedure(fence: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_vertex_program (EXT #233)
   glAreProgramsResidentNV: procedure(n: TGLSizei; programs: PGLuint; residences: PGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindProgramNV: procedure(target: TGLenum; id: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteProgramsNV: procedure(n: TGLSizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glExecuteProgramNV: procedure(target: TGLenum; id: TGLuint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenProgramsNV: procedure(n: TGLSizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramParameterdvNV: procedure (target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramParameterfvNV: procedure (target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramivNV: procedure (id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetProgramStringNV: procedure (id: TGLuint; pname: TGLenum; programIdx: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTrackMatrixivNV: procedure (target: TGLenum; address: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribdvNV: procedure (index: TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribfvNV: procedure (index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribivNV: procedure (index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribPointervNV: procedure (index: TGLuint; pname: TGLenum; pointer: PGLPointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsProgramNV: function (id: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glLoadProgramNV: procedure (target: TGLenum; id: TGLuint; len: TGLSizei; programIdx: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramParameter4dNV: procedure (target: TGLenum; index: TGLuint; x, y, z, w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramParameter4dvNV: procedure (target: TGLenum; index: TGLuint; v: PGLdouble ); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramParameter4fNV: procedure (target: TGLenum; index: TGLuint; x, y, z, w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramParameter4fvNV: procedure (target: TGLenum; index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramParameters4dvNV: procedure (target: TGLenum; index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramParameters4fvNV: procedure (target: TGLenum; index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glRequestResidentProgramsNV: procedure (n: TGLSizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTrackMatrixNV: procedure (target: TGLenum; address: TGLuint; matrix: TGLenum; transform: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribPointerNV: procedure (index: TGLuint; fsize: TGLint; vertextype: TGLenum; stride: TGLSizei; pointer: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1dNV: procedure (index: TGLuint; x: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1dvNV: procedure (index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1fNV: procedure (index: TGLuint; x: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1fvNV: procedure (index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1sNV: procedure (index: TGLuint; x: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib1svNV: procedure (index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2dNV: procedure (index: TGLuint; x: TGLdouble; y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2dvNV: procedure (index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2fNV: procedure (index: TGLuint; x: TGLfloat; y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2fvNV: procedure (index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2sNV: procedure (index: TGLuint; x: TGLshort; y: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib2svNV: procedure (index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3dNV: procedure (index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3dvNV: procedure (index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3fNV: procedure (index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3fvNV: procedure (index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3sNV: procedure (index: TGLuint; x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib3svNV: procedure (index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4dNV: procedure (index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4dvNV: procedure (index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4fNV: procedure(index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4fvNV: procedure(index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4sNV: procedure (index: TGLuint; x: TGLshort; y: TGLshort; z: TGLdouble; w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4svNV: procedure (index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttrib4ubvNV: procedure (index: TGLuint; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs1dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs1fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs1svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs2dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs2fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs2svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs3dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs3fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs3svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs4dvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs4fvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs4svNV: procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribs4ubvNV: procedure (index: TGLuint; count: TGLSizei; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_occlusion_query (EXT #261)
   glGenOcclusionQueriesNV: procedure(n: TGLsizei; ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteOcclusionQueriesNV: procedure(n: TGLsizei; const ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsOcclusionQueryNV: function(id: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBeginOcclusionQueryNV: procedure(id: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndOcclusionQueryNV: procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetOcclusionQueryivNV: procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetOcclusionQueryuivNV: procedure(id: TGLuint; pname: TGLenum; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_point_sprite (EXT #262)
   glPointParameteriNV: procedure(pname: TGLenum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPointParameterivNV: procedure(pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_stencil_two_side (EXT #268)
   glActiveStencilFaceEXT: procedure(face: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_ATI_draw_buffers (EXT #277)
   glDrawBuffersATI: procedure(n: GLsizei; const bufs: PGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_primitive_restart (EXT #285)
   glPrimitiveRestartNV: procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glPrimitiveRestartIndexNV: procedure(index: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_depth_bounds_test (EXT #297)
   glDepthBoundsEXT: procedure(zmin: TGLclampd; zmax: TGLclampd);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_blend_equation_separate (EXT #299)
   glBlendEquationSeparateEXT: procedure(modeRGB: TGLenum; modeAlpha: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_framebuffer_object (EXT #310)
   glIsRenderbufferEXT: function(renderbuffer: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindRenderbufferEXT: procedure(target: TGLenum; renderbuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteRenderbuffersEXT: procedure(n: TGLsizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenRenderbuffersEXT: procedure(n: TGLsizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glRenderbufferStorageEXT: procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetRenderbufferParameterivEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsFramebufferEXT: function(framebuffer: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindFramebufferEXT: procedure(target: TGLenum; framebuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDeleteFramebuffersEXT: procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenFramebuffersEXT: procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glCheckFramebufferStatusEXT: function(target: TGLenum): TGLenum; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTexture1DEXT: procedure(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTexture2DEXT: procedure(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTexture3DEXT: procedure(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint; zoffset: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferRenderbufferEXT: procedure(target: TGLenum; attachment: TGLenum; renderbuffertarget: TGLenum; renderbuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFramebufferAttachmentParameterivEXT: procedure(target: TGLenum; attachment: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGenerateMipmapEXT: procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_stencil_clear_tag (EXT #314)
   glStencilClearTagEXT: procedure(stencilTagBits: TGLsizei; stencilClearTag: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_framebuffer_blit (#316)
   glBlitFramebufferEXT: procedure(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint;
                            dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint;
                            mask: TGLbitfield; filter: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_framebuffer_multisample (#317)
   glRenderbufferStorageMultisampleEXT: procedure(target: TGLenum; samples: TGLsizei;
            internalformat: TGLenum; width: TGLsizei; height: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_timer_query (#319)
   glGetQueryObjecti64vEXT: procedure(id: TGLuint; pname: TGLenum; params: PGLint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetQueryObjectui64vEXT: procedure(id: TGLuint; pname: TGLenum; params: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_gpu_program_parameters (EXT #320)
   glProgramEnvParameters4fvEXT:   procedure(target:TGLenum; index:TGLuint; count:TGLsizei;
                                     const params:PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glProgramLocalParameters4fvEXT: procedure(target:TGLenum; index:TGLuint; count:TGLsizei;
                                     const params:PGLFloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_geometry_program4 (EXT #323)
   glProgramVertexLimitNV: procedure (target: TGLenum; limit: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_geometry_shader4 (EXT #324)
   glProgramParameteriEXT: procedure ( _program:TGLuint; pname:TGLenum; value: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTextureEXT: procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTextureLayerEXT: procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint; layer:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glFramebufferTextureFaceEXT: procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint; face:TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_gpu_shader4 (EXT #326)
   glVertexAttribI1iEXT: procedure(index: TGLuint; x: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2iEXT: procedure(index: TGLuint; x: TGLint; y: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3iEXT: procedure(index: TGLuint; x: TGLint; y: TGLint; z: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4iEXT: procedure(index: TGLuint; x: TGLint; y: TGLint; z: TGLint; w: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI1uiEXT: procedure(index: TGLuint; x: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2uiEXT: procedure(index: TGLuint; x: TGLuint; y: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3uiEXT: procedure(index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4uiEXT: procedure(index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint; w: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI1ivEXT: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2ivEXT: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3ivEXT: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4ivEXT: procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI1uivEXT: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI2uivEXT: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI3uivEXT: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4uivEXT: procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4bvEXT: procedure(index: TGLuint; v:PGLbyte);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4svEXT: procedure(index: TGLuint; v:PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4ubvEXT: procedure(index: TGLuint; v: PGLUbyte);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribI4usvEXT: procedure(index: TGLuint; v: PGLushort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glVertexAttribIPointerEXT: procedure(index: TGLuint; size: TGLint; _type: TGLenum;
                                stride: TGLsizei; _pointer: pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribIivEXT: procedure(index: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetVertexAttribIuivEXT: procedure(index: TGLuint; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1uiEXT: procedure(location: TGLInt; v0: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2uiEXT: procedure(location: TGLInt; v0: TGLuint; v1: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3uiEXT: procedure(location: TGLInt; v0: TGLuint; v1: TGLuint; v2: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4uiEXT: procedure(location: TGLInt; v0: TGLuint; v1: TGLuint; v2: TGLuint; v3: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform1uivEXT: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform2uivEXT: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform3uivEXT: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glUniform4uivEXT: procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformuivEXT: procedure(_program: TGLuint; location: TGLint; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindFragDataLocationEXT: procedure(_program: TGLuint; colorNumber: TGLuint; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetFragDataLocationEXT: function(_program: TGLuint; name: PGLChar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_draw_instanced (#327)
   glDrawArraysInstancedEXT: procedure(mode: TGLenum; first: TGLint; count: TGLsizei;
            primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDrawElementsInstancedEXT: procedure(mode: TGLenum; count: TGLSizei; _type: TGLenum;
            indices: PGLvoid; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_packed_float (#328)
   // WGL_EXT_pixel_format_packed_float
   // GLX_EXT_fbconfig_packed_float


   // GL_EXT_texture_array (#329)
   //glFramebufferTextureLayerEXT: procedure(target: TGLenum; attachment: TGLenum;
   //                                texture: TGLuint; level: TGLint; layer: TGLint);


   // GL_EXT_texture_buffer_object (#330)
   glTexBufferEXT: procedure(target: TGLenum; internalformat: TGLenum; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_draw_buffers2 (#340)
   glColorMaskIndexedEXT: procedure(buf: TGLuint; r: TGLboolean; g: TGLboolean;
                            b: TGLboolean; a: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetBooleanIndexedvEXT: procedure(value: TGLenum; index: TGLuint; data: PGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetIntegerIndexedvEXT: procedure(value: TGLenum; index: TGLuint; data: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEnableIndexedEXT: procedure(target: TGLenum; index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glDisableIndexedEXT: procedure(target: TGLenum; index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glIsEnabledIndexedEXT: function(target: TGLenum; index: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_transform_feedback (#341)
   glBindBufferRangeNV: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                                  offset: TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindBufferOffsetNV: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                                   offset: TGLintptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindBufferBaseNV: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTransformFeedbackAttribsNV: procedure(count: TGLsizei; attribs: PGLint;
                                           bufferMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTransformFeedbackVaryingsNV: procedure(_program: TGLuint; count: TGLsizei;
                                            locations: PGLint;
                                            bufferMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBeginTransformFeedbackNV: procedure(primitiveMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndTransformFeedbackNV: procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   glGetVaryingLocationNV: function(_program: TGLuint; name: PGLChar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetActiveVaryingNV: procedure(_program: TGLuint; index: TGLuint;
                                   bufSize: TGLsizei; length: PGLsizei; size: PGLsizei;
                                   _type: TGLenum; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glActiveVaryingNV: procedure(_program: TGLuint; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTransformFeedbackVaryingNV: procedure(_program: TGLuint; index: TGLuint;
                                              location: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}


   // GL_EXT_bindable_uniform (#342)
   glUniformBufferEXT: procedure(_program: TGLUint; location: TGLint; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformBufferSizeEXT: function(_program: TGLuint; location: TGLint): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetUniformOffsetEXT: function(_program: TGLuint; location: TGLint): PGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_texture_integer (#343)
   glClearColorIiEXT: procedure(r: TGLint; g: TGLint; b: TGLint; a: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glClearColorIuiEXT: procedure(r: TGLuint; g: TGLuint; b: TGLuint; a: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexParameterIivEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexParameterIuivEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTexParameterIivEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTexParameterIuivEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_conditional_render (#346)
   glBeginConditionalRenderNV: procedure(id: TGLuint; mode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndConditionalRenderNV: procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_transform_feedback (#352)
   glBindBufferRangeEXT: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                            offset:TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindBufferOffsetEXT: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                            offset:TGLintptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBindBufferBaseEXT: procedure(target: TGLenum; index: TGLuint; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glBeginTransformFeedbackEXT: procedure(primitiveMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glEndTransformFeedbackEXT: procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTransformFeedbackVaryingsEXT: procedure(_program: TGLuint; count: TGLsizei;
                                      const varyings: PGLPCharArray; bufferMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glGetTransformFeedbackVaryingEXT: procedure(_program: TGLuint; index: TGLuint;
                                        bufSize: TGLsizei; length: PGLsizei;
                                        size: PGLsizei; _type: PGLenum; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_AMD_vertex_shader_tessellator (#363)
   glTessellationFactorAMD: procedure(factor: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTessellationModeAMD: procedure(mode: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_NV_copy_image (#376)
   glCopyImageSubDataNV: procedure(
     srcName: GLuint; srcTarget: GLenum; srcLevel: GLint;
     srcX: GLint; srcY: GLint; srcZ: GLint;
     dstName: GLuint; dstTarget: GLenum; dstLevel: GLint;
     dstX: GLint; dstY: GLint; dstZ: GLint;
     width: GLsizei; height: GLsizei; depth: GLsizei);  {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}


{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}


//------------------------------------------------------------------------------

procedure ReadExtensions;
procedure ReadImplementationProperties;
{$IFDEF SUPPORT_WGL}
procedure ReadWGLExtensions;
procedure ReadWGLImplementationProperties;
{$ENDIF}
{$IFDEF SUPPORT_GLX}
procedure ReadGLXExtensions;
procedure ReadGLXImplementationProperties;
{$ENDIF}

procedure CloseOpenGL;
function InitOpenGL : Boolean;
function InitOpenGLFromLibrary(const GLName, GLUName : String) : Boolean;
function IsOpenGLInitialized: Boolean;

// compatibility routines
procedure UnloadOpenGL;
function LoadOpenGL : Boolean;
function LoadOpenGLFromLibrary(GLName, GLUName: String): Boolean;
function IsOpenGLLoaded : Boolean;

function IsMesaGL : Boolean;
function IsOpenGLVersionMet(MajorVersion,MinorVersion: Integer): boolean;

type
EOpenGLError = class(Exception);

{: Gets the oldest error from OpenGL engine and tries to clear the error queue.<p> }
procedure CheckOpenGLError;
{: Clears all pending OpenGL errors. }
procedure ClearGLError;
{: Raises an EOpenGLError with 'msg' error string. }
procedure RaiseOpenGLError(const msg : String);

var
   vIgnoreOpenGLErrors : Boolean = false;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ************** Windows specific ********************
{$IFDEF MSWINDOWS}
const
   INVALID_MODULEHANDLE = 0;

var
   GLHandle: HINST;
   GLUHandle: HINST;

function GLGetProcAddress(ProcName: PGLChar):Pointer;
begin
  result := wglGetProcAddress(ProcName);
end;

{$ENDIF}

// ************** UNIX specific ********************
{$IFDEF UNIX}
const
   INVALID_MODULEHANDLE = TLibHandle(0);//nil;

var
   GLHandle: TLibHandle;//Pointer;
   GLUHandle: TLibHandle;//Pointer;
   
function GLGetProcAddress(ProcName: PGLChar):Pointer;
begin
  result := GetProcAddress(TLibHandle(GLHandle),ProcName);
end;
{$ENDIF}

// CheckOpenGLError
//
procedure CheckOpenGLError;
var
   GLError : LongWord;
	Count : Word;
begin
	GLError:=glGetError;
	if GLError <> GL_NO_ERROR then begin
		Count:=0;
      // Because under some circumstances reading the error code creates a new error
      // and thus hanging up the thread, we limit the loop to 6 reads.
      try
         while (glGetError <> GL_NO_ERROR) and (Count < 6) do Inc(Count);
      except
         // Egg : ignore exceptions here, will perhaps avoid problem expressed before
		end;
      if not vIgnoreOpenGLErrors then
   		raise EOpenGLError.Create(String(gluErrorString(GLError)));
	end;
end;

// ClearGLError
//
procedure ClearGLError;
var
   n : Integer;
begin
   n:=0;
   while (glGetError<>GL_NO_ERROR) and (n<6) do Inc(n);
end;

// RaiseOpenGLError
//
procedure RaiseOpenGLError(const msg : String);
begin
   raise EOpenGLError.Create(msg);
end;

// ************** Extensions ********************

// ReadExtensions
//
procedure ReadExtensions;
   // To be used in an active rendering context only!
begin

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 1.2'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 1.2 core
   //  ###########################################################

   // promoted to core v1.2 from GL_EXT_blend_color (#2)
   glBlendColor := GLGetProcAddress('glBlendColor');

   //promoted to core v1.2 from GL_EXT_blend_minmax (#37)
   glBlendEquation := GLGetProcAddress('glBlendEquation');

   // promoted to core v1.2 from GL_EXT_draw_range_elements (#112)
   glDrawRangeElements := GLGetProcAddress('glDrawRangeElements');

   // promoted to core v1.2 from GL_SGI_color_table (#14)
   glColorTable := GLGetProcAddress('glColorTable');
   glColorTableParameterfv := GLGetProcAddress('glColorTableParameterfv');
   glColorTableParameteriv := GLGetProcAddress('glColorTableParameteriv');
   glCopyColorTable := GLGetProcAddress('glCopyColorTable');
   glGetColorTable := GLGetProcAddress('glGetColorTable');
   glGetColorTableParameterfv := GLGetProcAddress('glGetColorTableParameterfv');
   glGetColorTableParameteriv := GLGetProcAddress('glGetColorTableParameteriv');

   // promoted to core v1.2 from GL_EXT_color_subtable (#74)
   glColorSubTable := GLGetProcAddress('glColorSubTable');
   glCopyColorSubTable := GLGetProcAddress('glCopyColorSubTable');

   // promoted to core v1.2 from GL_EXT_convolution (#12)
   glConvolutionFilter1D := GLGetProcAddress('glConvolutionFilter1D');
   glConvolutionFilter2D := GLGetProcAddress('glConvolutionFilter2D'); 
   glConvolutionParameterf := GLGetProcAddress('glConvolutionParameterf');
   glConvolutionParameterfv := GLGetProcAddress('glConvolutionParameterfv');
   glConvolutionParameteri := GLGetProcAddress('glConvolutionParameteri'); 
   glConvolutionParameteriv := GLGetProcAddress('glConvolutionParameteriv');
   glCopyConvolutionFilter1D := GLGetProcAddress('glCopyConvolutionFilter1D');
   glCopyConvolutionFilter2D := GLGetProcAddress('glCopyConvolutionFilter2D');
   glGetConvolutionFilter := GLGetProcAddress('glGetConvolutionFilter');
   glGetConvolutionParameterfv := GLGetProcAddress('glGetConvolutionParameterfv');
   glGetConvolutionParameteriv := GLGetProcAddress('glGetConvolutionParameteriv');
   glGetSeparableFilter := GLGetProcAddress('glGetSeparableFilter');
   glSeparableFilter2D := GLGetProcAddress('glSeparableFilter2D');

   // promoted to core v1.2 from GL_EXT_histogram (#11)
   glGetHistogram := GLGetProcAddress('glGetHistogram');
   glGetHistogramParameterfv := GLGetProcAddress('glGetHistogramParameterfv');
   glGetHistogramParameteriv := GLGetProcAddress('glGetHistogramParameteriv');
   glGetMinmax := GLGetProcAddress('glGetMinmax');
   glGetMinmaxParameterfv := GLGetProcAddress('glGetMinmaxParameterfv');
   glGetMinmaxParameteriv := GLGetProcAddress('glGetMinmaxParameteriv');
   glHistogram := GLGetProcAddress('glHistogram');
   glMinmax := GLGetProcAddress('glMinmax');
   glResetHistogram := GLGetProcAddress('glResetHistogram');
   glResetMinmax := GLGetProcAddress('glResetMinmax');

   // promoted to core v1.2 from GL_EXT_texture3D (#6)
   glTexImage3D := GLGetProcAddress('glTexImage3D');
   glTexSubImage3D := GLGetProcAddress('glTexSubImage3D');

   // promoted to core v1.2 from GL_EXT_copy_texture
   glCopyTexSubImage3D := GLGetProcAddress('glCopyTexSubImage3D');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 1.3'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 1.3 core
   //  ###########################################################

   // promoted to core v1.3 from GL_ARB_multitexture (#1)
   glActiveTexture := GLGetProcAddress('glActiveTexture');
   glClientActiveTexture := GLGetProcAddress('glClientActiveTexture');
   glMultiTexCoord1d := GLGetProcAddress('glMultiTexCoord1d');
   glMultiTexCoord1dV := GLGetProcAddress('glMultiTexCoord1dV');
   glMultiTexCoord1f := GLGetProcAddress('glMultiTexCoord1f');
   glMultiTexCoord1fV := GLGetProcAddress('glMultiTexCoord1fV');
   glMultiTexCoord1i := GLGetProcAddress('glMultiTexCoord1i');
   glMultiTexCoord1iV := GLGetProcAddress('glMultiTexCoord1iV');
   glMultiTexCoord1s := GLGetProcAddress('glMultiTexCoord1s'); 
   glMultiTexCoord1sV := GLGetProcAddress('glMultiTexCoord1sV'); 
   glMultiTexCoord2d := GLGetProcAddress('glMultiTexCoord2d');
   glMultiTexCoord2dv := GLGetProcAddress('glMultiTexCoord2dv');
   glMultiTexCoord2f := GLGetProcAddress('glMultiTexCoord2f');
   glMultiTexCoord2fv := GLGetProcAddress('glMultiTexCoord2fv');
   glMultiTexCoord2i := GLGetProcAddress('glMultiTexCoord2i');
   glMultiTexCoord2iv := GLGetProcAddress('glMultiTexCoord2iv');
   glMultiTexCoord2s := GLGetProcAddress('glMultiTexCoord2s'); 
   glMultiTexCoord2sv := GLGetProcAddress('glMultiTexCoord2sv');
   glMultiTexCoord3d := GLGetProcAddress('glMultiTexCoord3d');
   glMultiTexCoord3dv := GLGetProcAddress('glMultiTexCoord3dv'); 
   glMultiTexCoord3f := GLGetProcAddress('glMultiTexCoord3f');
   glMultiTexCoord3fv := GLGetProcAddress('glMultiTexCoord3fv');
   glMultiTexCoord3i := GLGetProcAddress('glMultiTexCoord3i'); 
   glMultiTexCoord3iv := GLGetProcAddress('glMultiTexCoord3iv'); 
   glMultiTexCoord3s := GLGetProcAddress('glMultiTexCoord3s'); 
   glMultiTexCoord3sv := GLGetProcAddress('glMultiTexCoord3sv');
   glMultiTexCoord4d := GLGetProcAddress('glMultiTexCoord4d'); 
   glMultiTexCoord4dv := GLGetProcAddress('glMultiTexCoord4dv');
   glMultiTexCoord4f := GLGetProcAddress('glMultiTexCoord4f');
   glMultiTexCoord4fv := GLGetProcAddress('glMultiTexCoord4fv');
   glMultiTexCoord4i := GLGetProcAddress('glMultiTexCoord4i');
   glMultiTexCoord4iv := GLGetProcAddress('glMultiTexCoord4iv');
   glMultiTexCoord4s := GLGetProcAddress('glMultiTexCoord4s');
   glMultiTexCoord4sv := GLGetProcAddress('glMultiTexCoord4sv');
   glActiveTexture := GLGetProcAddress('glActiveTexture');

   // promoted to core v1.3 from GL_ARB_transpose_matrix
   glLoadTransposeMatrixf := GLGetProcAddress('glLoadTransposeMatrixf');
   glLoadTransposeMatrixd := GLGetProcAddress('glLoadTransposeMatrixd');
   glMultTransposeMatrixf := GLGetProcAddress('glMultTransposeMatrixf');
   glMultTransposeMatrixd := GLGetProcAddress('glMultTransposeMatrixd');

   // promoted to core v1.3 from GL_ARB_multisample (#5)
   glSampleCoverage := GLGetProcAddress('glSampleCoverage');

   // promoted to core v1.3 from GL_ARB_texture_compression (#12)
   glCompressedTexImage3D := GLGetProcAddress('glCompressedTexImage3D');
   glCompressedTexImage2D := GLGetProcAddress('glCompressedTexImage2D');
   glCompressedTexImage1D := GLGetProcAddress('glCompressedTexImage1D');
   glCompressedTexSubImage3D := GLGetProcAddress('glCompressedTexSubImage3D');
   glCompressedTexSubImage2D := GLGetProcAddress('glCompressedTexSubImage2D');
   glCompressedTexSubImage1D := GLGetProcAddress('glCompressedTexSubImage1D');
   glGetCompressedTexImage := GLGetProcAddress('glGetCompressedTexImage');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 1.4'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 1.4 core
   //  ###########################################################

   // promoted to core v1.4 from GL_EXT_blend_func_separate (#173)
   glBlendFuncSeparate := GLGetProcAddress('glBlendFuncSeparate');

   // promoted to core v1.4 from GL_EXT_fog_coord (#149)
   glFogCoordf := GLGetProcAddress('glFogCoordf');
   glFogCoordfv := GLGetProcAddress('glFogCoordfv');
   glFogCoordd := GLGetProcAddress('glFogCoordd');
   glFogCoorddv := GLGetProcAddress('glFogCoorddv');
   glFogCoordPointer := GLGetProcAddress('glFogCoordPointer');

   // promoted to core v1.4 from GL_EXT_multi_draw_arrays (#148)
   glMultiDrawArrays := GLGetProcAddress('glMultiDrawArrays');
   glMultiDrawElements := GLGetProcAddress('glMultiDrawElements');

   // promoted to core v1.4 from GL_ARB_point_parameters (#14), GL_NV_point_sprite (#262)
   glPointParameterf := GLGetProcAddress('glPointParameterf');
   glPointParameterfv := GLGetProcAddress('glPointParameterfv');
   glPointParameteri := GLGetProcAddress('glPointParameteri');
   glPointParameteriv := GLGetProcAddress('glPointParameteriv');

   // promoted to core v1.4 from GL_EXT_secondary_color (#145)
   glSecondaryColor3b := GLGetProcAddress('glSecondaryColor3b');
   glSecondaryColor3bv := GLGetProcAddress('glSecondaryColor3bv');
   glSecondaryColor3d := GLGetProcAddress('glSecondaryColor3d');
   glSecondaryColor3dv := GLGetProcAddress('glSecondaryColor3dv');
   glSecondaryColor3f := GLGetProcAddress('glSecondaryColor3f');
   glSecondaryColor3fv := GLGetProcAddress('glSecondaryColor3fv');
   glSecondaryColor3i := GLGetProcAddress('glSecondaryColor3i');
   glSecondaryColor3iv := GLGetProcAddress('glSecondaryColor3iv');
   glSecondaryColor3s := GLGetProcAddress('glSecondaryColor3s');
   glSecondaryColor3sv := GLGetProcAddress('glSecondaryColor3sv');
   glSecondaryColor3ub := GLGetProcAddress('glSecondaryColor3ub');
   glSecondaryColor3ubv := GLGetProcAddress('glSecondaryColor3ubv');
   glSecondaryColor3ui := GLGetProcAddress('glSecondaryColor3ui');
   glSecondaryColor3uiv := GLGetProcAddress('glSecondaryColor3uiv');
   glSecondaryColor3us := GLGetProcAddress('glSecondaryColor3us');
   glSecondaryColor3usv := GLGetProcAddress('glSecondaryColor3usv');
   glSecondaryColorPointer := GLGetProcAddress('glSecondaryColorPointer');

   // promoted to core v1.4 from GL_ARB_window_pos (#25)
   glWindowPos2d := GLGetProcAddress('glWindowPos2d');
   glWindowPos2dv := GLGetProcAddress('glWindowPos2dv');
   glWindowPos2f := GLGetProcAddress('glWindowPos2f');
   glWindowPos2fv := GLGetProcAddress('glWindowPos2fv');
   glWindowPos2i := GLGetProcAddress('glWindowPos2i');
   glWindowPos2iv := GLGetProcAddress('glWindowPos2iv');
   glWindowPos2s := GLGetProcAddress('glWindowPos2s');
   glWindowPos2sv := GLGetProcAddress('glWindowPos2sv');
   glWindowPos3d := GLGetProcAddress('glWindowPos3d');
   glWindowPos3dv := GLGetProcAddress('glWindowPos3dv');
   glWindowPos3f := GLGetProcAddress('glWindowPos3f');
   glWindowPos3fv := GLGetProcAddress('glWindowPos3fv');
   glWindowPos3i := GLGetProcAddress('glWindowPos3i');
   glWindowPos3iv := GLGetProcAddress('glWindowPos3iv');
   glWindowPos3s := GLGetProcAddress('glWindowPos3s');
   glWindowPos3sv := GLGetProcAddress('glWindowPos3sv');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 1.5'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 1.5 core
   //  ###########################################################

   // promoted to core v1.5 from GL_ARB_occlusion_query (#29)
   glGenQueries := GLGetProcAddress('glGenQueries');
   glDeleteQueries := GLGetProcAddress('glDeleteQueries');
   glIsQuery := GLGetProcAddress('glIsQuery');
   glBeginQuery := GLGetProcAddress('glBeginQuery');
   glEndQuery := GLGetProcAddress('glEndQuery');
   glGetQueryiv := GLGetProcAddress('glGetQueryiv');
   glGetQueryObjectiv := GLGetProcAddress('glGetQueryObjectiv');
   glGetQueryObjectuiv := GLGetProcAddress('glGetQueryObjectuiv');


   // promoted to core v1.5 from GL_ARB_vertex_buffer_object (#28)
   glBindBuffer := GLGetProcAddress('glBindBuffer');
   glDeleteBuffers := GLGetProcAddress('glDeleteBuffers');
   glGenBuffers := GLGetProcAddress('glGenBuffers');
   glIsBuffer := GLGetProcAddress('glIsBuffer');
   glBufferData := GLGetProcAddress('glBufferData');
   glBufferSubData := GLGetProcAddress('glBufferSubData');
   glGetBufferSubData := GLGetProcAddress('glGetBufferSubData');
   glMapBuffer := GLGetProcAddress('glMapBuffer');
   glUnmapBuffer := GLGetProcAddress('glUnmapBuffer');
   glGetBufferParameteriv := GLGetProcAddress('glGetBufferParameteriv');
   glGetBufferPointerv := GLGetProcAddress('glGetBufferPointerv');

   // promoted to core v1.5 from GL_EXT_shadow_funcs (#267)
   // (no functions or procedures)

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 2.0'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 2.0 core
   //  ###########################################################

   // promoted to core v2.0 from GL_EXT_blend_equation_separate (#299)
   glBlendEquationSeparate := GLGetProcAddress('glBlendEquationSeparate');

   // promoted to core v2.0 from GL_ARB_draw_buffers (#37)
   glDrawBuffers := GLGetProcAddress('glDrawBuffers');

   // promoted to core v2.0 from GL_ARB_stencil_two_side (no # found)
   glStencilOpSeparate := GLGetProcAddress('glStencilOpSeparate');
   glStencilFuncSeparate := GLGetProcAddress('glStencilFuncSeparate');
   glStencilMaskSeparate := GLGetProcAddress('glStencilMaskSeparate');

   // promoted to core v2.0 from GL_ARB_shader_objects (#30) / GL_ARB_vertex_shader (#31) / GL_ARB_fragment_shader (#32)
   glAttachShader := GLGetProcAddress('glAttachShader');
   glBindAttribLocation := GLGetProcAddress('glBindAttribLocation');
   glCompileShader := GLGetProcAddress('glCompileShader');
   glCreateProgram := GLGetProcAddress('glCreateProgram');
   glCreateShader := GLGetProcAddress('glCreateShader');
   glDeleteProgram := GLGetProcAddress('glDeleteProgram');
   glDeleteShader := GLGetProcAddress('glDeleteShader');
   glDetachShader := GLGetProcAddress('glDetachShader');
   glDisableVertexAttribArray := GLGetProcAddress('glDisableVertexAttribArray');
   glEnableVertexAttribArray := GLGetProcAddress('glEnableVertexAttribArray');
   glGetActiveAttrib := GLGetProcAddress('glGetActiveAttrib');
   glGetActiveUniform := GLGetProcAddress('glGetActiveUniform');
   glGetAttachedShaders := GLGetProcAddress('glGetAttachedShaders');
   glGetAttribLocation := GLGetProcAddress('glGetAttribLocation');
   glGetProgramiv := GLGetProcAddress('glGetProgramiv');
   glGetProgramInfoLog := GLGetProcAddress('glGetProgramInfoLog');
   glGetShaderiv := GLGetProcAddress('glGetShaderiv');
   glGetShaderInfoLog := GLGetProcAddress('glGetShaderInfoLog');
   glGetShaderSource := GLGetProcAddress('glGetShaderSource');
   glGetUniformLocation := GLGetProcAddress('glGetUniformLocation');
   glGetUniformfv := GLGetProcAddress('glGetUniformfv');
   glGetUniformiv := GLGetProcAddress('glGetUniformiv');
   glGetVertexAttribdv := GLGetProcAddress('glGetVertexAttribdv');
   glGetVertexAttribfv := GLGetProcAddress('glGetVertexAttribfv');
   glGetVertexAttribiv := GLGetProcAddress('glGetVertexAttribiv');
   glGetVertexAttribPointerv := GLGetProcAddress('glGetVertexAttribPointerv');
   glIsProgram := GLGetProcAddress('glIsProgram');
   glIsShader := GLGetProcAddress('glIsShader');
   glLinkProgram := GLGetProcAddress('glLinkProgram');
   glShaderSource := GLGetProcAddress('glShaderSource');
   glUseProgram := GLGetProcAddress('glUseProgram');
   glUniform1f := GLGetProcAddress('glUniform1f');
   glUniform2f := GLGetProcAddress('glUniform2f');
   glUniform3f := GLGetProcAddress('glUniform3f');
   glUniform4f := GLGetProcAddress('glUniform4f');
   glUniform1i := GLGetProcAddress('glUniform1i');
   glUniform2i := GLGetProcAddress('glUniform2i');
   glUniform3i := GLGetProcAddress('glUniform3i');
   glUniform4i := GLGetProcAddress('glUniform4i');
   glUniform1fv := GLGetProcAddress('glUniform1fv');
   glUniform2fv := GLGetProcAddress('glUniform2fv');
   glUniform3fv := GLGetProcAddress('glUniform3fv');
   glUniform4fv := GLGetProcAddress('glUniform4fv');
   glUniform1iv := GLGetProcAddress('glUniform1iv');
   glUniform2iv := GLGetProcAddress('glUniform2iv');
   glUniform3iv := GLGetProcAddress('glUniform3iv');
   glUniform4iv := GLGetProcAddress('glUniform4iv');
   glUniformMatrix2fv := GLGetProcAddress('glUniformMatrix2fv');
   glUniformMatrix3fv := GLGetProcAddress('glUniformMatrix3fv');
   glUniformMatrix4fv := GLGetProcAddress('glUniformMatrix4fv');
   glValidateProgram := GLGetProcAddress('glValidateProgram');
   glVertexAttrib1d := GLGetProcAddress('glVertexAttrib1d');
   glVertexAttrib1dv := GLGetProcAddress('glVertexAttrib1dv');
   glVertexAttrib1f := GLGetProcAddress('glVertexAttrib1f');
   glVertexAttrib1fv := GLGetProcAddress('glVertexAttrib1fv');
   glVertexAttrib1s := GLGetProcAddress('glVertexAttrib1s');
   glVertexAttrib1sv := GLGetProcAddress('glVertexAttrib1sv');
   glVertexAttrib2d := GLGetProcAddress('glVertexAttrib2d');
   glVertexAttrib2dv := GLGetProcAddress('glVertexAttrib2dv');
   glVertexAttrib2f := GLGetProcAddress('glVertexAttrib2f');
   glVertexAttrib2fv := GLGetProcAddress('glVertexAttrib2fv');
   glVertexAttrib2s := GLGetProcAddress('glVertexAttrib2s');
   glVertexAttrib2sv := GLGetProcAddress('glVertexAttrib2sv');
   glVertexAttrib3d := GLGetProcAddress('glVertexAttrib3d');
   glVertexAttrib3dv := GLGetProcAddress('glVertexAttrib3dv');
   glVertexAttrib3f := GLGetProcAddress('glVertexAttrib3f');
   glVertexAttrib3fv := GLGetProcAddress('glVertexAttrib3fv');
   glVertexAttrib3s := GLGetProcAddress('glVertexAttrib3s');
   glVertexAttrib3sv := GLGetProcAddress('glVertexAttrib3sv');
   glVertexAttrib4Nbv := GLGetProcAddress('glVertexAttrib4Nbv');
   glVertexAttrib4Niv := GLGetProcAddress('glVertexAttrib4Niv');
   glVertexAttrib4Nsv := GLGetProcAddress('glVertexAttrib4Nsv');
   glVertexAttrib4Nub := GLGetProcAddress('glVertexAttrib4Nub');
   glVertexAttrib4Nubv := GLGetProcAddress('glVertexAttrib4Nubv');
   glVertexAttrib4Nuiv := GLGetProcAddress('glVertexAttrib4Nuiv');
   glVertexAttrib4Nusv := GLGetProcAddress('glVertexAttrib4Nusv');
   glVertexAttrib4bv := GLGetProcAddress('glVertexAttrib4bv');
   glVertexAttrib4d := GLGetProcAddress('glVertexAttrib4d');
   glVertexAttrib4dv := GLGetProcAddress('glVertexAttrib4dv');
   glVertexAttrib4f := GLGetProcAddress('glVertexAttrib4f');
   glVertexAttrib4fv := GLGetProcAddress('glVertexAttrib4fv');
   glVertexAttrib4iv := GLGetProcAddress('glVertexAttrib4iv');
   glVertexAttrib4s := GLGetProcAddress('glVertexAttrib4s');
   glVertexAttrib4sv := GLGetProcAddress('glVertexAttrib4sv');
   glVertexAttrib4ubv := GLGetProcAddress('glVertexAttrib4ubv');
   glVertexAttrib4uiv := GLGetProcAddress('glVertexAttrib4uiv');
   glVertexAttrib4usv := GLGetProcAddress('glVertexAttrib4usv');
   glVertexAttribPointer := GLGetProcAddress('glVertexAttribPointer');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 2.1'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 2.1 core
   //  ###########################################################

   // promoted to core v2.1 from GL_ARB_pixel_buffer_object
   // (no functions or procedures)
   
   // promoted to core v2.1 from GL_EXT_texture_sRGB
   // (no functions or procedures)

   // New commands in OpenGL 2.1
   glUniformMatrix2x3fv := GLGetProcAddress('glUniformMatrix2x3fv');
   glUniformMatrix3x2fv := GLGetProcAddress('glUniformMatrix3x2fv');
   glUniformMatrix2x4fv := GLGetProcAddress('glUniformMatrix2x4fv');
   glUniformMatrix4x2fv := GLGetProcAddress('glUniformMatrix4x2fv');
   glUniformMatrix3x4fv := GLGetProcAddress('glUniformMatrix3x4fv');
   glUniformMatrix4x3fv := GLGetProcAddress('glUniformMatrix4x3fv');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures added with OpenGL 3.0'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 3.0 core
   //  ###########################################################

   // promoted to core v3.0 from GL_EXT_gpu_shader4
   glVertexAttribI1i := GLGetProcAddress('glVertexAttribI1i');
   glVertexAttribI2i := GLGetProcAddress('glVertexAttribI2i');
   glVertexAttribI3i := GLGetProcAddress('glVertexAttribI3i');
   glVertexAttribI4i := GLGetProcAddress('glVertexAttribI4i');
   glVertexAttribI1ui := GLGetProcAddress('glVertexAttribI1ui');
   glVertexAttribI2ui := GLGetProcAddress('glVertexAttribI2ui');
   glVertexAttribI3ui := GLGetProcAddress('glVertexAttribI3ui');
   glVertexAttribI4ui := GLGetProcAddress('glVertexAttribI4ui');
   glVertexAttribI1iv := GLGetProcAddress('glVertexAttribI1iv');
   glVertexAttribI2iv := GLGetProcAddress('glVertexAttribI2iv');
   glVertexAttribI3iv := GLGetProcAddress('glVertexAttribI3iv');
   glVertexAttribI4iv := GLGetProcAddress('glVertexAttribI4iv');
   glVertexAttribI1uiv := GLGetProcAddress('glVertexAttribI1uiv');
   glVertexAttribI2uiv := GLGetProcAddress('glVertexAttribI2uiv');
   glVertexAttribI3uiv := GLGetProcAddress('glVertexAttribI3uiv');
   glVertexAttribI4uiv := GLGetProcAddress('glVertexAttribI4uiv');
   glVertexAttribI4bv := GLGetProcAddress('glVertexAttribI4bv');
   glVertexAttribI4sv := GLGetProcAddress('glVertexAttribI4sv');
   glVertexAttribI4ubv := GLGetProcAddress('glVertexAttribI4ubv');
   glVertexAttribI4usv := GLGetProcAddress('glVertexAttribI4usv');
   glVertexAttribIPointer := GLGetProcAddress('glVertexAttribIPointer');
   glGetVertexAttribIiv := GLGetProcAddress('glGetVertexAttribIiv');
   glGetVertexAttribIuiv := GLGetProcAddress('glGetVertexAttribIuiv');
   glUniform1ui := GLGetProcAddress('glUniform1ui');
   glUniform2ui :=  GLGetProcAddress('glUniform2ui');
   glUniform3ui := GLGetProcAddress('glUniform3ui');
   glUniform4ui := GLGetProcAddress('glUniform4ui');
   glUniform1uiv := GLGetProcAddress('glUniform1uiv');
   glUniform2uiv := GLGetProcAddress('glUniform2uiv');
   glUniform3uiv := GLGetProcAddress('glUniform3uiv');
   glUniform4uiv := GLGetProcAddress('glUniform4uiv');
   glGetUniformuiv := GLGetProcAddress('glGetUniformuiv');
   glBindFragDataLocation := GLGetProcAddress('glBindFragDataLocation');
   glGetFragDataLocation := GLGetProcAddress('glGetFragDataLocation');

   // promoted to core v3.0 from GL_NV_conditional_render
   glBeginConditionalRender := GLGetProcAddress('glBeginConditionalRender');
   glEndConditionalRender := GLGetProcAddress('glEndConditionalRender');
   // promoted to core v3.0 from GL_ARB_color_buffer_float
   glClampColor := GLGetProcAddress('glClampColor');
   // promoted to core v3.0 from GL_EXT_texture_integer
   //glClearColorIi := GLGetProcAddress('glClearColorIi');
   //glClearColorIui := GLGetProcAddress('glClearColorIui');
   glTexParameterIiv := GLGetProcAddress('glTexParameterIiv');
   glTexParameterIuiv := GLGetProcAddress('glTexParameterIuiv');
   glGetTexParameterIiv := GLGetProcAddress('glGetTexParameterIiv');
   glGetTexParameterIuiv := GLGetProcAddress('glGetTexParameterIuiv');

   // promoted to core v3.0 from GL_EXT_draw_buffers2
   glColorMaski := GLGetProcAddress('glColorMaski');
   glGetBooleani_v := GLGetProcAddress('glGetBooleani_v');
   glGetIntegeri_v := GLGetProcAddress('glGetIntegeri_v');
   glEnablei := GLGetProcAddress('glEnablei');
   glDisablei := GLGetProcAddress('glDisablei');
   glIsEnabledi := GLGetProcAddress('glIsEnabledi');

   // GL_EXT_transform_feedback (#352)
   glBindBufferRange := GLGetProcAddress('glBindBufferRange');
   glBindBufferBase := GLGetProcAddress('glBindBufferBase');
   glBeginTransformFeedback := GLGetProcAddress('glBeginTransformFeedback');
   glEndTransformFeedback := GLGetProcAddress('glEndTransformFeedback');
   glTransformFeedbackVaryings := GLGetProcAddress('glTransformFeedbackVaryings');
   glGetTransformFeedbackVarying := GLGetProcAddress('glGetTransformFeedbackVarying');

   // New commands in OpenGL 3.0
   glClearBufferiv := GLGetProcAddress('glClearBufferiv');
   glClearBufferuiv := GLGetProcAddress('glClearBufferuiv');
   glClearBufferfv := GLGetProcAddress('glClearBufferfv');
   glClearBufferfi := GLGetProcAddress('glClearBufferfi');
   glGetStringi := GLGetProcAddress('glGetStringi');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}


{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures for OpenGL Utility (GLU) extensions'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //                     GLU extensions
   //  ###########################################################

   gluNurbsCallbackDataEXT := GLGetProcAddress('gluNurbsCallbackDataEXT');
   gluNewNurbsTessellatorEXT := GLGetProcAddress('gluNewNurbsTessellatorEXT');
   gluDeleteNurbsTessellatorEXT := GLGetProcAddress('gluDeleteNurbsTessellatorEXT');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP} {$region 'locate functions/procedures for ARB approved extensions'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //                  ARB approved extensions
   //  ###########################################################

   // GL_ARB_multitexture (#1)
   glActiveTextureARB := GLGetProcAddress('glActiveTextureARB');
   glClientActiveTextureARB := GLGetProcAddress('glClientActiveTextureARB');
   glMultiTexCoord1dARB := GLGetProcAddress('glMultiTexCoord1dARB');
   glMultiTexCoord1dVARB := GLGetProcAddress('glMultiTexCoord1dVARB');
   glMultiTexCoord1fARB := GLGetProcAddress('glMultiTexCoord1fARB');
   glMultiTexCoord1fVARB := GLGetProcAddress('glMultiTexCoord1fVARB');
   glMultiTexCoord1iARB := GLGetProcAddress('glMultiTexCoord1iARB');
   glMultiTexCoord1iVARB := GLGetProcAddress('glMultiTexCoord1iVARB');
   glMultiTexCoord1sARB := GLGetProcAddress('glMultiTexCoord1sARB');
   glMultiTexCoord1sVARB := GLGetProcAddress('glMultiTexCoord1sVARB');
   glMultiTexCoord2dARB := GLGetProcAddress('glMultiTexCoord2dARB');
   glMultiTexCoord2dvARB := GLGetProcAddress('glMultiTexCoord2dvARB');
   glMultiTexCoord2fARB := GLGetProcAddress('glMultiTexCoord2fARB');
   glMultiTexCoord2fvARB := GLGetProcAddress('glMultiTexCoord2fvARB');
   glMultiTexCoord2iARB := GLGetProcAddress('glMultiTexCoord2iARB');
   glMultiTexCoord2ivARB := GLGetProcAddress('glMultiTexCoord2ivARB');
   glMultiTexCoord2sARB := GLGetProcAddress('glMultiTexCoord2sARB');
   glMultiTexCoord2svARB := GLGetProcAddress('glMultiTexCoord2svARB');
   glMultiTexCoord3dARB := GLGetProcAddress('glMultiTexCoord3dARB');
   glMultiTexCoord3dvARB := GLGetProcAddress('glMultiTexCoord3dvARB');
   glMultiTexCoord3fARB := GLGetProcAddress('glMultiTexCoord3fARB');
   glMultiTexCoord3fvARB := GLGetProcAddress('glMultiTexCoord3fvARB');
   glMultiTexCoord3iARB := GLGetProcAddress('glMultiTexCoord3iARB');
   glMultiTexCoord3ivARB := GLGetProcAddress('glMultiTexCoord3ivARB');
   glMultiTexCoord3sARB := GLGetProcAddress('glMultiTexCoord3sARB');
   glMultiTexCoord3svARB := GLGetProcAddress('glMultiTexCoord3svARB');
   glMultiTexCoord4dARB := GLGetProcAddress('glMultiTexCoord4dARB');
   glMultiTexCoord4dvARB := GLGetProcAddress('glMultiTexCoord4dvARB');
   glMultiTexCoord4fARB := GLGetProcAddress('glMultiTexCoord4fARB');
   glMultiTexCoord4fvARB := GLGetProcAddress('glMultiTexCoord4fvARB');
   glMultiTexCoord4iARB := GLGetProcAddress('glMultiTexCoord4iARB');
   glMultiTexCoord4ivARB := GLGetProcAddress('glMultiTexCoord4ivARB');
   glMultiTexCoord4sARB := GLGetProcAddress('glMultiTexCoord4sARB');
   glMultiTexCoord4svARB := GLGetProcAddress('glMultiTexCoord4svARB');

   // GL_ARB_transpose_matrix (#3)
   glLoadTransposeMatrixfARB := GLGetProcAddress('glLoadTransposeMatrixfARB');
   glLoadTransposeMatrixdARB := GLGetProcAddress('glLoadTransposeMatrixdARB');
   glMultTransposeMatrixfARB := GLGetProcAddress('glMultTransposeMatrixfARB');
   glMultTransposeMatrixdARB := GLGetProcAddress('glMultTransposeMatrixdARB');

   // GL_ARB_multisample (#5)
   glSampleCoverageARB := GLGetProcAddress('glSampleCoverageARB');

   // GL_ARB_texture_compression (#12)
   glCompressedTexImage3DARB := GLGetProcAddress('glCompressedTexImage3DARB');
   glCompressedTexImage2DARB := GLGetProcAddress('glCompressedTexImage2DARB');
   glCompressedTexImage1DARB := GLGetProcAddress('glCompressedTexImage1DARB');
   glCompressedTexSubImage3DARB := GLGetProcAddress('glCompressedTexSubImage3DARB');
   glCompressedTexSubImage2DARB := GLGetProcAddress('glCompressedTexSubImage2DARB');
   glCompressedTexSubImage1DARB := GLGetProcAddress('glCompressedTexSubImage1DARB');
   glGetCompressedTexImageARB := GLGetProcAddress('glGetCompressedTexImageARB');

   // GL_ARB_point_parameter (#14)
   glPointParameterfARB := GLGetProcAddress('glPointParameterfARB');
   glPointParameterfvARB := GLGetProcAddress('glPointParameterfvARB');

   // GL_ARB_vertex_blend (#15) {deprecated?}
   glWeightbvARB := GLGetProcAddress('glWeightbvARB');
   glWeightsvARB := GLGetProcAddress('glWeightsvARB');
   glWeightivARB := GLGetProcAddress('glWeightivARB');
   glWeightfvARB := GLGetProcAddress('glWeightfvARB');
   glWeightdvARB := GLGetProcAddress('glWeightdvARB');
   glWeightubvARB := GLGetProcAddress('glWeightubvARB');
   glWeightusvARB := GLGetProcAddress('glWeightusvARB');
   glWeightuivARB := GLGetProcAddress('glWeightuivARB');
   glWeightPointerARB := GLGetProcAddress('glWeightPointerARB');
   glVertexBlendARB := GLGetProcAddress('glVertexBlendARB');

   // GL_ARB_matrix_palette (#16) {deprecated?}
   glCurrentPaletteMatrixARB := GLGetProcAddress('glCurrentPaletteMatrixARB');
   glMatrixIndexubvARB := GLGetProcAddress('glMatrixIndexubvARB');
   glMatrixIndexusvARB := GLGetProcAddress('glMatrixIndexusvARB');
   glMatrixIndexuivARB := GLGetProcAddress('glMatrixIndexuivARB');
   glMatrixIndexPointerARB := GLGetProcAddress('glMatrixIndexPointerARB');

   // GL_ARB_window_pos (#25)
   glWindowPos2dARB := GLGetProcAddress('glWindowPos2dARB');
   glWindowPos2dvARB := GLGetProcAddress('glWindowPos2dvARB');
   glWindowPos2fARB := GLGetProcAddress('glWindowPos2fARB');
   glWindowPos2fvARB := GLGetProcAddress('glWindowPos2fvARB');
   glWindowPos2iARB := GLGetProcAddress('glWindowPos2iARB');
   glWindowPos2ivARB := GLGetProcAddress('glWindowPos2ivARB');
   glWindowPos2sARB := GLGetProcAddress('glWindowPos2sARB');
   glWindowPos2svARB := GLGetProcAddress('glWindowPos2svARB');
   glWindowPos3dARB := GLGetProcAddress('glWindowPos3dARB');
   glWindowPos3dvARB := GLGetProcAddress('glWindowPos3dvARB');
   glWindowPos3fARB := GLGetProcAddress('glWindowPos3fARB');
   glWindowPos3fvARB := GLGetProcAddress('glWindowPos3fvARB');
   glWindowPos3iARB := GLGetProcAddress('glWindowPos3iARB');
   glWindowPos3ivARB := GLGetProcAddress('glWindowPos3ivARB');
   glWindowPos3sARB := GLGetProcAddress('glWindowPos3sARB');
   glWindowPos3svARB := GLGetProcAddress('glWindowPos3svARB');

   // GL_ARB_vertex_program (#26)
   glVertexAttrib1dARB := GLGetProcAddress('glVertexAttrib1dARB');
   glVertexAttrib1dvARB := GLGetProcAddress('glVertexAttrib1dvARB');
   glVertexAttrib1fARB := GLGetProcAddress('glVertexAttrib1fARB');
   glVertexAttrib1fvARB := GLGetProcAddress('glVertexAttrib1fvARB');
   glVertexAttrib1sARB := GLGetProcAddress('glVertexAttrib1sARB');
   glVertexAttrib1svARB := GLGetProcAddress('glVertexAttrib1svARB');
   glVertexAttrib2dARB := GLGetProcAddress('glVertexAttrib2dARB');
   glVertexAttrib2dvARB := GLGetProcAddress('glVertexAttrib2dvARB');
   glVertexAttrib2fARB := GLGetProcAddress('glVertexAttrib2fARB');
   glVertexAttrib2fvARB := GLGetProcAddress('glVertexAttrib2fvARB');
   glVertexAttrib2sARB := GLGetProcAddress('glVertexAttrib2sARB');
   glVertexAttrib2svARB := GLGetProcAddress('glVertexAttrib2svARB');
   glVertexAttrib3dARB := GLGetProcAddress('glVertexAttrib3dARB');
   glVertexAttrib3dvARB := GLGetProcAddress('glVertexAttrib3dvARB');
   glVertexAttrib3fARB := GLGetProcAddress('glVertexAttrib3fARB');
   glVertexAttrib3fvARB := GLGetProcAddress('glVertexAttrib3fvARB');
   glVertexAttrib3sARB := GLGetProcAddress('glVertexAttrib3sARB');
   glVertexAttrib3svARB := GLGetProcAddress('glVertexAttrib3svARB');
   glVertexAttrib4NbvARB := GLGetProcAddress('glVertexAttrib4NbvARB');
   glVertexAttrib4NivARB := GLGetProcAddress('glVertexAttrib4NivARB');
   glVertexAttrib4NsvARB := GLGetProcAddress('glVertexAttrib4NsvARB');
   glVertexAttrib4NubARB := GLGetProcAddress('glVertexAttrib4NubARB');
   glVertexAttrib4NubvARB := GLGetProcAddress('glVertexAttrib4NubvARB');
   glVertexAttrib4NuivARB := GLGetProcAddress('glVertexAttrib4NuivARB');
   glVertexAttrib4NusvARB := GLGetProcAddress('glVertexAttrib4NusvARB');
   glVertexAttrib4bvARB := GLGetProcAddress('glVertexAttrib4bvARB');
   glVertexAttrib4dARB := GLGetProcAddress('glVertexAttrib4dARB');
   glVertexAttrib4dvARB := GLGetProcAddress('glVertexAttrib4dvARB');
   glVertexAttrib4fARB := GLGetProcAddress('glVertexAttrib4fARB');
   glVertexAttrib4fvARB := GLGetProcAddress('glVertexAttrib4fvARB');
   glVertexAttrib4ivARB := GLGetProcAddress('glVertexAttrib4ivARB');
   glVertexAttrib4sARB := GLGetProcAddress('glVertexAttrib4sARB');
   glVertexAttrib4svARB := GLGetProcAddress('glVertexAttrib4svARB');
   glVertexAttrib4ubvARB := GLGetProcAddress('glVertexAttrib4ubvARB');
   glVertexAttrib4uivARB := GLGetProcAddress('glVertexAttrib4uivARB');
   glVertexAttrib4usvARB := GLGetProcAddress('glVertexAttrib4usvARB');
   glVertexAttribPointerARB := GLGetProcAddress('glVertexAttribPointerARB');
   glEnableVertexAttribArrayARB := GLGetProcAddress('glEnableVertexAttribArrayARB');
   glDisableVertexAttribArrayARB := GLGetProcAddress('glDisableVertexAttribArrayARB');
   glProgramStringARB := GLGetProcAddress('glProgramStringARB');
   glBindProgramARB := GLGetProcAddress('glBindProgramARB');
   glDeleteProgramsARB := GLGetProcAddress('glDeleteProgramsARB');
   glGenProgramsARB := GLGetProcAddress('glGenProgramsARB');
   glProgramEnvParameter4dARB := GLGetProcAddress('glProgramEnvParameter4dARB');
   glProgramEnvParameter4dvARB := GLGetProcAddress('glProgramEnvParameter4dvARB');
   glProgramEnvParameter4fARB := GLGetProcAddress('glProgramEnvParameter4fARB');
   glProgramEnvParameter4fvARB := GLGetProcAddress('glProgramEnvParameter4fvARB');
   glProgramLocalParameter4dARB := GLGetProcAddress('glProgramLocalParameter4dARB');
   glProgramLocalParameter4dvARB := GLGetProcAddress('glProgramLocalParameter4dvARB');
   glProgramLocalParameter4fARB := GLGetProcAddress('glProgramLocalParameter4fARB');
   glProgramLocalParameter4fvARB := GLGetProcAddress('glProgramLocalParameter4fvARB');
   glGetProgramEnvParameterdvARB := GLGetProcAddress('glGetProgramEnvParameterdvARB');
   glGetProgramEnvParameterfvARB := GLGetProcAddress('glGetProgramEnvParameterfvARB');
   glGetProgramLocalParameterdvARB := GLGetProcAddress('glGetProgramLocalParameterdvARB');
   glGetProgramLocalParameterfvARB := GLGetProcAddress('glGetProgramLocalParameterfvARB');
   glGetProgramivARB := GLGetProcAddress('glGetProgramivARB');
   glGetProgramStringARB := GLGetProcAddress('glGetProgramStringARB');
   glGetVertexAttribdvARB := GLGetProcAddress('glGetVertexAttribdvARB');
   glGetVertexAttribfvARB := GLGetProcAddress('glGetVertexAttribfvARB');
   glGetVertexAttribivARB := GLGetProcAddress('glGetVertexAttribivARB');
   glGetVertexAttribPointervARB := GLGetProcAddress('glGetVertexAttribPointervARB');
   glIsProgramARB := GLGetProcAddress('glIsProgramARB');

   // GL_ARB_vertex_buffer_object (#28)
   glBindBufferARB := GLGetProcAddress('glBindBufferARB');
   glDeleteBuffersARB := GLGetProcAddress('glDeleteBuffersARB');
   glGenBuffersARB := GLGetProcAddress('glGenBuffersARB');
   glIsBufferARB := GLGetProcAddress('glIsBufferARB');
   glBufferDataARB := GLGetProcAddress('glBufferDataARB');
   glBufferSubDataARB := GLGetProcAddress('glBufferSubDataARB');
   glGetBufferSubDataARB := GLGetProcAddress('glGetBufferSubDataARB');
   glMapBufferARB := GLGetProcAddress('glMapBufferARB');
   glUnmapBufferARB := GLGetProcAddress('glUnmapBufferARB');
   glGetBufferParameterivARB := GLGetProcAddress('glGetBufferParameterivARB');
   glGetBufferPointervARB := GLGetProcAddress('glGetBufferPointervARB');

   // GL_ARB_occlusion_query (#29)
   glGenQueriesARB := GLGetProcAddress('glGenQueriesARB');
   glDeleteQueriesARB := GLGetProcAddress('glDeleteQueriesARB');
   glIsQueryARB := GLGetProcAddress('glIsQueryARB');
   glBeginQueryARB := GLGetProcAddress('glBeginQueryARB');
   glEndQueryARB := GLGetProcAddress('glEndQueryARB');
   glGetQueryivARB := GLGetProcAddress('glGetQueryivARB');
   glGetQueryObjectivARB := GLGetProcAddress('glGetQueryObjectivARB');
   glGetQueryObjectuivARB := GLGetProcAddress('glGetQueryObjectuivARB');

   // GL_ARB_shader_objects (#30)
   glDeleteObjectARB := GLGetProcAddress('glDeleteObjectARB');
   glGetHandleARB := GLGetProcAddress('glGetHandleARB');
   glDetachObjectARB := GLGetProcAddress('glDetachObjectARB');
   glCreateShaderObjectARB := GLGetProcAddress('glCreateShaderObjectARB');
   glShaderSourceARB := GLGetProcAddress('glShaderSourceARB');
   glCompileShaderARB := GLGetProcAddress('glCompileShaderARB');
   glCreateProgramObjectARB := GLGetProcAddress('glCreateProgramObjectARB');
   glAttachObjectARB := GLGetProcAddress('glAttachObjectARB');
   glLinkProgramARB := GLGetProcAddress('glLinkProgramARB');
   glUseProgramObjectARB := GLGetProcAddress('glUseProgramObjectARB');
   glValidateProgramARB := GLGetProcAddress('glValidateProgramARB');
   glUniform1fARB := GLGetProcAddress('glUniform1fARB');
   glUniform2fARB := GLGetProcAddress('glUniform2fARB');
   glUniform3fARB := GLGetProcAddress('glUniform3fARB');
   glUniform4fARB := GLGetProcAddress('glUniform4fARB');
   glUniform1iARB := GLGetProcAddress('glUniform1iARB');
   glUniform2iARB := GLGetProcAddress('glUniform2iARB');
   glUniform3iARB := GLGetProcAddress('glUniform3iARB');
   glUniform4iARB := GLGetProcAddress('glUniform4iARB');
   glUniform1fvARB := GLGetProcAddress('glUniform1fvARB');
   glUniform2fvARB := GLGetProcAddress('glUniform2fvARB');
   glUniform3fvARB := GLGetProcAddress('glUniform3fvARB');
   glUniform4fvARB := GLGetProcAddress('glUniform4fvARB');
   glUniform1ivARB := GLGetProcAddress('glUniform1ivARB');
   glUniform2ivARB := GLGetProcAddress('glUniform2ivARB');
   glUniform3ivARB := GLGetProcAddress('glUniform3ivARB');
   glUniform4ivARB := GLGetProcAddress('glUniform4ivARB');
   glUniformMatrix2fvARB := GLGetProcAddress('glUniformMatrix2fvARB');
   glUniformMatrix3fvARB := GLGetProcAddress('glUniformMatrix3fvARB');
   glUniformMatrix4fvARB := GLGetProcAddress('glUniformMatrix4fvARB');
   glGetObjectParameterfvARB := GLGetProcAddress('glGetObjectParameterfvARB');
   glGetObjectParameterivARB := GLGetProcAddress('glGetObjectParameterivARB');
   glGetInfoLogARB := GLGetProcAddress('glGetInfoLogARB');
   glGetAttachedObjectsARB := GLGetProcAddress('glGetAttachedObjectsARB');
   glGetUniformLocationARB := GLGetProcAddress('glGetUniformLocationARB');
   glGetActiveUniformARB := GLGetProcAddress('glGetActiveUniformARB');
   glGetUniformfvARB := GLGetProcAddress('glGetUniformfvARB');
   glGetUniformivARB := GLGetProcAddress('glGetUniformivARB');
   glGetShaderSourceARB := GLGetProcAddress('glGetShaderSourceARB');

   // GL_ARB_vertex_shader (#31)
   glBindAttribLocationARB := GLGetProcAddress('glBindAttribLocationARB');
   glGetActiveAttribARB := GLGetProcAddress('glGetActiveAttribARB');
   glGetAttribLocationARB := GLGetProcAddress('glGetAttribLocationARB');

   // GL_ARB_draw_buffers (#37)
   glDrawBuffersARB := GLGetProcAddress('glDrawBuffersARB');

   // GL_ARB_color_buffer_float (#39)
   glClampColorARB := GLGetProcAddress('glClampColorARB');

   // GL_ARB_draw_instanced (ARB #44)
   glDrawArraysInstancedARB := GLGetProcAddress('glDrawArraysInstancedARB');
   glDrawElementsInstancedARB := GLGetProcAddress('glDrawElementsInstancedARB');

   // GL_ARB_framebuffer_object (ARB #45)
   glIsRenderbuffer := GLGetProcAddress('glIsRenderbuffer');
   glBindRenderbuffer := GLGetProcAddress('glBindRenderbuffer');
   glDeleteRenderbuffers := GLGetProcAddress('glDeleteRenderbuffers');
   glGenRenderbuffers := GLGetProcAddress('glGenRenderbuffers');
   glRenderbufferStorage := GLGetProcAddress('glRenderbufferStorage');
   glRenderbufferStorageMultisample := GLGetProcAddress('glRenderbufferStorageMultisample');
   glGetRenderbufferParameteriv := GLGetProcAddress('glGetRenderbufferParameteriv');
   glIsFramebuffer := GLGetProcAddress('glIsFramebuffer');
   glBindFramebuffer := GLGetProcAddress('glBindFramebuffer');
   glDeleteFramebuffers := GLGetProcAddress('glDeleteFramebuffers');
   glGenFramebuffers := GLGetProcAddress('glGenFramebuffers');
   glCheckFramebufferStatus := GLGetProcAddress('glCheckFramebufferStatus');
   glFramebufferTexture1D := GLGetProcAddress('glFramebufferTexture1D');
   glFramebufferTexture2D := GLGetProcAddress('glFramebufferTexture2D');
   glFramebufferTexture3D := GLGetProcAddress('glFramebufferTexture3D');
   glFramebufferTextureLayer := GLGetProcAddress('glFramebufferTextureLayer');
   glFramebufferRenderbuffer := GLGetProcAddress('glFramebufferRenderbuffer');
   glGetFramebufferAttachmentParameteriv := GLGetProcAddress('glGetFramebufferAttachmentParameteriv');
   glBlitFramebuffer := GLGetProcAddress('glBlitFramebuffer');
   glGenerateMipmap := GLGetProcAddress('glGenerateMipmap');

   // GL_ARB_geometry_shader4 (ARB #47)
   glProgramParameteriARB := GLGetProcAddress('glProgramParameteriARB');
   glFramebufferTextureARB := GLGetProcAddress('glFramebufferTextureARB');
   glFramebufferTextureLayerARB := GLGetProcAddress('glFramebufferTextureLayerARB');
   glFramebufferTextureFaceARB := GLGetProcAddress('glFramebufferTextureFaceARB');

   // GL_ARB_instanced_arrays (ARB #49)
   glVertexAttribDivisorARB := GLGetProcAddress('glVertexAttribDivisorARB');

   // GL_ARB_map_buffer_range (ARB #50)
   glMapBufferRange := GLGetProcAddress('glMapBufferRange');
   glFlushMappedBufferRange := GLGetProcAddress('glFlushMappedBufferRange');

   // GL_ARB_texture_buffer_object (ARB #51)
   glTexBufferARB := GLGetProcAddress('glTexBufferARB');

   // GL_ARB_vertex_array_object (ARB #54)
   glBindVertexArray := GLGetProcAddress('glBindVertexArray');
   glDeleteVertexArrays := GLGetProcAddress('glDeleteVertexArrays');
   glGenVertexArrays := GLGetProcAddress('glGenVertexArrays');
   glIsVertexArray := GLGetProcAddress('glIsVertexArray');

   // GL_ARB_uniform_buffer_object (ARB #57)
   glGetUniformIndices := GLGetProcAddress('glGetUniformIndices');
   glGetActiveUniformsiv := GLGetProcAddress('glGetActiveUniformsiv');
   glGetActiveUniformName := GLGetProcAddress('glGetActiveUniformName');
   glGetUniformBlockIndex := GLGetProcAddress('glGetUniformBlockIndex');
   glGetActiveUniformBlockiv := GLGetProcAddress('glGetActiveUniformBlockiv');
   glGetActiveUniformBlockName := GLGetProcAddress('glGetActiveUniformBlockName');
   glUniformBlockBinding := GLGetProcAddress('glUniformBlockBinding');

   // GL_ARB_copy_buffer (ARB #59)
   glCopyBufferSubData := GLGetProcAddress('glCopyBufferSubData');

   // GL_ARB_draw_elements_base_vertex (ARB #62)
   glDrawElementsBaseVertex := GLGetProcAddress('glDrawElementsBaseVertex');
   glDrawRangeElementsBaseVertex := GLGetProcAddress('glDrawRangeElementsBaseVertex');
   glDrawElementsInstancedBaseVertex := GLGetProcAddress('glDrawElementsInstancedBaseVertex');
   glMultiDrawElementsBaseVertex := GLGetProcAddress('glMultiDrawElementsBaseVertex');

   // GL_ARB_provoking_vertex (ARB #64)
   glProvokingVertex := GLGetProcAddress('glProvokingVertex');

   // GL_ARB_sync commands (ARB #66)
   glFenceSync := GLGetProcAddress('glFenceSync');
   glIsSync := GLGetProcAddress('glIsSync');
   glDeleteSync := GLGetProcAddress('glDeleteSync');
   glClientWaitSync := GLGetProcAddress('glClientWaitSync');
   glWaitSync := GLGetProcAddress('glWaitSync');
   glGetInteger64v := GLGetProcAddress('glGetInteger64v');
   glGetSynciv := GLGetProcAddress('glGetSynciv');

   // GL_ARB_texture_multisample (ARB #67)
   glTexImage2DMultisample := GLGetProcAddress('glTexImage2DMultisample');
   glTexImage3DMultisample := GLGetProcAddress('glTexImage3DMultisample');
   glGetMultisamplefv := GLGetProcAddress('glGetMultisamplefv');
   glSampleMaski := GLGetProcAddress('glSampleMaski');

   // GL_ARB_draw_buffers_blend (ARB #69)
   glBlendEquationiARB := GLGetProcAddress('glBlendEquationiARB');
   glBlendEquationSeparateiARB := GLGetProcAddress('glBlendEquationSeparateiARB');
   glBlendFunciARB := GLGetProcAddress('glBlendFunciARB');
   glBlendFuncSeparateiARB := GLGetProcAddress('glBlendFuncSeparateiARB');

   // GL_ARB_sample_shading (ARB #70)
   glMinSampleShadingARB := GLGetProcAddress('glMinSampleShadingARB');

{$IFDEF GLS_COMPILER_2005_UP} {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}  {$region 'locate functions/procedures for Vendor/EXT extensions'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //                   Vendor/EXT extensions
   //  ###########################################################

   // functions/procedures belonging to unknown extensions
   glSamplePassARB := GLGetProcAddress('glSamplePassARB');
   glArrayElementArrayEXT := GLGetProcAddress('glArrayElementArrayEXT');

   // WIN_swap_hint (extension # not found)
   glAddSwapHintRectWIN := GLGetProcAddress('glAddSwapHintRectWIN');

   // GL_EXT_blend_color (#2)
   glBlendColorEXT := GLGetProcAddress('glBlendColorEXT');

   // GL_EXT_polygon_offset (#3)
   glPolygonOffsetEXT := GLGetProcAddress('glPolygonOffsetEXT');

   // GL_EXT_texture3D (#6)
   glTexImage3DEXT := GLGetProcAddress('glTexImage3DEXT');

   // GL_EXT_subtexture (#9)
   glTexSubImage1dEXT := GLGetProcAddress('glTexSubImage1DEXT');
   glTexSubImage2dEXT := GLGetProcAddress('glTexSubImage2DEXT');
   glTexSubImage3dEXT := GLGetProcAddress('glTexSubImage3DEXT');

   // GL_EXT_copy_texture (#10)
   glCopyTexImage1DEXT := GLGetProcAddress('glCopyTexImage1DEXT');
   glCopyTexImage2DEXT := GLGetProcAddress('glCopyTexImage2DEXT');
   glCopyTexSubImage1DEXT := GLGetProcAddress('glCopyTexSubImage1DEXT');
   glCopyTexSubImage2DEXT := GLGetProcAddress('glCopyTexSubImage2DEXT');
   glCopyTexSubImage3DEXT := GLGetProcAddress('glCopyTexSubImage3DEXT');

   // GL_EXT_texture_object (#20)
   glGenTexturesEXT := GLGetProcAddress('glGenTexturesEXT');
   glDeleteTexturesEXT := GLGetProcAddress('glDeleteTexturesEXT');
   glBindTextureEXT := GLGetProcAddress('glBindTextureEXT');
   glPrioritizeTexturesEXT := GLGetProcAddress('glPrioritizeTexturesEXT');
   glAreTexturesResidentEXT := GLGetProcAddress('glAreTexturesResidentEXT');
   glIsTextureEXT := GLGetProcAddress('glIsTextureEXT');

   // GL_SGIS_multisample (#25)
   glSampleMaskSGIS := GLGetProcAddress('glSampleMaskSGIS');
   glSamplePatternSGIS := GLGetProcAddress('glSamplePatternSGIS');

   // GL_EXT_blend_minmax (#37)
   glBlendEquationEXT := GLGetProcAddress('glBlendEquationEXT');

   // GL_EXT_paletted_texture (#78)
   glColorTableEXT := GLGetProcAddress('glColorTableEXT');
   glColorSubTableEXT := GLGetProcAddress('glColorSubTableEXT');
   glGetColorTableEXT := GLGetProcAddress('glGetColorTableEXT');
   glGetColorTableParameterivEXT := GLGetProcAddress('glGetColorTableParameterivEXT');
   glGetColorTableParameterfvEXT := GLGetProcAddress('glGetColorTableParameterfvEXT');

   // GL_EXT_index_material (#94)
   glIndexMaterialEXT := GLGetProcAddress('glIndexMaterialEXT');

   // GL_EXT_index_func (#95)
   glIndexFuncEXT := GLGetProcAddress('glIndexFuncEXT');

   // EXT_compiled_vertex_array (#97)
   glLockArraysEXT := GLGetProcAddress('glLockArraysEXT');
   glUnlockArraysEXT := GLGetProcAddress('glUnlockArraysEXT');
   
   // GL_EXT_draw_range_elements (#112)
   glDrawRangeElementsEXT := GLGetProcAddress('glDrawRangeElementsEXT');

   // GL_EXT_secondary_color (#145)
   glSecondaryColor3bEXT := GLGetProcAddress('glSecondaryColor3bEXT');
   glSecondaryColor3bvEXT := GLGetProcAddress('glSecondaryColor3bvEXT');
   glSecondaryColor3dEXT := GLGetProcAddress('glSecondaryColor3dEXT');
   glSecondaryColor3dvEXT := GLGetProcAddress('glSecondaryColor3dvEXT');
   glSecondaryColor3fEXT := GLGetProcAddress('glSecondaryColor3fEXT');
   glSecondaryColor3fvEXT := GLGetProcAddress('glSecondaryColor3fvEXT');
   glSecondaryColor3iEXT := GLGetProcAddress('glSecondaryColor3iEXT');
   glSecondaryColor3ivEXT := GLGetProcAddress('glSecondaryColor3ivEXT');
   glSecondaryColor3sEXT := GLGetProcAddress('glSecondaryColor3sEXT');
   glSecondaryColor3svEXT := GLGetProcAddress('glSecondaryColor3svEXT');
   glSecondaryColor3ubEXT := GLGetProcAddress('glSecondaryColor3ubEXT');
   glSecondaryColor3ubvEXT := GLGetProcAddress('glSecondaryColor3ubvEXT');
   glSecondaryColor3uiEXT := GLGetProcAddress('glSecondaryColor3uiEXT');
   glSecondaryColor3uivEXT := GLGetProcAddress('glSecondaryColor3uivEXT');
   glSecondaryColor3usEXT := GLGetProcAddress('glSecondaryColor3usEXT');
   glSecondaryColor3usvEXT := GLGetProcAddress('glSecondaryColor3usvEXT');
   glSecondaryColorPointerEXT := GLGetProcAddress('glSecondaryColorPointerEXT');

   // GL_EXT_multi_draw_arrays (#148)
   glMultiDrawArraysEXT := GLGetProcAddress('glMultiDrawArraysEXT');
   glMultiDrawElementsEXT := GLGetProcAddress('glMultiDrawElementsEXT');

   // GL_EXT_fog_coord (#149)
   glFogCoordfEXT := GLGetProcAddress('glFogCoordfEXT'); 
   glFogCoordfvEXT := GLGetProcAddress('glFogCoordfvEXT'); 
   glFogCoorddEXT := GLGetProcAddress('glFogCoorddEXT');
   glFogCoorddvEXT := GLGetProcAddress('glFogCoorddvEXT'); 
   glFogCoordPointerEXT := GLGetProcAddress('glFogCoordPointerEXT'); 

   // GL_EXT_blend_func_separate (#173)
   glBlendFuncSeparateEXT := GLGetProcAddress('glBlendFuncSeparateEXT');

   // GL_NV_vertex_array_range (#190)
   glFlushVertexArrayRangeNV := GLGetProcAddress('glFlushVertexArrayRangeNV'); 
   glVertexArrayRangeNV := GLGetProcAddress('glVertexArrayRangeNV');
   wglAllocateMemoryNV := GLGetProcAddress('wglAllocateMemoryNV'); 
   wglFreeMemoryNV := GLGetProcAddress('wglFreeMemoryNV'); 

   // GL_NV_register_combiners (#191)
   glCombinerParameterfvNV := GLGetProcAddress('glCombinerParameterfvNV'); 
   glCombinerParameterfNV := GLGetProcAddress('glCombinerParameterfNV');
   glCombinerParameterivNV := GLGetProcAddress('glCombinerParameterivNV'); 
   glCombinerParameteriNV := GLGetProcAddress('glCombinerParameteriNV'); 
   glCombinerInputNV := GLGetProcAddress('glCombinerInputNV');
   glCombinerOutputNV := GLGetProcAddress('glCombinerOutputNV'); 
   glFinalCombinerInputNV := GLGetProcAddress('glFinalCombinerInputNV');
   glGetCombinerInputParameterfvNV := GLGetProcAddress('glGetCombinerInputParameterfvNV');
   glGetCombinerInputParameterivNV := GLGetProcAddress('glGetCombinerInputParameterivNV'); 
   glGetCombinerOutputParameterfvNV := GLGetProcAddress('glGetCombinerOutputParameterfvNV');
   glGetCombinerOutputParameterivNV := GLGetProcAddress('glGetCombinerOutputParameterivNV');
   glGetFinalCombinerInputParameterfvNV := GLGetProcAddress('glGetFinalCombinerInputParameterfvNV'); 
   glGetFinalCombinerInputParameterivNV := GLGetProcAddress('glGetFinalCombinerInputParameterivNV');

   // GL_MESA_resize_buffers (#196)
   glResizeBuffersMESA := GLGetProcAddress('glResizeBuffersMESA');

   // GL_3DFX_tbuffer (#208)
   glTbufferMask3DFX := GLGetProcAddress('glTbufferMask3DFX');

   // GL_EXT_multisample (#209)
   glSampleMaskEXT := GLGetProcAddress('glSampleMaskEXT');
   glSamplePatternEXT := GLGetProcAddress('glSamplePatternEXT');

   // GL_SGIS_texture_color_mask (#214)
   glTextureColorMaskSGIS := GLGetProcAddress('glTextureColorMaskSGIS');

   // GL_NV_fence (#222)
   glGenFencesNV := GLGetProcAddress('glGenFencesNV');
   glDeleteFencesNV := GLGetProcAddress('glDeleteFencesNV');
   glSetFenceNV := GLGetProcAddress('glSetFenceNV');
   glTestFenceNV := GLGetProcAddress('glTestFenceNV');
   glFinishFenceNV := GLGetProcAddress('glFinishFenceNV');
   glIsFenceNV := GLGetProcAddress('glIsFenceNV');
   glGetFenceivNV := GLGetProcAddress('glGetFenceivNV');

   // GL_NV_vertex_program (#233)
   glAreProgramsResidentNV := GLGetProcAddress('glAreProgramsResidentNV');
   glBindProgramNV := GLGetProcAddress('glBindProgramNV');
   glDeleteProgramsNV := GLGetProcAddress('glDeleteProgramsNV');
   glExecuteProgramNV := GLGetProcAddress('glExecuteProgramNV');
   glGenProgramsNV := GLGetProcAddress('glGenProgramsNV');
   glGetProgramParameterdvNV := GLGetProcAddress('glGetProgramParameterdvNV');
   glGetProgramParameterfvNV := GLGetProcAddress('glGetProgramParameterfvNV');
   glGetProgramivNV := GLGetProcAddress('glGetProgramivNV');
   glGetProgramStringNV := GLGetProcAddress('glGetProgramStringNV');
   glGetTrackMatrixivNV := GLGetProcAddress('glGetTrackMatrixivNV');
   glGetVertexAttribdvNV:= GLGetProcAddress('glGetVertexAttribdvNV');
   glGetVertexAttribfvNV:= GLGetProcAddress('glGetVertexAttribfvNV');
   glGetVertexAttribivNV:= GLGetProcAddress('glGetVertexAttribivNV');
   glGetVertexAttribPointervNV := GLGetProcAddress ('glGetVertexAttribPointervNV');
   glIsProgramNV := GLGetProcAddress('glIsProgramNV');
   glLoadProgramNV := GLGetProcAddress('glLoadProgramNV');
   glProgramParameter4dNV := GLGetProcAddress('glProgramParameter4dNV');
   glProgramParameter4dvNV := GLGetProcAddress('glProgramParameter4dvNV');
   glProgramParameter4fNV := GLGetProcAddress('glProgramParameter4fNV');
   glProgramParameter4fvNV := GLGetProcAddress('glProgramParameter4fvNV');
   glProgramParameters4dvNV := GLGetProcAddress ('glProgramParameters4dvNV');
   glProgramParameters4fvNV := GLGetProcAddress ('glProgramParameters4fvNV');
   glRequestResidentProgramsNV := GLGetProcAddress ('glRequestResidentProgramsNV');
   glTrackMatrixNV := GLGetProcAddress('glTrackMatrixNV');
   glVertexAttribPointerNV := GLGetProcAddress('glVertexAttribPointerNV');
   glVertexAttrib1dNV := GLGetProcAddress('glVertexAttrib1dNV');
   glVertexAttrib1dvNV := GLGetProcAddress('glVertexAttrib1dvNV');
   glVertexAttrib1fNV := GLGetProcAddress('glVertexAttrib1fNV');
   glVertexAttrib1fvNV := GLGetProcAddress('glVertexAttrib1fvNV');
   glVertexAttrib1sNV := GLGetProcAddress('glVertexAttrib1sNV');
   glVertexAttrib1svNV := GLGetProcAddress('glVertexAttrib1svNV');
   glVertexAttrib2dNV := GLGetProcAddress('glVertexAttrib2dNV');
   glVertexAttrib2dvNV := GLGetProcAddress('glVertexAttrib2dvNV');
   glVertexAttrib2fNV := GLGetProcAddress('glVertexAttrib2fNV');
   glVertexAttrib2fvNV := GLGetProcAddress('glVertexAttrib2fvNV');
   glVertexAttrib2sNV := GLGetProcAddress('glVertexAttrib2sNV');
   glVertexAttrib2svNV := GLGetProcAddress('glVertexAttrib2svNV');
   glVertexAttrib3dNV := GLGetProcAddress('glVertexAttrib3dNV');
   glVertexAttrib3dvNV := GLGetProcAddress('glVertexAttrib3dvNV');
   glVertexAttrib3fNV := GLGetProcAddress('glVertexAttrib3fNV');
   glVertexAttrib3fvNV := GLGetProcAddress('glVertexAttrib3fvNV');
   glVertexAttrib3sNV := GLGetProcAddress('glVertexAttrib3sNV');
   glVertexAttrib3svNV := GLGetProcAddress('glVertexAttrib3svNV');
   glVertexAttrib4dNV := GLGetProcAddress('glVertexAttrib4dNV');
   glVertexAttrib4dvNV := GLGetProcAddress('glVertexAttrib4dvNV');
   glVertexAttrib4fNV := GLGetProcAddress('glVertexAttrib4fNV');
   glVertexAttrib4fvNV := GLGetProcAddress('glVertexAttrib4fvNV');
   glVertexAttrib4sNV := GLGetProcAddress('glVertexAttrib4sNV');
   glVertexAttrib4svNV := GLGetProcAddress('glVertexAttrib4svNV');
   glVertexAttrib4ubvNV := GLGetProcAddress('glVertexAttrib4ubvNV');
   glVertexAttribs1dvNV := GLGetProcAddress('glVertexAttribs1dvNV');
   glVertexAttribs1fvNV := GLGetProcAddress('glVertexAttribs1fvNV');
   glVertexAttribs1svNV := GLGetProcAddress('glVertexAttribs1svNV');
   glVertexAttribs2dvNV := GLGetProcAddress('glVertexAttribs2dvNV');
   glVertexAttribs2fvNV := GLGetProcAddress('glVertexAttribs2fvNV');
   glVertexAttribs2svNV := GLGetProcAddress('glVertexAttribs2svNV');
   glVertexAttribs3dvNV := GLGetProcAddress('glVertexAttribs3dvNV');
   glVertexAttribs3fvNV := GLGetProcAddress('glVertexAttribs3fvNV');
   glVertexAttribs3svNV := GLGetProcAddress('glVertexAttribs3svNV');
   glVertexAttribs4dvNV := GLGetProcAddress('glVertexAttribs4dvNV');
   glVertexAttribs4fvNV := GLGetProcAddress('glVertexAttribs4fvNV');
   glVertexAttribs4svNV := GLGetProcAddress('glVertexAttribs4svNV');
   glVertexAttribs4ubvNV := GLGetProcAddress('glVertexAttribs4ubvN');

   // GL_NV_occlusion_query (#261)
   glGenOcclusionQueriesNV := GLGetProcAddress('glGenOcclusionQueriesNV');
   glDeleteOcclusionQueriesNV := GLGetProcAddress('glDeleteOcclusionQueriesNV');
   glIsOcclusionQueryNV := GLGetProcAddress('glIsOcclusionQueryNV');
   glBeginOcclusionQueryNV := GLGetProcAddress('glBeginOcclusionQueryNV');
   glEndOcclusionQueryNV := GLGetProcAddress('glEndOcclusionQueryNV');
   glGetOcclusionQueryivNV := GLGetProcAddress('glGetOcclusionQueryivNV');
   glGetOcclusionQueryuivNV := GLGetProcAddress('glGetOcclusionQueryuivNV');

   // GL_NV_point_sprite (#262)
   glPointParameteriNV := GLGetProcAddress('glPointParameteriNV');
   glPointParameterivNV := GLGetProcAddress('glPointParameterivNV');

   // GL_EXT_stencil_two_side (#268)
   glActiveStencilFaceEXT := GLGetProcAddress('glActiveStencilFaceEXT');

   // GL_ATI_draw_buffers (#277)
   glDrawBuffersATI := GLGetProcAddress('glDrawBuffersATI');

   // GL_NV_primitive_restart (#285)
   glPrimitiveRestartNV := GLGetProcAddress('glPrimitiveRestartNV');
   glPrimitiveRestartIndexNV := GLGetProcAddress('glPrimitiveRestartIndexNV');
   glPrimitiveRestartIndex := GLGetProcAddress('glPrimitiveRestartIndex');

   // GL_EXT_depth_bounds_test (#297)
   glDepthBoundsEXT := GLGetProcAddress('glDepthBoundsEXT');

   // GL_EXT_blend_equation_separate (#299)
   glBlendEquationSeparateEXT := GLGetProcAddress('glBlendEquationSeparateEXT');

   // GL_EXT_framebuffer_object (#310)
   glIsRenderbufferEXT := GLGetProcAddress('glIsRenderbufferEXT');
   glBindRenderbufferEXT := GLGetProcAddress('glBindRenderbufferEXT');
   glDeleteRenderbuffersEXT := GLGetProcAddress('glDeleteRenderbuffersEXT');
   glGenRenderbuffersEXT := GLGetProcAddress('glGenRenderbuffersEXT');
   glRenderbufferStorageEXT := GLGetProcAddress('glRenderbufferStorageEXT');
   glGetRenderbufferParameterivEXT := GLGetProcAddress('glGetRenderbufferParameterivEXT');
   glIsFramebufferEXT := GLGetProcAddress('glIsFramebufferEXT');
   glBindFramebufferEXT := GLGetProcAddress('glBindFramebufferEXT');
   glDeleteFramebuffersEXT := GLGetProcAddress('glDeleteFramebuffersEXT');
   glGenFramebuffersEXT := GLGetProcAddress('glGenFramebuffersEXT');
   glCheckFramebufferStatusEXT := GLGetProcAddress('glCheckFramebufferStatusEXT');
   glFramebufferTexture1DEXT := GLGetProcAddress('glFramebufferTexture1DEXT');
   glFramebufferTexture2DEXT := GLGetProcAddress('glFramebufferTexture2DEXT');
   glFramebufferTexture3DEXT := GLGetProcAddress('glFramebufferTexture3DEXT');
   glFramebufferRenderbufferEXT := GLGetProcAddress('glFramebufferRenderbufferEXT');
   glGetFramebufferAttachmentParameterivEXT := GLGetProcAddress('glGetFramebufferAttachmentParameterivEXT');
   glGenerateMipmapEXT := GLGetProcAddress('glGenerateMipmapEXT');

   // GL_EXT_stencil_clear_tag (EXT #314)
   glStencilClearTagEXT := GLGetProcAddress('glStencilClearTagEXT');

   // GL_EXT_framebuffer_blit (#316)
   glBlitFramebufferEXT := GLGetProcAddress('glBlitFramebufferEXT');

   // GL_EXT_framebuffer_multisample (#317)
   glRenderbufferStorageMultisampleEXT := GLGetProcAddress('glRenderbufferStorageMultisampleEXT');

   // GL_EXT_timer_query (#319)
   glGetQueryObjecti64vEXT := GLGetProcAddress('glGetQueryObjecti64vEXT');
   glGetQueryObjectui64vEXT := GLGetProcAddress('glGetQueryObjectui64vEXT');

   // GL_EXT_gpu_program_parameters (#320)
   glProgramEnvParameters4fvEXT := GLGetProcAddress('glProgramEnvParameters4fvEXT');
   glProgramLocalParameters4fvEXT := GLGetProcAddress('glProgramLocalParameters4fvEXT');

   // GL_NV_geometry_program4 (#323)
   glProgramVertexLimitNV := GLGetProcAddress('glProgramVertexLimitNV');

   // GL_EXT_geometry_shader4 (#324)
   glProgramParameteriEXT := GLGetProcAddress('glProgramParameteriEXT');
   glFramebufferTextureEXT := GLGetProcAddress('glFramebufferTextureEXT');
   glFramebufferTextureLayerEXT := GLGetProcAddress('glFramebufferTextureLayerEXT');
   glFramebufferTextureFaceEXT := GLGetProcAddress('glFramebufferTextureFaceEXT');

   // GL_EXT_gpu_shader4 (#326)
   glVertexAttribI1iEXT := GLGetProcAddress('glVertexAttribI1iEXT');
   glVertexAttribI2iEXT := GLGetProcAddress('glVertexAttribI2iEXT');
   glVertexAttribI3iEXT := GLGetProcAddress('glVertexAttribI3iEXT');
   glVertexAttribI4iEXT := GLGetProcAddress('glVertexAttribI4iEXT');

   glVertexAttribI1uiEXT := GLGetProcAddress('glVertexAttribI1uiEXT');
   glVertexAttribI2uiEXT := GLGetProcAddress('glVertexAttribI2uiEXT');
   glVertexAttribI3uiEXT := GLGetProcAddress('glVertexAttribI3uiEXT');
   glVertexAttribI4uiEXT := GLGetProcAddress('glVertexAttribI4uiEXT');

   glVertexAttribI1ivEXT := GLGetProcAddress('glVertexAttribI1ivEXT');
   glVertexAttribI2ivEXT := GLGetProcAddress('glVertexAttribI2ivEXT');
   glVertexAttribI3ivEXT := GLGetProcAddress('glVertexAttribI3ivEXT');
   glVertexAttribI4ivEXT := GLGetProcAddress('glVertexAttribI4ivEXT');

   glVertexAttribI1uivEXT := GLGetProcAddress('glVertexAttribI1uivEXT');
   glVertexAttribI2uivEXT := GLGetProcAddress('glVertexAttribI2uivEXT');
   glVertexAttribI3uivEXT := GLGetProcAddress('glVertexAttribI3uivEXT');
   glVertexAttribI4uivEXT := GLGetProcAddress('glVertexAttribI4uivEXT');

   glVertexAttribI4bvEXT := GLGetProcAddress('glVertexAttribI4bvEXT');
   glVertexAttribI4svEXT := GLGetProcAddress('glVertexAttribI4svEXT');
   glVertexAttribI4ubvEXT := GLGetProcAddress('glVertexAttribI4ubvEXT');
   glVertexAttribI4usvEXT := GLGetProcAddress('glVertexAttribI4usvEXT');

   glVertexAttribIPointerEXT := GLGetProcAddress('glVertexAttribIPointerEXT');

   glGetVertexAttribIivEXT := GLGetProcAddress('glGetVertexAttribIivEXT');
   glGetVertexAttribIuivEXT := GLGetProcAddress('glGetVertexAttribIuivEXT');

   glUniform1uiEXT := GLGetProcAddress('glUniform1uiEXT');
   glUniform2uiEXT := GLGetProcAddress('glUniform2uiEXT');
   glUniform3uiEXT := GLGetProcAddress('glUniform3uiEXT');
   glUniform4uiEXT := GLGetProcAddress('glUniform4uiEXT');

   glUniform1uivEXT := GLGetProcAddress('glUniform1uivEXT');
   glUniform2uivEXT := GLGetProcAddress('glUniform2uivEXT');
   glUniform3uivEXT := GLGetProcAddress('glUniform3uivEXT');
   glUniform4uivEXT := GLGetProcAddress('glUniform4uivEXT');

   glGetUniformuivEXT := GLGetProcAddress('glGetUniformuivEXT');

   glBindFragDataLocationEXT := GLGetProcAddress('glBindFragDataLocationEXT');
   glGetFragDataLocationEXT := GLGetProcAddress('glGetFragDataLocationEXT');

   // GL_EXT_draw_instanced (#327)
   glDrawArraysInstancedEXT := GLGetProcAddress('glDrawArraysInstancedEXT');
   glDrawElementsInstancedEXT := GLGetProcAddress('glDrawElementsInstancedEXT');

   // GL_EXT_texture_array (#329)
   glFramebufferTextureLayerEXT:= GLGetProcAddress('glFramebufferTextureLayerEXT');

   // GL_EXT_texture_buffer_object (#330)
   glTexBufferEXT := GLGetProcAddress('glTexBufferEXT');

   // GL_EXT_draw_buffers2 (#340)
   glColorMaskIndexedEXT := GLGetProcAddress('glColorMaskIndexedEXT');
   glGetBooleanIndexedvEXT := GLGetProcAddress('glGetBooleanIndexedvEXT');
   glGetIntegerIndexedvEXT:= GLGetProcAddress('glGetIntegerIndexedvEXT');
   glEnableIndexedEXT:= GLGetProcAddress('glEnableIndexedEXT');
   glDisableIndexedEXT:= GLGetProcAddress('glDisableIndexedEXT');
   glIsEnabledIndexedEXT:= GLGetProcAddress('glIsEnabledIndexedEXT');

   // GL_NV_transform_feedback (#341)
   glBindBufferRangeNV := GLGetProcAddress('glBindBufferRangeNV');
   glBindBufferOffsetNV := GLGetProcAddress('glBindBufferOffsetNV');
   glBindBufferBaseNV := GLGetProcAddress('glBindBufferBaseNV');
   glTransformFeedbackAttribsNV := GLGetProcAddress('glTransformFeedbackAttribsNV');
   glTransformFeedbackVaryingsNV := GLGetProcAddress('glTransformFeedbackVaryingsNV');
   glBeginTransformFeedbackNV := GLGetProcAddress('glBeginTransformFeedbackNV');
   glEndTransformFeedbackNV := GLGetProcAddress('glEndTransformFeedbackNV');
   glGetVaryingLocationNV := GLGetProcAddress('glGetVaryingLocationNV');
   glGetActiveVaryingNV := GLGetProcAddress('glGetActiveVaryingNV');
   glActiveVaryingNV := GLGetProcAddress('glActiveVaryingNV');
   glGetTransformFeedbackVaryingNV := GLGetProcAddress('glGetTransformFeedbackVaryingNV');

   // GL_EXT_bindable_uniform (#342)
   glUniformBufferEXT := GLGetProcAddress('glUniformBufferEXT');
   glGetUniformBufferSizeEXT := GLGetProcAddress('glGetUniformBufferSizeEXT');
   glGetUniformOffsetEXT := GLGetProcAddress('glGetUniformOffsetEXT');

   // GL_EXT_texture_integer (#343)
   glClearColorIiEXT := GLGetProcAddress('glClearColorIiEXT');
   glClearColorIuiEXT := GLGetProcAddress('glClearColorIuiEXT');
   glTexParameterIivEXT := GLGetProcAddress('glTexParameterIivEXT');
   glTexParameterIuivEXT := GLGetProcAddress('glTexParameterIuivEXT');
   glGetTexParameterIivEXT := GLGetProcAddress('glGetTexParameterIivEXT');
   glGetTexParameterIuivEXT := GLGetProcAddress('glGetTexParameterIuivEXT');

   // GL_NV_conditional_render (#346)
   glBeginConditionalRenderNV := GLGetProcAddress('glBeginConditionalRenderNV');
   glEndConditionalRenderNV := GLGetProcAddress('glEndConditionalRenderNV');

   // GL_EXT_transform_feedback (#352)
   glBindBufferRangeEXT := GLGetProcAddress('glBindBufferRangeEXT');
   glBindBufferOffsetEXT := GLGetProcAddress('glBindBufferOffsetEXT');
   glBindBufferBaseEXT := GLGetProcAddress('glBindBufferBaseEXT');
   glBeginTransformFeedbackEXT := GLGetProcAddress('glBeginTransformFeedbackEXT');
   glEndTransformFeedbackEXT := GLGetProcAddress('glEndTransformFeedbackEXT');
   glTransformFeedbackVaryingsEXT := GLGetProcAddress('glTransformFeedbackVaryingsEXT');
   glGetTransformFeedbackVaryingEXT:= GLGetProcAddress('glGetTransformFeedbackVaryingEXT');

   // GL_AMD_vertex_shader_tesselator (#363)
   glTessellationFactorAMD := GLGetProcAddress('glTessellationFactorAMD');
   glTessellationModeAMD := GLGetProcAddress('glTessellationModeAMD');

   // GL_NV_copy_image (#376)
   glCopyImageSubDataNV := GLGetProcAddress('glCopyImageSubDataNV');

{$IFDEF GLS_COMPILER_2005_UP}  {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}  {$region 'locate functions/procedures for Windows OpenGL (WGL) extensions'} {$ENDIF}
   {$IFDEF SUPPORT_WGL}
   ReadWGLExtensions;
   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP}  {$endregion} {$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}  {$region 'locate functions/procedures for GLX extensions'} {$ENDIF}
   {$IFDEF SUPPORT_GLX}
   ReadGLXExtensions;
   {$ENDIF}
{$IFDEF GLS_COMPILER_2005_UP}  {$endregion} {$ENDIF}

end;

{$IFDEF SUPPORT_WGL}
// ReadWGLExtensions
//
procedure ReadWGLExtensions;
begin
   // ARB wgl extensions

   //  ###########################################################
   //            locating functions and procedures for
   //                  ARB approved WGL extensions
   //  ###########################################################

   // WGL_buffer_region (ARB #4)
   wglCreateBufferRegionARB := GLGetProcAddress('wglCreateBufferRegionARB');
   wglDeleteBufferRegionARB := GLGetProcAddress('wglDeleteBufferRegionARB');
   wglSaveBufferRegionARB := GLGetProcAddress('wglSaveBufferRegionARB');
   wglRestoreBufferRegionARB := GLGetProcAddress('wglRestoreBufferRegionARB');

   // WGL_ARB_extensions_string (ARB #8)
   wglGetExtensionsStringARB := GLGetProcAddress('wglGetExtensionsStringARB');

   // WGL_ARB_pixel_format (ARB #9)
   wglGetPixelFormatAttribivARB := GLGetProcAddress('wglGetPixelFormatAttribivARB');
   wglGetPixelFormatAttribfvARB := GLGetProcAddress('wglGetPixelFormatAttribfvARB');
   wglChoosePixelFormatARB := GLGetProcAddress('wglChoosePixelFormatARB');

   // WGL_make_current_read (ARB #10)
   wglMakeContextCurrentARB := GLGetProcAddress('wglMakeContextCurrentARB');
   wglGetCurrentReadDCARB := GLGetProcAddress('wglGetCurrentReadDCARB');

   // WGL_ARB_pbuffer (ARB #11)
   wglCreatePbufferARB := GLGetProcAddress('wglCreatePbufferARB');
   wglGetPbufferDCARB := GLGetProcAddress('wglGetPbufferDCARB');
   wglReleasePbufferDCARB := GLGetProcAddress('wglReleasePbufferDCARB');
   wglDestroyPbufferARB := GLGetProcAddress('wglDestroyPbufferARB');
   wglQueryPbufferARB := GLGetProcAddress('wglQueryPbufferARB');

   // WGL_ARB_render_texture (ARB #20)
   wglBindTexImageARB := GLGetProcAddress('wglBindTexImageARB');
   wglReleaseTexImageARB := GLGetProcAddress('wglReleaseTexImageARB');
   wglSetPbufferAttribARB := GLGetProcAddress('wglSetPbufferAttribARB');

   // WGL_ARB_create_context (ARB #55)
   wglCreateContextAttribsARB := GLGetProcAddress('wglCreateContextAttribsARB');

   //  ###########################################################
   //            locating functions and procedures for
   //                Vendor/EXT WGL extensions
   //  ###########################################################

   // WGL_EXT_swap_control (EXT #172)
   wglSwapIntervalEXT := GLGetProcAddress('wglSwapIntervalEXT');
   wglGetSwapIntervalEXT := GLGetProcAddress('wglGetSwapIntervalEXT');
end;
{$ENDIF}

{$IFDEF SUPPORT_GLX}
// ReadGLXExtensions
//
procedure ReadGLXExtensions;
begin
   // ARB glx extensions

   //  ###########################################################
   //            locating functions and procedures for
   //                  ARB approved GLX extensions
   //  ###########################################################

   // GLX_ARB_create_context (EXT #56)
   glXCreateContextAttribsARB := GLGetProcAddress('glXCreateContextAttribsARB');

   //  ###########################################################
   //            locating functions and procedures for
   //                Vendor/EXT GLX extensions
   //  ###########################################################

   // WGL_EXT_swap_control (EXT #172)
   glXSwapIntervalSGI := GLGetProcAddress('glXSwapIntervalSGI');

end;
{$ENDIF}

// TrimAndSplitVersionString
//
procedure TrimAndSplitVersionString(Buffer: String; var Max, Min: Integer);
// Peels out the X.Y form from the given Buffer which must contain a version string like "text Minor.Major.Build text"
// at least however "Major.Minor".
var
  Separator: Integer;
begin
  try
    // There must be at least one dot to separate major and minor version number.
    Separator := Pos('.', Buffer);
    // At least one number must be before and one after the dot.
    if (Separator > 1) and (Separator < Length(Buffer)) and (AnsiChar(Buffer[Separator - 1]) in ['0'..'9']) and
      (AnsiChar(Buffer[Separator + 1]) in ['0'..'9']) then
    begin
      // OK, it's a valid version string. Now remove unnecessary parts.
      Dec(Separator);
      // Find last non-numeric character before version number.
      while (Separator > 0) and (AnsiChar(Buffer[Separator]) in ['0'..'9']) do
        Dec(Separator);
      // Delete leading characters which do not belong to the version string.
      Delete(Buffer, 1, Separator);
      Separator := Pos('.', Buffer) + 1;
      // Find first non-numeric character after version number
      while (Separator <= Length(Buffer)) and (AnsiChar(Buffer[Separator]) in ['0'..'9']) do
        Inc(Separator);
      // delete trailing characters not belonging to the version string
      Delete(Buffer, Separator, 255);
      // Now translate the numbers.
      Separator := Pos('.', Buffer); // This is necessary because the buffer length might have changed.
      Max := StrToInt(Copy(Buffer, 1, Separator - 1));
      Min := StrToInt(Copy(Buffer, Separator + 1, 255));
    end
    else
      Abort;
  except
    Min := 0;
    Max := 0;
  end;
end;

function IsVersionMet(MajorVersion,MinorVersion,actualMajorVersion, actualMinorVersion:Integer): boolean;
begin
  Result:=(actualMajorVersion>MajorVersion)or
          ((actualMajorVersion=MajorVersion)and(actualMinorVersion>=MinorVersion));
end;

// ReadImplementationProperties
//
procedure ReadImplementationProperties;
var
   Buffer : String;
   MajorVersion, MinorVersion: Integer;

   // Checks if the given Extension string is in Buffer.
   function CheckExtension(const Extension: string): Boolean;
   var
     ExtPos: Integer;
   begin
     // First find the position of the extension string as substring in Buffer.
     ExtPos := Pos(Extension, Buffer);
     Result := ExtPos > 0;
     // Now check that it isn't only a substring of another extension.
     if Result then
       Result := ((ExtPos + Length(Extension) - 1)= Length(Buffer))
                 or (Buffer[ExtPos + Length(Extension)]=' ');
   end;

begin
   // determine OpenGL versions supported
   buffer:=String(glGetString(GL_VERSION));
   TrimAndSplitVersionString(buffer, majorversion, minorVersion);
   GL_VERSION_1_0:=True;
   GL_VERSION_1_1:=IsVersionMet(1,1,majorVersion,minorVersion);
   GL_VERSION_1_2:=IsVersionMet(1,2,majorVersion,minorVersion);
   GL_VERSION_1_3:=IsVersionMet(1,3,majorVersion,minorVersion);
   GL_VERSION_1_4:=IsVersionMet(1,4,majorVersion,minorVersion);
   GL_VERSION_1_5:=IsVersionMet(1,5,majorVersion,minorVersion);
   GL_VERSION_2_0:=IsVersionMet(2,0,majorVersion,minorVersion);
   GL_VERSION_2_1:=IsVersionMet(2,1,majorVersion,minorVersion);
   GL_VERSION_2_2:=IsVersionMet(2,2,majorVersion,minorVersion);
   GL_VERSION_3_0:=IsVersionMet(3,0,majorVersion,minorVersion);
   GL_VERSION_3_1:=IsVersionMet(3,1,majorVersion,minorVersion);
   GL_VERSION_3_2:=IsVersionMet(3,2,majorVersion,minorVersion);

   // determine GLU versions met
   buffer:=String(gluGetString(GLU_VERSION));
   TrimAndSplitVersionString(buffer, majorversion, minorVersion);
   GLU_VERSION_1_1:=True; // won't load without at least GLU 1.1
   GLU_VERSION_1_2:=IsVersionMet(1,2,majorVersion,minorVersion);
   GLU_VERSION_1_3:=IsVersionMet(1,3,majorVersion,minorVersion);

   // check supported OpenGL extensions
   Buffer := String(glGetString(GL_EXTENSIONS));
   // check ARB approved OpenGL extensions
   GL_ARB_color_buffer_float := CheckExtension('GL_ARB_color_buffer_float');
   GL_ARB_compatibility := CheckExtension('GL_ARB_compatibility');
   GL_ARB_copy_buffer := CheckExtension('GL_ARB_copy_buffer');
   GL_ARB_depth_buffer_float := CheckExtension('GL_ARB_depth_buffer_float');
   GL_ARB_depth_clamp := CheckExtension('GL_ARB_depth_clamp');
   GL_ARB_depth_texture := CheckExtension('GL_ARB_depth_texture');
   GL_ARB_draw_buffers := CheckExtension('GL_ARB_draw_buffers');
   GL_ARB_draw_buffers_blend := CheckExtension('GL_ARB_draw_buffers_blend');
   GL_ARB_draw_elements_base_vertex := CheckExtension('GL_ARB_draw_elements_base_vertex');
   GL_ARB_draw_instanced := CheckExtension('GL_ARB_draw_instanced');
   GL_ARB_fragment_coord_conventions := CheckExtension('GL_ARB_fragment_coord_conventions');
   GL_ARB_fragment_program := CheckExtension('GL_ARB_fragment_program');
   GL_ARB_fragment_program_shadow := CheckExtension('GL_ARB_fragment_program_shadow');
   GL_ARB_fragment_shader := CheckExtension('GL_ARB_fragment_shader');
   GL_ARB_framebuffer_object := CheckExtension('GL_ARB_framebuffer_object');
   GL_ARB_framebuffer_sRGB := CheckExtension('GL_ARB_framebuffer_sRGB');
   GL_ARB_geometry_shader4 := CheckExtension('GL_ARB_geometry_shader4');
   GL_ARB_half_float_pixel := CheckExtension('GL_ARB_half_float_pixel');
   GL_ARB_half_float_vertex := CheckExtension('GL_ARB_half_float_vertex');
   GL_ARB_imaging := CheckExtension('GL_ARB_imaging');
   GL_ARB_instanced_arrays := CheckExtension('GL_ARB_instanced_arrays');
   GL_ARB_map_buffer_range := CheckExtension('GL_ARB_map_buffer_range');
   GL_ARB_matrix_palette  := CheckExtension('GL_ARB_matrix_palette');
   GL_ARB_multisample := CheckExtension(' GL_ARB_multisample'); // ' ' to avoid collision with WGL variant
   GL_ARB_multitexture := CheckExtension('GL_ARB_multitexture');
   GL_ARB_occlusion_query := CheckExtension('GL_ARB_occlusion_query');
   GL_ARB_pixel_buffer_object := CheckExtension('GL_ARB_pixel_buffer_object');
   GL_ARB_point_parameters := CheckExtension('GL_ARB_point_parameters');
   GL_ARB_point_sprite := CheckExtension('GL_ARB_point_sprite');
   GL_ARB_provoking_vertex := CheckExtension('GL_ARB_provoking_vertex');
   GL_ARB_sample_shading := CheckExtension('GL_ARB_sample_shading');
   GL_ARB_seamless_cube_map := CheckExtension('GL_ARB_seamless_cube_map');
   GL_ARB_shader_objects := CheckExtension('GL_ARB_shader_objects');
   GL_ARB_shader_texture_lod := CheckExtension('GL_ARB_shader_texture_lod');
   GL_ARB_shading_language_100 := CheckExtension('GL_ARB_shading_language_100');
   GL_ARB_shadow := CheckExtension('GL_ARB_shadow');
   GL_ARB_shadow_ambient := CheckExtension('GL_ARB_shadow_ambient');
   GL_ARB_sync := CheckExtension('GL_ARB_sync');
   GL_ARB_texture_border_clamp := CheckExtension('GL_ARB_texture_border_clamp');
   GL_ARB_texture_buffer_object := CheckExtension('GL_ARB_texture_buffer_object');
   GL_ARB_texture_compression := CheckExtension('GL_ARB_texture_compression');
   GL_ARB_texture_compression_rgtc := CheckExtension('GL_ARB_texture_compression_rgtc');
   GL_ARB_texture_cube_map := CheckExtension('GL_ARB_texture_cube_map');
   GL_ARB_texture_cube_map_array := CheckExtension('GL_ARB_texture_cube_map_array');
   GL_ARB_texture_env_add := CheckExtension('GL_ARB_texture_env_add');
   GL_ARB_texture_env_combine := CheckExtension('GL_ARB_texture_env_combine');
   GL_ARB_texture_env_crossbar := CheckExtension('GL_ARB_texture_env_crossbar');
   GL_ARB_texture_env_dot3 := CheckExtension('GL_ARB_texture_env_dot3');
   GL_ARB_texture_float := CheckExtension('GL_ARB_texture_float');
   GL_ARB_texture_gather := CheckExtension('GL_ARB_texture_gather');
   GL_ARB_texture_mirrored_repeat := CheckExtension('GL_ARB_texture_mirrored_repeat');
   GL_ARB_texture_multisample := CheckExtension('GL_ARB_texture_multisample');
   GL_ARB_texture_non_power_of_two := CheckExtension('GL_ARB_texture_non_power_of_two');
   GL_ARB_texture_query_lod := CheckExtension('GL_ARB_texture_query_lod');
   GL_ARB_texture_rectangle := CheckExtension('GL_ARB_texture_rectangle');
   GL_ARB_texture_rg := CheckExtension('GL_ARB_texture_rg');
   GL_ARB_transpose_matrix := CheckExtension('GL_ARB_transpose_matrix');
   GL_ARB_uniform_buffer_object := CheckExtension('GL_ARB_uniform_buffer_object');
   GL_ARB_vertex_array_bgra := CheckExtension('GL_ARB_vertex_array_bgra');
   GL_ARB_vertex_array_object := CheckExtension('GL_ARB_vertex_array_object');
   GL_ARB_vertex_blend := CheckExtension('GL_ARB_vertex_blend');
   GL_ARB_vertex_buffer_object := CheckExtension('GL_ARB_vertex_buffer_object');
   GL_ARB_vertex_program := CheckExtension('GL_ARB_vertex_program');
   GL_ARB_vertex_shader := CheckExtension('GL_ARB_vertex_shader');
   GL_ARB_window_pos := CheckExtension('GL_ARB_window_pos');

   // check Vendor/EXT OpenGL extensions
   GL_3DFX_multisample := CheckExtension('GL_3DFX_multisample');
   GL_3DFX_tbuffer := CheckExtension('GL_3DFX_tbuffer');
   GL_3DFX_texture_compression_FXT1 := CheckExtension('GL_3DFX_texture_compression_FXT1');
   GL_ATI_draw_buffers := CheckExtension('GL_ATI_draw_buffers');
   GL_ATI_texture_compression_3dc := CheckExtension('GL_ATI_texture_compression_3dc');
   GL_ATI_texture_float := CheckExtension('GL_ATI_texture_float');
   GL_ATI_texture_mirror_once := CheckExtension('GL_ATI_texture_mirror_once');

   GL_EXT_abgr := CheckExtension('GL_EXT_abgr');
   GL_EXT_bgra := CheckExtension('GL_EXT_bgra');
   GL_EXT_bindable_uniform := CheckExtension('GL_EXT_bindable_uniform');   
   GL_EXT_blend_color := CheckExtension('GL_EXT_blend_color');
   GL_EXT_blend_equation_separate := CheckExtension('GL_EXT_blend_equation_separate');
   GL_EXT_blend_func_separate := CheckExtension('GL_EXT_blend_func_separate');
   GL_EXT_blend_logic_op := CheckExtension('GL_EXT_blend_logic_op');
   GL_EXT_blend_minmax := CheckExtension('GL_EXT_blend_minmax');
   GL_EXT_blend_subtract := CheckExtension('GL_EXT_blend_subtract');
   GL_EXT_Cg_shader := CheckExtension('GL_EXT_Cg_shader');
   GL_EXT_clip_volume_hint := CheckExtension('GL_EXT_clip_volume_hint');
   GL_EXT_compiled_vertex_array := CheckExtension('GL_EXT_compiled_vertex_array');
   GL_EXT_copy_texture := CheckExtension('GL_EXT_copy_texture');
   GL_EXT_depth_bounds_test := CheckExtension('GL_EXT_depth_bounds_test');
   GL_EXT_draw_buffers2 := CheckExtension('GL_EXT_draw_buffers2');
   GL_EXT_draw_instanced := CheckExtension('GL_EXT_draw_instanced');
   GL_EXT_draw_range_elements := CheckExtension('GL_EXT_draw_range_elements');
   GL_EXT_fog_coord := CheckExtension('GL_EXT_fog_coord');
   GL_EXT_framebuffer_blit := CheckExtension('GL_EXT_framebuffer_blit');
   GL_EXT_framebuffer_multisample := CheckExtension('GL_EXT_framebuffer_multisample');
   GL_EXT_framebuffer_object := CheckExtension('GL_EXT_framebuffer_object');
   GL_EXT_framebuffer_sRGB := CheckExtension('GL_EXT_framebuffer_sRGB');
   GL_EXT_geometry_shader4 := CheckExtension('GL_EXT_geometry_shader4');
   GL_EXT_gpu_program_parameters := CheckExtension('GL_EXT_gpu_program_parameters');
   GL_EXT_gpu_shader4 := CheckExtension('GL_EXT_gpu_shader4');
   GL_EXT_multi_draw_arrays := CheckExtension('GL_EXT_multi_draw_arrays');
   GL_EXT_multisample := CheckExtension('GL_EXT_multisample');
   GL_EXT_packed_depth_stencil := CheckExtension('GL_EXT_packed_depth_stencil');
   GL_EXT_packed_float := CheckExtension('GL_EXT_packed_float');
   GL_EXT_packed_pixels := CheckExtension('GL_EXT_packed_pixels');
   GL_EXT_paletted_texture := CheckExtension('GL_EXT_paletted_texture');
   GL_EXT_pixel_buffer_object := CheckExtension('GL_EXT_pixel_buffer_object');
   GL_EXT_polygon_offset := CheckExtension('GL_EXT_polygon_offset');
   GL_EXT_rescale_normal := CheckExtension('GL_EXT_rescale_normal');
   GL_EXT_secondary_color := CheckExtension('GL_EXT_secondary_color');
   GL_EXT_separate_specular_color := CheckExtension('GL_EXT_separate_specular_color');
   GL_EXT_shadow_funcs := CheckExtension('GL_EXT_shadow_funcs');
   GL_EXT_shared_texture_palette := CheckExtension('GL_EXT_shared_texture_palette');
   GL_EXT_stencil_clear_tag := CheckExtension('GL_EXT_stencil_clear_tag');
   GL_EXT_stencil_two_side := CheckExtension('EXT_stencil_two_side');
   GL_EXT_stencil_wrap := CheckExtension('GL_EXT_stencil_wrap');
   GL_EXT_texture3D := CheckExtension('GL_EXT_texture3D');
   GL_EXT_texture_array := CheckExtension('GL_EXT_texture_array');
   GL_EXT_texture_buffer_object := CheckExtension('GL_EXT_texture_buffer_object');
   GL_EXT_texture_compression_latc := CheckExtension('GL_EXT_texture_compression_latc');
   GL_EXT_texture_compression_rgtc := CheckExtension('GL_EXT_texture_compression_rgtc');
   GL_EXT_texture_compression_s3tc := CheckExtension('GL_EXT_texture_compression_s3tc');
   GL_EXT_texture_cube_map := CheckExtension('GL_EXT_texture_cube_map');
   GL_EXT_texture_edge_clamp := CheckExtension('GL_EXT_texture_edge_clamp');
   GL_EXT_texture_env_add := CheckExtension('GL_EXT_texture_env_add');
   GL_EXT_texture_env_combine := CheckExtension('GL_EXT_texture_env_combine');
   GL_EXT_texture_env_dot3 := CheckExtension('GL_EXT_texture_env_dot3');
   GL_EXT_texture_filter_anisotropic := CheckExtension('GL_EXT_texture_filter_anisotropic');
   GL_EXT_texture_integer := CheckExtension('GL_EXT_texture_integer');
   GL_EXT_texture_lod := CheckExtension('GL_EXT_texture_lod');
   GL_EXT_texture_lod_bias := CheckExtension('GL_EXT_texture_lod_bias');
   GL_EXT_texture_mirror_clamp := CheckExtension('GL_EXT_texture_mirror_clamp');
   GL_EXT_texture_object := CheckExtension('GL_EXT_texture_object');
   GL_EXT_texture_rectangle := CheckExtension('GL_EXT_texture_rectangle');
   GL_EXT_texture_sRGB := CheckExtension('GL_EXT_texture_sRGB');
   GL_EXT_texture_shared_exponent := CheckExtension('GL_EXT_texture_shared_exponent');
   GL_EXT_timer_query := CheckExtension('GL_EXT_timer_query');
   GL_EXT_transform_feedback := CheckExtension('GL_EXT_transform_feedback');
   GL_EXT_vertex_array := CheckExtension('GL_EXT_vertex_array');

   GL_HP_occlusion_test := CheckExtension('GL_HP_occlusion_test');

   GL_IBM_rasterpos_clip := CheckExtension('GL_IBM_rasterpos_clip');

   GL_KTX_buffer_region := CheckExtension('GL_KTX_buffer_region');

   GL_MESA_resize_buffers := CheckExtension('GL_MESA_resize_buffers');

   GL_NV_blend_square := CheckExtension('GL_NV_blend_square');
   GL_NV_conditional_render := CheckExtension('GL_NV_conditional_render');
   GL_NV_copy_image := CheckExtension('GL_NV_copy_image');
   GL_NV_depth_buffer_float := CheckExtension('GL_NV_depth_buffer_float');
   GL_NV_fence := CheckExtension('GL_NV_fence');
   GL_NV_float_buffer := CheckExtension('GL_NV_float_buffer');
   GL_NV_fog_distance := CheckExtension('GL_NV_fog_distance');
   GL_NV_geometry_program4 := CheckExtension('GL_NV_geometry_program4');
   GL_NV_light_max_exponent := CheckExtension('GL_NV_light_max_exponent');
   GL_NV_multisample_filter_hint  := CheckExtension('GL_NV_multisample_filter_hint');
   GL_NV_occlusion_query := CheckExtension('GL_NV_occlusion_query');
   GL_NV_point_sprite := CheckExtension('GL_NV_point_sprite');
   GL_NV_primitive_restart := CheckExtension('GL_NV_primitive_restart');
   GL_NV_register_combiners := CheckExtension('GL_NV_register_combiners');
   GL_NV_texgen_reflection := CheckExtension('GL_NV_texgen_reflection');
   GL_NV_texture_compression_vtc := CheckExtension('GL_NV_texture_compression_vtc');
   GL_NV_texture_env_combine4 := CheckExtension('GL_NV_texture_env_combine4');
   GL_NV_texture_rectangle := CheckExtension('GL_NV_texture_rectangle');
   GL_NV_texture_shader := CheckExtension('GL_NV_texture_shader');
   GL_NV_texture_shader2 := CheckExtension('GL_NV_texture_shader2');
   GL_NV_texture_shader3 := CheckExtension('GL_NV_texture_shader3');
   GL_NV_transform_feedback := CheckExtension('GL_NV_transform_feedback');
   GL_NV_vertex_array_range := CheckExtension('GL_NV_vertex_array_range');
   GL_NV_vertex_array_range2 := CheckExtension('GL_NV_vertex_array_range2');
   GL_NV_vertex_program := CheckExtension('GL_NV_vertex_program');

   GL_SGI_color_matrix := CheckExtension('GL_SGI_color_matrix');

   GL_SGIS_generate_mipmap := CheckExtension('GL_SGIS_generate_mipmap');
   GL_SGIS_multisample := CheckExtension('GL_SGIS_multisample');
   GL_SGIS_texture_border_clamp := CheckExtension('GL_SGIS_texture_border_clamp');
   GL_SGIS_texture_color_mask := CheckExtension('GL_SGIS_texture_color_mask');
   GL_SGIS_texture_edge_clamp := CheckExtension('GL_SGIS_texture_edge_clamp');
   GL_SGIS_texture_lod := CheckExtension('GL_SGIS_texture_lod');

   GL_SGIX_depth_texture := CheckExtension('GL_SGIX_depth_texture');
   GL_SGIX_shadow := CheckExtension('GL_SGIX_shadow'); 
   GL_SGIX_shadow_ambient := CheckExtension('GL_SGIX_shadow_ambient');

   GL_AMD_vertex_shader_tessellator := CheckExtension('GL_AMD_vertex_shader_tessellator');

   GL_WIN_swap_hint := CheckExtension('GL_WIN_swap_hint');

   // check supported GLU extensions
   Buffer := String(gluGetString(GLU_EXTENSIONS));
   GLU_EXT_nurbs_tessellator := CheckExtension('GLU_EXT_nurbs_tessellator');
   GLU_EXT_object_space_tess := CheckExtension('GLU_EXT_object_space_tess');
   GLU_EXT_TEXTURE := CheckExtension('GLU_EXT_TEXTURE');

   {$IFDEF SUPPORT_WGL}
   //check supported WGL extensions
   ReadWGLImplementationProperties;
   {$ENDIF}

   {$IFDEF SUPPORT_GLX}
   //check supported GLX extensions
   ReadGLXImplementationProperties;
   {$ENDIF}
end;

{$IFDEF SUPPORT_WGL}
// ReadWGLImplementationProperties
//
procedure ReadWGLImplementationProperties;
var
   Buffer: string;

   // Checks if the given Extension string is in Buffer.
   function CheckExtension(const Extension: string): Boolean;
   var
     ExtPos: Integer;
   begin
     // First find the position of the extension string as substring in Buffer.
     ExtPos := Pos(Extension, Buffer);
     Result := ExtPos > 0;
     // Now check that it isn't only a substring of another extension.
     if Result then
       Result := ((ExtPos + Length(Extension) - 1)= Length(Buffer))
                 or (Buffer[ExtPos + Length(Extension)]=' ');
   end;

begin
   // ARB wgl extensions
   if Assigned(wglGetExtensionsStringARB) then
      Buffer:=String(wglGetExtensionsStringARB(wglGetCurrentDC))
   else Buffer:='';
   WGL_ARB_buffer_region:=CheckExtension('WGL_ARB_buffer_region');
   WGL_ARB_create_context := CheckExtension('WGL_ARB_create_context');
   WGL_ARB_create_context_profile := CheckExtension('WGL_ARB_create_context_profile');
   WGL_ARB_extensions_string:=CheckExtension('WGL_ARB_extensions_string');
   WGL_ARB_framebuffer_sRGB := CheckExtension('WGL_ARB_framebuffer_sRGB');
   WGL_ARB_make_current_read:=CheckExtension('WGL_ARB_make_current_read');
   WGL_ARB_multisample:=CheckExtension('WGL_ARB_multisample');
   WGL_ARB_pbuffer:=CheckExtension('WGL_ARB_pbuffer');
   WGL_ARB_pixel_format:=CheckExtension('WGL_ARB_pixel_format');
   WGL_ARB_pixel_format_float:=CheckExtension('WGL_ARB_pixel_format_float');
   WGL_ARB_render_texture:=CheckExtension('WGL_ARB_render_texture');
   // Vendor/EXT wgl extensions
   WGL_ATI_pixel_format_float:=CheckExtension('WGL_ATI_pixel_format_float');
   WGL_EXT_framebuffer_sRGB := CheckExtension('WGL_EXT_framebuffer_sRGB');
   WGL_EXT_pixel_format_packed_float := CheckExtension('WGL_EXT_pixel_format_packed_float');
   WGL_EXT_swap_control:=CheckExtension('WGL_EXT_swap_control');
end;
{$ENDIF}

{$IFDEF SUPPORT_GLX}
// ReadGLXImplementationProperties
//
procedure ReadGLXImplementationProperties;
var
   Buffer: string;

   // Checks if the given Extension string is in Buffer.
   function CheckExtension(const Extension: string): Boolean;
   var
     ExtPos: Integer;
   begin
     // First find the position of the extension string as substring in Buffer.
     ExtPos := Pos(Extension, Buffer);
     Result := ExtPos > 0;
     // Now check that it isn't only a substring of another extension.
     if Result then
       Result := ((ExtPos + Length(Extension) - 1)= Length(Buffer))
                 or (Buffer[ExtPos + Length(Extension)]=' ');
   end;

begin
   // This procedure will probably need changing, as totally untested
   // This might only work if GLX functions/procedures are loaded dynamically
   if Assigned(glXQueryExtensionsString) then
     Buffer := glXQueryExtensionsString(glXGetCurrentDisplay(), 0)  //guess at a valid screen
   else
     Buffer:='';
   // ARB GLX extensions
   GLX_ARB_create_context := CheckExtension('GLX_ARB_create_context');
   GLX_ARB_create_context_profile := CheckExtension('GLX_ARB_create_context_profile');
   GLX_ARB_framebuffer_sRGB := CheckExtension('GLX_ARB_framebuffer_sRGB');
   // EXT/vendor GLX extensions
   GLX_EXT_framebuffer_sRGB := CheckExtension('GLX_EXT_framebuffer_sRGB');
   GLX_EXT_fbconfig_packed_float := CheckExtension('GLX_EXT_fbconfig_packed_float');
   GLX_SGI_swap_control := CheckExtension('GLX_SGI_swap_control');
end;
{$ENDIF}

// CloseOpenGL
//
procedure CloseOpenGL;
begin
   if GLHandle<>INVALID_MODULEHANDLE then begin
      FreeLibrary(TLibHandle(GLHandle));
      GLHandle:=INVALID_MODULEHANDLE;
   end;

   if GLUHandle<>INVALID_MODULEHANDLE then begin
      FreeLibrary(TLibHandle(GLUHandle));
      GLUHandle:=INVALID_MODULEHANDLE;
   end;
end;

// InitOpenGL
//
function InitOpenGL : Boolean;
begin
   if (GLHandle=INVALID_MODULEHANDLE) or (GLUHandle=INVALID_MODULEHANDLE) then
      Result:=InitOpenGLFromLibrary(opengl32, glu32)
   else Result:=True;
end;

// InitOpenGLFromLibrary
//
function InitOpenGLFromLibrary(const GLName, GLUName : String) : Boolean;
begin
   Result := False;
   CloseOpenGL;

   GLHandle:=LoadLibrary(PChar(GLName));
   GLUHandle:=LoadLibrary(PChar(GLUName));

   {$IFDEF UNIX}   // make it work when only libGL.so.1 is installed
    {$IFnDEF DARWIN}
      if (GLHandle=INVALID_MODULEHANDLE) then
        GLHandle:=LoadLibrary(PChar(GLName+'.1'));
      if (GLUHandle=INVALID_MODULEHANDLE) then
        GLUHandle:=LoadLibrary(PChar(GLUName+'.1'));
    {$ENDIF}
   {$ENDIF}

   if (GLHandle<>INVALID_MODULEHANDLE) and (GLUHandle<>INVALID_MODULEHANDLE) then
     Result:=True
   else begin
      if GLHandle<>INVALID_MODULEHANDLE then
         FreeLibrary(TLibHandle(GLHandle));
      if GLUHandle<>INVALID_MODULEHANDLE then
         FreeLibrary(TLibHandle(GLUHandle));
   end;
end;

// IsOpenGLInitialized
//
function IsOpenGLInitialized: Boolean;
begin
   Result:=(GLHandle<>INVALID_MODULEHANDLE);
end;

// compatibility routines

// UnloadOpenGL
//
procedure UnloadOpenGL;
begin
   CloseOpenGL;
end;

// LoadOpenGL
//
function LoadOpenGL: Boolean;
begin
   Result := InitOpenGL;
end;

// LoadOpenGLFromLibrary
//
function LoadOpenGLFromLibrary(GLName, GLUName: String): Boolean;
begin
   Result := InitOpenGLFromLibrary(GLName, GLUName);
end;

// IsOpenGLLoaded
//
function IsOpenGLLoaded: Boolean;
begin
  Result := IsOpenGLInitialized();
end;

// IsMesaGL
//
function IsMesaGL : Boolean;
begin
  Result:=GLGetProcAddress('glResizeBuffersMESA')<>nil;
end;

// IsOpenGLVersionMet
//
function IsOpenGLVersionMet(MajorVersion, MinorVersion: Integer): boolean;
var
  Buffer : String;
  GLMajorVersion, GLMinorVersion: Integer;
begin
  buffer:=String(glGetString(GL_VERSION));
  TrimAndSplitVersionString(buffer, GLMajorVersion, GLMinorVersion);
  Result:=IsVersionMet(MajorVersion,MinorVersion,GLMajorVersion,GLMinorVersion);
end;


initialization

   Set8087CW($133F);

finalization

   CloseOpenGL;

end.

