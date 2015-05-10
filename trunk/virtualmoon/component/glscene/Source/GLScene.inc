//
// Heavily based on Delphi Free Stuff Include File
//                  by Brad Stowers: bstowers@pobox.com
//                  Delphi Free Stuff: http://www.delphifreestuff.com/
//
// History:
// 09/09/14 - PW - Dropped support for compilers older then Delphi7
// 10/11/12 - PW - Added support for Delphi and C++Builder with
//                 GLS_DELPHI and GLS_CPPB defines
// 02/04/10 - Yar - Added GLS_NO_INLINE option
// 13/12/09 - DaStr - Added GLS_MULTITHREAD option (thanks Controller)
// 10/11/09 - DaStr - Added Delphi 2010 support
// 07/11/09 - DaStr - Improved FPC compatibility (BugtrackerID = 2893580)
//                    Added GLS_NO_ASM option
// 12/09/07 - DaStr - Added GLS_DELPHI_OR_CPPB
// 30/03/07 - DaStr - Added GLS_ENABLE_UNSAFE_WARNINGS
// 25/03/07 - DaStr - Added UNIX and KYLIX defines
// 14/01/07 - DaStr - Added GLS_DELPHI/COMPILER_XX_DOWN defines
// 20/12/06 - DaStr - Added GLS_OPTIMIZATIONS option
// 08/03/06 - ur - Added Delphi 2006 detection
// 08/07/04 - LR - BCB5 and BCB6 detection
// 30/08/02 - EG - Delphi 7 detection
// 24/01/02 - EG - Linux/Kylix changes (Puthoon)
// 28/12/01 - EG - Added GLS_Graphics32_SUPPORT
// 27/07/01 - EG - Renamed and made GLScene-specific
// 26/06/01 - EG - Added Delphi6 stuff
//

{$DEFINE GLSCENE}

//
// Additionnal support & dependencies
//

// Activate support for Alex Denissov's Graphics32 library (http://www.g32.org)
{.$DEFINE GLS_Graphics32_SUPPORT}

// Activate combined support for Delphi and C++Builder
{$DEFINE GLS_DELPHI_OR_CPPB}

// Activate support for png images (using pngimage.pas library)
// GLS_PngImage_SUPPORT is automatically defined if Delphi>=2009 (see below)
// Also, Define "Auto" Resize To Nearest 8 Multiple Size
{.$DEFINE GLS_PngImage_SUPPORT}
{.$DEFINE GLS_PngImage_RESIZENEAREST}

// Activate support for some optimizations by disabling checks that occur very often
// Warning: it will disable some options (like ShowAxis and OptSaveGLStack)
{.$DEFINE GLS_OPTIMIZATIONS}

// Enable all "Unsafe Type" / "Unsafe Code" compiler warnings
// May be useful when debugging.
{.$DEFINE GLS_ENABLE_UNSAFE_WARNINGS}

// De-Activate assembly routines in GLVectorGeometry.pas
{.$DEFINE GEOMETRY_NO_ASM}

// De-Activate assembly routines in other GLS units.
{.$DEFINE GLS_NO_ASM}

// De-Activate inline directive in GLS units.
{.$DEFINE GLS_NO_INLINE}

// De-Activate regions in GLS units.
{.$DEFINE GLS_NO_REGIONS}

// Activate Logging
{.$DEFINE GLS_LOGGING}

// Activate OpenGL debug mode
{.$DEFINE GLS_OPENGL_DEBUG}

// Activate Multithread support
{.$DEFINE GLS_MULTITHREAD}

// Enable service rendering context, which automatically activate GLS_MULTITHREAD
// This work only on windows platform and with modern compiler (D2009 and FPC}
{$DEFINE GLS_SERVICE_CONTEXT}

// Activate support X Window System
{.$DEFINE GLS_X11_SUPPORT}

// Activate OpenGL ES
{.$DEFINE GLS_OPENGL_ES}

//
// Platform Detection
//
{$IFDEF WIN64}
  {$DEFINE GLS_NO_ASM}
  {$DEFINE GEOMETRY_NO_ASM}
{$ENDIF}

{$IFNDEF MSWINDOWS}
  {$IFDEF WIN32}
    // MSWINDOWS wasn't defined in Delphi 5, but WIN32 was
    // so need to define MSWINDOWS here to provide Delphi 5 support
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

{$IFDEF GLS_MULTITHREAD}
  {$DEFINE MULTITHREADOPENGL}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE GEOMETRY_NO_ASM}
  {$DEFINE UNIX}
{$ENDIF}

// FPC
{$IFDEF FPC}
  {$UNDEF GLS_DELPHI_OR_CPPB}
  {.$DEFINE GLS_GENERIC_PREFIX}
  {$DEFINE GEOMETRY_NO_ASM}
  {$DEFINE GLS_NO_ASM}
  {$TYPEINFO ON}
  {$INLINE ON}
  {$H+} // Enable long strings.
  {$IFDEF FPC_HAS_TYPE_EXTENDED}
    {$DEFINE GLS_PLATFORM_HAS_EXTENDED}
  {$ENDIF}
{$ENDIF}

// If it's not FPC, determine if C++ builder, or Delphi
{$IFNDEF FPC}
   // There no extended type on all non-x86 platforms.
  {.$DEFINE GLS_PLATFORM_HAS_EXTENDED}
  {$IFDEF GLS_DELPHI_OR_CPPB}
    {$DEFINE GLS_CPPB}
    // C++Builder requires hpp/obj files to use Delphi components in run-time packages.
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}

// .NET
{$IFDEF CLR}
  {$DEFINE GEOMETRY_NO_ASM}
  {$UNSAFECODE ON}
{$ENDIF}

// by default, GLX is not supported on windows, although windows implementations
// do exist
{$IFDEF MSWINDOWS}
  {$DEFINE SUPPORT_WGL}
  {$UNDEF SUPPORT_GLX}
  {$UNDEF GLS_X11_SUPPORT}
{$ENDIF}

{$IFDEF Linux}
  {$UNDEF SUPPORT_WGL}
  {$DEFINE SUPPORT_GLX}
  {$DEFINE GLS_X11_SUPPORT}
{$ENDIF}

{$IFDEF DARWIN}
  {$UNDEF SUPPORT_WGL}
  {$UNDEF SUPPORT_GLX}
  {$UNDEF GLS_X11_SUPPORT}
{$ENDIF}

{$UNDEF GLS_UNSUPPORTED}

// If it's not FPC, we assume it's some version of Delphi or CPPB so check
// them all, basically this checking goes to end of file + throws an error
// if unsupported
{$IFNDEF FPC}
// Conditional expressions were introduced in Delphi 6
{$ifdef CONDITIONALEXPRESSIONS}

  // Delphi XE7 & C++ Builder XE7
  {$IF CompilerVersion>28}
    {$DEFINE GLS_COMPILER_XE7_UP}
  {$elseif CompilerVersion=28}
    {$DEFINE GLS_COMPILER_XE7_UP}
  {$IFEND}

  // Delphi XE3 & C++ Builder XE3
  {$IF CompilerVersion>24}
    {$DEFINE GLS_COMPILER_XE3_UP}
  {$elseif CompilerVersion=24}
    {$DEFINE GLS_COMPILER_XE3_UP}
  {$IFEND}

  // Delphi XE2 & C++ Builder XE2
  {$IF CompilerVersion>23}
    {$DEFINE GLS_COMPILER_XE2_UP}
  {$elseif CompilerVersion=23}
    {$DEFINE GLS_COMPILER_XE2_UP}
  {$IFEND}

  // Delphi XE & C++ Builder XE
  {$if CompilerVersion>22}
    {$DEFINE GLS_COMPILER_XE_UP}
  {$elseif CompilerVersion=22}
    {$DEFINE GLS_COMPILER_XE_UP}
  {$ifend}

  // Delphi 2010 & C++ Builder 2010
  {$if CompilerVersion>21}
    {$DEFINE GLS_COMPILER_2010_UP}
  {$elseif CompilerVersion=21}
    {$DEFINE GLS_COMPILER_2010_UP}
  {$ifend}

  // Delphi 2009 & C++ Builder 2009
  {$if CompilerVersion>20}
    {$DEFINE GLS_COMPILER_2009_UP}
  {$elseif CompilerVersion=20}
    {$DEFINE GLS_COMPILER_2009_DOWN}
  {$elseif CompilerVersion<20}
    {$DEFINE GLS_COMPILER_2009_DOWN}
  {$ifend}

  // Delphi 7
  {$if CompilerVersion>15.0}
    {$DEFINE GLS_COMPILER_7_UP}
  {$elseif CompilerVersion=15.0}
    {$DEFINE GLS_COMPILER_7}
    {$DEFINE GLS_COMPILER_7_UP}
  {$ifend}

  // Friendly names
    {$IFDEF GLS_COMPILER_7}
      {$DEFINE GLS_DELPHI_7}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2005}
      {$DEFINE GLS_DELPHI_2005}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2007}
      {$DEFINE GLS_DELPHI_2007}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2009}
      {$DEFINE GLS_DELPHI_2009}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2010}
      {$DEFINE GLS_DELPHI_2010}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_XE}
      {$DEFINE GLS_DELPHI_XE}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_XE2}
      {$DEFINE GLS_DELPHI_XE2}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_XE3}
      {$DEFINE GLS_DELPHI_XE3}
    {$ENDIF}

  // UP Declarations
    {$IFDEF GLS_COMPILER_2005_UP}
      {$DEFINE GLS_DELPHI_2005_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2009_UP}
      {$DEFINE GLS_DELPHI_2009_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2010_UP}
      {$DEFINE GLS_DELPHI_2010_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_XE_UP}
      {$DEFINE GLS_DELPHI_XE_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_XE2_UP}
      {$DEFINE GLS_DELPHI_XE2_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_XE3_UP}
      {$DEFINE GLS_DELPHI_XE3_UP}
    {$ENDIF}

  // Down Declarations
   {$IFDEF GLS_COMPILER_2009_DOWN}
      {$DEFINE GLS_DELPHI_2009_DOWN}
   {$ENDIF}
 {$ENDIF}

// GLS_ENABLE_UNSAFE_WARNINGS.
{$IFDEF GLS_COMPILER_7_UP}
  {$IFDEF GLS_ENABLE_UNSAFE_WARNINGS}
    {$WARN UNSAFE_TYPE ON}
    {$WARN UNSAFE_CODE ON}
    {$WARN UNSAFE_CAST ON}
  {$ELSE}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$ENDIF}
{$ENDIF}

{$ENDIF FPC}

{$IFDEF GLS_UNSUPPORTED}
  Error: unsupported version, requires at least Delphi 5
{$ENDIF}

{$IFNDEF GLS_NO_INLINE}
  {$IFDEF GLS_COMPILER_2005_UP}
  {$DEFINE GLS_INLINE}

  {$ENDIF}
  {$IFDEF FPC}
  {$DEFINE GLS_INLINE}
  {$ENDIF}
{$ENDIF}

{$IFNDEF GLS_NO_REGIONS}
  {$IFDEF GLS_COMPILER_2005_UP}
  {$DEFINE GLS_REGIONS}
  {$ENDIF}
  {$IFDEF FPC}
  {$DEFINE GLS_REGIONS}
  {$ENDIF}
{$ENDIF}

// PNG is supported by Delphi since Dephi 2009
{$IFDEF GLS_DELPHI_2009_UP}
  {$DEFINE GLS_PngImage_SUPPORT}
{$ENDIF}

{$IFNDEF FPC}
  {$UNDEF GLS_SERVICE_CONTEXT}
{$ELSE}
  {$IFNDEF MSWINDOWS}
     {$UNDEF GLS_SERVICE_CONTEXT}
  {$ENDIF}
{$ENDIF}

{$IFDEF GLS_SERVICE_CONTEXT}
  {$DEFINE GLS_MULTITHREAD}
{$ENDIF}

