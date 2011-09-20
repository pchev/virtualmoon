//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLS_CUDA_API<p>

  <b>History : </b><font size=-1><ul>
  <li>07/02/11 - Yar - Added debug mode
  <li>08/04/10 - Yar - Corrected parameters of cuMemcpyDtoH, cuMemcpyAtoH
  <li>02/03/10 - Yar - Added missing constants, correct parameters of some functions
  <li>28/01/10 - Yar - Creation
  </ul></font>
}

/// *
// * Copyright 1993-2009 NVIDIA Corporation.  All rights reserved.
// *
// * NOTICE TO USER:
// *
// * This source code is subject to NVIDIA ownership rights under U.S. and
// * international Copyright laws.  Users and possessors of this source code
// * are hereby granted a nonexclusive, royalty-free license to use this code
// * in individual and commercial software.
// *
// * NVIDIA MAKES NO REPRESENTATION ABOUT THE SUITABILITY OF THIS SOURCE
// * CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR
// * IMPLIED WARRANTY OF ANY KIND.  NVIDIA DISCLAIMS ALL WARRANTIES WITH
// * REGARD TO THIS SOURCE CODE, INCLUDING ALL IMPLIED WARRANTIES OF
// * MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A PARTICULAR PURPOSE.
// * IN NO EVENT SHALL NVIDIA BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL,
// * OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
// * OF USE, DATA OR PROFITS,  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
// * OR OTHER TORTIOUS ACTION,  ARISING OUT OF OR IN CONNECTION WITH THE USE
// * OR PERFORMANCE OF THIS SOURCE CODE.
// *
// * U.S. Government End Users.   This source code is a "commercial item" as
// * that term is defined at  48 C.F.R. 2.101 (OCT 1995), consisting  of
// * "commercial computer  software"  and "commercial computer software
// * documentation" as such terms are  used in 48 C.F.R. 12.212 (SEPT 1995)
// * and is provided to the U.S. Government only as a commercial end item.
// * Consistent with 48 C.F.R.12.212 and 48 C.F.R. 227.7202-1 through
// * 227.7202-4 (JUNE 1995), all U.S. Government End Users acquire the
// * source code with only those rights set forth herein.
// *
// * Any use of this source code in individual and commercial software must
// * include, in the user documentation and internal comments to the code,
// * the above Disclaimer and U.S. Government End Users Notice.
// */

unit GLS_CUDA_API;

interface

{$I cuda.inc}

uses
{$IFDEF MSWINDOWS}Windows,{$ENDIF}
  GLCrossPlatform,
  GLS_CL_Platform;

const
  CUDAAPIDLL = 'nvcuda.dll';

type

  TCUdeviceptr = Pointer;
  /// < CUDA device pointer

  TCUdevice = Integer;
  /// < CUDA device

  PCUcontext = ^TCUcontext;

  TCUcontext = record
  end;

  /// < CUDA context
  PCUmodule = ^TCUmodule;

  TCUmodule = record
  end;

  /// < CUDA module
  PCUfunction = ^TCUfunction;

  TCUfunction = record
  end;

  /// < CUDA function
  PCUarray = ^TCUarray;

  TCUarray = record
  end;

  /// < CUDA array
  PCUtexref = ^TCUtexref;

  TCUtexref = record
  end;

  /// < CUDA texture reference
  PCUevent = ^TCUevent;

  TCUevent = record
  end;

  /// < CUDA event
  PCUstream = ^TCUstream;

  TCUstream = record
  end;

  /// < CUDA stream
  PPCUgraphicsResource = ^PCUgraphicsResource;
  PCUgraphicsResource = ^TCUgraphicsResource;

  TCUgraphicsResource = record
  end;
  /// < CUDA graphics interop resource

  // Context creation flags

  TCUctx_flags = (CU_CTX_SCHED_AUTO = 0,
    /// < Automatic scheduling
    CU_CTX_SCHED_SPIN = 1,
    /// < Set spin as default scheduling
    CU_CTX_SCHED_YIELD = 2,
    /// < Set yield as default scheduling
    CU_CTX_SCHED_MASK = 3, CU_CTX_BLOCKING_SYNC = 4,
    /// < Use blocking synchronization
    CU_CTX_MAP_HOST = 8,
    /// < Support mapped pinned allocations
    CU_CTX_FLAGS_MASK = 15);

  // Event creation flags

  TCUevent_flags = (CU_EVENT_DEFAULT = 0,
    /// < Default event flag
    CU_EVENT_BLOCKING_SYNC = 1
    /// < Event uses blocking synchronization
    );

  // Array formats

  TCUarray_format = (CU_AD_FORMAT_UNSIGNED_INT8 = $01,
    /// < Unsigned 8-bit integers
    CU_AD_FORMAT_UNSIGNED_INT16 = $02,
    /// < Unsigned 16-bit integers
    CU_AD_FORMAT_UNSIGNED_INT32 = $03,
    /// < Unsigned 32-bit integers
    CU_AD_FORMAT_SIGNED_INT8 = $08,
    /// < Signed 8-bit integers
    CU_AD_FORMAT_SIGNED_INT16 = $09,
    /// < Signed 16-bit integers
    CU_AD_FORMAT_SIGNED_INT32 = $0A,
    /// < Signed 32-bit integers
    CU_AD_FORMAT_HALF = $10,
    /// < 16-bit floating point
    CU_AD_FORMAT_FLOAT = $20
    /// < 32-bit floating point
    );

  // Texture reference addressing modes

  TCUaddress_mode = (CU_TR_ADDRESS_MODE_WRAP = 0,
    /// < Wrapping address mode
    CU_TR_ADDRESS_MODE_CLAMP = 1,
    /// < Clamp to edge address mode
    CU_TR_ADDRESS_MODE_MIRROR = 2
    /// < Mirror address mode
    );

  // Texture reference filtering modes

  TCUfilter_mode = (CU_TR_FILTER_MODE_POINT = 0,
    /// < Point filter mode
    CU_TR_FILTER_MODE_LINEAR = 1
    /// < Linear filter mode
    );

  // Device properties

  TCUdevice_attribute = (CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK = 1,
    /// < Maximum number of threads per block
    CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X = 2,
    /// < Maximum block dimension X
    CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Y = 3,
    /// < Maximum block dimension Y
    CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Z = 4,
    /// < Maximum block dimension Z
    CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X = 5,
    /// < Maximum grid dimension X
    CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Y = 6,
    /// < Maximum grid dimension Y
    CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Z = 7,
    /// < Maximum grid dimension Z
    CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK = 8,
    /// < Maximum shared memory available per block in bytes
    CU_DEVICE_ATTRIBUTE_SHARED_MEMORY_PER_BLOCK = 8,
    /// < Deprecated, use CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK
    CU_DEVICE_ATTRIBUTE_TOTAL_CONSTANT_MEMORY = 9,
    /// < Memory available on device for __constant__ variables in a CUDA C kernel in bytes
    CU_DEVICE_ATTRIBUTE_WARP_SIZE = 10,
    /// < Warp size in threads
    CU_DEVICE_ATTRIBUTE_MAX_PITCH = 11,
    /// < Maximum pitch in bytes allowed by memory copies
    CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK = 12,
    /// < Maximum number of 32-bit registers available per block
    CU_DEVICE_ATTRIBUTE_REGISTERS_PER_BLOCK = 12,
    /// < Deprecated, use CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK
    CU_DEVICE_ATTRIBUTE_CLOCK_RATE = 13,
    /// < Peak clock frequency in kilohertz
    CU_DEVICE_ATTRIBUTE_TEXTURE_ALIGNMENT = 14,
    /// < Alignment requirement for textures

    CU_DEVICE_ATTRIBUTE_GPU_OVERLAP = 15,
    /// < Device can possibly copy memory and execute a kernel concurrently
    CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT = 16,
    /// < Number of multiprocessors on device
    CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT = 17,
    /// < Specifies whether there is a run time limit on kernels
    CU_DEVICE_ATTRIBUTE_INTEGRATED = 18,
    /// < Device is integrated with host memory
    CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY = 19,
    /// < Device can map host memory into CUDA address space
    CU_DEVICE_ATTRIBUTE_COMPUTE_MODE = 20
    /// < Compute mode (See ::CUcomputemode for details)
    );

  (* *
    * CUDA Limits
  *)
  TcudaLimit = (cudaLimitStackSize = $00,
    /// < GPU thread stack size
    cudaLimitPrintfFifoSize = $01
    /// < GPU printf FIFO size
    );

  // Legacy device properties

  TCUdevprop = record
    maxThreadsPerBlock: Integer;
    /// < Maximum number of threads per block
    maxThreadsDim: array [0 .. 2] of Integer;
    /// < Maximum size of each dimension of a block
    maxGridSize: array [0 .. 2] of Integer;
    /// < Maximum size of each dimension of a grid
    sharedMemPerBlock: Integer;
    /// < Shared memory available per block in bytes
    totalConstantMemory: Integer;
    /// < Constant memory available on device in bytes
    SIMDWidth: Integer;
    /// < Warp size in threads
    memPitch: Integer;
    /// < Maximum pitch in bytes allowed by memory copies
    regsPerBlock: Integer;
    /// < 32-bit registers available per block
    clockRate: Integer;
    /// < Clock frequency in kilohertz
    textureAlign: Integer;
    /// < Alignment requirement for textures
  end;

  // Function properties

  TCUfunction_attribute = (

    { * The number of threads beyond which a launch of the function would fail.
      * This number depends on both the function and the device on which the
      * function is currently loaded. }

    CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK = 0,

    { * The size in bytes of statically-allocated shared memory required by
      * this function. This does not include dynamically-allocated shared
      * memory requested by the user at runtime. }

    CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES = 1,

    { * The size in bytes of user-allocated constant memory required by this
      * function. }

    CU_FUNC_ATTRIBUTE_CONST_SIZE_BYTES = 2,

    { * The size in bytes of thread local memory used by this function. }

    CU_FUNC_ATTRIBUTE_LOCAL_SIZE_BYTES = 3,

    { * The number of registers used by each thread of this function. }

    CU_FUNC_ATTRIBUTE_NUM_REGS = 4,

    CU_FUNC_ATTRIBUTE_MAX);

  // Memory types

  TCUmemorytype = (CU_MEMORYTYPE_HOST = $01,
    /// < Host memory
    CU_MEMORYTYPE_DEVICE = $02,
    /// < Device memory
    CU_MEMORYTYPE_ARRAY = $03
    /// < Array memory
    );

  // Compute Modes

  TCUcomputemode = (CU_COMPUTEMODE_DEFAULT = 0,
    /// < Default compute mode (Multiple contexts allowed per device)
    CU_COMPUTEMODE_EXCLUSIVE = 1,
    /// < Compute-exclusive mode (Only one context can be present on this device at a time)
    CU_COMPUTEMODE_PROHIBITED = 2
    /// < Compute-prohibited mode (No contexts can be created on this device at this time)
    );

  // Online compiler options

  TCUjit_option = (
    { * Max number of registers that a thread may use. }

    CU_JIT_MAX_REGISTERS = 0,

    { * IN: Specifies minimum number of threads per block to target compilation
      * for\n
      * OUT: Returns the number of threads the compiler actually targeted.
      * This restricts the resource utilization fo the compiler (e.g. max
      * registers) such that a block with the given number of threads should be
      * able to launch based on register limitations. Note, this option does not
      * currently take into account any other resource limitations, such as
      * shared memory utilization. }

    CU_JIT_THREADS_PER_BLOCK,

    { * Returns a float value in the option of the wall clock time, in
      * milliseconds, spent creating the cubin }

    CU_JIT_WALL_TIME,

    { * Pointer to a buffer in which to print any log messsages from PTXAS
      * that are informational in nature }

    CU_JIT_INFO_LOG_BUFFER,

    { * IN: Log buffer size in bytes.  Log messages will be capped at this size
      * (including null terminator)\n
      * OUT: Amount of log buffer filled with messages }

    CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES,

    { * Pointer to a buffer in which to print any log messages from PTXAS that
      * reflect errors }

    CU_JIT_ERROR_LOG_BUFFER,

    { * IN: Log buffer size in bytes.  Log messages will be capped at this size
      * (including null terminator)\n
      * OUT: Amount of log buffer filled with messages }

    CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES,

    { * Level of optimizations to apply to generated code (0 - 4), with 4
      * being the default and highest level of optimizations. }

    CU_JIT_OPTIMIZATION_LEVEL,

    { * No option value required. Determines the target based on the current
      * attached context (default) }

    CU_JIT_TARGET_FROM_CUCONTEXT,

    { * Target is chosen based on supplied CUjit_target_enum. }

    CU_JIT_TARGET,

    { * Specifies choice of fallback strategy if matching cubin is not found.
      * Choice is based on supplied CUjit_fallback_enum. }

    CU_JIT_FALLBACK_STRATEGY

    );

  // Online compilation targets

  TCUjit_target = (

    CU_TARGET_COMPUTE_10 = 0,
    /// < Compute device class 1.0
    CU_TARGET_COMPUTE_11,
    /// < Compute device class 1.1
    CU_TARGET_COMPUTE_12,
    /// < Compute device class 1.2
    CU_TARGET_COMPUTE_13
    /// < Compute device class 1.3
    );

  // Cubin matching fallback strategies

  TCUjit_fallback = (
    // ** Prefer to compile ptx */
    CU_PREFER_PTX = 0,
    // ** Prefer to fall back to compatible binary code */
    CU_PREFER_BINARY);

  // Flags to register a graphics resource

  TCUgraphicsRegisterFlags = (CU_GRAPHICS_REGISTER_FLAGS_NONE = $00000000);

  // Flags for mapping and unmapping interop resources

  TCUgraphicsMapResourceFlags =
    (CU_GRAPHICS_MAP_RESOURCE_FLAGS_NONE = $00000000,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_READ_ONLY = $00000001,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_WRITE_DISCARD = $00000002);

  // Array indices for cube faces

  TCUarray_cubemap_face = (CU_CUBEMAP_FACE_POSITIVE_X = $00000000,
    /// < Positive X face of cubemap
    CU_CUBEMAP_FACE_NEGATIVE_X = $00000001,
    /// < Negative X face of cubemap
    CU_CUBEMAP_FACE_POSITIVE_Y = $00000002,
    /// < Positive Y face of cubemap
    CU_CUBEMAP_FACE_NEGATIVE_Y = $00000003,
    /// < Negative Y face of cubemap
    CU_CUBEMAP_FACE_POSITIVE_Z = $00000004,
    /// < Positive Z face of cubemap
    CU_CUBEMAP_FACE_NEGATIVE_Z = $00000005
    /// < Negative Z face of cubemap
    );

  (*
    * CUDA function attributes
  *)

  TcudaFuncAttributes = record
    sharedSizeBytes: TSize_t;
    /// < Size of shared memory in bytes
    constSizeBytes: TSize_t;
    /// < Size of constant memory in bytes
    localSizeBytes: TSize_t;
    /// < Size of local memory in bytes
    maxThreadsPerBlock: Integer;
    /// < Maximum number of threads per block
    numRegs: Integer;
    /// < Number of registers used
    (* \brief PTX virtual architecture version for which the function was
      *  compiled. This value is the major PTX version * 10 + the minor PTX
      *  version, so a PTX version 1.3 function would return the value 13.
      *  For device emulation kernels, this is set to 9999.
    *)
    ptxVersion: Integer;
    (* * \brief Binary architecture version for which the function was compiled.
      *  This value is the major binary version * 10 + the minor binary version,
      *  so a binary version 1.3 function would return the value 13.
      *  For device emulation kernels, this is set to 9999.
    *)
    binaryVersion: Integer;
    __cudaReserved: array [0 .. 5] of Integer;
  end;

  (* *
    * CUDA function cache configurations
  *)

  TcudaFuncCache = (cudaFuncCachePreferNone = 0,
    /// < Default function cache configuration, no preference
    cudaFuncCachePreferShared = 1,
    /// < Prefer larger shared memory and smaller L1 cache
    cudaFuncCachePreferL1 = 2
    /// < Prefer larger L1 cache and smaller shared memory
    );

  // ************************************
  // **
  // **    Error codes
  // **
  // ***********************************/

  // Error codes

  TCUresult = type Cardinal;

const
  CUDA_SUCCESS: TCUresult = 0;
  /// < No errors
  CUDA_ERROR_INVALID_VALUE = 1;
  /// < Invalid value
  CUDA_ERROR_OUT_OF_MEMORY = 2;
  /// < Out of memory
  CUDA_ERROR_NOT_INITIALIZED = 3;
  /// < Driver not initialized
  CUDA_ERROR_DEINITIALIZED = 4;
  /// < Driver deinitialized

  CUDA_ERROR_NO_DEVICE = 100;
  /// < No CUDA-capable device available
  CUDA_ERROR_INVALID_DEVICE = 101;
  /// < Invalid device

  CUDA_ERROR_INVALID_IMAGE = 200;
  /// < Invalid kernel image
  CUDA_ERROR_INVALID_CONTEXT = 201;
  /// < Invalid context
  CUDA_ERROR_CONTEXT_ALREADY_CURRENT = 202;
  /// < Context already current
  CUDA_ERROR_MAP_FAILED = 205;
  /// < Map failed
  CUDA_ERROR_UNMAP_FAILED = 206;
  /// < Unmap failed
  CUDA_ERROR_ARRAY_IS_MAPPED = 207;
  /// < Array is mapped
  CUDA_ERROR_ALREADY_MAPPED = 208;
  /// < Already mapped
  CUDA_ERROR_NO_BINARY_FOR_GPU = 209;
  /// < No binary for GPU
  CUDA_ERROR_ALREADY_ACQUIRED = 210;
  /// < Already acquired
  CUDA_ERROR_NOT_MAPPED = 211;
  /// < Not mapped
  CUDA_ERROR_NOT_MAPPED_AS_ARRAY = 212;
  /// < Mapped resource not available for access as an array
  CUDA_ERROR_NOT_MAPPED_AS_POINTER = 213;
  /// < Mapped resource not available for access as a pointer

  CUDA_ERROR_INVALID_SOURCE = 300;
  /// < Invalid source
  CUDA_ERROR_FILE_NOT_FOUND = 301;
  /// < File not found

  CUDA_ERROR_INVALID_HANDLE = 400;
  /// < Invalid handle

  CUDA_ERROR_NOT_FOUND = 500;
  /// < Not found

  CUDA_ERROR_NOT_READY = 600;
  /// < CUDA not ready

  CUDA_ERROR_LAUNCH_FAILED = 700;
  /// < Launch failed
  CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES = 701;
  /// < Launch exceeded resources
  CUDA_ERROR_LAUNCH_TIMEOUT = 702;
  /// < Launch exceeded timeout
  CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING = 703;
  /// < Launch with incompatible texturing

  CUDA_ERROR_POINTER_IS_64BIT = 800;
  /// < Attempted to retrieve 64-bit pointer via 32-bit API function
  CUDA_ERROR_SIZE_IS_64BIT = 801;
  /// < Attempted to retrieve 64-bit size via 32-bit API function

  CUDA_ERROR_UNKNOWN = 999;
  /// < Unknown error

const

  { * If set, host memory is portable between CUDA contexts.
    * Flag for ::cuMemHostAlloc() }

  CU_MEMHOSTALLOC_PORTABLE = $01;

  { * If set, host memory is mapped into CUDA address space and
    * ::cuMemHostGetDevicePointer() may be called on the host pointer.
    * Flag for ::cuMemHostAlloc() }

  CU_MEMHOSTALLOC_DEVICEMAP = $02;

  { * If set, host memory is allocated as write-combined - fast to write,
    * faster to DMA, slow to read except via SSE4 streaming load instruction
    * (MOVNTDQA).
    * Flag for ::cuMemHostAlloc() }

  CU_MEMHOSTALLOC_WRITECOMBINED = $04;

  // 2D memory copy parameters

type

  PCUDA_MEMCPY2D = ^TCUDA_MEMCPY2D;

  TCUDA_MEMCPY2D = record
    srcXInBytes,
    /// < Source X in bytes
    srcY: Cardinal;
    /// < Source Y

    srcMemoryType: TCUmemorytype;
    /// < Source memory type (host, device, array)
    srcHost: Pointer;
    /// < Source host pointer
    srcDevice: TCUdeviceptr;
    /// < Source device pointer
    srcArray: PCUarray;
    /// < Source array reference
    srcPitch: Cardinal;
    /// < Source pitch (ignored when src is array)

    dstXInBytes,
    /// < Destination X in bytes
    dstY: Cardinal;
    /// < Destination Y
    dstMemoryType: TCUmemorytype;
    /// < Destination memory type (host, device, array)
    dstHost: Pointer;
    /// < Destination host pointer
    dstDevice: TCUdeviceptr;
    /// < Destination device pointer
    dstArray: PCUarray;
    /// < Destination array reference
    dstPitch: Cardinal;
    /// < Destination pitch (ignored when dst is array)

    WidthInBytes: Cardinal;
    /// < Width of 2D memory copy in bytes
    Height: Cardinal;
    /// < Height of 2D memory copy
  end;

  // 3D memory copy parameters

  TCUDA_MEMCPY3D = record

    srcXInBytes,
    /// < Source X in bytes
    srcY,
    /// < Source Y
    srcZ: Cardinal;
    /// < Source Z
    srcLOD: Cardinal;
    /// < Source LOD
    srcMemoryType: TCUmemorytype;
    /// < Source memory type (host, device, array)
    srcHost: Pointer;
    /// < Source host pointer
    srcDevice: TCUdeviceptr;
    /// < Source device pointer
    srcArray: PCUarray;
    /// < Source array reference
    reserved0: Pointer;
    /// < Must be NULL
    srcPitch: Cardinal;
    /// < Source pitch (ignored when src is array)
    srcHeight: Cardinal;
    /// < Source height (ignored when src is array; may be 0 if Depth==1)

    dstXInBytes,
    /// < Destination X in bytes
    dstY,
    /// < Destination Y
    dstZ: Cardinal;
    /// < Destination Z
    dstLOD: Cardinal;
    /// < Destination LOD
    dstMemoryType: TCUmemorytype;
    /// < Destination memory type (host, device, array)
    dstHost: Pointer;
    /// < Destination host pointer
    dstDevice: TCUdeviceptr;
    /// < Destination device pointer
    dstArray: PCUarray;
    /// < Destination array reference
    reserved1: Pointer;
    /// < Must be NULL
    dstPitch: Cardinal;
    /// < Destination pitch (ignored when dst is array)
    dstHeight: Cardinal;
    /// < Destination height (ignored when dst is array; may be 0 if Depth==1)

    WidthInBytes: Cardinal;
    /// < Width of 3D memory copy in bytes
    Height: Cardinal;
    /// < Height of 3D memory copy
    Depth: Cardinal;
    /// < Depth of 3D memory copy
  end;

  // Array descriptor

  PCUDA_ARRAY_DESCRIPTOR = ^TCUDA_ARRAY_DESCRIPTOR;

  TCUDA_ARRAY_DESCRIPTOR = record
    Width: Cardinal;
    /// < Width of array
    Height: Cardinal;
    /// < Height of array
    Format: TCUarray_format;
    /// < Array format
    NumChannels: Cardinal;
    /// < Channels per array element
  end;

  // 3D array descriptor

  TCUDA_ARRAY3D_DESCRIPTOR = record
    Width: Cardinal;
    /// < Width of 3D array
    Height: Cardinal;
    /// < Height of 3D array
    Depth: Cardinal;
    /// < Depth of 3D array
    Format: TCUarray_format;
    /// < Array format
    NumChannels: Cardinal;
    /// < Channels per array element
    Flags: Cardinal;
    /// < Flags
  end;

  // Flags to map or unmap a resource

  TCUGLmap_flags = (CU_GL_MAP_RESOURCE_FLAGS_NONE,
    CU_GL_MAP_RESOURCE_FLAGS_READ_ONLY, CU_GL_MAP_RESOURCE_FLAGS_WRITE_DISCARD);

const
  { * Override the texref format with a format inferred from the array.
    * Flag for ::cuTexRefSetArray() }

  CU_TRSA_OVERRIDE_FORMAT = $01;

  { * Read the texture as integers rather than promoting the values to floats
    * in the range [0,1].
    * Flag for ::cuTexRefSetFlags() }

  CU_TRSF_READ_AS_INTEGER = $01;

  { * Use normalized texture coordinates in the range [0,1) instead of [0,dim).
    * Flag for ::cuTexRefSetFlags() }

  CU_TRSF_NORMALIZED_COORDINATES = $02;

  { * For texture references loaded into the module, use default texunit from
    * texture reference. }

  CU_PARAM_TR_DEFAULT = -1;

type
  TDim3 = array [0 .. 2] of LongWord;

{$IFDEF MSWINDOWS}

type
  HGPUNV = Pointer;
{$ENDIF}

type
  TcuInit = function(Flags: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuDriverGetVersion = function(out driverVersion: Integer): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuDeviceGet = function(var device: TCUdevice; ordinal: Integer): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuDeviceGetCount = function(var count: Integer): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuDeviceGetName = function(name: PAnsiChar; len: Integer; dev: TCUdevice)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuDeviceComputeCapability = function(var major: Integer; var minor: Integer;
    dev: TCUdevice): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuDeviceTotalMem = function(bytes: PSize_t; dev: TCUdevice): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuDeviceGetProperties = function(var prop: TCUdevprop; dev: TCUdevice)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuDeviceGetAttribute = function(pi: PSize_t; attrib: TCUdevice_attribute;
    dev: TCUdevice): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuCtxCreate = function(var pctx: PCUcontext; Flags: Cardinal; dev: TCUdevice)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuCtxDestroy = function(ctx: PCUcontext): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuCtxAttach = function(var pctx: PCUcontext; Flags: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuCtxDetach = function(ctx: PCUcontext): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuCtxPushCurrent = function(ctx: PCUcontext): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuCtxPopCurrent = function(var pctx: PCUcontext): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuCtxGetDevice = function(var device: TCUdevice): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuCtxSynchronize = function: TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuModuleLoad = function(var module: PCUmodule; const fname: PAnsiChar)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuModuleLoadData = function(var module: PCUmodule; const image: PAnsiChar)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuModuleLoadDataEx = function(var module: PCUmodule; var image;
    numOptions: Cardinal; var options: TCUjit_option; var optionValues)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuModuleLoadFatBinary = function(var module: PCUmodule; var fatCubin)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuModuleUnload = function(hmod: PCUmodule): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuModuleGetFunction = function(out hfunc: PCUfunction; hmod: PCUmodule;
    const name: PAnsiChar): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuModuleGetGlobal = function(out dptr: TCUdeviceptr; var bytes: Cardinal;
    hmod: PCUmodule; const name: PAnsiChar): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuModuleGetTexRef = function(out pTexRef: PCUtexref; hmod: PCUmodule;
    const name: PAnsiChar): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemGetInfo = function(var free: Cardinal; var total: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemAlloc = function(var dptr: TCUdeviceptr; bytesize: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemAllocPitch = function(var dptr: TCUdeviceptr; var pPitch: Cardinal;
    WidthInBytes: Cardinal; Height: Cardinal; ElementSizeBytes: Cardinal)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemFree = function(dptr: TCUdeviceptr): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemGetAddressRange = function(var pbase: TCUdeviceptr; var psize: Cardinal;
    dptr: TCUdeviceptr): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemAllocHost = function(var pp; bytesize: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemFreeHost = function(p: Pointer): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemHostAlloc = function(var pp: Pointer; bytesize: Cardinal; Flags: Cardinal)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemHostGetDevicePointer = function(var pdptr: TCUdeviceptr; p: Pointer;
    Flags: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemHostGetFlags = function(var pFlags: Cardinal; var p): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpyHtoD = function(dstDevice: TCUdeviceptr; const srcHost: Pointer;
    ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpyDtoH = function(const dstHost: Pointer; srcDevice: TCUdeviceptr;
    ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpyDtoD = function(dstDevice: TCUdeviceptr; srcDevice: TCUdeviceptr;
    ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpyDtoDAsync = function(dstDevice: TCUdeviceptr;
    srcDevice: TCUdeviceptr; ByteCount: Cardinal; hStream: PCUstream)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpyDtoA = function(dstArray: PCUarray; dstIndex: Cardinal;
    srcDevice: TCUdeviceptr; ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpyAtoD = function(dstDevice: TCUdeviceptr; hSrc: PCUarray;
    SrcIndex: Cardinal; ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpyHtoA = function(dstArray: PCUarray; dstIndex: Cardinal;
    pSrc: Pointer; ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpyAtoH = function(dstHost: Pointer; srcArray: PCUarray;
    SrcIndex: Cardinal; ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpyAtoA = function(dstArray: PCUarray; dstIndex: Cardinal;
    srcArray: PCUarray; SrcIndex: Cardinal; ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpy2D = function(const pCopy: PCUDA_MEMCPY2D): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpy2DUnaligned = function(var pCopy: TCUDA_MEMCPY2D): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpy3D = function(var pCopy: TCUDA_MEMCPY3D): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpyHtoDAsync = function(dstDevice: TCUdeviceptr; var srcHost;
    ByteCount: Cardinal; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpyDtoHAsync = function(var dstHost; srcDevice: TCUdeviceptr;
    ByteCount: Cardinal; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpyHtoAAsync = function(dstArray: PCUarray; dstIndex: Cardinal;
    var pSrc; ByteCount: Cardinal; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpyAtoHAsync = function(var dstHost; srcArray: PCUstream;
    SrcIndex: Cardinal; ByteCount: Cardinal; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpy2DAsync = function(var pCopy: TCUDA_MEMCPY2D; hStream: PCUstream)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemcpy3DAsync = function(var pCopy: TCUDA_MEMCPY3D; hStream: PCUstream)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemsetD8 = function(dstDevice: TCUdeviceptr; ub: Byte; N: Cardinal)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemsetD16 = function(dstDevice: TCUdeviceptr; uw: Word; N: Cardinal)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemsetD32 = function(dstDevice: TCUdeviceptr; ui: Cardinal; N: Cardinal)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemsetD2D8 = function(dstDevice: TCUdeviceptr; dstPitch: Cardinal;
    ub: Byte; Width: Cardinal; Height: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemsetD2D16 = function(dstDevice: TCUdeviceptr; dstPitch: Cardinal;
    uw: Word; Width: Cardinal; Height: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuMemsetD2D32 = function(dstDevice: TCUdeviceptr; dstPitch: Cardinal;
    ui: Cardinal; Width: Cardinal; Height: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuFuncSetBlockShape = function(hfunc: PCUfunction; x: Integer; y: Integer;
    z: Integer): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuFuncSetSharedSize = function(hfunc: PCUfunction; bytes: Cardinal)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuFuncGetAttribute = function(var pi: Integer; attrib: TCUfunction_attribute;
    hfunc: PCUfunction): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuArrayCreate = function(var pHandle: PCUarray;
    var pAllocateArray: TCUDA_ARRAY_DESCRIPTOR): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuArrayGetDescriptor = function(var pArrayDescriptor: TCUDA_ARRAY_DESCRIPTOR;
    hArray: PCUarray): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuArrayDestroy = function(hArray: PCUarray): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuArray3DCreate = function(var pHandle: PCUarray;
    var pAllocateArray: TCUDA_ARRAY3D_DESCRIPTOR): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuArray3DGetDescriptor = function(var pArrayDescriptor
    : TCUDA_ARRAY3D_DESCRIPTOR; hArray: PCUarray): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefCreate = function(var pTexRef: PCUtexref): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefDestroy = function(hTexRef: PCUtexref): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefSetArray = function(hTexRef: PCUtexref; hArray: PCUarray;
    Flags: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefSetAddress = function(var ByteOffset: Cardinal; hTexRef: PCUtexref;
    dptr: TCUdeviceptr; bytes: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefSetAddress2D = function(hTexRef: PCUtexref;
    var desc: TCUDA_ARRAY_DESCRIPTOR; dptr: TCUdeviceptr; Pitch: Cardinal)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefSetFormat = function(hTexRef: PCUtexref; fmt: TCUarray_format;
    NumPackedComponents: Integer): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefSetAddressMode = function(hTexRef: PCUtexref; dim: Integer;
    am: TCUaddress_mode): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefSetFilterMode = function(hTexRef: PCUtexref; fm: TCUfilter_mode)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefSetFlags = function(hTexRef: PCUtexref; Flags: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefGetAddress = function(var pdptr: TCUdeviceptr; hTexRef: PCUtexref)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefGetArray = function(var phArray: PCUarray; hTexRef: PCUtexref)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefGetAddressMode = function(var pam: TCUaddress_mode;
    hTexRef: PCUtexref; dim: Integer): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefGetFilterMode = function(var pfm: TCUfilter_mode; hTexRef: PCUtexref)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefGetFormat = function(var pFormat: TCUarray_format;
    var pNumChannels: Integer; hTexRef: PCUtexref): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuTexRefGetFlags = function(var pFlags: Cardinal; hTexRef: PCUtexref)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuParamSetSize = function(hfunc: PCUfunction; numbytes: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuParamSeti = function(hfunc: PCUfunction; offset: Integer; value: Cardinal)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuParamSetf = function(hfunc: PCUfunction; offset: Integer; value: Single)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuParamSetv = function(hfunc: PCUfunction; offset: Integer; var ptr;
    numbytes: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuParamSetTexRef = function(hfunc: PCUfunction; texunit: Integer;
    hTexRef: PCUtexref): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuLaunch = function(f: PCUfunction): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuLaunchGrid = function(f: PCUfunction; grid_width: Integer;
    grid_height: Integer): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuLaunchGridAsync = function(f: PCUfunction; grid_width: Integer;
    grid_height: Integer; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuEventCreate = function(var phEvent: PCUevent; Flags: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuEventRecord = function(hEvent: PCUevent; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuEventQuery = function(hEvent: PCUevent): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuEventSynchronize = function(hEvent: PCUevent): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuEventDestroy = function(hEvent: PCUevent): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuEventElapsedTime = function(var pMilliseconds: Single; hStart: PCUevent;
    hEnd: PCUevent): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuStreamCreate = function(var phStream: PCUstream; Flags: Cardinal)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuStreamQuery = function(hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuStreamSynchronize = function(hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuStreamDestroy = function(hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGLCtxCreate = function(var pctx: PCUcontext; Flags: Cardinal;
    device: TCUdevice): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGraphicsGLRegisterBuffer = function(var pCudaResource: PCUgraphicsResource;
    buffer: Cardinal; Flags: TCUgraphicsMapResourceFlags): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGraphicsGLRegisterImage = function(var pCudaResource: PCUgraphicsResource;
    image, target: Cardinal; Flags: TCUgraphicsMapResourceFlags): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuWGLGetDevice = function(var pDevice: TCUdevice; hGpu: HGPUNV): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGraphicsUnregisterResource = function(resource: PCUgraphicsResource)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGraphicsSubResourceGetMappedArray = function(var pArray: PCUarray;
    resource: PCUgraphicsResource; arrayIndex: Cardinal; mipLevel: Cardinal)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGraphicsResourceGetMappedPointer = function(var pDevPtr: TCUdeviceptr;
    out psize: Cardinal; resource: PCUgraphicsResource): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGraphicsResourceSetMapFlags = function(resource: PCUgraphicsResource;
    Flags: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGraphicsMapResources = function(count: Cardinal;
    resources: PPCUgraphicsResource; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGraphicsUnmapResources = function(count: Cardinal;
    resources: PPCUgraphicsResource; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGLInit = procedure();
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGLRegisterBufferObject = function(buffer: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGLMapBufferObject = function(var dptr: TCUdeviceptr; var size: Cardinal;
    buffer: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGLUnmapBufferObject = function(buffer: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGLUnregisterBufferObject = function(buffer: Cardinal): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGLSetBufferObjectMapFlags = function(buffer: Cardinal; Flags: Cardinal)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGLMapBufferObjectAsync = function(var dptr: TCUdeviceptr;
    var size: Cardinal; buffer: Cardinal; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TcuGLUnmapBufferObjectAsync = function(buffer: Cardinal; hStream: PCUstream)
    : TCUresult;
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}

var
  cuInit: TcuInit;
  cuDriverGetVersion: TcuDriverGetVersion;
  cuDeviceGet: TcuDeviceGet;
  cuDeviceGetCount: TcuDeviceGetCount;
  cuDeviceGetName: TcuDeviceGetName;
  cuDeviceComputeCapability: TcuDeviceComputeCapability;
  cuDeviceTotalMem: TcuDeviceTotalMem;
  cuDeviceGetProperties: TcuDeviceGetProperties;
  cuDeviceGetAttribute: TcuDeviceGetAttribute;
  cuCtxCreate: TcuCtxCreate;
  cuCtxDestroy: TcuCtxDestroy;
  cuCtxAttach: TcuCtxAttach;
  cuCtxDetach: TcuCtxDetach;
  cuCtxPushCurrent: TcuCtxPushCurrent;
  cuCtxPopCurrent: TcuCtxPopCurrent;
  cuCtxGetDevice: TcuCtxGetDevice;
  cuCtxSynchronize: TcuCtxSynchronize;
  cuModuleLoad: TcuModuleLoad;
  cuModuleLoadData: TcuModuleLoadData;
  cuModuleLoadDataEx: TcuModuleLoadDataEx;
  cuModuleLoadFatBinary: TcuModuleLoadFatBinary;
  cuModuleUnload: TcuModuleUnload;
  cuModuleGetFunction: TcuModuleGetFunction;
  cuModuleGetGlobal: TcuModuleGetGlobal;
  cuModuleGetTexRef: TcuModuleGetTexRef;
  cuMemGetInfo: TcuMemGetInfo;
  cuMemAlloc: TcuMemAlloc;
  cuMemAllocPitch: TcuMemAllocPitch;
  cuMemFree: TcuMemFree;
  cuMemGetAddressRange: TcuMemGetAddressRange;
  cuMemAllocHost: TcuMemAllocHost;
  cuMemFreeHost: TcuMemFreeHost;
  cuMemHostAlloc: TcuMemHostAlloc;
  cuMemHostGetDevicePointer: TcuMemHostGetDevicePointer;
  cuMemHostGetFlags: TcuMemHostGetFlags;
  cuMemcpyHtoD: TcuMemcpyHtoD;
  cuMemcpyDtoH: TcuMemcpyDtoH;
  cuMemcpyDtoD: TcuMemcpyDtoD;
  cuMemcpyDtoDAsync: TcuMemcpyDtoDAsync;
  cuMemcpyDtoA: TcuMemcpyDtoA;
  cuMemcpyAtoD: TcuMemcpyAtoD;
  cuMemcpyHtoA: TcuMemcpyHtoA;
  cuMemcpyAtoH: TcuMemcpyAtoH;
  cuMemcpyAtoA: TcuMemcpyAtoA;
  cuMemcpy2D: TcuMemcpy2D;
  cuMemcpy2DUnaligned: TcuMemcpy2DUnaligned;
  cuMemcpy3D: TcuMemcpy3D;
  cuMemcpyHtoDAsync: TcuMemcpyHtoDAsync;
  cuMemcpyDtoHAsync: TcuMemcpyDtoHAsync;
  cuMemcpyHtoAAsync: TcuMemcpyHtoAAsync;
  cuMemcpyAtoHAsync: TcuMemcpyAtoHAsync;
  cuMemcpy2DAsync: TcuMemcpy2DAsync;
  cuMemcpy3DAsync: TcuMemcpy3DAsync;
  cuMemsetD8: TcuMemsetD8;
  cuMemsetD16: TcuMemsetD16;
  cuMemsetD32: TcuMemsetD32;
  cuMemsetD2D8: TcuMemsetD2D8;
  cuMemsetD2D16: TcuMemsetD2D16;
  cuMemsetD2D32: TcuMemsetD2D32;
  cuFuncSetBlockShape: TcuFuncSetBlockShape;
  cuFuncSetSharedSize: TcuFuncSetSharedSize;
  cuFuncGetAttribute: TcuFuncGetAttribute;
  cuArrayCreate: TcuArrayCreate;
  cuArrayGetDescriptor: TcuArrayGetDescriptor;
  cuArrayDestroy: TcuArrayDestroy;
  cuArray3DCreate: TcuArray3DCreate;
  cuArray3DGetDescriptor: TcuArray3DGetDescriptor;
  cuTexRefCreate: TcuTexRefCreate;
  cuTexRefDestroy: TcuTexRefDestroy;
  cuTexRefSetArray: TcuTexRefSetArray;
  cuTexRefSetAddress: TcuTexRefSetAddress;
  cuTexRefSetAddress2D: TcuTexRefSetAddress2D;
  cuTexRefSetFormat: TcuTexRefSetFormat;
  cuTexRefSetAddressMode: TcuTexRefSetAddressMode;
  cuTexRefSetFilterMode: TcuTexRefSetFilterMode;
  cuTexRefSetFlags: TcuTexRefSetFlags;
  cuTexRefGetAddress: TcuTexRefGetAddress;
  cuTexRefGetArray: TcuTexRefGetArray;
  cuTexRefGetAddressMode: TcuTexRefGetAddressMode;
  cuTexRefGetFilterMode: TcuTexRefGetFilterMode;
  cuTexRefGetFormat: TcuTexRefGetFormat;
  cuTexRefGetFlags: TcuTexRefGetFlags;
  cuParamSetSize: TcuParamSetSize;
  cuParamSeti: TcuParamSeti;
  cuParamSetf: TcuParamSetf;
  cuParamSetv: TcuParamSetv;
  cuParamSetTexRef: TcuParamSetTexRef;
  cuLaunch: TcuLaunch;
  cuLaunchGrid: TcuLaunchGrid;
  cuLaunchGridAsync: TcuLaunchGridAsync;
  cuEventCreate: TcuEventCreate;
  cuEventRecord: TcuEventRecord;
  cuEventQuery: TcuEventQuery;
  cuEventSynchronize: TcuEventSynchronize;
  cuEventDestroy: TcuEventDestroy;
  cuEventElapsedTime: TcuEventElapsedTime;
  cuStreamCreate: TcuStreamCreate;
  cuStreamQuery: TcuStreamQuery;
  cuStreamSynchronize: TcuStreamSynchronize;
  cuStreamDestroy: TcuStreamDestroy;
  cuGLInit: TcuGLInit;
  cuGLCtxCreate: TcuGLCtxCreate;
  cuGraphicsGLRegisterBuffer: TcuGraphicsGLRegisterBuffer;
  cuGraphicsGLRegisterImage: TcuGraphicsGLRegisterImage;
  cuWGLGetDevice: TcuWGLGetDevice;
  cuGraphicsUnregisterResource: TcuGraphicsUnregisterResource;
  cuGraphicsSubResourceGetMappedArray: TcuGraphicsSubResourceGetMappedArray;
  cuGraphicsResourceGetMappedPointer: TcuGraphicsResourceGetMappedPointer;
  cuGraphicsResourceSetMapFlags: TcuGraphicsResourceSetMapFlags;
  cuGraphicsMapResources: TcuGraphicsMapResources;
  cuGraphicsUnmapResources: TcuGraphicsUnmapResources;
  cuGLRegisterBufferObject: TcuGLRegisterBufferObject;
  cuGLMapBufferObject: TcuGLMapBufferObject;
  cuGLUnmapBufferObject: TcuGLUnmapBufferObject;
  cuGLUnregisterBufferObject: TcuGLUnregisterBufferObject;
  cuGLSetBufferObjectMapFlags: TcuGLSetBufferObjectMapFlags;
  cuGLMapBufferObjectAsync: TcuGLMapBufferObjectAsync;
  cuGLUnmapBufferObjectAsync: TcuGLUnmapBufferObjectAsync;

function InitCUDA: Boolean;
procedure CloseCUDA;
function InitCUDAFromLibrary(const LibName: WideString): Boolean;
function IsCUDAInitialized: Boolean;
function Get_CUDA_API_Error_String(AError: TCUresult): string;

implementation

uses
  GLSLog;

resourcestring
  cudasFuncRetErr = '%s return error: %s';

const
  INVALID_MODULEHANDLE = 0;

  // ************** Windows specific ********************
{$IFDEF MSWINDOWS}

var
  CUDAHandle: HINST;
{$ENDIF}
  // ************** UNIX specific ********************
{$IFDEF UNIX}

var
  CUDAHandle: TLibHandle;
{$ENDIF}

const
  cuInitName = 'cuInit';
  cuDriverGetVersionName = 'cuDriverGetVersion';
  cuDeviceGet_Name = 'cuDeviceGet';
  cuDeviceGetCountName = 'cuDeviceGetCount';
  cuDeviceGetNameName = 'cuDeviceGetName';
  cuDeviceComputeCapabilityName = 'cuDeviceComputeCapability';
  cuDeviceTotalMemName = 'cuDeviceTotalMem';
  cuDeviceGetPropertiesName = 'cuDeviceGetProperties';
  cuDeviceGetAttributeName = 'cuDeviceGetAttribute';
  cuCtxCreateName = 'cuCtxCreate';
  cuCtxDestroyName = 'cuCtxDestroy';
  cuCtxAttachName = 'cuCtxAttach';
  cuCtxDetachName = 'cuCtxDetach';
  cuCtxPushCurrentName = 'cuCtxPushCurrent';
  cuCtxPopCurrentName = 'cuCtxPopCurrent';
  cuCtxGetDeviceName = 'cuCtxGetDevice';
  cuCtxSynchronizeName = 'cuCtxSynchronize';
  cuModuleLoadName = 'cuModuleLoad';
  cuModuleLoadDataName = 'cuModuleLoadData';
  cuModuleLoadDataExName = 'cuModuleLoadDataEx';
  cuModuleLoadFatBinaryName = 'cuModuleLoadFatBinary';
  cuModuleUnloadName = 'cuModuleUnload';
  cuModuleGetFunctionName = 'cuModuleGetFunction';
  cuModuleGetGlobalName = 'cuModuleGetGlobal';
  cuModuleGetTexRefName = 'cuModuleGetTexRef';
  cuMemGetInfoName = 'cuMemGetInfo';
  cuMemAllocName = 'cuMemAlloc';
  cuMemAllocPitchName = 'cuMemAllocPitch';
  cuMemFreeName = 'cuMemFree';
  cuMemGetAddressRangeName = 'cuMemGetAddressRange';
  cuMemAllocHostName = 'cuMemAllocHost';
  cuMemFreeHostName = 'cuMemFreeHost';
  cuMemHostAllocName = 'cuMemHostAlloc';
  cuMemHostGetDevicePointerName = 'cuMemHostGetDevicePointer';
  cuMemHostGetFlagsName = 'cuMemHostGetFlags';
  cuMemcpyHtoDName = 'cuMemcpyHtoD';
  cuMemcpyDtoHName = 'cuMemcpyDtoH';
  cuMemcpyDtoDName = 'cuMemcpyDtoD';
  cuMemcpyDtoDAsyncName = 'cuMemcpyDtoDAsync';
  cuMemcpyDtoAName = 'cuMemcpyDtoA';
  cuMemcpyAtoDName = 'cuMemcpyAtoD';
  cuMemcpyHtoAName = 'cuMemcpyHtoA';
  cuMemcpyAtoHName = 'cuMemcpyAtoH';
  cuMemcpyAtoAName = 'cuMemcpyAtoA';
  cuMemcpy2DName = 'cuMemcpy2D';
  cuMemcpy2DUnalignedName = 'cuMemcpy2DUnaligned';
  cuMemcpy3DName = 'cuMemcpy3D';
  cuMemcpyHtoDAsyncName = 'cuMemcpyHtoDAsync';
  cuMemcpyDtoHAsyncName = 'cuMemcpyDtoHAsync';
  cuMemcpyHtoAAsyncName = 'cuMemcpyHtoAAsync';
  cuMemcpyAtoHAsyncName = 'cuMemcpyAtoHAsync';
  cuMemcpy2DAsyncName = 'cuMemcpy2DAsync';
  cuMemcpy3DAsyncName = 'cuMemcpy3DAsync';
  cuMemsetD8Name = 'cuMemsetD8';
  cuMemsetD16Name = 'cuMemsetD16';
  cuMemsetD32Name = 'cuMemsetD32';
  cuMemsetD2D8Name = 'cuMemsetD2D8';
  cuMemsetD2D16Name = 'cuMemsetD2D16';
  cuMemsetD2D32Name = 'cuMemsetD2D32';
  cuFuncSetBlockShapeName = 'cuFuncSetBlockShape';
  cuFuncSetSharedSizeName = 'cuFuncSetSharedSize';
  cuFuncGetAttributeName = 'cuFuncGetAttribute';
  cuArrayCreateName = 'cuArrayCreate';
  cuArrayGetDescriptorName = 'cuArrayGetDescriptor';
  cuArrayDestroyName = 'cuArrayDestroy';
  cuArray3DCreateName = 'cuArray3DCreate';
  cuArray3DGetDescriptorName = 'cuArray3DGetDescriptor';
  cuTexRefCreateName = 'cuTexRefCreate';
  cuTexRefDestroyName = 'cuTexRefDestroy';
  cuTexRefSetArrayName = 'cuTexRefSetArray';
  cuTexRefSetAddressName = 'cuTexRefSetAddress';
  cuTexRefSetAddress2DName = 'cuTexRefSetAddress2D';
  cuTexRefSetFormatName = 'cuTexRefSetFormat';
  cuTexRefSetAddressModeName = 'cuTexRefSetAddressMode';
  cuTexRefSetFilterModeName = 'cuTexRefSetFilterMode';
  cuTexRefSetFlagsName = 'cuTexRefSetFlags';
  cuTexRefGetAddressName = 'cuTexRefGetAddress';
  cuTexRefGetArrayName = 'cuTexRefGetArray';
  cuTexRefGetAddressModeName = 'cuTexRefGetAddressMode';
  cuTexRefGetFilterModeName = 'cuTexRefGetFilterMode';
  cuTexRefGetFormatName = 'cuTexRefGetFormat';
  cuTexRefGetFlagsName = 'cuTexRefGetFlags';
  cuParamSetSizeName = 'cuParamSetSize';
  cuParamSetiName = 'cuParamSeti';
  cuParamSetfName = 'cuParamSetf';
  cuParamSetvName = 'cuParamSetv';
  cuParamSetTexRefName = 'cuParamSetTexRef';
  cuLaunchName = 'cuLaunch';
  cuLaunchGridName = 'cuLaunchGrid';
  cuLaunchGridAsyncName = 'cuLaunchGridAsync';
  cuEventCreateName = 'cuEventCreate';
  cuEventRecordName = 'cuEventRecord';
  cuEventQueryName = 'cuEventQuery';
  cuEventSynchronizeName = 'cuEventSynchronize';
  cuEventDestroyName = 'cuEventDestroy';
  cuEventElapsedTimeName = 'cuEventElapsedTime';
  cuStreamCreateName = 'cuStreamCreate';
  cuStreamQueryName = 'cuStreamQuery';
  cuStreamSynchronizeName = 'cuStreamSynchronize';
  cuStreamDestroyName = 'cuStreamDestroy';
  cuGLCtxCreateName = 'cuGLCtxCreate';
  cuGraphicsGLRegisterBufferName = 'cuGraphicsGLRegisterBuffer';
  cuGraphicsGLRegisterImageName = 'cuGraphicsGLRegisterImage';
  cuWGLGetDeviceName = 'cuWGLGetDevice';
  cuGraphicsUnregisterResourceName = 'cuGraphicsUnregisterResource';
  cuGraphicsSubResourceGetMappedArrayName =
    'cuGraphicsSubResourceGetMappedArray';
  cuGraphicsResourceGetMappedPointerName = 'cuGraphicsResourceGetMappedPointer';
  cuGraphicsResourceSetMapFlagsName = 'cuGraphicsResourceSetMapFlags';
  cuGraphicsMapResourcesName = 'cuGraphicsMapResources';
  cuGraphicsUnmapResourcesName = 'cuGraphicsUnmapResources';
  cuGLInitName = 'cuGLInit';
  cuGLRegisterBufferObjectName = 'cuGLRegisterBufferObject';
  cuGLMapBufferObjectName = 'cuGLMapBufferObject';
  cuGLUnmapBufferObjectName = 'cuGLUnmapBufferObject';
  cuGLUnregisterBufferObjectName = 'cuGLUnregisterBufferObject';
  cuGLSetBufferObjectMapFlagsName = 'cuGLSetBufferObjectMapFlags';
  cuGLMapBufferObjectAsyncName = 'cuGLMapBufferObjectAsync';
  cuGLUnmapBufferObjectAsyncName = 'cuGLUnmapBufferObjectAsync';

{$IFDEF GLS_CUDA_DEBUG_MODE}

var
  cuInit_: TcuInit;
  cuDriverGetVersion_: TcuDriverGetVersion;
  cuDeviceGet_: TcuDeviceGet;
  cuDeviceGetCount_: TcuDeviceGetCount;
  cuDeviceGetName_: TcuDeviceGetName;
  cuDeviceComputeCapability_: TcuDeviceComputeCapability;
  cuDeviceTotalMem_: TcuDeviceTotalMem;
  cuDeviceGetProperties_: TcuDeviceGetProperties;
  cuDeviceGetAttribute_: TcuDeviceGetAttribute;
  cuCtxCreate_: TcuCtxCreate;
  cuCtxDestroy_: TcuCtxDestroy;
  cuCtxAttach_: TcuCtxAttach;
  cuCtxDetach_: TcuCtxDetach;
  cuCtxPushCurrent_: TcuCtxPushCurrent;
  cuCtxPopCurrent_: TcuCtxPopCurrent;
  cuCtxGetDevice_: TcuCtxGetDevice;
  cuCtxSynchronize_: TcuCtxSynchronize;
  cuModuleLoad_: TcuModuleLoad;
  cuModuleLoadData_: TcuModuleLoadData;
  cuModuleLoadDataEx_: TcuModuleLoadDataEx;
  cuModuleLoadFatBinary_: TcuModuleLoadFatBinary;
  cuModuleUnload_: TcuModuleUnload;
  cuModuleGetFunction_: TcuModuleGetFunction;
  cuModuleGetGlobal_: TcuModuleGetGlobal;
  cuModuleGetTexRef_: TcuModuleGetTexRef;
  cuMemGetInfo_: TcuMemGetInfo;
  cuMemAlloc_: TcuMemAlloc;
  cuMemAllocPitch_: TcuMemAllocPitch;
  cuMemFree_: TcuMemFree;
  cuMemGetAddressRange_: TcuMemGetAddressRange;
  cuMemAllocHost_: TcuMemAllocHost;
  cuMemFreeHost_: TcuMemFreeHost;
  cuMemHostAlloc_: TcuMemHostAlloc;
  cuMemHostGetDevicePointer_: TcuMemHostGetDevicePointer;
  cuMemHostGetFlags_: TcuMemHostGetFlags;
  cuMemcpyHtoD_: TcuMemcpyHtoD;
  cuMemcpyDtoH_: TcuMemcpyDtoH;
  cuMemcpyDtoD_: TcuMemcpyDtoD;
  cuMemcpyDtoDAsync_: TcuMemcpyDtoDAsync;
  cuMemcpyDtoA_: TcuMemcpyDtoA;
  cuMemcpyAtoD_: TcuMemcpyAtoD;
  cuMemcpyHtoA_: TcuMemcpyHtoA;
  cuMemcpyAtoH_: TcuMemcpyAtoH;
  cuMemcpyAtoA_: TcuMemcpyAtoA;
  cuMemcpy2D_: TcuMemcpy2D;
  cuMemcpy2DUnaligned_: TcuMemcpy2DUnaligned;
  cuMemcpy3D_: TcuMemcpy3D;
  cuMemcpyHtoDAsync_: TcuMemcpyHtoDAsync;
  cuMemcpyDtoHAsync_: TcuMemcpyDtoHAsync;
  cuMemcpyHtoAAsync_: TcuMemcpyHtoAAsync;
  cuMemcpyAtoHAsync_: TcuMemcpyAtoHAsync;
  cuMemcpy2DAsync_: TcuMemcpy2DAsync;
  cuMemcpy3DAsync_: TcuMemcpy3DAsync;
  cuMemsetD8_: TcuMemsetD8;
  cuMemsetD16_: TcuMemsetD16;
  cuMemsetD32_: TcuMemsetD32;
  cuMemsetD2D8_: TcuMemsetD2D8;
  cuMemsetD2D16_: TcuMemsetD2D16;
  cuMemsetD2D32_: TcuMemsetD2D32;
  cuFuncSetBlockShape_: TcuFuncSetBlockShape;
  cuFuncSetSharedSize_: TcuFuncSetSharedSize;
  cuFuncGetAttribute_: TcuFuncGetAttribute;
  cuArrayCreate_: TcuArrayCreate;
  cuArrayGetDescriptor_: TcuArrayGetDescriptor;
  cuArrayDestroy_: TcuArrayDestroy;
  cuArray3DCreate_: TcuArray3DCreate;
  cuArray3DGetDescriptor_: TcuArray3DGetDescriptor;
  cuTexRefCreate_: TcuTexRefCreate;
  cuTexRefDestroy_: TcuTexRefDestroy;
  cuTexRefSetArray_: TcuTexRefSetArray;
  cuTexRefSetAddress_: TcuTexRefSetAddress;
  cuTexRefSetAddress2D_: TcuTexRefSetAddress2D;
  cuTexRefSetFormat_: TcuTexRefSetFormat;
  cuTexRefSetAddressMode_: TcuTexRefSetAddressMode;
  cuTexRefSetFilterMode_: TcuTexRefSetFilterMode;
  cuTexRefSetFlags_: TcuTexRefSetFlags;
  cuTexRefGetAddress_: TcuTexRefGetAddress;
  cuTexRefGetArray_: TcuTexRefGetArray;
  cuTexRefGetAddressMode_: TcuTexRefGetAddressMode;
  cuTexRefGetFilterMode_: TcuTexRefGetFilterMode;
  cuTexRefGetFormat_: TcuTexRefGetFormat;
  cuTexRefGetFlags_: TcuTexRefGetFlags;
  cuParamSetSize_: TcuParamSetSize;
  cuParamSeti_: TcuParamSeti;
  cuParamSetf_: TcuParamSetf;
  cuParamSetv_: TcuParamSetv;
  cuParamSetTexRef_: TcuParamSetTexRef;
  cuLaunch_: TcuLaunch;
  cuLaunchGrid_: TcuLaunchGrid;
  cuLaunchGridAsync_: TcuLaunchGridAsync;
  cuEventCreate_: TcuEventCreate;
  cuEventRecord_: TcuEventRecord;
  cuEventQuery_: TcuEventQuery;
  cuEventSynchronize_: TcuEventSynchronize;
  cuEventDestroy_: TcuEventDestroy;
  cuEventElapsedTime_: TcuEventElapsedTime;
  cuStreamCreate_: TcuStreamCreate;
  cuStreamQuery_: TcuStreamQuery;
  cuStreamSynchronize_: TcuStreamSynchronize;
  cuStreamDestroy_: TcuStreamDestroy;
  cuGLCtxCreate_: TcuGLCtxCreate;
  cuGraphicsGLRegisterBuffer_: TcuGraphicsGLRegisterBuffer;
  cuGraphicsGLRegisterImage_: TcuGraphicsGLRegisterImage;
  cuWGLGetDevice_: TcuWGLGetDevice;
  cuGraphicsUnregisterResource_: TcuGraphicsUnregisterResource;
  cuGraphicsSubResourceGetMappedArray_: TcuGraphicsSubResourceGetMappedArray;
  cuGraphicsResourceGetMappedPointer_: TcuGraphicsResourceGetMappedPointer;
  cuGraphicsResourceSetMapFlags_: TcuGraphicsResourceSetMapFlags;
  cuGraphicsMapResources_: TcuGraphicsMapResources;
  cuGraphicsUnmapResources_: TcuGraphicsUnmapResources;
  cuGLRegisterBufferObject_: TcuGLRegisterBufferObject;
  cuGLMapBufferObject_: TcuGLMapBufferObject;
  cuGLUnmapBufferObject_: TcuGLUnmapBufferObject;
  cuGLUnregisterBufferObject_: TcuGLUnregisterBufferObject;
  cuGLSetBufferObjectMapFlags_: TcuGLSetBufferObjectMapFlags;
  cuGLMapBufferObjectAsync_: TcuGLMapBufferObjectAsync;
  cuGLUnmapBufferObjectAsync_: TcuGLUnmapBufferObjectAsync;

function cuInitShell(Flags: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuInit_(Flags);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuInitName, Get_CUDA_API_Error_String(Result)])
end;

function cuDriverGetVersionShell(out driverVersion: Integer): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuDriverGetVersion_(driverVersion);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuDriverGetVersionName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuDeviceGetShell(var device: TCUdevice; ordinal: Integer): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuDeviceGet_(device, ordinal);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuDeviceGet_Name, Get_CUDA_API_Error_String(Result)])
end;

function cuDeviceGetCountShell(var count: Integer): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuDeviceGetCount_(count);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuDeviceGetCountName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuDeviceGetNameShell(name: PAnsiChar; len: Integer; dev: TCUdevice)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuDeviceGetName_(name, len, dev);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuDeviceGetNameName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuDeviceComputeCapabilityShell(var major: Integer; var minor: Integer;
  dev: TCUdevice): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuDeviceComputeCapability_(major, minor, dev);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuDeviceComputeCapabilityName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuDeviceTotalMemShell(bytes: PSize_t; dev: TCUdevice): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuDeviceTotalMem_(bytes, dev);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuDeviceTotalMemName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuDeviceGetPropertiesShell(var prop: TCUdevprop; dev: TCUdevice)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuDeviceGetProperties_(prop, dev);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuDeviceGetPropertiesName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuDeviceGetAttributeShell(pi: PSize_t; attrib: TCUdevice_attribute;
  dev: TCUdevice): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuDeviceGetAttribute_(pi, attrib, dev);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuDeviceGetAttributeName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuCtxCreateShell(var pctx: PCUcontext; Flags: Cardinal; dev: TCUdevice)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuCtxCreate_(pctx, Flags, dev);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuCtxCreateName, Get_CUDA_API_Error_String(Result)])
end;

function cuCtxDestroyShell(ctx: PCUcontext): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuCtxDestroy_(ctx);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuCtxDestroyName, Get_CUDA_API_Error_String(Result)])
end;

function cuCtxAttachShell(var pctx: PCUcontext; Flags: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuCtxAttach_(pctx, Flags);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuCtxAttachName, Get_CUDA_API_Error_String(Result)])
end;

function cuCtxDetachShell(ctx: PCUcontext): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuCtxDetach_(ctx);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuCtxDetachName, Get_CUDA_API_Error_String(Result)])
end;

function cuCtxPushCurrentShell(ctx: PCUcontext): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuCtxPushCurrent_(ctx);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuCtxPushCurrentName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuCtxPopCurrentShell(var pctx: PCUcontext): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuCtxPopCurrent_(pctx);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuCtxPopCurrentName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuCtxGetDeviceShell(var device: TCUdevice): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuCtxGetDevice_(device);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuCtxGetDeviceName, Get_CUDA_API_Error_String(Result)])
end;

function cuCtxSynchronizeShell: TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuCtxSynchronize_;
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuCtxSynchronizeName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuModuleLoadShell(var module: PCUmodule; const fname: PAnsiChar)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuModuleLoad_(module, fname);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuModuleLoadName, Get_CUDA_API_Error_String(Result)])
end;

function cuModuleLoadDataShell(var module: PCUmodule; const image: PAnsiChar)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuModuleLoadData_(module, image);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuModuleLoadDataName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuModuleLoadDataExShell(var module: PCUmodule; var image;
  numOptions: Cardinal; var options: TCUjit_option; var optionValues)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuModuleLoadDataEx_(module, image, numOptions, options,
    optionValues);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuModuleLoadDataExName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuModuleLoadFatBinaryShell(var module: PCUmodule; var fatCubin)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuModuleLoadFatBinary_(module, fatCubin);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuModuleLoadFatBinaryName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuModuleUnloadShell(hmod: PCUmodule): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuModuleUnload_(hmod);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuModuleUnloadName, Get_CUDA_API_Error_String(Result)])
end;

function cuModuleGetFunctionShell(out hfunc: PCUfunction; hmod: PCUmodule;
  const name: PAnsiChar): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuModuleGetFunction_(hfunc, hmod, name);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuModuleGetFunctionName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuModuleGetGlobalShell(out dptr: TCUdeviceptr; var bytes: Cardinal;
  hmod: PCUmodule; const name: PAnsiChar): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuModuleGetGlobal_(dptr, bytes, hmod, name);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuModuleGetGlobalName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuModuleGetTexRefShell(out pTexRef: PCUtexref; hmod: PCUmodule;
  const name: PAnsiChar): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuModuleGetTexRef_(pTexRef, hmod, name);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuModuleGetTexRefName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuMemGetInfoShell(var free: Cardinal; var total: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemGetInfo_(free, total);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemGetInfoName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemAllocShell(var dptr: TCUdeviceptr; bytesize: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemAlloc_(dptr, bytesize);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemAllocName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemAllocPitchShell(var dptr: TCUdeviceptr; var pPitch: Cardinal;
  WidthInBytes: Cardinal; Height: Cardinal; ElementSizeBytes: Cardinal)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemAllocPitch_(dptr, pPitch, WidthInBytes, Height,
    ElementSizeBytes);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuMemAllocPitchName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuMemFreeShell(dptr: TCUdeviceptr): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemFree_(dptr);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemFreeName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemGetAddressRangeShell(var pbase: TCUdeviceptr; var psize: Cardinal;
  dptr: TCUdeviceptr): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemGetAddressRange_(pbase, psize, dptr);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuMemGetAddressRangeName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuMemAllocHostShell(var pp; bytesize: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemAllocHost_(pp, bytesize);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemAllocHostName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemFreeHostShell(p: Pointer): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemFreeHost_(p);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemFreeHostName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemHostAllocShell(var pp: Pointer; bytesize: Cardinal; Flags: Cardinal)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemHostAlloc_(pp, bytesize, Flags);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemHostAllocName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemHostGetDevicePointerShell(var pdptr: TCUdeviceptr; p: Pointer;
  Flags: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemHostGetDevicePointer_(pdptr, p, Flags);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuMemHostGetDevicePointerName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuMemHostGetFlagsShell(var pFlags: Cardinal; var p): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemHostGetFlags_(pFlags, p);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuMemHostGetFlagsName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpyHtoDShell(dstDevice: TCUdeviceptr; const srcHost: Pointer;
  ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpyHtoD_(dstDevice, srcHost, ByteCount);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemcpyHtoDName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpyDtoHShell(const dstHost: Pointer; srcDevice: TCUdeviceptr;
  ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpyDtoH_(dstHost, srcDevice, ByteCount);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemcpyDtoHName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpyDtoDShell(dstDevice: TCUdeviceptr; srcDevice: TCUdeviceptr;
  ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpyDtoD_(dstDevice, srcDevice, ByteCount);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemcpyDtoDName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpyDtoDAsyncShell(dstDevice: TCUdeviceptr;
  srcDevice: TCUdeviceptr; ByteCount: Cardinal; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpyDtoDAsync_(dstDevice, srcDevice, ByteCount, hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuMemcpyDtoDAsyncName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpyDtoAShell(dstArray: PCUarray; dstIndex: Cardinal;
  srcDevice: TCUdeviceptr; ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpyDtoA_(dstArray, dstIndex, srcDevice, ByteCount);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemcpyDtoAName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpyAtoDShell(dstDevice: TCUdeviceptr; hSrc: PCUarray;
  SrcIndex: Cardinal; ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpyAtoD_(dstDevice, hSrc, SrcIndex, ByteCount);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemcpyAtoDName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpyHtoAShell(dstArray: PCUarray; dstIndex: Cardinal;
  pSrc: Pointer; ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpyHtoA_(dstArray, dstIndex, pSrc, ByteCount);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemcpyHtoAName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpyAtoHShell(dstHost: Pointer; srcArray: PCUarray;
  SrcIndex: Cardinal; ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpyAtoH_(dstHost, srcArray, SrcIndex, ByteCount);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemcpyAtoHName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpyAtoAShell(dstArray: PCUarray; dstIndex: Cardinal;
  srcArray: PCUarray; SrcIndex: Cardinal; ByteCount: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpyAtoA_(dstArray, dstIndex, srcArray, SrcIndex, ByteCount);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemcpyAtoAName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpy2DShell(const pCopy: PCUDA_MEMCPY2D): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpy2D_(pCopy);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemcpy2DName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpy2DUnalignedShell(var pCopy: TCUDA_MEMCPY2D): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpy2DUnaligned_(pCopy);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuMemcpy2DUnalignedName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpy3DShell(var pCopy: TCUDA_MEMCPY3D): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpy3D_(pCopy);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemcpy3DName, Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpyHtoDAsyncShell(dstDevice: TCUdeviceptr; var srcHost;
  ByteCount: Cardinal; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpyHtoDAsync_(dstDevice, srcHost, ByteCount, hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuMemcpyHtoDAsyncName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpyDtoHAsyncShell(var dstHost; srcDevice: TCUdeviceptr;
  ByteCount: Cardinal; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpyDtoHAsync_(dstHost, srcDevice, ByteCount, hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuMemcpyDtoHAsyncName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpyHtoAAsyncShell(dstArray: PCUarray; dstIndex: Cardinal;
  var pSrc; ByteCount: Cardinal; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpyHtoAAsync_(dstArray, dstIndex, pSrc, ByteCount, hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuMemcpyHtoAAsyncName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpyAtoHAsyncShell(var dstHost; srcArray: PCUstream;
  SrcIndex: Cardinal; ByteCount: Cardinal; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpyAtoHAsync_(dstHost, srcArray, SrcIndex, ByteCount, hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuMemcpyAtoHAsyncName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpy2DAsyncShell(var pCopy: TCUDA_MEMCPY2D; hStream: PCUstream)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpy2DAsync_(pCopy, hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuMemcpy2DAsyncName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuMemcpy3DAsyncShell(var pCopy: TCUDA_MEMCPY3D; hStream: PCUstream)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemcpy3DAsync_(pCopy, hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuMemcpy3DAsyncName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuMemsetD8Shell(dstDevice: TCUdeviceptr; ub: Byte; N: Cardinal)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemsetD8_(dstDevice, ub, N);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemsetD8Name, Get_CUDA_API_Error_String(Result)])
end;

function cuMemsetD16Shell(dstDevice: TCUdeviceptr; uw: Word; N: Cardinal)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemsetD16_(dstDevice, uw, N);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemsetD16Name, Get_CUDA_API_Error_String(Result)])
end;

function cuMemsetD32Shell(dstDevice: TCUdeviceptr; ui: Cardinal; N: Cardinal)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemsetD32_(dstDevice, ui, N);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemsetD32Name, Get_CUDA_API_Error_String(Result)])
end;

function cuMemsetD2D8Shell(dstDevice: TCUdeviceptr; dstPitch: Cardinal;
  ub: Byte; Width: Cardinal; Height: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemsetD2D8_(dstDevice, dstPitch, ub, Width, Height);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemsetD2D8Name, Get_CUDA_API_Error_String(Result)])
end;

function cuMemsetD2D16Shell(dstDevice: TCUdeviceptr; dstPitch: Cardinal;
  uw: Word; Width: Cardinal; Height: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemsetD2D16_(dstDevice, dstPitch, uw, Width, Height);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemsetD2D16Name, Get_CUDA_API_Error_String(Result)])
end;

function cuMemsetD2D32Shell(dstDevice: TCUdeviceptr; dstPitch: Cardinal;
  ui: Cardinal; Width: Cardinal; Height: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuMemsetD2D32_(dstDevice, dstPitch, ui, Width, Height);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuMemsetD2D32Name, Get_CUDA_API_Error_String(Result)])
end;

function cuFuncSetBlockShapeShell(hfunc: PCUfunction; x: Integer; y: Integer;
  z: Integer): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuFuncSetBlockShape_(hfunc, x, y, z);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuFuncSetBlockShapeName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuFuncSetSharedSizeShell(hfunc: PCUfunction; bytes: Cardinal)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuFuncSetSharedSize_(hfunc, bytes);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuFuncSetSharedSizeName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuFuncGetAttributeShell(var pi: Integer; attrib: TCUfunction_attribute;
  hfunc: PCUfunction): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuFuncGetAttribute_(pi, attrib, hfunc);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuFuncGetAttributeName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuArrayCreateShell(var pHandle: PCUarray;
  var pAllocateArray: TCUDA_ARRAY_DESCRIPTOR): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuArrayCreate_(pHandle, pAllocateArray);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuArrayCreateName, Get_CUDA_API_Error_String(Result)])
end;

function cuArrayGetDescriptorShell(var pArrayDescriptor: TCUDA_ARRAY_DESCRIPTOR;
  hArray: PCUarray): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuArrayGetDescriptor_(pArrayDescriptor, hArray);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuArrayGetDescriptorName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuArrayDestroyShell(hArray: PCUarray): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuArrayDestroy_(hArray);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuArrayDestroyName, Get_CUDA_API_Error_String(Result)])
end;

function cuArray3DCreateShell(var pHandle: PCUarray;
  var pAllocateArray: TCUDA_ARRAY3D_DESCRIPTOR): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuArray3DCreate_(pHandle, pAllocateArray);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuArray3DCreateName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuArray3DGetDescriptorShell(var pArrayDescriptor
  : TCUDA_ARRAY3D_DESCRIPTOR; hArray: PCUarray): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuArray3DGetDescriptor_(pArrayDescriptor, hArray);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuArray3DGetDescriptorName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefCreateShell(var pTexRef: PCUtexref): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefCreate_(pTexRef);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuTexRefCreateName, Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefDestroyShell(hTexRef: PCUtexref): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefDestroy_(hTexRef);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuTexRefDestroyName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefSetArrayShell(hTexRef: PCUtexref; hArray: PCUarray;
  Flags: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefSetArray_(hTexRef, hArray, Flags);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuTexRefSetArrayName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefSetAddressShell(var ByteOffset: Cardinal; hTexRef: PCUtexref;
  dptr: TCUdeviceptr; bytes: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefSetAddress_(ByteOffset, hTexRef, dptr, bytes);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuTexRefSetAddressName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefSetAddress2DShell(hTexRef: PCUtexref;
  var desc: TCUDA_ARRAY_DESCRIPTOR; dptr: TCUdeviceptr; Pitch: Cardinal)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefSetAddress2D_(hTexRef, desc, dptr, Pitch);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuTexRefSetAddress2DName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefSetFormatShell(hTexRef: PCUtexref; fmt: TCUarray_format;
  NumPackedComponents: Integer): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefSetFormat_(hTexRef, fmt, NumPackedComponents);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuTexRefSetFormatName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefSetAddressModeShell(hTexRef: PCUtexref; dim: Integer;
  am: TCUaddress_mode): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefSetAddressMode_(hTexRef, dim, am);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuTexRefSetAddressModeName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefSetFilterModeShell(hTexRef: PCUtexref; fm: TCUfilter_mode)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefSetFilterMode_(hTexRef, fm);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuTexRefSetFilterModeName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefSetFlagsShell(hTexRef: PCUtexref; Flags: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefSetFlags_(hTexRef, Flags);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuTexRefSetFlagsName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefGetAddressShell(var pdptr: TCUdeviceptr; hTexRef: PCUtexref)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefGetAddress_(pdptr, hTexRef);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuTexRefGetAddressName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefGetArrayShell(var phArray: PCUarray; hTexRef: PCUtexref)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefGetArray_(phArray, hTexRef);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuTexRefGetArrayName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefGetAddressModeShell(var pam: TCUaddress_mode;
  hTexRef: PCUtexref; dim: Integer): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefGetAddressMode_(pam, hTexRef, dim);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuTexRefGetAddressModeName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefGetFilterModeShell(var pfm: TCUfilter_mode; hTexRef: PCUtexref)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefGetFilterMode_(pfm, hTexRef);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuTexRefGetFilterModeName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefGetFormatShell(var pFormat: TCUarray_format;
  var pNumChannels: Integer; hTexRef: PCUtexref): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefGetFormat_(pFormat, pNumChannels, hTexRef);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuTexRefGetFormatName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuTexRefGetFlagsShell(var pFlags: Cardinal; hTexRef: PCUtexref)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuTexRefGetFlags_(pFlags, hTexRef);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuTexRefGetFlagsName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuParamSetSizeShell(hfunc: PCUfunction; numbytes: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuParamSetSize_(hfunc, numbytes);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuParamSetSizeName, Get_CUDA_API_Error_String(Result)])
end;

function cuParamSetiShell(hfunc: PCUfunction; offset: Integer; value: Cardinal)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuParamSeti_(hfunc, offset, value);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuParamSetiName, Get_CUDA_API_Error_String(Result)])
end;

function cuParamSetfShell(hfunc: PCUfunction; offset: Integer; value: Single)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuParamSetf_(hfunc, offset, value);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuParamSetfName, Get_CUDA_API_Error_String(Result)])
end;

function cuParamSetvShell(hfunc: PCUfunction; offset: Integer; var ptr;
  numbytes: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuParamSetv_(hfunc, offset, ptr, numbytes);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuParamSetvName, Get_CUDA_API_Error_String(Result)])
end;

function cuParamSetTexRefShell(hfunc: PCUfunction; texunit: Integer;
  hTexRef: PCUtexref): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuParamSetTexRef_(hfunc, texunit, hTexRef);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuParamSetTexRefName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuLaunchShell(f: PCUfunction): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuLaunch_(f);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuLaunchName, Get_CUDA_API_Error_String(Result)])
end;

function cuLaunchGridShell(f: PCUfunction; grid_width: Integer;
  grid_height: Integer): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuLaunchGrid_(f, grid_width, grid_height);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuLaunchGridName, Get_CUDA_API_Error_String(Result)])
end;

function cuLaunchGridAsyncShell(f: PCUfunction; grid_width: Integer;
  grid_height: Integer; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuLaunchGridAsync_(f, grid_width, grid_height, hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuLaunchGridAsyncName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuEventCreateShell(var phEvent: PCUevent; Flags: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuEventCreate_(phEvent, Flags);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuEventCreateName, Get_CUDA_API_Error_String(Result)])
end;

function cuEventRecordShell(hEvent: PCUevent; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuEventRecord_(hEvent, hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuEventRecordName, Get_CUDA_API_Error_String(Result)])
end;

function cuEventQueryShell(hEvent: PCUevent): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuEventQuery_(hEvent);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuEventQueryName, Get_CUDA_API_Error_String(Result)])
end;

function cuEventSynchronizeShell(hEvent: PCUevent): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuEventSynchronize_(hEvent);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuEventSynchronizeName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuEventDestroyShell(hEvent: PCUevent): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuEventDestroy_(hEvent);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuEventDestroyName, Get_CUDA_API_Error_String(Result)])
end;

function cuEventElapsedTimeShell(var pMilliseconds: Single; hStart: PCUevent;
  hEnd: PCUevent): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuEventElapsedTime_(pMilliseconds, hStart, hEnd);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuEventElapsedTimeName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuStreamCreateShell(var phStream: PCUstream; Flags: Cardinal)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuStreamCreate_(phStream, Flags);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuStreamCreateName, Get_CUDA_API_Error_String(Result)])
end;

function cuStreamQueryShell(hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuStreamQuery_(hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuStreamQueryName, Get_CUDA_API_Error_String(Result)])
end;

function cuStreamSynchronizeShell(hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuStreamSynchronize_(hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuStreamSynchronizeName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuStreamDestroyShell(hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuStreamDestroy_(hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuStreamDestroyName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuGLCtxCreateShell(var pctx: PCUcontext; Flags: Cardinal;
  device: TCUdevice): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGLCtxCreate_(pctx, Flags, device);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuGLCtxCreateName, Get_CUDA_API_Error_String(Result)])
end;

function cuGraphicsGLRegisterBufferShell(var pCudaResource: PCUgraphicsResource;
  buffer: Cardinal; Flags: TCUgraphicsMapResourceFlags): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGraphicsGLRegisterBuffer_(pCudaResource, buffer, Flags);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuGraphicsGLRegisterBufferName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuGraphicsGLRegisterImageShell(var pCudaResource: PCUgraphicsResource;
  image, target: Cardinal; Flags: TCUgraphicsMapResourceFlags): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGraphicsGLRegisterImage_(pCudaResource, image, target, Flags);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuGraphicsGLRegisterImageName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuWGLGetDeviceShell(var pDevice: TCUdevice; hGpu: HGPUNV): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuWGLGetDevice_(pDevice, hGpu);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuWGLGetDeviceName, Get_CUDA_API_Error_String(Result)])
end;

function cuGraphicsUnregisterResourceShell(resource: PCUgraphicsResource)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGraphicsUnregisterResource_(resource);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuGraphicsUnregisterResourceName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuGraphicsSubResourceGetMappedArrayShell(var pArray: PCUarray;
  resource: PCUgraphicsResource; arrayIndex: Cardinal; mipLevel: Cardinal)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGraphicsSubResourceGetMappedArray_(pArray, resource, arrayIndex,
    mipLevel);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuGraphicsSubResourceGetMappedArrayName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuGraphicsResourceGetMappedPointerShell(var pDevPtr: TCUdeviceptr;
  out psize: Cardinal; resource: PCUgraphicsResource): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGraphicsResourceGetMappedPointer_(pDevPtr, psize, resource);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr,
      [cuGraphicsResourceGetMappedPointerName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuGraphicsResourceSetMapFlagsShell(resource: PCUgraphicsResource;
  Flags: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGraphicsResourceSetMapFlags_(resource, Flags);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuGraphicsResourceSetMapFlagsName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuGraphicsMapResourcesShell(count: Cardinal;
  resources: PPCUgraphicsResource; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGraphicsMapResources_(count, resources, hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuGraphicsMapResourcesName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuGraphicsUnmapResourcesShell(count: Cardinal;
  resources: PPCUgraphicsResource; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGraphicsUnmapResources_(count, resources, hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuGraphicsUnmapResourcesName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuGLRegisterBufferObjectShell(buffer: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGLRegisterBufferObject_(buffer);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuGLRegisterBufferObjectName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuGLMapBufferObjectShell(var dptr: TCUdeviceptr; var size: Cardinal;
  buffer: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGLMapBufferObject_(dptr, size, buffer);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuGLMapBufferObjectName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuGLUnmapBufferObjectShell(buffer: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGLUnmapBufferObject_(buffer);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuGLUnmapBufferObjectName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuGLUnregisterBufferObjectShell(buffer: Cardinal): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGLUnregisterBufferObject_(buffer);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuGLUnregisterBufferObjectName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuGLSetBufferObjectMapFlagsShell(buffer: Cardinal; Flags: Cardinal)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGLSetBufferObjectMapFlags_(buffer, Flags);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuGLSetBufferObjectMapFlagsName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuGLMapBufferObjectAsyncShell(var dptr: TCUdeviceptr;
  var size: Cardinal; buffer: Cardinal; hStream: PCUstream): TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGLMapBufferObjectAsync_(dptr, size, buffer, hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuGLMapBufferObjectAsyncName,
      Get_CUDA_API_Error_String(Result)])
end;

function cuGLUnmapBufferObjectAsyncShell(buffer: Cardinal; hStream: PCUstream)
  : TCUresult;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Result := cuGLUnmapBufferObjectAsync_(buffer, hStream);
  if Result <> CUDA_SUCCESS then
    GLSLogger.LogErrorFmt(cudasFuncRetErr, [cuGLUnmapBufferObjectAsyncName,
      Get_CUDA_API_Error_String(Result)])
end;

{$ENDIF GLS_CUDA_DEBUG_MODE}

function CUDAGetProcAddress(ProcName: PAnsiChar): Pointer;
var
  Alt: AnsiString;
begin
  Alt := AnsiString(ProcName) + '_v2';
  Result := GetProcAddress(PtrUInt(CUDAHandle), PAnsiChar(Alt));
  if Result = nil then
      Result := GetProcAddress(PtrUInt(CUDAHandle), ProcName);
end;

function InitCUDA: Boolean;
begin
  if CUDAHandle = INVALID_MODULEHANDLE then
    Result := InitCUDAFromLibrary(CUDAAPIDLL)
  else
    Result := True;
end;

procedure CloseCUDA;
begin
  if CUDAHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(Cardinal(CUDAHandle));
    CUDAHandle := INVALID_MODULEHANDLE;
  end;
end;

function InitCUDAFromLibrary(const LibName: WideString): Boolean;
var
  V: Integer;
begin
  Result := False;
  CloseCUDA;
  CUDAHandle := GetModuleHandleW(PWideChar(LibName));
  if CUDAHandle = INVALID_MODULEHANDLE then
    CUDAHandle := LoadLibraryW(PWideChar(LibName));

  if CUDAHandle = INVALID_MODULEHANDLE then
    Exit;
{$IFNDEF GLS_CUDA_DEBUG_MODE}
  cuInit := CUDAGetProcAddress(cuInitName);
  cuDriverGetVersion := CUDAGetProcAddress(cuDriverGetVersionName);
  cuDeviceGet := CUDAGetProcAddress(cuDeviceGet_Name);
  cuDeviceGetCount := CUDAGetProcAddress(cuDeviceGetCountName);
  cuDeviceGetName := CUDAGetProcAddress(cuDeviceGetNameName);
  cuDeviceComputeCapability := CUDAGetProcAddress
    (cuDeviceComputeCapabilityName);
  cuDeviceTotalMem := CUDAGetProcAddress(cuDeviceTotalMemName);
  cuDeviceGetProperties := CUDAGetProcAddress(cuDeviceGetPropertiesName);
  cuDeviceGetAttribute := CUDAGetProcAddress(cuDeviceGetAttributeName);
  cuCtxCreate := CUDAGetProcAddress(cuCtxCreateName);
  cuCtxDestroy := CUDAGetProcAddress(cuCtxDestroyName);
  cuCtxAttach := CUDAGetProcAddress(cuCtxAttachName);
  cuCtxDetach := CUDAGetProcAddress(cuCtxDetachName);
  cuCtxPushCurrent := CUDAGetProcAddress(cuCtxPushCurrentName);
  cuCtxPopCurrent := CUDAGetProcAddress(cuCtxPopCurrentName);
  cuCtxGetDevice := CUDAGetProcAddress(cuCtxGetDeviceName);
  cuCtxSynchronize := CUDAGetProcAddress(cuCtxSynchronizeName);
  cuModuleLoad := CUDAGetProcAddress(cuModuleLoadName);
  cuModuleLoadData := CUDAGetProcAddress(cuModuleLoadDataName);
  cuModuleLoadDataEx := CUDAGetProcAddress(cuModuleLoadDataExName);
  cuModuleLoadFatBinary := CUDAGetProcAddress(cuModuleLoadFatBinaryName);
  cuModuleUnload := CUDAGetProcAddress(cuModuleUnloadName);
  cuModuleGetFunction := CUDAGetProcAddress(cuModuleGetFunctionName);
  cuModuleGetGlobal := CUDAGetProcAddress(cuModuleGetGlobalName);
  cuModuleGetTexRef := CUDAGetProcAddress(cuModuleGetTexRefName);
  cuMemGetInfo := CUDAGetProcAddress(cuMemGetInfoName);
  cuMemAlloc := CUDAGetProcAddress(cuMemAllocName);
  cuMemAllocPitch := CUDAGetProcAddress(cuMemAllocPitchName);
  cuMemFree := CUDAGetProcAddress(cuMemFreeName);
  cuMemGetAddressRange := CUDAGetProcAddress(cuMemGetAddressRangeName);
  cuMemAllocHost := CUDAGetProcAddress(cuMemAllocHostName);
  cuMemFreeHost := CUDAGetProcAddress(cuMemFreeHostName);
  cuMemHostAlloc := CUDAGetProcAddress(cuMemHostAllocName);
  cuMemHostGetDevicePointer := CUDAGetProcAddress
    (cuMemHostGetDevicePointerName);
  cuMemHostGetFlags := CUDAGetProcAddress(cuMemHostGetFlagsName);
  cuMemcpyHtoD := CUDAGetProcAddress(cuMemcpyHtoDName);
  cuMemcpyDtoH := CUDAGetProcAddress(cuMemcpyDtoHName);
  cuMemcpyDtoD := CUDAGetProcAddress(cuMemcpyDtoDName);
  cuMemcpyDtoDAsync := CUDAGetProcAddress(cuMemcpyDtoDAsyncName);
  cuMemcpyDtoA := CUDAGetProcAddress(cuMemcpyDtoAName);
  cuMemcpyAtoD := CUDAGetProcAddress(cuMemcpyAtoDName);
  cuMemcpyHtoA := CUDAGetProcAddress(cuMemcpyHtoAName);
  cuMemcpyAtoH := CUDAGetProcAddress(cuMemcpyAtoHName);
  cuMemcpyAtoA := CUDAGetProcAddress(cuMemcpyAtoAName);
  cuMemcpy2D := CUDAGetProcAddress(cuMemcpy2DName);
  cuMemcpy2DUnaligned := CUDAGetProcAddress(cuMemcpy2DUnalignedName);
  cuMemcpy3D := CUDAGetProcAddress(cuMemcpy3DName);
  cuMemcpyHtoDAsync := CUDAGetProcAddress(cuMemcpyHtoDAsyncName);
  cuMemcpyDtoHAsync := CUDAGetProcAddress(cuMemcpyDtoHAsyncName);
  cuMemcpyHtoAAsync := CUDAGetProcAddress(cuMemcpyHtoAAsyncName);
  cuMemcpyAtoHAsync := CUDAGetProcAddress(cuMemcpyAtoHAsyncName);
  cuMemcpy2DAsync := CUDAGetProcAddress(cuMemcpy2DAsyncName);
  cuMemcpy3DAsync := CUDAGetProcAddress(cuMemcpy3DAsyncName);
  cuMemsetD8 := CUDAGetProcAddress(cuMemsetD8Name);
  cuMemsetD16 := CUDAGetProcAddress(cuMemsetD16Name);
  cuMemsetD32 := CUDAGetProcAddress(cuMemsetD32Name);
  cuMemsetD2D8 := CUDAGetProcAddress(cuMemsetD2D8Name);
  cuMemsetD2D16 := CUDAGetProcAddress(cuMemsetD2D16Name);
  cuMemsetD2D32 := CUDAGetProcAddress(cuMemsetD2D32Name);
  cuFuncSetBlockShape := CUDAGetProcAddress(cuFuncSetBlockShapeName);
  cuFuncSetSharedSize := CUDAGetProcAddress(cuFuncSetSharedSizeName);
  cuFuncGetAttribute := CUDAGetProcAddress(cuFuncGetAttributeName);
  cuArrayCreate := CUDAGetProcAddress(cuArrayCreateName);
  cuArrayGetDescriptor := CUDAGetProcAddress(cuArrayGetDescriptorName);
  cuArrayDestroy := CUDAGetProcAddress(cuArrayDestroyName);
  cuArray3DCreate := CUDAGetProcAddress(cuArray3DCreateName);
  cuArray3DGetDescriptor := CUDAGetProcAddress(cuArray3DGetDescriptorName);
  cuTexRefCreate := CUDAGetProcAddress(cuTexRefCreateName);
  cuTexRefDestroy := CUDAGetProcAddress(cuTexRefDestroyName);
  cuTexRefSetArray := CUDAGetProcAddress(cuTexRefSetArrayName);
  cuTexRefSetAddress := CUDAGetProcAddress(cuTexRefSetAddressName);
  cuTexRefSetAddress2D := CUDAGetProcAddress(cuTexRefSetAddress2DName);
  cuTexRefSetFormat := CUDAGetProcAddress(cuTexRefSetFormatName);
  cuTexRefSetAddressMode := CUDAGetProcAddress(cuTexRefSetAddressModeName);
  cuTexRefSetFilterMode := CUDAGetProcAddress(cuTexRefSetFilterModeName);
  cuTexRefSetFlags := CUDAGetProcAddress(cuTexRefSetFlagsName);
  cuTexRefGetAddress := CUDAGetProcAddress(cuTexRefGetAddressName);
  cuTexRefGetArray := CUDAGetProcAddress(cuTexRefGetArrayName);
  cuTexRefGetAddressMode := CUDAGetProcAddress(cuTexRefGetAddressModeName);
  cuTexRefGetFilterMode := CUDAGetProcAddress(cuTexRefGetFilterModeName);
  cuTexRefGetFormat := CUDAGetProcAddress(cuTexRefGetFormatName);
  cuTexRefGetFlags := CUDAGetProcAddress(cuTexRefGetFlagsName);
  cuParamSetSize := CUDAGetProcAddress(cuParamSetSizeName);
  cuParamSeti := CUDAGetProcAddress(cuParamSetiName);
  cuParamSetf := CUDAGetProcAddress(cuParamSetfName);
  cuParamSetv := CUDAGetProcAddress(cuParamSetvName);
  cuParamSetTexRef := CUDAGetProcAddress(cuParamSetTexRefName);
  cuLaunch := CUDAGetProcAddress(cuLaunchName);
  cuLaunchGrid := CUDAGetProcAddress(cuLaunchGridName);
  cuLaunchGridAsync := CUDAGetProcAddress(cuLaunchGridAsyncName);
  cuEventCreate := CUDAGetProcAddress(cuEventCreateName);
  cuEventRecord := CUDAGetProcAddress(cuEventRecordName);
  cuEventQuery := CUDAGetProcAddress(cuEventQueryName);
  cuEventSynchronize := CUDAGetProcAddress(cuEventSynchronizeName);
  cuEventDestroy := CUDAGetProcAddress(cuEventDestroyName);
  cuEventElapsedTime := CUDAGetProcAddress(cuEventElapsedTimeName);
  cuStreamCreate := CUDAGetProcAddress(cuStreamCreateName);
  cuStreamQuery := CUDAGetProcAddress(cuStreamQueryName);
  cuStreamSynchronize := CUDAGetProcAddress(cuStreamSynchronizeName);
  cuStreamDestroy := CUDAGetProcAddress(cuStreamDestroyName);
  cuGLCtxCreate := CUDAGetProcAddress(cuGLCtxCreateName);
  cuGraphicsGLRegisterBuffer := CUDAGetProcAddress
    (cuGraphicsGLRegisterBufferName);
  cuGraphicsGLRegisterImage := CUDAGetProcAddress
    (cuGraphicsGLRegisterImageName);
  cuWGLGetDevice := CUDAGetProcAddress(cuWGLGetDeviceName);
  cuGraphicsUnregisterResource :=
    CUDAGetProcAddress(cuGraphicsUnregisterResourceName);
  cuGraphicsSubResourceGetMappedArray :=
    CUDAGetProcAddress(cuGraphicsSubResourceGetMappedArrayName);
  cuGraphicsResourceGetMappedPointer :=
    CUDAGetProcAddress(cuGraphicsResourceGetMappedPointerName);
  cuGraphicsResourceSetMapFlags :=
    CUDAGetProcAddress(cuGraphicsResourceSetMapFlagsName);
  cuGraphicsMapResources := CUDAGetProcAddress(cuGraphicsMapResourcesName);
  cuGraphicsUnmapResources := CUDAGetProcAddress(cuGraphicsUnmapResourcesName);
  cuGLInit := CUDAGetProcAddress(cuGLInitName);
  cuGLRegisterBufferObject := CUDAGetProcAddress(cuGLRegisterBufferObjectName);
  cuGLMapBufferObject := CUDAGetProcAddress(cuGLMapBufferObjectName);
  cuGLUnmapBufferObject := CUDAGetProcAddress(cuGLUnmapBufferObjectName);
  cuGLUnregisterBufferObject := CUDAGetProcAddress
    (cuGLUnregisterBufferObjectName);
  cuGLSetBufferObjectMapFlags :=
    CUDAGetProcAddress(cuGLSetBufferObjectMapFlagsName);
  cuGLMapBufferObjectAsync := CUDAGetProcAddress(cuGLMapBufferObjectAsyncName);
  cuGLUnmapBufferObjectAsync := CUDAGetProcAddress
    (cuGLUnmapBufferObjectAsyncName);
{$ELSE}
  cuInit_ := CUDAGetProcAddress(cuInitName);
  cuInit := cuInitShell;
  cuDriverGetVersion_ := CUDAGetProcAddress(cuDriverGetVersionName);
  cuDriverGetVersion := cuDriverGetVersionShell;
  cuDeviceGet_ := CUDAGetProcAddress(cuDeviceGet_Name);
  cuDeviceGet := cuDeviceGetShell;
  cuDeviceGetCount_ := CUDAGetProcAddress(cuDeviceGetCountName);
  cuDeviceGetCount := cuDeviceGetCountShell;
  cuDeviceGetName_ := CUDAGetProcAddress(cuDeviceGetNameName);
  cuDeviceGetName := cuDeviceGetNameShell;
  cuDeviceComputeCapability_ := CUDAGetProcAddress
    (cuDeviceComputeCapabilityName);
  cuDeviceComputeCapability := cuDeviceComputeCapabilityShell;
  cuDeviceTotalMem_ := CUDAGetProcAddress(cuDeviceTotalMemName);
  cuDeviceTotalMem := cuDeviceTotalMemShell;
  cuDeviceGetProperties_ := CUDAGetProcAddress(cuDeviceGetPropertiesName);
  cuDeviceGetProperties := cuDeviceGetPropertiesShell;
  cuDeviceGetAttribute_ := CUDAGetProcAddress(cuDeviceGetAttributeName);
  cuDeviceGetAttribute := cuDeviceGetAttributeShell;
  cuCtxCreate_ := CUDAGetProcAddress(cuCtxCreateName);
  cuCtxCreate := cuCtxCreateShell;
  cuCtxDestroy_ := CUDAGetProcAddress(cuCtxDestroyName);
  cuCtxDestroy := cuCtxDestroyShell;
  cuCtxAttach_ := CUDAGetProcAddress(cuCtxAttachName);
  cuCtxAttach := cuCtxAttachShell;
  cuCtxDetach_ := CUDAGetProcAddress(cuCtxDetachName);
  cuCtxDetach := cuCtxDetachShell;
  cuCtxPushCurrent_ := CUDAGetProcAddress(cuCtxPushCurrentName);
  cuCtxPushCurrent := cuCtxPushCurrentShell;
  cuCtxPopCurrent_ := CUDAGetProcAddress(cuCtxPopCurrentName);
  cuCtxPopCurrent := cuCtxPopCurrentShell;
  cuCtxGetDevice_ := CUDAGetProcAddress(cuCtxGetDeviceName);
  cuCtxGetDevice := cuCtxGetDeviceShell;
  cuCtxSynchronize_ := CUDAGetProcAddress(cuCtxSynchronizeName);
  cuCtxSynchronize := cuCtxSynchronizeShell;
  cuModuleLoad_ := CUDAGetProcAddress(cuModuleLoadName);
  cuModuleLoad := cuModuleLoadShell;
  cuModuleLoadData_ := CUDAGetProcAddress(cuModuleLoadDataName);
  cuModuleLoadData := cuModuleLoadDataShell;
  cuModuleLoadDataEx_ := CUDAGetProcAddress(cuModuleLoadDataExName);
  cuModuleLoadDataEx := cuModuleLoadDataExShell;
  cuModuleLoadFatBinary_ := CUDAGetProcAddress(cuModuleLoadFatBinaryName);
  cuModuleLoadFatBinary := cuModuleLoadFatBinaryShell;
  cuModuleUnload_ := CUDAGetProcAddress(cuModuleUnloadName);
  cuModuleUnload := cuModuleUnloadShell;
  cuModuleGetFunction_ := CUDAGetProcAddress(cuModuleGetFunctionName);
  cuModuleGetFunction := cuModuleGetFunctionShell;
  cuModuleGetGlobal_ := CUDAGetProcAddress(cuModuleGetGlobalName);
  cuModuleGetGlobal := cuModuleGetGlobalShell;
  cuModuleGetTexRef_ := CUDAGetProcAddress(cuModuleGetTexRefName);
  cuModuleGetTexRef := cuModuleGetTexRefShell;
  cuMemGetInfo_ := CUDAGetProcAddress(cuMemGetInfoName);
  cuMemGetInfo := cuMemGetInfoShell;
  cuMemAlloc_ := CUDAGetProcAddress(cuMemAllocName);
  cuMemAlloc := cuMemAllocShell;
  cuMemAllocPitch_ := CUDAGetProcAddress(cuMemAllocPitchName);
  cuMemAllocPitch := cuMemAllocPitchShell;
  cuMemFree_ := CUDAGetProcAddress(cuMemFreeName);
  cuMemFree := cuMemFreeShell;
  cuMemGetAddressRange_ := CUDAGetProcAddress(cuMemGetAddressRangeName);
  cuMemGetAddressRange := cuMemGetAddressRangeShell;
  cuMemAllocHost_ := CUDAGetProcAddress(cuMemAllocHostName);
  cuMemAllocHost := cuMemAllocHostShell;
  cuMemFreeHost_ := CUDAGetProcAddress(cuMemFreeHostName);
  cuMemFreeHost := cuMemFreeHostShell;
  cuMemHostAlloc_ := CUDAGetProcAddress(cuMemHostAllocName);
  cuMemHostAlloc := cuMemHostAllocShell;
  cuMemHostGetDevicePointer_ := CUDAGetProcAddress
    (cuMemHostGetDevicePointerName);
  cuMemHostGetDevicePointer := cuMemHostGetDevicePointerShell;
  cuMemHostGetFlags_ := CUDAGetProcAddress(cuMemHostGetFlagsName);
  cuMemHostGetFlags := cuMemHostGetFlagsShell;
  cuMemcpyHtoD_ := CUDAGetProcAddress(cuMemcpyHtoDName);
  cuMemcpyHtoD := cuMemcpyHtoDShell;
  cuMemcpyDtoH_ := CUDAGetProcAddress(cuMemcpyDtoHName);
  cuMemcpyDtoH := cuMemcpyDtoHShell;
  cuMemcpyDtoD_ := CUDAGetProcAddress(cuMemcpyDtoDName);
  cuMemcpyDtoD := cuMemcpyDtoDShell;
  cuMemcpyDtoDAsync_ := CUDAGetProcAddress(cuMemcpyDtoDAsyncName);
  cuMemcpyDtoDAsync := cuMemcpyDtoDAsyncShell;
  cuMemcpyDtoA_ := CUDAGetProcAddress(cuMemcpyDtoAName);
  cuMemcpyDtoA := cuMemcpyDtoAShell;
  cuMemcpyAtoD_ := CUDAGetProcAddress(cuMemcpyAtoDName);
  cuMemcpyAtoD := cuMemcpyAtoDShell;
  cuMemcpyHtoA_ := CUDAGetProcAddress(cuMemcpyHtoAName);
  cuMemcpyHtoA := cuMemcpyHtoAShell;
  cuMemcpyAtoH_ := CUDAGetProcAddress(cuMemcpyAtoHName);
  cuMemcpyAtoH := cuMemcpyAtoHShell;
  cuMemcpyAtoA_ := CUDAGetProcAddress(cuMemcpyAtoAName);
  cuMemcpyAtoA := cuMemcpyAtoAShell;
  cuMemcpy2D_ := CUDAGetProcAddress(cuMemcpy2DName);
  cuMemcpy2D := cuMemcpy2DShell;
  cuMemcpy2DUnaligned_ := CUDAGetProcAddress(cuMemcpy2DUnalignedName);
  cuMemcpy2DUnaligned := cuMemcpy2DUnalignedShell;
  cuMemcpy3D_ := CUDAGetProcAddress(cuMemcpy3DName);
  cuMemcpy3D := cuMemcpy3DShell;
  cuMemcpyHtoDAsync_ := CUDAGetProcAddress(cuMemcpyHtoDAsyncName);
  cuMemcpyHtoDAsync := cuMemcpyHtoDAsyncShell;
  cuMemcpyDtoHAsync_ := CUDAGetProcAddress(cuMemcpyDtoHAsyncName);
  cuMemcpyDtoHAsync := cuMemcpyDtoHAsyncShell;
  cuMemcpyHtoAAsync_ := CUDAGetProcAddress(cuMemcpyHtoAAsyncName);
  cuMemcpyHtoAAsync := cuMemcpyHtoAAsyncShell;
  cuMemcpyAtoHAsync_ := CUDAGetProcAddress(cuMemcpyAtoHAsyncName);
  cuMemcpyAtoHAsync := cuMemcpyAtoHAsyncShell;
  cuMemcpy2DAsync_ := CUDAGetProcAddress(cuMemcpy2DAsyncName);
  cuMemcpy2DAsync := cuMemcpy2DAsyncShell;
  cuMemcpy3DAsync_ := CUDAGetProcAddress(cuMemcpy3DAsyncName);
  cuMemcpy3DAsync := cuMemcpy3DAsyncShell;
  cuMemsetD8_ := CUDAGetProcAddress(cuMemsetD8Name);
  cuMemsetD8 := cuMemsetD8Shell;
  cuMemsetD16_ := CUDAGetProcAddress(cuMemsetD16Name);
  cuMemsetD16 := cuMemsetD16Shell;
  cuMemsetD32_ := CUDAGetProcAddress(cuMemsetD32Name);
  cuMemsetD32 := cuMemsetD32Shell;
  cuMemsetD2D8_ := CUDAGetProcAddress(cuMemsetD2D8Name);
  cuMemsetD2D8 := cuMemsetD2D8Shell;
  cuMemsetD2D16_ := CUDAGetProcAddress(cuMemsetD2D16Name);
  cuMemsetD2D16 := cuMemsetD2D16Shell;
  cuMemsetD2D32_ := CUDAGetProcAddress(cuMemsetD2D32Name);
  cuMemsetD2D32 := cuMemsetD2D32Shell;
  cuFuncSetBlockShape_ := CUDAGetProcAddress(cuFuncSetBlockShapeName);
  cuFuncSetBlockShape := cuFuncSetBlockShapeShell;
  cuFuncSetSharedSize_ := CUDAGetProcAddress(cuFuncSetSharedSizeName);
  cuFuncSetSharedSize := cuFuncSetSharedSizeShell;
  cuFuncGetAttribute_ := CUDAGetProcAddress(cuFuncGetAttributeName);
  cuFuncGetAttribute := cuFuncGetAttributeShell;
  cuArrayCreate_ := CUDAGetProcAddress(cuArrayCreateName);
  cuArrayCreate := cuArrayCreateShell;
  cuArrayGetDescriptor_ := CUDAGetProcAddress(cuArrayGetDescriptorName);
  cuArrayGetDescriptor := cuArrayGetDescriptorShell;
  cuArrayDestroy_ := CUDAGetProcAddress(cuArrayDestroyName);
  cuArrayDestroy := cuArrayDestroyShell;
  cuArray3DCreate_ := CUDAGetProcAddress(cuArray3DCreateName);
  cuArray3DCreate := cuArray3DCreateShell;
  cuArray3DGetDescriptor_ := CUDAGetProcAddress(cuArray3DGetDescriptorName);
  cuArray3DGetDescriptor := cuArray3DGetDescriptorShell;
  cuTexRefCreate_ := CUDAGetProcAddress(cuTexRefCreateName);
  cuTexRefCreate := cuTexRefCreateShell;
  cuTexRefDestroy_ := CUDAGetProcAddress(cuTexRefDestroyName);
  cuTexRefDestroy := cuTexRefDestroyShell;
  cuTexRefSetArray_ := CUDAGetProcAddress(cuTexRefSetArrayName);
  cuTexRefSetArray := cuTexRefSetArrayShell;
  cuTexRefSetAddress_ := CUDAGetProcAddress(cuTexRefSetAddressName);
  cuTexRefSetAddress := cuTexRefSetAddressShell;
  cuTexRefSetAddress2D_ := CUDAGetProcAddress(cuTexRefSetAddress2DName);
  cuTexRefSetAddress2D := cuTexRefSetAddress2DShell;
  cuTexRefSetFormat_ := CUDAGetProcAddress(cuTexRefSetFormatName);
  cuTexRefSetFormat := cuTexRefSetFormatShell;
  cuTexRefSetAddressMode_ := CUDAGetProcAddress(cuTexRefSetAddressModeName);
  cuTexRefSetAddressMode := cuTexRefSetAddressModeShell;
  cuTexRefSetFilterMode_ := CUDAGetProcAddress(cuTexRefSetFilterModeName);
  cuTexRefSetFilterMode := cuTexRefSetFilterModeShell;
  cuTexRefSetFlags_ := CUDAGetProcAddress(cuTexRefSetFlagsName);
  cuTexRefSetFlags := cuTexRefSetFlagsShell;
  cuTexRefGetAddress_ := CUDAGetProcAddress(cuTexRefGetAddressName);
  cuTexRefGetAddress := cuTexRefGetAddressShell;
  cuTexRefGetArray_ := CUDAGetProcAddress(cuTexRefGetArrayName);
  cuTexRefGetArray := cuTexRefGetArrayShell;
  cuTexRefGetAddressMode_ := CUDAGetProcAddress(cuTexRefGetAddressModeName);
  cuTexRefGetAddressMode := cuTexRefGetAddressModeShell;
  cuTexRefGetFilterMode_ := CUDAGetProcAddress(cuTexRefGetFilterModeName);
  cuTexRefGetFilterMode := cuTexRefGetFilterModeShell;
  cuTexRefGetFormat_ := CUDAGetProcAddress(cuTexRefGetFormatName);
  cuTexRefGetFormat := cuTexRefGetFormatShell;
  cuTexRefGetFlags_ := CUDAGetProcAddress(cuTexRefGetFlagsName);
  cuTexRefGetFlags := cuTexRefGetFlagsShell;
  cuParamSetSize_ := CUDAGetProcAddress(cuParamSetSizeName);
  cuParamSetSize := cuParamSetSizeShell;
  cuParamSeti_ := CUDAGetProcAddress(cuParamSetiName);
  cuParamSeti := cuParamSetiShell;
  cuParamSetf_ := CUDAGetProcAddress(cuParamSetfName);
  cuParamSetf := cuParamSetfShell;
  cuParamSetv_ := CUDAGetProcAddress(cuParamSetvName);
  cuParamSetv := cuParamSetvShell;
  cuParamSetTexRef_ := CUDAGetProcAddress(cuParamSetTexRefName);
  cuParamSetTexRef := cuParamSetTexRefShell;
  cuLaunch_ := CUDAGetProcAddress(cuLaunchName);
  cuLaunch := cuLaunchShell;
  cuLaunchGrid_ := CUDAGetProcAddress(cuLaunchGridName);
  cuLaunchGrid := cuLaunchGridShell;
  cuLaunchGridAsync_ := CUDAGetProcAddress(cuLaunchGridAsyncName);
  cuLaunchGridAsync := cuLaunchGridAsyncShell;
  cuEventCreate_ := CUDAGetProcAddress(cuEventCreateName);
  cuEventCreate := cuEventCreateShell;
  cuEventRecord_ := CUDAGetProcAddress(cuEventRecordName);
  cuEventRecord := cuEventRecordShell;
  cuEventQuery_ := CUDAGetProcAddress(cuEventQueryName);
  cuEventQuery := cuEventQueryShell;
  cuEventSynchronize_ := CUDAGetProcAddress(cuEventSynchronizeName);
  cuEventSynchronize := cuEventSynchronizeShell;
  cuEventDestroy_ := CUDAGetProcAddress(cuEventDestroyName);
  cuEventDestroy := cuEventDestroyShell;
  cuEventElapsedTime_ := CUDAGetProcAddress(cuEventElapsedTimeName);
  cuEventElapsedTime := cuEventElapsedTimeShell;
  cuStreamCreate_ := CUDAGetProcAddress(cuStreamCreateName);
  cuStreamCreate := cuStreamCreateShell;
  cuStreamQuery_ := CUDAGetProcAddress(cuStreamQueryName);
  cuStreamQuery := cuStreamQueryShell;
  cuStreamSynchronize_ := CUDAGetProcAddress(cuStreamSynchronizeName);
  cuStreamSynchronize := cuStreamSynchronizeShell;
  cuStreamDestroy_ := CUDAGetProcAddress(cuStreamDestroyName);
  cuStreamDestroy := cuStreamDestroyShell;
  cuGLCtxCreate_ := CUDAGetProcAddress(cuGLCtxCreateName);
  cuGLCtxCreate := cuGLCtxCreateShell;
  cuGraphicsGLRegisterBuffer_ :=
    CUDAGetProcAddress(cuGraphicsGLRegisterBufferName);
  cuGraphicsGLRegisterBuffer := cuGraphicsGLRegisterBufferShell;
  cuGraphicsGLRegisterImage_ := CUDAGetProcAddress
    (cuGraphicsGLRegisterImageName);
  cuGraphicsGLRegisterImage := cuGraphicsGLRegisterImageShell;
  cuWGLGetDevice_ := CUDAGetProcAddress(cuWGLGetDeviceName);
  cuWGLGetDevice := cuWGLGetDeviceShell;
  cuGraphicsUnregisterResource_ :=
    CUDAGetProcAddress(cuGraphicsUnregisterResourceName);
  cuGraphicsUnregisterResource := cuGraphicsUnregisterResourceShell;
  cuGraphicsSubResourceGetMappedArray_ :=
    CUDAGetProcAddress(cuGraphicsSubResourceGetMappedArrayName);
  cuGraphicsSubResourceGetMappedArray :=
    cuGraphicsSubResourceGetMappedArrayShell;
  cuGraphicsResourceGetMappedPointer_ :=
    CUDAGetProcAddress(cuGraphicsResourceGetMappedPointerName);
  cuGraphicsResourceGetMappedPointer := cuGraphicsResourceGetMappedPointerShell;
  cuGraphicsResourceSetMapFlags_ :=
    CUDAGetProcAddress(cuGraphicsResourceSetMapFlagsName);
  cuGraphicsResourceSetMapFlags := cuGraphicsResourceSetMapFlagsShell;
  cuGraphicsMapResources_ := CUDAGetProcAddress(cuGraphicsMapResourcesName);
  cuGraphicsMapResources := cuGraphicsMapResourcesShell;
  cuGraphicsUnmapResources_ := CUDAGetProcAddress(cuGraphicsUnmapResourcesName);
  cuGraphicsUnmapResources := cuGraphicsUnmapResourcesShell;
  cuGLInit := CUDAGetProcAddress(cuGLInitName);
  cuGLRegisterBufferObject_ := CUDAGetProcAddress(cuGLRegisterBufferObjectName);
  cuGLRegisterBufferObject := cuGLRegisterBufferObjectShell;
  cuGLMapBufferObject_ := CUDAGetProcAddress(cuGLMapBufferObjectName);
  cuGLMapBufferObject := cuGLMapBufferObjectShell;
  cuGLUnmapBufferObject_ := CUDAGetProcAddress(cuGLUnmapBufferObjectName);
  cuGLUnmapBufferObject := cuGLUnmapBufferObjectShell;
  cuGLUnregisterBufferObject_ :=
    CUDAGetProcAddress(cuGLUnregisterBufferObjectName);
  cuGLUnregisterBufferObject := cuGLUnregisterBufferObjectShell;
  cuGLSetBufferObjectMapFlags_ :=
    CUDAGetProcAddress(cuGLSetBufferObjectMapFlagsName);
  cuGLSetBufferObjectMapFlags := cuGLSetBufferObjectMapFlagsShell;
  cuGLMapBufferObjectAsync_ := CUDAGetProcAddress(cuGLMapBufferObjectAsyncName);
  cuGLMapBufferObjectAsync := cuGLMapBufferObjectAsyncShell;
  cuGLUnmapBufferObjectAsync_ :=
    CUDAGetProcAddress(cuGLUnmapBufferObjectAsyncName);
  cuGLUnmapBufferObjectAsync := cuGLUnmapBufferObjectAsyncShell;
{$ENDIF GLS_CUDA_DEBUG_MODE}
  cuDriverGetVersion(V);
  GLSLogger.LogInfoFmt('%s version %d is loaded', [CUDAAPIDLL, V]);
  Result := True;
end;

function IsCUDAInitialized: Boolean;
begin
  Result := (CUDAHandle <> INVALID_MODULEHANDLE);
end;

function Get_CUDA_API_Error_String(AError: TCUresult): string;
begin
  if AError = CUDA_SUCCESS then
    Result := 'No errors'
  else if AError = CUDA_ERROR_INVALID_VALUE then
    Result := 'Invalid value'
  else if AError = CUDA_ERROR_OUT_OF_MEMORY then
    Result := 'Out of memory'
  else if AError = CUDA_ERROR_NOT_INITIALIZED then
    Result := 'Driver not initialized'
  else if AError = CUDA_ERROR_DEINITIALIZED then
    Result := 'Driver deinitialized'
  else if AError = CUDA_ERROR_NO_DEVICE then
    Result := 'No CUDA-capable device available'
  else if AError = CUDA_ERROR_INVALID_DEVICE then
    Result := 'Invalid device'
  else if AError = CUDA_ERROR_INVALID_IMAGE then
    Result := 'Invalid kernel image'
  else if AError = CUDA_ERROR_INVALID_CONTEXT then
    Result := 'Invalid context'
  else if AError = CUDA_ERROR_CONTEXT_ALREADY_CURRENT then
    Result := 'Context already current'
  else if AError = CUDA_ERROR_MAP_FAILED then
    Result := 'Map failed'
  else if AError = CUDA_ERROR_UNMAP_FAILED then
    Result := 'Unmap failed'
  else if AError = CUDA_ERROR_ARRAY_IS_MAPPED then
    Result := 'Array is mapped'
  else if AError = CUDA_ERROR_ALREADY_MAPPED then
    Result := 'Already mapped'
  else if AError = CUDA_ERROR_NO_BINARY_FOR_GPU then
    Result := 'No binary for GPU'
  else if AError = CUDA_ERROR_ALREADY_ACQUIRED then
    Result := 'Already acquired'
  else if AError = CUDA_ERROR_NOT_MAPPED then
    Result := 'Not mapped'
  else if AError = CUDA_ERROR_NOT_MAPPED_AS_ARRAY then
    Result := 'Not mapped as array'
  else if AError = CUDA_ERROR_NOT_MAPPED_AS_POINTER then
    Result := 'Not mapped as pointer'
  else if AError = CUDA_ERROR_INVALID_SOURCE then
    Result := 'Invalid source'
  else if AError = CUDA_ERROR_FILE_NOT_FOUND then
    Result := 'File not found'
  else if AError = CUDA_ERROR_INVALID_HANDLE then
    Result := 'Invalid handle'
  else if AError = CUDA_ERROR_NOT_FOUND then
    Result := 'Not found'
  else if AError = CUDA_ERROR_NOT_READY then
    Result := 'CUDA not ready'
  else if AError = CUDA_ERROR_LAUNCH_FAILED then
    Result := 'Launch failed'
  else if AError = CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES then
    Result := 'Launch exceeded resources'
  else if AError = CUDA_ERROR_LAUNCH_TIMEOUT then
    Result := 'Launch exceeded timeout'
  else if AError = CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING then
    Result := 'Launch with incompatible texturing'
  else if AError = CUDA_ERROR_POINTER_IS_64BIT then
    Result := 'Pointer is 64bit'
  else if AError = CUDA_ERROR_SIZE_IS_64BIT then
    Result := 'Size is 64bit'
  else
    Result := 'Unknown error';
end;

end.
