//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLContext<p>

   Prototypes and base implementation of TGLContext.<p>

   <b>History : </b><font size=-1><ul>
      <li>06/03/10 - Yar - Added to TGLProgramHandle BindFragDataLocation, GetUniformOffset, GetUniformBlockIndex
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>22/02/10 - DanB - Added TGLContext.GLStates, to be used to cache
                            global per-context state. Removed BindedGLSLProgram
                            since it should be per-context state.
      <li>21/02/10 - Yar - Added function BindedGLSLProgram
      <li>08/01/10 - DaStr - Added TGLFramebufferHandle.AttachLayer()
                             Added more AntiAliasing modes (thanks YarUndeoaker)
      <li>13/12/09 - DaStr - Modified for multithread support (thanks Controller)
      <li>30/08/09 - DanB - renamed vIgnoreContextActivationFailures to vContextActivationFailureOccurred
                            + re-enabled it's original behaviour (fixes major memory leak).
      <li>30/08/09 - DanB - Added TGLTransformFeedbackBufferHandle, TGLTextureBufferHandle,
                            TGLUniformBufferHandle, TGLVertexArrayHandle,
                            TGLFramebufferHandle, TGLRenderbufferHandle
      <li>24/08/09 - DaStr - Added TGLProgramHandle.GetVaryingLocation(),
                              AddActiveVarying() (thanks YarUnderoaker)
      <li>21/08/09 - DanB - TGLQueryHandle.GetTarget no longer a class function,
                            for earlier Delphi compatibility
      <li>13/08/09 - DanB - Added timer & primitive queries.  Occlusion queries now
                            use OpenGL 1.5+ queries, instead of GL_NV_occlusion_query extension
      <li>10/06/09 - DanB - removed OpenGL error handling code, it already exists in OpenGL1x.pas
      <li>16/03/08 - DanB - moved MRT_BUFFERS into unit from opengl1x.pas rewrite,
                            and added some experimental geometry shader code
      <li>15/03/08 - DaStr - Fixups for vIgnoreContextActivationFailures mode
                                                      (BugTracker ID = 1914782)
      <li>06/11/07 - LC - moved vIgnoreContextActivationFailures to "Interface" section
      <li>24/06/06 - LC - Refactored TGLVBOHandle, introduced TGLBufferObjectHandle
                          and TGLPackPBOHandle/TGLUnpackPBOHandle
      <li>15/02/07 - DaStr - Added more parameters to TGLProgramHandle
                             TGLProgramHandle.Name is now a property
      <li>15/02/07 - DaStr - Integer -> Cardinal because $R- was removed in GLScene.pas
      <li>15/09/06 - NC - TGLContextHandle.handle as Integer -> Cardinal
      <li>11/09/06 - NC - Added TGLProgramHandle.Name, TGLProgramHandle.Uniform2f,
                          SetUniform*, support for Multiple-Render-Target
      <li>25/04/04 - EG - Added TGLOcclusionQueryHandle.Active
      <li>25/09/03 - EG - Added TGLVBOHandle
      <li>20/09/03 - EG - Added TGLOcclusionQueryHandle
      <li>30/01/02 - EG - Added TGLVirtualHandle
      <li>29/01/02 - EG - Improved recovery for context creation failures
      <li>28/01/02 - EG - Activation failures always ignored
      <li>21/01/02 - EG - Activation failures now ignored if application is
                          terminating (workaround for some weird ICDs)
      <li>15/12/01 - EG - Added support for AlphaBits
      <li>30/11/01 - EG - Added TGLContextAcceleration
      <li>06/09/01 - EG - Win32Context moved to new GLWin32Context unit
      <li>04/09/01 - EG - Added ChangeIAttrib, support for 16bits depth buffer
      <li>25/08/01 - EG - Added pbuffer support and CreateMemoryContext interface
      <li>24/08/01 - EG - Fixed PropagateSharedContext
      <li>12/08/01 - EG - Handles management completed
      <li>22/07/01 - EG - Creation (glcontext.omm)
   </ul></font>
}
unit GLContext;

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS} {$IFDEF GLS_MULTITHREAD}
Windows,
{$ENDIF} {$ENDIF}

Classes, SysUtils, GLCrossPlatform, OpenGL1x, VectorGeometry, VectorTypes, GLState;

// Buffer ID's for Multiple-Render-Targets (using GL_ATI_draw_buffers)
const
  MRT_BUFFERS: array [0..3] of GLenum = (GL_FRONT_LEFT, GL_AUX0, GL_AUX1, GL_AUX2);

type

   // TGLRCOptions
   //
   TGLRCOption = ( rcoDoubleBuffered, rcoStereo );
   TGLRCOptions = set of TGLRCOption;

   TGLContextManager = class;

   // TGLContextAcceleration
   //
   TGLContextAcceleration = (chaUnknown, chaHardware, chaSoftware);

   // TGLAntiAliasing
   //
   TGLAntiAliasing = ( // Multisample Antialiasing
                       aaDefault, aaNone, aa2x, aa2xHQ, aa4x, aa4xHQ,
                       aa6x, aa8x, aa16x,
                       // Coverage Sampling Antialiasing
                       csa8x, csa8xHQ, csa16x, csa16xHQ);

   // TGLContext
   //
   {: Wrapper around an OpenGL rendering context.<p>
      The aim of this class is to offer platform-independant
      initialization, activation and management of OpenGL
      rendering context. The class also offers notifications
      event and error/problems detection.<br>
      This is a virtual abstract a class, and platform-specific
      subclasses must be used.<br>
      All rendering context share the same lists. }
   TGLContext = class
      private
         { Private Declarations }
         FColorBits, FAlphaBits : Integer;
         FDepthBits : Integer;
         FStencilBits : Integer;
         FAccumBits : Integer;
         FAuxBuffers : Integer;
         FAntiAliasing : TGLAntiAliasing;
         FOptions : TGLRCOptions;
         FOnDestroyContext : TNotifyEvent;
         FManager : TGLContextManager;
         FActivationCount : Integer;
         FGLStates : TGLStateCache;
{$IFNDEF GLS_MULTITHREAD}
         FSharedContexts : TList;
         FOwnedHandles : TList;
{$ELSE}
         FSharedContexts : TThreadList;
         FOwnedHandles : TThreadList;
         FLock: TRTLCriticalSection;
{$ENDIF}
      protected
         { Protected Declarations }
         FAcceleration : TGLContextAcceleration;

         procedure SetColorBits(const aColorBits : Integer);
         procedure SetAlphaBits(const aAlphaBits : Integer);
         procedure SetDepthBits(const val : Integer);
         procedure SetStencilBits(const aStencilBits : Integer);
         procedure SetAccumBits(const aAccumBits : Integer);
         procedure SetAuxBuffers(const aAuxBuffers : Integer);
         procedure SetOptions(const aOptions : TGLRCOptions);
         procedure SetAntiAliasing(const val : TGLAntiAliasing);
         function  GetActive : Boolean;
         procedure SetActive(const aActive : Boolean);
         procedure PropagateSharedContext;

         procedure DoCreateContext(outputDevice : HDC); dynamic; abstract;
         procedure DoCreateMemoryContext(outputDevice: HDC; width, height : Integer; BufferCount : integer = 1); dynamic; abstract;
         procedure DoShareLists(aContext : TGLContext); dynamic; abstract;
         procedure DoDestroyContext; dynamic; abstract;
         procedure DoActivate; virtual; abstract;
         procedure DoDeactivate; virtual; abstract;

      public
         { Public Declarations }
         constructor Create; virtual;
         destructor Destroy; override;

         {: An application-side cache of global per-context OpenGL states
            and parameters }
         property GLStates: TGLStateCache read FGLStates;

         //: Context manager reference
         property Manager : TGLContextManager read FManager;

         {: Color bits for the rendering context }
         property ColorBits : Integer read FColorBits write SetColorBits;
         {: Alpha bits for the rendering context }
         property AlphaBits : Integer read FAlphaBits write SetAlphaBits;
         {: Depth bits for the rendering context }
         property DepthBits : Integer read FDepthBits write SetDepthBits;
         {: Stencil bits for the rendering context }
         property StencilBits : Integer read FStencilBits write SetStencilBits;
         {: Accumulation buffer bits for the rendering context }
         property AccumBits : Integer read FAccumBits write SetAccumBits;
         {: Auxiliary buffers bits for the rendering context }
         property AuxBuffers : Integer read FAuxBuffers write SetAuxBuffers;
         {: AntiAliasing option.<p>
            Ignored if not hardware supported, currently based on ARB_multisample. }
         property AntiAliasing : TGLAntiAliasing read FAntiAliasing write SetAntiAliasing;
         {: Rendering context options. }
         property Options : TGLRCOptions read FOptions write SetOptions;
         {: Allows reading and defining the activity for the context.<p>
            The methods of this property are just wrappers around calls
            to Activate and Deactivate. }
         property Active : Boolean read GetActive write SetActive;
         {: Indicates if the context is hardware-accelerated. }
         property Acceleration : TGLContextAcceleration read FAcceleration;
         {: Triggered whenever the context is destroyed.<p>
            This events happens *before* the context has been
            actually destroyed, OpenGL resource cleanup can
            still occur here. }
         property OnDestroyContext : TNotifyEvent read FOnDestroyContext write FOnDestroyContext;

         {: Creates the context.<p>
            This method must be invoked before the context can be used. }
         procedure CreateContext(outputDevice : HDC);
         {: Creates an in-memory context.<p>
            The function should fail if no hardware-accelerated memory context
            can be created (the CreateContext method can handle software OpenGL
            contexts). }
         procedure CreateMemoryContext(outputDevice: HDC; width, height : Integer; BufferCount : integer = 1);
         {: Setup display list sharing between two rendering contexts.<p>
            Both contexts must have the same pixel format. }
         procedure ShareLists(aContext : TGLContext);
         {: Destroy the context.<p>
            Will fail if no context has been created.<br>
            The method will first invoke the OnDestroyContext
            event, then attempts to deactivate the context
            (if it is active) before destroying it. }
         procedure DestroyContext;
         {: Activates the context.<p>
            A context can be activated multiple times (and must be
            deactivated the same number of times), but this function
            will fail if another context is already active. }
         procedure Activate;
         {: Deactivates the context.<p>
            Will fail if the context is not active or another
            context has been activated. }
         procedure Deactivate;
         {: Returns true if the context is valid.<p>
            A context is valid from the time it has been successfully
            created to the time of its destruction. }
         function IsValid : Boolean; virtual; abstract;
         {: Request to swap front and back buffers if they were defined. }
         procedure SwapBuffers; virtual; abstract;

         {: Returns the first compatible context that isn't self in the shares. }
         function FindCompatibleContext : TGLContext;
         procedure DestroyAllHandles;

         function RenderOutputDevice : Integer; virtual; abstract;
   end;

   TGLContextClass = class of TGLContext;

   // TGLScreenControlingContext
   //
   {: A TGLContext with screen control property and methods.<p>
      This variety of contexts is for drivers that access windows and OpenGL
      through an intermediate opaque cross-platform API.<p>
      TGLSceneViewer won't use them, TGLMemoryViewer may be able to use them,
      but most of the time they will be accessed through a specific viewer
      class/subclass. }
   TGLScreenControlingContext = class (TGLContext)
      private
         { Private Declarations }
         FWidth, FHeight : Integer;
         FFullScreen : Boolean;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         property Width : Integer read FWidth write FWidth;
         property Height : Integer read FHeight write FHeight;
         property FullScreen : Boolean read FFullScreen write FFullScreen;
   end;

   // TGLContextHandle
   //
   {: Wrapper around an OpenGL context handle.<p>
      This wrapper also takes care of context registrations and data releases
      related to context releases an cleanups. This is an abstract class,
      use the TGLListHandle and TGLTextureHandle subclasses. }
   TGLContextHandle = class
      private
         { Private Declarations }
         FRenderingContext : TGLContext;
         FHandle : Cardinal;

      protected
         { Protected Declarations }
         //: Invoked by when there is no compatible context left for relocation
         procedure ContextDestroying;

         //: Specifies if the handle can be transfered across shared contexts
         class function Transferable : Boolean; virtual;

         function DoAllocateHandle : Cardinal; virtual; abstract;
         procedure DoDestroyHandle; virtual; abstract;

      public
         { Public Declarations }
         constructor Create; virtual;
         constructor CreateAndAllocate(failIfAllocationFailed : Boolean = True);
         destructor Destroy; override;

         property Handle : Cardinal read FHandle;
         property RenderingContext : TGLContext read FRenderingContext;

         //: Checks if required extensions / OpenGL version are met
         class function IsSupported : Boolean; virtual;

         procedure AllocateHandle;
         procedure DestroyHandle;
   end;

   TGLVirtualHandle = class;
   TGLVirtualHandleEvent = procedure (sender : TGLVirtualHandle; var handle : Cardinal) of object;

   // TGLVirtualHandle
   //
   {: A context handle with event-based handle allocation and destruction. }
   TGLVirtualHandle = class (TGLContextHandle)
      private
         { Private Declarations }
         FOnAllocate, FOnDestroy : TGLVirtualHandleEvent;
         FTag : Integer;

      protected
         { Protected Declarations }
         function DoAllocateHandle : Cardinal; override;
         procedure DoDestroyHandle; override;

      public
         { Public Declarations }
         property OnAllocate : TGLVirtualHandleEvent read FOnAllocate write FOnAllocate;
         property OnDestroy : TGLVirtualHandleEvent read FOnDestroy write FOnDestroy;

         property Tag : Integer read FTag write FTag;
   end;

   // TGLListHandle
   //
   {: Manages a handle to a display list. } 
   TGLListHandle = class (TGLContextHandle)
      private
         { Private Declarations }

      protected
         { Protected Declarations }
         function DoAllocateHandle : Cardinal; override;
         procedure DoDestroyHandle; override;

      public
         { Public Declarations }
         procedure NewList(mode : Cardinal);
         procedure EndList;
         procedure CallList;
   end;

   // TGLTextureHandle
   //
   {: Manages a handle to a texture. } 
   TGLTextureHandle = class (TGLContextHandle)
      private
         { Private Declarations }

      protected
         { Protected Declarations }
         function DoAllocateHandle : Cardinal; override;
         procedure DoDestroyHandle; override;

      public
         { Public Declarations }
   end;

   // TGLQueryHandle
   //
   {: Manages a handle to a query.<br>
      Do not use this class directly, use one of its subclasses instead. }
   TGLQueryHandle = class(TGLContextHandle)
      private
         { Private Declarations }
         FActive : Boolean;
      protected
         { Protected Declarations }
         class function Transferable : Boolean; override;
         function DoAllocateHandle : Cardinal; override;
         procedure DoDestroyHandle; override;
         function GetTarget: TGLuint; virtual; abstract;
         function GetQueryType: TQueryType; virtual; abstract;
      public
         { Public Declarations }
         procedure BeginQuery;
         procedure EndQuery;

         // Check if result is available from the query.  Result may not be available
         // immediately after ending the query
         function IsResultAvailable: boolean;
         // Number of bits used to store the query result. eg. 32/64 bit
         function CounterBits: integer;
         // Retrieve query result, may cause a stall if the result is not available yet
         function QueryResultInt: TGLInt;
         function QueryResultUInt: TGLUInt;
         function QueryResultInt64: TGLint64EXT;
         function QueryResultUInt64: TGLuint64EXT;

         property Target : TGLuint read GetTarget;
         property QueryType : TQueryType read GetQueryType;

         {: True if within a Begin/EndQuery. }
         property Active : Boolean read FActive;
   end;

   // TGLOcclusionQueryHandle
   //
   {: Manages a handle to an occlusion query.<br>
      Requires OpenGL 1.5+<br>
      Does *NOT* check for extension availability, this is assumed to have been
      checked by the user. }
   TGLOcclusionQueryHandle = class (TGLQueryHandle)
      protected
         function GetTarget: TGLuint; override;
         function GetQueryType: TQueryType; override;
      public
         class function IsSupported : Boolean; override;
         // Number of samples (pixels) drawn during the query, some pixels may
         // be drawn to several times in the same query
         function PixelCount : Integer;
   end;

   // TGLTimerQueryHandle
   //
   {: Manages a handle to a timer query.<br>
      Requires GL_EXT_timer_query extension.<br>
      Does *NOT* check for extension availability, this is assumed to have been
      checked by the user. }
   TGLTimerQueryHandle = class(TGLQueryHandle)
      protected
         function GetTarget: TGLuint; override;
         function GetQueryType: TQueryType; override;
      public
         class function IsSupported : Boolean; override;
         // Time, in nanoseconds (1 ns = 10^-9 s) between starting + ending the query.
         // with 32 bit integer can measure up to approximately 4 seconds, use
         // QueryResultUInt64 if you may need longer
         function Time: Integer;
   end;

   // TGLPrimitiveQueryHandle
   //
   {: Manages a handle to a primitive query.<br>
      Requires OpenGL 3.0+<br>
      Does *NOT* check for extension availability, this is assumed to have been
      checked by the user. }
   TGLPrimitiveQueryHandle = class(TGLQueryHandle)
      protected
         function GetTarget: TGLuint; override;
         function GetQueryType: TQueryType; override;
      public
         class function IsSupported : Boolean; override;
         // Number of primitives (eg. Points, Triangles etc.) drawn whilst the
         // query was active
         function PrimitivesGenerated: Integer;
   end;

   // TGLBufferObjectHandle
   //
   {: Manages a handle to a Buffer Object.<br>
      Does *NOT* check for extension availability, this is assumed to have been
      checked by the user.<br> }
   TGLBufferObjectHandle = class (TGLContextHandle)
      private
         { Private Declarations }
      protected
         { Protected Declarations }
         function DoAllocateHandle : Cardinal; override;
         procedure DoDestroyHandle; override;

         function GetTarget: TGLuint; virtual; abstract;

      public
         { Public Declarations }
         {: Creates the buffer object buffer and initializes it. }
         constructor CreateFromData(p : Pointer; size : Integer; bufferUsage : TGLuint);

         procedure Bind;
         {: Note that it is not necessary to UnBind before Binding another buffer. }
         procedure UnBind;

         {: Bind a buffer object to an indexed target, used by transform feedback
            buffer objects and uniform buffer objects. (OpenGL 3.0+) }
         procedure BindRange(index: TGLuint; offset: TGLintptr; size: TGLsizeiptr);
         {: Equivalent to calling BindRange with offset = 0, and size = the size of buffer.}
         procedure BindBase(index: TGLuint);
         procedure UnBindBase(index: TGLuint);

         {: Specifies buffer content.<p>
            Common bufferUsage values are GL_STATIC_DRAW_ARB for data that will
            change rarely, but be used often, GL_STREAM_DRAW_ARB for data specified
            once but used only a few times, and GL_DYNAMIC_DRAW_ARB for data
            that is re-specified very often.<p>
            Valid only if the buffer has been bound. }
         procedure BufferData(p : Pointer; size : Integer; bufferUsage : TGLuint);
         //: Invokes Bind then BufferData
         procedure BindBufferData(p : Pointer; size : Integer; bufferUsage : TGLuint);
         {: Updates part of an already existing buffer.<p>
            offset and size indicate which part of the data in the buffer is
            to bo modified and p where the data should be taken from. }
         procedure BufferSubData(offset, size : Integer; p : Pointer);
         {: Map buffer content to memory.<p>
            Values for access are GL_READ_ONLY_ARB, GL_WRITE_ONLY_ARB and
            GL_READ_WRITE_ARB.<p>
            Valid only if the buffer has been bound, must be followed by
            an UnmapBuffer, only one buffer may be mapped at a time. }
         function MapBuffer(access : TGLuint) : Pointer;
         {: Unmap buffer content from memory.<p>
            Must follow a MapBuffer, and happen before the buffer is unbound. }
         function UnmapBuffer : Boolean;

         class function IsSupported : Boolean; override;

         property Target : TGLuint read GetTarget;
   end;

   // TGLVBOHandle
   //
   {: Manages a handle to an Vertex Buffer Object.<br>
      Does *NOT* check for extension availability, this is assumed to have been
      checked by the user.<br>
      Do not use this class directly, use one of its subclasses instead. }
   TGLVBOHandle = class (TGLBufferObjectHandle)
      private
         { Private Declarations }

         function GetVBOTarget: TGLuint;
      public

         property VBOTarget : TGLuint read GetVBOTarget;
   end;

   // TGLVBOArrayBufferHandle
   //
   {: Manages a handle to VBO Array Buffer.<p>
      Typically used to store vertices, normals, texcoords, etc. }
   TGLVBOArrayBufferHandle = class (TGLVBOHandle)
      protected
         function GetTarget: TGLuint; override;
   end;

   // TGLVBOElementArrayHandle
   //
   {: Manages a handle to VBO Element Array Buffer.<p>
      Typically used to store vertex indices. }
   TGLVBOElementArrayHandle = class (TGLVBOHandle)
      protected
         function GetTarget: TGLuint; override;
   end;

   // TGLPackPBOHandle
   //
   {: Manages a handle to PBO Pixel Pack Buffer.<p>
      When bound, commands such as ReadPixels write
      their data into a buffer object. }
   TGLPackPBOHandle = class(TGLBufferObjectHandle)
      protected
         function GetTarget: TGLuint; override;
      public
         class function IsSupported : Boolean; override;
   end;

   // TGLUnpackPBOHandle
   //
   {: Manages a handle to PBO Pixel Unpack Buffer.<p>
      When bound, commands such as DrawPixels read
      their data from a buffer object. }
   TGLUnpackPBOHandle = class(TGLBufferObjectHandle)
     protected
       function GetTarget: TGLuint; override;
      public
         class function IsSupported : Boolean; override;
   end;

   // TGLTransformFeedbackBufferHandle
   //
   {: Manages a handle to a Transform Feedback Buffer Object.<p>
      Transform feedback buffers can be used to capture vertex data from the
      vertex or geometry shader stage to perform further processing without
      going on to the fragment shader stage. }
   TGLTransformFeedbackBufferHandle = class(TGLBufferObjectHandle)
//    FTransformFeedbackBufferBuffer: array[0..15] of TGLuint; // (0, 0, 0, ...)
//    FTransformFeedbackBufferStart: array[0..15] of TGLuint64; // (0, 0, 0, ...)
//    FTransformFeedbackBufferSize: array[0..15] of TGLuint64; // (0, 0, 0, ...)
      protected
         function GetTarget: TGLuint; override;
      public
         procedure BeginTransformFeedback(primitiveMode: TGLenum);
         procedure EndTransformFeedback();

         class function IsSupported : Boolean; override;
   end;

   // TGLTextureBufferHandle
   //
   {: Manages a handle to a Buffer Texture. (TexBO) }
   TGLTextureBufferHandle = class(TGLBufferObjectHandle)
      protected
         function GetTarget: TGLuint; override;
      public
         class function IsSupported : Boolean; override;
   end;

   // TGLUniformBufferHandle
   //
   {: Manages a handle to a Uniform Buffer Object (UBO).
      Uniform buffer objects store "uniform blocks"; groups of uniforms
      that can be passed as a group into a GLSL program. }
   TGLUniformBufferHandle = class(TGLBufferObjectHandle)
//    FUniformBufferBuffer: array[0..15] of TGLuint; // (0, 0, 0, ...)
//    FUniformBufferStart: array[0..15] of TGLuint64; // (0, 0, 0, ...)
//    FUniformBufferSize: array[0..15] of TGLuint64; // (0, 0, 0, ...)
      protected
         function GetTarget: TGLuint; override;
      public
         class function IsSupported : Boolean; override;
   end;

   // TGLVertexArrayHandle
   //
   {: Manages a handle to a Vertex Array Object (VAO).
      Vertex array objects are used to rapidly switch between large sets
      of array state. }
   TGLVertexArrayHandle = class(TGLContextHandle)
      protected
         class function Transferable : Boolean; override;
         function DoAllocateHandle : Cardinal; override;
         procedure DoDestroyHandle; override;
      public
         procedure Bind;
         {: Note that it is not necessary to unbind before binding another VAO. }
         procedure UnBind;
         class function IsSupported : Boolean; override;
   end;

   // TGLFramebufferHandle
   //
   {: Manages a handle to a Framebuffer Object (FBO).
      Framebuffer objects provide a way of drawing to rendering
      destinations other than the buffers provided to the GL by the
      window-system.  One or more "framebuffer-attachable images" can be attached
      to a Framebuffer for uses such as: offscreen rendering, "render to texture" +
      "multiple render targets" (MRT).
      There are several types of framebuffer-attachable images:
      - The image of a renderbuffer object, which is always 2D.
      - A single level of a 1D texture, which is treated as a 2D image with a height of one.
      - A single level of a 2D or rectangle texture.
      - A single face of a cube map texture level, which is treated as a 2D image.
      - A single layer of a 1D or 2D array texture or 3D texture, which is treated as a 2D image.
      Additionally, an entire level of a 3D texture, cube map texture,
      or 1D or 2D array texture can be attached to an attachment point.
      Such attachments are treated as an array of 2D images, arranged in
      layers, and the corresponding attachment point is considered to be layered. }
   TGLFramebufferHandle = class(TGLContextHandle)
      protected
         class function Transferable : Boolean; override;
         function DoAllocateHandle : Cardinal; override;
         procedure DoDestroyHandle; override;
      public
         // Bind framebuffer for both drawing + reading
         procedure Bind;
         // Bind framebuffer for drawing
         procedure BindForDrawing;
         // Bind framebuffer for reading
         procedure BindForReading;
         {: Note that it is not necessary to unbind before binding another framebuffer. }
         procedure UnBind;
         procedure UnBindForDrawing;
         procedure UnBindForReading;
         // target = GL_DRAW_FRAMEBUFFER, GL_READ_FRAMEBUFFER, GL_FRAMEBUFFER (attach to both READ + DRAW)
         // attachment = COLOR_ATTACHMENTi, DEPTH_ATTACHMENT, STENCIL_ATTACHMENT, DEPTH_STENCIL_ATTACHMENT
         procedure Attach1DTexture(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint);
         procedure Attach2DTexture(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint);
         procedure Attach3DTexture(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint; layer: TGLint);
         procedure AttachLayer(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint; layer: TGLint);
         procedure AttachRenderBuffer(target: TGLenum; attachment: TGLenum; renderbuffertarget: TGLenum; renderbuffer: TGLuint);
         // OpenGL 3.2+ only.
         // If texture is the name of a three-dimensional texture, cube map texture, one-or
         // two-dimensional array texture, or two-dimensional multisample array texture, the
         // texture level attached to the framebuffer attachment point is an array of images,
         // and the framebuffer attachment is considered layered.
         procedure AttachTexture(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint);
         // OpenGL 3.2+ only
         procedure AttachTextureLayer(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint; layer: TGLint);

         // copy rect from bound read framebuffer to bound draw framebuffer
         procedure Blit(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint;
			                  dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint;
			                  mask: TGLbitfield; filter: TGLenum);
         // target = GL_DRAW_FRAMEBUFFER, GL_READ_FRAMEBUFFER, GL_FRAMEBUFFER (equivalent to GL_DRAW_FRAMEBUFFER)
         // If default framebuffer (0) is bound:
         // attachment = GL_FRONT_LEFT, GL_FRONT_RIGHT, GL_BACK_LEFT, or GL_BACK_RIGHT, GL_DEPTH, GL_STENCIL
         // if a framebuffer object is bound:
         // attachment = GL_COLOR_ATTACHMENTi, GL_DEPTH_ATTACHMENT, GL_STENCIL_ATTACHMENT, GL_DEPTH_STENCIL_ATTACHMENT
         // param = GL_FRAMEBUFFER_ATTACHMENT_(OBJECT_TYPE, OBJECT_NAME,
         //       RED_SIZE, GREEN_SIZE, BLUE_SIZE, ALPHA_SIZE, DEPTH_SIZE, STENCIL_SIZE,
         //       COMPONENT_TYPE, COLOR_ENCODING, TEXTURE_LEVEL, LAYERED, TEXTURE_CUBE_MAP_FACE, TEXTURE_LAYER
         function GetAttachmentParameter(target: TGLenum; attachment: TGLenum; pname: TGLenum): TGLint;
         // Returns the type of object bound to attachment point:
         // GL_NONE, GL_FRAMEBUFFER_DEFAULT, GL_TEXTURE, or GL_RENDERBUFFER
         function GetAttachmentObjectType(target: TGLenum; attachment: TGLenum): TGLint;
         // Returns the name (ID) of the texture or renderbuffer attached to attachment point
         function GetAttachmentObjectName(target: TGLenum; attachment: TGLenum): TGLint;

         function CheckStatus(target: TGLenum): TGLenum;
         class function IsSupported : Boolean; override;
  end;

   // TGLRenderbufferHandle
   //
   {: Manages a handle to a Renderbuffer Object.
      A Renderbuffer is a "framebuffer-attachable image" for generalized offscreen
      rendering and it also provides a means to support rendering to GL logical
      buffer types which have no corresponding texture format (stencil, accum, etc). }
   TGLRenderbufferHandle = class(TGLContextHandle)
      protected
         function DoAllocateHandle : Cardinal; override;
         procedure DoDestroyHandle; override;
      public
         procedure Bind;
         procedure UnBind;
         procedure SetStorage(internalformat: TGLenum; width, height: TGLsizei);
         procedure SetStorageMultisample(internalformat: TGLenum; samples: TGLsizei; width, height: TGLsizei);
         class function IsSupported : Boolean; override;
   end;

   // TGLSLHandle
   //
   {: Base class for GLSL handles (programs and shaders).<p>
      Do not use this class directly, use one of its subclasses instead. }
   TGLSLHandle = class (TGLContextHandle)
      private
         { Private Declarations }

      protected
         { Protected Declarations }
         procedure DoDestroyHandle; override;
         
      public
         { Public Declarations }
         function InfoLog : String;
         class function IsSupported : Boolean; override;
   end;

   // TGLShaderHandle
   //
   {: Manages a handle to a Shader Object.<br>
      Does *NOT* check for extension availability, this is assumed to have been
      checked by the user.<br>
      Do not use this class directly, use one of its subclasses instead. }
   TGLShaderHandle = class (TGLSLHandle)
      private
         { Private Declarations }
         FShaderType : Cardinal;

      protected
         { Protected Declarations }
         function DoAllocateHandle : Cardinal; override;

      public
         { Public Declarations }
         procedure ShaderSource(const source : String); overload;
         //: Returns True if compilation sucessful
         function CompileShader : Boolean;

         property ShaderType : Cardinal read FShaderType;
   end;

   TGLShaderHandleClass = class of TGLShaderHandle;

   // TGLVertexShaderHandle
   //
   {: Manages a handle to a Vertex Shader Object. }
   TGLVertexShaderHandle = class (TGLShaderHandle)
      public
         { Public Declarations }
         constructor Create; override;
         class function IsSupported : Boolean; override;
   end;

   // TGLGeometryShaderHandle
   //
   {: Manages a handle to a Geometry Shader Object. }
   TGLGeometryShaderHandle = class (TGLShaderHandle)
      public
         { Public Declarations }
         constructor Create; override;
         class function IsSupported : Boolean; override;
   end;

   // TGLFragmentShaderHandle
   //
   {: Manages a handle to a Fragment Shader Object. }
   TGLFragmentShaderHandle = class (TGLShaderHandle)
      public
         { Public Declarations }
         constructor Create; override;
         class function IsSupported : Boolean; override;
   end;

   // TGLProgramHandle
   //
   {: Manages a GLSL Program Object.<br>
      Does *NOT* check for extension availability, this is assumed to have been
      checked by the user.<br> }
   TGLProgramHandle = class (TGLSLHandle)
      private
         { Private Declarations }
         FName: string;
         function GetUniform1i(const index : String) : Integer;
         procedure SetUniform1i(const index : String; val : Integer);
         function GetUniform2i(const index: String): TVector2i;
         procedure SetUniform2i(const index: String; const Value: TVector2i);
         function GetUniform3i(const index: String): TVector3i;
         procedure SetUniform3i(const index: String; const Value: TVector3i);
         function GetUniform4i(const index: String): TVector4i;
         procedure SetUniform4i(const index: String; const Value: TVector4i);

         function GetUniform1f(const index : String) : Single;
         procedure SetUniform1f(const index : String; val : Single);
         function GetUniform2f(const index : String) : TVector2f;
         procedure SetUniform2f(const index : String; const val : TVector2f);
         function GetUniform3f(const index : String) : TAffineVector;
         procedure SetUniform3f(const index : String; const val : TAffineVector);
         function GetUniform4f(const index : String) : TVector;
         procedure SetUniform4f(const index : String; const val : TVector);

         function GetUniformMatrix2fv(const index : String) : TMatrix2f;
         procedure SetUniformMatrix2fv(const index : String; const val : TMatrix2f);
         function GetUniformMatrix3fv(const index : String) : TMatrix3f;
         procedure SetUniformMatrix3fv(const index : String; const val : TMatrix3f);
         function GetUniformMatrix4fv(const index : String) : TMatrix;
         procedure SetUniformMatrix4fv(const index : String; const val : TMatrix);

        function GetUniformTextureHandle(const index: string;
          const TextureIndex: Integer; const TextureTarget: Cardinal): Cardinal;
        procedure SetUniformTextureHandle(const index: string;
          const TextureIndex: Integer; const TextureTarget, Value: Cardinal);
        procedure SetUniformBuffer(const index: string;
          Value: TGLUniformBufferHandle);
      protected
         { Protected Declarations }
         function DoAllocateHandle : cardinal; override;

      public
         { Public Declarations }
         property Name : string read FName write FName;

         constructor Create; override;

         {: Compile and attach a new shader.<p>
            Raises an EGLShader exception in case of failure. }
         procedure AddShader(shaderType : TGLShaderHandleClass; const shaderSource : String;
                             treatWarningsAsErrors : Boolean = False);

         procedure AttachObject(shader : TGLShaderHandle);
         procedure BindAttribLocation(index : Integer; const aName : String);
         procedure BindFragDataLocation(index: Integer; const aName : String);
         function LinkProgram : Boolean;
         function ValidateProgram : Boolean;
         function GetAttribLocation(const aName : String) : Integer;
         function GetUniformLocation(const aName : String) : Integer;
         function GetUniformOffset(const aName : String) : PGLInt;
         function GetUniformBlockIndex(const aName : String) : Integer;

         function GetVaryingLocation(const aName : String) : Integer; // Currently, NVidia-specific.
         procedure AddActiveVarying(const aName : String);            // Currently, NVidia-specific.

         function GetUniformBufferSize(const aName : String) : Integer;

         procedure UseProgramObject;
         procedure EndUseProgramObject;

         procedure SetUniformi(const index : String; const val: integer);   overload;
         procedure SetUniformi(const index : String; const val: TVector2i); overload;
         procedure SetUniformi(const index : String; const val: TVector3i); overload;
         procedure SetUniformi(const index : String; const val: TVector4i); overload;

         procedure SetUniformf(const index : String; const val: single);    overload;
         procedure SetUniformf(const index : String; const val: TVector2f); overload;
         procedure SetUniformf(const index : String; const val: TVector3f); overload;
         procedure SetUniformf(const index : String; const val: TVector4f); overload;

         {: Shader parameters. }
         property Uniform1i[const index : String] : Integer read GetUniform1i write SetUniform1i;
         property Uniform2i[const index : String] : TVector2i read GetUniform2i write SetUniform2i;
         property Uniform3i[const index : String] : TVector3i read GetUniform3i write SetUniform3i;
         property Uniform4i[const index : String] : TVector4i read GetUniform4i write SetUniform4i;

         property Uniform1f[const index : String] : Single read GetUniform1f write SetUniform1f;
         property Uniform2f[const index : String] : TVector2f read GetUniform2f write SetUniform2f;
         property Uniform3f[const index : String] : TAffineVector read GetUniform3f write SetUniform3f;
         property Uniform4f[const index : String] : TVector read GetUniform4f write SetUniform4f;

         property UniformMatrix2fv[const index : String] : TMatrix2f read GetUniformMatrix2fv write SetUniformMatrix2fv;
         property UniformMatrix3fv[const index : String] : TMatrix3f read GetUniformMatrix3fv write SetUniformMatrix3fv;
         property UniformMatrix4fv[const index : String] : TMatrix read GetUniformMatrix4fv write SetUniformMatrix4fv;

         property UniformTextureHandle[const index: string; const TextureIndex: Integer; const TextureTarget: Cardinal]: Cardinal read GetUniformTextureHandle write SetUniformTextureHandle;
         property UniformBuffer[const index: string]: TGLUniformBufferHandle write SetUniformBuffer;
   end;

   // TGLContextNotification
   //
   TGLContextNotification = record
      obj : TObject;
      event : TNotifyEvent;
   end;

   // TGLContextManager
   //
   {: Stores and manages all the TGLContext objects.<p> }
   TGLContextManager = class
      private
         { Private Declarations }
         FList : TThreadList;
         FTerminated : Boolean;
         FNotifications : array of TGLContextNotification;
         FCreatedRCCount : Integer;

      protected
         { Protected Declarations }
         procedure Lock;
         procedure UnLock;

         procedure RegisterContext(aContext : TGLContext);
         procedure UnRegisterContext(aContext : TGLContext);

         procedure ContextCreatedBy(aContext : TGLContext);
         procedure DestroyingContextBy(aContext : TGLContext);

      public
         { Public Declarations }
         constructor Create;
         destructor Destroy; override;

         {: Returns an appropriate, ready-to use context.<p>
            The returned context should be freed by caller. }
         function CreateContext : TGLContext;

         {: Returns the number of TGLContext object.<p>
            This is *not* the number of OpenGL rendering contexts! }
         function ContextCount : Integer;
         {: Registers a new object to notify when the last context is destroyed.<p>
            When the last rendering context is destroyed, the 'anEvent' will
            be invoked with 'anObject' as parameter.<br>
            Note that the registration is kept until the notification is triggered
            or a RemoveNotification on 'anObject' is issued. }
         procedure LastContextDestroyNotification(anObject : TObject; anEvent : TNotifyEvent);
         {: Unregisters an object from the notification lists.<p> }
         procedure RemoveNotification(anObject : TObject);

         //: Marks the context manager for termination
         procedure Terminate;

         {: Request all contexts to destroy all their handles. }
         procedure DestroyAllHandles;
   end;

   EGLContext = class(Exception);

   EGLShader = class(EGLContext);

{: Drivers should register themselves via this function. }
procedure RegisterGLContextClass(aGLContextClass : TGLContextClass);
{: The TGLContext that is the currently active context, if any.<p>
   Returns nil if no context is active. }
function CurrentGLContext : TGLContext;

resourcestring
   cIncompatibleContexts =       'Incompatible contexts';
   cDeleteContextFailed =        'Delete context failed';
   cContextActivationFailed =    'Context activation failed: %X, %s';
   cContextDeactivationFailed =  'Context deactivation failed';
   cUnableToCreateLegacyContext= 'Unable to create legacy context';

var
   GLContextManager : TGLContextManager;
   vIgnoreOpenGLErrors : Boolean = False;
   vContextActivationFailureOccurred : Boolean = false;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

resourcestring
   cCannotAlterAnActiveContext = 'Cannot alter an active context';
   cInvalidContextRegistration = 'Invalid context registration';
   cInvalidNotificationRemoval = 'Invalid notification removal';
   cContextAlreadyCreated =      'Context already created';
   cContextNotCreated =          'Context not created';
   cUnbalancedContexActivations= 'Unbalanced context activations';
   
var
   vContextClasses : TList = nil;
{$IFNDEF GLS_MULTITHREAD}
var
{$ELSE}
threadvar
{$ENDIF}
   vCurrentGLContext : TGLContext;

// CurrentGLContext
//
function CurrentGLContext : TGLContext;
begin
   Result:=vCurrentGLContext;
end;

// RegisterGLContextClass
//
procedure RegisterGLContextClass(aGLContextClass : TGLContextClass);
begin
   if not Assigned(vContextClasses) then
      vContextClasses:=TList.Create;
   vContextClasses.Add(aGLContextClass);
end;

// ------------------
// ------------------ TGLContext ------------------
// ------------------

// Create
//
constructor TGLContext.Create;
begin
   inherited Create;
{$IFDEF GLS_MULTITHREAD}
   InitializeCriticalSection(FLock);
{$ENDIF}
   FColorBits:=32;
   FStencilBits:=0;
   FAccumBits:=0;
   FAuxBuffers:=0;
   FOptions:=[];
{$IFNDEF GLS_MULTITHREAD}
   FSharedContexts:=TList.Create;
   FOwnedHandles:=TList.Create;
{$ELSE}
   FSharedContexts:=TThreadList.Create;
   FOwnedHandles:=TThreadList.Create;
{$ENDIF}
   FAcceleration:=chaUnknown;
   FGLStates := TGLStateCache.Create;
   GLContextManager.RegisterContext(Self);
end;

// Destroy
//
destructor TGLContext.Destroy;
begin
   if IsValid then
      DestroyContext;
   GLContextManager.UnRegisterContext(Self);
   FGLStates.Free;
   FOwnedHandles.Free;
   FSharedContexts.Free;
{$IFDEF GLS_MULTITHREAD}
   DeleteCriticalSection(FLock);
{$ENDIF}
   inherited Destroy;
end;

// SetColorBits
//
procedure TGLContext.SetColorBits(const aColorBits : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FColorBits:=aColorBits;
end;

// SetAlphaBits
//
procedure TGLContext.SetAlphaBits(const aAlphaBits : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FAlphaBits:=aAlphaBits;
end;

// SetDepthBits
//
procedure TGLContext.SetDepthBits(const val : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FDepthBits:=val;
end;

// SetStencilBits
//
procedure TGLContext.SetStencilBits(const aStencilBits : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FStencilBits:=aStencilBits;
end;

// SetAccumBits
//
procedure TGLContext.SetAccumBits(const aAccumBits : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FAccumBits:=aAccumBits;
end;

// SetAuxBuffers
//
procedure TGLContext.SetAuxBuffers(const aAuxBuffers : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FAuxBuffers:=aAuxBuffers;
end;

// SetOptions
//
procedure TGLContext.SetOptions(const aOptions : TGLRCOptions);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FOptions:=aOptions;
end;

// SetAntiAliasing
//
procedure TGLContext.SetAntiAliasing(const val : TGLAntiAliasing);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FAntiAliasing:=val;
end;

// GetActive
//
function TGLContext.GetActive : Boolean;
begin
   Result:=(FActivationCount>0);
end;

// SetActive
//
procedure TGLContext.SetActive(const aActive : Boolean);
begin
   // activation/deactivation can be nested...
   while aActive<>Active do begin
      if aActive then
         Activate
      else Deactivate;
   end;
end;

// CreateContext
//
procedure TGLContext.CreateContext(outputDevice : HDC);
begin
   if IsValid then
      raise EGLContext.Create(cContextAlreadyCreated);
   FAcceleration:=chaUnknown;
   DoCreateContext(outputDevice);
   FSharedContexts.Add(Self);
   Manager.ContextCreatedBy(Self);
end;

// CreateMemoryContext
//
procedure TGLContext.CreateMemoryContext(outputDevice: HDC;
  width, height : Integer; BufferCount : integer);
begin
   if IsValid then
      raise EGLContext.Create(cContextAlreadyCreated);
   FAcceleration:=chaUnknown;
   DoCreateMemoryContext(outputDevice, width, height, BufferCount);
   FSharedContexts.Add(Self);
   Manager.ContextCreatedBy(Self);
end;

// PropagateSharedContext
//
procedure TGLContext.PropagateSharedContext;
var
   i, j : Integer;
   otherContext : TGLContext;
begin
{$IFNDEF GLS_MULTITHREAD}
   for i:=0 to FSharedContexts.Count-1 do begin
      if TGLContext(FSharedContexts[i])<>Self then begin
         otherContext:=TGLContext(FSharedContexts[i]);
         otherContext.FSharedContexts.Clear;
         for j:=0 to FSharedContexts.Count-1 do
            otherContext.FSharedContexts.Add(FSharedContexts[j]);
      end;
   end;
{$ELSE}
  with FSharedContexts.LockList do
  try
    for i:=0 to Count-1 do begin
      if TGLContext(Items[i])<>Self then begin
         otherContext:=TGLContext(Items[i]);
         otherContext.FSharedContexts.Clear;
         for j:=0 to Count-1 do
            otherContext.FSharedContexts.Add(Items[j]);
      end;
    end;
  finally
    FSharedContexts.UnlockList;
  end;
{$ENDIF}
end;

// ShareLists
//
procedure TGLContext.ShareLists(aContext : TGLContext);
begin
   if IsValid then begin
{$IFNDEF GLS_MULTITHREAD}
      if FSharedContexts.IndexOf(aContext)<0 then begin
         DoShareLists(aContext);
         FSharedContexts.Add(aContext);
         PropagateSharedContext;
      end;
{$ELSE}
   with FSharedContexts.LockList do
   try
      if IndexOf(aContext)<0 then begin
         DoShareLists(aContext);
         Add(aContext);
         PropagateSharedContext;
      end;
   finally
     FSharedContexts.UnlockList;
   end;
{$ENDIF}
   end else raise EGLContext.Create(cContextNotCreated);
end;

// DestroyAllHandles
//
procedure TGLContext.DestroyAllHandles;
var
   i : Integer;
begin
   Activate;
   try
{$IFNDEF GLS_MULTITHREAD}
      for i:=FOwnedHandles.Count-1 downto 0 do
         TGLContextHandle(FOwnedHandles[i]).DestroyHandle;
{$ELSE}
     with FOwnedHandles.LockList do
     try
      for i:=Count-1 downto 0 do
         TGLContextHandle(Items[i]).DestroyHandle;
     finally
       FOwnedHandles.UnlockList;
     end;
{$ENDIF}
   finally
      Deactivate;
   end;
end;

// DestroyContext
//
procedure TGLContext.DestroyContext;
var
   i : Integer;
   oldContext, compatContext : TGLContext;
   contextHandle : TGLContextHandle;
{$IFNDEF GLS_MULTITHREAD}
begin
   if vCurrentGLContext<>Self then begin
      oldContext:=vCurrentGLContext;
      if Assigned(oldContext) then
         oldContext.Deactivate;
   end else oldContext:=nil;
   Activate;
   try
      compatContext:=FindCompatibleContext;
      if Assigned(compatContext) then begin
         // transfer handle ownerships to the compat context
         for i:=FOwnedHandles.Count-1 downto 0 do begin
            contextHandle:=TGLContextHandle(FOwnedHandles[i]);
            if contextHandle.Transferable then begin
               compatContext.FOwnedHandles.Add(contextHandle);
               contextHandle.FRenderingContext:=compatContext;
            end else contextHandle.ContextDestroying;
         end;
      end else begin
         // no compat context, release handles
         for i:=FOwnedHandles.Count-1 downto 0 do begin
            contextHandle:=TGLContextHandle(FOwnedHandles[i]);
            contextHandle.ContextDestroying;
         end;
      end;
{$ELSE}
   aList:TList;
begin
   if vCurrentGLContext<>Self then begin
      oldContext:=vCurrentGLContext;
      if Assigned(oldContext) then
         oldContext.Deactivate;
   end else oldContext:=nil;
   Activate;
   try
      compatContext:=FindCompatibleContext;
      aList := FOwnedHandles.LockList;
      try
        if Assigned(compatContext) then begin
           // transfer handle ownerships to the compat context
           for i:=aList.Count-1 downto 0 do begin
              contextHandle:=TGLContextHandle(aList[i]);
              if contextHandle.Transferable then begin
                 compatContext.FOwnedHandles.Add(contextHandle);
                 contextHandle.FRenderingContext:=compatContext;
              end else contextHandle.ContextDestroying;
           end;
        end else begin
           // no compat context, release handles
           for i:=aList.Count-1 downto 0 do begin
              contextHandle:=TGLContextHandle(aList[i]);
              contextHandle.ContextDestroying;
           end;
        end;
        aList.Clear;
      finally
        FOwnedHandles.UnlockList;
      end;
{$ENDIF}
      FOwnedHandles.Clear;
      Manager.DestroyingContextBy(Self);
      FSharedContexts.Remove(Self);
      PropagateSharedContext;
      FSharedContexts.Clear;
      Active:=False;
      DoDestroyContext;
   finally
      if Assigned(oldContext) then
         oldContext.Activate;
   end;
   FAcceleration:=chaUnknown;
end;

// Activate
//
procedure TGLContext.Activate;
begin
{$IFDEF GLS_MULTITHREAD}
   EnterCriticalSection(FLock);
{$ENDIF}
   if FActivationCount=0 then begin
      if not IsValid then
         raise EGLContext.Create(cContextNotCreated);
      vContextActivationFailureOccurred:=False;
      try
         DoActivate;
      except
         vContextActivationFailureOccurred:=True;
      end;
      vCurrentGLContext:=Self;
   end else Assert(vCurrentGLContext=Self, 'vCurrentGLContext <> Self');
   Inc(FActivationCount);
end;

// Deactivate
//
procedure TGLContext.Deactivate;
begin
   Assert(vCurrentGLContext=Self);
   Dec(FActivationCount);
   if FActivationCount=0 then begin
      if not IsValid then
         raise EGLContext.Create(cContextNotCreated);
      if not vContextActivationFailureOccurred then
         DoDeactivate;
      vCurrentGLContext:=nil;
   end else if FActivationCount<0 then
      raise EGLContext.Create(cUnbalancedContexActivations);
{$IFDEF GLS_MULTITHREAD}
  LeaveCriticalSection(FLock);
{$ENDIF}
end;

// FindCompatibleContext
//
function TGLContext.FindCompatibleContext : TGLContext;
var
   i : Integer;
begin
   Result:=nil;
{$IFNDEF GLS_MULTITHREAD}
   for i:=0 to FSharedContexts.Count-1 do
      if TGLContext(FSharedContexts[i])<>Self then begin
         Result:=TGLContext(FSharedContexts[i]);
         Break;
      end;
{$ELSE}
   with FSharedContexts.LockList do
   try
     for i:=0 to Count-1 do
       if Items[i]<>Self then begin
         Result:=TGLContext(Items[i]);
         Break;
       end;
   finally
     FSharedContexts.UnlockList;
   end;
{$ENDIF}
end;

// ------------------
// ------------------ TGLContextHandle ------------------
// ------------------

// Create
//
constructor TGLContextHandle.Create;
begin
   inherited Create;
end;

// CreateAndAllocate
//
constructor TGLContextHandle.CreateAndAllocate(failIfAllocationFailed : Boolean = True);
begin
   Create;
   AllocateHandle;
   if failIfAllocationFailed and (Handle=0) then
      raise EGLContext.Create('Auto-allocation failed');
end;

// Destroy
//
destructor TGLContextHandle.Destroy;
begin
   DestroyHandle;
   inherited Destroy;
end;

// AllocateHandle
//
procedure TGLContextHandle.AllocateHandle;
begin
   Assert(FHandle=0);
   Assert(vCurrentGLContext<>nil);
   FHandle:=DoAllocateHandle;
   if FHandle<>0 then begin
      FRenderingContext:=vCurrentGLContext;
      vCurrentGLContext.FOwnedHandles.Add(Self);
   end;
end;

// DestroyHandle
//
procedure TGLContextHandle.DestroyHandle;
var
   oldContext, handleContext : TGLContext;
{$IFNDEF GLS_MULTITHREAD}
begin
   if FHandle<>0 then begin
      FRenderingContext.FOwnedHandles.Remove(Self);
      if (vCurrentGLContext=FRenderingContext)
            or ((vCurrentGLContext<>nil)
                and (vCurrentGLContext.FSharedContexts.IndexOf(FRenderingContext)>=0)) then begin
         // current context is ours or compatible one
         DoDestroyHandle;
      end else begin
         // some other context (or none)
         oldContext:=vCurrentGLContext;
         if Assigned(oldContext) then
            oldContext.Deactivate;
         FRenderingContext.Activate;
         handleContext:=FRenderingContext;
         try
            DoDestroyHandle;
         finally
            handleContext.Deactivate;
            if Assigned(oldContext) then
               oldContext.Activate;
         end;
      end;
      FHandle:=0;
      FRenderingContext:=nil;
   end;
end;   
{$ELSE}
   function HasRenderingContext:boolean;
   var lList:TList;
   begin
     lList := vCurrentGLContext.FSharedContexts.LockList;
     try
       Result := lList.IndexOf(FRenderingContext)>=0;
     finally
       vCurrentGLContext.FSharedContexts.UnLockList;
     end;
   end;
begin
   if FHandle<>0 then begin
      FRenderingContext.FOwnedHandles.Remove(Self);
      if (vCurrentGLContext=FRenderingContext) then
         DoDestroyHandle
      else
        if (vCurrentGLContext<>nil) and HasRenderingContext then
        // May be vCurrentGLContext.FSharedContexts should've remained locked
        begin
           // current context is ours or compatible one
          DoDestroyHandle;
        end
        else
        begin
         // some other context (or none)
          oldContext:=vCurrentGLContext;
          if Assigned(oldContext) then
            oldContext.Deactivate;
          FRenderingContext.Activate;
          handleContext:=FRenderingContext;
          try
            DoDestroyHandle;
          finally
            handleContext.Deactivate;
            if Assigned(oldContext) then
               oldContext.Activate;
          end;
        end;
      FHandle:=0;
      FRenderingContext:=nil;
   end;
end;
{$ENDIF}


// ContextDestroying
//
procedure TGLContextHandle.ContextDestroying;
begin
   if FHandle<>0 then begin
      // we are always in the original context or a compatible context
      DoDestroyHandle;
      FHandle:=0;
      FRenderingContext:=nil;
   end;
end;

// Transferable
//
class function TGLContextHandle.Transferable : Boolean;
begin
   Result:=True;
end;

// IsSupported
//
class function TGLContextHandle.IsSupported : Boolean;
begin
   Result:=True;
end;

// ------------------
// ------------------ TGLVirtualHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLVirtualHandle.DoAllocateHandle : Cardinal;
begin
   Result:=0;
   if Assigned(FOnAllocate) then
      FOnAllocate(Self, Result);
end;

// DoDestroyHandle
//
procedure TGLVirtualHandle.DoDestroyHandle;
begin
   if not vContextActivationFailureOccurred then begin
      // reset error status
      ClearGLError;
      // delete
      if Assigned(FOnDestroy) then
         FOnDestroy(Self, FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// ------------------
// ------------------ TGLListHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLListHandle.DoAllocateHandle : Cardinal;
begin
   Result:=glGenLists(1);
end;

// DoDestroyHandle
//
procedure TGLListHandle.DoDestroyHandle;
begin
   if not vContextActivationFailureOccurred then begin
      // reset error status
      ClearGLError;
      // delete
      glDeleteLists(FHandle, 1);
      // check for error
      CheckOpenGLError;
   end;
end;

// NewList
//
procedure TGLListHandle.NewList(mode : Cardinal);
begin
   glNewList(FHandle, mode);
end;

// EndList
//
procedure TGLListHandle.EndList;
begin
   glEndList;
end;

// CallList
//
procedure TGLListHandle.CallList;
begin
   glCallList(FHandle);
end;

// ------------------
// ------------------ TGLTextureHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLTextureHandle.DoAllocateHandle : Cardinal;
begin
   glGenTextures(1, @Result);
end;

// DoDestroyHandle
//
procedure TGLTextureHandle.DoDestroyHandle;
begin
   if not vContextActivationFailureOccurred then begin
      // reset error status
      glGetError;
      // delete
      if glIsTexture(FHandle) then
    	   glDeleteTextures(1, @FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// ------------------
// ------------------ TGLQueryHandle ------------------
// ------------------

// BeginQuery
//
procedure TGLQueryHandle.BeginQuery;
begin
   Assert(Handle<>0);
   Assert(vCurrentGLContext=RenderingContext, 'Queries are not shareable '+
                                              'across contexts');
   //glBeginQuery(Target, FHandle);
   if vCurrentGLContext.GLStates.CurrentQuery[QueryType] = 0 then
     vCurrentGLContext.GLStates.BeginQuery(QueryType, Handle);
   Factive:=True;
end;

// CounterBits
//
function TGLQueryHandle.CounterBits: integer;
begin
   glGetQueryiv(Target, GL_QUERY_COUNTER_BITS, @Result);
end;

// DoAllocateHandle
//
function TGLQueryHandle.DoAllocateHandle: Cardinal;
begin
   glGenQueries(1, @Result);
end;

// DoDestroyHandle
//
procedure TGLQueryHandle.DoDestroyHandle;
begin
   if not vContextActivationFailureOccurred then begin
      // reset error status
      glGetError;
      // delete
 	    glDeleteQueries(1, @FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// EndQuery
//
procedure TGLQueryHandle.EndQuery;
begin
   Assert(FActive=true, 'Cannot end a query before it begins');
   Factive:=False;
   Assert(Handle<>0);
   //glEndQuery(Target);
   vCurrentGLContext.GLStates.EndQuery(QueryType);
end;

// IsResultAvailable
//
function TGLQueryHandle.IsResultAvailable: boolean;
begin
   glGetQueryObjectiv(Handle, GL_QUERY_RESULT_AVAILABLE, @Result);
end;

// QueryResultInt
//
function TGLQueryHandle.QueryResultInt: TGLInt;
begin
   glGetQueryObjectiv(Handle, GL_QUERY_RESULT, @Result);
end;

// QueryResultInt64
//
function TGLQueryHandle.QueryResultInt64: TGLint64EXT;
begin
   glGetQueryObjecti64vEXT(Handle, GL_QUERY_RESULT, @Result);
end;

// QueryResultUInt
//
function TGLQueryHandle.QueryResultUInt: TGLUInt;
begin
   glGetQueryObjectuiv(Handle, GL_QUERY_RESULT, @Result);
end;

// QueryResultUInt64
//
function TGLQueryHandle.QueryResultUInt64: TGLuint64EXT;
begin
   glGetQueryObjectui64vEXT(Handle, GL_QUERY_RESULT, @Result);
end;

// Transferable
//
class function TGLQueryHandle.Transferable: Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TGLOcclusionQueryHandle ------------------
// ------------------

// GetQueryType
//
function TGLOcclusionQueryHandle.GetQueryType: TQueryType;
begin
   Result := qrySamplesPassed;
end;

// GetTarget
//
function TGLOcclusionQueryHandle.GetTarget: TGLuint;
begin
   Result := GL_SAMPLES_PASSED;
end;

// IsSupported
//
class function TGLOcclusionQueryHandle.IsSupported: Boolean;
begin
   Result := GL_VERSION_1_5;
end;

// PixelCount
//
function TGLOcclusionQueryHandle.PixelCount : Integer;
begin
   Result := QueryResultUInt;
end;

// ------------------
// ------------------ TGLTimerQueryHandle ------------------
// ------------------

// GetTarget
//
function TGLTimerQueryHandle.GetQueryType: TQueryType;
begin
  Result := qryTimeElapsed;
end;

function TGLTimerQueryHandle.GetTarget: TGLuint;
begin
   Result := GL_TIME_ELAPSED_EXT;
end;

// IsSupported
//
class function TGLTimerQueryHandle.IsSupported: Boolean;
begin
   Result := GL_EXT_timer_query;
end;

// Time
//
function TGLTimerQueryHandle.Time: Integer;
begin
   Result := QueryResultUInt;
end;

// ------------------
// ------------------ TGLPrimitiveQueryHandle ------------------
// ------------------

// GetQueryType
//
function TGLPrimitiveQueryHandle.GetQueryType: TQueryType;
begin
   Result := qryPrimitivesGenerated;
end;

// GetTarget
//
function TGLPrimitiveQueryHandle.GetTarget: TGLuint;
begin
   Result := GL_PRIMITIVES_GENERATED;
end;

// IsSupported
//
class function TGLPrimitiveQueryHandle.IsSupported: Boolean;
begin
   Result := GL_VERSION_3_0;
end;

// PrimitivesGenerated
//
function TGLPrimitiveQueryHandle.PrimitivesGenerated: Integer;
begin
   Result := QueryResultUInt;
end;

// ------------------
// ------------------ TGLBufferObjectHandle ------------------
// ------------------

// CreateFromData
//
constructor TGLBufferObjectHandle.CreateFromData(p : Pointer; size : Integer; bufferUsage : TGLuint);
begin
   Create;
   AllocateHandle;
   Bind;
   BufferData(p, size, bufferUsage);
   UnBind;
end;

// DoAllocateHandle
//
function TGLBufferObjectHandle.DoAllocateHandle : Cardinal;
begin
   glGenBuffersARB(1, @Result);
end;

// DoDestroyHandle
//
procedure TGLBufferObjectHandle.DoDestroyHandle;
begin
   if not vContextActivationFailureOccurred then begin
      // reset error status
      glGetError;
      // delete
 	   glDeleteBuffersARB(1, @FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// IsSupported
//
class function TGLBufferObjectHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_vertex_buffer_object;
end;

// Bind
//
procedure TGLBufferObjectHandle.Bind;
begin
   glBindBufferARB(Target, Handle);
end;

// UnBind
//
procedure TGLBufferObjectHandle.UnBind;
begin
   glBindBufferARB(Target, 0);
end;

// BindRange
//
procedure TGLBufferObjectHandle.BindRange(index: TGLuint; offset: TGLintptr; size: TGLsizeiptr);
begin
   glBindBufferRange(Target, index, Handle, offset, size);
end;

// BindBase
//
procedure TGLBufferObjectHandle.BindBase(index: TGLuint);
begin
   glBindBufferBase(Target, index, Handle);
end;

// UnBindBase
//
procedure TGLBufferObjectHandle.UnBindBase(index: TGLuint);
begin
   glBindBufferBase(Target, index, 0);
end;

// BufferData
//
procedure TGLBufferObjectHandle.BufferData(p : Pointer; size : Integer; bufferUsage : TGLuint);
begin
   glBufferDataARB(Target, size, p, bufferUsage);
end;

// BindBufferData
//
procedure TGLBufferObjectHandle.BindBufferData(p : Pointer; size : Integer; bufferUsage : TGLuint);
begin
   glBindBufferARB(Target, Handle);
   glBufferDataARB(Target, size, p, bufferUsage);
end;

// BufferSubData
//
procedure TGLBufferObjectHandle.BufferSubData(offset, size : Integer; p : Pointer);
begin
   glBufferSubDataARB(Target, offset, size, p);
end;

// MapBuffer
//
function TGLBufferObjectHandle.MapBuffer(access : TGLuint) : Pointer;
begin
   Result:=glMapBufferARB(Target, access);
end;

// UnmapBuffer
//
function TGLBufferObjectHandle.UnmapBuffer : Boolean;
begin
   Result:=glUnmapBufferARB(Target);
end;

// ------------------
// ------------------ TGLVBOHandle ------------------
// ------------------

// GetVBOTarget
//
function TGLVBOHandle.GetVBOTarget: TGLuint;
begin
   Result:=Target;
end;

// ------------------
// ------------------ TGLVBOArrayBufferHandle ------------------
// ------------------

// GetTarget
//
function TGLVBOArrayBufferHandle.GetTarget: TGLuint;
begin
   Result:=GL_ARRAY_BUFFER_ARB;
end;

// ------------------
// ------------------ TGLVBOElementArrayHandle ------------------
// ------------------

// GetTarget
//
function TGLVBOElementArrayHandle.GetTarget: TGLuint;
begin
   Result:=GL_ELEMENT_ARRAY_BUFFER_ARB;
end;

// ------------------
// ------------------ TGLPackPBOHandle ------------------
// ------------------

// GetTarget
//
function TGLPackPBOHandle.GetTarget: TGLuint;
begin
   Result:=GL_PIXEL_PACK_BUFFER_ARB;
end;

// IsSupported
//
class function TGLPackPBOHandle.IsSupported: Boolean;
begin
   Result := GL_ARB_pixel_buffer_object;
end;

// ------------------
// ------------------ TGLUnpackPBOHandle ------------------
// ------------------

// GetTarget
//
function TGLUnpackPBOHandle.GetTarget: TGLuint;
begin
   Result:=GL_PIXEL_UNPACK_BUFFER_ARB;
end;

// IsSupported
//
class function TGLUnpackPBOHandle.IsSupported: Boolean;
begin
   Result := GL_ARB_pixel_buffer_object;
end;

// ------------------
// ------------------ TGLTransformFeedbackBufferHandle ------------------
// ------------------

// GetTarget
//
function TGLTransformFeedbackBufferHandle.GetTarget: TGLuint;
begin
   Result := GL_TRANSFORM_FEEDBACK_BUFFER;
end;

// BeginTransformFeedback
//
procedure TGLTransformFeedbackBufferHandle.BeginTransformFeedback(primitiveMode: TGLenum);
begin
   glBeginTransformFeedback(primitiveMode);
end;

// EndTransformFeedback
//
procedure TGLTransformFeedbackBufferHandle.EndTransformFeedback();
begin
   glEndTransformFeedback();
end;

// IsSupported
//
class function TGLTransformFeedbackBufferHandle.IsSupported: Boolean;
begin
   Result := {GL_EXT_transform_feedback or }GL_VERSION_3_0;
end;

// ------------------
// ------------------ TGLTextureBufferHandle ------------------
// ------------------

// GetTarget
//
function TGLTextureBufferHandle.GetTarget: TGLuint;
begin
   Result := GL_TEXTURE_BUFFER;
end;

// IsSupported
//
class function TGLTextureBufferHandle.IsSupported: Boolean;
begin
   Result := GL_EXT_texture_buffer_object or GL_ARB_texture_buffer_object or GL_VERSION_3_1;
end;

// ------------------
// ------------------ TGLUniformBufferHandle ------------------
// ------------------

// GetTarget
//
function TGLUniformBufferHandle.GetTarget: TGLuint;
begin
   Result := GL_UNIFORM_BUFFER;
end;

// IsSupported
//
class function TGLUniformBufferHandle.IsSupported: Boolean;
begin
   Result := GL_ARB_uniform_buffer_object;
end;

// ------------------
// ------------------ TGLVertexArrayHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLVertexArrayHandle.DoAllocateHandle: Cardinal;
begin
   glGenVertexArrays(1, @Result);
end;

// DoDestroyHandle
//
procedure TGLVertexArrayHandle.DoDestroyHandle;
begin
   if not vContextActivationFailureOccurred then begin
      // reset error status
      glGetError;
      // delete
 	    glDeleteVertexArrays(1, @FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// Bind
//
procedure TGLVertexArrayHandle.Bind;
begin
   //glBindVertexArray(Handle);
   Assert(vCurrentGLContext<>nil);
   vCurrentGLContext.GLStates.VertexArrayBinding := Handle;
end;

// UnBind
//
procedure TGLVertexArrayHandle.UnBind;
begin
//   glBindVertexArray(0);
   Assert(vCurrentGLContext<>nil);
   vCurrentGLContext.GLStates.VertexArrayBinding := 0;
end;

// IsSupported
//
class function TGLVertexArrayHandle.IsSupported: Boolean;
begin
   Result := GL_ARB_vertex_array_object;
end;

// Transferable
//
class function TGLVertexArrayHandle.Transferable: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TGLFramebufferHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLFramebufferHandle.DoAllocateHandle: Cardinal;
begin
   glGenFramebuffers(1, @Result);
end;

// DoDestroyHandle
//
procedure TGLFramebufferHandle.DoDestroyHandle;
begin
   if not vContextActivationFailureOccurred then begin
      // reset error status
      glGetError;
      // delete
 	    glDeleteFramebuffers(1, @FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// Bind
//
procedure TGLFramebufferHandle.Bind;
begin
   Assert(vCurrentGLContext<>nil);
   vCurrentGLContext.GLStates.SetFrameBuffer(Handle);
end;

// BindForDrawing
//
procedure TGLFramebufferHandle.BindForDrawing;
begin
   Assert(vCurrentGLContext<>nil);
   vCurrentGLContext.GLStates.DrawFrameBuffer := Handle;
end;

// BindForReading
//
procedure TGLFramebufferHandle.BindForReading;
begin
   Assert(vCurrentGLContext<>nil);
   vCurrentGLContext.GLStates.ReadFrameBuffer := Handle;
end;

// UnBind
//
procedure TGLFramebufferHandle.UnBind;
begin
   Assert(vCurrentGLContext<>nil);
   vCurrentGLContext.GLStates.SetFrameBuffer(0);
end;

// UnBindForDrawing
//
procedure TGLFramebufferHandle.UnBindForDrawing;
begin
   Assert(vCurrentGLContext<>nil);
   vCurrentGLContext.GLStates.DrawFrameBuffer := 0;
end;

// UnBindForReading
//
procedure TGLFramebufferHandle.UnBindForReading;
begin
   Assert(vCurrentGLContext<>nil);
   vCurrentGLContext.GLStates.ReadFrameBuffer := 0;
end;

// Attach1DTexture
//
procedure TGLFramebufferHandle.Attach1DTexture(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint);
begin
   glFramebufferTexture1D(target, attachment, textarget, texture, level);
end;

// Attach2DTexture
//
procedure TGLFramebufferHandle.Attach2DTexture(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint);
begin
   glFramebufferTexture2D(target, attachment, textarget, texture, level);
end;

// Attach3DTexture
//
procedure TGLFramebufferHandle.Attach3DTexture(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint; layer: TGLint);
begin
   glFramebufferTexture3D(target, attachment, textarget, texture, level, layer);
end;

// AttachLayer
//
procedure TGLFramebufferHandle.AttachLayer(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint; layer: TGLint);
begin
   glFramebufferTextureLayer(target, attachment, texture, level, layer);
end;

// AttachRenderBuffer
//
procedure TGLFramebufferHandle.AttachRenderBuffer(target: TGLenum; attachment: TGLenum; renderbuffertarget: TGLenum; renderbuffer: TGLuint);
begin
   glFramebufferRenderbuffer(target, attachment, renderbuffertarget, renderbuffer);
end;

// AttachTexture
//
procedure TGLFramebufferHandle.AttachTexture(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint);
begin
   glFramebufferTexture(target, attachment, texture, level);
end;

// AttachTextureLayer
//
procedure TGLFramebufferHandle.AttachTextureLayer(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint; layer: TGLint);
begin
   glFramebufferTextureLayer(target, attachment, texture, level, layer);
end;

// Blit
//
procedure TGLFramebufferHandle.Blit(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint;
			                              dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint;
			                              mask: TGLbitfield; filter: TGLenum);
begin
   glBlitFramebuffer(srcX0, srcY0, srcX1, srcY1, dstX0, dstY0, dstX1, dstY1, mask, filter);
end;

// GetAttachmentParameter
//
function TGLFramebufferHandle.GetAttachmentParameter(target: TGLenum; attachment: TGLenum; pname: TGLenum): TGLint;
begin
   glGetFramebufferAttachmentParameteriv(target, attachment, pname, @Result)
end;

// GetAttachmentObjectType
//
function TGLFramebufferHandle.GetAttachmentObjectType(target: TGLenum; attachment: TGLenum): TGLint;
begin
   glGetFramebufferAttachmentParameteriv(target, attachment, GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE, @Result);
end;

// GetAttachmentObjectName
//
function TGLFramebufferHandle.GetAttachmentObjectName(target: TGLenum; attachment: TGLenum): TGLint;
begin
   glGetFramebufferAttachmentParameteriv(target, attachment, GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME, @Result);
end;

// CheckStatus
//
function TGLFramebufferHandle.CheckStatus(target: TGLenum): TGLenum;
begin
   Result := glCheckFramebufferStatus(target);
end;

// IsSupported
//
class function TGLFramebufferHandle.IsSupported : Boolean;
begin
   Result := {GL_EXT_framebuffer_object or} GL_ARB_framebuffer_object;
end;

// Transferable
//
class function TGLFramebufferHandle.Transferable: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TGLRenderbufferObject ------------------
// ------------------

// DoAllocateHandle
//
function TGLRenderbufferHandle.DoAllocateHandle: Cardinal;
begin
   glGenRenderbuffers(1, @Result);
end;

// DoDestroyHandle
//
procedure TGLRenderbufferHandle.DoDestroyHandle;
begin
   if not vContextActivationFailureOccurred then begin
      // reset error status
      glGetError;
      // delete
 	    glDeleteRenderbuffers(1, @FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// Bind
//
procedure TGLRenderbufferHandle.Bind;
begin
//   glBindRenderbuffer(GL_RENDERBUFFER, FHandle);
   Assert(vCurrentGLContext<>nil);
   vCurrentGLContext.GLStates.RenderBuffer := FHandle;
end;

// UnBind
//
procedure TGLRenderbufferHandle.UnBind;
begin
//   glBindRenderbuffer(GL_RENDERBUFFER, 0);
   Assert(vCurrentGLContext<>nil);
   vCurrentGLContext.GLStates.RenderBuffer := 0;
end;

// SetStorage
//
procedure TGLRenderbufferHandle.SetStorage(internalformat: TGLenum; width, height: TGLsizei);
begin
   glRenderbufferStorage(GL_RENDERBUFFER, internalformat, width, height);
end;

// SetStorageMultisample
//
procedure TGLRenderbufferHandle.SetStorageMultisample(internalformat: TGLenum; samples: TGLsizei; width, height: TGLsizei);
begin
   glRenderbufferStorageMultisample(GL_RENDERBUFFER, samples, internalformat, width, height);
end;

// IsSupported
//
class function TGLRenderbufferHandle.IsSupported : Boolean;
begin
   Result := {GL_EXT_framebuffer_object or} GL_ARB_framebuffer_object;
end;

// ------------------
// ------------------ TGLSLHandle ------------------
// ------------------

// DoDestroyHandle
//
procedure TGLSLHandle.DoDestroyHandle;
begin
   if not vContextActivationFailureOccurred then begin
      // reset error status
      ClearGLError;
      // delete
      glDeleteObjectARB(FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// InfoLog
//
function TGLSLHandle.InfoLog : String;
var
   maxLength : Integer;
   log: TGLString;
begin
   maxLength:=0;
   glGetObjectParameterivARB(FHandle, GL_OBJECT_INFO_LOG_LENGTH_ARB, @maxLength);
   SetLength(log, maxLength);
   if maxLength>0 then begin
      glGetInfoLogARB(FHandle, maxLength, @maxLength, @log[1]);
      SetLength(log, maxLength);
   end;
   Result:=String(log);
end;

// IsSupported
//
class function TGLSLHandle.IsSupported: Boolean;
begin
  Result := GL_ARB_shader_objects;
end;

// ------------------
// ------------------ TGLShaderHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLShaderHandle.DoAllocateHandle : Cardinal;
begin
   Result:=glCreateShaderObjectARB(FShaderType);
end;

// ShaderSource
//
procedure TGLShaderHandle.ShaderSource(const source : String);
var
   p : PGLChar;
begin
   p:=PGLChar(TGLString(source));
   glShaderSourceARB(FHandle, 1, @p, nil);
end;

// CompileShader
//
function TGLShaderHandle.CompileShader : Boolean;
var
   compiled : Integer;
begin
   glCompileShaderARB(FHandle);
   compiled:=0;
   glGetObjectParameterivARB(FHandle, GL_OBJECT_COMPILE_STATUS_ARB, @compiled);
   Result:=(compiled<>0);
end;

// ------------------
// ------------------ TGLVertexShaderHandle ------------------
// ------------------

// Create
//
constructor TGLVertexShaderHandle.Create;
begin
   FShaderType:=GL_VERTEX_SHADER_ARB;
   inherited;
end;

// IsSupported
//
class function TGLVertexShaderHandle.IsSupported: Boolean;
begin
   Result := GL_ARB_vertex_shader;
end;

// ------------------
// ------------------ TGLGeometryShaderHandle ------------------
// ------------------

// Create
//
constructor TGLGeometryShaderHandle.Create;
begin
   FShaderType:=GL_GEOMETRY_SHADER_EXT;
   inherited;
end;

// IsSupported
//
class function TGLGeometryShaderHandle.IsSupported: Boolean;
begin
   Result := GL_EXT_geometry_shader4;
end;

// ------------------
// ------------------ TGLFragmentShaderHandle ------------------
// ------------------

// Create
//
constructor TGLFragmentShaderHandle.Create;
begin
   FShaderType:=GL_FRAGMENT_SHADER_ARB;
   inherited;
end;

// IsSupported
//
class function TGLFragmentShaderHandle.IsSupported: Boolean;
begin
   Result := GL_ARB_fragment_shader;
end;

// ------------------
// ------------------ TGLProgramHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLProgramHandle.DoAllocateHandle : cardinal;
begin
   Result:=glCreateProgramObjectARB();
end;

// AddShader
//
procedure TGLProgramHandle.AddShader(shaderType : TGLShaderHandleClass; const shaderSource : String;
                                     treatWarningsAsErrors : Boolean = False);
var
   shader : TGLShaderHandle;
begin
   shader:=shaderType.CreateAndAllocate;
   try
      if shader.Handle=0 then
         raise EGLShader.Create('Couldn''t allocate '+shaderType.ClassName);
      shader.ShaderSource(shaderSource);
      if (not shader.CompileShader)
         or (treatWarningsAsErrors and (Pos('warning', LowerCase(shader.InfoLog))>0)) then
         raise EGLShader.Create(FName + ' (' + shader.ClassName+'): '#13#10 + shader.InfoLog);
      AttachObject(shader);
   finally
      shader.Free;
   end;
   CheckOpenGLError;
end;

// AttachObject
//
procedure TGLProgramHandle.AttachObject(shader : TGLShaderHandle);
begin
   glAttachObjectARB(FHandle, shader.Handle);
end;

// BindAttribLocation
//
procedure TGLProgramHandle.BindAttribLocation(index : Integer; const aName : String);
begin
   glBindAttribLocationARB(FHandle, index, PGLChar(TGLString(aName)));
end;

// BindFragDataLocation
//
procedure TGLProgramHandle.BindFragDataLocation(index: Integer; const aName : String);
begin
  glBindFragDataLocation(FHandle, index, PGLChar(TGLString(name)));
end;

// LinkProgram
//
function TGLProgramHandle.LinkProgram : Boolean;
var
   linked : Integer;
begin
   glLinkProgramARB(FHandle);
   linked:=0;
   glGetObjectParameterivARB(FHandle, GL_OBJECT_LINK_STATUS_ARB, @linked);
   Result:=(linked<>0);
end;

// ValidateProgram
//
function TGLProgramHandle.ValidateProgram : Boolean;
var
   validated : Integer;
begin
   glValidateProgramARB(FHandle);
   validated:=0;
   glGetObjectParameterivARB(FHandle, GL_OBJECT_VALIDATE_STATUS_ARB, @validated);
   Result:=(validated<>0);
end;

// GetAttribLocation
//
function TGLProgramHandle.GetAttribLocation(const aName : String) : Integer;
begin
   Result:=glGetAttribLocationARB(Handle, PGLChar(TGLString(aName)));
   Assert(Result>=0, 'Unknown attrib "'+name+'" or program not in use');
end;

// GetUniformLocation
//
function TGLProgramHandle.GetUniformLocation(const aName : String) : Integer;
begin
   Result:=glGetUniformLocationARB(Handle, PGLChar(TGLString(aName)));
   Assert(Result>=0, 'Unknown uniform "'+name+'" or program not in use');
end;

// GetVaryingLocation
//
function TGLProgramHandle.GetVaryingLocation(const aName : String) : Integer;
begin
   Result:=glGetVaryingLocationNV( Handle, PGLChar(TGLString(aName)));
   Assert(Result>=0, 'Unknown varying "'+name+'" or program not in use');
end;

// AddActiveVarying
//
procedure TGLProgramHandle.AddActiveVarying(const aName : String);
begin
  glActiveVaryingNV( Handle, PGLChar(TGLString(aName)));
end;

// GetAttribLocation
//
procedure TGLProgramHandle.UseProgramObject;
begin
   Assert(vCurrentGLContext<>nil);
   vCurrentGLContext.GLStates.CurrentProgram := Handle;
end;

// GetAttribLocation
//
procedure TGLProgramHandle.EndUseProgramObject;
begin
   Assert(vCurrentGLContext<>nil);
   vCurrentGLContext.GLStates.CurrentProgram := 0;
end;

// GetUniform1i
//
function TGLProgramHandle.GetUniform1i(const index : String) : Integer;
begin
   glGetUniformivARB(FHandle, GetUniformLocation(index), @Result);
end;

// GetUniform2i
//
function TGLProgramHandle.GetUniform2i(const index: String): TVector2i;
begin
   glGetUniformivARB(FHandle, GetUniformLocation(index), @Result);
end;

// GetUniform3i
//
function TGLProgramHandle.GetUniform3i(const index: String): TVector3i;
begin
   glGetUniformivARB(FHandle, GetUniformLocation(index), @Result);
end;

// GetUniform4i
//
function TGLProgramHandle.GetUniform4i(const index: String): TVector4i;
begin
   glGetUniformivARB(FHandle, GetUniformLocation(index), @Result);
end;

// SetUniform1f
//
procedure TGLProgramHandle.SetUniform1f(const index : String; val : Single);
begin
   glUniform1fARB(GetUniformLocation(index), val);
end;

// GetUniform1f
//
function TGLProgramHandle.GetUniform1f(const index : String) : Single;
begin
   glGetUniformfvARB(FHandle, GetUniformLocation(index), @Result);
end;

// SetUniform1i
//
procedure TGLProgramHandle.SetUniform1i(const index : String; val : Integer);
begin
   glUniform1iARB(GetUniformLocation(index), val);
end;

// SetUniform2i
//
procedure TGLProgramHandle.SetUniform2i(const index: String;
  const Value: TVector2i);
begin
   glUniform2iARB(GetUniformLocation(index), Value[0], Value[1]);
end;

// SetUniform3i
//
procedure TGLProgramHandle.SetUniform3i(const index: String;
  const Value: TVector3i);
begin
   glUniform3iARB(GetUniformLocation(index), Value[0], Value[1], Value[2]);
end;

// SetUniform4i
//
procedure TGLProgramHandle.SetUniform4i(const index: String;
  const Value: TVector4i);
begin
   glUniform4iARB(GetUniformLocation(index), Value[0], Value[1], Value[2], Value[3]);
end;

// GetUniform2f
//
function TGLProgramHandle.GetUniform2f(const index : String) : TVector2f;
begin
   glGetUniformfvARB(FHandle, GetUniformLocation(index), @Result);
end;

// SetUniform2f
//
procedure TGLProgramHandle.SetUniform2f(const index : String; const val : TVector2f);
begin
   glUniform2fARB(GetUniformLocation(index), val[0], val[1]);
end;

// GetUniform3f
//
function TGLProgramHandle.GetUniform3f(const index : String) : TAffineVector;
begin
   glGetUniformfvARB(FHandle, GetUniformLocation(index), @Result);
end;

// SetUniform3f
//
procedure TGLProgramHandle.SetUniform3f(const index : String; const val : TAffineVector);
begin
   glUniform3fARB(GetUniformLocation(index), val[0], val[1], val[2]);
end;

// GetUniform4f
//
function TGLProgramHandle.GetUniform4f(const index : String) : TVector;
begin
   glGetUniformfvARB(FHandle, GetUniformLocation(index), @Result);
end;

// SetUniform4f
//
procedure TGLProgramHandle.SetUniform4f(const index : String; const val : TVector);
begin
   glUniform4fARB(GetUniformLocation(index), val[0], val[1], val[2], val[3]);
end;


// GetUniformMatrix2fv
//
function TGLProgramHandle.GetUniformMatrix2fv(const index : String) : TMatrix2f;
begin
   glGetUniformfvARB(FHandle, GetUniformLocation(index), @Result);
end;

// SetUniformMatrix2fv
//
procedure TGLProgramHandle.SetUniformMatrix2fv(const index : String; const val : TMatrix2f);
begin
   glUniformMatrix2fvARB(GetUniformLocation(index), 1, False, @val);
end;

// GetUniformMatrix3fv
//
function TGLProgramHandle.GetUniformMatrix3fv(const index : String) : TMatrix3f;
begin
   glGetUniformfvARB(FHandle, GetUniformLocation(index), @Result);
end;

// SetUniformMatrix3fv
//
procedure TGLProgramHandle.SetUniformMatrix3fv(const index : String; const val : TMatrix3f);
begin
   glUniformMatrix3fvARB(GetUniformLocation(index), 1, False, @val);
end;

// GetUniformMatrix4fv
//
function TGLProgramHandle.GetUniformMatrix4fv(const index : String) : TMatrix;
begin
   glGetUniformfvARB(FHandle, GetUniformLocation(index), @Result);
end;

// SetUniformMatrix4fv
//
procedure TGLProgramHandle.SetUniformMatrix4fv(const index : String; const val : TMatrix);
begin
   glUniformMatrix4fvARB(GetUniformLocation(index), 1, False, @val);
end;


// SetUniformf
//
procedure TGLProgramHandle.SetUniformf(const index: String;
  const val: single);
begin
  SetUniform1f(index, val);
end;

// SetUniformf
//
procedure TGLProgramHandle.SetUniformf(const index: String; const val: TVector2f);
begin
  SetUniform2f(index, val);
end;

// SetUniformf
//
procedure TGLProgramHandle.SetUniformf(const index: String;
  const val: TVector3f);
begin
  SetUniform3f(index, val);
end;

// SetUniformf
//
procedure TGLProgramHandle.SetUniformf(const index: String;
  const val: TVector4f);
begin
  SetUniform4f(index, val);
end;

// SetUniformf
//
procedure TGLProgramHandle.SetUniformi(const index: String;
  const val: integer);
begin
  SetUniform1f(index, val);
end;

// SetUniformf
//
procedure TGLProgramHandle.SetUniformi(const index: String; const val: TVector2i);
begin
  SetUniform2i(index, val);
end;

// SetUniformf
//
procedure TGLProgramHandle.SetUniformi(const index: String;
  const val: TVector3i);
begin
  SetUniform3i(index, val);
end;

// SetUniformf
//
procedure TGLProgramHandle.SetUniformi(const index: String;
  const val: TVector4i);
begin
  SetUniform4i(index, val);
end;

// GetUniformTextureHandle
//
function TGLProgramHandle.GetUniformTextureHandle(const index: string;
  const TextureIndex: Integer; const TextureTarget: Cardinal): Cardinal;
begin
  Result := GetUniform1i(index);
end;

// SetUniformTextureHandle
//
procedure TGLProgramHandle.SetUniformTextureHandle(const index: string;
  const TextureIndex: Integer; const TextureTarget, Value: Cardinal);
begin
  glActiveTextureARB(GL_TEXTURE0_ARB + TextureIndex);
  RenderingContext.GLStates.SetGLCurrentTexture(0, TextureTarget, Value);
  SetUniform1i(index, TextureIndex);
end;

// SetUniformBuffer
//
procedure TGLProgramHandle.SetUniformBuffer(const index: string;
  Value: TGLUniformBufferHandle);
begin
  glUniformBufferEXT(Handle, GetUniformLocation(index), Value.Handle);
end;

// GetUniformBufferSize
//
function TGLProgramHandle.GetUniformBufferSize(const aName : String) : Integer;
begin
  Result := glGetUniformBufferSizeEXT(Handle, GetUniformLocation(aName));
end;

// GetUniformOffset
//
function TGLProgramHandle.GetUniformOffset(const aName : String) : PGLInt;
begin
  Result := glGetUniformOffsetEXT(Handle, GetUniformLocation(aName));
end;

// GetUniformBlockIndex
//
function TGLProgramHandle.GetUniformBlockIndex(const aName : String) : Integer;
begin
  Result := glGetUniformBlockIndex(Handle, PGLChar(TGLString(aName)));
  Assert(Result>=0, 'Unknown uniform block"'+name+'" or program not in use');
end;

// Create
//
constructor TGLProgramHandle.Create;
begin
  inherited Create;
  FName := 'DefaultShaderName';
end;

// ------------------
// ------------------ TGLContextManager ------------------
// ------------------

// Create
//
constructor TGLContextManager.Create;
begin
   inherited Create;
   FList:=TThreadList.Create;
end;

// Destroy
//
destructor TGLContextManager.Destroy;
begin
   FList.Free;
   inherited Destroy;
end;

// CreateContext
//
function TGLContextManager.CreateContext : TGLContext;
begin
   if Assigned(vContextClasses) and (vContextClasses.Count>0) then begin
      Result:=TGLContextClass(vContextClasses[0]).Create;
      Result.FManager:=Self;
   end else Result:=nil;
end;

// Lock
//
procedure TGLContextManager.Lock;
begin
   FList.LockList;
end;

// UnLock
//
procedure TGLContextManager.UnLock;
begin
   FList.UnlockList;
end;

// ContextCount
//
function TGLContextManager.ContextCount : Integer;
begin
   // try..finally just a waste of CPU here, if Count fails, the list is amok,
   // and so is the lock...
   Result:=FList.LockList.Count;
   FList.UnLockList;
end;

// RegisterContext
//
procedure TGLContextManager.RegisterContext(aContext : TGLContext);
begin
   with FList.LockList do try
      if IndexOf(aContext)>=0 then
         raise EGLContext.Create(cInvalidContextRegistration)
      else Add(aContext);
   finally
      FList.UnlockList;
   end;
end;

// UnRegisterContext
//
procedure TGLContextManager.UnRegisterContext(aContext : TGLContext);
begin
   with FList.LockList do try
      if IndexOf(aContext)<0 then
         raise EGLContext.Create(cInvalidContextRegistration)
      else Remove(aContext);
   finally
      FList.UnlockList;
   end;
end;

// ContextCreatedBy
//
procedure TGLContextManager.ContextCreatedBy(aContext : TGLContext);
begin
   Lock;
   try
      Inc(FCreatedRCCount);
   finally
      UnLock;
   end;
end;

// DestroyingContextBy
//
procedure TGLContextManager.DestroyingContextBy(aContext : TGLContext);
var
   cn : TGLContextNotification;
begin
   Lock;
   try
      Dec(FCreatedRCCount);
      if FCreatedRCCount=0 then begin
         // yes, slow and bulky, but allows for the triggered event to
         // cascade-remove notifications safely
         while Length(FNotifications)>0 do begin
            cn:=FNotifications[High(FNotifications)];
            SetLength(FNotifications, Length(FNotifications)-1);
            cn.event(cn.obj);
         end;
      end;
   finally
      UnLock;
   end;
end;

// LastContextDestroyNotification
//
procedure TGLContextManager.LastContextDestroyNotification(
                                    anObject : TObject; anEvent : TNotifyEvent);
begin
   Lock;
   try
      SetLength(FNotifications, Length(FNotifications)+1);
      with FNotifications[High(FNotifications)] do begin
         obj:=anObject;
         event:=anEvent;
      end;
   finally
      UnLock;
   end;
end;

// RemoveNotification
//
procedure TGLContextManager.RemoveNotification(anObject : TObject);
var
   i : Integer;
   found : Boolean;
begin
   Lock;
   try
      found:=False;
      i:=Low(FNotifications);
      while i<=High(FNotifications) do begin
         if FNotifications[i].obj=anObject then begin
            found:=True;
            while i<=High(FNotifications) do begin
               FNotifications[i]:=FNotifications[i+1];
               Inc(i);
            end;
            SetLength(FNotifications, Length(FNotifications)-1);
            Break;
         end;
         Inc(i);
      end;
      if not found then
         raise EGLContext.Create(cInvalidNotificationRemoval);
   finally
      UnLock;
   end;
end;

// Terminate
//
procedure TGLContextManager.Terminate;
begin
   FTerminated:=True;
   if ContextCount=0 then begin
      GLContextManager:=nil;
      Free;
   end;
end;

// DestroyAllHandles
//
procedure TGLContextManager.DestroyAllHandles;
var
   i : Integer;
begin
   with FList.LockList do try
      for i:=Count-1 downto 0 do
         TGLContext(Items[i]).DestroyAllHandles;
   finally
      FList.UnLockList;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   GLContextManager:=TGLContextManager.Create;

finalization

   GLContextManager.Terminate;
   vContextClasses.Free;
   vContextClasses:=nil;

end.
