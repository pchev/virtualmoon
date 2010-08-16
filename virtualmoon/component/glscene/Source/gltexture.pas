//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLTexture<p>

 Handles all the color and texture stuff.<p>

 <b>History : </b><font size=-1><ul>
       <li>05/03/10 - DanB - Removed disabling Texture Rect/CubeMap/3D, since disabling will
                             cause errors on hardware that doesn't support them
       <li>05/03/10 - DanB - More state added to TGLStateCache
       <li>23/01/10 - Yar  - Added TextureFormatEx to TGLTexture
                             and tfExtended to TGLTextureFormat (thanks mif for idea)  
       <li>22/01/10 - Yar  - Added GLTextureFormat to uses,
                             1D, 3D, array, cube map array target support,
                             texture error indication,
                             TGLTextureImage ResorceName property,
                             Depth property,
                             NativeTextureTarget becomes property
      <li>07/01/10 - DaStr - Added tmAdd TextureMode and enhanced documentation
                             (thanks DungeonLords)
                             Removed IncludeTrailingBackslash function
      <li>10/11/09 - DaStr - Added more texture formats (thanks YarUnderoaker)
      <li>04/06/09 - DanB - Delphi 5 fix
      <li>17/10/08 - DanB - changed some NotifyChange(Sender) calls to NotifyChange(Self)
      <li>08/10/08 - DanB - split materials related stuff into GLMaterial.pas
      <li>06/10/08 - DanB - added Assert check for trying to create texture images
      <li>05/10/08 - DanB - separated texture image editor from texture unit
                            moved color related stuff to GLColor.pas
                            moved TRenderContextInfo into separate unit
      <li>12/04/08 - DaStr - Bugfixed TGLTextureExItem.Create()
                              (thanks dAlex) (BugTracker ID = 1940451)
      <li>10/04/08 - DaStr - Added a Delpi 5 interface bug work-around to
                              TGLMaterial (BugTracker ID = 1938988)
      <li>08/02/08 - Mrqzzz - Added tiaBottomRightPointColorTransparent
      <li>29/07/07 - LC - Modified how tmmEyeLinear is applied, see
                          Bugtracker ID = 1762966.
      <li>06/06/07 - DaStr - Moved all color types, constants and functions
                              to GLColor.pas (Bugtracker ID = 1732211)
      <li>31/03/07 - DaStr - Bugfixed TGLTexture.Assign (missed some properties)
                              (Bugtracker ID = 1692012) (thanks Zapology)
      <li>28/03/07 - DaStr - Added explicit pointer dereferencing
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
      <li>28/03/07 - DaStr - Renamed parameters in some methods
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678658)
      <li>23/03/07 - DaStr - Added missing parameters in procedure's implementation
                              (thanks Burkhard Carstens) (Bugtracker ID = 1681409)
      <li>06/03/07 - DaStr - Removed obsolete FPC IFDEF's
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678642)
      <li>14/03/07 - DaStr - TGLPicFileImage now provides correct Width and Height
                                                        (BugtrackerID = 1680742)
      <li>09/03/07 - DaStr - Added TGLMaterial.GetActualPrimaryMaterial, GetLibMaterial
                             Bugfixed TGLColor.Initialize and TGLColor.Destroy
                              (thanks Burkhard Carstens) (BugtrackerID = 1678650)
      <li>04/03/07 - DaStr - Added TGLTextureTarget, [Encode/Decode]GLTextureTarget
      <li>23/02/07 - DaStr - Added TGLShaderClass, TGLShaderFailedInitAction,
                              EGLShaderException
                             Added TGLShader.HandleFailedInitialization, ShaderSupported,
                              GetStardardNotSupportedMessage, FailedInitAction
                             Added default value for TGLShader.ShaderStyle
                             Fixed TGLShader.InitializeShader
                             Fixed TGLTextureExItem.Create (TGLCoordinatesStyle stuff)
      <li>16/02/07 - DaStr - Global $Q- removed
                             Added TGLLibMaterials.GetTextureIndex, GetMaterialIndex,
                               GetNameOfTexture, GetNameOfLibMaterial
                             Added TGLMaterialLibrary.TextureByName,
                               GetNameOfTexture, GetNameOfLibMaterial
      <li>01/02/07 - LIN - Added TGLLibMaterial.IsUsed : true if texture has registered users
      <li>23/01/07 - LIN - Added TGLTextureImage.AssignToBitmap : Converts the TextureImage to a TBitmap
      <li>23/01/07 - LIN - Added TGLTextureImage.AsBitmap : Returns the TextureImage as a TBitmap
      <li>22/01/07 - DaStr - IGLMaterialLibrarySupported abstracted
                             TGLLibMaterial.TextureOffset/TextureScale.FStyle bugfxed (thanks Ian Mac)
      <li>20/12/06 - DaStr - TGLColorManager.Enumcolors overloaded
                             TGLShader.Apply bugfixed, TGLShader.Assign added
      <li>19/10/06 - LC - Fixed TGLLibMaterial.UnApply so it doesn't unapply a 2nd
                          texture that was never applied. Bugtracker ID=1234085
      <li>19/10/06 - LC - Fixed TGLLibMaterial.Assign. Bugtracker ID=1549843 (thanks Zapology)
      <li>15/09/06 - NC - TGLShader.handle as Integer -> Cardinal
      <li>12/09/06 - NC - Added GetFloatTexImage and SetFloatTexImage
      <li>06/03/05 - EG - FTextureEx now autocreated (like FTexture)
      <li>30/11/04 - EG - No longer stores TextureEx if empty
      <li>06/10/04 - NC - Corrected filtering param. setting for float texture,
                          Now keep using GL_TEXTURE_RECTANGLE_NV for TGLFloatDataImage
      <li>05/10/04 - SG - Added Material.TextureEx (texture extension)
      <li>04/10/04 - NC - Added TGLFloatDataImage
      <li>03/07/04 - LR - Move InitWinColors to GLCrossPlatform
                          Replace TGraphics, TBitmap by TGLGraphics, TGLBitmap
      <li>29/06/04 - SG - Added bmModulate blending mode
      <li>08/04/04 - EG - Added AddMaterialsFromXxxx logic
      <li>04/09/03 - EG - Added TGLShader.Enabled
      <li>02/09/03 - EG - Added TGLColor.HSVA
      <li>28/07/03 - aidave - Added TGLColor.RandomColor
      <li>24/07/03 - EG - Introduced TGLTextureImageEditor mechanism
      <li>04/07/03 - EG - Material.Texture now autocreating,
                          added per-texture brightness and gamma correction
      <li>13/06/03 - EG - cubemap images can now be saved/restored as a whole
      <li>05/06/03 - EG - Assign fixes (Andrzej Kaluza)
      <li>23/05/03 - EG - More generic libmaterial registration
      <li>08/12/02 - EG - Added tiaInverseLuminance
      <li>13/11/02 - EG - Added tmmCubeMapLight0
      <li>18/10/02 - EG - CubeMap texture matrix now setup for 2nd texture unit too
      <li>24/07/02 - EG - Added TGLLibMaterials.DeleteUnusedMaterials
      <li>13/07/02 - EG - Improved materials when lighting is off
      <li>10/07/02 - EG - Added basic protection against cyclic material refs
      <li>08/07/02 - EG - Multipass support
      <li>18/06/02 - EG - Added TGLShader
      <li>26/01/02 - EG - Makes use of new xglBegin/EndUpdate mechanism
      <li>24/01/02 - EG - Added vUseDefaultSets mechanism,
                          TGLPictureImage no longer systematically creates a TPicture
      <li>21/01/02 - EG - Fixed OnTextureNeeded calls (Leonel)
      <li>20/01/02 - EG - Fixed texture memory use report error
      <li>10/01/02 - EG - Added Material.FaceCulling, default texture filters
                          are now Linear/MipMap
      <li>07/01/02 - EG - Added renderDPI to rci
      <li>16/12/01 - EG - Added support for cube maps (texture and mappings)
      <li>30/11/01 - EG - Texture-compression related errors now ignored (unsupported formats)
      <li>14/09/01 - EG - Use of vFileStreamClass
      <li>06/09/01 - EG - No longers depends on 'Windows'
      <li>04/09/01 - EG - Texture binding cache
      <li>31/08/01 - EG - tiaDefault wasn't honoured (Rene Lindsay)
      <li>25/08/01 - EG - Added TGLBlankImage
      <li>16/08/01 - EG - drawState now part of TRenderContextInfo
      <li>15/08/01 - EG - TexGen support (object_linear, eye_linear and sphere_map)
      <li>13/08/01 - EG - Fixed OnTextureNeeded handling (paths for mat lib)
      <li>12/08/01 - EG - Completely rewritten handles management
      <li>27/07/01 - EG - TGLLibMaterials now a TOwnedCollection
      <li>19/07/01 - EG - Added "Enabled" to TGLTexture
      <li>28/06/01 - EG - Added AddTextureMaterial TGraphic variant
      <li>14/03/01 - EG - Streaming fixes by Uwe Raabe
      <li>08/03/01 - EG - TGLPicFileImage.GetBitmap32 now resets filename if not found
      <li>01/03/01 - EG - Fixed TGLMaterial.DestroyHandle,
                          Added Texture2 notifications and material cacheing
      <li>26/02/01 - EG - Added support for GL_EXT_texture_filter_anisotropic
      <li>23/02/01 - EG - Fixed texture matrix messup (second was using first)
      <li>21/02/01 - EG - Minor fix for TextureImageRequiredMemory,
                          TexGen calls now based on XOpenGL
      <li>14/02/01 - EG - Added support for texture format & texture compression
      <li>31/01/01 - EG - Added Multitexture support
      <li>28/01/01 - EG - Added MaterialOptions
      <li>15/01/01 - EG - Enhanced TGLPicFileImage.LoadFromFile
      <li>13/01/01 - EG - New helper functions for TGLMaterialLibrary
      <li>08/01/01 - EG - Not-so-clean fix for TGLTexture.Destroy... better fix
                          will require awareness of rendering contexts...
      <li>06/12/00 - EG - Added PrepareBuildList mechanism
      <li>16/10/00 - EG - Fix in TGLPictureImage.Assign
      <li>25/09/00 - EG - New texture management implemented
      <li>13/08/00 - EG - Added AddTextureMaterial
      <li>06/08/00 - EG - File not found error now happens only once per texture,
                          also added some more doc and texture transforms support
                          to TGLLibMaterial
      <li>27/07/00 - EG - TGLPictureImage.Assign now accepts TGraphic & TPicture,
                          Added max texture size clamping
      <li>15/07/00 - EG - Upgrade for new list/handle destruction scheme
      <li>05/07/00 - EG - Added tiaTopLeftPointColorTransparent
      <li>28/06/00 - EG - Added asserts for missing texture files
      <li>01/06/00 - EG - Added ReloadTexture (support for texture library),
                          Fixed persistence of material names in a texture library
      <li>28/05/00 - EG - TGLColor now has NotifyChange support for TGLBaseSceneObject
      <li>23/04/00 - EG - Fixed bugs with TGLPicFileImage & TGLPersistentImage,
                          Added tiaOpaque
      <li>17/04/00 - EG - Added Assign to DummyCube and Sprite
      <li>16/04/00 - EG - Added TGLPicFileImage.Assign
      <li>26/03/00 - EG - Finally fixed nasty bug in TGLMaterial.Free
      <li>22/03/00 - EG - Added BeginUpdate/EndUpdate to TGLPictureImage,
          Made use of [Un]SetGLState in TGLMaterial
          (gain = 7-10% on T&L intensive rendering),
                          TGLTexBaseClass is no more (RIP)
      <li>21/03/00 - EG - TGLMaterial props are now longer stored when it is
          linked to a material library entry,
          Added TGLPictureImage (split from TGLPersistentImage),
          TGLPicFileImage has been updated and reactivated,
          ColorManager is now autocreated and non longer force-linked.
      <li>19/03/00 - EG - Added SaveToXxxx & LoadFromXxxx to TGLMaterialLibrary
      <li>18/03/00 - EG - Added GetGLTextureImageClassesAsStrings,
          Added FindGLTextureImageClassByFriendlyName,
          FChanges states now ignored in TGLTexture.GetHandle,
          Added SaveToFile/LoadFromFile to TextureImage
      <li>17/03/00 - EG - Added tiaLuminance
      <li>14/03/00 - EG - Added RegisterGLTextureImageClass stuff,
          Added ImageAlpha
      <li>13/03/00 - EG - Changed TGLTextureImage image persistence again,
          Added "Edit" method for texture image classes,
          TMagFilter/TMinFilter -> TGLMagFilter/TGLMinFilter
      <li>03/03/00 - EG - Removed TImagePath,
          Started major rework of the whole TGLTextureImage stuff,
          Fixed and optimized TGLTexture.PrepareImage
      <li>12/02/00 - EG - Added Material Library
      <li>10/02/00 - EG - Fixed crash when texture is empty
      <li>08/02/00 - EG - Added AsWinColor & DeclareCurrentAsDefault to TGLColor,
          fixed notification on material property setXxx methods,
          Objects now begin with 'TGL'
      <li>07/02/00 - EG - "Update"s renamed to "NotifyChange"s
      <li>06/02/00 - EG - RoundUpToPowerOf2, RoundDownToPowerOf2 and
                          IsPowerOf2 moved to GLMisc, added TGLPersistentImage.Assign,
                          fixed TGLMaterial.Assign,
                          disable inheritance stuff in TGLFaceProperties.Apply (needs fixing),
                          Diffuse & ambient color now default to openGL values
      <li>05/02/00 - EG - Javadocisation, fixes and enhancements :<br>
                          TGLColor.Update, ConvertWinColor, TPicImage,
          TGLMaterial.Apply
   </ul></font>
}
unit GLTexture;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes, SysUtils,

  // GLScene
  OpenGL1x, VectorGeometry, GLGraphics, GLContext, GLColor,
  GLCrossPlatform, BaseClasses, GLCoordinates, GLRenderContextInfo,
  GLTextureFormat, GLState;

const
  cDefaultNormalMapScale = 0.125;

type
  TGLTextureMode = (tmDecal, tmModulate, tmBlend, tmReplace, tmAdd);
  TGLTextureWrap = (twBoth, twNone, twVertical, twHorizontal, twSeparate);
  // if texture wrap mode is twSeparate then three dimension define separately
  TGLSeparateTextureWrap =
    (
    twRepeat,
    twClamp,
    twClampToEdge,
    twClampToBorder,
    twMirrorRepeat,
    twMirrorClamp,
    twMirrorClampToEdge,
    twMirrorClampToBorder
    );

  TGLTextureTarget = (ttTexture1d, ttTexture2d, ttTexture3d,
    ttTextureRect, ttTextureCube,
    ttTexture1dArray, ttTexture2dArray, ttTextureCubeArray);

  // Specifies the texture comparison mode for currently bound depth textures.
  // That is, a texture whose internal format is tfDEPTH_COMPONENT*
  TGLTextureCompareMode = (tcmNone, tcmCompareRtoTexture);

  // Specifies how depth values should be treated
  // during filtering and texture application
  TGLDepthTextureMode = (dtmLuminance, dtmIntensity, dtmAlpha);

  // Specifies the depth comparison function.
  TGLDepthCompareFunc = TDepthFunction;

  {: Texture format for OpenGL (rendering) use.<p>
  Internally, GLScene handles all "base" images as 32 Bits RGBA, but you can
  specify a generic format to reduce OpenGL texture memory use:<ul>}
  TGLTextureFormat = (
    tfDefault,
    tfRGB,            // = tfRGB8
    tfRGBA,           // = tfRGBA8
    tfRGB16,          // = tfRGB5
    tfRGBA16,         // = tfRGBA4
    tfAlpha,          // = tfALPHA8
    tfLuminance,      // = tfLUMINANCE8
    tfLuminanceAlpha, // = tfLUMINANCE8_ALPHA8
    tfIntensity,      // = tfINTENSITY8
    tfNormalMap,      // = tfRGB8
    tfRGBAFloat16,    // = tfRGBA_FLOAT16_ATI
    tfRGBAFloat32,    // = tfRGBA_FLOAT32_ATI
    tfExtended);

  // TGLTextureCompression
  //
  TGLTextureCompression = TGLInternalCompression;

  TGLTexture = class;

  IGLTextureNotifyAble = interface(IGLNotifyAble)
    ['{0D9DC0B0-ECE4-4513-A8A1-5AE7022C9426}']
    procedure NotifyTexMapChange(Sender: TObject);
  end;

  // TTextureNeededEvent
  //
  TTextureNeededEvent = procedure(Sender: TObject; var textureFileName: string)
    of object;

  TGLTextureChange = (tcImage, tcParams, tcTarget);
  TGLTextureChanges = set of TGLTextureChange;

  {: Defines how and if Alpha channel is defined for a texture image.<ul>
   <li>tiaDefault : uses the alpha channel in the image if any
   <li>tiaAlphaFromIntensity : the alpha channel value is deduced from other
    RGB components intensity (the brighter, the more opaque)
   <li>tiaSuperBlackTransparent : pixels with a RGB color of (0, 0, 0) are
    completely transparent, others are completely opaque
   <li>tiaLuminance : the luminance value is calculated for each pixel
    and used for RGB and Alpha values
   <li>tiaLuminanceSqrt : same as tiaLuminance but with an Sqrt(Luminance)
       <li>tiaOpaque : alpha channel is uniformously set to 1.0
       <li>tiaTopLeftPointColorTransparent : points of the same color as the
          top left point of the bitmap are transparent, others are opaque.
       </ul>
    }
  TGLTextureImageAlpha = (tiaDefault, tiaAlphaFromIntensity,
    tiaSuperBlackTransparent, tiaLuminance,
    tiaLuminanceSqrt, tiaOpaque,
    tiaTopLeftPointColorTransparent,
    tiaInverseLuminance, tiaInverseLuminanceSqrt,
    tiaBottomRightPointColorTransparent);

  // TGLTextureImage
  //
  {: Base class for texture image data.<p>
   Basicly, subclasses are to be considered as different ways of getting
   a HBitmap (interfacing the actual source).<br>
   SubClasses should be registered using RegisterGLTextureImageClass to allow
   proper persistence and editability in the IDE experts. }
  TGLTextureImage = class(TGLUpdateAbleObject)
  private
    function GetResourceName: string;
  protected
    fPreviousTarget: GLenum;
    FOwnerTexture: TGLTexture;
    FOnTextureNeeded: TTextureNeededEvent;
    FResourceFile: string;
    function GetTextureTarget: GLenum; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    function GetWidth: Integer; virtual; abstract;
    function GetDepth: Integer; virtual; abstract;

    property OnTextureNeeded: TTextureNeededEvent read FOnTextureNeeded write
      FOnTextureNeeded;

  public
    { Public Properties }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    property OwnerTexture: TGLTexture read FOwnerTexture write FOwnerTexture;
    procedure NotifyChange(Sender: TObject); override;

    {: Save textureImage to file.<p>
     This may not save a picture, but for instance, parameters, if the
     textureImage is a procedural texture. }
    procedure SaveToFile(const fileName: string); dynamic; abstract;
    {: Load textureImage from a file.<p>
     This may not load a picture, but for instance, parameters, if the
     textureImage is a procedural texture.<br>
             Subclasses should invoke inherited which will take care of the
             "OnTextureNeeded" stuff. }
    procedure LoadFromFile(const fileName: string); dynamic;
    {: Returns a user-friendly denomination for the class.<p>
     This denomination is used for picking a texture image class
     in the IDE expert. }
    class function FriendlyName: string; virtual; abstract;
    {: Returns a user-friendly description for the class.<p>
     This denomination is used for helping the user when picking a
     texture image class in the IDE expert. If it's not overriden,
     takes its value from FriendlyName. }
    class function FriendlyDescription: string; virtual;

    {: Request reload/refresh of data upon next use. }
    procedure Invalidate; dynamic;

    {: Returns image's bitmap handle.<p>
             The specified target can be TEXTURE_2D or one of the cube maps targets.<br>
     If the actual image is not a windows bitmap (BMP), descendants should
     take care of properly converting to bitmap. }
    function GetBitmap32(target: TGLUInt = GL_TEXTURE_2D): TGLBitmap32; virtual;
      abstract;
    {: Request for unloading bitmapData, to free some memory.<p>
     This one is invoked when GLScene no longer needs the Bitmap data
     it got through a call to GetHBitmap.<br>
     Subclasses may ignore this call if the HBitmap was obtained at
     no particular memory cost. }
    procedure ReleaseBitmap32; virtual;
    //{: AsBitmap : Returns the TextureImage as a TBitmap }
    function AsBitmap: TGLBitmap;
    procedure AssignToBitmap(aBitmap: TGLBitmap);

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Depth: Integer read GetDepth;
    {: Native opengl texture target.<p> }
    property NativeTextureTarget: TGLUInt read GetTextureTarget;
    property ResorceName: string read GetResourceName;
  end;

  TGLTextureImageClass = class of TGLTextureImage;

  // TGLBlankImage
  //
  {: A texture image with no specified content, only a size.<p>
       This texture image type is of use if the context of your texture is
       calculated at run-time (with a TGLMemoryViewer for instance). }
  TGLBlankImage = class(TGLTextureImage)
  private
    { Private Declarations }
    procedure SetWidth(val: Integer);
    procedure SetHeight(val: Integer);
    procedure SetDepth(val: Integer);
    procedure SetCubeMap(const val: Boolean);
    procedure SetArray(const val: Boolean);
  protected
    { Protected Declarations }
    fBitmap: TGLBitmap32;

    fWidth, fHeight, fDepth: Integer;
    {: Store a icolor format, because fBitmap is not always defined}
    fColorFormat: GLenum;
    {: Blank Cube Map }
    fCubeMap: Boolean;
    {: Flag to interparate depth as layer }
    fArray: Boolean;

    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
    function GetTextureTarget: GLenum; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function GetBitmap32(target: TGLUInt = GL_TEXTURE_2D): TGLBitmap32;
      override;
    procedure ReleaseBitmap32; override;

    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;

  published
    { Published Declarations }
    {: Width, heigth and depth of the blank image (for memory allocation). }
    property Width: Integer read GetWidth write SetWidth default 256;
    property Height: Integer read GetHeight write SetHeight default 256;
    property Depth: Integer read GetDepth write SetDepth default 0;
    property CubeMap: Boolean read fCubeMap write SetCubeMap default false;
    property TextureArray: Boolean read fArray write SetArray default false;
    property ColorFormat: GLenum read fColorFormat write fColorFormat;
  end;

  // TGLPictureImage
  //
  {: Base class for image data classes internally based on a TPicture. }
  TGLPictureImage = class(TGLTextureImage)
  private
    { Private Declarations }
    FBitmap: TGLBitmap32;
    FGLPicture: TGLPicture;
    FUpdateCounter: Integer;

  protected
    { Protected Declarations }
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function GetDepth: Integer; override;
    function GetTextureTarget: GLenum; override;

    function GetPicture: TGLPicture;
    procedure SetPicture(const aPicture: TGLPicture);
    procedure PictureChanged(Sender: TObject);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    {: Use this function if you are going to modify the Picture directly.<p>
     Each invokation MUST be balanced by a call to EndUpdate. }
    procedure BeginUpdate;
    {: Ends a direct picture modification session.<p>
       Follows a BeginUpdate. }
    procedure EndUpdate;
    function GetBitmap32(target: TGLUInt = GL_TEXTURE_2D): TGLBitmap32;
      override;
    procedure ReleaseBitmap32; override;

    {: Holds the image content. }
    property Picture: TGLPicture read GetPicture write SetPicture;
  end;

  // TGLPersistentImage
  //
  {: Stores any image compatible with Delphi's TPicture mechanism.<p>
   The picture's data is actually stored into the DFM, the original
   picture name or path is not remembered. It is similar in behaviour
   to Delphi's TImage.<p>
   Note that if original image is for instance JPEG format, only the JPEG
   data will be stored in the DFM (compact) }
  TGLPersistentImage = class(TGLPictureImage)
  private

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property NativeTextureTarget;
  published
    { Published Declarations }
    property Picture;
  end;

  // TGLPicFileImage
  //
  {: Uses a picture whose data is found in a file (only filename is stored).<p>
       The image is unloaded after upload to OpenGL. }
  TGLPicFileImage = class(TGLPictureImage)
  private
    FPictureFileName: string;
    FAlreadyWarnedAboutMissingFile: Boolean;
    FWidth: Integer;
    FHeight: Integer;

  protected
    procedure SetPictureFileName(const val: string);
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    //: Only picture file name is saved
    procedure SaveToFile(const fileName: string); override;
    {: Load picture file name or use fileName as picture filename.<p>
       The autodetection is based on the filelength and presence of zeros. }
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property NativeTextureTarget;

    function GetBitmap32(target: TGLUInt = GL_TEXTURE_2D): TGLBitmap32;
      override;
    procedure Invalidate; override;

  published
    {: Filename of the picture to use. }
    property PictureFileName: string read FPictureFileName write
      SetPictureFileName;
  end;

  // TGLCubeMapTarget
  //
  TGLCubeMapTarget = (cmtPX, cmtNX, cmtPY, cmtNY, cmtPZ, cmtNZ);

  // TGLCubeMapImage
  //
  {: A texture image used for specifying and stroing a cube map.<p>
       Not unlike TGLPictureImage, but storing 6 of them instead of just one.<br>
       Saving & loading as a whole currently not supported. }
  TGLCubeMapImage = class(TGLTextureImage)
  private
    { Private Declarations }
    FBitmap: TGLBitmap32;
    FUpdateCounter: Integer;
    FPicture: array[cmtPX..cmtNZ] of TGLPicture;
  protected
    { Protected Declarations }
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
    procedure SetPicture(index: TGLCubeMapTarget; const val: TGLPicture);
    function GetPicture(index: TGLCubeMapTarget): TGLPicture;
    function GetTextureTarget: GLenum; override;

    procedure PictureChanged(Sender: TObject);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function GetBitmap32(target: TGLUInt = GL_TEXTURE_CUBE_MAP_POSITIVE_X):
      TGLBitmap32; override;
    procedure ReleaseBitmap32; override;

    {: Use this function if you are going to modify the Picture directly.<p>
     Each invokation MUST be balanced by a call to EndUpdate. }
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property NativeTextureTarget;

    {: Indexed access to the cube map's sub pictures. }
    property Picture[index: TGLCubeMapTarget]: TGLPicture read GetPicture write
    SetPicture;

  published
    { Public Declarations }
    property PicturePX: TGLPicture index cmtPX read GetPicture write SetPicture;
    property PictureNX: TGLPicture index cmtNX read GetPicture write SetPicture;
    property PicturePY: TGLPicture index cmtPY read GetPicture write SetPicture;
    property PictureNY: TGLPicture index cmtNY read GetPicture write SetPicture;
    property PicturePZ: TGLPicture index cmtPZ read GetPicture write SetPicture;
    property PictureNZ: TGLPicture index cmtNZ read GetPicture write SetPicture;
  end;

  // TGLFloatDataImage
  //
  {: A texture image of float data type.<p>
       Currently only support dynamic nvidia 32bit/16bit RGBA. }
  TGLFloatDataImage = class(TGLTextureImage)
  private
    { Private Declarations }
    FBitmap: TGLBitmap32;
    FWidth, FHeight: Integer;
  protected
    { Protected Declarations }
    procedure SetWidth(val: Integer);
    procedure SetHeight(val: Integer);
    procedure SetDepth(val: Integer);
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
    function GetTextureTarget: GLenum; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function GetBitmap32(target: TGLUInt = GL_TEXTURE_2D): TGLBitmap32;
      override;
    procedure ReleaseBitmap32; override;

    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property NativeTextureTarget;

  published
    { Published Declarations }
    {: Width of the blank image (for memory allocation). }
    property Width: Integer read GetWidth write SetWidth default 256;
    {: Width of the blank image (for memory allocation). }
    property Height: Integer read GetHeight write SetHeight default 256;
    property Depth: Integer read GetDepth write SetDepth default 0;
  end;

  // TGLTextureFilteringQuality
  //
  TGLTextureFilteringQuality = (tfIsotropic, tfAnisotropic);

  // TGLTextureMappingMode
  //
  TGLTextureMappingMode = (tmmUser, tmmObjectLinear, tmmEyeLinear, tmmSphere,
    tmmCubeMapReflection, tmmCubeMapNormal,
    tmmCubeMapLight0, tmmCubeMapCamera);

  // TGLTexture
  //
    {: Defines basic texturing properties.<p>
       You can control texture wrapping, smoothing/filtering and of course define
       the texture map (note that texturing is disabled by default).<p>
       A built-in mechanism (through ImageAlpha) allows auto-generation of an
       Alpha channel for all bitmaps (see TGLTextureImageAlpha). }
  TGLTexture = class(TGLUpdateAbleObject)
  private
    { Private Declarations }
    FTextureHandle: TGLTextureHandle;
    FTextureFormat: TGLInternalFormat;
    FTextureMode: TGLTextureMode;
    FTextureWrap: TGLTextureWrap;
    FMinFilter: TGLMinFilter;
    FMagFilter: TGLMagFilter;
    FChanges: TGLTextureChanges;
    FDisabled: Boolean;
    FImage: TGLTextureImage;
    FImageAlpha: TGLTextureImageAlpha;
    FImageBrightness: Single;
    FImageGamma: Single;
    FMappingMode: TGLTextureMappingMode;
    FMapSCoordinates: TGLCoordinates4;
    FMapTCoordinates: TGLCoordinates4;
    FMapRCoordinates: TGLCoordinates4;
    FMapQCoordinates: TGLCoordinates4;
    FOnTextureNeeded: TTextureNeededEvent;
    FCompression: TGLTextureCompression;
    FRequiredMemorySize: Integer;
    FFilteringQuality: TGLTextureFilteringQuality;
    FTexWidth: Integer;
    FTexHeight: Integer;
    FTexDepth: Integer;
    FEnvColor: TGLColor;
    FBorderColor: TGLColor;
    FNormalMapScale: Single;
    FTextureWrapS: TGLSeparateTextureWrap;
    FTextureWrapT: TGLSeparateTextureWrap;
    FTextureWrapR: TGLSeparateTextureWrap;
    fTextureCompareMode: TGLTextureCompareMode;
    fTextureCompareFunc: TGLDepthCompareFunc;
    fDepthTextureMode: TGLDepthTextureMode;

  protected
    { Protected Declarations }
    procedure SetImage(AValue: TGLTextureImage);
    procedure SetImageAlpha(const val: TGLTextureImageAlpha);
    procedure SetImageBrightness(const val: Single);
    function StoreBrightness: Boolean;
    procedure SetImageGamma(const val: Single);
    function StoreGamma: Boolean;
    procedure SetMagFilter(AValue: TGLMagFilter);
    procedure SetMinFilter(AValue: TGLMinFilter);
    procedure SetTextureMode(AValue: TGLTextureMode);
    procedure SetTextureWrap(AValue: TGLTextureWrap);
    procedure SetTextureWrapS(AValue: TGLSeparateTextureWrap);
    procedure SetTextureWrapT(AValue: TGLSeparateTextureWrap);
    procedure SetTextureWrapR(AValue: TGLSeparateTextureWrap);
    function  GetTextureFormat: TGLTextureFormat;
    procedure SetTextureFormat(const val: TGLTextureFormat);
    procedure SetTextureFormatEx(const val: TGLInternalFormat);
    function  StoreTextureFormatEx: Boolean;
    procedure SetCompression(const val: TGLTextureCompression);
    procedure SetFilteringQuality(const val: TGLTextureFilteringQuality);
    procedure SetMappingMode(const val: TGLTextureMappingMode);
    function GetMappingSCoordinates: TGLCoordinates4;
    procedure SetMappingSCoordinates(const val: TGLCoordinates4);
    function StoreMappingSCoordinates: Boolean;
    function GetMappingTCoordinates: TGLCoordinates4;
    procedure SetMappingTCoordinates(const val: TGLCoordinates4);
    function StoreMappingTCoordinates: Boolean;
    function GetMappingRCoordinates: TGLCoordinates4;
    procedure SetMappingRCoordinates(const val: TGLCoordinates4);
    function StoreMappingRCoordinates: Boolean;
    function GetMappingQCoordinates: TGLCoordinates4;
    procedure SetMappingQCoordinates(const val: TGLCoordinates4);
    function StoreMappingQCoordinates: Boolean;
    procedure SetDisabled(AValue: Boolean);
    procedure SetEnabled(const val: Boolean);
    function GetEnabled: Boolean;
    procedure SetEnvColor(const val: TGLColor);
    procedure SetBorderColor(const val: TGLColor);
    procedure SetNormalMapScale(const val: Single);
    procedure SetTextureCompareMode(const val: TGLTextureCompareMode);
    procedure SetTextureCompareFunc(const val: TGLDepthCompareFunc);
    procedure SetDepthTextureMode(const val: TGLDepthTextureMode);
    function StoreNormalMapScale: Boolean;

    function StoreImageClassName: Boolean;

    function GetHandle: TGLuint; virtual;
    //: Load texture to OpenGL subsystem
    procedure PrepareImage(target: TGLUInt); virtual;
    //: Setup OpenGL texture parameters
    procedure PrepareParams(target: TGLUInt); virtual;

    procedure DoOnTextureNeeded(Sender: TObject; var textureFileName: string);
    //: Shows a special image that indicates an error
    procedure SetTextureErrorImage;
    function GetRenderingContext: TGLContext;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    property OnTextureNeeded: TTextureNeededEvent read FOnTextureNeeded write
      FOnTextureNeeded;

    procedure PrepareBuildList;
    procedure ApplyMappingMode;
    procedure UnApplyMappingMode;
    procedure Apply(var rci: TRenderContextInfo);
    procedure UnApply(var rci: TRenderContextInfo);
    {: Applies to TEXTURE1 }
    procedure ApplyAsTexture2(var rci: TRenderContextInfo; textureMatrix: PMatrix
      = nil);
    procedure UnApplyAsTexture2(var rci: TRenderContextInfo;
      reloadIdentityTextureMatrix: boolean);
    {: N=1 for TEXTURE0, N=2 for TEXTURE1, etc. }
    procedure ApplyAsTextureN(n: Integer; var rci: TRenderContextInfo;
      textureMatrix: PMatrix = nil);
    procedure UnApplyAsTextureN(n: Integer; var rci: TRenderContextInfo;
      reloadIdentityTextureMatrix: boolean);

    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure NotifyImageChange;
    procedure NotifyParamsChange;
    procedure NotifyTargetChange;

    procedure DestroyHandles;

    procedure SetImageClassName(const val: string);
    function GetImageClassName: string;

    {: Returns the OpenGL memory used by the texture.<p>
      The compressed size is returned if, and only if texture compression
      if active and possible, and the texture has been allocated (Handle
      is defined), otherwise the estimated size (from TextureFormat
      specification) is returned. }
    function TextureImageRequiredMemory: Integer;
    {: Allocates the texture handle if not already allocated.<p>
      The texture is binded and parameters are setup, but no image data
      is initialized by this call - for expert use only. }
    function AllocateHandle: TGLuint;
    function IsHandleAllocated: Boolean;
    {: Returns OpenGL texture format corresponding to current options. }
    function OpenGLTextureFormat: Integer;
    {: Returns if of float data type}
    function IsFloatType: Boolean;
    {: Copy texture image from texture memory to main memory.<p>
      Useful for retriving texture data generated with GPU.
      RenderingContext is needed because we need an activated rendering context
      to call OpenGL functions for accessing texture data.
      }
    procedure GetFloatTexImage(RenderingContext: TGLContext; data: pointer);
    procedure SetFloatTexImage(RenderingContext: TGLContext; data: pointer);

    {: Is the texture enabled?.<p>
      Always equals to 'not Disabled'. }
    property Enabled: Boolean read GetEnabled write SetEnabled;
    {: Handle to the OpenGL texture object.<p>
      If the handle hasn't already been allocated, it will be allocated
      by this call (ie. do not use if no OpenGL context is active!) }
    property Handle: TGLuint read GetHandle;

    {: Actual width, height and depth used for last texture
      specification binding. }
    property TexWidth: Integer read FTexWidth;
    property TexHeight: Integer read FTexHeight;
    property TexDepth: Integer read FTexDepth;
    {: Give texture rendering context }
    property RenderingContext: TGLContext read GetRenderingContext;
  published
    { Published Declarations }

    {: Image ClassName for enabling True polymorphism.<p>
    This is ugly, but since the default streaming mechanism does a
    really bad job at storing	polymorphic owned-object properties,
    and neither TFiler nor TPicture allow proper use of the built-in
    streaming, that's the only way I found to allow a user-extensible
    mechanism. }
    property ImageClassName: string read GetImageClassName write
      SetImageClassName stored StoreImageClassName;
    {: Image data for the texture.<p> }
    property Image: TGLTextureImage read FImage write SetImage;

    {: Automatic Image Alpha setting.<p>
    Allows to control how and if the image's Alpha channel (transparency)
    is computed. }
    property ImageAlpha: TGLTextureImageAlpha read FImageAlpha write
      SetImageAlpha default tiaDefault;
    {: Texture brightness correction.<p>
    This correction is applied upon loading a TGLTextureImage, it's a
    simple saturating scaling applied to the RGB components of
    the 32 bits image, before it is passed to OpenGL, and before
    gamma correction (if any). }
    property ImageBrightness: Single read FImageBrightness write
      SetImageBrightness stored StoreBrightness;
    {: Texture gamma correction.<p>
    The gamma correction is applied upon loading a TGLTextureImage,
    applied to the RGB components of the 32 bits image, before it is
    passed to OpenGL, after brightness correction (if any). }
    property ImageGamma: Single read FImageGamma write SetImageGamma stored
      StoreGamma;

    {: Texture magnification filter. }
    property MagFilter: TGLMagFilter read FMagFilter write SetMagFilter default
      maLinear;
    {: Texture minification filter. }
    property MinFilter: TGLMinFilter read FMinFilter write SetMinFilter default
      miLinearMipMapLinear;
    {: Texture application mode. }
    property TextureMode: TGLTextureMode read FTextureMode write SetTextureMode
      default tmDecal;
    {: Wrapping mode for the texture. }
    property TextureWrap: TGLTextureWrap read FTextureWrap write SetTextureWrap
      default twBoth;
    {: Wrapping mode for the texture when TextureWrap=twSeparate. }
    property TextureWrapS: TGLSeparateTextureWrap read FTextureWrapS write
      SetTextureWrapS default twRepeat;
    property TextureWrapT: TGLSeparateTextureWrap read FTextureWrapT write
      SetTextureWrapT default twRepeat;
    property TextureWrapR: TGLSeparateTextureWrap read FTextureWrapR write
      SetTextureWrapR default twRepeat;

    {: Texture format for use by the renderer.<p>
    See TGLTextureFormat for details. }
    property TextureFormat: TGLTextureFormat read GetTextureFormat write
      SetTextureFormat default tfDefault;
    property TextureFormatEx: TGLInternalFormat read FTextureFormat write
      SetTextureFormatEx stored StoreTextureFormatEx;

    {: Texture compression control.<p>
    If True the compressed TextureFormat variant (the OpenGL ICD must
    support GL_ARB_texture_compression, or this option is ignored). }
    property Compression: TGLTextureCompression read FCompression write
      SetCompression default tcDefault;
    {: Specifies texture filtering quality.<p>
    You can choose between bilinear and trilinear filetring (anisotropic).<p>
    The OpenGL ICD must support GL_EXT_texture_filter_anisotropic or
    this property is ignored. }
    property FilteringQuality: TGLTextureFilteringQuality read FFilteringQuality
      write SetFilteringQuality default tfIsotropic;

    {: Texture coordinates mapping mode.<p>
    This property controls automatic texture coordinates generation. }
    property MappingMode: TGLTextureMappingMode read FMappingMode write
      SetMappingMode default tmmUser;
    {: Texture mapping coordinates mode for S, T, R and Q axis.<p>
    This property stores the coordinates for automatic texture
    coordinates generation. }
    property MappingSCoordinates: TGLCoordinates4 read GetMappingSCoordinates
      write SetMappingSCoordinates stored StoreMappingSCoordinates;
    property MappingTCoordinates: TGLCoordinates4 read GetMappingTCoordinates
      write SetMappingTCoordinates stored StoreMappingTCoordinates;
    property MappingRCoordinates: TGLCoordinates4 read GetMappingRCoordinates
      write SetMappingRCoordinates stored StoreMappingRCoordinates;
    property MappingQCoordinates: TGLCoordinates4 read GetMappingQCoordinates
      write SetMappingQCoordinates stored StoreMappingQCoordinates;

    {: Texture Environment color. }
    property EnvColor: TGLColor read FEnvColor write SetEnvColor;
    {: Texture Border color. }
    property BorderColor: TGLColor read FBorderColor write SetBorderColor;
    {: If true, the texture is disabled (not used). }
    property Disabled: Boolean read FDisabled write SetDisabled default True;

    {: Normal Map scaling.<p>
    Only applies when TextureFormat is tfNormalMap, this property defines
    the scaling that is applied during normal map generation (ie. controls
    the intensity of the bumps). }
    property NormalMapScale: Single read FNormalMapScale write SetNormalMapScale
      stored StoreNormalMapScale;

    property TextureCompareMode: TGLTextureCompareMode read fTextureCompareMode
      write SetTextureCompareMode default tcmNone;
    property TextureCompareFunc: TGLDepthCompareFunc read fTextureCompareFunc
      write SetTextureCompareFunc default cfLequal;
    property DepthTextureMode: TGLDepthTextureMode read fDepthTextureMode write
      SetDepthTextureMode default dtmLuminance;
  end;

  // TGLTextureExItem
  //
  TGLTextureExItem = class(TCollectionItem, IGLTextureNotifyAble)
  private
    { Private Decalarations }
    FTexture: TGLTexture;
    FTextureIndex: Integer;
    FTextureOffset, FTextureScale: TGLCoordinates;
    FTextureMatrixIsIdentity: Boolean;
    FTextureMatrix: TMatrix;
    FApplied: Boolean;

    //implementing IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    { Protected Decalarations }
    function GetDisplayName: string; override;
    function GetOwner: TPersistent; override;
    procedure SetTexture(const Value: TGLTexture);
    procedure SetTextureIndex(const Value: Integer);
    procedure SetTextureOffset(const Value: TGLCoordinates);
    procedure SetTextureScale(const Value: TGLCoordinates);
    procedure NotifyTexMapChange(Sender: TObject);

    procedure CalculateTextureMatrix;

    procedure OnNotifyChange(Sender: TObject);

  public
    { Public Decalarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject);

    procedure Apply(var rci: TRenderContextInfo);
    procedure UnApply(var rci: TRenderContextInfo);

  published
    { Published Decalarations }
    property Texture: TGLTexture read FTexture write SetTexture;
    property TextureIndex: Integer read FTextureIndex write SetTextureIndex;
    property TextureOffset: TGLCoordinates read FTextureOffset write
      SetTextureOffset;
    property TextureScale: TGLCoordinates read FTextureScale write
      SetTextureScale;

  end;

  // TGLTextureEx
  //
  TGLTextureEx = class(TCollection)
  private
    FOwner: TGLUpdateAbleObject;

  protected
    { Protected Decalarations }
    procedure SetItems(index: Integer; const Value: TGLTextureExItem);
    function GetItems(index: Integer): TGLTextureExItem;
    function GetOwner: TPersistent; override;
  public
    { Public Decalarations }
    constructor Create(AOwner: TGLUpdateAbleObject);

    procedure NotifyChange(Sender: TObject);
    procedure Apply(var rci: TRenderContextInfo);
    procedure UnApply(var rci: TRenderContextInfo);
    function IsTextureEnabled(Index: Integer): Boolean;

    function Add: TGLTextureExItem;

    property Items[index: Integer]: TGLTextureExItem read GetItems write
    SetItems; default;
    procedure Loaded;
  end;

  ETexture = class(Exception);
  EGLShaderException = class(Exception);

  //: Register a TGLTextureImageClass (used for persistence and IDE purposes)
procedure RegisterGLTextureImageClass(textureImageClass: TGLTextureImageClass);
//: Finds a registerer TGLTextureImageClass using its classname
function FindGLTextureImageClass(const className: string): TGLTextureImageClass;
//: Finds a registerer TGLTextureImageClass using its FriendlyName
function FindGLTextureImageClassByFriendlyName(const friendlyName: string):
  TGLTextureImageClass;
//: Defines a TStrings with the list of registered TGLTextureImageClass.
procedure SetGLTextureImageClassesToStrings(aStrings: TStrings);
{: Creates a TStrings with the list of registered TGLTextureImageClass.<p>
 To be freed by caller. }
function GetGLTextureImageClassesAsStrings: TStrings;

function DecodeGLTextureTarget(const TextureTarget: TGLTextureTarget): Cardinal;
function EncodeGLTextureTarget(const TextureTarget: Cardinal): TGLTextureTarget;

procedure RegisterTGraphicClassFileExtension(const extension: string;
  const aClass: TGraphicClass);
function CreateGraphicFromFile(const fileName: string): TGLGraphic;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// TODO: remove dependancy on GLScene.pas unit (related to tmmCubeMapLight0)

uses GLScene, GLStrings, XOpenGL, ApplicationFileIO, PictureRegisteredFormats,
  GLUtils;

const
  cTextureMode: array[tmDecal..tmAdd] of TGLEnum =
    (GL_DECAL, GL_MODULATE, GL_BLEND, GL_REPLACE, GL_ADD);

  cOldTextureFormatToInternalFormat: array[tfRGB..tfRGBAFloat32] of TGLInternalFormat = (
    tfRGB8,
    tfRGBA8,
    tfRGB5,
    tfRGBA4,
    tfALPHA8,
    tfLUMINANCE8,
    tfLUMINANCE8_ALPHA8,
    tfINTENSITY8,
    tfRGB8,
    tfRGBA_FLOAT16,
    tfRGBA_FLOAT32);

var
  vGLTextureImageClasses: TList;
  vTGraphicFileExtension: array of string;
  vTGraphicClass: array of TGraphicClass;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Helper functions'}{$ENDIF}

  // DecodeGLTextureTarget
  //

function DecodeGLTextureTarget(const TextureTarget: TGLTextureTarget): Cardinal;
begin
  case TextureTarget of
    ttTexture1d: Result := GL_TEXTURE_1D;
    ttTexture2d: Result := GL_TEXTURE_2D;
    ttTexture3d: Result := GL_TEXTURE_3D;
    ttTextureRect: Result := GL_TEXTURE_RECTANGLE;
    ttTextureCube: Result := GL_TEXTURE_CUBE_MAP;
    ttTexture1dArray: Result := GL_TEXTURE_1D_ARRAY;
    ttTexture2dArray: Result := GL_TEXTURE_2D_ARRAY;
    ttTextureCubeArray: Result := GL_TEXTURE_CUBE_MAP_ARRAY;
  else
    begin
      Result := 0;
      Assert(False, glsErrorEx + glsUnknownType);
    end;
  end;
end;

// EncodeGLTextureTarget
//

function EncodeGLTextureTarget(const TextureTarget: Cardinal): TGLTextureTarget;
begin
  case TextureTarget of
    GL_TEXTURE_1D: Result := ttTexture1d;
    GL_TEXTURE_2D: Result := ttTexture2d;
    GL_TEXTURE_3D: Result := ttTexture3d;
    GL_TEXTURE_RECTANGLE: Result := ttTextureRect;
    GL_TEXTURE_CUBE_MAP: Result := ttTextureCube;
    GL_TEXTURE_1D_ARRAY: Result := ttTexture1dArray;
    GL_TEXTURE_2D_ARRAY: Result := ttTexture2dArray;
    GL_TEXTURE_CUBE_MAP_ARRAY: Result := ttTextureCubeArray;
  else
    begin
      Result := ttTexture2d;
      Assert(False, glsErrorEx + glsUnknownType);
    end;
  end;
end;

// RegisterTGraphicClassFileExtension
//

procedure RegisterTGraphicClassFileExtension(const extension: string;
  const aClass: TGraphicClass);
var
  n: Integer;
begin
  n := Length(vTGraphicFileExtension);
  SetLength(vTGraphicFileExtension, n + 1);
  SetLength(vTGraphicClass, n + 1);
  vTGraphicFileExtension[n] := LowerCase(extension);
  vTGraphicClass[n] := aClass;
end;

// CreateGraphicFromFile
//

function CreateGraphicFromFile(const fileName: string): TGLGraphic;
var
  i: Integer;
  ext: string;
  fs: TStream;
  graphicClass: TGraphicClass;
begin
  Result := nil;
  if FileStreamExists(fileName) then
  begin
    graphicClass := nil;
    ext := LowerCase(ExtractFileExt(fileName));
    for i := 0 to High(vTGraphicFileExtension) do
    begin
      if vTGraphicFileExtension[i] = ext then
      begin
        graphicClass := TGraphicClass(vTGraphicClass[i]);
        Break;
      end;
    end;
    if graphicClass = nil then
      graphicClass := GraphicClassForExtension(ext);
    if graphicClass <> nil then
    begin
      Result := graphicClass.Create;
      try
        fs := CreateFileStream(fileName, fmOpenRead);
        try
          Result.LoadFromStream(fs);
        finally
          fs.Free;
        end;
      except
        FreeAndNil(Result);
        raise;
      end;
    end;
  end;
end;

// RegisterGLTextureImageClass
//

procedure RegisterGLTextureImageClass(textureImageClass: TGLTextureImageClass);
begin
  if not Assigned(vGLTextureImageClasses) then
    vGLTextureImageClasses := TList.Create;
  vGLTextureImageClasses.Add(textureImageClass);
end;

// FindGLTextureImageClass
//

function FindGLTextureImageClass(const className: string): TGLTextureImageClass;
var
  i: Integer;
  tic: TGLTextureImageClass;
begin
  Result := nil;
  if Assigned(vGLTextureImageClasses) then
    for i := 0 to vGLTextureImageClasses.Count - 1 do
    begin
      tic := TGLTextureImageClass(vGLTextureImageClasses[i]);
      if tic.ClassName = className then
      begin
        Result := tic;
        Break;
      end;
    end;

end;

// FindGLTextureImageClassByFriendlyName
//

function FindGLTextureImageClassByFriendlyName(const friendlyName: string):
  TGLTextureImageClass;
var
  i: Integer;
  tic: TGLTextureImageClass;
begin
  Result := nil;
  if Assigned(vGLTextureImageClasses) then
    for i := 0 to vGLTextureImageClasses.Count - 1 do
    begin
      tic := TGLTextureImageClass(vGLTextureImageClasses[i]);
      if tic.FriendlyName = friendlyName then
      begin
        Result := tic;
        Break;
      end;
    end;
end;

// SetGLTextureImageClassesToStrings
//

procedure SetGLTextureImageClassesToStrings(aStrings: TStrings);
var
  i: Integer;
  tic: TGLTextureImageClass;
begin
  with aStrings do
  begin
    BeginUpdate;
    Clear;
    if Assigned(vGLTextureImageClasses) then
      for i := 0 to vGLTextureImageClasses.Count - 1 do
      begin
        tic := TGLTextureImageClass(vGLTextureImageClasses[i]);
        AddObject(tic.FriendlyName, TObject(Pointer(tic)));
      end;
    EndUpdate;
  end;
end;

// GetGLTextureImageClassesAsStrings
//

function GetGLTextureImageClassesAsStrings: TStrings;
begin
  Result := TStringList.Create;
  SetGLTextureImageClassesToStrings(Result);
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TGLTextureImage ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLTextureImage'}{$ENDIF}

// Create
//

constructor TGLTextureImage.Create(AOwner: TPersistent);
begin
  inherited;
  FOwnerTexture := (AOwner as TGLTexture);
  fPreviousTarget := GL_TEXTURE_2D;
end;

// Destroy
//

destructor TGLTextureImage.Destroy;
begin
  inherited Destroy;
end;

// FriendlyDescription
//

class function TGLTextureImage.FriendlyDescription: string;
begin
  Result := FriendlyName;
end;

// Invalidate
//

procedure TGLTextureImage.Invalidate;
begin
  ReleaseBitmap32;
  NotifyChange(Self);
end;

// ReleaseBitmap32
//

procedure TGLTextureImage.ReleaseBitmap32;
begin
  // nothing here.
end;

// AsBitmap : Returns the TextureImage as a TBitmap
// WARNING: This Creates a new bitmap. Remember to free it, to prevent leaks.
// If possible, rather use AssignToBitmap.
//

function TGLTextureImage.AsBitmap: TGLBitmap;
begin
  result := self.GetBitmap32(GL_TEXTURE_2D).Create32BitsBitmap;
end;

// AssignToBitmap
//

procedure TGLTextureImage.AssignToBitmap(aBitmap: TGLBitmap);
begin
  Self.GetBitmap32(GL_TEXTURE_2D).AssignToBitmap(aBitmap);
end;

// NotifyChange
//

procedure TGLTextureImage.NotifyChange(Sender: TObject);
begin
  if Assigned(FOwnerTexture) then
  begin
    Include(FOwnerTexture.FChanges, tcImage);
    // Check for texture target change
    GetTextureTarget;
    FOwnerTexture.NotifyChange(Self);
  end;
end;

// LoadFromFile
//

procedure TGLTextureImage.LoadFromFile(const fileName: string);
var
  buf: string;
begin
  if Assigned(FOnTextureNeeded) then
  begin
    buf := fileName;
    FOnTextureNeeded(Self, buf);
  end;
end;

// GetResourceFile
//

function TGLTextureImage.GetResourceName: string;
begin
  Result := FResourceFile;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TGLBlankImage ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLBlankImage'}{$ENDIF}

// Create
//

constructor TGLBlankImage.Create(AOwner: TPersistent);
begin
  inherited;
  fWidth := 256;
  fHeight := 256;
  fDepth := 0;
  fColorFormat := GL_RGBA;
end;

// Destroy
//

destructor TGLBlankImage.Destroy;
begin
  inherited Destroy;
end;

// Assign
//

procedure TGLBlankImage.Assign(Source: TPersistent);
var
  img: TGLBlankImage;
begin
  if Assigned(Source) then
  begin
    if (Source is TGLBlankImage) then
    begin
      img := Source as TGLBlankImage;
      FWidth := img.Width;
      FHeight := img.Height;
      FDepth := img.Depth;
      FCubeMap := img.fCubeMap;
      FArray := img.fArray;
      fColorFormat := img.ColorFormat;
      FResourceFile := img.ResorceName;
      Invalidate;
    end
    else
      GetBitmap32.Assign(Source);
    NotifyChange(Self);
  end
  else
    inherited;
end;

// SetWidth
//

procedure TGLBlankImage.SetWidth(val: Integer);
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

function TGLBlankImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

// SetHeight
//

procedure TGLBlankImage.SetHeight(val: Integer);
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

function TGLBlankImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

// SetDepth
//

procedure TGLBlankImage.SetDepth(val: Integer);
begin
  if val <> FDepth then
  begin
    FDepth := val;
    if FDepth < 0 then
      FDepth := 0;
    Invalidate;
  end;
end;

// GetDepth
//

function TGLBlankImage.GetDepth: Integer;
begin
  Result := fDepth;
end;

// SetCubeMap
//

procedure TGLBlankImage.SetCubeMap(const val: Boolean);
begin
  if val <> fCubeMap then
  begin
    fCubeMap := val;
    Invalidate;
  end;
end;

// SetArray
//

procedure TGLBlankImage.SetArray(const val: Boolean);
begin
  if val <> fArray then
  begin
    fArray := val;
    Invalidate;
  end;
end;

// GetBitmap32
//

function TGLBlankImage.GetBitmap32(target: TGLUInt): TGLBitmap32;
begin
  if not Assigned(FBitmap) then
  begin
    fBitmap := TGLBitmap32.Create;
    fBitmap.Width := FWidth;
    fBitmap.Height := FHeight;
    fBitmap.Depth := FDepth;
    fBitmap.CubeMap := FCubeMap;
    fBitmap.TextureArray := FArray;
    fBitmap.ColorFormat := FColorFormat;
  end;
  Result := FBitmap;
end;

// ReleaseBitmap32
//

procedure TGLBlankImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

// SaveToFile
//

procedure TGLBlankImage.SaveToFile(const fileName: string);
begin
  SaveAnsiStringToFile(fileName, AnsiString(
    '[BlankImage]'#13#10'Width=' + IntToStr(Width) +
    #13#10'Height=' + IntToStr(Height) +
    #13#10'Depth=' + IntToStr(Depth)));
end;

// LoadFromFile
//

procedure TGLBlankImage.LoadFromFile(const fileName: string);
var
  sl: TStringList;
  buf, temp: string;
begin
  buf := fileName;
  if Assigned(FOnTextureNeeded) then
    FOnTextureNeeded(Self, buf);
  if FileExists(buf) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(buf{$IFDEF GLS_DELPHI_2009_UP}, TEncoding.ASCII{$ENDIF});
      FWidth := StrToInt(sl.Values['Width']);
      FHeight := StrToInt(sl.Values['Height']);
      temp := sl.Values['Depth'];
      if Length(temp) > 0 then
        FDepth := StrToInt(temp)
      else
        FDepth := 1;
    finally
      sl.Free;
    end;
  end
  else
  begin
    Assert(False, Format(glsFailedOpenFile, [fileName]));
  end;
end;

// FriendlyName
//

class function TGLBlankImage.FriendlyName: string;
begin
  Result := 'Blank Image';
end;

// FriendlyDescription
//

class function TGLBlankImage.FriendlyDescription: string;
begin
  Result := 'Blank Image (Width x Height x Depth)';
end;

// GetTextureTarget
//

function TGLBlankImage.GetTextureTarget: GLenum;
begin
  Result := GL_TEXTURE_2D;
  // Choose a texture target
  if Assigned(fBitmap) then
  begin
    FWidth := fBitmap.Width;
    FHeight := fBitmap.Height;
    FDepth := fBitmap.Depth;
    FCubeMap := fBitmap.CubeMap;
    FArray := fBitmap.TextureArray;
  end;

  if FHeight = 1 then
    Result := GL_TEXTURE_1D;
  if FCubeMap then
    Result := GL_TEXTURE_CUBE_MAP;
  if FDepth > 0 then
    Result := GL_TEXTURE_3D;
  if FArray then
  begin
    if FDepth < 2 then
      Result := GL_TEXTURE_1D_ARRAY
    else
      Result := GL_TEXTURE_2D_ARRAY;
    if FCubeMap then
      Result := GL_TEXTURE_CUBE_MAP_ARRAY;
  end;

  if Assigned(FOwnerTexture) then
  begin
    if ((FOwnerTexture.FTextureFormat >= tfFLOAT_R16)
      and (FOwnerTexture.FTextureFormat <= tfFLOAT_RGBA32)) then
      Result := GL_TEXTURE_RECTANGLE;
  end;

  if Result = fPreviousTarget then
    Exit;
  fPreviousTarget := Result;
  // update texture target
  if Assigned(FOwnerTexture) then
    FOwnerTexture.NotifyTargetChange;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TGLPictureImage ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLPictureImage'}{$ENDIF}

// Create
//

constructor TGLPictureImage.Create(AOwner: TPersistent);
begin
  inherited;
end;

// Destroy
//

destructor TGLPictureImage.Destroy;
begin
  ReleaseBitmap32;
  FGLPicture.Free;
  inherited Destroy;
end;

// Assign
//

procedure TGLPictureImage.Assign(Source: TPersistent);
var
  bmp: TGLBitmap;
begin
  if Assigned(Source) then
  begin
    if (Source is TGLPersistentImage) then
      Picture.Assign(TGLPersistentImage(Source).Picture)
    else if (Source is TGLGraphic) then
      Picture.Assign(Source)
    else if (Source is TGLPicture) then
      Picture.Assign(Source)
    else if (Source is TGLBitmap32) then
    begin
      bmp := TGLBitmap32(Source).Create32BitsBitmap;
      Picture.Graphic := bmp;
      bmp.Free;
      FResourceFile := TGLBitmap32(Source).ResourceName;
    end
    else
      inherited;
  end
  else
    inherited;
end;

// BeginUpdate
//

procedure TGLPictureImage.BeginUpdate;
begin
  Inc(FUpdateCounter);
  Picture.OnChange := nil;
end;

// EndUpdate
//

procedure TGLPictureImage.EndUpdate;
begin
  Assert(FUpdateCounter > 0, ClassName + ': Unbalanced Begin/EndUpdate');
  Dec(FUpdateCounter);
  Picture.OnChange := PictureChanged;
  if FUpdateCounter = 0 then
    PictureChanged(Picture);
end;

// GetHeight
//

function TGLPictureImage.GetHeight: Integer;
begin
  Result := Picture.Height;
end;

// GetWidth
//

function TGLPictureImage.GetWidth: Integer;
begin
  Result := Picture.Width;
end;

// GetDepth
//

function TGLPictureImage.GetDepth: Integer;
begin
  Result := 0;
end;

// GetBitmap32
//

function TGLPictureImage.GetBitmap32(target: TGLUInt): TGLBitmap32;
begin
  if not Assigned(FBitmap) then
  begin
    FBitmap := TGLBitmap32.Create;
    // we need to deactivate OnChange, due to a "glitch" in some TGraphics,
    // for instance, TJPegImage triggers an OnChange when it is drawn...
    if Assigned(Picture.OnChange) then
    begin
      Picture.OnChange := nil;
      try
        FBitmap.Assign(Picture.Graphic);
      finally
        Picture.OnChange := PictureChanged;
      end;
    end
    else
      FBitmap.Assign(Picture.Graphic);
  end;
  Result := FBitmap;
end;

// ReleaseBitmap32
//

procedure TGLPictureImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

// PictureChanged
//

procedure TGLPictureImage.PictureChanged(Sender: TObject);
begin
  Invalidate;
end;

// GetPicture
//

function TGLPictureImage.GetPicture: TGLPicture;
begin
  if not Assigned(FGLPicture) then
  begin
    FGLPicture := TGLPicture.Create;
    FGLPicture.OnChange := PictureChanged;
  end;
  Result := FGLPicture;
end;

// SetPicture
//

procedure TGLPictureImage.SetPicture(const aPicture: TGLPicture);
begin
  Picture.Assign(aPicture);
end;

// GetTextureTarget
//

function TGLPictureImage.GetTextureTarget: GLenum;
begin
  Result := GL_TEXTURE_2D;
  if fPreviousTarget <> Result then
  begin
    if Assigned(FOwnerTexture) then
      FOwnerTexture.NotifyTargetChange;
    fPreviousTarget := Result;
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TGLPersistentImage ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLPersistentImage'}{$ENDIF}

// Create
//

constructor TGLPersistentImage.Create(AOwner: TPersistent);
begin
  inherited;
end;

// Destroy
//

destructor TGLPersistentImage.Destroy;
begin
  inherited Destroy;
end;

// SaveToFile
//

procedure TGLPersistentImage.SaveToFile(const fileName: string);
begin
  Picture.SaveToFile(fileName);
  FResourceFile := fileName;
end;

// LoadFromFile
//

procedure TGLPersistentImage.LoadFromFile(const fileName: string);
var
  buf: string;
  gr: TGLGraphic;
begin
  buf := fileName;
  FResourceFile := fileName;
  if Assigned(FOnTextureNeeded) then
    FOnTextureNeeded(Self, buf);
  if ApplicationFileIODefined then
  begin
    gr := CreateGraphicFromFile(buf);
    if Assigned(gr) then
    begin
      Picture.Graphic := gr;
      gr.Free;
      Exit;
    end;
  end
  else if FileExists(buf) then
  begin
    Picture.LoadFromFile(buf);
    Exit;
  end;
  Picture.Graphic := nil;
  raise ETexture.CreateFmt(glsFailedOpenFile, [fileName]);
end;

// FriendlyName
//

class function TGLPersistentImage.FriendlyName: string;
begin
  Result := 'Persistent Image';
end;

// FriendlyDescription
//

class function TGLPersistentImage.FriendlyDescription: string;
begin
  Result := 'Image data is stored in its original format with other form resources,'
    + 'ie. in the DFM at design-time, and embedded in the EXE at run-time.';
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TGLPicFileImage ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLPicFileImage'}{$ENDIF}

// Create
//

constructor TGLPicFileImage.Create(AOwner: TPersistent);
begin
  inherited;
end;

// Destroy
//

destructor TGLPicFileImage.Destroy;
begin
  inherited;
end;

// Assign
//

procedure TGLPicFileImage.Assign(Source: TPersistent);
begin
  if Source is TGLPicFileImage then
  begin
    FPictureFileName := TGLPicFileImage(Source).FPictureFileName;
    FResourceFile := TGLPicFileImage(Source).ResorceName;
  end
  else
    inherited;
end;

// SetPictureFileName
//

procedure TGLPicFileImage.SetPictureFileName(const val: string);
begin
  if val <> FPictureFileName then
  begin
    FPictureFileName := val;
    FResourceFile := val;
    FAlreadyWarnedAboutMissingFile := False;
    Invalidate;
  end;
end;

// Invalidate
//

procedure TGLPicFileImage.Invalidate;
begin
  Picture.OnChange := nil;
  try
    Picture.Assign(nil);
    FBitmap := nil;
  finally
    Picture.OnChange := PictureChanged;
  end;
  inherited;
end;

// GetHeight
//

function TGLPicFileImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

// GetWidth
//

function TGLPicFileImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

// GetDepth
//

function TGLPicFileImage.GetDepth: Integer;
begin
  Result := 0;
end;

// GetBitmap32
//

function TGLPicFileImage.GetBitmap32(target: TGLUInt): TGLBitmap32;
var
  buf: string;
  gr: TGLGraphic;
begin
  if (GetWidth <= 0) and (PictureFileName <> '') then
  begin
    Picture.OnChange := nil;
    try
      buf := PictureFileName;
      if Assigned(FOnTextureNeeded) then
        FOnTextureNeeded(Self, buf);
      if FileStreamExists(buf) then
      begin
        gr := CreateGraphicFromFile(buf);
        Picture.Graphic := gr;
        gr.Free;
      end
      else
      begin
        Picture.Graphic := nil;
        if not FAlreadyWarnedAboutMissingFile then
        begin
          FAlreadyWarnedAboutMissingFile := True;
          Assert(False, Format(glsFailedOpenFile, [PictureFileName]));
        end;
      end;
      Result := inherited GetBitmap32(target);
      FWidth := Result.Width;
      FHeight := Result.Height;
      Picture.Graphic := nil;
    finally
      Picture.OnChange := PictureChanged;
    end;
  end
  else
    Result := inherited GetBitmap32(target);
end;

// SaveToFile
//

procedure TGLPicFileImage.SaveToFile(const fileName: string);
begin
  FResourceFile := fileName;
  SaveAnsiStringToFile(fileName, AnsiString(PictureFileName));
end;

// LoadFromFile
//

procedure TGLPicFileImage.LoadFromFile(const fileName: string);
var
  buf: string;
begin
  inherited;
  // attempt to autodetect if we are pointed to a file containing
  // a filename or directly to an image
  if SizeOfFile(fileName) < 512 then
  begin
    buf := string(LoadAnsiStringFromFile(fileName));
    if Pos(#0, buf) > 0 then
      PictureFileName := fileName
    else
      PictureFileName := buf;
  end
  else
    PictureFileName := fileName;
  FResourceFile := FPictureFileName;
end;

// FriendlyName
//

class function TGLPicFileImage.FriendlyName: string;
begin
  Result := 'PicFile Image';
end;

// FriendlyDescription
//

class function TGLPicFileImage.FriendlyDescription: string;
begin
  Result := 'Image data is retrieved from a file.';
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TGLCubeMapImage ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLCubeMapImage'}{$ENDIF}

// Create
//

constructor TGLCubeMapImage.Create(AOwner: TPersistent);
var
  i: TGLCubeMapTarget;
begin
  inherited;
  for i := Low(FPicture) to High(FPicture) do
  begin
    FPicture[i] := TGLPicture.Create;
    FPicture[i].OnChange := PictureChanged;
  end;
end;

// Destroy
//

destructor TGLCubeMapImage.Destroy;
var
  i: TGLCubeMapTarget;
begin
  ReleaseBitmap32;
  for i := Low(FPicture) to High(FPicture) do
    FPicture[i].Free;
  inherited Destroy;
end;

// Assign
//

procedure TGLCubeMapImage.Assign(Source: TPersistent);
var
  i: TGLCubeMapTarget;
begin
  if Assigned(Source) then
  begin
    if (Source is TGLCubeMapImage) then
    begin
      for i := Low(FPicture) to High(FPicture) do
        FPicture[i].Assign(TGLCubeMapImage(Source).FPicture[i]);
      Invalidate;
    end
    else
      inherited;
  end
  else
    inherited;
end;

// GetWidth
//

function TGLCubeMapImage.GetWidth: Integer;
begin
  Result := FPicture[cmtPX].Width;
end;

// GetHeight
//

function TGLCubeMapImage.GetHeight: Integer;
begin
  Result := FPicture[cmtPX].Height;
end;

// GetDepth
//

function TGLCubeMapImage.GetDepth: Integer;
begin
  Result := 0;
end;

// GetBitmap32
//

function TGLCubeMapImage.GetBitmap32(target: TGLUInt): TGLBitmap32;
var
  i: TGLCubeMapTarget;
begin
  i := cmtPX;
  case target of
    GL_TEXTURE_CUBE_MAP_POSITIVE_X: i := cmtPX;
    GL_TEXTURE_CUBE_MAP_NEGATIVE_X: i := cmtNX;
    GL_TEXTURE_CUBE_MAP_POSITIVE_Y: i := cmtPY;
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Y: i := cmtNY;
    GL_TEXTURE_CUBE_MAP_POSITIVE_Z: i := cmtPZ;
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Z: i := cmtNZ;
  end;
  if Assigned(FBitmap) then
    FBitmap.Free;
  FBitmap := TGLBitmap32.Create;
  FPicture[i].OnChange := nil;
  try
    FBitmap.VerticalReverseOnAssignFromBitmap := True;
    FBitmap.Assign(FPicture[i].Graphic);
  finally
    FPicture[i].OnChange := PictureChanged;
  end;
  Result := FBitmap;
end;

// ReleaseBitmap32
//

procedure TGLCubeMapImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

// BeginUpdate
//

procedure TGLCubeMapImage.BeginUpdate;
var
  i: TGLCubeMapTarget;
begin
  Inc(FUpdateCounter);
  for i := Low(FPicture) to High(FPicture) do
    FPicture[i].OnChange := nil;
end;

// EndUpdate
//

procedure TGLCubeMapImage.EndUpdate;
var
  i: TGLCubeMapTarget;
begin
  Assert(FUpdateCounter > 0, ClassName + ': Unbalanced Begin/EndUpdate');
  Dec(FUpdateCounter);
  for i := Low(FPicture) to High(FPicture) do
    FPicture[i].OnChange := PictureChanged;
  if FUpdateCounter = 0 then
    PictureChanged(FPicture[cmtPX]);
end;

// SaveToFile
//

procedure TGLCubeMapImage.SaveToFile(const fileName: string);
var
  fs: TFileStream;
  bmp: TGLBitmap;
  i: TGLCubeMapTarget;
  version: Word;
begin
  fs := TFileStream.Create(fileName, fmCreate);
  bmp := TGLBitmap.Create;
  try
    version := $0100;
    fs.Write(version, 2);
    for i := Low(FPicture) to High(FPicture) do
    begin
      bmp.Assign(FPicture[i].Graphic);
      bmp.SaveToStream(fs);
    end;
  finally
    bmp.Free;
    fs.Free;
  end;
end;

// LoadFromFile
//

procedure TGLCubeMapImage.LoadFromFile(const fileName: string);
var
  fs: TFileStream;
  bmp: TGLBitmap;
  i: TGLCubeMapTarget;
  version: Word;
begin
  fs := TFileStream.Create(fileName, fmOpenRead + fmShareDenyWrite);
  bmp := TGLBitmap.Create;
  try
    fs.Read(version, 2);
    Assert(version = $0100);
    for i := Low(FPicture) to High(FPicture) do
    begin
      bmp.LoadFromStream(fs);
      FPicture[i].Graphic := bmp;
    end;
  finally
    bmp.Free;
    fs.Free;
  end;
end;

// FriendlyName
//

class function TGLCubeMapImage.FriendlyName: string;
begin
  Result := 'CubeMap Image';
end;

// FriendlyDescription
//

class function TGLCubeMapImage.FriendlyDescription: string;
begin
  Result := 'Image data is contain 6 pictures of cubemap faces.';
end;

// PictureChanged
//

procedure TGLCubeMapImage.PictureChanged(Sender: TObject);
begin
  Invalidate;
end;

// GetTextureTarget
//

function TGLCubeMapImage.GetTextureTarget: GLenum;
begin
  Result := GL_TEXTURE_CUBE_MAP;
  if fPreviousTarget <> Result then
  begin
    if Assigned(FOwnerTexture) then
      FOwnerTexture.NotifyTargetChange;
    fPreviousTarget := Result;
  end;
end;

// SetPicture
//

procedure TGLCubeMapImage.SetPicture(index: TGLCubeMapTarget; const val:
  TGLPicture);
begin
  FPicture[index].Assign(val);
end;

// GetPicture
//

function TGLCubeMapImage.GetPicture(index: TGLCubeMapTarget): TGLPicture;
begin
  Result := FPicture[index];
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TGLTexture ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLTexture'}{$ENDIF}

// Create
//

constructor TGLTexture.Create(AOwner: TPersistent);
begin
  inherited;
  FDisabled := True;
  FChanges := [tcImage, tcParams];
  FImage := TGLPersistentImage.Create(Self);
  FImage.FOnTextureNeeded := DoOnTextureNeeded;
  FImageAlpha := tiaDefault;
  FImageBrightness := 1.0;
  FImageGamma := 1.0;
  FMagFilter := maLinear;
  FMinFilter := miLinearMipMapLinear;
  FFilteringQuality := tfIsotropic;
  FRequiredMemorySize := -1;
  FTextureHandle := TGLTextureHandle.Create;
  FMappingMode := tmmUser;
  FEnvColor := TGLColor.CreateInitialized(Self, clrTransparent);
  FBorderColor := TGLColor.CreateInitialized(Self, clrTransparent);
  FNormalMapScale := cDefaultNormalMapScale;
  FTextureCompareMode := tcmNone;
  FTextureCompareFunc := cfLequal;
  FDepthTextureMode := dtmLuminance;
  TextureFormat := tfDefault;
end;

// Destroy
//

destructor TGLTexture.Destroy;
begin
  FEnvColor.Free;
  FBorderColor.Free;
  FMapSCoordinates.Free;
  FMapTCoordinates.Free;
  FMapRCoordinates.Free;
  FMapQCoordinates.Free;
  DestroyHandles;
  FTextureHandle.Free;
  FImage.Free;
  inherited Destroy;
end;

// Assign
//

procedure TGLTexture.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if (Source is TGLTexture) then
    begin
      if Source <> Self then
      begin
        FImageAlpha := TGLTexture(Source).FImageAlpha;
        FTextureMode := TGLTexture(Source).FTextureMode;
        FTextureWrap := TGLTexture(Source).FTextureWrap;
        FTextureFormat := TGLTexture(Source).FTextureFormat;
        FCompression := TGLTexture(Source).FCompression;
        FMinFilter := TGLTexture(Source).FMinFilter;
        FMagFilter := TGLTexture(Source).FMagFilter;
        FMappingMode := TGLTexture(Source).FMappingMode;
        MappingSCoordinates.Assign(TGLTexture(Source).MappingSCoordinates);
        MappingTCoordinates.Assign(TGLTexture(Source).MappingTCoordinates);
        FDisabled := TGLTexture(Source).FDisabled;
        SetImage(TGLTexture(Source).FImage);
        FImageBrightness := TGLTexture(Source).FImageBrightness;
        FImageGamma := TGLTexture(Source).FImageGamma;
        FFilteringQuality := TGLTexture(Source).FFilteringQuality;
        FEnvColor.Assign(TGLTexture(Source).FEnvColor);
        FBorderColor.Assign(TGLTexture(Source).FBorderColor);
        FNormalMapScale := TGLTexture(Source).FNormalMapScale;
        // Probably don't need to assign these....
        // FOnTextureNeeded := TGLTexture(Source).FImageGamma;
        // FRequiredMemorySize  : Integer;
        // FTexWidth, FTexHeight : Integer;
        FChanges := [tcParams, tcImage];
      end;
    end
    else if (Source is TGLGraphic) then
      Image.Assign(Source)
    else if (Source is TGLPicture) then
      Image.Assign(TGLPicture(Source).Graphic)
    else
      inherited Assign(Source);
  end
  else
  begin
    FDisabled := True;
    SetImage(nil);
    FChanges := [tcParams, tcImage];
  end;
end;

// NotifyChange
//

procedure TGLTexture.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
  begin
    if Owner is TGLTextureExItem then
      TGLTextureExItem(Owner).NotifyChange(Self);
  end;

  inherited;
end;

// NotifyImageChange
//

procedure TGLTexture.NotifyImageChange;
begin
  Include(FChanges, tcImage);
  NotifyChange(Self);
end;

// NotifyParamsChange
//

procedure TGLTexture.NotifyParamsChange;
begin
  Include(FChanges, tcParams);
  NotifyChange(Self);
end;

// NotifyTargetChange
//

procedure TGLTexture.NotifyTargetChange;
begin
  Include(FChanges, tcTarget);
  NotifyChange(Self);
end;

// SetImage
//

procedure TGLTexture.SetImage(AValue: TGLTextureImage);
begin
  if Assigned(aValue) then
  begin
    if FImage.ClassType <> AValue.ClassType then
    begin
      FImage.Free;
      FImage := TGLTextureImageClass(AValue.ClassType).Create(Self);
      FImage.OnTextureNeeded := DoOnTextureNeeded;
    end;
    FImage.Assign(AValue);
  end
  else
  begin
    FImage.Free;
    FImage := TGLPersistentImage.Create(Self);
    FImage.FOnTextureNeeded := DoOnTextureNeeded;
  end;
end;

// SetImageClassName
//

procedure TGLTexture.SetImageClassName(const val: string);
var
  newImageClass: TGLTextureImageClass;
begin
  if val <> '' then
    if FImage.ClassName <> val then
    begin
      FImage.Free;
      newImageClass := FindGLTextureImageClass(val);
      Assert(newImageClass <> nil, 'Make sure you include the unit for ' + val +
        ' in your uses clause');
      FImage := TGLTextureImageClass(newImageClass).Create(Self);
      FImage.OnTextureNeeded := DoOnTextureNeeded;
      NotifyImageChange;
      NotifyTargetChange;
    end;
end;

// GetImageClassName
//

function TGLTexture.GetImageClassName: string;
begin
  Result := FImage.ClassName;
end;

// TextureImageRequiredMemory
//

function TGLTexture.TextureImageRequiredMemory: Integer;
var
  w, h, e, levelSize: Integer;
begin
  if FRequiredMemorySize < 0 then
  begin
    if IsCompressedFormat(fTextureFormat) then
    begin
      w := (Image.Width + 3) div 4;
      h := (Image.Height + 3) div 4;
    end
    else
    begin
      w := Image.Width;
      h := Image.Height;
    end;

    e := GetTextureElementSize(fTextureFormat);
    FRequiredMemorySize := w * h * e;
    if Image.Depth > 0 then
      FRequiredMemorySize := FRequiredMemorySize * Image.Depth;

    if not (MinFilter in [miNearest, miLinear]) then
    begin
      levelSize := FRequiredMemorySize;
      while e<levelSize do
      begin
        levelSize := levelSize div 4;
        FRequiredMemorySize := FRequiredMemorySize + levelSize;
      end;
    end;

    if Image.GetTextureTarget = GL_TEXTURE_CUBE_MAP then
      FRequiredMemorySize := FRequiredMemorySize * 6;
  end;
  Result := FRequiredMemorySize;
end;

// SetImageAlpha
//

procedure TGLTexture.SetImageAlpha(const val: TGLTextureImageAlpha);
begin
  if FImageAlpha <> val then
  begin
    FImageAlpha := val;
    NotifyImageChange;
  end;
end;

// SetImageBrightness
//

procedure TGLTexture.SetImageBrightness(const val: Single);
begin
  if FImageBrightness <> val then
  begin
    FImageBrightness := val;
    NotifyImageChange;
  end;
end;

// StoreBrightness
//

function TGLTexture.StoreBrightness: Boolean;
begin
  Result := (FImageBrightness <> 1.0);
end;

// SetImageGamma
//

procedure TGLTexture.SetImageGamma(const val: Single);
begin
  if FImageGamma <> val then
  begin
    FImageGamma := val;
    NotifyImageChange;
  end;
end;

// StoreGamma
//

function TGLTexture.StoreGamma: Boolean;
begin
  Result := (FImageGamma <> 1.0);
end;

// SetMagFilter
//

procedure TGLTexture.SetMagFilter(AValue: TGLMagFilter);
begin
  if AValue <> FMagFilter then
  begin
    FMagFilter := AValue;
    NotifyParamsChange;
  end;
end;

// SetMinFilter
//

procedure TGLTexture.SetMinFilter(AValue: TGLMinFilter);
begin
  if AValue <> FMinFilter then
  begin
    FMinFilter := AValue;
    NotifyParamsChange;
  end;
end;

// SetTextureMode
//

procedure TGLTexture.SetTextureMode(AValue: TGLTextureMode);
begin
  if AValue <> FTextureMode then
  begin
    FTextureMode := AValue;
    NotifyParamsChange;
  end;
end;

// SetDisabled
//

procedure TGLTexture.SetDisabled(AValue: Boolean);
var
  intf: IGLTextureNotifyAble;
begin
  if AValue <> FDisabled then
  begin
    FDisabled := AValue;
    if Supports(Owner, IGLTextureNotifyAble, intf) then
      intf.NotifyTexMapChange(Self)
    else
      NotifyChange(Self);
  end;
end;

// SetEnabled
//

procedure TGLTexture.SetEnabled(const val: Boolean);
begin
  Disabled := not val;
end;

// GetEnabled
//

function TGLTexture.GetEnabled: Boolean;
begin
  Result := not Disabled;
end;

// SetEnvColor
//

procedure TGLTexture.SetEnvColor(const val: TGLColor);
begin
  FEnvColor.Assign(val);
  NotifyParamsChange;
end;

// SetBorederColor
//

procedure TGLTexture.SetBorderColor(const val: TGLColor);
begin
  FBorderColor.Assign(val);
  NotifyParamsChange;
end;

// SetNormalMapScale
//

procedure TGLTexture.SetNormalMapScale(const val: Single);
begin
  if val <> FNormalMapScale then
  begin
    FNormalMapScale := val;
    if TextureFormat = tfNormalMap then
      NotifyImageChange;
  end;
end;

// StoreNormalMapScale
//

function TGLTexture.StoreNormalMapScale: Boolean;
begin
  Result := (FNormalMapScale <> cDefaultNormalMapScale);
end;

// SetTextureWrap
//

procedure TGLTexture.SetTextureWrap(AValue: TGLTextureWrap);
begin
  if AValue <> FTextureWrap then
  begin
    FTextureWrap := AValue;
    NotifyParamsChange;
  end;
end;

// SetTextureWrapS
//

procedure TGLTexture.SetTextureWrapS(AValue: TGLSeparateTextureWrap);
begin
  if AValue <> FTextureWrapS then
  begin
    FTextureWrapS := AValue;
    NotifyParamsChange;
  end;
end;

// SetTextureWrapT
//

procedure TGLTexture.SetTextureWrapT(AValue: TGLSeparateTextureWrap);
begin
  if AValue <> FTextureWrapT then
  begin
    FTextureWrapT := AValue;
    NotifyParamsChange;
  end;
end;

// SetTextureWrapR
//

procedure TGLTexture.SetTextureWrapR(AValue: TGLSeparateTextureWrap);
begin
  if AValue <> FTextureWrapR then
  begin
    FTextureWrapR := AValue;
    NotifyParamsChange;
  end;
end;

// GetTextureFormat
//

function TGLTexture.GetTextureFormat: TGLTextureFormat;
var
  i: TGLTextureFormat;
begin
  if vDefaultTextureFormat=FTextureFormat then
  begin
    Result := tfDefault;
    Exit;
  end;
  for i := tfRGB to tfRGBAFloat32 do
  begin
    if cOldTextureFormatToInternalFormat[i] = FTextureFormat then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := tfExtended;
end;

// SetTextureFormat
//

procedure TGLTexture.SetTextureFormat(const val: TGLTextureFormat);
begin
  if val=tfDefault then
  begin
    FTextureFormat := vDefaultTextureFormat;
  end
  else if val<tfExtended then
  begin
    FTextureFormat := cOldTextureFormatToInternalFormat[val];
  end;
end;

// SetTextureFormat
//

procedure TGLTexture.SetTextureFormatEx(const val: TGLInternalFormat);
begin
  if val <> FTextureFormat then
  begin
    FTextureFormat := val;
    NotifyImageChange;
  end;
end;

// StoreTextureFormatEx
//

function TGLTexture.StoreTextureFormatEx: Boolean;
begin
  Result := GetTextureFormat>=tfExtended;
end;

// SetCompression
//

procedure TGLTexture.SetCompression(const val: TGLTextureCompression);
begin
  if val <> FCompression then
  begin
    FCompression := val;
    NotifyParamsChange;
  end;
end;

// SetFilteringQuality
//

procedure TGLTexture.SetFilteringQuality(const val: TGLTextureFilteringQuality);
begin
  if val <> FFilteringQuality then
  begin
    FFilteringQuality := val;
    NotifyParamsChange;
  end;
end;

// SetMappingMode
//

procedure TGLTexture.SetMappingMode(const val: TGLTextureMappingMode);
var
  texMapChange: Boolean;
  intf: IGLTextureNotifyAble;
begin
  if val <> FMappingMode then
  begin
    texMapChange := ((val = tmmUser) and (FMappingMode <> tmmUser))
      or ((val = tmmUser) and (FMappingMode <> tmmUser));
    FMappingMode := val;
    if texMapChange then
    begin
      // when switching between texGen modes and user mode, the geometry
      // must be rebuilt in whole (to specify/remove texCoord data!)
      if Supports(Owner, IGLTextureNotifyAble, intf) then
        intf.NotifyTexMapChange(Self);
    end
    else
      NotifyChange(Self);
  end;
end;

// SetMappingSCoordinates
//

procedure TGLTexture.SetMappingSCoordinates(const val: TGLCoordinates4);
begin
  MappingSCoordinates.Assign(val);
end;

// GetMappingSCoordinates
//

function TGLTexture.GetMappingSCoordinates: TGLCoordinates4;
begin
  if not Assigned(FMapSCoordinates) then
    FMapSCoordinates := TGLCoordinates4.CreateInitialized(Self, XHmgVector,
      csVector);
  Result := FMapSCoordinates;
end;

// StoreMappingSCoordinates
//

function TGLTexture.StoreMappingSCoordinates: Boolean;
begin
  if Assigned(FMapSCoordinates) then
    Result := not VectorEquals(FMapSCoordinates.AsVector, XHmgVector)
  else Result := false;
end;

// SetMappingTCoordinates
//

procedure TGLTexture.SetMappingTCoordinates(const val: TGLCoordinates4);
begin
  MappingTCoordinates.Assign(val);
end;

// GetMappingTCoordinates
//

function TGLTexture.GetMappingTCoordinates: TGLCoordinates4;
begin
  if not Assigned(FMapTCoordinates) then
    FMapTCoordinates := TGLCoordinates4.CreateInitialized(Self, YHmgVector,
      csVector);
  Result := FMapTCoordinates;
end;

// StoreMappingTCoordinates
//

function TGLTexture.StoreMappingTCoordinates: Boolean;
begin
  if Assigned(FMapTCoordinates) then
    Result := not VectorEquals(FMapTCoordinates.AsVector, YHmgVector)
  else Result := false;
end;

// SetMappingRCoordinates
//

procedure TGLTexture.SetMappingRCoordinates(const val: TGLCoordinates4);
begin
  MappingRCoordinates.Assign(val);
end;

// GetMappingRCoordinates
//

function TGLTexture.GetMappingRCoordinates: TGLCoordinates4;
begin
  if not Assigned(FMapRCoordinates) then
    FMapRCoordinates := TGLCoordinates4.CreateInitialized(Self, ZHmgVector,
      csVector);
  Result := FMapRCoordinates;
end;

// StoreMappingRCoordinates
//

function TGLTexture.StoreMappingRCoordinates: Boolean;
begin
  if Assigned(FMapRCoordinates) then
    Result := not VectorEquals(FMapRCoordinates.AsVector, ZHmgVector)
  else Result := false;
end;

// SetMappingQCoordinates
//

procedure TGLTexture.SetMappingQCoordinates(const val: TGLCoordinates4);
begin
  MappingQCoordinates.Assign(val);
end;

// GetMappingQCoordinates
//

function TGLTexture.GetMappingQCoordinates: TGLCoordinates4;
begin
  if not Assigned(FMapQCoordinates) then
    FMapQCoordinates := TGLCoordinates4.CreateInitialized(Self, WHmgVector,
      csVector);
  Result := FMapQCoordinates;
end;

// StoreMappingQCoordinates
//

function TGLTexture.StoreMappingQCoordinates: Boolean;
begin
  if Assigned(FMapQCoordinates) then
    Result := not VectorEquals(FMapQCoordinates.AsVector, WHmgVector)
  else Result := false;
end;

// StoreImageClassName
//

function TGLTexture.StoreImageClassName: Boolean;
begin
  Result := (FImage.ClassName <> TGLPersistentImage.ClassName);
end;

// SetTextureCompareMode
//

procedure TGLTexture.SetTextureCompareMode(const val: TGLTextureCompareMode);
begin
  if val <> fTextureCompareMode then
  begin
    fTextureCompareMode := val;
    NotifyParamsChange;
  end;
end;

// SetTextureCompareFunc
//

procedure TGLTexture.SetTextureCompareFunc(const val: TGLDepthCompareFunc);
begin
  if val <> fTextureCompareFunc then
  begin
    fTextureCompareFunc := val;
    NotifyParamsChange;
  end;
end;

// SetDepthTextureMode
//

procedure TGLTexture.SetDepthTextureMode(const val: TGLDepthTextureMode);
begin
  if val <> fDepthTextureMode then
  begin
    fDepthTextureMode := val;
    NotifyParamsChange;
  end;
end;

// PrepareBuildList
//

procedure TGLTexture.PrepareBuildList;
begin
  GetHandle;
end;

// ApplyMappingMode
//

procedure TGLTexture.ApplyMappingMode;
var
  R_Dim: Boolean;
begin
  R_Dim := GL_ARB_texture_cube_map or GL_EXT_texture3D;
  case MappingMode of
    tmmUser: ; // nothing to do, but checked first (common case)
    tmmObjectLinear:
      begin
        glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
        glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
        glTexGenfv(GL_S, GL_OBJECT_PLANE, @MappingSCoordinates.DirectVector);
        glTexGenfv(GL_T, GL_OBJECT_PLANE, @MappingTCoordinates.DirectVector);
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);

        if R_Dim then
        begin
          glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGeni(GL_Q, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenfv(GL_R, GL_OBJECT_PLANE, @MappingRCoordinates.DirectVector);
          glTexGenfv(GL_Q, GL_OBJECT_PLANE, @MappingQCoordinates.DirectVector);
          glEnable(GL_TEXTURE_GEN_R);
          glEnable(GL_TEXTURE_GEN_Q);
        end;
      end;
    tmmEyeLinear:
      begin
        glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
        glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
        // specify planes in eye space, not world space
        glMatrixMode(GL_MODELVIEW);
        glPushMatrix;
        glLoadIdentity;
        glTexGenfv(GL_S, GL_EYE_PLANE, @MappingSCoordinates.DirectVector);
        glTexGenfv(GL_T, GL_EYE_PLANE, @MappingTCoordinates.DirectVector);
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        if R_Dim then
        begin
          glTexGenfv(GL_R, GL_EYE_PLANE, @MappingRCoordinates.DirectVector);
          glTexGenfv(GL_Q, GL_EYE_PLANE, @MappingQCoordinates.DirectVector);
          glEnable(GL_TEXTURE_GEN_R);
          glEnable(GL_TEXTURE_GEN_Q);
        end;
        glPopMatrix;
      end;
    tmmSphere:
      begin
        glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
        glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
      end;
    tmmCubeMapReflection, tmmCubeMapCamera: if GL_ARB_texture_cube_map then
      begin
        glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
        glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
        glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP);
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        glEnable(GL_TEXTURE_GEN_R);
      end;
    tmmCubeMapNormal, tmmCubeMapLight0: if GL_ARB_texture_cube_map then
      begin
        glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
        glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
        glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        glEnable(GL_TEXTURE_GEN_R);
      end;
  else
    Assert(False);
  end;
end;

// ApplyMappingMode
//

procedure TGLTexture.UnApplyMappingMode;
begin
  if MappingMode <> tmmUser then
  begin
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
    if GL_EXT_texture3D or GL_ARB_texture_cube_map then
    begin
      glDisable(GL_TEXTURE_GEN_R);
      glDisable(GL_TEXTURE_GEN_Q);
    end;
  end;
end;

// Apply
//

procedure TGLTexture.Apply(var rci: TRenderContextInfo);

  procedure SetCubeMapTextureMatrix;
  var
    m: TMatrix;
  begin
    // compute model view matrix for proper viewing
    glMatrixMode(GL_TEXTURE);
    case MappingMode of
      tmmCubeMapReflection, tmmCubeMapNormal:
        begin
          m := rci.modelViewMatrix^;
          NormalizeMatrix(m);
          // Transposition = Matrix inversion (matrix is now orthonormal)
          if GL_ARB_transpose_matrix then
            glLoadTransposeMatrixfARB(@m)
          else
          begin
            TransposeMatrix(m);
            glLoadMatrixf(@m);
          end;
        end;
      tmmCubeMapLight0:
        begin
          with TGLScene(rci.scene).Lights do
            if Count > 0 then
            begin
              m := TGLLightSource(Items[0]).AbsoluteMatrix;
              NormalizeMatrix(m);
              if GL_ARB_transpose_matrix then
                glLoadTransposeMatrixfARB(@m)
              else
              begin
                TransposeMatrix(m);
                glLoadMatrixf(@m);
              end;

              m := rci.modelViewMatrix^;
              NormalizeMatrix(m);
              TransposeMatrix(m);
              glMultMatrixf(@m);
            end;
        end;
      tmmCubeMapCamera:
        begin
          m[0] := VectorCrossProduct(rci.cameraUp, rci.cameraDirection);
          m[1] := VectorNegate(rci.cameraDirection);
          m[2] := rci.cameraUp;
          m[3] := WHmgPoint;
          if GL_ARB_transpose_matrix then
            glLoadTransposeMatrixfARB(@m)
          else
          begin
            TransposeMatrix(m);
            glLoadMatrixf(@m);
          end;

          m := rci.modelViewMatrix^;
          NormalizeMatrix(m);
          TransposeMatrix(m);
          glMultMatrixf(@m);
        end;
    end;
    glMatrixMode(GL_MODELVIEW);
  end;

var
  target: TGLEnum;
  fc: Boolean;
begin // Apply
  fc := rci.GLStates.ForwardContext;
  if not Disabled then
  begin
    target := Image.NativeTextureTarget;
    rci.GLStates.SetGLCurrentTexture(0, target, Handle);
    if Handle = 0 then
      Exit;
    case target of
      GL_TEXTURE_1D: rci.GLStates.Enable(stTexture1D);
      GL_TEXTURE_2D: rci.GLStates.Enable(stTexture2D);
      GL_TEXTURE_RECTANGLE: rci.GLStates.Enable(stTextureRect);
      GL_TEXTURE_CUBE_MAP:
        begin
          rci.GLStates.Enable(stTextureCubeMap);
          if not fc then
            SetCubeMapTextureMatrix;
        end;
      GL_TEXTURE_3D: rci.GLStates.Enable(stTexture3D);
    end; // of case

    if not fc then
    begin
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
      glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);
      ApplyMappingMode;
      xglMapTexCoordToMain;
    end;
  end
  else if not fc then
  begin //default
    rci.GLStates.Disable(stTexture1D);
    rci.GLStates.Disable(stTexture2D);
    if GL_ARB_texture_rectangle then
      rci.GLStates.Disable(stTextureRect);
    if GL_ARB_texture_cube_map then
      rci.GLStates.Disable(stTextureCubeMap);
    if GL_EXT_texture3D then
      rci.GLStates.Disable(stTexture3D);
    xglMapTexCoordToMain;
  end;
end;

// UnApply
//

procedure TGLTexture.UnApply(var rci: TRenderContextInfo);
begin
  if not Disabled
    and not rci.GLStates.ForwardContext then
  begin
    if stTexture1D in rci.GLStates.States then
      rci.GLStates.Disable(stTexture1D)
    else if stTexture2D in rci.GLStates.States then
      rci.GLStates.Disable(stTexture2D)
    else if stTextureRect in rci.GLStates.States then
      rci.GLStates.Disable(stTextureRECT)
    else if stTextureCubeMap in rci.GLStates.States then
    begin
      rci.GLStates.Disable(stTextureCubeMap);
      glMatrixMode(GL_TEXTURE);
      glLoadIdentity;
      glMatrixMode(GL_MODELVIEW);
    end
    else if stTexture3D in rci.GLStates.States then
      rci.GLStates.Disable(stTexture3D);
    UnApplyMappingMode;
  end;
end;

// ApplyAsTexture2
//

procedure TGLTexture.ApplyAsTexture2(var rci: TRenderContextInfo; textureMatrix:
  PMatrix = nil);
begin
  ApplyAsTextureN(2, rci, textureMatrix);
end;

// UnApplyAsTexture2
//

procedure TGLTexture.UnApplyAsTexture2(var rci: TRenderContextInfo;
  reloadIdentityTextureMatrix: boolean);
begin
  UnApplyAsTextureN(2, rci, reloadIdentityTextureMatrix);
end;

// ApplyAsTextureN
//

procedure TGLTexture.ApplyAsTextureN(n: Integer; var rci: TRenderContextInfo;
  textureMatrix: PMatrix = nil);
var
  m: TMatrix;
  target: GLenum;
begin
  if not Disabled then
  begin
    rci.GLStates.ActiveTexture := n - 1;
    target := Image.NativeTextureTarget;

    if (target = GL_TEXTURE_CUBE_MAP) and GL_ARB_texture_cube_map then
    begin
      rci.GLStates.Enable(stTextureCubeMap);
      rci.GLStates.SetGLCurrentTexture(n - 1, GL_TEXTURE_CUBE_MAP, Handle);

      // compute model view matrix for proper viewing
      glMatrixMode(GL_TEXTURE);
      m := rci.modelViewMatrix^;
      NormalizeMatrix(m);
      // Transposition = Matrix inversion (matrix is now orthonormal)
      if GL_ARB_transpose_matrix then
        glLoadTransposeMatrixfARB(@m)
      else
      begin
        TransposeMatrix(m);
        glLoadMatrixf(@m);
      end;
      glMatrixMode(GL_MODELVIEW);
    end
    else
    begin
      glEnable(target);
      rci.GLStates.SetGLCurrentTexture(n - 1, target, Handle);

      if Assigned(textureMatrix) then
      begin
        glMatrixMode(GL_TEXTURE);
        glLoadMatrixf(PGLFloat(textureMatrix));
        glMatrixMode(GL_MODELVIEW);
      end;
    end;

    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
    glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);

    ApplyMappingMode;
    rci.GLStates.ActiveTexture := 0;
  end;
end;

// UnApplyAsTextureN
//

procedure TGLTexture.UnApplyAsTextureN(n: Integer; var rci: TRenderContextInfo;
  reloadIdentityTextureMatrix: boolean);
var
  target: GLenum;
begin
  rci.GLStates.ActiveTexture := n - 1;
  UnApplyMappingMode;
  target := Image.NativeTextureTarget;
  if (target = GL_TEXTURE_CUBE_MAP) and GL_ARB_texture_cube_map
    or reloadIdentityTextureMatrix then
  begin
    glMatrixMode(GL_TEXTURE);
    glLoadIdentity;
    glMatrixMode(GL_MODELVIEW);
  end;
  glDisable(target);
  rci.GLStates.ActiveTexture := 0;
end;

// AllocateHandle
//

function TGLTexture.AllocateHandle: TGLuint;
var
  target: TGLUInt;
begin
  target := Image.NativeTextureTarget;
  if tcTarget in FChanges then
    fTextureHandle.DestroyHandle;

  if FTextureHandle.Handle = 0 then
  begin
    FTextureHandle.AllocateHandle;
    Assert(FTextureHandle.Handle <> 0);
    Exclude(FChanges, tcTarget);
    Include(FChanges, tcImage);
    Include(FChanges, tcParams);
  end;

  // bind texture
  if IsTargetSupported(target) then
  begin
    glBindTexture(target, FTextureHandle.Handle);
    if tcParams in FChanges then
    begin
      PrepareParams(target);
      Exclude(FChanges, tcParams);
    end;
    Result := FTextureHandle.Handle;
  end
  else
    Result := 0;
end;

// IsHandleAllocated
//

function TGLTexture.IsHandleAllocated: Boolean;
begin
  Result := (FTextureHandle.Handle <> 0);
end;

// GetHandle
//

function TGLTexture.GetHandle: TGLuint;
var
  i, target: TGLUInt;
  cubeMapSize: Integer;
  cmt: TGLCubeMapTarget;
  cubeMapOk: Boolean;
  cubeMapImage: TGLCubeMapImage;
begin
  if (FTextureHandle.Handle = 0) or (FChanges <> []) then
  begin
    AllocateHandle;

    if tcImage in FChanges then
    begin
      Exclude(FChanges, tcImage);
      // Check supporting
      target := Image.NativeTextureTarget;
      if not IsTargetSupported(target)
        or not IsFormatSupported(TextureFormatEx) then
      begin
        SetTextureErrorImage;
        target := Image.NativeTextureTarget;
      end;
      // Load images
      if Image is TGLCubeMapImage then
      begin
        cubeMapImage := (Image as TGLCubeMapImage);
        // first check if everything is coherent, otherwise, bail out
        cubeMapSize := cubeMapImage.Picture[cmtPX].Width;
        cubeMapOk := (cubeMapSize > 0);
        if cubeMapOk then
        begin
          for cmt := cmtPX to cmtNZ do
            with cubeMapImage.Picture[cmt] do
            begin
              cubeMapOk := (Width = cubeMapSize) and (Height = cubeMapSize);
              if not cubeMapOk then
                Break;
            end;
        end;
        if cubeMapOk then
        begin
          for i := GL_TEXTURE_CUBE_MAP_POSITIVE_X to
            GL_TEXTURE_CUBE_MAP_NEGATIVE_Z do
            PrepareImage(i);
        end;
      end // of TGLCubeMapImage
      else
        PrepareImage(target);
    end;
  end;
  Result := FTextureHandle.Handle;
end;

// DestroyHandles
//

procedure TGLTexture.DestroyHandles;
begin
  FTextureHandle.DestroyHandle;
  FChanges := [tcParams, tcImage, tcTarget];
  FRequiredMemorySize := -1;
end;

// IsFloatType
//

function TGLTexture.IsFloatType: Boolean;
begin
  Result := IsFloatFormat(TextureFormatEx);
end;

// OpenGLTextureFormat
//

function TGLTexture.OpenGLTextureFormat: Integer;
var
  texComp: TGLTextureCompression;
begin
  if GL_ARB_texture_compression then
  begin
    if Compression = tcDefault then
      if vDefaultTextureCompression = tcDefault then
        texComp := tcNone
      else
        texComp := vDefaultTextureCompression
    else
      texComp := Compression;
  end
  else
    texComp := tcNone;

  if IsFloatType then
    texComp := tcNone; // no compression support for float_type

  if (texComp <> tcNone) and (TextureFormat <= tfNormalMap) then
  begin
    case texComp of
      tcStandard: glHint(GL_TEXTURE_COMPRESSION_HINT_ARB, GL_DONT_CARE);
      tcHighQuality: glHint(GL_TEXTURE_COMPRESSION_HINT_ARB, GL_NICEST);
      tcHighSpeed: glHint(GL_TEXTURE_COMPRESSION_HINT_ARB, GL_FASTEST);
    else
      Assert(False);
    end;
    Result := CompressedInternalFormatToOpenGL(TextureFormatEx);
  end
  else
    Result := InternalFormatToOpenGLFormat(TextureFormatEx);
end;

// PrepareImage
//

procedure TGLTexture.PrepareImage(target: TGLUInt);
var
  bitmap32: TGLBitmap32;
begin

  bitmap32 := Image.GetBitmap32(target);

  if (bitmap32 = nil) or bitmap32.IsEmpty then
    Exit;

  if TextureFormat = tfNormalMap then
    bitmap32.GrayScaleToNormalMap(NormalMapScale,
      TextureWrap in [twBoth, twHorizontal],
      TextureWrap in [twBoth, twVertical]);
  // prepare AlphaChannel
  case ImageAlpha of
    tiaDefault: ; // nothing to do
    tiaAlphaFromIntensity:
      bitmap32.SetAlphaFromIntensity;
    tiaSuperBlackTransparent:
      bitmap32.SetAlphaTransparentForColor($000000);
    tiaLuminance:
      bitmap32.SetAlphaFromIntensity;
    tiaLuminanceSqrt:
      begin
        bitmap32.SetAlphaFromIntensity;
        bitmap32.SqrtAlpha;
      end;
    tiaOpaque:
      bitmap32.SetAlphaToValue(255);
    tiaTopLeftPointColorTransparent:
      bitmap32.SetAlphaTransparentForColor(bitmap32.Data^[0]);
    tiaInverseLuminance:
      begin
        bitmap32.SetAlphaFromIntensity;
        bitmap32.InvertAlpha;
      end;
    tiaInverseLuminanceSqrt:
      begin
        bitmap32.SetAlphaFromIntensity;
        bitmap32.SqrtAlpha;
        bitmap32.InvertAlpha;
      end;
    tiaBottomRightPointColorTransparent:
      bitmap32.SetAlphaTransparentForColor(bitmap32.Data^[bitmap32.Width - 1]);
  else
    Assert(False);
  end;
  // apply brightness correction
  if FImageBrightness <> 1.0 then
    bitmap32.BrightnessCorrection(FImageBrightness);
  // apply gamma correction
  if FImageGamma <> 1.0 then
    bitmap32.GammaCorrection(FImageGamma);

  bitmap32.RegisterAsOpenGLTexture(
    target,
    FMinFilter,
    InternalFormatToOpenGLFormat(FTextureFormat),
    FTexWidth,
    FTexHeight,
    FTexDepth);

  if glGetError <> GL_NO_ERROR then
  begin
    ClearGLError;
    SetTextureErrorImage;
  end
  else
  begin
    FRequiredMemorySize := -1;
    TextureImageRequiredMemory;
    Image.ReleaseBitmap32;
  end;
end;

// PrepareParams
//

procedure TGLTexture.PrepareParams(target: TGLUInt);
const
  cTextureSWrap: array[twBoth..twHorizontal] of TGLEnum =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE, GL_REPEAT);
  cTextureTWrap: array[twBoth..twHorizontal] of TGLEnum =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_REPEAT, GL_CLAMP_TO_EDGE);
  cTextureRWrap: array[twBoth..twHorizontal] of TGLEnum =
    (GL_REPEAT, GL_CLAMP_TO_EDGE, GL_REPEAT, GL_CLAMP_TO_EDGE);
  cTextureSWrapOld: array[twBoth..twHorizontal] of TGLEnum =
    (GL_REPEAT, GL_CLAMP, GL_CLAMP, GL_REPEAT);
  cTextureTWrapOld: array[twBoth..twHorizontal] of TGLEnum =
    (GL_REPEAT, GL_CLAMP, GL_REPEAT, GL_CLAMP);
  cTextureMagFilter: array[maNearest..maLinear] of TGLEnum =
    (GL_NEAREST, GL_LINEAR);
  cTextureMinFilter: array[miNearest..miLinearMipmapLinear] of TGLEnum =
    (GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
    GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_LINEAR);
  cFilteringQuality: array[tfIsotropic..tfAnisotropic] of Integer = (1, 2);
  cSeparateTextureWrap: array[twRepeat..twMirrorClampToBorder] of TGLenum =
    (GL_REPEAT, GL_CLAMP, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER,
    GL_MIRRORED_REPEAT, GL_MIRROR_CLAMP_ATI,
    GL_MIRROR_CLAMP_TO_EDGE_ATI, GL_MIRROR_CLAMP_TO_BORDER_EXT);
  cTextureCompareMode: array[tcmNone..tcmCompareRtoTexture] of TGLenum =
    (GL_NONE, GL_COMPARE_R_TO_TEXTURE);
  cDepthTextureMode: array[dtmLuminance..dtmAlpha] of TGLenum =
    (GL_LUMINANCE, GL_INTENSITY, GL_ALPHA);

var
  R_Dim: Boolean;
begin
  R_Dim := GL_ARB_texture_cube_map or GL_EXT_texture3D;

  with FTextureHandle.RenderingContext.GLStates do
  begin
    UnpackAlignment := 4;
    UnpackRowLength :=0;
    UnpackSkipRows :=0;
    UnpackSkipPixels := 0;
  end;

  glTexParameterfv(target, GL_TEXTURE_BORDER_COLOR, FBorderColor.AsAddress);

  if (GL_VERSION_1_2 or GL_EXT_texture_edge_clamp) then
  begin
    if FTextureWrap = twSeparate then
    begin
      glTexParameteri(target, GL_TEXTURE_WRAP_S, cSeparateTextureWrap[FTextureWrapS]);
      glTexParameteri(target, GL_TEXTURE_WRAP_T, cSeparateTextureWrap[FTextureWrapT]);
      if R_Dim then
        glTexParameteri(target, GL_TEXTURE_WRAP_R,
          cSeparateTextureWrap[FTextureWrapR]);
    end
    else
    begin
      glTexParameteri(target, GL_TEXTURE_WRAP_S, cTextureSWrap[FTextureWrap]);
      glTexParameteri(target, GL_TEXTURE_WRAP_T, cTextureTWrap[FTextureWrap]);
      if R_Dim then
        glTexParameteri(target, GL_TEXTURE_WRAP_R, cTextureRWrap[FTextureWrap]);
    end;
  end
  else
  begin
    glTexParameteri(target, GL_TEXTURE_WRAP_S, cTextureSWrapOld[FTextureWrap]);
    glTexParameteri(target, GL_TEXTURE_WRAP_T, cTextureTWrapOld[FTextureWrap]);
  end;

  // Down paramenter to rectangular texture supported
  if (target = GL_TEXTURE_RECTANGLE)
    or not (GL_EXT_texture_lod or GL_SGIS_texture_lod) then
  begin
    if FMinFilter in [miNearestMipmapNearest, miNearestMipmapLinear] then
      FMinFilter := miNearest;
    if FMinFilter in [miLinearMipmapNearest, miLinearMipmapLinear] then
      FMinFilter := miLinear;
  end;

  glTexParameteri(target, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[FMinFilter]);
  glTexParameteri(target, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FMagFilter]);

  if GL_EXT_texture_filter_anisotropic then
    glTexParameteri(target, GL_TEXTURE_MAX_ANISOTROPY_EXT,
      cFilteringQuality[FFilteringQuality]);

  if IsDepthFormat(fTextureFormat) then
  begin
    glTexParameteri(target, GL_TEXTURE_COMPARE_MODE,
      cTextureCompareMode[fTextureCompareMode]);
    glTexParameteri(target, GL_TEXTURE_COMPARE_FUNC,
      cGLComparisonFunctionToGLEnum[fTextureCompareFunc]);
    if not FTextureHandle.RenderingContext.GLStates.ForwardContext then
      glTexParameteri(target, GL_DEPTH_TEXTURE_MODE,
        cDepthTextureMode[fDepthTextureMode]);
  end;
end;

// DoOnTextureNeeded
//

procedure TGLTexture.DoOnTextureNeeded(Sender: TObject; var textureFileName:
  string);
begin
  if Assigned(FOnTextureNeeded) then
    FOnTextureNeeded(Sender, textureFileName);
end;

procedure TGLTexture.GetFloatTexImage(RenderingContext: TGLContext; data:
  pointer);
var
  target: TGLEnum;
begin
  RenderingContext.Activate;
  target := Image.NativeTextureTarget;
  glBindTexture(target, FTextureHandle.Handle);
  glGetTexImage(target, 0, GL_RGBA, GL_FLOAT, data);
  RenderingContext.Deactivate;
end;

procedure TGLTexture.SetFloatTexImage(RenderingContext: TGLContext; data:
  pointer);
var
  target: TGLEnum;
begin
  RenderingContext.Activate;
  target := Image.NativeTextureTarget;
  glBindTexture(target, FTextureHandle.Handle);
  glTexImage2d(target, 0, GL_RGBA_FLOAT16_ATI, TexWidth, TexHeight, 0, GL_RGBA,
    GL_FLOAT, data);
  RenderingContext.Deactivate;
end;

procedure TGLTexture.SetTextureErrorImage;
const
{$I TextureError.inc}
var
  bmp32: TGLBitmap32;
begin
  bmp32 := TGLBitmap32.Create;
  bmp32.Width := 64;
  bmp32.Height := 64;
  bmp32.ColorFormat := GL_RGB;
  bmp32.Blank := false;
  Move(cTextureError[0], bmp32.Data[0], bmp32.DataSize);
  ImageClassName := TGLBlankImage.ClassName;
  TGLBlankImage(Image).Assign(bmp32);
  bmp32.Free;
  TextureFormatEx := tfRGB8;
  MagFilter := maNearest;
  MinFilter := miNearest;
  TextureWrap := twBoth;
  MappingMode := tmmUser;
  Compression := tcNone;
  AllocateHandle;
end;

function TGLTexture.GetRenderingContext: TGLContext;
begin
  Assert(IsHandleAllocated);
  Result := FTextureHandle.RenderingContext;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ---------------
// --------------- TGLTextureExItem ---------------
// ---------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLTextureExItem'}{$ENDIF}

// Create
//

constructor TGLTextureExItem.Create(ACollection: TCollection);
begin
  inherited;

  FTexture := TGLTexture.Create(Self);
  FTextureOffset := TGLCoordinates.CreateInitialized(Self, NullHMGVector,
    csPoint);
  FTextureOffset.OnNotifyChange := OnNotifyChange;
  FTextureScale := TGLCoordinates.CreateInitialized(Self, XYZHmgVector,
    csPoint);
  FTextureScale.OnNotifyChange := OnNotifyChange;

  FTextureIndex := ID;
  FTextureMatrix := IdentityHMGMatrix;

  //DanB - hmmm, not very flexible code, assumes it's owned by a material,
  // that has a Texture property, but may need to re-implement it somehow
{  if ACollection is TGLTextureEx then
    if TGLTextureEx(ACollection).FOwner <> nil then
      FTexture.OnTextureNeeded := TGLTextureEx(ACollection).FOwner.Texture.OnTextureNeeded;
      }
end;

// Destroy
//

destructor TGLTextureExItem.Destroy;
begin
  FTexture.Free;
  FTextureOffset.Free;
  FTextureScale.Free;

  inherited;
end;

// QueryInterface
//

function TGLTextureExItem.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

// _AddRef
//

function TGLTextureExItem._AddRef: Integer;
begin
  Result := -1; //ignore
end;

// _Release
//

function TGLTextureExItem._Release: Integer;
begin
  Result := -1; //ignore
end;

// Assign
//

procedure TGLTextureExItem.Assign(Source: TPersistent);
begin
  if Source is TGLTextureExItem then
  begin
    Texture := TGLTextureExItem(Source).Texture;
    TextureIndex := TGLTextureExItem(Source).TextureIndex;
    TextureOffset := TGLTextureExItem(Source).TextureOffset;
    TextureScale := TGLTextureExItem(Source).TextureScale;
    NotifyChange(Self);
  end
  else
    inherited;
end;

// NotifyChange
//

procedure TGLTextureExItem.NotifyChange(Sender: TObject);
begin
  if Assigned(Collection) then
    TGLTextureEx(Collection).NotifyChange(Self);
end;

// Apply
//

procedure TGLTextureExItem.Apply(var rci: TRenderContextInfo);
begin
  FApplied := False;
  if FTexture.Enabled then
  begin
    rci.GLStates.ActiveTexture := FTextureIndex;
    glMatrixMode(GL_TEXTURE);
    glPushMatrix;
    if FTextureMatrixIsIdentity then
      glLoadIdentity
    else
      glLoadMatrixf(@FTextureMatrix[0][0]);
    glMatrixMode(GL_MODELVIEW);
    rci.GLStates.ActiveTexture := 0;
    if FTextureIndex = 0 then
      FTexture.Apply(rci)
    else if FTextureIndex = 1 then
      FTexture.ApplyAsTexture2(rci, nil)
    else if FTextureIndex >= 2 then
      FTexture.ApplyAsTextureN(FTextureIndex + 1, rci, nil);
    FApplied := True;
  end;
end;

// UnApply
//

procedure TGLTextureExItem.UnApply(var rci: TRenderContextInfo);
begin
  if FApplied then
  begin
    if FTextureIndex = 0 then
      FTexture.UnApply(rci)
    else if FTextureIndex = 1 then
      FTexture.UnApplyAsTexture2(rci, false)
    else if FTextureIndex >= 2 then
      FTexture.UnApplyAsTextureN(FTextureIndex + 1, rci, false);
    rci.GLStates.ActiveTexture := FTextureIndex;
    glMatrixMode(GL_TEXTURE);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    rci.GLStates.ActiveTexture := 0;
    FApplied := False;
  end;
end;

// GetDisplayName
//

function TGLTextureExItem.GetDisplayName: string;
begin
  Result := Format('Tex [%d]', [FTextureIndex]);
end;

// GetOwner
//

function TGLTextureExItem.GetOwner: TPersistent;
begin
  Result := Collection;
end;

// NotifyTexMapChange
//

procedure TGLTextureExItem.NotifyTexMapChange(Sender: TObject);
var
  intf: IGLTextureNotifyAble;
begin
  if Supports(TObject(TGLTextureEx(Collection).FOwner), IGLTextureNotifyAble,
    intf) then
    intf.NotifyTexMapChange(Sender);
end;

// SetTexture
//

procedure TGLTextureExItem.SetTexture(const Value: TGLTexture);
begin
  FTexture.Assign(Value);
  NotifyChange(Self);
end;

// SetTextureIndex
//

procedure TGLTextureExItem.SetTextureIndex(const Value: Integer);
var
  temp: Integer;
begin
  temp := Value;
  if temp < 0 then
    temp := 0;
  if temp <> FTextureIndex then
  begin
    FTextureIndex := temp;
    NotifyChange(Self);
  end;
end;

// SetTextureOffset
//

procedure TGLTextureExItem.SetTextureOffset(const Value: TGLCoordinates);
begin
  FTextureOffset.Assign(Value);
  NotifyChange(Self);
end;

// SetTextureScale
//

procedure TGLTextureExItem.SetTextureScale(const Value: TGLCoordinates);
begin
  FTextureScale.Assign(Value);
  NotifyChange(Self);
end;

// CalculateTextureMatrix
//

procedure TGLTextureExItem.CalculateTextureMatrix;
begin
  if TextureOffset.Equals(NullHmgVector) and TextureScale.Equals(XYZHmgVector)
    then
    FTextureMatrixIsIdentity := True
  else
  begin
    FTextureMatrixIsIdentity := False;
    FTextureMatrix := CreateScaleAndTranslationMatrix(TextureScale.AsVector,
      TextureOffset.AsVector);
  end;
  NotifyChange(Self);
end;

// OnNotifyChange
//

procedure TGLTextureExItem.OnNotifyChange(Sender: TObject);
begin
  CalculateTextureMatrix;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ---------------
// --------------- TGLTextureEx ---------------
// ---------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLTextureEx'}{$ENDIF}

// Create
//

constructor TGLTextureEx.Create(AOwner: TGLUpdateAbleObject);
begin
  inherited Create(TGLTextureExItem);

  FOwner := AOwner;
end;

// NotifyChange
//

procedure TGLTextureEx.NotifyChange(Sender: TObject);
begin
  if Assigned(FOwner) then
    FOwner.NotifyChange(Self);
end;

// Apply
//

procedure TGLTextureEx.Apply(var rci: TRenderContextInfo);
var
  i, texUnits: Integer;
  units: Cardinal;
begin
  if not GL_ARB_multitexture then
    exit;

  units := 0;
  glGetIntegerv(GL_MAX_TEXTURE_UNITS, @texUnits);
  for i := 0 to Count - 1 do
  begin
    if Items[i].TextureIndex < texUnits then
    begin
      Items[i].Apply(rci);
      if Items[i].FApplied then
        if (Items[i].TextureIndex > 0) and (Items[i].Texture.MappingMode =
          tmmUser) then
          units := units or (1 shl Items[i].TextureIndex);
    end;
  end;
  if units > 0 then
    xglMapTexCoordToArbitraryAdd(units);
end;

// UnApply
//

procedure TGLTextureEx.UnApply(var rci: TRenderContextInfo);
var
  i: Integer;
begin
  if not GL_ARB_multitexture then
    exit;
  for i := 0 to Count - 1 do
    Items[i].UnApply(rci);
end;

// Add
//

function TGLTextureEx.Add: TGLTextureExItem;
begin
  Result := TGLTextureExItem(inherited Add);
end;

// Loaded
//

procedure TGLTextureEx.Loaded;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].CalculateTextureMatrix;
end;

// GetOwner
//

function TGLTextureEx.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// SetItems
//

procedure TGLTextureEx.SetItems(index: Integer; const Value: TGLTextureExItem);
begin
  inherited SetItem(index, Value);
end;

// GetItems
//

function TGLTextureEx.GetItems(index: Integer): TGLTextureExItem;
begin
  Result := TGLTextureExItem(inherited GetItem(index));
end;

// IsTextureEnabled
//

function TGLTextureEx.IsTextureEnabled(Index: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Self = nil then
    Exit;
  for i := 0 to Count - 1 do
    if Items[i].TextureIndex = Index then
      Result := Result or Items[i].Texture.Enabled;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TGLFloatDataImage ------------------
// ------------------

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLFloatDataImage'}{$ENDIF}

// Create
//

constructor TGLFloatDataImage.Create(AOwner: TPersistent);
begin
  inherited;
  FWidth := 256;
  FHeight := 256;
end;

// Destroy
//

destructor TGLFloatDataImage.Destroy;
begin
  ReleaseBitmap32;
  inherited Destroy;
end;

// Assign
//

procedure TGLFloatDataImage.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if (Source is TGLFloatDataImage) then
    begin
      FWidth := TGLFloatDataImage(Source).FWidth;
      FHeight := TGLFloatDataImage(Source).FHeight;
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

procedure TGLFloatDataImage.SetWidth(val: Integer);
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

function TGLFloatDataImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

// SetHeight
//

procedure TGLFloatDataImage.SetHeight(val: Integer);
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

function TGLFloatDataImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

// GetDepth
//

function TGLFloatDataImage.GetDepth: Integer;
begin
  Result := 0;
end;

// SetHeight
//

procedure TGLFloatDataImage.SetDepth(val: Integer);
begin

end;

// GetBitmap32
//

function TGLFloatDataImage.GetBitmap32(target: TGLUInt): TGLBitmap32;
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

procedure TGLFloatDataImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

// SaveToFile
//

procedure TGLFloatDataImage.SaveToFile(const fileName: string);
begin
  SaveAnsiStringToFile(fileName, AnsiString('[FloatDataImage]'#13#10'Width=' +
    IntToStr(Width)
    + #13#10'Height=' + IntToStr(Height)));
end;

// LoadFromFile
//

procedure TGLFloatDataImage.LoadFromFile(const fileName: string);
var
  sl: TStringList;
  buf: string;
begin
  buf := fileName;
  if Assigned(FOnTextureNeeded) then
    FOnTextureNeeded(Self, buf);
  if FileExists(buf) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(buf{$IFDEF GLS_DELPHI_2009_UP}, TEncoding.ASCII{$ENDIF});
      FWidth := StrToInt(sl.Values['Width']);
      FHeight := StrToInt(sl.Values['Height']);
    finally
      sl.Free;
    end;
  end
  else
  begin
    Assert(False, Format(glsFailedOpenFile, [fileName]));
  end;
end;

// FriendlyName
//

class function TGLFloatDataImage.FriendlyName: string;
begin
  Result := 'FloatData Image';
end;

// FriendlyDescription
//

class function TGLFloatDataImage.FriendlyDescription: string;
begin
  Result := 'Image to be dynamically generated by OpenGL';
end;

// GetTextureTarget
//

function TGLFloatDataImage.GetTextureTarget: GLenum;
begin
  Result := GL_TEXTURE_2D;
  if fPreviousTarget <> Result then
  begin
    if Assigned(FOwnerTexture) then
      FOwnerTexture.NotifyTargetChange;
    fPreviousTarget := Result;
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterGLTextureImageClass(TGLBlankImage);
  RegisterGLTextureImageClass(TGLPersistentImage);
  RegisterGLTextureImageClass(TGLPicFileImage);
  RegisterGLTextureImageClass(TGLCubeMapImage);
  RegisterGLTextureImageClass(TGLFloatDataImage);

  RegisterTGraphicClassFileExtension('.bmp', TGLBitmap);

finalization

  vGLTextureImageClasses.Free;
  vGLTextureImageClasses := nil;

end.

