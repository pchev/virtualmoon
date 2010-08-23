//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLMaterial<p>

 Handles all the material + material library stuff.<p>

 <b>History : </b><font size=-1><ul>
      <li>06/03/10 - Yar - Added to TGLDepthProperties DepthClamp property
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>21/02/10 - Yar - Added TGLDepthProperties,
                           optimization of switching states
      <li>22/01/10 - Yar - Remove Texture.Border and
                           added MappingRCoordinates, MappingQCoordinates
                           to WriteToFiler, ReadFromFiler
      <li>07/01/10 - DaStr - TexturePaths are now cross-platform (thanks Predator)
      <li>22/12/09 - DaStr - Updated TGLMaterialLibrary.WriteToFiler(),
                              ReadFromFiler() (thanks dAlex)
                             Small update for blending constants
      <li>13/12/09 - DaStr - Added a temporary work-around for multithread
                              mode (thanks Controller)
                             Added TGLBlendingParameters and bmCustom blending
                              mode(thanks DungeonLords, Fantom)
                             Fixed code formating in some places
      <li>24/08/09 - DaStr - Updated TGLLibMaterial.DoOnTextureNeeded:
                              Replaced IncludeTrailingBackslash() with
                              IncludeTrailingPathDelimiter()
      <li>28/07/09 - DaStr - Updated TGLShader.GetStardardNotSupportedMessage()
                              to use component name instead class name
      <li>24/07/09 - DaStr - TGLShader.DoInitialize() now passes rci
                              (BugTracker ID = 2826217)
      <li>14/07/09 - DaStr - Added $I GLScene.inc
      <li>08/10/08 - DanB - Created from split from GLTexture.pas,
                            Textures + materials are no longer so tightly bound
   </ul></font>
}
unit GLMaterial;

interface

uses Classes, GLRenderContextInfo, BaseClasses, GLContext, GLTexture, GLColor,
  GLCoordinates, VectorGeometry, PersistentClasses, OpenGL1x, GLCrossPlatform,
  GLState;

{$I GLScene.inc}
{$UNDEF GLS_MULTITHREAD}
type
  TGLFaceProperties = class;
  TGLMaterial = class;
  TGLMaterialLibrary = class;

  //an interface for proper TGLLibMaterialNameProperty support
  IGLMaterialLibrarySupported = interface(IInterface)
    ['{8E442AF9-D212-4A5E-8A88-92F798BABFD1}']
    function GetMaterialLibrary: TGLMaterialLibrary;
  end;

  TGLLibMaterial = class;

  // TGLShaderStyle
  //
  {: Define GLShader style application relatively to a material.<ul>
     <li>ssHighLevel: shader is applied before material application, and unapplied
           after material unapplication
     <li>ssLowLevel: shader is applied after material application, and unapplied
           before material unapplication
     <li>ssReplace: shader is applied in place of the material (and material
           is completely ignored)
     </ul> }
  TGLShaderStyle = (ssHighLevel, ssLowLevel, ssReplace);

  // TGLShaderFailedInitAction
  //
  {: Defines what to do if for some reason shader failed to initialize.<ul>
     <li>fiaSilentdisable:          just disable it
     <li>fiaRaiseHandledException:  raise an exception, and handle it right away
                                    (usefull, when debigging within Delphi)
     <li>fiaRaiseStardardException: raises the exception with a string from this
                                      function GetStardardNotSupportedMessage
     <li>fiaReRaiseException:       Re-raises the exception
     <li>fiaGenerateEvent:          Handles the exception, but generates an event
                                    that user can respond to. For example, he can
                                    try to compile a substitude shader, or replace
                                    it by a material.
                                    Note: HandleFailedInitialization does *not*
                                    create this event, it is left to user shaders
                                    which may chose to override this procedure.
                                    Commented out, because not sure if this
                                    option should exist, let other generations of
                                    developers decide ;)
     </ul> }
  TGLShaderFailedInitAction = (
    fiaSilentDisable, fiaRaiseStandardException,
    fiaRaiseHandledException, fiaReRaiseException
    {,fiaGenerateEvent});

  // TGLShader
  //
  {: Generic, abstract shader class.<p>
     Shaders are modeled here as an abstract material-altering entity with
     transaction-like behaviour. The base class provides basic context and user
     tracking, as well as setup/application facilities.<br>
     Subclasses are expected to provide implementation for DoInitialize,
     DoApply, DoUnApply and DoFinalize. }
  TGLShader = class(TGLUpdateAbleComponent)
  private
    { Private Declarations }
    FEnabled: Boolean;
    FLibMatUsers: TList;
    FVirtualHandle: TGLVirtualHandle;
    FShaderStyle: TGLShaderStyle;
    FUpdateCount: Integer;
    FShaderActive, FShaderInitialized: Boolean;
    FFailedInitAction: TGLShaderFailedInitAction;

  protected
    { Protected Declarations }
          {: Invoked once, before the first call to DoApply.<p>
             The call happens with the OpenGL context being active. }
    procedure DoInitialize(var rci: TRenderContextInfo; Sender: TObject);
      dynamic;
    {: Request to apply the shader.<p>
       Always followed by a DoUnApply when the shader is no longer needed. }
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); virtual;
      abstract;
    {: Request to un-apply the shader.<p>
       Subclasses can assume the shader has been applied previously.<br>
       Return True to request a multipass. }
    function DoUnApply(var rci: TRenderContextInfo): Boolean; virtual; abstract;
    {: Invoked once, before the destruction of context or release of shader.<p>
       The call happens with the OpenGL context being active. }
    procedure DoFinalize; dynamic;

    function GetShaderInitialized: Boolean;
    procedure InitializeShader(var rci: TRenderContextInfo; Sender: TObject);
    procedure FinalizeShader;
    procedure OnVirtualHandleAllocate(sender: TGLVirtualHandle; var handle:
      Cardinal);
    procedure OnVirtualHandleDestroy(sender: TGLVirtualHandle; var handle:
      Cardinal);
    procedure SetEnabled(val: Boolean);

    property ShaderInitialized: Boolean read GetShaderInitialized;
    property ShaderActive: Boolean read FShaderActive;

    procedure RegisterUser(libMat: TGLLibMaterial);
    procedure UnRegisterUser(libMat: TGLLibMaterial);

    {: Used by the DoInitialize procedure of descendant classes to raise errors. }
    procedure HandleFailedInitialization(const LastErrorMessage: string = '');
      virtual;

    {: May be this should be a function inside HandleFailedInitialization... }
    function GetStardardNotSupportedMessage: string; virtual;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {: Subclasses should invoke this function when shader properties are altered.
        This procedure can also be used to reset/recompile the shader. }
    procedure NotifyChange(Sender: TObject); override;
    procedure BeginUpdate;
    procedure EndUpdate;

    {: Apply shader to OpenGL state machine.}
    procedure Apply(var rci: TRenderContextInfo; Sender: TObject);
    {: UnApply shader.<p>
       When returning True, the caller is expected to perform a multipass
       rendering by re-rendering then invoking UnApply again, until a
       "False" is returned. }
    function UnApply(var rci: TRenderContextInfo): Boolean;

    {: Shader application style (default is ssLowLevel). }
    property ShaderStyle: TGLShaderStyle read FShaderStyle write FShaderStyle
      default ssLowLevel;

    procedure Assign(Source: TPersistent); override;

    {: Defines if shader is supported by hardware/drivers.
       Default - always supported. Descendants are encouraged to override
       this function. }
    function ShaderSupported: Boolean; virtual;

    {: Defines what to do if for some reason shader failed to initialize.
       Note, that in some cases it cannon be determined by just checking the
       required OpenGL extentions. You need to try to compile and link the
       shader - only at that stage you might catch an error }
    property FailedInitAction: TGLShaderFailedInitAction
      read FFailedInitAction write FFailedInitAction default
      fiaRaiseStandardException;

  published
    { Published Declarations }
      {: Turns on/off shader application.<p>
         Note that this only turns on/off the shader application, if the
         ShaderStyle is ssReplace, the material won't be applied even if
         the shader is disabled. }
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TGLShaderClass = class of TGLShader;

  TShininess = 0..128;

  // TGLFaceProperties
  //
  {: Stores basic face lighting properties.<p>
     The lighting is described with the standard ambient/diffuse/emission/specular
     properties that behave like those of most rendering tools.<br>
     You also have control over shininess (governs specular lighting) and
     polygon mode (lines / fill). }
  TGLFaceProperties = class(TGLUpdateAbleObject)
  private
    { Private Declarations }
    FAmbient, FDiffuse, FSpecular, FEmission: TGLColor;
    FPolygonMode: TPolygonMode;
    FShininess: TShininess;

  protected
    { Protected Declarations }
    procedure SetAmbient(AValue: TGLColor);
    procedure SetDiffuse(AValue: TGLColor);
    procedure SetEmission(AValue: TGLColor);
    procedure SetSpecular(AValue: TGLColor);
    procedure SetPolygonMode(AValue: TPolygonMode);
    procedure SetShininess(AValue: TShininess);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Apply(var rci: TRenderContextInfo; aFace: TCullFaceMode);
    procedure ApplyNoLighting(var rci: TRenderContextInfo; aFace:
      TCullFaceMode);
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    property Ambient: TGLColor read FAmbient write SetAmbient;
    property Diffuse: TGLColor read FDiffuse write SetDiffuse;
    property Emission: TGLColor read FEmission write SetEmission;
    property Shininess: TShininess read FShininess write SetShininess default 0;
    property PolygonMode: TPolygonMode read FPolygonMode write SetPolygonMode
      default pmFill;
    property Specular: TGLColor read FSpecular write SetSpecular;
  end;

  TGLDepthProperties = class(TGLUpdateAbleObject)
  private
    { Private Declarations }
    FDepthTest: boolean;
    FDepthWrite: boolean;
    FZNear, FZFar: Single;
    FCompareFunc: TDepthfunction;
    FDepthClamp: Boolean;
  protected
    { Protected Declarations }
    procedure SetZNear(Value: Single);
    procedure SetZFar(Value: Single);
    procedure SetCompareFunc(Value: TGLDepthCompareFunc);
    procedure SetDepthTest(Value: boolean);
    procedure SetDepthWrite(Value: boolean);
    procedure SetDepthClamp(Value: boolean);

    function StoreZNear: Boolean;
    function StoreZFar: Boolean;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;

    procedure Apply(var rci: TRenderContextInfo);
    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
    {: Specifies the mapping of the near clipping plane to
       window coordinates.  The initial value is 0.  }
    property ZNear: Single read FZNear write SetZNear stored StoreZNear;
    {: Specifies the mapping of the far clipping plane to
       window coordinates.  The initial value is 1. }
    property ZFar: Single read FZFar write SetZFar stored StoreZFar;
    {: Specifies the function used to compare each
      incoming pixel depth value with the depth value present in
      the depth buffer. }
    property DepthCompareFunction: TDepthFunction
      read FCompareFunc write SetCompareFunc default cfLequal;
    {: DepthTest enabling.<p>
       When DepthTest is enabled, objects closer to the camera will hide
       farther ones (via use of Z-Buffering).<br>
       When DepthTest is disabled, the latest objects drawn/rendered overlap
       all previous objects, whatever their distance to the camera.<br>
       Even when DepthTest is enabled, objects may chose to ignore depth
       testing through the osIgnoreDepthBuffer of their ObjectStyle property. }
    property DepthTest: boolean read FDepthTest write SetDepthTest default True;
    {: If True, object will not write to Z-Buffer. }
    property DepthWrite: boolean read FDepthWrite write SetDepthWrite default
      True;
    {: Enable clipping depth to the near and far planes }
    property DepthClamp: Boolean read FDepthClamp write SetDepthClamp default
      False;
  end;

  TGLLibMaterialName = string;

  //
  // DaStr: if you write smth like af_GL_NEVER = GL_NEVER in the definition,
  // it won't show up in the Dephi 7 design-time editor. So I had to add
  // vTGlAlphaFuncValues and vTGLBlendFuncFactorValues arrays.
  //
  TGlAlphaFunc = TComparisonFunction;

  // TGLBlendingParameters
  //
  TGLBlendingParameters = class(TGLUpdateAbleObject)
  private
    FUseAlphaFunc: Boolean;
    FUseBlendFunc: Boolean;
    FAlphaFuncType: TGlAlphaFunc;
    FAlphaFuncRef: TGLclampf;
    FBlendFuncSFactor: TBlendFunction;
    FBlendFuncDFactor: TBlendFunction;
    procedure SetUseAlphaFunc(const Value: Boolean);
    procedure SetUseBlendFunc(const Value: Boolean);
    procedure SetAlphaFuncRef(const Value: TGLclampf);
    procedure SetAlphaFuncType(const Value: TGlAlphaFunc);
    procedure SetBlendFuncDFactor(const Value: TBlendFunction);
    procedure SetBlendFuncSFactor(const Value: TBlendFunction);
    function StoreAlphaFuncRef: Boolean;
  protected
    function GetRealOwner: TGLMaterial;
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Apply(var rci: TRenderContextInfo);
  published
    property UseAlphaFunc: Boolean read FUseAlphaFunc write SetUseAlphaFunc
      default False;
    property AlphaFunctType: TGlAlphaFunc read FAlphaFuncType write
      SetAlphaFuncType default cfGreater;
    property AlphaFuncRef: TGLclampf read FAlphaFuncRef write SetAlphaFuncRef
      stored StoreAlphaFuncRef;

    property UseBlendFunc: Boolean read FUseBlendFunc write SetUseBlendFunc
      default True;
    property BlendFuncSFactor: TBlendFunction read FBlendFuncSFactor write
      SetBlendFuncSFactor default bfSrcAlpha;
    property BlendFuncDFactor: TBlendFunction read FBlendFuncDFactor write
      SetBlendFuncDFactor default bfOneMinusSrcAlpha;
  end;

  // TBlendingMode
  //
  {: Simplified blending options.<p>
     bmOpaque : disable blending<br>
     bmTransparency : uses standard alpha blending<br>
     bmAdditive : activates additive blending (with saturation)<br>
     bmAlphaTest50 : uses opaque blending, with alpha-testing at 50% (full
        transparency if alpha is below 0.5, full opacity otherwise)<br>
     bmAlphaTest100 : uses opaque blending, with alpha-testing at 100%<br>
     bmModulate : uses modulation blending<br>
     bmCustom : uses TGLBlendingParameters options
     }
  TBlendingMode = (bmOpaque, bmTransparency, bmAdditive,
    bmAlphaTest50, bmAlphaTest100, bmModulate, bmCustom);

  // TFaceCulling
  //
  TFaceCulling = (fcBufferDefault, fcCull, fcNoCull);

  // TMaterialOptions
  //
  {: Control special rendering options for a material.<p>
     moIgnoreFog : fog is deactivated when the material is rendered }
  TMaterialOption = (moIgnoreFog, moNoLighting);
  TMaterialOptions = set of TMaterialOption;

  // TGLMaterial
   //
   {: Describes a rendering material.<p>
      A material is basicly a set of face properties (front and back) that take
      care of standard material rendering parameters (diffuse, ambient, emission
      and specular) and texture mapping.<br>
      An instance of this class is available for almost all objects in GLScene
      to allow quick definition of material properties. It can link to a
      TGLLibMaterial (taken for a material library).<p>
      The TGLLibMaterial has more adavanced properties (like texture transforms)
      and provides a standard way of sharing definitions and texture maps. }
  TGLMaterial = class(TGLUpdateAbleObject, IGLMaterialLibrarySupported,
      IGLNotifyAble, IGLTextureNotifyAble)
  private
    { Private Declarations }
    FFrontProperties, FGLBackProperties: TGLFaceProperties;
    FDepthProperties: TGLDepthProperties;
    FBlendingMode: TBlendingMode;
    FBlendingParams: TGLBlendingParameters;
    FTexture: TGLTexture;
    FTextureEx: TGLTextureEx;
    FMaterialLibrary: TGLMaterialLibrary;
    FLibMaterialName: TGLLibMaterialName;
    FMaterialOptions: TMaterialOptions;
    FFaceCulling: TFaceCulling;
    currentLibMaterial: TGLLibMaterial;

    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TGLMaterialLibrary;
  protected
    { Protected Declarations }
    function GetBackProperties: TGLFaceProperties;
    procedure SetBackProperties(Values: TGLFaceProperties);
    procedure SetFrontProperties(Values: TGLFaceProperties);
    procedure SetDepthProperties(Values: TGLDepthProperties);
    procedure SetBlendingMode(const val: TBlendingMode);
    procedure SetMaterialOptions(const val: TMaterialOptions);
    function GetTexture: TGLTexture;
    procedure SetTexture(ATexture: TGLTexture);
    procedure SetMaterialLibrary(const val: TGLMaterialLibrary);
    procedure SetLibMaterialName(const val: TGLLibMaterialName);
    procedure SetFaceCulling(const val: TFaceCulling);
    function GetTextureEx: TGLTextureEx;
    procedure SetTextureEx(const value: TGLTextureEx);
    function StoreTextureEx: Boolean;
    procedure SetBlendingParams(const Value: TGLBlendingParameters);

    procedure NotifyLibMaterialDestruction;
    //: Back, Front, Texture and blending not stored if linked to a LibMaterial
    function StoreMaterialProps: Boolean;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure PrepareBuildList;
    procedure Apply(var rci: TRenderContextInfo);
    {: Restore non-standard material states that were altered;<p>
       A return value of True is a multipass request. }
    function UnApply(var rci: TRenderContextInfo): Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure NotifyChange(Sender: TObject); override;
    procedure NotifyTexMapChange(Sender: TObject);
    procedure DestroyHandles;

    procedure Loaded;

    {: Returns True if the material is blended.<p>
       Will return the libmaterial's blending if it is linked to a material
       library. }
    function Blended: Boolean;

    //: True if the material has a secondary texture
    function HasSecondaryTexture: Boolean;

    //: True if the material comes from the library instead of the texture property
    function MaterialIsLinkedToLib: Boolean;

    //: Gets the primary texture either from material library or the texture property
    function GetActualPrimaryTexture: TGLTexture;

    //: Gets the primary Material either from material library or the texture property
    function GetActualPrimaryMaterial: TGLMaterial;

    //: Return the LibMaterial (see LibMaterialName)
    function GetLibMaterial: TGLLibMaterial;

    procedure QuickAssignMaterial(const MaterialLibrary: TGLMaterialLibrary;
      const Material: TGLLibMaterial);
  published
    { Published Declarations }
    property BackProperties: TGLFaceProperties read GetBackProperties write
      SetBackProperties stored StoreMaterialProps;
    property FrontProperties: TGLFaceProperties read FFrontProperties write
      SetFrontProperties stored StoreMaterialProps;
    property DepthProperties: TGLDepthProperties read FDepthProperties write
      SetDepthProperties stored StoreMaterialProps;
    property BlendingMode: TBlendingMode read FBlendingMode write SetBlendingMode
      stored StoreMaterialProps default bmOpaque;
    property BlendingParams: TGLBlendingParameters read FBlendingParams write
      SetBlendingParams;

    property MaterialOptions: TMaterialOptions read FMaterialOptions write
      SetMaterialOptions default [];
    property Texture: TGLTexture read GetTexture write SetTexture stored
      StoreMaterialProps;
    property FaceCulling: TFaceCulling read FFaceCulling write SetFaceCulling
      default fcBufferDefault;

    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;
    property LibMaterialName: TGLLibMaterialName read FLibMaterialName write
      SetLibMaterialName;
    property TextureEx: TGLTextureEx read GetTextureEx write SetTextureEx stored
      StoreTextureEx;
  end;

  // TGLLibMaterial
  //
    {: Material in a material library.<p>
       Introduces Texture transformations (offset and scale). Those transformations
       are available only for lib materials to minimize the memory cost of basic
       materials (which are used in almost all objects). }
  TGLLibMaterial = class(TCollectionItem, IGLMaterialLibrarySupported,
      IGLNotifyAble, IGLTextureNotifyAble)
  private
    { Private Declarations }
    userList: TList;
    FName: TGLLibMaterialName;
    FNameHashKey: Integer;
    FMaterial: TGLMaterial;
    FTextureOffset, FTextureScale: TGLCoordinates;
    FTextureMatrixIsIdentity: Boolean;
    FTextureMatrix: TMatrix;
    FTexture2Name: TGLLibMaterialName;
    FShader: TGLShader;
    notifying: Boolean; // used for recursivity protection
    libMatTexture2: TGLLibMaterial; // internal cache
    FTag: Integer;
    //implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TGLMaterialLibrary;
    //implementing IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    { Protected Declarations }
    function GetDisplayName: string; override;
    procedure Loaded;

    class function ComputeNameHashKey(const name: string): Integer;

    procedure SetName(const val: TGLLibMaterialName);
    procedure SetMaterial(const val: TGLMaterial);
    procedure SetTextureOffset(const val: TGLCoordinates);
    procedure SetTextureScale(const val: TGLCoordinates);
    procedure SetTexture2Name(const val: TGLLibMaterialName);
    procedure SetShader(const val: TGLShader);

    procedure CalculateTextureMatrix;
    procedure DestroyHandles;
    procedure OnNotifyChange(Sender: TObject);
    procedure DoOnTextureNeeded(Sender: TObject; var textureFileName: string);

  public
    { Public Declarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure PrepareBuildList;
    procedure Apply(var rci: TRenderContextInfo);
    //: Restore non-standard material states that were altered
    function UnApply(var rci: TRenderContextInfo): Boolean;

    procedure RegisterUser(obj: TGLUpdateAbleObject); overload;
    procedure UnregisterUser(obj: TGLUpdateAbleObject); overload;
    procedure RegisterUser(comp: TGLUpdateAbleComponent); overload;
    procedure UnregisterUser(comp: TGLUpdateAbleComponent); overload;
    procedure RegisterUser(libMaterial: TGLLibMaterial); overload;
    procedure UnregisterUser(libMaterial: TGLLibMaterial); overload;
    procedure NotifyUsers;
    procedure NotifyUsersOfTexMapChange;
    function IsUsed: boolean; //returns true if the texture has registed users
    property NameHashKey: Integer read FNameHashKey;
    property TextureMatrix: TMatrix read FTextureMatrix;
    property TextureMatrixIsIdentity: boolean read FTextureMatrixIsIdentity;
    procedure NotifyTexMapChange(Sender: TObject);
    procedure NotifyChange(Sender: TObject);
  published
    { Published Declarations }
    property Name: TGLLibMaterialName read FName write SetName;
    property Material: TGLMaterial read FMaterial write SetMaterial;
    property Tag: Integer read FTag write FTag;

    {: Texture offset in texture coordinates.<p>
       The offset is applied <i>after</i> scaling. }
    property TextureOffset: TGLCoordinates read FTextureOffset write
      SetTextureOffset;
    {: Texture coordinates scaling.<p>
       Scaling is applied <i>before</i> applying the offset, and is applied
       to the texture coordinates, meaning that a scale factor of (2, 2, 2)
       will make your texture look twice <i>smaller</i>. }
    property TextureScale: TGLCoordinates read FTextureScale write
      SetTextureScale;

    {: Reference to the second texture.<p>
       The referred LibMaterial *must* be in the same material library.<p>
       Second textures are supported only through ARB multitexturing (ignored
       if not supported). }
    property Texture2Name: TGLLibMaterialName read FTexture2Name write
      SetTexture2Name;

    {: Optionnal shader for the material. }
    property Shader: TGLShader read FShader write SetShader;
  end;

  // TGLLibMaterials
  //
    {: A collection of materials, mainly used in material libraries. }
  TGLLibMaterials = class(TOwnedCollection)
  private
    { Protected Declarations }

  protected
    { Protected Declarations }
    procedure Loaded;

    procedure SetItems(index: Integer; const val: TGLLibMaterial);
    function GetItems(index: Integer): TGLLibMaterial;
    procedure DestroyHandles;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);

    function Owner: TPersistent;

    function IndexOf(const Item: TGLLibMaterial): Integer;
    function Add: TGLLibMaterial;
    function FindItemID(ID: Integer): TGLLibMaterial;
    property Items[index: Integer]: TGLLibMaterial read GetItems write SetItems;
    default;
    function MakeUniqueName(const nameRoot: TGLLibMaterialName):
      TGLLibMaterialName;
    function GetLibMaterialByName(const AName: TGLLibMaterialName):
      TGLLibMaterial;

    {: Returns index of this Texture if it exists. }
    function GetTextureIndex(const Texture: TGLTexture): Integer;

    {: Returns index of this Material if it exists. }
    function GetMaterialIndex(const Material: TGLMaterial): Integer;

    {: Returns name of this Texture if it exists. }
    function GetNameOfTexture(const Texture: TGLTexture): TGLLibMaterialName;

    {: Returns name of this Material if it exists. }
    function GetNameOfLibMaterial(const Material: TGLLibMaterial):
      TGLLibMaterialName;

    procedure PrepareBuildList;
    procedure SetNamesToTStrings(aStrings: TStrings);
    {: Deletes all the unused materials in the collection.<p>
       A material is considered unused if no other material or updateable object references it.
       WARNING: For this to work, objects that use the textuere, have to REGISTER to the texture.}
    procedure DeleteUnusedMaterials;
  end;

  // TGLMaterialLibrary
  //
  {: Stores a set of materials, to be used and shared by scene objects.<p>
     Use a material libraries for storing commonly used materials, it provides
     an efficient way to share texture and material data among many objects,
     thus reducing memory needs and rendering time.<p>
     Materials in a material library also feature advanced control properties
     like texture coordinates transforms. }
  TGLMaterialLibrary = class(TGLCadenceAbleComponent)
  private
    { Private Declarations }
    FDoNotClearMaterialsOnLoad: Boolean;
    FMaterials: TGLLibMaterials;
    FTexturePaths: string;
    FOnTextureNeeded: TTextureNeededEvent;
    FTexturePathList: TStringList;
    FLastAppliedMaterial: TGLLibMaterial;

  protected
    { Protected Declarations }
    procedure Loaded; override;
    procedure SetMaterials(const val: TGLLibMaterials);
    function StoreMaterials: Boolean;
    procedure SetTexturePaths(const val: string);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyHandles;

    procedure WriteToFiler(writer: TVirtualWriter);
    procedure ReadFromFiler(reader: TVirtualReader);
    procedure SaveToStream(aStream: TStream); dynamic;
    procedure LoadFromStream(aStream: TStream); dynamic;
    procedure AddMaterialsFromStream(aStream: TStream);

    {: Save library content to a file.<p>
       Recommended extension : .GLML<br>
       Currently saves only texture, ambient, diffuse, emission
       and specular colors. }
    procedure SaveToFile(const fileName: string);
    procedure LoadFromFile(const fileName: string);
    procedure AddMaterialsFromFile(const fileName: string);

    {: Add a "standard" texture material.<p>
       "standard" means linear texturing mode with mipmaps and texture
       modulation mode with default-strength color components.<br>
       If persistent is True, the image will be loaded persistently in memory
       (via a TGLPersistentImage), if false, it will be unloaded after upload
       to OpenGL (via TGLPicFileImage). }
    function AddTextureMaterial(const materialName, fileName: string;
      persistent: Boolean = True): TGLLibMaterial; overload;
    {: Add a "standard" texture material.<p>
       TGLGraphic based variant. }
    function AddTextureMaterial(const materialName: string; graphic:
      TGLGraphic): TGLLibMaterial; overload;

    {: Returns libMaterial of given name if any exists. }
    function LibMaterialByName(const AName: TGLLibMaterialName): TGLLibMaterial;

    {: Returns Texture of given material's name if any exists. }
    function TextureByName(const LibMatName: TGLLibMaterialName): TGLTexture;

    {: Returns name of texture if any exists. }
    function GetNameOfTexture(const Texture: TGLTexture): TGLLibMaterialName;

    {: Returns name of Material if any exists. }
    function GetNameOfLibMaterial(const LibMat: TGLLibMaterial):
      TGLLibMaterialName;

    {: Applies the material of given name.<p>
       Returns False if the material could not be found. ake sure this
       call is balanced with a corresponding UnApplyMaterial (or an
       assertion will be triggered in the destructor).<br>
       If a material is already applied, and has not yet been unapplied,
       an assertion will be triggered. }
    function ApplyMaterial(const materialName: string; var rci:
      TRenderContextInfo): Boolean;
    {: Un-applies the last applied material.<p>
       Use this function in conjunction with ApplyMaterial.<br>
       If no material was applied, an assertion will be triggered. }
    function UnApplyMaterial(var rci: TRenderContextInfo): Boolean;

  published
    { Published Declarations }
      {: The materials collection. }
    property Materials: TGLLibMaterials read FMaterials write SetMaterials stored
      StoreMaterials;
    {: Paths to lookup when attempting to load a texture.<p>
       You can specify multiple paths when loading a texture, the separator
       being the semi-colon ';' character. Directories are looked up from
       first to last, the first file name match is used.<br>
       The current directory is always implicit and checked last.<p>
       Note that you can also use the OnTextureNeeded event to provide a
       filename. }
    property TexturePaths: string read FTexturePaths write SetTexturePaths;
    {: This event is fired whenever a texture needs to be loaded from disk.<p>
       The event is triggered before even attempting to load the texture,
       and before TexturePaths is used. }
    property OnTextureNeeded: TTextureNeededEvent read FOnTextureNeeded write
      FOnTextureNeeded;

  end;

implementation

uses SysUtils, GLStrings, XOpenGL, ApplicationFileIO, GLGraphics;

//const
//  cTGlAlphaFuncValues: array[TGlAlphaFunc] of TGLEnum =
//    (GL_NEVER, GL_LESS, GL_EQUAL, GL_LEQUAL, GL_GREATER, GL_NOTEQUAL, GL_GEQUAL,
//    GL_ALWAYS);
//
//  cTGLBlendFuncFactorValues: array[TGLBlendFuncFactor] of TGLEnum =
//    (GL_ZERO, GL_ONE, GL_DST_COLOR, GL_ONE_MINUS_DST_COLOR, GL_SRC_ALPHA,
//    GL_ONE_MINUS_SRC_ALPHA, GL_DST_ALPHA, GL_ONE_MINUS_DST_ALPHA,
//    GL_SRC_ALPHA_SATURATE, GL_CONSTANT_COLOR, GL_ONE_MINUS_CONSTANT_COLOR,
//    GL_CONSTANT_ALPHA, GL_ONE_MINUS_CONSTANT_ALPHA);

  // ------------------
  // ------------------ TGLFaceProperties ------------------
  // ------------------

  // Create
  //

constructor TGLFaceProperties.Create(aOwner: TPersistent);
begin
  inherited;
  // OpenGL default colors
  FAmbient := TGLColor.CreateInitialized(Self, clrGray20);
  FDiffuse := TGLColor.CreateInitialized(Self, clrGray80);
  FEmission := TGLColor.Create(Self);
  FSpecular := TGLColor.Create(Self);
  FShininess := 0;
end;

// Destroy
//

destructor TGLFaceProperties.Destroy;
begin
  FAmbient.Free;
  FDiffuse.Free;
  FEmission.Free;
  FSpecular.Free;
  inherited Destroy;
end;

// Apply
//

procedure TGLFaceProperties.Apply(var rci: TRenderContextInfo;
  aFace: TCullFaceMode);
begin
  rci.GLStates.SetGLMaterialColors(aFace,
    Emission.Color, Ambient.Color, Diffuse.Color, Specular.Color, FShininess);
  rci.GLStates.SetGLPolygonMode(aFace, FPolygonMode);
end;

// ApplyNoLighting
//

procedure TGLFaceProperties.ApplyNoLighting(var rci: TRenderContextInfo;
  aFace: TCullFaceMode);
begin
  glColor4fv(@Diffuse.Color);
  rci.GLStates.SetGLPolygonMode(aFace, FPolygonMode);
end;

// Assign
//

procedure TGLFaceProperties.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLFaceProperties) then
  begin
    FAmbient.DirectColor := TGLFaceProperties(Source).Ambient.Color;
    FDiffuse.DirectColor := TGLFaceProperties(Source).Diffuse.Color;
    FEmission.DirectColor := TGLFaceProperties(Source).Emission.Color;
    FSpecular.DirectColor := TGLFaceProperties(Source).Specular.Color;
    FShininess := TGLFaceProperties(Source).Shininess;
    FPolygonMode := TGLFaceProperties(Source).PolygonMode;
    NotifyChange(Self);
  end;
end;

// SetAmbient
//

procedure TGLFaceProperties.SetAmbient(AValue: TGLColor);
begin
  FAmbient.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

// SetDiffuse
//

procedure TGLFaceProperties.SetDiffuse(AValue: TGLColor);
begin
  FDiffuse.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

// SetEmission
//

procedure TGLFaceProperties.SetEmission(AValue: TGLColor);
begin
  FEmission.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

// SetSpecular
//

procedure TGLFaceProperties.SetSpecular(AValue: TGLColor);
begin
  FSpecular.DirectColor := AValue.Color;
  NotifyChange(Self);
end;

// SetPolygonMode
//

procedure TGLFaceProperties.SetPolygonMode(AValue: TPolygonMode);
begin
  if AValue <> FPolygonMode then
  begin
    FPolygonMode := AValue;
    NotifyChange(Self);
  end;
end;

// SetShininess
//

procedure TGLFaceProperties.SetShininess(AValue: TShininess);
begin
  if FShininess <> AValue then
  begin
    FShininess := AValue;
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TGLDepthProperties ------------------
// ------------------

constructor TGLDepthProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FDepthTest := True;
  FDepthWrite := True;
  FZNear := 0;
  FZFar := 1;
  FCompareFunc := cfLequal;
  FDepthClamp := False;
end;

procedure TGLDepthProperties.Apply(var rci: TRenderContextInfo);
begin
  if FDepthTest and rci.bufferDepthTest then
    rci.GLStates.Enable(stDepthTest)
  else
    rci.GLStates.Disable(stDepthTest);
  rci.GLStates.DepthWriteMask := FDepthWrite;
  rci.GLStates.DepthFunc := FCompareFunc;
  rci.GLStates.SetDepthRange(FZNear, FZFar);
  if GL_ARB_depth_clamp then
    if FDepthClamp then
      rci.GLStates.Enable(stDepthClamp)
    else
      rci.GLStates.Disable(stDepthClamp);
end;

procedure TGLDepthProperties.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLDepthProperties) then
  begin
    FDepthTest := TGLDepthProperties(Source).FDepthTest;
    FDepthWrite := TGLDepthProperties(Source).FDepthWrite;
    FZNear := TGLDepthProperties(Source).FZNear;
    FZFar := TGLDepthProperties(Source).FZFar;
    FCompareFunc := TGLDepthProperties(Source).FCompareFunc;
    NotifyChange(Self);
  end;
end;

procedure TGLDepthProperties.SetZNear(Value: Single);
begin
  Value := ClampValue(Value, 0, 1);
  if Value <> FZNear then
  begin
    FZNear := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLDepthProperties.SetZFar(Value: Single);
begin
  Value := ClampValue(Value, 0, 1);
  if Value <> FZFar then
  begin
    FZFar := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLDepthProperties.SetCompareFunc(Value: TDepthFunction);
begin
  if Value <> FCompareFunc then
  begin
    FCompareFunc := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLDepthProperties.SetDepthTest(Value: boolean);
begin
  if Value <> FDepthTest then
  begin
    FDepthTest := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLDepthProperties.SetDepthWrite(Value: boolean);
begin
  if Value <> FDepthWrite then
  begin
    FDepthWrite := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLDepthProperties.SetDepthClamp(Value: boolean);
begin
  if Value <> FDepthClamp then
  begin
    FDepthClamp := Value;
    NotifyChange(Self);
  end;
end;

function TGLDepthProperties.StoreZNear: Boolean;
begin
  Result := FZNear <> 0.0;
end;

function TGLDepthProperties.StoreZFar: Boolean;
begin
  Result := FZFar <> 1.0;
end;

// ------------------
// ------------------ TGLShader ------------------
// ------------------

// Create
//

constructor TGLShader.Create(AOwner: TComponent);
begin
  FLibMatUsers := TList.Create;
  FVirtualHandle := TGLVirtualHandle.Create;
  FShaderStyle := ssLowLevel;
  FEnabled := True;
  FFailedInitAction := fiaRaiseStandardException;
  inherited;
end;

// Destroy
//

destructor TGLShader.Destroy;
var
  i: Integer;
  list: TList;
begin
  FVirtualHandle.DestroyHandle;
  FinalizeShader;
  inherited;
  list := FLibMatUsers;
  FLibMatUsers := nil;
  for i := list.Count - 1 downto 0 do
    TGLLibMaterial(list[i]).Shader := nil;
  list.Free;
  FVirtualHandle.Free;
end;

// NotifyChange
//

procedure TGLShader.NotifyChange(Sender: TObject);
var
  i: Integer;
begin
  if FUpdateCount = 0 then
  begin
    for i := FLibMatUsers.Count - 1 downto 0 do
      TGLLibMaterial(FLibMatUsers[i]).NotifyUsers;
    FinalizeShader;
  end;
end;

// BeginUpdate
//

procedure TGLShader.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// EndUpdate
//

procedure TGLShader.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    NotifyChange(Self);
end;

// DoInitialize
//

procedure TGLShader.DoInitialize(var rci: TRenderContextInfo; Sender: TObject);
begin
  // nothing here
end;

// DoFinalize
//

procedure TGLShader.DoFinalize;
begin
  // nothing here
end;

// GetShaderInitialized
//

function TGLShader.GetShaderInitialized: Boolean;
begin
  Result := (FVirtualHandle.Handle <> 0);
end;

// InitializeShader
//

procedure TGLShader.InitializeShader(var rci: TRenderContextInfo; Sender:
  TObject);
begin
  if FVirtualHandle.Handle = 0 then
  begin
    FVirtualHandle.OnAllocate := OnVirtualHandleAllocate;
    FVirtualHandle.OnDestroy := OnVirtualHandleDestroy;
    FVirtualHandle.AllocateHandle;
    FShaderInitialized := True;
    DoInitialize(rci, Sender);
  end;
end;

// FinalizeShader
//

procedure TGLShader.FinalizeShader;
var
  activateContext: Boolean;
begin
  if FVirtualHandle.Handle <> 0 then
  begin
    if FShaderInitialized then
    begin
      activateContext := (not FVirtualHandle.RenderingContext.Active);
      if activateContext then
        FVirtualHandle.RenderingContext.Activate;
      try
        FShaderInitialized := False;
        DoFinalize;
      finally
        if activateContext then
          FVirtualHandle.RenderingContext.Deactivate;
      end;
      FVirtualHandle.DestroyHandle;
    end;
  end;
end;

// Apply
//

procedure TGLShader.Apply(var rci: TRenderContextInfo; Sender: TObject);
begin
{$IFNDEF GLS_MULTITHREAD}
  Assert(not FShaderActive, 'Unbalanced shader application.');
{$ENDIF}
  // Need to check it twice, because shader may refuse to initialize
  // and choose to disable itself during initialization.
  if FEnabled then
    if FVirtualHandle.Handle = 0 then
      InitializeShader(rci, Sender);

  if FEnabled then
    DoApply(rci, Sender);

  FShaderActive := True;
end;

// UnApply
//

function TGLShader.UnApply(var rci: TRenderContextInfo): Boolean;
begin
{$IFNDEF GLS_MULTITHREAD}
  Assert(FShaderActive, 'Unbalanced shader application.');
{$ENDIF}
  if Enabled then
  begin
    Result := DoUnApply(rci);
    if not Result then
      FShaderActive := False;
  end
  else
  begin
    FShaderActive := False;
    Result := False;
  end;
end;

// OnVirtualHandleDestroy
//

procedure TGLShader.OnVirtualHandleDestroy(sender: TGLVirtualHandle; var handle:
  Cardinal);
begin
  FinalizeShader;
  handle := 0;
end;

// OnVirtualHandleAllocate
//

procedure TGLShader.OnVirtualHandleAllocate(sender: TGLVirtualHandle; var
  handle: Cardinal);
begin
  handle := 1;
end;

// SetEnabled
//

procedure TGLShader.SetEnabled(val: Boolean);
begin
{$IFNDEF GLS_MULTITHREAD}
  Assert(not FShaderActive, 'Shader is active.');
{$ENDIF}
  if val <> FEnabled then
  begin
    FEnabled := val;
    NotifyChange(Self);
  end;
end;

// RegisterUser
//

procedure TGLShader.RegisterUser(libMat: TGLLibMaterial);
var
  i: Integer;
begin
  i := FLibMatUsers.IndexOf(libMat);
  if i < 0 then
    FLibMatUsers.Add(libMat);
end;

// UnRegisterUser
//

procedure TGLShader.UnRegisterUser(libMat: TGLLibMaterial);
begin
  if Assigned(FLibMatUsers) then
    FLibMatUsers.Remove(libMat);
end;

// Assign
//

procedure TGLShader.Assign(Source: TPersistent);
begin
  if Source is TGLShader then
  begin
    FShaderStyle := TGLShader(Source).FShaderStyle;
    FFailedInitAction := TGLShader(Source).FFailedInitAction;
    Enabled := TGLShader(Source).FEnabled;
  end
  else
    inherited Assign(Source); //to the pit of doom ;)
end;

// Assign
//

function TGLShader.ShaderSupported: Boolean;
begin
  Result := True;
end;

// HandleFailedInitialization
//

procedure TGLShader.HandleFailedInitialization(const LastErrorMessage: string =
  '');
begin
  case FailedInitAction of
    fiaSilentdisable: ; // Do nothing ;)
    fiaRaiseHandledException:
      try
        raise EGLShaderException.Create(GetStardardNotSupportedMessage);
      except
      end;
    fiaRaiseStandardException:
      raise EGLShaderException.Create(GetStardardNotSupportedMessage);
    fiaReRaiseException:
      begin
        if LastErrorMessage <> '' then
          raise EGLShaderException.Create(LastErrorMessage)
        else
          raise EGLShaderException.Create(GetStardardNotSupportedMessage)
      end;
    //    fiaGenerateEvent:; // Do nothing. Event creation is left up to user shaders
    //                       // which may choose to override this procedure.
  else
    Assert(False, glsErrorEx + glsUnknownType);
  end;
end;

// GetStardardNotSupportedMessage
//

function TGLShader.GetStardardNotSupportedMessage: string;
begin
  if Name <> '' then
    Result := 'Your hardware/driver doesn''t support shader "' + Name + '"!'
  else
    Result := 'Your hardware/driver doesn''t support shader "' + ClassName +
      '"!';
end;

//----------------- TGLMaterial --------------------------------------------------

// Create
//

constructor TGLMaterial.Create(AOwner: TPersistent);
begin
  inherited;
  FFrontProperties := TGLFaceProperties.Create(Self);
  FTexture := nil; // AutoCreate
  FFaceCulling := fcBufferDefault;
  FBlendingParams := TGLBlendingParameters.Create(Self);
  FDepthProperties := TGLDepthProperties.Create(Self)
end;

// Destroy
//

destructor TGLMaterial.Destroy;
begin
  if Assigned(currentLibMaterial) then
    currentLibMaterial.UnregisterUser(Self);
  FGLBackProperties.Free;
  FFrontProperties.Free;
  FDepthProperties.Free;
  FTexture.Free;
  FTextureEx.Free;
  FBlendingParams.Free;
  inherited Destroy;
end;

// GetMaterialLibrary
//

function TGLMaterial.GetMaterialLibrary: TGLMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

// SetBackProperties
//

procedure TGLMaterial.SetBackProperties(Values: TGLFaceProperties);
begin
  BackProperties.Assign(Values);
  NotifyChange(Self);
end;

// GetBackProperties
//

function TGLMaterial.GetBackProperties: TGLFaceProperties;
begin
  if not Assigned(FGLBackProperties) then
    FGLBackProperties := TGLFaceProperties.Create(Self);
  Result := FGLBackProperties;
end;

// SetFrontProperties
//

procedure TGLMaterial.SetFrontProperties(Values: TGLFaceProperties);
begin
  FFrontProperties.Assign(Values);
  NotifyChange(Self);
end;

// TGLMaterial
//

procedure TGLMaterial.SetDepthProperties(Values: TGLDepthProperties);
begin
  FDepthProperties.Assign(Values);
  NotifyChange(Self);
end;

// SetBlendingMode
//

procedure TGLMaterial.SetBlendingMode(const val: TBlendingMode);
begin
  if val <> FBlendingMode then
  begin
    FBlendingMode := val;
    NotifyChange(Self);
  end;
end;

// SetMaterialOptions
//

procedure TGLMaterial.SetMaterialOptions(const val: TMaterialOptions);
begin
  if val <> FMaterialOptions then
  begin
    FMaterialOptions := val;
    NotifyChange(Self);
  end;
end;

// GetTexture
//

function TGLMaterial.GetTexture: TGLTexture;
begin
  if not Assigned(FTexture) then
    FTexture := TGLTexture.Create(Self);
  Result := FTexture;
end;

// SetTexture
//

procedure TGLMaterial.SetTexture(aTexture: TGLTexture);
begin
  if Assigned(aTexture) then
    Texture.Assign(ATexture)
  else
    FreeAndNil(FTexture);
end;

// SetFaceCulling
//

procedure TGLMaterial.SetFaceCulling(const val: TFaceCulling);
begin
  if val <> FFaceCulling then
  begin
    FFaceCulling := val;
    NotifyChange(Self);
  end;
end;

// SetMaterialLibrary
//

procedure TGLMaterial.SetMaterialLibrary(const val: TGLMaterialLibrary);
begin
  FMaterialLibrary := val;
  SetLibMaterialName(LibMaterialName);
end;

// SetLibMaterialName
//

procedure TGLMaterial.SetLibMaterialName(const val: TGLLibMaterialName);

  function MaterialLoopFrom(curMat: TGLLibMaterial): Boolean;
  var
    loopCount: Integer;
  begin
    loopCount := 0;
    while Assigned(curMat) and (loopCount < 16) do
    begin
      with curMat.Material do
      begin
        if MaterialLibrary <> nil then
          curMat :=
            MaterialLibrary.Materials.GetLibMaterialByName(LibMaterialName)
        else
          curMat := nil;
      end;
      Inc(loopCount)
    end;
    Result := (loopCount >= 16);
  end;

var
  newLibMaterial: TGLLibMaterial;
begin
  // locate new libmaterial
  if Assigned(FMaterialLibrary) then
    newLibMaterial := MaterialLibrary.Materials.GetLibMaterialByName(val)
  else
    newLibMaterial := nil;
  // make sure new won't trigger an infinite loop
  Assert(not MaterialLoopFrom(newLibMaterial),
    'Error: Cyclic material reference detected!');
  FLibMaterialName := val;
  // unregister if required
  if newLibMaterial <> currentLibMaterial then
  begin
    // unregister from old
    if Assigned(currentLibMaterial) then
      currentLibMaterial.UnregisterUser(Self);
    currentLibMaterial := newLibMaterial;
    // register with new
    if Assigned(currentLibMaterial) then
      currentLibMaterial.RegisterUser(Self);
    NotifyTexMapChange(Self);
  end;
end;

// GetTextureEx
//

function TGLMaterial.GetTextureEx: TGLTextureEx;
begin
  if not Assigned(FTextureEx) then
    FTextureEx := TGLTextureEx.Create(Self);
  Result := FTextureEx;
end;

// SetTextureEx
//

procedure TGLMaterial.SetTextureEx(const Value: TGLTextureEx);
begin
  if Assigned(Value) or Assigned(FTextureEx) then
    TextureEx.Assign(Value);
end;

// StoreTextureEx
//

function TGLMaterial.StoreTextureEx: Boolean;
begin
  Result := (Assigned(FTextureEx) and (TextureEx.Count > 0));
end;

// SetBlendingParams
//

procedure TGLMaterial.SetBlendingParams(const Value: TGLBlendingParameters);
begin
  FBlendingParams.Assign(Value);
end;

// NotifyLibMaterialDestruction
//

procedure TGLMaterial.NotifyLibMaterialDestruction;
begin
  FMaterialLibrary := nil;
  FLibMaterialName := '';
  currentLibMaterial := nil;
end;

// Loaded
//

procedure TGLMaterial.Loaded;
begin
  inherited;
  if Assigned(FTextureEx) then
    TextureEx.Loaded;
end;

// StoreMaterialProps
//

function TGLMaterial.StoreMaterialProps: Boolean;
begin
  Result := not Assigned(currentLibMaterial);
end;

// PrepareBuildList
//

procedure TGLMaterial.PrepareBuildList;
begin
  if Assigned(FTexture) and (not FTexture.Disabled) then
    FTexture.PrepareBuildList;
end;

// Apply
//

procedure TGLMaterial.Apply(var rci: TRenderContextInfo);
begin
  if Assigned(currentLibMaterial) then
    currentLibMaterial.Apply(rci)
  else
  begin
    // Lighting switch
    if (moNoLighting in MaterialOptions) or not rci.bufferLighting then
    begin
      rci.GLStates.Disable(stLighting);
      FFrontProperties.ApplyNoLighting(rci, cmFront);
    end
    else
    begin
      rci.GLStates.Enable(stLighting);
      FFrontProperties.Apply(rci, cmFront);
    end;

    // Apply FaceCulling and BackProperties (if needs be)
    case FFaceCulling of
      fcBufferDefault:
        begin
          if rci.bufferFaceCull then
            rci.GLStates.Enable(stCullFace)
          else
            rci.GLStates.Disable(stCullFace);
          BackProperties.Apply(rci, cmBack);
        end;
      fcCull: rci.GLStates.Enable(stCullFace);
      fcNoCull:
        begin
          rci.GLStates.Disable(stCullFace);
          BackProperties.Apply(rci, cmBack);
        end;
    end;
    // note: Front + Back with different PolygonMode are no longer supported.
    // Currently state cache just ignores back facing mode changes, changes to
    // front affect both front + back PolygonMode

    // Apply Blending mode
    if not rci.ignoreBlendingRequests then
      case FBlendingMode of
        bmOpaque:
          begin
            rci.GLStates.Disable(stBlend);
            rci.GLStates.Disable(stAlphaTest);
          end;
        bmTransparency:
          begin
            rci.GLStates.Enable(stBlend);
            rci.GLStates.Enable(stAlphaTest);
            rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
            rci.GLStates.SetGLAlphaFunction(cfGreater, 0);
          end;
        bmAdditive:
          begin
            rci.GLStates.Enable(stBlend);
            rci.GLStates.Enable(stAlphaTest);
            rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOne);
            rci.GLStates.SetGLAlphaFunction(cfGreater, 0);
          end;
        bmAlphaTest50:
          begin
            rci.GLStates.Disable(stBlend);
            rci.GLStates.Enable(stAlphaTest);
            rci.GLStates.SetGLAlphaFunction(cfGEqual, 0.5);
          end;
        bmAlphaTest100:
          begin
            rci.GLStates.Disable(stBlend);
            rci.GLStates.Enable(stAlphaTest);
            rci.GLStates.SetGLAlphaFunction(cfGEqual, 1.0);
          end;
        bmModulate:
          begin
            rci.GLStates.Enable(stBlend);
            rci.GLStates.Enable(stAlphaTest);
            rci.GLStates.SetBlendFunc(bfDstColor, bfZero);
            rci.GLStates.SetGLAlphaFunction(cfGreater, 0);
          end;
        bmCustom:
          begin
            FBlendingParams.Apply(rci);
          end;
      end;

    // Fog switch
    if (moIgnoreFog in MaterialOptions) or not rci.bufferFog then
      rci.GLStates.Disable(stFog)
    else
      rci.GLStates.Enable(stFog);

    if not Assigned(FTextureEx) then
    begin
      if Assigned(FTexture) then
        FTexture.Apply(rci)
    end
    else
    begin
      if Assigned(FTexture) and not FTextureEx.IsTextureEnabled(0) then
        FTexture.Apply(rci)
      else if FTextureEx.Count > 0 then
        FTextureEx.Apply(rci);
    end;

    // Apply depth properties
    if not rci.ignoreDepthRequests then
      FDepthProperties.Apply(rci);
  end;
end;

// UnApply
//

function TGLMaterial.UnApply(var rci: TRenderContextInfo): Boolean;
begin
  if Assigned(currentLibMaterial) then
    Result := currentLibMaterial.UnApply(rci)
  else
  begin
    if Assigned(FTexture) and (not FTexture.Disabled) and (not
      FTextureEx.IsTextureEnabled(0)) then
      FTexture.UnApply(rci)
    else if Assigned(FTextureEx) then
      FTextureEx.UnApply(rci);
    Result := False;
  end;
end;

// Assign
//

procedure TGLMaterial.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLMaterial) then
  begin
    if Assigned(TGLMaterial(Source).FGLBackProperties) then
      BackProperties.Assign(TGLMaterial(Source).BackProperties)
    else
      FreeAndNil(FGLBackProperties);
    FFrontProperties.Assign(TGLMaterial(Source).FFrontProperties);
    FBlendingMode := TGLMaterial(Source).FBlendingMode;
    FMaterialOptions := TGLMaterial(Source).FMaterialOptions;
    if Assigned(TGLMaterial(Source).FTexture) then
      Texture.Assign(TGLMaterial(Source).FTexture)
    else
      FreeAndNil(FTexture);
    FFaceCulling := TGLMaterial(Source).FFaceCulling;
    FMaterialLibrary := TGLMaterial(Source).MaterialLibrary;
    SetLibMaterialName(TGLMaterial(Source).LibMaterialName);
    TextureEx.Assign(TGLMaterial(Source).TextureEx);
    FDepthProperties.Assign(TGLMaterial(Source).DepthProperties);
    NotifyChange(Self);
  end
  else
    inherited;
end;

// NotifyChange
//

procedure TGLMaterial.NotifyChange(Sender: TObject);
var
  intf: IGLNotifyAble;
begin
  if Supports(Owner, IGLNotifyAble, intf) then
    intf.NotifyChange(Self);
end;

// NotifyTexMapChange
//

procedure TGLMaterial.NotifyTexMapChange(Sender: TObject);
var
  intf: IGLTextureNotifyAble;
begin
  if Supports(Owner, IGLTextureNotifyAble, intf) then
    intf.NotifyTexMapChange(Self)
  else
    NotifyChange(Self);
end;

// DestroyHandles
//

procedure TGLMaterial.DestroyHandles;
begin
  if Assigned(FTexture) then
    FTexture.DestroyHandles;
end;

// Blended
//

function TGLMaterial.Blended: Boolean;
begin
  if Assigned(currentLibMaterial) then
    Result := currentLibMaterial.Material.Blended
  else
    Result := not (BlendingMode in [bmOpaque, bmAlphaTest50, bmAlphaTest100]);
end;

// HasSecondaryTexture
//

function TGLMaterial.HasSecondaryTexture: Boolean;
begin
  Result := Assigned(currentLibMaterial) and
    Assigned(currentLibMaterial.libMatTexture2);
end;

// MaterialIsLinkedToLib
//

function TGLMaterial.MaterialIsLinkedToLib: Boolean;
begin
  Result := Assigned(currentLibMaterial);
end;

// GetActualPrimaryTexture
//

function TGLMaterial.GetActualPrimaryTexture: TGLTexture;
begin
  if Assigned(currentLibMaterial) then
    Result := currentLibMaterial.Material.Texture
  else
    Result := Texture;
end;

// GetActualPrimaryTexture
//

function TGLMaterial.GetActualPrimaryMaterial: TGLMaterial;
begin
  if Assigned(currentLibMaterial) then
    Result := currentLibMaterial.Material
  else
    Result := Self;
end;

// QuickAssignMaterial
//

function TGLMaterial.GetLibMaterial: TGLLibMaterial;
begin
  Result := currentLibMaterial;
end;

// QuickAssignMaterial
//

procedure TGLMaterial.QuickAssignMaterial(const MaterialLibrary:
  TGLMaterialLibrary; const Material: TGLLibMaterial);
begin
  FMaterialLibrary := MaterialLibrary;
  FLibMaterialName := Material.FName;

  if Material <> CurrentLibMaterial then
  begin
    // unregister from old
    if Assigned(CurrentLibMaterial) then
      currentLibMaterial.UnregisterUser(Self);
    CurrentLibMaterial := Material;
    // register with new
    if Assigned(CurrentLibMaterial) then
      CurrentLibMaterial.RegisterUser(Self);

    NotifyTexMapChange(Self);
  end;
end;

// ------------------
// ------------------ TGLLibMaterial ------------------
// ------------------

// Create
//

constructor TGLLibMaterial.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  userList := TList.Create;
  FName := TGLLibMaterials(ACollection).MakeUniqueName('LibMaterial');
  FNameHashKey := ComputeNameHashKey(FName);
  FMaterial := TGLMaterial.Create(Self);
  FMaterial.Texture.OnTextureNeeded := DoOnTextureNeeded;
  FTextureOffset := TGLCoordinates.CreateInitialized(Self, NullHmgVector,
    csPoint);
  FTextureOffset.OnNotifyChange := OnNotifyChange;
  FTextureScale := TGLCoordinates.CreateInitialized(Self, XYZHmgVector,
    csPoint);
  FTextureScale.OnNotifyChange := OnNotifyChange;
  FTextureMatrixIsIdentity := True;
end;

// Destroy
//

destructor TGLLibMaterial.Destroy;
var
  i: Integer;
  matObj: TObject;
begin
  Shader := nil; // drop dependency
  Texture2Name := ''; // drop dependency
  for i := 0 to userList.Count - 1 do
  begin
    matObj := TObject(userList[i]);
    if matObj is TGLMaterial then
      TGLMaterial(matObj).NotifyLibMaterialDestruction
    else if matObj is TGLLibMaterial then
    begin
      TGLLibMaterial(matObj).libMatTexture2 := nil;
      TGLLibMaterial(matObj).FTexture2Name := '';
    end;
  end;
  userList.Free;
  FMaterial.Free;
  FTextureOffset.Free;
  FTextureScale.Free;
  inherited Destroy;
end;

// GetMaterialLibrary
//

function TGLLibMaterial.GetMaterialLibrary: TGLMaterialLibrary;
begin
  if Collection = nil then
    Result := nil
  else
    Result := TGLMaterialLibrary(TGLLibMaterials(Collection).Owner);
end;

// QueryInterface
//

function TGLLibMaterial.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

// _AddRef
//

function TGLLibMaterial._AddRef: Integer;
begin
  Result := -1; //ignore
end;

// _Release
//

function TGLLibMaterial._Release: Integer;
begin
  Result := -1; //ignore
end;

// Assign
//

procedure TGLLibMaterial.Assign(Source: TPersistent);
begin
  if Source is TGLLibMaterial then
  begin
    FName :=
      TGLLibMaterials(Collection).MakeUniqueName(TGLLibMaterial(Source).Name);
    FNameHashKey := ComputeNameHashKey(FName);
    FMaterial.Assign(TGLLibMaterial(Source).Material);
    FTextureOffset.Assign(TGLLibMaterial(Source).TextureOffset);
    FTextureScale.Assign(TGLLibMaterial(Source).TextureScale);
    FTexture2Name := TGLLibMaterial(Source).Texture2Name;
    FShader := TGLLibMaterial(Source).Shader;
    CalculateTextureMatrix;
  end
  else
    inherited;
end;

// PrepareBuildList
//

procedure TGLLibMaterial.PrepareBuildList;
begin
  if Assigned(Self) then
    Material.PrepareBuildList;
end;

// Apply
//

procedure TGLLibMaterial.Apply(var rci: TRenderContextInfo);
var
  multitextured: Boolean;
begin
  xglBeginUpdate;
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssHighLevel: Shader.Apply(rci, Self);
      ssReplace:
        begin
          Shader.Apply(rci, Self);
          Exit;
        end;
    end;
  end;
  if (Texture2Name <> '') and GL_ARB_multitexture and (not
    vSecondTextureUnitForbidden) then
  begin
    if not Assigned(libMatTexture2) then
    begin
      libMatTexture2 :=
        TGLLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
      if Assigned(libMatTexture2) then
        libMatTexture2.RegisterUser(Self)
      else
        FTexture2Name := '';
    end;
    multitextured := Assigned(libMatTexture2)
      and (not libMatTexture2.Material.Texture.Disabled);
  end
  else
    multitextured := False;
  if not multitextured then
  begin
    // no multitexturing ("standard" mode)
    if not Material.Texture.Disabled then
      if not FTextureMatrixIsIdentity then
        rci.GLStates.SetGLTextureMatrix(FTextureMatrix);
    Material.Apply(rci);
  end
  else
  begin
    // multitexturing is ON
    if not FTextureMatrixIsIdentity then
      rci.GLStates.SetGLTextureMatrix(FTextureMatrix);
    Material.Apply(rci);

    if not libMatTexture2.FTextureMatrixIsIdentity then
      libMatTexture2.Material.Texture.ApplyAsTexture2(rci,
        @libMatTexture2.FTextureMatrix[0][0])
    else
      libMatTexture2.Material.Texture.ApplyAsTexture2(rci);

    if (not Material.Texture.Disabled) and (Material.Texture.MappingMode =
      tmmUser) then
      if libMatTexture2.Material.Texture.MappingMode = tmmUser then
        xglMapTexCoordToDual
      else
        xglMapTexCoordToMain
    else if libMatTexture2.Material.Texture.MappingMode = tmmUser then
      xglMapTexCoordToSecond
    else
      xglMapTexCoordToMain;

  end;
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssLowLevel: Shader.Apply(rci, Self);
    end;
  end;
  xglEndUpdate;
end;

// UnApply
//

function TGLLibMaterial.UnApply(var rci: TRenderContextInfo): Boolean;
begin
  Result := False;
  if Assigned(FShader) then
  begin
    case Shader.ShaderStyle of
      ssLowLevel: Result := Shader.UnApply(rci);
      ssReplace:
        begin
          Result := Shader.UnApply(rci);
          Exit;
        end;
    end;
  end;
  if not Result then
  begin
    // if multipassing, this will occur upon last pass only
{      if Assigned(Material.FTextureEx) then begin
       if not Material.TextureEx.IsTextureEnabled(1) then begin}
    if Assigned(libMatTexture2) and GL_ARB_multitexture and (not
      vSecondTextureUnitForbidden) then
    begin
      libMatTexture2.Material.Texture.UnApplyAsTexture2(rci, (not
        libMatTexture2.TextureMatrixIsIdentity));
      xglMapTexCoordToMain;
    end;
    {         end;
          end; }
    Material.UnApply(rci);
    if not Material.Texture.Disabled then
      if not FTextureMatrixIsIdentity then
        rci.GLStates.ResetGLTextureMatrix;
    if Assigned(FShader) then
    begin
      case Shader.ShaderStyle of
        ssHighLevel: Result := Shader.UnApply(rci);
      end;
    end;
  end;
end;

// RegisterUser
//

procedure TGLLibMaterial.RegisterUser(obj: TGLUpdateAbleObject);
begin
  Assert(userList.IndexOf(obj) < 0);
  userList.Add(obj);
end;

// UnregisterUser
//

procedure TGLLibMaterial.UnRegisterUser(obj: TGLUpdateAbleObject);
begin
  userList.Remove(obj);
end;

// RegisterUser
//

procedure TGLLibMaterial.RegisterUser(comp: TGLUpdateAbleComponent);
begin
  Assert(userList.IndexOf(comp) < 0);
  userList.Add(comp);
end;

// UnregisterUser
//

procedure TGLLibMaterial.UnRegisterUser(comp: TGLUpdateAbleComponent);
begin
  userList.Remove(comp);
end;

// RegisterUser
//

procedure TGLLibMaterial.RegisterUser(libMaterial: TGLLibMaterial);
begin
  Assert(userList.IndexOf(libMaterial) < 0);
  userList.Add(libMaterial);
end;

// UnregisterUser
//

procedure TGLLibMaterial.UnRegisterUser(libMaterial: TGLLibMaterial);
begin
  userList.Remove(libMaterial);
end;

// NotifyUsers
//

procedure TGLLibMaterial.NotifyChange(Sender: TObject);
begin
  NotifyUsers();
end;

procedure TGLLibMaterial.NotifyTexMapChange(Sender: TObject);
begin
  NotifyUsersOfTexMapChange();
end;

procedure TGLLibMaterial.NotifyUsers;
var
  i: Integer;
  obj: TObject;
begin
  if notifying then
    Exit;
  notifying := True;
  try
    for i := 0 to userList.Count - 1 do
    begin
      obj := TObject(userList[i]);
      if obj is TGLUpdateAbleObject then
        TGLUpdateAbleObject(userList[i]).NotifyChange(Self)
      else if obj is TGLUpdateAbleComponent then
        TGLUpdateAbleComponent(userList[i]).NotifyChange(Self)
      else
      begin
        Assert(obj is TGLLibMaterial);
        TGLLibMaterial(userList[i]).NotifyUsers;
      end;
    end;
  finally
    notifying := False;
  end;
end;

// NotifyUsersOfTexMapChange
//

procedure TGLLibMaterial.NotifyUsersOfTexMapChange;
var
  i: Integer;
  obj: TObject;
begin
  if notifying then
    Exit;
  notifying := True;
  try
    for i := 0 to userList.Count - 1 do
    begin
      obj := TObject(userList[i]);
      if obj is TGLMaterial then
        TGLMaterial(userList[i]).NotifyTexMapChange(Self)
      else if obj is TGLLibMaterial then
        TGLLibMaterial(userList[i]).NotifyUsersOfTexMapChange
      else if obj is TGLUpdateAbleObject then
        TGLUpdateAbleObject(userList[i]).NotifyChange(Self)
      else if obj is TGLUpdateAbleComponent then
        TGLUpdateAbleComponent(userList[i]).NotifyChange(Self);
    end;
  finally
    notifying := False;
  end;
end;

// IsUsed
//

function TGLLibMaterial.IsUsed: boolean;
begin
  result := Assigned(self) and (self.userlist.count > 0);
end;
// GetDisplayName
//

function TGLLibMaterial.GetDisplayName: string;
begin
  Result := Name;
end;

// Loaded
//

procedure TGLLibMaterial.Loaded;
begin
  CalculateTextureMatrix;
  Material.Loaded;
end;

// ComputeNameHashKey
//

class function TGLLibMaterial.ComputeNameHashKey(const name: string): Integer;
var
  i, n: Integer;
begin
  n := Length(name);
  Result := n;
  for i := 1 to n do
    Result := (Result shl 1) + Byte(name[i]);
end;

// SetName
//

procedure TGLLibMaterial.SetName(const val: TGLLibMaterialName);
begin
  if val <> FName then
  begin
    if not (csLoading in
      TComponent(TGLLibMaterials(Collection).GetOwner).ComponentState) then
    begin
      if TGLLibMaterials(Collection).GetLibMaterialByName(val) <> Self then
        FName := TGLLibMaterials(Collection).MakeUniqueName(val)
      else
        FName := val;
    end
    else
      FName := val;
    FNameHashKey := ComputeNameHashKey(FName);
  end;
end;

// SetMaterial
//

procedure TGLLibMaterial.SetMaterial(const val: TGLMaterial);
begin
  FMaterial.Assign(val);
end;

// SetTextureOffset
//

procedure TGLLibMaterial.SetTextureOffset(const val: TGLCoordinates);
begin
  FTextureOffset.AsVector := val.AsVector;
  CalculateTextureMatrix;
end;

// SetTextureScale
//

procedure TGLLibMaterial.SetTextureScale(const val: TGLCoordinates);
begin
  FTextureScale.AsVector := val.AsVector;
  CalculateTextureMatrix;
end;

// SetTexture2
//

procedure TGLLibMaterial.SetTexture2Name(const val: TGLLibMaterialName);
begin
  if val <> Texture2Name then
  begin
    if Assigned(libMatTexture2) then
    begin
      libMatTexture2.UnregisterUser(Self);
      libMatTexture2 := nil;
    end;
    FTexture2Name := val;
    NotifyUsers;
  end;
end;

// SetShader
//

procedure TGLLibMaterial.SetShader(const val: TGLShader);
begin
  if val <> FShader then
  begin
    if Assigned(FShader) then
      FShader.UnRegisterUser(Self);
    FShader := val;
    if Assigned(FShader) then
      FShader.RegisterUser(Self);
    NotifyUsers;
  end;
end;

// CalculateTextureMatrix
//

procedure TGLLibMaterial.CalculateTextureMatrix;
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
  NotifyUsers;
end;

// DestroyHandles
//

procedure TGLLibMaterial.DestroyHandles;
var
  libMat: TGLLibMaterial;
begin
  FMaterial.DestroyHandles;
  if FTexture2Name <> '' then
  begin
    libMat := TGLLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
    if Assigned(libMat) then
      libMat.DestroyHandles;
  end;
end;

// OnNotifyChange
//

procedure TGLLibMaterial.OnNotifyChange(Sender: TObject);
begin
  CalculateTextureMatrix;
end;

// DoOnTextureNeeded
//

procedure TGLLibMaterial.DoOnTextureNeeded(Sender: TObject; var textureFileName:
  string);
var
  mLib: TGLMaterialLibrary;
  i: Integer;
  tryName: string;
begin
  mLib := TGLMaterialLibrary((Collection as TGLLibMaterials).GetOwner);
  with mLib do
    if Assigned(FOnTextureNeeded) then
      FOnTextureNeeded(mLib, textureFileName);
  // if a ':' is present, or if it starts with a '\', consider it as an absolute path
  if (Pos(':', textureFileName) > 0) or (Copy(textureFileName, 1, 1) = PathDelim)
    then
    Exit;
  // ok, not an absolute path, try given paths
  with mLib do
  begin
    if FTexturePathList <> nil then
      for i := 0 to FTexturePathList.Count - 1 do
      begin
        tryName := IncludeTrailingPathDelimiter(FTexturePathList[i]) +
          textureFileName;
        if (Assigned(vAFIOCreateFileStream) and FileStreamExists(tryName)) or
          FileExists(tryName) then
        begin
          textureFileName := tryName;
          Break;
        end;
      end;
  end;
end;

// ------------------
// ------------------ TGLLibMaterials ------------------
// ------------------

// Create
//

constructor TGLLibMaterials.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TGLLibMaterial);
end;

// Loaded
//

procedure TGLLibMaterials.Loaded;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Loaded;
end;

// SetItems
//

procedure TGLLibMaterials.SetItems(index: Integer; const val: TGLLibMaterial);
begin
  inherited Items[index] := val;
end;

// GetItems
//

function TGLLibMaterials.GetItems(index: Integer): TGLLibMaterial;
begin
  Result := TGLLibMaterial(inherited Items[index]);
end;

// DestroyHandles
//

procedure TGLLibMaterials.DestroyHandles;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].DestroyHandles;
end;

// Owner
//

function TGLLibMaterials.Owner: TPersistent;
begin
  Result := GetOwner;
end;

// Add
//

function TGLLibMaterials.Add: TGLLibMaterial;
begin
  Result := (inherited Add) as TGLLibMaterial;
end;

// FindItemID
//

function TGLLibMaterials.FindItemID(ID: Integer): TGLLibMaterial;
begin
  Result := (inherited FindItemID(ID)) as TGLLibMaterial;
end;

// MakeUniqueName
//

function TGLLibMaterials.MakeUniqueName(const nameRoot: TGLLibMaterialName):
  TGLLibMaterialName;
var
  i: Integer;
begin
  Result := nameRoot;
  i := 1;
  while GetLibMaterialByName(Result) <> nil do
  begin
    Result := nameRoot + IntToStr(i);
    Inc(i);
  end;
end;

// GetLibMaterialByName
//

function TGLLibMaterials.GetLibMaterialByName(const AName: TGLLibMaterialName):
  TGLLibMaterial;
var
  i, hk: Integer;
  lm: TGLLibMaterial;
begin
  hk := TGLLibMaterial.ComputeNameHashKey(AName);
  for i := 0 to Count - 1 do
  begin
    lm := TGLLibMaterial(inherited Items[i]);
    if (lm.NameHashKey = hk) and (lm.Name = AName) then
    begin
      Result := lm;
      Exit;
    end;
  end;
  Result := nil;
end;

// GetTextureIndex
//

function TGLLibMaterials.GetTextureIndex(const Texture: TGLTexture): Integer;
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I).Material.Texture = Texture then
      begin
        Result := I;
        Exit;
      end;
  Result := -1;
end;

// GetMaterialIndex
//

function TGLLibMaterials.GetMaterialIndex(const Material: TGLMaterial): Integer;
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I).Material = Material then
      begin
        Result := I;
        Exit;
      end;
  Result := -1;
end;

// GetMaterialIndex
//

function TGLLibMaterials.GetNameOfTexture(const Texture: TGLTexture):
  TGLLibMaterialName;
var
  MatIndex: Integer;
begin
  MatIndex := GetTextureIndex(Texture);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).Name
  else
    Result := '';
end;

// GetNameOfMaterial
//

function TGLLibMaterials.GetNameOfLibMaterial(const Material: TGLLibMaterial):
  TGLLibMaterialName;
var
  MatIndex: Integer;
begin
  MatIndex := IndexOf(Material);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).Name
  else
    Result := '';
end;

// IndexOf
//

function TGLLibMaterials.IndexOf(const Item: TGLLibMaterial): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I) = Item then
      begin
        Result := I;
        Exit;
      end;
end;

// PrepareBuildList
//

procedure TGLLibMaterials.PrepareBuildList;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TGLLibMaterial(inherited Items[i]).PrepareBuildList;
end;

// SetNamesToTStrings
//

procedure TGLLibMaterials.SetNamesToTStrings(aStrings: TStrings);
var
  i: Integer;
  lm: TGLLibMaterial;
begin
  with aStrings do
  begin
    BeginUpdate;
    Clear;
    for i := 0 to Self.Count - 1 do
    begin
      lm := TGLLibMaterial(inherited Items[i]);
      AddObject(lm.Name, lm);
    end;
    EndUpdate;
  end;
end;

// DeleteUnusedMaterials
//

procedure TGLLibMaterials.DeleteUnusedMaterials;
var
  i: Integer;
  gotNone: Boolean;
begin
  BeginUpdate;
  repeat
    gotNone := True;
    for i := Count - 1 downto 0 do
    begin
      if TGLLibMaterial(inherited Items[i]).userList.Count = 0 then
      begin
        TGLLibMaterial(inherited Items[i]).Free;
        gotNone := False;
      end;
    end;
  until gotNone;
  EndUpdate;
end;

// ------------------
// ------------------ TGLMaterialLibrary ------------------
// ------------------

// Create
//

constructor TGLMaterialLibrary.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials := TGLLibMaterials.Create(Self);
end;

// Destroy
//

destructor TGLMaterialLibrary.Destroy;
begin
  Assert(FLastAppliedMaterial = nil, 'Unbalanced material application');
  FTexturePathList.Free;
  FMaterials.Free;
  FMaterials := nil;
  inherited;
end;

// DestroyHandles
//

procedure TGLMaterialLibrary.DestroyHandles;
begin
  if Assigned(FMaterials) then
    FMaterials.DestroyHandles;
end;

// Loaded
//

procedure TGLMaterialLibrary.Loaded;
begin
  FMaterials.Loaded;
  inherited;
end;

// SetMaterials
//

procedure TGLMaterialLibrary.SetMaterials(const val: TGLLibMaterials);
begin
  FMaterials.Assign(val);
end;

// StoreMaterials
//

function TGLMaterialLibrary.StoreMaterials: Boolean;
begin
  Result := (FMaterials.Count > 0);
end;

// SetTexturePaths
//

procedure TGLMaterialLibrary.SetTexturePaths(const val: string);
var
  i, lp: Integer;

  procedure AddCurrent;
  var
    buf: string;
  begin
    buf := Trim(Copy(val, lp + 1, i - lp - 1));
    if Length(buf) > 0 then
    begin
      // make sure '\' is the terminator
      buf := IncludeTrailingPathDelimiter(buf);
      FTexturePathList.Add(buf);
    end;
  end;

begin
  FTexturePathList.Free;
  FTexturePathList := nil;
  FTexturePaths := val;
  if val <> '' then
  begin
    FTexturePathList := TStringList.Create;
    lp := 0;
    for i := 1 to Length(val) do
    begin
      if val[i] = ';' then
      begin
        AddCurrent;
        lp := i;
      end;
    end;
    i := Length(val) + 1;
    AddCurrent;
  end;
end;

// WriteToFiler
//

procedure TGLMaterialLibrary.WriteToFiler(writer: TVirtualWriter);
var
  i, j: Integer;
  libMat: TGLLibMaterial;
  tex: TGLTexture;
  img: TGLTextureImage;
  pim: TGLPersistentImage;
  ss: TStringStream;
  bmp: TGLBitmap;
  texExItem: TGLTextureExItem;
begin
  with writer do
  begin
    WriteInteger(3); // archive version 0, texture persistence only
    // archive version 1, libmat properties
    // archive version 2, Material.TextureEx properties
    // archive version 3, Material.Texture properties
    WriteInteger(Materials.Count);
    for i := 0 to Materials.Count - 1 do
    begin
      // version 0
      libMat := Materials[i];
      WriteString(libMat.Name);
      tex := libMat.Material.Texture;
      img := tex.Image;
      pim := TGLPersistentImage(img);
      if tex.Enabled and (img is TGLPersistentImage) and (pim.Picture.Graphic <>
        nil) then
      begin
        WriteBoolean(true);
        ss := TStringStream.Create('');
        try
          bmp := TGLBitmap.Create;
          try
            bmp.Assign(pim.Picture.Graphic);
            bmp.SaveToStream(ss);
          finally
            bmp.Free;
          end;
          WriteString(ss.DataString);
        finally
          ss.Free;
        end;

        // version 3
        with libMat.Material.Texture do
        begin
          Write(BorderColor.AsAddress^, SizeOf(Single) * 4);
          WriteInteger(Integer(Compression));
          WriteInteger(Integer(DepthTextureMode));
          Write(EnvColor.AsAddress^, SizeOf(Single) * 4);
          WriteInteger(Integer(FilteringQuality));
          WriteInteger(Integer(ImageAlpha));
          WriteFloat(ImageBrightness);
          WriteFloat(ImageGamma);
          WriteInteger(Integer(MagFilter));
          WriteInteger(Integer(MappingMode));
          Write(MappingSCoordinates.AsAddress^, SizeOf(Single) * 4);
          Write(MappingTCoordinates.AsAddress^, SizeOf(Single) * 4);
          Write(MappingRCoordinates.AsAddress^, SizeOf(Single) * 4);
          Write(MappingQCoordinates.AsAddress^, SizeOf(Single) * 4);
          WriteInteger(Integer(MinFilter));
          WriteFloat(NormalMapScale);
          WriteInteger(Integer(TextureCompareFunc));
          WriteInteger(Integer(TextureCompareMode));
          WriteInteger(Integer(TextureFormat));
          WriteInteger(Integer(TextureMode));
          WriteInteger(Integer(TextureWrap));
          WriteInteger(Integer(TextureWrapR));
          WriteInteger(Integer(TextureWrapS));
          WriteInteger(Integer(TextureWrapT));
        end;
        // version 3 end

      end
      else
        WriteBoolean(False);
      with libMat.Material.FrontProperties do
      begin
        Write(Ambient.AsAddress^, SizeOf(Single) * 3);
        Write(Diffuse.AsAddress^, SizeOf(Single) * 4);
        Write(Emission.AsAddress^, SizeOf(Single) * 3);
        Write(Specular.AsAddress^, SizeOf(Single) * 3);
      end;

      //version 1
      with libMat.Material.FrontProperties do
      begin
        Write(FShininess, 1);
        WriteInteger(Integer(PolygonMode));
      end;
      with libMat.Material.BackProperties do
      begin
        Write(Ambient.AsAddress^, SizeOf(Single) * 3);
        Write(Diffuse.AsAddress^, SizeOf(Single) * 4);
        Write(Emission.AsAddress^, SizeOf(Single) * 3);
        Write(Specular.AsAddress^, SizeOf(Single) * 3);
        Write(Byte(FShininess), 1);
        WriteInteger(Integer(PolygonMode));
      end;
      WriteInteger(Integer(libMat.Material.BlendingMode));

      // version 3
      with libMat.Material do
      begin
        if BlendingMode = bmCustom then
        begin
          WriteBoolean(TRUE);
          with BlendingParams do
          begin
            WriteFloat(AlphaFuncRef);
            WriteInteger(Integer(AlphaFunctType));
            WriteInteger(Integer(BlendFuncDFactor));
            WriteInteger(Integer(BlendFuncSFactor));
            WriteBoolean(UseAlphaFunc);
            WriteBoolean(UseBlendFunc);
          end;
        end
        else
          WriteBoolean(FALSE);

        WriteInteger(Integer(FaceCulling));
      end;
      // version 3 end

      WriteInteger(SizeOf(TMaterialOptions));
      Write(libMat.Material.MaterialOptions, SizeOf(TMaterialOptions));
      Write(libMat.TextureOffset.AsAddress^, SizeOf(Single) * 3);
      Write(libMat.TextureScale.AsAddress^, SizeOf(Single) * 3);
      WriteString(libMat.Texture2Name);

      // version 2
      WriteInteger(libMat.Material.TextureEx.Count);
      for j := 0 to libMat.Material.TextureEx.Count - 1 do
      begin
        texExItem := libMat.Material.TextureEx[j];
        img := texExItem.Texture.Image;
        pim := TGLPersistentImage(img);
        if texExItem.Texture.Enabled and (img is TGLPersistentImage)
          and (pim.Picture.Graphic <> nil) then
        begin
          WriteBoolean(True);
          ss := TStringStream.Create('');
          try
            bmp := TGLBitmap.Create;
            try
              bmp.Assign(pim.Picture.Graphic);
              bmp.SaveToStream(ss);
            finally
              bmp.Free;
            end;
            WriteString(ss.DataString);
          finally
            ss.Free;
          end;
        end
        else
          WriteBoolean(False);
        WriteInteger(texExItem.TextureIndex);
        Write(texExItem.TextureOffset.AsAddress^, SizeOf(Single) * 3);
        Write(texExItem.TextureScale.AsAddress^, SizeOf(Single) * 3);
      end;
    end;
  end;
end;

// ReadFromFiler
//

procedure TGLMaterialLibrary.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: Integer;
  libMat: TGLLibMaterial;
  i, n, size, tex, texCount: Integer;
  LName: string;
  ss: TStringStream;
  bmp: TGLBitmap;
  texExItem: TGLTextureExItem;
begin
  archiveVersion := reader.ReadInteger;
  if (archiveVersion >= 0) and (archiveVersion <= 3) then
    with reader do
    begin
      if not FDoNotClearMaterialsOnLoad then
        Materials.Clear;
      n := ReadInteger;
      for i := 0 to n - 1 do
      begin
        // version 0
        LName := ReadString;
        if FDoNotClearMaterialsOnLoad then
          libMat := LibMaterialByName(LName)
        else
          libMat := nil;
        if ReadBoolean then
        begin
          ss := TStringStream.Create(ReadString);
          try
            bmp := TGLBitmap.Create;
            try
              bmp.LoadFromStream(ss);
              if libMat = nil then
                libMat := AddTextureMaterial(LName, bmp)
              else
                libMat.Material.Texture.Image.Assign(bmp);
            finally
              bmp.Free;
            end;
          finally
            ss.Free;
          end;

          // version 3
          if archiveVersion >= 3 then
            with libMat.Material.Texture do
            begin
              Read(BorderColor.AsAddress^, SizeOf(Single) * 4);
              Compression := TGLTextureCompression(ReadInteger);
              DepthTextureMode := TGLDepthTextureMode(ReadInteger);
              Read(EnvColor.AsAddress^, SizeOf(Single) * 4);
              FilteringQuality := TGLTextureFilteringQuality(ReadInteger);
              ImageAlpha := TGLTextureImageAlpha(ReadInteger);
              ImageBrightness := ReadFloat;
              ImageGamma := ReadFloat;
              MagFilter := TGLMagFilter(ReadInteger);
              MappingMode := TGLTextureMappingMode(ReadInteger);
              Read(MappingSCoordinates.AsAddress^, SizeOf(Single) * 4);
              Read(MappingTCoordinates.AsAddress^, SizeOf(Single) * 4);
              Read(MappingRCoordinates.AsAddress^, SizeOf(Single) * 4);
              Read(MappingQCoordinates.AsAddress^, SizeOf(Single) * 4);
              MinFilter := TGLMinFilter(ReadInteger);
              NormalMapScale := ReadFloat;
              TextureCompareFunc := TGLDepthCompareFunc(ReadInteger);
              TextureCompareMode := TGLTextureCompareMode(ReadInteger);
              TextureFormat := TGLTextureFormat(ReadInteger);
              TextureMode := TGLTextureMode(ReadInteger);
              TextureWrap := TGLTextureWrap(ReadInteger);
              TextureWrapR := TGLSeparateTextureWrap(ReadInteger);
              TextureWrapS := TGLSeparateTextureWrap(ReadInteger);
              TextureWrapT := TGLSeparateTextureWrap(ReadInteger);
            end;
          // version 3 end

        end
        else
        begin
          if libMat = nil then
          begin
            libMat := Materials.Add;
            libMat.Name := LName;
          end;
        end;
        with libMat.Material.FrontProperties do
        begin
          Read(Ambient.AsAddress^, SizeOf(Single) * 3);
          Read(Diffuse.AsAddress^, SizeOf(Single) * 4);
          Read(Emission.AsAddress^, SizeOf(Single) * 3);
          Read(Specular.AsAddress^, SizeOf(Single) * 3);
        end;

        // version 1
        if archiveVersion >= 1 then
        begin
          with libMat.Material.FrontProperties do
          begin
            Read(FShininess, 1);
            PolygonMode := TPolygonMode(ReadInteger);
          end;
          with libMat.Material.BackProperties do
          begin
            Read(Ambient.AsAddress^, SizeOf(Single) * 3);
            Read(Diffuse.AsAddress^, SizeOf(Single) * 4);
            Read(Emission.AsAddress^, SizeOf(Single) * 3);
            Read(Specular.AsAddress^, SizeOf(Single) * 3);
            Read(FShininess, 1);
            PolygonMode := TPolygonMode(ReadInteger);
          end;
          libMat.Material.BlendingMode := TBlendingMode(ReadInteger);

          // version 3
          if archiveVersion >= 3 then
          begin
            if ReadBoolean then
              with libMat.Material.BlendingParams do
              begin
                AlphaFuncRef := ReadFloat;
                AlphaFunctType := TGlAlphaFunc(ReadInteger);
                BlendFuncDFactor := TBlendFunction(ReadInteger);
                BlendFuncSFactor := TBlendFunction(ReadInteger);
                UseAlphaFunc := ReadBoolean;
                UseBlendFunc := ReadBoolean;
              end;

            libMat.Material.FaceCulling := TFaceCulling(ReadInteger);
          end;
          // version 3 end

          size := ReadInteger;
          Read(libMat.Material.FMaterialOptions, size);
          Read(libMat.TextureOffset.AsAddress^, SizeOf(Single) * 3);
          Read(libMat.TextureScale.AsAddress^, SizeOf(Single) * 3);
          libMat.Texture2Name := ReadString;
        end;

        // version 2
        if archiveVersion >= 2 then
        begin
          texCount := ReadInteger;
          for tex := 0 to texCount - 1 do
          begin
            texExItem := libMat.Material.TextureEx.Add;
            if ReadBoolean then
            begin
              ss := TStringStream.Create(ReadString);
              bmp := TGLBitmap.Create;
              try
                bmp.LoadFromStream(ss);
                texExItem.Texture.Image.Assign(bmp);
                texExItem.Texture.Enabled := True;
              finally
                bmp.Free;
                ss.Free;
              end;
            end;
            texExItem.TextureIndex := ReadInteger;
            Read(texExItem.TextureOffset.AsAddress^, SizeOf(Single) * 3);
            Read(texExItem.TextureScale.AsAddress^, SizeOf(Single) * 3);
          end;
        end;
      end;
    end
  else
    RaiseFilerException(Self.ClassType, archiveVersion);
end;

// SaveToStream
//

procedure TGLMaterialLibrary.SaveToStream(aStream: TStream);
var
  wr: TBinaryWriter;
begin
  wr := TBinaryWriter.Create(aStream);
  try
    Self.WriteToFiler(wr);
  finally
    wr.Free;
  end;
end;

// LoadFromStream
//

procedure TGLMaterialLibrary.LoadFromStream(aStream: TStream);
var
  rd: TBinaryReader;
begin
  rd := TBinaryReader.Create(aStream);
  try
    Self.ReadFromFiler(rd);
  finally
    rd.Free;
  end;
end;

// AddMaterialsFromStream
//

procedure TGLMaterialLibrary.AddMaterialsFromStream(aStream: TStream);
begin
  FDoNotClearMaterialsOnLoad := True;
  try
    LoadFromStream(aStream);
  finally
    FDoNotClearMaterialsOnLoad := False;
  end;
end;

// SaveToFile
//

procedure TGLMaterialLibrary.SaveToFile(const fileName: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

// LoadFromFile
//

procedure TGLMaterialLibrary.LoadFromFile(const fileName: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenRead + fmShareDenyNone);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

// AddMaterialsFromFile
//

procedure TGLMaterialLibrary.AddMaterialsFromFile(const fileName: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenRead + fmShareDenyNone);
  try
    AddMaterialsFromStream(fs);
  finally
    fs.Free;
  end;
end;

// AddTextureMaterial
//

function TGLMaterialLibrary.AddTextureMaterial(const materialName, fileName:
  string;
  persistent: Boolean = True): TGLLibMaterial;
begin
  Result := Materials.Add;
  with Result do
  begin
    Name := materialName;
    with Material.Texture do
    begin
      MinFilter := miLinearMipmapLinear;
      MagFilter := maLinear;
      TextureMode := tmModulate;
      Disabled := False;
      if persistent then
      begin
        ImageClassName := TGLPersistentImage.ClassName;
        if fileName <> '' then
          Image.LoadFromFile(fileName);
      end
      else
      begin
        ImageClassName := TGLPicFileImage.ClassName;
        TGLPicFileImage(Image).PictureFileName := fileName;
      end;
    end;
  end;
end;

// AddTextureMaterial
//

function TGLMaterialLibrary.AddTextureMaterial(const materialName: string;
  graphic: TGLGraphic): TGLLibMaterial;
begin
  Result := Materials.Add;
  with Result do
  begin
    Name := materialName;
    with Material.Texture do
    begin
      MinFilter := miLinearMipmapLinear;
      MagFilter := maLinear;
      TextureMode := tmModulate;
      Disabled := False;
      Image.Assign(graphic);
    end;
  end;
end;

// LibMaterialByName
//

function TGLMaterialLibrary.LibMaterialByName(const AName: TGLLibMaterialName):
  TGLLibMaterial;
begin
  if Assigned(Self) then
    Result := Materials.GetLibMaterialByName(AName)
  else
    Result := nil;
end;

// TextureByName
//

function TGLMaterialLibrary.TextureByName(const LibMatName: TGLLibMaterialName):
  TGLTexture;
var
  LibMat: TGLLibMaterial;
begin
  if Self = nil then
    raise ETexture.Create(glsErrorEx + glsMatLibNotDefined)
  else if LibMatName = '' then
    Result := nil
  else
  begin
    LibMat := LibMaterialByName(LibMatName);
    if LibMat = nil then
      raise ETexture.CreateFmt(glsErrorEx + glsMaterialNotFoundInMatlibEx,
        [LibMatName])
    else
      Result := LibMat.Material.Texture;
  end;
end;

// GetNameOfTexture
//

function TGLMaterialLibrary.GetNameOfTexture(const Texture: TGLTexture):
  TGLLibMaterialName;
begin
  if (Self = nil) or (Texture = nil) then
    Result := ''
  else
    Result := FMaterials.GetNameOfTexture(Texture);
end;

// GetNameOfMaterial
//

function TGLMaterialLibrary.GetNameOfLibMaterial(const LibMat: TGLLibMaterial):
  TGLLibMaterialName;
begin
  if (Self = nil) or (LibMat = nil) then
    Result := ''
  else
    Result := FMaterials.GetNameOfLibMaterial(LibMat);
end;

// ApplyMaterial
//

function TGLMaterialLibrary.ApplyMaterial(const materialName: string; var rci:
  TRenderContextInfo): Boolean;
begin
  FLastAppliedMaterial := Materials.GetLibMaterialByName(materialName);
  Result := Assigned(FLastAppliedMaterial);
  if Result then
    FLastAppliedMaterial.Apply(rci);
end;

// UnApplyMaterial
//

function TGLMaterialLibrary.UnApplyMaterial(var rci: TRenderContextInfo):
  Boolean;
begin
  if Assigned(FLastAppliedMaterial) then
  begin
    Result := FLastAppliedMaterial.UnApply(rci);
    if not Result then
      FLastAppliedMaterial := nil;
  end
  else
    Result := False;
end;

{ TGLBlendingParameters }

procedure TGLBlendingParameters.Apply(var rci: TRenderContextInfo);
begin
  if FUseAlphaFunc then
  begin
    rci.GLStates.Enable(stAlphaTest);
    rci.GLStates.SetGLAlphaFunction(FAlphaFuncType, FAlphaFuncRef);
  end
  else
    rci.GLStates.Disable(stAlphaTest);
  if FUseBlendFunc then
  begin
    rci.GLStates.Enable(stBlend);
    rci.GLStates.SetBlendFunc(FBlendFuncSFactor, FBlendFuncDFactor);
  end
  else
    rci.GLStates.Disable(stBlend);
end;

procedure TGLBlendingParameters.Changed;
begin
  // DaStr: turned off because there is no way to know TGLMaterial's real owner.
  //  if not (csLoading in GetRealOwner.GetRealOwner.ComponentState) then
  //  GetRealOwner.SetBlendingMode(bmCustom);
end;

constructor TGLBlendingParameters.Create(AOwner: TPersistent);
begin
  inherited;
  FUseAlphaFunc := False;
  FAlphaFuncType := cfGreater;
  FAlphaFuncRef := 0;

  FUseBlendFunc := True;
  FBlendFuncSFactor := bfSrcAlpha;
  FBlendFuncDFactor := bfOneMinusSrcAlpha;
end;

function TGLBlendingParameters.GetRealOwner: TGLMaterial;
begin
  Result := TGLMaterial(inherited GetOwner);
end;

procedure TGLBlendingParameters.SetAlphaFuncRef(const Value: TGLclampf);
begin
  if (FAlphaFuncRef <> Value) then
  begin
    FAlphaFuncRef := Value;
    Changed();
  end;
end;

procedure TGLBlendingParameters.SetAlphaFuncType(
  const Value: TGlAlphaFunc);
begin
  if (FAlphaFuncType <> Value) then
  begin
    FAlphaFuncType := Value;
    Changed();
  end;
end;

procedure TGLBlendingParameters.SetBlendFuncDFactor(
  const Value: TBlendFunction);
begin
  if (FBlendFuncDFactor <> Value) then
  begin
    FBlendFuncDFactor := Value;
    Changed();
  end;
end;

procedure TGLBlendingParameters.SetBlendFuncSFactor(
  const Value: TBlendFunction);
begin
  if (FBlendFuncSFactor <> Value) then
  begin
    FBlendFuncSFactor := Value;
    Changed();
  end;
end;

procedure TGLBlendingParameters.SetUseAlphaFunc(const Value: Boolean);
begin
  if (FUseAlphaFunc <> Value) then
  begin
    FUseAlphaFunc := Value;
    Changed();
  end;
end;

procedure TGLBlendingParameters.SetUseBlendFunc(const Value: Boolean);
begin
  if (FUseBlendFunc <> Value) then
  begin
    FUseBlendFunc := Value;
    Changed();
  end;
end;

function TGLBlendingParameters.StoreAlphaFuncRef: Boolean;
begin
  Result := (Abs(AlphaFuncRef) > 0.001);
end;

initialization

  RegisterClasses([TGLMaterialLibrary, TGLMaterial, TGLShader]);

end.

