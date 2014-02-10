//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLHeightData<p>

  Classes for height data access.<p>

  The components and classes in the unit are the core data providers for
  height-based objects (terrain rendering mainly), they are independant
  from the rendering stage.<p>

  In short: access to raw height data is performed by a THeightDataSource
  subclass, that must take care of performing all necessary data access,
  cacheing and manipulation to provide THeightData objects. A THeightData
  is basicly a square, power of two dimensionned raster heightfield, and
  holds the data a renderer needs.<p>

  <b>History : </b><font size=-1><ul>
  <li>10/01/13 - PW - Added CPP compatibility: considered sensitivity to upper case characters in identifiers
  <li>18/07/10 - Yar - Improved FPC compatibility (thanks to Rustam Asmandiarov aka Predator)
  <li>17/07/07 - LIN - Bugfix: hdsNone tiles were not being released. (Now also deletes Queued tiles that are no longer needed).
  <li>17/07/07 - LIN - Reversed the order in which Queued tiles are prepared.
  <li>03/04/07 - DaStr - Commented out lines that caused compiler hints
                 Added more explicit pointer dereferencing
                 Renamed GLS_DELPHI_5_UP to GLS_DELPHI_4_DOWN for
                 FPC compatibility (thanks Burkhard Carstens)
  <li>27/03/07 - LIN- Data is now prepared in 3 stages, to prevent multi-threading issues:
                -BeforePreparingData : (Main Thread) - Create empty data structures and textures here.
                -PreparingData       : (Sub-Thread)  - Fill in the empty structures (MUST be thread safe)
                -AfterPreparingData  : (Main Thread) - Perform any cleanup, which cant be done from a sub-thread
  <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTracekrID=1681585)
  <li>14/03/07 - DaStr - Added explicit pointer dereferencing
                (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
  <li>13/02/07 - LIN- Added THeightDataSource.TextureCoordinates -
                 Called from TGLBitmapHDS and TGLHeightTileFileHDS
                 Many tweaks and changes to threading. (I hope I havent broken anything)
  <li>02/02/07 - LIN- Added TGLHeightDataSourceFilter
  <li>30/01/07 - LIN- Added GLHeightData.LibMaterial. (Use instead of MaterialName)
                 GLHeightData is now derived from TGLUpdateAbleObject
                 GLHeightData is now compatible with TGLLibMaterials.DeleteUnusedMaterials
  <li>19/01/07 - LIN- Added 'Inverted' property to TGLBitmapHDS
  <li>10/08/04 - SG - THeightData.InterpolatedHeight fix (Alan Rose)
  <li>03/07/04 - LR - Corrections for Linux compatibility
                 CreateMonochromeBitmap NOT implemented for Linux
  <li>12/07/03 - EG - Further InterpolatedHeight fixes
  <li>26/06/03 - EG - Fixed InterpolatedHeight HDS selection
  <li>06/02/03 - EG - Added Hash index to HeightDataSource, HeightMin/Max
  <li>24/01/03 - EG - Fixed ByteHeight normalization scaling
  <li>07/01/03 - JJ - fixed InterpolatedHeight... Old code left in comment...
  <li>03/12/02 - EG - Added hdtDefault, InterpolatedHeight/Dirty fix (Phil Scadden)
  <li>25/08/02 - EG - THeightData.MarkData/Release fix (Phil Scadden)
  <li>10/07/02 - EG - Support for non-wrapping TGLBitmapHDS
  <li>16/06/02 - EG - Changed HDS destruction sequence (notification-safe),
                 THeightData now has a MaterialName property
  <li>24/02/02 - EG - Faster Cleanup & cache management
  <li>21/02/02 - EG - hdtWord replaced by hdtSmallInt, added MarkDirty
  <li>04/02/02 - EG - CreateMonochromeBitmap now shielded against Jpeg "Change" oddity
  <li>10/09/01 - EG - Added TGLTerrainBaseHDS
  <li>04/03/01 - EG - Added InterpolatedHeight
  <li>11/02/01 - EG - Creation
  </ul></font>
}
unit GLHeightData;

interface

{$I GLScene.inc}

uses SysUtils, Classes, VectorGeometry, GLCrossPlatform, GLMaterial, BaseClasses
{$IFDEF FPC}, IntfGraphics {$ENDIF};

type
  TByteArray = array [0 .. MaxInt div (2*SizeOf(Byte))] of Byte;
  TByteRaster = array [0 .. MaxInt div (2*SizeOf(Pointer))] of PByteArray;
  PByteRaster = ^TByteRaster;
  TSmallintArray = array [0 .. MaxInt div (2*SizeOf(SmallInt))] of SmallInt;
  PSmallIntArray = ^TSmallintArray;
  TSmallIntRaster = array [0 .. MaxInt div (2*SizeOf(Pointer))] of PSmallIntArray;
  PSmallIntRaster = ^TSmallIntRaster;
  TSingleRaster = array [0 .. MaxInt div (2*SizeOf(Pointer))] of PSingleArray;
  PSingleRaster = ^TSingleRaster;

  THeightData = class;
  THeightDataClass = class of THeightData;

  // THeightDataType
  //
  { : Determines the type of data stored in a THeightData.<p>
    There are 3 data types (8 bits unsigned, signed 16 bits and 32 bits).<p>
    Conversions: (128*(ByteValue-128)) = SmallIntValue = Round(SingleValue).<p>
    The 'hdtDefault' type is used for request only, and specifies that the
    default type for the source should be used. }
  THeightDataType = (hdtByte, hdtSmallInt, hdtSingle, hdtDefault);

  // THeightDataSource
  //
  { : Base class for height datasources.<p>
    This class is abstract and presents the standard interfaces for height
    data retrieval (THeightData objects). The class offers the following
    features (that a subclass may decide to implement or not, what follow
    is the complete feature set, check subclass doc to see what is actually
    supported):<ul>
    <li>Pooling / Cacheing (return a THeightData with its "Release" method)
    <li>Pre-loading : specify a list of THeightData you want to preload
    <li>Multi-threaded preload/queueing : specified list can be loaded in
    a background task.
    </p> }

  THeightDataSource = class(TComponent)
  private
    { Private Declarations }
    FData: TThreadList; // stores all THeightData, whatever their state/type
    FDataHash: array [0 .. 255] of TList; // X/Y hash references for HeightDatas
    FThread: TThread; // queue manager
    FMaxThreads: Integer;
    FMaxPoolSize: Integer;
    FHeightDataClass: THeightDataClass;
    // FReleaseLatency : TDateTime;      //Not used anymore???
    FDefaultHeight: Single;
  protected
    { Protected Declarations }
    procedure SetMaxThreads(const Val: Integer);

    function HashKey(XLeft, YTop: Integer): Integer;

    { : Adjust this property in you subclasses. }
    property HeightDataClass: THeightDataClass read FHeightDataClass
      write FHeightDataClass;

    { : Looks up the list and returns the matching THeightData, if any. }
    function FindMatchInList(XLeft, YTop, size: Integer;
      DataType: THeightDataType): THeightData;
  public
    { Public Declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

{$IFDEF GLS_DELPHI_4_DOWN}
    procedure RemoveFreeNotification(AComponent: TComponent);
{$ENDIF}
    { : Access to currently pooled THeightData objects, and Thread locking }
    property Data: TThreadList read FData;

    { : Empties the Data list, terminating thread if necessary.<p>
      If some THeightData are hdsInUse, triggers an exception and does
      nothing. }
    procedure Clear;
    { : Removes less used TDataHeight objects from the pool.<p>
      Only removes objects whose state is hdsReady and UseCounter is zero,
      starting from the end of the list until total data size gets below
      MaxPoolSize (or nothing can be removed). }
    procedure CleanUp;

    { : Base THeightData requester method.<p>
      Returns (by rebuilding it or from the cache) a THeightData
      corresponding to the given area. Size must be a power of two.<p>
      Subclasses may choose to publish it or just publish datasource-
      specific requester method using specific parameters. }
    function GetData(XLeft, YTop, Size: Integer; DataType: THeightDataType)
      : THeightData; virtual;
    { : Preloading request.<p>
      See GetData for details. }
    function PreLoad(XLeft, YTop, Size: Integer; DataType: THeightDataType)
      : THeightData; virtual;

    { : Replacing dirty tiles. }
    procedure PreloadReplacement(aHeightData: THeightData);

    { : Notification that the data is no longer used by the renderer.<p>
      Default behaviour is just to change DataState to hdsReady (ie. return
      the data to the pool) }
    procedure Release(aHeightData: THeightData); virtual;
    { : Marks the given area as "dirty" (ie source data changed).<p>
      All loaded and in-cache tiles overlapping the area are flushed. }
    procedure MarkDirty(const Area: TGLRect); overload; virtual;
    procedure MarkDirty(XLeft, YTop, xRight, yBottom: Integer); overload;
    procedure MarkDirty; overload;

    { : Maximum number of background threads.<p>
      If 0 (zero), multithreading is disabled and StartPreparingData
      will be called from the mainthread, and all preload requirements
      (queued THeightData objects) will be loaded in sequence from
      the main thread.<p>
      If 1, basic multithreading and queueing gets enabled,
      ie. StartPreparingData will be called from a thread, but from one
      thread only (ie. there is no need to implement a THeightDataThread,
      just make sure StartPreparingData code is thread-safe).<p>
      Other values (2 and more) are relevant only if you implement
      a THeightDataThread subclass and fire it in StartPreparingData. }
    property MaxThreads: Integer read FMaxThreads write SetMaxThreads;
    { : Maximum Size of TDataHeight pool in bytes.<p>
      The pool (cache) can actually get larger if more data than the pool
      can accomodate is used, but as soon as data gets released and returns
      to the pool, TDataHeight will be freed until total pool Size gets
      below this figure.<br>
      The pool manager frees TDataHeight objects who haven't been requested
      for the longest time first.<p>
      The default value of zero effectively disables pooling. }
    property MaxPoolSize: Integer read FMaxPoolSize write FMaxPoolSize;
    { : Height to return for undefined tiles. }
    property DefaultHeight: Single read FDefaultHeight write FDefaultHeight;

    { : Interpolates height for the given point. }
    function InterpolatedHeight(x, y: Single; tileSize: Integer)
      : Single; virtual;

    function Width: Integer; virtual; abstract;
    function Height: Integer; virtual; abstract;
    procedure ThreadIsIdle; virtual;

    { : This is called BEFORE StartPreparing Data, but always from the main thread. }
    procedure BeforePreparingData(HeightData: THeightData); virtual;

    { : Request to start preparing data.<p>
      If your subclass is thread-enabled, this is here that you'll create
      your thread and fire it (don't forget the requirements), if not,
      that'll be here you'll be doing your work.<br>
      Either way, you are responsible for adjusting the DataState to
      hdsReady when you're done (DataState will be hdsPreparing when this
      method will be invoked). }
    procedure StartPreparingData(HeightData: THeightData); virtual;

    { : This is called After "StartPreparingData", but always from the main thread. }
    procedure AfterPreparingData(HeightData: THeightData); virtual;

    procedure TextureCoordinates(HeightData: THeightData;
      Stretch: boolean = false);
  end;

  // THDTextureCoordinatesMode
  //
  THDTextureCoordinatesMode = (tcmWorld, tcmLocal);

  // THeightDataState
  //
  { : Possible states for a THeightData.<p>
    <ul>
    <li>hdsQueued : the data has been queued for loading
    <li>hdsPreparing : the data is currently loading or being prepared for use
    <li>hdsReady : the data is fully loaded and ready for use
    <li>hdsNone : the height data does not exist for this tile
    </ul> }
  THeightDataState = (hdsQueued, hdsPreparing, hdsReady, hdsNone);

  THeightDataThread = class;
  TOnHeightDataDirtyEvent = procedure(sender: THeightData) of object;

  THeightDataUser = record
    user: TObject;
    event: TOnHeightDataDirtyEvent;
  end;

  // THeightData
  //
  { : Base class for height data, stores a height-field raster.<p>
    The raster is a square, whose Size must be a power of two. Data can be
    accessed through a base pointer ("ByteData[n]" f.i.), or through pointer
    indirections ("ByteRaster[y][x]" f.i.), this are the fastest way to access
    height data (and the most unsecure).<br>
    Secure (with range checking) data access is provided by specialized
    methods (f.i. "ByteHeight"), in which coordinates (x & y) are always
    considered relative (like in raster access).<p>
    The class offers conversion facility between the types (as a whole data
    conversion), but in any case, the THeightData should be directly requested
    from the THeightDataSource with the appropriate format.<p>
    Though this class can be instantiated, you will usually prefer to subclass
    it in real-world cases, f.i. to add texturing data. }
  // THeightData = class (TObject)
  THeightData = class(TGLUpdateAbleObject)
  private
    { Private Declarations }
    FUsers: array of THeightDataUser;
    FOwner: THeightDataSource;
    FDataState: THeightDataState;
    FSize: Integer;
    FXLeft, FYTop: Integer;
    FUseCounter: Integer;
    FDataType: THeightDataType;
    FDataSize: Integer;
    FByteData: PByteArray;
    FByteRaster: PByteRaster;
    FSmallIntData: PSmallIntArray;
    FSmallIntRaster: PSmallIntRaster;
    FSingleData: PSingleArray;
    FSingleRaster: PSingleRaster;
    FTextureCoordinatesMode: THDTextureCoordinatesMode;
    FTCOffset, FTCScale: TTexPoint;
    FMaterialName: String; // Unsafe. Use FLibMaterial instead
    FLibMaterial: TGLLibMaterial;
    FObjectTag: TObject;
    FTag, FTag2: Integer;
    FOnDestroy: TNotifyEvent;
    FDirty: boolean;
    FHeightMin, FHeightMax: Single;

    procedure BuildByteRaster;
    procedure BuildSmallIntRaster;
    procedure BuildSingleRaster;

    procedure ConvertByteToSmallInt;
    procedure ConvertByteToSingle;
    procedure ConvertSmallIntToByte;
    procedure ConvertSmallIntToSingle;
    procedure ConvertSingleToByte;
    procedure ConvertSingleToSmallInt;

  protected
    { Protected Declarations }
    FThread: THeightDataThread;
    // thread used for multi-threaded processing (if any)

    procedure SetDataType(const val: THeightDataType);
    procedure SetMaterialName(const MaterialName: string);
    procedure SetLibMaterial(LibMaterial: TGLLibMaterial);

    function GetHeightMin: Single;
    function GetHeightMax: Single;

  public
    OldVersion: THeightData; // previous version of this tile
    NewVersion: THeightData; // the replacement tile
    DontUse: boolean; // Tells TerrainRenderer which version to use

    { Public Declarations }

    // constructor Create(AOwner : TComponent); override;
    constructor Create(AOwner: THeightDataSource; aXLeft, aYTop, aSize: Integer;
      aDataType: THeightDataType); reintroduce; virtual;
    destructor Destroy; override;

    { : The component who created and maintains this data. }
    property Owner: THeightDataSource read FOwner;

    { : Fired when the object is destroyed. }
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;

    { : Counter for use registration.<p>
      A THeightData is not returned to the pool until this counter reaches
      a value of zero. }
    property UseCounter: Integer read FUseCounter;
    { : Increments UseCounter.<p>
      User objects should implement a method that will be notified when
      the data becomes dirty, when invoked they should release the heightdata
      immediately after performing their own cleanups. }
    procedure RegisterUse;
    { : Allocate memory and prepare lookup tables for current datatype.<p>
      Fails if already allocated. Made Dynamic to allow descendants }
    procedure Allocate(const val: THeightDataType); dynamic;
    { : Decrements UseCounter.<p>
      When the counter reaches zero, notifies the Owner THeightDataSource
      that the data is no longer used.<p>
      The renderer should call Release when it no longer needs a THeighData,
      and never free/destroy the object directly. }
    procedure Release;
    { : Marks the tile as dirty.<p>
      The immediate effect is currently the destruction of the tile. }
    procedure MarkDirty;

    { : World X coordinate of top left point. }
    property XLeft: Integer read FXLeft;
    { : World Y coordinate of top left point. }
    property YTop: Integer read FYTop;
    { : Type of the data.<p>
      Assigning a new datatype will result in the data being converted. }
    property DataType: THeightDataType read FDataType write SetDataType;
    { : Current state of the data. }
    property DataState: THeightDataState read FDataState write FDataState;
    { : Size of the data square, in data units. }
    property Size: Integer read FSize;
    { : True if the data is dirty (ie. no longer up-to-date). }
    property Dirty: boolean read FDirty write FDirty;

    { : Memory Size of the raw data in bytes. }
    property DataSize: Integer read FDataSize;

    { : Access to data as a byte array (n = y*Size+x).<p>
      If THeightData is not of type hdtByte, this value is nil. }
    property ByteData: PByteArray read FByteData;
    { : Access to data as a byte raster (y, x).<p>
      If THeightData is not of type hdtByte, this value is nil. }
    property ByteRaster: PByteRaster read FByteRaster;
    { : Access to data as a SmallInt array (n = y*Size+x).<p>
      If THeightData is not of type hdtSmallInt, this value is nil. }
    property SmallIntData: PSmallIntArray read FSmallIntData;
    { : Access to data as a SmallInt raster (y, x).<p>
      If THeightData is not of type hdtSmallInt, this value is nil. }
    property SmallIntRaster: PSmallIntRaster read FSmallIntRaster;
    { : Access to data as a Single array (n = y*Size+x).<p>
      If THeightData is not of type hdtSingle, this value is nil. }
    property SingleData: PSingleArray read FSingleData;
    { : Access to data as a Single raster (y, x).<p>
      If THeightData is not of type hdtSingle, this value is nil. }
    property SingleRaster: PSingleRaster read FSingleRaster;

    { : Name of material for the tile (if terrain uses multiple materials). }
    // property MaterialName : String read FMaterialName write FMaterialName;
    // (WARNING: Unsafe when deleting textures! If possible, rather use LibMaterial.)
    property MaterialName: String read FMaterialName write SetMaterialName;
    // property LibMaterial : Links directly to the tile's TGLLibMaterial.
    // Unlike 'MaterialName', this property also registers the tile as
    // a user of the texture.
    // This prevents TGLLibMaterials.DeleteUnusedTextures from deleting the
    // used texture by mistake and causing Access Violations.
    // Use this instead of the old MaterialName property, to prevent AV's.
    property LibMaterial: TGLLibMaterial read FLibMaterial write SetLibMaterial;
    { : Texture coordinates generation mode.<p>
      Default is tcmWorld coordinates. }
    property TextureCoordinatesMode: THDTextureCoordinatesMode
      read FTextureCoordinatesMode write FTextureCoordinatesMode;
    property TextureCoordinatesOffset: TTexPoint read FTCOffset write FTCOffset;
    property TextureCoordinatesScale: TTexPoint read FTCScale write FTCScale;
    { : Height of point x, y as a Byte.<p> }
    function ByteHeight(x, y: Integer): Byte;
    { : Height of point x, y as a SmallInt.<p> }
    function SmallIntHeight(x, y: Integer): SmallInt;
    { : Height of point x, y as a Single.<p> }
    function SingleHeight(x, y: Integer): Single;
    { : Interopolated height of point x, y as a Single.<p> }
    function InterpolatedHeight(x, y: Single): Single;

    { : Minimum height in the tile.<p>
      DataSources may assign a value to prevent automatic computation
      if they have a faster/already computed value. }
    property HeightMin: Single read GetHeightMin write FHeightMin;
    { : Maximum height in the tile.<p>
      DataSources may assign a value to prevent automatic computation
      if they have a faster/already computed value. }
    property HeightMax: Single read GetHeightMax write FHeightMax;

    { : Returns the height as a single, whatever the DataType (slow). }
    function Height(x, y: Integer): Single;

    { : Calculates and returns the normal for vertex point x, y.<p>
      Sub classes may provide normal cacheing, the default implementation
      being rather blunt. }
    function Normal(x, y: Integer; const scale: TAffineVector)
      : TAffineVector; virtual;

    { : Calculates and returns the normal for cell x, y.(between vertexes) <p> }
    function NormalAtNode(x, y: Integer; const scale: TAffineVector)
      : TAffineVector; virtual;

    { : Returns True if the data tile overlaps the area. }
    function OverlapsArea(const area: TGLRect): boolean;

    { : Reserved for renderer use. }
    property ObjectTag: TObject read FObjectTag write FObjectTag;
    { : Reserved for renderer use. }
    property Tag: Integer read FTag write FTag;
    { : Reserved for renderer use. }
    property Tag2: Integer read FTag2 write FTag2;
    { : Used by perlin HDS. }
    property Thread: THeightDataThread read FThread write FThread;
  end;

  // THeightDataThread
  //
  { : A thread specialized for processing THeightData in background.<p>
    Requirements:<ul>
    <li>must have FreeOnTerminate set to true,
    <li>must check and honour Terminated swiftly
    </ul> }
  THeightDataThread = class(TThread)
  protected
    { Protected Declarations }
    FHeightData: THeightData;

  public
    { Public Declarations }
    destructor Destroy; override;
    { : The Height Data the thread is to prepare.<p> }
    property HeightData: THeightData read FHeightData write FHeightData;

  end;

  // TGLBitmapHDS
  //
  { : Bitmap-based Height Data Source.<p>
    The image is automatically wrapped if requested data is out of picture Size,
    or if requested data is larger than the picture.<p>
    The internal format is an 8 bit bitmap whose dimensions are a power of two,
    if the original image does not comply, it is StretchDraw'ed on a monochrome
    (gray) bitmap. }
  TGLBitmapHDS = class(THeightDataSource)
  private
    { Private Declarations }
    FScanLineCache: array of PByteArray;
    FBitmap: TGLBitmap;
{$IFDEF FPC}
    IntfImg1: TLazIntfImage;
{$ENDIF}
    FPicture: TGLPicture;
    FInfiniteWrap: boolean;
    FInverted: boolean;

  protected
    { Protected Declarations }
    procedure SetPicture(const val: TGLPicture);
    procedure OnPictureChanged(sender: TObject);
    procedure SetInfiniteWrap(val: boolean);
    procedure SetInverted(val: boolean);

    procedure CreateMonochromeBitmap(Size: Integer);
    procedure FreeMonochromeBitmap;
    function GetScanLine(y: Integer): PByteArray;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StartPreparingData(HeightData: THeightData); override;
    procedure MarkDirty(const area: TGLRect); override;
    function Width: Integer; override;
    function Height: Integer; override;

  published
    { Published Declarations }
    { : The picture serving as Height field data reference.<p>
      The picture is (if not already) internally converted to a 8 bit
      bitmap (grayscale). For better performance and to save memory,
      feed it this format! }
    property Picture: TGLPicture read FPicture write SetPicture;
    { : If true the height field is wrapped indefinetely. }
    property InfiniteWrap: boolean read FInfiniteWrap write SetInfiniteWrap default True;
    { : If true, the rendered terrain is a mirror image of the input data. }
    property Inverted: boolean read FInverted write SetInverted default True;

    property MaxPoolSize;
  end;

  TStartPreparingDataEvent = procedure(HeightData: THeightData) of object;
  TMarkDirtyEvent = procedure(const area: TGLRect) of object;

  // TTexturedHeightDataSource = class (TGLTexturedHeightDataSource)

  // TGLCustomHDS
  //
  { : An Height Data Source for custom use.<p>
    Provides event handlers for the various requests to be implemented
    application-side (for application-specific needs). }
  TGLCustomHDS = class(THeightDataSource)
  private
    { Private Declarations }
    FOnStartPreparingData: TStartPreparingDataEvent;
    FOnMarkDirty: TMarkDirtyEvent;

  protected
    { Protected Declarations }

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartPreparingData(HeightData: THeightData); override;

    procedure MarkDirty(const area: TGLRect); override;

  published
    { Published Declarations }
    property MaxPoolSize;

    property OnStartPreparingData: TStartPreparingDataEvent
      read FOnStartPreparingData write FOnStartPreparingData;
    property OnMarkDirtyEvent: TMarkDirtyEvent read FOnMarkDirty
      write FOnMarkDirty;
  end;

  // TGLTerrainBaseHDS
  //
  { : TerrainBase-based Height Data Source.<p>
    This component takes its data from the TerrainBase Gobal Terrain Model.<br>
    Though it can be used directly, the resolution of the TerrainBase dataset
    isn't high enough for accurate short-range representation and the data
    should rather be used as basis for further (fractal) refinement.<p>
    TerrainBase is freely available from the National Geophysical Data Center
    and World Data Center web site (http://ngdc.noaa.com).<p>
    (this component expects to find "tbase.bin" in the current directory). }
  TGLTerrainBaseHDS = class(THeightDataSource)
  private
    { Private Declarations }

  protected
    { Protected Declarations }

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartPreparingData(HeightData: THeightData); override;

  published
    { Published Declarations }
    property MaxPoolSize;
  end;

  THeightDataSourceFilter = Class;
  TSourceDataFetchedEvent = procedure(sender: THeightDataSourceFilter;
    HeightData: THeightData) of object;

  // THeightDataSourceFilter
  //
  { : Height Data Source Filter.<p>
    This component sits between the TGLTerrainRenderer, and a real THeightDataSource.
    i.e. TGLTerrainRenderer links to this. This links to the real THeightDataSource.
    Use the 'HeightDataSource' property, to link to a source HDS.
    The 'OnSourceDataFetched' event then gives you the opportunity to make any changes,
    or link in a texture to the THeightData object, BEFORE it is cached.
    It bypasses the cache of the source HDS, by calling the source's StartPreparingData procedure directly.
    The THeightData objects are then cached by THIS component, AFTER you have made your changes.
    This eliminates the need to copy and release the THeightData object from the Source HDS's cache,
    before linking your texture.  See the new version of TGLBumpmapHDS for an example. (LIN)
    To create your own HDSFilters, Derive from this component, and override the PreparingData procedure.
  }
  THeightDataSourceFilter = Class(THeightDataSource)
  private
    { Private Declarations }
    FHDS: THeightDataSource;
    FOnSourceDataFetched: TSourceDataFetchedEvent;
    FActive: boolean;
  protected
    { Protected Declarations }
    { : PreparingData:  <p>
      Override this function in your filter subclasses, to make any
      updates/changes to HeightData, before it goes into the cache.
      Make sure any code in this function is thread-safe, in case TAsyncHDS was used. }
    procedure PreparingData(HeightData: THeightData); virtual; abstract;
    procedure SetHDS(val: THeightDataSource);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Release(aHeightData: THeightData); override;
    procedure StartPreparingData(HeightData: THeightData); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function Width: Integer; override;
    function Height: Integer; override;
    property OnSourceDataFetched: TSourceDataFetchedEvent
      read FOnSourceDataFetched write FOnSourceDataFetched;

  published
    { Published Declarations }
    property MaxPoolSize;
    property HeightDataSource: THeightDataSource read FHDS write SetHDS;
    property Active: boolean read FActive write FActive;
    // If Active=False, height data passes through unchanged
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses ApplicationFileIO, GLUtils
{$IFDEF MSWINDOWS}
    , Windows // for CreateMonochromeBitmap
{$ENDIF}
    ;

// ------------------
// ------------------ THeightDataSourceThread ------------------
// ------------------

type
  THeightDataSourceThread = class(TThread)
    FOwner: THeightDataSource;
    procedure Execute; override;
    function WaitForTile(HD: THeightData; seconds: Integer): boolean;
    procedure HDSIdle;
  end;

  // Execute
  //
procedure THeightDataSourceThread.Execute;
var
  i: Integer;
  lst: TList;
  HD: THeightData;
  max: Integer;
  TdCtr: Integer;
begin
  while not Terminated do
  begin
    max := FOwner.MaxThreads;
    lst := FOwner.FData.LockList;

    // --count active threads--
    i := 0;
    TdCtr := 0;
    while (i < lst.Count) and (TdCtr < max) do
    begin
      if THeightData(lst.Items[i]).FThread <> nil then
        Inc(TdCtr);
      Inc(i);
    end;
    // ------------------------

    // --Find the queued tiles, and Start preparing them--
    i := 0;
    While ((i < lst.Count) and (TdCtr < max)) do
    begin
      HD := THeightData(lst.Items[i]);
      if HD.DataState = hdsQueued then
      begin
        FOwner.StartPreparingData(HD); // prepare
        Inc(TdCtr);
      end;
      Inc(i);
    end;
    // ---------------------------------------------------

    FOwner.FData.UnlockList;
    if (TdCtr = 0) then
      synchronize(HDSIdle);
    if (TdCtr = 0) then
      sleep(10)
    else
      sleep(0); // sleep longer if no Queued tiles were found
  end;
end;

// WaitForTile
//
// When Threading, wait a specified time, for the tile to finish preparing
function THeightDataSourceThread.WaitForTile(HD: THeightData;
  seconds: Integer): boolean;
var
  // i:integer;
  eTime: TDateTime;
begin
  eTime := now + (1000 * seconds);
  while (HD.FThread <> nil) and (now < eTime) do
  begin
    sleep(0);
  end;
  Result := (HD.FThread = nil); // true if the thread has finished
end;

// HDSIdle
//
// When using threads, HDSIdle is called in the main thread,
// whenever all HDS threads have finished, AND no queued tiles were found.
// (GLAsyncHDS uses this for the OnIdle event.)
procedure THeightDataSourceThread.HDSIdle;
begin
  self.FOwner.ThreadIsIdle;
end;

// ------------------
// ------------------ THeightDataSource ------------------
// ------------------

// Create
//
constructor THeightDataSource.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  FHeightDataClass := THeightData;
  FData := TThreadList.Create;
  for i := 0 to High(FDataHash) do
    FDataHash[i] := TList.Create;
  // FReleaseLatency:=15/(3600*24);
  FThread := THeightDataSourceThread.Create(True);
  FThread.FreeOnTerminate := false;
  THeightDataSourceThread(FThread).FOwner := self;
  if self.MaxThreads > 0 then
{$IFDEF GLS_DELPHI_2009_DOWN}
    FThread.Resume;
{$ELSE}
    FThread.Start;
{$ENDIF}
end;

// Destroy
//
destructor THeightDataSource.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  if Assigned(FThread) then
  begin
    FThread.Terminate;
{$IFDEF GLS_DELPHI_2009_DOWN}
    FThread.Resume;
{$ELSE}
    FThread.Start;
{$ENDIF}
    FThread.WaitFor;
    FThread.Free;
  end;
  Clear;
  FData.Free;
  for i := 0 to High(FDataHash) do
    FDataHash[i].Free;
end;

{$IFDEF GLS_DELPHI_4_DOWN}

// RemoveFreeNotification
//
procedure THeightDataSource.RemoveFreeNotification(AComponent: TComponent);
begin
  Notification(AComponent, opRemove);
end;
{$ENDIF}

// Clear
//
procedure THeightDataSource.Clear;
var
  i: Integer;
begin
  with FData.LockList do
  begin
    try
      for i := 0 to Count - 1 do
        if THeightData(Items[i]).UseCounter > 0 then
          if not(csDestroying in ComponentState) then
            raise Exception.Create('ERR: HeightData still in use');
      for i := 0 to Count - 1 do
      begin
        THeightData(Items[i]).FOwner := nil;
        THeightData(Items[i]).Free;
      end;
      for i := 0 to High(FDataHash) do
        FDataHash[i].Clear;
      Clear;
    finally
      FData.UnlockList;
    end;
  end;
end;

// HashKey
//
function THeightDataSource.HashKey(XLeft, YTop: Integer): Integer;
begin
  Result := (xLeft + (xLeft shr 8) + (YTop shl 1) + (YTop shr 7)) and
    High(FDataHash);
end;

// FindMatchInList
//

function THeightDataSource.FindMatchInList(XLeft, YTop, Size: Integer;
  DataType: THeightDataType): THeightData;
var
  i: Integer;
  HD: THeightData;
begin
  Result := nil;
  FData.LockList;
  try
    with FDataHash[HashKey(XLeft, YTop)] do
      for i := 0 to Count - 1 do
      begin
        HD := THeightData(Items[i]);
        // if (not hd.Dirty) and (hd.XLeft=xLeft) and (hd.YTop=YTop) and (hd.Size=Size) and (hd.DataType=DataType) then begin
        if (HD.xLeft = xLeft) and (HD.YTop = YTop) and (HD.Size = Size) and
          (HD.DataType = DataType) and (HD.DontUse = false) then
        begin
          Result := HD;
          Break;
        end;
      end;
  finally
    FData.UnlockList;
  end;
end;

// GetData
//
function THeightDataSource.GetData(XLeft, YTop, Size: Integer;
  DataType: THeightDataType): THeightData;
begin
  Result := FindMatchInList(XLeft, YTop, Size, DataType);
  if not Assigned(Result) then
    Result := PreLoad(XLeft, YTop, Size, DataType)
  else
    with FData.LockList do
    begin
      try
        Move(IndexOf(Result), 0); // Moves item to the beginning of the list.
      finally
        FData.UnlockList;
      end;
    end;
  // got one... can be used ?
  // while not (Result.DataState in [hdsReady, hdsNone]) do Sleep(0);
end;

// PreLoad
//
function THeightDataSource.PreLoad(XLeft, YTop, Size: Integer;
  DataType: THeightDataType): THeightData;
begin
  Result := HeightDataClass.Create(Self, XLeft, YTop, Size, DataType);
  with FData.LockList do
    try
      Add(Result);
      BeforePreparingData(Result);
      FDataHash[HashKey(XLeft, YTop)].Add(Result);
    finally
      FData.UnlockList;
    end;

  // -- When NOT using Threads, fully prepare the tile immediately--
  if MaxThreads = 0 then
  begin
    StartPreparingData(Result);
    AfterPreparingData(Result);
  end;
  // ---------------------------------------------------------------
end;

// PreloadReplacement
//
// When Multi-threading, this queues a replacement for a dirty tile
// The Terrain renderer will continue to use the dirty tile, until the replacement is complete
procedure THeightDataSource.PreloadReplacement(aHeightData: THeightData);
var
  HD: THeightData;
  NewHD: THeightData;
begin
  Assert(MaxThreads > 0);
  HD := aHeightData;
  NewHD := HeightDataClass.Create(self, HD.xLeft, HD.YTop, HD.Size,
    HD.DataType);
  with FData.LockList do
    try
      Add(NewHD);
      NewHD.OldVersion := HD; // link
      HD.NewVersion := NewHD; // link
      NewHD.DontUse := True;
      BeforePreparingData(NewHD);
      FDataHash[HashKey(HD.xLeft, HD.YTop)].Add(NewHD);
    finally
      FData.UnlockList;
    end;
end;

// Release
//
procedure THeightDataSource.Release(aHeightData: THeightData);
begin
  // nothing, yet
end;

// MarkDirty (rect)
//
procedure THeightDataSource.MarkDirty(const Area: TGLRect);
var
  i: Integer;
  HD: THeightData;
begin
  with FData.LockList do
  begin
    try
      for i := Count - 1 downto 0 do
      begin
        HD := THeightData(Items[i]);
        if HD.OverlapsArea(Area) then
          HD.MarkDirty;
      end;
    finally
      FData.UnlockList;
    end;
  end;
end;

// MarkDirty (ints)
//
procedure THeightDataSource.MarkDirty(XLeft, YTop, XRight, YBottom: Integer);
var
  r: TGLRect;
begin
  r.Left := XLeft;
  r.Top := YTop;
  r.Right := xRight;
  r.Bottom := yBottom;
  MarkDirty(r);
end;

// MarkDirty
//
procedure THeightDataSource.MarkDirty;
const
  m = MaxInt - 1;
begin
  MarkDirty(-m, -m, m, m);
end;

// CleanUp
//
procedure THeightDataSource.CleanUp;
var
  packList: boolean;
  i, k: Integer;
  usedMemory: Integer;
  HD: THeightData;
  ReleaseThis: boolean;
begin
  with FData.LockList do
  begin
    try
      usedMemory := 0;
      packList := false;
      // Cleanup dirty tiles and compute used memory
      for i := Count - 1 downto 0 do
      begin
        HD := THeightData(Items[i]);
        if HD <> nil then
          with HD do
          begin
            // --Release criteria for dirty tiles--
            ReleaseThis := false;
            if HD.Dirty then
            begin // Only release dirty tiles
              if (MaxThreads = 0) then
                ReleaseThis := True
                // when not threading, delete ALL dirty tiles
              else if (HD.DataState <> hdsPreparing) then
              begin // Dont release Preparing tiles
                if (HD.UseCounter = 0) then
                  ReleaseThis := True; // This tile is unused
                if (HD.NewVersion = nil) then
                  ReleaseThis := True
                  // This tile has no queued replacement to wait for
                else if (HD.DontUse) then
                  ReleaseThis := True; // ??This tile has already been replaced.
              end;
            end;
            // ------------------------------------
            // if Dirty then ReleaseThis:=true;
            if ReleaseThis then
            begin
              FDataHash[HashKey(HD.XLeft, HD.YTop)].Remove(HD);
              Items[i] := nil;
              FOwner := nil;
              Free;
              packList := True;
            end
            else
              usedMemory := usedMemory + HD.DataSize;
          end;
      end;
      // If MaxPoolSize exceeded, release all that may be, and pack the list
      k := 0;
      if usedMemory > MaxPoolSize then
      begin
        for i := 0 to Count - 1 do
        begin
          HD := THeightData(Items[i]);
          if HD <> nil then
            with HD do
            begin
              if (DataState <> hdsPreparing) and (UseCounter = 0) and
                (OldVersion = nil)
              // if (DataState=hdsReady)and(UseCounter=0)and(OldVersion=nil)
              then
              begin
                FDataHash[HashKey(HD.XLeft, HD.YTop)].Remove(HD);
                Items[i] := nil;
                FOwner := nil;
                Free;
                // packList:=True;
              end
              else
              begin
                Items[k] := HD;
                Inc(k);
              end;
            end;
        end;
        Count := k;
      end
      else if packList then
      begin
        for i := 0 to Count - 1 do
          if Items[i] <> nil then
          begin
            Items[k] := Items[i];
            Inc(k);
          end;
        Count := k;
      end;
    finally
      FData.UnlockList;
    end;
  end;
end;

// SetMaxThreads
//
procedure THeightDataSource.SetMaxThreads(const Val: Integer);
begin
  if (val <= 0) then
    FMaxThreads := 0
  else
  begin
    // If we didn't do threading, but will now
    // resume our thread
    if (FMaxThreads <= 0) then
{$IFDEF GLS_DELPHI_2009_DOWN}
      FThread.Resume;
{$ELSE}
      FThread.Start;
{$ENDIF}
    FMaxThreads := val;
  end;
end;

// BeforePreparingData
// Called BEFORE StartPreparingData, but always from the MAIN thread.
// Override this in subclasses, to prepare for Threading.
//
procedure THeightDataSource.BeforePreparingData(HeightData: THeightData);
begin
  //
end;

// StartPreparingData
// When Threads are used, this runs from the sub-thread, so this MUST be thread-safe.
// Any Non-thread-safe code should be placed in "BeforePreparingData"
//
procedure THeightDataSource.StartPreparingData(HeightData: THeightData);
begin
  // Only the tile Owner may set the preparing tile to ready
  if (HeightData.Owner = self) and (HeightData.DataState = hdsPreparing) then
    HeightData.FDataState := hdsReady;
end;

// AfterPreparingData
// Called AFTER StartPreparingData, but always from the MAIN thread.
// Override this in subclasses, if needed.
//
procedure THeightDataSource.AfterPreparingData(HeightData: THeightData);
begin
  //
end;

// ThreadIsIdle
//
procedure THeightDataSource.ThreadIsIdle;
begin
  // TGLAsyncHDS overrides this
end;

// TextureCoordinates
// Calculates texture World texture coordinates for the current tile.
// Use Stretch for OpenGL1.1, to hide the seams when using linear filtering.
procedure THeightDataSource.TextureCoordinates(HeightData: THeightData;
  Stretch: boolean = false);
var
  w, h, Size: Integer;
  scaleS, scaleT: Single;
  offsetS, offsetT: Single;
  HD: THeightData;
  halfpixel: Single;
begin
  HD := HeightData;
  w := self.Width;
  h := self.Height;
  Size := HD.FSize;
  // if GL_VERSION_1_2 then begin //OpenGL1.2 supports texture clamping, so seams dont show.
  if Stretch = false then
  begin // These are the real Texture coordinates
    scaleS := w / (Size - 1);
    scaleT := h / (Size - 1);
    offsetS := -((HD.XLeft / w) * scaleS);
    offsetT := -(h - (HD.YTop + Size - 1)) / (Size - 1);
  end
  else
  begin // --Texture coordinates: Stretched by 1 pixel, to hide seams on OpenGL-1.1(no Clamping)--
    scaleS := w / Size;
    scaleT := h / Size;
    halfpixel := 1 / (Size shr 1);
    offsetS := -((HD.XLeft / w) * scaleS) + halfpixel;
    offsetT := -(h - (HD.YTop + Size)) / Size - halfpixel;
  end;
  HD.FTCScale.S := scaleS;
  HD.FTCScale.T := scaleT;
  HD.FTCOffset.S := offsetS;
  HD.FTCOffset.T := offsetT;
end;

// InterpolatedHeight
//
function THeightDataSource.InterpolatedHeight(x, y: Single;
  tileSize: Integer): Single;
var
  i: Integer;
  HD, foundHd: THeightData;
begin
  with FData.LockList do
  begin
    try
      // first, lookup data list to find if aHeightData contains our point
      foundHd := nil;
      for i := 0 to Count - 1 do
      begin
        HD := THeightData(Items[i]);
        if (HD.XLeft <= x) and (HD.YTop <= y) and (HD.XLeft + HD.Size - 1 > x)
          and (HD.YTop + HD.Size - 1 > y) then
        begin
          foundHd := HD;
          Break;
        end;
      end;
    finally
      FData.UnlockList;
    end;
  end;
  if (foundHd = nil) or foundHd.Dirty then
  begin
    // not found, request one... slowest mode (should be avoided)
    if tileSize > 1 then
      foundHd := GetData(Round(x / (tileSize - 1) - 0.5) * (tileSize - 1),
        Round(y / (tileSize - 1) - 0.5) * (tileSize - 1), tileSize, hdtDefault)
    else
    begin
      Result := DefaultHeight;
      Exit;
    end;
  end
  else
  begin
    // request it using "standard" way (takes care of threads)
    foundHd := GetData(foundHd.XLeft, foundHd.YTop, foundHd.Size,
      foundHd.DataType);
  end;
  if foundHd.DataState = hdsNone then
    Result := DefaultHeight
  else
    Result := foundHd.InterpolatedHeight(x - foundHd.XLeft, y - foundHd.YTop);
end;

// ------------------
// ------------------ THeightData ------------------
// ------------------

// Create
//
constructor THeightData.Create(AOwner: THeightDataSource;
  aXLeft, aYTop, aSize: Integer; aDataType: THeightDataType);
begin
  inherited Create(AOwner);
  SetLength(FUsers, 0);
  FOwner := AOwner;
  FXLeft := aXLeft;
  FYTop := aYTop;
  FSize := aSize;
  FTextureCoordinatesMode := tcmWorld;
  FTCScale := XYTexPoint;
  FDataType := aDataType;
  FDataState := hdsQueued;
  FHeightMin := 1E30;
  FHeightMax := 1E30;

  OldVersion := nil;
  NewVersion := nil;
  DontUse := false;
end;

// Destroy
//
destructor THeightData.Destroy;
begin
  Assert(Length(FUsers) = 0,
    'You should *not* free a THeightData, use "Release" instead');
  Assert(not Assigned(FOwner),
    'You should *not* free a THeightData, use "Release" instead');
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    if FThread.Suspended then
{$IFDEF GLS_DELPHI_2009_DOWN}
      FThread.Resume;
{$ELSE}
      FThread.Start;
{$ENDIF}
    FThread.WaitFor;
  end;

  if Assigned(FOnDestroy) then
    FOnDestroy(self);
  case DataType of
    hdtByte:
      begin
        FreeMem(FByteData);
        FreeMem(FByteRaster);
      end;
    hdtSmallInt:
      begin
        FreeMem(FSmallIntData);
        FreeMem(FSmallIntRaster);
      end;
    hdtSingle:
      begin
        FreeMem(FSingleData);
        FreeMem(FSingleRaster);
      end;
    hdtDefault:
      ; // nothing
  else
    Assert(false);
  end;
  // ----------------------
  self.LibMaterial := nil; // release a used material

  // --Break any link with a new/old version of this tile--
  if Assigned(self.OldVersion) then
  begin
    self.OldVersion.NewVersion := nil;
    self.OldVersion := nil;
  end;
  if Assigned(self.NewVersion) then
  begin
    self.NewVersion.OldVersion := nil;
    self.NewVersion := nil;
  end;
  // ------------------------------------------------------

  // ----------------------
  inherited Destroy;
end;

// RegisterUse
//
procedure THeightData.RegisterUse;
begin
  Inc(FUseCounter);
end;

// Release
//
procedure THeightData.Release;
begin
  if FUseCounter > 0 then
    Dec(FUseCounter);
  if FUseCounter = 0 then
  begin
    Owner.Release(self); // ???
  end;
end;

// MarkDirty
//
// Release Dirty tiles, unless threading, and the tile is being used.
// In that case, start building a replacement tile instead.

procedure THeightData.MarkDirty;
begin
  with Owner.Data.LockList do
    try
      if (not Dirty) and (DataState <> hdsQueued) then
      begin // dont mark queued tiles as dirty
        FDirty := True;
        if (Owner.MaxThreads > 0) and (FUseCounter > 0) then
          Owner.PreloadReplacement(self)
        else
        begin
          FUseCounter := 0;
          Owner.Release(self);
        end;
      end;
    finally
      Owner.Data.UnlockList;
    end;
end;

// Allocate
//
procedure THeightData.Allocate(const val: THeightDataType);
begin
  Assert(FDataSize = 0);
  case val of
    hdtByte:
      begin
        FDataSize := Size * Size * SizeOf(Byte);
        GetMem(FByteData, FDataSize);
        BuildByteRaster;
      end;
    hdtSmallInt:
      begin
        FDataSize := Size * Size * SizeOf(SmallInt);
        GetMem(FSmallIntData, FDataSize);
        BuildSmallIntRaster;
      end;
    hdtSingle:
      begin
        FDataSize := Size * Size * SizeOf(Single);
        GetMem(FSingleData, FDataSize);
        BuildSingleRaster;
      end;
  else
    Assert(false);
  end;
  FDataType := val;
end;

// WARNING: SetMaterialName does NOT register the tile as a user of this texture.
// So, TGLLibMaterials.DeleteUnusedMaterials may see this material as unused, and delete it.
// This may lead to AV's the next time this tile is rendered.
// To be safe, rather assign the new THeightData.LibMaterial property
procedure THeightData.SetMaterialName(const MaterialName: string);
begin
  SetLibMaterial(nil);
  FMaterialName := MaterialName;
end;

procedure THeightData.SetLibMaterial(LibMaterial: TGLLibMaterial);
begin
  if Assigned(FLibMaterial) then
    FLibMaterial.UnregisterUser(self); // detach from old texture
  FLibMaterial := LibMaterial; // Attach new Material
  if Assigned(LibMaterial) then
  begin
    LibMaterial.RegisterUser(self); // Mark new Material as 'used'
    FMaterialName := LibMaterial.Name; // sync up MaterialName property
  end
  else
    FMaterialName := '';
end;

// SetDataType
//
procedure THeightData.SetDataType(const val: THeightDataType);
begin
  if (val <> FDataType) and (val <> hdtDefault) then
  begin
    if DataState <> hdsNone then
    begin
      case FDataType of
        hdtByte:
          case val of
            hdtSmallInt:
              ConvertByteToSmallInt;
            hdtSingle:
              ConvertByteToSingle;
          else
            Assert(false);
          end;
        hdtSmallInt:
          case val of
            hdtByte:
              ConvertSmallIntToByte;
            hdtSingle:
              ConvertSmallIntToSingle;
          else
            Assert(false);
          end;
        hdtSingle:
          case val of
            hdtByte:
              ConvertSingleToByte;
            hdtSmallInt:
              ConvertSingleToSmallInt;
          else
            Assert(false);
          end;
        hdtDefault:
          ; // nothing, assume StartPreparingData knows what it's doing
      else
        Assert(false);
      end;
    end;
    FDataType := val;
  end;
end;

// BuildByteRaster
//
procedure THeightData.BuildByteRaster;
var
  i: Integer;
begin
  GetMem(FByteRaster, Size * SizeOf(PByteArray));
  for i := 0 to Size - 1 do
    FByteRaster^[i] := @FByteData[i * Size]
end;

// BuildSmallIntRaster
//
procedure THeightData.BuildSmallIntRaster;
var
  i: Integer;
begin
  GetMem(FSmallIntRaster, Size * SizeOf(PSmallIntArray));
  for i := 0 to Size - 1 do
    FSmallIntRaster^[i] := @FSmallIntData[i * Size]
end;

// BuildSingleRaster
//
procedure THeightData.BuildSingleRaster;
var
  i: Integer;
begin
  GetMem(FSingleRaster, Size * SizeOf(PSingleArray));
  for i := 0 to Size - 1 do
    FSingleRaster^[i] := @FSingleData[i * Size]
end;

// ConvertByteToSmallInt
//
procedure THeightData.ConvertByteToSmallInt;
var
  i: Integer;
begin
  FreeMem(FByteRaster);
  FByteRaster := nil;
  FDataSize := Size * Size * SizeOf(SmallInt);
  GetMem(FSmallIntData, FDataSize);
  for i := 0 to Size * Size - 1 do
    FSmallIntData^[i] := (FByteData^[i] - 128) shl 7;
  FreeMem(FByteData);
  FByteData := nil;
  BuildSmallIntRaster;
end;

// ConvertByteToSingle
//
procedure THeightData.ConvertByteToSingle;
var
  i: Integer;
begin
  FreeMem(FByteRaster);
  FByteRaster := nil;
  FDataSize := Size * Size * SizeOf(Single);
  GetMem(FSingleData, FDataSize);
  for i := 0 to Size * Size - 1 do
    FSingleData^[i] := (FByteData^[i] - 128) shl 7;
  FreeMem(FByteData);
  FByteData := nil;
  BuildSingleRaster;
end;

// ConvertSmallIntToByte
//
procedure THeightData.ConvertSmallIntToByte;
var
  i: Integer;
begin
  FreeMem(FSmallIntRaster);
  FSmallIntRaster := nil;
  FByteData := Pointer(FSmallIntData);
  for i := 0 to Size * Size - 1 do
    FByteData^[i] := (FSmallIntData^[i] div 128) + 128;
  FDataSize := Size * Size * SizeOf(Byte);
  ReallocMem(FByteData, FDataSize);
  FSmallIntData := nil;
  BuildByteRaster;
end;

// ConvertSmallIntToSingle
//
procedure THeightData.ConvertSmallIntToSingle;
var
  i: Integer;
begin
  FreeMem(FSmallIntRaster);
  FSmallIntRaster := nil;
  FDataSize := Size * Size * SizeOf(Single);
  GetMem(FSingleData, FDataSize);
  for i := 0 to Size * Size - 1 do
    FSingleData^[i] := FSmallIntData^[i];
  FreeMem(FSmallIntData);
  FSmallIntData := nil;
  BuildSingleRaster;
end;

// ConvertSingleToByte
//
procedure THeightData.ConvertSingleToByte;
var
  i: Integer;
begin
  FreeMem(FSingleRaster);
  FSingleRaster := nil;
  FByteData := Pointer(FSingleData);
  for i := 0 to Size * Size - 1 do
    FByteData^[i] := (Round(FSingleData^[i]) div 128) + 128;
  FDataSize := Size * Size * SizeOf(Byte);
  ReallocMem(FByteData, FDataSize);
  FSingleData := nil;
  BuildByteRaster;
end;

// ConvertSingleToSmallInt
//
procedure THeightData.ConvertSingleToSmallInt;
var
  i: Integer;
begin
  FreeMem(FSingleRaster);
  FSingleRaster := nil;
  FSmallIntData := Pointer(FSingleData);
  for i := 0 to Size * Size - 1 do
    FSmallIntData^[i] := Round(FSingleData^[i]);
  FDataSize := Size * Size * SizeOf(SmallInt);
  ReallocMem(FSmallIntData, FDataSize);
  FSingleData := nil;
  BuildSmallIntRaster;
end;

// ByteHeight
//
function THeightData.ByteHeight(x, y: Integer): Byte;
begin
  Assert((Cardinal(x) < Cardinal(Size)) and (Cardinal(y) < Cardinal(Size)));
  Result := ByteRaster^[y]^[x];
end;

// SmallIntHeight
//
function THeightData.SmallIntHeight(x, y: Integer): SmallInt;
begin
  Assert((Cardinal(x) < Cardinal(Size)) and (Cardinal(y) < Cardinal(Size)));
  Result := SmallIntRaster^[y]^[x];
end;

// SingleHeight
//
function THeightData.SingleHeight(x, y: Integer): Single;
begin
  Assert((Cardinal(x) < Cardinal(Size)) and (Cardinal(y) < Cardinal(Size)));
  Result := SingleRaster^[y]^[x];
end;

// InterpolatedHeight
//
function THeightData.InterpolatedHeight(x, y: Single): Single;
var
  ix, iy, ixn, iyn: Integer;
  h1, h2, h3: Single;
begin
  if FDataState = hdsNone then
    Result := 0
  else
  begin
    ix := Trunc(x);
    x := Frac(x);
    iy := Trunc(y);
    y := Frac(y);
    ixn := ix + 1;
    if ixn >= Size then
      ixn := ix;
    iyn := iy + 1;
    if iyn >= Size then
      iyn := iy;
    if x > y then
    begin
      // top-right triangle
      h1 := Height(ixn, iy);
      h2 := Height(ix, iy);
      h3 := Height(ixn, iyn);
      Result := h1 + (h2 - h1) * (1 - x) + (h3 - h1) * y;
    end
    else
    begin
      // bottom-left triangle
      h1 := Height(ix, iyn);
      h2 := Height(ixn, iyn);
      h3 := Height(ix, iy);
      Result := h1 + (h2 - h1) * (x) + (h3 - h1) * (1 - y);
    end;
  end;
end;

// Height
//
function THeightData.Height(x, y: Integer): Single;
begin
  case DataType of
    hdtByte:
      Result := (ByteHeight(x, y) - 128) shl 7;
    hdtSmallInt:
      Result := SmallIntHeight(x, y);
    hdtSingle:
      Result := SingleHeight(x, y);
  else
    Result := 0;
    Assert(false);
  end;
end;

// GetHeightMin
//
function THeightData.GetHeightMin: Single;
var
  i: Integer;
  b: Byte;
  sm: SmallInt;
  si: Single;
begin
  if FHeightMin = 1E30 then
  begin
    if DataState = hdsReady then
    begin
      case DataType of
        hdtByte:
          begin
            b := FByteData^[0];
            for i := 1 to Size * Size - 1 do
              if FByteData^[i] < b then
                b := FByteData^[i];
            FHeightMin := ((Integer(b) - 128) shl 7);
          end;
        hdtSmallInt:
          begin
            sm := FSmallIntData^[0];
            for i := 1 to Size * Size - 1 do
              if FSmallIntData^[i] < sm then
                sm := FSmallIntData^[i];
            FHeightMin := sm;
          end;
        hdtSingle:
          begin
            si := FSingleData^[0];
            for i := 1 to Size * Size - 1 do
              if FSingleData^[i] < si then
                si := FSingleData^[i];
            FHeightMin := si;
          end;
      else
        FHeightMin := 0;
      end;
    end
    else
      FHeightMin := 0;
  end;
  Result := FHeightMin;
end;

// GetHeightMax
//
function THeightData.GetHeightMax: Single;
var
  i: Integer;
  b: Byte;
  sm: SmallInt;
  si: Single;
begin
  if FHeightMax = 1E30 then
  begin
    if DataState = hdsReady then
    begin
      case DataType of
        hdtByte:
          begin
            b := FByteData^[0];
            for i := 1 to Size * Size - 1 do
              if FByteData^[i] > b then
                b := FByteData^[i];
            FHeightMax := ((Integer(b) - 128) shl 7);
          end;
        hdtSmallInt:
          begin
            sm := FSmallIntData^[0];
            for i := 1 to Size * Size - 1 do
              if FSmallIntData^[i] > sm then
                sm := FSmallIntData^[i];
            FHeightMax := sm;
          end;
        hdtSingle:
          begin
            si := FSingleData^[0];
            for i := 1 to Size * Size - 1 do
              if FSingleData^[i] > si then
                si := FSingleData^[i];
            FHeightMax := si;
          end;
      else
        FHeightMax := 0;
      end;
    end
    else
      FHeightMax := 0;
  end;
  Result := FHeightMax;
end;

// Normal
//
// Calculates the normal at a vertex
function THeightData.Normal(x, y: Integer; const scale: TAffineVector)
  : TAffineVector;
var
  dx, dy: Single;
begin
  if x > 0 then
    if x < Size - 1 then
      dx := (Height(x + 1, y) - Height(x - 1, y))
    else
      dx := (Height(x, y) - Height(x - 1, y))
  else
    dx := (Height(x + 1, y) - Height(x, y));
  if y > 0 then
    if y < Size - 1 then
      dy := (Height(x, y + 1) - Height(x, y - 1))
    else
      dy := (Height(x, y) - Height(x, y - 1))
  else
    dy := (Height(x, y + 1) - Height(x, y));
  Result.V[0] := dx * scale.V[1] * scale.V[2];
  Result.V[1] := dy * scale.V[0] * scale.V[2];
  Result.V[2] := scale.V[0] * scale.V[1];
  NormalizeVector(Result);
end;

// NormalNode
//
// Calculates the normal at a surface cell (Between vertexes)
function THeightData.NormalAtNode(x, y: Integer; const scale: TAffineVector)
  : TAffineVector;
var
  dx, dy, Hxy: Single;
begin
  MinInteger(MaxInteger(x, 0), Size - 2); // clamp x to 0 -> Size-2
  MinInteger(MaxInteger(y, 0), Size - 2); // clamp x to 0 -> Size-2
  Hxy := Height(x, y);
  dx := Height(x + 1, y) - Hxy;
  dy := Height(x, y + 1) - Hxy;
  Result.V[0] := dx * scale.V[1] * scale.V[2]; // Result[0]:=dx/scale[0];
  Result.V[1] := dy * scale.V[0] * scale.V[2]; // Result[1]:=dy/scale[1];
  Result.V[2] := 1 * scale.V[0] * scale.V[1]; // Result[2]:=1 /scale[2];
  NormalizeVector(Result);
end;

// OverlapsArea
//
function THeightData.OverlapsArea(const Area: TGLRect): boolean;
begin
  Result := (XLeft <= Area.Right) and (YTop <= Area.Bottom) and
    (XLeft + Size > Area.Left) and (YTop + Size > Area.Top);
end;

// ------------------
// ------------------ THeightDataThread ------------------
// ------------------

// Destroy
//
destructor THeightDataThread.Destroy;
begin
  if Assigned(FHeightData) then
    FHeightData.FThread := nil;
  inherited;
end;

// ------------------
// ------------------ TGLBitmapHDS ------------------
// ------------------

// Create
//
constructor TGLBitmapHDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TGLPicture.Create;
  FPicture.OnChange := OnPictureChanged;
  FInfiniteWrap := True;
  FInverted := True;
end;

// Destroy
//
destructor TGLBitmapHDS.Destroy;
begin
  inherited Destroy;
  FreeMonochromeBitmap;
  FPicture.Free;
end;

// SetPicture
//
procedure TGLBitmapHDS.SetPicture(const val: TGLPicture);
begin
  FPicture.Assign(val);
end;

// OnPictureChanged
//
procedure TGLBitmapHDS.OnPictureChanged(sender: TObject);
var
  oldPoolSize, Size: Integer;
begin
  // cleanup pool
  oldPoolSize := MaxPoolSize;
  MaxPoolSize := 0;
  CleanUp;
  MaxPoolSize := oldPoolSize;
  // prepare MonoChromeBitmap
  FreeMonochromeBitmap;
  Size := Picture.Width;
  if Size > 0 then
    CreateMonochromeBitmap(Size);
end;

// SetInfiniteWrap
//
procedure TGLBitmapHDS.SetInfiniteWrap(val: boolean);
begin
  if FInfiniteWrap <> val then
  begin
    FInfiniteWrap := val;
{$IFDEF GLS_DELPHI_4}
    inherited MarkDirty;
{$ELSE}
    MarkDirty;
{$ENDIF}
  end;
end;

// SetInverted
//
procedure TGLBitmapHDS.SetInverted(val: boolean);
begin
  if FInverted = val then
    Exit;
  FInverted := val;
{$IFDEF GLS_DELPHI_4}
  inherited MarkDirty;
{$ELSE}
  MarkDirty;
{$ENDIF}
end;

// MarkDirty
//
procedure TGLBitmapHDS.MarkDirty(const Area: TGLRect);
begin
  inherited;
  FreeMonochromeBitmap;
  if Picture.Width > 0 then
    CreateMonochromeBitmap(Picture.Width);
end;

// CreateMonochromeBitmap
//
procedure TGLBitmapHDS.CreateMonochromeBitmap(Size: Integer);
{$IFDEF MSWINDOWS}
type
  TPaletteEntryArray = array [0 .. 255] of TPaletteEntry;
  PPaletteEntryArray = ^TPaletteEntryArray;

  TLogPal = record
    lpal: TLogPalette;
    pe: TPaletteEntryArray;
  end;

var
  x: Integer;
  logpal: TLogPal;
  hPal: HPalette;
begin
  Size := RoundUpToPowerOf2(Size);
  FBitmap := TGLBitmap.Create;
  FBitmap.PixelFormat := glpf8bit;
  FBitmap.Width := Size;
  FBitmap.Height := Size;
  for x := 0 to 255 do
    with PPaletteEntryArray(@logpal.lpal.palPalEntry[0])[x] do
    begin
      peRed := x;
      peGreen := x;
      peBlue := x;
      peFlags := 0;
    end;
  with logpal.lpal do
  begin
    palVersion := $300;
    palNumEntries := 256;
  end;
  hPal := CreatePalette(logpal.lpal);
  Assert(hPal <> 0);
  FBitmap.Palette := hPal;
  // some picture formats trigger a "change" when drawed
  Picture.OnChange := nil;
  try
    FBitmap.Canvas.StretchDraw(Classes.Rect(0, 0, Size, Size), Picture.Graphic);
  finally
    Picture.OnChange := OnPictureChanged;
  end;
{$IFDEF FPC}
  IntfImg1 := TLazIntfImage.Create(0, 0);
  IntfImg1.LoadFromBitmap(FBitmap.Handle, FBitmap.MaskHandle);
{$ENDIF}
  SetLength(FScanLineCache, 0); // clear the cache
  SetLength(FScanLineCache, Size);
end;
{$ENDIF}
{$IFDEF UNIX}
begin
{$MESSAGE Warn 'CreateMonochromeBitmap: Needs to be implemented'}
end;
{$ENDIF}

// FreeMonochromeBitmap
//
procedure TGLBitmapHDS.FreeMonochromeBitmap;
begin
  SetLength(FScanLineCache, 0);
  FBitmap.Free;
  FBitmap := nil;
{$IFDEF FPC}
  IntfImg1.Free;
  IntfImg1 := nil;
{$ENDIF}
end;

// GetScanLine
//
function TGLBitmapHDS.GetScanLine(y: Integer): PByteArray;
begin
  Result := FScanLineCache[y];
  if not Assigned(Result) then
  begin
{$IFNDEF FPC}
    Result := BitmapScanLine(FBitmap, y); // FBitmap.ScanLine[y];
{$ELSE}
    Result := IntfImg1.GetDataLineStart(y);
{$ENDIF}
    FScanLineCache[y] := Result;
  end;
end;

// StartPreparingData
//
procedure TGLBitmapHDS.StartPreparingData(HeightData: THeightData);
var
  y, x: Integer;
  bmpSize, wrapMask: Integer;
  bitmapLine, rasterLine: PByteArray;
  oldType: THeightDataType;
  b: Byte;
  YPos: Integer;
begin
  if FBitmap = nil then
    Exit;
  HeightData.FDataState := hdsPreparing;
  bmpSize := FBitmap.Width;
  wrapMask := bmpSize - 1;
  // retrieve data
  with HeightData do
  begin
    if (not InfiniteWrap) and ((XLeft >= bmpSize) or (XLeft < 0) or
      (YTop >= bmpSize) or (YTop < 0)) then
    begin
      HeightData.FDataState := hdsNone;
      Exit;
    end;
    oldType := DataType;
    Allocate(hdtByte);
    if Inverted then
      YPos := YTop
    else
      YPos := 1 - Size - YTop;
    for y := 0 to Size - 1 do
    begin
      bitmapLine := GetScanLine((y + YPos) and wrapMask);
      if Inverted then
        rasterLine := ByteRaster^[y]
      else
        rasterLine := ByteRaster^[Size - 1 - y];
      // *BIG CAUTION HERE* : Don't remove the intermediate variable here!!!
      // or Delphi compiler will "optimize" to 32 bits access with clamping
      // Resulting in possible reads of stuff beyon bitmapLine length!!!!
      for x := XLeft to XLeft + Size - 1 do
      begin
        b := bitmapLine^[x and wrapMask];
        rasterLine^[x - XLeft] := b;
      end;
    end;
    if (oldType <> hdtByte) and (oldType <> hdtDefault) then
      DataType := oldType;
  end;
  TextureCoordinates(HeightData);
  inherited;
end;

function TGLBitmapHDS.Width: Integer;
begin
  if Assigned(self.FBitmap) then
    Result := self.FBitmap.Width
  else
    Result := 0;
end;

function TGLBitmapHDS.Height: Integer;
begin
  if Assigned(self.FBitmap) then
    Result := self.FBitmap.Height
  else
    Result := 0;
end;


// ------------------
// ------------------ TGLCustomHDS ------------------
// ------------------

// Create
//
constructor TGLCustomHDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

// Destroy
//
destructor TGLCustomHDS.Destroy;
begin
  inherited Destroy;
end;

// MarkDirty
//
procedure TGLCustomHDS.MarkDirty(const Area: TGLRect);
begin
  inherited;
  if Assigned(FOnMarkDirty) then
    FOnMarkDirty(Area);
end;

// StartPreparingData
//
procedure TGLCustomHDS.StartPreparingData(HeightData: THeightData);
begin
  if Assigned(FOnStartPreparingData) then
    FOnStartPreparingData(HeightData);
  if HeightData.DataState <> hdsNone then
    HeightData.DataState := hdsReady;
end;

// ------------------
// ------------------ TGLTerrainBaseHDS ------------------
// ------------------

// Create
//
constructor TGLTerrainBaseHDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

// Destroy
//
destructor TGLTerrainBaseHDS.Destroy;
begin
  inherited Destroy;
end;

// StartPreparingData
//
procedure TGLTerrainBaseHDS.StartPreparingData(HeightData: THeightData);
const
  cTBWidth: Integer = 4320;
  cTBHeight: Integer = 2160;
var
  y, x, offset: Integer;
  rasterLine: PSmallIntArray;
  oldType: THeightDataType;
  b: SmallInt;
  fs: TStream;
begin
  if not FileExists('tbase.bin') then
    Exit;
  fs := CreateFileStream('tbase.bin', fmOpenRead + fmShareDenyNone);
  try
    // retrieve data
    with HeightData do
    begin
      oldType := DataType;
      Allocate(hdtSmallInt);
      for y := YTop to YTop + Size - 1 do
      begin
        offset := (y mod cTBHeight) * (cTBWidth * 2);
        rasterLine := SmallIntRaster^[y - YTop];
        for x := XLeft to XLeft + Size - 1 do
        begin
          fs.Seek(offset + (x mod cTBWidth) * 2, soFromBeginning);
          fs.Read(b, 2);
          if b < 0 then
            b := 0;
          rasterLine^[x - XLeft] := SmallInt(b);
        end;
      end;
      if oldType <> hdtSmallInt then
        DataType := oldType;
    end;
    inherited;
  finally
    fs.Free;
  end;
end;


// ------------------
// ------------------ THeightDataSourceFilter ------------------
// ------------------

constructor THeightDataSourceFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
end;

// Destroy
//
destructor THeightDataSourceFilter.Destroy;
begin
  HeightDataSource := nil;
  inherited Destroy;
end;

procedure THeightDataSourceFilter.Release(aHeightData: THeightData);
begin
  if Assigned(HeightDataSource) then
    HeightDataSource.Release(aHeightData);
end;

// Notification
//
procedure THeightDataSourceFilter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FHDS then
      HeightDataSource := nil
  end;
  inherited;
end;

// SetHDS  - Set HeightDataSource property
//
procedure THeightDataSourceFilter.SetHDS(val: THeightDataSource);
begin
  if val = self then
    val := nil; // prevent self-referencing
  if val <> FHDS then
  begin
    if Assigned(FHDS) then
      FHDS.RemoveFreeNotification(self);
    FHDS := val;
    if Assigned(FHDS) then
      FHDS.FreeNotification(self);
    // MarkDirty;
    self.Clear; // when removing the HDS, also remove all tiles from the cache
  end;
end;

function THeightDataSourceFilter.Width: Integer;
begin
  if Assigned(FHDS) then
    Result := FHDS.Width
  else
    Result := 0;
end;

function THeightDataSourceFilter.Height: Integer;
begin
  if Assigned(FHDS) then
    Result := FHDS.Height
  else
    Result := 0;
end;

procedure THeightDataSourceFilter.StartPreparingData(HeightData: THeightData);
begin
  // ---if there is no linked HDS then return an empty tile--
  if not Assigned(FHDS) then
  begin
    HeightData.Owner.Data.LockList;
    HeightData.DataState := hdsNone;
    HeightData.Owner.Data.UnlockList;
    Exit;
  end;
  // ---Use linked HeightDataSource to prepare height data--
  if HeightData.DataState = hdsQueued then
  begin
    HeightData.Owner.Data.LockList;
    HeightData.DataState := hdsPreparing;
    HeightData.Owner.Data.UnlockList;
  end;
  FHDS.StartPreparingData(HeightData);
  if Assigned(FOnSourceDataFetched) then
    FOnSourceDataFetched(self, HeightData);
  if HeightData.DataState = hdsNone then
    Exit;
  if FActive then
    PreparingData(HeightData);
  inherited; // HeightData.DataState:=hdsReady;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// class registrations
Classes.RegisterClasses([TGLBitmapHDS, TGLCustomHDS, THeightDataSourceFilter]);

end.
