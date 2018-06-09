//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  A Newton Game Dynamics Manager for GLScene.

  Where can I find ... ? 
   GLScene                                   (http://glscene.org)
   Newton Game Dynamics Engine               (http://newtondynamics.com)
   NewtonImport, a Delphi header translation (http://newtondynamics.com/forum/viewtopic.php?f=9&t=5273#p35865)
   

  Notes:
  This code is still being developed so any part of it may change at anytime.
  To install use the GLS_NGD?.dpk in the GLScene/Delphi? folder.

   History :  
   10/11/12 - PW - Added CPP compatibility: used records with arrays instead of vector arrays
   11/17/12 - YP - Check not nil result with GetBodyFromGLSceneObject
                      FreeAndNil when destroying objects
                      Destroy all relative joints when finalizing a behaviour to avoid random crash
                      Smart GetBBoxCollision
                      DestroyNewtonData is now common for all procedures
   28/06/12 - YP - Updated to newton 2.36 (no api change with 2.35)
   02/02/11 - FP - Read/Write to Filer update to version 1
                 Use RWFloat instead of RWSingle for Single for lazarus compatibility
   02/02/11 - FP - Add initial name for behavior
                 Moved two TNGDSurfacePair properties from published to public for Lazarus
   01/02/11 - FP - Fixed custom hinge DegToRad limit
                 Update newtoncreatebody API with matrix parameter (since newton 2.28)
                 Joint draw [parent-to-pivot-to-child] instead of [parent-to-child]
   21/01/11 - FP - Huge update: Joint in manager collection. Material (now surface) in manager collection
                  Callback as static class function now raise events
                  Debugs view use TGLLines instead of TGLRenderPoint
                  Reset filer version to zero
   16/12/10 - FP - Update to NewtonSDK 2.25-2.26
   19/11/10 - FP - Fixed FAngularDamping memory leak for TGLNGDDynamic
   19/11/10 - FP - Added UseGravity property for TGLNGDDynamic
   05/11/10 - FP - Removed check freeform in TGLNGDStatic.GeTree
                  Removed FCollisionArray from TGLNGDBehaviour
                  Modified misspelling usevelovity to usevelocity [thx bobrob69]
                  Moved Creation of compound collision for freeform from GetCollisionFromBaseSceneObject to SetCollision for TGLNGDDynamic [thx bobrob69]
   25/10/10 - FP - Fixed Material badly loaded when created in design time
   25/10/10 - FP - Commented 'Release each collision form the array' in TGLNGDBehaviour.SetCollision.
                  Changed angular friction in  TGLNGDDynamic.Pick method to be able to pick body with small mass.
                  Added Beta Serialize and Deserialise for TGLNGDBehaviour.
                  Commented 'rebuild in runtime' in TGLNGDStatic.Render, because this is conflicting with news serialize methods
   23/10/10 - Yar - Replace OpenGL1x to OpenGLAdapter
   08/10/10 - FP - Added show contact for dynamic in render.
                 Uncommented ShowContact property in manager.
   07/10/10 - FP - Joints connected to TGLNGDBehaviour are now freed in TGLNGDBehaviour.Destroy
   30/09/10 - FP - Removed beta functions of player and car in TGLNGDDynamic.
                 Added AddImpulse function in TGLNGDDynamic.
   29/09/10 - FP - Moved FManager assignation for MaterialPair from loaded to create
   21/09/10 - FP - Added timestep in TContactProcessEvent.
                  Removed Manager property of MaterialPair.
                  MaterialPair.loaded use the owner.owner component as manager now.
                  MaterialPair FilerVersion up to 1
   20/09/10 - FP - Call Finalize/Initialize in Setid
   20/09/10 - YP - Moved MaterialAutoCreateGroupID call into Material.Initialize
   19/09/10 - YP - Added MaterialAutoCreateGroupID to fix loaded order
   18/09/10 - YP - Added Get and GetOrCreate NGD behaviors routine
   15/07/10 - FP - Creation by Franck Papouin
   
}

unit GLNGDManager;

interface

{$I GLScene.inc}

uses
  Classes, // TComponent Tlist TWriter TReader TPersistent
  SysUtils, //System utilities
  Math, // Samevalue isZero to compare single
  NewtonImport, NewtonImport_JointLibrary, // Newton
  GLVectorGeometry, // PVector TVector TMatrix PMatrix NullHmgVector...
  GLVectorLists, // TaffineVectorList for Tree
  GLXCollection,   TGLXCollection file function
  GLBaseClasses, GLScene, GLManager, GLCrossPlatform, GLCoordinates, //
  GLObjects, GLGeomObjects, GLVectorFileObjects, // cube cone freeform...
  GLColor, GLGeometryBB; // For show debug

type

  NGDFloat = NewtonImport.Float;
  PNGDFloat = ^NGDFloat;

  { Record }
  THeightField = record
    heightArray: array of Word;
    width: Integer;
    depth: Integer;
    gridDiagonals: Boolean;
    widthDepthScale: Single;
    heightScale: Single;
  end;

  { Class }
  TGLNGDBehaviour = class;
  TGLNGDManager = class;
  TNGDSurfaceItem = class;
  TNGDJoint = class;

  { Enums }
  TNGDSolverModels = (smExact = 0, smLinear1, smLinear2, smLinear3, smLinear4,
    smLinear5, smLinear6, smLinear7, smLinear8, smLinear9);

  TNGDFrictionModels = (fmExact = 0, fmAdaptive);
  TNGDPickedActions = (paAttach = 0, paMove, paDetach);

  TNGDManagerDebug = (mdShowGeometry, mdShowAABB, mdShowCenterOfMass,
    mdShowContact, mdShowJoint, mdShowForce, mdShowAppliedForce,
    mdShowAppliedVelocity);
  TNGDManagerDebugs = set of TNGDManagerDebug;

  TNGDNewtonCollisions = (nc_Primitive = 0, nc_Convex, nc_BBox, nc_BSphere,
    nc_Tree, nc_Mesh, nc_Null, nc_HeightField, nc_NGDFile);

  TNGDNewtonJoints = (nj_BallAndSocket, nj_Hinge, nj_Slider, nj_Corkscrew,
    nj_Universal, nj_CustomBallAndSocket, nj_CustomHinge, nj_CustomSlider,
    nj_UpVector, nj_KinematicController);

  TGLNGDBehaviourList = class(TList)
  protected
    function GetBehav(index: Integer): TGLNGDBehaviour;
    procedure PutBehav(index: Integer; Item: TGLNGDBehaviour);
  public
    property ItemsBehav[index: Integer]
      : TGLNGDBehaviour read GetBehav write PutBehav; default;
  end;

  { Events for Newton Callback }

  TCollisionIteratorEvent = procedure(const userData: Pointer;
    vertexCount: Integer; const cfaceArray: PNGDFloat;
    faceId: Integer) of object;

  TApplyForceAndTorqueEvent = procedure(const cbody: PNewtonBody;
    timestep: NGDFloat; threadIndex: Integer) of object;

  TSetTransformEvent = procedure(const cbody: PNewtonBody;
    const cmatrix: PNGDFloat; threadIndex: Integer) of object;

  TSerializeEvent = procedure(serializeHandle: Pointer; const cbuffer: Pointer;
    size: Cardinal) of object;

  TDeSerializeEvent = procedure(serializeHandle: Pointer; buffer: Pointer;
    size: Cardinal) of object;

  TAABBOverlapEvent = function(const cmaterial: PNewtonMaterial;
    const cbody0: PNewtonBody; const cbody1: PNewtonBody;
    threadIndex: Integer): Boolean of object;

  TContactProcessEvent = procedure(const ccontact: PNewtonJoint;
    timestep: NGDFloat; threadIndex: Integer) of object;

  { Class }

  TNGDDebugOption = class(TPersistent)
  strict private
    FManager: TGLNGDManager;
    FGeomColorDyn: TGLColor; // Green
    FGeomColorStat: TGLColor; // Red
    FAABBColor: TGLColor; // Yellow
    FAABBColorSleep: TGLColor; // Orange
    FCenterOfMassColor: TGLColor; // Purple dot
    FContactColor: TGLColor; // White
    FJointAxisColor: TGLColor; // Blue
    FJointPivotColor: TGLColor; // Aquamarine
    FForceColor: TGLColor; // Black
    FAppliedForceColor: TGLColor; // Silver
    FAppliedVelocityColor: TGLColor; // Lime
    FCustomColor: TGLColor; // Aqua
    FDotAxisSize: Single; // 1
    FNGDManagerDebugs: TNGDManagerDebugs; // Default All false
    procedure SetNGDManagerDebugs(const Value: TNGDManagerDebugs);
    procedure SetDotAxisSize(const Value: Single);
    function StoredDotAxis: Boolean;

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

  published
    property GeomColorDyn: TGLColor read FGeomColorDyn write FGeomColorDyn;
    property GeomColorStat: TGLColor read FGeomColorStat write FGeomColorStat;
    property AABBColor: TGLColor read FAABBColor write FAABBColor;
    property AABBColorSleep
      : TGLColor read FAABBColorSleep write FAABBColorSleep;
    property CenterOfMassColor
      : TGLColor read FCenterOfMassColor write FCenterOfMassColor;
    property ContactColor: TGLColor read FContactColor write FContactColor;
    property JointAxisColor
      : TGLColor read FJointAxisColor write FJointAxisColor;
    property JointPivotColor
      : TGLColor read FJointPivotColor write FJointPivotColor;
    property ForceColor: TGLColor read FForceColor write FForceColor;
    property AppliedForceColor
      : TGLColor read FAppliedForceColor write FAppliedForceColor;
    property AppliedVelocityColor
      : TGLColor read FAppliedVelocityColor write FAppliedVelocityColor;
    property CustomColor: TGLColor read FCustomColor write FCustomColor;
    property NGDManagerDebugs: TNGDManagerDebugs read FNGDManagerDebugs write
      SetNGDManagerDebugs default[];
    property DotAxisSize: Single read FDotAxisSize write SetDotAxisSize stored
      StoredDotAxis;
  end;

  TGLNGDManager = class(TComponent)

  strict private
     
    FVisible: Boolean; // Show Debug at design time
    FVisibleAtRunTime: Boolean; // Show Debug at run time
    FDllVersion: Integer;
    FSolverModel: TNGDSolverModels; // Default=Exact
    FFrictionModel: TNGDFrictionModels; // Default=Exact
    FMinimumFrameRate: Integer; // Default=60
    FWorldSizeMin: TGLCoordinates; // Default=-100, -100, -100
    FWorldSizeMax: TGLCoordinates; // Default=100, 100, 100
    FThreadCount: Integer; // Default=1
    FGravity: TGLCoordinates; // Default=(0,-9.81,0)
    FNewtonSurfaceItem: TCollection;
    FNewtonSurfacePair: TOwnedCollection;
    FNewtonJointGroup: TOwnedCollection;
    FNGDDebugOption: TNGDDebugOption;
    FGLLines: TGLLines;

  private
    FNewtonWorld: PNewtonWorld;
    FNGDBehaviours: TGLNGDBehaviourList;
    FCurrentColor: TGLColor;
  protected
    procedure Loaded; override;
    procedure SetVisible(const Value: Boolean);
    procedure SetVisibleAtRunTime(const Value: Boolean);
    procedure SetSolverModel(const Value: TNGDSolverModels);
    procedure SetFrictionModel(const Value: TNGDFrictionModels);
    procedure SetMinimumFrameRate(const Value: Integer);
    procedure SetThreadCount(const Value: Integer);
    procedure SetGLLines(const Value: TGLLines);
    function GetBodyCount: Integer;
    function GetConstraintCount: Integer;
    procedure AddNode(const coords: TGLCustomCoordinates); overload;
    procedure AddNode(const X, Y, Z: Single); overload;
    procedure AddNode(const Value: TVector); overload;
    procedure AddNode(const Value: TAffineVector); overload;
    procedure RebuildAllMaterial;
    procedure RebuildAllJoint(Sender: TObject);

    // Events
    procedure NotifyWorldSizeChange(Sender: TObject);
    procedure NotifyChange(Sender: TObject); // Debug view

  public
     
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Step(deltatime: Single);

  published
     

    property Visible: Boolean read FVisible write SetVisible default True;
    property VisibleAtRunTime: Boolean read FVisibleAtRunTime write
      SetVisibleAtRunTime default False;
    property SolverModel: TNGDSolverModels read FSolverModel write
      SetSolverModel default smExact;
    property FrictionModel: TNGDFrictionModels read FFrictionModel write
      SetFrictionModel default fmExact;
    property MinimumFrameRate: Integer read FMinimumFrameRate write
      SetMinimumFrameRate default 60;
    property ThreadCount
      : Integer read FThreadCount write SetThreadCount default 1;
    property DllVersion: Integer read FDllVersion;
    property NewtonBodyCount: Integer read GetBodyCount;
    property NewtonConstraintCount: Integer read GetConstraintCount;
    property Gravity: TGLCoordinates read FGravity write FGravity;
    property WorldSizeMin
      : TGLCoordinates read FWorldSizeMin write FWorldSizeMin;
    property WorldSizeMax
      : TGLCoordinates read FWorldSizeMax write FWorldSizeMax;
    property NewtonSurfaceItem
      : TCollection read FNewtonSurfaceItem write FNewtonSurfaceItem;
    property NewtonSurfacePair: TOwnedCollection read FNewtonSurfacePair write
      FNewtonSurfacePair;
    property DebugOption: TNGDDebugOption read FNGDDebugOption write
      FNGDDebugOption;
    property Line: TGLLines read FGLLines write SetGLLines;
    property NewtonJoint: TOwnedCollection read FNewtonJointGroup write
      FNewtonJointGroup;
  end;

  { Basis structures for GLScene behaviour style implementations. }
  TGLNGDBehaviour = class(TGLBehaviour)
  private
    { Private Declartions }
    FManager: TGLNGDManager;
    FManagerName: string;
    FInitialized: Boolean;
    FNewtonBody: PNewtonBody;
    FCollision: PNewtonCollision;
    FNewtonBodyMatrix: TMatrix; // Position and Orientation
    FContinuousCollisionMode: Boolean; // Default=False
    FNGDNewtonCollisions: TNGDNewtonCollisions;
    FCollisionIteratorEvent: TCollisionIteratorEvent;
    FOwnerBaseSceneObject: TGLBaseSceneObject;
    // FNullCollisionMass: Single; // Default=0
    FTreeCollisionOptimize: Boolean; // Default=True
    FConvexCollisionTolerance: Single; // Default=0.01 1%
    FFileCollision: string;
    FNGDSurfaceItem: TNGDSurfaceItem;
    FHeightFieldOptions: THeightField;
  protected
     
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    procedure SetManager(Value: TGLNGDManager);
    procedure SetNewtonBodyMatrix(const Value: TMatrix);
    procedure SetContinuousCollisionMode(const Value: Boolean);
    function GetNewtonBodyMatrix: TMatrix;
    function GetNewtonBodyAABB: TAABB;
    procedure UpdCollision; virtual;
    procedure Render; virtual;
    procedure SetNGDNewtonCollisions(const Value: TNGDNewtonCollisions);
    procedure SetNGDSurfaceItem(const Value: TNGDSurfaceItem);
    procedure SetHeightFieldOptions(const Value: THeightField);

    function GetPrimitiveCollision(): PNewtonCollision;
    function GetConvexCollision(): PNewtonCollision;
    function GetBBoxCollision(): PNewtonCollision;
    function GetBSphereCollision(): PNewtonCollision;
    function GetTreeCollision(): PNewtonCollision;
    function GetMeshCollision(): PNewtonCollision;
    function GetNullCollision(): PNewtonCollision;
    function GetHeightFieldCollision(): PNewtonCollision;
    function GetNGDFileCollision(): PNewtonCollision;
    function StoredTolerance: Boolean;

    // Event
    procedure OnCollisionIteratorEvent(const userData: Pointer;
      vertexCount: Integer; const cfaceArray: PNGDFloat; faceId: Integer);

    // CallBack
    class procedure NewtonCollisionIterator(const userData: Pointer;
      vertexCount: Integer; const faceArray: PNGDFloat;
      faceId: Integer); static; cdecl;

    class procedure NewtonSerialize(serializeHandle: Pointer;
      const buffer: Pointer; size: Cardinal); static; cdecl;

    class procedure NewtonDeserialize(serializeHandle: Pointer;
      buffer: Pointer; size: Cardinal); static; cdecl;

  public
     
    constructor Create(AOwner: TGLXCollection); override;
    destructor Destroy; override;
    procedure Reinitialize;
    property Initialized: Boolean read FInitialized;
    class function UniqueItem: Boolean; override;
    property NewtonBodyMatrix: TMatrix read GetNewtonBodyMatrix write
      SetNewtonBodyMatrix;
    property NewtonBodyAABB: TAABB read GetNewtonBodyAABB;
    procedure Serialize(filename: string);
    procedure DeSerialize(filename: string);
    property HeightFieldOptions: THeightField read FHeightFieldOptions write
      SetHeightFieldOptions;

  published
     
    property Manager: TGLNGDManager read FManager write SetManager;
    property ContinuousCollisionMode
      : Boolean read FContinuousCollisionMode write
      SetContinuousCollisionMode default False;
    property NGDNewtonCollisions
      : TNGDNewtonCollisions read FNGDNewtonCollisions
      write SetNGDNewtonCollisions default nc_Primitive;
    property TreeCollisionOptimize: Boolean read FTreeCollisionOptimize write
      FTreeCollisionOptimize default True;
    property ConvexCollisionTolerance
      : Single read FConvexCollisionTolerance write
      FConvexCollisionTolerance stored StoredTolerance;
    property FileCollision: string read FFileCollision write FFileCollision;
    property NGDSurfaceItem: TNGDSurfaceItem read FNGDSurfaceItem write
      SetNGDSurfaceItem;
  end;

  TGLNGDDynamic = class(TGLNGDBehaviour)
  strict private
     
    FAABBmin: TGLCoordinates;
    FAABBmax: TGLCoordinates;
    FForce: TGLCoordinates;
    FTorque: TGLCoordinates;
    FCenterOfMass: TGLCoordinates;
    FAutoSleep: Boolean; // Default=True
    FLinearDamping: Single; // default=0.1
    FAngularDamping: TGLCoordinates; // Default=0.1
    FDensity: Single; // Default=1
    FUseGravity: Boolean; // Default=True
    FNullCollisionVolume: Single; // Default=0
    FApplyForceAndTorqueEvent: TApplyForceAndTorqueEvent;
    FSetTransformEvent: TSetTransformEvent;
    FCustomForceAndTorqueEvent: TApplyForceAndTorqueEvent;

    // Read Only
    FVolume: Single;
    FMass: Single;
    FAppliedForce: TGLCoordinates;
    FAppliedTorque: TGLCoordinates;
    FAppliedOmega: TGLCoordinates;
    FAppliedVelocity: TGLCoordinates;

    function StoredDensity: Boolean;
    function StoredLinearDamping: Boolean;
    function StoredNullCollisionVolume: Boolean;
  protected
     
    procedure SetAutoSleep(const Value: Boolean);
    procedure SetLinearDamping(const Value: Single);
    procedure SetDensity(const Value: Single); virtual;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    procedure Render; override;

    // Events
    procedure NotifyCenterOfMassChange(Sender: TObject);
    procedure NotifyAngularDampingChange(Sender: TObject);
    procedure OnApplyForceAndTorqueEvent(const cbody: PNewtonBody;
      timestep: NGDFloat; threadIndex: Integer);
    procedure OnSetTransformEvent(const cbody: PNewtonBody;
      const cmatrix: PNGDFloat; threadIndex: Integer);

    // Callback
    class procedure NewtonApplyForceAndTorque(const body: PNewtonBody;
      timestep: NGDFloat; threadIndex: Integer); static; cdecl;
    class procedure NewtonSetTransform(const body: PNewtonBody;
      const matrix: PNGDFloat; threadIndex: Integer); static; cdecl;


  public
     
    constructor Create(AOwner: TGLXCollection); override;
    destructor Destroy; override;
    procedure AddImpulse(const veloc, pointposit: TVector);
    function GetOmega: TVector;
    procedure SetOmega(const Omega: TVector);
    function GetVelocity: TVector;
    procedure SetVelocity(const Velocity: TVector);
    class function FriendlyName: string; override;
    property CustomForceAndTorqueEvent
      : TApplyForceAndTorqueEvent read FCustomForceAndTorqueEvent write
      FCustomForceAndTorqueEvent;
    property Velocity: TVector read GetVelocity write SetVelocity;
    property Omega: TVector read GetOmega write SetOmega;
  published
     
    property Force: TGLCoordinates read FForce write FForce;
    property Torque: TGLCoordinates read FTorque write FTorque;
    property CenterOfMass
      : TGLCoordinates read FCenterOfMass write FCenterOfMass;
    property AutoSleep: Boolean read FAutoSleep write SetAutoSleep default True;
    property LinearDamping
      : Single read FLinearDamping write SetLinearDamping
      stored StoredLinearDamping;
    property AngularDamping
      : TGLCoordinates read FAngularDamping write FAngularDamping;
    property Density
      : Single read FDensity write SetDensity stored StoredDensity;
    property UseGravity
      : Boolean read FUseGravity write FUseGravity default True;
    property NullCollisionVolume
      : Single read FNullCollisionVolume write FNullCollisionVolume stored
      StoredNullCollisionVolume;

    // Read Only
    property AppliedOmega: TGLCoordinates read FAppliedOmega;
    property AppliedVelocity: TGLCoordinates read FAppliedVelocity;
    property AppliedForce: TGLCoordinates read FAppliedForce;
    property AppliedTorque: TGLCoordinates read FAppliedTorque;
    property Volume: Single read FVolume;
    property Mass: Single read FMass;
  end;

  TGLNGDStatic = class(TGLNGDBehaviour)
  private
     

  protected
     
    procedure Render; override;

  public
     
    class function FriendlyName: string; override;

  published
     
  end;

  TNGDSurfaceItem = class(TCollectionItem)
  private
    FDisplayName: string;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;

  published
    property DisplayName;
    property ID;
  end;

  TNGDSurfacePair = class(TCollectionItem)
  strict private
    FManager: TGLNGDManager;
    FNGDSurfaceItem1: TNGDSurfaceItem;
    FNGDSurfaceItem2: TNGDSurfaceItem;
    FAABBOverlapEvent: TAABBOverlapEvent;
    FContactProcessEvent: TContactProcessEvent;

    FSoftness: Single; // 0.1
    FElasticity: Single; // 0.4
    FCollidable: Boolean; // true
    FStaticFriction: Single; // 0.9
    FKineticFriction: Single; // 0.5
    FContinuousCollisionMode: Boolean; // False
    FThickness: Boolean; // False

    procedure SetCollidable(const Value: Boolean);
    procedure SetElasticity(const Value: Single);
    procedure SetKineticFriction(const Value: Single);
    procedure SetSoftness(const Value: Single);
    procedure SetStaticFriction(const Value: Single);
    procedure SetContinuousCollisionMode(const Value: Boolean);
    procedure SetThickness(const Value: Boolean);

    function StoredElasticity: Boolean;
    function StoredKineticFriction: Boolean;
    function StoredSoftness: Boolean;
    function StoredStaticFriction: Boolean;

  private
    // Callback
    class function NewtonAABBOverlap(const material: PNewtonMaterial;
      const body0: PNewtonBody; const body1: PNewtonBody;
      threadIndex: Integer): Integer; static; cdecl;
    class procedure NewtonContactsProcess(const contact: PNewtonJoint;
      timestep: NGDFloat; threadIndex: Integer); static; cdecl;

    // Event
    function OnNewtonAABBOverlapEvent(const cmaterial: PNewtonMaterial;
      const cbody0: PNewtonBody; const cbody1: PNewtonBody;
      threadIndex: Integer): Boolean;
    procedure OnNewtonContactsProcessEvent(const ccontact: PNewtonJoint;
      timestep: NGDFloat; threadIndex: Integer);

  public
    constructor Create(Collection: TCollection); override;
    procedure SetMaterialItems(const item1, item2: TNGDSurfaceItem);
    property NGDSurfaceItem1: TNGDSurfaceItem read FNGDSurfaceItem1;
    property NGDSurfaceItem2: TNGDSurfaceItem read FNGDSurfaceItem2;

  published
    property Softness: Single read FSoftness write SetSoftness stored
      StoredSoftness;
    property Elasticity: Single read FElasticity write SetElasticity stored
      StoredElasticity;
    property Collidable
      : Boolean read FCollidable write SetCollidable default True;
    property StaticFriction
      : Single read FStaticFriction write SetStaticFriction
      stored StoredStaticFriction;
    property KineticFriction
      : Single read FKineticFriction write SetKineticFriction stored
      StoredKineticFriction;
    property ContinuousCollisionMode
      : Boolean read FContinuousCollisionMode write
      SetContinuousCollisionMode default False;
    property Thickness
      : Boolean read FThickness write SetThickness default False;
    property ContactProcessEvent
      : TContactProcessEvent read FContactProcessEvent
      write FContactProcessEvent;
    property AABBOverlapEvent: TAABBOverlapEvent read FAABBOverlapEvent write
      FAABBOverlapEvent;
  end;

  TNGDJointPivot = class(TPersistent)
  private
    FManager: TGLNGDManager;
    FPivotPoint: TGLCoordinates;
    FOuter: TNGDJoint;
  public
    constructor Create(AOwner: TComponent; aOuter: TNGDJoint); virtual;
    destructor Destroy; override;
  published
    property PivotPoint: TGLCoordinates read FPivotPoint write FPivotPoint;
  end;

  TNGDJointPin = class(TNGDJointPivot)
  private
    FPinDirection: TGLCoordinates;

  public
    constructor Create(AOwner: TComponent; aOuter: TNGDJoint); override;
    destructor Destroy; override;

  published
    property PinDirection
      : TGLCoordinates read FPinDirection write FPinDirection;
  end;

  TNGDJointPin2 = class(TNGDJointPin)
  private
    FPinDirection2: TGLCoordinates;

  public
    constructor Create(AOwner: TComponent; aOuter: TNGDJoint); override;
    destructor Destroy; override;

  published
    property PinDirection2
      : TGLCoordinates read FPinDirection2 write FPinDirection2;
  end;

  TNGDJointBallAndSocket = class(TNGDJointPivot)
  private
    FConeAngle: Single; // 90
    FMinTwistAngle: Single; // -90
    FMaxTwistAngle: Single; // 90
    procedure SetConeAngle(const Value: Single);
    procedure SetMaxTwistAngle(const Value: Single);
    procedure SetMinTwistAngle(const Value: Single);
    function StoredMaxTwistAngle: Boolean;
    function StoredMinTwistAngle: Boolean;
    function StoredConeAngle: Boolean;

  public
    constructor Create(AOwner: TComponent; aOuter: TNGDJoint); override;

  published
    property ConeAngle: Single read FConeAngle write SetConeAngle stored
      StoredConeAngle;
    property MinTwistAngle
      : Single read FMinTwistAngle write SetMinTwistAngle
      stored StoredMinTwistAngle;
    property MaxTwistAngle
      : Single read FMaxTwistAngle write SetMaxTwistAngle
      stored StoredMaxTwistAngle;
  end;

  TNGDJointHinge = class(TNGDJointPin)
  private
    FMinAngle: Single; // -90
    FMaxAngle: Single; // 90
    procedure SetMaxAngle(const Value: Single);
    procedure SetMinAngle(const Value: Single);
    function StoredMaxAngle: Boolean;
    function StoredMinAngle: Boolean;

  public
    constructor Create(AOwner: TComponent; aOuter: TNGDJoint); override;

  published
    property MinAngle: Single read FMinAngle write SetMinAngle stored
      StoredMinAngle;
    property MaxAngle: Single read FMaxAngle write SetMaxAngle stored
      StoredMaxAngle;
  end;

  TNGDJointSlider = class(TNGDJointPin)
  private
    FMinDistance: Single; // -10
    FMaxDistance: Single; // 10
    procedure SetMaxDistance(const Value: Single);
    procedure SetMinDistance(const Value: Single);
    function StoredMaxDistance: Boolean;
    function StoredMinDistance: Boolean;

  public
    constructor Create(AOwner: TComponent; aOuter: TNGDJoint); override;

  published
    property MinDistance: Single read FMinDistance write SetMinDistance stored 
	   StoredMinDistance;
    property MaxDistance: Single read FMaxDistance write SetMaxDistance stored 
	   StoredMaxDistance;
  end;

  TNGDJointKinematicController = class(TPersistent)
  private
    FPickModeLinear: Boolean; // False
    FLinearFriction: Single; // 750
    FAngularFriction: Single; // 250
    function StoredAngularFriction: Boolean;
    function StoredLinearFriction: Boolean;

  public
    constructor Create();

  published
    property PickModeLinear
      : Boolean read FPickModeLinear write FPickModeLinear
      default False;
    property LinearFriction
      : Single read FLinearFriction write FLinearFriction stored
      StoredLinearFriction;
    property AngularFriction
      : Single read FAngularFriction write FAngularFriction stored
      StoredAngularFriction;
  end;

  TNGDJoint = class(TCollectionItem)

  private
    // Global
    FManager: TGLNGDManager;
    FParentObject: TGLBaseSceneObject;
    FJointType: TNGDNewtonJoints;
    FStiffness: Single; // 0.9

    // With Two object
    // Every joint except nj_UpVector and nj_KinematicController
    FChildObject: TGLBaseSceneObject;
    FCollisionState: Boolean; // False

    // With classic joint
    // nj_BallAndSocket, nj_Hinge, nj_Slider, nj_Corkscrew
    // nj_Universal, nj_UpVector
    FNewtonJoint: PNewtonJoint;

    // With CustomJoint
    // nj_CustomBallAndSocket, nj_CustomHinge, nj_CustomSlider
    // nj_KinematicController
    FNewtonUserJoint: PNewtonUserJoint;

    // nj_UpVector
    FUPVectorDirection: TGLCoordinates;

    FBallAndSocketOptions: TNGDJointPivot;
    FHingeOptions: TNGDJointPin;
    FSliderOptions: TNGDJointPin;
    FCorkscrewOptions: TNGDJointPin;
    FUniversalOptions: TNGDJointPin2;

    FCustomBallAndSocketOptions: TNGDJointBallAndSocket;
    FCustomHingeOptions: TNGDJointHinge;
    FCustomSliderOptions: TNGDJointSlider;
    FKinematicOptions: TNGDJointKinematicController;

    procedure SetJointType(const Value: TNGDNewtonJoints);
    procedure SetChildObject(const Value: TGLBaseSceneObject);
    procedure SetCollisionState(const Value: Boolean);
    procedure SetParentObject(const Value: TGLBaseSceneObject);
    procedure SetStiffness(const Value: Single);
    procedure Render;
    function StoredStiffness: Boolean;
    procedure DestroyNewtonData;
  public

    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure KinematicControllerPick(pickpoint: TVector;
      PickedActions: TNGDPickedActions);

  published
    property BallAndSocketOptions
      : TNGDJointPivot read FBallAndSocketOptions write
      FBallAndSocketOptions;
    property HingeOptions: TNGDJointPin read FHingeOptions write FHingeOptions;
    property SliderOptions
      : TNGDJointPin read FSliderOptions write FSliderOptions;
    property CorkscrewOptions
      : TNGDJointPin read FCorkscrewOptions write FCorkscrewOptions;
    property UniversalOptions
      : TNGDJointPin2 read FUniversalOptions write FUniversalOptions;
    property CustomBallAndSocketOptions
      : TNGDJointBallAndSocket read FCustomBallAndSocketOptions write
      FCustomBallAndSocketOptions;
    property CustomHingeOptions: TNGDJointHinge read FCustomHingeOptions write
      FCustomHingeOptions;
    property CustomSliderOptions
      : TNGDJointSlider read FCustomSliderOptions write
      FCustomSliderOptions;
    property KinematicControllerOptions
      : TNGDJointKinematicController read FKinematicOptions write
      FKinematicOptions;
    property JointType: TNGDNewtonJoints read FJointType write SetJointType;
    property ParentObject: TGLBaseSceneObject read FParentObject write
      SetParentObject;
    property ChildObject: TGLBaseSceneObject read FChildObject write
      SetChildObject;
    property CollisionState
      : Boolean read FCollisionState write SetCollisionState default False;
    property Stiffness: Single read FStiffness write SetStiffness stored
      StoredStiffness;
    property UPVectorDirection
      : TGLCoordinates read FUPVectorDirection write FUPVectorDirection;
  end;

  { Global function }
function GetNGDStatic(Obj: TGLBaseSceneObject): TGLNGDStatic;
function GetOrCreateNGDStatic(Obj: TGLBaseSceneObject): TGLNGDStatic;
function GetNGDDynamic(Obj: TGLBaseSceneObject): TGLNGDDynamic;
function GetOrCreateNGDDynamic(Obj: TGLBaseSceneObject): TGLNGDDynamic;

function GetBodyFromGLSceneObject(Obj: TGLBaseSceneObject): PNewtonBody;

implementation

const
  epsilon = 0.0000001; // 1E-07

  // GetNGDStatic
  //
function GetNGDStatic(Obj: TGLBaseSceneObject): TGLNGDStatic;
begin
  Result := TGLNGDStatic(Obj.Behaviours.GetByClass(TGLNGDStatic));
end;

// GetOrCreateNGDStatic
//
function GetOrCreateNGDStatic(Obj: TGLBaseSceneObject): TGLNGDStatic;
begin
  Result := TGLNGDStatic(Obj.GetOrCreateBehaviour(TGLNGDStatic));
end;

// GetNGDDynamic
//
function GetNGDDynamic(Obj: TGLBaseSceneObject): TGLNGDDynamic;
begin
  Result := TGLNGDDynamic(Obj.Behaviours.GetByClass(TGLNGDDynamic));
end;

// GetOrCreateNGDDynamic
//
function GetOrCreateNGDDynamic(Obj: TGLBaseSceneObject): TGLNGDDynamic;
begin
  Result := TGLNGDDynamic(Obj.GetOrCreateBehaviour(TGLNGDDynamic));
end;

function GetBodyFromGLSceneObject(Obj: TGLBaseSceneObject): PNewtonBody;
var
  Behaviour: TGLNGDBehaviour;
begin
  Behaviour := TGLNGDBehaviour(Obj.Behaviours.GetByClass(TGLNGDBehaviour));
  Assert(Behaviour <> nil, 'NGD Behaviour (static or dynamic) is missing for this object');
  Result := Behaviour.FNewtonBody;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{ TNGDDebugOption }

constructor TNGDDebugOption.Create(AOwner: TComponent);
begin
  FManager := AOwner as TGLNGDManager;
  with FManager do
  begin
    FGeomColorDyn := TGLColor.CreateInitialized(self, clrGreen, NotifyChange);
    FGeomColorStat := TGLColor.CreateInitialized(self, clrRed, NotifyChange);
    FAABBColor := TGLColor.CreateInitialized(self, clrYellow, NotifyChange);
    FAABBColorSleep := TGLColor.CreateInitialized(self, clrOrange,
      NotifyChange);
    FCenterOfMassColor := TGLColor.CreateInitialized(self, clrPurple,
      NotifyChange);
    FContactColor := TGLColor.CreateInitialized(self, clrWhite, NotifyChange);
    FJointAxisColor := TGLColor.CreateInitialized(self, clrBlue, NotifyChange);
    FJointPivotColor := TGLColor.CreateInitialized(self, clrAquamarine,
      NotifyChange);

    FForceColor := TGLColor.CreateInitialized(self, clrBlack, NotifyChange);
    FAppliedForceColor := TGLColor.CreateInitialized(self, clrSilver,
      NotifyChange);
    FAppliedVelocityColor := TGLColor.CreateInitialized(self, clrLime,
      NotifyChange);

    FCustomColor := TGLColor.CreateInitialized(self, clrAqua, NotifyChange);
  end;
  FDotAxisSize := 1;
  FNGDManagerDebugs := [];

  FManager := AOwner as TGLNGDManager;
end;

destructor TNGDDebugOption.Destroy;
begin
  FGeomColorDyn.Free;
  FGeomColorStat.Free;
  FAABBColor.Free;
  FAABBColorSleep.Free;
  FCenterOfMassColor.Free;
  FContactColor.Free;
  FJointAxisColor.Free;
  FJointPivotColor.Free;
  FForceColor.Free;
  FAppliedForceColor.Free;
  FAppliedVelocityColor.Free;
  FCustomColor.Free;
  inherited;
end;

procedure TNGDDebugOption.SetDotAxisSize(const Value: Single);
begin
  FDotAxisSize := Value;
  FManager.NotifyChange(self);
end;

procedure TNGDDebugOption.SetNGDManagerDebugs(const Value: TNGDManagerDebugs);
begin
  FNGDManagerDebugs := Value;
  FManager.NotifyChange(self);
end;

function TNGDDebugOption.StoredDotAxis: Boolean;
begin
  Result := not SameValue(FDotAxisSize, 1, epsilon);
end;

{ TGLNGDManager }

procedure TGLNGDManager.AddNode(const Value: TVector);
begin
  if Assigned(FGLLines) then
  begin
    FGLLines.Nodes.AddNode(Value);

    with (FGLLines.Nodes.Last as TGLLinesNode) do
      Color := FCurrentColor;
  end;
end;

procedure TGLNGDManager.AddNode(const coords: TGLCustomCoordinates);
begin
  if Assigned(FGLLines) then
  begin
    FGLLines.Nodes.AddNode(coords); (FGLLines.Nodes.Last as TGLLinesNode)
    .Color := FCurrentColor;
  end;
end;

procedure TGLNGDManager.AddNode(const X, Y, Z: Single);
begin
  if Assigned(FGLLines) then
  begin
    FGLLines.Nodes.AddNode(X, Y, Z); (FGLLines.Nodes.Last as TGLLinesNode)
    .Color := FCurrentColor;
  end;
end;

procedure TGLNGDManager.AddNode(const Value: TAffineVector);
begin
  if Assigned(FGLLines) then
  begin
    FGLLines.Nodes.AddNode(Value); (FGLLines.Nodes.Last as TGLLinesNode)
    .Color := FCurrentColor;
  end;
end;

constructor TGLNGDManager.Create(AOwner: TComponent);
var
  minworld, maxworld: TVector;
begin
  inherited;
  FNGDBehaviours := TGLNGDBehaviourList.Create;
  FVisible := True;
  FVisibleAtRunTime := False;
  FSolverModel := smExact;
  FFrictionModel := fmExact;
  FMinimumFrameRate := 60;
  FWorldSizeMin := TGLCoordinates.CreateInitialized(self,
    VectorMake(-100, -100, -100, 0), csPoint);
  FWorldSizeMax := TGLCoordinates.CreateInitialized(self,
    VectorMake(100, 100, 100, 0), csPoint);

  // Using Events because we need to call API Function when
  // theses TGLCoordinates change.
  FWorldSizeMin.OnNotifyChange := NotifyWorldSizeChange;
  FWorldSizeMax.OnNotifyChange := NotifyWorldSizeChange;

  FThreadCount := 1;
  FGravity := TGLCoordinates3.CreateInitialized(self,
    VectorMake(0, -9.81, 0, 0), csVector);

  FNewtonWorld := NewtonCreate(nil, nil);
  FDllVersion := NewtonWorldGetVersion(FNewtonWorld);

  // This is to prevent body out the world at startTime
  minworld := VectorMake(-1E50, -1E50, -1E50);
  maxworld := VectorMake(1E50, 1E50, 1E50);
  NewtonSetWorldSize(FNewtonWorld, @minworld, @maxworld);

  NewtonWorldSetUserData(FNewtonWorld, self);

  FNewtonSurfaceItem := TCollection.Create(TNGDSurfaceItem);
  FNewtonSurfacePair := TOwnedCollection.Create(self, TNGDSurfacePair);
  FNewtonJointGroup := TOwnedCollection.Create(self, TNGDJoint);

  FNGDDebugOption := TNGDDebugOption.Create(self);

  RegisterManager(self);

end;

destructor TGLNGDManager.Destroy;
begin
  // Destroy joint before body.
  FreeAndNil(FNewtonJointGroup);

  // Unregister everything
  while FNGDBehaviours.Count > 0 do
    FNGDBehaviours[0].Manager := nil;

  // Clean up everything
  FreeAndNil(FNGDBehaviours);
  FreeAndNil(FWorldSizeMin);
  FreeAndNil(FWorldSizeMax);
  FreeAndNil(FGravity);
  FreeAndNil(FNewtonSurfaceItem);
  FreeAndNil(FNewtonSurfacePair);
  FreeAndNil(FNGDDebugOption);

  NewtonDestroyAllBodies(FNewtonWorld);
  NewtonMaterialDestroyAllGroupID(FNewtonWorld);
  NewtonDestroy(FNewtonWorld);
  FNewtonWorld := nil;

  DeregisterManager(self);
  inherited;
end;

procedure TGLNGDManager.Loaded;
begin
  inherited;
  NotifyWorldSizeChange(self);
  RebuildAllJoint(self);
end;

function TGLNGDManager.GetBodyCount: Integer;
begin
  if (csDesigning in ComponentState) then
    Result := FNGDBehaviours.Count
  else
    Result := NewtonWorldGetBodyCount(FNewtonWorld);
end;

function TGLNGDManager.GetConstraintCount: Integer;
begin
  if (csDesigning in ComponentState) then
    Result := FNewtonJointGroup.Count
  else
    // Constraint is the number of joint
    Result := NewtonWorldGetConstraintCount(FNewtonWorld);
end;

procedure TGLNGDManager.NotifyChange(Sender: TObject);
var
  I: Integer;
begin
  // This event is raise
  // when debugOptions properties are edited,
  // when a behavior is initialized/finalize,
  // when joints are rebuilded, (runtime only)
  // when visible and visibleAtRuntime are edited (designTime only),
  // in manager.step, and in SetGLLines.

  // Here the manager call render method for bodies and joints in its lists

  if not Assigned(FGLLines) then
    exit;
  FGLLines.Nodes.Clear;

  if not Visible then
    exit;
  if not(csDesigning in ComponentState) then
    if not VisibleAtRunTime then
      exit;

  for I := 0 to FNGDBehaviours.Count - 1 do
    FNGDBehaviours[I].Render;

  if mdShowJoint in FNGDDebugOption.NGDManagerDebugs then
    for I := 0 to NewtonJoint.Count - 1 do //
  (NewtonJoint.Items[I] as TNGDJoint)
      .Render;

end;

procedure TGLNGDManager.SetFrictionModel(const Value: TNGDFrictionModels);
begin
  FFrictionModel := Value;
  if not(csDesigning in ComponentState) then
    NewtonSetFrictionModel(FNewtonWorld, Ord(FFrictionModel));
end;

procedure TGLNGDManager.SetGLLines(const Value: TGLLines);
begin
  if Assigned(FGLLines) then
    FGLLines.Nodes.Clear;

  FGLLines := Value;

  if Assigned(FGLLines) then
  begin
    FGLLines.SplineMode := lsmSegments;
    FGLLines.NodesAspect := lnaInvisible;
    FGLLines.Options := [loUseNodeColorForLines];
    FGLLines.Pickable := False;
    NotifyChange(self);
  end;
end;

procedure TGLNGDManager.SetMinimumFrameRate(const Value: Integer);
begin
  if (Value >= 60) and (Value <= 1000) then
    FMinimumFrameRate := Value;
  if not(csDesigning in ComponentState) then
    NewtonSetMinimumFrameRate(FNewtonWorld, FMinimumFrameRate);
end;

procedure TGLNGDManager.SetSolverModel(const Value: TNGDSolverModels);
begin
  FSolverModel := Value;
  if not(csDesigning in ComponentState) then
    NewtonSetSolverModel(FNewtonWorld, Ord(FSolverModel));
end;

procedure TGLNGDManager.SetThreadCount(const Value: Integer);
begin
  if Value > 0 then
    FThreadCount := Value;
  NewtonSetThreadsCount(FNewtonWorld, FThreadCount);
  FThreadCount := NewtonGetThreadsCount(FNewtonWorld);
end;

procedure TGLNGDManager.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  if (csDesigning in ComponentState) then
    NotifyChange(self);
end;

procedure TGLNGDManager.SetVisibleAtRunTime(const Value: Boolean);
begin
  FVisibleAtRunTime := Value;
  if (csDesigning in ComponentState) then
    NotifyChange(self);
end;

procedure TGLNGDManager.NotifyWorldSizeChange(Sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    NewtonSetWorldSize(FNewtonWorld, @FWorldSizeMin.AsVector,
      @FWorldSizeMax.AsVector);
end;

procedure TGLNGDManager.RebuildAllJoint(Sender: TObject);

  procedure BuildBallAndSocket(Joint: TNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateBall(FNewtonWorld,
          @(FBallAndSocketOptions.FPivotPoint.AsVector),
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildHinge(Joint: TNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateHinge(FNewtonWorld,
          @(FHingeOptions.FPivotPoint.AsVector),
          @(FHingeOptions.FPinDirection.AsVector),
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildSlider(Joint: TNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateSlider(FNewtonWorld,
          @(FSliderOptions.FPivotPoint.AsVector),
          @(FSliderOptions.FPinDirection.AsVector),
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildCorkscrew(Joint: TNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateCorkscrew(FNewtonWorld,
          @(FCorkscrewOptions.FPivotPoint.AsVector),
          @(FCorkscrewOptions.FPinDirection.AsVector),
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildUniversal(Joint: TNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateUniversal(FNewtonWorld,
          @(FUniversalOptions.FPivotPoint.AsVector),
          @(FUniversalOptions.FPinDirection.AsVector),
          @(FUniversalOptions.FPinDirection2.AsVector),
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildCustomBallAndSocket(Joint: TNGDJoint);
  var
    pinAndPivot: TMatrix;
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        pinAndPivot := IdentityHmgMatrix;
        pinAndPivot.V[3] := FCustomBallAndSocketOptions.FPivotPoint.AsVector;
        FNewtonUserJoint := CreateCustomBallAndSocket(@pinAndPivot,
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        BallAndSocketSetConeAngle(FNewtonUserJoint,
          GLVectorGeometry.DegToRad(FCustomBallAndSocketOptions.FConeAngle));
        BallAndSocketSetTwistAngle(FNewtonUserJoint,
          GLVectorGeometry.DegToRad(FCustomBallAndSocketOptions.FMinTwistAngle),
          GLVectorGeometry.DegToRad(FCustomBallAndSocketOptions.FMaxTwistAngle));
        CustomSetBodiesCollisionState(FNewtonUserJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(CustomGetNewtonJoint(FNewtonUserJoint),
          FStiffness);
      end;
  end;

  procedure BuildCustomHinge(Joint: TNGDJoint);
  var
    pinAndPivot: TMatrix;
    bso: TGLBaseSceneObject;
  begin
    { Newton wait from FPinAndPivotMatrix a structure like that:
      First row: the pin direction
      Second and third rows are set to create an orthogonal matrix
      Fourth: The pivot position

      In glscene, the GLBaseSceneObjects direction is the third row,
      because the first row is the right vector (second row is up vector). }
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        bso := TGLBaseSceneObject.Create(FManager);
        bso.AbsolutePosition := FCustomHingeOptions.FPivotPoint.AsVector;
        bso.AbsoluteDirection := FCustomHingeOptions.FPinDirection.AsVector;
        pinAndPivot := bso.AbsoluteMatrix;
        pinAndPivot.V[0] := bso.AbsoluteMatrix.V[2];
        pinAndPivot.V[2] := bso.AbsoluteMatrix.V[0];
        bso.Free;

        FNewtonUserJoint := CreateCustomHinge(@pinAndPivot,
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        HingeEnableLimits(FNewtonUserJoint, 1);
        HingeSetLimits(FNewtonUserJoint,
          GLVectorGeometry.DegToRad(FCustomHingeOptions.FMinAngle),
          GLVectorGeometry.DegToRad(FCustomHingeOptions.FMaxAngle));
        CustomSetBodiesCollisionState(FNewtonUserJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(CustomGetNewtonJoint(FNewtonUserJoint),
          FStiffness);
        CustomSetUserData(FNewtonUserJoint, CustomHingeOptions);
      end;
  end;

  procedure BuildCustomSlider(Joint: TNGDJoint);
  var
    pinAndPivot: TMatrix;
    bso: TGLBaseSceneObject;

  begin
    { Newton wait from FPinAndPivotMatrix a structure like that:
      First row: the pin direction
      Second and third rows are set to create an orthogonal matrix
      Fourth: The pivot position

      In glscene, the GLBaseSceneObjects direction is the third row,
      because the first row is the right vector (second row is up vector). }
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin

        bso := TGLBaseSceneObject.Create(FManager);
        bso.AbsolutePosition := FCustomSliderOptions.FPivotPoint.AsVector;
        bso.AbsoluteDirection := FCustomSliderOptions.FPinDirection.AsVector;
        pinAndPivot := bso.AbsoluteMatrix;
        pinAndPivot.V[0] := bso.AbsoluteMatrix.V[2];
        pinAndPivot.V[2] := bso.AbsoluteMatrix.V[0];
        bso.Free;

        FNewtonUserJoint := CreateCustomSlider(@pinAndPivot, GetBodyFromGLSceneObject(FChildObject), GetBodyFromGLSceneObject(FParentObject));
        SliderEnableLimits(FNewtonUserJoint, 1);
        SliderSetLimits(FNewtonUserJoint, FCustomSliderOptions.FMinDistance, FCustomSliderOptions.FMaxDistance);
        NewtonJointSetStiffness(CustomGetNewtonJoint(FNewtonUserJoint),0);

        CustomSetBodiesCollisionState(FNewtonUserJoint, Ord(FCollisionState));
        CustomSetUserData(FNewtonUserJoint, CustomSliderOptions);
      end;
  end;

  procedure BuildUpVector(Joint: TNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateUpVector(FNewtonWorld,
          @FUPVectorDirection.AsVector,
          GetBodyFromGLSceneObject(FParentObject));
      end;
  end;

  procedure BuildKinematicController(Joint: TNGDJoint);
  begin
    // do nothing
  end;

  procedure BuildOneJoint(Joint: TNGDJoint);
  begin
    case Joint.FJointType of
      nj_BallAndSocket:
        begin
          Joint.DestroyNewtonData;
          BuildBallAndSocket(Joint);
        end;

      nj_Hinge:
        begin
          Joint.DestroyNewtonData;
          BuildHinge(Joint);
        end;

      nj_Slider:
        begin
          Joint.DestroyNewtonData;
          BuildSlider(Joint);
        end;

      nj_Corkscrew:
        begin
          Joint.DestroyNewtonData;
          BuildCorkscrew(Joint);
        end;

      nj_Universal:
        begin
          Joint.DestroyNewtonData;
          BuildUniversal(Joint);
        end;

      nj_CustomBallAndSocket:
        begin
          Joint.DestroyNewtonData;
          BuildCustomBallAndSocket(Joint);
        end;

      nj_CustomHinge:
        begin
          Joint.DestroyNewtonData;
          BuildCustomHinge(Joint);
        end;

      nj_CustomSlider:
        begin
          Joint.DestroyNewtonData;
          BuildCustomSlider(Joint);
        end;

      nj_UpVector:
        begin
          Joint.DestroyNewtonData;
          BuildUpVector(Joint);
        end;

      nj_KinematicController:
        begin
          // DestroyJoint(Joint);
          // BuildKinematicController(Joint);
        end;
    end;
  end;

var
  i: Integer;
begin

  if not(csDesigning in ComponentState) and not(csLoading in ComponentState)
    then
  begin
    if Sender is TGLNGDManager then
      for i := 0 to NewtonJoint.Count - 1 do
        BuildOneJoint(NewtonJoint.Items[i] as TNGDJoint);

    if (Sender is TNGDJoint) then
      BuildOneJoint((Sender as TNGDJoint));

    if Sender is TGLCoordinates then
      BuildOneJoint(((Sender as TGLCoordinates).Owner as TNGDJoint));

    NotifyChange(self);
  end;

end;

procedure TGLNGDManager.RebuildAllMaterial;

  procedure BuildMaterialPair;
  var
    I, ID0, ID1: Integer;
  begin
    for I := 0 to FNewtonSurfacePair.Count - 1 do
      with (FNewtonSurfacePair.Items[I] as TNGDSurfacePair) do
      begin
        if Assigned(NGDSurfaceItem1) and Assigned(NGDSurfaceItem2) then
        begin
          ID0 := NGDSurfaceItem1.ID;
          ID1 := NGDSurfaceItem2.ID;

          NewtonMaterialSetContinuousCollisionMode(FNewtonWorld, ID0, ID1,
            Ord(ContinuousCollisionMode));
          if Thickness then
            NewtonMaterialSetSurfaceThickness(FNewtonWorld, ID0, ID1, 1);
          NewtonMaterialSetDefaultSoftness(FNewtonWorld, ID0, ID1, Softness);
          NewtonMaterialSetDefaultElasticity(FNewtonWorld, ID0, ID1,
            Elasticity);
          NewtonMaterialSetDefaultCollidable(FNewtonWorld, ID0, ID1,
            Ord(Collidable));
          NewtonMaterialSetDefaultFriction(FNewtonWorld, ID0, ID1,
            StaticFriction, KineticFriction);

          NewtonMaterialSetCollisionCallback(FNewtonWorld, ID0, ID1,
            FNewtonSurfacePair.Items[I], @TNGDSurfacePair.NewtonAABBOverlap,
            @TNGDSurfacePair.NewtonContactsProcess);
        end;
      end;
  end;

var
  I: Integer;
  maxID: Integer;
begin
  maxID := 0;
  if not(csDesigning in ComponentState) then
  begin
    // Destroy newton materials
    NewtonMaterialDestroyAllGroupID(FNewtonWorld);

    // Create materialID
    for I := 0 to FNewtonSurfaceItem.Count - 1 do
      maxID := MaxInteger((FNewtonSurfaceItem.Items[I] as TNGDSurfaceItem).ID,
        maxID);
    for I := 0 to maxID - 1 do
      NewtonMaterialCreateGroupID(FNewtonWorld);

    // Assigned matID to bodies
    for I := 0 to FNGDBehaviours.Count - 1 do
      with FNGDBehaviours[I] do
        if Assigned(FNGDSurfaceItem) then
          NewtonBodySetMaterialGroupID(FNewtonBody, FNGDSurfaceItem.ID)
        else
          NewtonBodySetMaterialGroupID(FNewtonBody, 0);

    // Set values to newton material pair :callback userdata friction...
    BuildMaterialPair;
  end;
end;

procedure TGLNGDManager.Step(deltatime: Single);
begin
  if not(csDesigning in ComponentState) then
    NewtonUpdate(FNewtonWorld, deltatime);

  NotifyChange(self);
end;

{ TGLNGDBehaviour }

constructor TGLNGDBehaviour.Create(AOwner: TGLXCollection);
begin
  inherited;
  FInitialized := False;
  FOwnerBaseSceneObject := OwnerBaseSceneObject;

  FContinuousCollisionMode := False;
  FNewtonBody := nil;
  FCollision := nil;

  FNGDNewtonCollisions := nc_Primitive;

  FCollisionIteratorEvent := OnCollisionIteratorEvent;

  FTreeCollisionOptimize := True;
  FConvexCollisionTolerance := 0.01;
  FFileCollision := '';
  name := 'NGD Static';
end;

destructor TGLNGDBehaviour.Destroy;
begin
  if Assigned(FManager) then
    Manager := nil;  // This will call finalize
  inherited;
end;

procedure TGLNGDBehaviour.Finalize;
var
  i: integer;
begin
  FInitialized := False;

  if Assigned(FManager) then
  begin

    if Assigned(FManager.NewtonJoint) then
    for i := FManager.NewtonJoint.Count-1 downto 0 do
    begin
      if ((FManager.NewtonJoint.Items[i] as TNGDJoint).ParentObject = FOwnerBaseSceneObject)
      or ((FManager.NewtonJoint.Items[i] as TNGDJoint).ChildObject = FOwnerBaseSceneObject) then
      begin
        FManager.NewtonJoint.Items[i].Free;
      end;
    end;

    NewtonDestroyBody(FManager.FNewtonWorld, FNewtonBody);
    FNewtonBody := nil;
    FCollision := nil;
  end;
end;

function TGLNGDBehaviour.GetBBoxCollision: PNewtonCollision;
var
  vc: array [0 .. 7] of TVector;
  I: Integer;
begin
  for I := 0 to 8 - 1 do
    vc[I] := AABBToBB(FOwnerBaseSceneObject.AxisAlignedBoundingBoxEx).BBox[I];
  Result := NewtonCreateConvexHull(FManager.FNewtonWorld, 8, @vc[0],
    SizeOf(TVector), 0.01, 0, nil);
end;

function TGLNGDBehaviour.GetBSphereCollision: PNewtonCollision;
var
  boundingSphere: TBSphere;
  collisionOffsetMatrix: TMatrix;
begin
  AABBToBSphere(FOwnerBaseSceneObject.AxisAlignedBoundingBoxEx, boundingSphere);

  collisionOffsetMatrix := IdentityHmgMatrix;
  collisionOffsetMatrix.V[3] := VectorMake(boundingSphere.Center, 1);
  Result := NewtonCreateSphere(FManager.FNewtonWorld, boundingSphere.Radius,
    boundingSphere.Radius, boundingSphere.Radius, 0, @collisionOffsetMatrix);
end;

function TGLNGDBehaviour.GetConvexCollision: PNewtonCollision;
var
  I, J: Integer;
  vertexArray: array of TVertex;
begin
  if FOwnerBaseSceneObject is TGLBaseMesh then
  begin
    with (FOwnerBaseSceneObject as TGLBaseMesh) do
    begin

      for I := 0 to MeshObjects.Count - 1 do
        for J := 0 to MeshObjects[I].Vertices.Count - 1 do
        begin
          SetLength(vertexArray, Length(vertexArray) + 1);
          vertexArray[Length(vertexArray) - 1] := MeshObjects[I].Vertices[J];
        end;

      if Length(vertexArray) > 0 then
        Result := NewtonCreateConvexHull(FManager.FNewtonWorld,
          Length(vertexArray), @vertexArray[0], SizeOf(TVertex),
          FConvexCollisionTolerance, 0, nil)
      else
        Result := GetNullCollision;

    end;
  end
  else
    Result := GetNullCollision;
end;

function TGLNGDBehaviour.GetHeightFieldCollision: PNewtonCollision;
var
  I: Integer;
  attributeMap: array of ShortInt;
begin
  SetLength(attributeMap, Length(FHeightFieldOptions.heightArray));
  for I := 0 to Length(FHeightFieldOptions.heightArray) - 1 do
    attributeMap[I] := 0;

  Result := NewtonCreateHeightFieldCollision(FManager.FNewtonWorld,
    FHeightFieldOptions.width, FHeightFieldOptions.depth,
    Ord(FHeightFieldOptions.gridDiagonals),
    PUnsigned_short(FHeightFieldOptions.heightArray), P2Char(attributeMap),
    FHeightFieldOptions.widthDepthScale, FHeightFieldOptions.heightScale, 0);
end;

function TGLNGDBehaviour.GetMeshCollision: PNewtonCollision;
var
  collisionArray: array of PNewtonCollision;
  I, J: Integer;
  vertexArray: array of TVertex;
begin
  if FOwnerBaseSceneObject is TGLBaseMesh then
  begin
    with (FOwnerBaseSceneObject as TGLBaseMesh) do
    begin

      // Iterate trough mesh of GLobject
      for I := 0 to MeshObjects.Count - 1 do
      begin
        // Iterate trough vertices of mesh
        for J := 0 to MeshObjects[I].Vertices.Count - 1 do
        begin
          SetLength(vertexArray, Length(vertexArray) + 1);
          vertexArray[Length(vertexArray) - 1] := MeshObjects[I].Vertices[J];
        end;

        if Length(vertexArray) > 3 then
        begin
          SetLength(collisionArray, Length(collisionArray) + 1);

          collisionArray[Length(collisionArray) - 1] := NewtonCreateConvexHull
            (FManager.FNewtonWorld, Length(vertexArray), @vertexArray[0],
            SizeOf(TVertex), FConvexCollisionTolerance, 0, nil);

          // Remove last collision if the newton function was not successful
          if collisionArray[Length(collisionArray) - 1] = nil then
            SetLength(collisionArray, Length(collisionArray) - 1);

        end;
        SetLength(vertexArray, 0);
      end;

      if Length(collisionArray) > 0 then
        Result := NewtonCreateCompoundCollision(FManager.FNewtonWorld,
          Length(collisionArray), @collisionArray[0], 0)
      else
        Result := GetNullCollision;

    end;
  end
  else
    Result := GetNullCollision;

end;


function TGLNGDBehaviour.GetNewtonBodyMatrix: TMatrix;
begin
  if Assigned(FManager) then
    NewtonBodyGetmatrix(FNewtonBody, @FNewtonBodyMatrix);
  Result := FNewtonBodyMatrix;
end;

function TGLNGDBehaviour.GetNewtonBodyAABB: TAABB;
begin
  if Assigned(FManager) then
    NewtonBodyGetAABB(FNewtonBody, @(Result.min), @(Result.max));
end;

function TGLNGDBehaviour.GetNGDFileCollision: PNewtonCollision;
var
  MyFile: TFileStream;
begin

  if FileExists(FFileCollision) then
  begin
    MyFile := TFileStream.Create(FFileCollision, fmOpenRead);

    Result := NewtonCreateCollisionFromSerialization(FManager.FNewtonWorld,
      @TGLNGDBehaviour.NewtonDeserialize, Pointer(MyFile));

    MyFile.Free;
  end
  else
    Result := NewtonCreateNull(FManager.FNewtonWorld);

end;

function TGLNGDBehaviour.GetNullCollision: PNewtonCollision;
begin
  Result := NewtonCreateNull(FManager.FNewtonWorld);
end;

function TGLNGDBehaviour.GetPrimitiveCollision: PNewtonCollision;
var
  collisionOffsetMatrix: TMatrix; // For cone capsule and cylinder
begin
  collisionOffsetMatrix := IdentityHmgMatrix;

  if (FOwnerBaseSceneObject is TGLCube) then
  begin
    with (FOwnerBaseSceneObject as TGLCube) do
      Result := NewtonCreateBox(FManager.FNewtonWorld, CubeWidth, CubeHeight,
        CubeDepth, 0, @collisionOffsetMatrix);
  end

  else if (FOwnerBaseSceneObject is TGLSphere) then
  begin
    with (FOwnerBaseSceneObject as TGLSphere) do
      Result := NewtonCreateSphere(FManager.FNewtonWorld, Radius, Radius,
        Radius, 0, @collisionOffsetMatrix);
  end

  else if (FOwnerBaseSceneObject is TGLCone) then
  begin
    collisionOffsetMatrix := MatrixMultiply(collisionOffsetMatrix,
      CreateRotationMatrixZ(Pi / 2.0));
    with (FOwnerBaseSceneObject as TGLCone) do
      Result := NewtonCreateCone(FManager.FNewtonWorld, BottomRadius, Height,
        0, @collisionOffsetMatrix);
  end

  else if (FOwnerBaseSceneObject is TGLCapsule) then
  begin
    collisionOffsetMatrix := MatrixMultiply(collisionOffsetMatrix,
      CreateRotationMatrixY(Pi / 2.0));
    with (FOwnerBaseSceneObject as TGLCapsule) do
      // Use Cylinder shape for buoyancy
      Result := NewtonCreateCapsule(FManager.FNewtonWorld, Radius,
        Height + 2 * Radius, 0, @collisionOffsetMatrix);
  end

  else if (FOwnerBaseSceneObject is TGLCylinder) then
  begin
    collisionOffsetMatrix := MatrixMultiply(collisionOffsetMatrix,
      CreateRotationMatrixZ(Pi / 2.0));
    with (FOwnerBaseSceneObject as TGLCylinder) do
      Result := NewtonCreateCylinder(FManager.FNewtonWorld, BottomRadius,
        Height, 0, @collisionOffsetMatrix);
  end
  else
    Result := GetNullCollision;
end;

function TGLNGDBehaviour.GetTreeCollision: PNewtonCollision;
var
  meshIndex, triangleIndex: Integer;
  triangleList: TAffineVectorList;
  v: array [0 .. 2] of TAffineVector;
begin

  if FOwnerBaseSceneObject is TGLBaseMesh then
  begin
    with (FOwnerBaseSceneObject as TGLBaseMesh) do
    begin
      Result := NewtonCreateTreeCollision(FManager.FNewtonWorld, 0);
      NewtonTreeCollisionBeginBuild(Result);

      for meshIndex := 0 to MeshObjects.Count - 1 do
      begin
        triangleList := MeshObjects[meshIndex].ExtractTriangles;
        for triangleIndex := 0 to triangleList.Count - 1 do
        begin
          if triangleIndex mod 3 = 0 then
          begin
            v[0] := triangleList.Items[triangleIndex];
            // ScaleVector(v[0], FOwnerBaseSceneObject.Scale.X);
            v[1] := triangleList.Items[triangleIndex + 1];
            // ScaleVector(v[1], FOwnerBaseSceneObject.Scale.Y);
            v[2] := triangleList.Items[triangleIndex + 2];
            // ScaleVector(v[2], FOwnerBaseSceneObject.Scale.Z);
            NewtonTreeCollisionAddFace(Result, 3, @(v), SizeOf(TAffineVector),
              1);
          end;
        end;
        triangleList.Free;
      end;
      NewtonTreeCollisionEndBuild(Result, Ord(FTreeCollisionOptimize));
    end;
  end
  else
    Result := GetNullCollision;

end;

procedure TGLNGDBehaviour.Initialize;
begin
  FInitialized := True;

  if Assigned(FManager) then
  begin
    // Create NewtonBody with null collision
    FCollision := NewtonCreateNull(FManager.FNewtonWorld);
    FNewtonBodyMatrix := FOwnerBaseSceneObject.AbsoluteMatrix;
    FNewtonBody := NewtonCreateBody(FManager.FNewtonWorld, FCollision,
      @FNewtonBodyMatrix);

    // Release NewtonCollision
    NewtonReleaseCollision(FManager.FNewtonWorld, FCollision);

    // Set Link between glscene and newton
    NewtonBodySetUserdata(FNewtonBody, self);

    // Set position and orientation
    SetNewtonBodyMatrix(FOwnerBaseSceneObject.AbsoluteMatrix);

    // Set Collision
    UpdCollision;

  end;
end;

procedure TGLNGDBehaviour.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TGLNGDManager, FManagerName);
    if Assigned(mng) then
      Manager := TGLNGDManager(mng);
    FManagerName := '';
  end;

  if Assigned(FManager) then
  begin
    SetContinuousCollisionMode(FContinuousCollisionMode);
  end;
end;

class procedure TGLNGDBehaviour.NewtonCollisionIterator
  (const userData: Pointer; vertexCount: Integer; const faceArray: PNGDFloat;
  faceId: Integer)cdecl; static;
begin
  TGLNGDBehaviour(userData).FCollisionIteratorEvent(userData, vertexCount,
    faceArray, faceId);
end;

// Serializes are called by NGDBehaviour to save and load collision in file
// It's better to save/load big collisions [over 50000 polygones] to reduce
// loading time
class procedure TGLNGDBehaviour.NewtonDeserialize(serializeHandle,
  buffer: Pointer; size: Cardinal)cdecl; static;
begin
  TFileStream(serializeHandle).read(buffer^, size);
end;

class procedure TGLNGDBehaviour.NewtonSerialize(serializeHandle: Pointer;
  const buffer: Pointer; size: Cardinal)cdecl; static;

begin
  TFileStream(serializeHandle).write(buffer^, size);
end;

procedure TGLNGDBehaviour.OnCollisionIteratorEvent(const userData: Pointer;
  vertexCount: Integer; const cfaceArray: PNGDFloat; faceId: Integer);
var
  I: Integer;
  v0, v1: array [0 .. 2] of Single;
  vA: array of Single;
begin
  // This algorithme draw Collision Shape for Debuggin.
  // Taken to Sascha Willems in SDLNewton-Demo at
  // http://www.saschawillems.de/?page_id=82

  // Leave if there is no or to much vertex
  if (vertexCount = 0) then
    exit;

  SetLength(vA, vertexCount * 3);
  Move(cfaceArray^, vA[0], vertexCount * 3 * SizeOf(Single));
  v0[0] := vA[(vertexCount - 1) * 3];
  v0[1] := vA[(vertexCount - 1) * 3 + 1];
  v0[2] := vA[(vertexCount - 1) * 3 + 2];
  for I := 0 to vertexCount - 1 do
  begin
    v1[0] := vA[I * 3];
    v1[1] := vA[I * 3 + 1];
    v1[2] := vA[I * 3 + 2];
    FManager.AddNode(v0[0], v0[1], v0[2]);
    FManager.AddNode(v1[0], v1[1], v1[2]);
    v0 := v1;
  end;
end;

procedure TGLNGDBehaviour.Reinitialize;
begin
  if Initialized then
  begin
    // Set Appropriate NewtonCollision
    UpdCollision();
    // Set position and orientation
    SetNewtonBodyMatrix(FOwnerBaseSceneObject.AbsoluteMatrix);
  end;
  Loaded;
end;

procedure TGLNGDBehaviour.Render;
var
  M: TMatrix;
begin
  // Rebuild collision in design time
  if (csDesigning in FOwnerBaseSceneObject.ComponentState) then
    Reinitialize;

  if self is TGLNGDDynamic then
    FManager.FCurrentColor := FManager.DebugOption.GeomColorDyn
  else
    FManager.FCurrentColor := FManager.DebugOption.GeomColorStat;

  M := FOwnerBaseSceneObject.AbsoluteMatrix;

  if mdShowGeometry in FManager.DebugOption.NGDManagerDebugs then
    NewtonCollisionForEachPolygonDo(FCollision, @M,
      @TGLNGDBehaviour.NewtonCollisionIterator, self);
end;

// In this procedure, we assign collision to body
// [Because when initialised, the collision for body is type NULL]
procedure TGLNGDBehaviour.UpdCollision;
var
  collisionInfoRecord: TNewtonCollisionInfoRecord;
begin

  case FNGDNewtonCollisions of
    nc_Primitive:
      FCollision := GetPrimitiveCollision;
    nc_Convex:
      FCollision := GetConvexCollision;
    nc_BBox:
      FCollision := GetBBoxCollision;
    nc_BSphere:
      FCollision := GetBSphereCollision;
    nc_Tree:
      FCollision := GetTreeCollision;
    nc_Mesh:
      FCollision := GetMeshCollision;
    nc_Null:
      FCollision := GetNullCollision;
    nc_HeightField:
      FCollision := GetHeightFieldCollision;
    nc_NGDFile:
      FCollision := GetNGDFileCollision;
  end;

  if Assigned(FCollision) then
  begin
    NewtonBodySetCollision(FNewtonBody, FCollision);

    // The API Ask for releasing Collision to avoid memory leak
    NewtonCollisionGetInfo(FCollision, @collisionInfoRecord);
    if collisionInfoRecord.m_referenceCount > 2 then
      NewtonReleaseCollision(FManager.FNewtonWorld, FCollision);
  end;

end;

procedure TGLNGDBehaviour.SetContinuousCollisionMode(const Value: Boolean);
begin
  // for continue collision to be active the continue collision mode must on
  // the material pair of the colliding bodies as well as on at
  // least one of the two colliding bodies.
  // see NewtonBodySetContinuousCollisionMode
  // see NewtonMaterialSetContinuousCollisionMode
  FContinuousCollisionMode := Value;
  if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if Assigned(FManager) then
      NewtonBodySetContinuousCollisionMode(FNewtonBody, Ord(Value));
end;

procedure TGLNGDBehaviour.SetHeightFieldOptions(const Value: THeightField);
begin
  FHeightFieldOptions := Value;
  Reinitialize;
end;

procedure TGLNGDBehaviour.SetManager(Value: TGLNGDManager);
begin
  if FManager <> Value then
  begin
    if Assigned(FManager) then
    begin
      if Initialized then
        Finalize;
      FManager.FNGDBehaviours.Remove(self);
      // FManager.NotifyChange(self);
    end;
    FManager := Value;
    if Assigned(FManager) then
    begin
      Initialize;
      FManager.FNGDBehaviours.Add(self);
      FManager.NotifyChange(self);
    end;
  end;
end;

procedure TGLNGDBehaviour.SetNewtonBodyMatrix(const Value: TMatrix);
begin
  FNewtonBodyMatrix := Value;
  if Assigned(FManager) then
    NewtonBodySetmatrix(FNewtonBody, @FNewtonBodyMatrix);
end;

procedure TGLNGDBehaviour.SetNGDNewtonCollisions
  (const Value: TNGDNewtonCollisions);
begin
  FNGDNewtonCollisions := Value;
  if Assigned(FManager) then
    UpdCollision;
end;

procedure TGLNGDBehaviour.SetNGDSurfaceItem(const Value: TNGDSurfaceItem);
begin
  FNGDSurfaceItem := Value;
  FManager.RebuildAllMaterial;
end;

function TGLNGDBehaviour.StoredTolerance: Boolean;
begin
  Result := not SameValue(FConvexCollisionTolerance, 0.01, epsilon);
end;

class function TGLNGDBehaviour.UniqueItem: Boolean;
begin
  Result := True;
end;

procedure TGLNGDBehaviour.ReadFromFiler(reader: TReader);
var
  version: Integer;
begin
  inherited;
  with reader do
  begin
    version := ReadInteger; // read data version
    Assert(version <= 1); // Archive version

    FManagerName := ReadString;
    FContinuousCollisionMode := ReadBoolean;
    read(FNGDNewtonCollisions, SizeOf(TNGDNewtonCollisions));
    FTreeCollisionOptimize := ReadBoolean;
    if version <= 0 then
      FConvexCollisionTolerance := ReadSingle
    else
      FConvexCollisionTolerance := ReadFloat;
    FFileCollision := ReadString;
  end;
end;

procedure TGLNGDBehaviour.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(1); // Archive version
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
    WriteBoolean(FContinuousCollisionMode);
    write(FNGDNewtonCollisions, SizeOf(TNGDNewtonCollisions));
    WriteBoolean(FTreeCollisionOptimize);
    WriteFloat(FConvexCollisionTolerance);
    WriteString(FFileCollision);
  end;
end;

procedure TGLNGDBehaviour.Serialize(filename: string);
var
  MyFile: TFileStream;
begin
  MyFile := TFileStream.Create(filename, fmCreate or fmOpenReadWrite);

  NewtonCollisionSerialize(FManager.FNewtonWorld, FCollision,
    @TGLNGDBehaviour.NewtonSerialize, Pointer(MyFile));

  MyFile.Free;
end;

procedure TGLNGDBehaviour.DeSerialize(filename: string);
var
  MyFile: TFileStream;
  collisionInfoRecord: TNewtonCollisionInfoRecord;
begin
  MyFile := TFileStream.Create(filename, fmOpenRead);

  FCollision := NewtonCreateCollisionFromSerialization(FManager.FNewtonWorld,
    @TGLNGDBehaviour.NewtonDeserialize, Pointer(MyFile));

  // SetCollision;
  NewtonBodySetCollision(FNewtonBody, FCollision);

  // Release collision
  NewtonCollisionGetInfo(FCollision, @collisionInfoRecord);
  if collisionInfoRecord.m_referenceCount > 2 then
    NewtonReleaseCollision(FManager.FNewtonWorld, FCollision);

  MyFile.Free;
end;

{ TGLNGDDynamic }

procedure TGLNGDDynamic.AddImpulse(const veloc, pointposit: TVector);
begin
  if Assigned(FNewtonBody) then
    NewtonBodyAddImpulse(FNewtonBody, @veloc, @pointposit);
end;

constructor TGLNGDDynamic.Create(AOwner: TGLXCollection);
begin
  inherited;
  FAutoSleep := True;
  FLinearDamping := 0.1;
  FAngularDamping := TGLCoordinates.CreateInitialized(self,
    VectorMake(0.1, 0.1, 0.1, 0), csPoint);
  FAngularDamping.OnNotifyChange := NotifyAngularDampingChange;
  FDensity := 1;
  FVolume := 1;
  FForce := TGLCoordinates.CreateInitialized(self, NullHmgVector, csVector);
  FTorque := TGLCoordinates.CreateInitialized(self, NullHmgVector, csVector);
  FCenterOfMass := TGLCoordinates.CreateInitialized(self, NullHmgVector,
    csPoint);
  FCenterOfMass.OnNotifyChange := NotifyCenterOfMassChange;
  FAABBmin := TGLCoordinates.CreateInitialized(self, NullHmgVector, csPoint);
  FAABBmax := TGLCoordinates.CreateInitialized(self, NullHmgVector, csPoint);
  FAppliedOmega := TGLCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FAppliedVelocity := TGLCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FAppliedForce := TGLCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FAppliedTorque := TGLCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FUseGravity := True;
  FNullCollisionVolume := 0;

  FApplyForceAndTorqueEvent := OnApplyForceAndTorqueEvent;
  FSetTransformEvent := OnSetTransformEvent;
  name := 'NGD Dynamic'
end;

destructor TGLNGDDynamic.Destroy;
begin
  // Clean up everything
  FAngularDamping.Free;
  FForce.Free;
  FTorque.Free;
  FCenterOfMass.Free;
  FAABBmin.Free;
  FAABBmax.Free;
  FAppliedForce.Free;
  FAppliedTorque.Free;
  FAppliedVelocity.Free;
  FAppliedOmega.Free;
  inherited;
end;

procedure TGLNGDDynamic.Finalize;
begin
  if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if Assigned(FManager) then
    begin
      // Removing Callback
      NewtonBodySetForceAndTorqueCallback(FNewtonBody, nil);
      NewtonBodySetTransformCallback(FNewtonBody, nil);
    end;
  inherited;
end;

class function TGLNGDDynamic.FriendlyName: string;
begin
  Result := 'NGD Dynamic';
end;


procedure TGLNGDDynamic.Initialize;
begin
  inherited;
  if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if Assigned(FManager) then
    begin
      // Set Density, Mass and inertie matrix
      SetDensity(FDensity);

      // Set Callback
      NewtonBodySetForceAndTorqueCallback(FNewtonBody,
        @TGLNGDDynamic.NewtonApplyForceAndTorque);
      NewtonBodySetTransformCallback(FNewtonBody,
        @TGLNGDDynamic.NewtonSetTransform);
    end;
end;

procedure TGLNGDDynamic.Render;

  procedure DrawAABB(min, max: TGLCoordinates3);
  begin

    {
      //    H________G
      //   /.       /|
      //  / .      / |
      // D__._____C  |
      // |  .     |  |
      // | E.-----|--F
      // | .      | /
      // |.       |/
      // A________B
      }
    // Back
    FManager.AddNode(min.X, min.Y, min.Z); // E
    FManager.AddNode(max.X, min.Y, min.Z); // F

    FManager.AddNode(max.X, min.Y, min.Z); // F
    FManager.AddNode(max.X, max.Y, min.Z); // G

    FManager.AddNode(max.X, max.Y, min.Z); // G
    FManager.AddNode(min.X, max.Y, min.Z); // H

    FManager.AddNode(min.X, max.Y, min.Z); // H
    FManager.AddNode(min.X, min.Y, min.Z); // E

    // Front
    FManager.AddNode(min.X, min.Y, max.Z); // A
    FManager.AddNode(max.X, min.Y, max.Z); // B

    FManager.AddNode(max.X, min.Y, max.Z); // B
    FManager.AddNode(max.X, max.Y, max.Z); // C

    FManager.AddNode(max.X, max.Y, max.Z); // C
    FManager.AddNode(min.X, max.Y, max.Z); // D

    FManager.AddNode(min.X, max.Y, max.Z); // D
    FManager.AddNode(min.X, min.Y, max.Z); // A

    // Edges
    FManager.AddNode(min.X, min.Y, max.Z); // A
    FManager.AddNode(min.X, min.Y, min.Z); // E

    FManager.AddNode(max.X, min.Y, max.Z); // B
    FManager.AddNode(max.X, min.Y, min.Z); // F

    FManager.AddNode(max.X, max.Y, max.Z); // C
    FManager.AddNode(max.X, max.Y, min.Z); // G

    FManager.AddNode(min.X, max.Y, max.Z); // D
    FManager.AddNode(min.X, max.Y, min.Z); // H
  end;

  procedure DrawContact;
  var
    cnt: PNewtonJoint;
    thisContact: PNewtonJoint;
    material: PNewtonMaterial;
    pos, nor: TVector;
  begin
    FManager.FCurrentColor := FManager.DebugOption.ContactColor;
    cnt := NewtonBodyGetFirstContactJoint(FNewtonBody);
    while cnt <> nil do
    begin
      thisContact := NewtonContactJointGetFirstContact(cnt);
      while thisContact <> nil do
      begin
        material := NewtonContactGetMaterial(thisContact);
        NewtonMaterialGetContactPositionAndNormal(material, FNewtonBody, @pos, @nor);

        FManager.AddNode(pos);
        nor := VectorAdd(pos, nor);
        FManager.AddNode(nor);

        thisContact := NewtonContactJointGetNextContact(cnt, thisContact);
      end;
      cnt := NewtonBodyGetNextContactJoint(FNewtonBody, cnt);
    end;
  end;

  function GetAbsCom(): TVector;
  var
    M: TMatrix;
  begin
    NewtonBodyGetCentreOfMass(FNewtonBody, @Result);
    M := IdentityHmgMatrix;
    M.V[3] := Result;
    M.V[3].V[3] := 1;
    M := MatrixMultiply(M, FOwnerBaseSceneObject.AbsoluteMatrix);
    Result := M.V[3];
  end;

  procedure DrawForce;
  var
    pos: TVector;
    nor: TVector;
  begin
    pos := GetAbsCom;

    if mdShowForce in FManager.DebugOption.NGDManagerDebugs then
    begin
      FManager.FCurrentColor := FManager.DebugOption.ForceColor;
      nor := VectorAdd(pos, FForce.AsVector);
      FManager.AddNode(pos);
      FManager.AddNode(nor);
    end;

    if mdShowAppliedForce in FManager.DebugOption.NGDManagerDebugs then
    begin
      FManager.FCurrentColor := FManager.DebugOption.AppliedForceColor;
      nor := VectorAdd(pos, FAppliedForce.AsVector);
      FManager.AddNode(pos);
      FManager.AddNode(nor);

    end;

    if mdShowAppliedVelocity in FManager.DebugOption.NGDManagerDebugs then
    begin
      FManager.FCurrentColor := FManager.DebugOption.AppliedVelocityColor;
      nor := VectorAdd(pos, FAppliedVelocity.AsVector);
      FManager.AddNode(pos);
      FManager.AddNode(nor);
    end;

  end;

  procedure DrawCoM;
  var
    com: TVector;
    size: Single;
  begin
    FManager.FCurrentColor := FManager.DebugOption.CenterOfMassColor;
    size := FManager.DebugOption.DotAxisSize;
    com := GetAbsCom;
    FManager.AddNode(VectorAdd(com, VectorMake(0, 0, size)));
    FManager.AddNode(VectorAdd(com, VectorMake(0, 0, -size)));
    FManager.AddNode(VectorAdd(com, VectorMake(0, size, 0)));
    FManager.AddNode(VectorAdd(com, VectorMake(0, -size, 0)));
    FManager.AddNode(VectorAdd(com, VectorMake(size, 0, 0)));
    FManager.AddNode(VectorAdd(com, VectorMake(-size, 0, 0)));
  end;

begin
  inherited;

  // Move/Rotate NewtonObject if matrix are not equal in design time.
  if (csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if not MatrixEquals(NewtonBodyMatrix, FOwnerBaseSceneObject.AbsoluteMatrix)
      then
      SetNewtonBodyMatrix(FOwnerBaseSceneObject.AbsoluteMatrix);

  NewtonBodyGetAABB(FNewtonBody, @(FAABBmin.AsVector), @(FAABBmax.AsVector));

  if NewtonBodyGetSleepState(FNewtonBody) = 1 then
    FManager.FCurrentColor := FManager.DebugOption.AABBColorSleep
  else
    FManager.FCurrentColor := FManager.DebugOption.AABBColor;

  if mdShowAABB in FManager.DebugOption.NGDManagerDebugs then
    DrawAABB(FAABBmin, FAABBmax);

  if mdShowContact in FManager.DebugOption.NGDManagerDebugs then
    DrawContact;

  DrawForce; // Draw Force, AppliedForce and AppliedVelocity

  if mdShowCenterOfMass in FManager.DebugOption.NGDManagerDebugs then
    DrawCoM;
end;

procedure TGLNGDDynamic.SetAutoSleep(const Value: Boolean);
begin
  FAutoSleep := Value;
  if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if Assigned(FManager) then
      NewtonBodySetAutoSleep(FNewtonBody, Ord(FAutoSleep));
end;

procedure TGLNGDDynamic.SetDensity(const Value: Single);
var
  inertia: TVector;
  origin: TVector;
begin
  if Assigned(FManager) then
    if Value >= 0 then
    begin
      FDensity := Value;

      FVolume := NewtonConvexCollisionCalculateVolume(FCollision);
      NewtonConvexCollisionCalculateInertialMatrix(FCollision, @inertia,
        @origin);

      if IsZero(FVolume, epsilon) then
      begin
        FVolume := FNullCollisionVolume;
        inertia := VectorMake(FNullCollisionVolume, FNullCollisionVolume,
          FNullCollisionVolume, 0);
      end;

      FMass := FVolume * FDensity;

      if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
        NewtonBodySetMassMatrix(FNewtonBody, FMass, FMass * inertia.V[0],
          FMass * inertia.V[1], FMass * inertia.V[2]);

      FCenterOfMass.AsVector := origin;
    end;
end;

procedure TGLNGDDynamic.SetLinearDamping(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
    FLinearDamping := Value;
  if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if Assigned(FManager) then
      NewtonBodySetLinearDamping(FNewtonBody, FLinearDamping);
end;

function TGLNGDDynamic.GetOmega: TVector;
begin
  NewtonBodyGetOmega(FNewtonBody, @Result);
end;

procedure TGLNGDDynamic.SetOmega(const Omega: TVector);
begin
  NewtonBodySetOmega(FNewtonBody, @Omega);
end;

function TGLNGDDynamic.GetVelocity: TVector;
begin
  NewtonBodyGetVelocity(FNewtonBody, @Result);
end;

procedure TGLNGDDynamic.SetVelocity(const Velocity: TVector);
begin
  NewtonBodySetVelocity(FNewtonBody, @Velocity);
end;

function TGLNGDDynamic.StoredDensity: Boolean;
begin
  Result := not SameValue(FDensity, 1, epsilon);
end;

function TGLNGDDynamic.StoredLinearDamping: Boolean;
begin
  Result := not SameValue(FLinearDamping, 0.1, epsilon);
end;

function TGLNGDDynamic.StoredNullCollisionVolume: Boolean;
begin
  Result := not SameValue(FNullCollisionVolume, 0, epsilon);
end;

// WriteToFiler
//
procedure TGLNGDDynamic.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(1); // Archive version
    WriteBoolean(FAutoSleep);
    WriteFloat(FLinearDamping);
    WriteFloat(FDensity);
    WriteBoolean(FUseGravity);
    WriteFloat(FNullCollisionVolume);
  end;
  FForce.WriteToFiler(writer);
  FTorque.WriteToFiler(writer);
  FCenterOfMass.WriteToFiler(writer);
  FAngularDamping.WriteToFiler(writer);
end;

// ReadFromFiler
//
procedure TGLNGDDynamic.ReadFromFiler(reader: TReader);
var
  version: Integer;
begin
  inherited;
  with reader do
  begin
    version := ReadInteger; // read data version
    Assert(version <= 1); // Archive version

    FAutoSleep := ReadBoolean;
    if version <= 0 then
      FLinearDamping := ReadSingle
    else
      FLinearDamping := ReadFloat;
    if version <= 0 then
      FDensity := ReadSingle
    else
      FDensity := ReadFloat;

    // if Version >= 1 then
    FUseGravity := ReadBoolean;

    if version <= 0 then
      FNullCollisionVolume := ReadSingle
    else
      FNullCollisionVolume := ReadFloat;

  end;
  FForce.ReadFromFiler(reader);
  FTorque.ReadFromFiler(reader);
  FCenterOfMass.ReadFromFiler(reader);
  FAngularDamping.ReadFromFiler(reader);
end;

procedure TGLNGDDynamic.Loaded;
begin
  inherited;
  if Assigned(FManager) then
  begin
    SetAutoSleep(FAutoSleep);
    SetLinearDamping(FLinearDamping);
    SetDensity(FDensity);
    NotifyCenterOfMassChange(self);
    NotifyAngularDampingChange(self);
  end;
end;

class procedure TGLNGDDynamic.NewtonApplyForceAndTorque
  (const body: PNewtonBody; timestep: NGDFloat; threadIndex: Integer); cdecl; static;
begin
  TGLNGDDynamic(NewtonBodyGetUserData(body)).FApplyForceAndTorqueEvent(body,
    timestep, threadIndex);
end;

class procedure TGLNGDDynamic.NewtonSetTransform(const body: PNewtonBody;
  const matrix: PNGDFloat; threadIndex: Integer); cdecl; static;
begin
  TGLNGDDynamic(NewtonBodyGetUserData(body)).FSetTransformEvent(body, matrix,
    threadIndex);
end;

procedure TGLNGDDynamic.NotifyAngularDampingChange(Sender: TObject);
begin
  FAngularDamping.OnNotifyChange := nil;
  if (FAngularDamping.X >= 0) and (FAngularDamping.X <= 1) and
    (FAngularDamping.Y >= 0) and (FAngularDamping.Y <= 1) and
    (FAngularDamping.Z >= 0) and (FAngularDamping.Z <= 1) then
    if Assigned(FManager) then
      NewtonBodySetAngularDamping(FNewtonBody, @(FAngularDamping.AsVector));
  FAngularDamping.OnNotifyChange := NotifyAngularDampingChange;
end;

procedure TGLNGDDynamic.NotifyCenterOfMassChange(Sender: TObject);
begin
  FCenterOfMass.OnNotifyChange := nil;
  if Assigned(FManager) then
    NewtonBodySetCentreOfMass(FNewtonBody, @(FCenterOfMass.AsVector));
  FCenterOfMass.OnNotifyChange := NotifyCenterOfMassChange;
end;

procedure TGLNGDDynamic.OnApplyForceAndTorqueEvent(const cbody: PNewtonBody;
  timestep: NGDFloat; threadIndex: Integer);
var
  worldGravity: TVector;
begin

  // Read Only: We get the force and torque resulting from every interaction on this body
  NewtonBodyGetForce(cbody, @(FAppliedForce.AsVector));
  NewtonBodyGetTorque(cbody, @(FAppliedTorque.AsVector));

  NewtonBodyGetVelocity(cbody, @(FAppliedVelocity.AsVector));
  NewtonBodyGetOmega(cbody, @(FAppliedOmega.AsVector));

  // Raise Custom event
  if Assigned(FCustomForceAndTorqueEvent) then
    FCustomForceAndTorqueEvent(cbody, timestep, threadIndex)
  else
  begin
    NewtonBodySetForce(cbody, @(Force.AsVector));
    NewtonBodySetTorque(cbody, @(Torque.AsVector));

    // Add Gravity from World
    if FUseGravity then
    begin
      worldGravity := VectorScale(FManager.Gravity.AsVector, FMass);
      NewtonBodyAddForce(cbody, @(worldGravity));
    end;
  end;

end;

procedure TGLNGDDynamic.OnSetTransformEvent(const cbody: PNewtonBody;
  const cmatrix: PNGDFloat; threadIndex: Integer);
var
  epsi: Single;
begin
  // The Newton API does not support scale [scale modifie value in matrix],
  // so this line reset scale of the glsceneObject to (1,1,1)
  // to avoid crashing the application
  epsi := 0.0001;
  with FOwnerBaseSceneObject do
    if not SameValue(Scale.X, 1.0, epsi) or not SameValue(Scale.Y, 1.0, epsi)
      or not SameValue(Scale.Z, 1.0, epsi) then
    begin
      Scale.SetVector(1, 1, 1);
      SetNewtonBodyMatrix(AbsoluteMatrix);
    end
    else
      // Make the Position and orientation of the glscene-Object relative to the
      // NewtonBody position and orientation.
      FOwnerBaseSceneObject.AbsoluteMatrix := pMatrix(cmatrix)^;
end;

{ TGLNGDStatic }

procedure TGLNGDStatic.Render;
begin
  inherited;
  // Move/Rotate NewtonObject if matrix are not equal in run time.
  if not MatrixEquals(NewtonBodyMatrix, FOwnerBaseSceneObject.AbsoluteMatrix)
    then
    SetNewtonBodyMatrix(FOwnerBaseSceneObject.AbsoluteMatrix);

end;

class function TGLNGDStatic.FriendlyName: string;
begin
  Result := 'NGD Static';
end;

{ TNGDSurfaceItem }

function TNGDSurfaceItem.GetDisplayName: string;
begin
  if FDisplayName = '' then
    FDisplayName := 'Iron';
  Result := FDisplayName;
end;

procedure TNGDSurfaceItem.SetDisplayName(const Value: string);
begin
  inherited;
  FDisplayName := Value;
end;

{ TNGDSurfacePair }

constructor TNGDSurfacePair.Create(Collection: TCollection);
begin
  inherited;
  FSoftness := 0.1;
  FElasticity := 0.4;
  FCollidable := True;
  FStaticFriction := 0.9;
  FKineticFriction := 0.5;
  FContinuousCollisionMode := False;
  FThickness := False;

  FAABBOverlapEvent := OnNewtonAABBOverlapEvent;
  FContactProcessEvent := OnNewtonContactsProcessEvent;
  FManager := TGLNGDManager(Collection.Owner);
  FManager.RebuildAllMaterial;
end;

class function TNGDSurfacePair.NewtonAABBOverlap
  (const material: PNewtonMaterial;
  const body0, body1: PNewtonBody; threadIndex: Integer): Integer; cdecl; static;
begin
  Result := Ord(TNGDSurfacePair(NewtonMaterialGetMaterialPairUserData(material))
      .FAABBOverlapEvent(material, body0, body1, threadIndex));
end;

class procedure TNGDSurfacePair.NewtonContactsProcess
  (const contact: PNewtonJoint; timestep: NGDFloat; threadIndex: Integer); cdecl; static;
begin
  TNGDSurfacePair(NewtonMaterialGetMaterialPairUserData
   (NewtonContactGetMaterial
     (NewtonContactJointGetFirstContact(contact)))).FContactProcessEvent
	    (contact, timestep, threadIndex);
end;

function TNGDSurfacePair.OnNewtonAABBOverlapEvent
  (const cmaterial: PNewtonMaterial; const cbody0, cbody1: PNewtonBody;
  threadIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TNGDSurfacePair.OnNewtonContactsProcessEvent
  (const ccontact: PNewtonJoint; timestep: NGDFloat; threadIndex: Integer);
begin

end;

procedure TNGDSurfacePair.SetCollidable(const Value: Boolean);
begin
  FCollidable := Value;
  FManager.RebuildAllMaterial;
end;

procedure TNGDSurfacePair.SetContinuousCollisionMode(const Value: Boolean);
begin
  FContinuousCollisionMode := Value;
  FManager.RebuildAllMaterial;
end;

procedure TNGDSurfacePair.SetElasticity(const Value: Single);
begin
  if (Value >= 0) then
    FElasticity := Value;
  FManager.RebuildAllMaterial;
end;

procedure TNGDSurfacePair.SetKineticFriction(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
    FKineticFriction := Value;
  FManager.RebuildAllMaterial;
end;

procedure TNGDSurfacePair.SetMaterialItems(const item1, item2: TNGDSurfaceItem);
begin
  FNGDSurfaceItem1 := item1;
  FNGDSurfaceItem2 := item2;
  FManager.RebuildAllMaterial;
end;

procedure TNGDSurfacePair.SetSoftness(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
    FSoftness := Value;
  FManager.RebuildAllMaterial;
end;

procedure TNGDSurfacePair.SetStaticFriction(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
    FStaticFriction := Value;
  FManager.RebuildAllMaterial;
end;

procedure TNGDSurfacePair.SetThickness(const Value: Boolean);
begin
  FThickness := Value;
  FManager.RebuildAllMaterial;
end;

function TNGDSurfacePair.StoredElasticity: Boolean;
begin
  Result := not SameValue(FElasticity, 0.4, epsilon);
end;

function TNGDSurfacePair.StoredKineticFriction: Boolean;
begin
  Result := not SameValue(FKineticFriction, 0.5, epsilon);
end;

function TNGDSurfacePair.StoredSoftness: Boolean;
begin
  Result := not SameValue(FSoftness, 0.1, epsilon);
end;

function TNGDSurfacePair.StoredStaticFriction: Boolean;
begin
  Result := not SameValue(FStaticFriction, 0.9, epsilon);
end;

{ TNGDJoint }

constructor TNGDJoint.Create(Collection: TCollection);
begin
  inherited;
  FCollisionState := False;
  FStiffness := 0.9;
  FNewtonJoint := nil;
  FNewtonUserJoint := nil;
  FParentObject := nil;
  FChildObject := nil;

  FManager := TGLNGDManager(Collection.Owner);

  FBallAndSocketOptions := TNGDJointPivot.Create(FManager, self);
  FHingeOptions := TNGDJointPin.Create(FManager, self);
  FSliderOptions := TNGDJointPin.Create(FManager, self);
  FCorkscrewOptions := TNGDJointPin.Create(FManager, self);
  FUniversalOptions := TNGDJointPin2.Create(FManager, self);

  FCustomBallAndSocketOptions := TNGDJointBallAndSocket.Create(FManager, self);
  FCustomHingeOptions := TNGDJointHinge.Create(FManager, self);
  FCustomSliderOptions := TNGDJointSlider.Create(FManager, self);
  FKinematicOptions := TNGDJointKinematicController.Create;

  FUPVectorDirection := TGLCoordinates.CreateInitialized(self, YHmgVector,
    csVector);
  FUPVectorDirection.OnNotifyChange := FManager.RebuildAllJoint;
end;

destructor TNGDJoint.Destroy;
begin
  DestroyNewtonData;

  FParentObject := nil;
  FChildObject := nil;

  // Free options
  FBallAndSocketOptions.Free;
  FHingeOptions.Free;
  FSliderOptions.Free;
  FCorkscrewOptions.Free;
  FUniversalOptions.Free;

  FCustomBallAndSocketOptions.Free;
  FCustomHingeOptions.Free;
  FCustomSliderOptions.Free;
  FKinematicOptions.Free;
  FUPVectorDirection.Free;
  inherited;
end;

procedure TNGDJoint.DestroyNewtonData;
begin
  if FNewtonJoint <> nil then
  begin
    Assert((FManager <> nil) and (FManager.FNewtonWorld <> nil));
    NewtonDestroyJoint(FManager.FNewtonWorld, FNewtonJoint);
    FNewtonJoint := nil;
  end;
  if FNewtonUserJoint <> nil then
  begin
    CustomDestroyJoint(FNewtonUserJoint);
    FNewtonUserJoint := nil;
  end;
end;

procedure TNGDJoint.KinematicControllerPick(pickpoint: TVector;
  PickedActions: TNGDPickedActions);
begin
  if FJointType = nj_KinematicController then
    if Assigned(FParentObject) then
    begin
      // Create the joint
      if PickedActions = paAttach then
      begin
        if not Assigned(FNewtonUserJoint) then
          if Assigned(GetNGDDynamic(FParentObject).FNewtonBody) then
            FNewtonUserJoint := CreateCustomKinematicController
              (GetNGDDynamic(FParentObject).FNewtonBody, @pickpoint);
      end;

      // Change the TargetPoint
      if (PickedActions = paMove) or (PickedActions = paAttach) then
      begin
        if Assigned(FNewtonUserJoint) then
        begin
          CustomKinematicControllerSetPickMode(FNewtonUserJoint,
            Ord(FKinematicOptions.FPickModeLinear));
          CustomKinematicControllerSetMaxLinearFriction(FNewtonUserJoint,
            FKinematicOptions.FLinearFriction);
          CustomKinematicControllerSetMaxAngularFriction(FNewtonUserJoint,
            FKinematicOptions.FAngularFriction);
          CustomKinematicControllerSetTargetPosit(FNewtonUserJoint, @pickpoint);
        end;
      end;

      // Delete the joint
      if PickedActions = paDetach then
      begin
        if Assigned(FNewtonUserJoint) then
        begin
          CustomDestroyJoint(FNewtonUserJoint);
          FNewtonUserJoint := nil;
          // Reset autosleep because this joint turns it off
          NewtonBodySetAutoSleep(GetNGDDynamic(FParentObject).FNewtonBody,
            Ord(GetNGDDynamic(FParentObject).AutoSleep));
        end;
        ParentObject := nil;
      end;
    end;
end;

procedure TNGDJoint.Render;

  procedure DrawPivot(pivot: TVector);
  var
    size: Single;
  begin
    size := FManager.DebugOption.DotAxisSize;
    FManager.FCurrentColor := FManager.DebugOption.JointPivotColor;
    FManager.AddNode(VectorAdd(pivot, VectorMake(0, 0, size)));
    FManager.AddNode(VectorAdd(pivot, VectorMake(0, 0, -size)));
    FManager.AddNode(VectorAdd(pivot, VectorMake(0, size, 0)));
    FManager.AddNode(VectorAdd(pivot, VectorMake(0, -size, 0)));
    FManager.AddNode(VectorAdd(pivot, VectorMake(size, 0, 0)));
    FManager.AddNode(VectorAdd(pivot, VectorMake(-size, 0, 0)));
  end;

  procedure DrawPin(pin, pivot: TVector);
  begin
    FManager.FCurrentColor := FManager.DebugOption.JointAxisColor;
    FManager.AddNode(VectorAdd(pivot, pin));
    FManager.AddNode(VectorAdd(pivot, VectorNegate(pin)));
  end;

  procedure DrawJoint(pivot: TVector);
  begin
    FManager.FCurrentColor := FManager.DebugOption.CustomColor;
    FManager.AddNode(FParentObject.AbsolutePosition);
    FManager.AddNode(pivot);
    FManager.AddNode(pivot);
    FManager.AddNode(FChildObject.AbsolutePosition);
  end;

  procedure DrawKinematic;
  var
    pickedMatrix: TMatrix;
    size: Single;
  begin
    size := FManager.DebugOption.DotAxisSize;
    CustomKinematicControllerGetTargetMatrix(FNewtonUserJoint, @pickedMatrix);
    FManager.FCurrentColor := FManager.DebugOption.JointAxisColor;

    FManager.AddNode(FParentObject.AbsolutePosition);
    FManager.AddNode(pickedMatrix.V[3]);

    FManager.FCurrentColor := FManager.DebugOption.JointPivotColor;
    FManager.AddNode(VectorAdd(pickedMatrix.V[3], VectorMake(0, 0, size)));
    FManager.AddNode(VectorAdd(pickedMatrix.V[3], VectorMake(0, 0, -size)));
    FManager.AddNode(VectorAdd(pickedMatrix.V[3], VectorMake(0, size, 0)));
    FManager.AddNode(VectorAdd(pickedMatrix.V[3], VectorMake(0, -size, 0)));
    FManager.AddNode(VectorAdd(pickedMatrix.V[3], VectorMake(size, 0, 0)));
    FManager.AddNode(VectorAdd(pickedMatrix.V[3], VectorMake(-size, 0, 0)));

  end;

begin

  case FJointType of
    nj_BallAndSocket:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FBallAndSocketOptions.FPivotPoint.AsVector);
        DrawPivot(FBallAndSocketOptions.FPivotPoint.AsVector);
      end;

    nj_Hinge:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FHingeOptions.FPivotPoint.AsVector);
        DrawPin(FHingeOptions.FPinDirection.AsVector,
          FHingeOptions.FPivotPoint.AsVector);
        DrawPivot(FHingeOptions.FPivotPoint.AsVector);
      end;

    nj_Slider:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FSliderOptions.FPivotPoint.AsVector);
        DrawPin(FSliderOptions.FPinDirection.AsVector,
          FSliderOptions.FPivotPoint.AsVector);
        DrawPivot(FSliderOptions.FPivotPoint.AsVector);
      end;

    nj_Corkscrew:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FCorkscrewOptions.FPivotPoint.AsVector);
        DrawPin(FCorkscrewOptions.FPinDirection.AsVector,
          FCorkscrewOptions.FPivotPoint.AsVector);
        DrawPivot(FCorkscrewOptions.FPivotPoint.AsVector);
      end;

    nj_Universal:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FUniversalOptions.FPivotPoint.AsVector);
        DrawPin(FUniversalOptions.FPinDirection.AsVector,
          FUniversalOptions.FPivotPoint.AsVector);
        DrawPin(FUniversalOptions.FPinDirection2.AsVector,
          FUniversalOptions.FPivotPoint.AsVector);
        DrawPivot(FUniversalOptions.FPivotPoint.AsVector);
      end;

    nj_CustomBallAndSocket:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FCustomBallAndSocketOptions.FPivotPoint.AsVector);
        DrawPivot(FCustomBallAndSocketOptions.FPivotPoint.AsVector);
      end;

    nj_CustomHinge:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FCustomHingeOptions.FPivotPoint.AsVector);
        DrawPin(FCustomHingeOptions.FPinDirection.AsVector,
          FCustomHingeOptions.FPivotPoint.AsVector);
        DrawPivot(FCustomHingeOptions.FPivotPoint.AsVector);
      end;

    nj_CustomSlider:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FCustomSliderOptions.FPivotPoint.AsVector);
        DrawPin(FCustomSliderOptions.FPinDirection.AsVector,
          FCustomSliderOptions.FPivotPoint.AsVector);
        DrawPivot(FCustomSliderOptions.FPivotPoint.AsVector);
      end;

    nj_UpVector:
      if Assigned(FParentObject) then
      begin // special
        FManager.FCurrentColor := FManager.DebugOption.JointAxisColor;
        FManager.AddNode(FParentObject.AbsolutePosition);
        FManager.AddNode(VectorAdd(FParentObject.AbsolutePosition,
            FUPVectorDirection.AsVector));
      end;

    nj_KinematicController:
      if Assigned(FParentObject) and Assigned(FNewtonUserJoint) then
      begin // special
        DrawKinematic;
      end;

  end;
end;

procedure TNGDJoint.SetChildObject(const Value: TGLBaseSceneObject);
begin
  FChildObject := Value;
  FManager.RebuildAllJoint(self);
end;

procedure TNGDJoint.SetCollisionState(const Value: Boolean);
begin
  FCollisionState := Value;
  FManager.RebuildAllJoint(self);
end;

procedure TNGDJoint.SetJointType(const Value: TNGDNewtonJoints);
begin
  FJointType := Value;
  FManager.RebuildAllJoint(self);
end;

procedure TNGDJoint.SetParentObject(const Value: TGLBaseSceneObject);
begin
  FParentObject := Value;
  FManager.RebuildAllJoint(self);
end;

procedure TNGDJoint.SetStiffness(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
  begin
    FStiffness := Value;
    FManager.RebuildAllJoint(self);
  end;
end;

function TNGDJoint.StoredStiffness: Boolean;
begin
  Result := not SameValue(FStiffness, 0.9, epsilon);
end;

{ TNGDJoint.TNGDJointPivot }

constructor TNGDJointPivot.Create(AOwner: TComponent; aOuter: TNGDJoint);
begin
  FManager := AOwner as TGLNGDManager;
  FOuter := aOuter;
  FPivotPoint := TGLCoordinates.CreateInitialized(aOuter, NullHMGPoint,
    csPoint);
  FPivotPoint.OnNotifyChange := FManager.RebuildAllJoint;
end;

destructor TNGDJointPivot.Destroy;
begin
  FPivotPoint.Free;
  inherited;
end;

{ TNGDJoint.TNGDJointPin }

constructor TNGDJointPin.Create(AOwner: TComponent; aOuter: TNGDJoint);
begin
  inherited;
  FPinDirection := TGLCoordinates.CreateInitialized(aOuter, NullHmgVector,
    csVector);
  FPinDirection.OnNotifyChange := FManager.RebuildAllJoint;
end;

destructor TNGDJointPin.Destroy;
begin
  FPinDirection.Free;
  inherited;
end;

{ TNGDJoint.TNGDJointPin2 }

constructor TNGDJointPin2.Create(AOwner: TComponent; aOuter: TNGDJoint);
begin
  inherited;
  FPinDirection2 := TGLCoordinates.CreateInitialized(aOuter, NullHmgVector,
    csVector);
  FPinDirection2.OnNotifyChange := FManager.RebuildAllJoint;
end;

destructor TNGDJointPin2.Destroy;
begin
  FPinDirection2.Free;
  inherited;
end;

{ TNGDJoint.TNGDJointBallAndSocket }

constructor TNGDJointBallAndSocket.Create(AOwner: TComponent;
  aOuter: TNGDJoint);
begin
  inherited;
  FConeAngle := 90;
  FMinTwistAngle := -90;
  FMaxTwistAngle := 90;
end;

procedure TNGDJointBallAndSocket.SetConeAngle(const Value: Single);
begin
  FConeAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

procedure TNGDJointBallAndSocket.SetMaxTwistAngle(const Value: Single);
begin
  FMaxTwistAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

procedure TNGDJointBallAndSocket.SetMinTwistAngle(const Value: Single);
begin
  FMinTwistAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

function TNGDJointBallAndSocket.StoredConeAngle: Boolean;
begin
  Result := not SameValue(FConeAngle, 90, epsilon);
end;

function TNGDJointBallAndSocket.StoredMaxTwistAngle: Boolean;
begin
  Result := not SameValue(FMaxTwistAngle, 90, epsilon);
end;

function TNGDJointBallAndSocket.StoredMinTwistAngle: Boolean;
begin
  Result := not SameValue(FMinTwistAngle, -90, epsilon);
end;

{ TNGDJoint.TNGDJointHinge }

constructor TNGDJointHinge.Create(AOwner: TComponent; aOuter: TNGDJoint);
begin
  inherited;
  FMinAngle := -90;
  FMaxAngle := 90;
end;

procedure TNGDJointHinge.SetMaxAngle(const Value: Single);
begin
  FMaxAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

procedure TNGDJointHinge.SetMinAngle(const Value: Single);
begin
  FMinAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

function TNGDJointHinge.StoredMaxAngle: Boolean;
begin
  Result := not SameValue(FMaxAngle, 90, epsilon);
end;

function TNGDJointHinge.StoredMinAngle: Boolean;
begin
  Result := not SameValue(FMinAngle, -90, epsilon);
end;

{ TNGDJoint.TNGDJointSlider }


constructor TNGDJointSlider.Create(AOwner: TComponent; aOuter: TNGDJoint);
begin
  inherited;
  FMinDistance := -10;
  FMaxDistance := 10;
end;


procedure TNGDJointSlider.SetMaxDistance(const Value: Single);
begin
  FMaxDistance := Value;
  FManager.RebuildAllJoint(FOuter);
end;

procedure TNGDJointSlider.SetMinDistance(const Value: Single);
begin
  FMinDistance := Value;
  FManager.RebuildAllJoint(FOuter);
end;


function TNGDJointSlider.StoredMaxDistance: Boolean;
begin
  Result := not SameValue(FMaxDistance, 10, epsilon);
end;

function TNGDJointSlider.StoredMinDistance: Boolean;
begin
  Result := not SameValue(FMinDistance, -10, epsilon);
end;

{ TNGDJoint.TNGDJointKinematicController }

constructor TNGDJointKinematicController.Create;
begin
  FPickModeLinear := False;
  FLinearFriction := 750;
  FAngularFriction := 250;
end;

function TNGDJointKinematicController.StoredAngularFriction: Boolean;
begin
  Result := not SameValue(FAngularFriction, 250, epsilon);
end;

function TNGDJointKinematicController.StoredLinearFriction: Boolean;
begin
  Result := not SameValue(FLinearFriction, 750, epsilon);
end;

{ TGLNGDBehaviourList }

function TGLNGDBehaviourList.GetBehav(index: Integer): TGLNGDBehaviour;
begin
  Result := Items[index];
end;

procedure TGLNGDBehaviourList.PutBehav(index: Integer; Item: TGLNGDBehaviour);
begin
  inherited put(index, Item);
end;

initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

RegisterXCollectionItemClass(TGLNGDDynamic);
RegisterXCollectionItemClass(TGLNGDStatic);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

finalization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

UnregisterXCollectionItemClass(TGLNGDDynamic);
UnregisterXCollectionItemClass(TGLNGDStatic);

// CloseNGD;

end.
