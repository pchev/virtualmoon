{******************************************************************************
*                                                                             *
* version: mpl 1.1                                                            *
*                                                                             *
* the contents of this file are subject to the mozilla public license version *
* 1.1 (the "license"); you may not use this file except in compliance with    *
* the license. you may obtain a copy of the license at                        *
* http://www.mozilla.org/mpl/                                                 *
*                                                                             *
*                                                                             *
* software distributed under the license is distributed on an "as is" basis,  *
* without warranty of any kind, either express or implied. see the license    *
* for the specific language governing rights and limitations under the        *
* license.                                                                    *
*                                                                             *
* the original code is "oxglode opengl-physic-delphi-component".              *
*                                                                             *
* the initial developer of the original code is                               *
* dave gravel, orionx3d opengl & delphi programming, dave.gravel@cgocable.ca. *
*                       http://www.dave.serveusers.com/compo.html             *
*                       http://www.k00m.sexidude.com/compo.html               *
*                       http://k00m.sytes.net/compo.html                      *
*                       http://24.122.8.52/compo.html                         *
*                                                                             *
* portions created by dave gravel are copyright (c) 2004 - 2005.              *
* dave gravel. all rights reserved.                                           *
*                                                                             *
* contributor(s): glscene (http://www.glscene.org) - ode (http://ode.org) -   *
* delphiode (http://www.cambrianlabs.com/mattias/delphiode) -                 *
* stuart gooding (n/a)                                                        *
*                                                                             *
* i request all modifications or corrections or changes plz.                  *
* e-mail: dave.gravel@cgocable.ca                                             *
*                                                                             *
*******************************************************************************
* oxglode v1.0pre by k00m. (dave gravel)                                      *
*******************************************************************************}
{******************************************************************************}
unit gloxode;

{$ifdef fpc}
{$mode delphi}
{$optimization on}
{$m+}
{$endif}
{$i glscene.inc}
{******************************************************************************}
interface
{******************************************************************************}
uses
  {$ifdef mswindows}
  windows, messages,
  {$endif}
  sysutils, variants, classes, graphics, controls, extctrls,
  dialogs, math, vectorgeometry, globjects, glmisc, glscene, glvectorfileobjects,
  vectorlists, persistentclasses, opengl1x, gltexture, glmesh, meshutils, glstate,
  xcollection, glgeomobjects, glverletclothify, xopengl, glterrainrenderer,
  vectortypes, odegl, odeimport, glcrossplatform, glcontext, glsilhouette, tga,
  jpeg, GLColor;
{******************************************************************************}
const
  num = ( 20 );
{******************************************************************************}
type
  toxodemode = ( mdquickstep, mdstepfast1, mdnormalstep );
  toxodemodes = set of toxodemode;
  toxmode = ( mdslip2, mdslip1, mdsoftcfm, mdsofterp, mdbounce, mdfdir1, mdmu2 );
  toxmodes = set of toxmode;
  toxcartraction = ( mdfront, mdback, mdboth );
  toxworldsurfacemode = ( mdwsoftsurface, mdwhardsurface, mdwboundsurface,
  mdwdefaultsurface, mdwnonesurface  );
  toxobjectssurfacemode = ( mdosoftsurface, mdohardsurface, mdoboundsurface,
  mdodefaultsurface, mdononesurface );
{******************************************************************************}
type
  TOXOnCustomCollisionEvent = procedure ( Geom1, Geom2 : PdxGeom ) of object;
  TOXOnCollisionEvent = procedure ( Geom1, Geom2 : PdxGeom ) of object;
  TOXOnMultiStepRender = procedure ( delta : TdReal ) of object;
  TOXOnStepRender = procedure ( delta : TdReal ) of object;
  TOXOnEventTimer = procedure ( Sender : TObject ) of object;
  TOnCallBack = procedure ( Geom1, Geom2 : PdxGeom ) of object;
type
  TGLVisibilityDeterminationEvent = function ( Sender : TObject;
                              var rci : TRenderContextInfo ) : Boolean of object;
  TGLSizableDummyCube = class ( TGLCameraInvariantObject )
    private
    { Private Declarations }
      FCubeSizable : TAffineVector;
      FEdgeColor : TGLColor;
      FVisibleAtRunTime, FAmalgamate : Boolean;
      FGroupList : TGLListHandle;
      FOnVisibilityDetermination : TGLVisibilityDeterminationEvent;
      procedure SetCubeWidth( const aValue : Single );
      procedure SetCubeHeight( const aValue : Single );
      procedure SetCubeDepth( const aValue : Single );
    protected
    { Protected Declarations }
      procedure SetEdgeColor( const val : TGLColor );
      procedure SetVisibleAtRunTime( const val : Boolean );
      procedure SetAmalgamate( const val : Boolean );
    public
    { Public Declarations }
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
      procedure Assign( Source: TPersistent ); override;
      function AxisAlignedDimensionsUnscaled : TVector; override;
      function RayCastIntersect( const rayStart, rayVector : TVector;
      intersectPoint : PVector = nil;
      intersectNormal : PVector = nil ) : Boolean; override;
      procedure BuildList( var rci : TRenderContextInfo ); override;
      procedure DoRender( var rci : TRenderContextInfo; renderSelf,
      renderChildren : Boolean ); override;
      procedure StructureChanged; override;
      function BarycenterAbsolutePosition : TVector; override;
    published
    { Published Declarations }
      property CubeWidth : TGLFloat read FCubeSizable[0] write
      SetCubeWidth stored False;
      property CubeHeight : TGLFloat read FCubeSizable[1] write
      SetCubeHeight stored False;
      property CubeDepth : TGLFloat read FCubeSizable[2] write
      SetCubeDepth stored False;
      property EdgeColor : TGLColor read FEdgeColor write SetEdgeColor;
      property VisibleAtRunTime : Boolean read FVisibleAtRunTime write
      SetVisibleAtRunTime default False;
      property Amalgamate : Boolean read FAmalgamate write
      SetAmalgamate default False;
      property CamInvarianceMode default cimNone;
      property OnVisibilityDetermination : TGLVisibilityDeterminationEvent read
      FOnVisibilityDetermination write FOnVisibilityDetermination;
  end;
  procedure SizableDummyWireframeBuildList( var rci : TRenderContextInfo;
                                   size : TAffineVector; stipple : Boolean;
                                   const color : TColorVector );
{******************************************************************************}
 // [2005-06-08]: TGLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
type
  TGLOXOdeEngine = class( TGLSizableDummyCube )
    private
      FSurface : TdSurfaceParameters;
      FActived : boolean;
      FInit : boolean;
      FObjsList : TList;
      FWorld : PdxWorld;
      FSpace : PdxSpace;
      FContactGroup : TdJointGroupID;
      FContactGroupNum : integer;
      FContactNum : integer;
      FCustomObjsContact : boolean;
      FCustomObjsSurface : boolean;
      FGravity : TGLCoordinates;
      FODETime : TdReal;
      FODEStepTime : TdReal;
      FODEMultiplyTime : TdReal;
      FODEQuickStepNum : integer;
      FMaxCorrectingVel : TdReal;
      FSurfaceLayer : TdReal;
      FMode : integer;
      FModes : TOXModes;
      FMdMode : integer;
      FWorldModes : TOXOdeModes;
      FOdeModes : integer;
      FApprox0 : boolean;
      FApprox1_1 : boolean;
      FApprox1_2 : boolean;
      FApprox1 : boolean;
      FObjsColliding : boolean;
      FOnOdeStepRender : TOXOnMultiStepRender;
      FOnStepRender : TOXOnStepRender;
      FOnCustomCollisionEvent : TOXOnCustomCollisionEvent;
      FOnODECollisionEvent : TOXOnCollisionEvent;
      FContactMaxCorrectingVel : boolean;
      FContactSurfaceLayer : boolean;
      FTypeBall : boolean;
      FTypeHinge : boolean;
      FTypeSlider : boolean;
      FTypeContact : boolean;
      FTypeUniversal : boolean;
//      FTypeHinge2 : boolean;
      FTypeFixed : boolean;
      FTypeAMotor : boolean;
      FAreConnected : boolean;
      FWorldSurfaceMode : TOXWorldSurfaceMode;
      function GetWorldSurfaceMode : TOXWorldSurfaceMode;
      procedure SetWorldSurfaceMode( const val : TOXWorldSurfaceMode );
      procedure SetMaxCorrectingVel( const val : TdReal );
      procedure SetSurfaceLayer( const val : TdReal );
      procedure SetContactMaxCorrectingVel( const val : boolean );
      procedure SetContactSurfaceLayer( const val : boolean );
      procedure GravityChange( Sender : TObject );
      procedure SetTypeBall( const val : boolean );
      procedure SetTypeHinge( const val : boolean );
      procedure SetTypeSlider( const val : boolean );
      procedure SetTypeContact( const val : boolean );
      procedure SetTypeUniversal( const val : boolean );
//      procedure SetTypeHinge2( const val : boolean );
      procedure SetTypeFixed( const val : boolean );
      procedure SetTypeAMotor( const val : boolean );
      procedure SetAreConnected( const val : boolean );
      procedure SetODEGravity( val : TGLCoordinates );
      procedure SetODEStepTime( const val : TdReal );
      procedure SetODEMultiplyTime( const val : TdReal );
      procedure SetODEContactgroupNum( const val : integer );
      procedure SetODEQuickStepNum( const val : integer );
      procedure SetODEContactNum( const val : integer );
      procedure SetModes( const val : TOXModes );
      procedure SetWorldModes( const val: TOXOdeModes );
      procedure SetMu( const val : TdReal );
      procedure SetMu2( const val : TdReal );
      procedure SetSlip1( const val : TdReal );
      procedure SetSlip2( const val : TdReal );
      procedure SetBounce( const val : TdReal );
      procedure SetBounce_vel( const val : TdReal );
      procedure SetSoft_erp( const val : TdReal );
      procedure SetSoft_cfm( const val : TdReal );
      procedure SetMotion1( const val : TdReal );
      procedure SetMotion2( const val : TdReal );
      procedure SetCustomObjsContact( const val : boolean );
      procedure SetCustomObjsSurface( const val : boolean );
      procedure SetApprox0( const val : boolean );
      procedure SetApprox1_1( const val : boolean );
      procedure SetApprox1_2( const val : boolean );
      procedure SetApprox1( const val : boolean );
      function GetModes : TOXModes;
      function GetWorldModes : TOXOdeModes;
      function GetMu : TdReal;
      function GetMu2 : TdReal;
      function GetSlip1 : TdReal;
      function GetSlip2 : TdReal;
      function GetBounce : TdReal;
      function GetBounce_Vel : TdReal;
      function GetSoft_erp : TdReal;
      function GetSoft_cfm : TdReal;
      function GetMotion1 : TdReal;
      function GetMotion2 : TdReal;
      function GetWorld : PdxWorld;
      function GetSpace : PdxSpace;
    protected
      procedure Callback( g1, g2 : PdxGeom );
    public
      procedure InitODE;
      property World : PdxWorld read GetWorld;
      property Space : PdxSpace read GetSpace;
      procedure BuildList( var rci : TRenderContextInfo ); override;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property OnCustomCollisionEvent : TOXOnCustomCollisionEvent read
      FOnCustomCollisionEvent write FOnCustomCollisionEvent;
      property OnCollisionEvent : TOXOnCollisionEvent read
      FOnODECollisionEvent write FOnODECollisionEvent;
      property OnMultiStepRender : TOXOnMultiStepRender read
      FOnOdeStepRender write FOnOdeStepRender;
      property OnStepRender : TOXOnStepRender read
      FOnStepRender write FOnStepRender;
      property CustomObjsContact : boolean read
      FCustomObjsContact write SetCustomObjsContact;
      property CustomObjsSurface : boolean
      read FCustomObjsSurface write SetCustomObjsSurface;
      property Gravity : TGLCoordinates read FGravity write SetODEGravity;
      property StepTime : TdReal read FODEStepTime write SetODEStepTime;
      property MultiplyTime : TdReal read FODEMultiplyTime write
      SetODEMultiplyTime;
      property ContactGroupNum : integer read FContactgroupNum write
      SetODEContactgroupNum;
      property QuickStepNum : integer read FODEQuickStepNum write
      SetODEQuickStepNum;
      property ContactNum : integer read FContactNum write SetODEContactNum;
      property Modes : TOXModes read GetModes write SetModes;
      property WorldModes : TOXOdeModes read GetWorldModes write SetWorldModes;
      property Mu : TdReal read GetMu write SetMu;
      property Mu2 : TdReal read GetMu2 write SetMu2;
      property Slip1 : TdReal read Getslip1 write Setslip1;
      property Slip2 : TdReal read Getslip2 write Setslip2;
      property Bounce : TdReal read Getbounce write Setbounce;
      property Bounce_vel : TdReal read Getbounce_vel write Setbounce_vel;
      property Soft_erp : TdReal read Getsoft_erp write Setsoft_erp;
      property Soft_cfm : TdReal read Getsoft_cfm write Setsoft_cfm;
      property Motion1 : TdReal read Getmotion1 write Setmotion1;
      property Motion2 : TdReal read Getmotion2 write Setmotion2;
      property Approx0 : boolean read FApprox0 write SetApprox0;
      property Approx1_1 : boolean read FApprox1_1 write SetApprox1_1;
      property Approx1_2 : boolean read FApprox1_2 write SetApprox1_2;
      property Approx1 : boolean read FApprox1 write SetApprox1;
      property ContactMaxCorrectingVel : boolean read
      FContactMaxCorrectingVel write SetContactMaxCorrectingVel;
      property ContactSurfaceLayer : boolean read
      FContactSurfaceLayer write SetContactSurfaceLayer;
      property MaxCorrectingVel : TdReal read FMaxCorrectingVel write
      SetMaxCorrectingVel;
      property SurfaceLayer : TdReal read FSurfaceLayer write SetSurfaceLayer;
      property RemoveTypeBall : boolean read FTypeBall write SetTypeBall;
      property RemoveTypeHinge : boolean read FTypeHinge write SetTypeHinge;
      property RemoveTypeSlider : boolean read FTypeSlider write SetTypeSlider;
      property RemoveTypeContact : boolean read FTypeContact write
      SetTypeContact;
      property RemoveTypeUniversal : boolean read FTypeUniversal write
      SetTypeUniversal;
//      property RemoveTypeHinge2 : boolean read FTypeHinge2 write SetTypeHinge2;
      property RemoveTypeFixed : boolean read FTypeFixed write SetTypeFixed;
      property RemoveTypeAMotor : boolean read FTypeAMotor write SetTypeAMotor;
      property RemoveTypeAreConnected : boolean read FAreConnected write
      SetAreConnected;
      property WorldSurfaceMode : TOXWorldSurfaceMode read
      GetWorldSurfaceMode write SetWorldSurfaceMode;
  end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBox last change by Dave Gravel
{******************************************************************************}
type
  TGLOXStaBox = class( TGLCube )
    private
      FManager : TGLOXOdeEngine;
      FActived : boolean;
      FGeom : PdxGeom;
      procedure SetManager( val : TGLOXOdeEngine );
      function GetGeom : PdxGeom;
      procedure FreeOde;
    protected
    public
      property Geom : PdxGeom read GetGeom;
      procedure InitODE;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      procedure BuildList( var rci : TRenderContextInfo ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Manager : TGLOXOdeEngine read FManager write SetManager;
  end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBall last change by Dave Gravel
{******************************************************************************}
type
  TGLOXStaBall = class( TGLSphere )
    private
      FManager : TGLOXOdeEngine;
      FActived : boolean;
      FGeom : PdxGeom;
      procedure SetManager( val : TGLOXOdeEngine );
      function GetGeom : PdxGeom;
      procedure FreeOde;
    protected
    public
      property Geom : PdxGeom read GetGeom;
      procedure InitODE;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      procedure BuildList( var rci : TRenderContextInfo ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Manager : TGLOXOdeEngine read FManager write SetManager;
  end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCylinder last change by Dave Gravel
{******************************************************************************}
type
  TGLOXStaCylinder = class( TGLCylinder )
    private
      FManager : TGLOXOdeEngine;
      FActived : boolean;
      FGeom : PdxGeom;
      procedure SetManager( val : TGLOXOdeEngine );
      function GetGeom : PdxGeom;
      procedure FreeOde;
    protected
    public
      property Geom : PdxGeom read GetGeom;
      procedure InitODE;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      procedure BuildList( var rci : TRenderContextInfo ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Manager : TGLOXOdeEngine read FManager write SetManager;
  end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCCylinder last change by Dave Gravel
{******************************************************************************}
{type
  TGLOXStaCCylinder = class( TGLCylinder )
    private
      FManager : TGLOXOdeEngine;
      FActived : boolean;
      FCap1 : TGLSphere;
      FCap2 : TGLSphere;
      FGeom : PdxGeom;
      procedure SetManager( val : TGLOXOdeEngine );
      function GetGeom : PdxGeom;
      function GetCap1 : TGLSphere;
      function GetCap2 : TGLSphere;
      procedure FreeOde;
    protected
    public
      property Geom : PdxGeom read GetGeom;
      procedure InitODE;
      property Cap1 : TGLSphere read GetCap1;
      property Cap2 : TGLSphere read GetCap2;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      procedure BuildList( var rci : TRenderContextInfo ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Manager : TGLOXOdeEngine read FManager write SetManager;
  end;}
{******************************************************************************}
 // [2005-06-08]: GLOXZStaTerrain last change by Dave Gravel
{******************************************************************************}
type
  TCorner = class
    private
      FPos : TdVector3;
      FDepth : single;
    public
  end;
////////////////////////////////////////////////////////////////////////////////
type
  TGLOXZStaTerrain = class( TGLTerrainRenderer )
    private
      FManager : TGLOXOdeEngine;
      FActived : boolean;
      FCustomColliderClass : TdGeomClass;
      FNewClassNum : Integer;
      FNewCollider : PdxGeom;
      FCornerCache : array[0..7] of TCorner;
      FCornerList : TList;
      FTerrainAllocMem : integer;
      function GetGeom : PdxGeom;
      procedure FreeOde;
      procedure SetManager( val : TGLOXOdeEngine );
    protected
      function ColliderFormula(x, y : Single) : Single;
      function ColliderFormulaNormal( x, y : Single ) : TAffineVector;
    public
      property Geom : PdxGeom read GetGeom;
      procedure InitODE;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Manager : TGLOXOdeEngine read FManager write SetManager;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaMesh last change by Dave Gravel
{******************************************************************************}
type
  TGLOXStaMesh = class( TGLFreeForm )
    private
      FManager : TGLOXOdeEngine;
      FActived : boolean;
      FGeom : PdxGeom;
      Fvertices : PdVector3Array;
      Findices : PdIntegerArray;
      FTriMeshData : PdxTriMeshData;
      FMem1 : integer;
      FMem2 : integer;
      procedure FreeOde;
      function  BuildTriMeshMesh( GLBaseMesh : TGLBaseMesh; Space : PdxSpace;
      var Vertices : PdVector3Array; var Indices : PdIntegerArray ) : PdxGeom;
      procedure SetManager( val : TGLOXOdeEngine );
      function GetGeom : PdxGeom;
    protected
    public
      procedure InitODE;
      property Geom : PdxGeom read GetGeom;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Manager : TGLOXOdeEngine read FManager write SetManager;
  end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCone last change by Dave Gravel
{******************************************************************************}
{type
  TGLOXStaCone = class( TGLCone )
    private
      FManager : TGLOXOdeEngine;
      FActived : boolean;
      FGeom : PdxGeom;
      procedure SetManager( val : TGLOXOdeEngine );
      function GetGeom : PdxGeom;
      procedure FreeOde;
    protected
    public
      property Geom : PdxGeom read GetGeom;
      procedure InitODE;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      procedure BuildList( var rci : TRenderContextInfo ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Manager : TGLOXOdeEngine read FManager write SetManager;
end;}
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
type
  TGLOXDynBall = class( TGLSphere )
    private
      FAutoDisable : boolean;
      FManager : TGLOXOdeEngine;
      FActived : boolean;
      FSurface : TdSurfaceParameters;
      FDensity : TdReal;
      FCoeff : TdReal;
      FGeom : PdxGeom;
      FBody : PdxBody;
      FContactNum : integer;
      FFakeRollForce : boolean;
      FModes : TOXModes;
      FMdMode : integer;
      FApprox0 : boolean;
      FApprox1_1 : boolean;
      FApprox1_2 : boolean;
      FApprox1 : boolean;
      FMass : TdMass;
      FMassReal : TdReal;
      FFrictionForce : TdReal;
      FIgnoreSelfModel : boolean;
      FDisableThreshold : TdReal;
      FDisableSteps : TdReal;
      FWb_stepsdis : integer;
      FOnCallBack : TOnCallBack;
      FObjectsSurfaceMode : TOXObjectsSurfaceMode;
      function GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
      procedure SetObjectsSurfaceMode( const val : TOXObjectsSurfaceMode );
      procedure AutoDisabling;
      procedure FakeRollingFriction;
      procedure SetMass( const val : TdReal );
      procedure SetRollFriction( const val : boolean );
      procedure SetFrictionForce( const val : TdReal );
      procedure SetAutoDisable( const val : boolean );
      procedure SetDisableThreshold( const val : TdReal );
      procedure SetDisableSteps( const val : TdReal );
      procedure SetModes( const val : TOXModes );
      procedure SetMu( const val : TdReal );
      procedure SetMu2( const val : TdReal );
      procedure SetSlip1( const val : TdReal );
      procedure SetSlip2( const val : TdReal );
      procedure SetBounce( const val : TdReal );
      procedure SetBounce_vel( const val : TdReal );
      procedure SetSoft_erp( const val : TdReal );
      procedure SetSoft_cfm( const val : TdReal );
      procedure SetMotion1( const val : TdReal );
      procedure SetMotion2( const val : TdReal );
      procedure SetContactNum( const val : integer );
      procedure SetDensity( const val : TdReal );
      procedure SetApprox0( const val : boolean );
      procedure SetApprox1_1( const val : boolean );
      procedure SetApprox1_2( const val : boolean );
      procedure SetApprox1( const val : boolean );
      procedure SetIgnoreSelfModel( const val : boolean );
      procedure SetManager( const val : TGLOXOdeEngine );
      function GetGeom : PdxGeom;
      function GetBody : PdxBody;
      function GetDensity : TdReal;
      function GetModes : TOXModes;
      function GetMu : TdReal;
      function GetMu2 : TdReal;
      function GetSlip1 : TdReal;
      function GetSlip2 : TdReal;
      function GetBounce : TdReal;
      function GetBounce_Vel : TdReal;
      function GetSoft_erp : TdReal;
      function GetSoft_cfm : TdReal;
      function GetMotion1 : TdReal;
      function GetMotion2 : TdReal;
      procedure FreeOde;
    protected
    public
      property Geom : PdxGeom read GetGeom;
      property Body : PdxBody read GetBody;
      procedure InitODE;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      procedure BuildList( var rci : TRenderContextInfo ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Density : TdReal read GetDensity write SetDensity;
      property Modes : TOXModes read GetModes write SetModes;
      property Mu : TdReal read GetMu write SetMu;
      property Mu2 : TdReal read GetMu2 write SetMu2;
      property Slip1 : TdReal read Getslip1 write Setslip1;
      property Slip2 : TdReal read Getslip2 write Setslip2;
      property Bounce : TdReal read Getbounce write Setbounce;
      property Bounce_vel : TdReal read Getbounce_vel write Setbounce_vel;
      property Soft_erp : TdReal read Getsoft_erp write Setsoft_erp;
      property Soft_cfm : TdReal read Getsoft_cfm write Setsoft_cfm;
      property Motion1 : TdReal read Getmotion1 write Setmotion1;
      property Motion2 : TdReal read Getmotion2 write Setmotion2;
      property Approx0 : boolean read FApprox0 write SetApprox0;
      property Approx1_1 : boolean read FApprox1_1 write SetApprox1_1;
      property Approx1_2 : boolean read FApprox1_2 write SetApprox1_2;
      property Approx1 : boolean read FApprox1 write SetApprox1;
      property ContactNum : integer read FContactNum write SetContactNum;
      property IgnoreSelfModel : boolean read FIgnoreSelfModel write
      SeTIgnoreSelfModel;
      property DisableThreshold : TdReal read FDisableThreshold write
      SetDisableThreshold;
      property DisableSteps : TdReal read FDisableSteps write SetDisableSteps;
      property Manager : TGLOXOdeEngine read FManager write SetManager;
      property AutoDisable : boolean read FAutoDisable write SetAutoDisable;
      property FrictionForce : TdReal read FFrictionForce write
      SetFrictionForce;
      property RollFriction : boolean read FFakeRollForce write SetRollFriction;
      property Mass : TdReal read FMassReal write SetMass;
      property OnCallBack : TOnCallBack read FOnCallBack write FOnCallBack;
      property ObjectsSurfaceMode : TOXObjectsSurfaceMode read
      GetObjectsSurfaceMode write SetObjectsSurfaceMode;
  end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
type
  TGLOXDynBox = class( TGLCube )
    private
      FAutoDisable : boolean;
      FManager : TGLOXOdeEngine;
      FActived : boolean;
      FSurface : TdSurfaceParameters;
      FDensity : TdReal;
      FGeom : PdxGeom;
      FBody : PdxBody;
      FCoeff : TdReal;
      FMass : TdMass;
      FMassReal : TdReal;
      FFakeRollForce : boolean;
      FFrictionForce : TdReal;
      FContactNum : integer;
      FModes : TOXModes;
      FMdMode : integer;
      FApprox0 : boolean;
      FApprox1_1 : boolean;
      FApprox1_2 : boolean;
      FApprox1 : boolean;
      FIgnoreSelfModel : boolean;
      FDisableThreshold : TdReal;
      FDisableSteps : TdReal;
      FWb_stepsdis : integer;
      FOnCallBack : TOnCallBack;
      FObjectsSurfaceMode : TOXObjectsSurfaceMode;
      function GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
      procedure SetObjectsSurfaceMode( const val : TOXObjectsSurfaceMode );
      procedure AutoDisabling;
      procedure FakeRollingFriction;
      procedure SetMass( const val : TdReal );
      procedure SetRollFriction( const val : boolean );
      procedure SetFrictionForce( const val : TdReal );
      procedure SetAutoDisable( const val : boolean );
      procedure SetDisableThreshold( const val : TdReal );
      procedure SetDisableSteps( const val : TdReal );
      procedure SetModes( const val : TOXModes );
      procedure SetMu( const val : TdReal );
      procedure SetMu2( const val : TdReal );
      procedure SetSlip1( const val : TdReal );
      procedure SetSlip2( const val : TdReal );
      procedure SetBounce( const val : TdReal );
      procedure SetBounce_vel( const val : TdReal );
      procedure SetSoft_erp( const val : TdReal );
      procedure SetSoft_cfm( const val : TdReal );
      procedure SetMotion1( const val : TdReal );
      procedure SetMotion2( const val : TdReal );
      procedure SetContactNum( const val : integer );
      procedure SetDensity( const val : TdReal );
      procedure SetApprox0( const val : boolean );
      procedure SetApprox1_1( const val : boolean );
      procedure SetApprox1_2( const val : boolean );
      procedure SetApprox1( const val : boolean );
      procedure SetIgnoreSelfModel( const val : boolean );
      procedure SetManager( const val : TGLOXOdeEngine );
      function GetGeom : PdxGeom;
      function GetBody : PdxBody;
      function GetDensity : TdReal;
      function GetModes : TOXModes;
      function GetMu : TdReal;
      function GetMu2 : TdReal;
      function GetSlip1 : TdReal;
      function GetSlip2 : TdReal;
      function GetBounce : TdReal;
      function GetBounce_Vel : TdReal;
      function GetSoft_erp : TdReal;
      function GetSoft_cfm : TdReal;
      function GetMotion1 : TdReal;
      function GetMotion2 : TdReal;
      procedure FreeOde;
    protected
    public
      property Geom : PdxGeom read GetGeom;
      property Body : PdxBody read GetBody;
      procedure InitODE;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      procedure BuildList( var rci : TRenderContextInfo ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Density : TdReal read GetDensity write SetDensity;
      property Modes : TOXModes read GetModes write SetModes;
      property Mu : TdReal read GetMu write SetMu;
      property Mu2 : TdReal read GetMu2 write SetMu2;
      property Slip1 : TdReal read Getslip1 write Setslip1;
      property Slip2 : TdReal read Getslip2 write Setslip2;
      property Bounce : TdReal read Getbounce write Setbounce;
      property Bounce_vel : TdReal read Getbounce_vel write Setbounce_vel;
      property Soft_erp : TdReal read Getsoft_erp write Setsoft_erp;
      property Soft_cfm : TdReal read Getsoft_cfm write Setsoft_cfm;
      property Motion1 : TdReal read Getmotion1 write Setmotion1;
      property Motion2 : TdReal read Getmotion2 write Setmotion2;
      property Approx0 : boolean read FApprox0 write SetApprox0;
      property Approx1_1 : boolean read FApprox1_1 write SetApprox1_1;
      property Approx1_2 : boolean read FApprox1_2 write SetApprox1_2;
      property Approx1 : boolean read FApprox1 write SetApprox1;
      property ContactNum : integer read FContactNum write SetContactNum;
      property IgnoreSelfModel : boolean read FIgnoreSelfModel write
      SeTIgnoreSelfModel;
      property DisableThreshold : TdReal read FDisableThreshold write
      SetDisableThreshold;
      property DisableSteps : TdReal read FDisableSteps write SetDisableSteps;
      property Manager : TGLOXOdeEngine read FManager write SetManager;
      property AutoDisable : boolean read FAutoDisable write SetAutoDisable;
      property FrictionForce : TdReal read FFrictionForce write
      SetFrictionForce;
      property RollFriction : boolean read FFakeRollForce write SetRollFriction;
      property Mass : TdReal read FMassReal write SetMass;
      property OnCallBack : TOnCallBack read FOnCallBack write FOnCallBack;
      property ObjectsSurfaceMode : TOXObjectsSurfaceMode read
      GetObjectsSurfaceMode write SetObjectsSurfaceMode;
  end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
type
  TGLOXDynCylinder = class( TGLCylinder )
    private
      FAutoDisable : boolean;
      FManager : TGLOXOdeEngine;
      FActived : boolean;
      FSurface : TdSurfaceParameters;
      FFrictionForce : TdReal;
      FDensity : TdReal;
      FGeom : PdxGeom;
      FBody : PdxBody;
      FCoeff : TdReal;
      FMass : TdMass;
      FMassReal : TdReal;
      FFakeRollForce : boolean;
      FContactNum : integer;
      FModes : TOXModes;
      FMdMode : integer;
      FApprox0 : boolean;
      FApprox1_1 : boolean;
      FApprox1_2 : boolean;
      FApprox1 : boolean;
      FIgnoreSelfModel : boolean;
      FDisableThreshold : TdReal;
      FDisableSteps : TdReal;
      FWb_stepsdis : integer;
      FOnCallBack : TOnCallBack;
      FObjectsSurfaceMode : TOXObjectsSurfaceMode;
      function GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
      procedure SetObjectsSurfaceMode( const val : TOXObjectsSurfaceMode );
      procedure AutoDisabling;
      procedure FakeRollingFriction;
      procedure SetMass( const val : TdReal );
      procedure SetFrictionForce( const val : TdReal );
      procedure SetRollFriction( const val : boolean );
      procedure SetAutoDisable( const val : boolean );
      procedure SetDisableThreshold( const val : TdReal );
      procedure SetDisableSteps( const val : TdReal );
      procedure SetModes( const val : TOXModes );
      procedure SetMu( const val : TdReal );
      procedure SetMu2( const val : TdReal );
      procedure SetSlip1( const val : TdReal );
      procedure SetSlip2( const val : TdReal );
      procedure SetBounce( const val : TdReal );
      procedure SetBounce_vel( const val : TdReal );
      procedure SetSoft_erp( const val : TdReal );
      procedure SetSoft_cfm( const val : TdReal );
      procedure SetMotion1( const val : TdReal );
      procedure SetMotion2( const val : TdReal );
      procedure SetContactNum( const val : integer );
      procedure SetDensity( const val : TdReal );
      procedure SetApprox0( const val : boolean );
      procedure SetApprox1_1( const val : boolean );
      procedure SetApprox1_2( const val : boolean );
      procedure SetApprox1( const val : boolean );
      procedure SetIgnoreSelfModel( const val : boolean );
      procedure SetManager( val : TGLOXOdeEngine );
      function GetGeom : PdxGeom;
      function GetBody : PdxBody;
      function GetDensity : TdReal;
      function GetModes : TOXModes;
      function GetMu : TdReal;
      function GetMu2 : TdReal;
      function GetSlip1 : TdReal;
      function GetSlip2 : TdReal;
      function GetBounce : TdReal;
      function GetBounce_Vel : TdReal;
      function GetSoft_erp : TdReal;
      function GetSoft_cfm : TdReal;
      function GetMotion1 : TdReal;
      function GetMotion2 : TdReal;
      procedure FreeOde;
    protected
    public
      property Geom : PdxGeom read GetGeom;
      property Body : PdxBody read GetBody;
      procedure InitODE;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      procedure BuildList( var rci : TRenderContextInfo ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Density : TdReal read GetDensity write SetDensity;
      property Modes : TOXModes read GetModes write SetModes;
      property Mu : TdReal read GetMu write SetMu;
      property Mu2 : TdReal read GetMu2 write SetMu2;
      property Slip1 : TdReal read Getslip1 write Setslip1;
      property Slip2 : TdReal read Getslip2 write Setslip2;
      property Bounce : TdReal read Getbounce write Setbounce;
      property Bounce_vel : TdReal read Getbounce_vel write Setbounce_vel;
      property Soft_erp : TdReal read Getsoft_erp write Setsoft_erp;
      property Soft_cfm : TdReal read Getsoft_cfm write Setsoft_cfm;
      property Motion1 : TdReal read Getmotion1 write Setmotion1;
      property Motion2 : TdReal read Getmotion2 write Setmotion2;
      property Approx0 : boolean read FApprox0 write SetApprox0;
      property Approx1_1 : boolean read FApprox1_1 write SetApprox1_1;
      property Approx1_2 : boolean read FApprox1_2 write SetApprox1_2;
      property Approx1 : boolean read FApprox1 write SetApprox1;
      property ContactNum : integer read FContactNum write SetContactNum;
      property IgnoreSelfModel : boolean read FIgnoreSelfModel write
      SetIgnoreSelfModel;
      property DisableThreshold : TdReal read FDisableThreshold write
      SetDisableThreshold;
      property DisableSteps : TdReal read FDisableSteps write SetDisableSteps;
      property Manager : TGLOXOdeEngine read FManager write SetManager;
      property AutoDisable : boolean read FAutoDisable write SetAutoDisable;
      property FrictionForce : TdReal read FFrictionForce write
      SetFrictionForce;
      property RollFriction : boolean read FFakeRollForce write SetRollFriction;
      property Mass : TdReal read FMassReal write SetMass;
      property OnCallBack : TOnCallBack read FOnCallBack write FOnCallBack;
      property ObjectsSurfaceMode : TOXObjectsSurfaceMode read
      GetObjectsSurfaceMode write SetObjectsSurfaceMode;
  end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
{type
  TGLOXDynCCylinder = class( TGLCylinder )
    private
      FAutoDisable : boolean;
      FManager : TGLOXOdeEngine;
      FActived : boolean;
      FSurface : TdSurfaceParameters;
      FDensity : TdReal;
      FGeom : PdxGeom;
      FBody : PdxBody;
      FCap1 : TGLSphere;
      FCap2 : TGLSphere;
      FCoeff : TdReal;
      FMass : TdMass;
      FMassReal : TdReal;
      FFakeRollForce : boolean;
      FFrictionForce : TdReal;
      FContactNum : integer;
      FModes : TOXModes;
      FMdMode : integer;
      FApprox0 : boolean;
      FApprox1_1 : boolean;
      FApprox1_2 : boolean;
      FApprox1 : boolean;
      FIgnoreSelfModel : boolean;
      FDisableThreshold : TdReal;
      FDisableSteps : TdReal;
      FWb_stepsdis : integer;
      FOnCallBack : TOnCallBack;
      FObjectsSurfaceMode : TOXObjectsSurfaceMode;
      function GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
      procedure SetObjectsSurfaceMode( const val : TOXObjectsSurfaceMode );
      procedure AutoDisabling;
      procedure FakeRollingFriction;
      procedure SetMass( const val : TdReal );
      procedure SetFrictionForce( const val : TdReal );
      procedure SetRollFriction( const val : boolean );
      procedure SetAutoDisable( const val : boolean );
      procedure SetDisableThreshold( const val : TdReal );
      procedure SetDisableSteps( const val : TdReal );
      procedure SetModes( const val : TOXModes );
      procedure SetMu( const val : TdReal );
      procedure SetMu2( const val : TdReal );
      procedure SetSlip1( const val : TdReal );
      procedure SetSlip2( const val : TdReal );
      procedure SetBounce( const val : TdReal );
      procedure SetBounce_vel( const val : TdReal );
      procedure SetSoft_erp( const val : TdReal );
      procedure SetSoft_cfm( const val : TdReal );
      procedure SetMotion1( const val : TdReal );
      procedure SetMotion2( const val : TdReal );
      procedure SetContactNum( const val : integer );
      procedure SetDensity( const val : TdReal );
      procedure SetApprox0( const val : boolean );
      procedure SetApprox1_1( const val : boolean );
      procedure SetApprox1_2( const val : boolean );
      procedure SetApprox1( const val : boolean );
      procedure SetIgnoreSelfModel( const val : boolean );
      procedure SetManager( const val : TGLOXOdeEngine );
      function GetGeom : PdxGeom;
      function GetBody : PdxBody;
      function GetCap1 : TGLSphere;
      function GetCap2 : TGLSphere;
      function GetDensity : TdReal;
      function GetModes : TOXModes;
      function GetMu : TdReal;
      function GetMu2 : TdReal;
      function GetSlip1 : TdReal;
      function GetSlip2 : TdReal;
      function GetBounce : TdReal;
      function GetBounce_Vel : TdReal;
      function GetSoft_erp : TdReal;
      function GetSoft_cfm : TdReal;
      function GetMotion1 : TdReal;
      function GetMotion2 : TdReal;
      procedure FreeOde;
    protected
    public
      property Geom : PdxGeom read GetGeom;
      property Body : PdxBody read GetBody;
      property Cap1 : TGLSphere read GetCap1;
      property Cap2 : TGLSphere read GetCap2;
      procedure InitODE;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      procedure BuildList( var rci : TRenderContextInfo ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Density : TdReal read GetDensity write SetDensity;
      property Modes : TOXModes read GetModes write SetModes;
      property Mu : TdReal read GetMu write SetMu;
      property Mu2 : TdReal read GetMu2 write SetMu2;
      property Slip1 : TdReal read Getslip1 write Setslip1;
      property Slip2 : TdReal read Getslip2 write Setslip2;
      property Bounce : TdReal read Getbounce write Setbounce;
      property Bounce_vel : TdReal read Getbounce_vel write Setbounce_vel;
      property Soft_erp : TdReal read Getsoft_erp write Setsoft_erp;
      property Soft_cfm : TdReal read Getsoft_cfm write Setsoft_cfm;
      property Motion1 : TdReal read Getmotion1 write Setmotion1;
      property Motion2 : TdReal read Getmotion2 write Setmotion2;
      property Approx0 : boolean read FApprox0 write SetApprox0;
      property Approx1_1 : boolean read FApprox1_1 write SetApprox1_1;
      property Approx1_2 : boolean read FApprox1_2 write SetApprox1_2;
      property Approx1 : boolean read FApprox1 write SetApprox1;
      property ContactNum : integer read FContactNum write SetContactNum;
      property IgnoreSelfModel : boolean read FIgnoreSelfModel write
      SeTIgnoreSelfModel;
      property DisableThreshold : TdReal read FDisableThreshold write
      SetDisableThreshold;
      property DisableSteps : TdReal read FDisableSteps write SetDisableSteps;
      property Manager : TGLOXOdeEngine read FManager write SetManager;
      property AutoDisable : boolean read FAutoDisable write SetAutoDisable;
      property FrictionForce : TdReal read FFrictionForce write
      SetFrictionForce;
      property RollFriction : boolean read FFakeRollForce write SetRollFriction;
      property Mass : TdReal read FMassReal write SetMass;
      property OnCallBack : TOnCallBack read FOnCallBack write FOnCallBack;
      property ObjectsSurfaceMode : TOXObjectsSurfaceMode read
      GetObjectsSurfaceMode write SetObjectsSurfaceMode;
  end;}
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
{type
  TGLOXDynCone = class( TGLCone )
    private
      FAutoDisable : boolean;
      FManager : TGLOXOdeEngine;
      FSurface : TdSurfaceParameters;
      FActived : boolean;
      FDensity : TdReal;
      FGeom : PdxGeom;
      FBody : PdxBody;
      FCoeff : TdReal;
      FMass : TdMass;
      FMassReal : TdReal;
      FFakeRollForce : boolean;
      FFrictionForce : TdReal;
      FContactNum : integer;
      FModes : TOXModes;
      FMdMode : integer;
      FApprox0 : boolean;
      FApprox1_1 : boolean;
      FApprox1_2 : boolean;
      FApprox1 : boolean;
      FIgnoreSelfModel : boolean;
      FDisableThreshold : TdReal;
      FDisableSteps : TdReal;
      FWb_stepsdis : integer;
      FOnCallBack : TOnCallBack;
      FObjectsSurfaceMode : TOXObjectsSurfaceMode;
      function GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
      procedure SetObjectsSurfaceMode( const val : TOXObjectsSurfaceMode );
      procedure FreeOde;
      procedure AutoDisabling;
      procedure FakeRollingFriction;
      procedure SetMass( const val : TdReal );
      procedure SetRollFriction( const val : boolean );
      procedure SetFrictionForce( const val : TdReal );
      procedure SetAutoDisable( const val : boolean );
      procedure SetDisableThreshold( const val : TdReal );
      procedure SetDisableSteps( const val : TdReal );
      procedure dMassSetCone(var m : TdMass; const density,
      radius, length : TdReal);
      procedure SetModes( const val : TOXModes );
      procedure SetMu( const val : TdReal );
      procedure SetMu2( const val : TdReal );
      procedure SetSlip1( const val : TdReal );
      procedure SetSlip2( const val : TdReal );
      procedure SetBounce( const val : TdReal );
      procedure SetBounce_vel( const val : TdReal );
      procedure SetSoft_erp( const val : TdReal );
      procedure SetSoft_cfm( const val : TdReal );
      procedure SetMotion1( const val : TdReal );
      procedure SetMotion2( const val : TdReal );
      procedure SetContactNum( const val : integer );
      procedure SetDensity( const val : TdReal );
      procedure SetApprox0( const val : boolean );
      procedure SetApprox1_1( const val : boolean );
      procedure SetApprox1_2( const val : boolean );
      procedure SetApprox1( const val : boolean );
      procedure SetIgnoreSelfModel( const val : boolean );
      procedure SetManager( val : TGLOXOdeEngine );
      function GetGeom : PdxGeom;
      function GetBody : PdxBody;
      function GetDensity : TdReal;
      function GetModes : TOXModes;
      function GetMu : TdReal;
      function GetMu2 : TdReal;
      function GetSlip1 : TdReal;
      function GetSlip2 : TdReal;
      function GetBounce : TdReal;
      function GetBounce_Vel : TdReal;
      function GetSoft_erp : TdReal;
      function GetSoft_cfm : TdReal;
      function GetMotion1 : TdReal;
      function GetMotion2 : TdReal;
    protected
    public
      procedure InitODE;
      property Geom : PdxGeom read GetGeom;
      property Body : PdxBody read GetBody;
      procedure BuildList( var rci : TRenderContextInfo ); override;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Density : TdReal read GetDensity write SetDensity;
      property Modes : TOXModes read GetModes write SetModes;
      property Mu : TdReal read GetMu write SetMu;
      property Mu2 : TdReal read GetMu2 write SetMu2;
      property Slip1 : TdReal read Getslip1 write Setslip1;
      property Slip2 : TdReal read Getslip2 write Setslip2;
      property Bounce : TdReal read Getbounce write Setbounce;
      property Bounce_vel : TdReal read Getbounce_vel write Setbounce_vel;
      property Soft_erp : TdReal read Getsoft_erp write Setsoft_erp;
      property Soft_cfm : TdReal read Getsoft_cfm write Setsoft_cfm;
      property Motion1 : TdReal read Getmotion1 write Setmotion1;
      property Motion2 : TdReal read Getmotion2 write Setmotion2;
      property Approx0 : boolean read FApprox0 write SetApprox0;
      property Approx1_1 : boolean read FApprox1_1 write SetApprox1_1;
      property Approx1_2 : boolean read FApprox1_2 write SetApprox1_2;
      property Approx1 : boolean read FApprox1 write SetApprox1;
      property ContactNum : integer read FContactNum write SetContactNum;
      property IgnoreSelfModel : boolean read FIgnoreSelfModel write
      SeTIgnoreSelfModel;
      property DisableThreshold : TdReal read FDisableThreshold write
      SetDisableThreshold;
      property DisableSteps : TdReal read FDisableSteps write SetDisableSteps;
      property Manager : TGLOXOdeEngine read FManager write SetManager;
      property AutoDisable : boolean read FAutoDisable write SetAutoDisable;
      property FrictionForce : TdReal read FFrictionForce write
      SetFrictionForce;
      property RollFriction : boolean read FFakeRollForce write SetRollFriction;
      property Mass : TdReal read FMassReal write SetMass;
      property OnCallBack : TOnCallBack read FOnCallBack write FOnCallBack;
      property ObjectsSurfaceMode : TOXObjectsSurfaceMode read
      GetObjectsSurfaceMode write SetObjectsSurfaceMode;
end;}
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
type
  TGLOXDynMesh = class( TGLFreeForm )
    private
      FAutoDisable : boolean;
      FManager : TGLOXOdeEngine;
      FActived : boolean;
      FSurface : TdSurfaceParameters;
      FPos : TGLCoordinates;
      FGeom : PdxGeom;
      FBody : PdxBody;
      FCoeff : TdReal;
      FDensity : TdReal;
      Fvertices : PdVector3Array;
      Findices : PdIntegerArray;
      FTriMeshData : PdxTriMeshData;
      FMem1 : integer;
      FMem2 : integer;
      FMass : TdMass;
      FMassReal : TdReal;
      FFakeRollForce : boolean;
      FContactNum : integer;
      FModes : TOXModes;
      FMdMode : integer;
      FApprox0 : boolean;
      FApprox1_1 : boolean;
      FApprox1_2 : boolean;
      FApprox1 : boolean;
      FIgnoreSelfModel : boolean;
      FDisableThreshold : TdReal;
      FDisableSteps : TdReal;
      FFrictionForce : TdReal;
      FWb_stepsdis : integer;
      FOnCallBack : TOnCallBack;
      FObjectsSurfaceMode : TOXObjectsSurfaceMode;
      function GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
      procedure SetObjectsSurfaceMode( const val : TOXObjectsSurfaceMode );
      procedure FreeOde;
      procedure AutoDisabling;
      procedure FakeRollingFriction;
      procedure SetMass( const val : TdReal );
      procedure SetRollFriction( const val : boolean );
      procedure SetFrictionForce( const val : TdReal );
      procedure SetAutoDisable( const val : boolean );
      function  BuildTriMeshMesh( GLBaseMesh : TGLBaseMesh; Space : PdxSpace;
      var Vertices : PdVector3Array; var Indices : PdIntegerArray ) : PdxGeom;
      procedure SetDisableThreshold( const val : TdReal );
      procedure SetDisableSteps( const val : TdReal );
      procedure SetModes( const val : TOXModes );
      procedure SetMu( const val : TdReal );
      procedure SetMu2( const val : TdReal );
      procedure SetSlip1( const val : TdReal );
      procedure SetSlip2( const val : TdReal );
      procedure SetBounce( const val : TdReal );
      procedure SetBounce_vel( const val : TdReal );
      procedure SetSoft_erp( const val : TdReal );
      procedure SetSoft_cfm( const val : TdReal );
      procedure SetMotion1( const val : TdReal );
      procedure SetMotion2( const val : TdReal );
      procedure SetContactNum( const val : integer );
      procedure SetApprox0( const val : boolean );
      procedure SetApprox1_1( const val : boolean );
      procedure SetApprox1_2( const val : boolean );
      procedure SetApprox1( const val : boolean );
      procedure SetIgnoreSelfModel( const val : boolean );
      procedure SetManager( val : TGLOXOdeEngine );
      procedure SetDensity( const val : TdReal );
      function GetGeom : PdxGeom;
      function GetBody : PdxBody;
      function GetModes : TOXModes;
      function GetMu : TdReal;
      function GetMu2 : TdReal;
      function GetSlip1 : TdReal;
      function GetSlip2 : TdReal;
      function GetBounce : TdReal;
      function GetBounce_Vel : TdReal;
      function GetSoft_erp : TdReal;
      function GetSoft_cfm : TdReal;
      function GetMotion1 : TdReal;
      function GetMotion2 : TdReal;
      function GetDensity : TdReal;
    protected
    public
      procedure InitODE;
      property Geom : PdxGeom read GetGeom;
      property Body : PdxBody read GetBody;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      procedure BuildList( var rci : TRenderContextInfo ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Density : TdReal read GetDensity write SetDensity;
      property Modes : TOXModes read GetModes write SetModes;
      property Mu : TdReal read GetMu write SetMu;
      property Mu2 : TdReal read GetMu2 write SetMu2;
      property Slip1 : TdReal read Getslip1 write Setslip1;
      property Slip2 : TdReal read Getslip2 write Setslip2;
      property Bounce : TdReal read Getbounce write Setbounce;
      property Bounce_vel : TdReal read Getbounce_vel write Setbounce_vel;
      property Soft_erp : TdReal read Getsoft_erp write Setsoft_erp;
      property Soft_cfm : TdReal read Getsoft_cfm write Setsoft_cfm;
      property Motion1 : TdReal read Getmotion1 write Setmotion1;
      property Motion2 : TdReal read Getmotion2 write Setmotion2;
      property Approx0 : boolean read FApprox0 write SetApprox0;
      property Approx1_1 : boolean read FApprox1_1 write SetApprox1_1;
      property Approx1_2 : boolean read FApprox1_2 write SetApprox1_2;
      property Approx1 : boolean read FApprox1 write SetApprox1;
      property ContactNum : integer read FContactNum write SetContactNum;
      property IgnoreSelfModel : boolean read FIgnoreSelfModel write
      SeTIgnoreSelfModel;
      property DisableThreshold : TdReal read FDisableThreshold write
      SetDisableThreshold;
      property DisableSteps : TdReal read FDisableSteps write SetDisableSteps;
      property Manager : TGLOXOdeEngine read FManager write SetManager;
      property AutoDisable : boolean read FAutoDisable write SetAutoDisable;
      property FrictionForce : TdReal read FFrictionForce write
      SetFrictionForce;
      property RollFriction : boolean read FFakeRollForce write SetRollFriction;
      property Mass : TdReal read FMassReal write SetMass;
      property OnCallBack : TOnCallBack read FOnCallBack write FOnCallBack;
      property ObjectsSurfaceMode : TOXObjectsSurfaceMode read
      GetObjectsSurfaceMode write SetObjectsSurfaceMode;
  end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
{type
   TGLOXDynCar = class( TGLSizableDummyCube )
    private
      FAutoDisable : boolean;
      FManager : TGLOXOdeEngine;
      FSurface : TdSurfaceParameters;
      FContactNum : integer;
      FModes : TOXModes;
      FMdMode : integer;
      FApprox0 : boolean;
      FApprox1_1 : boolean;
      FApprox1_2 : boolean;
      FApprox1 : boolean;
      FActived : boolean;
      FFrontWheelLimit : TdReal;
      FFrontWheelTurnDegre : TdReal;
      FFrontWheelBackUp : TdReal;
      FFrontBackWheelRunForce : TdReal;
      FFrontBackWheelAltForce : TdReal;
      FCoeff : TdReal;
      FDensity : TdReal;
      FBody : array[0..4] of PdxBody;
      FJoint : array[0..3] of TdJointID;
      FFrame : array[0..0] of PdxGeom;
      FWheel : array[0..3] of PdxGeom;
      FDummyFrame : TGLSizableDummyCube;
      FWheelLF : TGLSizableDummyCube;
      FWheelLB : TGLSizableDummyCube;
      FWheelRF : TGLSizableDummyCube;
      FWheelRB : TGLSizableDummyCube;
      FMass1 : TdMass;
      FMass2 : TdMass;
      FCMASSReal : TdReal;
      FWMASSReal : TdReal;
      FZOOM : TdReal;
      FZOOM_CUBED : TdReal;
      FLENGTH : TdReal;
      FWIDTH : TdReal;
      FHEIGHT : TdReal;
      FRADIUS : TdReal;
      FSTARTZ : TdReal;
      FWHEEL_WOBBLE : TdReal;
      FFRONTSUSPENSION_CFM : TdReal;
      FFRONTSUSPENSION_ERP : TdReal;
      FBACKSUSPENSION_CFM : TdReal;
      FBACKSUSPENSION_ERP : TdReal;
      FWHEEL_OFFSET : TdReal;
      FACCEL : TdReal;
      FTURN_SPEED : TdReal;
      FSteer : TdReal;
      FSpeed : TdReal;
      FWheelsParamFMax : TdReal;
      FWheelsFrontLoHiStop : TdReal;
      FWheelsBackLoHiStop : TdReal;
      FIgnoreSelfModel : boolean;
      FFakeRollForce : boolean;
      FFrictionForce : TdReal;
      FDisableThreshold : TdReal;
      FDisableSteps : TdReal;
      FWb_stepsdis : integer;
      FMotorRunning : boolean;
      FOnCallBack : TOnCallBack;
      FWPosA : TGLCoordinates;
      FWPosB : TGLCoordinates;
      FWPosC : TGLCoordinates;
      FWPosD : TGLCoordinates;
      FModeTraction : TOXCarTraction;
      FObjectsSurfaceMode : TOXObjectsSurfaceMode;
      function GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
      procedure SetObjectsSurfaceMode( const val : TOXObjectsSurfaceMode );
      function GetDensity : TdReal;
      procedure DoMotor( delta : double );
      procedure AutoDisabling;
      procedure SetDensity( const val : TdReal );
      procedure SetSpeed( val : TdReal );
      procedure SetSteer( val : TdReal );
      procedure SetMotorRunning ( const val : boolean );
      procedure FakeRollingFriction;
      procedure SetFrictionForce( const val : TdReal );
      procedure SetRollFriction( const val : boolean );
      procedure SetDisableThreshold( const val : TdReal );
      procedure SetDisableSteps( const val : TdReal );
      procedure SetFrontWheelLimit( const val : TdReal );
      procedure SetFrontWheelTurnDegre( const val : TdReal );
      procedure SetFrontWheelBackUp( const val : TdReal );
      procedure SetFrontBackWheelRunForce( const val : TdReal );
      procedure SetFrontBackWheelAltForce( const val : TdReal );
      procedure SetModeTraction( const val : TOXCarTraction );
      procedure SetWheelPosA( val : TGLCoordinates );
      procedure SetWheelPosB( val : TGLCoordinates );
      procedure SetWheelPosC( val : TGLCoordinates );
      procedure SetWheelPosD( val : TGLCoordinates );
      procedure SetAutoDisable( const val : boolean );
      procedure SetACCEL( const val : TdReal );
      procedure SetTURN_SPEED( const val : TdReal );
      procedure SetZOOM( const val : TdReal );
      procedure SetZOOM_CUBED( const val : TdReal );
      procedure SetLENGTH( const val : TdReal );
      procedure SetWIDTH( const val : TdReal );
      procedure SetHEIGHT( const val : TdReal );
      procedure SetRADIUS( const val: TdReal );
      procedure SetCMASS( const val: TdReal );
      procedure SetWMASS( const val: TdReal );
      procedure SetFRONTSUSPENSION_ERP( const val: TdReal );
      procedure SetFRONTSUSPENSION_CFM( const val: TdReal );
      procedure SetBACKSUSPENSION_ERP( const val: TdReal );
      procedure SetBACKSUSPENSION_CFM( const val: TdReal );
      procedure SetWHEEL_WOBBLE( const val: TdReal );
      procedure SetWHEEL_OFFSET( const val: TdReal );
      procedure SetIgnoreSelfModel( const val : boolean );
      procedure SetManager( val : TGLOXOdeEngine );
      procedure SetModes( const val : TOXModes );
      procedure SetMu( const val : TdReal );
      procedure SetMu2( const val : TdReal );
      procedure SetSlip1( const val : TdReal );
      procedure SetSlip2( const val : TdReal );
      procedure SetBounce( const val : TdReal );
      procedure SetBounce_vel( const val : TdReal );
      procedure SetSoft_erp( const val : TdReal );
      procedure SetSoft_cfm( const val : TdReal );
      procedure SetMotion1( const val : TdReal );
      procedure SetMotion2( const val : TdReal );
      procedure SetContactNum( const val : integer );
      procedure SetApprox0( const val : boolean );
      procedure SetApprox1_1( const val : boolean );
      procedure SetApprox1_2( const val : boolean );
      procedure SetApprox1( const val : boolean );
      procedure SetWheelsParamFMax( const val : TdReal );
      procedure SetWheelsFrontLoHiStop( const val : TdReal );
      procedure SetWheelsBackLoHiStop( const val : TdReal );
      function GetJoint( Index: Integer ) : TdJointID;
      function GetBody( Index: Integer ) : PdxBody;
      function GetGeom( Index: Integer ) : PdxGeom;
      function GetDummyFrame : TGLSizableDummyCube;
      function GetWheelLF : TGLSizableDummyCube;
      function GetWheelLB : TGLSizableDummyCube;
      function GetWheelRF : TGLSizableDummyCube;
      function GetWheelRB : TGLSizableDummyCube;
      function GetModeTraction : TOXCarTraction;
      function GetModes : TOXModes;
      function GetMu : TdReal;
      function GetMu2 : TdReal;
      function GetSlip1 : TdReal;
      function GetSlip2 : TdReal;
      function GetBounce : TdReal;
      function GetBounce_Vel : TdReal;
      function GetSoft_erp : TdReal;
      function GetSoft_cfm : TdReal;
      function GetMotion1 : TdReal;
      function GetMotion2 : TdReal;
      procedure FreeOde;
    protected
    public
      procedure InitODE;
      property Joint[ index : integer ] : TdJointID read GetJoint;
      property Body[ index : integer ] : PdxBody read GetBody;
      property Geom[ index : integer ] : PdxGeom read GetGeom;
      property Frame : TGLSizableDummyCube read GetDummyFrame;
      property WheelLF : TGLSizableDummyCube read GetWheelLF;
      property WheelLB : TGLSizableDummyCube read GetWheelLB;
      property WheelRF : TGLSizableDummyCube read GetWheelRF;
      property WheelRB : TGLSizableDummyCube read GetWheelRB;
      procedure BuildList( var rci : TRenderContextInfo ); override;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Density : TdReal read GetDensity write SetDensity;
      property ContactNum : integer read FContactNum write SetContactNum;
      property Modes : TOXModes read GetModes write SetModes;
      property Mu : TdReal read GetMu write SetMu;
      property Mu2 : TdReal read GetMu2 write SetMu2;
      property Slip1 : TdReal read Getslip1 write Setslip1;
      property Slip2 : TdReal read Getslip2 write Setslip2;
      property Bounce : TdReal read Getbounce write Setbounce;
      property Bounce_vel : TdReal read Getbounce_vel write Setbounce_vel;
      property Soft_erp : TdReal read Getsoft_erp write Setsoft_erp;
      property Soft_cfm : TdReal read Getsoft_cfm write Setsoft_cfm;
      property Motion1 : TdReal read Getmotion1 write Setmotion1;
      property Motion2 : TdReal read Getmotion2 write Setmotion2;
      property Approx0 : boolean read FApprox0 write SetApprox0;
      property Approx1_1 : boolean read FApprox1_1 write SetApprox1_1;
      property Approx1_2 : boolean read FApprox1_2 write SetApprox1_2;
      property Approx1 : boolean read FApprox1 write SetApprox1;
      property ZOOM : TdReal read FZOOM write SetZOOM;
      property ZOOM_CUBED : TdReal read FZOOM_CUBED write SetZOOM_CUBED;
      property LENGTH : TdReal read FLENGTH write SetLENGTH;
      property WIDTH : TdReal read FWIDTH write SetWIDTH;
      property HEIGHT : TdReal read FHEIGHT write SetHEIGHT;
      property RADIUS: TdReal read FRADIUS write SetRADIUS;
      property CMASS: TdReal read FCMASSReal write SetCMASS;
      property WMASS: TdReal read FWMASSReal write SetWMASS;
      property WHEEL_OFFSET: TdReal read FWHEEL_OFFSET write SetWHEEL_OFFSET;
      property FRONTSUSPENSION_ERP: TdReal read FFRONTSUSPENSION_ERP write
      SetFRONTSUSPENSION_ERP;
      property FRONTSUSPENSION_CFM: TdReal read FFRONTSUSPENSION_CFM write
      SetFRONTSUSPENSION_CFM;
      property BACKSUSPENSION_ERP: TdReal read FBACKSUSPENSION_ERP write
      SetBACKSUSPENSION_ERP;
      property BACKSUSPENSION_CFM: TdReal read FBACKSUSPENSION_CFM write
      SetBACKSUSPENSION_CFM;
      property WHEEL_WOBBLE: TdReal read FWHEEL_WOBBLE write SetWHEEL_WOBBLE;
      property ACCEL : TdReal read FACCEL write SetACCEL;
      property TURN_SPEED : TdReal read FTURN_SPEED write SetTURN_SPEED;
      property ModeTraction : TOXCarTraction read GetModeTraction write
      SetModeTraction;
      property WheelPosA : TGLCoordinates read FWPosA write SetWheelPosA;
      property WheelPosB : TGLCoordinates read FWPosB write SetWheelPosB;
      property WheelPosC : TGLCoordinates read FWPosC write SetWheelPosC;
      property WheelPosD : TGLCoordinates read FWPosD write SetWheelPosD;
      property IgnoreSelfModel : boolean read FIgnoreSelfModel write
      SeTIgnoreSelfModel;
      property Manager : TGLOXOdeEngine read FManager write SetManager;
      property AutoDisable : boolean read FAutoDisable write SetAutoDisable;
      property FrictionForce : TdReal read FFrictionForce write
      SetFrictionForce;
      property RollFriction : boolean read FFakeRollForce write SetRollFriction;
      property DisableThreshold : TdReal read FDisableThreshold write
      SetDisableThreshold;
      property DisableSteps : TdReal read FDisableSteps write SetDisableSteps;
      property MotorRunning : boolean read FMotorRunning write SetMotorRunning;
      property Steer : TdReal read FSteer write SetSteer;
      property Speed : TdReal read FSpeed write SetSpeed;
      property FrontWheelLimit : TdReal read FFrontWheelLimit write
      SetFrontWheelLimit;
      property FrontWheelTurnDegre : TdReal read FFrontWheelTurnDegre write
      SetFrontWheelTurnDegre;
      property FrontWheelBackUp : TdReal read FFrontWheelBackUp write
      SetFrontWheelBackUp;
      property FrontBackWheelRunForce : TdReal read
      FFrontBackWheelRunForce write SetFrontBackWheelRunForce;
      property FrontBackWheelAltForce : TdReal read
      FFrontBackWheelAltForce write SetFrontBackWheelAltForce;
      property WheelsParamFMax : TdReal read FWheelsParamFMax write
      SetWheelsParamFMax;
      property WheelsFrontLoHiStop : TdReal read FWheelsFrontLoHiStop write
      SetWheelsFrontLoHiStop;
      property WheelsBackLoHiStop : TdReal read FWheelsBackLoHiStop write
      SetWheelsBackLoHiStop;
      property OnCallBack : TOnCallBack read FOnCallBack write FOnCallBack;
      property ObjectsSurfaceMode : TOXObjectsSurfaceMode read
      GetObjectsSurfaceMode write SetObjectsSurfaceMode;
end;}

type
  TGLOXRagdoll = class( TGLSizableDummyCube )
    private
      FragHeight, FuHead, FthighsL, FthighsW, FthighsH, FlegsL,
      FlegsW, FlegsH, FfootL, FfootW, FfootH, FpelvisL,
      FpelvisW, FpelvisH, Ftors1L, Ftors1W, Ftors1H, Ftors2L,
      Ftors2W, Ftors2H, FupArmL, FupArmW, FupArmH, FforArmL,
      FforArmW, FforArmH, FhandL, FhandW, FhandH, FheadL,
      FheadW, FheadH, Fj0x, Fj0y, Fj0z, Fj2x, Fj2y, Fj2z, Fj1x,
      Fj1y, Fj1z, Fj3x, Fj3y, Fj3z, Fj4x, Fj4y, Fj4z, Fj5x, Fj5y,
      Fj5z, Fj6x, Fj6y, Fj6z, Fj7x, Fj7y, Fj7z, Fj8x, Fj8y, Fj8z,
      Fj9x, Fj9y, Fj9z, Fj10x, Fj10y, Fj10z, Fj11x, Fj11y, Fj11z,
      Fj12x, Fj12y, Fj12z, FCoeff, FConstraint1Lo, FConstraint1Hi,
      FConstraint2Lo, FConstraint2Hi, FConstraint3Lo, FConstraint3Hi,
      FConstraint4Lo, FConstraint4Hi, FConstraint5Lo, FConstraint5Hi,
      FConstraint6Lo, FConstraint6Hi, FConstraint7Lo, FConstraint7Hi,
      FConstraint8Lo, FConstraint8Hi, FConstraint9Lo, FConstraint9Hi,
      FConstraint10Lo, FConstraint10Hi, FConstraint11Lo, FConstraint11Hi,
      FConstraint12Lo, FConstraint12Hi, FConstraint13Lo, FConstraint13Hi,
      FConstraint14Lo, FConstraint14Hi, FConstraint15Lo, FConstraint15Hi,
      FConstraint16Lo, FConstraint16Hi, FConstraint17Lo, FConstraint17Hi,
      FConstraint18Lo, FConstraint18Hi, FConstraint19Lo,
      FConstraint19Hi : TdReal;
      FMassReal : TdReal;
      FDensity : TdReal;  
      FMass : TdMass;
      FFrictionForce : TdReal;
      FFakeRollForce : boolean;
      Fbody : array [0..NUM-1] of PdxBody;
      Fjoint : array [0..19] of TdJointID;
      FGeom : array [0..NUM-1] of PdxGeom;
      FBoneBox : array [0..NUM-1] of TGLSizableDummyCube;
      FActived : boolean;
      FManager : TGLOXOdeEngine;
      FDisableThreshold : TdReal;
      FDisableSteps : TdReal;
      FAutoDisable : boolean;
      FWb_stepsdis : integer;
      FSurface : TdSurfaceParameters;
      FContactNum : integer;
      FModes : TOXModes;
      FMdMode : integer;
      FApprox0 : boolean;
      FApprox1_1 : boolean;
      FApprox1_2 : boolean;
      FApprox1 : boolean;
      FIgnoreSelfModel : boolean;       
      FOnCallBack : TOnCallBack;
      FObjectsSurfaceMode : TOXObjectsSurfaceMode;
      function GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
      procedure SetObjectsSurfaceMode( const val : TOXObjectsSurfaceMode );
      function GetDensity : TdReal;
      function GetJoint( Index: Integer ) : TdJointID;
      function GetBody( Index: Integer ) : PdxBody;
      function GetGeom( Index: Integer ) : PdxGeom;
      function GetBoneCube( Index: Integer ) : TGLSizableDummyCube;
      procedure SetDensity( const val : TdReal );
      procedure AutoDisabling;
      procedure SetAutoDisable( const val : boolean );
      procedure SetDisableThreshold( const val : TdReal );
      procedure SetDisableSteps( const val : TdReal );
      procedure FakeRollingFriction;
      procedure SetFrictionForce( const val : TdReal );
      procedure SetRollFriction( const val : boolean );
      procedure SetMass( const val : TdReal );
      procedure SetConstraint1Lo( const val : TdReal );
      procedure SetConstraint1Hi( const val : TdReal );
      procedure SetConstraint2Lo( const val : TdReal );
      procedure SetConstraint2Hi( const val : TdReal );
      procedure SetConstraint3Lo( const val : TdReal );
      procedure SetConstraint3Hi( const val : TdReal );
      procedure SetConstraint4Lo( const val : TdReal );
      procedure SetConstraint4Hi( const val : TdReal );
      procedure SetConstraint5Lo( const val : TdReal );
      procedure SetConstraint5Hi( const val : TdReal );
      procedure SetConstraint6Lo( const val : TdReal );
      procedure SetConstraint6Hi( const val : TdReal );
      procedure SetConstraint7Lo( const val : TdReal );
      procedure SetConstraint7Hi( const val : TdReal );
      procedure SetConstraint8Lo( const val : TdReal );
      procedure SetConstraint8Hi( const val : TdReal );
      procedure SetConstraint9Lo( const val : TdReal );
      procedure SetConstraint9Hi( const val : TdReal );
      procedure SetConstraint10Lo( const val : TdReal );
      procedure SetConstraint10Hi( const val : TdReal );
      procedure SetConstraint11Lo( const val : TdReal );
      procedure SetConstraint11Hi( const val : TdReal );
      procedure SetConstraint12Lo( const val : TdReal );
      procedure SetConstraint12Hi( const val : TdReal );
      procedure SetConstraint13Lo( const val : TdReal );
      procedure SetConstraint13Hi( const val : TdReal );
      procedure SetConstraint14Lo( const val : TdReal );
      procedure SetConstraint14Hi( const val : TdReal );
      procedure SetConstraint15Lo( const val : TdReal );
      procedure SetConstraint15Hi( const val : TdReal );
      procedure SetConstraint16Lo( const val : TdReal );
      procedure SetConstraint16Hi( const val : TdReal );
      procedure SetConstraint17Lo( const val : TdReal );
      procedure SetConstraint17Hi( const val : TdReal );
      procedure SetConstraint18Lo( const val : TdReal );
      procedure SetConstraint18Hi( const val : TdReal );
      procedure SetConstraint19Lo( const val : TdReal );
      procedure SetConstraint19Hi( const val : TdReal );
      procedure SetModes( const val : TOXModes );
      procedure SetMu( const val : TdReal );
      procedure SetMu2( const val : TdReal );
      procedure SetSlip1( const val : TdReal );
      procedure SetSlip2( const val : TdReal );
      procedure SetBounce( const val : TdReal );
      procedure SetBounce_vel( const val : TdReal );
      procedure SetSoft_erp( const val : TdReal );
      procedure SetSoft_cfm( const val : TdReal );
      procedure SetMotion1( const val : TdReal );
      procedure SetMotion2( const val : TdReal );
      procedure SetContactNum( const val : integer );
      procedure SetApprox0( const val : boolean );
      procedure SetApprox1_1( const val : boolean );
      procedure SetApprox1_2( const val : boolean );
      procedure SetApprox1( const val : boolean );
      function GetModes : TOXModes;
      function GetMu : TdReal;
      function GetMu2 : TdReal;
      function GetSlip1 : TdReal;
      function GetSlip2 : TdReal;
      function GetBounce : TdReal;
      function GetBounce_Vel : TdReal;
      function GetSoft_erp : TdReal;
      function GetSoft_cfm : TdReal;
      function GetMotion1 : TdReal;
      function GetMotion2 : TdReal;
      procedure FreeOde;
   protected
      procedure PlayerUpdate;
      procedure SetManager( const val : TGLOXOdeEngine );
      function GetSize: single;
      procedure SetSize( const val : TdReal );
    public
      property Joint[ index : integer ] : TdJointID read GetJoint;
      property Body[ index : integer ] : PdxBody read GetBody;
      property Geom[ index : integer ] : PdxGeom read GetGeom;
      property BoneBox[ index : integer ] : TGLSizableDummyCube read
      GetBoneCube;
      procedure InitODE;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Constraint1Lo : TdReal read FConstraint1Lo write
      SetConstraint1Lo;
      property Constraint1Hi : TdReal read FConstraint1Hi write
      SetConstraint1Hi;
      property Constraint2Lo : TdReal read FConstraint2Lo write
      SetConstraint2Lo;
      property Constraint2Hi : TdReal read FConstraint2Hi write
      SetConstraint2Hi;
      property Constraint3Lo : TdReal read FConstraint3Lo write
      SetConstraint3Lo;
      property Constraint3Hi : TdReal read FConstraint3Hi write
      SetConstraint3Hi;
      property Constraint4Lo : TdReal read FConstraint4Lo write
      SetConstraint4Lo;
      property Constraint4Hi : TdReal read FConstraint4Hi write
      SetConstraint4Hi;
      property Constraint5Lo : TdReal read FConstraint5Lo write
      SetConstraint5Lo;
      property Constraint5Hi : TdReal read FConstraint5Hi write
      SetConstraint5Hi;
      property Constraint6Lo : TdReal read FConstraint6Lo write
      SetConstraint6Lo;
      property Constraint6Hi : TdReal read FConstraint6Hi write
      SetConstraint6Hi;
      property Constraint7Lo : TdReal read FConstraint7Lo write
      SetConstraint7Lo;
      property Constraint7Hi : TdReal read FConstraint7Hi write
      SetConstraint7Hi;
      property Constraint8Lo : TdReal read FConstraint8Lo write
      SetConstraint8Lo;
      property Constraint8Hi : TdReal read FConstraint8Hi write
      SetConstraint8Hi;
      property Constraint9Lo : TdReal read FConstraint9Lo write
      SetConstraint9Lo;
      property Constraint9Hi : TdReal read FConstraint9Hi write
      SetConstraint9Hi;
      property Constraint10Lo : TdReal read FConstraint10Lo write
      SetConstraint10Lo;
      property Constraint10Hi : TdReal read FConstraint10Hi write
      SetConstraint10Hi;
      property Constraint11Lo : TdReal read FConstraint11Lo write
      SetConstraint11Lo;
      property Constraint11Hi : TdReal read FConstraint11Hi write
      SetConstraint11Hi;
      property Constraint12Lo : TdReal read FConstraint12Lo write
      SetConstraint12Lo;
      property Constraint12Hi : TdReal read FConstraint12Hi write
      SetConstraint12Hi;
      property Constraint13Lo : TdReal read FConstraint13Lo write
      SetConstraint13Lo;
      property Constraint13Hi : TdReal read FConstraint13Hi write
      SetConstraint13Hi;
      property Constraint14Lo : TdReal read FConstraint14Lo write
      SetConstraint14Lo;
      property Constraint14Hi : TdReal read FConstraint14Hi write
      SetConstraint14Hi;
      property Constraint15Lo : TdReal read FConstraint15Lo write
      SetConstraint15Lo;
      property Constraint15Hi : TdReal read FConstraint15Hi write
      SetConstraint15Hi;
      property Constraint16Lo : TdReal read FConstraint16Lo write
      SetConstraint16Lo;
      property Constraint16Hi : TdReal read FConstraint16Hi write
      SetConstraint16Hi;
      property Constraint17Lo : TdReal read FConstraint17Lo write
      SetConstraint17Lo;
      property Constraint17Hi : TdReal read FConstraint17Hi write
      SetConstraint17Hi;
      property Constraint18Lo : TdReal read FConstraint18Lo write
      SetConstraint18Lo;
      property Constraint18Hi : TdReal read FConstraint18Hi write
      SetConstraint18Hi;
      property Constraint19Lo : TdReal read FConstraint19Lo write
      SetConstraint19Lo;
      property Constraint19Hi : TdReal read FConstraint19Hi write
      SetConstraint19Hi;
      property Modes : TOXModes read GetModes write SetModes;
      property Mu : TdReal read GetMu write SetMu;
      property Mu2 : TdReal read GetMu2 write SetMu2;
      property Slip1 : TdReal read Getslip1 write Setslip1;
      property Slip2 : TdReal read Getslip2 write Setslip2;
      property Bounce : TdReal read Getbounce write Setbounce;
      property Bounce_vel : TdReal read Getbounce_vel write Setbounce_vel;
      property Soft_erp : TdReal read Getsoft_erp write Setsoft_erp;
      property Soft_cfm : TdReal read Getsoft_cfm write Setsoft_cfm;
      property Motion1 : TdReal read Getmotion1 write Setmotion1;
      property Motion2 : TdReal read Getmotion2 write Setmotion2;
      property Approx0 : boolean read FApprox0 write SetApprox0;
      property Approx1_1 : boolean read FApprox1_1 write SetApprox1_1;
      property Approx1_2 : boolean read FApprox1_2 write SetApprox1_2;
      property Approx1 : boolean read FApprox1 write SetApprox1;
      property ContactNum : integer read FContactNum write SetContactNum;
      property Density : TdReal read GetDensity write SetDensity;
      property Manager : TGLOXOdeEngine read FManager write SetManager;
      property Size : TdReal read GetSize write SetSize;
      property DisableThreshold : TdReal read FDisableThreshold write
      SetDisableThreshold;
      property DisableSteps : TdReal read FDisableSteps write SetDisableSteps;
      property AutoDisable : boolean read FAutoDisable write SetAutoDisable;
      property FrictionForce : TdReal read FFrictionForce write
      SetFrictionForce;
      property RollFriction : boolean read FFakeRollForce write SetRollFriction;
      property Mass : TdReal read FMassReal write SetMass;
      property OnCallBack : TOnCallBack read FOnCallBack write FOnCallBack;
      property ObjectsSurfaceMode : TOXObjectsSurfaceMode read
      GetObjectsSurfaceMode write SetObjectsSurfaceMode;
  end;
{******************************************************************************}
 // [2005-06-08]: TGLOXFPSMotor last change by Dave Gravel
{******************************************************************************}
type
  TGLOXAMotor = class( TGLSizableDummyCube )
    private
      FAmotor : TdJointID;
      FSurface : TdSurfaceParameters;
      FContactNum : integer;
      FModes : TOXModes;
      FMdMode : integer;
      FApprox0 : boolean;
      FApprox1_1 : boolean;
      FApprox1_2 : boolean;
      FApprox1 : boolean;
      FIgnoreSelfModel : boolean;
      FCoeff : TdReal;
      FActived : boolean;
      FManager : TGLOXOdeEngine;
      FDensity : TdReal;  
      FGeom : PdxGeom;
      FBody : PdxBody;
      FMass : TdMass;
      FMassReal : TdReal;
      FFrictionForce : TdReal;
      FFakeRollForce : boolean;
      FDisableThreshold : TdReal;
      FDisableSteps : TdReal;
      FAutoDisable : boolean;
      FWb_stepsdis : integer;
      FOnCallBack : TOnCallBack;
      FObjectsSurfaceMode : TOXObjectsSurfaceMode;
      function GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
      procedure SetObjectsSurfaceMode( const val : TOXObjectsSurfaceMode );     
      function GetDensity : TdReal;
      function GetGeom : PdxGeom;
      function GetBody : PdxBody;
      procedure SetMass( const val : TdReal );
      procedure AutoDisabling;
      procedure SetAutoDisable( const val : boolean );
      procedure SetDisableThreshold( const val : TdReal );
      procedure SetDisableSteps( const val : TdReal );
      procedure FakeRollingFriction;
      procedure SetFrictionForce( const val : TdReal );
      procedure SetRollFriction( const val : boolean );
      procedure SetDensity( const val : TdReal );
      procedure SetModes( const val : TOXModes );
      procedure SetMu( const val : TdReal );
      procedure SetMu2( const val : TdReal );
      procedure SetSlip1( const val : TdReal );
      procedure SetSlip2( const val : TdReal );
      procedure SetBounce( const val : TdReal );
      procedure SetBounce_vel( const val : TdReal );
      procedure SetSoft_erp( const val : TdReal );
      procedure SetSoft_cfm( const val : TdReal );
      procedure SetMotion1( const val : TdReal );
      procedure SetMotion2( const val : TdReal );
      procedure SetContactNum( const val : integer );
      procedure SetApprox0( const val : boolean );
      procedure SetApprox1_1( const val : boolean );
      procedure SetApprox1_2( const val : boolean );
      procedure SetApprox1( const val : boolean );
      procedure SetIgnoreSelfModel( const val : boolean );
      function GetModes : TOXModes;
      function GetMu : TdReal;
      function GetMu2 : TdReal;
      function GetSlip1 : TdReal;
      function GetSlip2 : TdReal;
      function GetBounce : TdReal;
      function GetBounce_Vel : TdReal;
      function GetSoft_erp : TdReal;
      function GetSoft_cfm : TdReal;
      function GetMotion1 : TdReal;
      function GetMotion2 : TdReal;
      procedure FreeOde;
    protected
      procedure SetManager( const val : TGLOXOdeEngine );
    public
      property Body : PdxBody read GetBody;
      property Geom : PdxGeom read GetGeom;
      procedure InitODE;
      procedure DoProgress( const progressTime : TProgressTimes ); override;
      constructor Create( AOwner : TComponent ); override;
      destructor Destroy; override;
    published
      property Density : TdReal read GetDensity write SetDensity;
      property Modes : TOXModes read GetModes write SetModes;
      property Mu : TdReal read GetMu write SetMu;
      property Mu2 : TdReal read GetMu2 write SetMu2;
      property Slip1 : TdReal read Getslip1 write Setslip1;
      property Slip2 : TdReal read Getslip2 write Setslip2;
      property Bounce : TdReal read Getbounce write Setbounce;
      property Bounce_vel : TdReal read Getbounce_vel write Setbounce_vel;
      property Soft_erp : TdReal read Getsoft_erp write Setsoft_erp;
      property Soft_cfm : TdReal read Getsoft_cfm write Setsoft_cfm;
      property Motion1 : TdReal read Getmotion1 write Setmotion1;
      property Motion2 : TdReal read Getmotion2 write Setmotion2;
      property Approx0 : boolean read FApprox0 write SetApprox0;
      property Approx1_1 : boolean read FApprox1_1 write SetApprox1_1;
      property Approx1_2 : boolean read FApprox1_2 write SetApprox1_2;
      property Approx1 : boolean read FApprox1 write SetApprox1;
      property ContactNum : integer read FContactNum write SetContactNum;
      property IgnoreSelfModel : boolean read FIgnoreSelfModel write
      SeTIgnoreSelfModel;
      property Manager : TGLOXOdeEngine read FManager write SetManager;
      property Mass : TdReal read FMassReal write SetMass;
      property DisableThreshold : TdReal read FDisableThreshold write
      SetDisableThreshold;
      property DisableSteps : TdReal read FDisableSteps write SetDisableSteps;
      property AutoDisable : boolean read FAutoDisable write SetAutoDisable;
      property FrictionForce : TdReal read FFrictionForce write
      SetFrictionForce;
      property RollFriction : boolean read FFakeRollForce write SetRollFriction;
      property OnCallBack : TOnCallBack read FOnCallBack write FOnCallBack;
      property ObjectsSurfaceMode : TOXObjectsSurfaceMode read
      GetObjectsSurfaceMode write SetObjectsSurfaceMode;
  end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
  procedure nearCallBack( Data : Pointer; o1, o2 : PdxGeom ); cdecl;
  procedure SetGeomPosRot( Geom : PdxGeom; Mat : TMatrix );
  procedure SetBodyPosRot( body : PdxBody; Mat : TMatrix );
{******************************************************************************}
var
  VirtualTerrainList: TList;

implementation
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
procedure SizableDummyWireframeBuildList( var rci : TRenderContextInfo;
                                 size : TAffineVector; stipple : Boolean;
                                 const color : TColorVector );
var
  csize : TAffineVector;
  procedure V( a, b, c : double );
  begin
    glVertex3d( csize[0]*a, csize[1]*b, csize[2]*c );
  end;
  procedure N( a, b, c : double );
  begin
    glNormal3d( a, b, c );
  end;
  procedure Looping;
  begin
    glBegin( GL_LINE_LOOP );
      N( 1.0, 0.0, 0.0 );
      V( +1, -1, +1 );
      V( +1, -1, -1 );
      V( +1, +1, -1 );
      V( +1, +1, +1 );
    glEnd();
    glBegin( GL_LINE_LOOP );
      N( 0.0, 1.0, 0.0 );
      V( +1, +1, +1 );
      V( +1, +1, -1 );
      V( -1, +1, -1 );
      V( -1, +1, +1 );
    glEnd();
    glBegin( GL_LINE_LOOP );
      N( 0.0, 0.0, 1.0 );
      V( +1, +1, +1 );
      V( -1, +1, +1 );
      V( -1, -1, +1 );
      V( +1, -1, +1 );
    glEnd();
    glBegin( GL_LINE_LOOP );
      N( -1.0, 0.0, 0.0 );
      V( -1, -1, +1 );
      V( -1, +1, +1 );
      V( -1, +1, -1 );
      V( -1, -1, -1 );
    glEnd();
    glBegin( GL_LINE_LOOP );
      N( 0.0, -1.0, 0.0 );
      V( -1, -1, +1 );
      V( -1, -1, -1 );
      V( +1, -1, -1 );
      V( +1, -1, +1 );
    glEnd();
    glBegin( GL_LINE_LOOP );
      N( 0.0, 0.0, -1.0 );
      V( -1, -1, -1 );
      V( -1, +1, -1 );
      V( +1, +1, -1 );
      V( +1, -1, -1 );
    glEnd();
  end;
begin
   glPushAttrib( GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LIGHTING_BIT or
                 GL_LINE_BIT or GL_COLOR_BUFFER_BIT );
   glDisable( GL_LIGHTING );
   glEnable( GL_LINE_SMOOTH );
   if stipple then
   begin
      glEnable( GL_LINE_STIPPLE );
      glEnable( GL_BLEND );
      glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
      glLineStipple( 1, $CCCC );
   end;
   glLineWidth( 1 );
   rci.GLStates.ResetGLMaterialColors;
   glColorMaterial( GL_FRONT, GL_EMISSION );
   glEnable( GL_COLOR_MATERIAL );
   glColor4fv( @color );
   csize[0] := size[0]*0.5;
   csize[1] := size[1]*0.5;
   csize[2] := size[2]*0.5;
   Looping;
   glPopAttrib;
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
constructor TGLSizableDummyCube.Create( AOwner : TComponent );
begin
  inherited;
  ObjectStyle := ObjectStyle+[osDirectDraw];
  FCubeSizable := XYZVector;
  FEdgeColor := TGLColor.Create( Self );
  FEdgeColor.Initialize( clrWhite );
  FGroupList := TGLListHandle.Create;
  CamInvarianceMode := cimNone;
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
destructor TGLSizableDummyCube.Destroy;
begin
  FGroupList.Free;
  FEdgeColor.Free;
  inherited;
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
procedure TGLSizableDummyCube.Assign( Source: TPersistent );
begin
  if Source is TGLSizableDummyCube then
  begin
    FEdgeColor.Color := TGLSizableDummyCube( Source ).FEdgeColor.Color;
    FVisibleAtRunTime := TGLSizableDummyCube( Source ).FVisibleAtRunTime;
    NotifyChange( Self );
  end;
  inherited Assign( Source );
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
function TGLSizableDummyCube.AxisAlignedDimensionsUnscaled : TVector;
begin
  Result[0] := 0.5*Abs( FCubeSizable[0] );
  Result[1] := 0.5*Abs( FCubeSizable[1] );
  Result[2] := 0.5*Abs( FCubeSizable[2] );
  Result[3] := 0;
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
function TGLSizableDummyCube.RayCastIntersect( const rayStart,
         rayVector : TVector; intersectPoint : PVector = nil;
         intersectNormal : PVector = nil ) : Boolean;
begin
  Result := False;
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
procedure TGLSizableDummyCube.BuildList( var rci : TRenderContextInfo );
begin
  if ( csDesigning in ComponentState ) or ( FVisibleAtRunTime ) then
    SizableDummyWireframeBuildList( rci, FCubeSizable, True, EdgeColor.Color );
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
procedure TGLSizableDummyCube.DoRender( var rci : TRenderContextInfo;
renderSelf, renderChildren : Boolean );
begin
  if Assigned( FOnVisibilityDetermination ) then
    if not FOnVisibilityDetermination( Self, rci ) then
      Exit;
  if FAmalgamate and (not rci.amalgamating) then
  begin
    if ( FGroupList.Handle = 0 ) then
    begin
      FGroupList.AllocateHandle;
      Assert( FGroupList.Handle <> 0, 'Handle=0 for '+ClassName );
      glNewList( FGroupList.Handle, GL_COMPILE );
      rci.amalgamating := True;
      try
        inherited;
        finally
          rci.amalgamating := False;
          glEndList;
        end;
      end;
      glCallList( FGroupList.Handle );
    end else
    begin
    // proceed as usual
    inherited;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
procedure TGLSizableDummyCube.StructureChanged;
begin
  if FAmalgamate then
    FGroupList.DestroyHandle;
  inherited;
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
function TGLSizableDummyCube.BarycenterAbsolutePosition : TVector;
var
  i : Integer;
begin
  if Count > 0 then
  begin
    Result := Children[0].BarycenterAbsolutePosition;
    for i := 1 to Count-1 do
      Result := VectorAdd( Result, Children[i].BarycenterAbsolutePosition );
      ScaleVector( Result, 1/Count );
  end else Result := AbsolutePosition;
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
procedure TGLSizableDummyCube.SetEdgeColor( const val : TGLColor );
begin
  if val<>FEdgeColor then
  begin
    FEdgeColor.Assign( val );
    StructureChanged;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
procedure TGLSizableDummyCube.SetCubeWidth( const aValue : Single );
begin
  if aValue<>FCubeSizable[0] then
  begin
    FCubeSizable[0] := aValue;
    StructureChanged;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
procedure TGLSizableDummyCube.SetCubeHeight( const aValue : Single );
begin
  if ( aValue <> FCubeSizable[1] ) then
  begin
    FCubeSizable[1] := aValue;
    StructureChanged;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
procedure TGLSizableDummyCube.SetCubeDepth( const aValue : Single );
begin
  if ( aValue <> FCubeSizable[2] ) then
  begin
    FCubeSizable[2] := aValue;
    StructureChanged;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
procedure TGLSizableDummyCube.SetVisibleAtRunTime( const val : Boolean );
begin
  if ( val <> FVisibleAtRunTime ) then
  begin
    FVisibleAtRunTime := val;
    StructureChanged;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
procedure TGLSizableDummyCube.SetAmalgamate( const val : Boolean );
begin
  if ( val <> FAmalgamate ) then
  begin
    FAmalgamate := val;
    if val then
      ObjectStyle := ObjectStyle+[osDoesTemperWithColorsOrFaceWinding]
    else begin
      FGroupList.DestroyHandle;
      ObjectStyle := ObjectStyle-[osDoesTemperWithColorsOrFaceWinding]
    end;
    inherited StructureChanged;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
procedure SetGeomPosRot( Geom : PdxGeom; Mat : TMatrix );
var
  R : TdMatrix3;
begin
  if not Assigned( Geom ) then exit;
  R[0] := Mat[0][0]; R[1] := Mat[1][0]; R[2] := Mat[2][0]; R[3] := 0;
  R[4] := Mat[0][1]; R[5] := Mat[1][1]; R[6] := Mat[2][1]; R[7] := 0;
  R[8] := Mat[0][2]; R[9] := Mat[1][2]; R[10] := Mat[2][2]; R[11] :=  0;
  dGeomSetRotation( Geom, R );
  dGeomSetPosition( Geom, Mat[3][0], Mat[3][1], Mat[3][2] );
end;
{******************************************************************************}
 // [2005-06-08]: Utils last change by Dave Gravel
{******************************************************************************}
procedure SetBodyPosRot( body : PdxBody; Mat : TMatrix );
var
  R : TdMatrix3;
begin
  if not Assigned( body ) then exit;
  R[0] := Mat[0][0]; R[1] := Mat[1][0]; R[2] := Mat[2][0]; R[3] := 0;
  R[4] := Mat[0][1]; R[5] := Mat[1][1]; R[6] := Mat[2][1]; R[7] := 0;
  R[8] := Mat[0][2]; R[9] := Mat[1][2]; R[10] := Mat[2][2]; R[11] := 0;
  dBodySetRotation( body, R );
  dBodySetPosition( body, Mat[3][0], Mat[3][1], Mat[3][2] );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
constructor TGLOXOdeEngine.create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FWorldSurfaceMode := mdwNoneSurface;
  FContactMaxCorrectingVel := False;
  FContactSurfaceLayer := False;
  FActived := False;
  VirtualTerrainList := TList.Create;
  FObjsList := TList.Create;
  DisabledDebugGeom := True;
  DisabledDebugCollision := True;
  FOdeModes := 0;
  FMaxCorrectingVel := 0.1;
  FSurfaceLayer := 0.001;
  FObjsColliding := False;
  FCustomObjsSurface := False;
  FGravity := TGLCoordinates.CreateInitialized( self, NullHmgPoint, csVector );
  FGravity.X := 0; FGravity.Y := 0; FGravity.Z := -9.81;
  FGravity.OnNotifyChange := GravityChange;
  if ( not ( csDesigning in ComponentState ) ) then
  begin
    FWorld := dWorldCreate();
    FSpace := dHashSpaceCreate( FSpace );
    FContactgroup := dJointGroupCreate( 0 );
    dWorldSetCFM( FWorld, 1e-5 );
    dWorldSetGravity( FWorld, FGravity.X, FGravity.Y, FGravity.Z );
  end;
  FInit := False;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.InitODE;
begin
  if ( not FInit ) then
  begin
    if Assigned( FWorld ) then
    begin
      GravityChange( self );
      //WARNING...           i'm tired of this lies it is all time same.
      //If this both options is set to true it can come not compatible with some
      //object types and can get some bad results with the no compatible objects.
      //You can't change the value true or false on runtime.
      //Use this both options just if you know what you do.
      //It is normaly used with the Dynamic TriMesh.
      if ( FContactMaxCorrectingVel ) then
        dWorldSetContactMaxCorrectingVel( FWorld, FMaxCorrectingVel );
      if ( FContactSurfaceLayer ) then
        dWorldSetContactSurfaceLayer( FWorld, FSurfaceLayer );
      //////////////////////////////////////////////////////////////////////////
      FInit := True;
      FActived := True;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.BuildList( var rci : TRenderContextInfo );
begin
  if ( csDesigning in ComponentState ) or ( VisibleAtRunTime ) then
    SizableDummyWireframeBuildList( rci, FCubeSizable, True, EdgeColor.Color );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.Callback( g1, g2 : PdxGeom );
const
  MAXBUFFER = 511;
var
  i, g, h : integer;
  numc : integer;
  contact : array [0..MAXBUFFER-1] of TdContact;
  c : TdJointID;
  b1, b2 : PdxBody;
  ObjContact : integer;
begin
  b1 := dGeomGetBody( g1 );
  b2 := dGeomGetBody( g2 );
  if assigned(TGLCustomSceneObject( g1.data )) and
  assigned(TGLCustomSceneObject( g2.data )) then
  if ( TGLCustomSceneObject( g1.data ) is TGLSizableDummyCube ) and
     ( TGLCustomSceneObject( g2.data ) is TGLSizableDummyCube ) then
  begin
    if ( b1 = TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
    Parent ).Fbody[6] ) and
       ( b2 = TGLOXRagdoll( TGLSizableDummyCube( g2.data ).
    Parent ).Fbody[0] ) then exit;
    if ( b1 = TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
    Parent ).Fbody[0] ) and
       ( b2 = TGLOXRagdoll( TGLSizableDummyCube( g2.data ).
    Parent ).Fbody[6] ) then exit;
    if ( b1 = TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
    Parent ).Fbody[6] ) and
       ( b2 = TGLOXRagdoll( TGLSizableDummyCube( g2.data ).
    Parent ).Fbody[3] ) then exit;
    if ( b1 = TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
    Parent ).Fbody[3] ) and
       ( b2 = TGLOXRagdoll( TGLSizableDummyCube( g2.data ).
    Parent ).Fbody[6] ) then exit;
    if ( b1 = TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
    Parent ).Fbody[8] ) and
       ( b2 = TGLOXRagdoll( TGLSizableDummyCube( g2.data ).
    Parent ).Fbody[11] ) then exit;
    if ( b1 = TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
    Parent ).Fbody[11] ) and
       ( b2 = TGLOXRagdoll( TGLSizableDummyCube( g2.data ).
    Parent ).Fbody[8] ) then exit;
    if ( b1 = TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
    Parent ).Fbody[8] ) and
       ( b2 = TGLOXRagdoll( TGLSizableDummyCube( g2.data ).
    Parent ).Fbody[12] ) then exit;
    if ( b1 = TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
    Parent ).Fbody[12] ) and
       ( b2 = TGLOXRagdoll( TGLSizableDummyCube( g2.data ).
    Parent ).Fbody[8] ) then exit;
    if ( b1 = TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
    Parent ).Fbody[17] ) and
       ( b2 = TGLOXRagdoll( TGLSizableDummyCube( g2.data ).
    Parent ).Fbody[18] ) then exit;
    if ( b1 = TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
    Parent ).Fbody[18] ) and
       ( b2 = TGLOXRagdoll( TGLSizableDummyCube( g2.data ).
    Parent ).Fbody[17] ) then exit;
  end;
  if assigned( FOnCustomCollisionEvent ) then
  begin
    FOnCustomCollisionEvent( g1, g2 );
    exit;
  end;
  if ( FAreConnected ) and ( assigned( b1 ) and assigned( b2 ) and
    ( dAreConnected( b1, b2 ) <> 0 ) ) then exit;
  if ( FTypeBall ) and ( assigned( b1 ) and assigned( b2 ) and
    ( dAreConnectedExcluding( b1, b2, dJointTypeBall ) <> 0 ) ) then exit;
  if ( FTypeHinge ) and ( assigned( b1 ) and assigned( b2 ) and
    ( dAreConnectedExcluding( b1, b2, dJointTypeHinge ) <> 0 ) ) then exit;
  if ( FTypeSlider ) and ( assigned( b1 ) and assigned( b2 ) and
    ( dAreConnectedExcluding( b1, b2, dJointTypeSlider ) <> 0 ) ) then exit;
  if ( FTypeContact ) and ( assigned( b1 ) and assigned( b2 ) and
    ( dAreConnectedExcluding( b1, b2, dJointTypeContact ) <> 0 ) ) then exit;
  if ( FTypeUniversal ) and ( assigned( b1 ) and assigned( b2 ) and
    ( dAreConnectedExcluding( b1, b2, dJointTypeUniversal ) <> 0 ) ) then exit;
//  if ( FTypeHinge2 ) and ( assigned( b1 ) and assigned( b2 ) and
//    ( dAreConnectedExcluding( b1, b2, dJointTypeHinge2 ) <> 0 ) ) then exit;
  if ( FTypeFixed ) and ( assigned( b1 ) and assigned( b2 ) and
    ( dAreConnectedExcluding( b1, b2, dJointTypeFixed ) <> 0 ) ) then exit;
  if ( FTypeAMotor ) and ( assigned( b1 ) and assigned( b2 ) and
    ( dAreConnectedExcluding( b1, b2, dJointTypeAMotor ) <> 0 ) ) then exit;
  ObjContact := FContactNum;
  if ( FCustomObjsContact ) then
  begin
    if ( TGLCustomSceneObject( g1.data ) is TGLOXDynBall ) then
    begin
      ObjContact := TGLOXDynBall( g1.data ).FContactNum;
      if ( TGLOXDynBall( g1.data ).FIgnoreSelfModel ) then
        if ( TGLCustomSceneObject( g1.data ) is TGLOXDynBall ) and
        ( TGLCustomSceneObject( g2.data ) is TGLOXDynBall ) then exit;
    end;
    if ( TGLCustomSceneObject( g1.data ) is TGLOXDynBox ) then
    begin
      ObjContact := TGLOXDynBox( g1.data ).FContactNum;
      if ( TGLOXDynBox( g1.data ).FIgnoreSelfModel ) then
        if ( TGLCustomSceneObject( g1.data ) is TGLOXDynBox ) and
        ( TGLCustomSceneObject( g2.data ) is TGLOXDynBox ) then exit;
    end;
    if ( TGLCustomSceneObject( g1.data ) is TGLOXDynCylinder ) then
    begin
      ObjContact := TGLOXDynCylinder( g1.data ).FContactNum;
      if ( TGLOXDynCylinder( g1.data ).FIgnoreSelfModel ) then
        if ( TGLCustomSceneObject( g1.data ) is TGLOXDynCylinder ) and
        ( TGLCustomSceneObject( g2.data ) is TGLOXDynCylinder ) then exit;
    end;
{    if ( TGLCustomSceneObject( g1.data ) is TGLOXDynCCylinder ) then
    begin
      ObjContact := TGLOXDynCCylinder( g1.data ).FContactNum;
      if ( TGLOXDynCCylinder( g1.data ).FIgnoreSelfModel ) then
        if ( TGLCustomSceneObject( g1.data ) is TGLOXDynCCylinder ) and
        ( TGLCustomSceneObject( g2.data ) is TGLOXDynCCylinder ) then exit;
    end;
    if ( TGLCustomSceneObject( g1.data ) is TGLOXDynCone ) then
    begin
      ObjContact := TGLOXDynCone( g1.data ).FContactNum;
      if ( TGLOXDynCone( g1.data ).FIgnoreSelfModel ) then
        if ( TGLCustomSceneObject( g1.data ) is TGLOXDynCone ) and
        ( TGLCustomSceneObject( g2.data ) is TGLOXDynCone ) then exit;
    end;}
    if ( TGLCustomSceneObject( g1.data ) is TGLOXDynMesh ) then
    begin
      ObjContact := TGLOXDynMesh( g1.data ).FContactNum;
      if ( TGLOXDynMesh( g1.data ).FIgnoreSelfModel ) then
        if ( TGLCustomSceneObject( g1.data ) is TGLOXDynMesh ) and
        ( TGLCustomSceneObject( g2.data ) is TGLOXDynMesh ) then exit;
    end;
    if ( TGLCustomSceneObject( g1.data ) is TGLOXAMotor ) then
    begin
      ObjContact := TGLOXAMotor( g1.data ).FContactNum;
      if ( TGLOXAMotor( g1.data ).FIgnoreSelfModel ) then
        if ( TGLCustomSceneObject( g1.data ) is TGLOXAMotor ) and
        ( TGLCustomSceneObject( g2.data ) is TGLOXAMotor ) then exit;
    end;
    if ( TGLCustomSceneObject( g1.data ) is TGLSizableDummyCube ) then
    begin
(*      for h := 0 to 4 do
      if ( b1 = TGLOXDynCar( TGLSizableDummyCube( g1.data ).
        Parent ).Fbody[h] ) then begin
        ObjContact := TGLOXDynCar( TGLSizableDummyCube( g1.data ).
        Parent ).FContactNum;
        if ( TGLOXDynCar( TGLSizableDummyCube( g1.data ).
        Parent ).FIgnoreSelfModel ) then
          if ( TGLCustomSceneObject( g1.data ) is TGLSizableDummyCube ) and
          ( TGLCustomSceneObject( g2.data ) is TGLSizableDummyCube ) then exit;
      end;*)
    end;
    if ( TGLCustomSceneObject( g1.data ) is TGLSizableDummyCube ) then
    begin
      for h := 0 to 19 do
      if ( b1 = TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
      Parent ).Fbody[h] ) then
      begin
        ObjContact := TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
        Parent ).FContactNum;
        if ( TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
        Parent ).FIgnoreSelfModel ) then
          if ( TGLCustomSceneObject( g1.data ) is TGLSizableDummyCube ) and
          ( TGLCustomSceneObject( g2.data ) is TGLSizableDummyCube ) then exit;
      end;
    end;
  end;
  if ( ObjContact <= 0 ) or ( ObjContact > 511 ) then
    exit;
  numc := dCollide( g1, g2, ObjContact, contact[0].geom, sizeof( TdContact ) );
  if numc > 0 then
  begin
    for i := 0 to numc -1 do
    begin
      if ( FCustomObjsSurface ) then
      begin
       FObjsColliding := False;
       contact[i].Surface := FSurface;
       FMode := 0;
       if ( TGLCustomSceneObject( g1.data ) is TGLOXDynBall ) then
       begin
         contact[i].surface := TGLOXDynBall( g1.data ).FSurface;
         FMode := contact[i].surface.mode;
         if ( TGLOXDynBall( g1.data ).FApprox0 ) then
           contact[i].surface.mode := FMode or ord( dContactApprox0 );
         FMode := contact[i].surface.mode;
         if ( TGLOXDynBall( g1.data ).FApprox1_1 ) then
           contact[i].surface.mode := FMode or ord( dContactApprox1_1 );
         FMode := contact[i].surface.mode;
         if ( TGLOXDynBall( g1.data ).FApprox1_2 ) then
           contact[i].surface.mode := FMode or ord( dContactApprox1_2 );
         FMode := contact[i].surface.mode;
         if ( TGLOXDynBall( g1.data ).FApprox1 ) then
           contact[i].surface.mode := FMode or ord( dContactApprox1 );
         if assigned( TGLOXDynBall( g1.data ).FOnCallBack ) then
           TGLOXDynBall( g1.data ).FOnCallBack( g1, g2 );
           FObjsColliding := True;
         end;
         if ( TGLCustomSceneObject( g1.data ) is TGLOXDynBox ) then
         begin
           contact[i].surface := TGLOXDynBox( g1.data ).FSurface;
           FMode := contact[i].surface.mode;
           if ( TGLOXDynBox( g1.data ).FApprox0 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox0 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynBox( g1.data ).FApprox1_1 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1_1 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynBox( g1.data ).FApprox1_2 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1_2 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynBox( g1.data ).FApprox1 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1 );
           if assigned( TGLOXDynBox( g1.data ).FOnCallBack ) then
             TGLOXDynBox( g1.data ).FOnCallBack( g1, g2 );
           FObjsColliding := True;
         end;
         if ( TGLCustomSceneObject( g1.data ) is TGLOXDynCylinder ) then
         begin
           contact[i].surface := TGLOXDynCylinder( g1.data ).FSurface;
           FMode := contact[i].surface.mode;
           if ( TGLOXDynCylinder( g1.data ).FApprox0 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox0 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynCylinder( g1.data ).FApprox1_1 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1_1 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynCylinder( g1.data ).FApprox1_2 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1_2 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynCylinder( g1.data ).FApprox1 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1 );
           if assigned( TGLOXDynCylinder( g1.data ).FOnCallBack ) then
             TGLOXDynCylinder( g1.data ).FOnCallBack( g1, g2 );
           FObjsColliding := True;
         end;
{         if ( TGLCustomSceneObject( g1.data ) is TGLOXDynCCylinder ) then
         begin
           contact[i].surface := TGLOXDynCCylinder( g1.data ).FSurface;
           FMode := contact[i].surface.mode;
           if ( TGLOXDynCCylinder( g1.data ).FApprox0 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox0 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynCCylinder( g1.data ).FApprox1_1 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1_1 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynCCylinder( g1.data ).FApprox1_2 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1_2 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynCCylinder( g1.data ).FApprox1 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1 );
           if assigned( TGLOXDynCCylinder( g1.data ).FOnCallBack ) then
             TGLOXDynCCylinder( g1.data ).FOnCallBack( g1, g2 );
           FObjsColliding := True;
         end;
         if ( TGLCustomSceneObject( g1.data ) is TGLOXDynCone ) then
         begin
           contact[i].surface := TGLOXDynCone( g1.data ).FSurface;
           FMode := contact[i].surface.mode;
           if ( TGLOXDynCone( g1.data ).FApprox0 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox0 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynCone( g1.data ).FApprox1_1 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1_1 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynCone( g1.data ).FApprox1_2 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1_2 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynCone( g1.data ).FApprox1 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1 );
           if assigned( TGLOXDynCone( g1.data ).FOnCallBack ) then
             TGLOXDynCone( g1.data ).FOnCallBack( g1, g2 );
           FObjsColliding := True;
         end;}
         if ( TGLCustomSceneObject( g1.data ) is TGLOXDynMesh ) then
         begin
           contact[i].surface := TGLOXDynMesh( g1.data ).FSurface;
           FMode := contact[i].surface.mode;
           if ( TGLOXDynMesh( g1.data ).FApprox0 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox0 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynMesh( g1.data ).FApprox1_1 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1_1 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynMesh( g1.data ).FApprox1_2 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1_2 );
           FMode := contact[i].surface.mode;
           if ( TGLOXDynMesh( g1.data ).FApprox1 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1 );
           if assigned( TGLOXDynMesh( g1.data ).FOnCallBack ) then
             TGLOXDynMesh( g1.data ).FOnCallBack( g1, g2 );
           FObjsColliding := True;
         end;
         if ( TGLCustomSceneObject( g1.data ) is TGLOXAMotor ) then
         begin
           contact[i].surface := TGLOXAMotor( g1.data ).FSurface;
           FMode := contact[i].surface.mode;
           if ( TGLOXAMotor( g1.data ).FApprox0 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox0 );
           FMode := contact[i].surface.mode;
           if ( TGLOXAMotor( g1.data ).FApprox1_1 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1_1 );
           FMode := contact[i].surface.mode;
           if ( TGLOXAMotor( g1.data ).FApprox1_2 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1_2 );
           FMode := contact[i].surface.mode;
           if ( TGLOXAMotor( g1.data ).FApprox1 ) then
             contact[i].surface.mode := FMode or ord( dContactApprox1 );
           if assigned( TGLOXAMotor( g1.data ).FOnCallBack ) then
             TGLOXAMotor( g1.data ).FOnCallBack( g1, g2 );
           FObjsColliding := True;
         end;
         if ( TGLCustomSceneObject( g1.data ) is TGLSizableDummyCube ) then
         begin
(*           for h := 0 to 4 do
           if ( b1 = TGLOXDynCar( TGLSizableDummyCube( g1.data ).
           Parent ).Fbody[h] ) then
           begin
             contact[i].surface := TGLOXDynCar( TGLSizableDummyCube( g1.data ).
             Parent ).FSurface;
             FMode := contact[i].surface.mode;
             if TGLOXDynCar( TGLSizableDummyCube( g1.data ).
             Parent ).FApprox0 then
               contact[i].surface.mode := FMode or ord( dContactApprox0 );
             FMode := contact[i].surface.mode;
             if TGLOXDynCar( TGLSizableDummyCube( g1.data ).
             Parent ).FApprox1_1 then
               contact[i].surface.mode := FMode or ord( dContactApprox1_1 );
             FMode := contact[i].surface.mode;
             if TGLOXDynCar( TGLSizableDummyCube( g1.data ).
             Parent ).FApprox1_2 then
               contact[i].surface.mode := FMode or ord( dContactApprox1_2 );
             FMode := contact[i].surface.mode;
             if TGLOXDynCar( TGLSizableDummyCube( g1.data ).
             Parent ).FApprox1 then
               contact[i].surface.mode := FMode or ord( dContactApprox1 );
             if assigned( TGLOXDynCar( TGLSizableDummyCube( g1.data ).
             Parent ).FOnCallBack ) then
               TGLOXDynCar( TGLSizableDummyCube( g1.data ).
             Parent ).FOnCallBack( g1, g2 );
             FObjsColliding := True;
           end;*)
         end;
         if ( TGLCustomSceneObject( g1.data ) is TGLSizableDummyCube ) then
         begin
           for h := 0 to 19 do
           if ( b1 = TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
           Parent ).Fbody[h] ) then
           begin
             contact[i].surface := TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
             Parent ).FSurface;
             FMode := contact[i].surface.mode;
             if TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
             Parent ).FApprox0 then
               contact[i].surface.mode := FMode or ord( dContactApprox0 );
             FMode := contact[i].surface.mode;
             if TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
             Parent ).FApprox1_1 then
               contact[i].surface.mode := FMode or ord( dContactApprox1_1 );
             FMode := contact[i].surface.mode;
             if TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
             Parent ).FApprox1_2 then
               contact[i].surface.mode := FMode or ord( dContactApprox1_2 );
             FMode := contact[i].surface.mode;
             if TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
             Parent ).FApprox1 then
               contact[i].surface.mode := FMode or ord( dContactApprox1 );
             if assigned( TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
             Parent ).FOnCallBack ) then
               TGLOXRagdoll( TGLSizableDummyCube( g1.data ).
             Parent ).FOnCallBack( g1, g2 );
             FObjsColliding := True;
           end;
         end;
       end;
       if assigned( FOnODECollisionEvent ) then
       begin
         FOnODECollisionEvent( g1, g2 );
       end;
       if ( FObjsColliding ) then
       begin
         c := dJointCreateContact( FWorld, FContactgroup, @contact[i] );
         if// ( not ( TGLCustomSceneObject( g1.data ) is TGLOXDynCCylinder ) ) and
         ( not ( TGLCustomSceneObject( g1.data ) is TGLOXDynMesh ) ) and
//         ( not ( TGLCustomSceneObject( g1.data ) is TGLOXDynCone ) ) and
         ( not ( TGLCustomSceneObject( g1.data ) is TGLOXAMotor ) ) then
         begin
           dJointAttach( c, dGeomGetBody( contact[i].geom.g1 ),
           dGeomGetBody( contact[i].geom.g2 ) );
         end else
         begin
           dJointAttach( c, b1, b2 );
         end;
      end else
      if ( not FObjsColliding ) then
      begin
        FMode := 0;
        contact[i].Surface := FSurface;
        FMode := contact[i].surface.mode;
        if ( FApprox0 ) then
          contact[i].surface.mode := FMode or ord( dContactApprox0 );
        FMode := contact[i].surface.mode;
        if ( FApprox1_1 ) then
          contact[i].surface.mode := FMode or ord( dContactApprox1_1 );
        FMode := contact[i].surface.mode;
        if ( FApprox1_2 ) then
          contact[i].surface.mode := FMode or ord( dContactApprox1_2 );
        FMode := contact[i].surface.mode;
        if ( FApprox1 ) then
          contact[i].surface.mode := FMode or ord( dContactApprox1 );
          c := dJointCreateContact( FWorld, FContactgroup, @contact[i] );
        if// ( not ( TGLCustomSceneObject( g1.data ) is TGLOXDynCCylinder ) ) and
        ( not ( TGLCustomSceneObject( g1.data ) is TGLOXDynMesh ) ) and
//        ( not ( TGLCustomSceneObject( g1.data ) is TGLOXDynCone ) ) and
        ( not ( TGLCustomSceneObject( g1.data ) is TGLOXAMotor ) )  then
        begin
          dJointAttach( c, dGeomGetBody( contact[i].geom.g1 ),
          dGeomGetBody( contact[i].geom.g2 ) );
        end else
        begin
          dJointAttach( c, b1, b2 );
        end;
      end;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.DoProgress( const progressTime : TProgressTimes );
var
  dtime: double;
  i: integer;
begin
  inherited;
  if Assigned( FWorld ) and ( FActived ) then
  begin
    dtime := progressTime.deltaTime;
    if ( dtime > 0.02 ) then
      dtime := 0.02;
    if FODEMultiplyTime <= 0 then
      FODEMultiplyTime := 1;
    FODETime := FODETime + ( dtime * FODEMultiplyTime );
    while ( FODETime >= 0.1 ) do
    begin
      FODETime := FODETime - 0.1;
      dSpaceCollide( FSpace, self, nearCallback );
      case FOdeModes of
        0 : dWorldQuickStep( FWorld, FODEStepTime );
        1 : dWorldStepFast1( FWorld, FODEStepTime, FODEQuickStepNum );
        2 : dWorldStep( FWorld, FODEStepTime );
      end;
      dJointGroupEmpty( FContactgroup );
      if Assigned( FOnOdeStepRender ) then
      begin
        FOnOdeStepRender( progressTime.deltaTime );
      end;
      for i := 0 to FObjsList.Count-1 do
      begin
(*        if ( TGLCustomSceneObject( FObjsList.Items[i] ) is TGLOXDynCar )
        then begin
          TGLOXDynCar( FObjsList.Items[i] ).DoMotor( progressTime.deltaTime );
        end;*)
      end;
    end;      
    if Assigned( FOnStepRender ) then
    begin
      FOnStepRender( progressTime.deltaTime );
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetWorldSurfaceMode : TOXWorldSurfaceMode;
begin
  result := FWorldSurfaceMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetWorldSurfaceMode( const val : TOXWorldSurfaceMode );
begin
  if FWorldSurfaceMode<> val then
  begin
    FWorldSurfaceMode := val;
      case FWorldSurfaceMode of
  mdwSoftSurface: begin
                    FWorldModes := [mdQuickStep];
                    SetModes([mdSoftCFM,mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0; soft_erp := 0.5; soft_cfm := 0.01;
                      motion1 := 0; motion2 := 0; slip1 := 0; slip2 := 0;
                    end;
                    FContactGroupNum := 0; FContactNum := 24;
                    FCustomObjsContact := False; FCustomObjsSurface := False;
                    FGravity.X := 0; FGravity.Y := 0; FGravity.Z := -9.81;
                    FODEStepTime := 0.01; FODEMultiplyTime := 8;
                    FODEQuickStepNum := 20; FMaxCorrectingVel := 0.1;
                    FSurfaceLayer := 0.001; FApprox0 := False;
                    FApprox1_1 := False; FApprox1_2 := False;
                    FApprox1 := False; FContactMaxCorrectingVel := False;
                    FContactSurfaceLayer := False; FTypeBall := True;
                    FTypeHinge := True; FTypeSlider := False;
                    FTypeContact := False; FTypeUniversal := False;
//                    FTypeHinge2 := True; FTypeFixed := True;
                    FTypeAMotor := True; FAreConnected := True;
                  end;
  mdwHardSurface: begin
                    FWorldModes := [mdQuickStep]; SetModes([mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0; soft_erp := 0.8; soft_cfm := 0;
                      motion1 := 0; motion2 := 0; slip1 := 0; slip2 := 0;
                    end;
                    FContactGroupNum := 0; FContactNum := 24;
                    FCustomObjsContact := False; FCustomObjsSurface := False;
                    FGravity.X := 0; FGravity.Y := 0; FGravity.Z := -9.81;
                    FODEStepTime := 0.01; FODEMultiplyTime := 8;
                    FODEQuickStepNum := 20; FMaxCorrectingVel := 0.1;
                    FSurfaceLayer := 0.001; FApprox0 := False;
                    FApprox1_1 := False; FApprox1_2 := False; FApprox1 := False;
                    FContactMaxCorrectingVel := False;
                    FContactSurfaceLayer := False; FTypeBall := True;
                    FTypeHinge := True; FTypeSlider := False;
                    FTypeContact := False; FTypeUniversal := False;
//                    FTypeHinge2 := True; FTypeFixed := True;
                    FTypeAMotor := True; FAreConnected := True;
                  end;
  mdwBoundSurface: begin
                     FWorldModes := [mdQuickStep];
                     SetModes([mdSoftCFM,mdSoftERP,mdBounce]);
                     with FSurface do begin
                       mode := FMdMode; mu := 5; mu2 := 0; bounce := 0.5;
                       bounce_vel := 0.1; soft_erp := 0.5; soft_cfm := 0.001;
                       motion1 := 0; motion2 := 0; slip1 := 0; slip2 := 0;
                     end;
                     FContactGroupNum := 0; FContactNum := 24;
                     FCustomObjsContact := False; FCustomObjsSurface := False;
                     FGravity.X := 0; FGravity.Y := 0; FGravity.Z := -9.81;
                     FODEStepTime := 0.01; FODEMultiplyTime := 8;
                     FODEQuickStepNum := 20; FMaxCorrectingVel := 0.1;
                     FSurfaceLayer := 0.001; FApprox0 := False;
                     FApprox1_1 := False; FApprox1_2 := False;
                     FApprox1 := False; FContactMaxCorrectingVel := False;
                     FContactSurfaceLayer := False; FTypeBall := True;
                     FTypeHinge := True; FTypeSlider := False;
                     FTypeContact := False; FTypeUniversal := False;
//                     FTypeHinge2 := True; FTypeFixed := True;
                     FTypeAMotor := True; FAreConnected := True;
                   end;
  mdwDefaultSurface: begin
                       FWorldModes :=
                       [mdQuickStep]; SetModes([mdSoftCFM,mdSoftERP]);
                       with FSurface do begin
                         mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                         bounce_vel := 0; soft_erp := 0.8; soft_cfm := 0.001;
                         motion1 := 0; motion2 := 0; slip1 := 0; slip2 := 0;
                       end;
                       FContactGroupNum := 0; FContactNum := 24;
                       FCustomObjsContact := False; FCustomObjsSurface := False;
                       FGravity.X := 0; FGravity.Y := 0; FGravity.Z := -9.81;
                       FODEStepTime := 0.01; FODEMultiplyTime := 8;
                       FODEQuickStepNum := 20; FMaxCorrectingVel := 0.1;
                       FSurfaceLayer := 0.001; FApprox0 := False;
                       FApprox1_1 := False; FApprox1_2 := False;
                       FApprox1 := False; FContactMaxCorrectingVel := False;
                       FContactSurfaceLayer := False; FTypeBall := True;
                       FTypeHinge := True; FTypeSlider := False;
                       FTypeContact := False; FTypeUniversal := False;
//                       FTypeHinge2 := True; FTypeFixed := True;
                       FTypeAMotor := True; FAreConnected := True;
                    end;
  mdwNoneSurface: begin

                    end;
                  end;    
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.GravityChange( Sender : TObject );
begin
  if Assigned( FWorld ) then
    dWorldSetGravity( FWorld, FGravity.X, FGravity.Y, FGravity.Z );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetMaxCorrectingVel( const val : TdReal );
begin
  if ( FMaxCorrectingVel <> val ) then
  begin
    FMaxCorrectingVel := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetSurfaceLayer( const val : TdReal );
begin
  if ( FSurfaceLayer <> val ) then
  begin
    FSurfaceLayer := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetContactMaxCorrectingVel( const val : boolean );
begin
  if ( FContactMaxCorrectingVel <> val ) then
  begin
    FContactMaxCorrectingVel := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetContactSurfaceLayer( const val : boolean );
begin
  if ( FContactSurfaceLayer <> val ) then
  begin
    FContactSurfaceLayer := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetTypeBall( const val : boolean );
begin
  if ( FTypeBall <> val ) then
  begin
    FTypeBall := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetTypeHinge( const val : boolean );
begin
  if ( FTypeHinge <> val ) then
  begin
    FTypeHinge := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetTypeSlider( const val : boolean );
begin
  if ( FTypeSlider <> val ) then
  begin
    FTypeSlider := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetTypeContact( const val : boolean );
begin
  if ( FTypeContact <> val ) then
  begin
    FTypeContact := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetTypeUniversal( const val : boolean );
begin
  if ( FTypeUniversal <> val ) then
  begin
    FTypeUniversal := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
{procedure TGLOXOdeEngine.SetTypeHinge2( const val : boolean );
begin
  if ( FTypeHinge2 <> val ) then
  begin
    FTypeHinge2 := val;
    NotifyChange( self );
  end;
end;}
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetTypeFixed( const val : boolean );
begin
  if ( FTypeFixed <> val ) then
  begin
    FTypeFixed := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetTypeAMotor( const val : boolean );
begin
  if ( FTypeAMotor <> val ) then
  begin
    FTypeAMotor := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetAreConnected( const val : boolean );
begin
  if ( FAreConnected <> val ) then
  begin
    FAreConnected := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetCustomObjsContact( const val : boolean );
begin
  if ( FCustomObjsContact <> val ) then
  begin
    FCustomObjsContact := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetCustomObjsSurface( const val : boolean );
begin
  if ( FCustomObjsSurface <> val ) then
  begin
    FCustomObjsSurface := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetApprox0( const val : boolean );
begin
  if ( FApprox0 <> val ) then
  begin
    FApprox0 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetApprox1_1( const val : boolean );
begin
  if ( FApprox1_1 <> val ) then
  begin
    FApprox1_1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetApprox1_2( const val : boolean );
begin
  if ( FApprox1_2 <> val ) then
  begin
    FApprox1_2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetApprox1( const val : boolean );
begin
  if ( FApprox1 <> val ) then
  begin
    FApprox1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetODEGravity( val : TGLCoordinates );
begin
  FGravity.SetPoint( val.DirectX, val.DirectY, val.DirectZ );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetODEStepTime( const val : TdReal );
begin
  if ( FODEStepTime <> val ) then
  begin
    FODEStepTime := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetODEMultiplyTime( const val : TdReal );
begin
  if ( FODEMultiplyTime <> val ) then
  begin
    FODEMultiplyTime := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetODEContactgroupNum( const val : integer );
begin
  if ( FContactgroupNum <> val ) then
  begin
    FContactgroupNum := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetODEQuickStepNum( const val : integer );
begin
  if ( FOdeQuickStepNum <> val ) then
  begin
    FOdeQuickStepNum := val;
    if Assigned( FWorld ) then
      dWorldSetQuickStepNumIterations( FWorld, val );
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetODEContactNum( const val : integer );
begin
  if ( FContactNum <> val ) then
  begin
    FContactNum := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetModes( const val : TOXModes );
begin
  if ( FModes <> val ) then
  begin
    FModes := val;
    FMdMode := 0;
    if mdSlip2 in FModes then
      FMdMode := FMdMode or dContactSlip2;
    if mdSlip1 in FModes then
      FMdMode := FMdMode or dContactSlip1;
    if mdSoftCFM in FModes then
      FMdMode := FMdMode or dContactSoftCFM;
    if mdSoftERP in FModes then
      FMdMode := FMdMode or dContactSoftERP;
    if mdBounce in FModes then
      FMdMode := FMdMode or dContactBounce;
    if mdFDir1 in FModes then
      FMdMode := FMdMode or dContactFDir1;
    if mdMu2 in FModes then
      FMdMode := FMdMode or dContactMu2;
    FSurface.mode := FMdMode;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetWorldModes( const val : TOXOdeModes );
begin
  if ( FWorldModes <> val ) then
  begin
    FWorldModes := val;
    FOdeModes := 0;
    if mdQuickStep in FWorldModes then
      FOdeModes := 0;
    if mdStepFast1 in FWorldModes then
      FOdeModes := 1;
    if mdNormalStep in FWorldModes then
      FOdeModes := 2;
    FOdeModes := FOdeModes;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetMu( const val : TdReal );
begin
  if ( FSurface.mu <> val ) then
  begin
    FSurface.mu := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetMu2( const val : TdReal );
begin
  if ( FSurface.mu2 <> val ) then
  begin
    FSurface.mu2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetSlip1( const val : TdReal );
begin
  if ( FSurface.slip1 <> val ) then
  begin
    FSurface.slip1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetSlip2( const val : TdReal );
begin
  if ( FSurface.slip2 <> val ) then
  begin
    FSurface.slip2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetBounce( const val : TdReal );
begin
  if ( FSurface.bounce <> val ) then
  begin
    FSurface.bounce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetBounce_vel( const val : TdReal );
begin
  if ( FSurface.bounce_vel <> val ) then
  begin
    FSurface.bounce_vel := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetSoft_erp( const val : TdReal );
begin
  if ( FSurface.soft_erp <> val ) then
  begin
    FSurface.soft_erp := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetSoft_cfm( const val : TdReal );
begin
  if ( FSurface.soft_cfm <> val ) then
  begin
    FSurface.soft_cfm := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetMotion1( const val : TdReal );
begin
  if ( FSurface.motion1 <> val ) then
  begin
    FSurface.motion1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
procedure TGLOXOdeEngine.SetMotion2( const val : TdReal );
begin
  if ( FSurface.motion2 <> val ) then
  begin
    FSurface.motion2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetModes : TOXModes;
var
  OXMode : TOXModes;
begin
  OXMode := [];
  if ( FSurface.mode and dContactSlip2 ) <> 0 then
    OXMode := OXMode + [mdSlip2];
  if ( FSurface.mode and dContactSlip1 ) <> 0 then
    OXMode := OXMode + [mdSlip1];
  if ( FSurface.mode and dContactSoftCFM ) <> 0 then
    OXMode := OXMode + [mdSoftCFM];
  if ( FSurface.mode and dContactSoftERP ) <> 0 then
    OXMode := OXMode + [mdSoftERP];
  if ( FSurface.mode and dContactBounce ) <> 0 then
    OXMode := OXMode + [mdBounce];
  if ( FSurface.mode and dContactFDir1 ) <> 0 then
    OXMode := OXMode + [mdFDir1];
  if ( FSurface.mode and dContactMu2 ) <> 0 then
    OXMode := OXMode + [mdMu2];
  result := OXMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetWorldModes : TOXOdeModes;
var
  OXWorldMode : TOXOdeModes;
begin
  OXWorldMode := [];
  if ( FOdeModes ) = 0 then
    OXWorldMode := [ mdQuickStep ];
  if ( FOdeModes ) = 1 then
    OXWorldMode := [ mdStepFast1 ];
  if ( FOdeModes ) = 2 then
    OXWorldMode := [ mdNormalStep ];
  result := OXWorldMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetMu : TdReal;
begin
  result := FSurface.mu;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetMu2 : TdReal;
begin
  result := FSurface.mu2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetSlip1 : TdReal;
begin
  result := FSurface.slip1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetSlip2 : TdReal;
begin
  result := FSurface.slip2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetBounce : TdReal;
begin
  result := FSurface.bounce;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetBounce_Vel : TdReal;
begin
  result := FSurface.bounce_vel;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetSoft_erp : TdReal;
begin
  result := FSurface.soft_erp;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetSoft_cfm : TdReal;
begin
  result := FSurface.soft_cfm;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetMotion1 : TdReal;
begin
  result := FSurface.motion1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetMotion2 : TdReal;
begin
  result := FSurface.motion2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetWorld : PdxWorld;
begin
  if assigned( FWorld ) then
    result := FWorld else
  raise Exception.Create
  ( 'init your GLOXOdeEngine before use the World.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
function TGLOXOdeEngine.GetSpace : PdxSpace;
begin
  if assigned( FSpace ) then
    result := FSpace else
  raise Exception.Create
  ( 'init your GLOXOdeEngine before use the Space.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXOdeEngine last change by Dave Gravel
{******************************************************************************}
destructor TGLOXOdeEngine.Destroy;
var i: integer;
begin
  if ( FActived ) then
  begin
    FActived := False;
    if assigned( FWorld ) then
    begin
      dJointGroupDestroy( FContactgroup );
      dSpaceDestroy( FSpace );
      dWorldDestroy( FWorld );
      dCloseODE;
    end;
    for i:= 0 to FObjsList.Count-1 do
      TGLCustomSceneObject(FObjsList.Items[i]).Free;
    FGravity.Free;
    FObjsList.Free;
    VirtualTerrainList.Free;
    FInit := False;
  end;
  inherited Destroy;
end;
{******************************************************************************}
 // [2005-06-08]: Collision last change by Dave Gravel
{******************************************************************************}
function CornerSort( Item1, Item2: Pointer ): Integer;
var
  c1, c2 : TCorner;
begin
  c1 := TCorner( Item1 );
  c2 := TCorner( Item2 );
  if ( c1.FDepth > c2.FDepth ) then
    result := -1
  else if ( c1.FDepth = c2.FDepth ) then
    result := 0
  else
    result := 1;
end;
{******************************************************************************}
 // [2005-06-08]: Collision last change by Dave Gravel
{******************************************************************************}
function CustomColliderFnSphere( o1, o2 : PdxGeom; flags : Integer;
                                contact : PdContactGeom;
                                skip : Integer ) : Integer; cdecl;
  procedure AddContact( x, y, z : Single; var nb : Integer );
  var
    n : TAffineVector;
    zs : Single;
    i: integer;
  begin
    if Assigned( VirtualTerrainList ) then
    begin
      for i:= 0 to VirtualTerrainList.Count-1 do
      begin
        if ( nb >= flags ) then Exit;
          zs := TGLOXZStaTerrain( VirtualTerrainList.Items[i] ).
          ColliderFormula( x, y );
        if ( z < zs ) then begin
          contact.pos[0] := x;
          contact.pos[1] := y;
        contact.pos[2] := zs;
        contact.pos[3] := 1;
        n := TGLOXZStaTerrain( VirtualTerrainList.Items[i] ).
        ColliderFormulaNormal( x, y );
        contact.normal[0] := -n[0];
        contact.normal[1] := -n[1];
        contact.normal[2] := -n[2];
        contact.normal[3] := 0;
        contact.depth := zs -z;
        contact.g1 := o1;
        contact.g2 := o2;
        contact := PdContactGeom( Integer( contact ) + skip );
        Inc( nb );
      end;
    end;
  end;
end;
var
  pos : PdVector3;
  r, dx, dy, dz : Single;
  i : Integer;
begin
  if not ( TGLCustomSceneObject( o2.data ) is TGLOXStaBall ) then
  begin
    pos := dGeomGetPosition( o2 );
    r := dGeomSphereGetRadius( o2 );
    Result := 0;
    AddContact( pos[0], pos[1], pos[2] -r, Result );
    for i:=0 to 5 do
    begin
      SinCos( DegToRad( i * 60 ), r * 0.4, dy, dx );
      dz := r -Sqrt( Sqr( r ) -Sqr( dx ) -Sqr( dy ) );
      AddContact( pos[0] + dx, pos[1] + dy, pos[2] -r + dz, Result );
      SinCos( DegToRad( i * 60 ), r * 0.8, dy, dx );
      dz := r -Sqrt( Sqr( r ) -Sqr( dx ) -Sqr( dy ) );
      AddContact( pos[0] + dx, pos[1] + dy, pos[2] -r + dz, Result );
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: Collision last change by Dave Gravel
{******************************************************************************}
function CustomColliderFnBox( o1, o2 : PdxGeom; flags : Integer;
                             contact : PdContactGeom;
                             skip : Integer ) : Integer; cdecl;
  procedure AddContact( x, y, z : Single; Terr: TGLOXZStaTerrain );
  var
    zs : Single;
    Corner : TCorner;
  begin
    zs := Terr.ColliderFormula( x, y );
    if ( z < zs ) then
    begin
      Corner := Terr.FCornerCache[Terr.FCornerList.Count];
      Corner.FPos[0] := x;
      Corner.Fpos[1] := y;
      Corner.Fpos[2] := zs;
      Corner.FDepth := zs -z;
      Terr.FCornerList.Add( Corner );
   end;
  end;
  procedure KeepDeepest( var nb : integer; Terr: TGLOXZStaTerrain );
  var
    i : integer;
    n : TAffineVector;
    Corner : TCorner;
  begin
    Terr.FCornerList.Sort( CornerSort );
    for i := 0 to Terr.FCornerList.Count-1 do
    begin
      if ( nb >= flags ) then Exit;
      Corner := TCorner( Terr.FCornerList[i] );
      contact.pos[0] := Corner.Fpos[0];
      contact.pos[1] := Corner.Fpos[1];
      contact.pos[2] := Corner.Fpos[2];
      n := Terr.ColliderFormulaNormal( Corner.Fpos[0], Corner.Fpos[1] );
      contact.normal[0] := -n[0];
      contact.normal[1] := -n[1];
      contact.normal[2] := -n[2];
      contact.depth := Corner.FDepth;
      contact.g1 := o1;
      contact.g2 := o2;
      contact := PdContactGeom( Integer( contact ) + skip );
      Inc( nb );
  end;
end;
var
  pos : PdVector3;
  Body : PdxBody;
  Sides, dPos : TdVector3;
  i: integer;
  Terrain: TGLOXZStaTerrain;
begin
  if not ( TGLCustomSceneObject( o2.data ) is TGLOXStaBox ) then
  begin
    if Assigned( VirtualTerrainList ) then
    begin
      for i:= 0 to VirtualTerrainList.Count-1 do
      begin
        Terrain := TGLOXZStaTerrain( VirtualTerrainList.Items[i] );
        Terrain.FCornerList.Clear;
        pos := dGeomGetPosition( o2 );
        dGeomBoxGetLengths( o2, Sides );
        Result := 0;
        Body := dGeomGetBody( o2 );
        dGeomBoxGetLengths( o2, Sides );
        dBodyVectorToWorld( Body, sides[0]/2, sides[1]/2, sides[2]/2, dPos );
        AddContact( pos[0] +dpos[0], pos[1] +dpos[1], pos[2] +dpos[2],
        Terrain );
        AddContact( pos[0] -dpos[0], pos[1] -dpos[1], pos[2] -dpos[2],
        Terrain );
        dBodyVectorToWorld( Body, sides[0]/2,sides[1]/2, -sides[2]/2, dPos );
        AddContact( pos[0] +dpos[0], pos[1] +dpos[1], pos[2] +dpos[2],
        Terrain );
        AddContact( pos[0] -dpos[0], pos[1] -dpos[1], pos[2] -dpos[2],
        Terrain );
        dBodyVectorToWorld( Body, sides[0]/2,-sides[1]/2, sides[2]/2, dPos );
        AddContact( pos[0] +dpos[0], pos[1] +dpos[1], pos[2] +dpos[2],
        Terrain );
        AddContact( pos[0] -dpos[0], pos[1] -dpos[1], pos[2] -dpos[2],
        Terrain );
        dBodyVectorToWorld( Body, -sides[0]/2,sides[1]/2, sides[2]/2, dPos );
        AddContact( pos[0] +dpos[0], pos[1] +dpos[1], pos[2] +dpos[2],
        Terrain );
        AddContact( pos[0] -dpos[0], pos[1] -dpos[1], pos[2] -dpos[2],
        Terrain );
        KeepDeepest( Result, Terrain );
        Terrain.FCornerList.Clear;
      end;
    end;
  end else
  result := 0;
end;
{******************************************************************************}
 // [2005-06-08]: Collision last change by Dave Gravel
{******************************************************************************}
{function CustomCollideCCylinder(o1, o2 : PdxGeom; flags : Integer;
                                contact : PdContactGeom;
                                skip : Integer) : Integer; cdecl;
  procedure AddContact( x, y, z : Single; var nb : Integer );
  var
    n : TAffineVector;
    zs : Single;
    i: integer;
  begin
    if Assigned( VirtualTerrainList ) then
    begin
      for i:= 0 to VirtualTerrainList.Count-1 do
      begin
        if ( nb >= flags ) then Exit;
          zs := TGLOXZStaTerrain( VirtualTerrainList.Items[i] ).
          ColliderFormula( x, y );
        if ( z < zs ) then begin
          contact.pos[0] := x;
          contact.pos[1] := y;
        contact.pos[2] := zs;
        contact.pos[3] := 1;
        n := TGLOXZStaTerrain( VirtualTerrainList.Items[i] ).
        ColliderFormulaNormal( x, y );
        contact.normal[0] := -n[0];
        contact.normal[1] := -n[1];
        contact.normal[2] := -n[2];
        contact.normal[3] := 0;
        contact.depth := zs -z;
        contact.g1 := o1;
        contact.g2 := o2;
        contact := PdContactGeom( Integer( contact ) + skip );
        Inc( nb );
      end;
    end;
  end;
end;
var
  pos : PdVector3;
  rad,len : Single;
begin
  if not ( TGLCustomSceneObject( o2.data ) is TGLOXStaCCylinder ) then
  begin
    pos := dGeomGetPosition( o2 );
    dGeomCCylinderGetParams(o2, rad, len);
    Result := 0;
    AddContact( pos[0], pos[1], pos[2] -rad, Result );
  end;
end;}
{******************************************************************************}
 // [2005-06-08]: Collision last change by Dave Gravel
{******************************************************************************}
function CustomCollideCylinder(o1, o2 : PdxGeom; flags : Integer;
                               contact : PdContactGeom;
                               skip : Integer) : Integer; cdecl;
  procedure AddContact( x, y, z : Single; var nb : Integer );
  var
    n : TAffineVector;
    zs : Single;
    i: integer;
  begin
    if Assigned( VirtualTerrainList ) then
    begin
      for i:= 0 to VirtualTerrainList.Count-1 do
      begin
        if ( nb >= flags ) then Exit;
          zs := TGLOXZStaTerrain( VirtualTerrainList.Items[i] ).
          ColliderFormula( x, y );
        if ( z < zs ) then begin
          contact.pos[0] := x;
          contact.pos[1] := y;
        contact.pos[2] := zs;
        contact.pos[3] := 1;
        n := TGLOXZStaTerrain( VirtualTerrainList.Items[i] ).
        ColliderFormulaNormal( x, y );
        contact.normal[0] := -n[0];
        contact.normal[1] := -n[1];
        contact.normal[2] := -n[2];
        contact.normal[3] := 0;
        contact.depth := zs -z;
        contact.g1 := o1;
        contact.g2 := o2;
        contact := PdContactGeom( Integer( contact ) + skip );
        Inc( nb );
      end;
    end;
  end;
end;
var
  pos : PdVector3;
  rad,len : Single;
begin
  if not ( TGLCustomSceneObject( o2.data ) is TGLOXStaCylinder ) then
  begin
    pos := dGeomGetPosition( o2 );
    dGeomCylinderGetParams(o2, rad, len);
    Result := 0;
    AddContact( pos[0], pos[1], pos[2] -rad, Result );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: Collision last change by Dave Gravel
{******************************************************************************}
{function CustomCollideCone(o1, o2 : PdxGeom; flags : Integer;
                           contact : PdContactGeom;
                           skip : Integer) : Integer; cdecl;
  procedure AddContact( x, y, z : Single; var nb : Integer );
  var
    n : TAffineVector;
    zs : Single;
    i: integer;
  begin
    if Assigned( VirtualTerrainList ) then
    begin
      for i:= 0 to VirtualTerrainList.Count-1 do
      begin
        if ( nb >= flags ) then Exit;
          zs := TGLOXZStaTerrain( VirtualTerrainList.Items[i] ).
          ColliderFormula( x, y );
        if ( z < zs ) then begin
          contact.pos[0] := x;
          contact.pos[1] := y;
        contact.pos[2] := zs;
        contact.pos[3] := 1;
        n := TGLOXZStaTerrain( VirtualTerrainList.Items[i] ).
        ColliderFormulaNormal( x, y );
        contact.normal[0] := -n[0];
        contact.normal[1] := -n[1];
        contact.normal[2] := -n[2];
        contact.normal[3] := 0;
        contact.depth := zs -z;
        contact.g1 := o1;
        contact.g2 := o2;
        contact := PdContactGeom( Integer( contact ) + skip );
        Inc( nb );
      end;
    end;
  end;
end;
var
  pos : PdVector3;
  rad,len : Single;
begin
//  if not ( TGLCustomSceneObject( o2.data ) is TGLOXStaCone ) then
  begin
    pos := dGeomGetPosition( o2 );
    dGeomCCylinderGetParams(o2, rad, len);
    Result := 0;
    AddContact( pos[0], pos[1], pos[2] -rad, Result );
  end;
end;}
{******************************************************************************}
 // [2005-06-08]: Collision last change by Dave Gravel
{******************************************************************************}
function CustomCollideTriMesh(o1, o2 : PdxGeom; flags : Integer;
                              contact : PdContactGeom;
                              skip : Integer) : Integer; cdecl;
  procedure AddContact( x, y, z : Single; var nb : Integer );
  var
    n : TAffineVector;
    zs : Single;
    i: integer;
  begin
    if Assigned( VirtualTerrainList ) then
    begin
      for i:= 0 to VirtualTerrainList.Count-1 do
      begin
        if ( nb >= flags ) then Exit;
          zs := TGLOXZStaTerrain( VirtualTerrainList.Items[i] ).
          ColliderFormula( x, y );
        if ( z < zs ) then begin
          contact.pos[0] := x;
          contact.pos[1] := y;
        contact.pos[2] := zs;
        contact.pos[3] := 1;
        n := TGLOXZStaTerrain( VirtualTerrainList.Items[i] ).
        ColliderFormulaNormal( x, y );
        contact.normal[0] := -n[0];
        contact.normal[1] := -n[1];
        contact.normal[2] := -n[2];
        contact.normal[3] := 0;
        contact.depth := zs -z;
        contact.g1 := o1;
        contact.g2 := o2;
        contact := PdContactGeom( Integer( contact ) + skip );
        Inc( nb );
      end;
    end;
  end;
end;
var
  pos : PdVector3;
  rad,len : Single;
begin
  if not ( TGLCustomSceneObject( o2.data ) is TGLOXStaMesh ) then
  begin
    pos := dGeomGetPosition( o2 );
    dGeomCylinderGetParams(o2, rad, len);
    Result := 0;
    AddContact( pos[0], pos[1], pos[2] -rad, Result );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: Collision last change by Dave Gravel
{******************************************************************************}
function CustomCollideGeomTransform(o1, o2 : PdxGeom; flags : Integer;
                                    contact : PdContactGeom;
                                    skip : Integer) : Integer; cdecl;
  procedure AddContact( x, y, z : Single; var nb : Integer );
  var
    n : TAffineVector;
    zs : Single;
    i: integer;
  begin
    if Assigned( VirtualTerrainList ) then
    begin
      for i:= 0 to VirtualTerrainList.Count-1 do
      begin
        if ( nb >= flags ) then Exit;
          zs := TGLOXZStaTerrain( VirtualTerrainList.Items[i] ).
          ColliderFormula( x, y );
        if ( z < zs ) then begin
          contact.pos[0] := x;
          contact.pos[1] := y;
        contact.pos[2] := zs;
        contact.pos[3] := 1;
        n := TGLOXZStaTerrain( VirtualTerrainList.Items[i] ).
        ColliderFormulaNormal( x, y );
        contact.normal[0] := -n[0];
        contact.normal[1] := -n[1];
        contact.normal[2] := -n[2];
        contact.normal[3] := 0;
        contact.depth := zs -z;
        contact.g1 := o1;
        contact.g2 := o2;
        contact := PdContactGeom( Integer( contact ) + skip );
        Inc( nb );
      end;
    end;
  end;
end;
var
  pos : PdVector3;
  rad,len : Single;
begin
  pos := dGeomGetPosition( o2 );
  dGeomCylinderGetParams(o2, rad, len);
  Result := 0;
  AddContact( pos[0], pos[1], pos[2] -rad, Result );
end;
{******************************************************************************}
 // [2005-06-08]: Collision last change by Dave Gravel
{******************************************************************************}
function CustomGetColliderFnFn( num : Integer ) : TdColliderFn; cdecl;
begin
  if ( num = dSphereClass ) then
    Result := CustomColliderFnSphere
  else
  if ( num = dBoxClass ) then
    Result := CustomColliderFnBox
  else
{  if ( num = dCCylinderClass ) then
    Result := CustomCollideCCylinder
  else                              }
  if ( num = dCylinderClass ) then
    Result := CustomCollideCylinder
  else
{  if ( num = dConeClass ) then
    Result := CustomCollideCone
  else                         }
  if ( num = dTriMeshClass ) then
    Result := CustomCollideTriMesh
  else
  if ( num = dGeomTransformClass ) then
    Result := CustomCollideGeomTransform
  else
    Result:= nil;
end;
{******************************************************************************}
 // [2005-06-08]: Collision last change by Dave Gravel
{******************************************************************************}
procedure nearCallback( data : pointer; o1, o2 : PdxGeom ); cdecl;
begin
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaBall ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaBall ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaBall ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaBox ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaBall ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaMesh ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaBall ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCCylinder ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaBall ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCylinder ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaBall ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCone ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaBall ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXZStaTerrain ) then exit;
  //
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaBox ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaBox ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaBox ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaBall ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaBox ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaMesh ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaBox ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCCylinder ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaBox ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCylinder ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaBox ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCone ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaBox ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXZStaTerrain ) then exit;
  //
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaMesh ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaMesh ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaMesh ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaBox ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaMesh ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaBall ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaMesh ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCCylinder ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaMesh ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCylinder ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaMesh ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCone ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaMesh ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXZStaTerrain ) then exit;
  //
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCylinder ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCylinder ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCylinder ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaBox ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCylinder ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaBall ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCylinder ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCCylinder ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCylinder ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaMesh ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCylinder ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCone ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCylinder ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXZStaTerrain ) then exit;
  //
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCCylinder ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCCylinder ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCCylinder ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaBox ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCCylinder ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaBall ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCCylinder ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCylinder ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCCylinder ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaMesh ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCCylinder ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCone ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCCylinder ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXZStaTerrain ) then exit;
  //
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCone ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCCylinder ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCone ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaBox ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCone ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaBall ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCone ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCylinder ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCone ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaMesh ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCone ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCCylinder ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXStaCone ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXZStaTerrain ) then exit;
  //
  if ( TGLCustomSceneObject( o1.data ) is TGLOXZStaTerrain ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCCylinder ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXZStaTerrain ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaBox ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXZStaTerrain ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaBall ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXZStaTerrain ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCylinder ) then exit;
  if ( TGLCustomSceneObject( o1.data ) is TGLOXZStaTerrain ) and
  ( TGLCustomSceneObject( o2.data ) is TGLOXStaMesh ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXZStaTerrain ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCCylinder ) then exit;
//  if ( TGLCustomSceneObject( o1.data ) is TGLOXZStaTerrain ) and
//  ( TGLCustomSceneObject( o2.data ) is TGLOXStaCone ) then exit;
  //
  TGLOXOdeEngine( Data ).Callback( o1, o2 );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXZStaTerrain last change by Dave Gravel
{******************************************************************************}
function TGLOXZStaTerrain.ColliderFormula(x, y : Single) : Single;
var
  Pos : TVector;
begin
  Pos[0] := x; Pos[1] := y; Result := InterpolatedHeight( Pos );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXZStaTerrain last change by Dave Gravel
{******************************************************************************}
function TGLOXZStaTerrain.ColliderFormulaNormal
( x, y : Single ) : TAffineVector;
const
  DELTA = 0.2;
begin
  Result := CalcPlaneNormal( AffineVectorMake
  ( x, y, ColliderFormula( x, y ) ),
  AffineVectorMake
  ( x + DELTA, y, ColliderFormula( x + DELTA, y ) ),
  AffineVectorMake
  ( x, y + DELTA, ColliderFormula( x, y + DELTA ) ) );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXZStaTerrain last change by Dave Gravel
{******************************************************************************}
procedure TGLOXZStaTerrain.InitODE;
begin
  if assigned( FManager ) then
  begin
    if not Assigned( FNewCollider ) then
    begin
      FManager.FObjsList.Add( self );
      VirtualTerrainList.Add( self );
      FCustomColliderClass.bytes := FTerrainAllocMem;
      FCustomColliderClass.collider := CustomGetColliderFnFn;
      FCustomColliderClass.aabb := dInfiniteAABB;
      FCustomColliderClass.aabb_test := nil;
      FCustomColliderClass.dtor := nil;
      FNewClassNum := dCreateGeomClass( FCustomColliderClass );
      FNewCollider := dCreateGeom( FNewClassNum );
      FNewCollider.data := self;
      SetGeomPosRot( FNewCollider, AbsoluteMatrix );
      dSpaceAdd( FManager.FSpace, FNewCollider );
      FActived := True;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXZStaTerrain last change by Dave Gravel
{******************************************************************************}
function TGLOXZStaTerrain.GetGeom : PdxGeom;
begin
  if assigned( FNewCollider ) then
    result := FNewCollider else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXZStaTerrain before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXZStaTerrain last change by Dave Gravel
{******************************************************************************}
procedure TGLOXZStaTerrain.SetManager( val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXZStaTerrain last change by Dave Gravel
{******************************************************************************}
procedure TGLOXZStaTerrain.FreeOde;
begin
  if assigned( FNewCollider ) then
  begin
    dGeomDestroy( FNewCollider );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXZStaTerrain last change by Dave Gravel
{******************************************************************************}
constructor TGLOXZStaTerrain.Create( AOwner : TComponent );
var i: integer;
begin
  inherited Create( AOwner );
  FActived := False;
  FCornerList := TList.Create;
  for i := 0 to 7 do
    FCornerCache[i] := TCorner.Create;  
  FTerrainAllocMem := 0;
  Direction.X := 0;
  Direction.Y := 0;
  Direction.Z := 1;
  Up.X := 0;
  Up.Y := 1;
  Up.Z := 0;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXZStaTerrain last change by Dave Gravel
{******************************************************************************}
destructor TGLOXZStaTerrain.Destroy;
var i: integer;
begin
  FActived := False;
  if assigned( FManager ) then
  begin
    FreeOde;
    VirtualTerrainList.Remove( self );
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  for i := 0 to 7 do
    FCornerCache[i].Free;  
  FCornerList.Free;
  inherited Destroy;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBall last change by Dave Gravel
{******************************************************************************}
constructor TGLOXStaBall.create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FActived := False;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaBall.InitODE;
begin
  if assigned( FManager ) then
  begin
    if not Assigned( FGeom ) then
    begin
      FGeom := dCreateSphere( FManager.FSpace, Radius );
      FGeom.data := self;
      SetGeomPosRot( FGeom, AbsoluteMatrix );
      FManager.FObjsList.Add( self );
      FActived := True;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaBall.BuildList( var rci : TRenderContextInfo );
var
  V1, V2, N1 : TAffineVector;
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH : Extended;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
  uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1 : TdReal;
  I, J: Integer; DoReverse: Boolean;
begin
  DoReverse := ( NormalDirection = ndInside );
  glPushAttrib( GL_POLYGON_BIT );
  if DoReverse then
    rci.GLStates.InvertGLFrontFace;
    AngTop := DegToRad( Top );
    AngBottom := DegToRad( Bottom );
    AngStart := DegToRad( Start );
    AngStop := DegToRad( Stop );
    StepH := ( AngStop - AngStart ) / Slices;
    StepV := ( AngTop - AngBottom ) / Stacks;
    glPushMatrix;
    glScalef( Radius, Radius, Radius );
    if ( Top < 90 ) and ( TopCap in [ ctCenter, ctFlat ] ) then
    begin
      glBegin( GL_TRIANGLE_FAN );
      SinCos( AngTop, SinP, CosP );
      xglTexCoord2f( 0.5, 0.5 );
      if DoReverse then
        glNormal3f( 0, -1, 0 )
      else
        glNormal3f( 0, 1, 0 );
      if TopCap = ctCenter then
        glVertex3f( 0, 0, 0 )
      else
      begin
        glVertex3f( 0, SinP, 0 );
        N1 := YVector;
        if DoReverse then
          N1[1] := -N1[1];
     end;
     V1[1] := SinP;
     Theta := AngStart;
     for I := 0 to Slices do
     begin
       SinCos( Theta, SinT, CosT );
       V1[0] := CosP * SinT;
       V1[2] := CosP * CosT;
       if TopCap = ctCenter then
       begin
         N1 := VectorPerpendicular( YVector, V1 );
         if DoReverse then
           NegateVector( N1 );
       end;
       xglTexCoord2f( SinT * 0.5 + 0.5, CosT * 0.5 + 0.5 );
       glNormal3fv( @N1 );
       glVertex3fv( @V1 );
       Theta := Theta + StepH;
     end;
     glEnd;
   end;
   Phi := AngTop;
   Phi2 := Phi-StepV;
   uTexFactor := 1 / Slices;
   vTexFactor := 1 / Stacks;
   for J := 0 to Stacks-1 do
   begin
     Theta := AngStart;
     SinCos( Phi, SinP, CosP );
     SinCos( Phi2, SinP2, CosP2 );
     V1[1] := SinP;
     V2[1] := SinP2;
     vTexCoord0 := 1 - j * vTexFactor;
     vTexCoord1 := 1 - ( j + 1 ) * vTexFactor;
     glBegin( GL_TRIANGLE_STRIP );
     for i := 0 to Slices do
     begin
       SinCos( Theta, SinT, CosT );
       V1[0] := CosP * SinT;
       V2[0] := CosP2 * SinT;
       V1[2] := CosP * CosT;
       V2[2] := CosP2 * CosT;
       uTexCoord := i * uTexFactor;
       xglTexCoord2f( uTexCoord, vTexCoord0 );
       if DoReverse then
       begin
         N1 := VectorNegate( V1 );
         glNormal3fv( @N1 );
       end else
         glNormal3fv( @V1 );
         glVertex3fv( @V1 );
         xglTexCoord2f( uTexCoord, vTexCoord1 );
         if DoReverse then
         begin
           N1 := VectorNegate( V2 );
           glNormal3fv( @N1 );
         end else
           glNormal3fv( @V2 );
           glVertex3fv( @V2 );
           Theta := Theta + StepH;
      end;
      glEnd;
      Phi := Phi2;
      Phi2 := Phi2 - StepV;
   end;
   if ( Bottom > -90 ) and ( BottomCap in [ ctCenter, ctFlat ] ) then
   begin
     glBegin( GL_TRIANGLE_FAN );
     SinCos( AngBottom, SinP, CosP );
     xglTexCoord2f( 0.5, 0.5 );
     if DoReverse then
       glNormal3f( 0, 1, 0 )
     else glNormal3f( 0, -1, 0 );
     if BottomCap = ctCenter then
       glVertex3f( 0, 0, 0 )
     else
     begin
       glVertex3f( 0, SinP, 0 );
       if DoReverse then
         MakeVector( N1, 0, -1, 0 )
       else
         N1 := YVector;
      end;
      V1[1] := SinP;
      Theta := AngStop;
      for I := 0 to Slices do
      begin
        SinCos( Theta, SinT, CosT );
        V1[0] := CosP * SinT;
        V1[2] := CosP * CosT;
        if TopCap = ctCenter then
        begin
          N1 := VectorPerpendicular( AffineVectorMake( 0, -1, 0 ), V1 );
          if DoReverse then
            NegateVector( N1 );
        end;
        xglTexCoord2f( SinT * 0.5 + 0.5, CosT * 0.5 + 0.5 );
        glNormal3fv( @N1 );
        glVertex3fv( @V1 );
        Theta := Theta - StepH;
      end;
     glEnd;
   end;
   if DoReverse then
     rci.GLStates.InvertGLFrontFace;
  glPopMatrix;
  glPopAttrib;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaBall.SetManager( val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBall last change by Dave Gravel
{******************************************************************************}
function TGLOXStaBall.GetGeom : PdxGeom;
begin
  if assigned( FGeom ) then
    result := FGeom else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXStaBall before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaBall.DoProgress( const progressTime : TProgressTimes );
begin
  inherited;
  if assigned( FManager ) and ( FActived ) then
  begin

  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaBall.FreeOde;
begin
  if assigned( FGeom ) then
  begin
    dGeomDestroy( FGeom );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBall last change by Dave Gravel
{******************************************************************************}
destructor TGLOXStaBall.Destroy;
begin
  FActived := False;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  inherited Destroy;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBox last change by Dave Gravel
{******************************************************************************}
constructor TGLOXStaBox.create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FActived := False;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaBox.InitODE;
begin
  if assigned( FManager ) then
  begin
    if not Assigned( FGeom ) then
    begin
      FGeom := dCreateBox( FManager.FSpace, CubeWidth, CubeHeight, CubeDepth );
      FGeom.data := self;
      SetGeomPosRot( FGeom, AbsoluteMatrix ); // k00m trace 4306
      FManager.FObjsList.Add( self );
      FActived := True;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaBox.BuildList( var rci : TRenderContextInfo );
var
  hw, hh, hd, nd  : TGLFloat;
begin
  if ( NormalDirection = ndInside ) then
    nd := -1
  else
    nd := 1;
  hw := CubeWidth * 0.5;
  hh := CubeHeight * 0.5;
  hd := CubeDepth * 0.5;
  glBegin( GL_QUADS );
  if cpFront in Parts then
  begin
    glNormal3f(  0,  0, nd );
    xglTexCoord2fv( @XYTexPoint ); glVertex3f( hw,  hh, hd );
    xglTexCoord2fv( @YTexPoint ); glVertex3f( -hw,  hh, hd );
    xglTexCoord2fv( @NullTexPoint ); glVertex3f( -hw, -hh, hd );
    xglTexCoord2fv( @XTexPoint ); glVertex3f( hw, -hh, hd );
  end;
  if cpBack in Parts then
  begin
    glNormal3f(  0,  0, -nd );
    xglTexCoord2fv( @YTexPoint ); glVertex3f( hw,  hh, -hd );
    xglTexCoord2fv( @NullTexPoint ); glVertex3f( hw, -hh, -hd );
    xglTexCoord2fv( @XTexPoint ); glVertex3f( -hw, -hh, -hd );
    xglTexCoord2fv( @XYTexPoint ); glVertex3f( -hw,  hh, -hd );
  end;
  if cpLeft in Parts then
  begin
    glNormal3f( -nd,  0,  0 );
    xglTexCoord2fv( @XYTexPoint ); glVertex3f( -hw,  hh,  hd );
    xglTexCoord2fv( @YTexPoint ); glVertex3f( -hw,  hh, -hd );
    xglTexCoord2fv( @NullTexPoint ); glVertex3f( -hw, -hh, -hd );
    xglTexCoord2fv( @XTexPoint ); glVertex3f( -hw, -hh,  hd );
  end;
  if cpRight in Parts then
  begin
    glNormal3f( nd,  0,  0 );
    xglTexCoord2fv( @YTexPoint ); glVertex3f( hw,  hh,  hd );
    xglTexCoord2fv( @NullTexPoint ); glVertex3f( hw, -hh,  hd );
    xglTexCoord2fv( @XTexPoint ); glVertex3f( hw, -hh, -hd );
    xglTexCoord2fv( @XYTexPoint ); glVertex3f( hw,  hh, -hd );
  end;
  if cpTop in Parts then
  begin
    glNormal3f(  0, nd,  0 );
    xglTexCoord2fv( @YTexPoint ); glVertex3f( -hw, hh, -hd );
    xglTexCoord2fv( @NullTexPoint ); glVertex3f( -hw, hh,  hd );
    xglTexCoord2fv( @XTexPoint ); glVertex3f( hw, hh,  hd );
    xglTexCoord2fv( @XYTexPoint ); glVertex3f( hw, hh, -hd );
  end;
  if cpBottom in Parts then
  begin
    glNormal3f(  0, -nd,  0 );
    xglTexCoord2fv( @NullTexPoint ); glVertex3f( -hw, -hh, -hd );
    xglTexCoord2fv( @XTexPoint ); glVertex3f( hw, -hh, -hd );
    xglTexCoord2fv( @XYTexPoint ); glVertex3f( hw, -hh,  hd );
    xglTexCoord2fv( @YTexPoint ); glVertex3f( -hw, -hh,  hd );
  end;
  glEnd;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaBox.SetManager( val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBox last change by Dave Gravel
{******************************************************************************}
function TGLOXStaBox.GetGeom : PdxGeom;
begin
  if assigned( FGeom ) then
    result := FGeom else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXStaBox before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaBox.DoProgress( const progressTime : TProgressTimes );
begin
  inherited;
  if assigned( FManager ) and ( FActived ) then
  begin

  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaBox.FreeOde;
begin
  if assigned( FGeom ) then
  begin
    dGeomDestroy( FGeom );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaBox last change by Dave Gravel
{******************************************************************************}
destructor TGLOXStaBox.Destroy;
begin
  FActived := False;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  inherited Destroy;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCylinder last change by Dave Gravel
{******************************************************************************}
constructor TGLOXStaCylinder.create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FActived := False;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCylinder.InitODE;
begin
  if assigned( FManager ) then
  begin
    if not Assigned( FGeom ) then
    begin
      FGeom := dCreateCylinder( FManager.FSpace, BottomRadius, Height );
      if TopRadius <> BottomRadius then
        TopRadius := BottomRadius;
      FGeom.data := self;
      SetGeomPosRot( FGeom, AbsoluteMatrix );
      FManager.FObjsList.Add( self );
      FActived := True;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCylinder.BuildList( var rci : TRenderContextInfo );
var
  quadric : PGLUquadricObj;
begin
  glPushMatrix;
  quadric := gluNewQuadric;
  SetupQuadricParams( Quadric );
  glRotatef( -90, 1, 0, 0 );
  case Alignment of
    caTop : glTranslatef( 0, 0, -Height );
    caBottom : ;
  else
    glTranslatef( 0, 0, -Height * 0.5 );
  end;
  if cySides in Parts then
    gluCylinder( Quadric, BottomRadius, TopRadius, Height, Slices, Stacks );
  if cyTop in Parts then
  begin
    glPushMatrix;
    glTranslatef( 0, 0, Height );
    gluDisk( Quadric, 0, TopRadius, Slices, Loops );
    glPopMatrix;
  end;
  if cyBottom in Parts then
  begin
    SetInvertedQuadricOrientation( quadric );
    gluDisk( quadric, 0, BottomRadius, Slices, Loops );
  end;
  gluDeleteQuadric( Quadric );
  glPopMatrix;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCylinder.SetManager( val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXStaCylinder.GetGeom : PdxGeom;
begin
  if assigned( FGeom ) then
    result := FGeom else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXStaCylinder before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCylinder.DoProgress( const progressTime : TProgressTimes );
begin
  inherited;
  if assigned( FManager ) and ( FActived ) then
  begin

  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCylinder.FreeOde;
begin
  if assigned( FGeom ) then
  begin
    dGeomDestroy( FGeom );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCylinder last change by Dave Gravel
{******************************************************************************}
destructor TGLOXStaCylinder.Destroy;
begin
  FActived := False;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  inherited Destroy;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCCylinder last change by Dave Gravel
{******************************************************************************}
(*constructor TGLOXStaCCylinder.create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FActived := False;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCCylinder.InitODE;
begin
  if assigned( FManager ) then
  begin
    if not Assigned( FGeom ) then
    begin
      FGeom := dCreateCCylinder( FManager.FSpace, BottomRadius, Height );
      if TopRadius <> BottomRadius then
        TopRadius := BottomRadius;
      FGeom.data := self;
      SetGeomPosRot( FGeom, AbsoluteMatrix );
      if ( not ( csDesigning in ComponentState ) ) then
      begin
        FCap1 := TGLSphere( AddNewChild( TGLSphere ) );
        FCap1.Position.Z := Height / 2;
        FCap1.Slices := Slices;
        FCap1.Stacks := Stacks;
        FCap1.Radius := BottomRadius;
        FCap1.Material := Material;
        FCap1.Direction.X := 0;
        FCap1.Direction.Y := 1;
        FCap1.Direction.Z := 0;
        FCap2 := TGLSphere( AddNewChild( TGLSphere ) );
        FCap2.Position.Z := Height / 2;
        FCap2.Slices := Slices;
        FCap2.Stacks := Stacks;
        FCap2.Radius := BottomRadius;
        FCap2.Material := Material;
        FCap2.Direction.X := 0;
        FCap2.Direction.Y := 1;
        FCap2.Direction.Z := 0;
        with FCap2 do
          Position.Z := -Position.Z;
        FManager.FObjsList.Add( self );  
        FActived := True;   
      end;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCCylinder.BuildList( var rci : TRenderContextInfo );
var
  quadric : PGLUquadricObj;
begin
  glPushMatrix;
  quadric := gluNewQuadric;
  SetupQuadricParams( Quadric );
  glRotatef( -90, 0, 0, 1 );
  case Alignment of
    caTop : glTranslatef( 0, 0, -Height );
    caBottom : ;
  else
    glTranslatef( 0, 0, -Height * 0.5 );
  end;
  if cySides in Parts then
    gluCylinder( Quadric, BottomRadius, TopRadius, Height, Slices, Stacks );
  if cyTop in Parts then
  begin
    glPushMatrix;
    glTranslatef( 0, 0, Height );
    gluDisk( Quadric, 0, TopRadius, Slices, Loops );
    glPopMatrix;
  end;
  if cyBottom in Parts then
  begin
    SetInvertedQuadricOrientation( quadric );
    gluDisk( quadric, 0, BottomRadius, Slices, Loops );
  end;
  gluDeleteQuadric( Quadric );
  glPopMatrix;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCCylinder.SetManager( val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXStaCCylinder.GetCap1 : TGLSphere;
begin
  if assigned( FCap1 ) then
    result := FCap1 else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXStaCCylinder before use the Cap1.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXStaCCylinder.GetCap2 : TGLSphere;
begin
  if assigned( FCap2 ) then
    result := FCap2 else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXStaCCylinder before use the Cap2.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXStaCCylinder.GetGeom : PdxGeom;
begin
  if assigned( FGeom ) then
    result := FGeom else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXStaCCylinder before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCCylinder.DoProgress( const progressTime : TProgressTimes );
begin
  inherited;
  if assigned( FManager ) and ( FActived ) then
  begin

  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCCylinder.FreeOde;
begin
  if assigned( FGeom ) then
  begin
    dGeomDestroy( FGeom );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCCylinder last change by Dave Gravel
{******************************************************************************}
destructor TGLOXStaCCylinder.Destroy;
begin
  FActived := False;
  DeleteChildren;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil
  end;;
  inherited Destroy;
end;*)
{******************************************************************************}
 // [2005-06-08]: GLOXStaMesh last change by Dave Gravel
{******************************************************************************}
constructor TGLOXStaMesh.Create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FActived := False;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaMesh.InitODE;
begin
  if assigned( FManager ) then
  begin
    if not Assigned( FGeom ) then
    begin
      FGeom := BuildTriMeshMesh( self, FManager.Fspace, Fvertices, Findices );
      FGeom.data := self;
      FManager.FObjsList.Add( self );
      FActived := True;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXStaMesh.BuildTriMeshMesh( GLBaseMesh : TGLBaseMesh;
Space : PdxSpace;
var Vertices : PdVector3Array; var Indices : PdIntegerArray ) : PdxGeom;
var
  i, j, p : integer;
  FaceExtractor : TFaceExtractor;
  VertexCount : integer;
  Vertex : TAffineVector;
  OffsetList : TIntegerList;
  Face : TFace;
  iMO : integer;
begin
  OffsetList := nil;
  FaceExtractor := TFaceExtractor.Create( GLBaseMesh );
  try
    OffsetList := TIntegerList.Create;
    FaceExtractor.ProcessMesh;
    VertexCount := 0;
    for i:= 0 to GLBaseMesh.MeshObjects.Count -1 do
    VertexCount := VertexCount + GLBaseMesh.MeshObjects[i].Vertices.Count;
    Vertices := AllocMem( sizeOf( TdVector3 ) * VertexCount );
    Indices := AllocMem( sizeOf( integer ) * FaceExtractor.FaceList.Count * 3 );
    FMem1 := sizeOf( TdVector3 ) * VertexCount;
    FMem2 := sizeOf( integer ) * FaceExtractor.FaceList.Count * 3;
    p := 0;
    for i:= 0 to GLBaseMesh.MeshObjects.Count -1 do
    begin
      OffsetList.Add( p );
      for j := 0 to GLBaseMesh.MeshObjects[i].Vertices.Count -1 do
      begin
        Vertex := GLBaseMesh.LocalToAbsolute
        ( GLBaseMesh.MeshObjects[i].Vertices[j] );
        Vertices^[ p, 0 ] := Vertex[0];
        Vertices^[ p, 1 ] := Vertex[1];
        Vertices^[ p, 2 ] := Vertex[2];
        Vertices^[ p, 3 ] := 0;
        inc( p );
      end;
    end;
    p := 0;
    for i := 0 to FaceExtractor.FaceList.Count -1 do
    begin
      Face := FaceExtractor.FaceList[i];
      iMO := GLBaseMesh.MeshObjects.IndexOf( Face.MeshObject );
      Indices^[p] := Face.Vertices[0] + OffsetList[iMO]; inc( p );
      Indices^[p] := Face.Vertices[1] + OffsetList[iMO]; inc( p );
      Indices^[p] := Face.Vertices[2] + OffsetList[iMO]; inc( p );
    end;
      FTriMeshData := dGeomTriMeshDataCreate;
      dGeomTriMeshDataBuildSimple( FTriMeshData, Vertices, VertexCount, Indices,
      FaceExtractor.FaceList.Count * 3 );
    result := dCreateTriMesh( space, FTriMeshData, nil, nil, nil );
    finally
      FaceExtractor.Free;
    if OffsetList <> nil then
      OffsetList.Free;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaMesh.FreeOde;
begin
  if assigned( FGeom ) then
  begin
    dGeomTriMeshDataDestroy( FTriMeshData );
    dGeomTriMeshClearTCCache( FGeom );
    Freemem( Fvertices, FMem1 );
    Freemem( Findices, FMem2 );
    dGeomDestroy( FGeom );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaMesh.SetManager( val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXStaMesh.GetGeom : PdxGeom;
begin
  if assigned( FGeom ) then
    result := FGeom else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXStaMesh before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaMesh.DoProgress( const progressTime : TProgressTimes );
begin
  inherited;
  if assigned( FManager ) and ( FActived ) then
  begin

  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaMesh last change by Dave Gravel
{******************************************************************************}
destructor TGLOXStaMesh.Destroy;
begin
  FActived := False;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  inherited Destroy;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCone last change by Dave Gravel
{******************************************************************************}
(*constructor TGLOXStaCone.create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FActived := False;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCone.InitODE;
begin
  if assigned( FManager ) then
  begin
    if not Assigned( FGeom ) then
    begin
      FGeom := dCreateCone( FManager.FSpace, BottomRadius, Height );
      FGeom.data := self;
      SetGeomPosRot( FGeom, AbsoluteMatrix );
      FManager.FObjsList.Add( self );
      FActived := True;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCone.BuildList( var rci : TRenderContextInfo );
var
  quadric : PGLUquadricObj;
begin
  glPushMatrix;
    quadric := gluNewQuadric();
    SetupQuadricParams( Quadric );
    glRotated( -90, 0, 0, 1 );
    glTranslatef( 0, 0, -Height * 0.5 );
    if coSides in Parts then
      gluCylinder( quadric, BottomRadius, 0, Height, Slices, Stacks );
    if coBottom in Parts then
    begin
      SetInvertedQuadricOrientation( quadric );
      gluDisk( quadric, 0, BottomRadius, Slices, Loops );
    end;
    gluDeleteQuadric( Quadric );
  glPopMatrix;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCone.SetManager( val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCone last change by Dave Gravel
{******************************************************************************}
function TGLOXStaCone.GetGeom : PdxGeom;
begin
  if assigned( FGeom ) then
    result := FGeom else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXStaCone before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCone.DoProgress( const progressTime : TProgressTimes );
begin
  inherited;
  if assigned( FManager ) and ( FActived ) then
  begin

  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXStaCone.FreeOde;
begin
  if assigned( FGeom ) then
  begin
    dGeomDestroy( FGeom );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXStaCone last change by Dave Gravel
{******************************************************************************}
destructor TGLOXStaCone.Destroy;
begin
  FActived := False;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  FManager := nil;
  inherited Destroy;
end;*)
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
constructor TGLOXDynBall.create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FObjectsSurfaceMode := mdoNoneSurface;
  FActived := False;
  FDensity := 1;
  FIgnoreSelfModel := False;
  FDisableThreshold := 0.001;
  FDisableSteps := 15;
  FWb_stepsdis := 0;
  FCoeff := 0;
  FMassReal := 1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
begin
  result := FObjectsSurfaceMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetObjectsSurfaceMode
( const val : TOXObjectsSurfaceMode );
begin
  if FObjectsSurfaceMode<> val then
  begin
    FObjectsSurfaceMode := val;
      case FObjectsSurfaceMode of
  mdoSoftSurface: begin
                    SetModes([mdSoftCFM,mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.5; soft_cfm := 0.01; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoHardSurface: begin
                    SetModes([mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.8; soft_cfm := 0; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoBoundSurface: begin
                     SetModes([mdSoftCFM,mdSoftERP,mdBounce]);
                     with FSurface do begin
                       mode := FMdMode; mu := 5; mu2 := 0; bounce := 0.5;
                       bounce_vel := 0.1;
                       soft_erp := 0.5; soft_cfm := 0.001; motion1 := 0;
                       motion2 := 0;
                       slip1 := 0; slip2 := 0;
                     end;
                     FContactNum := 8;
                   end;
  mdoDefaultSurface: begin
                       SetModes([mdSoftCFM,mdSoftERP]);
                       with FSurface do begin
                         mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                         bounce_vel := 0;
                         soft_erp := 0.8; soft_cfm := 0.001; motion1 := 0;
                         motion2 := 0;
                         slip1 := 0; slip2 := 0;
                       end;
                       FContactNum := 8;
                    end;
  mdoNoneSurface: begin

                    end;
                  end;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.InitODE;
var
  rad : TdReal;
begin
  if assigned( FManager ) then
  begin
    if not Assigned( FGeom ) then
    begin
      dMassSetZero( FMass );
      rad := Radius;
      if FMassReal <= 0 then
        FMassReal := 1;
      dMassSetSphere( FMass, FDensity, rad * FMassReal );
      FBody := dBodyCreate( FManager.FWorld );
      FGeom := dCreateSphere( FManager.FSpace, rad );
      dGeomSetBody( FGeom, FBody );
      dBodySetMass( FBody, @FMass );
      SetBodyPosRot( FBody, AbsoluteMatrix );
      FGeom.data := self;
      dBodySetLinearVel( FBody, 0, 0, -1 );
      FManager.FObjsList.Add( self );
      FActived := True;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.BuildList( var rci : TRenderContextInfo );
var
  V1, V2, N1 : TAffineVector;
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH : Extended;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
  uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1 : TdReal;
  I, J: Integer;
  DoReverse: Boolean;
begin
  DoReverse := ( NormalDirection = ndInside );
  glPushAttrib( GL_POLYGON_BIT );
  if DoReverse then
    rci.GLStates.InvertGLFrontFace;
   AngTop := DegToRad( Top );
   AngBottom := DegToRad( Bottom );
   AngStart := DegToRad( Start );
   AngStop := DegToRad( Stop );
   StepH := ( AngStop - AngStart ) / Slices;
   StepV := ( AngTop - AngBottom ) / Stacks;
   glPushMatrix;
   glScalef( Radius, Radius, Radius );
   if ( Top < 90 ) and ( TopCap in [ ctCenter, ctFlat ] ) then
   begin
     glBegin( GL_TRIANGLE_FAN );
     SinCos( AngTop, SinP, CosP );
     xglTexCoord2f( 0.5, 0.5 );
     if DoReverse then
       glNormal3f( 0, -1, 0 )
     else
       glNormal3f( 0, 1, 0 );
     if TopCap = ctCenter then
       glVertex3f( 0, 0, 0 )
     else
     begin
       glVertex3f( 0, SinP, 0 );
       N1 := YVector;
       if DoReverse then
         N1[1] := -N1[1];
     end;
     V1[1] := SinP;
     Theta := AngStart;
     for I := 0 to Slices do
     begin
       SinCos( Theta, SinT, CosT );
       V1[0] := CosP * SinT;
       V1[2] := CosP * CosT;
       if TopCap = ctCenter then
       begin
         N1 := VectorPerpendicular( YVector, V1 );
         if DoReverse then
           NegateVector( N1 );
       end;
       xglTexCoord2f( SinT * 0.5 + 0.5, CosT * 0.5 + 0.5 );
       glNormal3fv( @N1 );
       glVertex3fv( @V1 );
       Theta := Theta + StepH;
     end;
     glEnd;
   end;
   Phi := AngTop;
   Phi2 := Phi-StepV;
   uTexFactor := 1 / Slices;
   vTexFactor := 1 / Stacks;
   for J := 0 to Stacks-1 do
   begin
     Theta := AngStart;
     SinCos( Phi, SinP, CosP );
     SinCos( Phi2, SinP2, CosP2 );
     V1[1] := SinP;
     V2[1] := SinP2;
     vTexCoord0 := 1 - j * vTexFactor;
     vTexCoord1 := 1 - ( j + 1 ) * vTexFactor;
     glBegin( GL_TRIANGLE_STRIP );
     for i := 0 to Slices do
     begin
       SinCos( Theta, SinT, CosT );
       V1[0] := CosP * SinT;
       V2[0] := CosP2 * SinT;
       V1[2] := CosP * CosT;
       V2[2] := CosP2 * CosT;
       uTexCoord := i * uTexFactor;
       xglTexCoord2f( uTexCoord, vTexCoord0 );
       if DoReverse then
       begin
         N1 := VectorNegate( V1 );
         glNormal3fv( @N1 );
       end else
         glNormal3fv( @V1 );
         glVertex3fv( @V1 );
         xglTexCoord2f( uTexCoord, vTexCoord1 );
         if DoReverse then
         begin
           N1:=VectorNegate( V2 );
           glNormal3fv( @N1 );
         end else
           glNormal3fv( @V2 );
           glVertex3fv( @V2 );
           Theta := Theta + StepH;
      end;
      glEnd;
      Phi := Phi2;
      Phi2 := Phi2 - StepV;
   end;
   if ( Bottom > -90) and ( BottomCap in [ ctCenter, ctFlat ] ) then
   begin
     glBegin( GL_TRIANGLE_FAN );
     SinCos( AngBottom, SinP, CosP );
     xglTexCoord2f( 0.5, 0.5 );
     if DoReverse then
       glNormal3f( 0, 1, 0 )
     else glNormal3f( 0, -1, 0 );
     if BottomCap = ctCenter then
       glVertex3f( 0, 0, 0 )
     else
     begin
       glVertex3f( 0, SinP, 0 );
       if DoReverse then
         MakeVector( N1, 0, -1, 0 )
       else
         N1 := YVector;
      end;
      V1[1] := SinP;
      Theta := AngStop;
      for I := 0 to Slices do
      begin
        SinCos( Theta, SinT, CosT );
        V1[0] := CosP * SinT;
        V1[2] := CosP * CosT;
        if TopCap = ctCenter then
        begin
          N1 := VectorPerpendicular( AffineVectorMake( 0, -1, 0 ), V1 );
          if DoReverse then
            NegateVector( N1 );
        end;
        xglTexCoord2f( SinT * 0.5 + 0.5, CosT * 0.5 + 0.5 );
        glNormal3fv( @N1 );
        glVertex3fv( @V1 );
        Theta := Theta - StepH;
      end;
     glEnd;
   end;
   if DoReverse then
     rci.GLStates.InvertGLFrontFace;
  glPopMatrix;
  glPopAttrib;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetIgnoreSelfModel( const val : boolean );
begin
  if ( FIgnoreSelfModel <> val ) then
  begin
    FIgnoreSelfModel := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetFrictionForce( const val : TdReal );
begin
  if ( FFrictionForce <> val ) then
  begin
    FFrictionForce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetRollFriction( const val : boolean );
begin
  if ( FFakeRollForce <> val ) then
  begin
    FFakeRollForce := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetAutoDisable( const val : boolean );
begin
  if ( FAutoDisable <> val ) then
  begin
    FAutoDisable := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetManager( const val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetDisableThreshold( const val : TdReal );
begin
  if ( FDisableThreshold <> val ) then
  begin
    FDisableThreshold := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetDisableSteps( const val : TdReal );
begin
  if ( FDisableSteps <> val ) then
  begin
    FDisableSteps := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetDensity( const val : TdReal );
begin
  if ( FDensity <> val ) then
  begin
    FDensity := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetMass( const val : TdReal );
var
  m : TdMass;
begin
  if ( FMassReal <> val ) then
  begin
    dMassSetZero( m );
    FMassReal := val;
    dMassAdjust( m, FMassReal );
    dMassAdd( FMass, m );
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetModes( const val : TOXModes );
begin
  if ( FModes <> val ) then
  begin
    FModes := val;
    FMdMode := 0;
    if mdSlip2 in FModes then
      FMdMode := FMdMode or dContactSlip2;
    if mdSlip1 in FModes then
      FMdMode := FMdMode or dContactSlip1;
    if mdSoftCFM in FModes then
      FMdMode := FMdMode or dContactSoftCFM;
    if mdSoftERP in FModes then
      FMdMode := FMdMode or dContactSoftERP;
    if mdBounce in FModes then
      FMdMode := FMdMode or dContactBounce;
    if mdFDir1 in FModes then
      FMdMode := FMdMode or dContactFDir1;
    if mdMu2 in FModes then
      FMdMode := FMdMode or dContactMu2;
    FSurface.mode := FMdMode;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetMu( const val : TdReal );
begin
  if ( FSurface.mu <> val ) then
  begin
    FSurface.mu := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetMu2( const val : TdReal );
begin
  if ( FSurface.mu2 <> val ) then
  begin
    FSurface.mu2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetSlip1( const val : TdReal );
begin
  if ( FSurface.slip1 <> val ) then
  begin
    FSurface.slip1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetSlip2( const val : TdReal );
begin
  if ( FSurface.slip2 <> val ) then
  begin
    FSurface.slip2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetBounce( const val : TdReal );
begin
  if ( FSurface.bounce <> val ) then
  begin
    FSurface.bounce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetBounce_vel( const val : TdReal );
begin
  if ( FSurface.bounce_vel <> val ) then
  begin
    FSurface.bounce_vel := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetSoft_erp( const val : TdReal );
begin
  if ( FSurface.soft_erp <> val ) then
  begin
    FSurface.soft_erp := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetSoft_cfm( const val : TdReal );
begin
  if ( FSurface.soft_cfm <> val ) then
  begin
    FSurface.soft_cfm := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetMotion1( const val : TdReal );
begin
  if ( FSurface.motion1 <> val ) then
  begin
    FSurface.motion1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetMotion2( const val : TdReal );
begin
  if ( FSurface.motion2 <> val ) then
  begin
    FSurface.motion2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetContactNum( const val : integer );
begin
  if ( FContactNum <> val ) then
  begin
    FContactNum := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetApprox0( const val : boolean );
begin
  if ( FApprox0 <> val ) then
  begin
    FApprox0 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetApprox1_1( const val : boolean );
begin
  if ( FApprox1_1 <> val ) then
  begin
    FApprox1_1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetApprox1_2( const val : boolean );
begin
  if ( FApprox1_2 <> val ) then
  begin
    FApprox1_2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.SetApprox1( const val : boolean );
begin
  if ( FApprox1 <> val ) then
  begin
    FApprox1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetBody : PdxBody;
begin
  if assigned( FBody ) then
    result := FBody else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXDynBall before use the Body.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetGeom : PdxGeom;
begin
  if assigned( FGeom ) then
    result := FGeom else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXDynBall before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetDensity : TdReal;
begin
  result := FDensity;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetModes : TOXModes;
var
  OXMode : TOXModes;
begin
  OXMode := [];
  if ( FSurface.mode and dContactSlip2 ) <> 0 then
    OXMode := OXMode + [mdSlip2];
  if ( FSurface.mode and dContactSlip1 ) <> 0 then
    OXMode := OXMode + [mdSlip1];
  if ( FSurface.mode and dContactSoftCFM ) <> 0 then
    OXMode := OXMode + [mdSoftCFM];
  if ( FSurface.mode and dContactSoftERP ) <> 0 then
    OXMode := OXMode + [mdSoftERP];
  if ( FSurface.mode and dContactBounce ) <> 0 then
    OXMode := OXMode + [mdBounce];
  if ( FSurface.mode and dContactFDir1 ) <> 0 then
    OXMode := OXMode + [mdFDir1];
  if ( FSurface.mode and dContactMu2 ) <> 0 then
    OXMode := OXMode + [mdMu2];
  result := OXMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetMu : TdReal;
begin
  result := FSurface.mu;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetMu2 : TdReal;
begin
  result := FSurface.mu2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetSlip1 : TdReal;
begin
  result := FSurface.slip1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetSlip2 : TdReal;
begin
  result := FSurface.slip2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetBounce : TdReal;
begin
  result := FSurface.bounce;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetBounce_Vel : TdReal;
begin
  result := FSurface.bounce_vel;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetSoft_erp : TdReal;
begin
  result := FSurface.soft_erp;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetSoft_cfm : TdReal;
begin
  result := FSurface.soft_cfm;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetMotion1 : TdReal;
begin
  result := FSurface.motion1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBall.GetMotion2 : TdReal;
begin
  result := FSurface.motion2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.FreeOde;
begin
  if assigned( FGeom ) then
  begin
    dBodyDestroy( FBody );
    dGeomDestroy( FGeom );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.AutoDisabling;
var
  Disable : boolean;
  lvel : PdVector3;
  avel : PdVector3;
  lspeed : TdReal;
  aspeed : TdReal;
begin
  if Assigned( FBody ) then
  begin
    if ( dBodyIsEnabled( FBody ) = 1 ) then
    begin
      Disable := True;
      lvel := dBodyGetLinearVel( FBody );
      lspeed := lvel[0] * lvel[0] + lvel[1] * lvel[1] + lvel[2] * lvel[2];
      if ( lspeed > FDisableThreshold ) then
        Disable := false;
      avel := dBodyGetAngularVel( FBody );
      aspeed := avel[0] * avel[0] + avel[1] * avel[1] + avel[2] * avel[2];
      if ( aspeed > FDisableThreshold ) then
        Disable := false;
      if ( Disable ) then
        FWb_stepsdis := FWb_stepsdis + 1
      else
        FWb_stepsdis := 0;
      if ( FWb_stepsdis > FDisableSteps ) then
      begin
        dBodySetForce(FBody,0,0,0);
        dBodySetLinearVel(FBody,0,0,0);
        dBodySetAngularVel(FBody,0,0,0);
        dBodyDisable( FBody );
      end;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.FakeRollingFriction;
var
  lvel2 : PdVector3;
  SVel : TdReal;
begin
  FCoeff := 0;
  if Assigned( FBody ) then
  begin
    lvel2 := dBodyGetAngularVel( FBody );
    SVel := lvel2[0] * lvel2[0] + lvel2[1] * lvel2[1] + lvel2[2] * lvel2[2];
    if SVel > 0 then
    begin
      FCoeff := 1 - FFrictionForce / 1;
      dBodySetAngularVel( FBody, lvel2[0] * FCoeff, lvel2[1] * FCoeff, lvel2[2]
      * FCoeff );
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBall.DoProgress( const progressTime : TProgressTimes );
begin
  inherited;
  if assigned( FManager ) and ( FActived ) and
  ( dBodyIsEnabled( FBody ) = 1 ) then
  begin
    PositionSceneObject( TGLBaseSceneObject( PdxGeom( FGeom ).data ), FGeom );
    if FAutoDisable then
      AutoDisabling;
    if FFakeRollForce then
      FakeRollingFriction;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBall last change by Dave Gravel
{******************************************************************************}
destructor TGLOXDynBall.Destroy;
begin
  FActived := False;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  inherited Destroy;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
constructor TGLOXDynBox.create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FObjectsSurfaceMode := mdoNoneSurface;
  FActived := False;
  FDensity := 1;
  FIgnoreSelfModel := False;
  FDisableThreshold := 0.001;
  FDisableSteps := 15;
  FWb_stepsdis := 0;
  FCoeff := 0;
  FMassReal := 1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
begin
  result := FObjectsSurfaceMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetObjectsSurfaceMode( const val : TOXObjectsSurfaceMode );
begin
  if FObjectsSurfaceMode<> val then
  begin
    FObjectsSurfaceMode := val;
      case FObjectsSurfaceMode of
  mdoSoftSurface: begin
                    SetModes([mdSoftCFM,mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.5; soft_cfm := 0.01; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoHardSurface: begin
                    SetModes([mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.8; soft_cfm := 0; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoBoundSurface: begin
                     SetModes([mdSoftCFM,mdSoftERP,mdBounce]);
                     with FSurface do begin
                       mode := FMdMode; mu := 5; mu2 := 0; bounce := 0.5;
                       bounce_vel := 0.1;
                       soft_erp := 0.5; soft_cfm := 0.001; motion1 := 0;
                       motion2 := 0;
                       slip1 := 0; slip2 := 0;
                     end;
                     FContactNum := 8;
                   end;
  mdoDefaultSurface: begin
                       SetModes([mdSoftCFM,mdSoftERP]);
                       with FSurface do begin
                         mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                         bounce_vel := 0;
                         soft_erp := 0.8; soft_cfm := 0.001; motion1 := 0;
                         motion2 := 0;
                         slip1 := 0; slip2 := 0;
                       end;
                       FContactNum := 8;
                    end;
  mdoNoneSurface: begin

                    end;
                  end;    
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.InitODE;
begin
  if assigned( FManager ) then
  begin
    if not assigned( FGeom ) then
    begin
      dMassSetZero( FMass );
      FGeom := dCreateBox( FManager.FSpace, CubeWidth, CubeHeight, CubeDepth );
      FBody := dBodyCreate( FManager.FWorld );
      dGeomSetBody( FGeom, FBody );
      if FMassReal <= 0 then
        FMassReal := 1;
      dMassSetBox( FMass, FDensity, CubeWidth * FMassReal, CubeHeight *
      FMassReal, CubeDepth * FMassReal );
      dBodySetMass( FBody, @FMass );
      SetBodyPosRot( FBody, AbsoluteMatrix );
      FGeom.data := self;
      dBodySetLinearVel( FBody, 0, 0, -1 );
      FManager.FObjsList.Add( self );
      FActived := True;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.BuildList( var rci : TRenderContextInfo );
var
  hw, hh, hd, nd  : TGLFloat;
begin
  if NormalDirection = ndInside then
    nd := -1
  else
    nd := 1;
  hw := CubeWidth * 0.5;
  hh := CubeHeight * 0.5;
  hd := CubeDepth * 0.5;
  glBegin( GL_QUADS );
  if cpFront in Parts then
  begin
    glNormal3f(  0,  0, nd );
    xglTexCoord2fv( @XYTexPoint ); glVertex3f( hw,  hh, hd );
    xglTexCoord2fv( @YTexPoint ); glVertex3f( -hw,  hh, hd );
    xglTexCoord2fv( @NullTexPoint ); glVertex3f( -hw, -hh, hd );
    xglTexCoord2fv( @XTexPoint ); glVertex3f( hw, -hh, hd );
  end;
  if cpBack in Parts then
  begin
    glNormal3f(  0,  0, -nd );
    xglTexCoord2fv( @YTexPoint ); glVertex3f( hw,  hh, -hd );
    xglTexCoord2fv( @NullTexPoint ); glVertex3f( hw, -hh, -hd );
    xglTexCoord2fv( @XTexPoint ); glVertex3f( -hw, -hh, -hd );
    xglTexCoord2fv( @XYTexPoint ); glVertex3f( -hw,  hh, -hd );
  end;
  if cpLeft in Parts then
  begin
    glNormal3f( -nd,  0,  0 );
    xglTexCoord2fv( @XYTexPoint ); glVertex3f( -hw,  hh,  hd );
    xglTexCoord2fv( @YTexPoint ); glVertex3f( -hw,  hh, -hd );
    xglTexCoord2fv( @NullTexPoint ); glVertex3f( -hw, -hh, -hd );
    xglTexCoord2fv( @XTexPoint ); glVertex3f( -hw, -hh,  hd );
  end;
  if cpRight in Parts then
  begin
    glNormal3f( nd,  0,  0 );
    xglTexCoord2fv( @YTexPoint ); glVertex3f( hw,  hh,  hd );
    xglTexCoord2fv( @NullTexPoint ); glVertex3f( hw, -hh,  hd );
    xglTexCoord2fv( @XTexPoint ); glVertex3f( hw, -hh, -hd );
    xglTexCoord2fv( @XYTexPoint ); glVertex3f( hw,  hh, -hd );
  end;
  if cpTop in Parts then
  begin
    glNormal3f(  0, nd,  0 );
    xglTexCoord2fv( @YTexPoint ); glVertex3f( -hw, hh, -hd );
    xglTexCoord2fv( @NullTexPoint ); glVertex3f( -hw, hh,  hd );
    xglTexCoord2fv( @XTexPoint ); glVertex3f( hw, hh,  hd );
    xglTexCoord2fv( @XYTexPoint ); glVertex3f( hw, hh, -hd );
  end;
  if cpBottom in Parts then
  begin
    glNormal3f(  0, -nd,  0 );
    xglTexCoord2fv( @NullTexPoint ); glVertex3f( -hw, -hh, -hd );
    xglTexCoord2fv( @XTexPoint ); glVertex3f( hw, -hh, -hd );
    xglTexCoord2fv( @XYTexPoint ); glVertex3f( hw, -hh,  hd );
    xglTexCoord2fv( @YTexPoint ); glVertex3f( -hw, -hh,  hd );
  end;
  glEnd;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetFrictionForce( const val : TdReal );
begin
  if ( FFrictionForce <> val ) then
  begin
    FFrictionForce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetRollFriction( const val : boolean );
begin
  if ( FFakeRollForce <> val ) then
  begin
    FFakeRollForce := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetIgnoreSelfModel( const val : boolean );
begin
  if ( FIgnoreSelfModel <> val ) then
  begin
    FIgnoreSelfModel := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetAutoDisable( const val : boolean );
begin
  if ( FAutoDisable <> val ) then
  begin
    FAutoDisable := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetManager( const val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetDisableThreshold( const val : TdReal );
begin
  if ( FDisableThreshold <> val ) then
  begin
    FDisableThreshold := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetDisableSteps( const val : TdReal );
begin
  if ( FDisableSteps <> val ) then
  begin
    FDisableSteps := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetDensity( const val : TdReal );
begin
  if ( FDensity <> val ) then
  begin
    FDensity := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetMass( const val : TdReal );
var
  m : TdMass;
begin
  if ( FMassReal <> val ) then
  begin
    dMassSetZero( m );
    FMassReal := val;
    dMassAdjust( m, FMassReal );
    dMassAdd( FMass, m );
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetModes( const val : TOXModes );
begin
  if ( FModes <> val ) then
  begin
    FModes := val;
    FMdMode := 0;
    if mdSlip2 in FModes then
      FMdMode := FMdMode or dContactSlip2;
    if mdSlip1 in FModes then
      FMdMode := FMdMode or dContactSlip1;
    if mdSoftCFM in FModes then
      FMdMode := FMdMode or dContactSoftCFM;
    if mdSoftERP in FModes then
      FMdMode := FMdMode or dContactSoftERP;
    if mdBounce in FModes then
      FMdMode := FMdMode or dContactBounce;
    if mdFDir1 in FModes then
      FMdMode := FMdMode or dContactFDir1;
    if mdMu2 in FModes then
      FMdMode := FMdMode or dContactMu2;
    FSurface.mode := FMdMode;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetMu( const val : TdReal );
begin
  if ( FSurface.mu <> val ) then
  begin
    FSurface.mu := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetMu2( const val : TdReal );
begin
  if ( FSurface.mu2 <> val ) then
  begin
    FSurface.mu2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetSlip1( const val : TdReal );
begin
  if ( FSurface.slip1 <> val ) then
  begin
    FSurface.slip1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetSlip2( const val : TdReal );
begin
  if ( FSurface.slip2 <> val ) then
  begin
    FSurface.slip2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetBounce( const val : TdReal );
begin
  if ( FSurface.bounce <> val ) then
  begin
    FSurface.bounce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetBounce_vel( const val : TdReal );
begin
  if ( FSurface.bounce_vel <> val ) then
  begin
    FSurface.bounce_vel := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetSoft_erp( const val : TdReal );
begin
  if ( FSurface.soft_erp <> val ) then
  begin
    FSurface.soft_erp := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetSoft_cfm( const val : TdReal );
begin
  if ( FSurface.soft_cfm <> val ) then
  begin
    FSurface.soft_cfm := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetMotion1( const val : TdReal );
begin
  if ( FSurface.motion1 <> val ) then
  begin
    FSurface.motion1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetMotion2( const val : TdReal );
begin
  if ( FSurface.motion2 <> val ) then
  begin
    FSurface.motion2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetContactNum( const val : integer );
begin
  if ( FContactNum <> val ) then
  begin
    FContactNum := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetApprox0( const val : boolean );
begin
  if ( FApprox0 <> val ) then
  begin
    FApprox0 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetApprox1_1( const val : boolean );
begin
  if ( FApprox1_1 <> val ) then
  begin
    FApprox1_1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetApprox1_2( const val : boolean );
begin
  if ( FApprox1_2 <> val ) then
  begin
    FApprox1_2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.SetApprox1( const val : boolean );
begin
  if ( FApprox1 <> val ) then
  begin
    FApprox1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetBody : PdxBody;
begin
  if assigned( FBody ) then
    result := FBody else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXDynBox before use the Body.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetGeom : PdxGeom;
begin
  if assigned( FGeom ) then
    result := FGeom else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXDynBox before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetDensity : TdReal;
begin
  result := FDensity;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetModes : TOXModes;
var
  OXMode : TOXModes;
begin
  OXMode := [];
  if ( FSurface.mode and dContactSlip2 ) <> 0 then
    OXMode := OXMode + [mdSlip2];
  if ( FSurface.mode and dContactSlip1 ) <> 0 then
    OXMode := OXMode + [mdSlip1];
  if ( FSurface.mode and dContactSoftCFM ) <> 0 then
    OXMode := OXMode + [mdSoftCFM];
  if ( FSurface.mode and dContactSoftERP ) <> 0 then
    OXMode := OXMode + [mdSoftERP];
  if ( FSurface.mode and dContactBounce ) <> 0 then
    OXMode := OXMode + [mdBounce];
  if ( FSurface.mode and dContactFDir1 ) <> 0 then
    OXMode := OXMode + [mdFDir1];
  if ( FSurface.mode and dContactMu2 ) <> 0 then
    OXMode := OXMode + [mdMu2];
  result := OXMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetMu : TdReal;
begin
  result := FSurface.mu;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetMu2 : TdReal;
begin
  result := FSurface.mu2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetSlip1 : TdReal;
begin
  result := FSurface.slip1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetSlip2 : TdReal;
begin
  result := FSurface.slip2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetBounce : TdReal;
begin
  result := FSurface.bounce;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetBounce_Vel : TdReal;
begin
  result := FSurface.bounce_vel;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetSoft_erp : TdReal;
begin
  result := FSurface.soft_erp;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetSoft_cfm : TdReal;
begin
  result := FSurface.soft_cfm;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetMotion1 : TdReal;
begin
  result := FSurface.motion1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
function TGLOXDynBox.GetMotion2 : TdReal;
begin
  result := FSurface.motion2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.FreeOde;
begin
  if assigned( FGeom) then
  begin
    dBodyDestroy( FBody );
    dGeomDestroy( FGeom );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.AutoDisabling;
var
  Disable : boolean;
  lvel : PdVector3;
  avel : PdVector3;
  lspeed : TdReal;
  aspeed : TdReal;
begin
  if Assigned( FBody ) then
  begin
    if ( dBodyIsEnabled( FBody ) = 1 ) then
    begin
      Disable := True;
      lvel := dBodyGetLinearVel( FBody );
      lspeed := lvel[0] * lvel[0] + lvel[1] * lvel[1] + lvel[2] * lvel[2];
      if ( lspeed > FDisableThreshold ) then
        Disable := false;
      avel := dBodyGetAngularVel( FBody );
      aspeed := avel[0] * avel[0] + avel[1] * avel[1] + avel[2] * avel[2];
      if ( aspeed > FDisableThreshold ) then
        Disable := false;
      if ( Disable ) then
        FWb_stepsdis := FWb_stepsdis + 1
      else
        FWb_stepsdis := 0;
      if ( FWb_stepsdis > FDisableSteps ) then
      begin
        dBodySetForce(FBody,0,0,0);
        dBodySetLinearVel(FBody,0,0,0);
        dBodySetAngularVel(FBody,0,0,0);
        dBodyDisable( FBody );
      end;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.FakeRollingFriction;
var
  lvel2 : PdVector3;
  SVel : TdReal;
begin
  FCoeff := 0;
  if Assigned( FBody ) then
  begin
    lvel2 := dBodyGetAngularVel( FBody );
    SVel := lvel2[0] * lvel2[0] + lvel2[1] * lvel2[1] + lvel2[2] * lvel2[2];
    if SVel > 0 then
    begin
      FCoeff := 1 - FFrictionForce / 1;
      dBodySetAngularVel( FBody, lvel2[0] * FCoeff, lvel2[1] * FCoeff, lvel2[2]
      * FCoeff );
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynBox.DoProgress( const progressTime : TProgressTimes );
begin
  inherited;
  if assigned( FManager ) and ( FActived ) and
  ( dBodyIsEnabled( FBody ) = 1 ) then
  begin
    PositionSceneObject( TGLBaseSceneObject( PdxGeom( FGeom ).data ), FGeom );
    if FAutoDisable then
      AutoDisabling;
    if FFakeRollForce then
      FakeRollingFriction;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynBox last change by Dave Gravel
{******************************************************************************}
destructor TGLOXDynBox.Destroy;
begin
  FActived := False;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  inherited Destroy;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
constructor TGLOXDynCylinder.create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FObjectsSurfaceMode := mdoNoneSurface;
  FActived := False;
  FDensity := 1;
  FIgnoreSelfModel := False;
  FDisableThreshold := 0.001;
  FDisableSteps := 15;
  FWb_stepsdis := 0;
  FCoeff := 0;
  FMassReal := 1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
begin
  result := FObjectsSurfaceMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetObjectsSurfaceMode
( const val : TOXObjectsSurfaceMode );
begin
  if FObjectsSurfaceMode<> val then
  begin
    FObjectsSurfaceMode := val;
      case FObjectsSurfaceMode of
  mdoSoftSurface: begin
                    SetModes([mdSoftCFM,mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.5; soft_cfm := 0.01; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoHardSurface: begin
                    SetModes([mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.8; soft_cfm := 0; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoBoundSurface: begin
                     SetModes([mdSoftCFM,mdSoftERP,mdBounce]);
                     with FSurface do begin
                       mode := FMdMode; mu := 5; mu2 := 0; bounce := 0.5;
                       bounce_vel := 0.1;
                       soft_erp := 0.5; soft_cfm := 0.001; motion1 := 0;
                       motion2 := 0;
                       slip1 := 0; slip2 := 0;
                     end;
                     FContactNum := 8;
                   end;
  mdoDefaultSurface: begin
                       SetModes([mdSoftCFM,mdSoftERP]);
                       with FSurface do begin
                         mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                         bounce_vel := 0;
                         soft_erp := 0.8; soft_cfm := 0.001; motion1 := 0;
                         motion2 := 0;
                         slip1 := 0; slip2 := 0;
                       end;
                       FContactNum := 8;
                    end;
  mdoNoneSurface: begin

                    end;
                  end;    
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.InitODE;
begin
  if assigned( FManager ) then
  begin
    if not assigned( FGeom ) then
    begin
      dMassSetZero( FMass );
      if FMassReal <= 0 then
        FMassReal := 1;
      dMassSetBox( FMass, FDensity, Height * FMassReal, BottomRadius *
      FMassReal, Height * FMassReal );
      FGeom := dCreateCylinder( FManager.FSpace, BottomRadius, Height );
      if TopRadius <> BottomRadius then
        TopRadius := BottomRadius;
      FBody := dBodyCreate( FManager.FWorld );
      dGeomSetBody( FGeom, FBody );
      dBodySetMass( FBody, @FMass );
      SetBodyPosRot( FBody, AbsoluteMatrix );
      FGeom.data := self;
      dBodySetLinearVel( FBody, 0, 0, -1 );
      FManager.FObjsList.Add( self );
      FActived := True;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.BuildList( var rci : TRenderContextInfo );
var
  quadric : PGLUquadricObj;
begin
  glPushMatrix;
  quadric := gluNewQuadric;
  SetupQuadricParams( Quadric );
  glRotatef( -90, 1, 0, 0 );
  case Alignment of
    caTop : glTranslatef( 0, 0, -Height );
    caBottom : ;
  else
    glTranslatef( 0, 0, -Height * 0.5 );
  end;
  if cySides in Parts then
    gluCylinder( Quadric, BottomRadius, TopRadius, Height, Slices, Stacks );
  if cyTop in Parts then
  begin
    glPushMatrix;
    glTranslatef( 0, 0, Height );
    gluDisk( Quadric, 0, TopRadius, Slices, Loops );
    glPopMatrix;
  end;
  if cyBottom in Parts then
  begin
    SetInvertedQuadricOrientation( quadric );
    gluDisk( quadric, 0, BottomRadius, Slices, Loops );
  end;
  gluDeleteQuadric( Quadric );
  glPopMatrix;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetFrictionForce( const val : TdReal );
begin
  if ( FFrictionForce <> val ) then
  begin
    FFrictionForce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetRollFriction( const val : boolean );
begin
  if ( FFakeRollForce <> val ) then
  begin
    FFakeRollForce := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetAutoDisable( const val : boolean );
begin
  if ( FAutoDisable <> val ) then
  begin
    FAutoDisable := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetIgnoreSelfModel( const val : boolean );
begin
  if ( FIgnoreSelfModel <> val ) then
  begin
    FIgnoreSelfModel := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetManager( val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetDisableThreshold( const val : TdReal );
begin
  if ( FDisableThreshold <> val ) then
  begin
    FDisableThreshold := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetDisableSteps( const val : TdReal );
begin
  if ( FDisableSteps <> val ) then
  begin
    FDisableSteps := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetDensity( const val : TdReal );
begin
  if ( FDensity <> val ) then
  begin
    FDensity := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetMass( const val : TdReal );
var
  m : TdMass;
begin
  if ( FMassReal <> val ) then
  begin
    dMassSetZero( m );
    FMassReal := val;
    dMassAdjust( m, FMassReal );
    dMassAdd( FMass, m );
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetModes( const val : TOXModes );
begin
  if ( FModes <> val ) then
  begin
    FModes := val;
    FMdMode := 0;
    if mdSlip2 in FModes then
      FMdMode := FMdMode or dContactSlip2;
    if mdSlip1 in FModes then
      FMdMode := FMdMode or dContactSlip1;
    if mdSoftCFM in FModes then
      FMdMode := FMdMode or dContactSoftCFM;
    if mdSoftERP in FModes then
      FMdMode := FMdMode or dContactSoftERP;
    if mdBounce in FModes then
      FMdMode := FMdMode or dContactBounce;
    if mdFDir1 in FModes then
      FMdMode := FMdMode or dContactFDir1;
    if mdMu2 in FModes then
      FMdMode := FMdMode or dContactMu2;
    FSurface.mode := FMdMode;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetMu( const val : TdReal );
begin
  if ( FSurface.mu <> val ) then
  begin
    FSurface.mu := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetMu2( const val : TdReal );
begin
  if ( FSurface.mu2 <> val ) then
  begin
    FSurface.mu2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetSlip1( const val : TdReal );
begin
  if ( FSurface.slip1 <> val ) then
  begin
    FSurface.slip1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetSlip2( const val : TdReal );
begin
  if ( FSurface.slip2 <> val ) then
  begin
    FSurface.slip2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetBounce( const val : TdReal );
begin
  if ( FSurface.bounce <> val ) then
  begin
    FSurface.bounce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetBounce_vel( const val : TdReal );
begin
  if ( FSurface.bounce_vel <> val ) then
  begin
    FSurface.bounce_vel := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetSoft_erp( const val : TdReal );
begin
  if ( FSurface.soft_erp <> val ) then
  begin
    FSurface.soft_erp := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetSoft_cfm( const val : TdReal );
begin
  if ( FSurface.soft_cfm <> val ) then
  begin
    FSurface.soft_cfm := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetMotion1( const val : TdReal );
begin
  if ( FSurface.motion1 <> val ) then
  begin
    FSurface.motion1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetMotion2( const val : TdReal );
begin
  if ( FSurface.motion2 <> val ) then
  begin
    FSurface.motion2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetContactNum( const val : integer );
begin
  if ( FContactNum <> val ) then
  begin
    FContactNum := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetApprox0( const val : boolean );
begin
  if ( FApprox0 <> val ) then
  begin
    FApprox0 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetApprox1_1( const val : boolean );
begin
  if ( FApprox1_1 <> val ) then
  begin
    FApprox1_1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetApprox1_2( const val : boolean );
begin
  if ( FApprox1_2 <> val ) then
  begin
    FApprox1_2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.SetApprox1( const val : boolean );
begin
  if ( FApprox1 <> val ) then
  begin
    FApprox1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetBody : PdxBody;
begin
  if assigned( FBody ) then
    result := FBody else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXDynCylinder before use the Body.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetGeom : PdxGeom;
begin
  if assigned( FGeom ) then
    result := FGeom else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXDynCylinder before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetDensity : TdReal;
begin
  result := FDensity;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetModes : TOXModes;
var
  OXMode : TOXModes;
begin
  OXMode := [];
  if ( FSurface.mode and dContactSlip2 ) <> 0 then
    OXMode := OXMode + [mdSlip2];
  if ( FSurface.mode and dContactSlip1 ) <> 0 then
    OXMode := OXMode + [mdSlip1];
  if ( FSurface.mode and dContactSoftCFM ) <> 0 then
    OXMode := OXMode + [mdSoftCFM];
  if ( FSurface.mode and dContactSoftERP ) <> 0 then
    OXMode := OXMode + [mdSoftERP];
  if ( FSurface.mode and dContactBounce ) <> 0 then
    OXMode := OXMode + [mdBounce];
  if ( FSurface.mode and dContactFDir1 ) <> 0 then
    OXMode := OXMode + [mdFDir1];
  if ( FSurface.mode and dContactMu2 ) <> 0 then
    OXMode := OXMode + [mdMu2];
  result := OXMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetMu : TdReal;
begin
  result := FSurface.mu;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetMu2 : TdReal;
begin
  result := FSurface.mu2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetSlip1 : TdReal;
begin
  result := FSurface.slip1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetSlip2 : TdReal;
begin
  result := FSurface.slip2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetBounce : TdReal;
begin
  result := FSurface.bounce;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetBounce_Vel : TdReal;
begin
  result := FSurface.bounce_vel;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetSoft_erp : TdReal;
begin
  result := FSurface.soft_erp;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetSoft_cfm : TdReal;
begin
  result := FSurface.soft_cfm;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetMotion1 : TdReal;
begin
  result := FSurface.motion1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCylinder.GetMotion2 : TdReal;
begin
  result := FSurface.motion2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.FreeOde;
begin
  if assigned( FGeom ) then
  begin
    dBodyDestroy( FBody );
    dGeomDestroy( FGeom );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.AutoDisabling;
var
  Disable : boolean;
  lvel : PdVector3;
  avel : PdVector3;
  lspeed : TdReal;
  aspeed : TdReal;
begin
  if Assigned( FBody ) then
  begin
    if ( dBodyIsEnabled( FBody ) = 1 ) then
    begin
      Disable := True;
      lvel := dBodyGetLinearVel( FBody );
      lspeed := lvel[0] * lvel[0] + lvel[1] * lvel[1] + lvel[2] * lvel[2];
      if ( lspeed > FDisableThreshold ) then
        Disable := false;
      avel := dBodyGetAngularVel( FBody );
      aspeed := avel[0] * avel[0] + avel[1] * avel[1] + avel[2] * avel[2];
      if ( aspeed > FDisableThreshold ) then
        Disable := false;
      if ( Disable ) then
        FWb_stepsdis := FWb_stepsdis + 1
      else
        FWb_stepsdis := 0;
      if ( FWb_stepsdis > FDisableSteps ) then
      begin
        dBodySetForce(FBody,0,0,0);
        dBodySetLinearVel(FBody,0,0,0);
        dBodySetAngularVel(FBody,0,0,0);
        dBodyDisable( FBody );
      end;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.FakeRollingFriction;
var
  lvel2 : PdVector3;
  SVel : TdReal;
begin
  FCoeff := 0;
  if Assigned( FBody ) then
  begin
    lvel2 := dBodyGetAngularVel( FBody );
    SVel := lvel2[0] * lvel2[0] + lvel2[1] * lvel2[1] + lvel2[2] * lvel2[2];
    if SVel > 0 then
    begin
      FCoeff := 1 - FFrictionForce / 1;
      dBodySetAngularVel( FBody, lvel2[0] * FCoeff, lvel2[1] * FCoeff, lvel2[2]
      * FCoeff );
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCylinder.DoProgress( const progressTime : TProgressTimes );
begin
  inherited;
  if assigned( FManager ) and ( FActived ) and
  ( dBodyIsEnabled( FBody ) = 1 ) then
  begin
    PositionSceneObject( TGLBaseSceneObject( PdxGeom( FGeom ).data ), FGeom );
    if FAutoDisable then
      AutoDisabling;
    if FFakeRollForce then
      FakeRollingFriction;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCylinder last change by Dave Gravel
{******************************************************************************}
destructor TGLOXDynCylinder.Destroy;
begin
  FActived := False;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  inherited Destroy;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
(*constructor TGLOXDynCCylinder.create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FObjectsSurfaceMode := mdoNoneSurface;
  FActived := False;
  FDensity := 1;
  FIgnoreSelfModel := False;
  FDisableThreshold := 0.001;
  FDisableSteps := 15;
  FWb_stepsdis := 0;
  FCoeff := 0;
  FMassReal := 1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
begin
  result := FObjectsSurfaceMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetObjectsSurfaceMode
( const val : TOXObjectsSurfaceMode );
begin
  if FObjectsSurfaceMode<> val then
  begin
    FObjectsSurfaceMode := val;
      case FObjectsSurfaceMode of
  mdoSoftSurface: begin
                    SetModes([mdSoftCFM,mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.5; soft_cfm := 0.01; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoHardSurface: begin
                    SetModes([mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.8; soft_cfm := 0; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoBoundSurface: begin
                     SetModes([mdSoftCFM,mdSoftERP,mdBounce]);
                     with FSurface do begin
                       mode := FMdMode; mu := 5; mu2 := 0; bounce := 0.5;
                       bounce_vel := 0.1;
                       soft_erp := 0.5; soft_cfm := 0.001; motion1 := 0;
                       motion2 := 0;
                       slip1 := 0; slip2 := 0;
                     end;
                     FContactNum := 8;
                   end;
  mdoDefaultSurface: begin
                       SetModes([mdSoftCFM,mdSoftERP]);
                       with FSurface do begin
                         mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                         bounce_vel := 0;
                         soft_erp := 0.8; soft_cfm := 0.001; motion1 := 0;
                         motion2 := 0;
                         slip1 := 0; slip2 := 0;
                       end;
                       FContactNum := 8;
                    end;
  mdoNoneSurface: begin

                    end;
                  end;    
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.InitODE;
begin
  if assigned( FManager ) then
  begin
    if not assigned( FGeom ) then
    begin
      dMassSetZero( FMass );
      if FMassReal <= 0 then
        FMassReal := 1;
      dMassSetCappedCylinder( FMass, FDensity, 3, BottomRadius * FMassReal,
      Height * FMassReal );
      FGeom := dCreateCCylinder( FManager.FSpace, BottomRadius, Height );
      if TopRadius <> BottomRadius then
        TopRadius := BottomRadius;
      FBody := dBodyCreate( FManager.FWorld );
      dGeomSetBody( FGeom, FBody );
      dBodySetMass( FBody, @FMass );
      SetBodyPosRot( FBody, AbsoluteMatrix );
      if ( not ( csDesigning in ComponentState ) ) then
      begin
        FCap1 := TGLSphere( AddNewChild( TGLSphere ) );
        FCap1.Position.Z := Height / 2;
        FCap1.Slices := Slices;
        FCap1.Stacks := Stacks;
        FCap1.Radius := BottomRadius;
        FCap1.Material := Material;
        FCap1.Direction.X := 0;
        FCap1.Direction.Y := 1;
        FCap1.Direction.Z := 0;
        FCap2 := TGLSphere( AddNewChild( TGLSphere ) );
        FCap2.Position.Z := Height / 2;
        FCap2.Slices := Slices;
        FCap2.Stacks := Stacks;
        FCap2.Radius := BottomRadius;
        FCap2.Material := Material;
        FCap2.Direction.X := 0;
        FCap2.Direction.Y := 1;
        FCap2.Direction.Z := 0;
        with FCap2 do
          Position.Z := -Position.Z;
      end;
      FGeom.data := self;
      dBodySetLinearVel( FBody, 0, 0, -1 );
      FManager.FObjsList.Add( self );
      FActived := True;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.BuildList( var rci : TRenderContextInfo );
var
  quadric : PGLUquadricObj;
begin
  glPushMatrix;
  quadric := gluNewQuadric;
  SetupQuadricParams( Quadric );
  glRotatef( -90, 0, 0, 1 );
  case Alignment of
    caTop : glTranslatef( 0, 0, -Height );
    caBottom : ;
  else
    glTranslatef( 0, 0, -Height * 0.5 );
  end;
  if cySides in Parts then
    gluCylinder( Quadric, BottomRadius, TopRadius, Height, Slices, Stacks );
  if cyTop in Parts then
  begin
    glPushMatrix;
    glTranslatef( 0, 0, Height );
    gluDisk( Quadric, 0, TopRadius, Slices, Loops );
    glPopMatrix;
  end;
  if cyBottom in Parts then
  begin
    SetInvertedQuadricOrientation( quadric );
    gluDisk( quadric, 0, BottomRadius, Slices, Loops );
  end;
  gluDeleteQuadric( Quadric );
  glPopMatrix;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetIgnoreSelfModel( const val : boolean );
begin
  if ( FIgnoreSelfModel <> val ) then
  begin
    FIgnoreSelfModel := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetFrictionForce( const val : TdReal );
begin
  if ( FFrictionForce <> val ) then
  begin
    FFrictionForce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetRollFriction( const val : boolean );
begin
  if ( FFakeRollForce <> val ) then
  begin
    FFakeRollForce := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetAutoDisable( const val : boolean );
begin
  if ( FAutoDisable <> val ) then
  begin
    FAutoDisable := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetManager( const val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetDisableThreshold( const val : TdReal );
begin
  if ( FDisableThreshold <> val ) then
  begin
    FDisableThreshold := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetDisableSteps( const val : TdReal );
begin
  if ( FDisableSteps <> val ) then
  begin
    FDisableSteps := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetDensity( const val : TdReal );
begin
  if ( FDensity <> val ) then
  begin
    FDensity := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetMass( const val : TdReal );
var
  m : TdMass;
begin
  if ( FMassReal <> val ) then
  begin
    dMassSetZero( m );
    FMassReal := val;
    dMassAdjust( m, FMassReal );
    dMassAdd( FMass, m );
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetModes( const val : TOXModes );
begin
  if ( FModes <> val ) then
  begin
    FModes := val;
    FMdMode := 0;
    if mdSlip2 in FModes then
      FMdMode := FMdMode or dContactSlip2;
    if mdSlip1 in FModes then
      FMdMode := FMdMode or dContactSlip1;
    if mdSoftCFM in FModes then
      FMdMode := FMdMode or dContactSoftCFM;
    if mdSoftERP in FModes then
      FMdMode := FMdMode or dContactSoftERP;
    if mdBounce in FModes then
      FMdMode := FMdMode or dContactBounce;
    if mdFDir1 in FModes then
      FMdMode := FMdMode or dContactFDir1;
    if mdMu2 in FModes then
      FMdMode := FMdMode or dContactMu2;
    FSurface.mode := FMdMode;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetMu( const val : TdReal );
begin
  if ( FSurface.mu <> val ) then
  begin
    FSurface.mu := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetMu2( const val : TdReal );
begin
  if ( FSurface.mu2 <> val ) then
  begin
    FSurface.mu2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetSlip1( const val : TdReal );
begin
  if ( FSurface.slip1 <> val ) then
  begin
    FSurface.slip1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetSlip2( const val : TdReal );
begin
  if ( FSurface.slip2 <> val ) then
  begin
    FSurface.slip2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetBounce( const val : TdReal );
begin
  if ( FSurface.bounce <> val ) then
  begin
    FSurface.bounce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetBounce_vel( const val : TdReal );
begin
  if ( FSurface.bounce_vel <> val ) then
  begin
    FSurface.bounce_vel := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetSoft_erp( const val : TdReal );
begin
  if ( FSurface.soft_erp <> val ) then
  begin
    FSurface.soft_erp := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetSoft_cfm( const val : TdReal );
begin
  if ( FSurface.soft_cfm <> val ) then
  begin
    FSurface.soft_cfm := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetMotion1( const val : TdReal );
begin
  if ( FSurface.motion1 <> val ) then
  begin
    FSurface.motion1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetMotion2( const val : TdReal );
begin
  if ( FSurface.motion2 <> val ) then
  begin
    FSurface.motion2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetContactNum( const val : integer );
begin
  if ( FContactNum <> val ) then
  begin
    FContactNum := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetApprox0( const val : boolean );
begin
  if ( FApprox0 <> val ) then
  begin
    FApprox0 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetApprox1_1( const val : boolean );
begin
  if ( FApprox1_1 <> val ) then
  begin
    FApprox1_1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetApprox1_2( const val : boolean );
begin
  if ( FApprox1_2 <> val ) then
  begin
    FApprox1_2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.SetApprox1( const val : boolean );
begin
  if ( FApprox1 <> val ) then
  begin
    FApprox1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetCap1 : TGLSphere;
begin
  if assigned( FCap1 ) then
    result := FCap1 else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXDynCCylinder before use the Cap1.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetCap2 : TGLSphere;
begin
  if assigned( FCap2 ) then
    result := FCap2 else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXDynCCylinder before use the Cap2.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetBody : PdxBody;
begin
  if assigned( FBody ) then
    result := FBody else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXDynCCylinder before use the Body.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetGeom : PdxGeom;
begin
  if assigned( FGeom ) then
    result := FGeom else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXDynCCylinder before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetDensity : TdReal;
begin
  result := FDensity;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetModes : TOXModes;
var
  OXMode : TOXModes;
begin
  OXMode := [];
  if ( FSurface.mode and dContactSlip2 ) <> 0 then
    OXMode := OXMode + [mdSlip2];
  if ( FSurface.mode and dContactSlip1 ) <> 0 then
    OXMode := OXMode + [mdSlip1];
  if ( FSurface.mode and dContactSoftCFM ) <> 0 then
    OXMode := OXMode + [mdSoftCFM];
  if ( FSurface.mode and dContactSoftERP ) <> 0 then
    OXMode := OXMode + [mdSoftERP];
  if ( FSurface.mode and dContactBounce ) <> 0 then
    OXMode := OXMode + [mdBounce];
  if ( FSurface.mode and dContactFDir1 ) <> 0 then
    OXMode := OXMode + [mdFDir1];
  if ( FSurface.mode and dContactMu2 ) <> 0 then
    OXMode := OXMode + [mdMu2];
  result := OXMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetMu : TdReal;
begin
  result := FSurface.mu;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetMu2 : TdReal;
begin
  result := FSurface.mu2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetSlip1 : TdReal;
begin
  result := FSurface.slip1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetSlip2 : TdReal;
begin
  result := FSurface.slip2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetBounce : TdReal;
begin
  result := FSurface.bounce;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetBounce_Vel : TdReal;
begin
  result := FSurface.bounce_vel;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetSoft_erp : TdReal;
begin
  result := FSurface.soft_erp;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetSoft_cfm : TdReal;
begin
  result := FSurface.soft_cfm;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetMotion1 : TdReal;
begin
  result := FSurface.motion1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCCylinder.GetMotion2 : TdReal;
begin
  result := FSurface.motion2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.FreeOde;
begin
  if assigned( FGeom ) then
  begin
    dGeomDestroy( FGeom );
    dBodyDestroy( FBody );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.AutoDisabling;
var
  Disable : boolean;
  lvel : PdVector3;
  avel : PdVector3;
  lspeed : TdReal;
  aspeed : TdReal;
begin
  if Assigned( FBody ) then
  begin
    if ( dBodyIsEnabled( FBody ) = 1 ) then
    begin
      Disable := True;
      lvel := dBodyGetLinearVel( FBody );
      lspeed := lvel[0] * lvel[0] + lvel[1] * lvel[1] + lvel[2] * lvel[2];
      if ( lspeed > FDisableThreshold ) then
        Disable := false;
      avel := dBodyGetAngularVel( FBody );
      aspeed := avel[0] * avel[0] + avel[1] * avel[1] + avel[2] * avel[2];
      if ( aspeed > FDisableThreshold ) then
        Disable := false;
      if ( Disable ) then
        FWb_stepsdis := FWb_stepsdis + 1
      else
        FWb_stepsdis := 0;
      if ( FWb_stepsdis > FDisableSteps ) then
      begin
        dBodySetForce(FBody,0,0,0);
        dBodySetLinearVel(FBody,0,0,0);
        dBodySetAngularVel(FBody,0,0,0);
        dBodyDisable( FBody );
      end;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.FakeRollingFriction;
var
  lvel2 : PdVector3;
  SVel : TdReal;
begin
  FCoeff := 0;
  if Assigned( FBody ) then
  begin
    lvel2 := dBodyGetAngularVel( FBody );
    SVel := lvel2[0] * lvel2[0] + lvel2[1] * lvel2[1] + lvel2[2] * lvel2[2];
    if SVel > 0 then
    begin
      FCoeff := 1 - FFrictionForce / 1;
      dBodySetAngularVel( FBody, lvel2[0] * FCoeff, lvel2[1] * FCoeff, lvel2[2] *
      FCoeff );
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCCylinder.DoProgress( const progressTime : TProgressTimes );
begin
  inherited;
  if assigned( FManager ) and ( FActived ) and
  ( dBodyIsEnabled( FBody ) = 1 ) then
  begin
    PositionSceneObject( TGLBaseSceneObject( PdxGeom( FGeom ).data ), FGeom );
    if FAutoDisable then
      AutoDisabling;
    if FFakeRollForce then
      FakeRollingFriction;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCCylinder last change by Dave Gravel
{******************************************************************************}
destructor TGLOXDynCCylinder.Destroy;
begin
  FActived := False;
  DeleteChildren;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  inherited Destroy;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
constructor TGLOXDynCone.create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FObjectsSurfaceMode := mdoNoneSurface;
  FActived := False;
  FDensity := 1;
  FIgnoreSelfModel := False;
  FDisableThreshold := 0.001;
  FDisableSteps := 15;
  FWb_stepsdis := 0;
  FCoeff := 0;
  FMassReal := 1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
begin
  result := FObjectsSurfaceMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetObjectsSurfaceMode
( const val : TOXObjectsSurfaceMode );
begin
  if FObjectsSurfaceMode<> val then
  begin
    FObjectsSurfaceMode := val;
      case FObjectsSurfaceMode of
  mdoSoftSurface: begin
                    SetModes([mdSoftCFM,mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.5; soft_cfm := 0.01; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoHardSurface: begin
                    SetModes([mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.8; soft_cfm := 0; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoBoundSurface: begin
                     SetModes([mdSoftCFM,mdSoftERP,mdBounce]);
                     with FSurface do begin
                       mode := FMdMode; mu := 5; mu2 := 0; bounce := 0.5;
                       bounce_vel := 0.1;
                       soft_erp := 0.5; soft_cfm := 0.001; motion1 := 0;
                       motion2 := 0;
                       slip1 := 0; slip2 := 0;
                     end;
                     FContactNum := 8;
                   end;
  mdoDefaultSurface: begin
                       SetModes([mdSoftCFM,mdSoftERP]);
                       with FSurface do begin
                         mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                         bounce_vel := 0;
                         soft_erp := 0.8; soft_cfm := 0.001; motion1 := 0;
                         motion2 := 0;
                         slip1 := 0; slip2 := 0;
                       end;
                       FContactNum := 8;
                    end;
  mdoNoneSurface: begin

                    end;
                  end;    
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.InitODE;
begin
  if assigned( FManager ) then
  begin
    if not assigned( FGeom ) then
    begin
      dMassSetZero( FMass );
      FGeom := dCreateCone( FManager.FSpace, BottomRadius, Height );
      FBody := dBodyCreate( FManager.FWorld );
      dGeomSetBody( FGeom, FBody );
      SetBodyPosRot( FBody, AbsoluteMatrix );
      if FMassReal <= 0 then
        FMassReal := 1;
      FGeom.data := self;
      dMassSetCone( FMass, FDensity, BottomRadius * FMassReal, Height *
      FMassReal );
      dBodySetMass( FBody, @FMass );
      dBodySetLinearVel( FBody, 0, 0, -1 );
      FManager.FObjsList.Add( self );
      FActived := True;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.dMassSetCone(var m : TdMass; const density, radius,
length : TdReal);
var
  ms: TdReal;
begin
  ms := length/radius*density/1000;
  with m do begin
    mass := ms;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.BuildList( var rci : TRenderContextInfo );
var
  quadric : PGLUquadricObj;
begin
  glPushMatrix;
    quadric := gluNewQuadric();
    SetupQuadricParams( Quadric );
    glRotated( -90, 0, 0, 1 );
    glTranslatef( 0, 0, -Height * 0.5 );
    if coSides in Parts then
      gluCylinder( quadric, BottomRadius, 0, Height, Slices, Stacks );
    if coBottom in Parts then
    begin
      SetInvertedQuadricOrientation( quadric );
      gluDisk( quadric, 0, BottomRadius, Slices, Loops );
    end;
    gluDeleteQuadric( Quadric );
  glPopMatrix;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetFrictionForce( const val : TdReal );
begin
  if ( FFrictionForce <> val ) then
  begin
    FFrictionForce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetRollFriction( const val : boolean );
begin
  if ( FFakeRollForce <> val ) then
  begin
    FFakeRollForce := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetIgnoreSelfModel( const val : boolean );
begin
  if ( FIgnoreSelfModel <> val ) then
  begin
    FIgnoreSelfModel := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetAutoDisable( const val : boolean );
begin
  if ( FAutoDisable <> val ) then
  begin
    FAutoDisable := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetManager( val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetDisableThreshold( const val : TdReal );
begin
  if ( FDisableThreshold <> val ) then
  begin
    FDisableThreshold := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetDisableSteps( const val : TdReal );
begin
  if ( FDisableSteps <> val ) then
  begin
    FDisableSteps := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetDensity( const val : TdReal );
begin
  if ( FDensity <> val ) then
  begin
    FDensity := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetMass( const val : TdReal );
var
  m : TdMass;
begin
  if ( FMassReal <> val ) then
  begin
    dMassSetZero( m );
    FMassReal := val;
    dMassAdjust( m, FMassReal );
    dMassAdd( FMass, m );
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetContactNum(const val : integer);
begin
  if ( FContactNum <> val ) then
  begin
    FContactNum:= val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetApprox0( const val : boolean );
begin
  if ( FApprox0 <> val ) then
  begin
    FApprox0 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetApprox1_1( const val : boolean );
begin
  if ( FApprox1_1 <> val ) then
  begin
    FApprox1_1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetApprox1_2( const val : boolean );
begin
  if ( FApprox1_2 <> val ) then
  begin
    FApprox1_2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetApprox1( const val : boolean );
begin
  if ( FApprox1 <> val ) then
  begin
    FApprox1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetModes( const val : TOXModes );
begin
  if ( FModes <> val ) then
  begin
    FModes := val;
    FMdMode := 0;
    if mdSlip2 in FModes then
      FMdMode := FMdMode or dContactSlip2;
    if mdSlip1 in FModes then
      FMdMode := FMdMode or dContactSlip1;
    if mdSoftCFM in FModes then
      FMdMode := FMdMode or dContactSoftCFM;
    if mdSoftERP in FModes then
      FMdMode := FMdMode or dContactSoftERP;
    if mdBounce in FModes then
      FMdMode := FMdMode or dContactBounce;
    if mdFDir1 in FModes then
      FMdMode := FMdMode or dContactFDir1;
    if mdMu2 in FModes then
      FMdMode := FMdMode or dContactMu2;
    FSurface.mode := FMdMode;
    NotifyChange( Self );
    end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetMu( const val : TdReal );
begin
  if ( FSurface.mu <> val ) then
  begin
    FSurface.mu := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetMu2( const val : TdReal );
begin
  if ( FSurface.mu2 <> val ) then
  begin
    FSurface.mu2 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetSlip1( const val : TdReal );
begin
  if ( FSurface.slip1 <> val ) then
  begin
    FSurface.slip1 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetSlip2( const val : TdReal );
begin
  if ( FSurface.slip2 <> val ) then
  begin
    FSurface.slip2 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetBounce( const val : TdReal );
begin
  if ( FSurface.bounce <> val ) then
  begin
    FSurface.bounce:= val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetBounce_vel(const val : TdReal);
begin
  if ( FSurface.bounce_vel <> val ) then
  begin
    FSurface.bounce_vel := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetSoft_erp( const val : TdReal );
begin
  if ( FSurface.soft_erp <> val ) then
  begin
    FSurface.soft_erp := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetSoft_cfm( const val : TdReal );
begin
  if ( FSurface.soft_cfm <> val ) then
  begin
    FSurface.soft_cfm := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.Setmotion1( const val : TdReal );
begin
  if ( FSurface.motion1 <> val ) then
  begin
    FSurface.motion1 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.SetMotion2( const val : TdReal );
begin
  if ( FSurface.motion2 <> val ) then
  begin
    FSurface.motion2 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.GetDensity : TdReal;
begin
  result := FDensity;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.GetBody : PdxBody;
begin
  if assigned( FBody ) then
    result := FBody else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXDynCone before use the Body.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.GetGeom : PdxGeom;
begin
  if assigned( FGeom ) then
    result := FGeom else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXDynCone before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.GetModes : TOXModes;
var
  OXMode : TOXModes;
begin
  OXMode := [];
  if ( FSurface.mode and dContactSlip2 ) <> 0 then
    OXMode := OXMode + [ mdSlip2 ];
  if ( FSurface.mode and dContactSlip1 ) <> 0 then
    OXMode := OXMode + [ mdSlip1 ];
  if ( FSurface.mode and dContactSoftCFM ) <> 0 then
    OXMode := OXMode + [ mdSoftCFM ];
  if ( FSurface.mode and dContactSoftERP) <> 0 then
    OXMode := OXMode + [ mdSoftERP ];
  if ( FSurface.mode and dContactBounce ) <> 0 then
    OXMode := OXMode + [ mdBounce ];
  if ( FSurface.mode and dContactFDir1 ) <> 0 then
    OXMode := OXMode + [ mdFDir1 ];
  if ( FSurface.mode and dContactMu2 ) <> 0 then
    OXMode := OXMode + [ mdMu2 ];
    result := OXMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.GetMu : TdReal;
begin
  result := FSurface.mu;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.GetMu2 : TdReal;
begin
  result := FSurface.mu2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.GetSlip1 : TdReal;
begin
  result := FSurface.slip1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.GetSlip2 : TdReal;
begin
  result := FSurface.slip2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.GetBounce : TdReal;
begin
  result := FSurface.bounce;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.GetBounce_vel : TdReal;
begin
  result := FSurface.bounce_vel;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.GetSoft_erp : TdReal;
begin
  result := FSurface.soft_erp;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.GetSoft_cfm : TdReal;
begin
  result := FSurface.soft_cfm;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.Getmotion1 : TdReal;
begin
  result := FSurface.motion1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCone.GetMotion2 : TdReal;
begin
  result := FSurface.motion2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.AutoDisabling;
var
  Disable : boolean;
  lvel : PdVector3;
  avel : PdVector3;
  lspeed : TdReal;
  aspeed : TdReal;
begin
  if Assigned( FBody ) then
  begin
    if ( dBodyIsEnabled( FBody ) = 1 ) then
    begin
      Disable := True;
      lvel := dBodyGetLinearVel( FBody );
      lspeed := lvel[0] * lvel[0] + lvel[1] * lvel[1] + lvel[2] * lvel[2];
      if ( lspeed > FDisableThreshold ) then
        Disable := false;
      avel := dBodyGetAngularVel( FBody );
      aspeed := avel[0] * avel[0] + avel[1] * avel[1] + avel[2] * avel[2];
      if ( aspeed > FDisableThreshold ) then
        Disable := false;
      if ( Disable ) then
        FWb_stepsdis := FWb_stepsdis + 1
      else
        FWb_stepsdis := 0;
      if ( FWb_stepsdis > FDisableSteps ) then
      begin
        dBodySetForce(FBody,0,0,0);
        dBodySetLinearVel(FBody,0,0,0);
        dBodySetAngularVel(FBody,0,0,0);
        dBodyDisable( FBody );
      end;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.FakeRollingFriction;
var
  lvel2 : PdVector3;
  SVel : TdReal;
begin
  FCoeff := 0;
  if Assigned( FBody ) then
  begin
    lvel2 := dBodyGetAngularVel( FBody );
    SVel := lvel2[0] * lvel2[0] + lvel2[1] * lvel2[1] + lvel2[2] * lvel2[2];
    if SVel > 0 then
    begin
      FCoeff := 1 - FFrictionForce / 1;
      dBodySetAngularVel( FBody, lvel2[0] * FCoeff, lvel2[1] * FCoeff, lvel2[2]
      * FCoeff );
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.DoProgress( const progressTime : TProgressTimes );
begin
  inherited;
  if assigned( FManager ) and ( FActived ) and
  ( dBodyIsEnabled( FBody ) = 1 ) then
  begin
    PositionSceneObject( TGLBaseSceneObject( PdxGeom( FGeom ).data ), FGeom );
      if FAutoDisable then
        AutoDisabling;
      if FFakeRollForce then
        FakeRollingFriction;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCone.FreeOde;
begin
  if assigned( FGeom ) then
  begin
    dBodyDestroy( FBody );
    dGeomDestroy( FGeom );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCone last change by Dave Gravel
{******************************************************************************}
destructor TGLOXDynCone.Destroy;
begin
  FActived := False;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  inherited Destroy;
end;*)
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
constructor TGLOXDynMesh.Create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FObjectsSurfaceMode := mdoNoneSurface;
  FActived := False;
  FDensity := 1;
  FIgnoreSelfModel := False;
  FPos := TGLCoordinates.CreateInitialized( self, NullHmgPoint, csVector );
  FDisableThreshold := 0.001;
  FDisableSteps := 15;
  FWb_stepsdis := 0;
  FCoeff := 0;
  FMassReal := 1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
begin
  result := FObjectsSurfaceMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetObjectsSurfaceMode
( const val : TOXObjectsSurfaceMode );
begin
  if FObjectsSurfaceMode<> val then
  begin
    FObjectsSurfaceMode := val;
      case FObjectsSurfaceMode of
  mdoSoftSurface: begin
                    SetModes([mdSoftCFM,mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.5; soft_cfm := 0.01; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoHardSurface: begin
                    SetModes([mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.8; soft_cfm := 0; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoBoundSurface: begin
                     SetModes([mdSoftCFM,mdSoftERP,mdBounce]);
                     with FSurface do begin
                       mode := FMdMode; mu := 5; mu2 := 0; bounce := 0.5;
                       bounce_vel := 0.1;
                       soft_erp := 0.5; soft_cfm := 0.001; motion1 := 0;
                       motion2 := 0;
                       slip1 := 0; slip2 := 0;
                     end;
                     FContactNum := 8;
                   end;
  mdoDefaultSurface: begin
                       SetModes([mdSoftCFM,mdSoftERP]);
                       with FSurface do begin
                         mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                         bounce_vel := 0;
                         soft_erp := 0.8; soft_cfm := 0.001; motion1 := 0;
                         motion2 := 0;
                         slip1 := 0; slip2 := 0;
                       end;
                       FContactNum := 8;
                    end;
  mdoNoneSurface: begin

                    end;
                  end;    
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.InitODE;
begin
  if assigned( FManager ) then
  begin
    if not Assigned( FGeom ) then
    begin
      dMassSetZero( FMass );
      FPos.X := Position.X;
      FPos.Y := Position.Y;
      FPos.Z := Position.Z;
      Position.X := 0;
      Position.Y := 0;
      Position.Z := 0;
      FBody := dBodyCreate( FManager.FWorld );
      FGeom := BuildTriMeshMesh( self, FManager.Fspace, Fvertices, Findices );
      dGeomSetBody( FGeom, FBody );
      dBodySetPosition( FBody, FPos.X, FPos.Y, FPos.Z );
      FGeom.data := self;
      dBodySetLinearVel( FBody, 0, 0, -1 );
      if FMassReal <= 0 then
        FMassReal := 1;
      dMassSetBox( FMass, FDensity, Scale.X * FMassReal, Scale.Y * FMassReal,
      Scale.Z * FMassReal );
      //dMassSetTriMesh( FMass, FDensity, Fvertices, Findices );
      FManager.FObjsList.Add( self );
      FActived := True;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.BuildList( var rci : TRenderContextInfo );
begin
  MeshObjects.BuildList( rci );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.BuildTriMeshMesh
( GLBaseMesh : TGLBaseMesh; Space : PdxSpace;
var Vertices : PdVector3Array; var Indices : PdIntegerArray ) : PdxGeom;
var
  i, j, p : integer;
  FaceExtractor : TFaceExtractor;
  VertexCount : integer;
  Vertex : TAffineVector;
  OffsetList : TIntegerList;
  Face : TFace;
  iMO : integer;
begin
  OffsetList := nil;
  FaceExtractor := TFaceExtractor.Create( GLBaseMesh );
  try
    OffsetList := TIntegerList.Create;
    FaceExtractor.ProcessMesh;
    VertexCount := 0;
    for i:= 0 to GLBaseMesh.MeshObjects.Count -1 do
    VertexCount := VertexCount + GLBaseMesh.MeshObjects[i].Vertices.Count;
    Vertices := AllocMem( sizeOf( TdVector3 ) * VertexCount );
    Indices := AllocMem( sizeOf( integer ) * FaceExtractor.FaceList.Count * 3 );
    FMem1 := sizeOf( TdVector3 ) * VertexCount;
    FMem2 := sizeOf( integer ) * FaceExtractor.FaceList.Count * 3;
    p := 0;
    for i:= 0 to GLBaseMesh.MeshObjects.Count -1 do
    begin
      OffsetList.Add( p );
      for j := 0 to GLBaseMesh.MeshObjects[i].Vertices.Count -1 do
      begin
        Vertex := GLBaseMesh.LocalToAbsolute
        ( GLBaseMesh.MeshObjects[i].Vertices[j] );
        Vertices^[ p, 0 ] := Vertex[0];
        Vertices^[ p, 1 ] := Vertex[1];
        Vertices^[ p, 2 ] := Vertex[2];
        Vertices^[ p, 3 ] := 0;
        inc( p );
      end;
    end;
    p := 0;
    for i := 0 to FaceExtractor.FaceList.Count -1 do
    begin
      Face := FaceExtractor.FaceList[i];
      iMO := GLBaseMesh.MeshObjects.IndexOf( Face.MeshObject );
      Indices^[p] := Face.Vertices[0] + OffsetList[iMO]; inc( p );
      Indices^[p] := Face.Vertices[1] + OffsetList[iMO]; inc( p );
      Indices^[p] := Face.Vertices[2] + OffsetList[iMO]; inc( p );
    end;
      FTriMeshData := dGeomTriMeshDataCreate;
      dGeomTriMeshDataBuildSimple( FTriMeshData, Vertices, VertexCount, Indices,
      FaceExtractor.FaceList.Count * 3 );
    result := dCreateTriMesh( space, FTriMeshData, nil, nil, nil );
    finally
      FaceExtractor.Free;
    if OffsetList <> nil then
      OffsetList.Free;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.FreeOde;
begin
  if assigned( FGeom ) then
  begin
    dGeomTriMeshDataDestroy( FTriMeshData );
    dGeomTriMeshClearTCCache( FGeom );
    Freemem( Fvertices, FMem1 );
    Freemem( Findices, FMem2 );
    dBodyDestroy( FBody );
    dGeomDestroy( FGeom );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetFrictionForce( const val : TdReal );
begin
  if ( FFrictionForce <> val ) then
  begin
    FFrictionForce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetRollFriction( const val : boolean );
begin
  if ( FFakeRollForce <> val ) then
  begin
    FFakeRollForce := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetIgnoreSelfModel( const val : boolean );
begin
  if ( FIgnoreSelfModel <> val ) then
  begin
    FIgnoreSelfModel := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetAutoDisable( const val : boolean );
begin
  if ( FAutoDisable <> val ) then
  begin
    FAutoDisable := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetManager( val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetMass( const val : TdReal );
var
  m : TdMass;
begin
  if ( FMassReal <> val ) then
  begin
    dMassSetZero( m );
    FMassReal := val;
    dMassAdjust( m, FMassReal );
    dMassAdd( FMass, m );
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetDisableThreshold( const val : TdReal );
begin
  if ( FDisableThreshold <> val ) then
  begin
    FDisableThreshold := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetDisableSteps( const val : TdReal );
begin
  if ( FDisableSteps <> val ) then
  begin
    FDisableSteps := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetContactNum(const val : integer);
begin
  if ( FContactNum <> val ) then
  begin
    FContactNum:= val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetApprox0( const val : boolean );
begin
  if ( FApprox0 <> val ) then
  begin
    FApprox0 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetApprox1_1( const val : boolean );
begin
  if ( FApprox1_1 <> val ) then
  begin
    FApprox1_1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetApprox1_2( const val : boolean );
begin
  if ( FApprox1_2 <> val ) then
  begin
    FApprox1_2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetApprox1( const val : boolean );
begin
  if ( FApprox1 <> val ) then
  begin
    FApprox1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetModes( const val : TOXModes );
begin
  if ( FModes <> val ) then
  begin
    FModes := val;
    FMdMode := 0;
    if mdSlip2 in FModes then
      FMdMode := FMdMode or dContactSlip2;
    if mdSlip1 in FModes then
      FMdMode := FMdMode or dContactSlip1;
    if mdSoftCFM in FModes then
      FMdMode := FMdMode or dContactSoftCFM;
    if mdSoftERP in FModes then
      FMdMode := FMdMode or dContactSoftERP;
    if mdBounce in FModes then
      FMdMode := FMdMode or dContactBounce;
    if mdFDir1 in FModes then
      FMdMode := FMdMode or dContactFDir1;
    if mdMu2 in FModes then
      FMdMode := FMdMode or dContactMu2;
    FSurface.mode := FMdMode;
    NotifyChange( Self );
    end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetMu( const val : TdReal );
begin
  if ( FSurface.mu <> val ) then
  begin
    FSurface.mu := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetMu2( const val : TdReal );
begin
  if ( FSurface.mu2 <> val ) then
  begin
    FSurface.mu2 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetSlip1( const val : TdReal );
begin
  if ( FSurface.slip1 <> val ) then
  begin
    FSurface.slip1 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetSlip2( const val : TdReal );
begin
  if ( FSurface.slip2 <> val ) then
  begin
    FSurface.slip2 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetBounce( const val : TdReal );
begin
  if ( FSurface.bounce <> val ) then
  begin
    FSurface.bounce:= val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetBounce_vel(const val : TdReal);
begin
  if ( FSurface.bounce_vel <> val ) then
  begin
    FSurface.bounce_vel := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetSoft_erp( const val : TdReal );
begin
  if ( FSurface.soft_erp <> val ) then
  begin
    FSurface.soft_erp := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetSoft_cfm( const val : TdReal );
begin
  if ( FSurface.soft_cfm <> val ) then
  begin
    FSurface.soft_cfm := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.Setmotion1( const val : TdReal );
begin
  if ( FSurface.motion1 <> val ) then
  begin
    FSurface.motion1 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetMotion2( const val : TdReal );
begin
  if ( FSurface.motion2 <> val ) then
  begin
    FSurface.motion2 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.GetBody : PdxBody;
begin
  if assigned( FBody ) then
    result := FBody else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXDynMesh before use the Body.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.GetGeom : PdxGeom;
begin
  if assigned( FGeom ) then
    result := FGeom else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXDynMesh before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.GetModes : TOXModes;
var
  OXMode : TOXModes;
begin
  OXMode := [];
  if ( FSurface.mode and dContactSlip2 ) <> 0 then
    OXMode := OXMode + [ mdSlip2 ];
  if ( FSurface.mode and dContactSlip1 ) <> 0 then
    OXMode := OXMode + [ mdSlip1 ];
  if ( FSurface.mode and dContactSoftCFM ) <> 0 then
    OXMode := OXMode + [ mdSoftCFM ];
  if ( FSurface.mode and dContactSoftERP) <> 0 then
    OXMode := OXMode + [ mdSoftERP ];
  if ( FSurface.mode and dContactBounce ) <> 0 then
    OXMode := OXMode + [ mdBounce ];
  if ( FSurface.mode and dContactFDir1 ) <> 0 then
    OXMode := OXMode + [ mdFDir1 ];
  if ( FSurface.mode and dContactMu2 ) <> 0 then
    OXMode := OXMode + [ mdMu2 ];
    result := OXMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.GetMu : TdReal;
begin
  result := FSurface.mu;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.GetMu2 : TdReal;
begin
  result := FSurface.mu2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.GetSlip1 : TdReal;
begin
  result := FSurface.slip1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.GetSlip2 : TdReal;
begin
  result := FSurface.slip2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.GetBounce : TdReal;
begin
  result := FSurface.bounce;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.GetBounce_vel : TdReal;
begin
  result := FSurface.bounce_vel;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.GetSoft_erp : TdReal;
begin
  result := FSurface.soft_erp;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.GetSoft_cfm : TdReal;
begin
  result := FSurface.soft_cfm;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.Getmotion1 : TdReal;
begin
  result := FSurface.motion1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.GetMotion2 : TdReal;
begin
  result := FSurface.motion2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
function TGLOXDynMesh.GetDensity : TdReal;
begin
  result := FDensity;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.SetDensity( const val : TdReal );
begin
  if ( FDensity <> val ) then
  begin
    FDensity := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.AutoDisabling;
var
  Disable : boolean;
  lvel : PdVector3;
  avel : PdVector3;
  lspeed : TdReal;
  aspeed : TdReal;
begin
  if Assigned( FBody ) then
  begin
    if ( dBodyIsEnabled( FBody ) = 1 ) then
    begin
      Disable := True;
      lvel := dBodyGetLinearVel( FBody );
      lspeed := lvel[0] * lvel[0] + lvel[1] * lvel[1] + lvel[2] * lvel[2];
      if ( lspeed > FDisableThreshold ) then
        Disable := false;
      avel := dBodyGetAngularVel( FBody );
      aspeed := avel[0] * avel[0] + avel[1] * avel[1] + avel[2] * avel[2];
      if ( aspeed > FDisableThreshold ) then
        Disable := false;
      if ( Disable ) then
        FWb_stepsdis := FWb_stepsdis + 1
      else
        FWb_stepsdis := 0;
      if ( FWb_stepsdis > FDisableSteps ) then
      begin
        dBodySetForce(FBody,0,0,0);
        dBodySetLinearVel(FBody,0,0,0);
        dBodySetAngularVel(FBody,0,0,0);
        dBodyDisable( FBody );
      end;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.FakeRollingFriction;
var
  lvel2 : PdVector3;
  SVel : TdReal;
begin
  FCoeff := 0;
  if Assigned( FBody ) then
  begin
    lvel2 := dBodyGetAngularVel( FBody );
    SVel := lvel2[0] * lvel2[0] + lvel2[1] * lvel2[1] + lvel2[2] * lvel2[2];
    if SVel > 0 then
    begin
      FCoeff := 1 - FFrictionForce / 1;
      dBodySetAngularVel( FBody, lvel2[0] * FCoeff, lvel2[1] * FCoeff, lvel2[2]
      * FCoeff );
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynMesh.DoProgress( const progressTime : TProgressTimes );
begin
  inherited;
  if assigned( FManager ) and ( FActived ) and
  ( dBodyIsEnabled( FBody ) = 1 ) then
  begin
    PositionSceneObject( TGLBaseSceneObject( PdxGeom( FGeom ).data ), FGeom );
    if FAutoDisable then
      AutoDisabling;
    if FFakeRollForce then
      FakeRollingFriction;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynMesh last change by Dave Gravel
{******************************************************************************}
destructor TGLOXDynMesh.Destroy;
begin
  FActived := False;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  FPos.Free;
  inherited Destroy;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
(*constructor TGLOXDynCar.Create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  FObjectsSurfaceMode := mdoNoneSurface;
  FModeTraction := mdBoth;
  FActived := False;
  FContactNum := 24;
  FZOOM := 1;
  FZOOM_CUBED := FZOOM * FZOOM * FZOOM;
  FLENGTH := 2.8 * FZOOM;
  FWIDTH := 0.95 * FZOOM;
  FHEIGHT := 0.70 * FZOOM;
  FRADIUS := 0.5 * FZOOM;
  FCMASSReal := 5 * FZOOM;
  FWMASSReal := 1 * FZOOM;
  FSTARTZ := 0 * FZOOM;
  FWHEEL_WOBBLE := 0 * FZOOM;
  FFRONTSUSPENSION_CFM := 0 * FZOOM;
  FFRONTSUSPENSION_ERP := 0 * FZOOM;
  FBACKSUSPENSION_CFM := 0 * FZOOM;
  FBACKSUSPENSION_ERP := 0 * FZOOM;
  FWHEEL_OFFSET := 0.032 * FZOOM;
  FIgnoreSelfModel := False;
  FWheelsParamFMax := 100.0;
  FWheelsFrontLoHiStop := 1;
  FWheelsBackLoHiStop := 0;
  FWPosA := TGLCoordinates.CreateInitialized( self, NullHmgPoint, csVector );
  FWPosB := TGLCoordinates.CreateInitialized( self, NullHmgPoint, csVector );
  FWPosC := TGLCoordinates.CreateInitialized( self, NullHmgPoint, csVector );
  FWPosD := TGLCoordinates.CreateInitialized( self, NullHmgPoint, csVector );
  FDensity := 1;
  FFrontWheelLimit := 1;
  FFrontWheelTurnDegre := 3;
  FFrontWheelBackUp := 0.25;
  FFrontBackWheelRunForce := 5;
  FFrontBackWheelAltForce := 1;
  FDisableThreshold := 0.001;
  FDisableSteps := 15;
  FWb_stepsdis := 0;
  FCoeff := 0;
  FACCEL := 0.010/5;
  FTURN_SPEED := 0.5/10;
  FSteer := 0;
  FSpeed := 0;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
begin
  result := FObjectsSurfaceMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetObjectsSurfaceMode
( const val : TOXObjectsSurfaceMode );
begin
  if FObjectsSurfaceMode<> val then
  begin
    FObjectsSurfaceMode := val;
      case FObjectsSurfaceMode of
  mdoSoftSurface: begin
                    SetModes([mdSoftCFM,mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.5; soft_cfm := 0.01; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoHardSurface: begin
                    SetModes([mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.8; soft_cfm := 0; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoBoundSurface: begin
                     SetModes([mdSoftCFM,mdSoftERP,mdBounce]);
                     with FSurface do begin
                       mode := FMdMode; mu := 5; mu2 := 0; bounce := 0.5;
                       bounce_vel := 0.1;
                       soft_erp := 0.5; soft_cfm := 0.001; motion1 := 0;
                       motion2 := 0;
                       slip1 := 0; slip2 := 0;
                     end;
                     FContactNum := 8;
                   end;
  mdoDefaultSurface: begin
                       SetModes([mdSoftCFM,mdSoftERP]);
                       with FSurface do begin
                         mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                         bounce_vel := 0;
                         soft_erp := 0.8; soft_cfm := 0.001; motion1 := 0;
                         motion2 := 0;
                         slip1 := 0; slip2 := 0;
                       end;
                       FContactNum := 8;
                    end;
  mdoNoneSurface: begin

                    end;
                  end;    
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetDisableThreshold( const val : TdReal );
begin
  if ( FDisableThreshold <> val ) then
  begin
    FDisableThreshold := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetDisableSteps( const val : TdReal );
begin
  if ( FDisableSteps <> val ) then
  begin
    FDisableSteps := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetWheelPosA( val : TGLCoordinates );
begin
  if ( FWPosA <> val ) then
  begin
    FWPosA.SetPoint( val.DirectX, val.DirectY, val.DirectZ );
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetWheelPosB( val : TGLCoordinates );
begin
  if ( FWPosB <> val ) then
  begin
    FWPosB.SetPoint( val.DirectX, val.DirectY, val.DirectZ );
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetWheelPosC( val : TGLCoordinates );
begin
  if ( FWPosC <> val ) then
  begin
    FWPosC.SetPoint( val.DirectX, val.DirectY, val.DirectZ );
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetWheelPosD( val : TGLCoordinates );
begin
  if ( FWPosD <> val ) then
  begin
    FWPosD.SetPoint( val.DirectX, val.DirectY, val.DirectZ );
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.InitODE;
var
  i : integer;
  q : TdQuaternion;
  a : PdVector3;
begin
  if ( assigned( FManager ) ) and ( not assigned( FFrame[0] ) ) then
  begin
    dMassSetZero( FMass1 );
    dMassSetZero( FMass2 );
    if ( not ( csDesigning in ComponentState ) ) then begin
      FDummyFrame := TGLSizableDummyCube( AddNewChild( TGLSizableDummyCube ) );
      FWheelLF := TGLSizableDummyCube( AddNewChild( TGLSizableDummyCube ) );
      FWheelLB := TGLSizableDummyCube( AddNewChild( TGLSizableDummyCube ) );
      FWheelRF := TGLSizableDummyCube( AddNewChild( TGLSizableDummyCube ) );
      FWheelRB := TGLSizableDummyCube( AddNewChild( TGLSizableDummyCube ) );
    end;
      with FDummyFrame do begin
        CubeWidth := FLENGTH; CubeHeight := FWIDTH; CubeDepth := FHEIGHT; end;
      FBody[0] := dBodyCreate( FManager.FWorld );
      dMassSetBox( FMass1, FDensity, FLENGTH, FWIDTH, FHEIGHT );
      dMassAdjust( FMass1, FCMASSReal );
      dBodySetMass( FBody[0], @FMass1 );
      FFrame[0] := dCreateBox( FManager.FSpace, FLENGTH, FWIDTH, FHEIGHT );
      dGeomSetBody( FFrame[0], FBody[0] );
      FFrame[0].data := FDummyFrame;
      FDummyFrame.Position.AsVector := AbsolutePosition;
      SetBodyPosRot( FBody[0], FDummyFrame.AbsoluteMatrix );
      Position.SetPoint(0,0,0);
      FWheelLF.Position.SetPoint( FLENGTH - FLENGTH / 2 +
      FDummyFrame.Position.X* 2 + FWPosA.X,
      FWIDTH - FWIDTH / 2 + FDummyFrame.Position.Y * 2 + FWPosA.Y,
      FSTARTZ -FHEIGHT + FWHEEL_OFFSET +
      FDummyFrame.Position.Z * 2 + FWPosA.Z );
      FWheelLB.Position.SetPoint( FLENGTH -FLENGTH / 2 +
      FDummyFrame.Position.X * 2 + FWPosB.X,
      -FWIDTH + FWIDTH / 2 + FDummyFrame.Position.Y * 2 + FWPosB.Y,
      FSTARTZ -FHEIGHT + FWHEEL_OFFSET +
      FDummyFrame.Position.Z * 2 + FWPosB.Z );
      FWheelRF.Position.SetPoint( -FLENGTH + FLENGTH / 2 +
      FDummyFrame.Position.X * 2 + FWPosC.X,
      FWIDTH -FWIDTH / 2 + FDummyFrame.Position.Y * 2 + FWPosC.Y,
      FSTARTZ -FHEIGHT + FWHEEL_OFFSET +
      FDummyFrame.Position.Z * 2 + FWPosC.Z );
      FWheelRB.Position.SetPoint( -FLENGTH + FLENGTH / 2 +
      FDummyFrame.Position.X * 2 + FWPosD.X,
      -FWIDTH + FWIDTH / 2 + FDummyFrame.Position.Y * 2 + FWPosD.Y,
      FSTARTZ -FHEIGHT + FWHEEL_OFFSET +
      FDummyFrame.Position.Z * 2 + FWPosD.Z );
      for i:= 1 to 4 do begin
        FBody[i] := dBodyCreate( FManager.FWorld );
        dQFromAxisAndAngle( q, 1, 0, 0, PI * 0.5 );
        dBodySetQuaternion( FBody[i], q );
        dMassSetSphere( FMass2, FDensity, FRADIUS );
        FWheel[i-1] := dCreateSphere( FManager.FSpace, FRADIUS );
        dGeomSetBody( FWheel[i-1], FBody[i] );
        if ( i = 1 ) then begin with FWheelLF do begin
            CubeWidth := FRADIUS * 2; CubeHeight := FRADIUS * 2;
            CubeDepth := FRADIUS * 2;
          end;
          dMassAdjust( FMass2, FWMASSReal ); dBodySetMass( FBody[i], @FMass2 );
          FWheel[i-1].data := FWheelLF; SetBodyPosRot( FBody[1],
          FWheelLF.AbsoluteMatrix );
        end;
        if ( i = 2 ) then begin with FWheelRF do begin
            CubeWidth := FRADIUS * 2; CubeHeight := FRADIUS * 2;
            CubeDepth := FRADIUS * 2;
          end;
          dMassAdjust( FMass2, FWMASSReal ); dBodySetMass( FBody[i], @FMass2 );
          FWheel[i-1].data := FWheelRF; SetBodyPosRot( FBody[2],
          FWheelLB.AbsoluteMatrix );
        end;
        if ( i = 3 ) then begin with FWheelLB do begin
            CubeWidth := FRADIUS * 2; CubeHeight := FRADIUS * 2;
            CubeDepth := FRADIUS * 2;
          end;
          dMassAdjust( FMass2, FWMASSReal ); dBodySetMass( FBody[i], @FMass2 );
          FWheel[i-1].data := FWheelLB; SetBodyPosRot( FBody[3],
          FWheelRF.AbsoluteMatrix );
        end;
        if ( i = 4 ) then begin with FWheelRB do begin
            CubeWidth := FRADIUS * 2; CubeHeight := FRADIUS * 2;
            CubeDepth := FRADIUS * 2;
          end;
          dMassAdjust( FMass2, FWMASSReal ); dBodySetMass( FBody[i], @FMass2 );
          FWheel[i-1].data := FWheelRB; SetBodyPosRot( FBody[4],
          FWheelRB.AbsoluteMatrix );
        end;
      end;
      for i := 0 to 3 do begin
        FJoint[i] := dJointCreateHinge( FManager.FWorld, 0 );
        dJointAttach( FJoint[i], FBody[0], FBody[ i + 1 ] );
        A := dBodyGetPosition( FBody[ i + 1 ] );
        dJointSetHingeAnchor( FJoint[i], a[0], a[1], a[2] );
        dJointSetHingeAxis1( FJoint[i], 0, 0, 1 );
        dJointSetHingeAxis2( FJoint[i], 0, 1, 0 );
        dJointSetHingeParam( FJoint[i], dParamLoStop, -0 );
        dJointSetHingeParam( FJoint[i], dParamHiStop, 0 );
        dJointSetHingeParam( FJoint[i], dParamFMax, FWheelsParamFMax );
        if ( i = 0 ) or ( i = 1 ) then begin
          dJointSetHingeParam( FJoint[i], dParamSuspensionERP,
          FFRONTSUSPENSION_ERP );
          dJointSetHingeParam( FJoint[i], dParamSuspensionCFM,
          FFRONTSUSPENSION_CFM );
          dJointSetHingeParam( FJoint[i], dParamLoStop, -FWHEEL_WOBBLE );
          dJointSetHingeParam( FJoint[i], dParamHiStop, +FWHEEL_WOBBLE );
          dJointSetHingeParam( FJoint[i],
          dParamLoStop, -FWheelsFrontLoHiStop );
          dJointSetHingeParam( FJoint[i],
          dParamHiStop, FWheelsFrontLoHiStop );
        end else
        if ( i = 2 ) or ( i = 3 ) then begin
          dJointSetHingeParam( FJoint[i], dParamSuspensionERP,
          FBACKSUSPENSION_ERP );
          dJointSetHingeParam( FJoint[i], dParamSuspensionCFM,
          FBACKSUSPENSION_CFM );
          dJointSetHingeParam( FJoint[i], dParamLoStop, -FWHEEL_WOBBLE );
          dJointSetHingeParam( FJoint[i], dParamHiStop, +FWHEEL_WOBBLE );
          dJointSetHingeParam( FJoint[i],
          dParamLoStop, -FWheelsBackLoHiStop );
          dJointSetHingeParam( FJoint[i],
          dParamHiStop, FWheelsBackLoHiStop );
        end;
      end;
      FManager.FObjsList.Add( self );
      FActived := True;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetFrontWheelLimit( const val : TdReal );
begin
  if ( FFrontWheelLimit <> val ) then
  begin
    FFrontWheelLimit := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetFrontWheelTurnDegre( const val : TdReal );
begin
  if ( FFrontWheelTurnDegre <> val ) then
  begin
    FFrontWheelTurnDegre := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetFrontWheelBackUp( const val : TdReal );
begin
  if ( FFrontWheelBackUp <> val ) then
  begin
    FFrontWheelBackUp := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetFrontBackWheelRunForce( const val : TdReal );
begin
  if ( FFrontBackWheelRunForce <> val ) then
  begin
    FFrontBackWheelRunForce := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetFrontBackWheelAltForce( const val : TdReal );
begin
  if ( FFrontBackWheelAltForce <> val ) then
  begin
    FFrontBackWheelAltForce := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetIgnoreSelfModel( const val : boolean );
begin
  if ( FIgnoreSelfModel <> val ) then
  begin
    FIgnoreSelfModel := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetFrictionForce( const val : TdReal );
begin
  if ( FFrictionForce <> val ) then
  begin
    FFrictionForce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetRollFriction( const val : boolean );
begin
  if ( FFakeRollForce <> val ) then
  begin
    FFakeRollForce := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetAutoDisable( const val : boolean );
begin
  if ( FAutoDisable <> val ) then
  begin
    FAutoDisable := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetManager( val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetSpeed( val : TdReal );
begin
  if ( FSpeed <> val ) then
  begin
    FSpeed := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetSteer( val : TdReal );
begin
  if ( FSteer <> val ) then
  begin
    FSteer := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetMotorRunning ( const val : boolean );
begin
  if ( FMotorRunning <> val ) then
  begin
    FMotorRunning := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetZOOM( const val : TdReal );
begin
  if ( FZOOM <> val ) then
  begin
    FZOOM := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetZOOM_CUBED( const val : TdReal );
begin
  if ( FZOOM_CUBED <> val ) then
  begin
    FZOOM_CUBED := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetRADIUS( const val: TdReal );
begin
  if ( FRADIUS <> val ) then
  begin
    FRADIUS := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetLENGTH( const val : TdReal );
begin
  if ( FLENGTH <> val ) then
  begin
    FLENGTH := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetWIDTH( const val : TdReal );
begin
  if ( FWIDTH <> val ) then
  begin
    FWIDTH := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetFRONTSUSPENSION_ERP( const val: TdReal );
begin
  if ( FFRONTSUSPENSION_ERP <> val ) then
  begin
    FFRONTSUSPENSION_ERP := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetFRONTSUSPENSION_CFM( const val: TdReal );
begin
  if ( FFRONTSUSPENSION_CFM <> val ) then
  begin
    FFRONTSUSPENSION_CFM := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetBACKSUSPENSION_ERP( const val: TdReal );
begin
  if ( FBACKSUSPENSION_ERP <> val ) then
  begin
    FBACKSUSPENSION_ERP := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetBACKSUSPENSION_CFM( const val: TdReal );
begin
  if ( FBACKSUSPENSION_CFM <> val ) then
  begin
    FBACKSUSPENSION_CFM := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetWHEEL_WOBBLE( const val: TdReal );
begin
  if ( FWHEEL_WOBBLE <> val ) then
  begin
    FWHEEL_WOBBLE := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetHEIGHT( const val : TdReal );
begin
  if ( FHEIGHT <> val ) then
  begin
    FHEIGHT := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetWHEEL_OFFSET( const val: TdReal );
begin
  if ( FWHEEL_OFFSET <> val ) then
  begin
    FWHEEL_OFFSET := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetCMASS(const val: TdReal);
var
  m : TdMass;
begin
  if ( FCMASSReal <> val ) then
  begin
    dMassSetZero( m );
    FCMASSReal := val;
    dMassAdd( FMass1, m );
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetWMASS(const val: TdReal);
var
  m : TdMass;
begin
  if ( FWMASSReal <> val ) then
  begin
    dMassSetZero( m );
    FWMASSReal := val;
    dMassAdd( FMass2, m );
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.BuildList( var rci : TRenderContextInfo );
begin
  if ( csDesigning in ComponentState ) or ( VisibleAtRunTime ) then
    SizableDummyWireframeBuildList( rci, FCubeSizable, True, EdgeColor.Color );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetACCEL( const val : TdReal );
begin
  if FACCEL <> val then
  begin
    FACCEL := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetTURN_SPEED( const val : TdReal );
begin
  if FTURN_SPEED<> val then
  begin
    FTURN_SPEED := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetModeTraction( const val : TOXCarTraction );
begin
  if FModeTraction<> val then
  begin
    FModeTraction := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetWheelsParamFMax( const val : TdReal );
begin
  if FWheelsParamFMax<> val then
  begin
    FWheelsParamFMax := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetWheelsFrontLoHiStop( const val : TdReal );
begin
  if FWheelsFrontLoHiStop<> val then
  begin
    FWheelsFrontLoHiStop := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetWheelsBackLoHiStop( const val : TdReal );
begin
  if FWheelsBackLoHiStop<> val then
  begin
    FWheelsBackLoHiStop := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetModes : TOXModes;
var
  OXMode : TOXModes;
begin
  OXMode := [];
  if ( FSurface.mode and dContactSlip2 ) <> 0 then
    OXMode := OXMode + [mdSlip2];
  if ( FSurface.mode and dContactSlip1 ) <> 0 then
    OXMode := OXMode + [mdSlip1];
  if ( FSurface.mode and dContactSoftCFM ) <> 0 then
    OXMode := OXMode + [mdSoftCFM];
  if ( FSurface.mode and dContactSoftERP ) <> 0 then
    OXMode := OXMode + [mdSoftERP];
  if ( FSurface.mode and dContactBounce ) <> 0 then
    OXMode := OXMode + [mdBounce];
  if ( FSurface.mode and dContactFDir1 ) <> 0 then
    OXMode := OXMode + [mdFDir1];
  if ( FSurface.mode and dContactMu2 ) <> 0 then
    OXMode := OXMode + [mdMu2];
  result := OXMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetMu : TdReal;
begin
  result := FSurface.mu;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetMu2 : TdReal;
begin
  result := FSurface.mu2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetSlip1 : TdReal;
begin
  result := FSurface.slip1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetSlip2 : TdReal;
begin
  result := FSurface.slip2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetBounce : TdReal;
begin
  result := FSurface.bounce;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetBounce_Vel : TdReal;
begin
  result := FSurface.bounce_vel;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetSoft_erp : TdReal;
begin
  result := FSurface.soft_erp;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetSoft_cfm : TdReal;
begin
  result := FSurface.soft_cfm;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetMotion1 : TdReal;
begin
  result := FSurface.motion1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetMotion2 : TdReal;
begin
  result := FSurface.motion2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetModes( const val : TOXModes );
begin
  if ( FModes <> val ) then
  begin
    FModes := val;
    FMdMode := 0;
    if mdSlip2 in FModes then
      FMdMode := FMdMode or dContactSlip2;
    if mdSlip1 in FModes then
      FMdMode := FMdMode or dContactSlip1;
    if mdSoftCFM in FModes then
      FMdMode := FMdMode or dContactSoftCFM;
    if mdSoftERP in FModes then
      FMdMode := FMdMode or dContactSoftERP;
    if mdBounce in FModes then
      FMdMode := FMdMode or dContactBounce;
    if mdFDir1 in FModes then
      FMdMode := FMdMode or dContactFDir1;
    if mdMu2 in FModes then
      FMdMode := FMdMode or dContactMu2;
    FSurface.mode := FMdMode;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetMu( const val : TdReal );
begin
  if ( FSurface.mu <> val ) then
  begin
    FSurface.mu := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetMu2( const val : TdReal );
begin
  if ( FSurface.mu2 <> val ) then
  begin
    FSurface.mu2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetSlip1( const val : TdReal );
begin
  if ( FSurface.slip1 <> val ) then
  begin
    FSurface.slip1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetSlip2( const val : TdReal );
begin
  if ( FSurface.slip2 <> val ) then
  begin
    FSurface.slip2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetBounce( const val : TdReal );
begin
  if ( FSurface.bounce <> val ) then
  begin
    FSurface.bounce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetBounce_vel( const val : TdReal );
begin
  if ( FSurface.bounce_vel <> val ) then
  begin
    FSurface.bounce_vel := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetSoft_erp( const val : TdReal );
begin
  if ( FSurface.soft_erp <> val ) then
  begin
    FSurface.soft_erp := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetSoft_cfm( const val : TdReal );
begin
  if ( FSurface.soft_cfm <> val ) then
  begin
    FSurface.soft_cfm := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetMotion1( const val : TdReal );
begin
  if ( FSurface.motion1 <> val ) then
  begin
    FSurface.motion1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetMotion2( const val : TdReal );
begin
  if ( FSurface.motion2 <> val ) then
  begin
    FSurface.motion2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetContactNum( const val : integer );
begin
  if ( FContactNum <> val ) then
  begin
    FContactNum := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetApprox0( const val : boolean );
begin
  if ( FApprox0 <> val ) then
  begin
    FApprox0 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetApprox1_1( const val : boolean );
begin
  if ( FApprox1_1 <> val ) then
  begin
    FApprox1_1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetApprox1_2( const val : boolean );
begin
  if ( FApprox1_2 <> val ) then
  begin
    FApprox1_2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetApprox1( const val : boolean );
begin
  if ( FApprox1 <> val ) then
  begin
    FApprox1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetJoint( Index: Integer ) : TdJointID;
begin
  if ( Index >= 0 ) and ( Index <= 3 ) then
    Result := FJoint[ Index ]
  else
    raise Exception.Create
    ( 'assign the Manager or init your GLOXDynCar before use the Joint.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetBody( Index: Integer ) : PdxBody;
begin
  if ( Index >= 0 ) and ( Index <= 4 ) then
    Result := FBody[ Index ]
  else
    raise Exception.Create
    ( 'assign the Manager or init your GLOXDynCar before use the Body.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetGeom( Index: Integer ) : PdxGeom;
begin
  if ( Index >= 0 ) and ( Index <= 3 ) then
    Result := FWheel[ Index ]
  else
    raise Exception.Create
    ( 'assign the Manager or init your GLOXDynCar before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetDummyFrame : TGLSizableDummyCube;
begin
  if assigned( FDummyFrame ) then
    result := FDummyFrame
  else
    raise Exception.Create
    ( 'assign the Manager or init your GLOXDynCar before use the Frame.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetWheelLF : TGLSizableDummyCube;
begin
  if assigned( FWheelLF ) then
    result := FWheelLF
  else
    raise Exception.Create
    ( 'assign the Manager or init your GLOXDynCar before use the WheelLF.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetModeTraction : TOXCarTraction;
begin
  result := FModeTraction;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetWheelLB : TGLSizableDummyCube;
begin
  if assigned( FWheelLB ) then
    result := FWheelLB
  else
    raise Exception.Create
    ( 'assign the Manager or init your GLOXDynCar before use the WheelLB.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetWheelRF : TGLSizableDummyCube;
begin
  if assigned( FWheelRF ) then
    result := FWheelRF
  else
    raise Exception.Create
    ( 'assign the Manager or init your GLOXDynCar before use the WheelRF.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetWheelRB : TGLSizableDummyCube;
begin
  if assigned( FWheelRB ) then
    result := FWheelRB
  else
    raise Exception.Create
    ( 'assign the Manager or init your GLOXDynCar before use the WheelRB.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
function TGLOXDynCar.GetDensity : TdReal;
begin
  result := FDensity;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.SetDensity( const val : TdReal );
begin
  if ( FDensity <> val ) then
  begin
    FDensity := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.AutoDisabling;
var
  Disable : boolean;
  lvel : array[0..4] of PdVector3;
  avel : array[0..4] of PdVector3;
  lspeed : array[0..4] of TdReal;
  aspeed : array[0..4] of TdReal;
  i: integer;
begin
  for i:= 0 to 4 do
  if Assigned( FBody[i] ) then
  begin
    if ( dBodyIsEnabled( FBody[i] ) = 1 ) then
    begin
      Disable := True;
      lvel[i] := dBodyGetLinearVel( FBody[i] );
      lspeed[i] := lvel[i][0] * lvel[i][0] + lvel[i][1] * lvel[i][1] +
      lvel[i][2] * lvel[i][2];
      if ( lspeed[i] > FDisableThreshold ) then
        Disable := false;
      avel[i] := dBodyGetAngularVel( FBody[i] );
      aspeed[i] := avel[i][0] * avel[i][0] + avel[i][1] * avel[i][1] +
      avel[i][2] * avel[i][2];
      if ( aspeed[i] > FDisableThreshold ) then
        Disable := false;
      if ( Disable ) then
        FWb_stepsdis := FWb_stepsdis + 1
      else
        FWb_stepsdis := 0;
      if ( FWb_stepsdis > FDisableSteps ) then
       begin
        dBodySetForce(FBody[i],0,0,0);
        dBodySetLinearVel(FBody[i],0,0,0);
        dBodySetAngularVel(FBody[i],0,0,0);
        dBodyDisable( FBody[i] );
      end;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.FakeRollingFriction;
var
  lvel : array[0..4] of PdVector3;
  SVel : array[0..4] of TdReal;
  i: integer;
begin
  FCoeff := 0;
  for i:= 0 to 4 do
  if Assigned( FBody[i] ) then
  begin
    lvel[i] := dBodyGetAngularVel( FBody[i] );
    SVel[i] := lvel[i][0] * lvel[i][0] + lvel[i][1] * lvel[i][1] + lvel[i][2] * lvel[i][2];
    if SVel[i] > 0 then
    begin
      FCoeff := 1 - FFrictionForce / 1;
      dBodySetAngularVel( FBody[i], lvel[i][0] * FCoeff, lvel[i][1] * FCoeff, lvel[i][2] * FCoeff );
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.DoMotor( delta : double );
begin
  dJointSetHingeParam( joint[0], dParamLoStop, -FFrontWheelLimit );
  dJointSetHingeParam( FJoint[0], dParamVel, FFrontWheelTurnDegre
  * ( ( FSteer * 45.0 * PI ) / 180 ) - dJointGetHingeAngle1( FJoint[0] ) );
  dJointSetHingeParam( joint[0],dParamFudgeFactor,FFrontWheelBackUp );
  dJointSetHingeParam( joint[1], dParamHiStop, FFrontWheelLimit );
  dJointSetHingeParam( FJoint[1], dParamVel, FFrontWheelTurnDegre
  * ( ( FSteer * 45.0 * PI ) / 180 ) - dJointGetHingeAngle1( FJoint[1] ) );
  dJointSetHingeParam( joint[1],dParamFudgeFactor,FFrontWheelBackUp );
  if FMotorRunning then
  begin
    case FModeTraction of
  mdFront: begin
             dJointSetHingeParam( FJoint[0], dParamVel2, -FSpeed );
             dJointSetHingeParam( FJoint[0], dParamFMax2,
             FFrontBackWheelRunForce * FZOOM_CUBED );
             dJointSetHingeParam( FJoint[1], dParamVel2, -FSpeed );
             dJointSetHingeParam( FJoint[1], dParamFMax2,
             FFrontBackWheelRunForce * FZOOM_CUBED );
          end;
  mdBack: begin
            dJointSetHingeParam( FJoint[2], dParamVel2, -FSpeed );
            dJointSetHingeParam( FJoint[2], dParamFMax2,
            FFrontBackWheelRunForce * FZOOM_CUBED );
            dJointSetHingeParam( FJoint[3], dParamVel2, -FSpeed );
            dJointSetHingeParam( FJoint[3], dParamFMax2,
            FFrontBackWheelRunForce * FZOOM_CUBED );
          end;
  mdBoth: begin
            dJointSetHingeParam( FJoint[0], dParamVel2, -FSpeed );
            dJointSetHingeParam( FJoint[0], dParamFMax2,
            FFrontBackWheelRunForce * FZOOM_CUBED );
            dJointSetHingeParam( FJoint[1], dParamVel2, -FSpeed );
            dJointSetHingeParam( FJoint[1], dParamFMax2,
            FFrontBackWheelRunForce * FZOOM_CUBED );
            dJointSetHingeParam( FJoint[2], dParamVel2, -FSpeed );
            dJointSetHingeParam( FJoint[2], dParamFMax2,
            FFrontBackWheelRunForce * FZOOM_CUBED );
            dJointSetHingeParam( FJoint[3], dParamVel2, -FSpeed );
            dJointSetHingeParam( FJoint[3], dParamFMax2,
            FFrontBackWheelRunForce * FZOOM_CUBED );
          end;
        end;
  dJointCorrectHinge( FJoint[2] );
  dJointCorrectHinge( FJoint[3] );
  end else
  if not FMotorRunning then
  begin
    case FModeTraction of
  mdFront: begin
             dJointSetHingeParam( FJoint[0], dParamVel2, 0.0 );
             dJointSetHingeParam( FJoint[0], dParamFMax2,
             FFrontBackWheelAltForce );
             dJointSetHingeParam( FJoint[1], dParamVel2, 0.0 );
             dJointSetHingeParam( FJoint[1], dParamFMax2,
             FFrontBackWheelAltForce );
          end;
  mdBack: begin
            dJointSetHingeParam( FJoint[2], dParamVel2, 0.0 );
            dJointSetHingeParam( FJoint[2], dParamFMax2,
            FFrontBackWheelAltForce );
            dJointSetHingeParam( FJoint[3], dParamVel2, 0.0 );
            dJointSetHingeParam( FJoint[3], dParamFMax2,
            FFrontBackWheelAltForce );
          end;
  mdBoth: begin
            dJointSetHingeParam( FJoint[0], dParamVel2, 0.0 );
            dJointSetHingeParam( FJoint[0], dParamFMax2,
            FFrontBackWheelAltForce );
            dJointSetHingeParam( FJoint[1], dParamVel2, 0.0 );
            dJointSetHingeParam( FJoint[1], dParamFMax2,
            FFrontBackWheelAltForce );
            dJointSetHingeParam( FJoint[2], dParamVel2, 0.0 );
            dJointSetHingeParam( FJoint[2], dParamFMax2,
            FFrontBackWheelAltForce );
            dJointSetHingeParam( FJoint[3], dParamVel2, 0.0 );
            dJointSetHingeParam( FJoint[3], dParamFMax2,
            FFrontBackWheelAltForce );
          end;
        end;
  dJointCorrectHinge( FJoint[2] );
  dJointCorrectHinge( FJoint[3] );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.DoProgress( const progressTime : TProgressTimes );
var
  i : integer;
begin
  inherited;
  if assigned( FManager ) and ( FActived ) and
  ( dBodyIsEnabled( FBody[0] ) = 1 ) then
  begin
    for i := 0 to 4 do
      PositionSceneObject( TGLBaseSceneObject
      ( PdxGeom( FFrame[i] ).data ), FFrame[i] );
    if FAutoDisable then
      AutoDisabling;
    if FFakeRollForce then
      FakeRollingFriction;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
procedure TGLOXDynCar.FreeOde;
var
  i : integer;
begin
  if Assigned( FFrame[0] ) then
  begin
    for i := 0 to 3 do
      dJointDestroy( FJoint[i] );
    for i := 0 to 4 do
      dBodyDestroy( FBody[i] );
    for i := 0 to 4 do
      dGeomDestroy( FFrame[i] );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXDynCar last change by Dave Gravel
{******************************************************************************}
destructor TGLOXDynCar.Destroy;
begin
  FActived := False;
  DeleteChildren;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  FWPosA.Free;
  FWPosB.Free;
  FWPosC.Free;
  FWPosD.Free;
  inherited Destroy;
end;*)
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
constructor TGLOXRagdoll.Create( AOwner : TComponent );
begin
  inherited create( AOwner );
  FObjectsSurfaceMode := mdoNoneSurface;
  FActived := False;
  FConstraint1Lo := -1.5; FConstraint1Hi := 0.1;
  FConstraint2Lo := -0.75; FConstraint2Hi := 0.1;
  FConstraint3Lo := -1.5; FConstraint3Hi := 0.1;
  FConstraint4Lo := -0.75; FConstraint4Hi := 0.1;
  FConstraint5Lo := 0.1; FConstraint5Hi := 1.1;
  FConstraint6Lo := 0.1; FConstraint6Hi := 1.1;
  FConstraint7Lo := -0.3; FConstraint7Hi := 0.3;
  FConstraint8Lo := -0.3; FConstraint8Hi := 0.3;
  FConstraint9Lo := -0.2; FConstraint9Hi := 1.5;
  FConstraint10Lo := -1.5; FConstraint10Hi := 0.2;
  FConstraint11Lo := -0.75; FConstraint11Hi := 1.5;
  FConstraint12Lo := 0.1; FConstraint12Hi := 1.5;
  FConstraint13Lo := -0.75; FConstraint13Hi := 1.5;
  FConstraint14Lo := -1.5; FConstraint14Hi := -0.1;
  FConstraint15Lo := -1.5; FConstraint15Hi := -0.1;
  FConstraint16Lo := 0.1; FConstraint16Hi := 1.5;
  FConstraint17Lo := -0.3; FConstraint17Hi := 0.3;
  FConstraint18Lo := -0.3; FConstraint18Hi := 0.3;
  FConstraint19Lo := -0.75; FConstraint19Hi := 0.75;
  FragHeight := 3;
  FContactNum := 24;
  FDisableThreshold := 0.001;
  FDisableSteps := 15;
  FWb_stepsdis := 0;
  FCoeff := 0;
  FMassReal := 1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
begin
  result := FObjectsSurfaceMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetObjectsSurfaceMode
( const val : TOXObjectsSurfaceMode );
begin
  if FObjectsSurfaceMode<> val then
  begin
    FObjectsSurfaceMode := val;
      case FObjectsSurfaceMode of
  mdoSoftSurface: begin
                    SetModes([mdSoftCFM,mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.5; soft_cfm := 0.01; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoHardSurface: begin
                    SetModes([mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.8; soft_cfm := 0; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoBoundSurface: begin
                     SetModes([mdSoftCFM,mdSoftERP,mdBounce]);
                     with FSurface do begin
                       mode := FMdMode; mu := 5; mu2 := 0; bounce := 0.5;
                       bounce_vel := 0.1;
                       soft_erp := 0.5; soft_cfm := 0.001; motion1 := 0;
                       motion2 := 0;
                       slip1 := 0; slip2 := 0;
                     end;
                     FContactNum := 8;
                   end;
  mdoDefaultSurface: begin
                       SetModes([mdSoftCFM,mdSoftERP]);
                       with FSurface do begin
                         mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                         bounce_vel := 0;
                         soft_erp := 0.8; soft_cfm := 0.001; motion1 := 0;
                         motion2 := 0;
                         slip1 := 0; slip2 := 0;
                       end;
                       FContactNum := 8;
                    end;
  mdoNoneSurface: begin

                    end;
                  end;    
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetSize: TdReal;
begin
  result := FragHeight;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetSize( const val : TdReal );
begin
  if ( FragHeight <> val ) then
  begin
    FragHeight := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.InitODE;
var
  R : TdMatrix3;
  m : TdMass;
  i : integer;
  vPos : TVector;
begin
  if ( assigned( FManager ) ) and ( not Assigned( FGeom[0] ) ) then
  begin
      R[0] := 1; R[1] := 0; R[2] := 1; R[3] := 0; R[4] := 2; R[5] := 0;
      R[6] := 0; R[7] := 0; R[8] := 1; R[9] := 0; R[10] := 0; R[11] := 0;
      vPos[0]:= Position.X; vPos[1]:= Position.Y; vPos[2]:= Position.Z;
      Position.X := 0; Position.Y := 0; Position.Z := 0;
      FuHead := ( FragHeight / 8 ); FthighsL := ( 0.75 * FuHead );
      FthighsW := ( 0.5 * FuHead ); FthighsH := ( 2 * FuHead );
      FlegsL := ( 0.6 * FuHead ); FlegsW := ( 0.5 * FuHead );
      FlegsH := ( 1.75 * FuHead ); FfootL := ( FuHead );
      FfootW := ( 0.5 * FuHead ); FfootH := ( 0.5 * FuHead );
      FpelvisL := ( 0.75 * FuHead ); FpelvisW := ( 0.5 * FuHead * 1.75 );
      FpelvisH := ( 0.75 * FuHead ); Ftors1L := ( 0.6 * FuHead );
      Ftors1W := ( 0.5 * FuHead * 2 ); Ftors1H := ( 1.75 * FuHead / 2 );
      Ftors2L := ( 0.6 * FuHead ); Ftors2W := ( 0.5 * FuHead * 2 );
      Ftors2H := ( 1.75 * FuHead / 2 ); FupArmL := ( 0.5 * FuHead );
      FupArmW := ( 0.3 * FuHead ); FupArmH := ( 1.25 * FuHead );
      FforArmL := ( 0.4 * FuHead ); FforArmW := ( 0.25 * FuHead );
      FforArmH := ( 1.25 * FuHead ); FhandL := ( 0.3 * FuHead );
      FhandW := ( 0.2 * FuHead ); FhandH := ( 0.66 * FuHead );
      FheadL := ( FuHead ); FheadW := ( 0.66 * FuHead );
      FheadH := ( FuHead ); Fj0x := ( 0.75 * FlegsW );
      Fj0y := ( 0 ); Fj0z := ( FfootH + FlegsH );
      Fj2x := ( -0.75 * FlegsW ); Fj2y := ( 0 );
      Fj2z := ( FfootH + FlegsH ); Fj1x := ( 0.75 * FfootH );
      Fj1y := ( 0 ); Fj1z := ( FfootH );
      Fj3x := ( -0.75 * FfootH ); Fj3y := ( 0 );
      Fj3z := ( FfootH ); Fj4x := ( 0.75 * FthighsW );
      Fj4y := ( 0 ); Fj4z := ( FfootH + FlegsH + FthighsH );
      Fj5x := ( -0.75 * FthighsW ); Fj5y := ( 0 );
      Fj5z := ( FfootH + FlegsH + FthighsH ); Fj6x := ( 0 );
      Fj6y := ( 0 ); Fj6z := ( FfootH + FlegsH + FthighsH + FpelvisH * 0.5 );
      Fj7x := ( 0 ); Fj7y := ( 0 );
      Fj7z := ( FfootH + FlegsH + FthighsH + FpelvisH * 0.66 + Ftors1H );
      Fj8x := ( 0.75 * FuHead ); Fj8y := ( 0 );
      Fj8z := ( FfootH + FlegsH + FthighsH + 2 * FupArmH );
      Fj9x := ( -0.75 * FuHead ); Fj9y := ( 0 );
      Fj9z := ( FfootH + FlegsH + FthighsH + 2 * FupArmH );
      Fj10x := ( 0.75 * FuHead ); Fj10y := ( 0 );
      Fj10z := ( FfootH + FlegsH + FthighsH + FupArmH );
      Fj11x := ( -0.75 * FuHead ); Fj11y := ( 0 );
      Fj11z := ( FfootH + FlegsH + FthighsH + FupArmH );
      Fj12x := ( 0.75 * FuHead ); Fj12y := ( 0 );
      Fj12z := ( FfootH + FlegsH + FthighsH );
      for i := 0 to 2 -1 do
      begin
        Fbody[0+i*3] := dBodyCreate( FManager.World );
        dBodySetPosition( Fbody[0+i*3],
                          0.75 * FthighsW -i * 1.5 * FthighsW + vPos[0],
                          vPos[1],
                          4.25 * FuHead -0.5 * FthighsH + vPos[2] );
        dBodySetRotation( Fbody[0+i*3], R );
        dMassSetBox( m, 1, FthighsL, FthighsW, FthighsH );
        dMassAdjust( m, FMassReal );
        dBodySetMass( Fbody[0+i*3], @m );
        FGeom[0+i*3]:= dCreateBox( FManager.Space,
        FthighsL, FthighsW, FthighsH );
        FBoneBox[0+i*3] :=
        TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
        FGeom[0+i*3].data := FBoneBox[0+i*3];
        dGeomSetBody( FGeom[0+i*3], Fbody[0+i*3] );
        PositionSceneObject( FBoneBox[0+i*3], FGeom[0+i*3] );
        with FBoneBox[0+i*3] do
        begin
          CubeWidth := FthighsL;
          CubeHeight := FthighsW;
          CubeDepth := FthighsH;
        end;
      Fbody[1+i*3] := dBodyCreate( FManager.World );
      dBodySetPosition( Fbody[1+i*3],
                        0.75 * FlegsW -i * 1.5 * FlegsW + vPos[0],
                        vPos[1],
                        2.25 * FuHead -0.5 * FlegsH + vPos[2] );
      dBodySetRotation( Fbody[1+i*3], R );
      dMassSetBox( m, 1, FlegsL, FlegsW, FlegsH );
      dMassAdjust( m, FMassReal );
      dBodySetMass( Fbody[1+i*3], @m );
      FGeom[1+i*3] := dCreateBox( FManager.Space, FlegsL, FlegsW, FlegsH );
      FBoneBox[1+i*3] :=
      TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
      FGeom[1+i*3].data := FBoneBox[1+i*3];
      dGeomSetBody( FGeom[1+i*3], Fbody[1+i*3] );
      PositionSceneObject( FBoneBox[1+i*3], FGeom[1+i*3] );
      with FBoneBox[1+i*3] do
      begin
        CubeWidth := FlegsL;
        CubeHeight := FlegsW;
        CubeDepth := FlegsH;
      end;
      Fbody[2+i*3] := dBodyCreate( FManager.World );
      dBodySetPosition( Fbody[2+i*3],
                        0.75 * FfootW -i * 1.5 * FfootW + vPos[0],
                        -0.5 * FfootW + vPos[1],
                        FfootH -0.5 * FfootH + vPos[2] );
      dBodySetRotation( Fbody[2+i*3], R );
      dMassSetBox( m, 1, FfootL, FfootW, FfootH );
      dMassAdjust( m, FMassReal );
      dBodySetMass( Fbody[2+i*3], @m );
      FGeom[2+i*3]:= dCreateBox( FManager.Space, FfootL, FfootW, FfootH );
      FBoneBox[2+i*3] :=
      TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
      FGeom[2+i*3].data := FBoneBox[2+i*3];
      dGeomSetBody( FGeom[2+i*3], Fbody[2+i*3] );
      PositionSceneObject( FBoneBox[2+i*3], FGeom[2+i*3] );
      with FBoneBox[2+i*3] do
      begin
        CubeWidth := FfootL;
        CubeHeight := FfootW;
        CubeDepth := FfootH;
       end;
      end;
      Fbody[6] := dBodyCreate( FManager.World );
      dBodySetPosition( Fbody[6],
                        vPos[0],
                        vPos[1],
                        4.25 * FuHead + vPos[2] );
      dBodySetRotation( Fbody[6], R );
      dMassSetBox( m, 1, FpelvisL, FpelvisW, FpelvisH );
      dMassAdjust( m, FMassReal );
      dBodySetMass( Fbody[6], @m );
      FGeom[6]:= dCreateBox( FManager.Space, FpelvisL, FpelvisW ,FpelvisH );
      FBoneBox[6] := TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
      FGeom[6].data := FBoneBox[6];
      dGeomSetBody( FGeom[6], Fbody[6] );
      PositionSceneObject( FBoneBox[6], FGeom[6] );
      with FBoneBox[6] do
      begin
       CubeWidth := FpelvisL;
       CubeHeight := FpelvisW;
       CubeDepth := FpelvisH;
      end;
      Fbody[7] := dBodyCreate( FManager.World );
      dBodySetPosition( Fbody[7],
                        vPos[0],
                        vPos[1],
                        4.75 * FuHead + 0.5 * Ftors1H + vPos[2] );
      dBodySetRotation( Fbody[7], R );
      dMassSetBox( m, 1, Ftors1L, Ftors1W ,Ftors1H );
      dMassAdjust( m, FMassReal );
      dBodySetMass( Fbody[7], @m );
      FGeom[7]:= dCreateBox( FManager.Space, Ftors1L, Ftors1W, Ftors1H );
      FBoneBox[7] := TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
      FGeom[7].data := FBoneBox[7];
      dGeomSetBody( FGeom[7], Fbody[7] );
      PositionSceneObject( FBoneBox[7], FGeom[7] );
      with FBoneBox[7] do
      begin
        CubeWidth := Ftors1L;
        CubeHeight := Ftors1W;
        CubeDepth := Ftors1H;
      end;
     Fbody[8] := dBodyCreate( FManager.World );
     dBodySetPosition( Fbody[8],
                       vPos[0],
                       vPos[1],
                       4.75 *
                       FuHead + 0.5 * Ftors2H + 2.0 * FuHead / 2 + vPos[2] );
     dBodySetRotation( Fbody[8], R );
     dMassSetBox( m, 1,Ftors2L, Ftors2W ,Ftors2H );
     dMassAdjust( m, FMassReal );
     dBodySetMass( Fbody[8], @m );
     FGeom[8]:= dCreateBox( FManager.Space, Ftors2L, Ftors2W, Ftors2H );
     FBoneBox[8] := TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
     FGeom[8].data := FBoneBox[8];
     dGeomSetBody( FGeom[8], FBody[8] );
     PositionSceneObject( FBoneBox[8], FGeom[8] );
     with FBoneBox[8] do
     begin
       CubeWidth := Ftors2L;
       CubeHeight := Ftors2W;
       CubeDepth := Ftors2H;
     end;
     FBody[9] := dBodyCreate( FManager.World );
     dBodySetPosition( FBody[9],
                       Fj4x + vPos[0],
                       Fj4y + vPos[1],
                       Fj4z + vPos[2] );
     dBodySetRotation( FBody[9], R );
     dMassSetBox( m, 1, FuHead * 0.1, FuHead * 0.1, FuHead * 0.1 );
     dMassAdjust( m, FMassReal );
     dBodySetMass( FBody[9], @m );
     FGeom[9]:= dCreateBox( FManager.Space,
     FuHead * 0.1, FuHead * 0.1, FuHead * 0.1 );
     FBoneBox[9] := TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
     FGeom[9].data := FBoneBox[9];
     dGeomSetBody( FGeom[9], FBody[9] );
     PositionSceneObject( FBoneBox[9], FGeom[9] );
     with FBoneBox[9] do
     begin
       CubeWidth := FuHead * 0.1;
       CubeHeight := FuHead * 0.1;
       CubeDepth := FuHead * 0.1;
     end;
     FBody[10] := dBodyCreate( FManager.World );
     dBodySetPosition( FBody[10],
                       Fj5x + vPos[0],
                       Fj5y + vPos[1],
                       Fj5z + vPos[2] );
     dBodySetRotation( FBody[10], R );
     dMassSetBox( m, 1, FuHead * 0.1, FuHead * 0.1 ,FuHead * 0.1 );
     dMassAdjust( m, FMassReal );
     dBodySetMass( FBody[10], @m );
     FGeom[10]:= dCreateBox( FManager.Space,
     FuHead * 0.1, FuHead * 0.1, FuHead * 0.1 );
     FBoneBox[10] := TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
     FGeom[10].data := FBoneBox[10];
     dGeomSetBody( FGeom[10], FBody[10] );
     PositionSceneObject( FBoneBox[10], FGeom[10] );
     with FBoneBox[10] do
     begin
       CubeWidth := FuHead * 0.1;
       CubeHeight := FuHead * 0.1;
       CubeDepth := FuHead * 0.1;
     end;
     FBody[11] := dBodyCreate( FManager.World );
     dBodySetPosition( FBody[11],
                       0.75 * FuHead + vPos[0],
                       vPos[1],
                       6.75 * FuHead -0.5 * FupArmH + vPos[2] );
     dBodySetRotation( FBody[11], R );
     dMassSetBox( m, 1, FupArmL, FupArmW, FupArmH );
     dMassAdjust( m, FMassReal );
     dBodySetMass( FBody[11], @m );
     FGeom[11]:= dCreateBox( FManager.Space, FupArmL, FupArmW, FupArmH );
     FBoneBox[11] := TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
     FGeom[11].data := FBoneBox[11];
     dGeomSetBody( FGeom[11], FBody[11] );
     PositionSceneObject( FBoneBox[11], FGeom[11] );
     with FBoneBox[11] do
     begin
       CubeWidth := FupArmL;
       CubeHeight := FupArmW;
       CubeDepth := FupArmH;
     end;
     FBody[12] := dBodyCreate( FManager.World );
     dBodySetPosition( FBody[12],
                       -0.75 * FuHead + vPos[0],
                       vPos[1],
                       6.75 * FuHead -0.5 * FupArmH + vPos[2] );
     dBodySetRotation( FBody[12], R );
     dMassSetBox( m, 1, FupArmL, FupArmW, FupArmH );
     dMassAdjust( m, FMassReal );
     dBodySetMass( FBody[12], @m );
     FGeom[12]:= dCreateBox( FManager.Space, FupArmL, FupArmW, FupArmH );
     FBoneBox[12] := TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
     FGeom[12].data := FBoneBox[12];
     dGeomSetBody( FGeom[12], FBody[12] );
     PositionSceneObject( FBoneBox[12], FGeom[12] );
     with FBoneBox[12] do
     begin
       CubeWidth := FupArmL;
       CubeHeight := FupArmW;
       CubeDepth := FupArmH;
     end;
     FBody[13] := dBodyCreate( FManager.World );
     dBodySetPosition( FBody[13],
                       Fj8x + vPos[0],
                       Fj8y + vPos[1],
                       Fj8z + vPos[2] );
     dBodySetRotation( FBody[13], R );
     dMassSetBox( m, 1, FuHead * 0.1, FuHead * 0.1, FuHead * 0.1 );
     dMassAdjust( m, FMassReal );
     dBodySetMass( FBody[13], @m );
     FGeom[13]:= dCreateBox( FManager.Space,
     FuHead * 0.1, FuHead * 0.1, FuHead * 0.1 );
     FBoneBox[13] := TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
     FGeom[13].data := FBoneBox[13];
     dGeomSetBody( FGeom[13], FBody[13] );
     PositionSceneObject( FBoneBox[13], FGeom[13] );
     with FBoneBox[13] do
     begin
       CubeWidth := FuHead * 0.1;
       CubeHeight := FuHead * 0.1;
       CubeDepth := FuHead * 0.1;
     end;
     FBody[14] := dBodyCreate( FManager.World );
     dBodySetPosition( FBody[14],
                       Fj9x + vPos[0],
                       Fj9y + vPos[1],
                       Fj9z + vPos[2] );
     dBodySetRotation( FBody[14], R );
     dMassSetBox( m, 1, FuHead * 0.1, FuHead * 0.1, FuHead * 0.1 );
     dMassAdjust( m, FMassReal );
     dBodySetMass( FBody[14], @m );
     FGeom[14]:= dCreateBox( FManager.Space,
     FuHead * 0.1, FuHead * 0.1, FuHead * 0.1 );
     FBoneBox[14] := TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
     FGeom[14].data := FBoneBox[14];
     dGeomSetBody( FGeom[14], FBody[14] );
     PositionSceneObject( FBoneBox[14], FGeom[14] );
     with FBoneBox[14] do
     begin
       CubeWidth := FuHead * 0.1;
       CubeHeight := FuHead * 0.1;
       CubeDepth := FuHead * 0.1;
     end;
     FBody[15] := dBodyCreate( FManager.World );
     dBodySetPosition( FBody[15],
                       Fj10x + vPos[0],
                       vPos[1],
                       Fj10z -FforArmH / 2 + vPos[2] );
     dBodySetRotation( FBody[15], R );
     dMassSetBox( m, 1, FforArmL, FforArmW, FforArmH );
     dMassAdjust( m, FMassReal );
     dBodySetMass( FBody[15], @m );
     FGeom[15]:= dCreateBox( FManager.Space, FforArmL, FforArmW, FforArmH );
     FBoneBox[15] := TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
     FGeom[15].data := FBoneBox[15];
     dGeomSetBody( FGeom[15], FBody[15] );
     PositionSceneObject( FBoneBox[15], FGeom[15] );
     with FBoneBox[15] do
     begin
       CubeWidth := FforArmL;
       CubeHeight := FforArmW;
       CubeDepth := FforArmH;
     end;
     FBody[16] := dBodyCreate( FManager.World );
     dBodySetPosition( FBody[16],
                       Fj11x + vPos[0],
                       vPos[1],
                       Fj11z -FforArmH / 2 + vPos[2] );
     dBodySetRotation( FBody[16], R );
     dMassSetBox( m, 1, FforArmL, FforArmW, FforArmH );
     dMassAdjust( m, FMassReal );
     dBodySetMass( FBody[16], @m );
     FGeom[16]:= dCreateBox( FManager.Space, FforArmL, FforArmW, FforArmH );
     FBoneBox[16] := TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
     FGeom[16].data := FBoneBox[16];
     dGeomSetBody( FGeom[16], FBody[16] );
     PositionSceneObject( FBoneBox[16], FGeom[16] );
     with FBoneBox[16] do
     begin
       CubeWidth := FforArmL;
       CubeHeight := FforArmW;
       CubeDepth := FforArmH;
     end;
     FBody[17] := dBodyCreate( FManager.World );
     dBodySetPosition( FBody[17],
                       Fj12x + vPos[0],
                       vPos[1],
                       Fj5z -0.5 * FhandH + vPos[2] );
     dBodySetRotation( FBody[17], R );
     dMassSetBox( m, 1, FhandL, FhandW, FhandH );
     dMassAdjust( m, FMassReal );
     dBodySetMass( FBody[17], @m );
     FGeom[17]:= dCreateBox( FManager.Space, FhandL, FhandW, FhandH );
     FBoneBox[17] := TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
     FGeom[17].data := FBoneBox[17];
     dGeomSetBody( FGeom[17], FBody[17] );
     PositionSceneObject( FBoneBox[17], FGeom[17] );
     with FBoneBox[17] do
     begin
       CubeWidth := FhandL;
       CubeHeight := FhandW;
       CubeDepth := FhandH;
     end;
     FBody[18] := dBodyCreate( FManager.World );
     dBodySetPosition( FBody[18],
                       -Fj12x + vPos[0],
                       vPos[1],
                       Fj5z -0.5 * FhandH + vPos[2] );
     dBodySetRotation( FBody[18], R );
     dMassSetBox( m, 1, FhandL, FhandW, FhandH );
     dMassAdjust( m, FMassReal );
     dBodySetMass( FBody[18], @m );
     FGeom[18]:= dCreateBox( FManager.Space, FhandL, FhandW, FhandH );
     FBoneBox[18] := TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
     FGeom[18].data := FBoneBox[18];
     dGeomSetBody( FGeom[18], FBody[18] );
     PositionSceneObject( FBoneBox[18], FGeom[18] );
     with FBoneBox[18] do
     begin
       CubeWidth := FhandL;
       CubeHeight := FhandW;
       CubeDepth := FhandH;
     end;
     FBody[19] := dBodyCreate( FManager.World );
     dBodySetPosition( FBody[19],
                       vPos[0],
                       0.1 * FuHead + vPos[1],
                       7.5 * FuHead + vPos[2] );
     dBodySetRotation( FBody[19], R );
     dMassSetBox( m, 1, FheadL, FheadW, FheadH );
     dMassAdjust( m, FMassReal );
     dBodySetMass( FBody[19], @m );
     FGeom[19]:= dCreateBox( FManager.Space, FheadL, FheadW, FheadH );
     FBoneBox[19] := TGLSizableDummyCube(AddNewChild( TGLSizableDummyCube ));
     FGeom[19].data := FBoneBox[19];
     dGeomSetBody( FGeom[19], FBody[19] );
     PositionSceneObject( FBoneBox[19], FGeom[19] );
     with FBoneBox[19] do
     begin
       CubeWidth := FheadL;
       CubeHeight := FheadW;
       CubeDepth := FheadH;
     end;
     FJoint[0] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[0], FBody[0], FBody[1] );
     dJointSetHingeAnchor( FJoint[0],
                           Fj0x + vPos[0],
                           Fj0y + vPos[1],
                           Fj0z + vPos[2] );
     dJointSetHingeAxis( FJoint[0], 1, 0, 0 );
     FJoint[1] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[1], FBody[1], FBody[2] );
     dJointSetHingeAnchor( FJoint[1],
                           Fj1x + vPos[0],
                           Fj1y + vPos[1],
                           Fj1z + vPos[2] );
     dJointSetHingeAxis( FJoint[1], 1, 0, 0 );
     FJoint[2] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[2], FBody[3], FBody[4] );
     dJointSetHingeAnchor( FJoint[2],
                           Fj2x + vPos[0],
                           Fj2y + vPos[1],
                           Fj2z + vPos[2] );
     dJointSetHingeAxis( FJoint[2], 1, 0, 0 );
     FJoint[3] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[3], FBody[4], FBody[5] );
     dJointSetHingeAnchor( FJoint[3],
                           Fj3x + vPos[0],
                           Fj3y + vPos[1],
                           Fj3z + vPos[2] );
     dJointSetHingeAxis( FJoint[3], 1, 0, 0 );
     FJoint[4] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[4], FBody[6], FBody[9] );
     dJointSetHingeAnchor( FJoint[4],
                           Fj4x + vPos[0],
                           Fj4y + vPos[1],
                           Fj4z + vPos[2] );
     dJointSetHingeAxis( FJoint[4], 1, 0, 0 );
     FJoint[5] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[5], FBody[6], FBody[10] );
     dJointSetHingeAnchor( FJoint[5],
                           Fj5x + vPos[0],
                           Fj5y + vPos[1],
                           Fj5z + vPos[2] );
     dJointSetHingeAxis( FJoint[5], 1, 0, 0 );
     FJoint[6] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[6], FBody[6], FBody[7] );
     dJointSetHingeAnchor( FJoint[6],
                           Fj6x + vPos[0],
                           Fj6y + vPos[1],
                           Fj6z + vPos[2] );
     dJointSetHingeAxis( FJoint[6], 1, 0, 0 );
     FJoint[7] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[7], FBody[7], FBody[8] );
     dJointSetHingeAnchor( FJoint[7],
                           Fj7x + vPos[0],
                           Fj7y + vPos[1],
                           Fj7z + vPos[2] );
     dJointSetHingeAxis( FJoint[7], 1, 0, 0 );
     FJoint[8] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[8], FBody[9], FBody[0] );
     dJointSetHingeAnchor( FJoint[8],
                           Fj4x + vPos[0],
                           Fj4y + vPos[1],
                           Fj4z + vPos[2] );
     dJointSetHingeAxis( FJoint[8], 0, 1, 0 );
     FJoint[9] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[9], FBody[10], FBody[3] );
     dJointSetHingeAnchor( FJoint[9],
                           Fj5x + vPos[0],
                           Fj5y + vPos[1],
                           Fj5z + vPos[2] );
     dJointSetHingeAxis( FJoint[9], 0, 1, 0 );
     FJoint[10] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[10], FBody[8], FBody[13] );
     dJointSetHingeAnchor( FJoint[10],
                           Fj8x + vPos[0],
                           Fj8y + vPos[1],
                           Fj8z + vPos[2] );
     dJointSetHingeAxis( FJoint[10], 1, 0, 0 );
     FJoint[11] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[11], FBody[13], FBody[11] );
     dJointSetHingeAnchor( FJoint[11],
                           Fj8x + vPos[0],
                           Fj8y + vPos[1],
                           Fj8z + vPos[2] );
     dJointSetHingeAxis( FJoint[11], 0, 1, 0 );
     FJoint[12] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[12], FBody[8], FBody[14] );
     dJointSetHingeAnchor( FJoint[12],
                           Fj9x + vPos[0],
                           Fj9y + vPos[1],
                           Fj9z + vPos[2] );
     dJointSetHingeAxis( FJoint[12], 1, 0, 0 );
     FJoint[13] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[13], FBody[14], FBody[12] );
     dJointSetHingeAnchor( FJoint[13],
                           Fj9x + vPos[0],
                           Fj9y + vPos[1],
                           Fj9z + vPos[2] );
     dJointSetHingeAxis( FJoint[13], 0, 1, 0 );
     FJoint[14] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[14], FBody[11], FBody[15] );
     dJointSetHingeAnchor( FJoint[14],
                           Fj10x + vPos[0],
                           Fj10y + vPos[1],
                           Fj10z + vPos[2] );
     dJointSetHingeAxis( FJoint[14], 0, 1, 0 );
     FJoint[15] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[15], FBody[12], FBody[16] );
     dJointSetHingeAnchor( FJoint[15],
                           Fj11x + vPos[0],
                           Fj11y + vPos[1],
                           Fj11z + vPos[2] );
     dJointSetHingeAxis( FJoint[15], 0, 1, 0 );
     FJoint[16] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[16], FBody[15], FBody[17] );
     dJointSetHingeAnchor( FJoint[16],
                           Fj12x + vPos[0],
                           Fj12y + vPos[1],
                           Fj12z + vPos[2] );
     dJointSetHingeAxis( FJoint[16], 0, 1, 0 );
     FJoint[17] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[17], FBody[16], FBody[18] );
     dJointSetHingeAnchor( FJoint[17],
                           -Fj12x + vPos[0],
                           Fj12y + vPos[1],
                           Fj12z + vPos[2] );
     dJointSetHingeAxis( FJoint[17], 0, 1, 0 );
     FJoint[18] := dJointCreateHinge( FManager.World, 0 );
     dJointAttach( FJoint[18], FBody[8], FBody[19] );
     dJointSetHingeAnchor( FJoint[18],
                           vPos[0],
                           vPos[1],
                           7 * FuHead + vPos[2] );
     dJointSetHingeAxis( FJoint[18], 1, 1, 0 );
     dBodySetLinearVel( FBody[19], 0, 0, -1 );
     FManager.FObjsList.Add( self );
     FActived:= True;
   end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetManager( const val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.PlayerUpdate;
begin
                                  dJointSetHingeParam( Fjoint[0],
  dParamLoStop, FConstraint1Lo ); dJointSetHingeParam( Fjoint[0],
  dParamHiStop, FConstraint1Hi ); dJointSetHingeParam( Fjoint[1],
  dParamLoStop, FConstraint2Lo ); dJointSetHingeParam( Fjoint[1],
  dParamHiStop, FConstraint2Hi ); dJointSetHingeParam( Fjoint[2],
  dParamLoStop, FConstraint3Lo ); dJointSetHingeParam( Fjoint[2],
  dParamHiStop, FConstraint3Hi ); dJointSetHingeParam( Fjoint[3],
  dParamLoStop, FConstraint4Lo ); dJointSetHingeParam( Fjoint[3],
  dParamHiStop, FConstraint4Hi ); dJointSetHingeParam( Fjoint[4],
  dParamLoStop, FConstraint5Lo ); dJointSetHingeParam( Fjoint[4],
  dParamHiStop, FConstraint5Hi ); dJointSetHingeParam( Fjoint[5],
  dParamLoStop, FConstraint6Lo ); dJointSetHingeParam( Fjoint[5],
  dParamHiStop, FConstraint6Hi ); dJointSetHingeParam( Fjoint[6],
  dParamLoStop, FConstraint7Lo ); dJointSetHingeParam( Fjoint[6],
  dParamHiStop, FConstraint7Hi ); dJointSetHingeParam( Fjoint[7],
  dParamLoStop, FConstraint8Lo ); dJointSetHingeParam( Fjoint[7],
  dParamHiStop, FConstraint8Hi ); dJointSetHingeParam( Fjoint[8],
  dParamLoStop, FConstraint9Lo ); dJointSetHingeParam( Fjoint[8],
  dParamHiStop, FConstraint9Hi ); dJointSetHingeParam( Fjoint[9],
  dParamLoStop, FConstraint10Lo ); dJointSetHingeParam( Fjoint[9],
  dParamHiStop, FConstraint10Hi ); dJointSetHingeParam( Fjoint[10],
  dParamLoStop, FConstraint11Lo ); dJointSetHingeParam( Fjoint[10],
  dParamHiStop, FConstraint11Hi ); dJointSetHingeParam( Fjoint[11],
  dParamLoStop, FConstraint12Lo ); dJointSetHingeParam( Fjoint[11],
  dParamHiStop, FConstraint12Hi ); dJointSetHingeParam( Fjoint[12],
  dParamLoStop, FConstraint13Lo ); dJointSetHingeParam( Fjoint[12],
  dParamHiStop, FConstraint13Hi ); dJointSetHingeParam( Fjoint[13],
  dParamLoStop, FConstraint14Lo ); dJointSetHingeParam( Fjoint[13],
  dParamHiStop, FConstraint14Hi ); dJointSetHingeParam( Fjoint[14],
  dParamLoStop, FConstraint15Lo ); dJointSetHingeParam( Fjoint[14],
  dParamHiStop, FConstraint15Hi ); dJointSetHingeParam( Fjoint[15],
  dParamLoStop, FConstraint16Lo ); dJointSetHingeParam( Fjoint[15],
  dParamHiStop, FConstraint16Hi ); dJointSetHingeParam( Fjoint[16],
  dParamLoStop, FConstraint17Lo ); dJointSetHingeParam( Fjoint[16],
  dParamHiStop, FConstraint17Hi ); dJointSetHingeParam( Fjoint[17],
  dParamLoStop, FConstraint18Lo ); dJointSetHingeParam( Fjoint[17],
  dParamHiStop, FConstraint18Hi ); dJointSetHingeParam( Fjoint[18],
  dParamLoStop, FConstraint19Lo ); dJointSetHingeParam( Fjoint[18],
  dParamHiStop, FConstraint19Hi );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetDensity : TdReal;
begin
  result := FDensity;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetJoint( Index: Integer ) : TdJointID;
begin
  if ( Index >= 0 ) and ( Index <= 19-1 ) then
    Result := FJoint[ Index ]
  else
    raise Exception.Create
    ( 'assign the Manager or init your GLOXRagdoll before use the Joint.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetBody( Index: Integer ) : PdxBody;
begin
  if ( Index >= 0 ) and ( Index <= NUM-1 ) then
    Result := FBody[ Index ]
  else
    raise Exception.Create
    ( 'assign the Manager or init your GLOXRagdoll before use the Body.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetGeom( Index: Integer ) : PdxGeom;
begin
  if ( Index >= 0 ) and ( Index <= NUM-1 ) then
    Result := FGeom[ Index ]
  else
    raise Exception.Create
    ( 'assign the Manager or init your GLOXRagdoll before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetBoneCube( Index: Integer ) : TGLSizableDummyCube;
begin
  if ( Index >= 0 ) and ( Index <= NUM-1 ) then
    Result := FBoneBox[ Index ]
  else
    raise Exception.Create
    ( 'assign the Manager or init your GLOXRagdoll before use the BoneCube.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetAutoDisable( const val : boolean );
begin
  if ( FAutoDisable <> val ) then
  begin
    FAutoDisable := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetMass( const val : TdReal );
var
  m : TdMass;
begin
  if ( FMassReal <> val ) then
  begin
    dMassSetZero( m );
    FMassReal := val;
    dMassAdjust( m, FMassReal );
    dMassAdd( FMass, m );
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetDisableThreshold( const val : TdReal );
begin
  if ( FDisableThreshold <> val ) then
  begin
    FDisableThreshold := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint1Lo( const val : TdReal );
begin
  if ( FConstraint1Lo <> val ) then
  begin
    FConstraint1Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint1Hi( const val : TdReal );
begin
  if ( FConstraint1Hi <> val ) then
  begin
    FConstraint1Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint2Lo( const val : TdReal );
begin
  if ( FConstraint2Lo <> val ) then
  begin
    FConstraint2Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint2Hi( const val : TdReal );
begin
  if ( FConstraint2Hi <> val ) then
  begin
    FConstraint2Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint3Lo( const val : TdReal );
begin
  if ( FConstraint3Lo <> val ) then
  begin
    FConstraint3Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint3Hi( const val : TdReal );
begin
  if ( FConstraint3Hi <> val ) then
  begin
    FConstraint3Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint4Lo( const val : TdReal );
begin
  if ( FConstraint4Lo <> val ) then
  begin
    FConstraint4Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint4Hi( const val : TdReal );
begin
  if ( FConstraint4Hi <> val ) then
  begin
    FConstraint4Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint5Lo( const val : TdReal );
begin
  if ( FConstraint5Lo <> val ) then
  begin
    FConstraint5Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint5Hi( const val : TdReal );
begin
  if ( FConstraint5Hi <> val ) then
  begin
    FConstraint5Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint6Lo( const val : TdReal );
begin
  if ( FConstraint6Lo <> val ) then
  begin
    FConstraint6Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint6Hi( const val : TdReal );
begin
  if ( FConstraint6Hi <> val ) then
  begin
    FConstraint6Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint7Lo( const val : TdReal );
begin
  if ( FConstraint7Lo <> val ) then
  begin
    FConstraint7Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint7Hi( const val : TdReal );
begin
  if ( FConstraint7Hi <> val ) then
  begin
    FConstraint7Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint8Lo( const val : TdReal );
begin
  if ( FConstraint8Lo <> val ) then
  begin
    FConstraint8Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint8Hi( const val : TdReal );
begin
  if ( FConstraint8Hi <> val ) then
  begin
    FConstraint8Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint9Lo( const val : TdReal );
begin
  if ( FConstraint9Lo <> val ) then
  begin
    FConstraint9Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint9Hi( const val : TdReal );
begin
  if ( FConstraint9Hi <> val ) then
  begin
    FConstraint9Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint10Lo( const val : TdReal );
begin
  if ( FConstraint10Lo <> val ) then
  begin
    FConstraint10Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint10Hi( const val : TdReal );
begin
  if ( FConstraint10Hi <> val ) then
  begin
    FConstraint10Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint11Lo( const val : TdReal );
begin
  if ( FConstraint11Lo <> val ) then
  begin
    FConstraint11Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint11Hi( const val : TdReal );
begin
  if ( FConstraint11Hi <> val ) then
  begin
    FConstraint11Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint12Lo( const val : TdReal );
begin
  if ( FConstraint12Lo <> val ) then
  begin
    FConstraint12Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint12Hi( const val : TdReal );
begin
  if ( FConstraint12Hi <> val ) then
  begin
    FConstraint12Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint13Lo( const val : TdReal );
begin
  if ( FConstraint13Lo <> val ) then
  begin
    FConstraint13Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint13Hi( const val : TdReal );
begin
  if ( FConstraint13Hi <> val ) then
  begin
    FConstraint13Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint14Lo( const val : TdReal );
begin
  if ( FConstraint14Lo <> val ) then
  begin
    FConstraint14Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint14Hi( const val : TdReal );
begin
  if ( FConstraint14Hi <> val ) then
  begin
    FConstraint14Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint15Lo( const val : TdReal );
begin
  if ( FConstraint15Lo <> val ) then
  begin
    FConstraint15Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint15Hi( const val : TdReal );
begin
  if ( FConstraint15Hi <> val ) then
  begin
    FConstraint15Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint16Lo( const val : TdReal );
begin
  if ( FConstraint16Lo <> val ) then
  begin
    FConstraint16Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint16Hi( const val : TdReal );
begin
  if ( FConstraint16Hi <> val ) then
  begin
    FConstraint16Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint17Lo( const val : TdReal );
begin
  if ( FConstraint17Lo <> val ) then
  begin
    FConstraint17Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint17Hi( const val : TdReal );
begin
  if ( FConstraint17Hi <> val ) then
  begin
    FConstraint17Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint18Lo( const val : TdReal );
begin
  if ( FConstraint18Lo <> val ) then
  begin
    FConstraint18Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint18Hi( const val : TdReal );
begin
  if ( FConstraint18Hi <> val ) then
  begin
    FConstraint18Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint19Lo( const val : TdReal );
begin
  if ( FConstraint19Lo <> val ) then
  begin
    FConstraint19Lo := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetConstraint19Hi( const val : TdReal );
begin
  if ( FConstraint19Hi <> val ) then
  begin
    FConstraint19Hi := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetDisableSteps( const val : TdReal );
begin
  if ( FDisableSteps <> val ) then
  begin
    FDisableSteps := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.DoProgress( const progressTime : TProgressTimes );
var
  j: integer;
begin
  inherited;
  if assigned( FManager ) and ( FActived ) then
  begin
    for j := 0 to NUM -1 do
    begin
      if ( dBodyIsEnabled( FBody[j] ) = 1 ) then
      begin
        PlayerUpdate;
        PositionSceneObject( TGLBaseSceneObject( PdxGeom( FGeom[j] ).data ),
        FGeom[j] );
      end;
    end;
    if FAutoDisable then
      AutoDisabling;
    if FFakeRollForce then
      FakeRollingFriction;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.AutoDisabling;
var
  Disable : boolean;
  i: integer;
  lvel : array[0..NUM-1] of PdVector3;
  avel : array[0..NUM-1] of PdVector3;
  lspeed : array[0..NUM-1] of TdReal;
  aspeed : array[0..NUM-1] of TdReal;
begin
  for i:= 0 to NUM-1 do
  if Assigned( FBody[i] ) then
  begin
    if ( dBodyIsEnabled( FBody[i] ) = 1 ) then
    begin
      Disable := True;
      lvel[i] := dBodyGetLinearVel( FBody[i] );
      lspeed[i] := lvel[i][0] * lvel[i][0] + lvel[i][1] * lvel[i][1] +
      lvel[i][2] * lvel[i][2];
      if ( lspeed[i] > FDisableThreshold ) then
        Disable := false;
      avel[i] := dBodyGetAngularVel( FBody[i] );
      aspeed[i] := avel[i][0] * avel[i][0] + avel[i][1] * avel[i][1] +
      avel[i][2] * avel[i][2];
      if ( aspeed[i] > FDisableThreshold ) then
        Disable := false;
      if ( Disable ) then
        FWb_stepsdis := FWb_stepsdis + 1
      else
        FWb_stepsdis := 0;
      if ( FWb_stepsdis > FDisableSteps ) then
      begin
        dBodySetForce(FBody[i],0,0,0);
        dBodySetLinearVel(FBody[i],0,0,0);
        dBodySetAngularVel(FBody[i],0,0,0);
        dBodyDisable( FBody[i] );
      end;
    end;
  end;
end;     
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.FakeRollingFriction;
var
  lvel : array[0..3] of PdVector3;
  SVel : array[0..3] of TdReal;
  i: integer;
begin
  FCoeff := 0;
  for i:= 0 to 3 do
  if Assigned( FBody[i] ) then
  begin
    lvel[i] := dBodyGetAngularVel( FBody[i] );
    SVel[i] := lvel[i][0] * lvel[i][0] + lvel[i][1] * lvel[i][1] +
    lvel[i][2] * lvel[i][2];
    if SVel[i] > 0 then
    begin
      FCoeff := 1 - FFrictionForce / 1;
      dBodySetAngularVel( FBody[i], lvel[i][0] * FCoeff, lvel[i][1] *
      FCoeff, lvel[i][2] * FCoeff );
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetFrictionForce( const val : TdReal );
begin
  if ( FFrictionForce <> val ) then
  begin
    FFrictionForce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetRollFriction( const val : boolean );
begin
  if ( FFakeRollForce <> val ) then
  begin
    FFakeRollForce := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetDensity( const val : TdReal );
begin
  if ( FDensity <> val ) then
  begin
    FDensity := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetModes : TOXModes;
var
  OXMode : TOXModes;
begin
  OXMode := [];
  if ( FSurface.mode and dContactSlip2 ) <> 0 then
    OXMode := OXMode + [mdSlip2];
  if ( FSurface.mode and dContactSlip1 ) <> 0 then
    OXMode := OXMode + [mdSlip1];
  if ( FSurface.mode and dContactSoftCFM ) <> 0 then
    OXMode := OXMode + [mdSoftCFM];
  if ( FSurface.mode and dContactSoftERP ) <> 0 then
    OXMode := OXMode + [mdSoftERP];
  if ( FSurface.mode and dContactBounce ) <> 0 then
    OXMode := OXMode + [mdBounce];
  if ( FSurface.mode and dContactFDir1 ) <> 0 then
    OXMode := OXMode + [mdFDir1];
  if ( FSurface.mode and dContactMu2 ) <> 0 then
    OXMode := OXMode + [mdMu2];
  result := OXMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetMu : TdReal;
begin
  result := FSurface.mu;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetMu2 : TdReal;
begin
  result := FSurface.mu2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetSlip1 : TdReal;
begin
  result := FSurface.slip1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetSlip2 : TdReal;
begin
  result := FSurface.slip2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetBounce : TdReal;
begin
  result := FSurface.bounce;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetBounce_Vel : TdReal;
begin
  result := FSurface.bounce_vel;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetSoft_erp : TdReal;
begin
  result := FSurface.soft_erp;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetSoft_cfm : TdReal;
begin
  result := FSurface.soft_cfm;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetMotion1 : TdReal;
begin
  result := FSurface.motion1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
function TGLOXRagdoll.GetMotion2 : TdReal;
begin
  result := FSurface.motion2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetModes( const val : TOXModes );
begin
  if ( FModes <> val ) then
  begin
    FModes := val;
    FMdMode := 0;
    if mdSlip2 in FModes then
      FMdMode := FMdMode or dContactSlip2;
    if mdSlip1 in FModes then
      FMdMode := FMdMode or dContactSlip1;
    if mdSoftCFM in FModes then
      FMdMode := FMdMode or dContactSoftCFM;
    if mdSoftERP in FModes then
      FMdMode := FMdMode or dContactSoftERP;
    if mdBounce in FModes then
      FMdMode := FMdMode or dContactBounce;
    if mdFDir1 in FModes then
      FMdMode := FMdMode or dContactFDir1;
    if mdMu2 in FModes then
      FMdMode := FMdMode or dContactMu2;
    FSurface.mode := FMdMode;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetMu( const val : TdReal );
begin
  if ( FSurface.mu <> val ) then
  begin
    FSurface.mu := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetMu2( const val : TdReal );
begin
  if ( FSurface.mu2 <> val ) then
  begin
    FSurface.mu2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetSlip1( const val : TdReal );
begin
  if ( FSurface.slip1 <> val ) then
  begin
    FSurface.slip1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetSlip2( const val : TdReal );
begin
  if ( FSurface.slip2 <> val ) then
  begin
    FSurface.slip2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetBounce( const val : TdReal );
begin
  if ( FSurface.bounce <> val ) then
  begin
    FSurface.bounce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetBounce_vel( const val : TdReal );
begin
  if ( FSurface.bounce_vel <> val ) then
  begin
    FSurface.bounce_vel := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetSoft_erp( const val : TdReal );
begin
  if ( FSurface.soft_erp <> val ) then
  begin
    FSurface.soft_erp := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetSoft_cfm( const val : TdReal );
begin
  if ( FSurface.soft_cfm <> val ) then
  begin
    FSurface.soft_cfm := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetMotion1( const val : TdReal );
begin
  if ( FSurface.motion1 <> val ) then
  begin
    FSurface.motion1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetMotion2( const val : TdReal );
begin
  if ( FSurface.motion2 <> val ) then
  begin
    FSurface.motion2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetContactNum( const val : integer );
begin
  if ( FContactNum <> val ) then
  begin
    FContactNum := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetApprox0( const val : boolean );
begin
  if ( FApprox0 <> val ) then
  begin
    FApprox0 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetApprox1_1( const val : boolean );
begin
  if ( FApprox1_1 <> val ) then
  begin
    FApprox1_1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetApprox1_2( const val : boolean );
begin
  if ( FApprox1_2 <> val ) then
  begin
    FApprox1_2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.SetApprox1( const val : boolean );
begin
  if ( FApprox1 <> val ) then
  begin
    FApprox1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
destructor TGLOXRagdoll.Destroy;
begin
  FActived := False;
  DeleteChildren;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  inherited Destroy;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXRagdoll last change by Dave Gravel
{******************************************************************************}
procedure TGLOXRagdoll.FreeOde;
var
  i,h: integer;
begin
  if assigned( FGeom[0] ) then
  begin
    for i := 0 to NUM -1 do
    begin
      dBodyDestroy( Fbody[i] );
      dGeomDestroy( FGeom[i] );
    end;
    for h := 0 to 19 -1 do
      dJointDestroy( FJoint[h] );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
constructor TGLOXAMotor.Create( AOwner : TComponent );
begin
  inherited create( AOwner );
  FObjectsSurfaceMode := mdoNoneSurface;
  FActived := False;
  FContactNum := 8;
  FMassReal := 1.0;
  FDensity := 1;
  FCoeff := 0;
  FIgnoreSelfModel := False;
  FDisableThreshold := 0.001;
  FDisableSteps := 15;
  FWb_stepsdis := 0;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetObjectsSurfaceMode : TOXObjectsSurfaceMode;
begin
  result := FObjectsSurfaceMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetObjectsSurfaceMode
( const val : TOXObjectsSurfaceMode );
begin
  if FObjectsSurfaceMode<> val then
  begin
    FObjectsSurfaceMode := val;
      case FObjectsSurfaceMode of
  mdoSoftSurface: begin
                    SetModes([mdSoftCFM,mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.5; soft_cfm := 0.01; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoHardSurface: begin
                    SetModes([mdSoftERP]);
                    with FSurface do begin
                      mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                      bounce_vel := 0;
                      soft_erp := 0.8; soft_cfm := 0; motion1 := 0;
                      motion2 := 0;
                      slip1 := 0; slip2 := 0;
                    end;
                    FContactNum := 8;
                  end;
  mdoBoundSurface: begin
                     SetModes([mdSoftCFM,mdSoftERP,mdBounce]);
                     with FSurface do begin
                       mode := FMdMode; mu := 5; mu2 := 0; bounce := 0.5;
                       bounce_vel := 0.1;
                       soft_erp := 0.5; soft_cfm := 0.001; motion1 := 0;
                       motion2 := 0;
                       slip1 := 0; slip2 := 0;
                     end;
                     FContactNum := 8;
                   end;
  mdoDefaultSurface: begin
                       SetModes([mdSoftCFM,mdSoftERP]);
                       with FSurface do begin
                         mode := FMdMode; mu := 5; mu2 := 0; bounce := 0;
                         bounce_vel := 0;
                         soft_erp := 0.8; soft_cfm := 0.001; motion1 := 0;
                         motion2 := 0;
                         slip1 := 0; slip2 := 0;
                       end;
                       FContactNum := 8;
                    end;
  mdoNoneSurface: begin

                    end;
                  end;    
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.InitODE;
begin
  if assigned( FManager ) then
  begin
    if not Assigned( FGeom ) then
    begin
      dMassSetZero( FMass );
      if FMassReal <= 0 then
        FMassReal := 1;
      FGeom := dCreateBox( FManager.FSpace, Scale.X, Scale.Y, Scale.Z );
      FBody := dBodyCreate( FManager.FWorld );
      dGeomSetBody( FGeom, FBody );
      dMassSetBox( FMass, FDensity, Scale.X * FMassReal, Scale.Y *
      FMassReal, Scale.Z * FMassReal );
      dBodySetMass( FBody, @FMass );
      SetBodyPosRot( FBody, AbsoluteMatrix );
      FGeom.data := self;
      FAmotor := dJointCreateAMotor( FManager.FWorld, 0 );
      dJointAttach( FAmotor, FBody, nil );
      dJointSetAMotorMode( FAmotor, dAMotorEuler );
      dJointSetAMotorNumAxes( FAmotor, 3 );
      dJointSetAMotorAxis( FAmotor, 0, 0, 1, 0, 0 );
      dJointSetAMotorAxis( FAmotor, 1, 0, 0, 1, 0 );
      dJointSetAMotorAxis( FAmotor, 2, 0, 0, 0, 1 );
      dJointSetAMotorAngle( FAmotor, 0, 0 );
      dJointSetAMotorAngle( FAmotor, 1, 0 );
      dJointSetAMotorAngle( FAmotor, 2, 0 );
      dJointSetAMotorParam( FAmotor, dParamLoStop, -0 );
      dJointSetAMotorParam( FAmotor, dParamLoStop3, -0 );
      dJointSetAMotorParam( FAmotor, dParamLoStop2, -0 );
      dJointSetAMotorParam( FAmotor, dParamHiStop, 0 );
      dJointSetAMotorParam( FAmotor, dParamHiStop3, 0 );
      dJointSetAMotorParam( FAmotor, dParamHiStop2, 0 );
      dBodySetLinearVel( FBody, 0, 0, -1 );
      FManager.FObjsList.Add( self );
      FActived := True;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.DoProgress( const progressTime : TProgressTimes );
begin
  inherited;
  if assigned( FManager ) and ( FActived ) then
  begin
    if ( dBodyIsEnabled( FBody ) = 1 ) then
    begin
      PositionSceneObject( TGLBaseSceneObject( PdxGeom( FGeom ).data ),FGeom );
      if FAutoDisable then
        AutoDisabling;
      if FFakeRollForce then
        FakeRollingFriction;
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetMass( const val : TdReal );
var
  m : TdMass;
begin
  if ( FMassReal <> val ) then
  begin
    dMassSetZero( m );
    FMassReal := val;
    dMassAdjust( m, FMassReal );
    dMassAdd( FMass, m );
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetManager( const val : TGLOXOdeEngine );
begin
  if ( FManager <> val ) then
  begin
    FManager := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetBody : PdxBody;
begin
  if assigned( FBody ) then
    result := FBody else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXFPSMotor before use the Body.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetGeom : PdxGeom;
begin
  if assigned( FGeom ) then
    result := FGeom else
  raise Exception.Create
  ( 'assign the Manager or init your GLOXFPSMotor before use the Geom.' );
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.FreeOde;
begin
  if assigned( FGeom ) then
  begin
    dJointDestroy( FAmotor );
    dBodyDestroy( FBody );
    dGeomDestroy( FGeom );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.AutoDisabling;
var
  Disable : boolean;
  lvel : PdVector3;
  avel : PdVector3;
  lspeed : TdReal;
  aspeed : TdReal;
begin
  if Assigned( FBody ) and ( dBodyIsEnabled( FBody ) = 1 ) then
  begin
    Disable := True;
    lvel := dBodyGetLinearVel( FBody );
    lspeed := lvel[0] * lvel[0] + lvel[1] * lvel[1] +
    lvel[2] * lvel[2];
    if ( lspeed > FDisableThreshold ) then
      Disable := false;
    avel := dBodyGetAngularVel( FBody );
    aspeed := avel[0] * avel[0] + avel[1] * avel[1] +
    avel[2] * avel[2];
    if ( aspeed > FDisableThreshold ) then
      Disable := false;
    if ( Disable ) then
      FWb_stepsdis := FWb_stepsdis + 1
    else
      FWb_stepsdis := 0;
    if ( FWb_stepsdis > FDisableSteps ) then
    begin
      dBodySetForce(FBody,0,0,0);
      dBodySetLinearVel(FBody,0,0,0);
      dBodySetAngularVel(FBody,0,0,0);
      dBodyDisable( FBody );
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetAutoDisable( const val : boolean );
begin
  if ( FAutoDisable <> val ) then
  begin
    FAutoDisable := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetDisableThreshold( const val : TdReal );
begin
  if ( FDisableThreshold <> val ) then
  begin
    FDisableThreshold := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetDisableSteps( const val : TdReal );
begin
  if ( FDisableSteps <> val ) then
  begin
    FDisableSteps := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.FakeRollingFriction;
var
  lvel : PdVector3;
  SVel : TdReal;
begin
  FCoeff := 0;
  if Assigned( FBody ) then begin
    lvel := dBodyGetAngularVel( FBody );
    SVel := lvel[0] * lvel[0] + lvel[1] * lvel[1] +
    lvel[2] * lvel[2];
    if SVel > 0 then begin
      FCoeff := 1 - FFrictionForce / 1;
      dBodySetAngularVel( FBody, lvel[0] * FCoeff, lvel[1] *
      FCoeff, lvel[2] * FCoeff );
    end;
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetFrictionForce( const val : TdReal );
begin
  if ( FFrictionForce <> val ) then
  begin
    FFrictionForce := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetRollFriction( const val : boolean );
begin
  if ( FFakeRollForce <> val ) then
  begin
    FFakeRollForce := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetModes( const val : TOXModes );
begin
  if ( FModes <> val ) then
  begin
    FModes := val;
    FMdMode := 0;
    if mdSlip2 in FModes then
      FMdMode := FMdMode or dContactSlip2;
    if mdSlip1 in FModes then
      FMdMode := FMdMode or dContactSlip1;
    if mdSoftCFM in FModes then
      FMdMode := FMdMode or dContactSoftCFM;
    if mdSoftERP in FModes then
      FMdMode := FMdMode or dContactSoftERP;
    if mdBounce in FModes then
      FMdMode := FMdMode or dContactBounce;
    if mdFDir1 in FModes then
      FMdMode := FMdMode or dContactFDir1;
    if mdMu2 in FModes then
      FMdMode := FMdMode or dContactMu2;
    FSurface.mode := FMdMode;
    NotifyChange( Self );
    end;
end;
{******************************************************************************}
 // [2005-06-08]: TGLOXFPSMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetMu( const val : TdReal );
begin
  if ( FSurface.mu <> val ) then
  begin
    FSurface.mu := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: TGLOXFPSMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetMu2( const val : TdReal );
begin
  if ( FSurface.mu2 <> val ) then
  begin
    FSurface.mu2 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: TGLOXFPSMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetSlip1( const val : TdReal );
begin
  if ( FSurface.slip1 <> val ) then
  begin
    FSurface.slip1 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: TGLOXFPSMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetSlip2( const val : TdReal );
begin
  if ( FSurface.slip2 <> val ) then
  begin
    FSurface.slip2 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: TGLOXFPSMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetBounce( const val : TdReal );
begin
  if ( FSurface.bounce <> val ) then
  begin
    FSurface.bounce:= val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetBounce_vel( const val : TdReal );
begin
  if ( FSurface.bounce_vel <> val ) then
  begin
    FSurface.bounce_vel := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: TGLOXFPSMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetSoft_erp( const val : TdReal );
begin
  if ( FSurface.soft_erp <> val ) then
  begin
    FSurface.soft_erp := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: TGLOXFPSMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetSoft_cfm( const val : TdReal );
begin
  if ( FSurface.soft_cfm <> val ) then
  begin
    FSurface.soft_cfm := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: TGLOXFPSMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetMotion1( const val : TdReal );
begin
  if ( FSurface.motion1 <> val ) then
  begin
    FSurface.motion1 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetMotion2( const val : TdReal );
begin
  if ( FSurface.motion2 <> val ) then
  begin
    FSurface.motion2 := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetContactNum( const val : integer );
begin
  if ( FContactNum <> val ) then
  begin
    FContactNum:= val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetApprox0( const val : boolean );
begin
  if ( FApprox0 <> val ) then
  begin
    FApprox0 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: TGLOXFPSMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetApprox1_1( const val : boolean );
begin
  if ( FApprox1_1 <> val ) then
  begin
    FApprox1_1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: TGLOXFPSMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetApprox1_2( const val : boolean );
begin
  if ( FApprox1_2 <> val ) then
  begin
    FApprox1_2 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: TGLOXFPSMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetApprox1( const val : boolean );
begin
  if ( FApprox1 <> val ) then
  begin
    FApprox1 := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetIgnoreSelfModel( const val : boolean );
begin
  if ( FIgnoreSelfModel <> val ) then
  begin
    FIgnoreSelfModel := val;
    NotifyChange( Self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetModes : TOXModes;
var
  OXMode : TOXModes;
begin
  OXMode := [];
  if ( FSurface.mode and dContactSlip2 ) <> 0 then
    OXMode := OXMode + [ mdSlip2 ];
  if ( FSurface.mode and dContactSlip1 ) <> 0 then
    OXMode := OXMode + [ mdSlip1 ];
  if ( FSurface.mode and dContactSoftCFM ) <> 0 then
    OXMode := OXMode + [ mdSoftCFM ];
  if ( FSurface.mode and dContactSoftERP) <> 0 then
    OXMode := OXMode + [ mdSoftERP ];
  if ( FSurface.mode and dContactBounce ) <> 0 then
    OXMode := OXMode + [ mdBounce ];
  if ( FSurface.mode and dContactFDir1 ) <> 0 then
    OXMode := OXMode + [ mdFDir1 ];
  if ( FSurface.mode and dContactMu2 ) <> 0 then
    OXMode := OXMode + [ mdMu2 ];
    result := OXMode;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetMu : TdReal;
begin
  result := FSurface.mu;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetMu2 : TdReal;
begin
  result := FSurface.mu2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetSlip1 : TdReal;
begin
  result := FSurface.slip1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetSlip2 : TdReal;
begin
  result := FSurface.slip2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetBounce : TdReal;
begin
  result := FSurface.bounce;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetBounce_Vel : TdReal;
begin
  result := FSurface.bounce_vel;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetSoft_erp : TdReal;
begin
  result := FSurface.soft_erp;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetSoft_cfm : TdReal;
begin
  result := FSurface.soft_cfm;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetMotion1 : TdReal;
begin
  result := FSurface.motion1;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetMotion2 : TdReal;
begin
  result := FSurface.motion2;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
function TGLOXAMotor.GetDensity : TdReal;
begin
  result := FDensity;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
procedure TGLOXAMotor.SetDensity( const val : TdReal );
begin
  if ( FDensity <> val ) then
  begin
    FDensity := val;
    NotifyChange( self );
  end;
end;
{******************************************************************************}
 // [2005-06-08]: GLOXAMotor last change by Dave Gravel
{******************************************************************************}
destructor TGLOXAMotor.Destroy;
begin
  FActived := False;
  DeleteChildren;
  if assigned( FManager ) then
  begin
    FreeOde;
    FManager.FObjsList.Remove( self );
    FManager := nil;
  end;
  inherited Destroy;
end;
{******************************************************************************}
//{$D '[2005-06-08]: oxGLODE v1.0PRE by Dave Gravel under MPL-1.1 license.'}
{******************************************************************************}
initialization
  RegisterClasses( [ TGLSizableDummyCube, TGLOXOdeEngine, TGLOXStaBall, TGLOXStaBox,
                     TGLOXStaCylinder, TGLOXDynBall,
                     TGLOXDynBox, TGLOXDynCylinder,
                     TGLOXZStaTerrain, TGLOXDynMesh{, TGLOXDynCar}, TGLOXStaMesh,
                     TGLOXRagDoll, TGLOXAMotor ] );

end.
