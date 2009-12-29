//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLMisc<p>

   Miscellaneous support routines & classes.<p>

	<b>History : </b><font size=-1><ul>
      <li>10/04/08 - DaStr - Added a Delpi 5 interface bug work-around to
                              TGLUpdateAbleObject, TGLCadenceAbleComponent,
                              TGLCustomCoordinates.NotifyChange
                              (BugTracker ID = 1938988)
      <li>03/04/07 - DaStr - GLS_DELPHI_5_UP renamed to GLS_DELPHI_4_DOWN for
                             FPC compatibility (thanks Burkhard Carstens)
      <li>30/03/07 - DaStr - Added a work-around to the Delphi 5 interface bug
                             in TGLCustomCoordinates.NotifyChange
      <li>28/03/07 - DaStr - Renamed parameters in some methods
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678658)
      <li>14/03/07 - DaStr - Added explicit pointer dereferencing
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
      <li>09/03/07 - DaStr - Removed obsolete FPC IFDEF's
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678642)
      <li>29/01/07 - DaStr - TGLCustomCoordinates.SetVector - Added default value
                             to one of the procedure's parameters
                             Added TGLCustomCoordinates.AsPoint2D property
      <li>14/01/07 - DaStr - Added IGLCoordinatesUpdateAble
                              (abstracted from TGLCoordinatesUpdateAbleComponent)
                             TGLCoordinates.SetVector/SetPoint - fixed assertions
                             and added descriptions for them (BugTrackerID=1588388)
                             TGLCustomCoordinates abstracted
                             TGLCoordinates2 added
                             Added csPoint2D to TGLCoordinatesStyle
      <li>05/09/03 - EG - Some GLScene types and helper functions moved
                          to new GLState and GLUtils units
      <li>10/25/03 - Dave - Added TGLCoordinates.SetVector (TAffineVector)
                            Added TGLCoordinates.SetVector (TVector)
                            Added TGLCoordinates.SetPoint (TAffineVector)
                            Added TGLCoordinates.SetPoint (TVector)
      <li>05/09/03 - EG - TNotifyCollection moved in from GLMultiPolygon
      <li>21/08/03 - EG - Added osRenderNearestFirst
      <li>17/06/03 - EG - New TryStrToFloat, updated StrToFloatDef
      <li>05/06/03 - EG - TDataFile moved out to ApplicationFileIO,
                          added Silhouette classes
      <li>13/03/03 - Dave - Added TGLCoordinates.SetToZero
      <li>23/10/02 - EG - Added ParseFloat
      <li>22/10/02 - EG - Added ParseInteger
      <li>03/07/02 - EG - Added TGLNodes.Normal
      <li>17/03/02 - EG - Added First/Last to TGLNodes
      <li>24/01/02 - EG - Added vUseDefaultSets mechanism
      <li>07/01/02 - EG - TGLNodes.Barycenter fix (thx Bob)
      <li>15/12/01 - EG - Added support for cube maps
      <li>14/09/01 - EG - Addition of vFileStreamClass
      <li>04/09/01 - EG - SetGLCurrentTexture stuff
      <li>18/07/01 - EG - Added TGLVisibilityCulling
      <li>08/07/01 - EG - Changes in TGLNodes based on code from Uwe Raabe
      <li>19/06/01 - EG - Added StrToFloatDef
      <li>16/03/01 - EG - Added Capabilities to TDataFile
      <li>21/02/01 - EG - Now XOpenGL based (multitexture)
      <li>05/02/01 - EG - Faster SetGLMaterialColors
      <li>15/01/01 - EG - Added SizeOfFile
      <li>04/01/00 - EG - Added AsAffineVector to TGLNode
      <li>22/12/00 - EG - Fixed TGLNodes.Vector when there is only one node
      <li>03/11/00 - EG - Added TGLCoordinates.AsAffineVector
      <li>08/10/00 - EG - Added "Style" to TGLCoordinates to detect some misuses
      <li>06/08/00 - EG - TGLCoordinates moved in, added TextureMatrix stuff,
                          added TGLNodes.AddXYArc
      <li>19/07/00 - EG - Improvements to TGLNodes (tessellation, scaling...)
      <li>16/07/00 - EG - Added "Managers" support classes,
                          Added TDataFile
      <li>11/07/00 - EG - Added 'Sender' to MotifyChange
      <li>05/07/00 - EG - Added Begin/EndUpdate to TGLNodes
      <li>23/06/00 - EG - Added Read/WriteCRLFString
      <li>18/06/00 - EG - Added update control to TGLUpdateAbleObject
      <li>09/06/00 - EG - Added TGLCadenceAbleComponent
      <li>07/06/00 - EG - Added RemoveFreeNotification for Delphi 4
      <li>29/05/00 - EG - Added TGLNode/TGLNodes
      <li>26/05/00 - EG - TMeshMode & TVertexMode moved in
      <li>22/03/00 - EG - Added SetGLState/UnSetGLState
      <li>21/03/00 - EG - Added SaveStringToFile/LoadStringFromFile
      <li>18/03/00 - EG - Added GetSqrt255Array
      <li>06/02/00 - EG - Javadocisation, RoundUpToPowerOf2,
                          RoundDownToPowerOf2 and IsPowerOf2 moved in
   </ul></font>

   TODO : separating misc stuff from base classes and OpenGL support

}
unit GLMisc;

// GLMisc      - miscellaneous support routines
// version     - 0.1.0
// last change - 31. January 1999
// for more information see help file

interface

uses
  //VCL
  Classes,

  // GLScene
  VectorGeometry, OpenGL1x, Spline, VectorLists, VectorTypes, GLCrossPlatform,
  PersistentClasses;

{$I GLScene.inc}

type
   TMeshMode = (mmTriangleStrip, mmTriangleFan, mmTriangles,
                mmQuadStrip, mmQuads, mmPolygon);
   TVertexMode = (vmV, vmVN, vmVNC, vmVNCT, vmVNT, vmVT);

const
   cMeshModeToGLEnum : array [Low(TMeshMode)..High(TMeshMode)] of TGLEnum =
                     (GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_TRIANGLES,
                      GL_QUAD_STRIP, GL_QUADS, GL_POLYGON);
   cVertexModeToGLEnum : array [Low(TVertexMode)..High(TVertexMode)] of TGLEnum =
                     (GL_V3F, GL_N3F_V3F, GL_C4F_N3F_V3F, GL_T2F_C4F_N3F_V3F,
                      GL_T2F_N3F_V3F, GL_T2F_V3F);

type

   // TProgressTimes
   //
   TProgressTimes = record
      deltaTime, newTime : Double
   end;

   // TGLObjectsSorting
   //
   {: Determines if objects are sorted, and how.<p>
      Sorting is done level by level (and not for all entities), values are :<ul>
      <li>osInherited : use inherited sorting mode, defaults to osRenderFarthestFirst
      <li>osNone : do not sort objects.
		<li>osRenderFarthestFirst : render objects whose Position is the farthest from
			the camera first.
      <li>osRenderBlendedLast : opaque objects are not sorted and rendered
         first, blended ones are rendered afterwards and depth sorted.
		<li>osRenderNearestFirst : render objects whose Position is the nearest to
			the camera first.
       </ul> }
   TGLObjectsSorting = (osInherited, osNone,
                        osRenderFarthestFirst, osRenderBlendedLast,
                        osRenderNearestFirst);

   // TGLVisibilityCulling
   //
   {: Determines the visibility culling mode.
      Culling is done level by level, allowed values are:<ul>
      <li>vcInherited : use inherited culling value, if selected for the root
         level, defaults to vcNone
      <li>vcNone : no visibility culling is performed
      <li>vcObjectBased : culling is done on a per-object basis, each object may
         or may not be culled base on its own AxisAlignedDimensions,
         culling has no impact on the visibility of its children
      <li>vcHierarchical : culling is performed hierarchically, using hierarchical
         bounding boxes, if a parent is culled, all of its children, whatever their
         culling options are invisible.
      <li><br>Depending on the structure of your scene the most efficient culling
      method will be either vcObjectBased or vcHierarchical. Also note that if
      you use many objects with "static" geometry and have a T&amp;L graphics
      board, it may be faster not to cull at all (ie. leave this to the hardware). }
   TGLVisibilityCulling = (vcInherited, vcNone, vcObjectBased, vcHierarchical); 

   // TGLUpdateAbleObject
   //
   {: An abstract class describing the "update" interface.<p> }
   TGLUpdateAbleObject = class (TGLInterfacedPersistent)
      private
	      { Private Declarations }
         FOwner : TPersistent;
         FUpdating : Integer;
         FOnNotifyChange : TNotifyEvent;

      public
	      { Public Declarations }
         constructor Create(AOwner: TPersistent); virtual;

			procedure NotifyChange(Sender : TObject); virtual;
         function GetOwner : TPersistent; override;

         property Updating : Integer read FUpdating;
         procedure BeginUpdate;
         procedure EndUpdate;

         property Owner : TPersistent read FOwner;
         property OnNotifyChange : TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
	end;

	// TGLCadenceAbleComponent
	//
	{: A base class describing the "cadenceing" interface.<p> }
	TGLCadenceAbleComponent = class (TGLComponent)
		public
	      { Public Declarations }
{$ifdef GLS_DELPHI_4_DOWN}
         procedure RemoveFreeNotification(AComponent: TComponent);
{$endif}
			procedure DoProgress(const progressTime : TProgressTimes); virtual;
	end;

	// TGLUpdateAbleComponent
	//
	{: A base class describing the "update" interface.<p> }
	TGLUpdateAbleComponent = class (TGLCadenceAbleComponent)
		public
	      { Public Declarations }
			procedure NotifyChange(Sender : TObject); virtual;
	end;

   // TGLCoordinatesStyle
   //
   {: Identifie le type de donn�es stock�es au sein d'un TGLCustomCoordinates.<p>
      <ul><li>csPoint2D : a simple 2D point (Z=0, W=0)
      <ul><li>csPoint : un point (W=1)
      <li>csVector : un vecteur (W=0)
      <li>csUnknown : aucune contrainte
      </ul> }
   TGLCoordinatesStyle = (csPoint2D, csPoint, csVector, csUnknown);

	// TGLCustomCoordinates
	//
	{: Stores and homogenous vector.<p>
		This class is basicly a container for a TVector, allowing proper use of
		delphi property editors and editing in the IDE. Vector/Coordinates
		manipulation methods are only minimal.<br>
		Handles dynamic default values to save resource file space.<p> }
	TGLCustomCoordinates = class (TGLUpdateAbleObject)
		private
			{ Private Declarations }
			FCoords : TVector;
         FStyle : TGLCoordinatesStyle; // NOT Persistent
         FPDefaultCoords : PVector;
			procedure SetAsPoint2D(const Value : TVector2f);
			procedure SetAsVector(const value : TVector);
			procedure SetAsAffineVector(const value : TAffineVector);
      function GetAsAffineVector : TAffineVector;
      function GetAsPoint2D : TVector2f;
			procedure SetCoordinate(index : Integer; const aValue : TGLFloat);
      function GetAsString : String;

		protected
			{ Protected Declarations }
         procedure SetDirectVector(const v : TVector);

			procedure DefineProperties(Filer: TFiler); override;
			procedure ReadData(Stream: TStream);
			procedure WriteData(Stream: TStream);

		public
			{ Public Declarations }
         constructor CreateInitialized(aOwner : TPersistent; const aValue : TVector;
                                       const aStyle : TGLCoordinatesStyle = csUnknown);
         destructor Destroy; override;
			procedure Assign(Source: TPersistent); override;
         procedure WriteToFiler(writer : TWriter);
         procedure ReadFromFiler(reader : TReader);

         procedure Initialize(const value : TVector);
			procedure NotifyChange(Sender : TObject); override;

         {: Identifies the coordinates styles.<p>
            The property is NOT persistent, csUnknown by default, and should be
            managed by owner object only (internally).<p>
            It is used by the TGLCustomCoordinates for internal "assertion" checks
            to detect "misuses" or "misunderstandings" of what the homogeneous
            coordinates system implies. }
         property Style : TGLCoordinatesStyle read FStyle write FStyle;

			procedure Translate(const translationVector : TVector); overload;
			procedure Translate(const translationVector : TAffineVector); overload;
			procedure AddScaledVector(const factor : Single; const translationVector : TVector); overload;
			procedure AddScaledVector(const factor : Single; const translationVector : TAffineVector); overload;
         procedure Rotate(const anAxis : TAffineVector; anAngle: Single); overload;
         procedure Rotate(const anAxis : TVector; anAngle: Single); overload;
         procedure Normalize;
         procedure Invert;
         procedure Scale(factor : Single);
         function  VectorLength : TGLFloat;
         function  VectorNorm : TGLFloat;
         function  MaxXYZ : Single;
         function  Equals(const aVector : TVector) : Boolean;

         procedure SetVector(const x, y: Single; z : Single = 0); overload;
         procedure SetVector(const x, y, z, w: Single); overload;
         procedure SetVector(const v : TAffineVector); overload;
         procedure SetVector(const v : TVector); overload;

         procedure SetPoint(const x, y, z : Single); overload;
         procedure SetPoint(const v : TAffineVector); overload;
         procedure SetPoint(const v : TVector); overload;

         procedure SetPoint2D(const x, y: Single); overload;
         procedure SetPoint2D(const v : TAffineVector); overload;
         procedure SetPoint2D(const v : TVector); overload;
         procedure SetPoint2D(const v : TVector2f); overload;

         procedure SetToZero;
         function AsAddress : PGLFloat;

         {: The coordinates viewed as a vector.<p>
            Assigning a value to this property will trigger notification events,
            if you don't want so, use DirectVector instead. }
			property AsVector : TVector read FCoords write SetAsVector;

         {: The coordinates viewed as an affine vector.<p>
            Assigning a value to this property will trigger notification events,
            if you don't want so, use DirectVector instead.<br>
            The W component is automatically adjustes depending on style. }
			property AsAffineVector : TAffineVector read GetAsAffineVector write SetAsAffineVector;

         {: The coordinates viewed as a 2D point.<p>
            Assigning a value to this property will trigger notification events,
            if you don't want so, use DirectVector instead. }
			property AsPoint2D : TVector2f read GetAsPoint2D write SetAsPoint2D;

      property X: TGLFloat index 0 read FCoords[0] write SetCoordinate;
      property Y: TGLFloat index 1 read FCoords[1] write SetCoordinate;
      property Z: TGLFloat index 2 read FCoords[2] write SetCoordinate;
			property W: TGLFloat index 3 read FCoords[3] write SetCoordinate;

         {: The coordinates, in-between brackets, separated by semi-colons. }
         property AsString : String read GetAsString;

         //: Similar to AsVector but does not trigger notification events
         property DirectVector : TVector read FCoords write SetDirectVector;
         property DirectX : TGLFloat read FCoords[0] write FCoords[0];
         property DirectY : TGLFloat read FCoords[1] write FCoords[1];
         property DirectZ : TGLFloat read FCoords[2] write FCoords[2];
         property DirectW : TGLFloat read FCoords[3] write FCoords[3];
  	end;

   {: A TGLCustomCoordinates that publishes X, Y properties. }
    TGLCoordinates2 = class(TGLCustomCoordinates)
    published
      property X stored False;
      property Y stored False;
  	end;

   {: A TGLCustomCoordinates that publishes X, Y, Z properties. }
    TGLCoordinates3 = class(TGLCustomCoordinates)
    published
      property X stored False;
      property Y stored False;
      property Z stored False;
  	end;

   // TGLCoordinates4
   //
   {: A TGLCustomCoordinates that publishes X, Y, Z, W properties. }
	  TGLCoordinates4 = class (TGLCustomCoordinates)
    published
      property X stored False;
      property Y stored False;
      property Z stored False;
      property W stored False;
    end;

   // TGLCoordinates
   //
    TGLCoordinates = TGLCoordinates3;


    //Actually Sender should be TGLCustomCoordinates, but that would require
    //changes in a some other GLScene units and some other projects that use
    //TGLCoordinatesUpdateAbleComponent
    IGLCoordinatesUpdateAble = interface(IInterface)
    ['{ACB98D20-8905-43A7-AFA5-225CF5FA6FF5}']
      procedure CoordinateChanged(Sender: TGLCoordinates);
    end;


   // TGLCoordinatesUpdateAbleComponent
   //
   TGLCoordinatesUpdateAbleComponent = class (TGLUpdateAbleComponent, IGLCoordinatesUpdateAble)
      public
	      { Public Declarations }
         procedure CoordinateChanged(Sender: TGLCoordinates); virtual; abstract;
   end;

	// TGLNode
	//
	TGLNode = class (TCollectionItem)
	   private
	      { Private Declarations }
			FCoords : TVector;
			procedure SetAsVector(const value: TVector);
			procedure SetAsAffineVector(const value : TAffineVector);
         function GetAsAffineVector : TAffineVector;
			procedure SetCoordinate(AIndex: Integer; AValue: TGLFloat);

	   protected
	      { Protected Declarations }
         function StoreCoordinate(AIndex: Integer) : Boolean;

         function GetDisplayName : String; override;

      public
	      { Public Declarations }
	      constructor Create(ACollection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         function AsAddress : PGLFloat;
         {: The coordinates viewed as a vector.<p>
            Assigning a value to this property will trigger notification events,
            if you don't want so, use DirectVector instead. }
			property AsVector : TVector read FCoords write SetAsVector;
         {: The coordinates viewed as an affine vector.<p>
            Assigning a value to this property will trigger notification events,
            if you don't want so, use DirectVector instead.<br>
            The W component is automatically adjustes depending on style. }
			property AsAffineVector : TAffineVector read GetAsAffineVector write SetAsAffineVector;

			property W: TGLFloat index 3 read FCoords[3] write SetCoordinate stored StoreCoordinate;

	   published
	      { Published Declarations }
			property X: TGLFloat index 0 read FCoords[0] write SetCoordinate stored StoreCoordinate;
			property Y: TGLFloat index 1 read FCoords[1] write SetCoordinate stored StoreCoordinate;
			property Z: TGLFloat index 2 read FCoords[2] write SetCoordinate stored StoreCoordinate;
	end;


	// TGLNodes
	//
	TGLNodes = class (TOwnedCollection)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }
         procedure SetItems(index : Integer; const val : TGLNode);
	      function GetItems(index : Integer) : TGLNode;
         procedure Update(Item: TCollectionItem); override;

      public
	      { Public Declarations }
         constructor Create(AOwner : TPersistent; AItemClass: TCollectionItemClass = nil);
         function CreateCopy(AOwner : TPersistent) : TGLNodes;

         function Add : TGLNode;
	      function FindItemID(ID : Integer) : TGLNode;
	      property Items[index : Integer] : TGLNode read GetItems write SetItems; default;
         function First : TGLNode;
         function Last : TGLNode;

         procedure NotifyChange; virtual;
         procedure EndUpdate; override;

         procedure AddNode(const coords : TGLCustomCoordinates); overload;
         procedure AddNode(const X, Y, Z: TGLfloat); overload;
         procedure AddNode(const value : TVector); overload;
         procedure AddNode(const value : TAffineVector); overload;
         procedure AddXYArc(xRadius, yRadius : Single;
                            startAngle, stopAngle : Single;
                            nbSegments : Integer;
                            const center : TAffineVector);

         //: Calculates and returns the barycenter of the nodes
         function Barycenter : TAffineVector;
         {: Computes normal based on the 1st three nodes.<p>
            Returns NullVector if there are less than 3 nodes. }
         function Normal : TAffineVector;
         //: Returns normalized vector Nodes[i+1]-Nodes[i]
         function Vector(i : Integer) : TAffineVector;

         {: Calculates the extents of the nodes (min-max for all coordinates).<p>
            The returned values are also the two corners of the axis-aligned
            bounding box. }
         procedure GetExtents(var min, max : TAffineVector);
         //: Translate all nodes
         procedure Translate(const tv : TAffineVector);
         //: Scale all node coordinates
         procedure Scale(const fv : TAffineVector); overload;
         //: Scale all node coordinates
         procedure Scale(f : Single); overload;
         //: Rotate nodes around Y axis by the given angle (degrees)
         procedure RotateAroundX(angle : Single);
         //: Rotate nodes around Y axis by the given angle (degrees)
         procedure RotateAroundY(angle : Single);
         //: Rotate nodes around Y axis by the given angle (degrees)
         procedure RotateAroundZ(angle : Single);

         procedure RenderTesselatedPolygon(ATextured : Boolean;
                                           ANormal : PAffineVector = nil;
                                           ASplineDivisions : Integer = 1;
                                           AInvertNormals : Boolean = False);

         function CreateNewCubicSpline : TCubicSpline;

   end;

   TGLNodesClass = class of TGLNodes;

   // TNotifyCollection
   //
   TNotifyCollection = class (TOwnedCollection)
      private
	      { Private Declarations }
         FOnNotifyChange : TNotifyEvent;

      protected
	      { Protected Declarations }
         procedure Update(item : TCollectionItem); override;

      public
	      { Public Declarations }
         constructor Create(AOwner : TPersistent; AItemClass : TCollectionItemClass);
         property OnNotifyChange : TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
   end;

procedure RegisterManager(aManager : TComponent);
procedure DeRegisterManager(aManager : TComponent);
function FindManager(classType : TComponentClass; const managerName : String) : TComponent;

var
   // Specifies if TGLCustomCoordinates, TGLColor, etc. should allocate memory for
   // their default values (ie. design-time) or not (run-time)
   vUseDefaultSets : Boolean = False;

//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------
implementation
//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------

uses SysUtils, XOpenGL;

const
  csVectorHelp =   'If you are getting assertions here, consider using the SetPoint procedure';
  csPointHelp  =   'If you are getting assertions here, consider using the SetVector procedure';
  csPoint2DHelp =  'If you are getting assertions here, consider using one of the SetVector or SetPoint procedures';

var
   vManagers : TList;

// RegisterManager
//
procedure RegisterManager(aManager : TComponent);
begin
   if not Assigned(vManagers) then
      vManagers:=TList.Create;
   if vManagers.IndexOf(aManager)<0 then
      vManagers.Add(aManager);
end;

// DeRegisterManager
//
procedure DeRegisterManager(aManager : TComponent);
begin
   if Assigned(vManagers) then
      vManagers.Remove(aManager);
end;

// FindManager
//
function FindManager(classType : TComponentClass; const managerName : String) : TComponent;
var
   i : Integer;
begin
   Result:=nil;
   if Assigned(vManagers) then
      for i:=0 to vManagers.Count-1 do with TComponent(vManagers[i]) do
         if InheritsFrom(classType) and (Name=managerName) then begin
            Result:=TComponent(vManagers[i]);
            Break;
         end;
end;

//---------------------- TGLUpdateAbleObject -----------------------------------------

// Create
//
constructor TGLUpdateAbleObject.Create(AOwner: TPersistent);
begin
	inherited Create;
	FOwner:=AOwner;
end;

// NotifyChange
//
procedure TGLUpdateAbleObject.NotifyChange(Sender : TObject);
begin
   if (FUpdating=0) and Assigned(Owner) then begin
      if Owner is TGLUpdateAbleObject then
         TGLUpdateAbleObject(Owner).NotifyChange(Self)
      else if Owner is TGLUpdateAbleComponent then
         TGLUpdateAbleComponent(Owner).NotifyChange(Self);
      if Assigned(FOnNotifyChange) then
         FOnNotifyChange(Self);
   end;
end;

// GetOwner
//
function TGLUpdateAbleObject.GetOwner : TPersistent;
begin
   Result:=Owner;
end;

// BeginUpdate
//
procedure TGLUpdateAbleObject.BeginUpdate;
begin
   Inc(FUpdating);
end;

// EndUpdate
//
procedure TGLUpdateAbleObject.EndUpdate;
begin
   Dec(FUpdating);
   if FUpdating<=0 then begin
      Assert(FUpdating=0);
      NotifyChange(Self);
   end;
end;

// ------------------
// ------------------ TGLCadenceAbleComponent ------------------
// ------------------

{$ifdef GLS_DELPHI_4_DOWN}
// RemoveFreeNotification
//
procedure TGLCadenceAbleComponent.RemoveFreeNotification(AComponent: TComponent);
begin
   Notification(AComponent, opRemove);
end;
{$endif}

// DoProgress
//
procedure TGLCadenceAbleComponent.DoProgress(const progressTime : TProgressTimes);
begin
   // nothing
end;

// ------------------
// ------------------ TGLUpdateAbleObject ------------------
// ------------------

// NotifyChange
//
procedure TGLUpdateAbleComponent.NotifyChange(Sender : TObject);
begin
   if Assigned(Owner) then
   if (Owner is TGLUpdateAbleComponent) then
      (Owner as TGLUpdateAbleComponent).NotifyChange(Self);
end;

// ------------------
// ------------------ TGLCustomCoordinates ------------------
// ------------------

// CreateInitialized
//
constructor TGLCustomCoordinates.CreateInitialized(aOwner : TPersistent; const aValue : TVector;
                                             const aStyle : TGLCoordinatesStyle = csUnknown);
begin
   Create(aOwner);
   Initialize(aValue);
   FStyle:=aStyle;
end;

// Destroy
//
destructor TGLCustomCoordinates.Destroy;
begin
   if Assigned(FPDefaultCoords) then
      Dispose(FPDefaultCoords);
   inherited;
end;

// Initialize
//
procedure TGLCustomCoordinates.Initialize(const value : TVector);
begin
   FCoords:=value;
   if vUseDefaultSets then begin
      if not Assigned(FPDefaultCoords) then
         New(FPDefaultCoords);
      FPDefaultCoords^:=value;
   end;
end;

// Assign
//
procedure TGLCustomCoordinates.Assign(Source: TPersistent);
begin
   if Source is TGLCustomCoordinates then
      FCoords:=TGLCustomCoordinates(Source).FCoords
   else inherited;
end;

// WriteToFiler
//
procedure TGLCustomCoordinates.WriteToFiler(writer : TWriter);
var
   writeCoords : Boolean;
begin
   with writer do begin
      WriteInteger(0); // Archive Version 0
      if vUseDefaultSets then
         writeCoords:=not VectorEquals(FPDefaultCoords^, FCoords)
      else writeCoords:=True;
      WriteBoolean(writeCoords);
      if writeCoords then
         Write(FCoords[0], SizeOf(FCoords));
   end;
end;

// ReadFromFiler
//
procedure TGLCustomCoordinates.ReadFromFiler(reader : TReader);
var
   n : Integer;
begin
   with reader do begin
      ReadInteger; // Ignore ArchiveVersion
      if ReadBoolean then begin
         n:=SizeOf(FCoords);
         Assert(n=4*SizeOf(Single));
         Read(FCoords[0], n);
      end else if Assigned(FPDefaultCoords) then
         FCoords:=FPDefaultCoords^;
   end;
end;

// DefineProperties
//
procedure TGLCustomCoordinates.DefineProperties(Filer: TFiler);
begin
	inherited;
	Filer.DefineBinaryProperty('Coordinates', ReadData, WriteData,
                              not (Assigned(FPDefaultCoords) and VectorEquals(FPDefaultCoords^, FCoords)));
end;

// ReadData
//
procedure TGLCustomCoordinates.ReadData(Stream: TStream);
begin
	Stream.Read(FCoords, SizeOf(FCoords));
end;

// WriteData
//
procedure TGLCustomCoordinates.WriteData(Stream: TStream);
begin
	Stream.Write(FCoords, SizeOf(FCoords));
end;

// NotifyChange
//
procedure TGLCustomCoordinates.NotifyChange(Sender : TObject);
var
  Int: IGLCoordinatesUpdateAble;
begin
  if  Supports(Owner, IGLCoordinatesUpdateAble, Int) then
    Int.CoordinateChanged(TGLCoordinates(Self))
  else
    inherited NotifyChange(Sender);
end;

// Translate
//
procedure TGLCustomCoordinates.Translate(const translationVector : TVector);
begin
	FCoords[0]:=FCoords[0]+translationVector[0];
	FCoords[1]:=FCoords[1]+translationVector[1];
	FCoords[2]:=FCoords[2]+translationVector[2];
	NotifyChange(Self);
end;

// Translate
//
procedure TGLCustomCoordinates.Translate(const translationVector : TAffineVector);
begin
	FCoords[0]:=FCoords[0]+translationVector[0];
	FCoords[1]:=FCoords[1]+translationVector[1];
	FCoords[2]:=FCoords[2]+translationVector[2];
	NotifyChange(Self);
end;

// AddScaledVector (hmg)
//
procedure TGLCustomCoordinates.AddScaledVector(const factor : Single; const translationVector : TVector);
var
   f : Single;
begin
   f:=factor;
   CombineVector(FCoords, translationVector, f);
	NotifyChange(Self);
end;

// AddScaledVector (affine)
//
procedure TGLCustomCoordinates.AddScaledVector(const factor : Single; const translationVector : TAffineVector);
var
   f : Single;
begin
   f:=factor;
   CombineVector(FCoords, translationVector, f);
	NotifyChange(Self);
end;

// Rotate (affine)
//
procedure TGLCustomCoordinates.Rotate(const anAxis : TAffineVector; anAngle : Single);
begin
   RotateVector(FCoords, anAxis, anAngle);
   NotifyChange(Self);
end;

// Rotate (hmg)
//
procedure TGLCustomCoordinates.Rotate(const anAxis : TVector; anAngle : Single);
begin
   RotateVector(FCoords, anAxis, anAngle);
   NotifyChange(Self);
end;

// Normalize
//
procedure TGLCustomCoordinates.Normalize;
begin
   NormalizeVector(FCoords);
   NotifyChange(Self);
end;

// Invert
//
procedure TGLCustomCoordinates.Invert;
begin
   NegateVector(FCoords);
   NotifyChange(Self);
end;

// Scale
//
procedure TGLCustomCoordinates.Scale(factor : Single);
begin
   ScaleVector(PAffineVector(@FCoords)^, factor);
   NotifyChange(Self);
end;

// VectorLength
//
function TGLCustomCoordinates.VectorLength : TGLFloat;
begin
   Result:=VectorGeometry.VectorLength(FCoords);
end;

// VectorNorm
//
function TGLCustomCoordinates.VectorNorm : TGLFloat;
begin
   Result:=VectorGeometry.VectorNorm(FCoords);
end;

// MaxXYZ
//
function TGLCustomCoordinates.MaxXYZ : Single;
begin
   Result:=VectorGeometry.MaxXYZComponent(FCoords);
end;

// Equals
//
function TGLCustomCoordinates.Equals(const aVector : TVector) : Boolean;
begin
   Result:=VectorEquals(FCoords, aVector);
end;

// SetVector (affine)
//
procedure TGLCustomCoordinates.SetVector(const x, y: Single; z : Single = 0);
begin
  Assert(FStyle = csVector, csVectorHelp);
  VectorGeometry.SetVector(FCoords, x, y, z);
  NotifyChange(Self);
end;

// SetVector (TAffineVector)
//
procedure TGLCustomCoordinates.SetVector(const v : TAffineVector);
begin
  Assert(FStyle = csVector, csVectorHelp);
  VectorGeometry.SetVector(FCoords, v);
  NotifyChange(Self);
end;

// SetVector (TVector)
//
procedure TGLCustomCoordinates.SetVector(const v : TVector);
begin
  Assert(FStyle = csVector, csVectorHelp);
  VectorGeometry.SetVector(FCoords, v);
  NotifyChange(Self);
end;

// SetVector (hmg)
//
procedure TGLCustomCoordinates.SetVector(const x, y, z, w : Single);
begin
  Assert(FStyle = csVector, csVectorHelp);
  VectorGeometry.SetVector(FCoords, x, y, z, w);
  NotifyChange(Self);
end;

// SetDirectVector
//
procedure TGLCustomCoordinates.SetDirectVector(const v : TVector);
begin
   FCoords[0]:=v[0];
   FCoords[1]:=v[1];
   FCoords[2]:=v[2];
   FCoords[3]:=v[3];
end;

// SetToZero
//
procedure TGLCustomCoordinates.SetToZero;
begin
   FCoords[0]:=0;
   FCoords[1]:=0;
   FCoords[2]:=0;
   if FStyle=csPoint then
      FCoords[3]:=1
   else FCoords[3]:=0;
	NotifyChange(Self);
end;

// SetPoint
//
procedure TGLCustomCoordinates.SetPoint(const x, y, z : Single);
begin
  Assert(FStyle = csPoint, csPointHelp);
  VectorGeometry.MakePoint(FCoords, x, y, z);
  NotifyChange(Self);
end;

// SetPoint (TAffineVector)
//
procedure TGLCustomCoordinates.SetPoint(const v : TAffineVector);
begin
  Assert(FStyle = csPoint, csPointHelp);
  VectorGeometry.MakePoint(FCoords, v);
  NotifyChange(Self);
end;

// SetPoint (TVector)
//
procedure TGLCustomCoordinates.SetPoint(const v : TVector);
begin
  Assert(FStyle = csPoint, csPointHelp);
  VectorGeometry.MakePoint(FCoords, v);
  NotifyChange(Self);
end;

// SetPoint2D
//
procedure TGLCustomCoordinates.SetPoint2D(const x, y : Single);
begin
  Assert(FStyle = csPoint2D, csPoint2DHelp);
  VectorGeometry.MakeVector(FCoords, x, y, 0);
  NotifyChange(Self);
end;

// SetPoint2D (TAffineVector)
//
procedure TGLCustomCoordinates.SetPoint2D(const v : TAffineVector);
begin
  Assert(FStyle = csPoint2D, csPoint2DHelp);
  VectorGeometry.MakeVector(FCoords, v);
  NotifyChange(Self);
end;

// SetPoint2D (TVector)
//
procedure TGLCustomCoordinates.SetPoint2D(const v : TVector);
begin
  Assert(FStyle = csPoint2D, csPoint2DHelp);
  VectorGeometry.MakeVector(FCoords, v);
  NotifyChange(Self);
end;

// SetPoint2D (TVector2f)
//
procedure TGLCustomCoordinates.SetPoint2D(const v : TVector2f);
begin
  Assert(FStyle = csPoint2D, csPoint2DHelp);
  VectorGeometry.MakeVector(FCoords, v[0], v[1], 0);
  NotifyChange(Self);
end;

// AsAddress
//
function TGLCustomCoordinates.AsAddress : PGLFloat;
begin
   Result:=@FCoords;
end;

// SetAsVector
//
procedure TGLCustomCoordinates.SetAsVector(const value: TVector);
begin
   FCoords:=value;
   case FStyle of
      csPoint2D :
      begin
        FCoords[2] := 0;
        FCoords[3] := 0;
      end;
      csPoint :  FCoords[3]:=1;
      csVector : FCoords[3]:=0;
    else
      Assert(False);
   end;
	NotifyChange(Self);
end;

// SetAsAffineVector
//
procedure TGLCustomCoordinates.SetAsAffineVector(const value : TAffineVector);
begin
   case FStyle of
      csPoint2D : MakeVector(FCoords, value);
      csPoint :   MakePoint(FCoords, value);
      csVector:   MakeVector(FCoords, value);
   else
      Assert(False);
   end;
	NotifyChange(Self);
end;

// SetAsPoint2D
//
procedure TGLCustomCoordinates.SetAsPoint2D(const Value : TVector2f);
begin
   case FStyle of
      csPoint2D, csPoint,  csVector:
      begin
        FCoords[0] := Value[0];
        FCoords[1] := Value[1];
        FCoords[2] := 0;
        FCoords[3] := 0;
      end;
   else
      Assert(False);
   end;
	NotifyChange(Self);
end;

// GetAsAffineVector
//
function TGLCustomCoordinates.GetAsAffineVector : TAffineVector;
begin
   VectorGeometry.SetVector(Result, FCoords);
end;

// GetAsPoint2D
//
function TGLCustomCoordinates.GetAsPoint2D : TVector2f;
begin
   Result[0] := FCoords[0];
   Result[1] := FCoords[1];
end;

// SetCoordinate
//
procedure TGLCustomCoordinates.SetCoordinate(index : Integer; const aValue : TGLFloat);
begin
	FCoords[index]:=aValue;
	NotifyChange(Self);
end;

// GetAsString
//
function TGLCustomCoordinates.GetAsString : String;
begin
   case Style of
     csPoint2D: Result := Format('(%g; %g)',         [FCoords[0], FCoords[1]]);
     csPoint:   Result := Format('(%g; %g; %g)',     [FCoords[0], FCoords[1], FCoords[2]]);
     csVector:  Result := Format('(%g; %g; %g; %g)', [FCoords[0], FCoords[1], FCoords[2], FCoords[3]]);
   else
     Assert(False);
   end;
end;

// ------------------
// ------------------ TGLNode ------------------
// ------------------

// Create
//
constructor TGLNode.Create(ACollection : TCollection);
begin
	inherited Create(ACollection);
   // nothing, yet
end;

// Destroy
//
destructor TGLNode.Destroy;
begin
   // nothing, yet
	inherited Destroy;
end;

// Assign
//
procedure TGLNode.Assign(Source: TPersistent);
begin
	if Source is TGLNode then begin
      FCoords:=TGLNode(Source).FCoords;
	end else inherited;
end;

// GetDisplayName
//
function TGLNode.GetDisplayName : String;
begin
	Result:=Format('%.4f; %.4f; %.4f', [X, Y, Z]);
end;

// AsAddress
//
function TGLNode.AsAddress : PGLFloat;
begin
   Result:=@FCoords;
end;

// SetAsVector
//
procedure TGLNode.SetAsVector(const value: TVector);
begin
	FCoords:=Value;
   (Collection as TGLNodes).NotifyChange;
end;

// SetAsAffineVector
//
procedure TGLNode.SetAsAffineVector(const value : TAffineVector);
begin
   VectorGeometry.SetVector(FCoords, value);
   (Collection as TGLNodes).NotifyChange;
end;

// GetAsAffineVector
//
function TGLNode.GetAsAffineVector : TAffineVector;
begin
   VectorGeometry.SetVector(Result, FCoords);
end;

// SetCoordinate
//
procedure TGLNode.SetCoordinate(AIndex: Integer; AValue: TGLFloat);
begin
	FCoords[AIndex]:=AValue;
   (Collection as TGLNodes).NotifyChange;
end;

// StoreCoordinate
//
function TGLNode.StoreCoordinate(AIndex: Integer) : Boolean;
begin
   Result:=(FCoords[AIndex]<>0);
end;

// ------------------
// ------------------ TGLNodes ------------------
// ------------------

// Create
//
constructor TGLNodes.Create(AOwner : TPersistent; AItemClass: TCollectionItemClass = nil);
begin
   if not Assigned(AItemClass) then
      inherited Create(AOwner, TGLNode)
   else inherited Create(AOwner, AItemClass);
end;

// CreateCopy
//
function TGLNodes.CreateCopy(AOwner : TPersistent) : TGLNodes;
begin
   if Self<>nil then begin
      Result:=TGLNodesClass(Self.ClassType).Create(AOwner);
      Result.Assign(Self);
   end else Result:=nil;
end;

// SetItems
//
procedure TGLNodes.SetItems(index : Integer; const val : TGLNode);
begin
	inherited Items[index]:=val;
end;

// GetItems
//
function TGLNodes.GetItems(index : Integer) : TGLNode;
begin
	Result:=TGLNode(inherited Items[index]);
end;

//First
//
function TGLNodes.First : TGLNode;
begin
   if Count>0 then
      Result:=TGLNode(inherited Items[0])
   else Result:=nil;
end;

// Last
//
function TGLNodes.Last : TGLNode;
var
   n : Integer;
begin
   n:=Count-1;
   if n>=0 then
      Result:=TGLNode(inherited Items[n])
   else Result:=nil;
end;

// Update
//
procedure TGLNodes.Update(Item: TCollectionItem);
begin
   inherited;
   NotifyChange;
end;

// Add
//
function TGLNodes.Add: TGLNode;
begin
	Result:=(inherited Add) as TGLNode;
end;

// FindItemID
//
function TGLNodes.FindItemID(ID: Integer): TGLNode;
begin
	Result:=(inherited FindItemID(ID)) as TGLNode;
end;

// NotifyChange
//
procedure TGLNodes.NotifyChange;
begin
   if (UpdateCount=0) and (GetOwner<>nil) and (GetOwner is TGLUpdateAbleComponent) then
      TGLUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

// EndUpdate
//
procedure TGLNodes.EndUpdate;
begin
   inherited EndUpdate;
   // Workaround for a bug in VCL's EndUpdate
   if UpdateCount=0 then NotifyChange;
end;

// AddNode (TGLCustomCoordinates)
//
procedure TGLNodes.AddNode(const coords : TGLCustomCoordinates);
begin
   Add.AsVector:=coords.AsVector;
end;

// AddNode (floats)
//
procedure TGLNodes.AddNode(const x, y, z : Single);
begin
   Add.AsVector:=PointMake(x, y, z);
end;

// AddNode (TVector)
//
procedure TGLNodes.AddNode(const value : TVector);
begin
   Add.AsVector:=value;
end;

// AddNode (TAffineVector)
//
procedure TGLNodes.AddNode(const value : TAffineVector);
begin
   Add.AsAffineVector:=value;
end;

// AddXYArc
//
procedure TGLNodes.AddXYArc(xRadius, yRadius : Single;
                            startAngle, stopAngle : Single;
                            nbSegments : Integer;
                            const center : TAffineVector);
var
   i : Integer;
   f : Single;
   s, c : Single;
begin
   BeginUpdate;
   try
      startAngle:=DegToRad(startAngle);
      stopAngle :=DegToRad(stopAngle);
      f:=(stopAngle-startAngle)/nbSegments;
      for i:=0 to nbSegments do begin
         SinCos(i*f+startAngle, s, c);
         SetVector(Add.FCoords, center[0]+xRadius*c, center[1]+yRadius*s, center[2], 1);
      end;
   finally
      EndUpdate;
   end;
end;

// Barycenter
//
function TGLNodes.Barycenter : TAffineVector;
var
   i : Integer;
begin
   Result:=NullVector;
   if Count>0 then begin
      for i:=0 to Count-1 do
         AddVector(Result, PAffineVector(Items[i].AsAddress)^);
      ScaleVector(Result, 1.0/Count);
   end;
end;

// Normal
//
function TGLNodes.Normal : TAffineVector;
begin
   if Count>=3 then
      CalcPlaneNormal(Items[0].FCoords, Items[1].FCoords, Items[2].FCoords, Result)
   else Result:=NullVector;
end;

// Vector
//
function TGLNodes.Vector(i : Integer) : TAffineVector;

   procedure CalcUsingPrev; forward;

   procedure CalcUsingNext;
   begin
      if i<Count-1 then
         VectorSubtract(Items[i].AsVector, Items[i+1].AsVector, Result)
      else CalcUsingPrev;
   end;

   procedure CalcUsingPrev;
   begin
      if i>0 then
         VectorSubtract(Items[i-1].AsVector, Items[i].AsVector, Result)
      else CalcUsingNext;
   end;

begin
   Assert((i>=0) and (i<Count));
   if i=0 then
      if i=Count-1 then
         SetVector(Result, NullVector)
      else VectorSubtract(Items[i+1].AsVector, Items[i].AsVector, Result)
   else if i=Count-1 then
      VectorSubtract(Items[i].AsVector, Items[i-1].AsVector, Result)
   else VectorSubtract(Items[i+1].AsVector, Items[i-1].AsVector, Result);
   if VectorNorm(Result)<1e-5 then
      SetVector(Result, NullVector)
   else NormalizeVector(Result);
end;

// GetExtents
//
procedure TGLNodes.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   f : Single;
const
   cBigValue : Single = 1e50;
   cSmallValue : Single = -1e50;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to Count-1 do begin
      for k:=0 to 2 do begin
         f:=PAffineVector(Items[i].AsAddress)^[k];
         if f<min[k] then min[k]:=f;
         if f>max[k] then max[k]:=f;
      end;
   end;
end;

// Translate
//
procedure TGLNodes.Translate(const tv : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      AddVector(PAffineVector(Items[i].AsAddress)^, tv);
   NotifyChange;
end;

// Scale (vector)
//
procedure TGLNodes.Scale(const fv : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      ScaleVector(PAffineVector(Items[i].AsAddress)^, fv);
   NotifyChange;
end;

// Scale (single)
//
procedure TGLNodes.Scale(f : Single);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      ScaleVector(PAffineVector(Items[i].AsAddress)^, f);
   NotifyChange;
end;

// RotateAroundX
//
procedure TGLNodes.RotateAroundX(angle : Single);
var
   i : Integer;
   c, s, v2 : Single;
   v : PAffineVector;
begin
   SinCos(cPIDiv180*angle, s, c);
   for i:=0 to Count-1 do begin
      v:=PAffineVector(Items[i].AsAddress);
      v2:=v^[2];
      v^[1]:=c*v^[1]+s*v2;
      v^[2]:=c*v2-s*v^[1];
   end;
   NotifyChange;
end;

// RotateAroundY
//
procedure TGLNodes.RotateAroundY(angle : Single);
var
   i : Integer;
   c, s, v0 : Single;
   v : PAffineVector;
begin
   SinCos(cPIDiv180*angle, s, c);
   for i:=0 to Count-1 do begin
      v:=PAffineVector(Items[i].AsAddress);
      v0:=v^[0];
      v^[0]:=c*v0+s*v^[2];
      v^[2]:=c*v^[2]-s*v0;
   end;
   NotifyChange;
end;

// RotateAroundZ
//
procedure TGLNodes.RotateAroundZ(angle : Single);
var
   i : Integer;
   c, s, v1 : Single;
   v : PAffineVector;
begin
   SinCos(cPIDiv180*angle, s, c);
   for i:=0 to Count-1 do begin
      v:=PAffineVector(Items[i].AsAddress);
      v1:=v^[1];
      v^[1]:=c*v1+s*v^[0];
      v^[0]:=c*v^[0]-s*v1;
   end;
   NotifyChange;
end;

// CreateNewCubicSpline
//
function TGLNodes.CreateNewCubicSpline : TCubicSpline;
var
   i : Integer;
   xa, ya, za : PFloatArray;
begin
   GetMem(xa, SizeOf(TGLFloat)*Count);
   GetMem(ya, SizeOf(TGLFloat)*Count);
   GetMem(za, SizeOf(TGLFloat)*Count);
   for i:=0 to Count-1 do with Items[i] do begin
      xa^[i]:=X;
      ya^[i]:=Y;
      za^[i]:=Z;
   end;
   Result:=TCubicSpline.Create(xa, ya, za, nil, Count);
   FreeMem(xa);
   FreeMem(ya);
   FreeMem(za);
end;

// RenderTesselatedPolygon
//
var
   _rtp_nbExtraVertices : Integer;
   _rtp_newVertices : PAffineVectorArray;


   function _rtp_AllocNewVertex : PAffineVector;
   begin
      Inc(_rtp_nbExtraVertices);
      Result:=@_rtp_newVertices[_rtp_nbExtraVertices-1];
   end;

   procedure _rtp_tessError(errno : TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$ifdef unix} cdecl; {$ENDIF}
   begin
      Assert(False, IntToStr(errno)+': '+gluErrorString(errno));
   end;

   procedure _rtp_tessIssueVertex(vertexData : Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$ifdef unix} cdecl; {$ENDIF}
   begin
      xglTexCoord2fv(vertexData);
      glVertex3fv(vertexData);
   end;

   procedure _rtp_tessCombine(coords : PDoubleVector; vertex_data : Pointer;
                         weight : PGLFloat; var outData : Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$ifdef unix} cdecl; {$ENDIF}
   begin
      outData:=_rtp_AllocNewVertex;
      SetVector(PAffineVector(outData)^, coords^[0], coords^[1], coords^[2]);
   end;


procedure TGLNodes.RenderTesselatedPolygon(ATextured : Boolean;
                                           ANormal : PAffineVector = nil;
                                           ASplineDivisions : Integer = 1;
                                           AInvertNormals : Boolean = False);
var
   i : Integer;
   tess : PGLUTesselator;
   dblVector : TAffineDblVector;
   spline : TCubicSpline;
   splinePos : PAffineVector;
   f : Single;


begin
   if Count>2 then begin
      // Create and initialize the GLU tesselator
      tess:=gluNewTess;
      gluTessCallback(tess, GLU_TESS_BEGIN, @glBegin);
      if ATextured then
         gluTessCallback(tess, GLU_TESS_VERTEX, @_rtp_tessIssueVertex)
      else gluTessCallback(tess, GLU_TESS_VERTEX, @glVertex3fv);
      gluTessCallback(tess, GLU_TESS_END, @glEnd);
      gluTessCallback(tess, GLU_TESS_ERROR, @_rtp_tessError);
      gluTessCallback(tess, GLU_TESS_COMBINE, @_rtp_tessCombine);
      _rtp_nbExtraVertices:=0;
      // Issue normal
      if Assigned(ANormal) then begin
         glNormal3fv(PGLFloat(ANormal));
         gluTessNormal(tess, ANormal^[0], ANormal^[1], ANormal^[2]);
      end;
      // Issue polygon
      gluTessBeginPolygon(tess, nil);
      gluTessBeginContour(tess);
      if ASplineDivisions<=1 then begin
         // no spline, use direct coordinates
         GetMem(_rtp_newVertices, Count*SizeOf(TAffineVector));
         if AInvertNormals then begin
            for i:=Count-1 downto 0 do begin
               SetVector(dblVector, PAffineVector(Items[i].AsAddress)^);
               gluTessVertex(tess, dblVector, Items[i].AsAddress);
            end;
         end else begin
            for i:=0 to Count-1 do begin
               SetVector(dblVector, PAffineVector(Items[i].AsAddress)^);
               gluTessVertex(tess, dblVector, Items[i].AsAddress);
            end;
         end;
      end else begin
         // cubic spline
         GetMem(_rtp_newVertices, 2*ASplineDivisions*Count*SizeOf(TAffineVector));
         spline:=CreateNewCubicSpline;
         f:=1.0/ASplineDivisions;
         if AInvertNormals then begin
            for i:=ASplineDivisions*(Count-1) downto 0 do begin
               splinePos:=_rtp_AllocNewVertex;
               spline.SplineAffineVector(i*f, splinePos^);
               SetVector(dblVector, splinePos^);
               gluTessVertex(tess, dblVector, splinePos);
            end;
         end else begin
            for i:=0 to ASplineDivisions*(Count-1) do begin
               splinePos:=_rtp_AllocNewVertex;
               spline.SplineAffineVector(i*f, splinePos^);
               SetVector(dblVector, splinePos^);
               gluTessVertex(tess, dblVector, splinePos);
            end;
         end;
         spline.Free;
      end;
      gluTessEndContour(tess);
      gluTessEndPolygon(tess);
      // release stuff
      if Assigned(_rtp_newVertices) then
         FreeMem(_rtp_newVertices);
      gluDeleteTess(tess);
   end;
end;

// ------------------
// ------------------ TNotifyCollection ------------------
// ------------------

// Create
//
constructor TNotifyCollection.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
   inherited Create(AOwner,AItemClass);
   if Assigned(AOwner) and (AOwner is TGLUpdateAbleComponent) then
      OnNotifyChange:=TGLUpdateAbleComponent(AOwner).NotifyChange;
end;

// Update
//
procedure TNotifyCollection.Update(Item: TCollectionItem);
begin
   inherited;
   if Assigned(FOnNotifyChange) then
      FOnNotifyChange(Self);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
initialization
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

finalization

   vManagers.Free;
   vManagers:=nil;

end.
