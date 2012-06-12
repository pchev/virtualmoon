// GLMultiProxy
{: Implements a multi-proxy objects, useful for discreet LOD.<p>

	<b>History : </b><font size=-1><ul>
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>19/12/06 - DaS - Fixed a bug in TGLMultiProxy.Destroy
      <li>26/11/03 - EG - Added bounding, raycast and silhouette proxying
      <li>25/11/03 - EG - Added per-master visibility boolean
      <li>24/11/03 - EG - Creation
   </ul></font>
}
unit GLMultiProxy;

interface

uses Classes, GLScene, VectorGeometry, GLSilhouette,
     GLRenderContextInfo, BaseClasses, VectorTypes;

type

   TGLMultiProxy = class;

	// TGLMultiProxyMaster
	//
   {: MasterObject description for a MultiProxy object. }
	TGLMultiProxyMaster = class (TCollectionItem)
	   private
	      { Private Declarations }
         FMasterObject : TGLBaseSceneObject;
         FDistanceMin, FDistanceMin2 : Single;
         FDistanceMax, FDistanceMax2 : Single;
         FVisible : Boolean;

	   protected
	      { Protected Declarations }
         function GetDisplayName : String; override;
         procedure SetMasterObject(const val : TGLBaseSceneObject);
         procedure SetDistanceMin(const val : Single);
         procedure SetDistanceMax(const val : Single);
         procedure SetVisible(const val : Boolean);

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         function OwnerObject : TGLMultiProxy;
         procedure NotifyChange;

      published
         { Published Declarations }
         {: Specifies the Master object which will be proxy'ed. }
         property MasterObject : TGLBaseSceneObject read FMasterObject write SetMasterObject;
         {: Minimum visibility distance (inclusive). }
         property DistanceMin : Single read FDistanceMin write SetDistanceMin;
         {: Maximum visibility distance (exclusive). }
         property DistanceMax : Single read FDistanceMax write SetDistanceMax;
         {: Determines if the master object can be visible (proxy'ed).<p>
            Note: the master object's distance also has to be within DistanceMin
            and DistanceMax.}
         property Visible : Boolean read FVisible write SetVisible default True;
   end;

	// TGLMultiProxyMasters
	//
   {: Collection of TGLMultiProxyMaster. }
	TGLMultiProxyMasters = class (TOwnedCollection)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }
         procedure SetItems(index : Integer; const val : TGLMultiProxyMaster);
	      function GetItems(index : Integer) : TGLMultiProxyMaster;
         procedure Update(Item: TCollectionItem); override;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TPersistent);

         function Add : TGLMultiProxyMaster; overload;
         function Add(master : TGLBaseSceneObject; distanceMin, distanceMax : Single) : TGLMultiProxyMaster; overload;
	      property Items[index : Integer] : TGLMultiProxyMaster read GetItems write SetItems; default;

         procedure Notification(AComponent: TComponent);
         
         procedure NotifyChange;
         procedure EndUpdate; override;
   end;

   // TGLMultiProxy
   //
   {: Multiple Proxy object.<p>
      This proxy has multiple master objects, which are individually made visible
      depending on a distance to the camera criterion. It can be used to implement
      discreet level of detail directly for static objects, or objects that
      go through cyclic animation.<p>
      For dimensionsn raycasting and silhouette purposes, the first master is used
      (item zero in the MasterObjects collection). }
   TGLMultiProxy = class (TGLSceneObject)
      private
			{ Private Declarations }
         FMasterObjects : TGLMultiProxyMasters;
         FRendering : Boolean; // internal use (loop protection)

	   protected
	      { Protected Declarations }
         procedure SetMasterObjects(const val : TGLMultiProxyMasters);
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

         function PrimaryMaster : TGLBaseSceneObject;

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

	      procedure Assign(Source: TPersistent); override;
         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;
                            
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                 intersectPoint : PVector = nil;
                                 intersectNormal : PVector = nil) : Boolean; override;
         function GenerateSilhouette(const silhouetteParameters : TGLSilhouetteParameters) : TGLSilhouette; override;

      published
         { Published Declarations }
         property MasterObjects : TGLMultiProxyMasters read FMasterObjects write SetMasterObjects;
         
         property ObjectsSorting;
         property Direction;
         property PitchAngle;
         property Position;
         property RollAngle;
         property Scale;
         property ShowAxes;
         property TurnAngle;
         property Up;
         property Visible;
         property OnProgress;
         property Behaviours;
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses SysUtils, OpenGLTokens, GLContext;

// ------------------
// ------------------ TGLMultiProxyMaster ------------------
// ------------------

// Create
//
constructor TGLMultiProxyMaster.Create(Collection : TCollection);
begin
	inherited Create(Collection);
   FVisible:=True;
end;

// Destroy
//
destructor TGLMultiProxyMaster.Destroy;
begin
   MasterObject:=nil;
	inherited Destroy;
end;

// Assign
//
procedure TGLMultiProxyMaster.Assign(Source: TPersistent);
begin
	if Source is TGLMultiProxyMaster then begin
      MasterObject:=TGLMultiProxyMaster(Source).MasterObject;
      FDistanceMin:=TGLMultiProxyMaster(Source).FDistanceMin;
      FDistanceMin2:=TGLMultiProxyMaster(Source).FDistanceMin2;
      FDistanceMax:=TGLMultiProxyMaster(Source).FDistanceMax;
      FDistanceMax2:=TGLMultiProxyMaster(Source).FDistanceMax2;
      FVisible:=TGLMultiProxyMaster(Source).FVisible;
      NotifyChange;
	end else inherited;
end;

// OwnerObject
//
function TGLMultiProxyMaster.OwnerObject : TGLMultiProxy;
begin
   Result:=TGLMultiProxy(TGLMultiProxyMasters(Collection).GetOwner);
end;

// NotifyChange
//
procedure TGLMultiProxyMaster.NotifyChange;
begin
   TGLMultiProxyMasters(Collection).NotifyChange;
end;

// GetDisplayName
//
function TGLMultiProxyMaster.GetDisplayName : String;
begin
   if MasterObject<>nil then
      Result:=MasterObject.Name
   else Result:='???';
	Result:=Result+Format(' [%.2f; %.2f[', [DistanceMin, DistanceMax]);
   if not Visible then
      Result:=Result+' (hidden)';
end;

// SetMasterObject
//
procedure TGLMultiProxyMaster.SetMasterObject(const val : TGLBaseSceneObject);
begin
   if FMasterObject<>val then begin
      if Assigned(FMasterObject) then
         FMasterObject.RemoveFreeNotification(OwnerObject);
      FMasterObject:=val;
      if Assigned(FMasterObject) then
         FMasterObject.FreeNotification(OwnerObject);
      NotifyChange;
   end;
end;

// SetDistanceMin
//
procedure TGLMultiProxyMaster.SetDistanceMin(const val : Single);
begin
   if FDistanceMin<>val then begin
      FDistanceMin:=val;
      FDistanceMin2:=Sqr(val);
      NotifyChange;
   end;
end;

// SetDistanceMax
//
procedure TGLMultiProxyMaster.SetDistanceMax(const val : Single);
begin
   if FDistanceMax<>val then begin
      FDistanceMax:=val;
      FDistanceMax2:=Sqr(val);
      NotifyChange;
   end;
end;

// SetVisible
//
procedure TGLMultiProxyMaster.SetVisible(const val : Boolean);
begin
   if FVisible<>val then begin
      FVisible:=val;
      NotifyChange;
   end;
end;

// ------------------
// ------------------ TGLMultiProxyMasters ------------------
// ------------------

// Create
//
constructor TGLMultiProxyMasters.Create(AOwner : TPersistent);
begin
   inherited Create(AOwner, TGLMultiProxyMaster)
end;

// SetItems
//
procedure TGLMultiProxyMasters.SetItems(index : Integer; const val : TGLMultiProxyMaster);
begin
	inherited Items[index]:=val;
end;

// GetItems
//
function TGLMultiProxyMasters.GetItems(index : Integer) : TGLMultiProxyMaster;
begin
	Result:=TGLMultiProxyMaster(inherited Items[index]);
end;

// Update
//
procedure TGLMultiProxyMasters.Update(Item : TCollectionItem);
begin
   inherited;
   NotifyChange;
end;

// Add (simple)
//
function TGLMultiProxyMasters.Add : TGLMultiProxyMaster;
begin
	Result:=(inherited Add) as TGLMultiProxyMaster;
end;

// Add (classic params)
//
function TGLMultiProxyMasters.Add(master : TGLBaseSceneObject; distanceMin, distanceMax : Single) : TGLMultiProxyMaster;
begin
   BeginUpdate;
	Result:=(inherited Add) as TGLMultiProxyMaster;
   Result.MasterObject:=master;
   Result.DistanceMin:=distanceMin;
   Result.DistanceMax:=distanceMax;
   EndUpdate;
end;

// Notification
//
procedure TGLMultiProxyMasters.Notification(AComponent: TComponent);
var
   i : Integer;
begin
   for i:=0 to Count-1 do with Items[i] do
      if FMasterObject=AComponent then FMasterObject:=nil;
end;

// NotifyChange
//
procedure TGLMultiProxyMasters.NotifyChange;
begin
   if (UpdateCount=0) and (GetOwner<>nil) and (GetOwner is TGLUpdateAbleComponent) then
      TGLUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

// EndUpdate
//
procedure TGLMultiProxyMasters.EndUpdate;
begin
   inherited EndUpdate;
   // Workaround for a bug in VCL's EndUpdate
   if UpdateCount=0 then NotifyChange;
end;

// ------------------
// ------------------ TGLMultiProxy ------------------
// ------------------

// Create
//
constructor TGLMultiProxy.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FMasterObjects:=TGLMultiProxyMasters.Create(Self);
end;

// Destroy
//
destructor TGLMultiProxy.Destroy;
begin
  inherited Destroy;
  FMasterObjects.Free;
end;

// Notification
//
procedure TGLMultiProxy.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then
      FMasterObjects.Notification(AComponent);
   inherited;
end;

// SetMasterObjects
//
procedure TGLMultiProxy.SetMasterObjects(const val : TGLMultiProxyMasters);
begin
   FMasterObjects.Assign(val);
   StructureChanged;
end;

// Assign
//
procedure TGLMultiProxy.Assign(Source: TPersistent);
begin
   if Source is TGLMultiProxy then begin
      MasterObjects:=TGLMultiProxy(Source).MasterObjects;
   end;
   inherited;
end;

// Render
//
procedure TGLMultiProxy.DoRender(var rci : TRenderContextInfo;
                                  renderSelf, renderChildren : Boolean);
var
   i : Integer;
   oldProxySubObject : Boolean;
   mpMaster : TGLMultiProxyMaster;
   master : TGLBaseSceneObject;
   d2 : Single;
begin
   if FRendering then Exit;
   FRendering:=True;
   try
      d2:=VectorDistance2(rci.cameraPosition, AbsolutePosition);
      for i:=0 to MasterObjects.Count-1 do begin
         mpMaster:=MasterObjects[i];
         if mpMaster.Visible then begin
            master:=mpMaster.MasterObject;
            if (master<>nil) and (d2>=mpMaster.FDistanceMin2) and (d2<mpMaster.FDistanceMax2) then begin
               oldProxySubObject:=rci.proxySubObject;
               rci.proxySubObject:=True;
               GL.MultMatrixf(PGLFloat(master.MatrixAsAddress));
               master.DoRender(rci, renderSelf, (master.Count>0));
               rci.proxySubObject:=oldProxySubObject;
            end;
         end;
      end;
      // now render self stuff (our children, our effects, etc.)
      if renderChildren and (Count>0) then
         Self.RenderChildren(0, Count-1, rci);
//      if masterGotEffects then
//         FMasterObject.Effects.RenderPostEffects(Scene.CurrentBuffer, rci);
   finally
      FRendering:=False;
   end;
   ClearStructureChanged;
end;

// PrimaryMaster
//
function TGLMultiProxy.PrimaryMaster : TGLBaseSceneObject;
begin
   if MasterObjects.Count>0 then
      Result:=MasterObjects[0].MasterObject
   else Result:=nil;
end;

// AxisAlignedDimensions
//
function TGLMultiProxy.AxisAlignedDimensionsUnscaled : TVector;
var
   master : TGLBaseSceneObject;
begin
   master:=PrimaryMaster;
   if Assigned(master) then begin
      Result:=master.AxisAlignedDimensionsUnscaled;
   end else Result:=inherited AxisAlignedDimensionsUnscaled;
end;

// RayCastIntersect
//
function TGLMultiProxy.RayCastIntersect(const rayStart, rayVector : TVector;
                                 intersectPoint : PVector = nil;
                                 intersectNormal : PVector = nil) : Boolean;
var
   localRayStart, localRayVector : TVector;
   master : TGLBaseSceneObject;
begin
   master:=PrimaryMaster;
   if Assigned(master) then begin
      SetVector(localRayStart, AbsoluteToLocal(rayStart));
      SetVector(localRayStart, master.LocalToAbsolute(localRayStart));
      SetVector(localRayVector, AbsoluteToLocal(rayVector));
      SetVector(localRayVector, master.LocalToAbsolute(localRayVector));
      NormalizeVector(localRayVector);

      Result:=master.RayCastIntersect(localRayStart, localRayVector,
                                            intersectPoint, intersectNormal);
      if Result then begin
         if Assigned(intersectPoint) then begin
            SetVector(intersectPoint^, master.AbsoluteToLocal(intersectPoint^));
            SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
         end;
         if Assigned(intersectNormal) then begin
            SetVector(intersectNormal^, master.AbsoluteToLocal(intersectNormal^));
            SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
         end;
      end;
   end else Result:=False;
end;

// GenerateSilhouette
//
function TGLMultiProxy.GenerateSilhouette(const silhouetteParameters : TGLSilhouetteParameters) : TGLSilhouette;
var
   master : TGLBaseSceneObject;
begin
   master:=PrimaryMaster;
   if Assigned(master) then
      Result:=master.GenerateSilhouette(silhouetteParameters)
   else Result:=nil;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLMultiProxy]);

end.
