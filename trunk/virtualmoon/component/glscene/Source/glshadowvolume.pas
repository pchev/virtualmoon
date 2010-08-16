//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLShadowVolumes<p>

   Implements basic shadow volumes support.<p>

   Be aware that only objects that support silhouette determination have a chance
   to cast correct shadows. Transparent/blended/shader objects among the receivers
   or the casters will be rendered incorrectly.<p>

 <b>History : </b><font size=-1><ul>
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>31/03/07 - DaStr - Fixed issue with invalid typecasting
                            (thanks Burkhard Carstens) (Bugtracker ID = 1692016)
      <li>30/03/07 - DaStr - Added $I GLScene.inc
      <li>28/03/07 - DaStr - Renamed parameters in some methods
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678658)
      <li>08/12/04 - DB - Fixed bug in TGLShadowVolumeCaster.SetCaster
      <li>02/12/04 - MF - Added some documentation
      <li>23/03/04 - EG - Added Active property
      <li>29/11/03 - MF - Removed a "feature" that would draw the shadow of
                          (hierarchially) invisible objects
      <li>27/11/03 - MF - TGLShadowVolumeCaster now registers with the FCaster
                          for delete notification
      <li>11/06/03 - EG - Added silhouette cache
      <li>04/06/03 - EG - Creation (based on code from Mattias Fagerlund)
  </ul></font>
}
unit GLShadowVolume;

interface

{$I GLScene.inc}

uses Classes, GLScene, VectorGeometry, OpenGL1x, GLSilhouette,
  GLCrossPlatform, PersistentClasses, GeometryBB, GLColor, GLRenderContextInfo;

type

  TGLShadowVolume = class;

  {: Determines when a shadow volume should generate a cap at the beginning and
   end of the volume. This is ONLY necessary when there's a chance that the
   camera could end up inside the shadow _or_ between the light source and
   the camera. If those two situations can't occur then not using capping is
   the best option.<br>
   Note that if you use the capping, you must either set the depth of view of
   your camera to something very large (f.i. 1e9), or you could use the infinite
   mode (csInfinitePerspective) of your camera.
   <ul>
     <li>svcDefault : Default behaviour
     <li>svcAlways : Always generates caps
     <li>svcNever : Never generates caps
   </ul>
   }
  TGLShadowVolumeCapping = (svcDefault, svcAlways, svcNever);

  {: Determines when a caster should actually produce a shadow;
  <ul>
   <li>scmAlways : Caster always produces a shadow, ignoring visibility
   <li>scmVisible : Caster casts shadow if the object has visible=true
   <li>scmRecursivelyVisible : Caster casts shadow if ancestors up the hierarchy
     all have visible=true
   <li>scmParentVisible : Caster produces shadow if parent has visible=true
   <li>scmParentRecursivelyVisible : Caster casts shadow if ancestors up the hierarchy
     all have visible=true, starting from the parent (ignoring own visible setting)
  </ul> }

  TGLShadowCastingMode = (scmAlways, scmVisible, scmRecursivelyVisible,
    scmParentVisible, scmParentRecursivelyVisible);

  // TGLShadowVolumeCaster
  //
  {: Specifies an individual shadow caster.<p>
     Can be a light or an opaque object. }
  TGLShadowVolumeCaster = class(TCollectionItem)
  private
    { Private Declarations }
    FCaster: TGLBaseSceneObject;
    FEffectiveRadius: Single;
    FCapping: TGLShadowVolumeCapping;
    FCastingMode: TGLShadowCastingMode;

  protected
    { Protected Declarations }
    procedure SetCaster(const val: TGLBaseSceneObject);
    function GetGLShadowVolume: TGLShadowVolume;

    procedure RemoveNotification(aComponent: TComponent);
    function GetDisplayName: string; override;

  public
    { Public Declarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    {: Shadow casting object.<p>
       Can be an opaque object or a lightsource. }
    property Caster: TGLBaseSceneObject read FCaster write SetCaster;

    property GLShadowVolume: TGLShadowVolume read GetGLShadowVolume;

  published
    { Published Declarations }

          {: Radius beyond which the caster can be ignored.<p>
             Zero (default value) means the caster can never be ignored. }
    property EffectiveRadius: Single read FEffectiveRadius write
      FEffectiveRadius;
    {: Specifies if the shadow volume should be capped.<p>
       Capping helps solve shadowing artefacts, at the cost of performance. }
    property Capping: TGLShadowVolumeCapping read FCapping write FCapping default
      svcDefault;
    {: Determines when an object should cast a shadow or not. Typically, objects
    should only cast shadows when recursively visible. But if you're using
    dummy shadow casters which are less complex than their parent objects,
    you should use scmParentRecursivelyVisible.}
    property CastingMode: TGLShadowCastingMode read FCastingMode write
      FCastingMode default scmRecursivelyVisible;
  end;

  // TGLShadowVolumeOccluder
  //
  {: Specifies an individual shadow casting occluder.<p> }
  TGLShadowVolumeOccluder = class(TGLShadowVolumeCaster)
  published
    { Published Declarations }
    property Caster;
  end;

  // TGLShadowVolumeLight
  //
  {: Specifies an individual shadow casting light.<p> }
  TGLShadowVolumeLight = class(TGLShadowVolumeCaster)
  private
    { Private Declarations }
    FSilhouettes: TPersistentObjectList;

  protected
    { Protected Declarations }
    function GetLightSource: TGLLightSource;
    procedure SetLightSource(const ls: TGLLightSource);

    function GetCachedSilhouette(AIndex: Integer): TGLSilhouette;
    procedure StoreCachedSilhouette(AIndex: Integer; ASil: TGLSilhouette);

    {: Compute and setup scissor clipping rect for the light.<p>
       Returns true if a scissor rect was setup }
    function SetupScissorRect(worldAABB: PAABB; var rci: TRenderContextInfo):
      Boolean;

  public
    { Public Declarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure FlushSilhouetteCache;

  published
    { Published Declarations }
          {: Shadow casting lightsource.<p> }
    property LightSource: TGLLightSource read GetLightSource write
      SetLightSource;

  end;

  // TGLShadowVolumeCasters
  //
  {: Collection of TGLShadowVolumeCaster. }
  TGLShadowVolumeCasters = class(TOwnedCollection)
  private
    { Private Declarations }

  protected
    { Protected Declarations }
    function GetItems(index: Integer): TGLShadowVolumeCaster;
    procedure RemoveNotification(aComponent: TComponent);

  public
    { Public Declarations }
    function AddCaster(obj: TGLBaseSceneObject; effectiveRadius: Single = 0;
      CastingMode: TGLShadowCastingMode = scmRecursivelyVisible):
        TGLShadowVolumeCaster;
    procedure RemoveCaster(obj: TGLBaseSceneObject);
    function IndexOfCaster(obj: TGLBaseSceneObject): Integer;

    property Items[index: Integer]: TGLShadowVolumeCaster read GetItems;
      default;
  end;

  // TGLShadowVolumeOption
  //
  {: Shadow volume rendering options/optimizations.<p>
     <ul>
     <li>svoShowVolumes : make the shadow volumes visible
     <li>svoDesignVisible : the shadow are visible at design-time
     <li>svoCacheSilhouettes : cache shadow volume silhouettes, beneficial when
        some objects are static relatively to their light(s)
     <li>svoScissorClips : use scissor clipping per light, beneficial when
        lights are attenuated and don't illuminate the whole scene
     <li>svoWorldScissorClip : use scissor clipping for the world, beneficial
        when shadow receivers don't cover the whole viewer surface
     </ul> }
  TGLShadowVolumeOption = (svoShowVolumes, svoCacheSilhouettes, svoScissorClips,
    svoWorldScissorClip, svoDesignVisible);
  TGLShadowVolumeOptions = set of TGLShadowVolumeOption;

  // TGLShadowVolumeMode
  //
  {: Shadow rendering modes.<p>
     <ul>
     <li>svmAccurate : will render the scene with ambient lighting only, then
        for each light will make a diffuse+specular pass
     <li>svmDarkening : renders the scene with lighting on as usual, then darkens
        shadowed areas (i.e. inaccurate lighting, but will "shadow" objects
        that don't honour to diffuse or specular lighting)
     <li>svmOff : no shadowing will take place
     </ul> }
  TGLShadowVolumeMode = (svmAccurate, svmDarkening, svmOff);

  // TGLShadowVolume
  //
  {: Simple shadow volumes.<p>
     Shadow receiving objects are the ShadowVolume's children, shadow casters
     (opaque objects or lights) must be explicitly specified in the Casters
     collection.<p>
     Shadow volumes require that the buffer allows stencil buffers,
     GLSceneViewer.Buffer.ContextOptions contain roStencinBuffer. Without stencil
     buffers, shadow volumes will not work properly.<p>
     Another issue to look out for is the fact that shadow volume capping requires
     that the camera depth of view is either very high (fi 1e9) or that the
     camera style is csInfinitePerspective.
      }
  TGLShadowVolume = class(TGLImmaterialSceneObject)
  private
    { Private Declarations }
    FActive: Boolean;
    FRendering: Boolean;
    FLights: TGLShadowVolumeCasters;
    FOccluders: TGLShadowVolumeCasters;
    FCapping: TGLShadowVolumeCapping;
    FOptions: TGLShadowVolumeOptions;
    FMode: TGLShadowVolumeMode;
    FDarkeningColor: TGLColor;

  protected
    { Protected Declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    procedure SetActive(const val: Boolean);
    procedure SetLights(const val: TGLShadowVolumeCasters);
    procedure SetOccluders(const val: TGLShadowVolumeCasters);
    procedure SetOptions(const val: TGLShadowVolumeOptions);
    procedure SetMode(const val: TGLShadowVolumeMode);
    procedure SetDarkeningColor(const val: TGLColor);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

    procedure Assign(Source: TPersistent); override;

    procedure FlushSilhouetteCache;

  published
    { Public Declarations }
          {: Determines if shadow volume rendering is active.<p>
             When set to false, children will be rendered without any shadowing
             or multipass lighting. }
    property Active: Boolean read FActive write SetActive default True;
    {: Lights that cast shadow volumes. }
    property Lights: TGLShadowVolumeCasters read FLights write SetLights;
    {: Occluders that cast shadow volumes. }
    property Occluders: TGLShadowVolumeCasters read FOccluders write
      SetOccluders;

    {: Specifies if the shadow volume should be capped.<p>
       Capping helps solve shadowing artefacts, at the cost of performance. }
    property Capping: TGLShadowVolumeCapping read FCapping write FCapping default
      svcAlways;
    {: Shadow volume rendering options. }
    property Options: TGLShadowVolumeOptions read FOptions write SetOptions
      default [svoCacheSilhouettes, svoScissorClips];
    {: Shadow rendering mode. }
    property Mode: TGLShadowVolumeMode read FMode write SetMode default
      svmAccurate;
    {: Darkening color used in svmDarkening mode. }
    property DarkeningColor: TGLColor read FDarkeningColor write
      SetDarkeningColor;
  end;

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses SysUtils, VectorLists, GLState;

// ------------------
// ------------------ TGLShadowVolumeCaster ------------------
// ------------------

// Create
//

constructor TGLShadowVolumeCaster.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FCapping := svcDefault;
  FCastingMode := scmRecursivelyVisible;
end;

type
  // Required for Delphi 5 support.
  THackOwnedCollection = class(TOwnedCollection);

  // GetGLShadowVolume
  //

function TGLShadowVolumeCaster.GetGLShadowVolume: TGLShadowVolume;
begin
  Result := TGLShadowVolume(THackOwnedCollection(Collection).GetOwner);
end;

// Destroy
//

destructor TGLShadowVolumeCaster.Destroy;
begin
  if Assigned(FCaster) then
    FCaster.RemoveFreeNotification(GLShadowVolume);
  inherited;
end;

// Assign
//

procedure TGLShadowVolumeCaster.Assign(Source: TPersistent);
begin
  if Source is TGLShadowVolumeCaster then
  begin
    FCaster := TGLShadowVolumeCaster(Source).FCaster;
    FEffectiveRadius := TGLShadowVolumeCaster(Source).FEffectiveRadius;
    FCapping := TGLShadowVolumeCaster(Source).FCapping;
    GetGLShadowVolume.StructureChanged;
  end;
  inherited;
end;

// SetCaster
//

procedure TGLShadowVolumeCaster.SetCaster(const val: TGLBaseSceneObject);
begin
  if FCaster <> val then
  begin
    if FCaster <> nil then
      FCaster.RemoveFreeNotification(GLShadowVolume);
    FCaster := val;
    if FCaster <> nil then
      FCaster.FreeNotification(GLShadowVolume);
    GetGLShadowVolume.StructureChanged;
  end;
end;

// RemoveNotification
//

procedure TGLShadowVolumeCaster.RemoveNotification(aComponent: TComponent);
begin
  if aComponent = FCaster then
  begin
    // No point in keeping the TGLShadowVolumeCaster once the FCaster has been
    // destroyed.
    FCaster := nil;
    Free;
  end;
end;

// GetDisplayName
//

function TGLShadowVolumeCaster.GetDisplayName: string;
begin
  if Assigned(FCaster) then
  begin
    if FCaster is TGLLightSource then
      Result := '[Light]'
    else
      Result := '[Object]';
    Result := Result + ' ' + FCaster.Name;
    if EffectiveRadius > 0 then
      Result := Result + Format(' (%.1f)', [EffectiveRadius]);
  end
  else
    Result := 'nil';
end;

// ------------------
// ------------------ TGLShadowVolumeLight ------------------
// ------------------

// Create
//

constructor TGLShadowVolumeLight.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FSilhouettes := TPersistentObjectList.Create;
end;

// Destroy
//

destructor TGLShadowVolumeLight.Destroy;
begin
  FlushSilhouetteCache;
  FSilhouettes.Free;
  inherited;
end;

// FlushSilhouetteCache
//

procedure TGLShadowVolumeLight.FlushSilhouetteCache;
begin
  FSilhouettes.Clean;
end;

// Create
//

function TGLShadowVolumeLight.GetLightSource: TGLLightSource;
begin
  Result := TGLLightSource(Caster);
end;

// SetLightSource
//

procedure TGLShadowVolumeLight.SetLightSource(const ls: TGLLightSource);
begin
  SetCaster(ls);
end;

// GetCachedSilhouette
//

function TGLShadowVolumeLight.GetCachedSilhouette(AIndex: Integer):
  TGLSilhouette;
begin
  if AIndex < FSilhouettes.Count then
    Result := TGLSilhouette(FSilhouettes[AIndex])
  else
    Result := nil;
end;

// StoreCachedSilhouette
//

procedure TGLShadowVolumeLight.StoreCachedSilhouette(AIndex: Integer; ASil:
  TGLSilhouette);
begin
  while AIndex >= FSilhouettes.Count do
    FSilhouettes.Add(nil);
  if ASil <> FSilhouettes[AIndex] then
  begin
    if assigned(FSilhouettes[AIndex]) then
      FSilhouettes[AIndex].Free;
    FSilhouettes[AIndex] := ASil;
  end;
end;

// TGLShadowVolumeLight
//

function TGLShadowVolumeLight.SetupScissorRect(worldAABB: PAABB; var rci:
  TRenderContextInfo): Boolean;
var
  mv, proj, mvp: TMatrix;
  ls: TGLLightSource;
  aabb: TAABB;
  clipRect: TClipRect;
begin
  ls := LightSource;
  if (EffectiveRadius <= 0) or (not ls.Attenuated) then
  begin
    // non attenuated lights can't be clipped
    if not Assigned(worldAABB) then
    begin
      Result := False;
      Exit;
    end
    else
      aabb := worldAABB^;
  end
  else
  begin
    aabb := BSphereToAABB(ls.AbsolutePosition, EffectiveRadius);
    if Assigned(worldAABB) then
      aabb := AABBIntersection(aabb, worldAABB^);
  end;

  if PointInAABB(rci.cameraPosition, aabb) then
  begin
    // camera inside light volume radius, can't clip
    Result := False;
    Exit;
  end;

  mv := rci.modelViewMatrix^;
  glGetFloatv(GL_PROJECTION_MATRIX, @proj);

  // Calculate the window-space bounds of the light's bounding box.
  mvp := MatrixMultiply(mv, proj);

  clipRect := AABBToClipRect(aabb, mvp, rci.viewPortSize.cx,
    rci.viewPortSize.cy);

  if (clipRect.Right < 0) or (clipRect.Left > rci.viewPortSize.cx)
    or (clipRect.Top < 0) or (clipRect.Bottom > rci.viewPortSize.cy) then
  begin
    Result := False;
    Exit;
  end;

  with clipRect do
    glScissor(Round(Left), Round(Top), Round(Right - Left), Round(Bottom -
      Top));
  Result := True;
end;

// ------------------
// ------------------ TGLShadowVolumeCasters ------------------
// ------------------

// RemoveNotification
//

procedure TGLShadowVolumeCasters.RemoveNotification(aComponent: TComponent);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    Items[i].RemoveNotification(aComponent);
end;

// GetItems
//

function TGLShadowVolumeCasters.GetItems(index: Integer): TGLShadowVolumeCaster;
begin
  Result := TGLShadowVolumeCaster(inherited Items[index]);
end;

// AddCaster
//

function TGLShadowVolumeCasters.AddCaster(obj: TGLBaseSceneObject;
  effectiveRadius: Single = 0;
  CastingMode: TGLShadowCastingMode = scmRecursivelyVisible):
    TGLShadowVolumeCaster;
var
  newCaster: TGLShadowVolumeCaster;
begin
  newCaster := TGLShadowVolumeCaster(Add);
  newCaster.Caster := obj;
  newCaster.EffectiveRadius := effectiveRadius;
  newCaster.CastingMode := CastingMode;

  result := newCaster;
end;

// RemoveCaster
//

procedure TGLShadowVolumeCasters.RemoveCaster(obj: TGLBaseSceneObject);
var
  i: Integer;
begin
  i := IndexOfCaster(obj);
  if i >= 0 then
    Delete(i);
end;

// IndexOfCaster
//

function TGLShadowVolumeCasters.IndexOfCaster(obj: TGLBaseSceneObject): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Caster = obj then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

// ------------------
// ------------------ TGLShadowVolume ------------------
// ------------------

// Create
//

constructor TGLShadowVolume.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle - [osDirectDraw] + [osNoVisibilityCulling];
  FActive := True;
  FLights := TGLShadowVolumeCasters.Create(self, TGLShadowVolumeLight);
  FOccluders := TGLShadowVolumeCasters.Create(self, TGLShadowVolumeOccluder);
  FCapping := svcAlways;
  FMode := svmAccurate;
  FOptions := [svoCacheSilhouettes, svoScissorClips];
  FDarkeningColor := TGLColor.CreateInitialized(Self, VectorMake(0, 0, 0, 0.5));
end;

// Destroy
//

destructor TGLShadowVolume.Destroy;
begin
  inherited;
  FDarkeningColor.Free;
  FLights.Free;
  FOccluders.Free;
end;

// Notification
//

procedure TGLShadowVolume.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if Operation = opRemove then
  begin
    FLights.RemoveNotification(AComponent);
    FOccluders.RemoveNotification(AComponent);
  end;
  inherited;
end;

// Assign
//

procedure TGLShadowVolume.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLShadowVolume) then
  begin
    FLights.Assign(TGLShadowVolume(Source).Lights);
    FOccluders.Assign(TGLShadowVolume(Source).Occluders);
    FCapping := TGLShadowVolume(Source).FCapping;
    StructureChanged;
  end;
  inherited Assign(Source);
end;

// FlushSilhouetteCache
//

procedure TGLShadowVolume.FlushSilhouetteCache;
var
  i: Integer;
begin
  for i := 0 to Lights.Count - 1 do
    (Lights[i] as TGLShadowVolumeLight).FlushSilhouetteCache;
end;

// SetActive
//

procedure TGLShadowVolume.SetActive(const val: Boolean);
begin
  if FActive <> val then
  begin
    FActive := val;
    StructureChanged;
  end;
end;

// SetLights
//

procedure TGLShadowVolume.SetLights(const val: TGLShadowVolumeCasters);
begin
  Assert(val.ItemClass = TGLShadowVolumeLight);
  FLights.Assign(val);
  StructureChanged;
end;

// SetOccluders
//

procedure TGLShadowVolume.SetOccluders(const val: TGLShadowVolumeCasters);
begin
  Assert(val.ItemClass = TGLShadowVolumeOccluder);
  FOccluders.Assign(val);
  StructureChanged;
end;

// SetOptions
//

procedure TGLShadowVolume.SetOptions(const val: TGLShadowVolumeOptions);
begin
  if FOptions <> val then
  begin
    FOptions := val;
    if not (svoCacheSilhouettes in FOptions) then
      FlushSilhouetteCache;
    StructureChanged;
  end;
end;

// SetMode
//

procedure TGLShadowVolume.SetMode(const val: TGLShadowVolumeMode);
begin
  if FMode <> val then
  begin
    FMode := val;
    StructureChanged;
  end;
end;

// SetDarkeningColor
//

procedure TGLShadowVolume.SetDarkeningColor(const val: TGLColor);
begin
  FDarkeningColor.Assign(val);
end;

// DoRender
//

procedure TGLShadowVolume.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);

// Function that determines if an object is "recursively visible". It halts when
// * it finds an invisible ancestor (=> invisible)
// * it finds the root (=> visible)
// * it finds the shadow volume as an ancestor (=> visible)
//
// This does _not_ mean that the object is actually visible on the screen
  function DirectHierarchicalVisibility(obj: TGLBaseSceneObject): boolean;
  var
    p: TGLBaseSceneObject;
  begin
    if not Assigned(obj) then
    begin
      Result := True;
      exit;
    end;
    if not obj.Visible then
    begin
      Result := False;
      Exit;
    end;
    p := obj.Parent;
    while Assigned(p) and (p <> obj) and (p <> Self) do
    begin
      if not p.Visible then
      begin
        Result := False;
        Exit;
      end;
      p := p.Parent;
    end;
    Result := True;
  end;

var
  i, k: Integer;
  lightSource: TGLLightSource;
  lightCaster: TGLShadowVolumeLight;
  sil: TGLSilhouette;
  lightID: Cardinal;
  obj: TGLBaseSceneObject;
  caster: TGLShadowVolumeCaster;
  opaques, opaqueCapping: TList;
  silParams: TGLSilhouetteParameters;
  mat: TMatrix;
  worldAABB: TAABB;
  pWorldAABB: PAABB;
begin
  if not Active then
  begin
    inherited;
    Exit;
  end;
  if FRendering then
    Exit;
  if not (ARenderSelf or ARenderChildren) then
    Exit;
  ClearStructureChanged;
  if ((csDesigning in ComponentState) and not (svoDesignVisible in Options))
    or (Mode = svmOff)
    or (ARci.drawState = dsPicking) then
  begin
    inherited;
    Exit;
  end;
  if svoWorldScissorClip in Options then
  begin
    // compute shadow receiving world AABB in absolute coordinates
    worldAABB := Self.AxisAlignedBoundingBox;
    AABBTransform(worldAABB, AbsoluteMatrix);
    pWorldAABB := @worldAABB;
  end
  else
    pWorldAABB := nil;
  opaques := TList.Create;
  opaqueCapping := TList.Create;
  FRendering := True;
  try
    // collect visible casters
    for i := 0 to Occluders.Count - 1 do
    begin
      caster := Occluders[i];
      obj := caster.Caster;
      if Assigned(obj)
        and
        // Determine when to render this object or not
      (
        (Caster.CastingMode = scmAlways) or
        ((Caster.CastingMode = scmVisible) and obj.Visible) or
        ((Caster.CastingMode = scmRecursivelyVisible) and
          DirectHierarchicalVisibility(obj)) or
        ((Caster.CastingMode = scmParentRecursivelyVisible) and
          DirectHierarchicalVisibility(obj.Parent)) or
        ((Caster.CastingMode = scmParentVisible) and (not Assigned(obj.Parent) or
          obj.Parent.Visible))
        )
        and ((caster.EffectiveRadius <= 0)
        or (obj.DistanceTo(ARci.cameraPosition) < caster.EffectiveRadius)) then
      begin
        opaques.Add(obj);
        opaqueCapping.Add(Pointer(ord((caster.Capping = svcAlways)
          or ((caster.Capping = svcDefault)
          and (Capping = svcAlways)))));
      end
      else
      begin
        opaques.Add(nil);
        opaqueCapping.Add(nil);
      end;
    end;

    // render the shadow volumes
    ARci.GLStates.PushAttrib(cAllAttribBits);

    if Mode = svmAccurate then
    begin
      // first turn off all the shadow casting lights diffuse and specular
      for i := 0 to Lights.Count - 1 do
      begin
        lightCaster := TGLShadowVolumeLight(Lights[i]);
        lightSource := lightCaster.LightSource;
        if Assigned(lightSource) and (lightSource.Shining) then
        begin
          lightID := lightSource.LightID;
          glLightfv(lightID, GL_DIFFUSE, @NullHmgVector);
          glLightfv(lightID, GL_SPECULAR, @NullHmgVector);
        end;
      end;
    end;
    // render shadow receivers with ambient lighting

    // DanB - not sure why this doesn't render properly with these statements
    // where they were originally (after the RenderChildren call).
    ARci.ignoreBlendingRequests := True;
    ARci.ignoreDepthRequests := True;
    Self.RenderChildren(0, Count - 1, ARci);

    ARci.GLStates.DepthWriteMask := False;
    ARci.GLStates.Enable(stDepthTest);
    ARci.GLStates.SetBlendFunc(bfSrcAlpha, bfOne);
    ARci.GLStates.Disable(stAlphaTest);
    ARci.GLStates.Enable(stStencilTest);
    if GL_ARB_vertex_buffer_object then
    begin
      glBindBuffer(GL_ARRAY_BUFFER, 0);
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    end;

    // turn off *all* lights
    for i := 0 to TGLScene(ARci.scene).Lights.Count - 1 do
    begin
      lightSource := (TGLScene(ARci.scene).Lights.Items[i]) as TGLLightSource;
      if Assigned(lightSource) and lightSource.Shining then
        glDisable(lightSource.LightID);
    end;
    CheckOpenGLError;
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @NullHmgPoint);

    // render contribution of all shadow casting lights
    for i := 0 to Lights.Count - 1 do
    begin
      lightCaster := TGLShadowVolumeLight(lights[i]);
      lightSource := lightCaster.LightSource;

      if (not Assigned(lightSource)) or (not lightSource.Shining) then
        Continue;

      lightID := lightSource.LightID;

      SetVector(silParams.LightDirection,
        lightSource.SpotDirection.DirectVector);
      case lightSource.LightStyle of
        lsParallel: silParams.Style := ssParallel
      else
        silParams.Style := ssOmni;
      end;
      silParams.CappingRequired := True;

      if Assigned(pWorldAABB) or (svoScissorClips in Options) then
      begin
        if lightCaster.SetupScissorRect(pWorldAABB, ARci) then
          ARci.GLStates.Enable(stScissorTest)
        else
          ARci.GLStates.Disable(stScissorTest);
      end;

      // clear the stencil and prepare for shadow volume pass
      glClear(GL_STENCIL_BUFFER_BIT);
      ARci.GLStates.SetStencilFunc(cfAlways, 0, 255);
      ARci.GLStates.DepthFunc := cfLess;

      if svoShowVolumes in Options then
      begin
        glColor3f(0.05 * i, 0.1, 0);
        ARci.GLStates.Enable(stBlend);
      end
      else
      begin
        ARci.GLStates.SetGLColorWriting(False);
        ARci.GLStates.Disable(stBlend);
      end;
      ARci.GLStates.Enable(stCullFace);

      ARci.GLStates.Disable(stLighting);
      glEnableClientState(GL_VERTEX_ARRAY);
      ARci.GLStates.SetPolygonOffset(1, 1);

      // for all opaque shadow casters
      for k := 0 to opaques.Count - 1 do
      begin
        obj := TGLBaseSceneObject(opaques[k]);
        if obj = nil then
          Continue;

        SetVector(silParams.SeenFrom,
          obj.AbsoluteToLocal(lightSource.AbsolutePosition));

        sil := lightCaster.GetCachedSilhouette(k);
        if (not Assigned(sil)) or (not CompareMem(@sil.Parameters, @silParams,
          SizeOf(silParams))) then
        begin
          sil := obj.GenerateSilhouette(silParams);
          sil.Parameters := silParams;
          // extrude vertices to infinity
          sil.ExtrudeVerticesToInfinity(silParams.SeenFrom);
        end;
        if Assigned(sil) then
          try
            // render the silhouette
            glPushMatrix;

            glLoadMatrixf(PGLFloat(ARci.modelViewMatrix));
            mat := obj.AbsoluteMatrix;
            glMultMatrixf(@mat);

            glVertexPointer(4, GL_FLOAT, 0, sil.Vertices.List);

            if Boolean(opaqueCapping[k]) then
            begin
              // z-fail
              if GL_EXT_compiled_vertex_array then
                glLockArraysEXT(0, sil.Vertices.Count);

              ARci.GLStates.CullFaceMode := cmFront;
              ARci.GLStates.SetStencilOp(soKeep, soIncr, soKeep);

              with sil do
              begin
                glDrawElements(GL_QUADS, Indices.Count, GL_UNSIGNED_INT,
                  Indices.List);
                ARci.GLStates.Enable(stPolygonOffsetFill);
                glDrawElements(GL_TRIANGLES, CapIndices.Count, GL_UNSIGNED_INT,
                  CapIndices.List);
                ARci.GLStates.Disable(stPolygonOffsetFill);
              end;

              ARci.GLStates.CullFaceMode := cmBack;
              ARci.GLStates.SetStencilOp(soKeep, soDecr, soKeep);

              with sil do
              begin
                glDrawElements(GL_QUADS, Indices.Count, GL_UNSIGNED_INT,
                  Indices.List);
                ARci.GLStates.Enable(stPolygonOffsetFill);
                glDrawElements(GL_TRIANGLES, CapIndices.Count, GL_UNSIGNED_INT,
                  CapIndices.List);
                ARci.GLStates.Disable(stPolygonOffsetFill);
              end;

              if GL_EXT_compiled_vertex_array then
                glUnlockArraysEXT;
            end
            else
            begin
              // z-pass
              ARci.GLStates.CullFaceMode := cmBack;
              ARci.GLStates.SetStencilOp(soKeep, soKeep, soIncr);

              glDrawElements(GL_QUADS, sil.Indices.Count, GL_UNSIGNED_INT,
                sil.Indices.List);

              ARci.GLStates.CullFaceMode := cmFront;
              ARci.GLStates.SetStencilOp(soKeep, soKeep, soDecr);

              glDrawElements(GL_QUADS, sil.Indices.Count, GL_UNSIGNED_INT,
                sil.Indices.List);
            end;

            glPopMatrix;
          finally
            if (svoCacheSilhouettes in Options) and (not (osDirectDraw in
              ObjectStyle)) then
              lightCaster.StoreCachedSilhouette(k, sil)
            else
              sil.Free;
          end;
      end;

      glDisableClientState(GL_VERTEX_ARRAY);

      // re-enable light's diffuse and specular, but no ambient
      glEnable(lightID);
      glLightfv(lightID, GL_AMBIENT, @NullHmgVector);
      glLightfv(lightID, GL_DIFFUSE, lightSource.Diffuse.AsAddress);
      glLightfv(lightID, GL_SPECULAR, lightSource.Specular.AsAddress);

      ARci.GLStates.SetGLColorWriting(True);
      Arci.GLStates.SetStencilOp(soKeep, soKeep, soKeep);

      ARci.GLStates.Enable(stBlend);

      Arci.GLStates.CullFaceMode := cmBack;

      if Mode = svmAccurate then
      begin
        Arci.GLStates.SetStencilFunc(cfEqual, 0, 255);
        ARci.GLStates.DepthFunc := cfEqual;
  {removed?}  //glEnable(GL_LIGHTING);
        Self.RenderChildren(0, Count - 1, ARci);
      end
      else
      begin
        ARci.GLStates.SetStencilFunc(cfNotEqual, 0, 255);

        ARci.GLStates.DepthFunc := cfAlways;
        ARci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);

        glPushMatrix;
        glLoadIdentity;
        glMatrixMode(GL_PROJECTION);
        glPushMatrix;
        glLoadIdentity;
        gluOrtho2D(0, 1, 1, 0);

        glColor4fv(FDarkeningColor.AsAddress);
        glBegin(GL_QUADS);
        glVertex2f(0, 0);
        glVertex2f(0, 1);
        glVertex2f(1, 1);
        glVertex2f(1, 0);
        glEnd;

        glPopMatrix;
        glMatrixMode(GL_MODELVIEW);
        glPopMatrix;

        Arci.GLStates.SetBlendFunc(bfSrcAlpha, bfOne);
      end;

      // disable light, but restore its ambient component
      glDisable(lightID);
      glLightfv(lightID, GL_AMBIENT, lightSource.Ambient.AsAddress);
    end;

    // restore OpenGL state
    ARci.GLStates.PopAttrib;

    ARci.ignoreBlendingRequests := False;
    ARci.ignoreDepthRequests := False;
  finally
    FRendering := False;
    opaques.Free;
    opaqueCapping.Free;
  end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TGLShadowVolume]);

end.

