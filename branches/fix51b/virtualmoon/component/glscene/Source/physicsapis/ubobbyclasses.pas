unit ubobbyclasses;

interface

uses
  classes, sysutils, odeimport, math;

const
  cLIQUID_DENSITY = 3;
  cGRAVITY = -9.91;

type
  TBobby = class;
  TBobbyHandler = class;

  // TBobby is a particle that handles buoyancy. It bobs in the water, hence
  // the name
  TOnGetDepthAndNormal = procedure (Bobby: TBobby) of object;

  TBobby = class
    // The position of the Bobby, relative to the body. This will be modified
    // by the bobbys location and rotation
    Position : TdVector3;

    // TotalVolume is stored to speed up calculations. It's updated whenever
    // radius is altered
    TotalVolume : single;

    // Keeps track of how much of the bobbys volume is in submerged
    SubmergedVolume : single;

    // A Bobby must be connected to a body that it can act upon
    Body : PdxBody;

    // The current depth of the bobby
    CenterDepth : single;

    // WaterNormal is a vector that describes the normal of the water at the
    // location of the bobby
    WaterNormal : TdVector3;

    // This is the force as it has been calculated by the bobby, depending on
    // the displacement
    BuoyancyForce : TdVector3;

    // This is the position that the force acts upon
    BuoyancyForceCenter : TdVector3;

    // BobbyHandler is responsible for handling this bobby
    BobbyHandler : TBobbyHandler;

    // WorldPosition holds the world position of the bobby
    WorldPosition : TdVector3;

    // OldWorldPosition is used to calculate where the Bobby was before, for
    // calculating drag
    OldWorldPosition : TdVector3;

    // DragForce is the drag that the bobby feels due to it's speed in the
    // liquid. If the bobby isn't submerged, the drag is zero
    DragForce : TdVector3;
    BobbySpeed : TdVector3;

    // DragCoefficient is used to calculate drag forces
    DragCoefficient : single;

    // SurfaceArea is the area of the surface
    SurfaceArea : single;

    // SubmergedSurfaceArea is the area that's submerged
    SubmergedSurfaceArea : single;

    // Update
    procedure Update(DeltaTime : single);

    // UpdateWorldPosition updates the world position
    procedure UpdateWorldPosition;

    procedure UpdateSpeed(DeltaTime : single);

    // Calculate the drag force
    procedure CalcDragForce;

    // Calculate the magnitude and direction of the buoyancy force
    procedure CalcBuoyancyForce;

    // Apply the buoyancy force to the body
    procedure ApplyForces;

    constructor Create(BobbyHandler : TBobbyHandler);
    destructor Destroy; override;
  private
    FRadius: single;
    procedure SetRadius(const Value: single);

  public
    // The Radius of the bobby determines it's buoyancy. The amount of liquid
    // it displaces is the same as the volume
    property Radius : single read FRadius write SetRadius;
  end;

  TBobbyList = class(TList)
  private
    function GetItems(i: integer): TBobby;
    procedure SetItems(i: integer; const Value: TBobby);
  public
    property Items[i : integer] : TBobby read GetItems write SetItems; default;
  end;

  TBobbyHandler = class
  private
    FOnGetDepthAndNormal: TOnGetDepthAndNormal;
    procedure SetOnGetDepthAndNormal(const Value: TOnGetDepthAndNormal);
  public
    // Bobbies is a list of all handled bobbies (duh!)
    Bobbies : TBobbyList;

    // LiquidDensity determines the density of the liquid that the bobby is
    // suspended in
    LiquidDensity : single;

    // Gravity determines how much the mass of the water weights. This should
    // be the same gravity as ode uses.
    gravity : tdvector3;

    // AddBobby adds a new bobby to the bobbies list
    procedure AddBobby(Bobby : TBobby);

    // AddBobby removes a bobby to the bobbies list
    procedure RemoveBobby(Bobby : TBobby);

    // Free all bobbies and clear the list
    procedure ClearBobbies;

    // UpdateBobbies will
    // * Update each bobby world position
    // * Call GetDepthAndNormal with each bobby
    // * Calculate the buoyancy forces
    // * Calculate the drag forces
    // * Add the forces to the bodies
    procedure UpdateBobbies(DeltaTime : single);

    property OnGetDepthAndNormal : TOnGetDepthAndNormal read FOnGetDepthAndNormal write SetOnGetDepthAndNormal;

    function GetSubmergedAmount : single;

    constructor Create;
    destructor Destroy; override;
  end;

  function SphereCapVolume(const SphereRadius, CapHeight : single) : single;
  function SphereCapCentroidHeight(SphereRadius, CapHeight : single) : single;

  function SubmergedSphereCapVolume(const SphereRadius, CenterDepth : single) : single;
  function SubmergedSphereCapCentroidHeight(SphereRadius, CenterDepth : single) : single;

implementation

const
  c4PiDiv3 = 4 * pi / 3;
  cPiDiv3 = pi / 3;

function SphereCapVolume(const SphereRadius, CapHeight : single) : single;
begin
  // Calculates the volume of a sphere cap, as clipped by a plane
  // SphereRadius is the radius of the sphere
  // CapHeight is the height from the _TOP_ of the sphere to the clipping plane

{
See http://mathworld.wolfram.com/SphericalCap.html and
    http://mathforum.org/dr.math/faq/formulas/faq.sphere.html and
    http://mathforum.org/library/drmath/view/55253.html

     V_cap = 2/3 pi r^2 h - 1/3 pi (2rh - h^2)(r - h)
           = 2/3 pi r^2 h - 1/3 pi (2r^2h - 3rh^2 +h^3)
           = 1/3 pi h [2r^2 - 2r^2 + 3rh - h^2]
           = 1/3 pi h (3rh - h^2)
***

  Both sources seem to agree ;)
}

  Assert(Abs(CapHeight)<=2 * SphereRadius,
    Format('Cap must be smaller than sphere diameter, Abs(%f) > 2 * %f!',[CapHeight, SphereRadius]));//}

  // Calculate the volume
  result := cPiDiv3 * CapHeight * (3 * SphereRadius * CapHeight - CapHeight * CapHeight)
end;

function SphereCapCentroidHeight(SphereRadius, CapHeight : single) : single;
begin
  // This function from http://mathworld.wolfram.com/SphericalCap.html,
  //  (Harris and Stocker 1998, p. 107).

  Assert(Abs(CapHeight)<=2 * SphereRadius,
    Format('Cap must be smaller than sphere diameter, Abs(%f) > 2 * %f!',[CapHeight, SphereRadius]));//}

  result := 3*sqr(2 * SphereRadius - CapHeight)/(4 * (3 * SphereRadius - CapHeight));
end;

function SubmergedSphereCapVolume(const SphereRadius, CenterDepth : single) : single;
begin
  // Is it not submerged at all?
  if CenterDepth >= SphereRadius then
    result := 0
  // Is it fully submerged?
  else if CenterDepth <= -SphereRadius then
    result := c4PiDiv3 * SphereRadius * SphereRadius * SphereRadius
  else
    // Partially submerged, the amount submerged is calculated by the cap volume
    result := SphereCapVolume(SphereRadius, SphereRadius - CenterDepth );
end;

function SubmergedSphereCapCentroidHeight(SphereRadius, CenterDepth : single) : single;
begin
  // Is it not submerged at all? If it's not submerged, it has no centroid!
  if CenterDepth >= SphereRadius then
    result := 0
  // Is it fully submerged?
  else if CenterDepth <= -SphereRadius then
    result := 0
  else
    // Partially submerged, the amount submerged is calculated by the cap volume
    result := -SphereCapCentroidHeight(SphereRadius, (SphereRadius - CenterDepth));
end;

{ TBobby }

procedure TBobby.ApplyForces;
begin
  Assert(Assigned(Body),'Bobby has no body!');

  dBodyAddForceAtPos(Body,
    BuoyancyForce[0] + DragForce[0], BuoyancyForce[1] + DragForce[1], BuoyancyForce[2] + DragForce[2],
    BuoyancyForceCenter[0], BuoyancyForceCenter[1], BuoyancyForceCenter[2]);
end;

procedure TBobby.CalcBuoyancyForce;
var
  DisplacementMass : single;
  Depth : single;
  g : single;
begin
  // PLEASE NOTE: This function currently only handles gravity that lies along
  // z!
  
  // Make sure there's a body to apply the force to
  Assert(Assigned(Body),'Bobby has no body!');

  // The force center will go through the center of the Bobby, but it will be
  // moved ackording to the normal of the water and the depth of the water
  // around the bobby
  Depth := SubmergedSphereCapCentroidHeight(Radius, CenterDepth);

  BuoyancyForceCenter[0] := WorldPosition[0] + Depth * WaterNormal[0];
  BuoyancyForceCenter[1] := WorldPosition[1] + Depth * WaterNormal[1];
  BuoyancyForceCenter[2] := WorldPosition[2] + Depth * WaterNormal[2];//}

  // Calculate displaced volume
  SubmergedVolume := SubmergedSphereCapVolume(Radius, CenterDepth);

  if SubmergedVolume = 0 then
    SubmergedSurfaceArea := 0
  else if CenterDepth<-Radius then
    SubmergedSurfaceArea := SurfaceArea
  else
    SubmergedSurfaceArea := SurfaceArea * (Radius * 2 - CenterDepth);

  // Calculate displacement mass
  DisplacementMass := SubmergedVolume * BobbyHandler.LiquidDensity;

  // The lifting force is always opposing gravity
  BuoyancyForce[0] := - BobbyHandler.Gravity[0] * DisplacementMass;
  BuoyancyForce[1] := - BobbyHandler.Gravity[1] * DisplacementMass;
  BuoyancyForce[2] := - BobbyHandler.Gravity[2] * DisplacementMass;//}

  g := BobbyHandler.Gravity[0];

  if abs(BobbyHandler.Gravity[1]) > abs(g) then
    g := BobbyHandler.Gravity[1];

  if abs(BobbyHandler.Gravity[2]) > abs(g) then
    g := BobbyHandler.Gravity[2];

  // The sideways moving force is proportional to the slope of the water normal
  if (BobbyHandler.Gravity[0]=0) then
    BuoyancyForce[0] := BuoyancyForce[0] - WaterNormal[0] * DisplacementMass * g;

  if (BobbyHandler.Gravity[1]=0) then
    BuoyancyForce[1] := BuoyancyForce[1] - WaterNormal[1] * DisplacementMass * g;

  if (BobbyHandler.Gravity[2]=0) then
    BuoyancyForce[2] := BuoyancyForce[2] - WaterNormal[2] * DisplacementMass * g;//}
end;

procedure TBobby.CalcDragForce;
var
  ForceMagnitude : single;
  Speed, Speed2 : single;
begin
  // If this is the first time, ignore drag!
  if OldWorldPosition[0] = -10e5 then exit;

  // If the bobby isn't submerged, the force is zero
  if SubmergedVolume=0 then
  begin
    DragForce[0] := 0;
    DragForce[1] := 0;
    DragForce[2] := 0;
    exit;
  end;

  Speed2 := (sqr(BobbySpeed[0]) + sqr(BobbySpeed[1]) + sqr(BobbySpeed[2]));
  Speed := sqrt(Speed2);

  // Fd = Cd / 2 * p * V^2 * A
  // Cd = Drag coefficient
  // p = density of medium
  // v = flow speed
  // A = cross sectional area
  ForceMagnitude :=
    - DragCoefficient / 2 * BobbyHandler.LiquidDensity * Speed2 * SubmergedSurfaceArea;

  // Preset drag force to a normalized version of bobby speed
  DragForce[0] := BobbySpeed[0] / Speed * ForceMagnitude;
  DragForce[1] := BobbySpeed[1] / Speed * ForceMagnitude;
  DragForce[2] := BobbySpeed[2] / Speed * ForceMagnitude;
end;

constructor TBobby.Create(BobbyHandler: TBobbyHandler);
begin
  self.BobbyHandler := BobbyHandler;
  BobbyHandler.AddBobby(self);

  // Note that the bobby hasn't ever had a position before, so that the speed
  // calculation doesn't come out bonkers
  WorldPosition[0] := -10e5;
  OldWorldPosition[0] := -10e5;

  DragCoefficient := 1.5;
end;

destructor TBobby.Destroy;
begin
  BobbyHandler.RemoveBobby(self);

  inherited;
end;

procedure TBobby.SetRadius(const Value: single);
begin
  FRadius := Value;
  TotalVolume := 4 / 3 * pi * Value * Value * Value;
  SurfaceArea := pi * sqr(Value);
end;

procedure TBobby.Update(DeltaTime : single);
begin
  // If the body that the bobby is connected to is disabled, then we can jump out
  // here
  if (dBodyIsEnabled(Body)=0) then exit;

  // * Update bobby world position
  UpdateWorldPosition;

  // * Update bobby speed
  UpdateSpeed(DeltaTime);

  // * Call GetDepthAndNormal with each bobby
  BobbyHandler.OnGetDepthAndNormal(self);

  // * Calculate the buoyancy forces
  CalcBuoyancyForce;

  // * Calculate the drag forces
  CalcDragForce;

  // * Add the forces to the bodies
  ApplyForces;
end;

procedure TBobby.UpdateSpeed(DeltaTime: single);
begin
  // Also update the speed of the bobby
  BobbySpeed[0] := (WorldPosition[0] - OldWorldPosition[0]) / DeltaTime;
  BobbySpeed[1] := (WorldPosition[1] - OldWorldPosition[1]) / DeltaTime;
  BobbySpeed[2] := (WorldPosition[2] - OldWorldPosition[2]) / DeltaTime;
end;

procedure TBobby.UpdateWorldPosition;
var
  pos : PdVector3;
begin
  OldWorldPosition := WorldPosition;

  // First, calculate the actual center of the bobby
  dBodyVectorToWorld(Body, Position[0], Position[1], Position[2], WorldPosition);

  pos := dBodyGetPosition(Body);


  Assert(abs(pos[2])>-10e10 , Format('Bad body position : %f, %f, %f',[pos[0], pos[1], pos[2]]));

  WorldPosition[0] := WorldPosition[0] + Pos[0];
  WorldPosition[1] := WorldPosition[1] + Pos[1];
  WorldPosition[2] := WorldPosition[2] + Pos[2];
end;

{ TBobbyHandler }

procedure TBobbyHandler.AddBobby(Bobby: TBobby);
begin
  Bobbies.Add(Bobby);
end;

procedure TBobbyHandler.ClearBobbies;
begin
  try
    while Bobbies.Count>0 do
      Bobbies[Bobbies.Count-1].Free;
  finally
    Bobbies.Clear;
  end;
end;

constructor TBobbyHandler.Create;
begin
  Bobbies := TBobbyList.Create;
  LiquidDensity := cLIQUID_DENSITY;
  Gravity[0] := 0;
  Gravity[1] := 0;
  Gravity[2] := cGRAVITY;
end;

destructor TBobbyHandler.Destroy;
begin
  ClearBobbies;
  FreeAndNil(Bobbies);

  inherited;
end;

function TBobbyHandler.GetSubmergedAmount: single;
var
  i : integer;
  TotVolume, TotSubmergedVolume : single;
begin
  TotVolume := 0;
  TotSubmergedVolume := 0;
  for i := 0 to Bobbies.Count-1 do
    with Bobbies[i] do
    begin
      TotVolume := TotVolume + TotalVolume;
      TotSubmergedVolume := TotSubmergedVolume + SubmergedVolume;
    end;

  if TotVolume = 0 then
    result := 0
  else
    result := TotSubmergedVolume / TotVolume;
end;

procedure TBobbyHandler.RemoveBobby(Bobby: TBobby);
begin
  Bobbies.Remove(Bobby);
end;

procedure TBobbyHandler.SetOnGetDepthAndNormal(
  const Value: TOnGetDepthAndNormal);
begin
  FOnGetDepthAndNormal := Value;
end;

procedure TBobbyHandler.UpdateBobbies(DeltaTime : single);
var
  i : integer;
  Bobby : TBobby;
begin
  Assert(Assigned(OnGetDepthAndNormal),'OnGetDepthAndNormal must be assigned!');
  for i := 0 to Bobbies.Count-1 do
  begin
    // Retrieve the bobby for easy access
    Bobby := Bobbies[i];

    // Update the bobby
    Bobby.Update(DeltaTime);
  end;
end;

{ TBobbyList }

function TBobbyList.GetItems(i: integer): TBobby;
begin
  result := Get(i);
end;

procedure TBobbyList.SetItems(i: integer; const Value: TBobby);
begin
  Put(i, Value);
end;
end.
