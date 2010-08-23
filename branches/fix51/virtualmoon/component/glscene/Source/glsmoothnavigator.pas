//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSmoothNavigator<p>

     An extention of TGLNavigator, which allows to move objects with inertia
   Note: it is not completely FPS-independant. Only Moving code is, but
   MoveAroundTarget, Turn[Vertical/Horizontal] and AdjustDistanceTo[..] is not.

     Don't know why, but when I make their code identical, these function stop
   working completely. So you probably have to call the AutoScaleParameters
   procedure once in a while for it to adjust to the current framerate.
   If someone knows a better way to solve this issue, please contact me via
   glscene newsgroups.<p>


   <b>History : </b><font size=-1><ul>
      <li>25/02/07 - DaStr - Added the AdjustDistanceTo[..] procedures
      <li>23/02/07 - DaStr - Initial version (contributed to GLScene)


    TODO:
      1) Scale "Old values" too, when callin the Scale parameter procedure to
         avoid the temporary "freeze" of controls.
      2) AddImpulse procedures.



    Previous version history:
        v1.0    10 December  '2005  Creation
        v1.0.2  11 December  '2005  TurnMaxAngle added
        v1.1    04 March     '2006  Inertia became FPS-independant
                                    TGLSmoothNavigatorParameters added
        v1.1.6  18 February  '2007  Merged with GLInertedUserInterface.pas
                                    All parameters moved into separate classes
                                    Added MoveAroudTargetWithInertia
        v1.2    23 February  '2007  Finally made it trully FPS-independant
                                    Added default values to every property
                                    Contributed to GLScene
}

unit GLSmoothNavigator;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes,

  // GLScene
  GLNavigator, VectorGeometry, GLScene, GLCrossPlatform, GLCoordinates, GLScreen;

type
	{: TGLNavigatorAdjustDistanceParameters is wrapper for all parameters that
       affect how the AdjustDisanceTo[...] methods work<p>
  }
  TGLNavigatorAdjustDistanceParameters = class(TPersistent)
  private
    FOwner: TPersistent;
    OldDistanceRatio: Single;
    FInertia: Single;
    FSpeed: Single;
    FImpulseSpeed: Single;
    function StoreInertia: Boolean;
    function StoreSpeed: Boolean;
    function StoreImpulseSpeed: Boolean;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure ScaleParameters(const Value: Single); virtual;

    procedure AddImpulse(const Impulse: Single); virtual;
  published
    property Inertia: Single read FInertia write FInertia stored StoreInertia;
    property Speed: Single read FSpeed write FSpeed stored StoreSpeed;
    property ImpulseSpeed: Single read FImpulseSpeed write FImpulseSpeed stored StoreImpulseSpeed;
  end;


	{: TGLNavigatorInertiaParameters is wrapper for all parameters that affect the
       smoothness of movement<p>
  }
  TGLNavigatorInertiaParameters = class(TPersistent)
  private
    FOwner: TPersistent;

    OldTurnHorizontalAngle: Single;
    OldTurnVerticalAngle: Single;

    OldMoveForwardDistance: Single;
    OldStrafeHorizontalDistance: Single;
    OldStrafeVerticalDistance: Single;

    FTurnInertia: Single;
    FTurnSpeed: Single;
    FTurnMaxAngle: Single;
    FMovementAcceleration: Single;
    FMovementInertia: Single;
    FMovementSpeed: Single;

    function StoreTurnMaxAngle: Boolean;
    function StoreMovementAcceleration: Boolean;
    function StoreMovementInertia: Boolean;
    function StoreMovementSpeed: Boolean;
    function StoreTurnInertia: Boolean;
    function StoreTurnSpeed: Boolean;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure ScaleParameters(const Value: Single); virtual;
  published
    property MovementAcceleration: Single read FMovementAcceleration write FMovementAcceleration stored StoreMovementAcceleration;
    property MovementInertia: Single read FMovementInertia write FMovementInertia stored StoreMovementInertia;
    property MovementSpeed: Single read FMovementSpeed write FMovementSpeed stored StoreMovementSpeed;

    property TurnMaxAngle: Single read FTurnMaxAngle write FTurnMaxAngle stored StoreTurnMaxAngle;
    property TurnInertia: Single read FTurnInertia write FTurnInertia stored StoreTurnInertia;
    property TurnSpeed: Single read FTurnSpeed write FTurnSpeed stored StoreTurnSpeed;
  end;


  {: TGLNavigatorGeneralParameters is a wrapper for all general inertia parameters.

     These properties mean that if ExpectedMaxFPS is 100, FAutoScaleMin is 0.1,
     FAutoScaleMax is 0.75 then the "safe range" for it to change is [10..75].
     If these bounds are violated, then ExpectedMaxFPS is automaticly increased
     or decreased by AutoScaleMult.
  }
  TGLNavigatorGeneralParameters = class(TPersistent)
  private
    FOwner: TPersistent;
    FAutoScaleMin: Single;
    FAutoScaleMax: Single;
    FAutoScaleMult: Single;

    function StoreAutoScaleMax: Boolean;
    function StoreAutoScaleMin: Boolean;
    function StoreAutoScaleMult: Boolean;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoScaleMin: Single read FAutoScaleMin write FAutoScaleMin stored StoreAutoScaleMin;
    property AutoScaleMax: Single read FAutoScaleMax write FAutoScaleMax stored StoreAutoScaleMax;
    property AutoScaleMult: Single read FAutoScaleMult write FAutoScaleMult stored StoreAutoScaleMult;
  end;


  {: TGLNavigatorMoveAroundParameters is a wrapper for all parameters that
      effect how the TGLBaseSceneObject.MoveObjectAround() procedure works
  }
  TGLNavigatorMoveAroundParameters = class(TPersistent)
  private
    FOwner: TPersistent;
    FTargetObject: TGLBaseSceneObject;

    FOldPitchInertiaAngle : Single;
    FOldTurnInertiaAngle  : Single;

    FPitchSpeed : Single;
    FTurnSpeed  : Single;
    FInertia          : Single;
    FMaxAngle         : Single;

    function StoreInertia: Boolean;
    function StoreMaxAngle: Boolean;
    function StorePitchSpeed: Boolean;
    function StoreTurnSpeed: Boolean;
    procedure SetTargetObject(const Value: TGLBaseSceneObject);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure ScaleParameters(const Value: Single); virtual;
  published
    property Inertia: Single read FInertia write FInertia stored StoreInertia;
    property MaxAngle: Single read FMaxAngle write FMaxAngle stored StoreMaxAngle;
    property PitchSpeed: Single read FPitchSpeed write FPitchSpeed stored StorePitchSpeed;
    property TurnSpeed: Single read FTurnSpeed write FTurnSpeed stored StoreTurnSpeed;
    property TargetObject: TGLBaseSceneObject read FTargetObject write SetTargetObject;
  end;


	// TGLSmoothNavigator
	//
	{: TGLSmoothNavigator is the component for moving a TGLBaseSceneObject, and all
       classes based on it, this includes all the objects from the Scene Editor.<p>

	   The four calls to get you started are
      <ul>
  	 <li>TurnHorisontal : it turns left and right.
	   <li>TurnVertical : it turns up and down.
	   <li>MoveForward :	moves back and forth.
     <li>FlyForward : moves back and forth in the movingobject's direction
      </ul>
	   The three properties to get you started is
      <ul>
	   <li>MovingObject : The Object that you are moving.
	   <li>UseVirtualUp : When UseVirtualUp is set you navigate Quake style. If it isn't
   	                    it's more like Descent.
	   <li>AngleLock : Allows you to block the Vertical angles. Should only be used in
			               conjunction with UseVirtualUp.
	   <li>MoveUpWhenMovingForward : Changes movement from Quake to Arcade Airplane...
                                   (no tilt and flying)
	   <li>InvertHorizontalSteeringWhenUpsideDown : When using virtual up, and vertically
                      rotating beyond 90 degrees, will make steering seem inverted,
                      so we "invert" back to normal.
      </ul>
   }
  TGLSmoothNavigator = class(TGLNavigator)
  private
    FMaxExpectedDeltaTime: Single;
    FInertiaParams: TGLNavigatorInertiaParameters;
    FGeneralParams: TGLNavigatorGeneralParameters;
    FMoveAroundParams: TGLNavigatorMoveAroundParameters;
    FAdjustDistanceParams: TGLNavigatorAdjustDistanceParameters;
    procedure SetInertiaParams(const Value: TGLNavigatorInertiaParameters);
    function StoreMaxExpectedDeltaTime: Boolean;
    procedure SetGeneralParams(const Value: TGLNavigatorGeneralParameters);
    procedure SetMoveAroundParams(const Value: TGLNavigatorMoveAroundParameters);
    procedure SetAdjustDistanceParams(const Value: TGLNavigatorAdjustDistanceParameters);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    //: Constructors-destructors
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //: From TGLNavigator
    procedure SetObject(Value: TGLBaseSceneObject); override;

    //: InertiaParams
    procedure TurnHorizontal(Angle: Single; DeltaTime: Single); virtual;
    procedure TurnVertical(Angle: Single; DeltaTime: Single); virtual;
    procedure FlyForward(const Plus, Minus: Boolean; DeltaTime: Single; const Accelerate: Boolean = False); virtual;
    procedure MoveForward(const Plus, Minus: Boolean; DeltaTime: Single; const Accelerate: Boolean = False); virtual;
    procedure StrafeHorizontal(const Plus, Minus: Boolean; DeltaTime: Single; const Accelerate: Boolean = False); virtual;
    procedure StrafeVertical(const Plus, Minus: Boolean; DeltaTime: Single; const Accelerate: Boolean = False); virtual;

    //: MoveAroundParams
    procedure MoveAroundTarget(const PitchDelta, TurnDelta : Single; const DeltaTime: Single); virtual;
    procedure MoveObjectAround(const AObject: TGLBaseSceneObject; PitchDelta, TurnDelta : Single; DeltaTime: Single); virtual;

    //: AdjustDistanceParams
    procedure AdjustDistanceToPoint(const  APoint: TVector; const DistanceRatio : Single; DeltaTime: Single); virtual;
    procedure AdjustDistanceToTarget(const DistanceRatio : Single; const DeltaTime: Single); virtual;

    //: GeneralParams
      {: In ScaleParameters, Value should be around 1. }
    procedure ScaleParameters(const Value: Single); virtual;
    procedure AutoScaleParameters(const FPS: Single); virtual;
    procedure AutoScaleParametersUp(const FPS: Single); virtual;
 published
    property MaxExpectedDeltaTime: Single read FMaxExpectedDeltaTime write FMaxExpectedDeltaTime stored StoreMaxExpectedDeltaTime;
    property InertiaParams: TGLNavigatorInertiaParameters read FInertiaParams write SetInertiaParams;
    property GeneralParams: TGLNavigatorGeneralParameters read FGeneralParams write SetGeneralParams;
    property MoveAroundParams: TGLNavigatorMoveAroundParameters read FMoveAroundParams write SetMoveAroundParams;
    property AdjustDistanceParams: TGLNavigatorAdjustDistanceParameters read FAdjustDistanceParams write SetAdjustDistanceParams;
  end;


	// TGLSmoothUserInterface
	//
	{: TGLSmoothUserInterface is the component which reads the userinput and transform it into action.<p>
      <ul>
	   <li>Mouselook(deltaTime: double) : handles mouse look... Should be called
                           in the Cadencer event. (Though it works everywhere!)
      </ul>
	   The four properties to get you started are:
      <ul>
	   <li>InvertMouse     : Inverts the mouse Y axis.
	   <li>AutoUpdateMouse : If enabled (by defaul), than handles all mouse updates.
	   <li>GLNavigator     : The Navigator which receives the user movement.
	   <li>GLVertNavigator : The Navigator which if set receives the vertical user
                           movement. Used mostly for cameras....
      </ul>
   }
  TGLSmoothUserInterface = class(TComponent)
  private
    FAutoUpdateMouse: Boolean;
    FMouseLookActive: Boolean;
    FSmoothNavigator: TGLSmoothNavigator;
    FSmoothVertNavigator: TGLSmoothNavigator;
    FInvertMouse: Boolean;
    FOriginalMousePos: TGLCoordinates2;
    procedure SetSmoothNavigator(const Value: TGLSmoothNavigator); virtual;
    procedure SetOriginalMousePos(const Value: TGLCoordinates2); virtual;
    procedure SetSmoothVertNavigator(const Value: TGLSmoothNavigator); virtual;
    procedure SetMouseLookActive(const Value: Boolean); virtual;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure TurnHorizontal(const Angle : Single; const DeltaTime: Single); virtual;
    procedure TurnVertical(const Angle : Single; const DeltaTime: Single); virtual;
    procedure MouseLookActiveToggle; virtual;

    function MouseLook(const DeltaTime: Single): Boolean; overload;
    function MouseLook(const NewXY: TGLPoint; const DeltaTime: Single): Boolean; overload;
    function MouseLook(const NewX, NewY: Integer; const DeltaTime: Single): Boolean; overload;
  published
    property AutoUpdateMouse: Boolean read FAutoUpdateMouse write FAutoUpdateMouse default True;
    property MouseLookActive: Boolean read FMouseLookActive write SetMouseLookActive default False;
    property SmoothVertNavigator: TGLSmoothNavigator read FSmoothVertNavigator write SetSmoothVertNavigator;
    property SmoothNavigator: TGLSmoothNavigator read FSmoothNavigator write SetSmoothNavigator;
    property InvertMouse: Boolean read FInvertMouse write FInvertMouse default False;
    property OriginalMousePos: TGLCoordinates2 read FOriginalMousePos write SetOriginalMousePos;
  end;

implementation

const
  EPS =  0.001;
  EPS2 = 0.0001;

{ TGLSmoothNavigator }

constructor TGLSmoothNavigator.Create(AOwner: TComponent);
begin
  inherited;
  FMaxExpectedDeltaTime := 0.001;
  FInertiaParams := TGLNavigatorInertiaParameters.Create(Self);
  FGeneralParams := TGLNavigatorGeneralParameters.Create(Self);
  FMoveAroundParams := TGLNavigatorMoveAroundParameters.Create(Self);
  FAdjustDistanceParams := TGLNavigatorAdjustDistanceParameters.Create(Self);
end;

destructor TGLSmoothNavigator.Destroy;
begin
  FInertiaParams.Free;
  FGeneralParams.Free;
  FMoveAroundParams.Free;
  FAdjustDistanceParams.Free;
  inherited;
end;

procedure TGLSmoothNavigator.SetInertiaParams(
  const Value: TGLNavigatorInertiaParameters);
begin
  FInertiaParams.Assign(Value);
end;

procedure TGLSmoothNavigator.TurnHorizontal(Angle: Single; DeltaTime: Single);
var
  FinalAngle: Single;
begin
  with FInertiaParams do
  begin
    FinalAngle := 0;
    Angle := Angle * FTurnSpeed;
    while DeltaTime > FMaxExpectedDeltaTime do
    begin
      Angle := ClampValue((Angle * FMaxExpectedDeltaTime + OldTurnHorizontalAngle * FTurnInertia) / (FTurnInertia + 1), -FTurnMaxAngle, FTurnMaxAngle);
      OldTurnHorizontalAngle := Angle;
      DeltaTime := DeltaTime - FMaxExpectedDeltaTime;
      FinalAngle := FinalAngle + Angle;
    end;
  end;

  if (Abs(FinalAngle) > EPS) then
    inherited TurnHorizontal(FinalAngle);
end;

procedure TGLSmoothNavigator.TurnVertical(Angle: Single; DeltaTime: Single);
var
  FinalAngle: Single;
begin
  with FInertiaParams do
  begin
    FinalAngle := 0;
    Angle := Angle * FTurnSpeed;
    while DeltaTime > FMaxExpectedDeltaTime do
    begin
      Angle := ClampValue((Angle * FMaxExpectedDeltaTime + OldTurnVerticalAngle * FTurnInertia) / (FTurnInertia + 1), -FTurnMaxAngle, FTurnMaxAngle);
      OldTurnVerticalAngle := Angle;
      DeltaTime := DeltaTime - FMaxExpectedDeltaTime;
      FinalAngle := FinalAngle + Angle;
    end;
  end;

  if (Abs(FinalAngle) > EPS) then
    inherited TurnVertical(FinalAngle);
end;


procedure TGLSmoothNavigator.MoveForward(const Plus, Minus: Boolean; DeltaTime: Single; const Accelerate: Boolean = False);
var
  FinalDistance: Single;
  Distance:      Single;
begin
  with FInertiaParams do
  begin
    if Plus then
      Distance := FMovementSpeed
    else if Minus then
      Distance := -FMovementSpeed
    else
      Distance := 0;

    if Accelerate then
      Distance := Distance * FMovementAcceleration;

    FinalDistance := 0;

    while DeltaTime > FMaxExpectedDeltaTime do
    begin
      OldMoveForwardDistance := (Distance * FMaxExpectedDeltaTime + OldMoveForwardDistance * FMovementInertia) / (FMovementInertia + 1);
      DeltaTime := DeltaTime - FMaxExpectedDeltaTime;
      FinalDistance := FinalDistance + OldMoveForwardDistance;
    end;
  end;

  if Abs(FinalDistance) > EPS then
    inherited MoveForward(FinalDistance);
end;

procedure TGLSmoothNavigator.FlyForward(const Plus, Minus: Boolean; DeltaTime: Single; const Accelerate: Boolean = False);
var
  FinalDistance: Single;
  Distance:      Single;
begin
  with FInertiaParams do
  begin
    if Plus then
      Distance := FMovementSpeed
    else if Minus then
      Distance := -FMovementSpeed
    else
      Distance := 0;

    if Accelerate then
      Distance := Distance * FMovementAcceleration;

    FinalDistance := 0;

    while DeltaTime > FMaxExpectedDeltaTime do
    begin
      OldMoveForwardDistance := (Distance * FMaxExpectedDeltaTime + OldMoveForwardDistance * FMovementInertia) / (FMovementInertia + 1);
      DeltaTime := DeltaTime - FMaxExpectedDeltaTime;
      FinalDistance := FinalDistance + OldMoveForwardDistance;
    end;
  end;

  if Abs(FinalDistance) > EPS then
    inherited FlyForward(FinalDistance);
end;

procedure TGLSmoothNavigator.StrafeHorizontal(const Plus, Minus: Boolean; DeltaTime: Single; const Accelerate: Boolean = False);
var
  FinalDistance: Single;
  Distance:      Single;
begin
  with FInertiaParams do
  begin
    if Plus then
      Distance := FMovementSpeed
    else if Minus then
      Distance := -FMovementSpeed
    else
      Distance := 0;

    if Accelerate then
      Distance := Distance * FMovementAcceleration;

    FinalDistance := 0;

    while DeltaTime > FMaxExpectedDeltaTime do
    begin
      OldStrafeHorizontalDistance := (Distance * FMaxExpectedDeltaTime + OldStrafeHorizontalDistance * FMovementInertia) / (FMovementInertia + 1);
      DeltaTime := DeltaTime - FMaxExpectedDeltaTime;
      FinalDistance := FinalDistance + OldStrafeHorizontalDistance;
    end;
  end;

  if Abs(FinalDistance) > EPS then
    inherited StrafeHorizontal(FinalDistance);
end;

procedure TGLSmoothNavigator.StrafeVertical(const Plus, Minus: Boolean; DeltaTime: Single; const Accelerate: Boolean = False);
var
  FinalDistance: Single;
  Distance:      Single;
begin
  with FInertiaParams do
  begin
    if Plus then
      Distance := FMovementSpeed
    else if Minus then
      Distance := -FMovementSpeed
    else
      Distance := 0;

    if Accelerate then
      Distance := Distance * FMovementAcceleration;

    FinalDistance := 0;

    while DeltaTime > FMaxExpectedDeltaTime do
    begin
      OldStrafeVerticalDistance := (Distance * FMaxExpectedDeltaTime + OldStrafeVerticalDistance * FMovementInertia) / (FMovementInertia + 1);
      DeltaTime := DeltaTime - FMaxExpectedDeltaTime;
      FinalDistance := FinalDistance + OldStrafeVerticalDistance;
    end;
  end;

  if Abs(FinalDistance) > EPS then
    inherited StrafeVertical(FinalDistance);
end;

procedure TGLSmoothNavigator.AutoScaleParameters(const FPS: Single);
begin
  with FGeneralParams do
  begin
    if FPS > FAutoScaleMax / FMaxExpectedDeltatime then
      ScaleParameters(FAutoScaleMult)
    else if FPS < FAutoScaleMin / FMaxExpectedDeltatime then
      ScaleParameters(1/FAutoScaleMult);
  end;
end;


procedure TGLSmoothNavigator.AutoScaleParametersUp(const FPS: Single);
begin
  with FGeneralParams do
  begin
    if FPS > FAutoScaleMax / FMaxExpectedDeltatime then
      ScaleParameters(FAutoScaleMult)
  end;
end;

procedure TGLSmoothNavigator.ScaleParameters(const Value: Single);
begin
  Assert(Value > 0);
  FMaxExpectedDeltatime := FMaxExpectedDeltatime / Value;
  FInertiaParams.ScaleParameters(Value);
  FMoveAroundParams.ScaleParameters(Value);
  FAdjustDistanceParams.ScaleParameters(Value);
end;

function TGLSmoothNavigator.StoreMaxExpectedDeltaTime: Boolean;
begin
  Result := Abs(FMaxExpectedDeltaTime - 0.001) > EPS2;
end;

procedure TGLSmoothNavigator.SetGeneralParams(
  const Value: TGLNavigatorGeneralParameters);
begin
  FGeneralParams.Assign(Value);
end;

procedure TGLSmoothNavigator.SetMoveAroundParams(
  const Value: TGLNavigatorMoveAroundParameters);
begin
  FMoveAroundParams.Assign(Value);
end;

procedure TGLSmoothNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMoveAroundParams.FTargetObject then
      FMoveAroundParams.FTargetObject := nil;
  end;
end;

procedure TGLSmoothNavigator.SetObject(Value: TGLBaseSceneObject);
var
  I: Integer;
begin
  inherited;
  // Try to detect a TargetObject.
  if Value <> nil then
    if FMoveAroundParams.TargetObject = nil then
    begin
      // May be it is a camera...
      if Value is TGLCamera then
        FMoveAroundParams.TargetObject := TGLCamera(Value).TargetObject
      else
      begin
        // May be it has camera children...
        if Value.Count <> 0 then
          for I := 0 to Value.Count - 1 do
            if Value.Children[I] is TGLCamera then
            begin
              FMoveAroundParams.TargetObject := TGLCamera(Value.Children[I]).TargetObject;
              Exit;
            end;
      end;
    end;
end;

procedure TGLSmoothNavigator.MoveAroundTarget(const PitchDelta, TurnDelta,
  DeltaTime: Single);
begin
  MoveObjectAround(FMoveAroundParams.FTargetObject, PitchDelta, TurnDelta, DeltaTime);
end;

procedure TGLSmoothNavigator.MoveObjectAround(
  const AObject: TGLBaseSceneObject; PitchDelta, TurnDelta,
  DeltaTime: Single);
var
  FinalPitch: Single;
  FinalTurn:  Single;
begin
  FinalPitch := 0;
  FinalTurn := 0;
  with FMoveAroundParams do
  begin
    PitchDelta := PitchDelta * FPitchSpeed;
    TurnDelta := TurnDelta * FTurnSpeed;

    while DeltaTime > FMaxExpectedDeltatime do
    begin
      PitchDelta := ClampValue((PitchDelta * FMaxExpectedDeltatime + FOldPitchInertiaAngle * FInertia) / (FInertia + 1), - FMaxAngle, FMaxAngle);
      FOldPitchInertiaAngle := PitchDelta;
      FinalPitch := FinalPitch + PitchDelta;
      TurnDelta := ClampValue((TurnDelta * FMaxExpectedDeltatime + FOldTurnInertiaAngle * FInertia) / (FInertia + 1), - FMaxAngle, FMaxAngle);
      FOldTurnInertiaAngle := TurnDelta;
      FinalTurn := FinalTurn + TurnDelta;

      DeltaTime := DeltaTime - FMaxExpectedDeltatime;
    end;

  if (Abs(FinalPitch) > EPS) or (Abs(FinalTurn) > EPS) then
    MovingObject.MoveObjectAround(AObject, FinalPitch, FinalTurn);
  end;
end;


procedure TGLSmoothNavigator.AdjustDistanceToPoint(const APoint: TVector;
  const DistanceRatio: Single; DeltaTime: Single);

  // Based on TGLCamera.AdjustDistanceToTarget
  procedure DoAdjustDistanceToPoint(const DistanceRatio: Single);
  var
    vect: TVector;
  begin
    vect := VectorSubtract(MovingObject.AbsolutePosition, APoint);
    ScaleVector(vect, (distanceRatio - 1));
    AddVector(vect, MovingObject.AbsolutePosition);
    if Assigned(MovingObject.Parent) then
       vect := MovingObject.Parent.AbsoluteToLocal(vect);
    MovingObject.Position.AsVector := vect;
  end;

var
  FinalDistanceRatio: Single;
  TempDistanceRatio:  Single;
begin
  with FAdjustDistanceParams do
  begin
    TempDistanceRatio := DistanceRatio * FSpeed;
    FinalDistanceRatio := 0;
    while DeltaTime > FMaxExpectedDeltaTime do
    begin
      TempDistanceRatio := (TempDistanceRatio * FMaxExpectedDeltaTime + OldDistanceRatio * FInertia) / (FInertia + 1);
      OldDistanceRatio := TempDistanceRatio;
      DeltaTime := DeltaTime - FMaxExpectedDeltaTime;
      FinalDistanceRatio := FinalDistanceRatio + OldDistanceRatio / FMaxExpectedDeltaTime;
    end;

    if Abs(FinalDistanceRatio) > EPS2 then
    begin
      if FinalDistanceRatio > 0 then
        DoAdjustDistanceToPoint(1 / (1 + FinalDistanceRatio))
      else
        DoAdjustDistanceToPoint(1 * (1 - FinalDistanceRatio))
    end;
  end;
end;

procedure TGLSmoothNavigator.AdjustDistanceToTarget(const DistanceRatio,
  DeltaTime: Single);
begin
  Assert(FMoveAroundParams.FTargetObject <> nil);
  AdjustDistanceToPoint(FMoveAroundParams.FTargetObject.AbsolutePosition,
                        DistanceRatio, DeltaTime);
end;

procedure TGLSmoothNavigator.SetAdjustDistanceParams(
  const Value: TGLNavigatorAdjustDistanceParameters);
begin
  FAdjustDistanceParams.Assign(Value);
end;

{ TGLSmoothUserInterface }

function TGLSmoothUserInterface.MouseLook(
  const DeltaTime: Single): Boolean;
var
  MousePos: TGLPoint;
begin
  Assert(FAutoUpdateMouse, 'AutoUpdateMouse must be True to use this function');
  if FMouseLookActive then
  begin
    GLGetCursorPos(MousePos);
    Result := Mouselook(MousePos.X, MousePos.Y, DeltaTime);
    GLSetCursorPos(Round(OriginalMousePos.X), Round(OriginalMousePos.Y));
  end
  else
    Result := False;
end;

function TGLSmoothUserInterface.Mouselook(const NewX, NewY: Integer; const DeltaTime: Single): Boolean;
var
  DeltaX, DeltaY: Single;
begin
  Result := False;
  if FMouseLookActive then
  begin
    Deltax := (NewX - FOriginalMousePos.X);
    Deltay := (FOriginalMousePos.Y - NewY);

    if InvertMouse then
      DeltaY := -DeltaY;

    SmoothNavigator.TurnHorizontal(DeltaX, DeltaTime);
    SmoothNavigator.TurnVertical(DeltaY, DeltaTime);

    Result := (DeltaX <> 0) or (DeltaY <> 0);
  end;
end;


function TGLSmoothUserInterface.MouseLook(const NewXY: TGLPoint; const DeltaTime: Single): Boolean;
begin
  Result := Mouselook(NewXY.X, NewXY.Y, DeltaTime);
end;

constructor TGLSmoothUserInterface.Create(AOwner: TComponent);
begin
  inherited;
  FMouseLookActive := False;
  FAutoUpdateMouse := True;
  FOriginalMousePos := TGLCoordinates2.CreateInitialized(Self,
                             VectorMake(GLGetScreenWidth div 2,
                             GLGetScreenHeight div 2, 0, 0), csPoint2D);
end;

procedure TGLSmoothUserInterface.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if AComponent = FSmoothNavigator then
      FSmoothNavigator := nil;
    if AComponent = FSmoothVertNavigator then
      FSmoothNavigator := nil;
  end;
end;

procedure TGLSmoothUserInterface.SetSmoothNavigator(
  const Value: TGLSmoothNavigator);
begin
  if FSmoothNavigator <> nil then
    FSmoothNavigator.RemoveFreeNotification(Self);

  FSmoothNavigator := Value;

  if FSmoothNavigator <> nil then
    FSmoothNavigator.FreeNotification(Self);
end;

destructor TGLSmoothUserInterface.Destroy;
begin
  FOriginalMousePos.Destroy;
  inherited;
end;

procedure TGLSmoothUserInterface.SetOriginalMousePos(
  const Value: TGLCoordinates2);
begin
  FOriginalMousePos.Assign(Value);
end;

procedure TGLSmoothUserInterface.SetSmoothVertNavigator(
  const Value: TGLSmoothNavigator);
begin
  if FSmoothVertNavigator <> nil then
    FSmoothVertNavigator.RemoveFreeNotification(Self);

  FSmoothVertNavigator := Value;

  if FSmoothVertNavigator <> nil then
    FSmoothVertNavigator.FreeNotification(Self);
end;

procedure TGLSmoothUserInterface.MouseLookActiveToggle;
begin
  if FMouseLookActive then
    SetMouseLookActive(False)
  else
    SetMouseLookActive(True)
end;

procedure TGLSmoothUserInterface.SetMouseLookActive(const Value: Boolean);
var
  MousePos: TGLPoint;
begin
  if FMouseLookActive = Value then Exit;
  FMouseLookActive := Value;
  if FMouseLookActive then
  begin
    if FAutoUpdateMouse then
    begin
      GLGetCursorPos(MousePos);
      FOriginalMousePos.SetPoint2D(MousePos.X, MousePos.Y);
      GLShowCursor(False);
    end;
  end
  else
  begin
    if FAutoUpdateMouse then
      GLShowCursor(True);
  end;
end;

procedure TGLSmoothUserInterface.TurnHorizontal(const Angle: Single;
  const DeltaTime: Single);
begin
  FSmoothNavigator.TurnHorizontal(Angle, DeltaTime);
end;

procedure TGLSmoothUserInterface.TurnVertical(const Angle: Single;
  const DeltaTime: Single);
begin
  if Assigned(FSmoothNavigator) then
    FSmoothNavigator.TurnVertical(Angle, DeltaTime)
  else
    FSmoothVertNavigator.TurnVertical(Angle, DeltaTime);
end;

{ TGLNavigatorInertiaParameters }

procedure TGLNavigatorInertiaParameters.Assign(Source: TPersistent);
begin
  if Source is TGLNavigatorInertiaParameters then
  begin
    FMovementAcceleration := TGLNavigatorInertiaParameters(Source).FMovementAcceleration;
    FMovementInertia := TGLNavigatorInertiaParameters(Source).FMovementInertia;
    FMovementSpeed := TGLNavigatorInertiaParameters(Source).FMovementSpeed;
    FTurnMaxAngle := TGLNavigatorInertiaParameters(Source).FTurnMaxAngle;
    FTurnInertia := TGLNavigatorInertiaParameters(Source).FTurnInertia;
    FTurnSpeed := TGLNavigatorInertiaParameters(Source).FTurnSpeed;
  end
  else
    inherited; //to the pit of doom ;)
end;

constructor TGLNavigatorInertiaParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;

  FTurnInertia := 150;
  FTurnSpeed := 50;
  FTurnMaxAngle := 0.5;

  FMovementAcceleration := 7;
  FMovementInertia := 200;
  FMovementSpeed := 200;
end;

function TGLNavigatorInertiaParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGLNavigatorInertiaParameters.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value > 1 then
  begin
    FMovementInertia := FMovementInertia * VectorGeometry.Power(2, 1 / Value);
    FTurnInertia := FTurnInertia * VectorGeometry.Power(2, 1 / Value);
  end
  else
  begin
    FMovementInertia := FMovementInertia / VectorGeometry.Power(2, Value);
    FTurnInertia := FTurnInertia / VectorGeometry.Power(2, Value);
  end;
  FTurnMaxAngle := FTurnMaxAngle / Value;
  FTurnSpeed := FTurnSpeed * Value;
end;

function TGLNavigatorInertiaParameters.StoreTurnMaxAngle: Boolean;
begin
  Result := Abs(FTurnMaxAngle - 0.5) > EPS;
end;

function TGLNavigatorInertiaParameters.StoreMovementAcceleration: Boolean;
begin
  Result := Abs(FMovementAcceleration - 7) > EPS;
end;

function TGLNavigatorInertiaParameters.StoreMovementInertia: Boolean;
begin
  Result := Abs(FMovementInertia - 200) > EPS;
end;

function TGLNavigatorInertiaParameters.StoreMovementSpeed: Boolean;
begin
  Result := Abs(FMovementSpeed - 200) > EPS;
end;

function TGLNavigatorInertiaParameters.StoreTurnInertia: Boolean;
begin
  Result := Abs(FTurnInertia - 150) > EPS;
end;

function TGLNavigatorInertiaParameters.StoreTurnSpeed: Boolean;
begin
  Result := Abs(FTurnSpeed - 50) > EPS;
end;

{ TGLNavigatorGeneralParameters }

procedure TGLNavigatorGeneralParameters.Assign(Source: TPersistent);
begin
  if Source is TGLNavigatorGeneralParameters then
  begin
    FAutoScaleMin := TGLNavigatorGeneralParameters(Source).FAutoScaleMin;
    FAutoScaleMax := TGLNavigatorGeneralParameters(Source).FAutoScaleMax;
    FAutoScaleMult := TGLNavigatorGeneralParameters(Source).FAutoScaleMult;
  end
  else
    inherited; //die!
end;

constructor TGLNavigatorGeneralParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  FAutoScaleMin := 0.1;
  FAutoScaleMax := 0.75;
  FAutoScaleMult := 2;
end;

function TGLNavigatorGeneralParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TGLNavigatorGeneralParameters.StoreAutoScaleMax: Boolean;
begin
  Result := Abs(FAutoScaleMax - 0.75) > EPS;
end;

function TGLNavigatorGeneralParameters.StoreAutoScaleMin: Boolean;
begin
  Result := Abs(FAutoScaleMin - 0.1) > EPS;
end;

function TGLNavigatorGeneralParameters.StoreAutoScaleMult: Boolean;
begin
  Result := Abs(FAutoScaleMult - 2) > EPS;
end;

{ TGLNavigatorMoveAroundParameters }

procedure TGLNavigatorMoveAroundParameters.Assign(Source: TPersistent);
begin
  if Source is TGLNavigatorMoveAroundParameters then
  begin
    FMaxAngle := TGLNavigatorMoveAroundParameters(Source).FMaxAngle;
    FInertia :=  TGLNavigatorMoveAroundParameters(Source).FInertia;
    FPitchSpeed :=  TGLNavigatorMoveAroundParameters(Source).FPitchSpeed;
    FTurnSpeed :=  TGLNavigatorMoveAroundParameters(Source).FTurnSpeed;
  end
  else
    inherited; //die
end;

constructor TGLNavigatorMoveAroundParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  FPitchSpeed := 500;
  FTurnSpeed  := 500;
  FInertia          := 65;
  FMaxAngle         := 1.5;
end;

function TGLNavigatorMoveAroundParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGLNavigatorMoveAroundParameters.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value < 1 then
    FInertia := FInertia / VectorGeometry.Power(2, Value)
  else
    FInertia := FInertia * VectorGeometry.Power(2, 1 / Value);

  FMaxAngle := FMaxAngle / Value;
  FPitchSpeed := FPitchSpeed * Value;
  FTurnSpeed := FTurnSpeed * Value;
end;

procedure TGLNavigatorMoveAroundParameters.SetTargetObject(
  const Value: TGLBaseSceneObject);
begin
  if FTargetObject <> nil then
    if FOwner is TGLSmoothNavigator then
      FTargetObject.RemoveFreeNotification(TGLSmoothNavigator(FOwner));

  FTargetObject := Value;

  if FTargetObject <> nil then
    if FOwner is TGLSmoothNavigator then
      FTargetObject.FreeNotification(TGLSmoothNavigator(FOwner));
end;

function TGLNavigatorMoveAroundParameters.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 65) > EPS;
end;

function TGLNavigatorMoveAroundParameters.StoreMaxAngle: Boolean;
begin
  Result := Abs(FMaxAngle - 1.5) > EPS;
end;

function TGLNavigatorMoveAroundParameters.StorePitchSpeed: Boolean;
begin
  Result := Abs(FPitchSpeed - 500) > EPS;
end;

function TGLNavigatorMoveAroundParameters.StoreTurnSpeed: Boolean;
begin
  Result := Abs(FTurnSpeed - 500) > EPS;
end;

{ TGLNavigatorAdjustDistanceParameters }

procedure TGLNavigatorAdjustDistanceParameters.AddImpulse(
  const Impulse: Single);
begin
  OldDistanceRatio := OldDistanceRatio + Impulse * FSpeed / FInertia * FImpulseSpeed;
end;

procedure TGLNavigatorAdjustDistanceParameters.Assign(Source: TPersistent);
begin
  if Source is TGLNavigatorAdjustDistanceParameters then
  begin
    FInertia :=      TGLNavigatorAdjustDistanceParameters(Source).FInertia;
    FSpeed :=        TGLNavigatorAdjustDistanceParameters(Source).FSpeed;
    FImpulseSpeed := TGLNavigatorAdjustDistanceParameters(Source).FImpulseSpeed;
  end
  else
    inherited; //to the pit of doom ;)
end;

constructor TGLNavigatorAdjustDistanceParameters.Create(
  AOwner: TPersistent);
begin
  FOwner := AOwner;
  FInertia := 100;
  FSpeed := 0.005;
  FImpulseSpeed := 0.02;
end;

function TGLNavigatorAdjustDistanceParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


procedure TGLNavigatorAdjustDistanceParameters.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value < 1 then
    FInertia := FInertia / VectorGeometry.Power(2, Value)
  else
    FInertia := FInertia * VectorGeometry.Power(2, 1 / Value);

  FImpulseSpeed := FImpulseSpeed / Value;
end;

function TGLNavigatorAdjustDistanceParameters.StoreImpulseSpeed: Boolean;
begin
  Result := Abs(FImpulseSpeed - 0.02) > EPS;
end;

function TGLNavigatorAdjustDistanceParameters.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 100) > EPS;
end;

function TGLNavigatorAdjustDistanceParameters.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 0.005) > EPS2;
end;

initialization
  RegisterClasses([
      TGLSmoothNavigator, TGLSmoothUserInterface,
      TGLNavigatorInertiaParameters, TGLNavigatorGeneralParameters,
      TGLNavigatorMoveAroundParameters, TGLNavigatorAdjustDistanceParameters
                   ]);

end.

