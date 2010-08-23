//
// This unit is part of the GLScene Project, http://glscene.org
//

{: GLCameraController<p>

  Component for animating camera movement.
  Can be used to zoom in/out, for linear movement, orbiting and Google Earth - like "fly-to"
  Main purpose was the SafeOrbitAndZoomToPos method, the others are usable as well

  <b>History : </b><font size=-1><ul>
      <li>24/07/09 - DaStr - Got rid of compiler hints 
      <li>20/03/09 - DanB - Donated to GLScene by Bogdan Deaky.
    </ul></font>
}

//  ######## NOTE: *MAY* STILL BE WORK IN PROGRESS ON THIS COMPONENT ##########

//GLCameraController v1.1
//Bogdan Deaky / Bluemind Software
//Bluemind Software allows free usage and distribution of this component
//Do let the author know if you do code changes/improvements
//bogdan@bluemind-software.ro
//v1.0 2007
//v1.1 2009 (for GLScene, ships with glscene_icon_TGLCameraController.bmp)


//IMPORTANT!
//You should block user GUI access to the GLSceneViewer
//while movement is being done, check the AllowUserAction property!
//Block user GUI access while AllowUserAction is false to avoid behaviour errors
//simply put
//if GLCameraController1.AllowUserAction then
// //do whatever you want on mouse move, form wheel etc

// methods and properties are explained in the interface section (through comments)
// additional comments might apear in implementation section where needed

unit GLCameraController;

interface

uses GLScene, Classes, GLCadencer;

type

  TGLCameraController = class(TComponent)
  private
    //private variables - explained at the respective properties
    FAllowUserAction:boolean;
    FCamera:TGLCamera;
    FCadencer:TGLCadencer;
    //fields used by SafeOrbitAndZoomToPos
    FsoSafeDist,FsoTimeToSafePlacement,FsoTimeToOrbit,FsoTimeToZoomBackIn:double;
    //used to break current movement
    Stopped:boolean;
    //private methods
    //used to test whether camera and cadencer are assigned
    //TestExtendedPremises = true -> will test also for Camera.TargetObject
    function TestPremises(TestExtendedPremises:boolean):boolean;
    //this adjusts camera depth of view after each movement
    //contains a call to Application.Processmessages for not blocking the app
    procedure AdjustScene;
    //after AdjustScene the Camera.DepthofView will be modified
    //if you want to zoom back in from GUI
    //you should use something like
    //  Camera.DepthOfView:=2*Camera.DistanceToTarget+2*camera.TargetObject.BoundingSphereRadius;
  public
    //constructor
    constructor Create(AOwner:TComponent); override;
    //methods
    //linear movement from current pos
    procedure MoveToPos(x,y,z,time:double);
    //orbiting from current pos to the pos where
    //the camera points at the camera.targetObject TROUGH the given point
    //it will not move to the given point(!), use SafeOrbitAndZoomToPos instead
    //there has to be a camera.targetObject assigned!
    procedure OrbitToPos(x,y,z,time:double);
    //old commented prov with vectors - it's here only for reference
    //procedure OrbitToPosVector(x,y,z,time:double);
    //zooms in/out by moving to the given distance from camera.targetObject
    //there has to be a camera.targetObject assigned!
    procedure ZoomToDistance(Distance,Time:double);
    //google earth - like "fly-to"
    // = zoom out to safe distance, orbit, and then zoom in to the given point
    //there has to be a camera.targetObject assigned!
    procedure SafeOrbitAndZoomToPos(x,y,z:double);
    //Dan Bartlett said in the GLScene newsgroup that it might be a good idea
    //to introduce ability to stop movement and return control to user
    //here it is
    procedure StopMovement;
  published
    //properties
    //assign a TGLCamera instance to this
    property Camera:TGLCamera read FCamera write FCamera;
    //assign a TGLCadencer instance to this
    property Cadencer:TGLCadencer read FCadencer write FCadencer;
    //specifies whether user should be able interract with the GLSceneViewer
    //it is set to false while the camera is moving and
    //coders should check this value and block GUI access to GLSceneViewer
    property AllowUserAction:boolean read FAllowUserAction;
    //safe distance to avoid moving the camera trough the camera.targetObject
    //while performing  SafeOrbitAndZoomToPos
    property soSafeDistance:double read FsoSafeDist write FsoSafeDist;
    //time to zoom in/out to the safe position while performing  SafeOrbitAndZoomToPos
    property soTimeToSafePlacement:double read FsoTimeToSafePlacement write FsoTimeToSafePlacement;
    //time to orbit while performing  SafeOrbitAndZoomToPos
    property soTimeToOrbit:double read FsoTimeToOrbit write FsoTimeToOrbit;
    //time to zoom in/out to the given final position while performing  SafeOrbitAndZoomToPos
    property soTimeToZoomBackIn:double read FsoTimeToZoomBackIn write FsoTimeToZoomBackIn;
  end;

implementation

uses SysUtils, Forms, VectorTypes, VectorGeometry{, Dialogs};

//---TGLCameraMover

function TGLCameraController.TestPremises(TestExtendedPremises:boolean):boolean;
begin
  result:=true;
  //check camera assignament
  if not assigned(Camera) then
  begin
//    result:=false;
    raise Exception.Create('CameraMover needs to have camera assigned');
    exit;
  end;
  //check cadencer assignament
  if not assigned(Cadencer) then
  begin
//    result:=false;
    raise Exception.Create('CameraMover needs to have Cadencer assigned');
    exit;
  end;
  if TestExtendedPremises then
  //TestExtendedPremises = check camera.TargetObject assignament
  if not assigned(Camera.TargetObject) then
  begin
//    result:=false;
    raise Exception.Create('This movement needs needs to have Camera.TargetObject assigned');
  end;
end;

procedure TGLCameraController.AdjustScene;
begin
  Camera.DepthOfView:=2*Camera.DistanceToTarget+2*camera.TargetObject.BoundingSphereRadius;
  Camera.TransformationChanged;
  Application.ProcessMessages; //I call this for not blocking the app while moving the camera
end;

procedure TGLCameraController.StopMovement;
begin
  Stopped:=true;
end;

procedure TGLCameraController.MoveToPos(x,y,z,time:double);
var InitialCameraPos, FinalCameraPos, Vect: TVector;
    StartTime, TimeElapsed :double;
begin
  if not TestPremises(true) then exit;
  FAllowUserAction:=false;
  //assign initial and final positions
  InitialCameraPos:=VectorSubtract(Camera.AbsolutePosition, Camera.TargetObject.AbsolutePosition);
  MakeVector(FinalCameraPos, x, y, z);
  //movement
  StartTime:=Cadencer.GetCurrentTime;
  TimeElapsed:=Cadencer.GetCurrentTime-StartTime;
  while TimeElapsed<time do
  begin
    TimeElapsed:=Cadencer.GetCurrentTime-StartTime;
    Vect:=VectorLerp(InitialCameraPos,FinalCameraPos,TimeElapsed/time);
    if Assigned(Camera.Parent) then
      Vect:=Camera.Parent.AbsoluteToLocal(Vect);
    Camera.Position.AsVector:=Vect;
    AdjustScene;
    if Stopped then begin Stopped:=false; break; end;
  end;
  //finish movement - adjust to final point
  vect:=FinalCameraPos;
  if Assigned(Camera.Parent) then
    Vect:=Camera.Parent.AbsoluteToLocal(Vect);
  Camera.Position.AsVector:=Vect;
  AdjustScene;
  FAllowUserAction:=true;
end;

procedure TGLCameraController.OrbitToPos(x,y,z,time:double);
var pitchangle0,pitchangle1,turnangle0,turnangle1,
    pitchangledif,turnangledif,
    dx0,dy0,dz0,dx1,dy1,dz1,speedx,speedz,
    StartTime,LastTime,TimeElapsed:double;
    sign:shortint;
    InitialCameraPos, AbsFinalCameraPos, FinalCameraPos, Vect: TVector;
    Radius: double;
begin
  //all vector approaches have failed

  //some problems with VectorAngleLerp which internally uses Quaterinion Slerp
  //that is known to be problematic in a couple of particular cases
  //but I think the implementation is also wrong
  //also, a method where vectorlerp + normalize + scale was developed  (OrbitToPosVector)
  //but the velocity is not constant (of course) + other problems in particular cases

  //this method works superbly, as long as the camera position is
  //a combination of two 0s and one 1(or even -1)
  //the final adjustment is done with vectors and
  //the display is cleared for some reason in you position the camera on the Up axis.

  //this method does not have the problems of Slerp or general spherical interpolation
  //it computes the difference between Pitch and Turn Angles and rotates the camera around the target
  //it will never try to go over the blocking upper/lower points!  - this cause error in the mentioned algorithms

  if not TestPremises(true) then exit;
  FAllowUserAction:=false;
  //determine relative positions to determine the lines which form the angles
  //distances from initial camera pos to target object
  dx0:=Camera.Position.X-Camera.TargetObject.Position.x;
  dy0:=Camera.Position.Y-Camera.TargetObject.Position.Y;
  dz0:=Camera.Position.Z-Camera.TargetObject.Position.Z;
  //distances from final camera pos to target object
  dx1:=X-Camera.TargetObject.Position.x;
  dy1:=Y-Camera.TargetObject.Position.Y;
  dz1:=Z-Camera.TargetObject.Position.Z;

  //just to make sure we don't get division by 0 exceptions
  if dx0=0 then dx0:=0.001;
  if dy0=0 then dy0:=0.001;
  if dz0=0 then dz0:=0.001;
  if dx1=0 then dx1:=0.001;
  if dy1=0 then dy1:=0.001;
  if dz1=0 then dz1:=0.001;

  //determine "pitch" and "turn" angles for the initial and  final camera position
  //the formulas differ depending on the camera.Up vector
  //I tested all quadrants for all possible integer Camera.Up directions
  if abs(Camera.Up.AsAffineVector[2])=1 then  //Z=1/-1
  begin
    sign:= round(Camera.Up.AsAffineVector[2]/abs(Camera.Up.AsAffineVector[2]));
    pitchangle0:=arctan(dz0/sqrt(sqr(dx0)+sqr(dy0)));
    pitchangle1:=arctan(dz1/sqrt(sqr(dx1)+sqr(dy1)));
    turnangle0:=arctan(dy0/dx0);
    if (dx0<0) and (dy0<0) then turnangle0:=-(pi-turnangle0)
    else  if (dx0<0) and (dy0>0) then turnangle0:=-(pi-turnangle0);
    turnangle1:=arctan(dy1/dx1);
    if (dx1<0) and (dy1<0) then turnangle1:=-(pi-turnangle1)
    else  if (dx1<0) and (dy1>0) then turnangle1:=-(pi-turnangle1);
  end
  else if abs(Camera.Up.AsAffineVector[1])=1 then  //Y=1/-1
  begin
    sign:= round(Camera.Up.AsAffineVector[1]/abs(Camera.Up.AsAffineVector[1]));
    pitchangle0:=arctan(dy0/sqrt(sqr(dx0)+sqr(dz0)));
    pitchangle1:=arctan(dy1/sqrt(sqr(dx1)+sqr(dz1)));
    turnangle0:=-arctan(dz0/dx0);
    if (dx0<0) and (dz0<0) then turnangle0:=-(pi-turnangle0)
    else  if (dx0<0) and (dz0>0) then turnangle0:=-(pi-turnangle0);
    turnangle1:=-arctan(dz1/dx1);
    if (dx1<0) and (dz1<0) then turnangle1:=-(pi-turnangle1)
    else  if (dx1<0) and (dz1>0) then turnangle1:=-(pi-turnangle1);
  end
  else if abs(Camera.Up.AsAffineVector[0])=1 then //X=1/-1
  begin
    sign:= round(Camera.Up.AsAffineVector[0]/abs(Camera.Up.AsAffineVector[0]));
    pitchangle0:=arctan(dx0/sqrt(sqr(dz0)+sqr(dy0)));
    pitchangle1:=arctan(dx1/sqrt(sqr(dz1)+sqr(dy1)));
    turnangle0:=arctan(dz0/dy0);
    if (dz0>0) and (dy0>0) then turnangle0:=-(pi-turnangle0)
    else  if (dz0<0) and (dy0>0) then turnangle0:=-(pi-turnangle0);
    turnangle1:=arctan(dz1/dy1);
    if (dz1>0) and (dy1>0) then turnangle1:=-(pi-turnangle1)
    else  if (dz1<0) and (dy1>0) then turnangle1:=-(pi-turnangle1);
  end
  else
  begin
    raise Exception.Create('The Camera.Up vector may contain only -1, 0 or 1');
    exit;
  end;
  //determine pitch and turn angle differences
  pitchangledif:=sign*(pitchangle1-pitchangle0);
  turnangledif:=sign*(turnangle1-turnangle0);
  if abs(turnangledif)>pi then turnangledif:=-abs(turnangledif)/turnangledif*(2*pi-abs(turnangledif));
  //determine rotation speeds
  speedx:=-pitchangledif/time;
  speedz:=turnangledif/time;

  StartTime:=Cadencer.GetCurrentTime;
  LastTime:=StartTime;

  //make the actual movement
  while Cadencer.GetCurrentTime-StartTime<time do
  begin
    TimeElapsed:=(Cadencer.GetCurrentTime-LastTime);
    LastTime:=Cadencer.GetCurrentTime;
    Camera.MoveAroundTarget(radtodeg(speedx)*TimeElapsed,radtodeg(speedz)*TimeElapsed);
    AdjustScene;
    if Stopped then begin Stopped:=false; break; end;
  end;

  //finish  movement - init vectors
  InitialCameraPos:=VectorSubtract(Camera.AbsolutePosition, Camera.TargetObject.AbsolutePosition);
  Radius:=VectorLength(InitialCameraPos);
  MakeVector(AbsFinalCameraPos, x, y, z);
  FinalCameraPos:=VectorSubtract(AbsFinalCameraPos, Camera.TargetObject.AbsolutePosition);
  NormalizeVector(FinalCameraPos);
  ScaleVector(FinalCameraPos, Radius);
  //finish  movement - adjust to final point
  Vect:=FinalCameraPos;
  if Assigned(Camera.Parent) then
    Vect:=Camera.Parent.AbsoluteToLocal(Vect);
  Camera.Position.AsVector:=Vect;
  AdjustScene;

  FAllowUserAction:=true;
end;

//OrbitToPosVector - old vector implementation - not working well
//I left it here for reference
//the try with VectorAngleLerp was deleted as it proved
//uselles at the time of development

{procedure TGLCameraController.OrbitToPosVector(x,y,z,time:double);
var InitialCameraPos, AbsFinalCameraPos, FinalCameraPos, Vect, MidWayVector: TVector;
    StartTime, TimeElapsed, Radius :double;
begin
  //I have tried VectorAngleLerp but it does not seem to work correctly
  if not TestPremises(true) then exit;
  FAllowUserAction:=false;
  //assign initial and final positions
  InitialCameraPos:=VectorSubtract(Camera.AbsolutePosition, Camera.TargetObject.AbsolutePosition);
  Radius:=VectorLength(InitialCameraPos);
  MakeVector(AbsFinalCameraPos, x, y, z);
  FinalCameraPos:=VectorSubtract(AbsFinalCameraPos, Camera.TargetObject.AbsolutePosition);
  NormalizeVector(FinalCameraPos);
  ScaleVector(FinalCameraPos, Radius);

  MidWayVector:=VectorLerp(InitialCameraPos,FinalCameraPos,0.499); //0.5 is more probable to give sometimes the 0 point
  NormalizeVector(MidWayVector);
  ScaleVector(MidWayVector, Radius);

  //movement
  StartTime:=Cadencer.GetCurrentTime;
  TimeElapsed:=Cadencer.GetCurrentTime-StartTime;
  while TimeElapsed<time*0.499 do
  begin
    TimeElapsed:=Cadencer.GetCurrentTime-StartTime;
    Vect:=VectorLerp(InitialCameraPos,MidWayVector,TimeElapsed/(time*0.499));
    NormalizeVector(Vect);
    ScaleVector(Vect, Radius);
    if Assigned(Camera.Parent) then
      Vect:=Camera.Parent.AbsoluteToLocal(Vect);
    Camera.Position.AsVector:=Vect;
    AdjustScene;
  end;
  //finish half movement - adjust to mid point
  vect:=MidWayVector;
  if Assigned(Camera.Parent) then
    Vect:=Camera.Parent.AbsoluteToLocal(Vect);
  Camera.Position.AsVector:=Vect;
  AdjustScene;
  //showmessage(floattostr(Camera.Position.AsVector[0])+'/'+floattostr(Camera.Position.AsVector[1])+'/'+floattostr(Camera.Position.AsVector[2]));

  StartTime:=Cadencer.GetCurrentTime;
  TimeElapsed:=Cadencer.GetCurrentTime-StartTime;
  while TimeElapsed<time*0.501 do
  begin
    TimeElapsed:=Cadencer.GetCurrentTime-StartTime;
    Vect:=VectorLerp(MidWayVector,FinalCameraPos,TimeElapsed/(time*0.501));
    NormalizeVector(Vect);
    ScaleVector(Vect, Radius);
    if Assigned(Camera.Parent) then
      Vect:=Camera.Parent.AbsoluteToLocal(Vect);
    Camera.Position.AsVector:=Vect;
    AdjustScene;
  end;
  //finish  movement - adjust to final point
  vect:=FinalCameraPos;
  if Assigned(Camera.Parent) then
    Vect:=Camera.Parent.AbsoluteToLocal(Vect);
  Camera.Position.AsVector:=Vect;
  AdjustScene;
  FAllowUserAction:=true;
end;     }

procedure TGLCameraController.ZoomToDistance(Distance,Time:double);
var InitialCameraPos, FinalCameraPos, Vect: TVector;
    StartTime, TimeElapsed :double;
begin
  if not TestPremises(true) then exit;
  FAllowUserAction:=false;
  InitialCameraPos:=VectorSubtract(Camera.AbsolutePosition, Camera.TargetObject.AbsolutePosition);
  //to determine final positon we normalize original pos and scale it with final distance
  SetVector(FinalCameraPos, InitialCameraPos);
  NormalizeVector(FinalCameraPos);
  ScaleVector(FinalCameraPos,Distance);
  //movement
  StartTime:=Cadencer.GetCurrentTime;
  TimeElapsed:=Cadencer.GetCurrentTime-StartTime;
  while TimeElapsed<time do
  begin
    TimeElapsed:=Cadencer.GetCurrentTime-StartTime;
    Vect:=VectorLerp(InitialCameraPos,FinalCameraPos,TimeElapsed/time);
    if Assigned(Camera.Parent) then
      Vect:=Camera.Parent.AbsoluteToLocal(Vect);
    Camera.Position.AsVector:=Vect;
    AdjustScene;
    if Stopped then begin Stopped:=false; break; end;
  end;
  //finish movement - adjust to final point
  vect:=FinalCameraPos;
  if Assigned(Camera.Parent) then
    Vect:=Camera.Parent.AbsoluteToLocal(Vect);
  Camera.Position.AsVector:=Vect;
  AdjustScene;
  FAllowUserAction:=true;
end;

procedure TGLCameraController.SafeOrbitAndZoomToPos(x,y,z:double);
begin
  //this was the main purpose of this component
  //as you can see, it actually is a combination of the other 3 methods
  if not TestPremises(true) then exit;
  ZoomToDistance(soSafeDistance,soTimeToSafePlacement);
  OrbitToPos(x,y,z,soTimeToOrbit);
  MoveToPos(x,y,z,soTimeToZoomBackIn);
end;

constructor TGLCameraController.Create(AOwner:TComponent);
begin
  inherited;
  //initialize values
  soSafeDistance:=10;
  soTimeToSafePlacement:=1;
  soTimeToOrbit:=2;
  soTimeToZoomBackIn:=1;
  FAllowUserAction:=true;
  Stopped:=false;
end;

end.
