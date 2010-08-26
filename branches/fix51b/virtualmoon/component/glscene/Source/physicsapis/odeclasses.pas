{*************************************************************************
 *                                                                       *
 * open dynamics engine, copyright (c) 2001,2002 russell l. smith.       *
 * all rights reserved.  email: russ@q12.org   web: www.q12.org          *
 *                                                                       *
 * this library is free software; you can redistribute it and/or         *
 * modify it under the terms of either:                                  *
 *   (1) the gnu lesser general public license as published by the free  *
 *       software foundation; either version 2.1 of the license, or (at  *
 *       your option) any later version. the text of the gnu lesser      *
 *       general public license is included with this library in the     *
 *       file license.txt.                                               *
 *   (2) the bsd-style license that is included with this library in     *
 *       the file license-bsd.txt.                                       *
 *                                                                       *
 * this library is distributed in the hope that it will be useful,       *
 * but without any warranty; without even the implied warranty of        *
 * merchantability or fitness for a particular purpose. see the files    *
 * license.txt and license-bsd.txt for more details.                     *
 *                                                                       *
 *************************************************************************}

{*************************************************************************
 *                                                                       *
 * ode delphi classes unit                                               *
 *                                                                       *
 *   Created by Christophe Hosten ( chroma@skynet.be )                   *
 *                                                                       *
 *  All real work was of course performed by Russell L. Smith,           *
 *    who created ODE.                                                   *
 *                                                                       *
 *  Convertion started 2002-09-23.                                       *
 *                                                                       *
 *  There is no Delphi documentation for ODE, see the original ODE files *
 *  for information http://www.q12.org/ode/ode-0.03-userguide.html       *
 *                                                                       *
 *************************************************************************}

{
  Change history

  CH = christophe hosten ( chroma@skynet.be )
  MF = mattias fagerlund ( mattias@cambrianlabs.com )
  SG = stuart gooding ( sgooding@flemings.com.au )

  2002.09.?  Initial release by CH
  2002.09.29 Added ODEClasses.pas to official DelphiODE release by MF
  2002.10.01 Updated ODESpace and descendants to handle new PdxSpace classes by MF
  2003.01.?  Added support for Cylinders (the non-capped kind) by SG
  2003.03.18 Removed dGeomGroup due to deprecation, updated support for spaces by SG,

}

unit odeclasses;

interface

uses classes, odeimport;

Type
  TODEJoint = class;

  TPointerList = class(TList)
  private
    function GetItems(i: integer): Pointer;
    procedure SetItems(i: integer; const Value: Pointer);
  public
    property Items[i : integer] : Pointer read GetItems write SetItems; default;
  end;

  TODEWorld = class
  private
    { Private declarations }
    pWorld: PdxWorld;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    function GetPointer: PdxWorld;
    procedure SetCFM(const cfm: TdReal);
    function GetCFM: TdReal;
    procedure SetERP(const erp: TdReal);
    function GetERP: TdReal;
    procedure SetGravity(const x, y, z: TdReal);
    function GetGravity: TdVector3;
    procedure Step(const stepsize: TdReal);
    function ImpulseToForce(const stepsize, ix, iy, iz: TdReal): TdVector3;
  end;

  TODEBody = class
  private
    { Private declarations }
    pBody: PdxBody;
    pODEJoint: TPointerList;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const World: TODEWorld);
    destructor Destroy; override;
    function GetPointer: PdxBody;
    procedure AddForce(const fx, fy, fz: TdReal);
    procedure AddForceAtPos(const fx, fy, fz, px, py, pz: TdReal);
    procedure AddForceAtRelPos(const fx, fy, fz, px, py, pz: TdReal);
    procedure AddRelForce(const fx, fy, fz: TdReal);
    procedure AddRelForceAtPos(const fx, fy, fz, px, py, pz: TdReal);
    procedure AddRelForceAtRelPos(const fx, fy, fz, px, py, pz: TdReal);
    procedure AddRelTorque(const fx, fy, fz: TdReal);
    procedure AddTorque(const fx, fy, fz: TdReal);
    procedure Disable;
    procedure Enable;
    function GetAngularVel: TdVector3;
    function GetFiniteRotationAxis: TdVector3;
    function GetFiniteRotationMode: Integer;
    function GetForce: TdVector3;
    function GetGravityMode: Integer;
    function GetJoint(const index: Integer): TODEJoint;
    function GetLinearVel: TdVector3;
    function GetMass: Pointer;
    function GetNumJoints: Integer;
    function GetPointVel(const px, py, pz: TdReal): TdVector3;
    function GetPosRelPoint(const px, py, pz: TdReal): TdVector3;
    function GetPosition: TdVector3;
    function GetQuaternion: TdQuaternion;
    function GetRelPointPos(const px, py, pz: TdReal): TdVector3;
    function GetRelPointVel(const px, py, pz: TdReal): TdVector3;
    function GetRotation: TdMatrix3;
    function GetTorque: TdVector3;
    function IsEnabled: Boolean;
    procedure SetAngularVel(const x, y, z: TdReal);
    procedure SetFiniteRotationAxis(const x, y, z: TdReal);
    procedure SetFiniteRotationMode(const mode: Integer);
    procedure SetForce(const x, y, z: TdReal);
    procedure SetGravityMode(const mode: Integer);
    procedure SetLinearVel(const x, y, z: TdReal);
    procedure SetMass(const mass: TdMass);
    procedure SetPosition(const x, y, z: TdReal);
    procedure SetQuaternion(const q: TdQuaternion);
    procedure SetRotation(const R: TdMatrix3);
    procedure SetTorque(const x, y, z: TdReal);
    function VectorFromWorld(const px, py, pz: TdReal): TdVector3;
    function VectorToWorld(const px, py, pz: TdReal): TdVector3;
    function IsConnectedTo(const b: TODEBody): Boolean;
    procedure SetData(const data: Pointer);
    function GetData: Pointer;
  end;

  TODEJointGroup = class
  private
    { Private declarations }
    pJointGroup: TdJointGroupID;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const max_size: Integer);
    destructor Destroy; override;
    function GetPointer: TdJointGroupID;
    procedure Empty;
  end;

  TODEJoint = class
  private
    { Private declarations }
    pJoint: TdJointID;
    pODEBody: TPointerList;
  protected
    { Protected declarations }
  public
    { Public declarations }
    destructor Destroy; override;
    function GetPointer: TdJointID;
    procedure Attach(const body1, body2: TODEBody); overload;
    procedure Attach(const pbody1, pbody2: PdxBody); overload;
    function GetType: Integer;
    function GetBody(const index: Integer): TODEBody;
    procedure SetData(const data: Pointer);
    function GetData: Pointer;
  end;

  TODEJointBall = class(TODEJoint)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const World: TODEWorld; const JointGroup: TODEJointGroup);
    procedure SetAnchor(const x, y, z: TdReal);
    function GetAnchor: TdVector3;
  end;

  TODEJointHinge = class(TODEJoint)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const World: TODEWorld; const JointGroup: TODEJointGroup);
    procedure SetAnchor(const x, y, z: TdReal);
    function GetAnchor: TdVector3;
    procedure SetAxis(const x, y, z: TdReal);
    function GetAxis: TdVector3;
    function GetAngle: TdReal;
    function GetAngleRate: TdReal;
    procedure SetParam(const parameter: Integer; const value: TdReal);
    function GetParam(const parameter: Integer): TdReal;
  end;

  TODEJointHinge2 = class(TODEJoint)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const World: TODEWorld; const JointGroup: TODEJointGroup);
    procedure SetAnchor(const x, y, z: TdReal);
    function GetAnchor: TdVector3;
    procedure SetAxis1(const x, y, z: TdReal);
    procedure SetAxis2(const x, y, z: TdReal);
    function GetAxis1: TdVector3;
    function GetAxis2: TdVector3;
    function GetAngle1: TdReal;
    function GetAngle1Rate: TdReal;
    function GetAngle2Rate: TdReal;
    procedure SetParam(const parameter: Integer; const value: TdReal);
    function GetParam(const parameter: Integer): TdReal;
  end;

  TODEJointContact = class(TODEJoint)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const World: TODEWorld; const JointGroup: TODEJointGroup; const dContact: TdContact);
  end;

  TODEJointSlider = class(TODEJoint)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const World: TODEWorld; const JointGroup: TODEJointGroup);
    procedure SetAxis(const x, y, z: TdReal);
    function GetAxis: TdVector3;
    function GetPosition: TdReal;
    function GetPositionRate: TdReal;
    procedure SetParam(const parameter: Integer; const value: TdReal);
    function GetParam(const parameter: Integer): TdReal;
  end;

  TODEJointFixed = class(TODEJoint)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const World: TODEWorld; const JointGroup: TODEJointGroup);
    procedure SetFixed;
  end;

  TODEJointAMotor = class(TODEJoint)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const World: TODEWorld; const JointGroup: TODEJointGroup);
    procedure SetAngle(const anum: Integer; const angle: TdReal);
    function GetAngle(const anum: Integer): TdReal;
    function GetAngleRate(const anum: Integer): TdReal;
    procedure SetAxis(const anum, rel: Integer; const x, y, z: TdReal);
    function GetAxis(const anum: Integer): TdVector3;
    function GetAxisRel(const anum: Integer): Integer;
    procedure SetMode(const mode: TdAngularMotorModeNumbers);
    function GetMode: Integer;
    procedure SetNumAxes(const num: Integer);
    function GetNumAxes: Integer;
    procedure SetParam(const parameter: Integer; const value: TdReal);
    function GetParam(const parameter: Integer): TdReal;
  end;

  TODEJointUniversal = class(TODEJoint)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const World: TODEWorld; const JointGroup: TODEJointGroup);
    procedure SetAnchor(const x, y, z: TdReal);
    function GetAnchor: TdVector3;
    procedure SetAxis1(const x, y, z: TdReal);
    procedure SetAxis2(const x, y, z: TdReal);
    function GetAxis1: TdVector3;
    function GetAxis2: TdVector3;
  end;

  TODEGeom = class
  private
    { Private declarations }
    pGeom: PdxGeom;
    pODEBody: ^TODEBody;
  protected
    { Protected declarations }
  public
    { Public declarations }
{ TODO : Is dCreateGeom needed ? }
    destructor Destroy; override;
    function GetPointer: PdxGeom;
    function Collide(Geom: TODEGeom; flags: Integer; var Contact: TdContactGeom; Skip: Integer): Integer;
    function GetAABB: TdAABB;
    procedure SetBody(Body: TODEBody);
    function GetBody: TODEBody;
    function GetClass: Integer;
    function GetClassData: Pointer;
    procedure SetPosition(const x, y, z: TdReal);
    function GetPosition: PdVector3;
    procedure SetRotation(const R: TdMatrix3);
    function GetRotation: PdMatrix3;
    function GetSpaceAABB: TdReal;
    procedure SetData(const data: Pointer);
    function GetData: Pointer;
  end;

  TODESpace = class
  private
    { Private declarations }
    _pSpace: PdxSpace;
    ParentODESpace : TODESpace;
  protected
    { Protected declarations }
  public
    { Public declarations }
    destructor Destroy; override;
    function GetPointer: PdxSpace;
    procedure Add(const Geom: TODEGeom);
    procedure Remove(const Geom: TODEGeom);
    function Query(const Geom: TODEGeom): Integer;
    procedure Collide(data: pointer; callback: TdNearCallback);
    constructor Create(aSpace:TODESpace);virtual;
  end;

  TODESpaceSimple = class(TODESpace)
  private
    { Private declarations }
  protected
    { Protected declarations }
    pSpace : PdxSimpleSpace;
  public
    { Public declarations }
    constructor Create(aSpace:TODESpace);override;
  end;

  TODESpaceHash = class(TODESpace)
  private
    { Private declarations }
  protected
    { Protected declarations }
    pSpace : PdxHashSpace;
  public
    { Public declarations }
    constructor Create(aSpace:TODESpace);override;
    procedure SetLevels(const minlevel, maxlevel: Integer);

    function GetMinLevel : integer;
    function GetMaxLevel : integer;
  end;

  TODESphere = class(TODEGeom)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const Space: TODESpace; const radius: TdReal);
    procedure SetRadius(const radius: TdReal);
    function GetRadius: TdReal;
  end;

  TODEBox = class(TODEGeom)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const Space: TODESpace; const lx, ly, lz: TdReal);
    procedure SetLengths(const lx, ly, lz: TdReal);
    function GetLengths: TdVector3;
  end;

  TODEPlane = class(TODEGeom)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const Space: TODESpace; const a, b, c, d: TdReal);
    procedure SetParams(const a, b, c, d: TdReal);
    function GetParams: TdVector4;
  end;

  TODECCylinder = class(TODEGeom)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const Space: TODESpace; const radius, length: TdReal);
    procedure SetParams(const radius, length: TdReal);
    procedure GetParams(var radius, length: TdReal);
  end;

  TODECylinder = class(TODEGeom)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const Space: TODESpace; const radius, length: TdReal);
    procedure SetParams(const radius, length: TdReal);
    procedure GetParams(var radius, length: TdReal);
  end;

  TODEGeomTransform = class(TODEGeom)
  private
    { Private declarations }
    pODEGeom: ^TODEGeom;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const Space: TODESpace);
    procedure SetGeom(const Geom: TODEGeom);
    function GetGeom: TODEGeom;
    procedure SetCleanup(const mode: Integer);
    function GetCleanup: Integer;
  end;

implementation

// --------------------- TPointerList ---------------------

function TPointerList.GetItems(i: integer): Pointer;
begin
   result := Get(i);
end;

procedure TPointerList.SetItems(i: integer; const Value: Pointer);
begin
   Put(i, Value);
end;

// --------------------- TODEWorld ---------------------

constructor TODEWorld.Create;
begin
   pWorld := dWorldCreate();
end;

destructor TODEWorld.Destroy;
begin
   dWorldDestroy(pWorld);
   inherited Destroy;
end;

function TODEWorld.GetPointer: PdxWorld;
begin
   result := pWorld;
end;

procedure TODEWorld.SetCFM(const cfm: TdReal);
begin
   dWorldSetCFM(pWorld, cfm);
end;

function TODEWorld.GetCFM: TdReal;
begin
   result := pWorld.global_cfm;
end;

procedure TODEWorld.SetERP(const erp: TdReal);
begin
   dWorldSetERP(pWorld, erp);
end;

function TODEWorld.GetERP: TdReal;
begin
   result := pWorld.global_erp;
end;

procedure TODEWorld.SetGravity(const x, y, z: TdReal);
begin
   dWorldSetGravity(pWorld, x, y, z);
end;

function TODEWorld.GetGravity: TdVector3;
begin
   result := pWorld.gravity;
end;

procedure TODEWorld.Step(const stepsize: TdReal);
begin
   dWorldStep(pWorld, stepsize);
end;

function TODEWorld.ImpulseToForce(const stepsize, ix, iy, iz: TdReal): TdVector3;
begin
   dWorldImpulseToForce(pWorld, stepsize, ix, iy, iz, result);
end;

// --------------------- TODEBody ---------------------

constructor TODEBody.Create(const World: TODEWorld);
begin
   pBody := dBodyCreate(World.GetPointer);
   pODEJoint := TPointerList.Create;
end;

destructor TODEBody.Destroy;
begin
   pODEJoint.Free;
   dBodyDestroy(pBody);
   inherited Destroy;
end;

function TODEBody.GetPointer: PdxBody;
begin
   result := pBody;
end;

procedure TODEBody.AddForce(const fx, fy, fz: TdReal);
begin
   dBodyAddForce(pBody, fx, fy, fz);
end;

procedure TODEBody.AddForceAtPos(const fx, fy, fz, px, py, pz: TdReal);
begin
   dBodyAddForceAtPos(pBody, fx, fy, fz, px, py, pz);
end;

procedure TODEBody.AddForceAtRelPos(const fx, fy, fz, px, py, pz: TdReal);
begin
   dBodyAddForceAtRelPos(pBody, fx, fy, fz, px, py, pz);
end;

procedure TODEBody.AddRelForce(const fx, fy, fz: TdReal);
begin
   dBodyAddRelForce(pBody, fx, fy, fz);
end;

procedure TODEBody.AddRelForceAtPos(const fx, fy, fz, px, py, pz: TdReal);
begin
   dBodyAddRelForceAtPos(pBody, fx, fy, fz, px, py, pz);
end;

procedure TODEBody.AddRelForceAtRelPos(const fx, fy, fz, px, py, pz: TdReal);
begin
   dBodyAddRelForceAtRelPos(pBody, fx, fy, fz, px, py, pz);
end;

procedure TODEBody.AddRelTorque(const fx, fy, fz: TdReal);
begin
   dBodyAddRelTorque(pBody, fx, fy, fz);
end;

procedure TODEBody.AddTorque(const fx, fy, fz: TdReal);
begin
   dBodyAddTorque(pBody, fx, fy, fz);
end;

procedure TODEBody.Disable;
begin
   dBodyDisable(pBody);
end;

procedure TODEBody.Enable;
begin
    dBodyEnable(pBody);
end;

function TODEBody.GetAngularVel: TdVector3;
begin
   result := pBody.avel;
end;

function TODEBody.GetFiniteRotationAxis: TdVector3;
begin
   result := pBody.finite_rot_axis;
end;

function TODEBody.GetFiniteRotationMode: Integer;
begin
   //enum would be better :(
   if ((pBody.flags AND dxBodyFlagFiniteRotation) = 0) then
      result := dxBodyFlagFiniteRotationAxis
   else
      result := dxBodyFlagFiniteRotation;
end;

function TODEBody.GetForce: TdVector3;
begin
   result := pBody.facc;
end;

function TODEBody.GetGravityMode: Integer;
begin
   //enum would be better :(
   if ((pBody.flags AND dxBodyNoGravity) = 0) then
      result := 0  //Object is influenced by gravity
   else
      result := dxBodyNoGravity;
end;

function TODEBody.GetJoint(const index: Integer): TODEJoint;
begin
   result := TODEJoint(pODEJoint[index]^);
end;

function TODEBody.GetLinearVel: TdVector3;
begin
   result := pBody.lvel;
end;

function TODEBody.GetMass: Pointer;
begin
   dBodyGetMass(pBody, result);
end;

function TODEBody.GetNumJoints: Integer;
begin
   result := dBodyGetNumJoints(pBody);
end;

function TODEBody.GetPointVel(const px, py, pz: TdReal): TdVector3;
begin
   dBodyGetPointVel(pBody, px, py, pz, result);
end;

function TODEBody.GetPosRelPoint(const px, py, pz: TdReal): TdVector3;
begin
   dBodyGetPosRelPoint(pBody, px, py, pz, result);
end;

function TODEBody.GetPosition: TdVector3;
begin
   result := pBody.pos;
end;

function TODEBody.GetQuaternion: TdQuaternion;
begin
   result := pBody.q;
end;

function TODEBody.GetRelPointPos(const px, py, pz: TdReal): TdVector3;
begin
   dBodyGetRelPointPos(pBody, px, py, pz, result);
end;

function TODEBody.GetRelPointVel(const px, py, pz: TdReal): TdVector3;
begin
   dBodyGetRelPointVel(pBody, px, py, pz, result);
end;

function TODEBody.GetRotation: TdMatrix3;
begin
   result := pBody.R;
end;

function TODEBody.GetTorque: TdVector3;
begin
   result := pBody.tacc;
end;

function TODEBody.IsEnabled: Boolean;
begin
   if ((pBody.flags AND dxBodyDisabled) = 0) then
      result := True
   else
      result := False;
end;

procedure TODEBody.SetAngularVel(const x, y, z: TdReal);
begin
   dBodySetAngularVel(pBody, x, y, z);
end;

procedure TODEBody.SetFiniteRotationAxis(const x, y, z: TdReal);
begin
   dBodySetFiniteRotationAxis(pBody, x, y, z);
end;

procedure TODEBody.SetFiniteRotationMode(const mode: Integer);
begin
   dBodySetFiniteRotationMode(pBody, mode);
end;

procedure TODEBody.SetForce(const x, y, z: TdReal);
begin
   dBodySetForce(pBody, x, y, z);
end;

procedure TODEBody.SetGravityMode(const mode: Integer);
begin
   dBodySetGravityMode(pBody, mode);
end;

procedure TODEBody.SetLinearVel(const x, y, z: TdReal);
begin
   dBodySetLinearVel(pBody, x, y, z);
end;

procedure TODEBody.SetMass(const mass: TdMass);
begin
   dBodySetMass(pBody, mass);
end;

procedure TODEBody.SetPosition(const x, y, z: TdReal);
begin
   dBodySetPosition(pBody, x, y, z);
end;

procedure TODEBody.SetQuaternion(const q: TdQuaternion);
begin
   dBodySetQuaternion(pBody, q);
end;

procedure TODEBody.SetRotation(const R: TdMatrix3);
begin
   dBodySetRotation(pBody, R);
end;

procedure TODEBody.SetTorque(const x, y, z: TdReal);
begin
   dBodySetTorque(pBody, x, y, z);
end;

function TODEBody.VectorFromWorld(const px, py, pz: TdReal): TdVector3;
begin
   dBodyVectorFromWorld(pBody, px, py, pz, result);
end;

function TODEBody.VectorToWorld(const px, py, pz: TdReal): TdVector3;
begin
   dBodyVectorToWorld(pBody, px, py, pz, result);
end;

function TODEBody.IsConnectedTo(const b: TODEBody): Boolean;
begin
   result := (dAreConnected(pBody, b.GetPointer) = 1);
end;

procedure TODEBody.SetData(const data: Pointer);
begin
   dBodySetData(pBody, data);
end;

function TODEBody.GetData: Pointer;
begin
   result := pBody.BaseObject.userdata;
end;

// --------------------- TODEJointGroup ---------------------

constructor TODEJointGroup.Create(const max_size: Integer);
begin
   pJointGroup := dJointGroupCreate(max_size);
end;

destructor TODEJointGroup.Destroy;
begin
   dJointGroupDestroy(pJointGroup);
   inherited Destroy;
end;

procedure TODEJointGroup.Empty;
begin
   dJointGroupEmpty(pJointGroup);
end;

function TODEJointGroup.GetPointer: TdJointGroupID;
begin
   result := pJointGroup;
end;

// --------------------- TODEJoint ---------------------

destructor TODEJoint.Destroy;
begin
   pODEBody.Free;
   dJointDestroy(pJoint);
   inherited Destroy;
end;

function TODEJoint.GetPointer: TdJointID;
begin
   result := pJoint;
end;

procedure TODEJoint.Attach(const body1, body2: TODEBody);
begin
   dJointAttach(pJoint, body1.GetPointer, body2.GetPointer);

   if (body1.GetPointer <> nil) then
   begin
      pODEBody.Add(@body1);
      body1.pODEJoint.Add(@self);
   end;
   if (body2.GetPointer <> nil) then
   begin
      pODEBody.Add(@body2);
      body2.pODEJoint.Add(@self);
   end;
end;

procedure TODEJoint.Attach(const pbody1, pbody2: PdxBody);
begin
   dJointAttach(pJoint, pBody1, pBody2);

   //if (body1.GetPointer <> nil) then
   //begin
   //   pODEBody.Add(@body1);
   //   body1.pODEJoint.Add(@self);
   //end;
   //if (body2.GetPointer <> nil) then
   //begin
   //   pODEBody.Add(@body2);
   //   body2.pODEJoint.Add(@self);
   //end;
end;

function TODEJoint.GetType: Integer;
begin
   result := dJointGetType(pJoint);
end;

function TODEJoint.GetBody(const index: Integer): TODEBody;
begin
   result := TODEBody(pODEBody.Items[index]^);
end;

procedure TODEJoint.SetData(const data: Pointer);
begin
   dJointSetData(pJoint, data);
end;

function TODEJoint.GetData: Pointer;
begin
   result := dJointGetData(pJoint);
end;

// --------------------- TODEJointBall ---------------------

constructor TODEJointBall.Create(const World: TODEWorld; const JointGroup: TODEJointGroup);
begin
   if Assigned(JointGroup) then
      pJoint := dJointCreateBall(World.GetPointer, JointGroup.GetPointer)
   else
      pJoint := dJointCreateBall(World.GetPointer, 0);

   pODEBody := TPointerList.Create;
end;

procedure TODEJointBall.SetAnchor(const x, y, z: TdReal);
begin
   dJointSetBallAnchor(pJoint, x, y, z);
end;

function TODEJointBall.GetAnchor: TdVector3;
begin
   dJointGetBallAnchor(pJoint, result);
end;

// --------------------- TODEJointHinge ---------------------

constructor TODEJointHinge.Create(const World: TODEWorld; const JointGroup: TODEJointGroup);
begin
   if Assigned(JointGroup) then
      pJoint := dJointCreateHinge(World.GetPointer, JointGroup.GetPointer)
   else
      pJoint := dJointCreateHinge(World.GetPointer, 0);

   pODEBody := TPointerList.Create;
end;

procedure TODEJointHinge.SetAnchor(const x, y, z: TdReal);
begin
   dJointSetHingeAnchor(pJoint, x, y, z);
end;

function TODEJointHinge.GetAnchor: TdVector3;
begin
   dJointGetHingeAnchor(pJoint, result);
end;

procedure TODEJointHinge.SetAxis(const x, y, z: TdReal);
begin
   dJointSetHingeAxis(pJoint, x, y, z);
end;

function TODEJointHinge.GetAxis: TdVector3;
begin
   dJointGetHingeAxis(pJoint, result);
end;

function TODEJointHinge.GetAngle: TdReal;
begin
   result := dJointGetHingeAngle(pJoint);
end;

function TODEJointHinge.GetAngleRate: TdReal;
begin
   result := dJointGetHingeAngleRate(pJoint);
end;

procedure TODEJointHinge.SetParam(const parameter: Integer; const value: TdReal);
begin
   dJointSetHingeParam(pJoint, parameter, value);
end;

function TODEJointHinge.GetParam(const parameter: Integer): TdReal;
begin
   result := dJointGetHingeParam(pJoint, parameter);
end;

// --------------------- TODEJointHinge2 ---------------------

constructor TODEJointHinge2.Create(const World: TODEWorld; const JointGroup: TODEJointGroup);
begin
   if Assigned(JointGroup) then
      pJoint := dJointCreateHinge2(World.GetPointer, JointGroup.GetPointer)
   else
      pJoint := dJointCreateHinge2(World.GetPointer, 0);

   pODEBody := TPointerList.Create;
end;

procedure TODEJointHinge2.SetAnchor(const x, y, z: TdReal);
begin
   dJointSetHinge2Anchor(pJoint, x, y, z);
end;

function TODEJointHinge2.GetAnchor: TdVector3;
begin
   dJointGetHinge2Anchor(pJoint, result);
end;

procedure TODEJointHinge2.SetAxis1(const x, y, z: TdReal);
begin
   dJointSetHinge2Axis1(pJoint, x, y, z);
end;

procedure TODEJointHinge2.SetAxis2(const x, y, z: TdReal);
begin
   dJointSetHinge2Axis2(pJoint, x, y, z);
end;

function TODEJointHinge2.GetAxis1: TdVector3;
begin
   dJointGetHinge2Axis1(pJoint, result);
end;

function TODEJointHinge2.GetAxis2: TdVector3;
begin
   dJointGetHinge2Axis2(pJoint, result);
end;

function TODEJointHinge2.GetAngle1: TdReal;
begin
   result := dJointGetHinge2Angle1(pJoint);
end;

function TODEJointHinge2.GetAngle1Rate: TdReal;
begin
   result := dJointGetHinge2Angle1Rate(pJoint);
end;

function TODEJointHinge2.GetAngle2Rate: TdReal;
begin
   result := dJointGetHinge2Angle2Rate(pJoint);
end;

procedure TODEJointHinge2.SetParam(const parameter: Integer; const value: TdReal);
begin
   dJointSetHinge2Param(pJoint, parameter, value);
end;

function TODEJointHinge2.GetParam(const parameter: Integer): TdReal;
begin
   result := dJointGetHinge2Param(pJoint, parameter);
end;

// --------------------- TODEJointContact ---------------------

constructor TODEJointContact.Create(const World: TODEWorld; const JointGroup: TODEJointGroup; const dContact: TdContact);
begin
   if Assigned(JointGroup) then
      pJoint := dJointCreateContact(World.GetPointer, JointGroup.GetPointer, dContact)
   else
      pJoint := dJointCreateContact(World.GetPointer, 0, dContact);

   pODEBody := TPointerList.Create;
end;

// --------------------- TODEJointSlider ---------------------

constructor TODEJointSlider.Create(const World: TODEWorld; const JointGroup: TODEJointGroup);
begin
   if Assigned(JointGroup) then
      pJoint := dJointCreateSlider(World.GetPointer, JointGroup.GetPointer)
   else
      pJoint := dJointCreateSlider(World.GetPointer, 0);

   pODEBody := TPointerList.Create;
end;

procedure TODEJointSlider.SetAxis(const x, y, z: TdReal);
begin
   dJointSetSliderAxis(pJoint, x, y, z);
end;

function TODEJointSlider.GetAxis: TdVector3;
begin
   dJointGetSliderAxis(pJoint, result);
end;

function TODEJointSlider.GetPosition: TdReal;
begin
   result := dJointGetSliderPosition(pJoint);
end;

function TODEJointSlider.GetPositionRate: TdReal;
begin
   result := dJointGetSliderPositionRate(pJoint);
end;

procedure TODEJointSlider.SetParam(const parameter: Integer; const value: TdReal);
begin
   dJointSetSliderParam(pJoint, parameter, value);
end;

function TODEJointSlider.GetParam(const parameter: Integer): TdReal;
begin
   result := dJointGetSliderParam(pJoint, parameter);
end;

// --------------------- TODEJointFixed ---------------------

constructor TODEJointFixed.Create(const World: TODEWorld; const JointGroup: TODEJointGroup);
begin
   if Assigned(JointGroup) then
      pJoint := dJointCreateFixed(World.GetPointer, JointGroup.GetPointer)
   else
      pJoint := dJointCreateFixed(World.GetPointer, 0);

   pODEBody := TPointerList.Create;
end;

procedure TODEJointFixed.SetFixed;
begin
   dJointSetFixed(pJoint);
end;

// --------------------- TODEJointAMotor ---------------------

constructor TODEJointAMotor.Create(const World: TODEWorld; const JointGroup: TODEJointGroup);
begin
   if Assigned(JointGroup) then
      pJoint := dJointCreateAMotor(World.GetPointer, JointGroup.GetPointer)
   else
      pJoint := dJointCreateAMotor(World.GetPointer, 0);

   pODEBody := TPointerList.Create;
end;

procedure TODEJointAMotor.SetAngle(const anum: Integer; const angle: TdReal);
begin
   dJointSetAMotorAngle(pJoint, anum, angle);
end;

function TODEJointAMotor.GetAngle(const anum: Integer): TdReal;
begin
   result := dJointGetAMotorAngle(pJoint, anum);
end;

function TODEJointAMotor.GetAngleRate(const anum: Integer): TdReal;
begin
   result := dJointGetAMotorAngleRate(pJoint, anum);
end;

procedure TODEJointAMotor.SetAxis(const anum, rel: Integer; const x, y, z: TdReal);
begin
   dJointSetAMotorAxis(pJoint, anum, rel, x, y, z);
end;

function TODEJointAMotor.GetAxis(const anum: Integer): TdVector3;
begin
   dJointGetAMotorAxis(pJoint, anum, result);
end;

function TODEJointAMotor.GetAxisRel(const anum: Integer): Integer;
begin
   result := dJointGetAMotorAxisRel(pJoint, anum);
end;

procedure TODEJointAMotor.SetMode(const mode: TdAngularMotorModeNumbers);
begin
   dJointSetAMotorMode(pJoint, mode);
end;

function TODEJointAMotor.GetMode: Integer;
begin
   result := dJointGetAMotorMode(pJoint);
end;

procedure TODEJointAMotor.SetNumAxes(const num: Integer);
begin
   dJointSetAMotorNumAxes(pJoint, num);
end;

function TODEJointAMotor.GetNumAxes: Integer;
begin
   result := dJointGetAMotorNumAxes(pJoint);
end;

procedure TODEJointAMotor.SetParam(const parameter: Integer; const value: TdReal);
begin
   dJointSetAMotorParam(pJoint, parameter, value);
end;

function TODEJointAMotor.GetParam(const parameter: Integer): TdReal;
begin
   result := dJointGetAMotorParam(pJoint, parameter);
end;

// --------------------- TODEJointUniversal ---------------------

constructor TODEJointUniversal.Create(const World: TODEWorld; const JointGroup: TODEJointGroup);
begin
   if Assigned(JointGroup) then
      pJoint := dJointCreateUniversal(World.GetPointer, JointGroup.GetPointer)
   else
      pJoint := dJointCreateUniversal(World.GetPointer, 0);

   pODEBody := TPointerList.Create;
end;

procedure TODEJointUniversal.SetAnchor(const x, y, z: TdReal);
begin
   dJointSetUniversalAnchor(pJoint, x, y, z);
end;

function TODEJointUniversal.GetAnchor: TdVector3;
begin
   dJointGetUniversalAnchor(pJoint, result);
end;

procedure TODEJointUniversal.SetAxis1(const x, y, z: TdReal);
begin
   dJointSetUniversalAxis1(pJoint, x, y, z);
end;

procedure TODEJointUniversal.SetAxis2(const x, y, z: TdReal);
begin
   dJointSetUniversalAxis2(pJoint, x, y, z);
end;

function TODEJointUniversal.GetAxis1: TdVector3;
begin
   dJointGetUniversalAxis1(pJoint, result);
end;

function TODEJointUniversal.GetAxis2: TdVector3;
begin
   dJointGetUniversalAxis2(pJoint, result);
end;

// --------------------- TODEGeom ---------------------

destructor TODEGeom.Destroy;
begin
   pODEBody.Free;
   dGeomDestroy(pGeom);
   inherited Destroy;
end;

function TODEGeom.GetPointer: PdxGeom;
begin
   result := pGeom;
end;

function TODEGeom.Collide(Geom: TODEGeom; flags: Integer; var Contact: TdContactGeom; Skip: Integer): Integer;
begin
   result := dCollide(pGeom, Geom.GetPointer, flags, Contact, Skip);
end;

function TODEGeom.GetAABB: TdAABB;
begin
   pGeom._class^.aabb(pGeom, result);
end;

procedure TODEGeom.SetBody(Body: TODEBody);
begin
   dGeomSetBody(pGeom, Body.GetPointer);

   if (Body.GetPointer <> nil) then
      pODEBody := @Body
   else if Assigned(pGeom.Body) then
     pODEBody := nil;
end;

function TODEGeom.GetBody: TODEBody;
begin
   if Assigned(pODEBody) then
      result := pODEBody^
   else
      result := nil;
end;

function TODEGeom.GetClass: Integer;
begin
   result := pGeom._class^.num;
end;

function TODEGeom.GetClassData: Pointer;
begin
   result := dGeomGetClassData(pGeom);
end;

procedure TODEGeom.SetPosition(const x, y, z: TdReal);
begin
   dGeomSetPosition(pGeom, x, y, z);
end;

function TODEGeom.GetPosition: PdVector3;
begin
   result := pGeom.Pos
end;

procedure TODEGeom.SetRotation(const R: TdMatrix3);
begin
   dGeomSetRotation(pGeom, R);
end;

function TODEGeom.GetRotation: PdMatrix3;
begin
   result := pGeom.R;
end;

function TODEGeom.GetSpaceAABB: TdReal;
begin
   result := TdReal(pGeom.space_aabb^);
end;

procedure TODEGeom.SetData(const data: Pointer);
begin
   dGeomSetData(pGeom, data);
end;

function TODEGeom.GetData: Pointer;
begin
   result := dGeomGetData(pGeom);
end;

// --------------------- TODESpace ---------------------

destructor TODESpace.Destroy;
begin
   dSpaceDestroy(_pSpace);
   inherited Destroy;
end;

function TODESpace.GetPointer: PdxSpace;
begin
   result := _pSpace;
end;

procedure TODESpace.Add(const Geom: TODEGeom);
begin
   dSpaceAdd(_pSpace, Geom.GetPointer);
end;

procedure TODESpace.Remove(const Geom: TODEGeom);
begin
   dSpaceRemove(_pSpace, Geom.GetPointer);
end;

function TODESpace.Query(const Geom: TODEGeom): Integer;
begin
   result := dSpaceQuery(_pSpace, Geom.GetPointer);
end;

procedure TODESpace.Collide(data: pointer; callback: TdNearCallback);
begin
   dSpaceCollide(_pSpace, data, callback);
end;

constructor TODESpace.Create(aSpace: TODESpace);
begin
  Assert(false, 'You should never create TODESpace directly, use TODESpaceSimple or TODESpaceHash instead!');
end;

// --------------------- TODESpaceSimple ---------------------

constructor TODESpaceSimple.Create(aSpace:TODESpace);
begin
  ParentODESpace := aSpace;

  if Assigned(aSpace) then
    pSpace := PdxSimpleSpace(dSimpleSpaceCreate(aSpace._pSpace))
  else
    pSpace := PdxSimpleSpace(nil);

  _pSpace := PdxSpace(pSpace);
end;

// --------------------- TODESpaceHash ---------------------

constructor TODESpaceHash.Create(aSpace:TODESpace);
begin
  ParentODESpace := aSpace;

  if Assigned(aSpace) then
    pSpace := PdxHashSpace(dHashSpaceCreate(aSpace._pSpace))
  else
    pSpace := PdxHashSpace(dHashSpaceCreate(nil));

  _pSpace := PdxSpace(pSpace);
end;

function TODESpaceHash.GetMaxLevel: integer;
begin
  result := pSpace.global_maxlevel;
end;

function TODESpaceHash.GetMinLevel: integer;
begin
  result := pSpace.global_minlevel;
end;

procedure TODESpaceHash.SetLevels(const minlevel, maxlevel: Integer);
begin
   dHashSpaceSetLevels(_pSpace, minlevel, maxlevel);
end;

// --------------------- TODESphere ---------------------

constructor TODESphere.Create(const Space: TODESpace; const radius: TdReal);
begin
   pGeom := dCreateSphere(Space.GetPointer, radius);
end;

procedure TODESphere.SetRadius(const radius: TdReal);
begin
   dGeomSphereSetRadius(pGeom, radius);
end;

function TODESphere.GetRadius: TdReal;
begin
   result := dGeomSphereGetRadius(pGeom);
end;

// --------------------- TODEBox ---------------------

constructor TODEBox.Create(const Space: TODESpace; const lx, ly, lz: TdReal);
begin
   pGeom := dCreateBox(Space.GetPointer, lx, ly, lz);
end;

procedure TODEBox.SetLengths(const lx, ly, lz: TdReal);
begin
   dGeomBoxSetLengths(pGeom, lx, ly, lz);
end;

function TODEBox.GetLengths: TdVector3;
begin
   dGeomBoxGetLengths(pGeom, result);
end;

// --------------------- TODEPlane ---------------------

constructor TODEPlane.Create(const Space: TODESpace; const a, b, c, d: TdReal);
begin
   pGeom := dCreatePlane(Space.GetPointer, a, b, c, d);
end;

procedure TODEPlane.SetParams(const a, b, c, d: TdReal);
begin
   dGeomPlaneSetParams(pGeom, a, b, c, d);
end;

function TODEPlane.GetParams: TdVector4;
begin
   dGeomPlaneGetParams(pGeom, result);
end;

// --------------------- TODECCylinder ---------------------

constructor TODECCylinder.Create(const Space: TODESpace; const radius, length: TdReal);
begin
   pGeom := dCreateCCylinder(Space.GetPointer, radius, length);
end;

procedure TODECCylinder.SetParams(const radius, length: TdReal);
begin
   dGeomCCylinderSetParams(pGeom, radius, length);
end;

procedure TODECCylinder.GetParams(var radius, length: TdReal);
begin
   dGeomCCylinderGetParams(pGeom, radius, length);
end;

// --------------------- TODECylinder ---------------------

constructor TODECylinder.Create(const Space: TODESpace; const radius, length: TdReal);
begin
   pGeom := dCreateCylinder(Space.GetPointer, radius, length);
end;

procedure TODECylinder.SetParams(const radius, length: TdReal);
begin
   //dGeomCylinderSetParams(pGeom, radius, length);
end;

procedure TODECylinder.GetParams(var radius, length: TdReal);
begin
   //dGeomCylinderGetParams(pGeom, radius, length);
end;


// --------------------- TODEGeomTransform ---------------------

constructor TODEGeomTransform.Create(const Space: TODESpace);
begin
   pGeom := dCreateGeomTransform(Space.GetPointer);
end;

procedure TODEGeomTransform.SetGeom(const Geom: TODEGeom);
begin
   dGeomTransformSetGeom(pGeom, Geom.GetPointer);

   pODEGeom := @Geom;
end;

function TODEGeomTransform.GetGeom: TODEGeom;
begin
   result := TODEGeom(pODEGeom^);
end;

procedure TODEGeomTransform.SetCleanup(const mode: Integer);
begin
   dGeomTransformSetCleanup(pGeom, mode);
end;

function TODEGeomTransform.GetCleanup: Integer;
begin
   result := dGeomTransformGetCleanup(pGeom);
end;
end.

