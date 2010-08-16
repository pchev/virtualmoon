//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFile3DS<p>

	3DStudio 3DS vector file format implementation.<p>

	<b>History :</b><font size=-1><ul>
      <li>08/11/09 - DaStr - Improved FPC compatibility
                              (thanks Predator) (BugtrackerID = 2893580)  
      <li>07/06/08 - DaStr - Added vGLFile3DS_EnableAnimation option
                             Implemented TGLFile3DSDummyObject.ExtractTriangles() 
      <li>29/04/08 - DaStr - Fixed memory leak in TGLFile3DSCameraObject
      <li>27/04/08 - DaStr - TGL3DSVectorFile.UseTextureEx converted into a
                             global variable and disabled by default
      <li>12/04/08 - DaStr - Added TGL3DSVectorFile.UseTextureEx option
                             (Bugtracker ID = 1940451)
      <li>06/04/08 - DaStr - Added animation support (by Lexer, Luca Burlizzi,
                              Dave Gravel, mif, Oxygen and a bit myself)
      <li>05/11/07 - DaStr - Fixed transparency issue.
      <li>31/03/07 - DaStr - Added $I GLScene.inc
      <li>24/03/07 - DaStr - Added explicit pointer dereferencing
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
      <li>28/01/07 - DaStr - Added transparency and opacity texture support (thanks DIVON)
      <li>09/12/04 - LR - Add Integer cast line 94 for Linux
      <li>25/10/04 - SG - Added lightmap (3DS IllumMap) support
      <li>05/06/03 - SG - Separated from GLVectorFileObjects.pas
	</ul></font>
}
unit GLFile3DS;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes, SysUtils, Math,

  // GLScene
  GLScene,  GLObjects,  GLVectorFileObjects, GLTexture, ApplicationFileIO,
  VectorGeometry, File3DS, Types3DS, OpenGL1x, PersistentClasses,
  GLStrings, GLFile3DSSceneObjects, GLCrossPlatform, VectorLists,
  GLRenderContextInfo, GLMaterial;

type

  EGLFile3DS = class(Exception);

  {: TGLFile3DSAnimationData.
     A record that holds all the information that is used during 3ds animation. }
  TGLFile3DSAnimationData = packed record
    ModelMatrix: TMatrix;
    Color: TVector;            // Omni Light.
    TargetPos: TAffineVector;  // Spot Light.
    SpotLightCutOff: Single;
    HotSpot: Single;
    Roll: Single;
  end;

  {: TGLFile3DSAnimationKeys.

     An abstract class that describes how to interpolate animation keys. }
  TGLFile3DSAnimationKeys = class(TPersistentObject)
  private
    FNumKeys: Integer;
    FKeys: array of TKeyHeader3DS;
    procedure InterpolateFrame(var I: Integer; var w: Real; const AFrame: Real);
  protected
    function InterpolateValue(const AValues: array of Single; const AFrame: Real): Single; overload;
    function InterpolateValue(const AValues: array of TAffineVector; const AFrame: Real): TAffineVector; overload;
    function InterpolateValue(const AValues: array of TKFRotKey3DS; const AFrame: Real): TMatrix; overload;
  public
    procedure LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer); virtual;
    procedure Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real); virtual; abstract;

    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TGLFile3DSScaleAnimationKeys = class(TGLFile3DSAnimationKeys)
  private
    FScale: array of TAffineVector;
  public
    procedure LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TGLFile3DSRotationAnimationKeys = class(TGLFile3DSAnimationKeys)
  private
    FRot: array of TKFRotKey3DS;
  public
    procedure LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TGLFile3DSPositionAnimationKeys = class(TGLFile3DSAnimationKeys)
  private
    FPos: array of TAffineVector;
  public
    procedure LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TGLFile3DSColorAnimationKeys = class(TGLFile3DSAnimationKeys)
  private
    FCol: array of TAffineVector;
  public
    procedure LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TTGLFile3DSPositionAnimationKeys = class(TGLFile3DSAnimationKeys)
  private
    FTPos: array of TAffineVector;
  public
    procedure LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TGLFile3DSSpotLightCutOffAnimationKeys = class(TGLFile3DSAnimationKeys)
  private
    FFall: array of Single;
  public
    procedure LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TGLFile3DSLightHotSpotAnimationKeys = class(TGLFile3DSAnimationKeys)
  private
    FHot: array of Single;
  public
    procedure LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TGLFile3DSRollAnimationKeys = class(TGLFile3DSAnimationKeys)
  private
    FRoll: array of Single;
  public
    procedure LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer); override;
    procedure Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
  end;

  TGLFile3DSAnimationKeyList = class(TPersistentObject)
  private
    FAnimKeysList: array of TGLFile3DSAnimationKeys;
  protected
    procedure ApplyAnimKeys(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real);
  public
    procedure AddKeys(const AItem: TGLFile3DSAnimationKeys);
    procedure ClearKeys;

    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
    destructor Destroy; override;
  end;

  {: Used only for serialization. There probably is a more efficient way to do it. }
  TGLFile3DSAnimKeysClassType = (ctScale, ctRot, ctPos, ctCol, ctTPos, ctFall, ctHot, ctRoll);

  {: TGLFile3DSDummyObject. A 3ds-specific TMorphableMeshObject. }
  TGLFile3DSDummyObject = class(TMorphableMeshObject)
  private
    FAnimList: TGLFile3DSAnimationKeyList;
    FAnimData: Pointer;
    FRefTranf, FAnimTransf: TGLFile3DSAnimationData;
    FParent: TGLFile3DSDummyObject;
    FParentName: string[64];
  public
    procedure LoadAnimation(const AData: Pointer); virtual;
    procedure SetFrame(const AFrame: Real); virtual;

    procedure MorphTo(morphTargetIndex: Integer); override;
    procedure Lerp(morphTargetIndex1, morphTargetIndex2: Integer; lerpFactor: Single); override;
    procedure GetExtents(var min, max: TAffineVector); override;
    function ExtractTriangles(texCoords : TAffineVectorList = nil;
                              normals : TAffineVectorList = nil) : TAffineVectorList; override;

    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
    procedure Assign(Source: TPersistent); override;
    constructor Create; override;
    destructor Destroy; override;

    property AnimList: TGLFile3DSAnimationKeyList read FAnimList;
    property Parent: TGLFile3DSDummyObject read FParent write FParent;
    property RefrenceTransf: TGLFile3DSAnimationData read FRefTranf write FRefTranf;
  end;

  {: TGLFile3DSDummyObject. A 3ds-specific mesh object. }
  TGLFile3DSMeshObject = class(TGLFile3DSDummyObject)
  private
  public
    procedure LoadAnimation(const AData: Pointer); override;
    procedure BuildList(var ARci: TRenderContextInfo); override;
  end;

  {: TGLFile3DSDummyObject. A 3ds-specific omni light. }
  TGLFile3DSOmniLightObject = class(TGLFile3DSDummyObject)
  private
    FLightSrc: TGLFile3DSLight;
    FLightSrcName: string[64];
  public
    constructor Create; override;
    procedure LoadData(const AOwner: TGLBaseMesh; const AData: PLight3DS); virtual;
    procedure LoadAnimation(const AData: Pointer); override;
    procedure SetFrame(const AFrame: Real); override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
    destructor Destroy; override;
  end;

  {: TGLFile3DSSpotLightObject. A 3ds-specific spot light. }
  TGLFile3DSSpotLightObject = class(TGLFile3DSOmniLightObject)
  public
    procedure LoadData(const AOwner: TGLBaseMesh; const AData: PLight3DS); override;
    procedure LoadAnimation(const AData: Pointer); override;
    procedure SetFrame(const AFrame: Real); override;
  end;

  {: TGLFile3DSCameraObject. A 3ds-specific camera. }
  TGLFile3DSCameraObject = class(TGLFile3DSDummyObject)
  private
    FTargetObj: TGLDummyCube;
    FCameraSrc: TGLFile3DSCamera;
    FCameraSrcName: string[64];
  public
    constructor Create; override;
    procedure LoadData(Owner: TGLBaseMesh; AData: PCamera3DS);
    procedure LoadAnimation(const AData: Pointer); override;
    procedure SetFrame(const AFrame: Real); override;
    procedure WriteToFiler(Writer: TVirtualWriter); override;
    procedure ReadFromFiler(Reader: TVirtualReader); override;
    destructor Destroy; override;
  end;

  // TGL3DSVectorFile
  //
  {: The 3DStudio vector file.<p>
     Uses an upgraded version if a 3DS import library by Mike Lischke.<p>
     (http://www.lishcke-online.de). A 3DS file may contain material
     information and require textures when loading. }
  TGL3DSVectorFile = class(TVectorFile)
  public
    { Public Declarations }
    class function Capabilities: TDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
  end;

var
  {: If enabled, advanced parameters will be loaded from a 3ds file
     (TextureScale, TextureOffset), but it might break backwards compatibility.
     If disabled, it won't break anything, but some parameters will not be
     loaded correctly from a 3ds file.
     Also there is a significant drop in FPS when this option is on
     (for unknown reasons), so it is off by default. }
  vGLFile3DS_UseTextureEx: Boolean = False;

  {: If enabled, allows 3ds animation and fixes loading of some 3ds models,
     but has a few bugs:
     - TGLFreeForm.AutoCentering does now work correctly.
     - TMeshObject.vertices return values different from
        TMeshObject.ExtractTriangles()
     }
  vGLFile3DS_EnableAnimation: Boolean = False;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ Misc functions ------------------
// ------------------

// AnimKeysClassTypeToClass
//
function AnimKeysClassTypeToClass(const AAnimKeysClassType: TGLFile3DSAnimKeysClassType): TClass;
begin
  case AAnimKeysClassType of
    ctScale: Result := TGLFile3DSScaleAnimationKeys;
    ctRot: Result := TGLFile3DSRotationAnimationKeys;
    ctPos: Result := TGLFile3DSPositionAnimationKeys;
    ctCol: Result := TGLFile3DSColorAnimationKeys;
    ctTPos: Result := TTGLFile3DSPositionAnimationKeys;
    ctFall: Result := TGLFile3DSSpotLightCutOffAnimationKeys;
    ctHot: Result := TGLFile3DSLightHotSpotAnimationKeys;
    ctRoll: Result := TGLFile3DSRollAnimationKeys;
    else
    begin
      Result := nil;
      Assert(False, glsErrorEx + glsUnknownType);
    end;
  end;
end;

// ClassToAnimKeysClassType
//
function ClassToAnimKeysClassType(const AAnimKeysClass: TClass): TGLFile3DSAnimKeysClassType;
begin
  if AAnimKeysClass.InheritsFrom(TGLFile3DSScaleAnimationKeys) then
    Result := ctScale
  else if AAnimKeysClass.InheritsFrom(TGLFile3DSRotationAnimationKeys) then
    Result := ctRot
  else if AAnimKeysClass.InheritsFrom(TGLFile3DSPositionAnimationKeys) then
    Result := ctPos
  else if AAnimKeysClass.InheritsFrom(TGLFile3DSColorAnimationKeys) then
    Result := ctCol
  else if AAnimKeysClass.InheritsFrom(TTGLFile3DSPositionAnimationKeys) then
    Result := ctTPos
  else if AAnimKeysClass.InheritsFrom(TGLFile3DSSpotLightCutOffAnimationKeys) then
    Result := ctFall
  else if AAnimKeysClass.InheritsFrom(TGLFile3DSLightHotSpotAnimationKeys) then
    Result := ctHot
  else if AAnimKeysClass.InheritsFrom(TGLFile3DSRollAnimationKeys) then
    Result := ctRoll
  else
  begin
    Result := ctScale;
    Assert(False, glsErrorEx + glsUnknownType);
  end;
end;

// MakeRotationQuaternion
//
function MakeRotationQuaternion(const axis: TAffineVector; angle: Single): TQuaternion;
var
  v: Tvector;
  halfAngle, invAxisLengthMult: Single;
begin
  halfAngle := (angle) / 2;
  invAxisLengthMult := 1 / VectorLength(axis) * sin(halfAngle);

  v[0] := axis[0] * invAxisLengthMult;
  v[1] := axis[1] * invAxisLengthMult;
  v[2] := axis[2] * invAxisLengthMult;
  v[3] := cos(halfAngle);

  Result.ImagPart := AffineVectorMake(v);
  Result.RealPart := v[3];
end;

// QuaternionToRotateMatrix
//
function QuaternionToRotateMatrix(const Quaternion: TQuaternion): TMatrix;
var
  wx, wy, wz, xx, yy, yz, xy, xz, zz, x2, y2, z2: Single;
  quat: TVector;
  m:    TMatrix;
begin
  quat := VectorMake(Quaternion.ImagPart);
  quat[3] := Quaternion.RealPart;

  x2 := quat[0] + quat[0];
  y2 := quat[1] + quat[1];
  z2 := quat[2] + quat[2];
  xx := quat[0] * x2;
  xy := quat[0] * y2;
  xz := quat[0] * z2;
  yy := quat[1] * y2;
  yz := quat[1] * z2;
  zz := quat[2] * z2;
  wx := quat[3] * x2;
  wy := quat[3] * y2;
  wz := quat[3] * z2;

  m[0][0] := 1.0 - (yy + zz);
  m[0][1] := xy - wz;
  m[0][2] := xz + wy;
  m[1][0] := xy + wz;
  m[1][1] := 1.0 - (xx + zz);
  m[1][2] := yz - wx;
  m[2][0] := xz - wy;
  m[2][1] := yz + wx;
  m[2][2] := 1.0 - (xx + yy);

  m[0][3] := 0;
  m[1][3] := 0;
  m[2][3] := 0;
  m[3][0] := 0;
  m[3][1] := 0;
  m[3][2] := 0;
  m[3][3] := 1;

  Result := m;
end;

// ------------------
// ------------------ Support classes ------------------
// ------------------
procedure TGLFile3DSAnimationKeys.InterpolateFrame(var I: Integer; var w: Real; const AFrame: Real);
begin
  w := 1;
  I := 0;
  if FNumKeys > 1 then
  begin
    while (FNumKeys > I) and ((FKeys[I].Time) <= AFrame) do
      Inc(I);
    if (FNumKeys > I) and ((FKeys[I - 1].Time) <= AFrame) then
      w := (AFrame - FKeys[I - 1].Time) / (FKeys[I].Time - FKeys[I - 1].Time);

    // Don't allow keys to go our of range.
    if I = FNumKeys then
      I := FNumKeys - 1;
  end;
end;

function TGLFile3DSAnimationKeys.InterpolateValue(const AValues: array of Single; const AFrame: Real): Single;
var
  I: Integer;
  w: Real;
  start, stop: Single;
begin
  InterpolateFrame(I, w, AFrame);

  if I > 0 then
    start := AValues[I - 1]
  else
    start := 0;
  if FNumKeys > I then
    stop := AValues[I]
  else
    stop := 0;

  Result := Lerp(start, stop, w);
end;

function TGLFile3DSAnimationKeys.InterpolateValue(const AValues: array of TAffineVector; const AFrame: Real): TAffineVector;
var
  I: Integer;
  w: Real;
  start, stop: TAffineVector;
begin
  InterpolateFrame(I, w, AFrame);

  if I > 0 then
    start := AValues[I - 1]
  else
    start := NullVector;
  if FNumKeys > I then
    stop := AValues[I]
  else
    stop := NullVector;

  Result := VectorLerp(start, stop, w);
end;

function TGLFile3DSAnimationKeys.InterpolateValue(const AValues: array of TKFRotKey3DS; const AFrame: Real): TMatrix;
var
  I: Integer;
  w: Real;
begin
  Result := IdentityHmgMatrix;

  // First find the final matrix for this frame.
  I := 0;
  while (FNumKeys > I) and ((FKeys[I].Time) < AFrame) do
  begin
    with AValues[I] do
      Result := MatrixMultiply(Result, CreateRotationMatrix(AffineVectorMake(X, Y, Z), Angle));
    Inc(I);
  end;

  InterpolateFrame(I, w, AFrame);

  // Then interpolate this matrix
  if (FNumKeys > I) and ((FKeys[I].Time) > AFrame) and ((FKeys[I - 1].Time) < AFrame) then
  begin
    with AValues[I] do
      Result := MatrixMultiply(Result, CreateRotationMatrix(AffineVectorMake(X, Y, Z), AngleLerp(0, Angle, w)));
  end;
end;

procedure TGLFile3DSAnimationKeys.LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: Integer;
begin
  FNumKeys := ANumKeys;
  SetLength(FKeys, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FKeys[I] := Keys[I];
end;

procedure TGLFile3DSAnimationKeys.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TGLFile3DSAnimationKeys then
  begin
    FNumKeys := TGLFile3DSAnimationKeys(Source).FNumKeys;
    SetLength(FKeys, FNumKeys);
    for I := 0 to FNumKeys - 1 do
      FKeys[I] := TGLFile3DSAnimationKeys(Source).FKeys[I];
  end
  else
    inherited Assign(Source);
end;

procedure TGLFile3DSAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  Writer.WriteInteger(FNumKeys);
  if FNumKeys > 0 then
    Writer.write(FKeys[0], FNumKeys * SizeOf(TKeyHeader3DS));
end;

procedure TGLFile3DSAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  FNumKeys := Reader.ReadInteger;
  SetLength(FKeys, FNumKeys);
  if FNumKeys > 0 then
    Reader.read(FKeys[0], FNumKeys * SizeOf(TKeyHeader3DS));
end;


procedure TGLFile3DSScaleAnimationKeys.LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: Integer;
begin
  inherited;
  SetLength(FScale, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FScale[I] := TAffineVector(PPointList(AData)[I]);
end;

procedure TGLFile3DSScaleAnimationKeys.Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real);
begin
  if FNumKeys > 0 then
    DataTransf.ModelMatrix := MatrixMultiply(DataTransf.ModelMatrix, CreateScaleMatrix(InterpolateValue(FScale, AFrame)));
end;

procedure TGLFile3DSScaleAnimationKeys.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited;

  SetLength(FScale, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FScale[I] := (Source as TGLFile3DSScaleAnimationKeys).Fscale[I];
end;

procedure TGLFile3DSScaleAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.write(FScale[0], FNumKeys * SizeOf(TPoint3DS));
end;

procedure TGLFile3DSScaleAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FScale, FNumKeys);
  if FNumKeys > 0 then
    Reader.read(FScale[0], FNumKeys * SizeOf(TPoint3DS));
end;


procedure TGLFile3DSRotationAnimationKeys.LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer);
var
  I:   Integer;
  Rot: PKFRotKeyList;
begin
  inherited;

  SetLength(FRot, FNumKeys);
  Rot := PKFRotKeyList(AData);
  for I := 0 to FNumKeys - 1 do
  begin
    // The initial values do not contain any turns, that's why we have to make one.
    if (Rot[I].X = 0) and
      (Rot[I].Y = 0) and
      (Rot[I].Z = 0) then
      Rot[I].X := 1;

    // One quartalion can't describe a big angle (>180), that's why we have to subtract it from 2*pi
    if Rot[I].Angle > pi then
    begin
      Rot[I].Angle := 2 * pi - Rot[I].Angle;
      Rot[I].X := -Rot[I].X;
      Rot[I].Y := -Rot[I].Y;
      Rot[I].Z := -Rot[I].Z;
    end;
    FRot[I] := Rot[I];
  end;
end;

procedure TGLFile3DSRotationAnimationKeys.Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real);
begin
  if FNumKeys > 0 then
    DataTransf.ModelMatrix := MatrixMultiply(DataTransf.ModelMatrix, InterpolateValue(FRot, AFrame));
end;

procedure TGLFile3DSRotationAnimationKeys.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited;

  SetLength(FRot, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FRot[I] := (Source as TGLFile3DSRotationAnimationKeys).FRot[I];
end;

procedure TGLFile3DSRotationAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.write(FRot[0], FNumKeys * SizeOf(TKFRotKey3DS));
end;

procedure TGLFile3DSRotationAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FRot, FNumKeys);
  if FNumKeys > 0 then
    Reader.read(FRot[0], FNumKeys * SizeOf(TKFRotKey3DS));
end;


procedure TGLFile3DSPositionAnimationKeys.LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: Integer;
begin
  inherited;
  SetLength(FPos, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FPos[I] := TAffineVector(PPointList(AData)[I]);
end;

procedure TGLFile3DSPositionAnimationKeys.Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real);
begin
  if FNumKeys > 0 then
    DataTransf.ModelMatrix[3] := VectorAdd(DataTransf.ModelMatrix[3], VectorMake(InterpolateValue(FPos, AFrame)));
end;

procedure TGLFile3DSPositionAnimationKeys.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited;

  SetLength(FPos, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FPos[I] := (Source as TGLFile3DSPositionAnimationKeys).FPos[I];
end;

procedure TGLFile3DSPositionAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.write(FPos[0], FNumKeys * SizeOf(TPoint3DS));
end;

procedure TGLFile3DSPositionAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FPos, FNumKeys);
  if FNumKeys > 0 then
    Reader.read(FPos[0], FNumKeys * SizeOf(TPoint3DS));
end;


procedure TGLFile3DSColorAnimationKeys.LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: Integer;
begin
  inherited;

  SetLength(FCol, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FCol[I] := TaffineVector(PFColorList(AData)[I]);
end;

procedure TGLFile3DSColorAnimationKeys.Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real);
begin
  if FNumKeys > 0 then
    DataTransf.Color := VectorAdd(DataTransf.Color, VectorMake(InterpolateValue(FCol, AFrame)));
end;

procedure TGLFile3DSColorAnimationKeys.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited;

  SetLength(FCol, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FCol[I] := (Source as TGLFile3DSColorAnimationKeys).FCol[I];
end;

procedure TGLFile3DSColorAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.write(FCol[0], FNumKeys * SizeOf(TFColor3DS));
end;

procedure TGLFile3DSColorAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FCol, FNumKeys);
  if FNumKeys > 0 then
    Reader.read(FCol[0], FNumKeys * SizeOf(TFColor3DS));
end;


procedure TTGLFile3DSPositionAnimationKeys.LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: Integer;
begin
  inherited;

  SetLength(FTPos, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FTPos[I] := TaffineVector(PPointList(AData)[I]);
end;

procedure TTGLFile3DSPositionAnimationKeys.Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real);
begin
  if FNumKeys > 0 then
    DataTransf.TargetPos := VectorAdd(DataTransf.TargetPos, InterpolateValue(FTPos, AFrame));
end;

procedure TTGLFile3DSPositionAnimationKeys.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited;

  SetLength(FTPos, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FTPos[I] := (Source as TTGLFile3DSPositionAnimationKeys).FTPos[I];
end;

procedure TTGLFile3DSPositionAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.write(FTPos[0], FNumKeys * SizeOf(TPoint3DS));
end;

procedure TTGLFile3DSPositionAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FTPos, FNumKeys);
  if FNumKeys > 0 then
    Reader.read(FTPos[0], FNumKeys * SizeOf(TPoint3DS));
end;


procedure TGLFile3DSSpotLightCutOffAnimationKeys.LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: Integer;
begin
  inherited;

  SetLength(FFall, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FFall[I] := PSingleList(AData)[I];
end;

procedure TGLFile3DSSpotLightCutOffAnimationKeys.Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real);
begin
  if FNumKeys > 0 then
    DataTransf.SpotLightCutOff := DataTransf.SpotLightCutOff + InterpolateValue(FFall, AFrame);
end;

procedure TGLFile3DSSpotLightCutOffAnimationKeys.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited;

  SetLength(FFall, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FFall[I] := (Source as TGLFile3DSSpotLightCutOffAnimationKeys).FFall[I];
end;

procedure TGLFile3DSSpotLightCutOffAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.write(FFall[0], FNumKeys * SizeOf(Single));
end;

procedure TGLFile3DSSpotLightCutOffAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FFall, FNumKeys);
  if FNumKeys > 0 then
    Reader.read(FFall[0], FNumKeys * SizeOf(Single));
end;


procedure TGLFile3DSLightHotSpotAnimationKeys.LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: Integer;
begin
  inherited;

  SetLength(FHot, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FHot[I] := PSingleList(AData)[I];
end;

procedure TGLFile3DSLightHotSpotAnimationKeys.Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real);
begin
  if FNumKeys > 0 then
    DataTransf.HotSpot := DataTransf.HotSpot + InterpolateValue(FHot, AFrame);
end;

procedure TGLFile3DSLightHotSpotAnimationKeys.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited;

  SetLength(FHot, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FHot[I] := (Source as TGLFile3DSLightHotSpotAnimationKeys).FHot[I];
end;

procedure TGLFile3DSLightHotSpotAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.write(FHot[0], FNumKeys * SizeOf(Single));
end;

procedure TGLFile3DSLightHotSpotAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FHot, FNumKeys);
  if FNumKeys > 0 then
    Reader.read(FHot[0], FNumKeys * SizeOf(Single));
end;

procedure TGLFile3DSRollAnimationKeys.LoadData(const ANumKeys: Integer; const Keys: PKeyHeaderList; const AData: Pointer);
var
  I: Integer;
begin
  inherited;

  SetLength(FRoll, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FRoll[I] := PSingleList(AData)[I];
end;

procedure TGLFile3DSRollAnimationKeys.Apply(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real);
begin
  if FNumKeys > 0 then
    DataTransf.Roll := DataTransf.Roll + InterpolateValue(FRoll, AFrame);
end;

procedure TGLFile3DSRollAnimationKeys.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited;

  SetLength(FRoll, FNumKeys);
  for I := 0 to FNumKeys - 1 do
    FRoll[I] := (Source as TGLFile3DSRollAnimationKeys).FRoll[I];
end;

procedure TGLFile3DSRollAnimationKeys.WriteToFiler(Writer: TVirtualWriter);
begin
  inherited;

  if FNumKeys > 0 then
    Writer.write(FRoll[0], FNumKeys * SizeOf(Single));
end;

procedure TGLFile3DSRollAnimationKeys.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  SetLength(FRoll, FNumKeys);
  if FNumKeys > 0 then
    Reader.read(FRoll[0], FNumKeys * SizeOf(Single));
end;

procedure TGLFile3DSAnimationKeyList.AddKeys(const AItem: TGLFile3DSAnimationKeys);
var
  ind: Integer;
begin
  if AItem = nil then
    Exit;
  ind := Length(FAnimKeysList);
  SetLength(FAnimKeysList, ind + 1);
  FAnimKeysList[ind] := AItem;
end;

procedure TGLFile3DSAnimationKeyList.ApplyAnimKeys(var DataTransf: TGLFile3DSAnimationData; const AFrame: Real);
var
  I: Integer;
begin
  for I := 0 to Length(FAnimKeysList) - 1 do
    FAnimKeysList[I].Apply(DataTransf, AFrame);
end;

procedure TGLFile3DSAnimationKeyList.ClearKeys;
var
  I: Integer;
begin
  for I := 0 to Length(FAnimKeysList) - 1 do
    FAnimKeysList[I].Free;
  SetLength(FAnimKeysList, 0);
end;

procedure TGLFile3DSAnimationKeyList.Assign(Source: TPersistent);
var
  I:    Integer;
  item: TGLFile3DSAnimationKeys;
begin
  if Source is TGLFile3DSAnimationKeyList then
  begin
    ClearKeys;
    for I := 0 to Length(TGLFile3DSAnimationKeyList(Source).FAnimKeysList) - 1 do
    begin
      item := (TGLFile3DSAnimationKeyList(Source).FAnimKeysList[I].ClassType.Create as TGLFile3DSAnimationKeys);
      item.Assign(TGLFile3DSAnimationKeyList(Source).FAnimKeysList[I]);
      AddKeys(item);
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TGLFile3DSAnimationKeyList.WriteToFiler(Writer: TVirtualWriter);
var
  I:   Integer;
  Val: TGLFile3DSAnimKeysClassType;
begin
  Writer.WriteInteger(Length(FAnimKeysList));
  for I := 0 to Length(FAnimKeysList) - 1 do
  begin
    Val := ClassToAnimKeysClassType(FAnimKeysList[I].ClassType);
    Writer.write(Val, SizeOf(Val));
    FAnimKeysList[I].WriteToFiler(Writer);
  end;
end;

procedure TGLFile3DSAnimationKeyList.ReadFromFiler(Reader: TVirtualReader);
var
  I, cnt: Integer;
  Val:    TGLFile3DSAnimKeysClassType;
begin
  ClearKeys;
  cnt := Reader.ReadInteger;
  SetLength(FAnimKeysList, cnt);
  for I := 0 to Length(FAnimKeysList) - 1 do
  begin
    Reader.read(Val, SizeOf(Val));
    FAnimKeysList[I] := AnimKeysClassTypeToClass(Val).Create as TGLFile3DSAnimationKeys;
    FAnimKeysList[I].ReadFromFiler(Reader);
  end;
end;

destructor TGLFile3DSAnimationKeyList.Destroy;
begin
  ClearKeys;
  inherited Destroy;
end;

constructor TGLFile3DSDummyObject.Create;
begin
  inherited;

  FAnimList := TGLFile3DSAnimationKeyList.Create;
  FRefTranf.ModelMatrix := IdentityHmgMatrix;
  FAnimTransf.ModelMatrix := IdentityHmgMatrix;
end;

procedure TGLFile3DSDummyObject.LoadAnimation(const AData: Pointer);
begin
  FAnimList.ClearKeys;
  FAnimData := AData;
end;

procedure TGLFile3DSDummyObject.SetFrame(const AFrame: Real);
var
  p:      TGLFile3DSDummyObject;
  lAnimationData: TGLFile3DSAnimationData;
begin
  if not vGLFile3DS_EnableAnimation then Exit;

  if (FParentName <> '') then
  begin
    FParent := Owner.FindMeshByName(FParentName) as TGLFile3DSDummyObject;
    FParentName := '';
  end;
  lAnimationData := FRefTranf;
  p := self;
  while p <> nil do
  begin
    p.FAnimList.ApplyAnimKeys(lAnimationData, AFrame);
    p := p.FParent;
  end;
  FAnimTransf := lAnimationData;
end;

procedure TGLFile3DSDummyObject.MorphTo(morphTargetIndex: Integer);
begin
  SetFrame(morphTargetIndex + 1);
end;

procedure TGLFile3DSDummyObject.Lerp(morphTargetIndex1, morphTargetIndex2: Integer; lerpFactor: Single);
begin
  if (Owner.Owner is TGLActor) and ((Owner.Owner as TGLActor).AnimationMode in [aamBounceBackward, aamLoopBackward]) then
    SetFrame(morphTargetIndex1 + 1 - lerpFactor)
  else
    SetFrame(morphTargetIndex1 + 1 + lerpFactor);
end;

procedure TGLFile3DSDummyObject.Assign(Source: TPersistent);
begin
  inherited; // Assign all published properties here.
  if Source is TGLFile3DSDummyObject then
  begin
    FRefTranf := (Source as TGLFile3DSDummyObject).FRefTranf;
    FParent := (Source as TGLFile3DSDummyObject).FParent;
    FAnimList.Assign((Source as TGLFile3DSDummyObject).FAnimList);
    SetFrame(1);
  end;
end;

procedure TGLFile3DSDummyObject.GetExtents(var min, max: TAffineVector);
begin
  Vertices.GetExtents(min, max);
  if not IsInfinite(min[0]) then
    min := VectorTransform(min, FAnimTransf.ModelMatrix);
  if not IsInfinite(max[0]) then
    max := VectorTransform(max, FAnimTransf.ModelMatrix);
end;

function TGLFile3DSDummyObject.ExtractTriangles(texCoords,
  normals: TAffineVectorList): TAffineVectorList;
var
  I: Integer;
begin
  Result := inherited ExtractTriangles(texCoords, normals);

  if (Result.Count <> 0) and not MatrixEquals(FAnimTransf.ModelMatrix, IdentityHmgMatrix) then
    for I := 0 to Result.Count - 1 do
      Result[I] := VectorTransform(Result[I], FAnimTransf.ModelMatrix);
end;

procedure TGLFile3DSDummyObject.WriteToFiler(Writer: TVirtualWriter);
var
  str: string[32];
begin
  inherited;

  Writer.write(FRefTranf, SizeOf(FRefTranf));
  if FParent <> nil then
    str := FParent.Name
  else
    str := 'nil';
  Writer.WriteString(str);
  FAnimList.WriteToFiler(Writer);
end;

procedure TGLFile3DSDummyObject.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  Reader.read(FRefTranf, SizeOf(FRefTranf));
  FParentName := Reader.ReadString;
  if FParentName = 'nil' then
    FParentName := '';
  FAnimList.ReadFromFiler(Reader);
end;

destructor TGLFile3DSDummyObject.Destroy;
begin
  FAnimList.Free;

  inherited;
end;


procedure TGLFile3DSMeshObject.LoadAnimation(const AData: Pointer);
var
  aScale: TGLFile3DSScaleAnimationKeys;
  aRot:   TGLFile3DSRotationAnimationKeys;
  aPos:   TGLFile3DSPositionAnimationKeys;
begin
  inherited;

  with PKFMesh3DS(AData)^, FAnimList do
  begin
    aScale := TGLFile3DSScaleAnimationKeys.Create;
    aScale.LoadData(NSKeys, SKeys, Scale);
    AddKeys(aScale);

    aRot := TGLFile3DSRotationAnimationKeys.Create;
    aRot.LoadData(NRKeys, RKeys, Rot);
    AddKeys(aRot);

    aPos := TGLFile3DSPositionAnimationKeys.Create;
    aPos.LoadData(NPKeys, PKeys, Pos);
    AddKeys(aPos);

    if ParentStr <> '' then
      FParent := TGLFile3DSDummyObject(Owner.FindMeshByName(ParentStr));

    with FRefTranf do
    begin
      ModelMatrix[3, 0] := ModelMatrix[3, 0] - Pivot.X;
      ModelMatrix[3, 1] := ModelMatrix[3, 1] - Pivot.Y;
      ModelMatrix[3, 2] := ModelMatrix[3, 2] - Pivot.Z;
    end;
  end;

  SetFrame(1);
end;

procedure TGLFile3DSMeshObject.BuildList(var ARci: TRenderContextInfo);
begin
  glPushMatrix;
  glMultMatrixf(@FAnimTransf.ModelMatrix);
  inherited;
  glPopMatrix;
end;

constructor TGLFile3DSOmniLightObject.Create;
begin
  inherited;
  FLightSrc := TGLFile3DSLight.Create(nil);
end;

procedure TGLFile3DSOmniLightObject.LoadData(const AOwner: TGLBaseMesh; const AData: PLight3DS);
begin
  FLightSrc.Parent := AOwner;
  FLightSrc.LightStyle := lsOmni;
  FLightSrc.Name := AData.NameStr;
  Name := AData.NameStr;
  FLightSrc.Position.SetPoint(PAffineVector(@AData.Pos)^);
  FLightSrc.Diffuse.Color := VectorMake(AData.Color.R, AData.Color.G, AData.Color.B);
  FLightSrc.Specular.Color := VectorMake(AData.Color.R, AData.Color.G, AData.Color.B);
  FLightSrc.Diffuse.Color := VectorScale(FLightSrc.Diffuse.Color, AData.Multiplier);  //���� ���������
  FLightSrc.Shining := not AData.DLOff;
  FLightSrc.Multipler := AData.Multiplier;
  FLightSrc.ConstAttenuation := 1;
  FLightSrc.LinearAttenuation := 0;
  FLightSrc.QuadraticAttenuation := 0;
end;

procedure TGLFile3DSOmniLightObject.LoadAnimation(const AData: Pointer);
var
  aPos: TGLFile3DSPositionAnimationKeys;
  aCol: TGLFile3DSColorAnimationKeys;
begin
  inherited;

  with PKFOmni3DS(AData)^, FAnimList do
  begin
    aPos := TGLFile3DSPositionAnimationKeys.Create;
    aPos.LoadData(NPKeys, PKeys, Pos);
    AddKeys(aPos);

    aCol := TGLFile3DSColorAnimationKeys.Create;
    aCol.LoadData(NCKeys, CKeys, Color);
    AddKeys(aCol);

    if Parent <> '' then
      FParent := TGLFile3DSDummyObject(Owner.FindMeshByName(Parent));
  end;

  SetFrame(1);
end;

procedure TGLFile3DSOmniLightObject.SetFrame(const AFrame: Real);
var
  obj: TComponent;
begin
  if FLightSrcName <> '' then
  begin
    obj := Owner.Owner.FindChild(FLightSrcName, True);
    if obj is TGLFile3DSLight then
    begin
      FLightSrc.Free;
      FLightSrc := obj as TGLFile3DSLight;
    end;
    FLightSrcName := '';
  end;

  inherited;
  FLightSrc.Position.SetPoint(FAnimTransf.ModelMatrix[3]);
  FLightSrc.Diffuse.Color := FAnimTransf.Color;
end;

procedure TGLFile3DSOmniLightObject.Assign(Source: TPersistent);
begin
  inherited;

  if Source is TGLFile3DSOmniLightObject then
    FlightSrc.Assign((Source as TGLFile3DSOmniLightObject).FLightSrc);
end;

procedure TGLFile3DSOmniLightObject.WriteToFiler(Writer: TVirtualWriter);
var
  str: string[64];
begin
  inherited;

  if FLightSrc.Name = '' then
    str := 'nil'
  else
    str := FLightSrc.Name;
  Writer.WriteString(str);
end;

procedure TGLFile3DSOmniLightObject.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  FLightSrcName := Reader.ReadString;
  if FLightSrcName = 'nil' then
    FLightSrcName := '';
end;

destructor TGLFile3DSOmniLightObject.Destroy;
begin
  FLightSrc.Free;

  inherited;
end;


procedure TGLFile3DSSpotLightObject.LoadData(const AOwner: TGLBaseMesh; const AData: PLight3DS);
begin
  inherited;

  FLightSrc.LightStyle := lsSpot;
  FLightSrc.SpotTargetPos.SetPoint(TAffineVector(AData.Spot.Target));
  FLightSrc.SpotCutOff := AData.Spot.FallOff / 2;
  FLightSrc.HotSpot := AData.Spot.Hotspot / 2;
end;

procedure TGLFile3DSSpotLightObject.LoadAnimation(const AData: Pointer);
var
  aTPos: TTGLFile3DSPositionAnimationKeys;
  aFall: TGLFile3DSSpotLightCutOffAnimationKeys;
  aHot:  TGLFile3DSLightHotSpotAnimationKeys;
begin
  inherited;

  with PKFSpot3DS(AData)^, FAnimList do
  begin
    aTPos := TTGLFile3DSPositionAnimationKeys.Create;
    aTPos.LoadData(NTKeys, TKeys, TPos);
    AddKeys(aTPos);

    aFall := TGLFile3DSSpotLightCutOffAnimationKeys.Create;
    aFall.LoadData(NFKeys, FKeys, Fall);
    AddKeys(aFall);

    aHot := TGLFile3DSLightHotSpotAnimationKeys.Create;
    aHot.LoadData(NHKeys, HKeys, Hot);
    AddKeys(aHot);

    if Parent <> '' then
      FParent := TGLFile3DSDummyObject(Owner.FindMeshByName(Parent));
  end;

  SetFrame(1);
end;

procedure TGLFile3DSSpotLightObject.SetFrame(const AFrame: Real);
begin
  inherited;

  FLightSrc.SpotTargetPos.SetPoint(FAnimTransf.TargetPos);
  FLightSrc.SpotCutOff := FAnimTransf.SpotLightCutOff / 2;
  FLightSrc.HotSpot := FAnimTransf.HotSpot / 2;
end;


constructor TGLFile3DSCameraObject.Create;
begin
  inherited;

  FCameraSrc := TGLFile3DSCamera.Create(nil);
  FTargetObj := TGLDummyCube.Create(nil);
  FCameraSrc.TargetObject := FTargetObj;
end;

procedure TGLFile3DSCameraObject.LoadData(Owner: TGLBaseMesh; AData: PCamera3DS);
begin
  FCameraSrc.Parent := Owner;
  FTargetObj.Parent := Owner;
  FCameraSrc.Name := AData.NameStr;
  Name := AData.NameStr;
  FCameraSrc.Position.AsAffineVector := TAffineVector(AData.Position);
  FTargetObj.Position.SetPoint(TAffineVector(AData.Target));
  FCameraSrc.RollAngle := AData.Roll;

  FCameraSrc.FocalLength := AData.FOV;
end;

procedure TGLFile3DSCameraObject.LoadAnimation(const AData: Pointer);
var
  aPos:  TGLFile3DSPositionAnimationKeys;
  aRoll: TGLFile3DSRollAnimationKeys;
  aTPos: TTGLFile3DSPositionAnimationKeys;
begin
  inherited;

  with PKFCamera3DS(AData)^, FAnimList do
  begin
    aPos := TGLFile3DSPositionAnimationKeys.Create;
    aPos.LoadData(NPKeys, PKeys, Pos);
    AddKeys(aPos);

    aRoll := TGLFile3DSRollAnimationKeys.Create;
    aRoll.LoadData(NRKeys, RKeys, Roll);
    AddKeys(aRoll);

    aTPos := TTGLFile3DSPositionAnimationKeys.Create;
    aTPos.LoadData(NTKeys, TKeys, TPos);
    AddKeys(aTPos);
  end;

  SetFrame(1);
end;

procedure TGLFile3DSCameraObject.SetFrame(const AFrame: Real);
var
  obj: TComponent;
begin
  inherited;

  if FCameraSrcName <> '' then
  begin
    obj := Owner.Owner.FindChild(FCameraSrcName, True);
    if obj is TGLFile3DSCamera then
    begin
      FCameraSrc.Free;
      FCameraSrc := obj as TGLFile3DSCamera;
    end;
    FCameraSrcName := '';
  end;

  FCameraSrc.Position.SetPoint(FAnimTransf.ModelMatrix[3]);
  FCameraSrc.RollAngle := FAnimTransf.Roll;
  FTargetObj.Position.SetPoint(FAnimTransf.TargetPos);
end;

procedure TGLFile3DSCameraObject.WriteToFiler(Writer: TVirtualWriter);
var
  str: string[64];
begin
  inherited;

  if FCameraSrc.Name = '' then
    str := 'nil'
  else
    str := FCameraSrc.Name;
  Writer.WriteString(str);
end;

procedure TGLFile3DSCameraObject.ReadFromFiler(Reader: TVirtualReader);
begin
  inherited;

  FCameraSrcName := Reader.ReadString;
  if FCameraSrcName = 'nil' then
    FCameraSrcName := '';
end;

destructor TGLFile3DSCameraObject.Destroy;
begin
  FCameraSrc.Free;
  FTargetObj.Free;
  inherited;
end;



// ------------------
// ------------------ TGL3DSVectorFile ------------------
// ------------------

// Capabilities
//
class function TGL3DSVectorFile.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

// LoadFromStream
//
procedure TGL3DSVectorFile.LoadFromStream(aStream: TStream);
type
   TSmoothIndexEntry = array[0..31] of Cardinal;
   PSmoothIndexArray = ^TSmoothIndexArray;
   TSmoothIndexArray = array[0..MaxInt shr 8] of TSmoothIndexEntry;
var
   Marker: PByteArray;
   CurrentVertexCount: Integer;
   SmoothIndices: PSmoothIndexArray;
   mesh: TGLFile3DSMeshObject;
   hasLightmap : Boolean;

   //--------------- local functions -------------------------------------------

  function GetOrAllocateMaterial(materials: TMaterialList; const Name: string): string;
  var
    material:  PMaterial3DS;
    specColor: TVector;
    matLib:    TGLMaterialLibrary;
    libMat, SecondMaterial:    TGLLibMaterial;
  begin
    material := Materials.MaterialByName[Name];
    Assert(Assigned(material));
    if GetOwner is TGLBaseMesh then
    begin
      matLib := TGLBaseMesh(GetOwner).MaterialLibrary;
      if Assigned(matLib) then
      begin
        Result := Name;
        libMat := matLib.Materials.GetLibMaterialByName(Name);
        if not Assigned(libMat) then
        begin
          libMat := matLib.Materials.Add;
          libMat.Name := Name;
          libMat.Material.Texture.Disabled := False;
          with libMat.Material.FrontProperties do
          begin
            Ambient.Color := VectorMake(material.Ambient.R, material.Ambient.G, material.Ambient.B, 1);
            // Material transparency can be stored as a positive or negative value.
            Diffuse.Color := VectorMake(material.Diffuse.R, material.Diffuse.G, material.Diffuse.B, 1 - Abs(material.Transparency));
            specColor := VectorMake(material.Specular.R, material.Specular.G, material.Specular.B, 1);
            Specular.Color := VectorScale(specColor, material.ShinStrength);
            Shininess := MaxInteger(0, Integer(round((material.Shininess) * 128)));
            if material.Transparency <> 0 then
              libMat.Material.BlendingMode := bmTransparency;
          end;
          if Trim(material.Texture.Map.NameStr) <> '' then
            try
              if vGLFile3DS_UseTextureEx then
                with libMat.Material.TextureEx.Add do
                begin
                  Texture.Image.LoadFromFile(material.Texture.Map.NameStr);
                  Texture.Disabled := False;
                  Texture.TextureMode := tmModulate;
                  TextureIndex := 0;
                  with material.Texture.Map do
                  begin
                    TextureScale.SetPoint(UScale, VScale, 0);
                    TextureOffset.SetPoint((1 - frac(UOffset)) * UScale, (frac(VOffset)) * VScale, 0);
                  end;
                end
              else
                with libMat.Material.Texture do
                begin
                  Image.LoadFromFile(material.Texture.Map.NameStr);
                  Disabled := False;
                  TextureMode := tmModulate;
                end

            except
              on E: ETexture do
              begin
                if not Owner.IgnoreMissingTextures then
                  raise E.Create(glsError + 'loading main texture' + #13#13 +
                    e.Message + ' in' + #10#13 + matLib.TexturePaths);
              end
              else
                raise;
            end;

          if Trim(material.Opacity.Map.NameStr) <> '' then
            try
              if vGLFile3DS_UseTextureEx then
                with libMat.Material.TextureEx.Add do
                begin
                  libMat.Material.BlendingMode := bmTransparency;
                  Texture.ImageAlpha := tiaAlphaFromIntensity;
                  Texture.TextureMode := tmModulate;
                  Texture.Image.LoadFromFile(material.Opacity.Map.NameStr);
                  Texture.Disabled := False;
                  TextureIndex := 1;
                  with material.Opacity.Map do
                  begin
                    TextureScale.SetPoint(UScale, VScale, 0);
                    TextureOffset.SetPoint((1 - frac(UOffset)) * UScale, (frac(VOffset)) * VScale, 0);
                  end;
                end
              else
                with libMat.Material.Texture do
                begin
                  SecondMaterial := matLib.Materials.Add;
                  SecondMaterial.Material.Texture.Image.LoadFromFile(material.Opacity.Map.NameStr);
                  SecondMaterial.Material.Texture.Disabled := false;
                  SecondMaterial.Material.Texture.ImageAlpha := tiaAlphaFromIntensity;
                  SecondMaterial.Material.Texture.TextureMode := tmModulate;
                  SecondMaterial.Name := material.Opacity.Map.NameStr;
                  LibMat.Texture2Name := SecondMaterial.Name;
                  Disabled := False;
                end;

            except
              on E: ETexture do
              begin
                if not Owner.IgnoreMissingTextures then
                  raise E.Create(glsError + 'loading opacity texture' + #13#13 +
                    e.Message + ' in' + #10#13 + matLib.TexturePaths);
              end
              else
                raise;
            end;
          if Trim(material.Bump.Map.NameStr) <> '' then
            try
              if vGLFile3DS_UseTextureEx then
                with libMat.Material.TextureEx.Add do
                begin
                  Texture.Image.LoadFromFile(material.Bump.Map.NameStr);
                  Texture.Disabled := False;
                  Texture.TextureMode := tmModulate;
                  // You need a hight map for this parameter (like in 3d Max).
                  // Texture.TextureFormat := tfNormalMap;
                  TextureIndex := 2;
                  with material.Bump.Map do
                  begin
                    TextureScale.SetPoint(UScale, VScale, 0);
                    TextureOffset.SetPoint((1 - frac(UOffset)) * UScale, (frac(VOffset)) * VScale, 0);
                  end;
                end
              else
                with libMat.Material.Texture do
                begin
                  SecondMaterial := matLib.Materials.Add;
                  SecondMaterial.Material.Texture.Image.LoadFromFile(material.Bump.Map.NameStr);
                  SecondMaterial.Material.Texture.Disabled := False;
                  SecondMaterial.Material.Texture.ImageAlpha := tiaAlphaFromIntensity;
                  SecondMaterial.Material.Texture.TextureMode := tmModulate;
                  SecondMaterial.Name := material.Opacity.Map.NameStr;
                  Disabled := False;
                end;

            except
              on E: ETexture do
              begin
                if not Owner.IgnoreMissingTextures then
                  raise E.Create(glsError + 'loading bump map texture' + #13#13 +
                    e.Message + ' in' + #10#13 + matLib.TexturePaths);
              end
              else
                raise;
            end;
        end;
      end
      else
        Result := '';
    end
    else
      Result := '';
  end;

   function GetOrAllocateLightMap(materials : TMaterialList; const name : String) : Integer;
   var
      material : PMaterial3DS;
      matLib : TGLMaterialLibrary;
      libMat : TGLLibMaterial;
   begin
      Result:=-1;
      material:=Materials.MaterialByName[Name];
      Assert(Assigned(material));
      if GetOwner is TGLBaseMesh then begin
         matLib:=TGLBaseMesh(GetOwner).LightmapLibrary;
         if Assigned(matLib) then begin
            if Trim(material.IllumMap.Map.NameStr)<>'' then begin
               libMat:=matLib.Materials.GetLibMaterialByName(material.IllumMap.Map.NameStr);
               if not Assigned(libMat) then begin
                  libMat:=matLib.Materials.Add;
                  libMat.Name:=material.IllumMap.Map.NameStr;
                  try
                     with libMat.Material.Texture do begin
                        Image.LoadFromFile(material.IllumMap.Map.NameStr);
                        Disabled:=False;
                        TextureMode:=tmModulate;
                     end;
                  except
                     on E: ETexture do begin
                        if not Owner.IgnoreMissingTextures then
                           raise E.Create('Error loading light map texture' + #13#13 +
                              e.Message + ' in' + #10#13 + matLib.TexturePaths);
                     end
                     else
                       raise;
                  end;
               end;
               Result:=libmat.Index;
               hasLightMap:=True;
            end;
         end;
      end;
   end;

   //----------------------------------------------------------------------

  function InvertMeshMatrix(Objects: TObjectList; const Name: string): TMatrix;
    // constructs a 4x4 matrix from 3x4 local mesh matrix given by Name and
    // inverts it so it can be used for the keyframer stuff
  var
    I, Index: Integer;
    boolY:  Boolean;
    m:      TMatrix;
    v4:     TVector;
    factor: Single;
  begin
    with Objects do
    begin
      Index := -1;
      for I := 0 to MeshCount - 1 do
        if CompareText(Mesh[I].NameStr, Name) = 0 then
        begin
          Index := I;
          Break;
        end;

      if Index > -1 then
      begin
        with Mesh[Index]^ do
        begin
          Result[0, 0] := LocMatrix[0];
          Result[0, 1] := LocMatrix[1];
          Result[0, 2] := LocMatrix[2];
          Result[0, 3] := 0;
          Result[1, 0] := LocMatrix[3];
          Result[1, 1] := LocMatrix[4];
          Result[1, 2] := LocMatrix[5];
          Result[1, 3] := 0;
          Result[2, 0] := LocMatrix[6];
          Result[2, 1] := LocMatrix[7];
          Result[2, 2] := LocMatrix[8];
          Result[2, 3] := 0;
          Result[3, 0] := LocMatrix[9];
          Result[3, 1] := LocMatrix[10];
          Result[3, 2] := LocMatrix[11];
          Result[3, 3] := 1;
        end;
        InvertMatrix(Result);

        //���� ������� �� �������������, �.�. ������ ������� �� ����� ���������� ������������ ������ ���� ��������,
        //�� ������ �� ����� ��������� �� -pi ������ ��� Y.
        m := Result;
        v4 := m[3];
        factor := VectorLength(m[0]);
        NormalizeMatrix(m);
        ScaleMatrix(m, factor);
        m[3] := v4;

        v4 := VectorAbs(VectorSubtract(Result[2], m[2]));
        boolY := (v4[0] > abs(Result[2, 0])) and
          (v4[1] > abs(Result[2, 1])) and
          (v4[2] > abs(Result[2, 2]));

        if boolY then
          Result := MatrixMultiply(Result, CreateRotationMatrix(AffineVectorMake(0, 1, 0), -pi));

      end
      else
        Result := IdentityHmgMatrix;
    end;
  end;

   //----------------------------------------------------------------------

{$IFDEF NO_ASM}
   function IsVertexMarked(P: PByteArray; Index: word): Boolean; inline;
      // tests the Index-th bit, returns True if set else False
   var mi : word;
   begin
     DivMod(index,8,mi,index);
     result:=(((p^[mi] shr Index) and 1) = 1);
   end;
{$ELSE}
   function IsVertexMarked(P: Pointer; Index: Integer): Boolean; assembler;
      // tests the Index-th bit, returns True if set else False
   asm
                     BT [EAX], EDX
                     SETC AL
   end;
{$ENDIF}

   //---------------------------------------------------------------------------

{$IFDEF NO_ASM}
   function MarkVertex(P: PByteArray; Index: word): Boolean;  inline;
      // sets the Index-th bit and return True if it was already set else False
   var mi : word;
   begin
     DivMod(index,8,mi,index);
     result:=(((p^[mi] shr Index) and 1) = 1);
     if not(result) then p^[mi]:=p^[mi] or (1 shl index);
   end;
{$ELSE}
   function MarkVertex(P: Pointer; Index: Integer): Boolean; assembler;
      // sets the Index-th bit and return True if it was already set else False
   asm
                     BTS [EAX], EDX
                     SETC AL
   end;
{$ENDIF}

   //---------------------------------------------------------------------------
   
      // Stores new vertex index (NewIndex) into the smooth index array of vertex ThisIndex
      // using field SmoothingGroup, which must not be 0.
      // For each vertex in the vertex array (also for duplicated vertices) an array of 32 cardinals
      // is maintained (each for one possible smoothing group. If a vertex must be duplicated because
      // it has no smoothing group or a different one then the index of the newly created vertex is
      // stored in the SmoothIndices to avoid loosing the conjunction between not yet processed vertices
      // and duplicated vertices.
      // Note: Only one smoothing must be assigned per vertex. Some available models break this rule and
      //       have more than one group assigned to a face. To make the code fail safe the group ID
      //       is scanned for the lowest bit set.

{$IFDEF NO_ASM}
   procedure StoreSmoothIndex(ThisIndex, SmoothingGroup, NewIndex: Cardinal; P: PSmoothIndexArray);
   var i : word;
   begin
     i:=0;
     while SmoothingGroup and (1 shl i) = 0 do inc(i);
     p^[ThisIndex,i]:=NewIndex;
   end;
{$ELSE}
   procedure StoreSmoothIndex(ThisIndex, SmoothingGroup, NewIndex: Cardinal; P: Pointer);
   asm
                   PUSH EBX
                   BSF EBX, EDX                  // determine smoothing group index (convert flag into an index)
                   MOV EDX, [P]                  // get address of index array
                   SHL EAX, 7                    // ThisIndex * SizeOf(TSmoothIndexEntry)
                   ADD EAX, EDX
                   LEA EDX, [4 * EBX + EAX]      // Address of array + vertex index + smoothing group index
                   MOV [EDX], ECX
                   POP EBX
   end;
{$ENDIF}

   //---------------------------------------------------------------------------

{$IFDEF NO_ASM}
   function GetSmoothIndex(ThisIndex, SmoothingGroup: Cardinal; P: PSmoothIndexArray): Integer; inline;
      // Retrieves the vertex index for the given index and smoothing group.
      // This redirection is necessary because a vertex might have been duplicated.
   var i : word;
   begin
     i:=0;
     while SmoothingGroup and (1 shl i) = 0 do inc(i);
     result:=integer(p^[ThisIndex,i]);
   end;
{$ELSE}
   function GetSmoothIndex(ThisIndex, SmoothingGroup: Cardinal; P: Pointer): Integer;
      // Retrieves the vertex index for the given index and smoothing group.
      // This redirection is necessary because a vertex might have been duplicated.
   asm
                   PUSH EBX
                   BSF EBX, EDX                  // determine smoothing group index
                   SHL EAX, 7                    // ThisIndex * SizeOf(TSmoothIndexEntry)
                   ADD EAX, ECX
                   LEA ECX, [4 * EBX + EAX]      // Address of array + vertex index + smoothing group index
                   MOV EAX, [ECX]
                   POP EBX
   end;
{$ENDIF}

   //---------------------------------------------------------------------------

   procedure DuplicateVertex(Index: Integer);
      // extends the vector and normal array by one entry and duplicates the vertex AData given by Index
      // the marker and texture arrays will be extended too, if necessary
   begin
      // enhance vertex array
      with mesh.Vertices do Add(Items[index]);
      mesh.Normals.Add(NullVector);
      // enhance smooth index array
      ReallocMem(SmoothIndices, (CurrentVertexCount + 1) * SizeOf(TSmoothIndexEntry));
      FillChar(SmoothIndices[CurrentVertexCount], SizeOf(TSmoothIndexEntry), $FF);
      // enhance marker array
      if (CurrentVertexCount div 8) <> ((CurrentVertexCount + 1) div 8) then begin
         ReallocMem(Marker, ((CurrentVertexCount + 1) div 8) + 1);
         Marker[(CurrentVertexCount div 8) + 1]:=0;
      end;
      with mesh.TexCoords do if Count>0 then Add(Items[index]);
      Inc(CurrentVertexCount);
   end;

   //---------------------------------------------------------------------------

  function FindMotionIndex(KeyFramer: TKeyFramer; const ObjectName: string): Integer;
    // Looks through the motion list for the object "ObjectName" and returns its index
    // or -1 if the name is not it the list
  var
    I: Integer;
  begin
    Result := -1;
    with KeyFramer do
      for I := 0 to MeshMotionCount - 1 do
        if CompareText(MeshMotion[I].NameStr, ObjectName) = 0 then
        begin
          Result := I;
          Break;
        end;
  end;

   //--------------- end local functions ---------------------------------------

var
  CurrentMotionIndex, iMaterial, i, j, x : Integer;
  aFaceGroup : TFGVertexIndexList;
  Face, Vertex, TargetVertex: Integer;
  SmoothingGroup: Cardinal;
  CurrentIndex: Word;
  Vector1, Vector2, Normal : TAffineVector;
  standardNormalsOrientation : Boolean;
  lights_mesh: TGLFile3DSOmniLightObject;
  camera_mesh: TGLFile3DSCameraObject; 
begin
   with TFile3DS.Create do try
      LoadFromStream(aStream);
      // determine front face winding
      { TODO : better face winding }
      standardNormalsOrientation:=not (NormalsOrientation=mnoDefault);


      for i:=0 to Objects.MeshCount-1 do with PMesh3DS(Objects.Mesh[I])^ do
      begin
         hasLightMap:=False;
         mesh:=TGLFile3DSMeshObject.CreateOwned(Owner.MeshObjects);
         mesh.Name:=PMesh3DS(Objects.Mesh[I])^.NameStr;
        //dummy targets
        for x:=0 to KeyFramer.Settings.Anim.Length-1 do
          TMeshMorphTarget.CreateOwned(mesh.MorphTargets);

         with mesh do begin
            Mode:=momFaceGroups;
            // make a copy of the vertex data, this must always be available
            Vertices.Capacity:=NVertices;
            Normals.AddNulls(NVertices);
            if NTextVerts>0 then begin
               TexCoords.Capacity:=NVertices;
               for j:=0 to NVertices-1 do begin
                  Vertices.Add(PAffineVector(@VertexArray[j])^);
                  TexCoords.Add(PTexPoint(@TextArray[j])^);
               end;
            end else begin
               for j:=0 to NVertices-1 do
                  Vertices.Add(PAffineVector(@VertexArray[j])^);
            end;
         end;

         // allocate memory for the smoothindices and the marker array
         CurrentVertexCount:=NVertices;
         Marker:=AllocMem((NVertices div 8) + 1); // one bit for each vertex
         //writeln('******MARKER ALOCATED MEM FOR ',NVertices,' VERTICES');
         SmoothIndices:=AllocMem(NVertices * SizeOf(TSmoothIndexEntry));

         if SmoothArray=nil then begin
            // no smoothing groups to consider
            for face:=0 to NFaces-1 do with FaceArray^[Face] do
            begin
               // normal vector for the face
               with mesh.Vertices do begin
                  VectorSubtract(Items[V1], Items[V2], vector1);
                  VectorSubtract(Items[V3], Items[V2], vector2);
               end;
               if standardNormalsOrientation then
                  Normal:=VectorCrossProduct(Vector1, Vector2)
               else Normal:=VectorCrossProduct(Vector2, Vector1);
               // go for each vertex in the current face
               for Vertex:=0 to 2 do begin
                  // copy current index for faster access
                  CurrentIndex:=FaceRec[Vertex];
                  // already been touched?
                  if IsVertexMarked(Marker, CurrentIndex) then begin
                     // already touched vertex must be duplicated
                     DuplicateVertex(CurrentIndex);
                     FaceRec[Vertex]:=CurrentVertexCount-1;
                     mesh.Normals[CurrentVertexCount-1]:=Normal;
                  end else begin
                     // not yet touched, so just store the normal
                     mesh.Normals[CurrentIndex]:=Normal;
                     MarkVertex(Marker, CurrentIndex);
                  end;
               end;
            end;
         end else begin
            // smoothing groups are to be considered
            for Face:=0 to NFaces-1 do with FaceArray^[Face] do begin
               // normal vector for the face
               with mesh.Vertices do begin
                  VectorSubtract(Items[V1], Items[V2], vector1);
                  VectorSubtract(Items[V3], Items[V2], vector2);
               end;
               if standardNormalsOrientation then
                  Normal:=VectorCrossProduct(Vector1, Vector2)
               else Normal:=VectorCrossProduct(Vector2, Vector1);
               SmoothingGroup:=SmoothArray^[Face];
               // go for each vertex in the current face
               for Vertex:=0 to 2 do begin
                  // copy current index for faster access
                  currentIndex:=FaceRec[Vertex];
                  // Has vertex already been touched?
                  if IsVertexMarked(Marker, currentIndex) then begin
                     // check smoothing group
                     if SmoothingGroup = 0 then begin
                        // no smoothing then just duplicate vertex
                        DuplicateVertex(CurrentIndex);
                        FaceRec[Vertex]:=CurrentVertexCount - 1;
                        mesh.Normals[CurrentVertexCount - 1]:=Normal;
                        // mark new vertex also as touched
                        MarkVertex(Marker, CurrentVertexCount - 1);
                     end else begin
                        // this vertex must be smoothed, check if there's already
                        // a (duplicated) vertex for this smoothing group
                        TargetVertex:=GetSmoothIndex(CurrentIndex, SmoothingGroup, SmoothIndices);
                        if TargetVertex < 0 then begin
                           // vertex has not yet been duplicated for this smoothing
                           // group, so do it now
                           DuplicateVertex(CurrentIndex);
                           FaceRec[Vertex]:=CurrentVertexCount - 1;
                           mesh.Normals[CurrentVertexCount - 1]:=Normal;
                           StoreSmoothIndex(CurrentIndex, SmoothingGroup, CurrentVertexCount - 1, SmoothIndices);
                           StoreSmoothIndex(CurrentVertexCount - 1, SmoothingGroup, CurrentVertexCount - 1, SmoothIndices);
                           // mark new vertex also as touched
                           MarkVertex(Marker, CurrentVertexCount - 1);
                        end else begin
                           // vertex has already been duplicated,
                           // so just add normal vector to other vertex...
                           mesh.Normals[TargetVertex]:=VectorAdd(mesh.Normals[TargetVertex], Normal);
                           // ...and tell which new vertex has to be used from now on
                           FaceRec[Vertex]:=TargetVertex;
                        end;
                     end;
                  end else begin
                     // vertex not yet touched, so just store the normal
                     mesh.Normals[CurrentIndex]:=Normal;
                     // initialize smooth indices for this vertex
                     FillChar(SmoothIndices[CurrentIndex], SizeOf(TSmoothIndexEntry), $FF);
                     if SmoothingGroup <> 0 then
                        StoreSmoothIndex(CurrentIndex, SmoothingGroup, CurrentIndex, SmoothIndices);
                     MarkVertex(Marker, CurrentIndex);
                  end;
               end;
            end;
         end;
         FreeMem(Marker);
         FreeMem(SmoothIndices);

         Assert(mesh.Vertices.Count=CurrentVertexCount);

         // and normalize the Normals array
         mesh.Normals.Normalize;

         // now go for each material group
         // if there's no face to material assignment then just copy the
         // face definitions and rely on the default texture of the scene object
         if (NMats = 0) or (not vGLVectorFileObjectsAllocateMaterials) then begin
            aFaceGroup:=TFGVertexIndexList.CreateOwned(mesh.FaceGroups);
            with aFaceGroup do begin
               if TGLBaseMesh(GetOwner).MaterialLibrary <> nil then
                  MaterialName:=TGLBaseMesh(GetOwner).MaterialLibrary.Materials.Add.Name;
               // copy the face list
               for j:=0 to NFaces-1 do begin
                  Add(FaceArray[J].V1);
                  Add(FaceArray[J].V2);
                  Add(FaceArray[J].V3);
               end;
            end;
         end else begin
            for iMaterial:=0 to NMats - 1 do begin
               aFaceGroup:=TFGVertexIndexList.CreateOwned(mesh.FaceGroups);
               with aFaceGroup do begin
                  MaterialName:=GetOrAllocateMaterial(Materials, MatArray[iMaterial].NameStr);
                  LightMapIndex:=GetOrAllocateLightMap(Materials, MatArray[iMaterial].NameStr);
                  // copy all vertices belonging to the current face into our index array,
                  // there won't be redundant vertices since this would mean a face has more than one
                  // material
                  // NFaces is the one from FaceGroup
                  with MatArray[iMaterial] do for j:=0 to NFaces - 1 do begin
                     Add(FaceArray[FaceIndex[J]].V1);
                     Add(FaceArray[FaceIndex[J]].V2);
                     Add(FaceArray[FaceIndex[J]].V3);
                  end;
               end;
            end;
         end;
         if hasLightMap then
            for j:=0 to mesh.TexCoords.Count-1 do
               mesh.LightMapTexCoords.Add(mesh.TexCoords[j][0], mesh.TexCoords[j][1]);
      end;

      // Adding non-mesh objects (for example, dummies).
      for I := 0 to KeyFramer.MeshMotionCount - 1 do
        if (Owner.MeshObjects.FindMeshByName(KeyFramer.MeshMotion[I].NameStr) = nil) then
        begin
          mesh := TGLFile3DSMeshObject.CreateOwned(Owner.MeshObjects);
          mesh.Name := KeyFramer.MeshMotion[I].NameStr;
          //dummy targets
          for X := 0 to KeyFramer.Settings.Anim.Length - 1 do
            TMeshMorphTarget.CreateOwned(mesh.MorphTargets);

          mesh.LoadAnimation(KeyFramer.MeshMotion[I]);
        end;

      for I := 0 to Objects.MeshCount - 1 do
        with PMesh3DS(Objects.Mesh[I])^ do
        begin
          mesh := Owner.MeshObjects.FindMeshByName(NameStr) as TGLFile3DSMeshObject;
          with mesh, KeyFramer do
          begin
            CurrentMotionIndex := FindMotionIndex(KeyFramer, NameStr);
            FRefTranf.ModelMatrix := InvertMeshMatrix(Objects, NameStr);
            if MeshMotionCount > 0 then
              LoadAnimation(MeshMotion[CurrentMotionIndex]);
          end;
        end;


      // Lights Omni.
      for I := 0 to Objects.OmniLightCount - 1 do
      begin
        lights_mesh := TGLFile3DSOmniLightObject.CreateOwned(Owner.MeshObjects);
        // Dummy targets for it.
        for X := 0 to KeyFramer.Settings.Anim.Length - 1 do
          TMeshMorphTarget.CreateOwned(lights_mesh.MorphTargets);
        lights_mesh.LoadData(Owner, Objects.OmniLight[I]);
        if KeyFramer.OmniLightMotionCount > 0 then
          lights_mesh.LoadAnimation(KeyFramer.OmniLightMotion[I]);
      end;

      // Lights Spot.
      for I := 0 to Objects.SpotLightCount - 1 do
      begin
        lights_mesh := TGLFile3DSSpotLightObject.CreateOwned(Owner.MeshObjects);
        // Dummy targets for it.
        for X := 0 to KeyFramer.Settings.Anim.Length - 1 do
          TMeshMorphTarget.CreateOwned(lights_mesh.MorphTargets);
        lights_mesh.LoadData(Owner, Objects.SpotLight[I]);
        if KeyFramer.SpotLightMotionCount > 0 then
          lights_mesh.LoadAnimation(KeyFramer.SpotLightMotion[I]);
      end;

      // Camera Objects.
      for I := 0 to Objects.CameraCount - 1 do
      begin
        camera_mesh := TGLFile3DSCameraObject.CreateOwned(Owner.MeshObjects);
        // Dummy targets for it.
        for X := 0 to KeyFramer.Settings.Anim.Length - 1 do
          TMeshMorphTarget.CreateOwned(camera_mesh.MorphTargets);
        camera_mesh.LoadData(Owner, Objects.Camera[I]);
        if KeyFramer.CameraMotionCount > 0 then
          camera_mesh.LoadAnimation(KeyFramer.CameraMotion[I]);
      end;

   finally
      Free;
   end;   
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
   RegisterClasses([TGLFile3DSDummyObject, TGLFile3DSMeshObject,
                    TGLFile3DSOmniLightObject, TGLFile3DSSpotLightObject,
                    TGLFile3DSCameraObject]);

   RegisterVectorFileFormat('3ds', '3D Studio files', TGL3DSVectorFile);
   RegisterVectorFileFormat('prj', '3D Studio project files', TGL3DSVectorFile);

end.
