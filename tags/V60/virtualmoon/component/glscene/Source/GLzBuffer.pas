//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLzBuffer<p>

   ZBuffer retrieval and computations.<p>

   See readme.txt in the Demos/SpecialsFX/Shadows directory.<br>
   By Ren� Lindsay.<p>

 <b>History : </b><font size=-1><ul>
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>22/04/10 - Yar - Fixes after GLState revision
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>29/05/08 - DaStr - Removed unused variables in TGLZShadows.CalcShadowTexture()
      <li>20/01/08 - DaStr - Bugfixed TGLZShadows.HardSet(Bugtracker ID = 1875708)
                             Removed some old commented out code
                             Removed TGLZShadows.Trnc() procedure - there is a
                              similar procedure in VectorGeometry.pas
      <li>28/03/07 - DaStr - Renamed parameters in some methods
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678658)
      <li>24/03/07 - DaStr - Improved Cross-Platform compatibility
                             (thanks Burkhard Carstens) (Bugtracker ID = 1684432)
      <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTracekrID=1681585)
      <li>16/03/07 - DaStr - Added explicit pointer dereferencing
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
      <li>08/02/07 - Lin - LONG overdue bugfix: Now sets GL_BLEND to prevent black screen.(thanks Jurgen Linker)
      <li>08/03/06 - ur - Fixed warnigs for Delphi 2006
      <li>03/07/04 - LR - Added ifdef for Linux
      <li>07/03/02 - Lin - Removed XRes/YRes properties - Shadow-res now always matches viewer-res.
      <li>21/02/02 - Lin - Now uses 1 Byte per pixel, instead of 4. Faster, and uses less Video Ram.
      <li>14/02/02 - Lin - Bugfix: No longer stalls the Cadencer + small speed improvements
      <li>12/02/02 - Lin - Bilinear filtering of soft shadow edges - much better quality
      <li>08/02/02 - Lin - Removed the material property (not used)
      <li>05/02/02 - Lin - Tolerance scaling - reduces shadow-creeping(far) and self-shadowing(near)
      <li>05/02/02 - Lin - A little more speed in 16in1 mode (but 9in1 is still the best quality)
      <li>05/02/02 - EG  - Fixed glTex[Sub]Image calls
      <li>20/11/01 - EG  - Removed warnings (axed out... hope I didn't broke anything)
      <li>17/10/01 - Lin - Added Xres and Yres...makes shadow texture size independent from viewer.
                           Calculations now use z-depth in stead of world distance
                           - more acurate, and 15% faster.
      <li>27/09/01 - Lin - Bypass the GLScene Material.texture.image, and send the shadow
                           texture directly to OpenGL. This increased speed by almost 20%
      <li>25/09/01 - Lin - Add Optimise property to specify faster rastering methods
      <li>07/09/01 - Lin - Restructure zBuffer code, to support the new TGLMemoryViewer
      <li>06/09/01 - Lin - Created TGLZShadows object, for casting shadows
      <li>30/08/01 - Lin - More speed + bugfixes
      <li>24/07/01 - Lin - Greatly improved speed
      <li>07/07/01 - Lin - Added PixelToWorld, WorldToPixel, and PixelToDistance
      <li>01/07/01 - Lin - Precalculate the corner vectors in GetDepthBuffer,
                            to speed up FastVectorToScreen and FastScreenToVector
      <li>28/06/01 - Lin - First operational code
      <li>26/06/01 - Lin - Creation of zBuffer class
 </ul></font>
}

   //--------These formulas are the key to making use of the z-Buffer--------
   //
   // dst (d): world distance
   // dov    : depth of view (distance between Far-plane and Near-plane)
   // np     : near plane
   // fp     : far plane (dov+np)
   //
   //------------------------
   //dst:=(fp*np)/(fp-z*dov);  //calc from z-buffer value to frustrum depth
   //z  :=(1-np/d)/(1-np/fp);  //calc from frustrum depth to z-buffer value
   //------------------------  z:=1-(fp/d-1)/(fp/np-1); //old FtoZ
   //------------------------------------------------------------------------

unit GLzBuffer;

interface

{$I GLScene.inc}

uses Classes, SysUtils, GLScene, VectorGeometry, GLGraphics,
  GLObjects, GLContext, GLViewer, GLColor,
  GLRenderContextInfo, GLState, GLTextureFormat;

type
  EZBufferException = class(Exception);

  TZArray = array[0..MaxInt shr 3] of Single;
  PZArray = ^TZArray;
  TZArrayIdx = array of PZArray;

  TAArray = array[0..MaxInt shr 3] of Byte;
  PAArray = ^TAArray;
  TAArrayIdx = array of PAArray;

  TOptimise = (opNone, op4in1, op9in1, op16in1);

  TGLzBuffer = class(TPersistent)
  private
    FData: PZArray;
    FDataIdx, FDataInvIdx: TZArrayIdx;
    FWidth, FHeight: Integer;
    FDataSize: Integer;

    ang1, ang2, scal, c1, s1, c2, s2, vw, vh: single; //VectorToScreen variables;
    lt, rt, lb, rb: TAffineVector; //ScreenToVector corner vectors;
    UpVec, riVec: TAffineVector;

    ltW, rtW, lbW, rbW: TAffineVector; //ScreenToVector corner vectors;(Warped)
    UpVecW, riVecW: TAffineVector;
    OrthInvDov, OrthAddX, OrthMulX, OrthAddY, OrthMulY: single;

    dov, np, fp, NpFp, OneMinNp_Fp, invOneMinNp_Fp: single; //Calc Variables;

    cam: TGLCamera;

    procedure DoCalcVectors;

  protected
    procedure PrepareBufferMemory;
    procedure SetWidth(val: Integer);
    procedure SetHeight(const val: Integer);

  public
    SceneViewer: TGLSceneViewer;
    MemoryViewer: TGLMemoryViewer;
    Buffer: TGLSceneBuffer;

    Normal: TAffineVector; //Absolute direction of camera

    constructor Create;
    destructor Destroy; override;

    procedure LinkToViewer(viewer: TGLSceneViewer); overload;
    procedure LinkToViewer(viewer: TGLMemoryViewer); overload;
    function GetDepthBuffer(CalcVectors: Boolean; ContextIsActive: boolean):
      PZArray;

    function GetPixelzDepth(x, y: integer): Single;
    function PixelToDistance_OLD(x, y: integer): Single;
    function PixelToDistance(x, y: integer): Single;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property DataSize: Integer read FDataSize;
    property Data: PZArray read FData;
    property DataIdx: TZArrayIdx read FDataIdx;
    property DataInvIdx: TZArrayIdx read FDataIdx;

    procedure Refresh;
    function FastScreenToVector(x, y: Integer): TAffineVector;
    function FastVectorToScreen(vec: TAffineVector): TAffineVector;

    function PixelToWorld(const x, y: Integer): TAffineVector;
    function WorldToPixel(const aPoint: TAffineVector; out pixX, pixY: integer;
      out pixZ: single): boolean;
    function WorldToPixelZ(const aPoint: TAffineVector; out pixX, pixY: integer;
      out pixZ: single): boolean; overload;
    function WorldToPixelZ(const aPoint: TAffineVector; out pixX, pixY: single;
      out pixZ: single): boolean; overload;
    function OrthWorldToPixelZ(const aPoint: TAffineVector; out pixX, pixY:
      single; out pixZ: single): boolean;
  end;

  // TGLZShadows
  //
  TGLZShadows = class(TGLBaseSceneObject)
  private
    FViewer: TGLSceneViewer;
    FCaster: TGLMemoryViewer;
    FDepthFade: Boolean;
    FFrustShadow: Boolean;
    FSkyShadow: Boolean;
    FOptimise: TOptimise;

    FData: PAArray;
    FDataIdx, FDataInvIdx: TAArrayIdx;
    FDataSize: Integer;

    FWidth: integer;
    FHeight: integer;
    FXRes: integer;
    FYRes: integer;
    Fsoft: boolean;
    FTolerance: single;

    FColor: TGLColor;
    SCol: TGLPixel32;

    //stepX, stepY :single;

    FTexturePrepared: Boolean;

    FTexHandle: TGLTextureHandle;

  protected
    procedure PrepareAlphaMemory;

    function GetViewer: TGLSceneViewer;
    procedure SetViewer(const val: TGLSceneViewer);
    function GetCaster: TGLMemoryViewer;
    procedure SetCaster(const val: TGLMemoryViewer);
    procedure CalcShadowTexture(var rci: TRenderContextInfo);
    function HardSet(const x, y: integer): Byte;

    function SoftTest(const x, y: integer): Byte;
    procedure SetWidth(const val: integer);
    procedure SetHeight(const val: integer);
    procedure SetXRes(const val: integer);
    procedure SetYRes(const val: integer);
    procedure SetSoft(const val: boolean);

    procedure BindTexture;
  public
    ViewerZBuf: TGLzBuffer;
    CasterZBuf: TGLzBuffer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TRenderContextInfo; ARenderSelf,
      ARenderChildren: Boolean); override;
  published
    property Viewer: TGLSceneViewer read GetViewer write SetViewer;
    property Caster: TGLMemoryViewer read GetCaster write SetCaster;
    property FrustShadow: Boolean read FFrustShadow write FFrustShadow;
    property SkyShadow: Boolean read FSkyShadow write FSkyShadow;
    property Optimise: TOptimise read FOptimise write FOptimise;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property Color: TGLColor read FColor write FColor;
    //          property Xres        :integer read FXRes write SetXRes;// default 64;
    //          property Yres        :integer read FYRes write SetYRes;// default 64;
    property Soft: Boolean read Fsoft write SetSoft;
    property Tolerance: single read FTolerance write FTolerance;
    //          property Material;
    property ObjectsSorting;
    property Visible;

    property DepthFade: Boolean read FDepthFade write FDepthFade;
    function CastShadow: boolean;
  end;

implementation

uses OpenGLTokens, XOpenGL {$IFDEF GLS_DELPHI}, VectorTypes{$ENDIF};

constructor TGLzBuffer.Create;
begin
  inherited Create;

  self.FWidth := 0;
  self.FHeight := 0;
  self.FDataSize := 0;
  self.cam := nil;
  self.SceneViewer := nil;
  self.MemoryViewer := nil;
  self.buffer := nil;
  // self.DoCalcVectors;
end;

procedure TGLzBuffer.LinkToViewer(viewer: TGLSceneViewer); // overload;
begin
  if ((FWidth <> Viewer.width) or (FHeight <> Viewer.height)) then
  begin
    FWidth := Viewer.width;
    FHeight := Viewer.height;
    PrepareBufferMemory;
  end;
  cam := Viewer.camera;
  SceneViewer := Viewer;
  Buffer := Viewer.Buffer;
  self.DoCalcVectors;
end;

procedure TGLzBuffer.LinkToViewer(viewer: TGLMemoryViewer); // overload;
begin
  if ((FWidth <> Viewer.width) or (FHeight <> Viewer.height)) then
  begin
    FWidth := Viewer.width;
    FHeight := Viewer.height;
    PrepareBufferMemory;
  end;
  cam := Viewer.camera;
  MemoryViewer := Viewer;
  Buffer := Viewer.Buffer;
  self.DoCalcVectors;
end;

//---Destroy---

destructor TGLzBuffer.Destroy;
begin
  FreeMem(FData);
  inherited Destroy;
end;

procedure TGLzBuffer.PrepareBufferMemory;
var
  i: Integer;
begin
  FDataSize := FWidth * FHeight * 4;
  ReallocMem(FData, FDataSize);
  SetLength(FDataIdx, FHeight + 2);
  SetLength(FDataInvIdx, FHeight + 2);
  for i := 0 to FHeight - 1 do
  begin
    FDataIdx[i] := @FData[i * FWidth]; // range: [0..height-1]
    FDataInvIdx[i] := @FData[(FHeight - i - 1) * FWidth]; // range: [0..height-1]
  end;
  FDataIdx[FHeight] := FDataIdx[FHeight - 1];
  FDataInvIdx[FHeight] := FDataInvIdx[FHeight - 1];
end;

//---Width---

procedure TGLzBuffer.SetWidth(val: Integer);
begin
  if val <> FWidth then
  begin
    Assert(val >= 0);
    FWidth := val;
    PrepareBufferMemory;
  end;
end;

//---Height---

procedure TGLzBuffer.SetHeight(const val: Integer);
begin
  if val <> FHeight then
  begin
    Assert(val >= 0);
    FHeight := val;
    PrepareBufferMemory;
  end;
end;

function TGLzBuffer.GetDepthBuffer(CalcVectors: Boolean; ContextIsActive:
  boolean): PZArray;
begin
  if ContextIsActive = True then
  begin
    GL.ReadPixels(0, 0, FWidth, FHeight, GL_DEPTH_COMPONENT, GL_FLOAT, FData);
  end
  else
  begin
    Buffer.RenderingContext.Activate;
    try
      GL.ReadPixels(0, 0, FWidth, FHeight, GL_DEPTH_COMPONENT, GL_FLOAT, FData);
    finally
      Buffer.RenderingContext.Deactivate;
    end;
  end;

  if CalcVectors = True then
    DoCalcVectors;
  Result := FData;
end;

function TGLzBuffer.GetPixelzDepth(x, y: integer): Single;
begin
  if (Cardinal(x) < Cardinal(FWidth)) and (Cardinal(y) < Cardinal(FHeight)) then
    Result := FDataInvIdx[y]^[x]
  else
    Result := 0;
end;

function TGLzBuffer.PixelToDistance_OLD(x, y: integer): Single;
var
  z, dst, camAng, wrpdst: single;
  vec: TAffineVector;
begin
  if ((x < 0) or (x > FWidth) or (y < 0) or (y > FWidth)) then
    result := 0
  else
  begin
    z := FData^[x + (FHeight - y) * FWidth]; //fetch pixel z-depth
    dst := (NpFp) / (fp - z * dov); //calc from z-buffer value to frustrum depth
    vec := FastScreenToVector(x, y);
    camAng := VectorAngleCosine(normal, vec);
    wrpdst := dst / camAng; //compensate for flat frustrum face
    result := wrpdst;
  end;
end;

function TGLzBuffer.PixelToDistance(x, y: integer): Single;
var
  z, dst: single;
  xx, yy, zz: single;
  fy: integer;
begin
  if ((x < 0) or (x >= FWidth) or (y < 0) or (y >= FHeight)) then
    result := 0
  else
  begin
    fy := FHeight - y;
    z := FData^[x + fy * FWidth]; //fetch pixel z-depth
    if z < 1 then
    begin
      dst := (NpFp) / (fp - z * dov);
      //calc from z-buffer value to frustrum depth
      xx := (lbW[0] + riVecW[0] * x + UpVecW[0] * fy);
      yy := (lbW[1] + riVecW[1] * x + UpVecW[1] * fy);
      zz := (lbW[2] + riVecW[2] * x + UpVecW[2] * fy);
      result := sqrt(xx * xx + yy * yy + zz * zz) * dst;
    end
    else
      result := 0;
  end;
end;

procedure TGLzBuffer.Refresh;
begin
  if assigned(Buffer) then
    GetDepthBuffer(True, False);
end;

procedure TGLzBuffer.DoCalcVectors;
var
  axs: TAffineVector;
  Hnorm, hcvec: TVector;
  vec: TAffineVector;
  w, h: integer;
  wrp: single;
begin
  if not (assigned(Buffer) and Buffer.RCInstantiated) then
    exit;
  if not assigned(cam) then
    raise EZBufferException.Create('No Camera!');

  //-----------For ScreenToVector-------------
  w := FWidth;
  h := FHeight;
  setVector(vec, 0, 0, 0);
  lb := buffer.ScreenToVector(vec); // same as cvec...optimise?
  setVector(vec, w, 0, 0);
  rb := buffer.ScreenToVector(vec);
  setVector(vec, 0, h, 0);
  lt := buffer.ScreenToVector(vec);
  setVector(vec, w, h, 0);
  rt := buffer.ScreenToVector(vec);
  //------------Set Camera values-------------
  normal := VectorLerp(lb, rt, 0.5);
  upVec := VectorSubtract(lt, lb);
  riVec := VectorSubtract(rb, lb);
  // cam:=viewer.Camera;
  dov := Cam.DepthOfView;
  np := Cam.NearPlane;
  fp := Cam.NearPlane + dov;
  NpFp := np * fp;
  OneMinNp_Fp := 1 - np / fp;
  invOneMinNp_Fp := 1 / OneMinNp_Fp;
  //-----------For VectorToScreen-------------
  {
    cam :=Viewer.Camera.Position.AsAffineVector;
    targ:=Viewer.Camera.TargetObject.Position.AsAffineVector;
    norm:=VectorSubtract(targ,cam);     //---Camera Normal vector---
    MakeVector(hnorm,norm);
  }
  MakeVector(hnorm, normal);

  MakeVector(hcVec, lb); //---Corner Vector---
  ang1 := ArcTan2(Hnorm[0], Hnorm[2]);
  SetVector(axs, 0, 1, 0);
  RotateVector(hnorm, axs, ang1);
  RotateVector(hcvec, axs, ang1);

  ang2 := ArcTan2(Hnorm[1], Hnorm[2]);
  SetVector(axs, 1, 0, 0);
  RotateVector(hcvec, axs, -ang2);

  hcvec[0] := hcvec[0] / hcvec[2];
  vw := Fwidth / 2;
  vh := Fheight / 2;
  scal := vw / hcvec[0];
  SinCos(-ang1, s1, c1);
  SinCos(-ang2, s2, c2);
  //------------------------------------------
  //--------------------2-----------------
  vec := self.FastScreenToVector(0, 1);
  wrp := VectorAngleCosine(normal, vec);
  ltW := VectorNormalize(lt);
  rtW := VectorNormalize(rt);
  lbW := VectorNormalize(lb);
  rbW := VectorNormalize(rb);
  ltW := VectorScale(ltW, 1 / wrp);
  rtW := VectorScale(rtW, 1 / wrp);
  lbW := VectorScale(lbW, 1 / wrp);
  rbW := VectorScale(rbW, 1 / wrp);
  upVecW := VectorSubtract(ltW, lbW);
  upVecW := VectorScale(upVecW, 1 / Fheight);
  riVecW := VectorSubtract(rbW, lbW);
  riVecW := VectorScale(riVecW, 1 / Fwidth);

  // UpVecW[0]:=-UpVecW[0];
  // UpVecW[1]:=-UpVecW[1];
  // UpVecW[2]:=-UpVecW[2];

  //--------------------------------------
  //-------orth---------
  // OrthAdd:=2;
  // OrthMul:=64;

  orthAddX := rt[0];
  OrthMulX := FWidth / (OrthAddX * 2);
  orthAddY := rt[2];
  OrthMulY := FHeight / (OrthAddY * 2);
  OrthInvDov := 1 / dov;

  //--------------------
end;

function TGLzBuffer.FastScreenToVector(x, y: integer): TAffineVector;
var
  w, h: integer;
  Rlerp, Ulerp: single;
begin
  w := FWidth;
  h := FHeight;
  Rlerp := x / w;
  Ulerp := (h - y) / h;
  result[0] := lb[0] + riVec[0] * Rlerp + UpVec[0] * Ulerp;
  result[1] := lb[1] + riVec[1] * Rlerp + UpVec[1] * Ulerp;
  result[2] := lb[2] + riVec[2] * Rlerp + UpVec[2] * Ulerp;
end;

function TGLzBuffer.FastVectorToScreen(Vec: TAffineVector): TAffineVector;
var
  v0, v1, x, y, z: Single;
begin
  x := vec[0];
  y := vec[1];
  z := vec[2];
  v0 := x;
  x := c1 * v0 + s1 * z;
  z := c1 * z - s1 * v0; //Rotate around Y-axis
  v1 := y;
  y := c2 * v1 + s2 * z;
  z := c2 * z - s2 * v1; //Rotate around X-axis
  Result[0] := Round(-x / z * scal + vw);
  Result[1] := Round(y / z * scal + vh);

end;

function TGLzBuffer.PixelToWorld(const x, y: Integer): TAffineVector;
var
  z, dst: single;
  fy: integer;
  camvec: TVector;
begin
  // if (Cardinal(x)<Cardinal(FWidth)) and (Cardinal(y)<Cardinal(FWidth)) then begin       //xres,yres?
  if (x < FWidth) and (y < FHeight) then
  begin
    z := FDataInvIdx[y]^[x];
    dst := (NpFp) / (fp - z * dov); //calc from z-buffer value to frustrum depth
    camvec := cam.AbsolutePosition;
    fy := FHeight - y;
    result[0] := (lbW[0] + riVecW[0] * x + UpVecW[0] * fy) * dst + camvec[0];
    result[1] := (lbW[1] + riVecW[1] * x + UpVecW[1] * fy) * dst + camvec[1];
    result[2] := (lbW[2] + riVecW[2] * x + UpVecW[2] * fy) * dst + camvec[2];
  end
  else
  begin
    result[0] := 0;
    result[1] := 0;
    result[2] := 0;
  end;
end;

function TGLzBuffer.WorldToPixel(const aPoint: TAffineVector; out pixX, pixY:
  integer; out pixZ: single): boolean;
var
  camPos: TVector;
  x, y, z, v0, v1, zscal: single;
begin
  //---Takes x,y,z world coordinate.
  //---Result is true if pixel lies within view frustrum
  //---returns canvas pixel x,y coordinate, and the world distance
  result := false;
  campos := cam.AbsolutePosition;
  x := apoint[0] - camPos[0];
  y := apoint[1] - camPos[1];
  z := apoint[2] - camPos[2]; //get vector from camera to world point
  v0 := x;
  x := c1 * v0 + s1 * z;
  z := c1 * z - s1 * v0; //Rotate around Y-axis
  v1 := y;
  y := c2 * v1 + s2 * z;
  z := c2 * z - s2 * v1; //Rotate around X-axis
  if z > 0 then
  begin
    zscal := scal / z;
    pixX := Round(-x * zscal + vw);
    pixY := Round(y * zscal + vh);
    pixZ := sqrt(x * x + y * y + z * z);
    if (pixX >= 0) and (pixX < FWidth) and (pixY >= 0) and (pixY < FHeight) then
      Result := true;
  end
  else
  begin //ignore anything that is behind the camera
    pixX := 0;
    pixY := 0;
    pixZ := 0;
  end;
end;

function TGLzBuffer.WorldToPixelZ(const aPoint: TAffineVector; out pixX, pixY:
  integer; out pixZ: single): boolean; //OVERLOAD
var
  camPos: TVector;
  x, y, z, v0, v1, zscal: single;
begin
  //---Takes x,y,z world coordinate.
  //---Result is true if pixel lies within view frustrum
  //---returns canvas pixel x,y coordinate, and CALCULATES the z-buffer distance
  campos := cam.AbsolutePosition;
  x := apoint[0] - camPos[0];
  y := apoint[1] - camPos[1];
  z := apoint[2] - camPos[2]; //get vector from camera to world point
  v0 := x;
  x := c1 * v0 + s1 * z;
  z := c1 * z - s1 * v0; //Rotate around Y-axis
  v1 := y;
  y := c2 * v1 + s2 * z;
  z := c2 * z - s2 * v1; //Rotate around X-axis
  if z > 0 then
  begin
    zscal := scal / z;
    pixX := Round(-x * zscal + vw);
    pixY := Round(y * zscal + vh);
    //------z:=(1-np/z)/(1-np/fp);------
    //      pixZ:=(1-np/z)/(1-np/fp);
    pixZ := (1 - np / z) * invOneMinNp_Fp;
    Result := (Cardinal(pixX) < Cardinal(FWidth)) and (Cardinal(pixY) <
      Cardinal(FHeight));
  end
  else
  begin //ignore anything that is behind the camera
    Result := false;
    pixX := 0;
    pixY := 0;
    pixZ := 0;
  end;
end;

function TGLzBuffer.WorldToPixelZ(const aPoint: TAffineVector; out pixX, pixY:
  single; out pixZ: single): boolean; //OVERLOAD
var
  camPos: TVector;
  x, y, z, invZ, v0, v1, zscal: single;
begin
  //---Takes x,y,z world coordinate. (aPoint)
  //---Result is true if pixel lies within view frustrum
  //---returns canvas pixel x,y coordinate, and CALCULATES the z-buffer distance
  campos := cam.AbsolutePosition;
  x := apoint[0] - camPos[0];
  y := apoint[1] - camPos[1];
  z := apoint[2] - camPos[2]; //get vector from camera to world point
  v0 := x;
  x := c1 * v0 + s1 * z;
  z := c1 * z - s1 * v0; //Rotate around Y-axis
  v1 := y;
  y := c2 * v1 + s2 * z;
  z := c2 * z - s2 * v1; //Rotate around X-axis
  if z > 0 then
  begin
    invZ := 1 / z;
    zscal := scal * invZ;
    pixX := vw - x * zscal;
    pixY := vh + y * zscal;
    //------z:=(1-np/z)/(1-np/fp);------
    //      pixZ:=(1-np/z)/(1-np/fp);
    pixZ := (1 - np * invZ) * invOneMinNp_Fp;
    Result := (pixX >= 0) and (pixX < FWidth) and (pixY >= 0) and (pixY <
      FHeight);
  end
  else
  begin //ignore anything that is behind the camera
    result := false;
    pixX := 0;
    pixY := 0;
    pixZ := 0;
  end;
end;

function TGLzBuffer.OrthWorldToPixelZ(const aPoint: TAffineVector; out pixX,
  pixY: single; out pixZ: single): boolean;
var
  camPos: TVector;
  x, y, z: single;
begin
  campos := cam.AbsolutePosition;
  x := apoint[0] - camPos[0];
  y := apoint[1] - camPos[1];
  z := apoint[2] - camPos[2]; //get vector from camera to world point

  pixX := (x + OrthAddX) * OrthMulX;
  pixY := (z + OrthAddY) * OrthMulY;
  pixZ := (-y - np) * OrthInvDov; //(-y-np)/dov
  Result := (pixX >= 0) and (pixX < FWidth) and (pixY >= 0) and (pixY <
    FHeight);
end;

// ------------------
// ------------------ TGLZShadows ------------------
// ------------------

// Create
//

constructor TGLZShadows.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FColor := TGLColor.Create(Self);
  self.FDataSize := 0;
  self.FXRes := 64;
  self.FYRes := 64;
  self.Tolerance := 0.015;
  FTexHandle := TGLTextureHandle.Create;
end;

//---Destroy---

destructor TGLZShadows.Destroy;
begin
  ViewerZBuf.Free;
  CasterZBuf.Free;
  FColor.Free;
  FTexHandle.Free;
  FreeMem(FData);
  inherited Destroy;
end;

// BindTexture
//

procedure TGLZShadows.BindTexture;
begin
  if FTexHandle.Handle = 0 then
    with FTexHandle do
    begin
      AllocateHandle;
      with RenderingContext.GLStates do
      begin
        TextureBinding[0, ttTexture2D] := Handle;

        GL.Hint(GL_PERSPECTIVE_CORRECTION_HINT, GL_Fastest);
        UnpackAlignment := 1;
        UnpackRowLength := 0;
        UnpackSkipRows := 0;
        UnpackSkipPixels := 0;

        GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

        ActiveTextureEnabled[ttTexture2D] := True;
        SetBlendFunc(bfSRCALPHA, bfONEMINUSSRCALPHA);
        GL.TexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
        Enable(stBlend);

        PrepareAlphaMemory;
      end;
    end
  else
    with FTexHandle do
      RenderingContext.GLStates.TextureBinding[0, ttTexture2D] := Handle;
end;

procedure TGLZShadows.PrepareAlphaMemory;
var
  i: Integer;
begin
  //   ShowMessage(IntToStr(FWidth)+'  '+IntToStr(FXRes));
  FDataSize := FXRes * FYRes * 1;
  ReallocMem(FData, FDataSize);
  SetLength(FDataIdx, FYRes);
  SetLength(FDataInvIdx, FYRes);
  for i := 0 to FYres - 1 do
  begin
    FDataIdx[i] := @FData[i * FXRes]; // range: [0..height-1]
    FDataInvIdx[i] := @FData[(FYRes - i - 1) * FXRes]; // range: [0..height-1]
  end;
end;

// DoRender
//

procedure TGLZShadows.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  vx, vy, vx1, vy1: Single;
  xtex, ytex: single;
begin
  if not assigned(FViewer) then
    exit;
  if not assigned(FCaster) then
    exit;
  if not assigned(CasterZBuf) then
    exit; //only render if a shadow has been cast
  //only render in view-camera
  if TGLSceneBuffer(ARci.buffer).Camera <> FViewer.Camera then
    exit;
  if not assigned(ViewerZBuf) then
  begin //Create viewer zbuffer
    ViewerZBuf := TGLZBuffer.Create;
    ViewerZBuf.LinkToViewer(FViewer);
    Bindtexture;
    FTexturePrepared := False;
  end;
  ViewerZBuf.Refresh;

  ARci.GLStates.ActiveTextureEnabled[ttTexture2D] := True;
  ARci.GLStates.Enable(stBlend);
  ARci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);

  if FWidth > ARci.viewPortSize.cx then
    Fwidth := ARci.viewPortSize.cx;
  if FHeight > ARci.viewPortSize.cy then
    FHeight := ARci.viewPortSize.cy;

  //-----------------------
  CalcShadowTexture(ARci);
  //-----------------------
  ARci.GLStates.TextureBinding[0, ttTexture2D] := FTexHandle.Handle;

  //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  GL.Color3f(SCol.r, SCol.g, SCol.b);

  if not FTexturePrepared then
  begin
    GL.TexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA, FXRes, FYRes, 0, GL_ALPHA,
      GL_UNSIGNED_BYTE, @FData[0]);
    FTexturePrepared := True;
  end
  else
    GL.TexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, FXRes, FYRes, GL_ALPHA,
      GL_UNSIGNED_BYTE, @FData[0]);

  //   NotifyChange(Self);
  //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

     // Prepare matrices
  GL.MatrixMode(GL_MODELVIEW);
  GL.PushMatrix;
  GL.LoadMatrixf(@TGLSceneBuffer(ARci.buffer).BaseProjectionMatrix);
  GL.Scalef(2 / ARci.viewPortSize.cx, 2 / ARci.viewPortSize.cy, 1);
  GL.Translatef(Position.X - ARci.viewPortSize.cx * 0.5,
    ARci.viewPortSize.cy * 0.5 - Position.Y, Position.Z);

  GL.MatrixMode(GL_PROJECTION);
  GL.PushMatrix;
  GL.LoadIdentity;
  ARci.GLStates.Disable(stDepthTest);
  ARci.GLStates.Disable(stLighting);

  vx := 0;
  vx1 := vx + FWidth;
  vy := 0;
  vy1 := vy - FHeight;

  Xtex := FWidth / FXres; //1
  Ytex := 1 - (FHeight / FYres); //0

  // issue quad
  GL.Begin_(GL_QUADS);
  GL.Normal3fv(@YVector);
  GL.TexCoord2f(0, ytex);
  GL.Vertex2f(vx, vy1);
  GL.TexCoord2f(xtex, ytex);
  GL.Vertex2f(vx1, vy1);
  GL.TexCoord2f(xtex, 1);
  GL.Vertex2f(vx1, vy);
  GL.TexCoord2f(0, 1);
  GL.Vertex2f(vx, vy);
  GL.End_;
  // restore state
  GL.PopMatrix;
  GL.MatrixMode(GL_MODELVIEW);
  GL.PopMatrix;

  if Count > 0 then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TGLZShadows.CalcShadowTexture(var rci: TRenderContextInfo);
var
  pix, p0, p1, p2, p3, p4: Byte;
  pM, pL, pT: Byte;
  pixa: PAArray;
  x, y, w, h: integer;
  xy: integer;
begin
  pixa := FData;
  w := fXres;
  h := fYres;

  SCol.r := Round(FColor.Red * 255);
  SCol.g := Round(FColor.green * 255);
  SCol.b := Round(FColor.Blue * 255);
  SCol.a := Round(FColor.Alpha * 255);

  //-----------No optimising-----------
  if FOptimise = opNone then
  begin

    y := 0;
    while y < FHeight do
    begin
      x := 0;
      while x < fWidth do
      begin
        HardSet(x, y);
        x := x + 1;
      end;
      y := y + 1;
    end;
  end
  else

    if FOptimise = op4in1 then
    begin
      for x := 0 to fXres - 1 do
        HardSet(x, 0);
      for x := 0 to fXres - 1 do
        HardSet(x, fYres - 1);
      for y := 1 to fYres - 1 do
        HardSet(0, y);
      for y := 1 to fYres - 1 do
        HardSet(fXres - 1, y);
      y := 2;
      while y < fYres do
      begin
        x := 2;
        p1 := HardSet(x - 1, y - 2);
        HardSet(x - 1, y - 1);
        p0 := HardSet(x - 1, y);
        while x < fXres do
        begin
          pix := HardSet(x, y);
          if (pix = p1) and (pix = p0) then
          begin
            FDataInvIdx[y]^[x - 1] := pix;
            FDataInvIdx[y - 1]^[x - 1] := pix;
          end
          else
          begin
            HardSet(x - 1, y);
            HardSet(x - 1, y - 1);
          end;
          p2 := SoftTest(x + 1, y - 2);

          if (pix = p2) then
            FDataInvIdx[y - 1]^[x] := pix
          else
            HardSet(x, y - 1);
          p1 := p2;
          p0 := pix;
          x := x + 2;
        end;
        y := y + 2;
      end;

    end
    else
      if FOptimise = op9in1 then
      begin
        for x := 0 to fXres - 1 do
          HardSet(x, 0);
        for x := 0 to fXres - 1 do
          HardSet(x, fYres - 1);
        for y := 0 to fYres - 1 do
          HardSet(fXres - 1, y);
        //  for y:=1 to fYres-1 do HardSet(fXres-2,y);

        y := 3;
        while y < fYres do
        begin
          x := 3;
          p1 := HardSet(x - 3, y - 3);
          //    p2:=HardSet(x  ,y-3);
          p3 := HardSet(x - 3, y);
          while x < fXres do
          begin
            p2 := SoftTest(x, y - 3);
            p4 := HardSet(x, y);
            if ((p1 = p2) and (p3 = p4) and (p2 = p4)) then
            begin
              xy := x + (fYres - (y - 3) - 1) * fXres;
              pixa^[xy - 2] := p4;
              pixa^[xy - 1] := p4;
              xy := xy - w; //xy:=x+(fYres-(y-2)-1)*fXres;
              pixa^[xy - 3] := p4;
              pixa^[xy - 2] := p4;
              pixa^[xy - 1] := p4;
              xy := xy - w; //xy:=x+(fYres-(y-1)-1)*fXres;
              pixa^[xy - 3] := p4;
              pixa^[xy - 2] := p4;
              pixa^[xy - 1] := p4;
            end
            else
            begin
              HardSet(x - 2, y - 3);
              HardSet(x - 1, y - 3);
              HardSet(x - 3, y - 2);
              HardSet(x - 2, y - 2);
              HardSet(x - 1, y - 2);
              HardSet(x - 3, y - 1);
              HardSet(x - 2, y - 1);
              HardSet(x - 1, y - 1);
            end;
            p1 := p2;
            p3 := p4;

            x := x + 3;
          end;
          y := y + 3;
        end;

      end
      else

        if FOptimise = op16in1 then
        begin

          y := 4;
          while (y <> FHeight + 3) do
          begin
            if y >= FHeight then
              y := FHeight - 1;
            repeat
              x := 4;
              p1 := HardSet(x - 4, y - 4);
              //        HardSet(x  ,y-4); //p2
              p3 := HardSet(x - 4, y);
              while (x <> fWidth + 3) do
              begin
                if x >= FWidth then
                  x := FWidth - 1;
                repeat
                  p2 := SoftTest(x, y - 4);
                  p4 := HardSet(x, y);
                  //p4.r:=255;
                  if ((p1 = p2) and (p3 = p4) and (p2 = p4)) then
                  begin
                    xy := x + (h - (y - 4) - 1) * w;
                    pixa^[xy - 3] := p4;
                    pixa^[xy - 2] := p4;
                    pixa^[xy - 1] := p4;
                    xy := xy - w;
                    pixa^[xy - 4] := p4;
                    pixa^[xy - 3] := p4;
                    pixa^[xy - 2] := p4;
                    pixa^[xy - 1] := p4;
                    xy := xy - w;
                    pixa^[xy - 4] := p4;
                    pixa^[xy - 3] := p4;
                    pixa^[xy - 2] := p4;
                    pixa^[xy - 1] := p4;
                    xy := xy - w;
                    pixa^[xy - 4] := p4;
                    pixa^[xy - 3] := p4;
                    pixa^[xy - 2] := p4;
                    pixa^[xy - 1] := p4;
                  end
                  else
                  begin
                    //--------------------------------------------
                    pM := HardSet(x - 2, y - 2);
                    pL := HardSet(x - 4, y - 2);
                    pT := HardSet(x - 2, y - 4);

                    xy := x + (h - (y - 4) - 1) * w;
                    if (p1 = pT) then
                      pixa^[xy - 3] := pT
                    else
                      HardSet(x - 3, y - 4);
                    if (p2 = pT) then
                      pixa^[xy - 1] := pT
                    else
                      HardSet(x - 1, y - 4);
                    xy := xy - w; //down
                    if (pL = p1) then
                      pixa^[xy - 4] := pL
                    else
                      HardSet(x - 4, y - 3);
                    if (p1 = pM) then
                      pixa^[xy - 3] := pM
                    else
                      HardSet(x - 3, y - 3);
                    if (p2 = pM) then
                      pixa^[xy - 1] := pM
                    else
                      HardSet(x - 1, y - 3); //p2m
                    if (pT = pM) then
                      pixa^[xy - 2] := pM
                    else
                      HardSet(x - 2, y - 3);
                    xy := xy - w; //down
                    if (pL = pM) then
                      pixa^[xy - 3] := pM
                    else
                      HardSet(x - 3, y - 2);
                    xy := xy - w; //down
                    if (p3 = pL) then
                      pixa^[xy - 4] := pL
                    else
                      HardSet(x - 4, y - 1);
                    if (p3 = pM) then
                      pixa^[xy - 3] := pM
                    else
                      HardSet(x - 3, y - 1); //p3m
                    if (p4 = pM) then
                    begin
                      pixa^[xy - 1] := pM;
                      if (pM = p2) then
                        pixa^[xy + w - 1] := pM
                      else
                        HardSet(x - 1, y - 2);
                      if (pM = p3) then
                        pixa^[xy - 2] := pM
                      else
                        HardSet(x - 2, y - 1);
                    end
                    else
                    begin
                      HardSet(x - 1, y - 1); //p4m
                      HardSet(x - 1, y - 2);
                      HardSet(x - 2, y - 1);
                    end;
                  end;
                  p1 := p2;
                  p3 := p4;
                  x := x + 4;
                until x >= FWidth;
              end; //while
              y := y + 4;
            until y > (FHeight - 2);
          end; //while
          for x := 0 to FWidth - 1 do
            FDataIdx[0][x] := FDataIdx[1][x];
          for y := 0 to FHeight - 1 do
            FDataIdx[y][FWidth - 1] := FDataIdx[y][FWidth - 2];
        end;
end;

function TGLZShadows.HardSet(const x, y: integer): Byte;

var
  pix: Byte;
  coord: TAffineVector;

  ipixX, ipixY: integer;
  pixX, pixY: single;
  pixZ: single;
  IsInFrust: Boolean;
  ilum: Integer;
  shad: single;
  Tol: Single;

  modx, mody: single;

  d2, d4, d5, d6, d8: single;
  shad2, shad4, shad5, shad6, shad8: single;

  function ComputeIlum: Integer;
  begin
    //---Lighting---
    if FDepthFade = True then
    begin
      Result := Round(SCol.a * (pixZ * 10 - 9));
      if Result < 0 then
        Result := 0;
      //if ilum>255 then ilum:=255;
      if Result > SCol.a then
        Result := SCol.a;
    end
    else
      Result := 0;
  end;

begin
  //---test pixel for shadow---
  if ViewerZBuf.GetPixelzDepth(x, y) < 1 then
  begin
    coord := ViewerZBuf.PixelToWorld(x, y);
    IsInFrust := CasterZBuf.WorldToPixelZ(coord, pixX, pixY, pixZ);
    //if caster.Camera.CameraStyle=csOrthogonal  then IsInFrust:=CasterZBuf.OrthWorldToPixelZ(coord,pixX,pixY,pixZ);
    //--- Tolerance scaling - reduces shadow-creeping at long-range and self-shadowing at short-range ---
    tol := FTolerance * (1.0 - pixZ);
    //---  ilum=light  ------  SCol.a=shade  ------
    if not isInFrust then
    begin
      if FFrustShadow then
        pix := SCol.a //dark  outside frustrum
      else
        pix := ComputeIlum; //light outside frustrum
    end
    else
    begin
      ipixX := Trunc(pixX);
      ipixY := Trunc(pixY);
      if (FSoft = True) and (ipixY > 0) then
      begin //---soft shadows---
        modx := Frac(pixX);
        //extract the fraction part only - used to interpolate soft shadow edges
        mody := Frac(pixY);

        if ipixX > 0 then
          d4 := CasterZBuf.DataIdx[ipixY]^[ipixX - 1]
        else
          d4 := CasterZBuf.DataIdx[ipixY]^[0];

        d5 := CasterZBuf.DataIdx[ipixY]^[ipixX];
        d6 := CasterZBuf.DataIdx[ipixY]^[ipixX + 1];
        d8 := CasterZBuf.DataIdx[ipixY + 1]^[ipixX];
        //             if ipixY<1 then d2:=d5 else
        d2 := CasterZBuf.DataIdx[ipixY - 1]^[ipixX];
        ilum := ComputeIlum;

        if ((pixZ - d2) > Tol) then
          Shad2 := SCol.a
        else
          Shad2 := ilum;
        if ((pixZ - d4) > Tol) then
          Shad4 := SCol.a
        else
          Shad4 := ilum;
        if ((pixZ - d5) > Tol) then
          Shad5 := SCol.a
        else
          Shad5 := ilum;
        if ((pixZ - d6) > Tol) then
          Shad6 := SCol.a
        else
          Shad6 := ilum;
        if ((pixZ - d8) > Tol) then
          Shad8 := SCol.a
        else
          Shad8 := ilum;
        shad := shad2 + (shad8 - shad2) * mody +
          shad4 + (shad6 - shad4) * modx + shad5;
        pix := Round(Shad / 3);

      end
      else
      begin //---hard shadows---
        if pixZ - Tol > CasterZBuf.DataIdx[ipixY]^[ipixX] then
          pix := SCol.a //dark
        else
          pix := ComputeIlum; //light
      end;
    end;
  end
  else
  begin // if z=1 ... i.e. nothing was drawn at this pixel (sky)
    if FSkyShadow then
      pix := SCol.a // dark
    else
      pix := 0; //ComputeIlum;            // light
  end;
  FDataInvIdx[y]^[x] := pix; //Write pixel
  result := pix;
end;

function TGLZShadows.SoftTest(const x, y: integer): Byte;
begin
  result := FDataInvIdx[y]^[x];
end;

function TGLZShadows.GetViewer: TGLSceneViewer;
begin
  result := FViewer;
end;

procedure TGLZShadows.SetViewer(const val: TGLSceneViewer);
begin
  FViewer := Val;
  Width := FViewer.Width;
  Height := FViewer.Height;
end;

function TGLZShadows.GetCaster: TGLMemoryViewer;
begin
  result := FCaster;
end;

procedure TGLZShadows.SetCaster(const val: TGLMemoryViewer);
begin
  FCaster := Val;
end;

function TGLZShadows.CastShadow: Boolean;
begin
  if Caster <> nil then
  begin
    if not assigned(CasterZBuf) then
    begin
      CasterZBuf := TGLZBuffer.Create;
      CasterZBuf.LinkToViewer(FCaster);
    end;

    if FCaster.Camera.CameraStyle = csOrthogonal then
    begin
      if assigned(FCaster.Camera.TargetObject) then
      begin
        FCaster.Camera.Position.x := FCaster.Camera.TargetObject.Position.x;
        FCaster.Camera.Position.z := FCaster.Camera.TargetObject.Position.z;
      end;
      with FCaster.Camera.direction do
      begin
        x := 0;
        y := -1;
        z := 0;
      end;
    end;

    try
      FCaster.Render;
    except
      Caster := nil; // prevents further attempts
      raise;
    end;

    CasterZBuf.Refresh;
    Result := False;
  end
  else
    Result := True;
end;

procedure TGLZShadows.SetWidth(const val: integer);
begin
  FWidth := val;
  SetXRes(val);
end;

procedure TGLZShadows.SetHeight(const val: integer);
begin
  FHeight := val;
  SetYRes(val);
end;

procedure TGLZShadows.SetXRes(const val: integer);
var
  i: integer;
begin
  i := 2;
  while val > i do
    i := i * 2; //
  FXRes := i; //calculate the closest power of 2, smaller than val
  FTexturePrepared := False;
  PrepareAlphaMemory;
end;

procedure TGLZShadows.SetYRes(const val: integer);
var
  i: integer;
begin
  i := 2;
  while val > i do
    i := i * 2; //
  FYRes := i; //calculate the closest power of 2, larger than val
  FTexturePrepared := False;
  PrepareAlphaMemory;
end;

procedure TGLZShadows.SetSoft(const val: boolean);
begin
  FSoft := val;
  NotifyChange(Self);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

     // class registrations
  RegisterClasses([TGLZShadows]);

end.

