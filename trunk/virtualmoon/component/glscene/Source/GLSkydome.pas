//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSkydome<p>

   Skydome object<p>

 <b>History : </b><font size=-1><ul>
      <li>17/02/13 - Yar - Added SetSunAtTime method (thanks to Dimitriy) 
      <li>10/11/12 - PW - Added CPP compatibility: changed vector arrays to records
      <li>24/03/11 - Yar - Added esoDepthTest to TEarthSkydomeOption
                           (Drawing sky dome latest with depth test reduce pixels overdraw)
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>22/04/10 - Yar - Fixes after GLState revision
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>10/10/08 - DanB - changed Skydome buildlists to use rci instead
                            of Scene.CurrentGLCamera
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>30/03/07 - DaStr - Moved all UNSAFE_TYPE, UNSAFE_CODE checks to GLSCene.inc
      <li>25/03/07 - DaStr - Fixed compiler directives for Delphi5 compatibility
      <li>22/03/07 - DaStr - Removed "unsafe type/unsafe code" warnings
      <li>19/12/06 - DaStr - TSkyDomeStars.AddRandomStars() overloaded
      <li>29/06/06 - PvD - Fixed small bug to properly deal with polygon fill
      <li>20/01/05 - Mathx - Added the ExtendedOptions of the EarthSkyDome
      <li>02/08/04 - LR, YHC - BCB corrections: use record instead array
      <li>09/01/04 - EG - Now based on TGLCameraInvariantObject
      <li>04/08/03 - SG - Fixed small bug with random star creation
      <li>17/06/03 - EG - Fixed PolygonMode (Carlos Ferreira)
      <li>26/02/02 - EG - Enhanced star support (generation and twinkle),
                          Skydome now 'exports' its coordinate system to children
      <li>21/01/02 - EG - Skydome position now properly ignored
      <li>23/09/01 - EG - Fixed and improved TGLEarthSkyDome
      <li>26/08/01 - EG - Added SkyDomeStars
      <li>12/08/01 - EG - DepthMask no set to False during rendering
      <li>18/07/01 - EG - VisibilityCulling compatibility changes
      <li>12/03/01 - EG - Reversed polar caps orientation
      <li>28/01/01 - EG - Fixed TSkyDomeBand rendering (vertex coordinates)
      <li>18/01/01 - EG - First working version of TGLEarthSkyDome
      <li>14/01/01 - EG - Creation
 </ul></font>
}
unit GLSkydome;

interface

{$I GLScene.inc}

uses
  Classes,
{$IFDEF GLS_DELPHI_XE2_UP}
  VCL.Graphics,
{$ELSE}
  Graphics,
{$ENDIF}

  // GLSCene
  GLScene,
  GLVectorGeometry,
  GLGraphics,
  GLCrossPlatform,
  GLVectorTypes,
  GLColor,
  GLRenderContextInfo;

type

  // TSkyDomeBand
  //
  TSkyDomeBand = class(TCollectionItem)
  private
    { Private Declarations }
    FStartAngle: Single;
    FStopAngle: Single;
    FStartColor: TGLColor;
    FStopColor: TGLColor;
    FSlices: Integer;
    FStacks: Integer;

  protected
    { Protected Declarations }
    function GetDisplayName: string; override;
    procedure SetStartAngle(const val: Single);
    procedure SetStartColor(const val: TGLColor);
    procedure SetStopAngle(const val: Single);
    procedure SetStopColor(const val: TGLColor);
    procedure SetSlices(const val: Integer);
    procedure SetStacks(const val: Integer);
    procedure OnColorChange(sender: TObject);

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TRenderContextInfo);

  published
    { Published Declarations }
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property StartColor: TGLColor read FStartColor write SetStartColor;
    property StopAngle: Single read FStopAngle write SetStopAngle;
    property StopColor: TGLColor read FStopColor write SetStopColor;
    property Slices: Integer read FSlices write SetSlices default 12;
    property Stacks: Integer read FStacks write SetStacks default 1;
  end;

  // TSkyDomeBands
  //
  TSkyDomeBands = class(TCollection)
  protected
    { Protected Declarations }
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TSkyDomeBand);
    function GetItems(index: Integer): TSkyDomeBand;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);
    function Add: TSkyDomeBand;
    function FindItemID(ID: Integer): TSkyDomeBand;
    property Items[index: Integer]: TSkyDomeBand read GetItems write SetItems;
    default;

    procedure NotifyChange;
    procedure BuildList(var rci: TRenderContextInfo);
  end;

  // TSkyDomeStar
  //
  TSkyDomeStar = class(TCollectionItem)
  private
    { Private Declarations }
    FRA, FDec: Single;
    FMagnitude: Single;
    FColor: TColor;
    FCacheCoord: TAffineVector; // cached cartesian coordinates

  protected
    { Protected Declarations }
    function GetDisplayName: string; override;

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

  published
    { Published Declarations }
      {: Right Ascension, in degrees. }
    property RA: Single read FRA write FRA;
    {: Declination, in degrees. }
    property Dec: Single read FDec write FDec;
    {: Absolute magnitude. }
    property Magnitude: Single read FMagnitude write FMagnitude;
    {: Color of the star. }
    property Color: TColor read FColor write FColor;

  end;

  // TSkyDomeStars
  //
  TSkyDomeStars = class(TCollection)
  protected
    { Protected Declarations }
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TSkyDomeStar);
    function GetItems(index: Integer): TSkyDomeStar;

    procedure PrecomputeCartesianCoordinates;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);

    function Add: TSkyDomeStar;
    function FindItemID(ID: Integer): TSkyDomeStar;
    property Items[index: Integer]: TSkyDomeStar read GetItems write SetItems;
    default;

    procedure BuildList(var rci: TRenderContextInfo; twinkle: Boolean);

    {: Adds nb random stars of the given color.<p>
       Stars are homogenously scattered on the complete sphere, not only the band defined or visible dome. }
    procedure AddRandomStars(const nb: Integer; const color: TColor; const limitToTopDome: Boolean = False); overload;
    procedure AddRandomStars(const nb: Integer; const ColorMin, ColorMax:TVector3b; const Magnitude_min, Magnitude_max: Single;const limitToTopDome: Boolean = False); overload;

    {: Load a 'stars' file, which is made of TGLStarRecord.<p>
       Not that '.stars' files should already be sorted by magnitude and color. }
    procedure LoadStarsFile(const starsFileName: string);
  end;

  // TSkyDomeOption
  //
  TSkyDomeOption = (sdoTwinkle);
  TSkyDomeOptions = set of TSkyDomeOption;

  // TGLSkyDome
  //
    {: Renders a sky dome always centered on the camera.<p>
       If you use this object make sure it is rendered *first*, as it ignores
       depth buffering and overwrites everything. All children of a skydome
       are rendered in the skydome's coordinate system.<p>
       The skydome is described by "bands", each "band" is an horizontal cut
       of a sphere, and you can have as many bands as you wish.<p>
       Estimated CPU cost (K7-500, GeForce SDR, default bands):<ul>
       <li>800x600 fullscreen filled: 4.5 ms (220 FPS, worst case)
       <li>Geometry cost (0% fill): 0.7 ms (1300 FPS, best case)
       </ul> }
  TGLSkyDome = class(TGLCameraInvariantObject)
  private
    { Private Declarations }
    FOptions: TSkyDomeOptions;
    FBands: TSkyDomeBands;
    FStars: TSkyDomeStars;

  protected
    { Protected Declarations }
    procedure SetBands(const val: TSkyDomeBands);
    procedure SetStars(const val: TSkyDomeStars);
    procedure SetOptions(const val: TSkyDomeOptions);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TRenderContextInfo); override;

  published
    { Published Declarations }
    property Bands: TSkyDomeBands read FBands write SetBands;
    property Stars: TSkyDomeStars read FStars write SetStars;
    property Options: TSkyDomeOptions read FOptions write SetOptions default [];
  end;

  TEarthSkydomeOption = (esoFadeStarsWithSun, esoRotateOnTwelveHours, esoDepthTest);
  TEarthSkydomeOptions = set of TEarthSkydomeOption;

  // TGLEarthSkyDome
  //
  {: Render a skydome like what can be seen on earth.<p>
     Color is based on sun position and turbidity, to "mimic" atmospheric
     Rayleigh and Mie scatterings. The colors can be adjusted to render
     weird/extra-terrestrial atmospheres too.<p>
     The default slices/stacks values make for an average quality rendering,
     for a very clean rendering, use 64/64 (more is overkill in most cases).
     The complexity is quite high though, making a T&L 3D board a necessity
     for using TGLEarthSkyDome. }
  TGLEarthSkyDome = class(TGLSkyDome)
  private
    { Private Declarations }
    FSunElevation: Single;
    FTurbidity: Single;
    FCurSunColor, FCurSkyColor, FCurHazeColor: TColorVector;
    FCurHazeTurbid, FCurSunSkyTurbid: Single;
    FSunZenithColor: TGLColor;
    FSunDawnColor: TGLColor;
    FHazeColor: TGLColor;
    FSkyColor: TGLColor;
    FNightColor: TGLColor;
    FDeepColor: TGLColor;
    FSlices, FStacks: Integer;
    FExtendedOptions: TEarthSkydomeOptions;
    FMorning: boolean;
  protected
    { Protected Declarations }
    procedure Loaded; override;

    procedure SetSunElevation(const val: Single);
    procedure SetTurbidity(const val: Single);
    procedure SetSunZenithColor(const val: TGLColor);
    procedure SetSunDawnColor(const val: TGLColor);
    procedure SetHazeColor(const val: TGLColor);
    procedure SetSkyColor(const val: TGLColor);
    procedure SetNightColor(const val: TGLColor);
    procedure SetDeepColor(const val: TGLColor);
    procedure SetSlices(const val: Integer);
    procedure SetStacks(const val: Integer);

    procedure OnColorChanged(Sender: TObject);
    procedure PreCalculate;
    procedure RenderDome;
    function CalculateColor(const theta, cosGamma: Single): TColorVector;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TRenderContextInfo); override;

    procedure SetSunAtTime(HH, MM: Single);

  published
    { Published Declarations }
      {: Elevation of the sun, measured in degrees. }
    property SunElevation: Single read FSunElevation write SetSunElevation;
    {: Expresses the purity of air.<p> Value range is from 1 (pure athmosphere) to 120 (very nebulous) }
    property Turbidity: Single read FTurbidity write SetTurbidity;

    property SunZenithColor: TGLColor read FSunZenithColor write SetSunZenithColor;
    property SunDawnColor: TGLColor read FSunDawnColor write SetSunDawnColor;
    property HazeColor: TGLColor read FHazeColor write SetHazeColor;
    property SkyColor: TGLColor read FSkyColor write SetSkyColor;
    property NightColor: TGLColor read FNightColor write SetNightColor;
    property DeepColor: TGLColor read FDeepColor write SetDeepColor;
    property ExtendedOptions: TEarthSkydomeOptions read FExtendedOptions write FExtendedOptions;
    property Slices: Integer read FSlices write SetSlices default 24;
    property Stacks: Integer read FStacks write SetStacks default 48;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  SysUtils,
  OpenGLTokens,
  GLContext,
  GLStarRecord,
  GLState;

// ------------------
// ------------------ TSkyDomeBand ------------------
// ------------------

// Create
//

constructor TSkyDomeBand.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FStartColor := TGLColor.Create(Self);
  FStartColor.Initialize(clrBlue);
  FStartColor.OnNotifyChange := OnColorChange;
  FStopColor := TGLColor.Create(Self);
  FStopColor.Initialize(clrBlue);
  FStopColor.OnNotifyChange := OnColorChange;
  FSlices := 12;
  FStacks := 1;
end;

// Destroy
//

destructor TSkyDomeBand.Destroy;
begin
  FStartColor.Free;
  FStopColor.Free;
  inherited Destroy;
end;

// Assign
//

procedure TSkyDomeBand.Assign(Source: TPersistent);
begin
  if Source is TSkyDomeBand then
  begin
    FStartAngle := TSkyDomeBand(Source).FStartAngle;
    FStopAngle := TSkyDomeBand(Source).FStopAngle;
    FStartColor.Assign(TSkyDomeBand(Source).FStartColor);
    FStopColor.Assign(TSkyDomeBand(Source).FStopColor);
    FSlices := TSkyDomeBand(Source).FSlices;
    FStacks := TSkyDomeBand(Source).FStacks;
  end;
  inherited Destroy;
end;

// GetDisplayName
//

function TSkyDomeBand.GetDisplayName: string;
begin
  Result := Format('%d: %.1f° - %.1f°', [Index, StartAngle, StopAngle]);
end;

// SetStartAngle
//

procedure TSkyDomeBand.SetStartAngle(const val: Single);
begin
  FStartAngle := ClampValue(val, -90, 90);
  if FStartAngle > FStopAngle then FStopAngle := FStartAngle;
  TSkyDomeBands(Collection).NotifyChange;
end;

// SetStartColor
//

procedure TSkyDomeBand.SetStartColor(const val: TGLColor);
begin
  FStartColor.Assign(val);
end;

// SetStopAngle
//

procedure TSkyDomeBand.SetStopAngle(const val: Single);
begin
  FStopAngle := ClampValue(val, -90, 90);
  if FStopAngle < FStartAngle then
    FStartAngle := FStopAngle;
  TSkyDomeBands(Collection).NotifyChange;
end;

// SetStopColor
//

procedure TSkyDomeBand.SetStopColor(const val: TGLColor);
begin
  FStopColor.Assign(val);
end;

// SetSlices
//

procedure TSkyDomeBand.SetSlices(const val: Integer);
begin
  if val < 3 then
    FSlices := 3
  else
    FSlices := val;
  TSkyDomeBands(Collection).NotifyChange;
end;

// SetStacks
//

procedure TSkyDomeBand.SetStacks(const val: Integer);
begin
  if val < 1 then
    FStacks := 1
  else
    FStacks := val;
  TSkyDomeBands(Collection).NotifyChange;
end;

// OnColorChange
//

procedure TSkyDomeBand.OnColorChange(sender: TObject);
begin
  TSkyDomeBands(Collection).NotifyChange;
end;

// BuildList
//

procedure TSkyDomeBand.BuildList(var rci: TRenderContextInfo);

// coordinates system note: X is forward, Y is left and Z is up
// always rendered as sphere of radius 1

  procedure RenderBand(start, stop: Single; const colStart, colStop:
    TColorVector);
  var
    i: Integer;
    f, r, r2: Single;
    vertex1, vertex2: TVector;
  begin
    vertex1.V[3] := 1;
    if start = -90 then
    begin
      // triangle fan with south pole
      GL.Begin_(GL_TRIANGLE_FAN);
      GL.Color4fv(@colStart);
      GL.Vertex3f(0, 0, -1);
      f := 2 * PI / Slices;
      SinCos(GLVectorGeometry.DegToRad(stop), vertex1.V[2], r);
      GL.Color4fv(@colStop);
      for i := 0 to Slices do
      begin
        SinCos(i * f, r, vertex1.V[1], vertex1.V[0]);
        GL.Vertex4fv(@vertex1);
      end;
      GL.End_;
    end
    else if stop = 90 then
    begin
      // triangle fan with north pole
      GL.Begin_(GL_TRIANGLE_FAN);
      GL.Color4fv(@colStop);
      GL.Vertex3fv(@ZHmgPoint);
      f := 2 * PI / Slices;
      SinCos(GLVectorGeometry.DegToRad(start), vertex1.V[2], r);
      GL.Color4fv(@colStart);
      for i := Slices downto 0 do
      begin
        SinCos(i * f, r, vertex1.V[1], vertex1.V[0]);
        GL.Vertex4fv(@vertex1);
      end;
      GL.End_;
    end
    else
    begin
      vertex2.V[3] := 1;
      // triangle strip
      GL.Begin_(GL_TRIANGLE_STRIP);
      f := 2 * PI / Slices;
      SinCos(GLVectorGeometry.DegToRad(start), vertex1.V[2], r);
      SinCos(GLVectorGeometry.DegToRad(stop), vertex2.V[2], r2);
      for i := 0 to Slices do
      begin
        SinCos(i * f, r, vertex1.V[1], vertex1.V[0]);
        GL.Color4fv(@colStart);
        GL.Vertex4fv(@vertex1);
        SinCos(i * f, r2, vertex2.V[1], vertex2.V[0]);
        GL.Color4fv(@colStop);
        GL.Vertex4fv(@vertex2);
      end;
      GL.End_;
    end;
  end;

var
  n: Integer;
  t, t2: Single;
begin
  if StartAngle = StopAngle then
    Exit;
  for n := 0 to Stacks - 1 do
  begin
    t := n / Stacks;
    t2 := (n + 1) / Stacks;
    RenderBand(Lerp(StartAngle, StopAngle, t),
      Lerp(StartAngle, StopAngle, t2),
      VectorLerp(StartColor.Color, StopColor.Color, t),
      VectorLerp(StartColor.Color, StopColor.Color, t2));
  end;
end;

// ------------------
// ------------------ TSkyDomeBands ------------------
// ------------------

constructor TSkyDomeBands.Create(AOwner: TComponent);
begin
  Owner := AOwner;
  inherited Create(TSkyDomeBand);
end;

function TSkyDomeBands.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TSkyDomeBands.SetItems(index: Integer; const val: TSkyDomeBand);
begin
  inherited Items[index] := val;
end;

function TSkyDomeBands.GetItems(index: Integer): TSkyDomeBand;
begin
  Result := TSkyDomeBand(inherited Items[index]);
end;

function TSkyDomeBands.Add: TSkyDomeBand;
begin
  Result := (inherited Add) as TSkyDomeBand;
end;

function TSkyDomeBands.FindItemID(ID: Integer): TSkyDomeBand;
begin
  Result := (inherited FindItemID(ID)) as TSkyDomeBand;
end;

procedure TSkyDomeBands.NotifyChange;
begin
  if Assigned(owner) and (owner is TGLBaseSceneObject) then TGLBaseSceneObject(owner).StructureChanged;
end;

// BuildList
//

procedure TSkyDomeBands.BuildList(var rci: TRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do Items[i].BuildList(rci);
end;

// ------------------
// ------------------ TSkyDomeStar ------------------
// ------------------

// Create
//

constructor TSkyDomeStar.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

// Destroy
//

destructor TSkyDomeStar.Destroy;
begin
  inherited Destroy;
end;

// Assign
//

procedure TSkyDomeStar.Assign(Source: TPersistent);
begin
  if Source is TSkyDomeStar then
  begin
    FRA := TSkyDomeStar(Source).FRA;
    FDec := TSkyDomeStar(Source).FDec;
    FMagnitude := TSkyDomeStar(Source).FMagnitude;
    FColor := TSkyDomeStar(Source).FColor;
    SetVector(FCacheCoord, TSkyDomeStar(Source).FCacheCoord);
  end;
  inherited Destroy;
end;

// GetDisplayName
//

function TSkyDomeStar.GetDisplayName: string;
begin
  Result := Format('RA: %5.1f / Dec: %5.1f', [RA, Dec]);
end;

// ------------------
// ------------------ TSkyDomeStars ------------------
// ------------------

// Create
//

constructor TSkyDomeStars.Create(AOwner: TComponent);
begin
  Owner := AOwner;
  inherited Create(TSkyDomeStar);
end;

// GetOwner
//

function TSkyDomeStars.GetOwner: TPersistent;
begin
  Result := Owner;
end;

// SetItems
//

procedure TSkyDomeStars.SetItems(index: Integer; const val: TSkyDomeStar);
begin
  inherited Items[index] := val;
end;

// GetItems
//

function TSkyDomeStars.GetItems(index: Integer): TSkyDomeStar;
begin
  Result := TSkyDomeStar(inherited Items[index]);
end;

// Add
//

function TSkyDomeStars.Add: TSkyDomeStar;
begin
  Result := (inherited Add) as TSkyDomeStar;
end;

// FindItemID
//

function TSkyDomeStars.FindItemID(ID: Integer): TSkyDomeStar;
begin
  Result := (inherited FindItemID(ID)) as TSkyDomeStar;
end;

// PrecomputeCartesianCoordinates
//

procedure TSkyDomeStars.PrecomputeCartesianCoordinates;
var
  i: Integer;
  star: TSkyDomeStar;
  raC, raS, decC, decS: Single;
begin
  // to be enhanced...
  for i := 0 to Count - 1 do
  begin
    star := Items[i];
    SinCos(star.DEC * cPIdiv180, decS, decC);
    SinCos(star.RA * cPIdiv180, decC, raS, raC);
    star.FCacheCoord.V[0] := raC;
    star.FCacheCoord.V[1] := raS;
    star.FCacheCoord.V[2] := decS;
  end;
end;

// BuildList
//

procedure TSkyDomeStars.BuildList(var rci: TRenderContextInfo; twinkle:
  Boolean);
var
  i, n: Integer;
  star: TSkyDomeStar;
  lastColor: TColor;
  lastPointSize10, pointSize10: Integer;
  color, twinkleColor: TColorVector;

  procedure DoTwinkle;
  begin
    if (n and 63) = 0 then
    begin
      twinkleColor := VectorScale(color, Random * 0.6 + 0.4);
      GL.Color3fv(@twinkleColor.V[0]);
      n := 0;
    end
    else
      Inc(n);
  end;

begin
  if Count = 0 then
    Exit;
  PrecomputeCartesianCoordinates;
  lastColor := -1;
  n := 0;
  lastPointSize10 := -1;

  rci.GLStates.Enable(stPointSmooth);
  rci.GLStates.Enable(stAlphaTest);
  rci.GLStates.SetGLAlphaFunction(cfNotEqual, 0.0);
  rci.GLStates.Enable(stBlend);
  rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOne);

  GL.Begin_(GL_POINTS);
  for i := 0 to Count - 1 do
  begin
    star := Items[i];
    pointSize10 := Round((4.5 - star.Magnitude) * 10);
    if pointSize10 <> lastPointSize10 then
    begin
      if pointSize10 > 15 then
      begin
        GL.End_;
        lastPointSize10 := pointSize10;
        rci.GLStates.PointSize := pointSize10 * 0.1;
        GL.Begin_(GL_POINTS);
      end
      else if lastPointSize10 <> 15 then
      begin
        GL.End_;
        lastPointSize10 := 15;
        rci.GLStates.PointSize := 1.5;
        GL.Begin_(GL_POINTS);
      end;
    end;
    if lastColor <> star.FColor then
    begin
      color := ConvertWinColor(star.FColor);
      if twinkle then
      begin
        n := 0;
        DoTwinkle;
      end
      else
        GL.Color3fv(@color.V[0]);
      lastColor := star.FColor;
    end
    else if twinkle then
      DoTwinkle;
    GL.Vertex3fv(@star.FCacheCoord.V[0]);
  end;
  GL.End_;

  // restore default GLScene AlphaFunc
  rci.GLStates.SetGLAlphaFunction(cfGreater, 0);
end;

// AddRandomStars
//

procedure TSkyDomeStars.AddRandomStars(const nb: Integer; const color: TColor;
  const limitToTopDome: Boolean = False);
var
  i: Integer;
  coord: TAffineVector;
  star: TSkyDomeStar;
begin
  for i := 1 to nb do
  begin
    star := Add;
    // pick a point in the half-cube
    if limitToTopDome then
      coord.V[2] := Random
    else
      coord.V[2] := Random * 2 - 1;
    // calculate RA and Dec
    star.Dec := ArcSin(coord.V[2]) * c180divPI;
    star.Ra := Random * 360 - 180;
    // pick a color
    star.Color := color;
    // pick a magnitude
    star.Magnitude := 3;
  end;
end;

// AddRandomStars
//

procedure TSkyDomeStars.AddRandomStars(const nb: Integer; const ColorMin,
  ColorMax: TVector3b;
  const Magnitude_min, Magnitude_max: Single;
  const limitToTopDome: Boolean = False);

  function RandomTT(Min, Max: Byte): Byte;
  begin
    Result := Min + Random(Max - Min);
  end;

var
  i: Integer;
  coord: TAffineVector;
  star: TSkyDomeStar;

begin
  for i := 1 to nb do
  begin
    star := Add;
    // pick a point in the half-cube
    if limitToTopDome then
      coord.V[2] := Random
    else
      coord.V[2] := Random * 2 - 1;
    // calculate RA and Dec
    star.Dec := ArcSin(coord.V[2]) * c180divPI;
    star.Ra := Random * 360 - 180;
    // pick a color
    star.Color := RGB(RandomTT(ColorMin.V[0], ColorMax.V[0]),
      RandomTT(ColorMin.V[1], ColorMax.V[1]),
      RandomTT(ColorMin.V[2], ColorMax.V[2]));
    // pick a magnitude
    star.Magnitude := Magnitude_min + Random * (Magnitude_max - Magnitude_min);
  end;
end;

// LoadStarsFile
//

procedure TSkyDomeStars.LoadStarsFile(const starsFileName: string);
var
  fs: TFileStream;
  sr: TGLStarRecord;
  colorVector: TColorVector;
begin
  fs := TFileStream.Create(starsFileName, fmOpenRead + fmShareDenyWrite);
  try
    while fs.Position < fs.Size do
    begin
      fs.Read(sr, SizeOf(sr));
      with Add do
      begin
        RA := sr.RA * 0.01;
        DEC := sr.DEC * 0.01;
        colorVector := StarRecordColor(sr, 3);
        Magnitude := sr.VMagnitude * 0.1;
        if sr.VMagnitude > 35 then
          Color := ConvertColorVector(colorVector, colorVector.V[3])
        else
          Color := ConvertColorVector(colorVector);
      end;
    end;
  finally
    fs.Free;
  end;
end;

// ------------------
// ------------------ TGLSkyDome ------------------
// ------------------

// CreateOwned
//

constructor TGLSkyDome.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CamInvarianceMode := cimPosition;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FBands := TSkyDomeBands.Create(Self);
  with FBands.Add do
  begin
    StartAngle := 0;
    StartColor.Color := clrWhite;
    StopAngle := 15;
    StopColor.Color := clrBlue;
  end;
  with FBands.Add do
  begin
    StartAngle := 15;
    StartColor.Color := clrBlue;
    StopAngle := 90;
    Stacks := 4;
    StopColor.Color := clrNavy;
  end;
  FStars := TSkyDomeStars.Create(Self);
end;

// Destroy
//

destructor TGLSkyDome.Destroy;
begin
  FStars.Free;
  FBands.Free;
  inherited Destroy;
end;

// Assign
//

procedure TGLSkyDome.Assign(Source: TPersistent);
begin
  if Source is TGLSkyDome then
  begin
    FBands.Assign(TGLSkyDome(Source).FBands);
    FStars.Assign(TGLSkyDome(Source).FStars);
  end;
  inherited;
end;

// SetBands
//

procedure TGLSkyDome.SetBands(const val: TSkyDomeBands);
begin
  FBands.Assign(val);
  StructureChanged;
end;

// SetStars
//

procedure TGLSkyDome.SetStars(const val: TSkyDomeStars);
begin
  FStars.Assign(val);
  StructureChanged;
end;

// SetOptions
//

procedure TGLSkyDome.SetOptions(const val: TSkyDomeOptions);
begin
  if val <> FOptions then
  begin
    FOptions := val;
    if sdoTwinkle in FOptions then
      ObjectStyle := ObjectStyle + [osDirectDraw]
    else
    begin
      ObjectStyle := ObjectStyle - [osDirectDraw];
      DestroyHandle;
    end;
    StructureChanged;
  end;
end;

// BuildList
//

procedure TGLSkyDome.BuildList(var rci: TRenderContextInfo);
var
  f: Single;
begin
  // setup states
  with rci.GLStates do
  begin
    Disable(stLighting);
    Disable(stDepthTest);
    Disable(stFog);
    Disable(stCullFace);
    Disable(stBlend);
    DepthWriteMask := False;
    PolygonMode := pmFill;
  end;

  f := rci.rcci.farClippingDistance * 0.90;
  GL.Scalef(f, f, f);

  Bands.BuildList(rci);
  Stars.BuildList(rci, (sdoTwinkle in FOptions));
end;

// ------------------
// ------------------ TGLEarthSkyDome ------------------
// ------------------

// CreateOwned
//

constructor TGLEarthSkyDome.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMorning:=true;
  Bands.Clear;
  FSunElevation := 75;
  FTurbidity := 15;
  FSunZenithColor := TGLColor.CreateInitialized(Self, clrWhite, OnColorChanged);
  FSunDawnColor := TGLColor.CreateInitialized(Self, Vectormake(1, 0.5, 0, 0),OnColorChanged);
  FHazeColor := TGLColor.CreateInitialized(Self, VectorMake(0.9, 0.95, 1, 0),OnColorChanged);
  FSkyColor := TGLColor.CreateInitialized(Self, VectorMake(0.45, 0.6, 0.9, 0),OnColorChanged);
  FNightColor := TGLColor.CreateInitialized(Self, clrTransparent,OnColorChanged);
  FDeepColor := TGLColor.CreateInitialized(Self, VectorMake(0, 0.2, 0.4, 0));
  FStacks := 24;
  FSlices := 48;
  PreCalculate;
end;

// Destroy
//

destructor TGLEarthSkyDome.Destroy;
begin
  FSunZenithColor.Free;
  FSunDawnColor.Free;
  FHazeColor.Free;
  FSkyColor.Free;
  FNightColor.Free;
  FDeepColor.Free;
  inherited Destroy;
end;

// Assign
//

procedure TGLEarthSkyDome.Assign(Source: TPersistent);
begin
  if Source is TGLSkyDome then
  begin
    FSunElevation := TGLEarthSkyDome(Source).SunElevation;
    FTurbidity := TGLEarthSkyDome(Source).Turbidity;
    FSunZenithColor.Assign(TGLEarthSkyDome(Source).FSunZenithColor);
    FSunDawnColor.Assign(TGLEarthSkyDome(Source).FSunDawnColor);
    FHazeColor.Assign(TGLEarthSkyDome(Source).FHazeColor);
    FSkyColor.Assign(TGLEarthSkyDome(Source).FSkyColor);
    FNightColor.Assign(TGLEarthSkyDome(Source).FNightColor);
    FSlices := TGLEarthSkyDome(Source).FSlices;
    FStacks := TGLEarthSkyDome(Source).FStacks;
    PreCalculate;
  end;
  inherited;
end;

// Loaded
//

procedure TGLEarthSkyDome.Loaded;
begin
  inherited;
  PreCalculate;
end;

// SetSunElevation
//

procedure TGLEarthSkyDome.SetSunElevation(const val: Single);
var
  newVal: single;
begin
  newval := clampValue(val, -90, 90);
  if FSunElevation <> newval then
  begin
    FSunElevation := newval;
    PreCalculate;
  end;
end;

// SetTurbidity
//

procedure TGLEarthSkyDome.SetTurbidity(const val: Single);
begin
  FTurbidity := ClampValue(val, 1, 120);
  PreCalculate;
end;

// SetSunZenithColor
//

procedure TGLEarthSkyDome.SetSunZenithColor(const val: TGLColor);
begin
  FSunZenithColor.Assign(val);
  PreCalculate;
end;

// SetSunDawnColor
//

procedure TGLEarthSkyDome.SetSunDawnColor(const val: TGLColor);
begin
  FSunDawnColor.Assign(val);
  PreCalculate;
end;

// SetHazeColor
//

procedure TGLEarthSkyDome.SetHazeColor(const val: TGLColor);
begin
  FHazeColor.Assign(val);
  PreCalculate;
end;

// SetSkyColor
//

procedure TGLEarthSkyDome.SetSkyColor(const val: TGLColor);
begin
  FSkyColor.Assign(val);
  PreCalculate;
end;

// SetNightColor
//

procedure TGLEarthSkyDome.SetNightColor(const val: TGLColor);
begin
  FNightColor.Assign(val);
  PreCalculate;
end;

// SetDeepColor
//

procedure TGLEarthSkyDome.SetDeepColor(const val: TGLColor);
begin
  FDeepColor.Assign(val);
  PreCalculate;
end;

// SetSlices
//

procedure TGLEarthSkyDome.SetSlices(const val: Integer);
begin
  if val>6 then FSlices:=val else FSlices:=6;
  StructureChanged;
end;

// SetStacks
//

procedure TGLEarthSkyDome.SetStacks(const val: Integer);
begin
  if val>1 then FStacks:=val else FStacks:=1;
  StructureChanged;
end;

// BuildList
//

procedure TGLEarthSkyDome.BuildList(var rci: TRenderContextInfo);
var
  f: Single;
begin
  // setup states
  with rci.GLStates do
  begin
    CurrentProgram := 0;
    Disable(stLighting);
    if esoDepthTest in FExtendedOptions then
    begin
      Enable(stDepthTest);
      DepthFunc := cfLEqual;
    end
    else
      Disable(stDepthTest);
    Disable(stFog);
    Disable(stCullFace);
    Disable(stBlend);
    Disable(stAlphaTest);
    DepthWriteMask := False;
    PolygonMode := pmFill;
  end;

  f := rci.rcci.farClippingDistance * 0.95;
  GL.Scalef(f, f, f);

  RenderDome;
  Bands.BuildList(rci);
  Stars.BuildList(rci, (sdoTwinkle in FOptions));

  // restore
  rci.GLStates.DepthWriteMask := True;
end;

// OnColorChanged
//

procedure TGLEarthSkyDome.OnColorChanged(Sender: TObject);
begin
  PreCalculate;
end;

procedure TGLEarthSkyDome.SetSunAtTime(HH, MM: Single);
const
  cHourToElevation1: array[0..23] of Single =
  (-45, -67.5, -90, -57.5, -45, -22.5, 0, 11.25, 22.5, 33.7, 45, 56.25, 67.5,
   78.75, 90, 78.75, 67.5, 56.25, 45, 33.7, 22.5, 11.25, 0, -22.5);
  cHourToElevation2: array[0..23] of Single =
  (-0.375, -0.375, 0.375, 0.375, 0.375, 0.375, 0.1875, 0.1875, 0.1875, 0.1875,  
     0.1875, 0.1875, 0.1875, 0.1875, -0.1875, -0.1875, -0.1875, -0.1875, -0.1875,
     -0.1875, -0.1875, -0.1875, -0.375, -0.375);
var
  ts:Single;
  fts:Single;
  i:integer;
  color:TColor;
begin
  HH:=Round(HH);
  if HH<0 then HH:=0;
  if HH>23 then HH:=23;
  if MM<0 then MM:=0;
  if MM>=60 then
  begin
    MM:=0;
    HH:=HH+1;
    if HH>23 then HH:=0;
  end;
  FSunElevation := cHourToElevation1[Round(HH)] + cHourToElevation2[Round(HH)]*MM;

  ts := DegToRad(90 - FSunElevation);
  // Mix base colors
  fts := exp(-6 * (PI / 2 - ts));
  VectorLerp(SunZenithColor.Color, SunDawnColor.Color, fts, FCurSunColor);
  fts := Power(1 - cos(ts - 0.5), 2);
  VectorLerp(HazeColor.Color, NightColor.Color, fts, FCurHazeColor);
  VectorLerp(SkyColor.Color, NightColor.Color, fts, FCurSkyColor);
  // Precalculate Turbidity factors
  FCurHazeTurbid := -sqrt(121 - Turbidity) * 2;
  FCurSunSkyTurbid := -(121 - Turbidity);

  //fade stars if required
  if SunElevation>-40 then ts:=power(1-(SunElevation+40)/90,11)else ts:=1;
  color := RGB(round(ts * 255), round(ts * 255), round(ts * 255));
  if esoFadeStarsWithSun in ExtendedOptions then for i:=0 to Stars.Count-1 do stars[i].Color:=color;


  if esoRotateOnTwelveHours in ExtendedOptions then // spining around blue orb
  begin
    if (HH>=14) and (FMorning=true) then
    begin
      roll(180);
      for i:=0 to Stars.Count-1 do stars[i].RA:=Stars[i].RA+180;
      FMorning:=false;
    end;

    if (HH>=2) and (HH<14) and (FMorning=false) then
    begin
      roll(180);
      for i:=0 to Stars.Count-1 do stars[i].RA:=Stars[i].RA+180;
      FMorning:=true;
    end;
  end;
  StructureChanged;
end;






// PreCalculate
//

procedure TGLEarthSkyDome.PreCalculate;
var
  ts: Single;
  fts: Single;
  i: integer;
  color: TColor;
begin
  ts := DegToRad(90 - SunElevation);
  // Precompose base colors
  fts := exp(-6 * (PI / 2 - ts));
  VectorLerp(SunZenithColor.Color, SunDawnColor.Color, fts, FCurSunColor);
  fts := Power(1 - cos(ts - 0.5), 2);
  VectorLerp(HazeColor.Color, NightColor.Color, fts, FCurHazeColor);
  VectorLerp(SkyColor.Color, NightColor.Color, fts, FCurSkyColor);
  // Precalculate Turbidity factors
  FCurHazeTurbid := -sqrt(121 - Turbidity) * 2;
  FCurSunSkyTurbid := -(121 - Turbidity);

  //fade stars if required
  if SunElevation>-40 then
    ts := power(1 - (SunElevation+40) / 90, 11)
  else
    ts := 1;
  color := RGB(round(ts * 255), round(ts * 255), round(ts * 255));
  if esoFadeStarsWithSun in ExtendedOptions then
    for i := 0 to Stars.Count - 1 do
      stars[i].Color := color;

  if esoRotateOnTwelveHours in ExtendedOptions then
  begin
    if SunElevation = 90 then
    begin
      roll(180);
      for i := 0 to Stars.Count - 1 do
        stars[i].RA := Stars[i].RA + 180;
    end
    else if SunElevation = -90 then
    begin
      roll(180);
      for i := 0 to Stars.Count - 1 do
        stars[i].RA := Stars[i].RA + 180;
    end;
  end;

  StructureChanged;
end;

// CalculateColor
//

function TGLEarthSkyDome.CalculateColor(const theta, cosGamma: Single):
  TColorVector;
var
  t: Single;
begin
  t := PI / 2 - theta;
  // mix to get haze/sky
  VectorLerp(FCurSkyColor, FCurHazeColor, ClampValue(exp(FCurHazeTurbid * t), 0,
    1), Result);
  // then mix sky with sun
  VectorLerp(Result, FCurSunColor, ClampValue(exp(FCurSunSkyTurbid * cosGamma *
    (1 + t)) * 1.1, 0, 1), Result);
end;

// SetSunElevation
//

procedure TGLEarthSkyDome.RenderDome;
var
  ts: Single;
  steps: Integer;
  sunPos: TAffineVector;
  sinTable, cosTable: PFloatArray;

  // coordinates system note: X is forward, Y is left and Z is up
  // always rendered as sphere of radius 1

  function CalculateCosGamma(const p: TVector): Single;
  begin
    Result := 1 - VectorAngleCosine(PAffineVector(@p)^, sunPos);
  end;

  procedure RenderDeepBand(stop: Single);
  var
    i: Integer;
    r, thetaStart: Single;
    vertex1: TVector;
    color: TColorVector;
  begin
    r := 0;
    vertex1.V[3] := 1;
    // triangle fan with south pole
    GL.Begin_(GL_TRIANGLE_FAN);
    color := CalculateColor(0, CalculateCosGamma(ZHmgPoint));
    GL.Color4fv(DeepColor.AsAddress);
    GL.Vertex3f(0, 0, -1);
    SinCos(GLVectorGeometry.DegToRad(stop), vertex1.V[2], r);
    thetaStart := GLVectorGeometry.DegToRad(90 - stop);
    for i := 0 to steps - 1 do
    begin
      vertex1.V[0] := r * cosTable[i];
      vertex1.V[1] := r * sinTable[i];
      color := CalculateColor(thetaStart, CalculateCosGamma(vertex1));
      GL.Color4fv(@color);
      GL.Vertex4fv(@vertex1);
    end;
    GL.End_;
  end;

  procedure RenderBand(start, stop: Single);
  var
    i: Integer;
    r, r2, thetaStart, thetaStop: Single;
    vertex1, vertex2: TVector;
    color: TColorVector;
  begin
    vertex1.V[3] := 1;
    if stop = 90 then
    begin
      // triangle fan with north pole
      GL.Begin_(GL_TRIANGLE_FAN);
      color := CalculateColor(0, CalculateCosGamma(ZHmgPoint));
      GL.Color4fv(@color);
      GL.Vertex4fv(@ZHmgPoint);
      SinCos(GLVectorGeometry.DegToRad(start), vertex1.V[2], r);
      thetaStart := GLVectorGeometry.DegToRad(90 - start);
      for i := 0 to steps - 1 do
      begin
        vertex1.V[0] := r * cosTable[i];
        vertex1.V[1] := r * sinTable[i];
        color := CalculateColor(thetaStart, CalculateCosGamma(vertex1));
        GL.Color4fv(@color);
        GL.Vertex4fv(@vertex1);
      end;
      GL.End_;
    end
    else
    begin
      vertex2.V[3] := 1;
      // triangle strip
      GL.Begin_(GL_TRIANGLE_STRIP);
      SinCos(GLVectorGeometry.DegToRad(start), vertex1.V[2], r);
      SinCos(GLVectorGeometry.DegToRad(stop), vertex2.V[2], r2);
      thetaStart := GLVectorGeometry.DegToRad(90 - start);
      thetaStop := GLVectorGeometry.DegToRad(90 - stop);
      for i := 0 to steps - 1 do
      begin
        vertex1.V[0] := r * cosTable[i];
        vertex1.V[1] := r * sinTable[i];
        color := CalculateColor(thetaStart, CalculateCosGamma(vertex1));
        GL.Color4fv(@color);
        GL.Vertex4fv(@vertex1);
        vertex2.V[0] := r2 * cosTable[i];
        vertex2.V[1] := r2 * sinTable[i];
        color := CalculateColor(thetaStop, CalculateCosGamma(vertex2));
        GL.Color4fv(@color);
        GL.Vertex4fv(@vertex2);
      end;
      GL.End_;
    end;
  end;

var
  n, i, sdiv2: Integer;
  t, t2, p, fs: Single;
begin
  ts := GLVectorGeometry.DegToRad(90 - SunElevation);
  SetVector(sunPos, sin(ts), 0, cos(ts));
  // prepare sin/cos LUT, with a higher sampling around 0Ѝ
  n := Slices div 2;
  steps := 2 * n + 1;
  GetMem(sinTable, steps * SizeOf(Single));
  GetMem(cosTable, steps * SizeOf(Single));
  for i := 1 to n do
  begin
    p := (1 - Sqrt(Cos((i / n) * cPIdiv2))) * PI;
    SinCos(p, sinTable[n + i], cosTable[n + i]);
    sinTable[n - i] := -sinTable[n + i];
    cosTable[n - i] := cosTable[n + i];
  end;
  // these are defined by hand for precision issue: the dome must wrap exactly
  sinTable[n] := 0;
  cosTable[n] := 1;
  sinTable[0] := 0;
  cosTable[0] := -1;
  sinTable[steps - 1] := 0;
  cosTable[steps - 1] := -1;
  fs := SunElevation / 90;
  // start render
  t := 0;
  sdiv2 := Stacks div 2;
  for n := 0 to Stacks - 1 do
  begin
    if fs > 0 then
    begin
      if n < sdiv2 then
        t2 := fs - fs * Sqr((sdiv2 - n) / sdiv2)
      else
        t2 := fs + Sqr((n - sdiv2) / (sdiv2 - 1)) * (1 - fs);
    end
    else
      t2 := (n + 1) / Stacks;
    RenderBand(Lerp(1, 90, t), Lerp(1, 90, t2));
    t := t2;
  end;
  RenderDeepBand(1);
  FreeMem(sinTable);
  FreeMem(cosTable);
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TGLSkyDome, TGLEarthSkyDome]);

end.