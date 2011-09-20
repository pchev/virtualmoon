//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLLensFlare<p>

   Lens flare object.<p>

 <b>History : </b><font size=-1><ul>
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>22/04/10 - Yar - Fixes after GLState revision
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>13/03/09 - DanB - changed glReadPixels/glTexImage2D calls to glCopyTexImage2D
      <li>10/10/08 - DanB - changed Lensflare buildlists to use rci.cameraPosition instead
                            of Scene.CurrentGLCamera.DistanceTo
      <li>08/08/07 - Lin - Bugfix for AutoZTest:
                           Lensflare is no longer occluded by objects BEHIND the flare.
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>30/03/07 - DaStr - Moved all UNSAFE_TYPE, UNSAFE_CODE checks to GLSCene.inc
      <li>25/03/07 - DaStr - UNSAFE_TYPE and UNSAFE_CODE warnings are now ignored
      <li>23/03/07 - DaStr - Added missing parameters in procedure's implementation
                             (thanks Burkhard Carstens) (Bugtracker ID = 1681409)
      <li>22/03/07 - DaStr - Cleanup after previous fix - now object does not
                             igore its children in picking state
                             Removed "unsafe type/unsafe code" warnings
      <li>15/03/07 - DaStr - Removed flicker that occured when LensFlare was
                             rendered in a picking state (BugTracker ID = 1681031)
      <li>19/04/04 - EG - Fixed occlusion test and pojection matrix stack issues
      <li>16/04/04 - EG - Added StreakAngle
      <li>15/04/04 - EG - Texture-based Lens-flare moved to GLTexLensFlare,
                          replaced gradient arrays with design-time editable colors
      <li>25/09/03 - EG - Increased occlusion testing robustness
      <li>20/09/03 - EG - Can now use occlusion testing/query for AutoZTest
      <li>19/09/03 - EG - Misc. cleanup, added PreRender
      <li>18/08/03 - SG - Added TGLTextureLensFlare (Tobias Peirick)
      <li>26/03/03 - EG - Framerate independant glow transitions (Tobias Peirick)
      <li>08/12/02 - EG - Added AutoZTest
      <li>29/10/02 - EG - Initial, added defaults and encapsulation,
                          fixed positionning, RandSeed now preserved,
                          minor speedup
 </ul></font><p>

   Author  : Tobias Peirick<br>
   eMail   : peirick@onlinehome.de<br>
   Homepage: http://www.TobSoft.de
}
unit GLLensFlare;

interface

{$I GLScene.inc}

uses
  Classes, GLScene, VectorGeometry, GLObjects, OpenGLTokens,
  GLContext, GLColor, BaseClasses, GLRenderContextInfo, GLState,
  GLTextureFormat;

type

  // TFlareElement
  //
  TFlareElement = (feGlow, feRing, feStreaks, feRays, feSecondaries);
  TFlareElements = set of TFlareElement;

  {: The actual gradients between two colors are, of course, calculated by OpenGL.<p>
     The start and end colors of a gradient are stored to represent the color of
     lens flare elements. }
  TGLFlareGradient = class(TGLUpdateAbleObject)
  private
    { Private Declarations }
    FFromColor: TGLColor;
    FToColor: TGLColor;

  protected
    { Protected Declarations }
    procedure SetFromColor(const val: TGLColor);
    procedure SetToColor(const val: TGLColor);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    constructor CreateInitialized(AOwner: TPersistent;
      const fromColor, toColor: TColorVector);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

  published
    { Public Declarations }
    property FromColor: TGLColor read FFromColor write SetFromColor;
    property ToColor: TGLColor read FToColor write SetToColor;
  end;

const
  cDefaultFlareElements = [feGlow, feRing, feStreaks, feRays, feSecondaries];

type

  // TGLLensFlare
  //
  TGLLensFlare = class(TGLBaseSceneObject)
  private
    { Private Declarations }
    FSize: Integer;
    FDeltaTime: Single;
    FCurrSize: Single;
    FSeed: Integer;
    FSqueeze: Single;
    FNumStreaks: Integer;
    FStreakWidth, FStreakAngle: Single;
    FNumSecs: Integer;
    FResolution: Integer;
    FAutoZTest: Boolean;
    FElements: TFlareElements;
    FSin20Res, FCos20Res: array of Single;
    FSinRes, FCosRes: array of Single;
    FTexRays: TGLTextureHandle;
    FFlareIsNotOccluded: Boolean;
    FOcclusionQuery: TGLOcclusionQueryHandle;
    FGlowGradient: TGLFlareGradient;
    FRingGradient: TGLFlareGradient;
    FStreaksGradient: TGLFlareGradient;
    FRaysGradient: TGLFlareGradient;
    FSecondariesGradient: TGLFlareGradient;
    FDynamic: Boolean;
    FPreRenderPoint: TGLRenderPoint;

  protected
    { Protected Declarations }
    procedure SetGlowGradient(const val: TGLFlareGradient);
    procedure SetRingGradient(const val: TGLFlareGradient);
    procedure SetStreaksGradient(const val: TGLFlareGradient);
    procedure SetRaysGradient(const val: TGLFlareGradient);
    procedure SetSecondariesGradient(const val: TGLFlareGradient);
    procedure SetSize(aValue: Integer);
    procedure SetSeed(aValue: Integer);
    procedure SetSqueeze(aValue: Single);
    function StoreSqueeze: Boolean;
    procedure SetNumStreaks(aValue: Integer);
    procedure SetStreakWidth(aValue: Single);
    function StoreStreakWidth: Boolean;
    procedure SetStreakAngle(aValue: Single);
    procedure SetNumSecs(aValue: Integer);
    procedure SetResolution(aValue: Integer);
    procedure SetAutoZTest(aValue: Boolean);
    procedure SetElements(aValue: TFlareElements);
    procedure SetDynamic(aValue: Boolean);
    procedure SetPreRenderPoint(const val: TGLRenderPoint);
    procedure PreRenderEvent(Sender: TObject; var rci: TRenderContextInfo);
    procedure PreRenderPointFreed(Sender: TObject);

    // These are quite unusual in that they don't use an RCI, since
    // PreRender is done before proper rendering starts, but we do know
    // which RC is being used, so we can use this state cache
    procedure SetupRenderingOptions(StateCache: TGLStateCache);

    procedure RenderRays(StateCache: TGLStateCache; const size: Single);
    procedure RenderStreaks(StateCache: TGLStateCache);
    procedure RenderRing;
    procedure RenderSecondaries(const posVector: TAffineVector);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    procedure BuildList(var rci: TRenderContextInfo); override;
    procedure DoProgress(const progressTime: TProgressTimes); override;

    {: Prepares pre-rendered texture to speed up actual rendering.<p>
       Will use the currently active context as scratch space, and will
       automatically do nothing if things have already been prepared,
       thus you can invoke it systematically in a Viewer.BeforeRender
       event f.i. }
    procedure PreRender(activeBuffer: TGLSceneBuffer);
    {: Access to the Flare's current size.<p>
       Flares decay or grow back over several frames, depending on their
       occlusion status, and this property allows to track or manually
       alter this instantaneous size. }
    property FlareInstantaneousSize: Single read FCurrSize write FCurrSize;

  published
    { Public Declarations }
    property GlowGradient: TGLFlareGradient read FGlowGradient write
      SetGlowGradient;
    property RingGradient: TGLFlareGradient read FRingGradient;
    property StreaksGradient: TGLFlareGradient read FStreaksGradient;
    property RaysGradient: TGLFlareGradient read FRaysGradient;
    property SecondariesGradient: TGLFlareGradient read FSecondariesGradient;

    //: MaxRadius of the flare.
    property Size: Integer read FSize write SetSize default 50;
    //: Random seed
    property Seed: Integer read FSeed write SetSeed;
    //: To create elliptic flares.
    property Squeeze: Single read FSqueeze write SetSqueeze stored StoreSqueeze;
    //: Number of streaks.
    property NumStreaks: Integer read FNumStreaks write SetNumStreaks default 4;
    //: Width of the streaks.
    property StreakWidth: Single read FStreakWidth write SetStreakWidth stored
      StoreStreakWidth;
    //: Angle of the streaks (in degrees)
    property StreakAngle: Single read FStreakAngle write SetStreakAngle;
    //: Number of secondary flares.
    property NumSecs: Integer read FNumSecs write SetNumSecs default 8;
    //: Number of segments used when rendering circles.
    property Resolution: Integer read FResolution write SetResolution default
      64;
    {: Automatically computes FlareIsNotOccluded depending on ZBuffer test.<p>
       Not that the automated test may use test result from the previous
       frame into the next (to avoid a rendering stall). }
    property AutoZTest: Boolean read FAutoZTest write SetAutoZTest default True;
    {: Is the LensFlare not occluded?.<p>
       If false the flare will fade away, if true, it will fade in and stay.
       This value is automatically updated if AutoZTest is set. }
    property FlareIsNotOccluded: Boolean read FFlareIsNotOccluded write
      FFlareIsNotOccluded;
    //: Which elements should be rendered?
    property Elements: TFlareElements read FElements write SetElements default
      cDefaultFlareElements;
    {: Is the flare size adjusted dynamically?<p>
       If true, the flare size will be grown and reduced over a few frames
       when it switches between occluded and non-occluded states. This
       requires animation to be active, but results in a smoother appearance.<br>
       When false, flare will either be at full size or hidden.<p>
       The flare is always considered non-dynamic at design-time. }
    property Dynamic: Boolean read FDynamic write FDynamic default True;

    {: PreRender point for pre-rendered flare textures.<p>
       See PreRender method for more details. }
    property PreRenderPoint: TGLRenderPoint read FPreRenderPoint write
      SetPreRenderPoint;

    property ObjectsSorting;
    property Position;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses GLUtils {$IFDEF GLS_DELPHI}, VectorTypes{$ENDIF};

// ------------------
// ------------------ TGLFlareGradient ------------------
// ------------------

// Create
//

constructor TGLFlareGradient.Create(AOwner: TPersistent);
begin
  inherited;
  FFromColor := TGLColor.Create(Self);
  FToColor := TGLColor.Create(Self);
end;

// CreateInitialized
//

constructor TGLFlareGradient.CreateInitialized(AOwner: TPersistent;
  const fromColor, toColor: TColorVector);
begin
  Create(AOwner);
  FFromColor.Initialize(fromColor);
  FToColor.Initialize(toColor);
end;

// Destroy
//

destructor TGLFlareGradient.Destroy;
begin
  FToColor.Free;
  FFromColor.Free;
  inherited;
end;

// Assign
//

procedure TGLFlareGradient.Assign(Source: TPersistent);
begin
  if Source is TGLFlareGradient then
  begin
    FromColor := TGLFlareGradient(Source).FromColor;
    ToColor := TGLFlareGradient(Source).ToColor;
  end;
  inherited;
end;

// SetFromColor
//

procedure TGLFlareGradient.SetFromColor(const val: TGLColor);
begin
  FFromColor.Assign(val);
end;

// SetToColor
//

procedure TGLFlareGradient.SetToColor(const val: TGLColor);
begin
  FToColor.Assign(val);
end;

// ------------------
// ------------------ TGLLensFlare ------------------
// ------------------

// Create
//

constructor TGLLensFlare.Create(AOwner: TComponent);
begin
  inherited;
  // Set default parameters:
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FSize := 50;
  FSeed := 1465;
  FSqueeze := 1;
  FNumStreaks := 4;
  FStreakWidth := 2;
  FNumSecs := 8;
  FAutoZTest := True;
  FlareIsNotOccluded := True;
  FDynamic := True;

  SetResolution(64);

  // Render all elements by default.
  FElements := [feGlow, feRing, feStreaks, feRays, feSecondaries];
  // Setup default gradients:
  FGlowGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(1, 1, 0.8, 0.3), VectorMake(1, 0.2, 0, 0));
  FRingGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(0.5, 0.2, 0, 0.1), VectorMake(0.5, 0.4, 0, 0.1));
  FStreaksGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(1, 1, 1, 0.2), VectorMake(0.2, 0, 1, 0));
  FRaysGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(1, 0.8, 0.5, 0.05), VectorMake(0.5, 0.2, 0, 0));
  FSecondariesGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(0, 0.2, 1, 0), VectorMake(0, 0.8, 0.2, 0.15));

  FTexRays := TGLTextureHandle.Create;
end;

// Destroy
//

destructor TGLLensFlare.Destroy;
begin
  PreRenderPoint := nil;
  FGlowGradient.Free;
  FRingGradient.Free;
  FStreaksGradient.Free;
  FRaysGradient.Free;
  FSecondariesGradient.Free;
  FOcclusionQuery.Free;
  FTexRays.Free;
  inherited;
end;

// Notification
//

procedure TGLLensFlare.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (Operation = opRemove) and (AComponent = FPreRenderPoint) then
    PreRenderPoint := nil;
  inherited;
end;

// SetupRenderingOptions
//

procedure TGLLensFlare.SetupRenderingOptions(StateCache: TGLStateCache);
begin
  with StateCache do
  begin
    Disable(stLighting);
    Disable(stDepthTest);
    Disable(stFog);
    Disable(stColorMaterial);
    Disable(stCullFace);
    DepthWriteMask := False;
    Enable(stBlend);
    SetBlendFunc(bfSrcAlpha, bfOne);
    Disable(stAlphaTest);
    PolygonMode := pmFill;
  end;
end;

// RenderRays
//

procedure TGLLensFlare.RenderRays(StateCache: TGLStateCache; const size:
  Single);
var
  i: Integer;
  rnd: Single;
begin
{$IFDEF GLS_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(14, 'LensFlare.Rays');
{$ENDIF}

  with StateCache do
  begin
    LineWidth := 1;
    Disable(stLineSmooth);
    Disable(stLineStipple);
  end;

  GL.Begin_(GL_LINES);
  for i := 0 to Resolution * 20 - 1 do
  begin
    if (i and 1) <> 0 then
      rnd := 1.5 * Random * size
    else
      rnd := Random * size;
    GL.Color4fv(RaysGradient.FromColor.AsAddress);
    GL.Vertex2f(0, 0);
    GL.Color4fv(RaysGradient.ToColor.AsAddress);
    GL.Vertex2f(rnd * FCos20Res[i], rnd * FSin20Res[i] * Squeeze);
  end;
  GL.End_;
end;

// RenderStreak
//

procedure TGLLensFlare.RenderStreaks(StateCache: TGLStateCache);
var
  i: Integer;
  a, f, s, c: Single;
begin
{$IFDEF GLS_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(17, 'LensFlare.Streaks');
{$ENDIF}
  StateCache.Enable(stLineSmooth);
  StateCache.LineWidth := StreakWidth;
  a := c2PI / NumStreaks;
  f := 1.5 * FCurrSize;
  GL.Begin_(GL_LINES);
  for i := 0 to NumStreaks - 1 do
  begin
    SinCos(StreakAngle * cPIdiv180 + a * i, f, s, c);
    GL.Color4fv(StreaksGradient.FromColor.AsAddress);
    GL.Vertex3fv(@NullVector);
    GL.Color4fv(StreaksGradient.ToColor.AsAddress);
    GL.Vertex2f(c, Squeeze * s);
  end;
  GL.End_;
  StateCache.Disable(stLineSmooth);
end;

// RenderRing
//

procedure TGLLensFlare.RenderRing;
var
  i: Integer;
  rW, s0, c0, s, c: Single;
begin
{$IFDEF GLS_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(14, 'LensFlare.Ring');
{$ENDIF}
  rW := FCurrSize * (1 / 15); // Ring width
  GL.Begin_(GL_QUADS);
  s0 := 0;
  c0 := 0.6;
  for i := 0 to Resolution - 1 do
  begin
    s := s0;
    c := c0;
    s0 := FSinRes[i] * 0.6 * Squeeze;
    c0 := FCosRes[i] * 0.6;

    GL.Color4fv(GlowGradient.ToColor.AsAddress);
    GL.Vertex2f((FCurrSize - rW) * c, (FCurrSize - rW) * s);
    GL.Color4fv(RingGradient.FromColor.AsAddress);
    GL.Vertex2f(FCurrSize * c, Squeeze * FCurrSize * s);

    GL.Vertex2f(FCurrSize * c0, FCurrSize * s0);
    GL.Color4fv(GlowGradient.ToColor.AsAddress);
    GL.Vertex2f((FCurrSize - rW) * c0, (FCurrSize - rW) * s0);

    GL.Color4fv(RingGradient.FromColor.AsAddress);
    GL.Vertex2f(FCurrSize * c, FCurrSize * s);
    GL.Vertex2f(FCurrSize * c0, FCurrSize * s0);

    GL.Color4fv(GlowGradient.ToColor.AsAddress);
    GL.Vertex2f((FCurrSize + rW) * c0, (FCurrSize + rW) * s0);
    GL.Vertex2f((FCurrSize + rW) * c, (FCurrSize + rW) * s);
  end;
  GL.End_;
end;

// RenderSecondaries
//

procedure TGLLensFlare.RenderSecondaries(const posVector: TAffineVector);
var
  i, j: Integer;
  rnd: Single;
  v: TAffineVector;
  grad: TGLFlareGradient;
begin
{$IFDEF GLS_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(21, 'LensFlare.Secondaries');
{$ENDIF}
  // Other secondaries (plain gradiented circles, like the glow):
  for j := 1 to NumSecs do
  begin
    rnd := 2 * Random - 1;
    // If rnd < 0 then the secondary glow will end up on the other side
    // of the origin. In this case, we can push it really far away from
    // the flare. If  the secondary is on the flare's side, we pull it
    // slightly towards the origin to avoid it winding up in the middle
    // of the flare.
    if rnd < 0 then
      v := VectorScale(posVector, rnd)
    else
      v := VectorScale(posVector, 0.8 * rnd);
    if j mod 3 = 0 then
      grad := GlowGradient
    else
      grad := SecondariesGradient;
    rnd := (Random + 0.1) * FCurrSize * 0.25;

    GL.Begin_(GL_TRIANGLE_FAN);
    GL.Color4fv(grad.FromColor.AsAddress);
    GL.Vertex2f(v[0], v[1]);
    GL.Color4fv(grad.ToColor.AsAddress);
    for i := 0 to Resolution - 1 do
      GL.Vertex2f(FCosRes[i] * rnd + v[0], FSinRes[i] * rnd + v[1]);
    GL.End_;
  end;
end;

// BuildList
//

procedure TGLLensFlare.BuildList(var rci: TRenderContextInfo);
var
  i: Integer;
  depth, dist: Single;
  posVector, v, rv: TAffineVector;
  screenPos: TAffineVector;
  flareInViewPort, dynamicSize: Boolean;
  oldSeed: {$IFDEF FPC}LongWord{$ELSE}LongInt{$ENDIF};
  projMatrix: TMatrix;
  CurrentBuffer: TGLSceneBuffer;
begin
  if (rci.drawState = dsPicking) then
  begin
    if Count <> 0 then
      Self.RenderChildren(0, Count - 1, rci);
    Exit;
  end;
  CurrentBuffer := TGLSceneBuffer(rci.buffer);

  SetVector(v, AbsolutePosition);
  // are we looking towards the flare?
  rv := VectorSubtract(v, PAffineVector(@rci.cameraPosition)^);
  if VectorDotProduct(rci.cameraDirection, rv) > 0 then
  begin
    // find out where it is on the screen.
    screenPos := CurrentBuffer.WorldToScreen(v);
    flareInViewPort := (screenPos[0] < rci.viewPortSize.cx)
    and (screenPos[0] >= 0)
    and (screenPos[1] < rci.viewPortSize.cy)
    and (screenPos[1] >= 0);
  end
  else
    flareInViewPort := False;

  dynamicSize := FDynamic and not (csDesigning in ComponentState);
  if dynamicSize then
  begin
    // make the glow appear/disappear progressively
    if flareInViewPort and FlareIsNotOccluded then
    begin
      FCurrSize := FCurrSize + FDeltaTime * 10 * Size;
      if FCurrSize > Size then
        FCurrSize := Size;
    end
    else
    begin
      FCurrSize := FCurrSize - FDeltaTime * 10 * Size;
      if FCurrSize < 0 then
        FCurrSize := 0;
    end;
  end
  else
  begin
    if flareInViewPort and FlareIsNotOccluded then
      FCurrSize := Size
    else
      FCurrSize := 0;
  end;

  // Prepare matrices
  GL.PushMatrix;
  GL.LoadMatrixf(@CurrentBuffer.BaseProjectionMatrix);

  GL.MatrixMode(GL_PROJECTION);
  GL.PushMatrix;
  projMatrix := IdentityHmgMatrix;
  projMatrix[0][0] := 2 / rci.viewPortSize.cx;
  projMatrix[1][1] := 2 / rci.viewPortSize.cy;
  GL.LoadMatrixf(@projMatrix);

  MakeVector(posVector,
    screenPos[0] - rci.viewPortSize.cx * 0.5,
    screenPos[1] - rci.viewPortSize.cy * 0.5,
    0);

  if AutoZTest then
  begin
    if dynamicSize and (GL.HP_occlusion_test or
      TGLOcclusionQueryHandle.IsSupported) then
    begin
      // hardware-based occlusion test is possible
      FlareIsNotOccluded := True;

      rci.GLStates.SetColorMask([]);
      rci.GLStates.Disable(stAlphaTest);
      rci.GLStates.DepthWriteMask := False;
      rci.GLStates.Enable(stDepthTest);
      rci.GLStates.DepthFunc := cfLEqual;

      if TGLOcclusionQueryHandle.IsSupported then
      begin
        // preferred method, doesn't stall rendering too badly
        if not Assigned(FOcclusionQuery) then
          FOcclusionQuery := TGLOcclusionQueryHandle.Create;
        FOcclusionQuery.AllocateHandle;
        if FOcclusionQuery.IsDataNeedUpdate then
          FOcclusionQuery.NotifyDataUpdated
        else
          FlareIsNotOccluded := (FOcclusionQuery.PixelCount <> 0);
        FOcclusionQuery.BeginQuery;
      end
      else
      begin
        // occlusion_test, stalls rendering a bit
        GL.Enable(GL_OCCLUSION_TEST_HP);
      end;

      GL.Begin_(GL_QUADS);
      GL.Vertex3f(posVector[0] + 2, posVector[1], 1);
      GL.Vertex3f(posVector[0], posVector[1] + 2, 1);
      GL.Vertex3f(posVector[0] - 2, posVector[1], 1);
      GL.Vertex3f(posVector[0], posVector[1] - 2, 1);
      GL.End_;

      if TGLOcclusionQueryHandle.IsSupported then
        FOcclusionQuery.EndQuery
      else
      begin
        GL.Disable(GL_OCCLUSION_TEST_HP);
        GL.GetBooleanv(GL_OCCLUSION_TEST_RESULT_HP, @FFlareIsNotOccluded)
      end;

      rci.GLStates.DepthFunc := cfLEqual;
      rci.GLStates.SetColorMask(cAllColorComponents);
    end
    else
    begin
      //Compares the distance to the lensflare, to the z-buffer depth.
      //This prevents the flare from being occluded by objects BEHIND the light.
      depth := CurrentBuffer.PixelToDistance(Round(ScreenPos[0]),
        Round(rci.viewPortSize.cy - ScreenPos[1]));
      dist := VectorDistance(rci.cameraPosition, self.AbsolutePosition);
      FlareIsNotOccluded := ((dist - depth) < 1);
    end;
  end;

  if FCurrSize >= 0 then
  begin

    // Random seed must be backed up, could be used for other purposes
    // (otherwise we essentially reset the random generator at each frame)
    oldSeed := RandSeed;
    RandSeed := Seed;

    SetupRenderingOptions(rci.GLStates);

    if [feGlow, feStreaks, feRays, feRing] * Elements <> [] then
    begin
      GL.Translatef(posVector[0], posVector[1], posVector[2]);

      // Glow (a circle with transparent edges):
      if feGlow in Elements then
      begin
        GL.Begin_(GL_TRIANGLE_FAN);
        GL.Color4fv(GlowGradient.FromColor.AsAddress);
        GL.Vertex2f(0, 0);
        GL.Color4fv(GlowGradient.ToColor.AsAddress);
        for i := 0 to Resolution - 1 do
          GL.Vertex2f(FCurrSize * FCosRes[i],
            Squeeze * FCurrSize * FSinRes[i]);
        GL.End_;
      end;

      if feStreaks in Elements then
        RenderStreaks(rci.GLStates);

      // Rays (random-length lines from the origin):
      if feRays in Elements then
      begin
        if FTexRays.Handle <> 0 then
        begin
        {$IFDEF GLS_OPENGL_DEBUG}
          if GL.GREMEDY_string_marker then
            GL.StringMarkerGREMEDY(19, 'LensFlare.RaysQuad');
        {$ENDIF}
          rci.GLStates.TextureBinding[0, ttTexture2D] := FTexRays.Handle;
          rci.GLStates.ActiveTextureEnabled[ttTexture2D] := True;
          GL.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

          GL.Begin_(GL_QUADS);
          GL.TexCoord2f(0, 0);
          GL.Vertex2f(-FCurrSize, -FCurrSize);
          GL.TexCoord2f(1, 0);
          GL.Vertex2f(FCurrSize, -FCurrSize);
          GL.TexCoord2f(1, 1);
          GL.Vertex2f(FCurrSize, FCurrSize);
          GL.TexCoord2f(0, 1);
          GL.Vertex2f(-FCurrSize, FCurrSize);
          GL.End_;

          rci.GLStates.ActiveTextureEnabled[ttTexture2D] := False;
        end
        else
          RenderRays(rci.GLStates, FCurrSize);
      end;

      if feRing in Elements then
        RenderRing;

      GL.LoadMatrixf(@projMatrix);
    end;

    if feSecondaries in Elements then
      RenderSecondaries(posVector);

    RandSeed := oldSeed;
  end;

  GL.PopMatrix;
  GL.MatrixMode(GL_MODELVIEW);
  GL.PopMatrix;

  if Count > 0 then
    Self.RenderChildren(0, Count - 1, rci);
end;

// DoProgress
//

procedure TGLLensFlare.DoProgress(const progressTime: TProgressTimes);
begin
  inherited;
  FDeltaTime := progressTime.deltaTime;
end;

// PreRender
//

procedure TGLLensFlare.PreRender(activeBuffer: TGLSceneBuffer);
var
  i, texSize, maxSize: Integer;
  stateCache: TGLStateCache;
begin
  if FTexRays.Handle <> 0 then
    Exit;
  with activeBuffer.RenderingContext do
  begin
    stateCache := GLStates;
    PipelineTransformation.Push;
    PipelineTransformation.ProjectionMatrix := CreateOrthoMatrix(0, activeBuffer.Width, 0, activeBuffer.Height, -1, 1);
    PipelineTransformation.ViewMatrix := IdentityHmgMatrix;
  end;
  SetupRenderingOptions(stateCache);

  texSize := RoundUpToPowerOf2(Size);
  if texSize < Size * 1.5 then
    texSize := texSize * 2;
  GL.GetIntegerv(GL_MAX_TEXTURE_SIZE, @maxSize);
  if texSize > maxSize then
    texSize := maxSize;

  stateCache.Disable(stBlend);
  GL.Color4f(0, 0, 0, 0);
  GL.Begin_(GL_QUADS);
  GL.Vertex2f(0, 0);
  GL.Vertex2f(texSize + 4, 0);
  GL.Vertex2f(texSize + 4, texSize + 4);
  GL.Vertex2f(0, texSize + 4);
  GL.End_;
  stateCache.Enable(stBlend);

  GL.Translatef(texSize * 0.5 + 2, texSize * 0.5 + 2, 0);
  RenderRays(stateCache, texSize * 0.5);

  FTexRays.AllocateHandle;
  stateCache.TextureBinding[0, ttTexture2D] := FTexRays.Handle;
  if GL.EXT_texture_edge_clamp then
    i := GL_CLAMP_TO_EDGE
  else
    i := GL_CLAMP;
  GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, i);
  GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, i);
  GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  GL.CopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 2, 2, texSize, texSize, 0);

  activeBuffer.RenderingContext.PipelineTransformation.Pop;

  GL.CheckError;
end;

// SetGlowGradient
//

procedure TGLLensFlare.SetGlowGradient(const val: TGLFlareGradient);
begin
  FGlowGradient.Assign(val);
  StructureChanged;
end;

// SetRingGradient
//

procedure TGLLensFlare.SetRingGradient(const val: TGLFlareGradient);
begin
  FRingGradient.Assign(val);
  StructureChanged;
end;

// SetStreaksGradient
//

procedure TGLLensFlare.SetStreaksGradient(const val: TGLFlareGradient);
begin
  FStreaksGradient.Assign(val);
  StructureChanged;
end;

// SetRaysGradient
//

procedure TGLLensFlare.SetRaysGradient(const val: TGLFlareGradient);
begin
  FRaysGradient.Assign(val);
  StructureChanged;
end;

// SetSecondariesGradient
//

procedure TGLLensFlare.SetSecondariesGradient(const val: TGLFlareGradient);
begin
  FSecondariesGradient.Assign(val);
  StructureChanged;
end;

// SetSize
//

procedure TGLLensFlare.SetSize(aValue: Integer);
begin
  FSize := aValue;
  StructureChanged;
end;

// SetSeed
//

procedure TGLLensFlare.SetSeed(aValue: Integer);
begin
  FSeed := aValue;
  StructureChanged;
end;

// SetSqueeze
//

procedure TGLLensFlare.SetSqueeze(aValue: Single);
begin
  FSqueeze := aValue;
  StructureChanged;
end;

// StoreSqueeze
//

function TGLLensFlare.StoreSqueeze: Boolean;
begin
  Result := (FSqueeze <> 1);
end;

// SetNumStreaks
//

procedure TGLLensFlare.SetNumStreaks(aValue: Integer);
begin
  FNumStreaks := aValue;
  StructureChanged;
end;

// SetStreakWidth
//

procedure TGLLensFlare.SetStreakWidth(aValue: Single);
begin
  FStreakWidth := aValue;
  StructureChanged;
end;

// StoreStreakWidth
//

function TGLLensFlare.StoreStreakWidth: Boolean;
begin
  Result := (FStreakWidth <> 2);
end;

// SetStreakAngle
//

procedure TGLLensFlare.SetStreakAngle(aValue: Single);
begin
  FStreakAngle := aValue;
  StructureChanged;
end;

// SetNumSecs
//

procedure TGLLensFlare.SetNumSecs(aValue: Integer);
begin
  FNumSecs := aValue;
  StructureChanged;
end;

// SetResolution
//

procedure TGLLensFlare.SetResolution(aValue: Integer);
begin
  if FResolution <> aValue then
  begin
    FResolution := aValue;
    StructureChanged;
    SetLength(FSin20Res, 20 * FResolution);
    SetLength(FCos20Res, 20 * FResolution);
    PrepareSinCosCache(FSin20Res, FCos20Res, 0, 360);
    SetLength(FSinRes, FResolution);
    SetLength(FCosRes, FResolution);
    PrepareSinCosCache(FSinRes, FCosRes, 0, 360);
  end;
end;

// SetAutoZTest
//

procedure TGLLensFlare.SetAutoZTest(aValue: Boolean);
begin
  if FAutoZTest <> aValue then
  begin
    FAutoZTest := aValue;
    StructureChanged;
  end;
end;

// SetElements
//

procedure TGLLensFlare.SetElements(aValue: TFlareElements);
begin
  if FElements <> aValue then
  begin
    FElements := aValue;
    StructureChanged;
  end;
end;

// SetDynamic
//

procedure TGLLensFlare.SetDynamic(aValue: Boolean);
begin
  if aValue <> FDynamic then
  begin
    FDynamic := aValue;
    NotifyChange(Self);
  end;
end;

// SetPreRenderPoint
//

procedure TGLLensFlare.SetPreRenderPoint(const val: TGLRenderPoint);
begin
  if val <> FPreRenderPoint then
  begin
    if Assigned(FPreRenderPoint) then
      FPreRenderPoint.UnRegisterCallBack(Self.PreRenderEvent);
    FPreRenderPoint := val;
    if Assigned(FPreRenderPoint) then
      FPreRenderPoint.RegisterCallBack(Self.PreRenderEvent,
        Self.PreRenderPointFreed);
  end;
end;

// PreRenderEvent
//

procedure TGLLensFlare.PreRenderEvent(Sender: TObject; var rci:
  TRenderContextInfo);
begin
  PreRender(rci.buffer as TGLSceneBuffer);
end;

// PreRenderPointFreed
//

procedure TGLLensFlare.PreRenderPointFreed(Sender: TObject);
begin
  FPreRenderPoint := nil;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterClasses([TGLLensFlare]);

end.
