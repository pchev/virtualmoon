//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLTexLensFlare<p>

   Texture-based Lens flare object.<p>

 <b>History : </b><font size=-1><ul>
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>22/04/10 - Yar - Fixes after GLState revision
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>30/03/07 - DaStr - Added $I GLScene.inc
      <li>23/03/07 - DaStr - Added missing parameters in procedure's implementation
                             (thanks Burkhard Carstens) (Bugtracker ID = 1681409)
      <li>25/09/03 - EG - Creation from GLLensFlare split
 </ul></font><p>
}
unit GLTexLensFlare;

interface

{$I GLScene.inc}

uses
  Classes,
  GLScene,
  VectorGeometry,
  GLObjects,
  GLTexture,
  OpenGLTokens,
  GLContext,
  GLRenderContextInfo,
  BaseClasses,
  GLState
  {$IFDEF GLS_DELPHI}, VectorTypes{$ENDIF};

type

  // TGLTextureLensFlare
  //
  TGLTextureLensFlare = class(TGLBaseSceneObject)
  private
    { Private Declarations }
    FSize: integer;
    FCurrSize: Single;
    FNumSecs: integer;
    FAutoZTest: boolean;
    //used for internal calculation
    FDeltaTime: Double;
    FImgSecondaries: TGLTexture;
    FImgRays: TGLTexture;
    FImgRing: TGLTexture;
    FImgGlow: TGLTexture;
    FSeed: Integer;
    procedure SetImgGlow(const Value: TGLTexture);
    procedure SetImgRays(const Value: TGLTexture);
    procedure SetImgRing(const Value: TGLTexture);
    procedure SetImgSecondaries(const Value: TGLTexture);
    procedure SetSeed(const Value: Integer);
  protected
    { Protected Declarations }
    procedure SetSize(aValue: integer);
    procedure SetNumSecs(aValue: integer);
    procedure SetAutoZTest(aValue: boolean);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TRenderContextInfo); override;
    procedure DoProgress(const progressTime: TProgressTimes); override;
  published
    { Public Declarations }
    //: MaxRadius of the flare.
    property Size: integer read FSize write SetSize default 50;
    //: Random seed
    property Seed: Integer read FSeed write SetSeed;
    //: Number of secondary flares.
    property NumSecs: integer read FNumSecs write SetNumSecs default 8;
    //: Number of segments used when rendering circles.
    //property Resolution: integer read FResolution write SetResolution default 64;
    property AutoZTest: boolean read FAutoZTest write SetAutoZTest default True;
    // The Textures
    property ImgGlow: TGLTexture read FImgGlow write SetImgGlow;
    property ImgRays: TGLTexture read FImgRays write SetImgRays;
    property ImgRing: TGLTexture read FImgRing write SetImgRing;
    property ImgSecondaries: TGLTexture read FImgSecondaries write SetImgSecondaries;

    property ObjectsSorting;
    property Position;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

implementation

// ------------------
// ------------------ TGLTextureLensFlare ------------------
// ------------------

constructor TGLTextureLensFlare.Create(AOwner: TComponent);
begin
  inherited;
  Randomize;
  FSeed := Random(2000) + 465;

  // Set default parameters:
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FSize := 50;
  FCurrSize := FSize;
  FNumSecs := 8;
  FAutoZTest := True;

  FImgRays := TGLTexture.Create(Self);
  FImgSecondaries := TGLTexture.Create(Self);
  FImgRing := TGLTexture.Create(Self);
  FImgGlow := TGLTexture.Create(Self);
end;

procedure TGLTextureLensFlare.SetSize(aValue: integer);
begin
  if FSize <> aValue then
  begin
    FSize := aValue;
    FCurrSize := FSize;
    StructureChanged;
  end;
end;

procedure TGLTextureLensFlare.SetNumSecs(aValue: integer);
begin
  if FNumSecs <> aValue then
  begin
    FNumSecs := aValue;
    StructureChanged;
  end;
end;

// SetAutoZTest
//

procedure TGLTextureLensFlare.SetAutoZTest(aValue: boolean);
begin
  if FAutoZTest <> aValue then
  begin
    FAutoZTest := aValue;
    StructureChanged;
  end;
end;

// BuildList
//

procedure TGLTextureLensFlare.BuildList(var rci: TRenderContextInfo);
var
  v, rv, screenPos, posVector: TAffineVector;
  depth, rnd: Single;
  flag: Boolean;
  i: Integer;
  CurrentBuffer: TGLSceneBuffer;
begin
  CurrentBuffer := TGLSceneBuffer(rci.buffer);
  SetVector(v, AbsolutePosition);
  // are we looking towards the flare?
  rv := VectorSubtract(v, PAffineVector(@rci.cameraPosition)^);
  if VectorDotProduct(rci.cameraDirection, rv) > 0 then
  begin
    // find out where it is on the screen.
    screenPos := CurrentBuffer.WorldToScreen(v);
    if (screenPos[0] < rci.viewPortSize.cx) and (screenPos[0] >= 0)
      and (screenPos[1] < rci.viewPortSize.cy) and (screenPos[1] >= 0) then
    begin
      if FAutoZTest then
      begin
        depth := CurrentBuffer.GetPixelDepth(Round(ScreenPos[0]),
          Round(rci.viewPortSize.cy - ScreenPos[1]));
        // but is it behind something?
        if screenPos[2] >= 1 then
          flag := (depth >= 1)
        else
          flag := (depth >= screenPos[2]);
      end
      else
        flag := True;
    end
    else
      flag := False;
  end
  else
    flag := False;

  MakeVector(posVector,
    screenPos[0] - rci.viewPortSize.cx / 2,
    screenPos[1] - rci.viewPortSize.cy / 2, 0);

  // make the glow appear/disappear progressively

  if Flag then
    if FCurrSize < FSize then
      FCurrSize := FCurrSize + FDeltaTime * 200 {FSize * 4};
  if not Flag then
    if FCurrSize > 0 then
      FCurrSize := FCurrSize - FDeltaTime * 200 {FSize * 4};
  if FCurrSize <= 0 then
    Exit;

  // Prepare matrices
  GL.MatrixMode(GL_MODELVIEW);
  GL.PushMatrix;
  GL.LoadMatrixf(@CurrentBuffer.BaseProjectionMatrix);

  GL.MatrixMode(GL_PROJECTION);
  GL.PushMatrix;
  GL.LoadIdentity;
  GL.Scalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);

  rci.GLStates.Disable(stLighting);
  rci.GLStates.Disable(stDepthTest);
  rci.GLStates.Enable(stBlend);
  rci.GLStates.SetBlendFunc(bfOne, bfOne);

  //Rays and Glow on Same Position
  GL.PushMatrix;
  GL.Translatef(posVector[0], posVector[1], posVector[2]);

  if not ImgGlow.Disabled and Assigned(ImgGlow.Image) then
  begin
    ImgGlow.Apply(rci);
    GL.begin_(GL_QUADS);
    GL.TexCoord2f(0, 0);
    GL.Vertex3f(-FCurrSize, -FCurrSize, 0);
    GL.TexCoord2f(1, 0);
    GL.Vertex3f(FCurrSize, -FCurrSize, 0);
    GL.TexCoord2f(1, 1);
    GL.Vertex3f(FCurrSize, FCurrSize, 0);
    GL.TexCoord2f(0, 1);
    GL.Vertex3f(-FCurrSize, FCurrSize, 0);
    GL.end_;
    ImgGlow.UnApply(rci);
  end;

  if not ImgRays.Disabled and Assigned(ImgRays.Image) then
  begin
    ImgRays.Apply(rci);
    GL.begin_(GL_QUADS);
    GL.TexCoord2f(0, 0);
    GL.Vertex3f(-FCurrSize, -FCurrSize, 0);
    GL.TexCoord2f(1, 0);
    GL.Vertex3f(FCurrSize, -FCurrSize, 0);
    GL.TexCoord2f(1, 1);
    GL.Vertex3f(FCurrSize, FCurrSize, 0);
    GL.TexCoord2f(0, 1);
    GL.Vertex3f(-FCurrSize, FCurrSize, 0);
    GL.end_;
    ImgRays.UnApply(rci);
  end;
  GL.PopMatrix;

  if not ImgRing.Disabled and Assigned(ImgRing.Image) then
  begin
    GL.PushMatrix;
    GL.Translatef(posVector[0] * 1.1, posVector[1] * 1.1, posVector[2]);
    ImgRing.Apply(rci);
    GL.begin_(GL_QUADS);
    GL.TexCoord2f(0, 0);
    GL.Vertex3f(-FCurrSize, -FCurrSize, 0);
    GL.TexCoord2f(1, 0);
    GL.Vertex3f(FCurrSize, -FCurrSize, 0);
    GL.TexCoord2f(1, 1);
    GL.Vertex3f(FCurrSize, FCurrSize, 0);
    GL.TexCoord2f(0, 1);
    GL.Vertex3f(-FCurrSize, FCurrSize, 0);
    GL.end_;
    ImgRing.UnApply(rci);
    GL.PopMatrix;
  end;

  if not ImgSecondaries.Disabled and Assigned(ImgSecondaries.Image) then
  begin
    RandSeed := FSeed;
    GL.PushMatrix;
    ImgSecondaries.Apply(rci);
    for i := 1 to FNumSecs do
    begin
      rnd := 2 * Random - 1;
      v := PosVector;
      if rnd < 0 then
        ScaleVector(V, rnd)
      else
        ScaleVector(V, 0.8 * rnd);
      GL.PushMatrix;
      GL.Translatef(v[0], v[1], v[2]);

      rnd := random * 0.5 + 0.1;
      GL.begin_(GL_QUADS);
      GL.TexCoord2f(0, 0);
      GL.Vertex3f(-FCurrSize * rnd, -FCurrSize * rnd, 0);
      GL.TexCoord2f(1, 0);
      GL.Vertex3f(FCurrSize * rnd, -FCurrSize * rnd, 0);
      GL.TexCoord2f(1, 1);
      GL.Vertex3f(FCurrSize * rnd, FCurrSize * rnd, 0);
      GL.TexCoord2f(0, 1);
      GL.Vertex3f(-FCurrSize * rnd, FCurrSize * rnd, 0);
      GL.end_;
      GL.PopMatrix
    end;
    ImgSecondaries.UnApply(rci);
    GL.PopMatrix;
  end;

  // restore state

  GL.PopMatrix;
  GL.MatrixMode(GL_MODELVIEW);
  GL.PopMatrix;

  if Count > 0 then
    Self.RenderChildren(0, Count - 1, rci);
end;

// DoProgress
//

procedure TGLTextureLensFlare.DoProgress(const progressTime: TProgressTimes);
begin
  FDeltaTime := progressTime.deltaTime;
  inherited;
end;

procedure TGLTextureLensFlare.SetImgGlow(const Value: TGLTexture);
begin
  FImgGlow.Assign(Value);
  StructureChanged;
end;

procedure TGLTextureLensFlare.SetImgRays(const Value: TGLTexture);
begin
  FImgRays.Assign(Value);
  StructureChanged;
end;

procedure TGLTextureLensFlare.SetImgRing(const Value: TGLTexture);
begin
  FImgRing.Assign(Value);
  StructureChanged;
end;

procedure TGLTextureLensFlare.SetImgSecondaries(const Value: TGLTexture);
begin
  FImgSecondaries.Assign(Value);
  StructureChanged;
end;

destructor TGLTextureLensFlare.Destroy;
begin
  FImgRays.Free;
  FImgSecondaries.Free;
  FImgRing.Free;
  FImgGlow.Free;
  inherited;
end;

procedure TGLTextureLensFlare.SetSeed(const Value: Integer);
begin
  FSeed := Value;
  StructureChanged;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterClasses([TGLTextureLensFlare]);

end.

