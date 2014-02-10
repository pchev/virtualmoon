//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLHUDObjects<p>

  GLScene objects that get rendered in 2D coordinates<p>

  <b>History : </b><font size=-1><ul>
  <li>27/01/12 - Yar - Added texture coordinates mirroring for HUDSprite (thanks Tamahome)
  <li>15/11/10 - FP - Restore DepthTest at the end of RenderTextAtPosition
  <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
  Fixed light state changing
  <li>22/04/10 - Yar - Fixes after GLState revision
  <li>05/03/10 - DanB - More state added to TGLStateCache
  <li>15/03/08 - DaStr - Bugfixed TGLAbsoluteHUDText.DoRender()
  (thanks Nicoara Adrian) (BugtrackerID = 1914823)
  <li>18/09/07 - DaStr - Added TGLResolutionIndependantHUDText and
  TGLAbsoluteHUDText to the list of registered classes
  Cleaned up "uses" section
  <li>07/09/07 - DaStr - AlphaChannel is now applied to ActualPrimaryMaterial
  Added TGLResolutionIndependantHUDText,
  TGLAbsoluteHUDText
  <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
  <li>30/03/07 - DaStr - Added $I GLScene.inc
  <li>23/02/07 - DaStr - Added default values to TGLHUDSprite.Width & Height
  <li>15/02/07 - DaStr - Added default values to TGLHUDText.Alignment & Layout
  <li>28/06/04 - LR - Change TTextLayout to TGLTextLayout for Linux
  <li>27/11/02 - EG - HUDSprite and HUDText now honour renderDPI
  <li>23/11/02 - EG - Added X/YTiles to HUDSprite
  <li>12/05/02 - EG - ModulateColor for HUDText (Nelson Chu)
  <li>20/12/01 - EG - PolygonMode properly adjusted for HUDText
  <li>18/07/01 - EG - VisibilityCulling compatibility changes
  <li>20/06/01 - EG - Default hud sprite size is now 16x16
  <li>21/02/01 - EG - Now XOpenGL based (multitexture)
  <li>15/01/01 - EG - Creation
  </ul></font>
}
unit GLHUDObjects;

interface

{$I GLScene.inc}

uses
  Classes,
  GLScene,
  VectorGeometry,
  GLObjects,
  GLBitmapFont,
  GLCrossPlatform,
  GLColor,
  GLRenderContextInfo;

type

  // TGLHUDSprite
  //
  { : A rectangular area, NOT perspective projected.<p>
    (x, y) coordinates map directly to the viewport (in pixels) and refer
    the center of the area.<br>
    The coordinate system is that of an equivalent TCanvas, ie. top-left
    point is the origin (0, 0).<p>
    The z component is ignored and Z-Buffer is disabled when rendering.<p>
    <b>Using TGLHUDSprite in 2D only scenes :</b><br>
    The most convenient way to use a TGLHUDSprite as a simple 2D sprite with
    blending capabilities (transparency or additive), is to set the texture
    mode to tmModulate, in FrontProperties, to use the Emission color to
    control coloring/intensity, and finally use the Diffuse color's alpha
    to control transparency (while setting the other RGB components to 0).<br>
    You can also control aplha-blending by defining a <1 value in the sprite's
    AlphaChannel field. This provides you with hardware accelerated,
    alpha-blended blitting.<p>
    Note : since TGLHUDSprite works in absolute coordinates, TGLProxyObject
    can't be used to duplicate an hud sprite. }
  TGLHUDSprite = class(TGLSprite)
  private
    { Private Declarations }
    FXTiles, FYTiles: Integer;
    function StoreWidth: Boolean;
    function StoreHeight: Boolean;
  protected
    { Protected Declarations }
    procedure SetXTiles(const val: Integer);
    procedure SetYTiles(const val: Integer);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;

    procedure DoRender(var rci: TRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;

  published
    { Published Declarations }
    property XTiles: Integer read FXTiles write SetXTiles default 1;
    property YTiles: Integer read FYTiles write SetYTiles default 1;
    // Redeclare them with new default values.
    property Width stored StoreWidth;
    property Height stored StoreHeight;
  end;

  // TGLHUDText
  //
  { : A 2D text displayed and positionned in 2D coordinates.<p>
    The HUDText uses a character font defined and stored by a TGLBitmapFont
    component. The text can be scaled and rotated (2D), the layout and
    alignment can also be controled. }
  TGLHUDText = class(TGLImmaterialSceneObject)
  private
    { Private Declarations }
    FBitmapFont: TGLCustomBitmapFont;
    FText: UnicodeString;
    FRotation: Single;
    FAlignment: TAlignment;
    FLayout: TGLTextLayout;
    FModulateColor: TGLColor;

  protected
    { Protected Declarations }
    procedure SetBitmapFont(const val: TGLCustomBitmapFont);
    procedure SetText(const val: UnicodeString);
    procedure SetRotation(const val: Single);
    procedure SetAlignment(const val: TAlignment);
    procedure SetLayout(const val: TGLTextLayout);
    procedure SetModulateColor(const val: TGLColor);

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure RenderTextAtPosition(const X, Y, Z: Single;
      var rci: TRenderContextInfo);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoRender(var rci: TRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;

  published
    { Published Declarations }
    { : Refers the bitmap font to use.<p>
      The referred bitmap font component stores and allows access to
      individual character bitmaps. }
    property BitmapFont: TGLCustomBitmapFont read FBitmapFont
      write SetBitmapFont;
    { : Text to render.<p>
      Be aware that only the characters available in the bitmap font will
      be rendered. CR LF sequences are allowed. }
    property Text: UnicodeString read FText write SetText;
    { : Rotation angle in degrees (2d). }
    property Rotation: Single read FRotation write SetRotation;
    { : Controls the text alignment (horizontal).<p>
      Possible values : taLeftJustify, taRightJustify, taCenter }
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    { : Controls the text layout (vertical).<p>
      Possible values : tlTop, tlCenter, tlBottom }
    property Layout: TGLTextLayout read FLayout write SetLayout default tlTop;
    { : Color modulation, can be used for fade in/out too. }
    property ModulateColor: TGLColor read FModulateColor write SetModulateColor;
  end;

  { : Position (X, Y and X) is in absolute coordinates. This component converts
    them to screen coordinates and renderes text there. }
  TGLAbsoluteHUDText = class(TGLHUDText)
  public
    procedure DoRender(var rci: TRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
  end;

  { : Position (X and Y) is expected in a [0..1] range (from Screen size)
    This component converts this position to the actual screen position and
    renders the text there. This way a HUD text always appears to be in the
    the same place, regardless of the currect screen resolution.
    Note: this still does not solve the font scaling problem. }
  TGLResolutionIndependantHUDText = class(TGLHUDText)
  public
    procedure DoRender(var rci: TRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
    constructor Create(AOwner: TComponent); override;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses OpenGLTokens,
  GLContext,
  GLState,
  XOpenGL;

// ------------------
// ------------------ TGLHUDSprite ------------------
// ------------------

// Create
//

constructor TGLHUDSprite.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  Width := 16;
  Height := 16;
  FXTiles := 1;
  FYTiles := 1;
end;

// SetXTiles
//

procedure TGLHUDSprite.SetXTiles(const val: Integer);
begin
  if val <> FXTiles then
  begin
    FXTiles := val;
    StructureChanged;
  end;
end;

// SetYTiles
//

procedure TGLHUDSprite.SetYTiles(const val: Integer);
begin
  if val <> FYTiles then
  begin
    FYTiles := val;
    StructureChanged;
  end;
end;

// DoRender
//

procedure TGLHUDSprite.DoRender(var rci: TRenderContextInfo;
  renderSelf, renderChildren: Boolean);
var
  vx, vy, vx1, vy1, f: Single;
  u0, v0, u1, v1: Integer;
begin
  if rci.ignoreMaterials then
    Exit;
  Material.Apply(rci);
  repeat
    if AlphaChannel <> 1 then
    begin
      if stLighting in rci.GLStates.States then
        rci.GLStates.SetGLMaterialAlphaChannel(GL_FRONT, AlphaChannel)
      else
        with Material.GetActualPrimaryMaterial.FrontProperties.Diffuse do
          GL.Color4f(Red, Green, Blue, AlphaChannel);
    end;
    // Prepare matrices
    GL.MatrixMode(GL_MODELVIEW);
    GL.PushMatrix;
    GL.LoadMatrixf(@TGLSceneBuffer(rci.buffer).BaseProjectionMatrix);
    if rci.renderDPI = 96 then
      f := 1
    else
      f := rci.renderDPI / 96;
    GL.Scalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);
    GL.Translatef(f * Position.X - rci.viewPortSize.cx * 0.5,
      rci.viewPortSize.cy * 0.5 - f * Position.Y, Position.Z);
    if Rotation <> 0 then
      GL.Rotatef(Rotation, 0, 0, 1);
    GL.MatrixMode(GL_PROJECTION);
    GL.PushMatrix;
    GL.LoadIdentity;
    rci.GLStates.Disable(stDepthTest);
    rci.GLStates.DepthWriteMask := False;

    // precalc coordinates
    vx := -Width * 0.5 * f;
    vx1 := vx + Width * f;
    vy := +Height * 0.5 * f;
    vy1 := vy - Height * f;

    // Texture coordinates
    if MirrorU then
    begin
      u0 := FXTiles;
      u1 := 0;
    end
    else
    begin
      u0 := 0;
      u1 := FXTiles;
    end;

    if MirrorV then
    begin
      v0 := FYTiles;
      v1 := 0;
    end
    else
    begin
      v0 := 0;
      v1 := FYTiles;
    end;

    // issue quad
    GL.Begin_(GL_QUADS);
    GL.Normal3fv(@YVector);
    xgl.TexCoord2f(u0, v0);
    GL.Vertex2f(vx, vy1);
    xgl.TexCoord2f(u1, v0);
    GL.Vertex2f(vx1, vy1);
    xgl.TexCoord2f(u1, v1);
    GL.Vertex2f(vx1, vy);
    xgl.TexCoord2f(u0, v1);
    GL.Vertex2f(vx, vy);
    GL.End_;

    // restore state
    GL.PopMatrix;
    GL.MatrixMode(GL_MODELVIEW);
    GL.PopMatrix;
  until not Material.UnApply(rci);
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

// StoreHeight
//

function TGLHUDSprite.StoreHeight: Boolean;
begin
  Result := Abs(Height - 16) > 0.001;
end;

// StoreWidth
//

function TGLHUDSprite.StoreWidth: Boolean;
begin
  Result := Abs(Height - 16) > 0.001;
end;

// ------------------
// ------------------ TGLHUDText ------------------
// ------------------

// Create
//

constructor TGLHUDText.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FModulateColor := TGLColor.CreateInitialized(Self, clrWhite);
end;

// Destroy
//

destructor TGLHUDText.Destroy;
begin
  FModulateColor.Free;
  BitmapFont := nil;
  inherited;
end;

// Notification
//

procedure TGLHUDText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FBitmapFont) then
    BitmapFont := nil;
  inherited;
end;

// SetBitmapFont
//

procedure TGLHUDText.SetBitmapFont(const val: TGLCustomBitmapFont);
begin
  if val <> FBitmapFont then
  begin
    if Assigned(FBitmapFont) then
      FBitmapFont.UnRegisterUser(Self);
    FBitmapFont := val;
    if Assigned(FBitmapFont) then
    begin
      FBitmapFont.RegisterUser(Self);
      FBitmapFont.FreeNotification(Self);
    end;
    StructureChanged;
  end;
end;

// SetText
//

procedure TGLHUDText.SetText(const val: UnicodeString);
begin
  FText := val;
  StructureChanged;
end;

// SetRotation
//

procedure TGLHUDText.SetRotation(const val: Single);
begin
  FRotation := val;
  StructureChanged;
end;

// SetAlignment
//

procedure TGLHUDText.SetAlignment(const val: TAlignment);
begin
  FAlignment := val;
  StructureChanged;
end;

// SetLayout
//

procedure TGLHUDText.SetLayout(const val: TGLTextLayout);
begin
  FLayout := val;
  StructureChanged;
end;

// SetModulateColor
//

procedure TGLHUDText.SetModulateColor(const val: TGLColor);
begin
  FModulateColor.Assign(val);
end;

// RenderTextAtPosition
//

procedure TGLHUDText.RenderTextAtPosition(const X, Y, Z: Single;
  var rci: TRenderContextInfo);
var
  f: Single;
begin
  if Assigned(FBitmapFont) and (Text <> '') then
  begin
    rci.GLStates.PolygonMode := pmFill;
    // Prepare matrices
    GL.MatrixMode(GL_MODELVIEW);
    GL.PushMatrix;
    GL.LoadMatrixf(@TGLSceneBuffer(rci.buffer).BaseProjectionMatrix);
    f := rci.renderDPI / 96;
    GL.Scalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);
    GL.Translatef(X * f - rci.viewPortSize.cx / 2, rci.viewPortSize.cy / 2 -
      Y * f, Z);
    if FRotation <> 0 then
      GL.Rotatef(FRotation, 0, 0, 1);
    GL.Scalef(Scale.DirectX * f, Scale.DirectY * f, 1);
    GL.MatrixMode(GL_PROJECTION);
    GL.PushMatrix;
    GL.LoadIdentity;
    rci.GLStates.Disable(stDepthTest);
    // render text
    FBitmapFont.RenderString(rci, Text, FAlignment, FLayout,
      FModulateColor.Color);
    // restore state
    rci.GLStates.Enable(stDepthTest);
    GL.PopMatrix;
    GL.MatrixMode(GL_MODELVIEW);
    GL.PopMatrix;
  end;
end;

// DoRender
//

procedure TGLHUDText.DoRender(var rci: TRenderContextInfo;
  renderSelf, renderChildren: Boolean);
begin
  RenderTextAtPosition(Position.X, Position.Y, Position.Z, rci);
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

// ------------------
// ------------------ TGLResolutionIndependantHUDText ------------------
// ------------------

// Create
//

constructor TGLResolutionIndependantHUDText.Create(AOwner: TComponent);
begin
  inherited;
  Position.X := 0.5;
  Position.Y := 0.5;
end;

// DoRender
//

procedure TGLResolutionIndependantHUDText.DoRender(var rci: TRenderContextInfo;
  renderSelf, renderChildren: Boolean);
begin
  RenderTextAtPosition(Position.X * rci.viewPortSize.cx,
    Position.Y * rci.viewPortSize.cy, Position.Z, rci);
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

// ------------------
// ------------------ TGLAbsoluteHUDText ------------------
// ------------------

// DoRender
//

procedure TGLAbsoluteHUDText.DoRender(var rci: TRenderContextInfo;
  renderSelf, renderChildren: Boolean);
var
  Temp: TAffineVector;
begin
  Temp := TGLSceneBuffer(rci.buffer).WorldToScreen(Self.AbsoluteAffinePosition);
  Temp.V[1] := rci.viewPortSize.cy - Temp.V[1];
  RenderTextAtPosition(Temp.V[0], Temp.V[1], Temp.V[2], rci);
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// class registrations
RegisterClasses([TGLHUDText, TGLHUDSprite, TGLResolutionIndependantHUDText,
  TGLAbsoluteHUDText]);

end.
