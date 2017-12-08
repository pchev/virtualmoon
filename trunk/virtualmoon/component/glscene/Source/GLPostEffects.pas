//
// This unit is part of the GLScene Project, http://glscene.org
//
{
    A collection of components that generate post effects.

	 History :  
       23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
       22/04/10 - Yar - Fixes after GLState revision
       28/05/08 - DaStr - Fixed AV in TGLPostEffect.MakeDistortEffect()
                             Got rid of all R- hacks
       10/04/08 - DaStr - Added a Delpi 5 interface bug work-around to
                              TGLPostShaderCollectionItem.SetShader()
                              (BugTracker ID = 1938988)
       16/08/07 - DaStr - Added pepBlur preset (by Paul van Dinther)
       25/03/07 - DaStr - Small fix for Delphi5 compatibility
       23/03/07 - DaStr - Added TGLPostShaderHolder.Assign
       20/03/07 - DaStr - Fixed TGLPostShaderHolder.DoRender
       09/03/07 - DaStr - Added pepNightVision preset (thanks Roman Ganz)
                             Changed back all Trunc() calls to Round()
       07/03/07 - DaStr - Moved "Weird" effect to the demo
                             Added "Distort" effect
                             Modified "RedNoise" to simple monochrome noise
                                                   (preset renamed to "Noise")
                             Made "Negative" effect really negative,
                                             instead swapping R and B channels
                             Changed all Round() calls to Trunc()
                             Removed all TGLPostEffectColor typecasts
                             (All above changes were made by Michail Glukhov)
                             TGLPostEffect and TGLPostShaderHolder are not
                              rendered when DrawState=dsPicking (suggested by Riz)
       04/03/07 - DaStr - Added TGLPostShaderHolder
       02/03/07 - DaStr - TGLOnCustomPostEffectEvent now passes rci
                             pepNone preset does not call gl[Read/Draw]Pixels
       23/02/07 - DaStr - Initial version of TGLPostEffect
                                                (based on OldCity demo by FedeX)
                                                     

}
unit GLPostEffects;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes, SysUtils,


  GLScene, GLTexture, OpenGLTokens, GLGraphics, GLStrings, GLCustomShader,
  GLContext, GLVectorGeometry, GLRenderContextInfo, GLMaterial, GLTextureFormat;

type
  EGLPostShaderHolderException = class(Exception);
  TGLPostShaderHolder = class;

  TGLPostShaderCollectionItem = class(TCollectionItem)
  private
    FShader: TGLShader;
    FPostShaderInterface: IGLPostShader;
    procedure SetShader(const Value: TGLShader);
  protected
    function GetRealOwner: TGLPostShaderHolder;
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Shader: TGLShader read FShader write SetShader;
  end;

  TGLPostShaderCollection = class(TOwnedCollection)
  private
    function GetItems(const Index: Integer): TGLPostShaderCollectionItem;
    procedure SetItems(const Index: Integer;
      const Value: TGLPostShaderCollectionItem);
  public
    procedure Remove(const Item: TGLShader);
    function Add: TGLPostShaderCollectionItem;

    property Items[const Index: Integer]: TGLPostShaderCollectionItem read GetItems write SetItems; default;
  end;

  { A class that allows several post-shaders to be applied to the scene,
    one after another. It does not provide any optimizations related to
    multi-shader rendering, just a convenient interface. }
  TGLPostShaderHolder = class(TGLBaseSCeneObject)
  private
    FShaders: TGLPostShaderCollection;
    FTempTexture: TGLTextureHandle;
    FPreviousViewportSize: TGLSize;
    FTempTextureTarget: TGLTextureTarget;
    procedure SetShaders(const Value: TGLPostShaderCollection);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var rci : TGLRenderContextInfo;
                       renderSelf, renderChildren : Boolean); override;
  published
    property TempTextureTarget: TGLTextureTarget read FTempTextureTarget write FTempTextureTarget default ttTexture2d;
    property Shaders: TGLPostShaderCollection read FShaders write SetShaders;

    // Publish some stuff from TGLBaseSceneObject.
    property Visible;
    property OnProgress;
  end;


  TGLPostEffectColor = record
    R, G, B, A: TGLubyte;
  end;

  TGLPostEffectBuffer = array of TGLPostEffectColor;

  TGLOnCustomPostEffectEvent = procedure(Sender: TObject; var rci : TGLRenderContextInfo; var Buffer: TGLPostEffectBuffer) of object;

  { Some presets for TGLPostEffect:
       pepNone - does nothing.
       pepGray - makes picture gray.
       pepNegative - inverts all colors.
       pepDistort - simulates shaky TV image.
       pepNoise - just adds random niose.
       pepNightVision - simulates nightvision goggles.
       pepBlur - blurs the scene.
       pepCustom - calls the OnCustomEffect event.
  }
  TGLPostEffectPreset = (pepNone, pepGray, pepNegative, pepDistort, pepNoise,
                         pepNightVision, pepBlur, pepCustom);

  { Provides a simple way to producing post-effects without shaders.
     It is slow as hell, but it's worth it in some cases.}
  TGLPostEffect = class(TGLBaseSCeneObject)
  private
    FOnCustomEffect: TGLOnCustomPostEffectEvent;
    FPreset: TGLPostEffectPreset;
    FRenderBuffer: TGLPostEffectBuffer;
  protected
    // May be should be private...
    procedure MakeGrayEffect; virtual;
    procedure MakeNegativeEffect; virtual;
    procedure MakeDistortEffect; virtual;
    procedure MakeNoiseEffect; virtual;
    procedure MakeNightVisionEffect; virtual;
    procedure MakeBlurEffect(var rci : TGLRenderContextInfo); virtual;
    procedure DoOnCustomEffect(var rci : TGLRenderContextInfo; var Buffer: TGLPostEffectBuffer); virtual;
  public
    procedure DoRender(var rci : TGLRenderContextInfo;
                       renderSelf, renderChildren : Boolean); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Preset: TGLPostEffectPreset read FPreset write FPreset default pepNone;
    // User creates this effect.
    property OnCustomEffect: TGLOnCustomPostEffectEvent read FOnCustomEffect write FOnCustomEffect;
    // Publish some stuff from TGLBaseSCeneObject.
    property Visible;
    property OnProgress;
  end;

implementation

{ TGLPostEffect }

procedure TGLPostEffect.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TGLPostEffect then
  begin
    FPreset := TGLPostEffect(Source).FPreset;
  end;
end;

procedure TGLPostEffect.DoOnCustomEffect(
  var rci : TGLRenderContextInfo; var Buffer: TGLPostEffectBuffer);
begin
  if Assigned(FOnCustomEffect) then
    FOnCustomEffect(Self, rci, Buffer);
end;

procedure TGLPostEffect.DoRender(var rci : TGLRenderContextInfo;
                                      renderSelf, renderChildren : Boolean);
var
  NewScreenSize: Integer;
begin
  if (not rci.ignoreMaterials) and (FPreset <> pepNone) and (rci.drawState <> dsPicking) then
  begin
    NewScreenSize := rci.viewPortSize.cx * rci.viewPortSize.cy;
    if NewScreenSize <> Length(FRenderBuffer) then
      SetLength(FRenderBuffer, NewScreenSize);

    GL.ReadPixels(0, 0, rci.viewPortSize.cx, rci.viewPortSize.cy, GL_RGBA, GL_UNSIGNED_BYTE, FRenderBuffer);
    case FPreset of
      // pepNone is handled in the first line.
      pepGray:        MakeGrayEffect;
      pepNegative:    MakeNegativeEffect;
      pepDistort:     MakeDistortEffect;
      pepNoise:       MakeNoiseEffect;
      pepNightVision: MakeNightVisionEffect;
      pepBlur:        MakeBlurEffect(rci);
      pepCustom:      DoOnCustomEffect(rci, FRenderBuffer);
    else
      Assert(False, glsErrorEx + glsUnknownType);
    end;
    GL.DrawPixels(rci.viewPortSize.cx, rci.viewPortSize.cy, GL_RGBA, GL_UNSIGNED_BYTE, FRenderBuffer);
  end;

  // Start rendering children (if any).
  if renderChildren then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TGLPostEffect.MakeGrayEffect;
var
  I:    Longword;
  gray: TGLubyte;
begin
  for I := 0 to High(FRenderBuffer) do
  begin
    gray := Round((0.30 * FRenderBuffer[I].r) +
                  (0.59 * FRenderBuffer[I].g) +
                  (0.11 * FRenderBuffer[I].b));
    FRenderBuffer[I].r := gray;
    FRenderBuffer[I].g := gray;
    FRenderBuffer[I].b := gray;
  end;
end;

procedure TGLPostEffect.MakeNegativeEffect;
var
  I: Longword;
begin
  for I := 0 to High(FRenderBuffer) do
  begin
    FRenderBuffer[I].r := 255 - FRenderBuffer[I].r;
    FRenderBuffer[I].g := 255 - FRenderBuffer[I].g;
    FRenderBuffer[I].b := 255 - FRenderBuffer[I].b;
  end;
end;

procedure TGLPostEffect.MakeDistortEffect;
var
  I: Integer;
  lMaxLength: Integer;
  lNewIndex: Integer;
begin
  lMaxLength := High(FRenderBuffer);

  for I := 0 to lMaxLength do
  begin
    lNewIndex := MaxInteger(0, MinInteger(lMaxLength, I + Random(10) - 5));
    FRenderBuffer[I].r := FRenderBuffer[lNewIndex].r;
    FRenderBuffer[I].g := FRenderBuffer[lNewIndex].g;
    FRenderBuffer[I].b := FRenderBuffer[lNewIndex].b;
  end;
end;

procedure TGLPostEffect.MakeNoiseEffect;
var
  I:   Longword;
  rnd: Single;
begin
  for I := 0 to High(FRenderBuffer) do
  begin
    rnd := 0.25 + Random(75)/100;

    FRenderBuffer[I].r := Round(FRenderBuffer[I].r * rnd);
    FRenderBuffer[I].g := Round(FRenderBuffer[I].g * rnd);
    FRenderBuffer[I].b := Round(FRenderBuffer[I].b * rnd);
  end;
end;

procedure TGLPostEffect.MakeNightVisionEffect;
var
   gray: Single;
   I: Integer;
   lNewIndex, lMaxLength: Integer;
begin
  lMaxLength := High(FRenderBuffer);

  for I := 0 to lMaxLength do
  begin
    lNewIndex := MaxInteger(0, MinInteger(lMaxLength, I + Random(20) - 10));

    gray := 60 + (0.30 * FRenderBuffer[lNewIndex].r) +
                 (0.59 * FRenderBuffer[lNewIndex].g) +
                 (0.11 * FRenderBuffer[lNewIndex].b);

    FRenderBuffer[I].r := Round(gray * 0.25);
    FRenderBuffer[I].g := Round((gray + 4) * 0.6);
    FRenderBuffer[I].b := Round((gray + 4) * 0.11);
  end;
end;

procedure TGLPostEffect.MakeBlurEffect(var rci : TGLRenderContextInfo);
const
  lOffset: Integer = 2;
var
  I: Integer;
  lUp: Integer;
begin
  lUp := rci.viewPortSize.cx * lOffset;
  for I := lUp to High(FRenderBuffer) - lUp do
  begin
    FRenderBuffer[I].r := (FRenderBuffer[I].r + FRenderBuffer[I - lOffset].r +
        FRenderBuffer[I + lOffset].r + FRenderBuffer[I - lUp].r +
        FRenderBuffer[I + lUp].r) div 5;
    FRenderBuffer[I].g := (FRenderBuffer[I].g + FRenderBuffer[I - lOffset].g +
        FRenderBuffer[I + lOffset].g + FRenderBuffer[I - lUp].g +
        FRenderBuffer[I + lUp].r) div 5;
    FRenderBuffer[I].b := (FRenderBuffer[I].b + FRenderBuffer[I - lOffset].b +
        FRenderBuffer[I + lOffset].b + FRenderBuffer[I - lUp].g +
        FRenderBuffer[I + lUp].r) div 5;
  end;
end;

{ TGLPostShaderCollectionItem }

procedure TGLPostShaderCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TGLPostShaderCollectionItem then
  begin
    SetShader(TGLPostShaderCollectionItem(Source).FShader);
  end
  else
    inherited; // Die!!!
end;

function TGLPostShaderCollectionItem.GetDisplayName: string;
begin
  if FShader = nil then
    Result := ''
  else
  begin
    if FShader.Name <> '' then
      Result := FShader.Name
    else
      Result := FShader.ClassName;
  end;
end;

type
  // Required for Delphi5 compatibility.
  THackCollection = class(TOwnedCollection)end;

function TGLPostShaderCollectionItem.GetRealOwner: TGLPostShaderHolder;
begin
  if Collection = nil then
    Result := nil
  else
    Result := TGLPostShaderHolder(THackCollection(Collection).GetOwner);
end;

procedure TGLPostShaderCollectionItem.SetShader(const Value: TGLShader);
var
  RealOwner: TGLPostShaderHolder;
begin
  if FShader = Value then Exit;
  RealOwner := GetRealOwner;

  if FShader <> nil then
      FShader.RemoveFreeNotification(RealOwner);

  if not Supports(TObject(Value), IGLPostShader, FPostShaderInterface) then
    raise EGLPostShaderHolderException.Create('Shader must support interface IGLPostShader!');

  if RealOwner <> nil then
    if FPostShaderInterface.GetTextureTarget <> RealOwner.TempTextureTarget then
      raise EGLPostShaderHolderException.Create(glsErrorEx + 'TextureTarget is not compatible!');
  // If RealOwner = nil, we ignore this case and hope it will turn out ok...

  FShader := Value;

  if FShader <> nil then
    if RealOwner <> nil then
      FShader.FreeNotification(RealOwner);
end;

{ TGLPostShaderHolder }

procedure TGLPostShaderHolder.Assign(Source: TPersistent);
begin
  if Source is TGLPostShaderHolder then
  begin
    FShaders.Assign(TGLPostShaderHolder(Source).FShaders);
    FTempTextureTarget := TGLPostShaderHolder(Source).FTempTextureTarget;
  end;
  inherited;
end;

constructor TGLPostShaderHolder.Create(Owner: TComponent);
begin
  inherited;
  FTempTexture := TGLTextureHandle.Create;
  FTempTextureTarget :=ttTexture2D;
  FShaders := TGLPostShaderCollection.Create(Self, TGLPostShaderCollectionItem);
end;

destructor TGLPostShaderHolder.Destroy;
begin
  FShaders.Destroy;
  FTempTexture.Destroy;
  inherited;
end;

procedure TGLPostShaderHolder.DoRender(var rci: TGLRenderContextInfo;
  renderSelf, renderChildren: Boolean);
var
  I: Integer;
begin
  if not (rci.ignoreMaterials) and not (csDesigning in ComponentState) and
         (rci.drawState <> dsPicking) then
  begin
    if (FPreviousViewportSize.cx <> rci.viewPortSize.cx) or
       (FPreviousViewportSize.cy <> rci.viewPortSize.cy) then
    begin
      InitTexture(FTempTexture.Handle, rci.viewPortSize,
        FTempTextureTarget);
      FPreviousViewportSize := rci.viewPortSize;
    end;

    if FShaders.Count <> 0 then
    begin
      for I := 0 to FShaders.Count - 1 do
      begin
        Assert(Assigned(FShaders[I].FShader));
        if FShaders[I].FShader.Enabled then
        begin
          rci.GLStates.ActiveTextureEnabled[FTempTextureTarget] := True;
          FShaders[I].FShader.Apply(rci, Self);
          repeat
            CopyScreenToTexture(rci.viewPortSize, DecodeGLTextureTarget(FTempTextureTarget));
            FShaders[I].FPostShaderInterface.DoUseTempTexture(FTempTexture, FTempTextureTarget);
            DrawTexturedScreenQuad5(rci.viewPortSize);
          until not FShaders[I].FShader.UnApply(rci);
          rci.GLStates.ActiveTextureEnabled[FTempTextureTarget] := False;
        end;
      end;
    end;
  end;
  if renderChildren then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TGLPostShaderHolder.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent is TGLShader then
      FShaders.Remove(TGLShader(AComponent));
  end;
end;

procedure TGLPostShaderHolder.SetShaders(
  const Value: TGLPostShaderCollection);
begin
  FShaders.Assign(Value);
end;

{ TGLPostShaderCollection }

function TGLPostShaderCollection.Add: TGLPostShaderCollectionItem;
begin
  Result := TGLPostShaderCollectionItem(inherited Add);
end;

function TGLPostShaderCollection.GetItems(
  const Index: Integer): TGLPostShaderCollectionItem;
begin
  Result := TGLPostShaderCollectionItem(GetItem(Index));
end;

procedure TGLPostShaderCollection.Remove(
  const Item: TGLShader);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := Count - 1 downto 0 do
      if GetItems(I).FShader = Item then
        Delete(I);
  // Don't exit because the same shader might be applied more than once.
end;

procedure TGLPostShaderCollection.SetItems(const Index: Integer;
  const Value: TGLPostShaderCollectionItem);
begin
  GetItems(Index).Assign(Value);
end;

initialization
  RegisterClasses([TGLPostEffect, TGLPostShaderHolder,
                   TGLPostShaderCollection, TGLPostShaderCollectionItem]);

end.
