//
// This unit is part of the GLScene Project, http://glscene.org
//
{
    This shader allows to apply multiple textures, gathering them from existing materials.
    This allows saving resources, since you can reference the textures of any material in
    any materialLibrary.
    Note that actually the component references a Material (not a texture) but
    it uses that material's texture. The referenced material settings will be ignored,
    but the texture's settings (like TextureMode, ImageGamma, ImageBrightness) will be used.
    Instead the local material settings (listed in the collection) will be used.
    </p>

   History :  
       16/03/11 - Yar - Fixes after emergence of GLMaterialEx
       23/08/10 - Yar - Fixed light state changes
       22/04/10 - Yar - Fixes after GLState revision
       05/03/10 - DanB - More state added to TGLStateCache
       10/04/08 - DaStr - Added a Delpi 5 interface bug work-around
                              (BugTracker ID = 1938988).
                             TGLTextureSharingShaderMaterial.GetTextureSharingShader()
                              is now more safe
       24/03/08 - DaStr - Small fixups with setting LibMaterial and for
                               Delphi 5 compatibility (thanks Pascal)
       21/03/08 - DaStr - Reformated according to VCL standard, made some renamings
       17/03/08 - mrqzzz - Added IGLMaterialLibrarySupported, moved registration
       14/03/08 - Pascal - Initial version (contributed to GLScene)

}

unit GLTextureSharingShader;

interface

uses
  // VCL
  Classes, SysUtils,

  GLScene, GLVectorGeometry, GlColor, GLMaterial, GLStrings,
  GLVectorFileObjects, XOpenGL, GLState, GLPersistentClasses,
  {Needed for Delphi 5} GlCrossPlatform, GLCoordinates, GLRenderContextInfo;

type
  TGLTextureSharingShader = class;

  TGLTextureSharingShaderMaterial = class(TGLInterfacedCollectionItem, IGLMaterialLibrarySupported)
  private
    FTextureMatrix: TMatrix;
    FNeedToUpdateTextureMatrix: Boolean;
    FTextureMatrixIsUnitary: Boolean;

    FLibMaterial: TGLLibMaterial;
    FTexOffset: TGLCoordinates2;
    FTexScale: TGLCoordinates2;
    FBlendingMode: TBlendingMode;
    FSpecular: TGLColor;
    FAmbient: TGLColor;
    FDiffuse: TGLColor;
    FEmission: TGLColor;
    FShininess: TShininess;
    FMaterialLibrary: TGLMaterialLibrary;
    FLibMaterialName: TGLLibMaterialName;

    procedure SetAmbient(const Value: TGLColor);
    procedure SetDiffuse(const Value: TGLColor);
    procedure SetEmission(const Value: TGLColor);
    procedure SetShininess(const Value: TShininess);
    procedure SetSpecular(const Value: TGLColor);
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    procedure SetLibMaterialName(const Value: TGLLibMaterialName);
    procedure SetBlendingMode(const Value: TBlendingMode);
    procedure SetLibMaterial(const Value: TGLLibMaterial);
    procedure SetTexOffset(const Value: TGLCoordinates2);
    procedure SetTexScale(const Value: TGLCoordinates2);

    function GetTextureMatrix: TMatrix;
    function GetTextureMatrixIsUnitary: Boolean;
  protected
    procedure coordNotifychange(Sender: TObject);
    procedure OtherNotifychange(Sender: TObject);

    function GetDisplayName: string; override;
    function GetTextureSharingShader: TGLTextureSharingShader;

    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TGLAbstractMaterialLibrary; virtual;

  public
    procedure Apply(var rci: TGLRenderContextInfo);
    procedure UnApply(var rci: TGLRenderContextInfo);
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property LibMaterial: TGLLibMaterial read FLibMaterial write SetLibMaterial;

    property TextureMatrix: TMatrix read GetTextureMatrix;
    property TextureMatrixIsUnitary: Boolean read GetTextureMatrixIsUnitary;
  published

    property TexOffset: TGLCoordinates2 read FTexOffset write SetTexOffset;
    property TexScale: TGLCoordinates2 read FTexScale write SetTexScale;
    property BlendingMode: TBlendingMode read FBlendingMode write SetBlendingMode;
    property Emission: TGLColor read FEmission write SetEmission;
    property Ambient: TGLColor read FAmbient write SetAmbient;
    property Diffuse: TGLColor read FDiffuse write SetDiffuse;
    property Specular: TGLColor read FSpecular write SetSpecular;
    property Shininess: TShininess read FShininess write SetShininess;
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property LibMaterialName: TGLLibMaterialName read FLibMaterialName write SetLibMaterialName;
  end;

  TGLTextureSharingShaderMaterials = class(TOwnedCollection)
  protected
    function GetItems(const AIndex: Integer): TGLTextureSharingShaderMaterial;
    procedure SetItems(const AIndex: Integer; const Value: TGLTextureSharingShaderMaterial);
    function GetParent: TGLTextureSharingShader;
  public
    function Add: TGLTextureSharingShaderMaterial;
    constructor Create(AOwner: TGLTextureSharingShader);
    property Items[const AIndex: Integer]: TGLTextureSharingShaderMaterial read GetItems write SetItems; default;
  end;

  TGLTextureSharingShader = class(TGLShader)
  private
    FMaterials: TGLTextureSharingShaderMaterials;
    FCurrentPass: Integer;
    procedure SetMaterials(const Value: TGLTextureSharingShaderMaterials);
  protected
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddLibMaterial(const ALibMaterial: TGLLibMaterial): TGLTextureSharingShaderMaterial;
    function FindLibMaterial(const ALibMaterial: TGLLibMaterial): TGLTextureSharingShaderMaterial;
  published
    property Materials: TGLTextureSharingShaderMaterials read FMaterials write SetMaterials;
  end;



implementation

{ TGLTextureSharingShaderMaterial }

procedure TGLTextureSharingShaderMaterial.Apply(var rci: TGLRenderContextInfo);
begin
  if not Assigned(FLibMaterial) then
    Exit;
  xgl.BeginUpdate;
  if Assigned(FLibMaterial.Shader) then
  begin
    case FLibMaterial.Shader.ShaderStyle of
      ssHighLevel: FLibMaterial.Shader.Apply(rci, FLibMaterial);
      ssReplace:
      begin
        FLibMaterial.Shader.Apply(rci, FLibMaterial);
        Exit;
      end;
    end;
  end;
  if not FLibMaterial.Material.Texture.Disabled then
  begin
    if not (GetTextureMatrixIsUnitary) then
    begin
      rci.GLStates.SetGLTextureMatrix(TextureMatrix);
    end;
  end;

  if moNoLighting in FLibMaterial.Material.MaterialOptions then
    rci.GLStates.Disable(stLighting);

  if stLighting in rci.GLStates.States then
  begin
    rci.GLStates.SetGLMaterialColors(cmFront,
      Emission.Color, Ambient.Color, Diffuse.Color, Specular.Color, Shininess);
    rci.GLStates.PolygonMode :=FLibMaterial.Material.PolygonMode;
  end
  else
    FLibMaterial.Material.FrontProperties.ApplyNoLighting(rci, cmFront);
  if (stCullFace in rci.GLStates.States) then
  begin
    case FLibMaterial.Material.FaceCulling of
      fcBufferDefault: if not rci.bufferFaceCull then
        begin
          rci.GLStates.Disable(stCullFace);
          FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
        end;
      fcCull: ; // nothing to do
      fcNoCull:
      begin
        rci.GLStates.Disable(stCullFace);
        FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
      end;
      else
        Assert(False);
    end;
  end
  else
  begin
    // currently NOT culling
    case FLibMaterial.Material.FaceCulling of
      fcBufferDefault:
      begin
        if rci.bufferFaceCull then
          rci.GLStates.Enable(stCullFace)
        else
          FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
      end;
      fcCull: rci.GLStates.Enable(stCullFace);
      fcNoCull: FLibMaterial.Material.BackProperties.Apply(rci, cmBack);
      else
        Assert(False);
    end;
  end;

  // Apply Blending mode
  if not rci.ignoreBlendingRequests then
    case BlendingMode of
      bmOpaque:
      begin
        rci.GLStates.Disable(stBlend);
        rci.GLStates.Disable(stAlphaTest);
      end;
      bmTransparency:
      begin
        rci.GLStates.Enable(stBlend);
        rci.GLStates.Enable(stAlphaTest);
        rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      end;
      bmAdditive:
      begin
        rci.GLStates.Enable(stBlend);
        rci.GLStates.Enable(stAlphaTest);
        rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOne);
      end;
      bmAlphaTest50:
      begin
        rci.GLStates.Disable(stBlend);
        rci.GLStates.Enable(stAlphaTest);
        rci.GLStates.SetGLAlphaFunction(cfGEqual, 0.5);
      end;
      bmAlphaTest100:
      begin
        rci.GLStates.Disable(stBlend);
        rci.GLStates.Enable(stAlphaTest);
        rci.GLStates.SetGLAlphaFunction(cfGEqual, 1.0);
      end;
      bmModulate:
      begin
        rci.GLStates.Enable(stBlend);
        rci.GLStates.Enable(stAlphaTest);
        rci.GLStates.SetBlendFunc(bfDstColor, bfZero);
      end;
      else
        Assert(False);
    end;
  // Fog switch
  if moIgnoreFog in FLibMaterial.Material.MaterialOptions then
  begin
    if stFog in rci.GLStates.States then
    begin
      rci.GLStates.Disable(stFog);
      Inc(rci.fogDisabledCounter);
    end;
  end;

  if not Assigned(FLibMaterial.Material.TextureEx) then
  begin
    if Assigned(FLibMaterial.Material.Texture) then
      FLibMaterial.Material.Texture.Apply(rci);
  end
  else
  begin
    if Assigned(FLibMaterial.Material.Texture) and not FLibMaterial.Material.TextureEx.IsTextureEnabled(0) then
      FLibMaterial.Material.Texture.Apply(rci)
    else
    if FLibMaterial.Material.TextureEx.Count > 0 then
      FLibMaterial.Material.TextureEx.Apply(rci);
  end;

  if Assigned(FLibMaterial.Shader) then
  begin
    case FLibMaterial.Shader.ShaderStyle of
      ssLowLevel: FLibMaterial.Shader.Apply(rci, FLibMaterial);
    end;
  end;
  xgl.EndUpdate;
end;

procedure TGLTextureSharingShaderMaterial.coordNotifychange(Sender: TObject);
begin
  FNeedToUpdateTextureMatrix := True;
  GetTextureSharingShader.NotifyChange(Self);
end;

constructor TGLTextureSharingShaderMaterial.Create(Collection: TCollection);
begin
  inherited;
  FSpecular := TGLColor.Create(Self);
  FSpecular.OnNotifyChange := OtherNotifychange;
  FAmbient := TGLColor.Create(Self);
  FAmbient.OnNotifyChange := OtherNotifychange;
  FDiffuse := TGLColor.Create(Self);
  FDiffuse.OnNotifyChange := OtherNotifychange;
  FEmission := TGLColor.Create(Self);
  FEmission.OnNotifyChange := OtherNotifychange;

  FTexOffset := TGLCoordinates2.CreateInitialized(Self, NullHmgVector, csPoint2d);
  FTexOffset.OnNotifyChange := coordNotifychange;

  FTexScale := TGLCoordinates2.CreateInitialized(Self, XYZHmgVector, csPoint2d);
  FTexScale.OnNotifyChange := coordNotifychange;
  FNeedToUpdateTextureMatrix := True;
end;

destructor TGLTextureSharingShaderMaterial.Destroy;
begin
  FSpecular.Free;
  FAmbient.Free;
  FDiffuse.Free;
  FEmission.Free;
  FTexOffset.Free;
  FTexScale.Free;
  inherited;
end;


function TGLTextureSharingShaderMaterial.GetDisplayName: string;
var
  st: string;
begin
  if Assigned(MaterialLibrary) then
    st := MaterialLibrary.Name
  else
    st := '';
  Result := '[' + st + '.' + Self.LibMaterialName + ']';
end;

function TGLTextureSharingShaderMaterial.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

function TGLTextureSharingShaderMaterial.GetTextureMatrix: TMatrix;
begin
  if FNeedToUpdateTextureMatrix then
  begin
    if not (TexOffset.Equals(NullHmgVector) and TexScale.Equals(XYZHmgVector)) then
    begin
      FTextureMatrixIsUnitary := False;
      FTextureMatrix := CreateScaleAndTranslationMatrix(TexScale.AsVector, TexOffset.AsVector)
    end  
    else
      FTextureMatrixIsUnitary := True;
    FNeedToUpdateTextureMatrix := False;
  end;
  Result := FTextureMatrix;
end;

function TGLTextureSharingShaderMaterial.GetTextureMatrixIsUnitary: Boolean;
begin
  if FNeedToUpdateTextureMatrix then
    GetTextureMatrix;
  Result := FTextureMatrixIsUnitary;
end;

function TGLTextureSharingShaderMaterial.GetTextureSharingShader: TGLTextureSharingShader;
begin
  if Collection is TGLTextureSharingShaderMaterials then
    Result := TGLTextureSharingShaderMaterials(Collection).GetParent
  else
    Result := nil;
end;

procedure TGLTextureSharingShaderMaterial.OtherNotifychange(Sender: TObject);
begin
  GetTextureSharingShader.NotifyChange(Self);
end;

procedure TGLTextureSharingShaderMaterial.SetAmbient(const Value: TGLColor);
begin
  FAmbient.Assign(Value);
end;

procedure TGLTextureSharingShaderMaterial.SetBlendingMode(const Value: TBlendingMode);
begin
  FBlendingMode := Value;
end;

procedure TGLTextureSharingShaderMaterial.SetDiffuse(const Value: TGLColor);
begin
  FDiffuse.Assign(Value);
end;

procedure TGLTextureSharingShaderMaterial.SetEmission(const Value: TGLColor);
begin
  FEmission.Assign(Value);
end;

procedure TGLTextureSharingShaderMaterial.SetLibMaterialName(const Value: TGLLibMaterialName);
begin
  FLibMaterialName := Value;
  if (FLibMaterialName = '') or (FMaterialLibrary = nil) then
    FLibMaterial := nil
  else
    SetLibMaterial(FMaterialLibrary.LibMaterialByName(FLibMaterialName));
end;

procedure TGLTextureSharingShaderMaterial.SetLibMaterial(const Value: TGLLibMaterial);
begin
  FLibMaterial := Value;
  if FLibMaterial <> nil then
  begin
    FLibMaterialName := FLibMaterial.DisplayName;
    FMaterialLibrary := TGLMaterialLibrary(TGLLibMaterials(Value.Collection).Owner);
    if not (csloading in GetTextureSharingShader.ComponentState) then
    begin
      FTexOffset.Assign(FLibMaterial.TextureOffset);
      FTexScale.Assign(FLibMaterial.TextureScale);
      FBlendingMode := FLibMaterial.Material.BlendingMode;
      fEmission.Assign(FLibMaterial.Material.FrontProperties.Emission);
      fAmbient.Assign(FLibMaterial.Material.FrontProperties.Ambient);
      fDiffuse.Assign(FLibMaterial.Material.FrontProperties.Diffuse);
      fSpecular.Assign(FLibMaterial.Material.FrontProperties.Specular);
      fShininess := FLibMaterial.Material.FrontProperties.Shininess;
    end;
  end;
end;


procedure TGLTextureSharingShaderMaterial.SetMaterialLibrary(const Value: TGLMaterialLibrary);
begin
  FMaterialLibrary := Value;
  if (FLibMaterialName = '') or (FMaterialLibrary = nil) then
    FLibMaterial := nil
  else
    SetLibMaterial(FMaterialLibrary.LibMaterialByName(FLibMaterialName));
end;

procedure TGLTextureSharingShaderMaterial.SetShininess(const Value: TShininess);
begin
  FShininess := Value;
end;

procedure TGLTextureSharingShaderMaterial.SetSpecular(const Value: TGLColor);
begin
  FSpecular.Assign(Value);
end;

procedure TGLTextureSharingShaderMaterial.SetTexOffset(const Value: TGLCoordinates2);
begin
  FTexOffset.Assign(Value);
  FNeedToUpdateTextureMatrix := True;
end;

procedure TGLTextureSharingShaderMaterial.SetTexScale(const Value: TGLCoordinates2);
begin
  FTexScale.Assign(Value);
  FNeedToUpdateTextureMatrix := True;
end;

procedure TGLTextureSharingShaderMaterial.UnApply(var rci: TGLRenderContextInfo);
begin
  if not Assigned(FLibMaterial) then
    Exit;

  if Assigned(FLibMaterial.Shader) then
  begin
    case FLibMaterial.Shader.ShaderStyle of
      ssLowLevel: FLibMaterial.Shader.UnApply(rci);
      ssReplace:
      begin
        FLibMaterial.Shader.UnApply(rci);
        Exit;
      end;
    end;
  end;

  FLibMaterial.Material.UnApply(rci);

  if not FLibMaterial.Material.Texture.Disabled then
    if not (GetTextureMatrixIsUnitary) then
    begin
      rci.GLStates.ResetGLTextureMatrix;
    end;

  if Assigned(FLibMaterial.Shader) then
  begin
    case FLibMaterial.Shader.ShaderStyle of
      ssHighLevel: FLibMaterial.Shader.UnApply(rci);
    end;
  end;
end;

{ TGLTextureSharingShader }

function TGLTextureSharingShader.AddLibMaterial(const ALibMaterial: TGLLibMaterial): TGLTextureSharingShaderMaterial;
begin
  Result := FMaterials.Add;
  Result.SetLibMaterial(ALibMaterial);
end;

constructor TGLTextureSharingShader.Create(AOwner: TComponent);
begin
  inherited;
  FMaterials := TGLTextureSharingShaderMaterials.Create(Self);
  ShaderStyle := ssReplace;
end;

destructor TGLTextureSharingShader.Destroy;
begin
  FMaterials.Free;
  inherited;
end;

procedure TGLTextureSharingShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
begin
  if Materials.Count > 0 then
  begin
    rci.GLStates.Enable(stDepthTest);
    rci.GLStates.DepthFunc := cfLEqual;
    Materials[0].Apply(rci);
    FCurrentPass := 1;
  end;
end;

function TGLTextureSharingShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  Result := False;
  if Materials.Count > 0 then
  begin
    Materials[FCurrentPass - 1].UnApply(rci);
    if FCurrentPass < Materials.Count then
    begin
      Materials[FCurrentPass].Apply(rci);
      Inc(FCurrentPass);
      Result := True;
    end
    else
    begin
      rci.GLStates.DepthFunc := cfLess;
      rci.GLStates.Disable(stBlend);
      rci.GLStates.Disable(stAlphaTest);
      FCurrentPass := 0;
    end;
  end;
end;

function TGLTextureSharingShader.FindLibMaterial(const ALibMaterial: TGLLibMaterial): TGLTextureSharingShaderMaterial;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FMaterials.Count - 1 do
    if FMaterials[I].FLibMaterial = ALibMaterial then
    begin
      Result := FMaterials[I];
      Break;
    end;
end;

procedure TGLTextureSharingShader.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent is TGLMaterialLibrary then
    begin
      for I := 0 to Materials.Count - 1 do
      begin
        if Materials.Items[I].MaterialLibrary = AComponent then
          Materials.Items[I].MaterialLibrary := nil;
      end;
    end;
  end;
end;

procedure TGLTextureSharingShader.SetMaterials(const Value: TGLTextureSharingShaderMaterials);
begin
  FMaterials.Assign(Value);
end;

{ TGLTextureSharingShaderMaterials }

function TGLTextureSharingShaderMaterials.Add: TGLTextureSharingShaderMaterial;
begin
  Result := (inherited Add) as TGLTextureSharingShaderMaterial;
end;

constructor TGLTextureSharingShaderMaterials.Create(AOwner: TGLTextureSharingShader);
begin
  inherited Create(AOwner, TGLTextureSharingShaderMaterial);
end;

function TGLTextureSharingShaderMaterials.GetItems(const AIndex: Integer): TGLTextureSharingShaderMaterial;
begin
  Result := (inherited Items[AIndex]) as TGLTextureSharingShaderMaterial;
end;

function TGLTextureSharingShaderMaterials.GetParent: TGLTextureSharingShader;
begin
  Result := TGLTextureSharingShader(GetOwner);
end;

procedure TGLTextureSharingShaderMaterials.SetItems(const AIndex: Integer; const Value: TGLTextureSharingShaderMaterial);
begin
  inherited Items[AIndex] := Value;
end;


initialization
  RegisterClasses([TGLTextureSharingShader, TGLTextureSharingShaderMaterials,
                   TGLTextureSharingShaderMaterial]);

end.
