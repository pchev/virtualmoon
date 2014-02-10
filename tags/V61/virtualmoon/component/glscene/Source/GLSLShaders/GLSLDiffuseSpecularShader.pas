//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSLDiffuseSpecularShader<p>

    This is a collection of GLSL diffuse-specular shaders.<p>

	<b>History : </b><font size=-1><ul>
      <li>16/03/11 - Yar - Fixes after emergence of GLMaterialEx
      <li>23/10/10 - Yar - Bugfixed memory leak
      <li>23/08/10 - Yar - Replaced OpenGL1x to OpenGLTokens
      <li>07/01/10 - DaStr - Bugfixed all DoInitialize() calls
                              (thanks YarUnderoaker)  
      <li>25/07/09 - DaStr - Fixed a bug with "dot(reflect_vec, LightVector)" clamping
                              which occured on all GeForce 8x and later graphic cards
      <li>24/07/09 - DaStr - Added Fog support for single-light shaders and fixed
                              a bug with material Alpha (thanks Controller)
      <li>02/09/07 - LC - Fixed texture bug in TGLSLMLDiffuseSpecularShader.
                          (Bugtracker ID = 1786286)
      <li>03/04/07 - LC - Shader didn't respect the texture matrix. Changed
                          vertex shader to fix this. (Bugtracker ID = 1693389)
      <li>20/03/07 - DaStr - Made changes related to the new parameter passing model
      <li>06/03/07 - DaStr - Again replaced DecimalSeparator stuff with
                              a single Str procedure (thanks Uwe Raabe)
      <li>03/03/07 - DaStr - Made compatible with Delphi6
                             Added more stuff to RegisterClasses()
      <li>21/02/07 - DaStr - Initial version (contributed to GLScene)


    This is a collection of GLSL Diffuse Specular shaders, comes in these variaties
              (to know what these suffixes and prefixes mean see GLCustomShader.pas):
      - TGLSLDiffuseSpecularShader
      - TGLSLDiffuseSpecularShaderMT
      - TGLSLDiffuseSpecularShaderAM

      - TGLSLMLDiffuseSpecularShader
      - TGLSLMLDiffuseSpecularShaderMT

    Notes:
     1) Alpha is a synthetic property, in real life your should set each
      color's Alpha individualy
     2) TGLSLDiffuseSpecularShader takes all Light parameters directly
      from OpenGL (that includes TGLLightSource's)


    Previous version history:
      v1.0    01 November  '2006  Creation
      v1.1    19 December  '2006  TGLBaseCustomGLSLDiffuseSpecular[MT] abstracted
                                  5 different versions of this shader added
      v1.1.2  06 February  '2007  IGLMaterialLibrarySupported renamed to
                                   IGLMaterialLibrarySupported
      v1.2    16 February  '2007  Updated to the latest CVS version of GLScene

}
unit GLSLDiffuseSpecularShader;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes, SysUtils,

  // GLScene
  GLTexture, GLScene, VectorGeometry, OpenGLTokens, GLStrings, GLCustomShader,
  GLSLShader, GLColor, GLRenderContextInfo, GLMaterial;

type
  EGLSLDiffuseSpecularShaderException = class(EGLSLShaderException);

  //: Abstract class.
  TGLBaseCustomGLSLDiffuseSpecular = class(TGLCustomGLSLShader)
  private
    FSpecularPower: Single;
    FLightPower: Single;
    FRealisticSpecular: Boolean;
    FFogSupport: TGLShaderFogSupport;
    procedure SetRealisticSpecular(const Value: Boolean);
    procedure SetFogSupport(const Value: TGLShaderFogSupport);
  protected
    procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    property SpecularPower: Single read FSpecularPower write FSpecularPower;
    property LightPower: Single read FLightPower write FLightPower;
    property RealisticSpecular: Boolean read FRealisticSpecular write SetRealisticSpecular;

    //: User can disable fog support and save some FPS if he doesn't need it.
    property FogSupport: TGLShaderFogSupport read FFogSupport write SetFogSupport default sfsAuto;
  end;

  //: Abstract class.
  TGLBaseGLSLDiffuseSpecularShaderMT = class(TGLBaseCustomGLSLDiffuseSpecular, IGLMaterialLibrarySupported)
  private
    FMaterialLibrary: TGLMaterialLibrary;
    FMainTexture: TGLTexture;
    FMainTextureName: TGLLibMaterialName;
    function GetMainTextureName: TGLLibMaterialName;
    procedure SetMainTextureName(const Value: TGLLibMaterialName);
    //: Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
  protected
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
  public
    property MainTexture: TGLTexture read FMainTexture write FMainTexture;
    property MainTextureName: TGLLibMaterialName read GetMainTextureName write SetMainTextureName;
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
  end;

                     {********  Single Light  ************}

  TGLCustomGLSLDiffuseSpecularShaderAM = class(TGLBaseGLSLDiffuseSpecularShaderMT)
  private
    FAmbientColor: TGLColor;
    FDiffuseColor: TGLColor;
    FSpecularColor: TGLColor;

    function GetAlpha: Single;
    procedure SetAlpha(const Value: Single);
  protected
    procedure DoInitialize(var rci: TRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property AmbientColor: TGLColor read FAmbientColor;
    property DiffuseColor: TGLColor read FDiffuseColor;
    property SpecularColor: TGLColor read FSpecularColor;

    property Alpha: Single read GetAlpha write SetAlpha;
  end;

  TGLCustomGLSLDiffuseSpecularShader = class(TGLBaseCustomGLSLDiffuseSpecular)
  protected
    procedure DoInitialize(var rci: TRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
  end;


  TGLCustomGLSLDiffuseSpecularShaderMT = class(TGLBaseGLSLDiffuseSpecularShaderMT)
  protected
    procedure DoInitialize(var rci: TRenderContextInfo; Sender: TObject); override;
  end;

                     {********  Multi Light  ************}

  {: Note: probably LightCount should be replaced by LightSources, like in
     GLSLBumpShader.pas }

  TGLCustomGLSLMLDiffuseSpecularShader = class(TGLBaseCustomGLSLDiffuseSpecular)
  private
    FLightCount: Integer;
    FLightCompensation: Single;
    procedure SetLightCount(const Value: Integer);
    procedure SetLightCompensation(const Value: Single);
  protected
    procedure DoInitialize(var rci: TRenderContextInfo; Sender: TObject); override;
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;

  public
    constructor Create(AOwner : TComponent); override;

    property LightCount: Integer read FLightCount write SetLightCount default 1;
    {: Setting LightCompensation to a value less than 1 decreeses individual
       light intensity when using multiple lights }
    property LightCompensation: Single read FLightCompensation write SetLightCompensation;
  end;

  TGLCustomGLSLMLDiffuseSpecularShaderMT = class(TGLBaseGLSLDiffuseSpecularShaderMT)
  private
    FLightCount: Integer;
    FLightCompensation: Single;
    procedure SetLightCount(const Value: Integer);
    procedure SetLightCompensation(const Value: Single);
  protected
    procedure DoInitialize(var rci: TRenderContextInfo; Sender: TObject); override;
  public
    constructor Create(AOwner : TComponent); override;

    property LightCount: Integer read FLightCount write SetLightCount default 1;
    {: Setting LightCompensation to a value less than 1 decreeses individual
       light intensity when using multiple lights }
    property LightCompensation: Single read FLightCompensation write SetLightCompensation;
  end;


                     {********  Published Stuff  ************}

  TGLSLDiffuseSpecularShaderAM = class(TGLCustomGLSLDiffuseSpecularShaderAM)
  published
    property AmbientColor;
    property DiffuseColor;
    property SpecularColor;
    property Alpha stored False;

    property MainTexture;

    property SpecularPower;
    property LightPower;
    property FogSupport;
  end;

  TGLSLDiffuseSpecularShaderMT = class(TGLCustomGLSLDiffuseSpecularShaderMT)
  published
    property MainTextureName;

    property SpecularPower;
    property LightPower;
    property FogSupport;
  end;

  TGLSLDiffuseSpecularShader = class(TGLCustomGLSLDiffuseSpecularShader)
  published
    property SpecularPower;
    property LightPower;
    property FogSupport;
  end;


  TGLSLMLDiffuseSpecularShaderMT = class(TGLCustomGLSLMLDiffuseSpecularShaderMT)
  published
    property MainTextureName;

    property SpecularPower;
    property LightPower;
    property LightCount;
    property LightCompensation;
    property FogSupport;
  end;

  TGLSLMLDiffuseSpecularShader = class(TGLCustomGLSLMLDiffuseSpecularShader)
  published
    property SpecularPower;
    property LightPower;
    property LightCount;
    property LightCompensation;
    property FogSupport;
  end;

implementation

procedure GetVertexProgramCode(const Code: TStrings;
  const AFogSupport: Boolean; var rci: TRenderContextInfo);
begin
  with Code do
  begin
    Clear;
    Add('varying vec3 Normal; ');
    Add('varying vec3 LightVector; ');
    Add('varying vec3 CameraVector; ');

    if AFogSupport then
    begin
      Add('varying float fogFactor; ');
    end;

    Add(' ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add('  gl_Position = ftransform(); ');
    Add('  gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0; ');
    Add('  Normal = normalize(gl_NormalMatrix * gl_Normal); ');
    Add('  vec3 p = (gl_ModelViewMatrix * gl_Vertex).xyz; ');
    Add('  LightVector = normalize(gl_LightSource[0].position.xyz - p); ');
    Add('  CameraVector = normalize(p); ');

    if AFogSupport then
    begin
    Add('  const float LOG2 = 1.442695; ');
    Add('  gl_FogFragCoord = length(p); ');

    case TGLSceneBuffer(rci.buffer).FogEnvironment.FogMode of
      fmLinear:
        Add('  fogFactor = (gl_Fog.end - gl_FogFragCoord) * gl_Fog.scale; ');
      fmExp, // Yep, I don't know about this type, so I use fmExp2.
      fmExp2:
      begin
        Add('  fogFactor = exp2( -gl_Fog.density * ');
        Add('  				   gl_Fog.density * ');
        Add('  				   gl_FogFragCoord * ');
        Add('  				   gl_FogFragCoord * ');
        Add('  				   LOG2 ); ');
      end;
    else
      Assert(False, glsUnknownType);
    end;

      Add('  fogFactor = clamp(fogFactor, 0.0, 1.0); ');
    end;

    Add('} ');
  end;
end;

procedure GetFragmentProgramCode(const Code: TStrings; const ARealisticSpecular: Boolean;
  const AFogSupport: Boolean);
begin
  with Code do
  begin
    Clear;
    Add('uniform float LightIntensity; ');
    Add('uniform float SpecPower; ');
    Add('uniform sampler2D MainTexture; ');
    Add(' ');
    Add('varying vec3 Normal; ');
    Add('varying vec3 LightVector; ');
    Add('varying vec3 CameraVector; ');

    if AFogSupport then
    begin
      Add('varying float fogFactor; ');
    end;

    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add('  vec4 TextureContrib = texture2D(MainTexture, gl_TexCoord[0].xy); ');
    Add('  vec4 DiffuseContrib = clamp(gl_LightSource[0].diffuse * dot(LightVector, Normal), 0.0, 1.0); ');
    Add(' ');
    Add('  vec3 reflect_vec = reflect(CameraVector, -Normal); ');
    Add('  float Temp = max(dot(reflect_vec, LightVector), 0.0); ');
    Add('  vec4 SpecContrib = gl_LightSource[0].specular * clamp(pow(Temp, SpecPower), 0.0, 0.95); ');
    Add(' ');
    if AFogSupport then
    begin
      if ARealisticSpecular then
        Add('  gl_FragColor = mix(gl_Fog.color, LightIntensity * (TextureContrib * (gl_LightSource[0].ambient + DiffuseContrib) + SpecContrib), fogFactor ); ')
      else
        Add('  gl_FragColor = mix(gl_Fog.color, TextureContrib * LightIntensity * (gl_LightSource[0].ambient + DiffuseContrib + SpecContrib), fogFactor ); ');
    end
    else
    begin
      if ARealisticSpecular then
        Add('  gl_FragColor = LightIntensity * (TextureContrib * (gl_LightSource[0].ambient + DiffuseContrib) + SpecContrib); ')
      else
        Add('  gl_FragColor = TextureContrib * LightIntensity * (gl_LightSource[0].ambient + DiffuseContrib + SpecContrib); ');

    end;
      Add('  gl_FragColor.a = TextureContrib.a; ');
    Add('} ');
  end;
end;

procedure GetFragmentProgramCodeAM(const Code: TStrings; const ARealisticSpecular: Boolean;
  const AFogSupport: Boolean);
begin
  with Code do
  begin
    Clear;
    Add('uniform vec4 AmbientColor; ');
    Add('uniform vec4 DiffuseColor; ');
    Add('uniform vec4 SpecularColor; ');
    Add(' ');
    Add('uniform float LightIntensity; ');
    Add('uniform float SpecPower; ');
    Add('uniform sampler2D MainTexture; ');
    Add(' ');
    Add('varying vec3 Normal; ');
    Add('varying vec3 LightVector; ');
    Add('varying vec3 CameraVector; ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add('  vec4 TextureContrib = texture2D(MainTexture, gl_TexCoord[0].xy); ');
    Add('  vec4 DiffuseContrib = clamp(DiffuseColor * dot(LightVector, Normal), 0.0, 1.0); ');
    Add(' ');
    Add('  vec3 reflect_vec = reflect(CameraVector, -Normal); ');
    Add('  float Temp = max(dot(reflect_vec, LightVector), 0.0); ');
    Add('  vec4 SpecContrib = SpecularColor * clamp(pow(Temp, SpecPower), 0.0, 0.95); ');
    Add(' ');
    if AFogSupport then
    begin
      if ARealisticSpecular then
        Add('  gl_FragColor = mix(gl_Fog.color, LightIntensity * (TextureContrib * (AmbientColor + DiffuseContrib) + SpecContrib); ')
      else
        Add('  gl_FragColor = mix(gl_Fog.color, TextureContrib * LightIntensity * (AmbientColor + DiffuseContrib + SpecContrib), fogFactor ); ');
    end
    else
    begin
      if ARealisticSpecular then
        Add('  gl_FragColor = LightIntensity * (TextureContrib * (AmbientColor + DiffuseContrib) + SpecContrib); ')
      else
        Add('  gl_FragColor = TextureContrib * LightIntensity * (AmbientColor + DiffuseContrib + SpecContrib); ');
    end;

    Add('  gl_FragColor.a = TextureContrib.a; ');
    Add('} ');
  end;
end;


procedure GetMLVertexProgramCode(const Code: TStrings);
begin
  with Code do
  begin
    Clear;
    Add('varying vec3 Normal; ');
    Add('varying vec3 LightVector; ');
    Add('varying vec3 ViewDirection; ');
    Add(' ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add('  gl_Position = ftransform(); ');
    Add('  gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0; ');
    Add('  Normal = normalize(gl_NormalMatrix * gl_Normal); ');
    Add('  ViewDirection = (gl_ModelViewMatrix * gl_Vertex).xyz; ');
    Add('} ');
  end;
end;

procedure GetMLFragmentProgramCodeBeg(const Code: TStrings);
begin
  with Code do
  begin
    Clear;
    Add('uniform float LightIntensity; ');
    Add('uniform float SpecPower; ');
    Add('uniform sampler2D MainTexture; ');
    Add(' ');
    Add('varying vec3 Normal; ');
    Add('varying vec3 ViewDirection; ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add('  vec3 LightVector; ');
    Add('  vec3 reflect_vec; ');
    Add('  float Temp; ');
    Add('  vec4 TextureContrib = texture2D(MainTexture, gl_TexCoord[0].st); ');
    Add('  vec3 CameraVector = normalize(ViewDirection); ');
    Add(' ');
    Add('  vec4 DiffuseContrib = vec4(0, 0, 0, 0); ');
    Add('  vec4 SpecContrib    = vec4(0, 0, 0, 0); ');
    Add('  vec4 AmbientContrib = vec4(0, 0, 0, 0); ');
  end;
end;

procedure GetMLFragmentProgramCodeMid(const Code: TStrings; const CurrentLight: Integer);
begin
  with Code do
  begin
    Add('  LightVector = normalize(gl_LightSource[' + IntToStr(CurrentLight) + '].position.xyz - ViewDirection); ');
    Add('  AmbientContrib = AmbientContrib + gl_LightSource[' + IntToStr(CurrentLight) + '].ambient; ');
    Add('  DiffuseContrib = min(DiffuseContrib + clamp(gl_LightSource[' + IntToStr(CurrentLight) + '].diffuse * dot(LightVector, Normal), 0.0, 1.0), 1.0); ');
    Add('  reflect_vec = reflect(CameraVector, -Normal); ');
    Add('  Temp = max(dot(reflect_vec, LightVector), 0.0); ');
    Add('  SpecContrib = min(SpecContrib + gl_LightSource[' + IntToStr(CurrentLight) + '].specular * clamp(pow(Temp, SpecPower), 0.0, 0.95), 1.0); ');
  end;
end;

procedure GetMLFragmentProgramCodeEnd(const Code: TStrings; const FLightCount: Integer; const FLightCompensation: Single; const FRealisticSpecular: Boolean);
var
  Temp: AnsiString;
begin
  with Code do
  begin
    if (FLightCount = 1) or (FLightCompensation = 1) then
    begin
      if FRealisticSpecular then
        Add('  gl_FragColor = LightIntensity * (TextureContrib * (AmbientContrib + DiffuseContrib) + SpecContrib); ')
      else
        Add('  gl_FragColor = LightIntensity * TextureContrib  * (AmbientContrib + DiffuseContrib  + SpecContrib); ');
    end
    else
    begin
      Str((1 + (FLightCount  - 1) * FLightCompensation) / FLightCount: 1: 1, Temp);
      if FRealisticSpecular then
        Add('  gl_FragColor = (LightIntensity * TextureContrib * (AmbientContrib + DiffuseContrib) + SpecContrib) * ' + string(Temp) + '; ')
      else
        Add('  gl_FragColor =  LightIntensity * TextureContrib * (AmbientContrib + DiffuseContrib  + SpecContrib) * ' + string(Temp) + '; ');
    end;

    Add('  gl_FragColor.a = TextureContrib.a; ');
    Add('} ');
  end;
end;


{ TGLBaseCustomGLSLDiffuseSpecular }

constructor TGLBaseCustomGLSLDiffuseSpecular.Create(
  AOwner: TComponent);
begin
  inherited;

  FSpecularPower  := 8;
  FLightPower     := 1;
  FFogSupport := sfsAuto;
  TStringList(VertexProgram.Code).OnChange := nil;
  TStringList(FragmentProgram.Code).OnChange := nil;
  VertexProgram.Enabled := true;
  FragmentProgram.Enabled := true;
end;

procedure TGLBaseCustomGLSLDiffuseSpecular.DoApply(
  var rci: TRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  Param['SpecPower'].AsVector1f := FSpecularPower;
  Param['LightIntensity'].AsVector1f := FLightPower;
end;

function TGLBaseCustomGLSLDiffuseSpecular.DoUnApply(
  var rci: TRenderContextInfo): Boolean;
begin
  Result := False;
  GetGLSLProg.EndUseProgramObject;
end;

procedure TGLBaseCustomGLSLDiffuseSpecular.SetFogSupport(
  const Value: TGLShaderFogSupport);
begin
  if FFogSupport <> Value then
  begin
    FFogSupport := Value;
    Self.FinalizeShader;
  end;
end;

procedure TGLBaseCustomGLSLDiffuseSpecular.SetRealisticSpecular(
  const Value: Boolean);
begin
  if FRealisticSpecular <> Value then
  begin
    FRealisticSpecular := Value;
    Self.FinalizeShader;
  end;
end;


{ TGLBaseGLSLDiffuseSpecularShaderMT }

procedure TGLBaseGLSLDiffuseSpecularShaderMT.DoApply(
  var rci: TRenderContextInfo; Sender: TObject);
begin
  inherited;
  Param['MainTexture'].AsTexture2D[0] := FMainTexture;
end;

function TGLBaseGLSLDiffuseSpecularShaderMT.GetMainTextureName: TGLLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfTexture(FMainTexture);
end;

function TGLBaseGLSLDiffuseSpecularShaderMT.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLBaseGLSLDiffuseSpecularShaderMT.Notification(
  AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin
        //need to nil the textures that were ownned by it
        if FMainTexture <> nil then
        begin
          Index := FMaterialLibrary.Materials.GetTextureIndex(FMainTexture);
          if Index <> -1 then
            FMainTexture := nil;
        end;
        FMaterialLibrary := nil;
      end;
end;

procedure TGLBaseGLSLDiffuseSpecularShaderMT.SetMainTextureName(
  const Value: TGLLibMaterialName);
begin
  if FMaterialLibrary = nil then
  begin
    FMainTextureName := Value;
    if not (csLoading in ComponentState) then
      raise EGLSLDiffuseSpecularShaderException.Create(glsErrorEx + glsMatLibNotDefined);
  end
  else
  begin
    FMainTexture := FMaterialLibrary.TextureByName(Value);
    FMainTextureName := '';
  end;
end;

procedure TGLBaseGLSLDiffuseSpecularShaderMT.SetMaterialLibrary(
  const Value: TGLMaterialLibrary);
begin
  if FMaterialLibrary <> nil then
    FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;

  if FMaterialLibrary <> nil then
  begin
    FMaterialLibrary.FreeNotification(Self);

    if FMainTextureName <> '' then
      SetMainTextureName(FMainTextureName);
  end
  else
    FMainTextureName := '';
end;

{ TGLCustomGLSLDiffuseSpecularShaderAM }

constructor TGLCustomGLSLDiffuseSpecularShaderAM.Create(AOwner: TComponent);
begin
  inherited;
  FAmbientColor := TGLColor.Create(Self);
  FDiffuseColor := TGLColor.Create(Self);
  FSpecularColor := TGLColor.Create(Self);

  //setup initial parameters
  FAmbientColor.SetColor(0.15, 0.15, 0.15, 1);
  FDiffuseColor.SetColor(1, 1, 1, 1);
  FSpecularColor.SetColor(1, 1, 1, 1);
end;

destructor TGLCustomGLSLDiffuseSpecularShaderAM.Destroy;
begin
  FAmbientColor.Destroy;
  FDiffuseColor.Destroy;
  FSpecularColor.Destroy;
  inherited;
end;

procedure TGLCustomGLSLDiffuseSpecularShaderAM.DoApply(var rci: TRenderContextInfo;
  Sender: TObject);
begin
  inherited;
  Param['AmbientColor'].AsVector4f := FAmbientColor.Color;
  Param['DiffuseColor'].AsVector4f := FDiffuseColor.Color;
  Param['SpecularColor'].AsVector4f := FSpecularColor.Color;
end;

procedure TGLCustomGLSLDiffuseSpecularShaderAM.DoInitialize(var rci: TRenderContextInfo; Sender: TObject);
begin
  GetVertexProgramCode(VertexProgram.Code, IsFogEnabled(FFogSupport, rci), rci);
  GetFragmentProgramCodeAM(FragmentProgram.Code, FRealisticSpecular, IsFogEnabled(FFogSupport, rci));
  inherited;
end;

function TGLCustomGLSLDiffuseSpecularShaderAM.GetAlpha: Single;
begin
  Result := (FAmbientColor.Alpha + FDiffuseColor.Alpha + FSpecularColor.Alpha) / 3;
end;

procedure TGLCustomGLSLDiffuseSpecularShaderAM.SetAlpha(const Value: Single);
begin
  FAmbientColor.Alpha := Value;
  FDiffuseColor.Alpha := Value;
  FSpecularColor.Alpha := Value;
end;

{ TGLCustomGLSLDiffuseSpecularShaderMT }

procedure TGLCustomGLSLDiffuseSpecularShaderMT.DoInitialize(var rci: TRenderContextInfo; Sender: TObject);
begin
  GetVertexProgramCode(VertexProgram.Code, IsFogEnabled(FFogSupport, rci), rci);
  GetFragmentProgramCode(FragmentProgram.Code, FRealisticSpecular, IsFogEnabled(FFogSupport, rci));
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited;
end;

{ TGLCustomGLSLDiffuseSpecularShader }

procedure TGLCustomGLSLDiffuseSpecularShader.DoApply(
  var rci: TRenderContextInfo; Sender: TObject);
begin
  inherited;
  Param['MainTexture'].AsVector1i := 0;  // Use the current texture.
end;

procedure TGLCustomGLSLDiffuseSpecularShader.DoInitialize(var rci: TRenderContextInfo; Sender: TObject);
begin
  GetVertexProgramCode(VertexProgram.Code, IsFogEnabled(FFogSupport, rci), rci);
  GetFragmentProgramCode(FragmentProgram.Code, FRealisticSpecular, IsFogEnabled(FFogSupport, rci));
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited;
end;

{ TGLCustomGLSLMLDiffuseSpecularShader }

constructor TGLCustomGLSLMLDiffuseSpecularShader.Create(
  AOwner: TComponent);
begin
  inherited;
  FLightCount := 1;
  FLightCompensation := 1;
end;

procedure TGLCustomGLSLMLDiffuseSpecularShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
begin
  inherited;
  Param['MainTexture'].AsVector1i := 0;  // Use the current texture.
end;

procedure TGLCustomGLSLMLDiffuseSpecularShader.DoInitialize(var rci: TRenderContextInfo; Sender: TObject);
var
  I: Integer;
begin
  GetMLVertexProgramCode(VertexProgram.Code);
  with FragmentProgram.Code do
  begin
    GetMLFragmentProgramCodeBeg(FragmentProgram.Code);

    // Repeat for all lights.
    for I := 0 to FLightCount - 1 do
      GetMLFragmentProgramCodeMid(FragmentProgram.Code, I);

    GetMLFragmentProgramCodeEnd(FragmentProgram.Code, FLightCount, FLightCompensation, FRealisticSpecular);
  end;
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited;
end;

procedure TGLCustomGLSLMLDiffuseSpecularShader.SetLightCompensation(
  const Value: Single);
begin
  FLightCompensation := Value;
  FinalizeShader;
end;

procedure TGLCustomGLSLMLDiffuseSpecularShader.SetLightCount(
  const Value: Integer);
begin
  Assert(FLightCount > 0, glsErrorEx + glsShaderNeedsAtLeastOneLightSource);
  FLightCount := Value;
  FinalizeShader;
end;


{ TGLCustomGLSLMLDiffuseSpecularShaderMT }

constructor TGLCustomGLSLMLDiffuseSpecularShaderMT.Create(
  AOwner: TComponent);
begin
  inherited;
  FLightCount := 1;
  FLightCompensation := 1;
end;

procedure TGLCustomGLSLMLDiffuseSpecularShaderMT.DoInitialize(var rci: TRenderContextInfo; Sender: TObject);
var
  I: Integer;
begin
  GetMLVertexProgramCode(VertexProgram.Code);
  with FragmentProgram.Code do
  begin
    GetMLFragmentProgramCodeBeg(FragmentProgram.Code);

    // Repeat for all lights.
    for I := 0 to FLightCount - 1 do
      GetMLFragmentProgramCodeMid(FragmentProgram.Code, I);

    GetMLFragmentProgramCodeEnd(FragmentProgram.Code, FLightCount, FLightCompensation, FRealisticSpecular);
  end;
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited;
end;

procedure TGLCustomGLSLMLDiffuseSpecularShaderMT.SetLightCompensation(
  const Value: Single);
begin
  FLightCompensation := Value;
  FinalizeShader;
end;

procedure TGLCustomGLSLMLDiffuseSpecularShaderMT.SetLightCount(
  const Value: Integer);
begin
  Assert(FLightCount > 0, glsErrorEx + glsShaderNeedsAtLeastOneLightSource);
  FLightCount := Value;
  FinalizeShader;
end;

initialization
  RegisterClasses([
                  TGLCustomGLSLDiffuseSpecularShader,
                  TGLCustomGLSLDiffuseSpecularShaderAM,
                  TGLCustomGLSLDiffuseSpecularShaderMT,
                  TGLCustomGLSLMLDiffuseSpecularShader,
                  TGLCustomGLSLMLDiffuseSpecularShaderMT,

                  TGLSLDiffuseSpecularShader,
                  TGLSLDiffuseSpecularShaderAM,
                  TGLSLDiffuseSpecularShaderMT,
                  TGLSLMLDiffuseSpecularShader,
                  TGLSLMLDiffuseSpecularShaderMT
                  ]);

end.

