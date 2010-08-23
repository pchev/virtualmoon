//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSLPostBlurShader <p>

   A shader that blurs the entire scene.<p>

   <b>History : </b><font size=-1><ul>
      <li>16/04/07 - DaStr - Shader made ATI compatible
      <li>05/04/07 - DaStr - Initial version (contributed to GLScene)


   Previous version history:
      v1.0  04 November    '2006  Creation (based on RenderMonkey demo)
      v1.1  04 March       '2007  IGLPostShader support added
      v1.2  30 March       '2007  TGLCustomGLSLPostBlurShaderSceneObject removed
                                  Shader now supports GL_TEXTURE_RECTANGLE_ARB
}
unit GLSLPostBlurShader;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes,

  // GLScene
  GLTexture, GLScene, VectorGeometry, GLContext,
  GLSLShader, GLCustomShader, GLRenderContextInfo;

type
  TGLCustomGLSLPostBlurShader = class(TGLCustomGLSLShader, IGLPostShader)
  private
    FThreshold: Single;

    // Implementing IGLPostShader.
    procedure DoUseTempTexture(const TempTexture: TGLTextureHandle; const TextureTarget: Cardinal);
    function GetTextureTarget: TGLTextureTarget;
    function StoreThreshold: Boolean;
  protected
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

    property Threshold: Single read FThreshold write FThreshold stored StoreThreshold;
  end;

  TGLSLPostBlurShader = class(TGLCustomGLSLPostBlurShader)
  published
    property Threshold;
  end;

implementation

{ TGLCustomGLSLPostBlurShader }

constructor TGLCustomGLSLPostBlurShader.Create(
  AOwner: TComponent);
begin
  inherited;
  with VertexProgram.Code do
  begin
    Add('varying vec2 vTexCoord; ');
    Add(' '); 
    Add('void main(void) '); 
    Add('{ '); 
    Add(' '); 
    Add('   // Clean up inaccuracies '); 
    Add('   vec2 Position; '); 
    Add('   Position.xy = sign(gl_Vertex.xy); '); 
    Add(' '); 
    Add('   gl_Position = vec4(Position.xy, 0.0, 1.0); '); 
    Add('   vTexCoord = Position.xy *.5 + .5; '); 
    Add('    ');
    Add('} '); 
  end;

  with FragmentProgram.Code do
  begin
    Add('uniform float threshold; ');
    Add('uniform vec2 ScreenExtents; ');
    Add('uniform sampler2DRect Image; ');
    Add(' ');
    Add('varying vec2 vTexCoord; ');
    Add(' ');
    Add('void main() ');
    Add('{ ');
    Add('   ');
    Add('   vec2 samples[8]; ');
    Add('   vec2 vTexCoordScr = vTexCoord * ScreenExtents; ');
    Add('    ');
    Add('   samples[0]  = vTexCoordScr + vec2(-1.0, -1.0); ');
    Add('   samples[1]  = vTexCoordScr + vec2( 0.0, -1.0); ');
    Add('   samples[2]  = vTexCoordScr + vec2( 1.0, -1.0); ');
    Add('   samples[3]  = vTexCoordScr + vec2(-1.0,  0.0); ');
    Add('   samples[4]  = vTexCoordScr + vec2( 1.0,  0.0); ');
    Add('   samples[5]  = vTexCoordScr + vec2(-1.0,  1.0); ');
    Add('   samples[6]  = vTexCoordScr + vec2( 0.0,  1.0); ');
    Add('   samples[7]  = vTexCoordScr + vec2( 1.0,  1.0); ');
    Add(' ');
    Add('   vec4 sample = texture2DRect(Image, vTexCoordScr); ');
    Add(' ');
    Add('   // Neighborhood average ');
    Add('   vec4 avg = sample; ');
    Add('   for (int i = 0; i < 8; i++) ');
    Add('   { ');
    Add('      avg += texture2DRect(Image,  samples[i]); ');
    Add('   } ');
    Add('    ');
    Add(' ');
    Add('   avg /= 9.0; ');
    Add(' ');
    Add('   // If the difference between the average and the sample is ');
    Add('   // large, we''ll assume it''s noise. ');
    Add('   vec4  diff = abs(sample - avg); ');
    Add('   float sel  = float(dot(diff, vec4(0.25)) > threshold); ');
    Add(' ');
    Add('   gl_FragColor =  mix(sample, avg, sel); ');
    Add('} '); 
  end;
  FThreshold := 0.1;
end;

procedure TGLCustomGLSLPostBlurShader.DoApply(
  var rci: TRenderContextInfo; Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  GetGLSLProg.Uniform1f['threshold'] := FThreshold;
  GetGLSLProg.Uniform2f['ScreenExtents'] := Vector2fMake(rci.viewPortSize.cx, rci.viewPortSize.cy);
end;

function TGLCustomGLSLPostBlurShader.DoUnApply(
  var rci: TRenderContextInfo): Boolean;
begin
  rci.GLStates.ActiveTexture := 0;
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TGLCustomGLSLPostBlurShader.DoUseTempTexture(
  const TempTexture: TGLTextureHandle; const TextureTarget: Cardinal);
begin
  Param['Image'].AsCustomTexture[2, TextureTarget] := TempTexture.Handle;
end;

function TGLCustomGLSLPostBlurShader.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTextureRect;
end;

function TGLCustomGLSLPostBlurShader.StoreThreshold: Boolean;
begin
  Result := Abs(FThreshold - 0.1) > 0.00001;
end;

end.

