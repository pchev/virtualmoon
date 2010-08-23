//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLCelShader<p>

   A shader that applies cel shading through a vertex program
   and shade definition texture.<p>

   <b>History : </b><font size=-1><ul>
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>22/01/10 - Yar   - Added bmp32.Blank:=false for memory allocation
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>31/03/07 - DaStr - Added $I GLScene.inc
      <li>21/03/07 - DaStr - Added explicit pointer dereferencing
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
      <li>25/02/07 - DaStr - Moved registration to GLSceneRegister.pas
      <li>28/09/04 - SG - Vertex program now uses ARB_position_invariant option.
      <li>09/06/04 - SG - Added OutlineColor, vertex programs now use GL state.
      <li>28/05/04 - SG - Creation.
   </ul></font>
}
unit GLCelShader;

interface

{$I GLScene.inc}

uses
   Classes, SysUtils, GLTexture, GLContext, GLGraphics, GLUtils,
   VectorGeometry, OpenGL1x, ARBProgram, GLColor, GLRenderContextInfo,
   GLMaterial, GLState;

type
   // TGLCelShaderOption
   //
   {: Cel shading options.<p>
      csoOutlines: Render a second outline pass.
      csoTextured: Allows for a primary texture that the cel shading
                   is modulated with and forces the shade definition
                   to render as a second texture. }
   TGLCelShaderOption = (csoOutlines, csoTextured, csoNoBuildShadeTexture);
   TGLCelShaderOptions = set of TGLCelShaderOption;

   // TGLCelShaderGetIntensity
   //
   //: An event for user defined cel intensity.
   TGLCelShaderGetIntensity = procedure (Sender : TObject; var intensity : Byte) of object;

   // TGLCelShader
   //
   {: A generic cel shader.<p> }
   TGLCelShader = class (TGLShader)
      private
         FOutlineWidth : Single;
         FCelShaderOptions : TGLCelShaderOptions;
         FVPHandle : Cardinal;
         FShadeTexture : TGLTexture;
         FOnGetIntensity : TGLCelShaderGetIntensity;
         FOutlinePass,
         FUnApplyShadeTexture : Boolean;
         FOutlineColor : TGLColor;

      protected
         procedure SetCelShaderOptions(const val : TGLCelShaderOptions);
         procedure SetOutlineWidth(const val : Single);
         procedure SetOutlineColor(const val : TGLColor);
         procedure BuildShadeTexture;
         procedure Loaded; override;
         function GenerateVertexProgram : String;
         procedure DestroyVertexProgram;

      public
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

         procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
         function DoUnApply(var rci: TRenderContextInfo) : Boolean; override;

         property ShadeTexture : TGLTexture read FShadeTexture;

      published
         property CelShaderOptions : TGLCelShaderOptions read FCelShaderOptions write SetCelShaderOptions;
         property OutlineColor : TGLColor read FOutlineColor write SetOutlineColor;
         property OutlineWidth : Single read FOutlineWidth write SetOutlineWidth;
         property OnGetIntensity : TGLCelShaderGetIntensity read FOnGetIntensity write FOnGetIntensity;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLCelShader ------------------
// ------------------

// Create
//
constructor TGLCelShader.Create(AOwner : TComponent);
begin
   inherited;

   FOutlineWidth:=3;
   FCelShaderOptions:=[csoOutlines];
   FShadeTexture:=TGLTexture.Create(Self);
   with FShadeTexture do begin
      Enabled:=True;
      MinFilter:=miNearest;
      MagFilter:=maNearest;
      TextureWrap:=twNone;
      TextureMode:=tmModulate;
   end;

   FOutlineColor:=TGLColor.Create(Self);
   FOutlineColor.OnNotifyChange:=NotifyChange;
   FOutlineColor.Initialize(clrBlack);

   ShaderStyle:=ssLowLevel;
end;

// Destroy
//
destructor TGLCelShader.Destroy;
begin
   DestroyVertexProgram;
   FShadeTexture.Free;
   FOutlineColor.Free;
   inherited;
end;

// Loaded
//
procedure TGLCelShader.Loaded;
begin
   inherited;
   BuildShadeTexture;
end;

// BuildShadeTexture
//
procedure TGLCelShader.BuildShadeTexture;
var
   bmp32 : TGLBitmap32;
   i : Integer;
   intensity : Byte;
begin
   if csoNoBuildShadeTexture in FCelShaderOptions then exit;

   with FShadeTexture do begin
      ImageClassName:='TGLBlankImage';
      TGLBlankImage(Image).Width:=128;
      TGLBlankImage(Image).Height:=1;
   end;

   bmp32:=FShadeTexture.Image.GetBitmap32(GL_TEXTURE_2D);
   bmp32.Blank := false;
   for i:=0 to bmp32.Width-1 do begin
      intensity:=i*(256 div bmp32.Width);

      if Assigned(FOnGetIntensity) then
         FOnGetIntensity(Self, intensity)
      else begin
         if intensity>230 then intensity:=255
         else if intensity>150 then intensity:=230
         else if intensity>100 then intensity:=intensity+50
         else intensity:=150;
      end;

      bmp32.Data^[i].r:=intensity;
      bmp32.Data^[i].g:=intensity;
      bmp32.Data^[i].b:=intensity;
      bmp32.Data^[i].a:=1;
   end;
end;

// DestroyVertexProgram
//
procedure TGLCelShader.DestroyVertexProgram;
begin
   if FVPHandle > 0 then
      glDeleteProgramsARB(1, @FVPHandle);
end;

// GenerateVertexProgram
//
function TGLCelShader.GenerateVertexProgram : String;
var
   VP : TStringList;
begin
   VP:=TStringList.Create;
   
   VP.Add('!!ARBvp1.0');
   VP.Add('OPTION ARB_position_invariant;');

   VP.Add('PARAM mvinv[4] = { state.matrix.modelview.inverse };');
   VP.Add('PARAM lightPos = program.local[0];');
   VP.Add('TEMP temp, light, normal;');

   VP.Add('   DP4 light.x, mvinv[0], lightPos;');
   VP.Add('   DP4 light.y, mvinv[1], lightPos;');
   VP.Add('   DP4 light.z, mvinv[2], lightPos;');
   VP.Add('   ADD light, light, -vertex.position;');
   VP.Add('   DP3 temp.x, light, light;');
   VP.Add('   RSQ temp.x, temp.x;');
   VP.Add('   MUL light, temp.x, light;');

   VP.Add('   DP3 temp, vertex.normal, vertex.normal;');
   VP.Add('   RSQ temp.x, temp.x;');
   VP.Add('   MUL normal, temp.x, vertex.normal;');

   VP.Add('   MOV result.color, state.material.diffuse;');

   if csoTextured in FCelShaderOptions then begin
      VP.Add('   MOV result.texcoord[0], vertex.texcoord[0];');
      VP.Add('   DP3 result.texcoord[1].x, normal, light;');
   end else begin
      VP.Add('   DP3 result.texcoord[0].x, normal, light;');
   end;   
    
   VP.Add('END');
   
   Result:=VP.Text;
   VP.Free;
end;

// DoApply
//
procedure TGLCelShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
var
   success : Boolean;
   str : String;
   light : TVector;
begin
   if (csDesigning in ComponentState) then exit;

   if FVPHandle = 0 then begin
      success:=false;
      try
         str:=GenerateVertexProgram;
         LoadARBProgram(GL_VERTEX_PROGRAM_ARB, str, FVPHandle);
         success:=True;
      finally
         if not success then Enabled:=False;  
      end;
   end;

   rci.GLStates.PushAttrib([sttEnable, sttCurrent, sttColorBuffer, sttHint,
                            sttLine, sttPolygon, sttDepthBuffer]);

   rci.GLStates.Disable(stLighting);
   glEnable(GL_VERTEX_PROGRAM_ARB);
   glGetLightfv(GL_LIGHT0, GL_POSITION, @light[0]);
   glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB, 0, @light[0]);
   glBindProgramARB(GL_VERTEX_PROGRAM_ARB, FVPHandle);
   

   if (csoTextured in FCelShaderOptions) then
      FShadeTexture.ApplyAsTexture2(rci, nil)
   else
      FShadeTexture.Apply(rci);

   FOutlinePass:=csoOutlines in FCelShaderOptions;
   FUnApplyShadeTexture:=True;
end;

// DoUnApply
//
function TGLCelShader.DoUnApply(var rci: TRenderContextInfo) : Boolean;
begin
   Result:=False;
   if (csDesigning in ComponentState) then exit;

   glDisable(GL_VERTEX_PROGRAM_ARB);

   if FUnApplyShadeTexture then begin
      if (csoTextured in FCelShaderOptions) then
         FShadeTexture.UnApplyAsTexture2(rci, false)
      else
         FShadeTexture.UnApply(rci);
      FUnApplyShadeTexture:=False;
   end;

   if FOutlinePass then begin
      glDisable(GL_TEXTURE_2D);

      rci.GLStates.Enable(stBlend);
      rci.GLStates.Enable(stLineSmooth);
      rci.GLStates.Enable(stCullFace);

      rci.GLStates.PolygonMode := pmLines;
      rci.GLStates.CullFaceMode := cmFront;
      rci.GLStates.LineSmoothHint := hintNicest;
      rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
      rci.GLStates.DepthFunc := cfLEqual;
      glColor4fv(FOutlineColor.AsAddress);
      rci.GLStates.LineWidth := FOutlineWidth;

      Result:=True;
      FOutlinePass:=False;
      Exit;
   end;

   rci.GLStates.PopAttrib;
end;

// SetCelShaderOptions
//
procedure TGLCelShader.SetCelShaderOptions(const val: TGLCelShaderOptions);
begin
   if val<>FCelShaderOptions then begin
      FCelShaderOptions:=val;
      BuildShadeTexture;
      DestroyVertexProgram;
      NotifyChange(Self);
   end;
end;

// SetOutlineWidth
//
procedure TGLCelShader.SetOutlineWidth(const val : Single);
begin
   if val<>FOutlineWidth then begin
      FOutlineWidth:=val;
      NotifyChange(Self);
   end;
end;

// SetOutlineColor
//
procedure TGLCelShader.SetOutlineColor(const val: TGLColor);
begin
  if val<>FOutlineColor then begin
    FOutlineColor.Assign(val);
    NotifyChange(Self);
  end;
end;

end.
