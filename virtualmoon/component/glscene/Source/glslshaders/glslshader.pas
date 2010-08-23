//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSLShader<p>

    TGLSLShader is a wrapper for GLS shaders.<p>

	<b>History : </b><font size=-1><ul>
      <li>15/03/08 - DaStr - Fixups for vIgnoreContextActivationFailures mode
                                                      (BugTracker ID = 1914782)
      <li>25/12/07 - DaStr - Fix-up for previous update (BugtrackerID = 1772477)
      <li>12/08/07 - LC - TGLSLShaderParameter.SetAsCustomTexture now restores the
                          active texture unit (BugtrackerID = 1772477)
      <li>12/07/07 - DaStr - TGLSLInitializedShaderParameters removed because
                              even if implemented, it could not give
                              a significant performance increase
      <li>30/03/07 - fig -   Changed OnInitialize event to be fired after
                              linking, but before validation. This can now be
                              used to set texture units for different sampler
                              types (1D/2D/3D) before validation, which fixes
                              a bug (or complies to strict validation) with ATI
                              drivers.
      <li>30/03/07 - DaStr - Bugfixed TGLCustomGLSLShader.DoUnApply
                              (Result was not initialized)
      <li>20/03/07 - DaStr - TGLCustomGLSLShader now generates its own events
                             Added TGLSLShaderParameter
                             Added TGLCustomGLSLShader.DoInitialPass
                             Added TGLCustomGLSLShader.Param[]
      <li>21/02/07 - DaStr - Initial version (contributed to GLScene)



    Previous version history:
      v1.0    11 March     '2006  Creation
      v1.1    06 August    '2006  TGLCustomGLSLShader.DoInitialize bugfixed
      v1.1.2  24 August    '2006  TGLCustomShader.SetParameterTexture[1-3]D added
      v1.1.4  09 September '2006  Fixed a memory leak which occured when
                                   enabling / disabling the shader several times
      v1.1.6  22 September '2006  DoUnApply fixed (suggested by Nelsol Chu)
      v1.2    04 November  '2006  function GetGLSLProg added (just in case)
                                  TGLSLShader has more published properties
                                  Bugfix in DoInitialize (when no shader is active)
                                  (Get/Set)ParameterTexture[1/2/3]DHandle added
                                  (Get/Set)ParameterCustomTextureHandle support added
      v1.2.4  22 November  '2006  TGLProgramHandle.Name is now used
                                  Assign() bugfixed
                                  Fixed a possible bug in DoInitialize
                                    (Handle was freed, but not nil'ed)

}
unit GLSLShader;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes, SysUtils,

  // GLScene
  VectorGeometry, VectorTypes, GLTexture, GLContext, OpenGL1x, GLCustomShader;

type
  TGLSLShaderParameter = class;
  TGLCustomGLSLShader = class;
  EGLSLShaderException = class(EGLCustomShaderException);

  TGLSLShaderEvent = procedure(Shader: TGLCustomGLSLShader) of object;
  TGLSLShaderUnUplyEvent = procedure(Shader: TGLCustomGLSLShader;
                                     var ThereAreMorePasses: Boolean) of object;

  TGLCustomGLSLShader = class(TGLCustomShader)
  private
    FGLSLProg: TGLProgramHandle;
    FParam: TGLSLShaderParameter;

    FOnInitialize: TGLSLShaderEvent;
    FOnApply: TGLSLShaderEvent;
    FOnUnApply: TGLSLShaderUnUplyEvent;

    function GetParam(const Index: string): TGLSLShaderParameter;
    function GetDirectParam(const Index: Cardinal): TGLSLShaderParameter;
  protected
    property OnApply: TGLSLShaderEvent read FOnApply write FOnApply;
    property OnUnApply: TGLSLShaderUnUplyEvent read FOnUnApply write FOnUnApply;
    property OnInitialize: TGLSLShaderEvent read FOnInitialize write FOnInitialize;

    procedure DoInitialPass; virtual;

    function GetGLSLProg: TGLProgramHandle; virtual;
    function GetCurrentParam: TGLSLShaderParameter; virtual;
    procedure DoInitialize; override;
    procedure DoFinalize; override;
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ShaderSupported: Boolean; override;

    property Param[const Index: string]: TGLSLShaderParameter read GetParam;
    property DirectParam[const Index: Cardinal]: TGLSLShaderParameter read GetDirectParam;
  end;


  {: Wrapper around a parameter of a GLSL program. }
  TGLSLShaderParameter = class(TGLCustomShaderParameter)
  private
    { Private Declarations }
    FGLSLProg: TGLProgramHandle;
    FParameterID: GLInt;
  protected
    { Protected Declarations }
    function GetAsVector1f: Single; override;
    function GetAsVector1i: Integer; override;
    function GetAsVector2f: TVector2f; override;
    function GetAsVector2i: TVector2i; override;
    function GetAsVector3f: TVector3f; override;
    function GetAsVector3i: TVector3i; override;
    function GetAsVector4f: TVector; override;
    function GetAsVector4i: TVector4i; override;

    procedure SetAsVector1f(const Value: Single); override;
    procedure SetAsVector1i(const Value: Integer); override;
    procedure SetAsVector2i(const Value: TVector2i); override;
    procedure SetAsVector3i(const Value: TVector3i); override;
    procedure SetAsVector4i(const Value: TVector4i); override;
    procedure SetAsVector2f(const Value: TVector2f); override;
    procedure SetAsVector3f(const Value: TVector3f); override;
    procedure SetAsVector4f(const Value: TVector4f); override;

    function GetAsMatrix2f: TMatrix2f; override;
    function GetAsMatrix3f: TMatrix3f; override;
    function GetAsMatrix4f: TMatrix4f; override;
    procedure SetAsMatrix2f(const Value: TMatrix2f); override;
    procedure SetAsMatrix3f(const Value: TMatrix3f); override;
    procedure SetAsMatrix4f(const Value: TMatrix4f); override;

    function GetAsCustomTexture(const TextureIndex: Integer;
      const TextureTarget: Word): Cardinal; override;
    procedure SetAsCustomTexture(const TextureIndex: Integer;
      const TextureTarget: Word; const Value: Cardinal); override;

   public
     // Nothing here ...yet.
   end;

  TGLSLShader = class(TGLCustomGLSLShader)
  published
    property FragmentProgram;
    property VertexProgram;

    property OnApply;
    property OnUnApply;
    property OnInitialize;

    property ShaderStyle;
    property FailedInitAction;
  end;


implementation

{ TGLCustomGLSLShader }

procedure TGLCustomGLSLShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
begin
  FGLSLProg.UseProgramObject;
  if Assigned(FOnApply) then
    FOnApply(Self);
end;


procedure TGLCustomGLSLShader.DoInitialize;
begin
  try
    if not ShaderSupported then
      HandleFailedInitialization
    else
    try
      if VertexProgram.Enabled or FragmentProgram.Enabled then
      begin
        FGLSLProg := TGLProgramHandle.CreateAndAllocate;
        FParam.FGLSLProg := FGLSLProg;
        if Name <> '' then
          FGLSLProg.Name := Name
        else
          FGLSLProg.Name := ClassName;
      end;

      if VertexProgram.Enabled then
        FGLSLProg.AddShader(TGLVertexShaderHandle, VertexProgram.Code.Text);
      if FragmentProgram.Enabled then
        FGLSLProg.AddShader(TGLFragmentShaderHandle, FragmentProgram.Code.Text);

      if VertexProgram.Enabled or FragmentProgram.Enabled then
      begin
        if (not FGLSLProg.LinkProgram) then
          raise EGLSLShaderException.Create(FGLSLProg.InfoLog);
      end
      else
        FreeAndNil(FGLSLProg);

    except
      on E: Exception do
      begin
        HandleFailedInitialization(E.Message);
        FreeAndNil(FGLSLProg);
      end;
    end;

  finally
    Enabled := (FGLSLProg <> nil);

    if Enabled then
    try
      DoInitialPass;
      if (not FGLSLProg.ValidateProgram) then
        raise EGLSLShaderException.Create(FGLSLProg.InfoLog);
    except
      on E: Exception do
      begin
        HandleFailedInitialization(E.Message);
        FreeAndNil(FGLSLProg);
        Enabled:=false;
      end;
    end;
  end;
end;


function TGLCustomGLSLShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnUnApply) then
    FOnUnApply(Self, Result);
  if not Result then
    FGLSLProg.EndUseProgramObject;
end;


function TGLCustomGLSLShader.ShaderSupported: Boolean;
begin
  Result := (GL_ARB_shader_objects and GL_ARB_vertex_program and
             GL_ARB_vertex_shader and GL_ARB_fragment_shader);
end;

procedure TGLCustomGLSLShader.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TGLCustomGLSLShader then
  begin
    FreeAndNil(FGLSLProg); //just free the handle for it to be recreated on next initialization
  end;
end;

procedure TGLCustomGLSLShader.DoFinalize;
begin
  inherited;
  FreeAndNil(FGLSLProg);
end;

function TGLCustomGLSLShader.GetGLSLProg: TGLProgramHandle;
begin
  Result := FGLSLProg;
end;

function TGLCustomGLSLShader.GetParam(
  const Index: string): TGLSLShaderParameter;
begin
  FParam.FParameterID := FGLSLProg.GetUniformLocation(Index);
  Result := FParam;
end;

function TGLCustomGLSLShader.GetDirectParam(
  const Index: Cardinal): TGLSLShaderParameter;
begin
  FParam.FParameterID := Index;
  Result := FParam;
end;

function TGLCustomGLSLShader.GetCurrentParam: TGLSLShaderParameter;
begin
  Result := FParam;
end;

constructor TGLCustomGLSLShader.Create(AOwner: TComponent);
begin
  inherited;
  FParam := TGLSLShaderParameter.Create;
end;

destructor TGLCustomGLSLShader.Destroy;
begin
  if vIgnoreContextActivationFailures then
    FGLSLProg.Free;
  FParam.Free;
  inherited;
end;

procedure TGLCustomGLSLShader.DoInitialPass;
begin
  if Assigned(FOnInitialize) then
  begin
    FGLSLProg.UseProgramObject;
    FOnInitialize(Self);
    FGLSLProg.EndUseProgramObject;
  end;
end;

{ TGLSLShaderParameter }

function TGLSLShaderParameter.GetAsCustomTexture(
  const TextureIndex: Integer; const TextureTarget: Word): Cardinal;
begin
  glGetUniformivARB(FGLSLProg.Handle, TextureIndex, @Result);
end;

function TGLSLShaderParameter.GetAsMatrix2f: TMatrix2f;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsMatrix3f: TMatrix3f;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsMatrix4f: TMatrix4f;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector1f: Single;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector1i: Integer;
begin
  glGetUniformivARB(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector2f: TVector2f;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector2i: TVector2i;
begin
  glGetUniformivARB(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector3f: TVector3f;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector3i: TVector3i;
begin
  glGetUniformivARB(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector4f: TVector;
begin
  glGetUniformfvARB(FGLSLProg.Handle, FParameterID, @Result);
end;

function TGLSLShaderParameter.GetAsVector4i: TVector4i;
begin
  glGetUniformivARB(FGLSLProg.Handle, FParameterID, @Result);
end;

procedure TGLSLShaderParameter.SetAsCustomTexture(
  const TextureIndex: Integer; const TextureTarget: Word;
  const Value: Cardinal);
begin
  glActiveTextureARB(GL_TEXTURE0_ARB + TextureIndex);
  glBindTexture(TextureTarget, Value);
  glUniform1iARB(FParameterID, TextureIndex);
  glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure TGLSLShaderParameter.SetAsMatrix2f(const Value: TMatrix2f);
begin
  glUniformMatrix2fvARB(FParameterID, 1, False, @Value);
end;

procedure TGLSLShaderParameter.SetAsMatrix3f(const Value: TMatrix3f);
begin
  glUniformMatrix3fvARB(FParameterID, 1, False, @Value);
end;

procedure TGLSLShaderParameter.SetAsMatrix4f(const Value: TMatrix4f);
begin
  glUniformMatrix4fvARB(FParameterID, 1, False, @Value);
end;

procedure TGLSLShaderParameter.SetAsVector1f(const Value: Single);
begin
  glUniform1fARB(FParameterID, Value);
end;

procedure TGLSLShaderParameter.SetAsVector1i(const Value: Integer);
begin
  glUniform1iARB(FParameterID, Value);
end;

procedure TGLSLShaderParameter.SetAsVector2f(const Value: TVector2f);
begin
  glUniform2fARB(FParameterID, Value[0], Value[1]);
end;

procedure TGLSLShaderParameter.SetAsVector2i(const Value: TVector2i);
begin
  glUniform2iARB(FParameterID, Value[0], Value[1]);
end;

procedure TGLSLShaderParameter.SetAsVector3f(const Value: TVector3f);
begin
  glUniform3fARB(FParameterID, Value[0], Value[1], Value[2]);
end;

procedure TGLSLShaderParameter.SetAsVector3i(const Value: TVector3i);
begin
  glUniform3iARB(FParameterID, Value[0], Value[1], Value[2]);
end;

procedure TGLSLShaderParameter.SetAsVector4f(const Value: TVector4f);
begin
  glUniform4fARB(FParameterID, Value[0], Value[1], Value[2], Value[3]);
end;

procedure TGLSLShaderParameter.SetAsVector4i(const Value: TVector4i);
begin
  glUniform4iARB(FParameterID, Value[0], Value[1], Value[2], Value[3]);
end;

initialization
  RegisterClasses([TGLCustomGLSLShader, TGLSLShader]);

end.

