//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSComputingRegister<p>

   Registration unit for GLScene Computing package.<p>

	<b>History : </b><font size=-1><ul>
      <li>05/03/11 - Yar - Added TCUDAConstant, TCUDAFuncParam
      <li>22/08/10 - Yar - Some improvements for FPC (thanks Predator)
      <li>09/06/10 - Yar - Added dropdown list ProjectModule for TGLSCUDACompiler
      <li>19/03/10 - Yar - Creation
	</ul></font>
}
unit GLSComputingRegister;

interface

uses
  Classes, SysUtils, GLSceneRegister,
  {$IFNDEF FPC}
  DesignIntf, DesignEditors, STREDIT, ToolsAPI
  {$ELSE}
  propedits, componenteditors
  {$ENDIF};

procedure Register;

type

  TGLSCUDAEditor = class(TComponentEditor)
  public
    { Public Declarations }
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TGLSCUDACompilerEditor = class(TComponentEditor)
  public
    { Public Declarations }
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TGLSCUDACompilerSourceProperty = class(TStringProperty)
  private
    FModuleList: TStringList;
    procedure RefreshModuleList;
  public
    { Public Declarations }
    {$IFNDEF FPC}
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
    {$ELSE}
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer); override;
    {$ENDIF}
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: String); override;
  end;

  TGLSCUDADeviceProperty = class(TStringProperty)
  private
    FDeviceList: TStringList;
  public
    { Public Declarations }
    {$IFNDEF FPC}
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
    {$ELSE}
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer); override;
    {$ENDIF}
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: String); override;
  end;

implementation

uses
  GLS_CUDA_RUNTIME,
  GLSCUDAEditor,
  GLSCUDAContext,
  GLSCUDA,
  GLSCUDACompiler,
  GLSCUDAFFTPlan,
  GLSCUDAGraphics,
  GLSCUDAParser;

procedure Register;
begin
  RegisterComponents('GLScene Computing', [TGLSCUDA, TGLSCUDADevice,
    TGLSCUDACompiler]);
  RegisterComponentEditor(TGLSCUDA, TGLSCUDAEditor);
  RegisterComponentEditor(TGLSCUDACompiler, TGLSCUDACompilerEditor);
  RegisterPropertyEditor(TypeInfo(string), TGLSCUDACompiler, 'ProjectModule',
    TGLSCUDACompilerSourceProperty);
  RegisterPropertyEditor(TypeInfo(string), TGLSCUDADevice, 'SelectDevice',
    TGLSCUDADeviceProperty);
  RegisterNoIcon([TCUDAModule, TCUDAMemData, TCUDAFunction, TCUDATexture,
    TCUDAFFTPlan, TCUDAGLImageResource, TCUDAGLGeometryResource, TCUDAConstant,
    TCUDAFuncParam]);

  ObjectManager.RegisterSceneObject(TGLFeedBackMesh, 'GPU generated mesh', 'Computing', HInstance);
end;

function FindCuFile(var AModuleName: string): Boolean;
var
  proj: IOTAProject;
  I: Integer;
  LModule: IOTAModuleInfo;
  LName: string;
begin
  proj := GetActiveProject;
  if proj <> nil then
  begin
    for I := 0 to proj.GetModuleCount - 1 do
    begin
      LModule := proj.GetModule(I);
      LName := ExtractFileName(LModule.FileName);
      if LName = AModuleName then
      begin
        AModuleName := LModule.FileName;
        exit(True);
      end;
    end;
  end;
  Result := False;
end;

// ------------------
// ------------------ TGLSCUDAEditor ------------------
// ------------------

procedure TGLSCUDAEditor.Edit;
begin
  with GLSCUDAEditorForm do
  begin
    SetCUDAEditorClient(TGLSCUDA(Self.Component), Self.Designer);
    Show;
  end;
end;

procedure TGLSCUDAEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TGLSCUDAEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Show CUDA Items Editor';
  end;
end;

function TGLSCUDAEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

// ------------------
// ------------------ TGLSCUDACompilerEditor ------------------
// ------------------

procedure TGLSCUDACompilerEditor.Edit;
var
  CUDACompiler: TGLSCUDACompiler;
  I, J: Integer;
  func: TCUDAFunction;
  tex: TCUDATexture;
  cnst: TCUDAConstant;
  param: TCUDAFuncParam;
  parent: TCUDAModule;
  info: TCUDAModuleInfo;
  bUseless: Boolean;
  useless: array of TCUDAComponent;
  CTN: TChannelTypeAndNum;

  procedure CreateFuncParams;
  var
    K: Integer;
  begin
    for K := 0 to High(info.Func[I].Args) do
    begin
      param := TCUDAFuncParam(Designer.CreateComponent(TCUDAFuncParam,
        func, 0, 0, 0, 0));
      param.Master := TCUDAComponent(func);
      param.KernelName := info.Func[I].Args[K].Name;
      param.Name := func.KernelName+'_'+param.KernelName;
      param.DataType := info.Func[I].Args[K].DataType;
      param.CustomType := info.Func[I].Args[K].CustomType;
      param.Reference := info.Func[I].Args[K].Ref;
    end;
  end;

begin
  CUDACompiler := TGLSCUDACompiler(Self.Component);
  if CUDACompiler.Compile then
  begin
    info := CUDACompiler.ModuleInfo;
    parent := TCUDAModule(info.Owner);

    // Create kernel's functions
    for I := 0 to High(info.Func) do
    begin
      func := parent.KernelFunction[info.Func[I].KernelName];
      if not Assigned(func) then
      begin
        func := TCUDAFunction(Designer.CreateComponent(TCUDAFunction,
          info.Owner, 0, 0, 0, 0));
        func.Master := TCUDAComponent(info.Owner);
        func.KernelName := info.Func[I].KernelName;
        func.Name := TCUDAComponent(info.Owner).MakeUniqueName(info.Func[I].Name);
      end
      else
      begin
        // destroy old parameters
        while func.ItemsCount > 0 do
          func.Items[0].Destroy;
      end;

      try
        bUseless := func.Handle = nil;
      except
        bUseless := True;
      end;
      if bUseless then
      begin
        Designer.SelectComponent(func);
        Designer.DeleteSelection(True);
        func := nil;
      end
      else
        CreateFuncParams;
    end;

    // Create kernel's textures
    for I := 0 to High(info.TexRef) do
    begin
      tex := parent.KernelTexture[info.TexRef[I].Name];
      if not Assigned(tex) then
      begin
        tex := TCUDATexture(Designer.CreateComponent(TCUDATexture,
          info.Owner, 0, 0, 0, 0));
        tex.Master := TCUDAComponent(info.Owner);
        tex.KernelName := info.TexRef[I].Name;
        tex.Name := tex.KernelName;
        tex.ReadAsInteger := (info.TexRef[I].ReadMode = cudaReadModeElementType);
        CTN := GetChannelTypeAndNum(info.TexRef[I].DataType);
        tex.Format := CTN.F;
      end;

      tex.ChannelNum := CTN.C;
      try
        bUseless := tex.Handle = nil;
      except
        bUseless := True;
      end;
      if bUseless then
      begin
        Designer.SelectComponent(tex);
        Designer.DeleteSelection(True);
      end;
    end;
    // Create kernel's constants
    for I := 0 to High(info.Constant) do
    begin
      cnst := parent.KernelConstant[info.Constant[I].Name];
      if not Assigned(cnst) then
      begin
        cnst := TCUDAConstant(Designer.CreateComponent(TCUDAConstant,
          info.Owner, 0, 0, 0, 0));
        cnst.Master := TCUDAComponent(info.Owner);
        cnst.KernelName := info.Constant[I].Name;
        cnst.Name := cnst.KernelName;
        cnst.DataType := info.Constant[I].DataType;
        cnst.CustomType := info.Constant[I].CustomType;
        cnst.IsValueDefined := info.Constant[I].DefValue;
      end;

      try
        bUseless := cnst.DeviceAddress = nil;
      except
        bUseless := True;
      end;
      if bUseless then
      begin
        Designer.SelectComponent(cnst);
        Designer.DeleteSelection(True);
      end;
    end;

    // Delete useless components
    SetLength(useless, parent.ItemsCount);
    j := 0;
    for i := 0 to parent.ItemsCount - 1 do
    begin
      if not TCUDAComponent(parent.Items[i]).IsAllocated then
        begin
          useless[j] := parent.Items[i];
          inc(j);
        end;
    end;

    for i := 0 to j - 1 do
      useless[i].Destroy;
  end;
  Designer.Modified;
end;

procedure TGLSCUDACompilerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TGLSCUDACompilerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Compile Module';
  end;
end;

function TGLSCUDACompilerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

// ------------------
// ------------------ TGLSCUDACompilerSourceProperty ------------------
// ------------------

{$IFNDEF FPC}
constructor TGLSCUDACompilerSourceProperty.Create(
    const ADesigner: IDesigner; APropCount: Integer);
{$ELSE}
constructor TGLSCUDACompilerSourceProperty.Create(
       Hook:TPropertyEditorHook; APropCount: Integer);
{$ENDIF}
begin
  inherited;
  FModuleList := TStringList.Create;
end;

// Destroy
//

destructor TGLSCUDACompilerSourceProperty.Destroy;
begin
  FModuleList.Destroy;
  inherited;
end;

// RefreshModuleList
//

procedure TGLSCUDACompilerSourceProperty.RefreshModuleList;
var
  proj: IOTAProject;
  I: Integer;
  LModule: IOTAModuleInfo;
  LName: string;
begin
  FModuleList.Clear;
  FModuleList.Add('none');
  proj := GetActiveProject;
  if proj <> nil then
  begin
    for I := 0 to proj.GetModuleCount - 1 do
    begin
      LModule := proj.GetModule(I);
      LName := UpperCase(ExtractFileExt(LModule.FileName));
      if LName = '.CU' then
        FModuleList.Add(LModule.FileName);
    end;
  end;
end;

// GetAttributes
//

function TGLSCUDACompilerSourceProperty.GetAttributes;
begin
  Result := [paValueList];
end;

// GetValues
//
procedure TGLSCUDACompilerSourceProperty.GetValues(Proc: TGetStrProc);
var
   I : Integer;
begin
  RefreshModuleList;
  for I := 0 to FModuleList.Count - 1 do
      Proc(ExtractFileName(FModuleList[I]));
end;

// SetValue
//
procedure TGLSCUDACompilerSourceProperty.SetValue(const Value: String);
var
  I, J: Integer;
begin
  RefreshModuleList;
  J := -1;
  for I := 1 to FModuleList.Count - 1 do
    if Value = ExtractFileName(FModuleList[I]) then
    begin
      J := I;
      Break;
    end;

  if J > 0 then
  begin
    TGLSCUDACompiler(GetComponent(0)).SetSourceCodeFile(FModuleList[J]);
    SetStrValue(ExtractFileName(Value));
  end
  else
  begin
    SetStrValue('none');
  end;
	Modified;
end;

// ------------------
// ------------------ TGLSCUDADeviceProperty ------------------
// ------------------

{$IFNDEF FPC}
constructor TGLSCUDADeviceProperty.Create(const ADesigner: IDesigner; APropCount: Integer);
{$ELSE}
constructor TGLSCUDADeviceProperty.Create(Hook: TPropertyEditorHook; APropCount: Integer);
{$ENDIF}
begin
  inherited;
  FDeviceList := TStringList.Create;
end;

destructor TGLSCUDADeviceProperty.Destroy;
begin
  FDeviceList.Destroy;
  inherited;
end;

function TGLSCUDADeviceProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TGLSCUDADeviceProperty.GetValues(Proc: TGetStrProc);
begin
  CUDAContextManager.FillUnusedDeviceList(FDeviceList);
end;

procedure TGLSCUDADeviceProperty.SetValue(const Value: String);
var
  I: Integer;
begin
  for I := 0 to FDeviceList.Count - 1 do
    if Value = FDeviceList[I] then
    begin
      SetStrValue(Value);
      Break;
    end;
  Modified;
end;

initialization

  vFindCuFileFunc := FindCuFile;

end.

