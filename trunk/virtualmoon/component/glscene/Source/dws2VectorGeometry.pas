// dws2VectorGeometry
{: DelphiWebScriptII symbol creation for VectorGeometry types
   and functions.<p>

   <b>History : </b><font size=-1><ul>
      <li>27/04/2004 - SG - Creation
   </ul></font>
}
unit dws2VectorGeometry;

interface

uses
  Classes, dws2Exprs, dws2Symbols, dws2Comp;

type
  Tdws2VectorGeometryUnit = class(Tdws2UnitComponent)
    protected
      procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;
    public
      constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

uses
  dws2Functions, VectorGeometry;

type
  TVectorMakeFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TSetVectorFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorAddFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorSubtractFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorScaleFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCombineVectorFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorCombineFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorCombine3Function = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorDotProductFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorCrossProductFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorNormalizeFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorTransformFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TInvertMatrixFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TTransposeMatrixFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TMatrixMultiplyFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCreateScaleMatrixFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;
  
  TCreateTranslationMatrixFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCreateScaleAndTranslationMatrixFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCreateRotationMatrixXFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCreateRotationMatrixYFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCreateRotationMatrixZFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCreateRotationMatrixFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;


procedure Register;
begin
  RegisterComponents('GLScene DWS2', [Tdws2VectorGeometryUnit]);
end;

function GetVectorFromInfo(Info : IInfo) : TVector;
begin
  Result:=VectorMake(Info.Element([0]).Value,
                     Info.Element([1]).Value,
                     Info.Element([2]).Value,
                     Info.Element([3]).Value);
end;

procedure SetInfoFromVector(Info : IInfo; vec : TVector);
var
  i : Integer;
begin
  for i:=0 to 3 do
    Info.Element([i]).Value:=vec[i];
end;

function GetMatrixFromInfo(Info : IInfo) : TMatrix;
var
  i : Integer;
begin
  for i:=0 to 3 do
    Result[i]:=VectorMake(Info.Element([i]).Element([0]).Value,
                          Info.Element([i]).Element([1]).Value,
                          Info.Element([i]).Element([2]).Value,
                          Info.Element([i]).Element([3]).Value);
end;

procedure SetInfoFromMatrix(Info : IInfo; mat : TMatrix);
var
  i,j : Integer;
begin
  for i:=0 to 3 do
    for j:=0 to 3 do
      Info.Element([i]).Element([j]).Value:=mat[i][j];
end;

// ----------
// ---------- Tdws2VectorGeometryUnit ----------
// ----------

procedure Tdws2VectorGeometryUnit.AddUnitSymbols(SymbolTable: TSymbolTable);
var
  FloatSymbol : TSymbol;
begin
  FloatSymbol:=SymbolTable.FindSymbol('Float');

  // Array types
  SymbolTable.AddSymbol(TStaticArraySymbol.Create('TVector', FloatSymbol, 0, 3));
  SymbolTable.AddSymbol(TStaticArraySymbol.Create('TMatrix', SymbolTable.FindSymbol('TVector'), 0, 3));

  // Vector functions
  TVectorMakeFunction.Create(SymbolTable, 'VectorMake', ['x', 'Float', 'y', 'Float', 'z', 'Float', 'w', 'Float'], 'TVector');
  TSetVectorFunction.Create(SymbolTable, 'SetVector', ['@v', 'TVector', 'x', 'Float', 'y', 'Float', 'z', 'Float', 'w', 'Float'], '');
  TVectorAddFunction.Create(SymbolTable, 'VectorAdd', ['v1', 'TVector', 'v2', 'TVector'], 'TVector');
  TVectorSubtractFunction.Create(SymbolTable, 'VectorSubtract', ['v1', 'TVector', 'v2', 'TVector'], 'TVector');
  TVectorScaleFunction.Create(SymbolTable, 'VectorScale', ['v', 'TVector', 'f', 'Float'], 'TVector');
  TCombineVectorFunction.Create(SymbolTable, 'CombineVector', ['@vr', 'TVector', 'v', 'TVector', '@f', 'Float'], '');
  TVectorCombineFunction.Create(SymbolTable, 'VectorCombine', ['v1', 'TVector', 'v2', 'TVector', 'f1', 'Float', 'f2', 'Float'], 'TVector');
  TVectorCombine3Function.Create(SymbolTable, 'VectorCombine3', ['v1', 'TVector', 'v2', 'TVector', 'v3', 'TVector', 'f1', 'Float', 'f2', 'Float', 'f3', 'Float'], 'TVector');
  TVectorDotProductFunction.Create(SymbolTable, 'VectorDotProduct', ['v1', 'TVector', 'v2', 'TVector'], 'Float');
  TVectorCrossProductFunction.Create(SymbolTable, 'VectorCrossProduct', ['v1', 'TVector', 'v2', 'TVector'], 'TVector');
  TVectorNormalizeFunction.Create(SymbolTable, 'VectorNormalize', ['v', 'TVector'], 'TVector');
  TVectorTransformFunction.Create(SymbolTable, 'VectorTransform', ['v', 'TVector', 'm', 'TMatrix'], 'TVector');

  // Matrix function
  TInvertMatrixFunction.Create(SymbolTable, 'InvertMatrix', ['@mat', 'TMatrix'], '');
  TTransposeMatrixFunction.Create(SymbolTable, 'TransposeMatrix', ['@mat', 'TMatrix'], '');
  TMatrixMultiplyFunction.Create(SymbolTable, 'MatrixMultiply', ['m1', 'TMatrix', 'm2', 'TMatrix'], 'TMatrix');
  TCreateScaleMatrixFunction.Create(SymbolTable, 'CreateScaleMatrix', ['v', 'TVector'], 'TMatrix');
  TCreateTranslationMatrixFunction.Create(SymbolTable, 'CreateTranslationMatrix', ['v', 'TVector'], 'TMatrix');
  TCreateScaleAndTranslationMatrixFunction.Create(SymbolTable, 'CreateScaleAndTranslationMatrix', ['scale', 'TVector', 'offset', 'TVector'], 'TMatrix');
  TCreateRotationMatrixXFunction.Create(SymbolTable, 'CreateRotationMatrixX', ['angle', 'Float'], 'TMatrix');
  TCreateRotationMatrixYFunction.Create(SymbolTable, 'CreateRotationMatrixY', ['angle', 'Float'], 'TMatrix');
  TCreateRotationMatrixZFunction.Create(SymbolTable, 'CreateRotationMatrixZ', ['angle', 'Float'], 'TMatrix');
  TCreateRotationMatrixFunction.Create(SymbolTable, 'CreateRotationMatrix', ['anAxis', 'TVector', 'angle', 'Float'], 'TMatrix');
end;

constructor Tdws2VectorGeometryUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName := 'VectorGeometry';
end;

// ----------
// ---------- TVectorMakeFunction ----------
// ----------

procedure TVectorMakeFunction.Execute;
begin
  Info.Vars['Result'].Element([0]).Value:=Info['x'];
  Info.Vars['Result'].Element([1]).Value:=Info['y'];
  Info.Vars['Result'].Element([2]).Value:=Info['z'];
  Info.Vars['Result'].Element([3]).Value:=Info['w'];
end;

// ----------
// ---------- TSetVectorFunction ----------
// ----------

procedure TSetVectorFunction.Execute;
begin
  Info.Vars['v'].Element([0]).Value:=Info['x'];
  Info.Vars['v'].Element([1]).Value:=Info['y'];
  Info.Vars['v'].Element([2]).Value:=Info['z'];
  Info.Vars['v'].Element([3]).Value:=Info['w'];
end;

// ----------
// ---------- TVectorAddFunction ----------
// ----------

procedure TVectorAddFunction.Execute;
var
  v1,v2,vr : TVector;
begin
  v1:=GetVectorFromInfo(Info.Vars['v1']);
  v2:=GetVectorFromInfo(Info.Vars['v2']);
  VectorAdd(v1,v2,vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

// ----------
// ---------- TVectorSubtractFunction ----------
// ----------

procedure TVectorSubtractFunction.Execute;
var
  v1,v2,vr : TVector;
begin
  v1:=GetVectorFromInfo(Info.Vars['v1']);
  v2:=GetVectorFromInfo(Info.Vars['v2']);
  VectorSubtract(v1,v2,vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

// ----------
// ---------- TVectorScaleFunction ----------
// ----------

procedure TVectorScaleFunction.Execute;
var
  v,vr : TVector;
  f : Single;
begin
  v:=GetVectorFromInfo(Info.Vars['v']);
  f:=Info['f'];
  VectorScale(v,f,vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

// ----------
// ---------- TCombineVectorFunction ----------
// ----------

procedure TCombineVectorFunction.Execute;
var
  vr,v : TVector;
  f : Single;
begin
  vr:=GetVectorFromInfo(Info.Vars['vr']);
  v:=GetVectorFromInfo(Info.Vars['v']);
  f:=Info['f'];
  CombineVector(vr,v,f);
  SetInfoFromVector(Info.Vars['Result'], vr);
  Info.Vars['f'].Value:=f;
end;

// ----------
// ---------- TVectorCombineFunction ----------
// ----------

procedure TVectorCombineFunction.Execute;
var
  v1,v2,vr : TVector;
  f1,f2 : Single;
begin
  v1:=GetVectorFromInfo(Info.Vars['v1']);
  v2:=GetVectorFromInfo(Info.Vars['v2']);
  f1:=Info['f1'];
  f2:=Info['f2'];
  VectorCombine(v1,v2,f1,f2,vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

// ----------
// ---------- TVectorCombine3Function ----------
// ----------

procedure TVectorCombine3Function.Execute;
var
  v1,v2,v3,vr : TVector;
  f1,f2,f3 : Single;
begin
  v1:=GetVectorFromInfo(Info.Vars['v1']);
  v2:=GetVectorFromInfo(Info.Vars['v2']);
  v3:=GetVectorFromInfo(Info.Vars['v3']);
  f1:=Info['f1'];
  f2:=Info['f2'];
  f3:=Info['f3'];
  VectorCombine3(v1,v2,v3,f1,f2,f3,vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

// ----------
// ---------- TVectorDotProductFunction ----------
// ----------

procedure TVectorDotProductFunction.Execute;
var
  v1,v2 : TVector;
begin
  v1:=GetVectorFromInfo(Info.Vars['v1']);
  v2:=GetVectorFromInfo(Info.Vars['v2']);
  Info.Result:=VectorDotProduct(v1, v2);
end;

// ----------
// ---------- TVectorCrossProductFunction ----------
// ----------

procedure TVectorCrossProductFunction.Execute;
var
  v1,v2,vr : TVector;
begin
  v1:=GetVectorFromInfo(Info.Vars['v1']);
  v2:=GetVectorFromInfo(Info.Vars['v2']);
  VectorCrossProduct(v1, v2, vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

// ----------
// ---------- TVectorNormalizeFunction ----------
// ----------

procedure TVectorNormalizeFunction.Execute;
var
  v,vr : TVector;
begin
  v:=GetVectorFromInfo(Info.Vars['v']);
  vr:=VectorNormalize(v);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

// ----------
// ---------- TVectorTransformFunction ----------
// ----------

procedure TVectorTransformFunction.Execute;
var
  v,vr : TVector;
  mat : TMatrix;
begin
  v:=GetVectorFromInfo(Info.Vars['v']);
  mat:=GetMatrixFromInfo(Info.Vars['mat']);
  vr:=VectorTransform(v, mat);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

// ----------
// ---------- TInvertMatrixFunction ----------
// ----------

procedure TInvertMatrixFunction.Execute;
var
  mat : TMatrix;
begin
  mat:=GetMatrixFromInfo(Info.Vars['mat']);
  InvertMatrix(mat);
  SetInfoFromMatrix(Info.Vars['Result'], mat);
end;

// ----------
// ---------- TTransposeMatrixFunction ----------
// ----------

procedure TTransposeMatrixFunction.Execute;
var
  mat : TMatrix;
begin
  mat:=GetMatrixFromInfo(Info.Vars['mat']);
  TransposeMatrix(mat);
  SetInfoFromMatrix(Info.Vars['Result'], mat);
end;

// ----------
// ---------- TMatrixMultiplyFunction ----------
// ----------

procedure TMatrixMultiplyFunction.Execute;
var
  m1,m2,mr : TMatrix;
begin
  m1:=GetMatrixFromInfo(Info.Vars['m1']);
  m2:=GetMatrixFromInfo(Info.Vars['m2']);
  MatrixMultiply(m1,m2,mr);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

// ----------
// ---------- TCreateScaleMatrixFunction ----------
// ----------

procedure TCreateScaleMatrixFunction.Execute;
var
  v : TVector;
  mr : TMatrix;
begin
  v:=GetVectorFromInfo(Info.Vars['v']);
  mr:=CreateScaleMatrix(v);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

// ----------
// ---------- TCreateTranslationMatrixFunction ----------
// ----------

procedure TCreateTranslationMatrixFunction.Execute;
var
  v : TVector;
  mr : TMatrix;
begin
  v:=GetVectorFromInfo(Info.Vars['v']);
  mr:=CreateTranslationMatrix(v);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

// ----------
// ---------- TCreateScaleAndTranslationMatrixFunction ----------
// ----------

procedure TCreateScaleAndTranslationMatrixFunction.Execute;
var
  scale, offset : TVector;
  mr : TMatrix;
begin
  scale:=GetVectorFromInfo(Info.Vars['scale']);
  offset:=GetVectorFromInfo(Info.Vars['offset']);
  mr:=CreateScaleAndTranslationMatrix(scale, offset);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

// ----------
// ---------- TCreateRotationMatrixXFunction ----------
// ----------

procedure TCreateRotationMatrixXFunction.Execute;
var
  angle : Single;
  mr : TMatrix;
begin
  angle:=Info['angle'];
  mr:=CreateRotationMatrixX(angle);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

// ----------
// ---------- TCreateRotationMatrixYFunction ----------
// ----------

procedure TCreateRotationMatrixYFunction.Execute;
var
  angle : Single;
  mr : TMatrix;
begin
  angle:=Info['angle'];
  mr:=CreateRotationMatrixY(angle);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

// ----------
// ---------- TCreateRotationMatrixZFunction ----------
// ----------

procedure TCreateRotationMatrixZFunction.Execute;
var
  angle : Single;
  mr : TMatrix;
begin
  angle:=Info['angle'];
  mr:=CreateRotationMatrixZ(angle);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

// ----------
// ---------- TCreateRotationMatrixFunction ----------
// ----------

procedure TCreateRotationMatrixFunction.Execute;
var
  angle : Single;
  anAxis : TVector;
  mr : TMatrix;
begin
  anAxis:=GetVectorFromInfo(Info.Vars['anAxis']);
  angle:=Info['angle'];
  mr:=CreateRotationMatrix(anAxis, angle);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

end.
