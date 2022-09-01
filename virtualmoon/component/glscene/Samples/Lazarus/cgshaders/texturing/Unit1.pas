{
  Cg Multi-Texturing Demo

  Shows how to do texture coordinate shifting with a VP and blending with a FP.

  Last update: 09/02/04
  Nelson Chu
}
unit Unit1;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLLCLViewer, GLTexture,
  GLCgShader, Cg, cgGL, StdCtrls, GLVectorGeometry, GLCadencer, ExtCtrls, ComCtrls,
  GLGraph, GLCrossPlatform, GLMaterial, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    CgShader1: TCgShader;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Splitter1: TSplitter;
    Panel2: TPanel;
    CBVertexProgram: TCheckBox;
    LabelVertProfile: TLabel;
    Panel4: TPanel;
    LabelFragProfile: TLabel;
    CheckBox1: TCheckBox;
    Splitter2: TSplitter;
    Panel6: TPanel;
    Panel7: TPanel;
    MemoFragCode: TMemo;
    Panel8: TPanel;
    Memo3: TMemo;
    Panel3: TPanel;
    ButtonApplyFP: TButton;
    Panel11: TPanel;
    Panel12: TPanel;
    MemoVertCode: TMemo;
    Panel13: TPanel;
    ButtonApplyVP: TButton;
    Splitter3: TSplitter;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Panel5: TPanel;
    Label2: TLabel;
    Memo1: TMemo;
    Button1: TButton;
    Button4: TButton;
    Panel9: TPanel;
    Panel10: TPanel;
    GLSceneViewer1: TGLSceneViewer;
    Timer1: TTimer;
    GLXYZGrid1: TGLXYZGrid;
    GLPlane1: TGLPlane;
    GLMatLib: TGLMaterialLibrary;
    TabSheet3: TTabSheet;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    TrackBar1: TTrackBar;
    Label4: TLabel;
    TrackBar2: TTrackBar;
    Label5: TLabel;
    TrackBar3: TTrackBar;
    Label6: TLabel;
    TrackBar4: TTrackBar;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    TrackBar7: TTrackBar;
    TrackBar8: TTrackBar;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    CheckBox2: TCheckBox;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure FormCreate(Sender: TObject);
    procedure CBVertexProgramClick(Sender: TObject);
    procedure CBFragmentProgramClick(Sender: TObject);
    procedure ButtonApplyFPClick(Sender: TObject);
    procedure MemoFragCodeChange(Sender: TObject);
    procedure MemoVertCodeChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ButtonApplyVPClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure CgShader1ApplyVP(CgProgram: TCgProgram; Sender: TObject);
    procedure CgShader1ApplyFP(CgProgram: TCgProgram; Sender: TObject);
    procedure CgShader1UnApplyFP(CgProgram: TCgProgram);
    procedure CgShader1Initialize(CgShader: TCustomCgShader);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  FileUtil;

procedure TForm1.FormCreate(Sender: TObject);
var
  path: UTF8String;
  p: integer;
begin
  path := ExtractFilePath(ParamStrUTF8(0));
  // load Cg proggy
  with CgShader1 do
  begin
    VertexProgram.LoadFromFile('cg_texture_vp.cg');
    MemoVertCode.Lines.Assign(VertexProgram.Code);

    FragmentProgram.LoadFromFile('cg_texture_fp.cg');
    MemoFragCode.Lines.Assign(FragmentProgram.Code);
  end;
  p := Pos('DemosLCL', path);
  Delete(path, p + 5, Length(path));
  path := IncludeTrailingPathDelimiter(path) + 'media';
  SetCurrentDirUTF8(path);


  with GLMatLib do
  begin
    Materials[0].Material.Texture.Image.LoadFromFile('moon.bmp');
    Materials[1].Material.Texture.Image.LoadFromFile('clover.jpg');
    Materials[2].Material.Texture.Image.LoadFromFile('marbletiles.jpg');
    Materials[3].Material.Texture.Image.LoadFromFile('chrome_buckle.bmp');
  end;
end;

procedure TForm1.CgShader1Initialize(CgShader: TCustomCgShader);
begin
  // Due to parameter shadowing (ref. Cg Manual), parameters that doesn't change
  // once set can be assigned for once in the OnInitialize event.
  with CgShader1.FragmentProgram, GLMatLib do
  begin
    ParamByName('Map0').SetToTextureOf(Materials[0]);
    ParamByName('Map1').SetToTextureOf(Materials[1]);
    ParamByName('Map2').SetToTextureOf(Materials[2]);
    ParamByName('Map3').SetToTextureOf(Materials[3]);
    // Alternatively, you can set texture parameters using two other methods:
    // SetTexture('Map0', Materials[0].Material.Texture.Handle);
    // ParamByName('Map0').SetAsTexture2D(Materials[0].Material.Texture.Handle);
  end;

  // Display profiles used
  LabelVertProfile.Caption := 'Using profile: ' + CgShader1.VertexProgram.GetProfileString;
  LabelFragProfile.Caption := 'Using profile: ' +
    CgShader1.FragmentProgram.GetProfileString;
end;

procedure TForm1.CgShader1ApplyVP(CgProgram: TCgProgram; Sender: TObject);
var
  v: TVector;

  function conv(TrackBar: TTrackBar): single;
  var
    half: integer;
  begin
    half := TrackBar.max div 2;
    Result := (TrackBar.Position - half) / half;
  end;

begin
  with CgProgram.ParamByName('ModelViewProj') do
    SetAsStateMatrix(CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);
  // Alternatively, you can set it using:
  // CgProgram.SetStateMatrix('ModelViewProj', CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);

  v := vectormake(conv(TrackBar1), conv(TrackBar2), conv(TrackBar3), conv(TrackBar4));

  CgProgram.ParamByName('shifts').SetAsVector(v);
end;

procedure TForm1.CgShader1ApplyFP(CgProgram: TCgProgram; Sender: TObject);
var
  v: TVector;

  function conv(TrackBar: TTrackBar): single;
  var
    half: integer;
  begin
    half := TrackBar.max div 2;
    Result := (TrackBar.Position - half) / half;
  end;

begin
  with CgProgram do
  begin
    ParamByName('Map0').EnableTexture;
    ParamByName('Map1').EnableTexture;
    ParamByName('Map2').EnableTexture;
    ParamByName('Map3').EnableTexture;
  end;

  v := vectormake(conv(TrackBar5), conv(TrackBar6), conv(TrackBar7), conv(TrackBar8));

  CgProgram.ParamByName('weights').SetAsVector(v);
end;

procedure TForm1.CgShader1UnApplyFP(CgProgram: TCgProgram);
begin
  with CgProgram do
  begin
    ParamByName('Map0').DisableTexture;
    ParamByName('Map1').DisableTexture;
    ParamByName('Map2').DisableTexture;
    ParamByName('Map3').DisableTexture;
  end;
end;

// Code below takes care of the UI

procedure TForm1.CBVertexProgramClick(Sender: TObject);
begin
  CgShader1.VertexProgram.Enabled := (Sender as TCheckBox).Checked;
end;

procedure TForm1.CBFragmentProgramClick(Sender: TObject);
begin
  CgShader1.FragmentProgram.Enabled := (Sender as TCheckBox).Checked;
end;

procedure TForm1.ButtonApplyFPClick(Sender: TObject);
begin
  CgShader1.FragmentProgram.Code := MemoFragCode.Lines;
  (Sender as TButton).Enabled := False;
end;

procedure TForm1.ButtonApplyVPClick(Sender: TObject);
begin
  CgShader1.VertexProgram.Code := MemoVertCode.Lines;
  (Sender as TButton).Enabled := False;
end;

procedure TForm1.MemoFragCodeChange(Sender: TObject);
begin
  ButtonApplyFP.Enabled := True;
end;

procedure TForm1.MemoVertCodeChange(Sender: TObject);
begin
  ButtonApplyVP.Enabled := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CgShader1.VertexProgram.ListParameters(Memo1.Lines);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CgShader1.FragmentProgram.ListParameters(Memo3.Lines);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CgShader1.FragmentProgram.ListCompilation(Memo3.Lines);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  CgShader1.VertexProgram.ListCompilation(Memo1.Lines);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  mx := x;
  my := y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if Shift <> [] then
  begin
    GLCamera1.MoveAroundTarget(my - y, mx - x);
    mx := x;
    my := y;
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  with GLSceneViewer1 do
    if PtInRect(ClientRect, ScreenToClient(MousePos)) then
    begin
      GLCamera1.SceneScale := GLCamera1.SceneScale * (1000 - WheelDelta) / 1000;
      Handled := True;
    end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  with GLSceneViewer1 do
  begin
    Caption := Format('Cg Shader Demo - %.1f fps', [FramesPerSecond]);
    ResetPerformanceMonitor;
  end;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  if key = #27 then
    Close;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  CgShader1.Enabled := CheckBox2.Checked;
end;

end.

