unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLTeapot, GLObjects, GLLCLViewer,
  GLVectorGeometry, GLTexture, GLCadencer, GLImposter, StdCtrls, ExtCtrls, GLSkydome,
  GLCrossPlatform, GLCoordinates, GLRenderContextInfo;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLTeapot1: TGLTeapot;
    GLLightSource1: TGLLightSource;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    GLSkyDome1: TGLSkyDome;
    GLDummyCube1: TGLDummyCube;
    Panel1: TPanel;
    Label1: TLabel;
    CBShowTeapot: TCheckBox;
    CBShowImposter: TCheckBox;
    CBSampleSize: TComboBox;
    Label2: TLabel;
    procedure GLDirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure CBSampleSizeChange(Sender: TObject);
    procedure CBShowImposterClick(Sender: TObject);
    procedure CBShowTeapotClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    impBuilder: TGLStaticImposterBuilder;
    renderPoint: TGLRenderPoint;
    mx, my: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
//var
//   x, y : Integer;
begin
  renderPoint := TGLRenderPoint(GLDummyCube1.AddNewChild(TGLRenderPoint));

  impBuilder := TGLStaticImposterBuilder.Create(Self);
  impBuilder.SampleSize := 64;
  impBuilder.SamplingRatioBias := 1.3;
  impBuilder.Coronas.Items[0].Samples := 32;
  impBuilder.Coronas.Add(15, 24);
  impBuilder.Coronas.Add(30, 24);
  impBuilder.Coronas.Add(45, 16);
  impBuilder.Coronas.Add(60, 16);
  impBuilder.Coronas.Add(85, 16);
  impBuilder.RenderPoint := renderPoint;

  impBuilder.RequestImposterFor(GLTeapot1);
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
var
  camPos, pos: TVector;
  imp: TImposter;
  x, y: integer;
begin
  imp := impBuilder.ImposterFor(GLTeapot1);
  if (imp = nil) or (imp.Texture.Handle = 0) then
    Exit;

  imp.BeginRender(rci);
  for x := -30 to 30 do
    for y := -30 to 30 do
    begin
      MakePoint(pos, x * 5, 0, y * 4);
      camPos := VectorSubtract(rci.cameraPosition, pos);
      imp.Render(rci, pos, camPos, 1);
    end;
  imp.EndRender(rci);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := GLSceneViewer1.FramesPerSecondText;
  if CBShowImposter.Checked then
    Caption := Caption + ' - 3721 imposters';
  GLSceneViewer1.ResetPerformanceMonitor;

  Label1.Caption := Format('%d x %d - %.1f%%',
    [impBuilder.TextureSize.X, impBuilder.TextureSize.Y,
    impBuilder.TextureFillRatio * 100]);

end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(my - y, mx - x);
  end;
  mx := x;
  my := y;
end;

procedure TForm1.CBSampleSizeChange(Sender: TObject);
var
  s: integer;
begin
  s := StrToInt(CBSampleSize.Text);
  if (GLSceneViewer1.Width >= s) and (GLSceneViewer1.Height >= s) then
    impBuilder.SampleSize := s
  else
  begin
    ShowMessage('Viewer is too small to allow rendering the imposter samples');
  end;
end;

procedure TForm1.CBShowImposterClick(Sender: TObject);
begin
  GLDirectOpenGL1.Visible := CBShowImposter.Checked;
end;

procedure TForm1.CBShowTeapotClick(Sender: TObject);
begin
  GLTeapot1.Visible := CBShowTeapot.Checked;
end;

end.

