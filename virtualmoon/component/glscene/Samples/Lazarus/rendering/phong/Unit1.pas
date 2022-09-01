{: Per-Pixel phong shading demo.<p>

   The TGLPhongShader implements phong shading through the use of an
   ARB vertex and fragment program. So far only the material and light
   properties are supported, some form of texture support will be
   added in future updates.<p>

}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLTeapot, GLTexture, GLPhongShader,
  GLLCLViewer, GLAsyncTimer, GLCadencer, StdCtrls, GLCustomShader,
  GLAsmShader, GLCrossPlatform, GLMaterial, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLPhongShader1: TGLPhongShader;
    GLCamera1: TGLCamera;
    GLTeapot1: TGLTeapot;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    AsyncTimer1: TGLAsyncTimer;
    CheckBox1: TCheckBox;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=x;
  my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my-y, mx-x);
  mx:=x;
  my:=y;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  Form1.Caption:='Phong Shader - '+GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  GLPhongShader1.Enabled:=CheckBox1.Checked;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  CheckBox1.Checked:=GLPhongShader1.Enabled;
  GLSceneViewer1.Invalidate;
end;

end.