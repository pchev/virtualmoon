{ : Moving objects with the mouse.<p>

  In this demo you can move the two cubes around by picking and dragging
  them. This showcases the use of ScreenVectorIntersectXxxx functions.<p>

  You can also use the numeric keypad to move/zoom the camera and the arrow
  to move the selected object around.<p>

  (Based on Rado Stoyanov's test project)
}
unit Unit1;

interface

uses
  Forms, Dialogs, SysUtils, GLScene, GLObjects, Classes, Controls,
  GLGraph,
  GLCollision, GLTexture, StdCtrls, ExtCtrls, GLVectorGeometry, Graphics,
  GLVectorFileObjects, GLLCLViewer, GLSpaceText, GLGeomObjects, GLColor,
  GLCrossPlatform, GLCoordinates, GLBaseClasses, GLBitmapFont, GLWindowsFont,
  GLHUDObjects;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    Scn: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    ZArrow: TGLArrowLine;
    XArrow: TGLArrowLine;
    YArrow: TGLArrowLine;
    Cube1: TGLCube;
    TopLight1: TGLLightSource;
    Cube2: TGLCube;
    Floor: TGLCube;
    Panel1: TPanel;
    Button1: TButton;
    Label2: TLabel;
    Label1: TLabel;
    TxtX: TGLSpaceText;
    TxtY: TGLSpaceText;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    TxtZ: TGLSpaceText;
    TopText: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    ObjText: TGLHUDText;
    GroupBox1: TGroupBox;
    ShowAxes: TCheckBox;
    procedure ScnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure ShowAxesClick(Sender: TObject);
  private
    lastMouseWorldPos: TVector;
    movingOnZ: Boolean;
    CurrentPick: TGLCustomSceneObject;
    ScnMouseMoveCnt: Integer;
    function MouseWorldPos(X, Y: Integer): TVector;
    procedure UpdateHudText;
    procedure ProcessPick(pick: TGLBaseSceneObject);
  end;

const
  SelectionColor: TColorVector = (X: 0.243; Y: 0.243; Z: 0.243; W: 1.000);

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LCLType;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateHudText;
end;

function TForm1.MouseWorldPos(X, Y: Integer): TVector;
var
  v: TVector;
begin
  Y := Scn.Height - Y;
  if Assigned(CurrentPick) then
  begin
    SetVector(v, X, Y, 0);
    if movingOnZ then
      Scn.Buffer.ScreenVectorIntersectWithPlaneXZ(v, CurrentPick.Position.Y,
        Result)
    else
      Scn.Buffer.ScreenVectorIntersectWithPlaneXY(v, CurrentPick.Position.Z,
        Result);
  end
  else
    SetVector(Result, NullVector);
end;

procedure TForm1.ProcessPick(pick: TGLBaseSceneObject);
begin
  if Assigned(pick) then
  begin
    // Only Cube1 and Cube2 can be selected
    if (pick.Name <> 'Cube1') and (pick.Name <> 'Cube2') then
      pick := nil;
  end;
  if pick <> CurrentPick then
  begin
    if Assigned(CurrentPick) then
    begin
      CurrentPick.ShowAxes := false;
      CurrentPick.Material.FrontProperties.Emission.Color := clrBlack;
    end;
    CurrentPick := TGLCustomSceneObject(pick);
    if Assigned(CurrentPick) then
    begin
      if ShowAxes.Checked then
        CurrentPick.ShowAxes := true;
      CurrentPick.Material.FrontProperties.Emission.Color := SelectionColor;
    end;
  end;
  UpdateHudText;
end;

procedure TForm1.ScnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pick: TGLBaseSceneObject;
begin
  movingOnZ := (ssShift in Shift);
  // If an object is picked...
  pick := (Scn.Buffer.GetPickedObject(X, Y) as TGLCustomSceneObject);
  ProcessPick(Pick);

  // store mouse pos
  if Assigned(CurrentPick) then
    lastMouseWorldPos := MouseWorldPos(X, Y);
end;

procedure TForm1.ScnMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  newPos: TVector;
begin
  Inc(ScnMouseMoveCnt);
  Assert(ScnMouseMoveCnt < 2);
  if ssLeft in Shift then
  begin
    // handle hold/unhold of shift
    if (ssShift in Shift) <> movingOnZ then
    begin
      movingOnZ := (ssShift in Shift);
      lastMouseWorldPos := MouseWorldPos(X, Y);
    end;
    newPos := MouseWorldPos(X, Y);
    if Assigned(CurrentPick) and (VectorNorm(lastMouseWorldPos) <> 0) then
      CurrentPick.Position.Translate(VectorSubtract(newPos, lastMouseWorldPos));
    lastMouseWorldPos := newPos;

    UpdateHudText;
  end;
  Dec(ScnMouseMoveCnt);
end;

procedure TForm1.ShowAxesClick(Sender: TObject);
begin
  // Unselect all
  ProcessPick(nil);
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // Note that 1 wheel-step induces a WheelDelta of 120,
  // this code adjusts the distance to target with a 10% per wheel-step ratio
  if WheelDelta <> 0 then
    GLCamera1.AdjustDistanceToTarget(Power(1.1, -WheelDelta / 120));
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  with GLCamera1 do
    case Key of
      '2':
        MoveAroundTarget(3, 0);
      '4':
        MoveAroundTarget(0, -3);
      '6':
        MoveAroundTarget(0, 3);
      '8':
        MoveAroundTarget(-3, 0);
      '-':
        AdjustDistanceToTarget(1.1);
      '+':
        AdjustDistanceToTarget(1 / 1.1);
    end;
end;

procedure TForm1.UpdateHudText;
var
  objPos, winPos: TAffineVector;
begin
  if Assigned(CurrentPick) then
  begin
    SetVector(objPos, CurrentPick.AbsolutePosition);

    TopText.Text := Format(
      'New Object Position: Xn: %4.4f, Yn: %4.4f, Zn: %4.4f',
      [objPos.X, objPos.Y, objPos.Z]);

    winPos := Scn.Buffer.WorldToScreen(objPos);

    with ObjText do
    begin
      Visible := true;
      Text := CurrentPick.Name;
      Position.X := winPos.X + 10;
      Position.Y := Scn.Height - winPos.Y + 10;
    end;
  end
  else
  begin
    TopText.Text := 'No selected object';
    ObjText.Visible := false;
  end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(CurrentPick) then
    with CurrentPick do
      case Key of
        VK_UP:
          if ssShift in Shift then
            Translate(0, 0, 0.3)
          else
            Translate(-0.3, 0, 0);
        VK_DOWN:
          if ssShift in Shift then
            Translate(0, 0, -0.3)
          else
            Translate(0.3, 0, 0);
        VK_LEFT:
          Translate(0, -0.3, 0);
        VK_RIGHT:
          Translate(0, 0.3, 0);
      end;
end;

end.
