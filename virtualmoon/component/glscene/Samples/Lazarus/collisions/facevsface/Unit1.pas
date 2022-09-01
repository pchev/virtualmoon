{: Face vs Face collision detection.<p>

   This sample illustrates:<ul>
   <li>collisions between FreeForm Objects (Triangle-based)
   <li>collisions between Cubes
   <li>basic user-driven camera movements.
   <li>picking Objects
   <li>moving Objects
   </ul></li><p>

   Usage:<ul>
   <li>left Mouse will move Camera
   <li>right Mouse will move an object
   <li>left Mouse + shift will roll and pitch the object
   <li>Wheel scroll will zoom in/out
   </ul><p>
   Bernd Klaiber.
   (modified by DanB 08/07/2003)
}
unit Unit1;

{$MODE Delphi}

interface

uses
  Windows, Forms, GLScene, GLObjects, Classes, Controls, SysUtils, Graphics,
  GLLCLViewer, ExtCtrls, GLVectorGeometry, StdCtrls, GLSpaceText,
  ComCtrls, GLCollision, GLVectorFileObjects, GLCrossPlatform, GLVectorLists,
  Grids, GLFile3DS, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    Timer1: TTimer;
    GLCamera2: TGLCamera;
    Panel1: TPanel;
    txtX: TGLSpaceText;
    txtY: TGLSpaceText;
    txtZ: TGLSpaceText;
    CollisionManager1: TGLCollisionManager;
    cbCollisionMode: TRadioGroup;
    Bar: TGLCube;
    Teapot1: TGLFreeForm;
    Teapot2: TGLFreeForm;
    Shape1: TShape;
    Cube2: TGLCube;
    Label1: TLabel;
    LATime: TLabel;
    Label2: TLabel;
    GLSphere1: TGLSphere;
    CubePoint1: TGLCube;
    GLSphere2: TGLSphere;
    Panel2: TPanel;
    StringGrid1: TStringGrid;
    Memo1: TMemo;
    GLSphereEllipsoid1: TGLSphere;
    GLSphereEllipsoid2: TGLSphere;
    CubePoint2: TGLCube;
    GLLightSource2: TGLLightSource;
    GLCube1: TGLCube;
    GLCamera1: TGLCamera;
    GLCamera3: TGLCamera;
    Splitter1: TSplitter;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CollisionManager1Collision(Sender: TObject; object1,
      object2: TGLBaseSceneObject);
    procedure cbCollisionModeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { D�clarations priv�es }
    mdx, mdy : Integer;
    CollisionDetected : Boolean;
  public
    CurrSO: TGLCustomSceneObject;
  end;

var
  Form1: TForm1;

const
  StringNames:array[0..Ord(cbmFaces)] of String = ('Point','Sphere','Ellipsoid','Cube','Faces');

implementation

{$R *.lfm}

uses Math, GLUtils;

procedure TForm1.FormCreate(Sender: TObject);
var
  i:integer;
begin
   SetGLSceneMediaDir();
   TeaPot1.LoadFromFile('teapot.3ds');
   TeaPot1.BuildOctree;

   TeaPot2.LoadFromFile('teapot.3ds');
   TeaPot2.BuildOctree;

//   rgObjectsClick(nil);

   //Fill StringGrid1 with current state of collisions
   for i:=0 to Ord(cbmFaces) do
   begin
     StringGrid1.Cells[0,i+1]:=StringNames[i];
     StringGrid1.Cells[i+1,0]:=StringNames[i];
   end;
   //point
   StringGrid1.Cells[1,1]:='complete';      //Point-Point
   StringGrid1.Cells[1,2]:='complete';      //Sphere-Point
   StringGrid1.Cells[1,3]:='complete';      //Ellipsoid-Point
   StringGrid1.Cells[1,4]:='complete';      //Cube-Point
   StringGrid1.Cells[1,5]:='Cube-Point';    //Faces-Point
   //sphere
   StringGrid1.Cells[2,1]:='complete';      //Point-Sphere
   StringGrid1.Cells[2,2]:='complete';      //Sphere-Sphere
   StringGrid1.Cells[2,3]:='complete';      //Ellipsoid-Sphere
   StringGrid1.Cells[2,4]:='complete';      //Cube-Sphere
   StringGrid1.Cells[2,5]:='Cube-Sphere';   //Faces-Sphere
   //ellipsoid
   StringGrid1.Cells[3,1]:='complete';      //Point-Ellipsoid
   StringGrid1.Cells[3,2]:='complete';      //Sphere-Ellipsoid
   StringGrid1.Cells[3,3]:='incorrect';     //Ellipsoid-Ellipsoid
   StringGrid1.Cells[3,4]:='Cube-Sphere';   //Cube-Ellipsoid
   StringGrid1.Cells[3,5]:='Cube-Ellipsoid';//Faces-Ellipsoid
   //cube
   StringGrid1.Cells[4,1]:='complete';      //Point-Cube
   StringGrid1.Cells[4,2]:='complete';      //Sphere-Cube
   StringGrid1.Cells[4,3]:='Sphere-Cube';   //Ellipsoid-Cube
   StringGrid1.Cells[4,4]:='complete';      //Cube-Cube
   StringGrid1.Cells[4,5]:='experimental';  //Faces-Cube
   //Faces
   StringGrid1.Cells[5,1]:='Point-Cube';    //Point-Faces
   StringGrid1.Cells[5,2]:='Sphere-Cube';   //Sphere-Faces
   StringGrid1.Cells[5,3]:='Ellipsoid-Cube';//Ellipsoid-Faces
   StringGrid1.Cells[5,4]:='experimental';  //Cube-Faces
   StringGrid1.Cells[5,5]:='complete';      //Faces-Faces


end;

procedure TForm1.FormShow(Sender: TObject);
begin
   //initialize
   CurrSO:= TeaPot1;
   cbCollisionModeClick(nil);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
const
   cColor : array [False..True] of TColor = (clLime, clRed);
var
   t : Int64;
begin
   Timer1.Enabled:=False;
   CollisionDetected:=False;

   t:=StartPrecisionTimer;

   Memo1.Lines.Clear;
   Memo1.Lines.BeginUpdate;
   CollisionManager1.CheckCollisions;
   Memo1.Lines.EndUpdate;

   LATime.Caption:=Format('%.1f ms', [StopPrecisionTimer(t)*1000]);

   Shape1.Brush.Color:=cColor[CollisionDetected];
   Timer1.Enabled:=True;
end;

procedure TForm1.CollisionManager1Collision(Sender: TObject; object1,
  object2: TGLBaseSceneObject);
begin
   if Sender=CollisionManager1 then
   begin
   CollisionDetected:=True;
   Memo1.Lines.Add(object1.Name+'('+StringNames[Ord(TGLBCollision(object1.Behaviours.GetByClass(TGLBCollision)).BoundingMode)]+')' +
            '  -  '+Object2.Name+'('+StringNames[Ord(TGLBCollision(object2.Behaviours.GetByClass(TGLBCollision)).BoundingMode)]+')');
   end
   else
   begin
   Memo1.Lines.Add(object1.Name+'('+StringNames[Ord(TGLBCollision(object1.Behaviours.GetByClass(TGLBCollision)).BoundingMode)]+')' +
            '  -  '+Object2.Name+'('+StringNames[Ord(TGLBCollision(object2.Behaviours.GetByClass(TGLBCollision)).BoundingMode)]+') ** BB collision **');

   end;
end;

procedure TForm1.cbCollisionModeClick(Sender: TObject);
begin
   TGLBCollision(TeaPot1.Behaviours[0]).BoundingMode:=TCollisionBoundingMode(cbCollisionMode.ItemIndex);
   TGLBCollision(TeaPot2.Behaviours[0]).BoundingMode:=TCollisionBoundingMode(cbCollisionMode.ItemIndex);
   TGLBCollision(Bar.Behaviours[0]).BoundingMode:=cbmCube;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   pick : TGLCustomSceneObject;
begin
   pick:=(GLSceneViewer1.Buffer.GetPickedObject(x, y) as TGLCustomSceneObject);
   if Assigned(Pick) then
      CurrSO:=pick;
   // store mouse coordinates when a button went down
   mdx:=x;
   mdy:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
   dx, dy: Integer;
   VX, VY: TVector;
   Camera: TGLCamera;
begin
   Camera := GLSceneViewer1.Camera;
   // calculate delta since last move or last mousedown
   dx := mdx - x;
   dy := mdy - y;
   mdx := x;
   mdy := y;
   if ssLeft in Shift then begin
      if ssShift in Shift then begin
        // left button with shift rotates the object
        // (rotation happens around camera's axis)
        Camera.RotateObject(CurrSO, dy, dx);
      end else begin
        // left button without shift changes camera angle
        // (we're moving around the parent and target dummycube)
        Camera.MoveAroundTarget(dy, dx)
      end;
   end else if Shift=[ssRight] then begin
      //Moving the objects
      //Description:
      //1. via VectorPerpendicular we create a vector that is 90� to camera view and points to Y (Up)
      //   this is Y-direction of moving
      //2. now using VectorCrossProduct we create the vector that is 90� to camera view and to the other
      //   vector (VY), this is X-direction of moving
      VY := VectorMake(VectorPerpendicular(YVector, VectorNormalize(GLCamera2.Position.AsAffineVector)));
      VX := VectorCrossProduct(VY, VectorNormalize(GLCamera2.Position.AsVector));
      NormalizeVector(VY);
      NormalizeVector(VX);
      CurrSO.Position.Translate(VectorCombine(VX, VY, -dx * 0.132 * Camera.DistanceToTarget / Camera.FocalLength, dy * 0.132 * Camera.DistanceToTarget / Camera.FocalLength));
   end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
   Camera: TGLCamera;
begin
   Camera := GLSceneViewer1.Camera;
   // Note that 1 wheel-step induces a WheelDelta of 120,
   // this code adjusts the distance to target with a 10% per wheel-step ratio
   Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

end.

