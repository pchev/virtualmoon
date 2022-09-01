unit umainform;



interface

uses

  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLCadencer, GLLCLViewer,
  GLNavigator, GLVectorGeometry, GLTexture, GLHeightData,
  GLTerrainRenderer, GLAsyncTimer, StdCtrls, ComCtrls, ExtCtrls, GLMaterial,
  GLCoordinates, GLCrossPlatform, GLBaseClasses, GLRenderContextInfo, GLContext, openGLAdapter,
  OpenGLTokens;

type
  TMainForm = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    GLCadencer: TGLCadencer;
    Camera: TGLCamera;
    GLNavigator: TGLNavigator;
    GLUserInterface: TGLUserInterface;
    Wind1: TGLDummyCube;
    DirectOpenGL: TGLDirectOpenGL;
    Terrain: TGLTerrainRenderer;
    GLBitmapHDS: TGLBitmapHDS;
    GLAsyncTimer: TGLAsyncTimer;
    MatLib: TGLMaterialLibrary;
    Wind2: TGLDummyCube;
    Wind3: TGLDummyCube;
    Panel: TPanel;
    Leafbar: TTrackBar;
    AnimateBox: TCheckBox;
    LeafLabel: TLabel;
    NodeCountLabel: TLabel;
    NodeCountBar: TTrackBar;
    NodeHLabel: TLabel;
    NodeHBar: TTrackBar;
    NodeAngleLabel: TLabel;
    NodeAngleBar: TTrackBar;
    Label1: TLabel;
    WindRangeBar: TTrackBar;
    WindRangeLabel: TLabel;
    WindPowerLabel: TLabel;
    WindPowerBar: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure DirectOpenGLRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure LeafbarChange(Sender: TObject);
    procedure GLSceneViewerClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TLine=record
     pos: TAffineVector;
     org: TAffineVector;
  end;

  TLines=record
     n: array of TLine;
     dir: TAffineVector;
  end;

var
  MainForm: TMainForm;
  org: TAffineVector;
  Lines: array of TLines;
  WindPower: single=2;
  WindRange: single=20;
  NodeCount: integer=3;
  NodeH: single=1;
  NodeAngle: single=1;
//  Size: integer=40;
  LeafNum: integer=1000;
  S: single=0.3;

implementation
uses GLKeyBoard;

  {$R *.lfm}


procedure MakeGrass;
var
   n, l: integer;
   opos, ndir: TAffineVector;
   h, xx, yy: single;
   new: boolean;
begin
   with MainForm do begin
   if Length(Lines)=leafnum then new:=False else new:=True;
   SetLength(Lines, LeafNum);
   for l:=0 to LeafNum-1 do begin
         SetLength(Lines[l].n, NodeCount);
         Lines[l].dir:=AffineVectorMake(1, 0, 0);
         Lines[l].dir:=VectorRotateAroundY(Lines[l].dir, l/LeafNum*pi*2);
         Lines[l].dir:=VectorNormalize(Lines[l].dir);
         ndir:=VectorRotateAroundY(Lines[l].dir, pi/2);
         if new then begin
            xx:=Random*40-20;
            yy:=Random*40-20;
         end else begin
            xx:=Lines[l].n[0].pos.V[0];
            yy:=Lines[l].n[0].pos.V[2];
         end;

         h:=Terrain.InterpolatedHeight(VectorMake(xx+26, 0, yy-26));
         opos:=AffineVectorMake(xx, h+0.1, yy);
         for n:=0 to Length(Lines[l].n)-1 do begin
            Lines[l].n[n].pos:=opos;
            Lines[l].n[n].org:=opos;
            Lines[l].n[n].org.V[0]:=opos.V[0]+(n/Length(Lines[l].n))*ndir.V[0]*NodeAngle;
            Lines[l].n[n].org.V[1]:=opos.V[1]+NodeH;
            Lines[l].n[n].org.V[2]:=opos.V[2]+(n/Length(Lines[l].n))*ndir.V[2]*NodeAngle;
            opos:=Lines[l].n[n].org;
         end;
      end;
   end;
end;

procedure UpdateGrass;
begin
   with MainForm do begin
      LeafNum:=LeafBar.Position;
      NodeCount:=NodeCountBar.Position;
      NodeH:=NodeHBar.Position/10;
      NodeAngle:=NodeAngleBar.Position/90*pi*NodeCount;
      WindRange:=WindRangeBar.Position/10;
      WindPower:=WindPowerBar.Position/10;
      LeafLabel.Caption:='Leaf count: '+IntToStr(LeafNum);
      NodeCountLabel.Caption:='Node count: '+IntToStr(NodeCount);
      NodeHLabel.Caption:='Node height: '+Format('%f', [NodeH]);
      NodeAngleLabel.Caption:='Node angle: '+IntToStr(NodeAngleBar.Position);
      WindRangeLabel.Caption:='Wind range: '+Format('%f', [WindRange]);
      WindPowerLabel.Caption:='Wind power: '+Format('%f', [WindPower]);
      MakeGrass;
   end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
   GLBitmapHDS.Picture.LoadFromFile('terrain.bmp');
   MatLib.AddTextureMaterial('grass', 'grass.bmp');
   Terrain.Material.Texture.Image.LoadFromFile('clover.jpg');
   UpdateGrass;
   Label1.Caption:='Click on viewer to'#13#10'toggle mouse look +'#13#10'(W,S,A,D)';
end;

procedure TMainForm.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   speed: double;
begin
   GLUserInterface.MouseUpdate;
   GLUserInterface.MouseLook;

   speed:=20*deltaTime;
   if IsKeyDown('W') then Camera.Move(speed);
   if IsKeyDown('S') then Camera.Move(-speed);
   if IsKeyDown('A') then Camera.Slide(-speed);
   if IsKeyDown('D') then Camera.Slide(speed);

   if AnimateBox.Checked then begin
      Wind1.Position.X:=sin(newTime*4)*10;
      Wind1.Position.Z:=cos(newTime)*10;
      Wind2.Position.X:=sin(newTime)*10;
      Wind2.Position.Z:=cos(newTime*4)*10;
      Wind3.Position.X:=sin(newTime*2)*20;
      Wind3.Position.Z:=cos(newTime*2)*20;

      Wind1.Position.Y:=2+Terrain.InterpolatedHeight(VectorMake(Wind1.Position.X+26, 0, Wind1.Position.Z-26));
      Wind2.Position.Y:=2+Terrain.InterpolatedHeight(VectorMake(Wind2.Position.X+26, 0, Wind2.Position.Z-26));
      Wind3.Position.Y:=2+Terrain.InterpolatedHeight(VectorMake(Wind3.Position.X+26, 0, Wind3.Position.Z-26));
   end;
end;

procedure TMainForm.DirectOpenGLRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
   i, n, ln, w: integer;
   v, pos, new, w1, w2, tmp: TAffineVector;
   d, ws1, ws2: single;
   Wind: TGLDummyCube;
begin
   GL.Disable(GL_CULL_FACE);
   gl.Color3f(1, 1, 1);
   gl.LineWidth(1);
   MatLib.ApplyMaterial('grass', rci);
   gl.Begin_(GL_TRIANGLES);
   for i:=0 to Length(Lines)-1 do begin
      ws2:=NodeCount*0.05+0.05;
      ws1:=ws2+0.05;
      for n:=0 to Length(Lines[i].n)-1 do begin
         tmp:=AffineVectorMake(0,0,0);
         for w:=1 to 3 do begin
            if w=1 then
            Wind:=Wind1 else
            if w=2 then
            Wind:=Wind2 else
            Wind:=Wind3;

            v:=VectorSubtract(Lines[i].n[n].org, Wind.Position.AsAffineVector);
            d:=VectorLength(v);
            if d>WindRange then new:=Lines[i].n[n].org else begin
               NormalizeVector(v);
               ScaleVector(v, (1-d/WindRange)*WindPower);
               new:=VectorAdd(Lines[i].n[n].org, v);
            end;
            tmp:=VectorAdd(tmp, new);
         end;
         ScaleVector(tmp, 1/3);

         new:=tmp;
         d:=VectorDistance(Lines[i].n[n].Pos, new);
         if d>NodeH then new.V[1]:=new.V[1]-(d-NodeH);
         if new.V[1]<Lines[i].n[n].pos.V[1] then
           new.V[1]:=Lines[i].n[n].pos.V[1];
         d:=VectorDistance(Lines[i].n[n].Pos, new);
         if d>NodeH then begin
            v:=VectorSubtract(new, Lines[i].n[n].pos);
            NormalizeVector(v);
            ScaleVector(v, NodeH);
            new:=VectorAdd(lines[i].n[n].pos, v);
         end;

         if n<Length(Lines[i].n)-1 then begin
            v:=VectorSubtract(new, Lines[i].n[n+1].pos);
            Lines[i].n[n+1].pos:=new;
            Lines[i].n[n+1].org:=VectorAdd(Lines[i].n[n+1].org, v);
         end;
         ln:=Length(Lines[i].n);

         pos:=Lines[i].n[n].pos;
         w1.V[0]:=ws1*Lines[i].dir.V[0];
         w1.V[1]:=ws1*Lines[i].dir.V[1];
         w1.V[2]:=ws1*Lines[i].dir.V[2];
         w2.V[0]:=ws2*Lines[i].dir.V[0];
         w2.V[1]:=ws2*Lines[i].dir.V[1];
         w2.V[2]:=ws2*Lines[i].dir.V[2];

         if n<ln-1 then begin
            gl.TexCoord2f(0, n/ln);
            gl.Vertex3f(pos.V[0]-w1.V[0], pos.V[1]-w1.V[1], pos.V[2]-w1.V[2]);
            gl.TexCoord2f(0, (n+1)/ln);
            gl.Vertex3f(new.V[0]-w2.V[0], new.V[1]-w2.V[1], new.V[2]-w2.V[2]);
            gl.TexCoord2f(1, n/ln);
            gl.Vertex3f(pos.V[0]+w1.V[0], pos.V[1]+w1.V[1], pos.V[2]+w1.V[2]);

            gl.TexCoord2f(1, n/ln);
            gl.Vertex3f(pos.V[0]+w1.V[0], pos.V[1]+w1.V[1], pos.V[2]+w1.V[2]);
            gl.TexCoord2f(0, (n+1)/ln);
            gl.Vertex3f(new.V[0]-w2.V[0], new.V[1]-w2.V[1], new.V[2]-w2.V[2]);
            gl.TexCoord2f(1, (n+1)/ln);
            gl.Vertex3f(new.V[0]+w2.V[0], new.V[1]+w2.V[1], new.V[2]+w2.V[2]);
         end else begin
            gl.TexCoord2f(0, n/ln);
            gl.Vertex3f(pos.V[0]-w1.V[0], pos.V[1]-w1.V[1], pos.V[2]-w1.V[2]);
            gl.TexCoord2f(0.5, n/ln);
            gl.Vertex3f(new.V[0], new.V[1], new.V[2]);
            gl.TexCoord2f(1, (n+1)/ln);
            gl.Vertex3f(pos.V[0]+w1.V[0], pos.V[1]+w1.V[1], pos.V[2]+w1.V[2]);
         end;
         ws1:=ws1-0.05;
         ws2:=ws2-0.05;
      end;
   end;
   gl.End_;
   MatLib.UnApplyMaterial(rci);
   gl.Enable(GL_CULL_FACE);
end;

procedure TMainForm.AsyncTimer1Timer(Sender: TObject);
begin
   Caption:=Format('Grass with '+'%d faces at %f FPS', [(2*(NodeCount-1)+1)*LeafNum, GLSceneViewer.FramesPerSecond]);
   GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TMainForm.LeafbarChange(Sender: TObject);
begin
   UpdateGrass;
end;

procedure TMainForm.GLSceneViewerClick(Sender: TObject);
begin
   GLUserInterface.MouseLookActive:=not GLUserInterface.MouseLookActive;
end;

end.
