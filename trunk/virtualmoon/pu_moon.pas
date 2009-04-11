unit pu_moon;
{$MODE Objfpc}
{$H+}

interface

uses u_util, u_constant, GLGraphics, GLContext,
  GLColor, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  GLObjects, GLMisc, ExtCtrls, GLTexture, GLCadencer,
  GLViewer, GLCrossPlatform, LResources, GLScene, GLMultiMaterialShader,
  StdCtrls, GLBumpShader, GLHUDObjects, GLWindowsFont, GLGeomObjects, GLMirror;

const
   MaxLabel=500;

type

  { Tf_moon }

  TMoonClickEvent = procedure(Sender: TObject; Button: TMouseButton;
                     Shift: TShiftState; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single) of object;
  TGetSingleEvent = procedure(Sender: TObject; value: single) of object;
  TGetStringEvent = procedure(Sender: TObject; value: string) of object;

  Tf_moon = class(TForm)
    GLAnnulus1: TGLAnnulus;
    GLArrowLine1: TGLArrowLine;
    GLBumpShader1: TGLBumpShader;
     GLCadencer1: TGLCadencer;
     GLDummyCubeMarks: TGLDummyCube;
     GLDummyCubeLabels: TGLDummyCube;
     GLHUDSpriteDistance: TGLHUDSprite;
     GLHUDSpriteMark: TGLHUDSprite;
     GLHUDTextMark: TGLHUDText;
     GLHUDTextMarkShadow: TGLHUDText;
     GLMaterialLibrary1: TGLMaterialLibrary;
     BumpMaterialLibrary: TGLMaterialLibrary;
     GLMirror1: TGLMirror;
     GLMultiMaterialLibrary: TGLMaterialLibrary;
     GLMultiMaterialShader: TGLMultiMaterialShader;
     GLSceneViewer1: TGLSceneViewer;
     GLScene1: TGLScene;
     GLCamera1: TGLCamera;
     DummyCube1: TGLDummyCube;
     GLLightSource1: TGLLightSource;
     GLSphereMoon: TGLSphere;
     GLBitmapFont1: TGLWindowsBitmapFont;
     Moon: TPanel;
     Timer1: TTimer;
     Timer2: TTimer;
     procedure FormCreate(Sender: TObject);
     procedure FormDestroy(Sender: TObject);
     procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
       newTime: Double);
     procedure GLSceneViewer1MouseDown(Sender: TObject;
       Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
     procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
       X, Y: Integer);
     procedure GLSceneViewer1MouseWheelDown(Sender: TObject;
       Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
     procedure GLSceneViewer1MouseWheelUp(Sender: TObject;
       Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
     procedure Timer1Timer(Sender: TObject);
     procedure Timer2Timer(Sender: TObject);
  private
    { Déclarations privées }
    marked: boolean;
    marktext: string;
    markl,markb: single;
    MaxTextureSize: integer;
    MaxZoom: single;
    mx,my, zone, maxzone,curlabel : integer;
    maps2, newmaps: array[0..8] of integer;
    pmaps2 : array[0..2] of integer;
    cap2,newcap: integer;
    lock_Zoom : boolean;
    blankbmp: Tbitmap;
    FOnMoonClick: TMoonClickEvent;
    FOnGetMsg: TGetStringEvent;
    FOnGetLabel: TNotifyEvent;
    FTexturePath: String;
    FTexture: String;
    FOverlayPath: String;
    FOverlay: String;
    FBumpPath: String;
    FBumpmap: Boolean;
    FBumpOk : boolean;
    FShowPhase: Boolean;
    FMirror: Boolean;
    FVisibleSideLock: Boolean;
    FRotation: single;
    FPhase: single;
    FSunIncl: single;
    FLibrLon: single;
    FLibrLat: single;
    FOrientation: single;
    Fzoom : single;
    Flabelcolor: TColor;
    procedure SetTexture(fn:string);
    procedure SetOverlay(fn:string);
    procedure SetBumpPath(fn:string);
    procedure SetBumpmap(value:boolean);
    procedure SetShowPhase(value:boolean);
    procedure SetMirror(value:boolean);
    procedure SetOrientation(value: single);
    procedure SetVisibleSideLock(value:boolean);
    procedure SetRotation(value:single);
    procedure SetPhase(value:single);
    procedure SetSunIncl(value:single);
    procedure SetLibrLon(value:single);
    procedure SetLibrLat(value:single);
    procedure SetLabelFont(f:Tfont);
    procedure DisableSlice2;
    function  GetLabelFont : Tfont;
    procedure SetLabelColor(c:TColor);
    procedure CreateMaterial(level:integer);
    Procedure LoadSlice(level:integer);
    Procedure ClearSlice(level:integer);
    procedure ClearOverlay;
    Procedure SetZoomLevel(zoom:single);
    function  Screen2Moon(x,y:integer; var lon,lat: single): boolean;
    function  Moon2Screen(lon,lat: single; var x,y:integer): boolean;
    function  Moon2ScreenNoClip(lon,lat: single; var x,y:integer): boolean;
    function  Moon2World(lon,lat: single; var x,y:single): boolean;
    procedure InitLabel;
    procedure ClearLabel;
    procedure OrientLightSource;
    procedure OrientMoon;
    procedure ResetMoon;

  public
    { Declarations publiques }
    procedure Init;
    procedure SetMark(lon,lat:single; txt:string);
    procedure CenterAt(lon,lat:single);
    procedure CenterMark;
    function AddLabel(lon,lat:single; txt:string):boolean;
    procedure RefreshLabel;
    property TexturePath : String read FtexturePath write FTexturePath;
    property Texture : String read Ftexture write SetTexture;
    property OverlayPath : String read FOverlayPath write FOverlayPath;
    property Overlay : String read FOverlay write SetOverlay;
    property BumpPath : String read FBumpPath write SetBumpPath;
    property Bumpmap : Boolean read FBumpmap write SetBumpmap;
    property CanBump : Boolean read FBumpOk;
    property Rotation : single read FRotation write SetRotation;
    property Phase : single read FPhase write SetPhase;
    property SunIncl : single read FSunIncl write SetSunIncl;
    property LibrLon : single read FLibrLon write SetLibrLon;
    property LibrLat : single read FLibrLat write SetLibrLat;
    property Zoom : single read Fzoom write SetZoomLevel;
    property ZoomMax: single read MaxZoom;
    property Mirror : Boolean read FMirror write SetMirror;
    property Orientation:single read FOrientation write SetOrientation;
    property ShowPhase : Boolean read FShowPhase write SetShowPhase;
    property VisibleSideLock : Boolean read FVisibleSideLock write SetVisibleSideLock;
    property LabelFont : TFont read GetLabelFont write SetLabelFont;
    property LabelColor : TColor read FLabelColor write SetLabelColor;
    property onMoonClick : TMoonClickEvent read FOnMoonClick write FOnMoonClick;
    property onGetMsg : TGetStringEvent read FOnGetMsg write FOnGetMsg;
    property onGetLabel : TNotifyEvent read FOnGetLabel write FOnGetLabel;
  end;

var
  f_moon: Tf_moon;

const
  ZoomByZone: array[1..4] of integer=(3,8,14,20);

implementation

uses VectorGeometry, OpenGL1x;

{ Tf_moon }

procedure Tf_moon.CreateMaterial(level:integer);
var i : integer;
begin
with GLMaterialLibrary1 do begin
  case level of
  1: begin
    for i:=0 to 7 do begin
       with AddTextureMaterial('L1_'+inttostr(i),blankbmp) do begin
              Material.BlendingMode:=bmTransparency;
              Material.Texture.Compression:=tcStandard;
              Material.Texture.ImageAlpha:=tiaBottomRightPointColorTransparent;
              Material.Texture.TextureMode:=tmModulate;
              Material.Texture.TextureWrap:=twNone;
              Material.Texture.MagFilter:=maNearest;
              Material.Texture.MinFilter:=miNearest;
       end;
    end;
  end;
  2: begin
    if LibMaterialByName('L1_0')=nil then CreateMaterial(1);
    for i:=0 to 2 do begin
       with AddTextureMaterial('P2_'+inttostr(i),blankbmp) do begin
              Material.BlendingMode:=bmTransparency;
              Material.Texture.Compression:=tcStandard;
              Material.Texture.ImageAlpha:=tiaBottomRightPointColorTransparent;
              Material.Texture.TextureWrap:=twNone;
              Material.Texture.TextureMode:=tmModulate;
              Material.Texture.MagFilter:=maNearest;
              Material.Texture.MinFilter:=miNearest;
       end;
    end;
    for i:=0 to 8 do begin
       with AddTextureMaterial('L2_'+inttostr(i),blankbmp) do begin
              Material.BlendingMode:=bmTransparency;
              Material.Texture.Compression:=tcStandard;
              Material.Texture.ImageAlpha:=tiaBottomRightPointColorTransparent;
              Material.Texture.TextureWrap:=twNone;
              Material.Texture.TextureMode:=tmModulate;
              Material.Texture.MagFilter:=maNearest;
              Material.Texture.MinFilter:=miNearest;
       end;
    end;
  end;
  99: begin
       if LibMaterialByName('L1_0')=nil then CreateMaterial(1);
       if LibMaterialByName('L2_0')=nil then CreateMaterial(2);
       with AddTextureMaterial('O1',blankbmp) do begin
        Material.BlendingMode:=bmTransparency;
        Material.Texture.Compression:=tcStandard;
        Material.Texture.ImageAlpha:=tiaBottomRightPointColorTransparent;
        Material.Texture.TextureWrap:=twNone;
        Material.Texture.TextureMode:=tmModulate;
       end;
    end;
end;
end;
end;

Procedure Tf_moon.LoadSlice(level:integer);
var toffset,tscale : single;
    tpath,nn: string;
    i,j,k,row,col,maxcol,maxrow : integer;
    lc,bc:single;
    ok: boolean;
    jp: TJPEGImage;

procedure LoadSlice2;
var i,j: integer;
begin
     toffset:=12/1024;
     tscale:=1000/1024;
     tpath:=slash(FTexturePath)+slash(Ftexture)+slash('L'+inttostr(level));
     if GLMaterialLibrary1.LibMaterialByName('L2_0')=nil then CreateMaterial(2);
     // search slices
     Screen2Moon(GLSceneViewer1.Width div 2, GLSceneViewer1.Height div 2, lc, bc);
     lc:=lc+pi;
     bc:=pi/2-bc;
     col:=trunc(maxcol*lc/(2*pi));
     row:=trunc(maxrow*bc/pi);
     pmaps2[0]:=-1;
     pmaps2[1]:=-1;
     pmaps2[2]:=-1;
     newcap:=-1;
     newmaps[0]:=maxcol*row+col;  // center frame
     if col<(maxcol-1) then newmaps[1]:=maxcol*row+col+1  // right
                       else newmaps[1]:=maxcol*row;
     if col>0 then newmaps[2]:=maxcol*row+col-1           // left
              else newmaps[2]:=maxcol*row+maxcol-1;
     if row<(maxrow-1) then begin
        newmaps[3]:=newmaps[0]+maxcol; // bottom
        newmaps[4]:=newmaps[1]+maxcol; // bottom right
        newmaps[5]:=newmaps[2]+maxcol; // bottom left
     end else begin
        newmaps[3]:=-1;
        newmaps[4]:=-1;
        newmaps[5]:=-1;
        pmaps2[0]:=20000;    // South cap
        pmaps2[1]:=20001;
        pmaps2[2]:=20002;
        newcap:=2;
     end;
     if row>0 then begin
        newmaps[6]:=newmaps[0]-maxcol; // top
        newmaps[7]:=newmaps[1]-maxcol; // top right
        newmaps[8]:=newmaps[2]-maxcol; // top left
    end else begin
        newmaps[6]:=-1;
        newmaps[7]:=-1;
        newmaps[8]:=-1;
        pmaps2[0]:=10000;  // North cap
        pmaps2[1]:=10001;
        pmaps2[2]:=10002;
        newcap:=1;
     end;
     // remove unused slices
     for i:=0 to 8 do begin
       if maps2[i]<0 then continue;
       ok:=false;
       for j:=0 to 8 do begin
          if newmaps[j]<0 then continue;
          if (newmaps[j]=maps2[i]) then begin
             ok:=true;
             break;
          end;
       end;
       if not ok then begin
          GLMaterialLibrary1.LibMaterialByName('L2_'+inttostr(i)).Material.Texture.Image.Assign(blankbmp);
          maps2[i]:=-1;
       end;
     end;
     // add new slices
     for i:=0 to 8 do begin
       if newmaps[i]<0 then continue;
       ok:=false;
       for j:=0 to 8 do begin
          if maps2[j]<0 then continue;
          if (newmaps[i]=maps2[j]) then begin
             ok:=true;
             break;
          end;
       end;
       if not ok then begin
         for j:=0 to 8 do
             if maps2[j]<0 then begin
               k:=j;
               break;
             end;
         maps2[k]:=newmaps[i];
         row:=maps2[k] div maxcol;
         col:=maps2[k] mod maxcol;
         with GLMaterialLibrary1 do begin
            jp.LoadFromFile(tpath+inttostr(maps2[k])+'.jpg');
            with jp.Canvas do begin
              brush.Color:=clWhite;   // replace white border because of jpeg compression
              pen.Color:=clWhite;
              rectangle(0,0,jp.Width,12);
              rectangle(0,0,12,jp.Height);
              rectangle(jp.width-12,0,jp.width,jp.Height);
              rectangle(0,jp.Height-12,jp.width,jp.Height);
            end;
            with LibMaterialByName('L2_'+inttostr(k)) do begin
              Material.Texture.ImageBrightness:=1.1;
              Material.Texture.Image.Assign(jp);
              TextureOffset.SetVector(-col*tscale+toffset,(row-maxrow+1)*tscale+toffset,0);
              TextureScale.SetVector(maxcol*tscale,maxrow*tscale,0);
            end;
         end;
         GLSceneViewer1.Refresh;
         Application.ProcessMessages;
        end;
      end;
      // Polar caps
      if (newcap<>cap2) then
      if (pmaps2[0]>0) then begin
        cap2:=newcap;
        for i:=0 to 2 do begin
         if pmaps2[i]>=20000 then begin
           row:=maxrow-1;
           col:=pmaps2[i] mod 20000;
           maxcol:=3;
          end
          else if pmaps2[i]>=10000 then begin
           row:=0;
           col:=pmaps2[i] mod 10000;
           maxcol:=3;
          end;
         with GLMaterialLibrary1 do begin
            jp.LoadFromFile(tpath+inttostr(pmaps2[i])+'.jpg');
            with jp.Canvas do begin
              brush.Color:=clWhite;
              pen.Color:=clWhite;
              rectangle(0,0,jp.Width,12);
              rectangle(0,0,12,jp.Height);
              rectangle(jp.width-12,0,jp.width,jp.Height);
              rectangle(0,jp.Height-12,jp.width,jp.Height);
            end;
            with LibMaterialByName('P2_'+inttostr(i)) do begin
              Material.Texture.ImageBrightness:=1.1;
              Material.Texture.Image.Assign(jp);
              TextureOffset.SetVector(-col*tscale+toffset,(row-maxrow+1)*tscale+toffset,0);
              TextureScale.SetVector(maxcol*tscale,maxrow*tscale,0);
            end;
         end;
         GLSceneViewer1.Refresh;
         Application.ProcessMessages;
        end;
     end else begin
        cap2:=-1;
        for i:=0 to 2 do begin
         GLMaterialLibrary1.LibMaterialByName('P2_'+inttostr(i)).Material.Texture.Image.Assign(blankbmp);
         pmaps2[i]:=-1;
        end;
     end;
end;

begin
try
jp:=TJPEGImage.Create;
case level of
 1 : begin
     { level 1 :
     4000x2000 full picture.
     8 slices 1024x1024 with 1000x1000 active and 12px white border.
     Slices start on the middle of far side, longitude -180° ,
     slices 1-4 for North, slices 5-8 for South.
     On level 1 we always load the full picture.
     }
     toffset:=12/1024;
     tscale:=1000/1024;
     tpath:=slash(FTexturePath)+slash(Ftexture)+slash('L'+inttostr(level));
     if GLMaterialLibrary1.LibMaterialByName('L1_0')=nil then CreateMaterial(1);
     for i:=0 to 7 do begin
       row:=i div 4;
       col:=i mod 4;
       nn:=inttostr(i);
       with GLMaterialLibrary1 do begin
          jp.LoadFromFile(tpath+nn+'.jpg');
          with jp.Canvas do begin
            brush.Color:=clWhite;    // replace white border because of jpeg compression
            pen.Color:=clWhite;
            rectangle(0,0,jp.Width,12);
            rectangle(0,0,12,jp.Height);
            rectangle(jp.width-12,0,jp.width,jp.Height);
            rectangle(0,jp.Height-12,jp.width,jp.Height);
          end;
          with LibMaterialByName('L1_'+nn) do begin
            Material.Texture.ImageBrightness:=1.1;
            Material.Texture.Image.Assign(jp);
            TextureOffset.SetVector(-col*tscale+toffset,(row-1)*tscale+toffset,0);
            TextureScale.SetVector(4*tscale,2*tscale,0);
          end;
       end;
      end;
     end; // 1
 2 : begin
     { level 2 :
     10000x5000 full picture.
     50 slices 1024x1024 with 1000x1000 active and 12px white border.
     Slices start on the middle of far side, longitude -180° ,
     5 rows of 10 slices from North to South.
     + 1 x 3 polars cap
     }
     maxcol:=10;
     maxrow:=5;
     LoadSlice2;
     end;  // 2
 3 : begin
     { level 3 :
     20000x10000 full picture.
     200 slices 1024x1024 with 1000x1000 active and 12px white border.
     Slices start on the middle of far side, longitude -180° ,
     10 rows of 20 slices from North to South.
     + 1 x 3 polars cap
     }
     maxcol:=20;
     maxrow:=10;
     LoadSlice2;
     end; // 3
 4 : begin
     { level 4 :
     40000x20000 full picture.
     800 slices 1024x1024 with 1000x1000 active and 12px white border.
     Slices start on the middle of far side, longitude -180° ,
     20 rows of 40 slices from North to South.
     + 1 x 3 polars cap
     }
     maxcol:=40;
     maxrow:=20;
     LoadSlice2;
     end; // 4
end; //case level
finally
jp.Free;
end;
end;

procedure Tf_moon.ClearSlice(level:integer);
var j: integer;
begin
if level>1 then begin
    if GLMaterialLibrary1.LibMaterialByName('L2_0')<>nil then begin
    for j:=0 to 8 do begin
       GLMaterialLibrary1.LibMaterialByName('L2_'+inttostr(j)).Material.Texture.Image.Assign(blankbmp);
       maps2[j]:=-1;
    end;
    for j:=0 to 2 do begin
       GLMaterialLibrary1.LibMaterialByName('P2_'+inttostr(j)).Material.Texture.Image.Assign(blankbmp);
       pmaps2[j]:=-1;
    end;
    end;
    cap2:=-1;
end;
GLSceneViewer1.Refresh;
end;

procedure Tf_moon.DisableSlice2;
var j: integer;
begin
    for j:=0 to 8 do begin
       maps2[j]:=-1;
    end;
    for j:=0 to 2 do begin
       pmaps2[j]:=-1;
    end;
    cap2:=-1;
end;

procedure Tf_moon.ClearOverlay;
begin
if GLMaterialLibrary1.LibMaterialByName('O1')<>nil then begin
  GLMaterialLibrary1.LibMaterialByName('O1').Material.Texture.Image.Assign(blankbmp);
  GLMaterialLibrary1.LibMaterialByName('O1').Material.Texture.ImageAlpha:=tiaBottomRightPointColorTransparent;
  GLSceneViewer1.Refresh;
end;
end;

procedure Tf_moon.SetOverlay(fn:string);
begin
if fn='' then begin
  ClearOverlay;
  FOverlay:='';
end else begin
  if not FileExists(slash(FOverlayPath)+fn) then raise Exception.Create('Overlay not found '+slash(FOverlayPath)+fn);
  FOverlay:=fn;
  if GLMaterialLibrary1.LibMaterialByName('O1')=nil then CreateMaterial(99);
  with GLMaterialLibrary1 do begin
      with LibMaterialByName('O1') do begin
        Material.Texture.ImageBrightness:=1.1;
        Material.Texture.ImageAlpha:=tiaAlphaFromIntensity;
        Material.Texture.Image.LoadFromFile(slash(FOverlayPath)+fn);
      end;
   end;
end;
GLSceneViewer1.Refresh;
end;

procedure Tf_moon.SetBumpmap(value: boolean);
var i: integer;
begin
if FBumpOk and (value<>FBumpmap) then begin
  FBumpmap:=value;
  if FBumpmap then begin
    i:=MaxTextureSize div 1024;
    if i>=8 then i:=4  // 8 eat 800MB of memory!
    else if i>=4 then i:=4
    else if i>=2 then i:=2
    else i:=1;
    BumpMaterialLibrary.Materials[0].Material.Texture.Image.LoadFromFile(slash(FBumpPath)+'normal'+inttostr(i)+'k.jpg');
    BumpMaterialLibrary.Materials[1].Material.Texture.Image.LoadFromFile(slash(FBumpPath)+'map'+inttostr(i)+'k.jpg');
    GLSphereMoon.Material.MaterialLibrary:=BumpMaterialLibrary;
    GLSphereMoon.Material.LibMaterialName:='Bump';
    if GLBumpShader1.BumpMethod=bmBasicARBFP then
       GLLightSource1.ConstAttenuation:=0.6
    else
       GLLightSource1.ConstAttenuation:=0.3;
    ClearSlice(2);
    ClearSlice(3);
    MaxZoom:=6;
    if GLCamera1.SceneScale>MaxZoom then SetZoomLevel(MaxZoom);
  end else begin
    LoadSlice(zone);
    GLSphereMoon.Material.MaterialLibrary:=GLMultiMaterialLibrary;
    GLSphereMoon.Material.LibMaterialName:='MultiMaterial';
    BumpMaterialLibrary.Materials[0].Material.Texture.Image.Assign(blankbmp);
    BumpMaterialLibrary.Materials[1].Material.Texture.Image.Assign(blankbmp);
    GLLightSource1.ConstAttenuation:=0.5;
    maxzoom:=ZoomByZone[maxzone];
  end;
  GLSceneViewer1.Refresh;
end;
end;

procedure Tf_moon.SetShowPhase(value:boolean);
begin
if value<>FShowPhase then begin
  FShowPhase:=value;
  if FShowPhase then begin
     GLLightSource1.Position.x:=0;
     GLLightSource1.Position.y:=0;
     GLLightSource1.Position.z:=-100;
     GLLightSource1.SpotDirection.x:=0;
     GLLightSource1.SpotDirection.y:=0;
     GLLightSource1.SpotDirection.z:=1;
     SetPhase(FPhase);
  end else begin
     GLLightSource1.Position:=GLCamera1.Position;
     GLLightSource1.SpotDirection.SetVector(-GLLightSource1.Position.X,-GLLightSource1.Position.Y,-GLLightSource1.Position.Z);
  end;
  GLSceneViewer1.Refresh;
end;
end;

procedure Tf_moon.SetVisibleSideLock(value:boolean);
begin
if value<>FVisibleSideLock then begin
   FVisibleSideLock:=value;
   if FVisibleSideLock then begin
      GLSphereMoon.TurnAngle:=0;
      GLCamera1.TargetObject:=nil;
      GLCamera1.Position.SetVector(0,0,-100);
      GLCamera1.Direction.SetVector(0,0,1);
      SetZoomLevel(1);
   end else begin
      GLCamera1.TargetObject:=GLSphereMoon;
      SetZoomLevel(1);
   end;
   if not FShowPhase then begin
      GLLightSource1.Position:=GLCamera1.Position;
      GLLightSource1.SpotDirection.SetVector(-GLLightSource1.Position.X,-GLLightSource1.Position.Y,-GLLightSource1.Position.Z);
   end;
   GLSceneViewer1.Refresh;
end;
end;

function Tf_moon.Moon2Screen(lon,lat: single; var x,y:integer): boolean;
var qr,l,b,xx,yy,zz: single;
    v : TAffineVector;
begin
if FMirror then begin
  GLCamera1.Direction.SetVector(0,0,1);
  GLSceneViewer1.Buffer.ContextOptions:=[roNoSwapBuffers];
  GLSceneViewer1.Update;
end;
  qr:=0.5;
  l:=-lon-pi/2;
  b:=lat;
  xx:=qr*cos(b)*cos(l);
  yy:=qr*cos(b)*sin(l);
  zz:=qr*sin(b);
  v[0]:=xx;
  v[1]:=zz;
  v[2]:=yy;
  v:=GLSphereMoon.LocalToAbsolute(v);
  v:=GLSceneViewer1.Buffer.WorldToScreen(v);
  if (v[0]>=0)and(v[0]<=GLSceneViewer1.Width)and(v[1]>=0)and(v[1]<=GLSceneViewer1.Height) then begin
    x:=round(v[0]);
    y:=GLSceneViewer1.Height-round(v[1]);
    if FMirror then x:=GLSceneViewer1.Width-x;
    Screen2Moon(x,y,xx,yy);
    result:=(abs(lon-xx)<0.1)and(abs(lat-yy)<0.1);
  end else result:=false;
if FMirror then begin
  GLCamera1.Direction.SetVector(0,0,-1);
  GLSceneViewer1.Buffer.ContextOptions:=[roDoubleBuffer,roRenderToWindow];
  GLSceneViewer1.Update;
end;
end;

function Tf_moon.Screen2Moon(x,y:integer; var lon,lat: single): boolean;
var
  pick : TGLCustomSceneObject;
  v : TAffineVector;
  qr,xx,yy,zz: single;
begin
if FMirror then begin
  GLCamera1.Direction.SetVector(0,0,1);
  GLSceneViewer1.Buffer.ContextOptions:=[roNoSwapBuffers];
  GLSceneViewer1.Update;
end;
 if FMirror then x:=GLSceneViewer1.Width-x;
 pick:=(GLSceneViewer1.Buffer.GetPickedObject(x, y) as TGLCustomSceneObject);
 if assigned(pick) then begin
   v:=GLSceneViewer1.Buffer.PixelRayToWorld(x, y);
   v:=pick.AbsoluteToLocal(v);
   xx:=v[0];
   yy:=v[2];
   zz:=v[1];
   lon:=-arctan2(yy,xx)-pi/2;
   if (lon<-pi) then lon:=lon+2*pi;
   qr:=sqrt(xx*xx+yy*yy);
   if qr<>0 then begin
      lat:=arctan(zz/qr);
      result:=true;
   end else begin
      lat:=0;
      result:=false;
   end;
 end
 else result:=false;
if FMirror then begin
  GLCamera1.Direction.SetVector(0,0,-1);
  GLSceneViewer1.Buffer.ContextOptions:=[roDoubleBuffer,roRenderToWindow];
  GLSceneViewer1.Update;
end;
end;

function Tf_moon.Moon2ScreenNoClip(lon,lat: single; var x,y:integer): boolean;
var qr,l,b,xx,yy,zz: single;
    v : TAffineVector;
begin
if FMirror then begin
  GLCamera1.Direction.SetVector(0,0,1);
  GLSceneViewer1.Buffer.ContextOptions:=[roNoSwapBuffers];
  GLSceneViewer1.Update;
end;
  qr:=0.5;
  l:=-lon-pi/2;
  b:=lat;
  xx:=qr*cos(b)*cos(l);
  yy:=qr*cos(b)*sin(l);
  zz:=qr*sin(b);
  v[0]:=xx;
  v[1]:=zz;
  v[2]:=yy;
  v:=GLSphereMoon.LocalToAbsolute(v);
  v:=GLSceneViewer1.Buffer.WorldToScreen(v);
  x:=round(v[0]);
  y:=GLSceneViewer1.Height-round(v[1]);
  result:=true;
if FMirror then begin
  GLCamera1.Direction.SetVector(0,0,-1);
  GLSceneViewer1.Buffer.ContextOptions:=[roDoubleBuffer,roRenderToWindow];
  GLSceneViewer1.Update;
end;
end;

function Tf_moon.Moon2World(lon,lat: single; var x,y:single): boolean;
var qr,l,b,xx,yy,zz: single;
    v : TAffineVector;
begin
if FMirror then begin
  GLCamera1.Direction.SetVector(0,0,1);
  GLSceneViewer1.Buffer.ContextOptions:=[roNoSwapBuffers];
  GLSceneViewer1.Update;
end;
  qr:=0.5;
  l:=-lon-pi/2;
  b:=lat;
  xx:=qr*cos(b)*cos(l);
  yy:=qr*cos(b)*sin(l);
  zz:=qr*sin(b);
  v[0]:=xx;
  v[1]:=zz;
  v[2]:=yy;
  v:=GLSphereMoon.LocalToAbsolute(v);
  x:=v[0];
  y:=v[1];
  result:=true;
if FMirror then begin
  GLCamera1.Direction.SetVector(0,0,-1);
  GLSceneViewer1.Buffer.ContextOptions:=[roDoubleBuffer,roRenderToWindow];
  GLSceneViewer1.Update;
end;
end;

Procedure Tf_moon.SetZoomLevel(zoom:single);
var newzone: integer;
begin
if lock_Zoom then exit;
try
  lock_Zoom:=true;
  if zoom<1 then zoom:=1;
  if zoom>MaxZoom then zoom:=MaxZoom;
  newzone:=1;
  if zoom>ZoomByZone[1] then begin
     if maxzone>=2 then newzone:=2
                   else zoom:=ZoomByZone[1];
     if zoom>ZoomByZone[2] then begin
       if maxzone>=3 then newzone:=3
                     else zoom:=ZoomByZone[2];
       if zoom>ZoomByZone[3] then begin
         if maxzone>=4 then newzone:=4
                       else zoom:=ZoomByZone[3];
       end;
     end;
  end;
  Fzoom:=zoom;
  GLCamera1.SceneScale:=Fzoom;
  if (not FBumpmap)and(newzone<>zone) then begin
    if zone=1 then
       LoadSlice(newzone)
    else if newzone=1 then
       ClearSlice(zone)
    else begin
       DisableSlice2;
       LoadSlice(newzone);
    end;
  end;
  zone:=newzone;
  if assigned(FOnGetMsg) then FOnGetMsg(self,'Zoom:'+formatfloat('0.0',zoom)+'  Level:'+inttostr(zone));
  RefreshLabel;
finally
  lock_Zoom:=false;
end;
end;

procedure Tf_moon.RefreshLabel;
begin
  ClearLabel;
  timer1.Enabled:=false;
  timer1.Enabled:=true;
end;

procedure Tf_moon.SetTexture(fn:string);
var i: integer;
begin
 if not DirectoryExists(slash(FTexturePath)+slash(fn)+'L1') then raise Exception.Create('Missing L1 slices for '+slash(FTexturePath)+fn);
 Ftexture:=fn;
 maxzone:=1;
 if DirectoryExists(slash(FTexturePath)+slash(Ftexture)+'L2') then begin
    maxzone:=2;
    if DirectoryExists(slash(FTexturePath)+slash(Ftexture)+'L3') then begin
       maxzone:=3;
       if DirectoryExists(slash(FTexturePath)+slash(Ftexture)+'L4') then begin
          maxzone:=4;
       end;
    end;
 end;
 maxzoom:=ZoomByZone[maxzone];
 for i:=1 to zone do ClearSlice(i);
 LoadSlice(1);
 if zone<>1 then LoadSlice(zone);
 GLSceneViewer1.Refresh;
end;

procedure Tf_moon.SetBumpPath(fn:string);
var i: integer;
begin
if not FileExists(slash(fn)+'normal2k.jpg') then raise Exception.Create('No bumpmap in '+fn);
FBumpPath:=fn;
end;

procedure Tf_moon.SetRotation(value:single);
begin
 FRotation:=value;
 GLCadencer1.Enabled:=(FRotation<>0);
 timer2.Enabled:= GLCadencer1.Enabled;
end;

procedure Tf_moon.FormCreate(Sender: TObject);
var i: integer;
begin
 if Owner is TWinControl then Moon.Parent:=TWinControl(Owner);
 blankbmp:=Tbitmap.Create;
 blankbmp.Width:=4;
 blankbmp.Height:=4;
 blankbmp.Canvas.brush.Color:=clWhite;
 blankbmp.Canvas.pen.Color:=clWhite;
 blankbmp.Canvas.rectangle(0,0,4,4);
 ldeg:='d';
 lmin:='m';
 lsec:='s';
 lock_Zoom:=false;
 for i:=0 to 8 do begin
    maps2[i]:=-1;
 end;
 for i:=0 to 2 do begin
    pmaps2[i]:=-1;
 end;
 cap2:=-1;
 zone:=1;
 FTexture:='';
 FOverlay:='';
 FRotation:=0;
 FBumpOk:=false;
 MaxZoom:=3;
 MaxTextureSize:=1024;
 Flabelcolor:=clWhite;
end;

procedure Tf_moon.Init;
begin
try
  if  GL_ARB_multitexture
  and GL_ARB_vertex_program
  and GL_ARB_texture_env_dot3 then begin
    GLBumpShader1.BumpMethod:=bmDot3TexCombiner;
    FBumpOk:=true;
  end;
  if  GL_ARB_multitexture
  and GL_ARB_vertex_program
  and GL_ARB_fragment_program then begin
    GLBumpShader1.BumpMethod:=bmBasicARBFP;
    FBumpOk:=true;
  end;
if GLsceneviewer1.Buffer.Acceleration=chaSoftware then begin
   raise exception.Create('This program only run with a graphic card that support OpenGL hardware acceleration.');
   halt;
end;
MaxTextureSize:=Glsceneviewer1.Buffer.LimitOf[limTextureSize];
if MaxTextureSize<1024 then begin
   raise exception.Create('Graphic card not supported! Texture max size:'+inttostr(MaxTextureSize)+'. Must be at least 1024.');
   halt;
end;
InitLabel;
except
  raise exception.Create('Could not initialize OpenGL.');
  halt;
end;
end;

procedure Tf_moon.FormDestroy(Sender: TObject);
begin
 if GLDummyCubeLabels<>nil then GLDummyCubeLabels.DeleteChildren;
 blankbmp.Free;
end;

procedure Tf_moon.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var lat,lon: single;
    OnMoon: boolean;
begin
  mx:=x;
  my:=y;
  if FMirror then mx:=GLSceneViewer1.Width-mx;
  OnMoon:=false;
  if ssLeft in shift then begin
     OnMoon:=Screen2Moon(x,y,lon,lat);
  end;
  if Assigned(onMoonClick) then onMoonClick(Self,Button,Shift,X,Y,OnMoon,lon,lat);
end;

procedure Tf_moon.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var movespeed: single;
begin
  if FMirror then x:=GLSceneViewer1.Width-x;
  if (ssMiddle in shift) or
     ((ssLeft in shift)and(ssCtrl in shift))
   then begin
    if FVisibleSideLock then begin
      movespeed:=0.002/GLCamera1.SceneScale;
      GLCamera1.Position.X:=GLCamera1.Position.X-(mx-x)*movespeed;
      GLCamera1.Position.Y:=GLCamera1.Position.Y-(my-y)*movespeed;
    end else begin
      movespeed:=0.3/GLCamera1.SceneScale;
      GLCamera1.MoveAroundTarget((my-y)*movespeed,(mx-x)*movespeed);
    end;
    if not FShowPhase then begin
       GLLightSource1.Position:=GLCamera1.Position;
       GLLightSource1.SpotDirection.SetVector(-GLLightSource1.Position.X,-GLLightSource1.Position.Y,-GLLightSource1.Position.Z);
    end;
    RefreshLabel;
  end;
  mx:=x;
  my:=y;
end;

procedure Tf_moon.GLSceneViewer1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  SetZoomLevel(GLCamera1.SceneScale/1.5);
end;

procedure Tf_moon.GLSceneViewer1MouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  SetZoomLevel(GLCamera1.SceneScale*1.5);
end;

procedure Tf_moon.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=false;
  if zone>1 then LoadSlice(zone);
  if assigned(FOnGetLabel) then FOnGetLabel(self);
  if marked then SetMark(markl,markb,marktext);
end;

procedure Tf_moon.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
GLSphereMoon.TurnAngle:=FRotation*newTime; // rotation ° per second
RefreshLabel;
end;

procedure Tf_moon.Timer2Timer(Sender: TObject);
begin
if assigned(FOnGetMsg) then FOnGetMsg(self,Format('%.2f FPS', [GLSceneViewer1.FramesPerSecond]));
GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure Tf_moon.OrientLightSource;
var s,c: single;
begin
sincos(FPhase,s,c);
GLScene1.BeginUpdate;
GLLightSource1.BeginUpdate;
  GLLightSource1.Position.x := -LightDist * s;
  GLLightSource1.Position.y := -LightDist * tan(-Fsunincl);
  GLLightSource1.Position.z := -LightDist * c;
  GLLightSource1.SpotDirection.x := GLLightSource1.Position.x;
  GLLightSource1.SpotDirection.y := GLLightSource1.Position.y;
  GLLightSource1.SpotDirection.z := GLLightSource1.Position.z;
GLLightSource1.EndUpdate;
GLScene1.EndUpdate;
end;

procedure Tf_moon.SetPhase(value:single);   // 0 -> full moon
begin
FPhase:=value;
if FShowPhase then OrientLightSource;
end;

procedure Tf_moon.SetSunIncl(value:single);
var s,c: single;
begin
if abs(rad2deg*value) > 89.9 then
    Fsunincl := sgn(value) * deg2rad*89.9
else
    FSunIncl:=value;
if FShowPhase then OrientLightSource;
end;

procedure Tf_moon.ResetMoon;
begin
  with GLSphereMoon do
  begin
    RollAngle := 0;
    TurnAngle := 0;
    PitchAngle := 0;
    direction.X := 0;
    direction.Y := 0;
    direction.Z := 1;
    Up.X := 0;
    Up.Y := 1;
    Up.Z := 0;
  end;
end;

procedure Tf_moon.OrientMoon;
begin
  GLScene1.BeginUpdate;
  GLSphereMoon.BeginUpdate;
    ResetMoon;
    GLSphereMoon.PitchAngle := FLibrLat;
    GLSphereMoon.TurnAngle := FLibrLon;
    GLSphereMoon.up.x := 0;
  GLScene1.EndUpdate;
  GLSphereMoon.EndUpdate;
end;

procedure Tf_moon.SetLibrLon(value:single);
begin
FLibrLon:=value;
if FVisibleSideLock then OrientMoon;
end;

procedure Tf_moon.SetLibrLat(value:single);
begin
FLibrLat:=value;
if FVisibleSideLock then OrientMoon;
end;

procedure Tf_moon.SetLabelFont(f:Tfont);
begin
  if f.Size>20 then f.size:=20;
  GLBitmapFont1.Font:=f;
end;

function  Tf_moon.GetLabelFont : Tfont;
begin
  result:=GLBitmapFont1.Font;
end;

procedure Tf_moon.SetLabelColor(c:TColor);
var i: single;
begin
  Flabelcolor:=c;
  InitLabel;
end;

procedure Tf_moon.InitLabel;
var 	i : Integer;
	newlabel : TGLHUDText;
begin
GLDummyCubeLabels.DeleteChildren;
curlabel:=0;
for i:=0 to 2*Maxlabel do begin
    newlabel:=TGLHUDText(GLDummyCubeLabels.AddNewChild(TGLHUDText));
    newlabel.Name:='ML'+inttostr(i);
    newlabel.Visible:=false;
    newlabel.BitmapFont:=GLBitmapFont1;
    newlabel.Layout:=tlCenter;
    newlabel.Up.SetVector(0,1,0);
//    newlabel.Scale.SetVector(Label3dSize*LabelSize,Label3dSize*LabelSize,1);
    if (i mod 2) = 0 then
       newlabel.ModulateColor.AsWinColor:=clBlack
    else
       newlabel.ModulateColor.AsWinColor:=Flabelcolor;
end;
end;

Procedure Tf_moon.ClearLabel;
var i:integer;
begin
curlabel:=0;
if (GLDummyCubeLabels.Count>0) and GLDummyCubeLabels.Children[0].visible then
   for i:=0 to 2*Maxlabel do
      with GLDummyCubeLabels.Children[i] as TGLHUDText do visible:=false;
GLHUDSpriteMark.Visible:=false;
GLHUDTextMark.Visible:=false;
GLHUDTextMarkShadow.Visible:=false;
end;

function Tf_moon.AddLabel(lon,lat:single; txt:string):boolean;
var x,y: integer;
begin
result:=false;
if curlabel>MaxLabel then exit;
if marked and (marktext=txt) then exit;
if not Moon2Screen(lon,lat,x,y) then exit;
if (x > 0) and (y > 0) and (x < GLSceneViewer1.Width) and
  (y < GLSceneViewer1.Height) and ((currenteyepiece = 0) or
  (sqrt(Intpower(GLSceneViewer1.Width / 2 - x, 2) + Intpower(
  GLSceneViewer1.Height / 2 - y, 2)) < 0.475 * GLSceneViewer1.Width))
  then begin
    with GLDummyCubeLabels.Children[2*curlabel] as TGLHUDText do begin
      Position.SetVector(x+1,y+1);
      if labelcenter then
        begin
          Text      := txt;
          Alignment := taCenter;
        end
        else
        begin
          Text      := '.' + txt;
          Alignment := taLeftJustify;
      end;
      Visible:=true;
    end;
    with GLDummyCubeLabels.Children[2*curlabel+1] as TGLHUDText do begin
      Position.SetVector(x,y);
      if labelcenter then
        begin
          Text      := txt;
          Alignment := taCenter;
        end
        else
        begin
          Text      := '.' + txt;
          Alignment := taLeftJustify;
      end;
      Visible:=true;
    end;
    inc(curlabel);
end;
end;

procedure Tf_moon.SetMark(lon,lat:single; txt:string);
var x,y: integer;
begin
if (txt>'')and Moon2Screen(lon,lat,x,y) then begin
  marked:=true;
  marktext:=txt;
  markl:=lon;
  markb:=lat;
  if showmark then
    begin
      GLHUDSpriteMark.Position.SetVector(x,y);
      GLHUDSpriteMark.Width:=marksize;
      GLHUDSpriteMark.Height:=marksize;
      GLHUDSpriteMark.Material.FrontProperties.Emission.AsWinColor := MarkColor;
      GLHUDSpriteMark.Visible:=true;
    end else
      GLHUDSpriteMark.Visible:=false;
  if showlabel then
    begin
      if x < (glsceneviewer1.Width div 2) then
      begin
        x:=x + 4 ;
        GLHUDTextMarkShadow.Position.SetVector(x+1,y+1);
        GLHUDTextMark.Position.SetVector(x,y);
        GLHUDTextMarkShadow.Alignment  := taLeftJustify;
        GLHUDTextMark.Alignment  := taLeftJustify;
      end else begin
        x:=x - 4 ;
        GLHUDTextMarkShadow.Position.SetVector(x+1,y+1);
        GLHUDTextMark.Position.SetVector(x,y);
        GLHUDTextMarkShadow.Alignment  := taRightJustify;
        GLHUDTextMark.Alignment  := taRightJustify;
      end;
      GLHUDTextMarkShadow.Text:=txt;
      GLHUDTextMark.Text:=txt;
      GLHUDTextMark.ModulateColor.AsWinColor := marklabelcolor;
      GLHUDTextMarkShadow.Visible:=true;
      GLHUDTextMark.Visible:=true;
    end else begin
      GLHUDTextMarkShadow.Visible:=false;
      GLHUDTextMark.Visible:=false;
    end;
end else begin
  marked:=false;
  GLHUDSpriteMark.Visible:=false;
  GLHUDTextMark.Visible:=false;
  GLHUDTextMarkShadow.Visible:=false;
end;
end;

procedure Tf_moon.SetMirror(value:boolean);
begin
if FMirror<>value then begin
    FMirror:=value;
    if FMirror then
    begin
      GLMirror1.Visible     := True;
      GLCamera1.Direction.SetVector(0,0,-1);
    end
    else
    begin
      GLMirror1.Visible     := False;
      GLCamera1.Direction.SetVector(0,0,1);
    end;
    RefreshLabel;
end;
end;

procedure Tf_moon.SetOrientation(value: single);
var
  c1, s1: single;
begin
if value<>FOrientation then begin
  FOrientation:=value;
  sincos(deg2rad * FOrientation, s1, c1);
  GLHUDSpriteDistance.Visible := False;
  GLCamera1.BeginUpdate;
  GLCamera1.Up.X := s1;
  GLCamera1.Up.Y := c1;
  GLCamera1.Up.X := s1;
  GLCamera1.Up.Y := c1;
  GLCamera1.Up.X := s1;
  GLCamera1.Up.Y := c1;
  GLCamera1.EndUpdate;
//  SetScrollBar(GLCamera1.Position.x, GLCamera1.Position.y);
//  Mark(shapePositionX, shapePositionY, hudtext1.Text);
end;
end;

procedure Tf_moon.CenterAt(lon,lat:single);
var x,y: single;
    sl,cl,sb,cb: single;
begin
if VisibleSideLock then begin
  if Moon2World(lon,lat,x,y) then begin
     GLCamera1.Position.X:=x;
     GLCamera1.Position.Y:=y;
  end;
end else begin
  sincos(lon+deg2rad*FLibrLon,sl,cl);
  sincos(lat-deg2rad*FLibrLat,sb,cb);
  GLScene1.BeginUpdate;
  GLCamera1.BeginUpdate;
  GLCamera1.Position.x := -100*cb * sl;
  GLCamera1.Position.y := 100 * sb;
  GLCamera1.Position.z := -100*cb * cl;
  GLCamera1.EndUpdate;
  if not FShowPhase then begin
     GLLightSource1.Position:=GLCamera1.Position;
     GLLightSource1.SpotDirection.SetVector(-GLLightSource1.Position.X,-GLLightSource1.Position.Y,-GLLightSource1.Position.Z);
  end;
  GLScene1.EndUpdate;
end;
RefreshLabel;
end;

procedure Tf_moon.CenterMark;
var x,y: integer;
begin
CenterAt(markl,markb);
end;

initialization
  {$i pu_moon.lrs}

end.
