unit pu_moon;
{$MODE Objfpc}
{$H+}

interface

uses u_util, u_constant, u_projection, Graphics, GLGraphics, GLContext,
  GLColor, GLObjects, GLMisc, ExtCtrls, GLTexture, GLCadencer, Info,
  GLViewer, GLCrossPlatform, LResources, GLScene, GLMultiMaterialShader,
  StdCtrls, GLBumpShader, GLHUDObjects, GLWindowsFont, GLGeomObjects, GLMirror,
  Messages, SysUtils, Classes, Controls, Forms, AsyncTimer, Menus ;

const
   MaxLabel=500;

type

  { Tf_moon }

  TMoonMsgClass = (MsgZoom, MsgPerf, MsgOther);
  TMoonKeyClass = (mkUp, mkDown);

  TMoonClickEvent = procedure(Sender: TObject; Button: TMouseButton;
                     Shift: TShiftState; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single) of object;
  TMoonMoveEvent = procedure(Sender: TObject; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single) of object;
  TGetSingleEvent = procedure(Sender: TObject; value: single) of object;
  TGetStringEvent = procedure(Sender: TObject; value: string) of object;
  TGetMsgEvent = procedure(Sender: TObject; msgclass:TMoonMsgClass; value: string) of object;
  TMoonMeasureEvent = procedure(Sender: TObject; m1,m2,m3,m4: string) of object;

  Tf_moon = class(TForm)
    GLAnnulus1: TGLAnnulus;
    GLArrowLine1: TGLArrowLine;
    GLBumpShader1: TGLBumpShader;
    LibrationDummyCube: TGLDummyCube;
    PerfCadencer: TGLCadencer;
     RotationCadencer: TGLCadencer;
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
//     DummyCube1: TGLDummyCube;
     GLLightSource1: TGLLightSource;
     GLSphereMoon: TGLSphere;
     GLBitmapFont1: TGLWindowsBitmapFont;
     Moon: TPanel;
     RefreshTimer: TTimer;
     procedure FormCreate(Sender: TObject);
     procedure FormDestroy(Sender: TObject);
     procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
       Shift: TShiftState; X, Y: Integer);
     procedure PerfCadencerProgress(Sender: TObject; const deltaTime,
       newTime: Double);
     procedure RotationCadencerProgress(Sender: TObject; const deltaTime,
       newTime: Double);
     procedure GLSceneViewer1MouseDown(Sender: TObject;
       Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
     procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
       X, Y: Integer);
     procedure GLSceneViewer1MouseWheelDown(Sender: TObject;
       Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
     procedure GLSceneViewer1MouseWheelUp(Sender: TObject;
       Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
     procedure RefreshTimerTimer(Sender: TObject);
  private
    { Déclarations privées }
    marked: boolean;
    marktext: string;
    markl,markb: single;
    MaxTextureSize: integer;
    MaxZoom: single;
    mx,my, zone, maxzone,curlabel,cursprite : integer;
    maps2, newmaps: array[0..8] of integer;
    pmaps2 : array[0..2] of integer;
    cap2,newcap: integer;
    perftime: double;
    lock_Zoom : boolean;
    distancestart: boolean;
    startl,startb,startxx,startyy : single;
    startx, starty, ShadowOffset : integer;
    blankbmp: Tbitmap;
    MaxSprite: integer;
    FOnMoonClick: TMoonClickEvent;
    FOnMoonMove: TMoonMoveEvent;
    FonMoonMeasure: TMoonMeasureEvent;
    FOnGetMsg: TGetMsgEvent;
    FOnGetLabel: TNotifyEvent;
    FOnGetSprite: TNotifyEvent;
    FTexturePath: String;
    FTexture: String;
    FOverlayPath: String;
    FOverlay: String;
    FBumpPath: String;
    FBumpmap: Boolean;
    FBumpOk : boolean;
    FAsMultiTexture: boolean;
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
    perfdeltay: double;
    FShowFPS: Boolean;
    FMoveCursor: boolean;
    FLibrationMark: Boolean;
    FEyepiece: single;
    FTextureCompression: Boolean;
    TextureCmp: TGLTextureCompression;
    FMeasuringDistance: Boolean;
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
    function  GetLabelFont : Tfont;
    procedure SetShowFPS(value:boolean);
    procedure SetPopUp(value:TPopupMenu);
    function  GetPopUp : TPopupMenu;
    procedure SetEyepiece(value:single);
    procedure SetTextureCompression(value:boolean);
    procedure SetMeasuringDistance(value:boolean);
    procedure MeasureDistance(x, y: integer);
    function  GetAcceleration: integer;
    procedure SetAmbientColor(value:TColor);
    function  GetAmbientColor:TColor;
    procedure SetDiffuseColor(value:TColor);
    function  GetDiffuseColor:TColor;
    procedure SetSpecularColor(value:TColor);
    function  GetSpecularColor:TColor;
    procedure DisableSlice2;
    procedure SetLabelColor(c:TColor);
    procedure CreateMaterial(level:integer);
    Procedure LoadSlice(lv:integer);
    Procedure ClearSlice(level:integer);
    procedure ClearOverlay;
    Procedure SetZoomLevel(zoom:single);
    procedure SetMoveCursor(value:boolean);
    function  Screen2Moon(x,y:integer; var lon,lat: single): boolean;
    function  Moon2Screen(lon,lat: single; var x,y:integer): boolean;
    function  Moon2World(lon,lat: single; var x,y,z:single): boolean;
    function  World2Moon(x,y,z:single; var lon,lat: single): boolean;
    procedure InitLabel;
    procedure ClearLabel;
    procedure InitSprite;
    procedure MoreSprite;
    procedure OrientLightSource;
    procedure OrientMoon;
    procedure ResetMoon;
    procedure ShowLibrationMark;
  public
    { Declarations publiques }
    procedure ShowInfo;
    procedure Init;
    procedure GetBounds(var lmin,lmax,bmin,bmax: single);
    procedure SetMark(lon,lat:single; txt:string);
    procedure CenterAt(lon,lat:single);
    procedure CenterMark;
    procedure KeyEvent(event: TMoonKeyClass; key: word);
    function AddLabel(lon,lat:single; txt:string):boolean;
    function AddSprite(lon,lat:single):boolean;
    procedure RefreshAll;
    procedure RenderToBitmap(var bmp: TBitmap; size: integer; white: boolean);
    procedure SnapShot(var bmp: TBitmap; white: boolean);
    property TexturePath : String read FtexturePath write FTexturePath;
    property Texture : String read Ftexture write SetTexture;
    property OverlayPath : String read FOverlayPath write FOverlayPath;
    property Overlay : String read FOverlay write SetOverlay;
    property BumpPath : String read FBumpPath write SetBumpPath;
    property Bumpmap : Boolean read FBumpmap write SetBumpmap;
    property CanBump : Boolean read FBumpOk;
    property AsMultiTexture : boolean read FAsMultiTexture;
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
    property ShowFPS: Boolean read FShowFPS write SetShowFPS;
    property LibrationMark: Boolean read FLibrationMark write FLibrationMark;
    property PopUp: TPopupMenu read GetPopUp write SetPopUp;
    // Eyepiece field of vision in Moon apparent diameter unit.
    property Eyepiece : single read FEyepiece write SetEyepiece;
    property TextureCompression: Boolean read FTextureCompression write SetTextureCompression;
    property MeasuringDistance: Boolean read FMeasuringDistance write SetMeasuringDistance;
    property Acceleration: integer read GetAcceleration;
    property AmbientColor: TColor read GetAmbientColor Write SetAmbientColor;
    property DiffuseColor: TColor read GetDiffuseColor Write SetDiffuseColor;
    property SpecularColor: TColor read GetSpecularColor Write SetSpecularColor;
    property MoveCursor: Boolean read FMoveCursor Write SetMoveCursor;
    property onMoonClick : TMoonClickEvent read FOnMoonClick write FOnMoonClick;
    property onMoonMove : TMoonMoveEvent read FOnMoonMove write FOnMoonMove;
    property onMoonMeasure: TMoonMeasureEvent read FonMoonMeasure write FonMoonMeasure;
    property onGetMsg : TGetMsgEvent read FOnGetMsg write FOnGetMsg;
    property onGetLabel : TNotifyEvent read FOnGetLabel write FOnGetLabel;
    property onGetSprite : TNotifyEvent read FonGetSprite write FonGetSprite;
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
              Material.FrontProperties.Ambient.AsWinColor:=clWhite;
              Material.FrontProperties.Diffuse.AsWinColor:=clWhite;
              Material.FrontProperties.Specular.AsWinColor:=clWhite;
              Material.Texture.Compression:=TextureCmp;
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
              Material.FrontProperties.Ambient.AsWinColor:=clWhite;
              Material.FrontProperties.Diffuse.AsWinColor:=clWhite;
              Material.FrontProperties.Specular.AsWinColor:=clWhite;
              Material.Texture.Compression:=TextureCmp;
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
              Material.FrontProperties.Ambient.AsWinColor:=clWhite;
              Material.FrontProperties.Diffuse.AsWinColor:=clWhite;
              Material.FrontProperties.Specular.AsWinColor:=clWhite;
              Material.Texture.Compression:=TextureCmp;
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
        Material.FrontProperties.Ambient.AsWinColor:=clWhite;
        Material.FrontProperties.Diffuse.AsWinColor:=clWhite;
        Material.FrontProperties.Specular.AsWinColor:=clWhite;
        Material.Texture.Compression:=TextureCmp;
        Material.Texture.ImageAlpha:=tiaBottomRightPointColorTransparent;
        Material.Texture.TextureWrap:=twNone;
        Material.Texture.TextureMode:=tmModulate;
       end;
    end;
end;
end;
end;

Procedure Tf_moon.LoadSlice(lv:integer);
var toffset,tscale : single;
    tpath,nn: string;
    i,j,k,row,col,maxcol,maxrow,level : integer;
    lc,bc:single;
    ok: boolean;
    jp: TJPEGImage;

procedure LoadSlice2;
var i,j: integer;
begin
 try
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
              Material.Texture.ImageBrightness:=1;
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
              Material.Texture.ImageBrightness:=1;
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
    except
    end;
end;
// LoadSlice
begin
try
level:=lv;
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
            Material.Texture.ImageBrightness:=1;
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
        Material.Texture.ImageBrightness:=1;
{ TODO : Alpha = toplefttransparent
set top left to trans, color }
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
    if i>=8 then i:=4
    else if i>=4 then i:=4
    else if i>=2 then i:=2
    else i:=1;
    BumpMaterialLibrary.Materials[0].Material.Texture.Image.LoadFromFile(slash(FBumpPath)+'normal'+inttostr(i)+'k.jpg');
    BumpMaterialLibrary.Materials[1].Material.Texture.Image.LoadFromFile(slash(FBumpPath)+'map'+inttostr(i)+'k.jpg');
    GLSphereMoon.Material.MaterialLibrary:=BumpMaterialLibrary;
    GLSphereMoon.Material.LibMaterialName:='Bump';
    if GLBumpShader1.BumpMethod=bmBasicARBFP then
       GLLightSource1.ConstAttenuation:=0.8
    else
       GLLightSource1.ConstAttenuation:=0.3;
    GLLightSource1.LightStyle:=lsSpot;
    ClearSlice(2);
    MaxZoom:=6;
    if GLCamera1.SceneScale>MaxZoom then SetZoomLevel(MaxZoom);
  end else begin
    LoadSlice(zone);
    GLSphereMoon.Material.MaterialLibrary:=GLMultiMaterialLibrary;
    GLSphereMoon.Material.LibMaterialName:='MultiMaterial';
    BumpMaterialLibrary.Materials[0].Material.Texture.Image.Assign(blankbmp);
    BumpMaterialLibrary.Materials[1].Material.Texture.Image.Assign(blankbmp);
    GLLightSource1.ConstAttenuation:=0.5;
    GLLightSource1.LightStyle:=lsParallel;
    GLLightSource1.LightStyle:=lsSpot;
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
     if FBumpmap then
        GLLightSource1.LightStyle:=lsSpot
     else
        GLLightSource1.LightStyle:=lsParallel;
     GLLightSource1.Position.x:=0;
     GLLightSource1.Position.y:=0;
     GLLightSource1.Position.z:=-100;
     GLLightSource1.SpotDirection.x:=0;
     GLLightSource1.SpotDirection.y:=0;
     GLLightSource1.SpotDirection.z:=1;
     SetPhase(FPhase);
  end else begin
     GLLightSource1.LightStyle:=lsParallel;
     GLLightSource1.Position:=GLCamera1.Position;
     GLLightSource1.SpotDirection.SetVector(GLLightSource1.Position.X,GLLightSource1.Position.Y,GLLightSource1.Position.Z);
  end;
  OrientMoon;
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
      Eyepiece:=0;
      GLCamera1.TargetObject:=GLSphereMoon;
      SetZoomLevel(1);
   end;
   if not FShowPhase then begin
      GLLightSource1.Position:=GLCamera1.Position;
      GLLightSource1.SpotDirection.SetVector(GLLightSource1.Position.X,GLLightSource1.Position.Y,GLLightSource1.Position.Z);
   end;
   GLSceneViewer1.Refresh;
end;
end;

function Tf_moon.Moon2Screen(lon,lat: single; var x,y:integer): boolean;
var xx,yy,zz: single;
    v : TAffineVector;
begin
Moon2World(lon,lat,xx,yy,zz);
v[0]:=xx;
v[1]:=yy;
v[2]:=zz;
v:=GLSceneViewer1.Buffer.WorldToScreen(v);
if (v[0]>=0)and(v[0]<=GLSceneViewer1.Width)and(v[1]>=0)and(v[1]<=GLSceneViewer1.Height) then begin
  if FMirror then begin
     x:=GLSceneViewer1.Width-round(v[0]);
     y:=round(v[1]);
  end
  else begin
    x:=round(v[0]);
    y:=GLSceneViewer1.Height-round(v[1]);
  end;
  result:=Screen2Moon(x,y,xx,yy) and (abs(lon-xx)<0.1)and(abs(lat-yy)<0.1);
end
else
  result:=false;
end;

function Tf_moon.Screen2Moon(x,y:integer; var lon,lat: single): boolean;
var
  farp : integer;
  p0,p1,raystart,rayvector,ipoint:TVector;
  xx,yy: single;
begin
if FMirror then begin
   x:=GLSceneViewer1.Width-x;
   farp:=-1;
end else begin
   y:=GLSceneViewer1.height-y;
   farp:=1;
end;
p0:=GLSceneViewer1.Buffer.ScreenToWorld(vectormake(x,y,0));
p1:=GLSceneViewer1.Buffer.ScreenToWorld(vectormake(x,y,farp));
raystart:=p0;
rayvector:=vectornormalize(vectorsubtract(p1,p0));
if GLSphereMoon.RayCastIntersect(raystart, rayvector, @ipoint)
then  begin
   result:=World2Moon(ipoint[0],ipoint[2],ipoint[1],lon,lat);
   if GLAnnulus1.Visible then begin
      xx:=ipoint[0]-GLCamera1.Position.X;
      yy:=ipoint[1]-GLCamera1.Position.Y;
      if sqrt(xx*xx+yy*yy)>(GLAnnulus1.BottomInnerRadius)
      then result:=false;
   end;
end else begin
  result:=false;
end;
end;

function Tf_moon.Moon2World(lon,lat: single; var x,y,z:single): boolean;
var qr,l,b,xx,yy,zz: single;
    v : TAffineVector;
begin
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
  z:=v[2];
  result:=true;
end;

function Tf_moon.World2Moon(x,y,z:single; var lon,lat: single): boolean;
var lo,la,qr: single;
begin
   lo:=-arctan2(y,x)-pi/2;
   if (lo<-pi) then lo:=lo+2*pi;
   qr:=sqrt(x*x+y*y);
   if qr<>0 then begin
      la:=arctan(z/qr);
      lat:=la+deg2rad*LibrLat*cos(lo);
      lon:=lo-deg2rad*LibrLon+deg2rad*LibrLat*tan(lat)*sin(lo);
      result:=true;
   end else begin
      lat:=0;
      result:=false;
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
  if (not FBumpmap)and(newzone<>zone)and(zone>1) then begin
     if newzone=1 then
       ClearSlice(2)
     else
       DisableSlice2;
  end;
  zone:=newzone;
  if assigned(FOnGetMsg) then FOnGetMsg(self,MsgZoom,'Zoom:'+formatfloat('0.0',zoom)+'  Level:'+inttostr(zone));
  RefreshAll;
finally
  lock_Zoom:=false;
end;
end;

procedure Tf_moon.RefreshAll;
begin
  ClearLabel;
  RefreshTimer.Enabled:=false;
  RefreshTimer.Enabled:=true;
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
 ClearSlice(2);
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
 RotationCadencer.Enabled:=(FRotation<>0);
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
 perfdeltay := 0.0000001;
 cap2:=-1;
 zone:=1;
 ShadowOffset:=1;
 FTexture:='';
 FOverlay:='';
 FRotation:=0;
 FBumpOk:=false;
 FMoveCursor:=false;
 TextureCompression:=true;
 MaxZoom:=3;
 MaxTextureSize:=1024;
 Flabelcolor:=clWhite;
 FMeasuringDistance := False;
 GLLightSource1.Ambient.AsWinColor :=$4B4B4B;
 GLLightSource1.Diffuse.AsWinColor :=$FFFFFF;
 GLLightSource1.Specular.AsWinColor:=$474747;
end;

procedure Tf_moon.Init;
begin
try
  if  GL_ARB_multitexture
  and GL_ARB_vertex_program then begin
    if GL_ARB_fragment_program then begin
       GLBumpShader1.BumpMethod:=bmBasicARBFP;
       FBumpOk:=true;
    end
    else if GL_ARB_texture_env_dot3 then begin
       GLBumpShader1.BumpMethod:=bmDot3TexCombiner;
       FBumpOk:=true;
    end;
end;
if GLSceneViewer1.Buffer.Acceleration=chaSoftware then begin
   raise exception.Create('This program only run with a graphic card that support OpenGL hardware acceleration.');
   halt;
end;
MaxTextureSize:=Glsceneviewer1.Buffer.LimitOf[limTextureSize];
if MaxTextureSize<1024 then begin
   raise exception.Create('Graphic card not supported! Texture max size:'+inttostr(MaxTextureSize)+'. Must be at least 1024.');
   halt;
end;
FAsMultiTexture := GL_ARB_multitexture and (GLSceneViewer1.Buffer.LimitOf[limNbTextureUnits] > 1);
InitLabel;
InitSprite;
except
  raise exception.Create('Could not initialize OpenGL.');
  halt;
end;
end;

procedure Tf_moon.FormDestroy(Sender: TObject);
begin
 if GLDummyCubeLabels<>nil then GLDummyCubeLabels.DeleteChildren;
 if GLDummyCubeMarks<>nil then GLDummyCubeMarks.DeleteChildren;
 blankbmp.Free;
 GLSceneViewer1.Buffer.DestroyRC;
end;

procedure Tf_moon.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Distance
  if measuringdistance and distancestart then
  begin
    MeasureDistance(x, y);
//    ShowCoordinates(x, y);
    distancestart := False;
  end
  else begin
    if (ssMiddle in shift) or
       (FMoveCursor and (ssLeft in shift)) or
       ((ssLeft in shift)and(ssCtrl in shift))
     then begin
        if FMoveCursor then GLSceneViewer1.Cursor:=crHandPoint
               else GLSceneViewer1.Cursor:=crRetic;
     end;
  end;
end;

procedure Tf_moon.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var lat,lon,z,s1,c1: single;
    OnMoon: boolean;
    xx:integer;
begin
  if FMirror then xx:=GLSceneViewer1.Width-x
             else xx:=x;
  if FVisibleSideLock then begin
      sincos(deg2rad * FOrientation, s1, c1);
      mx:=round(xx*c1+y*s1);
      my:=round(y*c1-xx*s1);
  end else begin
    mx:=xx;
    my:=y;
  end;
  OnMoon:=false;
  if FMeasuringDistance and (Button = mbLeft) then
  begin
    if Screen2Moon(x,y,startl,startb) then begin
      startx  := x;
      starty  := y;
      Moon2World(startl,startb,startxx,startyy,z);
      distancestart := True;
      SetMark(0, 0, '');
      GLHUDSpriteDistance.Visible    := True;
      GLHUDSpriteDistance.Material.FrontProperties.Emission.AsWinColor := MarkColor;
      GLHUDSpriteDistance.Width      := 1;
      GLHUDSpriteDistance.Position.X := startx;
      GLHUDSpriteDistance.Position.Y := starty;
      GLHUDSpriteDistance.Rotation   := 0;
    end;
  end
  else begin
    if ((not FMoveCursor)and(ssLeft in shift)) or
       ((ssLeft in shift)and(ssShift in shift))
     then begin
       OnMoon:=Screen2Moon(x,y,lon,lat);
       if Assigned(onMoonClick) then onMoonClick(Self,Button,Shift,X,Y,OnMoon,lon,lat);
    end;
    if (ssMiddle in shift) or
       (FMoveCursor and (ssLeft in shift)) or
       ((ssLeft in shift)and(ssCtrl in shift))
     then begin
        GLSceneViewer1.Cursor:=crHandPoint;
     end;
  end;
end;

procedure Tf_moon.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var movespeed,s1,c1: single;
    xx,yy:integer;
    lat,lon: single;
    OnMoon: boolean;
begin
  // Distance
  if FMeasuringDistance and distancestart then
     begin
       MeasureDistance(x, y);
//       ShowCoordinates(x, y);
     end
  else
  // Cord. display
  if shift=[] then begin
     if Assigned(onMoonMove) then begin
        OnMoon:=Screen2Moon(x,y,lon,lat);
        onMoonMove(Self,X,Y,OnMoon,lon,lat);
     end;
  end
  else
  // Move map
  if (ssMiddle in shift) or
     (FMoveCursor and (ssLeft in shift)) or
     ((ssLeft in shift)and(ssCtrl in shift))
   then begin
    if FMirror then x:=GLSceneViewer1.Width-x;
    if FVisibleSideLock then begin
      sincos(deg2rad * FOrientation, s1, c1);
      xx:=round(x*c1+y*s1);
      yy:=round(y*c1-x*s1);
      movespeed:=0.002/GLCamera1.SceneScale;
      GLCamera1.Position.X:=GLCamera1.Position.X-(mx-xx)*movespeed;
      GLCamera1.Position.Y:=GLCamera1.Position.Y-(my-yy)*movespeed;
      GLAnnulus1.Position.x := GLCamera1.Position.x;
      GLAnnulus1.Position.y := GLCamera1.Position.y;
      mx:=xx;
      my:=yy;
    end else begin
      movespeed:=0.3/GLCamera1.SceneScale;
      GLCamera1.MoveAroundTarget((my-y)*movespeed,(mx-x)*movespeed);
      mx:=x;
      my:=y;
    end;
    if not FShowPhase then begin
       GLLightSource1.Position:=GLCamera1.Position;
       GLLightSource1.SpotDirection.SetVector(GLLightSource1.Position.X,GLLightSource1.Position.Y,GLLightSource1.Position.Z);
    end;
    RefreshAll;
  end;
end;

procedure Tf_moon.GLSceneViewer1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  SetZoomLevel(GLCamera1.SceneScale/1.1);
end;

procedure Tf_moon.GLSceneViewer1MouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  SetZoomLevel(GLCamera1.SceneScale*1.1);
end;

procedure Tf_moon.RefreshTimerTimer(Sender: TObject);
begin
  RefreshTimer.Enabled:=false;
  if marked then SetMark(markl,markb,marktext);
  ShowLibrationMark;
  if assigned(FOnGetSprite) then FOnGetSprite(self);
  if assigned(FOnGetLabel) then FOnGetLabel(self);
  if zone>1 then LoadSlice(zone);
end;

procedure Tf_moon.RotationCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
GLCamera1.MoveAroundTarget(0,FRotation*deltaTime);
RefreshAll;
if zone>1 then LoadSlice(zone);
end;

procedure Tf_moon.PerfCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  perfdeltay := -perfdeltay;
  GLCamera1.Position.y := GLCamera1.Position.y + perfdeltay;
  if newtime>(perftime+2) then begin
    if assigned(FOnGetMsg) then FOnGetMsg(self,MsgPerf,Format('%.2f FPS', [GLSceneViewer1.FramesPerSecond]));
    GLSceneViewer1.ResetPerformanceMonitor;
    perftime:=newtime;
    Application.ProcessMessages;
  end;
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
  with LibrationDummyCube do
  begin
    RollAngle := 0;
    TurnAngle := 0;
    PitchAngle := 0;
    direction.SetVector(0,0,1);
    Up.SetVector(0,1,0);
  end;
  with GLSphereMoon do
  begin
    RollAngle := 0;
    TurnAngle := 0;
    PitchAngle := 0;
    direction.SetVector(0,0,1);
    Up.SetVector(0,1,0);
  end;
end;

procedure Tf_moon.OrientMoon;
begin
  GLScene1.BeginUpdate;
  ResetMoon;
  if FShowPhase then begin
    LibrationDummyCube.PitchAngle := FLibrLat;
    LibrationDummyCube.TurnAngle := FLibrLon;
    LibrationDummyCube.up.x := 0;
  end else begin
    GLSphereMoon.PitchAngle := FLibrLat;
    GLSphereMoon.TurnAngle := FLibrLon;
    GLSphereMoon.up.x := 0;
  end;
  GLScene1.EndUpdate;
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
  case f.size of
  0..12 : ShadowOffset:=1;
  else    ShadowOffset:=2;
  end;
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

procedure Tf_moon.MoreSprite;
var
  i: integer;
  newSprite: TGLHUDSprite;
begin
  for i := 0 to InitialSprite - 1 do
  begin
    newSprite      := TGLHUDSprite(GLDummyCubeMarks.AddNewChild(TGLHUDSprite));
    newSprite.Name := 'MoonSprite' + IntToStr(maxsprite + i);
    newSprite.Visible := False;
    newSprite.Material.FrontProperties.Emission.AsWinColor := MarkColor;
    newSprite.Material.FrontProperties.Ambient.Color := clrBlack;
    newSprite.Material.FrontProperties.Diffuse.Color := clrBlack;
    newSprite.Material.FrontProperties.Specular.Color := clrBlack;
    newSprite.Up.SetVector(0, 1, 0);
    newSprite.Scale.SetVector(1, 1, 1);
    newSprite.Width  := marksize;
    newSprite.Height := marksize;
  end;
  maxsprite := maxsprite + InitialSprite;
end;

procedure Tf_moon.InitSprite;
begin
  maxsprite := 0;
  cursprite:=0;
  GLDummyCubeMarks.DeleteChildren;
  MoreSprite;
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
cursprite:=0;
if (GLDummyCubeLabels.Count>0) and GLDummyCubeLabels.Children[0].visible then
   for i:=0 to 2*Maxlabel do
      with GLDummyCubeLabels.Children[i] as TGLHUDText do visible:=false;
if (GLDummyCubeMarks.Count > 0) and GLDummyCubeMarks.Children[0].Visible then
   for i := 0 to MaxSprite - 1 do
      with GLDummyCubeMarks.Children[i] as TGLHUDSprite do Visible := False;
GLHUDSpriteMark.Visible:=false;
GLHUDTextMark.Visible:=false;
GLHUDTextMarkShadow.Visible:=false;
GLHUDSpriteDistance.Visible:= false;
distancestart := false;
end;

function Tf_moon.AddSprite(lon,lat:single):boolean;
var x,y: integer;
begin
result:=false;
if cursprite>AbsoluteMaxSprite then exit;
if not Moon2Screen(lon,lat,x,y) then exit;
if (x > 0) and (y > 0) and (x < GLSceneViewer1.Width) and
  (y < GLSceneViewer1.Height) and ((currenteyepiece = 0) or
  (sqrt(Intpower(GLSceneViewer1.Width / 2 - x, 2) + Intpower(
  GLSceneViewer1.Height / 2 - y, 2)) < 0.475 * GLSceneViewer1.Width))
  then begin
      with GLDummyCubeMarks.Children[cursprite] as TGLHUDSprite do
      begin
        Width      := marksize;
        Height     := marksize;
        Visible    := True;
        Position.SetVector(x,y);
      end;
      Inc(cursprite);
      if (cursprite>=MaxSprite)and(cursprite<AbsoluteMaxSprite) then MoreSprite;
  end;
end;

function Tf_moon.AddLabel(lon,lat:single; txt:string):boolean;
var x,y: integer;
begin
result:=false;
if curlabel>=MaxLabel then exit;
if marked and (marktext=txt) then exit;
if not Moon2Screen(lon,lat,x,y) then exit;
if (x > 0) and (y > 0) and (x < GLSceneViewer1.Width) and
  (y < GLSceneViewer1.Height) and ((currenteyepiece = 0) or
  (sqrt(Intpower(GLSceneViewer1.Width / 2 - x, 2) + Intpower(
  GLSceneViewer1.Height / 2 - y, 2)) < 0.475 * GLSceneViewer1.Width))
  then begin
    with GLDummyCubeLabels.Children[2*curlabel] as TGLHUDText do begin
      Position.SetVector(x+ShadowOffset,y+ShadowOffset);
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
    RefreshAll;
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
  GLCamera1.Up.SetVector(s1,c1);
  GLCamera1.EndUpdate;
end;
end;

procedure Tf_moon.CenterAt(lon,lat:single);
var x,y,z: single;
    sl,cl,sb,cb: single;
begin
if VisibleSideLock then begin
  if Moon2World(lon,lat,x,y,z) then begin
     GLCamera1.Position.X:=x;
     GLCamera1.Position.Y:=y;
     GLAnnulus1.Position.x := GLCamera1.Position.x;
     GLAnnulus1.Position.y := GLCamera1.Position.y;
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
     GLLightSource1.SpotDirection.SetVector(GLLightSource1.Position.X,GLLightSource1.Position.Y,GLLightSource1.Position.Z);
  end;
  GLScene1.EndUpdate;
end;
RefreshAll;
end;

procedure Tf_moon.CenterMark;
var x,y: integer;
begin
CenterAt(markl,markb);
end;

procedure Tf_moon.SetShowFPS(value:boolean);
begin
if FShowFPS<>value then begin
   FShowFPS:=value;
   PerfCadencer.Enabled:=FShowFPS;
end;
end;

procedure Tf_moon.ShowLibrationMark;
var
  a, x, y: double;
  xx, yy:  integer;
begin
  with GLArrowline1 do
  if FLibrationMark and VisibleSideLock then
    begin
      Visible := True;
      a      := arctan2(FLibrLat, FLibrLon);
      x      := sqrt(FLibrLat * FLibrLat + FLibrLon * FLibrLon);
      Height := x * 0.005 / zoom;
      position.X := (0.5 + Height / 1.5) * cos(a);
      position.Y := (0.5 + Height / 1.5) * sin(a);
      TopRadius := Height / 10;
      BottomRadius := TopRadius;
      TopArrowHeadRadius := 3 * TopRadius;
      TopArrowHeadHeight := 6 * TopRadius;
      Turnangle := -rad2deg * a;
      Material.FrontProperties.Emission.AsWinColor := MarkColor;
    end
    else
    begin
      Visible := False;
    end;
end;

procedure Tf_moon.SetPopUp(value:TPopupMenu);
begin
  GLSceneViewer1.PopupMenu:=value;
end;

function  Tf_moon.GetPopUp : TPopupMenu;
begin
  result:=GLSceneViewer1.PopupMenu;
end;

procedure Tf_moon.SetEyepiece(value:single);
begin
if not VisibleSideLock then value:=0;
if FEyepiece<>value then begin
   FEyepiece:=value;
   if FEyepiece=0 then begin
     GLAnnulus1.Visible:=false;
   end else begin
     GLAnnulus1.Position.x := GLCamera1.Position.x;
     GLAnnulus1.Position.y := GLCamera1.Position.y;
     GLAnnulus1.BottomInnerRadius:=GLSphereMoon.Radius*FEyepiece;
     GLAnnulus1.Visible:=true;
     SetZoomLevel(1/FEyepiece);
   end;
   RefreshAll;
end;
end;

procedure Tf_moon.SetTextureCompression(value:boolean);
begin
FTextureCompression:=value;
if TextureCompression then
  begin
    TextureCmp := tcStandard;
  end
else
  begin
    TextureCmp := tcDefault;
  end;
end;

procedure Tf_moon.SetMeasuringDistance(value:boolean);
begin
  FMeasuringDistance := value;
  if FMeasuringDistance then
  begin
    GLSceneViewer1.Cursor := crCross;
  end
  else
  begin
    if FMoveCursor then GLSceneViewer1.Cursor:=crHandPoint
               else GLSceneViewer1.Cursor:=crRetic;
    GLHUDSpriteDistance.Visible    := False;
  end;
end;

procedure Tf_moon.MeasureDistance(x, y: integer);
var
  i: integer;
  xx, yy, zz, l, b, d: single;
  m1,m2,m3,m4: string;
begin
 if Screen2Moon(x,y,l,b) then begin
  d  := angulardistance(l, b, startl, startb);
  m1 := formatfloat(f1, d * Rmoon);
  Moon2World(l,b,xx,yy,zz);
  xx := startxx - xx;
  yy := startyy - yy;
  d  := sqrt(xx * xx + yy * yy) * diam / 3600;
  m2 := Deptostr(d);
  i:=pos(ldeg,m2)+length(ldeg);
  m2 := copy(Deptostr(d), i, 99);
  xx := startx + ((x - startx) / 2);
  yy := starty + ((y - starty) / 2);
  Screen2Moon(round(xx),round(yy),l,b);    // replace by world2moon
  m3 := formatfloat(f1, rad2deg*l);
  m4 := formatfloat(f1, rad2deg*b);
  x := x - startx;
  y := y - starty;
  d := sqrt(x * x + y * y);
  GLHUDSpriteDistance.Width := d;
  GLHUDSpriteDistance.Position.X := startx + x / 2;
  GLHUDSpriteDistance.Position.Y := starty + y / 2;
  GLHUDSpriteDistance.Rotation := -rad2deg * (fastarctan2(y, x));
  if assigned(onMoonMeasure) then onMoonMeasure(self,m1,m2,m3,m4);
end;
end;

procedure Tf_moon.GetBounds(var lmin,lmax,bmin,bmax: single);
var
  l, b, deltab, deltal: single;
  xx, yy: integer;
  ok: boolean;
begin
  xx := GLSceneViewer1.Width div 2;
  yy := GLSceneViewer1.Height div 2;
  if Screen2Moon(xx,yy,l,b) then
  begin
    deltab := (2+sin(b))* pid2 / zoom;
    deltal := 2* deltab / cos(b);
  end
  else
  begin
    l      := deg2rad*LibrLon;
    b      := deg2rad*LibrLat;
    deltab := pid2;
    deltal := pid2;
  end;
  if abs(l) > pid2 then
  begin // face cachee
    l      := 0;
    deltal := pi;
  end;
  lmin:=l-deltal;
  lmax:=l+deltal;
  bmin:=b-deltab;
  bmax:=b+deltab;
end;

procedure Tf_moon.SnapShot(var bmp: TBitmap; white: boolean);
var
  bmp32: TGLBitmap32;
begin
  if white then
  begin
    GLSceneViewer1.Buffer.BackgroundColor := clWhite;
    GLSceneViewer1.Update;
  end;
  bmp32 := GLSceneViewer1.Buffer.CreateSnapShot;
  bmp   := bmp32.Create32BitsBitmap;
  GLSceneViewer1.Buffer.BackgroundColor := clBlack;
  bmp32.Free;
end;

procedure Tf_moon.RenderToBitmap(var bmp: TBitmap; size: integer; white: boolean);
begin
  bmp.PixelFormat := pf24bit;
  bmp.Width  := size;
  bmp.Height := size;
  if white then
    GLSceneViewer1.Buffer.BackgroundColor := clWhite;
  GLSceneViewer1.Buffer.RenderToBitmap(bmp, 96);
  GLSceneViewer1.Buffer.BackgroundColor := clBlack;
end;

function Tf_moon.GetAcceleration: integer;
begin
  case GLsceneviewer1.Buffer.Acceleration of
    chaUnknown:  result:=0;
    chaHardware: result:=1;
    chaSoftware: result:=2;
  end;
end;

procedure Tf_moon.SetAmbientColor(value:TColor);
begin
GLLightSource1.Ambient.AsWinColor := value;
end;

function  Tf_moon.GetAmbientColor:TColor;
begin
result:=GLLightSource1.Ambient.AsWinColor;
end;

procedure Tf_moon.SetDiffuseColor(value:TColor);
begin
GLlightSource1.diffuse.AsWinColor := value;
end;

function  Tf_moon.GetDiffuseColor:TColor;
begin
result:=GLlightSource1.diffuse.AsWinColor;
end;

procedure Tf_moon.SetSpecularColor(value:TColor);
begin
GLlightSource1.specular.AsWinColor := value;
end;

function  Tf_moon.GetSpecularColor:TColor;
begin
result:=GLlightSource1.specular.AsWinColor;
end;

procedure Tf_moon.ShowInfo;
begin
GLSceneViewer1.Buffer.ShowInfo;
end;

procedure Tf_moon.SetMoveCursor(value:boolean);
begin
FMoveCursor:=value;
if FMoveCursor then GLSceneViewer1.Cursor:=crHandPoint
               else GLSceneViewer1.Cursor:=crRetic;
end;

procedure Tf_moon.KeyEvent(event: TMoonKeyClass; key: word);
begin
case key of
  16  : begin   // Shift
           if FMoveCursor then begin
              if event=mkDown then GLSceneViewer1.Cursor:=crRetic
                              else GLSceneViewer1.Cursor:=crHandPoint;
           end;
        end;
  17  : begin  // Ctrl
           if not FMoveCursor then begin
              if event=mkDown then GLSceneViewer1.Cursor:=crHandPoint
                              else GLSceneViewer1.Cursor:=crRetic;
           end;
        end;
end;
end;

initialization
  {$i pu_moon.lrs}

end.
