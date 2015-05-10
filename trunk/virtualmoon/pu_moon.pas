unit pu_moon;
{$MODE Objfpc}
{$H+}

{$DEFINE trace_debug}

{
Copyright (C) 2009 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

interface

uses u_translation, u_util, u_constant, u_projection, Graphics, GLGraphics,
  GLContext, GLColor, GLObjects, GLMaterial, GLTextureFormat, ExtCtrls,
  GLTexture, GLCadencer, GLViewer, GLCrossPlatform, GLLCLViewer, LResources,
  GLScene, GLMultiMaterialShader, GLBumpShader, GLPhongShader, GLHUDObjects,
  OpenGLTokens, GLWindowsFont, GLGeomObjects, GLMirror, GLMesh,
  GLVectorFileObjects, FPImage, FileUtil, LCLType, IntfGraphics, SysUtils,
  Classes, Controls, Forms, Menus, Dialogs, Math ;

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
  TBumpMapCapability = (bcDot3TexCombiner,bcBasicARBFP);
  TBumpMapCapabilities = set of TBumpMapCapability;

  Tf_moon = class(TForm)
    GLAnnulus1: TGLAnnulus;
    GLArrowLine1: TGLArrowLine;
    GLBumpShader1: TGLBumpShader;
    GLCameraSatellite: TGLCamera;
    BaseCube: TGLDummyCube;
    GLDummyCubeSatellite: TGLDummyCube;
    GLDummyCubeCoord: TGLDummyCube;
    GLFreeFormSatelite: TGLFreeForm;
    LabelGroup: TGLHUDSprite;
    GLHUDSpriteCCD2: TGLHUDSprite;
    GLHUDSpriteCCD3: TGLHUDSprite;
    GLHUDSpriteCCD4: TGLHUDSprite;
    GLHUDSpriteCCD1: TGLHUDSprite;
    GLHUDTextScaleShadow: TGLHUDText;
    GLHUDTextScalekmShadow: TGLHUDText;
    GLHUDTextScalekm: TGLHUDText;
    GLLightSource2: TGLLightSource;
    LibrationDummyCube: TGLDummyCube;
    PerfCadencer: TGLCadencer;
     RotationCadencer: TGLCadencer;
     GLDummyCubeMarks: TGLDummyCube;
     GLHUDSpriteDistance: TGLHUDSprite;
     GLHUDSpriteMark: TGLHUDSprite;
     GLHUDTextMark: TGLHUDText;
     GLHUDTextMarkShadow: TGLHUDText;
     GLHUDSpriteScale: TGLHUDSprite;
     GLHUDTextScale: TGLHUDText;
     GLMaterialLibrary1: TGLMaterialLibrary;
     BumpMaterialLibrary: TGLMaterialLibrary;
     GLMirror1: TGLMirror;
     GLMultiMaterialLibrary: TGLMaterialLibrary;
     GLMultiMaterialShader: TGLMultiMaterialShader;
     GLSceneViewer1: TGLSceneViewer;
     GLScene1: TGLScene;
     GLCamera1: TGLCamera;
     GLLightSource1: TGLLightSource;
     GLSphereMoon: TGLSphere;
     GLBitmapFont1: TGLWindowsBitmapFont;
     Moon: TPanel;
     RefreshTimer: TTimer;
     procedure FormCreate(Sender: TObject);
     procedure FormDestroy(Sender: TObject);
     procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
       Shift: TShiftState; X, Y: Integer);
     procedure MoonResize(Sender: TObject);
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
    mx,my, zone, maxzone,curlabel,cursprite, lastyzoom : integer;
    maps2, newmaps: array[0..8] of integer;
    pmaps2 : array[0..2] of integer;
    cap2,newcap: integer;
    perftime: double;
    DownShift: TShiftState;
    lock_Zoom,SkipIdent,AbortSliceLoading,SaveShowScale : boolean;
    distancestart,BumpMapLimit1K,moveok: boolean;
    startl,startb,startxx,startyy : single;
    startx, starty, ShadowOffset : integer;
    satl,satb,satr,satli,satlc: single;
    blankbmp: Tbitmap;
    blankjp: TJPEGImage;
    MaxSprite: integer;
    FForceBumpMapSize:integer;
    FSatAltitude,FSatInclination,FSatModelScale,FSatViewDistance : single;
    FSatPosX,FSatPosY,FSatPosZ : single;
    FSatModel : string;
    FOnMoonClick: TMoonClickEvent;
    FOnMoonMove: TMoonMoveEvent;
    FonMoonMeasure: TMoonMeasureEvent;
    FOnGetMsg: TGetMsgEvent;
    FOnGetLabel: TNotifyEvent;
    FOnGetSprite: TNotifyEvent;
    FTexturePath: String;
    FTexture: TStringList;
    FOverlayPath: String;
    FOverlay,FOverlayTitle: String;
    FBumpPath: String;
    FBumpmap: Boolean;
    FBumpOk : boolean;
    FBumpMapCapabilities: TBumpMapCapabilities;
    FBumpMipmap: Boolean;
    FAsMultiTexture: boolean;
    FShowPhase: Boolean;
    FGridSpacing: integer;
    FShowGrid: Boolean;
    FShowScale: Boolean;
    FShowCCD: Boolean;
    FCCDw,FCCDh,FCCDr: single;
    FMirror: Boolean;
    FVisibleSideLock: Boolean;
    FRotation: single;
    FPhase: single;
    FSunIncl: single;
    FLibrLon: single;
    FLibrLat: single;
    FEarthDistance: single;
    FOrientation: single;
    FPoleorientation: single;
    FFollowNorth: boolean;
    FZenithOnTop: Boolean;
    Fzoom : single;
    Flabelcolor: TColor;
    perfdeltay: double;
    FShowFPS: Boolean;
    FLibrationMark: Boolean;
    FEyepiece: single;
    FTextureCompression: Boolean;
    TextureCmp: TGLTextureCompression;
    FMeasuringDistance: Boolean;
    FRaCentre, FDeCentre, FDiameter, FPositionAngle: single;
    FOverlayTransparency: single;
    FOverlayTransparencyMethode: integer;
    Fjd: double;
    procedure MoveMoonAround(anObject: TGLBaseSceneObject; pitchDelta, turnDelta: Single);
    procedure SetTexture(lfn:TStringList);
    procedure SetOverlay(fn:string);
    procedure SetBumpPath(fn:string);
    procedure SetBumpmap(value:boolean);
    procedure SetShowPhase(value:boolean);
    procedure SetMirror(value:boolean);
    procedure SetOrientation(value: single);
    procedure SetVisibleSideLock(value:boolean);
    procedure SetRotation(value:single);
    procedure SetSatModel(value:string);
    procedure SetSatModelScale(value:single);
    procedure SetPhase(value:single);
    procedure SetSunIncl(value:single);
    procedure SetLibrLon(value:single);
    procedure SetLibrLat(value:single);
    procedure SetEarthDistance(value:single);
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
    procedure SetShowGrid(value:boolean);
    procedure SetShowScale(value:boolean);
    procedure SetShowCCD(value:boolean);
    procedure SetGridSpacing(value:integer);
    function  GetBumpMethod:TBumpMapCapability;
    procedure SetBumpMethod(bm:TBumpMapCapability);
    procedure SetBumpMipmap(value: boolean);
    function  GetAntialiasing: boolean;
    procedure SetAntialiasing(value:boolean);
    procedure SetSatAltitude(value:single);
    procedure SetSatInclination(value:single);
    function  GetSatInclination:single;
    procedure SetSatViewDistance(value:single);
    function  Screen2Moon(x,y:integer; var lon,lat: single): boolean;
    function  Moon2Screen(lon,lat: single; var x,y:integer): boolean;
    function  Moon2World(lon,lat: single; var x,y,z:single): boolean;
    function  World2Moon(x,y,z:single; var lon,lat: single): boolean;
    procedure World2RaDec(x, y: single; var r, d: single);
    procedure RaDec2World(r, d: single; var x, y: single);
    procedure InitLabel;
    procedure ClearLabel;
    procedure InitSprite;
    procedure MoreSprite;
    procedure OrientLightSource;
    procedure OrientMoon;
    procedure ResetMoon;
    procedure ShowLibrationMark;
    function GetCurrentName : string;
    Procedure SetScale;
    Procedure SetCCDfield;
  public
    { Declarations publiques }
    procedure AssignMoon(Source: TF_moon);
    procedure Init(check:boolean=true);
    Procedure GetZoomInfo;
    procedure GetBounds(var lmin,lmax,bmin,bmax: single);
    function  GetCenter(var lon,lat:single):boolean;
    function  GetMarkRaDec(var ra,de: single): boolean;
    procedure CenterAtRaDec(r,d: double);
    procedure SetMark(lon,lat:single; txt:string);
    procedure CenterAt(lon,lat:single);
    procedure CenterMark;
    procedure KeyEvent(event: TMoonKeyClass; key: word);
    function AddLabel(lon,lat:single; txt:string; notcenter:boolean):boolean;
    function AddSprite(lon,lat:single):boolean;
    procedure RefreshAll;
    procedure RenderToBitmap(var bmp: TBitmap; size: integer; white: boolean);
    procedure SnapShot(var bmp: TBitmap; white: boolean);
    procedure SatCenter;
    procedure SatEast;
    procedure SatWest;
    procedure SatDirection(x,y,z:single);
    procedure SatUp(x,y,z:single);
    procedure SatPos(x,y,z:single);
    procedure SetCCD(w,h,r: single);
    property TexturePath : String read FtexturePath write FTexturePath;
    property Texture : TStringList read Ftexture write SetTexture;
    property OverlayPath : String read FOverlayPath write FOverlayPath;
    property OverlayTransparency : single read FOverlayTransparency write FOverlayTransparency;
    property OverlayTransparencyMethode : integer read FOverlayTransparencyMethode write FOverlayTransparencyMethode;
    property Overlay : String read FOverlay write SetOverlay;
    property BumpPath : String read FBumpPath write SetBumpPath;
    property Bumpmap : Boolean read FBumpmap write SetBumpmap;
    property BumpMapCapabilities: TBumpMapCapabilities read FBumpMapCapabilities;
    property BumpMethod : TBumpMapCapability read GetBumpMethod write SetBumpMethod;
    property BumpMipmap : Boolean read FBumpMipmap write SetBumpMipmap;
    property CanBump : Boolean read FBumpOk;
    property ForceBumpMapSize: integer read FForceBumpMapSize write FForceBumpMapSize;
    property AsMultiTexture : boolean read FAsMultiTexture;
    property SatelliteAltitude : single read FSatAltitude write SetSatAltitude;
    property SatInclination : single read GetSatInclination write SetSatInclination;
    property SatelliteRotation : single read FRotation write SetRotation;
    property SatelliteModel : string read FSatModel write SetSatModel;
    property SatelliteModelScale : single read FSatModelScale write SetSatModelScale;
    property SatViewDistance : single read FSatViewDistance write SetSatViewDistance;
    property Phase : single read FPhase write SetPhase;
    property JD: double read Fjd write Fjd;
    property CurrentName : string read GetCurrentName;
    property CurrentL : single read MarkL;
    property CurrentB : single read MarkB;
    property SunIncl : single read FSunIncl write SetSunIncl;
    property LibrLon : single read FLibrLon write SetLibrLon;
    property LibrLat : single read FLibrLat write SetLibrLat;
    property EarthDistance: single read FEarthDistance write SetEarthDistance;
    property RaCentre: single read FRaCentre write FRaCentre;
    property DeCentre: single read FDeCentre write FDeCentre;
    property Diameter: single read FDiameter write FDiameter;
    property PositionAngle: single read FPositionAngle write FPositionAngle;
    property Zoom : single read Fzoom write SetZoomLevel;
    property ZoomMax: single read MaxZoom;
    property Mirror : Boolean read FMirror write SetMirror;
    property Orientation:single read FOrientation write SetOrientation;
    property Poleorientation: single read FPoleorientation write FPoleorientation;
    property FollowNorth: boolean read FFollowNorth write FFollowNorth;
    property ZenithOnTop: boolean read FZenithOnTop write FZenithOnTop;
    property ShowPhase : Boolean read FShowPhase write SetShowPhase;
    property GridSpacing: integer read FGridSpacing write SetGridSpacing;
    property ShowGrid : Boolean read FShowGrid write SetShowGrid;
    property ShowScale: Boolean read FShowScale write SetShowScale;
    property ShowCCD: Boolean read FShowCCD write SetShowCCD;
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
    property Antialiasing: boolean read GetAntialiasing write SetAntialiasing;
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
  ZoomByZone: array[1..6] of integer=(3,8,16,35,65,90);

implementation

{$R pu_moon.lfm}

uses LCLProc, GLVectorGeometry, GLFile3DS, OpenGLAdapter;

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
              Material.Texture.ImageAlpha:=tiaTopLeftPointColorTransparent;
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
              Material.Texture.ImageAlpha:=tiaTopLeftPointColorTransparent;
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
              Material.Texture.ImageAlpha:=tiaTopLeftPointColorTransparent;
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
        Material.BlendingMode:=bmModulate;
        Material.FrontProperties.Ambient.AsWinColor:=clWhite;
        Material.FrontProperties.Diffuse.AsWinColor:=clWhite;
        Material.FrontProperties.Diffuse.Alpha:=0;
        Material.FrontProperties.Specular.AsWinColor:=clWhite;
        Material.Texture.Compression:=TextureCmp;
        Material.Texture.ImageAlpha:=tiaOpaque;
        Material.Texture.TextureWrap:=twNone;
        Material.Texture.TextureMode:=tmModulate;
        Material.Texture.MinFilter:=miNearest;
       end;
    end;
end;
end;
end;

Procedure Tf_moon.LoadSlice(lv:integer);
var toffset,tscale : single;
    tpath,nn: string;
    i,k,row,col,maxcol,maxrow,level : integer;
    lc,bc:single;
    ok: boolean;
    jp: TJPEGImage;
    bmp: Tbitmap;

procedure LoadSlice2;
var i,j: integer;
    pmapfn: string;
begin
 try
     toffset:=12/1024;
     tscale:=1000/1024;
     tpath:=slash(FTexturePath)+slash(Ftexture[level-1])+slash('L'+inttostr(level));
     if GLMaterialLibrary1.LibMaterialByName('L2_0')=nil then CreateMaterial(2);
     // search slices
     if RotationCadencer.Enabled then begin
       lc:=satl+pi;
       bc:=pi/2-satb;
     end else begin
       Screen2Moon(GLSceneViewer1.Width div 2, GLSceneViewer1.Height div 2, lc, bc);
       lc:=lc+pi;
       bc:=pi/2-bc;
     end;
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
        pmaps2[0]:=120000;    // South cap
        pmaps2[1]:=120001;
        pmaps2[2]:=120002;
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
        pmaps2[0]:=110000;  // North cap
        pmaps2[1]:=110001;
        pmaps2[2]:=110002;
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
       if AbortSliceLoading then break;
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
           if Ftexture[level-1]='NONE'
             then jp.Assign(blankjp)
             else jp.LoadFromFile(tpath+inttostr(maps2[k])+'.jpg');
            bmp.Assign(jp);
            with bmp.Canvas do begin
              brush.Color:=clWhite;   // replace white border because of jpeg compression
              pen.Color:=clWhite;
              FillRect(0,0,bmp.Width,12);
              FillRect(0,0,12,bmp.Height);
              FillRect(bmp.width-12,0,bmp.width,bmp.Height);
              FillRect(0,bmp.Height-12,bmp.width,bmp.Height);
            end;
            with LibMaterialByName('L2_'+inttostr(k)) do begin
              Material.Texture.ImageBrightness:=1;
              Material.Texture.Image.Assign(bmp);
              TextureOffset.SetPoint(-col*tscale+toffset,(row-maxrow+1)*tscale+toffset,0);
              TextureScale.SetPoint(maxcol*tscale,maxrow*tscale,0);
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
         if AbortSliceLoading then break;
         if pmaps2[i]>=120000 then begin
           row:=maxrow-1;
           col:=pmaps2[i] mod 120000;
           maxcol:=3;
          end
          else if pmaps2[i]>=110000 then begin
           row:=0;
           col:=pmaps2[i] mod 110000;
           maxcol:=3;
          end;
         with GLMaterialLibrary1 do begin
            if level<6 then pmapfn:=tpath+inttostr(pmaps2[i]-100000)+'.jpg'
                       else pmapfn:=tpath+inttostr(pmaps2[i])+'.jpg';
            if FileExistsUTF8(pmapfn) then begin
            if Ftexture[level-1]='NONE'
                then jp.Assign(blankjp)
                else jp.LoadFromFile(pmapfn);
            bmp.Assign(jp);
            with bmp.Canvas do begin
              brush.Color:=clWhite;
              pen.Color:=clWhite;
              FillRect(0,0,bmp.Width,12);
              FillRect(0,0,12,bmp.Height);
              FillRect(bmp.width-12,0,bmp.width,bmp.Height);
              FillRect(0,bmp.Height-12,bmp.width,bmp.Height);
            end;
            with LibMaterialByName('P2_'+inttostr(i)) do begin
              Material.Texture.ImageBrightness:=1;
              Material.Texture.Image.Assign(bmp);
              TextureOffset.SetPoint(-col*tscale+toffset,(row-maxrow+1)*tscale+toffset,0);
              TextureScale.SetPoint(maxcol*tscale,maxrow*tscale,0);
            end;
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
      on E: Exception do begin
      {$ifdef trace_debug}
        debugln('Exception LoadSlice2 '+E.Message);
      {$endif}
      end;
    end;
end;
// LoadSlice
begin
try
AbortSliceLoading:=false;
level:=lv;
jp:=TJPEGImage.Create;
bmp:=Tbitmap.Create;
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
     tpath:=slash(FTexturePath)+slash(Ftexture[level-1])+slash('L'+inttostr(level));
     if GLMaterialLibrary1.LibMaterialByName('L1_0')=nil then CreateMaterial(1);
     for i:=0 to 7 do begin
       row:=i div 4;
       col:=i mod 4;
       nn:=inttostr(i);
       with GLMaterialLibrary1 do begin
          if Ftexture[level-1]='NONE'
            then jp.Assign(blankjp)
            else jp.LoadFromFile(tpath+nn+'.jpg');
          bmp.Assign(jp);
          with bmp.Canvas do begin
            brush.Color:=clWhite;    // replace white border because of jpeg compression
            pen.Color:=clWhite;
            FillRect(0,0,bmp.Width,12);
            FillRect(0,0,12,bmp.Height);
            FillRect(bmp.width-12,0,bmp.width,bmp.Height);
            FillRect(0,bmp.Height-12,bmp.width,bmp.Height);
          end;
          with LibMaterialByName('L1_'+nn) do begin
            Material.Texture.ImageBrightness:=1;
            Material.Texture.Image.Assign(bmp);
            TextureOffset.SetPoint(-col*tscale+toffset,(row-1)*tscale+toffset,0);
            TextureScale.SetPoint(4*tscale,2*tscale,0);
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
 5 : begin
     { level 5 :
     80000x40000 full picture.
     3200 slices 1024x1024 with 1000x1000 active and 12px white border.
     Slices start on the middle of far side, longitude -180° ,
     40 rows of 80 slices from North to South.
     + 1 x 3 polars cap
     }
     maxcol:=80;
     maxrow:=40;
     LoadSlice2;
     end; // 5
 6 : begin
     { level 6 :
     160000x80000 full picture.
     12800 slices 1024x1024 with 1000x1000 active and 12px white border.
     Slices start on the middle of far side, longitude -180° ,
     80 rows of 160 slices from North to South.
     + 1 x 3 polars cap
     }
     maxcol:=160;
     maxrow:=80;
     LoadSlice2;
     end; // 6
end; //case level
except
  on E: Exception do begin
  {$ifdef trace_debug}
    debugln('Exception LoadSlice '+E.Message);
  {$endif}
  end;
end;
jp.Free;
bmp.Free;
end;

procedure Tf_moon.ClearSlice(level:integer);
var j: integer;
begin
AbortSliceLoading:=true;
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
AbortSliceLoading:=true;
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
  GLMaterialLibrary1.LibMaterialByName('O1').Material.FrontProperties.Diffuse.Alpha:=0;
  GLSceneViewer1.Refresh;
end;
end;

procedure FixImgSize(b:TBitmap);
{$ifdef mswindows}
var x,y: integer;
{$endif}
begin
{ TODO : Dirty hack to make the image to load on my XP machine. Work without that on Vista and Linux. }
{$ifdef mswindows}
  x:=b.height;
  if (x=8192)or(x=4096)or(x=2048)or(x=1024)or(x=512)or(x=256)or(x=128)or(x=64)or(x=32) then
  begin
     b.Width:=b.Width-2;
     b.Height:=b.Height-1;
  end;
{$endif}
end;

procedure Tf_moon.SetOverlay(fn:string);
var b : Tbitmap;
    j : Tjpegimage;
begin
if fn='' then begin
  ClearOverlay;
  FOverlay:='';
  FOverlayTitle:='';
end else begin
  if not FileExists(slash(FOverlayPath)+fn) then raise Exception.Create('Overlay not found '+slash(FOverlayPath)+fn);
  FOverlay:=fn;
  FOverlayTitle:=ExtractFileNameWithoutExt(fn);
  if GLMaterialLibrary1.LibMaterialByName('O1')=nil then CreateMaterial(99);
  j:=TJpegImage.Create;
  b:=Tbitmap.Create;
  try
  j.LoadFromFile(slash(FOverlayPath)+fn);
  b.Assign(j);
  with GLMaterialLibrary1 do begin
      with LibMaterialByName('O1') do begin
        case FOverlayTransparencyMethode of
         0 : begin
             Material.BlendingMode:=bmModulate;
             Material.Texture.ImageAlpha:=tiaDefault;
             Material.FrontProperties.Diffuse.Alpha:=1;
             SetImgLum(b,trunc(200*FOverlayTransparency));
             end;
         1 : begin
             Material.BlendingMode:=bmTransparency;
             Material.Texture.ImageAlpha:=tiaOpaque;
             Material.FrontProperties.Diffuse.Alpha:=FOverlayTransparency;
             end;
         2 : begin
             Material.BlendingMode:=bmTransparency;
             Material.Texture.ImageAlpha:=tiaLuminanceSqrt;
             Material.FrontProperties.Diffuse.Alpha:=FOverlayTransparency;
             end;
        end;
        FixImgSize(b);
        Material.Texture.Image.Assign(b);
      end;
   end;
   GLSceneViewer1.Refresh;
except
  on E: Exception do begin
  {$ifdef trace_debug}
    debugln('Exception SetOverlay '+E.Message);
  {$endif}
  end;
end;
 j.free;
 b.free;
end;
end;

function Tf_moon.GetBumpMethod:TBumpMapCapability;
begin
  case GLBumpShader1.BumpMethod of
    bmDot3TexCombiner : result:=bcDot3TexCombiner;
    bmBasicARBFP : result:=bcBasicARBFP;
  end;
end;

procedure Tf_moon.SetBumpMethod(bm:TBumpMapCapability);
begin
  if bm in FBumpMapCapabilities then begin
    case bm of
      bcDot3TexCombiner : begin
          GLBumpShader1.BumpMethod:=bmDot3TexCombiner;
          if FBumpmap then GLLightSource1.ConstAttenuation:=0.3;
          end;
      bcBasicARBFP : begin
          GLBumpShader1.BumpMethod:=bmBasicARBFP;
          if FBumpmap then GLLightSource1.ConstAttenuation:=0.8;
          end;
    end;
  end;
end;

procedure Tf_moon.SetBumpMipmap(value: boolean);
begin
FBumpMipmap:=value;
if FBumpMipmap then begin
    BumpMaterialLibrary.Materials[0].Material.Texture.MagFilter:=maLinear;
    BumpMaterialLibrary.Materials[0].Material.Texture.MinFilter:=miLinearMipmapLinear;
    BumpMaterialLibrary.Materials[1].Material.Texture.MagFilter:=maLinear;
    BumpMaterialLibrary.Materials[1].Material.Texture.MinFilter:=miLinearMipmapLinear;
end else begin
    BumpMaterialLibrary.Materials[0].Material.Texture.MagFilter:=maNearest;
    BumpMaterialLibrary.Materials[0].Material.Texture.MinFilter:=miNearest;
    BumpMaterialLibrary.Materials[1].Material.Texture.MagFilter:=maNearest;
    BumpMaterialLibrary.Materials[1].Material.Texture.MinFilter:=miNearest;
end;
end;

procedure Tf_moon.SetBumpmap(value: boolean);
var i: integer;
    retry:boolean;
begin
if FBumpOk and (value<>FBumpmap) then begin
  FBumpmap:=value;
  if FBumpmap then begin
    if BumpMapLimit1K then
      i:=1
    else begin
      if ForceBumpMapSize=0 then
        i:=MaxTextureSize div 1024
      else
        i:=ForceBumpMapSize;
      if i>=8 then i:=8
      else if i>=4 then i:=4
      else if i>=2 then i:=2
      else i:=1;
    end;
    if assigned(FOnGetMsg) then FOnGetMsg(self,MsgOther,rsPhaseWithDyn+', '+rsm_57+blank+inttostr(i)+'k');
    repeat
      retry:=false;
      try
      BumpMaterialLibrary.Materials[0].Material.Texture.Image.LoadFromFile(slash(FBumpPath)+'normal'+inttostr(i)+'k.jpg');
      BumpMaterialLibrary.Materials[1].Material.Texture.Image.LoadFromFile(slash(FBumpPath)+'map'+inttostr(i)+'k.jpg');
      GLSphereMoon.Material.MaterialLibrary:=BumpMaterialLibrary;
      GLSphereMoon.Material.LibMaterialName:='Bump';
      GLSceneViewer1.Refresh;
      if not GLBumpShader1.Enabled then begin
         SetBumpmap(false);
         if assigned(FOnGetMsg) then FOnGetMsg(self,MsgOther,'Graphic card do not support the dynamic shadow.');
         exit;
      end;
      except
        on E: Exception do begin
        {$ifdef trace_debug}
          debugln('Exception SetBumpmap '+E.Message);
        {$endif}
        if i=1 then begin
          if assigned(FOnGetMsg) then FOnGetMsg(self,MsgOther,'Cannot load bumpmap');
          SetBumpmap(false);
          exit;
        end;
        i:=i div 2;
        retry:=true;
        if assigned(FOnGetMsg) then FOnGetMsg(self,MsgOther,'Bumpmap size reduced to '+inttostr(i)+'k');
        end;
     end;
    until not retry;
    if GLBumpShader1.BumpMethod=bmBasicARBFP then
       GLLightSource1.ConstAttenuation:=0.8
    else
       GLLightSource1.ConstAttenuation:=0.3;
    GLLightSource1.LightStyle:=lsSpot;
    ClearSlice(2);
    if i>7 then MaxZoom:=12
    else if i>3 then MaxZoom:=8
    else if i>1 then MaxZoom:=4
    else MaxZoom:=2;
    if GLCamera1.SceneScale>MaxZoom then SetZoomLevel(MaxZoom);
  end else begin
    if assigned(FOnGetMsg) then FOnGetMsg(self,MsgOther,'');
    LoadSlice(zone);
    GLSphereMoon.Material.MaterialLibrary:=GLMultiMaterialLibrary;
    GLSphereMoon.Material.LibMaterialName:='MultiMaterial';
    BumpMaterialLibrary.Materials[0].Material.Texture.Image.Assign(blankbmp);
    BumpMaterialLibrary.Materials[1].Material.Texture.Image.Assign(blankbmp);
    GLLightSource1.ConstAttenuation:=0.5;
    GLLightSource1.LightStyle:=lsParallel;
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
     GLLightSource1.Position.SetPoint(0,0,-100);
     GLLightSource1.SpotDirection.SetVector(GLLightSource1.Position.X,GLLightSource1.Position.Y,GLLightSource1.Position.Z);
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
var cl,cb: single;
begin
if value<>FVisibleSideLock then begin
   Screen2Moon(GLSceneViewer1.Width div 2,GLSceneViewer1.Height div 2,cl,cb);
   FVisibleSideLock:=value;
   if FVisibleSideLock then begin
      GLSphereMoon.TurnAngle:=0;
      GLCamera1.TargetObject:=nil;
      GLCamera1.Position.SetPoint(0,0,-100*FEarthDistance/MeanEarthDistance);
      GLAnnulus1.Position.Z:=GLCamera1.Position.Z+90;
      GLMirror1.Position.SetPoint(0,0,-100.01*FEarthDistance/MeanEarthDistance);
      GLCamera1.Direction.SetVector(0,0,1);
      CenterAt(cl,cb);
   end else begin
      Eyepiece:=0;
      FShowCCD:=false;
      GLCamera1.Position.SetPoint(0,0,-100);
      GLAnnulus1.Position.Z:=GLCamera1.Position.Z+90;
      GLMirror1.Position.SetPoint(0,0,-100.01);
      GLCamera1.TargetObject:=BaseCube;
      CenterAt(cl,cb);
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
v.V[0]:=xx;
v.V[1]:=yy;
v.V[2]:=zz;
v:=GLSceneViewer1.Buffer.WorldToScreen(v);
if (v.V[0]>=0)and(v.V[0]<=GLSceneViewer1.Width)and(v.V[1]>=0)and(v.V[1]<=GLSceneViewer1.Height) then begin
  if FMirror then begin
     x:=GLSceneViewer1.Width-round(v.V[0]);
     y:=round(v.V[1]);
  end
  else begin
    x:=round(v.V[0]);
    y:=GLSceneViewer1.Height-round(v.V[1]);
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
   result:=World2Moon(ipoint.V[0],ipoint.V[2],ipoint.V[1],lon,lat);
   if GLAnnulus1.Visible then begin
      xx:=ipoint.V[0]-GLCamera1.Position.X;
      yy:=ipoint.V[1]-GLCamera1.Position.Y;
      if sqrt(xx*xx+yy*yy)>(GLAnnulus1.BottomInnerRadius/abs(90/GLCamera1.Position.Z))
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
  v.V[0]:=xx;
  v.V[1]:=zz;
  v.V[2]:=yy;
  v:=GLSphereMoon.LocalToAbsolute(v);
  x:=v.V[0];
  y:=v.V[1];
  z:=v.V[2];
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
      lat:=la+LibrLat*cos(lo);
      lon:=lo-LibrLon+LibrLat*tan(lat)*sin(lo);
      result:=true;
   end else begin
      lat:=0;
      result:=false;
   end;
end;

Procedure Tf_moon.GetZoomInfo;
begin
  if assigned(FOnGetMsg)and(FRaCentre>-9999) then begin
    if RotationCadencer.Enabled then
       FOnGetMsg(self,MsgZoom,rsm_43+inttostr(round(GLCameraSatellite.GetFieldOfView(GLSceneViewer1.Width)/Fzoom))+ldeg+' '+rst_4+formatfloat('0.0',Fzoom)+'  '+rsLevel+inttostr(zone))
    else
       FOnGetMsg(self,MsgZoom,rsm_43+inttostr(round(60*0.119*GLCamera1.GetFieldOfView(GLSceneViewer1.Width)/Fzoom))+lmin+' '+rst_4+formatfloat('0.0',Fzoom)+'  '+rsLevel+inttostr(zone)+' '+FTexture[zone-1]+' '+FOverlayTitle);
  end;
end;

Procedure Tf_moon.SetZoomLevel(zoom:single);
var newzone: integer;
    satzone: single;
begin
if lock_Zoom then exit;
try
  lock_Zoom:=true;
  if RotationCadencer.Enabled then begin
    if zoom<2 then zoom:=2;
    if zoom>6 then zoom:=6;
  end else begin
    if zoom<1 then zoom:=1;
    if zoom>MaxZoom then zoom:=MaxZoom;
  end;
  newzone:=1;
  if RotationCadencer.Enabled then satzone:=1000/FSatAltitude
     else satzone:=1;
  if (zoom*satzone)>ZoomByZone[1] then begin
     if maxzone>=2 then newzone:=2;
     if (zoom*satzone)>ZoomByZone[2] then begin
       if maxzone>=3 then newzone:=3;
       if (zoom*satzone)>ZoomByZone[3] then begin
         if maxzone>=4 then newzone:=4;
         if (zoom*satzone)>ZoomByZone[4] then begin
           if maxzone>=5 then newzone:=5;
           if (zoom*satzone)>ZoomByZone[5] then begin
             if maxzone>=6 then newzone:=6;
           end;
         end;
       end;
     end;
  end;
  Fzoom:=zoom;
  GLCamera1.SceneScale:=Fzoom;
  if RotationCadencer.Enabled then
     GLCameraSatellite.SceneScale:=Fzoom;
  if (not FBumpmap)and(newzone<>zone)and(zone>1) then begin
     if newzone=1 then
       ClearSlice(2)
     else
       DisableSlice2;
  end;
  zone:=newzone;
  GetZoomInfo;
  RefreshAll;
except
  on E: Exception do begin
  {$ifdef trace_debug}
    debugln('Exception SetZoomLevel '+E.Message);
  {$endif}
  end;
end;
lock_Zoom:=false;
end;

procedure Tf_moon.SetTexture(lfn:TStringList);
begin
 if (lfn[0]<>'NONE')and(not DirectoryExists(slash(FTexturePath)+slash(lfn[0])+'L1')) then raise Exception.Create('Missing L1 slices for '+slash(FTexturePath)+lfn[0]);
 Ftexture:=lfn;
 maxzone:=1;
 if (lfn[1]='NONE')or DirectoryExists(slash(FTexturePath)+slash(Ftexture[1])+'L2') then begin
    maxzone:=2;
    if (lfn[2]='NONE')or DirectoryExists(slash(FTexturePath)+slash(Ftexture[2])+'L3') then begin
       maxzone:=3;
       if (lfn[3]='NONE')or DirectoryExists(slash(FTexturePath)+slash(Ftexture[3])+'L4') then begin
          maxzone:=4;
           if (lfn[4]='NONE')or DirectoryExists(slash(FTexturePath)+slash(Ftexture[4])+'L5') then begin
             maxzone:=5;
             if (lfn[5]='NONE')or DirectoryExists(slash(FTexturePath)+slash(Ftexture[5])+'L6') then begin
                maxzone:=6;
             end;
           end;
       end;
    end;
 end;
 if not FBumpmap then begin
   maxzoom:=ZoomByZone[maxzone];
   ClearSlice(2);
   LoadSlice(1);
   if zone<>1 then LoadSlice(zone);
   GLSceneViewer1.Refresh;
 end;
end;

procedure Tf_moon.SetBumpPath(fn:string);
begin
if not FileExists(slash(fn)+'normal2k.jpg') then raise Exception.Create('No bumpmap in '+fn);
FBumpPath:=fn;
end;

procedure Tf_moon.FormCreate(Sender: TObject);
var i: integer;
begin
 if Owner is TWinControl then Moon.Parent:=TWinControl(Owner);
 vIgnoreOpenGLErrors:=true;
 blankbmp:=Tbitmap.Create;
 blankbmp.Width:=4;
 blankbmp.Height:=4;
 blankbmp.Canvas.brush.Color:=clWhite;
 blankbmp.Canvas.pen.Color:=blankbmp.Canvas.brush.Color;
 blankbmp.Canvas.FillRect(0,0,4,4);
 blankjp:=TJPEGImage.Create;
 blankjp.Width:=1024;
 blankjp.Height:=1024;
 blankjp.Canvas.brush.Color:=TColor($FCFCFC);
 blankjp.Canvas.pen.Color:=blankjp.Canvas.brush.Color;
 blankjp.Canvas.FillRect(0,0,1024,1024);
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
 FOverlay:='';
 FRotation:=0;
 FRaCentre:=-9999;
 FEarthDistance:=MeanEarthDistance;
 FSatAltitude:=800;
 FSatViewDistance:=1;
 FSatViewDistance:=2;
 FSatModel:='';
 FBumpOk:=false;
 BumpMapLimit1K:=false;
 ForceBumpMapSize:=0;
 TextureCompression:=true;
 MaxZoom:=3;
 MaxTextureSize:=1024;
 Flabelcolor:=clWhite;
 FMeasuringDistance := False;
 GLLightSource2.Diffuse.AsWinColor :=$272727;
 GLLightSource1.Ambient.AsWinColor :=0;
 GLLightSource1.Diffuse.AsWinColor :=$FFFFFF;
 GLLightSource1.Specular.AsWinColor:=$636363;
 FShowScale:=false;
 FShowGrid:=false;
 FShowCCD:=false;
 FCCDw:=0;
 FCCDh:=0;
 curlabel:=0;
 moveok:=false;
end;

procedure Tf_moon.Init(check:boolean=true);
const nRestricted=1;
      RestrictedDrivers: array[1..nRestricted] of string =('INTEL 945');
var   CurentDriver:string;
      Restricted: boolean;
      i: integer;
begin
{$ifdef trace_debug}
 debugln('Init OpenGL');
{$endif}
try
if check then begin
// Check Acceleration
{$ifdef trace_debug}
 debugln('Check Acceleration');
{$endif}
if GLSceneViewer1.Buffer.Acceleration=chaSoftware then begin
   ShowMessage('Warning! OpenGL hardware acceleration not detected, program performance can be very poor or the program may crash. Please install a graphic card and driver that support OpenGL acceleration.');
end;
// Check texture size
{$ifdef trace_debug}
 debugln('Check texture size');
{$endif}
try
  GLSceneViewer1.Buffer.RenderingContext.Activate;
  MaxTextureSize:=Glsceneviewer1.Buffer.LimitOf[limTextureSize];
  {$ifdef trace_debug}
   debugln('Texture max: '+inttostr(MaxTextureSize));
  {$endif}
except
  MaxTextureSize:=1024;
  {$ifdef trace_debug}
   debugln('Failed to get texture max. size from driver.');
   debugln('Using default value: '+inttostr(MaxTextureSize));
  {$endif}
end;
if MaxTextureSize=0 then MaxTextureSize:=1024;
if MaxTextureSize<1024 then begin
   raise exception.Create('Graphic card not supported! Texture max size:'+inttostr(MaxTextureSize)+'. Must be at least 1024.');
   halt;
end;
// Check Bumpmap
{$ifdef trace_debug}
 debugln('Check Bumpmap');
{$endif}
FBumpMapCapabilities:=[];
try
CurentDriver:=uppercase(trim(StrPas(PChar(CurrentGLContext.GL.GetString(GL_VENDOR)))+' '+StrPas(PChar(CurrentGLContext.GL.GetString(GL_RENDERER))) ));
{$ifdef trace_debug}
 debugln('Driver: '+CurentDriver);
{$endif}
Restricted:=false;
for i:=1 to nRestricted do begin
  if pos(RestrictedDrivers[i],CurentDriver)>0 then
     Restricted:=true;
end;
if Restricted and (ForceBumpMapSize=0)
  then begin
   {$ifdef trace_debug}
     debugln('Restricted driver found!');
   {$endif}
    BumpMapLimit1K:=true;
    if  CurrentGLContext.GL.ARB_multitexture
    and CurrentGLContext.GL.ARB_texture_env_dot3 then begin
        {$ifdef trace_debug}
         debugln('GL_ARB_texture_env_dot3 OK');
        {$endif}
         FBumpMapCapabilities:=[bcDot3TexCombiner];
         FBumpOk:=true;
    end;
end else begin
    if  CurrentGLContext.GL.ARB_multitexture
    and CurrentGLContext.GL.ARB_vertex_program then begin
      if CurrentGLContext.GL.ARB_texture_env_dot3 then begin
        {$ifdef trace_debug}
         debugln('GL_ARB_texture_env_dot3 OK');
        {$endif}
         FBumpMapCapabilities:=FBumpMapCapabilities+[bcDot3TexCombiner];
         FBumpOk:=true;
      end;
      if CurrentGLContext.GL.ARB_fragment_program then begin
        {$ifdef trace_debug}
         debugln('GL_ARB_fragment_program OK');
        {$endif}
         FBumpMapCapabilities:=FBumpMapCapabilities+[bcBasicARBFP];
         FBumpOk:=true;
      end;
    end;
end;
{$ifdef trace_debug}
 debugln('SetBumpMethod');
{$endif}
if bcDot3TexCombiner in FBumpMapCapabilities then
   SetBumpMethod(bcDot3TexCombiner)
else if bcBasicARBFP in FBumpMapCapabilities then
   SetBumpMethod(bcBasicARBFP);
except
  FBumpOk:=false;
  {$ifdef trace_debug}
   debugln('Failed to initialize bumpmap shader');
  {$endif}
end;
// Initialisation
{$ifdef trace_debug}
 debugln('Check MultiTexture');
{$endif}
try
 FAsMultiTexture := CurrentGLContext.GL.ARB_multitexture and (GLSceneViewer1.Buffer.LimitOf[limNbTextureUnits] > 1);
except
 FAsMultiTexture := false;
 {$ifdef trace_debug}
  debugln('Failed to get multitexture capability');
  debugln('Overlay function is disabled');
 {$endif}
end;
end;
{$ifdef trace_debug}
 debugln('InitLabel');
{$endif}
InitLabel;
{$ifdef trace_debug}
 debugln('InitSprite');
{$endif}
InitSprite;
except
  on E: Exception do begin
  {$ifdef trace_debug}
   debugln('Could not initialize OpenGL: '+E.Message);
  {$endif}
   MessageDlg('Could not initialize OpenGL: '+E.Message, mtError, [mbClose], 0);
   halt;
  end;
end;
{$ifdef trace_debug}
 debugln('Init OpenGL OK');
{$endif}
end;

procedure Tf_moon.FormDestroy(Sender: TObject);
begin
ClearLabel;
if LabelGroup<>nil then LabelGroup.DeleteChildren;
if GLDummyCubeMarks<>nil then GLDummyCubeMarks.DeleteChildren;
if GLDummyCubeCoord<>nil then GLDummyCubeCoord.DeleteChildren;
blankbmp.Free;
blankjp.Free;
GLSceneViewer1.Buffer.DestroyRC;
GLBitmapFont1.Ranges.Clear;
end;


procedure Tf_moon.AssignMoon(Source: TF_moon);
begin
 MaxTextureSize:=Source.MaxTextureSize;
 BumpMapLimit1K:=Source.BumpMapLimit1K;
 FBumpMapCapabilities:=Source.FBumpMapCapabilities;
 BumpMipmap:=Source.BumpMipmap;
// FBumpOk:=false;
 FBumpOk:=Source.FBumpOk;
 FAsMultiTexture:=Source.FAsMultiTexture;
 TexturePath:=Source.TexturePath;
 OverlayPath:=Source.OverlayPath;
 if CanBump then BumpPath:=Source.BumpPath;
 TextureCompression:=Source.TextureCompression;
 if Texture<>Source.Texture then
    Texture :=Source.Texture;
 if Overlay<>Source.Overlay then
    Overlay :=Source.Overlay;
// if CanBump then Bumpmap :=Source.Bumpmap;
 Bumpmap := false;   // no bumpmap by default on second copy
 ShowPhase :=Source.ShowPhase;
 VisibleSideLock :=Source.VisibleSideLock;
 Mirror :=Source.Mirror;
 Orientation:=Source.Orientation;
 Poleorientation   := Source.Poleorientation;
 FollowNorth:=Source.FollowNorth;
 ZenithOnTop:=Source.ZenithOnTop;
 LabelFont :=Source.LabelFont;
 LabelColor :=Source.LabelColor;
 LibrationMark:=Source.LibrationMark;
 SatelliteRotation :=Source.SatelliteRotation;
 SatelliteAltitude:= Source.SatelliteAltitude;
 Phase :=Source.Phase;
 SunIncl :=Source.SunIncl;
 LibrLon :=Source.LibrLon;
 LibrLat :=Source.LibrLat;
 EarthDistance:=Source.EarthDistance;
 Zoom :=Source.Zoom;
 Eyepiece :=Source.Eyepiece;
 AmbientColor:=Source.AmbientColor;
 DiffuseColor:=Source.DiffuseColor;
 SpecularColor:=Source.SpecularColor;
 ShowScale := Source.ShowScale;
 ShowGrid := Source.ShowGrid;
 GridSpacing := Source.GridSpacing;
 RaCentre:=Source.RaCentre;
 DeCentre:=Source.DeCentre;
 JD:=Source.JD;
 GLSceneViewer1.Cursor := Source.GLSceneViewer1.Cursor;
 GLSphereMoon.Slices := Source.GLSphereMoon.Slices;
 GLSphereMoon.Stacks := Source.GLSphereMoon.Stacks;
 LibrationDummyCube.PitchAngle  := Source.LibrationDummyCube.PitchAngle;
 LibrationDummyCube.TurnAngle  := Source.LibrationDummyCube.TurnAngle;
 LibrationDummyCube.up  := Source.LibrationDummyCube.Up;
 GLMirror1.Position   := Source.GLMirror1.Position;
 GLMirror1.Visible  := Source.GLMirror1.Visible;
 GLCamera1.TargetObject  := Source.GLCamera1.TargetObject;
 GLCamera1.Position  := Source.GLCamera1.Position;
 GLCamera1.Direction  := Source.GLCamera1.Direction;
 GLCamera1.SceneScale  := Source.GLCamera1.SceneScale;
 GLCamera1.Up  := Source.GLCamera1.Up;
 GLLightSource1.ConstAttenuation  := Source.GLLightSource1.ConstAttenuation;
 GLLightSource1.LightStyle  := Source.GLLightSource1.LightStyle;
 GLLightSource1.Position  := Source.GLLightSource1.Position;
 GLLightSource1.SpotDirection  := Source.GLLightSource1.SpotDirection;
 GLSphereMoon.PitchAngle  := Source.GLSphereMoon.PitchAngle;
 GLSphereMoon.TurnAngle   := Source.GLSphereMoon.TurnAngle;
 GLSphereMoon.up  := Source.GLSphereMoon.Up;
 GLAnnulus1.Visible  := Source.GLAnnulus1.Visible;
 GLAnnulus1.Position  := Source.GLAnnulus1.Position;
 GLAnnulus1.BottomInnerRadius   := Source.GLAnnulus1.BottomInnerRadius;
 if Source.Bumpmap then begin   // no bumpmap by default on second copy
    LoadSlice(zone);
    GLSphereMoon.Material.MaterialLibrary:=GLMultiMaterialLibrary;
    GLSphereMoon.Material.LibMaterialName:='MultiMaterial';
    BumpMaterialLibrary.Materials[0].Material.Texture.Image.Assign(blankbmp);
    BumpMaterialLibrary.Materials[1].Material.Texture.Image.Assign(blankbmp);
    GLLightSource1.ConstAttenuation:=0.5;
    GLLightSource1.LightStyle:=lsParallel;
    maxzoom:=ZoomByZone[maxzone];
  end;
end;

procedure Tf_moon.MoonResize(Sender: TObject);
begin
  RefreshAll;
end;

procedure Tf_moon.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var z,s1,c1: single;
    xx:integer;
    Pt: TPoint;
begin
  DownShift:=Shift;
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
  lastyzoom:=y;
  moveok:=true;
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
    if (ssLeft in shift)
     then begin
        SkipIdent:=false;
     end;
    // Popup
    if (ssRight in shift)
     then begin
        GLSceneViewer1.PopupMenu.Parent:=self;
        Pt:=GLSceneViewer1.ClientToScreen(point(x,y));
        GLSceneViewer1.PopupMenu.PopUp(Pt.X+1,Pt.Y+1);
     end;
  end;
end;

procedure Tf_moon.MoveMoonAround(anObject: TGLBaseSceneObject; pitchDelta, turnDelta: Single);
//procedure TGLBaseSceneObject.MoveObjectAround(anObject: TGLBaseSceneObject; pitchDelta, turnDelta: Single);
// Copied here to change pitchNow ClampValue limits
// Original only allow to go to 1.5 degree from the pole
var
  originalT2C, normalT2C, normalCameraRight, newPos: TVector;
  pitchNow, dist: Single;
begin
  if Assigned(anObject) then
  begin
    // normalT2C points away from the direction the camera is looking
    originalT2C := VectorSubtract(GLCamera1.AbsolutePosition,
      anObject.AbsolutePosition);
    SetVector(normalT2C, originalT2C);
    dist := VectorLength(normalT2C);
    NormalizeVector(normalT2C);
    // normalRight points to the camera's right
    // the camera is pitching around this axis.
    normalCameraRight := VectorCrossProduct(GLCamera1.AbsoluteUp, normalT2C);
    if VectorLength(normalCameraRight) < 0.001 then
      SetVector(normalCameraRight, XVector) // arbitrary vector
    else
      NormalizeVector(normalCameraRight);
    // calculate the current pitch.
    // 0 is looking down and PI is looking up
    pitchNow := ArcCos(VectorDotProduct(GLCamera1.AbsoluteUp, normalT2C));
    pitchNow := ClampValue(pitchNow + DegToRad(pitchDelta), 0 + 0.0025, PI -
      0.0025);
    // create a new vector pointing up and then rotate it down
    // into the new position
    SetVector(normalT2C, GLCamera1.AbsoluteUp);
    RotateVector(normalT2C, normalCameraRight, -pitchNow);
    RotateVector(normalT2C, GLCamera1.AbsoluteUp, -DegToRad(turnDelta));
    ScaleVector(normalT2C, dist);
    newPos := VectorAdd(GLCamera1.AbsolutePosition, VectorSubtract(normalT2C,
      originalT2C));
    if Assigned(GLCamera1.Parent) then
      newPos := GLCamera1.Parent.AbsoluteToLocal(newPos);
    GLCamera1.Position.AsVector := newPos;
  end;
end;

procedure Tf_moon.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var movespeed,s1,c1,zm: single;
    xx,yy:integer;
    lat,lon: single;
    OnMoon: boolean;
begin
  // Cord. display
  if (not moveok)or(shift=[]) then begin
     if Assigned(onMoonMove) then begin
        OnMoon:=Screen2Moon(x,y,lon,lat);
        onMoonMove(Self,X,Y,OnMoon,lon,lat);
     end;
  end
  else
  // Distance
  if FMeasuringDistance and distancestart then
     begin
       MeasureDistance(x, y);
//       ShowCoordinates(x, y);
     end
  else
  // Move map
  if (ssLeft in shift)
   then begin
    if FMirror then x:=GLSceneViewer1.Width-x;
    if FVisibleSideLock then begin
      sincos(deg2rad * FOrientation, s1, c1);
      xx:=round(x*c1+y*s1);
      yy:=round(y*c1-x*s1);
      if SkipIdent or (abs(mx-xx)>2) or (abs(my-yy)>2) then begin
        movespeed:=0.002/GLCamera1.SceneScale;
        GLCamera1.Position.X:=GLCamera1.Position.X-(mx-xx)*movespeed;
        GLCamera1.Position.Y:=GLCamera1.Position.Y-(my-yy)*movespeed;
        GLAnnulus1.Position.x := GLCamera1.Position.x;
        GLAnnulus1.Position.y := GLCamera1.Position.y;
        mx:=xx;
        my:=yy;
        SkipIdent:=true;
        GLSceneViewer1.Cursor:=crHandPoint;
      end;
    end else begin
      if SkipIdent or (abs(mx-x)>2) or (abs(my-y)>2) then begin
        if RotationCadencer.Enabled then begin
          movespeed:=0.3/GLCamera1.SceneScale;
          GLCameraSatellite.MoveAroundTarget((y-my)*movespeed,(x-mx)*movespeed);
        end else begin
          movespeed:=0.3/GLCamera1.SceneScale;
          MoveMoonAround(GLCamera1.TargetObject,(my-y)*movespeed,(mx-x)*movespeed);
          //GLCamera1.MoveAroundTarget((my-y)*movespeed,(mx-x)*movespeed);
        end;
        mx:=x;
        my:=y;
        SkipIdent:=true;
        GLSceneViewer1.Cursor:=crHandPoint;
      end;
    end;
    if (not FShowPhase)and(not RotationCadencer.Enabled) then begin
       GLLightSource1.Position:=GLCamera1.Position;
       GLLightSource1.SpotDirection.SetVector(GLLightSource1.Position.X,GLLightSource1.Position.Y,GLLightSource1.Position.Z);
    end;
    RefreshAll;
  end
  else
  // Zoom
  if (ssMiddle in shift)
   then begin
     zm:=FZoom*(1-(y-lastyzoom)/200);
     lastyzoom:=y;
     SetZoomLevel(zm);
  end;
end;

procedure Tf_moon.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var lat,lon: single;
    OnMoon: boolean;
    Pt: TPoint;
begin
  moveok:=false;
  // Distance
  if measuringdistance and distancestart then
  begin
    MeasureDistance(x, y);
    distancestart := False;
  end
  else begin
    // Identification
    if (ssLeft in DownShift) and (not SkipIdent)
    then begin
       OnMoon:=Screen2Moon(x,y,lon,lat);
       if Assigned(onMoonClick) then onMoonClick(Self,Button,DownShift,X,Y,OnMoon,lon,lat);
    end;
    // Popup
    if (ssRight in DownShift)
     then begin
        if Assigned(onMoonClick) then onMoonClick(Self,Button,DownShift,X,Y,OnMoon,lon,lat);
        GLSceneViewer1.PopupMenu.parent:=self;
        Pt:=GLSceneViewer1.ClientToScreen(point(x,y));
        GLSceneViewer1.PopupMenu.PopUp(Pt.X+1,Pt.Y+1);
     end;
    GLSceneViewer1.Cursor:=crRetic;
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

procedure Tf_moon.RefreshAll;
begin
  ClearLabel;
 // if marked then SetMark(markl,markb,marktext);
  ShowLibrationMark;
//  SetScale;
  SetCCDfield;
  RefreshTimer.Enabled:=false;
  RefreshTimer.Enabled:=true;
end;

procedure Tf_moon.RefreshTimerTimer(Sender: TObject);
begin
  RefreshTimer.Enabled:=false;
  SetScale;
//  SetCCDfield;
  if marked then SetMark(markl,markb,marktext);
  if not RotationCadencer.Enabled then begin
    ShowLibrationMark;
    if assigned(FOnGetSprite) then FOnGetSprite(self);
    if assigned(FOnGetLabel) then FOnGetLabel(self);
    if zone>1 then LoadSlice(zone);
    GetZoomInfo;
  end;
end;

procedure Tf_moon.SetRotation(value:single);
var changestate:boolean;
begin
 if FRotation=0 then begin
   GetCenter(satl,satb);
   satli:=satl;
   satlc:=0;
 end;
 FRotation:=value;
 changestate:=RotationCadencer.Enabled<>(FRotation<>0);
 RotationCadencer.Enabled:=(FRotation<>0);
 if RotationCadencer.Enabled then begin
   if changestate then begin
     SaveShowScale:=FShowScale;
     FShowScale:=false;
     SetScale;
   end;
   GLSceneViewer1.Camera:=GLCameraSatellite;
   if FSatModel<>'' then GLFreeFormSatelite.Visible:=true;
   SetZoomLevel(Fzoom);
 end else begin
   GLSceneViewer1.Camera:=GLCamera1;
   GLFreeFormSatelite.Visible:=false;
   if (not FShowPhase) then begin
     GLLightSource1.Position:=GLCamera1.Position;
     GLLightSource1.SpotDirection.SetVector(GLLightSource1.Position.X,GLLightSource1.Position.Y,GLLightSource1.Position.Z);
   end;
   SetZoomLevel(Fzoom);
   FShowScale := SaveShowScale;
   SetScale;
 end;
end;

procedure Tf_moon.SetSatAltitude(value:single);
begin
  FSatAltitude:=value;
  SetZoomLevel(Fzoom);
end;

procedure Tf_moon.SetSatInclination(value:single);
begin
  FSatInclination:=deg2rad*value;
end;

function Tf_moon.GetSatInclination:single;
begin
  result:=rad2deg*FSatInclination;
end;

procedure Tf_moon.SetSatViewDistance(value:single);
var s:single;
begin
  FSatViewDistance:=value;
  s:=FSatModelScale/FSatViewDistance;
  GLFreeFormSatelite.Scale.SetVector(s,s,s);
  GLFreeFormSatelite.Position.X:=FSatPosX/FSatViewDistance;
  GLFreeFormSatelite.Position.Y:=FSatPosY/FSatViewDistance;
  GLFreeFormSatelite.Position.Z:=FSatPosZ/FSatViewDistance;
end;

procedure Tf_moon.SetSatModelScale(value:single);
begin
  FSatModelScale:=value;
  SetSatViewDistance(FSatViewDistance);
end;

procedure Tf_moon.SatDirection(x,y,z:single);
begin
  GLFreeFormSatelite.Direction.X:=x;
  GLFreeFormSatelite.Direction.Y:=Y;
  GLFreeFormSatelite.Direction.Z:=z;
end;

procedure Tf_moon.SatUp(x,y,z:single);
begin
  GLFreeFormSatelite.Up.X:=x;
  GLFreeFormSatelite.Up.Y:=Y;
  GLFreeFormSatelite.Up.Z:=z;
end;

procedure Tf_moon.SatPos(x,y,z:single);
begin
FSatPosX:=x;
FSatPosY:=y;
FSatPosZ:=z;
SetSatViewDistance(FSatViewDistance);
end;

procedure Tf_moon.SetSatModel(value:string);
begin
if value<>FSatModel then begin
   FSatModel:=value;
   GLFreeFormSatelite.ResetRotations;
   if FSatModel<>'' then
      GLFreeFormSatelite.LoadFromFile(FSatModel);
   if RotationCadencer.Enabled then begin
      if FSatModel<>'' then GLFreeFormSatelite.Visible:=true
                       else GLFreeFormSatelite.Visible:=false;
   end;
end;
end;

procedure Tf_moon.RotationCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var cl,sl,cb,sb,ci,si,cc,sc,x,y,z: single;
    v,dv,nv: TVector;
begin
  // new position after deltatime
  satlc:=satlc+deg2rad*FRotation*deltaTime;
  satr:=0.5*(Rmoon+FSatAltitude)/Rmoon;
  // convert to long, lat
  sincos(FSatInclination,si,ci);
  sincos(satlc,sc,cc);
  satl:=arctan2(sc*ci,cc)+satli;
  satb:=arcsin(si*sc);
  // convert to cartesian
  sincos(-satl-pi/2,sl,cl);
  sincos(satb,sb,cb);
  x:=satr*cb*cl;
  y:=satr*cb*sl;
  z:=satr*sb;
  v:=VectorMake(x,z,y);
  // move satellite
  dv:=VectorSubtract(v,GLDummyCubeSatellite.AbsolutePosition);
  GLDummyCubeSatellite.Position.Translate(dv);
  // orient satellite
  NormalizeVector(v);
  nv:=VectorNegate(v);
  GLDummyCubeSatellite.ResetRotations;
  GLDummyCubeSatellite.Direction.SetVector(nv);
  // orient camera
  if (abs(GLCameraSatellite.Position.X)>0.001)or(abs(GLCameraSatellite.Position.Y)>0.001) then
     GLCameraSatellite.Up.SetVector(v);
  if not FShowPhase then begin
    GLLightSource1.Position:=GLDummyCubeSatellite.Position;
    GLLightSource1.SpotDirection.SetVector(GLLightSource1.Position.X,GLLightSource1.Position.Y,GLLightSource1.Position.Z);
  end;
  SetMark(satl,satb,' ');
RefreshAll;
if zone>1 then LoadSlice(zone);
end;

procedure Tf_moon.SatCenter;
begin
  GLCameraSatellite.Position.x:=0;
  GLCameraSatellite.Position.y:=0;
  GLCameraSatellite.Position.z:=-0.01;
  GLCameraSatellite.ResetRotations;
  GLCameraSatellite.Up.SetVector(0,1,0);
  SetZoomLevel(2);
end;

procedure Tf_moon.SatEast;
begin
  GLCameraSatellite.Position.x:=0;
  GLCameraSatellite.Position.y:=0;
  GLCameraSatellite.Position.z:=-0.01;
  GLCameraSatellite.ResetRotations;
  GLCameraSatellite.MoveAroundTarget(0,-(35+(800-FSatAltitude)/40));
  SetZoomLevel(4);
end;

procedure Tf_moon.SatWest;
begin
  GLCameraSatellite.Position.x:=0;
  GLCameraSatellite.Position.y:=0;
  GLCameraSatellite.Position.z:=-0.01;
  GLCameraSatellite.ResetRotations;
  GLCameraSatellite.MoveAroundTarget(0,(35+(800-FSatAltitude)/40));
  SetZoomLevel(4);
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
  GLLightSource1.Position.SetPoint(-LightDist * s,-LightDist * tan(-Fsunincl),-LightDist * c);
  GLLightSource1.SpotDirection.SetVector(GLLightSource1.Position.X,GLLightSource1.Position.Y,GLLightSource1.Position.Z);
GLLightSource1.EndUpdate;
GLScene1.EndUpdate;
end;

procedure Tf_moon.SetPhase(value:single);   // 0 -> full moon
begin
FPhase:=value;
if FShowPhase then OrientLightSource;
end;

procedure Tf_moon.SetSunIncl(value:single);
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
    LibrationDummyCube.PitchAngle := rad2deg*FLibrLat;
    LibrationDummyCube.TurnAngle := rad2deg*FLibrLon;
    LibrationDummyCube.up.x := 0;
  end else begin
    GLSphereMoon.PitchAngle := rad2deg*FLibrLat;
    GLSphereMoon.TurnAngle := rad2deg*FLibrLon;
    GLSphereMoon.up.x := 0;
  end;
  GLScene1.EndUpdate;
end;

procedure Tf_moon.SetLibrLon(value:single);
begin
FLibrLon:=value;
OrientMoon;
end;

procedure Tf_moon.SetLibrLat(value:single);
begin
FLibrLat:=value;
OrientMoon;
end;

procedure Tf_moon.SetEarthDistance(value:single);
begin
FEarthDistance:=value;
if VisibleSideLock then begin
  GLCamera1.Position.Z:=-100*FEarthDistance/MeanEarthDistance;
  GLAnnulus1.Position.Z:=GLCamera1.Position.Z+90;
  GLMirror1.Position.SetPoint(0,0,-100.01*FEarthDistance/MeanEarthDistance);
end;
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
    newSprite.Material.FrontProperties.Emission.AsWinColor := SpriteColor;
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
LabelGroup.DeleteChildren;
curlabel:=0;
for i:=0 to 2*Maxlabel do begin
    newlabel:=TGLHUDText(LabelGroup.AddNewChild(TGLHUDText));
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
if (LabelGroup.Count>0) and (LabelGroup.Children[0].visible or LabelGroup.Children[2].visible)then
   for i:=0 to 2*Maxlabel do
      with LabelGroup.Children[i] as TGLHUDText do visible:=false;
if (GLDummyCubeMarks.Count > 0) and (GLDummyCubeMarks.Children[0].Visible or GLDummyCubeMarks.Children[1].Visible)then
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
        Position.SetPoint(x,y,0);
      end;
      Inc(cursprite);
      if (cursprite>=MaxSprite)and(cursprite<AbsoluteMaxSprite) then MoreSprite;
  end;
end;

function Tf_moon.AddLabel(lon,lat:single; txt:string; notcenter:boolean):boolean;
var x,y: integer;
    vis: boolean;
begin
result:=false;
if curlabel>=MaxLabel then exit;
vis:=not (marked and (marktext=txt));
if not Moon2Screen(lon,lat,x,y) then exit;
if (x > 0) and (y > 0) and (x < GLSceneViewer1.Width) and
  (y < GLSceneViewer1.Height) and ((currenteyepiece = 0) or
  (sqrt(Intpower(GLSceneViewer1.Width / 2 - x, 2) + Intpower(
  GLSceneViewer1.Height / 2 - y, 2)) < 0.475 * GLSceneViewer1.Width))
  then begin
    with LabelGroup.Children[2*curlabel] as TGLHUDText do begin
      Position.SetPoint(x+ShadowOffset,y+ShadowOffset,0);
      if labelcenter and (not notcenter) then
        begin
          Text      := txt;
          Alignment := taCenter;
        end
        else
        begin
          Text      := '-' + txt;
          Alignment := taLeftJustify;
      end;
      Visible:=vis;
    end;
    with LabelGroup.Children[2*curlabel+1] as TGLHUDText do begin
      Position.SetPoint(x,y,0);
      if labelcenter and (not notcenter) then
        begin
          Text      := txt;
          Alignment := taCenter;
        end
        else
        begin
          Text      := '-' + txt;
          Alignment := taLeftJustify;
      end;
      Visible:=vis;
    end;
    inc(curlabel);
end;
end;

procedure Tf_moon.SetShowGrid(value:boolean);
begin
FShowGrid:=value;
GLDummyCubeCoord.Visible:=FShowGrid;
end;

procedure Tf_moon.SetGridSpacing(value:integer);
const   qr=0.5005;
var 	i,j,k,nl,nb,nk : Integer;
        l,b,x,y,z,space: single;
	newline : TGLLines;
procedure coords;
var cl,sl,cb,sb: single;
begin
  sincos(-l-pi/2,sl,cl);
  sincos(b,sb,cb);
  x:=qr*cb*cl;
  y:=qr*cb*sl;
  z:=qr*sb;
end;
begin
if value>30 then value:=30;
if value<1 then value:=1;
FGridSpacing:=value;
if FGridSpacing<=5 then
   nk:=1
else  if FGridSpacing<=15 then
   nk:=3
else
   nk:=6;
space:=FGridSpacing/nk;
GLDummyCubeCoord.DeleteChildren;
nl:=360 div FGridSpacing;
nb:=(180 div FGridSpacing);
for i:=0 to nl do begin
    newline:=TGLLines(GLDummyCubeCoord.AddNewChild(TGLLines));
    newline.SplineMode:=lsmCubicSpline;
    newline.ShowAxes:=false;
    newline.AntiAliased:=true;
    newline.LineColor.AsWinColor:=clWhite;
    newline.NodesAspect:=lnaInvisible;
    newline.Visible:=true;
    l:=deg2rad*i*FGridSpacing;
    for j:=0 to nb do begin
       for k:=1 to nk do begin
         b:=deg2rad* ((j-1)*nk+k)*space  -pi/2;
         if (pi/2-abs(b))>0.05 then begin
           coords;
           newline.Nodes.AddNode(x,z,y);
         end;
       end;
    end;
end;
for i:=1 to nb do begin
    newline:=TGLLines(GLDummyCubeCoord.AddNewChild(TGLLines));
    newline.SplineMode:=lsmCubicSpline;
    newline.ShowAxes:=false;
    newline.AntiAliased:=true;
    newline.LineColor.AsWinColor:=clWhite;
    newline.NodesAspect:=lnaInvisible;
    newline.Visible:=true;
    b:=deg2rad*i*FGridSpacing-pi/2;
    for j:=0 to nl do begin
       for k:=1 to nk do begin
         l:=deg2rad* ((j-1)*nk+k)*space;
         coords;
         newline.Nodes.AddNode(x,z,y);
       end;
    end;
end;
end;

function Tf_moon.GetCurrentName : string;
begin
if marked then result:=marktext
          else result:='';
end;

procedure Tf_moon.SetMark(lon,lat:single; txt:string);
var x,y: integer;
    buf:string;
    labvis: boolean;
begin
if txt='' then begin
  marked:=false;
  GLHUDSpriteMark.Visible:=false;
  GLHUDTextMark.Visible:=false;
  GLHUDTextMarkShadow.Visible:=false;
end
else if Moon2Screen(lon,lat,x,y) then begin
  marked:=true;
  marktext:=txt;
  markl:=lon;
  markb:=lat;
  if showmark then
    begin
      GLHUDSpriteMark.BeginUpdate;
      GLHUDSpriteMark.Position.SetPoint(x,y,0);
      GLHUDSpriteMark.Width:=marksize;
      GLHUDSpriteMark.Height:=marksize;
      GLHUDSpriteMark.Material.FrontProperties.Emission.AsWinColor := MarkColor;
      GLHUDSpriteMark.Visible:=true;
      GLHUDSpriteMark.EndUpdate;
    end else
      GLHUDSpriteMark.Visible:=false;
  if showlabel then
    begin
      GLHUDTextMark.BeginUpdate;
      GLHUDTextMarkShadow.BeginUpdate;
      if x < (glsceneviewer1.Width div 2) then
      begin
        x:=x + 4 ;
        GLHUDTextMarkShadow.Position.SetPoint(x+1,y+1,0);
        GLHUDTextMark.Position.SetPoint(x,y,0);
        GLHUDTextMarkShadow.Alignment  := taLeftJustify;
        GLHUDTextMark.Alignment  := taLeftJustify;
      end else begin
        x:=x - 4 ;
        GLHUDTextMarkShadow.Position.SetPoint(x+1,y+1,0);
        GLHUDTextMark.Position.SetPoint(x,y,0);
        GLHUDTextMarkShadow.Alignment  := taRightJustify;
        GLHUDTextMark.Alignment  := taRightJustify;
      end;
      GLHUDTextMarkShadow.Text:=txt;
      GLHUDTextMark.Text:=txt;
      GLHUDTextMark.ModulateColor.AsWinColor := marklabelcolor;
      GLHUDTextMarkShadow.Visible:=true;
      GLHUDTextMark.Visible:=true;
      GLHUDTextMark.EndUpdate;
      GLHUDTextMarkShadow.EndUpdate;
    end else begin
      GLHUDTextMarkShadow.Visible:=false;
      GLHUDTextMark.Visible:=false;
    end;
end else begin
  marked:=true;
  marktext:=txt;
  markl:=lon;
  markb:=lat;
end;
for x:=0 to curlabel do begin
  if TGLHUDText(LabelGroup.Children[2*x]).Visible then begin
    buf:=TGLHUDText(LabelGroup.Children[2*x]).Text;
    if copy(buf,1,1)='-' then Delete(buf,1,1);
    labvis:=(buf<>marktext);
    TGLHUDText(LabelGroup.Children[2*x]).Visible:=labvis;
    TGLHUDText(LabelGroup.Children[2*x+1]).Visible:=labvis;
  end;
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
if lon>10000 then lon:=-FLibrLon;
if lat>10000 then lat:=FLibrLat;
if VisibleSideLock then begin
  if Moon2World(lon,lat,x,y,z) then begin
     GLCamera1.Position.X:=x;
     GLCamera1.Position.Y:=y;
     GLAnnulus1.Position.x := GLCamera1.Position.x;
     GLAnnulus1.Position.y := GLCamera1.Position.y;
  end;
end else begin
  sincos(lon+FLibrLon,sl,cl);
  sincos(lat-FLibrLat,sb,cb);
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
  a, x: double;
begin
  with GLArrowline1 do
  if FLibrationMark and VisibleSideLock then
    begin
      Visible := True;
      a      := arctan2(FLibrLat, FLibrLon);
      x      := sqrt(FLibrLat * FLibrLat + FLibrLon * FLibrLon);
      Height := x * 0.3 / zoom;
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
     GLAnnulus1.BottomInnerRadius:=GLSphereMoon.Radius*FEyepiece*abs(90/GLCamera1.Position.Z);
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
    GLSceneViewer1.Cursor:=crRetic;
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
  d  := sqrt(xx * xx + yy * yy) * rad2deg*FDiameter;
  if not VisibleSideLock then
     d:=d*FEarthDistance/MeanEarthDistance;
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
  if assigned(FonMoonMeasure) then FonMoonMeasure(self,m1,m2,m3,m4);
end;
end;

procedure Tf_moon.GetBounds(var lmin,lmax,bmin,bmax: single);
var
  l, b, deltab, deltal: single;
  xx, yy: integer;
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
    l      := LibrLon;
    b      := LibrLat;
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

function Tf_moon.GetCenter(var lon,lat:single):boolean;
var xx, yy: integer;
begin
  xx := GLSceneViewer1.Width div 2;
  yy := GLSceneViewer1.Height div 2;
  result:=Screen2Moon(xx,yy,lon,lat);
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
GLLightSource2.Diffuse.AsWinColor := value;
{GLLightSource1.Ambient.AsWinColor := value;
GLSceneViewer1.Buffer.AmbientColor.Red := GLLightSource1.Ambient.Red / 10;
GLSceneViewer1.Buffer.AmbientColor.Green := GLLightSource1.Ambient.Green / 10;
GLSceneViewer1.Buffer.AmbientColor.Blue := GLLightSource1.Ambient.Blue / 10;}
end;

function  Tf_moon.GetAmbientColor:TColor;
begin
result:=GLLightSource2.Diffuse.AsWinColor;//GLLightSource1.Ambient.AsWinColor;
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

procedure Tf_moon.KeyEvent(event: TMoonKeyClass; key: word);
begin
case key of
  16  : ;
end;
end;

procedure Tf_moon.World2RaDec(x, y: single; var r, d: single);
var
  spa, cpa, s: extended;
begin
  sincos(FPositionAngle, spa, cpa);
  s := FDiameter;
  r := FRaCentre + (cpa * x + spa * y) * s  / cos(FDeCentre);
  d := FDeCentre + (cpa * y - spa * x) * s;
end;

procedure Tf_moon.RaDec2World(r, d: single; var x, y: single);
var
  spa, cpa, s: extended;
begin
  sincos(-FPositionAngle, spa, cpa);
  s := FDiameter;
  r := (r - FRaCentre) * cos(FDeCentre) / s;
  d := (d - FDeCentre) / s;
  x := cpa * r + spa * d;
  y := cpa * d - spa * r;
end;

function Tf_moon.GetMarkRaDec(var ra,de: single): boolean;
var x,y,z: single;
begin
if VisibleSideLock and marked and Moon2World(markl,markb,x,y,z) then begin
  World2RaDec(x,y,ra,de);
  result:=true;
end
else
  result:=false;
end;

procedure Tf_moon.CenterAtRaDec(r,d: double);
var x,y: single;
begin
if VisibleSideLock then begin
   RaDec2World(r, d, x, y);
   if (abs(x)<1) and (abs(y)<1) then begin
     GLCamera1.Position.X:=x;
     GLCamera1.Position.Y:=y;
     GLAnnulus1.Position.x := GLCamera1.Position.x;
     GLAnnulus1.Position.y := GLCamera1.Position.y;
   end;
end;
end;

function  Tf_moon.GetAntialiasing: boolean;
begin
result:=(GLSceneViewer1.Buffer.AntiAliasing=aa4x);
end;

procedure Tf_moon.SetAntialiasing(value:boolean);
begin
if value then GLSceneViewer1.Buffer.AntiAliasing:=aa4x
         else GLSceneViewer1.Buffer.AntiAliasing:=aaDefault;
end;

procedure Tf_moon.SetShowScale(value:boolean);
begin
FShowScale:=value and (not RotationCadencer.Enabled);
SaveShowScale:=FShowScale;
SetScale;
end;

Procedure Tf_moon.SetScale;
var fv,bx,u,val:double;
    x,y,xp,yp: single;
    n,s:integer;
    scal,valkm:string;
begin
if FShowScale then begin
  fv:=0.119*GLCamera1.GetFieldOfView(GLSceneViewer1.Width)/Fzoom;
  bx:=GLSceneViewer1.Width/fv;
  fv:=fv/3;
  if trunc(fv)>20 then begin n:=trunc(fv/5); s:=5; u:=1; end
  else if trunc(fv)>5 then begin n:=trunc(fv); s:=1; u:=1; end
  else if trunc(fv)>0 then begin n:=trunc(fv)*2; s:=30; u:=1/60; end
  else if trunc(6*fv/2)>0 then begin n:=trunc(6*fv); s:=10; u:=1/60; end
  else if trunc(30*fv/2)>0 then begin n:=trunc(30*fv); s:=2; u:=1/60; end
  else if trunc(60*fv/2)>0 then begin n:=trunc(60*fv); s:=1; u:=1/60; end
  else if trunc(360*fv/2)>0 then begin n:=trunc(360*fv); s:=10; u:=1/3600; end
  else if trunc(1800*fv/2)>0 then begin n:=trunc(1800*fv); s:=2; u:=1/3600; end
  else begin n:=trunc(3600*fv); s:=1; u:=deg2rad/3600; end;
  if n<1 then n:=1;
  xp:=10;
  yp:=GLSceneViewer1.Height-15;
  GLHUDSpriteScale.width:=n*s*u*bx;
  GLHUDSpriteScale.Position.SetPoint(xp+GLHUDSpriteScale.width/2,yp,0);
  GLHUDSpriteScale.Material.FrontProperties.Emission.AsWinColor := marklabelcolor;
  GLHUDSpriteScale.Visible:=true;
  val:=round(n*s*u*60*1000)/1000;
  scal:=lmin;
  if (val<=1)or(frac(val)>0) then begin
    val:=val*60;
    scal:=lsec;
  end;
  x:=xp+GLHUDSpriteScale.width/2;
  y:=yp-8;
  GLHUDTextScale.BeginUpdate;
  GLHUDTextScaleShadow.BeginUpdate;
  GLHUDTextScale.Position.SetPoint(x,y,0);
  GLHUDTextScaleShadow.Position.SetPoint(x+1,y+1,0);
  GLHUDTextScale.Text:=formatfloat('0',val)+scal;
  GLHUDTextScaleShadow.Text:=formatfloat('0',val)+scal;
  GLHUDTextScale.ModulateColor.AsWinColor := marklabelcolor;
  GLHUDTextScale.EndUpdate;
  GLHUDTextScaleShadow.EndUpdate;
  GLHUDTextScale.Visible:=true;
  GLHUDTextScaleShadow.Visible:=true;
  if VisibleSideLock then
     valkm:=''
  else
     valkm:=formatfloat('0',tan(n*s*u*deg2rad)*(MeanEarthDistance-Rmoon))+rsm_18;
  y:=yp+8;
  GLHUDTextScalekm.BeginUpdate;
  GLHUDTextScalekmShadow.BeginUpdate;
  GLHUDTextScalekm.Position.SetPoint(x,y,0);
  GLHUDTextScalekmShadow.Position.SetPoint(x+1,y+1,0);
  GLHUDTextScalekm.Text:=valkm;
  GLHUDTextScalekmShadow.Text:=valkm;
  GLHUDTextScalekm.ModulateColor.AsWinColor := marklabelcolor;
  GLHUDTextScalekm.EndUpdate;
  GLHUDTextScalekmShadow.EndUpdate;
  GLHUDTextScalekm.Visible:=true;
  GLHUDTextScalekmShadow.visible:=true;
end else begin
  GLHUDSpriteScale.Visible:=false;
  GLHUDTextScale.Visible:=false;
  GLHUDTextScaleShadow.Visible:=false;
  GLHUDTextScalekm.Visible:=false;
  GLHUDTextScalekmShadow.visible:=false;
end;
end;

procedure Tf_moon.SetCCD(w,h,r: single);
begin
  FCCDw:=w;
  FCCDh:=h;
  FCCDr:=r;
end;

procedure Tf_moon.SetShowCCD(value:boolean);
begin
FShowCCD:=value and (VisibleSideLock) and (FCCDw>0) and (FCCDh>0);
SetCCDfield;
end;

Procedure Tf_moon.SetCCDfield;
var i: integer;
    sinr,cosr: extended;
    fv,bx,u: double;
    x,y,xc,yc,wc,hc: single;
begin
if FShowCCD then begin
  fv:=0.119*GLCamera1.GetFieldOfView(GLSceneViewer1.Width)/Fzoom;
  bx:=GLSceneViewer1.Width/fv;
  u:=1/60;
  wc:=FCCDw*u*bx;
  hc:=FCCDh*u*bx;
  SinCos(deg2rad*FCCDr,sinr,cosr);
  GLHUDSpriteCCD1.Width:=wc;
  GLHUDSpriteCCD3.Width:=wc;
  GLHUDSpriteCCD2.Width:=hc;
  GLHUDSpriteCCD4.Width:=hc;
  xc:=GLSceneViewer1.Width/2;
  yc:=GLSceneViewer1.Height/2;
  x:=xc-(hc/2)*sinr;
  y:=yc-(hc/2)*cosr;
  GLHUDSpriteCCD1.Position.SetPoint(x,y,0);
  GLHUDSpriteCCD1.Rotation:=FCCDr;
  x:=xc+(hc/2)*sinr;
  y:=yc+(hc/2)*cosr;
  GLHUDSpriteCCD3.Position.SetPoint(x,y,0);
  GLHUDSpriteCCD3.Rotation:=FCCDr;
  x:=xc+(wc/2)*cosr;
  y:=yc-(wc/2)*sinr;
  GLHUDSpriteCCD2.Position.SetPoint(x,y,0);
  GLHUDSpriteCCD2.Rotation:=90+FCCDr;
  x:=xc-(wc/2)*cosr;
  y:=yc+(wc/2)*sinr;
  GLHUDSpriteCCD4.Position.SetPoint(x,y,0);
  GLHUDSpriteCCD4.Rotation:=90+FCCDr;
  GLHUDSpriteCCD1.Material.FrontProperties.Emission.AsWinColor := markcolor;
  GLHUDSpriteCCD2.Material.FrontProperties.Emission.AsWinColor := markcolor;
  GLHUDSpriteCCD3.Material.FrontProperties.Emission.AsWinColor := markcolor;
  GLHUDSpriteCCD4.Material.FrontProperties.Emission.AsWinColor := markcolor;
  GLHUDSpriteCCD1.Visible:=true;
end else begin
  GLHUDSpriteCCD1.Visible:=false;
end;
end;

initialization

SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

end.

