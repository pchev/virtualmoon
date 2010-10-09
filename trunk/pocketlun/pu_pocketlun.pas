unit pu_pocketlun;
{
Copyright (C) 2007 Patrick Chevalley

http://www.astrosurf.com/avl
pch@freesurf.ch

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

{$mode objfpc}{$H+}

interface

uses   u_translation, wince_func, u_astro, cu_moon, cu_tz, passqlite, IniFiles, Windows,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, FileUtil,
  StdCtrls, ExtCtrls, Math, ComCtrls, Menus, ActnList, Buttons;

Const
      crlf = chr(10)+chr(13);
      tab  = chr(9);

type

  TPhraseFormat = (PhraseFormatEnglish, PhraseFormatLatin, PhraseFormatRussian);

  TChartDrawingControl = class(TCustomControl)
  public
    procedure Paint; override;
    property onMouseDown;
    property onMouseMove;
    property onMouseUp;
  end;

  { Tf_pocketlun }

  Tf_pocketlun = class(TForm)
    about1: TAction;
    btn01: TImage;
    btn10: TImage;
    btn08: TImage;
    btn02: TImage;
    btn07: TImage;
    btn06: TImage;
    btn09: TImage;
    btn03: TImage;
    btn05: TImage;
    btn04: TImage;
    comment1: TAction;
    DebugBtn: TMenuItem;
    DateLabel: TLabel;
    MapLabel: TLabel;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    TopPanel: TPanel;
    SplashPanel: TPanel;
    ShowLabel1: TAction;
    estouest1: TAction;
    ptotos2: TAction;
    centre1: TAction;
    zoom11: TAction;
    nordsud1: TAction;
    rotation1: TAction;
    phase1: TAction;
    zoommoins1: TAction;
    zoomplus1: TAction;
    info1: TAction;
    MenuItem11: TMenuItem;
    popup_name: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    notes1: TAction;
    photos1: TAction;
    display1: TAction;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PopupMenu1: TPopupMenu;
    tools1: TAction;
    ephemeris1: TAction;
    calendar1: TAction;
    Search1: TAction;
    help1: TAction;
    config1: TAction;
    Close1: TAction;
    ActionList1: TActionList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    dbm: TLiteDB;
    MainMenu1: TMainMenu;
    splash: TPanel;
    procedure about1Execute(Sender: TObject);
    procedure comment1Execute(Sender: TObject);
    procedure InitScreen(Sender: TObject);
    procedure calendar1Execute(Sender: TObject);
    procedure centre1Execute(Sender: TObject);
    procedure Close1Execute(Sender: TObject);
    procedure config1Execute(Sender: TObject);
    procedure display1Execute(Sender: TObject);
    procedure ephemeris1Execute(Sender: TObject);
    procedure estouest1Execute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure help1Execute(Sender: TObject);
    procedure info1Execute(Sender: TObject);
    procedure DebugBtnClick(Sender: TObject);
    procedure nordsud1Execute(Sender: TObject);
    procedure notes1Execute(Sender: TObject);
    procedure phase1Execute(Sender: TObject);
    procedure photos1Execute(Sender: TObject);
    procedure rotation1Execute(Sender: TObject);
    procedure Search1Execute(Sender: TObject);
    procedure ShowLabel1Execute(Sender: TObject);
    procedure tools1Execute(Sender: TObject);
    procedure zoom11Execute(Sender: TObject);
    procedure zoommoins1Execute(Sender: TObject);
    procedure zoomplus1Execute(Sender: TObject);
  private
    { private declarations }
    moon: TMoon;
    tzinfo: TCdCTimeZone;
    moona,image2d:TBitmap;
    zoom,zoommax,zoommin,zoombase,bx,by,lrot,inclination,librl,librb,librlong,librlat: double;
    MoonRa,MoonDec,rad,ded,MoonDist,MoonDkm,MoonDiam,Phase,MoonIllum,MoonPA: double;
    sunincl,tsunincl,tphase,nmjd,fqjd,fmjd,lqjd,lunaison,cphase,colong : double;
    ObsLongitude,ObsLatitude,ObsAltitude, ObsRefractionCor,ObsRoCosPhi,ObsRoSinPhi: double;
    FindX,FindY,CurrentJD,Currenttime,timezone,dt_ut: double;
    dummy: double;
    CurYear,CurrentMonth,CurrentDay,TextureQuality,LabelDensity,Labelsize: integer;
    moveX,moveY,xpos,ypos,ax,ay: integer;
    ox,oy,os,deffontsize,LabelColorIndex: integer;
    mr_Y,mr_M,mr_D : integer;
    lastX,lastY: integer;
    PhraseFormat: TPhraseFormat;
    phaseumbra,LabelColor: Tcolor;
    SkipIdent,SkipMove,SkipLabel,lockmove,lockrefresh,minilabel,showlabel,labelcenter:boolean;
    farside,geocentric,phaseeffect,librationeffect,PhaseLine : boolean;
    initialized,flipx,flipy,ShowLibrationMark : boolean;
    ObsTZ,ObsCountry,deffont: string;
    moonrise,moonset,moontransit,azimuthrise,azimuthset,altitudetransit : string;
    AppDataDir,homedir,prgfilesdir,privatedir,configfile,dbfile,imgfile,sidelist,currentid,currentname: string;
    next_win : Tvma_window;
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure Image1Paint(Sender: TObject);
    Procedure SetLang;
    Procedure GetLanguage;
    Function GetTimeZone(jdt:Double) : double;
    Procedure InitDate;
    Procedure SetDate(jdd: double);
    procedure LoadTexture(retry:boolean=true);
    procedure LoadDB;
    procedure InitObservatoire;
    procedure Window2World(x,y : integer; var xx,yy : double);
    procedure World2Window(xx,yy : double; var x,y : integer);
    function ProjMoon(l,b,lc,bc : Double ; var X,Y : Double ):boolean;
    function InvProjMoon (x,y,lc,bc : Double ; var l,b : Double ):boolean;
    Procedure Refresh2DImage;
    Procedure DrawPhase;
    Procedure DrawLabel;
    Procedure LibrationMark(libl,libb:double);
    Procedure RefreshMoon;
    Procedure ComputeMoon;
    procedure RefreshPhase(Sender: TObject);
    procedure SelectPhase(Sender: TObject);
    Procedure UpdateCalendar;
    procedure markpos(x,y: double);
    procedure ShowPopup;
    function SearchAtPos(l,b: double):boolean;
    function SearchName(n: string; center: boolean; mark:boolean=false):boolean;
    procedure Search(Sender: TObject);
    procedure GetDetail(lin:TStringList);
    procedure MoveCamera(x,y :double);
    procedure UpdateEphemeris;
    procedure EphemerisDateChange(Sender: TObject);
    procedure UpdateConfig(Sender: TObject);
    procedure UpdateConfigDisplay(Sender: TObject);
    procedure nextwindow;
    procedure SaveDefault(savelanguage: boolean=false);
    procedure LoadDefault;
    procedure SetButtons;
  public
    { public declarations }
    Image1 : TChartDrawingControl;
    cbmp: TBitmap;
    procedure Init;
  end;

var
  f_pocketlun: Tf_pocketlun;

implementation

Uses pu_info, pu_ephemeris, pu_about, pu_config_display, pu_config, pu_calendar,
     pu_search, pu_photo, pu_notes, pu_tools ;

{$R vma_icon_menu.res}

const
     deg2rad=pi/180;
     rad2deg=180/pi;
     d1='0.0';
     d2='0.00';
     px=0.95467; py=0.95467; // 1432/1500


{ Tf_pocketlun }


Function Tf_pocketlun.GetTimeZone(jdt: double) : double;
var x: double;
begin
x:=tzinfo.JD;
tzinfo.JD:=jdt;
result:=tzinfo.SecondsOffset/3600;
tzinfo.JD:=x;
end;

Procedure Tf_pocketlun.InitDate;
var y,mm,d,h,n,s,ms : word;
begin
decodedate(now,y,mm,d);
decodetime(now,h,n,s,ms);
dt_ut:=dtminusut(y);
CurYear:=y;
CurrentMonth:=mm;
CurrentDay:=d;
Currenttime:=h+n/60+s/3600;
tzinfo.JD:=jd(CurYear,CurrentMonth,CurrentDay,Currenttime);
timezone:=tzinfo.SecondsOffset/3600;
CurrentJD:=jd(CurYear,CurrentMonth,CurrentDay,Currenttime-timezone+DT_UT);
end;

Procedure Tf_pocketlun.SetDate(jdd: double);
begin
djd(jdd,CurYear,CurrentMonth,CurrentDay,Currenttime);
dt_ut:=dtminusut(CurYear);
tzinfo.JD:=jd(CurYear,CurrentMonth,CurrentDay,Currenttime);
timezone:=tzinfo.SecondsOffset/3600;
CurrentJD:=jd(CurYear,CurrentMonth,CurrentDay,Currenttime-timezone+DT_UT);
end;

procedure Tf_pocketlun.InitObservatoire;
var u,p : double;
const ratio = 0.99664719;
      H0 = 6378140.0 ;
begin
   p:=deg2rad*ObsLatitude;
   u:=arctan(ratio*tan(p));
   ObsRoSinPhi:=ratio*sin(u)+(ObsAltitude/H0)*sin(p);
   ObsRoCosPhi:=cos(u)+(ObsAltitude/H0)*cos(p);
   ObsRefractionCor:=1;
   tzinfo.TimeZoneFile:=AppDir+slash('zoneinfo')+StringReplace(ObsTZ,'/',PathDelim,[rfReplaceAll]);
   timezone:=tzinfo.SecondsOffset/3600;
end;

procedure Tf_pocketlun.Window2World(x,y : integer; var xx,yy : double);
begin
  x:=ax+round(bx*x);
  y:=ay+round(by*y);
  xx:=-((x-ox*image2d.Width/os)/(px*image2d.Width)-0.5);
  yy:=-((y-oy*image2d.height/os)/(py*image2d.height)-0.5);
  maplabel.caption:='x:'+inttostr(x)+' y:'+inttostr(y)+'  xx:'+formatfloat('0.00',xx)+'  yy:'+formatfloat('0.00',yy);
end;

procedure Tf_pocketlun.World2Window(xx,yy : double; var x,y : integer);
begin
if (bx=0)or(by=0) then begin
  x:=0;
  y:=0;
end else begin
  x:=round(ox*image2d.Width/os+(px*image2d.Width)*(0.5-xx));
  y:=round(oy*image2d.Height/os+(py*image2d.Height)*(0.5-yy));
  x:=round((x-ax)/bx);
  y:=round((y-ay)/by);
  end;
end;

function Tf_pocketlun.ProjMoon(l,b,lc,bc : Double ; var X,Y : Double ):boolean;
Var hh,s1,s2,s3,c1,c2,c3 : extended ;
BEGIN
 if inclination=0 then begin
    hh := l-lc+lrot ;
    if hh>180 then hh:=hh-360;
    if hh<-180 then hh:=hh+360;
    if (abs(hh)>95) then begin result:=false; exit; end;   // tolerance sur le limbe.
    sincos(Deg2Rad*bc,s1,c1);
    sincos(Deg2Rad*b,s2,c2);
    sincos(Deg2Rad*hh,s3,c3);
    x:= -(c2*s3);
    y:= (s2*c1-c2*s1*c3);
    x:=sgn(x)*minvalue([abs(x),10000])/2;
    y:=sgn(y)*minvalue([abs(y),10000])/2;
    result:=true;
 end else result:=false;
END ;

function Tf_pocketlun.InvProjMoon (x,y,lc,bc : Double ; var l,b : Double ):boolean;
Var r,s1,c1,s : extended ;
Begin
    sincos(deg2rad*bc,s1,c1);
    s:=1-x*x-y*y;
    if (inclination=0)and(s>=0) then begin
      r:=sqrt(s);
      l:=lc-lrot-rad2deg*arctan2(x,(c1*r-y*s1));
      if l>180 then l:=l-360;
      if l<-180 then l:=360+l;
      b:=rad2deg*arcsin(y*c1+s1*r);
      result:=true;
    end else begin
      l:=nan;
      b:=nan;
      result:=false;
    end;
end ;

Procedure Tf_pocketlun.RefreshMoon;
begin
image2d.Canvas.Draw(0,0,moona);
DrawPhase;
Refresh2DImage;
SetButtons;
end;

Procedure Tf_pocketlun.Refresh2DImage;
var x1,y1,x2,y2,dx,dy,x0,y0,xd,yd,x,xd1,yd1,xd2,yd2: integer;
    winratio: double;
begin
if zoom<zoommin then zoom:=zoommin;
if zoom=zoommin then begin
  Xpos:=image2d.Width div 2;
  Ypos:=image2d.Height div 2;
end;
cbmp.Width:=Image1.Width;
cbmp.Height:=Image1.Height;
winratio:=Image1.width/Image1.height;
dx:=round((image2d.width)/zoom/2);
x0:=image2d.width-dx-dx;
dy:=round(image2d.height/zoom/2/winratio);
y0:=image2d.height-dy-dy;
x1:=Xpos-dx;
if x1<0 then x1:=0;
if x1>x0 then x1:=x0;
Xpos:=x1+dx;
y1:=Ypos-dy;
if y1<0 then y1:=0;
if y1>y0 then y1:=y0;
Ypos:=y1+dy;
x2:=x1+dx+dx-1;
y2:=y1+dy+dy-1;
xd:=Image1.Width;
yd:=Image1.Height;
if flipx then begin
   x:=x1;
   x1:=x2;
   x2:=x;
end;
if flipy then begin
   x:=y1;
   y1:=y2;
   y2:=x;
end;
ax:=x1;
ay:=y1;
bx:=2*dx/xd;
by:=2*dy/yd;
if flipx then bx:=-bx;
if flipy then by:=-by;
xd1:=0; yd1:=0;
xd2:=xd;
yd2:=yd;
cbmp.canvas.brush.color:=clBlack;
cbmp.canvas.Brush.style:=bsSolid;
cbmp.canvas.fillrect(rect(0,0,cbmp.width,cbmp.height));
cbmp.canvas.CopyRect(rect(xd1,yd1,xd2,yd2),image2d.Canvas,rect(x1,y1,x2,y2));
LibrationMark(librlong,librlat);
DrawLabel;
Image1.Invalidate;
end;

procedure Tf_pocketlun.InitScreen(Sender: TObject);
begin
  if splash.Visible then init;
end;

procedure Tf_pocketlun.nextwindow;
begin
case next_win of
  w_info           : info1.Execute;
  w_ephemeris      : ephemeris1.Execute;
  w_about          : about1.Execute;
  w_calendar       : calendar1.Execute;
  w_config         : config1.Execute;
  w_config_display : display1.Execute;
  w_help           : help1.Execute;
  w_search         : Search1.Execute;
  w_notes          : notes1.Execute;
  w_photo          : photos1.Execute;
  w_tools          : tools1.Execute;
end;
end;

procedure Tf_pocketlun.about1Execute(Sender: TObject);
begin
  f_about.Font.Name:=deffont;
  f_about.Font.Size:=deffontsize;
  VMAShowmodal(f_about,self);
  next_win:=f_about.NextWindow;
  nextwindow;
end;

procedure Tf_pocketlun.comment1Execute(Sender: TObject);
var f : Tsearchrec;
    r,i : integer;
    buf,nom,nom2,commentfile,dir:string;
const  audiofile = '.mp3';
begin
commentfile:='';
dir:=appdir+slash('Audio')+slash(language);
if not DirectoryExists(dir) then dir:=appdir+slash('Audio')+slash('en');
nom:=trim(currentname);
r:=length(nom)-1;
if copy(nom,r,1)=' ' then begin
  buf:=copy(nom,r+1,1);
  if ((buf>='a')and(buf<='z'))or((buf>='A')and(buf<='Z')) then nom2:=copy(nom,1,r-1);
end else nom2:='';
r:=findfirst(dir+nom+audiofile,0,f);
commentfile:=f.Name;
findclose(f);
if r<>0 then begin
  r:=findfirst(dir+nom+'_*'+audiofile,0,f);
  commentfile:=f.Name;
  findclose(f);
  if r<>0 then begin
  if nom2<>'' then begin
    r:=findfirst(dir+nom2+audiofile,0,f);
    commentfile:=f.Name;
    findclose(f);
    if r<>0 then begin
      r:=findfirst(dir+nom2+'_*'+audiofile,0,f);
      commentfile:=f.Name;
      findclose(f);
    end;
  end;
  end;
end;
if (r=0) then begin
   try
   commentfile:=dir+commentfile;
   if fileexists(commentfile) then
      i:=ExecuteFile(commentfile);
      if (i>=0)and(i<=32) then begin
        ExecNoWait('\Windows\wmplayer.exe','"'+commentfile+'"');
      end;
   except
   end;
end;
end;

procedure Tf_pocketlun.help1Execute(Sender: TObject);
var i: integer;
begin
 i:=ExecuteFile(appdir+slash('doc')+language+'_PocketLun.html');
 if (i>=0)and(i<=32) then begin
     ExecNoWait('\Windows\iexplore.exe',appdir+slash('doc')+language+'_PocketLun.html');
 end;
end;

procedure Tf_pocketlun.Search1Execute(Sender: TObject);
var i: integer;
    buf: string;
begin
if splash.visible then exit;
f_search.Font.Name:=deffont;
f_search.Font.Size:=deffontsize;
f_search.StringGrid1.Font.Name:=deffont;
f_search.StringGrid1.Font.Size:=deffontsize;
f_search.EditSearch.Font.Name:=deffont;
f_search.EditSearch.Font.Size:=deffontsize;
f_search.ButtonSearch.Font.Name:=deffont;
f_search.ButtonSearch.Font.Size:=deffontsize;
VMAShowmodal(f_search,self);
if f_search.ModalResult=mrOK then Search(Sender)
else if f_search.ModalResult=mrCancel then begin
  next_win:=f_search.NextWindow;
  nextwindow;
  end;
end;

procedure Tf_pocketlun.Search(Sender: TObject);
var xx,yy: integer;
    ip,sp: TPoint;
begin
if (f_search.searchname>'') and SearchName(f_search.searchname,true,true) then begin
    world2window(FindX,FindY,xx,yy);
    PopUp_Name.Caption:=currentname;
    ip.x:=xx+5;
    ip.y:=yy+5;
    sp:=Image1.ClientToScreen(ip);
    PopupMenu1.PopUp(sp.x,sp.y);
end;
end;

procedure Tf_pocketlun.tools1Execute(Sender: TObject);
begin
  f_tools.Font.Name:=deffont;
  f_tools.Font.Size:=deffontsize;
  VMAShowmodal(f_tools,self);
  next_win:=f_tools.NextWindow;
  nextwindow;
end;


procedure Tf_pocketlun.ShowLabel1Execute(Sender: TObject);
begin
if splash.visible then exit;
ShowLabel:=not ShowLabel;
Refresh2DImage;
end;

procedure Tf_pocketlun.zoom11Execute(Sender: TObject);
begin
if splash.visible then exit;
zoom:=1;
Refresh2DImage;
end;

procedure Tf_pocketlun.zoommoins1Execute(Sender: TObject);
begin
if splash.visible then exit;
zoom:=zoom/1.5;
if zoom<zoommin then zoom:=zoommin;
Refresh2DImage;
end;

procedure Tf_pocketlun.zoomplus1Execute(Sender: TObject);
begin
if splash.visible then exit;
zoom:=1.5*zoom;
if zoom>zoommax then zoom:=zoommax;
Refresh2DImage;
end;

Procedure Tf_pocketlun.UpdateCalendar;
var aa,mm,dd: integer;
    hh,tz: double;
begin
  djd(nmjd-(DT_UT)/24,aa,mm,dd,hh);
  tz:=gettimezone(nmjd);
  djd(nmjd+(tz-DT_UT)/24,aa,mm,dd,hh);
  f_calendar.DateNL.caption:=dattostr(aa,mm,dd)+' '+timmtostr(hh);

  djd(fqjd-(DT_UT)/24,aa,mm,dd,hh);
  tz:=gettimezone(fqjd);
  djd(fqjd+(tz-DT_UT)/24,aa,mm,dd,hh);
  f_calendar.DatePQ.caption:=dattostr(aa,mm,dd)+' '+timmtostr(hh);

  djd(fmjd-(DT_UT)/24,aa,mm,dd,hh);
  tz:=gettimezone(fmjd);
  djd(fmjd+(tz-DT_UT)/24,aa,mm,dd,hh);
  f_calendar.DatePL.caption:=dattostr(aa,mm,dd)+' '+timmtostr(hh);

  djd(lqjd-(DT_UT)/24,aa,mm,dd,hh);
  tz:=gettimezone(lqjd);
  djd(lqjd+(tz-DT_UT)/24,aa,mm,dd,hh);
  f_calendar.DateLQ.caption:=dattostr(aa,mm,dd)+' '+timmtostr(hh);
end;

procedure Tf_pocketlun.calendar1Execute(Sender: TObject);
begin
  f_calendar.Font.Name:=deffont;
  f_calendar.Font.Size:=deffontsize;
  f_calendar.phaseoffset:=0;
  UpdateCalendar;
  VMAShowmodal(f_calendar,self);
  if f_calendar.ModalResult=mrOK then begin
     if f_calendar.PhaseSelect>0 then SelectPhase(Sender);
  end else begin
    next_win:=f_calendar.NextWindow;
    nextwindow;
  end;
end;

procedure Tf_pocketlun.RefreshPhase(Sender: TObject);
var jd0:double;
begin
  jd0:=jd(CurYear,1,1,0.0);
  moon.MoonPhases((f_calendar.phaseoffset/12.3685)+CurYear+(CurrentJD-jd0)/365.25,nmjd,fqjd,fmjd,lqjd);
  UpdateCalendar;
end;

procedure Tf_pocketlun.SelectPhase(Sender: TObject);
var jdt: double;
begin
case f_calendar.PhaseSelect of
  1 : jdt:=nmjd;
  2 : jdt:=fqjd;
  3 : jdt:=fmjd;
  4 : jdt:=lqjd;
end;
SetDate(jdt);
ComputeMoon;
RefreshMoon;
end;

procedure Tf_pocketlun.centre1Execute(Sender: TObject);
var xx,yy : double;
begin
if splash.visible then exit;
if zoom=1 then Movecamera(0,0)
//else if marked then MoveCamera(markx,marky)
else begin
 xx:=Findx;
 yy:=Findy;
 if (abs(xx)<0.5)and(abs(yy)<0.5) then begin
    MoveCamera(xx,yy);
    if currentname<>'' then begin
      Markpos(xx,yy);
      ShowPopup;
    end;
 end;
end;
end;

procedure Tf_pocketlun.Close1Execute(Sender: TObject);
begin
  Close;
end;

procedure Tf_pocketlun.config1Execute(Sender: TObject);
begin
  f_config.Font.Name:=deffont;
  f_config.Font.Size:=deffontsize;
  f_config.ObsLat:=ObsLatitude;
  f_config.ObsLon:=ObsLongitude;
  f_config.ObsTZ:=ObsTZ;
  f_config.ObsCountry:=ObsCountry;
  f_config.language:=language;
  VMAShowmodal(f_config,self);
  UpdateConfig(Sender);
  next_win:=f_config.NextWindow;
  nextwindow;
end;

procedure Tf_pocketlun.UpdateConfig(Sender: TObject);
var savelang: string;
begin
screen.Cursor:=crHourGlass;
try
savelang:=language;
ObsLatitude:=f_config.ObsLat;
ObsLongitude:=f_config.ObsLon;
ObsTZ:=f_config.ObsTZ;
ObsCountry:=f_config.ObsCountry;
language:=f_config.language;
SaveDefault(true);
language:=savelang;
InitObservatoire;
ComputeMoon;
RefreshMoon;
finally
language:=savelang;
screen.Cursor:=crDefault;
end;
end;

procedure Tf_pocketlun.display1Execute(Sender: TObject);
var mem: double;
begin
  f_config_display.Font.Name:=deffont;
  f_config_display.Font.Size:=deffontsize;
  f_config_display.TextSize.Position:=deffontsize;
  f_config_display.LabelSize.Position:=LabelSize;
  f_config_display.LabelDensity.Position:=LabelDensity;
  f_config_display.ColorBox1.ItemIndex:=LabelColorIndex;
  f_config_display.TextureQuality.Max:=3;
  f_config_display.TextureQuality.Position:=TextureQuality;
  f_config_display.CenterLabel.Checked:=labelcenter;
  VMAShowmodal(f_config_display,self);
  UpdateConfigDisplay(Sender);
  next_win:=f_config_display.NextWindow;
  nextwindow;
end;

procedure Tf_pocketlun.UpdateConfigDisplay(Sender: TObject);
begin
screen.Cursor:=crHourGlass;
try
deffontsize:=f_config_display.TextSize.Position;
LabelSize:=f_config_display.LabelSize.Position;
LabelDensity:=f_config_display.LabelDensity.Position;
LabelColorIndex:=f_config_display.ColorBox1.ItemIndex;
LabelColor:=f_config_display.ColorBox1.Color;
LabelCenter:=f_config_display.CenterLabel.Checked;
if f_config_display.TextureQuality.Position<>TextureQuality then begin
   TextureQuality:=f_config_display.TextureQuality.Position;
   LoadTexture;
end;
SetButtons;
SaveDefault;
ComputeMoon;
RefreshMoon;
finally
screen.Cursor:=crDefault;
end;
end;

procedure Tf_pocketlun.UpdateEphemeris;
var i,risetype : integer;
    jd0,CurST,az,ah: double;
    jdr,jdt,jds,rar,der,rat,det,ras,des:double;
const b=' ';
begin
  f_ephemeris.LabelDate.Caption:=rsDate+':'+dattostr(curyear, currentmonth,
    currentday)+' '+rsTime+':'+timtostr(currenttime);
  i:=0;
  f_ephemeris.Stringgrid1.Cells[0, i]:=rsObservatory;
  if geocentric then f_ephemeris.Stringgrid1.Cells[1, i]:=rsGeocentric
                else begin f_ephemeris.Stringgrid1.Cells[1,i]:=demtostr(ObsLatitude)+' '+demtostr(ObsLongitude);end;
  inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsTimeZone;
    f_ephemeris.Stringgrid1.Cells[1, i]:=ObsTz;
  inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:='(J2000) '+rsRightAscensi+':';
    f_ephemeris.Stringgrid1.Cells[1, i]:=artostr(rad2deg*MoonRa/15);
  inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:='(J2000) '+rsDeclination+':';
    f_ephemeris.Stringgrid1.Cells[1, i]:=detostr(rad2deg*MoonDec);
  inc(i); f_ephemeris.Stringgrid1.Cells[0,i]:='('+rsDate+')'+b+rsRightAscensi+':'; f_ephemeris.Stringgrid1.Cells[1,i]:=artostr(rad2deg*rad/15);
  inc(i); f_ephemeris.Stringgrid1.Cells[0,i]:='('+rsDate+')'+b+rsDeclination+':'; f_ephemeris.Stringgrid1.Cells[1,i]:=detostr(rad2deg*ded);
  inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsDistance+':';
    f_ephemeris.Stringgrid1.Cells[1, i]:=inttostr(round(MoonDkm))+rsKm;
  inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsApparentDiam+':';
    f_ephemeris.Stringgrid1.Cells[1, i]:=formatfloat(d2, MoonDiam/60)+lmin;
  inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsColongitude+':';
    f_ephemeris.Stringgrid1.Cells[1, i]:=formatfloat(d1, colong)+ldeg;
  inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsPhase+':';
    f_ephemeris.Stringgrid1.Cells[1, i]:=formatfloat(d1, tphase)+ldeg;
  inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsLunation+':';
    f_ephemeris.Stringgrid1.Cells[1, i]:=formatfloat(d2, lunaison)+' '+rsDays;
  inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsIllumination+':';
    f_ephemeris.Stringgrid1.Cells[1, i]:=formatfloat(d1, MoonIllum*100)+'%';
  inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsField+':';
    f_ephemeris.Stringgrid1.Cells[1, i]:=formatfloat(d1, tsunincl)+ldeg;
  inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsLibrationInL+':';
    f_ephemeris.Stringgrid1.Cells[1, i]:=demtostr(librlat);
  inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsLibrationInL2+':';
    f_ephemeris.Stringgrid1.Cells[1, i]:=demtostr(librlong);
  inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsPositionAngl+':';
    f_ephemeris.Stringgrid1.Cells[1, i]:=formatfloat(d1, MoonPA)+ldeg;
  if not geocentric then begin
    jd0:=jd(CurYear,currentmonth,currentday,0);
    CurST:=Sidtim(jd0,currenttime-TimeZone,ObsLongitude);
    eq2hz(CurST-rad,ded,ObsLatitude,az,ah);
    az:=rmod(az+pi,pi2);
    inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsAzimuth+':';
      f_ephemeris.Stringgrid1.Cells[1, i]:=demtostr(rad2deg*az);
    inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsAltitude+':';
      f_ephemeris.Stringgrid1.Cells[1, i]:=demtostr(rad2deg*ah);
    if (mr_Y<>CurYear)or(mr_M<>CurrentMonth)or(mr_D<>CurrentDay) then begin
       moon.MoonRiseSet(jd0,TimeZone,DT_UT,Obslatitude,Obslongitude,ObsRoCosPhi,ObsRoSinPhi,true,moonrise,moontransit,moonset,azimuthrise,altitudetransit,azimuthset,jdr,jdt,jds,rar,der,rat,det,ras,des,risetype);
       mr_Y:=CurYear;
       mr_M:=CurrentMonth;
       mr_D:=CurrentDay;
    end;
    inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsRise+':';
      f_ephemeris.Stringgrid1.Cells[1, i]:=moonrise;
    inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsTransit+':';
      f_ephemeris.Stringgrid1.Cells[1, i]:=moontransit;
    inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsSet+':';
      f_ephemeris.Stringgrid1.Cells[1, i]:=moonset;
    inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsRiseAzimuth+':';
      f_ephemeris.Stringgrid1.Cells[1, i]:=azimuthrise;
    inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsTransitAltit+':';
      f_ephemeris.Stringgrid1.Cells[1, i]:=altitudetransit;
    inc(i); f_ephemeris.Stringgrid1.Cells[0, i]:=rsSetAzimuth+':';
      f_ephemeris.Stringgrid1.Cells[1, i]:=azimuthset;
  end else begin
    inc(i); f_ephemeris.Stringgrid1.Cells[0,i]:=b; f_ephemeris.Stringgrid1.Cells[1,i]:=b;
    inc(i); f_ephemeris.Stringgrid1.Cells[0,i]:=b; f_ephemeris.Stringgrid1.Cells[1,i]:=b;
    inc(i); f_ephemeris.Stringgrid1.Cells[0,i]:=b; f_ephemeris.Stringgrid1.Cells[1,i]:=b;
    inc(i); f_ephemeris.Stringgrid1.Cells[0,i]:=b; f_ephemeris.Stringgrid1.Cells[1,i]:=b;
    inc(i); f_ephemeris.Stringgrid1.Cells[0,i]:=b; f_ephemeris.Stringgrid1.Cells[1,i]:=b;
    inc(i); f_ephemeris.Stringgrid1.Cells[0,i]:=b; f_ephemeris.Stringgrid1.Cells[1,i]:=b;
    inc(i); f_ephemeris.Stringgrid1.Cells[0,i]:=b; f_ephemeris.Stringgrid1.Cells[1,i]:=b;
    inc(i); f_ephemeris.Stringgrid1.Cells[0,i]:=b; f_ephemeris.Stringgrid1.Cells[1,i]:=b;
  end;

end;

procedure Tf_pocketlun.EphemerisDateChange(Sender: TObject);
begin
SetDate(f_ephemeris.JDD);
ComputeMoon;
UpdateEphemeris;
SkipLabel:=true;
RefreshMoon;
SkipLabel:=false;
end;

procedure Tf_pocketlun.ephemeris1Execute(Sender: TObject);
begin
  screen.Cursor:=crHourGlass;
  try
  f_ephemeris.Font.Name:=deffont;
  f_ephemeris.Font.Size:=deffontsize;
  f_ephemeris.StringGrid1.Font.Name:=deffont;
  f_ephemeris.StringGrid1.Font.Size:=deffontsize;
  UpdateEphemeris;
  finally
    screen.cursor:=crDefault;
  end;
  f_ephemeris.JDD:=jd(CurYear,CurrentMonth,CurrentDay,Currenttime);
  VMAShowmodal(f_ephemeris,self);
  Refresh2DImage;
  next_win:=f_ephemeris.NextWindow;
  nextwindow;
end;

procedure Tf_pocketlun.estouest1Execute(Sender: TObject);
begin
if splash.visible then exit;
  flipx:=not flipx;
  btn04.Transparent:=not flipx;
{//  btn05.down:=flipx;
  if flipx then btn05.Color:=clBtnShadow
           else btn05.Color:=clBtnFace;}
  Refresh2DImage;
end;

procedure Tf_pocketlun.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveDefault;
end;

procedure Tf_pocketlun.info1Execute(Sender: TObject);
var buf:TStringList;
    i:integer;
begin
  buf:=TStringList.Create;
  f_info.Font.Name:=deffont;
  f_info.Font.Size:=deffontsize;
  f_info.Title.Caption:=currentname;
  GetDetail(buf);
  f_info.Memo1.Lines.Assign(buf);
  VMAShowmodal(f_info,self);
  next_win:=f_info.NextWindow;
  nextwindow;
  buf.free;
end;

procedure Tf_pocketlun.DebugBtnClick(Sender: TObject);
begin
ShowMessage(formatfloat('0.0 MB',MemoryAvailable()/1024/1024));
end;

procedure Tf_pocketlun.nordsud1Execute(Sender: TObject);
begin
if splash.visible then exit;
  flipy:=not flipy;
  btn05.Transparent:=not flipy;
{//  btn03.down:=flipy;
  if flipy then btn03.Color:=clBtnShadow
           else btn03.Color:=clBtnFace;}
  Refresh2DImage;
end;

procedure Tf_pocketlun.notes1Execute(Sender: TObject);
begin
  f_notes.Font.Name:=deffont;
  f_notes.Font.Size:=deffontsize;
  f_notes.GetNotes(currentname);
  VMAShowmodal(f_notes,self);
  next_win:=f_notes.NextWindow;
  nextwindow;
end;

procedure Tf_pocketlun.phase1Execute(Sender: TObject);
begin
if splash.visible then exit;
PhaseEffect := not PhaseEffect;
if farside then PhaseEffect:=false;
btn07.Transparent:=not PhaseEffect;
{//btn06.down:=PhaseEffect;
if PhaseEffect then btn06.Color:=clBtnShadow
               else btn06.Color:=clBtnFace;}
ComputeMoon;
RefreshMoon;
end;

procedure Tf_pocketlun.photos1Execute(Sender: TObject);
begin
if splash.visible then exit;
  f_photo.Font.Name:=deffont;
  f_photo.Font.Size:=deffontsize;
  f_photo.Title.Font.Name:=deffont;
  f_photo.Title.Font.Size:=deffontsize;
  f_photo.autoflipx:=flipx;
  f_photo.autoflipy:=flipy;
  f_photo.appdir:=appdir;
  f_photo.maximgdir:=2;
  f_photo.ImgDirl[1]:='Lopam';
  f_photo.ImgDirl[2]:='LAC_LM';
  f_photo.autorotate[1]:=true;
  f_photo.autorotate[2]:=false;
  if f_photo.ListImg(currentname) then begin
     VMAShowmodal(f_photo,self);
     next_win:=f_photo.NextWindow;
     nextwindow;
  end;
end;

procedure Tf_pocketlun.rotation1Execute(Sender: TObject);
begin
if splash.visible then exit;
screen.Cursor:=crHourGlass;
try
  farside:=not farside;
  if farside then PhaseEffect:=false;
  btn06.Transparent:=not farside;
{//  btn09.down := farside;
  if farside then btn09.Color:=clBtnShadow
             else btn09.Color:=clBtnFace;}
  if farside then lrot:=180
             else lrot:=0;
  LoadTexture;
  LoadDB;
  f_search.StringGrid1.RowCount:=0;
  RefreshMoon;
finally
screen.Cursor:=crDefault;
end;
end;

Procedure Tf_pocketlun.SetLang;
var buf : string;
    i : integer;
begin
buf:=rsPhraseFormat;
if buf='PhraseFormatLatin' then PhraseFormat:=PhraseFormatLatin
  else if buf='PhraseFormatRussian' then PhraseFormat:=PhraseFormatRussian
  else PhraseFormat:=PhraseFormatEnglish;
ldeg:=rsDegreeSymbol;
lmin:=rsMinutesSymbol;
lsec:=rsSecondsSymbol;
label1.Caption:=rsVirtualMoonA;
close1.Caption:=rsQuit;
config1.Caption:=rsConfiguratio;
display1.Caption:=rsDisplay;
ephemeris1.Caption:=rsEphemeris;
calendar1.Caption:=rsCalendar;
Search1.Caption:=rsSearch;
help1.Caption:=rsHelp;
about1.Caption:=rsAbout+'...';
info1.Caption:=rsInformation;
notes1.Caption:=rsPersonalNote;
photos1.Caption:=rsPhotos;
comment1.Caption:=rsComments;
MenuItem1.Caption:=rsMenu;
end;

Procedure Tf_pocketlun.GetLanguage;
var inif: TMemIniFile;
begin
if fileexists(configfile) then begin
  inif:=TMeminifile.create(configfile);
  try
  language:=inif.ReadString('main','language','');
  finally
   inif.Free;
  end;
end;
end;

procedure Tf_pocketlun.FormCreate(Sender: TObject);
begin
initialized:=false;
Caption:=AppCaption;
decimalseparator:='.';
lockrefresh:=true;
compile_time:={$I %DATE%}+' '+{$I %TIME%};
DateLabel.Caption:=compile_time;
appdir:=slash(extractfilepath(paramstr(0)));
AppDataDir:=AppDataDirectory();
ForceDirectories(slash(AppDataDir)+slash('PocketLun'));
configfile:=slash(AppDataDir)+slash('PocketLun')+'config.ini';
homedir:=homedirectory();
prgfilesdir:=ProgramFilesDirectory();
if pos(prgfilesdir,appdir)>0 then begin
  privatedir:=slash(homedir)+slash('PocketLun');
end else begin
  privatedir:=appdir+slash('MyData');
end;
ForceDirectories(privatedir);
GetLanguage;
u_translation.appdir:=appdir;
language:=u_translation.translate(language);
SetLang;
Panel1.Align:=alTop;
Image1:= TChartDrawingControl.Create(Self);
Image1.Parent := Panel1;
Image1.Align:=alClient;
Image1.DoubleBuffered := true;
Image1.OnMouseDown:=@Image1MouseDown;
Image1.OnMouseMove:=@Image1MouseMove;
Image1.OnMouseUp:=@Image1MouseUp;
Image1.OnPaint:=@Image1Paint;
cbmp:=TBitmap.Create;
moona:=TBitmap.create;
image2d:=TBitmap.create;
moon:=TMoon.Create(self);
tzinfo:=TCdCTimeZone.Create;
tzinfo.LoadZoneTab(AppDir+slash('zoneinfo')+'zone.tab');
librl:=0;
librb:=0;
FindX:=0;
FindY:=0;
mr_Y:=0;
mr_M:=0;
mr_D:=0;
currentname:='';
zoombase:=5;
zoom:=1;
lrot:=0;
farside:=false;
TextureQuality:=1;
zoommax:=zoombase+2*TextureQuality;
flipx:=false;
flipy:=false;
inclination:=0;
phaseeffect:=false;
ShowLibrationMark:=true;
phaseumbra:=$00484848;
PhaseLine:=false;
librationeffect:=false;
showlabel:=true;
minilabel:=false;
LabelDensity:=500;
LabelColor:=clYellow;
LabelColorIndex:=11;
Labelsize:=12;
labelcenter:=false;
sidelist:='1';
ObsCountry:='';
ObsTZ:='Europe/Paris';
deffont:='Tahoma';
deffontsize:=8;
if max(screen.Height,screen.Width)>320 then deffontsize:=10;
geocentric:=false;
ObsLatitude:=49;
ObsLongitude:=-2;
ObsAltitude:=0;
LoadDefault;
SetButtons;
SplashPanel.Left:=(screen.Width-SplashPanel.Width) div 2;
SplashPanel.Top:=((screen.Height-SplashPanel.Height) div 2) - 2*TopPanel.Height;
end;

procedure Tf_pocketlun.LoadTexture(retry:boolean=true);
var jpg: TJpegImage;
{$ifdef debugtexture}
    f:textfile;
{$endif}
procedure wt(txt:string);
begin
{$ifdef debugtexture}
  append(f);
  writeln(f,txt);
  closefile(f);
{$endif}
end;
begin
try
if not splash.visible then screen.Cursor:=crHourGlass;
{$ifdef debugtexture}
  system.Assign(f,AppDir+'loadtexture.txt');
  rewrite(f);
  writeln(f,'Load texture');
  closefile(f);
{$endif}
if farside then imgfile:=appdir+slash('texture')+'moon_far'
           else imgfile:=appdir+slash('texture')+'moon_near';
imgfile:=imgfile+inttostr(TextureQuality)+'.jpg';
wt('texture='+imgfile);
moona.Width:=1;
moona.Height:=1;
image2d.Width:=1;
image2d.Height:=1;
jpg:=TJpegImage.Create;
wt('jpeg created');
if fileexists(imgfile) then jpg.LoadFromFile(imgfile)
   else raise Exception.Create('File not found: '+imgfile);
wt('jpeg loaded');
moona.Assign(jpg);
wt('Assign ok');
jpg.Free;
wt('jpeg free');
os:=moona.Width;
wt('imgwidth='+inttostr(os));
ox:=(36*os) div 1500;
oy:=(36*os) div 1500;
wt('resize img2d width');
image2d.Width:=moona.Width;
wt('resize img2d Height');
image2d.Height:=moona.Height;
wt('set position');
xpos:=image2d.Width div 2;
ypos:=image2d.Height div 2;
wt('set zoom');
zoommax:=zoombase+2*TextureQuality;
wt('zoommax='+formatfloat('0.0',zoommax));
zoom:=1;
screen.Cursor:=crDefault;
except
 on E: Exception do begin
   wt('Erreur:'+E.Message);
   ShowMessage('Erreur de chargement! Liberez de la mémoire!');
   TextureQuality:=1;
   screen.Cursor:=crDefault;
   jpg.free;
   moona.Width:=1;
   moona.Height:=1;
   image2d.Width:=1;
   image2d.Height:=1;
   if retry then LoadTexture(false)
      else halt;
 end;
end;
end;

procedure Tf_pocketlun.LoadDB;
var dbprefix: string;
begin
if farside then dbprefix:=appdir+slash('database')+'Farside_Named_'
           else dbprefix:=appdir+slash('database')+'Nearside_Named_';
dbfile:=dbprefix+trim(language)+'.db';
if fileexists(dbfile) then dbm.Use(dbfile)
   else begin
      dbfile:=dbprefix+'en.db';
      if fileexists(dbfile) then dbm.Use(dbfile)
         else raise Exception.Create('File not found: '+dbfile);
   end
end;

procedure Tf_pocketlun.Init;
begin
if initialized then exit;
f_ephemeris.onDateChange:=@EphemerisDateChange;
f_calendar.onUpdate:=@RefreshPhase;
f_search.dbm:=dbm;
application.ProcessMessages;
LoadTexture;
application.ProcessMessages;
LoadDB;
application.ProcessMessages;
f_notes.LoadDB(privatedir+'notes.csv');
panel1.Height:=clientHeight-TopPanel.Height;
Image1.Width:=panel1.Width;
Image1.Height:=panel1.Height;
cbmp.Width:=Image1.Width;
cbmp.Height:=Image1.Height;
zoommin:=Image1.Height/Image1.Width;
zoom:=1;
f_config.tzinfo:=tzinfo;
f_config.LoadCountry(AppDir+slash('zoneinfo')+'country.tab');
f_config.InitLanguage;
InitObservatoire;
InitDate;
application.ProcessMessages;
ComputeMoon;
application.ProcessMessages;
splash.visible:=false;
panel1.visible:=true;
if ObsCountry='' then begin
   ObsCountry:='FR';
   f_config.MenuItem1.Visible:=false;
   config1.Execute;
   f_config.MenuItem1.Visible:=true;
   InitObservatoire;
   InitDate;
   ComputeMoon;
   RefreshMoon;
   TopPanel.Invalidate;
end
else
   RefreshMoon;
lockrefresh:=false;
initialized:=true;
end;

procedure Tf_pocketlun.FormDestroy(Sender: TObject);
begin
lockrefresh:=true;
cbmp.Free;
moon.Free;
tzinfo.Free;
moona.free;
image2d.free;
image1.Free;
end;

procedure Tf_pocketlun.FormResize(Sender: TObject);
begin
if lockrefresh then exit;
Image1.Top:=TopPanel.Top+TopPanel.Height;
Image1.Left:=0;
Image1.Height:=clientHeight-2*TopPanel.Height;
cbmp.Width:=Image1.Width;
cbmp.Height:=Image1.Height;
zoommin:=Image1.Height/Image1.Width;
Refresh2DImage;
end;

procedure Tf_pocketlun.FormShow(Sender: TObject);
begin
MapLabel.SendToBack;  // hide debug text
screen.Cursor:=crDefault;
end;

procedure Tf_pocketlun.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
if splash.visible then exit;
lastX:=X;
lastY:=Y;
SkipLabel:=true;
end;

procedure Tf_pocketlun.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
procedure domove(x,y:integer);
var dx,dy: integer;
begin
  dx:=round(bx*(lastx-x));
  dy:=round(by*(lasty-y));
  Xpos:=Xpos+dx;
  Ypos:=Ypos+dy;
  if SkipIdent or (abs(dx/bx)>5)or(abs(dy/by)>5) then begin
     lastX:=X;
     lastY:=Y;
     SkipIdent:=true;
     SkipLabel:=true;
     Refresh2DImage;
  end;
end;
begin
if splash.visible then exit;
if Shift=[ssLeft] then begin  // move the chart
  if SkipMove then begin
     lastX:=X;
     lastY:=Y;
     SkipMove:=false;
     exit;
  end;
  moveX:=X;
  moveY:=Y;
  if lockmove then exit;
  lockmove:=true;
  domove(x,y);
  application.ProcessMessages;
  if (moveX<>X)or(moveY<>Y) then begin
    domove(moveX,moveY);
  end;
  lockmove:=false;
end;
end;

procedure Tf_pocketlun.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var xx,yy,l,b: double;
begin
if splash.visible then exit;
application.ProcessMessages;
SkipLabel:=false;
if SkipIdent then begin
   SkipIdent:=false;
   Refresh2DImage;
   exit;
end else begin
  SkipMove:=true;
  if lockrefresh then exit;
  Window2World(x,y,xx,yy);
  if InvProjMoon(2*xx,2*yy,librl,librb,l,b) then
    if SearchAtPos(l,b) then begin
       FindX:=xx;
       FindY:=yy;
       showpopup;
    end;
end;
end;

procedure Tf_pocketlun.ShowPopup;
var x,y: integer;
    ip,sp: TPoint;
begin
if currentname<>'' then begin
 PopUp_Name.Caption:=currentname;
 World2Window(FindX,FindY,x,y);
 ip.x:=x;
 ip.y:=y;
 sp:=Image1.ClientToScreen(ip);
 PopupMenu1.PopUp(sp.x,sp.y);
end;
end;

function Tf_pocketlun.SearchAtPos(l,b: double):boolean;
var mindist,d,l1,b1,deltab,deltal: double;
    rec: string;
    i : integer;
begin
mindist:=9999;
result:=false;
currentname:='';
rec:='0';
deltab:=2;
deltal:=deltab/cos(deg2rad*b);
dbm.query('select ID,LONGIN,LATIN from moon '+
            ' where '+
            ' LONGIN > '+formatfloat(d2,l-deltal)+
            ' and LONGIN < '+formatfloat(d2,l+deltal)+
            ' and LATIN > '+formatfloat(d2,b-deltab)+
            ' and LATIN < '+formatfloat(d2,b+deltab)+
            //' and DBN in ('+sidelist+')'+
            ' ');
for i:=0 to dbm.RowCount-1 do begin
  l1:=strtofloat(dbm.Results[i].Strings[1]);
  b1:=strtofloat(dbm.Results[i].Strings[2]);
  d:=angulardistance(l,b,l1,b1);
  if d<mindist then begin
     result:=true;
     mindist:=d;
     rec:=dbm.Results[i].Strings[0];
  end;
end;
if result then begin
   currentid:=rec;
   dbm.query('select NAME from moon where ID='+currentid+';');
   currentname:=dbm.Results[0].Strings[0];
   result:=dbm.RowCount>0;
end;
end;

procedure Tf_pocketlun.markpos(x,y: double);
var xx,yy: integer;
begin
 with cbmp.Canvas do begin
   world2window(x,y,xx,yy);
   Pen.Color:=clRed;
   Pen.Width:=1;
   Brush.style:=bsClear;
   Ellipse(xx-5,yy-5,xx+5,yy+5);
 end;
 Image1.Invalidate;
end;

function Tf_pocketlun.SearchName(n: string; center: boolean; mark:boolean=false):boolean;
var l1,b1,x,y: double;
begin
result:=false;
currentname:='';
dbm.Query('select ID,NAME,LONGIN,LATIN from moon '+
          ' where NAME = '''+trim(uppercase(n))+'''');
if dbm.RowCount > 0 then begin
   currentid:=dbm.Results[0].Strings[0];
   currentname:=dbm.Results[0].Strings[1];
   l1:=strtofloat(dbm.Results[0].Strings[2]);
   b1:=strtofloat(dbm.Results[0].Strings[3]);
   if (projMoon(l1,b1,librl,librb,x,y)) then begin
     FindX:=x;
     FindY:=y;
     if center then MoveCamera(x,y) ;
     if mark then markpos(x,y);
     result:=true;
   end;
end;
end;

Procedure Tf_pocketlun.GetDetail(lin:TStringList);
const b=' ';
var buf,buf2,txt : string;
    i:integer;
Function GetField(field:integer):string;
begin
 result:=AnsiToUtf8(dbm.Results[0].Strings[field]);
end;

begin
lin.clear;
buf:='select TYPE,PERIOD,LENGTHKM,WIDEKM,LENGTHMI,WIDEMI,HEIGHTM,HEIGHTFE,RAPPORT,GENERAL,SLOPES,WALLS,FLOOR,';
buf:=buf+'INTERESTC,MOONDAYS,MOONDAYM,PRINSTRU,LONGIC,LATIC,QUADRANT,AREA,RUKL,RUKLC,VISCARDY,HATFIELD,WESTFALL,WOOD,';
buf:=buf+'NAMEDETAIL,WORK,NATIONLITY,CENTURYC,COUNTRY,BIRTHPLACE,BIRTHDATE,DEATHPLACE,DEATHDATE,FACTS,NAMEORIGIN,LANGRENUS,HEVELIUS,RICCIOLI ';
buf:=buf+' from moon where ID='+currentid;
dbm.query(buf);
if dbm.RowCount>0 then begin
  lin.Add(rsType+':'+b+GetField(0));
  lin.Add(rsGeologicalPe+':'+b+GetField(1));
  lin.Add('');
  lin.Add(rsSize+':'); //Taille
  lin.Add(rsDimension+':'+b+GetField(2)+'x'+GetField(3)+rsKm+b
          +'/'+b+GetField(4)+'x'+GetField(5)+rsMi);
  buf:=GetField(6);
  buf2:=GetField(7);
  if buf=buf2 then txt:=rsHeight+':'+b+buf  // inconnue
  else begin
     txt:=rsHeight+b;
     val(buf,dummy,i);
     if i=0 then txt:=txt+buf+rsM+b+'/'+b;
     val(buf2,dummy,i);
     if i=0 then txt:=txt+buf2+rsFt;
  end;
  lin.Add(txt);
  lin.Add(rsHeightWideRa+':'+b+GetField(8));
  lin.Add('');
  lin.Add(rsDescription+':'); //Description
  if GetField(9)>'' then lin.Add(GetField(9));
  if GetField(10)>'' then lin.Add(GetField(10));
  if GetField(11)>'' then lin.Add(GetField(11));
  if GetField(12)>'' then lin.Add(GetField(12));
  
  lin.Add('');
  lin.Add(rsObservation+':'); //Observation
  lin.Add(rsInterest+':'+b+GetField(13));
  buf:=GetField(14);
  buf2:=GetField(15);
  if buf=buf2 then txt:=rsObservationP+':'+b+buf
              else txt:=rsObservationP+':'+b+buf+b+rsOr+b+buf2;
  lin.Add(txt);
  lin.Add(rsMinimalInstr+':'+b+GetField(16));
  lin.Add('');
  lin.Add(rsPosition+':'); //Position
  lin.Add(rsLongitude+':'+b+GetField(17));
  lin.Add(rsLatitude+':'+b+GetField(18));
  lin.Add(rsQuadrant+':'+b+GetField(19));
  lin.Add(rsArea+':'+b+GetField(20));
  lin.Add('');
  lin.Add(rsAtlas+':'); //Atlas
  lin.Add(rsRuklMap+':'+b+GetField(21)+' '+GetField(22));
  buf:=GetField(23);
  if trim(buf)>'' then lin.Add(rsViscardyPage+':'+b+buf);
  buf:=GetField(24) ;
  if trim(buf)>'' then lin.Add(rsHatfieldMap+':'+b+buf);
  buf:=GetField(25) ;
  if trim(buf)>'' then lin.Add(rsWestfallAtla+':'+b+buf);
  buf:=GetField(26) ;
  if trim(buf)>'' then lin.Add(rsCharlesWoodA+':'+b+buf);
  lin.Add('');
  lin.Add(rsNameOrigine+':'); //Origine
  lin.Add(rsDetailedName+':'+b+GetField(27));
  if (trim(GetField(28)+GetField(29))>'')and(trim(GetField(30)+GetField(31))>'') then begin
    case PhraseFormat of
      PhraseFormatEnglish : lin.Add(GetField(30)+b+
        GetField(29)+b+GetField(28)+b+rsBornIn+b+
        GetField(31));
      PhraseFormatLatin   : lin.Add(GetField(28)+b+
        GetField(29)+b+rsFrom+b+GetField(30)+b+rsBornIn+b+
        GetField(31));
      PhraseFormatRussian : lin.Add(GetField(29)+b+
        GetField(28)+b+GetField(30)+b+rsBornIn+b+
        GetField(31));
    end;
    lin.Add(rsBornAt+':'+b+GetField(32)+b+rsIn+b+
      GetField(33));
    lin.Add(rsDeadAt+':'+b+GetField(34)+b+rsIn+b+
      GetField(35));
  end;
  if (trim(GetField(36))<>'??')and(trim(GetField(36))<>'') then begin
    lin.Add('');
    lin.Add(rsImportantFac+':'+b+GetField(36));
  end;
  lin.Add('');
  lin.Add(rsNameAuthor+':'+b+GetField(37));
  lin.Add(rsNameByLangre+':'+b+GetField(38));
  lin.Add(rsNameByHeveli+':'+b+GetField(39));
  lin.Add(rsNameByRiccio+':'+b+GetField(40));
end;
end;

procedure Tf_pocketlun.MoveCamera(x,y :double);
var xx,yy : integer;
begin
if (x=0)and(y=0) then begin
  Xpos:=image2d.Width div 2;
  Ypos:=image2d.Height div 2;
end else begin
  world2window(x,y,xx,yy);
  Xpos:=ax+round(bx*xx);
  Ypos:=ay+round(by*yy);
end;
Refresh2dImage;
end;

Procedure Tf_pocketlun.ComputeMoon;
var jd0,st0,q : double;
    gpa,glibrb,gsunincl,glibrl: double;
begin
  st0:=0;
  moon.Moon(CurrentJD,MoonRa,MoonDec,MoonDist,MoonDkm,MoonDiam,Phase,MoonIllum);
  moon.MoonOrientation(CurrentJD,MoonRa,MoonDec,MoonDist,gpa,glibrb,gsunincl,glibrl);
  if not geocentric then begin
     jd0:=jd(CurYear,CurrentMonth,CurrentDay,0.0);
     st0:=SidTim(jd0,CurrentTime-Timezone,ObsLongitude);
     Paralaxe(st0,MoonDist,MoonRa,MoonDec,currentjd,ObsRoCosPhi,ObsRoSinPhi,MoonRa,MoonDec,q);
     MoonDiam:=MoonDiam/q;
     MoonDkm:=MoonDkm*q;
     MoonDist:=MoonDist*q;
  end;
  rad:=MoonRa; ded:=MoonDec;
  precession(jd2000,CurrentJD,rad,ded);
  moon.MoonOrientation(CurrentJD,MoonRa,MoonDec,MoonDist,MoonPA,librb,sunincl,librl);
  cphase:=Phase+glibrl;
  tphase:=Phase;
  tsunincl:=sunincl;
  colong:=rmod(90-tphase-glibrl+360,360);
  jd0:=jd(CurYear,1,1,0.0);
  moon.MoonPhases(CurYear+(CurrentJD-jd0)/365.25,nmjd,fqjd,fmjd,lqjd);
  lunaison:=CurrentJD-nmjd;
  if lunaison<0 then begin
     lunaison:=CurrentJD-moon.MoonPhase(floor(12.3685*(CurYear-2000-0.04+(CurrentJD-jd0)/365.25)));
  end;
if (not phaseeffect)or farside then begin
  Phase:=1;
  cphase:=1;
  sunincl:=0;
end;
librlong:=librl;
librlat:=librb;
if (not librationeffect) then begin
  librl:=0;
  librb:=0;
end;
end;

Procedure Tf_pocketlun.LibrationMark(libl,libb:double);
var a,x,y :double;
    xx,yy : integer;
begin
 if ShowLibrationMark then begin
  a:=arctan2(libb,-libl);
  X:=0.51*cos(a);
  Y:=0.51*sin(a);
  world2window(x,y,xx,yy);
  with cbmp.Canvas  do begin
     Pen.Color:=clRed;
     Pen.Width:=1;
     Brush.Color:=clRed;
     Brush.style:=bsSolid;
     Rectangle(xx-2,yy-2,xx+2,yy+2);
  end;
  Image1.Invalidate;
end;
end;

Procedure Tf_pocketlun.DrawPhase;
var ds1,ds2,n,ex1,ey1,xx,yy : integer;
    th,ex,ey,ci,si,sph : double;
    rvx:boolean;
    p : array[0..23] of Tpoint;
begin
{if currenteyepiece>0 then begin
   col:=$00202020;
   image2d.Canvas.Brush.Style:=bsSolid;
   image2d.Canvas.Brush.Color:=col;
   image2d.Canvas.FloodFill(1,1,clBlack,fsSurface);
end;}
if phaseeffect then begin
   ds1:=round(px*image2d.Width/2);
   ds2:=round(-cos(degtorad(cphase))*ds1);
   si:=sin(degtorad(-sunincl));
   ci:=cos(degtorad(-sunincl));
   th:=pi/2;
   xx:=image2d.Width div 2;
   yy:=image2d.Height div 2;
   if (phase<180) then sph:=-1
                  else sph:=1;
   if farside then rvx:=(phase>=180)
              else rvx:=(phase<180);
   if rvx then begin
      p[0]:=point(0,0);
      p[23]:=point(0,image2d.Height);
   end else begin
      p[0]:=point(image2d.Width,0);
      p[23]:=point(image2d.Width,image2d.Height);
   end;
   for n:=1 to 22 do begin
     ex:=sph*ds2*cos(th);
     ey:=ds1*sin(th);
     ey1:=round(ex*si - ey*ci)+yy ;
     ex1:=round(ex*ci + ey*si)+xx ;
     p[n]:=point(ex1,ey1);
     th:=th+0.15;
   end;
   if PhaseLine then begin
     image2d.Canvas.Pen.Color:=clBlack;
     image2d.Canvas.Pen.width:=2;
     image2d.Canvas.brush.style:=bsClear;
     image2d.Canvas.polygon(p);
   end else begin
     image2d.Canvas.Pen.Color:=clBlack;
     image2d.Canvas.Pen.width:=1;
     image2d.Canvas.brush.Color:=clBlack;
     image2d.Canvas.brush.style:=bsSolid;
     image2d.Canvas.polygon(p);
   end;
(*     ph:=tbitmap.create;
     try
     ph.width:=image2d.width;
     ph.height:=image2d.height;
     ph.canvas.brush.color:=clBlack;
     ph.canvas.fillrect(rect(0,0,ph.width,ph.height));
     ph.canvas.brush.color:=phaseumbra;
     ph.canvas.pen.color:=ph.canvas.brush.color;
     ph.canvas.polygon(p);
     BitmapSubstract(image2d,ph);
     finally
     ph.free;
     end; *)
end;
end;

procedure Tf_pocketlun.SetButtons;
begin
  btn01.Transparent:=true;
  btn02.Transparent:=true;
  btn03.Transparent:=true;
  btn04.Transparent:=not flipx;
  btn05.Transparent:=not flipy;
  btn06.Transparent:=not farside;
  btn07.Transparent:=not PhaseEffect;
  btn08.Transparent:=true;
  btn09.Transparent:=true;
  btn10.Transparent:=true;
{  btn05.down:=flipx;
  btn03.down:=flipy;
  btn06.down:=PhaseEffect;
  btn09.down := farside;}
{  if flipx then btn05.Color:=clBtnShadow
           else btn05.Color:=clBtnFace;
  if flipy then btn03.Color:=clBtnShadow
           else btn03.Color:=clBtnFace;
  if PhaseEffect then btn06.Color:=clBtnShadow
                 else btn06.Color:=clBtnFace;
  if farside then btn09.Color:=clBtnShadow
             else btn09.Color:=clBtnFace;}
end;

procedure Tf_pocketlun.SaveDefault(savelanguage: boolean=false);
var inif: TMemIniFile;
begin
inif:=TMeminifile.create(configfile);
try
  with inif do begin
    if savelanguage then WriteString('main','language',language);
    WriteString('main','font',deffont);
    WriteInteger('main','fontsize',deffontsize);
    WriteInteger('main','TextureQuality',TextureQuality);
    WriteInteger('main','LabelDensity',LabelDensity);
    WriteInteger('main','Labelsize',Labelsize);
    WriteInteger('main','LabelColor',LabelColor);
    WriteInteger('main','LabelColorIndex',LabelColorIndex);
    WriteBool('main','showlabel',showlabel);
    WriteBool('main','LabelCenter',LabelCenter);
    WriteBool('main','PhaseEffect',PhaseEffect);
    WriteBool('main','flipx',flipx);
    WriteBool('main','flipy',flipy);
    WriteBool('main','ShowLibrationMark',ShowLibrationMark);
    WriteFloat('main','ObsLatitude',ObsLatitude);
    WriteFloat('main','ObsLongitude',ObsLongitude);
    WriteString('main','ObsTZ',ObsTZ);
    WriteString('main','ObsCountry',ObsCountry);
    UpdateFile;
  end;
finally
   inif.Free;
end;
end;

procedure Tf_pocketlun.LoadDefault;
var inif: TMemIniFile;
begin
if fileexists(configfile) then begin
  inif:=TMeminifile.create(configfile);
  try
  with inif do begin
    deffont:=ReadString('main','font',deffont);
    deffontsize:=ReadInteger('main','fontsize',deffontsize);
    TextureQuality:=ReadInteger('main','TextureQuality',TextureQuality);
    LabelDensity:=ReadInteger('main','LabelDensity',LabelDensity);
    Labelsize:=ReadInteger('main','Labelsize',Labelsize);
    LabelColor:=ReadInteger('main','LabelColor',LabelColor);
    LabelColorIndex:=ReadInteger('main','LabelColorIndex',LabelColorIndex);
    showlabel:=ReadBool('main','showlabel',showlabel);
    LabelCenter:=ReadBool('main','LabelCenter',LabelCenter);
    PhaseEffect:=ReadBool('main','PhaseEffect',PhaseEffect);
    flipx:=ReadBool('main','flipx',flipx);
    flipy:=ReadBool('main','flipy',flipy);
    ShowLibrationMark:=ReadBool('main','ShowLibrationMark',ShowLibrationMark);
    ObsLatitude:=ReadFloat('main','ObsLatitude',ObsLatitude);
    ObsLongitude:=ReadFloat('main','ObsLongitude',ObsLongitude);
    ObsTZ:=ReadString('main','ObsTZ',ObsTZ);
    ObsCountry:=ReadString('main','ObsCountry',ObsCountry);
  end;
  finally
   inif.Free;
  end;
end;
end;


Procedure Tf_pocketlun.DrawLabel;
var l,b,l1,b1,deltab,deltal,x,y,w,wmin,wfact: double;
    xx,yy,j : integer;
    miniok : boolean;
    nom,let:string;
    ts: Tsize;
begin
if showlabel and (not SkipLabel) then begin
try
screen.Cursor:=crHourGlass;
xx:=cbmp.Width div 2;
yy:=cbmp.Height div 2;
Window2World(xx,yy,x,y);
if not InvProjMoon(2*x,2*y,librl,librb,l,b) then begin
   l:=librl;
   b:=librb;
   deltab:=90;
   deltal:=90;
end else begin
   deltab:=90/zoom;
   deltal:=deltab/cos(deg2rad*b);
end;
if abs(l)>90 then begin // face cachee
 l:=0;
 deltal:=180;
end;
if minilabel then wfact:=0.5 else wfact:=1;
w:=zoommax;
LabelDensity:=maxintvalue([100,LabelDensity]);
LabelDensity:=minintvalue([1000,LabelDensity]);
if (zoom>3)and(zoom>=w) then wmin:=-1
           else wmin:=min(650,1.5*(1100-LabelDensity)/(zoom*zoom));
dbm.Query('select NAME,LONGIN,LATIN,WIDEKM,WIDEMI,LENGTHKM,LENGTHMI from moon'+
           // ' where DBN in ('+sidelist+')'+
{            ' where LONGIN > '+formatfloat(d2,l-deltal)+
            ' and LONGIN < '+formatfloat(d2,l+deltal)+
            ' and LATIN > '+formatfloat(d2,b-deltab)+
            ' and LATIN < '+formatfloat(d2,b+deltab)+}

            ' where LONGIN between '+formatfloat(d2,l-deltal)+
            ' and '+formatfloat(d2,l+deltal)+
            ' and LATIN between '+formatfloat(d2,b-deltab)+
            ' and '+formatfloat(d2,b+deltab)+

            ' and (WIDEKM=0 or WIDEKM>='+formatfloat(d2,(wmin*wfact)/2.5)+')'+
            ' ;');
for j:=0 to dbm.RowCount-1 do begin
  l1:=strtofloat(dbm.Results[j].Strings[1]);
  b1:=strtofloat(dbm.Results[j].Strings[2]);
  w:=strtofloat(dbm.Results[j].Strings[3]);
  if w<=0 then w:=1.67*strtofloat(dbm.Results[j].Strings[4]);
  if (w>200)and(abs(l1)>90) then w:=2.5*w; // moins de grosse formation face cachee
  if w<(wmin*wfact) then continue;
  nom:=dbm.Results[j].Strings[0];
  if projMoon(l1,b1,librl,librb,x,y) then begin
    nom:=trim(nom);
    if minilabel then begin
      miniok:=true;
      if copy(nom,1,6)='DOMES ' then miniok:=false;
      if copy(nom,1,5)='DOME ' then miniok:=false;
      if copy(nom,1,6)='DORSA ' then miniok:=false;
      if copy(nom,1,5)='RIMA ' then miniok:=false;
      let:=trim(copy(nom,length(nom)-1,2));
      if miniok and(length(let)=1)and(let>='A')and(let<='Z') then begin
         nom:=let;
      end else begin
        if w<(wmin) then continue;
      end;
    end;
    world2window(x,y,xx,yy);
    begin
    if (xx>0)and(yy>0)and(xx<cbmp.Width)and(yy<cbmp.Height)
//     and((currenteyepiece=0)or( sqrt(Intpower(form1.image1.Width/2-xx,2)+Intpower(form1.image1.Height/2-yy,2))<0.475*form1.image1.Width ))
     then with cbmp.Canvas do begin
      Font.Color:=LabelColor;
      Font.name:=deffont;
      Font.Style:=[fsBold];
      Font.Height:=Labelsize;
      Brush.style:=bsClear;
      ts:=TextExtent(nom);
      yy:=yy-ts.cy div 2;
      if labelcenter then begin
         xx:=xx-ts.cx div 2;
      end else begin
         xx:=xx+ts.cy div 4;
         nom:='-'+nom;
      end;
      textout(xx,yy,nom);
    end;
    end;
  end;
end;
finally
screen.Cursor:=crDefault;
Image1.Invalidate;
end;
end;
end;


procedure Tf_pocketlun.Image1Paint(Sender: TObject);
begin
Image1.Canvas.CopyMode:=cmSrcCopy;
Image1.Canvas.Draw(0,0,cbmp); // draw bitmap to screen
inherited Paint;
end;

procedure TChartDrawingControl.Paint;
begin
  inherited Paint;
end;

initialization
  {$I pu_pocketlun.lrs}

end.

