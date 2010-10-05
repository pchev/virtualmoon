unit pu_photo;
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

uses u_translation, wince_func, lazjpg, math,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, StdCtrls, ComCtrls, ActnList;

type

  { Tf_photo }

  Tf_photo = class(TForm)
    btn1: TImage;
    btn2: TImage;
    btn3: TImage;
    btn5: TImage;
    btn6: TImage;
    btn4: TImage;
    btn7: TImage;
    zoomplus1: TAction;
    flipy1: TAction;
    flipx1: TAction;
    zoommin1: TAction;
    zoommax1: TAction;
    zoommoins1: TAction;
    ImgList1: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    imglst: TListBox;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    Panel1: TPanel;
    Title: TLabel;
    Image1: TImage;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    TitleBar: TPanel;
    ToolBar1: TToolBar;
    procedure flipxExecute(Sender: TObject);
    procedure flipyExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure imglstMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImgListExecute(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure zoommaxExecute(Sender: TObject);
    procedure zoomminExecute(Sender: TObject);
    procedure zoomminusExecute(Sender: TObject);
    procedure zoomplusExecute(Sender: TObject);
  private
    { private declarations }
    FNextWindow : Tvma_window;
    imgbmp: TBitmap;
    xpos,ypos,ax,ay,lastx,lasty,movex,movey: integer;
    zoom,zoommin,zoommax,bx,by: double;
    flipx,flipy: boolean;
    lockmove: boolean;
    procedure DrawImg;
    procedure SetButtons;
  public
    { public declarations }
    autoflipx,autoflipy: boolean;
    maximgdir: integer;
    imgdirl: array [1..10] of string;
    autorotate: array [1..10] of boolean;
    appdir: string;
    procedure SetLang;
    function ListImg(nom:string):boolean;
//    function LoadImg(nom:string):boolean;
    procedure LoadJpegImg(nom,imgfile:string);
    property NextWindow : Tvma_window read FNextWindow;
  end;

var
  f_photo: Tf_photo;

implementation

{ Tf_photo }

procedure Tf_photo.SetLang;
begin
MenuItem1.Caption:=rsMenu;
MenuItem2.Caption:=rsConfiguratio;
MenuItem3.Caption:=rsDisplay;
MenuItem5.Caption:=rsEphemeris;
MenuItem6.Caption:=rsCalendar;
MenuItem7.Caption:=rsSearch;
MenuItem8.Caption:=rsHelp;
MenuItem9.Caption:=rsAbout+'...';
MenuItem10.Caption:=rsMap;
end;

Function Tf_photo.ListImg(nom:string):boolean;
var desc,dir,buf,nom2 : string;
    r : integer;
    f : Tsearchrec;
Procedure SearchImages;
var i : integer;
begin
imglst.clear;
for i:=1 to maximgdir do begin
  if imgdirl[i]='' then continue;
  dir:=appdir+Slash(imgdirl[i]);
  r:=findfirst(dir+nom+'.jpg',0,f);
  while r=0 do begin
    imglst.Items.Add(imgdirl[i]+' : '+ChangeFileExt(f.Name,''));
    r:=findnext(f);
  end;
  findclose(f);
  r:=findfirst(dir+nom+'_*.jpg',0,f);
  while r=0 do begin
    imglst.Items.Add(imgdirl[i]+' : '+ChangeFileExt(f.Name,''));
    r:=findnext(f);
  end;
  findclose(f);
end;
end;
//-------ListImg
begin
nom:=trim(nom);
r:=length(nom)-1;
if copy(nom,r,1)=' ' then begin
  buf:=copy(nom,r+1,1);
  if ((buf>='a')and(buf<='z'))or((buf>='A')and(buf<='Z')) then nom2:=copy(nom,1,r-1);
end else nom2:='';
SearchImages;
if (imglst.Count=0)and(nom2>'') then begin
  nom:=nom2;
  SearchImages;
end;
case imglst.Count of
0 : begin
    imglst.visible:=false;
    result:=false;
    end;
1 : begin
     imglst.visible:=false;
     buf:=ImgLst.Items[0];
     r:=pos(':',buf);
     dir:=appdir+trim(copy(buf,1,r-1));
     nom:=trim(copy(buf,r+1,9999));
     LoadJpegImg(nom,slash(dir)+nom+'.jpg');
     result:=true;
    end;
else begin
     imglst.visible:=true;
     result:=true;
     end;
end;
end;

procedure Tf_photo.imglstMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Apoint : Tpoint;
    i,p : integer;
    buf,dir,nom : string;
begin
  APoint.X := X;
  APoint.Y := Y;
  i := ImgLst.ItemAtPos(APoint, True);
  if i>=0 then begin
     buf:=ImgLst.Items[i];
     p:=pos(':',buf);
     dir:=appdir+trim(copy(buf,1,p-1));
     nom:=trim(copy(buf,p+1,9999));
     imglst.Visible:=false;
     LoadJpegImg(nom,slash(dir)+nom+'.jpg');
     Drawimg;
  end;
end;

procedure Tf_photo.LoadJpegImg(nom,imgfile:string);
var  jpg: TJpegImage;
     i: integer;
     autorot: boolean;
begin
   try
   screen.Cursor:=crHourGlass;
   for i:=1 to maximgdir do begin
     if imgdirl[i]='' then continue;
     if pos(PathDelim+imgdirl[i]+PathDelim,imgfile)>0 then begin
        autorot:=autorotate[i];
        break;
     end;
   end;
   if autorot then begin
      flipx:=autoflipx;
      flipy:=autoflipy;
   end else begin
      flipx:=false;
      flipy:=false;
   end;
   jpg:=TJpegImage.Create;
   if fileexists(imgfile) then jpg.LoadFromFile(imgfile)
      else raise Exception.Create('File error: '+imgfile);
   imgbmp.Assign(jpg);
   jpg.Free;
   Title.Caption:=nom;
   zoom:=1;
   zoommin:=0.8;
   zoommax:=max(1.5*imgbmp.Width/Image1.width,1.5*imgbmp.Height/Image1.Height);
   Xpos:=imgbmp.Width div 2;
   Ypos:=imgbmp.Height div 2;
   finally
   screen.Cursor:=crDefault;
   end;
end;

procedure Tf_photo.SetButtons;
begin
btn1.Transparent:=true;
btn2.Transparent:=true;
btn3.Transparent:=true;
btn4.Transparent:=true;
btn5.Transparent:=not flipx;
btn6.Transparent:=not flipy;
btn7.Transparent:=true;
end;

procedure Tf_photo.DrawImg;
var x1,y1,x2,y2,dx,dy,x0,y0,xd1,yd1,xd2,yd2,x: integer;
    winratio,imgratio,r: double;
begin
if zoom=0 then exit;
SetButtons;
winratio:=Image1.width/Image1.height;
imgratio:=imgbmp.width/imgbmp.height;
if imgratio>1 then begin
  dx:=round(min(imgbmp.width/2,imgbmp.width/zoom/2));
  dy:=round(min(imgbmp.height/2,imgbmp.width/zoom/2/winratio));
  r:=dx/dy;
  xd2:=Image1.Width;
  yd2:=round(min(Image1.Height,xd2/r));
end else begin
  dy:=round(min(imgbmp.height/2,imgbmp.height/zoom/2));
  dx:=round(min(imgbmp.width/2,imgbmp.height*winratio/zoom/2));
  r:=dy/dx;
  yd2:=Image1.Height;
  xd2:=Image1.Width;
  xd2:=round(min(Image1.Width,yd2/r));
end;
x0:=imgbmp.width-dx-dx;
y0:=imgbmp.height-dy-dy;
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
bx:=2*dx/xd2;
by:=2*dy/yd2;
if flipx then bx:=-bx;
if flipy then by:=-by;
if xd2>=Image1.Width then
   xd1:=0
else begin
   xd1:=(Image1.Width-xd2) div 2;
   xd2:=xd1+xd2;
end;
if yd2>=Image1.Height then
   yd1:=0
else begin
   yd1:=(Image1.Height-yd2) div 2;
   yd2:=yd1+yd2;
end;
Image1.picture.bitmap.Width:=Image1.Width;
Image1.picture.bitmap.Height:=Image1.Height;
Image1.picture.bitmap.canvas.brush.color:=clBlack;
Image1.picture.bitmap.canvas.fillrect(rect(0,0,Image1.picture.bitmap.width,Image1.picture.bitmap.height));
Image1.picture.bitmap.canvas.CopyRect(rect(xd1,yd1,xd2,yd2),imgbmp.Canvas,rect(x1,y1,x2,y2));
Image1.Refresh;
end;

procedure Tf_photo.zoomminusExecute(Sender: TObject);
begin
if zoom>zoommin then begin
  zoom:=zoom/1.3;
  DrawImg;
end;
end;

procedure Tf_photo.zoomplusExecute(Sender: TObject);
begin
if zoom<zoommax then begin
  zoom:=zoom*1.3;
  DrawImg;
end;
end;

procedure Tf_photo.zoommaxExecute(Sender: TObject);
begin
  zoom:=zoommax/1.5;
  DrawImg;
end;

procedure Tf_photo.zoomminExecute(Sender: TObject);
begin
  zoom:=1;
  DrawImg;
end;

procedure Tf_photo.flipxExecute(Sender: TObject);
begin
  flipx:= not flipx;
  DrawImg;
end;

procedure Tf_photo.flipyExecute(Sender: TObject);
begin
  flipy:= not flipy;
  DrawImg;
end;

procedure Tf_photo.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
lastX:=X;
lastY:=Y;
end;

procedure Tf_photo.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
procedure domove(x,y:integer);
var dx,dy: integer;
begin
  dx:=round(bx*(lastx-x));
  dy:=round(by*(lasty-y));
  Xpos:=Xpos+dx;
  Ypos:=Ypos+dy;
  lastX:=X;
  lastY:=Y;
  DrawImg;
end;
begin
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

procedure Tf_photo.ImgListExecute(Sender: TObject);
begin
  imglst.Visible:=not imglst.Visible;
end;

procedure Tf_photo.FormShow(Sender: TObject);
begin
  FNextWindow:=w_none;
  FormResize(Sender);
  SetButtons;
end;

procedure Tf_photo.FormCreate(Sender: TObject);
begin
 imgbmp:=TBitmap.Create;
 SetLang;
end;

procedure Tf_photo.FormDestroy(Sender: TObject);
begin
imgbmp.Free;
end;

procedure Tf_photo.FormResize(Sender: TObject);
begin
Image1.Top:=TitleBar.Top+TitleBar.Height;
Image1.Left:=0;
Image1.Height:=clientHeight-2*TitleBar.Height;
Image1.picture.bitmap.Width:=Image1.Width;
Image1.picture.bitmap.Height:=Image1.Height;
Title.Top:=Image1.Top;
Title.Left:=Image1.Left;
DrawImg;
end;

// main menu

procedure Tf_photo.MenuItem2Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config;
end;

procedure Tf_photo.MenuItem3Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config_display;
end;

procedure Tf_photo.MenuItem4Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_tools;
end;

procedure Tf_photo.MenuItem5Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_ephemeris
end;

procedure Tf_photo.MenuItem6Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_calendar;
end;

procedure Tf_photo.MenuItem7Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_search;
end;

procedure Tf_photo.MenuItem8Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_help;
end;

procedure Tf_photo.MenuItem9Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_about;
end;

procedure Tf_photo.MenuItem10Click(Sender: TObject);
begin
  modalresult:=mrOK;
  FNextWindow:=w_none;
end;


initialization
  {$I pu_photo.lrs}

end.

