unit pu_photo;
{
Copyright (C) 2007 Patrick Chevalley

http://www.astrosurf.com/astropc
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
{
 Photo display
}

{$mode objfpc}{$H+}

interface

uses u_translation, u_bitmap, math, IniFiles,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, StdCtrls, ComCtrls, ExtDlgs, uniqueinstance;

type

  { Tf_photo }

  Tf_photo = class(TForm)
    ImageList1: TImageList;
    Image1: TImage;
    ResizeTimer: TTimer;
    SavePictureDialog1: TSavePictureDialog;
    HScrollBar: TScrollBar;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    VScrollBar: TScrollBar;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure HScrollBarChange(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure flipxbtnClick(Sender: TObject);
    procedure flipybtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure ResizeTimerTimer(Sender: TObject);
    procedure ToolButton13Click(Sender: TObject);
    procedure ToolButton14Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure VScrollBarChange(Sender: TObject);
    procedure zoommaxClick(Sender: TObject);
    procedure zoomminClick(Sender: TObject);
    procedure zoomminusClick(Sender: TObject);
    procedure zoomplusClick(Sender: TObject);
  private
    { private declarations }
    oribmp: TBitmap;
    xpos,ypos,ax,ay,lastx,lasty,movex,movey: integer;
    zoom,zoommin,zoommax,bx,by: double;
    flipx,flipy: boolean;
    lockmove: boolean;
    Fimage: string;
    Fautorotate: boolean;
    lum2,con2: integer;
    rot2: double;
    Fparam: string;
    FonSaveParam: TNotifyEvent;
    procedure SetLang;
    procedure SetParam(value:string);
    procedure SetImage(value:string);
    procedure DrawImg;
    procedure LoadJpegImg(imgfile:string);
  public
    { public declarations }
    imgbmp: TBitmap;
    lum,con: integer;
    rot: double;
    autoflipx,autoflipy: boolean;
    property param: string read Fparam write SetParam;
    property image: string read Fimage write SetImage;
    property autorotate: boolean read Fautorotate write Fautorotate;
    property onSaveParam: TNotifyEvent read FonSaveParam write FonSaveParam;
  end;

var
  f_photo: Tf_photo;

implementation

{ Tf_photo }

procedure Tf_photo.SetLang;
begin
ToolButton1.Hint:=rsZoom+' +';
ToolButton2.Hint:=rsZoom+' -';
ToolButton3.Hint:=rsRotateCounte;
ToolButton4.Hint:=rsRotateClockw;
ToolButton5.Hint:=rsBrighter;
ToolButton6.Hint:=rsDarker;
ToolButton7.Hint:=rsIncreaseCont;
ToolButton8.Hint:=rsReduceContra;
ToolButton9.Hint:=rsVerticalMirr;
ToolButton10.Hint:=rsHorizontalMi;
ToolButton11.Hint:=rsAdjustToWind;
ToolButton12.Hint:=rsFullSize;
ToolButton13.Hint:=rsResetDefault;
ToolButton14.Hint:=rsSaveSetting;
end;

procedure Tf_photo.SetParam(value:string);
var inif: TMemIniFile;
    section: string;
begin
if value<>Fparam then begin
  Fparam:=value;
  if fileexists(Fparam) then begin
     inif:=TMeminifile.create(Fparam);
     section:='image';
     con:=inif.ReadInteger(section,'Contrast',0);
     lum:=inif.ReadInteger(section,'Luminosity',0);
     rot:=inif.ReadFloat(section,'Rotation',0);
     inif.Free;
  end
  else begin
     con:=0;
     lum:=0;
     rot:=0;
  end;
end;
end;

procedure Tf_photo.SetImage(value:string);
begin
if value<>Fimage then begin
  rot2:=0; con2:=0; lum2:=0;
  LoadJpegImg(value);
  Fimage:=value;
  if Fautorotate then begin
    flipx:=autoflipx;
    flipy:=autoflipy;
    ToolButton10.down:=flipx;
    ToolButton9.down:=flipy;
  end;
  DrawImg;
end;
end;

procedure Tf_photo.LoadJpegImg(imgfile:string);
var  jpg: TJpegImage;
     i: integer;

begin
   try
   screen.Cursor:=crHourGlass;
   if Fautorotate then begin
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
   oribmp.Assign(imgbmp);
   jpg.Free;
   zoom:=1;
   zoommin:=min(imgbmp.Width/Image1.width,imgbmp.Height/Image1.Height);
   if zoommin>1 then zoommin:=1;
   zoommax:=max(1.5*imgbmp.Width/Image1.width,1.5*imgbmp.Height/Image1.Height);
   zoommax:=max(4*zoommin,zoommax);
   if zoommax<1 then zoommax:=1;
   Xpos:=imgbmp.Width div 2;
   Ypos:=imgbmp.Height div 2;
   finally
   screen.Cursor:=crDefault;
   end;
end;

procedure Tf_photo.DrawImg;
var x1,y1,x2,y2,dx,dy,x0,y0,xd1,yd1,xd2,yd2,x,fbw,fbh: integer;
    winratio,imgratio,r,z: double;
    FB: TBitmap;
begin
if zoom=0 then exit;
Image1.picture.bitmap.Width:=Image1.Width;
Image1.picture.bitmap.Height:=Image1.Height;
if (rot<>rot2) or (lum<>lum2) or (con<>con2) then begin
  BitmapRotation(oribmp,imgbmp,rot,false);
  BitmapLumCon(imgbmp,lum,con);
  rot2:=rot;
  lum2:=lum;
  con2:=con;
end;
winratio:=Image1.width/Image1.height;
imgratio:=imgbmp.width/imgbmp.height;
if imgratio>1 then begin
//  dx:=round(min(imgbmp.width/2,imgbmp.width/zoom/2));
//  dy:=round(min(imgbmp.height/2,imgbmp.width/zoom/2/winratio));
  dx:=round(imgbmp.width/zoom/2);
  dy:=round(imgbmp.width/zoom/2/winratio);
  r:=dx/dy;
  xd2:=Image1.Width;
  yd2:=round(min(Image1.Height,xd2/r));
end else begin
//  dy:=round(min(imgbmp.height/2,imgbmp.height/zoom/2));
//  dx:=round(min(imgbmp.width/2,imgbmp.height*winratio/zoom/2));
  dy:=round(imgbmp.height/zoom/2);
  dx:=round(imgbmp.height*winratio/zoom/2);
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
z:=abs((xd2-xd1)/(x2-x1));
Image1.picture.bitmap.Width:=Image1.Width;
Image1.picture.bitmap.Height:=Image1.Height;
Image1.picture.bitmap.canvas.brush.color:=clBlack;
Image1.picture.bitmap.canvas.fillrect(rect(0,0,Image1.picture.bitmap.width,Image1.picture.bitmap.height));
if z>=1 then begin
  Image1.picture.bitmap.canvas.CopyRect(rect(xd1,yd1,xd2,yd2),imgbmp.Canvas,rect(x1,y1,x2,y2));
end else begin
  FB:=TBitmap.Create;
  fbw:=round(abs(xd2-xd1)/z);
  fbh:=round(abs(yd2-yd1)/z);
  FB.Width:=fbw;
  FB.Height:=fbh;
  FB.canvas.brush.color:=clBlack;
  FB.canvas.fillrect(rect(0,0,fbw,fbh));
  FB.Canvas.CopyRect(Rect(0, 0, fbw, fbh ),imgbmp.Canvas,rect(x1,y1,x2,y2));
  BitmapResize(FB,FB,z,true);
  Image1.picture.bitmap.Assign(FB);
  FB.Free;
end;
HScrollBar.SetParams(XPos,dx,max(dx,imgbmp.width-dx),dx);
VScrollBar.SetParams(YPos,dy,max(dy,imgbmp.height-dy),dy);
Image1.Refresh;
Toolbutton10.down:=flipx;
Toolbutton9.down:=flipy;
StatusBar1.Panels[0].Text:=inttostr(oribmp.Width)+' x '+inttostr(oribmp.Width);
StatusBar1.Panels[1].Text:=rsZoom+' : '+FormatFloat('0.0', z);
end;

procedure Tf_photo.zoomminusClick(Sender: TObject);
begin
if zoom>zoommin then begin
  zoom:=zoom/1.3;
  DrawImg;
end;
end;

procedure Tf_photo.zoomplusClick(Sender: TObject);
begin
if zoom<zoommax then begin
  zoom:=zoom*1.3;
  DrawImg;
end;
end;

procedure Tf_photo.zoommaxClick(Sender: TObject);
begin
  zoom:=zoommax;
//  zoom:=zoommax/1.5;
  DrawImg;
end;

procedure Tf_photo.zoomminClick(Sender: TObject);
begin
  zoom:=1;
//  zoom:=zoommin;
  DrawImg;
end;

procedure Tf_photo.flipxbtnClick(Sender: TObject);
begin
  flipx:= ToolButton10.down;
  DrawImg;
end;

procedure Tf_photo.flipybtnClick(Sender: TObject);
begin
  flipy:= ToolButton9.down;
  DrawImg;
end;

procedure Tf_photo.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
lastX:=X;
lastY:=Y;
screen.Cursor:=crHandPoint;
end;

procedure Tf_photo.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
screen.Cursor:=crDefault;
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
if Shift=[ssLeft] then begin
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

procedure Tf_photo.HScrollBarChange(Sender: TObject);
begin
if XPos<>HScrollBar.Position then begin
  XPos:=HScrollBar.Position;
  DrawImg;
end;
end;

procedure Tf_photo.VScrollBarChange(Sender: TObject);
begin
if YPos<>VScrollBar.Position then begin
  YPos:=VScrollBar.Position;
  DrawImg;
end;
end;

procedure Tf_photo.FormShow(Sender: TObject);
begin
  FormResize(Sender);
end;

procedure Tf_photo.FormCreate(Sender: TObject);
begin
 SetLang;
 imgbmp:=TBitmap.Create;
 oribmp:=TBitmap.Create;
 lum:=0;
 con:=0;
 rot:=0;
end;

procedure Tf_photo.FormDestroy(Sender: TObject);
begin
imgbmp.Free;
oribmp.Free;
end;

procedure Tf_photo.FormResize(Sender: TObject);
begin
ResizeTimer.Enabled:=false;
ResizeTimer.Enabled:=true;
end;

procedure Tf_photo.ResizeTimerTimer(Sender: TObject);
begin
ResizeTimer.Enabled:=false;
zoommax:=max(1.5*imgbmp.Width/Image1.width,1.5*imgbmp.Height/Image1.Height);
zoommax:=max(4*zoommin,zoommax);
if zoom>zoommax then zoom:=zoommax;
DrawImg;
end;

procedure Tf_photo.ToolButton13Click(Sender: TObject);
begin
  rot:=0;
  lum:=0;
  con:=0;
  DrawImg;
end;

procedure Tf_photo.ToolButton14Click(Sender: TObject);
var inif: TMemIniFile;
    section: string;
begin
if (lum=0)and(con=0)and(rot=0) then
 DeleteFile(Fparam)
else begin
 inif:=TMeminifile.create(Fparam);
 section:='image';
 inif.WriteInteger(section,'Contrast',con);
 inif.WriteInteger(section,'Luminosity',lum);
 inif.WriteFloat(section,'Rotation',rot);
 inif.UpdateFile;
 inif.Free;
end;
if assigned(FonSaveParam) then FonSaveParam(self);
end;
{
var  jpg: TJpegImage;
begin
 SavePictureDialog1.FileName:=ExtractFileName(Fimage);
 if SavePictureDialog1.Execute then begin
   jpg:=TJpegImage.Create;
   jpg.Assign(imgbmp);
   jpg.SaveToFile(SavePictureDialog1.FileName);
   jpg.Free;
 end;
end;
}

procedure Tf_photo.ToolButton3Click(Sender: TObject);
begin
 rot:=rot+pi/12;
 DrawImg;
end;

procedure Tf_photo.ToolButton4Click(Sender: TObject);
begin
 rot:=rot-pi/12;
 DrawImg;
end;

procedure Tf_photo.ToolButton5Click(Sender: TObject);
begin
 lum:=lum+10;
 DrawImg;
end;

procedure Tf_photo.ToolButton6Click(Sender: TObject);
begin
 lum:=lum-10;
 DrawImg;
end;

procedure Tf_photo.ToolButton7Click(Sender: TObject);
begin
 con:=con+10;
 DrawImg;
end;

procedure Tf_photo.ToolButton8Click(Sender: TObject);
begin
 con:=con-10;
 DrawImg;
end;

initialization
  {$I pu_photo.lrs}

end.

