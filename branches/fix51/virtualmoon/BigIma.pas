unit BigIma;
{
Copyright (C) 2003 Patrick Chevalley

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

{$MODE Delphi}
{$H+}

interface

uses Math, FileUtil,
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls, ToolWin, LResources;

type
  TBigImaForm = class(TForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    Image1: TImage;
    Label1: TLabel;
    Timer1: TTimer;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ToolBar1Resize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  imagewidth,imageheight,labelstart : integer;
  public
    { Public declarations }
    zoom : double;
    titre,labeltext: string;
    Procedure LoadImage(f : string);
    Procedure ZoomN(n:double);
    Procedure Zoomplus;
    Procedure Zoommoins;
    Procedure Init;
  end;

var
  BigImaForm: TBigImaForm;

implementation

uses u_util;


Procedure TbigImaform.LoadImage(f : string);
begin
 image1.autosize:=false;
 image1.stretch:=false;
 image1.Proportional:=true;
 image1.picture.LoadFromFile(systoutf8(f));
 imagewidth:=image1.picture.Width;
 imageheight:=image1.picture.Height;
 zoomN(zoom);
end;

Procedure TbigImaform.ZoomN(n:double);
var x : double;
begin
   zoom:=n;
   if zoom>2 then zoom:=2;
   if zoom<-2 then zoom:=-2;
   x:=Power(2,zoom);
   image1.autosize:=false;
   image1.stretch:=true;
   image1.Width:=round(x*imageWidth);
   image1.Height:=round(x*imageHeight);
   Caption:=titre+' x'+formatfloat('0.#',Power(2,zoom));
   ClientWidth:=trunc(minvalue([0.8*Screen.Width,image1.Width]));
   ClientHeight:=trunc(minvalue([0.8*Screen.Height,image1.Height+toolbar1.height]));
   formpos(self,self.Left,self.Top);
end;

Procedure TbigImaform.Zoomplus;
begin
if (zoom<2) then begin
   zoom:=trunc(zoom)+1;
   ZoomN(zoom);
end;
end;

Procedure TbigImaform.Zoommoins;
begin
if (zoom>-2) then begin
   zoom:=trunc(zoom)-1;
   ZoomN(zoom);
end;
end;

procedure TBigImaForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
if key=chr(27) then Close;
if (key='+') then Zoomplus;
if (key='-') then Zoommoins;
end;

procedure TBigImaForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   timer1.Enabled:=false;
   image1.autosize:=true;
   image1.stretch:=false;
end;

procedure TBigImaForm.Init;
begin
Caption:=titre+' x'+formatfloat('0.#',Power(2,zoom));
labelstart:=0;
label1.Caption:=labeltext;
if labeltext<>'' then timer1.Enabled:=true
                 else timer1.Enabled:=false;
end;

procedure TBigImaForm.FormShow(Sender: TObject);
begin
Init;
end;

procedure TBigImaForm.ToolButton2Click(Sender: TObject);
begin
Zoomplus;
end;

procedure TBigImaForm.ToolButton3Click(Sender: TObject);
begin
Zoommoins;
end;

procedure TBigImaForm.ToolButton1Click(Sender: TObject);
begin
close;
end;

procedure TBigImaForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var x : double;
begin
if wheeldelta>0 then x:=zoom+0.5
                else x:=zoom-0.5;
ZoomN(x);
end;

procedure TBigImaForm.ToolBar1Resize(Sender: TObject);
begin
Label1.Width:=ToolBar1.Width-Label1.Left;
end;

procedure TBigImaForm.Timer1Timer(Sender: TObject);
begin
inc(labelstart,2);
if labelstart>length(labeltext) then labelstart:=0;
label1.Caption:=copy(labeltext+' '+labeltext,labelstart,9999);
end;

procedure TBigImaForm.FormCreate(Sender: TObject);
begin
zoom:=0;
titre:='';
labeltext:='';
end;

initialization
  {$i BigIma.lrs}

end.
