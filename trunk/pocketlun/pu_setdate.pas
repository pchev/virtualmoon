unit pu_setdate;
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

uses u_translation, wince_func,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, jdcalendar, Buttons, StdCtrls, ComCtrls;

type

  { Tf_setdate }

  Tf_setdate = class(TForm)
    BtnOK: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    JDMonthlyCalendar1: TJDMonthlyCalendar;
    Label1: TLabel;
    Panel1: TPanel;
    Hours: TUpDown;
    Minutes: TUpDown;
    Seconds: TUpDown;
    procedure BtnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FDateChange: TNotifyEvent;
    procedure Setjd(value: double);
    function Getjd: double;
  public
    { public declarations }
    procedure SetLang;
    property JDD : double read Getjd write Setjd;
    property onDateChange: TNotifyEvent read FDateChange write FDateChange;
  end; 

var
  f_setdate: Tf_setdate;

implementation

{ Tf_setdate }

procedure Tf_setdate.SetLang;
var labels: TDatesLabelsArray;
begin
Label1.Caption:=rsTime;
labels.mon:=rsMon;
labels.thu:=rsTue;
labels.wed:=rsWed;
labels.tue:=rsThu;
labels.fri:=rsFri;
labels.sat:=rsSat;
labels.sun:=rsSun;
labels.jd:=rsJulianDay;
labels.today:='-';
JDMonthlyCalendar1.labels:=labels;
end;

procedure Tf_setdate.Setjd(value: double);
var x: double;
    h,m,s: integer;
begin
x:=trunc(value-0.5)+0.5;
JDMonthlyCalendar1.JD:=x;
x:=(value-x)*24;
h:=trunc(x);
m:=trunc((x-h)*60);
s:=round( (((x-h)*60)-m)*60 );
Hours.Position:=h;
Minutes.Position:=m;
Seconds.Position:=s;
end;

function Tf_setdate.Getjd: double;
var h: double;
begin
h:=Hours.Position+(Minutes.Position/60)+(Seconds.Position/3600);
result:=JDMonthlyCalendar1.JD+h/24;
end;

procedure Tf_setdate.BtnCancelClick(Sender: TObject);
begin

end;

procedure Tf_setdate.FormCreate(Sender: TObject);
begin
SetLang;
end;

initialization
  {$I pu_setdate.lrs}

end.

