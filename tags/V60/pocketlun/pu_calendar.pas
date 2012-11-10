unit pu_calendar;
{
Copyright (C) 2007 Patrick Chevalley

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

{$mode objfpc}{$H+}

interface

uses  u_translation, wince_func,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Buttons;

type

  { Tf_calendar }

  Tf_calendar = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    LabelNL: TLabel;
    DateNL: TLabel;
    DateLQ: TLabel;
    LabelLQ: TLabel;
    DatePQ: TLabel;
    LabelPQ: TLabel;
    DatePL: TLabel;
    LabelPL: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    ScrollBar1: TScrollBar;
    Title: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image4MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
  private
    { private declarations }
    FNextWindow : Tvma_window;
    FUpdate: TNotifyevent;
  public
    { public declarations }
    phaseoffset: integer;
    PhaseSelect: integer;
    procedure SetLang;
    property NextWindow : Tvma_window read FNextWindow;
    property onUpdate : TNotifyevent read FUpdate write FUpdate;
  end;

var
  f_calendar: Tf_calendar;

implementation

{ Tf_calendar }

procedure Tf_calendar.SetLang;
begin
Title.Caption:=rsPhaseCalenda;
LabelNL.Caption:=rsNewMoon;
LabelPQ.Caption:=rsFirstQuarter;
LabelPL.Caption:=rsFullMoon;
LabelLQ.Caption:=rsLastQuarter;
MenuItem1.Caption:=rsMenu;
MenuItem2.Caption:=rsConfiguratio;
MenuItem3.Caption:=rsDisplay;
MenuItem5.Caption:=rsEphemeris;
MenuItem7.Caption:=rsSearch;
MenuItem8.Caption:=rsHelp;
MenuItem9.Caption:=rsAbout+'...';
MenuItem10.Caption:=rsMap;
end;

procedure Tf_calendar.FormShow(Sender: TObject);
begin
  PhaseSelect:=0;
  FNextWindow:=w_none;
end;

procedure Tf_calendar.FormCreate(Sender: TObject);
begin
  SetLang;
end;

procedure Tf_calendar.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
PhaseSelect:=1;
modalresult:=mrOK;
end;

procedure Tf_calendar.Image2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
PhaseSelect:=2;
modalresult:=mrOK;
end;

procedure Tf_calendar.Image3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
PhaseSelect:=3;
modalresult:=mrOK;
end;

procedure Tf_calendar.Image4MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
PhaseSelect:=4;
modalresult:=mrOK;
end;

procedure Tf_calendar.ScrollBar1Change(Sender: TObject);
begin
if ScrollBar1.Position<4 then begin
   dec(phaseoffset);
   if assigned(FUpdate) then FUpdate(self);
end;
if ScrollBar1.Position>4 then begin
   inc(phaseoffset);
   if assigned(FUpdate) then FUpdate(self);
end;
ScrollBar1.Position:=4;
end;

// main menu
procedure Tf_calendar.MenuItem2Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config;
end;

procedure Tf_calendar.MenuItem3Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config_display;
end;

procedure Tf_calendar.MenuItem4Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_tools;
end;

procedure Tf_calendar.MenuItem5Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_ephemeris
end;

{procedure Tf_calendar.MenuItem6Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_calendar;
end;}

procedure Tf_calendar.MenuItem7Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_search;
end;

procedure Tf_calendar.MenuItem8Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_help;
end;

procedure Tf_calendar.MenuItem9Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_about;
end;

procedure Tf_calendar.MenuItem10Click(Sender: TObject);
begin
  modalresult:=mrOK;
  FNextWindow:=w_none;
end;

initialization
  {$I pu_calendar.lrs}

end.

