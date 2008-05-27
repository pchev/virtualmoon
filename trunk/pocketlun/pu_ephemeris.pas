unit pu_ephemeris;
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
  ExtCtrls, StdCtrls, Grids, ExtDlgs, Buttons, jdcalendar;

type

  { Tf_ephemeris }

  Tf_ephemeris = class(TForm)
    BtnCalendar: TButton;
    BtnFor2: TButton;
    BtnFor1: TButton;
    BtnBack1: TButton;
    BtnBack2: TButton;
    btnNow: TButton;
    Button1: TButton;
    LabelDate: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    StringGrid1: TStringGrid;
    Title: TPanel;
    procedure BtnBack1Click(Sender: TObject);
    procedure BtnBack2Click(Sender: TObject);
    procedure BtnCalendarClick(Sender: TObject);
    procedure BtnFor1Click(Sender: TObject);
    procedure BtnFor2Click(Sender: TObject);
    procedure btnNowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
  private
    { private declarations }
    FNextWindow : Tvma_window;
    FDateChange: TNotifyEvent;
    Fjd: double;
    procedure SetDateChange(Sender: TObject);
  public
    { public declarations }
    procedure SetLang;
    property NextWindow : Tvma_window read FNextWindow;
    property JDD : double read Fjd write Fjd;
    property onDateChange: TNotifyEvent read FDateChange write FDateChange;
  end; 

var
  f_ephemeris: Tf_ephemeris;

implementation

uses pu_setdate, u_astro;

{ Tf_ephemeris }

procedure Tf_ephemeris.SetLang;
begin
Title.Caption:=rsEphemeris;
btnNow.Caption:=rsNow;
MenuItem1.Caption:=rsMenu;
MenuItem2.Caption:=rsConfiguratio;
MenuItem3.Caption:=rsDisplay;
MenuItem6.Caption:=rsCalendar;
MenuItem7.Caption:=rsSearch;
MenuItem8.Caption:=rsHelp;
MenuItem9.Caption:=rsAbout+'...';
MenuItem10.Caption:=rsMap;
end;

procedure Tf_ephemeris.SetDateChange(Sender: TObject);
begin
  Fjd:=f_setdate.JDD;
  if assigned(FDateChange) then FDateChange(self);
end;

procedure Tf_ephemeris.BtnCalendarClick(Sender: TObject);
begin
 f_setdate.JDD:=Fjd;
 f_setdate.Font.Name:=Font.Name;
 f_setdate.Font.Size:=Font.Size;
 VMAShowmodal(f_setdate,self);
 if f_setdate.ModalResult=mrOK then
    SetDateChange(Sender);
end;

procedure Tf_ephemeris.BtnBack2Click(Sender: TObject);
begin
  Fjd:=Fjd-1;
  if assigned(FDateChange) then FDateChange(self);
end;

procedure Tf_ephemeris.BtnBack1Click(Sender: TObject);
begin
  Fjd:=Fjd-(1/24);
  if assigned(FDateChange) then FDateChange(self);
end;

procedure Tf_ephemeris.BtnFor1Click(Sender: TObject);
begin
  Fjd:=Fjd+(1/24);
  if assigned(FDateChange) then FDateChange(self);
end;

procedure Tf_ephemeris.BtnFor2Click(Sender: TObject);
begin
  Fjd:=Fjd+1;
  if assigned(FDateChange) then FDateChange(self);
end;

procedure Tf_ephemeris.btnNowClick(Sender: TObject);
var y,mm,d,h,n,s,ms : word;
begin
decodedate(now,y,mm,d);
decodetime(now,h,n,s,ms);
Fjd:=jd(y,mm,d,h+n/60+s/3600);
if assigned(FDateChange) then FDateChange(self);
end;

procedure Tf_ephemeris.FormCreate(Sender: TObject);
begin
  SetLang;
end;

procedure Tf_ephemeris.FormShow(Sender: TObject);
begin
  FNextWindow:=w_none;
  f_setdate.onDateChange:=@SetDateChange;
end;

// main menu
procedure Tf_ephemeris.MenuItem2Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config;
end;

procedure Tf_ephemeris.MenuItem3Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config_display;
end;

procedure Tf_ephemeris.MenuItem4Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_tools;
end;

{procedure Tf_ephemeris.MenuItem5Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_ephemeris
end;}

procedure Tf_ephemeris.MenuItem6Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_calendar;
end;

procedure Tf_ephemeris.MenuItem7Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_search;
end;

procedure Tf_ephemeris.MenuItem8Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_help;
end;

procedure Tf_ephemeris.MenuItem9Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_about;
end;

procedure Tf_ephemeris.MenuItem10Click(Sender: TObject);
begin
  modalresult:=mrOK;
  FNextWindow:=w_none;
end;

initialization
  {$I pu_ephemeris.lrs}

end.

