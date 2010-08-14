unit pu_info;
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

uses  u_translation, wince_func,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, Buttons;

type

  { Tf_info }

  Tf_info = class(TForm)
    Button1: TButton;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Title: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
  private
    { private declarations }
    FNextWindow : Tvma_window;
  public
    { public declarations }
    procedure SetLang;
    property NextWindow : Tvma_window read FNextWindow;
  end;

var
  f_info: Tf_info;

implementation

{ Tf_info }

procedure Tf_info.SetLang;
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

procedure Tf_info.FormShow(Sender: TObject);
begin
  FNextWindow:=w_none;
end;

procedure Tf_info.FormCreate(Sender: TObject);
begin
  SetLang;
end;

// main menu

procedure Tf_info.MenuItem2Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config;
end;

procedure Tf_info.MenuItem3Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config_display;
end;

procedure Tf_info.MenuItem4Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_tools;
end;

procedure Tf_info.MenuItem5Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_ephemeris
end;

procedure Tf_info.MenuItem6Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_calendar;
end;

procedure Tf_info.MenuItem7Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_search;
end;

procedure Tf_info.MenuItem8Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_help;
end;

procedure Tf_info.MenuItem9Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_about;
end;

procedure Tf_info.MenuItem10Click(Sender: TObject);
begin
  modalresult:=mrOK;
  FNextWindow:=w_none;
end;

initialization
  {$I pu_info.lrs}

end.

