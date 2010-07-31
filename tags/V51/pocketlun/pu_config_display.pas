unit pu_config_display;
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
  ExtCtrls, StdCtrls, ComCtrls, Buttons, ColorBox;

type

  { Tf_config_display }

  Tf_config_display = class(TForm)
    Button1: TButton;
    CenterLabel: TCheckBox;
    ColorBox1: TColorBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MemLabel: TLabel;
    LabelDensity: TTrackBar;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    TextSize: TTrackBar;
    LabelSize: TTrackBar;
    Title: TPanel;
    TextureQuality: TTrackBar;
    procedure ColorBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure TextSizeChange(Sender: TObject);
  private
    { private declarations }
    FNextWindow : Tvma_window;
  public
    { public declarations }
    procedure SetLang;
    property NextWindow : Tvma_window read FNextWindow;
  end;

var
  f_config_display: Tf_config_display;

implementation

{ Tf_config_display }

procedure Tf_config_display.SetLang;
begin
Title.Caption:=rsDisplaySetti;
Label1.Caption:=rsTextureQuali;
Label2.Caption:=rsTextSize;
Label3.Caption:=rsLabelSize;
Label4.Caption:=rsLabelDensity;
Label5.Caption:=rsLabelColor;
CenterLabel.Caption:=rsCenterLabelO;
MenuItem1.Caption:=rsMenu;
MenuItem2.Caption:=rsConfiguratio;
MenuItem5.Caption:=rsEphemeris;
MenuItem6.Caption:=rsCalendar;
MenuItem7.Caption:=rsSearch;
MenuItem8.Caption:=rsHelp;
MenuItem9.Caption:=rsAbout+'...';
MenuItem10.Caption:=rsMap;
end;

procedure Tf_config_display.FormShow(Sender: TObject);
var mem: double;
    p,m: integer;
begin
  FNextWindow:=w_none;
  ColorBox1Change(Sender);
  mem:=MemoryAvailable()/1024/1024;
  p:=TextureQuality.Position;
  m:=TextureQuality.Max;
  case p of
  1: if mem>10 then m:=3
     else if mem>5 then m:=2
     else m:=1;
  2: if mem>5 then m:=3
     else m:=2;
  3: m:=3;
  end;
  if p>m then p:=m;
  TextureQuality.Max:=m;
  TextureQuality.Min:=1;
  TextureQuality.Position:=p;
  TextureQuality.Enabled:=(m>1);
  if m<3 then begin
     MemLabel.Caption:=rsNotEnoughMem;
     MemLabel.Font.Color:=clRed;
  end
     else MemLabel.Caption:='';
end;

procedure Tf_config_display.ColorBox1Change(Sender: TObject);
begin
 ColorBox1.Color:=ColorBox1.Colors[colorbox1.ItemIndex];
 ColorBox1.Font.Color:=$FFFFFF xor ColorBox1.Color;
 Title.SetFocus;
end;

procedure Tf_config_display.FormCreate(Sender: TObject);
begin
  SetLang;
end;

procedure Tf_config_display.TextSizeChange(Sender: TObject);
begin
  Font.Size:=TextSize.Position;
end;

// main menu

procedure Tf_config_display.MenuItem2Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config;
end;

{procedure Tf_config_display.MenuItem3Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config_display;
end;}

procedure Tf_config_display.MenuItem4Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_tools;
end;

procedure Tf_config_display.MenuItem5Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_ephemeris
end;

procedure Tf_config_display.MenuItem6Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_calendar;
end;

procedure Tf_config_display.MenuItem7Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_search;
end;

procedure Tf_config_display.MenuItem8Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_help;
end;

procedure Tf_config_display.MenuItem9Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_about;
end;

procedure Tf_config_display.MenuItem10Click(Sender: TObject);
begin
  modalresult:=mrOK;
  FNextWindow:=w_none;
end;

initialization
  {$I pu_config_display.lrs}

end.

