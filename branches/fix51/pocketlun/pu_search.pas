unit pu_search;
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

uses u_translation, wince_func, passqlite,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Grids, Buttons;

type

  { Tf_search }

  Tf_search = class(TForm)
    Button1: TButton;
    ButtonSearch: TButton;
    EditSearch: TEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Selection1: TMenuItem;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    Title: TPanel;
    procedure ButtonSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure Selection1Click(Sender: TObject);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    FNextWindow : Tvma_window;
    procedure SetSelection(row:integer);
  public
    { public declarations }
    dbm: TLiteDB;
    searchstring,searchname,SelectedObject: string;
    procedure SetLang;
    property NextWindow : Tvma_window read FNextWindow;
  end;

var
  f_search: Tf_search;

implementation

{ Tf_search }

procedure Tf_search.SetLang;
begin
Title.Caption:=rsSearch;
Selection1.Caption:=rsSelection;
MenuItem1.Caption:=rsMenu;
MenuItem2.Caption:=rsConfiguratio;
MenuItem3.Caption:=rsDisplay;
MenuItem5.Caption:=rsEphemeris;
MenuItem6.Caption:=rsCalendar;
MenuItem8.Caption:=rsHelp;
MenuItem9.Caption:=rsAbout+'...';
MenuItem10.Caption:=rsMap;
end;

procedure Tf_search.SetSelection(row:integer);
var sel: TGridRect;
begin
 if (row<=StringGrid1.RowCount) and (trim(StringGrid1.Cells[0,row])<>'') then begin
    SelectedObject:=StringGrid1.Cells[0,row];
    sel.Top:=row;
    sel.Bottom:=row;
    sel.Left:=0;
    sel.Right:=0;
    StringGrid1.Selection:=sel;
 end;
end;

procedure Tf_search.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var col,row:integer;
begin
SelectedObject:='';
 StringGrid1.MouseToCell(X, Y, Col, Row);
 if (col>=0) and (row>=0) then
     SetSelection(row);
end;

procedure Tf_search.FormShow(Sender: TObject);
begin
searchname:='';
StringGrid1.DefaultColWidth:=StringGrid1.ClientWidth;
ButtonSearchClick(Sender);
EditSearch.SetFocus;
EditSearch.SelStart:=999;
end;

procedure Tf_search.ButtonSearchClick(Sender: TObject);
var i: integer;
    buf: string;
begin
if EditSearch.Text<>searchstring then begin
  screen.Cursor:=crHourGlass;
  searchstring:=EditSearch.Text;
  dbm.query('select NAME from moon where NAME like '''+trimleft(searchstring)+'%''');
  StringGrid1.RowCount:=dbm.RowCount;
  for i:=0 to dbm.RowCount-1 do begin
    buf:=dbm.Results[i].Strings[0];
    StringGrid1.Cells[0,i]:=buf;
  end;
  if StringGrid1.RowCount>0 then SetSelection(StringGrid1.Selection.Top);
  screen.Cursor:=crDefault;
end;
EditSearch.SetFocus;
end;

procedure Tf_search.FormCreate(Sender: TObject);
begin
  SetLang;
end;

procedure Tf_search.Selection1Click(Sender: TObject);
begin
 searchname:=SelectedObject;
 modalresult:=mrOK;
end;

// main menu

procedure Tf_search.MenuItem2Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config;
end;

procedure Tf_search.MenuItem3Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config_display;
end;

procedure Tf_search.MenuItem4Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_tools;
end;

procedure Tf_search.MenuItem5Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_ephemeris
end;

procedure Tf_search.MenuItem6Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_calendar;
end;

{procedure Tf_search.MenuItem7Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_search;
end;}

procedure Tf_search.MenuItem8Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_help;
end;

procedure Tf_search.MenuItem9Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_about;
end;

procedure Tf_search.MenuItem10Click(Sender: TObject);
begin
  modalresult:=mrOK;
  FNextWindow:=w_none;
end;

initialization
  {$I pu_search.lrs}

end.

