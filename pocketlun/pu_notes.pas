unit pu_notes;
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

uses u_translation, wince_func, mlb2,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, StdCtrls;

type

  { Tf_notes }

  Tf_notes = class(TForm)
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
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
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
    dbnotes : TMlb2;
    notesrow: integer;
    notesok,notesedited: boolean;
    dbfile,currentname:string;
  public
    { public declarations }
    procedure SetLang;
    procedure LoadDB(fn: string);
    procedure GetNotes(n: string);
    procedure SaveNotes;
    property NextWindow : Tvma_window read FNextWindow;
  end;

var
  f_notes: Tf_notes;

implementation

{ Tf_notes }

procedure Tf_notes.SetLang;
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

procedure Tf_notes.LoadDB(fn: string);
begin
dbfile:=fn;
if not dbnotes.LoadFromFile(dbfile) then begin
  dbnotes.AddField('NAME');
  dbnotes.AddField('NOTES');
  dbnotes.SaveToCSVFile(dbfile);
end;
dbnotes.GoFirst;
end;

procedure Tf_notes.GetNotes(n: string);
begin
Title.Caption:=rsNotes+': '+n;
currentname:=n;
memo1.Clear;
notesok:=false;
notesedited:=false;
notesrow:=-1;
dbnotes.GoFirst;
notesok:=dbnotes.MatchData('NAME', '=', trim(uppercase(n)));
if not notesok then notesok:=dbnotes.SeekData('NAME', '=', trim(uppercase(n)));
if notesok then begin
   notesrow:=dbnotes.GetPosition;
   memo1.Text:=dbnotes.GetData('NOTES');
   notesedited:=false;
end;
end;

procedure Tf_notes.SaveNotes;
begin
notesedited:=false;
if notesok then begin
  dbnotes.Go(notesrow);
  dbnotes.SetData('NOTES',memo1.Text);
end else begin
  if currentname='' then exit;
  dbnotes.Gofirst;
  notesok:=dbnotes.MatchData('NAME','>=',currentname);
  if not notesok then notesok:=dbnotes.SeekData('NAME','>=',currentname);
  if not notesok then dbnotes.golast;
  if dbnotes.GetData('NAME')<>currentname then dbnotes.InsertRow(not notesok);
  dbnotes.SetData('NAME',currentname);
  dbnotes.SetData('NOTES',memo1.Text);
  notesrow:=dbnotes.GetPosition;
  notesok:=true;
end;
screen.Cursor:=crHourGlass;
try
  dbnotes.SaveToCSVFile(dbfile);
finally
screen.Cursor:=crDefault;
end;
end;

procedure Tf_notes.Memo1Change(Sender: TObject);
begin
  notesedited:=true;
end;

procedure Tf_notes.Button1Click(Sender: TObject);
begin
if notesedited then SaveNotes;
modalresult:=mrOK;
end;

procedure Tf_notes.FormShow(Sender: TObject);
begin
  FNextWindow:=w_none;
end;

procedure Tf_notes.FormCreate(Sender: TObject);
begin
  dbnotes := TMlb2.Create;
  SetLang;
end;

procedure Tf_notes.FormDestroy(Sender: TObject);
begin
  dbnotes.free;
end;

// main menu

procedure Tf_notes.MenuItem2Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config;
end;

procedure Tf_notes.MenuItem3Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config_display;
end;

procedure Tf_notes.MenuItem4Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_tools;
end;

procedure Tf_notes.MenuItem5Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_ephemeris
end;

procedure Tf_notes.MenuItem6Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_calendar;
end;

procedure Tf_notes.MenuItem7Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_search;
end;

procedure Tf_notes.MenuItem8Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_help;
end;

procedure Tf_notes.MenuItem9Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_about;
end;

procedure Tf_notes.MenuItem10Click(Sender: TObject);
begin
  modalresult:=mrOK;
  FNextWindow:=w_none;
end;

initialization
  {$I pu_notes.lrs}

end.

