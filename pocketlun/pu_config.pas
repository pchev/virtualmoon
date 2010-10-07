unit pu_config;
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

uses u_translation, wince_func, cu_tz, u_astro, FileUtil,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, StdCtrls, ComCtrls, EditBtn, FileCtrl;

type

  { Tf_config }

  Tf_config = class(TForm)
    Button1: TButton;
    Label7: TLabel;
    LanguageList: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LoD: TUpDown;
    LaD: TUpDown;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    LoM: TUpDown;
    LaM: TUpDown;
    LoS: TUpDown;
    LaS: TUpDown;
    Title: TPanel;
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LanguageListChange(Sender: TObject);
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
    Fisocode,Flanguage : string;
    countrycode: TStringList;
    procedure UpdateTzList;
    function GetLat: double;
    procedure SetLat(value:double);
    function GetLon: double;
    procedure SetLon(value:double);
    procedure SetObsCountry(value:string);
    procedure Setlanguage(value:string);
  public
    { public declarations }
    tzinfo: TCdCTimeZone;
    ObsTZ :string;
    procedure SetLang;
    procedure InitLanguage;
    procedure LoadCountry(fn:string);
    property NextWindow : Tvma_window read FNextWindow;
    property ObsLat: double read GetLat write SetLat;
    property ObsLon: double read GetLon write SetLon;
    property ObsCountry: string read Fisocode write SetObsCountry;
    property language: string read Flanguage write Setlanguage;
  end;

var
  f_config: Tf_config;

implementation

{ Tf_config }

procedure Tf_config.SetLang;
begin
Title.Caption:=rsConfiguratio;
Label3.Caption:=rsObservatory;
Label2.Caption:=rsLongitude;
Label1.Caption:=rsLatitude;
Label5.Caption:=rsCountry;
Label6.Caption:=rsTimeZone;
Label4.Caption:=rsLanguage;
Label7.Caption:=rsLanguageChan;
MenuItem1.Caption:=rsMenu;
//MenuItem2.Caption:=rsConfiguratio;
MenuItem3.Caption:=rsDisplay;
MenuItem5.Caption:=rsEphemeris;
MenuItem6.Caption:=rsCalendar;
MenuItem7.Caption:=rsSearch;
MenuItem8.Caption:=rsHelp;
MenuItem9.Caption:=rsAbout+'...';
MenuItem10.Caption:=rsMap;
end;

procedure Tf_config.InitLanguage;
const blank=' ';
var f: textfile;
    dir,buf,buf1,buf2: string;
begin
LanguageList.clear;
dir:=slash(appdir)+slash('language');
try
AssignFile(f,dir+'pocketlun.lang');
Reset(f);
repeat
  Readln(f,buf);
  buf1:=words(buf,'',1,1);
//  buf2:=CondUTF8Decode(words(buf,'',2,1));
  buf2:=words(buf,'',2,1);
  if fileexists(dir+'pocketlun.'+buf1+'.po') then begin
     LanguageList.items.Add(buf1+blank+'-'+blank+buf2);
  end;
until eof(f);
CloseFile(f);
except
end;
LanguageList.ItemIndex:=0;
end;

procedure Tf_config.Setlanguage(value:string);
var i: integer;
begin
Flanguage:=value;
for i:=0 to LanguageList.Items.Count-1 do begin
  if Flanguage=words(LanguageList.Items[i],'',1,1) then LanguageList.itemindex:=i;
end;
end;

function Tf_config.GetLat: double;
var s: double;
begin
s:=sgn(LaD.Position);
result:=s*(abs(LaD.Position)+(LaM.Position/60)+(LaS.Position/3600));
end;

procedure Tf_config.SetLat(value:double);
var d,m,s: integer;
begin
DEToStri(value,d,m,s);
LaD.Position:=d;
LaM.Position:=m;
LaS.Position:=s;
end;

function Tf_config.GetLon: double;
var s: double;
begin
s:=sgn(LoD.Position);
result:=s*(abs(LoD.Position)+(LoM.Position/60)+(LoS.Position/3600));
end;

procedure Tf_config.SetLon(value:double);
var d,m,s: integer;
begin
DEToStri(value,d,m,s);
LoD.Position:=d;
LoM.Position:=m;
LoS.Position:=s;
end;

procedure Tf_config.FormShow(Sender: TObject);
begin
  FNextWindow:=w_none;
  combobox2.Width:=ClientWidth-combobox2.Left;
  combobox3.Width:=ClientWidth-combobox3.Left;
end;

procedure Tf_config.LanguageListChange(Sender: TObject);
begin
 Flanguage:=words(LanguageList.Text,'',1,1);
end;

procedure Tf_config.LoadCountry(fn:string);
var f: textfile;
    buf: string;
    i: integer;
    rec: TStringList;
const tab  = chr(9);
procedure SplitRec(buf,sep:string; var arg: TStringList);
var i,l:integer;
begin
arg.clear;
l:=length(sep);
while pos(sep,buf)<>0 do begin
 for i:=1 to length(buf) do begin
  if copy(buf,i,l) = sep then begin
      arg.add(copy(buf,1,i-1));
      delete(buf,1,i-1+l);
      break;
  end;
 end;
end;
arg.add(buf);
end;
begin
if fileexists(fn) then begin
  rec:=TStringList.create;
  ComboBox2.Clear;
  countrycode.Clear;
  Filemode:=0;
  system.assign(f,fn);
  reset(f);
  repeat
    readln(f,buf);
    buf:=trim(buf);
    if buf='' then continue;
    if buf[1]='#' then continue;
    SplitRec(buf,#9,rec);
    if rec.Count<3 then continue;
    countrycode.Add(rec[1]);
    ComboBox2.Items.Add(rec[2]);
  until eof(f);
  CloseFile(f);
  rec.Free;
end;
end;

procedure Tf_config.SetObsCountry(value:string);
var i: integer;
begin
Fisocode:=value;
for i:=0 to countrycode.Count-1 do begin
   if Fisocode=countrycode[i] then begin
      Combobox2.ItemIndex:=i;
      break;
   end;
end;
UpdateTzList;
end;

procedure Tf_config.UpdateTzList;
var
  i,j: Integer;
  buf: string;
begin
ComboBox3.clear;
j:=0;
for i:=0 to tzinfo.ZoneTabCnty.Count-1 do begin
  if tzinfo.ZoneTabCnty[i]=Fisocode then begin
     buf:=tzinfo.ZoneTabZone[i];
     ComboBox3.Items.Add(buf);
     if (j=0)or(tzinfo.ZoneTabZone[i]=ObsTZ) then ComboBox3.ItemIndex:=j;
     inc(j);
  end;
end;
ObsTZ:=ComboBox3.Text;
combobox2.Width:=ClientWidth-combobox2.Left;
combobox3.Width:=ClientWidth-combobox3.Left;
end;

procedure Tf_config.ComboBox2Change(Sender: TObject);
begin
Fisocode:=countrycode[ComboBox2.ItemIndex];
UpdateTzList;
end;

procedure Tf_config.ComboBox3Change(Sender: TObject);
begin
  ObsTZ:=ComboBox3.Text;
end;

procedure Tf_config.FormCreate(Sender: TObject);
begin
  SetLang;
  countrycode:=TStringList.Create;
end;

procedure Tf_config.FormDestroy(Sender: TObject);
begin
  countrycode.Free;
end;

procedure Tf_config.FormResize(Sender: TObject);
begin
  combobox2.Width:=ClientWidth-combobox2.Left;
  combobox3.Width:=ClientWidth-combobox3.Left;
end;

// main menu

procedure Tf_config.MenuItem2Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config;
end;

procedure Tf_config.MenuItem3Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config_display;
end;

procedure Tf_config.MenuItem4Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_tools;
end;

procedure Tf_config.MenuItem5Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_ephemeris
end;

procedure Tf_config.MenuItem6Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_calendar;
end;

procedure Tf_config.MenuItem7Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_search;
end;

procedure Tf_config.MenuItem8Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_help;
end;

procedure Tf_config.MenuItem9Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_about;
end;

procedure Tf_config.MenuItem10Click(Sender: TObject);
begin
  modalresult:=mrOK;
  FNextWindow:=w_none;
end;

initialization
  {$I pu_config.lrs}

end.

