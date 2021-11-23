unit config;

{$MODE Delphi}
{$H+}
{
Copyright (C) 2003 Patrick Chevalley

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
interface

uses
{$ifdef mswindows}
  LCLIntf,
{$endif}
  u_translation,
  Math, u_constant, cu_tz,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, Inifiles, Grids,
  CheckLst, LResources;

type

  { Tf_config }

  Tf_config = class(TForm)
    Button1: TSpeedButton;
    Button4: TSpeedButton;
    ComboBoxCountry: TComboBox;
    ComboBoxTZ: TComboBox;
    Edit3: TEdit;
    Label62: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Bevel7: TBevel;
    Label16: TLabel;
    Bevel8: TBevel;
    Label23: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ComboBoxCountryChange(Sender: TObject);
    procedure ComboBoxTZChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    countrycode: TStringList;
    FPrinterDialog : TNotifyEvent;
    procedure UpdateTzList;
  public
    newlang: string;
    tzinfo: TCdCTimeZone;
    obscountry,obstz : string;
    TexturePath,HistTex: string;
    HistN: array[0..maxlevel] of integer;
    texturefn: TStringList;
    TextureList: TStringList;
    TextureChanged: boolean;
    procedure SetObsCountry(value:string);
    procedure LoadCountry(fn:string);
    procedure Setlang;
  property onPrinterDialog : TNotifyEvent read FPrinterDialog write FPrinterDialog;
  end;

var
  f_config: Tf_config;

implementation

{$R config.lfm}

uses u_util;

procedure Tf_config.Setlang;
begin
      Caption := rsConfiguratio;
      label1.Caption := rsLatitude;
      label2.Caption := rsLongitude;
      label62.Caption := rsAltitude;
      label4.Caption := rsLang;
      Button1.Caption := rsOK;
      Button4.Caption := rsCancel;
      Label16.Caption := rsObservatory;
      combobox1.items[0] := rsN;
      combobox1.items[1] := rsS;
      combobox1.ItemIndex := 0;
      combobox2.items[0] := rsE;
      combobox2.items[1] := rsW;
      combobox2.ItemIndex := 0;
      Label23.Caption := rsDateTime;
      Label35.Caption:=rsCountry;
      Label34.Caption:=rsTimeZone;
end;

Function GetLangCode(buf:string):string;
var p : integer;
begin
p:=pos(' ',buf);
result:=trim(copy(buf,1,p-1));
end;

function GBRadioChecked(GB: TGroupBox): integer;
var i: integer;
begin
result:=-1;
for i:=0 to GB.ControlCount-1 do begin
  if TRadioButton(GB.Controls[i]).Checked then begin
    result:=i;
    break;
  end;
end;
end;

procedure Tf_config.FormCreate(Sender: TObject);
var i,j,p : integer;
    buf,code,AVLlang : string;
    fs : TSearchRec;
    ft : TextFile;
    bt : TRadioButton;
    cb : TCheckBox;
begin
PageControl1.ActivePageIndex:=0;
i:=findfirst(slash(appdir)+slash('language')+'calclun.*.po',0,fs);
while i=0 do begin
  AssignFile(ft,slash(appdir)+slash('language')+fs.name);
  reset(ft);
  AVLlang:='';
  while not eof(ft) do begin
    readln(ft,buf);
    if buf='msgid "English"' then begin
       readln(ft,buf);
       p:=pos('"',buf);
       Delete(buf,1,p);
       p:=pos('"',buf);
       AVLlang:=copy(buf,1,p-1);
       break;
    end;
  end;
  CloseFile(ft);
  code:=extractfilename(fs.name);
  p:=pos('.',code);
  Delete(code,1,p);
  if p=0 then p:=9999;
  p:=pos('.',code);
  code:=copy(code,1,p-1);
  if code='en' then AVLlang:='English';
  buf:=code+' '+AVLlang;
  combobox3.Items.Add(buf);
  i:=findnext(fs);
end;
findclose(fs);
countrycode:=TStringList.Create;
end;

procedure Tf_config.Button1Click(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure Tf_config.Button4Click(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;



procedure Tf_config.FormDestroy(Sender: TObject);
var i: integer;
begin
countrycode.Free;
end;

procedure Tf_config.FormShow(Sender: TObject);
var j: integer;
begin
for j:=0 to combobox3.Items.Count-1 do
   if GetLangCode(combobox3.Items[j])=language then
     combobox3.ItemIndex:=j;
end;

procedure Tf_config.ComboBox3Change(Sender: TObject);
begin
newlang:=GetLangCode(combobox3.text);
end;

procedure Tf_config.LoadCountry(fn:string);
var f: textfile;
    buf: string;
    rec: TStringList;
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
  ComboBoxCountry.Clear;
  countrycode.Clear;
  Filemode:=0;
  system.assign(f,fn);
  reset(f);
  repeat
    readln(f,buf);
    buf:=trim(buf);
    if buf='' then continue;
    if buf[1]='#' then continue;
    SplitRec(buf,tab,rec);
    if rec.Count<3 then continue;
    countrycode.Add(rec[1]);
    ComboBoxCountry.Items.Add(rec[2]);
  until eof(f);
  CloseFile(f);
  rec.Free;
end;
end;

procedure Tf_config.SetObsCountry(value:string);
var i: integer;
begin
obscountry:=value;
for i:=0 to countrycode.Count-1 do begin
   if obscountry=countrycode[i] then begin
      ComboBoxCountry.ItemIndex:=i;
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
ComboBoxTZ.clear;
for i:=0 to tzinfo.ZoneTabCnty.Count-1 do begin
  if tzinfo.ZoneTabCnty[i]=obscountry then begin
     buf:=tzinfo.ZoneTabZone[i];
     j:=ComboBoxTZ.Items.Add(buf);
     if (tzinfo.ZoneTabZone[i]=obstz) then
       ComboBoxTZ.ItemIndex:=j;
  end;
end;
if ComboBoxTZ.ItemIndex<0 then ComboBoxTZ.ItemIndex:=0;
ObsTZ:=ComboBoxTZ.Text;
end;

procedure Tf_config.ComboBoxCountryChange(Sender: TObject);
begin
obscountry:=countrycode[ComboBoxCountry.ItemIndex];
UpdateTzList;
end;

procedure Tf_config.ComboBoxTZChange(Sender: TObject);
begin
 obstz:=ComboBoxTZ.Text;
end;


end.

