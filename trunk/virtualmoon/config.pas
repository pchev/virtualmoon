unit config;

{$MODE Delphi}
{$H+}
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
interface

uses
{$ifdef mswindows}
  LCLIntf,
{$endif}
  Math, u_constant, cu_tz,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, Inifiles, Grids, EnhEdits,
  CheckLst, LResources;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    BumpCheckBox: TCheckBox;
    Button5: TButton;
    CheckBox10: TCheckBox;
    CheckBox4: TCheckBox;
    CheckListBox2: TCheckListBox;
    ColorDialog1: TColorDialog;
    ComboBoxCountry: TComboBox;
    ComboBoxTZ: TComboBox;
    FontDialog1: TFontDialog;
    GroupBox2: TGroupBox;
    Label19: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    LabelFont: TLabel;
    PageControl1: TNotebook;
    RadioGroup1: TRadioGroup;
    TabSheet1: TPage;
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox3: TComboBox;
    CheckBox3: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    TabSheet2: TPage;
    TrackBar3: TTrackBar;
    UpDown1: TUpDown;
    numwin: TLongEdit;
    Label8: TLabel;
    StringGrid1: TStringGrid;
    Label9: TLabel;
    Impression: TPage;
    Label12: TLabel;
    LongEdit1: TLongEdit;
    Label13: TLabel;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    TrackBar1: TTrackBar;
    Label14: TLabel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Button2: TButton;
    Label15: TLabel;
    CheckBox12: TCheckBox;
    Edit4: TEdit;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    CheckBox13: TCheckBox;
    Label11: TLabel;
    CheckBox7: TCheckBox;
    Bevel4: TBevel;
    Label10: TLabel;
    ComboBox4: TComboBox;
    Button4: TButton;
    TabSheet3: TPage;
    Bevel1: TBevel;
    CheckBox2: TCheckBox;
    CheckBox1: TCheckBox;
    Label5: TLabel;
    Shape1: TShape;
    Label6: TLabel;
    Shape2: TShape;
    Label7: TLabel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    CheckBox6: TCheckBox;
    CheckBox5: TCheckBox;
    Bevel7: TBevel;
    Label16: TLabel;
    CheckBox14: TCheckBox;
    Shape3: TShape;
    Label17: TLabel;
    TrackBar2: TTrackBar;
    Label18: TLabel;
    CheckBox15: TCheckBox;
    ruklprefix: TEdit;
    ruklsuffix: TEdit;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    TabSheet4: TPage;
    TabSheet5: TPage;
    StringGrid2: TStringGrid;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Button6: TButton;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    CheckBox17: TCheckBox;
    CheckBox18: TCheckBox;
    Label28: TLabel;
    Edit10: TEdit;
    TabSheet6: TPage;
    TabSheet7: TPage;
    GroupBox1: TGroupBox;
    CheckBox19: TCheckBox;
    CheckBox20: TCheckBox;
    CheckBox21: TCheckBox;
    CheckBox22: TCheckBox;
    CheckBox23: TCheckBox;
    Bevel8: TBevel;
    Label23: TLabel;
    CheckBox16: TCheckBox;
    CheckListBox1: TCheckListBox;
    Label31: TLabel;
    TrackBar4: TTrackBar;
    Label33: TLabel;
    Bevel9: TBevel;
    OverlayPanel: TPanel;
    CheckBox11: TCheckBox;
    ComboBox5: TComboBox;
    Image1: TImage;
    Label30: TLabel;
    Label32: TLabel;
    TrackBar5: TTrackBar;
    CheckBox24: TCheckBox;
    procedure Button5Click(Sender: TObject);
    procedure CheckListBox2ItemClick(Sender: TObject; Index: integer);
    procedure ComboBoxCountryChange(Sender: TObject);
    procedure ComboBoxTZChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure Shape1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox12Click(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure Button6Click(Sender: TObject);
    procedure CheckBox19Click(Sender: TObject);
    procedure StringGrid2DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure StringGrid2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid2SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure ComboBox5Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox16Click(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
  private
    countrycode: TStringList;
    procedure UpdateTzList;

  public
    tzinfo: TCdCTimeZone;
    obscountry,obstz : string;
    texturefn: string;
    procedure SetObsCountry(value:string);
    procedure LoadCountry(fn:string);
  end;

var
  Form2: TForm2;
  newlang : string;
  savelibration : boolean;

implementation

uses u_util,
     virtualmoon1;

Function GetLangCode(buf:string):string;
var p : integer;
begin
p:=pos(' ',buf);
result:=trim(copy(buf,1,p-1));
end;

procedure TForm2.FormCreate(Sender: TObject);
var inifile : Tinifile;
    i,j,p : integer;
    buf,code,ver,AVLver : string;
    fs : TSearchRec;
begin
// hide developpement tools or not finished function
if not fileexists('version.developpement') then begin
  CheckBox12.Visible:=false;   // external image display
  Edit4.Visible:=false;        // external image display
  Button3.Visible:=false;      // external image display
  checkbox15.Visible:=false;   // direct LOPAM image link
  label20.Visible:=false;      // Rukl chart
  label21.Visible:=false;      // Rukl chart
  label22.Visible:=false;      // Rukl chart
  ruklprefix.Visible:=false;   // Rukl chart
  ruklsuffix.Visible:=false;   // Rukl chart
end;
AVLver:=copy(AVLversion,1,3);
i:=findfirst(slash(appdir)+slash('language')+'lang_u*.ini',0,fs);
while i=0 do begin
  inifile:=Tinifile.create(slash(appdir)+slash('language')+fs.name);
  buf:=inifile.ReadString('default','language','Invalid File '+fs.name);
  ver:=inifile.ReadString('default','version','1.0');
  if ver>=AVLver then ver:=''
     else ver:=' Wrong version '+ver+' !';
  inifile.free;
  code:=extractfilename(fs.name);
  p:=pos('.',code);
  if p=0 then p:=9999;
  code:=copy(code,7,p-7);
  buf:=code+' '+buf+ver;
  combobox3.Items.Add(buf);
  i:=findnext(fs);
end;
findclose(fs);
for j:=0 to combobox3.Items.Count-1 do if GetLangCode(combobox3.Items[j])=language then combobox3.ItemIndex:=j;
i:=findfirst(Slash(appdir)+Slash('Textures')+Slash('Overlay')+'*.jpg',0,fs);
combobox5.clear;
combobox5.Sorted:=true;
while i=0 do begin
  combobox5.Items.Add(remext(fs.name));
  i:=findnext(fs);
end;
findclose(fs);
i:=findfirst(Slash(appdir)+Slash('Textures')+'*',faDirectory,fs);
CheckListBox2.clear;
CheckListBox2.Sorted:=true;
while i=0 do begin
  if ((fs.Attr and faDirectory)= faDirectory)and(fs.Name<>'.')and(fs.Name<>'..')and(fs.Name<>'Bumpmap')and(fs.Name<>'Overlay') then
    CheckListBox2.Items.Add(fs.name);
  i:=findnext(fs);
end;
findclose(fs);
savelibration:=librationeffect;
countrycode:=TStringList.Create;
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
if FontDialog1.Execute then begin
   LabelFont.Caption:=FontDialog1.Font.Name;
   LabelFont.Font:=FontDialog1.Font;
   LabelFont.Font.Color:=clWindowText;
end;
end;

procedure TForm2.FormDestroy(Sender: TObject);
var i: integer;
begin
for i:=0 to Checklistbox1.Count-1 do (Checklistbox1.Items.Objects[i] as TDBinfo).Free;
countrycode.Free;
end;

procedure TForm2.ComboBox3Change(Sender: TObject);
begin
newlang:=GetLangCode(combobox3.text);
end;

procedure TForm2.CheckBox3Click(Sender: TObject);
begin
label1.Enabled:=not checkbox3.checked;
label2.Enabled:=not checkbox3.checked;
edit1.Enabled:=not checkbox3.checked;
edit2.Enabled:=not checkbox3.checked;
combobox1.Enabled:=not checkbox3.checked;
combobox2.Enabled:=not checkbox3.checked;
end;

procedure TForm2.Shape1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   if ColorDialog1.Execute then with sender as TShape do begin
      Brush.Color:=ColorDialog1.Color;
   end;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
form1.PrinterSetupDialog1.Execute;
//GetPrinterResolution(PrtName,PrinterResolution);
end;

procedure TForm2.FormShow(Sender: TObject);
var myRect: TGridRect;
begin
  OverlayPanel.visible:=AsMultiTexture;
  myRect.Left := 0;
  myRect.Top := 3;
  myRect.Right := 0;
  myRect.Bottom := 3;
  StringGrid1.Selection := myRect;
//  Panel1.Visible:=checkbox4.Checked;
  TrackBar2.Min:=-1000;
  TrackBar2.Max:=-100;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
if opendialog1.execute then edit4.Text:=opendialog1.FileName;
end;

procedure TForm2.CheckBox12Click(Sender: TObject);
begin
edit4.Enabled:=CheckBox12.checked;
button3.Enabled:=CheckBox12.checked;
end;

procedure TForm2.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
if (ACol=0)and(Arow<=2) then CanSelect:=False;
end;

procedure TForm2.Button6Click(Sender: TObject);
begin
edit10.Text:=inttostr(round((strtofloat(edit6.text)/strtofloat(edit7.text)) ));
edit9.Text:=inttostr(round(60* strtofloat(edit8.text)/(strtofloat(edit6.text)/strtofloat(edit7.text)) ));
end;

procedure TForm2.CheckBox19Click(Sender: TObject);
begin
 CheckBox19.Checked:=true;
end;

procedure TForm2.CheckListBox2ItemClick(Sender: TObject; Index: integer);
var i: integer;
begin
    for i:=0 to CheckListBox2.Count-1 do begin
       if i=Index then
       begin
         CheckListBox2.checked[i]:=true;
         texturefn:=CheckListBox2.Items[i];
       end else
         CheckListBox2.checked[i]:=false;
    end;
end;

procedure TForm2.StringGrid2DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
with Sender as TStringGrid do begin
if (Acol>=2)and(Arow>0) then begin
  if (cells[acol,arow]='0')then begin
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(Rect);
  end else if (cells[acol,arow]='1')then begin
    Canvas.Brush.Color := clRed;
    Canvas.FillRect(Rect);
  end else if (cells[acol,arow]='2')then begin
    Canvas.Brush.Color := clLime;
    Canvas.FillRect(Rect);
  end;
  end;
end;
end;

procedure TForm2.StringGrid2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var col,row,i:integer;
begin
StringGrid2.MouseToCell(X, Y, Col, Row);
if row=0 then exit;
if col>=2 then begin
    i:=strtointdef(stringgrid2.Cells[col,row],-1);
    inc(i);
    if i>2 then i:=0;
    stringgrid2.Cells[col,row]:=inttostr(i);
end;
end;

procedure TForm2.StringGrid2SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
if Acol>=2 then canselect:=false else canselect:=true;
end;

procedure TForm2.ComboBox5Change(Sender: TObject);
begin
if fileexists(Slash(appdir)+Slash('Textures')+Slash('Overlay')+combobox5.text+'.jpg') then begin
   image1.Picture.LoadFromFile(Slash(appdir)+Slash('Textures')+Slash('Overlay')+combobox5.text+'.jpg');
   CheckBox11.Checked:=true;
end else begin
   image1.Picture.Assign(nil);
end;
end;

procedure TForm2.CheckBox16Click(Sender: TObject);
begin
end;

procedure TForm2.TrackBar3Change(Sender: TObject);
begin
  if TrackBar3.Position>=25 then TrackBar3.Position:=30
  else if TrackBar3.Position>=20 then TrackBar3.Position:=20
  else if TrackBar3.Position>=15 then TrackBar3.Position:=15
  else if TrackBar3.Position>=10 then TrackBar3.Position:=10
  else if TrackBar3.Position>=5 then TrackBar3.Position:=5;
end;

procedure TForm2.LoadCountry(fn:string);
var f: textfile;
    buf: string;
    i: integer;
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

procedure TForm2.SetObsCountry(value:string);
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

procedure TForm2.UpdateTzList;
var
  i,j: Integer;
  buf: string;
begin
ComboBoxTZ.clear;
j:=0;
for i:=0 to tzinfo.ZoneTabCnty.Count-1 do begin
  if tzinfo.ZoneTabCnty[i]=obscountry then begin
     buf:=tzinfo.ZoneTabZone[i];
     ComboBoxTZ.Items.Add(buf);
     if (j=0)or(tzinfo.ZoneTabZone[i]=obstz) then ComboBoxTZ.ItemIndex:=j;
     inc(j);
  end;
end;
ObsTZ:=ComboBoxTZ.Text;
end;

procedure TForm2.ComboBoxCountryChange(Sender: TObject);
begin
obscountry:=countrycode[ComboBoxCountry.ItemIndex];
UpdateTzList;
end;

procedure TForm2.ComboBoxTZChange(Sender: TObject);
begin
 obstz:=ComboBoxTZ.Text;
end;

initialization
  {$i config.lrs}

end.

