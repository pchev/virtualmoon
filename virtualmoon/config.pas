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
  u_translation,
  Math, u_constant, cu_tz,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, Inifiles, Grids, EnhEdits,
  CheckLst, LResources;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Button4: TButton;
    Button5: TButton;
    Button7: TButton;
    Button8: TButton;
    CheckBox10: TCheckBox;
    CheckBox25: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ColorDialog1: TColorDialog;
    ComboBoxCountry: TComboBox;
    ComboBoxTZ: TComboBox;
    Label29: TLabel;
    Label3: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    LabelImp: TLabel;
    LabelGrid: TLabel;
    FontDialog1: TFontDialog;
    GroupBox2: TGroupBox;
    Label19: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    LabelFont: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    RadioGroup4: TRadioGroup;
    RadioGroup5: TRadioGroup;
    RadioGroup6: TRadioGroup;
    BumpRadioGroup: TRadioGroup;
    RadioGroup7: TRadioGroup;
    TabSheet1: TTabSheet;
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    TabSheet2: TTabSheet;
    TrackBar3: TTrackBar;
    StringGrid1: TStringGrid;
    Label9: TLabel;
    Impression: TTabSheet;
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
    TabSheet3: TTabSheet;
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
    Label22: TLabel;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
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
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
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
    procedure Button7Click(Sender: TObject);
    procedure ComboBoxCountryChange(Sender: TObject);
    procedure ComboBoxTZChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup7Click(Sender: TObject);
    procedure RadioGroupTextureClick(Sender: TObject);
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
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
  private
    countrycode: TStringList;
    ov: TBitmap;
    lockoverlay,locktexture: boolean;
    savelibration : boolean;
    FPrinterDialog : TNotifyEvent;
    procedure UpdateTzList;
    procedure showtexture;
  public
    newlang: string;
    tzinfo: TCdCTimeZone;
    obscountry,obstz : string;
    TexturePath: string;
    texturefn: TStringList;
    TextureList: TStringList;
    TextureChanged: boolean;
    procedure SetObsCountry(value:string);
    procedure LoadCountry(fn:string);
    procedure Setlang;
  property onPrinterDialog : TNotifyEvent read FPrinterDialog write FPrinterDialog;
  end;

var
  Form2: TForm2;

implementation

uses u_util, pu_features;

procedure TForm2.Setlang;
begin
      Caption := rst_3;
      label1.Caption := rst_19;
      label2.Caption := rsm_10;
      CheckBox1.Caption := rst_22;
      CheckBox2.Caption := rst_23;
      label4.Caption := rst_24;
      Button1.Caption := rst_18;
      Radiogroup7.Items[0] := rsTopocentric;
      Radiogroup7.Items[1] := rst_26;
      Label5.Caption := rst_28;
      Label17.Caption := rst_29;
      Label7.Caption := rst_52;
      CheckBox5.Caption := rst_53;
      CheckBox6.Caption := rst_54;
      TabSheet1.Caption := rst_57;
      TabSheet2.Caption := rst_58;
      label9.Caption := rst_60;
      stringgrid1.Cells[0, 0] := rst_47;
      stringgrid1.Cells[1, 0] := rst_62;
      Label11.Caption := rst_75;
      Label10.Caption := rst_76;
      Combobox4.Items[0] := rst_77;
      Combobox4.Text := Combobox4.Items[0];
      CheckBox12.Caption := rst_89;
      CheckBox7.Caption := rst_80;
      Impression.Caption := rst_90;
      Label15.Caption := rst_91;
      Button2.Caption := rst_55;
      Label12.Caption := rst_92;
      Label13.Caption := rst_93;
      Checkbox8.Caption := rst_94;
      Checkbox9.Caption := rst_95;
      Checkbox13.Caption := rst_96;
      Label14.Caption := rst_97;
      Button4.Caption := rst_99;
      Label16.Caption := rst_100;
      Tabsheet3.Caption := rst_101;
      Checkbox14.Caption := rst_102;
      Label6.Caption := rst_103;
      Label18.Caption := rst_104;
      TabSheet5.Caption := rst_109;
      label24.Caption := rst_110;
      label25.Caption := rst_111;
      label26.Caption := rst_112;
      button6.Caption := rst_113;
      stringgrid2.Cells[0, 0] := rst_118;
      stringgrid2.Cells[1, 0] := rst_119;
      stringgrid2.Cells[2, 0] := '<->';
      stringgrid2.Cells[3, 0] := 'N/S';
      Checkbox17.Caption := rst_120;
      Checkbox18.Caption := rst_121;
      Checkbox19.Caption := rst_122;
      Checkbox20.Caption := rst_123;
      Checkbox21.Caption := rst_131;
      Checkbox22.Caption := rst_168;
      Checkbox23.Caption := rst_164;
      Checkbox24.Caption := rst_187;
      Checkbox25.Caption := rst_188;
      label28.Caption := rst_124;
      GroupBox1.Caption := rst_129;
      TabSheet7.Caption := GroupBox1.Caption;
      combobox1.items[0] := rst_147;
      combobox1.items[1] := rst_148;
      combobox1.ItemIndex := 0;
      combobox2.items[0] := rst_149;
      combobox2.items[1] := rst_150;
      combobox2.ItemIndex := 0;
      TabSheet4.Caption := rst_152;
      Label19.Caption := rst_144;
      TabSheet6.Caption := rst_169;
      CheckBox11.Caption := rst_170;
      label30.Caption := rst_171;
      label32.Caption := rst_172;
      Label23.Caption := rst_173;
      CheckBox16.Caption := rst_174;
      label31.Caption := rst_179;
      label33.Caption := rst_181;
      CheckBox4.Caption:= rsRemoveUnknow;
      GroupBox2.Caption:=rsGrid;
      CheckBox10.Caption:=rsShowGrid;
      label3.Caption:= rsZoomLevel;
      Button5.Caption:=rsLabelsFont;
      Label35.Caption:=rsCountry;
      Label34.Caption:=rst_21;
      BumpRadioGroup.Items[0]:=rsPhaseWithDyn;
      BumpRadioGroup.Items[1]:=rsPhaseWithout;
      RadioGroup1.Caption:=rsDynamicRelie;
      RadioGroup1.Items[0]:=rsSimple;
      RadioGroup1.Items[1]:=rsAdvanced;
      Button7.Caption:=rsCheckForOpti;
      Button8.Caption:=rsCheckForOpti;
      checkbox3.Caption:=rst_87;
end;

Function GetLangCode(buf:string):string;
var p : integer;
begin
p:=pos(' ',buf);
result:=trim(copy(buf,1,p-1));
end;

procedure TForm2.FormCreate(Sender: TObject);
var i,j,p : integer;
    buf,code,AVLlang : string;
    fs : TSearchRec;
    ft : TextFile;
begin
{$ifdef mswindows}
 ScaleForm(self,Screen.PixelsPerInch/96);
{$endif}
ov:=Tbitmap.Create;
lockoverlay:=false;
locktexture:=false;
// hide developpement tools or not finished function
if not fileexists('version.developpement') then begin
  CheckBox12.Visible:=false;   // external image display
  Edit4.Visible:=false;        // external image display
  Button3.Visible:=false;      // external image display
  checkbox15.Visible:=false;   // direct LOPAM image link
  label22.Visible:=false;      // Rukl chart
  ruklprefix.Visible:=false;   // Rukl chart
  ruklsuffix.Visible:=false;   // Rukl chart
end;
i:=findfirst(slash(appdir)+slash('language')+'maplun.*.po',0,fs);
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
for j:=0 to combobox3.Items.Count-1 do if GetLangCode(combobox3.Items[j])=language then combobox3.ItemIndex:=j;
i:=findfirst(Slash(appdir)+Slash('Textures')+Slash('Overlay')+'*.jpg',0,fs);
combobox5.clear;
combobox5.Sorted:=true;
while i=0 do begin
  combobox5.Items.Add(remext(fs.name));
  i:=findnext(fs);
end;
findclose(fs);
Texturefn:=TStringList.Create;
TextureList:=TStringList.Create;
i:=findfirst(Slash(appdir)+Slash('Textures')+'*',faDirectory,fs);
while i=0 do begin
  if ((fs.Attr and faDirectory)= faDirectory)and(fs.Name<>'.')and(fs.Name<>'..')and(fs.Name<>'Bumpmap')and(fs.Name<>'Overlay') then begin
    TextureList.Add(fs.name);
  end;
  i:=findnext(fs);
end;
findclose(fs);
TextureList.Sort;
RadioGroup2.Items.clear;
RadioGroup3.Items.clear;
RadioGroup4.Items.clear;
RadioGroup5.Items.clear;
RadioGroup6.Items.clear;
for i:=0 to TextureList.Count-1 do begin
    if TextureList[i]='Airbrush' then buf:=rsAirburshReli
    else if TextureList[i]='Airbrush_no_albedo' then buf:=rsAirburshReli2
    else if TextureList[i]='Clementine' then buf:=rsClementinePh
    else if TextureList[i]='Lopam' then buf:=rsLOPAMPhotogr
    else buf:=TextureList[i];
    RadioGroup2.Items.Add(buf);
    RadioGroup3.Items.Add('');
    RadioGroup4.Items.Add('');
    RadioGroup5.Items.Add('');
    RadioGroup6.Items.Add('');
end;
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

procedure TForm2.Button7Click(Sender: TObject);
begin
  f_features.showmodal;
end;

procedure TForm2.FormDestroy(Sender: TObject);
var i: integer;
begin
for i:=0 to Checklistbox1.Count-1 do (Checklistbox1.Items.Objects[i] as TDBinfo).Free;
countrycode.Free;
TextureList.Free;
Texturefn.Free;
ov.Free;
end;

procedure TForm2.TrackBar1Change(Sender: TObject);
begin
  LabelImp.Caption:=inttostr(trackbar1.Position);
end;

procedure TForm2.ComboBox3Change(Sender: TObject);
begin
newlang:=GetLangCode(combobox3.text);
end;


procedure TForm2.RadioGroup7Click(Sender: TObject);
var topo:boolean;
begin
topo:=(RadioGroup7.ItemIndex=0);
label1.Enabled:= topo;
label2.Enabled:= topo;
edit1.Enabled:= topo;
edit2.Enabled:= topo;
combobox1.Enabled:= topo;
combobox2.Enabled:= topo;
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
if Assigned(FPrinterDialog) then FPrinterDialog(self);
end;

procedure TForm2.showtexture;
var i,j: smallint;
begin
locktexture:=true;
    for j:=0 to 3 do begin
      for i:=0 to TextureList.Count-1 do begin
         if TextureList[i]=texturefn[j] then begin
           case j of
             0: RadioGroup3.ItemIndex:=i;
             1: RadioGroup4.ItemIndex:=i;
             2: RadioGroup5.ItemIndex:=i;
             3: RadioGroup6.ItemIndex:=i;
           end;
         end;
      end;
    end;
locktexture:=false;
end;

procedure TForm2.FormShow(Sender: TObject);
var myRect: TGridRect;
begin
  memo1.Text:=rst_184;
  OverlayPanel.visible:=AsMultiTexture;
  panel2.Visible:=not AsMultiTexture;
  myRect.Left := 0;
  myRect.Top := 3;
  myRect.Right := 0;
  myRect.Bottom := 3;
  StringGrid1.Selection := myRect;
  TrackBar2.Min:=-1000;
  TrackBar2.Max:=-100;
  TextureChanged:=false;
  showtexture;
  LabelGrid.Caption:=inttostr(TrackBar3.Position)+ldeg;
  LabelImp.Caption:=inttostr(trackbar1.Position);
  RadioGroup7Click(nil);
  BringToFront;
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

procedure TForm2.RadioGroup2Click(Sender: TObject);
var i: integer;
begin
i:=RadioGroup2.ItemIndex;
RadioGroup3.ItemIndex:=i;
RadioGroup4.ItemIndex:=i;
RadioGroup5.ItemIndex:=i;
RadioGroup6.ItemIndex:=i;
Application.ProcessMessages;
showtexture;
end;

procedure TForm2.RadioGroupTextureClick(Sender: TObject);
var i,j: integer;
    tex: string;
begin
if locktexture then exit;
i:=(sender as TRadioGroup).ItemIndex;
j:=(sender as TRadioGroup).Tag;
if (i>=0)and(j>=0) then begin
  tex:=TextureList[i];
  if DirectoryExists(slash(TexturePath)+slash(tex)+'L'+inttostr(j+1)) then
  begin
    texturefn[j]:=tex;
    TextureChanged:=true;
  end
    else begin
      (sender as TRadioGroup).ItemIndex:=-1;
      showtexture;
    end;
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

procedure TForm2.TrackBar3Change(Sender: TObject);
begin
  if TrackBar3.Position>24 then TrackBar3.Position:=30
  else if TrackBar3.Position>=19 then TrackBar3.Position:=20
  else if TrackBar3.Position>=14 then TrackBar3.Position:=15
  else if TrackBar3.Position>=9 then TrackBar3.Position:=10
  else if TrackBar3.Position>=4 then TrackBar3.Position:=5
  else TrackBar3.Position:=1;
LabelGrid.Caption:=inttostr(TrackBar3.Position)+ldeg;
if TrackBar3.Position=30 then TrackBar3.PageSize:=6
   else TrackBar3.PageSize:=5;
TrackBar3.LineSize:=TrackBar3.PageSize;
end;

{procedure TForm2.ComboBox5Change(Sender: TObject);
begin
if fileexists(Slash(appdir)+Slash('Textures')+Slash('Overlay')+combobox5.text+'.jpg') then begin
   image1.Picture.LoadFromFile(Slash(appdir)+Slash('Textures')+Slash('Overlay')+combobox5.text+'.jpg');
   CheckBox11.Checked:=true;
end else begin
   image1.Picture.Assign(nil);
end;
end; }
procedure TForm2.ComboBox5Change(Sender: TObject);
var  j:tjpegimage;
begin
if fileexists(Slash(appdir)+Slash('Textures')+Slash('Overlay')+combobox5.text+'.jpg') then begin
   j:=tjpegimage.create;
   try
   j.LoadFromFile(Slash(appdir)+Slash('Textures')+Slash('Overlay')+combobox5.text+'.jpg');
   ov.Width:=image1.Width;
   ov.Height:=image1.Height;
   ov.pixelformat:=pf24bit;
   ov.Canvas.StretchDraw(rect(0,0,ov.Width,ov.Height),j);
   TrackBar5Change(Sender);
   CheckBox11.Checked:=true;
   finally
    j.free;
   end;
end else begin
   ov.Assign(nil);
   image1.Picture.Assign(nil);
end;
end;

procedure TForm2.TrackBar5Change(Sender: TObject);
var l : integer;
begin
if not ov.Empty then begin
   if lockoverlay then exit;
   lockoverlay:=true;
   try
    image1.Picture.bitmap.Assign(ov);
    l:=2*trackbar5.position;
    SetImgLum(image1.Picture.bitmap,l);
    image1.Refresh;
   finally
    lockoverlay:=false;
   end;
end;
end;

procedure TForm2.LoadCountry(fn:string);
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

