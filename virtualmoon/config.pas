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
  StdCtrls, ComCtrls, Buttons, ExtCtrls, Inifiles, Grids, EnhEdits,
  CheckLst, LResources;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TSpeedButton;
    ButtonDefault: TButton;
    Button4: TSpeedButton;
    Button5: TSpeedButton;
    Button7: TSpeedButton;
    Button8: TSpeedButton;
    Button9: TSpeedButton;
    CheckBox10: TCheckBox;
    CheckBoxTerminatorLine: TCheckBox;
    DbList: TCheckListBox;
    ColorDialog1: TColorDialog;
    ComboBox6: TComboBox;
    ComboBoxCountry: TComboBox;
    ComboBoxTZ: TComboBox;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    Edit17: TEdit;
    Edit3: TEdit;
    GBlvall: TGroupBox;
    GBlv1: TGroupBox;
    GBlv2: TGroupBox;
    GBlv3: TGroupBox;
    GBlv4: TGroupBox;
    GBlv5: TGroupBox;
    GBlv6: TGroupBox;
    GroupBox1: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBoxColor: TGroupBox;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
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
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label8: TLabel;
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
    Panel3: TPanel;
    ScrollBox1: TScrollBox;
    Shape4: TShape;
    Shape5: TShape;
    TexturePanel: TPanel;
    BumpRadioGroup: TRadioGroup;
    RadioGroup7: TRadioGroup;
    StringGrid3: TStringGrid;
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
    TabSheet8: TTabSheet;
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
    Button2: TSpeedButton;
    Label15: TLabel;
    CheckBox12: TCheckBox;
    Edit4: TEdit;
    Button3: TSpeedButton;
    OpenDialog1: TOpenDialog;
    CheckBox13: TCheckBox;
    Label11: TLabel;
    CheckBox7: TCheckBox;
    Bevel4: TBevel;
    Label10: TLabel;
    ComboBox4: TComboBox;
    TabSheet3: TTabSheet;
    CheckBox2: TCheckBox;
    CheckBox1: TCheckBox;
    Shape1: TShape;
    Label6: TLabel;
    Shape2: TShape;
    Label7: TLabel;
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
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    StringGrid2: TStringGrid;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Button6: TSpeedButton;
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
    Bevel8: TBevel;
    Label23: TLabel;
    CheckBox16: TCheckBox;
    UserDbList: TCheckListBox;
    Label31: TLabel;
    TrackBar4: TTrackBar;
    Label33: TLabel;
    OverlayPanel: TPanel;
    CheckBox11: TCheckBox;
    ComboBox5: TComboBox;
    Image1: TImage;
    Label30: TLabel;
    Label32: TLabel;
    TrackBar5: TTrackBar;
    procedure BumpRadioGroupClick(Sender: TObject);
    procedure ButtonDefaultClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure ComboBoxCountryChange(Sender: TObject);
    procedure ComboBoxTZChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure RadioButtonAllClick(Sender: TObject);
    procedure RadioGroup7Click(Sender: TObject);
    procedure RadioButtonLvClick(Sender: TObject);
    procedure Shape1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox12Click(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure Button6Click(Sender: TObject);
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
    procedure FillHistorical;
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
  Form2: TForm2;

implementation

{$R config.lfm}

uses u_util, pu_features;

procedure TForm2.Setlang;
begin
      Caption := rst_3;
      label1.Caption := rst_19;
      label2.Caption := rsm_10;
      label62.Caption := rsm_74;
      CheckBox1.Caption := rst_22;
      CheckBox2.Caption := rst_23;
      CheckBoxTerminatorLine.Caption:=rsTerminatorLi;
      label4.Caption := rst_24;
      Button1.Caption := rst_18;
      Radiogroup7.Items[0] := rsTopocentric;
      Radiogroup7.Items[1] := rst_26;
      GroupBoxColor.Caption := rst_28;
      Label17.Caption := rst_29;
      label22.Caption:=rsImpactBassin;
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
      label49.Caption := rst_110;
      label50.Caption:=rsPixelSize;
      label53.Caption:=rsPixelCount;
      button9.Caption := rst_113;
      stringgrid3.Cells[0, 0] := rsCCDName;
      stringgrid3.Cells[1, 0] := rsWidth;
      stringgrid3.Cells[2, 0] := rsHeight;
      stringgrid3.Cells[3, 0] := rst_64;
      Checkbox17.Caption := rst_120;
      Checkbox18.Caption := rst_121;
      label28.Caption := rst_124;
      label61.Caption := rst_129;
      TabSheet7.Caption := rst_129;
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
      GroupBox2.Caption:=rsGrid;
      CheckBox10.Caption:=rsShowGrid;
      label3.Caption:= rsZoomLevel;
      Button5.Caption:=rsLabelsFont;
      Label35.Caption:=rsCountry;
      Label34.Caption:=rst_21;
      BumpRadioGroup.Items[0]:=rsPhaseWithout+blank+rsStandardText;
      BumpRadioGroup.Items[1]:=rsPhaseWithDyn;
      BumpRadioGroup.Items[2]:=rsNoTextureToU;
      Button7.Caption:=rsCheckForOpti;
      Button8.Caption:=rsCheckForOpti;
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

procedure TForm2.FormCreate(Sender: TObject);
var i,j,p : integer;
    buf,code,AVLlang : string;
    fs : TSearchRec;
    ft : TextFile;
    bt : TRadioButton;
    cb : TCheckBox;
procedure addbuttons(n:integer;txt:string);
var toppos,hh:integer;
begin
  cb:=TCheckBox.Create(self);
  cb.Parent:=GBlvall;
  toppos:= 2+n*(cb.Height+4);
  hh:=toppos+cb.Height+40;
  GBlvall.Height:=hh;
  GBlv1.Height:=hh;
  GBlv2.Height:=hh;
  GBlv3.Height:=hh;
  GBlv4.Height:=hh;
  GBlv5.Height:=hh;
  GBlv6.Height:=hh;
  TexturePanel.Height:=GBlvall.top+hh;
  cb.Top:=toppos;
  cb.Caption:=txt;
  cb.Tag:=n;
  cb.OnClick:=RadioButtonAllClick;
  bt:=TRadioButton.Create(self);
  bt.Parent:=GBlv1;
  bt.Top:=toppos;
  bt.Caption:='';
  bt.Tag:=n;
  bt.OnClick:=RadioButtonLvClick;
  bt:=TRadioButton.Create(self);
  bt.Parent:=GBlv2;
  bt.Top:=toppos;
  bt.Caption:='';
  bt.Tag:=1000+n;
  bt.OnClick:=RadioButtonLvClick;
  bt:=TRadioButton.Create(self);
  bt.Parent:=GBlv3;
  bt.Top:=toppos;
  bt.Caption:='';
  bt.Tag:=2000+n;
  bt.OnClick:=RadioButtonLvClick;
  bt:=TRadioButton.Create(self);
  bt.Parent:=GBlv4;
  bt.Top:=toppos;
  bt.Caption:='';
  bt.Tag:=3000+n;
  bt.OnClick:=RadioButtonLvClick;
  bt:=TRadioButton.Create(self);
  bt.Parent:=GBlv5;
  bt.Top:=toppos;
  bt.Caption:='';
  bt.Tag:=4000+n;
  bt.OnClick:=RadioButtonLvClick;
  bt:=TRadioButton.Create(self);
  bt.Parent:=GBlv6;
  bt.Top:=toppos;
  bt.Caption:='';
  bt.Tag:=5000+n;
  bt.OnClick:=RadioButtonLvClick;
end;
begin
ScaleFormForFontSize(self,96);
ov:=Tbitmap.Create;
lockoverlay:=false;
locktexture:=false;
// hide developpement tools or not finished function
if not fileexists('version.developpement') then begin
  CheckBox12.Visible:=false;   // external image display
  Edit4.Visible:=false;        // external image display
  Button3.Visible:=false;      // external image display
  checkbox15.Visible:=false;   // direct LOPAM image link
end;
PageControl1.ActivePageIndex:=0;
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
for i:=0 to TextureList.Count-1 do begin
    if TextureList[i]='Airbrush' then buf:=rsAirburshReli
    else if TextureList[i]='Airbrush_no_albedo' then buf:=rsAirburshReli2
    else if TextureList[i]='Clementine' then buf:=rsClementinePh
    else if TextureList[i]='Lopam' then buf:=rsLOPAMPhotogr
    else if TextureList[i]='WAC' then buf:='LRO WAC Mosaic'
    else if TextureList[i]='WAC_LOWSUN' then buf:='LRO WAC '+rsLowSunElevat
    else if TextureList[i]=HistoricalDir then begin
      buf:=rsHistorical;
      FillHistorical;
    end
    else buf:=TextureList[i];
    buf:=StringReplace(buf,'_',' ',[rfReplaceAll]);
    addbuttons(i,buf);
end;
savelibration:=librationeffect;
countrycode:=TStringList.Create;
end;

procedure TForm2.FillHistorical;
var i : integer;
    fs : TSearchRec;
begin
ComboBox6.clear;
i:=findfirst(Slash(appdir)+Slash('Textures')+Slash('Historical')+'*',faDirectory,fs);
while i=0 do begin
  if ((fs.Attr and faDirectory)= faDirectory)and(fs.Name<>'.')and(fs.Name<>'..') then begin
    ComboBox6.Items.Add(fs.name);
  end;
  i:=findnext(fs);
end;
findclose(fs);
ComboBox6.ItemIndex:=0;
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
if FontDialog1.Execute then begin
   LabelFont.Caption:=FontDialog1.Font.Name;
   LabelFont.Font:=FontDialog1.Font;
   LabelFont.Font.Color:=clWindowText;
end;
end;

procedure TForm2.BumpRadioGroupClick(Sender: TObject);
begin
{$ifdef lclgtk2}
if BumpRadioGroup.ItemIndex=1 then BumpRadioGroup.ItemIndex:=0;  // do not work on Linux for now
{$endif}
TexturePanel.Visible:=(BumpRadioGroup.ItemIndex=0);
TextureChanged:=true;
if BumpRadioGroup.ItemIndex=2 then CheckBox11.Checked:=true;
end;

procedure TForm2.ButtonDefaultClick(Sender: TObject);
begin
  CheckBox1.Checked:=true;
  CheckBox2.Checked:=true;
  CheckBoxTerminatorLine.Checked:=false;
  Shape1.Brush.Color:=clYellow;
  Shape2.Brush.Color:=clRed;
  Shape3.Brush.Color:=clYellow;
  Shape4.Brush.Color:=clYellow;
  Shape5.Brush.Color:=clBlue;
  CheckBox14.Checked:=false;
  CheckBox6.Checked:=true;
  TrackBar4.Position:=5;
  CheckBox5.Checked:=true;
  CheckBox17.Checked:=true;
  CheckBox18.Checked:=true;
  TrackBar2.Position:=-600;
  FontDialog1.Font.Name:='default';
  FontDialog1.Font.Height:=0;
  FontDialog1.Font.Pitch:=fpDefault;
  FontDialog1.Font.Quality:=fqDefault;
  FontDialog1.Font.Size:=0;
  FontDialog1.Font.Style:=[];
  LabelFont.Caption:=FontDialog1.Font.Name;
  LabelFont.Font:=FontDialog1.Font;
  LabelFont.Font.Color:=clWindowText;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TForm2.Button7Click(Sender: TObject);
begin
  f_features.showmodal;
end;

procedure TForm2.FormDestroy(Sender: TObject);
var i: integer;
begin
for i:=0 to UserDbList.Count-1 do (UserDbList.Items.Objects[i] as TDBinfo).Free;
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
edit3.Enabled:= topo;
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
    tex: string;
begin
locktexture:=true;
    for j:=0 to 5 do begin
      for i:=0 to TextureList.Count-1 do begin
         tex:=texturefn[j];
         if pos(HistoricalDir,tex)>0 then tex:=noslash(ExtractFilePath(noslash(tex)));
         if TextureList[i]=tex then begin
           case j of
             0: TRadioButton(GBlv1.Controls[i]).Checked:=true;
             1: TRadioButton(GBlv2.Controls[i]).Checked:=true;
             2: TRadioButton(GBlv3.Controls[i]).Checked:=true;
             3: TRadioButton(GBlv4.Controls[i]).Checked:=true;
             4: TRadioButton(GBlv5.Controls[i]).Checked:=true;
             5: TRadioButton(GBlv6.Controls[i]).Checked:=true;
           end;
         end;
      end;
    end;
locktexture:=false;
end;

procedure TForm2.FormShow(Sender: TObject);
var myRect: TGridRect;
    i,k:integer;
begin
  memo1.Text:=rst_184;
  OverlayPanel.visible:=AsMultiTexture;
  panel2.Visible:=not AsMultiTexture;
  TexturePanel.Visible:=(BumpRadioGroup.ItemIndex=0);
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
  for i:=1 to maxlevel do HistN[i]:=-1;
  for i:=0 to texturefn.count-1 do
    if pos(HistoricalDir,texturefn[i])>0 then begin
      HistTex:=ExtractFileName(noslash(texturefn[i]));
      HistN[i]:=i;
    end;
  k:=-1;
  for i:=0 to GBlvall.ControlCount-1 do
     if TRadioButton(GBlvall.Controls[i]).Caption=rsHistorical then begin
       k:=TRadioButton(GBlvall.Controls[i]).Top;
       break;
     end;
  if k>0 then begin
     ComboBox6.Visible:=true;
     ComboBox6.Top:=k-3;
     for i:=0 to ComboBox6.Items.Count-1 do
       if ComboBox6.Items[i]=HistTex then begin
         ComboBox6.ItemIndex:=i;
         break;
     end;
  end
  else ComboBox6.Visible:=false;
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

procedure TForm2.Button9Click(Sender: TObject);
var f,px,py,cx,cy: single;
begin
f:=strtofloat(edit11.text);
px:=strtofloat(edit12.text)/1000;
py:=strtofloat(edit13.text)/1000;
cx:=strtofloat(edit15.text);
cy:=strtofloat(edit16.text);
Edit14.Text:=FormatFloat(f2,arctan(px/f)*cx*rad2deg*60);
Edit17.Text:=FormatFloat(f2,arctan(py/f)*cy*rad2deg*60);
end;

procedure TForm2.ComboBox6Change(Sender: TObject);
var i: Integer;
begin
  HistTex:=ComboBox6.Text;
  for i:=0 to texturefn.count-1 do
    if pos(HistoricalDir,texturefn[i])>0 then begin
      texturefn[i]:=slash(ExtractFilePath(noslash(texturefn[i])))+HistTex;
    end;
  TextureChanged:=true;
end;

procedure TForm2.RadioButtonAllClick(Sender: TObject);
var i: integer;
begin
i:=TCheckBox(sender).tag;
TCheckBox(sender).Checked:=false;
if TRadioButton(GBlv1.Controls[i]).Enabled then TRadioButton(GBlv1.Controls[i]).Checked:=true;
if TRadioButton(GBlv2.Controls[i]).Enabled then TRadioButton(GBlv2.Controls[i]).Checked:=true;
if TRadioButton(GBlv3.Controls[i]).Enabled then TRadioButton(GBlv3.Controls[i]).Checked:=true;
if TRadioButton(GBlv4.Controls[i]).Enabled then TRadioButton(GBlv4.Controls[i]).Checked:=true;
if TRadioButton(GBlv5.Controls[i]).Enabled then TRadioButton(GBlv5.Controls[i]).Checked:=true;
if TRadioButton(GBlv6.Controls[i]).Enabled then TRadioButton(GBlv6.Controls[i]).Checked:=true;
Application.ProcessMessages;
showtexture;
end;

procedure TForm2.RadioButtonLvClick(Sender: TObject);
var i,j: integer;
    tex: string;
begin
if locktexture then exit;
i:=TRadioButton(sender).Tag mod 1000;
j:=trunc(TRadioButton(sender).Tag/1000);
if (i>=0)and(j>=0) then begin
  tex:=TextureList[i];
  if tex=HistoricalDir then tex:=slash(tex)+ComboBox6.text;
  if DirectoryExists(slash(TexturePath)+slash(tex)+'L'+inttostr(j+1)) then begin
    HistTex:=ComboBox6.Text;
    texturefn[j]:=tex;
    TextureChanged:=true;
  end
    else begin
      TRadioButton(sender).Checked:=false;
//      TRadioButton(sender).Enabled:=false;
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
    if rec[1]='ZZ' then
       rec[2]:='UTC';
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
for i:=0 to tzinfo.ZoneTabCnty.Count-1 do begin
  if tzinfo.ZoneTabCnty[i]=obscountry then begin
     buf:=tzinfo.ZoneTabZone[i];
     if (obscountry = 'ZZ') then
        buf := TzGMT2UTC(buf);
     j:=ComboBoxTZ.Items.Add(buf);
     if (tzinfo.ZoneTabZone[i]=obstz) then ComboBoxTZ.ItemIndex:=j;
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
 if copy(obstz, 1, 3) = 'UTC' then
    obstz := TzUTC2GMT(obstz);
end;

end.

