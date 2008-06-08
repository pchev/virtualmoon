unit config;
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

uses Math, jpeg,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, Inifiles, Grids, EnhEdits,
  CheckLst;

type
  TForm2 = class(TForm)
    Button1: TButton;
    ColorDialog1: TColorDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ComboBox3: TComboBox;
    CheckBox3: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit3: TEdit;
    TabSheet2: TTabSheet;
    UpDown1: TUpDown;
    numwin: TLongEdit;
    Label8: TLabel;
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
    Button4: TButton;
    TabSheet3: TTabSheet;
    CheckBox4: TCheckBox;
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
    Label19: TLabel;
    TrackBar3: TTrackBar;
    CheckBox15: TCheckBox;
    ruklprefix: TEdit;
    ruklsuffix: TEdit;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    TabSheet4: TTabSheet;
    RadioGroup1: TRadioGroup;
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
    RadioGroup2: TRadioGroup;
    CheckBox10: TCheckBox;
    RadioGroup3: TRadioGroup;
    Label29: TLabel;
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
    nooverlay: TLabel;
    Label34: TLabel;
    CheckBox24: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
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
    procedure RadioGroup2Click(Sender: TObject);
    procedure StringGrid2DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure StringGrid2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid2SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure TrackBar5Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox16Click(Sender: TObject);
    procedure Label34Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    hiresfn: string;
  end;

var
  Form2: TForm2;
  newlang : string;
  savegeol,savelibration,savemipmaps : boolean;
  lockoverlay: boolean=false;
  ov : Tbitmap;
  graytexture:integer;

implementation

uses skylib,
{$IFDEF opengl}
   {$IFDEF openglx}
      virtualmoonx;
   {$ELSE}
     virtualmoon1;
   {$ENDIF}
{$ELSE}
virtualmoon2;
{$ENDIF}

{$R *.DFM}

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
ov:=Tbitmap.Create;
CheckBox4.Visible:=false;
{$IFNDEF opengl}
CheckBox2.Visible:=false;
CheckBox10.Visible:=false;
//panel1.Visible:=false;
{$ENDIF}
// hide developpement tools or not finished function
if not fileexists('version.developpement') then begin
  CheckBox12.Visible:=false;   // external image display
  Edit4.Visible:=false;        // external image display
  Button3.Visible:=false;      // external image display
  checkbox15.Visible:=false;   // direct LOPAM image link
  label20.Visible:=false;      // Rükl chart
  label21.Visible:=false;      // Rükl chart
  label22.Visible:=false;      // Rükl chart
  ruklprefix.Visible:=false;   // Rükl chart
  ruklsuffix.Visible:=false;   // Rükl chart
end;
{$ifdef vmalight}
  CheckBox3.Visible:=false;
  checkbox18.Visible:=false;
  checkbox20.Visible:=false;
  checkbox22.Visible:=false;
  RadioGroup3.Visible:=false;
  Tabsheet6.TabVisible:=false;
{$endif}
{$ifdef vmabasic}
  checkbox21.Visible:=false;
  CheckBox22.Visible:=false;
  radiogroup2.Visible:=false;
  RadioGroup3.Visible:=false;
  Tabsheet6.TabVisible:=false;
{$endif}
{$ifndef vmapro}
  checkbox24.Visible:=false;
  TabSheet6.TabVisible:=false;
  Label31.Visible:=false;
  CheckListBox1.Visible:=false;
{$endif}
{$ifdef opengl}
  OverlayPanel.visible:=AsMultiTexture;
{$endif}
AVLver:=copy(AVLversion,1,3);
i:=findfirst(slash(appdir)+slash('language')+'lang_*.ini',0,fs);
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
  code:=copy(code,6,p-6);
  buf:=code+' '+buf+ver;
  combobox3.Items.Add(buf);
  i:=findnext(fs);
end;
findclose(fs);
for j:=0 to combobox3.Items.Count-1 do if GetLangCode(combobox3.Items[j])=language then combobox3.ItemIndex:=j;
i:=findfirst(Slash(appdir)+Slash('textures')+Slash('overlay')+'*.jpg',0,fs);
combobox5.clear;
combobox5.Sorted:=true;
while i=0 do begin
  combobox5.Items.Add(remext(fs.name));
  i:=findnext(fs);
end;
findclose(fs);
savegeol:=GeologicalMap;
savelibration:=librationeffect;
savemipmaps:=MipMaps;
end;

procedure TForm2.FormDestroy(Sender: TObject);
var i: integer;
begin
ov.Free;
for i:=0 to Checklistbox1.Count-1 do (Checklistbox1.Items.Objects[i] as TDBinfo).Free;
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

procedure TForm2.CheckBox4Click(Sender: TObject);
begin
if CheckBox4.checked then begin
  CheckBox1.enabled:=true;
  CheckBox2.enabled:=true;
  CheckBox2.checked:=savelibration;
  CheckBox10.enabled:=true;
  CheckBox10.checked:=savemipmaps;
end else begin
  CheckBox1.enabled:=true;
  CheckBox2.checked:=false;
  CheckBox2.enabled:=false;
  CheckBox10.checked:=false;
  CheckBox10.enabled:=false;
end;
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
GetPrinterResolution(PrtName,PrinterResolution);
end;

procedure TForm2.FormShow(Sender: TObject);
var myRect: TGridRect;
begin
  myRect.Left := 0;
  myRect.Top := 3;
  myRect.Right := 0;
  myRect.Bottom := 3;
  StringGrid1.Selection := myRect;
//  Panel1.Visible:=checkbox4.Checked;
  TrackBar2.Min:=-1000;
  TrackBar2.Max:=-100;
  if hiresfile='hires_light.jpg' then graytexture:=0
                               else graytexture:=80;
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
begin
{$ifdef vmaexpert}
case RadioGroup2.itemindex of
0 : hiresfn:='hires.jpg';
1 : hiresfn:='hires_clem.jpg';
2 : hiresfn:='hires_lopam.jpg';
end;
if not fileexists(Slash(appdir)+Slash('textures')+hiresfn) then begin
 hiresfn:=hiresfile;
 if hiresfile='hires.jpg' then form2.radiogroup2.itemindex:=0
   else if hiresfile='hires_clem.jpg' then form2.radiogroup2.itemindex:=1
   else if hiresfile='hires_lopam.jpg' then form2.radiogroup2.itemindex:=2
   else form2.radiogroup2.itemindex:=-1;
end;
if hiresfn='hires_light.jpg' then graytexture:=0
                               else graytexture:=80;
{$endif}
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
var  j:tjpegimage;
begin
if fileexists(Slash(appdir)+Slash('textures')+Slash('overlay')+combobox5.text+'.jpg') then begin
   j:=tjpegimage.create;
   try
   j.LoadFromFile(Slash(appdir)+Slash('textures')+Slash('overlay')+combobox5.text+'.jpg');
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
var ma:double;
    i,n,l : integer;
    p1,p2: pbytearray;
begin
if not ov.Empty then begin
   if lockoverlay then exit;
   lockoverlay:=true;
   try
    image1.Picture.bitmap.Width:=ov.Width;
    image1.Picture.bitmap.Height:=ov.Height;
    image1.Picture.bitmap.pixelformat:=ov.pixelformat;
    l:=trackbar5.Position;
    ma:=(255-l)/255;
    for i:=0 to image1.Picture.bitmap.height-1 do begin
     p1:=ov.scanline[i];
     p2:=image1.Picture.bitmap.scanline[i];
     for n:=0 to image1.Picture.bitmap.width-1 do begin
       case ov.pixelformat of
         pf8bit  : p2[n]:=max(0,l+trunc(p1[n]*ma)-graytexture);
         pf24bit : begin
                   p2[3*n]:=max(0,l+trunc(p1[3*n]*ma)-graytexture);
                   p2[3*n+1]:=max(0,l+trunc(p1[3*n+1]*ma)-graytexture);
                   p2[3*n+2]:=max(0,l+trunc(p1[3*n+2]*ma)-graytexture);
                   end;
       end;
     end;
    end;
    image1.Refresh;
   finally
    lockoverlay:=false;
   end;
end;
end;

procedure TForm2.CheckBox16Click(Sender: TObject);
begin
Label3.Enabled:=not CheckBox16.Checked;
Label29.Enabled:=not CheckBox16.Checked;
Edit3.Enabled:=not CheckBox16.Checked;
end;

procedure TForm2.Label34Click(Sender: TObject);
begin
executefile('http://www.delphi3d.net/hardware/extsupport.php?extension=GL_ARB_multitexture','', '', SW_SHOWNOACTIVATE);
end;

end.

