unit vmabrowser4;

{$MODE Delphi}{$H+}

{
Copyright (C) 2006 Patrick Chevalley

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

uses  u_translation,
  u_constant, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, mlb2, Grids, {ValEdit,} CheckLst, Math,
  passql, passqlite,u_util, dbutil, ComCtrls, ExtCtrls, IniFiles, LResources;

type

  { TLoadCSV }

  TLoadCSV = class(TForm)
    Button3: TButton;
    Button4: TButton;
    ButtonTemplate: TButton;
    ButtonExpert: TButton;
    ButtonSimple: TButton;
    ButtonLoadSimplified: TButton;
    ButtonNext2: TButton;
    ButtonNext3: TButton;
    dbu: TLiteDB;
    Edit2: TEdit;
    FileSelect1: TButton;
    Label10: TLabel;
    Memo3: TMemo;
    Memo4: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    SaveDialog2: TSaveDialog;
    TabSheet1: TTabSheet;
    Edit1: TEdit;
    FileSelect: TButton;
    SepBox: TComboBox;
    Label1: TLabel;
    TabSheet2: TTabSheet;
    samplenext: TButton;
    sampleprev: TButton;
    StringGrid1: TStringGrid;
    AssignField: TButton;
    CheckListBox1: TCheckListBox;
    ConstantText: TEdit;
    AssignConstant: TButton;
    Msg: TEdit;
    TabSheet3: TTabSheet;
    ButtonLoadDb: TButton;
    Label2: TLabel;
    QuoteBox: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Memo1: TMemo;
    memo2: TMemo;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Label7: TLabel;
    Button1: TButton;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Label8: TLabel;
    Button2: TButton;
    Bevel6: TBevel;
    SaveDialog1: TSaveDialog;
    OpenDialog2: TOpenDialog;
    Label9: TLabel;
    TabSheetSimple: TTabSheet;
    TabSheetExpert: TTabSheet;
    procedure ButtonExpertClick(Sender: TObject);
    procedure ButtonLoadSimplifiedClick(Sender: TObject);
    procedure ButtonNext2Click(Sender: TObject);
    procedure ButtonSimpleClick(Sender: TObject);
    procedure ButtonTemplateClick(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure CheckListBox1ItemClick(Sender: TObject; Index: integer);
    procedure FileSelect1Click(Sender: TObject);
    procedure samplenextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AssignConstantClick(Sender: TObject);
    procedure AssignFieldClick(Sender: TObject);
    procedure ButtonLoadDbClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure sampleprevClick(Sender: TObject);
    procedure FileSelectClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    mlb:Tmlb2;
    samplerec:integer;
    fieldmode: array[1..NumMoonDBFields] of byte;  // 0: not used // 1: use constant // 2: use csv field
    fieldlist: array[1..NumMoonDBFields] of byte;  // coresponding csv field number
    fieldconst: array[1..NumMoonDBFields] of string; // constant field value
    procedure resetselection;
    procedure GetSampleData;
  public
    { Public declarations }
    dbname:string;
    procedure SetLang;
  end;

var
  LoadCSV: TLoadCSV;

implementation

{$R vmabrowser4.lfm}

procedure TLoadCSV.SetLang;
begin
  label9.Caption:=rst_17;
  TabSheet1.Caption:=rst_23;
  TabSheet2.Caption:=rst_24;
  TabSheet3.Caption:=rst_25;
  label1.Caption:=rst_26;
  label2.Caption:=rst_27;
  label4.Caption:=rst_28;
  label3.Caption:=rst_29;
  label6.Caption:=rst_30;
  AssignConstant.Caption:=rst_31;
  AssignField.Hint:=rst_47;
  ButtonLoadDb.Caption:=rst_32;
  caption:=rst_34;
  label7.Caption:=rst_37;
  Button1.Caption:=rst_38;
  label8.Caption:=rst_39;
  Button2.Caption:=rst_40;
  memo2.Lines.Clear;
  memo2.Lines.Add(rsloadcsv01);
  memo2.Lines.Add(rsloadcsv02);
  memo2.Lines.Add('');
  memo2.Lines.Add(rsloadcsv03);
  memo2.Lines.Add(rsloadcsv04);
  memo2.Lines.Add(rsloadcsv05);
  memo2.Lines.Add('');
  memo2.Lines.Add(rsloadcsv06);
  memo2.Lines.Add('');
  memo2.Lines.Add(rsloadcsv07);
  memo2.Lines.Add(rsloadcsv08);
  memo2.Lines.Add('');
  memo2.Lines.Add(rsloadcsv09);
  memo2.Lines.Add(rsloadcsv10);
  memo2.Lines.Add('');
  memo2.Lines.Add(rsloadcsv11);
  memo2.Lines.Add(rsloadcsv12);
  memo2.Lines.Add('');
  memo2.Lines.Add(rsloadcsv13);
  memo2.Lines.Add(rsloadcsv14);

  ButtonTemplate.Caption:=rsCreateTempla;
  Label10.Caption:=rst_28;
  ButtonLoadSimplified.Caption:=rst_32;
  ButtonExpert.Caption:=rst_49;
  Button3.Caption:=rst_7;
  ButtonSimple.Caption:=rsSimpleMode;
  ButtonNext2.Caption:=rsNext;
  ButtonNext3.Caption:=rsNext;

  memo4.Lines.Clear;
  memo4.Lines.Add(rsThisProgramL);
  memo4.Lines.Add('');
  memo4.Lines.Add(rsTheSimplifie);
  memo4.Lines.Add(rsIfYouNeedToA);
  memo4.Lines.Add('');
  memo4.Lines.Add(rsTheFormatOfT);
  memo4.Lines.Add('Formation name; Longitude -180/+180; Latitude; Dimension km; Description');
  memo4.Lines.Add('');
  memo4.Lines.Add('"Formation name" '+rsIsTheNameYou);
  memo4.Lines.Add('"Longitude -180/+180" '+rsIsTheFormati);
  memo4.Lines.Add('"Latitude" '+rsIsTheFormati2);
  memo4.Lines.Add('"Dimension km" '+rsIsTheSizeOfT);
  memo4.Lines.Add('"Description" '+rsIsAnyTextInf);
  memo4.Lines.Add('');
  memo4.Lines.Add(rsTheColumnSep);
  memo4.Lines.Add(rsClicTheButto);

end;

procedure TLoadCSV.FileSelect1Click(Sender: TObject);
begin
chdir(appdir);
OpenDialog1.FileName:=Edit2.Text;
if OpenDialog1.Execute then Edit2.Text:=OpenDialog1.FileName;
chdir(appdir);
end;


procedure TLoadCSV.FileSelectClick(Sender: TObject);
begin
chdir(appdir);
OpenDialog1.FileName:=Edit1.Text;
if OpenDialog1.Execute then Edit1.Text:=OpenDialog1.FileName;
chdir(appdir);
end;

procedure TLoadCSV.Edit1Change(Sender: TObject);
var i : integer;
begin
if SepBox.Text='TAB' then Mlb.CSVSeparator:=chr(09)
   else Mlb.CSVSeparator:=SepBox.Text;
Mlb.QuoteSeparator:=QuoteBox.Text;
if Mlb.LoadFromFile(edit1.text) then begin
  Mlb.GoFirst;
  samplerec:=Mlb.GetCurrentRow;
  StringGrid1.RowCount:=Mlb.FieldCount+1;
  for i:=1 to Mlb.FieldCount do begin
    StringGrid1.Cells[0,i]:=Mlb.GetFieldName(i);
    StringGrid1.Cells[1,i]:=Mlb.GetDataByIndex(i);
  end;
  Label5.Caption:=inttostr(Mlb.RowCount)+' '+rsm_8;
  Checklistbox1.Checked[0]:=true;
  fieldmode[1]:=1;
  fieldconst[1]:=inttostr(min(200,max(101,1+strtointdef(dbu.queryone('select max(DBN) from moon'),99))));
  Tabsheet2.TabVisible:=true;
  ButtonNext2.Visible:=true;
  memo1.Clear;
  Tabsheet3.TabVisible:=false;
  ButtonNext3.Visible:=false;
end
else begin
  Mlb.Clear;
  StringGrid1.RowCount:=2;
  StringGrid1.Cells[0,1]:='';
  StringGrid1.Cells[1,1]:='';
  Label5.Caption:='';
  Pagecontrol1.ActivePageIndex:=0;
  Tabsheet2.TabVisible:=false;
  ButtonNext2.Visible:=false;
  memo1.Clear;
  Tabsheet3.TabVisible:=false;
  ButtonNext3.Visible:=false;
  for i:=1 to NumMoonDBFields do fieldmode[i]:=0;
  for i:=1 to NumMoonDBFields do fieldList[i]:=0;
  for i:=1 to NumMoonDBFields do fieldconst[i]:='';
  for i:=0 to NumMoonDBFields-1 do checklistbox1.Checked[i]:=false;
end;
resetselection;
end;

procedure TLoadCSV.GetSampleData;
var i : integer;
begin
Mlb.Go(samplerec);
for i:=1 to Mlb.FieldCount do begin
  StringGrid1.Cells[1,i]:=Mlb.GetDataByIndex(i);
end;
end;

procedure TLoadCSV.samplenextClick(Sender: TObject);
begin
inc(samplerec);
samplerec:=min(samplerec,Mlb.RowCount);
GetSampleData;
end;

procedure TLoadCSV.sampleprevClick(Sender: TObject);
begin
dec(samplerec);
samplerec:=max(samplerec,1);
GetSampleData;
end;

procedure TLoadCSV.resetselection;
var sel: TGridRect;
begin
sel.Top:=0;
sel.Left:=1;
sel.Bottom:=0;
sel.Right:=1;
StringGrid1.Selection:=sel;
end;

procedure TLoadCSV.FormCreate(Sender: TObject);
var i: integer;
begin
{$ifdef mswindows}
//ScaleForm(self,Screen.PixelsPerInch/96);
{$endif}
Mlb:=TMlb2.Create;
for i:= 1 to NumMoonDBFields do fieldmode[i]:=0;
end;

procedure TLoadCSV.FormShow(Sender: TObject);
begin
PageControl1.ActivePageIndex:=0;
PageControl2.ActivePageIndex:=0;
Tabsheet2.TabVisible:=false;
Tabsheet3.TabVisible:=false;
ButtonNext2.Visible:=false;
ButtonNext3.Visible:=false;
StringGrid1.Cells[0,0]:=rsm_9;
StringGrid1.Cells[1,0]:=rsm_10;
dbu.DataBase:=dbname;
edit1.Text:='';
end;

procedure TLoadCSV.FormDestroy(Sender: TObject);
begin
Mlb.Free;
end;

procedure TLoadCSV.AssignConstantClick(Sender: TObject);
var i,v: integer;
begin
i:=Checklistbox1.ItemIndex+1;
if (i<1) or (not Checklistbox1.Checked[i-1]) then begin showmessage(rsm_11);exit; end;
v:=StrToIntDef(ConstantText.Text,0);
if (i=FDBN)and((v<101)or(v>199)) then begin showmessage('DBN must be comprise between 101 and 199');exit; end;
fieldmode[i]:=1;
fieldconst[i]:=ConstantText.Text;
CheckListBox1ItemClick(Sender,i-1);
end;

procedure TLoadCSV.AssignFieldClick(Sender: TObject);
var i,j: integer;
begin
i:=Checklistbox1.ItemIndex+1;
if i<1 then begin showmessage(rsm_11);exit; end;
j:=StringGrid1.Selection.Top;
if j<1 then begin showmessage(rsm_12);exit; end;
fieldmode[i]:=2;
fieldlist[i]:=j;
CheckListBox1ItemClick(Sender,i-1);
end;

procedure TLoadCSV.CheckListBox1ItemClick(Sender: TObject; Index: integer);
var i: integer;
begin
i:=Index+1;
case fieldmode[i] of
 0: begin
    msg.Text:=rsm_13;
    ConstantText.Text:='';
    end;
 1: begin
    msg.Text:=rsm_14+': '+fieldconst[i];
    ConstantText.Text:=fieldconst[i];
    end;
 2: begin
    msg.Text:=rsm_15+': '+StringGrid1.Cells[0,fieldlist[i]];
    ConstantText.Text:='';
    end;
end;
memo1.Clear;
if (fieldmode[FDBN]=0)or(fieldmode[FNAME]=0)or(fieldmode[FLONGIN]=0)or(fieldmode[FLATIN]=0)
   then begin
     Tabsheet3.TabVisible:=false;
     ButtonNext3.Visible:=false;
   end
   else begin
     Tabsheet3.TabVisible:=true;
     ButtonNext3.Visible:=true;
   end;
end;

procedure TLoadCSV.CheckListBox1Click(Sender: TObject);
begin
  CheckListBox1ItemClick(sender,CheckListBox1.ItemIndex);
end;

procedure TLoadCSV.ButtonNext2Click(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=PageControl1.ActivePageIndex+1;
end;

procedure TLoadCSV.ButtonSimpleClick(Sender: TObject);
begin
  PageControl2.ActivePageIndex:=0;
end;

procedure TLoadCSV.ButtonTemplateClick(Sender: TObject);
var f: textfile;
begin
  if SaveDialog2.Execute then begin
    AssignFile(f,savedialog2.filename);
    Rewrite(f);
    WriteLn(f,'Formation name; Longitude -180/+180; Latitude; Dimension km; Description');
    CloseFile(f);
  end;
end;

procedure TLoadCSV.ButtonExpertClick(Sender: TObject);
begin
  PageControl2.ActivePageIndex:=1;
end;

procedure TLoadCSV.ButtonLoadSimplifiedClick(Sender: TObject);
var i: integer;
    cmd,v: string;
begin
try
  Memo3.Clear;
  Mlb.CSVSeparator:=';';
  Mlb.QuoteSeparator:='';
  if Mlb.LoadFromFile(Edit2.Text) then begin
    if Mlb.FieldCount=5 then begin
      // insert to database
      mlb.GoFirst;
      memo3.Lines.Add(rsm_21+' '+dbname);
      dbu.DataBase:=dbname;
      dbjournal(extractfilename(dbu.DataBase),'DELETE WHERE DBN='+inttostr(SimplifiedDBN));
      dbu.query('delete from moon where DBN='+inttostr(SimplifiedDBN)+';');
      dbjournal(extractfilename(dbu.DataBase),'LOAD DATABASE DBN='+inttostr(SimplifiedDBN)+' FROM FILE: '+mlb.Name);
      dbu.StartTransaction;
      repeat
        cmd:='insert into moon values(NULL,'+inttostr(SimplifiedDBN)+',';
        for i:=1 to NumMoonDBFields do begin
          case i of
           FNAME-1:     v:=mlb.GetDataByIndex(1);
           FLONGIN-1:   v:=mlb.GetDataByIndex(2);
           FLATIN-1:    v:=mlb.GetDataByIndex(3);
           FWIDEKM-1:   v:=mlb.GetDataByIndex(4);
           FGENERAL1-1: v:=mlb.GetDataByIndex(5);
           else       v:='';
          end;
          v:=stringreplace(v,',','.',[rfreplaceall]);
          v:=stringreplace(v,'""','''',[rfreplaceall]);
          v:=stringreplace(v,'"','',[rfreplaceall]);
          cmd:=cmd+'"'+v+'",';
        end;
        cmd:=copy(cmd,1,length(cmd)-1)+');';
        if not dbu.Query(cmd) then raise exception.Create(rsm_24+crlf+dbu.GetErrorMessage+crlf+cmd);
        mlb.GoNext;
      until mlb.EndOfFile;
      dbu.Commit;
      dbjournal(extractfilename(dbu.DataBase),'INSERT DBN='+inttostr(SimplifiedDBN)+' MAX ID='+inttostr(dbu.GetLastInsertID));
      cmd:='delete from user_database where DBN='+inttostr(SimplifiedDBN)+';';
      dbu.Query(cmd);
      cmd:='insert into user_database values('+inttostr(SimplifiedDBN)+',"'+extractfilename(mlb.Name)+'");';
      dbu.Query(cmd);
      memo3.Lines.Add(rsm_22);
      memo3.Lines.Add(rsm_23);
      dbselection:='DBN in ('+inttostr(SimplifiedDBN)+')';
    end
    else begin
      Memo3.Lines.Add(rsIncorrectNum+' '+IntToStr(Mlb.FieldCount));
      Memo3.Lines.Add(rsMustBe+' 5');
      Mlb.clear;
    end;
  end
  else begin
    Memo3.Lines.Add(rsErrorReading+' '+Edit2.Text);
    Memo3.Lines.Add(mlb.MLBErrorComment);
  end;
except
  on E: Exception do begin
    Memo3.Lines.Add(rsError);
    Memo3.Lines.Add(E.Message);
    dbu.Rollback;
  end;
end;
end;

procedure TLoadCSV.ButtonLoadDbClick(Sender: TObject);
var i: integer;
    cmd,v,dbn: string;
Procedure ErrorMsg(txt:string);
begin
  Pagecontrol1.ActivePageIndex:=1;
  raise exception.Create(txt);
end;
begin
memo1.Clear;
memo1.Lines.Add(rsm_16);
for i:=1 to NumMoonDBFields do begin
   if checklistbox1.Checked[i-1] and (fieldmode[i]=0) then errormsg(checklistbox1.Items[i-1]+' '+rsm_17);
   if not checklistbox1.Checked[i-1] then fieldmode[i]:=0;
end;
memo1.Lines.Add(rsm_18);
if fieldmode[FDBN]=0 then errormsg(rsm_19+' '+checklistbox1.Items[FDBN-1]);
if fieldmode[FDBN]<>1 then errormsg(rsm_1);
if fieldmode[FNAME]=0 then errormsg(rsm_19+' '+checklistbox1.Items[FNAME-1]);
if fieldmode[FLONGIN]=0 then errormsg(rsm_19+' '+checklistbox1.Items[FLONGIN-1]);
if fieldmode[FLATIN]=0 then errormsg(rsm_19+' '+checklistbox1.Items[FLATIN]);
memo1.Lines.Add(rsm_20);
// insert to database
mlb.GoFirst;
memo1.Lines.Add(rsm_21+' '+dbname);
dbjournal(extractfilename(dbu.DataBase),'LOAD DATABASE DBN='+fieldconst[1]+' FROM FILE: '+mlb.Name);
dbu.DataBase:=dbname;
dbu.StartTransaction;
try
repeat
  cmd:='insert into moon values(NULL,';
  for i:=1 to checklistbox1.Items.Count do begin
    case fieldmode[i] of
     0: begin
        v:='';
        end;
     1: begin
        v:=fieldconst[i];
        end;
     2: begin
        v:=mlb.GetDataByIndex(fieldlist[i]);
        end;
    end;
    v:=stringreplace(v,',','.',[rfreplaceall]);
    v:=stringreplace(v,'""','''',[rfreplaceall]);
    v:=stringreplace(v,'"','',[rfreplaceall]);
    if i=1 then dbn:=v;
    cmd:=cmd+'"'+v+'",';
  end;
  cmd:=copy(cmd,1,length(cmd)-1)+');';
  if not dbu.Query(cmd) then raise exception.Create(rsm_24+crlf+dbu.GetErrorMessage+crlf+cmd);
  mlb.GoNext;
until mlb.EndOfFile;
dbjournal(extractfilename(dbu.DataBase),'INSERT DBN='+fieldconst[1]+' MAX ID='+inttostr(dbu.GetLastInsertID));
cmd:='delete from user_database where DBN='+dbn+');';
dbu.Query(cmd);
cmd:='insert into user_database values('+dbn+',"'+extractfilename(mlb.Name)+'");';
dbu.Query(cmd);
dbu.Commit;
memo1.Lines.Add(rsm_22);
memo1.Lines.Add(rsm_23);
dbselection:='DBN in ('+dbn+')';
except
  dbu.Rollback;
  memo1.Lines.Add(rsm_24);
  memo1.Lines.Add(rsm_25+' '+rsm_26);
  raise
end;
end;

procedure TLoadCSV.Button2Click(Sender: TObject);
var i:integer;
    inif : TMemIniFile;
    section: string;
begin
chdir(appdir);
if Savedialog1.Execute then begin
  chdir(appdir);
  inif:=Tmeminifile.create(Savedialog1.FileName);
  with inif do begin
    section:='VMA_CSV_to_DATABASE';
    WriteString(section,'FieldSep',SepBox.Text);
    WriteString(section,'TextSep',QuoteBox.Text);
    WriteString(section,'InputFile',Edit1.Text);
    for i:=1 to NumMoonDBFields do
       WriteInteger(section,'FieldMode'+inttostr(i),fieldmode[i]);
    for i:=1 to NumMoonDBFields do
       WriteInteger(section,'FieldList'+inttostr(i),fieldList[i]);
    for i:=1 to NumMoonDBFields do
       WriteString(section,'FieldConst'+inttostr(i),fieldconst[i]);
    for i:=0 to NumMoonDBFields-1 do
       WriteBool(section,'FieldChecked'+inttostr(i),checklistbox1.Checked[i]);
  end;
  inif.UpdateFile;
  inif.Free;
end;
end;

procedure TLoadCSV.Button1Click(Sender: TObject);
var i:integer;
    inif : TMemIniFile;
    section: string;
begin
chdir(appdir);
if Opendialog2.Execute then begin
  chdir(appdir);
  inif:=Tmeminifile.create(Opendialog2.FileName);
  with inif do begin
    section:='VMA_CSV_to_DATABASE';
    SepBox.Text:=ReadString(section,'FieldSep',SepBox.Text);
    QuoteBox.Text:=ReadString(section,'TextSep',QuoteBox.Text);
    Edit1.Text:=ReadString(section,'InputFile',Edit1.Text);
    for i:=1 to NumMoonDBFields do
       fieldmode[i]:=ReadInteger(section,'FieldMode'+inttostr(i),fieldmode[i]);
    for i:=1 to NumMoonDBFields do
       fieldList[i]:=ReadInteger(section,'FieldList'+inttostr(i),fieldList[i]);
    for i:=1 to NumMoonDBFields do
       fieldconst[i]:=ReadString(section,'FieldConst'+inttostr(i),fieldconst[i]);
    for i:=0 to NumMoonDBFields-1 do
       checklistbox1.Checked[i]:=ReadBool(section,'FieldChecked'+inttostr(i),checklistbox1.Checked[i]);
  end;
  inif.Free;
  Checklistbox1.Checked[0]:=true;
  fieldmode[1]:=1;
  fieldconst[1]:=inttostr(min(200,max(101,1+strtointdef(dbu.queryone('select max(DBN) from moon'),99))));
  memo1.Clear;
  Tabsheet3.TabVisible:=true;
  ButtonNext3.Visible:=true;
end;
end;

end.
