unit vmabrowser4;

{$MODE Delphi}{$H+}

{
Copyright (C) 2006 Patrick Chevalley

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

uses  u_translation,
  u_constant, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, mlb2, Grids, {ValEdit,} CheckLst, Math,
  passql, passqlite,u_util, dbutil, ComCtrls, ExtCtrls, IniFiles, LResources;

const maxcol=52;

type

  { TLoadCSV }

  TLoadCSV = class(TForm)
    dbu: TLiteDB;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
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
    Button3: TButton;
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
    procedure CheckListBox1Click(Sender: TObject);
    procedure CheckListBox1ItemClick(Sender: TObject; Index: integer);
    procedure samplenextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AssignConstantClick(Sender: TObject);
    procedure AssignFieldClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
    fieldmode: array[1..maxcol] of byte;  // 0: not used // 1: use constant // 2: use csv field
    fieldlist: array[1..maxcol] of byte;  // coresponding csv field number
    fieldconst: array[1..maxcol] of string; // constant field value
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
  Button3.Caption:=rst_32;
  caption:=rst_33;
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
end;

procedure TLoadCSV.FileSelectClick(Sender: TObject);
begin
chdir(appdir);
OpenDialog1.FileName:=edit1.Text;
if OpenDialog1.Execute then edit1.Text:=OpenDialog1.FileName;
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
  fieldconst[1]:=inttostr(min(99,max(10,1+strtointdef(dbu.queryone('select max(DBN) from moon'),10))));
  Tabsheet2.TabVisible:=true;
  memo1.Clear;
  Tabsheet3.TabVisible:=false;
end
else begin
  Mlb.Clear;
  StringGrid1.RowCount:=2;
  StringGrid1.Cells[0,1]:='';
  StringGrid1.Cells[1,1]:='';
  Label5.Caption:='';
  Pagecontrol1.ActivePageIndex:=0;
  Tabsheet2.TabVisible:=false;
  memo1.Clear;
  Tabsheet3.TabVisible:=false;
  for i:=1 to maxcol do fieldmode[i]:=0;
  for i:=1 to maxcol do fieldList[i]:=0;
  for i:=1 to maxcol do fieldconst[i]:='';
  for i:=0 to maxcol-1 do checklistbox1.Checked[i]:=false;
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
Mlb:=TMlb2.Create;
for i:= 1 to maxcol do fieldmode[i]:=0;
Tabsheet2.TabVisible:=false;
Tabsheet3.TabVisible:=false;
end;

procedure TLoadCSV.FormShow(Sender: TObject);
begin
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
var i: integer;
begin
i:=Checklistbox1.ItemIndex+1;
if (i<1) or (not Checklistbox1.Checked[i-1]) then begin showmessage(rsm_11);exit; end;
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
if (fieldmode[1]=0)or(fieldmode[2]=0)or(fieldmode[20]=0)or(fieldmode[22]=0)
   then Tabsheet3.TabVisible:=false
   else Tabsheet3.TabVisible:=true;
end;

procedure TLoadCSV.CheckListBox1Click(Sender: TObject);
begin
  CheckListBox1ItemClick(sender,CheckListBox1.ItemIndex);
end;

procedure TLoadCSV.Button3Click(Sender: TObject);
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
for i:=1 to maxcol do begin
   if checklistbox1.Checked[i-1] and (fieldmode[i]=0) then errormsg(checklistbox1.Items[i-1]+' '+rsm_17);
   if not checklistbox1.Checked[i-1] then fieldmode[i]:=0;
end;
memo1.Lines.Add(rsm_18);
if fieldmode[1]=0 then errormsg(rsm_19+' '+checklistbox1.Items[0]);
if fieldmode[1]<>1 then errormsg(rsm_1);
if fieldmode[2]=0 then errormsg(rsm_19+' '+checklistbox1.Items[1]);
if fieldmode[20]=0 then errormsg(rsm_19+' '+checklistbox1.Items[19]);
if fieldmode[22]=0 then errormsg(rsm_19+' '+checklistbox1.Items[21]);
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
    if (trim(v)='')and((i=20)or(i=22)or(i=33)or(i=34)or(i=35)or(i=36)or(i=45))
       then v:='0';
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
    for i:=1 to maxcol do
       WriteInteger(section,'FieldMode'+inttostr(i),fieldmode[i]);
    for i:=1 to maxcol do
       WriteInteger(section,'FieldList'+inttostr(i),fieldList[i]);
    for i:=1 to maxcol do
       WriteString(section,'FieldConst'+inttostr(i),fieldconst[i]);
    for i:=0 to maxcol-1 do
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
    for i:=1 to maxcol do
       fieldmode[i]:=ReadInteger(section,'FieldMode'+inttostr(i),fieldmode[i]);
    for i:=1 to maxcol do
       fieldList[i]:=ReadInteger(section,'FieldList'+inttostr(i),fieldList[i]);
    for i:=1 to maxcol do
       fieldconst[i]:=ReadString(section,'FieldConst'+inttostr(i),fieldconst[i]);
    for i:=0 to maxcol-1 do
       checklistbox1.Checked[i]:=ReadBool(section,'FieldChecked'+inttostr(i),checklistbox1.Checked[i]);
  end;
  inif.Free;
  Checklistbox1.Checked[0]:=true;
  fieldmode[1]:=1;
  fieldconst[1]:=inttostr(min(99,max(10,1+strtointdef(dbu.queryone('select max(DBN) from moon'),10))));
  memo1.Clear;
  Tabsheet3.TabVisible:=true;
end;
end;

initialization
  {$i vmabrowser4.lrs}

end.
