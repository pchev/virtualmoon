unit vmabrowser2;

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

uses u_translation,
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, CheckLst, LResources;

type

  { TSelection }

  TSelection = class(TForm)
    Button20: TButton;
    Button21: TButton;
    CheckListBox2: TCheckListBox;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button12: TButton;
    Button13: TButton;
    sel: TMemo;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    fieldlist: TComboBox;
    Button11: TButton;
    Button10: TButton;
    TabSheet2: TTabSheet;
    RadioGroup2: TRadioGroup;
    fieldlist2: TComboBox;
    colgt: TEdit;
    collt: TEdit;
    colbetween1: TEdit;
    StaticText1: TStaticText;
    colbetween2: TEdit;
    ViewSel: TLabel;
    coleq: TEdit;
    ExpertMode: TCheckBox;
    Button19: TButton;
    CheckListBox1: TCheckListBox;
    ButtonAll: TButton;
    ButtonNone: TButton;
    procedure Button20Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure fieldlistSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure ExpertModeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure ButtonAllClick(Sender: TObject);
    procedure ButtonNoneClick(Sender: TObject);
  private
    { Private declarations }
    oldsel,orderby: string;
    function isDBselectionChanged: boolean;
  public
    { Public declarations }
    lastselection: string;
    Procedure SetLang;
    property DBselectionChanged: boolean read isDBselectionChanged;
  end;

var
  Selection: TSelection;

implementation

{$R vmabrowser2.lfm}

uses vmabrowser5, u_util, u_constant;

Procedure TSelection.SetLang;
var i:integer;
begin
  caption:=rst_3;
  ButtonAll.caption:=rst_5;
  ButtonNone.caption:=rst_6;
  Button11.caption:=rst_9;
  Button9.caption:=rst_10;
  Button13.caption:=rst_11;
  label1.caption:=rst_12;
  Button10.caption:=rst_8;
  TabSheet1.Caption:=rst_21;
  TabSheet3.Caption:=rst_22;
  TabSheet2.Caption:=rst_44;
  RadioGroup2.items[3]:=rst_45;
  StaticText1.Caption:=rst_46;
  button19.Caption:=rst_48;
  ExpertMode.Caption:=rst_49;
  Button20.Caption:=rst_10;
  Button21.Caption:=rsAdd;
  Checklistbox1.Items.Clear;
  for i:=1 to numdbtypepage-1 do
      Checklistbox1.Items.Add(dbtype[i]);
  Checklistbox2.Items.Clear;
  for i:=numdbtypepage to numdbtype do
      Checklistbox2.Items.Add(dbtype[i]);

end;

procedure TSelection.PageControl1Change(Sender: TObject);
var buf: string;
    p: integer;
begin
if PageControl1.ActivePageIndex=1 then begin
  buf:=sel.text;
  p:=pos('ORDER BY',UpperCase(buf));
  if p>0 then begin
    sel.text:=copy(buf,1,p-1);
    orderby:=copy(buf,p+8,9999)+',';
  end;
end;
if PageControl1.ActivePageIndex=3 then begin
  sel.SetFocus;
  sel.SelStart:=length(sel.Text);
  sel.SelLength:=sel.SelStart;
end;  
end;

procedure TSelection.Button10Click(Sender: TObject);
var buf: string;
begin
if (PageControl1.ActivePageIndex=1) then begin
  if (trim(sel.Text)='') or (trim(sel.text)=dbselection) then
    Button21Click(nil);
  if (orderby>'') then begin
    buf:=orderby;
    if copy(buf,length(buf),1)=',' then
      delete(buf,Length(buf),1);
    sel.text:=sel.text+' order by '+buf;
  end;
end;
modalresult:=mrOK;
lastselection:=sel.text;
end;

procedure TSelection.Button11Click(Sender: TObject);
begin
dbselection:=oldsel;
modalresult:=mrCancel;
end;

procedure TSelection.ButtonClick(Sender: TObject);
begin
  sel.Text:=sel.Text+(Sender as TButton).Caption;
  if (Sender as TButton).tag=1 then sel.Text:=sel.Text+' ';
  sel.SetFocus;
  sel.SelStart:=length(sel.Text);
  sel.SelLength:=sel.SelStart;
end;

procedure TSelection.Button9Click(Sender: TObject);
begin
sel.Text:='';
end;

procedure TSelection.Button13Click(Sender: TObject);
begin
if lastselection<>'' then sel.Text:=lastselection;
end;

procedure TSelection.fieldlistSelect(Sender: TObject);
begin
  sel.Text:=sel.Text+Fieldlist.Text+' ';
  sel.SetFocus;
  sel.SelStart:=length(sel.Text);
  sel.SelLength:=sel.SelStart;
end;

procedure TSelection.FormCreate(Sender: TObject);
begin
  {$ifdef mswindows}
  //ScaleForm(self,Screen.PixelsPerInch/96);
  {$endif}
end;

procedure TSelection.CheckListBox1ClickCheck(Sender: TObject);
var i: integer;
    first, allchecked: boolean;
    buf, wildcard: string;
begin
first:=true;
buf:='';
wildcard:='';
allchecked:=true;
for i:=0 to CheckListBox1.Count-1 do
    allchecked:=(allchecked and CheckListBox1.Checked[i]);
for i:=0 to CheckListBox2.Count-1 do
    allchecked:=(allchecked and CheckListBox2.Checked[i]);
if not allchecked then begin
 for i:=0 to CheckListBox1.Count-1 do begin
  if CheckListBox1.Checked[i] then begin
     if first then begin
        buf:=' and ( (TYPE LIKE "'+dbtype[i+1]+wildcard+'")';
        first:=false;
     end else begin
        buf:=buf+ ' or (TYPE LIKE "'+dbtype[i+1]+wildcard+'")';
     end;
  end;
 end; // for
 for i:=0 to CheckListBox2.Count-1 do begin
  if CheckListBox2.Checked[i] then begin
     if i>=8 then wildcard:='%';
     if first then begin
        buf:=' and ( (TYPE LIKE "'+dbtype[i+numdbtypepage]+wildcard+'")';
        first:=false;
     end else begin
        buf:=buf+ ' or (TYPE LIKE "'+dbtype[i+numdbtypepage]+wildcard+'")';
     end;
  end;
 end; // for
 if buf='' then buf:=' and ( (TYPE LIKE "")';
end;
if buf>'' then buf:=buf+' ) ORDER BY NAME';
sel.Text:=dbselection+buf;
ViewSel.Caption:=dbselection;
end;

procedure TSelection.ButtonAllClick(Sender: TObject);
var i: integer;
begin
 for i:=0 to CheckListBox1.Count-1 do CheckListBox1.Checked[i]:=true;
 for i:=0 to CheckListBox2.Count-1 do CheckListBox2.Checked[i]:=true;
 CheckListBox1ClickCheck(Sender);
end;

procedure TSelection.ButtonNoneClick(Sender: TObject);
var i: integer;
begin
 for i:=0 to CheckListBox1.Count-1 do CheckListBox1.Checked[i]:=false;
 for i:=0 to CheckListBox2.Count-1 do CheckListBox2.Checked[i]:=false;
 CheckListBox1ClickCheck(Sender);
end;

procedure TSelection.RadioGroup2Click(Sender: TObject);
begin
case RadioGroup2.ItemIndex of
  0 : begin
        coleq.Enabled:=true;
        colgt.Enabled:=false;
        collt.Enabled:=false;
        colbetween1.Enabled:=false;
        colbetween2.Enabled:=false;
        coleq.Color:=clWindow;
        colgt.Color:=clBtnFace;
        collt.Color:=clBtnFace;
        colbetween1.Color:=clBtnFace;
        colbetween2.Color:=clBtnFace;
      end;
  1 : begin
        coleq.Enabled:=false;
        colgt.Enabled:=true;
        collt.Enabled:=false;
        colbetween1.Enabled:=false;
        colbetween2.Enabled:=false;
        coleq.Color:=clBtnFace;
        colgt.Color:=clWindow;
        collt.Color:=clBtnFace;
        colbetween1.Color:=clBtnFace;
        colbetween2.Color:=clBtnFace;
      end;
  2 : begin
        coleq.Enabled:=false;
        colgt.Enabled:=false;
        collt.Enabled:=true;
        colbetween1.Enabled:=false;
        colbetween2.Enabled:=false;
        coleq.Color:=clBtnFace;
        colgt.Color:=clBtnFace;
        collt.Color:=clWindow;
        colbetween1.Color:=clBtnFace;
        colbetween2.Color:=clBtnFace;
      end;
  3 : begin
        coleq.Enabled:=false;
        colgt.Enabled:=false;
        collt.Enabled:=false;
        colbetween1.Enabled:=true;
        colbetween2.Enabled:=true;
        coleq.Color:=clBtnFace;
        colgt.Color:=clBtnFace;
        collt.Color:=clBtnFace;
        colbetween1.Color:=clWindow;
        colbetween2.Color:=clWindow;
      end;
end;
end;

procedure TSelection.Button21Click(Sender: TObject);
var buf: string;
begin
if trim(sel.Text)='' then
  sel.Text:=dbselection;
case RadioGroup2.ItemIndex of
  0 : begin
        if trim(coleq.Text)<>'' then
           sel.Text:=sel.Text+' and ('+fieldlist2.Text+' LIKE "'+coleq.Text+'")';
      end;
  1 : begin
        if trim(colgt.Text)<>'' then begin
           sel.Text:=sel.Text+' and ('+fieldlist2.Text+' >= "'+colgt.Text+'")';
           if pos(fieldlist2.Text,orderby)=0 then
             orderby:=orderby+fieldlist2.Text+' ASC'+',';
        end;
      end;
  2 : begin
        if trim(collt.Text)<>'' then begin
           sel.Text:=sel.Text+' and ('+fieldlist2.Text+' <= "'+collt.Text+'")';
           if pos(fieldlist2.Text,orderby)=0 then
             orderby:=orderby+fieldlist2.Text+' DESC'+',';
        end;
      end;
  3 : begin
        if (trim(colbetween1.Text)<>'')and(trim(colbetween2.Text)<>'') then begin
           sel.Text:=sel.Text+' and ('+fieldlist2.Text+' BETWEEN "'+colbetween1.Text+'" AND "'+colbetween2.Text+'")';
           if pos(fieldlist2.Text,orderby)=0 then
             orderby:=orderby+fieldlist2.Text+' ASC'+',';
        end;
      end;
end;
if orderby>'' then begin
  buf:=' order by '+orderby;
  if copy(buf,length(buf),1)=',' then
    delete(buf,Length(buf),1);
end
else begin
  buf:='';
end;
Viewsel.Caption:=sel.Text+buf;
end;

procedure TSelection.Button20Click(Sender: TObject);
begin
  sel.Text:=dbselection;
  Viewsel.Caption:=sel.Text;
  orderby:='';
  coleq.Caption:='';
  colgt.Caption:='';
  collt.Caption:='';
  colbetween1.Caption:='';
  colbetween2.Caption:='';
end;

procedure TSelection.ExpertModeClick(Sender: TObject);
begin
TabSheet2.TabVisible:=ExpertMode.Checked;
TabSheet3.TabVisible:=ExpertMode.Checked;
if not ExpertMode.Checked then begin
   PageControl1.ActivePageIndex:=0;
   CheckListBox1ClickCheck(Sender);
end
else begin
  if PageControl1.ActivePageIndex=0 then PageControl1.ActivePageIndex:=1;
end;
end;

procedure TSelection.FormShow(Sender: TObject);
var buf: string;
    p: integer;
begin
oldsel:=dbselection;
ViewSel.Caption:=lastselection;
sel.Text:=lastselection;
if PageControl1.ActivePageIndex=1 then begin
  buf:=sel.text;
  p:=pos('ORDER BY',UpperCase(buf));
  if p>0 then begin
    sel.text:=copy(buf,1,p-1);
    orderby:=copy(buf,p+8,9999)+',';
  end;
end;
ExpertModeClick(Sender);
end;

procedure TSelection.Button19Click(Sender: TObject);
var i,n: integer;
begin
formpos(SelectDB,mouse.CursorPos.X-SelectDB.Width,mouse.CursorPos.Y-SelectDB.Height);
SelectDB.showmodal;
if SelectDB.modalresult=mrOk then begin
   dbselection:='DBN in (';
   n:=0;
   for i:=0 to SelectDB.CheckListBox1.Count-1 do begin
     if SelectDB.CheckListBox1.Checked[i] then begin
        if n>0 then dbselection:=dbselection+',';
        dbselection:=dbselection+SelectDB.dblist[i];
        inc(n);
     end;
   end;
   dbselection:=dbselection+')';
   CheckListBox1ClickCheck(Sender);
end;
end;

function TSelection.isDBselectionChanged: boolean;
begin
result:=dbselection<>oldsel;
end;

end.
