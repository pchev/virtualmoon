unit vmabrowser2;

{$MODE Delphi}

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

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, CheckLst, LResources;

type

  { TSelection }

  TSelection = class(TForm)
    CheckListBox2: TCheckListBox;
    PageControl1: TPageControl;
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
    procedure ButtonClick(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure fieldlistSelect(Sender: TObject);
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
  public
    { Public declarations }
    lastselection: string;
  end;

var
  Selection: TSelection;

implementation


uses vmabrowser1, vmabrowser5, u_util;

procedure TSelection.PageControl1Change(Sender: TObject);
begin
if PageControl1.ActivePageIndex=3 then begin
  sel.SetFocus;
  sel.SelStart:=length(sel.Text);
  sel.SelLength:=sel.SelStart;
end;  
end;

procedure TSelection.Button10Click(Sender: TObject);
begin
modalresult:=mrOK;
lastselection:=sel.text;
end;

procedure TSelection.Button11Click(Sender: TObject);
begin
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
        buf:=' and ( (TYPE LIKE "'+f_main.dbtype[i+1]+wildcard+'")';
        first:=false;
     end else begin
        buf:=buf+ ' or (TYPE LIKE "'+f_main.dbtype[i+1]+wildcard+'")';
     end;
  end;
 end; // for
 for i:=0 to CheckListBox2.Count-1 do begin
  if CheckListBox2.Checked[i] then begin
     if i>=8 then wildcard:='%';
     if first then begin
        buf:=' and ( (TYPE LIKE "'+f_main.dbtype[i+14]+wildcard+'")';
        first:=false;
     end else begin
        buf:=buf+ ' or (TYPE LIKE "'+f_main.dbtype[i+14]+wildcard+'")';
     end;
  end;
 end; // for
 if buf='' then buf:=' and ( (TYPE LIKE "")';
end;
if buf>'' then buf:=buf+' ) ORDER BY NAME';
sel.Text:=f_main.dbselection+buf;
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
        if trim(coleq.Text)<>'' then
           sel.Text:=f_main.dbselection+' and ('+fieldlist2.Text+' LIKE "'+coleq.Text+'")';
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
        if trim(colgt.Text)<>'' then
           sel.Text:=f_main.dbselection+' and ('+fieldlist2.Text+' >= "'+colgt.Text+'") ORDER BY '+fieldlist2.Text+' ASC';
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
        if trim(collt.Text)<>'' then
           sel.Text:=f_main.dbselection+' and ('+fieldlist2.Text+' <= "'+collt.Text+'") ORDER BY '+fieldlist2.Text+' DESC';
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
        if (trim(colbetween1.Text)<>'')and(trim(colbetween2.Text)<>'') then
           sel.Text:=f_main.dbselection+' and ('+fieldlist2.Text+' BETWEEN "'+colbetween1.Text+'" AND "'+colbetween2.Text+'") ORDER BY '+fieldlist2.Text+' ASC';
      end;
end;
Viewsel.Caption:=sel.Text;
end;

procedure TSelection.ExpertModeClick(Sender: TObject);
begin
TabSheet2.TabVisible:=ExpertMode.Checked;
TabSheet3.TabVisible:=ExpertMode.Checked;
if not ExpertMode.Checked then begin
   PageControl1.ActivePageIndex:=0;
   CheckListBox1ClickCheck(Sender);
end;
end;

procedure TSelection.FormShow(Sender: TObject);
begin
ViewSel.Caption:=lastselection;
ExpertModeClick(Sender);
end;

procedure TSelection.Button19Click(Sender: TObject);
var i,n: integer;
begin
formpos(SelectDB,mouse.CursorPos.X-SelectDB.Width,mouse.CursorPos.Y-SelectDB.Height);
SelectDB.showmodal;
if SelectDB.modalresult=mrOk then begin
   f_main.dbselection:='DBN in (';
   n:=0;
   for i:=0 to SelectDB.CheckListBox1.Count-1 do begin
     if SelectDB.CheckListBox1.Checked[i] then begin
        if n>0 then f_main.dbselection:=f_main.dbselection+',';
        f_main.dbselection:=f_main.dbselection+SelectDB.dblist[i];
        inc(n);
     end;
   end;
   f_main.dbselection:=f_main.dbselection+')';
   CheckListBox1ClickCheck(Sender);
end;
end;

initialization
  {$i vmabrowser2.lrs}

end.
