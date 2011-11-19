unit vmabrowser3;

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

uses  u_translation, u_util,
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, LResources;

type

  { TColumns }

  TColumns = class(TForm)
    CheckListBox1: TCheckListBox;
    ButtonAll: TButton;
    ButtonNone: TButton;
    ButtonClose: TButton;
    CheckListBox2: TCheckListBox;
    procedure ButtonAllClick(Sender: TObject);
    procedure ButtonNoneClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure CheckListBox1ItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetLang;
  end;

var
  Columns: TColumns;

implementation

{$R vmabrowser3.lfm}

uses u_constant, vmabrowser1;

procedure TColumns.SetLang;
begin
  caption:=rst_4;
  ButtonAll.caption:=rst_5;
  ButtonNone.caption:=rst_6;
  ButtonClose.caption:=rst_7;
end;

procedure TColumns.ButtonAllClick(Sender: TObject);
var i: integer;
begin
for i:=0 to CheckListBox1.Items.Count-1 do
    CheckListBox1.Checked[i]:=(not ((i+1) in hidenfields));
for i:=0 to CheckListBox2.Items.Count-1 do
    CheckListBox2.Checked[i]:=(not ((i+1+CheckListBox1.Count) in hidenfields));
end;

procedure TColumns.ButtonNoneClick(Sender: TObject);
var i: integer;
begin
for i:=2 to CheckListBox1.Items.Count-1 do
    CheckListBox1.Checked[i]:=false;
for i:=0 to CheckListBox2.Items.Count-1 do
    CheckListBox2.Checked[i]:=false;
end;

procedure TColumns.ButtonCloseClick(Sender: TObject);
var i: integer;
begin
for i:=0 to CheckListBox1.Items.Count-1 do begin
  if  CheckListBox1.Checked[i] or (i<2) then begin
     if f_main.MoonGrid.ColWidths[i]=0 then f_main.MoonGrid.ColWidths[i]:=f_main.MoonGrid.DefaultColWidth;
  end else
      f_main.MoonGrid.ColWidths[i]:=0;
end;
for i:=0 to CheckListBox2.Items.Count-1 do begin
  if  CheckListBox2.Checked[i] then begin
     if f_main.MoonGrid.ColWidths[i+26]=0 then f_main.MoonGrid.ColWidths[i+26]:=f_main.MoonGrid.DefaultColWidth;
  end else
      f_main.MoonGrid.ColWidths[i+26]:=0;
end;
ModalResult:=mrOK;
end;

procedure TColumns.CheckListBox1ItemClick(Sender: TObject; Index: integer);
begin
 if Index<=1 then CheckListBox1.Checked[Index]:=true;
end;

procedure TColumns.FormCreate(Sender: TObject);
begin
  {$ifdef mswindows}
  ScaleForm(self,Screen.PixelsPerInch/96);
  {$endif}
end;

end.
