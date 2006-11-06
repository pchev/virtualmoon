unit vmabrowser3;
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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst;

type
  TColumns = class(TForm)
    CheckListBox1: TCheckListBox;
    ButtonAll: TButton;
    ButtonNone: TButton;
    ButtonClose: TButton;
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure ButtonAllClick(Sender: TObject);
    procedure ButtonNoneClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Columns: TColumns;

implementation

{$R *.dfm}

uses vmabrowser1;

procedure TColumns.CheckListBox1ClickCheck(Sender: TObject);
var i: integer;
begin
i:=CheckListBox1.ItemIndex;
if i<=1 then CheckListBox1.Checked[i]:=true
else begin
  if  CheckListBox1.Checked[i] then
      Form1.MoonGrid.ColWidths[i]:=Form1.MoonGrid.DefaultColWidth
  else
      Form1.MoonGrid.ColWidths[i]:=-1;
end;
end;

procedure TColumns.ButtonAllClick(Sender: TObject);
var i: integer;
begin
for i:=0 to CheckListBox1.Items.Count-1 do begin
    CheckListBox1.Checked[i]:=true;
    if Form1.MoonGrid.ColWidths[i]=-1 then Form1.MoonGrid.ColWidths[i]:=Form1.MoonGrid.DefaultColWidth;
end;                                
end;

procedure TColumns.ButtonNoneClick(Sender: TObject);
var i: integer;
begin
for i:=2 to CheckListBox1.Items.Count-1 do begin
    CheckListBox1.Checked[i]:=false;
    Form1.MoonGrid.ColWidths[i]:=-1;
end;
end;

procedure TColumns.ButtonCloseClick(Sender: TObject);
begin
ModalResult:=mrOK;
end;

end.   
