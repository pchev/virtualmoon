unit craterlist;

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
  u_util, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources;

type

  { Tf_craterlist }

  Tf_craterlist = class(TForm)
    CraterLst: TListBox;
    procedure CraterLstMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  f_craterlist: Tf_craterlist;

implementation

{$R CraterList.lfm}

uses
     u_constant, virtualmoon1;



procedure Tf_craterlist.CraterLstMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Apoint : Tpoint;
    i : integer;
    buf : string;
begin
  APoint.X := X;
  APoint.Y := Y;
  i := CraterLst.ItemAtPos(APoint, True);
  if i>=0 then begin
     buf:=CraterLst.Items[i];
     Firstsearch:=true;
     form1.SearchName(buf,true);
  end;
end;

procedure Tf_craterlist.FormCreate(Sender: TObject);
begin
ScaleFormForFontSize(self,96);
end;

procedure Tf_craterlist.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if key=27 then close;
end;

end.
