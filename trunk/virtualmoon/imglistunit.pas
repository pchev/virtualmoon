unit imglistunit;
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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TBehavior = (image,formation);
  TImglist = class(TForm)
    ImgLst: TListBox;
    procedure ImgLstMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    Behavior : TBehavior;
  end;

var
  Imglist: TImglist;

implementation

uses
{$IFDEF opengl}
virtualmoon1;
{$ELSE}
virtualmoon2;
{$ENDIF}


{$R *.dfm}

procedure TImglist.ImgLstMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Apoint : Tpoint;
    i,p : integer;
    buf,desc,nom : string;
begin
  APoint.X := X;
  APoint.Y := Y;
  i := ImgLst.ItemAtPos(APoint, True);
  if i>=0 then begin
     buf:=ImgLst.Items[i];
     case Behavior of
     image : begin
             p:=pos(':',buf);
             desc:=trim(copy(buf,1,p-1));
             nom:=trim(copy(buf,p+1,9999));
             ShowImg(desc,nom,false);
             end;
     formation: begin
             Firstsearch:=true;
             SearchName(buf,true);
             end;
     end;
  end;
end;

procedure TImglist.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if key=27 then close;
end;

end.
