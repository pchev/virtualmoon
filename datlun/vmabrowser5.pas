unit vmabrowser5;

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

uses u_translation, u_util,
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, LResources;

type
  TSelectDB = class(TForm)
    CheckListBox1: TCheckListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    dblist: TStringList;
    dbn1,dbn2,dbn3,dbn4,dbn5,dbn6,dbn7: string;
    procedure SetLang;
  end;

var
  SelectDB: TSelectDB;

implementation


uses u_constant;

procedure TSelectDB.SetLang;
begin
  dbn1:=rst_15;
  dbn2:=rst_16;
  dbn3:=rst_18;
  dbn4:=rst_19;
  dbn5:=rst_20;
  dbn6:=rst_51;
  dbn7:=rst_54;
  Button1.Caption:=rst_8;
  Button2.Caption:=rst_9;
  Button3.Caption:=rst_5;
  Button4.Caption:=rst_6;
end;

procedure TSelectDB.FormShow(Sender: TObject);
var i,n: integer;
    lst,buf: string;
begin
lst:='';
i:=pos('(',dbselection);
if i>0 then begin
   lst:=copy(dbselection,i,9999);
   lst:=stringreplace(lst,'(',' ',[rfReplaceAll]);
   lst:=stringreplace(lst,')',' ',[rfReplaceAll]);
   lst:=stringreplace(lst,',',' ',[rfReplaceAll]);
end;
lst:=' '+lst+' ';
CheckListBox1.Clear;
dblist.Clear;
n:=0;
CheckListBox1.Items.Add(dbn1);
dblist.Add('1');
if pos(' 1 ',lst)>0 then CheckListBox1.Checked[n]:=true;
CheckListBox1.Items.Add(dbn2);
dblist.Add('2');
inc(n);
if pos(' 2 ',lst)>0 then CheckListBox1.Checked[n]:=true;
CheckListBox1.Items.Add(dbn3);
dblist.Add('3');
inc(n);
if pos(' 3 ',lst)>0 then CheckListBox1.Checked[n]:=true;
CheckListBox1.Items.Add(dbn4);
dblist.Add('4');
inc(n);
if pos(' 4 ',lst)>0 then CheckListBox1.Checked[n]:=true;
CheckListBox1.Items.Add(dbn5);
dblist.Add('5');
inc(n);
if pos(' 5 ',lst)>0 then CheckListBox1.Checked[n]:=true;
CheckListBox1.Items.Add(dbn6);
dblist.Add('6');
inc(n);
if pos(' 6 ',lst)>0 then CheckListBox1.Checked[n]:=true;
CheckListBox1.Items.Add(dbn7);
dblist.Add('7');
inc(n);
if pos(' 7 ',lst)>0 then CheckListBox1.Checked[n]:=true;
if dbm.Query('select DBN,NAME from user_database') then begin
  for i:=0 to dbm.RowCount-1 do begin
    buf:=dbm.Results[i][0];
    dblist.Add(buf);
    CheckListBox1.Items.Add(dbm.Results[i][1]);
    inc(n);
    if pos(' '+buf+' ',lst)>0 then CheckListBox1.Checked[n]:=true;
  end;
end;
end;

procedure TSelectDB.FormCreate(Sender: TObject);
begin
{$ifdef mswindows}
ScaleForm(self,Screen.PixelsPerInch/96);
{$endif}
dblist:=TStringList.Create;
end;

procedure TSelectDB.FormDestroy(Sender: TObject);
begin
dblist.Free;
end;

procedure TSelectDB.Button3Click(Sender: TObject);
var i: integer;
begin
for i:=0 to CheckListBox1.Items.Count-1 do begin
    CheckListBox1.Checked[i]:=true;
end;
end;

procedure TSelectDB.Button4Click(Sender: TObject);
var i: integer;
begin
for i:=0 to CheckListBox1.Items.Count-1 do begin
    CheckListBox1.Checked[i]:=false;
end;
end;

initialization
  {$i vmabrowser5.lrs}

end.
