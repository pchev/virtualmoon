unit vmabrowser5;

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

uses u_translation, u_util,
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, LResources, ExtCtrls;

type

  { TSelectDB }

  TSelectDB = class(TForm)
    CheckListBox1: TCheckListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Panel1: TPanel;
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
    procedure SetLang;
  end;

var
  SelectDB: TSelectDB;

implementation

{$R vmabrowser5.lfm}

uses u_constant;

procedure TSelectDB.SetLang;
begin
  Button1.Caption:=rst_8;
  Button2.Caption:=rst_9;
  Button3.Caption:=rst_5;
  Button4.Caption:=rst_6;
end;

procedure TSelectDB.FormShow(Sender: TObject);
var i,j,n: integer;
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
for i:=1 to DatabaseList.Count do begin
  buf:=inttostr(i);
  n:=CheckListBox1.Items.Add(DatabaseList[i-1]);
  dblist.Add(buf);
  if pos(' '+buf+' ',lst)>0 then CheckListBox1.Checked[n]:=true;
end;
if dbm.Query('select DBN,NAME from user_database') then begin
  for i:=0 to dbm.RowCount-1 do begin
    buf:=dbm.Results[i][0];
    dblist.Add(buf);
    n:=CheckListBox1.Items.Add(dbm.Results[i][1]);
    if pos(' '+buf+' ',lst)>0 then CheckListBox1.Checked[n]:=true;
  end;
end;
end;

procedure TSelectDB.FormCreate(Sender: TObject);
begin
{$ifdef mswindows}
//ScaleForm(self,Screen.PixelsPerInch/96);
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
  CheckListBox1.Checked[i]:=CheckListBox1.Items[i]<>'-';
end;
end;

procedure TSelectDB.Button4Click(Sender: TObject);
var i: integer;
begin
for i:=0 to CheckListBox1.Items.Count-1 do begin
    CheckListBox1.Checked[i]:=false;
end;
end;

end.
