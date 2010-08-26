unit fiximg1;
{
Copyright (C) 2007 Patrick Chevalley

http://www.astrosurf.com/astropc
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
{
 This program must be compiled with Delphi !

 It fix buggy 8 bit jpeg format by saving the picture again using 24 bit.
 Also set even size to avoid a Lazarus bug.

}

interface

uses jpeg,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, FoldrDlg, XPMan;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Timer1: TTimer;
    Label2: TLabel;
    Panel1: TPanel;
    Edit1: TEdit;
    SelDir: TButton;
    btnok: TButton;
    btncancel: TButton;
    FolderDialog1: TFolderDialog;
    XPManifest1: TXPManifest;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SelDirClick(Sender: TObject);
    procedure btnokClick(Sender: TObject);
    procedure btncancelClick(Sender: TObject);
  private
    { Private declarations }
    fixdirectory: boolean;
    procedure fixjpeg8bit(dir: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  dir: string;

implementation

{$R *.dfm}

Function Slash(nom : string) : string;
begin
result:=trim(nom);
if copy(result,length(nom),1)<>PathDelim then result:=result+PathDelim;
end;

procedure TForm1.fixjpeg8bit(dir: string);
var jp,jf:TJPEGImage;
    bb: TBitmap;
    f: TSearchRec;
    r: integer;
    fn: string;
procedure convjpeg;
begin
try
    jp.Grayscale:=false;
    jp.PixelFormat:=jf24Bit;
    jp.LoadFromFile(fn);
    bb.Assign(jp);
    bb.PixelFormat:=pf24bit;
    if odd(bb.Width) then bb.Width:=bb.Width-1;
    if odd(bb.Height) then bb.Height:=bb.Height-1;
    jf.Assign(bb);
    jf.PixelFormat:=jf24Bit;
    jf.CompressionQuality:=70;
    jf.Grayscale:=false;
    jf.SaveToFile(fn);
except
end;
end;
begin
try
jp:=TJPEGImage.create;
jf:=TJPEGImage.create;
bb:=TBitmap.Create;
if fixdirectory then begin
   try
   r:=FindFirst(dir,0,f);
   dir:=slash(ExtractFilePath(dir));
   while (r=0) do begin
     fn:=dir+f.Name;
     convjpeg;
     r:=findnext(f);
   end;
   finally
     FindClose(f);
   end;
end else begin
   fn:=dir;
   convjpeg;
end;
finally
  jp.Free;
  jf.Free;
  bb.Free;
end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
Timer1.Enabled:=false;
try
if dir>'' then begin
  screen.Cursor:=crHourGlass;
  fixjpeg8bit(dir);
  screen.Cursor:=crDefault;
end;
except
  screen.Cursor:=crDefault;
end;
Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
var x,y: string;
    xx,yy,n: integer;
begin
x:=ParamStr(2);
y:=ParamStr(3);
if x>'' then begin
  val(x,xx,n);
  if n=0 then left:=xx;
end;
if y>'' then begin
  val(y,yy,n);
  if n=0 then top:=yy;
end;
if dir='' then begin
   BorderStyle:=bsSizeable;
   Edit1.Text:=GetCurrentDir;
   Panel1.Visible:=true;
   label1.Caption:='Convert image format for : ';
   label2.caption:='';
end else begin
try
if UpperCase(ExtractFileExt(dir))<>'.JPG' then begin
   fixdirectory:=true;
   label1.Caption:='Convert image format for : '+dir;
   label2.caption:='Please wait.';
   dir:=slash(dir)+'*.jpg';
end
 else begin
   fixdirectory:=false;
   label1.Caption:='Convert image format for : '+dir;
   label2.caption:='Please wait.';
 end;
finally
Timer1.Enabled:=true;
end;
end;
end;

procedure TForm1.SelDirClick(Sender: TObject);
begin
 FolderDialog1.Directory:=Edit1.Text;
 if FolderDialog1.Execute then
    Edit1.Text:=FolderDialog1.Directory;
end;

procedure TForm1.btncancelClick(Sender: TObject);
begin
Close;
end;

procedure TForm1.btnokClick(Sender: TObject);
begin
fixdirectory:=true;
dir:=slash(Edit1.Text)+'*.jpg';
label1.Caption:='Convert image format for : '+dir;
label2.caption:='Please wait.';
Timer1.Enabled:=true;
Panel1.Visible:=false;
BorderStyle:=bsNone;
end;

end.
