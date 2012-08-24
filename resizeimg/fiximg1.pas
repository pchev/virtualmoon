unit fiximg1;
{
Copyright (C) 2007 Patrick Chevalley

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
    FolderDialog1: TFolderDialog;
    XPManifest1: TXPManifest;
    RadioGroup1: TRadioGroup;
    Memo1: TMemo;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SelDirClick(Sender: TObject);
    procedure btnokClick(Sender: TObject);
    procedure btncancelClick(Sender: TObject);
  private
    { Private declarations }
    fixdirectory: boolean;
    maxsize: integer;
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
    bb,rbb: TBitmap;
    f: TSearchRec;
    r,nimg,nerr: integer;
    fn: string;
    ratio: double;
    newwidth,newheight: integer;
function convjpeg: boolean;
begin
result:=false;
try
    jp.Grayscale:=false;
    jp.PixelFormat:=jf24Bit;
    jp.LoadFromFile(fn);
    bb.Assign(jp);
    bb.PixelFormat:=pf24bit;
    ratio:=bb.Width/bb.Height;
    newwidth:=0;
    newwidth:=0;
    if ratio>1 then begin
      if bb.width>maxsize then begin
         newwidth:=maxsize;
         newheight:=round(maxsize/ratio);
      end;
    end else begin
      if bb.height>maxsize then begin
         newwidth:=round(maxsize*ratio);
         newheight:=maxsize;
      end;
    end;
    if (newwidth>0)then begin
      rbb.Assign(bb);
      bb.Width:=newwidth;
      bb.Height:=newheight;
      bb.Canvas.StretchDraw(rect(0,0,newwidth,newheight),rbb);
    end;
    if odd(bb.Width) then bb.Width:=bb.Width-1;
    if odd(bb.Height) then bb.Height:=bb.Height-1;
    jf.Assign(bb);
    jf.PixelFormat:=jf24Bit;
    jf.CompressionQuality:=70;
    jf.Grayscale:=false;
    jf.SaveToFile(fn);
    result:=true;
except
end;
end;
begin
try
nimg:=0;
nerr:=0;
jp:=TJPEGImage.create;
jf:=TJPEGImage.create;
bb:=TBitmap.Create;
rbb:=TBitmap.Create;
memo1.Clear;
memo1.Lines.Add('Start conversion.');
if fixdirectory then begin
   try
   r:=FindFirst(dir,0,f);
   dir:=slash(ExtractFilePath(dir));
   while (r=0) do begin
     fn:=dir+f.Name;
     label2.Caption:=f.Name;
     if convjpeg then begin
       inc(nimg);
     end else begin
       if nerr=0 then memo1.Lines.Add('Files in error:');
       memo1.lines.add(f.Name);
       inc(nerr);
     end;
     application.ProcessMessages;
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
  rbb.Free;
end;
memo1.Lines.Add('End conversion. '+inttostr(nimg)+' OK, '+inttostr(nerr)+' errors');
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
Timer1.Enabled:=false;
try
if dir>'' then begin
  btnok.Enabled:=false;
  screen.Cursor:=crHourGlass;
  fixjpeg8bit(dir);
  screen.Cursor:=crDefault;
  btnok.Enabled:=true;
end;
except
  screen.Cursor:=crDefault;
  btnok.Enabled:=true;
end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var x,y: string;
    xx,yy,n: integer;
begin
   Edit1.Text:=GetCurrentDir;
   Panel1.Visible:=true;
   label1.Caption:='Convert image format for : ';
   label2.caption:='';
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
case RadioGroup1.ItemIndex of
0 : maxsize:=640;
1 : maxsize:=800;
2 : maxsize:=1024;
end;
Timer1.Enabled:=true;
end;

end.
