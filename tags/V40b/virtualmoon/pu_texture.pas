unit pu_texture;

interface

uses jpeg, u_util,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Panel1: TPanel;
    Label1: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    appdir, hiresfile, imgsuffix: string;
    function TestWriteAccess: boolean;
    procedure CreateLowres(fn:string);
    Procedure CreateHires500(fn:string);
    Procedure CreateHires(fn:string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.CreateLowres(fn:string);
var //pal : Hpalette;
    p1,p2:Pbytearray;
    img:Tbitmap;
    bm:Tbitmap;
    jm:tjpegimage;
    src,dest:Trect;
begin
if not fileexists(fn) then raise exception.create('File not found '+fn);
Label1.caption:='Generating Lowres Texture. Please Wait ...';
Application.ProcessMessages;
img:=Tbitmap.Create;
bm:=Tbitmap.create;
jm:=TJpegImage.Create;
try
jm.Grayscale:=true;
jm.Scale:=jsHalf;
jm.LoadFromFile(fn);
//crepalette(pal);
//pal:=jm.palette;
bm.PixelFormat:=pf8bit;
//bm.Palette:=pal;
bm.height:=2048;
bm.width:=4096;
dest:=rect(0,0,4096,2048);
bm.canvas.stretchdraw(dest,jm);
img.Width:=1024;
img.Height:=1024;
img.PixelFormat:=pf8bit;
//img.Palette:=pal;
dest:=rect(0,0,1024,1024);
src:=rect(0,0,1024,1024);
img.Canvas.CopyRect(dest,bm.canvas,src);
p1:=img.ScanLine[0];
p2:=img.ScanLine[1023];
move(p2[0],p1[0],1024);
jm.assign(img);
jm.CompressionQuality:=80;
jm.savetofile(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c6.jpg');
src:=rect(1024,0,2048,1024);
img.Canvas.CopyRect(dest,bm.canvas,src);
p1:=img.ScanLine[0];
p2:=img.ScanLine[1023];
move(p2[0],p1[0],1024);
jm.assign(img);
jm.CompressionQuality:=80;
jm.savetofile(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c1.jpg');
src:=rect(2048,0,3072,1024);
img.Canvas.CopyRect(dest,bm.canvas,src);
p1:=img.ScanLine[0];
p2:=img.ScanLine[1023];
move(p2[0],p1[0],1024);
jm.assign(img);
jm.CompressionQuality:=80;
jm.savetofile(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c2.jpg');
src:=rect(3072,0,4096,1024);
img.Canvas.CopyRect(dest,bm.canvas,src);
p1:=img.ScanLine[0];
p2:=img.ScanLine[1023];
move(p2[0],p1[0],1024);
jm.assign(img);
jm.CompressionQuality:=80;
jm.savetofile(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c5.jpg');
src:=rect(0,1024,1024,2048);
img.Canvas.CopyRect(dest,bm.canvas,src);
p1:=img.ScanLine[0];
p2:=img.ScanLine[1023];
move(p1[0],p2[0],1024);
jm.assign(img);
jm.CompressionQuality:=80;
jm.savetofile(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c8.jpg');
src:=rect(1024,1024,2048,2048);
img.Canvas.CopyRect(dest,bm.canvas,src);
p1:=img.ScanLine[0];
p2:=img.ScanLine[1023];
move(p1[0],p2[0],1024);
jm.assign(img);
jm.CompressionQuality:=80;
jm.savetofile(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c3.jpg');
src:=rect(2048,1024,3072,2048);
img.Canvas.CopyRect(dest,bm.canvas,src);
p1:=img.ScanLine[0];
p2:=img.ScanLine[1023];
move(p1[0],p2[0],1024);
jm.assign(img);
jm.CompressionQuality:=80;
jm.savetofile(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c4.jpg');
src:=rect(3072,1024,4096,2048);
img.Canvas.CopyRect(dest,bm.canvas,src);
p1:=img.ScanLine[0];
p2:=img.ScanLine[1023];
move(p1[0],p2[0],1024);
jm.assign(img);
jm.CompressionQuality:=80;
jm.savetofile(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c7.jpg');
finally
bm.free;
img.free;
jm.free;
end;
end;

Procedure TForm1.CreateHires(fn:string);
var p : Pbytearray;
    jm : Tjpegimage;
    bm: Tbitmap;
    i,x,y,n:integer;
    fh: file;
begin
if not fileexists(fn) then raise exception.create('File not found '+fn);
Label1.caption:='Generating Hires Texture. Please Wait ...';
Application.ProcessMessages;
jm:=TJpegImage.Create;
bm:=Tbitmap.Create;
filemode:=2;
assignfile(fh,Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'.dat');
rewrite(fh,1);
try
 try
 jm.LoadFromFile(fn);
 bm.Assign(jm);
 jm.free;
 except
  raise exception.create('Out of memory!');
 end;
 x:=bm.Width;
 y:=bm.Height;
// if (x<>2*y)or((x mod 360)<>0)or((512 mod (x div 360))<>0) then raise exception.create('Invalid hires file size.');
// if (x<>11520)or(y<>5760) then raise exception.create('Invalid hires file size, must be 11520x5760');
 for i:=0 to y-1 do begin
   p:=bm.ScanLine[i];
   blockwrite(fh,p[0],x,n);
   if n<>x then raise exception.create('Cannot create hires file.');
 end;
{ hi_w:=x;
 hi_dl:=x div 360;
 hi_wd:=512 div hi_dl;}
finally
 closefile(fh);
 bm.free;
end;
end;

Procedure TForm1.CreateHires500(fn:string);
var p : Pbytearray;
    jm : Tjpegimage;
    bm: Tbitmap;
    i,x,y,n:integer;
    fh: file;
begin
if fileexists(fn) then begin
Label1.caption:='Generating 500m/pixel Texture. Please Wait ...';
Application.ProcessMessages;
jm:=TJpegImage.Create;
bm:=Tbitmap.Create;
filemode:=2;
assignfile(fh,Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'_500.dat');
rewrite(fh,1);
try
 try
 jm.LoadFromFile(fn);
 bm.Assign(jm);
 jm.free;
 except
  raise exception.create('Out of memory!');
 end;
 x:=bm.Width;
 y:=bm.Height;
 for i:=0 to y-1 do begin
   p:=bm.ScanLine[i];
   blockwrite(fh,p[0],x,n);
   if n<>x then raise exception.create('Cannot create hires file.');
 end;
finally
 closefile(fh);
 bm.free;
end;
end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
timer1.Enabled:=false;
if TestWriteAccess then begin
  // aerograph
  hiresfile:='hires.jpg';
  imgsuffix:='';
  if ((not fileexists(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'.dat'))and
     (fileexists(Slash(appdir)+Slash('textures')+hiresfile)))
      then begin
        try
         CreateHires(Slash(appdir)+Slash('textures')+hiresfile);
        except
         deletefile(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'.dat');
        end;
      end;
  if ((
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c1.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c2.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c3.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c4.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c5.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c6.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c7.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c8.jpg')))and
     (fileexists(Slash(appdir)+Slash('textures')+hiresfile)))
      then begin
        try
         CreateLowres(Slash(appdir)+Slash('textures')+hiresfile);
        except
         deletefile(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c8.jpg');
        end;
      end;

  // lopam
  hiresfile:='hires_lopam.jpg';
  imgsuffix:='_lopam';
  if ((not fileexists(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'.dat'))and
     (fileexists(Slash(appdir)+Slash('textures')+hiresfile)))
      then begin
        try
         CreateHires(Slash(appdir)+Slash('textures')+hiresfile);
        except
         deletefile(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'.dat');
        end;
      end;
  if ((
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c1.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c2.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c3.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c4.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c5.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c6.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c7.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c8.jpg')))and
     (fileexists(Slash(appdir)+Slash('textures')+hiresfile)))
      then begin
        try
         CreateLowres(Slash(appdir)+Slash('textures')+hiresfile);
        except
         deletefile(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c8.jpg');
        end;
      end;

  // clementine
  hiresfile:='hires_clem.jpg';
  imgsuffix:='_clem';
  if ((not fileexists(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'.dat'))and
     (fileexists(Slash(appdir)+Slash('textures')+hiresfile)))
      then begin
        try
         CreateHires(Slash(appdir)+Slash('textures')+hiresfile);
        except
         deletefile(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'.dat');
        end;
      end;
  if ((
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c1.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c2.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c3.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c4.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c5.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c6.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c7.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c8.jpg')))and
     (fileexists(Slash(appdir)+Slash('textures')+hiresfile)))
      then begin
        try
         CreateLowres(Slash(appdir)+Slash('textures')+hiresfile);
        except
         deletefile(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c8.jpg');
        end;
      end;

  // clementine 500m
  imgsuffix:='_clem';
  if ((not fileexists(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'_500.dat'))and
     (fileexists(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'_500.jpg')))
      then begin
        try
         CreateHires500(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'_500.jpg');
        except
         deletefile(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'_500.dat');
        end;
      end;
end
else begin
  ShowMessage('Could not write to directory '+Slash(appdir)+Slash('textures')+chr(10)+chr(13)+
  'Do you run this program as administrator from the Virtualmoon install directory ?');
end;
Close;
end;

function TForm1.TestWriteAccess: boolean;
var f: textfile;
    buf,fn:string;
begin
result:=false;
try
fn:=Slash(appdir)+Slash('textures')+'test.file';
DeleteFile(fn);
AssignFile(f,fn);
rewrite(f);
writeln(f,'test');
closefile(f);
reset(f);
readln(f,buf);
closefile(f);
if buf='test' then result:=true;
DeleteFile(fn);
except
result:=false;
end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
if ParamCount=1 then
   appdir:=ParamStr(1)
else
   appdir:=GetCurrentDir;
chdir(appdir);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
label1.Caption:='';
Timer1.Enabled:=true;
end;

end.
