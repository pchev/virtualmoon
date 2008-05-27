
procedure CreateLowres(fn:string);
var //pal : Hpalette;
    p1,p2:Pbytearray;
    img:Tbitmap;
    bm:Tbitmap;
    jm:tjpegimage;
    src,dest:Trect;
begin
MsgForm:=TMsgForm.create(application);
MsgForm.Label1.caption:='Generating Lowres Texture. Please Wait ...';
msgform.show;
msgform.Refresh;
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
msgform.close;
bm.free;
img.free;
jm.free;
msgform.free;
end;
end;

Procedure CreateHires(fn:string);
var p : Pbytearray;
    jm : Tjpegimage;
    bm: Tbitmap;
    i,x,y,n:integer;
begin
if not fileexists(fn) then raise exception.create('File not found '+fn);
MsgForm:=TMsgForm.create(application);
MsgForm.Label1.caption:='Generating Hires Texture. Please Wait ...';
msgform.show;
msgform.Refresh;
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
 hi_w:=x;
 hi_dl:=x div 360;
 hi_wd:=512 div hi_dl;
finally
 msgform.close;
 closefile(fh);
 bm.free;
 msgform.free;
end;
end;

Procedure CreateHires500(fn:string);
var p : Pbytearray;
    jm : Tjpegimage;
    bm: Tbitmap;
    i,x,y,n:integer;
begin
if fileexists(fn) then begin
MsgForm:=TMsgForm.create(application);
MsgForm.Label1.caption:='Generating 500m/pixel Texture. Please Wait ...';
msgform.show;
msgform.Refresh;
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
 msgform.close;
 closefile(fh);
 bm.free;
 msgform.free;
end;
end;
end;

begin
  if hires
      then begin
        try
         CreateHires(Slash(appdir)+Slash('textures')+hiresfile);
        except
         deletefile(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'.dat');
        end;
      end;

  if hi500
      then begin
        try
         CreateHires500(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'_500.jpg');
        except
         deletefile(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'_500.dat');
        end;
      end;

  if lowres
      then begin
        try
         CreateLowres(Slash(appdir)+Slash('textures')+hiresfile);
        except
         deletefile(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c8.jpg');
        end;
      end;
end.      
