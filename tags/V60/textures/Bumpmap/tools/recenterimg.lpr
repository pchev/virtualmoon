program recenterimg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,SysUtils,Math
  { you can add units after this };

var pdsfile,binfile: file;
    pdsname, binname, buf: string;
    val1,val2: array[0..999999] of char;
    iw, rw: smallint;
    i: integer;
begin
pdsname:=ParamStr(1);
buf:=ParamStr(2);
if buf='' then begin
  writeln('Syntax: recenterimg pds_filename img_width');
  halt(1);
end;
iw:=strtoint(buf);
rw:=iw div 2;
if rw>length(val1) then exit ;
binname:=ChangeFileExt(pdsname,'.bin');
AssignFile(pdsfile,pdsname);
reset(pdsfile,2);
AssignFile(binfile,binname);
rewrite(binfile,2);
writeln('Begin to convert data .');
// read data
i:=0;
repeat
  BlockRead(pdsfile,val1,rw);
  BlockRead(pdsfile,val2,rw);
  BlockWrite(binfile,val2,rw);
  BlockWrite(binfile,val1,rw);
  inc(i);
  if (i mod 1000)=0 then write('.');
until eof(pdsfile);
writeln;
CloseFile(binfile);
CloseFile(pdsfile);
end.



