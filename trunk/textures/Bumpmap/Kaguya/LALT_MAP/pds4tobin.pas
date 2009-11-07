program pds4tobin;
{
   Dump convertion of a PDS IMG file to a 16 bit integer stream.
   It need to be adapted for each use case.
}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,SysUtils,Math;

var pdsfile,binfile: file;
    pdsname, binname, buf: string;
    c: char;
    val: single;
    ival: smallint;
    i: integer;
begin
pdsname:=ParamStr(1);
if pdsname='' then begin
  writeln('Syntax: pds4tobin pds_filename');
  halt(1);
end;
binname:=ChangeFileExt(pdsname,'.bin');
AssignFile(pdsfile,pdsname);
reset(pdsfile,1);
AssignFile(binfile,binname);
rewrite(binfile,1);
// skip header
repeat
  buf:='';
  repeat
    blockread(pdsfile,c,1);
    buf:=buf+c;
  until (c=char(10)) or eof(pdsfile);
until (trim(buf)='END') or eof(pdsfile);
if eof(pdsfile) then begin
  writeln('Is '+pdsname+' really a PDS file ?');
  halt(1);
end;
writeln('Begin to convert data .');
// read data
i:=0;
repeat
  BlockRead(pdsfile,val,4);
  // convert float to integer, this is specific to the data content
  ival:=round(val*1000);  // kilometers to meters
  BlockWrite(binfile,ival,2);
  inc(i);
  if (i mod 100000)=0 then write('.');
until eof(pdsfile);
writeln;
CloseFile(binfile);
CloseFile(pdsfile);
end.

