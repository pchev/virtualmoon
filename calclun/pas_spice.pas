unit pas_spice;

{$mode objfpc}{$H+}

interface

uses cspice, u_util, u_constant,
  Classes, SysUtils;

var datestd: string;

procedure InitError;
function SpiceLastErr: string;
function SpiceLastError: string;
function initdoublecell(index: SpiceInt): PSpiceCell;
function initintcell(index: SpiceInt): PSpiceCell;
function DateTime2ET(tim:TDateTime): double;
function ET2DateTime(et:double): TDateTime;
procedure spkinfo(spk: ConstSpiceChar; info:TStrings);
procedure pckinfo(pck: ConstSpiceChar; info:TStrings);
procedure spkallinfo(info:TStrings);
procedure pckallinfo(info:TStrings);
function ObservatoryPosition(lon,lat,alt: SpiceDouble; out obspos:TDouble3):boolean;

implementation


function initdoublecell(index: SpiceInt): PSpiceCell;
begin
  // get a SPICEDOUBLE_CELL predefined in getcel.c and clear it
  result:=getdoublecell(index);
  scard_c (0, result);
end;

function initintcell(index: SpiceInt): PSpiceCell;
begin
  // get a SPICEINT_CELL predefined in getcel.c and clear it
  result:=getintcell(index);
  scard_c (0, result);
end;

procedure InitError;
var buf: array[0..WORD_SIZE] of char;
begin
  // initialize the cspice error to be compatible with a GUI program:
  // - disable trace
  // - do not print error on console
  // - do not halt in case of error
  trcoff_c;
  buf:='NONE';
  errprt_c ('SET', 0, @buf);
  buf:='RETURN';
  erract_c ('SET', 0, @buf);
end;

function SpiceLastErr: string;
var buf: array[0..MSG_SIZE] of char;
begin
  // get the short message for the last error
  getmsg_c('SHORT',MSG_SIZE,@buf);
  result:=buf;
end;

function SpiceLastError: string;
var buf: array[0..MSG_SIZE] of char;
begin
  // get the long message for the last error
  getmsg_c('LONG',MSG_SIZE,@buf);
  result:=buf;
end;


function DateTime2ET(tim:TDateTime): double;
var str: ConstSpiceChar;
begin
  // convert a TDateTime to cspice ET
  str:=Pchar(DateTime2DateIso(tim));
  str2et_c(str,result);
end;

function ET2DateTime(et:double): TDateTime;
var buf: array[0..80] of char;
begin
  // convert a cspice ET to TDateTime
  et2utc_c(et,'ISOC',3,80,@buf);
  result:=DateIso2DateTime(buf);
end;

procedure spkinfo(spk: ConstSpiceChar; info:TStrings);
var i,j,n,niv: integer;
  cover,ids: PSpiceCell;
  obj: SpiceInt;
  b,e: SpiceDouble;
  ok: SpiceBoolean;
  buf: array[0..WORD_SIZE] of char;
  objn: string;
begin
  // show information about a SPK file
  reset_c;
  info.Add('Fichier: '+spk);
  cover:=initdoublecell(1);
  ids:=initintcell(1);
  spkobj_c( pchar(spk), ids );
  if failed_c then begin
    info.Add(SpiceLastError);
    exit;
  end;
  n:=card_c(ids);
  for i:=0 to n-1 do begin
    obj:=spice_cell_elem_i(ids,i);
    bodc2n_c(obj,WORD_SIZE,@buf,ok);
    if ok then objn:=trim(buf)
          else objn:=inttostr(obj);
    scard_c(0, cover);
    spkcov_c(spk, obj, cover);
    niv := wncard_c (cover);
    for j:=0 to niv-1 do begin
       wnfetd_c (cover, j, b, e );
       info.Add(tab+objn+tab+'depuis: '+FormatDateTime(datestd,ET2DateTime(b))+tab+'jusqu''à: '+FormatDateTime(datestd,ET2DateTime(e)));
    end;
  end;
end;

procedure pckinfo(pck: ConstSpiceChar; info:TStrings);
var i,j,n,niv: integer;
  cover,ids: PSpiceCell;
  obj: SpiceInt;
  b,e: SpiceDouble;
  ok: SpiceBoolean;
  frcode,center:SpiceInt;
  buf: array[0..WORD_SIZE] of char;
  objn: string;
begin
  // show information about a PCK file
  reset_c;
  info.Add('Fichier: '+pck);
  cover:=initdoublecell(1);
  ids:=initintcell(1);
  pckfrm_c( pchar(pck), ids );
  if failed_c then begin
    info.Add(SpiceLastError);
    exit;
  end;
  n:=card_c(ids);
  for i:=0 to n-1 do begin
    obj:=spice_cell_elem_i(ids,i);
    ccifrm_c ( 2, obj, WORD_SIZE, frcode, @buf, center, ok );
    if ok then objn:=trim(buf)
          else objn:=inttostr(obj);
    scard_c(0, cover);
    pckcov_c(pck, obj, cover);
    niv := wncard_c (cover);
    for j:=0 to niv-1 do begin
       wnfetd_c (cover, j, b, e );
       info.Add(tab+objn+tab+'depuis: '+FormatDateTime(datestd,ET2DateTime(b))+tab+'jusqu''à: '+FormatDateTime(datestd,ET2DateTime(e)));
    end;
  end;
end;

procedure spkallinfo(info:TStrings);
var kind:ConstSpiceChar;
    count:SpiceInt;
    filename: array[0..FILE_SIZE] of char;
    filtyp: array[0..WORD_SIZE] of char;
    source: array[0..FILE_SIZE] of char;
    handle: SpiceInt;
    found: SpiceBoolean;
    i:integer;
begin
  // search all loaded SPK and show information
  kind:='SPK';
  ktotal_c(kind,count);
  for i:=0 to count-1 do begin
    kdata_c(i,kind,FILE_SIZE,WORD_SIZE,FILE_SIZE,@filename,@filtyp,@source,handle,found);
    if found then
      spkinfo(filename,info);
  end;
end;

procedure pckallinfo(info:TStrings);
var kind:ConstSpiceChar;
    count:SpiceInt;
    filename: array[0..FILE_SIZE] of char;
    filtyp: array[0..WORD_SIZE] of char;
    source: array[0..FILE_SIZE] of char;
    handle: SpiceInt;
    found: SpiceBoolean;
    i:integer;
begin
  // search all loaded PCK and show information
  kind:='PCK';
  ktotal_c(kind,count);
  for i:=0 to count-1 do begin
    kdata_c(i,kind,FILE_SIZE,WORD_SIZE,FILE_SIZE,@filename,@filtyp,@source,handle,found);
    if found then
      pckinfo(filename,info);
  end;
end;

function ObservatoryPosition(lon,lat,alt: SpiceDouble; out obspos:TDouble3):boolean;
var re,rp,f: SpiceDouble;
  n: SpiceInt;
  radii: TDouble3;
begin
  // convert observatory position on earth to rectangular
  result:=false;
  reset_c;
  bodvrd_c ( 'EARTH', 'RADII', 3, n, radii );
  if failed_c then exit;
  re := radii[0];
  rp := radii[2];
  f  := ( re - rp ) / re;
  georec_c(lon, lat, alt, re, f, obspos);
  result:= not failed_c;
end;

end.

