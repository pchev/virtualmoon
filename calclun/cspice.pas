unit cspice;

{$mode objfpc}{$H+}
{$WARN 3187 off : C arrays are passed by reference}

interface

uses
  Classes, SysUtils;

const
  {$ifdef mswindows}
    {$ifdef cpu32}
      libcspice = 'libcspice32.dll';
      {$L getcellwin32.o}
    {$endif}
    {$ifdef cpu64}
      libcspice = 'libcspice64.dll';
      {$L getcellwin64.o}
    {$endif}
  {$else}
    {$ifdef darwin}
      libcspice = './libcspice.dylib';
      {$L getcell.o}
    {$else}
      libcspice = 'libcspicevma.so';
      {$L getcell.o}
    {$endif}
  {$endif}

const
  WORD_SIZE = 80;
  MSG_SIZE = 1024;
  FILE_SIZE = 1024;
  MAXIVL = 1000;  // must correspond to value in getcell.c

type
  SpiceBoolean = LongBool;
  SpiceInt = LongInt;
  PSpiceInt = ^SpiceInt;
  SpiceChar = array of char;
  PSpiceChar = ^SpiceChar;
  ConstSpiceChar = PChar;
  SpiceDouble = Double;
  PSpiceDouble = ^SpiceDouble;
  TDouble3 = array[0..2] of SpiceDouble;
  TDouble6 = array[0..5] of SpiceDouble;
  TDouble12 = array[0..11] of SpiceDouble;
  TArray3 = array[0..2,0..2] of SpiceDouble;
  SpiceCell = record
              dtype:Integer;
              length:SpiceInt;
              size:SpiceInt;
              card:SpiceInt;
              isSet:SpiceBoolean;
              adjust:SpiceBoolean;
              init:SpiceBoolean;
              base:Pointer;
              data:Pointer;
              end;
  PSpiceCell = ^SpiceCell;
  Tudfuns = procedure(et:SpiceDouble; out value:SpiceDouble); cdecl;
  Tudqdec = procedure(f:Tudfuns; et:SpiceDouble; out isdecr:SpiceBoolean); cdecl;
  Tudfunb = procedure(f:Tudfuns; et:SpiceDouble; out xbool:SpiceBoolean); cdecl;

const
  frameJ2000: ConstSpiceChar = 'J2000';
  abcorrNone: ConstSpiceChar = 'NONE';
  abcorrLT: ConstSpiceChar = 'LT';
  abcorrLTS: ConstSpiceChar = 'LT+S';
  abcorrCN: ConstSpiceChar = 'CN';
  abcorrCNS: ConstSpiceChar = 'CN+S';
  fixrefME: ConstSpiceChar = 'MOON_ME';
  fixrefIAU: ConstSpiceChar = 'IAU_MOON';
  submethod: ConstSpiceChar = 'Intercept: ellipsoid';
  naifBarycenter: SpiceInt = 0;
  naifSun: SpiceInt = 10;
  naifEarthBarycenter: SpiceInt = 3;
  naifEarth: SpiceInt = 399;
  naifMoon: SpiceInt = 301;
  cnaifBarycenter: ConstSpiceChar = '0';
  cnaifSun: ConstSpiceChar = '10';
  cnaifEarthBarycenter: ConstSpiceChar = '3';
  cnaifEarth: ConstSpiceChar = '399';
  cnaifMoon: ConstSpiceChar = '301';
  TIMFMTutc: ConstSpiceChar = 'YYYY MON DD HR:MN:SC.###### UTC';
  TIMFMTutc0: ConstSpiceChar = 'YYYY MM DD HR:MN:SC UTC';

// error handling
procedure erract_c (op:ConstSpiceChar; lenout:SpiceInt; action:PSpiceChar); cdecl; external libcspice;
procedure errprt_c (op:ConstSpiceChar; lenout:SpiceInt; list:PSpiceChar); cdecl; external libcspice;
procedure trcoff_c; cdecl; external libcspice;
function failed_c: SpiceBoolean; cdecl; external libcspice;
procedure getmsg_c (option: ConstSpiceChar; lenout: SpiceInt; msg: PSpiceChar); cdecl; external libcspice;
procedure reset_c; cdecl; external libcspice;

// load files
procedure furnsh_c(fn: ConstSpiceChar); cdecl; external libcspice;
procedure ktotal_c(kind:ConstSpiceChar; out count:SpiceInt); cdecl; external libcspice;
procedure kdata_c (which: SpiceInt; kind:ConstSpiceChar; fillen,typlen,srclen:SpiceInt; filename,filtyp,source:SpiceChar; out handle: SpiceInt; out found:SpiceBoolean); cdecl; external libcspice;

// kernel information
procedure bodvrd_c (bodynm,item: ConstSpiceChar; maxn: SpiceInt; out dim: SpiceInt; out values: array of SpiceDouble); cdecl; external libcspice;
procedure spkobj_c (spk:ConstSpiceChar; ids: PSpiceCell); cdecl; external libcspice;
procedure spkcov_c (spk:ConstSpiceChar; idcode:SpiceInt; cover: PSpiceCell); cdecl; external libcspice;
procedure bodn2c_c (name:ConstSpiceChar; out code:SpiceInt; out found:SpiceBoolean); cdecl; external libcspice;
procedure bodc2n_c (code:SpiceInt; lenout:SpiceInt; name:SpiceChar; out found:SpiceBoolean); cdecl; external libcspice;
procedure pckfrm_c (pck:ConstSpiceChar; ids: PSpiceCell); cdecl; external libcspice;
procedure pckcov_c (pck:ConstSpiceChar; idcode:SpiceInt; cover: PSpiceCell); cdecl; external libcspice;
procedure namfrm_c (frname:ConstSpiceChar; out frcode:SpiceInt); cdecl; external libcspice;
procedure frinfo_c (frcode:SpiceInt; out cent,frclss,clssid:SpiceInt; out found:SpiceBoolean); cdecl; external libcspice;
procedure ccifrm_c (frclss,clssid,lenout:SpiceInt; out frcode:SpiceInt; frname:SpiceChar; out center:SpiceInt; out found:SpiceBoolean); cdecl; external libcspice;

// time convertion
procedure et2utc_c (et: SpiceDouble; format:ConstSpiceChar; prec,lenout:SpiceInt; utcstr:PSpiceChar); cdecl; external libcspice;
procedure str2et_c (str: ConstSpiceChar; out et: SpiceDouble); cdecl; external libcspice;
procedure timout_c (et: SpiceDouble; pictur:ConstSpiceChar; lenout: SpiceInt; output: PSpiceChar); cdecl; external libcspice;

// coordinates computation
procedure spkezp_c (targ: SpiceInt; et: SpiceDouble; ref,abcorr: ConstSpiceChar; obs: SpiceInt; out ptarg: TDouble3; lt: PSpiceDouble); cdecl; external libcspice;
procedure spkcpo_c (target:ConstSpiceChar; et:SpiceDouble; outref,refloc,abcorr:ConstSpiceChar; obspos:TDouble3; obsctr,obsref:ConstSpiceChar; out state:TDouble6; lt:PSpiceDouble); cdecl; external libcspice;
procedure recrad_c (rectan: TDouble3; out range,ra,dec: SpiceDouble); cdecl; external libcspice;

// orientation
procedure subpnt_c (method,target:ConstSpiceChar; et:SpiceDouble; fixref,abcorr,obsrvr:ConstSpiceChar; out spoint:TDouble3; out trgepc:SpiceDouble; out srfvec:TDouble3); cdecl; external libcspice;
procedure subslr_c (method,target:ConstSpiceChar; et:SpiceDouble; fixref,abcorr,obsrvr:ConstSpiceChar; out spoint:TDouble3; out trgepc:SpiceDouble; out srfvec:TDouble3); cdecl; external libcspice;
procedure reclat_c (rectan: TDouble3; out radius,longitude,latitude: SpiceDouble); cdecl; external libcspice;
procedure latrec_c (radius,longitude,latitude: SpiceDouble; out rectan: TDouble3); cdecl; external libcspice;
procedure srfrec_c (body: SpiceInt; longitude, latitude: SpiceDouble; out rectan: TDouble3); cdecl; external libcspice;
procedure recgeo_c (rectan: TDouble3; re,f:SpiceDouble; out lon,lat,alt); cdecl; external libcspice;
procedure georec_c (lon,lat,alt,re,f: SpiceDouble; out rectan: TDouble3); cdecl; external libcspice;
function phaseq_c (et:SpiceDouble; target,illmn,obsrvr,abcorr:ConstSpiceChar): SpiceDouble; cdecl; external libcspice;
procedure surfpt_c(positn,u:TDouble3; a,b,c:SpiceDouble; out point:TDouble3; out found:SpiceBoolean); cdecl; external libcspice;

// transformation matrix
procedure mxv_c (m1:TArray3; vin: TDouble3; out vout: TDouble3); cdecl; external libcspice;
procedure tipbod_c(ref:ConstSpiceChar; body:SpiceInt; et:SpiceDouble; out tipm:TArray3); cdecl; external libcspice;
procedure twovec_c (axdef: TDouble3; indexa: SpiceInt; plndef: TDouble3; indexp: SpiceInt; out mout: TArray3); cdecl; external libcspice;
procedure pxform_c (fromframe,toframe:ConstSpiceChar; et:SpiceDouble; out rotate:TArray3); cdecl; external libcspice;

// Cells
function getdoublecell(index: SpiceInt): PSpiceCell; cdecl; external; // in getcell.o
function getintcell(index: SpiceInt): PSpiceCell; cdecl; external;    // in getcell.o
function spice_cell_elem_i(c:PSpiceCell; i: SpiceInt): SpiceInt; cdecl; external; // in getcell.o
procedure scard_c (card: SpiceInt; cell: PSpiceCell); cdecl; external libcspice;
procedure wninsd_c(left,right:SpiceDouble; window: PSpiceCell); cdecl; external libcspice;
procedure wnexpd_c(left,right:SpiceDouble; window: PSpiceCell); cdecl; external libcspice;
function wncard_c(window: PSpiceCell): SpiceInt; cdecl; external libcspice;
procedure wnfetd_c (window:PSpiceCell; n: SpiceInt; out left,right: SpiceDouble); cdecl; external libcspice;
function card_c(cell:PSpiceCell):SpiceInt; cdecl; external libcspice;
procedure copy_c (cell,copy: PSpiceCell); cdecl; external libcspice;
procedure wnunid_c (a,b,c: PSpiceCell); cdecl; external libcspice;
procedure wnintd_c (a,b,c: PSpiceCell); cdecl; external libcspice;
procedure wndifd_c (a,b,c: PSpiceCell); cdecl; external libcspice;

// Event
procedure gfilum_c (method,angtyp,target,illmn,fixref,abcorr,obsrvr: ConstSpiceChar; spoint: TDouble3;relate: ConstSpiceChar; refval,adjust,step:SpiceDouble; nintvls: SpiceInt; cnfine,result: PSpiceCell); cdecl; external libcspice;
procedure gfsubc_c (target,fixref,method,abcorr,obsrvr,crdsys,coord,relate: ConstSpiceChar; refval,adjust,step:SpiceDouble;nintvls:SpiceInt;cnfine,result:PSpiceCell); cdecl; external libcspice;
procedure gfpa_c (target,illmn,abcorr,obsrvr,relate:ConstSpiceChar; refval,adjust,step:SpiceDouble; nintvls:SpiceInt; cnfine,result: PSpiceCell); cdecl; external libcspice;
procedure gfuds_c(f:Tudfuns; q:Tudqdec; relate:ConstSpiceChar; refval,adjust,step:SpiceDouble; nintvls:SpiceInt; cnfine,result: PSpiceCell); cdecl; external libcspice;
procedure gfudb_c(f:Tudfuns; q:Tudfunb; step:SpiceDouble; cnfine,result: PSpiceCell); cdecl; external libcspice;
procedure uddc_c(f:Tudfuns; x,dx:SpiceDouble; out isdecr: SpiceBoolean);cdecl; external libcspice;
procedure udf_c(et:SpiceDouble; out value:SpiceDouble); cdecl;  external libcspice;

implementation

{procedure udf_c(et:SpiceDouble; out value:SpiceDouble); cdecl;
begin
  // dummy procedure
end;}

end.

