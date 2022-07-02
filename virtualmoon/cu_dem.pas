unit cu_dem;

{$mode ObjFPC}{$H+}
interface

uses u_util, u_constant, {BGRABitmap, BGRABitmapTypes,} math,
  Classes, SysUtils;


type

  TDemHdr = record
     FILE_NAME : string;
     MAP_RESOLUTION,LINE_FIRST_PIXEL,LINE_LAST_PIXEL,SAMPLE_FIRST_PIXEL,SAMPLE_LAST_PIXEL  : integer;
     SCALING_FACTOR,OFFSET,CENTER_LATITUDE,CENTER_LONGITUDE,MAP_SCALE,LINE_PROJECTION_OFFSET,SAMPLE_PROJECTION_OFFSET: double;
  end;

  TGreatCircle = record
    lat1,lon1,lat2,lon2,radius: double; // input
    l0,a0,dist,s01,s02: double;         // computed
  end;

  TSmallintArray = array of array of smallint;

  Tdem = class(TObject)
   private
    FDemHdr: TDemHdr;
    fDem: file;
    fDemOpen: boolean;
//    palette: array[0..255] of TBGRAPixel;
    FLastMessage: string;
    function ReadDemHDR(fn: string):boolean;
   public
    constructor Create;
    destructor Destroy;
    function OpenDem(n:string):boolean;
//    function OpenPalette(n:string):boolean;
    function GetDemCenter(lon,lat: double; n: integer; out d:TSmallintArray): boolean;
    function GetDemArea(lon1,lat1,lon2,lat2: double; out d:TSmallintArray): boolean;
    //function GetBitmap(d:TSmallintArray; var bgra: TBGRABitmap):boolean;
    function GetDemElevation(lon,lat: double): double;
    procedure GreatCircle(lon1,lat1,lon2,lat2,r: double; var c:TGreatCircle);
    procedure PointOnCircle(c:TGreatCircle; s: double; out la,lo: double);
    property DemOpen: Boolean read fDemOpen;
    property DemHdr: TDemHdr read FDemHdr;
    property LastMessage: string read FLastMessage;
  end;


implementation

constructor Tdem.Create;
begin
  inherited Create;
  fDemOpen:=false;
end;

destructor Tdem.Destroy;
begin
  if fDemOpen then CloseFile(fdem);
  inherited Destroy;
end;

function Tdem.OpenDem(n:string):boolean;
begin
  result:=false;
  FLastMessage:='';
  if not  ReadDemHDR(n+'.LBL') then exit;
  AssignFile(fDem,DemHdr.FILE_NAME);
  try
   reset(fDem,1);
   fDemOpen:=true;
   result:=true;
  except
   on E: Exception do begin
     result:=false;
     FLastMessage:='Open '+DemHdr.FILE_NAME+' : '+E.Message;
   end;
  end;
end;


function Tdem.ReadDemHDR(fn: string):Boolean;
var f: TextFile;
    i: integer;
    fdir,buf,bufk,bufv,bufu: string;
    p,k,v,u: TStringList;
function getval(sk: string; var sv,su: string):boolean;
var x: integer;
begin
   result:=false;
   bufk:=sk;
   try
   x:=k.IndexOf(sk);
   if x<0 then exit;
   sv:=v[x];
   su:=u[x];
   result:=true;
   except
     result:=false;
   end;
end;
begin
  result:=false;
  fn:=ExpandFileName(fn);
  if not FileExists(fn) then begin
    FLastMessage:='File not found: '+fn;
    exit;
  end;
  fdir:=ExtractFileDir(fn);
  p:=TStringList.Create;
  k:=TStringList.Create;
  v:=TStringList.Create;
  u:=TStringList.Create;
  try
  // read .lbl file
  AssignFile(f,fn);
  reset(f);
  try
  while not eof(f) do begin
    readln(f,buf);
    if copy(buf,1,2)='/*' then continue;
    SplitRec(UpperCase(buf),'=',p);
    if p.Count<>2 then continue;
    bufk:=trim(p[0]);
    p[1]:=StringReplace(p[1],'"','',[rfReplaceAll]);
    p[1]:=StringReplace(p[1],'''','',[rfReplaceAll]);
    i:=pos('<',p[1]);
    if i>0 then begin
      buf:=p[1];
      bufv:=trim(copy(buf,1,i-1));
      Delete(buf,1,i);
      i:=pos('>',buf);
      if i>0 then
        bufu:=trim(copy(buf,1,i-1))
      else
        bufu:=trim(buf);
    end else begin
      bufv:=trim(p[1]);
      bufu:='';
    end;
    k.Add(bufk);
    v.Add(bufv);
    u.Add(bufu);
  end;
  finally
    closefile(f);
  end;
  try
  // some check
  if not getval('TARGET_NAME',bufv,bufu) then exit;
  if bufv<>'MOON' then exit;
  if not getval('NAME',bufv,bufu) then exit;
  if bufv<>'HEIGHT' then exit;
  if not getval('RECORD_TYPE',bufv,bufu) then exit;
  if bufv<>'FIXED_LENGTH' then exit;
  if not getval('SAMPLE_TYPE',bufv,bufu) then exit;
  if bufv<>'LSB_INTEGER' then exit;
  if not getval('SAMPLE_BITS',bufv,bufu) then exit;
  if bufv<>'16' then exit;
  if not getval('UNIT',bufv,bufu) then exit;
  if bufv<>'METER' then exit;
  if not getval('MAP_PROJECTION_TYPE',bufv,bufu) then exit;
  if bufv<>'SIMPLE CYLINDRICAL' then exit;
  if not getval('POSITIVE_LONGITUDE_DIRECTION',bufv,bufu) then exit;
  if bufv<>'EAST' then exit;
  if not getval('MAXIMUM_LATITUDE',bufv,bufu) then exit;
  if bufv<>'90' then exit;
  if not getval('MINIMUM_LATITUDE',bufv,bufu) then exit;
  if bufv<>'-90' then exit;
  if not getval('WESTERNMOST_LONGITUDE',bufv,bufu) then exit;
  if bufv<>'0' then exit;
  if not getval('EASTERNMOST_LONGITUDE',bufv,bufu) then exit;
  if bufv<>'360' then exit;

  // set parameters
  if not getval('FILE_NAME',bufv,bufu) then exit;
  FDemHdr.FILE_NAME:=slash(fdir)+bufv;
  if not FileExists(DemHdr.FILE_NAME) then exit;
  if not getval('LINE_FIRST_PIXEL',bufv,bufu) then exit;
  FDemHdr.LINE_FIRST_PIXEL:=StrToInt(bufv);
  if not getval('LINE_LAST_PIXEL',bufv,bufu) then exit;
  FDemHdr.LINE_LAST_PIXEL:=StrToInt(bufv);
  if not getval('SAMPLE_FIRST_PIXEL',bufv,bufu) then exit;
  FDemHdr.SAMPLE_FIRST_PIXEL:=StrToInt(bufv);
  if not getval('SAMPLE_LAST_PIXEL',bufv,bufu) then exit;
  FDemHdr.SAMPLE_LAST_PIXEL:=StrToInt(bufv);
  if not getval('MAP_RESOLUTION',bufv,bufu) then exit;
  if bufu<>'PIX/DEG' then exit;
  FDemHdr.MAP_RESOLUTION:=StrToInt(bufv);
  if not getval('SCALING_FACTOR',bufv,bufu) then exit;
  FDemHdr.SCALING_FACTOR:=StrToFloat(bufv);
  if not getval('OFFSET',bufv,bufu) then exit;
  FDemHdr.OFFSET:=StrToFloat(bufv);
  if not getval('LINE_PROJECTION_OFFSET',bufv,bufu) then exit;
  FDemHdr.LINE_PROJECTION_OFFSET:=StrToFloat(bufv);
  if not getval('SAMPLE_PROJECTION_OFFSET',bufv,bufu) then exit;
  FDemHdr.SAMPLE_PROJECTION_OFFSET:=StrToFloat(bufv);
  if not getval('MAP_SCALE',bufv,bufu) then exit;
  FDemHdr.MAP_SCALE:=StrToFloat(bufv);
  if not getval('CENTER_LATITUDE',bufv,bufu) then exit;
  FDemHdr.CENTER_LATITUDE:=StrToFloat(bufv);
  if not getval('CENTER_LONGITUDE',bufv,bufu) then exit;
  FDemHdr.CENTER_LONGITUDE:=StrToFloat(bufv);

  result:=true;
  except
    result:=false;
  end;
  finally
    if not result then FLastMessage:='Error '+fn+', '+bufk+' '+bufv+' '+bufu;
    p.Free;
    k.Free;
    v.Free;
    u.Free;
  end;
end;

function Tdem.GetDemCenter(lon,lat: double; n: integer; out d:TSmallintArray): boolean;
var pxc,pyc,s,i,j: integer;
    x: smallint;
begin
  result:=false;
  if lon<0 then lon:=lon+360;
  SetLength(d,n,n);
  pxc:=round(lon*DemHdr.MAP_RESOLUTION);
  pyc:=round((90-lat)*DemHdr.MAP_RESOLUTION);
  s:=n div 2;
  if pyc<=s then pyc:=s+1;
  if pyc>=(DemHdr.LINE_LAST_PIXEL-s) then pyc:=DemHdr.LINE_LAST_PIXEL-s-1;
  for j:=0 to n-1 do begin
    seek(fDem, (pyc-s+j)*DemHdr.SAMPLE_LAST_PIXEL*2+2*(pxc-s));
    for i:=0 to n-1 do begin
      BlockRead(fDem,x,sizeof(smallint));
      d[i,j]:=x;
    end;
  end;
  result:=true;
end;

function Tdem.GetDemArea(lon1,lat1,lon2,lat2: double; out d:TSmallintArray): boolean;
var w,h: integer;
    px,py,i,j: integer;
    x: smallint;
begin
  result:=false;
  if lon1<0 then lon1:=lon1+360;
  if lon2<0 then lon2:=lon2+360;
  w:=round(abs(lon2-lon1)*DemHdr.MAP_RESOLUTION);
  h:=round(abs(lat2-lat1)*DemHdr.MAP_RESOLUTION);
  SetLength(d,w,h);
  if lon1<lon2 then
    px:=round(lon1*DemHdr.MAP_RESOLUTION)
  else
    px:=round(lon2*DemHdr.MAP_RESOLUTION);
  if lat1>lat2 then
    py:=round((90-lat1)*DemHdr.MAP_RESOLUTION)
  else
    py:=round((90-lat2)*DemHdr.MAP_RESOLUTION);
  if py<=0 then py:=0;
  if py=(DemHdr.LINE_LAST_PIXEL-h) then py:=DemHdr.LINE_LAST_PIXEL-h-1;
  for j:=0 to h-1 do begin
    seek(fDem, (py+j)*DemHdr.SAMPLE_LAST_PIXEL*2+2*px);
    for i:=0 to w-1 do begin
      BlockRead(fDem,x,sizeof(smallint));
      d[i,j]:=x;
    end;
  end;
  result:=true;
end;

{function Tdem.OpenPalette(n:string):boolean;
var i: integer;
    r,g,b: double;
    f: TextFile;
begin
  result:=false;
  AssignFile(f,n);
  reset(f);
  palette[0].red:=0;
  palette[0].green:=0;
  palette[0].blue:=0;
  palette[0].alpha:=255;
  for i:=1 to 255 do begin
    ReadLn(f,r,g,b);
    palette[i].red:=round(r*255);
    palette[i].green:=round(g*255);
    palette[i].blue:=round(b*255);
    palette[i].alpha:=255;
  end;
  CloseFile(f);
end;  }

{function Tdem.GetBitmap(d:TSmallintArray; var bgra: TBGRABitmap):boolean;
var p: PBGRAPixel;
    w,h,i,j,mi,ma: integer;
    c: double;
begin
  result:=false;
  mi:=MaxInt;
  ma:=-MaxInt;
  w:=Length(d);
  if w=0 then exit;
  h:=Length(d[0]);
  bgra.SetSize(w,h);
  for j:=0 to h-1 do begin
    for i:=0 to w-1 do begin
      mi:=min(mi,d[i,j]);
      ma:=max(ma,d[i,j]);
    end;
  end;
  ma:=ma+32768;
  mi:=mi+32768;
  c:=65535/(ma-mi);
  for j:=0 to h-1 do begin
    p := bgra.Scanline[j];
    for i:=0 to w-1 do begin
      p^:=palette[trunc((d[i,j]-mi+32768)*c/255)];
      inc(p);
    end;
  end;
  bgra.InvalidateBitmap;
  result:=true;
end; }

function Tdem.GetDemElevation(lon,lat: double): double;
var pxc,pyc: integer;
    x: smallint;
begin
if fDemOpen then begin
  try
  pxc:=round(lon*DemHdr.MAP_RESOLUTION);
  pyc:=round((90-lat)*DemHdr.MAP_RESOLUTION);
  seek(fDem, pyc*DemHdr.SAMPLE_LAST_PIXEL*2+2*pxc);
  BlockRead(fDem,x,sizeof(smallint));
  result:=x*DemHdr.SCALING_FACTOR;
  finally
  end;
end
else result:=0;
end;

procedure Tdem.GreatCircle(lon1,lat1,lon2,lat2,r: double; var c:TGreatCircle);
// https://en.wikipedia.org/wiki/Great-circle_navigation
var l01,l12,a1,s12: double;
begin
  c.lon1:=lon1;
  c.lat1:=lat1;
  c.lon2:=lon2;
  c.lat2:=lat2;
  c.radius:=r;
  l12:=c.lon2-c.lon1;
  if l12>pi then l12:=l12-pi2;
  if l12<-pi then l12:=l12+pi2;
  a1:=ArcTan2(cos(c.lat2)*sin(l12),cos(c.lat1)*sin(c.lat2)-sin(c.lat1)*cos(c.lat2)*cos(l12));
  s12:=ArcTan2(sqrt((cos(c.lat1)*sin(c.lat2)-sin(c.lat1)*cos(c.lat2)*cos(l12))**2 + (cos(c.lat2)*sin(l12))**2),sin(c.lat1)*sin(c.lat2)+cos(c.lat1)*cos(c.lat2)*cos(l12));
  c.dist:=s12*c.radius;
  c.a0:=ArcTan2(sin(a1)*cos(c.lat1),sqrt((cos(a1)**2)+((sin(a1)**2)*(sin(c.lat1)**2))));
  if cos(a1)=0 then
    c.s01:=0
  else
    c.s01:=ArcTan2(tan(c.lat1),cos(a1));
  c.s02:=c.s01+s12;
  l01:=ArcTan2(sin(c.a0)*sin(c.s01),cos(c.s01));
  c.l0:=c.lon1-l01;
end;

procedure Tdem.PointOnCircle(c:TGreatCircle; s: double; out la,lo: double);
var ll:double;
begin
  la:=ArcTan2(cos(c.a0)*sin(s),sqrt((cos(s))**2+((sin(c.a0))**2)*((sin(s))**2)));
  ll:=ArcTan2(sin(c.a0)*sin(s),cos(s));
  lo:=ll+c.l0;
end;

end.

