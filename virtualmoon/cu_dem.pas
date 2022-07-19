unit cu_dem;

{$mode ObjFPC}{$H+}
interface

uses u_util, u_constant, {BGRABitmap, BGRABitmapTypes,} math,
  Classes, SysUtils;

// Download LDEM files from
// https://pds-geosciences.wustl.edu/lro/lro-l-lola-3-rdr-v1/lrolol_1xxx/data/lola_gdr/cylindrical/img/
// keep lowercase filename

const
  MaxDemFile=144; //  ldem_1024
  MaxDemList=7;   // 4,16,64,128,256,512,1024 pix/deg
  MaxId=2;        // moon1,moon2

type

  TDemHdr = record
     FILE_NAME : string;
     MAP_RESOLUTION,LINE_FIRST_PIXEL,LINE_LAST_PIXEL,SAMPLE_FIRST_PIXEL,SAMPLE_LAST_PIXEL  : integer;
     SCALING_FACTOR,OFFSET,CENTER_LATITUDE,CENTER_LONGITUDE,MAP_SCALE,LINE_PROJECTION_OFFSET,SAMPLE_PROJECTION_OFFSET: double;
     MINIMUM_LATITUDE,MAXIMUM_LATITUDE,WESTERNMOST_LONGITUDE,EASTERNMOST_LONGITUDE: integer;
  end;

  TSmallintArray = array of array of smallint;

  Tdem = class(TObject)
   private
    FDemHdr: array[0..MaxDemFile] of TDemHdr;
    fDemFileOpen: array[0..MaxDemFile] of boolean;
    fDem: array[0..MaxDemFile] of file;
    fDemNum: array[0..360,-90..90] of SmallInt;
    fMapResolution: integer;
    fDemOpen: boolean;
    fNumDem: integer;
    FLastMessage: string;
    //    palette: array[0..255] of TBGRAPixel;
    function ReadDemHDR(fn: string; var h:TDemHdr):boolean;
   public
    constructor Create;
    destructor Destroy; override;
    function OpenDem(path,n:string):boolean;
    function GetDemElevation(lon,lat: double): double;
    property DemOpen: Boolean read fDemOpen;
    property MapResolution: integer read fMapResolution;
    property LastMessage: string read FLastMessage;
    //    function OpenPalette(n:string):boolean;
    //    function GetDemCenter(lon,lat: double; n: integer; out d:TSmallintArray): boolean;
    //    function GetDemArea(lon1,lat1,lon2,lat2: double; out d:TSmallintArray): boolean;
    //    function GetBitmap(d:TSmallintArray; var bgra: TBGRABitmap):boolean;
  end;

  TdemLibrary = class(TObject)
  private
    FDems: array[0..MaxDemList] of Tdem;
    FDemsRes: array[0..MaxDemList] of integer;
    FDemsName: array[0..MaxDemList] of string;
    FDemsPath: array[0..MaxDemList] of string;
    fNumDems: integer;
    FCurrentDem,FCurrentResolution: array[0..MaxId] of integer;
    FSetResolution: array[0..MaxId] of double;
    FLastMessage: string;
    function GetOpen: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function AddPath(path:string):boolean;
    procedure SetResolution(id:integer; value:double);
    function GetResolution(id:integer): integer;
    function GetElevation(id:integer; lon,lat: double): double;
    property Open: boolean read GetOpen;
    property LastMessage: string read FLastMessage;
  end;

implementation

Function SplitLbl(buf: string; var bufk,bufv,bufu: string): boolean;
var p: TStringList;
    i: integer;
begin
  p:=TStringList.Create;
  try
    result:=false;
    if copy(buf,1,2)='/*' then exit;
    SplitRec(UpperCase(buf),'=',p);
    if p.Count<>2 then exit;
    result:=true;
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
  finally
    p.free;
  end;
end;

function DemCompare(List: TStringList; Index1, Index2: Integer): Integer;
var res1,res2: integer;
function res(fn:string):integer;
var p:integer;
begin
  p:=pos('_',fn);
  delete(fn,1,p);
  p:=pos('_',fn);
  if p=0 then p:=pos('.',fn);
  fn:=copy(fn,1,p-1);
  result:=StrToIntDef(fn,0);
end;
begin
  res1:=res(List[Index1]);
  res2:=res(List[Index2]);
  if res1=res2 then
    result:=0
  else if res1>res2 then
    result:=1
  else
    result:=-1;
end;

//// TDemLibrary ////

constructor TdemLibrary.Create;
var i:integer;
begin
  inherited Create;
  fNumDems:=0;
  for i:=0 to MaxId do begin
    FCurrentDem[i]:=0;
    FCurrentResolution[i]:=0;
    FSetResolution[i]:=0;
  end;
end;

destructor TdemLibrary.Destroy;
var i: integer;
begin
  for i:=0 to fNumDems-1 do
     FDems[i].Free;
  inherited Destroy;
end;

function TdemLibrary.GetOpen: boolean;
begin
  result:=fNumDems>0;
end;

function TdemLibrary.AddPath(path:string):boolean;
var k,v,u,buf: string;
    f: Tsearchrec;
    ft: textfile;
    found: boolean;
    i,j,res: integer;
    demlist: TStringList;
begin
  FLastMessage:='';
  result:=false;
  demlist:=TStringList.Create;
  try
  i:=FindFirst(slash(path)+'ldem*.lbl',faNormal,f);
  if i<>0 then FLastMessage:='No DEM found';
  while i=0 do begin
    demlist.Add(f.Name);
    i:=FindNext(f);
  end;
  FindClose(f);
  demlist.CustomSort(@DemCompare);
  for i:=0 to demlist.Count-1 do begin
    AssignFile(ft,slash(path)+demlist[i]);
    reset(ft);
    res:=0;
    repeat
      ReadLn(ft,buf);
      if not SplitLbl(buf,k,v,u) then continue;
      if k='MAP_RESOLUTION' then begin
        if (u='PIX/DEG')or(u='PIXEL/DEG') then
          res:=StrToInt(v);
        break;
      end;
    until EOF(ft);
    CloseFile(ft);
    if res>0 then begin
      found:=false;
      for j:=0 to fNumDems-1 do begin
        if FDemsRes[j]=res then begin
           found:=true;
           break;
        end;
      end;
      if (not found)and(fNumDems<MaxDemList) then begin
         result:=true;
         inc(fNumDems);
         FDemsRes[fNumDems-1]:=res;
         FDemsPath[fNumDems-1]:=path;
         FDemsName[fNumDems-1]:='ldem_'+IntToStr(res);
         FDems[fNumDems-1]:=Tdem.Create;
         if not FDems[fNumDems-1].OpenDem(path,FDemsName[fNumDems-1]) then
           FLastMessage:=FDems[fNumDems-1].LastMessage;
      end;
    end;
  end;
  finally
    demlist.Free;
  end;
end;

procedure TdemLibrary.SetResolution(id:integer; value:double);
var i: integer;
begin
  if value<>FSetResolution[id] then begin
    FSetResolution[id]:=value;
    for i:=0 to fNumDems-1 do begin
       if (FDemsRes[i]>=FSetResolution[id])or(i=fNumDems-1) then begin
         FCurrentDem[id]:=i;
         FCurrentResolution[id]:=FDemsRes[i];
         break;
       end;
    end;
  end;
end;

function TdemLibrary.GetResolution(id:integer): integer;
begin
  result:=FCurrentResolution[id];
end;

function TdemLibrary.GetElevation(id:integer; lon,lat: double): double;
begin
  result:=FDems[FCurrentDem[id]].GetDemElevation(lon,lat);
end;

//// TDem ////

constructor Tdem.Create;
var i: integer;
begin
  inherited Create;
  fDemOpen:=false;
  for i:=0 to MaxDemFile do
    fDemFileOpen[i]:=false;
end;

destructor Tdem.Destroy;
var i: integer;
begin
  if fDemOpen then
    for i:=0 to MaxDemFile do
      if fDemFileOpen[i] then CloseFile(fdem[i]);
  inherited Destroy;
end;

function Tdem.OpenDem(path,n:string):boolean;
var f: Tsearchrec;
    d,i,j,k: integer;
begin
  result:=false;
  try
  for i:=0 to 360 do begin
    for j:=-90 to 90 do begin
       fDemNum[i,j]:=-1;
    end;
  end;
  FLastMessage:='';
  d:=0;
  k:=FindFirst(slash(path)+n+'*'+'.lbl', faNormal, f);
  while k=0 do begin
    if d>MaxDemFile then
      raise(exception.Create('Maximum number of DEM files reach!'));
    if not ReadDemHDR(slash(path)+f.Name,FDemHdr[d]) then exit;
    if d=0 then
      fMapResolution:=FDemHdr[d].MAP_RESOLUTION
    else if fMapResolution<>FDemHdr[d].MAP_RESOLUTION then
      raise(exception.Create('Inconsistent MAP_RESOLUTION!'));
    AssignFile(fDem[d],FDemHdr[d].FILE_NAME);
    reset(fDem[d],1);
    fDemFileOpen[d]:=true;
    for i:=FDemHdr[d].WESTERNMOST_LONGITUDE to FDemHdr[d].EASTERNMOST_LONGITUDE-1 do begin
      for j:=FDemHdr[d].MINIMUM_LATITUDE to FDemHdr[d].MAXIMUM_LATITUDE-1 do begin
        fDemNum[i,j]:=d;
      end;
    end;
    inc(d);
    k:=FindNext(f);
  end;
  FindClose(f);
  fNumDem:=d;
  if fNumDem=0 then raise(exception.Create('No '+n+' file found!'));
  fDemOpen:=true;
  result:=fDemOpen;
  except
   on E: Exception do begin
     result:=false;
     fDemOpen:=false;
     FLastMessage:='Open '+FDemHdr[d].FILE_NAME+' : '+E.Message;
   end;
  end;
end;

function Tdem.ReadDemHDR(fn: string; var h:TDemHdr):Boolean;
var f: TextFile;
    fdir,buf,bufk,bufv,bufu: string;
    k,v,u: TStringList;
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
    if not SplitLbl(buf,bufk,bufv,bufu) then continue;
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

  // set parameters
  if not getval('FILE_NAME',bufv,bufu) then exit;
  h.FILE_NAME:=slash(fdir)+lowercase(bufv);
  if not FileExists(h.FILE_NAME) then exit;
  if not getval('LINE_FIRST_PIXEL',bufv,bufu) then exit;
  h.LINE_FIRST_PIXEL:=StrToInt(bufv);
  if not getval('LINE_LAST_PIXEL',bufv,bufu) then exit;
  h.LINE_LAST_PIXEL:=StrToInt(bufv);
  if not getval('SAMPLE_FIRST_PIXEL',bufv,bufu) then exit;
  h.SAMPLE_FIRST_PIXEL:=StrToInt(bufv);
  if not getval('SAMPLE_LAST_PIXEL',bufv,bufu) then exit;
  h.SAMPLE_LAST_PIXEL:=StrToInt(bufv);
  if not getval('MAP_RESOLUTION',bufv,bufu) then exit;
  if (bufu<>'PIX/DEG')and(bufu<>'PIXEL/DEG') then exit;
  h.MAP_RESOLUTION:=StrToInt(bufv);
  if not getval('SCALING_FACTOR',bufv,bufu) then exit;
  h.SCALING_FACTOR:=StrToFloat(bufv);
  if not getval('OFFSET',bufv,bufu) then exit;
  h.OFFSET:=StrToFloat(bufv);
  if not getval('LINE_PROJECTION_OFFSET',bufv,bufu) then exit;
  h.LINE_PROJECTION_OFFSET:=StrToFloat(bufv);
  if not getval('SAMPLE_PROJECTION_OFFSET',bufv,bufu) then exit;
  h.SAMPLE_PROJECTION_OFFSET:=StrToFloat(bufv);
  if not getval('MAP_SCALE',bufv,bufu) then exit;
  h.MAP_SCALE:=StrToFloat(bufv);
  if not getval('CENTER_LATITUDE',bufv,bufu) then exit;
  h.CENTER_LATITUDE:=StrToFloat(bufv);
  if not getval('CENTER_LONGITUDE',bufv,bufu) then exit;
  h.CENTER_LONGITUDE:=StrToFloat(bufv);
  if not getval('MAXIMUM_LATITUDE',bufv,bufu) then exit;
  h.MAXIMUM_LATITUDE:=StrToInt(bufv);
  if not getval('MINIMUM_LATITUDE',bufv,bufu) then exit;
  h.MINIMUM_LATITUDE:=StrToInt(bufv);
  if not getval('WESTERNMOST_LONGITUDE',bufv,bufu) then exit;
  h.WESTERNMOST_LONGITUDE:=StrToInt(bufv);
  if not getval('EASTERNMOST_LONGITUDE',bufv,bufu) then exit;
  h.EASTERNMOST_LONGITUDE:=StrToInt(bufv);

  result:=true;
  except
    result:=false;
  end;
  finally
    if not result then FLastMessage:='Error '+fn+', '+bufk+' '+bufv+' '+bufu;
    k.Free;
    v.Free;
    u.Free;
  end;
end;

function Tdem.GetDemElevation(lon,lat: double): double;
var pxc,pyc,d: integer;
    x: smallint;
begin
result:=0;
try
if fDemOpen then begin
  if lon<0 then lon:=lon+360;
  if lon>360 then lon:=lon-360;
  d:=fDemNum[Trunc(lon),Floor(lat)];
  if d>=0 then begin
    pxc:=min(FDemHdr[d].SAMPLE_LAST_PIXEL-1,round((lon-FDemHdr[d].WESTERNMOST_LONGITUDE)*FDemHdr[d].MAP_RESOLUTION));
    pyc:=min(FDemHdr[d].LINE_LAST_PIXEL-1, round((FDemHdr[d].MAXIMUM_LATITUDE-lat)*FDemHdr[d].MAP_RESOLUTION));
    seek(fDem[d], pyc*FDemHdr[d].SAMPLE_LAST_PIXEL*2+2*pxc);
    BlockRead(fDem[d],x,sizeof(smallint));
    result:=x*FDemHdr[d].SCALING_FACTOR;
  end;
end;
except
  result:=0;
end;
end;

{ Bitmap function not used for now

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

function Tdem.OpenPalette(n:string):boolean;
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

end.

