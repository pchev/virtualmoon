unit SkyLib;
{
Copyright (C) 2003 Patrick Chevalley

http://www.astrosurf.com/avl
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
interface
uses ShellAPI,Inifiles,Math, Printers, Winspool,UrlMon,
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

Procedure Init;
Procedure SetLang;
Procedure ScaleWindow;
Procedure WindowXY(x,y:Double; var WindowX,WindowY: Integer);
Procedure XYWindow( x,y: Integer; var Xwindow,Ywindow: double);
PROCEDURE Projection(ar,de : Double ; VAR X,Y : Double; clip:boolean );
PROCEDURE Proj2(ar,de,ac,dc : Double ; VAR X,Y : Double );
Procedure InvProj (xx,yy : Double ; VAR ar,de : Double );
Procedure InvProj2 (xx,yy,ac,dc : Double ; VAR ar,de : Double );
procedure GetADxy(x,y:Integer ; var a,d : Double);
procedure GetAHxy(x,y:Integer ; var a,h : Double);
procedure GetAHxyF(x,y:Integer ; var a,h : Double);
function NorthPoleInMap : Boolean;
function SouthPoleInMap : Boolean;
function ZenithInMap : Boolean;
function NadirInMap : Boolean;
Function AngularDistance(ar1,de1,ar2,de2 : Double) : Double;
function PositionAngle(ac,dc,ar,de:double):double;
Function DEToStr(de: Double) : string;
Function ARtoStr(ar: Double) : string;
Function DEmToStr(de: Double) : string;
Function DEdToStr(de: Double) : string;
Function ARmtoStr(ar: Double) : string;
Function DEpToStr(de: Double) : string;
Function ARptoStr(ar: Double) : string;
Function TimToStr(de: Double) : string;
Function ARToStr2(ar: Double; var d,m,s : string) : string;
Function DEToStr2(de: Double; var d,m,s : string) : string;
Function DEToStr3(de: Double) : string;
Function DEToStr4(de: Double) : string;
function  Rmod(x,y:Double):Double;
Function sgn(x:Double):Double ;
Function PadZeros(x : string ; l :integer) : string;
function Jd(annee,mois,jour :INTEGER; Heure:double):double;
PROCEDURE Djd(jd:Double;VAR annee,mois,jour:INTEGER; VAR Heure:double);
function SidTim(jd0,ut,long : double): double;
procedure Paralaxe(SideralTime,dist,ar1,de1 : double; var ar,de,q : double);
function DTminusUT(annee : integer) : double;
PROCEDURE PrecessionFK4(ti,tf : double; VAR ari,dei : double);
PROCEDURE Precession(ti,tf : double; VAR ari,dei : double);
procedure PrecessionEcl(ti,tf : double; VAR l,b : double);
function words(str,sep : string; p,n : integer) : string;
Procedure FormPos(form : Tform; x,y : integer);
function InvertI16(X : Word) : SmallInt;
function InvertI32(X : LongWord) : LongInt;
function InvertI64(X : Int64) : Int64;
function InvertF32(X : LongWord) : Single;
function InvertF64(X : Int64) : Double;
PROCEDURE Horizontal(HH,DE : double ; VAR A,h : double );
          { HH = angle horaire
            de = declinaison
            A  = azimut
            h  = hauteur
          }
PROCEDURE HorizontalGeometric(HH,DE : double ; VAR A,h : double );
PROCEDURE Eq2Hz(HH,DE : double ; VAR A,h : double );
Procedure Hz2Eq(A,h : double; var hh,de : double);
Procedure Ecl2Eq(l,b,e: double; var ar,de : double);
Procedure Eq2Ecl(ar,de,e: double; var l,b: double);
Procedure Gal2Eq(l,b: double; var ar,de : double);
Procedure RiseSet(typobj:integer; jd0,ar,de:double; var hr,ht,hs,azr,azs:double;var irc:integer);
          { typeobj = 1 etoile ; typeobj = 2 soleil,lune
            irc = 0 lever et coucher
            irc = 1 circumpolaire
            irc = 2 invisible
          }
Procedure HeurePo(jd,ar,de,h :Double; VAR hp1,hp2 :Double );
          (*
             Heure a laquel un astre est a un hauteur donnee sur l'horizon .
             jd       :  date julienne desiree a 0H TU
             ar       :  ascension droite
             de       :  declinaison
             h        :  hauteur sur l'horizon
                         crepuscule nautique h=-12
                         crepuscule astronomique h=-18
             hp1      :  heure matin
             hp2      :  heure soir
           *)
Function GetTempFile(i : integer) : String;
Function ExecuteFile(const FileName, Params, DefaultDir: string; ShowCmd: Integer): THandle;
Function Exec(cmd: string; hide: boolean): cardinal;
Function ExecNoWait(cmd: string; hide: boolean): boolean;
Procedure InitDebug;
Procedure WriteDebug( buf : string);
Procedure MemoryDebug;
Procedure ShowHelp(helpfile : string);
procedure RiseSetInt(typobj:integer; jd0,ar1,de1,ar2,de2,ar3,de3:double; var hr,ht,hs,azr,azs:double;var irc:integer);
Procedure GetPrinterResolution(var name : string; var resol : integer);
Function mm2pi(l : single): integer;
Function Slash(nom : string) : string;
Function NoSlash(nom : string) : string;
Function YearADBC(year : integer) : string;
function IsNumber(n : string) : boolean;
function AddColor(c1,c2 : Tcolor):Tcolor;
function SubColor(c1,c2 : Tcolor):Tcolor;
function roundF(x:double;n:integer):double;

//  gzip zlib.dll
type
 Tgzopen =Function(path,mode :pchar): integer ; Stdcall;
 Tgzread =Function(gzFile: integer; buf : pchar; len:cardinal): integer; Stdcall;
 Tgzeof =Function(gzFile: integer): integer; Stdcall;
 Tgzclose =Function(gzFile: integer): integer; Stdcall;
var gzopen : Tgzopen;
    gzread : Tgzread;
    gzeof : Tgzeof;
    gzclose : Tgzclose;

// SERIES96.DLL
type double6 = array[1..6] of double;
     Pdouble6 = ^double6;
     TPlan96 = Procedure(tjd:double;ipla:integer; velocities:boolean ; Pr: Pdouble6; var ierr:integer); stdcall;
     TEarth96 = Procedure(tjd:double; Pr : Pdouble6) stdcall;
     TSetSeriesPath = Procedure(path : shortstring) stdcall;
var Plan96 : Tplan96;
    Earth96 : TEarth96;
    SetSeriesPath : TSetSeriesPath;

// PLAN404.DLL
type
TPlanetData = record
   JD : double;
   l : double ;
   b : double ;
   r : double ;
   x : double ;
   y : double ;
   z : double ;
   ipla : integer;
   end;
PPlanetData = ^TPlanetData;
Function Plan404( pla : PPlanetData):integer; cdecl; external 'PLAN404.DLL';

// SATXY.DLL
type double8 = array[1..8] of double;
     Pdouble8 = ^double8;
     TSatxyfm = Function(djc : double; ipla : integer; Pxx,Pyy : Pdouble8):integer; stdcall;
var  Satxyfm : TSatxyfm;

// ELP82.DLL
Procedure Lune(t0 : double; var alpha,delta,dist,dkm,diam,phase,illum : double) stdcall; external 'ELP82.DLL';
Procedure LuneIncl(Lar,Lde,Sar,Sde : double; var incl : double) stdcall; external 'ELP82.DLL';
Procedure CalcMoonRiseSet(var Latitude,Longitude,TimeZone : double;
                          var Year,Month,Day : integer;
                          var moonrise,moonset,moontransit,azimuthrise,azimuthset : widestring); stdcall; external 'elp82.dll';

const MaxSim = 100 ;
      MaxComet = 500;
      MaxAsteroid = 500;
      MaxPla = 32;
      jd2000 : double =2451545.0 ;
      jd1950 : double =2433282.4235;
      deg2rad = pi/180;
      rad2deg = 180/pi;

type Tplanetlst = array[0..MaxSim,1..MaxPla,1..7] of double; // 1..9 : planet ; 10 : soleil ; 11 : lune ; 12..15 : jup sat ; 16..23 : sat sat ; 24..28 : ura sat ; 29..30 : mar sat ; 31 : sat ring ; 32 : earth umbra ;
     Tcometlst = array[0..MaxSim,1..MaxComet,1..7] of double;
     Tasteroidlst = array[0..MaxSim,1..MaxAsteroid,1..4] of double;
var
  pla : array[1..11] of string[8] = ('Mercure ','Venus   ','*       ','Mars    ','Jupiter ','Saturne ','Uranus  ','Neptune ','Pluton  ','Soleil  ','Lune    ');
  arcentre,decentre,largeur,theta,Acentre,hcentre : Double;
  xmin,xmax,ymin,ymax,Xshift,Yshift : Integer;
  WindowRatio,BxGlb,ByGlb,AxGlb,AyGlb,sintheta,costheta: Double;
  Xwrldmin,Xwrldmax,Ywrldmin,Ywrldmax: Double;
  LeftMargin,RightMargin,TopMargin,BottomMargin,Xcentre,Ycentre: Integer;
  ObsLatitude,ObsLongitude,ObsAltitude,SaveObsLatitude,SaveObsLongitude : double;
  ObsRoSinPhi,ObsRoCosPhi,ObsTemperature,ObsPressure,ObsRefractionCor : Double;
  CurYear,CurrentMonth,CurrentDay : integer;
  CurrentTime,TimeBias,DT_UT,CurrentST,CurrentJD,DT_UT_val,CurrentSunH,CurrentMoonH,CurrentMoonIllum : Double;
  UseSeries96 : Boolean = false;
  Force_DT_UT : Boolean = false;
  arref,deref : double;
  projtype : char;
  Language : shortstring;
  commsg : array [1..10] of string;
  DebugOn,ShowConstl,ShowConstB,NetworkOn,ByPassNetworkCheck,zlibok,PlanetParalaxe,satxyok,AzNorth : boolean;
  AppDir,hp,ObsNom,SaveObsNom : string;
  debugfile : string;
  FlipX, FlipY, ProjPole,ReqProj,lastprojpole : integer;
  PrinterResolution, DefaultPrtRes : Integer;
  PrtName : string;
  PrtLeftMargin,PrtRightMargin,PrtTopMargin,PrtBottomMargin: single;
  InternalBrowser,helpcreated,SaveObsOn,horizonopaque : boolean;
//  ldeg,lmin,lsec : char;
  ldeg,lmin,lsec : string;
  dummy_double : double;
  zlib,satxy,series96 : Dword;
  horizonlist : array [0..360] of single;
  HorizonMax : single;

const crlf=chr(10)+chr(13);
      greek : array[1..2,1..24]of string=(('Alpha','Beta','Gamma','Delta','Epsilon','Zeta','Eta','Theta','Iota','Kappa','Lambda','Mu','Nu','Xi','Omicron','Pi','Rho','Sigma','Tau','Upsilon','Phi','Chi','Psi','Omega'),
              ('alp','bet','gam','del','eps','zet','eta','the','iot','kap','lam','mu','nu','xi','omi','pi','rho','sig','tau','ups','phi','chi','psi','ome'));
      greeksymbol : array[1..2,1..24]of string=(('alp','bet','gam','del','eps','zet','eta','the','iot','kap','lam','mu','nu','xi','omi','pi','rho','sig','tau','ups','phi','chi','psi','ome'),
                  ('a','b','g','d','e','z','h','q','i','k','l','m','n','x','o','p','r','s','t','u','f','c','y','w'));

implementation

uses helpunit, catalogues
{$IFDEF VER140}
, variants
{$ENDIF}
;

var fdebug : textfile;

function AddColor(c1,c2 : Tcolor):Tcolor;
var max,r,v,b : integer;
    f : double;
begin
r:=(c1 and $000000ff)+(c2 and $000000ff);
v:=((c1 and $0000ff00)+(c2 and $0000ff00)) shr 8;
b:=((c1 and $00ff0000)+(c2 and $00ff0000)) shr 16;
max:=maxintvalue([r,v,b]);
if max>255 then begin
  f:=255/max;
  r:=trunc(f*r);
  v:=trunc(f*v);
  b:=trunc(f*b);
end;
result:=r+256*v+65536*b;
end;

function SubColor(c1,c2 : Tcolor):Tcolor;
var min,r,v,b : integer;
    f : double;
begin
r:=(c1 and $000000ff)-(c2 and $000000ff);
v:=((c1 and $0000ff00)-(c2 and $0000ff00)) shr 8;
b:=((c1 and $00ff0000)-(c2 and $00ff0000)) shr 16;
min:=minintvalue([r,v,b]);
if min<0 then begin
  f:=255/(255-min);
  r:=trunc(f*(r-min));
  v:=trunc(f*(v-min));
  b:=trunc(f*(b-min));
end;
result:=r+256*v+65536*b;
end;

Function NoSlash(nom : string) : string;
begin
result:=trim(nom);
if copy(result,length(nom),1)='\' then result:=copy(result,1,length(nom)-1);
end;

Function Slash(nom : string) : string;
begin
result:=trim(nom);
if copy(result,length(nom),1)<>'\' then result:=result+'\';
end;

function IsNumber(n : string) : boolean;
var i : integer;
begin
val(n,Dummy_double,i);
result:= (i=0) ;
end;

function roundF(x:double;n:integer):double;
var y : double;
begin
y:=intpower(10,n);
result:=round(x*y)/y;
end;

Procedure InitDebug;
begin
 debugfile:='trace.txt';
 assignfile(fdebug,debugfile);
 rewrite(fdebug);
 writeln(fdebug,DateTimeToStr(Now)+'  Start trace');
 closefile(fdebug);
end;

Procedure WriteDebug( buf : string);
begin
try
 assignfile(fdebug,debugfile);
 append(fdebug);
 writeln(fdebug,DateTimeToStr(Now)+'  '+buf);
 closefile(fdebug);
except
{$I-}                       
 debugon:=false;
 closefile(fdebug);
{$I+}
end;
end;

Procedure MemoryDebug;
var mem : TMEMORYSTATUS;
    HeapStatus:THeapStatus;
    buf : string;
//    rSYS,rGDI,rUSR : integer;
begin
mem.dwLength:=sizeof(mem);
GlobalMemoryStatus(mem);
{$WARNINGS OFF}
HeapStatus:=GetHeapStatus;
{$WARNINGS ON}
buf:='Alloc:'+Inttostr((HeapStatus.TotalAllocated div 1024))+'kb'+' Free:'+Inttostr((HeapStatus.TotalFree div 1024))+'kb'+' AddrSpace:'+Inttostr((HeapStatus.TotalAddrSpace div 1024))+'kb'+' Load:'+inttostr(mem.dwMemoryLoad)+'% Real_Tot:'+inttostr(mem.dwTotalPhys)+' Real_Free:'+inttostr(mem.dwAvailPhys)+' Tot.Used:'+inttostr(mem.dwTotalVirtual-mem.dwAvailVirtual);
WriteDebug(buf);
//rSYS := GetFreeSystemResources(gfsr_SystemResources);
//rUSR := GetFreeSystemResources(gfsr_UserResources);
//rGDI := GetFreeSystemResources(gfsr_GDIResources);
//buf:='Free resources:  System:'+inttostr(rSYS)+'%  GDI:'+inttostr(rGDI)+'%  User:'+inttostr(rUSR)+'%';
//WriteDebug(buf);
end;

Function mm2pi(l : single): integer;
begin
result:=round(l*PrinterResolution/25.4);
end;

Procedure InitString;
begin
commsg[1]:='Erreur de définition des fichier temporaires';
commsg[2]:='Projection invalide';
commsg[3]:='Indiquer le chemin correct pour';
commsg[4]:='Monter le disque pour';
commsg[5]:='Erreur de configuration pour';
commsg[6]:='Erreur de capacité';
end;

Procedure SetLang;
var section : string;
    inifile : Tmeminifile;
    i : integer;
const deftxt = '?';
      blank = '        ';
begin
if language>'0' then begin
inifile:=Tmeminifile.create(AppDir+'\language_'+language+'.ini');
section:='skylib';
with inifile do begin
   for i:=1 to 10 do begin
      commsg[i]:=ReadString(section,'m_'+trim(inttostr(i)),deftxt);
   end;
end;
section:='planet';
with inifile do begin
    for i:=1 to 11 do begin
       pla[i]:=copy(ReadString(section,'p_'+trim(inttostr(i)),deftxt)+blank,1,8);
    end;
end;
inifile.free;
SetCatLang(commsg[3],commsg[4],commsg[5],application.Title)
end;
end;

Procedure Init;
begin
InitString;
SetLang;
if debugon then writedebug('Load ZLib DLL');
zlib := LoadLibrary('zlib.dll');
if zlib<>0 then begin
    gzopen:= Tgzopen(GetProcAddress(zlib, 'gzopen'));
    gzread:= Tgzread(GetProcAddress(zlib, 'gzread'));
    gzeof:= Tgzeof(GetProcAddress(zlib, 'gzeof'));
    gzclose:= Tgzclose(GetProcAddress(zlib, 'gzclose'));
    zlibok:=true;
    if debugon then writedebug('ZLib DLL found');
end else begin
    zlibok:=false;
    if debugon then writedebug('ZLib DLL not found');
end;
if debugon then writedebug('Load Satxy DLL');
satxy:=LoadLibrary('satxy.dll');
if satxy<>0 then begin
    Satxyfm:= TSatxyfm(GetProcAddress(satxy, 'Satxyfm'));
    satxyok:=true;
    if debugon then writedebug('Satxy DLL found');
end else begin
    satxyok:=false;
    if debugon then writedebug('Satxy DLL not found');
end;
if debugon then writedebug('Load Series96 DLL');
series96:=LoadLibrary('series96.dll');
if series96<>0 then begin
    Plan96:= TPlan96(GetProcAddress(series96, 'Plan96'));
    Earth96:= TEarth96(GetProcAddress(series96, 'Earth96'));
    SetSeriesPath:= TSetSeriesPath(GetProcAddress(series96, 'SetSeriesPath'));
    UseSeries96:=true;
    if debugon then writedebug('Satxy DLL found');
end else begin
    UseSeries96:=false;
    if debugon then writedebug('Series96 DLL not found');
end;
end;

Function GetTempFile(i : integer) : String;
var N, P :String;
begin
  Result:='';
  SetLength( P, MAX_PATH );
  SetLength( P, GetTempPath( MAX_PATH, PChar( P ) ) );
  SetLength( N, MAX_PATH );
  if ( GetTempFileName( PChar( P ), '~ci', i, PChar( N ) ) = 0 )
    then begin ShowMessage(commsg[1]);exit;end;
  SetLength( N, lstrlen( PChar( N ) ) );
  Result := N;
end;

Function PadZeros(x : string ; l :integer) : string;
const zero = '000000000000';
var p : integer;
begin
x:=trim(x);
p:=l-length(x);
result:=copy(zero,1,p)+x;
end;

function  Rmod(x,y:Double):Double;
BEGIN
    Rmod := x - Int(x/y) * y ;
END  ;

Function sgn(x:Double):Double ;
begin
if x<0 then
   sgn:= -1
else
   sgn:=  1 ;
end ;

Procedure GetPrinterResolution(var name : string; var resol : integer);
var
  FDevice: PChar;
  FDriver: PChar;
  FPort: PChar;
  DeviceMode: THandle;
  DevMode: PDeviceMode;
  hPrinter: THandle;
begin
  GetMem(FDevice, 128);
  GetMem(FDriver, 128);
  GetMem(FPort, 128);
  Printer().GetPrinter(FDevice, FDriver, FPort, DeviceMode);
  if DeviceMode = 0 then
    Printer().GetPrinter(FDevice, FDriver, FPort, DeviceMode);
  OpenPrinter(FDevice, hPrinter, nil);
  DevMode := GlobalLock(DeviceMode);
  resol:=DevMode^.dmYResolution;
  if resol=0 then resol:=DevMode^.dmPrintQuality;
  name:=DevMode^.dmDeviceName;
  if debugon then WriteDebug(Format('Printer %s  Resolution: %d', [name ,resol]));
  if resol=0 then resol:=DefaultPrtRes;
//  ShowMessage(Format('dmYResolution: %d', [resol]));
  GlobalUnlock(DeviceMode);
  FreeMem(FDevice, 128);
  FreeMem(FDriver, 128);
  FreeMem(FPort, 128);
end;

Procedure ScaleWindow;
var X1,x2,Y1,Y2 : Integer;
begin
   X1 := Xmin+LeftMargin ; X2 := Xmax-RightMargin;
   Y1 := Ymin+TopMargin ; Y2 := Ymax-BottomMargin;
   WindowRatio := (X2-X1) / (Y2-Y1) ;
   Xcentre:=round((x2-x1)/2);
   Ycentre:=round((y2-y1)/2);
   Xwrldmin := -largeur/2;
   Ywrldmin := -largeur/WindowRatio/2;
   Xwrldmax := largeur/2;
   Ywrldmax := largeur/WindowRatio/2;
   BxGlb:= FlipX * (X2-X1) / (Xwrldmax-Xwrldmin) ;
   ByGlb:= FlipY * (Y1-Y2) / (Ywrldmax-Ywrldmin) ;
   AxGlb:= FlipX * (- Xwrldmin * BxGlb) ;
   AyGlb:=  FlipY * ( Ywrldmin * ByGlb) ;
   sintheta:=sin(deg2rad*(theta));
   costheta:=cos(deg2rad*(theta));
end;

Procedure WindowXY(x,y:Double; var WindowX,WindowY: Integer);
BEGIN
    WindowX:=round(AxGlb+BxGlb*x)+Xshift;
    WindowY:=round(AyGlb+ByGlb*y)+Yshift;
END ;

Procedure XYWindow( x,y: Integer; var Xwindow,Ywindow: double);
Begin
   xwindow:= (x-xshift-Axglb)/BxGlb ;
   ywindow:= (y-yshift-AyGlb)/ByGlb;
end ;

PROCEDURE Proj2(ar,de,ac,dc : Double ; VAR X,Y : Double );
Var r,hh,s1,s2,s3,c1,c2,c3,xx,yy : Extended ;
BEGIN
case projtype of              // AIPS memo 27
'A' : begin                   //  ARC
    hh := Deg2Rad*(ac-ar) ;
    dc := Deg2Rad*(dc) ;
    de := Deg2Rad*(de) ;
    sincos(dc,s1,c1);
    sincos(de,s2,c2);
    sincos(hh,s3,c3);
    r:=s1*s2 + c1*c2*c3;
    if r>1 then r:=1;
    r:= arccos(r)  ;
    if r<>0 then r:= (r/sin(r));
    xx:= rad2deg*(r*c2*s3);
    yy:= rad2deg*(r*(s2*c1-c2*s1*c3));
    end;
'C' : begin                 // CAR
    hh:=rmod(ac+180,360);
    if ar>hh then ar:=ar-hh
             else ar:=ar+360-hh;
    ac:=rmod(hh-ac+360,360);
    xx:=ac-ar;
    yy:=de-dc;
    end;
'S' : begin                 // SIN
    hh := Deg2Rad*(ar-ac) ;
    dc := Deg2Rad*(dc) ;
    de := Deg2Rad*(de) ;
    sincos(dc,s1,c1);
    sincos(de,s2,c2);
    sincos(hh,s3,c3);
    r:=s1*s2+c2*c1*c3;  // cos the
    if r<=0 then begin  // > 90°
      xx:=9999;
      yy:=9999;
    end else begin
      xx:= -rad2deg*(c2*s3);
      yy:= rad2deg*(s2*c1-c2*s1*c3);
    end;
    if xx>10000 then xx:=10000
     else if xx<-10000 then xx:=-10000;
    if yy>10000 then yy:=10000
     else if yy<-10000 then yy:=-10000;
    end;
'T' : begin                  //  TAN
    hh := Deg2Rad*(ar-ac) ;
    dc := Deg2Rad*(dc) ;
    de := Deg2Rad*(de) ;
    sincos(dc,s1,c1);
    sincos(de,s2,c2);
    sincos(hh,s3,c3);
    r:=s1*s2+c2*c1*c3;     // cos the
    if r<=0 then begin  // > 90°
      xx:=9999;
      yy:=9999;
    end else begin
      xx := -rad2deg*( c2*s3/r );
      yy := rad2deg*( (s2*c1-c2*s1*c3)/r );
    end;
    if xx>10000 then xx:=10000
     else if xx<-10000 then xx:=-10000;
    if yy>10000 then yy:=10000
     else if yy<-10000 then yy:=-10000;
    end;
else begin
    projtype:='A';
    hh := DegToRad(ac-ar) ;
    dc := DegToRad(dc) ;
    de := DegToRad(de) ;
    s1:=sin(dc);
    s2:=sin(de);
    c1:=cos(dc);
    c2:=cos(de);
    c3:=cos(hh);
    r:= (arccos( s1*s2 + c1*c2*c3-1e-12 ))  ;
    r:= (r/sin(r));
    xx:= radtodeg(r*c2*sin(hh));
    yy:= radtodeg(r*(s2*c1-c2*s1*c3));
    end;
end;
X:=xx*costheta+yy*sintheta;
Y:=yy*costheta-xx*sintheta;
END ;

PROCEDURE Projection(ar,de : Double ; VAR X,Y : Double; clip:boolean );
Var a,h,ac,dc,d1,d2 : Double ;
    a1,a2,i1,i2 : integer;
BEGIN
case Projpole of
   1 : begin
       ar:=ar/15;
       precession(jd2000,currentjd,ar,de);
       ar:=ar*15;
       Eq2Hz(15*CurrentST-ar,de,a,h) ;
       ar:=-a;
       de:=h;
       ac:=-acentre;
       dc:=hcentre;
       end;
   else begin
       ac:=arcentre*15;
       dc:=decentre;
       end;
end;
if clip and (projpole=1) and horizonopaque and (h<=HorizonMax) then begin
  if h<0 then begin
       X:=10000;
       Y:=10000;
  end else begin
    a:=rmod(-ar+181+360,360);
    a1:=trunc(a);
    if a1=0 then i1:=360 else i1:=a1;
    a2:=a1+1;
    if a2=361 then i2:=1 else i2:=a2;
    d1:=horizonlist[i1];
    d2:=horizonlist[i2];
    h:=d1+(a-a1)*(d2-d1)/(a2-a1);
    if de<h then begin
       X:=10000;
       Y:=10000;
    end else Proj2(ar,de,ac,dc,X,Y);
  end;
end else Proj2(ar,de,ac,dc,X,Y);
END ;

Procedure InvProj2 (xx,yy,ac,dc : Double ; VAR ar,de : Double );
Var a,r,hh,s1,c1,s2,c2,s3,c3,x,y : Extended ;
Begin
x:=(xx*costheta-yy*sintheta) ;     // AIPS memo 27
y:=(-yy*costheta-xx*sintheta);
case projtype of
'A' : begin
    r :=Deg2Rad*(90-sqrt(x*x+y*y)) ;
    a := arctan2(x,y) ;
    dc:= Deg2Rad*(dc) ;
    sincos(a,s1,c1);
    sincos(dc,s2,c2);
    sincos(r,s3,c3);
    de:=Rad2Deg*(arcsin( s2*s3 - c2*c3*c1)) + 1E-7 ;
    hh:=Rad2Deg*(arctan2((c3*s1),(c2*s3+s2*c3*c1) ));
    ar := ac - hh - 1E-7 ;
   end;
'C' : begin
//    ar:=rmod(ac-x+360,360);
    ar:=ac-x;
    de:=dc-y;
    if de>0 then de:=minvalue([de,89.999]) else de:=maxvalue([de,-89.999]);
    end;
'S' : begin
    dc:=deg2rad*(dc);
    sincos(dc,s1,c1);
    x:=-deg2rad*(x);
    y:=-deg2rad*(y);
    r:=sqrt(1-x*x-y*y);
    ar:=ac+radtodeg(arctan2(x,(c1*r-y*s1)));
    de:=radtodeg(arcsin(y*c1+s1*r));
    end;
'T' : begin
    dc:=deg2rad*(dc);
    sincos(dc,s1,c1);
    x:=-deg2rad*(x);
    y:=-deg2rad*(y);
    ar:=ac+radtodeg(arctan2(x,(c1-y*s1)));
    de:=radtodeg(arctan((cos(degtorad(ar-ac))*(y*c1+s1))/(c1-y*s1)));
    end;
else begin
    //showmessage(commsg[2]+' '+projtype);
    projtype:='A';
    r :=DegToRad(90-sqrt(x*x+y*y)) ;
    a := arctan2(x,y) ;
    dc:= DegToRad(dc) ;
    de:=RadToDeg(arcsin( sin(dc)*sin(r) - cos(dc)*cos(r)*cos(a) )) + 1E-7 ;
    hh:=RadToDeg(arctan2((cos(r)*sin(a)),(cos(dc)*sin(r)+sin(dc)*cos(r)*cos(a)) ));
    ar := ac - hh - 1E-7 ;
    end;
end;
end ;

Procedure InvProj (xx,yy : Double ; VAR ar,de : Double );
Var a,hh,ac,dc : Double ;
Begin
case Projpole of
   1 : begin
       ac:=-acentre;
       dc:=hcentre;
       end;
   else begin
       ac:=arcentre*15;
       dc:=decentre;
       end;
end;
InvProj2 (xx,yy,ac,dc,ar,de);
case Projpole of
   1 : begin
       Hz2Eq(-ar,de,a,hh) ;
       ar:=15*CurrentST-a;
       de:=hh;
       ar:=ar/15;
       precession(currentjd,jd2000,ar,de);
       ar:=ar*15;
       end;
end;
end ;

procedure GetADxy(x,y:Integer ; var a,d : Double);
var
   x1,y1: Double;
begin
  XYwindow(x,y,x1,y1);
  InvProj (x1,y1,a,d);
  a:=a/15.0;
end;

procedure GetAHxy(x,y:Integer ; var a,h : Double);
var
   x1,y1: Double;
begin
  XYwindow(x,y,x1,y1);
  InvProj2 (x1,y1,-acentre,hcentre,a,h);
  a:=rmod(720-a,360);
end;

procedure GetAHxyF(x,y:Integer ; var a,h : Double);
var
   x1,y1: Double;
begin
  XYwindow(x,y,x1,y1);
  InvProj2 (x1,y1,-acentre,hcentre,a,h);
  a:=-a;
//  a:=rmod(720-a,360);
end;

function NorthPoleInMap : Boolean;
var a,d,x1,y1: Double; xx,yy : integer;
begin
a:=0 ; d:=90;
if projpole=1 then precession(CurrentJD,JD2000,a,d);
projection(a*15,d,x1,y1,false) ;
windowxy(x1,y1,xx,yy);
Result:=(xx>=xmin) and (xx<=xmax) and (yy>=ymin) and (yy<=ymax);
end;

function SouthPoleInMap : Boolean;
var a,d,x1,y1: Double; xx,yy : integer;
begin
a:=0 ; d:=-90;
if projpole=1 then precession(CurrentJD,JD2000,a,d);
projection(a*15,d,x1,y1,false) ;
windowxy(x1,y1,xx,yy);
Result:=(xx>=xmin) and (xx<=xmax) and (yy>=ymin) and (yy<=ymax);
end;

function ZenithInMap : Boolean;
var x1,y1: Double; xx,yy : integer;
begin
proj2(1.0,90.0,acentre,hcentre,x1,y1) ;
windowxy(x1,y1,xx,yy);
Result:=(xx>=xmin) and (xx<=xmax) and (yy>=ymin) and (yy<=ymax);
end;

function NadirInMap : Boolean;
var x1,y1: Double; xx,yy : integer;
begin
proj2(1.0,-90.0,acentre,hcentre,x1,y1) ;
windowxy(x1,y1,xx,yy);
Result:=(xx>=xmin) and (xx<=xmax) and (yy>=ymin) and (yy<=ymax);
end;

Function AngularDistance(ar1,de1,ar2,de2 : Double) : Double;
var s1,s2,c1,c2: extended;
begin
try
if (ar1=ar2) and (de1=de2) then result:=0.0
else begin
//    result:=RadToDeg(arccos((sin(DegToRad(de1))*sin(DegToRad(de2)))+(cos(DegToRad(de1))*cos(DegToRad(de2))*cos(DegToRad((ar1-ar2))))));
    sincos(Deg2Rad*(de1),s1,c1);
    sincos(Deg2Rad*(de2),s2,c2);
    result:=Rad2Deg*(arccos((s1*s2)+(c1*c2*cos(Deg2Rad*((ar1-ar2))))));
end;    
except
  result:=0;
end;
end;

function PositionAngle(ac,dc,ar,de:double):double;
var hh,s1,s2,s3,c1,c2,c3 : extended;
begin
    hh := Deg2Rad*(ac-ar) ;
    dc := Deg2Rad*(dc) ;
    de := Deg2Rad*(de) ;
    sincos(dc,s1,c1);
    sincos(de,s2,c2);
    sincos(hh,s3,c3);
    result:= 180+rad2deg*(arctan2((c2*s3) , (-c1*s2+s1*c2*c3) ));
end;

Function DEToStr(de: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
    dd:=Int(de);
    min1:=abs(de-dd)*60;
    if min1>=59.99 then begin
       dd:=dd+sgn(de);
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.5 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(abs(dd):2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    if de<0 then d:='-'+d else d:='+'+d;
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.5 then s:='0'+trim(s);
    result := d+ldeg+m+lmin+s+lsec;
end;

Function DEToStr3(de: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
    dd:=Int(de);
    min1:=abs(de-dd)*60;
    if min1>=59.99 then begin
       dd:=dd+sgn(de);
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.5 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(abs(dd):2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    if de<0 then d:='-'+d else d:='+'+d;
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.5 then s:='0'+trim(s);
    result := d+'d'+m+'m'+s+'s';
end;

Function DEToStr4(de: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
    dd:=Int(de);
    min1:=abs(de-dd)*60;
    if min1>=59.99 then begin
       dd:=dd+sgn(de);
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.5 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(abs(dd):2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    if de<0 then d:='-'+d else d:='+'+d;
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.5 then s:='0'+trim(s);
    result := d+'°'+m+''''+s+'"';
end;

Function TimToStr(de: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
    dd:=Int(de);
    min1:=abs(de-dd)*60;
    if min1>=59.99 then begin
       dd:=dd+sgn(de);
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.5 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(abs(dd):2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.5 then s:='0'+trim(s);
    result := d+':'+m+':'+s;
end;

Function ARToStr(ar: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
    dd:=Int(ar);
    min1:=abs(ar-dd)*60;
    if min1>=59.999 then begin
       dd:=dd+sgn(ar);
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.95 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(dd:3:0,d);
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:4:1,s);
    if abs(sec)<9.95 then s:='0'+trim(s);
    result := d+'h'+m+'m'+s+'s';
end;

Function DEpToStr(de: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
    dd:=Int(de);
    min1:=abs(de-dd)*60;
    if min1>=59.99 then begin
       dd:=dd+sgn(de);
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.995 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(abs(dd):2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    if de<0 then d:='-'+d else d:='+'+d;
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:4:1,s);
    if abs(sec)<9.95 then s:='0'+trim(s);
    result := d+ldeg+m+lmin+s+lsec;
end;

Function ARpToStr(ar: Double) : string;
var dd,min1,min,sec: Double;
    d,m,s : string;
begin
    dd:=Int(ar);
    min1:=abs(ar-dd)*60;
    if min1>=59.999 then begin
       dd:=dd+sgn(ar);
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.995 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(dd:3:0,d);
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:5:2,s);
    if abs(sec)<9.995 then s:='0'+trim(s);
   result := d+'h'+m+'m'+s+'s';
end;

Function DEmToStr(de: Double) : string;
var dd,min: Double;
    d,m : string;
begin
    dd:=Int(de);
    min:=abs(de-dd)*60;
    if min>=59.5 then begin
       dd:=dd+sgn(de);
       min:=0.0;
    end;
    min:=Round(min);
    str(abs(dd):2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    if de<0 then d:='-'+d else d:='+'+d;
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    result := d+ldeg+m+lmin;
end;

Function DEdToStr(de: Double) : string;
var dd: Double;
    d : string;
begin
    dd:=round(de);
    str(abs(dd):2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    if de<0 then d:='-'+d else d:='+'+d;
    result := d+ldeg;
end;

Function ARmToStr(ar: Double) : string;
var dd,min: Double;
    d,m: string;
begin
    dd:=Int(ar);
    min:=abs(ar-dd)*60;
    if min>=59.5 then begin
       dd:=dd+sgn(ar);
       min:=0.0;
    end;
    min:=Round(min);
    str(dd:3:0,d);
    str(min:2:0,m);
    if abs(min)<9.5 then m:='0'+trim(m);
    result := d+'h'+m+'m';
end;

Function DEToStr2(de: Double; var d,m,s : string) : string;
var dd,min1,min,sec: Double;
begin
    dd:=Int(de);
    min1:=abs(de-dd)*60;
    if min1>=59.99 then begin
       dd:=dd+sgn(de);
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.5 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(abs(dd):2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    if de<0 then d:='-'+d else d:='+'+d;
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.5 then s:='0'+trim(s);
    result := d+ldeg+m+lmin+s+lsec;
end;

Function ARToStr2(ar: Double; var d,m,s : string) : string;
var dd,min1,min,sec: Double;
begin
    dd:=Int(ar);
    min1:=abs(ar-dd)*60;
    if min1>=59.999 then begin
       dd:=dd+sgn(ar);
       min1:=0.0;
    end;
    min:=Int(min1);
    sec:=(min1-min)*60;
    if sec>=59.95 then begin
       min:=min+1;
       sec:=0.0;
    end;
    str(dd:2:0,d);
    if abs(dd)<10 then d:='0'+trim(d);
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    str(sec:2:0,s);
    if abs(sec)<9.95 then s:='0'+trim(s);
    result := d+'h'+m+'m'+s+'s';
end;

function Jd(annee,mois,jour :INTEGER; Heure:double):double;
var u,u0,u1,u2 : double;
	gregorian : boolean;
begin
if annee*10000+mois*100+jour >= 15821015 then gregorian:=true else gregorian:=false;
u:=annee;
if mois<3 then u:=u-1;
u0:=u+4712;
u1:=mois+1;
if u1<4 then u1:=u1+12;
result:=floor(u0*365.25)+floor(30.6*u1+0.000001)+jour+heure/24-63.5;
if gregorian then begin
   u2:=floor(abs(u)/100)-floor(abs(u)/400);
   if u<0 then u2:=-u2;
   result:=result-u2+2;
   if (u<0)and((u/100)=floor(u/100))and((u/400)<>floor(u/400)) then result:=result-1;
end;
end;

PROCEDURE Djd(jd:Double;VAR annee,mois,jour:INTEGER; VAR Heure:double);
var u0,u1,u2,u3,u4 : double;
	gregorian : boolean;
begin
u0:=jd+0.5;
if int(u0)>=2299161 then gregorian:=true else gregorian:=false;
u0:=jd+32082.5;
if gregorian then begin
   u1:=u0+floor(u0/36525)-floor(u0/146100)-38;
   if jd>=1830691.5 then u1:=u1+1;
   u0:=u0+floor(u1/36525)-floor(u1/146100)-38;
end;
u2:=floor(u0+123);
u3:=floor((u2-122.2)/365.25);
u4:=floor((u2-floor(365.25*u3))/30.6001);
mois:=round(u4-1);
if mois>12 then mois:=mois-12;
jour:=round(u2-floor(365.25*u3)-floor(30.6001*u4));
annee:=round(u3+floor((u4-2)/12)-4800);
heure:=(jd-floor(jd+0.5)+0.5)*24;
end;

{function Jd(annee,mois,jour :INTEGER; Heure:double):double;
 VAR siecle,cor:INTEGER ;
 begin
    if mois<=2 then begin
      annee:=annee-1;
      mois:=mois+12;
    end ;
    if annee*10000+mois*100+jour >= 15821015 then begin
       siecle:=annee div 100;
       cor:=2 - siecle + siecle div 4;
    end else cor:=0;
    jd:=int(365.25*(annee+4716))+int(30.6001*(mois+1))+jour+cor-1524.5 + heure/24;
 END ;

PROCEDURE Djd(jd:Double;VAR annee,mois,jour:INTEGER; VAR Heure:double);
 VAR z,f,a,al,b,c,d,e:double;
     n : integer;
 BEGIN
    a:=jd+0.5;
    z := int(a);
    f := frac(a);
    if z<2299161.0 then a := z
       else begin
            al:= Int((z-1867216.25)/36524.25);
            a := z + 1.0 + al - Int(al/4.0);
    end;
    b := a + 1524.0;
    c := Int((b-122.1)/365.25);
    d := Int(365.25*c);
    e := Int((b-d)/30.6001);
    jour := Trunc(b-d-Int(30.6001*e)+f);
    IF e<13.5 THEN
        mois:=Trunc(e)-1
      ELSE
        mois:=Trunc(e)-13;
    IF mois>2 THEN
        annee:=Trunc(c)-4716
      ELSE
        annee:=Trunc(c)-4715;
    IF (annee MOD 4 <> 0) AND (mois=2) AND (jour=29) THEN
    begin
            mois:=3;
            jour:=1;
    END;
    Heure:=24.0*f ;
//    if jd<0 then annee:=annee-n;
 END ;}

 function SidTim(jd0,ut,long : double): double;
 VAR t,te: double;
 BEGIN
{    t:= (jd0-2415020.0)/36525.0;
    te:= 6.6460656 + 2400.051262*t + 2.581E-5*t*t ;}
    t:=(jd0-2451545.0)/36525;
    te:=100.46061837 + 36000.770053608*t + 0.000387933*t*t - t*t*t/38710000;
    result :=  Rmod(te/15 - long/15 + 1.002737908*ut,24) ;
//    result := rmod(result+24,24);
 END ;

procedure Paralaxe(SideralTime,dist,ar1,de1 : double; var ar,de,q : double);
var
   sinpi,dar,H,rde,a,b,c : double;
const
     desinpi = 4.26345151e-5;
begin
// AR, DE are J2000 but paralaxe is to be computed with coordinates of the date.
precession(jd2000,currentjd,ar1,de1);
H:=degtorad(15*(SideralTime-ar1));
rde:=degtorad(de1);
sinpi:=desinpi/dist;
dar:=arctan2(-ObsRoCosPhi*sinpi*sin(H),(cos(rde)-ObsRoCosPhi*sinpi*cos(H)));
ar :=ar1+radtodeg(dar)/15;
de :=arctan2((sin(rde)-ObsRoSinPhi*sinpi)*cos(dar),cos(rde)-ObsRoCosPhi*sinpi*cos(H));
de :=radtodeg(de);
a := cos(rde)*sin(H);
b := cos(rde)*cos(H)-ObsRoSinPhi*sinpi;
c := sin(rde)-ObsRoSinPhi*sinpi;
q := sqrt(a*a+b*b+c*c);
precession(currentjd,jd2000,ar,de);
end;

function DTminusUT(annee : integer) : double;
var t : double;
begin
if Force_DT_UT then result:=DT_UT_val
else begin
case annee of
{ Atlas of Historical Eclipse Maps East Asia 1500 BC - AD 1900, Stephenson and Houlden (1986)
     (1) prior to 948 AD
         delta-T (seconds) = 1830 - 405*t + 46.5*t^2
             (t = centuries since 948 AD)

     (2) 948 AD to 1600 AD
         delta-T (seconds) = 22.5*t^2
             (t = centuries since 1850 AD)
}
-99999..948 : begin
              t:=(annee-2000)/100;
              result:=(2715.6 + 573.36 * t + 46.5 * t*t) / 3600;
//              t:=(annee-948)/100;
//              result:=(1830-405*t+46.5*t*t)/3600;
              end;
  949..1619 : begin
              t:=(annee-1850)/100;
              result:=(22.5*t*t)/3600;
              end;
  1620..1621 : result:=124/3600;
  1622..1623 : result:=115/3600;
  1624..1625 : result:=106/3600;
  1626..1627 : result:= 98/3600;
  1628..1629 : result:= 91/3600;
  1630..1631 : result:= 85/3600;
  1632..1633 : result:= 79/3600;
  1634..1635 : result:= 74/3600;
  1636..1637 : result:= 70/3600;
  1638..1639 : result:= 65/3600;
  1640..1645 : result:= 60/3600;
  1646..1653 : result:= 50/3600;
  1654..1661 : result:= 40/3600;
  1662..1671 : result:= 30/3600;
  1672..1681 : result:= 20/3600;
  1682..1691 : result:= 10/3600;
  1692..1707 : result:=  9/3600;
  1708..1717 : result:= 10/3600;
  1718..1733 : result:= 11/3600;
  1734..1743 : result:= 12/3600;
  1744..1751 : result:= 13/3600;
  1752..1757 : result:= 14/3600;
  1758..1765 : result:= 15/3600;
  1766..1775 : result:= 16/3600;
  1776..1791 : result:= 17/3600;
  1792..1795 : result:= 16/3600;
  1796..1797 : result:= 15/3600;
  1798..1799 : result:= 14/3600;
 1800..1899 : begin
              t:=(annee-1900)/100;
              result:=(-1.4e-5+t*(1.148e-3+t*(3.357e-3+t*(-1.2462e-2+t*(-2.2542e-2+t*(6.2971e-2+t*(7.9441e-2+t*(-0.146960+t*(-0.149279+t*(0.161416+t*(0.145932+t*(-6.7471e-2+t*(-5.8091e-2))))))))))))  )*24;
              end;
 1900..1987 : begin
              t:=(annee-1900)/100;
              result:=(-2e-5+t*(2.97e-4+t*(2.5184e-2+t*(-0.181133+t*(0.553040+t*(-0.861938+t*(0.677066+t*(-0.212591))))))))*24;
              end;
 1988..1996 : begin
              t:=(annee-2000)/100;
              result:=(67+123.5*t+32.5*t*t)/3600;
              end;
       1997 : result:=62/3600;
       1998 : result:=63/3600;
       1999 : result:=63/3600;
       2000 : result:=64/3600;
       2001 : result:=64/3600;
 2002..2020 : begin
              t:=(annee-2000)/100;
              result:=(63+123.5*t+32.5*t*t)/3600;
              end;
 2021..99999 : begin
              t:=(annee-1875.1)/100;
              result:=45.39*t*t/3600;
              end;
 else result:=0;
 end;
end;
end;

PROCEDURE PrecessionFK4(ti,tf : double; VAR ari,dei : double);
var i1,i2,i3,i4,i5,i6,i7 : double ;
   BEGIN
      ari:=ari*15;
      I1:=(TI-2415020.3135)/36524.2199 ;
      I2:=(TF-TI)/36524.2199 ;
      I3:=((1.8E-2*I2+3.02E-1)*I2+(2304.25+1.396*I1))*I2/3600.0 ;
      I4:=I2*I2*(7.91E-1+I2/1000.0)/3600.0+I3 ;
      I5:=((2004.682-8.35E-1*I1)-(4.2E-2*I2+4.26E-1)*I2)*I2/3600.0 ;
      I6:=COS(degtorad(DEI))*SIN(degtorad(ARI+I3)) ;
      I7:=COS(degtorad(I5))*COS(degtorad(DEI))*COS(degtorad(ARI+I3))-SIN(degtorad(I5))*SIN(degtorad(DEI)) ;
      DEI:=radtodeg(ArcSIN(SIN(degtorad(I5))*COS(degtorad(DEI))*COS(degtorad(ARI+I3))+COS(degtorad(I5))*SIN(degtorad(DEI)))) ;
      ARI:=radtodeg(ARCTAN2(I6,I7)) ;
      ARI:=ARI+I4   ;
      ARI:=RMOD(ARI+360.0,360.0)/15;
   END  ;

PROCEDURE Precession(ti,tf : double; VAR ari,dei : double);  // ICRS
var i1,i2,i3,i4,i5,i6,i7 : double ;
   BEGIN
      ari:=ari*15;
      I1:=(TI-2451545.0)/36525 ;
      I2:=(TF-TI)/36525;
      I3:=((2306.2181+1.39656*i1-1.39e-4*i1*i1)*i2+(0.30188-3.44e-4*i1)*i2*i2+1.7998e-2*i2*i2*i2)/3600 ;
      I4:=((2306.2181+1.39656*i1-1.39e-4*i1*i1)*i2+(1.09468+6.6e-5*i1)*i2*i2+1.8203e-2*i2*i2*i2)/3600 ;
      I5:=((2004.3109-0.85330*i1-2.17e-4*i1*i1)*i2-(0.42665+2.17e-4*i1)*i2*i2-4.1833e-2*i2*i2*i2)/3600 ;
      I6:=COS(degtorad(DEI))*SIN(degtorad(ARI+I3)) ;
      I7:=COS(degtorad(I5))*COS(degtorad(DEI))*COS(degtorad(ARI+I3))-SIN(degtorad(I5))*SIN(degtorad(DEI)) ;
      i1:=(SIN(degtorad(I5))*COS(degtorad(DEI))*COS(degtorad(ARI+I3))+COS(degtorad(I5))*SIN(degtorad(DEI)));
      if i1>1 then i1:=1;
      if i1<-1 then i1:=-1;
      DEI:=radtodeg(ArcSIN(i1)) ;
      ARI:=radtodeg(ARCTAN2(I6,I7)) ;
      ARI:=ARI+I4   ;
      ARI:=RMOD(ARI+360.0,360.0)/15;
   END  ;

procedure PrecessionEcl(ti,tf : double; VAR l,b : double);  // l,b en radian !
var i1,i2,i3,i4,i5,i6,i7,i8 : double ;
begin
i1:=(ti-2451545.0)/36525 ;
i2:=(tf-ti)/36525;
i3:=degtorad(((47.0029-0.06603*i1+0.000598*i1*i1)*i2+(-0.03302+0.000598*i1)*i2*i2+0.000060*i2*i2*i2)/3600);
i4:=degtorad((174.876384*3600+3289.4789*i1+0.60622*i1*i1-(869.8089+0.50491*i1)*i2+0.03536*i2*i2)/3600);
i5:=degtorad(((5029.0966+2.22226*i1-0.000042*i1*i1)*i2+(1.11113-0.000042*i1)*i2*i2-0.000006*i2*i2*i2)/3600);
i6:=cos(i3)*cos(b)*sin(i4-l)-sin(i3)*sin(b);
i7:=cos(b)*cos(i4-l);
i8:=cos(i3)*sin(b)+sin(i3)*cos(b)*sin(i4-l);
l:=i5+i4-arctan2(i6,i7);
b:=arcsin(i8);
l:=rmod(l+2*pi,2*pi);
end;

function words(str,sep : string; p,n : integer) : string;
var     i,j : Integer;
begin
result:='';
str:=trim(str);
for i:=1 to p-1 do begin
 j:=pos(' ',str);
 if j=0 then j:=length(str)+1;
 str:=trim(copy(str,j,length(str)));
end;
for i:=1 to n do begin
 j:=pos(' ',str);
 if j=0 then j:=length(str)+1;
 result:=result+trim(copy(str,1,j))+sep;
 str:=trim(copy(str,j,length(str)));
end;
end;

Procedure FormPos(form : Tform; x,y : integer);
var htaskbar : thandle;
    bot : integer;
    tbrect: Trect;
begin
bot:=0;
htaskbar:=FindWindow('Shell_TrayWnd', Nil );
if htaskbar<>null then begin
  GetWindowRect(htaskbar,tbrect);
  if (tbrect.top>0) then bot:=abs(tbrect.Bottom-tbrect.Top);
end;
with Form do begin
  left:=x;
  if left+width>Screen.Width then left:=Screen.Width-width;
  if left<0 then left:=0;
  top:=y;
  if top+height>(Screen.height-bot) then top:=Screen.height-height-bot;
  if top<0 then top:=0;
end;
end;

function InvertI16(X : word) : smallInt;
var  P : PbyteArray;
     temp : word;
begin
    P:=@X;
    temp:=(P[0] shl 8) or (P[1]);
    move(temp,result,2);
end;

function InvertI32(X : LongWord) : LongInt;
var  P : PbyteArray;
begin
    P:=@X;
    result:=(P[0] shl 24) or (P[1] shl 16) or (P[2] shl 8) or (P[3]);
end;

function InvertI64(X : Int64) : Int64;
var  P : PbyteArray;
begin
    P:=@X;
    result:=4294967296 * ((P[0] shl 24) or (P[1] shl 16) or (P[2] shl 8) or (P[3])) + ((P[4] shl 24) or (P[5] shl 16) or (P[6] shl 8) or (P[7]));
end;

function InvertF32(X : LongWord) : Single;
var  P : PbyteArray;
     temp : LongWord;
begin
    P:=@X;
    if (P[0]=$7F)or(P[0]=$FF) then result:=0   // IEEE-754 NaN
    else begin
    temp:=(P[0] shl 24) or (P[1] shl 16) or (P[2] shl 8) or (P[3]);
    move(temp,result,4);
    end;
end;

function InvertF64(X : Int64) : Double;
var  P : PbyteArray;
     temp : Int64;
begin
    P:=@X;
    if (P[0]=$7F)or(P[0]=$FF) then result:=0   // IEEE-754 NaN
    else begin
    temp:=4294967296 * ((P[0] shl 24) or (P[1] shl 16) or (P[2] shl 8) or (P[3])) + ((P[4] shl 24) or (P[5] shl 16) or (P[6] shl 8) or (P[7]));
    move(temp,result,8);
    end;
end;

PROCEDURE Horizontal(HH,DE : double ; VAR A,h : double );
var l1,d1,h1 : double;
BEGIN
l1:=degtorad(ObsLatitude);
d1:=degtorad(DE);
h1:=degtorad(HH);
h:= radtodeg(arcsin( sin(l1)*sin(d1)+cos(l1)*cos(d1)*cos(h1) ))  ;
//A:= radtodeg(arctan2((cos(d1)*sin(h1)),(-cos(l1)*sin(d1)+sin(l1)*cos(d1)*cos(h1)) )) ;
A:= radtodeg(arctan2(sin(h1),cos(h1)*sin(l1)-tan(d1)*cos(l1)));
A:=Rmod(A+360,360);                                    
{ refraction meeus91 15.4 }
if h>-1 then h:=minvalue([90,h+ObsRefractionCor*(1.02/tan(degtorad(h+10.3/(h+5.11))))/60])
        else h:=h+0.64658062088;
END ;

PROCEDURE HorizontalGeometric(HH,DE : double ; VAR A,h : double );
var l1,d1,h1 : double;
BEGIN
l1:=degtorad(ObsLatitude);
d1:=degtorad(DE);
h1:=degtorad(HH);
h:= radtodeg(arcsin( sin(l1)*sin(d1)+cos(l1)*cos(d1)*cos(h1) ))  ;
//A:= radtodeg(arctan2((cos(d1)*sin(h1)),(-cos(l1)*sin(d1)+sin(l1)*cos(d1)*cos(h1)) )) ;
A:= radtodeg(arctan2(sin(h1),cos(h1)*sin(l1)-tan(d1)*cos(l1)));
A:=Rmod(A+360,360);
END ;

PROCEDURE Eq2Hz(HH,DE : double ; VAR A,h : double );
var l1,d1,h1 : double;
BEGIN
l1:=degtorad(ObsLatitude);
d1:=degtorad(DE);
h1:=degtorad(HH);
h:= radtodeg(arcsin( sin(l1)*sin(d1)+cos(l1)*cos(d1)*cos(h1) ))  ;
A:= radtodeg(arctan2(sin(h1),cos(h1)*sin(l1)-tan(d1)*cos(l1)));
A:=Rmod(A+360,360);
{ refraction meeus91 15.4 }
if h>-1 then h:=minvalue([90,h+ObsRefractionCor*(1.02/tan(degtorad(h+10.3/(h+5.11))))/60])
        else h:=h+0.64658062088;
END ;

Procedure Hz2Eq(A,h : double; var hh,de : double);
var l1,a1,h1 : double;
BEGIN
l1:=degtorad(ObsLatitude);
a1:=degtorad(A);
{ refraction meeus91 15.3 }
if h>-0.3534193791 then h:=minvalue([90,h-ObsRefractionCor*(1/tan(degtorad(h+(7.31/(h+4.4)))))/60])
                else h:=h-0.64658062088;
h1:=degtorad(h);
de:= radtodeg(arcsin( sin(l1)*sin(h1)-cos(l1)*cos(h1)*cos(a1) ))  ;
hh:= radtodeg(arctan2(sin(a1),cos(a1)*sin(l1)+tan(h1)*cos(l1)));
hh:=Rmod(hh+360,360);
END ;

Procedure Ecl2Eq(l,b,e: double; var ar,de : double);
begin
l:=degtorad(l);
b:=degtorad(b);
e:=degtorad(e);
ar:=radtodeg(arctan2(sin(l)*cos(e)-tan(b)*sin(e),cos(l)))/15;
de:=radtodeg(arcsin(sin(b)*cos(e)+cos(b)*sin(e)*sin(l)));
end;

Procedure Eq2Ecl(ar,de,e: double; var l,b: double);
begin
ar:=degtorad(ar*15);
de:=degtorad(de);
e:=degtorad(e);
l:=radtodeg(arctan2(sin(ar)*cos(e)+tan(de)*sin(e),cos(ar)));
b:=radtodeg(arcsin(sin(de)*cos(e)-cos(de)*sin(e)*sin(ar)));
end;

Procedure Gal2Eq(l,b: double; var ar,de : double);
var dp : double;
begin
l:=degtorad(l-123);
b:=degtorad(b);
dp:=degtorad(27.4);
ar:=12.25+radtodeg(arctan2(sin(l),cos(l)*sin(dp)-tan(b)*cos(dp)));
de:=radtodeg(arcsin(sin(b)*sin(dp)+cos(b)*cos(dp)*cos(l)));
ar:=ar/15;
precession(jd1950,jd2000,ar,de);
end;

procedure RiseSet(typobj:integer; jd0,ar,de:double; var hr,ht,hs,azr,azs:double;var irc:integer);
const ho : array[1..3] of Double = (-9.89E-3,-1.454E-2,2.18E-3) ;
var hs0,chh0,hh0,m0,m1,m2,a0 : double;
begin
precession(2451545.0,jd0,ar,de); // J2000 coord. to date
hs0 := sidtim(jd0,0.0,0.0)*15 ;
chh0 :=(ho[typobj]-sin(degtorad(ObsLatitude))*sin(degtorad(de)))/(cos(degtorad(ObsLatitude))*cos(degtorad(de))) ;
if abs(chh0)<=1 then begin
   hh0:=radtodeg(arccos(chh0));
   m0:=(ar*15+ObsLongitude-hs0)/360;
   m1:=m0-hh0/360;
   m2:=m0+hh0/360;
   ht:=rmod(rmod(m0+1,1)*24+TimeBias+24,24);
   hr:=rmod(rmod(m1+1,1)*24+TimeBias+24,24);
   hs:=rmod(rmod(m2+1,1)*24+TimeBias+24,24);
   a0:= radtodeg(arctan2(sin(degtorad(hh0)),cos(degtorad(hh0))*sin(degtorad(Obslatitude))-tan(degtorad(de))*cos(degtorad(Obslatitude))));
   azr:=360-a0;
   azs:=a0;
   irc:=0;
end else begin
   hr:=0;hs:=0;azr:=0;azs:=0;
   if sgn(de)=sgn(ObsLatitude) then begin
      m0:=(ar*15+ObsLongitude-hs0)/360;     (* circumpolaire *)
      ht:=rmod(rmod(m0+1,1)*24+TimeBias+24,24);
      irc:=1 ;
    end else begin
      ht:=0;      (* invisible *)
      irc:=2;
    end;
end;
end;

Function int3(n,y1,y2,y3 : double): double;
var a,b,c : double;
begin
a:= y2 - y1;
b:= y3 - y2;
c:= b - a;
result:= y2 + n/2*(a + b + n*c);
end;

procedure RiseSetInt(typobj:integer; jd0,ar1,de1,ar2,de2,ar3,de3:double; var hr,ht,hs,azr,azs:double;var irc:integer);
const ho : array[1..3] of Double = (-0.5667,-0.8333,0.125) ;
var hs0,chh0,hh0,m0,m1,m2,a0,n,hsg,aa,dd,hl,h,dm,longref : double;
begin
precession(2451545.0,jd0,ar1,de1); // J2000 coord. to date
precession(2451545.0,jd0,ar2,de2);
precession(2451545.0,jd0,ar3,de3);
if ar1>12 then begin
   if ar2<12 then ar2:=ar2+24;
   if ar3<12 then ar3:=ar3+24;
end;
longref:=-timebias*15;
//hs0 := sidtim(jd0,0.0,0.0)*15 ;
hs0 := sidtim(jd0,-timebias,longref)*15 ;
chh0 :=(sin(degtorad(ho[typobj]))-sin(degtorad(ObsLatitude))*sin(degtorad(de2)))/(cos(degtorad(ObsLatitude))*cos(degtorad(de2))) ;
if abs(chh0)<=1 then begin
   hh0:=radtodeg(arccos(chh0));
//   m0:=(ar2*15+ObsLongitude-hs0)/360;
   m0:=(ar2*15+Obslongitude-longref-hs0)/360;
   m1:=m0-hh0/360;
   m2:=m0+hh0/360;
  if m0<0 then m0:=m0+1;
  if m0>1 then m0:=m0-1;
  if m1<0 then m1:=m1+1;
  if m1>1 then m1:=m1-1;
  if m2<0 then m2:=m2+1;
  if m2>1 then m2:=m2-1;
   // lever
   hsg:= hs0 + 360.985647 * m1;
   n:= m1 ;
   aa:=int3(n,ar1,ar2,ar3)*15;
   dd:=int3(n,de1,de2,de3);
//   hl:= hsg - ObsLongitude - aa;
   hl:= hsg - Obslongitude + longref - aa;
   h:= radtodeg(arcsin(sin(degtorad(Obslatitude)) * sin(degtorad(dd)) + cos(degtorad(Obslatitude)) * cos(degtorad(dd)) * cos(degtorad(hl)) ));
   dm:= (h - ho[typobj]) / (360 * cos(degtorad(dd)) * cos(degtorad(Obslatitude)) * sin(degtorad(hl)) );
//   hr:=rmod((m1+dm)*24+TimeBias+24,24);
//   hr:=(m1+dm)*24+timebias;
   hr:=(m1+dm)*24;
   a0:= radtodeg(arctan2(sin(degtorad(hh0)),cos(degtorad(hh0))*sin(degtorad(Obslatitude))-tan(degtorad(dd))*cos(degtorad(Obslatitude))));
   azr:=360-a0;
   // culmination
   hsg:= hs0 + 360.985647 * m0;
   n:= m0 ;
   aa:=int3(n,ar1,ar2,ar3)*15;
//   hl:= hsg - ObsLongitude - aa;
   hl:= hsg - Obslongitude + longref - aa;
   dm:= -(hl / 360);
//   ht:=rmod((m0+dm)*24+TimeBias+24,24);
//   ht:=(m0+dm)*24+timebias;
   ht:=rmod((m0+dm)*24+24,24);
   if (ht<10)and(m0>0.6) then ht:=ht+24;
   if (ht>14)and(m0<0.4) then ht:=ht-24;
   // coucher
   hsg:= hs0 + 360.985647 * m2;
   n:= m2 ;
   aa:=int3(n,ar1,ar2,ar3)*15;
   dd:=int3(n,de1,de2,de3);
//   hl:= hsg - ObsLongitude - aa;
   hl:= hsg - Obslongitude + longref - aa;
   h:= radtodeg(arcsin(sin(degtorad(Obslatitude)) * sin(degtorad(dd)) + cos(degtorad(Obslatitude)) * cos(degtorad(dd)) * cos(degtorad(hl)) ));
   dm:= (h - ho[typobj]) / (360 * cos(degtorad(dd)) * cos(degtorad(Obslatitude)) * sin(degtorad(hl)) );
//   hs:=rmod((m2+dm)*24+TimeBias+24,24);
//   hs:=(m2+dm)*24+timebias;
   hs:=(m2+dm)*24;
   a0:= radtodeg(arctan2(sin(degtorad(hh0)),cos(degtorad(hh0))*sin(degtorad(Obslatitude))-tan(degtorad(dd))*cos(degtorad(Obslatitude))));
   azs:=a0;
   irc:=0;
end else begin
   hr:=0;hs:=0;azr:=0;azs:=0;
   if sgn(de1)=sgn(ObsLatitude) then begin
      m0:=(ar2*15+ObsLongitude-hs0)/360;     (* circumpolaire *)
      hsg:= hs0 + 360.985647 * m0;
      n:= m0 + dt_ut / 24;
      aa:=int3(n,ar1,ar2,ar3)*15;
      hl:= hsg - ObsLongitude - aa;
      dm:= -(hl / 360);
      ht:=rmod((m0+dm)*24+TimeBias+24,24);
      irc:=1 ;
    end else begin
      ht:=0;      (* invisible *)
      irc:=2;
    end;
end;
end;

Procedure HeurePo(jd,ar,de,h :Double; VAR hp1,hp2 :Double );
VAR hh,st,st0 : Double ;
BEGIN
hh := (sin(degtorad(h))-sin(degtorad(ObsLatitude))*sin(degtorad(de)))/(cos(degtorad(ObsLatitude))*cos(degtorad(de))) ;
if abs(hh)<=1 then begin
     hh := radtodeg(arccos(hh)) ;
     st0 := sidtim(jd,0.0,ObsLongitude) ;
     st := (ar-hh)/15 ;
     hp1 := rmod((st-st0)/1.002737908+24,24) ;
     st := (ar+hh)/15 ;
     hp2 := rmod((st-st0)/1.002737908+24,24) ;
end else begin
     hp1:=-99;
     hp2:=-99;
end;
END;

Function Exec(cmd: string; hide: boolean): cardinal;
var
   bchExec: array[0..MAX_PATH] of char;
   pchEXEC: Pchar;
   si: TStartupInfo;
   pi: TProcessInformation;
begin
   pchExec := @bchExec;
   StrPCopy(pchExec,cmd);
   Result := 0;
   FillChar(si,sizeof(si),0);
   FillChar(pi,sizeof(pi),0);
   si.dwFlags:=STARTF_USESHOWWINDOW;
   if hide then si.wShowWindow:=SW_SHOWMINIMIZED
           else si.wShowWindow:=SW_SHOWNORMAL;
   si.cb := sizeof(si);
   try
      if CreateProcess(Nil,pchExec,Nil,Nil,false,CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                       Nil,Nil,si,pi) = True then
         begin
           Result := 0;
           WaitForSingleObject(pi.hProcess,INFINITE);
           GetExitCodeProcess(pi.hProcess,Result);
         end
         else
           Result := GetLastError;
    except;
       Result := GetLastError;
    end;
end;

Function ExecNoWait(cmd: string; hide: boolean): boolean;
var
   bchExec: array[0..MAX_PATH] of char;
   pchEXEC: Pchar;
   si: TStartupInfo;
   pi: TProcessInformation;
begin
   pchExec := @bchExec;
   StrPCopy(pchExec,cmd);
   FillChar(si,sizeof(si),0);
   FillChar(pi,sizeof(pi),0);
   si.dwFlags:=STARTF_USESHOWWINDOW;
   if hide then si.wShowWindow:=SW_SHOWMINIMIZED
           else si.wShowWindow:=SW_SHOWNORMAL;
   si.cb := sizeof(si);
   try
      result:= CreateProcess(Nil,pchExec,Nil,Nil,false,CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                       Nil,Nil,si,pi);
    except;
       Result := false;
    end;
end;

Function ExecuteFile(const FileName, Params, DefaultDir: string; ShowCmd: Integer): THandle;
var
  zFileName, zParams, zDir: array[0..79] of Char;
begin
  Result := ShellExecute(Application.MainForm.Handle, nil, StrPCopy(zFileName, FileName),
                         StrPCopy(zParams, Params), StrPCopy(zDir, DefaultDir), ShowCmd);
end;

Procedure ShowHelp(helpfile : string);
var p: integer;
begin
if InternalBrowser then begin
  if not helpcreated then begin
    if debugon then writedebug('Create ThelpForm');
    Application.CreateForm(ThelpForm, helpForm);
    helpcreated:=true;
  end;
  helpform.caption:=Application.title;
  helpunit.docfile:=hp+helpfile;
  helpshow;
end
else begin
 p:=pos('#',helpfile);
 if p>0 then begin
    helpfile:=copy(helpfile,1,p-1);
 end;
// HlinkNavigateString(Nil,pwidechar('file://'+appdir+'\doc\'+hp+helpfile));
 ExecuteFile(hp+helpfile,'',appdir+'\doc',SW_SHOWNORMAL);
end;
end;

Function YearADBC(year : integer) : string;
begin
if year>0 then begin
   result:=inttostr(year);
end else begin
   result:=inttostr(-year+1)+'BC' ;
end;
end;

end.

