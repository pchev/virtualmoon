unit cu_planet;
{
Copyright (C) 2002 Patrick Chevalley

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
 Planet computation component.
}
{$mode objfpc}{$H+}
interface

uses
  uDE,          // JPL ephemeris
  u_constant, u_util, u_projection,
  Classes, Sysutils, Math;

type
  TPlanet = class(TComponent)
  private
    { Private declarations }
    LockPla : boolean;
    de_folder: PChar;
    de_type:integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
     constructor Create(AOwner:TComponent); override;
     destructor  Destroy; override;
     Procedure SunEcl(t0 : double ; var l,b : double);
     function Moon(t0 : double; var alpha,delta,dist,dkm,diam,phase,illum : double; highprec:boolean=true):string;
     Procedure MoonIncl(Lar,Lde,Sar,Sde : double; var incl : double);
     Function MoonMag(phase:double):double;
     Function MoonOrientation(jde,ra,dec,d:double; var P,llat,llong,lats,longs : double):boolean;
     Function MoonPhase(k: double):double;
     Procedure MoonPhases(year:double; var nm,fq,fm,lq : double);
     procedure PlanetRiseSet(pla:integer; jd0:double; AzNorth:boolean; var thr,tht,ths,tazr,tazs: string; var jdr,jdt,jds,rar,der,rat,det,ras,des:double ; var i: integer);
     procedure PlanetAltitude(pla: integer; jd0,hh:double; var har,sina: double);
     procedure SetDE(folder:string);
     function load_de(t: double): boolean;
     procedure nutation(j:double; var nutl,nuto:double);
  end;

implementation

constructor TPlanet.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 lockpla:=false;
 de_folder:='';
end;

destructor TPlanet.Destroy;
begin
try
 if de_created then close_de_file;
 inherited destroy;
except
end;
end;

function to360(x:double):double;
begin
result:=rmod(x+3600000000,360);
end;


Function TPlanet.MoonOrientation(jde,ra,dec,d:double; var P,llat,llong,lats,longs : double):boolean;
var lp,l,b,f,om,w,T,a,aa,i,lh,bh,e,v,x,y,l0 {,cel,sel,asol,dsol} : double;
    pl :TPlanetData;
begin
T := (jde-2451545)/36525;
if abs(T)>2 then begin   // valid only 1800-2200
  P:=0;
  llat:=0;
  lats:=0;
  llong:=0;
  result:=false;
end else begin
e:=deg2rad*23.4392911;
eq2ecl(ra,dec,e,lp,b);
F:=93.2720993+483202.0175273*t-0.0034029*t*t-t*t*t/3526000+t*t*t*t/863310000;
om:=125.0445550-1934.1361849*t+0.0020762*t*t+t*t*t/467410-t*t*t*t/60616000;
w:=lp-deg2rad*om;
i:=deg2rad*1.54242;
l:=lp;
a:=rad2deg*(arctan2(sin(w)*cos(b)*cos(i)-sin(b)*sin(i),cos(w)*cos(b)));
llong:=to360(a-F);
if llong>180 then llong:=llong-360;
llat:=arcsin(-sin(w)*cos(b)*sin(i)-sin(b)*cos(i));
pl.ipla:=3;
pl.JD:=jde-tlight;
Plan404(addr(pl));
PrecessionEcl(jd2000,jde,pl.l,pl.b);
l0:=rad2deg*(pl.l)-180;
lh:=l0+180+(d/pl.r)*rad2deg*cos(b)*sin(pl.l-pi-l);
bh:=(d/pl.r)*pl.b;
w:=deg2rad*(lh-om);
lats:=rad2deg*(arcsin(-sin(w)*cos(bh)*sin(i)-sin(bh)*cos(i)));
aa:=rad2deg*(arctan2(sin(w)*cos(bh)*cos(i)-sin(bh)*sin(i),cos(w)*cos(bh)));
longs:=to360(aa-F);
v:=deg2rad*(om);
x:=sin(i)*sin(v);
y:=sin(i)*cos(v)*cos(e)-cos(i)*sin(e);
w:=arctan2(x,y);
P:=rad2deg*(arcsin(sqrt(x*x+y*y)*cos(ra-w)/cos(llat)));
llat:=rad2deg*(llat);
result:=true;
end;
end;

Function TPlanet.MoonMag(phase:double):double;
// The following table of lunar magnitudes is derived from relative
// intensities in Table 1 of M. Minnaert (1961),
// Phase  Frac.            Phase  Frac.            Phase  Frac.
// angle  ill.   Mag       angle  ill.   Mag       angle  ill.   Mag
//  0    1.00  -12.7        60   0.75  -11.0       120   0.25  -8.7
// 10    0.99  -12.4        70   0.67  -10.8       130   0.18  -8.2
// 20    0.97  -12.1        80   0.59  -10.4       140   0.12  -7.6
// 30    0.93  -11.8        90   0.50  -10.0       150   0.07  -6.7
// 40    0.88  -11.5       100   0.41   -9.6       160   0.03  -3.4
// 50    0.82  -11.2       110   0.33   -9.2
const mma : array[0..18]of double=(-12.7,-12.4,-12.1,-11.8,-11.5,-11.2,
                                   -11.0,-10.8,-10.4,-10.0,-9.6,-9.2,
                                   -8.7,-8.2,-7.6,-6.7,-3.4,0,0);
var i,j,k,p : integer;
begin
p:=(round(phase)+360) mod 360;
if p>180 then p:=360-p;
i:=p div 10;
k:=p-10*i;
j:=minintvalue([18,i+1]);
result:=mma[i]+((mma[j]-mma[i])*k/10);
end;

function TPlanet.Moon(t0 : double; var alpha,delta,dist,dkm,diam,phase,illum : double; highprec:boolean=true):string;
{
	t0      :  julian date DT
	alpha   :  Moon RA J2000
        delta   :  Moon DEC J2000
	dist    :  Earth-Moon distance UA
	dkm     :  Earth-Moon distance Km
	diam    :  Apparent diameter (arcseconds)
	phase   :  Phase angle  (degree)
	illum	:  Illuminated percentage
}
var
   p :TPlanetData;
   q : double;
   t,sm,mm,md : double;
   w : array[1..3] of double;
   planet_arr: Array_5D;
   i : integer;
   pp : double;
begin
result:='';
t0:=t0-(1.27/3600/24); // mean lighttime
if highprec then begin
   if (de_folder>'')and load_de(t0) then begin
     Calc_Planet_de(t0, 10, planet_arr,false,3,false);
     for i:=0 to 2 do w[i+1]:=planet_arr[i];
     result:='DE'+inttostr(de_type);
   end
   else begin
      result:=Moon(t0,alpha,delta,dist,dkm,diam,phase,illum,false);
      exit;
   end;
   dkm:=sqrt(w[1]*w[1]+w[2]*w[2]+w[3]*w[3]);
   alpha:=arctan2(w[2],w[1]);
   if (alpha<0) then alpha:=alpha+2*pi;
   pp:=sqrt(w[1]*w[1]+w[2]*w[2]);
   delta:=arctan(w[3]/pp);
   dist:=dkm/km_au;
end else begin  // use plan404
   p.JD:=t0;
   p.ipla:=11;
   Plan404(addr(p));
   dist:=sqrt(p.x*p.x+p.y*p.y+p.z*p.z);
   alpha:=arctan2(p.y,p.x);
   if (alpha<0) then alpha:=alpha+pi2;
   q:=sqrt(p.x*p.x+p.y*p.y);
   delta:=arctan(p.z/q);
   // plan404 give equinox of the date for the moon.
   precession(t0,jd2000,alpha,delta);
   dkm:=dist*km_au;
   result:='plan404';
end;
diam:=2*358482800/dkm;
t:=(t0-2415020)/36525;  { meeus 15.1 }
sm:=degtorad(358.475833+35999.0498*t-0.000150*t*t-0.0000033*t*t*t);  {meeus 30. }
mm:=degtorad(296.104608+477198.8491*t+0.009192*t*t+0.0000144*t*t*t);
md:=rmod(350.737486+445267.1142*t-0.001436*t*t+0.0000019*t*t*t,360);
phase:=180-md ;     { meeus 31.4 }
md:=degtorad(md);
phase:=rmod(phase-6.289*sin(mm)+2.100*sin(sm)-1.274*sin(2*md-mm)-0.658*sin(2*md)-0.214*sin(2*mm)-0.112*sin(md)+360,360);
illum:=(1+cos(degtorad(phase)))/2;
end;

Procedure TPlanet.MoonIncl(Lar,Lde,Sar,Sde : double; var incl : double);
{
	Lar  :  Moon RA
	Lde  :  Moon Dec
	Sar  :  Sun RA
	Sde  :  Sun Dec

	incl :  Position angle of the bright limb.
}
begin
{meeus 46.5 }
incl:=arctan2(cos(Sde)*sin(Sar-Lar),cos(Lde)*sin(Sde)-sin(Lde)*cos(Sde)*cos(Sar-Lar) );
end;

Procedure TPlanet.PlanetAltitude(pla: integer; jd0,hh: double;  var har,sina: double);
var jdt,ra,de,dm4,dm5,dm6,dm7,dm8: double;
begin
jdt:=jd0+(hh-TimeZone-DT_UT)/24;
case pla of
11 :  Moon(jdt,ra,de,dm4,dm5,dm6,dm7,dm8,false);
end;
precession(jd2000,jd0,ra,de);
har:=rmod(sidtim(jd0, hh-TimeZone, Obslongitude) - ra + pi2, pi2);
sina:=(sin(deg2rad*Obslatitude) * sin(de) + cos(deg2rad*Obslatitude) * cos(de) * cos(har));
end;

procedure TPlanet.PlanetRiseSet(pla:integer; jd0:double; AzNorth:boolean; var thr,tht,ths,tazr,tazs: string; var jdr,jdt,jds,rar,der,rat,det,ras,des:double ;var i: integer);
var hr,ht,hs,h1,h2,azr,azs,dist,q,diam : double;
    ho,sinho,dt,hh,y1,y2,y3,x1,x2,x3,xmax,ymax,xmax2,ymax2,ymax0,ra,de,dm5,dm6,dm7,dm8: double;
    frise,fset,ftransit: boolean;
    n: integer;
const  na='      ';
begin
jdr:=0;jdt:=0;jds:=0;ymax0:=0;
frise := false;fset := false;ftransit := false;
case pla of
1..9: ho:=-0.5667;
10 : ho:=-0.8333;
11: begin
    Moon(jd0,ra,de,dist,dm5,diam,dm7,dm8,false);
    ho:=(8.794/dist/3600)-0.5748*ObsRefractionCor-diam/2/3600-0.04;
    end;
end;
sinho:=sin(deg2rad*ho);
dt := jd0;
hh:=1;
PlanetAltitude(pla,dt,hh-1.0,x1,y1);
if x1>pi then x1:=x1-pi2;
y1:=y1-sinho;
// loop for event
while ( (hh < 25) and ( (fset=false) or (frise=false) or (ftransit=false) )) do begin
   PlanetAltitude(pla,dt,hh,x2,y2);
   if x2>pi then x2:=x2-pi2;
   y2:=y2-sinho;
   PlanetAltitude(pla,dt,hh+1.0,x3,y3);
   if x3>pi then x3:=x3-pi2;
   y3:=y3-sinho;
   int4(y1,y2,y3,n,h1,h2,xmax,ymax);
   // one rise/set
   if (n=1) then begin
      if (y1<0.0) then begin
         hr := hh + h1;
	 frise := true;
      end else begin
         hs := hh + h1;
	 fset := true;
      end;
   end;
   // two rise/set
   if (n = 2) then begin
      if (ymax < 0.0) then begin
         hr := hh + h2;
	 hs := hh + h1;
      end else begin
         hr := hh + h1;
	 hs := hh + h2;
      end;
   end;
   // transit
   if ((abs(xmax)<1) and (ymax>ymax0)) or
      ((hh<23) and (hh>1) and (abs(xmax)<1.5) and (ymax>ymax0)) //this one to correct for some imprecision in the extrema when very near to a time boundary
      then begin
         int4(x1,x2,x3,n,h1,h2,xmax2,ymax2);
         if (n=1) then begin
            ht := hh + h1;
            ymax0:=ymax;
            ftransit := true;
         end;
   end;
   y1 := y3;
   x1 := x3;
   hh:=hh+2;
end;
// format result
if (frise or fset) then begin    // rise (and/or) set and transit
   i:=0;
   if (frise) then begin       // rise
        thr:=armtostr(hr);
        jdr:=jd0+(hr-TimeZone)/24;
        case pla of
        11 :  Moon(jdr,ra,de,dist,dm5,dm6,dm7,dm8,false);
        end;
        precession(jd2000,jd0,ra,de);
        if PlanetParalaxe then begin
           Paralaxe(SidTim(jd0,hr-TimeZone,ObsLongitude),dist,ra,de,rar,der,q,jd0,jd0);
        end else begin
           rar:=ra;
           der:=de;
        end;
        Eq2Hz(sidtim(jd0,hr-TimeZone,ObsLongitude)-ra,de,azr,dist);
        if AzNorth then Azr:=rmod(Azr+pi,pi2);
        tazr:=demtostr(rad2deg*Azr);
      end
      else begin
        thr:=na;
        tazr:=na;
      end;
   if (fset) then begin       // set
        ths:=armtostr(hs);
        jds:=jd0+(hs-TimeZone)/24;
        case pla of
        11 :  Moon(jds,ra,de,dist,dm5,dm6,dm7,dm8,false);
        end;
        precession(jd2000,jd0,ra,de);
        if PlanetParalaxe then begin
           Paralaxe(SidTim(jd0,hs-TimeZone,ObsLongitude),dist,ra,de,ras,des,q,jd0,jd0);
        end else begin
           ras:=ra;
           des:=de;
        end;
        Eq2Hz(sidtim(jd0,hs-TimeZone,ObsLongitude)-ra,de,azs,dist);
        if AzNorth then Azs:=rmod(Azs+pi,pi2);
        tazs:=demtostr(rad2deg*Azs);
      end
      else begin
        ths:=na;
        tazs:=na;
      end;
   if (ftransit) then begin      // transit
        tht:=armtostr(ht);
        jdt:=jd0+(ht-TimeZone)/24;
        case pla of
        11 :  Moon(jdt,ra,de,dist,dm5,dm6,dm7,dm8,false);
        end;
        precession(jd2000,jd0,ra,de);
        if PlanetParalaxe then begin
           Paralaxe(SidTim(jd0,ht-TimeZone,ObsLongitude),dist,ra,de,rat,det,q,jd0,jd0);
        end else begin
           rat:=ra;
           det:=de;
        end;
      end
      else begin
        tht:=na;
      end;
end else begin
   if (ftransit) then begin   // circumpolar
        i:=1;
        thr:=na;
        ths:=na;
        tazr:=na;
        tazs:=na;
        tht:=armtostr(ht);
        jdt:=jd0+(ht-TimeZone)/24;
        case pla of
        11 :  Moon(jdt,ra,de,dist,dm5,dm6,dm7,dm8,false);
        end;
        precession(jd2000,jd0,ra,de);
        if PlanetParalaxe then begin
           Paralaxe(SidTim(jd0,ht-TimeZone,ObsLongitude),dist,ra,de,rat,det,q,jd0,jd0);
        end else begin
           rat:=ra;
           det:=de;
        end;
      end
      else begin  // not visible
        i:=2;
        thr:=na;
        ths:=na;
        tht:=na;
        tazr:=na;
        tazs:=na;
      end;
end;
end;

Function TPlanet.MoonPhase(k: double):double;
var t,t2,t3,t4,cor,e,e2,M,Mm,f,ome,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,fr,w : double;
begin
t := k/1236.85;
t2 := t*t;
t3 := t2*t;
t4 := t3*t;
result:= 2451550.09766 + 29.530588861*k + 0.00015437*T2 - 0.000000150*T3 + 0.00000000073*T4;
e := 1 - 0.002516*T - 0.0000074*T2;
e2 := e*e;
M := deg2rad*rmod(2.5534 + 29.10535670*k - 0.0000014*T2 - 0.00000011*T3 + 360000000000,360);
Mm := deg2rad*rmod(201.5643 + 385.81693528*k + 0.0107582*T2 + 0.00001238*T3 - 0.000000058*T4 + 360000000000,360);
F  := deg2rad*rmod(160.7108 + 390.67050284*k - 0.0016118*T2 - 0.00000227*T3 + 0.00000001*T4 + 360000000000,360);
ome := deg2rad*rmod(124.7746 - 1.56375588*k + 0.0020672*T2 + 0.00000215*T3 + 360000000000,360);
A1 := deg2rad*rmod(299.77 + 0.107408*k - 0.009173*T2 + 360000000000,360);
A2 := deg2rad*rmod(251.88 + 0.016321*k + 360000000000,360);
A3 := deg2rad*rmod(251.83 + 26.651886*k + 360000000000,360);
A4 := deg2rad*rmod(349.42 + 36.412478*k + 360000000000,360);
A5 := deg2rad*rmod(84.66 + 18.206239*k + 360000000000,360);
A6 := deg2rad*rmod(141.74 + 53.303771*k + 360000000000,360);
A7 := deg2rad*rmod(207.14 + 2.453732*k + 360000000000,360);
A8 := deg2rad*rmod(154.84 + 7.306860*k + 360000000000,360);
A9 := deg2rad*rmod(34.52 + 27.261239*k + 360000000000,360);
A10 := deg2rad*rmod(207.19 + 0.121824*k + 360000000000,360);
A11 := deg2rad*rmod(291.34 + 1.844379*k + 360000000000,360);
A12 := deg2rad*rmod(161.72 + 24.198154*k + 360000000000,360);
A13 := deg2rad*rmod(239.56 + 25.513099*k + 360000000000,360);
A14 := deg2rad*rmod(331.55 + 3.592518*k + 360000000000,360);
fr := k - floor(k);
if (fr = 0) then begin //New Moon
   cor := -0.40720*sin(Mm) +
          0.17241*E*sin(M) +
          0.01608*sin(2*Mm) +
          0.01039*sin(2*F) +
          0.00739*E*sin(Mm - M) +
          -0.00514*E*sin(Mm + M) +
          0.00208*E2*sin(2*M) +
          -0.00111*sin(Mm - 2*F) +
          -0.00057*sin(Mm + 2*F) +
          0.00056*E*sin(2*Mm + M) +
          -0.00042*sin(3*Mm) +
          0.00042*E*sin(M + 2*F) +
          0.00038*E*sin(M - 2*F) +
          -0.00024*E*sin(2*Mm - M) +
          -0.00017*sin(ome) +
          -0.00007*sin(Mm + 2*M) +
          0.00004*sin(2*Mm - 2*F) +
          0.00004*sin(3*M) +
          0.00003*sin(Mm + M - 2*F) +
          0.00003*sin(2*Mm + 2*F) +
          -0.00003*sin(Mm + M + 2*F) +
          0.00003*sin(Mm - M + 2*F) +
          -0.00002*sin(Mm - M - 2*F) +
          -0.00002*sin(3*Mm + M) +
          0.00002*sin(4*Mm);
    result := result + cor;
  end
  else if ((fr = 0.25)or(fr = 0.75)) then begin //First and Last Quarter
    cor := -0.62801*sin(Mm) +
          0.17172*E*sin(M) +
          -0.01183*E*sin(Mm + M) +
          0.00862*sin(2*Mm) +
          0.00804*sin(2*F) +
          0.00454*E*sin(Mm - M) +
          0.00204*E2*sin(2*M) +
          -0.00180*sin(Mm - 2*F) +
          -0.00070*sin(Mm + 2*F) +
          -0.00040*sin(3*Mm) +
          -0.00034*E*sin(2*Mm - M) +
          0.00032*E*sin(M + 2*F) +
          0.00032*E*sin(M - 2*F) +
          -0.00028*E2*sin(Mm + 2*M) +
          0.00027*E*sin(2*Mm + M) +
          -0.00017*sin(ome) +
          -0.00005*sin(Mm - M - 2*F) +
          0.00004*sin(2*Mm + 2*F) +
          -0.00004*sin(Mm + M + 2*F) +
          0.00004*sin(Mm - 2*M) +
          0.00003*sin(Mm + M - 2*F) +
          0.00003*sin(3*M) +
          0.00002*sin(2*Mm - 2*F) +
          0.00002*sin(Mm - M + 2*F) +
          -0.00002*sin(3*Mm + M);
    result := result + cor;
    W := 0.00306 - 0.00038*E*cos(M) + 0.00026*cos(Mm) - 0.00002*cos(Mm - M) + 0.00002*cos(Mm + M) + 0.00002*cos(2*F);
    if (fr = 0.25) then //First quarter
      result := result + W
    else
      result := result - W;
  end
  else if (fr = 0.5) then begin //Full Moon
    cor := -0.40614*sin(Mm) +
          0.17302*E*sin(M) +
          0.01614*sin(2*Mm) +
          0.01043*sin(2*F) +
          0.00734*E*sin(Mm - M) +
          -0.00514*E*sin(Mm + M) +
          0.00209*E2*sin(2*M) +
          -0.00111*sin(Mm - 2*F) +
          -0.00057*sin(Mm + 2*F) +
          0.00056*E*sin(2*Mm + M) +
          -0.00042*sin(3*Mm) +
          0.00042*E*sin(M + 2*F) +
          0.00038*E*sin(M - 2*F) +
          -0.00024*E*sin(2*Mm - M) +
          -0.00017*sin(ome) +
          -0.00007*sin(Mm + 2*M) +
          0.00004*sin(2*Mm - 2*F) +
          0.00004*sin(3*M) +
          0.00003*sin(Mm + M - 2*F) +
          0.00003*sin(2*Mm + 2*F) +
          -0.00003*sin(Mm + M + 2*F) +
          0.00003*sin(Mm - M + 2*F) +
          -0.00002*sin(Mm - M - 2*F) +
          -0.00002*sin(3*Mm + M) +
          0.00002*sin(4*Mm);
    result := result + cor;
  end
  else result:=0/0;

  //Additional corrections
  cor:= 0.000325*sin(A1) +
        0.000165*sin(A2) +
        0.000164*sin(A3) +
        0.000126*sin(A4) +
        0.000110*sin(A5) +
        0.000062*sin(A6) +
        0.000060*sin(A7) +
        0.000056*sin(A8) +
        0.000047*sin(A9) +
        0.000042*sin(A10) +
        0.000040*sin(A11) +
        0.000037*sin(A12) +
        0.000035*sin(A13) +
        0.000023*sin(A14);
  result := result + cor;
end;

Procedure TPlanet.MoonPhases(year:double; var nm,fq,fm,lq : double);
var k : double;
begin
k := (year - 2000) * 12.3685;
k := floor(k);
nm := MoonPhase(k);
fq := MoonPhase(k+0.25);
fm := MoonPhase(k+0.50);
lq := MoonPhase(k+0.75);
end;

procedure TPlanet.SetDE(folder:string);
begin
   de_folder:=pchar(folder);
end;

function TPlanet.load_de(t: double): boolean;
var
  i: integer;
  y,ys,ye,m,d : integer;
  hour,jdstart,jdend: double;
  defn: string;
begin
djd(t,y,m,d,hour);
if y=de_year then begin
  result:=(de_type<>0);
end else begin
  result:=false;
  de_type:=0;
  for i:=1 to nJPL_DE do begin
     if load_de_file(t,de_folder,JPL_DE[i], defn,jdstart,jdend) then begin
       result:=true;
       de_type:=JPL_DE[i];
       break;
     end;
  end;
  if result then begin
    djd(jdstart,ys,m,d,hour);
    djd(jdend,ye,m,d,hour);
    if (y=ys)or(y=ye) then
       de_year:=MaxInt
    else
       de_year:=y;
  end
   else
    de_year:=MaxInt;
end;
end;

procedure TPlanet.nutation(j:double; var nutl,nuto:double);
var planet_arr: Array_5D;
    ok:boolean;
begin
ok:=false;
if (de_folder>'')and load_de(j) then begin
  ok:=Calc_Planet_de(j, 14, planet_arr,false,3,false);
  if ok then begin
    nutl:=planet_arr[0];
    nuto:=planet_arr[1];
  end;
end;
if not ok then
  nutationMe(j,nutl,nuto);
end;

Procedure TPlanet.SunEcl(t0 : double ; var l,b : double);
var p :TPlanetData;
begin
p.ipla:=3;
p.JD:=t0;
Plan404(addr(p));
l:=rmod(pi+p.l,pi2);
b:=-p.b;
end;

end.

