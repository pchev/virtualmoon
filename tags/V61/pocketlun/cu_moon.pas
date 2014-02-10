unit cu_moon;
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
 Moon computation component.
}
{$mode objfpc}{$H+}
interface

uses
  series96main, // series 96 statically linked
  elp82main,    // elp82 statically linked
  u_astro,
  Classes, Sysutils, Math;

type
  TMoon = class(TComponent)
  private
    { Private declarations }
    SolT0,XSol,YSol,ZSol : double;
    SolAstrometric : boolean;
  protected
    { Protected declarations }
  public
    { Public declarations }
     constructor Create(AOwner:TComponent); override;
     destructor  Destroy; override;
     Procedure SunRect(t0 : double ; astrometric : boolean; var x,y,z : double);
     Procedure Sun(t0 : double; var alpha,delta,dist,diam : double);
     Procedure SunEcl(t0 : double ; var l,b : double);
     Procedure Moon(t0 : double; var alpha,delta,dist,dkm,diam,phase,illum : double);
     Procedure QuickMoon(t0 : double; var alpha,delta,prec : double);
     Procedure MoonIncl(Lar,Lde,Sar,Sde : double; var incl : double);
     Function MoonMag(phase:double):double;
     Procedure MoonOrientation(jde,ra,dec,d:double; var P,llat,lats,llong : double);
     Function MoonPhase(k: double):double;
     Procedure MoonPhases(year:double; var nm,fq,fm,lq : double);
     Procedure MoonAltitude(jd0,hh,TimeZone,DT_UT,Obslatitude,Obslongitude: double; var har,sina: double);
     procedure MoonRiseSet(jd0,TimeZone,DT_UT,Obslatitude,Obslongitude,ObsRoCosPhi,ObsRoSinPhi:double; AzNorth:boolean; var thr,tht,ths,tazr,talt,tazs: string; var jdr,jdt,jds,rar,der,rat,det,ras,des:double ;var i: integer);
  end;

implementation

//const
//    Dates limits series96
//      series96t1 = 2415020.5;    // From : JD2415020.5d0 (1 Jan 1900 0h)
//      series96t2 = 2487980.5;    // To   : JD2487980.5d0 (4 Oct 2099 0h)
//    Dates limits elp82
//      elp82t1 = 2415020.5;       // From : JD2415020.5d0 (1 Jan 1900 0h)
//      elp82t2 = 2487980.5;       // To   : JD2487980.5d0 (4 Oct 2099 0h)

constructor TMoon.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
end;

destructor TMoon.Destroy;
begin
inherited destroy;
end;

Procedure TMoon.SunRect(t0 : double ; astrometric : boolean; var x,y,z : double);
var v,v2 : double6;
    tjd : double;
    i : integer;
begin
if (t0=SolT0)and(astrometric=Solastrometric) then begin
   x:=XSol;
   y:=YSol;
   z:=ZSol;
   end
else begin
if astrometric then tjd:=t0-tlight
               else tjd:=t0;
  Plan96 (tjd,3,false,addr(v),i);
  if (i<>0) then exit;
  Earth96 (tjd,addr(v2));
  for i:=0 to 2 do begin
    v[i]:=v2[i]-v[i];
  end;
  x:=v[0];
  y:=v[1];
  z:=v[2];
// save result for repetitive call
Solastrometric:=astrometric;
SolT0:=t0;
XSol:=x;
YSol:=y;
ZSol:=z;
end;
end;

Procedure TMoon.Sun(t0 : double; var alpha,delta,dist,diam : double);
var x,y,z : double;
begin
  SunRect(t0,true,x,y,z);
  dist:=sqrt(x*x+y*y+z*z);
  alpha:=arctan2(y,x);
  if (alpha<0) then alpha:=alpha+pi2;
  delta:=arctan(z/sqrt(x*x+y*y));
  diam:=2*959.63/dist;
end;

Procedure TMoon.SunEcl(t0 : double ; var l,b : double);
var ar,de,d1,d2: double;
begin
Sun(t0,ar,de,d1,d2);
eq2ecl(ar,de,eps2000,l,b);
end;

Procedure TMoon.MoonOrientation(jde,ra,dec,d:double; var P,llat,lats,llong : double);
var lp,l,b,f,om,w,T,a,i,lh,bh,e,v,x,y,l0 {,cel,sel,asol,dsol} : double;
    ars,des,ls,bs,rs,ds,pll,plb,plr: double;
begin
T := (jde-2451545)/36525;
e:=eps2000;
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
Sun(jde,ars,des,rs,ds);
eq2ecl(ars,des,eps2000,ls,bs);
pll:=rmod(pi+ls,pi2);
plb:=-bs;
plr:=rs;
PrecessionEcl(jd2000,jde,pll,plb);
l0:=rad2deg*(pll)-180;
lh:=l0+180+(d/plr)*rad2deg*cos(b)*sin(pll-pi-l);
bh:=(d/plr)*plb;
w:=deg2rad*(lh-om);
lats:=rad2deg*(arcsin(-sin(w)*cos(bh)*sin(i)-sin(bh)*cos(i)));
v:=deg2rad*(om);
x:=sin(i)*sin(v);
y:=sin(i)*cos(v)*cos(e)-cos(i)*sin(e);
w:=arctan2(x,y);
P:=rad2deg*(arcsin(sqrt(x*x+y*y)*cos(ra-w)/cos(llat)));
llat:=rad2deg*(llat);
end;


Function TMoon.MoonMag(phase:double):double;
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

Procedure TMoon.Moon(t0 : double; var alpha,delta,dist,dkm,diam,phase,illum : double);
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
   t,sm,mm,md : double;
   w : array[1..3] of double;
   ierr : integer;
   prec,pp : double;
begin
   prec:=0;
   ELP82B(t0,prec,w,ierr);
   dkm:=sqrt(w[1]*w[1]+w[2]*w[2]+w[3]*w[3]);
   alpha:=arctan2(w[2],w[1]);
   if (alpha<0) then alpha:=alpha+2*pi;
   pp:=sqrt(w[1]*w[1]+w[2]*w[2]);
   delta:=arctan(w[3]/pp);
   dist:=dkm/km_au;
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

Procedure TMoon.QuickMoon(t0 : double; var alpha,delta,prec : double);
{
	t0      :  julian date DT
	alpha   :  Moon RA J2000
        delta   :  Moon DEC J2000
        prec    :  truncation level in radian
}
var
   w : array[1..3] of double;
   ierr : integer;
   pp : double;
begin
   ELP82B(t0,prec,w,ierr);
   alpha:=arctan2(w[2],w[1]);
   if (alpha<0) then alpha:=alpha+2*pi;
   pp:=sqrt(w[1]*w[1]+w[2]*w[2]);
   delta:=arctan(w[3]/pp);
end;

Procedure TMoon.MoonIncl(Lar,Lde,Sar,Sde : double; var incl : double);
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

Function TMoon.MoonPhase(k: double):double;
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

Procedure TMoon.MoonPhases(year:double; var nm,fq,fm,lq : double);
var k : double;
begin
k := (year - 2000) * 12.3685;
k := floor(k);
nm := MoonPhase(k);
fq := MoonPhase(k+0.25);
fm := MoonPhase(k+0.50);
lq := MoonPhase(k+0.75);
end;

Procedure TMoon.MoonAltitude(jd0,hh,TimeZone,DT_UT,Obslatitude,Obslongitude: double; var har,sina: double);
var jdt,ra,de,prec: double;
begin
prec:=deg2rad*10/60; // 10 arcmin precision is sufficient for rise time computation
jdt:=jd0+(hh-TimeZone-DT_UT)/24;
QuickMoon(jdt,ra,de,prec);
precession(jd2000,jd0,ra,de);
har:=rmod(sidtim(jd0, hh-TimeZone, Obslongitude) - ra + pi2, pi2);
sina:=(sin(deg2rad*Obslatitude) * sin(de) + cos(deg2rad*Obslatitude) * cos(de) * cos(har));
end;

procedure int4(y1,y2,y3:double; var n: integer; var x1,x2,xmax,ymax: double);
var a, b, c, d, dx: double;
begin
n:=0;
a:=(y1+y3)/2-y2;
b:=(y3-y1)/2;
c:=y2;
xmax:=-b/(2*a);
ymax:=(a*xmax+b)*xmax+c;
d:=b*b-4.0*a*c;
if (d>0) then begin
   dx:=sqrt(d)/abs(a)/2;
   x1:=xmax-dx;
   x2:=xmax+dx;
   if (abs(x1)<=1) then inc(n);
   if (abs(x2)<=1) then inc(n);
   if (x1<-1) then x1:=x2;
end;
end;

procedure TMoon.MoonRiseSet(jd0,TimeZone,DT_UT,Obslatitude,Obslongitude,ObsRoCosPhi,ObsRoSinPhi:double; AzNorth:boolean; var thr,tht,ths,tazr,talt,tazs: string; var jdr,jdt,jds,rar,der,rat,det,ras,des:double ;var i: integer);
var hr,ht,hs,h1,h2,azr,azs,dist,q,diam,alt,culmalt : double;
    ho,sinho,dt,hh,y1,y2,y3,x1,x2,x3,xmax,ymax,xmax2,ymax2,ymax0,ra,de,dm5,dm6,dm7,dm8,dm9: double;
    frise,fset,ftransit: boolean;
    n: integer;
const  na='      ';
begin
jdr:=0;jdt:=0;jds:=0;ymax0:=0;
frise := false;fset := false;ftransit := false;
Moon(jd0,ra,de,dist,dm5,diam,dm7,dm8);
ho:=(8.794/dist/3600)-0.5748-diam/2/3600-0.04;
sinho:=sin(deg2rad*ho);
dt := jd0;
hh:=1;
MoonAltitude(dt,hh-1.0,TimeZone,DT_UT,Obslatitude,Obslongitude,x1,y1);
if x1>pi then x1:=x1-pi2;
y1:=y1-sinho;
// loop for event
while ( (hh < 25) and ( (fset=false) or (frise=false) or (ftransit=false) )) do begin
   MoonAltitude(dt,hh,TimeZone,DT_UT,Obslatitude,Obslongitude,x2,y2);
   if x2>pi then x2:=x2-pi2;
   y2:=y2-sinho;
   MoonAltitude(dt,hh+1.0,TimeZone,DT_UT,Obslatitude,Obslongitude,x3,y3);
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
        Moon(jdr,ra,de,dist,dm5,dm6,dm7,dm8);
        precession(jd2000,jd0,ra,de);
        Paralaxe(SidTim(jd0,hr-TimeZone,ObsLongitude),dist,ra,de,jdr,ObsRoCosPhi,ObsRoSinPhi,rar,der,q);
        Eq2Hz(sidtim(jd0,hr-TimeZone,ObsLongitude)-ra,de,ObsLatitude,azr,alt);
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
        Moon(jds,ra,de,dist,dm5,dm6,dm7,dm8);
        precession(jd2000,jd0,ra,de);
        Paralaxe(SidTim(jd0,hs-TimeZone,ObsLongitude),dist,ra,de,jds,ObsRoCosPhi,ObsRoSinPhi,ras,des,q);
        Eq2Hz(sidtim(jd0,hs-TimeZone,ObsLongitude)-ra,de,ObsLatitude,azs,alt);
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
        Moon(jdt,ra,de,dist,dm5,dm6,dm7,dm8);
        precession(jd2000,jd0,ra,de);
        Paralaxe(SidTim(jd0,ht-TimeZone,ObsLongitude),dist,ra,de,jdt,ObsRoCosPhi,ObsRoSinPhi,rat,det,q);
        culmalt:= 90 - ObsLatitude + rad2deg*det;
        if culmalt>90 then culmalt:=180-culmalt;
        if culmalt>-1 then culmalt:=min(90,culmalt+(1.02/tan(deg2rad*(culmalt+10.3/(culmalt+5.11))))/60)
                      else culmalt:=culmalt+0.64658062088;
        talt:=demtostr(culmalt);
      end
      else begin
        tht:=na;
        talt:=na;
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
        Moon(jdt,ra,de,dist,dm5,dm6,dm7,dm8);
        precession(jd2000,jd0,ra,de);
        Paralaxe(SidTim(jd0,ht-TimeZone,ObsLongitude),dist,ra,de,jdt,ObsRoCosPhi,ObsRoSinPhi,rat,det,q);
        culmalt:= 90 - ObsLatitude + rad2deg*det;
        if culmalt>90 then culmalt:=180-culmalt;
        if culmalt>-1 then culmalt:=min(90,culmalt+(1.02/tan(deg2rad*(culmalt+10.3/(culmalt+5.11))))/60)
                      else culmalt:=culmalt+0.64658062088;
        talt:=demtostr(culmalt);
      end
      else begin  // not visible
        i:=2;
        thr:=na;
        ths:=na;
        tht:=na;
        tazr:=na;
        talt:=na;
        tazs:=na;
      end;
end;
end;

end.

