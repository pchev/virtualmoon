unit u_astro;
{
Copyright (C) 2007 Patrick Chevalley

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

Function AngularDistance(ar1,de1,ar2,de2 : Double) : Double;
function  Rmod(x,y:Double):Double;
function to360(x:double):double;
Function sgn(x:Double):Double ;
function Jd(annee,mois,jour :INTEGER; Heure:double):double;
PROCEDURE Djd(jd:Double;out annee,mois,jour:INTEGER; out Heure:double);
function SidTim(jd0,ut,long : double): double;
PROCEDURE Precession(ti,tf : double; var ari,dei : double);  // ICRS
procedure Paralaxe(SideralTime,dist,ar1,de1,currentjd,ObsRoCosPhi,ObsRoSinPhi : double; out ar,de,q : double);
PROCEDURE Eq2Hz(HH,DE,ObsLatitude : double ; VAR A,h : double);
Procedure Eq2Ecl(ar,de,e: double; out l,b: double);
procedure PrecessionEcl(ti,tf : double; var l,b : double);
function DTminusUT(annee : integer) : double;
Function ARToStr(ar: Double) : string;
Function DEToStr(de: Double) : string;
Function ARmToStr(ar: Double) : string;
Function DEmToStr(de: Double) : string;
Function DEdToStr(de: Double) : string;
Function DatToStr(y,m,d: integer) : string;
Function TimToStr(de: Double) : string;
Function TimmToStr(de: Double) : string;
const
   tlight : double = 0.577551831e-2;
   deg2rad = pi/180;
   rad2deg = 180/pi;
   jd2000 = 2451545.0 ;
   eps2000 = 23.439291111*deg2rad;
   pi2 = 2*pi;
   pi4 = 4*pi;
   pid2 = pi/2;
   km_au = 149597870.691 ;
var
   ldeg : string = 'd';
   lmin : string  = 'm';
   lsec : string  = 's';

implementation

Function AngularDistance(ar1,de1,ar2,de2 : Double) : Double;
var s1,s2,c1,c2: extended;
begin
try
if (ar1=ar2) and (de1=de2) then result:=0.0
else begin
    sincos(Deg2Rad*(de1),s1,c1);
    sincos(Deg2Rad*(de2),s2,c2);
    result:=Rad2Deg*(arccos((s1*s2)+(c1*c2*cos(Deg2Rad*((ar1-ar2))))));
end;
except
  result:=0;
end;
end;

function  Rmod(x,y:Double):Double;
BEGIN
    Rmod := x - Int(x/y) * y ;
END  ;

function to360(x:double):double;
begin
result:=rmod(x+3600000000,360);
end;

Function sgn(x:Double):Double ;
begin
if x<0 then
   sgn:= -1
else
   sgn:=  1 ;
end ;

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

PROCEDURE Djd(jd:Double;out annee,mois,jour:INTEGER; out Heure:double);
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

function SidTim(jd0,ut,long : double): double;
 VAR t,te: double;
BEGIN
 t:=(jd0-2451545.0)/36525;
 te:=100.46061837 + 36000.770053608*t + 0.000387933*t*t - t*t*t/38710000;
 result := deg2rad*Rmod(te - long + 1.00273790935*ut*15,360) ;
END ;

procedure Paralaxe(SideralTime,dist,ar1,de1,currentjd,ObsRoCosPhi,ObsRoSinPhi : double; out ar,de,q : double);
var
   sinpi,H,a,b,d : double;
const
     desinpi = 4.26345151e-5;
begin
// AR, DE are J2000 but paralaxe is to be computed with coordinates of the date.
precession(jd2000,currentjd,ar1,de1);
H:=(SideralTime-ar1);
sinpi:=desinpi/dist;
a := cos(de1)*sin(H);
b := cos(de1)*cos(H)-ObsRoCosPhi*sinpi;
d := sin(de1)-ObsRoSinPhi*sinpi;
q := sqrt(a*a+b*b+d*d);
ar:=SideralTime-arctan2(a,b);
de:=double(arcsin(d/q));
precession(currentjd,jd2000,ar,de);
end;

PROCEDURE Precession(ti,tf : double; var ari,dei : double);  // ICRS
var i1,i2,i3,i4,i5,i6,i7 : double ;
   BEGIN
   if abs(ti-tf)<0.01 then exit;
      I1:=(TI-2451545.0)/36525 ;
      I2:=(TF-TI)/36525;
      I3:=deg2rad*((2306.2181+1.39656*i1-1.39e-4*i1*i1)*i2+(0.30188-3.44e-4*i1)*i2*i2+1.7998e-2*i2*i2*i2)/3600 ;
      I4:=deg2rad*((2306.2181+1.39656*i1-1.39e-4*i1*i1)*i2+(1.09468+6.6e-5*i1)*i2*i2+1.8203e-2*i2*i2*i2)/3600 ;
      I5:=deg2rad*((2004.3109-0.85330*i1-2.17e-4*i1*i1)*i2-(0.42665+2.17e-4*i1)*i2*i2-4.1833e-2*i2*i2*i2)/3600 ;
      I6:=COS(DEI)*SIN(ARI+I3) ;
      I7:=COS(I5)*COS(DEI)*COS(ARI+I3)-SIN(I5)*SIN(DEI) ;
      i1:=(SIN(I5)*COS(DEI)*COS(ARI+I3)+COS(I5)*SIN(DEI));
      if i1>1 then i1:=1;
      if i1<-1 then i1:=-1;
      DEI:=double(ArcSIN(i1));
      ARI:=double(ARCTAN2(I6,I7)) ;
      ARI:=ARI+I4;
      ARI:=RMOD(ARI+pi2,pi2);
   END  ;

PROCEDURE Eq2Hz(HH,DE,ObsLatitude : double ; VAR A,h : double);
var l1,d1,h1 : double;
BEGIN
l1:=deg2rad*ObsLatitude;
d1:=DE;
h1:=HH;
h:= double(arcsin( sin(l1)*sin(d1)+cos(l1)*cos(d1)*cos(h1) ));
A:= double(arctan2(sin(h1),cos(h1)*sin(l1)-tan(d1)*cos(l1)));
A:=Rmod(A+pi2,pi2);
{ refraction meeus91 15.4 }
h1:=rad2deg*h;
if h1>-1 then h:=double(minvalue([pid2,h+deg2rad*(1.02/tan(deg2rad*(h1+10.3/(h1+5.11))))/60]))
         else h:=h+deg2rad*0.64658062088*(h1+90)/89;
END ;

Procedure Eq2Ecl(ar,de,e: double; out l,b: double);
begin
l:=double(arctan2(sin(ar)*cos(e)+tan(de)*sin(e),cos(ar)));
b:=double(arcsin(sin(de)*cos(e)-cos(de)*sin(e)*sin(ar)));
end;

procedure PrecessionEcl(ti,tf : double; var l,b : double);
var i1,i2,i3,i4,i5,i6,i7,i8 : double ;
begin
i1:=(ti-2451545.0)/36525 ;
i2:=(tf-ti)/36525;
i3:=deg2rad*(((47.0029-0.06603*i1+0.000598*i1*i1)*i2+(-0.03302+0.000598*i1)*i2*i2+0.000060*i2*i2*i2)/3600);
i4:=deg2rad*((174.876384*3600+3289.4789*i1+0.60622*i1*i1-(869.8089+0.50491*i1)*i2+0.03536*i2*i2)/3600);
i5:=deg2rad*(((5029.0966+2.22226*i1-0.000042*i1*i1)*i2+(1.11113-0.000042*i1)*i2*i2-0.000006*i2*i2*i2)/3600);
i6:=cos(i3)*cos(b)*sin(i4-l)-sin(i3)*sin(b);
i7:=cos(b)*cos(i4-l);
i8:=cos(i3)*sin(b)+sin(i3)*cos(b)*sin(i4-l);
l:=i5+i4-arctan2(i6,i7);
b:=double(arcsin(i8));
l:=rmod(l+pi2,pi2);
end;

function DTminusUT(annee : integer) : double;
var t : double;
begin
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
 1999..2005 : result:=64/3600;
 2006..2007 : result:=65/3600;
 2008..2020 : begin
              t:=(annee-2007)/100;
              result:=(65+50*t+32.5*t*t)/3600;
              end;
 2021..99999 : begin
              t:=(annee-1875.1)/100;
              result:=45.39*t*t/3600;
              end;
 else result:=0;
 end;
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

Function DatToStr(y,m,d: integer) : string;
var buf:string;
begin
result:=inttostr(y)+'-';
buf:=inttostr(m);
if m<10 then buf:='0'+buf;
result:=result+buf+'-';
buf:=inttostr(d);
if d<10 then buf:='0'+buf;
result:=result+buf;
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

Function TimmToStr(de: Double) : string;
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
    str(min:2:0,m);
    if abs(min)<10 then m:='0'+trim(m);
    result := d+':'+m;
end;

end.

