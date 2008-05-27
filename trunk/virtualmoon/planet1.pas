unit Planet1;
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

uses Math,Skylib,
  Windows, Messages,  Classes, Graphics, Controls, Forms, Dialogs;

Procedure Planet(ipla : integer; t0 : double ; var alpha,delta,distance,illum,phase,diameter,magn,dp : double; var ierr :integer) ;
Procedure SolRect(t0 : double ; astrometric : boolean; var x,y,z : double;var ierr :integer);
Procedure Soleil(t0 : double; var alpha,delta,dist,diam : double;var ierr :integer);
Function MarSat(jde,diam : double; var xsat,ysat : double8; var supconj : array of boolean):integer;
Function JupSat(jde,diam : double; var xsat,ysat : double8; var supconj : array of boolean):integer;
Function SatSat(jde,diam : double; var xsat,ysat : double8; var supconj : array of boolean):integer;
Function UraSat(jde,diam : double; var xsat,ysat : double8; var supconj : array of boolean):integer;
Procedure SatRing(jde : double; var P,a,b,be : double);
Function MoonMag(phase:double):double;
Procedure PlanetOrientation(jde:double; ipla:integer; var P,De,Ds,w1,w2,w3 : double);
Procedure MoonOrientation(jde,ra,dec,d:double; var P,llat,lats,llong : double);
Function MoonPhase(k: double):double;
Procedure MoonPhases(year:double; var nm,fq,fm,lq : double);

const
      nommarsat : array [1..2] of string[8] = ('Phobos  ','Deimos  ');
      nomjupsat : array [1..4] of string[8] = ('Io      ','Europa  ','Ganymede','Callisto');
      nomsatsat : array [1..8] of string[8] = ('Mimas   ','Encelade','Tethys  ','Dione   ','Rhea    ','Titan   ','Hyperion','Iapetus ');
      nomurasat : array [1..5] of string[8] = ('Miranda ','Ariel   ','Umbriel ','Titania ','Oberon  ');
//      V0mar : array [0..2] of double = (0.523,11.6,12.7);
//      V0jup : array [0..4] of double = (4.202,5.0,5.3,4.6,5.6);
//      V0sat : array [0..8] of double = (8.538,12.9,11.8,10.3,10.4,9.7,8.4,14.2,10.2);
//      V0ura : array [0..5] of double = (18.182,16.5,14.4,15.3,14,14.2);
      V0mar : array [1..2] of double = (11.80,12.89);
      V0jup : array [1..4] of double = (-1.68,-1.41,-2.09,-1.05);
      V0sat : array [1..8] of double = (3.30,2.10,0.60,0.80,0.10,-1.28,4.63,1.50);
      V0ura : array [1..5] of double = (3.60,1.45,2.10,1.02,1.23);
      D0mar : array [1..2] of double = (11,6);
      D0jup : array [1..4] of double = (1821,1565,2634,2403);
      D0sat : array [1..8] of double = (199,249,530,560,764,2575,143,718);
      D0ura : array [1..5] of double = (236,581,585,789,761);
      km_au = 149597870.691 ;
      tlight : double = 0.577551831e-2;
      eps2000 = 23.4392911;


implementation

const
{*     ------------
*     Dates limits series96
*     ------------
*}
      t1 = 2415020.5;    // From : JD2415020.5d0 (1 Jan 1900 0h)
      t2 = 2487980.5;    // To   : JD2487980.5d0 (4 Oct 2099 0h)

var SolT0,XSol,YSol,ZSol : double;
    SolAstrometric : boolean;

function  Rmod(x,y:Double):Double;
BEGIN
    Rmod := x - Int(x/y) * y ;
END  ;


Procedure Planet(ipla : integer; t0 : double ; var alpha,delta,distance,illum,phase,diameter,magn,dp : double; var ierr :integer) ;
{*
          Translation pour Delphi : P. Chevalley 22 mars 1998
          + retourne  distance,phase,diametre,magnitude
          + utilise Plan404 en dehors de 1900-2100

*     ----------------
*     PROGRAM SERIES96   BDL-GF9612
*     ----------------
*
*     This program is an example of use of the subroutine SERIES
*     and the files of planetary series (96) in the computation of the
*     astrometric coordinates of a planet (equinox and equator J2000).
*
*     If the user wants to improve the rapidity of computation, it is
*     recommended to transform the files in direct access and to read
*     the series in memory once for all in the subroutine SERIES.
*
*     The subroutine SERIES computes the heliocentric coordinates of
*     planets (equinox and equator J2000).
*     The subroutine EARTH is necessary for geocentric coordinates.
*}
var
      v1,v2: double6;
      w : array[1..3] of double;
      tjd,t,dt : double;
      i,k : integer;
const
      s0 : array[1..9] of double =(3.34,8.41,0,4.68,98.47,83.33,34.28,36.56,1.57);
      V0 : array[1..9] of double =(-0.42,-4.40,0,-1.52,-9.40,-8.80,-7.19,-6.87,-1.0);
      A0 : array[1..9] of double =(0.11,0.65,0,0.15,0.52,0.47,0.51,0.41,0.3);
//      V0 : array[1..9] of double =(1.16,-4.0,0,-1.3,-8.93,-8.68,-6.85,-7.05,-1.0);

var  p :TPlanetData;
     lt,bt,rt,lp,bp,rp,l,b,x,y,z,ce,se : double;

begin
ierr:=1;
if (ipla<1) or (ipla=3) or (ipla>9) then exit;
{*
*     ----------
*     Test dates
*     ----------
*}
if UseSeries96 and (t0>t1) and (t0<t2) then begin   // use SERIES96
         tjd:=t0;
         SolRect(tjd,false,v1[1],v1[2],v1[3],ierr);
         dt:=sqrt(v1[1]*v1[1]+v1[2]*v1[2]+v1[3]*v1[3]);
         Plan96 (tjd,ipla,false,addr(v2),ierr);
         if (ierr<>0) then exit;
         for i:=1 to 3 do begin
            w[i]:=v2[i]+v1[i];
         end;
         distance:=sqrt(w[1]*w[1]+w[2]*w[2]+w[3]*w[3]);
         dp:=sqrt(v2[1]*v2[1]+v2[2]*v2[2]+v2[3]*v2[3]);
         t:=tjd-distance*tlight;
         Plan96 (t,ipla,false,addr(v2),ierr);
         if (ierr<>0) then exit;
         for i:=1 to 3 do begin
            w[i]:=v2[i]+v1[i];
         end;
         alpha:=arctan2(w[2],w[1]);
         if (alpha<0) then alpha:=alpha+2*pi;
         delta:=arctan(w[3]/sqrt(w[1]*w[1]+w[2]*w[2]));
end else begin               // use Plan404
     // position de la terre
     p.ipla:=3;
     p.JD:=t0-tlight;
     Plan404(addr(p));
     lt:=p.l; bt:=p.b; rt:=p.r;
     dt:=rt;
     // position de la planete
     p.ipla:=ipla;
     p.JD:=t0;
     Plan404(addr(p));
     lp:=p.l; bp:=p.b; rp:=p.r;
     dp:=rp;
     x:=rp*cos(bp)*cos(lp) - rt*cos(bt)*cos(lt);
     y:=rp*cos(bp)*sin(lp) - rt*cos(bt)*sin(lt);
     z:=rp*sin(bp) - rt*sin(bt);
     distance:=sqrt(x*x+y*y+z*z);
     // position avec temps de lumiere
     p.ipla:=ipla;
     p.JD:=t0-distance*tlight;
     Plan404(addr(p));
     lp:=p.l; bp:=p.b; rp:=p.r;
     x:=rp*cos(bp)*cos(lp) - rt*cos(bt)*cos(lt);
     y:=rp*cos(bp)*sin(lp) - rt*cos(bt)*sin(lt);
     z:=rp*sin(bp) - rt*sin(bt);
     // ne pas recalculer la distance
     // coordonnees geocentrique
     l:=arctan2(y,x);
     b:=arctan(z/sqrt(x*x+y*y));
     ce:=cos(degtorad(eps2000));
     se:=sin(degtorad(eps2000));
     alpha:=arctan2(sin(l)*ce-tan(b)*se , cos(l) );
     delta:=arcsin(sin(b)*ce+cos(b)*se*sin(l) );
end;
{
  calcul de la fraction illumin�e
}
//         illum:=(power(dp+distance,2)-dt*dt)/(4*dp*distance);
//         phase:=rmod(radtodeg(arccos(2*illum-1))+360,360);
           phase:=(dp*dp+distance*distance-dt*dt)/(2*dp*distance); //cos(phase)
           illum:=(1+phase)/2;
           phase:=radtodeg(arccos(phase));
{
  calcul du diam�tre
}
         diameter:=2*s0[ipla]/distance;
{
  Calcul de la magnitude
}
magn:=V0[ipla]+5*log10(dp*distance); {meeus91 40. }
case ipla of
    1 : magn:=magn+phase*(0.0380+phase*(-0.000273+phase*2e-6));
    2 : magn:=magn+phase*(0.0009+phase*(0.000239+phase*6.5e-7));
    4 : magn:=magn+0.016*phase;
//    5 : magn:=magn+0.005*phase;
//    6 : magn:=magn+0.044*phase;
end;
{case ipla of
     1 : magn:=magn+0.02838*(phase-50)+0.0001023*(phase-50)*(phase-50);
    2 : magn:=magn+phase*(0.01322+phase*0.0000004247);
    4 : magn:=magn+0.01486*phase;
    6 : magn:=magn+0.044*abs(phase);
end;}
{*
*     ---
*     End
*     ---
*}
alpha:=rmod(radtodeg(alpha)/15+24,24);
delta:=radtodeg(delta);
ierr:=0;
end;

Procedure SolRect(t0 : double ; astrometric : boolean; var x,y,z : double;var ierr :integer);
var p :TPlanetData;
    v,v2 : double6;
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
if UseSeries96 and (t0>t1) and (t0<t2) then begin   // use SERIES96
  Plan96 (tjd,3,false,addr(v),ierr);
  if (ierr<>0) then exit;
  EARTH96 (tjd,addr(v2));
  for i:=1 to 3 do begin
    v[i]:=v2[i]-v[i];
  end;
  x:=v[1]; y:=v[2]; z:=v[3];
end else begin               // use Plan404
     p.ipla:=3;
     p.JD:=t0-tlight;
     ierr:=Plan404(addr(p));
     if (ierr<>0) then exit;
     x:=-p.x; y:=-p.y; z:=-p.z;
end;
Solastrometric:=astrometric;
SolT0:=t0;
XSol:=x;
YSol:=y;
ZSol:=z;
end;
end;

Procedure Soleil(t0 : double; var alpha,delta,dist,diam : double;var ierr :integer);
var x,y,z : double;
begin
         SolRect(t0,true,x,y,z,ierr);
         dist:=sqrt(x*x+y*y+z*z);
         alpha:=arctan2(y,x);
         if (alpha<0) then alpha:=alpha+2*pi;
         delta:=arctan(z/sqrt(x*x+y*y));
         alpha:=radtodeg(alpha)/15;
         delta:=radtodeg(delta);
         diam:=2*959.63/dist;
end;

function to360(x:double):double;
begin
result:=rmod(x+3600000000,360);
end;

Procedure JupSatInt(jde : double;var P : double; var xsat,ysat : array of double; var supconj : array of boolean);
var pl :TPlanetData;
    d,V1,M1,N1,J1,A1,B1,K1,Re,Rj,pha : double;
    d2,T0,T1,A0,D0,WW1,WW2,l0,b0,r0,l,b,r,x,y,z,del,eps,ceps,seps,alps,dels,DS,u,v,aa,dd,k,DE,w1,w2 : double;
    u1,u2,u3,u4,G,H,r1,r2,r3,r4,sDe : double;
begin
//  meeus 42.low
d := jde - 2451545.0;
V1 := to360(172.74 + 0.00111588 * d);
M1 := to360(357.529 + 0.9856003 * d);
N1 := to360(20.020 + 0.0830853 * d + 0.329 * sin(degtorad(V1)));
J1 := to360(66.115 + 0.9025179 * d - 0.329 * sin(degtorad(V1)));
A1 := to360(1.915 * sin(degtorad(M1)) + 0.020 * sin(degtorad(2*M1)));
B1 := to360(5.555 * sin(degtorad(N1)) + 0.168 * sin(degtorad(2*N1)));
K1 := J1 + A1 - B1;
Re := 1.00014 - 0.01671 * cos(degtorad(M1)) - 0.00014 * cos(degtorad(2*M1));
Rj := 5.20872 - 0.25208 * cos(degtorad(N1)) - 0.00611 * cos(degtorad(2*N1));
del := sqrt(Rj*Rj + Re*Re - 2*Rj*Re * cos(degtorad(K1)));
pha := radtodeg(arcsin(Re * sin(degtorad(K1)) / del));
//  meeus 42.
d2 := jde - 2433282.5;
T1 := d2/36525;
T0 := (jde - 2451545.0)/36525;
A0 := 268.00 + 0.1061 * T1;
D0 := 64.50 - 0.0164 * T1;
WW1 := to360(17.710 + 877.90003539 * d2);
WW2 := to360(16.838 + 870.27003539 * d2);
pl.ipla:=3;
pl.JD:=jde;
Plan404(addr(pl));
l0:=pl.l; b0:=pl.b; r0:=pl.r;
pl.ipla:=5;
pl.JD:=jde;
Plan404(addr(pl));
l:=pl.l; b:=pl.b; r:=pl.r;
x := r * cos(b) * cos(l) - r0 * cos(l0);
y := r * cos(b) * sin(l) - r0 * sin(l0);
z := r * sin(b) - r0 * sin(b0);
del := sqrt( x*x + y*y + z*z);
l := l - degtorad(0.012990 * del / (r*r) );
x := r * cos(b) * cos(l) - r0 * cos(l0);
y := r * cos(b) * sin(l) - r0 * sin(l0);
z := r * sin(b) - r0 * sin(b0);
del := sqrt( x*x + y*y + z*z);
eps := 23.439291111 - 0.0130042 * T0 - 1.64e-7 * T0*T0 + 5.036e-7 *T0*T0*T0;
ceps := cos(degtorad(eps));
seps := sin(degtorad(eps));
AlpS := radtodeg(arctan2(ceps*sin(l)-seps*tan(b),cos(l)));
DelS := radtodeg(arcsin(ceps*sin(b)+seps*cos(b)*sin(l)));
DS := radtodeg(arcsin(-sin(degtorad(D0))*sin(degtorad(DelS))-cos(degtorad(D0))*cos(degtorad(DelS))*cos(degtorad(A0-AlpS))));
u := y * ceps - z * seps;
v := y * seps + z * ceps;
aa := radtodeg(arctan2(u,x));
dd := radtodeg(arctan(v/sqrt(x*x+u*u)));
k := radtodeg(arctan2(sin(degtorad(D0))*cos(degtorad(dd))*cos(degtorad(A0-aa))-sin(degtorad(dd))*cos(degtorad(D0)),cos(degtorad(dd))*sin(degtorad(A0-aa))));
DE := radtodeg(arcsin(-sin(degtorad(D0))*sin(degtorad(dd))-cos(degtorad(d0))*cos(degtorad(dd))*cos(degtorad(A0-aa))));
w1 := to360(WW1 - k - 5.07033 * del);
w2 := to360(WW2 - k - 5.02626 * del);
P := radtodeg(arctan2(cos(degtorad(D0))*sin(degtorad(A0-aa)),sin(degtorad(D0))*cos(degtorad(dd))-cos(degtorad(D0))*sin(degtorad(dd))*cos(degtorad(A0-aa))));
// meeus 43.low
u1 := to360(163.8067 + 203.4058643 * (d-del/173) + pha - B1);
u2 := to360(358.4108 + 101.2916334 * (d-del/173) + pha - B1);
u3 := to360(5.7129 + 50.2345179 * (d-del/173) + pha - B1);
u4 := to360(224.8151 + 21.4879801 * (d-del/173) + pha - B1);
G := to360(331.18 + 50.310482 * (d -del/173));
H := to360(87.40 + 21.569231 * (d -del/173));
r1 := 5.9073 - 0.0244 * cos(degtorad(2*(u1-u2)));
r2 := 9.3991 - 0.0882 * cos(degtorad(2*(u2-u3)));
r3 := 14.9924 - 0.0216 * cos(degtorad(G));
r4 := 26.3699 - 0.1935 * cos(degtorad(H));
u1 := degtorad(u1 + 0.473 * sin(degtorad(2*(u1-u2))));
u2 := degtorad(u2 + 1.0653 * sin(degtorad(2*(u2-u3))));
u3 := degtorad(u3 + 0.165 * sin(degtorad(G)));
u4 := degtorad(u4 + 0.841 * sin(degtorad(H)));
sDe:=sin(degtorad(De));
xsat[0] := r1 * sin(u1);
ysat[0] := -r1 * cos(u1)*sDe;
xsat[1] := r2 * sin(u2);
ysat[1] := -r2 * cos(u2)*sDe;
xsat[2] := r3 * sin(u3);
ysat[2] := -r3 * cos(u3)*sDe;
xsat[3] := r4 * sin(u4);
ysat[3] := -r4 * cos(u4)*sDe;
supconj[0] := (u1>(pi/2))and(u1<(3*pi/2));
supconj[1] := (u2>(pi/2))and(u2<(3*pi/2));
supconj[2] := (u3>(pi/2))and(u3<(3*pi/2));
supconj[3] := (u4>(pi/2))and(u4<(3*pi/2));
end;

Function JupSat(jde,diam : double; var xsat,ysat : double8; var supconj : array of boolean):integer;
var i : integer;
    sp,cp,xs,ys,P : double;
    x2,y2 : double8;
begin
if not satxyok then result:=1
               else result:=satxyfm(jde,5,addr(xsat),addr(ysat));
if result>0 then begin
   jupsatInt(jde,P,xsat,ysat,supconj);
   sp:=sin(degtorad(P));
   cp:=cos(degtorad(P));
   for i:=1 to 4 do begin
       xs:=xsat[i]*diam/2;
       ys:=ysat[i]*diam/2;
       xsat[i]:=-xs*cp+ys*sp;
       ysat[i]:=+xs*sp+ys*cp;
   end;
   result:=0;
end else begin
   satxyfm(jde+0.02,5,addr(x2),addr(y2));
   for i:=1 to 4 do begin
   supconj[i-1] := xsat[i]<x2[i];
   end;
end;
end;

Function SatSat(jde,diam : double; var xsat,ysat : double8; var supconj : array of boolean):integer;
var i : integer;
    x2,y2 : double8;
begin
if not satxyok then begin result:=1; exit; end;
result:=satxyfm(jde,6,addr(xsat),addr(ysat));
if result=0 then begin
satxyfm(jde+0.02,6,addr(x2),addr(y2));
for i:=1 to 8 do begin
  supconj[i-1] := xsat[i]<x2[i];
end;
end;
end;

Function UraSat(jde,diam : double; var xsat,ysat : double8; var supconj : array of boolean):integer;
var i : integer;
    x2,y2 : double8;
begin
if not satxyok then begin result:=1; exit; end;
result:=satxyfm(jde,7,addr(xsat),addr(ysat));
if result=0 then begin
satxyfm(jde+0.02,7,addr(x2),addr(y2));
for i:=1 to 5 do begin
  supconj[i-1] := xsat[i]<x2[i];
end;
end;
end;

Function MarSat(jde,diam : double; var xsat,ysat : double8; var supconj : array of boolean):integer;
var i : integer;
    x2,y2 : double8;
begin
if not satxyok then begin result:=1; exit; end;
result:=satxyfm(jde,4,addr(xsat),addr(ysat));
if result=0 then begin
satxyfm(jde+0.02,4,addr(x2),addr(y2));
for i:=1 to 2 do begin
  supconj[i-1] := xsat[i]<x2[i];
end;
end;
end;

Procedure PlanetOrientation(jde:double; ipla:integer; var P,De,Ds,w1,w2,w3 : double);
const VP : array[1..10,1..4] of double = (
          (281.01,-0.033,61.54,-0.005),   //mercure
          (272.6,0,67.16,0),              //venus
          (0,-0.641,90,-0.557),           //terre
          (317.681,-0.108,52.886,-0.061), //mars
          (268.05,-0.009,64.49,0.003),    //jupiter
          (40.589,-0.036,83.537,-0.004),  //saturn
          (257.311,0,-15.175,0),          //uranus
          (299.36,0.70,43.46,-0.51),      //neptune !
          (313.02,0,9.09,0),              //pluto
          (286.13,0,63.87,0));            //sun
      W : array[1..10,1..2] of double =(
          (329.68,6.1385025),
          (160.20,-1.4813688),
          (190.16,360.9856235),
          (176.901,350.8919830),
          (67.1,877.90003539),
          (38.90,810.7939024),
          (203.81,-501.1600928),
          (253.18,536.3128492),
          (236.77,-56.3623195),
          (84.10,14.1844000));
var d,T,N,a0,d0,l0,b0,r0,l1,b1,r1,x,y,z,del,eps,als,des,u,v,al,dl,f,th,k,i : double;
    pl :TPlanetData;
begin
d := (jde-jd2000);
T := d/36525;
if ipla=10 then begin  // sun
  th:=(jde-2398220)*360/25.38;
  i:=degtorad(7.25);
  k:=degtorad(73.6667+1.3958333*(jde-2396758)/36525);
  pl.ipla:=3;
  pl.JD:=jde;
  Plan404(addr(pl));
  PrecessionEcl(jd2000,jde,pl.l,pl.b);
  l0:=pl.l+pi;
  eps := degtorad(23.439291111 - 0.0130042 * T - 1.64e-7 * T*T + 5.036e-7 *T*T*T);
  x:=arctan(-cos(l0)*tan(eps));
  y:=arctan(-cos(l0-k)*tan(i));
  P:=radtodeg(x+y);
  De:=radtodeg(arcsin(sin(l0-k)*sin(i)));
  n:=arctan2(-sin(l0-k)*cos(i),-cos(l0-k));
  w1:=to360(radtodeg(n)-th);
end else begin
if ipla=8 then N:=sin(357.85+52.316*T)
          else N:=T;
a0:=(VP[ipla,1]+VP[ipla,2]*N)/15;
if ipla=8 then N:=cos(357.85+52.316*T)
          else N:=T;
d0:=VP[ipla,3]+VP[ipla,4]*N;
precession(jd2000,jde,a0,d0);
a0:=degtorad(15*a0);
d0:=degtorad(d0);
w1:=W[ipla,1]+W[ipla,2]*d;
if ipla=5 then begin
   w2:=43.3+870.27003539*d;
   w3:=284.95+870.5360000*d;
end else begin
   w2:=-999;
   w3:=-999;
end;
pl.ipla:=3;
pl.JD:=jde;
Plan404(addr(pl));
PrecessionEcl(jd2000,jde,pl.l,pl.b);
l0:=pl.l; b0:=pl.b; r0:=pl.r;
pl.ipla:=ipla;
pl.JD:=jde;
Plan404(addr(pl));
PrecessionEcl(jd2000,jde,pl.l,pl.b);
l1:=pl.l; b1:=pl.b; r1:=pl.r;
x := r1 * cos(b1) * cos(l1) - r0 * cos(l0);
y := r1 * cos(b1) * sin(l1) - r0 * sin(l0);
z := r1 * sin(b1) - r0 * sin(b0);
del := sqrt( x*x + y*y + z*z);
pl.ipla:=ipla;
pl.JD:=jde-del*tlight;
Plan404(addr(pl));
PrecessionEcl(jd2000,jde,pl.l,pl.b);
l1:=pl.l; b1:=pl.b; r1:=pl.r;
x := r1 * cos(b1) * cos(l1) - r0 * cos(l0);
y := r1 * cos(b1) * sin(l1) - r0 * sin(l0);
z := r1 * sin(b1) - r0 * sin(b0);
del := sqrt( x*x + y*y + z*z);
eps := degtorad(23.439291111 - 0.0130042 * T - 1.64e-7 * T*T + 5.036e-7 *T*T*T);
als:=arctan2(cos(eps)*sin(l1)-sin(eps)*tan(b1),cos(l1));
des:=arcsin(cos(eps)*sin(b1)+sin(eps)*cos(b1)*sin(l1));
Ds:=radtodeg(arcsin(-sin(d0)*sin(des)-cos(d0)*cos(des)*cos(a0-als)));
u:=y*cos(eps)-z*sin(eps);
v:=y*sin(eps)+z*cos(eps);
al:=arctan2(u,x);
dl:=arctan(v/sqrt(x*x+u*u));
f:=radtodeg(arctan2(sin(d0)*cos(dl)*cos(a0-al)-sin(dl)*cos(d0),cos(dl)*sin(a0-al)));
De:=radtodeg(arcsin(-sin(d0)*sin(dl)-cos(d0)*cos(dl)*cos(a0-al)));
w1:=to360(w1-f-del*tlight*W[ipla,2]);
if ipla=5 then begin
   w2:=to360(w2-f-del*tlight*870.27003539);
   w3:=to360(w3-f-del*tlight*870.5360000);
end;
P:=to360(radtodeg(arctan2(cos(d0)*sin(a0-al),sin(d0)*cos(dl)-cos(d0)*sin(dl)*cos(a0-al))));
end;
end;

Procedure MoonOrientation(jde,ra,dec,d:double; var P,llat,lats,llong : double);
var lp,l,b,f,om,w,T,a,i,lh,bh,e,v,x,y,l0 {,cel,sel,asol,dsol} : double;
    pl :TPlanetData;
begin
T := (jde-2451545)/36525;
e:=23.4392911;
eq2ecl(ra,dec,e,lp,b);
lp:=deg2rad*lp;
b:=deg2rad*b;
PrecessionEcl(jd2000,jde,lp,b);
lp:=rad2deg*lp;
b:=rad2deg*b;
F:=93.2720993+483202.0175273*t-0.0034029*t*t-t*t*t/3526000+t*t*t*t/863310000;
om:=125.0445550-1934.1361849*t+0.0020762*t*t+t*t*t/467410-t*t*t*t/60616000;
w:=degtorad(lp-om);
i:=degtorad(1.54242);
l:=degtorad(lp);
b:=degtorad(b);
a:=radtodeg(arctan2(sin(w)*cos(b)*cos(i)-sin(b)*sin(i),cos(w)*cos(b)));
llong:=to360(a-F);
if llong>180 then llong:=llong-360;
llat:=arcsin(-sin(w)*cos(b)*sin(i)-sin(b)*cos(i));
pl.ipla:=3;
pl.JD:=jde-tlight;
Plan404(addr(pl));
{
//cel:=cos(b)*cos(l-pl.l);
//sel:=sqrt(1-cel*cel);
asol:=arctan2(-pl.y,-pl.x);
dsol:=arctan(-pl.z/sqrt(pl.x*pl.x+pl.y*pl.y));
cel:=sin(dsol)*sin(deg2rad*dec)+cos(dsol)*cos(deg2rad*dec)*cos(asol-deg2rad*15*ra);
sel:=sqrt(1-cel*cel);
phase:=rad2deg*arctan2(pl.r*sel, d-pl.r*cel);
lp:=rmod(lp+360,360);
lp:=rmod(pl.l-lp+360,360);
if (lp>0)and(lp<=180) then phase:=rmod(720-phase,360);
}
PrecessionEcl(jd2000,jde,pl.l,pl.b);
l0:=radtodeg(pl.l)-180;
lh:=l0+180+(d/pl.r)*57.296*cos(b)*sin(pl.l-pi-l);
bh:=(d/pl.r)*pl.b;
w:=degtorad(lh-om);
lats:=radtodeg(arcsin(-sin(w)*cos(bh)*sin(i)-sin(bh)*cos(i)));
e:=degtorad(e);
v:=degtorad(om);
x:=sin(i)*sin(v);
y:=sin(i)*cos(v)*cos(e)-cos(i)*sin(e);
w:=arctan2(x,y);
P:=radtodeg(arcsin(sqrt(x*x+y*y)*cos(degtorad(ra*15)-w)/cos(llat)));
llat:=radtodeg(llat);
end;

Procedure SatRing(jde : double; var P,a,b,be : double);
var T,i,om,l0,b0,r0,l1,b1,r1,x,y,z,del,lam,bet,sinB,eps,ceps,seps,lam0,bet0,al,de,al0,de0 : double;
    pl :TPlanetData;
begin
{ meeus 44. }
T := (jde-2451545)/36525;
i := 28.075216 - 0.012998 * T + 0.000004 *T*T;
om := 169.508470 + 1.394681 * T + 0.000412 *T*T;
pl.ipla:=3;
pl.JD:=jde-9*0.0057755183; // aprox. light time
Plan404(addr(pl));
l0:=pl.l; b0:=pl.b; r0:=pl.r;
pl.ipla:=6;
pl.JD:=jde-9*0.0057755183; // aprox. light time
Plan404(addr(pl));
l1:=pl.l; b1:=pl.b; r1:=pl.r;
x := r1 * cos(b1) * cos(l1) - r0 * cos(l0);
y := r1 * cos(b1) * sin(l1) - r0 * sin(l0);
z := r1 * sin(b1) - r0 * sin(b0);
del := sqrt( x*x + y*y + z*z);
lam:=radtodeg(arctan2(y,x));
bet:=radtodeg(arctan(z/sqrt(x*x+y*y)));
sinB := sin(degtorad(i))*cos(degtorad(bet))*sin(degtorad(lam-om))-cos(degtorad(i))*sin(degtorad(bet));
be:=radtodeg(arcsin(sinB));
a:=375.35/del;
b:=a*abs(sinB);
eps := 23.439291111 - 0.0130042 * T - 1.64e-7 * T*T + 5.036e-7 *T*T*T;
ceps := cos(degtorad(eps));
seps := sin(degtorad(eps));
lam0 := degtorad(om - 90);
bet0 := degtorad(90 - i);
lam:=degtorad(lam);
bet:=degtorad(bet);
al := (arctan2(ceps*sin(lam)-seps*tan(bet),cos(lam)));
de := (arcsin(ceps*sin(bet)+seps*cos(bet)*sin(lam)));
al0 := (arctan2(ceps*sin(lam0)-seps*tan(bet0),cos(lam0)));
de0 := (arcsin(ceps*sin(bet0)+seps*cos(bet0)*sin(lam0)));
P := to360(90+radtodeg(arctan2(cos(de0)*sin(al0-al),sin(de0)*cos(de)-cos(de0)*sin(de)*cos(al0-al))));
end;

Function MoonMag(phase:double):double;
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

Function MoonPhase(k: double):double;
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

Procedure MoonPhases(year:double; var nm,fq,fm,lq : double);
var k : double;
begin
k := (year - 2000) * 12.3685;
k := floor(k);
nm := MoonPhase(k);
fq := MoonPhase(k+0.25);
fm := MoonPhase(k+0.50);
lq := MoonPhase(k+0.75);
end;

end.
