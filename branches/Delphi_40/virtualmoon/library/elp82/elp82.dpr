library elp82;

uses Math, sysutils,
  elp in 'elp.pas';      

var tz,ObsLatitude,ObsLongitude,dt_ut :double;

function  Rmod(x,y:Double):Double;
BEGIN
    Rmod := x - Int(x/y) * y ;
END  ;

Procedure ELP82B(tjj,prec : double; var v : array of double; var ierr : integer);
{      subroutine ELP82B (tjj,prec,nulog,r,ierr)
*-----------------------------------------------------------------------

          Translation pour Delphi : P. Chevalley 31 mars 1998

*
*     Reference : Bureau des Logitudes - MCTJCGF9502.
*
*     Object :
*     Computation of geocentric lunar coordinates from ELP 2000-82 and
*     ELP2000-85 theories (M. Chapront-Touze and J. Chapront).
*     Constants fitted to JPL's ephemerides DE200/LE200.
*
*     Input :
*     tjj    julian date TDB (real double precision).
*     prec   truncation level in radian (real double precision).
*     nulog  number of logical unit for reading the files (integer).
*
*     Output :
*     r(3)   table of rectangular coordinates (real double precision).
*            reference frame : mean dynamical ecliptic and inertial
*            equinox of J2000 (JD 2451545.0).
*            r(1) : X (kilometer).
*            r(2) : Y (kilometer).
*            r(3) : Z (kilometer).
*     ierr   error index (integer).
*            ierr=0 : no error.
*            ierr=1 : error in elp 2000-82 files (end of file).
*            ierr=2 : error in elp 2000-82 files (reading error).
*
*     Remarks :
*     36 data files include the series related to various components of
*     the theory for the 3 spherical coordinates : longitude, latitude
*     and distance.
*     Files, series, constants and coordinate systems are described in
*     the notice LUNAR SOLUTION ELP 2000-82B.
*
*-----------------------------------------------------------------------
*
*     Declarations.
*
*      implicit double precision (a-h,o-z)
}

var  a : string; fic : string;
     w : array[1..3,1..5] of double;
     eart,peri,t : array[1..5] of double;
     p : array[1..8,1..2] of double;
     del : array[1..4,1..5] of double;
     zeta: array[1..2] of double;
     r,pre : array[1..3] of double;
{     coef : array[1..7] of double;
     ilu : array[1..4] of integer;
     ipla : array[1..11] of integer;}
     ideb,i,ific,iv,k : integer;
     cpi,cpi2,pis2,rad,deg,c1,c2,ath,a0,am,alfa,dtasm,preces : double;
     delnu,dele,delg,delnp,delep : double;
     p1,p2,p3,p4,p5,q1,q2,q3,q4,q5,tgv,y : double;
     x1,x2,x3,pw,qw,ra,pwqw,pw2,qw2 : double;
     fin : Boolean;

Procedure MainProblem;
var i,k,l,m : integer;
    x : double;
    lin : MainBin;
begin
{*
*     Main problem.
*}
iv := ((ific-1) mod 3)+1;
case ific of
     1 : m:= length(main_1);
     2 : m:= length(main_2);
     3 : m:= length(main_3);
else m:=0;
end;
l:=0;
repeat
      inc(l);
      case ific of
           1 : lin:= main_1[l];
           2 : lin:= main_2[l];
           3 : lin:= main_3[l];
      end;
      x:=lin.coef[1];
      if abs(x) < pre[iv] then continue;
      tgv:=lin.coef[2]+dtasm*lin.coef[6];
      if ific=3 then lin.coef[1]:=lin.coef[1]-2.0*lin.coef[1]*delnu/3.0;
      x:=lin.coef[1]+tgv*(delnp-am*delnu)+lin.coef[3]*delg+lin.coef[4]*dele+lin.coef[5]*delep;
      y:=0.0;
      for k:=1 to 5 do begin
        for i:=1 to 4 do begin
          y:=y+lin.ilu[i]*del[i,k]*t[k];
        end;
      end;
      if iv=3 then y:=y+pis2;
      y:=rmod(y,cpi2);
      r[iv]:=r[iv]+x*sin(y);
until l=m;
end;

Procedure FiguresTides;
var i,k,l,m : integer;
    lin : FigurBin;
begin
{*
*     Figures - Tides - Relativity - Solar eccentricity.
*}
iv:=((ific-1) mod 3)+1;
case ific of
     4 : m:= length(figur_4);
     5 : m:= length(figur_5);
     6 : m:= length(figur_6);
     7 : m:= length(figur_7);
     8 : m:= length(figur_8);
     9 : m:= length(figur_9);
     22 : m:= length(figur_22);
     23 : m:= length(figur_23);
     24 : m:= length(figur_24);
     25 : m:= length(figur_25);
     26 : m:= length(figur_26);
     27 : m:= length(figur_27);
     28 : m:= length(figur_28);
     29 : m:= length(figur_29);
     30 : m:= length(figur_30);
     31 : m:= length(figur_31);
     32 : m:= length(figur_32);
     33 : m:= length(figur_33);
     34 : m:= length(figur_34);
     35 : m:= length(figur_35);
     36 : m:= length(figur_36);
else m:=0;
end;
l:=0;
repeat
      inc(l);
      case ific of
           4 : lin:=figur_4[l];
           5 : lin:=figur_5[l];
           6 : lin:=figur_6[l];
           7 : lin:=figur_7[l];
           8 : lin:=figur_8[l];
           9 : lin:=figur_9[l];
           22 : lin:=figur_22[l];
           23 : lin:=figur_23[l];
           24 : lin:=figur_24[l];
           25 : lin:=figur_25[l];
           26 : lin:=figur_26[l];
           27 : lin:=figur_27[l];
           28 : lin:=figur_28[l];
           29 : lin:=figur_29[l];
           30 : lin:=figur_30[l];
           31 : lin:=figur_31[l];
           32 : lin:=figur_32[l];
           33 : lin:=figur_33[l];
           34 : lin:=figur_34[l];
           35 : lin:=figur_35[l];
           36 : lin:=figur_36[l];
      end;
      if abs(lin.x)<pre[iv] then continue;
      if (ific>=7) and (ific<=9) then  lin.x:=lin.x*t[2];
      if (ific>=25) and (ific<=27) then lin.x:=lin.x*t[2];
      if (ific>=34) and (ific<=36) then lin.x:=lin.x*t[3];
      y:=lin.pha*deg;
      for k:=1 to 2 do begin
        y:=y+lin.iz*zeta[k]*t[k];
        for i:=1 to 4 do begin
          y:=y+lin.ilu[i]*del[i,k]*t[k];
        end;
      end;
      y:=rmod(y,cpi2);
      r[iv]:=r[iv]+lin.x*sin(y);
until l=m;
end;

Procedure PlanetaryPerturbations;
var i,k,l,m : integer;
    lin : PlanPerBin;
begin
{*
*     Planetary perturbations.
*}
iv:=((ific-1) mod 3)+1;
case ific of
     10 : m:= length(planper_10);
     11 : m:= length(planper_11);
     12 : m:= length(planper_12);
     13 : m:= length(planper_13);
     14 : m:= length(planper_14);
     15 : m:= length(planper_15);
     16 : m:= length(planper_16);
     17 : m:= length(planper_17);
     18 : m:= length(planper_18);
     19 : m:= length(planper_19);
     20 : m:= length(planper_20);
     21 : m:= length(planper_21);
else m:=0;
end;
l:=0;
repeat
      inc(l);
      case ific of
           10 : lin:=planper_10[l];
           11 : lin:=planper_11[l];
           12 : lin:=planper_12[l];
           13 : lin:=planper_13[l];
           14 : lin:=planper_14[l];
           15 : lin:=planper_15[l];
           16 : lin:=planper_16[l];
           17 : lin:=planper_17[l];
           18 : lin:=planper_18[l];
           19 : lin:=planper_19[l];
           20 : lin:=planper_20[l];
           21 : lin:=planper_21[l];
      end;
      if abs(lin.x)<pre[iv] then continue;
      if (ific>=13) and (ific<=15) then lin.x:=lin.x*t[2];
      if (ific>=19) and (ific<=21) then lin.x:=lin.x*t[2];
      y:=lin.pha*deg;
      if (ific<16) then begin
        for k:=1 to 2 do begin
          y:=y+(lin.ipla[9]*del[1,k]+lin.ipla[10]*del[3,k]+lin.ipla[11]*del[4,k])*t[k];
          for i:=1 to 8 do begin
            y:=y+lin.ipla[i]*p[i,k]*t[k];
          end;
        end;
      end
      else begin
        for k:=1 to 2 do begin
          for i:=1 to 4 do begin
            y:=y+lin.ipla[i+7]*del[i,k]*t[k];
          end;
          for i:=1 to 7 do begin
            y:=y+lin.ipla[i]*p[i,k]*t[k];
          end;
        end;
      end;
      y:=rmod(y,cpi2);
      r[iv]:=r[iv]+lin.x*sin(y);
until l=m;
end;

begin
{*
*     Initialisation.
*}
      r[1]:=0.0;
      r[2]:=0.0;
      r[3]:=0.0;
         cpi:=3.141592653589793;
         cpi2:=2.0*cpi;
         pis2:=cpi/2.0;
         rad:=648000.0/cpi;
         deg:=cpi/180.0;
         c1:=60.0;
         c2:=3600.0;
         ath:=384747.9806743165;
         a0:=384747.9806448954;
         am:=0.074801329518;
         alfa:=0.002571881335;
         dtasm:=2.0*alfa/(3.0*am);
{*
*     Lunar arguments.
*}
         w[1,1]:=(218+18/c1+59.95571/c2)*deg;
         w[2,1]:=(83+21/c1+11.67475/c2)*deg;
         w[3,1]:=(125+2/c1+40.39816/c2)*deg;
         eart[1]:=(100+27/c1+59.22059/c2)*deg;
         peri[1]:=(102+56/c1+14.42753/c2)*deg;
         w[1,2]:=1732559343.73604/rad;
         w[2,2]:=14643420.2632/rad;
         w[3,2]:=-6967919.3622/rad;
         eart[2]:=129597742.2758/rad;
         peri[2]:=1161.2283/rad;
         w[1,3]:=-5.8883/rad;
         w[2,3]:=-38.2776/rad;
         w[3,3]:=6.3622/rad;
         eart[3]:=-0.0202/rad;
         peri[3]:=0.5327/rad;
         w[1,4]:=0.6604e-2/rad;
         w[2,4]:=-0.45047e-1/rad;
         w[3,4]:=0.7625e-2/rad;
         eart[4]:=0.9e-5/rad;
         peri[4]:=-0.138e-3/rad;
         w[1,5]:=-0.3169e-4/rad;
         w[2,5]:=0.21301e-3/rad;
         w[3,5]:=-0.3586e-4/rad;
         eart[5]:=0.15e-6/rad;
         peri[5]:=0.0;
{*
*     Planetary arguments.
*}
         preces:=5029.0966/rad;
         p[1,1]:=(252+15/c1+3.25986/c2)*deg;
         p[2,1]:=(181+58/c1+47.28305/c2)*deg;
         p[3,1]:=eart[1];
         p[4,1]:=(355+25/c1+59.78866/c2)*deg;
         p[5,1]:=(34+21/c1+5.34212/c2)*deg;
         p[6,1]:=(50+4/c1+38.89694/c2)*deg;
         p[7,1]:=(314+3/c1+18.01841/c2)*deg;
         p[8,1]:=(304+20/c1+55.19575/c2)*deg;
         p[1,2]:=538101628.68898/rad;
         p[2,2]:=210664136.43355/rad;
         p[3,2]:=eart[2];
         p[4,2]:=68905077.59284/rad;
         p[5,2]:=10925660.42861/rad;
         p[6,2]:=4399609.65932/rad;
         p[7,2]:=1542481.19393/rad;
         p[8,2]:=786550.32074/rad;
{*
*     Corrections of the constants (fit to DE200/LE200).
*}
         delnu:=+0.55604/rad/w[1,2];
         dele:=+0.01789/rad;
         delg:=-0.08066/rad;
         delnp:=-0.06424/rad/w[1,2];
         delep:=-0.12879/rad;
{*
*     Delaunay's arguments.
*}
         for i:=1 to 5 do begin
            del[1,i]:=w[1,i]-eart[i];
            del[4,i]:=w[1,i]-w[3,i];
            del[3,i]:=w[1,i]-w[2,i];
            del[2,i]:=eart[i]-peri[i];
         end;
         del[1,1]:=del[1,1]+cpi;
         zeta[1]:=w[1,1];
         zeta[2]:=w[1,2]+preces;
{*
*     Precession matrix.
*}
         p1:=0.10180391e-4;
         p2:=0.47020439e-6;
         p3:=-0.5417367e-9;
         p4:=-0.2507948e-11;
         p5:=0.463486e-14;
         q1:=-0.113469002e-3;
         q2:=0.12372674e-6;
         q3:=0.1265417e-8;
         q4:=-0.1371808e-11;
         q5:=-0.320334e-14;

         t[1]:=1.0;

      t[2]:=(tjj-2451545.0)/36525.0;
      t[3]:=t[2]*t[2];
      t[4]:=t[3]*t[2];
      t[5]:=t[4]*t[2];
      pre[1]:=prec*rad;
      pre[2]:=prec*rad;
      pre[3]:=prec*ath;
      ific:=1;
      ierr:=0;
{*
*     Distribution of files.
*}
fin:=false;
repeat
case ific of
      1..3  : MainProblem;
      4..9  : FiguresTides;
     10..21 : PlanetaryPerturbations;
     22..36 : FiguresTides;
     else     fin:=true;
end;
inc(ific);
until fin ;
{*
*     Change of coordinates.
*}
      r[1]:=r[1]/rad+w[1,1]+w[1,2]*t[2]+w[1,3]*t[3]+w[1,4]*t[4]+w[1,5]*t[5];
      r[2]:=r[2]/rad;
      r[3]:=r[3]*a0/ath;
      x1:=r[3]*cos(r[2]);
      x2:=x1*sin(r[1]);
      x1:=x1*cos(r[1]);
      x3:=r[3]*sin(r[2]);
      pw:=(p1+p2*t[2]+p3*t[3]+p4*t[4]+p5*t[5])*t[2];
      qw:=(q1+q2*t[2]+q3*t[3]+q4*t[4]+q5*t[5])*t[2];
      ra:=2.0*sqrt(1-pw*pw-qw*qw);
      pwqw:=2.0*pw*qw;
      pw2:=1-2.0*pw*pw;
      qw2:=1-2.0*qw*qw;
      pw:=pw*ra;
      qw:=qw*ra;
      r[1]:=pw2*x1+pwqw*x2+pw*x3;
      r[2]:=pwqw*x1+qw2*x2-qw*x3;
      r[3]:=-pw*x1+qw*x2+(pw2+qw2-1)*x3;
{
   coordonnées équatoriales
}
      v[0]:=r[1]+4.37913e-7*r[2]-1.89859e-7*r[3];
      v[1]:=-4.77299e-7*r[1]+0.917482137607*r[2]-0.397776981701*r[3];
      v[2]:=0.397776981701*r[2]+0.917482137607*r[3];
end;

Procedure Lune(t0 : double; var alpha,delta,dist,dkm,diam,phase,illum : double) stdcall;
{
	t0      :  date julienne TDB

	alpha   :  ascension droite J2000 en heure
	delta   :  declinaison J2000
	dist    :  distance terre-lune en UA
	dkm     :  distance terre-lune en km
	diam    :  diametre apparent en arcseconde
	phase   :  angle de phase
	illum	:  pourcentage illumine
}
var
   prec,p : double;
   w : array[1..3] of double;
   ierr : integer;
   t,sm,mm,md : double;
const km_au = 149597870.691 ;
begin
prec:=0;
ELP82B(t0,prec,w,ierr);
dkm:=sqrt(w[1]*w[1]+w[2]*w[2]+w[3]*w[3]);
alpha:=arctan2(w[2],w[1]);
if (alpha<0) then alpha:=alpha+2*pi;
p:=sqrt(w[1]*w[1]+w[2]*w[2]);
delta:=arctan(w[3]/p);
alpha:=radtodeg(alpha)/15;
delta:=radtodeg(delta);
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

Procedure LuneIncl(Lar,Lde,Sar,Sde : double; var incl : double) stdcall;
{
	Lar  :  ascension droite de la lune
	Lde  :  declinaison de la lune
	Sar  :  ascension droite du soleil
	Sde  :  declinaison du soleil

	incl :  angle de position du bord eclaire
}
begin
Lar:=degtorad(15*Lar);
Lde:=degtorad(Lde);
Sar:=degtorad(15*Sar);
Sde:=degtorad(Sde);
{meeus 46.5 }
incl:=arctan2(cos(Sde)*sin(Sar-Lar),cos(Lde)*sin(Sde)-sin(Lde)*cos(Sde)*cos(Sar-Lar) );
incl:=radtodeg(incl);
end;

//*****************************************************************//
//  Require functions for moon rise/set
//*****************************************************************//

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

function Jd(annee,mois,jour :INTEGER; Heure:double):double;
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
    jd:=int(365.25*(annee+4716))+int(30.6001*(mois+1))+jour+cor-1524.5 +heure/24;
END ;

function SidTim(jd0,ut,long : double): double;
 VAR t,te: double;
BEGIN
    t:=(jd0-2451545.0)/36525;
    te:=100.46061837 + 36000.770053608*t + 0.000387933*t*t - t*t*t/38710000;
    result :=  Rmod(te/15 - long/15 + 1.002737908*ut,24) ;
END ;

Function sgn(x:Double):Double ;
begin
if x<0 then
   sgn:= -1
else
   sgn:=  1 ;
end ;

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
              t:=(annee-948)/100;
              result:=(1830-405*t+46.5*t*t)/3600;
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

Function int3(n,y1,y2,y3 : double): double;
var a,b,c : double;
begin
a:= y2 - y1;
b:= y3 - y2;
c:= b - a;
result:= y2 + n/2*(a + b + n*c);
end;

//*****************************************************************//
//  Moon rise/set
//*****************************************************************//

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
longref:=-tz*15;
hs0 := sidtim(jd0,-tz,longref)*15 ;
chh0 :=(sin(degtorad(ho[typobj]))-sin(degtorad(ObsLatitude))*sin(degtorad(de2)))/(cos(degtorad(ObsLatitude))*cos(degtorad(de2))) ;
if abs(chh0)<=1 then begin
   hh0:=radtodeg(arccos(chh0));
   m0:=(ar2*15+Obslongitude-longref-hs0)/360;
   m1:=m0-hh0/360;
   m2:=m0+hh0/360;
   while m0<0 do m0:=m0+1;
   while m0>1 do m0:=m0-1;
   while m1<0 do m1:=m1+1;
   while m1>1 do m1:=m1-1;
   while m2<0 do m2:=m2+1;
   while m2>1 do m2:=m2-1;
   // lever
   hsg:= hs0 + 360.985647 * m1;
   n:= m1 ;
   aa:=int3(n,ar1,ar2,ar3)*15;
   dd:=int3(n,de1,de2,de3);
   hl:= hsg - Obslongitude + longref - aa;
   h:= radtodeg(arcsin(sin(degtorad(Obslatitude)) * sin(degtorad(dd)) + cos(degtorad(Obslatitude)) * cos(degtorad(dd)) * cos(degtorad(hl)) ));
   dm:= (h - ho[typobj]) / (360 * cos(degtorad(dd)) * cos(degtorad(Obslatitude)) * sin(degtorad(hl)) );
   hr:=(m1+dm)*24;
   a0:= radtodeg(arctan2(sin(degtorad(hh0)),cos(degtorad(hh0))*sin(degtorad(Obslatitude))-tan(degtorad(dd))*cos(degtorad(Obslatitude))));
   azr:=360-a0;
   // culmination
   hsg:= hs0 + 360.985647 * m0;
   n:= m0 ;
   aa:=int3(n,ar1,ar2,ar3)*15;
   hl:= hsg - Obslongitude + longref - aa;
   dm:= -(hl / 360);
   ht:=rmod((m0+dm)*24+24,24);
   if (ht<10)and(m0>0.6) then ht:=ht+24;
   if (ht>14)and(m0<0.4) then ht:=ht-24;
   // coucher
   hsg:= hs0 + 360.985647 * m2;
   n:= m2 ;
   aa:=int3(n,ar1,ar2,ar3)*15;
   dd:=int3(n,de1,de2,de3);
   hl:= hsg - Obslongitude + longref - aa;
   h:= radtodeg(arcsin(sin(degtorad(Obslatitude)) * sin(degtorad(dd)) + cos(degtorad(Obslatitude)) * cos(degtorad(dd)) * cos(degtorad(hl)) ));
   dm:= (h - ho[typobj]) / (360 * cos(degtorad(dd)) * cos(degtorad(Obslatitude)) * sin(degtorad(hl)) );
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
      ht:=rmod((m0+dm)*24+tz+24,24);
      irc:=1 ;
    end else begin
      ht:=0;      (* invisible *)
      irc:=2;
    end;
end;
end;

//CalcMoonRiseSet(Lat as double, Long as double, TimeZone as double,
//Year as integer, Month as integer, Day as integer, MoonRise as String,
//MoonSet as String, MoonTransit as String, AzimuthRise as String, AzimuthSet as String)

Procedure CalcMoonRiseSet(var Latitude,Longitude,TimeZone : double;
                          var Year,Month,Day : integer;
                          var moonrise,moonset,moontransit,azimuthrise,azimuthset : widestring); stdcall;
var jd0,am1,am2,am3,dm1,dm2,dm3,dm4,dm5,dm6,dm7,dm8,hr,ht,hs,azr,azs : double;
    i : integer;
const b5='     ';
      b6='      ';
      na='     ';
begin
ObsLatitude:=Latitude;
ObsLongitude:=Longitude;
tz:=-TimeZone;
dt_ut:=DTminusUT(Year);
jd0:=jd(Year,Month,Day,0);
Lune(jd0-tz/24-1,am1,dm1,dm4,dm5,dm6,dm7,dm8);
if (am1<0) then am1:=am1+24;
Lune(jd0-tz/24,am2,dm2,dm4,dm5,dm6,dm7,dm8);
if (am2<0) then am2:=am2+24;
Lune(jd0-tz/24+1,am3,dm3,dm4,dm5,dm6,dm7,dm8);
if (am3<0) then am3:=am3+24;
RiseSetInt(3,jd0,am1,dm1,am2,dm2,am3,dm3,hr,ht,hs,azr,azs,i);
case i of
0 : begin
    moonrise:=formatdatetime('hh:nn',hr/24);
    moonset:=formatdatetime('hh:nn',hs/24);
    moontransit:=formatdatetime('hh:nn',ht/24);
    azr:=rmod(azr+180,360);
    azs:=rmod(azs+180,360);
    str(azr:6:2,azimuthrise);
    str(azs:6:2,azimuthset);
    if hr<0 then begin moonrise:=b5; azimuthrise:=b6; end;  // d-1
    if hr>24 then begin moonrise:=b5; azimuthrise:=b6; end; //d+1
    if ht<0 then begin moontransit:=b5; end; // d-1
    if ht>24 then begin moontransit:=b5; end; //d+1
    if hs<0 then begin moonset:=b5; azimuthset:=b6; end; // d-1
    if hs>24 then begin moonset:=b5; azimuthset:=b6; end; //d+1
    end;
1 : begin
    moonrise:=na;
    moonset:=na;
    azimuthrise:=na;
    azimuthset:=na;
    moontransit:=formatdatetime('hh:nn',ht/24);
    if ht<0 then begin moontransit:=b5; end; // d-1
    if ht>24 then begin moontransit:=b5; end; //d+1
    end;
2 : begin
    moonrise:=na;
    moonset:=na;
    moontransit:=na;
    azimuthrise:=na;
    azimuthset:=na;
    end;
end;
end;

exports
       Lune index 1,
       LuneIncl index 2,
       CalcMoonRiseSet index 3;
begin
decimalseparator:='.';
end.
