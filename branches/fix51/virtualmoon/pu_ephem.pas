unit pu_ephem;

{$mode objfpc}{$H+}

interface

uses   u_translation, u_constant, u_util, cu_planet, u_projection, cu_tz, math,
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons, Grids, EditBtn, enhedits, IpHtml;

type

  { Tf_ephem }

  Tf_ephem = class(TForm)
    annee: TLongEdit;
    annee1: TLongEdit;
    Button1: TButton;
    Compute1: TButton;
    FileNameEdit1: TFileNameEdit;
    jour: TLongEdit;
    jour1: TLongEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label9: TLabel;
    step: TLongEdit;
    mois: TLongEdit;
    mois1: TLongEdit;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    UpDown3: TUpDown;
    UpDown4: TUpDown;
    UpDown5: TUpDown;
    UpDown6: TUpDown;
    procedure Compute1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Fplanet: TPlanet;
    tz: TCdCTimeZone;
    geocentric: boolean;
    procedure SetLang;
  end; 

var
  f_ephem: Tf_ephem;

implementation

{ Tf_ephem }


procedure Tf_ephem.FormCreate(Sender: TObject);
begin
  FileNameEdit1.FileName:=slash(privatedir)+'ephem.csv';
end;

procedure Tf_ephem.SetLang;
begin
  Caption:=rsSaveEphemeri;
  label9.Caption:=rsStartDate;
  label10.Caption:=rsEndDate;
  label1.Caption:=rsSteps;
  label2.Caption:=rsHours;
  Label3.Caption:=rst_1;
  Button1.Caption:=rst_99;
  Compute1.Caption:=rst_113;
end;

procedure Tf_ephem.Compute1Click(Sender: TObject);
var startjd,endjd,stepjd,curjd,ctime: double;
    y1,y2,m1,m2,d1,d2: integer;
    moonrise, moonset, moontransit, azimuthrise, azimuthset, eph: string;
    jd0, st0, q, cphase, colong, hh, az, ah: double;
    v1, v2, v3, v4, v5, v6, v7, v8, v9: double;
    gpa, glibrb, gsunincl, glibrl: double;
    aa, mm, dd, i, j: integer;
    ecl,nutl,nuto,sunl,sunb,abe,abp, ra, Dec, dist, dkm, diam, phase, illum : double;
    rad,ded, pa, librb, sunincl, librl,tphase, nmjd, fqjd, fmjd, lqjd,lunaison: double;
    CYear, CMonth, CDay,LastDay: integer;
    f: textfile;
    buf: string;
const sep='","';
      b = ' ';
  function  GetJDTimeZone(jdt: double): double;
  begin
      tz.JD := jdt;
      Result  := tz.SecondsOffset / 3600;
  end;
begin
  y1     :=annee.Value;
  m1     :=mois.Value;
  d1     :=jour.Value;
  timezone := GetJDTimeZone(jd(y1, m1, d1, 12));
  dt_ut := dtminusut(y1);
  startjd := jd(y1, m1, d1, 0 - timezone + DT_UT);
  y2     :=annee1.Value;
  m2     :=mois1.Value;
  d2     :=jour1.Value;
  timezone := GetJDTimeZone(jd(y2, m2, d2, 12));
  dt_ut := dtminusut(y2);
  endjd := jd(y2, m2, d2, 23 - timezone + DT_UT);
  stepjd:=step.Value/24;
  if (endjd>startjd)and(stepjd>0) then begin
  AssignFile(f,FileNameEdit1.FileName);
  rewrite(f);
  buf:='"'+rst_100;
  if geocentric then buf:=buf+sep+rst_26
     else buf:=buf+sep+formatfloat(f5,ObsLatitude)+sep+formatfloat(f5,ObsLongitude)+sep+ObsTZ;
  buf:=buf+'"';
  writeln(f,buf);
  buf:='"'+rsm_51+sep+rsm_51+' TT'+sep+rsm_51+' JD'+sep+'(J2000) '+rsm_29+sep+'(J2000) '+rsm_30+sep+
       '('+rsm_51+')'+b+rsm_29+sep+'('+rsm_51+')'+b+rsm_30+sep+rsm_31+b+rsm_18+sep+rsm_36+sep+rsm_48+sep+
       rsm_32+sep+rsm_46+sep+rsm_35+sep+rsm_45+sep+rsm_33+sep+rsm_34+sep+rsm_37+sep+rsm_73+sep+rsm_74+sep+
       rsm_38+sep+rsm_39+sep+rsm_40+sep+rsm_41+sep+rsm_55+sep+rsm_42+sep+rsEphemeris+'"';
  writeln(f,buf);
  LastDay:=-1;
  curjd:=startjd;
  repeat
    djd(curjd, aa, mm, dd, hh);
    dt_ut := dtminusut(aa);
    timezone := GetJDTimeZone(curjd);
    djd(curjd-(DT_UT-timezone)/24, CYear,CMonth,CDay, CTime);
    st0 := 0;
    ecl:=ecliptic(curjd);
    Fplanet.SetDE(jpldir);
    Fplanet.nutation(curjd,nutl,nuto);
    Fplanet.sunecl(curjd,sunl,sunb);
    PrecessionEcl(jd2000,curjd,sunl,sunb);
    aberration(curjd,abe,abp);
    eph:=Fplanet.Moon(curjd, ra, Dec, dist, dkm, diam, phase, illum);
    Fplanet.MoonOrientation(curjd, ra, Dec, dist, gpa, glibrb, gsunincl, glibrl);
    if not geocentric then
    begin
      jd0 := jd(CYear, CMonth, CDay, 0.0);
      st0 := SidTim(jd0, CTime - Timezone, ObsLongitude);
      Paralaxe(st0, dist, ra, Dec, ra, Dec, q, jd2000, curjd);
      diam := diam / q;
      dkm  := dkm * q;
      dist := dist * q;
    end;
    apparent_equatorial(ra,Dec,ecl,sunl,abp,abe,nutl,nuto,false);
    rad := ra;
    ded := Dec;
    mean_equatorial(ra,Dec,ecl,sunl,abp,abe,nutl,nuto);
    precession(jd2000, curjd, rad, ded);
    Fplanet.MoonOrientation(curjd, ra, Dec, dist, pa, librb, sunincl, librl);
    cphase := phase + glibrl;
    tphase := phase;
    colong := rmod(90 - tphase - glibrl + 360, 360);
    jd0    := jd(CYear, 1, 1, 0.0);
    Fplanet.MoonPhases(CYear + (curjd - jd0) / 365.25, nmjd, fqjd, fmjd, lqjd);
    lunaison := curjd - nmjd;
    if lunaison < 0 then
    begin
      lunaison := curjd - Fplanet.MoonPhase(floor(12.3685 *
        (CYear - 2000 - 0.04 + (curjd - jd0) / 365.25)));
    end;
    buf:='"';
    buf:=buf+date2str(cyear, cmonth, cday) + ' ' + timtostr(ctime);
    buf:=buf+sep+date2str(aa, mm, dd) + ' ' + timtostr(hh);
    buf:=buf+sep+formatfloat(f5,curjd);
    buf:=buf+sep+ formatfloat(f6,rad2deg * ra / 15);
    buf:=buf+sep+ formatfloat(f5,rad2deg * Dec);
    buf:=buf+sep+ formatfloat(f6,rad2deg * rad / 15);
    buf:=buf+sep+ formatfloat(f5,rad2deg * ded);
    buf:=buf+sep+ IntToStr(round(dkm));
    buf:=buf+sep+ formatfloat(f2, diam / 60);
    buf:=buf+sep+ formatfloat(f1, colong);
    buf:=buf+sep+ formatfloat(f1, phase);
    buf:=buf+sep+ formatfloat(f2, lunaison);
    buf:=buf+sep+ formatfloat(f1, illum * 100);
    buf:=buf+sep+ formatfloat(f1, sunincl);
    buf:=buf+sep+ formatfloat(f2,librb);
    buf:=buf+sep+ formatfloat(f2,librl);
    buf:=buf+sep+ formatfloat(f1, pa);
    if not geocentric then begin
      eq2hz(st0 - rad, ded, az, ah);
      az := rmod(rad2deg * az + 180, 360);
      buf:=buf+sep+formatfloat(f3,az);
      buf:=buf+sep+formatfloat(f2,rad2deg*ah);
      if CDay<>LastDay then begin
        LastDay:=CDay;
        Fplanet.PlanetRiseSet(11, jd(CYear, CMonth, CDay, 0),True, moonrise, moontransit, moonset, azimuthrise, azimuthset, v1, v2, v3, v4, v5, v6, v7, v8, v9, j);
        buf:=buf+sep+moonrise;
        buf:=buf+sep+moontransit;
        buf:=buf+sep+moonset;
        buf:=buf+sep+azimuthrise;
        if obslatitude > 0 then begin
          buf:=buf+sep+formatfloat(f0,90-obslatitude+rad2deg*Dec);
        end else begin
          buf:=buf+sep+formatfloat(f0,90+obslatitude-rad2deg*Dec);
        end;
        buf:=buf+sep+azimuthset;
      end else begin
         buf:=buf+sep+' '+sep+' '+sep+' '+sep+' '+sep+' '+sep+' ';
      end;
    end else begin
      buf:=buf+sep+' '+sep+' '+sep+' '+sep+' '+sep+' '+sep+' '+sep+' '+sep+' ';
    end;
    buf:=buf+sep+eph;
    buf:=buf+'"';
    writeln(f,buf);
    curjd:=curjd+stepjd;
  until curjd>endjd;
  closefile(f);
  ModalResult:=mrOK;
  end
  else ShowMessage('Invalid date range!');
end;

initialization
  {$I pu_ephem.lrs}

end.

