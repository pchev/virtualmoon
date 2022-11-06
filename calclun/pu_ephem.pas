unit pu_ephem;

{$mode objfpc}{$H+}

interface

uses   u_translation, u_constant, u_util, cspice, pas_spice, moon_spice, cu_tz,
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons, EditBtn, Spin;

type

  { Tf_ephem }

  Tf_ephem = class(TForm)
    annee1: TSpinEdit;
    Button1: TSpeedButton;
    Compute1: TSpeedButton;
    FileNameEdit1: TFileNameEdit;
    jour1: TSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    StatusLabel: TLabel;
    Label9: TLabel;
    annee: TSpinEdit;
    mois: TSpinEdit;
    jour: TSpinEdit;
    mois1: TSpinEdit;
    step: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Compute1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    tz: TCdCTimeZone;
    procedure SetLang;
  end; 

var
  f_ephem: Tf_ephem;

implementation

{$R pu_ephem.lfm}

{ Tf_ephem }


procedure Tf_ephem.FormCreate(Sender: TObject);
begin
  ScaleFormForFontSize(self,96);
  FileNameEdit1.FileName:=slash(Homedir)+'ephem.csv';
end;

procedure Tf_ephem.FormDestroy(Sender: TObject);
begin
end;

procedure Tf_ephem.SetLang;
begin
  Caption:=rsSaveEphemeri;
  label9.Caption:=rsStartDate;
  label10.Caption:=rsEndDate;
  label1.Caption:=rsSteps;
  label2.Caption:=rsHour;
  Label3.Caption:=rsFile;
  Button1.Caption:=rsCancel;
  Compute1.Caption:=rsCompute;
end;

procedure Tf_ephem.Compute1Click(Sender: TObject);
var startdt,enddt,st: double;
    et: SpiceDouble;
    x,y,z,r,ra,de,pa,llon,llat,slon,slat,colongitude: SpiceDouble;
    obsref,fixref: ConstSpiceChar;
    obspos: TDouble3;

    stepd,idt: extended;
    ctime,cjd: double;
    ctt: string;
    y1,y2,m1,m2,d1,d2: integer;
    az, ah: double;
    dkm, diam, phase, illum : double;
    rad,ded, nm, fq, fm, lq, lunation: double;
    CYear, CMonth, CDay: word;
    f: textfile;
    buf: string;
const sep='","';
      b = ' ';
  function  GetTimeZoneD(t: TDateTime): double;
  begin
      tz.Date := t;
      Result  := tz.SecondsOffset / 3600 / 24;
  end;
begin
  reset_c;
  if not ObservatoryPosition(ObsLongitude,ObsLatitude,ObsAltitude,obspos) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  y1     :=annee.Value;
  m1     :=mois.Value;
  d1     :=jour.Value;
  startdt := EncodeDate(y1, m1, d1);
  startdt := startdt-GetTimeZoneD(startdt); // UT
  y2     :=annee1.Value;
  m2     :=mois1.Value;
  d2     :=jour1.Value;
  enddt := EncodeDate(y2, m2, d2)+23/24;
  enddt := enddt-GetTimeZoneD(enddt); // UT
  stepd:=step.Value/24;
  if (enddt>startdt)and(stepd>0) then begin
    AssignFile(f,FileNameEdit1.FileName);
    rewrite(f);
    buf:='"'+rsObservatory;
    buf:=buf+sep+formatfloat(f5,ObsLatitude*rad2deg)+sep+formatfloat(f5,ObsLongitude*rad2deg)+sep+ObsTZ;
    buf:=buf+'"';
    writeln(f,buf);
    buf:='"'+rsDate+sep+rsDate+' TT'+sep+rsDate+' JD'+sep+rsRA2000+sep+rsDE2000+sep+
         rsRAApparent+sep+rsDEApparent+sep+rsDistance+b+'Km'+sep+rsDiameter+sep+
         rsPhase+sep+rsLunation+sep+rsIllumination+sep+rsColongitude+sep+rsSubSolarLati+sep+rsLibrationLat+sep+
         rsLibrationLon+sep+rsPA+sep+rsAzimut+sep+rsAltitude+sep+rsEphemeris+'"';
    writeln(f,buf);
    idt:=startdt;
    repeat
      st:=idt+GetTimeZoneD(idt);
      cjd:=DateTimetoJD(idt);
      DecodeDate(st,CYear,CMonth,CDay);
      ctime:=frac(st)*24;
      et:=DateTime2ET(idt);
      ctt:=ET2Str(et);
      // compute j2000 geocentric
      if not MoonTopocentric(et,false,obspos,obsref,x,y,z,dkm,ra,de) then begin
        StatusLabel.Caption:=SpiceLastError;
        exit;
      end;
      MoonPA(et,ra,de,pa);
      pa:=pa*rad2deg;
      ra:=ra*rad2deg/15;
      de:=de*rad2deg;
      diam:=60*rad2deg*arctan(2*Rmoon/dkm);
      // Apparent position
      if not MoonTopocentric(et,true,obspos,obsref,x,y,z,r,rad,ded) then begin
        StatusLabel.Caption:=SpiceLastError;
        exit;
      end;
      rad:=rad*rad2deg/15;
      ded:=ded*rad2deg;
      // Libration
      if not MoonSubObserverPoint(et,obspos,x,y,z,r,llon,llat) then begin
        StatusLabel.Caption:=SpiceLastError;
        exit;
      end;
      llon:=llon*rad2deg;
      llat:=llat*rad2deg;
      // Colongitude
      if not MoonSubSolarPoint(et,fixref,x,y,z,r,slon,slat,colongitude) then begin
         StatusLabel.Caption:=SpiceLastError;
         exit;
      end;
      // Alt/Az
      if not MoonAltAz(et,ObsLongitude,ObsLatitude,obspos,obsref,az,ah) then begin
        StatusLabel.Caption:=SpiceLastError;
        exit;
      end;
      az:=rmod(az*rad2deg+360, 360);
      ah:=ah*rad2deg;
      // Lunation
       if not MoonPhases(idt, nm, fq, fm, lq,lunation) then begin
         StatusLabel.Caption:=SpiceLastError;
         exit;
       end;
      // Illumination
      if not MoonPhase(et,phase) then begin
        StatusLabel.Caption:=SpiceLastError;
        exit;
      end;
      if (colongitude>pid2) and (colongitude<(3*pid2))then phase:=pi2-phase;
      illum:=(1+cos(phase))/2;
      phase:=phase*rad2deg;
      colongitude:=colongitude*rad2deg;
      slat:=slat*rad2deg;

      buf:='"';
      buf:=buf+date2str(cyear, cmonth, cday) + ' ' + timtostr(ctime);
      buf:=buf+sep+ctt;
      buf:=buf+sep+formatfloat(f5,cjd);
      buf:=buf+sep+ formatfloat(f6,ra);
      buf:=buf+sep+ formatfloat(f5,de);
      buf:=buf+sep+ formatfloat(f6,rad);
      buf:=buf+sep+ formatfloat(f5,ded);
      buf:=buf+sep+ formatfloat(f3,dkm);
      buf:=buf+sep+ formatfloat(f2, diam);
      buf:=buf+sep+ formatfloat(f3, phase);
      buf:=buf+sep+ formatfloat(f2, lunation);
      buf:=buf+sep+ formatfloat(f2, illum * 100);
      buf:=buf+sep+ formatfloat(f4, colongitude);
      buf:=buf+sep+ formatfloat(f4, slat);
      buf:=buf+sep+ formatfloat(f4,llat);
      buf:=buf+sep+ formatfloat(f4,llon);
      buf:=buf+sep+ formatfloat(f2, pa);
      buf:=buf+sep+formatfloat(f3,az);
      buf:=buf+sep+formatfloat(f3,ah);
      buf:=buf+sep+'CSPICE';
      buf:=buf+'"';
      writeln(f,buf);
      idt:=idt+stepd;
    until idt>enddt;
    closefile(f);
    ModalResult:=mrOK;
  end
  else ShowMessage('Invalid date range!');
end;

procedure Tf_ephem.Button1Click(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

end.

