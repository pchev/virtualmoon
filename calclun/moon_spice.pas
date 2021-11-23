unit moon_spice;

{$mode objfpc}{$H+}

interface

uses cspice, pas_spice, u_util, u_constant,
  Math, Classes, SysUtils;

function MoonSurfacePos(lon,lat:SpiceDouble): TDouble3;
function MoonPhase(et: SpiceDouble; out phase:SpiceDouble): Boolean;
function MoonPhases(startdate:TDateTime; out nm, fq, fm, lq: TDateTime; out lunation: double): Boolean;
function MoonGeocentric(et: SpiceDouble; trueequinox: boolean; out x,y,z,r,ra,de:SpiceDouble): Boolean;
function MoonPA(et,moonra,moonde: SpiceDouble; out pa:SpiceDouble): Boolean;
function MoonTopocentric(et: SpiceDouble; trueequinox: boolean; obspos:TDouble3; out obsref:ConstSpiceChar; out x,y,z,r,ra,de:SpiceDouble): Boolean;
function MoonAltAz(et,obslon,obslat: SpiceDouble; obspos:TDouble3; out obsref:ConstSpiceChar; out az,el:SpiceDouble): Boolean;
function MoonSubEarthPoint(et: SpiceDouble; out fixref:ConstSpiceChar; out x,y,z,r,lon,lat:SpiceDouble): Boolean;
function MoonSubObserverPoint(et: SpiceDouble; obspos:TDouble3; out x,y,z,r,lon,lat:SpiceDouble): Boolean;
function MoonSubSolarPoint(et: SpiceDouble; out fixref:ConstSpiceChar; out x,y,z,r,lon,lat,colongitude:SpiceDouble): Boolean;
function MoonSearchPhase(value: SpiceDouble; relate: ConstSpiceChar; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell): boolean;
function MoonSearchIllum(pos: TDouble3; value: SpiceDouble; relate: ConstSpiceChar; var fixref:ConstSpiceChar; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell): boolean;
function MoonSearchLibration(coord: ConstSpiceChar; value: SpiceDouble; relate: ConstSpiceChar; var fixref:ConstSpiceChar; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell):boolean;
function MoonSearchRiseSet(pos: TDouble3; obslat,el: SpiceDouble; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell): boolean;
function MoonSearchColongitude(value,delta: SpiceDouble; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell):boolean;
procedure Moon_udqdec(f:Tudfuns; et:SpiceDouble; out isdecr:SpiceBoolean); cdecl;
procedure MoonSunLongitude_udfuns(et:SpiceDouble; out value:SpiceDouble); cdecl;
function MoonSearchSunLongitudeDiff(refval: SpiceDouble; relate: ConstSpiceChar; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell):boolean; cdecl;
procedure MoonColongitude_test(f:Tudfuns; et:SpiceDouble; out xbool:SpiceBoolean); cdecl;
function SunTopocentric(et: SpiceDouble; trueequinox: boolean; obspos:TDouble3; out obsref:ConstSpiceChar; out x,y,z,r,ra,de:SpiceDouble): Boolean;
function SunSearchRiseSet(pos: TDouble3; obslat,el: SpiceDouble; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell): boolean;
function SearchNight(pos: TDouble3; obslat,el: SpiceDouble; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell): boolean;

implementation

var colongmin,colongmax: SpiceDouble;

function MoonSurfacePos(lon,lat:SpiceDouble): TDouble3;
var radii: TDouble3;
  n: SpiceInt;
  re,rp,f: SpiceDouble;
begin
  // position vector on moon
  reset_c;
  bodvrd_c ( 'MOON', 'RADII', 3, n, radii );
  re := radii[0];
  rp := radii[2];
  f  := ( re - rp ) / re;
  georec_c(lon, lat, 0, re,f, result);
end;

function MoonPhase(et: SpiceDouble; out phase:SpiceDouble): Boolean;
begin
  // compute the moon phase angle
  // beware there is no distinction between ascending and descending phase
  reset_c;
  phase:=phaseq_c(et,cnaifMoon,cnaifSun,cnaifEarth,abcorrLT);
  result:=not failed_c;
end;

function MoonGeocentric(et: SpiceDouble; trueequinox: boolean; out x,y,z,r,ra,de:SpiceDouble): Boolean;
var state: TDouble3;
    lt: SpiceDouble;
    abcorr,outref: ConstSpiceChar;
begin
  // compute geocentric moon position
  // return apparent position if trueequinox is True, J2000 otherwise
  reset_c;
  if trueequinox then begin
    outref := 'EARTH_TRUE_EQUATOR';
    abcorr := abcorrLTS;
  end
  else begin
    outref := 'J2000';
    abcorr := abcorrLT;
  end;
  spkezp_c ( naifMoon, et, outref, abcorr, naifEarth, state, @lt );
  result := not failed_c;
  if result then begin
    x:=state[0];
    y:=state[1];
    z:=state[2];
    recrad_c(state,r,ra,de);
  end;
end;

function MoonPA(et,moonra,moonde: SpiceDouble; out pa:SpiceDouble): Boolean;
// Compute J2000 Moon position angle
// moonra and moonde must be the Moon J2000 position at the same ET
var rotate:TArray3;
    mpos,jpos: TDouble3;
    r,polera,polede: SpiceDouble;
begin
  result:=false;
  reset_c;
  // find rotation matrix from Moon to J2000 frame at epoch ET
  pxform_c(fixrefME,frameJ2000,et,rotate);
  if failed_c then begin
    if SpiceLastErr='SPICE(FRAMEDATANOTFOUND)' then begin
      reset_c;
      pxform_c(fixrefIAU,frameJ2000,et,rotate);
      if failed_c then begin
        exit;
      end;
    end
    else begin
      exit;
    end;
  end;
  result := not failed_c;

  // vector to the Moon pole
  mpos[0]:=0;
  mpos[1]:=0;
  mpos[2]:=1;
  // rotate to J2000
  mxv_c(rotate,mpos,jpos);
  // convert to ra,dec
  recrad_c(jpos,r,polera,polede);
  // compute position angle
  pa := arctan2(cos(polede) * sin(polera - moonra), sin(polede) * cos(moonde) - cos(polede) * sin(moonde) * cos(polera - moonra));

end;

function MoonTopocentric(et: SpiceDouble; trueequinox: boolean; obspos:TDouble3; out obsref:ConstSpiceChar; out x,y,z,r,ra,de:SpiceDouble): Boolean;
var state: TDouble3;
    state1: TDouble6;
    target,outref,refloc,obsctr,abcorr: ConstSpiceChar;
    lt: SpiceDouble;
    i: integer;
begin
  // compute topocentric moon position view from obspos
  // return apparent position if trueequinox is True, J2000 otherwise
  reset_c;
  result:=false;
  refloc := 'OBSERVER';
  target := cnaifMoon;
  obsctr := 'EARTH';
  obsref := 'ITRF93';
  if trueequinox then begin
    outref := 'EARTH_TRUE_EQUATOR';
    abcorr := abcorrLTS;
  end
  else begin
    outref := 'J2000';
    abcorr := abcorrLT;
  end;
  spkcpo_c (target,et,outref,refloc,abcorr,obspos,obsctr,obsref,state1, @lt);
  if failed_c then begin
    if SpiceLastErr='SPICE(FRAMEDATANOTFOUND)' then begin
      // out of ITRF93 date range, retry with buildin IAU_EARTH
      reset_c;
      obsref := 'IAU_EARTH';
      spkcpo_c (target,et,outref,refloc,abcorr,obspos,obsctr,obsref,state1, @lt);
      if failed_c then
        exit;
    end
    else
      exit;
  end;
  result := not failed_c;
  if result then begin
    for i:=0 to 2 do state[i]:=state1[i];
    x:=state[0];
    y:=state[1];
    z:=state[2];
    recrad_c(state,r,ra,de);
  end;
end;

function SunTopocentric(et: SpiceDouble; trueequinox: boolean; obspos:TDouble3; out obsref:ConstSpiceChar; out x,y,z,r,ra,de:SpiceDouble): Boolean;
var state: TDouble3;
    state1: TDouble6;
    target,outref,refloc,obsctr,abcorr: ConstSpiceChar;
    lt: SpiceDouble;
    i: integer;
begin
  // compute topocentric sun position view from obspos
  // return apparent position if trueequinox is True, J2000 otherwise
  reset_c;
  result:=false;
  refloc := 'OBSERVER';
  target := cnaifSun;
  obsctr := 'EARTH';
  obsref := 'ITRF93';
  if trueequinox then begin
    outref := 'EARTH_TRUE_EQUATOR';
    abcorr := abcorrLTS;
  end
  else begin
    outref := 'J2000';
    abcorr := abcorrLT;
  end;
  spkcpo_c (target,et,outref,refloc,abcorr,obspos,obsctr,obsref,state1, @lt);
  if failed_c then begin
    if SpiceLastErr='SPICE(FRAMEDATANOTFOUND)' then begin
      // out of ITRF93 date range, retry with buildin IAU_EARTH
      reset_c;
      obsref := 'IAU_EARTH';
      spkcpo_c (target,et,outref,refloc,abcorr,obspos,obsctr,obsref,state1, @lt);
      if failed_c then
        exit;
    end
    else
      exit;
  end;
  result := not failed_c;
  if result then begin
    for i:=0 to 2 do state[i]:=state1[i];
    x:=state[0];
    y:=state[1];
    z:=state[2];
    recrad_c(state,r,ra,de);
  end;
end;

function MoonAltAz(et,obslon,obslat: SpiceDouble; obspos:TDouble3; out obsref:ConstSpiceChar; out az,el:SpiceDouble): Boolean;
var state,normal,topvec: TDouble3;
    state1: TDouble6;
    xform: TArray3;
    target,outref,refloc,obsctr,abcorr: ConstSpiceChar;
    lt: SpiceDouble;
    r,lon,lat,ra,de: SpiceDouble;
    i: integer;
const
    z: TDouble3 = ( 0.0, 0.0, 1.0 );
begin
  // compute moon Azimut/Elevation position for obspos
  reset_c;
  result:=false;
  refloc := 'OBSERVER';
  target := cnaifMoon;
  obsctr := 'EARTH';
  obsref := 'ITRF93';
  outref := obsref;
  abcorr := abcorrLTS;
  // equatorial position
  spkcpo_c (target,et,outref,refloc,abcorr,obspos,obsctr,obsref,state1, @lt);
  if failed_c then begin
    if SpiceLastErr='SPICE(FRAMEDATANOTFOUND)' then begin
      // out of ITRF93 date range, retry with buildin IAU_EARTH
      reset_c;
      obsref := 'IAU_EARTH';
      outref := obsref;
      spkcpo_c (target,et,outref,refloc,abcorr,obspos,obsctr,obsref,state1, @lt);
      if failed_c then
        exit;
    end
    else
      exit;
  end;
  result := not failed_c;
  if result then begin
    for i:=0 to 2 do state[i]:=state1[i];
    // equatorial ra/dec
    recrad_c(state,r,ra,de);
    // observer surface normal vector
    latrec_c ( 1.0, obslon, obslat, normal );
    // get rotation matrix
    twovec_c ( normal, 3, z, 1, xform );
    // rotate to topcentric
    mxv_c ( xform, state, topvec );
    // get azimut and elevation
    reclat_c ( topvec, r, lon, lat );
    az := -lon;
    el := lat;
  end;
end;

function MoonSubEarthPoint(et: SpiceDouble; out fixref:ConstSpiceChar; out x,y,z,r,lon,lat:SpiceDouble): Boolean;
var
  target:ConstSpiceChar;
  obsrvr:ConstSpiceChar;
  spoint:TDouble3;
  trgepc:SpiceDouble;
  srfvec:TDouble3;
begin
  // moon sub-earth point is the geocentric libration
  reset_c;
  result:=false;
  target:=cnaifMoon;
  obsrvr:=cnaifEarth;
  fixref:=fixrefME;
  // find sub point
  subpnt_c (submethod,target,et,fixref,abcorrLTS,obsrvr,spoint,trgepc,srfvec);
  if failed_c then begin
    if SpiceLastErr='SPICE(FRAMEDATANOTFOUND)' then begin
      // out of MOON_ME date range, retry with buildin IAU_MOON
      reset_c;
      fixref:=fixrefIAU;
      subpnt_c (submethod,target,et,fixref,abcorrLTS,obsrvr,spoint,trgepc,srfvec);
      if failed_c then begin
        exit;
      end;
    end
    else begin
      exit;
    end;
  end;
  result := not failed_c;
  if result then begin
    reclat_c(spoint,r,lon,lat);
    x:=spoint[0];
    y:=spoint[1];
    z:=spoint[2];
  end;
end;

function MoonSubObserverPoint(et: SpiceDouble; obspos:TDouble3; out x,y,z,r,lon,lat:SpiceDouble): Boolean;
var point,u,postn,positn,radii: TDouble3;
    state: TDouble6;
    tipm: TArray3;
    target,posref,refloc,obsctr,abcorr,obsref: ConstSpiceChar;
    lt,a,b,c: SpiceDouble;
    n: SpiceInt;
    found: SpiceBoolean;
    i: integer;
begin
  // moon sub-observer point is the topocentric libration
  reset_c;
  result:=false;
  refloc := 'OBSERVER';
  target := cnaifMoon;
  obsctr := 'EARTH';
  obsref := 'ITRF93';
  posref := 'J2000';
  abcorr := abcorrLT;
  // compute j2000 vector from observer to moon center
  spkcpo_c (target,et,posref,refloc,abcorr,obspos,obsctr,obsref,state, @lt);
  if failed_c then begin
    if SpiceLastErr='SPICE(FRAMEDATANOTFOUND)' then begin
      // out of ITRF93 date range, retry with buildin IAU_EARTH
      reset_c;
      obsref := 'IAU_EARTH';
      spkcpo_c (target,et,posref,refloc,abcorr,obspos,obsctr,obsref,state, @lt);
      if failed_c then
        exit;
    end
    else
      exit;
  end;
  if failed_c then exit;
  // revert vector from moon center to observer
  for i:=0 to 2 do postn[i]:=-state[i];
  // get rotation matrix from j2000 to moon fixed
  tipbod_c(posref,naifMoon,et,tipm);
  // rotate the position vector to moon fixed
  mxv_c(tipm, postn, u);
  // set origin at moon center
  positn[0]:=0;
  positn[1]:=0;
  positn[2]:=0;
  // moon radius
  bodvrd_c ( 'MOON', 'RADII', 3, n, radii );
  a:=radii[0];
  b:=radii[1];
  c:=radii[2];
  // compute moon surface intersect point
  surfpt_c(positn,u,a,b,c,point,found);
  result:=found;
  if result then begin
    x:=point[0];
    y:=point[1];
    z:=point[2];
    reclat_c(point,r,lon,lat);
  end;
end;

function MoonSubSolarPoint(et: SpiceDouble; out fixref:ConstSpiceChar; out x,y,z,r,lon,lat,colongitude:SpiceDouble): Boolean;
var target:ConstSpiceChar;
  obsrvr:ConstSpiceChar;
  spoint:TDouble3;
  trgepc:SpiceDouble;
  srfvec:TDouble3;
begin
  // moon sub-solar point, colongitude is 90-(sub-solar longitude)
  reset_c;
  result:=false;
  target:=cnaifMoon;
  obsrvr:=cnaifEarth;
  fixref:=fixrefIAU;//fixrefME;
  subslr_c (submethod,target,et,fixref,abcorrLTS,obsrvr,spoint,trgepc,srfvec);
  if failed_c then begin
    if SpiceLastErr='SPICE(FRAMEDATANOTFOUND)' then begin
      // out of MOON_ME date range, retry with buildin IAU_MOON
      reset_c;
      fixref:=fixrefIAU;
      subslr_c (submethod,target,et,fixref,abcorrLTS,obsrvr,spoint,trgepc,srfvec);
      if failed_c then begin
        exit;
      end;
    end
    else begin
      exit;
    end;
  end;
  result := not failed_c;
  if result then begin
    reclat_c(spoint,r,lon,lat);
    if lon<0 then
       lon:=pi2+lon;
    colongitude:=rmod(pid2-lon+pi2,pi2);
    x:=spoint[0];
    y:=spoint[1];
    z:=spoint[2];
  end;
end;

function FindOnePhase(k: double; realphase:boolean; out dt: double): boolean;
var i: integer;
    relate: ConstSpiceChar;
    refval: SpiceDouble;
    Pcnfine,Presult: PSpiceCell;
    et0,et1,x,y: SpiceDouble;
    t,jdstart:double;
    str: ConstSpiceChar;
    n: integer;
begin
  // find a principal phase date base on k
  reset_c;
  result:=false;
  // Approximate start from Meeus
  t := k/1236.85;
  jdstart:= 2451550.09766 + 29.530588861*k + 0.00015437*t**2 - 0.000000150*t**3 + 0.00000000073*t**4;
  // start 2 days before
  str:=PChar(jddate(jdstart-2));
  str2et_c(str,et0);
  // search for 4 days
  et1:=et0+4*SecsPerDay;
  Pcnfine:=initdoublecell(1);
  Presult:=initdoublecell(2);
  wninsd_c ( et0, et1, Pcnfine );
  if failed_c then begin
    exit;
  end;
  // Frac(k) must be 0, 0.25, 0.5 or 0.75, use this method to prevent rounding error
  i := round(100*(k - floor(k)));
  case i of
   0..10 : begin                  //new moon
          if realphase then
            relate:='ABSMAX'
          else
            relate:='ABSMIN';
          refval:=0;
          end;
   11..30: begin                  //first quarter
          relate:='=';
          refval:=90.0*deg2rad;
          end;
   31..60: begin                  //full moon
           if realphase then
             relate:='ABSMIN'
           else
             relate:='ABSMAX';
          refval:=0;
          end;
   61..99: begin                  // last quarter
          relate:='=';
          refval:=90.0*deg2rad;
          end;
  end;

  if realphase then
    MoonSearchPhase(refval,relate,n,Pcnfine,Presult) // extrema of phase angle
  else
    MoonSearchSunLongitudeDiff(refval,relate,n,Pcnfine,Presult); // official definition using ecliptic longitude

  if n>0 then begin
    wnfetd_c(Presult,0,x,y);
    dt:=ET2DateTime(x);
    result:=true;
  end;
  scard_c (0, Pcnfine);
  scard_c (0, Presult);
end;

function MoonPhases(startdate:TDateTime; out nm, fq, fm, lq: TDateTime; out lunation: double): Boolean;
var
  dy,dm,dd: Word;
  year,k,x:double;
begin
  result:=false;
  // Approximate start from Meeus
  DecodeDate(startdate,dy,dm,dd);
  year := dy+(dm-1)/12+(dd-1)/365.25;
  k := floor((year - 2000) * 12.3685);
  // search new moon
  if not FindOnePhase(k,false,nm) then exit;
  // search first quarter
  if not FindOnePhase(k+0.25,false,fq) then exit;
  // search full moon
  if not FindOnePhase(k+0.5,false,fm) then exit;
  // search last quarter
  if not FindOnePhase(k+0.75,false,lq) then exit;
  lunation := startdate - nm;
  if lunation < 0 then
  begin
    year:=year-14/365.25;
    k := floor((year - 2000) * 12.3685);
    if FindOnePhase(k,false,x) then
      lunation := startdate - x;
  end;
  result:=true;
end;

function MoonSearchPhase(value: SpiceDouble; relate: ConstSpiceChar; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell): boolean;
var illmn,obsrvr,target,abcorr: ConstSpiceChar;
  adjust,refval,step: SpiceDouble;
begin
  // find date for any phase value, both ascending and descending
  reset_c;
  illmn  := cnaifSun;
  obsrvr := cnaifEarth;
  target := cnaifMoon;
  abcorr := abcorrLTS;
  adjust := 0.0;
  refval := value;
  step   := SecsPerDay;
  gfpa_c (target,illmn,abcorr,obsrvr,relate,refval,adjust,step,MAXIVL,Pcnfine,Presult);
  result := not failed_c;
  nfind:=wncard_c(Presult);
end;


procedure Moon_udqdec(f:Tudfuns; et:SpiceDouble; out isdecr:SpiceBoolean); cdecl;
var dt: SpiceDouble;
begin
  // differenrial of f at et
  dt:=10;
  uddc_c(f,et,dt,isdecr);
end;

procedure MoonSunLongitude_udfuns(et:SpiceDouble; out value:SpiceDouble); cdecl;
var state: TDouble3;
    lt: SpiceDouble;
    abcorr,outref: ConstSpiceChar;
    r,lonm,latm,lons,lats: SpiceDouble;
begin
  // return difference between moon and sun ecliptic longitude
  reset_c;
  outref := 'ECLIPJ2000';
  abcorr := abcorrLTS;
  spkezp_c ( naifMoon, et, outref, abcorr, naifEarth, state, @lt );
  if (not failed_c) then begin
    reclat_c(state,r,lonm,latm);
    spkezp_c ( naifSun, et, outref, abcorr, naifEarth, state, @lt );
    if (not failed_c) then begin
      reclat_c(state,r,lons,lats);
    end;
    value:=abs(lonm-lons);
    if value>pi then
       value:=pi2-value;
  end;
end;

function MoonSearchSunLongitudeDiff(refval: SpiceDouble; relate: ConstSpiceChar; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell):boolean; cdecl;
var   adjust,step: SpiceDouble;
begin
  // find date for difference in Sun and Moon longitude.
  // definition of the "Moon phase":
  // full moon    : refval=0 relate='ABSMAX';
  // first quarter: refval=90 relate='=';
  // new moon     : refval=180 relate='ABSMIN';
  // last quarter : refval=270 relate='=';
  reset_c;
  adjust := 0.0;
  step   := 6*SecsPerDay;
  gfuds_c(@MoonSunLongitude_udfuns,@Moon_udqdec,relate,refval,adjust,step,MAXIVL,Pcnfine,Presult);
  result := not failed_c;
  nfind:=wncard_c(Presult);
end;

procedure MoonColongitude_test(f:Tudfuns; et:SpiceDouble; out xbool:SpiceBoolean); cdecl;
var fixref:ConstSpiceChar;
    x,y,z,r,lon,lat,colong:SpiceDouble;
begin
  MoonSubSolarPoint(et,fixref,x,y,z,r,lon,lat,colong);
  xbool:=AngleBetween(colong,colongmin,colongmax);
end;

function MoonSearchColongitude(value,delta: SpiceDouble; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell):boolean;
var
  step: SpiceDouble;
begin
  // find date when colongitude is between value-delta and value+delta
  result:=false;
  nfind:=0;
  reset_c;
  colongmin:=rmod(value-delta+pi2,pi2);
  colongmax:=rmod(value+delta+pi2,pi2);
  step := 28*SecsPerDay*(delta/pi2);
  gfudb_c(@udf_c,@MoonColongitude_test,step,Pcnfine,Presult);
  result := not failed_c;
  nfind:=wncard_c(Presult);
end;

function MoonSearchIllum(pos: TDouble3; value: SpiceDouble; relate: ConstSpiceChar; var fixref:ConstSpiceChar; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell): boolean;
var
  illmn,obsrvr,target,abcorr,method,angtyp: ConstSpiceChar;
  adjust,step: SpiceDouble;
begin
  // search date when a specific point on moon get some illumination from the sun defined by relate and value
  reset_c;
  result:=false;
  illmn  := cnaifSun;
  obsrvr := cnaifEarth;
  target := cnaifMoon;
  abcorr := abcorrLTS;
  adjust := 0.0;
  method := 'Ellipsoid';
  angtyp := 'INCIDENCE';
  step   := 3*SecsPerDay;
  gfilum_c(method,angtyp,target,illmn,fixref,abcorr,obsrvr,pos,relate,value,adjust,step,MAXIVL,Pcnfine,Presult);
  if failed_c then begin
    if SpiceLastErr='SPICE(FRAMEDATANOTFOUND)' then begin
      // out of MOON_ME date range, retry with buildin IAU_MOON
      reset_c;
      fixref:=fixrefIAU;
      gfilum_c(method,angtyp,target,illmn,fixref,abcorr,obsrvr,pos,relate,value,adjust,step,MAXIVL,Pcnfine,Presult);
      if failed_c then begin
        exit;
      end;
    end
    else begin
      exit;
    end;
  end;
  result := not failed_c;
  nfind:=wncard_c(Presult);
end;

function MoonSearchLibration(coord: ConstSpiceChar; value: SpiceDouble; relate: ConstSpiceChar; var fixref:ConstSpiceChar; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell):boolean;
var obsrvr,target,abcorr,method: ConstSpiceChar;
  crdsys: ConstSpiceChar;
  adjust,step: SpiceDouble;
begin
  // search date for a specific geocentric libration defined by relate and value
  result:=false;
  reset_c;
  target:=cnaifMoon;
  obsrvr:=cnaifEarth;
  method:=submethod;
  abcorr:=abcorrLTS;
  crdsys:='LATITUDINAL';
  adjust:=0;
  step:=3*SecsPerDay;
  gfsubc_c(target,fixref,method,abcorr,obsrvr,crdsys,coord,relate,value,adjust,step,MAXIVL,Pcnfine,Presult);
  if failed_c then begin
    if SpiceLastErr='SPICE(FRAMEDATANOTFOUND)' then begin
      reset_c;
      fixref:=fixrefIAU;
      gfsubc_c(target,fixref,method,abcorr,obsrvr,crdsys,coord,relate,value,adjust,step,MAXIVL,Pcnfine,Presult);
      if failed_c then begin
        exit;
      end;
    end
    else begin
      exit;
    end;
  end;
  result := not failed_c;
  nfind:=wncard_c(Presult);
end;

function MoonSearchRiseSet(pos: TDouble3; obslat,el: SpiceDouble; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell): boolean;
var
  illmn,obsrvr,target,abcorr,method,angtyp,fixref,relate: ConstSpiceChar;
  refval,adjust,step: SpiceDouble;
begin
  // search date when moon is above horizon at pos
  result:=false;
  illmn  := cnaifMoon;
  obsrvr := cnaifMoon;
  target := cnaifEarth;
  fixref :='ITRF93';
  abcorr := abcorrNone;
  adjust := 0.0;
  method := 'Ellipsoid';
  angtyp := 'INCIDENCE';
  relate:='<';
  refval := deg2rad*(90-el);
  if abs(obslat)>(60*deg2rad) then
    step   := SecsPerHour
  else
    step   := 5*SecsPerHour;
  gfilum_c(method,angtyp,target,illmn,fixref,abcorr,obsrvr,pos,relate,refval,adjust,step,MAXIVL,Pcnfine,Presult);
  if failed_c then begin
    if SpiceLastErr='SPICE(FRAMEDATANOTFOUND)' then begin
      // out of ITRF93 date range, retry with buildin IAU_EARTH;
      reset_c;
      fixref:='IAU_EARTH';
      gfilum_c(method,angtyp,target,illmn,fixref,abcorr,obsrvr,pos,relate,refval,adjust,step,MAXIVL,Pcnfine,Presult);
      if failed_c then begin
        exit;
      end;
    end
    else begin
      exit;
    end;
  end;
  result := not failed_c;
  nfind:=wncard_c(Presult);
end;

function SunSearchRiseSet(pos: TDouble3; obslat,el: SpiceDouble; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell): boolean;
var
  illmn,obsrvr,target,abcorr,method,angtyp,fixref,relate: ConstSpiceChar;
  refval,adjust,step: SpiceDouble;
begin
  // search date when sun is above horizon at pos
  result:=false;
  illmn  := cnaifSun;
  obsrvr := cnaifSun;
  target := cnaifEarth;
  fixref :='ITRF93';
  abcorr := abcorrNone;
  adjust := 0.0;
  method := 'Ellipsoid';
  angtyp := 'INCIDENCE';
  relate:='<';
  refval := deg2rad*(90-el);
  if abs(obslat)>(60*deg2rad) then
    step   := SecsPerHour
  else
    step   := 5*SecsPerHour;
  gfilum_c(method,angtyp,target,illmn,fixref,abcorr,obsrvr,pos,relate,refval,adjust,step,MAXIVL,Pcnfine,Presult);
  if failed_c then begin
    if SpiceLastErr='SPICE(FRAMEDATANOTFOUND)' then begin
      // out of ITRF93 date range, retry with buildin IAU_EARTH;
      reset_c;
      fixref:='IAU_EARTH';
      gfilum_c(method,angtyp,target,illmn,fixref,abcorr,obsrvr,pos,relate,refval,adjust,step,MAXIVL,Pcnfine,Presult);
      if failed_c then begin
        exit;
      end;
    end
    else begin
      exit;
    end;
  end;
  result := not failed_c;
  nfind:=wncard_c(Presult);
end;

function SearchNight(pos: TDouble3; obslat,el: SpiceDouble; out nfind:SpiceInt; Pcnfine,Presult:PSpiceCell): boolean;
var
  illmn,obsrvr,target,abcorr,method,angtyp,fixref,relate: ConstSpiceChar;
  refval,adjust,step: SpiceDouble;
begin
  // search date when sun is below horizon at pos
  result:=false;
  illmn  := cnaifSun;
  obsrvr := cnaifSun;
  target := cnaifEarth;
  fixref :='ITRF93';
  abcorr := abcorrNone;
  adjust := 0.0;
  method := 'Ellipsoid';
  angtyp := 'INCIDENCE';
  relate:='>';
  refval := deg2rad*(90-el);
  if abs(obslat)>(60*deg2rad) then
    step   := SecsPerHour
  else
    step   := 3*SecsPerHour;
  gfilum_c(method,angtyp,target,illmn,fixref,abcorr,obsrvr,pos,relate,refval,adjust,step,MAXIVL,Pcnfine,Presult);
  if failed_c then begin
    if SpiceLastErr='SPICE(FRAMEDATANOTFOUND)' then begin
      // out of ITRF93 date range, retry with buildin IAU_EARTH;
      reset_c;
      fixref:='IAU_EARTH';
      gfilum_c(method,angtyp,target,illmn,fixref,abcorr,obsrvr,pos,relate,refval,adjust,step,MAXIVL,Pcnfine,Presult);
      if failed_c then begin
        exit;
      end;
    end
    else begin
      exit;
    end;
  end;
  result := not failed_c;
  nfind:=wncard_c(Presult);
end;

end.

