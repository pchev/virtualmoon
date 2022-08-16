unit u_constant;
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
 Type and constant declaration
}
{$mode objfpc}{$H+}
interface

uses
     dynlibs, Classes, Controls, Graphics, passql, passqlite;
{
     cu_tz, dynlibs,
     Classes, Controls, FPCanvas, Graphics;}

const crlf = chr(10)+chr(13);
      cpyr = '©'; //chr($a9)+chr($c2);  // ©
      AVLversion = '8.0beta';
      version = '8.0a';
      VersionName = 'AtLun';
      avlcpy = 'Copyright '+cpyr+' 2002-2020 Christian Legrand, Patrick Chevalley';
      vmaurl='http://ap-i.net/avl';
      jd2000 =2451545.0 ;
      jd1950 =2433282.4235;
      jd1900 =2415020.3135;
      km_au = 149597870.691 ;
      clight = 299792.458 ;
      tlight = km_au/clight/3600/24;
      Rmoon = 1737.40;  // moon radius Km
      MeanEarthDistance=384401;
      meter2feet = 3.28084;
      footpermeter = 0.3048;
      km2miles = 0.621371;
      mikm = 1.60934;
      SynodicMonth = 29.530588;
      kmperdegree=111.1111;
      secday=3600*24;
      eps2000 = 23.439291111;
      deg2rad = pi/180;
      rad2deg = 180/pi;
      pi2 = 2*pi;
      pi4 = 4*pi;
      pid2 = pi/2;
      minarc = deg2rad/60;
      secarc = deg2rad/3600;
      musec  = deg2rad/3600/1000000; // 1 microarcsec for rounding test
      DefaultPrtRes = 300;
      LightDist=100;
      cameradist=2000;
      crRetic = 5;
      ox=36; oy=36; os=1500; px=0.95467; py=0.95467; //image 1500x1500, lune 1432x1432
      nummessage = 75;
      MaxLabel=500;
      MaxMeasurePoint=100;
      InitialSprite=1000;
      AbsoluteMaxSprite=5000;
      Label3dSize=1;
      maxfocbase=1900;
      abek = secarc*20.49552;  // aberration constant
      maxlevel = 6;
      siderealrate = 15.041067178669; // arcsec/second
      encryptpwd = 'aehooX4Aekiu7tha;Eiraingienugoo1v.Aexejae3outhah3O';

      nJPL_DE = 7;
      JPL_DE: array [1..nJPL_DE] of integer = (423, 421, 422, 405, 406, 403, 200);

      // Paper size
      PaperNumber=9;
      PaperName : array[1..PaperNumber] of string=('A5','A4','A3', 'A2', 'A1', 'A0',  'Letter','Legal','Tabloid');
      PaperWidth: array[1..PaperNumber] of single=(5.83,8.27,11.69, 16.54,23.39,33.11, 8.5,     8.5,    11.0);
      PaperHeight:array[1..PaperNumber] of single=(8.27,11.69,16.54,23.39,33.11,46.81, 11.0,    14.0,   17.0);

      greek : array[1..2,1..24]of string=(('Alpha','Beta','Gamma','Delta','Epsilon','Zeta','Eta','Theta','Iota','Kappa','Lambda','Mu','Nu','Xi','Omicron','Pi','Rho','Sigma','Tau','Upsilon','Phi','Chi','Psi','Omega'),
              ('alp','bet','gam','del','eps','zet','eta','the','iot','kap','lam','mu','nu','xi','omi','pi','rho','sig','tau','ups','phi','chi','psi','ome'));
      greeksymbol : array[1..2,1..24]of string=(('alp','bet','gam','del','eps','zet','eta','the','iot','kap','lam','mu','nu','xi','omi','pi','rho','sig','tau','ups','phi','chi','psi','ome'),
                  ('a','b','g','d','e','z','h','q','i','k','l','m','n','x','o','p','r','s','t','u','f','c','y','w'));
      greekUTF8 : array[1..24] of word =($CEB1,$CEB2,$CEB3,$CEB4,$CEB5,$CEB6,$CEB7,$CEB8,$CEB9,$CEBA,$CEBB,$CEBC,$CEBD,$CEBE,$CEBF,$CF80,$CF81,$CF83,$CF84,$CF85,$CF86,$CF87,$CF88,$CF89);
      blank15='               ';
      blank=' ';
      tab=#09;
      deftxt = '?';
      f0='0';
      f1='0.0';
      f1s='0.#';
      f2='0.00';
      f3='0.000';
      f4='0.0000';
      f5='0.00000';
      f6='0.000000';
      datetimedisplay='yy"-"mm"-"dd" "hh":"nn":"ss';
      datedisplay='yy"-"mm"-"dd"';
      dateiso='yyyy"-"mm"-"dd"T"hh":"nn":"ss.zzz';
      HistoricalDir='Historical';
      nOptionalFeature= 7;
      OptionalFeatureCheck: array[1..nOptionalFeature]of string=(
                      'BestOfCathala/NEUMAYER_LCATHALA 1.jpg',  // picture
                      'Textures/LOLA_Kaguya_Shade/L4/0.jpg',             // data
                      'Textures/LOLA_Kaguya_Shade/L5/0.jpg',             // hires
                      'Textures/Change/L6/0.jpg',                        // very hires Chang'e
                      'Textures/Lopam/L6/0.jpg',                         // very hires LOPAM
                      'Textures/WAC/L6/0.jpg',                           // LRO WAC
                      'Textures/LOLA_Kaguya_Shade/L6/0.jpg'              // LOLA - Kaguya
                      );

      DbSatellite=2;
      DbImpactBassin=9;
      // Datlun constant
      numdb=99;
      numdbtype = 25;
      // NAMETYPE SUBTYPE PROCESS GEOLOGY AREA TIPS
      //hidenfields = [5,7,9,10,33,53];
      hidenfields = [];

{$ifdef linux}
      DefaultHome='~/';
      DefaultPrivateDir='~/.virtualmoon';
      Defaultconfigfile='~/.virtualmoon/vma.rc';
      SharedDir='../share/virtualmoon';
      DefaultTmpDir='tmp';
      DefaultPhotlun='photlun';
      DefaultMaplun='atlun';
      DefaultDatlun='datlun';
      DefaultWeblun='weblun';
      DefaultCalclun='calclun';
      DefaultNotelun='notelun';
      DefaultCdC='skychart';
      DefaultCdCconfig='~/.skychart/skychart.ini';
      DefaultVignetteDir='vignette';
{$endif}
{$ifdef darwin}
      DefaultHome='~/';
      DefaultPrivateDir='~/.virtualmoon';
      Defaultconfigfile='~/.virtualmoon/vma.rc';
      SharedDir='/usr/share/virtualmoon';
      DefaultTmpDir='tmp';
      DefaultPhotlun='photlun.app/Contents/MacOS/photlun';
      DefaultMaplun='atlun.app/Contents/MacOS/atlun';
      DefaultDatlun='datlun.app/Contents/MacOS/datlun';
      DefaultWeblun='weblun.app/Contents/MacOS/weblun';
      DefaultCalclun='calclun.app/Contents/MacOS/calclun';
      DefaultNotelun='notelun.app/Contents/MacOS/notelun';
      DefaultCdC='skychart.app/Contents/MacOS/skychart';
      DefaultCdCconfig='~/.skychart/skychart.ini';
      DefaultVignetteDir='vignette';
{$endif}
{$ifdef mswindows}
      DefaultPrivateDir='virtualmoon';
      Defaultconfigfile='vma.rc';
      SharedDir='.\';
      DefaultTmpDir='tmp';
      DefaultPhotlun='photlun.exe';
      DefaultMaplun='atlun.exe';
      DefaultDatlun='datlun.exe';
      DefaultWeblun='weblun.exe';
      DefaultCalclun='calclun.exe';
      DefaultNotelun='notelun.exe';
      DefaultCdC='skychart.exe';
      DefaultCdCconfig='Skychart\skychart.ini';
      DefaultVignetteDir='vignette';
{$endif}

type
     double6 = array[1..6] of double;
     Pdouble6 = ^double6;
     doublearray = array of double;
     Pdoublearray = ^doublearray;

type
  TDBInfo = class(TObject)
    dbnum: integer;
  end;

// external library
const
{$ifdef linux}
      lib404   = 'libpasplan404.so.1';
{$endif}
{$ifdef darwin}
      lib404   = 'libplan404.dylib';
{$endif}
{$ifdef mswindows}
      lib404 = 'libplan404.dll';
{$endif}

// libplan404
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
     TPlan404=Function( pla : PPlanetData):integer; cdecl;
var Plan404 : TPlan404;
    Plan404lib: TLibHandle;

// pseudo-constant only here
Var  Splashversion, compile_time, compile_version: string;
     BinDir, Homedir, Appdir, PrivateDir, SampleDir, DBdir, TempDir, ZoneDir, HelpDir,CdCdir,jpldir,vignettedir : string;
     MapLun,Photlun,DatLun,WebLun,CalcLun,NoteLun,CdC,PrtName, transmsg : String;
     ObsLatitude,ObsLongitude,ObsAltitude : double;
     ObsTZ,ObsCountry: string;
     ObsTemperature,ObsPressure,ObsRefractionCor,ObsHorizonDepression : Double;
     TimeZone,TimeZoneD,DT_UT,ObsRoCosPhi,ObsRoSinPhi,CurrentJD : double;
     CurYear,CurrentMonth,CurrentDay : integer;
     CurrentTime,TimeBias,CurrentST,DT_UT_val,CurrentSunH,CurrentMoonH,CurrentMoonIllum,diam : Double;
     PlanetParalaxe: boolean;
     ForceConfig, Configfile, CdCconfig, language, uplanguage, helpprefix : string;
     ldeg,lmin,lsec : string;
     PrinterResolution: integer;
     librationeffect, AsMultiTexture : Boolean;
     Firstsearch: boolean;
     DisplayIs32bpp: Boolean;
     ThemePath:string ='data/Themes';
     LinuxDesktop: integer = 0;  // FreeDesktop=0, KDE=1, GNOME=2, Other=3
     Params : TStringList;
     OptionalFeatureName: array[1..nOptionalFeature]of string;
     de_type, de_year, NumDist: integer;
     DistStartL,DistStartB,DistEndL,DistEndB: array[0..MaxMeasurePoint] of double;
{$ifdef darwin}
     OpenFileCMD:string = 'open';   //
{$else}
     OpenFileCMD:string = 'xdg-open';   // default FreeDesktop.org
{$endif}
     // to move to pu_moon properties:
     labelcenter,showlabel,showmark: boolean;
     currenteyepiece,marksize, CurrentCCD: integer;
     marklabelcolor, markcolor, SpriteColor, bassinColor: Tcolor;
     DarkTheme : boolean;
     DatabaseList: Tstringlist;
     UnnamedList: string;

     // Datlun var
     dbtype : array[1..numdbtype] of string;
     dbshortname : array[1..numdb] of string;
     dbselection: string;
     dbm,dbnotes: TLiteDB;


// Text formating constant
const
     html_h        = '<HTML><body bgcolor="#FFFFFF" text="#000000">';
     html_h_nv     = '<HTML><body bgcolor="#000000" text="#C03030">';
     htms_h        = '</body></HTML>';
     html_ffx      = '<font face="fixed">';
     htms_f        = '</font>';
     html_b        = '<b>';
     htms_b        = '</b>';
     html_h2       = '<font size="+2"><b>';
     htms_h2       = '</b></font><br>';
     html_p        = '<p>';
     htms_p        = '</p>';
     html_br       = '<br>';
     html_sp       = '&nbsp;';
     html_pre      = '<pre>';
     htms_pre      = '</pre>';


// INDI Telescope driver
const
      NumIndiDriver=10;
      IndiDriverLst: array[0..NumIndiDriver,1..2] of string =(('Other',''),
                  ('AstroPhysic','apmount'),
                  ('Celestron GPS','celestrongps'),
                  ('LX200 Basic','lx200basic'),
                  ('LX200 Generic','lx200generic'),
                  ('LX200 Classic','lx200classic'),
                  ('LX200 GPS','lx200gps'),
                  ('LX200 Autostar','lx200autostar'),
                  ('LX200 16','lx200_16'),
                  ('SkyCommander','skycommander'),
                  ('Takahashi Temma','temma'));

implementation

end.

