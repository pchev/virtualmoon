unit u_constant;
{
Copyright (C) 2002 Patrick Chevalley

http://www.astrosurf.com/astropc
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
{
 Type and constant declaration
}
{$mode objfpc}{$H+}
interface

uses
     dynlibs, Classes, Controls, Graphics,passql, passqlite;

const crlf = chr(10)+chr(13);
      AVLversion = '5.0';
      Splashversion = AVLversion+' 2009-12-12';
      VersionName = 'Pro';
      avlcpy = 'Copyright (C) 2002-2009 Christian Legrand, Patrick Chevalley';
      vmaurl='http://ap-i.net/avl';
      jd2000 =2451545.0 ;
      jd1950 =2433282.4235;
      jd1900 =2415020.3135;
      km_au = 149597870.691 ;
      clight = 299792.458 ;
      tlight = km_au/clight/3600/24;
      Rmoon = 1737.103;  // moon radius Km
      MeanEarthDistance=384401;
      footpermeter = 0.3048;
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
      InitialSprite=1000;
      AbsoluteMaxSprite=5000;
      Label3dSize=1;
      maxfocbase=1900;
      numdb=7;
      numdbtype = 25;

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
      dateiso='yyyy"-"mm"-"dd"T"hh":"nn":"ss.zzz';

      VMAbrowser='DATLUN';

{$ifdef linux}
      DefaultPrivateDir='~/.virtualmoon';
      Defaultconfigfile='~/.virtualmoon/vma.rc';
      SharedDir='../share/virtualmoon';
      DefaultTmpDir='tmp';
      DefaultPhotlun='photlun';
      DefaultMaplun='atlun';
      DefaultCdC='skychart';
      DefaultCdCconfig='~/.skychart/skychart.ini';
{$endif}
{$ifdef darwin}
      DefaultPrivateDir='~/.virtualmoon';
      Defaultconfigfile='~/.virtualmoon/vma.rc';
      SharedDir='/usr/share/virtualmoon';
      DefaultTmpDir='tmp';
      DefaultPhotlun='photlun.app/Contents/MacOS/photlun';
      DefaultMaplun='atlun.app/Contents/MacOS/atlun';
      DefaultCdC='skychart.app/Contents/MacOS/skychart';
      DefaultCdCconfig='~/.skychart/skychart.ini';
{$endif}
{$ifdef win32}
      DefaultPrivateDir='virtualmoon';
      Defaultconfigfile='vma.rc';
      SharedDir='.\';
      DefaultTmpDir='tmp';
      DefaultPhotlun='photlun.exe';
      DefaultMaplun='atlun.exe';
      DefaultCdC='skychart.exe';
      DefaultCdCconfig='Skychart\skychart.ini';
{$endif}

type
     double6 = array[1..6] of double;
     Pdouble6 = ^double6;

// external library
const
{$ifdef linux}
      lib404   = 'libplan404.so';
{$endif}
{$ifdef darwin}
      lib404   = 'libplan404.dylib';
{$endif}
{$ifdef win32}
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
Var  BinDir, Appdir, PrivateDir, SampleDir, DBdir, TempDir, ZoneDir, HelpDir,CdCdir : string;
     Photlun,MapLun,CdC,PrtName, CdCcaption, transmsg : String;
     ObsLatitude,ObsLongitude,ObsAltitude : double; ObsTZ: string;
     ObsTemperature,ObsPressure,ObsRefractionCor,ObsHorizonDepression : Double;
     TimeZone,DT_UT,ObsRoCosPhi,ObsRoSinPhi,CurrentJD : double;
     CurYear,CurrentMonth,CurrentDay : integer;
     CurrentTime,TimeBias,CurrentST,DT_UT_val,CurrentSunH,CurrentMoonH,CurrentMoonIllum,diam : Double;
     PlanetParalaxe: boolean;
     ForceConfig, Configfile, CdCconfig, language, uplanguage : string;
     compile_time,compile_version:string;
     ldeg,lmin,lsec : string;
     PrinterResolution: integer;
     librationeffect, AsMultiTexture : Boolean;
     texturefile: string;
     Firstsearch: boolean;
     DisplayIs32bpp: Boolean;
     ThemePath:string ='data/Themes';
     LinuxDesktop: integer = 0;  // FreeDesktop=0, KDE=1, GNOME=2, Other=3
     Params : TStringList;
     dbtype : array[1..numdbtype] of string;
     dbshortname : array[1..numdb] of string;
     dbselection: string;
     dbm: TLiteDB;
{$ifdef darwin}
     OpenFileCMD:string = 'open';   //
{$else}
     OpenFileCMD:string = 'xdg-open';   // default FreeDesktop.org
{$endif}
     // to move to pu_moon properties:
     labelcenter,showlabel,showmark: boolean;
     currenteyepiece,marksize: integer;
     marklabelcolor, markcolor: Tcolor;

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

