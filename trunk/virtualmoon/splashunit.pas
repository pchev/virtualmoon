unit splashunit;
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

uses IniFiles, Registry,Skylib,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, jpeg;

type
  Tsplash = class(TForm)
    Timer1: TTimer;
    Panel1: TPanel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel2: TPanel;
    Label5: TLabel;
    Timer2: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  splash: Tsplash;
  SplashTimer: Boolean = true;
  closing : boolean;

implementation
{$IFDEF opengl}
uses virtualmoon1;
{$ELSE}
uses virtualmoon2;
{$ENDIF}

{$R *.DFM}

const nlin=33;
var
cpylst: array[1..2,1..nlin]of string=((
'IDEE ORIGINALE : Christian LEGRAND',
'CONCEPTION GENERALE : Christian LEGRAND & Patrick  CHEVALLEY',
'',
'LOGICIEL :',
'PROGRAMMATION : Patrick  CHEVALLEY',
'INTERFACE : Patrick  CHEVALLEY',
'',
'TRADUCTION:',
'',
'',
'CARTOGRAPHIE :',
'CARTE 2D TOPOGRAPHIQUE : Patrick  CHEVALLEY',
'CARTE 2D GEOLOGIQUE : Christian LEGRAND',
'CARTE 3D TOPOGRAPHIQUE : Patrick  CHEVALLEY',
'',
'BASE DE DONNEES :',
'RECHERCHE DES INFORMATIONS : Christian LEGRAND',
'SAISIE & MISE EN FORME DES INFORMATIONS : Christian LEGRAND',
'',
'BIBLIOTHEQUE D''IMAGES :',
'RECHERCHE DES IMAGES : Christian LEGRAND',
'MISE EN FORME ET TRAITEMENT DES IMAGES : Christian LEGRAND',
'',
'DOCUMENTATION :',
'REDACTION : Christian LEGRAND & Patrick  CHEVALLEY',
'MISE EN FORME : Patrick  CHEVALLEY',
'GLOSSAIRE : Christian LEGRAND',
'',
'SITE INTERNET :',
'REDACTION : Christian LEGRAND & Patrick  CHEVALLEY',
'MISE EN FORME : Patrick  CHEVALLEY',
'WEBMESTRE : Patrick  CHEVALLEY',
''),(
'ORIGINAL IDEA : Christian LEGRAND',
'GENERAL CONCEPT : Christian LEGRAND & Patrick  CHEVALLEY',
'',
'SOFTWARE :',
'PROGRAMING : Patrick  CHEVALLEY',
'INTERFACE : Patrick  CHEVALLEY',
'',
'TRANSLATION:',
'',
'',
'CARTOGRAPHY :',
'2D TOPOGRAPHICAL MAP : Patrick  CHEVALLEY',
'2D GEOLOGICAL MAP : Christian LEGRAND',
'3D TOPOGRAPHICAL MAP: Patrick  CHEVALLEY',
'',
'DATABASE :',
'INFORMATIONS COLLECTING : Christian LEGRAND',
'INFORMATIONS TYPING AND FORMATING  : Christian LEGRAND',
'',
'PICTURES LIBRARY :',
'PICTURES SEARCH  : Christian LEGRAND',
'PICTURES FORMATING : Christian LEGRAND',
'',
'DOCUMENTATION :',
'EDITION : Christian LEGRAND & Patrick  CHEVALLEY',
'FORMATING : Patrick  CHEVALLEY',
'GLOSSARY : Christian LEGRAND',
'',
'INTERNET SITE :',
'EDITION : Christian LEGRAND & Patrick  CHEVALLEY',
'FORMATING : Patrick  CHEVALLEY',
'WEBMASTER : Patrick  CHEVALLEY',
''));


procedure Tsplash.Timer1Timer(Sender: TObject);
begin
  Timer1.enabled:=false;
  splash.release;
end;

procedure Tsplash.FormCreate(Sender: TObject);
var //ok: boolean;
//    Registry1: TRegistry;
    inifile : Tmeminifile;
    txt : string;
begin            
closing:=false;
{$ifdef vmalight}
 label2.Caption:='Light '+Splashversion;
{$endif}
{$ifdef vmabasic}
 label2.Caption:='Basic '+Splashversion;
{$endif}
{$ifdef vmaexpert}
 label2.Caption:='Expert '+Splashversion;
{$endif}
if language<>'UK' then begin
inifile:=Tmeminifile.create(AppDir+'\lang_'+language+'.ini');
with inifile do begin
    txt:=(ReadString('default','title',''));
end;
inifile.free;
if trim(txt)>'' then label1.caption:=txt;
end;
end;

procedure Tsplash.Image1DblClick(Sender: TObject);
begin
//if not timer1.Enabled then splash.release;
closing:=true;
end;

Procedure Animate;
var i,l,p :integer;
    buf : string;
begin
if language='FR' then l:=1 else l:=2;
buf:='';
for i:=1 to 6 do begin
  buf:=buf+' '+crlf;
end;
splash.label5.Caption:=buf;
cpylst[l,9]:=transmsg;
application.ProcessMessages;
i:=0;
repeat
  inc(i);
  if i>nlin then i:=1;
  p:=pos(crlf,buf);
  if p>0 then buf:=copy(buf,p+2,9999);
  buf:=buf+cpylst[l,i]+crlf;
  splash.label5.Caption:=buf;
  application.ProcessMessages;
  sleep(800);
until closing;
splash.release;
end;

procedure Tsplash.FormShow(Sender: TObject);
begin
  caption:=stringreplace(form1.Apropos1.caption,'&','',[]);;
  if SplashTimer then begin
//     panel2.Visible:=false;
     panel2.Height:=20;
     splash.ClientHeight:=panel1.Height+panel2.Height;
     label5.top:=0;
     label5.Alignment:=taCenter;
     label5.Caption:=transmsg;
     Timer1.enabled:=true
  end else begin
     splash.ClientHeight:=panel1.Height+panel2.Height;
     Timer2.enabled:=true;
  end;
end;

procedure Tsplash.Timer2Timer(Sender: TObject);
begin
timer2.Enabled:=false;
animate;
end;

procedure Tsplash.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
closing:=true;
canclose:=false;
end;

end.
