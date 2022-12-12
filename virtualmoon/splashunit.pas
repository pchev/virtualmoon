unit splashunit;

{$MODE Delphi}
{$H+}
{
Copyright (C) 2003 Patrick Chevalley

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
interface

uses u_translation, IniFiles, Registry, u_constant, u_util,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, LResources;

type

  { Tsplash }

  Tsplash = class(TForm)
    Label5: TLabel;
    Label6: TLabel;
    Timer1: TTimer;
    Panel1: TPanel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Timer2: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    Procedure Animate;
  public
    VersionName,Splashversion,transmsg: string;
  end;

var
  splash: Tsplash;
  SplashTimer: Boolean = true;
  closing : boolean;

implementation

{$R splashunit.lfm}

const nlin=66;
      trlin=48;
var
cpylst: array[1..2,1..nlin]of string=((
'Copyright © 2002-2022  Christian Legrand, Patrick Chevalley,',
'All rights reserved',
'This program is free software; you can redistribute it and/or',
'modify it under the terms of the GNU General Public License.',
'All database, pictures and documentation © Ch. Legrand',
'Pictures library are copyrighted by their respective owner.',
'',
'IDEE ORIGINALE : Christian LEGRAND',
'',
'CONCEPTION GENERALE : Christian LEGRAND & Patrick CHEVALLEY ',
'',
'LOGICIEL :',
'PROGRAMMATION : Patrick  CHEVALLEY ',
'INTERFACE : Patrick  CHEVALLEY & Christian LEGRAND ',
'',
'CARTOGRAPHIE :',
'RECHERCHE DES TEXTURES : Christian LEGRAND',
'MISE EN FORME DES TEXTURES : Patrick  CHEVALLEY ',
'RECHERCHE DES COUCHES SCIENTIFIQUES : Christian LEGRAND',
'MISE EN FORME DES COUCHES SCIENTIFIQUES : Christian LEGRAND',
'AUTORISATION D''UTILISATION DES TEXTURES ET DES COUCHES : Christian LEGRAND ',
'',
'BASE DE DONNEES :',
'ORGANISATION DE LA BASE : Christian LEGRAND',
'RECHERCHE DES INFORMATIONS : Christian LEGRAND',
'SAISIE & MISE EN FORME DES INFORMATIONS : Christian LEGRAND',
'OUTILS INFORMATIQUES DE GESTION DES BASES : Patrick CHEVALLEY ',
'',
'BIBLIOTHEQUE D''IMAGES :',
'RECHERCHE DES IMAGES : Christian LEGRAND',
'AUTORISATIONS D’UTILISATION : Christian LEGRAND',
'MISE EN FORME ET TRAITEMENT DES IMAGES : Christian LEGRAND',
'OUTILS INFORMATIQUES DE GESTION DES IMAGES : Patrick CHEVALLEY ',
'',
'DOCUMENTATION :',
'REDACTION : Christian LEGRAND & Patrick  CHEVALLEY ',
'MISE EN FORME : Patrick  CHEVALLEY ',
'GLOSSAIRE : Christian LEGRAND',
'ENCYCLOPEDIE : Christian LEGRAND',
'',
'SITE INTERNET :',
'REDACTION : Patrick CHEVALLEY & Christian LEGRAND',
'MISE EN FORME : Patrick  CHEVALLEY ',
'WEBMESTRE : Patrick  CHEVALLEY ',
'',
'TRADUCTIONS :',
'COORDINATION DES TRADUCTIONS : Patrick CHEVALLEY ',
'',
'',
'HOT LINE :',
'LOGICIEL : Patrick CHEVALLEY ',
'AUTRES SUJETS : Christian LEGRAND',
'',
'PROMOTION :',
'SUISSE : Patrick CHEVALLEY ',
'FRANCE :  Christian LEGRAND',
'AUTRES PAYS : Christian LEGRAND',
'',
'PRODUCTION DES CD :',
'GRAVAGE & IMPRESSION : Christian LEGRAND',
'ENVOI DES CD : Christian LEGRAND',
'',
'LICENSES D’UTILISATION :',
'OCTROI DES LICENSES : Christian LEGRAND & Patrick CHEVALLEY ',
'GESTION DES LICENSES : Christian LEGRAND',
''),(
'Copyright © 2002-2022  Christian Legrand, Patrick Chevalley,',
'All rights reserved',
'This program is free software; you can redistribute it and/or',
'modify it under the terms of the GNU General Public License.',
'All database, pictures and documentation © Ch. Legrand',
'Pictures library are copyrighted by their respective owner.',
'',
'ORIGINAL IDEA : Christian LEGRAND',
'',
'GENERAL CONCEPT : Christian LEGRAND & Patrick CHEVALLEY ',
'',
'SOFTWARE :',
'PROGRAMING : Patrick  CHEVALLEY ',
'INTERFACE : Patrick  CHEVALLEY & Christian LEGRAND',
'',
'CARTOGRAPHY :',
'TEXTURES SEARCH : Christian LEGRAND',
'TEXTURES FORMATING : Patrick  CHEVALLEY ',
'OVERLAYS SEARCH : Christian LEGRAND',
'OVERLAYS FORMATING : Christian LEGRAND',
'TEXTURES AND OVERLAYS AUTHORIZATIONS : Christian LEGRAND',
'',
'DATABASES :',
'DATABASE ORGANISATION  : Christian LEGRAND',
'INFORMATIONS COLLECTING : Christian LEGRAND',
'INFORMATIONS TYPING AND FORMATING  : Christian LEGRAND',
'SOFTWARE DATABASES TOOLS : Patrick CHEVALLEY ',
'',
'PICTURES LIBRARY :',
'PICTURES SEARCH  : Christian LEGRAND',
'PICTURES USE AUTHORIZATIONS : Christian LEGRAND',
'PICTURES FORMATING : Christian LEGRAND',
'SOFTWARE PICTURES TOOLS : Patrick CHEVALLEY ',
'',
'DOCUMENTATION :',
'EDITION : Christian LEGRAND & Patrick  CHEVALLEY ',
'FORMATING : Patrick  CHEVALLEY ',
'GLOSSARY : Christian LEGRAND',
'ENCYCLOPEDIA : Christian LEGRAND',
'',
'INTERNET SITE :',
'EDITION : Christian LEGRAND & Patrick  CHEVALLEY ',
'FORMATING : Patrick  CHEVALLEY ',
'WEBMASTER : Patrick  CHEVALLEY ',
'',
'TRANSLATIONS :',
'TRANSLATIONS MANAGEMENT : Patrick CHEVALLEY ',
'',
'',
'HOT LINE :',
'SOFTWARE : Patrick CHEVALLEY ',
'OTHERS : Christian LEGRAND',
'',
'PROMOTION :',
'SWITZERLAND : Patrick CHEVALLEY ',
'FRANCE : Christian LEGRAND',
'OTHERS COUNTRIES : Christian LEGRAND',
'',
'CD PRODUCTION :',
'BURNING AND PRINTING : Christian LEGRAND',
'CD EXPEDITION : Christian LEGRAND',
'',
'USE LICENSES :',
'LICENSES AUTHORIZATIONS : Christian LEGRAND & Patrick CHEVALLEY ',
'LICENSES MANAGEMENT : Christian LEGRAND',
''));


procedure Tsplash.Timer1Timer(Sender: TObject);
begin
  Timer1.enabled:=false;
  release;
end;

procedure Tsplash.FormCreate(Sender: TObject);
begin
ScaleFormForFontSize(self,96);
closing:=false;
label1.caption:=rstitle;
end;

procedure Tsplash.Image1DblClick(Sender: TObject);
begin
closing:=true;
end;

Procedure Tsplash.Animate;
var i,l,p :integer;
    buf : string;
begin
if uppercase(language)='FR' then l:=1 else l:=2;
buf:='';
for i:=1 to 6 do begin
  buf:=buf+cpylst[l,i]+crlf;
end;
label3.Caption:=buf;
application.ProcessMessages;
i:=6;
repeat
  inc(i);
  if i>nlin then i:=1;
  p:=pos(crlf,buf);
  if p>0 then buf:=copy(buf,p+2,9999);
  buf:=buf+cpylst[l,i]+crlf;
  label3.Caption:=buf;
  application.ProcessMessages;
  sleep(500);
until closing;
Close;
end;

procedure Tsplash.FormShow(Sender: TObject);
begin
ClientHeight:=panel1.Height;
label2.Caption:=VersionName+' '+Splashversion;
label6.Caption:=compile_version;
 if SplashTimer then begin
     label5.Caption:=transmsg;
     label6.visible:=false;
     Timer1.enabled:=true
  end else begin
     label5.Caption:='';
     label6.visible:=true;
     cpylst[1,trlin]:=transmsg;
     cpylst[2,trlin]:=transmsg;
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
if closing then canclose:=true
else begin
  closing:=true;
  canclose:=false;
end;  
end;

procedure Tsplash.FormClose(Sender: TObject; var Action: TCloseAction);
begin
action:=caFree;
end;

end.
