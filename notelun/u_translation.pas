unit u_translation;

{$mode objfpc}{$H+}

interface

uses gettext, translations, u_util, u_constant,
  Classes, SysUtils;

function GetDefaultLanguage:string;
function Translate(lang : string = ''; lang2 : string = ''):string;

resourcestring

rslanguage='English';
rstitle='NoteLun';
rsConfiguratio = 'Configuration';
rsLatitude = 'Latitude';
rsLongitude = 'Longitude';
rsAltitude = 'Altitude';
rsLang = 'Language';
rsOK = 'OK';
rsCancel = 'Cancel';
rsObservatory = 'Observatory';
rsDateTime = 'Date / Time';
rsCountry = 'Country';
rsTimeZone = 'Time Zone';
rsRA2000 = 'RA 2000';
rsDE2000 = 'DE 2000';
rsRAApparent = 'RA apparent';
rsDEApparent = 'DE apparent';
rsDistance = 'Distance';
rsDiameter = 'Diameter';
rsPhase = 'Phase';
rsLunation = 'Lunation';
rsIllumination = 'Illumination';
rsColongitude = 'Colongitude';
rsSubSolarLati = 'Sub-solar latitude';
rsLibrationLon = 'Libration longitude';
rsLibrationLat = 'Libration latitude';
rsStartTime = 'Start time';
rsEndTime = 'End time';
rsClose = 'Close';
rsDate = 'Date';
rsAzimut = 'Azimuth';
rsSetup = 'Setup';
rsYear = 'Year';
rsMonth = 'Month';

implementation

function GetDefaultLanguage:string;
var buf1,buf2: string;
begin
 GetLanguageIDs(buf1,buf2);
 if buf2<>'' then result:=buf2
    else result:=buf1;
end;

function Translate(lang : string = ''; lang2 : string = ''):string;
var pofile: string;
begin
 if lang='' then lang:=GetDefaultLanguage;
 // translate LCL messages
 TranslateUnitResourceStrings('LCLStrConsts',slash(appdir)+slash('language')+'lclstrconsts.%s.po',lang,lang2);
 // translate messages
 pofile:=format(slash(appdir)+slash('language')+'notelun.%s.po',[lang]);
 if FileExists(pofile) then result:=lang
                       else result:=lang2;
 TranslateUnitResourceStrings('u_translation',slash(appdir)+slash('language')+'notelun.%s.po',lang,lang2);
end;

end.

