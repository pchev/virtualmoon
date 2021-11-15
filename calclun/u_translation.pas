unit u_translation;

{$mode objfpc}{$H+}

interface

uses gettext, translations, u_util, u_constant,
  Classes, SysUtils;

function GetDefaultLanguage:string;
function Translate(lang : string = ''; lang2 : string = ''):string;

resourcestring

rsConfiguratio = 'Configuration';
rsLatitude = 'Latitude';
rsLongitude = 'Longitude';
rsAltitude = 'Altitude';
rsLanguage = 'Language';
rsOK = 'OK';
rsCancel = 'Cancel';
rsObservatory = 'Observatory';
rsN = 'N';
rsS = 'S';
rsE = 'E';
rsW = 'W';
rsDateTime = 'Date / Time';
rsCountry = 'Country';
rsTimeZone = 'Time Zone';

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
 pofile:=format(slash(appdir)+slash('language')+'calclun.%s.po',[lang]);
 if FileExists(pofile) then result:=lang
                       else result:=lang2;
 TranslateUnitResourceStrings('u_translation',slash(appdir)+slash('language')+'calclun.%s.po',lang,lang2);
end;

end.

