unit u_translation_database;

{
  This file is to be maintened in datlun
  and copied to virtualmoon if changed
}

{$mode objfpc}{$H+}

interface

uses gettext, translations, u_util, u_constant,
  Classes, SysUtils;

function GetDefaultLanguage:string;
function Translate(lang : string = ''; lang2 : string = ''):string;

const
  nrsb=9;

resourcestring
warning1= ' Note to translator:';
warning2= ' The following text must be the same as the content of the TYPE column in the database you use for your language.';
warning3= ' If you not translate the database keep the English text here!';
rscol_1='Crater';
rscol_2='Craterlet';
rscol_3='Crater chain';
rscol_4='Walled plain';
rscol_5='Plain';
rscol_6='Mountain';
rscol_7='Mountain range';
rscol_8='Dome';
rscol_9='Dome system';
rscol_10='Wrinkle ridge';
rscol_11='Wrinkle ridges network';
rscol_12='Scarp';
rscol_13='Rille';
rscol_14='Rilles network';
rscol_15='Valley';
rscol_16='Marsh';
rscol_17='Lake';
rscol_18='Sea';
rscol_19='Cape';
rscol_20='Bay';
rscol_21='Ejecta';
rscol_22='Probe ';
rscol_23='Human mission';
rscol_24='Inert equipment';
rscol_25='Pyroclastic area';
rscol_26='Irregular Mare Patch';
rscol_27='Lunar pit';
rscol_28='Impact basin';
rscol_29='Contemporary meteoritic impact';
rsb_1='Not named';
rsb_2='Not indicated';
rsb_3='Not evaluated';
rsb_4='Not observable';
rsb_5='No map';
rsb_6='No map or picture';
rsb_7='No article';
rsb_8='Not defined';
rsb_9='Unknown in this period';


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
 // translate messages
 pofile:=format(slash(appdir)+slash('language')+'vmadatabase.%s.po',[lang]);
 if FileExists(pofile) then result:=lang
                       else result:=lang2;
 TranslateUnitResourceStrings('u_translation_database',slash(appdir)+slash('language')+'vmadatabase.%s.po',lang,lang2);
end;

end.

