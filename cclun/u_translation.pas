unit u_translation;

{$mode objfpc}{$H+}

interface

uses gettext, translations, u_constant,
  Classes, SysUtils;

function GetDefaultLanguage:string;
function Translate(lang : string = ''; lang2 : string = ''):string;

resourcestring
rstranslator='Translation: P. Chevalley / C. Legrand';
rslanguage='English';
rstitle='Virtual Lunar Atlas Command Center';
rshelp_prefix='EN';
rsVirtualLunar = 'Virtual Lunar Atlas';
rsSpecialEditi = 'Special edition "10th anniversary"';
rsDocumentatio = 'Documentation';
rsTutorial = 'Tutorial ';
rsQuit = 'Quit';
rsExploreTheLu = 'Explore the Lunar Atlas maps';
rsSearchInTheA = 'Search in the Atlas databases';
rsBrowseTheAtl = 'Browse the Atlas pictures';
rsTheAtlasInte = 'The Atlas Internet resources';
rsExitTheComma = 'Exit the Command Center';
rsReadTheQuick = 'Read the quick guide';
rsReadTheFullD = 'Read the full documentation';
rsAtLunDocumen = 'AtLun documentation';
rsDatLunDocume = 'DatLun documentation';
rsPhotLunDocum = 'PhotLun documentation';
rsWebLunDocume = 'WebLun documentation';
rsNoteLunDocum = 'NoteLun documentation';
rsCalcLunDocum = 'CalcLun documentation';
rsLunarCalcula = 'Lunar calculation';

implementation

function GetDefaultLanguage:string;
var buf1,buf2: string;
begin
 GetLanguageIDs(buf1,buf2);
 if buf2<>'' then result:=buf2
    else result:=buf1;
end;

Function Slash(nom : string) : string;
begin
result:=trim(nom);
if copy(result,length(nom),1)<>PathDelim then result:=result+PathDelim;
end;

function Translate(lang : string = ''; lang2 : string = ''):string;
var pofile: string;
begin
 if lang='' then lang:=GetDefaultLanguage;
 // translate LCL messages
 TranslateUnitResourceStrings('LCLStrConsts',slash(appdir)+slash('language')+'lclstrconsts.%s.po',lang,lang2);
 // translate messages
 pofile:=format(slash(appdir)+slash('language')+'cclun.%s.po',[lang]);
 if FileExists(pofile) then result:=lang
                       else result:=lang2;
 TranslateUnitResourceStrings('u_translation',slash(appdir)+slash('language')+'cclun.%s.po',lang,lang2);
end;

end.

