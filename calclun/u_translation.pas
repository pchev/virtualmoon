unit u_translation;

{$mode objfpc}{$H+}

interface

uses gettext, translations, u_util, u_constant,
  Classes, SysUtils;

function GetDefaultLanguage:string;
function Translate(lang : string = ''; lang2 : string = ''):string;

resourcestring

rslanguage='English';
rstitle='Calclun';
rsConfiguratio = 'Configuration';
rsLatitude = 'Latitude';
rsLongitude = 'Longitude';
rsAltitude = 'Altitude';
rsLang = 'Language';
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
rsPA = 'PA';
rsRiseTime = 'Rise time';
rsSetTime = 'Set time';
rsNewMoon = 'New Moon';
rsFirstQuarter = 'First quarter';
rsFullMoon = 'Full Moon';
rsLastQuarter = 'Last quarter';
rsEast = 'East';
rsWest = 'West';
rsNorth = 'North';
rsSouth = 'South';
rsDay = 'Day';
rsHour = 'Hour';
rsRise = 'Rise';
rsSet = 'Set';
rsStartTime = 'Start time';
rsEndTime = 'End time';
rsLibrLon = 'Libr. lon.';
rsLibrLat = 'Libr. lat.';
rsClose = 'Close';
rsDate = 'Date';
rsByYear = 'by year';
rsByMonth = 'by month';
rsByDay = 'by day';
rsAlwaysUp = 'Always up';
rsNeverRise = 'Never rise';
rsAzimut = 'Azimuth';
rsElevation = 'Elevation';
rsAlt = 'Alt';
rsLibration = 'Libration';
rsSetup = 'Setup';
rsYear = 'Year';
rsMonth = 'Month';
rsTerminator = 'Terminator';
rsToday = 'Today';
rsSelectGraph = 'Select graph';
rsTwilightAndM = 'Twilight and Moon visibility';
rsSunRise = 'Sun rise';
rsSunSet = 'Sun set';
rsSearchFormat = 'Search formation';
rsSearch = 'Search';
rsLunarCoordin = 'Lunar coordinates';
rsSunElevation = 'Sun elevation';
rsMinimum = 'Minimum';
rsMaximum = 'Maximum';
rsTime = 'Time';
rsJanuary = 'January';
rsFebruary = 'February';
rsMarch = 'March';
rsApril = 'April';
rsMay = 'May';
rsJune = 'June';
rsJuly = 'July';
rsAugust = 'August';
rsSeptember = 'September';
rsOctober = 'October';
rsNovember = 'November';
rsDecember = 'December';
rsMoonPhase = 'Moon phase';
rsLibrationExt = 'Libration extrema';


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

