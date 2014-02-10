unit u_translation;
{
Copyright (C) 2007 Patrick Chevalley

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

{$mode objfpc}{$H+}

interface

uses  translations, wince_func,  dialogs,
  Classes, SysUtils;

procedure GetDefaultLanguage(var buf1,buf2: string);
function Translate(lang : string = ''):string;

var appdir: string;

resourcestring
  rsObservatory = 'Observatory';
  rsGeocentric = 'Geocentric';
  rsTimeZone = 'Time Zone';
  rsPhraseFormat = 'PhraseFormatEnglish';
  rsDegreeSymbol = 'degree symbol';
  rsMinutesSymbol = 'minutes symbol';
  rsSecondsSymbol = 'seconds symbol';
  rsFrom = 'from';
  rsDate = 'Date';
  rsTime = 'Time';
  rsRightAscensi = 'Right Ascension';
  rsDeclination = 'Declination';
  rsDistance = 'Distance';
  rsKm = 'Km';
  rsApparentDiam = 'Apparent diameter';
  rsColongitude = 'Colongitude';
  rsPhase = 'Phase';
  rsLunation = 'Lunation';
  rsDays = 'days';
  rsIllumination = 'Illumination';
  rsField = 'Field';
  rsLibrationInL = 'Libration in Latitude';
  rsLibrationInL2 = 'Libration in Longitude';
  rsPositionAngl = 'Position angle';
  rsAzimuth = 'Azimuth';
  rsAltitude = 'Altitude';
  rsRise = 'Rise';
  rsTransit = 'Transit';
  rsSet = 'Set';
  rsRiseAzimuth = 'Rise azimuth';
  rsTransitAltit = 'Transit Altitude';
  rsSetAzimuth = 'Set azimuth';
  rsType = 'Type';
  rsGeologicalPe = 'Geological period';
  rsSize = 'Size';
  rsDimension = 'Dimension';
  rsMi = 'Mi';
  rsHeight = 'Height';
  rsM = 'm';
  rsFt = 'ft';
  rsHeightWideRa = 'Height/Wide ratio';
  rsDescription = 'Description';
  rsObservation = 'Observation';
  rsInterest = 'Interest';
  rsObservationP = 'Observation period';
  rsOr = 'or';
  rsMinimalInstr = 'Minimal Instrument';
  rsPosition = 'Position';
  rsLongitude = 'Longitude';
  rsLatitude = 'Latitude';
  rsQuadrant = 'Quadrant';
  rsArea = 'Area';
  rsAtlas = 'Atlas';
  rsRuklMap = 'Rukl map';
  rsViscardyPage = 'Viscardy page';
  rsHatfieldMap = 'Hatfield map';
  rsWestfallAtla = 'Westfall Atlas';
  rsCharlesWoodA = 'Charles Wood article';
  rsNameOrigine = 'Name Origine';
  rsDetailedName = 'Detailed Name';
  rsBornIn = 'born in';
  rsBornAt = 'Born at';
  rsIn = 'in';
  rsDeadAt = 'Dead at';
  rsImportantFac = 'Important Facts';
  rsNameAuthor = 'Name Author';
  rsNameByLangre = 'Name by Langrenus';
  rsNameByHeveli = 'Name by Hevelius';
  rsNameByRiccio = 'Name by Riccioli';
  rsVirtualMoonA = 'Virtual Moon Atlas';
  rsQuit = 'Quit';
  rsConfiguratio = 'Configuration';
  rsDisplay = 'Display';
  rsEphemeris = 'Ephemeris';
  rsCalendar = 'Calendar';
  rsSearch = 'Search';
  rsHelp = 'Help';
  rsAbout = 'About';
  rsInformation = 'Information';
  rsPersonalNote = 'Personal Notes';
  rsPhotos = 'Photos';
  rsComments = 'Audio comment';
  rsCountry = 'Country';
  rsLanguage = 'Language';
  rsLanguageChan = '(language change only after the program is restarted)';
  rsMenu = 'Menu';
  rsMap = 'Map';
  rsNow = 'Now';
  rsMon = 'Mon';
  rsTue = 'Tue';
  rsWed = 'Wed';
  rsThu = 'Thu';
  rsFri = 'Fri';
  rsSat = 'Sat';
  rsSun = 'Sun';
  rsJulianDay = 'Julian day';
  rsTranslator_N = '%Translator_Name';
  rsPhaseCalenda = 'Phase Calendar';
  rsNewMoon = 'New Moon';
  rsFirstQuarter = 'First Quarter';
  rsFullMoon = 'Full Moon';
  rsLastQuarter = 'Last Quarter';
  rsDisplaySetti = 'Display Setting';
  rsTextureQuali = 'Texture quality';
  rsTextSize = 'Text size';
  rsLabelSize = 'Label size';
  rsLabelDensity = 'Label density';
  rsLabelColor = 'Label color';
  rsCenterLabelO = 'Center label on the formation';
  rsNotEnoughMem = 'Not enough memory!';
  rsSelection = 'Selection';
  rsNotes = 'Notes';

implementation

procedure GetDefaultLanguage(var buf1,buf2: string);
begin
 GetLanguageIDs(buf1,buf2);
end;

function Translate(lang : string = ''):string;
var lang2,pofile: string;
begin
 lang2:='';
 if lang='' then GetDefaultLanguage(lang,lang2);
 pofile:=format(slash(appdir)+slash('language')+'pocketlun.%s.po',[lang]);
 if FileExists(pofile) then result:=lang
 else begin
    pofile:=format(slash(appdir)+slash('language')+'pocketlun.%s.po',[lang2]);
    if FileExists(pofile) then result:=lang2
    else begin
        pofile:=format(slash(appdir)+slash('language')+'pocketlun.%s.po',['en']);
        result:='en';
    end;
 end;
 // translate messages
 TranslateUnitResourceStrings('u_translation',slash(appdir)+slash('language')+'pocketlun.%s.po',result,'');
 // translate LCL messages
 TranslateUnitResourceStrings('LCLStrConsts',slash(appdir)+slash('language')+'lclstrconsts.%s.po',result,'');
end;

end.

