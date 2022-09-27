unit u_translation;

{$mode objfpc}{$H+}

interface

uses gettext, translations, u_util, u_constant,
  Classes, SysUtils;

function GetDefaultLanguage:string;
function Translate(lang : string = ''; lang2 : string = ''):string;

resourcestring
  rshelp_prefix='EN';
  rsFile = 'File';
  rsLibrary = 'Library';
  rsSearch = 'Search';
  rsQuit = 'Quit';
  rsVignettes = 'Vignettes';
  rsSize = 'Size';
  rsSmall = 'Small';
  rsMedium = 'Medium';
  rsLarge = 'Large';
  rsImages = 'Images';
  rsNumberOfWind = 'Number of window';
  rsDatabase = 'Database';
  rsShowOnMap = 'Show on map';
  rsAllLibraries = 'All libraries';
  rsFormationNam = 'Formation name';
  rsSelection = 'Selection';
  rsOK = 'OK';
  rsAll = 'All';
  rsZoom = 'Zoom';
  rsSort = 'Sort';
  rsByName = 'by name';
  rsByLibrary = 'by library';
  rsCloseAllWind = 'Close all window';
  rsImageMirror = 'Image mirror';
  rsNone1 = 'None';
  rsEastWest = 'East<->West';
  rsNorthSouth = 'North<->South';
  rsHelp = 'Help';
  rsAbout = 'About ...';
  rsName = 'Name';
  rsFolder = 'Folder';
  rsRotation = 'Rotation';
  rsImageFolders = 'Image folders:';
  rsCancel = 'Cancel';
  rsLibrarySetti = 'Library Setting';
  rsRotateClockw = 'Rotate clockwise';
  rsRotateCounte = 'Rotate counterclockwise';
  rsBrighter = 'Brighter';
  rsDarker = 'Darker';
  rsIncreaseCont = 'Increase Contrast';
  rsReduceContra = 'Reduce Contrast';
  rsVerticalMirr = 'Vertical Mirror';
  rsHorizontalMi = 'Horizontal Mirror';
  rsAdjustToWind = 'Adjust to window';
  rsFullSize = 'Full size';
  rsResetDefault = 'Reset default setting';
  rsSaveSetting = 'Save setting';
  rsTranslatedBy = 'Translated by :';
  rsView = 'View';

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
 pofile:=format(slash(appdir)+slash('language')+'photlun.%s.po',[lang]);
 if FileExists(pofile) then result:=lang
                       else result:=lang2;
 TranslateUnitResourceStrings('u_translation',slash(appdir)+slash('language')+'photlun.%s.po',lang,lang2);
end;

end.

