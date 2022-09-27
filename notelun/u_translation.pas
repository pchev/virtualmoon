unit u_translation;

{$mode objfpc}{$H+}

interface

uses gettext, translations, u_util, u_constant,
  Classes, SysUtils;

function GetDefaultLanguage:string;
function Translate(lang : string = ''; lang2 : string = ''):string;

resourcestring

rslanguage='English';
rshelp_prefix='EN';
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
rsNewObservati = 'New observation note';
rsNewInformati = 'New information note';
rsEditNote = 'Edit note';
rsDeleteNote = 'Delete note';
rsPrintNote = 'Print note';
rsPrintListOfN = 'Print list of notes';
rsQuit = 'Quit';
rsSortByFormat = 'Sort by formation';
rsSortByDate = 'Sort by date';
rsSortByType = 'Sort by type';
rsLocation = 'Location';
rsObserver = 'Observer';
rsInstrument = 'Instrument';
rsBarlow = 'Barlow';
rsEyepiece = 'Eyepiece';
rsCamera = 'Camera';
rsListAndNotes = 'List and notes';
rsHelp = 'Help';
rsAbout = 'About';
rsObservationN = 'Observation note';
rsCircumstance = 'Circumstance';
rsObservingPla = 'Observing place';
rsObservationS = 'Observation start';
rsObservationE = 'Observation end';
rsMeteorologic = 'Meteorological condition';
rsSeeing = 'Seeing';
rsGear = 'Gear';
rsBarlowReduce = 'Barlow/reducer';
rsEphemeris = 'Ephemeris';
rsPositionAngl = 'Position angle';
rsNote = 'Note';
rsFiles = 'Files';
rsAdd = 'Add';
rsDelete = 'Delete';
rsPast = 'Past';
rsFormation = 'Formation';
rsType = 'Type';
rsAll = 'All';
rsSelectFormat = 'Select formation';
rsNew = 'New';
rsSave = 'Save';
rsEdit = 'Edit';
rsInformationN = 'Information note';
rsNoteAuthor = 'Note author';
rsFile = 'File';
rsManage = 'Manage';
rsSearch = 'Search';
rsTypeOfNote = 'Type of note';
rsDeleteNote2 = 'Delete note?';
rsMagnificatio = 'Magnification';
rsElevation = 'Elevation';
rsDays = 'days';
rsNoteIsModifi = 'Note is modified do you want to save the change?';
rsNotesList = 'Notes list';
rsRemoveFile = 'Remove file';
rsName = 'Name';
rsEW = 'E/W';
rsElevationMet = 'Elevation [meter]';
rsTimezone2 = 'Timezone';
rsFirstName = 'First name';
rsPseudo = 'Pseudo';
rsContact = 'Contact';
rsDiameterMm = 'Diameter [mm]';
rsFocalMm = 'Focal [mm]';
rsFD = 'F/D';
rsBarlowReduce2 = 'Barlow/Reducer';
rsPower = 'Power';
rsField = 'Field';
rsHorizontalRe = 'Horizontal resolution [pixel]';
rsVerticalReso = 'Vertical resolution [pixel]';
rsPixelSize = 'Pixel size';
rsAddRow = 'Add row';
rsDefaultListS = 'Default list sorting options';
rsByFormationN = 'By formation name';
rsByDate = 'By date';
rsByTypeOfNote = 'By type of note';
rsReverseSortO = 'Reverse sort order';
rsNoteOptions = 'Note options';
rsShowEphemeri = 'Show ephemeris';
rsPrintOptions = 'Print options';
rsFontToPrintN = 'Font to print note text';
rsFixedPitchFo = 'Fixed pitch font to print list of notes';
rsW = 'W';
rsE = 'E';
rsSelectLocati = 'Select location';
rsSelectObserv = 'Select observer';
rsSelectInstru = 'Select instrument';
rsSelectEyepie = 'Select eyepiece';
rsSelectCamera = 'Select camera';
rsSelectFileAt = 'Select file attachment';
rsSelectNoteTe = 'Select note text';
rsNoteTextToSe = 'Note text to search';
rsFileNameFilt = 'File name filter';
rsExport = 'Export';
rsNotesToExpor = 'Notes to export';
rsReplaceLineB = 'Replace line break character by';
rsExportToCSV = 'Export to CSV';
rsExportInform = 'Export information notes as';
rsExportObserv = 'Export observation notes as';

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

