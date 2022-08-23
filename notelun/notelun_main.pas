unit notelun_main;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef mswindows}
    Windows, ShlObj,
  {$endif}
  dbutil, u_constant, u_util, libsql, cu_tz, passql, passqlite, UniqueInstance, notelun_setup,
  LCLVersion, IniFiles, u_translation, pu_search, pu_date, LazUTF8, Clipbrd, cu_planet, u_projection, math,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, Grids, ComCtrls, StdCtrls, Buttons, EditBtn, ExtDlgs, Types;

type

  { Tf_notelun }

  Tf_notelun = class(TForm)
    BtnAddInfoFile: TSpeedButton;
    BtnDelInfoFile: TSpeedButton;
    BtnListNew: TSpeedButton;
    BtnListSelection: TSpeedButton;
    BtnPastObsFile: TSpeedButton;
    BtnDelObsFile: TSpeedButton;
    BtnChangeInfoDate: TSpeedButton;
    BtnAddObsFile: TSpeedButton;
    BtnEdit: TSpeedButton;
    BtnDelete: TSpeedButton;
    BtnListAll: TSpeedButton;
    BtnPastInfoFile: TSpeedButton;
    BtnSearchFormation1: TSpeedButton;
    CalendarDialog1: TCalendarDialog;
    ImageList1: TImageList;
    Label10: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label35: TLabel;
    ObsPa: TLabel;
    ObsIllum: TLabel;
    ObsSubsolarLat: TLabel;
    ObsCameraFov: TLabel;
    ObsLocationDetailRO: TLabel;
    ObsPowerRO: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    ObsCameraRO: TLabel;
    ObsEypieceRO: TLabel;
    ObsBarlowRO: TLabel;
    ObsInstrumentRO: TLabel;
    ObsGear1: TGroupBox;
    ObsCameraFovRO: TLabel;
    ObsSeeingRO: TLabel;
    ObsMeteoRO: TLabel;
    ObsEndRO: TLabel;
    ObsStartRO: TLabel;
    ObsObserverRO: TLabel;
    ObsLocationRO: TLabel;
    ObsCircumstance1: TGroupBox;
    ObsFiles: TStringGrid;
    ObsEyepiece: TComboBox;
    InfoFiles: TStringGrid;
    Label21: TLabel;
    MenuItemSetupBarlow: TMenuItem;
    ObsInstrument: TComboBox;
    ObsBarlow: TComboBox;
    ObsCamera: TComboBox;
    ObsMeteo: TEdit;
    ObsSeeing: TEdit;
    ObsEnd: TEdit;
    ObsStart: TEdit;
    Label20: TLabel;
    MenuItemHelp: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemSetupLocation: TMenuItem;
    MenuItemSetupLastFormation: TMenuItem;
    MenuItemSetupObserver: TMenuItem;
    MenuItemSetupInstrument: TMenuItem;
    MenuItemSetupEyepiece: TMenuItem;
    MenuItemSetupCamera: TMenuItem;
    MenuItemSetupEphemeris: TMenuItem;
    MenuItemSetupFont: TMenuItem;
    MenuItemSetupList: TMenuItem;
    MenuItemSetupPrint: TMenuItem;
    MenuItemSortFormation: TMenuItem;
    MenuItemSortDate: TMenuItem;
    MenuItemSortType: TMenuItem;
    MenuItemSortPlace: TMenuItem;
    MenuItemSortObserver: TMenuItem;
    MenuItemSortInstrument: TMenuItem;
    MenuItemSortEyepiece: TMenuItem;
    MenuItemSortFileFormat: TMenuItem;
    MenuItemNewObs: TMenuItem;
    MenuItemNewInfo: TMenuItem;
    MenuItemEditNote: TMenuItem;
    MenuItemDeleteNote: TMenuItem;
    MenuItemPrintNote: TMenuItem;
    MenuItemPrintList: TMenuItem;
    MenuItemExport: TMenuItem;
    OpenDialog1: TOpenDialog;
    PanelObsfileEdit: TPanel;
    PanelInfofileEdit: TPanel;
    PCobs: TPageControl;
    PanelObs: TPanel;
    Quit: TMenuItem;
    ObsAlt: TLabel;
    ObsAzimut: TLabel;
    ObsLibrLat: TLabel;
    ObsLibrLon: TLabel;
    ObsColongitude: TLabel;
    ObsLunation: TLabel;
    ObsDiam: TLabel;
    ObsDec: TLabel;
    ObsRA: TLabel;
    ObsObserver: TComboBox;
    ObsLocation: TComboBox;
    ObsFilesBox: TGroupBox;
    ObsPower: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ObsText: TMemo;
    ObsNote: TGroupBox;
    ObsEph: TGroupBox;
    ObsGear: TGroupBox;
    ObsCircumstance: TGroupBox;
    ObsDate: TEdit;
    InfoFilesBox: TGroupBox;
    ObsFormation: TEdit;
    InfoNote: TGroupBox;
    InfoAuthor: TEdit;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    InfoText: TMemo;
    MenuFile: TMenuItem;
    MenuSetup: TMenuItem;
    MenuHelp: TMenuItem;
    MenuManage: TMenuItem;
    InfoFormation: TEdit;
    InfoDate: TEdit;
    PageControl1: TPageControl;
    PanelObsTop: TPanel;
    PanelTopRight: TPanel;
    PanelTopLeft: TPanel;
    PanelInfoTop: TPanel;
    PanelStatus: TPanel;
    PanelList: TPanel;
    PanelListBottom: TPanel;
    PanelRight: TPanel;
    PanelLeft: TPanel;
    PanelTop: TPanel;
    BtnSave: TSpeedButton;
    ListNotes: TStringGrid;
    ButtonAtlun: TSpeedButton;
    ButtonPhotlun: TSpeedButton;
    ButtonDatlun: TSpeedButton;
    ButtonWeblun: TSpeedButton;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    BtnSearchFormation: TSpeedButton;
    BtnDateEnd: TSpeedButton;
    BtnDateStart: TSpeedButton;
    Splitter1: TSplitter;
    PCRW: TTabSheet;
    PCRO: TTabSheet;
    TabSheetObservation: TTabSheet;
    TabSheetInformation: TTabSheet;
    UniqueInstance1: TUniqueInstance;
    procedure BtnAddInfoFileClick(Sender: TObject);
    procedure BtnAddObsFileClick(Sender: TObject);
    procedure BtnChangeInfoDateClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnDelInfoFileClick(Sender: TObject);
    procedure BtnDelObsFileClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure BtnListAllClick(Sender: TObject);
    procedure BtnListNewClick(Sender: TObject);
    procedure BtnListSelectionClick(Sender: TObject);
    procedure BtnPastInfoFileClick(Sender: TObject);
    procedure BtnPastObsFileClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnSearchFormationClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InfoFilesDblClick(Sender: TObject);
    procedure InfoFilesSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure InfoFilesValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
    procedure InfoNoteChange(Sender: TObject);
    procedure ListNotesDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure ListNotesHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure ListNotesSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure MenuItemNewInfoClick(Sender: TObject);
    procedure MenuItemNewObsClick(Sender: TObject);
    procedure MenuItemSortDateClick(Sender: TObject);
    procedure MenuItemSortFormationClick(Sender: TObject);
    procedure MenuItemSortTypeClick(Sender: TObject);
    procedure MenuSetupObservation(Sender: TObject);
    procedure ChangeObsDate(Sender: TObject);
    procedure ObsBarlowChange(Sender: TObject);
    procedure ObsCameraChange(Sender: TObject);
    procedure ObsEyepieceChange(Sender: TObject);
    procedure ObsFilesDblClick(Sender: TObject);
    procedure ObsFilesSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure ObsFilesValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
    procedure ObsInstrumentChange(Sender: TObject);
    procedure ObsLocationChange(Sender: TObject);
    procedure ObsNoteChange(Sender: TObject);
    procedure ObsObserverChange(Sender: TObject);
    procedure UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
    procedure BtnListNewSelection(sender: TObject);

  private
    tz: TCdCTimeZone;
    Fplanet: TPlanet;
    Finfodate,Fobsdatestart,Fobsdateend: double;
    param : Tstringlist;
    StartVMA,CanCloseVMA,locklist,SortAsc: boolean;
    EditingObservation,EditingInformation,ModifiedObservation,ModifiedInformation,NewInformation,NewObservation: boolean;
    CurrentInfoId, CurrentObsId: int64;
    LastLocation,LastObserver,LastInstrument,LastBarlow,LastEyepiece,LastCamera: Int64;
    CurrentFormation: string;
    CurrentObsFile,CurrentInfoFile,SortCol: integer;
    procedure SetLang;
    procedure GetAppDir;
    procedure ReadConfig;
    procedure WriteConfig;
    Procedure ReadParam(first:boolean=true);
    function  FormatDate(val:string):string;
    procedure ClearList;
    procedure NotesList(formation:string='';prefix:char=' ';fid:int64=0);
    procedure ClearObsBox(box:TComboBox);
    procedure LoadObsBox(table: string;box:TComboBox;  name2:string='');
    procedure LoadObsBoxes;
    function  GetObsBoxIndex(box:TComboBox):int64;
    procedure SetObsBoxIndex(box:TComboBox; lbl:TLabel; id: int64);
    procedure ComputePower;
    procedure ComputeCamera;
    procedure ComputeEphemeris(obs:integer;dt:double);
    procedure ClearInfoNote;
    procedure SetInfoDate(val:string);
    procedure ShowInfoNote(id: int64);
    procedure SetEditInformation(onoff: boolean);
    procedure SaveInformationNote;
    procedure CancelEditInformation;
    procedure NewInformationNote(formation:string='');
    procedure DeleteInformation(id: int64);
    procedure SetInfoFiles(txt: string);
    function  GetInfoFiles: string;

    procedure ClearEphemeris;
    procedure ClearObsNote;
    procedure SetObsDate(val1,val2:string);
    procedure ShowObsNote(id: int64);
    procedure SetEditObservation(onoff: boolean);
    procedure SaveObservationNote;
    procedure CancelEditObservation;
    procedure NewObservationNote(formation:string='');
    procedure DeleteObservation(id: int64);
    procedure SetObsFiles(txt: string);
    function  GetObsFiles: string;

  public

  end;

var
  f_notelun: Tf_notelun;

implementation

{$R *.lfm}

{ Tf_notelun }


procedure Tf_notelun.FormCreate(Sender: TObject);
var inifile:Tmeminifile;
    i: integer;
begin
  DefaultFormatSettings.DateSeparator:='/';
  DefaultFormatSettings.TimeSeparator:=':';
  DefaultFormatSettings.DecimalSeparator:='.';
  compile_time := {$I %DATE%}+' '+{$I %TIME%};
  compile_version := 'Lazarus '+lcl_version+' Free Pascal '+{$I %FPCVERSION%}+' '+{$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%};
  StartVMA:=false;
  CanCloseVMA:=true;
  locklist:=false;
  SortCol:=0;
  SortAsc:=true;
  GetAppDir;
  inifile := Tmeminifile.Create(ConfigFile);
  language:= inifile.ReadString('default', 'lang_po_file', '');
  inifile.Free;
  language:=u_translation.translate(language,'en');
  uplanguage:=UpperCase(language);
  SetLang;
  Fplanet:=TPlanet.Create(self);
  tz:=TCdCTimeZone.Create;
  tz.LoadZoneTab(ZoneDir+'zone.tab');
  Plan404    := nil;
  Plan404lib := LoadLibrary(lib404);
  if Plan404lib <> 0 then
  begin
    Plan404 := TPlan404(GetProcAddress(Plan404lib, 'Plan404'));
  end;
  if @Plan404=nil then begin
     MessageDlg('Could not load library '+lib404+crlf
               +'Please try to reinstall the program.',
               mtError, [mbAbort], 0);
     Halt;
  end;
  dbm:=TLiteDB.Create(self);
  dbnotes:=TLiteDB.Create(self);
  ReadConfig;
  param:=Tstringlist.Create;
  param.clear;
  if paramcount>0 then begin
   for i:=1 to paramcount do begin
      param.Add(paramstr(i));
   end;
  end;
end;

procedure Tf_notelun.ReadConfig;
var inif: TMemIniFile;
    section: string;
begin
  inif := Tmeminifile.Create(ConfigFile);
  section:='Notelun';
  LastLocation:=inif.ReadInt64(section, 'LastLocation', 0);
  LastObserver:=inif.ReadInt64(section, 'LastObserver', 0);
  LastInstrument:=inif.ReadInt64(section, 'LastInstrument', 0);
  LastBarlow:=inif.ReadInt64(section, 'LastBarlow', 0);
  LastEyepiece:=inif.ReadInt64(section, 'LastEyepiece', 0);
  LastCamera:=inif.ReadInt64(section, 'LastCamera', 0);
  inif.Free;
end;

procedure Tf_notelun.WriteConfig;
var inif: TMemIniFile;
    section: string;
begin
  inif := Tmeminifile.Create(ConfigFile);
  section:='Notelun';
  inif.WriteInt64(section, 'LastLocation', LastLocation);
  inif.WriteInt64(section, 'LastObserver', LastObserver);
  inif.WriteInt64(section, 'LastInstrument', LastInstrument);
  inif.WriteInt64(section, 'LastBarlow', LastBarlow);
  inif.WriteInt64(section, 'LastEyepiece', LastEyepiece);
  inif.WriteInt64(section, 'LastCamera', LastCamera);
  inif.UpdateFile;
  inif.Free;
end;

procedure Tf_notelun.FormDestroy(Sender: TObject);
begin
  ClearList;
  ClearObsBox(ObsLocation);
  ClearObsBox(ObsObserver);
  ClearObsBox(ObsInstrument);
  ClearObsBox(ObsBarlow);
  ClearObsBox(ObsEyepiece);
  ClearObsBox(ObsCamera);
  dbm.free;
  dbnotes.free;
  tz.Free;
  Fplanet.Free;
  DatabaseList.free;
  param.free;
end;

procedure Tf_notelun.FormShow(Sender: TObject);
var i: integer;
begin
  for i:=1 to maxdbn do usedatabase[i]:=false;
  usedatabase[1]:=true;
  DatabaseList:=Tstringlist.Create;
  LoadDB(dbm);
  LoadNotelunDB(dbnotes);
  f_search.dbm:=dbm;
  ClearObsNote;
  ClearInfoNote;
  PageControl1.ActivePageIndex:=0;
  CurrentFormation:='';
  LoadObsBoxes;
  ReadParam;
  if CurrentFormation='' then NotesList;
end;

procedure Tf_notelun.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteConfig;
end;

procedure Tf_notelun.SetLang;
begin
  ldeg:='°';
  lmin:='''';
  lsec:='"';
  MenuItemSetupLocation.Caption:='Location';
  MenuItemSetupObserver.Caption:='Observer';
  MenuItemSetupInstrument.Caption:='Instrument';
  MenuItemSetupBarlow.Caption:='Barlow';
  MenuItemSetupEyepiece.Caption:='Eyepiece';
  MenuItemSetupCamera.Caption:='Camera';
end;

procedure Tf_notelun.GetAppDir;
var
  buf: string;
  inif: TMeminifile;
{$ifdef darwin}
  i:      integer;
{$endif}
{$ifdef mswindows}
  PIDL:   PItemIDList;
  Folder: array[0..MAX_PATH] of char;
{$endif}
begin
{$ifdef darwin}
  appdir := getcurrentdir;
  if (not directoryexists(slash(appdir) + slash('Textures'))) then
  begin
    appdir := ExtractFilePath(ParamStr(0));
    i      := pos('.app/', appdir);
    if i > 0 then
    begin
      appdir := ExtractFilePath(copy(appdir, 1, i));
    end;
  end;
{$else}
  appdir     := getcurrentdir;
  if not DirectoryExists(slash(appdir)+slash('Textures')) then begin
     appdir:=ExtractFilePath(ParamStr(0));
  end;
{$endif}
  privatedir := DefaultPrivateDir;
{$ifdef unix}
  appdir     := expandfilename(appdir);
  bindir     := slash(appdir);
  privatedir := expandfilename(PrivateDir);
  configfile := expandfilename(Defaultconfigfile);
  CdCconfig  := ExpandFileName(DefaultCdCconfig);
{$endif}
{$ifdef mswindows}
  buf:='';
  SHGetSpecialFolderLocation(0, CSIDL_LOCAL_APPDATA, PIDL);
  SHGetPathFromIDList(PIDL, Folder);
  buf:=systoutf8(Folder);
  buf:=trim(buf);
  buf:=SafeUTF8ToSys(buf);
  if buf='' then begin  // old windows version
     SHGetSpecialFolderLocation(0, CSIDL_APPDATA, PIDL);
     SHGetPathFromIDList(PIDL, Folder);
     buf:=trim(Folder);
  end;
  if buf='' then begin
     MessageDlg('Unable to create '+privatedir,
               mtError, [mbAbort], 0);
     Halt;
  end;
  privatedir := slash(buf) + privatedir;
  configfile := slash(privatedir) + Defaultconfigfile;
  CdCconfig  := slash(buf) + DefaultCdCconfig;
  bindir:=slash(appdir);
{$endif}

  if fileexists(configfile) then begin
    inif:=TMeminifile.create(configfile);
    try
    buf:=inif.ReadString('default','Install_Dir',appdir);
    if Directoryexists(slash(buf)+slash('Textures')) then appdir:=noslash(buf);
    finally
     inif.Free;
    end;
  end;
  if not directoryexists(privatedir) then
    CreateDir(privatedir);
  if not directoryexists(privatedir) then
    forcedirectories(privatedir);
  if not directoryexists(privatedir) then
  begin
    privatedir := appdir;
  end;
  Tempdir := slash(privatedir) + DefaultTmpDir;
  if not directoryexists(TempDir) then
    CreateDir(TempDir);
  if not directoryexists(TempDir) then
    forcedirectories(TempDir);
  DBdir := Slash(privatedir) + 'database';
  if not directoryexists(DBdir) then
    CreateDir(DBdir);
  if not directoryexists(DBdir) then
    forcedirectories(DBdir);
  // Be sur the Textures directory exists
  if (not directoryexists(slash(appdir) + slash('Textures'))) then
  begin
    // try under the current directory
    buf := GetCurrentDir;
    if (directoryexists(slash(buf) + slash('Textures'))) then
      appdir := buf
    else
    begin
      // try under the program directory
      buf := ExtractFilePath(ParamStr(0));
      if (directoryexists(slash(buf) + slash('Textures'))) then
        appdir := buf
      else
      begin
        // try share directory under current location
        buf := ExpandFileName(slash(GetCurrentDir) + SharedDir);
        if (directoryexists(slash(buf) + slash('Textures'))) then
          appdir := buf
        else
        begin
          // try share directory at the same location as the program
          buf := ExpandFileName(slash(ExtractFilePath(ParamStr(0))) + SharedDir);
          if (directoryexists(slash(buf) + slash('Textures'))) then
            appdir := buf
          else
          begin
            MessageDlg('Could not found the application Textures directory.' +
              crlf + 'Please try to reinstall the program at a standard location.',
              mtError, [mbAbort], 0);
            Halt;
          end;
        end;
      end;
    end;
  end;
 {$ifndef darwin}
  if not FileExists(slash(bindir)+ExtractFileName(ParamStr(0))) then begin
     bindir := slash(ExtractFilePath(ParamStr(0)));
     if not FileExists(slash(bindir)+ExtractFileName(ParamStr(0))) then begin
        bindir := slash(ExpandFileName(slash(appdir) + slash('..')+slash('..')+'bin'));
        if not FileExists(slash(bindir)+ExtractFileName(ParamStr(0))) then begin
           bindir:='';
        end;
     end;
  end;
  {$endif}
  Maplun  := '"'+bindir + DefaultMaplun+'"';
  Photlun := '"'+bindir + DefaultPhotlun+'"';     // Photlun normally at same location as vma
  Datlun  := '"'+bindir + DefaultDatlun+'"';
  Weblun  := '"'+bindir + DefaultWeblun+'"';
  helpdir := slash(appdir) + slash('doc');
  jpldir  := slash(appdir)+slash('data')+'jpleph';
  // Be sure zoneinfo exists in standard location or in vma directory
  ZoneDir  := slash(appdir) + slash('data') + slash('zoneinfo');
  buf      := slash('') + slash('usr') + slash('share') + slash('zoneinfo');
  if (FileExists(slash(buf) + 'zone.tab')) then
    ZoneDir := slash(buf)
  else
  begin
    buf := slash('') + slash('usr') + slash('lib') + slash('zoneinfo');
    if (FileExists(slash(buf) + 'zone.tab')) then
      ZoneDir := slash(buf)
    else
    begin
      if (not FileExists(slash(ZoneDir) + 'zone.tab')) then
      begin
        MessageDlg('zoneinfo directory not found!' + crlf +
          'Please install the tzdata package.' + crlf +
          'If it is not installed at a standard location create a logical link zoneinfo in virtualmoon data directory.',
          mtError, [mbAbort], 0);
        Halt;
      end;
    end;
  end;
end;

procedure Tf_notelun.UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
var i: integer;
begin
  application.Restore;
  application.BringToFront;
  if ParamCount > 0 then begin
     param.Clear;
     for i:=0 to ParamCount-1 do begin
        param.add(Parameters[i]);
     end;
     ReadParam(false);
  end;
end;

Procedure Tf_notelun.ReadParam(first:boolean=true);
var i : integer;
    id: int64;
    nam,txt: string;
    prefix: char;
begin
nam:='';
prefix:=' ';
id:=0;
i:=0;
while i <= param.count-1 do begin
  if (param[i]='-nx')and first then begin       // when started by vma do not close vma on exit!
     CanCloseVMA:=false;
  end
  else if param[i]='-n' then begin   // set formation name to search
     inc(i);
     if i <= param.count-1 then
        nam:=param[i];
  end
  else if param[i]='-p' then begin   // set note type to search
     inc(i);
     if i <= param.count-1 then
        prefix:=param[i].Chars[0];
  end
  else if param[i]='-i' then begin   //set note id to search
     inc(i);
     if i <= param.count-1 then
        id:=StrToIntDef(param[i],0);
  end
  else if param[i]='-newi' then begin   // create new information note
     inc(i);
     if i <= param.count-1 then
        txt:=param[i]
     else
        txt:='';
     CurrentFormation:=txt;
     NewInformationNote(txt);
  end
  else if param[i]='-newo' then begin   // create new observation note
     inc(i);
     if i <= param.count-1 then
        txt:=param[i]
     else
        txt:='';
     CurrentFormation:=txt;
     NewObservationNote(txt);
  end
  else if param[i]='-quit' then begin  // close current instance
     Close;
  end
  else if param[i]='--' then begin   // last parameter
       break;
  end;
  inc(i);
end;
if (nam<>'')or(id<>0) then begin
  CurrentFormation:=nam;
  NotesList(nam,prefix,id);
end;
end;

procedure Tf_notelun.BtnSearchFormationClick(Sender: TObject);
var p: tpoint;
    ed:tedit;
begin
  case TSpeedButton(sender).tag of
    0: ed:=ObsFormation;
    1: ed:=InfoFormation;
  end;
  p.x:=ed.Left;
  p.y:=ed.Top+ed.Height;
  p:=ed.Parent.ClientToScreen(p);
  f_search.SetFormation(ed.Text);
  f_search.Left:=p.x;
  f_search.Top:=p.y;
  if f_search.ShowModal=mrOK then begin
    ed.Text:=f_search.ListBox1.GetSelectedText;
  end;
end;

procedure Tf_notelun.ChangeObsDate(Sender: TObject);
var p: tpoint;
    ed:tedit;
    val: double;
begin
  case TSpeedButton(sender).tag of
    1: begin ed:=ObsStart; val:=Fobsdatestart;end;
    2: begin ed:=ObsEnd; val:=Fobsdateend;end;
  end;
  p.x:=ed.Left;
  p.y:=ed.Top+ed.Height;
  p:=ed.Parent.ClientToScreen(p);
  f_date.Left:=p.x;
  f_date.Top:=p.y;
  if val=0 then
    f_date.Date:=now
  else
    f_date.Date:=val;
  if f_date.ShowModal=mrOK then begin
    case TSpeedButton(sender).tag of
      1: Fobsdatestart:=f_date.Date;
      2: Fobsdateend:=f_date.Date;
    end;
    ed.Text:=FormatDateTime(datetimedisplay,f_date.Date);
    if TSpeedButton(sender).tag=1 then ObsDate.Text:=FormatDateTime(datedisplay,Fobsdatestart);
    ModifiedObservation:=true;
  end;
end;

procedure Tf_notelun.ComputePower;
var fi,fe,b,fve,power: double;
begin
  fi:=StrToFloatDef(dbnotes.QueryOne('select focal from instrument where id='+inttostr(GetObsBoxIndex(ObsInstrument))),0);
  fe:=StrToFloatDef(dbnotes.QueryOne('select focal from eyepiece where id='+inttostr(GetObsBoxIndex(ObsEyepiece))),0);
  fve:=StrToFloatDef(dbnotes.QueryOne('select field from eyepiece where id='+inttostr(GetObsBoxIndex(ObsEyepiece))),0);
  b:=StrToFloatDef(dbnotes.QueryOne('select power from barlow where id='+inttostr(GetObsBoxIndex(ObsBarlow))),1);
  if (fi>0)and(fe>0) then begin
    power:=b*fi/fe;
    ObsPower.Caption:='Power: '+FormatFloat(f0,power)+'x';
    if fve>0 then ObsPower.Caption:=ObsPower.Caption+' , FOV: '+FormatFloat(f1,60*fve/power)+'''';
  end
  else
    ObsPower.Caption:='';
  ObsPowerRO.Caption:=ObsPower.Caption;
end;

procedure Tf_notelun.ComputeCamera;
var fi,b,cx,cy,cpx,fx,fy: double;
begin
  fi:=StrToFloatDef(dbnotes.QueryOne('select focal from instrument where id='+inttostr(GetObsBoxIndex(ObsInstrument))),0);
  b:=StrToFloatDef(dbnotes.QueryOne('select power from barlow where id='+inttostr(GetObsBoxIndex(ObsBarlow))),1);
  cx:=StrToFloatDef(dbnotes.QueryOne('select pixelx from camera where id='+inttostr(GetObsBoxIndex(ObsCamera))),0);
  cy:=StrToFloatDef(dbnotes.QueryOne('select pixely from camera where id='+inttostr(GetObsBoxIndex(ObsCamera))),0);
  cpx:=StrToFloatDef(dbnotes.QueryOne('select pixelsize from camera where id='+inttostr(GetObsBoxIndex(ObsCamera))),0);
  if (fi>0)and(cx>0)and(cy>0)and(cpx>0) then begin
    cx:=cx*cpx/1000;
    cy:=cy*cpx/1000;
    fx:=60*rad2deg*ArcTan(cx/fi/b);
    fy:=60*rad2deg*ArcTan(cy/fi/b);
    ObsCameraFov.Caption:='FOV : '+FormatFloat(f1,fx)+'''x'+FormatFloat(f1,fy)+'''';
  end
  else
    ObsCameraFov.Caption:='';
  ObsCameraFovRO.Caption:=ObsCameraFov.Caption;
end;

procedure Tf_notelun.ComputeEphemeris(obs:integer;dt:double);
var cmd, tzname,ew: string;
  u, p: double;
  h: double;
  year,month,day: Word;
  jd0, st0, ecl, q, colong, az, ah, ra,de, ra2, de2,rad,ded,pa: double;
  v1, v2, dist, dkm,phase, illum: double;
  gpa, glibrb, glibrl, nutl,nuto,sunl,sunb,abe,abp,sunlat, sunlong: double;
  librb, librl,lunaison,nmjd, fqjd, fmjd, lqjd: double;
const
  ratio = 0.99664719;
  H0    = 6378140.0;
begin
   cmd:='select longitude,latitude,elevation,timezone from location where id='+IntToStr(obs);
   dbnotes.Query(cmd);
   if dbnotes.RowCount>0 then begin
     ObsLongitude:=dbnotes.Results[0].Format[0].AsFloat;
     ObsLatitude:=dbnotes.Results[0].Format[1].AsFloat;
     ObsAltitude:=dbnotes.Results[0].Format[2].AsFloat;
     tzname:=dbnotes.Results[0][3];
     if ObsLongitude<0 then ew:='W' else ew:='E';
     ObsLocationDetailRO.Caption:='Longitude: '+ew+' '+FormatFloat(f4,abs(ObsLongitude))+', Latitude: '+FormatFloat(f4,ObsLatitude)+', Elevation: '+FormatFloat(f0,ObsAltitude)+', Time zone: '+tzname;
     ObsLongitude:=-ObsLongitude; // Meeus convention
     p := degtorad(ObsLatitude);
     u := arctan(ratio * tan(p));
     ObsRoSinPhi := ratio * sin(u) + (ObsAltitude / H0) * sin(p);
     ObsRoCosPhi := cos(u) + (ObsAltitude / H0) * cos(p);
     ObsRefractionCor := 1;
     tz.TimeZoneFile:=ZoneDir + StringReplace(tzname, '/', PathDelim, [rfReplaceAll]);
     tz.Date:=dt;
     timezone:=tz.SecondsOffset/3600;
     DecodeDate(dt,year,month,day);
     h:=frac(dt)*24;
     dt_ut:=dtminusut(year);
     CurrentJD:=jd(year,month,day,h-timezone+dt_ut);
     ecl:=ecliptic(CurrentJD);
     Fplanet.SetDE(jpldir);
     Fplanet.nutation(CurrentJD,nutl,nuto);
     Fplanet.sunecl(CurrentJD,sunl,sunb);
     PrecessionEcl(jd2000,CurrentJD,sunl,sunb);
     aberration(CurrentJD,abe,abp);
     // compute j2000 geocentric
     Fplanet.Moon(CurrentJD, ra, de, dist, dkm, diam, phase, illum);
     ra2 := ra;   de2 := de;
     // orientation need jdnow
     precession(jd2000, CurrentJD, ra2, de2);
     // geocentric orientation, return valid sub-solar position
     Fplanet.MoonOrientation(CurrentJD, ra2, de2, dist, gpa, glibrb, glibrl, sunlat, sunlong);
     // parallax
     jd0 := jd(year, month, day, 0.0);
     st0 := SidTim(jd0, h - Timezone, ObsLongitude);
     Paralaxe(st0, dist, ra, de, ra, de, q, jd2000, CurrentJD);
     diam := diam / q;
     dkm  := dkm * q;
     dist := dist * q;
     // apparent coordinates
     apparent_equatorial(ra,de,ecl,sunl,abp,abe,nutl,nuto,false);
     rad := ra;
     ded := de;
     mean_equatorial(ra,de,ecl,sunl,abp,abe,nutl,nuto);
     precession(jd2000, CurrentJD, rad, ded);
      // topocentric libration, ignore invalid sub-solar position
      Fplanet.MoonOrientation(CurrentJD, rad, ded, dist, pa, librb, librl, v1, v2);
      colong := rmod(90 - sunlong + 360, 360);
      jd0    := jd(year, 1, 1, 0.0);
      Fplanet.MoonPhases(year + (CurrentJD - jd0) / 365.25, nmjd, fqjd, fmjd, lqjd);
      lunaison := CurrentJD - nmjd;
      if lunaison < 0 then
      begin
        lunaison := CurrentJD - Fplanet.MoonPhase(floor(12.3685 *
          (year - 2000 - 0.04 + (CurrentJD - jd0) / 365.25)));
      end;
      eq2hz(st0 - rad, ded, az, ah);
      az := rmod(rad2deg * az + 180, 360);
      ah := rad2deg * ah;

      ObsLunation.Caption:=formatfloat(f2, lunaison)+' days';
      ObsColongitude.Caption:=formatfloat(f2, colong)+ldeg;
      ObsSubsolarLat.Caption:=formatfloat(f2, sunlat)+ldeg;
      ObsLibrLon.Caption:=demtostr(librl);
      ObsLibrLat.Caption:=demtostr(librb);
      ObsIllum.Caption:=formatfloat(f1, illum * 100) + '%';
      ObsDiam.Caption:=formatfloat(f2, diam / 60) + lmin;
      ObsRA.Caption:=arptostr(rad2deg * rad / 15,1);
      ObsDec.Caption:=deptostr(rad2deg * ded,1);
      ObsPa.Caption:=formatfloat(f1, pa) + ldeg;;
      ObsAzimut.Caption:=demtostr(az);
      ObsAlt.Caption:=demtostr(ah);
   end
   else
      ClearEphemeris;
end;

procedure Tf_notelun.ObsBarlowChange(Sender: TObject);
begin
  LastBarlow:=GetObsBoxIndex(ObsBarlow);
  ModifiedObservation:=true;
  ComputePower;
  ComputeCamera;
end;

procedure Tf_notelun.ObsCameraChange(Sender: TObject);
begin
  LastCamera:=GetObsBoxIndex(ObsCamera);
  ModifiedObservation:=true;
  ComputeCamera;
end;

procedure Tf_notelun.ObsEyepieceChange(Sender: TObject);
begin
  LastEyepiece:=GetObsBoxIndex(ObsEyepiece);
  ModifiedObservation:=true;
  ComputePower;
end;

procedure Tf_notelun.ObsInstrumentChange(Sender: TObject);
begin
  LastInstrument:=GetObsBoxIndex(ObsInstrument);
  ModifiedObservation:=true;
  ComputePower;
  ComputeCamera;
end;

procedure Tf_notelun.ObsLocationChange(Sender: TObject);
begin
  LastLocation:=GetObsBoxIndex(ObsLocation);
  ModifiedObservation:=true;
end;

procedure Tf_notelun.ObsObserverChange(Sender: TObject);
begin
  LastObserver:=GetObsBoxIndex(ObsObserver);
  ModifiedObservation:=true;
end;

procedure Tf_notelun.ObsNoteChange(Sender: TObject);
begin
  ModifiedObservation:=true;
end;

procedure Tf_notelun.ObsFilesValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
begin
  if OldValue<>NewValue then ObsNoteChange(sender);
end;

procedure Tf_notelun.BtnChangeInfoDateClick(Sender: TObject);
var p: tpoint;
begin
  p.x:=InfoDate.Left;
  p.y:=InfoDate.Top+InfoDate.Height;
  p:=InfoDate.Parent.ClientToScreen(p);
  if Finfodate=0 then
    CalendarDialog1.Date:=now
  else
    CalendarDialog1.Date:=Finfodate;
  CalendarDialog1.Left:=p.x;
  CalendarDialog1.Top:=p.y;
  if CalendarDialog1.Execute then begin
    Finfodate:=trunc(CalendarDialog1.Date);
    InfoDate.Text:=FormatDateTime(datedisplay,Finfodate);
  end;
end;

function Tf_notelun.FormatDate(val:string):string;
var dt: double;
begin
  dt:=StrToFloatDef(val,0);
  if dt=0 then
    result:=''
  else
    result:=FormatDateTime(datedisplay,dt);
end;

procedure Tf_notelun.SetInfoDate(val:string);
begin
  Finfodate:=StrToFloatDef(val,0);
  if Finfodate=0 then
    InfoDate.Text:=''
  else
    InfoDate.Text:=FormatDateTime(datedisplay,Finfodate);
end;

procedure Tf_notelun.SetObsDate(val1,val2:string);
begin
  Fobsdatestart:=StrToFloatDef(val1,0);
  Fobsdateend:=StrToFloatDef(val2,0);
  if Fobsdatestart=0 then begin
    ObsDate.Text:='';
    ObsStart.Text:='';
    ObsStartRO.Caption:='';
  end
  else begin
    ObsDate.Text:=FormatDateTime(datedisplay,Fobsdatestart);
    ObsStart.Text:=FormatDateTime(datetimedisplay,Fobsdatestart);
    ObsStartRO.Caption:=ObsStart.Text;
  end;
  if Fobsdateend=0 then begin
    ObsEnd.Text:='';
    ObsEndRO.Caption:='';
  end
  else begin
    ObsEnd.Text:=FormatDateTime(datetimedisplay,Fobsdateend);
    ObsEndRO.Caption:=ObsEnd.Text;
  end;
end;

procedure Tf_notelun.NotesList(formation:string='';prefix:char=' ';fid:int64=0);
var cmd: string;
    i,n,k: integer;
    id:TNoteID;
    ok: boolean;
begin
  locklist:=true;
  try
  if formation='' then
    formation:=CurrentFormation
  else
    CurrentFormation:=formation;
  ClearList;
  k:=1;
  n:=1;
  cmd:='select id,formation,date,"I" from infonotes';
  if formation<>'' then begin
    if pos('%',formation)=0 then
      cmd:=cmd+' where FORMATION="'+formation+'"'
    else
      cmd:=cmd+' where FORMATION like "'+formation+'"';
  end;
  cmd:=cmd+' union select id,formation,datestart,"O" from obsnotes';
  if formation<>'' then begin
    if pos('%',formation)=0 then
      cmd:=cmd+' where FORMATION="'+formation+'"'
    else
      cmd:=cmd+' where FORMATION like "'+formation+'"';
  end;
  cmd:=cmd+' order by '+IntToStr(SortCol+2);
  if SortAsc then
    cmd:=cmd+' ASC'
  else
    cmd:=cmd+' DESC';
  dbnotes.Query(cmd);
  ListNotes.RowCount:=dbnotes.RowCount+1;
  for i:=0 to dbnotes.RowCount-1 do begin
    id:=TNoteID.Create;
    id.prefix:=dbnotes.Results[i][3];
    id.id:=dbnotes.Results[i].Format[0].AsInteger;
    if (fid<>0)and(prefix=id.prefix)and(id.id=fid) then k:=n;
    ListNotes.Objects[0,n]:=id;
    ListNotes.Cells[0,n]:=dbnotes.Results[i][1];
    ListNotes.Cells[1,n]:=FormatDate(dbnotes.Results[i][2]);
    ListNotes.Cells[2,n]:=id.prefix;
    inc(n);
  end;
  finally
   locklist:=false;
   ListNotesSelectCell(ListNotes,0,k,ok);
  end;
end;

procedure Tf_notelun.ListNotesHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
begin
 if IsColumn then begin
   if SortCol=Index then
     SortAsc:=not SortAsc
   else
     SortCol:=index;
   NotesList;
 end;
end;

procedure Tf_notelun.ClearList;
var i: integer;
begin
  for i:=1 to ListNotes.RowCount-1 do begin
    if ListNotes.Objects[0,i]<>nil then begin
      ListNotes.Objects[0,i].Free;
      ListNotes.Objects[0,i]:=nil;
    end;
  end;
  ListNotes.RowCount:=1;
end;

procedure Tf_notelun.ClearInfoNote;
begin
  Finfodate:=0;
  InfoFormation.Text:='';
  InfoDate.Text:='';
  InfoAuthor.Text:='';
  InfoText.Text:='';
  InfoFiles.Clear;
  ModifiedInformation:=false;
end;

procedure Tf_notelun.ClearObsNote;
begin
  Fobsdatestart:=0;
  Fobsdateend:=0;
  ObsFormation.Text:='';
  ObsDate.Text:='';
  ObsLocation.Text:='';
  ObsObserver.Text:='';
  ObsStart.Text:='';
  ObsEnd.Text:='';
  ObsMeteo.Text:='';
  ObsSeeing.Text:='';
  ObsInstrument.Text:='';
  ObsBarlow.Text:='';
  ObsPower.Caption:='';
  ObsCamera.Text:='';
  ObsText.Text:='';
  ObsFiles.Clear;
  ClearEphemeris;
  ModifiedObservation:=false;
end;

procedure Tf_notelun.ClearEphemeris;
begin
  ObsRA.Caption:=' ';
  ObsDec.Caption:=' ';
  ObsDiam.Caption:=' ';
  ObsLunation.Caption:=' ';
  ObsColongitude.Caption:=' ';
  ObsSubsolarLat.Caption:=' ';
  ObsIllum.Caption:=' ';
  ObsPa.Caption:=' ';
  ObsLibrLon.Caption:=' ';
  ObsLibrLat.Caption:=' ';
  ObsAzimut.Caption:=' ';
  ObsAlt.Caption:=' ';
end;

procedure Tf_notelun.ListNotesSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
var i: int64;
    id: TNoteID;
begin
  if locklist then exit;
  id:=TNoteID(ListNotes.Objects[0,aRow]);
  if id<>nil then begin
    i:=id.id;
    if id.prefix='O' then begin
      ShowObsNote(i);
    end
    else if id.prefix='I' then begin
      ShowInfoNote(i);
    end;
    CanSelect:=true;
  end
  else begin
    CanSelect:=false;
  end;
end;

procedure Tf_notelun.MenuSetupObservation(Sender: TObject);
begin
  FSetup.PageControl1.ActivePageIndex:=TMenuItem(Sender).tag;
  FSetup.ShowModal;
  if FSetup.ModalResult=mrOK then
    LoadObsBoxes;
end;

procedure Tf_notelun.ClearObsBox(box:TComboBox);
var i: integer;
begin
  for i:=0 to box.items.Count-1 do begin
    if box.items.Objects[i]<>nil then begin
      box.items.Objects[i].Free;
      box.items.Objects[i]:=nil;
    end;
  end;
  box.clear;
end;

procedure Tf_notelun.LoadObsBox(table: string;box:TComboBox; name2:string='');
var cmd: string;
    i,n: integer;
    id: TNoteID;
begin
  ClearObsBox(box);
  box.Items.Add('N/A');
  n:=0;
  if name2='' then
    cmd:='select id,name from '+table+' order by name'
  else
    cmd:='select id,name,'+name2+' from '+table+' order by name';
  dbnotes.Query(cmd);
  for i:=0 to dbnotes.RowCount-1 do begin
    id:=TNoteID.Create;
    id.id:=dbnotes.Results[i].Format[0].AsInteger;
    if name2='' then
      box.Items.AddObject(dbnotes.Results[i][1],id)
    else
      box.Items.AddObject(dbnotes.Results[i][1]+' '+dbnotes.Results[i][2],id);
  end;
end;

procedure Tf_notelun.LoadObsBoxes;
begin
  LoadObsBox('location',ObsLocation);
  LoadObsBox('observer',ObsObserver,'firstname');
  LoadObsBox('instrument',ObsInstrument);
  LoadObsBox('barlow',ObsBarlow);
  LoadObsBox('eyepiece',ObsEyepiece);
  LoadObsBox('camera',ObsCamera);
end;

procedure Tf_notelun.ShowInfoNote(id: int64);
var cmd: string;
begin
  PageControl1.ActivePageIndex:=1;
  SetEditInformation(false);
  ClearInfoNote;
  cmd:='select ID,FORMATION,DATE,AUTHOR,NOTE,FILES from infonotes where ID='+inttostr(id);
  dbnotes.Query(cmd);
  if dbnotes.RowCount>0 then begin
    CurrentInfoId:=dbnotes.Results[0].Format[0].AsInteger;
    InfoFormation.Text:=dbnotes.Results[0][1];
    SetInfoDate(dbnotes.Results[0][2]);
    InfoAuthor.Text:=dbnotes.Results[0][3];
    InfoText.Text:=dbnotes.Results[0][4];
    SetInfoFiles(dbnotes.Results[0][5]);
  end
  else
    CurrentInfoId:=-1;
  ModifiedInformation:=false;
end;

procedure Tf_notelun.BtnEditClick(Sender: TObject);
begin
 if EditingObservation or EditingInformation then begin
   case PageControl1.ActivePageIndex of
     0 : CancelEditObservation;
     1 : CancelEditInformation;
   end;
 end
 else begin
   case PageControl1.ActivePageIndex of
     0 : SetEditObservation(true);
     1 : SetEditInformation(true);
   end;
 end;
end;

procedure Tf_notelun.BtnListAllClick(Sender: TObject);
begin
  CurrentFormation:='';
  NotesList;
end;

procedure Tf_notelun.BtnListNewClick(Sender: TObject);
var r: TRadioGroup;
    f: Tform;
    p: tpoint;
begin
  r:=TRadioGroup.Create(self);
  r.Caption:='Type of note';
  r.Items.Add('Observation');
  r.Items.Add('Information');
  r.ItemIndex:=-1;
  r.Align:=alClient;
  r.OnClick:=@BtnListNewSelection;
  f:=Tform.Create(self);
  f.Width:=300;
  f.Height:=150;
  r.Parent:=f;
  p.x:=BtnListNew.Left;
  p.y:=BtnListNew.Top+BtnListNew.Height;
  p:=BtnListNew.Parent.ClientToScreen(p);
  f.Left:=p.x;
  f.Top:=p.y;
  f.ShowModal;
  if f.ModalResult=mrOK then begin
    case r.ItemIndex of
      0: NewObservationNote;
      1: NewInformationNote;
    end;
  end;
  r.free;
  f.free;
end;

procedure Tf_notelun.BtnListNewSelection(sender: TObject);
begin
  Tform(TRadioGroup(sender).Parent).ModalResult:=mrOK;
end;

procedure Tf_notelun.BtnListSelectionClick(Sender: TObject);
var txt:string;
begin
  txt:=CurrentFormation;
  if InputQuery('Selection','Formation',txt) then begin
    CurrentFormation:=StringReplace(txt,'*','%',[rfReplaceAll]);
    NotesList;
  end;
end;

procedure Tf_notelun.BtnSaveClick(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
    0 : SaveObservationNote;
    1 : SaveInformationNote;
  end;
end;

procedure Tf_notelun.BtnDeleteClick(Sender: TObject);
begin
  if MessageDlg('Delete note?',mtConfirmation,mbYesNo,0)=mrYes then begin
    case PageControl1.ActivePageIndex of
      0 : DeleteObservation(CurrentInfoId);
      1 : DeleteInformation(CurrentInfoId);
    end;
    NotesList;
  end;
end;

procedure Tf_notelun.SetEditInformation(onoff: boolean);
var b:TBorderStyle;
begin
  if (not onoff) and ModifiedInformation then begin
    if MessageDlg('Note is modified do you want to save the modification?',mtConfirmation,mbYesNo,0)=mrYes then
      SaveInformationNote
    else begin
      if NewInformation then DeleteInformation(CurrentInfoId);
    end;
  end;
  EditingInformation:=onoff;
  BtnEdit.Down:=EditingInformation;
  if EditingInformation then
    b:=bsSingle
  else
    b:=bsNone;
  InfoFormation.BorderStyle:=b;
  BtnSearchFormation1.Visible:=EditingInformation;
  InfoDate.BorderStyle:=b;
  BtnChangeInfoDate.Visible:=EditingInformation;
  InfoAuthor.BorderStyle:=b;
  InfoAuthor.ReadOnly:=not EditingInformation;
  InfoText.BorderStyle:=b;
  InfoText.ReadOnly:=not EditingInformation;
  InfoFiles.BorderStyle:=b;
  PanelInfofileEdit.Visible:=EditingInformation;
  if (EditingInformation)and(not (goEditing in InfoFiles.Options)) then
    InfoFiles.Options:=InfoFiles.Options+[goEditing];
  if (not EditingInformation)and(goEditing in InfoFiles.Options) then
     InfoFiles.Options:=InfoFiles.Options-[goEditing];
end;

procedure Tf_notelun.CancelEditInformation;
begin
  if ModifiedInformation then begin
    if MessageDlg('Note is modified do you want to save the modification?',mtConfirmation,mbYesNo,0)=mrYes then
      SaveInformationNote
    else begin
      if NewInformation then DeleteInformation(CurrentInfoId);
    end;
  end;
  ModifiedInformation:=false;
  if NewInformation then
    NotesList(InfoFormation.Text,'I')
  else
    NotesList;
  ShowInfoNote(CurrentInfoId);
  NewInformation:=false;
end;

procedure Tf_notelun.InfoNoteChange(Sender: TObject);
begin
 ModifiedInformation:=true;
end;

procedure Tf_notelun.ListNotesDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  if (ARow=0)and(aCol=SortCol) then begin
    if SortAsc then
      Imagelist1.Draw(ListNotes.Canvas,aRect.Right-Imagelist1.width,aRect.Top,1,true)
    else
      Imagelist1.Draw(ListNotes.Canvas,aRect.Right-Imagelist1.width,aRect.Top,0,true);
  end;

end;

procedure Tf_notelun.InfoFilesValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
begin
 if OldValue<>NewValue then InfoNoteChange(sender);
end;

procedure Tf_notelun.SaveInformationNote;
var cmd: string;
    id: int64;
begin
 if CurrentInfoId>0 then begin
   id:=CurrentInfoId;
   cmd:='replace into infonotes (ID,FORMATION,DATE,AUTHOR,NOTE,FILES) values ('+
        IntToStr(id)+',"'+
        SafeSqlText(InfoFormation.Text)+'",'+
        FormatFloat(f5,Finfodate)+',"'+
        SafeSqlText(InfoAuthor.Text)+'","'+
        SafeSqlText(InfoText.Text)+'","'+
        SafeSqlText(GetInfoFiles)+'")';
   dbnotes.Query(cmd);
   if dbnotes.LastError=0 then begin
     ModifiedInformation:=false;
     if NewInformation then
       NotesList(InfoFormation.Text,'I')
     else
       NotesList;
     ShowInfoNote(id);
     NewInformation:=false;
   end
   else
     ShowMessage('Error: '+dbnotes.ErrorMessage)
 end;
end;

procedure Tf_notelun.MenuItemNewInfoClick(Sender: TObject);
begin
  NewInformationNote;
end;

procedure Tf_notelun.MenuItemNewObsClick(Sender: TObject);
begin
  NewObservationNote;
end;

procedure Tf_notelun.MenuItemSortDateClick(Sender: TObject);
begin
  SortCol:=1;
  NotesList;
end;

procedure Tf_notelun.MenuItemSortFormationClick(Sender: TObject);
begin
  SortCol:=0;
  NotesList;
end;

procedure Tf_notelun.MenuItemSortTypeClick(Sender: TObject);
begin
  SortCol:=2;
  NotesList;
end;

procedure Tf_notelun.NewInformationNote(formation:string='');
var cmd: string;
begin
  SetEditInformation(false);
  cmd:='insert into infonotes (ID,FORMATION,DATE,AUTHOR,NOTE,FILES) values ('+'null,"'+
       SafeSqlText(formation)+'",'+FormatFloat(f5,trunc(now))+',"","","")';
  dbnotes.Query(cmd);
 if dbnotes.LastError=0 then begin
   NewInformation:=true;
   CurrentInfoId:=dbnotes.LastInsertID;
   ModifiedInformation:=false;
   ShowInfoNote(CurrentInfoId);
   SetEditInformation(true);
   ModifiedInformation:=true;
 end
 else
   ShowMessage('Error: '+dbnotes.ErrorMessage)
end;

procedure Tf_notelun.DeleteInformation(id: int64);
var cmd: string;
begin
  NewInformation:=false;
  if id>0 then begin
    cmd:='delete from infonotes where ID='+inttostr(id);
    dbnotes.Query(cmd);
  end;
end;

procedure Tf_notelun.SetInfoFiles(txt: string);
var lst: TStringList;
    i: integer;
begin
  CurrentInfoFile:=-1;
  if txt='' then begin
    InfoFiles.RowCount:=0;
  end
  else begin
    lst:=TStringList.Create;
    SplitRec2(txt,#10,lst);
    InfoFiles.RowCount:=lst.Count;
    for i:=0 to lst.Count-1 do begin
      InfoFiles.Cells[0,i]:=lst[i];
    end;
    lst.free;
  end;
end;

function  Tf_notelun.GetInfoFiles: string;
var i: integer;
begin
  result:='';
  for i:=0 to InfoFiles.RowCount-1 do begin
    result:=Result+InfoFiles.Cells[0,i]+#10;
  end;
  if Length(result)>1 then Delete(result,Length(result),1);
end;

procedure Tf_notelun.InfoFilesSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  CurrentInfoFile:=aRow;
  CanSelect:=true;
end;

procedure Tf_notelun.InfoFilesDblClick(Sender: TObject);
var fn: string;
begin
  if CurrentInfoFile<0 then exit;
  fn:=InfoFiles.Cells[0,CurrentInfoFile];
  if fn<>'' then
    ExecuteFile(fn);
end;

procedure Tf_notelun.BtnAddInfoFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    InfoFiles.RowCount:=InfoFiles.RowCount+1;
    InfoFiles.Cells[0,InfoFiles.RowCount-1]:=OpenDialog1.FileName;
    ModifiedInformation:=true;
  end;
end;

procedure Tf_notelun.BtnPastInfoFileClick(Sender: TObject);
var url:string;
begin
  url:=Clipboard.AsText;
  if url<>'' then begin
    InfoFiles.RowCount:=InfoFiles.RowCount+1;
    InfoFiles.Cells[0,InfoFiles.RowCount-1]:=url;
    ModifiedInformation:=true;
  end;
end;

procedure Tf_notelun.BtnDelInfoFileClick(Sender: TObject);
begin
  if CurrentInfoFile<0 then exit;
  if MessageDlg('Remove file '+crlf+InfoFiles.Cells[0,CurrentInfoFile],mtConfirmation,mbYesNo,0)=mrYes then begin
    InfoFiles.DeleteRow(CurrentInfoFile);
    ModifiedInformation:=true;
  end;
end;

function Tf_notelun.GetObsBoxIndex(box:TComboBox):int64;
var i: integer;
begin
  i:=box.ItemIndex;
  if box.items.Objects[i]=nil then
    result:=0
  else
    result:=TNoteID(box.items.Objects[i]).id;
end;

procedure Tf_notelun.SetObsBoxIndex(box:TComboBox; lbl:TLabel; id: int64);
var i,n: integer;
    rid: int64;
begin
  n:=0;
  for i:=0 to box.Items.Count-1 do begin
     if box.items.Objects[i]=nil then
       rid:=0
     else
       rid:=TNoteID(box.items.Objects[i]).id;
     if rid=id then begin
       n:=i;
       if lbl<>nil then lbl.Caption:=box.Items[i];
       break;
     end;
  end;
  box.ItemIndex:=n;
end;

procedure Tf_notelun.ShowObsNote(id: int64);
var cmd: string;
begin
  PageControl1.ActivePageIndex:=0;
  SetEditObservation(false);
  ClearObsNote;
  cmd:='select ID,FORMATION,DATESTART,DATEEND,LOCATION,OBSERVER,METEO,SEEING,INSTRUMENT,BARLOW,EYEPIECE,CAMERA,NOTE,FILES from obsnotes where ID='+inttostr(id);
  dbnotes.Query(cmd);
  if dbnotes.RowCount>0 then begin
    CurrentObsId:=dbnotes.Results[0].Format[0].AsInteger;
    ObsFormation.Text:=dbnotes.Results[0][1];
    SetObsDate(dbnotes.Results[0][2],dbnotes.Results[0][3]);
    SetObsBoxIndex(ObsLocation,ObsLocationRO,dbnotes.Results[0].Format[4].AsInteger);
    SetObsBoxIndex(ObsObserver,ObsObserverRO,dbnotes.Results[0].Format[5].AsInteger);
    ObsMeteo.Text:=dbnotes.Results[0][6];
    ObsMeteoRO.Caption:=ObsMeteo.Text;
    ObsSeeing.Text:=dbnotes.Results[0][7];
    ObsSeeingRO.Caption:=ObsSeeing.Text;
    SetObsBoxIndex(ObsInstrument,ObsInstrumentRO,dbnotes.Results[0].Format[8].AsInteger);
    SetObsBoxIndex(ObsBarlow,ObsBarlowRO,dbnotes.Results[0].Format[9].AsInteger);
    SetObsBoxIndex(ObsEyepiece,ObsEypieceRO,dbnotes.Results[0].Format[10].AsInteger);
    SetObsBoxIndex(ObsCamera,ObsCameraRO,dbnotes.Results[0].Format[11].AsInteger);
    ObsText.Text:=dbnotes.Results[0][12];
    SetObsFiles(dbnotes.Results[0][13]);
    ComputeEphemeris(dbnotes.Results[0].Format[4].AsInteger,dbnotes.Results[0].Format[2].AsFloat);
    ComputePower;
    ComputeCamera;
  end
  else
    CurrentObsId:=-1;
  ModifiedObservation:=false;
end;

procedure Tf_notelun.SetEditObservation(onoff: boolean);
var b:TBorderStyle;
begin
  if (not onoff) and ModifiedObservation then begin
    if MessageDlg('Note is modified do you want to save the modification?',mtConfirmation,mbYesNo,0)=mrYes then
      SaveObservationNote
    else begin
      if NewObservation then DeleteObservation(CurrentObsId);
    end;
  end;
  EditingObservation:=onoff;
  BtnEdit.Down:=EditingObservation;
  if EditingObservation then begin
    b:=bsSingle;
    PCobs.ActivePageIndex:=0;
    ClearEphemeris;
  end
  else begin
    b:=bsNone;
    PCobs.ActivePageIndex:=1;
  end;
  ObsFormation.BorderStyle:=b;
  BtnSearchFormation.Visible:=EditingObservation;
  ObsDate.BorderStyle:=b;
  ObsText.BorderStyle:=b;
  ObsText.ReadOnly:=not EditingObservation;
  ObsFiles.BorderStyle:=b;
  PanelObsfileEdit.Visible:=EditingObservation;
  if (EditingObservation)and(not (goEditing in ObsFiles.Options)) then
    ObsFiles.Options:=ObsFiles.Options+[goEditing];
  if (not EditingObservation)and(goEditing in ObsFiles.Options) then
     ObsFiles.Options:=ObsFiles.Options-[goEditing];
end;

procedure Tf_notelun.SaveObservationNote;
var cmd: string;
    id: int64;
begin
 if CurrentObsId>0 then begin
   id:=CurrentObsId;
   cmd:='replace into obsnotes (ID,FORMATION,DATESTART,DATEEND,LOCATION,OBSERVER,METEO,SEEING,INSTRUMENT,BARLOW,EYEPIECE,CAMERA,NOTE,FILES) values ('+
        IntToStr(id)+',"'+
        SafeSqlText(ObsFormation.Text)+'",'+
        FormatFloat(f9,Fobsdatestart)+','+
        FormatFloat(f9,Fobsdateend)+','+
        inttostr(GetObsBoxIndex(ObsLocation))+','+
        inttostr(GetObsBoxIndex(ObsObserver))+',"'+
        SafeSqlText(ObsMeteo.Text)+'","'+
        SafeSqlText(ObsSeeing.Text)+'",'+
        inttostr(GetObsBoxIndex(ObsInstrument))+','+
        inttostr(GetObsBoxIndex(ObsBarlow))+','+
        inttostr(GetObsBoxIndex(ObsEyepiece))+','+
        inttostr(GetObsBoxIndex(ObsCamera))+',"'+
        SafeSqlText(ObsText.Text)+'","'+
        SafeSqlText(GetObsFiles)+'")';
   dbnotes.Query(cmd);
   if dbnotes.LastError=0 then begin
     ModifiedObservation:=false;
     if NewObservation then
       NotesList(ObsFormation.Text,'O')
     else
       NotesList;
     ShowObsNote(id);
     NewObservation:=false;
   end
   else
     ShowMessage('Error: '+dbnotes.ErrorMessage)
 end;
end;

procedure Tf_notelun.CancelEditObservation;
begin
  if ModifiedObservation then begin
    if MessageDlg('Note is modified do you want to save the modification?',mtConfirmation,mbYesNo,0)=mrYes then
      SaveObservationNote
    else begin
      if NewObservation then DeleteObservation(CurrentObsId);
    end;
  end;
  ModifiedObservation:=false;
  if NewObservation then
    NotesList(ObsFormation.Text,'O')
  else
    NotesList;
  ShowObsNote(CurrentObsId);
  NewObservation:=false;
end;

procedure Tf_notelun.NewObservationNote(formation:string='');
var cmd: string;
begin
 SetEditObservation(false);
 cmd:='insert into obsnotes (ID,FORMATION,DATESTART,DATEEND,LOCATION,OBSERVER,METEO,SEEING,INSTRUMENT,BARLOW,EYEPIECE,CAMERA,NOTE,FILES) values ('+'null,"'+
       SafeSqlText(formation)+'",'+FormatFloat(f5,trunc(now))+','+FormatFloat(f5,trunc(now))+','+
       IntToStr(LastLocation)+','+IntToStr(LastObserver)+',"","",'+
       IntToStr(LastInstrument)+','+IntToStr(LastBarlow)+','+IntToStr(LastEyepiece)+','+IntToStr(LastCamera)+',"","")';
 dbnotes.Query(cmd);
 if dbnotes.LastError=0 then begin
   NewObservation:=true;
   CurrentObsId:=dbnotes.LastInsertID;
   ModifiedObservation:=false;
   ShowObsNote(CurrentObsId);
   SetEditObservation(true);
   ModifiedObservation:=true;
 end
 else
   ShowMessage('Error: '+dbnotes.ErrorMessage)
 end;

procedure Tf_notelun.DeleteObservation(id: int64);
var cmd: string;
begin
  NewObservation:=false;
  if id>0 then begin
    cmd:='delete from obsnotes where ID='+inttostr(id);
    dbnotes.Query(cmd);
  end;
end;

procedure Tf_notelun.SetObsFiles(txt: string);
var lst: TStringList;
    i: integer;
begin
  CurrentObsFile:=-1;
  if txt='' then begin
    ObsFiles.RowCount:=0;
  end
  else begin
    lst:=TStringList.Create;
    SplitRec2(txt,#10,lst);
    ObsFiles.RowCount:=lst.Count;
    for i:=0 to lst.Count-1 do begin
      ObsFiles.Cells[0,i]:=lst[i];
    end;
    lst.free;
  end;
end;

function  Tf_notelun.GetObsFiles: string;
var i: integer;
begin
  result:='';
  for i:=0 to ObsFiles.RowCount-1 do begin
    result:=Result+ObsFiles.Cells[0,i]+#10;
  end;
  if Length(result)>1 then Delete(result,Length(result),1);
end;

procedure Tf_notelun.ObsFilesSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  CurrentObsFile:=aRow;
  CanSelect:=true;
end;

procedure Tf_notelun.ObsFilesDblClick(Sender: TObject);
var fn: string;
begin
  if CurrentObsFile<0 then exit;
  fn:=ObsFiles.Cells[0,CurrentObsFile];
  if fn<>'' then
    ExecuteFile(fn);
end;

procedure Tf_notelun.BtnAddObsFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    ObsFiles.RowCount:=ObsFiles.RowCount+1;
    ObsFiles.Cells[0,ObsFiles.RowCount-1]:=OpenDialog1.FileName;
    ModifiedObservation:=true;
  end;
end;

procedure Tf_notelun.BtnPastObsFileClick(Sender: TObject);
var url:string;
begin
  url:=Clipboard.AsText;
  if url<>'' then begin
    ObsFiles.RowCount:=ObsFiles.RowCount+1;
    ObsFiles.Cells[0,ObsFiles.RowCount-1]:=url;
    ModifiedObservation:=true;
  end;
end;

procedure Tf_notelun.BtnDelObsFileClick(Sender: TObject);
begin
  if CurrentObsFile<0 then exit;
  if MessageDlg('Remove file '+crlf+ObsFiles.Cells[0,CurrentObsFile],mtConfirmation,mbYesNo,0)=mrYes then begin
    ObsFiles.DeleteRow(CurrentObsFile);
    ModifiedObservation:=true;
  end;
end;

end.

