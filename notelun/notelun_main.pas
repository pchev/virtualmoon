unit notelun_main;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef mswindows}
    Windows, ShlObj,
  {$endif}
  dbutil, u_constant, u_util, libsql, cu_tz, passql, passqlite,
  LCLVersion, IniFiles, u_translation, pu_search, pu_date, LazUTF8,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, Grids, ComCtrls, StdCtrls, Buttons, EditBtn;

type

  TNoteID = class(TObject)
    id: integer;
  end;

  { Tf_notelun }

  Tf_notelun = class(TForm)
    BtnChangeInfoDate: TSpeedButton;
    BtnEdit: TSpeedButton;
    BtnDelete: TSpeedButton;
    BtnSearchFormation1: TSpeedButton;
    ObsInstrument: TComboBox;
    ObsOptic: TComboBox;
    ObsCamera: TComboBox;
    ObsMeteo: TEdit;
    ObsSeeing: TEdit;
    ObsPower: TEdit;
    ObsEnd: TEdit;
    ObsStart: TEdit;
    Label20: TLabel;
    MenuItemHelp: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemSetupPlace: TMenuItem;
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
    Quit: TMenuItem;
    ObsAltitude: TLabel;
    ObsAzimut: TLabel;
    ObsLibrLat: TLabel;
    ObsLibrLon: TLabel;
    ObsColongitude: TLabel;
    ObsLunation: TLabel;
    ObsDiam: TLabel;
    ObsDec: TLabel;
    ObsRA: TLabel;
    ObsName: TComboBox;
    ObsPlace: TComboBox;
    ObsFiles: TListBox;
    ObsFilesBox: TGroupBox;
    Label10: TLabel;
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
    InfoFiles: TListBox;
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
    BtnChangeObsDate: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Splitter1: TSplitter;
    TabSheetObservation: TTabSheet;
    TabSheetInformation: TTabSheet;
    procedure BtnChangeInfoDateClick(Sender: TObject);
    procedure BtnChangeObsDateClick(Sender: TObject);
    procedure BtnSearchFormationClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListNotesSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);

  private
    tz: TCdCTimeZone;
    Finfodate,Fobsdate: double;
    procedure SetLang;
    procedure GetAppDir;
    function  FormatDate(val:string):string;
    procedure SetInfoDate(val:string);
    procedure ClearList;
    procedure ClearInfoNote;
    procedure ClearObsNote;
    procedure NotesList;
    procedure ShowInfoNote(id: integer);
    procedure ShowObsNote(id: integer);
  public

  end;

var
  f_notelun: Tf_notelun;

implementation

{$R *.lfm}

{ Tf_notelun }


procedure Tf_notelun.FormCreate(Sender: TObject);
var inifile:Tmeminifile;
begin
  DefaultFormatSettings.DateSeparator:='/';
  DefaultFormatSettings.TimeSeparator:=':';
  DefaultFormatSettings.DecimalSeparator:='.';
  compile_time := {$I %DATE%}+' '+{$I %TIME%};
  compile_version := 'Lazarus '+lcl_version+' Free Pascal '+{$I %FPCVERSION%}+' '+{$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%};

  GetAppDir;
  inifile := Tmeminifile.Create(ConfigFile);
  language:= inifile.ReadString('default', 'lang_po_file', '');
  inifile.Free;
  language:=u_translation.translate(language,'en');
  uplanguage:=UpperCase(language);
  SetLang;
  tz:=TCdCTimeZone.Create;
  tz.LoadZoneTab(ZoneDir+'zone.tab');
  dbm:=TLiteDB.Create(self);
  dbnotes:=TLiteDB.Create(self);
end;

procedure Tf_notelun.FormDestroy(Sender: TObject);
begin
  ClearList;
  dbm.free;
  tz.Free;
  DatabaseList.free;
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
  NotesList;
end;

procedure Tf_notelun.SetLang;
begin

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

procedure Tf_notelun.BtnChangeObsDateClick(Sender: TObject);
var p: tpoint;
begin
  p.x:=ObsDate.Left;
  p.y:=ObsDate.Top+ObsDate.Height;
  p:=ObsDate.Parent.ClientToScreen(p);
  f_date.Left:=p.x;
  f_date.Top:=p.y;
  if Fobsdate=0 then
    f_date.Date:=now
  else
    f_date.Date:=Fobsdate;
  if f_date.ShowModal=mrOK then begin
    Fobsdate:=f_date.Date;
    ObsDate.Text:=FormatDateTime(datetimedisplay,Fobsdate);
  end;
end;

procedure Tf_notelun.BtnChangeInfoDateClick(Sender: TObject);
var p: tpoint;
begin
  p.x:=InfoDate.Left;
  p.y:=InfoDate.Top+InfoDate.Height;
  p:=InfoDate.Parent.ClientToScreen(p);
  f_date.Left:=p.x;
  f_date.Top:=p.y;
  if Finfodate=0 then
    f_date.Date:=now
  else
    f_date.Date:=Finfodate;
  if f_date.ShowModal=mrOK then begin
    Finfodate:=f_date.Date;
    InfoDate.Text:=FormatDateTime(datetimedisplay,Finfodate);
  end;
end;

function Tf_notelun.FormatDate(val:string):string;
var dt: double;
begin
  dt:=StrToFloatDef(val,0);
  if dt=0 then
    result:=''
  else
    result:=FormatDateTime(datetimedisplay,dt);
end;

procedure Tf_notelun.SetInfoDate(val:string);
begin
  Finfodate:=StrToFloatDef(val,0);
  if Finfodate=0 then
    InfoDate.Text:=''
  else
    InfoDate.Text:=FormatDateTime(datetimedisplay,Finfodate);
end;

procedure Tf_notelun.NotesList;
var sortcol,cmd: string;
    i,n: integer;
    id:TNoteID;
    ok: boolean;
begin
  ClearList;
  n:=1;
  sortcol:='FORMATION';
  cmd:='select ID,FORMATION,DATE from infonotes order by '+sortcol;
  dbnotes.Query(cmd);
  ListNotes.RowCount:=ListNotes.RowCount+dbnotes.RowCount;
  for i:=0 to dbnotes.RowCount-1 do begin
    id:=TNoteID.Create;
    id.id:=dbnotes.Results[i].Format[0].AsInteger;
    ListNotes.Objects[0,n]:=id;
    ListNotes.Cells[0,n]:=dbnotes.Results[i][1];
    ListNotes.Cells[1,n]:=FormatDate(dbnotes.Results[i][2]);
    ListNotes.Cells[2,n]:='I';
    inc(n);
  end;
  cmd:='select ID,FORMATION,DATESTART from obsnotes order by '+sortcol;
  dbnotes.Query(cmd);
  ListNotes.RowCount:=ListNotes.RowCount+dbnotes.RowCount;
  for i:=0 to dbnotes.RowCount-1 do begin
    id:=TNoteID.Create;
    id.id:=dbnotes.Results[i].Format[0].AsInteger;
    ListNotes.Objects[0,n]:=id;
    ListNotes.Cells[0,n]:=dbnotes.Results[i][1];
    ListNotes.Cells[1,n]:=FormatDate(dbnotes.Results[i][2]);
    ListNotes.Cells[2,n]:='O';
    inc(n);
  end;
  ListNotesSelectCell(ListNotes,0,1,ok);
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
end;

procedure Tf_notelun.ClearObsNote;
begin
  Fobsdate:=0;
  ObsFormation.Text:='';
  ObsDate.Text:='';
  ObsPlace.Text:='';
  ObsName.Text:='';
  ObsStart.Text:='';
  ObsEnd.Text:='';
  ObsMeteo.Text:='';
  ObsSeeing.Text:='';
  ObsInstrument.Text:='';
  ObsOptic.Text:='';
  ObsPower.Text:='';
  ObsCamera.Text:='';
  ObsText.Text:='';
  ObsFiles.Clear;
  ObsRA.Caption:='';
  ObsDec.Caption:='';
  ObsDiam.Caption:='';
  ObsLunation.Caption:='';
  ObsColongitude.Caption:='';
  ObsLibrLon.Caption:='';
  ObsLibrLat.Caption:='';
  ObsAzimut.Caption:='';
  ObsAltitude.Caption:='';
end;

procedure Tf_notelun.ListNotesSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
var i: LongWord;
    id: TNoteID;
begin
  id:=TNoteID(ListNotes.Objects[0,aRow]);
  if id<>nil then begin
    i:=id.id;
    if ListNotes.Cells[2,aRow]='O' then begin
      ShowObsNote(i);
    end
    else if ListNotes.Cells[2,aRow]='I' then begin
      ShowInfoNote(i);
    end;
    CanSelect:=true;
  end
  else begin
    CanSelect:=false;
  end;
end;

procedure Tf_notelun.ShowInfoNote(id: integer);
var cmd: string;
begin
  PageControl1.ActivePageIndex:=1;
  ClearInfoNote;
  cmd:='select FORMATION,DATE,AUTHOR,NOTE,FILES from infonotes where ID='+inttostr(id);
  dbnotes.Query(cmd);
  if dbnotes.RowCount>0 then begin
    InfoFormation.Text:=dbnotes.Results[0][0];
    SetInfoDate(dbnotes.Results[0][1]);
    InfoAuthor.Text:=dbnotes.Results[0][2];
    InfoText.Text:=dbnotes.Results[0][3];
    InfoFiles.Items.Add(dbnotes.Results[0][4]);
  end;
end;

procedure Tf_notelun.ShowObsNote(id: integer);
var cmd: string;
begin
  PageControl1.ActivePageIndex:=0;
  ClearObsNote;
end;

end.

