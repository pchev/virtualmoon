unit calclun_main;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef mswindows}
    Windows, Registry, ShlObj,
  {$endif}
  cspice, pas_spice, moon_spice, u_util, u_constant, TAGraph, TARadialSeries, TASeries, TAFuncSeries, IniFiles,
  TAChartUtils, TAIntervalSources, math, u_projection, cu_tz, LazUTF8, config, u_translation, FileUtil, downloaddialog, splashunit,
  passql, passqlite, dbutil, pu_ephem,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Spin, ComCtrls, Grids, Menus, Buttons,
  LCLVersion, Types, TACustomSeries, TAMultiSeries, TATransformations;

const
    mcolDay=0; mcolRa2000=1; mcolDe2000=2; mcolRa=3; mcolDe=4; mcolDist=5; mcolDiam=6; mcolPhase=7; mcolLunation=8; mcolIllum=9; mcolColong=10; mcolSubSolLat=11; mcolLibrLon=12; mcolLibrLat=13; mcolPa=14; mcolRise=15; mcolSet=16;
    dcolHour=0; dcolRa2000=1; dcolDe2000=2; dcolRa=3; dcolDe=4; dcolDist=5; dcolDiam=6; dcolColong=7; dcolSubSolLat=8; dcolLibrLon=9; dcolLibrLat=10; dcolPa=11;
    VersionName = 'CalcLun';

type

  TMoonMonthData = class(TObject)
    day: integer;
    date: TDateTime;
    ra2000,de2000,ra,de,dist,diam,phase,lunation,illum,colong,subsollat,librlong,librlat,pa,trise,tset: double;
  end;

  TYearInfo = record
    dst: double;                             // DST offset for the day
    moonra,moondec,sunra,sundec: double;     // all time are local at january 1
    moonrise,moonset,sunrise,sunset: double;
    astro1,astro2,nautic1,nautic2,civil1,civil2: double;
  end;

  { Tf_calclun }

  Tf_calclun = class(TForm)
    BtnDecTime: TSpeedButton;
    BtnGraph: TSpeedButton;
    BtnIncTime: TSpeedButton;
    BtnToday: TSpeedButton;
    BtnSearch: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart2: TChart;
    Chart2CubicSplineSeries1: TCubicSplineSeries;
    Chart2LineSeries1: TLineSeries;
    ChartAltAzCivilArea: TAreaSeries;
    ChartAltAzNauticArea: TAreaSeries;
    ChartAltAzAstroArea: TAreaSeries;
    ChartAltazSunArea: TAreaSeries;
    ChartAltAzLineSeries2: TLineSeries;
    ChartYear: TChart;
    ChartAltAz: TChart;
    ChartAltAzLineSeries1: TLineSeries;
    Chart3: TChart;
    ChartYearBoxAndWhiskerSeries1: TBoxAndWhiskerSeries;
    ChartYearBoxAndWhiskerSeries2: TBoxAndWhiskerSeries;
    Chart3BoxAndWhiskerSeries1: TBoxAndWhiskerSeries;
    Chart3BoxAndWhiskerSeries2: TBoxAndWhiskerSeries;
    ChartYearSeriesAstro1: TAreaSeries;
    ChartYearSeriesAstro2: TAreaSeries;
    Chart3SeriesAstro1: TAreaSeries;
    Chart3SeriesAstro2: TAreaSeries;
    ChartYearSeriesNautic1: TAreaSeries;
    ChartYearSeriesNautic2: TAreaSeries;
    Chart3SeriesNautic1: TAreaSeries;
    Chart3SeriesNautic2: TAreaSeries;
    ChartYearSeriesSunrise: TAreaSeries;
    Chart3SeriesSunrise: TAreaSeries;
    ChartYearSeriesSunset: TAreaSeries;
    Chart3SeriesSunset: TAreaSeries;
    CheckBoxNightEvents: TCheckBox;
    CheckBoxMoonVisible: TCheckBox;
    DirLon: TComboBox;
    DirLat: TComboBox;
    ComboBoxFormation: TComboBox;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    DayPhase: TImage;
    EditColongitudeDelta: TFloatSpinEdit;
    EditFormation: TEdit;
    LibrLon: TFloatSpinEdit;
    LibrLat: TFloatSpinEdit;
    GridLibration: TStringGrid;
    GridTerminator1: TStringGrid;
    GridTerminator2: TStringGrid;
    GridColongitude: TStringGrid;
    GroupBoxLibration: TGroupBox;
    GroupBoxDuration: TGroupBox;
    GroupBoxConstraint: TGroupBox;
    GroupBoxColongitude: TGroupBox;
    Label1: TLabel;
    Label11: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelMissingDB: TLabel;
    LabelMonth: TLabel;
    LabelDay: TLabel;
    LabelTime: TLabel;
    LabelTZ: TLabel;
    MemoDay: TMemo;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    MenuItemDocumentation: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuQuit: TMenuItem;
    MenuPrintEphem: TMenuItem;
    PageControlPrediction: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    PanelLibration: TPanel;
    PanelFormation: TPanel;
    PanelGlobal: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PanelTerminator1: TPanel;
    PanelTerminator2: TPanel;
    PanelColongitude: TPanel;
    PanelTresult: TScrollBox;
    DayHour: TSpinEdit;
    DayMin: TSpinEdit;
    DaySec: TSpinEdit;
    PredictionDuration: TSpinEdit;
    EditTwilight: TSpinEdit;
    EditMinElevation: TSpinEdit;
    SunMin: TFloatSpinEdit;
    SunMax: TFloatSpinEdit;
    FormationLong: TFloatSpinEdit;
    FormationLat: TFloatSpinEdit;
    GroupBoxFormation: TGroupBox;
    GroupBoxCoord: TGroupBox;
    GroupBoxSunElev: TGroupBox;
    Label10: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    LabelChartYear: TLabel;
    DateChangeTimer: TTimer;
    MainMenu1: TMainMenu;
    GridLibrationList: TStringGrid;
    MenuSetup: TMenuItem;
    PanelT: TPanel;
    PanelChartAltAz: TPanel;
    PanelChartYear: TPanel;
    PanelGraph3: TPanel;
    ScrollBoxY2: TScrollBox;
    EditColongitude: TFloatSpinEdit;
    TabSheetLibration: TTabSheet;
    TabSheetColongitude: TTabSheet;
    TabSheetSunElevation: TTabSheet;
    TabSheetPrediction: TTabSheet;
    TerminatorTimer: TTimer;
    ColongitudeTimer: TTimer;
    LibrationTimer: TTimer;
    TZspinedit: TFloatSpinEdit;
    GridYear: TStringGrid;
    ImageListPhase: TImageList;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelAltAz: TLabel;
    LabelRise1: TLabel;
    LabelRise2: TLabel;
    LabelPhase2: TLabel;
    LabelPhase1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PanelBtnGraph: TPanel;
    PanelAltAz: TPanel;
    PanelRiseSet: TPanel;
    PanelDRight: TPanel;
    PanelYRight: TPanel;
    PanelYList: TPanel;
    PanelYphase: TPanel;
    PanelGraph2: TPanel;
    PanelGraph1: TPanel;
    PanelPhase: TPanel;
    PopupMenuGraph: TPopupMenu;
    ScrollBoxD: TScrollBox;
    ScrollBoxM: TScrollBox;
    ScrollBoxY: TScrollBox;
    SpinEditDay: TSpinEdit;
    StatusLabel: TLabel;
    PageControl1: TPageControl;
    PanelRight: TPanel;
    PanelLeft: TPanel;
    PanelBottom: TPanel;
    PanelTop: TPanel;
    SpinEditYear: TSpinEdit;
    SpinEditMonth: TSpinEdit;
    GridMonth: TStringGrid;
    GridPhaseList: TStringGrid;
    GridDay: TStringGrid;
    TabSheetYear: TTabSheet;
    TabSheetDay: TTabSheet;
    TabSheetMonth: TTabSheet;
    procedure BtnDecTimeClick(Sender: TObject);
    procedure BtnGraphClick(Sender: TObject);
    procedure BtnIncTimeClick(Sender: TObject);
    procedure BtnTodayClick(Sender: TObject);
    procedure BtnSearchClick(Sender: TObject);
    procedure ChartAltAzMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ChartYearAxisList1MarkToText(var AText: String; AMark: Double);
    procedure ColongitudeTimerTimer(Sender: TObject);
    procedure ComboBoxFormationSelect(Sender: TObject);
    procedure ColongitudeChange(Sender: TObject);
    procedure FormationEditingDone(Sender: TObject);
    procedure DayTimeChange(Sender: TObject);
    procedure GridColongitudeDblClick(Sender: TObject);
    procedure GridDayHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure GridLibrationDblClick(Sender: TObject);
    procedure GridTerminator1DblClick(Sender: TObject);
    procedure GridTerminator2DblClick(Sender: TObject);
    procedure LibrationTimerTimer(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemDocumentationClick(Sender: TObject);
    procedure MenuPrintEphemClick(Sender: TObject);
    procedure MenuQuitClick(Sender: TObject);
    procedure PageControlPredictionChange(Sender: TObject);
    procedure PredictionChange(Sender: TObject);
    procedure PredictionEditingDone(Sender: TObject);
    procedure TerminatorChange(Sender: TObject);
    procedure LibrationChange(Sender: TObject);
    procedure OpenChart(Sender: TObject);
    procedure OpenChartAsync(data: PtrInt);
    procedure ChartYearMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DateChange(Sender: TObject);
    procedure DateChangeTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridMonthHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure GridYearDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure GridYearHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure GridYearSelection(Sender: TObject; aCol, aRow: Integer);
    procedure MenuSetupClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure SelectGraph(Sender: TObject);
    procedure TerminatorTimerTimer(Sender: TObject);
  private
    dbm: TLiteDB;
    et : SpiceDouble;
    obspos: TDouble3;
    tz: TCdCTimeZone;
    stdtz: double; // standard timezone, non dst. in day
    localoffset: double; // local-std offset in day
    FCurrentMonthGraph: integer;
    YearInfo: array[0..13,1..31] of TYearInfo;
    FirstChange,ForceDateChange: boolean;
    LockPredictionTimer: boolean;
    CurColongColong,CurColongDelta,CurColondDt: double;
    CurColongDuration,CurColongSunElev,CurColongMoonElev: integer;
    CurColongNight,CurColongVisible: boolean;
    CurSunElevLon,CurSunElevLat,CurSunElevDt,CurSunElevSunMin,CurSunElevSunMax: double;
    CurSunElevDuration,CurSunElevSunElev,CurSunElevMoonElev: integer;
    CurSunElevNight,CurSunElevVisible: boolean;
    CurLibrationLon,CurLibrationLat,CurLibrationDt,CurLibrationLibrLon,CurLibrationLibrLat: double;
    CurLibrationEW,CurLibrationNS,CurLibrationDuration,CurLibrationSunElev,CurLibrationMoonElev: integer;
    CurLibrationNight,CurLibrationVisible: boolean;
    StartVMA: boolean;
    procedure SetLang;
    procedure GetAppDir;
    procedure SetKernelsPath;
    procedure CheckUpdate;
    procedure SetObservatory;
    function  GetTimeZone(sdt: Tdatetime): double;
    function  GetTimeZoneD(sdt: Tdatetime): double;
    function  GetYearInfo(m,d,doffset:integer):TYearInfo;
    procedure ComputeYear;
    procedure ComputeMonth;
    procedure ComputeDay;
    procedure PlotYearGraph;
    procedure PlotMonthGraph(col:integer);
    procedure PlotMonthTwilight;
    procedure PlotDayGraph;
    procedure CloseChartClick(Sender: TObject);
    procedure CloseChart(Sender: TObject; var CloseAction: TCloseAction);
    procedure SelectFormation(id: integer);
    procedure ComputeColongitude;
    procedure ComputeTerminator;
    procedure ComputeLibration;
    procedure OpenAtlun(dt,nam:string);

  public

  end;

var
  f_calclun: Tf_calclun;


implementation

{$R *.lfm}

{ Tf_calclun }

procedure Tf_calclun.FormCreate(Sender: TObject);
var inifile:Tmeminifile;
begin
  ScaleFormForFontSize(self,96);
  DefaultFormatSettings.DateSeparator:='/';
  DefaultFormatSettings.TimeSeparator:=':';
  DefaultFormatSettings.DecimalSeparator:='.';
  compile_time := {$I %DATE%}+' '+{$I %TIME%};
  compile_version := 'Lazarus '+lcl_version+' Free Pascal '+{$I %FPCVERSION%}+' '+{$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%};
  StatusLabel.Caption:='';
  LabelAltAz.Caption:='';
  LabelChartYear.Caption:='';
  ObsLatitude:=0;
  ObsLongitude:=0;
  ObsAltitude:=0;
  ObsTZ:='Etc/GMT';
  FirstChange:=true;
  ForceDateChange:=false;
  LockPredictionTimer:=false;
  dbm:=TLiteDB.Create(self);
  DatabaseList:=Tstringlist.Create;
  ConnectDatabaseList:=Tstringlist.Create;
  datestd:=datetimedisplay;

  Top := 50;
  Left := 50;
  Height := min(1000,screen.Height - 100);
  Width := min(1400,screen.Width - 100);

  GetAppDir;
  inifile := Tmeminifile.Create(ConfigFile);
  language:= inifile.ReadString('default', 'lang_po_file', '');
  inifile.Free;
  language:=u_translation.translate(language,'en');
  uplanguage:=UpperCase(language);
  SetLang;
  tz:=TCdCTimeZone.Create;
  tz.LoadZoneTab(ZoneDir+'zone.tab');

end;

procedure Tf_calclun.FormDestroy(Sender: TObject);
var i: integer;
begin
  dbm.free;
  DatabaseList.free;
  for i:=1 to ConnectDatabaseList.Count do
    ConnectDBCols[i].free;
  ConnectDatabaseList.Free;
  tz.Free;
  for i:=0 to GridMonth.RowCount-1 do begin
    if GridMonth.Objects[0,i]<>nil then GridMonth.Objects[0,i].free;
  end;
end;

procedure Tf_calclun.FormShow(Sender: TObject);
var i: integer;
begin
  for i:=1 to maxdbn do usedatabase[i]:=false;
  usedatabase[1]:=true;
  LoadDB(dbm);

  InitError;
  reset_c;
  SetKernelsPath;

  furnsh_c(pchar(slash(PrivateDir)+'vma.tm'));
  if failed_c then begin
    ShowMessage('Error loading file!'+crlf+SpiceLastError);
    halt;
  end;

  BtnToday.Click;
  PageControl1.ActivePage:=TabSheetYear;
  label10.Caption:=rsByYear;
  FCurrentMonthGraph:=mcolLibrLon;
  PageControlPrediction.ActivePageIndex:=0;
  PanelGlobal.Parent:=TabSheetColongitude;
  PanelFormation.Visible:=false;
  GroupBoxColongitude.Visible:=true;
  GroupBoxSunElev.Visible:=false;
  GroupBoxLibration.Visible:=false;

  SetObservatory;
  f_config.tzinfo:=tz;
  f_config.LoadCountry(slash(Appdir)+slash('data')+'country.tab');
  DateChange(nil);
end;

procedure Tf_calclun.SetLang;
var i: integer;
  buf: string;
begin
  ldeg     := '°';
  lmin     := '''';
  lsec     := '"';

  u_util.hp := rshelp_prefix;
  Label7.Caption:=rsYear;
  Label8.Caption:=rsMonth;
  Label9.Caption:=rsDay;
  BtnToday.Caption:=rsToday;
  MenuHelp.Caption:=rsHelp;
  MenuItemDocumentation.Caption:=rsHelp;
  MenuItemAbout.Caption:=rsAbout;

  TabSheetYear.Caption:=rsYear;
  ChartYear.Title.Text.Clear;
  ChartYear.Title.Text.Add(rsTwilightAndM);
  Panel6.Caption:=rsMoonPhase;
  Panel7.Caption:=rsLibrationExt;
  GridYear.ColWidths[0]:=120;
  GridYear.Width:=GridYear.ColWidths[0]+(GridYear.ColCount-1)*GridYear.DefaultColWidth;
  for i:=1 to 31 do GridYear.Cells[i,0]:=inttostr(i);
  GridYear.Cells[0, 1]:=rsJanuary;
  GridYear.Cells[0, 2]:=rsFebruary;
  GridYear.Cells[0, 3]:=rsMarch;
  GridYear.Cells[0, 4]:=rsApril;
  GridYear.Cells[0, 5]:=rsMay;
  GridYear.Cells[0, 6]:=rsJune;
  GridYear.Cells[0, 7]:=rsJuly;
  GridYear.Cells[0, 8]:=rsAugust;
  GridYear.Cells[0, 9]:=rsSeptember;
  GridYear.Cells[0, 10]:=rsOctober;
  GridYear.Cells[0, 11]:=rsNovember;
  GridYear.Cells[0, 12]:=rsDecember;
  GridPhaseList.Cells[0, 0]:=rsNewMoon;
  GridPhaseList.Cells[1, 0]:=rsFirstQuarter;
  GridPhaseList.Cells[2, 0]:=rsFullMoon;
  GridPhaseList.Cells[3, 0]:=rsLastQuarter;
  GridLibrationList.Cells[0, 0]:=rsEast;
  GridLibrationList.Cells[1, 0]:=rsWest;
  GridLibrationList.Cells[2, 0]:=rsNorth;
  GridLibrationList.Cells[3, 0]:=rsSouth;

  TabSheetMonth.Caption:=rsMonth;
  BtnGraph.Caption:=rsSelectGraph;
  Chart3.Title.Text.Clear;
  Chart3.Title.Text.Add(rsTwilightAndM);
  MenuSetup.Caption:=rsSetup;
  MenuItem1.Caption:=rsRA2000;
  Menuitem2.Caption:=rsDE2000;
  Menuitem3.Caption:=rsRAApparent;
  Menuitem4.Caption:=rsDEApparent;
  Menuitem5.Caption:=rsDistance;
  Menuitem6.Caption:=rsDiameter;
  Menuitem7.Caption:=rsPhase;
  Menuitem8.Caption:=rsLunation;
  Menuitem9.Caption:=rsIllumination;
  Menuitem10.Caption:=rsColongitude;
  Menuitem11.Caption:=rsSubSolarLati;
  Menuitem12.Caption:=rsLibrationLon;
  Menuitem13.Caption:=rsLibrationLat;
  Menuitem14.Caption:=rsPA;
  Menuitem15.Caption:=rsRiseTime;
  Menuitem16.Caption:=rsSetTime;
  GridMonth.ColCount:=mcolSet+1;
  GridMonth.ColWidths[0]:=50;
  GridMonth.Cells[mcolDay, 0]:=rsDay;
  GridMonth.Cells[mcolRa2000, 0]:=rsRA2000;
  GridMonth.Cells[mcolDe2000, 0]:=rsDE2000;
  GridMonth.Cells[mcolRa, 0]:=rsRAApparent;
  GridMonth.Cells[mcolDe, 0]:=rsDEApparent;
  GridMonth.Cells[mcolDist, 0]:=rsDistance;
  GridMonth.Cells[mcolDiam, 0]:=rsDiameter;
  GridMonth.Cells[mcolPhase, 0]:=rsPhase;
  GridMonth.Cells[mcolLunation, 0]:=rsLunation;
  GridMonth.Cells[mcolIllum, 0]:=rsIllumination;
  GridMonth.Cells[mcolColong, 0]:=rsColongitude;
  GridMonth.Cells[mcolSubSolLat, 0]:=rsSubSolarLati;
  GridMonth.Cells[mcolLibrLon, 0]:=rsLibrationLon;
  GridMonth.Cells[mcolLibrLat, 0]:=rsLibrationLat;
  GridMonth.Cells[mcolPa, 0]:=rsPA;
  GridMonth.Cells[mcolRise, 0]:=rsRiseTime;
  GridMonth.Cells[mcolSet, 0]:=rsSetTime;
  LabelPhase1.Caption:=rsNewMoon+':'+crlf+
                      rsFirstQuarter+':'+crlf+
                      rsFullMoon+':'+crlf+
                      rsLastQuarter+':';

  TabSheetDay.Caption:=rsDay;
  LabelTime.Caption:=rsTime;
  GridDay.ColCount:=dcolPa+1;
  GridDay.ColWidths[0]:=50;
  GridDay.Cells[dcolHour, 0]:=rsHour;
  GridDay.Cells[dcolRa2000, 0]:=rsRA2000;
  GridDay.Cells[dcolDe2000, 0]:=rsDE2000;
  GridDay.Cells[dcolRa, 0]:=rsRAApparent;
  GridDay.Cells[dcolDe, 0]:=rsDEApparent;
  GridDay.Cells[dcolDist, 0]:=rsDistance;
  GridDay.Cells[dcolDiam, 0]:=rsDiameter;
  GridDay.Cells[dcolColong, 0]:=rsColongitude;
  GridDay.Cells[dcolSubSolLat, 0]:=rsSubSolarLati;
  GridDay.Cells[dcolLibrLon, 0]:=rsLibrationLon;
  GridDay.Cells[dcolLibrLat, 0]:=rsLibrationLat;
  GridDay.Cells[dcolPa, 0]:=rsPA;
  LabelRise1.Caption:=rsRise+' :'+crlf+rsSet+' :';

  TabSheetPrediction.Caption:=rsPrediction;
  TabSheetColongitude.Caption:=rsColongitude;
  TabSheetSunElevation.Caption:=rsSunElevation3;
  TabSheetLibration.Caption:=rsLibration;
  GroupBoxColongitude.Caption:=rsColongitude;
  Label16.Caption:=rsColongitude;
  GroupBoxDuration.Caption:=rsPredictionDu;
  Label3.Caption:=rsYear;
  GroupBoxFormation.Caption:=rsSearchFormat;
  BtnSearch.Caption:=rsSearch;
  LabelMissingDB.Caption:=rsDatabaseNotF;
  GroupBoxCoord.Caption:=rsFormationCoo;
  Label12.Caption:=rsLongitude;
  Label13.Caption:=rsLatitude;
  GroupBoxConstraint.Caption:=rsConstraints;
  CheckBoxNightEvents.Caption:=rsOnlyNightEve;
  CheckBoxMoonVisible.Caption:=rsMoonVisibleF;
  label2.Caption:=rsSun;
  Label5.Caption:=rsDegreeBelowH;
  label4.Caption:=rsMoon;
  Label6.Caption:=rsDegreeAboveH;
  GroupBoxSunElev.Caption:=rsSunElevation2;
  Label14.Caption:=rsMinimum;
  Label15.Caption:=rsMaximum;
  GroupBoxLibration.Caption:=rsLibration;
  label11.Caption:=rsLongitude;
  label17.Caption:=rsLatitude;
  Panel11.Caption:=rsPredictionFo;
  GridLibration.Cells[0, 0]:=rsStartTime;
  GridLibration.Cells[1, 0]:=rsEndTime;
  GridLibration.Cells[2, 0]:=rsSunElevation;
  GridLibration.Cells[3, 0]:=rsSunElevation4;
  Panel10.Caption:=rsTimePredicti3;
  GridColongitude.Cells[0, 0]:=rsStartTime;
  GridColongitude.Cells[1, 0]:=rsEndTime;
  GridColongitude.Cells[2, 0]:=rsSubSolarLati;
  Panel1.Caption:=rsTimePredicti;
  GridTerminator1.ColWidths[2]:=80;
  GridTerminator1.ColWidths[3]:=80;
  GridTerminator1.Cells[0, 0]:=rsStartTime;
  GridTerminator1.Cells[1, 0]:=rsEndTime;
  GridTerminator1.Cells[2, 0]:=rsLibrLon;
  GridTerminator1.Cells[3, 0]:=rsLibrLat;
  Panel2.Caption:=rsTimePredicti2;
  GridTerminator2.ColWidths[2]:=80;
  GridTerminator2.ColWidths[3]:=80;
  GridTerminator2.Cells[0, 0]:=rsStartTime;
  GridTerminator2.Cells[1, 0]:=rsEndTime;
  GridTerminator2.Cells[2, 0]:=rsLibrLon;
  GridTerminator2.Cells[3, 0]:=rsLibrLat;
{  dbm.Use(Slash(DBdir)+'dbmoon8'+UpperCase(language)+'.dbl');
  buf:=dbm.QueryOne('select version from dbversion;');
  if buf>'' then begin
    EditFormation.Enabled:=true;
    BtnSearch.Enabled:=true;
    ComboBoxFormation.Visible:=true;
    LabelMissingDB.Visible:=false;
  end
  else begin
    EditFormation.Enabled:=false;
    BtnSearch.Enabled:=false;
    ComboBoxFormation.Visible:=false;
    LabelMissingDB.Visible:=true;
  end;}
end;

procedure Tf_calclun.SetKernelsPath;
var buf: string;
    fi,fo: TextFile;
begin
  // update data
  CheckUpdate;
  // if it fail use the distributed data
  if (not FileExists(slash(PrivateDir)+'latest_leapseconds.tls')) then
    CopyFile(slash(appdir) + slash('data') + slash('kernels')+'latest_leapseconds.tls',slash(PrivateDir)+'latest_leapseconds.tls',true,true);
  if (not FileExists(slash(PrivateDir)+'earth_latest_high_prec.bpc')) then
    CopyFile(slash(appdir) + slash('data') + slash('kernels')+'earth_latest_high_prec.bpc',slash(PrivateDir)+'earth_latest_high_prec.bpc',true,true);
  AssignFile(fi,slash(appdir) + slash('data') + slash('kernels')+'vma.tm');
  Reset(fi);
  AssignFile(fo,slash(PrivateDir)+'vma.tm');
  Rewrite(fo);
  repeat
    ReadLn(fi,buf);
    if copy(buf,1,11)='PATH_VALUES' then begin
      buf:='PATH_VALUES = ( '''+slash(appdir) + slash('data') + slash('kernels')+''',';
      WriteLn(fo,buf);
      buf:='           '''+slash(privatedir)+''')';
    end;
    WriteLn(fo,buf);
  until eof(fi);
  CloseFile(fi);
  CloseFile(fo);
end;

procedure Tf_calclun.CheckUpdate;
var dl: TDownloadDialog;
    fn: string;
begin
 dl:=TDownloadDialog.Create(self);
 try
 fn:=slash(PrivateDir)+'earth_latest_high_prec.bpc';
 if (not FileExists(fn)) or ((now-FileDateToDateTime(FileAge(fn))) > 30)
 then begin
    dl.ConfirmDownload := False;
    dl.URL := 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels/pck/earth_latest_high_prec.bpc';
    dl.SaveToFile := fn;
    if not dl.Execute then
      deletefile(fn);
 end;
 fn:=slash(PrivateDir)+'latest_leapseconds.tls';
 if (not FileExists(fn)) or ((now-FileDateToDateTime(FileAge(fn))) > 30)
 then begin
    dl.ConfirmDownload := False;
    dl.URL := 'https://naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/latest_leapseconds.tls';
    dl.SaveToFile := fn;
    if not dl.Execute then
      deletefile(fn);
 end;
 finally
  dl.free;
 end;
end;

procedure Tf_calclun.GetAppDir;
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
  HomeDir := DefaultHomeDir;
{$ifdef unix}
  appdir     := expandfilename(appdir);
  bindir     := slash(appdir);
  privatedir := expandfilename(PrivateDir);
  Homedir := expandfilename(Homedir);
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
  SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PIDL);
  SHGetPathFromIDList(PIDL, Folder);
  homedir := trim(WinCPToUTF8(Folder));
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
  if not FileExists(slash(bindir)+'atlun') then begin
     bindir := slash(ExtractFilePath(ParamStr(0)));
     if not FileExists(slash(bindir)+'atlun') then begin
        bindir := slash(ExpandFileName(slash(appdir) + slash('..')+slash('..')+'bin'));
        if not FileExists(slash(bindir)+'atlun') then begin
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

procedure Time_Alt(jd, ar, de, h: double; out hp1, hp2: double; Lat, Long: double);
var
  hh, st, st0: double;
begin
  if (ar<0) then ar:=ar+pi2;
  hh := (sin(deg2rad * h) - sin(Lat) * sin(de)) / (cos(Lat) * cos(de));
  if abs(hh) <= 1 then
  begin
    hh := double(arccos(hh));
    st0 := rad2deg * sidtim(jd, 0.0, -rad2deg*Long) / 15;
    st := rad2deg * (ar - hh) / 15;
    hp1 := rmod((st - st0) / 1.002737908 + 24, 24);
    st := rad2deg * (ar + hh) / 15;
    hp2 := rmod((st - st0) / 1.002737908 + 24, 24);
  end
  else
  begin
    if hh > 0 then
    begin
      hp1 := -99;      // never above H
      hp2 := -99;
    end
    else
    begin
      hp1 := 99;      // always above H
      hp2 := 99;
    end;
  end;
end;


procedure Tf_calclun.ComputeYear;
var
  t1,dt,nm,fq,fm,lq,lunation,jd0,ct1,ct2: double;
  i,j,k,r: integer;
  year: Word;
  newmoon: array[0..14] of double;
  refval: SpiceDouble;
  Pcnfine,Presult: PSpiceCell;
  nfind: SpiceInt;
  et0,et1: SpiceDouble;
  yy,mm,dd: Word;
  x,y,sx,sy,sz,sr,ra,de,llon,llat : SpiceDouble;
  dtr, dts: double;
  obsref: ConstSpiceChar;
  fixref,coord,relate: ConstSpiceChar;

  procedure PositionTwilight(year,i,im,j:integer);
  begin
    dt:=EncodeDate(year,i,j);
    et:=DateTime2ET(dt-stdtz);
    if not SunTopocentric(et,true,obspos,obsref,sx,sy,sz,sr,ra,de) then begin
      StatusLabel.Caption:=SpiceLastError;
      exit;
    end;
    // twilight
    jd0:=Jd(year,i,j,0);
    Time_Alt(jd0,ra,de,-18,ct1,ct2,ObsLatitude,ObsLongitude);
    if abs(ct1)<=24 then YearInfo[im,j].astro1:=trunc(dt)+stdtz+ct1/24 else YearInfo[im,j].astro1:=0;
    if abs(ct2)<=24 then YearInfo[im,j].astro2:=trunc(dt)+stdtz+ct2/24 else YearInfo[im,j].astro2:=0;
    Time_Alt(jd0,ra,de,-12,ct1,ct2,ObsLatitude,ObsLongitude);
    if abs(ct1)<=24 then YearInfo[im,j].nautic1:=trunc(dt)+stdtz+ct1/24 else YearInfo[im,j].nautic1:=0;
    if abs(ct2)<=24 then YearInfo[im,j].nautic2:=trunc(dt)+stdtz+ct2/24 else YearInfo[im,j].nautic2:=0;
    Time_Alt(jd0,ra,de,-6,ct1,ct2,ObsLatitude,ObsLongitude);
    if abs(ct1)<=24 then YearInfo[im,j].civil1:=trunc(dt)+stdtz+ct1/24 else YearInfo[im,j].civil1:=0;
    if abs(ct2)<=24 then YearInfo[im,j].civil2:=trunc(dt)+stdtz+ct2/24 else YearInfo[im,j].civil2:=0;
    YearInfo[im,j].sunra:=ra;
    YearInfo[im,j].sundec:=de;
    if not MoonTopocentric(et,true,obspos,obsref,sx,sy,sz,sr,ra,de) then begin
      StatusLabel.Caption:=SpiceLastError;
      exit;
    end;
    YearInfo[im,j].moonra:=ra;
    YearInfo[im,j].moondec:=de;
    // fix incoherent value near circum-polar limit
    if (YearInfo[im,j].sunrise=0)and(YearInfo[im,j].sunset<>0)and(j>1) then YearInfo[im,j].sunrise:=YearInfo[im,j-1].sunrise;
    if (YearInfo[im,j].sunset=0)and(YearInfo[im,j].sunrise<>0)and(j>1) then YearInfo[im,j].sunset:=YearInfo[im,j-1].sunset;
    if (YearInfo[im,j].astro1=0)and(YearInfo[im,j].astro2<>0)and(j>1) then YearInfo[im,j].astro1:=YearInfo[im,j-1].astro1;
    if (YearInfo[im,j].astro2=0)and(YearInfo[im,j].astro1<>0)and(j>1) then YearInfo[im,j].astro2:=YearInfo[im,j-1].astro2;
    if (YearInfo[im,j].nautic1=0)and(YearInfo[im,j].nautic2<>0)and(j>1) then YearInfo[im,j].nautic1:=YearInfo[im,j-1].nautic1;
    if (YearInfo[im,j].nautic2=0)and(YearInfo[im,j].nautic1<>0)and(j>1) then YearInfo[im,j].nautic2:=YearInfo[im,j-1].nautic2;
    if (YearInfo[im,j].civil1=0)and(YearInfo[im,j].civil2<>0)and(j>1) then YearInfo[im,j].civil1:=YearInfo[im,j-1].civil1;
    if (YearInfo[im,j].civil2=0)and(YearInfo[im,j].civil1<>0)and(j>1) then YearInfo[im,j].civil2:=YearInfo[im,j-1].civil2;
  end;

begin  // ComputeYear
  GridPhaseList.RowCount:=1;
  GridPhaseList.RowCount:=15;
  GridLibrationList.RowCount:=1;
  GridLibrationList.RowCount:=15;
  LabelChartYear.Caption:='';
  // phase table
  reset_c;
  year:=SpinEditYear.Value;
  t1:=EncodeDate(year,12,31);
  dt:=EncodeDate(year-1,12,1);
  if not MoonPhases(dt, nm, fq, fm, lq,lunation) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  newmoon[0]:=nm;
  r:=1;
  repeat
    dt:=lq+7;
    if not MoonPhases(dt, nm, fq, fm, lq,lunation) then begin
      StatusLabel.Caption:=SpiceLastError;
      exit;
    end;
    newmoon[r]:=nm;
    GridPhaseList.Cells[0,r]:=FormatDateTime('mm/dd hh:nn:ss',nm+GetTimeZoneD(nm));
    GridPhaseList.Cells[1,r]:=FormatDateTime('mm/dd hh:nn:ss',fq+GetTimeZoneD(fq));
    GridPhaseList.Cells[2,r]:=FormatDateTime('mm/dd hh:nn:ss',fm+GetTimeZoneD(fm));
    GridPhaseList.Cells[3,r]:=FormatDateTime('mm/dd hh:nn:ss',lq+GetTimeZoneD(lq));
    inc(r);
  until (lq>t1)or(r>=GridPhaseList.RowCount);

  // year calendar
  GridYear.Cells[0,0]:=IntToStr(year);
  dt:=EncodeDate(year,1,1);
  r:=0;
  for i:=0 to 2 do begin
    if newmoon[i]+SynodicMonth>=dt then break;
    inc(r);
  end;
  for i:=1 to 12 do begin
    for j:=1 to 31 do begin
      if j<=MonthDays[IsLeapYear(year)][i] then begin
       dt:=EncodeDate(year,i,j);
       if newmoon[r]+SynodicMonth<dt then inc(r);
       k:=trunc(dt-newmoon[r]);
       GridYear.Cells[j,i]:=inttostr(k);
      end
      else
       GridYear.Cells[j,i]:=' ';
    end;
  end;
  // standard timezone
  stdtz:=GetTimeZoneD(EncodeDate(year,1,1));
  if tz.Daylight then stdtz:=GetTimeZoneD(EncodeDate(year,6,30));
  // local time
  localoffset:=-ObsLongitude*rad2deg/15/24-stdtz;
  // initialize timezone
  FillByte(YearInfo,12*31*sizeof(TYearInfo),0);
  for i:=1 to 12 do begin
    for j:=1 to 31 do begin
      if j<=MonthDays[IsLeapYear(year)][i] then
         YearInfo[i,j].dst:=stdtz-GetTimeZoneD(EncodeDate(year,i,j)+0.5)
      else
         YearInfo[i,j].dst:=0;
    end;
  end;

  // Moon rise and set for the year
  Pcnfine:=initdoublecell(1);
  Presult:=initdoublecell(2);
  dt:=EncodeDate(year,1,1);
  et:=DateTime2ET(dt);
  et0:=et;
  et1:=et0+366*SecsPerDay;
  wninsd_c ( et0, et1, Pcnfine );
  // expand by two day to get a margin a the beginning and end of the year
  wnexpd_c ( 2*SecsPerDay, 2*SecsPerDay, Pcnfine );
  refval := -0.8;  // elevation degree, upper limb with refraction
  if not MoonSearchRiseSet(obspos,ObsLatitude,refval,nfind,Pcnfine,Presult) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  if nfind>1 then for i:=0 to nfind-1 do begin
    wnfetd_c(Presult,i,x,y);
    dtr:=ET2DateTime(x)+stdtz;
    DecodeDate(dtr,yy,mm,dd);
    if yy<year then mm:=0;      // one of the two day in previous year
    if yy>year then mm:=13;     // one of the two day in next year
    YearInfo[mm,dd].moonrise:=dtr;
    dts:=ET2DateTime(y)+stdtz;
    DecodeDate(dts,yy,mm,dd);
    if yy<year then mm:=0;
    if yy>year then mm:=13;
    YearInfo[mm,dd].moonset:=dts;
  end;
  scard_c (0, Pcnfine);
  scard_c (0, Presult);

  // Sun rise, set for the year
  Pcnfine:=initdoublecell(1);
  Presult:=initdoublecell(2);
  dt:=EncodeDate(year,1,1);
  et:=DateTime2ET(dt);
  et0:=et;
  et1:=et0+366*SecsPerDay;
  wninsd_c ( et0, et1, Pcnfine );
  // expand by two day
  wnexpd_c ( 2*SecsPerDay, 2*SecsPerDay, Pcnfine );
  refval := -0.8;  // elevation degree, upper limb with refraction
  if not SunSearchRiseSet(obspos,ObsLatitude,refval,nfind,Pcnfine,Presult) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  if nfind>1 then for i:=0 to nfind-1 do begin
    wnfetd_c(Presult,i,x,y);
    dtr:=ET2DateTime(x)+stdtz;
    DecodeDate(dtr,yy,mm,dd);
    if yy<year then mm:=0;
    if yy>year then mm:=13;
    YearInfo[mm,dd].sunrise:=dtr;
    dts:=ET2DateTime(y)+stdtz;
    DecodeDate(dts,yy,mm,dd);
    if yy<year then mm:=0;
    if yy>year then mm:=13;
    YearInfo[mm,dd].sunset:=dts;
  end;
  scard_c (0, Pcnfine);
  scard_c (0, Presult);

  // sun and moon geocentric position and twilight
  for i:=1 to 12 do begin
    for j:=1 to MonthDays[IsLeapYear(year)][i] do begin
      PositionTwilight(year,i,i,j);
    end;
  end;
  // previous year extra
  PositionTwilight(year-1,12,0,30);
  PositionTwilight(year-1,12,0,31);
  // next year extra
  PositionTwilight(year+1,1,13,1);
  PositionTwilight(year+1,1,13,2);

  PlotYearGraph;

  // Libration extrema for the year
  Pcnfine:=initdoublecell(1);
  Presult:=initdoublecell(2);
  dt:=EncodeDate(year,1,1);
  et:=DateTime2ET(dt);
  et0:=et;
  et1:=et0+366*SecsPerDay;
  wninsd_c ( et0, et1, Pcnfine );
  // expand by one day for the case the moon is already rised at the start of first interval
  wnexpd_c ( SecsPerDay, SecsPerDay, Pcnfine );
  fixref:=fixrefME;
  coord:='LONGITUDE';
  relate:='LOCMIN';
  refval:=0;
  if not MoonSearchLibration(coord,refval,relate,fixref,nfind,Pcnfine,Presult) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  r:=1;
  if nfind>1 then for i:=0 to nfind-1 do begin
    wnfetd_c(Presult,i,x,y);
    dtr:=ET2DateTime(x);
    if not MoonSubEarthPoint(x,fixref,sx,sy,sz,sr,llon,llat) then begin
      StatusLabel.Caption:=SpiceLastError;
      exit;
    end;
    GridLibrationList.Cells[1,r]:=FormatDateTime('mm/dd',dtr+GetTimeZoneD(dtr))+' '+DEmToStr(llon*rad2deg);
    inc(r);
    if r>=GridLibrationList.RowCount then break;
  end;
  scard_c (0, Presult);
  fixref:=fixrefME;
  coord:='LONGITUDE';
  relate:='LOCMAX';
  refval:=0;
  if not MoonSearchLibration(coord,refval,relate,fixref,nfind,Pcnfine,Presult) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  r:=1;
  if nfind>1 then for i:=0 to nfind-1 do begin
    wnfetd_c(Presult,i,x,y);
    dtr:=ET2DateTime(x);
    if not MoonSubEarthPoint(x,fixref,sx,sy,sz,sr,llon,llat) then begin
      StatusLabel.Caption:=SpiceLastError;
      exit;
    end;
    GridLibrationList.Cells[0,r]:=FormatDateTime('mm/dd',dtr+GetTimeZoneD(dtr))+' '+DEmToStr(llon*rad2deg);
    inc(r);
    if r>=GridLibrationList.RowCount then break;
  end;
  scard_c (0, Presult);
  fixref:=fixrefME;
  coord:='LATITUDE';
  relate:='LOCMIN';
  refval:=0;
  if not MoonSearchLibration(coord,refval,relate,fixref,nfind,Pcnfine,Presult) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  r:=1;
  if nfind>1 then for i:=0 to nfind-1 do begin
    wnfetd_c(Presult,i,x,y);
    dtr:=ET2DateTime(x);
    if not MoonSubEarthPoint(x,fixref,sx,sy,sz,sr,llon,llat) then begin
      StatusLabel.Caption:=SpiceLastError;
      exit;
    end;
    GridLibrationList.Cells[3,r]:=FormatDateTime('mm/dd',dtr+GetTimeZoneD(dtr))+' '+DEmToStr(llat*rad2deg);
    inc(r);
    if r>=GridLibrationList.RowCount then break;
  end;
  scard_c (0, Presult);
  fixref:=fixrefME;
  coord:='LATITUDE';
  relate:='LOCMAX';
  refval:=0;
  if not MoonSearchLibration(coord,refval,relate,fixref,nfind,Pcnfine,Presult) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  r:=1;
  if nfind>1 then for i:=0 to nfind-1 do begin
    wnfetd_c(Presult,i,x,y);
    dtr:=ET2DateTime(x);
    if not MoonSubEarthPoint(x,fixref,sx,sy,sz,sr,llon,llat) then begin
      StatusLabel.Caption:=SpiceLastError;
      exit;
    end;
    GridLibrationList.Cells[2,r]:=FormatDateTime('mm/dd',dtr+GetTimeZoneD(dtr))+' '+DEmToStr(llat*rad2deg);
    inc(r);
    if r>=GridLibrationList.RowCount then break;
  end;
  scard_c (0, Pcnfine);
  scard_c (0, Presult);
end;

procedure Tf_calclun.PlotYearGraph;
var i,j,k,l: integer;
    moonrise,moonset: double;
    d:Tdatetime;
    maxv,minv:double;
begin
  ChartYearSeriesSunrise.Clear;
  ChartYearSeriesSunset.Clear;
  ChartYearSeriesAstro1.Clear;
  ChartYearSeriesAstro2.Clear;
  ChartYearSeriesNautic1.Clear;
  ChartYearSeriesNautic2.Clear;
  ChartYearBoxAndWhiskerSeries1.Clear;
  ChartYearBoxAndWhiskerSeries2.Clear;
  maxv:=0.5;
  minv:=-0.5;
  moonrise:=0;
  moonset:=0;
  k:=StrToIntDef(GridYear.Cells[1,1],1);
  // Use same time zome for all the year to ensure graph continuity
  for i:=1 to 12 do begin
    for j:=1 to MonthDays[IsLeapYear(SpinEditYear.Value)][i] do begin
      d:=EncodeDate(SpinEditYear.Value,i,j);
      if ((YearInfo[i,j].sunrise=0)or(YearInfo[i,j].sunset=0))or(YearInfo[i,j].sunrise>YearInfo[i,j].sunset) then begin
       // no sun rise/set
       if sign(YearInfo[i,j].sundec)=sign(ObsLatitude) then begin
          // sun always visible
          ChartYearSeriesAstro1.AddXY(d,0);
          ChartYearSeriesAstro2.AddXY(d,0);
          ChartYearSeriesNautic1.AddXY(d,0);
          ChartYearSeriesNautic2.AddXY(d,0);
          ChartYearSeriesSunrise.AddXY(d,0);
          ChartYearSeriesSunset.AddXY(d,0);
       end
       else begin
         // sun do not rise
         if ((YearInfo[i,j].astro1=0)or(YearInfo[i,j].astro2=0))or(YearInfo[i,j].astro1>YearInfo[i,j].astro2) then begin
           // no astro twilight
           ChartYearSeriesAstro1.AddXY(d,0);
           ChartYearSeriesAstro2.AddXY(d,0);
         end
         else begin
           // astro twilight
           ChartYearSeriesAstro1.AddXY(d,(YearInfo[i,j].astro1+localoffset)-d);
           ChartYearSeriesAstro2.AddXY(d,max(minv,(YearInfo[i,j].astro2+localoffset)-d-1));
         end;
          if ((YearInfo[i,j].nautic1=0)or(YearInfo[i,j].nautic2=0))or(YearInfo[i,j].nautic1>YearInfo[i,j].nautic2) then begin
            // no nautic twilight
            ChartYearSeriesNautic1.AddXY(d,maxv);
            ChartYearSeriesNautic2.AddXY(d,minv);
            ChartYearSeriesSunrise.AddXY(d,minv);
            ChartYearSeriesSunset.AddXY(d,minv);
          end
          else begin
            // nautic twilight
            ChartYearSeriesNautic1.AddXY(d,(YearInfo[i,j].nautic1+localoffset)-d);
            ChartYearSeriesNautic2.AddXY(d,max(minv,(YearInfo[i,j].nautic2+localoffset)-d-1));
            ChartYearSeriesSunrise.AddXY(d,maxv);
            ChartYearSeriesSunset.AddXY(d,minv);
          end;
       end
      end
      else begin
        // sun rise and set
        ChartYearSeriesSunrise.AddXY(d,(YearInfo[i,j].sunrise+localoffset)-d);
        ChartYearSeriesSunset.AddXY(d,max(minv,(YearInfo[i,j].sunset+localoffset)-d-1));
        if ((YearInfo[i,j].astro1=0)or(YearInfo[i,j].astro2=0))or(YearInfo[i,j].astro1>YearInfo[i,j].astro2) then begin
          // no astro twilight
          ChartYearSeriesAstro1.AddXY(d,0);
          ChartYearSeriesAstro2.AddXY(d,0);
          if ((YearInfo[i,j].nautic1=0)or(YearInfo[i,j].nautic2=0))or(YearInfo[i,j].nautic1>YearInfo[i,j].nautic2) then begin
            // no nautic twilight
            ChartYearSeriesNautic1.AddXY(d,0);
            ChartYearSeriesNautic2.AddXY(d,0);
          end
          else begin
            //  nautic twilight
            ChartYearSeriesNautic1.AddXY(d,(YearInfo[i,j].nautic1+localoffset)-d);
            ChartYearSeriesNautic2.AddXY(d,max(minv,(YearInfo[i,j].nautic2+localoffset)-d-1));
            end
        end
        else begin
          // all twilight
          ChartYearSeriesAstro1.AddXY(d,(YearInfo[i,j].astro1+localoffset)-d);
          ChartYearSeriesNautic1.AddXY(d,(YearInfo[i,j].nautic1+localoffset)-d);
          ChartYearSeriesNautic2.AddXY(d,max(minv,(YearInfo[i,j].nautic2+localoffset)-d-1));
          ChartYearSeriesAstro2.AddXY(d,max(minv,(YearInfo[i,j].astro2+localoffset)-d-1));
        end;
      end;
      // moon luminosity in 0-255 range
      k:=StrToIntDef(GridYear.Cells[j,i],k);
      if k<15 then l:=k*17
              else l:=(30-k)*17;
      // check if moon rise and set
      if (YearInfo[i,j].moonrise=0)and(YearInfo[i,j].moonset=0) then begin
        if Sign(YearInfo[i,j].moondec)=sign(ObsLatitude) then begin
          // always up
          moonrise:=minv;
          moonset:=maxv;
          ChartYearBoxAndWhiskerSeries1.AddXY(d,moonrise,moonrise,moonrise,moonset,moonset,'',RGBToColor(l,l,0));
          ChartYearBoxAndWhiskerSeries2.AddXY(d,0,0,0,0,0);
        end
        else begin
          // no rise
          ChartYearBoxAndWhiskerSeries1.AddXY(d,0,0,0,0,0);
          ChartYearBoxAndWhiskerSeries2.AddXY(d,0,0,0,0,0);
        end;
      end
      else begin
        // moon rise set local time
        if YearInfo[i,j].moonrise<>0 then
          moonrise:=YearInfo[i,j].moonrise-d+localoffset
        else
          moonrise:=GetYearInfo(i,j,-1).moonrise-d+localoffset;
        if YearInfo[i,j].moonset<>0 then
          moonset:=YearInfo[i,j].moonset-d+localoffset
        else
          moonset:=GetYearInfo(i,j,1).moonset-d+localoffset;
        // in -12 +12 range
        if moonrise>0.5 then moonrise:=moonrise-1;
        if moonset>0.5 then moonset:=moonset-1;
        if (moonrise<moonset) then begin
          // single bar
          ChartYearBoxAndWhiskerSeries1.AddXY(d,moonrise,moonrise,moonrise,moonset,moonset,'',RGBToColor(l,l,0));
          ChartYearBoxAndWhiskerSeries2.AddXY(d,0,0,0,0,0);
        end
        else begin
          // split in two bar
          ChartYearBoxAndWhiskerSeries1.AddXY(d,moonrise,moonrise,moonrise,maxv,maxv,'',RGBToColor(l,l,0));
          ChartYearBoxAndWhiskerSeries2.AddXY(d,minv,minv,minv,moonset,moonset,'',RGBToColor(l,l,0));
        end;
      end;
    end;
  end;
end;

procedure Tf_calclun.ChartYearAxisList1MarkToText(var AText: String; AMark: Double);
begin
 amark:=(amark-localoffset)*24;
 if amark<0 then
   atext:=FormatFloat(f0,24+AMark)
 else
   atext:=FormatFloat(f0,AMark)
end;

procedure Tf_calclun.OpenChart(Sender: TObject);
begin
  Application.QueueAsyncCall(@OpenChartAsync,PtrInt(Sender));
end;

procedure Tf_calclun.OpenChartAsync(data: PtrInt);
var f:TForm;
    p:TPanel;
    b:TButton;
    pt:Tpoint;
    Sender: TObject;
begin
 Sender:=TObject(data);
 if TChart(Sender).Parent is TForm then
   TForm(TChart(Sender).Parent).close
 else begin
   f:=TForm.Create(self);
   p:=TPanel.Create(f);
   b:=TButton.Create(f);
   f.OnClose:=@CloseChart;
   p.Parent:=f;
   p.Caption:='';
   p.Height:=b.Height+8;
   p.Align:=alBottom;
   b.Caption:=rsClose;
   b.OnClick:=@CloseChartClick;
   b.Parent:=p;
   b.AnchorSideTop.Control:=p;
   b.AnchorSideTop.Side:=asrCenter;
   b.AnchorSideLeft.Control:=p;
   b.AnchorSideLeft.Side:=asrCenter;
   f.Width:=TChart(Sender).Width+8;
   f.Height:=TChart(Sender).Height+p.Height+8;
   pt.X:=TChart(Sender).Left;
   pt.Y:=TChart(Sender).Top;
   pt:=TChart(Sender).Parent.ClientToScreen(pt);
   FormPos(f,pt.X,pt.Y);
   TChart(Sender).Parent:=f;
   TChart(Sender).Align:=alClient;
   f.Show;
 end;
end;

procedure Tf_calclun.CloseChartClick(Sender: TObject);
begin
 TForm(TPanel(TButton(Sender).Parent).Parent).close;
end;

procedure Tf_calclun.CloseChart(Sender: TObject; var CloseAction: TCloseAction);
var i: integer;
begin
  for i:=0 to TForm(Sender).ControlCount-1 do begin
     if TForm(Sender).Controls[i] is TChart then
       with TForm(Sender).Controls[i] as TChart do begin
         Align:=alClient;
         if Name='ChartYear' then begin
            Parent:=PanelChartYear;
         end
         else if Name='Chart1' then begin
            Parent:=PanelGraph1;
         end
         else if Name='Chart2' then begin
            Parent:=PanelGraph2;
         end
         else if Name='Chart3' then begin
            Parent:=PanelGraph3;
         end
         else if Name='ChartAltAz' then begin
            Parent:=PanelChartAltAz;
         end
       end;
  end;
  CloseAction:=caFree;
end;

procedure Tf_calclun.ChartYearMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var pointi: TPoint;
    pointg: TDoublePoint;
begin
 if (ChartYear.ScalingValid)and(x>1)and(x<(ChartYear.Width-1))and(y>1)and(y<(ChartYear.Height-1)) then begin
   // mouse position
   pointi.x:=X;
   pointi.y:=Y;
   pointg:=ChartYear.ImageToGraph(pointi);
   LabelChartYear.Caption:=rsDate+': '+FormatDateTime(datestd, round(pointg.y)+pointg.x-localoffset);
 end;
end;

procedure Tf_calclun.GridYearDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var img: TBitmap;
    i: integer;
begin
with sender as TStringGrid do begin
  if (aRow>0)and(aCol>0) then begin
      Canvas.Brush.Style:=bsSolid;
      Canvas.Brush.Color:=clBlack;
      Canvas.FillRect(aRect);
      i:=StrToIntDef(Cells[aCol,aRow],-1);
      if i>=0 then begin
        img := TBitmap.Create;
        ImageListPhase.GetBitmap(i, img);
        Canvas.Draw(aRect.Left,aRect.Top+2,img);
        img.Free;
      end;
  end
  else begin
    Canvas.TextOut(aRect.Left+4,aRect.Top+2,Cells[aCol,aRow]);
  end;
end;
end;

procedure Tf_calclun.GridYearHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
begin
  if not IsColumn then begin
    SpinEditMonth.Value:=index;
    PageControl1.ActivePage:=TabSheetMonth;
    PageControl1Change(nil);
  end;
end;

procedure Tf_calclun.GridYearSelection(Sender: TObject; aCol, aRow: Integer);
begin
  SpinEditMonth.Value:=arow;
  SpinEditDay.Value:=acol;
  PageControl1.ActivePage:=TabSheetDay;
  PageControl1Change(nil);
end;

procedure Tf_calclun.MenuSetupClick(Sender: TObject);
var inif: TMemIniFile;
begin
  f_config.Setlang;
  f_config.obstz:=ObsTZ;
  f_config.newlang:=language;
  f_config.SetObsCountry(ObsCountry);
  f_config.Edit1.Value := abs(rad2deg*ObsLatitude);
  f_config.Edit2.Value := abs(rad2deg*ObsLongitude);
  f_config.Edit3.Value := ObsAltitude*1000;
  if Obslatitude >= 0 then
    f_config.ComboBox1.ItemIndex := 0
  else
    f_config.ComboBox1.ItemIndex := 1;
  if Obslongitude >= 0 then
    f_config.ComboBox2.ItemIndex := 0
  else
    f_config.ComboBox2.ItemIndex := 1;

  if f_config.ShowModal = mrOK then begin
    ObsLatitude := deg2rad*f_config.Edit1.Value;
    if f_config.ComboBox1.ItemIndex = 1 then
      ObsLatitude := -ObsLatitude;
    ObsLongitude := deg2rad*f_config.Edit2.Value;
    ObsAltitude := f_config.Edit3.Value/1000;
    if f_config.ComboBox2.ItemIndex = 0 then
      Obslongitude := -Obslongitude;
    ObsCountry:=f_config.ObsCountry;
    ObsTZ := f_config.obstz;

    inif := Tmeminifile.Create(ConfigFile);
    inif.WriteFloat('default', 'Obslatitude', rad2deg * Obslatitude);
    inif.WriteFloat('default', 'Obslongitude', rad2deg * Obslongitude);
    inif.WriteFloat('default', 'Obsaltitude', ObsAltitude * 1000);
    inif.WriteString('default', 'ObsCountry', ObsCountry);
    inif.WriteString('default', 'ObsTZ', ObsTZ);

    if f_config.newlang <> language then
    begin
      language:=u_translation.translate(f_config.newlang,'en');
      inif.WriteString('default', 'lang_po_file', language);
      SetLang;
    end;

    inif.UpdateFile;
    inif.Free;

    SetObservatory;
    ForceDateChange:=true;
    DateChange(nil);
  end;
end;

procedure Tf_calclun.PageControl1Change(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
    0: begin
         label10.Caption:=rsByYear;
       end;
    1: begin
         label10.Caption:=rsByMonth;
       end;
    2: begin
         label10.Caption:=rsByDay;
       end;
    3: begin
         label10.Caption:=rsByMonth;
       end;
  end;
end;

procedure Tf_calclun.SetObservatory;
var inif: TMemIniFile;
begin
  inif := Tmeminifile.Create(ConfigFile);
  Obslatitude  := inif.ReadFloat('default', 'Obslatitude', Obslatitude) * deg2rad;
  Obslongitude := -inif.ReadFloat('default', 'Obslongitude', Obslongitude) * deg2rad;
  ObsAltitude := inif.ReadFloat('default', 'Obsaltitude', ObsAltitude) / 1000;
  ObsCountry  := inif.ReadString('default', 'ObsCountry', ObsCountry);
  ObsTZ := inif.ReadString('default', 'ObsTZ', ObsTZ);
  inif.Free;
  tz.TimeZoneFile := ZoneDir + StringReplace(ObsTZ, '/', PathDelim, [rfReplaceAll]);
  labeltz.Caption:=tz.ZoneName;
  reset_c;
  if not ObservatoryPosition(ObsLongitude,ObsLatitude,ObsAltitude,obspos) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
end;

function Tf_calclun.GetTimeZone(sdt: Tdatetime): double;
begin
    tz.Date := sdt;
    Result  := tz.SecondsOffset / 3600;
end;

function Tf_calclun.GetTimeZoneD(sdt: Tdatetime): double;
begin
    tz.Date := sdt;
    Result  := tz.SecondsOffset / 3600 / 24;
end;

procedure Tf_calclun.ComputeMonth;
var dt, nm, fq, fm, lq, dtr, dts: double;
    i, nday: integer;
    Year, Month, rsy,rsm,rsd: Word;
    x,y,z,r,ra,de,pa,llon,llat,slon,slat,colongitude,phase,lunation: SpiceDouble;
    obsref,fixref: ConstSpiceChar;
    data: TMoonMonthData;
begin
  for i:=0 to GridMonth.RowCount-1 do begin
    if GridMonth.Objects[0,i]<>nil then GridMonth.Objects[0,i].Free;
  end;
  year:=SpinEditYear.Value;
  month:=SpinEditMonth.Value;
  LabelMonth.Caption:=IntToStr(year)+' '+GridYear.Cells[0,month];
  nday:=MonthDays[IsLeapYear(year)][month];
  GridMonth.RowCount:=1;
  GridMonth.RowCount:=nday+1;
  for i:=0 to GridMonth.RowCount-1 do begin
    GridMonth.Objects[0,i]:=TMoonMonthData.Create;
  end;
  for i:=1 to nday do begin
     data:=TMoonMonthData(GridMonth.Objects[0,i]);
     GridMonth.Cells[mcolDay,i]:=inttostr(i);
     dt:=EncodeDate(year,month,i);
     et:=DateTime2ET(dt);
     data.day:=i;
     data.date:=dt;

     // J2000 position
     if not MoonTopocentric(et,false,obspos,obsref,x,y,z,r,ra,de) then begin
       StatusLabel.Caption:=SpiceLastError;
       exit;
     end;
     data.ra2000:=ra;
     data.de2000:=de;
     data.dist:=r;
     MoonPA(et,ra,de,pa);
     data.pa:=pa;
     pa:=pa*rad2deg;
     ra:=ra*rad2deg/15;
     de:=de*rad2deg;
     GridMonth.Cells[mcolRa2000,i]:=ARpToStr(ra);
     GridMonth.Cells[mcolDe2000,i]:=DEpToStr(de);
     GridMonth.Cells[mcolDist,i]:=FormatFloat(f3,r);
     diam:=60*rad2deg*arctan(2*Rmoon/r);
     data.diam:=diam;
     GridMonth.Cells[mcolDiam,i]:=FormatFloat(f2,diam);
     GridMonth.Cells[mcolPa,i]:=FormatFloat(f2,pa);

     // Apparent position
     if not MoonTopocentric(et,true,obspos,obsref,x,y,z,r,ra,de) then begin
       StatusLabel.Caption:=SpiceLastError;
       exit;
     end;
     data.ra:=ra;
     data.de:=de;
     ra:=ra*rad2deg/15;
     de:=de*rad2deg;
     GridMonth.Cells[mcolRa,i]:=ARpToStr(ra);
     GridMonth.Cells[mcolDe,i]:=DEpToStr(de);

     // Libration
     if not MoonSubObserverPoint(et,obspos,x,y,z,r,llon,llat) then begin
       StatusLabel.Caption:=SpiceLastError;
       exit;
     end;
     data.librlong:=llon;
     data.librlat:=llat;
     GridMonth.Cells[mcolLibrLon,i]:=DEmToStr(llon*rad2deg);
     GridMonth.Cells[mcolLibrLat,i]:=DEmToStr(llat*rad2deg);

     // Colongitude
     if not MoonSubSolarPoint(et,fixref,x,y,z,r,slon,slat,colongitude) then begin
        StatusLabel.Caption:=SpiceLastError;
        exit;
     end;
     data.colong:=colongitude;
     data.subsollat:=slat;
     GridMonth.Cells[mcolColong,i]:=FormatFloat(f4,colongitude*rad2deg);
     GridMonth.Cells[mcolSubSolLat,i]:=FormatFloat(f4,slat*rad2deg);

     // Lunation
     if not MoonPhases(dt, nm, fq, fm, lq,lunation) then begin
       StatusLabel.Caption:=SpiceLastError;
       exit;
     end;
     data.lunation:=lunation;
     GridMonth.Cells[mcolLunation,i]:=FormatFloat(f2,lunation);

     // Illumination
     if not MoonPhase(et,phase) then begin
       StatusLabel.Caption:=SpiceLastError;
       exit;
     end;
     if (colongitude>pid2) and (colongitude<(3*pid2))then phase:=pi2-phase;
     data.phase:=phase;
     data.illum:=(1+cos(phase))/2;
     GridMonth.Cells[mcolPhase,i]:=FormatFloat(f3,rad2deg*phase);
     GridMonth.Cells[mcolIllum,i]:=FormatFloat(f1,100*data.illum);

     // Rise and set
     dtr:=YearInfo[month,i].moonrise;
     dts:=YearInfo[month,i].moonset;
     if (dtr=0)and(dts=0) then begin  // no rise/set
       if Sign(YearInfo[month,i].moondec)=sign(ObsLatitude) then
         GridMonth.Cells[mcolRise, i]:=rsAlwaysUp
       else
         GridMonth.Cells[mcolRise, i]:=rsNeverRise;
     end else begin
       if dtr>0 then begin
         dtr:=dtr-YearInfo[month,i].dst;
         DecodeDate(dtr,rsy,rsm,rsd);
         if (rsy=year)and(rsm=month) then begin
           TMoonMonthData(GridMonth.Objects[0,rsd]).trise:=dtr;
           GridMonth.Cells[mcolRise,rsd]:=TimToStr(frac(dtr)*24);
         end;
       end;
       if dts>0 then begin
         dts:=dts-YearInfo[month,i].dst;
         DecodeDate(dts,rsy,rsm,rsd);
         if (rsy=year)and(rsm=month) then begin
           TMoonMonthData(GridMonth.Objects[0,rsd]).tset:=dts;
           GridMonth.Cells[mcolSet,rsd]:=TimToStr(frac(dts)*24);
         end;
       end;
     end;
  end;

  // Phases for the month
  dt:=EncodeDate(year,month,1);
  if not MoonPhases(dt, nm, fq, fm, lq,lunation) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  LabelPhase2.Caption:=FormatDateTime(datestd,nm+GetTimeZoneD(nm))+crlf+
                      FormatDateTime(datestd,fq+GetTimeZoneD(fq))+crlf+
                      FormatDateTime(datestd,fm+GetTimeZoneD(fm))+crlf+
                      FormatDateTime(datestd,lq+GetTimeZoneD(lq));

  // draw selected graph
  PlotMonthGraph(FCurrentMonthGraph);

end;

procedure Tf_calclun.ComputeDay;
var dt: double;
    i: integer;
    Year, Month, Day: Word;
    x,y,z,r,ra,de,pa,llon,llat,slon,slat,colongitude: SpiceDouble;
    obsref,fixref: ConstSpiceChar;
    img: TBitmap;
begin
  year:=SpinEditYear.Value;
  month:=SpinEditMonth.Value;
  day:=SpinEditDay.Value;
  LabelDay.Caption:=IntToStr(year)+' '+GridYear.Cells[0,month]+' '+IntToStr(day);
  GridDay.RowCount:=1;
  GridDay.RowCount:=25;
  reset_c;
  for i:=1 to 24 do begin
     GridDay.Cells[dcolHour,i]:=inttostr(i-1)+':00';
     dt:=EncodeDate(year,month,day)+(i-1)/24-TimeZoneD;
     et:=DateTime2ET(dt);

     // J2000 position
     if not MoonTopocentric(et,false,obspos,obsref,x,y,z,r,ra,de) then begin
       StatusLabel.Caption:=SpiceLastError;
       exit;
     end;
     MoonPA(et,ra,de,pa);
     pa:=pa*rad2deg;
     ra:=ra*rad2deg/15;
     de:=de*rad2deg;
     GridDay.Cells[dcolRa2000,i]:=ARpToStr(ra);
     GridDay.Cells[dcolDe2000,i]:=DEpToStr(de);
     GridDay.Cells[dcolDist,i]:=FormatFloat(f3,r);
     diam:=60*rad2deg*arctan(2*Rmoon/r);
     GridDay.Cells[dcolDiam,i]:=FormatFloat(f2,diam);
     GridDay.Cells[dcolPa,i]:=FormatFloat(f2,pa);

      // Apparent position
     if not MoonTopocentric(et,true,obspos,obsref,x,y,z,r,ra,de) then begin
       StatusLabel.Caption:=SpiceLastError;
       exit;
     end;
     ra:=ra*rad2deg/15;
     de:=de*rad2deg;
     GridDay.Cells[dcolRa,i]:=ARpToStr(ra);
     GridDay.Cells[dcolDe,i]:=DEpToStr(de);

     // Libration
     if not MoonSubObserverPoint(et,obspos,x,y,z,r,llon,llat) then begin
       StatusLabel.Caption:=SpiceLastError;
       exit;
     end;
     GridDay.Cells[dcolLibrLon,i]:=DEmToStr(llon*rad2deg);
     GridDay.Cells[dcolLibrLat,i]:=DEmToStr(llat*rad2deg);

     // Colongitude
     if not MoonSubSolarPoint(et,fixref,x,y,z,r,slon,slat,colongitude) then begin
        StatusLabel.Caption:=SpiceLastError;
        exit;
     end;
     GridDay.Cells[dcolColong,i]:=FormatFloat(f4,colongitude*rad2deg);
     GridDay.Cells[dcolSubSolLat,i]:=FormatFloat(f4,slat*rad2deg);
  end;

  // rise / set label
  LabelRise2.Caption:=GridMonth.Cells[mcolRise,day]+crlf+GridMonth.Cells[mcolSet,day];

  // Phase image
  img := TBitmap.Create;
  i:=round(TMoonMonthData(GridMonth.Objects[0,day]).lunation);
  ImageListPhase.GetBitmap(i, img);
  DayPhase.Canvas.StretchDraw(DayPhase.ClientRect,img);
  img.Free;

  PlotDayGraph;
  DayTimeChange(nil);

end;

procedure Tf_calclun.DayTimeChange(Sender: TObject);
var dt: double;
    Year, Month, Day: Word;
    x,y,z,r,r1,ra1,de1,ra,de,pa,llon,llat,slon,slat,colongitude,az,el: SpiceDouble;
    obsref,fixref: ConstSpiceChar;
begin
  year:=SpinEditYear.Value;
  month:=SpinEditMonth.Value;
  day:=SpinEditDay.Value;
  reset_c;
  dt:=EncodeDate(year,month,day);
  dt:=dt+(DayHour.Value+DayMin.Value/60+DaySec.Value/3600)/24-TimeZoneD;
  et:=DateTime2ET(dt);

  // J2000 position
  if not MoonTopocentric(et,false,obspos,obsref,x,y,z,r,ra,de) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  MoonPA(et,ra,de,pa);
  pa:=pa*rad2deg;
  ra:=ra*rad2deg/15;
  de:=de*rad2deg;
  diam:=60*rad2deg*arctan(2*Rmoon/r);
  // Apparent position
  if not MoonTopocentric(et,true,obspos,obsref,x,y,z,r1,ra1,de1) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  ra1:=ra1*rad2deg/15;
  de1:=de1*rad2deg;
  // Libration
  if not MoonSubObserverPoint(et,obspos,x,y,z,r1,llon,llat) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  // Colongitude
  if not MoonSubSolarPoint(et,fixref,x,y,z,r1,slon,slat,colongitude) then begin
     StatusLabel.Caption:=SpiceLastError;
     exit;
  end;
  // Alt/Az
  if not MoonAltAz(et,ObsLongitude,ObsLatitude,obspos,obsref,az,el) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;

  MemoDay.Clear;
  MemoDay.Lines.Add(rsRA2000+':'+tab+ARpToStr(ra));
  MemoDay.Lines.Add(rsDE2000+':'+tab+DEpToStr(de));
  MemoDay.Lines.Add(rsRAApparent+':'+tab+ARpToStr(ra1));
  MemoDay.Lines.Add(rsDEApparent+':'+tab+DEpToStr(de1));
  MemoDay.Lines.Add(rsDistance+':'+tab+FormatFloat(f3, r));
  MemoDay.Lines.Add(rsDiameter+':'+tab+FormatFloat(f2, diam));
  MemoDay.Lines.Add(rsColongitude+':'+tab+FormatFloat(f4, colongitude*rad2deg));
  MemoDay.Lines.Add(rsSubSolarLati+':'+tab+FormatFloat(f4, slat*rad2deg));
  MemoDay.Lines.Add(rsLibrationLon+':'+tab+DEmToStr(llon*rad2deg));
  MemoDay.Lines.Add(rsLibrationLat+':'+tab+DEmToStr(llat*rad2deg));
  MemoDay.Lines.Add(rsPA+':'+tab+FormatFloat(f2, pa));
  MemoDay.Lines.Add(rsAzimut+':'+tab+DEmToStr(rmod(az*rad2deg+360, 360)));
  MemoDay.Lines.Add(rsElevation+':'+tab+DEmToStr(el * rad2deg));
end;

procedure Tf_calclun.OpenAtlun(dt,nam:string);
var p: string;
begin
  p:='-nc -d '+dt;
  if nam<>'' then p:=p+' -n "'+nam+'"';
  chdir(appdir);
  Execnowait(maplun+' '+p);
  StartVMA:=true;
end;

procedure Tf_calclun.GridDayHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
var buf:string;
    dt:double;
    Year, Month, Day: Word;
    h,p: integer;
begin
  if IsColumn then exit;
  if Index>0 then begin
  buf:=GridDay.Cells[0,index];
  p:=pos(':',buf);
  if (p>0) then begin
    buf:=copy(buf,1,p-1);
    h:=StrToIntdef(buf,0);
    year:=SpinEditYear.Value;
    month:=SpinEditMonth.Value;
    day:=SpinEditDay.Value;
    dt:=EncodeDate(year,month,day)+h/24;
    buf:=FormatDateTime(dateiso,dt);
    OpenAtlun(buf,'');
  end;
  end;
end;

procedure Tf_calclun.GridColongitudeDblClick(Sender: TObject);
var buf:string;
    c,r: integer;
begin
  buf:=GetSelectedCell(GridColongitude,c,r);
  if (r>0)and(c<=1)and(buf<>'') then begin
    buf:=StringReplace(buf,' ','T',[]);
    OpenAtlun(buf,'');
  end;
end;

procedure Tf_calclun.GridLibrationDblClick(Sender: TObject);
var buf,f:string;
    c,r: integer;
begin
  buf:=GetSelectedCell(GridLibration,c,r);
  if (r>0)and(c<=1)and(buf<>'') then begin
    buf:=StringReplace(buf,' ','T',[]);
    f:=trim(ComboBoxFormation.Text);
    OpenAtlun(buf,f);
  end;
end;

procedure Tf_calclun.GridTerminator1DblClick(Sender: TObject);
var buf,f:string;
    c,r: integer;
begin
  buf:=GetSelectedCell(GridTerminator1,c,r);
  if (r>0)and(c<=1)and(buf<>'') then begin
    buf:=StringReplace(buf,' ','T',[]);
    f:=trim(ComboBoxFormation.Text);
    OpenAtlun(buf,f);
  end;
end;

procedure Tf_calclun.GridTerminator2DblClick(Sender: TObject);
var buf,f:string;
    c,r: integer;
begin
  buf:=GetSelectedCell(GridTerminator2,c,r);
  if (r>0)and(c<=1)and(buf<>'') then begin
    buf:=StringReplace(buf,' ','T',[]);
    f:=trim(ComboBoxFormation.Text);
    OpenAtlun(buf,f);
  end;
end;

procedure Tf_calclun.PlotDayGraph;
var dt,startt,endt,dt0,dstoff: double;
    Year, Month, Day: Word;
    az,el: SpiceDouble;
    obsref: ConstSpiceChar;
    moongraph: boolean;
begin
  year:=SpinEditYear.Value;
  month:=SpinEditMonth.Value;
  day:=SpinEditDay.Value;
  dt0:=EncodeDate(year,month,day);
  dstoff:=TimeZoneD-stdtz;

  // Alt-az graph
  ChartAltAzLineSeries1.Clear;
  ChartAltAzLineSeries2.Clear;
  ChartAltazSunArea.Clear;
  ChartAltAzCivilArea.Clear;
  ChartAltAzNauticArea.Clear;
  ChartAltAzAstroArea.Clear;
  moongraph:=true;
  ChartAltAz.Extent.XMin:=dt0;
  ChartAltAz.Extent.XMax:=dt0+1;
  startt:=dt0-1;
  endt:=dt0+2;
  if moongraph then begin
    dt:=startt-1/48;
    repeat
      dt:=dt+1/48;
      if dt>=endt then dt:=endt;
      et:=DateTime2ET(dt-stdtz);
      if not MoonAltAz(et,ObsLongitude,ObsLatitude,obspos,obsref,az,el) then begin
        StatusLabel.Caption:=SpiceLastError;
        exit;
      end;
      el := el * rad2deg;
      az := rmod(az*rad2deg+360,360);
      ChartAltAzLineSeries1.AddXY(dt+dstoff,el)
    until dt=endt;
  end;
  if (YearInfo[month,day].sunrise>0)and(YearInfo[month,day].sunset>0) then begin
    ChartAltazSunArea.AddXY((GetYearInfo(month,day,-1).sunrise+dstoff),900);
    ChartAltazSunArea.AddXY((GetYearInfo(month,day,-1).sunset+dstoff),900);
    ChartAltazSunArea.AddXY((GetYearInfo(month,day,-1).sunset+dstoff),-900);
    ChartAltazSunArea.AddXY((YearInfo[month,day].sunrise+dstoff),-900);
    ChartAltazSunArea.AddXY((YearInfo[month,day].sunrise+dstoff),900);
    ChartAltazSunArea.AddXY((YearInfo[month,day].sunset+dstoff),900);
    ChartAltazSunArea.AddXY((YearInfo[month,day].sunset+dstoff),-900);
    ChartAltazSunArea.AddXY((GetYearInfo(month,day,1).sunrise+dstoff),-900);
    ChartAltazSunArea.AddXY((GetYearInfo(month,day,1).sunrise+dstoff),900);
    ChartAltazSunArea.AddXY((GetYearInfo(month,day,1).sunset+dstoff),900);
    ChartAltazSunArea.AddXY((GetYearInfo(month,day,1).sunset+dstoff),-900);
  end
  else begin
    if sign(YearInfo[month,day].sundec)=sign(ObsLatitude) then begin
      ChartAltazSunArea.AddXY(startt,900);
      ChartAltazSunArea.AddXY(endt,900);
    end
    else begin
      ChartAltazSunArea.AddXY(startt,-900);
      ChartAltazSunArea.AddXY(endt,-900);
    end;
  end;
  if (YearInfo[month,day].Civil1>0)and(YearInfo[month,day].Civil2>0) then begin
    ChartAltAzCivilArea.AddXY((GetYearInfo(month,day,-1).Civil1+dstoff),900);
    ChartAltAzCivilArea.AddXY((GetYearInfo(month,day,-1).Civil2+dstoff),900);
    ChartAltAzCivilArea.AddXY((GetYearInfo(month,day,-1).Civil2+dstoff),-900);
    ChartAltAzCivilArea.AddXY((YearInfo[month,day].Civil1+dstoff),-900);
    ChartAltAzCivilArea.AddXY((YearInfo[month,day].Civil1+dstoff),900);
    ChartAltAzCivilArea.AddXY((YearInfo[month,day].Civil2+dstoff),900);
    ChartAltAzCivilArea.AddXY((YearInfo[month,day].Civil2+dstoff),-900);
    ChartAltAzCivilArea.AddXY((GetYearInfo(month,day,1).Civil1+dstoff),-900);
    ChartAltAzCivilArea.AddXY((GetYearInfo(month,day,1).Civil1+dstoff),900);
    ChartAltAzCivilArea.AddXY((GetYearInfo(month,day,1).Civil2+dstoff),900);
    ChartAltAzCivilArea.AddXY((GetYearInfo(month,day,1).Civil2+dstoff),-900);
  end
  else begin
    if sign(YearInfo[month,day].sundec)=sign(ObsLatitude) then begin
      ChartAltAzCivilArea.AddXY(startt,900);
      ChartAltAzCivilArea.AddXY(endt,900);
    end
    else begin
      ChartAltAzCivilArea.AddXY(startt,-900);
      ChartAltAzCivilArea.AddXY(endt,-900);
    end;
  end;
  if (YearInfo[month,day].nautic1>0)and(YearInfo[month,day].nautic2>0) then begin
    ChartAltAzNauticArea.AddXY((GetYearInfo(month,day,-1).nautic1+dstoff),900);
    ChartAltAzNauticArea.AddXY((GetYearInfo(month,day,-1).nautic2+dstoff),900);
    ChartAltAzNauticArea.AddXY((GetYearInfo(month,day,-1).nautic2+dstoff),-900);
    ChartAltAzNauticArea.AddXY((YearInfo[month,day].nautic1+dstoff),-900);
    ChartAltAzNauticArea.AddXY((YearInfo[month,day].nautic1+dstoff),900);
    ChartAltAzNauticArea.AddXY((YearInfo[month,day].nautic2+dstoff),900);
    ChartAltAzNauticArea.AddXY((YearInfo[month,day].nautic2+dstoff),-900);
    ChartAltAzNauticArea.AddXY((GetYearInfo(month,day,1).nautic1+dstoff),-900);
    ChartAltAzNauticArea.AddXY((GetYearInfo(month,day,1).nautic1+dstoff),900);
    ChartAltAzNauticArea.AddXY((GetYearInfo(month,day,1).nautic2+dstoff),900);
    ChartAltAzNauticArea.AddXY((GetYearInfo(month,day,1).nautic2+dstoff),-900);
  end
  else begin
    if sign(YearInfo[month,day].sundec)=sign(ObsLatitude) then begin
      ChartAltAzNauticArea.AddXY(startt,900);
      ChartAltAzNauticArea.AddXY(endt,900);
    end
    else begin
      ChartAltAzNauticArea.AddXY(startt,-900);
      ChartAltAzNauticArea.AddXY(endt,-900);
    end;
  end;
  if (YearInfo[month,day].Astro1>0)and(YearInfo[month,day].Astro2>0) then begin
    ChartAltAzAstroArea.AddXY((GetYearInfo(month,day,-1).Astro1+dstoff),900);
    ChartAltAzAstroArea.AddXY((GetYearInfo(month,day,-1).Astro2+dstoff),900);
    ChartAltAzAstroArea.AddXY((GetYearInfo(month,day,-1).Astro2+dstoff),-900);
    ChartAltAzAstroArea.AddXY((YearInfo[month,day].Astro1+dstoff),-900);
    ChartAltAzAstroArea.AddXY((YearInfo[month,day].Astro1+dstoff),900);
    ChartAltAzAstroArea.AddXY((YearInfo[month,day].Astro2+dstoff),900);
    ChartAltAzAstroArea.AddXY((YearInfo[month,day].Astro2+dstoff),-900);
    ChartAltAzAstroArea.AddXY((GetYearInfo(month,day,1).Astro1+dstoff),-900);
    ChartAltAzAstroArea.AddXY((GetYearInfo(month,day,1).Astro1+dstoff),900);
    ChartAltAzAstroArea.AddXY((GetYearInfo(month,day,1).Astro2+dstoff),900);
    ChartAltAzAstroArea.AddXY((GetYearInfo(month,day,1).Astro2+dstoff),-900);
  end
  else begin
    if sign(YearInfo[month,day].sundec)=sign(ObsLatitude) then begin
      ChartAltAzAstroArea.AddXY(startt,900);
      ChartAltAzAstroArea.AddXY(endt,900);
    end
    else begin
      ChartAltAzAstroArea.AddXY(startt,-900);
      ChartAltAzAstroArea.AddXY(endt,-900);
    end;
  end;
end;

function Tf_calclun.GetYearInfo(m,d,doffset:integer):TYearInfo;
var month,rmonth,day: integer;
begin
  day:=d+doffset;
  month:=m;
  if day<1 then begin
    month:=month-1;
    if month>=1 then
      rmonth:=month
    else
      rmonth:=12;
    day:=MonthDays[IsLeapYear(SpinEditYear.Value)][rmonth]+day;
  end
  else
  if day>MonthDays[IsLeapYear(SpinEditYear.Value)][month] then begin
    day:=day-MonthDays[IsLeapYear(SpinEditYear.Value)][month];
    month:=month+1;
  end;
  result:=YearInfo[month,day];
end;

procedure Tf_calclun.ChartAltAzMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var pointi: TPoint;
    pointg: TDoublePoint;
begin
  if (ChartAltAz.ScalingValid)and(x>5)and(x<(ChartAltAz.Width-5))and(y>5)and(y<(ChartAltAz.Height-5)) then begin
  // mouse position
  pointi.x:=X;
  pointi.y:=Y;
  pointg:=ChartAltAz.ImageToGraph(pointi);
  // show position
  LabelAltAz.Caption:=FormatDateTime('mm/dd hh:nn', pointg.x)+':  '+rsAlt+':'+FormatFloat(f1, pointg.y);
  end else begin
    LabelAltAz.Caption:='';
  end;
end;

procedure Tf_calclun.GridMonthHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
begin
  if IsColumn then begin
    if index>0 then PlotMonthGraph(index);
  end
  else begin
    if index>0 then begin
      SpinEditDay.Value:=index;
      PageControl1.ActivePage:=TabSheetDay;
      PageControl1Change(nil);
    end;
  end;
end;

procedure Tf_calclun.SelectGraph(Sender: TObject);
begin
  PlotMonthGraph(TMenuItem(sender).tag);
end;

procedure Tf_calclun.PlotMonthGraph(col:integer);
var i: integer;
    prev: double;
begin
  Chart1LineSeries1.Clear;
  Chart2LineSeries1.Clear;
  Chart1.Title.Text.Clear;
  Chart2.Title.Text.Clear;
  PanelGraph2.Visible:=false;
  PanelGraph3.Visible:=false;
  FCurrentMonthGraph:=col;
  case col of
    mcolDay      :  ;
    mcolRa2000   : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart1.Extent.YMin:=0;
                   chart1.Extent.YMax:=24;
                   chart1.Extent.UseYMin:=true;
                   chart1.Extent.UseYMax:=true;
                   prev:=TMoonMonthData(GridMonth.Objects[0,1]).ra2000;
                   for i:=1 to GridMonth.RowCount-1 do begin;
                     if abs(TMoonMonthData(GridMonth.Objects[0,i]).ra2000-prev)<pi then
                        Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).ra2000*rad2deg/15)
                     else begin
                        if (TMoonMonthData(GridMonth.Objects[0,i]).ra2000-prev)>0 then
                           Chart1LineSeries1.AddXY(i-1,(prev+pi2)*rad2deg/15,'',chart1.BackColor)
                        else
                           Chart1LineSeries1.AddXY(i-1,(prev-pi2)*rad2deg/15,'',chart1.BackColor);
                        Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).ra2000*rad2deg/15);
                     end;
                     prev:=TMoonMonthData(GridMonth.Objects[0,i]).ra2000;
                   end;
                   end;
    mcolDe2000   : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart1.Extent.YMin:=-90;
                   chart1.Extent.YMax:=90;
                   chart1.Extent.UseYMin:=true;
                   chart1.Extent.UseYMax:=true;
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).de2000*rad2deg);
                   end;
                   end;
    mcolRa       : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart1.Extent.YMin:=0;
                   chart1.Extent.YMax:=24;
                   chart1.Extent.UseYMin:=true;
                   chart1.Extent.UseYMax:=true;
                   prev:=TMoonMonthData(GridMonth.Objects[0,1]).ra;
                   for i:=1 to GridMonth.RowCount-1 do begin;
                     if abs(TMoonMonthData(GridMonth.Objects[0,i]).ra-prev)<pi then
                        Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).ra*rad2deg/15)
                     else begin
                        if (TMoonMonthData(GridMonth.Objects[0,i]).ra-prev)>0 then
                           Chart1LineSeries1.AddXY(i-1,(prev+pi2)*rad2deg/15,'',chart1.BackColor)
                        else
                           Chart1LineSeries1.AddXY(i-1,(prev-pi2)*rad2deg/15,'',chart1.BackColor);
                        Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).ra*rad2deg/15);
                     end;
                     prev:=TMoonMonthData(GridMonth.Objects[0,i]).ra;
                   end;
                   end;
    mcolDe       : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart1.Extent.YMin:=-90;
                   chart1.Extent.YMax:=90;
                   chart1.Extent.UseYMin:=true;
                   chart1.Extent.UseYMax:=true;
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).de*rad2deg);
                   end;
                   end;
    mcolDist     : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart1.Extent.UseYMin:=false;
                   chart1.Extent.UseYMax:=false;
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).dist);
                   end;
                   end;
    mcolDiam     : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart1.Extent.UseYMin:=false;
                   chart1.Extent.UseYMax:=false;
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).diam);
                   end;
                   end;
    mcolPhase    : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart1.Extent.UseYMin:=false;
                   chart1.Extent.UseYMax:=false;
                   prev:=TMoonMonthData(GridMonth.Objects[0,1]).phase;
                   for i:=1 to GridMonth.RowCount-1 do begin;
                     if abs(TMoonMonthData(GridMonth.Objects[0,i]).phase-prev)<pi then
                        Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).phase*rad2deg)
                     else begin
                        if (TMoonMonthData(GridMonth.Objects[0,i]).phase-prev)>0 then
                           Chart1LineSeries1.AddXY(i-1,(prev+pi2)*rad2deg,'',chart1.BackColor)
                        else
                           Chart1LineSeries1.AddXY(i-1,(prev-pi2)*rad2deg,'',chart1.BackColor);
                        Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).phase*rad2deg);
                     end;
                     prev:=TMoonMonthData(GridMonth.Objects[0,i]).phase;
                   end;
                   end;
    mcolLunation : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart1.Extent.UseYMin:=false;
                   chart1.Extent.UseYMax:=false;
                   prev:=TMoonMonthData(GridMonth.Objects[0,1]).lunation;
                   for i:=1 to GridMonth.RowCount-1 do begin;
                     if abs(TMoonMonthData(GridMonth.Objects[0,i]).lunation-prev)<15 then
                        Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).lunation)
                     else begin
                        if (TMoonMonthData(GridMonth.Objects[0,i]).lunation-prev)>0 then
                           Chart1LineSeries1.AddXY(i-1,(prev+29.5),'',chart1.BackColor)
                        else
                           Chart1LineSeries1.AddXY(i-1,(prev-29.5),'',chart1.BackColor);
                        Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).lunation);
                     end;
                     prev:=TMoonMonthData(GridMonth.Objects[0,i]).lunation;
                   end;
                   end;
    mcolIllum    : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart1.Extent.UseYMin:=false;
                   chart1.Extent.UseYMax:=false;
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).illum);
                   end;
                   end;
    mcolColong   : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart1.Extent.UseYMin:=false;
                   chart1.Extent.UseYMax:=false;
                   prev:=TMoonMonthData(GridMonth.Objects[0,1]).colong;
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      if abs(TMoonMonthData(GridMonth.Objects[0,i]).colong-prev)<pi then
                         Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).colong*rad2deg)
                      else begin
                         if (TMoonMonthData(GridMonth.Objects[0,i]).colong-prev)>0 then
                            Chart1LineSeries1.AddXY(i-1,(prev+pi2)*rad2deg,'',chart1.BackColor)
                         else
                            Chart1LineSeries1.AddXY(i-1,(prev-pi2)*rad2deg,'',chart1.BackColor);
                         Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).colong*rad2deg);
                      end;
                      prev:=TMoonMonthData(GridMonth.Objects[0,i]).colong;
                   end;
                   end;
    mcolSubSolLat: begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart1.Extent.UseYMin:=false;
                   chart1.Extent.UseYMax:=false;
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).subsollat*rad2deg);
                   end;
                   end;
    mcolLibrLon  : begin
                   PanelGraph2.Visible:=true;
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart2.Title.Text.Add(rsLibration+' '+LabelMonth.Caption);
                   chart1.Extent.UseYMin:=false;
                   chart1.Extent.UseYMax:=false;
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).librlong*rad2deg);
                      Chart2LineSeries1.AddXY(TMoonMonthData(GridMonth.Objects[0,i]).librlong*rad2deg,TMoonMonthData(GridMonth.Objects[0,i]).librlat*rad2deg,inttostr(i));
                   end;
                   end;
    mcolLibrLat  : begin
                   PanelGraph2.Visible:=true;
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart2.Title.Text.Add(rsLibration+' '+LabelMonth.Caption);
                   chart1.Extent.UseYMin:=false;
                   chart1.Extent.UseYMax:=false;
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).librlat*rad2deg);
                      Chart2LineSeries1.AddXY(TMoonMonthData(GridMonth.Objects[0,i]).librlong*rad2deg,TMoonMonthData(GridMonth.Objects[0,i]).librlat*rad2deg,inttostr(i));
                   end;
                   end;
    mcolPa       : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart1.Extent.UseYMin:=false;
                   chart1.Extent.UseYMax:=false;
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).pa*rad2deg);
                   end;
                   end;
    mcolRise     : begin
                   PanelGraph3.Visible:=true;
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart1.Extent.YMin:=0;
                   chart1.Extent.YMax:=24;
                   chart1.Extent.UseYMin:=true;
                   chart1.Extent.UseYMax:=true;
                   prev:=frac(TMoonMonthData(GridMonth.Objects[0,1]).trise);
                   for i:=1 to GridMonth.RowCount-1 do begin;
                     if abs(frac(TMoonMonthData(GridMonth.Objects[0,i]).trise)-prev)<0.5 then
                        Chart1LineSeries1.Add(frac(TMoonMonthData(GridMonth.Objects[0,i]).trise)*24)
                     else begin
                        if (frac(TMoonMonthData(GridMonth.Objects[0,i]).trise)-prev)>0 then
                           Chart1LineSeries1.AddXY(i-1,(prev+1)*24,'',chart1.BackColor)
                        else
                           Chart1LineSeries1.AddXY(i-1,(prev-1)*24,'',chart1.BackColor);
                        Chart1LineSeries1.Add(frac(TMoonMonthData(GridMonth.Objects[0,i]).trise)*24);
                     end;
                     prev:=frac(TMoonMonthData(GridMonth.Objects[0,i]).trise);
                   end;
                   PlotMonthTwilight;
                   end;
    mcolSet      : begin
                   PanelGraph3.Visible:=true;
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+LabelMonth.Caption);
                   chart1.Extent.YMin:=0;
                   chart1.Extent.YMax:=24;
                   chart1.Extent.UseYMin:=true;
                   chart1.Extent.UseYMax:=true;
                   prev:=frac(TMoonMonthData(GridMonth.Objects[0,1]).tset);
                   for i:=1 to GridMonth.RowCount-1 do begin;
                     if abs(frac(TMoonMonthData(GridMonth.Objects[0,i]).tset)-prev)<0.5 then
                        Chart1LineSeries1.Add(frac(TMoonMonthData(GridMonth.Objects[0,i]).tset)*24)
                     else begin
                        if (frac(TMoonMonthData(GridMonth.Objects[0,i]).tset)-prev)>0 then
                           Chart1LineSeries1.AddXY(i-1,(prev+1)*24,'',chart1.BackColor)
                        else
                           Chart1LineSeries1.AddXY(i-1,(prev-1)*24,'',chart1.BackColor);
                        Chart1LineSeries1.Add(frac(TMoonMonthData(GridMonth.Objects[0,i]).tset)*24);
                     end;
                     prev:=frac(TMoonMonthData(GridMonth.Objects[0,i]).tset);
                   end;
                   PlotMonthTwilight;
                   end;
  end;
end;

procedure Tf_calclun.PlotMonthTwilight;
var i,j,k,l: integer;
    moonrise,moonset: double;
    d:Tdatetime;
    maxv,minv:double;
begin
  Chart3SeriesSunrise.Clear;
  Chart3SeriesSunset.Clear;
  Chart3SeriesAstro1.Clear;
  Chart3SeriesAstro2.Clear;
  Chart3SeriesNautic1.Clear;
  Chart3SeriesNautic2.Clear;
  Chart3BoxAndWhiskerSeries1.Clear;
  Chart3BoxAndWhiskerSeries2.Clear;
  maxv:=0.5;
  minv:=-0.5;
  moonrise:=0;
  moonset:=0;
  k:=StrToIntDef(GridYear.Cells[1,1],1);
  // Use same time zome for all the year to ensure graph continuity
  i:=SpinEditMonth.Value;
    for j:=1 to MonthDays[IsLeapYear(SpinEditYear.Value)][i] do begin
      d:=EncodeDate(SpinEditYear.Value,i,j);
      if ((YearInfo[i,j].sunrise=0)or(YearInfo[i,j].sunset=0))or(YearInfo[i,j].sunrise>YearInfo[i,j].sunset) then begin
       // no sun rise/set
       if sign(YearInfo[i,j].sundec)=sign(ObsLatitude) then begin
          // sun always visible
          Chart3SeriesAstro1.AddXY(d,0);
          Chart3SeriesAstro2.AddXY(d,0);
          Chart3SeriesNautic1.AddXY(d,0);
          Chart3SeriesNautic2.AddXY(d,0);
          Chart3SeriesSunrise.AddXY(d,0);
          Chart3SeriesSunset.AddXY(d,0);
       end
       else begin
         // sun do not rise
         if ((YearInfo[i,j].astro1=0)or(YearInfo[i,j].astro2=0))or(YearInfo[i,j].astro1>YearInfo[i,j].astro2) then begin
           // no astro twilight
           Chart3SeriesAstro1.AddXY(d,0);
           Chart3SeriesAstro2.AddXY(d,0);
         end
         else begin
           // astro twilight
           Chart3SeriesAstro1.AddXY(d,(YearInfo[i,j].astro1+localoffset)-d);
           Chart3SeriesAstro2.AddXY(d,max(minv,(YearInfo[i,j].astro2+localoffset)-d-1));
         end;
          if ((YearInfo[i,j].nautic1=0)or(YearInfo[i,j].nautic2=0))or(YearInfo[i,j].nautic1>YearInfo[i,j].nautic2) then begin
            // no nautic twilight
            Chart3SeriesNautic1.AddXY(d,maxv);
            Chart3SeriesNautic2.AddXY(d,minv);
            Chart3SeriesSunrise.AddXY(d,minv);
            Chart3SeriesSunset.AddXY(d,minv);
          end
          else begin
            // nautic twilight
            Chart3SeriesNautic1.AddXY(d,(YearInfo[i,j].nautic1+localoffset)-d);
            Chart3SeriesNautic2.AddXY(d,max(minv,(YearInfo[i,j].nautic2+localoffset)-d-1));
            Chart3SeriesSunrise.AddXY(d,maxv);
            Chart3SeriesSunset.AddXY(d,minv);
          end;
       end
      end
      else begin
        // sun rise and set
        Chart3SeriesSunrise.AddXY(d,(YearInfo[i,j].sunrise+localoffset)-d);
        Chart3SeriesSunset.AddXY(d,max(minv,(YearInfo[i,j].sunset+localoffset)-d-1));
        if ((YearInfo[i,j].astro1=0)or(YearInfo[i,j].astro2=0))or(YearInfo[i,j].astro1>YearInfo[i,j].astro2) then begin
          // no astro twilight
          Chart3SeriesAstro1.AddXY(d,0);
          Chart3SeriesAstro2.AddXY(d,0);
          if ((YearInfo[i,j].nautic1=0)or(YearInfo[i,j].nautic2=0))or(YearInfo[i,j].nautic1>YearInfo[i,j].nautic2) then begin
            // no nautic twilight
            Chart3SeriesNautic1.AddXY(d,0);
            Chart3SeriesNautic2.AddXY(d,0);
          end
          else begin
            //  nautic twilight
            Chart3SeriesNautic1.AddXY(d,(YearInfo[i,j].nautic1+localoffset)-d);
            Chart3SeriesNautic2.AddXY(d,max(minv,(YearInfo[i,j].nautic2+localoffset)-d-1));
            end
        end
        else begin
          // all twilight
          Chart3SeriesAstro1.AddXY(d,(YearInfo[i,j].astro1+localoffset)-d);
          Chart3SeriesNautic1.AddXY(d,(YearInfo[i,j].nautic1+localoffset)-d);
          Chart3SeriesNautic2.AddXY(d,max(minv,(YearInfo[i,j].nautic2+localoffset)-d-1));
          Chart3SeriesAstro2.AddXY(d,max(minv,(YearInfo[i,j].astro2+localoffset)-d-1));
        end;
      end;
      // moon luminosity in 0-255 range
      k:=StrToIntDef(GridYear.Cells[j,i],k);
      if k<15 then l:=k*17
              else l:=(30-k)*17;
      // check if moon rise and set
      if (YearInfo[i,j].moonrise=0)and(YearInfo[i,j].moonset=0) then begin
        if Sign(YearInfo[i,j].moondec)=sign(ObsLatitude) then begin
          // always up
          moonrise:=minv;
          moonset:=maxv;
          Chart3BoxAndWhiskerSeries1.AddXY(d,moonrise,moonrise,moonrise,moonset,moonset,'',RGBToColor(l,l,0));
          Chart3BoxAndWhiskerSeries2.AddXY(d,0,0,0,0,0);
        end
        else begin
          // no rise
          Chart3BoxAndWhiskerSeries1.AddXY(d,0,0,0,0,0);
          Chart3BoxAndWhiskerSeries2.AddXY(d,0,0,0,0,0);
        end;
      end
      else begin
        // moon rise set local time
        if YearInfo[i,j].moonrise<>0 then
          moonrise:=YearInfo[i,j].moonrise-d+localoffset
        else
          moonrise:=GetYearInfo(i,j,-1).moonrise-d+localoffset;
        if YearInfo[i,j].moonset<>0 then
          moonset:=YearInfo[i,j].moonset-d+localoffset
        else
          moonset:=GetYearInfo(i,j,1).moonset-d+localoffset;
        // in -12 +12 range
        if moonrise>0.5 then moonrise:=moonrise-1;
        if moonset>0.5 then moonset:=moonset-1;
        if (moonrise<moonset) then begin
          // single bar
          Chart3BoxAndWhiskerSeries1.AddXY(d,moonrise,moonrise,moonrise,moonset,moonset,'',RGBToColor(l,l,0));
          Chart3BoxAndWhiskerSeries2.AddXY(d,0,0,0,0,0);
        end
        else begin
          // split in two bar
          Chart3BoxAndWhiskerSeries1.AddXY(d,moonrise,moonrise,moonrise,maxv,maxv,'',RGBToColor(l,l,0));
          Chart3BoxAndWhiskerSeries2.AddXY(d,minv,minv,minv,moonset,moonset,'',RGBToColor(l,l,0));
        end;
      end;
    end;

end;

procedure Tf_calclun.DateChange(Sender: TObject);
begin
  if not (fsFirstShow in FormState) then begin
    SpinEditDay.MaxValue:=MonthDays[IsLeapYear(SpinEditYear.Value)][SpinEditMonth.Value];
    SpinEditDay.Hint:='1..'+inttostr(SpinEditDay.MaxValue);
    DateChangeTimer.Enabled:=false;
    DateChangeTimer.Enabled:=true;
    Screen.Cursor:=crHourGlass;
  end;
end;

procedure Tf_calclun.DateChangeTimerTimer(Sender: TObject);
begin
try
  DateChangeTimer.Enabled:=false;
  StatusLabel.Caption:='';
  TimeZoneD := GetTimeZoneD(EncodeDate(SpinEditYear.Value,SpinEditMonth.Value,SpinEditDay.Value));
  labeltz.Caption:=tz.ZoneName;
  // keep compute order year,month,day
  if ForceDateChange or (CurYear<>SpinEditYear.Value) then ComputeYear;
  if ForceDateChange or (CurrentMonth<>SpinEditMonth.Value)or(CurYear<>SpinEditYear.Value) then begin
     ComputeMonth;
     PredictionChange(Sender);
  end;
  ComputeDay;
  ForceDateChange:=false;
  CurYear:=SpinEditYear.Value;
  CurrentMonth:=SpinEditMonth.Value;
  CurrentDay:=SpinEditDay.Value;
finally
  Screen.Cursor:=crDefault;
  if FirstChange then begin
    FirstChange:=false;
    if splash.Visible then splash.close;
  end;
end;
end;

procedure Tf_calclun.BtnIncTimeClick(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
    0: SpinEditYear.Value:=SpinEditYear.Value+1;
    1,3: begin
         if SpinEditMonth.Value<12 then
           SpinEditMonth.Value:=SpinEditMonth.Value+1
         else begin
           SpinEditYear.Value:=SpinEditYear.Value+1;
           SpinEditMonth.Value:=1;
         end;
        end;
    2:  begin
          if SpinEditDay.Value<MonthDays[IsLeapYear(SpinEditYear.Value)][SpinEditMonth.Value] then
            SpinEditDay.Value:=SpinEditDay.Value+1
          else begin
            SpinEditDay.Value:=1;
            if SpinEditMonth.Value<12 then
              SpinEditMonth.Value:=SpinEditMonth.Value+1
            else begin
              SpinEditYear.Value:=SpinEditYear.Value+1;
              SpinEditMonth.Value:=1;
            end;
          end;
        end;
  end;
end;

procedure Tf_calclun.BtnDecTimeClick(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
    0: SpinEditYear.Value:=SpinEditYear.Value-1;
    1,3: begin
         if SpinEditMonth.Value>1 then
           SpinEditMonth.Value:=SpinEditMonth.Value-1
         else begin
           SpinEditYear.Value:=SpinEditYear.Value-1;
           SpinEditMonth.Value:=12;
         end;
        end;
    2:  begin
          if SpinEditDay.Value>1 then
            SpinEditDay.Value:=SpinEditDay.Value-1
          else begin
            if SpinEditMonth.Value>1 then
              SpinEditMonth.Value:=SpinEditMonth.Value-1
            else begin
              SpinEditYear.Value:=SpinEditYear.Value-1;
              SpinEditMonth.Value:=12;
            end;
            SpinEditDay.Value:=MonthDays[IsLeapYear(SpinEditYear.Value)][SpinEditMonth.Value];
          end;
        end;
  end;
end;

procedure Tf_calclun.BtnTodayClick(Sender: TObject);
var Year, Month, Day: Word;
begin
  DecodeDate(now, Year, Month, Day);
  SpinEditYear.Value:=Year;
  SpinEditMonth.Value:=Month;
  SpinEditDay.Value:=Day;
end;

procedure Tf_calclun.FormationEditingDone(Sender: TObject);
begin
  ActiveControl:=FormationLong;
  BtnSearchClick(nil);
end;

procedure Tf_calclun.BtnSearchClick(Sender: TObject);
var
  i:   integer;
  n: string;
begin
  n:=trim(EditFormation.Text);
  if Length(n)<3 then exit;
  dbm.Query('select id,name from moon ' + ' where '+
      ' NAME like "' + trim(uppercase(n)) + '%"' +
      ' order by NAME limit 100;');
  ComboBoxFormation.Clear;
  for i := 0 to dbm.RowCount - 1 do
  begin
    ComboBoxFormation.Items.AddObject(dbm.Results[i].Format[1].AsString,TObject(PtrInt(dbm.Results[i].Format[0].AsInteger)));
  end;
  if ComboBoxFormation.Items.Count>0 then begin
     ComboBoxFormation.ItemIndex:=0;
     ComboBoxFormationSelect(nil);
  end;
end;

procedure Tf_calclun.ComboBoxFormationSelect(Sender: TObject);
var i: integer;
begin
  i:=ComboBoxFormation.ItemIndex;
  SelectFormation(PtrInt(ComboBoxFormation.Items.Objects[i]));
end;

procedure Tf_calclun.SelectFormation(id: integer);
var lon,lat,llon,llat: double;
begin
  dbm.Query('select LONGI_N,LATI_N from moon where id=' + inttostr(id));
  if dbm.RowCount > 0 then begin
    lon := dbm.Results[0].Format[0].AsFloat;
    lat := dbm.Results[0].Format[1].AsFloat;
    if lon>=0 then
      DirLon.ItemIndex:=0
    else
      DirLon.ItemIndex:=1;
    if lat>=0 then
      DirLat.ItemIndex:=0
    else
      DirLat.ItemIndex:=1;
    if Abs(lon)>100 then
      llon:=0
    else if Abs(lon)>40 then
      llon:=min(7,max(2,Abs(lon)-90+2))
    else
      llon:=0;
    if Abs(lat)>40 then
      llat:=min(7,max(2,Abs(lat)-90+2))
    else
      llat:=0;
    LibrLon.Value:=llon;
    LibrLat.Value:=llat;
    FormationLong.Value := lon;
    FormationLat.Value := lat;
    case PageControlPrediction.ActivePageIndex of
      1:ComputeTerminator;
      2:ComputeLibration;
    end;
  end;
end;

procedure Tf_calclun.BtnGraphClick(Sender: TObject);
var p: TPoint;
begin
  p:=point(BtnGraph.Left,BtnGraph.Top+BtnGraph.Height);
  p:=PanelBtnGraph.ClientToScreen(p);
  PopupMenuGraph.PopUp(p.X,p.Y);
end;

procedure Tf_calclun.PageControlPredictionChange(Sender: TObject);
begin
  case PageControlPrediction.ActivePageIndex of
    0: begin
        PanelGlobal.Parent:=TabSheetColongitude;
        PanelFormation.Visible:=false;
        GroupBoxColongitude.Visible:=true;
        GroupBoxColongitude.Left:=0;
        GroupBoxSunElev.Visible:=false;
        GroupBoxLibration.Visible:=false;
        ColongitudeChange(Sender);
       end;
    1: begin
         PanelGlobal.Parent:=TabSheetSunElevation;
         PanelFormation.Visible:=true;
         PanelFormation.Left:=0;
         GroupBoxSunElev.Visible:=true;
         GroupBoxColongitude.Visible:=false;
         GroupBoxLibration.Visible:=false;
         TerminatorChange(Sender);
       end;
    2: begin
         PanelGlobal.Parent:=TabSheetLibration;
         PanelFormation.Visible:=true;
         PanelFormation.Left:=0;
         GroupBoxLibration.Visible:=true;
         GroupBoxColongitude.Visible:=false;
         GroupBoxSunElev.Visible:=false;
         LibrationChange(Sender);
       end;
  end;
  GroupBoxDuration.Left:=5000;
  GroupBoxConstraint.Left:=5100;
end;

procedure Tf_calclun.PredictionEditingDone(Sender: TObject);
begin
  ActiveControl:=FormationLong;
  PredictionChange(Sender);
end;

procedure Tf_calclun.PredictionChange(Sender: TObject);
begin
 case PageControlPrediction.ActivePageIndex of
   0: ColongitudeChange(Sender);
   1: TerminatorChange(Sender);
   2: LibrationChange(Sender);
 end;
end;

procedure Tf_calclun.ColongitudeTimerTimer(Sender: TObject);
begin
  ColongitudeTimer.Enabled:=false;
  if LockPredictionTimer then begin
    ColongitudeTimer.Enabled:=true;
  end
  else begin
    LockPredictionTimer:=true;
    ComputeColongitude;
    LockPredictionTimer:=false;
  end;
end;

procedure Tf_calclun.ColongitudeChange(Sender: TObject);
begin
  ColongitudeTimer.Enabled:=false;
  ColongitudeTimer.Enabled:=true;
  screen.Cursor:=crHourGlass;
end;

procedure Tf_calclun.ComputeColongitude;
var refval: SpiceDouble;
  colong,delta: SpiceDouble;
  sc1,sc2,sc3,scresult: PSpiceCell;
  et0,et1,x,y,z,dt,r,slon,slat,colongitude: SpiceDouble;
  fixref: ConstSpiceChar;
  i,nfind: integer;
  sti,eni: string;
  Year, Month: Word;
begin
 try
  colong:=EditColongitude.Value;
  delta:=EditColongitudeDelta.Value;
  year:=SpinEditYear.Value;
  month:=SpinEditMonth.Value;
  dt:=EncodeDate(year,month,1);

  if (CurColongColong=colong)and(CurColongDelta=delta)and(CurColondDt=dt)and(CurColongDuration=PredictionDuration.Value)and
     (CurColongNight=CheckBoxNightEvents.Checked)and(CurColongVisible=CheckBoxMoonVisible.Checked)and
     (CurColongSunElev=EditTwilight.Value)and(CurColongMoonElev=EditMinElevation.Value)
     then exit;

  CurColongColong:=colong;
  CurColongDelta:=delta;
  CurColondDt:=dt;
  CurColongDuration:=PredictionDuration.Value;
  CurColongNight:=CheckBoxNightEvents.Checked;
  CurColongVisible:=CheckBoxMoonVisible.Checked;
  CurColongSunElev:=EditTwilight.Value;
  CurColongMoonElev:=EditMinElevation.Value;

  GridColongitude.RowCount:=1;
  GridColongitude.RowCount:=15;
  reset_c;

  et:=DateTime2ET(dt);
  sc1:=initdoublecell(1);
  sc2:=initdoublecell(2);
  sc3:=initdoublecell(3);
  scresult:=initdoublecell(4);
  et0:=et;
  et1:=et0+365.25*SecsPerDay*PredictionDuration.Value;
  wninsd_c ( et0, et1, sc1 );

  if delta<1 then begin
    if not MoonSearchColongitude(colong*deg2rad,1.0*deg2rad,nfind,sc1,sc2) then begin
      StatusLabel.Caption:=SpiceLastError;
      exit;
    end;
    copy_c(sc2,sc1);
  end;
  if not MoonSearchColongitude(colong*deg2rad,delta*deg2rad,nfind,sc1,sc2) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;

  copy_c(sc2,scresult);

  if CheckBoxMoonVisible.Checked then begin
    refval := EditMinElevation.Value;  // elevation degree
    if not MoonSearchRiseSet(obspos,ObsLatitude,refval,nfind,scresult,sc2) then begin
      StatusLabel.Caption:=SpiceLastError;
      exit;
    end;
    copy_c(sc2,scresult);
  end;

  if CheckBoxNightEvents.Checked then begin
    refval := -EditTwilight.Value;   // elevation degree
    if not SearchNight(obspos,ObsLatitude,refval,nfind,scresult,sc2) then begin
      StatusLabel.Caption:=SpiceLastError;
      exit;
    end;
    copy_c(sc2,scresult);
  end;

  if nfind>0 then
  for i:=0 to nfind-1 do begin
    if i>(GridColongitude.RowCount-2) then GridColongitude.RowCount:=GridColongitude.RowCount+1;
    wnfetd_c(scresult,i,x,y);
    dt:=ET2DateTime(x);
    sti:=FormatDateTime(datestd,dt+GetTimeZoneD(dt));
    dt:=ET2DateTime(y);
    eni:=FormatDateTime(datestd,dt+GetTimeZoneD(dt));
    MoonSubSolarPoint((x+y)/2,fixref,x,y,z,r,slon,slat,colongitude);
    GridColongitude.Cells[0,i+1]:=sti;
    GridColongitude.Cells[1,i+1]:=eni;
    GridColongitude.Cells[2,i+1]:=FormatFloat(f2,slat*rad2deg);
  end;

  scard_c (0, sc1);
  scard_c (0, sc2);
  scard_c (0, sc3);
  scard_c (0, scresult);
 finally
  screen.Cursor:=crDefault;
 end;

end;

procedure Tf_calclun.TerminatorChange(Sender: TObject);
begin
  TerminatorTimer.Enabled:=false;
  TerminatorTimer.Enabled:=true;
  screen.Cursor:=crHourGlass;
end;

procedure Tf_calclun.TerminatorTimerTimer(Sender: TObject);
begin
  TerminatorTimer.Enabled:=false;
  if LockPredictionTimer then begin
    TerminatorTimer.Enabled:=true;
  end
  else begin
    LockPredictionTimer:=true;
    ComputeTerminator;
    LockPredictionTimer:=false;
  end;
end;

procedure Tf_calclun.ComputeTerminator;
var fixref: ConstSpiceChar;
  relate: ConstSpiceChar;
  refval: SpiceDouble;
  sc1,sc2,sc3,scresult: PSpiceCell;
  et0,et1,x,y,dt: SpiceDouble;
  i,nfind: integer;
  sti,eni: string;
  lon,lat,dl,llon,llat,z: SpiceDouble;
  xx,yy,zz,r,lo,la,colongitude:SpiceDouble;
  pos: TDouble3;
  Year, Month: Word;
begin
 try
  lon:=FormationLong.Value*deg2rad;
  lat:=FormationLat.Value*deg2rad;
  pos:=MoonSurfacePos(lon,lat);

  year:=SpinEditYear.Value;
  month:=SpinEditMonth.Value;
  dt:=EncodeDate(year,month,1);

  if (CurSunElevLon=lon)and(CurSunElevLat=lat)and(CurSunElevDt=dt)and(CurSunElevDuration=PredictionDuration.Value)and
     (CurSunElevSunMin=SunMin.Value)and(CurSunElevSunMax=SunMax.Value)and
     (CurSunElevNight=CheckBoxNightEvents.Checked)and(CurSunElevVisible=CheckBoxMoonVisible.Checked)and
     (CurSunElevSunElev=EditTwilight.Value)and(CurSunElevMoonElev=EditMinElevation.Value)
     then exit;

  CurSunElevLon:=lon;
  CurSunElevLat:=lat;
  CurSunElevDt:=dt;
  CurSunElevSunMin:=SunMin.Value;
  CurSunElevSunMax:=SunMax.Value;
  CurSunElevDuration:=PredictionDuration.Value;
  CurSunElevNight:=CheckBoxNightEvents.Checked;
  CurSunElevVisible:=CheckBoxMoonVisible.Checked;
  CurSunElevSunElev:=EditTwilight.Value;
  CurSunElevMoonElev:=EditMinElevation.Value;

  GridTerminator1.RowCount:=1;
  GridTerminator1.RowCount:=15;
  GridTerminator2.RowCount:=1;
  GridTerminator2.RowCount:=15;
  if SunMax.Value<=SunMin.Value then exit;

  reset_c;

  et:=DateTime2ET(dt);
  sc1:=initdoublecell(1);
  sc2:=initdoublecell(2);
  sc3:=initdoublecell(3);
  scresult:=initdoublecell(4);
  et0:=et;
  et1:=et0+365.25*SecsPerDay*PredictionDuration.Value;
  wninsd_c ( et0, et1, sc1 );

  fixref := fixrefME;
  relate:='<';
  refval := (90-SunMin.Value) * deg2rad;  // minimal sun elevation
  if not MoonSearchIllum(pos,refval,relate,fixref,nfind,sc1,sc2) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;

  relate:='>';
  refval := (90-SunMax.Value) * deg2rad;  // maximal sun elevation
  if not MoonSearchIllum(pos,refval,relate,fixref,nfind,sc2,sc1)then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;

  copy_c(sc1,scresult);

  if CheckBoxMoonVisible.Checked then begin
    refval := StrToFloatDef(EditMinElevation.Text,15);  // elevation degree
    if not MoonSearchRiseSet(obspos,ObsLatitude,refval,nfind,scresult,sc2) then begin
      StatusLabel.Caption:=SpiceLastError;
      exit;
    end;
    copy_c(sc2,scresult);
  end;

  if CheckBoxNightEvents.Checked then begin
    refval := -StrToFloatDef(EditTwilight.Text,6);  // civil twilight
    if not SearchNight(obspos,ObsLatitude,refval,nfind,scresult,sc2) then begin
      StatusLabel.Caption:=SpiceLastError;
      exit;
    end;
    copy_c(sc2,scresult);
  end;

  scard_c (0, sc2);
  scard_c (0, sc3);
  for i:=0 to nfind-1 do begin
     wnfetd_c(scresult,i,x,y);
     MoonSubSolarPoint(x,fixref,xx,yy,zz,r,lo,la,colongitude);
     dl:=rmod(lo-lon+pi2,pi2);
     if dl<pi then
       wninsd_c(x,y,sc2)   // morning terminator
     else
       wninsd_c(x,y,sc3);  // evening terminator
  end;

  // morning terminator
  nfind:=wncard_c(sc2);
  if nfind>0 then
  for i:=0 to nfind-1 do begin
    if i>(GridTerminator1.RowCount-2) then GridTerminator1.RowCount:=GridTerminator1.RowCount+1;
    wnfetd_c(sc2,i,x,y);
    dt:=ET2DateTime(x);
    sti:=FormatDateTime(datestd,dt+GetTimeZoneD(dt));
    dt:=ET2DateTime(y);
    eni:=FormatDateTime(datestd,dt+GetTimeZoneD(dt));
    GridTerminator1.Cells[0,i+1]:=sti;
    GridTerminator1.Cells[1,i+1]:=eni;
    if MoonSubObserverPoint((x+y)/2,obspos,x,y,z,r,llon,llat) then begin
      GridTerminator1.Cells[2,i+1]:=DEmToStr(llon*rad2deg);
      GridTerminator1.Cells[3,i+1]:=DEmToStr(llat*rad2deg);
    end;
  end;
  // evening terminator
  nfind:=wncard_c(sc3);
  if nfind>0 then
  for i:=0 to nfind-1 do begin
    if i>(GridTerminator2.RowCount-2) then GridTerminator2.RowCount:=GridTerminator2.RowCount+1;
    wnfetd_c(sc3,i,x,y);
    dt:=ET2DateTime(x);
    sti:=FormatDateTime(datestd,dt+GetTimeZoneD(dt));
    dt:=ET2DateTime(y);
    eni:=FormatDateTime(datestd,dt+GetTimeZoneD(dt));
    GridTerminator2.Cells[0,i+1]:=sti;
    GridTerminator2.Cells[1,i+1]:=eni;
    if MoonSubObserverPoint((x+y)/2,obspos,x,y,z,r,llon,llat) then begin
      GridTerminator2.Cells[2,i+1]:=DEmToStr(llon*rad2deg);
      GridTerminator2.Cells[3,i+1]:=DEmToStr(llat*rad2deg);
    end;
  end;

  scard_c (0, sc1);
  scard_c (0, sc2);
  scard_c (0, sc3);
  scard_c (0, scresult);
 finally
  screen.Cursor:=crDefault;
 end;
end;

procedure Tf_calclun.LibrationChange(Sender: TObject);
begin
  LibrationTimer.Enabled:=false;
  LibrationTimer.Enabled:=true;
  screen.Cursor:=crHourGlass;
end;

procedure Tf_calclun.LibrationTimerTimer(Sender: TObject);
begin
  LibrationTimer.Enabled:=false;
  if LockPredictionTimer then begin
    LibrationTimer.Enabled:=true;
  end
  else begin
    LockPredictionTimer:=true;
    ComputeLibration;
    LockPredictionTimer:=false;
  end;
end;

procedure Tf_calclun.MenuItemDocumentationClick(Sender: TObject);
begin
  ShowHelpDoc('Doc','CalcLun','doc');
end;

procedure Tf_calclun.MenuItemAboutClick(Sender: TObject);
begin
Showmessage('CalcLun '+Splashversion+crlf+
            compile_version+crlf+
            avlcpy+crlf+crlf+
            'Conception : Patrick Chevalley'+crlf+
            'Programming : Patrick Chevalley'+crlf+crlf+
            'This program is free software; you can redistribute it and/or '+crlf+
            'modify it under the terms of the GNU General Public License '+crlf+
            'as published by the Free Software Foundation.'
            );
end;

procedure Tf_calclun.MenuPrintEphemClick(Sender: TObject);
begin
  if f_ephem=nil then begin
     f_ephem:=Tf_ephem.Create(self);
     f_ephem.tz:=tz;
     f_ephem.Setlang;
     f_ephem.annee.Value:=CurYear;
     f_ephem.annee1.Value:=CurYear;
     f_ephem.mois.Value:=CurrentMonth;
     f_ephem.mois1.Value:=CurrentMonth;
     f_ephem.jour.Value:=CurrentDay;
     f_ephem.jour1.Value:=CurrentDay;
  end;
  FormPos(f_ephem,mouse.CursorPos.X,Mouse.CursorPos.Y);
  f_ephem.ShowModal;
end;

procedure Tf_calclun.MenuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_calclun.ComputeLibration;
var fixref: ConstSpiceChar;
  relate,coord: ConstSpiceChar;
  refval: SpiceDouble;
  sc1,sc2,sc3,scresult: PSpiceCell;
  et0,et1,x,y,dt: SpiceDouble;
  phase,incdnc,emissn:SpiceDouble;
  i,nfind: integer;
  sti,eni: string;
  lon,lat: SpiceDouble;
  pos: TDouble3;
  Year, Month: Word;
begin
 try
  lon:=FormationLong.Value*deg2rad;
  lat:=FormationLat.Value*deg2rad;
  pos:=MoonSurfacePos(lon,lat);

  year:=SpinEditYear.Value;
  month:=SpinEditMonth.Value;
  dt:=EncodeDate(year,month,1);

  if (CurLibrationLon=lon)and(CurLibrationLat=lat)and(CurLibrationDt=dt)and(CurLibrationDuration=PredictionDuration.Value)and
     (CurLibrationLibrLon=LibrLon.Value)and(CurLibrationLibrLat=LibrLat.Value)and
     (CurLibrationEW=DirLon.ItemIndex)and(CurLibrationNS=DirLat.ItemIndex)and
     (CurLibrationNight=CheckBoxNightEvents.Checked)and(CurLibrationVisible=CheckBoxMoonVisible.Checked)and
     (CurLibrationSunElev=EditTwilight.Value)and(CurLibrationMoonElev=EditMinElevation.Value)
     then exit;

  CurLibrationLon:=lon;
  CurLibrationLat:=lat;
  CurLibrationDt:=dt;
  CurLibrationLibrLon:=LibrLon.Value;
  CurLibrationLibrLat:=LibrLat.Value;
  CurLibrationEW:=DirLon.ItemIndex;
  CurLibrationNS:=DirLat.ItemIndex;
  CurLibrationDuration:=PredictionDuration.Value;
  CurLibrationNight:=CheckBoxNightEvents.Checked;
  CurLibrationVisible:=CheckBoxMoonVisible.Checked;
  CurLibrationSunElev:=EditTwilight.Value;
  CurLibrationMoonElev:=EditMinElevation.Value;

  GridLibration.RowCount:=1;
  GridLibration.RowCount:=15;
  reset_c;

  et:=DateTime2ET(dt);

  sc1:=initdoublecell(1);
  sc2:=initdoublecell(2);
  sc3:=initdoublecell(3);
  scresult:=initdoublecell(4);
  et0:=et;
  et1:=et0+365.25*SecsPerDay*PredictionDuration.Value;
  wninsd_c ( et0, et1, sc1 );

  fixref := fixrefME;
  relate:='<';
  refval := 85 * deg2rad;  // 5° sun elevation
  if not MoonSearchIllum(pos,refval,relate,fixref,nfind,sc1,sc2) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;

  coord:='LONGITUDE';
  if DirLon.ItemIndex=1 then begin // W
    relate:='<';
    refval:=-LibrLon.Value*deg2rad;
  end
  else begin // E
    relate:='>';
    refval:=LibrLon.Value*deg2rad;
  end;
  if not MoonSearchLibration(coord,refval,relate,fixref,nfind,sc2,sc1) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  coord:='LATITUDE';
  if DirLat.ItemIndex=1 then begin // S
    relate:='<';
    refval:=-LibrLat.Value*deg2rad;
  end
  else begin // N
    relate:='>';
    refval:=LibrLat.Value*deg2rad;
  end;
  if not MoonSearchLibration(coord,refval,relate,fixref,nfind,sc1,sc2) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;

  copy_c(sc2,scresult);

  if CheckBoxMoonVisible.Checked then begin
    refval := StrToFloatDef(EditMinElevation.Text,15);  // elevation degree
    if not MoonSearchRiseSet(obspos,ObsLatitude,refval,nfind,scresult,sc2) then begin
      StatusLabel.Caption:=SpiceLastError;
      exit;
    end;
    copy_c(sc2,scresult);
  end;

  if CheckBoxNightEvents.Checked then begin
    refval := -StrToFloatDef(EditTwilight.Text,6);  // civil twilight
    if not SearchNight(obspos,ObsLatitude,refval,nfind,scresult,sc2) then begin
      StatusLabel.Caption:=SpiceLastError;
      exit;
    end;
    copy_c(sc2,scresult);
  end;

  if nfind>0 then
  for i:=0 to nfind-1 do begin
    if i>(GridLibration.RowCount-2) then GridLibration.RowCount:=GridLibration.RowCount+1;
    wnfetd_c(scresult,i,x,y);
    dt:=ET2DateTime(x);
    sti:=FormatDateTime(datestd,dt+GetTimeZoneD(dt));
    dt:=ET2DateTime(y);
    eni:=FormatDateTime(datestd,dt+GetTimeZoneD(dt));
    GridLibration.Cells[0,i+1]:=sti;
    GridLibration.Cells[1,i+1]:=eni;
    if MoonPointIllum(pos,x,fixref,phase,incdnc,emissn) then
      GridLibration.Cells[2,i+1]:=FormatFloat(f2,90-rad2deg*incdnc)+ldeg
    else
      GridLibration.Cells[2,i+1]:='-';
    if MoonPointIllum(pos,y,fixref,phase,incdnc,emissn) then
      GridLibration.Cells[3,i+1]:=FormatFloat(f2,90-rad2deg*incdnc)+ldeg
    else
      GridLibration.Cells[3,i+1]:='-';
  end;

  scard_c (0, sc1);
  scard_c (0, sc2);
  scard_c (0, sc3);
  scard_c (0, scresult);
 finally
  screen.Cursor:=crDefault;
 end;
end;

end.

