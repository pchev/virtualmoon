unit calclun_main;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef mswindows}
    Windows, Registry, ShlObj,
  {$endif}
  cspice, pas_spice, moon_spice, u_util, u_constant, LazSysUtils, TAGraph, TARadialSeries, TASeries, TAFuncSeries, IniFiles,
  TAChartUtils, TAIntervalSources, math, u_projection, cu_tz, LazUTF8,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, EditBtn, Spin, ComCtrls, Grids, Menus, Types, TACustomSeries, TAMultiSeries, TATransformations;

const
    mcolDay=0; mcolRa2000=1; mcolDe2000=2; mcolRa=3; mcolDe=4; mcolDist=5; mcolDiam=6; mcolPhase=7; mcolLunation=8; mcolIllum=9; mcolColong=10; mcolSubSolLat=11; mcolLibrLon=12; mcolLibrLat=13; mcolPa=14; mcolRise=15; mcolSet=16;
    dcolHour=0; dcolRa2000=1; dcolDe2000=2; dcolRa=3; dcolDe=4; dcolDist=5; dcolDiam=6; dcolColong=7; dcolSubSolLat=8; dcolLibrLon=9; dcolLibrLat=10; dcolPa=11;

type

  TMoonMonthData = class(TObject)
    day: integer;
    date: TDateTime;
    ra2000,de2000,ra,de,dist,diam,phase,lunation,illum,colong,subsollat,librlong,librlat,pa,trise,tset: double;
  end;

  TYearInfo = record  // all time are UT
    tz: double;
    moonra,moondec,sunra,sundec: double;
    moonrise,moonset,sunrise,sunset: double;
    astro1,astro2,nautic1,nautic2,civil1,civil2: double;
  end;

  { Tf_calclun }

  Tf_calclun = class(TForm)
    BtnCompute: TButton;
    BtnPrevision: TButton;
    Button1: TButton;
    BtnToday: TButton;
    BtnGraph: TButton;
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
    ChartYearBoxAndWhiskerSeries1: TBoxAndWhiskerSeries;
    ChartYearBoxAndWhiskerSeries2: TBoxAndWhiskerSeries;
    ChartYearSeriesAstro1: TAreaSeries;
    ChartYearSeriesAstro2: TAreaSeries;
    ChartYearSeriesNautic1: TAreaSeries;
    ChartYearSeriesNautic2: TAreaSeries;
    ChartYearSeriesSunrise: TAreaSeries;
    ChartYearSeriesSunset: TAreaSeries;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    DayPhase: TImage;
    LabelChartYear: TLabel;
    DateChangeTimer: TTimer;
    Memo1: TMemo;
    GridLibrationList: TStringGrid;
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
    NightEvent: TCheckBox;
    MinElevation: TFloatSpinEdit;
    Label6: TLabel;
    PageControl1: TPageControl;
    PanelRight: TPanel;
    PanelLeft: TPanel;
    PanelBottom: TPanel;
    PanelTop: TPanel;
    RiseSetBox: TCheckBox;
    DateEdit1: TDateEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Longitude: TFloatSpinEdit;
    Latitude: TFloatSpinEdit;
    Altitude: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    PanelTest: TPanel;
    SpinEditYear: TSpinEdit;
    SpinEditMonth: TSpinEdit;
    GridMonth: TStringGrid;
    GridPhaseList: TStringGrid;
    GridDay: TStringGrid;
    TabSheetYear: TTabSheet;
    TabSheetDay: TTabSheet;
    TabSheetMonth: TTabSheet;
    TabSheetTest: TTabSheet;
    TimeEdit1: TTimeEdit;
    procedure BtnComputeClick(Sender: TObject);
    procedure BtnGraphClick(Sender: TObject);
    procedure BtnPrevisionClick(Sender: TObject);
    procedure BtnTodayClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ChartAltAzMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ChartYearAxisList1MarkToText(var AText: String; AMark: Double);
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
    procedure SelectGraph(Sender: TObject);
  private
    et : SpiceDouble;
    obspos: TDouble3;
    tz: TCdCTimeZone;
    FCurrentMonthGraph: integer;
    YearInfo: array[1..12,1..31] of TYearInfo;
    procedure SetLang;
    procedure GetAppDir;
    procedure SetKernelsPath;
    procedure SetObservatory;
    function  GetTimeZone(sdt: Tdatetime): double;
    function  GetTimeZoneD(sdt: Tdatetime): double;
    procedure ComputeYear;
    procedure ComputeMonth;
    procedure ComputeDay;
    procedure PlotYearGraph;
    procedure PlotMonthGraph(col:integer);
    procedure Illum;
    procedure Position;
    procedure PositionTopo;
    procedure SubEarth;
    procedure SubSolar;
    procedure FindPhase(realphase:boolean=false);
    procedure FindRiseSet;
    procedure FindIllum;
    procedure FindLibration;
    procedure FindMixed;
    procedure FindColongitude;
  public

  end;

var
  f_calclun: Tf_calclun;


implementation

{$R *.lfm}

{ Tf_calclun }

procedure Tf_calclun.FormCreate(Sender: TObject);
begin
  DefaultFormatSettings.DateSeparator:='-';
  StatusLabel.Caption:='';
  LabelAltAz.Caption:='';
  LabelChartYear.Caption:='';
  ObsLatitude:=0;
  ObsLongitude:=0;
  ObsAltitude:=0;
  ObsTZ:='Etc/GMT';
  tz:=TCdCTimeZone.Create;

  SetLang;

  GetAppDir;
  InitError;
  reset_c;
  SetKernelsPath;

  furnsh_c(pchar(slash(PrivateDir)+'vma.tm'));
  if failed_c then begin
    ShowMessage('Error loading file!'+crlf+SpiceLastError);
    halt;
  end;

  memo1.Font.Height:=0;
  memo1.clear;
  memo1.Lines.Add('Données disponibles:');
  pckallinfo(memo1.Lines);
  spkallinfo(memo1.Lines);

  DateEdit1.Date:=nowutc;
  TimeEdit1.Time:=19/24;

  BtnToday.Click;
  PageControl1.ActivePage:=TabSheetYear;
  FCurrentMonthGraph:=mcolLibrLon;

end;

procedure Tf_calclun.FormDestroy(Sender: TObject);
var i: integer;
begin
  tz.Free;
  for i:=0 to GridMonth.RowCount-1 do begin
    if GridMonth.Objects[0,i]<>nil then GridMonth.Objects[0,i].free;
  end;
end;

procedure Tf_calclun.FormShow(Sender: TObject);
begin
  SetObservatory;
  DateChange(nil);
end;

procedure Tf_calclun.SetLang;
var i: integer;
begin
  MenuItem1.Caption:='RA 2000';
  Menuitem2.Caption:='DE 2000';
  Menuitem3.Caption:='RA apparent';
  Menuitem4.Caption:='DE apparent';
  Menuitem5.Caption:='Distance';
  Menuitem6.Caption:='Diameter';
  Menuitem7.Caption:='Phase';
  Menuitem8.Caption:='Lunation';
  Menuitem9.Caption:='Illumination';
  Menuitem10.Caption:='Colongitude';
  Menuitem11.Caption:='Sub-solar latitude';
  Menuitem12.Caption:='Libration longitude';
  Menuitem13.Caption:='Libration latitude';
  Menuitem14.Caption:='PA';
  Menuitem15.Caption:='Rise time';
  Menuitem16.Caption:='Set time';
  GridYear.ColWidths[0]:=120;
  GridYear.Width:=GridYear.ColWidths[0]+(GridYear.ColCount-1)*GridYear.DefaultColWidth;
  for i:=1 to 31 do GridYear.Cells[i,0]:=inttostr(i);
  for i:=1 to 12 do GridYear.Cells[0,i]:=DefaultFormatSettings.LongMonthNames[i];
  GridPhaseList.Cells[0,0]:='New Moon';
  GridPhaseList.Cells[1,0]:='First quarter';
  GridPhaseList.Cells[2,0]:='Full Moon';
  GridPhaseList.Cells[3,0]:='Last quarter';
  GridLibrationList.Cells[0,0]:='East';
  GridLibrationList.Cells[1,0]:='West';
  GridLibrationList.Cells[2,0]:='North';
  GridLibrationList.Cells[3,0]:='South';
  GridMonth.ColCount:=mcolSet+1;
  GridMonth.ColWidths[0]:=50;
  GridMonth.Cells[mcolDay,0]:='Day';
  GridMonth.Cells[mcolRa2000,0]:='RA 2000';
  GridMonth.Cells[mcolDe2000,0]:='DE 2000';
  GridMonth.Cells[mcolRa,0]:='RA apparent';
  GridMonth.Cells[mcolDe,0]:='DE apparent';
  GridMonth.Cells[mcolDist,0]:='Distance';
  GridMonth.Cells[mcolDiam,0]:='Diameter';
  GridMonth.Cells[mcolPhase,0]:='Phase';
  GridMonth.Cells[mcolLunation,0]:='Lunation';
  GridMonth.Cells[mcolIllum,0]:='Illumination';
  GridMonth.Cells[mcolColong,0]:='Colongitude';
  GridMonth.Cells[mcolSubSolLat,0]:='Sub-solar latitude';
  GridMonth.Cells[mcolLibrLon,0]:='Libration longitude';
  GridMonth.Cells[mcolLibrLat,0]:='Libration latitude';
  GridMonth.Cells[mcolPa,0]:='PA';
  GridMonth.Cells[mcolRise,0]:='Rise time';
  GridMonth.Cells[mcolSet,0]:='Set time';
  LabelPhase1.Caption:='New Moon:'+crlf+
                      'First quarter:'+crlf+
                      'Full Moon:'+crlf+
                      'Last quarter:';
  GridDay.ColCount:=dcolPa+1;
  GridDay.ColWidths[0]:=50;
  GridDay.Cells[dcolHour,0]:='Hour';
  GridDay.Cells[dcolRa2000,0]:='RA 2000';
  GridDay.Cells[dcolDe2000,0]:='DE 2000';
  GridDay.Cells[dcolRa,0]:='RA apparent';
  GridDay.Cells[dcolDe,0]:='DE apparent';
  GridDay.Cells[dcolDist,0]:='Distance';
  GridDay.Cells[dcolDiam,0]:='Diameter';
  GridDay.Cells[dcolColong,0]:='Colongitude';
  GridDay.Cells[dcolSubSolLat,0]:='Sub-solar latitude';
  GridDay.Cells[dcolLibrLon,0]:='Libration longitude';
  GridDay.Cells[dcolLibrLat,0]:='Libration latitude';
  GridDay.Cells[dcolPa,0]:='PA';
  LabelRise1.Caption:='Rise :'+crlf+'Set :';

end;

procedure Tf_calclun.SetKernelsPath;
var i: integer;
    buf: string;
    fi,fo: TextFile;
begin
  AssignFile(fi,slash(appdir) + slash('data') + slash('kernels')+'vma.tm');
  Reset(fi);
  AssignFile(fo,slash(PrivateDir)+'vma.tm');
  Rewrite(fo);
  repeat
    ReadLn(fi,buf);
    if copy(buf,1,11)='PATH_VALUES' then begin
      buf:='PATH_VALUES = ( '''+slash(appdir) + slash('data') + slash('kernels')+''' )';
    end;
    WriteLn(fo,buf);
  until eof(fi);
  CloseFile(fi);
  CloseFile(fo);
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
  i,j,k,r,nd: integer;
  year: Word;
  newmoon: array[0..14] of double;
  refval: SpiceDouble;
  Pcnfine,Presult: PSpiceCell;
  nfind: SpiceInt;
  et0,et1: SpiceDouble;
  yy,mm,dd: Word;
  x,y,z,sx,sy,sz,sr,ra,de,llon,llat : SpiceDouble;
  dtr, dts, dtz: double;
  obsref: ConstSpiceChar;
  fixref,coord,relate: ConstSpiceChar;
begin
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

  // initialize timezone
  dtz:=GetTimeZoneD(EncodeDate(year,1,1)+0.5);
  FillByte(YearInfo,12*31*sizeof(TYearInfo),0);
  for i:=1 to 12 do begin
    for j:=1 to 31 do begin
      if j<=MonthDays[IsLeapYear(year)][i] then
         YearInfo[i,j].tz:=GetTimeZoneD(EncodeDate(year,i,j)+0.5)
      else
         YearInfo[i,j].tz:=dtz;
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
  // expand by one day for the case the moon is already rised at the start of first interval
  wnexpd_c ( SecsPerDay, SecsPerDay, Pcnfine );
  refval := -0.8;  // elevation degree, upper limb with refraction
  if not MoonSearchRiseSet(obspos,ObsLatitude,refval,nfind,Pcnfine,Presult) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  if nfind>1 then for i:=1 to nfind-2 do begin // skip first and last expanded window
    wnfetd_c(Presult,i,x,y);
    dtr:=ET2DateTime(x);
    DecodeDate(dtr,yy,mm,dd);
    if yy=year then
      YearInfo[mm,dd].moonrise:=dtr;
    dts:=ET2DateTime(y);
    DecodeDate(dts,yy,mm,dd);
    if yy=year then
      YearInfo[mm,dd].moonset:=dts;
  end;
  scard_c (0, Pcnfine);
  scard_c (0, Presult);

  // Sun rise, set and twilight for the year
  Pcnfine:=initdoublecell(1);
  Presult:=initdoublecell(2);
  dt:=EncodeDate(year,1,1);
  et:=DateTime2ET(dt);
  et0:=et;
  et1:=et0+366*SecsPerDay;
  wninsd_c ( et0, et1, Pcnfine );
  // expand by one day for the case the moon is already rised at the start of first interval
  wnexpd_c ( SecsPerDay, SecsPerDay, Pcnfine );
  refval := -0.8;  // elevation degree, upper limb with refraction
  if not SunSearchRiseSet(obspos,ObsLatitude,refval,nfind,Pcnfine,Presult) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  if nfind>1 then for i:=1 to nfind-2 do begin // skip first and last expanded window
    wnfetd_c(Presult,i,x,y);
    dtr:=ET2DateTime(x);
    DecodeDate(dtr,yy,mm,dd);
    if yy=year then begin
      YearInfo[mm,dd].sunrise:=dtr;
     end;
    dts:=ET2DateTime(y);
    DecodeDate(dts,yy,mm,dd);
    if yy=year then begin
      YearInfo[mm,dd].sunset:=dts;
    end;
  end;
  scard_c (0, Pcnfine);
  scard_c (0, Presult);

  // sun and moon geocentric position and twilight
  for i:=1 to 12 do begin
    nd:=MonthDays[IsLeapYear(year)][i];
    for j:=1 to 31 do begin
      if j<=nd then begin
        dt:=EncodeDate(year,i,j);
        et:=DateTime2ET(dt);
        if not SunTopocentric(et,true,obspos,obsref,sx,sy,sz,sr,ra,de) then begin
          StatusLabel.Caption:=SpiceLastError;
          exit;
        end;
        // twilight
        jd0:=Jd(year,i,j,0);
        Time_Alt(jd0,ra,de,-18,ct1,ct2,ObsLatitude,ObsLongitude);
        if abs(ct1)<=24 then YearInfo[i,j].astro1:=trunc(dt)+rmod(ct1+dtz+24,24)/24 else YearInfo[i,j].astro1:=0;
        if abs(ct2)<=24 then YearInfo[i,j].astro2:=trunc(dt)+rmod(ct2+dtz+24,24)/24 else YearInfo[i,j].astro2:=0;
        Time_Alt(jd0,ra,de,-12,ct1,ct2,ObsLatitude,ObsLongitude);
        if abs(ct1)<=24 then YearInfo[i,j].nautic1:=trunc(dt)+rmod(ct1+dtz+24,24)/24 else YearInfo[i,j].nautic1:=0;
        if abs(ct2)<=24 then YearInfo[i,j].nautic2:=trunc(dt)+rmod(ct2+dtz+24,24)/24 else YearInfo[i,j].nautic2:=0;
        Time_Alt(jd0,ra,de,-6,ct1,ct2,ObsLatitude,ObsLongitude);
        if abs(ct1)<=24 then YearInfo[i,j].civil1:=trunc(dt)+rmod(ct1+dtz+24,24)/24 else YearInfo[i,j].civil1:=0;
        if abs(ct2)<=24 then YearInfo[i,j].civil2:=trunc(dt)+rmod(ct2+dtz+24,24)/24 else YearInfo[i,j].civil2:=0;
        YearInfo[i,j].sunra:=ra;
        YearInfo[i,j].sundec:=de;
        if not MoonTopocentric(et,true,obspos,obsref,sx,sy,sz,sr,ra,de) then begin
          StatusLabel.Caption:=SpiceLastError;
          exit;
        end;
        YearInfo[i,j].moonra:=ra;
        YearInfo[i,j].moondec:=de;
        // fix incoherent value near circum-polar limit
        if (YearInfo[i,j].sunrise=0)and(YearInfo[i,j].sunset<>0)and(j>1) then YearInfo[i,j].sunrise:=YearInfo[i,j-1].sunrise;
        if (YearInfo[i,j].sunset=0)and(YearInfo[i,j].sunrise<>0)and(j>1) then YearInfo[i,j].sunset:=YearInfo[i,j-1].sunset;
        if (YearInfo[i,j].astro1=0)and(YearInfo[i,j].astro2<>0)and(j>1) then YearInfo[i,j].astro1:=YearInfo[i,j-1].astro1;
        if (YearInfo[i,j].astro2=0)and(YearInfo[i,j].astro1<>0)and(j>1) then YearInfo[i,j].astro2:=YearInfo[i,j-1].astro2;
        if (YearInfo[i,j].nautic1=0)and(YearInfo[i,j].nautic2<>0)and(j>1) then YearInfo[i,j].nautic1:=YearInfo[i,j-1].nautic1;
        if (YearInfo[i,j].nautic2=0)and(YearInfo[i,j].nautic1<>0)and(j>1) then YearInfo[i,j].nautic2:=YearInfo[i,j-1].nautic2;
        if (YearInfo[i,j].civil1=0)and(YearInfo[i,j].civil2<>0)and(j>1) then YearInfo[i,j].civil1:=YearInfo[i,j-1].civil1;
        if (YearInfo[i,j].civil2=0)and(YearInfo[i,j].civil1<>0)and(j>1) then YearInfo[i,j].civil2:=YearInfo[i,j-1].civil2;
      end;

    end;
  end;

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
    dtz,moonrise,moonset,val: double;
    d:Tdatetime;
begin
  ChartYearSeriesSunrise.Clear;
  ChartYearSeriesSunset.Clear;
  ChartYearSeriesAstro1.Clear;
  ChartYearSeriesAstro2.Clear;
  ChartYearSeriesNautic1.Clear;
  ChartYearSeriesNautic2.Clear;
  ChartYearBoxAndWhiskerSeries1.Clear;
  ChartYearBoxAndWhiskerSeries2.Clear;
  // Use same time zome for all the year to ensure graph continuity
  dtz:=GetTimeZoneD(EncodeDate(SpinEditYear.Value,1,1));
  k:=StrToIntDef(GridYear.Cells[1,1],1);
  for i:=1 to 12 do begin
    for j:=1 to MonthDays[IsLeapYear(SpinEditYear.Value)][i] do begin
      d:=EncodeDate(SpinEditYear.Value,i,j);
      if (YearInfo[i,j].sunrise=0)or(YearInfo[i,j].sunset=0) then begin
       // no sun rise/set
       if sign(YearInfo[i,j].sundec)=sign(ObsLatitude) then begin
          // sun always visible
          ChartYearSeriesAstro1.AddXY(d,-12);
          ChartYearSeriesAstro2.AddXY(d,-12);
          ChartYearSeriesNautic1.AddXY(d,-12);
          ChartYearSeriesNautic2.AddXY(d,-12);
          ChartYearSeriesSunrise.AddXY(d,-12);
          ChartYearSeriesSunset.AddXY(d,-12);
       end
       else begin
         // sun do not rise
         if (YearInfo[i,j].astro1=0)or(YearInfo[i,j].astro2=0) then begin
           // no astro twilight
           ChartYearSeriesAstro1.AddXY(d,12);
           ChartYearSeriesAstro2.AddXY(d,-12);
           ChartYearSeriesNautic1.AddXY(d,-12);
           ChartYearSeriesNautic2.AddXY(d,-12);
           ChartYearSeriesSunrise.AddXY(d,-12);
           ChartYearSeriesSunset.AddXY(d,-12);
         end
         else begin
           // astro twilight
           ChartYearSeriesAstro1.AddXY(d,24*frac(YearInfo[i,j].astro1+dtz));
           ChartYearSeriesAstro2.AddXY(d,24*frac(YearInfo[i,j].astro2+dtz)-24);
           if (YearInfo[i,j].nautic1=0)or(YearInfo[i,j].nautic2=0) then begin
             // no nautic twilight
             ChartYearSeriesNautic1.AddXY(d,12);
             ChartYearSeriesNautic2.AddXY(d,-12);
             ChartYearSeriesSunrise.AddXY(d,-12);
             ChartYearSeriesSunset.AddXY(d,-12);
           end
           else begin
             // nautic twilight
             ChartYearSeriesNautic1.AddXY(d,24*frac(YearInfo[i,j].nautic1+dtz));
             ChartYearSeriesNautic2.AddXY(d,24*frac(YearInfo[i,j].nautic2+dtz)-24);
             ChartYearSeriesSunrise.AddXY(d,12);
             ChartYearSeriesSunset.AddXY(d,-12);
           end
         end
       end
      end
      else begin
        // sun rise and set
        ChartYearSeriesSunrise.AddXY(d,24*frac(YearInfo[i,j].sunrise+dtz));
        ChartYearSeriesSunset.AddXY(d,24*frac(YearInfo[i,j].sunset+dtz)-24);
        if (YearInfo[i,j].astro1=0)or(YearInfo[i,j].astro2=0) then begin
          // no astro twilight
          ChartYearSeriesAstro1.AddXY(d,-12);
          ChartYearSeriesAstro2.AddXY(d,-12);
          if (YearInfo[i,j].nautic1=0)or(YearInfo[i,j].nautic2=0) then begin
            // no nautic twilight
            ChartYearSeriesNautic1.AddXY(d,-12);
            ChartYearSeriesNautic2.AddXY(d,-12);
          end
          else begin
            //  nautic twilight
            ChartYearSeriesNautic1.AddXY(d,24*frac(YearInfo[i,j].nautic1+dtz));
            ChartYearSeriesNautic2.AddXY(d,24*frac(YearInfo[i,j].nautic2+dtz)-24);
            end
        end
        else begin
          // all twilight
          ChartYearSeriesAstro1.AddXY(d,24*frac(YearInfo[i,j].astro1+dtz));
          ChartYearSeriesNautic1.AddXY(d,24*frac(YearInfo[i,j].nautic1+dtz));
          ChartYearSeriesNautic2.AddXY(d,24*frac(YearInfo[i,j].nautic2+dtz)-24);
          ChartYearSeriesAstro2.AddXY(d,24*frac(YearInfo[i,j].astro2+dtz)-24);
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
          moonrise:=-12;
          moonset:=12;
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
        moonrise:=24*frac(YearInfo[i,j].moonrise+dtz);
        moonset:=24*frac(YearInfo[i,j].moonset+dtz);
        // in -12 +12 range
        if moonrise>12 then moonrise:=moonrise-24;
        if moonset>12 then moonset:=moonset-24;
        if (moonrise<moonset) then begin
          // single bar
          ChartYearBoxAndWhiskerSeries1.AddXY(d,moonrise,moonrise,moonrise,moonset,moonset,'',RGBToColor(l,l,0));
          ChartYearBoxAndWhiskerSeries2.AddXY(d,0,0,0,0,0);
        end
        else begin
          // split in two bar
          ChartYearBoxAndWhiskerSeries1.AddXY(d,moonrise,moonrise,moonrise,12,12,'',RGBToColor(l,l,0));
          ChartYearBoxAndWhiskerSeries2.AddXY(d,-12,-12,-12,moonset,moonset,'',RGBToColor(l,l,0));
        end;
      end;
    end;
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
  end;
end;

procedure Tf_calclun.GridYearSelection(Sender: TObject; aCol, aRow: Integer);
begin
  SpinEditMonth.Value:=arow;
  SpinEditDay.Value:=acol;
  PageControl1.ActivePage:=TabSheetDay;
end;

procedure Tf_calclun.SetObservatory;
var inif: TMemIniFile;
begin
  inif := Tmeminifile.Create(ConfigFile);
  Obslatitude  := inif.ReadFloat('default', 'Obslatitude', Obslatitude) * deg2rad;
  Obslongitude := -inif.ReadFloat('default', 'Obslongitude', Obslongitude) * deg2rad;
  ObsAltitude := inif.ReadFloat('default', 'Obsaltitude', ObsAltitude) / 1000;
  ObsTZ := inif.ReadString('default', 'ObsTZ', ObsTZ);
  inif.Free;
  tz.TimeZoneFile := ZoneDir + StringReplace(ObsTZ, '/', PathDelim, [rfReplaceAll]);
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
     diam:=60*rad2deg*arctan(2*1737.4/r);
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
         GridMonth.Cells[mcolRise,i]:='Always up'
       else
         GridMonth.Cells[mcolRise,i]:='Never rise';
     end else begin
       if dtr>0 then begin
         dtr:=dtr+YearInfo[month,i].tz;
         DecodeDate(dtr,rsy,rsm,rsd);
         if (rsy=year)and(rsm=month) then begin
           TMoonMonthData(GridMonth.Objects[0,rsd]).trise:=dtr;
           GridMonth.Cells[mcolRise,rsd]:=TimToStr(frac(dtr)*24);
         end;
       end;
       if dts>0 then begin
         dts:=dts+YearInfo[month,i].tz;
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
var dt,startt,endt,dt0: double;
    i: integer;
    Year, Month, Day: Word;
    x,y,z,r,ra,de,pa,llon,llat,slon,slat,colongitude,az,el: SpiceDouble;
    obsref,fixref: ConstSpiceChar;
    img: TBitmap;
    moonriseset, moongraph: boolean;
begin
  year:=SpinEditYear.Value;
  month:=SpinEditMonth.Value;
  day:=SpinEditDay.Value;
  dt0:=EncodeDate(year,month,day);
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
     diam:=60*rad2deg*arctan(2*1737.4/r);
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

  // moon rise or set
  moonriseset:=(YearInfo[month,day].moonrise<>0)or(YearInfo[month,day].moonset<>0);
  // find true rise/set time and day
  if moonriseset then begin
    moongraph:=true;
    startt:=YearInfo[month,day].moonrise;
    if startt=0 then begin
       if day>1 then
         startt:=YearInfo[month,day-1].moonrise
       else if month>1 then
         startt:=YearInfo[month-1,MonthDays[IsLeapYear(year)][month-1]].moonrise
       else
         exit;
    end;
    if trunc(startt+TimeZoneD)>trunc(startt) then begin
       if day>1 then
         startt:=YearInfo[month,day-1].moonrise
       else if month>1 then
         startt:=YearInfo[month-1,MonthDays[IsLeapYear(year)][month-1]].moonrise
       else
         exit;
    end;
    endt:=YearInfo[month,day].moonset;
    if (endt<startt) then begin
       if day<MonthDays[IsLeapYear(year)][month] then
         endt:=YearInfo[month,day+1].moonset
       else if month<12 then
         endt:=YearInfo[month+1,1].moonset
       else
         exit;
    end;
    if endt-startt>1 then begin
      endt:=endt-1;
    end;
    // Rise and set label
    LabelRise2.Caption:=FormatDateTime('mmm d  hh:nn:ss',startt+TimeZoneD)+' '+crlf+FormatDateTime('mmm d  hh:nn:ss',endt+TimeZoneD);
  end
  else begin
    if sign(YearInfo[month,day].moondec)=sign(ObsLatitude) then begin
      startt:=EncodeDate(year,month,day)-TimeZoneD;
      endt:=startt+1;
      moongraph:=true;
    end
    else
      moongraph:=false;
    LabelRise2.Caption:=GridMonth.Cells[mcolRise,day];
  end;
  if (startt=0) or (endt=0) then moongraph:=false;

  // Alt-az graph
  ChartAltAzLineSeries1.Clear;
  ChartAltAzLineSeries2.Clear;
  ChartAltazSunArea.Clear;
  ChartAltAzCivilArea.Clear;
  ChartAltAzNauticArea.Clear;
  ChartAltAzAstroArea.Clear;
  if moongraph then begin
    dt:=startt-1/48;
    repeat
      dt:=dt+1/48;
      if dt>=endt then dt:=endt;
      et:=DateTime2ET(dt);
      if not MoonAltAz(et,ObsLongitude,ObsLatitude,obspos,obsref,az,el) then begin
        StatusLabel.Caption:=SpiceLastError;
        exit;
      end;
      el := el * rad2deg;
      az := rmod(az*rad2deg+360,360);
      if trunc(dt+TimeZoneD)=trunc(startt+TimeZoneD) then
        ChartAltAzLineSeries1.AddXY(frac(dt+TimeZoneD)*24,el)
      else
        ChartAltAzLineSeries2.AddXY(frac(dt+TimeZoneD)*24,el);
    until dt=endt;
  end;
  if (YearInfo[month,day].sunrise=0)or(YearInfo[month,day].sunset=0) then begin
   // no sun rise/set
   if sign(YearInfo[month,day].sundec)=sign(ObsLatitude) then begin
      // sun always visible
     ChartAltazSunArea.AddXY(0,90);
     ChartAltazSunArea.AddXY(24,90);
   end
   else begin
     // sun do not rise
     if (YearInfo[month,day].nautic1=0)or(YearInfo[month,day].nautic2=0) then begin
       // no nautic twilight
     end
     else begin
       // nautic twilight
       ChartAltAzNauticArea.AddXY(frac(YearInfo[month,day].nautic1+TimeZoneD)*24,90);
       ChartAltAzNauticArea.AddXY(frac(YearInfo[month,day].nautic2+TimeZoneD)*24,90);
       if (YearInfo[month,day].civil1=0)or(YearInfo[month,day].civil2=0) then begin
         // no civil twilight
       end
       else begin
         // civil twilight
         ChartAltAzCivilArea.AddXY(frac(YearInfo[month,day].civil1+TimeZoneD)*24,90);
         ChartAltAzCivilArea.AddXY(frac(YearInfo[month,day].civil2+TimeZoneD)*24,90);
       end
     end
   end
  end
  else begin
    // sun rise and set
    ChartAltazSunArea.AddXY((YearInfo[month,day].sunrise+TimeZoneD-dt0)*24,90);
    ChartAltazSunArea.AddXY((YearInfo[month,day].sunset+TimeZoneD-dt0)*24,90);
    if (YearInfo[month,day].civil1=0)or(YearInfo[month,day].civil2=0) then begin
      // no civil twilight
      ChartAltAzCivilArea.AddXY(0,90);
      ChartAltAzCivilArea.AddXY(24,90);
    end
    else begin
      //  civil twilight
      ChartAltAzCivilArea.AddXY((YearInfo[month,day].civil1+TimeZoneD-dt0)*24,90);
      ChartAltAzCivilArea.AddXY((YearInfo[month,day].civil2+TimeZoneD-dt0)*24,90);
      if (YearInfo[month,day].nautic1=0)or(YearInfo[month,day].nautic2=0) then begin
        // no nautic twilight
        ChartAltAzNauticArea.AddXY(0,90);
        ChartAltAzNauticArea.AddXY(24,90);
      end
      else begin
        // nautic twilight
        ChartAltAzNauticArea.AddXY((YearInfo[month,day].nautic1+TimeZoneD-dt0)*24,90);
        ChartAltAzNauticArea.AddXY((YearInfo[month,day].nautic2+TimeZoneD-dt0)*24,90);
        if (YearInfo[month,day].astro1=0)or(YearInfo[month,day].astro2=0) then begin
          // no astro twilight
          ChartAltAzAstroArea.AddXY(0,90);
          ChartAltAzAstroArea.AddXY(24,90);
        end
        else begin
          // astro twilight
          ChartAltAzAstroArea.AddXY((YearInfo[month,day].astro1+TimeZoneD-dt0)*24,90);
          ChartAltAzAstroArea.AddXY((YearInfo[month,day].astro2+TimeZoneD-dt0)*24,90);
        end;
      end;
    end;
  end;

  // Phase image
  img := TBitmap.Create;
  i:=round(TMoonMonthData(GridMonth.Objects[0,day]).lunation);
  ImageListPhase.GetBitmap(i, img);
  DayPhase.Canvas.StretchDraw(DayPhase.ClientRect,img);
  img.Free;

end;

procedure Tf_calclun.ChartAltAzMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var pointi: TPoint;
    pointg: TDoublePoint;
begin
  if (x>5)and(x<(ChartAltAz.Width-5))and(y>5)and(y<(ChartAltAz.Height-5)) then begin
  // mouse position
  pointi.x:=X;
  pointi.y:=Y;
  pointg:=ChartAltAz.ImageToGraph(pointi);
  // show position
  LabelAltAz.Caption:=FormatDateTime('hh:nn',pointg.x/24)+':  Alt:'+FormatFloat(f1,pointg.y);
  end else begin
    LabelAltAz.Caption:='';
  end;
end;

procedure Tf_calclun.ChartYearAxisList1MarkToText(var AText: String; AMark: Double);
begin
 if amark<0 then
   atext:=FormatFloat(f0,24+AMark)
 else
   atext:=FormatFloat(f0,AMark)
end;

procedure Tf_calclun.ChartYearMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var pointi: TPoint;
    pointg: TDoublePoint;
    i: integer;
begin
 if (x>1)and(x<(ChartYear.Width-1))and(y>1)and(y<(ChartYear.Height-1)) then begin
   // mouse position
   pointi.x:=X;
   pointi.y:=Y;
   pointg:=ChartYear.ImageToGraph(pointi);
   LabelChartYear.Caption:='Date: '+FormatDateTime(datestd,round(pointg.y)+(pointg.x+24)/24);
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
  FCurrentMonthGraph:=col;
  case col of
    mcolDay      :  ;
    mcolRa2000   : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
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
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).de2000*rad2deg);
                   end;
                   end;
    mcolRa       : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
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
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).de*rad2deg);
                   end;
                   end;
    mcolDist     : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).dist);
                   end;
                   end;
    mcolDiam     : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).diam);
                   end;
                   end;
    mcolPhase    : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
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
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
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
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).illum);
                   end;
                   end;
    mcolColong   : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
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
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).subsollat*rad2deg);
                   end;
                   end;
    mcolLibrLon  : begin
                   PanelGraph2.Visible:=true;
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
                   chart2.Title.Text.Add('Libration'+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).librlong*rad2deg);
                      Chart2LineSeries1.AddXY(TMoonMonthData(GridMonth.Objects[0,i]).librlong*rad2deg,TMoonMonthData(GridMonth.Objects[0,i]).librlat*rad2deg,inttostr(i));
                   end;
                   end;
    mcolLibrLat  : begin
                   PanelGraph2.Visible:=true;
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
                   chart2.Title.Text.Add('Libration'+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).librlat*rad2deg);
                      Chart2LineSeries1.AddXY(TMoonMonthData(GridMonth.Objects[0,i]).librlong*rad2deg,TMoonMonthData(GridMonth.Objects[0,i]).librlat*rad2deg,inttostr(i));
                   end;
                   end;
    mcolPa       : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
                   for i:=1 to GridMonth.RowCount-1 do begin;
                      Chart1LineSeries1.Add(TMoonMonthData(GridMonth.Objects[0,i]).pa*rad2deg);
                   end;
                   end;
    mcolRise     : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
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
                   end;
    mcolSet      : begin
                   chart1.Title.Text.Add(GridMonth.Cells[col,0]+' '+SpinEditYear.Text+'/'+SpinEditMonth.text);
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
                   end;
  end;
end;

procedure Tf_calclun.DateChange(Sender: TObject);
begin
  if not (fsFirstShow in FormState) then begin
    SpinEditDay.MaxValue:=MonthDays[IsLeapYear(SpinEditYear.Value)][SpinEditMonth.Value];
    DateChangeTimer.Enabled:=false;
    DateChangeTimer.Enabled:=true;
  end;
end;

procedure Tf_calclun.DateChangeTimerTimer(Sender: TObject);
var p1,p2: integer;
begin
  DateChangeTimer.Enabled:=false;
  StatusLabel.Caption:='';
  TimeZoneD := GetTimeZoneD(EncodeDate(SpinEditYear.Value,SpinEditMonth.Value,SpinEditDay.Value));
  if CurYear<>SpinEditYear.Value then ComputeYear; // keep compute order year,month,day
  if (CurrentMonth<>SpinEditMonth.Value)or(CurYear<>SpinEditYear.Value) then ComputeMonth;
  ComputeDay;
  CurYear:=SpinEditYear.Value;
  CurrentMonth:=SpinEditMonth.Value;
  CurrentDay:=SpinEditDay.Value;
end;



procedure Tf_calclun.BtnComputeClick(Sender: TObject);
var dt,st: double;
begin
  st:=now;
  memo1.clear;
  reset_c;
  dt:=DateEdit1.Date+TimeEdit1.Time;
  et:=DateTime2ET(dt);
  dt:=ET2DateTime(et);
  memo1.Lines.Add('Calcul pour TU: '+DateTime2DateIso(dt));
  Position;
  PositionTopo;
  SubEarth;
  SubSolar;
  Illum;
  memo1.Lines.Add('');
  memo1.Lines.Add('Temps de calcul: '+FormatDateTime('ss.zzz',now-st)+' secondes');
end;

procedure Tf_calclun.BtnGraphClick(Sender: TObject);
var p: TPoint;
begin
  p:=point(BtnGraph.Left,BtnGraph.Top+BtnGraph.Height);
  p:=PanelBtnGraph.ClientToScreen(p);
  PopupMenuGraph.PopUp(p.X,p.Y);
end;

procedure Tf_calclun.BtnPrevisionClick(Sender: TObject);
var dt,st: double;
begin
  st:=now;
  memo1.clear;
  reset_c;
  dt:=DateEdit1.Date+TimeEdit1.Time;
  et:=DateTime2ET(dt);
  dt:=ET2DateTime(et);
  memo1.Lines.Add('Calcul depuis TU: '+DateTime2DateIso(dt));
  FindIllum;
  FindColongitude;
  FindMixed;
  FindLibration;
  FindRiseSet;
  FindPhase;
  memo1.Lines.Add('');
  memo1.Lines.Add('Temps de calcul: '+FormatDateTime('ss.zzz',now-st)+' secondes');
end;

procedure Tf_calclun.BtnTodayClick(Sender: TObject);
var Year, Month, Day: Word;
begin
  DecodeDate(now, Year, Month, Day);
  SpinEditYear.Value:=Year;
  SpinEditMonth.Value:=Month;
  SpinEditDay.Value:=Day;
end;

procedure Tf_calclun.Button1Click(Sender: TObject);
var i:integer;
  fixref:ConstSpiceChar;
  tt,x,y,z,r,lon,lat,value: SpiceDouble;
begin

  str2et_c('2021-01-01 00:00:00',tt);
  for i:=0 to 60 do begin
    tt:=tt+SecsPerDay;

    MoonPhase(tt,value);
    memo1.Lines.Add(FormatDateTime(datestd,ET2DateTime(tt))+tab+FormatFloat(f3,rad2deg*value));

    //MoonSubSolarPoint(tt,fixref,x,y,z,r,lon,lat,value);
    //memo1.Lines.Add(FormatDateTime(datestd,ET2DateTime(tt))+tab+FormatFloat(f3,rad2deg*lon)+tab+FormatFloat(f3,rad2deg*value));

  end;
end;

procedure Tf_calclun.Illum;
var phase,lunation,illumination: Double;
  nm, fq, fm, lq: TDateTime;
begin
  memo1.Lines.Add('');
  if MoonPhase(et,phase) then begin
    phase:=rad2deg*phase;
    memo1.Lines.Add('Angle de phase:'+tab+FormatFloat(f3,phase)+'°');
    illumination:=(1+cos(deg2rad*phase))/2;
    memo1.Lines.Add('Illumination:  '+tab+FormatFloat(f0,100*illumination)+'%');
  end
  else begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
  if MoonPhases(DateEdit1.Date+TimeEdit1.Time, nm, fq, fm, lq,lunation) then begin
    memo1.Lines.Add('Lunaison:        '+tab+FormatFloat(f1,lunation)+' jours');
    memo1.Lines.Add('Nouvelle lune:   '+tab+FormatDateTime(datestd,nm));
    memo1.Lines.Add('Premier quartier:'+tab+FormatDateTime(datestd,fq));
    memo1.Lines.Add('Pleine lune:     '+tab+FormatDateTime(datestd,fm));
    memo1.Lines.Add('Dernier quartier:'+tab+FormatDateTime(datestd,lq));
  end
  else begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
end;

procedure Tf_calclun.Position;
var
    x,y,z,r,ra,de,pa: SpiceDouble;
    trueequinox:boolean;
begin
  trueequinox:=false;
  if not MoonGeocentric(et,trueequinox,x,y,z,r,ra,de) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
  MoonPA(et,ra,de,pa);
  pa:=pa*rad2deg;
  ra:=ra*rad2deg/15;
  de:=de*rad2deg;
  memo1.Lines.Add('');
  memo1.Lines.Add('Distance géocentrique:  '+FormatFloat(f3,r)+' km');
  memo1.Lines.Add('Position géocentrique J2000:');
  memo1.Lines.Add(tab+'ra:  '+ARpToStr(ra,5));
  memo1.Lines.Add(tab+'dec: '+DEpToStr(de,5));
  memo1.Lines.Add(tab+'angle de position: '+FormatFloat(f2,pa)+ldeg);
  //memo1.Lines.Add('X Y Z: '+FormatFloat(f3,x)+', '+FormatFloat(f3,y)+', '+FormatFloat(f3,z));

  trueequinox:=true;
  if not MoonGeocentric(et,trueequinox,x,y,z,r,ra,de) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
  ra:=ra*rad2deg/15;
  de:=de*rad2deg;
  memo1.Lines.Add('Position géocentrique apparente:');
  memo1.Lines.Add(tab+'ra:  '+ARpToStr(ra));
  memo1.Lines.Add(tab+'dec: '+DEpToStr(de));
  //memo1.Lines.Add('X Y Z: '+FormatFloat(f3,x)+', '+FormatFloat(f3,y)+', '+FormatFloat(f3,z));
end;

procedure Tf_calclun.PositionTopo;
var obsref: ConstSpiceChar;
    x,y,z,r,ra,de,el,az: SpiceDouble;
    diam: double;
begin
  if not MoonTopocentric(et,false,obspos,obsref,x,y,z,r,ra,de) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
  ra:=ra*rad2deg/15;
  de:=de*rad2deg;
  memo1.Lines.Add('');
  memo1.Lines.Add('Distance topocentrique: '+FormatFloat(f3,r)+' km');
  diam:=60*rad2deg*arctan(2*1737.4/r);
  memo1.Lines.Add('Diamètre apparent:'+tab+FormatFloat(f2,diam)+lmin);
  memo1.Lines.Add('Position topocentrique J2000, référentiel: '+obsref);
  memo1.Lines.Add(tab+'ra:  '+ARpToStr(ra));
  memo1.Lines.Add(tab+'dec: '+DEpToStr(de));
  //memo1.Lines.Add('X Y Z: '+FormatFloat(f3,x)+', '+FormatFloat(f3,y)+', '+FormatFloat(f3,z));

  if not MoonTopocentric(et,true,obspos,obsref,x,y,z,r,ra,de) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
  ra:=ra*rad2deg/15;
  de:=de*rad2deg;
  memo1.Lines.Add('Position topocentrique apparente, référentiel: '+obsref);
  memo1.Lines.Add(tab+'ra:  '+ARpToStr(ra));
  memo1.Lines.Add(tab+'dec: '+DEpToStr(de));
  //memo1.Lines.Add('X Y Z: '+FormatFloat(f3,x)+', '+FormatFloat(f3,y)+', '+FormatFloat(f3,z));

  if not MoonAltAz(et,ObsLongitude,ObsLatitude,obspos,obsref,az,el) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
  el := el * rad2deg;
  az := rmod(az*rad2deg+360,360);
  memo1.Lines.Add(tab+'azimut:'+FormatFloat(f2,az)+ldeg);
  memo1.Lines.Add(tab+'élévation:'+FormatFloat(f2,el)+ldeg);
end;

procedure Tf_calclun.SubEarth;
var fixref:ConstSpiceChar;
    x,y,z,lat,lon,r,llat,llon: SpiceDouble;
begin
  if not MoonSubEarthPoint(et,fixref,x,y,z,r,llon,llat) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
  llon:=llon*rad2deg;
  llat:=llat*rad2deg;
  lon:=rmod(llon+360,360);
  lat:=llat;
  memo1.Lines.Add('');
//  memo1.Lines.Add('Point sub-terrestre, référentiel: '+fixref+',  lon360: '+FormatFloat(f6,lon)+' lat: '+FormatFloat(f6,lat));
  memo1.Lines.Add('Libration géocentrique');
  memo1.Lines.Add(tab+'lon: '+FormatFloat(f4,llon)+ldeg);
  memo1.Lines.Add(tab+'lat: '+FormatFloat(f4,llat)+ldeg);
  //memo1.Lines.Add('X Y Z: '+FormatFloat(f3,x)+', '+FormatFloat(f3,y)+', '+FormatFloat(f3,z));
  if not MoonSubObserverPoint(et,obspos,x,y,z,r,llon,llat) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
  llon:=llon*rad2deg;
  llat:=llat*rad2deg;
  lon:=rmod(llon+360,360);
  lat:=llat;
//  memo1.Lines.Add('Point sub-observateur:  lon360: '+FormatFloat(f6,lon)+' lat: '+FormatFloat(f6,lat));
  memo1.Lines.Add('Libration topocentrique');
  memo1.Lines.Add(tab+'lon: '+FormatFloat(f4,llon)+ldeg);
  memo1.Lines.Add(tab+'lat: '+FormatFloat(f4,llat)+ldeg);
end;

procedure Tf_calclun.SubSolar;
var
  x,y,z,r,lat,lon,colongitude: SpiceDouble;
  fixref:ConstSpiceChar;
begin
  if not MoonSubSolarPoint(et,fixref,x,y,z,r,lon,lat,colongitude) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
  memo1.Lines.Add('');
  memo1.Lines.Add('Point sub-solaire, référentiel: '+fixref);
  memo1.Lines.Add(tab+'lon: '+FormatFloat(f6,lon*rad2deg)+ldeg);
  memo1.Lines.Add(tab+'lat: '+FormatFloat(f6,lat*rad2deg)+ldeg);
  memo1.Lines.Add(tab+'Colongitude: '+FormatFloat(f4,colongitude*rad2deg)+ldeg);
  //memo1.Lines.Add('X Y Z: '+FormatFloat(f6,x)+', '+FormatFloat(f6,y)+', '+FormatFloat(f6,z));
end;

procedure Tf_calclun.FindPhase(realphase:boolean=false);
var Pcnfine,Presult: PSpiceCell;
  et0,et1,x,y: SpiceDouble;
  i,n: integer;
  ok:boolean;
  sti: string;
begin
  reset_c;
  memo1.Lines.Add('');
  memo1.Lines.Add('Pleine lune pour les 12 prochains mois:');
  Pcnfine:=initdoublecell(1);
  Presult:=initdoublecell(2);
  et0:=et;
  et1:=et0+12*30.5*SecsPerDay;
  wninsd_c ( et0, et1, Pcnfine );
  if realphase then
    ok:=MoonSearchPhase(0.0,'LOCMIN',n,Pcnfine,Presult) // extrema of phase angle
  else
    ok:=MoonSearchSunLongitudeDiff(0.0,'LOCMAX',n,Pcnfine,Presult); // official definition using ecliptic longitude
  if not ok then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
  if n>0 then for i:=0 to n-1 do begin
    wnfetd_c(Presult,i,x,y);
    sti:=FormatDateTime(datestd,ET2DateTime(x));
    memo1.Lines.Add(sti);
  end
  else
    memo1.Lines.Add('No interval found');

  scard_c (0, Pcnfine);
  scard_c (0, Presult);
end;

procedure Tf_calclun.FindRiseSet;
var
  refval: SpiceDouble;
  Pcnfine,Presult: PSpiceCell;
  nfind: SpiceInt;
  et0,et1,x,y: SpiceDouble;
  i: integer;
  sti,eni: string;
begin
  reset_c;
  Pcnfine:=initdoublecell(1);
  Presult:=initdoublecell(2);
  et0:=et;
  et1:=et0+7*SecsPerDay;
  wninsd_c ( et0, et1, Pcnfine );
  // expand by one day for the case the moon is already rised at the start of first interval
  wnexpd_c ( SecsPerDay, SecsPerDay, Pcnfine );
  refval := -0.8;  // elevation degree
  if not MoonSearchRiseSet(obspos,ObsLatitude,refval,nfind,Pcnfine,Presult) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
  memo1.Lines.Add('');
  memo1.Lines.Add('Lever et coucher pour la prochaine semaine');
  if nfind>1 then for i:=1 to nfind-2 do begin // skip first and last expanded window
    wnfetd_c(Presult,i,x,y);
    sti:=FormatDateTime(datestd,ET2DateTime(x));
    eni:=FormatDateTime(datestd,ET2DateTime(y));
    memo1.Lines.Add('Lever: '+sti+tab+'Coucher: '+eni);
  end
  else
    memo1.Lines.Add('No interval found');

  scard_c (0, Pcnfine);
  scard_c (0, Presult);

end;

procedure Tf_calclun.FindIllum;
var lon,lat,colong,delta: SpiceDouble;
  xx,yy,zz,r,lo,la,colongitude:SpiceDouble;
  relate,fixref: ConstSpiceChar;
  refval: SpiceDouble;
  pos: TDouble3;
  sc1,sc2,scresult: PSpiceCell;
  nfind: SpiceInt;
  et0,et1,x,y: SpiceDouble;
  i: integer;
  sti,eni: string;
begin
  reset_c;
  // Werner
  lon:=3.293*deg2rad;
  lat:=-28.026*deg2rad;

  pos:=MoonSurfacePos(lon,lat);

  memo1.Lines.Add('');
  memo1.Lines.Add('Recherche sur six mois: X visible, élévation solaire entre 0.7° et 2° sur Werner au lever');

  sc1:=initdoublecell(1);
  sc2:=initdoublecell(2);
  scresult:=initdoublecell(3);
  et0:=et;
  et1:=et0+6*30.5*SecsPerDay;
  wninsd_c ( et0, et1, sc1 );

  fixref := fixrefME;
  relate:='<';
  refval := 89.3 * deg2rad;  // 0.7° sun elevation
  if not MoonSearchIllum(pos,refval,relate,fixref,nfind,sc1,sc2) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;

  relate:='>';
  refval := 88.0 * deg2rad;  // 2° sun elevation
  if not MoonSearchIllum(pos,refval,relate,fixref,nfind,sc2,sc1)then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;

  scard_c (0, sc2);
  for i:=0 to nfind-1 do begin
     wnfetd_c(sc1,i,x,y);
     MoonSubSolarPoint(x,fixref,xx,yy,zz,r,lo,la,colongitude);
     if colongitude>pi then
       wninsd_c(x,y,sc2);
  end;
  nfind:=wncard_c(sc2);
  copy_c(sc2,scresult);

  if RiseSetBox.Checked then begin
    refval := MinElevation.Value;  // elevation degree
    if not MoonSearchRiseSet(obspos,ObsLatitude,refval,nfind,scresult,sc2) then begin
      memo1.Lines.Add(SpiceLastError);
      exit;
    end;
    copy_c(sc2,scresult);
  end;

  if NightEvent.Checked then begin
    refval := -6;  // civil twilight
    if not SearchNight(obspos,ObsLatitude,refval,nfind,scresult,sc2) then begin
      memo1.Lines.Add(SpiceLastError);
      exit;
    end;
    copy_c(sc2,scresult);
  end;

  //memo1.Lines.Add('Référentiel: '+fixref);
  if nfind>0 then for i:=0 to nfind-1 do begin
    wnfetd_c(scresult,i,x,y);
    sti:=FormatDateTime(datestd,ET2DateTime(x));
    eni:=FormatDateTime(datestd,ET2DateTime(y));
    memo1.Lines.Add('Début: '+sti+tab+'Fin: '+eni);
  end
  else
    memo1.Lines.Add('No interval found');

  scard_c (0, sc1);
  scard_c (0, sc2);
  scard_c (0, scresult);
end;


procedure Tf_calclun.FindLibration;
var fixref: ConstSpiceChar;
  coord,relate: ConstSpiceChar;
  refval: SpiceDouble;
  sc1,sc2,sc3,scresult: PSpiceCell;
  et0,et1,x,y: SpiceDouble;
  i,nfind: integer;
  sti,eni: string;
begin

  memo1.Lines.Add('');
  memo1.Lines.Add('Recherche sur six mois: libration en longitude > 5° ou < -5°');
  sc1:=initdoublecell(1);
  sc2:=initdoublecell(2);
  sc3:=initdoublecell(3);
  scresult:=initdoublecell(4);
  et0:=et;
  et1:=et0+6*30.5*SecsPerDay;
  wninsd_c ( et0, et1, sc1 );

  fixref:=fixrefME;
  coord:='LONGITUDE';
  relate:='>';
  refval:=5*deg2rad;
  if not MoonSearchLibration(coord,refval,relate,fixref,nfind,sc1,sc2) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;

  relate:='<';
  refval:=-5*deg2rad;
  if not MoonSearchLibration(coord,refval,relate,fixref,nfind,sc1,sc3) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;

  // add the two result
  wnunid_c(sc2,sc3,scresult);

  //memo1.Lines.Add('Référentiel: '+fixref);

  nfind:=wncard_c(scresult);
  if nfind>0 then for i:=0 to nfind-1 do begin
    wnfetd_c(scresult,i,x,y);
    sti:=FormatDateTime(datestd,ET2DateTime(x));
    eni:=FormatDateTime(datestd,ET2DateTime(y));
    memo1.Lines.Add('Début: '+sti+tab+'Fin: '+eni);
  end
  else
    memo1.Lines.Add('No interval found');

  scard_c (0, sc1);
  scard_c (0, sc2);
  scard_c (0, sc3);
  scard_c (0, scresult);
end;

procedure Tf_calclun.FindMixed;
var fixref: ConstSpiceChar;
  coord,relate: ConstSpiceChar;
  refval: SpiceDouble;
  sc1,sc2,scresult: PSpiceCell;
  et0,et1,x,y: SpiceDouble;
  i,nfind: integer;
  sti,eni: string;
  lon,lat: SpiceDouble;
  pos: TDouble3;
begin
  reset_c;
  // Mare Orientale
  lon:=-94.670*deg2rad;
  lat:=-19.866*deg2rad;
  pos:=MoonSurfacePos(lon,lat);

  memo1.Lines.Add('');
  memo1.Lines.Add('Recherche sur un an: élévation solaire > 5° sur Mare Orientale avec');
  memo1.Lines.Add('libration en longitude < -6.5° et libration en latitude < 0');

  sc1:=initdoublecell(1);
  sc2:=initdoublecell(2);
  scresult:=initdoublecell(3);
  et0:=et;
  et1:=et0+1*365*SecsPerDay;
  wninsd_c ( et0, et1, sc1 );

  fixref := fixrefME;
  relate:='<';
  refval := 85 * deg2rad;  // 5° sun elevation
  if not MoonSearchIllum(pos,refval,relate,fixref,nfind,sc1,sc2) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;


  coord:='LONGITUDE';
  relate:='<';
  refval:=-6.5*deg2rad;
  if not MoonSearchLibration(coord,refval,relate,fixref,nfind,sc2,sc1) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
  coord:='LATITUDE';
  relate:='<';
  refval:=0*deg2rad;
  if not MoonSearchLibration(coord,refval,relate,fixref,nfind,sc1,sc2) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;

  copy_c(sc2,scresult);

  if RiseSetBox.Checked then begin
    refval := MinElevation.Value;  // elevation degree
    if not MoonSearchRiseSet(obspos,ObsLatitude,refval,nfind,scresult,sc1) then begin
      memo1.Lines.Add(SpiceLastError);
      exit;
    end;
    copy_c(sc1,scresult);
  end;

  if NightEvent.Checked then begin
    refval := -6;  // civil twilight
    if not SearchNight(obspos,ObsLatitude,refval,nfind,scresult,sc1) then begin
      memo1.Lines.Add(SpiceLastError);
      exit;
    end;
    copy_c(sc1,scresult);
  end;

  nfind:=wncard_c(scresult);
  if nfind>0 then for i:=0 to nfind-1 do begin
    wnfetd_c(scresult,i,x,y);
    sti:=FormatDateTime(datestd,ET2DateTime(x));
    eni:=FormatDateTime(datestd,ET2DateTime(y));
    memo1.Lines.Add('Début: '+sti+tab+'Fin: '+eni);
  end
  else
    memo1.Lines.Add('No interval found');

  scard_c (0, sc1);
  scard_c (0, sc2);
  scard_c (0, scresult);
end;

procedure Tf_calclun.FindColongitude;
var refval: SpiceDouble;
  colong,delta: SpiceDouble;
  sc1,sc2,sc3,scresult: PSpiceCell;
  nfind: SpiceInt;
  et0,et1,x,y,yy: SpiceDouble;
  i: integer;
  sti,eni: string;
begin
  reset_c;

  colong:=358.0*deg2rad;
  delta:=1.2*deg2rad;

  memo1.Lines.Add('');
  memo1.Lines.Add('Recherche sur six mois: X visible, colongitude de 358° +/- 1.2°');

  sc1:=initdoublecell(1);
  sc2:=initdoublecell(2);
  sc3:=initdoublecell(4);
  scresult:=initdoublecell(3);
  et0:=et;
  et1:=et0+6*30.5*SecsPerDay;
  wninsd_c ( et0, et1, sc1 );

  if not MoonSearchColongitude(colong,delta,nfind,sc1,sc2) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;

  copy_c(sc2,scresult);

  if RiseSetBox.Checked then begin
    refval := MinElevation.Value;  // elevation degree
    if not MoonSearchRiseSet(obspos,ObsLatitude,refval,nfind,scresult,sc2) then begin
      memo1.Lines.Add(SpiceLastError);
      exit;
    end;
    copy_c(sc2,scresult);
  end;

  if NightEvent.Checked then begin
    refval := -6;  // civil twilight
    if not SearchNight(obspos,ObsLatitude,refval,nfind,scresult,sc2) then begin
      memo1.Lines.Add(SpiceLastError);
      exit;
    end;
    copy_c(sc2,scresult);
  end;

  if nfind>0 then for i:=0 to nfind-1 do begin
    wnfetd_c(scresult,i,x,y);
    sti:=FormatDateTime(datestd,ET2DateTime(x));
    eni:=FormatDateTime(datestd,ET2DateTime(y));
    memo1.Lines.Add('Début: '+sti+tab+'Fin: '+eni);
  end
  else
    memo1.Lines.Add('No interval found');

  scard_c (0, sc1);
  scard_c (0, sc2);
  scard_c (0, sc3);
  scard_c (0, scresult);
end;

end.

