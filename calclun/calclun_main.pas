unit calclun_main;

{$mode objfpc}{$H+}

interface

uses cspice, pas_spice, moon_spice, u_util, u_constant, LazSysUtils, SynEdit, TAGraph, TARadialSeries, TASeries, TAFuncSeries, IniFiles,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, EditBtn, Spin, ComCtrls, Grids, Types;

const
    mcolDay=0; mcolRa2000=1; mcolDe2000=2; mcolRa=3; mcolDe=4; mcolDist=5; mcolDiam=6; mcolPhase=7; mcolLunation=8; mcolIllum=9; mcolColong=10; mcolSubSolLat=11; mcolLibrLon=12; mcolLibrLat=13; mcolPa=14; mcolRise=15; mcolSet=16;

type

  TMoonMonthData = class(TObject)
    day: integer;
    date: TDateTime;
    ra2000,de2000,ra,de,dist,diam,phase,lunation,illum,colong,subsollat,librlong,librlat,pa,trise,tset: double;
  end;

  { Tf_calclun }

  Tf_calclun = class(TForm)
    BtnCompute: TButton;
    BtnPrevision: TButton;
    Button1: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart2: TChart;
    Chart2CubicSplineSeries1: TCubicSplineSeries;
    Chart2LineSeries1: TLineSeries;
    GridYear: TStringGrid;
    ImageListPhase: TImageList;
    LabelPhase2: TLabel;
    LabelPhase1: TLabel;
    PanelYRight: TPanel;
    PanelYList: TPanel;
    PanelYphase: TPanel;
    PanelGraph2: TPanel;
    PanelGraph1: TPanel;
    PanelPhase: TPanel;
    ScrollBoxM: TScrollBox;
    ScrollBoxY: TScrollBox;
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
    memo1: TSynEdit;
    SpinEditYear: TSpinEdit;
    SpinEditMonth: TSpinEdit;
    GridMonth: TStringGrid;
    GridPhaseList: TStringGrid;
    TabSheetYear: TTabSheet;
    TabSheetDay: TTabSheet;
    TabSheetMonth: TTabSheet;
    TabSheetTest: TTabSheet;
    TimeEdit1: TTimeEdit;
    procedure BtnComputeClick(Sender: TObject);
    procedure BtnPrevisionClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure DateChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridMonthHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure GridYearDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  private
    et : SpiceDouble;
    obspos: TDouble3;
    ObsLon,ObsLat,ObsAlt: double;
    FCurrentMonthGraph: integer;
    procedure GetAppDir;
    procedure SetKernelsPath;
    procedure ComputeYear;
    procedure ComputeMonth;
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
var Year, Month, Day: Word;
begin
  DefaultFormatSettings.DateSeparator:='-';
  StatusLabel.Caption:='';

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

  DecodeDate(now, Year, Month, Day);
  SpinEditYear.Value:=Year;
  SpinEditMonth.Value:=Month;

  ComputeYear;

  FCurrentMonthGraph:=mcolLibrLon;
  ComputeMonth;

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

procedure Tf_calclun.ComputeYear;
var
  t1,dt, nm, fq, fm, lq,lunation: double;
  i,j,k,r: integer;
  year: Word;
  newmoon: array[0..14] of double;
begin
  StatusLabel.Caption:='';
  GridYear.ColWidths[0]:=100;
  GridYear.Width:=GridYear.ColWidths[0]+(GridYear.ColCount-1)*GridYear.DefaultColWidth;
  for i:=1 to 31 do GridYear.Cells[i,0]:=inttostr(i);
  for i:=1 to 12 do GridYear.Cells[0,i]:=DefaultFormatSettings.LongMonthNames[i];
  GridPhaseList.Cells[0,0]:='New Moon';
  GridPhaseList.Cells[1,0]:='First quarter';
  GridPhaseList.Cells[2,0]:='Full Moon';
  GridPhaseList.Cells[3,0]:='Last quarter';
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
    GridPhaseList.Cells[0,r]:=FormatDateTime('mm/dd hh:nn:ss',nm);
    GridPhaseList.Cells[1,r]:=FormatDateTime('mm/dd hh:nn:ss',fq);
    GridPhaseList.Cells[2,r]:=FormatDateTime('mm/dd hh:nn:ss',fm);
    GridPhaseList.Cells[3,r]:=FormatDateTime('mm/dd hh:nn:ss',lq);
    inc(r);
  until (lq>t1)or(r>=GridPhaseList.RowCount);

  dt:=EncodeDate(year,1,1);
  r:=0;
  for i:=0 to 2 do begin
    if newmoon[i]+29.530588861>=dt then break;
    inc(r);
  end;
  for i:=1 to 12 do begin
    for j:=1 to 31 do begin
      if j<=MonthDays[IsLeapYear(year)][i] then begin
       dt:=EncodeDate(year,i,j);
       if newmoon[r]+29.530588861<dt then inc(r);
       k:=trunc(dt-newmoon[r]);
       GridYear.Cells[j,i]:=inttostr(k);
      end
      else
       GridYear.Cells[j,i]:=' ';
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
    Canvas.TextOut(aRect.Left,aRect.Top+2,Cells[aCol,aRow]);
  end;
end;
end;

procedure Tf_calclun.ComputeMonth;
var dt, nm, fq, fm, lq, dtr, dts: double;
    i, nday: integer;
    Year, Month, yy,mm,dd: Word;
    x,y,z,r,ra,de,pa,llon,llat,slon,slat,colongitude,phase,lunation: SpiceDouble;
    obsref,fixref: ConstSpiceChar;
    refval: SpiceDouble;
    Pcnfine,Presult: PSpiceCell;
    nfind: SpiceInt;
    et0,et1: SpiceDouble;
    data: TMoonMonthData;
begin
  StatusLabel.Caption:='';
  for i:=0 to GridMonth.RowCount-1 do begin
    if GridMonth.Objects[0,i]<>nil then GridMonth.Objects[0,i].Free;
  end;
  year:=SpinEditYear.Value;
  month:=SpinEditMonth.Value;
  nday:=MonthDays[IsLeapYear(year)][month];
  GridMonth.Clear;
  GridMonth.ColCount:=mcolSet+1;
  GridMonth.RowCount:=nday+1;
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
  reset_c;
  ObsLon:=Longitude.Value*deg2rad;
  ObsLat:=Latitude.Value*deg2rad;
  ObsAlt:=Altitude.Value/1000;
  if not ObservatoryPosition(ObsLon,ObsLat,ObsAlt,obspos) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  for i:=1 to nday do begin
     data:=TMoonMonthData.Create;
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
     GridMonth.Objects[0,i]:=data;
  end;

  // Rise and set for the month
  Pcnfine:=initdoublecell(1);
  Presult:=initdoublecell(2);
  dt:=EncodeDate(year,month,1);
  et:=DateTime2ET(dt);
  et0:=et;
  et1:=et0+31*SecsPerDay;
  wninsd_c ( et0, et1, Pcnfine );
  // expand by one day for the case the moon is already rised at the start of first interval
  wnexpd_c ( SecsPerDay, SecsPerDay, Pcnfine );
  refval := -0.8;  // elevation degree
  if not MoonSearchRiseSet(obspos,ObsLat,refval,nfind,Pcnfine,Presult) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  if nfind>1 then for i:=1 to nfind-2 do begin // skip first and last expanded window
    wnfetd_c(Presult,i,x,y);
    dtr:=ET2DateTime(x);
    DecodeDate(dtr,yy,mm,dd);
    if mm=Month then begin
      GridMonth.Cells[mcolRise,dd]:=TimToStr(frac(dtr)*24);
      TMoonMonthData(GridMonth.Objects[0,dd]).trise:=dtr;
    end;
    dts:=ET2DateTime(y);
    DecodeDate(dts,yy,mm,dd);
    if mm=Month then begin
      GridMonth.Cells[mcolSet,dd]:=TimToStr(frac(dts)*24);
      TMoonMonthData(GridMonth.Objects[0,dd]).tset:=dts;
    end;
   end;
  scard_c (0, Pcnfine);
  scard_c (0, Presult);

  // Phases for the month
  dt:=EncodeDate(year,month,1);
  if not MoonPhases(dt, nm, fq, fm, lq,lunation) then begin
    StatusLabel.Caption:=SpiceLastError;
    exit;
  end;
  LabelPhase1.Caption:='New Moon:'+crlf+
                      'First quarter:'+crlf+
                      'Full Moon:'+crlf+
                      'Last quarter:';
  LabelPhase2.Caption:=FormatDateTime(datestd,nm)+crlf+
                      FormatDateTime(datestd,fq)+crlf+
                      FormatDateTime(datestd,fm)+crlf+
                      FormatDateTime(datestd,lq);

  PlotMonthGraph(FCurrentMonthGraph);

end;

procedure Tf_calclun.GridMonthHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
begin
  if IsColumn and (index>0) then
    PlotMonthGraph(index);
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
    ComputeMonth;
    if CurYear<>SpinEditYear.Value then ComputeYear;
    CurYear:=SpinEditYear.Value;
  end;
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
  ObsLon:=Longitude.Value*deg2rad;
  ObsLat:=Latitude.Value*deg2rad;
  ObsAlt:=Altitude.Value/1000;
  if not ObservatoryPosition(ObsLon,ObsLat,ObsAlt,obspos) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
  Position;
  PositionTopo;
  SubEarth;
  SubSolar;
  Illum;
  memo1.Lines.Add('');
  memo1.Lines.Add('Temps de calcul: '+FormatDateTime('ss.zzz',now-st)+' secondes');
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
  ObsLon:=Longitude.Value*deg2rad;
  ObsLat:=Latitude.Value*deg2rad;
  ObsAlt:=Altitude.Value/1000;
  if not ObservatoryPosition(ObsLon,ObsLat,ObsAlt,obspos) then begin
    memo1.Lines.Add(SpiceLastError);
    exit;
  end;
  FindIllum;
  FindColongitude;
  FindMixed;
  FindLibration;
  FindRiseSet;
  FindPhase;
  memo1.Lines.Add('');
  memo1.Lines.Add('Temps de calcul: '+FormatDateTime('ss.zzz',now-st)+' secondes');
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

  if not MoonAltAz(et,ObsLon,ObsLat,obspos,obsref,az,el) then begin
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
  if not MoonSearchRiseSet(obspos,ObsLat,refval,nfind,Pcnfine,Presult) then begin
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
    if not MoonSearchRiseSet(obspos,ObsLat,refval,nfind,scresult,sc2) then begin
      memo1.Lines.Add(SpiceLastError);
      exit;
    end;
    copy_c(sc2,scresult);
  end;

  if NightEvent.Checked then begin
    refval := -6;  // civil twilight
    if not SearchNight(obspos,ObsLat,refval,nfind,scresult,sc2) then begin
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
    if not MoonSearchRiseSet(obspos,ObsLat,refval,nfind,scresult,sc1) then begin
      memo1.Lines.Add(SpiceLastError);
      exit;
    end;
    copy_c(sc1,scresult);
  end;

  if NightEvent.Checked then begin
    refval := -6;  // civil twilight
    if not SearchNight(obspos,ObsLat,refval,nfind,scresult,sc1) then begin
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
    if not MoonSearchRiseSet(obspos,ObsLat,refval,nfind,scresult,sc2) then begin
      memo1.Lines.Add(SpiceLastError);
      exit;
    end;
    copy_c(sc2,scresult);
  end;

  if NightEvent.Checked then begin
    refval := -6;  // civil twilight
    if not SearchNight(obspos,ObsLat,refval,nfind,scresult,sc2) then begin
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

