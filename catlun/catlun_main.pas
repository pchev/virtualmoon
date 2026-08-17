unit catlun_main;

{$mode objfpc}{$H+}

interface

uses
{$ifdef mswindows}
  Windows, ShlObj,
{$endif}
  u_translation, pu_moon, u_constant, u_util, Math, mlb2,
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,  cu_dem,
  ExtCtrls, ComCtrls, StdCtrls, Menus, Buttons, IpHtml;

type

  { Tf_catlun }

  Tf_catlun = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TSpeedButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TSpeedButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit18: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    MainMenu1: TMainMenu;
    Fichier1: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Notebook1: TPageControl;
    Page1: TTabSheet;
    PopupMenu1: TPopupMenu;
    Quit1: TMenuItem;
    PanelMoon: TPanel;
    StatusBar1: TStatusBar;
    StartTimer: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure Notebook1PageChanged(Sender: TObject);
    procedure Quit1Click(Sender: TObject);
    procedure StartTimerTimer(Sender: TObject);
  private
    { private declarations }
    moon1 : TF_moon;
    texturefiles: TStringList;
    curlat, curlon: double;
    quadrantnum,facenum,lunation: integer;
    lockmeasure: boolean;
    csvfr,csven: string;
    Demlib:TdemLibrary;
    dbfr,dben: TMlb2;
    procedure GetAppDir;
    procedure Init;
    procedure RemoveEmptyRow(db:TMlb2);
    procedure MoonClickEvent(Sender: TObject; Button: TMouseButton;
                     Shift: TShiftState; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single);
    procedure MoonMoveEvent(Sender: TObject; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single);
    procedure MoonMeasureEvent(Sender: TObject; m1,m2,m3,m4,m5: string);
  public
    { public declarations }
  end;

var
  f_catlun: Tf_catlun;

const

  numtype=20;
  formationtype: array[0..numtype,0..1] of string = (
    ('Chaîne de cratères','Crater chain'),
    ('Plaine murée','Walled plain'),
    ('Cratère','Crater'),
    ('Craterlet','Craterlet'),
    ('Dôme','Dome'),
    ('Système de dorsales','Wrinkle ridges network'),
    ('Dorsale','Wrinkle ridge'),
    ('Lac','Lake'),
    ('Mer','Sea'),
    ('Plateau','Plateau'),
    ('Montagne','Mountain'),
    ('Chaîne de montagnes','Mountain range'),
    ('Océan','Ocean'),
    ('Marais','Marsh'),
    ('Cap','Cape'),
    ('Rainure','Rille'),
    ('Système de rainures','Rilles network'),
    ('Escarpement','Scarp'),
    ('Golfe','Bay'),
    ('Vallée','Valley'),
    ('Autre','Other'));

  formationdescriptor: array[0..numtype] of string = (
    'CA',
    'AA',
    'AA',
    'AA',
    'DM',
    'DO',
    'DO',
    'LC',
    'ME',
    'ME',
    'MO',
    'MO',
    'OC',
    'PA',
    'PR',
    'RI',
    'RI',
    'RU',
    'SI',
    'VA',
    'XX');

  quadrant : array[0..4,0..1] of string = (
    ('Nord-Est','North-East'),
    ('Nord-Ouest','North-West'),
    ('Sud-Est','South-East'),
    ('Sud-Ouest','South-West'),
    ('',''));

  face : array[0..2,0..1] of string = (
    ('Face visible','Nearside'),
    ('Face cachée','Farside'),
    ('Zone des librations','Librations zone'));

  moondays : array[0..14,0..1] of string = (
    ('Non observable','not observable'),
    ('2 jours après la Nouvelle Lune','2 days after New Moon'),
    ('2 jours après la Nouvelle Lune','2 days after New Moon'),
    ('3 jours après la Nouvelle Lune','3 days after New Moon'),
    ('4 jours après la Nouvelle Lune','4 days after New Moon'),
    ('5 jours après la Nouvelle Lune','5 days after New Moon'),
    ('6 jours après la Nouvelle Lune','6 days after New Moon'),
    ('Premier Quartier','First Quarter'),
    ('1 jour après le Premier Quartier','1 day after First Quarter'),
    ('2 jours après le Premier Quartier','2 days after First Quarter'),
    ('3 jours après le Premier Quartier','3 days after First Quarter'),
    ('4 jours après le Premier Quartier','4 days after First Quarter'),
    ('5 jours après le Premier Quartier','5 days after First Quarter'),
    ('6 jours après le Premier Quartier','6 days after First Quarter'),
    ('Pleine Lune','Full Moon'));

  moondaym : array[0..14,0..1] of string = (
    ('Non observable','not observable'),
    ('1 jour après la Pleine Lune','1 day after Full Moon'),
    ('1 jour après la Pleine Lune','1 day after Full Moon'),
    ('2 jours après la Pleine Lune','2 days after Full Moon'),
    ('3 jours après la Pleine Lune','3 days after Full Moon'),
    ('4 jours après la Pleine Lune','4 days after Full Moon'),
    ('5 jours après la Pleine Lune','5 days after Full Moon'),
    ('6 jours après la Pleine Lune','6 days after Full Moon'),
    ('Dernier Quartier','Last Quarter'),
    ('1 jour après le Dernier Quartier','1 day after Last Quarter'),
    ('2 jours après le Dernier Quartier','2 days after Last Quarter'),
    ('3 jours après le Dernier Quartier','3 days after Last Quarter'),
    ('4 jours après le Dernier Quartier','4 days after Last Quarter'),
    ('5 jours après le Dernier Quartier','5 days after Last Quarter'),
    ('6 jours après le Dernier Quartier','6 days after Last Quarter'));


implementation

uses LazUTF8;

{$R catlun_main.lfm}

{ Tf_catlun }

procedure Tf_catlun.GetAppDir;
var
  buf: string;
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
{$endif}
  privatedir := DefaultPrivateDir;
{$ifdef unix}
  homedir    := expandfilename(DefaultHomeDir);
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
  buf:='';
  SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PIDL);
  SHGetPathFromIDList(PIDL, Folder);
  buf:=systoutf8(Folder);
  buf:=trim(buf);
  homedir:=SafeUTF8ToSys(buf);
{$endif}

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
  Photlun := '"'+bindir + DefaultPhotlun+'"';     // Photlun normally at same location as vma
  Datlun  := '"'+bindir + DefaultDatlun+'"';
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

procedure Tf_catlun.FormCreate(Sender: TObject);
var i: integer;
begin
  {$ifdef mswindows}
  Application.UpdateFormatSettings := False;
  {$endif}
  DefaultFormatSettings.DecimalSeparator := '.';
  DefaultFormatSettings.ThousandSeparator:=' ';
  GetAppDir;
  chdir(appdir);
  DatabaseList:=TStringList.Create;
  ConnectDatabaseList:=TStringList.Create;
  texturefiles:=TStringList.Create;
  for i:=0 to 5 do texturefiles.Add('');
  texturefiles[0]:='WAC';
  texturefiles[1]:='WAC';
  if DirectoryExists(slash(appdir)+slash('Textures')+slash('WAC')+'L3') then
     texturefiles[2]:='WAC';
  if DirectoryExists(slash(appdir)+slash('Textures')+slash('WAC')+'L4') then
     texturefiles[3]:='WAC';
  if DirectoryExists(slash(appdir)+slash('Textures')+slash('WAC')+'L5') then
     texturefiles[4]:='WAC';
  if DirectoryExists(slash(appdir)+slash('Textures')+slash('WAC')+'L6') then
     texturefiles[5]:='WAC';

  demlib:=TdemLibrary.Create;
  demlib.AddPath(slash(Appdir)+slash('data')+slash('dem'));

 moon1:=Tf_moon.Create(PanelMoon);
 moon1.Moon.Align:=alClient;
 moon1.onMoonClick:=@MoonClickEvent;
 moon1.Demlib:=demlib;
 moon1.onMoonMove:=@MoonMoveEvent;
 moon1.onMoonMeasure:=@MoonMeasureEvent;
 moon1.PopUp:=PopupMenu1;
 moon1.TexturePath:=slash(appdir)+slash('Textures');
 moon1.OverlayPath:=slash(appdir)+slash('Textures')+slash('Overlay');
 marksize:=4;
 spritecolor:=clRed;
 markcolor:=clYellow;
 marklabelcolor:=clYellow;
 showmark:=true;
 showlabel:=true;
 labelcenter := True;

 lockmeasure:=false;
 ComboBox1.Clear;
 for i:=0 to numtype do ComboBox1.Items.Add(formationtype[i,0]);
 ComboBox1.ItemIndex:=2;

 u_translation.translate('fr','fr');

  ldeg:='°';
  lmin:='''';
  lsec:='"';

end;

procedure Tf_catlun.Button1Click(Sender: TObject);
var la,lo: string;
    lon360: double;
begin
// generer
try
lockmeasure:=true;
Button4.Down:=false;
Button4.Caption := 'Length';
Button7.Down:=false;
Button7.Caption := 'Width';
moon1.MeasuringDistance := false;
finally
  lockmeasure:=false;
end;
// LUN
la:=formatfloat('##0000',abs(curlat*10000));
if curlat>=0 then la:=la+'N'
             else la:=la+'S';
if curlon>=0 then
  lon360:=curlon
else
  lon360:=360+curlon;
lo:=formatfloat('###0000',lon360*10000);
edit7.Text:=formationdescriptor[ComboBox1.ItemIndex]+la+lo;
// quadrant
if curlat>=0 then begin
   if (curlon>=0)and(curlon<=90) then quadrantnum:=0
   else if (curlon<0)and(curlon>-90) then quadrantnum:=1
   else quadrantnum:=4;
end else begin
   if (curlon>=0)and(curlon<=90) then quadrantnum:=2
   else if (curlon<0)and(curlon>-90) then quadrantnum:=3
   else quadrantnum:=4;
end;
edit8.Text:=quadrant[quadrantnum,0];
// face
if (curlon>=0)and(curlon<=80) then facenum:=0
else if (curlon<0)and(curlon>=-80) then facenum:=0
else if (curlon>80)and(curlon<=100) then facenum:=2
else if (curlon<-80)and(curlon>=-100) then facenum:=2
else facenum:=1;
edit18.Text:=face[facenum,0];
// lunaison
case floor(curlon) of
 -90..-78 : lunation:=14;
 -77..-65 : lunation:=13;
 -64..-52 : lunation:=12;
 -51..-39 : lunation:=11;
 -38..-26 : lunation:=10;
 -25..-13 : lunation:=9;
 -12..-1  : lunation:=8;
  0..12   : lunation:=7;
  13..25  : lunation:=6;
  26..38  : lunation:=5;
  39..51  : lunation:=4;
  52..64  : lunation:=3;
  65..77  : lunation:=2;
  78..90  : lunation:=1;
  else lunation:=0;
end;
edit12.Text:=IntToStr(lunation);
edit13.Text:=moondays[lunation,0];
edit14.Text:=moondaym[lunation,0];
end;

procedure Tf_catlun.Button6Click(Sender: TObject);
var buf:string;
    i,ipos:integer;
    trouve: boolean;
begin
// Recherche
trouve:=false;
buf:=trim(edit4.text);
i:=length(buf);
ipos:=dbfr.GetPosition;
dbfr.GoFirst;
if copy(dbfr.GetData('NAME'),1,i)=buf then begin
  dben.GoFirst;
  trouve:=true;
end
else if dbfr.SeekData('NAME','LIKE',buf+'*') then begin
  dben.Go(dbfr.GetPosition);
  trouve:=true;
end
else begin
  dbfr.Go(ipos);
  dben.Go(ipos);
  ShowMessage('Pas trouvé!');
end;

if trouve then begin
  edit2.Text:=dbfr.GetData('NAME');
  edit7.text:=dbfr.GetData('LUN');
  edit5.Text:=dbfr.GetData('LATI_N');
  curlat:=StrToFloat(edit5.Text);
  edit6.Text:=dbfr.GetData('LONGI_N');
  curlon:=StrToFloat(edit6.Text);
  edit1.Text:=dbfr.GetData('LENGTH_KM');
  edit3.Text:=dbfr.GetData('WIDE_KM');
  edit12.Text:=dbfr.GetData('LUNATION');
  lunation:=StrToInt(edit12.Text);
  edit13.Text:=dbfr.GetData('MOONDAY_S');
  edit14.Text:=dbfr.GetData('MOONDAY_M');

  buf:=dbfr.GetData('IAU_TYPE');
  ComboBox1.ItemIndex:=-1;
  for i:=0 to ComboBox1.Items.Count-1 do
    if formationdescriptor[i]=buf then ComboBox1.ItemIndex:=i;

  buf:=dbfr.GetData('QUADRANT');
  edit8.Text:=buf;
  quadrantnum:=0;
  for i:=0 to 3 do
    if quadrant[i,0]=buf then quadrantnum:=i;

  buf:=dbfr.GetData('FACE');
  edit18.Text:=buf;
  facenum:=0;
  for i:=0 to 2 do
    if face[i,0]=buf then facenum:=i;

  moon1.CenterAt(deg2rad*curlon,deg2rad*curlat);
  moon1.SetMark(deg2rad*curlon,deg2rad*curlat,edit2.Text);

end;
end;

procedure Tf_catlun.Button2Click(Sender: TObject);
var latic,longic : string;
    lon360: double;
begin
// enregistrer
if (trim(edit7.Text)='')or(trim(edit1.Text)='')or(trim(edit2.Text)='') then begin
   ShowMessage('Champs manquant!');
   exit;
end;
if curlon>=0 then
  lon360:=curlon
else
  lon360:=360+curlon;
// français
latic:=LatToStr(curlat);
latic:=StringReplace(latic,'N','Nord',[]);
latic:=StringReplace(latic,'S','Sud',[]);
longic:=LON180ToStr(curlon);
longic:=StringReplace(longic,'E','Est',[]);
longic:=StringReplace(longic,'W','Ouest',[]);
dbfr.SetData('NAME',edit2.text);
dbfr.SetData('LUN',edit7.text);
dbfr.SetData('IAU_TYPE',formationdescriptor[ComboBox1.ItemIndex]);
dbfr.SetData('TYPE',formationtype[ComboBox1.ItemIndex,0]);
dbfr.SetData('LONGI_N',FormatFloat(f5,curlon));
dbfr.SetData('LONGI_N_360',FormatFloat(f5,lon360));
dbfr.SetData('LONGI_C',longic);
dbfr.SetData('LATI_N',FormatFloat(f5,curlat));
dbfr.SetData('LATI_C',latic);
dbfr.SetData('FACE',face[facenum,0]);
dbfr.SetData('QUADRANT',quadrant[quadrantnum,0]);
dbfr.SetData('LENGTH_KM',edit1.Text);
dbfr.SetData('WIDE_KM',edit3.Text);
dbfr.SetData('LUNATION',edit12.Text);
dbfr.SetData('MOONDAY_S',moondays[lunation,0]);
dbfr.SetData('MOONDAY_M',moondaym[lunation,0]);


// anglais
 latic:=LatToStr(curlat);
 latic:=StringReplace(latic,'N','North',[]);
 latic:=StringReplace(latic,'S','South',[]);
 longic:=LON180ToStr(curlon);
 longic:=StringReplace(longic,'E','East',[]);
 longic:=StringReplace(longic,'W','West',[]);
 dben.SetData('NAME',edit2.text);
 dben.SetData('LUN',edit7.text);
 dben.SetData('IAU_TYPE',formationdescriptor[ComboBox1.ItemIndex]);
 dben.SetData('TYPE',formationtype[ComboBox1.ItemIndex,1]);
 dben.SetData('LONGI_N',FormatFloat(f5,curlon));
 dben.SetData('LONGI_N_360',FormatFloat(f5,lon360));
 dben.SetData('LONGI_C',longic);
 dben.SetData('LATI_N',FormatFloat(f5,curlat));
 dben.SetData('LATI_C',latic);
 dben.SetData('FACE',face[facenum,1]);
 dben.SetData('QUADRANT',quadrant[quadrantnum,1]);
 dben.SetData('LENGTH_KM',edit1.Text);
 dben.SetData('WIDE_KM',edit3.Text);
 dben.SetData('LUNATION',edit12.Text);
 dben.SetData('MOONDAY_S',moondays[lunation,1]);
 dben.SetData('MOONDAY_M',moondaym[lunation,1]);

 RemoveEmptyRow(dbfr);
 RemoveEmptyRow(dben);
 dbfr.SaveToCSVFile(csvfr);
 dben.SaveToCSVFile(csven);

 Button3Click(nil);
 moon1.SetMark(0,0,'');
 moon1.RefreshAll;
end;

procedure Tf_catlun.RemoveEmptyRow(db:TMlb2);
var i:integer;
    empty: boolean;
begin
db.GoFirst;
repeat
  empty:=true;
  for i:=1 to db.FieldCount do begin
    if db.GetDataByIndex(i)<>'' then begin
      empty:=false;
      break;
    end;
  end;
  if empty then
    db.RemoveRow
  else
    db.GoNext;
until db.GetPosition>=db.RowCount;

end;

procedure Tf_catlun.Button3Click(Sender: TObject);
begin
// nouveau
if moon1.MeasuringDistance then Button4Click(nil);
edit1.Text:='';
edit2.Text:='';
edit3.Text:='';
edit5.Text:='';
edit6.Text:='';
edit7.Text:='';
edit8.Text:='';
edit12.Text:='';
edit13.Text:='';
edit14.Text:='';
edit18.Text:='';
dbfr.AddRow;
dben.AddRow;

moon1.SetMark(0,0,'');
Button2.Caption:='Enregistrer';
Button3.Caption:='Nouveau';
end;

procedure Tf_catlun.Button4Click(Sender: TObject);
begin
  if lockmeasure then exit;
  try
  lockmeasure:=true;
  Button7.Down:=false;
  Button7.Caption := 'Width';
  moon1.MeasuringDistance := Button4.Down;
  if moon1.MeasuringDistance then begin
    Button4.Caption := 'Fin mesure'
  end else begin
    Button4.Caption := 'Length';
    moon1.SetMark(deg2rad*curlon,deg2rad*curlat,' ');
  end;
  finally
    lockmeasure:=false;
  end;
end;

procedure Tf_catlun.Button7Click(Sender: TObject);
begin
  if lockmeasure then exit;
  try
  lockmeasure:=true;
  Button4.Down:=false;
  Button4.Caption := 'Length';
  moon1.MeasuringDistance := Button7.Down;
  if moon1.MeasuringDistance then begin
    Button7.Caption := 'Fin mesure'
  end else begin
    Button7.Caption := 'Width';
    moon1.SetMark(deg2rad*curlon,deg2rad*curlat,' ');
  end;
  finally
    lockmeasure:=false;
  end;
end;

procedure Tf_catlun.Button5Click(Sender: TObject);
begin
  edit3.Text:=edit1.Text;
end;

procedure Tf_catlun.ComboBox1Change(Sender: TObject);
begin
  if trim(edit7.Text)>'' then Button1Click(nil);
end;

procedure Tf_catlun.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
end;

procedure Tf_catlun.FormResize(Sender: TObject);
begin
  if csDestroying in ComponentState then
    exit;
  if csLoading in ComponentState then
    exit;
  moon1.GLSceneViewer1.Align:=alNone;
  moon1.GLSceneViewer1.Top:=0;
  moon1.GLSceneViewer1.Align:=alClient;
  moon1.RefreshAll;
end;

procedure Tf_catlun.FormShow(Sender: TObject);
begin
  moon1.GLSceneViewer1.Camera:=nil;
  StartTimer.Enabled:=true;
end;

procedure Tf_catlun.MenuItem1Click(Sender: TObject);
begin
  moon1.Zoom:=6;
  moon1.RefreshAll;
end;

procedure Tf_catlun.MenuItem2Click(Sender: TObject);
begin
  moon1.Zoom:=moon1.ZoomMax/2;
  moon1.RefreshAll;
end;

procedure Tf_catlun.MenuItem3Click(Sender: TObject);
begin
  moon1.Zoom:=6;
  moon1.CenterAt(0,0);
  moon1.RefreshAll;
end;

procedure Tf_catlun.MenuItem6Click(Sender: TObject);
begin
  moon1.ShowGrid:=MenuItem6.Checked;
end;

procedure Tf_catlun.Notebook1PageChanged(Sender: TObject);
begin
if (moon1<>nil) and moon1.MeasuringDistance then Button4Click(nil);
if (moon1<>nil) then moon1.RefreshAll;
end;

procedure Tf_catlun.Quit1Click(Sender: TObject);
begin
  close;
end;

procedure Tf_catlun.StartTimerTimer(Sender: TObject);
begin
StartTimer.Enabled:=false;
init;
try
  moon1.GLSceneViewer1.Visible:=true;
  moon1.GLSceneViewer1.Camera:=moon1.GLCamera1;
  Application.ProcessMessages;
  moon1.LibrationMark:=False;
  moon1.Mirror:=False;
  moon1.ShowPhase:=false;
  moon1.LibrLat:=0;
  moon1.LibrLon:=0;
  moon1.Zoom:=6;
  moon1.VisibleSideLock:=false;
  moon1.CenterAt(0,0);
  moon1.RefreshAll;
  moon1.ShowGrid:=false;
finally
 screen.cursor := crArrow;
end;
end;

procedure Tf_catlun.Init;
var f: TextFile;
    hdr: string;
begin
try
  dbfr:=TMlb2.Create;
  dben:=TMlb2.Create;
  csvfr:=slash(Homedir)+'catlunFR.csv';
  csven:=slash(Homedir)+'catlunEN.csv';
  hdr:='NAME;LUN;NAME_TYPE;IAU_TYPE;TYPE;SUBTYPE;PROCESS;PERIOD;PERIOD_SOURCE;GEOLOGY;NAME_DETAIL;'+
       'NAME_ORIGIN;IAU_APPROVAL;LANGRENUS;HEVELIUS;RICCIOLI;WORK;COUNTRY;NATIONLITY;CENTURY_N;CENTURY_C;'+
       'BIRTH_PLACE;BIRTH_DATE;DEATH_PLACE;DEATH_DATE;FACTS;LONGI_N;LONGI_N_360;LONGI_C;LATI_N;LATI_C;FACE;'+
       'QUADRANT;AREA;LENGTH_KM;WIDE_KM;LENGTH_ARCSEC;HEIGHT_M;RAPPORT;GENERAL_1;GENERAL_2;SLOPES;WALLS;FLOOR;'+
       'INTEREST_N;INTEREST_C;LUNATION;MOONDAY_S;MOONDAY_M;DIAM_INST;TH_INSTRU;PR_INSTRU';
  if not FileExists(csvfr) then begin
    AssignFile(f,csvfr);
    rewrite(f);
    writeln(f,hdr);
    closefile(f);
  end;
  if not FileExists(csven) then begin
    AssignFile(f,csven);
    rewrite(f);
    writeln(f,hdr);
    closefile(f);
  end;
  dbfr.LoadFromCSVFile(csvfr);
  dben.LoadFromCSVFile(csven);
  Button3Click(nil);
  moon1.GLSphereMoon.Slices := 180;
  moon1.GLSphereMoon.Stacks := 90;
  moon1.Init;
  moon1.texture:=texturefiles;
  moon1.VisibleSideLock:=true;
  moon1.Labelcolor:=clWhite;
  moon1.SetMark(0, 0, '');
  moon1.zoom:=1;
  moon1.GridSpacing:=1;
  moon1.ShowGrid:=false;
  moon1.ShowFPS:=false;
  Visible:=true;
finally
  screen.cursor := crDefault;
end;
end;

procedure Tf_catlun.MoonMeasureEvent(Sender: TObject; m1,m2,m3,m4,m5: string);
var ll: string;
begin
  ll:=trim(StringReplace(m1,'Km','',[]));
  if button4.Down then
    edit1.Text := ll
  else if button7.Down then
    edit3.Text := ll
end;

procedure Tf_catlun.MoonClickEvent(Sender: TObject; Button: TMouseButton;
                     Shift: TShiftState; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single);
begin
if Button=mbLeft then begin
  if OnMoon then begin
     curlat:=rad2deg*Lat;
     curlon:=rad2deg*Lon;
     edit5.Text:=FormatFloat(f4,curlat);
     edit6.Text:=FormatFloat(f4,curlon);
     edit7.Text:='';
     moon1.SetMark(Lon,Lat,' ');
  end;
end;
if Button=mbRight then begin
    Button4Click(nil); // mesure distance
end;
end;

procedure Tf_catlun.Edit5Change(Sender: TObject);
var buf: string;
    p: TPoint;
begin
if trim(edit5.text)='' then exit;
try
 p:=edit5.CaretPos;
 buf:=StringReplace(edit5.Text,',','.',[]);
 curlat:=StrToFloat(buf);
 if buf<>edit5.Text then begin
    edit5.Text:=buf;
    edit5.CaretPos:=p;
 end;
except
 edit5.Text:='';
end;
end;

procedure Tf_catlun.Edit6Change(Sender: TObject);
var buf: string;
    p: TPoint;
begin
if trim(edit6.text)='' then exit;
try
 p:=edit6.CaretPos;
 buf:=StringReplace(edit6.Text,',','.',[]);
 curlon:=StrToFloat(buf);
 if buf<>edit6.Text then begin
    edit6.Text:=buf;
    edit6.CaretPos:=p;
 end;
except
 edit6.Text:='';
end;
end;

procedure Tf_catlun.Edit1Change(Sender: TObject);
var buf: string;
    p: TPoint;
begin
  if trim(edit1.text)='' then exit;
  p:=edit1.CaretPos;
  buf:=StringReplace(edit1.Text,',','.',[]);
  if buf<>edit1.Text then begin
     edit1.Text:=buf;
     edit1.CaretPos:=p;
  end;
end;

procedure Tf_catlun.Edit3Change(Sender: TObject);
var buf: string;
    p: TPoint;
begin
  if trim(edit3.text)='' then exit;
  p:=edit3.CaretPos;
  buf:=StringReplace(edit3.Text,',','.',[]);
  if buf<>edit3.Text then begin
     edit3.Text:=buf;
     edit3.CaretPos:=p;
  end;
end;

procedure Tf_catlun.MoonMoveEvent(Sender: TObject; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single);
begin
if OnMoon then begin
  statusbar1.Panels[0].Text := 'Longitude: ' + formatfloat(f2, Rad2Deg*Lon);
  statusbar1.Panels[1].Text := 'Latitude: ' + formatfloat(f2, Rad2Deg*Lat);
end else begin
  statusbar1.Panels[0].Text := '';
  statusbar1.Panels[1].Text := '';
end;
end;

end.

