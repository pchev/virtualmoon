unit catlun_main;

{$mode objfpc}{$H+}

interface

uses
{$ifdef mswindows}
  Windows, Registry, ShlObj,
{$endif}
  u_translation, pu_moon, u_constant, u_util, dbutil, u_projection, fmsg, Math, passql, passqlite,
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Menus, IpHtml;

type

  { Tf_catlun }

  Tf_catlun = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    Desc1: TIpHtmlPanel;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    Edit17: TEdit;
    Edit18: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Label1: TLabel;
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
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MainMenu1: TMainMenu;
    Fichier1: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Load1: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Notebook1: TNotebook;
    OpenDialog1: TOpenDialog;
    Page1: TPage;
    Page2: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Quit1: TMenuItem;
    Save1: TMenuItem;
    PanelMoon: TPanel;
    PopupMenu1: TPopupMenu;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    StatusBar1: TStatusBar;
    StartTimer: TTimer;
    procedure Button10Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure Notebook1PageChanged(Sender: TObject);
    procedure Quit1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure StartTimerTimer(Sender: TObject);
  private
    { private declarations }
    moon1 : TF_moon;
    dbfr,dben,dbq: TLiteDB;
    texturefiles: TStringList;
    curlat, curlon: double;
    quadrantnum,facenum,lunation,instnum,labeldensity,wordformat : integer;
    currentid, currentname: string;
    searchl,searchb,currentl,currentb,dummy : double;
    minilabel,modesaisie,modeupdate,shortdesc: boolean;
    currentselection, updateid: string;
    procedure GetAppDir;
    procedure Init;
    procedure GetLabel(Sender: TObject);
    procedure GetSprite(Sender: TObject);
    procedure IdentLB(l, b: single);
    function SearchAtPos(l, b: double): boolean;
    procedure GetHTMLDetail(row: TResultRow; var txt: string);
    procedure SetDescText(const Value: string);
    procedure MoonClickEvent(Sender: TObject; Button: TMouseButton;
                     Shift: TShiftState; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single);
    procedure MoonMoveEvent(Sender: TObject; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single);
    procedure MoonMeasureEvent(Sender: TObject; m1,m2,m3,m4: string);
  public
    { public declarations }
  end;

var
  f_catlun: Tf_catlun;

const
  dbn=99;

  nametype: array[0..1] of string =(
     'Nom non officiel',
     'Non official name');

  formationtype: array[0..19,0..1] of string = (
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
    ('Vallée','Valley'));

  formationdescriptor: array[0..19] of string = (
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
    'VA');

  subtype : array[0..2,0..1] of string = (
  ('Craterlet d''impact primaire','Primary impact craterlet'),
  ('Craterlet d''impact secondaire','Secondary impact craterlet'),
  ('Sous-type non défini','Undefined sub-type'));

  geolprocess : array[0..3,0..1] of string = (
    ('Exogène météoritique','Exogen meteoritical'),
    ('Endogène volcanique','Endogen volcanic'),
    ('Endogène tectonique','Endogen tectonic'),
    ('Processus non défini','Undefined process'));

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

  interestc : array[0..4,0..1] of string = (
    ('Non évalué','Not evaluated'),
    ('Formation peu intéressante ou difficile','Low interest or difficult formation'),
    ('Formation assez intéressante','Pretty interesting formation'),
    ('Formation très intéressante','Very interesting formation'),
    ('Formation exceptionnelle','Exceptional formation'));

  descgeneral : array [0..1,0..1] of string = (
    ('Formation circulaire en forme de bol','Bowl shaped formation'),
    ('',''));

  descslopes : array [0..1,0..1] of string = (
    ('Versants peu escarpés','Pretty steep slopes'),
    ('',''));

  descwalls : array [0..1,0..1] of string = (
    ('Murailles assez profondes','Pretty high walls'),
    ('',''));

  descfloor : array [0..1,0..1] of string = (
    ('Fond arrondi peu étendu','Rounded floor'),
    ('',''));

  diaminst : array[0..6] of string = (
    '5', '30', '50', '100', '150', '250', '999');

  thinstru : array[0..6,0..1] of string = (
    ('Oeil nu','Naked eye'),
    ('Jumelles x10','10x binoculars'),
    ('Lunette 50 mm','50 mm refractor'),
    ('Télescope 100 mm','100 mm reflector'),
    ('Télescope 150 mm','150 mm reflector'),
    ('Télescope 250 mm','250 mm reflector'),
    ('Non visible','Not visible'));

  prinstru : array[0..6,0..1] of string = (
    ('Jumelles x10','10x binoculars'),
    ('Lunette 50 mm','50 mm refractor'),
    ('Télescope 100 mm','100 mm refractor'),
    ('Télescope 200 mm','200 mm reflector'),
    ('Télescope 300 mm','300 mm reflector'),
    ('Télescope 500 mm','500 mm reflector'),
    ('Non visible','Not visible'));

implementation

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
  homedir    := expandfilename(DefaultHome);
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
  GetAppDir;
  chdir(appdir);
  texturefiles:=TStringList.Create;
  for i:=0 to 5 do texturefiles.Add('');
  texturefiles[0]:='Airbrush';
  texturefiles[1]:='Airbrush';
  if DirectoryExists(slash(appdir)+slash('Textures')+slash('Lopam')+'L3') then
     texturefiles[2]:='Lopam';
  if DirectoryExists(slash(appdir)+slash('Textures')+slash('Lopam')+'L4') then
     texturefiles[3]:='Lopam';
  if DirectoryExists(slash(appdir)+slash('Textures')+slash('Lopam')+'L5') then
     texturefiles[4]:='Lopam';
  if DirectoryExists(slash(appdir)+slash('Textures')+slash('Lopam')+'L6') then
     texturefiles[5]:='Lopam';

 moon1:=Tf_moon.Create(PanelMoon);
 moon1.Moon.Align:=alClient;
 moon1.onMoonClick:=@MoonClickEvent;
 moon1.onMoonMove:=@MoonMoveEvent;
 moon1.onMoonMeasure:=@MoonMeasureEvent;
// moon1.onGetMsg:=GetMsg;
 moon1.onGetLabel:=@GetLabel;
 moon1.onGetSprite:=@GetSprite;
 moon1.PopUp:=PopupMenu1;
 moon1.TexturePath:=slash(appdir)+slash('Textures');
 moon1.OverlayPath:=slash(appdir)+slash('Textures')+slash('Overlay');

// moon1.AntiAliasing:=antialias;

marksize:=2;
spritecolor:=clYellow;
markcolor:=clRed;
showmark:=true;
showlabel:=true;
currentselection:='DBN in ('+inttostr(dbn)+')';
minilabel:=true;
LabelDensity := 400;
labelcenter := True;

modesaisie:=true;
modeupdate:=false;
shortdesc:=false;
wordformat:=1;

ComboBox1.Clear;
for i:=0 to 19 do ComboBox1.Items.Add(formationtype[i,0]);
ComboBox1.ItemIndex:=3;

ComboBox7.Clear;
for i:=0 to 2 do ComboBox7.Items.Add(subtype[i,0]);
ComboBox7.ItemIndex:=0;

ComboBox8.Clear;
for i:=0 to 3 do ComboBox8.Items.Add(geolprocess[i,0]);
ComboBox8.ItemIndex:=0;

ComboBox2.Clear;
for i:=0 to 1 do ComboBox2.Items.Add(descgeneral[i,0]);
ComboBox2.ItemIndex:=0;

ComboBox3.Clear;
for i:=0 to 1 do ComboBox3.Items.Add(descslopes[i,0]);
ComboBox3.ItemIndex:=0;

ComboBox4.Clear;
for i:=0 to 1 do ComboBox4.Items.Add(descwalls[i,0]);
ComboBox4.ItemIndex:=0;

ComboBox5.Clear;
for i:=0 to 1 do ComboBox5.Items.Add(descfloor[i,0]);
ComboBox5.ItemIndex:=0;

ComboBox6.Clear;
for i:=0 to 4 do ComboBox6.Items.Add(interestc[i,0]);
ComboBox6.ItemIndex:=1;

u_translation.translate('fr','fr');

end;

procedure Tf_catlun.GetLabel(Sender: TObject);
var lmin,lmax,bmin,bmax: single;
    w, wmin, wfact, l1, b1: single;
    miniok:    boolean;
    nom, lun, let:  string;
    j: integer;
begin
// Labels
  if showlabel then
  begin
  // get search boundaries
    Tf_moon(Sender).GetBounds(lmin,lmax,bmin,bmax);
  // minimal feature size
    if minilabel then
      wfact := 0.5
    else
      wfact := 1;
    LabelDensity := maxintvalue([100, LabelDensity]);
    if (Tf_moon(Sender).Zoom >= 8) and (Tf_moon(Sender).Zoom >= (Tf_moon(Sender).ZoomMax-5)) then
      wmin := -1
    else
      wmin := MinValue([650.0, 3 * LabelDensity / (Tf_moon(Sender).Zoom * Tf_moon(Sender).Zoom)]);
    dbfr.Query('select NAME,LONGIN,LATIN,WIDEKM,WIDEMI,LENGTHKM,LENGTHMI,LUN from moon' +
      ' where DBN in (' + sidelist + ')' + ' and LONGIN > ' +
      formatfloat(f2, rad2deg*lmin) + ' and LONGIN < ' + formatfloat(f2, rad2deg*lmax) +
      ' and LATIN > ' + formatfloat(f2, rad2deg*bmin) +
      ' and LATIN < ' + formatfloat(f2, rad2deg*bmax) +
      ' and (WIDEKM=0 or WIDEKM>=' + formatfloat(f2, (wmin * wfact) / 2.5) + ')' +
      ' ;');
    for j := 0 to dbfr.RowCount - 1 do
    begin
      l1 := dbfr.Results[j].Format[1].AsFloat;
      b1 := dbfr.Results[j].Format[2].AsFloat;
      w  := dbfr.Results[j].Format[3].AsFloat;
      if w <= 0 then
        w := 1.67 * dbfr.Results[j].Format[4].AsFloat;
      if (w > 200) and (abs(l1) > 90) then
        w := 2.5 * w; // moins de grosse formation face cachee
      if w < (wmin * wfact) then
        continue;
      nom := trim(dbfr.Results[j][0]);
      lun := trim(dbfr.Results[j][7]);
      if minilabel then
      begin
        miniok := True;
        if copy(nom, 1, 6) = 'DOMES ' then
          miniok := False;
        if copy(nom, 1, 5) = 'DOME ' then
          miniok := False;
        if copy(nom, 1, 6) = 'DORSA ' then
          miniok := False;
        if copy(nom, 1, 5) = 'RIMA ' then
          miniok := False;
        let      := trim(copy(nom, length(nom) - 1, 2));
        if miniok and (length(let) = 1) and (let >= 'A') and (let <= 'Z') then
        begin
          nom := let;
        end
        else
        begin
          if w < (wmin) then
            continue;
        end;
      end;
      if lun<>nom then nom:=capitalize(nom);
      Tf_moon(Sender).AddLabel(deg2rad*l1,deg2rad*b1,nom);
     end;
  end;
end;

procedure Tf_catlun.GetSprite(Sender: TObject);
var lmin,lmax,bmin,bmax: single;
    l1, b1: single;
    j: integer;
begin
// Mark selection
  if (currentselection<>'') then
  begin
    // get search boundaries
    Tf_moon(Sender).GetBounds(lmin,lmax,bmin,bmax);
    dbfr.Query('select LONGIN,LATIN from moon where ' + currentselection +
      ' and LONGIN > ' + formatfloat(f2, rad2deg*lmin) +
      ' and LONGIN < ' + formatfloat(f2, rad2deg*lmax) +
      ' and LATIN > ' + formatfloat(f2, rad2deg*bmin) +
      ' and LATIN < ' + formatfloat(f2, rad2deg*lmax) +
      ' ORDER BY WIDEKM DESC ' + ' ;');
    for j := 0 to dbfr.RowCount - 1 do
    begin
      l1 := dbfr.Results[j].Format[0].AsFloat;
      b1 := dbfr.Results[j].Format[1].AsFloat;
      Tf_moon(Sender).AddSprite(deg2rad*l1,deg2rad*b1);
    end;
  end;
end;

procedure Tf_catlun.Button1Click(Sender: TObject);
var la,lo: string;
    i : integer;
    n : double;
begin
// generer
// LUN
la:=formatfloat('0000',abs(curlat*100));
if curlat>=0 then la:=la+'N'
             else la:=la+'S';
lo:=formatfloat('00000',abs(curlon*100));
if curlon>=0 then lo:=lo+'E'
             else lo:=lo+'W';
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
// instrument
val(edit1.Text,n,i);
if i=0 then begin
  case trunc(10*n) of
    0..8 : instnum:=6;
    9..14 : instnum:=5;
    15..21: instnum:=4;
    22..44: instnum:=3;
    45..109: instnum:=2;
    110..1099: instnum:=1;
    1100..99999: instnum:=0;
    else instnum:=6;
  end;
  edit15.Text:=diaminst[instnum];
  edit16.Text:=thinstru[instnum,0];
  edit17.Text:=prinstru[instnum,0];
end;

end;

procedure Tf_catlun.Button10Click(Sender: TObject);
var cmd,buf:string;
    i:integer;
begin
// Modifier
if moon1.MeasuringDistance then Button4Click(nil);
cmd:='select * from moon where id='+currentid+' and dbn='+inttostr(dbn)+';';
dbfr.Query(cmd);
if dbfr.RowCount=0 then begin
   ShowMessage('Pas de formation Nonames selectionnée.');
   exit;
end;
modeupdate:=true;
updateid:=currentid;
curlat:=dbfr.Results[0].ByField['LATIN'].AsFloat;
curlon:=dbfr.Results[0].ByField['LONGIN'].AsFloat;
edit1.Text:=dbfr.Results[0].ByField['LENGTHKM'].AsString;
edit2.Text:=dbfr.Results[0].ByField['LENGTHMI'].AsString;
edit3.Text:=dbfr.Results[0].ByField['WIDEKM'].AsString;
edit4.Text:=dbfr.Results[0].ByField['WIDEMI'].AsString;
edit5.Text:=dbfr.Results[0].ByField['LATIN'].AsString;
edit6.Text:=dbfr.Results[0].ByField['LONGIN'].AsString;
edit7.Text:=dbfr.Results[0].ByField['LUN'].AsString;
edit8.Text:=dbfr.Results[0].ByField['QUADRANT'].AsString;
edit9.Text:=dbfr.Results[0].ByField['AREA'].AsString;
edit10.Text:=dbfr.Results[0].ByField['RUKL'].AsString;
edit11.Text:=dbfr.Results[0].ByField['RUKLC'].AsString;
edit12.Text:=dbfr.Results[0].ByField['LUNATION'].AsString;
edit13.Text:=dbfr.Results[0].ByField['MOONDAYS'].AsString;
edit14.Text:=dbfr.Results[0].ByField['MOONDAYM'].AsString;
edit15.Text:=dbfr.Results[0].ByField['DIAMINST'].AsString;
edit16.Text:=dbfr.Results[0].ByField['THINSTRU'].AsString;
edit17.Text:=dbfr.Results[0].ByField['PRINSTRU'].AsString;
edit18.Text:=dbfr.Results[0].ByField['FACE'].AsString;
buf:=dbfr.Results[0].ByField['TYPE'].AsString;
ComboBox1.ItemIndex:=-1;
for i:=0 to ComboBox1.Items.Count-1 do
  if ComboBox1.Items[i]=buf then ComboBox1.ItemIndex:=i;
buf:=dbfr.Results[0].ByField['SUBTYPE'].AsString;
ComboBox7.ItemIndex:=-1;
for i:=0 to ComboBox7.Items.Count-1 do
  if ComboBox7.Items[i]=buf then ComboBox7.ItemIndex:=i;
buf:=dbfr.Results[0].ByField['PROCESS'].AsString;
ComboBox8.ItemIndex:=-1;
for i:=0 to ComboBox8.Items.Count-1 do
  if ComboBox8.Items[i]=buf then ComboBox8.ItemIndex:=i;
buf:=dbfr.Results[0].ByField['QUADRANT'].AsString;
quadrantnum:=-1;
for i:=0 to 4 do
  if quadrant[i,0]=buf then quadrantnum:=i;
buf:=dbfr.Results[0].ByField['GENERAL'].AsString;
ComboBox2.ItemIndex:=-1;
for i:=0 to ComboBox2.Items.Count-1 do
  if ComboBox2.Items[i]=buf then ComboBox2.ItemIndex:=i;
buf:=dbfr.Results[0].ByField['SLOPES'].AsString;
ComboBox3.ItemIndex:=-1;
for i:=0 to ComboBox3.Items.Count-1 do
  if ComboBox3.Items[i]=buf then ComboBox3.ItemIndex:=i;
buf:=dbfr.Results[0].ByField['WALLS'].AsString;
ComboBox4.ItemIndex:=-1;
for i:=0 to ComboBox4.Items.Count-1 do
  if ComboBox4.Items[i]=buf then ComboBox4.ItemIndex:=i;
buf:=dbfr.Results[0].ByField['FLOOR'].AsString;
ComboBox5.ItemIndex:=-1;
for i:=0 to ComboBox5.Items.Count-1 do
  if ComboBox5.Items[i]=buf then ComboBox5.ItemIndex:=i;
buf:=dbfr.Results[0].ByField['INTERESTN'].AsString;
ComboBox6.ItemIndex:=strtointdef(buf,-1);
lunation:=StrToIntDef(edit12.Text,-1);
buf:=dbfr.Results[0].ByField['DIAMINST'].AsString;
instnum:=-1;
for i:=0 to 6 do
  if diaminst[i]=buf then instnum:=i;

Notebook1.ActivePage:=page1.Caption;

moon1.SetMark(deg2rad*curlon,deg2rad*curlat,'Modification');
Button2.Caption:='Modifier';
Button3.Caption:='Abandon';

end;

procedure Tf_catlun.Button2Click(Sender: TObject);
var cmd,latic,longic : string;
      y, m, d: word;
const
    ndf='Non déterminé';
    nde='Not determined';
    nnf='Non nommé';
    nne='Not named';
    naf='';  //'Sans objet';
    nae='';  //'N/A';
    nmf='Non mesuré';
    nme='Not measured';
    vif='Hauteur inconnue. Visualisation impossible';
    vie='Unknown height. Impossible drawing.';
    cif='Hauteur inconnue. Calcul impossible.';
    cie='Unknown height. Impossible calculation.';
begin
// enregistrer
if (trim(edit7.Text)='')or(trim(edit1.Text)='') then begin
   ShowMessage('Champs manquant!');
   exit;
end;
if not modeupdate then begin
  cmd:='select count(*) from moon where LUN="'+edit7.text+'";';
  dbfr.Query(cmd);
  if dbfr.Result<>'0' then begin
     ShowMessage('Ce LUN existe déjà dans la base FR! Effacez le avant de pouvoir l''enregistrer');
     exit;
  end;
  dben.Query(cmd);
  if dben.Result<>'0' then begin
     ShowMessage('Ce LUN existe déjà dans la base EN! Effacez le avant de pouvoir l''enregistrer');
     exit;
  end;
end;
decodedate(now, y, m, d);
// français
latic:=formatfloat(f2,abs(curlat))+'°';
if curlat>=0 then latic:=latic+' Nord'
             else latic:=latic+' Sud';
longic:=formatfloat(f2,abs(curlon))+'°';
if curlon>=0 then longic:=longic+' Est'
             else longic:=longic+' Ouest';
if modeupdate then
  cmd:='update moon set '+
    'NAME="'+edit7.text+'",'+
    'LUN="'+edit7.text+'",'+
    'NAMETYPE="'+nametype[0]+'",'+
    'TYPE="'+formationtype[ComboBox1.ItemIndex,0]+'",'+
    'SUBTYPE="'+subtype[ComboBox7.ItemIndex,0]+'",'+
    'PROCESS="'+geolprocess[ComboBox8.ItemIndex,0]+'",'+
    'NAMEDETAIL="'+edit7.text+'",'+
    'LONGIN="'+edit6.Text+'",'+
    'LONGIC="'+longic+'",'+
    'LATIN="'+edit5.Text+'",'+
    'LATIC="'+latic+'",'+
    'FACE="'+face[facenum,0]+'",'+ // face
    'QUADRANT="'+quadrant[quadrantnum,0]+'",'+
    'AREA="'+edit9.Text+'",'+  // area
    'RUKL="'+edit10.Text+'",'+ // rukl
    'RUKLC="'+edit11.Text+'",'+
    'LENGTHKM="'+edit1.Text+'",'+
    'WIDEKM="'+edit3.Text+'",'+
    'LENGTHMI="'+edit2.Text+'",'+
    'WIDEMI="'+edit4.Text+'",'+
    'GENERAL="'+descgeneral[ComboBox2.ItemIndex,0]+'",'+
    'SLOPES="'+descslopes[ComboBox3.ItemIndex,0]+'",'+
    'WALLS="'+descwalls[ComboBox4.ItemIndex,0]+'",'+
    'FLOOR="'+descfloor[ComboBox5.ItemIndex,0]+'",'+
    'INTERESTN="'+inttostr(ComboBox6.ItemIndex)+'",'+
    'INTERESTC="'+interestc[ComboBox6.ItemIndex,0]+'",'+
    'LUNATION="'+edit12.Text+'",'+
    'MOONDAYS="'+moondays[lunation,0]+'",'+
    'MOONDAYM="'+moondaym[lunation,0]+'",'+
    'DIAMINST="'+edit15.Text+'",'+
    'THINSTRU="'+thinstru[instnum,0]+'",'+
    'PRINSTRU="'+prinstru[instnum,0]+'"'+
    ' where id='+updateid+' ;'
else
  cmd:='insert into moon values(NULL,'+
    inttostr(dbn)+','+
    '"'+edit7.text+'",'+
    '"'+edit7.text+'",'+
    '"'+nametype[0]+'",'+
    '"'+formationtype[ComboBox1.ItemIndex,0]+'",'+
    '"'+subtype[ComboBox7.ItemIndex,0]+'",'+
    '"'+ndf+'",'+
    '"'+geolprocess[ComboBox8.ItemIndex,0]+'",'+
    '"'+naf+'",'+  //geology
    '"'+edit7.text+'",'+
    '"'+'Legrand '+inttostr(y)+'",'+
    '"'+nnf+'",'+
    '"'+nnf+'",'+
    '"'+nnf+'",'+
    '"'+naf+'",'+
    '"'+naf+'",'+
    '"'+naf+'",'+
    '"'+naf+'",'+
    '"'+naf+'",'+
    '"'+naf+'",'+
    '"'+naf+'",'+
    '"'+naf+'",'+
    '"'+naf+'",'+
    '"'+naf+'",'+
    '"'+edit6.Text+'",'+
    '"'+longic+'",'+
    '"'+edit5.Text+'",'+
    '"'+latic+'",'+
    '"'+face[facenum,0]+'",'+ // face
    '"'+quadrant[quadrantnum,0]+'",'+
    '"'+edit9.Text+'",'+  // area
    '"'+edit10.Text+'",'+ // rukl
    '"'+edit11.Text+'",'+
    '"'+ndf+'",'+
    '"'+ndf+'",'+
    '"'+ndf+'",'+
    '"'+ndf+'",'+
    '"'+ndf+'",'+
    '"'+edit1.Text+'",'+
    '"'+edit3.Text+'",'+
    '"'+edit2.Text+'",'+
    '"'+edit4.Text+'",'+
    '"'+nmf+'",'+
    '"'+nmf+'",'+
    '"'+cif+'",'+
    '"'+vif+'",'+
    '"'+descgeneral[ComboBox2.ItemIndex,0]+'",'+
    '"'+descslopes[ComboBox3.ItemIndex,0]+'",'+
    '"'+descwalls[ComboBox4.ItemIndex,0]+'",'+
    '"'+descfloor[ComboBox5.ItemIndex,0]+'",'+
    '"'+naf+'",'+  //tips
    '"'+inttostr(ComboBox6.ItemIndex)+'",'+
    '"'+interestc[ComboBox6.ItemIndex,0]+'",'+
    '"'+edit12.Text+'",'+
    '"'+moondays[lunation,0]+'",'+
    '"'+moondaym[lunation,0]+'",'+
    '"'+edit15.Text+'",'+
    '"'+thinstru[instnum,0]+'",'+
    '"'+prinstru[instnum,0]+'"'+
    ');';

 dbfr.Query(cmd);

// anglais
 latic:=formatfloat(f2,abs(curlat))+'°';
 if curlat>=0 then latic:=latic+' North'
             else latic:=latic+' South';
 longic:=formatfloat(f2,abs(curlon))+'°';
 if curlon>=0 then longic:=longic+' East'
             else longic:=longic+' West';
 if modeupdate then
  cmd:='update moon set '+
    'NAME="'+edit7.text+'",'+
    'LUN="'+edit7.text+'",'+
    'NAMETYPE="'+nametype[1]+'",'+
    'TYPE="'+formationtype[ComboBox1.ItemIndex,1]+'",'+
    'SUBTYPE="'+subtype[ComboBox7.ItemIndex,1]+'",'+
    'PROCESS="'+geolprocess[ComboBox8.ItemIndex,1]+'",'+
    'NAMEDETAIL="'+edit7.text+'",'+
    'LONGIN="'+edit6.Text+'",'+
    'LONGIC="'+longic+'",'+
    'LATIN="'+edit5.Text+'",'+
    'LATIC="'+latic+'",'+
    'FACE="'+face[facenum,1]+'",'+ // face
    'QUADRANT="'+quadrant[quadrantnum,1]+'",'+
    'AREA="'+edit9.Text+'",'+  // area
    'RUKL="'+edit10.Text+'",'+ // rukl
    'RUKLC="'+edit11.Text+'",'+
    'LENGTHKM="'+edit1.Text+'",'+
    'WIDEKM="'+edit3.Text+'",'+
    'LENGTHMI="'+edit2.Text+'",'+
    'WIDEMI="'+edit4.Text+'",'+
    'GENERAL="'+descgeneral[ComboBox2.ItemIndex,1]+'",'+
    'SLOPES="'+descslopes[ComboBox3.ItemIndex,1]+'",'+
    'WALLS="'+descwalls[ComboBox4.ItemIndex,1]+'",'+
    'FLOOR="'+descfloor[ComboBox5.ItemIndex,1]+'",'+
    'INTERESTN="'+inttostr(ComboBox6.ItemIndex)+'",'+
    'INTERESTC="'+interestc[ComboBox6.ItemIndex,1]+'",'+
    'LUNATION="'+edit12.Text+'",'+
    'MOONDAYS="'+moondays[lunation,1]+'",'+
    'MOONDAYM="'+moondaym[lunation,1]+'",'+
    'DIAMINST="'+edit15.Text+'",'+
    'THINSTRU="'+thinstru[instnum,1]+'",'+
    'PRINSTRU="'+prinstru[instnum,1]+'"'+
    ' where id='+updateid+' ;'
 else
  cmd:='insert into moon values(NULL,'+
    inttostr(dbn)+','+
    '"'+edit7.text+'",'+
    '"'+edit7.text+'",'+
    '"'+nametype[1]+'",'+
    '"'+formationtype[ComboBox1.ItemIndex,1]+'",'+
    '"'+subtype[ComboBox7.ItemIndex,1]+'",'+
    '"'+nde+'",'+
    '"'+geolprocess[ComboBox8.ItemIndex,1]+'",'+
    '"'+nae+'",'+  //geology
    '"'+edit7.text+'",'+
    '"'+'Legrand '+inttostr(y)+'",'+
    '"'+nne+'",'+
    '"'+nne+'",'+
    '"'+nne+'",'+
    '"'+nae+'",'+
    '"'+nae+'",'+
    '"'+nae+'",'+
    '"'+nae+'",'+
    '"'+nae+'",'+
    '"'+nae+'",'+
    '"'+nae+'",'+
    '"'+nae+'",'+
    '"'+nae+'",'+
    '"'+nae+'",'+
    '"'+edit6.Text+'",'+
    '"'+longic+'",'+
    '"'+edit5.Text+'",'+
    '"'+latic+'",'+
    '"'+face[facenum,1]+'",'+ // face
    '"'+quadrant[quadrantnum,1]+'",'+
    '"'+edit9.Text+'",'+  // area
    '"'+edit10.Text+'",'+ // rukl
    '"'+edit11.Text+'",'+
    '"'+nde+'",'+
    '"'+nde+'",'+
    '"'+nde+'",'+
    '"'+nde+'",'+
    '"'+nde+'",'+
    '"'+edit1.Text+'",'+
    '"'+edit3.Text+'",'+
    '"'+edit2.Text+'",'+
    '"'+edit4.Text+'",'+
    '"'+nme+'",'+
    '"'+nme+'",'+
    '"'+cie+'",'+
    '"'+vie+'",'+
    '"'+descgeneral[ComboBox2.ItemIndex,1]+'",'+
    '"'+descslopes[ComboBox3.ItemIndex,1]+'",'+
    '"'+descwalls[ComboBox4.ItemIndex,1]+'",'+
    '"'+descfloor[ComboBox5.ItemIndex,1]+'",'+
    '"'+nae+'",'+  //tips
    '"'+inttostr(ComboBox6.ItemIndex)+'",'+
    '"'+interestc[ComboBox6.ItemIndex,1]+'",'+
    '"'+edit12.Text+'",'+
    '"'+moondays[lunation,1]+'",'+
    '"'+moondays[lunation,1]+'",'+
    '"'+edit15.Text+'",'+
    '"'+thinstru[instnum,1]+'",'+
    '"'+prinstru[instnum,1]+'"'+
    ');';

 dben.Query(cmd);

 dbfr.Commit;
 dben.Commit;

 modeupdate:=false;
 Button2.Caption:='Enregistrer';
 Button3.Caption:='Nouveau';
 Button3Click(nil);
 moon1.SetMark(0,0,'');
 moon1.RefreshAll;
end;

procedure Tf_catlun.Button3Click(Sender: TObject);
begin
// nouveau
if moon1.MeasuringDistance then Button4Click(nil);
edit1.Text:='';
edit2.Text:='';
edit3.Text:='';
edit4.Text:='';
edit5.Text:='';
edit6.Text:='';
edit7.Text:='';
edit8.Text:='';
edit9.Text:='';
edit10.Text:='';
edit11.Text:='';
edit12.Text:='';
edit13.Text:='';
edit14.Text:='';
edit15.Text:='';
edit16.Text:='';
edit17.Text:='';
edit18.Text:='';
moon1.SetMark(0,0,'');
modeupdate:=false;
Button2.Caption:='Enregistrer';
Button3.Caption:='Nouveau';
end;

procedure Tf_catlun.Button4Click(Sender: TObject);
begin
  moon1.MeasuringDistance := not moon1.MeasuringDistance;
  if moon1.MeasuringDistance then begin
    Button4.Caption      := 'Fin mesure'
  end else begin
    Button4.Caption      := 'Mesure distance';
    moon1.SetMark(deg2rad*curlon,deg2rad*curlat,' ');
  end;
end;

procedure Tf_catlun.Button5Click(Sender: TObject);
begin
  edit3.Text:=edit1.Text;
  edit4.Text:=edit2.Text;
end;

procedure Tf_catlun.Button6Click(Sender: TObject);
begin
  ComboBox1.ItemIndex:=3;
  ComboBox7.ItemIndex:=0;
  ComboBox8.ItemIndex:=0;
  ComboBox2.ItemIndex:=0;
  ComboBox3.ItemIndex:=0;
  ComboBox4.ItemIndex:=0;
  ComboBox5.ItemIndex:=0;
end;

procedure Tf_catlun.Button7Click(Sender: TObject);
begin
  u_translation.translate('fr','fr');
  dbq:=dbfr;
  identLB(currentl,currentb);
end;

procedure Tf_catlun.Button8Click(Sender: TObject);
begin
  u_translation.translate('en','en');
  dbq:=dben;
  identLB(currentl,currentb);
end;

procedure Tf_catlun.Button9Click(Sender: TObject);
var cmd,nfr,nen : string;
begin
if (trim(currentname)>'') and
   (mrYes=MessageDlg('Supprimer '+currentname+' des bases de donnée français et anglais ?',mtConfirmation, mbYesNo, 0))
   then begin
   cmd:='delete from moon where dbn='+inttostr(dbn)+' and name="'+currentname+'";';
   dbfr.Query(cmd);
   nfr:=inttostr(dbfr.RowsAffected);
   dbfr.Commit;
   dben.Query(cmd);
   nen:=inttostr(dben.RowsAffected);
   dben.Commit;
   ShowMessage('Suppression terminée.'+crlf+nfr+' enregistrements de Nonames FR.'+crlf+nen+' enregistrements de Nonames EN.');
   moon1.RefreshAll;
end;

end;

procedure Tf_catlun.ComboBox1Change(Sender: TObject);
begin
  if trim(edit7.Text)>'' then Button1Click(nil);
end;

procedure Tf_catlun.Edit1Change(Sender: TObject);
begin
try
if IsNumber(edit1.text) then begin
  edit2.Text:= formatfloat(f1,StrToFloat(edit1.text) / 1.609);
  if ComboBox1.ItemIndex in [1,2,3] then begin
    edit3.Text:=edit1.Text;
//    edit4.Text:=edit2.Text;
  end;
end;
except
end;
end;

procedure Tf_catlun.Edit3Change(Sender: TObject);
begin
try
if IsNumber(edit3.text) then begin
  edit4.Text:= formatfloat(f1,StrToFloat(edit3.text) / 1.609);
end;
except
end;
end;

procedure Tf_catlun.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  dbfr.Free;
  dben.Free;
end;

procedure Tf_catlun.FormResize(Sender: TObject);
var
  dx,w1,w2: integer;
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
  Button3Click(nil);
  StartTimer.Enabled:=true;
end;

procedure Tf_catlun.Load1Click(Sender: TObject);
var fn: string;
    i: integer;
begin
if (mrYes=MessageDlg('Remplacer les bases de donnée Nonames français et anglais par le contenu d''un fichier?',mtConfirmation, mbYesNo, 0)) then begin
  OpenDialog1.InitialDir:=Homedir;
  OpenDialog1.Title:='Choisir le fichier Nonames FR';
  ShowMessage('Choisir le fichier Nonames FR');
  if OpenDialog1.Execute then begin
     fn:=OpenDialog1.FileName;
     i:=Pos('Nonames_uFR_',fn);
     if i=0 then begin
        ShowMessage('Mauvais fichier!');
        exit;
     end;
     convertDB(dbfr,fn,inttostr(dbn));
     if MsgForm<>nil then MsgForm.Close;
  end;
  OpenDialog1.Title:='Choisir le fichier Nonames EN de la même date que le FR';
  ShowMessage('Choisir le fichier Nonames EN de la même date que le FR');
  if OpenDialog1.Execute then begin
     fn:=OpenDialog1.FileName;
     i:=Pos('Nonames_uEN_',fn);
     if i=0 then begin
        ShowMessage('Mauvais fichier!');
        exit;
     end;
     convertDB(dben,fn,inttostr(dbn));
     if MsgForm<>nil then MsgForm.Close;
  end;
  moon1.RefreshAll;
end;
end;

procedure Tf_catlun.MenuItem1Click(Sender: TObject);
begin
  moon1.Zoom:=5;
  moon1.RefreshAll;
end;

procedure Tf_catlun.MenuItem2Click(Sender: TObject);
begin
  moon1.Zoom:=moon1.ZoomMax/2;
  moon1.RefreshAll;
end;

procedure Tf_catlun.MenuItem3Click(Sender: TObject);
begin
  showlabel:=not showlabel;
  moon1.RefreshAll;
end;

procedure Tf_catlun.MenuItem6Click(Sender: TObject);
begin
  moon1.ShowGrid:=not moon1.ShowGrid;
end;

procedure Tf_catlun.Notebook1PageChanged(Sender: TObject);
begin
modesaisie:=(Notebook1.ActivePage=page1.Caption);
if (moon1<>nil) and moon1.MeasuringDistance then Button4Click(nil);
if modesaisie then sidelist:='1,2,3,4,5,6,7'
              else sidelist:='1,2,3,4,5,6,7,'+inttostr(dbn);
if not modesaisie then identLB(curLon,curLat);
if (moon1<>nil) then moon1.RefreshAll;
end;

procedure Tf_catlun.Quit1Click(Sender: TObject);
begin
  close;
end;

procedure Tf_catlun.Save1Click(Sender: TObject);
var fname, savedate, row: string;
    f : textfile;
    i,j,nfr,nen: integer;
begin
savedate:=FormatDateTime('YYYY-MM-DD_HH-NN',now);
SelectDirectoryDialog1.InitialDir:=Homedir;
if SelectDirectoryDialog1.Execute then begin
   // français
   nfr:=0;
   fname:=slash(SelectDirectoryDialog1.FileName)+'Nonames_uFR_'+savedate+'.csv';
   AssignFile(f,fname);
   rewrite(f);
   dbfr.Query('select * from moon where dbn='+IntToStr(dbn)+' order by NAME;');
   row:='';
   for i:=2 to dbfr.FieldCount-1 do begin
     if row<>'' then row:=row+';';
     row:=row+dbfr.Fields[i];
   end;
   WriteLn(f,row);
   for i:=0 to dbfr.RowCount-1 do begin
     row:='';
     for j:=2 to dbfr.ColCount-1 do begin
        if row<>'' then row:=row+';';
        row:=row+dbfr.Results[i][j];
     end;
     WriteLn(f,row);
     inc(nfr);
   end;
   CloseFile(f);
   dbfr.ClearResultSets;
   // anglais
   nen:=0;
   fname:=slash(SelectDirectoryDialog1.FileName)+'Nonames_uEN_'+savedate+'.csv';
   AssignFile(f,fname);
   rewrite(f);
   dben.Query('select * from moon where dbn='+IntToStr(dbn)+' order by NAME;');
   row:='';
   for i:=2 to dben.FieldCount-1 do begin
     if row<>'' then row:=row+';';
     row:=row+dben.Fields[i];
   end;
   WriteLn(f,row);
   for i:=0 to dben.RowCount-1 do begin
     row:='';
     for j:=2 to dben.ColCount-1 do begin
        if row<>'' then row:=row+';';
        row:=row+dben.Results[i][j];
     end;
     WriteLn(f,row);
     inc(nen);
   end;
   CloseFile(f);
   dben.ClearResultSets;
   ShowMessage('Enregistrement terminé.'+crlf+inttostr(nfr)+' enregistrements dans Nonames FR.'+crlf+inttostr(nen)+' enregistrements dans Nonames EN.');
end;
end;


procedure Tf_catlun.StartTimerTimer(Sender: TObject);
begin
StartTimer.Enabled:=false;
init;
try
  moon1.GLSceneViewer1.Visible:=true;
  moon1.GLSceneViewer1.Camera:=moon1.GLCamera1;
  Application.ProcessMessages;
  sidelist:='1,2,3,4,5,6,7';
  moon1.LibrationMark:=False;
  moon1.Mirror:=False;
  moon1.ShowPhase:=false;
  moon1.LibrLat:=0;
  moon1.LibrLon:=0;
  moon1.Zoom:=5;
  moon1.VisibleSideLock:=false;
  moon1.CenterAt(0,0);
  moon1.RefreshAll;
finally
 screen.cursor := crArrow;
end;
end;

procedure Tf_catlun.Init;
var i:integer;
    savecaption,savesidelist: string;
begin
try
  usedatabase[1] := True;
  usedatabase[2] := True;
  usedatabase[3] := True;
  usedatabase[4] := True;
  usedatabase[5] := True;
  usedatabase[6] := True;
  usedatabase[7] := True;
  usedatabase[dbn] := True;
  dben:=TLiteDB.Create(nil);
  dbfr:=TLiteDB.Create(nil);
  uplanguage:='EN';
  LoadDB(dben);
  uplanguage:='FR';
  LoadDB(dbfr);
  dbfr.Query('select * from user_database where DBN="'+inttostr(dbn)+'";');
  if dbfr.RowCount=0 then begin
     dbfr.Query('insert into user_database values('+inttostr(dbn)+',"Catlun Nonames");');
     dbfr.Commit;
  end;
  dben.Query('select * from user_database where DBN="'+inttostr(dbn)+'";');
  if dben.RowCount=0 then begin
     dben.Query('insert into user_database values('+inttostr(dbn)+',"Catlun Nonames");');
     dben.Commit;
  end;
  dbq:=dbfr;
  moon1.Init;
  moon1.TextureCompression:=true;
  moon1.texture:=texturefiles;
  moon1.VisibleSideLock:=true;
  moon1.Labelcolor:=clWhite;
  moon1.SetMark(0, 0, '');
  moon1.zoom:=1;
  moon1.GridSpacing:=1;
  moon1.GLSphereMoon.Slices := 180;
  moon1.GLSphereMoon.Stacks := 90;
  Visible:=true;
finally
  screen.cursor := crDefault;
end;
end;

function Tf_catlun.SearchAtPos(l, b: double): boolean;
var
  mindist, d, l1, b1, deltab, deltal: double;
  rec, i: integer;
begin
  mindist := 9999;
  Result  := False;
  rec     := 0;
  deltab  := 5;
  deltal  := deltab / cos(deg2rad * b);
  dbq.query('select ID,LONGIN,LATIN from moon ' + ' where DBN in (' + sidelist + ')' +
    ' and LONGIN > ' + formatfloat(f2, l - deltal) +
    ' and LONGIN < ' + formatfloat(f2, l + deltal) +
    ' and LATIN > ' + formatfloat(f2, b - deltab) +
    ' and LATIN < ' + formatfloat(f2, b + deltab) + ' ;');
  for i := 0 to dbq.RowCount - 1 do
  begin
    l1 := dbq.Results[i].format[1].AsFloat;
    b1 := dbq.Results[i].format[2].AsFloat;
    d  := angulardistance(deg2rad*l, deg2rad*b, deg2rad*l1, deg2rad*b1);
    if d < mindist then
    begin
      Result  := True;
      mindist := d;
      rec     := dbq.Results[i].Format[0].AsInteger;
    end;
  end;
  if Result then
  begin
    currentid := IntToStr(rec);
    dbq.query('select * from moon where ID=' + currentid + ';');
    Result := dbq.RowCount > 0;
  end;
end;

procedure Tf_catlun.IdentLB(l, b: single);
var
  txt: string;
begin
  if SearchAtPos(l, b) then
  begin
    l := dbq.Results[0].ByField['LONGIN'].AsFloat;
    b := dbq.Results[0].ByField['LATIN'].AsFloat;
    searchl := l;
    searchb := b;
    currentl := l;
    currentb := b;
    currentname := dbq.Results[0].ByField['NAME'].AsString;
    GetHTMLDetail(dbq.Results[0], txt);
    SetDescText(txt);
  end;
end;

procedure Tf_catlun.GetHTMLDetail(row: TResultRow; var txt: string);
const
  b     = '&nbsp;';
  t1    = '<center><font size=+1 color="#0000FF"><b>';
  t1end = '</b></font></center>';
  t2    = '<font size=+1>';
  t2end = '</font>';
  t3    = '<b>';
  t3end = '</b>';
var
  nom, carte, url, img, remoteurl, txtbuf, buf, buf2: string;
  ok:   boolean;
  i, j: integer;
  function GetField(fn:string):string;
  var k: integer;
  begin
    result:=trim(row.ByField[fn].AsString);
    if shortdesc then begin
      if result='?' then result:='';
      if result='??' then result:='';
      if result>'' then
{        for k := 1 to num_bl do
        begin
          if result=bldb[k] then result:='';
        end;}
    end
    else
      if result='' then result:=' ';
  end;
begin
  txt := '<html> <body bgcolor="white">';
  nom := GetField('NAME');
{  dblox.Gofirst;
  ok := dblox.MatchData('NAME', '=', nom);
  if not ok then
    ok := dblox.SeekData('NAME', '=', nom);}
  ok:=false;
  txt  := txt + t1 + nom + t1end + '<br>';
  i    := row.ByField['DBN'].AsInteger;
  button9.Enabled:=(i=dbn);
  button10.Enabled:=(i=dbn);
{  if i > 9 then
  begin
    txt := txt + t3 + 'From Database:' + t3end + b + IntToStr(i) + b;
    for j := 0 to form2.CheckListBox1.Count - 1 do
      if (form2.CheckListBox1.Items.Objects[j] as TDBinfo).dbnum = i then
        txt := txt + form2.CheckListBox1.Items[j] + '<br>';
  end;}
  if (GetField('LUN'))>'' then
     txt  := txt + t3 + 'L.U.N.:' + t3end + b + GetField('LUN') + '<br>';
  txt  := txt + t3 + rsm_56 + t3end + b + GetField('TYPE') + '<br>';
  if (GetField('PERIOD'))>'' then
     txt  := txt + t3 + rsm_49 + t3end + b + GetField('PERIOD') + '<br>';
  txt  := txt + b + '<br>';

  //Taille
  txtbuf:='';
  if (GetField('LENGTHKM')>'')or(GetField('WIDEKM')>'')or(GetField('LENGTHMI')>'')or(GetField('WIDEMI')>'') then
     txtbuf  := txtbuf + t3 + rsm_17 + t3end + b + GetField('LENGTHKM') + 'x' +
             GetField('WIDEKM') + rsm_18 + b + '/' + b + GetField('LENGTHMI') +
             'x' + GetField('WIDEMI') + rsm_19 + '<br>';
  buf  := GetField('HEIGHTM');
  buf2 := GetField('HEIGHTFE');
  if buf <> buf2 then begin
    txtbuf := txtbuf + t3 + rsm_20 + t3end + b;
    val(buf, dummy, i);
    if i = 0 then
      txtbuf := txtbuf + buf + rsm_21 + b + '/' + b;
    val(buf2, dummy, i);
    if i = 0 then
      txtbuf := txtbuf + buf2 + rsm_22;
    txtbuf   := txtbuf + '<br>';
    if (GetField('RAPPORT'))>'' then
       txtbuf := txtbuf + t3 + rsm_23 + t3end + b + GetField('RAPPORT') + '<br>';
  end;
  if txtbuf>'' then
     txt  := txt + t2 + rsm_57 + t2end + '<br>'+txtbuf+ b + '<br>'; //Taille

  //Description
  txtbuf:='';
  if GetField('GENERAL') > '' then
    txtbuf := txtbuf + GetField('GENERAL') + '<br>';
  if GetField('SLOPES') > '' then
    txtbuf := txtbuf + GetField('SLOPES') + '<br>';
  if GetField('WALLS') > '' then
    txtbuf:= txtbuf + GetField('WALLS') + '<br>';
  if GetField('FLOOR') > '' then
    txtbuf := txtbuf + GetField('FLOOR') + '<br>';
  if txtbuf>'' then
    txt := txt + t2 + rsm_58 + t2end + '<br>'+txtbuf+b + '<br>'; //Description

  //Observation
  txtbuf:='';
  if GetField('INTERESTC') > '' then
     txtbuf   := txtbuf + t3 + rsm_24 + t3end + b + GetField('INTERESTC') + '<br>';
  buf   := GetField('MOONDAYS');
  buf2  := GetField('MOONDAYM');
  if (buf+buf2)>'' then
    if buf = buf2 then
      txtbuf := txtbuf + t3 + rsm_25 + t3end + b + buf + '<br>'
    else
      txtbuf := txtbuf + t3 + rsm_25 + t3end + b + buf + b + rsm_26 + b + buf2 + '<br>';
  if GetField('PRINSTRU') > '' then
     txtbuf := txtbuf + t3 + rsm_28 + t3end + b + GetField('PRINSTRU') + '<br>';
  if txtbuf>'' then
     txt   := txt + t2 + rsm_59 + t2end + '<br>'+txtbuf+b + '<br>'; //Observation

  //Position
  txtbuf:='';
  if GetField('LONGIC') > '' then
     txtbuf   := txtbuf + t3 + rsm_10 + t3end + b + GetField('LONGIC') + '<br>';
  if GetField('LATIC') > '' then
     txtbuf   := txtbuf + t3 + rsm_11 + t3end + b + GetField('LATIC') + '<br>';
  if trim(GetField('FACE')) > '' then
     txtbuf   := txtbuf + t3 + rsSide + t3end + b + GetField('FACE') + '<br>';
  if GetField('QUADRANT') > '' then
     txtbuf   := txtbuf + t3 + rsm_12 + t3end + b + GetField('QUADRANT') + '<br>';
  if GetField('AREA') > '' then
     txtbuf   := txtbuf + t3 + rsm_13 + t3end + b + GetField('AREA') + '<br>';
  if txtbuf>'' then
     txt   := txt + t2 + rsm_60 + t2end + '<br>'+txtbuf+b + '<br>'; //Position

  //Atlas
  txtbuf:='';
  // RUKL link
  carte := GetField('RUKL') + ' ' + GetField('RUKLC');
  img   := padzeros(GetField('RUKL'), 2);
{  url   := ruklprefix + img + ruklsuffix;
  if fileexists(url) then
    url := ' <A HREF="file://' + url + '">' + carte + '</A>'
  else }
    url := carte;
  if trim(url)>'' then
     txtbuf := txtbuf + t3 + rsm_14 + t3end + b + url + '<br>';
  buf := GetField('VISCARDY');
  if trim(buf) > '' then
    txtbuf := txtbuf + t3 + rsm_15 + t3end + b + buf + '<br>';
  buf   := GetField('HATFIELD');
  if trim(buf) > '' then
    txtbuf := txtbuf + t3 + rsm_16 + t3end + b + buf + '<br>';
  buf   := GetField('WESTFALL');
  if trim(buf) > '' then
    txtbuf := txtbuf + t3 + rsm_66 + t3end + b + buf + '<br>';
  buf   := GetField('WOOD');
  if trim(buf) > '' then
    txtbuf := txtbuf + t3 + rsm_72 + t3end + b + buf + '<br>';
 { if ok then
  begin
    txtbuf := txtbuf + t3 + rsm_65 + t3end;
    while ok do
    begin
      carte := dblox.GetDATA('PLATE');
      if lopamdirect then
      begin
        img := dblox.GetDATA('IMAGE');
        url := lopamlocalurl + img + lopamlocalsuffix;
        remoteurl := lopamdirecturl + img + lopamdirectsuffix;
        // on regarde d'abord en local
        if fileexists(url) then
          url := 'file://' + url     // nouveau nom: iv_038_h3.jpg
        else
        begin
          img := stringreplace(img, '_0', '_', []);
          img := stringreplace(img, '_0', '_', []);
          url := lopamlocalurl + img + lopamlocalsuffix;    // ancien nom: iv_38_h3.jpg
          if fileexists(url) then
            url := 'file://' + url
          else
          begin
            img := uppercase(img);
            img := stringreplace(img, 'I_', '1-', []);
            img := stringreplace(img, 'II_', '2-', []);
            img := stringreplace(img, 'III_', '3-', []);
            img := stringreplace(img, 'IV_', '4-', []);
            img := stringreplace(img, 'V_', '5-', []);
            img := stringreplace(img, '_', '', []);
            url := lopamlocalurl + img + lopamlocalsuffix;    // ancien nom: 4-38h3.jpg
            if fileexists(url) then
              url := 'file://' + url
            else
              url := remoteurl;  // sinon on va chercher sur le site
          end;
        end;
      end
      else
      begin
        url := lopamplateurl + carte + lopamplatesuffix;
      end;
      txtbuf := txtbuf + b + ' <A HREF="' + url + '">' + carte + '</A>';
      ok  := dblox.SeekData('NAME', '=', nom);
    end;
    txtbuf := txtbuf + '<br>';
  end;}
  if txtbuf>'' then
     txt   := txt + t2 + rsm_61 + t2end + '<br>'+txtbuf+b + '<br>'; //Atlas

  //Origine
  txtbuf:='';
  if GetField('NAMEDETAIL') > '' then
     txtbuf := txtbuf + t3 + rsm_63 + t3end + b + GetField('NAMEDETAIL') + '<br>';
  if (trim(GetField('WORK') + GetField('NATIONLITY')) > '') and
    (trim(GetField('CENTURYC') + GetField('COUNTRY')) > '') then
  begin
    case wordformat of
      0: txtbuf := txtbuf + GetField('CENTURYC') + b + GetField('NATIONLITY') +
          b + GetField('WORK') + b + rsm_2 + b + GetField('COUNTRY') + '<br>';
      // english
      1: txtbuf := txtbuf + GetField('WORK') + b + GetField('NATIONLITY') +
          b + rsm_1 + b + GetField('CENTURYC') + b + rsm_2 + b +
          GetField('COUNTRY') + '<br>';
      // francais, italian
      2: txtbuf := txtbuf + GetField('NATIONLITY') + b + GetField('WORK') +
          b + GetField('CENTURYC') + b + rsm_2 + b + GetField('COUNTRY') + '<br>';
      // russian
    end;
    if (GetField('BIRTHPLACE')>'')or((GetField('BIRTHDATE')>'')) then
       txtbuf := txtbuf + t3 + rsm_3 + t3end + b + GetField('BIRTHPLACE') + b +
                 rsm_4 + b + GetField('BIRTHDATE') + '<br>';
    if (GetField('DEATHPLACE')>'')or((GetField('DEATHDATE')>'')) then
       txtbuf := txtbuf + t3 + rsm_5 + t3end + b + GetField('DEATHPLACE') + b +
                 rsm_4 + b + GetField('DEATHDATE') + '<br>';
  end;
  if GetField('FACTS')<>'' then
     txtbuf := txtbuf + t3 + rsm_64 + t3end + b + GetField('FACTS') + '<br>';
  if GetField('NAMEORIGIN')<>'' then
     txtbuf   := txtbuf + t3 + rsm_6 + t3end + b + GetField('NAMEORIGIN') + '<br>';
  if GetField('LANGRENUS')<>'' then
     txtbuf   := txtbuf + t3 + rsm_7 + t3end + b + GetField('LANGRENUS') + '<br>';
  if GetField('HEVELIUS')<>'' then
     txtbuf   := txtbuf + t3 + rsm_8 + t3end + b + GetField('HEVELIUS') + '<br>';
  if GetField('RICCIOLI')<>'' then
     txtbuf   := txtbuf + t3 + rsm_9 + t3end + b + GetField('RICCIOLI') + '<br>';
  if txtbuf>'' then
     txt := txt + t2 + rsm_62 + t2end + '<br>'+txtbuf+ b + '<br>'; //Origine

  txt   := txt + '</body></html>';
{  if copy(GetField('PROFIL'),1,2)='A_' then begin
    Label7.Caption := GetField('PROFIL');
    Label7.Font.Size := 8;
    Label7.Left := 8;
    Label7.Top := 8;
    while (Label7.Font.Size > 3) and (Label7.Width > GroupBox1.ClientWidth) do
      Label7.Font.Size := Label7.Font.Size - 1;
    Label7.Left := (GroupBox1.ClientWidth - Label7.Width) div 2;
    Label7.Top  := (GroupBox1.ClientHeight - Label7.Height + 4) div 2;
  end
  else
    Label7.Caption := ''; }
  statusbar1.Panels[0].Text := rsm_10 + GetField('LONGIN');
  statusbar1.Panels[1].Text := rsm_11 + GetField('LATIN');
//  Addtolist(nom);
end;

procedure Tf_catlun.SetDescText(const Value: string);
var
  s: TStringStream;
  NewHTML: TIpHtml;
begin
  try
    s := TStringStream.Create(UTF8FileHeader+Value);
    try
      NewHTML := TIpHtml.Create; // Beware: Will be freed automatically by Desc1
      NewHTML.LoadFromStream(s);
    finally
      s.Free;
    end;
    Desc1.SetHtml(NewHTML);
  except
  end;
end;

procedure Tf_catlun.MoonMeasureEvent(Sender: TObject; m1,m2,m3,m4: string);
begin
  edit1.Text := m1;
(*  edit2.Text:= formatfloat(f1,StrToFloat(m1) / 1.609);
  if ComboBox1.ItemIndex in [1,2,3] then begin
    edit3.Text:=edit1.Text;
    edit4.Text:=edit2.Text;
  end;  *)
end;

procedure Tf_catlun.MoonClickEvent(Sender: TObject; Button: TMouseButton;
                     Shift: TShiftState; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single);
begin
if Button=mbLeft then begin
  if OnMoon then begin
   if modesaisie then begin
     curlat:=rad2deg*Lat;
     curlon:=rad2deg*Lon;
     edit5.Text:=FormatFloat(f2,curlat);
     edit6.Text:=FormatFloat(f2,curlon);
     edit7.Text:='';
     moon1.SetMark(Lon,Lat,' ');
   end else begin
     identLB(Rad2Deg*Lon,Rad2Deg*Lat);
     Tf_moon(Sender).SetMark(deg2rad*currentl,deg2rad*currentb,' ');
   end;
  end else begin
     Tf_moon(Sender).SetMark(0,0,'');
  end;
end;
if Button=mbRight then begin
 if modesaisie then begin
    Button4Click(nil); // mesure distance
 end;
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

initialization
  {$I catlun_main.lrs}

end.

