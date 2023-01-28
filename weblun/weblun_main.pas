unit weblun_main;

{$mode objfpc}{$H+}

interface

uses
{$ifdef mswindows}
Windows, ShlObj,
{$endif}
  u_constant, u_translation, u_util, passql, passqlite,
  downloaddialog, UniqueInstance, IniFiles, Classes, SysUtils, FileUtil, Forms, Controls,
  LCLVersion, LazUTF8, Graphics, Dialogs, ComCtrls, Menus, Grids, ExtCtrls, StdCtrls;

const ncols=7;
      crlf = chr(10)+chr(13);

type

  { Tf_weblun }

  Tf_weblun = class(TForm)
    Button1: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    DownloadDialog1: TDownloadDialog;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Help1: TMenuItem;
    MenuHelp: TMenuItem;
    MenuAbout: TMenuItem;
    PanelTop: TPanel;
    Quit1: TMenuItem;
    ResetSelection: TMenuItem;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    InitTimer: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InitTimerTimer(Sender: TObject);
    procedure MenuHelpClick(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure Quit1Click(Sender: TObject);
    procedure ResetSelectionClick(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
  private
    { private declarations }
    MouseX, MouseY, HintX, HintY, checkdate, webdate : integer;
    SelectedSite: string;
    locktheme: boolean;
    procedure SetLang;
    procedure GetAppDir;
    Procedure ReadParam(first:boolean=true);
    Procedure LoadDB;
    Procedure SortByCol(col:integer);
    procedure SetTitle;
    procedure RefreshGrid;
    procedure FillTheme;
    procedure SelectAll;
    Procedure WebUpdate;
  public
    { public declarations }
    param: TStringList;
    procedure InitApp;
  end; 

var
  f_weblun: Tf_weblun;

implementation

{$R *.lfm}

{ Tf_weblun }

procedure Tf_weblun.FormShow(Sender: TObject);
begin
  InitTimer.Enabled:=true;
end;

procedure Tf_weblun.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  InitApp;
  FillTheme;
  SelectAll;
  Application.BringToFront;
end;

procedure Tf_weblun.MenuHelpClick(Sender: TObject);
begin
 ShowHelpDoc('Doc','WebLun','doc')
end;

procedure Tf_weblun.MenuAboutClick(Sender: TObject);
begin
Showmessage('WebLun '+Splashversion+crlf+
            compile_version+crlf+
            avlcpy+crlf+crlf+
            'Conception : Christian Legrand'+crlf+
            'Programming : Patrick Chevalley'+crlf+crlf+
            'This program is free software; you can redistribute it and/or '+crlf+
            'modify it under the terms of the GNU General Public License '+crlf+
            'as published by the Free Software Foundation.'
            );
end;

procedure Tf_weblun.Quit1Click(Sender: TObject);
begin
  Close;
end;

procedure Tf_weblun.ResetSelectionClick(Sender: TObject);
begin
  SelectAll;
end;

procedure Tf_weblun.FormCreate(Sender: TObject);
var i: integer;
begin
  compile_time := {$I %DATE%}+' '+{$I %TIME%};
  compile_version := 'Lazarus '+lcl_version+' Free Pascal '+{$I %FPCVERSION%}+' '+{$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%};
  Splashversion := AVLversion+blank+compile_time;
  ScaleFormForFontSize(self,96);
  DefaultFormatSettings.DecimalSeparator := '.';
  DefaultFormatSettings.ThousandSeparator:=' ';
  GetAppDir;
  chdir(appdir);
  param:=Tstringlist.Create;
  param.clear;
  if paramcount>0 then begin
     for i:=1 to paramcount do begin
        param.Add(paramstr(i));
     end;
  end;
  dbm:=TLiteDB.Create(self);
end;

procedure Tf_weblun.FormDestroy(Sender: TObject);
begin
  param.free;
  dbm.free;
end;

procedure Tf_weblun.InitApp;
begin
  SetLang;
  LoadDB;
  ReadParam;
end;

Procedure Tf_weblun.ReadParam(first:boolean=true);
var i : integer;
begin
i:=0;
while i <= param.count-1 do begin
if param[i]='-quit' then begin  // close current instance
   Close;
end
else if param[i]='--' then begin   // last parameter
     break;
end;
inc(i);
end;
end;


procedure Tf_weblun.UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
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


procedure Tf_weblun.StringGrid1DblClick(Sender: TObject);
begin
 if SelectedSite<>'' then ExecuteFile(SelectedSite);
end;

procedure Tf_weblun.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if (aRow>0) and (aCol=4) then begin
    StringGrid1.Canvas.Brush.Style:=bsSolid;
    StringGrid1.Canvas.Brush.Color:=clWindow;
    StringGrid1.Canvas.FillRect(aRect);
    if gdHot in aState
       then StringGrid1.Canvas.Font.Style:=[fsUnderline]
       else StringGrid1.Canvas.Font.Style:=[];
    StringGrid1.Canvas.Font.Color:=clNavy;
    StringGrid1.Canvas.TextRect(aRect,aRect.Left,aRect.Top,StringGrid1.Cells[aCol,aRow]);
    StringGrid1.Canvas.Font.Style:=[];
    StringGrid1.Canvas.Font.Color:=clDefault;
  end;
end;

procedure Tf_weblun.StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
MouseX:=X;
MouseY:=Y;
end;

procedure Tf_weblun.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var col,row:integer;
begin
 StringGrid1.MouseToCell(X, Y, Col, Row);
 if (col>=0) and (row>=0) then begin
   if (HintX<>row)or(HintY<>col) then begin
      StringGrid1.Hint:='';
      StringGrid1.ShowHint:=false;
      if (HintY=4)and(HintX>0)and(HintX<StringGrid1.RowCount)
        then StringGrid1DrawCell(Sender,HintY,HintX,StringGrid1.CellRect(HintY,HintX),[]);
      HintX:=row;
      HintY:=col;
   end else begin
      if (trim(StringGrid1.Cells[col,row])<>'')and(StringGrid1.Canvas.TextWidth(StringGrid1.Cells[col,row])>StringGrid1.ColWidths[col]) then begin
         StringGrid1.Hint:=StringGrid1.Cells[col,row];
         StringGrid1.ShowHint:=true;
      end;
      if col=4 then StringGrid1DrawCell(Sender,col,row,StringGrid1.CellRect(col,row),[gdHot]);
   end;
 end;
end;

procedure Tf_weblun.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var col,row:integer;
begin
SelectedSite:='';
if Button=mbRight then begin
  // popup ?
end else begin
 StringGrid1.MouseToCell(X, Y, Col, Row);
 if (col>=0) and (row>=0) then begin
   if row=0 then begin  // column title -> Sort data
     if (abs(mousex-x)+abs(mousey-y))<2 then begin
        SortByCol(col);
     end;
   end
   else begin // site url
      if trim(StringGrid1.Cells[4,row])<>'' then SelectedSite:=StringGrid1.Cells[4,row];
      if (SelectedSite<>'')and(Col=4) then ExecuteFile(SelectedSite);
   end;
 end;
end;
end;

Procedure Tf_weblun.SortByCol(col:integer);
begin
if StringGrid1.SortOrder=soAscending then StringGrid1.SortOrder:=soDescending else StringGrid1.SortOrder:=soAscending;
StringGrid1.SortColRow(true,col);
end;

procedure Tf_weblun.SetTitle;
begin
  StringGrid1.ColWidths[0]:=300;
  StringGrid1.ColWidths[1]:=70;
  StringGrid1.ColWidths[2]:=100;
  StringGrid1.ColWidths[3]:=150;
  StringGrid1.ColWidths[4]:=200;
  StringGrid1.ColWidths[5]:=400;
  StringGrid1.ColWidths[6]:=150;
  StringGrid1.Cells[0, 0]:=rsSiteName;
  StringGrid1.Cells[1, 0]:=rsLanguage;
  StringGrid1.Cells[2, 0]:=rsThMe;
  StringGrid1.Cells[3, 0]:=rsSubThMe;
  StringGrid1.Cells[4, 0]:=rsAddresse;
  StringGrid1.Cells[5, 0]:=rsDescription;
  StringGrid1.Cells[6, 0]:=rsRecDate;
end;

procedure Tf_weblun.RefreshGrid;
var i,j: integer;
begin
if dbm.RowCount>0 then begin
  StringGrid1.RowCount:=dbm.RowCount+1;
  for i:=0 to dbm.RowCount-1 do begin
    for j:=0 to ncols-1 do begin
       StringGrid1.Cells[j,i+1]:=dbm.Results[i][j];
    end;
  end;
end;
StatusBar1.SimpleText:=Format(rsSelectedSite, [inttostr(dbm.RowCount)]);
end;

procedure Tf_weblun.SelectAll;
var cmd: string;
begin
 locktheme:=true;
 StringGrid1.Clear;
 Stringgrid1.RowCount:=5;
 SetTitle;
 cmd:='select SITE_NAME,LANGUAGE,THEME,SUB_THEME,ADDRESS,DESCRIPTION,DATE from weblun order by THEME,SUB_THEME,SITE_NAME;';
 dbm.Query(cmd);
 RefreshGrid;
 ComboBox1.ItemIndex:=0;
 ComboBox2.ItemIndex:=0;
 Application.ProcessMessages;
 locktheme:=false;
end;

procedure Tf_weblun.FillTheme;
var i: integer;
    cmd: string;
begin
 locktheme:=true;
 ComboBox1.Clear;
 ComboBox2.Clear;
 ComboBox1.Items.Add(rsAll);
 ComboBox2.Items.Add(rsAll);
 cmd:='select distinct(THEME) from weblun order by THEME;';
 dbm.Query(cmd);
 for i:=0 to dbm.RowCount-1 do begin
   ComboBox1.Items.Add(dbm.Results[i][0]);
 end;
 ComboBox1.ItemIndex:=0;
 ComboBox2.ItemIndex:=0;
 Application.ProcessMessages;
 locktheme:=false;
end;

procedure Tf_weblun.ComboBox1Change(Sender: TObject);
var th: string;
    i: integer;
    cmd: string;
begin
if locktheme then exit;
locktheme:=true;
edit1.text:='';
 th:=ComboBox1.Text;
 if th=rsAll then begin
   ComboBox2.ItemIndex:=0;
   SelectAll;
 end else begin
   ComboBox2.Clear;
   ComboBox2.Items.Add(rsAll);
   cmd:='select distinct(SUB_THEME) from weblun where THEME="'+th+'" order by SUB_THEME;';
   dbm.Query(cmd);
   for i:=0 to dbm.RowCount-1 do begin
     ComboBox2.Items.Add(dbm.Results[i][0]);
   end;
   Application.ProcessMessages;
   locktheme:=false;
   ComboBox2.ItemIndex:=0;
   ComboBox2Change(sender);
 end;
 Application.ProcessMessages;
 locktheme:=false;
end;

procedure Tf_weblun.ComboBox2Change(Sender: TObject);
var th: string;
    cmd: string;
begin
 if locktheme then exit;
 locktheme:=true;
 edit1.text:='';
 th:=ComboBox2.Text;
 if th=rsAll then begin
   cmd:='select SITE_NAME,LANGUAGE,THEME,SUB_THEME,ADDRESS,DESCRIPTION,DATE from weblun '+
        'where THEME="'+ComboBox1.Text+'" order by THEME,SUB_THEME,SITE_NAME;';
 end else begin
   cmd:='select SITE_NAME,LANGUAGE,THEME,SUB_THEME,ADDRESS,DESCRIPTION,DATE from weblun '+
        'where THEME="'+ComboBox1.Text+'" and SUB_THEME="'+th+'" order by THEME,SUB_THEME,SITE_NAME;';
 end;
 dbm.Query(cmd);
 Application.ProcessMessages;
 locktheme:=false;
 RefreshGrid;
end;

procedure Tf_weblun.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const key_cr = 13;
begin
  if key=key_cr then Button1Click(Sender);
end;

procedure Tf_weblun.Button1Click(Sender: TObject);
var buf,cmd, sel:string;
begin
 buf:=trim(edit1.text);
 sel:='';
 case ComboBox3.ItemIndex of
   0 : sel:='where SITE_NAME LIKE"%'+buf+'%" ';
   1 : sel:='where LANGUAGE LIKE"%'+buf+'%" ';
   2 : sel:='where THEME LIKE"%'+buf+'%" ';
   3 : sel:='where SUB_THEME LIKE"%'+buf+'%" ';
   4 : sel:='where ADDRESS LIKE"%'+buf+'%" ';
   5 : sel:='where DESCRIPTION LIKE"%'+buf+'%" ';
   6 : sel:='where DATE LIKE"%'+buf+'%" ';
   7 : sel:='where (SITE_NAME LIKE"%'+buf+'%" '+
            'or LANGUAGE LIKE"%'+buf+'%" '+
            'or THEME LIKE"%'+buf+'%" '+
            'or SUB_THEME LIKE"%'+buf+'%" '+
            'or ADDRESS LIKE"%'+buf+'%" '+
            'or DESCRIPTION LIKE"%'+buf+'%" '+
            'or DATE LIKE"%'+buf+'%") ';

 end;
 if ComboBox1.Text<>rsAll then sel:=sel+' AND THEME="'+ComboBox1.Text+'" ';
 if ComboBox2.Text<>rsAll then sel:=sel+' AND SUB_THEME="'+ComboBox2.Text+'" ';
 cmd:='select SITE_NAME,LANGUAGE,THEME,SUB_THEME,ADDRESS,DESCRIPTION,DATE from weblun '+
      sel+
      'order by THEME,SUB_THEME,SITE_NAME;';
 dbm.Query(cmd);
 RefreshGrid;
end;

Procedure Tf_weblun.SetLang;
var section : string;
    inifile : Tmeminifile;
begin
language := '';
inifile := Tmeminifile.Create(ConfigFile);
with inifile do
begin
  section := 'default';
  language:= ReadString(section, 'lang_po_file', language);
end;
inifile.Free;
chdir(appdir);
language:=u_translation.translate(language,'en');
uplanguage:=UpperCase(language);
u_util.hp := rshelp_prefix;
file1.Caption:=rsFile;
Quit1.Caption:=rsQuit;
help1.Caption:=rsHelp;
MenuHelp.Caption:=rsHelp;
MenuAbout.Caption:=rsAbout;
ResetSelection.Caption:=rsResetSelecti;
label1.Caption:=rsThMe;
label2.Caption:=rsSubThMe;
label3.Caption:=rsSearch;
ComboBox3.Items[0]:=rsSiteName;
ComboBox3.Items[1]:=rsLanguage;
ComboBox3.Items[2]:=rsThMe;
ComboBox3.Items[3]:=rsSubThMe;
ComboBox3.Items[4]:=rsAddresse;
ComboBox3.Items[5]:=rsDescription;
ComboBox3.Items[6]:=rsRecDate;
ComboBox3.Items[7]:=rsAll;
Button1.Hint:=rsSearch;
end;

procedure Tf_weblun.GetAppDir;
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
  if not DirectoryExists(slash(appdir) + slash('Textures')) then
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
  // Be sur the Database directory exists
  if (not directoryexists(slash(appdir) + slash('Database'))) then
  begin
    // try under the current directory
    buf := GetCurrentDir;
    if (directoryexists(slash(buf) + slash('Database'))) then
      appdir := buf
    else
    begin
      // try under the program directory
      buf := ExtractFilePath(ParamStr(0));
      if (directoryexists(slash(buf) + slash('Database'))) then
        appdir := buf
      else
      begin
        // try share directory under current location
        buf := ExpandFileName(slash(GetCurrentDir) + SharedDir);
        if (directoryexists(slash(buf) + slash('Database'))) then
          appdir := buf
        else
        begin
          // try share directory at the same location as the program
          buf := ExpandFileName(slash(ExtractFilePath(ParamStr(0))) + SharedDir);
          if (directoryexists(slash(buf) + slash('Database'))) then
            appdir := buf
          else
          begin
            MessageDlg('Could not found the application Database directory.' +
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
  helpdir  := slash(appdir) + slash('doc');
  // Be sure zoneinfo exists in standard location or in vma directory
{  ZoneDir  := slash(appdir) + slash('data') + slash('zoneinfo');
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
          'If it is not installed at a standard location create a logical link zoneinfo in skychart data directory.',
          mtError, [mbAbort], 0);
        Halt;
      end;
    end;
  end;   }
end;

Procedure Tf_weblun.WebUpdate;
var udate,vdate,cdate:TDateTime;
    buf,fn: string;
    y,m,d:word;
    f: textfile;
begin
  checkdate:=trunc(Now);
  buf:=dbm.QueryOne('select cdate from weblun_file_date;');
  cdate:=trunc(StrToFloatDef(buf,0));
  if checkdate>(cdate+30) then begin   // check for new version once a month
     dbm.Query('update weblun_file_date set cdate='+inttostr(checkdate)+';');
     dbm.Commit;
     buf:=dbm.QueryOne('select udate from weblun_file_date;');
     udate:=trunc(StrToFloatDef(buf,0));
     fn:=slash(TempDir)+'weblun8.date';
     DeleteFile(fn);
     DownloadDialog1.Title:='Weblun database web update';
     DownloadDialog1.URL:='http://www.ap-i.net/pub/vma/weblun/weblun8.date';
     DownloadDialog1.ConfirmDownload:=false;
     DownloadDialog1.SaveToFile:=fn;
     DownloadDialog1.Timeout:=5000;
     if DownloadDialog1.Execute and FileExists(fn) then begin
       AssignFile(f,fn);
       Reset(f);
       ReadLn(f,buf);
       CloseFile(f);
       buf:=trim(buf);
       y:=StrToIntDef(copy(buf,1,4),0);
       m:=StrToIntDef(copy(buf,5,2),0);
       d:=StrToIntDef(copy(buf,7,2),0);
       vdate:=EncodeDate(y,m,d);
     end
     else vdate:=0;
     if vdate>udate then begin  // new version available
       fn:=slash(TempDir)+'weblun8.csv';
       DeleteFile(fn);
       DownloadDialog1.URL:='http://www.ap-i.net/pub/vma/weblun/weblun8.csv';
       DownloadDialog1.ConfirmDownload:=false;
       DownloadDialog1.SaveToFile:=fn;
       DownloadDialog1.Timeout:=5000;
       if DownloadDialog1.Execute then
         webdate:=trunc(vdate)
       else
         DeleteFile(fn);
     end;
  end;
end;

Procedure Tf_weblun.LoadDB;
var i,fage1,fage2,fage,db_age: integer;
    buf,cmd,fn1,fn2,fn:string;
    f: textfile;
    row:TStringList;
    cols: array[1..ncols] of integer;
begin
buf:=Slash(DBdir)+'dbmoon8'+uplanguage+'.dbl';
dbm.Use(utf8encode(buf));
webdate:=0;
WebUpdate;
fage1:=0; fage2:=0; fage:=0;
fn1:=Slash(appdir)+Slash('Database')+'weblun.csv';
fn2:=Slash(Tempdir)+'weblun8.csv';
if FileExists(fn1) then fage1:=fileage(fn1);
if FileExists(fn2) then fage2:=fileage(fn2);
if fage2>fage1 then begin
  fn:=fn2;
  fage:=fage2;
end else begin
  fn:=fn1;
  fage:=fage1;
end;
buf:=dbm.QueryOne('select fdate from weblun_file_date;');
if buf='' then
  db_age:=0
else
  db_age:=strtoint(buf);
cmd:='select SITE_NAME from weblun limit 1';
dbm.Query(cmd);
if (dbm.RowCount=0) then begin;
  dbm.Query('drop table weblun;');
  dbm.Query('drop table weblun_file_date;');
  dbm.Commit;
  cmd:='create table weblun_file_date ( '+
       'FDATE integer,'+
       'CDATE integer,'+
       'UDATE integer'+
       ');';
  dbm.Query(cmd);
  cmd:='create table weblun ( '+
       'SITE_NAME text,'+
       'LANGUAGE text,'+
       'THEME text,'+
       'SUB_THEME text,'+
       'ADDRESS text,'+
       'DESCRIPTION text,'+
       'DATE text'+
      ');';
   dbm.Query(cmd);
end;
if fage>db_age then begin
  if uplanguage='FR' then begin
    cols[1]:=1;
    cols[2]:=3;
    cols[3]:=5;
    cols[4]:=7;
    cols[5]:=9;
    cols[6]:=10;
    cols[7]:=12;
  end else begin
    cols[1]:=2;
    cols[2]:=4;
    cols[3]:=6;
    cols[4]:=8;
    cols[5]:=9;
    cols[6]:=11;
    cols[7]:=13;
  end;
  cmd:='delete from weblun;';
  dbm.Query(cmd);
  AssignFile(f,fn);
  Reset(f);
  readln(f);
  row:=TStringList.Create;
  repeat
    readln(f,buf);
    SplitRec2(buf,';',row);
    cmd:='insert into weblun values(';
    for i:=1 to ncols do begin
      cmd:=cmd+'"'+SafeSqlText(row[cols[i]-1])+'",';
    end;
    cmd:=copy(cmd,1,length(cmd)-1)+');';
    dbm.Query(cmd);
    if dbm.LastError<>0 then ShowMessage(dbm.ErrorMessage);
  until EOF(f);
  dbm.Query('insert into weblun_file_date values ('+inttostr(fage)+','+inttostr(checkdate)+','+inttostr(webdate)+');');
  dbm.Commit;
  row.Free;
 end;
end;

end.

