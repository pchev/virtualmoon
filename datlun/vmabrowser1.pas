unit vmabrowser1;

{$MODE Delphi}{$H+}

{
Copyright (C) 2006 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

interface

uses
{$ifdef mswindows}
Windows, ShlObj,
{$endif}
  u_translation_database, u_translation, FileUtil, LazUTF8,
  u_constant, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Grids, Math, passql, passqlite, u_util, dbutil, StdCtrls,
  ExtCtrls, IniFiles, ImgList, LResources, uniqueinstance;

const
  ExitProMsg='Virtual_Moon_Atlas_Pro_exit';
  versionname='DATLUN';

type

  { Tf_main }

  Tf_main = class(TForm)
    Edit1: TMenuItem;
    Columns1: TMenuItem;
    ShowSelection1: TMenuItem;
    Selection1: TMenuItem;
    OpenPhotlun1: TMenuItem;
    MoonGrid: TStringGrid;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Quit1: TMenuItem;
    ScrollBar1: TScrollBar;
    Panel1: TPanel;
    FindDialog1: TFindDialog;
    Export1: TMenuItem;
    SaveDialog1: TSaveDialog;
    PopupMenu1: TPopupMenu;
    Find1: TMenuItem;
    OpeninVMA1: TMenuItem;
    Import1: TMenuItem;
    N1: TMenuItem;
    Delete1: TMenuItem;
    N2: TMenuItem;
    Help1: TMenuItem;
    Help2: TMenuItem;
    APropos1: TMenuItem;
    Sortby1: TMenuItem;
    ImageList1: TImageList;
    N3: TMenuItem;
    Databasemaintenance1: TMenuItem;
    Default1: TMenuItem;
    N4: TMenuItem;
    procedure OpenPhotlun1Click(Sender: TObject);
    procedure Quit1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure MoonGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Selection1Click(Sender: TObject);
    procedure Columns1Click(Sender: TObject);
    procedure MoonGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FindDialog1Find(Sender: TObject);
    procedure Export1Click(Sender: TObject);
    procedure MoonGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Find1Click(Sender: TObject);
    procedure OpeninVMA1Click(Sender: TObject);
    procedure Import1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure ShowSelection1Click(Sender: TObject);
    procedure APropos1Click(Sender: TObject);
    procedure Help2Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Sortby1Click(Sender: TObject);
    procedure Selectfrom1Click(Sender: TObject);
    procedure MoonGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure MoonGridDblClick(Sender: TObject);
    procedure Default1Click(Sender: TObject);
  private
    { Private declarations }
    UniqueInstance1: TCdCUniqueInstance;
    currentrow,FindFrom,FindCol, sortorder: integer;
    currentselection, currentsort, transmsg, SelectedObject: string;
    FindCaption, ShowCaption, SortCaption: string;
    MouseX, MouseY, HintX, HintY: integer;
    StartVMA,CanCloseVMA, StartPhotlun, CanClosePhotlun, ExpertMode: boolean;
    IDlist: array of integer;
    procedure OtherInstance(Sender : TObject; ParamCount: Integer; Parameters: array of String);
    procedure InstanceRunning(Sender : TObject);
    Procedure SetLang;
    procedure GetAppDir;
    procedure SaveDefault;
    Procedure ReadParam(first:boolean=true);
    Procedure Readdefault;
    procedure RefreshGrid;
    procedure Select;
    procedure ClearSelection;
    procedure RemoveUnusedDBN;
    procedure OpenVMA(objname,otherparam:string);
    procedure OpenPhotlun(objname,otherparam:string);
    procedure SortByCol(col:integer);
  public
    { Public declarations }
    param : Tstringlist;
    procedure InitApp;
  end;

var
  f_main: Tf_main;

implementation

{$R vmabrowser1.lfm}

uses vmabrowser2, vmabrowser3, vmabrowser4, vmabrowser5;


procedure Tf_main.Quit1Click(Sender: TObject);
begin
close;
end;

procedure Tf_main.GetAppDir;
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
  Photlun := '"'+bindir + DefaultPhotlun+'"';     // Photlun normally at same location as vma
  Maplun  := '"'+bindir + DefaultMaplun+'"';
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

Procedure Tf_main.SetLang;
var section,buf : string;
    inifile : Tmeminifile;
    i : integer;
begin
language := '';
inifile := Tmeminifile.Create(ConfigFile);
with inifile do
begin
  section := 'default';
  language     := ReadString(section, 'lang_po_file', language);
end;
inifile.Free;
chdir(appdir);
language:=u_translation.translate(language,'en');
uplanguage:=UpperCase(language);
u_translation_database.translate(language,'en');
  transmsg:=rstranslator;
  u_util.hp:=rshelp_prefix+'_';
  Help1.caption:=rst_52;
  Help2.caption:=Help1.caption;
  APropos1.caption:=rst_53;
  file1.caption:=rst_1;
  quit1.caption:=rst_2;
  edit1.Caption:=rsEdit;
  Selection1.caption:=rst_3;
  Columns1.caption:=rst_4;
  Databasemaintenance1.Caption:=rst_14;
  Export1.Caption:=rst_33;
  Import1.Caption:=rst_34;
  Delete1.Caption:=rst_35;
  ShowSelection1.Caption:=rst_36;
  FindCaption:=rst_41;
  ShowCaption:=rst_42;
  SortCaption:=rst_43;
  Default1.Caption:=rst_50;
  dbtype[1]:=rscol_1;
  dbtype[2]:=rscol_2;
  dbtype[3]:=rscol_3;
  dbtype[4]:=rscol_4;
  dbtype[5]:=rscol_5;
  dbtype[6]:=rscol_6;
  dbtype[7]:=rscol_7;
  dbtype[8]:=rscol_8;
  dbtype[9]:=rscol_9;
  dbtype[10]:=rscol_10;
  dbtype[11]:=rscol_11;
  dbtype[12]:=rscol_12;
  dbtype[13]:=rscol_13;
  dbtype[14]:=rscol_14;
  dbtype[15]:=rscol_15;
  dbtype[16]:=rscol_16;
  dbtype[17]:=rscol_17;
  dbtype[18]:=rscol_18;
  dbtype[19]:=rscol_19;
  dbtype[20]:=rscol_20;
  dbtype[21]:=rscol_21;
  dbtype[22]:=rscol_22;
  dbtype[23]:=rscol_23;
  dbtype[24]:=rscol_24;
  dbtype[25]:=rscol_25;
  dbshortname[1]:=rsdb_1;
  dbshortname[2]:=rsdb_2;
  dbshortname[3]:=rsdb_3;
  dbshortname[4]:=rsdb_4;
  dbshortname[5]:=rsdb_5;
  dbshortname[6]:=rsdb_6;
  dbshortname[7]:=rsdb_7;
  dbshortname[8]:=rsdb_8;
  dbshortname[9]:=rsdb_9;
  Selection.SetLang;
  LoadCSV.SetLang;
  SelectDB.SetLang;
  Columns.SetLang;
end;

procedure Tf_main.OtherInstance(Sender: TObject;
  ParamCount: Integer; Parameters: array of String);
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

procedure Tf_main.InstanceRunning(Sender : TObject);
var i : integer;
begin
  UniqueInstance1.RetryOrHalt;
end;

procedure Tf_main.FormCreate(Sender: TObject);
var i: integer;
begin
DecimalSeparator := '.';
ThousandSeparator:=' ';
//{$ifndef darwin}
  UniqueInstance1:=TCdCUniqueInstance.Create(self);
  UniqueInstance1.Identifier:='Virtual_Moon_Atlas_DatLun';
  UniqueInstance1.OnOtherInstance:=OtherInstance;
  UniqueInstance1.OnInstanceRunning:=InstanceRunning;
  UniqueInstance1.Enabled:=true;
  UniqueInstance1.Loaded;
//{$endif}
{$ifdef mswindows}
ScaleForm(self,Screen.PixelsPerInch/96);
{$endif}
dbm:=TLiteDB.Create(self);
GetAppDir;
chdir(appdir);
param:=Tstringlist.Create;
param.clear;
if paramcount>0 then begin
   for i:=1 to paramcount do begin
      param.Add(paramstr(i));
   end;
end;
StartVMA:=false;
StartPhotlun:=false;
CanCloseVMA:=true;
CanClosePhotlun:=true;
SelectedObject:='';
dbselection:='DBN in (1,2,3,4,5,6,7)';
currentselection:=dbselection;
ExpertMode:=false;
Application.HintHidePause:=10000;
end;

Procedure Tf_main.ReadParam(first:boolean=true);
var i : integer;
begin
i:=0;
while i <= param.count-1 do begin
if (param[i]='-nx')and first then begin       // when started by vma do not close vma on exit!
   CanCloseVMA:=false;
end
else if (param[i]='-np')and first then begin  // when started by photlun do not close photlun on exit!
   CanClosePhotlun:=false;
end
else if param[i]='-n' then begin
   inc(i);
   if i <= param.count-1 then begin
      currentselection:='NAME LIKE "'+param[i]+'%"';
      Select;
      RefreshGrid;
   end;
end
else if param[i]='-quit' then begin  // close current instance
   Close;
end
else if param[i]='--' then begin   // last parameter
     break;
end;
inc(i);
end;
end;

procedure Tf_main.FormResize(Sender: TObject);
begin
MoonGrid.RowCount:=max(2,(MoonGrid.Height div (MoonGrid.DefaultRowHeight+MoonGrid.GridLineWidth)) );
ScrollBar1.PageSize:=MoonGrid.RowCount-1;
ScrollBar1.LargeChange:=MoonGrid.RowCount-1;
ScrollBar1.Visible:=(ScrollBar1.Max>MoonGrid.RowCount);
if dbm.RowCount>0 then RefreshGrid;
end;

procedure Tf_main.FormShow(Sender: TObject);
begin
FormResize(sender);
Application.BringToFront;
end;

procedure Tf_main.InitApp;
var dbcol,i:integer;
begin
SetLang;
ReadDefault;
for i:=1 to 7 do usedatabase[i]:=true;
usedatabase[8]:=false;
usedatabase[9]:=false;
for i:=10 to maxdbn do usedatabase[i]:=false;
LoadDB(dbm);
Loadcsv.dbname:=dbm.DataBase;
Selection.lastselection:=CurrentSelection;
Selection.ExpertMode.Checked:=ExpertMode;
Select;
if dbm.RowCount=0 then begin
 CurrentSelection:='';
 Select;
end;
Columns.CheckListBox1.Items.Clear;
Columns.CheckListBox2.Items.Clear;
Loadcsv.CheckListBox1.Items.Clear;
Selection.fieldlist.Items.Clear;
Selection.fieldlist2.Items.Clear;
if dbm.RowCount>0 then begin
  MoonGrid.ColCount:=dbm.ColCount-1;    // pas l'id
  MoonGrid.FixedCols:=2;
  MoonGrid.ColWidths[0]:=round(0.3*MoonGrid.DefaultColWidth);
  MoonGrid.ColWidths[1]:=round(1.5*MoonGrid.DefaultColWidth);
  for dbcol:=1 to MoonGrid.ColCount do begin
    MoonGrid.Cells[dbcol-1,0]:=dbm.GetField(dbcol);
    if dbcol in hidenfields then MoonGrid.ColWidths[dbcol-1]:=0;
    if dbcol<=26 then begin
      Columns.CheckListBox1.Items.Add(MoonGrid.Cells[dbcol-1,0]);
      Columns.CheckListBox1.Checked[dbcol-1]:=(MoonGrid.ColWidths[dbcol-1]>0);
    end else begin
      Columns.CheckListBox2.Items.Add(MoonGrid.Cells[dbcol-1,0]);
      Columns.CheckListBox2.Checked[dbcol-27]:=(MoonGrid.ColWidths[dbcol-1]>0);
    end;
    Loadcsv.CheckListBox1.Items.Add(MoonGrid.Cells[dbcol-1,0]);
    Loadcsv.CheckListBox1.Checked[dbcol-1]:=false;
    Selection.fieldlist.Items.Add(MoonGrid.Cells[dbcol-1,0]);
    Selection.fieldlist2.Items.Add(MoonGrid.Cells[dbcol-1,0]);
  end;
end;
Selection.fieldlist.Text:=Selection.fieldlist.Items[0];
Selection.fieldlist2.Text:=Selection.fieldlist2.Items[0];
ReadParam;
end;

Procedure Tf_main.Readdefault;
var inif: TMemIniFile;
    section : string;
    i: integer;
begin
inif:=Tmeminifile.create(configfile);
section:='DatLun';
with inif do begin
{$ifndef darwin}
i:=ReadInteger(section,'Top',1);
if (i>=-10)and(i<screen.Height-20) then f_main.Top:=i else f_main.Top:=1;
i:=ReadInteger(section,'Left',1);
if (i>=-10)and(i<screen.width-20) then f_main.Left:=i else f_main.Left:=1;
i:=screen.height-50;
i:=minintvalue([i,ReadInteger(section,'Height',f_main.height)]);
if (i>=20) then f_main.Height:=i;
i:=screen.width-5;
i:=minintvalue([i,ReadInteger(section,'Width',f_main.width)]);
if (i>=20) then f_main.Width:=i;
if ReadBool(section,'Maximized',false) then f_main.windowstate:=wsMaximized;
{$endif}
dbselection:=ReadString(section,'DBselection',dbselection);
CurrentSelection:=ReadString(section,'CurrentSelection',CurrentSelection);
ExpertMode:=ReadBool(section,'ExpertMode',ExpertMode);
i:=ReadInteger(section,'Cols',0);
if i>0 then moongrid.colcount:=i;
for i:=0 to moongrid.colcount-1 do
  moongrid.ColWidths[i]:=ReadInteger(section,'ColWidth'+inttostr(i),moongrid.ColWidths[i]);
for i:=0 to Selection.CheckListBox1.Count-1 do
  Selection.CheckListBox1.checked[i]:=ReadBool(section,'Selection1'+inttostr(i),true);
for i:=0 to Selection.CheckListBox2.Count-1 do
  Selection.CheckListBox2.checked[i]:=ReadBool(section,'Selection2'+inttostr(i),true);
end;
inif.Free;
end;

procedure Tf_main.SaveDefault;
var
    inif: TMemIniFile;
    i : integer;
    section: string;
begin
inif:=Tmeminifile.create(configfile);
try
section:='DatLun';
with inif do begin
  WriteInteger(section,'Top',f_main.Top);
  WriteInteger(section,'Left',f_main.Left);
  WriteInteger(section,'Height',f_main.Height);
  WriteInteger(section,'Width',f_main.Width);
  WriteBool(section,'Maximized',(f_main.windowstate=wsMaximized));
  WriteString(section,'DBselection',dbselection);
  WriteString(section,'CurrentSelection',CurrentSelection);
  WriteBool(section,'ExpertMode',ExpertMode);
  WriteInteger(section,'Cols',moongrid.colcount);
  for i:=0 to moongrid.colcount do
    WriteInteger(section,'ColWidth'+inttostr(i),moongrid.ColWidths[i]);
  for i:=0 to Selection.CheckListBox1.Count-1 do
    WriteBool(section,'Selection1'+inttostr(i),Selection.CheckListBox1.checked[i]);
  for i:=0 to Selection.CheckListBox2.Count-1 do
    WriteBool(section,'Selection2'+inttostr(i),Selection.CheckListBox2.checked[i]);
  inif.UpdateFile;
end;
finally
inif.Free;
end;
end;

procedure Tf_main.Select;
var ok: boolean;
    buf: string;
    p: integer;
begin
screen.Cursor:=crHourGlass;
try
dbm.DataBase:=Loadcsv.dbname;
buf:=trim(currentselection);
p:=pos('ORDER BY',uppercase(buf));
if p>0 then begin
   currentsort:=uppercase(copy(buf,p+8,999));
   buf:=trim(copy(buf,1,p-1));
   p:=pos('DESC',currentsort);
   if p>0 then begin
      currentsort:=trim(copy(currentsort,1,p-1));
      sortorder:=0;
   end else begin
      p:=pos('ASC',currentsort);
      if p>0 then begin
         currentsort:=trim(copy(currentsort,1,p-1));
         sortorder:=1;
      end else begin
         currentsort:=trim(currentsort);
         sortorder:=1;
      end;
   end;
end else begin
   currentsort:='';
end;
if currentselection='' then
   ok:=dbm.query('select * from moon order by DBN,NAME;')
else if buf<>'' then
   ok:=dbm.query('select * from moon where '+currentselection+';')
else
   ok:=dbm.query('select * from moon '+currentselection+';');
if ok then begin
 if dbm.RowCount>0 then begin
    ScrollBar1.Max:=max(dbm.RowCount,MoonGrid.RowCount);
 end
 else Showmessage(rsm_2);
end
else Showmessage(dbm.GetErrorMessage);
currentrow:=0;
ScrollBar1.Position:=0;
ScrollBar1.Visible:=(ScrollBar1.Max>MoonGrid.RowCount);
finally
screen.Cursor:=crDefault;
end;
end;

procedure Tf_main.ClearSelection;
var sel: TGridRect;
begin
 sel.Top:=1; sel.Left:=2; sel.Bottom:=1; sel.Right:=MoonGrid.ColCount;
 MoonGrid.Selection:=sel;
end;

procedure Tf_main.RefreshGrid;
var dbcol,dbrow,dbn,i:integer;
    buf:string;
begin
screen.Cursor:=crHourGlass;
try
 ClearSelection;
 setlength(IDlist,MoonGrid.RowCount+2);
 for i:=0 to MoonGrid.RowCount+1 do IDlist[i]:=-1;
 if (currentrow>0)and(currentrow+MoonGrid.RowCount>dbm.RowCount) then begin   // last line.
    currentrow:=max(0,dbm.RowCount-MoonGrid.RowCount+1);
    ScrollBar1.Position:=currentrow;
 end;
 for dbrow:=currentrow to currentrow+MoonGrid.RowCount-2 do begin
  buf:=dbm.Results[dbrow][0];
  IDlist[dbrow-currentrow+1]:=strtointdef(buf,-1);
  buf:=dbm.Results[dbrow][1];
  dbn:=strtointdef(buf,-1);
  if (dbn>0)and(dbn<=numdb) then buf:=dbshortname[dbn];
  MoonGrid.Cells[0,dbrow-currentrow+1]:=buf;
  for dbcol:=2 to MoonGrid.ColCount do begin
    if MoonGrid.ColWidths[dbcol-1]>0 then
       MoonGrid.Cells[dbcol-1,dbrow-currentrow+1]:=dbm.Results[dbrow][dbcol]
    else
       MoonGrid.Cells[dbcol-1,dbrow-currentrow+1]:='';
  end;
 end;
 if currentselection='' then buf:='*'
    else buf:=currentselection;
 panel1.Caption:=rsm_4+' '+inttostr(currentrow+1)+'/'+inttostr(dbm.RowCount)+'   '+rsm_3+' '+buf;
finally
screen.Cursor:=crDefault;
end;
end;

procedure Tf_main.ScrollBar1Change(Sender: TObject);
begin
currentrow:=ScrollBar1.Position;
RefreshGrid;
MoonGrid.SetFocus;
end;

procedure Tf_main.MoonGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 MouseX:=X;
 MouseY:=Y;
end;

procedure Tf_main.MoonGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var col,row:integer;
begin
SelectedObject:='';
if Button=mbRight then begin
  // popup
end else begin
 MoonGrid.MouseToCell(X, Y, Col, Row);
 if (col>=0) and (row>=0) then begin
   if row=0 then begin  // column title -> Sort data
     if (abs(mousex-x)+abs(mousey-y))<2 then begin
        SortByCol(col);
     end;
   end
   else begin // object name -> select to vma
      if trim(MoonGrid.Cells[1,row])<>'' then SelectedObject:=MoonGrid.Cells[1,row];
   end;
 end;
end;
end;

procedure Tf_main.MoonGridDblClick(Sender: TObject);
begin
 if SelectedObject<>'' then OpenVMA(SelectedObject,'');
end;

Procedure Tf_main.SortByCol(col:integer);
var i,p,q:integer;
    buf,direction: string;
begin
direction:='ASC';
p:=pos('ORDER BY',uppercase(currentselection));
if p>0 then begin
  buf:=copy(currentselection,p,9999)+' ';
  currentselection:=copy(currentselection,1,p-1);
  q:=pos(' '+MoonGrid.Cells[col,0]+' ',buf);
  if q>0 then begin
     if pos('DESC',buf)>0 then direction:='ASC'
         else direction:='DESC';
  end;
end;
currentselection:=trim(currentselection)+' ORDER BY '+MoonGrid.Cells[col,0]+' '+direction;
Select;
for i:=1 to MoonGrid.ColCount do 
    MoonGrid.Cells[i-1,0]:=dbm.GetField(i);
RefreshGrid;
end;

procedure Tf_main.PopupMenu1Popup(Sender: TObject);
begin
Sortby1.Caption:=SortCaption+': '+MoonGrid.Cells[HintY,0];
Find1.Caption:=FindCaption+': '+MoonGrid.Cells[HintY,0];
OpeninVMA1.Caption:=ShowCaption+': '+MoonGrid.Cells[1,HintX];
OpeninVMA1.Visible:=HintX>0;
end;

procedure Tf_main.Selectfrom1Click(Sender: TObject);
var s: string;
    i,n: integer;
begin
  Selection.lastselection:=CurrentSelection;
  Selection.fieldlist2.Text:=MoonGrid.Cells[HintY,0];
  Selection.PageControl1.ActivePageIndex:=1;
  Selection.ShowModal;
  if selection.ModalResult=mrOK then begin
     ExpertMode:=Selection.ExpertMode.Checked;
     currentselection:=trim(selection.sel.Text);
     if Selection.DBselectionChanged then begin
        for i:=1 to 9 do begin
          s:=inttostr(i);
          n:=pos(s,dbselection);
          usedatabase[i]:=(n>0);
        end;
        LoadDB(dbm);
     end;
     Select;
     RefreshGrid;
  end;
end;

procedure Tf_main.Sortby1Click(Sender: TObject);
begin
  SortByCol(HintY);
end;

procedure Tf_main.Find1Click(Sender: TObject);
begin
  FindCol:=HintY;
  FindFrom:=currentrow+HintX;
  finddialog1.Execute;
end;

procedure Tf_main.OpeninVMA1Click(Sender: TObject);
begin
  if trim(MoonGrid.Cells[1,HintX])<>'' then OpenVMA(MoonGrid.Cells[1,HintX],'');
end;

procedure Tf_main.OpenPhotlun1Click(Sender: TObject);
begin
  if trim(MoonGrid.Cells[1,HintX])<>'' then OpenPhotLun(MoonGrid.Cells[1,HintX],'');
end;

procedure Tf_main.MoonGridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var col,row:integer;
begin
 MoonGrid.MouseToCell(X, Y, Col, Row);
 if (col>=0) and (row>=0) then begin
   if (HintX<>row)or(HintY<>col) then begin
      moongrid.Hint:='';
      moongrid.ShowHint:=false;
      HintX:=row;
      HintY:=col;
   end else begin
      if (trim(MoonGrid.Cells[col,row])<>'')and(MoonGrid.Canvas.TextWidth(MoonGrid.Cells[col,row])>MoonGrid.ColWidths[col]) then begin
         moongrid.Hint:=MoonGrid.Cells[col,row];
         moongrid.ShowHint:=true;
      end;
   end;
 end;
end;

procedure Tf_main.Selection1Click(Sender: TObject);
begin
  formpos(selection,mouse.CursorPos.X,mouse.CursorPos.Y);
  selection.sel.Text:=currentselection;
  selection.ShowModal;
  if selection.ModalResult=mrOK then begin
     ExpertMode:=Selection.ExpertMode.Checked;
     currentselection:=trim(selection.sel.Text);
     Select;
     RefreshGrid;
  end;
end;

procedure Tf_main.Columns1Click(Sender: TObject);
begin
formpos(Columns,mouse.CursorPos.X,mouse.CursorPos.Y);
Columns.ShowModal;
FormResize(sender);
end;

procedure Tf_main.OpenVMA(objname,otherparam:string);
var param:string;
begin
    param:='-nd ';
    if CanCloseVMA and (not StartVMA) then begin
      param:=param+' -3d ';
    end;
    if objname<>'' then param:=param+' -n "'+objname+'" ';
    param:=param+otherparam;
    chdir(appdir);
    Execnowait(maplun+' '+param);
    StartVMA:=true;
end;

procedure Tf_main.OpenPhotlun(objname,otherparam:string);
var param:string;
begin
    param:='-nd ';
    if objname<>'' then param:=param+' -n "'+objname+'" ';
    param:=param+otherparam;
    chdir(appdir);
    Execnowait(photlun+' '+param);
    StartPhotlun:=true;
end;

procedure Tf_main.ShowSelection1Click(Sender: TObject);
var buf,oname:string;
    p,i: integer;
    first:boolean;
    sel: TGridRect;
begin
buf:='';
oname:='';
sel:=MoonGrid.Selection;
if (sel.Bottom>sel.Top) then begin
  first:=true;
  for i:=sel.Top to sel.Bottom do begin
     if IDlist[i]>0 then
        if first then begin
           buf:=inttostr(IDlist[i]);
           oname:=MoonGrid.Cells[1,i];
           first:=false;
        end else
           buf:=buf+','+inttostr(IDlist[i]);
  end;
  if buf>'' then begin
    buf:='ID in ('+buf+')';
  end;
end
else
  oname:=MoonGrid.Cells[1,1];
if buf='' then begin
  p:=pos('ORDER BY',uppercase(currentselection));
  if p>0 then buf:=copy(currentselection,1,p-1)
         else buf:=currentselection;
  if trim(buf)='' then buf:='DBN>0';
end;
buf:=stringreplace(buf,'"','''',[rfReplaceAll]);
OpenVMA(oname,' -s "'+buf+'"');
end;

procedure Tf_main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
SaveDefault;
if CanCloseVMA and StartVMA then OpenVMA('','-quit');
if CanClosePhotLun and StartPhotlun then OpenPhotLun('','-quit');
end;

procedure Tf_main.FindDialog1Find(Sender: TObject);
var buf1,buf2: string;
    toupper,WholeWord: boolean;
    i: integer;
    findok:boolean;
begin
findok:=false;
toupper := not (frMatchCase in FindDialog1.Options);
WholeWord:= (frWholeWord in FindDialog1.Options);
buf1:=FindDialog1.FindText;
if toupper then buf1:=uppercase(buf1);
if WholeWord then buf1:=' '+buf1+' ';
if frDown in FindDialog1.Options then
 for i:=Findfrom to dbm.RowCount do begin
   buf2:=dbm.Results[i][Findcol+1];
   if toupper then buf2:=uppercase(buf2);
   if WholeWord then buf2:=' '+buf2+' ';
   if pos(buf1,buf2)>0 then begin
      currentrow:=i;
      FindFrom:=currentrow+1;
      ScrollBar1.Position:=currentrow;
      RefreshGrid;
      findok:=true;
      break;
   end;
 end
else
 for i:=Findfrom downto 0 do begin
   buf2:=dbm.Results[i][Findcol+1];
   if toupper then buf2:=uppercase(buf2);
   if WholeWord then buf2:=' '+buf2+' ';
   if pos(buf1,buf2)>0 then begin
      currentrow:=i;
      FindFrom:=currentrow-1;
      ScrollBar1.Position:=currentrow;
      RefreshGrid;
      findok:=true;
      break;
   end;
 end;
if not findok then showmessage(FindDialog1.FindText+' '+rsm_5+' '+Moongrid.Cells[Findcol,0]);
end;

procedure Tf_main.Export1Click(Sender: TObject);
var f: textfile;
    buf: string;
    dbcol,dbrow: integer;            
begin
if dbm.RowCount=0 then exit;
chdir(appdir);
if savedialog1.Execute then begin
   chdir(appdir);
   assignfile(f,savedialog1.FileName);
   rewrite(f);
   buf:='';
   for dbcol:=2 to dbm.ColCount-1 do begin
      if moongrid.ColWidths[dbcol-1]>0 then
         buf:=buf+dbm.GetField(dbcol)+';';
   end;
   writeln(f,buf);
   buf:='Lunar formations database V2.1;© Copyright Christian Legrand;';
   writeln(f,buf);
   buf:='For personal use only;';
   writeln(f,buf);
   for dbrow:=0 to dbm.RowCount-1 do begin
      buf:='';
      for dbcol:=2 to dbm.ColCount-1 do begin  // ne pas exporter DBN
         if moongrid.ColWidths[dbcol-1]>0 then
            buf:=buf+dbm.Results[dbrow][dbcol]+';';
      end;
      writeln(f,buf);
   end;
   closefile(f);
end;
chdir(appdir);
end;

{ petite fonction de test avec resultat dans un fichier
procedure Tf_main.Button1Click(Sender: TObject);
var ok:boolean;
    i: integer;
    f:textfile;
begin
ok:=dbm.Query('select distinct(TYPE) from moon');
if ok then begin
 assignfile(f,'type.txt');
 rewrite(f);
 for i:=0 to dbm.RowCount do
    writeln(f,dbm.Results[i][0]);
 Closefile(f);
end
else Showmessage(dbm.GetErrorMessage);
select;
end;}

procedure Tf_main.Import1Click(Sender: TObject);
begin
formpos(loadcsv,mouse.CursorPos.X,mouse.CursorPos.Y);
loadcsv.showmodal;
currentselection:=dbselection;
Select;
RefreshGrid;
end;

procedure Tf_main.Delete1Click(Sender: TObject);
begin
if currentselection='' then begin showmessage(rsm_6);exit;end;
if messagedlg(rsm_7,mtConfirmation,[mbYes,mbNo],0)=mrYes then begin
   dbjournal(extractfilename(dbm.DataBase),'DELETE WHERE '+currentselection);
   if dbm.query('delete from moon where '+currentselection+';') then begin
      dbselection:='DBN in (1,2,3,4,5,6,7)';
      currentselection:=dbselection;
      RemoveUnusedDBN;
      Select;
      RefreshGrid;
   end
   else Showmessage(dbm.GetErrorMessage);
end;
end;

Procedure Tf_main.RemoveUnusedDBN;
begin
if not dbm.Query('delete from user_database where DBN in ('
                 +'select DBN from user_database where dbn not in ('
                 +'select distinct(DBN) from moon))')
then Showmessage(dbm.GetErrorMessage);
end;

procedure Tf_main.APropos1Click(Sender: TObject);
begin
  Showmessage('Datlun '+Splashversion+crlf+
              compile_version+crlf+
              avlcpy+crlf+crlf+
              'Conception : Christian Legrand'+crlf+
              'Programming : Patrick Chevalley'+crlf+crlf+
              'This program is free software; you can redistribute it and/or '+crlf+
              'modify it under the terms of the GNU General Public License '+crlf+
              'as published by the Free Software Foundation.'
);
end;

procedure Tf_main.Help2Click(Sender: TObject);
begin
showhelpdoc('Doc','_DatLun','doc');
end;

procedure Tf_main.MoonGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
if (ARow=0)and(MoonGrid.Cells[ACol,ARow]=currentsort) then
   Imagelist1.Draw(MoonGrid.Canvas,Rect.Right-Imagelist1.width,Rect.Top,sortorder,true);
end;

procedure Tf_main.Default1Click(Sender: TObject);
begin
dbselection:='DBN in (1,2,3,4,5,6,7)';
Columns.ButtonAllClick(Self);
Selection.ButtonAllClick(Self);
currentselection:=trim(selection.sel.Text);
Select;
RefreshGrid;
end;

end.
