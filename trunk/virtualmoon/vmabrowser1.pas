unit vmabrowser1;
{
Copyright (C) 2006 Patrick Chevalley

http://www.astrosurf.com/avl
pch@freesurf.ch

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Grids, Math, passql, passqlite, skylib, dbutil, StdCtrls,
  splashunit, ExtCtrls, IniFiles, ImgList;

const
  IdMsg='Virtual_Moon_Atlas_Browser_message';
  ExitProMsg='Virtual_Moon_Atlas_Pro_exit';
  nummessage = 30;
  numdb=5;
  numdbtype = 24;
  Splashversion ='Version 3.5 2006-11-4';
  versionname='DATLUN';

type
  TForm1 = class(TForm)
    MoonGrid: TStringGrid;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Quit1: TMenuItem;
    ScrollBar1: TScrollBar;
    Panel1: TPanel;
    Selection1: TMenuItem;
    Columns1: TMenuItem;
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
    ShowSelection1: TMenuItem;
    Help1: TMenuItem;
    Help2: TMenuItem;
    APropos1: TMenuItem;
    Sortby1: TMenuItem;
    ImageList1: TImageList;
    N3: TMenuItem;
    Databasemaintenance1: TMenuItem;
    Default1: TMenuItem;
    N4: TMenuItem;
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
    vmaexe: string;
    currentrow,FindFrom,FindCol, sortorder: integer;
    currentselection, currentsort, transmsg, SelectedObject: string;
    FindCaption, ShowCaption, SortCaption: string;
    MouseX, MouseY, HintX, HintY: integer;
    StartVMA,CanCloseVMA, ExpertMode: boolean;
    IDlist: array of integer;
    param : Tstringlist;
    splash: Tsplash;
    Procedure SetLang;
    procedure SaveDefault;
    Procedure ReadParam;
    Procedure Readdefault;
    procedure RefreshGrid;
    procedure Select;
    procedure ClearSelection;
    procedure RemoveUnusedDBN;
    procedure OpenVMA(objname,otherparam:string);
    procedure SortByCol(col:integer);
  public
    { Public declarations }
    dbm: TLiteDB;
    dbselection: string;
    m : array[1..nummessage] of string;
    dbtype : array[1..numdbtype] of string;
    dbshortname : array[1..numdb] of string;
  end;

var
  Form1: TForm1;
  MyMsg : DWord; {custom systemwide message}
  OldWindowProc : Pointer; {Variable for the old windows proc}

type
 TSwitchToThisWindow = procedure (h1: hWnd; restore: bool); stdcall;
var
 SwitchToThisWindow: TSwitchToThisWindow;
 user32dll: Dword;

implementation

uses vmabrowser2, vmabrowser3, vmabrowser4, vmabrowser5;

{$R *.dfm}

procedure TForm1.Quit1Click(Sender: TObject);
begin
close;
end;

Procedure TForm1.SetLang;
var section,buf : string;
    inifile : Tmeminifile;
    i : integer;
const deftxt='?';
begin
language:='UK';
inifile:=Tmeminifile.create(Slash(AppDir)+'virtualmoon.ini');
with inifile do begin
section:='default';
buf:=ReadString(section,'Language',language);
end;
inifile.Free;
chdir(appdir);
if fileexists(Slash(AppDir)+'lang_'+buf+'.ini') then language:=buf;
inifile:=Tmeminifile.create(Slash(AppDir)+'lang_'+language+'.ini');
with inifile do begin
  section:='default';
  transmsg:=ReadString(section,'translator','');
  skylib.hp:=ReadString(section,'help_prefix','UK')+'_';
  Help1.caption:=ReadString(section,'t_15',Help1.caption);
  Help2.caption:=Help1.caption;
  APropos1.caption:=ReadString(section,'t_16',APropos1.caption);
  section:='datlun';
  file1.caption:=ReadString(section,'t_1',file1.caption);
  quit1.caption:=ReadString(section,'t_2',quit1.caption);
  vmabrowser2.Selection.caption:=ReadString(section,'t_3',vmabrowser2.Selection.caption);
  Selection1.caption:=vmabrowser2.Selection.caption;
  vmabrowser3.Columns.caption:=ReadString(section,'t_4',vmabrowser3.Columns.caption);
  Columns1.caption:=vmabrowser3.Columns.caption;
  vmabrowser3.Columns.ButtonAll.caption:=ReadString(section,'t_5',vmabrowser3.Columns.ButtonAll.caption);
  vmabrowser2.Selection.ButtonAll.caption:=vmabrowser3.Columns.ButtonAll.caption;
  vmabrowser3.Columns.ButtonNone.caption:=ReadString(section,'t_6',vmabrowser3.Columns.ButtonNone.caption);
  vmabrowser2.Selection.ButtonNone.caption:=vmabrowser3.Columns.ButtonNone.caption;
  vmabrowser3.Columns.ButtonClose.caption:=ReadString(section,'t_7',vmabrowser3.Columns.ButtonClose.caption);
  vmabrowser2.Selection.Button10.caption:=ReadString(section,'t_8',vmabrowser2.Selection.Button10.caption);
  vmabrowser2.Selection.Button11.caption:=ReadString(section,'t_9',vmabrowser2.Selection.Button11.caption);
  vmabrowser2.Selection.Button9.caption:=ReadString(section,'t_10',vmabrowser2.Selection.Button9.caption);
  vmabrowser2.Selection.Button13.caption:=ReadString(section,'t_11',vmabrowser2.Selection.Button13.caption);
  vmabrowser2.Selection.label1.caption:=ReadString(section,'t_12',vmabrowser2.Selection.label1.caption);
  for i:=1 to nummessage do begin
    m[i]:=ReadString(section,'m_'+trim(inttostr(i)),deftxt);
  end;
  vmabrowser5.SelectDB.dbn1:=ReadString(section,'t_15','Near Side Named Formation');
  vmabrowser5.SelectDB.dbn2:=ReadString(section,'t_16','Near Side Indexed Craters');
  vmabrowser5.SelectDB.dbn3:=ReadString(section,'t_18','Far Side Named Formation');
  vmabrowser5.SelectDB.dbn4:=ReadString(section,'t_19','Far Side Indexed Craters');
  vmabrowser5.SelectDB.dbn5:=ReadString(section,'t_20','Historical Sites');
  vmabrowser5.SelectDB.Button1.Caption:=ReadString(section,'t_8',vmabrowser5.SelectDB.Button1.caption);
  vmabrowser5.SelectDB.Button2.Caption:=ReadString(section,'t_9',vmabrowser5.SelectDB.Button2.caption);
  vmabrowser5.SelectDB.Button3.Caption:=ReadString(section,'t_5',vmabrowser5.SelectDB.Button3.caption);
  vmabrowser5.SelectDB.Button4.Caption:=ReadString(section,'t_6',vmabrowser5.SelectDB.Button4.caption);
  vmabrowser2.Selection.TabSheet1.Caption:=ReadString(section,'t_21',vmabrowser2.Selection.TabSheet1.Caption);
  vmabrowser2.Selection.TabSheet3.Caption:=ReadString(section,'t_22',vmabrowser2.Selection.TabSheet2.Caption);
  vmabrowser4.LoadCSV.label9.Caption:=ReadString(section,'t_17',vmabrowser4.LoadCSV.label9.Caption);
  vmabrowser4.LoadCSV.TabSheet1.Caption:=ReadString(section,'t_23',vmabrowser4.LoadCSV.TabSheet1.Caption);
  vmabrowser4.LoadCSV.TabSheet2.Caption:=ReadString(section,'t_24',vmabrowser4.LoadCSV.TabSheet2.Caption);
  vmabrowser4.LoadCSV.TabSheet3.Caption:=ReadString(section,'t_25',vmabrowser4.LoadCSV.TabSheet3.Caption);
  vmabrowser4.LoadCSV.label1.Caption:=ReadString(section,'t_26',vmabrowser4.LoadCSV.label1.Caption);
  vmabrowser4.LoadCSV.label2.Caption:=ReadString(section,'t_27',vmabrowser4.LoadCSV.label2.Caption);
  vmabrowser4.LoadCSV.label4.Caption:=ReadString(section,'t_28',vmabrowser4.LoadCSV.label4.Caption);
  vmabrowser4.LoadCSV.label3.Caption:=ReadString(section,'t_29',vmabrowser4.LoadCSV.label3.Caption);
  vmabrowser4.LoadCSV.label6.Caption:=ReadString(section,'t_30',vmabrowser4.LoadCSV.label6.Caption);
  vmabrowser4.LoadCSV.AssignConstant.Caption:=ReadString(section,'t_31',vmabrowser4.LoadCSV.AssignConstant.Caption);
  vmabrowser4.LoadCSV.AssignField.Hint:=ReadString(section,'t_47',vmabrowser4.LoadCSV.AssignField.Hint);
  vmabrowser4.LoadCSV.Button3.Caption:=ReadString(section,'t_32',vmabrowser4.LoadCSV.Button3.Caption);
  Databasemaintenance1.Caption:=ReadString(section,'t_14',Databasemaintenance1.Caption);
  Export1.Caption:=ReadString(section,'t_33',Export1.Caption);
  Import1.Caption:=ReadString(section,'t_34',Import1.Caption);
  Delete1.Caption:=ReadString(section,'t_35',Delete1.Caption);
  ShowSelection1.Caption:=ReadString(section,'t_36',ShowSelection1.Caption);
  vmabrowser4.LoadCSV.caption:=Import1.Caption;
  vmabrowser4.LoadCSV.label7.Caption:=ReadString(section,'t_37',vmabrowser4.LoadCSV.label7.Caption);
  vmabrowser4.LoadCSV.Button1.Caption:=ReadString(section,'t_38',vmabrowser4.LoadCSV.Button1.Caption);
  vmabrowser4.LoadCSV.label8.Caption:=ReadString(section,'t_39',vmabrowser4.LoadCSV.label8.Caption);
  vmabrowser4.LoadCSV.Button2.Caption:=ReadString(section,'t_40',vmabrowser4.LoadCSV.Button2.Caption);
  FindCaption:=ReadString(section,'t_41',Find1.Caption);
  ShowCaption:=ReadString(section,'t_42',OpeninVMA1.Caption);
  SortCaption:=ReadString(section,'t_43',SortBy1.Caption);
  vmabrowser2.Selection.TabSheet2.Caption:=ReadString(section,'t_44',vmabrowser2.Selection.TabSheet2.Caption);
  vmabrowser2.Selection.RadioGroup2.items[3]:=ReadString(section,'t_45',vmabrowser2.Selection.RadioGroup2.items[3]);
  vmabrowser2.Selection.StaticText1.Caption:=ReadString(section,'t_46',vmabrowser2.Selection.StaticText1.Caption);
  vmabrowser2.Selection.button19.Caption:=ReadString(section,'t_48',vmabrowser2.Selection.button19.Caption);
  vmabrowser2.Selection.ExpertMode.Caption:=ReadString(section,'t_49',vmabrowser2.Selection.ExpertMode.Caption);
  Default1.Caption:=ReadString(section,'t_50',Default1.Caption);
  section:='database';
  vmabrowser2.Selection.Checklistbox1.Items.Clear;
  for i:=1 to numdbtype do begin
    dbtype[i]:=ReadString(section,'t_'+trim(inttostr(i)),deftxt);
    vmabrowser2.Selection.Checklistbox1.Items.Add(dbtype[i]);
  end;
  section:='db_shortname';
  for i:=1 to numdb do begin
    dbshortname[i]:=ReadString(section,'t_'+trim(inttostr(i)),'db'+trim(inttostr(i)));
  end;
end;
inifile.free;
buf:=slash(appdir)+slash('doc')+'loadcsv_'+language+'.txt';
if not fileexists(buf) then buf:=slash(appdir)+slash('doc')+'loadcsv_UK.txt';
try
if fileexists(buf) then  vmabrowser4.LoadCSV.memo2.Lines.LoadFromFile(BUF);
except
end;

end;

procedure LoadSwitchToThisWindow;
begin
 SwitchToThisWindow:=nil;
 user32dll:=LoadLibrary(Pchar('user32.dll'));
 if user32dll<>0 then begin
    SwitchToThisWindow := TSwitchToThisWindow(GetProcAddress(user32dll, 'SwitchToThisWindow'));
 end;
end;

function NewWindowProc(WindowHandle : hWnd;TheMessage: Dword; ParamW: LongInt; ParamL: LongInt) : LongInt stdcall;
var
    buf : string;
    f : textfile;
begin
  if TheMessage = MyMsg  then begin
    try
   {Tell the application to restore, let it restore the form}
    SendMessage(Application.handle, WM_SYSCOMMAND, SC_RESTORE, 0);
    if @SwitchToThisWindow=nil then
       SetForegroundWindow(Application.Handle)
    else
       SwitchToThisWindow(Application.Handle,true);
    application.ProcessMessages;
    if ParamW > 0 then begin
       chdir(appdir);
       filemode:=0;
       assignfile(f,'@@param@@');
       reset(f);
       Form1.param.Clear;
       while not eof(f) do begin
          readln(f,buf);
          Form1.param.add(buf);
       end;
       Closefile(f);
       deletefile('@@param@@');
       Form1.ReadParam;
    end;
    finally
   {We handled the message - we are done}
    Result := 0;
    end;
    exit;
  end;
 {Call the original winproc}
  Result := CallWindowProc(OldWindowProc,
                           WindowHandle,
                           TheMessage,
                           ParamW,
                           ParamL);
end;

procedure TForm1.FormCreate(Sender: TObject);
var i: integer;
begin
dbm:=TLiteDB.Create(self);
appdir:=getcurrentdir;
param:=Tstringlist.Create;
param.clear;
if paramcount>0 then begin
   for i:=1 to paramcount do begin
      param.Add(paramstr(i));
   end;
end;
{Register a custom windows message}
MyMsg := RegisterWindowMessage(IdMsg);
{Set form1's windows proc to ours and remember the old window proc}
OldWindowProc := Pointer(SetWindowLong(Form1.Handle,GWL_WNDPROC,LongInt(@NewWindowProc)));
StartVMA:=false;
CanCloseVMA:=true;
SelectedObject:='';
dbselection:='DBN in (1,2,3,4,5)';
currentselection:=dbselection;
ExpertMode:=false;
vmaexe:='vmapro.exe';
LoadSwitchToThisWindow;
Application.HintHidePause:=10000;
end;

Procedure TForm1.ReadParam;
var i : integer;
begin
i:=0;
while i <= param.count-1 do begin
if param[i]='-nx' then begin       // when started by vma do not close vma on exit!
   CanCloseVMA:=false;
end
else if param[i]='--' then begin   // last parameter
     break;
end;
inc(i);
end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
MoonGrid.RowCount:=max(2,(MoonGrid.ClientHeight div (MoonGrid.DefaultRowHeight+MoonGrid.GridLineWidth)) );
ScrollBar1.PageSize:=MoonGrid.RowCount-1;
ScrollBar1.LargeChange:=MoonGrid.RowCount-1;
ScrollBar1.Visible:=(ScrollBar1.Max>MoonGrid.RowCount);
if dbm.RowCount>0 then RefreshGrid;
end;

procedure TForm1.FormShow(Sender: TObject);
var dbcol,i:integer;
begin
SetLang;
ReadDefault;
for i:=1 to 5 do usedatabase[i]:=true;
for i:=6 to maxdbn do usedatabase[i]:=false;
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
Loadcsv.CheckListBox1.Items.Clear;
Selection.fieldlist.Items.Clear;
Selection.fieldlist2.Items.Clear;
if dbm.RowCount>0 then begin
  MoonGrid.ColCount:=dbm.ColCount-1;    // pas l'id
  MoonGrid.FixedCols:=2;
  MoonGrid.ColWidths[0]:=30;
  MoonGrid.ColWidths[1]:=150;
  for dbcol:=1 to MoonGrid.ColCount do begin
    MoonGrid.Cells[dbcol-1,0]:=dbm.GetField(dbcol);
    Columns.CheckListBox1.Items.Add(MoonGrid.Cells[dbcol-1,0]);
    Columns.CheckListBox1.Checked[dbcol-1]:=(MoonGrid.ColWidths[dbcol-1]>0);
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

Procedure TForm1.Readdefault;
var inif: TMemIniFile;
    section : string;
    i: integer;
begin

inif:=Tmeminifile.create(Slash(AppDir)+'datlun.ini');
section:='default';
with inif do begin
i:=ReadInteger(section,'Top',0);
if (i>=-10)and(i<screen.Height-20) then form1.Top:=i else form1.Top:=0;
i:=ReadInteger(section,'Left',0);
if (i>=-10)and(i<screen.width-20) then form1.Left:=i else form1.Left:=0;
i:=screen.height-50;
i:=minintvalue([i,ReadInteger(section,'Height',form1.height)]);
if (i>=20) then form1.Height:=i;
i:=screen.width-5;
i:=minintvalue([i,ReadInteger(section,'Width',form1.width)]);
if (i>=20) then form1.Width:=i;
if ReadBool(section,'Maximized',false) then form1.windowstate:=wsMaximized;
dbselection:=ReadString(section,'DBselection',dbselection);
CurrentSelection:=ReadString(section,'CurrentSelection',CurrentSelection);
ExpertMode:=ReadBool(section,'ExpertMode',ExpertMode);
moongrid.colcount:=ReadInteger(section,'Cols',0);
for i:=0 to moongrid.colcount-1 do
  moongrid.ColWidths[i]:=ReadInteger(section,'ColWidth'+inttostr(i),moongrid.ColWidths[i]);
for i:=0 to Selection.CheckListBox1.Count-1 do
  Selection.CheckListBox1.checked[i]:=ReadBool(section,'Selection'+inttostr(i),true);
end;
inif.Free;
end;

procedure Tform1.SaveDefault;
var
    inif: TMemIniFile;
    i : integer;
    section: string;
begin
inif:=Tmeminifile.create(Slash(AppDir)+'datlun.ini');
try
section:='default';
with inif do begin
  WriteInteger(section,'Top',Form1.Top);
  WriteInteger(section,'Left',Form1.Left);
  WriteInteger(section,'Height',Form1.Height);
  WriteInteger(section,'Width',Form1.Width);
  WriteBool(section,'Maximized',(form1.windowstate=wsMaximized));
  WriteString(section,'DBselection',dbselection);
  WriteString(section,'CurrentSelection',CurrentSelection);
  WriteBool(section,'ExpertMode',ExpertMode);
  WriteInteger(section,'Cols',moongrid.colcount);
  for i:=0 to moongrid.colcount do
    WriteInteger(section,'ColWidth'+inttostr(i),moongrid.ColWidths[i]);
  for i:=0 to Selection.CheckListBox1.Count-1 do
    WriteBool(section,'Selection'+inttostr(i),Selection.CheckListBox1.checked[i]);
  inif.UpdateFile;
end;
finally
inif.Free;
end;
end;

procedure TForm1.Select;
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
 else Showmessage(m[2]);
end
else Showmessage(dbm.GetErrorMessage);
currentrow:=0;
ScrollBar1.Position:=0;
ScrollBar1.Visible:=(ScrollBar1.Max>MoonGrid.RowCount);
finally
screen.Cursor:=crDefault;
end;
end;

procedure TForm1.ClearSelection;
var sel: TGridRect;
begin
 sel.Top:=1; sel.Left:=2; sel.Bottom:=1; sel.Right:=MoonGrid.ColCount;
 MoonGrid.Selection:=sel;
end;

procedure TForm1.RefreshGrid;
var dbcol,dbrow,dbn,i:integer;
    buf:string;
begin
 ClearSelection;
 setlength(IDlist,MoonGrid.RowCount+2);
 for i:=0 to MoonGrid.RowCount+1 do IDlist[i]:=-1;
 if (currentrow>0)and(currentrow+MoonGrid.RowCount>dbm.RowCount) then begin   // last line.
    currentrow:=max(0,dbm.RowCount-MoonGrid.RowCount+1);
    ScrollBar1.Position:=currentrow;
 end;
 for dbrow:=currentrow to currentrow+MoonGrid.RowCount do begin
  buf:=dbm.Results[dbrow][0];
  IDlist[dbrow-currentrow+1]:=strtointdef(buf,-1);
  buf:=dbm.Results[dbrow][1];
  dbn:=strtointdef(buf,-1);
  if (dbn>0)and(dbn<=numdb) then buf:=dbshortname[dbn];
  MoonGrid.Cells[0,dbrow-currentrow+1]:=buf;
  for dbcol:=2 to MoonGrid.ColCount do begin
    MoonGrid.Cells[dbcol-1,dbrow-currentrow+1]:=dbm.Results[dbrow][dbcol];
  end;
 end;
 if currentselection='' then buf:='*'
    else buf:=currentselection;
 panel1.Caption:=m[4]+' '+inttostr(currentrow+1)+'/'+inttostr(dbm.RowCount)+'   '+m[3]+' '+buf;
// MoonGrid.Refresh;
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
currentrow:=ScrollBar1.Position;
RefreshGrid;
MoonGrid.SetFocus;
end;

procedure TForm1.MoonGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 MouseX:=X;
 MouseY:=Y;
end;

procedure TForm1.MoonGridMouseUp(Sender: TObject; Button: TMouseButton;
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

procedure TForm1.MoonGridDblClick(Sender: TObject);
begin
 if SelectedObject<>'' then OpenVMA(SelectedObject,'');
end;

Procedure TForm1.SortByCol(col:integer);
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

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
Sortby1.Caption:=SortCaption+': '+MoonGrid.Cells[HintY,0];
Find1.Caption:=FindCaption+': '+MoonGrid.Cells[HintY,0];
OpeninVMA1.Caption:=ShowCaption+': '+MoonGrid.Cells[1,HintX];
OpeninVMA1.Visible:=HintX>0;
end;

procedure TForm1.Selectfrom1Click(Sender: TObject);
begin
  Selection.fieldlist2.Text:=MoonGrid.Cells[HintY,0];
  Selection.PageControl1.ActivePageIndex:=1;
  Selection.ShowModal;
  if selection.ModalResult=mrOK then begin
     ExpertMode:=Selection.ExpertMode.Checked;
     currentselection:=trim(selection.sel.Text);
     Select;
     RefreshGrid;
  end;
end;

procedure TForm1.Sortby1Click(Sender: TObject);
begin
  SortByCol(HintY);
end;

procedure TForm1.Find1Click(Sender: TObject);
begin
  FindCol:=HintY;
  FindFrom:=currentrow+HintX;
  finddialog1.Execute;
end;

procedure TForm1.OpeninVMA1Click(Sender: TObject);
begin
  if trim(MoonGrid.Cells[1,HintX])<>'' then OpenVMA(MoonGrid.Cells[1,HintX],'');
end;

procedure TForm1.MoonGridMouseMove(Sender: TObject; Shift: TShiftState; X,
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

procedure TForm1.Selection1Click(Sender: TObject);
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

procedure TForm1.Columns1Click(Sender: TObject);
begin
formpos(Columns,mouse.CursorPos.X,mouse.CursorPos.Y);
Columns.ShowModal;
FormResize(sender);
end;

procedure TForm1.OpenVMA(objname,otherparam:string);
var param:string;
begin
    param:='';
    if CanCloseVMA and (not StartVMA){and(objname<>'')} then begin
      param:=param+' -3d ';
    //  param:=param+' -f '+formatfloat('0.0',500);
    end;
    if objname<>'' then param:=param+' -n "'+objname+'" ';
    param:=param+otherparam;
    ExecuteFile(vmaexe,param, appdir, SW_SHOWNOACTIVATE);
    StartVMA:=true;
end;

procedure TForm1.ShowSelection1Click(Sender: TObject);
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

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
SaveDefault;
if CanCloseVMA and StartVMA then SendMessage(HWND_BROADCAST,RegisterWindowMessage(ExitProMsg),0,0);
end;

procedure TForm1.FindDialog1Find(Sender: TObject);
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
if not findok then showmessage(FindDialog1.FindText+' '+m[5]+' '+Moongrid.Cells[Findcol,0]);
end;

procedure TForm1.Export1Click(Sender: TObject);
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
procedure TForm1.Button1Click(Sender: TObject);
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

procedure TForm1.Import1Click(Sender: TObject);
begin
formpos(loadcsv,mouse.CursorPos.X,mouse.CursorPos.Y);
loadcsv.showmodal;
currentselection:=dbselection;
Select;
RefreshGrid;
end;

procedure TForm1.Delete1Click(Sender: TObject);
begin
if currentselection='' then begin showmessage(m[6]);exit;end;
if messagedlg(m[7],mtConfirmation,[mbYes,mbNo],0)=mrYes then begin
   dbjournal(dbm,'DELETE WHERE '+currentselection);
   if dbm.query('delete from moon where '+currentselection+';') then begin
      dbselection:='DBN in (1,2,3,4,5)';
      currentselection:=dbselection;
      RemoveUnusedDBN;
      Select;
      RefreshGrid;
   end
   else Showmessage(dbm.GetErrorMessage);
end;
end;

Procedure TForm1.RemoveUnusedDBN;
begin
if not dbm.Query('delete from user_database where DBN in ('
                 +'select DBN from user_database where dbn not in ('
                 +'select distinct(DBN) from moon))')
then Showmessage(dbm.GetErrorMessage);
end;

procedure TForm1.APropos1Click(Sender: TObject);
begin
  splash := Tsplash.create(application);
  splashunit.SplashTimer:=false;
  splash.BorderStyle:=bsToolWindow;
  splash.caption:=stringreplace(Apropos1.caption,'&','',[]);;
  splash.VersionName:=VersionName;
  splash.Splashversion:=Splashversion;
  splash.transmsg:=transmsg;
  splash.show;
  splash.refresh;
end;

procedure TForm1.Help2Click(Sender: TObject);
begin
internalbrowser:=true;
showhelp('Doc','_DATLUN','doc');
end;

procedure TForm1.MoonGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
if (ARow=0)and(MoonGrid.Cells[ACol,ARow]=currentsort) then
   Imagelist1.Draw(MoonGrid.Canvas,Rect.Right-Imagelist1.width,Rect.Top,sortorder,true);
end;

procedure TForm1.Default1Click(Sender: TObject);
begin
dbselection:='DBN in (1,2,3,4,5)';
Columns.ButtonAllClick(Self);
Selection.ButtonAllClick(Self);
currentselection:=trim(selection.sel.Text);
Select;
RefreshGrid;
end;

end.
