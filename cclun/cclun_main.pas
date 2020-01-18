unit cclun_main;
{
Copyright (C) 2012 Patrick Chevalley

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

{$mode objfpc}{$H+}

interface

uses
  {$ifdef mswindows}
    Windows, ShlObj,
  {$endif}
  {$ifdef unix}
    unix,baseunix,
  {$endif}
  u_translation, u_constant, IniFiles, LazUTF8,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus;

type

  { Tf_cclun }

  Tf_cclun = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PanelCenter: TPanel;
    PanelBottom: TPanel;
    PanelTop: TPanel;
    PopupMenu1: TPopupMenu;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
  private
    { private declarations }
    mousex,mousey: integer;
    moveform: boolean;
    procedure GetAppDir;
    procedure SetLang;
    procedure OpenDoc(doc:string);
  public
    { public declarations }
  end; 

var
  f_cclun: Tf_cclun;

implementation

{$R *.lfm}

{ Tf_cclun }

Function NoSlash(nom : string) : string;
begin
result:=trim(nom);
if copy(result,length(nom),1)=PathDelim then result:=copy(result,1,length(nom)-1);
end;

Function Slash(nom : string) : string;
begin
result:=trim(nom);
if copy(result,length(nom),1)<>PathDelim then result:=result+PathDelim;
end;

function words(str,sep : string; p,n : integer) : string;
var     i,j : Integer;
begin
result:='';
str:=trim(str);
for i:=1 to p-1 do begin
 j:=pos(blank,str);
 if j=0 then j:=length(str)+1;
 str:=trim(copy(str,j,length(str)));
end;
for i:=1 to n do begin
 j:=pos(blank,str);
 if j=0 then j:=length(str)+1;
 result:=result+trim(copy(str,1,j))+sep;
 str:=trim(copy(str,j,length(str)));
end;
end;

procedure ExecNoWait(cmd: string; title:string=''; hide: boolean=true);
{$ifdef unix}
begin
 fpSystem(cmd+' &');
end;
{$endif}
{$ifdef mswindows}
var
   bchExec: array[0..1024] of char;
   pchEXEC: Pchar;
   si: TStartupInfo;
   pi: TProcessInformation;
begin
   pchExec := @bchExec;
   StrPCopy(pchExec,cmd);
   FillChar(si,sizeof(si),0);
   FillChar(pi,sizeof(pi),0);
   si.dwFlags:=STARTF_USESHOWWINDOW;
   if title<>'' then si.lpTitle:=Pchar(title);
   if hide then si.wShowWindow:=SW_SHOWMINIMIZED
           else si.wShowWindow:=SW_SHOWNORMAL;
   si.cb := sizeof(si);
   try
     CreateProcess(Nil,pchExec,Nil,Nil,false,CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, Nil,Nil,si,pi);
    except;
    end;
end;
{$endif}

{$ifdef unix}
function ExecFork(cmd:string;p1:string='';p2:string='';p3:string='';p4:string='';p5:string=''):integer;
var
  parg: array[1..7] of PChar;
begin
  result := fpFork;
  if result = 0 then
  begin
    parg[1] := Pchar(cmd);
    if p1='' then parg[2]:=nil else parg[2] := PChar(p1);
    if p2='' then parg[3]:=nil else parg[3] := PChar(p2);
    if p3='' then parg[4]:=nil else parg[4] := PChar(p3);
    if p4='' then parg[5]:=nil else parg[5] := PChar(p4);
    if p5='' then parg[6]:=nil else parg[6] := PChar(p5);
    parg[7] := nil;
    if fpExecVP(cmd,PPChar(@parg[1])) = -1 then
    begin
    end;
  end;
end;
{$endif}

Function ExecuteFile(const FileName: string): integer;
{$ifdef mswindows}
var
  zFileName, zParams, zDir: array[0..255] of Char;
begin
  Result := ShellExecute(Application.MainForm.Handle, nil, StrPCopy(zFileName, FileName),
                         StrPCopy(zParams, ''), StrPCopy(zDir, ''), SW_SHOWNOACTIVATE);
{$endif}
{$ifdef unix}
var cmd,p1,p2,p3,p4: string;
begin
  cmd:=trim(words(OpenFileCMD,blank,1,1));
  p1:=trim(words(OpenFileCMD,blank,2,1));
  p2:=trim(words(OpenFileCMD,blank,3,1));
  p3:=trim(words(OpenFileCMD,blank,4,1));
  p4:=trim(words(OpenFileCMD,blank,5,1));
  if p1='' then result:=ExecFork(cmd,FileName)
  else if p2='' then result:=ExecFork(cmd,p1,FileName)
  else if p3='' then result:=ExecFork(cmd,p1,p2,FileName)
  else if p4='' then result:=ExecFork(cmd,p1,p2,p3,FileName)
  else result:=ExecFork(cmd,p1,p2,p3,p4,FileName);
{$endif}
end;

function SafeUTF8ToSys(v:string):string;
begin
result:=UTF8ToSys(v);
if result='' then result:=v;
end;

procedure Tf_cclun.GetAppDir;
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

procedure Tf_cclun.SetLang;
var
  section: string;
  inifile: Tmeminifile;
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
  Caption  := rstitle;
  helpprefix := rshelp_prefix;
  Label1.Caption:=rsVirtualLunar+blank+AVLversion;
//  Label9.Caption:=rsSpecialEditi;
  Label8.Caption:=rsDocumentatio;
  Label7.Caption:=rsTutorial;
  Label10.Caption:=rsQuit;
  BitBtn1.Hint:=rsExploreTheLu;
  BitBtn2.Hint:=rsSearchInTheA;
  BitBtn3.Hint:=rsBrowseTheAtl;
  BitBtn4.Hint:=rsTheAtlasInte;
  BitBtn5.Hint:=rsExitTheComma;
  BitBtn6.Hint:=rsReadTheQuick;
  BitBtn7.Hint:=rsReadTheFullD;
  MenuItem1.Caption:=rsAtLunDocumen;
  MenuItem2.Caption:=rsDatLunDocume;
  MenuItem3.Caption:=rsPhotLunDocum;
  MenuItem4.Caption:=rsWebLunDocume;
end;

procedure Tf_cclun.FormCreate(Sender: TObject);
begin
  moveform:=false;
  GetAppDir;
  SetLang;
end;

procedure Tf_cclun.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var p: TPoint;
begin
   p:=TControl(sender).ClientToScreen(point(x,y));
   mousex:=p.X;
   mousey:=p.Y;
   moveform:=true;
end;

procedure Tf_cclun.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var xx,yy: integer;
    p: TPoint;
begin
if moveform then begin
  p:=TControl(sender).ClientToScreen(point(x,y));
  xx:=p.X-mousex;
  yy:=p.Y-mousey;
  Left:=Left+xx;
  Top:=Top+yy;
  mousex:=p.X;
  mousey:=p.Y;
  application.ProcessMessages;
end;
end;

procedure Tf_cclun.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  moveform:=false;
end;

procedure Tf_cclun.MenuItem1Click(Sender: TObject);
begin
OpenDoc('Doc_AtLun');
end;

procedure Tf_cclun.MenuItem2Click(Sender: TObject);
begin
OpenDoc('Doc_DatLun');
end;

procedure Tf_cclun.MenuItem3Click(Sender: TObject);
begin
OpenDoc('Doc_PhotLun');

end;

procedure Tf_cclun.MenuItem4Click(Sender: TObject);
begin
OpenDoc('Doc_WebLun');
end;

procedure Tf_cclun.BitBtn1Click(Sender: TObject);
begin
  chdir(appdir);
  Execnowait(MapLun);
end;

procedure Tf_cclun.BitBtn2Click(Sender: TObject);
begin
  chdir(appdir);
  Execnowait(DatLun);
end;

procedure Tf_cclun.BitBtn3Click(Sender: TObject);
begin
  chdir(appdir);
  Execnowait(PhotLun);
end;

procedure Tf_cclun.BitBtn4Click(Sender: TObject);
begin
  chdir(appdir);
  Execnowait(WebLun);
end;

procedure Tf_cclun.BitBtn5Click(Sender: TObject);
begin
  Close;
end;

procedure Tf_cclun.OpenDoc(doc:string);
var
  fn: string;
begin
  fn := slash(HelpDir) + helpprefix +'_'+doc+'.pdf';
  if not FileExists(fn) then
  begin
    fn := slash(HelpDir) + helpprefix +'_'+doc+'.html';
    if not FileExists(fn) then
    begin
      fn := slash(HelpDir) + 'UK_'+doc+'.pdf';
      if not FileExists(fn) then
      begin
        fn := slash(HelpDir) + 'UK_'+doc+'.html';
      end;
    end;
  end;
  ExecuteFile(fn);
end;

procedure Tf_cclun.BitBtn6Click(Sender: TObject);
begin
OpenDoc('tutorial');
end;

procedure Tf_cclun.BitBtn7Click(Sender: TObject);
var
  P:Tpoint;
begin
P.X:=BitBtn7.Left+BitBtn7.Width;
P.Y:=BitBtn7.Top;
P:=PanelCenter.ClientToScreen(P);
PopupMenu1.PopUp(P.X,P.Y);
end;


end.

