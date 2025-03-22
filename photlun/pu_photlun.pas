unit pu_photlun;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef mswindows}
    Windows, ShlObj,
  {$endif}
  {$ifdef unix}
    unix,
  {$endif}
  u_translation,
  BGRABitmap, BGRABitmapTypes, LazUTF8, UniqueInstance, math,
  fu_img, pu_config, u_util, u_constant, LazFileUtils, IniFiles,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, Menus;

type

  TVignette = class(TPanel)
    img: TImage;
    lbl: TPanel;
  private
    FonClick: TNotifyEvent;
    procedure imgclick(sender: TObject);
  public
    imgpath: string;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property onClick: TNotifyEvent read FonClick write FonClick;
  end;

  { Tf_photlun }

  Tf_photlun = class(TForm)
    Biblio1: TMenuItem;
    Carte: TMenuItem;
    Database: TMenuItem;
    File1: TMenuItem;
    help1: TMenuItem;
    MainMenu1: TMainMenu;
    MenuClose: TMenuItem;
    MenuConfig: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    PageControl1: TPageControl;
    PanelVignette: TPanel;
    Panel3: TPanel;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    UniqueInstance1: TUniqueInstance;
    View1: TMenuItem;
    procedure CarteClick(Sender: TObject);
    procedure DatabaseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageClose(Sender: TObject);
    procedure ImageFormClose(Sender: TObject);
    procedure ImageDetach(Sender: TObject);
    procedure MenuCloseClick(Sender: TObject);
    procedure MenuConfigClick(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure ScrollBox1Resize(Sender: TObject);
    procedure UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
  private
    imgnum, maximgdir: integer;
    imgdir : array of array[0..3] of string;
    imglist: TStringList;
    SelectedObject,pofile: string;
    StartVMA,CanCloseVMA, StartDatlun, CanCloseDatlun: boolean;
    SortByName, autoflipx, autoflipy: Boolean;
    procedure SetLang;
    procedure GetAppDir;
    procedure ReadConfig;
    procedure SaveConfig;
    Procedure SetBiblioMenu;
    procedure BiblioClick(Sender: TObject);
    procedure AllBiblioClick(Sender: TObject);
    Procedure InitImagesDir;
    Procedure AddImagesDir(dir,nom,cpy:string);
    Function GetImgCpy(dir,nom:string):string;
    procedure ClearVignette;
    procedure CreateVignette(fn: string);
    procedure VignetteClick(Sender: TObject);
    Procedure ReadParam(first:boolean=true);
    procedure ListImgDir(dir: string; filter:string='*');
    procedure SelectObject(nom: string);
    procedure RefreshVignettes;
    function GetObjectFromPhoto(p:string): string;
    procedure OpenVMA(objname,otherparam:string);
    procedure OpenDatlun(objname,otherparam:string);
  public
    param : Tstringlist;

  end;

var
  f_photlun: Tf_photlun;

implementation

const
  VignetteWidth=300;
  VignetteHeight=220;

{$R *.lfm}

{ Tf_photlun }

procedure Tf_photlun.FormCreate(Sender: TObject);
var i: integer;
begin
  DefaultFormatSettings.DecimalSeparator := '.';
  DefaultFormatSettings.ThousandSeparator:=' ';
  GetAppDir;
  chdir(appdir);
  ReadConfig;
  language:=u_translation.translate(pofile,'en');
  SelectedObject:='';
  SortByName:=true;
  param:=Tstringlist.Create;
  param.clear;
  if paramcount>0 then begin
   for i:=1 to paramcount do begin
      param.Add(paramstr(i));
   end;
  end;
  StartVMA:=false;
  StartDatlun:=false;
  CanCloseVMA:=true;
  CanCloseDatlun:=true;
  imglist:=TStringList.Create;

  imgnum:=0;

end;

procedure Tf_photlun.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
SaveConfig;
if CanCloseVMA and StartVMA then OpenVMA('','-quit');
if CanCloseDatLun and StartDatLun then OpenDatLun('','-quit');
end;

procedure Tf_photlun.DatabaseClick(Sender: TObject);
begin
  OpenDatlun(SelectedObject,'');
end;

procedure Tf_photlun.CarteClick(Sender: TObject);
begin
  OpenVMA(SelectedObject,'');
end;

procedure Tf_photlun.FormDestroy(Sender: TObject);
begin
imglist.Free;
param.Free;
end;

procedure Tf_photlun.FormShow(Sender: TObject);
begin
  SetLang;
  PageControl1.ActivePageIndex:=0;
  ReadParam;
  SetBiblioMenu;
  if imglist.Count=0 then AllBiblioClick(Sender);
  Application.BringToFront;
end;

procedure Tf_photlun.SetLang;
begin
File1.Caption:=rsFile;
Biblio1.Caption:=rsLibrary;
MenuItem1.Caption:=rsSearch;
MenuClose.Caption:=rsQuit;
View1.Caption:=rsView;
Database.Caption:=rsDatabase;
Carte.Caption:=rsShowOnMap;
u_util.hp:=rshelp_prefix;
help1.Caption:=rsHelp;
MenuItem14.Caption:=rsHelp;
MenuItem15.Caption:=rsAbout;
MenuConfig.Caption:=rsLibrarySetti;
end;

procedure Tf_photlun.ReadConfig;
var inif: TMemIniFile;
    section,fn,fd,cpy: string;
    i,n: integer;
begin
inif:=TMeminifile.create(configfile);
try
with inif do begin
section:='default';
pofile:=ReadString(section,'lang_po_file','');
section:='photlun';
section:='images';
n:=ReadInteger(section,'NumDir',0);
if n=0 then
  InitImagesDir
else begin
  maximgdir:=0;
  setlength(imgdir,0);
  for i:=1 to n do begin
    fd:=noslash(ReadString(section,'dir'+inttostr(i),''));
    fn:=ReadString(section,'name'+inttostr(i),'');
    cpy:=GetImgCpy(fd,fn);
    AddImagesDir(slash(fd), fn, cpy);
  end;
  InitImagesDir
end;
end;
finally
inif.free;
end;
end;

procedure Tf_photlun.SaveConfig;
var inif: TMemIniFile;
    section: string;
    i: integer;
begin
inif:=TMeminifile.create(configfile);
try
with inif do begin
section:='images';
WriteInteger(section,'NumDir',maximgdir);
for i:=1 to maximgdir do begin
  WriteString(section,'name'+inttostr(i),imgdir[i-1,2]);
  WriteString(section,'dir'+inttostr(i),noslash(imgdir[i-1,0]));
end;
UpdateFile;
end;
finally
inif.free;
end;
end;

Procedure Tf_photlun.AddImagesDir(dir,nom,cpy:string);
var i:integer;
    ok:boolean;
begin
ok:=true;
for i:=0 to maximgdir-1 do if (imgdir[i,2]=nom)or(noslash(imgdir[i,0])=noslash(dir)) then ok:=false;
if ok then begin
  inc(maximgdir);
  setlength(imgdir,maximgdir);
  imgdir[maximgdir-1,0]:=noslash(dir);
  imgdir[maximgdir-1,1]:=cpy;
  imgdir[maximgdir-1,2]:=nom;
  imgdir[maximgdir-1,3]:='0';
end;
end;

Function Tf_photlun.GetImgCpy(dir,nom:string):string;
var f:textfile;
begin
result:='';
if uppercase(nom)='LOPAM' then result :='Digitial Lunar Orbiter Photographic Atlas of the Moon / Courtesy Jeff Gillis - Lunar and Planetary Institute (http://www.lpi.usra.edu/research/lunar_orbiter/) '
else if uppercase(nom)='APOLLO' then result :='Courtesy NASA / http://www.nasa.gov '
else if fileexists(slash(dir)+'copyright.txt') then begin
  filemode:=0;
  assignfile(f,slash(dir)+'copyright.txt');
  reset(f);
  readln(f,result);
  closefile(f);
end;
end;

Procedure Tf_photlun.InitImagesDir;
begin
if maximgdir=0 then begin
   maximgdir:=3;
   setlength(imgdir,maximgdir);
   imgdir[0,0]:=slash(appdir)+'Lopam';
   imgdir[0,1]:=GetImgCpy(imgdir[0,0],imgdir[0,2]);
   imgdir[0,2]:='LOPAM';
   imgdir[0,3]:='1';
   imgdir[1,0]:=slash(appdir)+'Apollo';
   imgdir[1,1]:=GetImgCpy(imgdir[1,0],imgdir[1,2]);
   imgdir[1,2]:='Apollo';
   imgdir[1,3]:='0';
   imgdir[2,0]:=slash(appdir)+'My Images';
   imgdir[2,1]:=GetImgCpy(imgdir[2,0],imgdir[2,2]);
   imgdir[2,2]:='My Images';
   imgdir[2,3]:='1';
end;
AddImagesDir(slash(appdir)+'Clementine','Clementine','');
AddImagesDir(slash(appdir)+'Probes','Probes','');
AddImagesDir(slash(appdir)+'LunaStars','LunaStars','');
AddImagesDir(slash(appdir)+'CLA','CLA','Consolidated Lunar Atlas Copyright 2003 Lunar and Planetary Institute / Universities Space Research Association');
AddImagesDir(slash(appdir)+'LAC_LM','LAC_LM','Lunar Chart / Lunar Map. The Defense Mapping Agency 1973, Lunar and Planetary Institute 2005');
AddImagesDir(slash(appdir)+'ApolloMapping','Apollo Mapping Camera','Courtesy NASA / http://www.nasa.gov');
AddImagesDir(slash(appdir)+'LunarPits','Lunar Pits','Wagner, Robinson, and the LROC Team');
AddImagesDir(slash(appdir)+'Smart-1','Smart-1','European Space Agency (ESA)');
AddImagesDir(slash(appdir)+'BestOfAmateurs','Best of Amateurs','');
AddImagesDir(slash(appdir)+'BestOfHiggins','Best of Higgins','');
AddImagesDir(slash(appdir)+'BestOfLazzarotti','Best of Lazzarotti','');
AddImagesDir(slash(appdir)+'Kaguya','Kaguya','provided by JAXA/SELENE');
AddImagesDir(slash(appdir)+'Best_Pic_du_Midi','Best of Pic du Midi Observatory','JL Dauvergne, P. Tosi, E. Rousset, F. Colas, IMCCE, S2P , OMP');
AddImagesDir(slash(appdir)+'Best_of_Peach','Best of Damian Peach','Damian Peach');
AddImagesDir(slash(appdir)+'BestOfCathala','Best of Cathala','Luc Cathala');
AddImagesDir(slash(appdir)+'BestOfBrahic','Best of Jean Pierre Brahic','Jean Pierre Brahic');
AddImagesDir(slash(appdir)+'BestOfViladrich','Best of Christian Viladrich','Christian Viladrich');
end;

Procedure Tf_photlun.ReadParam(first:boolean=true);
var i : integer;
begin
i:=0;
while i <= param.count-1 do begin
  if (param[i]='-nx')and first then begin       // when started by vma do not close vma on exit!
     CanCloseVMA:=false;
  end
  else if (param[i]='-nd')and first then begin  // when started by datlun do not close datlun on exit!
     CanCloseDatlun:=false;
  end
  else if param[i]='-n' then begin
     inc(i);
     if i <= param.count-1 then
        SelectObject(param[i]);
  end
  else if param[i]='-fx' then begin
     inc(i);
     if i <= param.count-1 then
        autoflipx:=(param[i]='1');
  end
  else if param[i]='-fy' then begin
     inc(i);
     if i <= param.count-1 then
        autoflipy:=(param[i]='1');
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

Procedure Tf_photlun.SetBiblioMenu;
var i : integer;
    m:TMenuItem;
begin
  biblio1.Clear;
  for i:=0 to maximgdir-1 do begin
    if DirectoryExists(imgdir[i,0]) then begin
      m:=TMenuItem.Create(self);
      m.Caption:=imgdir[i,2];
      m.Tag:=i;
      m.OnClick:=@BiblioClick;
      Biblio1.Add(m);
    end;
  end;
  m:=TMenuItem.Create(self);
  m.Caption:=rsAll;
  m.OnClick:=@AllBiblioClick;
  Biblio1.Add(m);
end;

function ComparePhotoName(List: TStringList; Index1, Index: Integer): Integer;
begin
  Result := CompareText(ExtractFileName(List[Index1]),ExtractFileName(List[Index]));
end;

procedure Tf_photlun.BiblioClick(Sender: TObject);
var i: integer;
begin
  i:=TMenuItem(Sender).tag;
  imglist.Clear;
  ListImgDir(imgdir[i,0]);
  StatusBar1.Panels[0].Text:=imgdir[i,2];
  if SortByName then imglist.CustomSort(@ComparePhotoName)
     else imglist.Sort;
  RefreshVignettes;
end;

procedure Tf_photlun.AllBiblioClick(Sender: TObject);
var i: integer;
begin
  imglist.Clear;
  for i:=0 to maximgdir-1 do begin
     ListImgDir(imgdir[i,0]);
  end;
  StatusBar1.Panels[0].Text:=rsAllLibraries;
  if SortByName then imglist.CustomSort(@ComparePhotoName)
     else imglist.Sort;
  RefreshVignettes;
end;

procedure Tf_photlun.ClearVignette;
var i: integer;
begin
  for i:=PanelVignette.ControlCount-1 downto 0 do begin
    if PanelVignette.Controls[i] is TVignette then
      TVignette(PanelVignette.Controls[i]).Free;
  end;
end;

procedure Tf_photlun.CreateVignette(fn: string);
var v: TVignette;
    bmp: TBGRABitmap;
    r1,r2: double;
    w,h: integer;
begin
  v:=TVignette.Create(self);
  v.onClick:=@VignetteClick;
  v.Parent:=PanelVignette;
  v.lbl.Caption:=ExtractFileNameOnly(fn);
  v.imgpath:=fn;
  v.img.Stretch:=false;
  bmp:=TBGRABitmap.Create(v.imgpath);
  r1:=bmp.Width/bmp.Height;
  w:=v.Width;
  h:=v.height;
  r2:=w/h;
  if r1>r2 then
    h:=max(1,trunc(w/r1))
  else
    w:=max(1,trunc(h*r1));
  BGRAReplace(bmp, bmp.Resample(w,h,rmSimpleStretch));
  v.img.Picture.Assign(bmp);
  bmp.Free;
end;

procedure Tf_photlun.MenuItem1Click(Sender: TObject);
var n: String;
begin
if InputQuery(rsSearch, rsFormationNam, n) then
   SelectObject(n);
end;

procedure Tf_photlun.SelectObject(nom: string);
var i,r: integer;
    nom2,buf: string;
begin
SelectedObject:=nom;
imglist.Clear;
nom:=trim(nom);
nom2:='';
r:=length(nom)-1;
if copy(nom,r,1)=' ' then begin
  buf:=copy(nom,r+1,1);
  if ((buf>='a')and(buf<='z'))or((buf>='A')and(buf<='Z')) then nom2:=copy(nom,1,r-1);
end;
for i:=0 to maximgdir-1 do begin
  ListImgDir(imgdir[i,0],nom);
end;
if (imglist.Count=0)and(nom2>'') then begin
  for i:=0 to maximgdir-1 do begin
     ListImgDir(imgdir[i,0],nom2);
  end;
end;
  StatusBar1.Panels[0].Text:=rsSelection+' : '+nom;
  imglist.Sort;
  RefreshVignettes;
end;

procedure Tf_photlun.RefreshVignettes;
var i,n: integer;
begin
  ClearVignette;
  if imglist.count=0 then exit;
  n:=min(100,imglist.Count-1);
  SelectedObject:=GetObjectFromPhoto(imglist[0]);
  for i:=0 to n do begin
    CreateVignette(imglist[i]);
  end;
end;

procedure Tf_photlun.ListImgDir(dir: string; filter:string='*');
var f: TSearchRec;
    r: integer;
begin
try
  dir:=slash(dir);
  filter:=uppercase(filter);
  r:=FindFirst(dir+'*.*',0,f);
  while (r=0) do begin
    if (uppercase(ExtractFileExt(f.Name))='.JPG')
       and (  ( filter='*' )
           or (pos(filter,uppercase(f.Name))=1) )
       then imglist.Add(dir+f.Name);
    r:=findnext(f);
  end;
finally
  FindClose(f);
end;
end;

function Tf_photlun.GetObjectFromPhoto(p:string): string;
var i: integer;
begin
p:=ExtractFileName(p);
i:=pos('_',p);
if i>0 then delete(p,i,999);
i:=pos('.',p);
if i>0 then delete(p,i,999);
result:=p;
end;

procedure Tf_photlun.ScrollBox1Resize(Sender: TObject);
begin
  PanelVignette.ChildSizing.ControlsPerLine:=ScrollBox1.Width div VignetteWidth;
end;

procedure Tf_photlun.UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
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

procedure Tf_photlun.VignetteClick(Sender: TObject);
var img: Tf_img;
begin
  inc(imgnum);
  img:=Tf_img.Create(self);
  img.Name:='f_img'+inttostr(imgnum);
  img.Parent:=panel3;
  img.Align:=alClient;
  img.image:=TVignette(Sender).imgpath;
  img.onClose:=@ImageClose;
  img.onDetach:=@ImageDetach;
  PageControl1.ActivePageIndex:=1;
end;

procedure Tf_photlun.ImageDetach(Sender: TObject);
var f: TForm;
begin
  f:=TForm.Create(self);
  f.Width:=800;
  f.Height:=600;
  Tf_img(Sender).Parent:=f;
  Tf_img(Sender).onClose:=@ImageFormClose;
  Tf_img(Sender).BtnDetach.Visible:=False;
  Tf_img(Sender).PanelTop.Visible:=False;
  FormPos(f,Mouse.CursorPos.X,Mouse.CursorPos.Y);
  f.ShowOnTop;
  PageControl1.ActivePageIndex:=0;
end;

procedure Tf_photlun.MenuCloseClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_photlun.MenuConfigClick(Sender: TObject);
var i: integer;
    cpy: string;
begin
f_config.StringGrid1.RowCount:=maximgdir+10;
for i:=1 to maximgdir do begin
  f_config.StringGrid1.Cells[0,i]:=imgdir[i-1,2];
  f_config.StringGrid1.Cells[1,i]:=noslash(imgdir[i-1,0]);
end;
FormPos(f_config,mouse.cursorpos.x,mouse.cursorpos.y);
f_config.showmodal;
if f_config.ModalResult=mrOK then begin
   maximgdir:=0;
   setlength(imgdir,maximgdir);
   for i:=1 to f_config.StringGrid1.RowCount-1 do
     if trim(f_config.StringGrid1.Cells[0,i])>'' then begin
       cpy:=GetImgCpy(f_config.StringGrid1.Cells[1,i], f_config.StringGrid1.Cells[0,i]);
       AddImagesDir(noslash(f_config.StringGrid1.Cells[1,i]), f_config.StringGrid1.Cells[0,i], cpy);
   end;
   SetBiblioMenu;
end;
end;

procedure Tf_photlun.MenuItem14Click(Sender: TObject);
begin
  ShowHelpDoc('Doc','PhotLun','doc');
end;

procedure Tf_photlun.MenuItem15Click(Sender: TObject);
begin
  Showmessage('Photlun '+Splashversion+crlf+
              compile_version+crlf+
              avlcpy+crlf+crlf+
              'Conception : Christian Legrand & Patrick Chevalley'+crlf+
              'Programming : Patrick Chevalley'+crlf+crlf+
              'This program is free software; you can redistribute it and/or '+crlf+
              'modify it under the terms of the GNU General Public License '+crlf+
              'as published by the Free Software Foundation.');
end;

procedure Tf_photlun.ImageFormClose(Sender: TObject);
begin
  if Tf_img(Sender).Parent is TForm then
    TForm(Tf_img(Sender).Parent).close;
end;

procedure Tf_photlun.ImageClose(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
end;

procedure Tf_photlun.GetAppDir;
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
end;

procedure Tf_photlun.OpenVMA(objname,otherparam:string);
var parm:string;
begin
    parm:='-np ';
    if CanCloseVMA and (not StartVMA) then begin
      parm:=parm+' -3d ';
    end;
    if objname<>'' then parm:=parm+' -n "'+objname+'" ';
    parm:=parm+otherparam;
    chdir(appdir);
    Execnowait(maplun+' '+parm);
    StartVMA:=true;
end;

procedure Tf_photlun.OpenDatlun(objname,otherparam:string);
var parm:string;
begin
    parm:='-np ';
    if objname<>'' then parm:=parm+' -n "'+objname+'" ';
    parm:=parm+otherparam;
    chdir(appdir);
    Execnowait(datlun+' '+parm);
    StartDatlun:=true;
end;

//============ TVignette =========================

constructor TVignette.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  AutoSize:=true;
  lbl:=TPanel.Create(self);
  img:=TImage.Create(self);
  img.Picture.Bitmap.Width:=VignetteWidth;
  img.Picture.Bitmap.Height:=VignetteHeight;
  img.Width:=VignetteWidth;
  img.Height:=VignetteHeight;
  img.top:=0;
  img.left:=0;
  img.OnClick:=@imgclick;
  lbl.height:=30;
  lbl.width:=VignetteWidth;
  lbl.top:=224;
  lbl.left:=0;
  lbl.Parent:=self;
  img.Parent:=self;
  lbl.Caption:=' ';
end;

destructor TVignette.Destroy;
begin
  inherited Destroy;
end;

procedure TVignette.imgclick(sender: TObject);
begin
  if assigned(FonClick) then FonClick(self);
end;

end.

