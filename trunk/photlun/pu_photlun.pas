unit pu_photlun;
{
Copyright (C) 2007 Patrick Chevalley

http://www.astrosurf.com/astropc
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
{
  Main PhotLun window (vignettes)
}

{$mode objfpc}{$H+}

interface

uses
{$ifdef mswindows}
  Windows,
{$endif}
{$ifdef unix}
    unix,
{$endif}
  u_translation,
  pu_photo, pu_config, u_bitmap, u_util,
  FPReadJPEG, Math, Inifiles,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, ComCtrls, Spin, UniqueInstance;

const maxvignette = 25;
      maxphotowindow=9;
      vignwidth = 160;
      vignheight = 120;

type

  Tvignette_info = record
                   autorot: boolean;
                   imgfile,vignettefile: string;
                   end;

  { Tf_photlun }

  Tf_photlun = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Biblio1: TMenuItem;
    MenuItem1: TMenuItem;
    MenuClose: TMenuItem;
    Carte: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuConfig: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    Database: TMenuItem;
    MenuItem9: TMenuItem;
    PanelVignette: TPanel;
    PanelTop: TPanel;
    ScrollBar1: TScrollBar;
    StatusBar1: TStatusBar;
    SetSizeTimer1: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure FormResize(Sender: TObject);
    procedure MenuConfigClick(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure BtnLeftClick(Sender: TObject);
    procedure BtnRightClick(Sender: TObject);
    procedure CarteClick(Sender: TObject);
    procedure DatabaseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuCloseClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure PanelVignetteResize(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure SetSizeTimer1Timer(Sender: TObject);
    procedure UniqueInstance1OtherInstance(Sender: TObject;
      ParamCount: Integer; Parameters: array of String);
    procedure VignetteClick(Sender: TObject);
  private
    { private declarations }
    param : Tstringlist;
    privatedir,vignettedir, SelectedObject,ConfigFile, pofile : string;
    maxphoto,curphoto,photow,photoh,maxheight: integer;
    vignettenum, currentvignette, vignetteleft, vignetteright,maximgdir: integer;
    VignetteHeight, Nhphoto: integer;
    imglist: TStringList;
    SortByName, autoflipx, autoflipy: Boolean;
    vignette : array [0..maxvignette] of TImage;
    vignette_info : array [0..maxvignette] of Tvignette_info;
    imglabel : array [0..maxvignette] of TLabel;
    photo : array [0..maxphotowindow] of TF_photo;
    imgdir : array of array[0..3] of string;
    FileAgeLimit : Longint;
    vmaexe, datlunexe: string;
    StartVMA,CanCloseVMA, StartDatlun, CanCloseDatlun, lockresize: boolean;
    procedure SetLang;
    Procedure ReadParam;
    procedure ReadConfig;
    procedure SaveConfig;
    procedure PhotoSaveParam(Sender: TObject);
    Procedure SetBiblioMenu;
    procedure BiblioClick(Sender: TObject);
    procedure AllBiblioClick(Sender: TObject);
    Procedure AddImagesDir(dir,nom,cpy,autorot:string);
    Function GetImgCpy(dir,nom:string):string;
    Procedure InitImagesDir;
    function GetAutorot(fn:string):string;
    procedure ListImgDir(dir: string; filter:string='*');
    function GetObjectFromPhoto(p:string): string;
    procedure ClearVignettes;
    procedure RefreshVignettes(first:integer);
    procedure ReloadVignettes;
    procedure CreateVignette(orig,vign: string);
    procedure doCreateVignette(bmp:TBitmap; vign: string);
    procedure loadvignette(num:integer; fn:string);
    Procedure SetVignetteSize(h:integer);
    procedure SetPath;
    procedure SelectObject(nom: string);
    procedure OpenVMA(objname,otherparam:string);
    procedure OpenDatlun(objname,otherparam:string);
  public
    { public declarations }
  end; 

var
  f_photlun: Tf_photlun;

implementation

const
  ExitProMsg='Virtual_Moon_Atlas_Pro_exit';

{$ifdef windows}
   {$R photlun.res}
{$endif}

{ Tf_photlun }

Function Slash(nom : string) : string;
begin
result:=trim(nom);
if copy(result,length(nom),1)<>PathDelim then result:=result+PathDelim;
end;

Function NoSlash(nom : string) : string;
begin
result:=trim(nom);
if copy(result,length(nom),1)=PathDelim then result:=copy(result,1,length(nom)-1);
end;

Procedure FormPos(form : Tform; x,y : integer);
const bot=40; //minimal distance from screen bottom
begin
with Form do begin
  left:=x;
  if left+width>Screen.Width then left:=Screen.Width-width;
  if left<0 then left:=0;
  top:=y;
  if top+height>(Screen.height-bot) then top:=Screen.height-height-bot;
  if top<0 then top:=0;
end;
end;

Function Exec(cmd: string; hide: boolean=true): integer;
{$ifdef unix}
begin
 result:=fpSystem(cmd);
end;
{$endif}
{$ifdef mswindows}
var
   bchExec: array[0..1024] of char;
   pchEXEC: Pchar;
   si: TStartupInfo;
   pi: TProcessInformation;
   res:cardinal;
begin
   pchExec := @bchExec;
   StrPCopy(pchExec,cmd);
   FillChar(si,sizeof(si),0);
   FillChar(pi,sizeof(pi),0);
   si.dwFlags:=STARTF_USESHOWWINDOW;
   if hide then si.wShowWindow:=SW_SHOWMINIMIZED
           else si.wShowWindow:=SW_SHOWNORMAL;
   si.cb := sizeof(si);
   try
      if CreateProcess(Nil,pchExec,Nil,Nil,false,CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                       Nil,Nil,si,pi) = True then
         begin
           WaitForSingleObject(pi.hProcess,INFINITE);
           GetExitCodeProcess(pi.hProcess,Res);
           result:=res;
         end
         else
           Result := GetLastError;
    except;
       Result := GetLastError;
    end;
end;
{$endif}

procedure Tf_photlun.CreateVignette(orig,vign: string);
var vbmp1:Tbitmap;
    jpeg:TJPEGImage;
begin
try
 jpeg:=TJPEGImage.Create;
 vbmp1:=TBitmap.Create;
 jpeg.Performance:=jpBestSpeed;
 try
   if FileAge(orig)<FileAgeLimit then raise exception.Create('too old file');
   jpeg.LoadFromFile(orig);
 {$ifdef mswindows}
   if odd(jpeg.Width) then raise exception.Create('invalid odd size');
   if odd(jpeg.Height) then raise exception.Create('invalid odd size');
 {$endif}
 except
 {$ifdef unix}
   exec('./fiximg.sh "'+orig+'"');
 {$endif}
 {$ifdef mswindows}
   exec('fiximg "'+orig+'"');
 {$endif}
   jpeg.LoadFromFile(orig);
 end;
 vbmp1.Assign(jpeg);
 doCreateVignette(vbmp1,vign);
finally
 jpeg.Free;
 vbmp1.Free;
end;
end;

procedure Tf_photlun.doCreateVignette(bmp:TBitmap; vign: string);
var vh,vw: integer;
    rs,rv,zoom: double;
    vbmp1,vbmp2:Tbitmap;
    jpeg:TJPEGImage;
begin
jpeg:=TJPEGImage.Create;
vbmp2:=TBitmap.Create;
try
 rs:=bmp.Width/bmp.Height;
 rv:=vignwidth/vignheight;
 if rs>rv then begin
    zoom:=vignwidth/bmp.Width;
 end else begin
    zoom:=vignheight/bmp.Height;
 end;
 BitmapResize(bmp,vbmp2,zoom);
 jpeg.Assign(vbmp2);
 jpeg.CompressionQuality:=50;
 jpeg.SaveToFile(vign);
finally
 jpeg.Free;
 vbmp2.Free;
end;
end;

procedure Tf_photlun.loadvignette(num:integer; fn:string);
var
  jpeg:TJPEGImage;
  fp,vfp,vfn,libr,nom,autor: string;
  i: integer;
begin
  fp:=noslash(ExtractFilePath(fn));
  vfp:=vignettedir+ExtractFileName(fp);
  if not DirectoryExists(vfp) then ForceDirectories(vfp);
  vfn:=slash(vfp)+ExtractFileName(fn);
  jpeg:=TJPEGImage.Create;
  if (not FileExists(vfn)) or (FileAge(vfn)<FileAge(fn)) then CreateVignette(fn,vfn);
  jpeg.LoadFromFile(vfn);
  vignette[num].Picture.Bitmap.Assign(jpeg);
  libr:=noslash(ExtractFilePath(fn));
  for i:=0 to maximgdir-1 do begin
     if noslash(imgdir[i,0])=libr then begin
        libr:=imgdir[i,2];
        autor:=imgdir[i,3];
        break;
     end;
  end;
  nom:=ChangeFileExt(ExtractFileName(fn),'');
  vignette[num].Hint:=libr+'  '+nom;
  vignette_info[num].autorot:=(autor='1');
  vignette_info[num].imgfile:=fn;
  vignette_info[num].vignettefile:=vfn;
  if PanelVignette.Height>100 then begin
     imglabel[num].Caption:=nom;
     imglabel[num].Hint:=vignette[num].Hint;
  end;
  jpeg.Free;
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

procedure Tf_photlun.RefreshVignettes(first:integer);
var i,n: integer;
begin
  if imglist.count=0 then exit;
  if first>(imglist.count-vignettenum) then first:=(imglist.count-vignettenum);
  if first<0 then first:=0;
  if first<>vignetteleft then begin
    vignetteleft:=first;
    n:=min(vignettenum-1,imglist.Count-1);
    SelectedObject:=GetObjectFromPhoto(imglist[vignetteleft]);
    for i:=0 to n do begin
      vignetteright:=first+i;
      loadvignette(i,imglist[vignetteright]);
    end;
    ScrollBar1.Position:=vignetteleft;
  end;
end;

procedure Tf_photlun.ReloadVignettes;
var i:integer;
begin
i:=vignetteleft;
vignetteleft:=-1;
RefreshVignettes(i);
end;

procedure Tf_photlun.ClearVignettes;
var i: integer;
begin
  for i:=0 to vignettenum do begin
    vignette[i].Picture.Clear;
    vignette[i].Hint:='';
    imglabel[i].Caption:='';
    imglabel[i].Hint:='';
  end;
end;

Procedure Tf_photlun.AddImagesDir(dir,nom,cpy,autorot:string);
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
  imgdir[maximgdir-1,3]:=autorot;
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

function Tf_photlun.GetAutorot(fn:string):string;
begin
result:='0';
if (fn='LOPAM') or
   (fn='My Images') or
   (fn='Clementine') or
   (fn='LunaStars') or
   (fn='CLA') or
   (fn='LAC_LM')
   then result:='1';
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
AddImagesDir(slash(appdir)+'Clementine','Clementine','','1');
AddImagesDir(slash(appdir)+'Probes','Probes','','0');
AddImagesDir(slash(appdir)+'LunaStars','LunaStars','','1');
AddImagesDir(slash(appdir)+'CLA','CLA','Consolidated Lunar Atlas Copyright 2003 Lunar and Planetary Institute / Universities Space Research Association','1');
AddImagesDir(slash(appdir)+'LAC_LM','LAC_LM','Lunar Chart / Lunar Map. The Defense Mapping Agency 1973, Lunar and Planetary Institute 2005','1');
AddImagesDir(slash(appdir)+'ApolloMapping','Apollo Mapping Camera','Courtesy NASA / http://www.nasa.gov','0');
AddImagesDir(slash(appdir)+'BestOfAmateurs','Best of Amateurs','','0');
AddImagesDir(slash(appdir)+'BestOfHiggins','Best of Higgins','','0');
AddImagesDir(slash(appdir)+'BestOfLazzarotti','Best of Lazzarotti','','0');

end;

procedure Tf_photlun.MenuConfigClick(Sender: TObject);
var i,j: integer;
    cpy,autorot: string;
begin
f_config.StringGrid1.RowCount:=maximgdir+10;
for i:=1 to maximgdir do begin
  f_config.StringGrid1.Cells[0,i]:=imgdir[i-1,3];
  f_config.StringGrid1.Cells[1,i]:=imgdir[i-1,2];
  f_config.StringGrid1.Cells[2,i]:=noslash(imgdir[i-1,0]);
end;
FormPos(f_config,mouse.cursorpos.x,mouse.cursorpos.y);
f_config.showmodal;
if f_config.ModalResult=mrOK then begin
   maximgdir:=0;
   setlength(imgdir,maximgdir);
   for i:=1 to f_config.StringGrid1.RowCount-1 do
     if trim(f_config.StringGrid1.Cells[1,i])>'' then begin
       cpy:=GetImgCpy(f_config.StringGrid1.Cells[2,i], f_config.StringGrid1.Cells[1,i]);
       autorot:=f_config.StringGrid1.Cells[0,i];
       if autorot<>'1' then autorot:='0';
       AddImagesDir(noslash(f_config.StringGrid1.Cells[2,i]), f_config.StringGrid1.Cells[1,i], cpy, autorot);
       inc(j);
   end;
   SetBiblioMenu;
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
  ScrollBar1.min:=0;
  ScrollBar1.max:=max(0,imglist.count-vignettenum);
  ClearVignettes;
  vignetteleft:=-1;
  RefreshVignettes(0);
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
  ScrollBar1.min:=0;
  ScrollBar1.max:=max(0,imglist.count-vignettenum);
  vignetteleft:=-1;
  RefreshVignettes(0);
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
  if SortByName then imglist.CustomSort(@ComparePhotoName)
     else imglist.Sort;
  ScrollBar1.min:=0;
  ScrollBar1.max:=max(0,imglist.count-vignettenum);
  ClearVignettes;
  vignetteleft:=-1;
  RefreshVignettes(0);
end;

Procedure Tf_photlun.SetVignetteSize(h:integer);
var mh,nh,mw: TConstraintSize;
begin
VignetteHeight:=h;
SetSizeTimer1.Enabled:=true;
end;

procedure Tf_photlun.SetSizeTimer1Timer(Sender: TObject);
begin
SetSizeTimer1.Enabled:=false;
lockresize:=true;
 PanelVignette.Height:=VignetteHeight;
 ClientHeight:=VignetteHeight+ScrollBar1.Height+4+StatusBar1.Height;
 if VignetteHeight>100 then
     ClientHeight:=ClientHeight+StatusBar1.Height
 else
     ClientHeight:=ClientHeight+8;
 maxheight:=Height;
lockresize:=false;
end;

procedure Tf_photlun.FormResize(Sender: TObject);
begin
if not lockresize then begin
   if Height<>maxheight then Height:=maxheight;
   if Width<photow then Width:=photow;
end;
Nhphoto:=max(1,width div photow);
end;

procedure Tf_photlun.MenuCloseClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_photlun.MenuItem4Click(Sender: TObject);
begin
  SetVignetteSize(60);
end;

procedure Tf_photlun.MenuItem5Click(Sender: TObject);
begin
  SetVignetteSize(120);
end;

procedure Tf_photlun.MenuItem6Click(Sender: TObject);
begin
  SetVignetteSize(200);
end;

procedure Tf_photlun.MenuItem8Click(Sender: TObject);
var f: TForm;
    sp: TSpinEdit;
    la: TLabel;
    bt: TButton;
begin
 f:=TForm.Create(self) ;
 f.BorderStyle:=bsToolWindow;
 f.AutoSize:=true;
 sp:=TSpinEdit.Create(self);
 la:=TLabel.Create(self);
 bt:=TButton.Create(self);
 la.Parent:=f;
 la.Caption:=rsNumberOfWind;
 la.Width:=200;
// la.AutoSize:=true;
 la.Top:=8;
 la.Left:=8;
 sp.Parent:=f;
 sp.MaxValue:=10;
 sp.MinValue:=1;
 sp.Value:=maxphoto;
 sp.Top:=8;
 sp.Left:=la.Left+la.Width+8;
 bt.Parent:=f;
 bt.Caption:=rsOK;
 bt.ModalResult:=mrOK;
 bt.Left:=sp.Left;
 bt.Top:=sp.Top+sp.Height+8;
 formpos(f,mouse.CursorPos.x,mouse.CursorPos.y);
 f.ShowModal;
 if f.ModalResult=mrOK then maxphoto:=sp.Value;
 sp.Free;
 la.Free;
 bt.Free;
 f.Free;
end;

procedure Tf_photlun.SetPath;
{$ifdef mswindows}
var PIDL : PItemIDList;
    Folder : array[0..MAX_PATH] of Char;
const CSIDL_APPDATA = $001A;
{$endif}
begin
  appdir:=getcurrentdir;
{$ifdef mswindows}
  SHGetSpecialFolderLocation(0, CSIDL_APPDATA, PIDL);
  SHGetPathFromIDList(PIDL, Folder);
  privatedir:=slash(Folder)+'virtualmoon';
{$endif}
{$ifdef unix}
  privatedir:=slash(ExpandFileName('~/.virtualmoon'));
{$endif}
  if not DirectoryExists(privatedir) then ForceDirectories(privatedir);
  vignettedir:=slash(privatedir)+slash('vignette');
  if not DirectoryExists(vignettedir) then ForceDirectories(vignettedir);
  ConfigFile:=slash(privatedir)+'virtualmoon.ini';
end;

procedure Tf_photlun.FormCreate(Sender: TObject);
var i: integer;
begin
  SetPath;
  ReadConfig;
  language:=u_translation.translate(pofile,'en');
  vmaexe:='vmapro.exe';
  datlunexe:='datlun.exe';
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
  FileAgeLimit:=DateTimeToFileDate(EncodeDate(2007,1,1));
  imglist:=TStringList.Create;
  vignettenum:=0;
  vignetteleft:=-1;
  for i:=0 to maxvignette do begin
    vignette[i]:=TImage.Create(self);
    vignette[i].Parent:=PanelVignette;
    vignette[i].Visible:=false;
    vignette[i].ShowHint:=true;
    vignette[i].Tag:=i;
    vignette[i].OnClick:=@Vignetteclick;
    imglabel[i]:=TLabel.Create(self);
    imglabel[i].Parent:=PanelTop;
    imglabel[i].Transparent:=false;
    imglabel[i].ParentColor:=true;
    imglabel[i].Visible:=false;
    imglabel[i].ShowHint:=true;
  end;
  for i:=0 to maxphotowindow do begin
     photo[i]:=TF_photo.Create(self);
     photo[i].onSaveParam:=@PhotoSaveParam;
  end;
  maxphoto:=2;
  curphoto:=0;
  photow:=photo[0].Width;
  photoh:=photo[0].Height;
  lockresize:=true;
  maxheight:=height;
  Nhphoto:=2;
  autoflipx:=false;
  autoflipy:=false;
end;

procedure Tf_photlun.FormShow(Sender: TObject);
begin
  SetLang;
  ReadParam;
  SetBiblioMenu;
  if imglist.Count=0 then AllBiblioClick(Sender);
end;

procedure Tf_photlun.FormDestroy(Sender: TObject);
var i : integer;
begin
  imglist.Free;
  param.Free;
  for i:=0 to maxvignette do begin
     vignette[i].Free;
     imglabel[i].Free;
  end;
  for i:=0 to maxphotowindow do photo[i].Free;
end;

Procedure Tf_photlun.ReadParam;
var i : integer;
begin
i:=0;
while i <= param.count-1 do begin
  if param[i]='-nx' then begin       // when started by vma do not close vma on exit!
     CanCloseVMA:=false;
  end
  else if param[i]='-nd' then begin  // when started by datlun do not close datlun on exit!
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

procedure Tf_photlun.PanelVignetteResize(Sender: TObject);
var vw,vh,newcount: integer;
    i,p : integer;
begin
vh:=PanelVignette.Height;
vw:=round(vh*4/3);
newcount:=PanelVignette.Width div vw;
if newcount>maxvignette then newcount:=maxvignette;
if newcount<>vignettenum then begin
  vignettenum:=newcount;
  p:=0;
  for i:=0 to vignettenum-1 do begin
     vignette[i].Visible:=true;
     vignette[i].Top:=0;
     vignette[i].Left:=p;
     vignette[i].Height:=vh;
     vignette[i].Width:=vw;
     vignette[i].Stretch:=true;
     vignette[i].Proportional:=true;
     vignette[i].transparent:=true;
     vignette[i].AutoSize:=false;
     imglabel[i].Visible:=true;
     imglabel[i].Top:=vignette[i].Top+vignette[i].Height+4;
     imglabel[i].Left:=vignette[i].Left+2;
     imglabel[i].Caption:='';
     p:=p+vw+1;
  end;
  for i:=vignettenum to maxvignette do vignette[i].Visible:=false;
  ScrollBar1.max:=max(0,imglist.count-vignettenum);
  ScrollBar1.PageSize:=vignettenum;
  ScrollBar1.LargeChange:=vignettenum;
  ReloadVignettes;
end;
end;

procedure Tf_photlun.ScrollBar1Change(Sender: TObject);
begin
  RefreshVignettes(ScrollBar1.Position);
end;

procedure Tf_photlun.UniqueInstance1OtherInstance(Sender: TObject;
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
     ReadParam;
  end;
end;

procedure Tf_photlun.BtnLeftClick(Sender: TObject);
begin
  RefreshVignettes(vignetteleft-vignettenum);
end;

procedure Tf_photlun.BtnRightClick(Sender: TObject);
begin
  RefreshVignettes(vignetteright+1);
end;

procedure Tf_photlun.CarteClick(Sender: TObject);
begin
  OpenVMA(SelectedObject,'');
end;

procedure Tf_photlun.DatabaseClick(Sender: TObject);
begin
  OpenDatlun(SelectedObject,'');
end;

procedure Tf_photlun.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig;
  {$ifdef mswindows}
  if CanCloseVMA and StartVMA then SendMessage(HWND_BROADCAST,RegisterWindowMessage(ExitProMsg),0,0);
  {$endif}
  if CanCloseDatLun and StartDatLun then OpenDatLun('','-quit');
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

procedure Tf_photlun.VignetteClick(Sender: TObject);
var n,num: integer;
    p: TPoint;
begin
n:=TImage(Sender).Tag;
num:=vignetteleft+n;
if (num>=0) and (num<=(imglist.Count-1)) then begin
  SelectedObject:=GetObjectFromPhoto(imglist[num]);
  photo[curphoto].autorotate:=vignette_info[n].autorot;
  photo[curphoto].autoflipx:=autoflipx;
  photo[curphoto].autoflipy:=autoflipy;
  photo[curphoto].param:=ChangeFileExt(vignette_info[n].vignettefile,'.param');
  photo[curphoto].image:=imglist[num];
  photo[curphoto].Caption:='PhotLun : '+TImage(Sender).Hint;
  if not photo[curphoto].Visible then begin
    p.x:=left+(photow+8)*(curphoto mod Nhphoto);
    p.y:=top+height+34+photoh*(curphoto div Nhphoto);
    ClientToScreen(p);
    formpos(photo[curphoto],p.x,p.y);
  end;
  photo[curphoto].Show;
  photo[curphoto].BringToFront;
  inc(curphoto);
  if curphoto>(maxphoto-1) then curphoto:=0;
end;
end;

procedure Tf_photlun.OpenVMA(objname,otherparam:string);
var parm:string;
begin
    parm:='';
    if CanCloseVMA and (not StartVMA) then begin
      parm:=parm+' -3d ';
    end;
    if objname<>'' then parm:=parm+' -n "'+objname+'" ';
    parm:=parm+otherparam;
    chdir(appdir);
    Execnowait(vmaexe+' '+parm);
    StartVMA:=true;
end;

procedure Tf_photlun.OpenDatlun(objname,otherparam:string);
var parm:string;
begin
    parm:='';
    if objname<>'' then parm:=parm+' -n "'+objname+'" ';
    parm:=parm+otherparam;
    chdir(appdir);
    Execnowait(datlunexe+' '+parm);
    StartDatlun:=true;
end;

procedure Tf_photlun.SetLang;
begin
File1.Caption:=rsFile;
Biblio1.Caption:=rsLibrary;
MenuItem1.Caption:=rsSearch;
MenuClose.Caption:=rsQuit;
MenuItem9.Caption:=rsCloseAllWind;
MenuItem2.Caption:=rsVignettes;
MenuItem3.Caption:=rsSize;
MenuItem4.Caption:=rsSmall;
MenuItem5.Caption:=rsMedium;
MenuItem6.Caption:=rsLarge;
MenuItem7.Caption:=rsImages;
MenuItem8.Caption:=rsNumberOfWind;
MenuItem10.Caption:=rsSort;
MenuItem11.Caption:=rsByName;
MenuItem12.Caption:=rsByLibrary;
Database.Caption:=rsDatabase;
Carte.Caption:=rsShowOnMap;
u_util.hp:=rsHelpPrefix;
MenuItem16.Caption:=rsImageMirror;
MenuItem20.Caption:=rsNone1;
MenuItem18.Caption:=rsEastWest;
MenuItem19.Caption:=rsNorthSouth;
MenuItem13.Caption:=rsHelp;
MenuItem14.Caption:=rsHelp;
MenuItem15.Caption:=rsAbout;
MenuConfig.Caption:=rsLibrarySetti;
end;

procedure Tf_photlun.MenuItem11Click(Sender: TObject);
begin
 SortByName:=MenuItem11.Checked;
 if (imglist<>nil)and(imglist.Count>0) then begin
  if SortByName then imglist.CustomSort(@ComparePhotoName)
     else imglist.Sort;
  ScrollBar1.min:=0;
  ScrollBar1.max:=max(0,imglist.count-vignettenum);
  ClearVignettes;
  vignetteleft:=-1;
  RefreshVignettes(0);
 end;
end;

procedure Tf_photlun.MenuItem14Click(Sender: TObject);
begin
ShowHelpDoc('Doc','_PHOTLUN','doc');
end;

procedure Tf_photlun.MenuItem15Click(Sender: TObject);
begin
  Showmessage('Photlun version 1.0'+crlf+
              'http://astrosurf.com/avl'+crlf+
              'Copyright (C) 2008 Christian Legrand, Patrick Chevalley'+crlf+crlf+
              'Conception : Christian Legrand'+crlf+
              'Programming : Patrick Chevalley'+crlf+
              rsTranslatedBy+crlf+crlf+
              'This program is free software; you can redistribute it and/or '+crlf+
              'modify it under the terms of the GNU General Public License '+crlf+
              'as published by the Free Software Foundation.'
);
end;

procedure Tf_photlun.MenuItem18Click(Sender: TObject);
begin
  MenuItem20.Checked:=false;
  MenuItem18.Checked:=true;
  autoflipx:=true;
end;

procedure Tf_photlun.MenuItem19Click(Sender: TObject);
begin
  MenuItem20.Checked:=false;
  MenuItem19.Checked:=true;
  autoflipy:=true;
end;

procedure Tf_photlun.MenuItem20Click(Sender: TObject);
begin
  autoflipx:=false;
  autoflipy:=false;
  MenuItem18.Checked:=false;
  MenuItem19.Checked:=false;
  MenuItem20.Checked:=true;
end;

procedure Tf_photlun.MenuItem9Click(Sender: TObject);
var i: integer;
begin
  for i:=0 to maxphotowindow do photo[i].Close;
  curphoto:=0;
end;

procedure Tf_photlun.PhotoSaveParam(Sender: TObject);
var vign: string;
begin
vign:=ChangeFileExt(Tf_photo(sender).param,'.jpg');
doCreateVignette(Tf_photo(sender).imgbmp,vign);
ReloadVignettes;
end;

procedure Tf_photlun.ReadConfig;
var inif: TMemIniFile;
    section,fn,fd,fr,cpy,autorot: string;
    i,n: integer;
begin
{$ifdef mswindows}    // migrate old config in app directory
if not fileexists(ConfigFile) then
   CopyFile(pchar(slash(AppDir)+'virtualmoon.ini'),pchar(ConfigFile),true);
{$endif}
inif:=TMeminifile.create(configfile);
try
with inif do begin
section:='default';
pofile:=ReadString(section,'lang_po_file','');
section:='photlun';
maxphoto:=ReadInteger(section,'ImageWindow',2);
if ReadBool(section,'VignetteSmall',false) then MenuItem4.Click;
if ReadBool(section,'VignetteBig',false) then MenuItem6.Click;
if ReadBool(section,'VignetteMedium',true) then MenuItem5.Click;
if ReadBool(section,'SortByName',true) then MenuItem11.Click
   else MenuItem12.Click;
if ReadBool(section,'MirrorNone',true) then MenuItem20.Click;
if ReadBool(section,'MirrorEastWest',false) then MenuItem18.Click;
if ReadBool(section,'MirrorNorthSouth',false) then MenuItem19.Click;
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
    autorot:=ReadString(section,'autorot'+inttostr(i),'?');
    if autorot='?' then autorot:=getautorot(fn);
    cpy:=GetImgCpy(fd,fn);
    if autorot<>'1' then autorot:='0';
    AddImagesDir(slash(fd), fn, cpy, autorot);
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
section:='photlun';
WriteBool(section,'VignetteSmall',MenuItem4.Checked);
WriteBool(section,'VignetteBig',MenuItem6.Checked);
WriteBool(section,'VignetteMedium',MenuItem5.Checked);
WriteBool(section,'SortByName',MenuItem11.Checked);
WriteInteger(section,'ImageWindow',maxphoto);
WriteBool(section,'MirrorNone',MenuItem20.Checked);
WriteBool(section,'MirrorEastWest',MenuItem18.Checked);
WriteBool(section,'MirrorNorthSouth',MenuItem19.Checked);
section:='images';
WriteInteger(section,'NumDir',maximgdir);
for i:=1 to maximgdir do begin
  WriteString(section,'name'+inttostr(i),imgdir[i-1,2]);
  WriteString(section,'dir'+inttostr(i),noslash(imgdir[i-1,0]));
  WriteString(section,'autorot'+inttostr(i),imgdir[i-1,3]);
end;
UpdateFile;
end;
finally
inif.free;
end;
end;

initialization
  {$I pu_photlun.lrs}

end.

