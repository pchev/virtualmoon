unit pu_updcatalog;
{
Copyright (C) 2024 Patrick Chevalley

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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

{$mode ObjFPC}{$H+}

interface

uses u_constant, u_util, u_translation, UScaleDPI, downloaddialog,
  cu_httpdownload, FileUtil, md5, zipper, LazFileUtils, inifiles,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids, ComCtrls, StdCtrls, Menus, EditBtn, Types;

type

  TCatInfo = class(TObject)
    installed, prereqok, newversion, index: boolean;
    cattype,cdcminversion,version,catname,desc,size,url,prereq,path,checkfile: string;
    installedversion: string;
    grid: TStringGrid;
    constructor Create(data:Tstringlist);
    procedure SearchInstalled(basedir:string);
    procedure SearchPrereqInGrid(g:TStringGrid);
    function ListDependInGrid(g:TStringGrid): string;
    function ListSamePathInGrid(g:TStringGrid): string;
  end;

  { Tf_updcatalog }

  Tf_updcatalog = class(TForm)
    btnConfig: TButton;
    btnNo1: TButton;
    btnYes: TButton;
    btnNo: TButton;
    btnYes1: TButton;
    Button1: TButton;
    ButtonClose: TButton;
    ButtonAbort: TButton;
    Label1: TLabel;
    memomig: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelAction: TLabel;
    LabelProgress: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    GridBase: TStringGrid;
    PanelMigComplete: TPanel;
    PanelMigBtn: TPanel;
    PanelInfo: TPanel;
    PanelDownload: TPanel;
    ProgressBar1: TProgressBar;
    ProgressBarMigration: TProgressBar;
    ProgressCat: TLabel;
    TabSheetNewInstall: TTabSheet;
    TabSheetMigrate: TTabSheet;
    TabSheetBase: TTabSheet;
    EndInstallTimer: TTimer;
    procedure btnYesClick(Sender: TObject);
    procedure btnNoClick(Sender: TObject);
    procedure ButtonAbortClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure EndInstallTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure GridGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure BtnConfigClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    FSaveConfig, FChartRefresh, FActivatePictures: TNotifyEvent;
    InstallInfo: TCatInfo;
    httpdownload: THTTPBigDownload;
    FUnZipper: TUnZipper;
    FRunning,FAbort,FDownloadNow: boolean;
    ProgressIndex,ProgressCount: integer;
    FConfig: string;
    migration_response: string;
    ProgressMessage: string;
    LocalDataPath: string;
    procedure ClearGrid(g:TStringGrid);
    procedure LoadCatalogList;
    function UpdateList: boolean;
    procedure ShowStatus(grid: TStringGrid);
    procedure InstallDlg(info: TCatInfo);
    procedure UninstallDlg(info: TCatInfo);
    procedure Install(info: TCatInfo);
    procedure CopyOne(info: TCatInfo);
    procedure InstallOne(info: TCatInfo);
    procedure Uninstall(info: TCatInfo);
    procedure ShowProgress;
    procedure DownloadComplete;
    procedure UnZipFile(fn:string);
    procedure ActivateCatalog;
    procedure DownloadError;
    procedure SelectSourceClick(Sender: TObject);
   procedure ProgressEvent(Sender : TObject; Const Ratio : Double);
  public
    procedure SetLang;
    function ReadConfig(ConfigPath: string): boolean;
    procedure SaveConfig;
    procedure Abort;
    procedure InitialData;
    procedure BaseData;
    procedure Migrate;
    property Running: boolean read FRunning;
    property onSaveConfig: TNotifyEvent read FSaveConfig write FSaveConfig;
    property onChartRefresh: TNotifyEvent read FChartRefresh write FChartRefresh;
    property onActivatePictures: TNotifyEvent read FActivatePictures write FActivatePictures;
  end;

var
  f_updcatalog: Tf_updcatalog;

implementation

const colaction=0; colinstall=1; colname=2; coldesc=3; colsize=4; colinfo=5;

{$R *.lfm}

constructor TCatInfo.Create(data:Tstringlist);
begin
  inherited Create;
  cattype:=data[0];
  cdcminversion:=data[1];
  version:=data[2];
  catname:=data[3];
  desc:=data[4];
  size:=data[5];
  url:=data[6];
  prereq:=data[7];
  path:=data[8];
  checkfile:=data[9];
end;

procedure TCatInfo.SearchInstalled(basedir:string);
var f: TextFile;
    vf,cf: string;
begin
  vf:=slash(basedir)+slash(path)+catname+'_version.txt';
  installed:=FileExists(vf);
  cf:=slash(basedir)+slash(path)+checkfile;
  installed:=installed and FileExists(cf);
  if installed then begin
    AssignFile(f,vf);
    Reset(f);
    readln(f,installedversion);
    CloseFile(f);
    installedversion:=trim(installedversion);
    newversion:=(installedversion<version);
  end
  else begin
    newversion:=false;
    installedversion:='';
  end;
end;

procedure TCatInfo.SearchPrereqInGrid(g:TStringGrid);
var i:integer;
    info:TCatInfo;
begin
  if prereq='' then
    prereqok:=true
  else
  begin
    for i:=1 to g.RowCount-1 do begin
      info:=TCatInfo(g.Objects[colinstall,i]);
      if (info<>nil)and(info.catname=prereq) then begin
        if info.installed then begin
          info.SearchPrereqInGrid(g);
          prereqok:=info.prereqok;
        end
        else begin
          prereqok:=false;
        end;
      end;
    end;
  end;
end;

function TCatInfo.ListDependInGrid(g:TStringGrid): string;
var i:integer;
    info:TCatInfo;
begin
  result:='';
  for i:=1 to g.RowCount-1 do begin
    info:=TCatInfo(g.Objects[colinstall,i]);
    if (info<>nil)and(catname=info.prereq) then begin
      result:=result+';'+info.catname;
      result:=result+info.ListDependInGrid(g);
    end;
  end;
end;

function TCatInfo.ListSamePathInGrid(g:TStringGrid): string;
var i:integer;
    info:TCatInfo;
begin
  result:='';
  for i:=1 to g.RowCount-1 do begin
    info:=TCatInfo(g.Objects[colinstall,i]);
    if (catname<>info.catname)and(path=info.path) then begin
      result:=result+';'+info.catname;
    end;
  end;
end;

{ Tf_updcatalog }

procedure Tf_updcatalog.SetLang;
begin
  Caption:=rsInstallAddit;
  panel1.Caption:=rsSelectDataTo;
  GridBase.Columns[1].Title.Caption:=rsStatus;
  GridBase.Columns[2].Title.Caption:=rsPackage;
  GridBase.Columns[3].Title.Caption:=rsm_58;
  GridBase.Columns[4].Title.Caption:=rsm_57;
  GridBase.Columns[5].Title.Caption:=rst_6;
  ButtonAbort.Caption:=rsAbort;
  ButtonClose.Caption:=rst_30;
  BtnConfig.Caption:=rsConfigureThe;
  Label8.Caption:=rsDataMigratio;
  label5.Caption:=Format(rsThePicturesA,[crlf,crlf]);
  label3.Caption:=rsCopyFilesFro+':';
  label4.Caption:=rsTo+':';
  btnYes.Caption:=rsYes;
  btnNo.Caption:=rsNo;
  label9.Caption:=rsNewDataInsta;
  label6.Caption:=Format(rsTheBaseDataN,[crlf+' '+crlf]);
  label7.Caption:=rsDoYouWantToC;
  btnYes1.Caption:=rsYes;
  btnNo1.Caption:=rsNo;
  button1.Caption:=rsContinue;

end;

procedure Tf_updcatalog.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  SetLang;
  FUnZipper:=TUnZipper.Create;
  FUnZipper.OnEndFile:=@ProgressEvent;
  PageControl1.ActivePageIndex:=0;
  PanelInfo.Visible:=true;
  PanelDownload.Visible:=false;
  PanelMigComplete.Visible:=false;
  FAbort:=false;
  FRunning:=false;
  FDownloadNow:=true;
  GridBase.RowCount:=1;
  LocalDataPath:='';
  label1.Caption:='';
  label2.Caption:='';
  memomig.caption:='';
end;

procedure Tf_updcatalog.FormShow(Sender: TObject);
begin
  FRunning:=false;
  PanelInfo.Visible:=true;
  PanelDownload.Visible:=false;
  LoadCatalogList;
  FDownloadNow:=false;
end;

procedure Tf_updcatalog.FormDestroy(Sender: TObject);
begin
  FUNZipper.Free;
  ClearGrid(GridBase);
end;

procedure Tf_updcatalog.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_updcatalog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not FRunning;
end;

function Tf_updcatalog.ReadConfig(ConfigPath: string): boolean;
var
  ini: tinifile;
begin
  FConfig:='';
  Result := DirectoryExists(ConfigPath);
  if Result then
    FConfig := slash(ConfigPath) + 'data.ini'
  else
    exit;
  ini := tinifile.Create(FConfig);
  LocalDataPath := ini.ReadString('default', 'datapath', '');
  ini.Free;
end;

procedure Tf_updcatalog.SaveConfig;
var
  ini: tinifile;
begin
  if FConfig='' then exit;
  ini := tinifile.Create(FConfig);
  ini.writestring('default', 'datapath', LocalDataPath);
  ini.UpdateFile;
  ini.Free;
end;

procedure Tf_updcatalog.ClearGrid(g:TStringGrid);
var i: integer;
begin
  for i:=1 to g.RowCount-1 do begin
    if g.Objects[colinstall,i]<>nil then g.Objects[colinstall,i].Free;
    g.Objects[colinstall,i]:=nil;
  end;
  g.RowCount:=1;
end;

procedure Tf_updcatalog.LoadCatalogList;
var f: textfile;
    fn,buf: string;
    info: TCatInfo;
    row: Tstringlist;
    grid: TStringGrid;
begin
  UpdateList;
  fn := slash(appdir)+catalog_list;
  ClearGrid(GridBase);
  if FileExists(fn) then begin
    row := Tstringlist.Create;
    AssignFile(f,fn);
    Reset(f);
    repeat
      ReadLn(f,buf);
      if copy(buf,1,1)='#' then continue;
      Splitrec2(buf,';',row);
      if row.Count<10 then continue;
      if row[1] > AVLversion then continue; // skip catalog not supported by this version of the program
      // type of object
      if row[0]='base' then grid:=GridBase
      else if (not NeedToInstallData)and(row[0]='additional') then grid:=GridBase
      else if (not NeedToInstallData)and(row[0]='hires') then grid:=GridBase
      else if (not NeedToInstallData)and(row[0]='vhres') then grid:=GridBase
      else continue;
      info:=TCatInfo.Create(row);
      info.SearchInstalled(appdir);
      if info.newversion and (grid.Parent<>nil)and(grid.Parent is TTabSheet)and(copy(TTabSheet(grid.Parent).Caption,1,1)<>'*') then
        TTabSheet(grid.Parent).Caption:='* '+TTabSheet(grid.Parent).Caption;
      info.grid:=grid;
      grid.RowCount:=grid.RowCount+1;
      grid.Objects[colinstall,grid.RowCount-1]:=info;
      grid.Cells[colaction,grid.RowCount-1]:=Ellipsis;
      grid.Cells[colname,grid.RowCount-1]:=info.catname;
      grid.Cells[coldesc,grid.RowCount-1]:=info.desc;
      grid.Cells[colsize,grid.RowCount-1]:=info.size;
      grid.Cells[colinfo,grid.RowCount-1]:='?';
    until eof(f);
    CloseFile(f);
    row.Free;
    ShowStatus(GridBase);
  end;
end;

function Tf_updcatalog.UpdateList: boolean;
var
  dl: TDownloadDialog;
  fn: string;
  ft: TDateTime;
  doDownload: boolean;
begin
  result:=false;
  fn := slash(appdir)+catalog_list;
  doDownload:=true;
  if (not FDownloadNow) and FileExists(fn) then begin
    if FileAge(fn,ft) then begin
      doDownload:=(ft<(now-1));
    end;
  end;
  if doDownload then begin
    if LocalDataPath='' then begin
      dl := TDownloadDialog.Create(self);
      dl.UserAgent:='VirtualMoon-'+AVLversion;
      dl.ScaleDpi:=UScaleDPI.scale;
      try
  {      if Fcmain.HttpProxy then
        begin
          dl.SocksProxy := '';
          dl.SocksType := '';
          dl.HttpProxy := Fcmain.ProxyHost;
          dl.HttpProxyPort := Fcmain.ProxyPort;
          dl.HttpProxyUser := Fcmain.ProxyUser;
          dl.HttpProxyPass := Fcmain.ProxyPass;
        end
        else if Fcmain.SocksProxy then
        begin
          dl.HttpProxy := '';
          dl.SocksType := Fcmain.SocksType;
          dl.SocksProxy := Fcmain.ProxyHost;
          dl.HttpProxyPort := Fcmain.ProxyPort;
          dl.HttpProxyUser := Fcmain.ProxyUser;
          dl.HttpProxyPass := Fcmain.ProxyPass;
        end
        else }
        begin
          dl.SocksProxy := '';
          dl.SocksType := '';
          dl.HttpProxy := '';
          dl.HttpProxyPort := '';
          dl.HttpProxyUser := '';
          dl.HttpProxyPass := '';
        end;
        dl.ConfirmDownload := False;
        // try availability of faster download
        dl.URL := URL_CATALOG_LIST1+catalog_list1+'_'+uplanguage+'.txt';
        dl.SaveToFile := fn;
        result:=dl.Execute;
        if not result then begin
          dl.URL := URL_CATALOG_LIST1+catalog_list1+'_EN.txt';
          dl.SaveToFile := fn;
          result:=dl.Execute;
          if not result then begin
            // if not available try with sourceforge
            LabelProgress.Caption:=dl.ResponseText;
            dl.URL := URL_CATALOG_LIST2+catalog_list1+'_'+uplanguage+'.txt';
            dl.SaveToFile := fn;
            result:=dl.Execute;
            if not result then begin
              // if not available try with sourceforge
              LabelProgress.Caption:=dl.ResponseText;
              dl.URL := URL_CATALOG_LIST2+catalog_list1+'_EN.txt';
              dl.SaveToFile := fn;
              result:=dl.Execute;
            end;
            if not result then
              LabelProgress.Caption:=dl.ResponseText;
          end;
        end;
      finally
        dl.Free;
      end;
    end
    else begin
      if FileExists(slash(LocalDataPath)+catalog_list) then begin
        CopyFile(slash(LocalDataPath)+catalog_list,fn,[cffOverwriteFile]);
      end;
    end;
  end;
end;

procedure Tf_updcatalog.ShowStatus(grid: TStringGrid);
var i: integer;
    info: TCatInfo;
    txt: string;
    prereqgrid:TStringGrid;
begin
  for i:=1 to grid.RowCount-1 do begin
    info:=TCatInfo(grid.Objects[colinstall,i]);
    if (info=nil) then continue;
    prereqgrid:=grid;
    info.SearchPrereqInGrid(prereqgrid);
    if info.installed then begin
      if info.newversion then
        txt:=rsNewVersionAv
      else
        txt:=rsInstalled;
    end
    else begin
      if info.prereqok then
        txt:=rsInstall
      else
        txt:=rsMissingPrere;
    end;
    grid.Cells[colinstall,i]:=txt;
  end;
end;

procedure Tf_updcatalog.GridButtonClick(Sender: TObject; aCol, aRow: Integer);
var info: TCatInfo;
begin
  with Tstringgrid(Sender) do
  begin
    if (aRow >= 1) and (aCol = colaction) then
    begin
      if FRunning then Exit;
      FRunning:=True;
      FAbort:=false;
      ButtonClose.Enabled:=false;
      info:=TCatInfo(Objects[colinstall,aRow]);
      if (info=nil) then exit;
      if info.installed then
      begin
        if info.newversion then
          InstallDlg(info)
        else
          UninstallDlg(info);
      end
      else begin
        if info.prereqok then
          InstallDlg(info)
        else begin
          ShowMessage(rsMissingPrere+' '+info.prereq);
          FRunning:=False;
          ButtonClose.Enabled:=true;
        end;
      end;
    end
    else if (aRow >= 1) and (aCol = colinfo) then
    begin
       ExecuteFile('https://ap-i.net/avl/en/download');
    end;
  end;
end;

procedure Tf_updcatalog.GridGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
var txt: string;
begin
with sender as TStringGrid do begin
 if ((aCol=colinstall)or(aCol=colaction))and(arow>0) then begin
   if Objects[colinstall,ARow]<>nil then
   with Objects[colinstall,ARow] as TCatInfo do begin
      if installed then begin
        if newversion then
          txt:=rsNewVersionAv+', '+Format(rsInstallNewVe, [version, installedversion])
        else
          txt:=rsInstalled+': '+slash(appdir);
      end
      else begin
        if prereqok then
          txt:=rsInstall+': '+catname
        else
          txt:=rsMissingPrere+': '+prereq;
      end;
   end;
   HintText:=txt;
 end
 else if (trim(Cells[Acol,Arow])<>'')and(Canvas.TextWidth(Cells[Acol,Arow])>ColWidths[Acol]) then begin
   HintText:=Cells[Acol,Arow];
 end;
end;
end;

procedure Tf_updcatalog.GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  if (aCol=colinstall)and(aRow>0) then begin
    with sender as TStringGrid do begin
     if Objects[colinstall,ARow] <> nil then
      with Objects[colinstall,ARow] as TCatInfo do begin
         if installed then begin
           if newversion then
             Canvas.Pen.Color:=clYellow
           else
             Canvas.Pen.Color:=clLime;
         end
         else begin
           if prereqok then
             Canvas.Pen.Color:=clWindow
           else
             Canvas.Pen.Color:=clred;
         end;
      end;
      Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Width:=DoScaleX(3);
      Canvas.Brush.Style:=bsClear;
      Canvas.Rectangle(aRect);
    end;
  end;
end;

procedure Tf_updcatalog.BtnConfigClick(Sender: TObject);
var F: TForm;
    L1: Tlabel;
    R: TRadioGroup;
    L2: Tlabel;
    D: TDirectoryEdit;
    B: TButton;
begin
  F:= TForm.Create(self);
  F.Caption:=rsSelectTheDat;
  F.ChildSizing.LeftRightSpacing := 6;
  F.ChildSizing.TopBottomSpacing := 6;
  F.ChildSizing.EnlargeHorizontal := crsScaleChilds;
  F.ChildSizing.EnlargeVertical := crsAnchorAligning;
  F.ChildSizing.ShrinkHorizontal := crsScaleChilds;
  F.ChildSizing.ShrinkVertical := crsAnchorAligning;
  F.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  F.ChildSizing.ControlsPerLine := 1;
  F.Width:=DoScaleX(400);
  F.Height:=DoScaley(200);
  L1:= Tlabel.Create(F);
  L1.Parent:=F;
  L1.Caption:=rsSelectTheDat;
  R:= TRadioGroup.Create(F);
  R.Parent:=F;
  R.OnClick:=@SelectSourceClick;
  R.Items.Add(rsInternet);
  R.Items.Add(rsLocalFiles);
  L2:= Tlabel.Create(F);
  L2.Parent:=F;
  L2.Caption:=rsSelectLocalD;
  D:= TDirectoryEdit.Create(F);
  D.Name:='DirectoryEdit1';
  D.Parent:=F;
  D.Directory:=LocalDataPath;
  B:= TButton.Create(F);
  B.Parent:=F;
  B.Caption:=rst_18;
  B.ModalResult:=mrOK;
  if LocalDataPath='' then begin
    R.ItemIndex:=0;
    D.Enabled:=false;
  end
  else begin
    R.ItemIndex:=1;
    D.Enabled:=true;
  end;
  FormPos(F,mouse.CursorPos.X,mouse.CursorPos.Y);
  if F.ShowModal = mrOK then begin
    if R.ItemIndex=0 then
      LocalDataPath:=''
    else
      LocalDataPath:=D.Directory;
    SaveConfig;
    FDownloadNow:=true;
    LoadCatalogList;
  end;
  F.Free;
end;

procedure Tf_updcatalog.SelectSourceClick(Sender: TObject);
begin
  if Sender is TRadioGroup then
  with Sender as TRadioGroup do begin
    if Parent is TForm then begin
      with TDirectoryEdit(TForm(Parent).FindChildControl('DirectoryEdit1')) do begin
        Enabled:=TRadioGroup(Sender).ItemIndex=1;
      end;
    end;
  end;
end;

procedure Tf_updcatalog.PageControl1Change(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
    0: begin  // install data
         panel1.Visible:=true;
         panel2.Visible:=true;
       end;
    1: begin  // migrate data
         panel1.Visible:=false;
         panel2.Visible:=false;
       end;
    2: begin  // new base data
         panel1.Visible:=true;
         panel2.Visible:=false;
       end;
  end;
end;

procedure Tf_updcatalog.InstallDlg(info: TCatInfo);
begin
  if info.newversion then
  begin
    if MessageDlg(info.catname+crlf+Format(rsInstallNewVe, [info.version, info.installedversion]), mtConfirmation, mbYesNo, 0)=mryes then
    begin
      Uninstall(info);
      Install(info);
    end
    else EndInstallTimer.Enabled:=true;
  end
  else begin
    if (info.catname='Base_Data')or(MessageDlg(rsInstall+' '+info.catname+crlf+info.desc,mtConfirmation,mbYesNo,0)=mryes) then
    begin
      Install(info);
    end
    else EndInstallTimer.Enabled:=true;
  end;
end;

procedure Tf_updcatalog.UninstallDlg(info: TCatInfo);
begin
  if MessageDlg(rsUninstall+' '+info.catname, mtConfirmation, mbYesNo, 0)=mryes then
  begin
    Uninstall(info);
  end;
  EndInstallTimer.Enabled:=true;
end;

procedure Tf_updcatalog.Install(info: TCatInfo);
var ext: string;
begin
  ext := LowerCase(ExtractFileExt(info.url));
  if (ext='.zip') then begin
    if LocalDataPath='' then
      InstallOne(info)
    else
      CopyOne(info);
  end
  else begin
    ShowMessage('Catalog file download is '+ext+', only .zip is supported');
    EndInstallTimer.Enabled:=true;
    Exit;
  end;
end;

procedure Tf_updcatalog.CopyOne(info: TCatInfo);
var src,fn: string;
begin
try
  PanelInfo.Visible:=false;
  PanelDownload.Visible:=true;
  ButtonAbort.Visible:=true;
  LabelAction.Caption:=rsInstalling+' '+info.catname+Ellipsis;
  LabelProgress.Caption:='';
  InstallInfo:=info;
  fn:=slash(TempDir)+'catalog.zip';
  src:=slash(LocalDataPath)+ExtractFileName(info.url);
  ProgressMessage:=rsCopyFile+' :'+src;
  LabelProgress.Caption:=ProgressMessage;
  Application.ProcessMessages;
  CopyFile(src,fn,[cffOverwriteFile]);
  ProgressMessage:=rsCopyComplete;
  LabelProgress.Caption:=ProgressMessage;
  Application.ProcessMessages;
  if FileExists(fn) then
  begin
     UnZipFile(fn);
  end
  else
  begin
    ShowMessage(rsCopyFileErro);
  end;
finally
  EndInstallTimer.Enabled:=true;
end;
end;

procedure Tf_updcatalog.InstallOne(info: TCatInfo);
var fn: string;
begin
  httpdownload:=THTTPBigDownload.Create(true);
{  if Fcmain.HttpProxy then
  begin
    httpdownload.SocksProxy := '';
    httpdownload.SocksType := '';
    httpdownload.HttpProxy := Fcmain.ProxyHost;
    httpdownload.HttpProxyPort := Fcmain.ProxyPort;
    httpdownload.HttpProxyUser := Fcmain.ProxyUser;
    httpdownload.HttpProxyPass := Fcmain.ProxyPass;
  end
  else if Fcmain.SocksProxy then
  begin
    httpdownload.HttpProxy := '';
    httpdownload.SocksType := Fcmain.SocksType;
    httpdownload.SocksProxy := Fcmain.ProxyHost;
    httpdownload.HttpProxyPort := Fcmain.ProxyPort;
    httpdownload.HttpProxyUser := Fcmain.ProxyUser;
    httpdownload.HttpProxyPass := Fcmain.ProxyPass;
  end
  else   }
  begin
    httpdownload.SocksProxy := '';
    httpdownload.SocksType := '';
    httpdownload.HttpProxy := '';
    httpdownload.HttpProxyPort := '';
    httpdownload.HttpProxyUser := '';
    httpdownload.HttpProxyPass := '';
  end;
  fn:=slash(TempDir)+'catalog.zip';
  httpdownload.url:=info.url;
  httpdownload.filename:=fn;
  httpdownload.onProgress:=@ShowProgress;
  httpdownload.onDownloadComplete:=@DownloadComplete;
  httpdownload.onDownloadError:=@DownloadError;
  PanelInfo.Visible:=false;
  PanelDownload.Visible:=true;
  ButtonAbort.Visible:=true;
  LabelAction.Caption:=rsInstalling+' '+info.catname+Ellipsis;
  LabelProgress.Caption:='';
  InstallInfo:=info;
  httpdownload.Start;
end;

procedure Tf_updcatalog.DownloadComplete;
var fn: string;
begin
try
  ButtonAbort.Visible:=false;
  fn:=httpdownload.filename;
  if httpdownload.HttpResult and FileExists(fn) then
  begin
     UnZipFile(fn);
  end
  else
  begin
    ShowMessage(rsDownloadErro+': '+httpdownload.HttpErr);
  end;
finally
  EndInstallTimer.Enabled:=true;
end;
end;

procedure Tf_updcatalog.UnZipFile(fn:string);
var ok: boolean;
    i:integer;
    ff,fd:TextFile;
begin
     ProgressIndex:=0;
     ok:=true;
     try
       ProgressCount:=0;
       FUnZipper.FileName:=fn;
       FUnZipper.OutputPath:=appdir;
       FUnZipper.UseUTF8:=True;
       FUnZipper.Examine;
       CreateDir(slash(appdir)+slash(InstallInfo.path));
       AssignFile(ff,slash(appdir)+slash(InstallInfo.path)+InstallInfo.catname+'_files.txt');
       rewrite(ff);
       AssignFile(fd,slash(appdir)+slash(InstallInfo.path)+InstallInfo.catname+'_dir.txt');
       rewrite(fd);
       for i:=0 to FUnZipper.Entries. Count-1 do begin
         if AnsiLastChar(FUnZipper.Entries[i].ArchiveFileName)='/' then
           writeln(fd,FUnZipper.Entries[i].ArchiveFileName)
         else
           writeln(ff,FUnZipper.Entries[i].ArchiveFileName);
       end;
       CloseFile(ff);
       CloseFile(fd);
       FUnZipper.UnZipAllFiles;
     except
          on E: Exception do
          begin
            ok:=false;
            ShowMessage('Unzip error : '+E.Message)
          end;
     end;
     if ok then
     begin
        ActivateCatalog;
        DeleteFile(fn);
     end;
end;

procedure Tf_updcatalog.ActivateCatalog;
var f: textfile;
begin
  AssignFile(f,slash(appdir)+slash(InstallInfo.path)+InstallInfo.catname+'_version.txt');
  Rewrite(f);
  WriteLn(f,InstallInfo.version);
  CloseFile(f);
end;

procedure Tf_updcatalog.DownloadError;
begin
try
 ShowMessage(rsDownloadErro+': '+httpdownload.HttpErr);
finally
  EndInstallTimer.Enabled:=true;
end;
end;

procedure Tf_updcatalog.EndInstallTimerTimer(Sender: TObject);
begin
  EndInstallTimer.Enabled:=false;
  PanelInfo.Visible:=true;
  PanelDownload.Visible:=false;
  LoadCatalogList;
  FRunning:=False;
  ButtonClose.Enabled:=true;
  ProgressIndex:=0;
  if assigned(FChartRefresh) then FChartRefresh(self);
end;

procedure Tf_updcatalog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig;
end;

procedure Tf_updcatalog.ButtonAbortClick(Sender: TObject);
begin
  Abort;
end;

procedure Tf_updcatalog.Abort;
begin
try
  if FAbort then begin
    // second tentative, force aborting
    httpdownload.AbortForce;
    EndInstallTimer.Enabled:=true;
  end
  else begin
    // try soft abort
    FAbort:=true;
    httpdownload.Abort;
  end;
except
end;
end;

procedure Tf_updcatalog.ShowProgress;
begin
  LabelProgress.Caption:=httpdownload.progresstext;
end;

procedure Tf_updcatalog.ProgressEvent(Sender : TObject; Const Ratio : Double);
begin
  inc(ProgressCount);
  if (ProgressCount mod 100) = 0 then begin
    ProgressMessage:=inttostr(ProgressIndex+ProgressCount)+' '+rsFilesExtract+Ellipsis;
    LabelProgress.Caption:=ProgressMessage;
    Application.ProcessMessages;
  end;
end;

procedure Tf_updcatalog.Uninstall(info: TCatInfo);
var fn,ff: string;
    i,j: integer;
    f: TStringList;
begin
try
  if (trim(Appdir)='') then exit;
  PanelInfo.Visible:=false;
  PanelDownload.Visible:=true;
  LabelAction.Caption:=rsDelete+blank+Info.catname;
  Application.ProcessMessages;
  try
  f:=TStringList.Create;
  ff:=slash(appdir)+slash(info.path)+info.catname+'_files.txt';
  if FileExists(ff) then begin
    f.LoadFromFile(ff);
    {$i-}  // ignore missing files or other errors
    for i:=0 to f.count-1 do begin
      fn:=f[i];
      DeleteFile(slash(appdir)+fn);
      j:=IOResult;
    end;
    {$i+}
  end;
  f.Clear;
  ff:=slash(appdir)+slash(info.path)+info.catname+'_dir.txt';
  if FileExists(ff) then begin
    f.LoadFromFile(ff);
    {$i-}  // ignore missing or non empty directories or other errors
    for i:=f.count-1 downto 0 do begin
      fn:=f[i];
      RmDir(slash(appdir)+fn);
      j:=IOResult;
    end;
   {$i+}
  end;
  f.Free;
  DeleteFile(slash(appdir)+slash(info.path)+info.catname+'_files.txt');
  DeleteFile(slash(appdir)+slash(info.path)+info.catname+'_dir.txt');
  DeleteFile(slash(appdir)+slash(info.path)+info.catname+'_version.txt');
  if Assigned(FSaveConfig) then FSaveConfig(self);
  except // ignore file deletion errors
  end;
finally
  LabelAction.Caption:='';
  PanelInfo.Visible:=true;
  PanelDownload.Visible:=false;
end;
end;


procedure Tf_updcatalog.btnYesClick(Sender: TObject);
begin
  migration_response:='yes';
end;

procedure Tf_updcatalog.btnNoClick(Sender: TObject);
begin
  migration_response:='no';
end;

procedure Tf_updcatalog.InitialData;
begin
  Migrate;
  BaseData;
end;

procedure Tf_updcatalog.BaseData;
begin
  PageControl1.ActivePageIndex:=2;

  migration_response:='';
  while  migration_response='' do begin
    sleep(100);
    Application.ProcessMessages;
  end;

  if migration_response='yes' then begin
    PageControl1.ActivePageIndex:=0;
    Application.ProcessMessages;
    if GridBase.RowCount<2 then begin
      BtnConfigClick(self);
      LoadCatalogList;
    end;
    if GridBase.RowCount<2 then begin
      MessageDlg(rsError2, rsCannotInstal+'!', mtError, [mbClose], 0);
      exit;
    end;
    GridButtonClick(GridBase,colaction,1);
    while FRunning do begin
      Application.ProcessMessages;
      sleep(100);
    end;
    SaveConfig;
  end;
end;

procedure Tf_updcatalog.Migrate;
const
  {$ifdef linux}
  oldpath1='/usr/share/virtualmoon';
  oldpath2='/usr/local/share/virtualmoon';
  {$endif}
  {$ifdef windows}
  oldpath1='\Program Files (x86)\VirtualMoon';
  oldpath2='\Program Files\VirtualMoon';
  {$endif}
  oldversion='20250615'; // version 9
  newversion='20261018'; // version 10 for unchanged files
  max_amateur=9;
  pict_amateur: array [0..max_amateur] of string = ('BestOfAmateurs','BestOfHiggins','BestOfLazzarotti','Best_Pic_du_Midi','Best_of_Peach','BestOfCathala','BestOfBrahic','BestOfViladrich','BestOfHeully','BestofPelissard');
  max_probe=9;
  pict_probe: array [0..max_probe] of string = ('Lopam','Clementine','CLA','LAC_LM','ApolloMapping','LunarPits','Smart-1','Artemis2','Bestofhistorical','BestofLROobliques');
  max_texture=14;
  textures: array [0..max_texture] of string = ('Airbrush','Historical','Overlay','WAC_Color_Shade','Airbrush_no_albedo','Clementine','USGS Geological 2020','LAC','CNSA Geological map','Clementine_Negative','Change','Lopam','LOLA_Kaguya_Shade','WAC_LOWSUN','WAC');
var source,dest: string;
   i: integer;
   f: textfile;
   ok: boolean;
begin
 {$ifdef darwin}
  exit;   // no previous Mac version
 {$endif}

 if DirectoryExists(oldpath1) then
   source:=oldpath1
 else if DirectoryExists(oldpath2) then
   source:=oldpath2
 else
   exit;  // no previous standard install

  try
  PageControl1.ActivePageIndex:=1;
  PanelMigBtn.Visible:=true;
  PanelMigComplete.Visible:=false;
  label1.Caption:=source;
  label2.Caption:=slash(appdir);
  ProgressBarMigration.Max:=max_amateur+max_probe+max_texture*10;
  ProgressBarMigration.Position:=0;
  memomig.caption:='';

  migration_response:='';
  while  migration_response='' do begin
    sleep(100);
    Application.ProcessMessages;
  end;
  PanelMigBtn.Visible:=false;

  if migration_response='yes' then begin
    Application.ProcessMessages;
    // Amateur Pictures
    dest:=slash(appdir)+'Pictures';
    ForceDirectories(dest);
    ok:=false;
    for i:=0 to max_amateur do begin
      ProgressBarMigration.Position:=ProgressBarMigration.Position+1;
      if DirectoryExists(slash(source)+pict_amateur[i]) then begin
        label1.Caption:=slash(source)+pict_amateur[i];
        label2.Caption:=slash(dest)+pict_amateur[i];
        Application.ProcessMessages;
        ok:=CopyDirTree(slash(source)+pict_amateur[i],slash(dest)+pict_amateur[i]) or ok;
      end;
    end;
    if ok then begin
      AssignFile(f,slash(dest)+'Picture1_version.txt');
      Rewrite(f);
      WriteLn(f,oldversion);
      CloseFile(f);
    end;
    // Probes Pictures
    ok:=false;
    for i:=0 to max_probe do begin
      ProgressBarMigration.Position:=ProgressBarMigration.Position+10;
      if DirectoryExists(slash(source)+pict_probe[i]) then begin
        label1.Caption:=slash(source)+pict_probe[i];
        label2.Caption:=slash(dest)+pict_probe[i];
        Application.ProcessMessages;
        ok:=CopyDirTree(slash(source)+pict_probe[i],slash(dest)+pict_probe[i]) or ok;
      end;
    end;
    if ok then begin
      AssignFile(f,slash(dest)+'Picture2_version.txt');
      Rewrite(f);
      WriteLn(f,oldversion);
      CloseFile(f);
    end;
    // Textures
    dest:=slash(appdir)+'Textures';
    ForceDirectories(dest);
    for i:=0 to max_texture do begin
      ProgressBarMigration.Position:=ProgressBarMigration.Position+1;
      if DirectoryExists(slash(source)+slash('Textures')+textures[i]) then begin
        label1.Caption:=slash(source)+slash('Textures')+textures[i];
        label2.Caption:=slash(dest)+textures[i];
        Application.ProcessMessages;
        ok:=CopyDirTree(slash(source)+slash('Textures')+textures[i],slash(dest)+textures[i]) or ok;
      end;
    end;
    ProgressBarMigration.Position:=ProgressBarMigration.Max;

    if FileExists(slash(dest)+slash('WAC')+slash('L4')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'Textures_version.txt');
      Rewrite(f);
      WriteLn(f,oldversion);
      CloseFile(f);
    end;

    if FileExists(slash(dest)+slash('LAC')+slash('L1')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'HiresLac_version.txt');
      Rewrite(f);
      WriteLn(f,oldversion);
      CloseFile(f);
    end;

    if FileExists(slash(dest)+slash('Lopam')+slash('L5')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'HiresLopam_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;
    if FileExists(slash(dest)+slash('Lopam')+slash('L6')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'VhLopam_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;
    if FileExists(slash(dest)+slash('Change')+slash('L6')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'VhChange_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;

    if FileExists(slash(dest)+slash('WAC')+slash('L5')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'HiresWac_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;
    if FileExists(slash(dest)+slash('WAC')+slash('L6')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'VhWac_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;
    if FileExists(slash(dest)+slash('WAC_LOWSUN')+slash('L6')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'VhWacLs_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;

    if FileExists(slash(dest)+slash('LOLA_Kaguya_Shade')+slash('L5')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'HiresLola_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;
    if FileExists(slash(dest)+slash('LOLA_Kaguya_Shade')+slash('L6')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'VhLola_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;

    PanelMigComplete.Visible:=true;
    memomig.Lines.Add(rsToSaveSpaceO+':');
    memomig.Lines.Add(slash(source)+'Textures');
    memomig.Lines.Add(rsAndThePictur);
    {$ifdef linux}
    memomig.Lines.Add(Format(rsToDoThatRemo,['"sudo apt-get remove"'])+':');
    memomig.Lines.Add('virtualmoon-basedata virtualmoon-data virtualmoon-picture');
    memomig.Lines.Add('virtualmoon-artemis2 virtualmoon-hires virtualmoon-vhres-*');
    {$endif}
    migration_response:='';
    while  migration_response='' do begin
      sleep(100);
      Application.ProcessMessages;
    end;

   end;
   finally
   PageControl1.ActivePageIndex:=0;
   end;
end;

end.

