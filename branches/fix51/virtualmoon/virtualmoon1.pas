unit virtualmoon1;

{$MODE delphi}
{$H+}

{
Copyright (C) 2003 Patrick Chevalley

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
{$ifdef mswindows}
  Windows, Registry, ShlObj,
{$endif}
{$IFDEF LCLgtk}
  GtkProc,
{$endif}
{$IFDEF LCLgtk2}
  Gtk2Proc,
{$endif} glscene,
  u_translation_database, u_translation,
  u_constant, u_util, cu_planet, u_projection, cu_tz, pu_moon,
  LCLIntf, Forms, StdCtrls, ExtCtrls, Graphics, Grids,
  mlb2, PrintersDlgs, Printers, Controls, DateUtils,
  Messages, SysUtils, Classes, Dialogs, FileUtil,
  ComCtrls, Menus, Buttons, dynlibs, BigIma,
  EnhEdits, IniFiles, passql, passqlite,
  Math, CraterList, LResources, IpHtml, UniqueInstance, GLViewer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button12: TButton;
    Button13: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button21: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    Desc1:   TIpHtmlPanel;
    Edit5: TEdit;
    FilePopup: TPopupMenu;
    DoNotRemove: TGLSceneViewer;
    HelpPopup: TPopupMenu;
    Label17: TLabel;
    Label18: TLabel;
    Label22: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    LabelIncl: TLabel;
    LabelAltitude: TLabel;
    Label5: TLabel;
    FullScreen1: TMenuItem;
    DecreaseFont1: TMenuItem;
    IncreaseFont1: TMenuItem;
    OptFeatures1: TMenuItem;
    PanelTel: TPanel;
    PanelRot: TPanel;
    PanelMoon: TPanel;
    PanelMoon2: TPanel;
    PrintDialog1: TPrintDialog;
    Quitter1: TMenuItem;
    PageControl1: TNoteBook;
    Position: TPage;
    Panel1:  TPanel;
    Button1: TButton;
    Button2: TButton;
    Ephemerides: TPage;
    Panel4:  TPanel;
    Label6:  TLabel;
    Label9:  TLabel;
    jour:    TLongEdit;
    mois:    TLongEdit;
    annee:   TLongEdit;
    RadioGroup2: TRadioGroup;
    seconde: TLongEdit;
    minute:  TLongEdit;
    heure:   TLongEdit;
    Button4: TButton;
    Button5: TButton;
    Apropos1: TMenuItem;
    SpeedButton7: TSpeedButton;
    Splitter1: TSplitter;
    GridButton: TToolButton;
    Splitter2: TSplitter;
    StartTimer: TTimer;
    ResizeTimer: TTimer;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    TrackBar6: TTrackBar;
    TrackBar7: TTrackBar;
    TrackBar8: TTrackBar;
    trackdelay: TUpDown;
    ZoomTimer: TTimer;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    UpDown3: TUpDown;
    UpDown4: TUpDown;
    UpDown5: TUpDown;
    UpDown6: TUpDown;
    GroupBox1: TGroupBox;
    Label7:  TLabel;
    Terminateur: TPage;
    Reglage: TPage;
    TrackBar2: TTrackBar;
    Label8:  TLabel;
    TrackBar3: TTrackBar;
    Label11: TLabel;
    TrackBar4: TTrackBar;
    Label12: TLabel;
    Bevel1:  TBevel;
    Label14: TLabel;
    ComboBox1: TComboBox;
    Button3: TSpeedButton;
    Button6: TSpeedButton;
    Button7: TSpeedButton;
    Button8: TSpeedButton;
    EphTimer1: TTimer;
    Panel5:  TPanel;
    ListBox1: TListBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    RadioGroup1: TRadioGroup;
    Aide2:   TMenuItem;
    StringGrid1: TStringGrid;
    PopupMenu1: TPopupMenu;
    Position1: TMenuItem;
    Image2:  TMenuItem;
    Centre1: TMenuItem;
    Zoom1:   TMenuItem;
    x11:     TMenuItem;
    x21:     TMenuItem;
    x41:     TMenuItem;
    Outils:  TPage;
    Bevel4:  TBevel;
    Label23: TLabel;
    Button11: TButton;
    Edit1:   TEdit;
    Edit2:   TEdit;
    Label24: TLabel;
    Label25: TLabel;
    Distance1: TMenuItem;
    CartesduCiel1: TMenuItem;
    Imprimer1: TMenuItem;
    Enregistrersous1: TMenuItem;
    Voisinage1: TMenuItem;
    BMP1:    TMenuItem;
    SaveDialog1: TSaveDialog;
    JPG1:    TMenuItem;
    PrinterSetupDialog1: TPrinterSetupDialog;
    Selectiondimprimante1: TMenuItem;
    Memo2:   TMemo;
    PopupMenu2: TPopupMenu;
    Copy1:   TMenuItem;
    SelectAll1: TMenuItem;
    StatusBar1: TStatusBar;
    N1:      TMenuItem;
    N2:      TMenuItem;
    N3:      TMenuItem;
    Glossaire1: TMenuItem;
    ControlBar1: TPanel;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton8: TToolButton;
    ToolBar1: TToolBar;
    Label10: TLabel;
    TrackBar1: TTrackBar;
    ToolButton9: TToolButton;
    ToolButton5: TToolButton;
    ToolButton7: TToolButton;
    ToolButton10: TToolButton;
    dbtab:   TPage;
    StringGrid2: TStringGrid;
    Panel6:  TPanel;
    Button9: TButton;
    Edit3:   TEdit;
    Edit4:   TEdit;
    Label1:  TLabel;
    Label2:  TLabel;
    Label3:  TLabel;
    Enregistredist: TButton;
    btnEffacer: TButton;
    ToolButton3: TToolButton;
    BMP15001: TMenuItem;
    Eyepiece1: TMenuItem;
    e11:     TMenuItem;
    e21:     TMenuItem;
    e31:     TMenuItem;
    e41:     TMenuItem;
    e51:     TMenuItem;
    e61:     TMenuItem;
    e71:     TMenuItem;
    e81:     TMenuItem;
    e91:     TMenuItem;
    e101:    TMenuItem;
    e01:     TMenuItem;
    BMP30001: TMenuItem;
    Notes:   TPage;
    Memo1:   TMemo;
    Panel7:  TPanel;
    notes_name: TLabel;
    Button15: TButton;
    Notes1:  TMenuItem;
    Panel8:  TPanel;
    Label13: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    TrackBar5: TTrackBar;
    Button14: TButton;
    GroupBox2: TGroupBox;
    Rotation1: TMenuItem;
    N5seconde1: TMenuItem;
    Stop1:   TMenuItem;
    N10seconde1: TMenuItem;
    N1seconde1: TMenuItem;
    N02seconde1: TMenuItem;
    N05seconde1: TMenuItem;
    EastWest1: TMenuItem;
    GroupBox3: TGroupBox;
    Label4:  TLabel;
    ComboBox4: TComboBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    OverlayCaption1: TMenuItem;
    OverlayCaption2: TMenuItem;
    TelescopeTimer: TTimer;
    NM:      TImage;
    LabelNM: TLabel;
    LabelFQ: TLabel;
    LabelFM: TLabel;
    LabelLQ: TLabel;
    FQ:      TImage;
    FM:      TImage;
    LQ:      TImage;
    nextM:   TImage;
    prevM:   TImage;
    dbm:     TLiteDB;
    Encyclopedia1: TMenuItem;
    NewWindowButton: TToolButton;
    Snapshot1: TMenuItem;
    LibrationButton: TToolButton;
    PhaseButton: TToolButton;
    Button10: TButton;
    DataBase1: TMenuItem;
    N4:      TMenuItem;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    Button19: TButton;
    Button20: TButton;
    ToolButton11: TToolButton;
    RemoveMark1: TMenuItem;
    ButtonDatabase: TToolButton;
    CheckBox8: TCheckBox;
    ImageList1: TImageList;
    ToolButton12: TToolButton;
    procedure Button21Click(Sender: TObject);
    procedure Button3MouseLeave(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure DecreaseFont1Click(Sender: TObject);
    procedure FullScreen1Click(Sender: TObject);
    procedure GridButtonClick(Sender: TObject);
    procedure Desc1HotClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure IncreaseFont1Click(Sender: TObject);
    procedure OptFeatures1Click(Sender: TObject);
    procedure Quitter1Click(Sender: TObject);
    procedure Configuration1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ResizeTimerTimer(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure StartTimerTimer(Sender: TObject);
    procedure ToolButton12Click(Sender: TObject);
    procedure ToolButton14Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToolButton8Click(Sender: TObject);
    procedure Apropos1Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure Button3MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Button3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Button6MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Button7MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Button8MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure EphTimer1Timer(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Aide2Click(Sender: TObject);
    procedure Position1Click(Sender: TObject);
    procedure TrackBar6Change(Sender: TObject);
    procedure TrackBar7Change(Sender: TObject);
    procedure TrackBar8Change(Sender: TObject);
    procedure x21Click(Sender: TObject);
    procedure x41Click(Sender: TObject);
    procedure Button12MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Button13MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Button11Click(Sender: TObject);
    procedure Distance1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CartesduCiel1Click(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure BMP1Click(Sender: TObject);
    procedure JPG1Click(Sender: TObject);
    procedure Selectiondimprimante1Click(Sender: TObject);
    procedure Imprimer1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure Glossaire1Click(Sender: TObject);
    procedure x81Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure StringGrid2SetEditText(Sender: TObject; ACol, ARow: integer;
      const Value: string);
    procedure EnregistredistClick(Sender: TObject);
    procedure btnEffacerClick(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure BMP15001Click(Sender: TObject);
    procedure ZoomEyepieceClick(Sender: TObject);
    procedure BMP30001Click(Sender: TObject);
    procedure UpdNotesClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Notes1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure Stop1Click(Sender: TObject);
    procedure N5seconde1Click(Sender: TObject);
    procedure N10seconde1Click(Sender: TObject);
    procedure N1seconde1Click(Sender: TObject);
    procedure N02seconde1Click(Sender: TObject);
    procedure N05seconde1Click(Sender: TObject);
    procedure EastWest1Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure OverlayCaption1Click(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure TelescopeTimerTimer(Sender: TObject);
    procedure NMClick(Sender: TObject);
    procedure FQClick(Sender: TObject);
    procedure FMClick(Sender: TObject);
    procedure LQClick(Sender: TObject);
    procedure prevMClick(Sender: TObject);
    procedure nextMClick(Sender: TObject);
    procedure Encyclopedia1Click(Sender: TObject);
    procedure NewWindowButtonClick(Sender: TObject);
    procedure Snapshot1Click(Sender: TObject);
    procedure LibrationButtonClick(Sender: TObject);
    procedure PhaseButtonClick(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure DataBase1Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure RemoveMark1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure ZoomTimerTimer(Sender: TObject);
  private
    UniqueInstance1: TCdCUniqueInstance;
    moon1, moon2, activemoon : TF_moon;
    CursorImage1: TCursorImage;
    tz: TCdCTimeZone;
    ima: TBigImaForm;
    ToolsWidth: integer;
    FullScreen: boolean;
    lockzoombar: boolean;
    texturefiles: TStringList;
    SplitSize: single;
    nutl,nuto,abe,abp,sunl,sunb,ecl:double;
    firstuse,CanCloseDatlun,CanClosePhotlun,CanCloseCDC,StartDatlun,StartPhotlun,StartCDC: boolean;
    Desctxt: string;
    {$ifdef windows}
    savetop,saveleft,savewidth,saveheight: integer;
    {$endif}
    procedure OpenDatlun(objname,otherparam:string);
    procedure OpenPhotlun(objname,otherparam:string);
    procedure OpenCDC(objname,otherparam:string);
    procedure OtherInstance(Sender : TObject; ParamCount: Integer; Parameters: array of String);
    procedure InstanceRunning(Sender : TObject);
    procedure SetEyepieceMenu;
    procedure SetLang1;
    procedure SetLang;
    procedure InitObservatoire;
    procedure GetAppDir;
    function GetTimeZone(sdt: Tdatetime): double;
    function GetJDTimeZone(jdt: double): double;
    procedure InitImages;
    procedure AddImagesDir(dir, nom, cpy, rot: string);
    procedure ReadParam(first:boolean=true);
    procedure SetObs(param: string);
    procedure Readdefault;
    procedure SaveDefault;
    procedure UpdTerminateur(range:double=12);
    procedure AddToList(buf: string);
    procedure GetDetail(row: TResultRow; memo: Tmemo);
    procedure GetHTMLDetail(row: TResultRow; var txt: string);
    procedure GetNotes(n: string);
    function ImgExists(nom: string): boolean;
    procedure InitDate;
    procedure SetJDDate;
    procedure GetMsg(Sender: TObject; msgclass:TMoonMsgClass; value: String);
    procedure IdentLB(l, b: single);
    procedure InitLopamIdx;
    procedure ListUserDB;
    procedure ShowImg(desc, nom: string; forceinternal: boolean);
    procedure RefreshPhase;
    procedure SetDate(param: string);
    procedure SetDescText(const Value: string);
    procedure SetZoomBar;
    procedure GetSkychartInfo;
    procedure SetActiveMoon(mf: Tf_moon);
    procedure MoonClickEvent(Sender: TObject; Button: TMouseButton;
                     Shift: TShiftState; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single);
    procedure MoonMoveEvent(Sender: TObject; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single);
    procedure MoonMeasureEvent(Sender: TObject; m1,m2,m3,m4: string);
    public
    autolabelcolor: Tcolor;
    lastx, lasty, lastyzoom, MaxSprite: integer;
    LastIma, maximgdir, startx, starty, saveimagesize: integer;
    LeftMargin, PrintTextWidth, clickX, clickY: integer;
    PrintEph, PrintDesc, phaseeffect, externalimage, PrintChart, lopamdirect: boolean;
    librl, librb, wheelstep, EphStep, fov, searchl,
    searchb, markx, marky, flipx, rotstep, lunaison: double;
    ra, Dec, rad, ded, dist, dkm, phase, illum, pa, sunincl, currentphase,
    tphase, by, bxpos, dummy: double;
    editrow, notesrow, rotdirection, searchpos,BumpMethod: integer;
    dbedited: boolean;
    SkipIdent, wantbump, geocentric, FollowNorth, notesok, notesedited,
    minilabel, shortdesc, BumpMipmap: boolean;
    lockmove, lockrepeat, showlibrationmark, saveimagewhite, skipresize,skiporient, skiprot: boolean;
    searchtext, imac1, imac2, imac3, lopamplateurl, lopamnameurl,
    lopamdirecturl, lopamlocalurl, lopamplatesuffix, lopamnamesuffix,
    lopamdirectsuffix, lopamlocalsuffix: string;
    externalimagepath, helpprefix, ruklprefix, ruklsuffix,
    scopeinterface, markname, currentname, currentid: string;
    appname, pofile: string;
    //m: array[1..nummessage] of string;
    num_bl: integer;
    bldb: array[1..20] of string;
    CameraOrientation, PoleOrientation, startl, startb, startxx, startyy: double;
    curx, cury: double;
    LabelDensity, phaseoffset, gridspacing: integer;
    overlaytr: single;
    perfdeltay: double;
    ddeparam, currenttexture, overlayname, currentselection: string;
    CielHnd: Thandle;
    lockchart: boolean;
    StartedByDS: boolean;
    distancestart: boolean;
    param:  TStringList;
    imgdir: array of array[0..2] of string;
    LONGIN, LATIN, WIDEKM, WIDEMI, LENGHTKM, LENGHTMI, FNAME, INTERESTN,
    DIAMINST, wordformat: integer;
    overlayhi, overlayimg: Tbitmap;
    eyepiecename: array[1..10] of string;
    eyepiecefield, eyepiecemirror, eyepiecerotation: array[1..10] of integer;
    EyepieceRatio: double;
    zoom:   double;
    useDBN: integer;
    nmjd, fqjd, fmjd, lqjd, currentl, currentb: double;
    searchlist: TStringList;
    compresstexture,antialias : boolean;
    ForceBumpMapSize: integer;
    showoverlay: boolean;
    LastScopeTracking: double;
    UseComputerTime: boolean;
    procedure Init;
    procedure LoadOverlay(fn: string; transparent: single);
    procedure GetLabel(Sender: TObject);
    procedure GetSprite(Sender: TObject);
    function SearchAtPos(l, b: double): boolean;
    procedure ListObject(delta: double);
    procedure InitTelescope;
    procedure SetFullScreen;
    procedure GetDBgrid;
    procedure RefreshMoonImage;
    function SearchName(n: string; center: boolean): boolean;
  end;

var
  Form1:   TForm1;
  dblox, dbnotes: TMlb2;
  Fplanet: TPlanet;

implementation

uses
  telescope, config, splashunit, pu_features,
  glossary, fmsg, dbutil, LCLProc;

procedure TForm1.SetEyepieceMenu;
var
  i: integer;

  procedure setmenuitem(mi: Tmenuitem; i: integer);
  begin
    if (trim(eyepiecename[i]) > '') and (eyepiecefield[i] > 0) then
    begin
      mi.Visible := True;
      mi.Caption := eyepiecename[i];
    end
    else
      mi.Visible := False;
  end;

begin
  for i := 1 to 10 do
    case i of
      1: setmenuitem(e11, i);
      2: setmenuitem(e21, i);
      3: setmenuitem(e31, i);
      4: setmenuitem(e41, i);
      5: setmenuitem(e51, i);
      6: setmenuitem(e61, i);
      7: setmenuitem(e71, i);
      8: setmenuitem(e81, i);
      9: setmenuitem(e91, i);
      10: setmenuitem(e101, i);
    end;
end;

procedure TForm1.SetLang1;
var
  section: string;
  inifile:      Tmeminifile;
const
  deftxt = '?';
  blank  = '        ';
  b      = ' ';
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
  ldeg     := rsdegree;
  lmin     := rsminute;
  lsec     := rssecond;
  transmsg := rstranslator;
  Caption  := rstitle;
end;

procedure TForm1.SetLang;
var
  buf: string;
const
  deftxt = '?';
  blank  = '        ';
  b      = ' ';
begin
  wordformat := 0;
    wordformat := strtointdef(rsformat, wordformat);
    ldeg     := rsdegree;
    lmin     := rsminute;
    lsec     := rssecond;
    transmsg := rstranslator;
    Caption  := rstitle;
    helpprefix := rshelp_prefix;
    ToolButton1.Caption := rst_1;
    quitter1.Caption := rst_2;
    ToolButton2.Caption := rst_3;
    label10.Caption := rst_4;
    zoom1.Caption := label10.Caption;
    ToolButton9.Hint:=rst_31+' 1:1';
    toolbutton5.hint := rst_5;
    centre1.Caption := toolbutton5.hint;
    Position.Caption := rst_6;
    Position1.Caption := Position.Caption;
    Ephemerides.Caption := rst_7;
    Button1.Caption := rst_10;
    Button2.Caption := rst_11;
    Label9.Caption := rsm_51;
    Label6.Caption := rsm_50;
    Groupbox1.Caption := rst_14;
    toolbutton8.Caption := rst_15;
    aide2.Caption := toolbutton8.Caption;
    OptFeatures1.Caption:=rsCheckForOpti;
    Apropos1.Caption := rst_16;
    Button5.Caption := rst_17;
    Button4.Caption := rst_113;
    ToolButton7.hint := rst_25;
    Image2.Caption := ToolButton7.hint;
    Label14.Caption := rst_32;
    Label8.Caption := rst_33;
    Label11.Caption := rst_34;
    Label12.Caption := rst_35;
    Label16.Caption := rst_36;
    Label13.Caption := rst_37;
    Combobox2.Items.Clear;
    Combobox2.Items.Add(rst_42);
    Combobox2.Items.Add(rst_43);
    Combobox2.Items.Add(rst_44);
    Combobox2.Items.Add(rst_45);
    Combobox2.ItemIndex := 0;
    Terminateur.Caption := rst_51;
    RadioGroup1.Caption := rst_46;
    RadioGroup1.Items[0] := rst_47;
    RadioGroup1.Items[1] := rst_48;
    RadioGroup1.Items[2] := rst_49;
    RadioGroup1.Items[3] := rst_19;
    Reglage.Caption  := rst_55;
    RadioGroup1.ItemIndex := 0;
    Label19.Caption  := RadioGroup1.Items[1];
    Label20.Caption  := RadioGroup1.Items[2];
    CartesduCiel1.Caption := rst_56;
    Outils.Caption   := rst_63;
    label40.Caption  := b + rst_64 + b;
    Button12.Caption := rst_65;
    Button13.Caption := rst_66;
    RadioGroup2.Caption := rst_67;
    RadioGroup2.Items[0]:=rst_72;
    RadioGroup2.Items[1]:=rst_73;
    CheckBox1.Caption := rst_68;
    distance1.Caption := rst_69;
    FullScreen1.Caption:=rsFullScreen;
    ToolButton13.Hint:=rsFullScreen;
    label23.Caption  := b + distance1.Caption + b;
    label24.Caption  := rst_70;
    label25.Caption  := rst_71;
    CheckBox2.Caption := rst_74;
    Enregistrersous1.Caption := rst_78;
    Imprimer1.Caption := rst_79;
    ToolButton10.hint := rst_81;
    Voisinage1.Caption := ToolButton10.hint;
    Selectiondimprimante1.Caption := rst_82;
    selectall1.Caption := rst_84;
    copy1.Caption    := rst_85;
    DecreaseFont1.Caption:=rsDecreaseFont+' Ctrl+-';
    IncreaseFont1.Caption:=rsIncreaseFont+' Ctrl++';
    glossaire1.Caption := rst_98;
    button15.Caption := rst_114;
    Notes.Caption    := rst_115;
    Notes1.Caption   := Notes.Caption;
    label3.Caption   := rst_116;
    Eyepiece1.Caption := rst_109;
    e01.Caption      := rst_117;
    groupbox2.Caption := rst_125;
    Database1.Caption := rst_129;
    Button14.Caption := rst_145;
    label1.Caption   := rst_153;
    label2.Caption   := rst_154;
    label21.Caption  := rst_155;
    Encyclopedia1.Caption := rst_165;
    Snapshot1.Caption := rst_167;
    LibrationButton.hint := rst_175;
    PhaseButton.hint := rst_176;
    ToolButton4.hint := rst_177;
    ToolButton6.hint := rst_178;
    RemoveMark1.Caption := rst_180;
    Label5.Caption:=rsOrbitAltitud;
    Label17.Caption:=rsOrbitInclina;

    ButtonDatabase.hint := Database1.Caption;
    CheckBox8.Caption := rst_182;
    Toolbutton12.hint := rsShowLabels;
    GridButton.Hint:=rsShowGrid;
    ToolButton14.Hint:=rsShowScale;
    CheckBox6.Caption := rst_156;
    CheckBox7.Caption := rst_157;
    label28.Caption := rst_158;
    label26.Caption := rst_159;
    label41.Caption := rst_160;
    Button16.Caption := rst_161;
    Button17.Caption := rst_162;
    Button18.Caption := rst_163;
    Toolbutton3.hint := rst_134;
    Rotation1.Caption := rst_135;
    GroupBox3.Caption := Rotation1.Caption;
    Stop1.Caption := rst_136;
    EastWest1.Caption := rst_137;
    buf := rst_138;
    N10seconde1.Caption := '10' + buf;
    N5seconde1.Caption := '5' + buf;
    N1seconde1.Caption := '1' + buf;
    N05seconde1.Caption := '0.5' + buf;
    N02seconde1.Caption := '0.2' + buf;
    ComboBox4.items.Clear;
    ComboBox4.items.add(N10seconde1.Caption);
    ComboBox4.items.add(N5seconde1.Caption);
    ComboBox4.items.add(N1seconde1.Caption);
    ComboBox4.items.add(N05seconde1.Caption);
    ComboBox4.items.add(N02seconde1.Caption);
    Combobox4.ItemIndex := 2;
    Label4.Caption      := rst_139;
    SpeedButton5.Caption := rst_140;
    SpeedButton6.Caption := rst_5;
    SpeedButton4.Caption := rst_142;
    NewWindowButton.hint := rst_166;
    Button21.Caption:=rsDefault;
    imac1 := rst_30;
    imac2 := rst_8;
    imac3 := rst_9;
    num_bl:=nrsb;
    bldb[1] := rsb_1;
    bldb[2] := rsb_2;
    bldb[3] := rsb_3;
    bldb[4] := rsb_4;
    bldb[5] := rsb_5;
    bldb[6] := rsb_6;
    bldb[7] := rsb_7;
    bldb[8] := rsb_8;
    bldb[9] := rsb_9;
    // Config
    Form2.Setlang;
    f_features.SetLang;
  if gloss <> nil then
    gloss.InitGlossary;
end;

procedure TForm1.InitObservatoire;
var
  u, p: double;
const
  ratio = 0.99664719;
  H0    = 6378140.0;
begin
  p := degtorad(ObsLatitude);
  u := arctan(ratio * tan(p));
  ObsRoSinPhi := ratio * sin(u) + (ObsAltitude / H0) * sin(p);
  ObsRoCosPhi := cos(u) + (ObsAltitude / H0) * cos(p);
  ObsRefractionCor := 1;
end;

function TForm1.GetTimeZone(sdt: Tdatetime): double;
begin
    tz.Date := sdt;
    Result  := tz.SecondsOffset / 3600;
end;

function  TForm1.GetJDTimeZone(jdt: double): double;
begin
    tz.JD := jdt;
    Result  := tz.SecondsOffset / 3600;
end;

// image are now processed by Photlun but we keep the configuration also here for convenience
procedure TForm1.AddImagesDir(dir, nom, cpy, rot: string);
var
  i:  integer;
  ok: boolean;
begin
  ok := True;
  for i := 0 to maximgdir - 1 do
    if (imgdir[i, 2] = nom) or (imgdir[i, 0] = dir) then
      ok := False;
  if ok then
  begin
    Inc(maximgdir);
    setlength(imgdir, maximgdir);
    imgdir[maximgdir - 1, 0] := dir;
    imgdir[maximgdir - 1, 2] := nom;
    imgdir[maximgdir - 1, 1] := cpy;
  end;
end;

procedure TForm1.InitImages;
begin
  if maximgdir = 0 then
  begin
    maximgdir := 3;
    setlength(imgdir, maximgdir);
    imgdir[0, 0] := slash(appdir) + 'Lopam';
    imgdir[0, 2] := 'LOPAM';
    imgdir[0, 1] := '';
    imgdir[1, 0] := slash(appdir) + 'Apollo';
    imgdir[1, 2] := 'Apollo';
    imgdir[1, 1] := '';
    imgdir[2, 0] := slash(appdir) + 'My Images';
    imgdir[2, 2] := 'My Images';
    imgdir[2, 1] := '';
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
AddImagesDir(slash(appdir) + 'Kaguya', 'Kaguya', 'provided by JAXA/SELENE','0');
end;

procedure TForm1.Readdefault;
var
  inif:    TMemIniFile;
  section: string;
  i, j:    integer;
  smooth:  integer;
  labf: TFont;
begin
  database[1]    := 'Nearside_Named_ufr.csv';
  usedatabase[1] := True;
  usedatabase[2] := True;
  usedatabase[3] := True;
  usedatabase[4] := True;
  usedatabase[5] := True;
  usedatabase[6] := True;
  usedatabase[7] := True;
  for i := 8 to maxdbn do
    usedatabase[i] := False;
  timezone := 0;
  Obslatitude := 48.86;
  Obslongitude := -2.33;
  ObsCountry:='FR';
  ObsTZ    := 'Europe/Paris';
  Obsaltitude := 0;
  phaseeffect := True;
  librationeffect := True;
  geocentric := False;
  showoverlay := False;
  wheelstep := 1.05;
  marklabelcolor := clYellow;
  markcolor := clRed;
  autolabelcolor := clWhite;
  labelcenter := True;
  shortdesc:=false;
  minilabel := True;
  showlabel := True;
  showmark := True;
  currentselection := '';
  showlibrationmark := False;
  lockmove := False;
  LabelDensity := 400;
  gridspacing:=15;
  marksize := 5;
  saveimagesize := 0;
  saveimagewhite := False;
  lastima  := 0;
  currentphase := -999;
  fov      := 45;
  CameraOrientation := 0;
  PoleOrientation := 0;
  FollowNorth := False;
  flipx    := 1;
  LeftMargin := 10;
  PrintTextWidth := 700;
  PrintChart := True;
  PrintEph := True;
  PrintDesc := True;
  LopamDirect := False;
  ruklprefix := 'C:\rukl\';
  ruklsuffix := '_large.jpg';
  externalimage := False;
  externalimagepath := 'mspaint.exe';
  wantbump := false;
  eyepiecename[1] := 'SCT 8" + Plossl 10mm';
  eyepiecefield[1] := 15;
  rotdirection := -1;
  rotstep  := 1;
  smooth   := 180;
  inif := Tmeminifile.Create(ConfigFile);
  firstuse := (not inif.SectionExists('default'));
  with inif do
  begin
    section     := 'images';
    LopamDirect := ReadBool(section, 'LopamDirect', LopamDirect);
    externalimage := ReadBool(section, 'ExternalSoftware', externalimage);
    externalimagepath := ReadString(section, 'ExternalSoftwarePath', externalimagepath);
    maximgdir   := ReadInteger(section, 'NumDir', 0);
    saveimagesize := ReadInteger(section, 'SaveImageSize', saveimagesize);
    saveimagewhite := ReadBool(section, 'SaveImageWhite', saveimagewhite);
    setlength(imgdir, maximgdir);
    for i := 1 to maximgdir do
    begin
      imgdir[i - 1, 0] := ReadString(section, 'dir' + IntToStr(i), '');
      imgdir[i - 1, 2] := ReadString(section, 'name' + IntToStr(i), imgdir[i - 1, 0]);
      imgdir[i - 1, 1] := '';
    end;
    ruklprefix := ReadString(section, 'ruklprefix', ruklprefix);
    ruklsuffix := ReadString(section, 'ruklsuffix', ruklsuffix);
    section    := 'Print';
    LeftMargin := ReadInteger(section, 'LeftMargin', LeftMargin);
    PrintTextWidth := ReadInteger(section, 'PrintTextWidth', PrintTextWidth);
    PrintChart := ReadBool(section, 'PrintChart', PrintChart);
    PrintEph   := ReadBool(section, 'PrintEph', PrintEph);
    PrintDesc  := ReadBool(section, 'PrintDesc', PrintDesc);
    section    := 'default';
    pofile     := ReadString(section, 'lang_po_file', '');
    UseComputerTime := ReadBool(section, 'UseComputerTime', UseComputerTime);
    compresstexture := ReadBool(section, 'compresstexture', compresstexture);
    antialias := ReadBool(section, 'antialias', antialias);
    ForceBumpMapSize := ReadInteger(section, 'ForceBumpMapSize', ForceBumpMapSize);
    Obslatitude  := ReadFloat(section, 'Obslatitude', Obslatitude);
    Obslongitude := ReadFloat(section, 'Obslongitude', Obslongitude);
    ObsCountry  := ReadString(section, 'ObsCountry', ObsCountry);
    ObsTZ := ReadString(section, 'ObsTZ', ObsTZ);
    tz.TimeZoneFile := ZoneDir + StringReplace(ObsTZ, '/', PathDelim, [rfReplaceAll]);
    if UseComputerTime then
      timezone := gettimezone(now)
    else
    begin
      CurrentJD := ReadFloat(section, 'CurrentJD', CurrentJD);
      dt_ut     := ReadFloat(section, 'dt_ut', dt_ut);
      timezone := GetJDTimeZone(CurrentJD);
    end;
    cameraorientation := ReadFloat(section, 'CameraOrientation', CameraOrientation);
    phaseeffect  := ReadBool(section, 'PhaseEffect', phaseeffect);
    wantbump  := ReadBool(section, 'BumpMap', wantbump);
    BumpMethod:= Readinteger(section,'BumpMethod',0);
    BumpMipmap:= ReadBool(section,'BumpMipmap',true);
    librationeffect := ReadBool(section, 'LibrationEffect', librationeffect);
    ShowLabel    := ReadBool(section, 'ShowLabel', ShowLabel);
    ShowMark     := ReadBool(section, 'ShowMark', ShowMark);
    ShowLibrationMark := ReadBool(section, 'ShowLibrationMark', ShowLibrationMark);
    MarkLabelColor   := ReadInteger(section, 'LabelColor', MarkLabelColor);
    MarkColor    := ReadInteger(section, 'MarkColor', MarkColor);
    AutolabelColor := ReadInteger(section, 'AutolabelColor', AutolabelColor);
    gridspacing := ReadInteger(section, 'GridSpacing', gridspacing);
    LabelDensity := ReadInteger(section, 'LabelDensity', LabelDensity);
    marksize     := ReadInteger(section, 'MarkSize', marksize);
    labelcenter  := ReadBool(section, 'LabelCenter', labelcenter);
    minilabel    := ReadBool(section, 'MiniLabel', minilabel);
    FollowNorth  := ReadBool(section, 'FollowNorth', FollowNorth);
    shortdesc  := ReadBool(section, 'shortdesc', shortdesc);
    CheckBox2.Checked := ReadBool(section, 'Mirror', False);
    GridButton.Down:= ReadBool(section, 'Grid', False);
    PoleOrientation := ReadFloat(section, 'PoleOrientation', PoleOrientation);
    ToolsWidth:=ReadInteger(section, 'ToolsWidth', ToolsWidth);
    if ToolsWidth<100 then ToolsWidth:=100;
    PageControl1.Width:=ToolsWidth;
    i := ReadInteger(section, 'Top', -9999);
    if i > -1000 then
    begin
      if (i >= -10) and (i < screen.Height - 20) then
        Top := i
      else
        Top := 0;
    end;
    i := ReadInteger(section, 'Left', -9999);
    if i > -1000 then
    begin
      if (i >= -10) and (i < screen.Width - 20) then
        Left := i
      else
        Left := 0;
      i := screen.Height - 50;
      i   := minintvalue([i, ReadInteger(section, 'Height', i)]);
      if (i >= 20) then
        Height := i;
      i := screen.Width - 5;
      i := minintvalue([i, ReadInteger(section, 'Width', i)]);
      if (i >= 20) then
        Width := i;
    end;
    if ReadBool(section, 'Maximized', False) then
      windowstate := wsMaximized;
    for j:=0 to 3 do
      texturefiles[j] := ReadString(section, 'texturefile' + IntToStr(j), texturefiles[j]);

    labf:=TFont.Create;
    labf.Assign(moon1.LabelFont);
    labf.Name := ReadString(section, 'LabelFontName', labf.Name);
    labf.Size := ReadInteger(section, 'LabelFontSize', labf.Size);
    labf.Style:=[];
    if ReadBool(section, 'LabelFontBold', false) then labf.Style:=labf.Style+[fsBold];
    if ReadBool(section, 'LabelFontItalic', false) then labf.Style:=labf.Style+[fsItalic];
    moon1.LabelFont:=labf;
    labf.Free;
    Desc1.DefaultFontSize:=ReadInteger(section, 'DescFontSize', Desc1.DefaultFontSize);
    for j := 1 to 10 do
    begin
      eyepiecename[j]     := ReadString(section, 'eyepiecename' + IntToStr(j), eyepiecename[j]);
      eyepiecefield[j]    := ReadInteger(section, 'eyepiecefield' + IntToStr(j), eyepiecefield[j]);
      eyepiecemirror[j]   := ReadInteger(section, 'eyepiecemirror' + IntToStr(j), eyepiecemirror[j]);
      eyepiecerotation[j] := ReadInteger(section, 'eyepiecerotation' + IntToStr(j), eyepiecerotation[j]);
    end;
    useDBN := ReadInteger(section, 'useDBN', useDBN);
    for i := 1 to useDBN do
      usedatabase[i] := ReadBool(section, 'UseDatabase' + IntToStr(i), usedatabase[i]);
    overlayname := ReadString(section, 'overlayname', 'Colors natural.jpg');
    overlaytr  := ReadFloat(section, 'overlaytr', 0);
    showoverlay := ReadBool(section, 'showoverlay', showoverlay);
    Geocentric  := ReadBool(section, 'Geocentric', Geocentric);
    moon1.AmbientColor := ReadInteger(section, 'AmbientLight', moon1.AmbientColor);
    moon1.DiffuseColor := ReadInteger(section, 'DiffuseLight', moon1.DiffuseColor);
    moon1.SpecularColor :=ReadInteger(section, 'SpecularLight', moon1.SpecularColor);
    smooth := ReadInteger(section, 'Smooth', smooth);
    i := ReadInteger(section, 'ListCount', 0);
    if i > 0 then
      for j := 0 to i do
      begin
        combobox1.Items.add(ReadString(section, 'List_' + IntToStr(j), ''));
      end;
  end;
  inif.Free;
  chdir(appdir);
  InitObservatoire;
  InitImages;
  moon1.GLSphereMoon.Slices := smooth;
  moon1.GLSphereMoon.Stacks := smooth div 2;
end;

procedure TForm1.SaveDefault;
var
  inif: TMemIniFile;
  i:    integer;
  section: string;
begin
  inif := Tmeminifile.Create(ConfigFile);
  try
    section := 'default';
    with inif do
    begin
      if not (ValueExists(section, 'Install_Dir')) then
        WriteString(section, 'Install_Dir', appdir);
      WriteString(section, 'lang_po_file', Language);
      WriteBool(section, 'LibrationEffect', librationeffect);
      WriteBool(section, 'compresstexture', compresstexture);
      WriteBool(section, 'antialias', antialias);
      WriteInteger(section, 'ForceBumpMapSize', ForceBumpMapSize);
      WriteFloat(section, 'CameraOrientation', CameraOrientation);
      WriteInteger(section, 'useDBN', useDBN);
      for i := 1 to useDBN do
        WriteBool(section, 'UseDatabase' + IntToStr(i), usedatabase[i]);
      WriteString(section, 'overlayname', overlayname);
      WriteFloat(section, 'overlaytr', overlaytr);
      WriteBool(section, 'showoverlay', showoverlay);
      for i := 0 to 3 do
        WriteString(section, 'texturefile' + IntToStr(i), texturefiles[i]);
      WriteBool(section, 'Geocentric', Geocentric);
      WriteString(section, 'telescope', Combobox5.Text);
      WriteFloat(section, 'Obslatitude', Obslatitude);
      WriteFloat(section, 'Obslongitude', Obslongitude);
      WriteString(section, 'ObsCountry', ObsCountry);
      WriteString(section, 'ObsTZ', ObsTZ);
      WriteBool(section, 'UseComputerTime', UseComputerTime);
      WriteFloat(section, 'CurrentJD', CurrentJD);
      WriteFloat(section, 'dt_ut', dt_ut);
      WriteBool(section, 'PhaseEffect', phaseeffect);
      WriteBool(section, 'BumpMap', wantbump or moon1.Bumpmap);
      WriteInteger(section,'BumpMethod',ord(moon1.BumpMethod));
      WriteBool(section,'BumpMipmap',moon1.BumpMipmap);
      WriteBool(section, 'ShowLabel', ShowLabel);
      WriteBool(section, 'ShowMark', ShowMark);
      WriteBool(section, 'ShowLibrationMark', ShowLibrationMark);
      WriteInteger(section, 'LabelColor', MarkLabelColor);
      WriteInteger(section, 'MarkColor', MarkColor);
      WriteInteger(section, 'AutolabelColor', AutolabelColor);
      WriteInteger(section, 'LabelDensity', LabelDensity);
      WriteInteger(section, 'GridSpacing', gridspacing);
      WriteInteger(section, 'MarkSize', marksize);
      WriteBool(section, 'LabelCenter', labelcenter);
      WriteBool(section, 'MiniLabel', minilabel);
      WriteBool(section, 'shortdesc', shortdesc);
      WriteBool(section, 'FollowNorth', FollowNorth);
      WriteBool(section, 'Mirror', CheckBox2.Checked);
      WriteBool(section, 'Grid', GridButton.Down);
      WriteFloat(section, 'PoleOrientation', PoleOrientation);
      WriteInteger(section, 'ToolsWidth', ToolsWidth);
      WriteInteger(section, 'Top', Top);
      WriteInteger(section, 'Left', Left);
      WriteInteger(section, 'Height', Height);
      WriteInteger(section, 'Width', Width);
      WriteBool(section, 'Maximized', (windowstate = wsMaximized));
      WriteString(section, 'LabelFontName', moon1.LabelFont.Name);
      WriteInteger(section, 'LabelFontSize', moon1.LabelFont.Size);
      WriteBool(section, 'LabelFontBold', fsBold in moon1.LabelFont.Style);
      WriteBool(section, 'LabelFontItalic', fsItalic in moon1.LabelFont.Style);
      WriteInteger(section, 'DescFontSize', Desc1.DefaultFontSize);
      for i := 1 to 10 do
      begin
        WriteString(section, 'eyepiecename' + IntToStr(i), eyepiecename[i]);
        WriteInteger(section, 'eyepiecefield' + IntToStr(i), eyepiecefield[i]);
        WriteInteger(section, 'eyepiecemirror' + IntToStr(i), eyepiecemirror[i]);
        WriteInteger(section, 'eyepiecerotation' + IntToStr(i), eyepiecerotation[i]);
      end;
      WriteInteger(section, 'AmbientLight', moon1.AmbientColor);
      WriteInteger(section, 'DiffuseLight', moon1.DiffuseColor);
      WriteInteger(section, 'SpecularLight', moon1.SpecularColor);
      WriteInteger(section, 'Smooth',   moon1.GLSphereMoon.Slices);
      WriteInteger(section, 'ListCount', combobox1.Items.Count - 1);
      for i := 0 to combobox1.Items.Count - 1 do
      begin
        WriteString(section, 'List_' + IntToStr(i), combobox1.Items.Strings[i]);
      end;
      section := 'images';
      WriteInteger(section, 'NumDir', maximgdir);
      WriteInteger(section, 'SaveImageSize', saveimagesize);
      WriteBool(section, 'SaveImageWhite', saveimagewhite);
      for i := 1 to maximgdir do
      begin
        WriteString(section, 'dir' + IntToStr(i), imgdir[i - 1, 0]);
        WriteString(section, 'name' + IntToStr(i), imgdir[i - 1, 2]);
      end;
      section := 'Print';
      WriteInteger(section, 'LeftMargin', LeftMargin);
      WriteInteger(section, 'PrintTextWidth', PrintTextWidth);
      WriteBool(section, 'PrintChart', PrintChart);
      WriteBool(section, 'PrintEph', PrintEph);
      WriteBool(section, 'PrintDesc', PrintDesc);
      inif.UpdateFile;
    end;
  finally
    inif.Free;
  end;
end;

procedure TForm1.GetLabel(Sender: TObject);
var lmin,lmax,bmin,bmax: single;
    w, wmin, wfact, l1, b1: single;
    miniok:    boolean;
    nom, let:  string;
    j: integer;
begin
// Labels
  if showlabel and (not TelescopeTimer.Enabled) then
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
    dbm.Query('select NAME,LONGIN,LATIN,WIDEKM,WIDEMI,LENGTHKM,LENGTHMI from moon' +
      ' where DBN in (' + sidelist + ')' + ' and LONGIN > ' +
      formatfloat(f2, rad2deg*lmin) + ' and LONGIN < ' + formatfloat(f2, rad2deg*lmax) +
      ' and LATIN > ' + formatfloat(f2, rad2deg*bmin) +
      ' and LATIN < ' + formatfloat(f2, rad2deg*bmax) +
      ' and (WIDEKM=0 or WIDEKM>=' + formatfloat(f2, (wmin * wfact) / 2.5) + ')' +
      ' ;');
    for j := 0 to dbm.RowCount - 1 do
    begin
      l1 := dbm.Results[j].Format[1].AsFloat;
      b1 := dbm.Results[j].Format[2].AsFloat;
      w  := dbm.Results[j].Format[3].AsFloat;
      if w <= 0 then
        w := 1.67 * dbm.Results[j].Format[4].AsFloat;
      if (w > 200) and (abs(l1) > 90) then
        w := 2.5 * w; // moins de grosse formation face cachee
      if w < (wmin * wfact) then
        continue;
      nom := trim(dbm.Results[j][0]);
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
      Tf_moon(Sender).AddLabel(deg2rad*l1,deg2rad*b1,capitalize(nom));
     end;
  end;
end;

procedure TForm1.GetSprite(Sender: TObject);
var lmin,lmax,bmin,bmax: single;
    l1, b1: single;
    j: integer;
begin
// Mark selection
  if (currentselection<>'') and (not TelescopeTimer.Enabled) then
  begin
    // get search boundaries
    Tf_moon(Sender).GetBounds(lmin,lmax,bmin,bmax);
    dbm.Query('select LONGIN,LATIN from moon where ' + currentselection +
      ' and LONGIN > ' + formatfloat(f2, rad2deg*lmin) +
      ' and LONGIN < ' + formatfloat(f2, rad2deg*lmax) +
      ' and LATIN > ' + formatfloat(f2, rad2deg*bmin) +
      ' and LATIN < ' + formatfloat(f2, rad2deg*lmax) +
      ' ORDER BY WIDEKM DESC ' + ' ;');
    for j := 0 to dbm.RowCount - 1 do
    begin
      l1 := dbm.Results[j].Format[0].AsFloat;
      b1 := dbm.Results[j].Format[1].AsFloat;
      Tf_moon(Sender).AddSprite(deg2rad*l1,deg2rad*b1);
    end;
  end;
end;

procedure TForm1.InitDate;
var
  y, m, d, h, n, s, ms: word;
begin
  decodedate(now, y, m, d);
  decodetime(now, h, n, s, ms);
  timezone   := gettimezone(now);
  annee.Value := y;
  mois.Value := m;
  jour.Value := d;
  heure.Value := h;
  minute.Value := n;
  seconde.Value := s;
  updown1.position := y;
  updown2.position := m;
  updown3.position := d;
  updown4.position := h;
  updown5.position := n;
  updown6.position := s;
  dt_ut      := dtminusut(y);
  CurYear    := y;
  CurrentMonth := m;
  CurrentDay := d;
  Currenttime := h + n / 60 + s / 3600;
  CurrentJD  := jd(CurYear, CurrentMonth, CurrentDay, Currenttime - timezone + DT_UT);
end;

procedure TForm1.SetJDDate;
var
  y, m, d: integer;
  h, n, s, ms: word;
  hh: double;
begin
  djd(CurrentJD - (DT_UT / 24) + 1e-8, y, m, d, hh);
  if hh>23.9999 then hh:=24.0;
  decodetime(hh / 24, h, n, s, ms);
  timezone := GetJDTimeZone(CurrentJD);
  djd(CurrentJD + (timezone / 24) - (DT_UT / 24) + 1e-8, y, m, d, hh);
  decodetime(hh / 24, h, n, s, ms);
  annee.Value  := y;
  mois.Value   := m;
  jour.Value   := d;
  heure.Value  := h;
  minute.Value := n;
  seconde.Value := s;
  updown1.position := y;
  updown2.position := m;
  updown3.position := d;
  updown4.position := h;
  updown5.position := n;
  updown6.position := s;
  CurYear      := y;
  CurrentMonth := m;
  CurrentDay   := d;
  Currenttime  := h + n / 60 + s / 3600;
end;

procedure TForm1.SetDate(param: string);
var
  buf: string;
  p, yy, mm, dd, h, m, s: integer;
begin
  buf := trim(param);
  p   := pos('-', buf);
  if p = 0 then
    exit;
  yy  := StrToInt(trim(copy(buf, 1, p - 1)));
  buf := copy(buf, p + 1, 999);
  p   := pos('-', buf);
  if p = 0 then
    exit;
  mm  := StrToInt(trim(copy(buf, 1, p - 1)));
  buf := copy(buf, p + 1, 999);
  p   := pos('T', buf);
  if p = 0 then
    exit;
  dd  := StrToInt(trim(copy(buf, 1, p - 1)));
  buf := copy(buf, p + 1, 999);
  p   := pos(':', buf);
  if p = 0 then
    exit;
  h   := StrToInt(trim(copy(buf, 1, p - 1)));
  buf := copy(buf, p + 1, 999);
  p   := pos(':', buf);
  if p = 0 then
    exit;
  m     := StrToInt(trim(copy(buf, 1, p - 1)));
  buf   := copy(buf, p + 1, 999);
  s     := StrToInt(trim(buf));
  CurYear := yy;
  CurrentMonth := mm;
  CurrentDay := dd;
  CurrentTime := h + m / 60 + s / 3600;
  dt_ut := dtminusut(CurYear);
  CurrentJD := jd(CurYear, CurrentMonth, CurrentDay, Currenttime - timezone + DT_UT);
  SetJDDate;
end;

procedure TForm1.SetObs(param: string);
var
  buf: string;
  p:   integer;
  s, la, lo: double;
begin
  p := pos('LAT:', param);
  if p = 0 then
    exit;
  buf := copy(param, p + 4, 999);
  p   := pos('d', buf);
  la  := strtofloat(copy(buf, 1, p - 1));
  s   := sgn(la);
  buf := copy(buf, p + 1, 999);
  p   := pos('m', buf);
  la  := la + s * strtofloat(copy(buf, 1, p - 1)) / 60;
  buf := copy(buf, p + 1, 999);
  p   := pos('s', buf);
  la  := la + s * strtofloat(copy(buf, 1, p - 1)) / 3600;
  p   := pos('LON:', param);
  if p = 0 then
    exit;
  buf := copy(param, p + 4, 999);
  p   := pos('d', buf);
  lo  := strtofloat(copy(buf, 1, p - 1));
  s   := sgn(lo);
  buf := copy(buf, p + 1, 999);
  p   := pos('m', buf);
  lo  := lo + s * strtofloat(copy(buf, 1, p - 1)) / 60;
  buf := copy(buf, p + 1, 999);
  p   := pos('s', buf);
  lo  := lo + s * strtofloat(copy(buf, 1, p - 1)) / 3600;
  p   := pos('TZ:', param);
  if p > 0 then
  begin
    buf := copy(param, p + 3, 999);
    p   := pos('h', buf);
    if p = 0 then
      Timezone := strtofloat(trim(buf))
    else
      Timezone := strtofloat(copy(buf, 1, p - 1));
  end;
  Obslatitude  := la;
  Obslongitude := lo;
  InitObservatoire;
  CurrentJD := jd(CurYear, CurrentMonth, CurrentDay, Currenttime - timezone + DT_UT);
end;

procedure TForm1.ReadParam(first:boolean=true);
var
  i:    integer;
  buf:  string;
  x, y: double;
begin
  try
    i := 0;
    while i <= param.Count - 1 do
    begin
      if param[i] = '-o' then
      begin    // observatory
        Inc(i);
        buf := trim(StringReplace(param[i], '"', '', [rfReplaceAll]));
        setobs(buf);
      end
      else if param[i] = '-d' then
      begin    // date
        Inc(i);
        buf := trim(StringReplace(param[i], '"', '', [rfReplaceAll]));
        setdate(buf);
      end
      else if param[i] = '-c' then
      begin   // center coordinates
        Inc(i);
        buf := trim(StringReplace(param[i], '"', '', [rfReplaceAll]));
        x   := strtofloat(buf);
        Inc(i);
        buf := trim(StringReplace(param[i], '"', '', [rfReplaceAll]));
        y   := strtofloat(buf);
        moon1.CenterAt(deg2rad*x, deg2rad*y);
      end
      else if param[i] = '-z' then
      begin   // zoom , to use instead of obsolete -f
        Inc(i);
        buf := trim(StringReplace(param[i], '"', '', [rfReplaceAll]));
        x   := strtofloat(buf);
        moon1.zoom:=x;
      end
      else if param[i] = '-n' then
      begin  // center object
        Inc(i);
        buf := trim(StringReplace(param[i], '"', '', [rfReplaceAll]));
        Firstsearch := True;
        SearchName(buf, True);
      end
      else if param[i] = '-s' then
      begin   // mark selection
        Inc(i);
        currentselection := trim(StringReplace(param[i], '"', '', [rfReplaceAll]));
        if currentselection = '*' then
          currentselection := '';
      end
      else if param[i] = '-3d' then
      begin     // full globe
        ToolButton3.Down := True;
        ToolButton3Click(nil);
      end
      else if param[i] = '-r' then
      begin   // full globe at give rotation
        Inc(i);
        buf := trim(StringReplace(param[i], '"', '', [rfReplaceAll]));
        x   := strtofloat(buf);
        if x <> 0 then
        begin
          ToolButton3.Down := True;
          ToolButton3Click(nil);
          moon1.CenterAt(deg2rad*x, 0);
        end;
      end
      else if (param[i]='-nd')and first then
      begin
        CanCloseDatlun:=false;  // when started by datlun do not close datlun on exit!
      end
      else if (param[i]='-np')and first then
      begin
        CanClosePhotlun:=false;  // when started by photlun do not close photlun on exit!
      end
      else if (param[i]='-ns')and first then
      begin
        CanCloseCDC:=false;  // when started by skychart do not close skychart on exit!
      end
      else if param[i]='-quit' then
      begin
        Close;  // close current instance
      end
      else if param[i] = '--' then
      begin   // last parameter
        break;
      end;
      Inc(i);
    end;
  except
  end;
end;

procedure TForm1.GetAppDir;
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

procedure TForm1.UpdTerminateur(range:double=12);
var
  interest, diam, i: integer;
  l1, l2: double;
  buf:    string;
begin
  listbox1.Clear;
  if tphase < 180 then
  begin
    l1 := tphase - 90 + librl;
    l2 := l1 + range;
  end
  else
  begin
    l2 := tphase - 90 + librl - 180;
    l1 := l2 - range;
  end;
  currentphase := tphase;
  diam     := strtointdef(combobox3.Text, 5);
  interest := combobox2.ItemIndex + 1;
  try
    listbox1.Items.BeginUpdate;
    dbm.Query('select NAME,LATIN,INTERESTN,DIAMINST from moon ' +
      ' where longin>' + formatfloat(f2, l1) +
      ' and longin<' + formatfloat(f2, l2) + ' and DBN in (' + sidelist + ')' +
      ' and INTERESTN >=' + IntToStr(interest) +
      ' and DIAMINST <=' + IntToStr(diam));
    for i := 0 to dbm.rowcount - 1 do
    begin
      case RadioGroup1.ItemIndex of
        0: listbox1.Items.Add(dbm.Results[i][0]);
        1: listbox1.Items.Add(IntToStr(5 - dbm.Results[i].format[2].AsInteger) +
            ' ' + dbm.Results[i][0]);
        2: listbox1.Items.Add(formatfloat('000', dbm.Results[i].format[3].AsFloat) +
            ' ' + dbm.Results[i][0]);
        3:
        begin
          buf := demtostr(dbm.Results[i].format[1].AsFloat) + ' ' + dbm.Results[i][0];
          listbox1.Items.Add(buf);
        end;
      end;
    end;
  finally
    listbox1.Items.EndUpdate;
    if listbox1.Items.Count = 0 then
      listbox1.Items.Add(rsm_27);
  end;
end;

procedure TForm1.ListObject(delta: double);
var
  l, b, deltal: double;
  i: integer;
begin
  l      := currentl;
  b      := currentb;
  deltal := delta / cos(deg2rad * b);
  with f_craterlist do
  begin
    f_craterlist.Caption := form1.ToolButton10.hint;
    craterlst.Clear;
    dbm.query('select NAME from moon ' + ' where DBN in (' + sidelist + ')' +
      ' and LONGIN > ' + formatfloat(f2, l - deltal) +
      ' and LONGIN < ' + formatfloat(f2, l + deltal) +
      ' and LATIN > ' + formatfloat(f2, b - delta) +
      ' and LATIN < ' + formatfloat(f2, b + delta) + ' ;');
    for i := 0 to dbm.RowCount - 1 do
    begin
      craterlst.items.add(dbm.Results[i][0]);
    end;
    if not f_craterlist.Visible then
      FormPos(f_craterlist, PageControl1.Clienttoscreen(point(0, 0)).x,
        PageControl1.Clienttoscreen(point(0, 0)).y);
    f_craterlist.Show;
  end;
end;

function Tform1.SearchAtPos(l, b: double): boolean;
var
  mindist, d, l1, b1, deltab, deltal: double;
  rec, i: integer;
begin
  mindist := 9999;
  Result  := False;
  rec     := 0;
  deltab  := 5;
  deltal  := deltab / cos(deg2rad * b);
  dbm.query('select ID,LONGIN,LATIN from moon ' + ' where DBN in (' + sidelist + ')' +
    ' and LONGIN > ' + formatfloat(f2, l - deltal) +
    ' and LONGIN < ' + formatfloat(f2, l + deltal) +
    ' and LATIN > ' + formatfloat(f2, b - deltab) +
    ' and LATIN < ' + formatfloat(f2, b + deltab) + ' ;');
  for i := 0 to dbm.RowCount - 1 do
  begin
    l1 := dbm.Results[i].format[1].AsFloat;
    b1 := dbm.Results[i].format[2].AsFloat;
    d  := angulardistance(deg2rad*l, deg2rad*b, deg2rad*l1, deg2rad*b1);
    if d < mindist then
    begin
      Result  := True;
      mindist := d;
      rec     := dbm.Results[i].Format[0].AsInteger;
    end;
  end;
  if Result then
  begin
    currentid := IntToStr(rec);
    dbm.query('select * from moon where ID=' + currentid + ';');
    Result := dbm.RowCount > 0;
  end;
end;

procedure TForm1.AddToList(buf: string);
var
  i: integer;
begin
  i := combobox1.Items.IndexOf(buf);
  if (i < 0) and (combobox1.Items.Count >= combobox1.DropDownCount) then
    i := combobox1.DropDownCount - 1;
  if i >= 0 then
    combobox1.Items.Delete(i);
  combobox1.Items.Insert(0, buf);
  combobox1.ItemIndex := 0;
end;

procedure TForm1.GetDetail(row: TResultRow; memo: Tmemo);
const
  b = ' ';
var
  nom, carte, buf, buf2, txt: string;
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
        for k := 1 to num_bl do
        begin
          if result=bldb[k] then result:='';
        end;
    end
    else
      if result='' then result:=' ';
  end;
begin
  nom := GetField('NAME');
  memo.Lines.Add(nom);
  i := row.ByField['DBN'].AsInteger;
  if i > 9 then
  begin
    buf := rsm_75 + b + IntToStr(i) + b;
    for j := 0 to form2.CheckListBox1.Count - 1 do
      if (form2.CheckListBox1.Items.Objects[j] as TDBinfo).dbnum = i then
        buf := buf + form2.CheckListBox1.Items[j];
    memo.Lines.Add(buf);
  end;
  memo.Lines.Add(rsm_56 + b + GetField('TYPE'));
  if (GetField('PERIOD'))>'' then
     memo.Lines.Add(rsm_49 + b + GetField('PERIOD'));

  //Taille
  if (GetField('LENGTHKM')>'')or(GetField('WIDEKM')>'')or(GetField('LENGTHMI')>'')or(GetField('WIDEMI')>'') then
     memo.Lines.Add(rsm_57); //Taille
  if (GetField('LENGTHKM')>'')or(GetField('WIDEKM')>'')or(GetField('LENGTHMI')>'')or(GetField('WIDEMI')>'') then
     memo.Lines.Add(rsm_17 + b + GetField('LENGTHKM') + 'x' +
                    GetField('WIDEKM') + rsm_18 + b + '/' + b + GetField('LENGTHMI') +
                    'x' + GetField('WIDEMI') + rsm_19);
  buf  := GetField('HEIGHTM');
  buf2 := GetField('HEIGHTFE');
  if buf<>buf2 then
  begin
    txt := rsm_20 + b;
    val(buf, dummy, i);
    if i = 0 then
      txt := txt + buf + rsm_21 + b + '/' + b;
    val(buf2, dummy, i);
    if i = 0 then
      txt := txt + buf2 + rsm_22;
    memo.Lines.Add(txt);
  end;
  if (GetField('RAPPORT'))>'' then
     memo.Lines.Add(rsm_23 + b + GetField('RAPPORT'));

  //Description
  if (GetField('GENERAL')>'')or(GetField('SLOPES')>'')or(GetField('WALLS')>'')or(GetField('FLOOR')>'') then
     memo.Lines.Add(rsm_58); //Description
  if GetField('GENERAL') > '' then
    memo.Lines.Add(GetField('GENERAL'));
  if GetField('SLOPES') > '' then
    memo.Lines.Add(GetField('SLOPES'));
  if GetField('WALLS') > '' then
    memo.Lines.Add(GetField('WALLS'));
  if GetField('FLOOR') > '' then
    memo.Lines.Add(GetField('FLOOR'));

  //Observation
  if (GetField('INTERESTC')>'')or(GetField('MOONDAYS')>'')or(GetField('MOONDAYM')>'')or(GetField('PRINSTRU')>'') then
     memo.Lines.Add(rsm_59); //Observation
  if GetField('INTERESTC') > '' then
     memo.Lines.Add(rsm_24 + b + GetField('INTERESTC'));
  buf  := GetField('MOONDAYS');
  buf2 := GetField('MOONDAYM');
  if (buf+buf2)>'' then begin
    if buf = buf2 then
      txt := rsm_25 + b + buf
    else
      txt := rsm_25 + b + buf + b + rsm_26 + b + buf2;
    memo.Lines.Add(txt);
  end;
  if GetField('PRINSTRU') > '' then
     memo.Lines.Add(rsm_28 + b + GetField('PRINSTRU'));

  if (GetField('LONGIC')>'')or(GetField('LATIC')>'')or(GetField('QUADRANT')>'')or(GetField('AREA')>'') then
     memo.Lines.Add(rsm_60); //Position
  if GetField('LONGIC') > '' then
     memo.Lines.Add(rsm_10 + b + GetField('LONGIC'));
  if GetField('LATIC') > '' then
     memo.Lines.Add(rsm_11 + b + GetField('LATIC'));
  if GetField('QUADRANT') > '' then
     memo.Lines.Add(rsm_12 + b + GetField('QUADRANT'));
  if GetField('AREA') > '' then
     memo.Lines.Add(rsm_13 + b + GetField('AREA'));

  //Atlas
  dblox.Gofirst;
  ok := dblox.MatchData('NAME', '=', nom);
  if not ok then
    ok := dblox.SeekData('NAME', '=', nom);
  if ok or (GetField('RUKL')>'')or(GetField('RUKLC')>'')or(GetField('VISCARDY')>'')or(GetField('HATFIELD')>'')or(GetField('WESTFALL')>'')or(GetField('WOOD')>'') then
     memo.Lines.Add(rsm_61); //Atlas
if (GetField('RUKL')>'')or(GetField('RUKLC')>'') then
     memo.Lines.Add(rsm_14 + b + GetField('RUKL') + ' ' + GetField('RUKLC'));
  buf := GetField('VISCARDY');
  if trim(buf) > '' then
    memo.Lines.Add(rsm_15 + b + buf);
  buf := GetField('HATFIELD');
  if trim(buf) > '' then
    memo.Lines.Add(rsm_16 + b + buf);
  buf := GetField('WESTFALL');
  if trim(buf) > '' then
    memo.Lines.Add(rsm_66 + b + buf);
  buf := GetField('WOOD');
  if trim(buf) > '' then
    memo.Lines.Add(rsm_72 + b + buf);
  if ok then
  begin
    buf := rsm_65;
    while ok do
    begin
      carte := dblox.GetDATA('PLATE');
      buf   := buf + b + carte;
      ok    := dblox.SeekData('NAME', '=', nom);
    end;
    memo.Lines.Add(buf);
  end;

  //Origine
  memo.Lines.Add(rsm_62); //Origine
  if GetField('NAMEDETAIL') > '' then
     memo.Lines.Add(rsm_63 + b + GetField('NAMEDETAIL'));
  if (trim(GetField('WORK') + GetField('NATIONLITY')) > '') and
    (trim(GetField('CENTURYC') + GetField('COUNTRY')) > '') then
  begin
    case wordformat of
      0: memo.Lines.Add(GetField('CENTURYC') + b +
          GetField('NATIONLITY') + b + GetField('WORK') + b +
          rsm_2 + b + GetField('COUNTRY'));
      1: memo.Lines.Add(GetField('WORK') + b +
          GetField('NATIONLITY') + b + rsm_1 + b + GetField('CENTURYC') +
          b + rsm_2 + b + GetField('COUNTRY'));
      2: memo.Lines.Add(GetField('NATIONLITY') + b +
          GetField('WORK') + b + GetField('CENTURYC') + b +
          rsm_2 + b + GetField('COUNTRY'));
    end;
    if (GetField('BIRTHPLACE')>'')or((GetField('BIRTHDATE')>'')) then
       memo.Lines.Add(rsm_3 + b + GetField('BIRTHPLACE') + b + rsm_4 +
            b + GetField('BIRTHDATE'));
    if (GetField('DEATHPLACE')>'')or((GetField('DEATHDATE')>'')) then
       memo.Lines.Add(rsm_5 + b + GetField('DEATHPLACE') + b + rsm_4 +
            b + GetField('DEATHDATE'));
  end;
  if GetField('FACTS')>'' then
    memo.Lines.Add(rsm_64 + b + GetField('FACTS'));
  if GetField('NAMEORIGIN')>'' then
     memo.Lines.Add(rsm_6 + b + GetField('NAMEORIGIN'));
  if GetField('LANGRENUS')>'' then
     memo.Lines.Add(rsm_7 + b + GetField('LANGRENUS'));
  if GetField('HEVELIUS')>'' then
     memo.Lines.Add(rsm_8 + b + GetField('HEVELIUS'));
  if GetField('RICCIOLI')>'' then
     memo.Lines.Add(rsm_9 + b + GetField('RICCIOLI'));
end;

procedure TForm1.GetHTMLDetail(row: TResultRow; var txt: string);
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
        for k := 1 to num_bl do
        begin
          if result=bldb[k] then result:='';
        end;
    end
    else
      if result='' then result:=' ';
  end;
begin
  txt := '<html> <body bgcolor="white">';
  nom := GetField('NAME');
  dblox.Gofirst;
  ok := dblox.MatchData('NAME', '=', nom);
  if not ok then
    ok := dblox.SeekData('NAME', '=', nom);
  txt  := txt + t1 + nom + t1end + '<br>';
  i    := row.ByField['DBN'].AsInteger;
  if i > 9 then
  begin
    txt := txt + t3 + 'From Database:' + t3end + b + IntToStr(i) + b;
    for j := 0 to form2.CheckListBox1.Count - 1 do
      if (form2.CheckListBox1.Items.Objects[j] as TDBinfo).dbnum = i then
        txt := txt + form2.CheckListBox1.Items[j] + '<br>';
  end;
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
  url   := ruklprefix + img + ruklsuffix;
  if fileexists(url) then
    url := ' <A HREF="file://' + url + '">' + carte + '</A>'
  else
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
  if ok then
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
  end;
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
  if copy(GetField('PROFIL'),1,2)='A_' then begin
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
    Label7.Caption := '';
  statusbar1.Panels[0].Text := rsm_10 + GetField('LONGIN');
  statusbar1.Panels[1].Text := rsm_11 + GetField('LATIN');
  Addtolist(nom);
end;

procedure Tform1.GetDBgrid;
var
  dbcol: integer;
begin
  if dbedited then
    if messagedlg(
      'Attention les modifications de la base de donnee n''ont pas ete enregistree et seront perdue. Continuer quand meme ?',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      exit;
  dbedited := False;
  dbm.query('select * from moon where ID=' + currentid + ';');
  if dbm.RowCount = 0 then
    exit;
  editrow := dbm.Results[0].ByField['ID'].AsInteger;
  stringgrid2.RowCount := dbm.ColCount - 1;    // pas l'id.
  for dbcol := 1 to stringgrid2.RowCount do
  begin
    stringgrid2.Cells[0, dbcol - 1] := dbm.GetField(dbcol);
    stringgrid2.Cells[1, dbcol - 1] := dbm.Results[0][dbcol];
  end;
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  dbcol:  integer;
  cmd, v: string;
begin
  if editrow < 0 then
  begin
    ShowMessage('Pas d''enregistrement selectionne!');
    exit;
  end;
  if (stringgrid2.Cells[1, 0] = '') or (stringgrid2.Cells[1, 1] = '') or
    (stringgrid2.Cells[1, 19] = '') or (stringgrid2.Cells[1, 21] = '') then
  begin
    ShowMessage('DBN, NAME, LONGIN et LATIN obligatoire!');
    exit;
  end;
  if editrow = 0 then
  begin  // insert new record.
    cmd := 'insert into moon values(NULL,';
    for dbcol := 1 to stringgrid2.RowCount do
    begin
      v   := stringgrid2.Cells[1, dbcol - 1];
      v   := stringreplace(v, ',', '.', [rfreplaceall]);
      v   := stringreplace(v, '""', '''', [rfreplaceall]);
      v   := stringreplace(v, '"', '', [rfreplaceall]);
      cmd := cmd + '"' + v + '",';
    end;
    cmd := copy(cmd, 1, length(cmd) - 1) + ');';
    dbm.Query(cmd);
    editrow := dbm.GetLastInsertID;
    dbjournal(extractfilename(dbm.database), 'INSERT DBN=' + stringgrid2.Cells[1, 0] +
      ' NAME=' + stringgrid2.Cells[1, 1] + ' ID=' + IntToStr(editrow));
  end
  else
  begin   // update existing record.
    cmd := 'update moon set ';
    for dbcol := 1 to stringgrid2.RowCount do
    begin
      v   := stringgrid2.Cells[1, dbcol - 1];
      v   := stringreplace(v, ',', '.', [rfreplaceall]);
      v   := stringreplace(v, '""', '''', [rfreplaceall]);
      v   := stringreplace(v, '"', '', [rfreplaceall]);
      cmd := cmd + stringgrid2.Cells[0, dbcol - 1] + '="' + v + '",';
    end;
    cmd := copy(cmd, 1, length(cmd) - 1) + ' where ID=' + IntToStr(editrow) + ';';
    dbm.Query(cmd);
    dbjournal(extractfilename(dbm.database), 'UPDATE DBN=' + stringgrid2.Cells[1, 0] +
      ' NAME=' + stringgrid2.Cells[1, 1] + ' ID=' + IntToStr(editrow));
  end;
  moon1.setmark(0, 0, '');
  moon1.RefreshAll;
  dbedited := False;
end;

procedure TForm1.Button20Click(Sender: TObject);  // delete dans la db!
var
  cmd: string;
begin
  if editrow <= 0 then
  begin
    ShowMessage('Pas d''enregistrement selectionne en base de donnee!');
    exit;
  end;
  if messagedlg('Vraiment supprimer ' + stringgrid2.Cells[1, 1] +
    ' de la base de donnee ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    cmd := 'delete from moon where ID=' + IntToStr(editrow) + ';';
    dbm.Query(cmd);
    dbjournal(extractfilename(dbm.database), 'DELETE DBN=' + stringgrid2.Cells[1, 0] +
      ' NAME=' + stringgrid2.Cells[1, 1] + ' ID=' + IntToStr(editrow));
    dbedited := False;
    editrow  := -1;
    btnEffacer.Click;
    moon1.setmark(0, 0, '');
    moon1.RefreshAll;
  end;
end;

procedure TForm1.btnEffacerClick(Sender: TObject);
// remise a blanc de toute les champs, on fait rien dans la db.
var
  dbcol: integer;
begin
  if dbedited then
    if messagedlg(
      'Attention les modifications de la base de donnee n''ont pas ete enregistree et seront perdue. Continuer quand meme ?',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      exit;
  dbedited := False;
  for dbcol := 1 to stringgrid2.RowCount do
  begin
    stringgrid2.Cells[1, dbcol - 1] := '';
  end;
  editrow := 0;
end;

procedure TForm1.StringGrid2SetEditText(Sender: TObject; ACol, ARow: integer;
  const Value: string);
begin
  if editrow >= 0 then
    dbedited := True;
end;

procedure TForm1.EnregistredistClick(Sender: TObject);
var
  dbcol, i: integer;
  l, b, w:  double;
  ls, bs, ws, ls2, bs2, quadrant, dbn, defaultname: string;
  sel:      TGridRect;
begin
  if trim(edit1.Text) = '' then
    exit;
  ws := words(edit1.Text, '', 1, 1);
  i  := pos(rsm_18, ws);
  if i > 0 then
    ws := copy(ws, 1, i - 1);
  ls   := edit3.Text;
  bs   := edit4.Text;
  w    := strtofloat(ws);
  l    := strtofloat(ls);
  b    := strtofloat(bs);
  if l >= 0 then
    ls2 := formatfloat(f1, abs(l)) + '° Est'
  else
    ls2 := formatfloat(f1, abs(l)) + '° Ouest';
  if b >= 0 then
    bs2 := formatfloat(f1, abs(b)) + '° Nord'
  else
    bs2 := formatfloat(f1, abs(b)) + '° Sud';
  quadrant := words(bs2, '', 2, 1) + '-' + words(ls2, '', 2, 1);
  if abs(l) <= 90 then
    dbn := '2' // indice face visible
  else
    dbn := '4'; // indice face cachee
  defaultname := formatfloat('"+"0000;"-"0000;"+"0000', l * 10) + formatfloat(
    '"+"000;"-"000;"+"000', b * 10);
  for dbcol := 1 to stringgrid2.RowCount do
  begin
    if stringgrid2.Cells[0, dbcol - 1] = 'DBN' then
      stringgrid2.Cells[1, dbcol - 1] := dbn;
    if (stringgrid2.Cells[0, dbcol - 1] = 'NAME') and (stringgrid2.Cells[1, dbcol - 1] = '') then
      stringgrid2.Cells[1, dbcol - 1] := defaultname;
    if stringgrid2.Cells[0, dbcol - 1] = 'LENGTHKM' then
      stringgrid2.Cells[1, dbcol - 1] := ws;
    if stringgrid2.Cells[0, dbcol - 1] = 'WIDEKM' then
      stringgrid2.Cells[1, dbcol - 1] := ws;
    if stringgrid2.Cells[0, dbcol - 1] = 'LENGTHMI' then
      stringgrid2.Cells[1, dbcol - 1] := formatfloat(f1, 0.621 * w);
    if stringgrid2.Cells[0, dbcol - 1] = 'WIDEMI' then
      stringgrid2.Cells[1, dbcol - 1] := formatfloat(f1, 0.621 * w);
    if stringgrid2.Cells[0, dbcol - 1] = 'LONGIN' then
      stringgrid2.Cells[1, dbcol - 1] := ls;
    if stringgrid2.Cells[0, dbcol - 1] = 'LATIN' then
      stringgrid2.Cells[1, dbcol - 1] := bs;
    if stringgrid2.Cells[0, dbcol - 1] = 'LONGIC' then
      stringgrid2.Cells[1, dbcol - 1] := ls2;
    if stringgrid2.Cells[0, dbcol - 1] = 'LATIC' then
      stringgrid2.Cells[1, dbcol - 1] := bs2;
    if stringgrid2.Cells[0, dbcol - 1] = 'QUADRANT' then
      stringgrid2.Cells[1, dbcol - 1] := quadrant;
  end;
  pagecontrol1.ActivePage := dbtab.Caption;
  sel.left   := 1;
  sel.top    := 1;
  sel.right  := 1;
  sel.bottom := 1;
  stringgrid2.Selection := sel;
  stringgrid2.SetFocus;
end;

procedure TForm1.GetNotes(n: string);
begin
  if notesedited then
    if messagedlg(rsm_67, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      UpdNotesClick(nil);
  notes_name.Caption := n;
  memo1.Clear;
  notesok     := False;
  notesedited := False;
  notesrow    := -1;
  dbnotes.GoFirst;
  notesok := dbnotes.MatchData('NAME', '=', trim(uppercase(n)));
  if not notesok then
    notesok := dbnotes.SeekData('NAME', '=', trim(uppercase(n)));
  if notesok then
  begin
    notesrow    := dbnotes.GetPosition;
    memo1.Text  := dbnotes.GetData('NOTES');
    notesedited := False;
  end;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  notesedited := True;
end;

procedure TForm1.UpdNotesClick(Sender: TObject);
var
  nom: string;
begin
  notesedited := False;
  if notesok then
  begin
    dbnotes.Go(notesrow);
    dbnotes.SetData('NOTES', memo1.Text);
  end
  else
  begin
    nom := uppercase(trim(notes_name.Caption));
    if nom = '' then
      exit;
    dbnotes.Gofirst;
    notesok := dbnotes.MatchData('NAME', '>=', nom);
    if not notesok then
      notesok := dbnotes.SeekData('NAME', '>=', nom);
    if not notesok then
      dbnotes.golast;
    if dbnotes.GetData('NAME') <> nom then
      dbnotes.InsertRow(not notesok);
    dbnotes.SetData('NAME', nom);
    dbnotes.SetData('NOTES', memo1.Text);
    notesrow := dbnotes.GetPosition;
    notesok  := True;
  end;
  screen.Cursor := crHourGlass;
  try
    dbnotes.SaveToCSVFile(Slash(DBdir) + 'notes.csv');
  finally
    screen.Cursor := crDefault;
  end;
end;

function TForm1.ImgExists(nom: string): boolean;
var
  f:    Tsearchrec;
  r, i: integer;
  buf, nom2, dir: string;
begin
  nom := trim(nom);
  r   := length(nom) - 1;
  if copy(nom, r, 1) = ' ' then
  begin
    buf := copy(nom, r + 1, 1);
    if ((buf >= 'a') and (buf <= 'z')) or ((buf >= 'A') and (buf <= 'Z')) then
      nom2 := copy(nom, 1, r - 1);
  end
  else
    nom2 := '';
  for i := 0 to maximgdir - 1 do
  begin
    dir := Slash(imgdir[i, 0]);
    r   := findfirst(dir + nom + '.jpg', 0, f);
    findclose(f);
    if r = 0 then
      break;
    r := findfirst(dir + nom + '_*.jpg', 0, f);
    findclose(f);
    if r = 0 then
      break;
    if nom2 <> '' then
    begin
      r := findfirst(dir + nom2 + '.jpg', 0, f);
      findclose(f);
      if r = 0 then
        break;
      r := findfirst(dir + nom2 + '_*.jpg', 0, f);
      findclose(f);
      if r = 0 then
        break;
    end;
  end;
  Result := (r = 0);
end;

function TForm1.SearchName(n: string; center: boolean): boolean;
var
  l, b: double;
  i:   integer;
begin
  Result := False;
  if Firstsearch then
  begin
    dbm.Query('select id,name from moon ' + ' where DBN in (' + sidelist + ')' +
      ' and NAME like "' + trim(uppercase(n)) + '%"' +
      ' order by NAME limit 100;');
    searchlist.Clear;
    for i := 0 to dbm.RowCount - 1 do
    begin
      searchlist.Add(dbm.Results[i].Format[0].AsString);
    end;
    searchpos := -1;
  end;
  Firstsearch := False;
  Inc(searchpos);
  if searchpos < searchlist.Count then
  begin
    dbm.Query('select * from moon where id=' + searchlist[searchpos]);
    if dbm.RowCount = 0 then
      exit;
    l := dbm.Results[0].ByField['LONGIN'].AsFloat;
    b := dbm.Results[0].ByField['LATIN'].AsFloat;
    currentl    := l;
    currentb    := b;
    currentid   := searchlist[searchpos];
    currentname := dbm.Results[0].ByField['NAME'].AsString;
    activemoon.SetMark(deg2rad*currentl, deg2rad*currentb, capitalize(currentname));
    if center then begin
      activemoon.CenterAt(deg2rad*currentl, deg2rad*currentb);
    end;
    //if center then activemoon.CenterMark;
    GetHTMLDetail(dbm.Results[0], Desctxt);
    SetDescText(Desctxt);
    if dbtab.TabVisible then
      GetDBgrid;
    GetNotes(Combobox1.Text);
    if ImgExists(currentname) then
    begin
      ToolButton7.Enabled := True;
      Image2.Enabled      := True;
    end
    else
    begin
      ToolButton7.Enabled := False;
      Image2.Enabled      := False;
    end;
    if f_craterlist.Visible then
    begin
      ToolButton10Click(nil);
    end;
    Result := True;
  end;
end;

procedure TForm1.SetDescText(const Value: string);
var
  s: TStringStream;
  NewHTML: TIpHtml;
begin
  try
    s := TStringStream.Create(UTF8FileHeader+Value);
    try
      NewHTML := TIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel1
      NewHTML.LoadFromStream(s);
    finally
      s.Free;
    end;
    Desc1.SetHtml(NewHTML);
  except
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Firstsearch := True;
  SearchText  := Combobox1.Text;
  SearchName(SearchText, True);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SearchName(SearchText, True);
end;

procedure TForm1.DecreaseFont1Click(Sender: TObject);
var i: integer;
begin
try
  i:=Desc1.DefaultFontSize-1;
  if i>4 then Desc1.DefaultFontSize:=i;
  SetDescText(Desctxt);
except
end;
end;

procedure TForm1.IncreaseFont1Click(Sender: TObject);
var i:integer;
begin
try
  i:=Desc1.DefaultFontSize+1;
  if i<20 then Desc1.DefaultFontSize:=i;
  SetDescText(Desctxt);
except
end;
end;

procedure TForm1.OptFeatures1Click(Sender: TObject);
begin
  f_features.showmodal;
end;


procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  Firstsearch := True;
  SearchName(Combobox1.Text, True);
end;

procedure TForm1.Quitter1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.RefreshMoonImage;
var
  moonrise, moonset, moontransit, azimuthrise, azimuthset, eph: string;
  jd0, st0, q, cphase, colong, hh, az, ah: double;
  v1, v2, v3, v4, v5, v6, v7, v8, v9: double;
  gpa, glibrb, gsunincl, glibrl: double;
  aa, mm, dd, i, j: integer;
const
  b = ' ';
begin
  st0 := 0;
  ecl:=ecliptic(CurrentJD);
  Fplanet.SetDE(jpldir);
  Fplanet.nutation(CurrentJD,nutl,nuto);
  Fplanet.sunecl(CurrentJD,sunl,sunb);
  PrecessionEcl(jd2000,CurrentJD,sunl,sunb);
  aberration(CurrentJD,abe,abp);
  eph:=Fplanet.Moon(CurrentJD, ra, Dec, dist, dkm, diam, phase, illum);
  Fplanet.MoonOrientation(CurrentJD, ra, Dec, dist, gpa, glibrb, gsunincl, glibrl);
  if not geocentric then
  begin
    jd0 := jd(CurYear, CurrentMonth, CurrentDay, 0.0);
    st0 := SidTim(jd0, CurrentTime - Timezone, ObsLongitude);
    Paralaxe(st0, dist, ra, Dec, ra, Dec, q, jd2000, CurrentJD);
    diam := diam / q;
    dkm  := dkm * q;
    dist := dist * q;
  end;
  apparent_equatorial(ra,Dec,ecl,sunl,abp,abe,nutl,nuto,false);
  rad := ra;
  ded := Dec;
  mean_equatorial(ra,Dec,ecl,sunl,abp,abe,nutl,nuto);
  precession(jd2000, CurrentJD, rad, ded);
  Fplanet.MoonOrientation(CurrentJD, ra, Dec, dist, pa, librb, sunincl, librl);
  cphase := phase + glibrl;
  tphase := phase;
  colong := rmod(90 - tphase - glibrl + 360, 360);
  jd0    := jd(CurYear, 1, 1, 0.0);
  Fplanet.MoonPhases(CurYear + (CurrentJD - jd0) / 365.25, nmjd, fqjd, fmjd, lqjd);
  lunaison := CurrentJD - nmjd;
  if lunaison < 0 then
  begin
    lunaison := CurrentJD - Fplanet.MoonPhase(floor(12.3685 *
      (CurYear - 2000 - 0.04 + (CurrentJD - jd0) / 365.25)));
  end;
  StatusBar1.Panels[2].Text := rsm_51 + ': ' + date2str(curyear, currentmonth, currentday) +
    '   ' + rsm_50 + ': ' + timtostr(currenttime);
  phaseoffset := 0;

  djd(nmjd + (GetJDTimeZone(nmjd) - DT_UT) / 24, aa, mm, dd, hh);
  labelnm.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);
  djd(fqjd + (GetJDTimeZone(fqjd) - DT_UT) / 24, aa, mm, dd, hh);
  labelfq.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);
  djd(fmjd + (GetJDTimeZone(fmjd) - DT_UT) / 24, aa, mm, dd, hh);
  labelfm.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);
  djd(lqjd + (GetJDTimeZone(lqjd) - DT_UT) / 24, aa, mm, dd, hh);
  labellq.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);

 // Stringgrid1.colwidths[1] := 150;
  i := 0;
  Stringgrid1.Cells[0, i] := rsEphemeris;
  Stringgrid1.Cells[1, i] := eph;
  Inc(i);
  if geocentric then
  begin
    Stringgrid1.Cells[0, i] := form2.Label16.Caption + ':';
    Stringgrid1.Cells[1, i] := rst_26;
  end
  else
  begin
    Stringgrid1.Cells[0, i] := form2.Label16.Caption + ':';
    Stringgrid1.Cells[1, i] := demtostr(ObsLatitude) + ' ' + demtostr(ObsLongitude) +
      ' Tz:' + armtostr(timezone);
  end;
  Inc(i);
  Stringgrid1.Cells[0, i] := rsm_51 + ':';
  Stringgrid1.Cells[1, i] := date2str(curyear, currentmonth, currentday) +
    ' ' + timtostr(currenttime);
  djd(currentjd, aa, mm, dd, hh);
  Inc(i);
  Stringgrid1.Cells[0, i] := rsm_51 + ' (TT):';
  Stringgrid1.Cells[1, i] := date2str(aa, mm, dd) + ' ' + timtostr(hh);
  Inc(i);
  Stringgrid1.Cells[0, i] := '(J2000) ' + rsm_29;
  Stringgrid1.Cells[1, i] := arptostr(rad2deg * ra / 15,1);
  Inc(i);
  Stringgrid1.Cells[0, i] := '(J2000) ' + rsm_30;
  Stringgrid1.Cells[1, i] := deptostr(rad2deg * Dec,1);
  Inc(i);
  Stringgrid1.Cells[0, i] := '(' + rsm_51 + ')' + b + rsm_29;
  Stringgrid1.Cells[1, i] := arptostr(rad2deg * rad / 15,1);
  Inc(i);
  Stringgrid1.Cells[0, i] := '(' + rsm_51 + ')' + b + rsm_30;
  Stringgrid1.Cells[1, i] := deptostr(rad2deg * ded,1);
  Inc(i);
  Stringgrid1.Cells[0, i] := rsm_31;
  Stringgrid1.Cells[1, i] := IntToStr(round(dkm)) + rsm_18;
  Inc(i);
  Stringgrid1.Cells[0, i] := rsm_36;
  Stringgrid1.Cells[1, i] := formatfloat(f2, diam / 60) + lmin;
  Inc(i);
  Stringgrid1.Cells[0, i] := rsm_48;
  Stringgrid1.Cells[1, i] := formatfloat(f1, colong) + ldeg;
  Inc(i);
  Stringgrid1.Cells[0, i] := rsm_32;
  Stringgrid1.Cells[1, i] := formatfloat(f1, phase) + ldeg;
  Inc(i);
  Stringgrid1.Cells[0, i] := rsm_46;
  Stringgrid1.Cells[1, i] := formatfloat(f2, lunaison) + ' ' + rsm_47;
  Inc(i);
  Stringgrid1.Cells[0, i] := rsm_35;
  Stringgrid1.Cells[1, i] := formatfloat(f1, illum * 100) + '%';
  Inc(i);
  Stringgrid1.Cells[0, i] := rsm_45;
  Stringgrid1.Cells[1, i] := formatfloat(f1, sunincl) + ldeg;
  Inc(i);
  Stringgrid1.Cells[0, i] := rsm_33;
  Stringgrid1.Cells[1, i] := demtostr(librb);
  Inc(i);
  Stringgrid1.Cells[0, i] := rsm_34;
  Stringgrid1.Cells[1, i] := demtostr(librl);
  Inc(i);
  Stringgrid1.Cells[0, i] := rsm_37;
  Stringgrid1.Cells[1, i] := formatfloat(f1, pa) + ldeg;
  if not geocentric then
  begin
    eq2hz(st0 - rad, ded, az, ah);
    az := rmod(rad2deg * az + 180, 360);
    Inc(i);
    Stringgrid1.Cells[0, i] := rsm_73;
    Stringgrid1.Cells[1, i] := demtostr(az);
    Inc(i);
    Stringgrid1.Cells[0, i] := rsm_74;
    Stringgrid1.Cells[1, i] := demtostr(rad2deg * ah);
    Fplanet.PlanetRiseSet(11, jd(CurYear, CurrentMonth, CurrentDay, 0),
      True, moonrise, moontransit, moonset, azimuthrise, azimuthset, v1, v2, v3, v4, v5, v6, v7, v8, v9, j);
    Inc(i);
    Stringgrid1.Cells[0, i] := rsm_38;
    Stringgrid1.Cells[1, i] := moonrise;
    Inc(i);
    Stringgrid1.Cells[0, i] := rsm_39;
    Stringgrid1.Cells[1, i] := moontransit;
    Inc(i);
    Stringgrid1.Cells[0, i] := rsm_40;
    Stringgrid1.Cells[1, i] := moonset;
    Inc(i);
    Stringgrid1.Cells[0, i] := rsm_41;
    Stringgrid1.Cells[1, i] := azimuthrise;
    if obslatitude > 0 then
    begin
      Inc(i);
      Stringgrid1.Cells[0, i] := rsm_55;
      Stringgrid1.Cells[1, i] := dedtostr(90 - obslatitude + rad2deg * Dec);
    end
    else
    begin
      Inc(i);
      Stringgrid1.Cells[0, i] := rsm_55;
      Stringgrid1.Cells[1, i] := dedtostr(90 + obslatitude - rad2deg * Dec);
    end;
    Inc(i);
    Stringgrid1.Cells[0, i] := rsm_42;
    Stringgrid1.Cells[1, i] := azimuthset;
  end
  else
  begin
    Inc(i);
    Stringgrid1.Cells[0, i] := b;
    Stringgrid1.Cells[1, i] := b;
    Inc(i);
    Stringgrid1.Cells[0, i] := b;
    Stringgrid1.Cells[1, i] := b;
    Inc(i);
    Stringgrid1.Cells[0, i] := b;
    Stringgrid1.Cells[1, i] := b;
    Inc(i);
    Stringgrid1.Cells[0, i] := b;
    Stringgrid1.Cells[1, i] := b;
    Inc(i);
    Stringgrid1.Cells[0, i] := b;
    Stringgrid1.Cells[1, i] := b;
    Inc(i);
    Stringgrid1.Cells[0, i] := b;
    Stringgrid1.Cells[1, i] := b;
    Inc(i);
    Stringgrid1.Cells[0, i] := b;
    Stringgrid1.Cells[1, i] := b;
    Inc(i);
    Stringgrid1.Cells[0, i] := b;
    Stringgrid1.Cells[1, i] := b;
  end;
  if librationeffect and activemoon.VisibleSideLock then
  begin
    activemoon.LibrLat := deg2rad*librb;
    activemoon.LibrLon := -deg2rad*librl;
  end
  else  begin
    activemoon.LibrLat := 0;
    activemoon.LibrLon := 0;
  end;
  if FollowNorth then
  begin
    CameraOrientation := rmod(-PA + PoleOrientation + 360, 360);
    activemoon.Orientation:=CameraOrientation;
  end;
  activemoon.JD:=CurrentJD;
  activemoon.PositionAngle:=deg2rad*PA;
  activemoon.RaCentre:=rad;
  activemoon.DeCentre:=ded;
  activemoon.Diameter:=deg2rad*diam/3600;
  activemoon.EarthDistance:=dkm;
  activemoon.ShowPhase:=phaseeffect;
  activemoon.Phase:=deg2rad*cphase;
  activemoon.SunIncl:=deg2rad*sunincl;
  activemoon.LibrationMark:=ShowLibrationMark;
  activemoon.RefreshAll;
end;

procedure  TForm1.SetZoomBar;
begin
lockzoombar:=true;
trackbar1.min := 100;
trackbar1.max := round(100 * activemoon.ZoomMax);
trackbar1.position := round(100 * activemoon.Zoom);
end;

procedure  TForm1.GetMsg(Sender: TObject; msgclass:TMoonMsgClass; value: String);
begin
if sender is Tf_moon then SetActiveMoon(Tf_moon(sender));
case msgclass of
MsgZoom: begin
          value:=StringReplace(value,'FOV:',rsm_43,[]);
          statusbar1.Panels[3].Text := value;
          SetZoomBar;
         end;
MsgPerf: begin
          Label15.Caption := rsm_44 + blank + value;
         end;
   else  statusbar1.Panels[4].Text := value;
end;
end;


procedure TForm1.IdentLB(l, b: single);
begin
  if SearchAtPos(l, b) then
  begin
    l := dbm.Results[0].ByField['LONGIN'].AsFloat;
    b := dbm.Results[0].ByField['LATIN'].AsFloat;
    searchl := l;
    searchb := b;
    currentl := l;
    currentb := b;
    currentname := dbm.Results[0].ByField['NAME'].AsString;
    Combobox1.Text := currentname;
    GetHTMLDetail(dbm.Results[0], Desctxt);
    SetDescText(Desctxt);
    if dbtab.TabVisible then
      GetDBgrid;
    GetNotes(Combobox1.Text);
    if ImgExists(currentname) then
    begin
      ToolButton7.Enabled := True;
      Image2.Enabled      := True;
    end
    else
    begin
      ToolButton7.Enabled := False;
      Image2.Enabled      := False;
    end;
    if f_craterlist.Visible then
    begin
      ToolButton10Click(nil);
    end;
  end
end;

procedure TForm1.InitLopamIdx;
var
  f: textfile;
  i: integer;
begin
  filemode := 0;
  assignfile(f, Slash(appdir) + Slash('Database') + 'lopamidx.txt');
  reset(f);
  readln(f);
  readln(f, lopamplateurl);
  readln(f, lopamnameurl);
  readln(f, lopamdirecturl);
  readln(f, lopamlocalurl);
  closefile(f);
  i := pos('URL=', lopamplateurl);
  lopamplateurl := copy(lopamplateurl, i + 4, 999);
  i := pos('SUFFIX=', lopamplateurl);
  lopamplatesuffix := trim(copy(lopamplateurl, i + 7, 999));
  lopamplateurl := trim(copy(lopamplateurl, 1, i - 1));

  i := pos('URL=', lopamnameurl);
  lopamnameurl := copy(lopamnameurl, i + 4, 999);
  i := pos('SUFFIX=', lopamnameurl);
  lopamnamesuffix := trim(copy(lopamnameurl, i + 7, 999));
  lopamnameurl := trim(copy(lopamnameurl, 1, i - 1));

  i := pos('URL=', lopamdirecturl);
  lopamdirecturl := copy(lopamdirecturl, i + 4, 999);
  i := pos('SUFFIX=', lopamdirecturl);
  lopamdirectsuffix := trim(copy(lopamdirecturl, i + 7, 999));
  lopamdirecturl := trim(copy(lopamdirecturl, 1, i - 1));

  i := pos('URL=', lopamlocalurl);
  lopamlocalurl := copy(lopamlocalurl, i + 4, 999);
  i := pos('SUFFIX=', lopamlocalurl);
  lopamlocalsuffix := trim(copy(lopamlocalurl, i + 7, 999));
  lopamlocalurl := trim(copy(lopamlocalurl, 1, i - 1));
end;

procedure TForm1.ListUserDB;
var
  i, j, k: integer;
begin
  for i := 0 to form2.Checklistbox1.Count - 1 do
    (form2.Checklistbox1.Items.Objects[i] as TDBinfo).Free;
  form2.CheckListBox1.Clear;
  dbm.query('select * from user_database where DBN>9');
  for i := 0 to dbm.RowCount - 1 do
  begin
    j := dbm.Results[i].format[0].AsInteger;
    k := form2.CheckListBox1.Items.Add(dbm.Results[i][1]);
    form2.Checklistbox1.Items.Objects[k] := TDBinfo.Create;
    (form2.Checklistbox1.Items.Objects[k] as TDBinfo).dbnum := j;
    form2.CheckListBox1.Checked[k] := usedatabase[j];
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  DecimalSeparator := '.';
  ThousandSeparator:=' ';
  UniqueInstance1:=TCdCUniqueInstance.Create(self);
  UniqueInstance1.Identifier:='Virtual_Moon_Atlas_MapLun';
  UniqueInstance1.OnOtherInstance:=OtherInstance;
  UniqueInstance1.OnInstanceRunning:=InstanceRunning;
  UniqueInstance1.Enabled:=true;
  UniqueInstance1.Loaded;
  ToolsWidth:=300;
  {$ifdef mswindows}
  ScaleForm(self,Screen.PixelsPerInch/96);
  ToolsWidth:=round(ToolsWidth*Screen.PixelsPerInch/96);
  {$endif}
  PageControl1.Align:=alRight;
  Splitter1.Align:=alRight;
  PanelMoon2.Align:=alLeft;
  Splitter2.Align:=alLeft;
  PanelMoon.Align:=alClient;
  Splitter2.Visible:=false;
  PanelMoon2.Visible:=false;
{$ifdef darwin}
  TrackBar1.Top:=2;
  ToolButton13.Visible:=false; // fullscreen
  FullScreen1.Visible:=false;
{$endif}
{$ifdef linux}
  TrackBar1.Top:=-8;
{$endif}
{$ifdef mswindows}
  TrackBar1.Top:=-2;
  CheckBox3.Visible:=true;  // antialias
  paneltel.Visible:=true;  // telescope
{$endif}
  StartDatlun:=false;
  StartPhotlun:=false;
  StartCDC:=false;
  CanCloseDatlun:=true;
  CanClosePhotlun:=true;
  CanCloseCDC:=true;
  dbedited  := False;
  perfdeltay := 0.00001;
  lockchart := False;
  StartedByDS := False;
  distancestart := False;
  CurrentEyepiece := 0;
  EyepieceRatio := 1;
  zoom      := 1;
  useDBN    := 7;
  compresstexture := true;
  antialias := false;
  ForceBumpMapSize:=0;
  showoverlay := True;
  LastScopeTracking := 0;
  UseComputerTime := True;
  GetAppDir;
  chdir(appdir);
  skipresize := True;
  skipresize := False;
  skiporient:=false;
  skiprot:=false;
  Fplanet    := TPlanet.Create(self);
  searchlist := TStringList.Create;
  param      := TStringList.Create;
  param.Clear;
  Plan404    := nil;
  Plan404lib := LoadLibrary(lib404);
  if Plan404lib <> 0 then
  begin
    Plan404 := TPlan404(GetProcAddress(Plan404lib, 'Plan404'));
  end;
  if @Plan404=nil then begin
     MessageDlg('Could not load library '+lib404+crlf
               +'Please try to reinstall the program.',
               mtError, [mbAbort], 0);
     Halt;
  end;
  tz := TCdCTimeZone.Create;
  tz.LoadZoneTab(ZoneDir+'zone.tab');
  texturefiles:=TStringList.Create;
  for i:=0 to 3 do texturefiles.Add('');
  texturefiles[0]:='Airbrush';
  texturefiles[1]:='Airbrush';
  if DirectoryExists(slash(appdir)+slash('Textures')+slash('Lopam')+'L3') then
     texturefiles[2]:='Lopam';
  if DirectoryExists(slash(appdir)+slash('Textures')+slash('Lopam')+'L4') then
     texturefiles[3]:='Lopam';
  CursorImage1 := TCursorImage.Create;
  overlayhi := Tbitmap.Create;
  overlayimg := Tbitmap.Create;
  // hide developpement tools or not finished function
  if not fileexists('version.developpement') then
  begin
    dbtab.TabVisible := False;       // edit database
    Enregistredist.Visible := False; // record new object
    BMP15001.Visible := False;       // save bmp for light version
    BMP30001.Visible := False;       // save bmp for light version
  end;
 moon1:=Tf_moon.Create(PanelMoon);
 activemoon:=moon1;
 moon1.Moon.Align:=alClient;
 moon1.onMoonClick:=MoonClickEvent;
 moon1.onMoonMove:=MoonMoveEvent;
 moon1.onMoonMeasure:=MoonMeasureEvent;
 moon1.onGetMsg:=GetMsg;
 moon1.onGetLabel:=GetLabel;
 moon1.onGetSprite:=GetSprite;
 moon1.PopUp:=PopupMenu1;
 moon1.TexturePath:=slash(appdir)+slash('Textures');
 moon1.OverlayPath:=slash(appdir)+slash('Textures')+slash('Overlay');
 if fileexists(slash(appdir) + slash('data') + 'retic.cur') then
  begin
    CursorImage1.LoadFromFile(slash(appdir) + slash('data') + 'retic.cur');
    Screen.Cursors[crRetic] := CursorImage1.Handle;
    moon1.GLSceneViewer1.Cursor := crRetic;
  end;
  SetLang1;
  readdefault;
  moon1.AntiAliasing:=antialias;
  moon1.ForceBumpMapSize:=ForceBumpMapSize;
  currentid := '';
  librl     := 0;
  librb     := 0;
  zoom      := 1;
  lastx     := 0;
  lasty     := 0;
  SkipIdent := False;
  dblox     := TMlb2.Create;
  dbnotes   := TMlb2.Create;
  GetSkyChartInfo;
  CheckBox8.Checked := compresstexture;
  CheckBox3.Checked := antialias;
  if PoleOrientation = 0 then
    RadioGroup2.ItemIndex := 0
  else
    RadioGroup2.ItemIndex := 1;
  checkbox1.Checked := FollowNorth;
  ToolButton12.Down := showlabel;
  appname := ParamStr(0);
  if paramcount > 0 then
  begin
    for i := 1 to paramcount do
    begin
      param.Add(ParamStr(i));
      if ParamStr(i) = '-safe' then
        borderstyle := bsNone; // canot set this later in formshow
    end;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
// bug with tupdown size. width must be 13 on form designer
UpDown1.Width:=14;
UpDown2.Width:=14;
UpDown3.Width:=14;
UpDown4.Width:=14;
UpDown5.Width:=14;
UpDown6.Width:=14;
trackdelay.Width:=14;
Application.BringToFront;
//moon1.GLSceneViewer1.Visible:=false;
moon1.GLSceneViewer1.Camera:=nil;
StartTimer.Enabled:=true;
end;

procedure TForm1.Init;
var i:integer;
    savecaption,savesidelist: string;
begin
try
  Setlang;
  form2.onPrinterDialog:=Selectiondimprimante1Click;
  screen.cursor := crHourGlass;
 // moon1.GLSceneViewer1.Visible:=false;
  application.ProcessMessages;
  LoadDB(dbm);
  application.ProcessMessages;
  ListUserDB;
  InitLopamIdx;
  InitTelescope;
  dblox.LoadFromFile(Slash(appdir) + Slash('Database') + 'lopamidx.csv');
  dblox.GoFirst;
  if fileexists(Slash(DBdir) + 'notes.csv') then
    dbnotes.LoadFromFile(Slash(DBdir) + 'notes.csv')
  else
  begin
    if fileexists(Slash(appdir) + Slash('Database') + 'notes.csv') then
      dbnotes.LoadFromFile(Slash(appdir) + Slash('Database') + 'notes.csv')
    else
    begin
      dbnotes.AddField('NAME');
      dbnotes.AddField('NOTES');
    end;
  end;
  dbnotes.GoFirst;
  if UseComputerTime then
    InitDate
  else
    SetJDDate;
  if screen.Width < 750 then
  begin
   Top    := 0;
   Left   := 0;
   Width  := screen.Width;
   Height := screen.Height;
  end;
  form2.tzinfo:=tz;
  form2.LoadCountry(slash(Appdir)+slash('data')+'country.tab');
  moon1.Init;
  moon1.BumpMethod:=TBumpMapCapability(BumpMethod);
  moon1.BumpMipmap:=BumpMipmap;
  moon1.TextureCompression:=compresstexture;
  moon1.texture:=texturefiles;
  if moon1.CanBump then
     moon1.BumpPath:=slash(appdir)+slash('Textures')+slash('Bumpmap');
  moon1.VisibleSideLock:=true;
  moon1.Labelcolor:=autolabelcolor;
  AsMultiTexture:=moon1.AsMultiTexture;
  moon1.SetMark(0, 0, '');
  moon1.zoom:=1;
  moon1.GridSpacing:=gridspacing;
  ReadParam;
  memo2.Width    := PrintTextWidth;
  label10.Left   := toolbar2.left + toolbar2.Width + 2;
  trackbar1.Left := label10.Left + label10.Width + 2;
  toolbar1.Left  := trackbar1.Left + trackbar1.Width + 2;
  Trackbar2.position := moon1.ambientColor and $FF;
  Trackbar3.position := moon1.diffuseColor and $FF;
  Trackbar4.position := moon1.specularColor and $FF;
  if moon1.GLSphereMoon.Slices = 720 then
    Trackbar5.position := 5
  else if moon1.GLSphereMoon.Slices = 360 then
    Trackbar5.position := 4
  else if moon1.GLSphereMoon.Slices = 180 then
    Trackbar5.position := 3
  else if moon1.GLSphereMoon.Slices = 90 then
    Trackbar5.position := 2
  else if moon1.GLSphereMoon.Slices = 45 then
    Trackbar5.position := 1
  else
    Trackbar5.position := 1;
  LabelAltitude.Caption:=inttostr(TrackBar6.Position) + blank + rsm_18;
  LabelIncl.Caption:=inttostr(TrackBar7.Position)+ ldeg;
  LibrationButton.Down := librationeffect;
  PhaseButton.Down := phaseeffect;
  LoadOverlay(overlayname, overlaytr);
  Visible:=true;
  GridButtonClick(nil);
  if currentname <> '' then
  begin
    firstsearch := True;
    searchname(currentname, False);
  end;
  btnEffacerClick(nil);
  SetEyepieceMenu;
  moon1.Mirror:=checkbox2.Checked;
  GridButton.Visible:=AsMultiTexture;
finally
  screen.cursor := crDefault;
//  StartTimer.Enabled:=true;
end;
end;

procedure TForm1.StartTimerTimer(Sender: TObject);
var savecaption,savesidelist: string;
    i: integer;
begin
StartTimer.Enabled:=false;
screen.cursor := crHourGlass;
init;
try
  if (currentid = '') then
  begin
    // show an interesting object
    RefreshMoonImage;
    Combobox2.ItemIndex   := 0;
    Combobox3.ItemIndex   := 5;
    RadioGroup1.ItemIndex := 1;
    savesidelist:=sidelist;
    sidelist:='1';
    Updterminateur(5);
    SearchText  := trim(copy(ListBox1.Items[0], 2, 999));
    if searchtext=trim(copy(rsm_27, 2, 999)) then begin
      Combobox3.ItemIndex := 0;
      Updterminateur(12);
      SearchText  := trim(copy(ListBox1.Items[0], 2, 999));
    end;
    Firstsearch := True;
    SearchName(SearchText, false);
    sidelist := savesidelist;
    Combobox3.ItemIndex := 0;
    currentphase := -999;
    moon1.CenterAt(99999, 99999);
  end;
  moon1.GLSceneViewer1.Visible:=true;
  moon1.GLSceneViewer1.Camera:=moon1.GLCamera1;
  Application.ProcessMessages;
  PhaseButtonClick(nil);
  SetZoomBar;
  moon1.RefreshAll;
  screen.cursor := crDefault;
if firstuse then begin
    savecaption:=form2.Caption;
    form2.Caption:=rsFirstUseSett;
    form2.PageControl1.Page[0].TabVisible:=true;
    for i:=1 to 7 do form2.PageControl1.Page[i].TabVisible:=false;
    Configuration1Click(nil);
    for i:=0 to 7 do form2.PageControl1.Page[i].TabVisible:=true;
    form2.Caption := savecaption;
end;
finally
 screen.cursor := crDefault;
end;
end;

procedure TForm1.Configuration1Click(Sender: TObject);
var
  reload, reloaddb, systemtimechange: boolean;
  i, j: integer;
  p: TPoint;
begin
  p:=Point(ToolButton2.Left,ToolButton2.Top+ToolButton2.Height);
  p:=ToolBar2.ClientToScreen(p);
  try
    reload   := False;
    reloaddb := False;
    form2.Edit1.Text := formatfloat(f2, abs(ObsLatitude));
    form2.Edit2.Text := formatfloat(f2, abs(ObsLongitude));
    if Obslatitude >= 0 then
      form2.ComboBox1.ItemIndex := 0
    else
      form2.ComboBox1.ItemIndex := 1;
    if Obslongitude >= 0 then
      form2.ComboBox2.ItemIndex := 1
    else
      form2.ComboBox2.ItemIndex := 0;
    form2.checkbox19.Checked := usedatabase[1];
    form2.checkbox20.Checked := usedatabase[2];
    form2.checkbox21.Checked := usedatabase[3];
    form2.checkbox22.Checked := usedatabase[4];
    form2.checkbox23.Checked := usedatabase[5];
    form2.checkbox24.Checked := usedatabase[6];
    form2.checkbox25.Checked := usedatabase[7];
    ListUserDB;
    form2.checkbox1.Checked := phaseeffect;
    form2.checkbox2.Checked := librationeffect;
    if geocentric then
       form2.radiogroup7.ItemIndex:=1
    else
       form2.radiogroup7.ItemIndex:=0;
    form2.checkbox5.Checked := showlabel;
    form2.checkbox6.Checked := showmark;
    form2.checkbox14.Checked := showlibrationmark;
    form2.checkbox17.Checked := labelcenter;
    form2.checkbox18.Checked := minilabel;
    form2.CheckBox4.Checked := shortdesc;
    form2.Shape1.Brush.Color := marklabelcolor;
    form2.Shape2.Brush.Color := markcolor;
    form2.Shape3.Brush.Color := autolabelcolor;
    form2.TrackBar2.Position := -LabelDensity;
    form2.TrackBar4.Position := marksize;
    form2.newlang := language;
    if wantbump or activemoon.Bumpmap then
       form2.BumpRadioGroup.ItemIndex:=0
    else
       form2.BumpRadioGroup.ItemIndex:=1;
    form2.BumpRadioGroup.Visible:=(activemoon.BumpMapCapabilities<>[]);
    form2.RadioGroup1.ItemIndex:=ord(activemoon.BumpMethod);
    form2.RadioGroup1.Visible:=((bcDot3TexCombiner in activemoon.BumpMapCapabilities)and(bcBasicARBFP in moon1.BumpMapCapabilities));
    form2.CheckBox3.Checked:=activemoon.BumpMipmap;
    form2.StringGrid1.RowCount := maximgdir + 10;
    if saveimagesize = 0 then
      form2.ComboBox4.ItemIndex := 0
    else
      form2.ComboBox4.Text      := IntToStr(saveimagesize);
    form2.CheckBox12.Checked := externalimage;
    form2.edit4.Text := externalimagepath;
    form2.CheckBox7.Checked := saveimagewhite;
    for i := 1 to maximgdir do
    begin
      form2.StringGrid1.Cells[0, i] := imgdir[i - 1, 2];
      form2.StringGrid1.Cells[1, i] := imgdir[i - 1, 0];
    end;
    for i := 1 to 10 do
    begin
      form2.StringGrid2.Cells[0, i] := eyepiecename[i];
      form2.StringGrid2.Cells[1, i] := IntToStr(eyepiecefield[i]);
      form2.StringGrid2.Cells[2, i] := IntToStr(eyepiecemirror[i]);
      form2.StringGrid2.Cells[3, i] := IntToStr(eyepiecerotation[i]);
    end;
    form2.LongEdit1.Value    := LeftMargin;
    form2.TrackBar1.Position := PrintTextWidth;
    form2.CheckBox13.Checked := PrintChart;
    form2.CheckBox8.Checked  := PrintEph;
    form2.CheckBox9.Checked  := PrintDesc;
    form2.TexturePath := activemoon.TexturePath;
    form2.texturefn.Assign(texturefiles);
    form2.CheckBox15.Checked := LopamDirect;
    form2.ruklprefix.Text := ruklprefix;
    form2.ruklsuffix.Text := ruklsuffix;
    form2.combobox5.Text := remext(overlayname);
    form2.trackbar5.position := round(overlaytr*100);
    form2.combobox5change(Sender);
    form2.checkbox11.Checked := showoverlay;
    form2.checkbox10.Checked := GridButton.Down;
    form2.TrackBar3.Position:=gridspacing;
    form2.checkbox16.Checked := UseComputerTime;
    form2.FontDialog1.Font:=activemoon.LabelFont;
    form2.LabelFont.Caption:=activemoon.LabelFont.Name;
    form2.LabelFont.Font:=activemoon.LabelFont;
    form2.LabelFont.Font.Color:=clWindowText;
    form2.obstz:=ObsTZ;
    form2.SetObsCountry(ObsCountry);
    FormPos(form2,p.x,p.y);
    Form2.showmodal;
    if form2.ModalResult = mrOk then
    begin
      screen.cursor := crhourglass;
      if (form2.combobox5.Text <> remext(overlayname)) or
        (form2.trackbar5.position <> round(overlaytr*100)) or
        (form2.TrackBar3.Position<>gridspacing) or
        (form2.checkbox11.Checked <> showoverlay) then
        reload    := True;
      overlayname := form2.combobox5.Text + '.jpg';
      overlaytr  := form2.trackbar5.position/100;
      showoverlay := form2.checkbox11.Checked;
      GridButton.Down:=form2.checkbox10.Checked;
      gridspacing:=form2.TrackBar3.Position;
      if form2.TextureChanged then
      begin
        texturefiles.Assign(form2.texturefn);
        reload := True;
      end;
      if activemoon=moon1 then begin
        if wantbump<>(form2.BumpRadioGroup.ItemIndex=0) then reload:=true;
        wantbump := (form2.BumpRadioGroup.ItemIndex=0);
      end else
        wantbump:=false;
      activemoon.BumpMethod:=TBumpMapCapability(form2.RadioGroup1.ItemIndex);
      activemoon.BumpMipmap := form2.CheckBox3.Checked;
      ruklprefix    := form2.ruklprefix.Text;
      ruklsuffix    := form2.ruklsuffix.Text;
      markcolor     := form2.Shape2.Brush.Color;
      marklabelcolor    := form2.Shape1.Brush.Color;
      autolabelcolor := form2.Shape3.Brush.Color;
      LabelDensity  := abs(form2.TrackBar2.Position);
      marksize      := form2.TrackBar4.Position;
      showlabel     := form2.checkbox5.Checked;
      showmark      := form2.checkbox6.Checked;
      showlibrationmark := form2.checkbox14.Checked;
      labelcenter   := form2.checkbox17.Checked;
      minilabel     := form2.checkbox18.Checked;
      shortdesc := form2.CheckBox4.Checked;
      activemoon.LabelFont:=form2.FontDialog1.Font;
      activemoon.Labelcolor:=autolabelcolor;
      Obslatitude := strtofloat(form2.Edit1.Text);
      if form2.ComboBox1.ItemIndex = 1 then
        Obslatitude := -Obslatitude;
      Obslongitude  := strtofloat(form2.Edit2.Text);
      if form2.ComboBox2.ItemIndex = 0 then
        Obslongitude := -Obslongitude;
      systemtimechange := UseComputerTime <> form2.checkbox16.Checked;
      ObsCountry:=form2.ObsCountry;
      ObsTZ := form2.obstz;
      tz.TimeZoneFile := ZoneDir + StringReplace(ObsTZ, '/', PathDelim, [rfReplaceAll]);
      UseComputerTime := form2.checkbox16.Checked;
      timezone := gettimezone(now);
      if systemtimechange then
      begin
        InitDate;
        reload := True;
      end;
      phaseeffect     := form2.checkbox1.Checked;
      librationeffect := form2.checkbox2.Checked;
      Geocentric      := (form2.RadioGroup7.ItemIndex=1);
      if usedatabase[1] <> form2.checkbox19.Checked then
        reloaddb := True;
      if usedatabase[2] <> form2.checkbox20.Checked then
        reloaddb := True;
      if usedatabase[3] <> form2.checkbox21.Checked then
        reloaddb := True;
      if usedatabase[4] <> form2.checkbox22.Checked then
        reloaddb := True;
      if usedatabase[5] <> form2.checkbox23.Checked then
        reloaddb := True;
      if usedatabase[6] <> form2.checkbox24.Checked then
        reloaddb     := True;
      if usedatabase[7] <> form2.checkbox25.Checked then
        reloaddb     := True;
      usedatabase[1] := form2.checkbox19.Checked;
      usedatabase[2] := form2.checkbox20.Checked;
      usedatabase[3] := form2.checkbox21.Checked;
      usedatabase[4] := form2.checkbox22.Checked;
      usedatabase[5] := form2.checkbox23.Checked;
      usedatabase[6] := form2.checkbox24.Checked;
      usedatabase[7] := form2.checkbox25.Checked;
      for i := 0 to form2.Checklistbox1.Count - 1 do
      begin
        j := (form2.Checklistbox1.Items.Objects[i] as TDBinfo).dbnum;
        if usedatabase[j] <> form2.Checklistbox1.Checked[i] then
          reloaddb     := True;
        usedatabase[j] := form2.Checklistbox1.Checked[i];
      end;
      InitObservatoire;
      CurrentJD := jd(CurYear, CurrentMonth, CurrentDay, Currenttime - timezone + DT_UT);
      if form2.newlang <> language then
      begin
        language:=u_translation.translate(form2.newlang,'en');
        u_translation_database.translate(language,'en');
        uplanguage:=uppercase(language);
        SetLang;
        reloaddb := True;
      end;
      if reloaddb then begin
        LoadDB(dbm);
        firstsearch := True;
        SearchName(currentname, False);
      end;
      LibrationButton.Down := librationeffect;
      PhaseButton.Down     := phaseeffect;
      externalimage := form2.CheckBox12.Checked;
      LopamDirect   := form2.CheckBox15.Checked;
      externalimagepath := form2.edit4.Text;
      saveimagewhite := form2.CheckBox7.Checked;
      val(form2.ComboBox4.Text, saveimagesize, i);
      if i <> 0 then
        saveimagesize := 0;
      maximgdir := 0;
      for i := 1 to form2.StringGrid1.RowCount - 1 do
        if trim(form2.StringGrid1.Cells[1, i]) > '' then
          Inc(maximgdir);
      setlength(imgdir, maximgdir);
      j := 0;
      for i := 1 to form2.StringGrid1.RowCount - 1 do
        if trim(form2.StringGrid1.Cells[1, i]) > '' then
        begin
          imgdir[j, 0] := form2.StringGrid1.Cells[1, i];
          imgdir[j, 2] := form2.StringGrid1.Cells[0, i];
          imgdir[j, 1] := '';
          Inc(j);
        end;
      for i := 1 to 10 do
      begin
        eyepiecename[i]     := trim(form2.StringGrid2.Cells[0, i]);
        eyepiecefield[i]    := strtointdef(form2.StringGrid2.Cells[1, i], 0);
        eyepiecemirror[i]   := strtointdef(form2.StringGrid2.Cells[2, i], 0);
        eyepiecerotation[i] := strtointdef(form2.StringGrid2.Cells[3, i], 0);
      end;
      SetEyepieceMenu;
      LeftMargin := form2.LongEdit1.Value;
      PrintTextWidth := form2.TrackBar1.Position;
      memo2.Width := PrintTextWidth;
      PrintChart := form2.CheckBox13.Checked;
      PrintEph  := form2.CheckBox8.Checked;
      PrintDesc := form2.CheckBox9.Checked;
      PhaseButtonClick(nil);
      GridButtonClick(nil);
      if reload then
      begin
        application.ProcessMessages;
        activemoon.Eyepiece := 0;
        LoadOverlay(overlayname, overlaytr);
        activemoon.Texture:=texturefiles;
        activemoon.GridSpacing:=gridspacing;
        RefreshMoonImage;
        activemoon.Zoom:=activemoon.Zoom;
      end
      else
        RefreshMoonImage;
      SaveDefault;
    end;
  finally
    screen.cursor := crdefault;
  end;
end;

procedure TForm1.Splitter1Moved(Sender: TObject);
begin
 ToolsWidth:=PageControl1.Width;
 if ToolsWidth<100 then ToolsWidth:=100;
 FormResize(Sender);
end;

procedure TForm1.Splitter2Moved(Sender: TObject);
begin
if PanelMoon.Width>0 then begin
  SplitSize:=PanelMoon.Width/(PanelMoon.Width+PanelMoon2.Width);
  FormResize(Sender);
end;
end;


procedure TForm1.ToolButton12Click(Sender: TObject);
begin
 showlabel:=not showlabel;
 ToolButton12.Down := showlabel;
 activemoon.RefreshAll;
end;

procedure TForm1.GridButtonClick(Sender: TObject);
begin
  activemoon.ShowGrid:=GridButton.Down;
end;

procedure TForm1.ToolButton14Click(Sender: TObject);
begin
  activemoon.ShowScale:=ToolButton14.Down;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
var p: TPoint;
begin
  p:=Point(ToolButton1.Left,ToolButton1.Top+ToolButton1.Height);
  p:=ToolBar2.ClientToScreen(p);
  FilePopup.PopUp(p.x,p.y);
end;

procedure TForm1.ToolButton8Click(Sender: TObject);
var p: TPoint;
begin
  p:=Point(ToolButton8.Left,ToolButton8.Top+ToolButton8.Height);
  p:=ToolBar2.ClientToScreen(p);
  HelpPopup.PopUp(p.x,p.y);
end;

procedure TForm1.FormResize(Sender: TObject);
var
  dx,w1,w2: integer;
begin
  if skipresize then
    exit;
  if csDestroying in ComponentState then
    exit;
  if csLoading in ComponentState then
    exit;
  if ToolsWidth<100 then ToolsWidth:=100;
  PageControl1.width:=ToolsWidth;
  if PanelMoon2.Visible then begin
    dx:=ClientWidth-Splitter2.Width;
    if not FullScreen then dx:=dx-ToolsWidth-Splitter1.Width;
    w1:=round(SplitSize*dx);
    w2:=dx-w1;
    PanelMoon.Width:=w1;
    PanelMoon2.Width:=w2;
    moon2.GLSceneViewer1.Align:=alNone;
    moon2.GLSceneViewer1.Top:=0;
    moon2.GLSceneViewer1.Align:=alClient;
  end;
  moon1.GLSceneViewer1.Align:=alNone;
  moon1.GLSceneViewer1.Top:=0;
  moon1.GLSceneViewer1.Align:=alClient;
  moon1.RefreshAll;
  activemoon.GetZoomInfo;
  if sender<>nil then ResizeTimer.Enabled:=true;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  initdate;
  RefreshMoonImage;
end;

procedure TForm1.ResizeTimerTimer(Sender: TObject);
begin
ResizeTimer.Enabled:=false;
FormResize(nil);
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  heure.Value      := 0;
  minute.Value     := 0;
  seconde.Value    := 0;
  updown4.position := 0;
  updown5.position := 0;
  updown6.position := 0;
  Button4Click(Sender);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  y, m, d: integer;
  h, n, s: word;
begin
  y     :=annee.Value;
  m     :=mois.Value;
  d     :=jour.Value;
  h     :=heure.Value;
  n     :=minute.Value;
  s     :=seconde.Value;
  timezone := GetJDTimeZone(jd(y, m, d, h+n/60+s/3600));
  dt_ut := dtminusut(y);
  CurYear := y;
  CurrentMonth := m;
  CurrentDay := d;
  Currenttime := h + n / 60 + s / 3600;
  CurrentJD := jd(y, m, d, Currenttime - timezone + DT_UT);
  RefreshMoonImage;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);

begin
ZoomTimer.Enabled:=false;
if not lockzoombar then
   ZoomTimer.Enabled:=true;
lockzoombar:=false;
end;

procedure TForm1.ZoomTimerTimer(Sender: TObject);
begin
ZoomTimer.Enabled:=false;
activemoon.Zoom := trackbar1.position / 100;
end;

procedure TForm1.EphTimer1Timer(Sender: TObject);
begin
  if lockrepeat then
    exit;
  lockrepeat := True;
  CurrentJD  := CurrentJD + EphStep;
  SetJDDate;
  RefreshMoonImage;
  EphTimer1.interval := 50;
  lockrepeat := False;
end;

procedure TForm1.Button3MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  EphStep    := +1;
  lockrepeat := False;
  ephtimer1timer(Sender);
  EphTimer1.interval := 1000;
  EphTimer1.Enabled  := True;
end;

procedure TForm1.Button3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  EphTimer1.Enabled := False;
end;

procedure TForm1.Button3MouseLeave(Sender: TObject);
begin
  EphTimer1.Enabled := False;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  AntiAlias:=CheckBox3.Checked;
end;

procedure TForm1.ComboBox6Change(Sender: TObject);
begin
  case ComboBox6.ItemIndex of
  0 : activemoon.SatelliteModel:='';
  1 : begin
      activemoon.SatelliteModel:=slash(Appdir)+slash('Models')+'lem.3ds';
      activemoon.SatelliteModelScale:=0.0006;
      activemoon.SatDirection(0,0,-1);
      activemoon.SatUp(-1,0,0);
      activemoon.SatPos(0,0,0);
      end;
  2 : begin
      activemoon.SatelliteModel:=slash(Appdir)+slash('Models')+'apollo.3ds';
      activemoon.SatelliteModelScale:=0.0000007;
      activemoon.SatDirection(1,0,0);
      activemoon.SatPos(0.003,0,0.005);
      end;
  end;
end;

procedure TForm1.FullScreen1Click(Sender: TObject);
begin
  SetFullScreen;
end;

procedure TForm1.Button6MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  EphStep    := +1 / 24;
  lockrepeat := False;
  ephtimer1timer(Sender);
  EphTimer1.interval := 1000;
  EphTimer1.Enabled  := True;
end;

procedure TForm1.Button7MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  EphStep    := -1 / 24;
  lockrepeat := False;
  ephtimer1timer(Sender);
  EphTimer1.interval := 1000;
  EphTimer1.Enabled  := True;
end;

procedure TForm1.Button8MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  EphStep    := -1;
  lockrepeat := False;
  ephtimer1timer(Sender);
  EphTimer1.interval := 1000;
  EphTimer1.Enabled  := True;
end;

procedure TForm1.ToolButton5Click(Sender: TObject);
begin
  if activemoon.zoom = 1 then
    activemoon.CenterAt(99999, 99999)
  else
    activemoon.CenterMark;
end;


{$ifdef windows}
procedure TForm1.SetFullScreen;
var lPrevStyle: LongInt;
begin
FullScreen:=not FullScreen;
if FullScreen then begin
   savetop:=top;
   saveleft:=left;
   savewidth:=width;
   saveheight:=height;
   lPrevStyle := GetWindowLong(handle, GWL_STYLE);
   SetWindowLong(handle, GWL_STYLE, (lPrevStyle And (Not WS_THICKFRAME) And (Not WS_BORDER) And (Not WS_CAPTION) And (Not WS_MINIMIZEBOX) And (Not WS_MAXIMIZEBOX)));
   SetWindowPos(handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED Or SWP_NOMOVE Or SWP_NOSIZE Or SWP_NOZORDER);
   PageControl1.Visible:=false;
   Splitter1.Visible:=false;
   ControlBar1.Visible:=false;
   StatusBar1.Visible:=false;
   Position1.Visible:=false;
   Notes1.Visible:=false;
   Distance1.Visible:=false;
   FullScreen1.Caption:=rsQuitfull;
   top:=0;
   left:=0;
   skipresize:=true;
   width:=screen.Width;
   skipresize:=true;
   height:=screen.Height;
   skipresize:=false;
end else begin
   lPrevStyle := GetWindowLong(handle, GWL_STYLE);
   SetWindowLong(handle, GWL_STYLE, (lPrevStyle Or WS_THICKFRAME Or WS_BORDER Or WS_CAPTION Or WS_MINIMIZEBOX Or WS_MAXIMIZEBOX));
   SetWindowPos(handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED Or SWP_NOMOVE Or SWP_NOSIZE Or SWP_NOZORDER);
   ControlBar1.Visible:=true;
   StatusBar1.Visible:=true;
   PageControl1.Visible:=true;
   Splitter1.Visible:=true;
   Position1.Visible:=true;
   Notes1.Visible:=true;
   Distance1.Visible:=true;
   FullScreen1.Caption:=rsFullScreen;
   top:=savetop;
   left:=saveleft;
   width:=savewidth;
   height:=saveheight;
end;
end;
{$endif}

{$ifdef unix}
procedure TForm1.SetFullScreen;
begin
FullScreen:=not FullScreen;
{$IF DEFINED(LCLgtk) or DEFINED(LCLgtk2)}
  skipresize:=true;
  SetWindowFullScreen(Form1,FullScreen);
  if FullScreen then begin
    PageControl1.Visible:=false;
    Splitter1.Visible:=false;
    ControlBar1.Visible:=false;
    StatusBar1.Visible:=false;
    Position1.Visible:=false;
    Notes1.Visible:=false;
    Distance1.Visible:=false;
    FullScreen1.Caption:=rsQuitfull;
  end
  else begin
    ControlBar1.Visible:=true;
    StatusBar1.Visible:=true;
    PageControl1.Visible:=true;
    Splitter1.Visible:=true;
    Position1.Visible:=true;
    Notes1.Visible:=true;
    Distance1.Visible:=true;
    FullScreen1.Caption:=rsFullScreen;
  end;
  skipresize:=false;
{$endif}
end;
{$endif}

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
//statusbar1.Panels[3].Text :='Down: '+inttostr(key);
case key of
  16  :  activemoon.KeyEvent(mkDown,key); // Shift
  17  :  activemoon.KeyEvent(mkDown,key); // Ctrl
  27  :  SetFullScreen; // Esc
  122 :  SetFullScreen; // F11
end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//statusbar1.Panels[3].Text :='Up: '+inttostr(key);
try
case key of
  16  :  activemoon.KeyEvent(mkUp,key); // Shift
  17  :  activemoon.KeyEvent(mkUp,key); // Ctrl
  107 :  if Shift=[ssCtrl] then IncreaseFont1Click(Sender);   //ctrl+
  109 :  if Shift=[ssCtrl] then DecreaseFont1Click(Sender);   //ctrl-
end;
except
end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := True;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ok:     boolean;
begin
  try
    SaveDefault;
    if CanCloseCDC and StartCDC then OpenCDC('','--quit');
    if CanCloseDatLun and StartDatLun then OpenDatLun('','-quit');
    if CanClosePhotLun and StartPhotlun then OpenPhotLun('','-quit');
    if scopelibok then
      if ScopeConnected then
      begin
        ScopeDisconnect(ok);
      end;
  except
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  try
    if moon2<>nil then begin
       moon2.close;
       moon2.Free;
    end;
    dblox.Free;
    dbnotes.Free;
    tz.Free;
    Fplanet.Free;
    overlayimg.Free;
    overlayhi.Free;
    searchlist.Free;
    param.Free;
    texturefiles.Free;
    if CursorImage1 <> nil then
    begin
      CursorImage1.Free;
    end;
  except
  end;
end;


procedure TForm1.ShowImg(desc, nom: string; forceinternal: boolean);
var
  buf, buf1: string;
  i, j: integer;
  jpg: Tjpegimage;
  bmp: Tbitmap;
begin
  chdir(appdir);
  buf  := slash(desc) + trim(nom);
  buf1 := '';
  if fileexists(buf) then
  begin
       if ima=nil then begin
          ima:=Tbigimaform.Create(application);
          ima.toolbutton1.caption:=imac1;
          ima.toolbutton2.caption:=imac2;
          ima.toolbutton3.caption:=imac3;
       end;
       if forceinternal then ima.zoom:=0;
       ima.image1.visible:=true;
       ima.toolbar1.visible:=true;
       ima.titre:=nom;
       ima.labeltext:=buf1;
       ima.Toolbar1.hint:=buf1;
       ima.LoadImage(buf);
       ima.Init;
       ima.Show;
    end;
end;

procedure TForm1.Apropos1Click(Sender: TObject);
begin
  ToolButton8.Down := False;
  splash := Tsplash.Create(application);
  splashunit.SplashTimer := False;
  splash.BorderStyle := bsToolWindow;
  splash.Caption := stringreplace(Apropos1.Caption, '&', '', []);
  ;
  splash.VersionName   := VersionName;
  splash.Splashversion := Splashversion;
  splash.transmsg      := transmsg;
  splash.Show;
  splash.refresh;
end;

procedure TForm1.ToolButton9Click(Sender: TObject);
begin
  activemoon.Zoom:=1;
end;

procedure TForm1.Edit1KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 13 then
    button1.Click;  //Enter
end;


procedure TForm1.TrackBar5Change(Sender: TObject);
var
  i: integer;
begin
  i := Trackbar5.position;
  if i = 5 then
    i := 720
  else if i = 4 then
    i := 360
  else if i = 3 then
    i := 180
  else if i = 2 then
    i := 90
  else if i = 1 then
    i := 45
  else
    i := 90;
  moon1.GLSphereMoon.Slices:=i;
  moon1.GLSphereMoon.Stacks:=i div 2;
  if moon2<>nil then begin
    moon2.GLSphereMoon.Slices:=i;
    moon2.GLSphereMoon.Stacks:=i div 2;
  end;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  if moon1=nil then exit;
  if (pagecontrol1.ActivePage = Reglage.Caption) then
  begin
    moon1.ShowFPS:=true;
    Label15.Caption     := rsm_44 + ' 0 FPS';
  end
  else
  begin
    moon1.ShowFPS:=false;
  end;

  if pagecontrol1.ActivePage = Terminateur.Caption then
  begin
    if currentphase <> tphase then
      UpdTerminateur;
  end;

  if pagecontrol1.ActivePage = Outils.Caption then
  begin
    RadioGroup2.Invalidate;
    if activemoon.MeasuringDistance then
      Button11.Caption      := rsm_53
    else
      Button11.Caption      := rsm_52;
  end
  else
  begin
    activemoon.MeasuringDistance := False;
  end;
end;

procedure TForm1.Stop1Click(Sender: TObject);
begin
  activemoon.SatelliteRotation:=0;
end;

procedure TForm1.EastWest1Click(Sender: TObject);
begin
  rotdirection := -rotdirection;
  activemoon.SatelliteRotation:=rotdirection*rotstep;
end;

procedure TForm1.N10seconde1Click(Sender: TObject);
begin
  combobox4.ItemIndex := 0;
  rotstep := 10;
  activemoon.SatelliteRotation:=rotdirection*rotstep;
end;

procedure TForm1.N5seconde1Click(Sender: TObject);
begin
  combobox4.ItemIndex := 1;
  rotstep := 5;
  activemoon.SatelliteRotation:=rotdirection*rotstep;
end;

procedure TForm1.N1seconde1Click(Sender: TObject);
begin
  combobox4.ItemIndex := 2;
  rotstep := 1;
  activemoon.SatelliteRotation:=rotdirection*rotstep;
end;

procedure TForm1.N05seconde1Click(Sender: TObject);
begin
  combobox4.ItemIndex := 3;
  rotstep := 0.5;
  activemoon.SatelliteRotation:=rotdirection*rotstep;
end;

procedure TForm1.N02seconde1Click(Sender: TObject);
begin
  combobox4.ItemIndex := 4;
  rotstep := 0.2;
  activemoon.SatelliteRotation:=rotdirection*rotstep;
end;

procedure TForm1.ComboBox4Change(Sender: TObject);
begin
if skiprot then exit;
  case combobox4.ItemIndex of
    0: rotstep := 10;
    1: rotstep := 5;
    2: rotstep := 1;
    3: rotstep := 0.5;
    4: rotstep := 0.2;
  end;
  activemoon.SatelliteRotation:=rotdirection*rotstep;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  rotdirection := 1;
  activemoon.SatelliteRotation:=rotdirection*rotstep;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  activemoon.SatelliteRotation:=0;
end;

procedure TForm1.SpeedButton7Click(Sender: TObject);
begin
 activemoon.SatelliteRotation:=rotdirection*MinSingle;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  rotdirection := -1;
  activemoon.SatelliteRotation:=rotdirection*rotstep;
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
activemoon.SatWest;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
activemoon.SatEast;
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
  activemoon.SatCenter;
end;

procedure TForm1.TrackBar6Change(Sender: TObject);
begin
if skiprot then exit;
  activemoon.SatelliteAltitude:=TrackBar6.Position;
  LabelAltitude.Caption:=inttostr(TrackBar6.Position) + blank + rsm_18;
end;

procedure TForm1.TrackBar7Change(Sender: TObject);
begin
if skiprot then exit;
  activemoon.SatInclination:=TrackBar7.Position;
  LabelIncl.Caption:=inttostr(TrackBar7.Position)+ ldeg;
end;

procedure TForm1.TrackBar8Change(Sender: TObject);
begin
  activemoon.SatViewDistance:=TrackBar8.Position/4;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  Updterminateur;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  i, p: integer;
begin
  i := listbox1.ItemIndex;
  if i >= 0 then
  begin
    Firstsearch := True;
    SearchText := listbox1.Items[i];
    p := pos(' ', SearchText) + 1;
    case RadioGroup1.ItemIndex of
      1: SearchText := copy(Searchtext, p, 999);
      2: SearchText := copy(Searchtext, p, 999);
      3: SearchText := copy(Searchtext, p, 999);
    end;
    SearchName(SearchText, True);
  end;
end;

procedure TForm1.GetSkychartInfo;
var
  inif: TMemIniFile;
  buf:  string;
begin
  CdCdir := '';
  // Try CdC V3
  if fileexists(CdCconfig) then
  begin
    inif := TMeminifile.Create(CdCconfig);
    try
      buf := inif.ReadString('main', 'AppDir', '');
      if DirectoryExists(buf) then
        CdCdir := buf;
    finally
      inif.Free;
    end;
    CdC := slash(CdCdir) + DefaultCdC;
    if not FileExists(CdC) then begin
       CdC :=ExpandFileName(slash(CdCdir)+slash('..')+slash('..')+slash('bin') + DefaultCdC);
       if not FileExists(CdC) then
          CartesduCiel1.Visible:=false;
    end;
    CdC:='"'+CdC+'"';
  end;
end;

procedure TForm1.CartesduCiel1Click(Sender: TObject);
begin
  OpenCdC('','');
end;

procedure TForm1.Aide2Click(Sender: TObject);
var
  fn: string;
begin
  fn := slash(HelpDir) + helpprefix + '_Index_Doc.pdf';
  if not FileExists(fn) then
  begin
    fn := slash(HelpDir) + helpprefix + '_Index_Doc.html';
    if not FileExists(fn) then
    begin
      fn := slash(HelpDir) + 'UK_Index_Doc.pdf';
      if not FileExists(fn) then
      begin
        fn := slash(HelpDir) + 'UK_Index_Doc.html';
      end;
    end;
  end;
  ExecuteFile(fn);
end;

procedure TForm1.Encyclopedia1Click(Sender: TObject);
var
  fn: string;
begin
  fn := slash(AppDir) + slash('Encyclopedia') + uplanguage + '_Encyclopedia.html';
  if not FileExists(fn) then
  begin
    fn := slash(AppDir) + slash('Encyclopedia') + 'EN_Encyclopedia.html';
  end;
  ExecuteFile(fn);
end;

procedure TForm1.Position1Click(Sender: TObject);
begin
  Pagecontrol1.ActivePage := Position.Caption;
  PageControl1Change(Sender);
  combobox1.SetFocus;
end;

procedure TForm1.OtherInstance(Sender : TObject; ParamCount: Integer; Parameters: array of String);
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

procedure TForm1.InstanceRunning(Sender : TObject);
begin
  UniqueInstance1.RetryOrHalt;
end;

procedure TForm1.Notes1Click(Sender: TObject);
begin
  Pagecontrol1.ActivePage := Notes.Caption;
  PageControl1Change(Sender);
end;

procedure TForm1.x21Click(Sender: TObject);
begin
  activemoon.zoom:=2;
end;

procedure TForm1.x41Click(Sender: TObject);
begin
  activemoon.zoom:=4;
end;

procedure TForm1.x81Click(Sender: TObject);
begin
  activemoon.zoom:=8;
end;

procedure TForm1.Button12MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if shift = [ssShift] then
    CameraOrientation := CameraOrientation - 1
  else if shift = [ssAlt] then
    CameraOrientation := CameraOrientation - 90
  else
    CameraOrientation := CameraOrientation - 15;
  CameraOrientation := rmod(CameraOrientation + 360, 360);
  activemoon.Orientation:=CameraOrientation;
  activemoon.RefreshAll;
end;

procedure TForm1.Button13MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if shift = [ssShift] then
    CameraOrientation := CameraOrientation + 1
  else if shift = [ssAlt] then
    CameraOrientation := CameraOrientation + 90
  else
    CameraOrientation := CameraOrientation + 15;
  CameraOrientation := rmod(CameraOrientation, 360);
  activemoon.Orientation:=CameraOrientation;
  activemoon.RefreshAll;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
if skiporient then exit;
  case RadioGroup2.ItemIndex of
    0: PoleOrientation := 0;
    1: PoleOrientation := 180;
  end;
  ToolButton4.Down := (RadioGroup2.ItemIndex = 1);
  if FollowNorth then
    CameraOrientation := rmod(-PA + PoleOrientation + 360, 360)
  else
    CameraOrientation := Poleorientation;
  activemoon.Poleorientation:=Poleorientation;
  activemoon.Orientation:=CameraOrientation;
  activemoon.RefreshAll;
end;

procedure TForm1.ToolButton4Click(Sender: TObject);
begin
  RadioGroup2.ItemIndex := abs(RadioGroup2.ItemIndex - 1);
  ToolButton4.Down      := (RadioGroup2.ItemIndex = 1);
end;

procedure TForm1.ToolButton6Click(Sender: TObject);
begin
  CheckBox2.Checked := not CheckBox2.Checked;
  ToolButton6.Down  := CheckBox2.Checked;
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  activemoon.MeasuringDistance := not activemoon.MeasuringDistance;
  if activemoon.MeasuringDistance then
  begin
    Button11.Caption      := rsm_53;
  end
  else
  begin
    Button11.Caption      := rsm_52;
  end;
end;

procedure TForm1.Distance1Click(Sender: TObject);
begin
  Pagecontrol1.ActivePage := Outils.Caption;
  PageControl1Change(Sender);
  Button11.Caption  := rsm_53;
  activemoon.MeasuringDistance := true;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
if skiporient then exit;
  FollowNorth := CheckBox1.Checked;
  if FollowNorth then
    CameraOrientation := rmod(-PA + PoleOrientation + 360, 360)
  else
    CameraOrientation := Poleorientation;
  activemoon.FollowNorth:=FollowNorth;
  activemoon.Orientation:=CameraOrientation;
  activemoon.RefreshAll;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  ToolButton6.Down := checkbox2.Checked;
  activemoon.Mirror:=checkbox2.Checked;
end;

procedure TForm1.ToolButton10Click(Sender: TObject);
begin
  listobject(5);
end;

procedure TForm1.BMP1Click(Sender: TObject);
var
  b: tbitmap;
begin
  savedialog1.DefaultExt := '.bmp';
  savedialog1.Filter     := 'bmp image|*.bmp';
  savedialog1.FileName   := combobox1.Text;
  if savedialog1.Execute then
  begin
    b := Tbitmap.Create;
    try
      activemoon.SnapShot(b, saveimagewhite);
      b.SaveToFile(ChangeFileExt(savedialog1.FileName, '.bmp'));
    finally
      b.Free;
    end;
  end;
end;

procedure TForm1.BMP15001Click(Sender: TObject);
// pour faire les images 2D.  Enlever les labels avant!
var
  b:  tbitmap;
begin
  savedialog1.DefaultExt := '.bmp';
  savedialog1.Filter     := 'bmp image|*.bmp';
  savedialog1.FileName   := combobox1.Text;
  if savedialog1.Execute then
  begin
    b := Tbitmap.Create;
    try
      activemoon.Rendertobitmap(b, 1500, False);
      b.SaveToFile(ChangeFileExt(savedialog1.FileName, '.bmp'));
    finally
      b.Free;
    end;
  end;
end;

procedure TForm1.BMP30001Click(Sender: TObject);
var
 b:  tbitmap;
begin
  savedialog1.DefaultExt := '.bmp';
  savedialog1.Filter     := 'bmp image|*.bmp';
  savedialog1.FileName   := combobox1.Text;
  if savedialog1.Execute then
  begin
    b := Tbitmap.Create;
    try
      activemoon.Rendertobitmap(b, 3000, False);
      b.SaveToFile(ChangeFileExt(savedialog1.FileName, '.bmp'));
    finally
      b.Free;
    end;
  end;
end;

procedure TForm1.JPG1Click(Sender: TObject);
var
  b: Tbitmap;
  j: Tjpegimage;
begin
  savedialog1.DefaultExt := '.jpg';
  savedialog1.Filter     := 'jpeg image|*.jpg';
  savedialog1.FileName   := combobox1.Text;
  if savedialog1.Execute then
  begin
    b := Tbitmap.Create;
    j := Tjpegimage.Create;
    try
      activemoon.SnapShot(b, saveimagewhite);
      j.Assign(b);
      j.CompressionQuality:=100;
      j.SaveToFile(ChangeFileExt(savedialog1.FileName, '.jpg'));
    finally
      b.Free;
      j.Free;
    end;
  end;
end;

procedure TForm1.Snapshot1Click(Sender: TObject);
var
  b:  Tbitmap;
  fn: string;
begin
  fn := 'snapshot.bmp';
  b  := Tbitmap.Create;
  try
    activemoon.SnapShot(b, False);
    b.SaveToFile(systoutf8(slash(tempdir) + fn));
    ShowImg(tempdir, fn, False);
  finally
    b.Free;
  end;
end;

procedure TForm1.Selectiondimprimante1Click(Sender: TObject);
begin
{$ifdef mswindows}
  PrinterSetupDialog1.execute;
{$endif}
{$ifdef unix}
  PrintDialog1.execute;
{$endif}
  GetPrinterResolution(PrtName, PrinterResolution);
end;

procedure TForm1.Imprimer1Click(Sender: TObject);
var
  i, w, ww, tl, hl, l, maxt: integer;
  xmin, xmax, ymin, ymax: integer;
  s:    double;
  b:    tbitmap;
  buf1: string;
begin
  GetPrinterResolution(PrtName, PrinterResolution);
  s := PrinterResolution / 300;
  Printer.Orientation := poPortrait;
  Printer.Title := currentname;
  Printer.BeginDoc;
  with Printer do
  begin
    xmin := round(LeftMargin * PrinterResolution / 25.4);
    xmax := PageWidth;
    ymin := 0;
    ymax := PageHeight;
    Canvas.Font.Name := memo2.Font.Name;
    Canvas.Font.Color := clBlack;
    Canvas.Font.Size := 8;
    hl   := round(Canvas.TextHeight('H') * 1.1);
    Canvas.Font.Style := [fsBold];
    buf1 := Caption+':  '+vmaurl;
    Canvas.TextOut(Xmin + 10, ymin, buf1);
    Canvas.Font.Style := [];
    if language = 'fr' then
      buf1 := 'Lunar formations database V2.1 '+cpyr+' Ch. Legrand,  Reproduction interdite / Pour usage personnel uniquement'
    else
      buf1 := 'Lunar formations database V2.1 '+cpyr+' Ch. Legrand,  Forbidden copy / For personal use only';
    i := (xmax - xmin - Canvas.TextWidth(buf1)) div 2;
    w := Canvas.TextHeight(buf1);
    maxt := ymax - 3 * w;
    Canvas.TextOut(Xmin + 10 + i, ymax - 2*w, buf1);
    w := 0;
    b := Tbitmap.Create;
    try
      memo2.Visible:=true;
      if PrintChart then
        // carte
        w := (xmax - xmin) * 2 div 3;
      activemoon.snapshot(b, True);
      canvas.StretchDraw(rect(xmin, ymin + hl, xmin + w, ymin + w + hl), b);
      if PrintDesc then
      begin
        ww := w;
        if (ww = 0) and PrintEph then
          ww := (xmax - xmin) * 2 div 3;
        Canvas.Font.Name := memo2.Font.Name;
        Canvas.Font.Color := clBlack;
        Canvas.Font.Size := 8;
        Canvas.Font.Style := [];
        Canvas.Brush.Style := bsClear;
        tl := ymin + ww + round(1.5 * hl);
        l  := Xmin + round(s * 10);
        memo2.Clear;
        dbm.Query('select * from moon where id=' + currentid);
        if dbm.RowCount > 0 then
          GetDetail(dbm.Results[0], memo2);  // we use a TMemo for the wordwrap capability
        for i := 1 to memo2.Lines.Count do
        begin
          if tl >= maxt then
          begin
            Canvas.TextOut(l, tl, 'Truncated ...');
            break;
          end;
          buf1 := memo2.Lines[i - 1];
          if (i = 1) or (copy(buf1, length(buf1), 1) = ':') then
            Canvas.Font.Style := [fsBold]
          else
            Canvas.Font.Style := [];
          Canvas.TextOut(l, tl, buf1);
          tl := tl + hl;
        end;
        memo2.Clear;
      end;
      if PrintEph then
      begin
        tl := ymin + 3 * hl;
        l  := round(s * 30) + xmin + w;
        Canvas.Font.Style := [fsBold];
        Canvas.TextOut(l, tl, Ephemerides.Caption);
        Canvas.Font.Style := [];
        tl := tl + hl;
        for i := 1 to Stringgrid1.RowCount do
        begin
          Canvas.TextOut(l, tl, Stringgrid1.Cells[0, i - 1] + ' ' + Stringgrid1.Cells[1, i - 1]);
          tl := tl + hl;
        end;
      end;
    finally
      b.Free;
      Printer.EndDoc;
      memo2.Visible:=false;
    end;
  end;
end;

procedure TForm1.Desc1HotClick(Sender: TObject);
begin
  ExecuteFile(desc1.HotURL);
end;

procedure TForm1.Copy1Click(Sender: TObject);
begin
case PageControl1.PageIndex of
0:  Desc1.CopyToClipboard;
1:  Memo1.CopyToClipboard;
2:  StringGrid1.CopyToClipboard;
end;
end;

procedure TForm1.SelectAll1Click(Sender: TObject);
begin
case PageControl1.PageIndex of
0:  Desc1.SelectAll;
1:  Memo1.SelectAll;
end;
end;

procedure TForm1.ToolButton7Click(Sender: TObject);
var
  param, fx, fy: string;
begin
  if flipx < 0 then
    fx := '1'
  else
    fx := '0';
  if poleorientation > 0 then
    fy := '1'
  else
    fy := '0';
  param := ' -fx ' + fx + ' -fy ' + fy;
  OpenPhotlun(currentname,param);
end;

procedure TForm1.OverlayCaption1Click(Sender: TObject);
var
  dir: string;
begin
  chdir(appdir);
  dir := slash('Textures') + slash('Overlay') + slash('caption');
  if fileexists(dir + overlayname) then
    showimg(dir, overlayname, True);
end;

procedure TForm1.Glossaire1Click(Sender: TObject);
begin
  if gloss = nil then
    gloss := TGloss.Create(application);
  gloss.button2.Caption := imac1;
  gloss.Caption := stringreplace(Glossaire1.Caption, '&', '', []);
  gloss.Show;
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
// rotation
var l,b: single;
    recenter:boolean;
begin
recenter:=activemoon.getcenter(l,b);
  if ToolButton3.Down then
  begin
    librl := 0;
    librb := 0;
    checkbox2.Visible := False;   //mirror
    ToolButton6.Enabled := False; //mirror
    paneltel.Visible := False;   // telescope
    PanelRot.Visible := False;   // rotation
    GroupBox3.Visible := True;    // satellite
    Rotation1.Visible := True;
    LibrationButton.Enabled := False;
    activemoon.LibrationMark:=False;
    activemoon.Mirror:=False;
    activemoon.VisibleSideLock:=false;
    activemoon.LibrLat:=0;
    activemoon.LibrLon:=0;
    activemoon.RefreshAll;
  end
  else
  begin
    checkbox2.Visible := True;
    ToolButton6.Enabled := True;
    PanelRot.Visible := True;
{$ifdef mswindows}
    paneltel.Visible := True;  // telescope
{$endif}
    case RadioGroup2.ItemIndex of
      0: CameraOrientation := 0;
      1: CameraOrientation := 180;
    end;
    GroupBox3.Visible := False;
    Rotation1.Visible := False;
    LibrationButton.Enabled := True;
    activemoon.VisibleSideLock:=true;
    activemoon.LibrationMark:=ShowLibrationMark;
    activemoon.SatelliteRotation:=0;
    activemoon.Orientation:=CameraOrientation;
    activemoon.Mirror:=checkbox2.Checked;
    RefreshMoonImage;
  end;
if recenter then activemoon.CenterAt(l,b);
end;

procedure TForm1.Button14Click(Sender: TObject);
begin
  moon1.ShowInfo;
end;

procedure TForm1.ZoomEyepieceClick(Sender: TObject);
begin
  with Sender as TMenuItem do
    CurrentEyepiece := tag;
  if CurrentEyepiece = 0 then
  begin
    activemoon.Eyepiece:=0;
  end
  else
  begin
    activemoon.Eyepiece:=eyepiecefield[CurrentEyepiece]/(diam/60);
    case eyepiecerotation[CurrentEyepiece] of
      1:
      begin
        RadioGroup2.ItemIndex := 0;
        RadioGroup2click(self);
      end;
      2:
      begin
        RadioGroup2.ItemIndex := 1;
        RadioGroup2click(self);
      end;
    end;
    case eyepiecemirror[CurrentEyepiece] of
      1:
      begin
        checkbox2.Checked := False;
        Checkbox2click(self);
      end;
      2:
      begin
        checkbox2.Checked := True;
        Checkbox2click(self);
      end;
    end;
  end;
end;

procedure TForm1.CheckBox8Click(Sender: TObject);
begin
  compresstexture := CheckBox8.Checked;
end;

procedure TForm1.InitTelescope;
{$ifdef mswindows}
var
  fs:   TSearchRec;
  i, p: integer;
  buf:  string;
{$endif}
begin
{$ifdef mswindows}
  i := findfirst(slash(appdir) + '*.tid', 0, fs);
  Combobox5.Clear;
  while i = 0 do
  begin
    buf := extractfilename(fs.Name);
    p   := pos('.tid', buf);
    buf := trim(copy(buf, 1, p - 1));
    Combobox5.items.Add(buf);
    i := findnext(fs);
  end;
  findclose(fs);
  if Combobox5.items.Count > 0 then begin
    paneltel.Visible := True;
    if scopeinterface <> '' then
      Combobox5.Text := scopeinterface
    else
      Combobox5.Text := Combobox5.items[0];
    scopeinterface := ComboBox5.Text;
  end;
{$endif}
end;

procedure TForm1.ComboBox5Change(Sender: TObject);
begin
  scopeinterface := ComboBox5.Text;
end;

// Scope menu
procedure TForm1.Button16Click(Sender: TObject);
var ok: boolean;
begin
  if not scopelibok then  begin  // initialistaion
    InitScopeLibrary(slash(appdir) + scopeinterface + '.tid');
    if scopelibok then
    begin
      ScopeReadConfig(ExtractFilePath(Configfile),ok);
    end;
  end;
  if scopelibok then
  begin
    ScopeSetObs(ObsLatitude, ObsLongitude);
    ScopeShow;
    geocentric      := False;
    librationeffect := True;
  end;
end;

// Goto button
procedure TForm1.Button17Click(Sender: TObject);
var
  ok1: boolean;
  r, d: single;
begin
  if scopelibok and scopeconnected and activemoon.GetMarkRaDec(r,d)  then
  begin
    ScopeGoto(rad2deg*r/15, rad2deg*d, ok1);
  end
  else
    ShowMessage('Please first connect the telescope and select a formation.');
  // add to translation!
end;

// Sync button
procedure TForm1.Button18Click(Sender: TObject);
var
  r, d: single;
begin
  if scopelibok and scopeconnected and activemoon.GetMarkRaDec(r,d) then
  begin
    ScopeAlign('markname', rad2deg*r/15, rad2deg*d);
  end
  else
    ShowMessage('Please first connect the telescope and select a formation.');
end;

// track
procedure TForm1.CheckBox6Click(Sender: TObject);
begin
  if not (scopelibok and scopeconnected) then
    CheckBox6.Checked := False
  else
  begin
    TelescopeTimer.Enabled  := CheckBox6.Checked;
    ToolButton3.Enabled     := not TelescopeTimer.Enabled;
  end;
  if CheckBox6.Checked then
  begin
    CheckBox7.Enabled := True;
    Edit5.Enabled     := True;
    Label26.Enabled   := True;
    Label28.Enabled   := True;
  end
  else
  begin
    CheckBox7.Checked := False;
    CheckBox7.Enabled := False;
    CheckBox7.Checked := False;
    Edit5.Enabled     := False;
    Label26.Enabled   := False;
    Label28.Enabled   := False;
  end;
end;

procedure TForm1.TelescopeTimerTimer(Sender: TObject);
var
  r, d: double;
  ok:  boolean;
begin
  ScopeGetRaDec(r, d, ok);
  if ok then
  begin
    initdate;
    RefreshMoonImage;
    activemoon.CenterAtRaDec(deg2rad*r*15,deg2rad*d);
    if CheckBox7.Checked and
      ((LastScopeTracking + trackdelay.position / 86400) <= now) then
    begin
      LastScopeTracking := now;
      Button17Click(Sender);
    end;
  end
  else
  begin
    TelescopeTimer.Enabled := False;
    CheckBox6.Checked      := False;
  end;
end;

procedure TForm1.NMClick(Sender: TObject);
begin
  CurrentJD := nmjd;
  SetJDDate;
  RefreshMoonImage;
end;

procedure TForm1.FQClick(Sender: TObject);
begin
  CurrentJD := fqjd;
  SetJDDate;
  RefreshMoonImage;
end;

procedure TForm1.FMClick(Sender: TObject);
begin
  CurrentJD := fmjd;
  SetJDDate;
  RefreshMoonImage;
end;

procedure TForm1.LQClick(Sender: TObject);
begin
  CurrentJD := lqjd;
  SetJDDate;
  RefreshMoonImage;
end;

procedure TForm1.RefreshPhase;
var
  jd0, hh:    double;
  aa, mm, dd: integer;
begin
  jd0 := jd(CurYear, 1, 1, 0.0);
  Fplanet.MoonPhases((phaseoffset / 12.3685) + CurYear +
    (CurrentJD - jd0) / 365.25, nmjd, fqjd, fmjd, lqjd);
  djd(nmjd + (GetJDTimeZone(nmjd) - DT_UT) / 24, aa, mm, dd, hh);
  labelnm.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);
  djd(fqjd + (GetJDTimeZone(fqjd) - DT_UT) / 24, aa, mm, dd, hh);
  labelfq.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);
  djd(fmjd + (GetJDTimeZone(fmjd) - DT_UT) / 24, aa, mm, dd, hh);
  labelfm.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);
  djd(lqjd + (GetJDTimeZone(lqjd) - DT_UT) / 24, aa, mm, dd, hh);
  labellq.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);
end;

procedure TForm1.prevMClick(Sender: TObject);
begin
  system.Dec(phaseoffset);
  RefreshPhase;
end;

procedure TForm1.nextMClick(Sender: TObject);
begin
  Inc(phaseoffset);
  RefreshPhase;
end;


procedure TForm1.NewWindowButtonClick(Sender: TObject);
begin
if moon2=nil then begin
 moon2:=Tf_moon.Create(PanelMoon2);
 moon2.GLSceneViewer1.Visible:=false;
 moon2.Caption:=Caption;
 moon2.Moon.Align:=alClient;
 moon2.onMoonClick:=MoonClickEvent;
 moon2.onGetLabel:=GetLabel;
 moon2.onGetSprite:=GetSprite;
 moon2.onMoonMove:=MoonMoveEvent;
 moon2.onMoonMeasure:=MoonMeasureEvent;
 moon2.onGetMsg:=GetMsg;
 moon2.PopUp:=PopupMenu1;
 moon2.Visible:=false;
 moon2.Init(false);
end;
if NewWindowButton.Down then begin
  SplitSize:=0.5;
  PanelMoon2.Width:=PanelMoon.Width div 2;
  Splitter2.Visible:=true;
  PanelMoon2.Visible:=true;
  wantbump:=false;
  moon2.AssignMoon(moon1);
  moon2.GLSceneViewer1.Visible:=true;
end else begin
  wantbump:=moon1.Bumpmap;
  PanelMoon2.Width:=0;
  Splitter2.Visible:=false;
  PanelMoon2.Visible:=false;
end;
end;

procedure TForm1.LibrationButtonClick(Sender: TObject);
begin
  librationeffect := LibrationButton.Down;
  RefreshMoonImage;
end;

procedure TForm1.PhaseButtonClick(Sender: TObject);
begin
  phaseeffect := PhaseButton.Down;
  if phaseeffect and wantbump and activemoon.CanBump then
     activemoon.Bumpmap:=true
  else
     activemoon.Bumpmap:=false;
  RefreshMoonImage;
end;

procedure TForm1.OpenDatlun(objname,otherparam:string);
var parm:string;
begin
    parm:='-nx ';
    if objname<>'' then parm:=parm+' -n "'+objname+'" ';
    parm:=parm+otherparam;
    chdir(appdir);
    Execnowait(datlun+' '+parm);
    StartDatlun:=true;
end;

procedure TForm1.OpenPhotlun(objname,otherparam:string);
var param:string;
begin
    param:='-nx ';
    if objname<>'' then param:=param+' -n "'+objname+'" ';
    param:=param+otherparam;
    chdir(appdir);
    Execnowait(photlun+' '+param);
    StartPhotlun:=true;
end;

procedure TForm1.OpenCDC(objname,otherparam:string);
var param:string;
begin
    param:='-nx --unique --nosplash ';
//    if objname<>'' then param:=param+' -n "'+objname+'" ';
    param:=param+otherparam;
    chdir(appdir);
    Execnowait(CdC+' '+param);
    StartCDC:=true;
end;

procedure TForm1.DataBase1Click(Sender: TObject);
begin
OpenDatlun('','');
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  if TObject(TPopupMenu(sender).parent) is Tf_moon then SetActiveMoon(Tf_moon(TPopupMenu(sender).parent));
  RemoveMark1.Visible := (CurrentSelection <> '');
end;

procedure TForm1.RemoveMark1Click(Sender: TObject);
begin
  CurrentSelection := '';
  activemoon.RefreshAll;
end;

procedure TForm1.MoonMeasureEvent(Sender: TObject; m1,m2,m3,m4: string);
begin
  edit1.Text := m1 + rsm_18;
  edit2.Text := m2;
  edit3.Text := m3;
  edit4.Text := m4;
end;

procedure TForm1.SetActiveMoon(mf: Tf_moon);
begin
ToolButton14.Enabled:=mf.SatelliteRotation=0;
if mf<>activemoon then begin
  activemoon:=Tf_moon(mf);
  checkbox2.Checked:=activemoon.Mirror;
  ToolButton3.Down:=not activemoon.VisibleSideLock;
  if ToolButton3.Down then
  begin
    checkbox2.Visible := False;   //mirror
    ToolButton6.Enabled := False; //mirror
    paneltel.Visible := False;   // telescope
    PanelRot.Visible := False;   // rotation
    GroupBox3.Visible := True;    // satellite
    Rotation1.Visible := True;
    LibrationButton.Enabled := False;
  end
  else
  begin
    checkbox2.Visible := True;
    ToolButton6.Enabled := True;
    PanelRot.Visible := True;
{$ifdef mswindows}
    paneltel.Visible := True;
{$endif}
    GroupBox3.Visible := False;
    Rotation1.Visible := False;
    LibrationButton.Enabled := True;
  end;
  if activemoon.MeasuringDistance then
  begin
    Button11.Caption:= rsm_53;
  end
  else
  begin
    Button11.Caption:= rsm_52;
  end;
  Poleorientation   := activemoon.Poleorientation;
  CameraOrientation := activemoon.Orientation;
  skiporient:=true;
  if PoleOrientation = 0 then
    RadioGroup2.ItemIndex := 0
  else
    RadioGroup2.ItemIndex := 1;
  ToolButton4.Down := (RadioGroup2.ItemIndex = 1);
  FollowNorth:=activemoon.FollowNorth;
  checkbox1.Checked := FollowNorth;
  skiporient:=false;
  phaseeffect:=activemoon.ShowPhase;
  PhaseButton.Down:=phaseeffect;
  librationeffect:=not((activemoon.LibrLat=0) and (activemoon.LibrLon=0));
  LibrationButton.Down:=librationeffect;
  GridButton.Down := activemoon.ShowGrid;
  ToolButton14.Down:= activemoon.ShowScale;
  if activemoon.CurrentName<>'' then begin
     CurrentName:=activemoon.CurrentName;
     Currentl:=rad2deg*activemoon.CurrentL;
     Currentb:=rad2deg*activemoon.CurrentB;
  end;
  skiprot:=true;
  if activemoon.SatelliteRotation<>0 then rotstep := abs(activemoon.SatelliteRotation);
  if rotstep=10 then combobox4.ItemIndex :=0
    else if rotstep=5 then combobox4.ItemIndex :=1
    else if rotstep=1 then combobox4.ItemIndex :=2
    else if rotstep=0.5 then combobox4.ItemIndex :=3
    else if rotstep=0.2 then combobox4.ItemIndex :=4
    else combobox4.ItemIndex :=2;
  TrackBar6.Position := round(activemoon.SatelliteAltitude);
  TrackBar7.Position := round(activemoon.SatInclination);
  skiprot:=false;
  showoverlay:=not (activemoon.Overlay='');
  overlayname:=activemoon.Overlay;
  overlaytr:=activemoon.OverlayTransparency;
  if showoverlay and (overlayname<>'') and fileexists(Slash(activemoon.OverlayPath) + slash('caption') + overlayname) then begin
     OverlayCaption1.Caption := remext(overlayname) + ' ' + rsCaption;
     OverlayCaption2.Caption := OverlayCaption1.Caption;
     OverlayCaption1.Visible := True;
     OverlayCaption2.Visible := True;
  end else begin
     OverlayCaption1.Visible := False;
     OverlayCaption2.Visible := False;
  end;
  CurrentJD:=activemoon.JD;
  SetJDDate;
  RefreshMoonImage;
  if pagecontrol1.ActivePage = Terminateur.Caption then
  begin
    if currentphase <> tphase then
      UpdTerminateur;
  end;
end;
end;

procedure TForm1.MoonClickEvent(Sender: TObject; Button: TMouseButton;
                     Shift: TShiftState; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single);
begin
if sender is Tf_moon then SetActiveMoon(Tf_moon(sender));
if button=mbLeft then begin
  if OnMoon then begin
     identLB(Rad2Deg*Lon,Rad2Deg*Lat);
     Tf_moon(Sender).SetMark(deg2rad*currentl,deg2rad*currentb,capitalize(currentname));
  end else begin
     Tf_moon(Sender).SetMark(0,0,'');
  end;
end;
end;

procedure TForm1.MoonMoveEvent(Sender: TObject; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single);
begin
if OnMoon then begin
  statusbar1.Panels[0].Text := rsm_10 + formatfloat(f1, Rad2Deg*Lon);
  statusbar1.Panels[1].Text := rsm_11 + formatfloat(f1, Rad2Deg*Lat);
end else begin
  statusbar1.Panels[0].Text := rsm_10;
  statusbar1.Panels[1].Text := rsm_11;
end;
end;

function SetWhiteColor(x: integer): TColor;
begin
  Result := x + (x shl 8) + (x shl 16);
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  moon1.AmbientColor := SetWhitecolor(Trackbar2.position);
  if moon2<>nil then moon2.AmbientColor := SetWhitecolor(Trackbar2.position);
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
  moon1.DiffuseColor := SetWhitecolor(Trackbar3.position);
  if moon2<>nil then moon2.DiffuseColor := SetWhitecolor(Trackbar3.position);
end;

procedure TForm1.TrackBar4Change(Sender: TObject);
begin
  moon1.SpecularColor := SetWhitecolor(Trackbar4.position);
  if moon2<>nil then moon2.SpecularColor := SetWhitecolor(Trackbar4.position);
end;

procedure TForm1.Button21Click(Sender: TObject);
begin
  TrackBar2.Position:=20;
  TrackBar2Change(Sender);
  TrackBar3.Position:=255;
  TrackBar3Change(Sender);
  TrackBar4.Position:=50;
  TrackBar4Change(Sender);
end;

procedure TForm1.LoadOverlay(fn: string; transparent: single);
begin
  if showoverlay and (fn<>'') and fileexists(Slash(moon1.OverlayPath) + fn) then
  begin
      activemoon.OverlayTransparency:=transparent;
      activemoon.OverlayTransparencyMethode:=0;
      activemoon.Overlay:=fn;
      if fileexists(Slash(activemoon.OverlayPath) + slash('caption') + fn) then
      begin
        OverlayCaption1.Caption := remext(fn) + ' ' + rsCaption;
        OverlayCaption2.Caption := OverlayCaption1.Caption;
        OverlayCaption1.Visible := True;
        OverlayCaption2.Visible := True;
      end
      else
      begin
        OverlayCaption1.Visible := False;
        OverlayCaption2.Visible := False;
      end;
  end
  else
  begin
    showoverlay := False;
    OverlayCaption1.Visible := False;
    OverlayCaption2.Visible := False;
    activemoon.Overlay:='';
  end;
end;

initialization
{$I virtualmoon1.lrs}

end.

