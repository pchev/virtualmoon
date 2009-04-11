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
  Windows, ShlObj, Registry,
{$endif}
  u_constant, u_util, cu_planet, u_projection, cu_tz, pu_moon,
  LCLIntf, Forms, StdCtrls, ExtCtrls, Graphics, Grids,
  GLScene, GLObjects, GLViewer, GLTexture, Info,
  GLcontext, GLCadencer, GLBitmapFont, GLHUDObjects, GLGraphics,
  GLColor, GLGraph, GLMirror, GLCrossPlatForm,
  GLGeomObjects,
  mlb2, PrintersDlgs, Printers, Controls, DateUtils,
  Messages, SysUtils, Classes, Dialogs,
  ComCtrls, Menus, Buttons, dynlibs, BigIma,
  EnhEdits, IniFiles, passql, passqlite,
  Math, CraterList, LResources, IpHtml, PairSplitter;

type

  { TForm1 }

  TForm1 = class(TForm)
    Desc1:   TIpHtmlPanel;
    FilePopup: TPopupMenu;
    HelpPopup: TPopupMenu;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PanelMoon: TPanel;
    PerfTimer: TTimer;
    Quitter1: TMenuItem;
    Panel2:  TPanel;
    PageControl1: TNoteBook;
    Position: TPage;
    Panel1:  TPanel;
    Button1: TButton;
    Button2: TButton;
    Ephemerides: TPage;
    Panel4:  TPanel;
    Label6:  TLabel;
    Label9:  TLabel;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    jour:    TLongEdit;
    mois:    TLongEdit;
    annee:   TLongEdit;
    seconde: TLongEdit;
    minute:  TLongEdit;
    heure:   TLongEdit;
    Button4: TButton;
    Button5: TButton;
    Apropos1: TMenuItem;
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
    Bevel3:  TBevel;
    Button12: TButton;
    Label22: TLabel;
    Button13: TButton;
    Bevel4:  TBevel;
    Label23: TLabel;
    Button11: TButton;
    Edit1:   TEdit;
    Edit2:   TEdit;
    Label24: TLabel;
    Label25: TLabel;
    Distance1: TMenuItem;
    CheckBox1: TCheckBox;
    RadioGroup2: TRadioGroup;
    CheckBox2: TCheckBox;
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
    LgendeGologique1: TMenuItem;
    StatusBar1: TStatusBar;
    LabelTimer: TTimer;
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
    GLScene1: TGLScene;
    GLLightSource1: TGLLightSource;
    Sphere4: TGLSphere;
    Sphere3: TGLSphere;
    Sphere2: TGLSphere;
    Sphere1: TGLSphere;
    Sphere5: TGLSphere;
    Sphere6: TGLSphere;
    HiresSphere: TGLSphere;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    HUDText1: TGLHUDText;
    HUDSprite1: TGLHUDSprite;
    DummyCube2: TGLDummyCube;
    HUDSprite2: TGLHUDSprite;
    GLMirror1: TGLMirror;
    DummyCube3: TGLDummyCube;
    DummyCube4: TGLDummyCube;
    ArrowLine1: TGLArrowLine;
    BitmapFont1: TGLBitmapFont;
    Sphere7: TGLSphere;
    Sphere8: TGLSphere;
    Annulus1: TGLAnnulus;
    Panel8:  TPanel;
    Label13: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    TrackBar5: TTrackBar;
    Button14: TButton;
    GroupBox2: TGroupBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    DummyCube1: TGLDummyCube;
    RotationCadencer: TGLCadencer;
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
    HiresSphere500: TGLSphere;
    OverlayCaption1: TMenuItem;
    OverlayCaption2: TMenuItem;
    GroupBox4: TGroupBox;
    ComboBox5: TComboBox;
    Button16: TButton;
    CheckBox6: TCheckBox;
    Button17: TButton;
    Button18: TButton;
    TelescopeTimer: TTimer;
    CheckBox7: TCheckBox;
    Label26: TLabel;
    Edit5:   TEdit;
    trackdelay: TUpDown;
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
    GLLightSource2: TGLLightSource;
    Label28: TLabel;
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
    DummyCube5: TGLDummyCube;
    RemoveMark1: TMenuItem;
    ButtonDatabase: TToolButton;
    CheckBox8: TCheckBox;
    ImageList1: TImageList;
    ToolButton12: TToolButton;
    procedure Desc1HotClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure PairSplitterSide2Resize(Sender: TObject);
    procedure Quitter1Click(Sender: TObject);
    procedure Configuration1Click(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure FormResize(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure Apropos1Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
    procedure PerfTimerTimer(Sender: TObject);
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
    procedure x21Click(Sender: TObject);
    procedure x41Click(Sender: TObject);
    procedure Button12MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Button13MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Button11Click(Sender: TObject);
    procedure Distance1Click(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: boolean);
    procedure CheckBox1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CartesduCiel1Click(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure BMP1Click(Sender: TObject);
    procedure SnapShot(var bmp: TBitmap; white: boolean);
    procedure RenderToBitmap(var bmp: TBitmap; size: integer; white: boolean);
    procedure JPG1Click(Sender: TObject);
    procedure Selectiondimprimante1Click(Sender: TObject);
    procedure Imprimer1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure LgendeGologique1Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure LabelTimerTimer(Sender: TObject);
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
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure RotationCadencerProgress(Sender: TObject;
      const deltaTime, newTime: double);
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
    procedure SpeedButton8Click(Sender: TObject);
    procedure RemoveMark1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure ZoomTimerTimer(Sender: TObject);
  private
    { Déclarations privées }
    moon1 : TF_moon;
    CursorImage1: TCursorImage;
    tz: TCdCTimeZone;
    ima: TBigImaForm;
    ToolsWidth: integer;
    procedure SetEyepieceMenu;
    procedure SetLang1;
    procedure SetLang;
    procedure InitObservatoire;
    procedure GetAppDir;
    function GetTimeZone(sdt: Tdatetime): double;
    procedure InitImages;
    procedure AddImages(dir, nom, cpy: string);
    procedure Moon2RaDec(x, y: double; var r, d: double);
    procedure RaDec2Moon(r, d: double; var x, y: double);
    procedure RefreshLabel;
    procedure ReadParam;
    procedure SetObs(param: string);
    procedure Readdefault;
    procedure SaveDefault;
    procedure UpdTerminateur;
    procedure AddToList(buf: string);
    procedure GetDetail(row: TResultRow; memo: Tmemo);
    procedure GetHTMLDetail(row: TResultRow; var txt: string);
    procedure GetNotes(n: string);
    function ImgExists(nom: string): boolean;
    procedure InitDate;
    procedure SetJDDate;
    procedure GetMsg(Sender: TObject; value: String);
    procedure ShowCoordinates(x, y: integer);
    procedure MeasureDistance(x, y: integer);
    procedure IdentLB(l, b: single);
    procedure IdentXY(x, y: integer);
    procedure InitLopamIdx;
    procedure ListUserDB;
    procedure ShowImg(desc, nom: string; forceinternal: boolean);
    procedure RefreshPhase;
    procedure SetDate(param: string);
    procedure SetDescText(const Value: string);
//    procedure SetMirror(onoff: boolean);
    procedure ShowSphere;
    procedure SetCompression(onoff: boolean);
    function ProjMoon(l, b, lc, bc: double; var X, Y: double): boolean;
    function InvProjMoon(x, y, lc, bc: double; var l, b: double): boolean;
    procedure InitLabel;
    procedure MoreSprite;
    procedure InitSprite;
    procedure ClearLabel;
    procedure Mark(x, y: double; txt: string);
    procedure LibrationMark(librl, librb: double);
    procedure SetScrollBar(xx, yy: extended);
    procedure MoveCamera(x, y: double);
    procedure SetMinFilter(onoff: boolean);
    procedure SetZoom(yy: double);
    procedure SetZoomBar;
    procedure Window2World(x, y: integer; var xx, yy: double);
    procedure World2Window(xx, yy: double; var x, y: integer);
    procedure SetCameraOrientation;
    procedure OrientLightSource(phase, sunincl: double);
    procedure resetorientation;
    procedure GetSkychartInfo;
    procedure MoonClickEvent(Sender: TObject; Button: TMouseButton;
                     Shift: TShiftState; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single);   public
    { Déclarations publiques }
    autolabelcolor: Tcolor;
    lastx, lasty, lastyzoom, posmin, posmax, ax, ay, MaxSprite: integer;
    ReduceTexture, ReduceTextureFar, LastIma, maximgdir, maxima, startx,
    starty, saveimagesize, lastscrollx, lastscrolly: integer;
    LeftMargin, PrintTextWidth, clickX, clickY: integer;
    PrintEph, PrintDesc, externalimage, PrintChart, lopamdirect, doublebuf,
    stencilbuf, hiresok, hires500ok: boolean;
    PicZoom: array of double;
    PicTop, PicLeft: array of integer;
    librl, librb, lrot, librlong, librlat, wheelstep, EphStep, fov, searchl,
    searchb, markx, marky, flipx, rotstep, lunaison: double;
    ra, Dec, rad, ded, dist, dkm, diam, phase, illum, pa, sunincl, currentphase,
    tphase, LabelSize, bx, by, bxpos, dummy: double;
    editrow, notesrow, hi_w, hi_wd, hi_dl, hi_mult, rotdirection, searchpos: integer;
    dbedited: boolean;
    SkipIdent, phaseeffect, geocentric, FollowNorth, notesok, notesedited,
    minilabel: boolean;
    lockmove, lockrepeat, lockrot, DDEreceiveok, showautolabel,
    showlibrationmark, marked, saveimagewhite, skippanelresize, skipresize: boolean;
    searchtext, imac1, imac2, imac3, lopamplateurl, lopamnameurl,
    lopamdirecturl, lopamlocalurl, lopamplatesuffix, lopamnamesuffix,
    lopamdirectsuffix, lopamlocalsuffix: string;
    externalimagepath, helpprefix, AntiAlias, ruklprefix, ruklsuffix,
    exitpassword, password, scopeinterface, markname, currentname, currentid: string;
    appname, pofile: string;
    multi_instance, CloseVMAbrowser, ClosePhotlun, CloseCdC: boolean;
    m:      array[1..nummessage] of string;
    shapepositionX, shapepositionY, CameraOrientation,
    PoleOrientation, startl, startb, startxx, startyy: double;
    curfoc, curx, cury: double;
    maxfoc, LabelDensity, overlaylum, phaseoffset: integer;
    minfoc: integer;
    perfdeltay: double;
    ddeparam, currenttexture, imgsuffix, overlayname, currentselection: string;
    CielHnd: Thandle;
    lockchart: boolean;
    StartedByDS: boolean;
    MeasuringDistance: boolean;
    distancestart: boolean;
    param:  TStringList;
    imgdir: array of array[0..2] of string;
    LONGIN, LATIN, WIDEKM, WIDEMI, LENGHTKM, LENGHTMI, FNAME, INTERESTN,
    DIAMINST, wordformat: integer;
    locktrackbar: boolean;
    lockscrollbar: boolean;
    hires, hires500, overlayhi, overlayimg: Tbitmap;
    fh, fh500: file;
    eyepiecename: array[1..10] of string;
    eyepiecefield, eyepiecemirror, eyepiecerotation: array[1..10] of integer;
    EyepieceRatio: double;
    zoom:   double;
    phasehash: boolean;
    phaseumbrachanging: boolean;
    phaseumbra: Tcolor;
    useDBN: integer;
    db_age: array[1..6] of integer;
    nmjd, fqjd, fmjd, lqjd, currentl, currentb: double;
    searchlist: TStringList;
    compresstexture: boolean;
    showoverlay: boolean;
    LastScopeTracking: double;
    UseComputerTime: boolean;
    procedure Init;
    procedure InitGraphic(Sender: TObject);
    procedure LoadOverlay(fn: string; lum: integer);
    procedure GetLabel(Sender: TObject);
    procedure SetLabel;
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

type
  TDBInfo = class(TObject)
    dbnum: integer;
  end;

implementation

uses telescope, config, splashunit,
  glossary, fmsg, dbutil;

procedure OpenHires(forcerebuild: boolean);
begin
  // no more used
end;

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
  section, buf: string;
  inifile:      Tmeminifile;
const
  deftxt = '?';
  blank  = '        ';
  b      = ' ';
begin
  language := 'UK';
  if fileexists(ConfigFile) then
    inifile := Tmeminifile.Create(ConfigFile)
  else
    inifile := Tmeminifile.Create(slash(AppDir) + 'virtualmoon.ini');
  with inifile do
  begin
    section := 'default';
    buf     := ReadString(section, 'Language', language);
  end;
  inifile.Free;
  chdir(appdir);
  if fileexists(Slash(AppDir) + slash('language') + 'lang_' + buf + '.ini') then
    language := buf;
  inifile    := Tmeminifile.Create(Slash(AppDir) + slash('language') + 'lang_' + language + '.ini');
  section    := 'default';
  with inifile do
  begin
    ldeg     := UTF8Encode(ReadString(section, 'degree', '°'));
    lmin     := UTF8Encode(ReadString(section, 'minute', ''''));
    lsec     := UTF8Encode(ReadString(section, 'second', '"'));
    transmsg := UTF8Encode(ReadString(section, 'translator', ''));
    Caption  := UTF8Encode(ReadString(section, 'title', 'Virtual Moon Atlas'));
  end;
  inifile.Free;
end;

procedure TForm1.SetLang;
var
  section, buf: string;
  inifile: Tmeminifile;
  i: integer;
const
  deftxt = '?';
  blank  = '        ';
  b      = ' ';

  function ReadStr(s, k, def: string): string;  // utf-8 translation
  begin
    Result := UTF8Encode(inifile.ReadString(s, k, def));
  end;

begin
  wordformat := 0;
  inifile    := Tmeminifile.Create(Slash(AppDir) + slash('language') + 'lang_' + language + '.ini');
  section    := 'default';
  with inifile do
  begin
    wordformat := ReadInteger(section, 'format', wordformat);
    helpprefix := ReadStr(section, 'help_prefix', 'UK');
    pofile     := ReadStr(section, 'lang_po_file', '');
    ToolButton1.Caption := '&' + ReadStr(section, 't_1', deftxt);
    quitter1.Caption := (ReadStr(section, 't_2', deftxt));
    ToolButton2.Caption := '&' + ReadStr(section, 't_3', deftxt);
    label10.Caption := (ReadStr(section, 't_4', deftxt));
    zoom1.Caption := label10.Caption;
    toolbutton5.hint := ReadStr(section, 't_5', deftxt);
    centre1.Caption := toolbutton5.hint;
    Position.Caption := (ReadStr(section, 't_6', deftxt));
    Position1.Caption := Position.Caption;
    Ephemerides.Caption := (ReadStr(section, 't_7', deftxt));
    Button1.Caption := (ReadStr(section, 't_10', deftxt));
    Button2.Caption := (ReadStr(section, 't_11', deftxt));
    Label9.Caption := (ReadStr(section, 't_12', deftxt));
    Label6.Caption := (ReadStr(section, 't_13', deftxt));
    Groupbox1.Caption := (ReadStr(section, 't_14', deftxt));
    toolbutton8.Caption := '&' + ReadStr(section, 't_15', deftxt);
    aide2.Caption := toolbutton8.Caption;
    Apropos1.Caption := (ReadStr(section, 't_16', deftxt));
    Button5.Caption := (ReadStr(section, 't_17', deftxt));
    Button4.Caption := (ReadStr(section, 't_113', Button4.Caption));
    ToolButton7.hint := ReadStr(section, 't_25', deftxt);
    Image2.Caption := ToolButton7.hint;
    Label14.Caption := (ReadStr(section, 't_32', deftxt));
    Label8.Caption := (ReadStr(section, 't_33', deftxt));
    Label11.Caption := (ReadStr(section, 't_34', deftxt));
    Label12.Caption := (ReadStr(section, 't_35', deftxt));
    Label16.Caption := (ReadStr(section, 't_36', deftxt));
    Label13.Caption := (ReadStr(section, 't_37', deftxt));
    Combobox2.Items.Clear;
    Combobox2.Items.Add(ReadStr(section, 't_42', deftxt));
    Combobox2.Items.Add(ReadStr(section, 't_43', deftxt));
    Combobox2.Items.Add(ReadStr(section, 't_44', deftxt));
    Combobox2.Items.Add(ReadStr(section, 't_45', deftxt));
    Combobox2.ItemIndex := 0;
    Terminateur.Caption := ReadStr(section, 't_51', deftxt);
    RadioGroup1.Caption := ReadStr(section, 't_46', deftxt);
    RadioGroup1.Items[0] := ReadStr(section, 't_47', deftxt);
    RadioGroup1.Items[1] := ReadStr(section, 't_48', deftxt);
    RadioGroup1.Items[2] := ReadStr(section, 't_49', deftxt);
    RadioGroup1.Items[3] := ReadStr(section, 't_50', deftxt);
    Reglage.Caption  := ReadStr(section, 't_55', deftxt);
    RadioGroup1.ItemIndex := 0;
    Label19.Caption  := RadioGroup1.Items[1];
    Label20.Caption  := RadioGroup1.Items[2];
    CartesduCiel1.Caption := (ReadStr(section, 't_56', deftxt));
    Outils.Caption   := (ReadStr(section, 't_63', deftxt));
    label22.Caption  := b + (ReadStr(section, 't_64', deftxt)) + b;
    Button12.Caption := (ReadStr(section, 't_65', deftxt));
    Button13.Caption := (ReadStr(section, 't_66', deftxt));
    RadioGroup2.Caption := (ReadStr(section, 't_67', deftxt));
    CheckBox1.Caption := (ReadStr(section, 't_68', deftxt));
    distance1.Caption := ReadStr(section, 't_69', deftxt);
    label23.Caption  := b + distance1.Caption + b;
    label24.Caption  := ReadStr(section, 't_70', deftxt);
    label25.Caption  := ReadStr(section, 't_71', deftxt);
    RadioGroup2.Items[0] := ReadStr(section, 't_72', deftxt);
    RadioGroup2.Items[1] := ReadStr(section, 't_73', deftxt);
    CheckBox2.Caption := ReadStr(section, 't_74', deftxt);
    Enregistrersous1.Caption := ReadStr(section, 't_78', deftxt);
    Imprimer1.Caption := ReadStr(section, 't_79', deftxt);
    ToolButton10.hint := ReadStr(section, 't_81', deftxt);
    Voisinage1.Caption := ToolButton10.hint;
    Selectiondimprimante1.Caption := ReadStr(section, 't_82', deftxt);
    LgendeGologique1.Caption := ReadStr(section, 't_83', deftxt);
    selectall1.Caption := ReadStr(section, 't_84', deftxt);
    copy1.Caption    := ReadStr(section, 't_85', deftxt);
    glossaire1.Caption := ReadStr(section, 't_98', deftxt);
    button15.Caption := ReadStr(section, 't_114', button15.Caption);
    Notes.Caption    := ReadStr(section, 't_115', notes.Caption);
    Notes1.Caption   := Notes.Caption;
    label3.Caption   := ReadStr(section, 't_116', label3.Caption);
    Eyepiece1.Caption := ReadStr(section, 't_109', Eyepiece1.Caption);
    e01.Caption      := ReadStr(section, 't_117', e01.Caption);
    groupbox2.Caption := ReadStr(section, 't_125', groupbox2.Caption);
    CheckBox3.Caption := ReadStr(section, 't_126', CheckBox3.Caption);
    CheckBox4.Caption := ReadStr(section, 't_127', CheckBox4.Caption);
    Database1.Caption := ReadStr(section, 't_129', Database1.Caption);
    Button14.Caption := ReadStr(section, 't_145', Button14.Caption);
    label1.Caption   := ReadStr(section, 't_153', label1.Caption);
    label2.Caption   := ReadStr(section, 't_154', label2.Caption);
    label21.Caption  := ReadStr(section, 't_155', label21.Caption);
    Encyclopedia1.Caption := ReadStr(section, 't_165', deftxt);
    Snapshot1.Caption := ReadStr(section, 't_167', Snapshot1.Caption);
    LibrationButton.hint := (ReadStr(section, 't_175', LibrationButton.Caption));
    PhaseButton.hint := (ReadStr(section, 't_176', PhaseButton.Caption));
    ToolButton4.hint := (ReadStr(section, 't_177', ToolButton4.Caption));
    ToolButton6.hint := (ReadStr(section, 't_178', ToolButton6.Caption));
    RemoveMark1.Caption := (ReadStr(section, 't_180', RemoveMark1.Caption));

    ButtonDatabase.hint := Database1.Caption;
    CheckBox8.Caption := ReadStr(section, 't_182', CheckBox8.Caption);
    Toolbutton12.hint := ReadStr(section, 't_183', deftxt);
    CheckBox6.Caption := (ReadStr(section, 't_156', deftxt));
    CheckBox7.Caption := (ReadStr(section, 't_157', deftxt));
    label28.Caption := (ReadStr(section, 't_158', deftxt));
    label26.Caption := (ReadStr(section, 't_159', deftxt));
    Groupbox4.Caption := (ReadStr(section, 't_160', deftxt));
    Button16.Caption := (ReadStr(section, 't_161', deftxt));
    Button17.Caption := (ReadStr(section, 't_162', deftxt));
    Button18.Caption := (ReadStr(section, 't_163', deftxt));
    Toolbutton3.hint := ReadStr(section, 't_134', Toolbutton3.Caption);
    Rotation1.Caption := ReadStr(section, 't_135', Rotation1.Caption);
    GroupBox3.Caption := Rotation1.Caption;
    Stop1.Caption := ReadStr(section, 't_136', Stop1.Caption);
    EastWest1.Caption := ReadStr(section, 't_137', EastWest1.Caption);
    buf := ReadStr(section, 't_138', 'd/second');
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
    Combobox4.ItemIndex := 1;
    Label4.Caption      := ReadStr(section, 't_139', Label4.Caption);
    SpeedButton5.Caption := ReadStr(section, 't_140', SpeedButton5.Caption);
    SpeedButton6.Caption := ReadStr(section, 't_141', SpeedButton6.Caption);
    SpeedButton4.Caption := ReadStr(section, 't_142', SpeedButton4.Caption);
    NewWindowButton.hint := ReadStr(section, 't_166', NewWindowButton.Caption);
    // Config
    with Form2 do
    begin
      Caption := form1.ToolButton2.Caption;
      if multi_instance then
        Caption      := 'Temporary change only!';
      label1.Caption := (ReadStr(section, 't_19', deftxt));
      label2.Caption := (ReadStr(section, 't_20', deftxt));
      label3.Caption := (ReadStr(section, 't_21', deftxt));
      CheckBox1.Caption := (ReadStr(section, 't_22', deftxt));
      CheckBox2.Caption := (ReadStr(section, 't_23', deftxt));
      label4.Caption := (ReadStr(section, 't_24', deftxt));
      Button1.Caption := ReadStr(section, 't_18', deftxt);
      CheckBox3.Caption := (ReadStr(section, 't_26', deftxt));
      CheckBox4.Caption := (ReadStr(section, 't_27', deftxt));
      Label5.Caption := (ReadStr(section, 't_28', deftxt));
      Label17.Caption := (ReadStr(section, 't_29', deftxt));
      RadioGroup1.Caption := (ReadStr(section, 't_38', deftxt));
      RadioGroup1.Items[0] := (ReadStr(section, 't_39', deftxt));
      RadioGroup1.Items[1] := (ReadStr(section, 't_40', deftxt));
      RadioGroup1.Items[2] := (ReadStr(section, 't_41', deftxt));
      RadioGroup3.Caption := ReadStr(section, 't_143', deftxt);
      RadioGroup3.Items[0] := RadioGroup1.Items[0];
      RadioGroup3.Items[1] := RadioGroup1.Items[1];
      RadioGroup3.Items[2] := RadioGroup1.Items[2];
      Label7.Caption := (ReadStr(section, 't_52', deftxt));
      CheckBox5.Caption := (ReadStr(section, 't_53', deftxt));
      CheckBox6.Caption := (ReadStr(section, 't_54', deftxt));
      TabSheet1.Caption := (ReadStr(section, 't_57', deftxt));
      TabSheet2.Caption := (ReadStr(section, 't_58', deftxt));
      label8.Caption := (ReadStr(section, 't_59', deftxt));
      label9.Caption := (ReadStr(section, 't_60', deftxt));
      stringgrid1.Cells[0, 0] := (ReadStr(section, 't_61', deftxt));
      stringgrid1.Cells[1, 0] := (ReadStr(section, 't_62', deftxt));
      Label11.Caption := (ReadStr(section, 't_75', deftxt));
      Label10.Caption := (ReadStr(section, 't_76', deftxt));
      Combobox4.Items[0] := (ReadStr(section, 't_77', deftxt));
      Combobox4.Text := Combobox4.Items[0];
      CheckBox10.Caption := ReadStr(section, 't_87', deftxt);
      CheckBox12.Caption := ReadStr(section, 't_89', deftxt);
      CheckBox7.Caption := ReadStr(section, 't_80', deftxt);
      Impression.Caption := ReadStr(section, 't_90', deftxt);
      Label15.Caption := ReadStr(section, 't_91', deftxt);
      Button2.Caption := form1.Reglage.Caption;
      Label12.Caption := ReadStr(section, 't_92', deftxt);
      Label13.Caption := ReadStr(section, 't_93', deftxt);
      Checkbox8.Caption := ReadStr(section, 't_94', deftxt);
      Checkbox9.Caption := ReadStr(section, 't_95', deftxt);
      Checkbox13.Caption := ReadStr(section, 't_96', deftxt);
      Label14.Caption := ReadStr(section, 't_97', deftxt);
      Button4.Caption := ReadStr(section, 't_99', deftxt);
      Label16.Caption := ReadStr(section, 't_100', deftxt);
      Tabsheet3.Caption := ReadStr(section, 't_101', deftxt);
      Checkbox14.Caption := ReadStr(section, 't_102', deftxt);
      Label6.Caption := ReadStr(section, 't_103', deftxt);
      Label18.Caption := ReadStr(section, 't_104', deftxt);
      Label19.Caption := ReadStr(section, 't_105', deftxt);
      TabSheet5.Caption := ReadStr(section, 't_109', TabSheet5.Caption);
      label24.Caption := ReadStr(section, 't_110', label24.Caption);
      label25.Caption := ReadStr(section, 't_111', label25.Caption);
      label26.Caption := ReadStr(section, 't_112', label26.Caption);
      button6.Caption := ReadStr(section, 't_113', button6.Caption);
      stringgrid2.Cells[0, 0] := ReadStr(section, 't_118', 'Eyepiece Name');
      stringgrid2.Cells[1, 0] := ReadStr(section, 't_119', 'Field in minutes');
      stringgrid2.Cells[2, 0] := '<->';
      stringgrid2.Cells[3, 0] := 'N/S';
      Checkbox17.Caption := ReadStr(section, 't_120', Checkbox17.Caption);
      Checkbox18.Caption := ReadStr(section, 't_121', Checkbox18.Caption);
      Checkbox19.Caption := ReadStr(section, 't_122', Checkbox19.Caption);
      Checkbox20.Caption := ReadStr(section, 't_123', Checkbox20.Caption);
      Checkbox21.Caption := ReadStr(section, 't_131', Checkbox21.Caption);
      Checkbox22.Caption := ReadStr(section, 't_168', Checkbox22.Caption);
      Checkbox23.Caption := ReadStr(section, 't_164', Checkbox23.Caption);
      Checkbox24.Caption := ReadStr(section, 't_187', Checkbox24.Caption);
      label28.Caption := ReadStr(section, 't_124', label28.Caption);
      GroupBox1.Caption := ReadStr(section, 't_129', GroupBox1.Caption);
      TabSheet7.Caption := GroupBox1.Caption;
      RadioGroup3.Caption := ReadStr(section, 't_143', deftxt);
      RadioGroup3.Items[0] := RadioGroup1.Items[0];
      RadioGroup3.Items[1] := RadioGroup1.Items[1];
      RadioGroup3.Items[2] := RadioGroup1.Items[2];
      label29.Caption := ReadStr(section, 't_146', label29.Caption);
      combobox1.items[0] := ReadStr(section, 't_147', combobox1.items[0]);
      combobox1.items[1] := ReadStr(section, 't_148', combobox1.items[1]);
      combobox1.ItemIndex := 0;
      combobox2.items[0] := ReadStr(section, 't_149', combobox2.items[0]);
      combobox2.items[1] := ReadStr(section, 't_150', combobox2.items[1]);
      combobox2.ItemIndex := 0;
      TabSheet4.Caption := ReadStr(section, 't_152', TabSheet4.Caption);
      RadioGroup2.Caption := ReadStr(section, 't_144', deftxt);
      TabSheet6.Caption := ReadStr(section, 't_169', TabSheet6.Caption);
      CheckBox11.Caption := ReadStr(section, 't_170', CheckBox11.Caption);
      label30.Caption := ReadStr(section, 't_171', label30.Caption);
      label32.Caption := ReadStr(section, 't_172', label32.Caption);
      Label23.Caption := ReadStr(section, 't_173', label23.Caption);
      CheckBox16.Caption := ReadStr(section, 't_174', CheckBox16.Caption);
      label31.Caption := ReadStr(section, 't_179', label31.Caption);
      label33.Caption := ReadStr(section, 't_181', label33.Caption);
      nooverlay.Caption := ReadStr(section, 't_184', label33.Caption);
      label34.Caption := ReadStr(section, 't_185', label33.Caption);

      RadioGroup2.items.Clear;
      RadioGroup2.items.add(ReadStr(section, 't_132', 'Aerograph'));
      RadioGroup2.items.add(ReadStr(section, 't_133', 'Clementine'));
      RadioGroup2.items.add(ReadStr(section, 't_186', 'Lopam'));
    end;
    imac1 := (ReadStr(section, 't_30', deftxt));
    imac2 := (ReadStr(section, 't_8', deftxt));
    imac3 := (ReadStr(section, 't_9', deftxt));
    for i := 1 to nummessage do
    begin
      m[i] := ReadStr(section, 'm_' + trim(IntToStr(i)), deftxt);
    end;
  end;
  inifile.Free;
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
var
  lt, st: TSystemTime;
begin
  if UseComputerTime then
  begin
    tz.Date := sdt;
    Result  := tz.SecondsOffset / 3600;
  end
  else
    Result := timezone;
end;

// image are now processed by Photlun but we keep the configuration also here for convenience
procedure TForm1.AddImages(dir, nom, cpy: string);
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
  addimages(slash(appdir) + 'Clementine', 'Clementine', '');
  addimages(slash(appdir) + 'Probes', 'Probes', '');
  addimages(slash(appdir) + 'LunaStars', 'LunaStars', '');
  addimages(slash(appdir) + 'CLA', 'CLA', '');
  addimages(slash(appdir) + 'LAC_LM', 'LAC_LM', '');
  addimages(slash(appdir) + 'ApolloMapping', 'Apollo Mapping Camera', '');
  addimages(slash(appdir) + 'BestOfAmateurs', 'Best of Amateurs', '');
  addimages(slash(appdir) + 'BestOfHiggins', 'Best of Higgins', '');
  addimages(slash(appdir) + 'BestOfLazzarotti', 'Best of Lazzarotti', '');
end;

procedure TForm1.Readdefault;
var
  inif:    TMemIniFile;
  section: string;
  i, j:    integer;
  smooth:  integer;
begin
  database[1]    := 'Nearside_Named_FR.csv';
  usedatabase[1] := True;
  usedatabase[2] := True;
  usedatabase[3] := True;
  usedatabase[4] := True;
  usedatabase[5] := True;
  usedatabase[6] := True;
  for i := 7 to maxdbn do
    usedatabase[i] := False;
  timezone := 0;
  Obslatitude := 46;
  Obslongitude := -6;
  ObsTZ    := 'Europe/Zurich';
  Obsaltitude := 0;
  ToolsWidth:=400;
  phaseeffect := True;
  librationeffect := True;
  geocentric := False;
  showoverlay := False;
  wheelstep := 1.05;
  marklabelcolor := clYellow;
  markcolor := clRed;
  autolabelcolor := clLime;
  labelcenter := True;
  minilabel := True;
  showlabel := True;
  showmark := True;
  currentselection := '';
  showlibrationmark := False;
  lockmove := False;
  maxima   := 2;
  LabelDensity := 600;
  LabelSize := 1;
  marksize := 5;
  saveimagesize := 0;
  saveimagewhite := False;
  ReduceTexture := 1;
  ReduceTextureFar := 1;
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
  MipMaps  := False;
  doublebuf := True;
  stencilbuf := False;
  antialias := 'Default';
  LopamDirect := False;
  ruklprefix := 'C:\rukl\';
  ruklsuffix := '_large.jpg';
  GeologicalMap := False;
  externalimage := False;
  externalimagepath := 'mspaint.exe';
  hiresfile := 'hires.jpg';
  imgsuffix := '';
  hiresok  := False;
  hires500ok := False;
  hi_w     := 11520;
  hi_dl    := 32;
  hi_wd    := 16;
  phaseumbra := $00484848;
  eyepiecename[1] := 'SCT 8" + Plossl 10mm';
  eyepiecefield[1] := 15;
  rotdirection := 1;
  rotstep  := 5;
  CloseVMAbrowser := False;
  ClosePhotlun := False;
  CloseCdC := False;
  smooth   := 180;
  if fileexists(ConfigFile) then
    inif := Tmeminifile.Create(ConfigFile)
  else
    inif := Tmeminifile.Create(slash(AppDir) + 'virtualmoon.ini');
  with inif do
  begin
    section     := 'images';
    LopamDirect := ReadBool(section, 'LopamDirect', LopamDirect);
    externalimage := ReadBool(section, 'ExternalSoftware', externalimage);
    externalimagepath := ReadString(section, 'ExternalSoftwarePath', externalimagepath);
    maximgdir   := ReadInteger(section, 'NumDir', 0);
    maxima      := ReadInteger(section, 'NumWindow', 2);
    saveimagesize := ReadInteger(section, 'SaveImageSize', saveimagesize);
    saveimagewhite := ReadBool(section, 'SaveImageWhite', saveimagewhite);
    setlength(imgdir, maximgdir);
    setlength(piczoom, maxima);
    setlength(pictop, maxima);
    setlength(picleft, maxima);
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
    for i := 1 to 6 do
      usedatabase[i] := ReadBool(section, 'UseDatabase' + IntToStr(i), usedatabase[i]);
    for i := 1 to 6 do
      db_age[i]  := ReadInteger(section, 'DB_Age' + IntToStr(i), 0);
    doublebuf    := ReadBool(section, 'DoubleBuffer', doublebuf);
    stencilbuf   := ReadBool(section, 'StencilBuffer', stencilbuf);
    compresstexture := ReadBool(section, 'compresstexture', compresstexture);
    antialias    := ReadString(section, 'AntiAlias', antialias);
    Obslatitude  := ReadFloat(section, 'Obslatitude', Obslatitude);
    Obslongitude := ReadFloat(section, 'Obslongitude', Obslongitude);
    if UseComputerTime then
      timezone := gettimezone(now)
    else
    begin
      timezone  := ReadFloat(section, 'TimeZone', timezone);
      CurrentJD := ReadFloat(section, 'CurrentJD', CurrentJD);
      dt_ut     := ReadFloat(section, 'dt_ut', dt_ut);
    end;
    cameraorientation := ReadFloat(section, 'CameraOrientation', CameraOrientation);
    phaseeffect  := ReadBool(section, 'PhaseEffect', phaseeffect);
    mipmaps      := ReadBool(section, 'MipMaps', mipmaps);
    GeologicalMap := ReadBool(section, 'GeologicalMap', GeologicalMap);
    librationeffect := ReadBool(section, 'LibrationEffect', librationeffect);
    phasehash    := ReadBool(section, 'PhaseHash', PhaseHash);
    phaseumbra   := ReadInteger(section, 'PhaseUmbra', PhaseUmbra);
    ShowLabel    := ReadBool(section, 'ShowLabel', ShowLabel);
    ShowMark     := ReadBool(section, 'ShowMark', ShowMark);
    ShowLibrationMark := ReadBool(section, 'ShowLibrationMark', ShowLibrationMark);
    MarkLabelColor   := ReadInteger(section, 'LabelColor', MarkLabelColor);
    MarkColor    := ReadInteger(section, 'MarkColor', MarkColor);
    AutolabelColor := ReadInteger(section, 'AutolabelColor', AutolabelColor);
    LabelDensity := ReadInteger(section, 'LabelDensity', LabelDensity);
    marksize     := ReadInteger(section, 'MarkSize', marksize);
    LabelSize    := ReadFloat(section, 'LabelSize', LabelSize);
    labelcenter  := ReadBool(section, 'LabelCenter', labelcenter);
    minilabel    := ReadBool(section, 'MiniLabel', minilabel);
    FollowNorth  := ReadBool(section, 'FollowNorth', FollowNorth);
    CheckBox2.Checked := ReadBool(section, 'Mirror', False);
    PoleOrientation := ReadFloat(section, 'PoleOrientation', PoleOrientation);
    ToolsWidth:=ReadInteger(section, 'ToolsWidth', ToolsWidth);
    PairSplitter1.Align:=alClient;
    PairSplitter1.Position:=ClientWidth-ToolsWidth;
    PageControl1.Width:=ToolsWidth;

    i := ReadInteger(section, 'Top', -1);
    if i > 0 then
    begin
      if multi_instance then
        i := i + 25;
      if (i >= -10) and (i < screen.Height - 20) then
        Top := i
      else
        Top := 0;
    end;

    i := ReadInteger(section, 'Left', -1);
    if i > 0 then
    begin
      if multi_instance then
        i := i + 25;
      if (i >= -10) and (i < screen.Width - 20) then
        Left := i
      else
        Left := 0;

      i := screen.Height - 50;
      if multi_instance then
        i := round(0.8 * i);
      i   := minintvalue([i, ReadInteger(section, 'Height', i)]);
      if (i >= 20) then
        Height := i;
      if multi_instance then
        ClientWidth := ClientHeight - controlbar1.Height - statusbar1.Height
      else
      begin
        i := screen.Width - 5;
        i := minintvalue([i, ReadInteger(section, 'Width', i)]);
        if (i >= 20) then
          Width := i;
      end;
    end;
    if (not multi_instance) and ReadBool(section, 'Maximized', False) then
      windowstate := wsMaximized;

    for j := 1 to maxima do
    begin
      i := ReadInteger(section, 'PicTop_' + IntToStr(j), -99);
      if (i >= -10) and (i < screen.Height - 20) then
        PicTop[j - 1] := i
      else
        PicTop[j - 1] := 20 * (j - 1);
      i := ReadInteger(section, 'PicLeft_' + IntToStr(j), -99);
      if (i >= -10) and (i < screen.Width - 20) then
        PicLeft[j - 1] := i
      else
        PicLeft[j - 1] := 20 * (j - 1);
      PicZoom[j - 1] := ReadFloat(section, 'PicZoom_' + IntToStr(j), 0);
    end;

    ReduceTexture := ReadInteger(section, 'ReduceTexture', ReduceTexture);
    ReduceTextureFar := ReadInteger(section, 'ReduceTextureFar', ReduceTextureFar);
    hiresfile := ReadString(section, 'hiresfile', hiresfile);
    for j := 1 to 10 do
    begin
      eyepiecename[j]     := ReadString(section, 'eyepiecename' + IntToStr(j), eyepiecename[j]);
      eyepiecefield[j]    := ReadInteger(section, 'eyepiecefield' + IntToStr(j), eyepiecefield[j]);
      eyepiecemirror[j]   := ReadInteger(section, 'eyepiecemirror' + IntToStr(j), eyepiecemirror[j]);
      eyepiecerotation[j] := ReadInteger(section, 'eyepiecerotation' + IntToStr(
        j), eyepiecerotation[j]);
    end;
    useDBN := ReadInteger(section, 'useDBN', useDBN);
    for i := 1 to useDBN do
      usedatabase[i] := ReadBool(section, 'UseDatabase' + IntToStr(i), usedatabase[i]);
    for i := 1 to useDBN do
      db_age[i] := ReadInteger(section, 'DB_Age' + IntToStr(i), 0);
    overlayname := ReadString(section, 'overlayname', 'Colors natural.jpg');
    overlaylum  := ReadInteger(section, 'overlaylum', 0);
    showoverlay := ReadBool(section, 'showoverlay', showoverlay);
    Geocentric  := ReadBool(section, 'Geocentric', Geocentric);
    if copy(hiresfile, 6, 1) <> '.' then
    begin
      i := pos('.', hiresfile);
      if i > 6 then
        imgsuffix := copy(hiresfile, 6, i - 6);
    end
    else
      imgsuffix := '';
    glscene1.BeginUpdate;
    SetCompression(compresstexture);
    if not doublebuf then
      GLSceneViewer1.Buffer.ContextOptions :=
        GLSceneViewer1.Buffer.ContextOptions - [roDoubleBuffer];
    if stencilbuf then
      GLSceneViewer1.Buffer.ContextOptions :=
        GLSceneViewer1.Buffer.ContextOptions + [roStencilBuffer];
    if antialias = '2x' then
      GLSceneViewer1.Buffer.AntiAliasing := aa2x
    else if antialias = '2xHQ' then
      GLSceneViewer1.Buffer.AntiAliasing := aa2xHQ
    else if antialias = '4x' then
      GLSceneViewer1.Buffer.AntiAliasing := aa4x
    else if antialias = '4xHQ' then
      GLSceneViewer1.Buffer.AntiAliasing := aa4xHQ
    else if antialias = 'None' then
      GLSceneViewer1.Buffer.AntiAliasing := aaNone;
    GLLightSource1.Ambient.AsWinColor :=
      ReadInteger(section, 'AmbientLight', GLLightSource1.Ambient.AsWinColor);
    GLLightSource1.Diffuse.AsWinColor :=
      ReadInteger(section, 'DiffuseLight', GLLightSource1.Diffuse.AsWinColor);
    GLLightSource1.Specular.AsWinColor :=
      ReadInteger(section, 'SpecularLight', GLLightSource1.Specular.AsWinColor);
    smooth := ReadInteger(section, 'Smooth', smooth);
    glscene1.EndUpdate;
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
  Showautolabel := showlabel and (Labeldensity < 1000);
  glscene1.BeginUpdate;
  if smooth > 120 then
    smooth := 180
  else if smooth > 60 then
    smooth := 90
  else
    smooth := 45;
  sphere1.Slices := smooth;
  sphere1.Stacks := sphere1.Slices;
  sphere2.Slices := sphere1.Slices;
  sphere2.Stacks := sphere1.Slices;
  sphere3.Slices := sphere1.Slices;
  sphere3.Stacks := sphere1.Slices;
  sphere4.Slices := sphere1.Slices;
  sphere4.Stacks := sphere1.Slices;
  sphere5.Slices := sphere1.Slices;
  sphere5.Stacks := sphere1.Slices;
  sphere6.Slices := sphere1.Slices;
  sphere6.Stacks := sphere1.Slices;
  sphere7.Slices := sphere1.Slices;
  sphere7.Stacks := sphere1.Slices;
  sphere8.Slices := sphere1.Slices;
  sphere8.Stacks := sphere1.Slices;
  case sphere1.Slices of
    45: smooth  := 16;
    90: smooth  := 32;
    180: smooth := 64;
  end;
  hiressphere.Slices    := smooth;
  hiressphere.Stacks    := smooth;
  hiressphere500.Slices := smooth;
  hiressphere500.Stacks := smooth;
  glscene1.EndUpdate;
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
      WriteString(section, 'Language', Language);
      WriteBool(section, 'DoubleBuffer', doublebuf);
      WriteBool(section, 'StencilBuffer', stencilbuf);
      WriteString(section, 'AntiAlias', antialias);
      WriteBool(section, 'MipMaps', mipmaps);
      WriteBool(section, 'LibrationEffect', librationeffect);
      WriteBool(section, 'compresstexture', compresstexture);
      WriteFloat(section, 'CameraOrientation', CameraOrientation);
      WriteInteger(section, 'useDBN', useDBN);
      for i := 1 to useDBN do
        WriteBool(section, 'UseDatabase' + IntToStr(i), usedatabase[i]);
      for i := 1 to useDBN do
        WriteInteger(section, 'DB_Age' + IntToStr(i), db_age[i]);
      WriteString(section, 'overlayname', overlayname);
      WriteInteger(section, 'overlaylum', overlaylum);
      WriteBool(section, 'showoverlay', showoverlay);
      for i := 1 to 6 do
        WriteBool(section, 'UseDatabase' + IntToStr(i), usedatabase[i]);
      for i := 1 to 6 do
        WriteInteger(section, 'DB_Age' + IntToStr(i), db_age[i]);
      WriteInteger(section, 'ReduceTextureFar', ReduceTextureFar);
      WriteString(section, 'hiresfile', hiresfile);
      WriteBool(section, 'Geocentric', Geocentric);
      WriteString(section, 'telescope', Combobox5.Text);
      WriteFloat(section, 'Obslatitude', Obslatitude);
      WriteFloat(section, 'Obslongitude', Obslongitude);
      WriteString(section, 'lang_po_file', pofile);
      WriteBool(section, 'UseComputerTime', UseComputerTime);
      WriteFloat(section, 'TimeZone', timezone);
      WriteFloat(section, 'CurrentJD', CurrentJD);
      WriteFloat(section, 'dt_ut', dt_ut);
      WriteBool(section, 'PhaseEffect', phaseeffect);
      WriteBool(section, 'GeologicalMap', GeologicalMap);
      WriteBool(section, 'PhaseHash', PhaseHash);
      WriteInteger(section, 'PhaseUmbra', PhaseUmbra);
      WriteBool(section, 'ShowLabel', ShowLabel);
      WriteBool(section, 'ShowMark', ShowMark);
      WriteBool(section, 'ShowLibrationMark', ShowLibrationMark);
      WriteInteger(section, 'LabelColor', MarkLabelColor);
      WriteInteger(section, 'MarkColor', MarkColor);
      WriteInteger(section, 'AutolabelColor', AutolabelColor);
      WriteInteger(section, 'LabelDensity', LabelDensity);
      WriteFloat(section, 'LabelSize', LabelSize);
      WriteInteger(section, 'MarkSize', marksize);
      WriteBool(section, 'LabelCenter', labelcenter);
      WriteBool(section, 'MiniLabel', minilabel);
      WriteBool(section, 'FollowNorth', FollowNorth);
      WriteBool(section, 'Mirror', CheckBox2.Checked);
      WriteFloat(section, 'PoleOrientation', PoleOrientation);
      WriteInteger(section, 'ToolsWidth', ToolsWidth);
      WriteInteger(section, 'Top', Top);
      WriteInteger(section, 'Left', Left);
      WriteInteger(section, 'Height', Height);
      WriteInteger(section, 'Width', Width);
      WriteBool(section, 'Maximized', (windowstate = wsMaximized));
      WriteInteger(section, 'ReduceTexture', ReduceTexture);
      for i := 1 to 10 do
      begin
        WriteString(section, 'eyepiecename' + IntToStr(i), eyepiecename[i]);
        WriteInteger(section, 'eyepiecefield' + IntToStr(i), eyepiecefield[i]);
        WriteInteger(section, 'eyepiecemirror' + IntToStr(i), eyepiecemirror[i]);
        WriteInteger(section, 'eyepiecerotation' + IntToStr(i), eyepiecerotation[i]);
      end;
      WriteInteger(section, 'AmbientLight', GLLightSource1.Ambient.AsWinColor);
      WriteInteger(section, 'DiffuseLight', GLLightSource1.Diffuse.AsWinColor);
      WriteInteger(section, 'SpecularLight', GLLightSource1.Specular.AsWinColor);
      WriteInteger(section, 'Smooth', sphere1.Slices);
      WriteInteger(section, 'ListCount', combobox1.Items.Count - 1);
      for i := 0 to combobox1.Items.Count - 1 do
      begin
        WriteString(section, 'List_' + IntToStr(i), combobox1.Items.Strings[i]);
      end;
      section := 'images';
      WriteBool(section, 'LopamDirect', LopamDirect);
      WriteBool(section, 'ExternalSoftware', externalimage);
      WriteString(section, 'ExternalSoftwarePath', externalimagepath);
      WriteInteger(section, 'NumWindow', maxima);
      WriteInteger(section, 'NumDir', maximgdir);
      WriteInteger(section, 'SaveImageSize', saveimagesize);
      WriteBool(section, 'SaveImageWhite', saveimagewhite);
      for i := 1 to maximgdir do
      begin
        WriteString(section, 'dir' + IntToStr(i), imgdir[i - 1, 0]);
        WriteString(section, 'name' + IntToStr(i), imgdir[i - 1, 2]);
      end;
      WriteString(section, 'ruklprefix', ruklprefix);
      WriteString(section, 'ruklsuffix', ruklsuffix);
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

procedure TForm1.Moon2RaDec(x, y: double; var r, d: double);
var
  spa, cpa, s: extended;
begin
  sincos(deg2rad * pa, spa, cpa);
  s := diam / 3600;
  r := rad + (cpa * x + spa * y) * s / 15 / cos(deg2rad * ded);
  d := ded + (cpa * y - spa * x) * s;
end;

procedure TForm1.RaDec2Moon(r, d: double; var x, y: double);
var
  spa, cpa, s: extended;
begin
  sincos(-deg2rad * pa, spa, cpa);
  s := diam / 3600;
  r := (r - rad) * 15 * cos(deg2rad * ded) / s;
  d := (d - ded) / s;
  x := cpa * r + spa * d;
  y := cpa * d - spa * r;
end;

procedure TForm1.GetLabel(Sender: TObject);
begin
//  database select +
moon1.AddLabel(degtorad(-20),degtorad(9.7),'Copernicus');
moon1.AddLabel(degtorad(-11.2),degtorad(-43.3),'Tycho');
moon1.AddLabel(degtorad(-39.9),degtorad(-17.5),'Gassendi');
moon1.AddLabel(degtorad(-47.4),degtorad(23.7),'Aristarchus');
moon1.AddLabel(degtorad(-9.3),degtorad(51.6),'Plato');
moon1.AddLabel(degtorad(29.9),degtorad(31.8),'Posidonius');
moon1.AddLabel(degtorad(26.4),degtorad(-11.4),'Theophilus');
end;

procedure Tform1.SetLabel;
var
  l, b, l1, b1, deltab, deltal, x, y, w, wmin, wfact: double;
  xx, yy, j: integer;
  miniok:    boolean;
  nom, let:  string;
  curlab, cursprite, i: integer;
begin
  curlab := 0;
  xx     := form1.GLSceneViewer1.Width div 2;
  yy     := form1.GLSceneViewer1.Height div 2;
  Window2World(xx, yy, x, y);
  if not InvProjMoon(2 * x, 2 * y, librl, librb, l, b) then
  begin
    l      := librl;
    b      := librb;
    deltab := 90;
    deltal := 90;
  end
  else
  begin
    deltab := 90 / zoom;
    deltal := deltab / cos(deg2rad * b);
  end;
  if abs(l) > 90 then
  begin // face cachee
    l      := 0;
    deltal := 180;
  end;
  if minilabel then
    wfact := 0.5
  else
    wfact := 1;
  w := maxfoc / minfoc;
  LabelDensity := maxintvalue([100, LabelDensity]);
  if (zoom > 3) and (zoom >= w) then
    wmin := -1
  else
    wmin := MinValue([650.0, 1.5 * LabelDensity / (zoom * zoom)]);
  if showlabel then
  begin
    dbm.Query('select NAME,LONGIN,LATIN,WIDEKM,WIDEMI,LENGTHKM,LENGTHMI from moon' +
      ' where DBN in (' + sidelist + ')' + ' and LONGIN > ' +
      formatfloat(f2, l - deltal) + ' and LONGIN < ' + formatfloat(f2, l + deltal) +
      ' and LATIN > ' + formatfloat(f2, b - deltab) +
      ' and LATIN < ' + formatfloat(f2, b + deltab) +
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
      nom := dbm.Results[j][0];
      if marked and (form1.hudtext1.Text = nom) then
        continue;
      if projMoon(l1, b1, librl, librb, x, y) then
      begin
        nom := trim(nom);
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
        world2window(x, y, xx, yy);
        if (xx > 0) and (yy > 0) and (xx < form1.GLSceneViewer1.Width) and
          (yy < form1.GLSceneViewer1.Height) and ((currenteyepiece = 0) or
          (sqrt(Intpower(form1.GLSceneViewer1.Width / 2 - xx, 2) + Intpower(
          form1.GLSceneViewer1.Height / 2 - yy, 2)) < 0.475 * form1.GLSceneViewer1.Width)) then
        begin
          with form1.DummyCube4.Children[curlab] as TGLHUDText do
          begin
            Visible    := True;
            Position.Y := yy;
            Position.X := xx;
            if labelcenter then
            begin
              Text      := nom;
              Alignment := taCenter;
            end
            else
            begin
              Text      := '.' + nom;
              Alignment := taLeftJustify;
            end;
          end;
          Inc(curlab);
          if curlab > Maxlabel then
            break;
        end;
      end;
    end;
  end;
  // Mark selection
  cursprite := 0;
  if currentselection <> '' then
  begin
    dbm.Query('select LONGIN,LATIN from moon where ' + currentselection +
      ' and LONGIN > ' + formatfloat(f2, l - deltal) +
      ' and LONGIN < ' + formatfloat(f2, l + deltal) +
      ' and LATIN > ' + formatfloat(f2, b - deltab) +
      ' and LATIN < ' + formatfloat(f2, b + deltab) +
      ' ORDER BY WIDEKM DESC ' + ' ;');
    for j := 0 to dbm.RowCount - 1 do
    begin
      l1 := dbm.Results[j].Format[0].AsFloat;
      b1 := dbm.Results[j].Format[1].AsFloat;
      if projMoon(l1, b1, librl, librb, x, y) then
      begin
        world2window(x, y, xx, yy);
        if (xx > 0) and (yy > 0) and (xx < form1.GLSceneViewer1.Width) and
          (yy < form1.GLSceneViewer1.Height) and ((currenteyepiece = 0) or
          (sqrt(Intpower(form1.GLSceneViewer1.Width / 2 - xx, 2) + Intpower(
          form1.GLSceneViewer1.Height / 2 - yy, 2)) < 0.475 * form1.GLSceneViewer1.Width)) then
        begin
          with form1.DummyCube5.Children[cursprite] as TGLHUDSprite do
          begin
            Width      := marksize;
            Height     := marksize;
            Visible    := True;
            Position.Y := yy;
            Position.X := xx;
          end;
          Inc(cursprite);
          if cursprite >= MaxSprite then
            if cursprite < AbsoluteMaxSprite then
              MoreSprite
            else
              break;
        end;
      end;
    end;
  end;

  for i := curlab to Maxlabel do
    with DummyCube4.Children[i] as TGLHUDText do
      Visible := False;
  for i := cursprite to MaxSprite - 1 do
    with DummyCube5.Children[i] as TGLHUDSprite do
      Visible := False;
end;

procedure TForm1.RefreshLabel;
begin
  ClearLabel;
  LabelTimer.Enabled := False;
  LabelTimer.Enabled := True;
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
  decodetime(hh / 24, h, n, s, ms);
  timezone := gettimezone(encodedatetime(y, m, d, h, n, s, ms));
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

procedure TForm1.ReadParam;
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
        MoveCamera(x, y);
      end
      else if param[i] = '-f' then
      begin   // focal
        Inc(i);
        buf := trim(StringReplace(param[i], '"', '', [rfReplaceAll]));
        x   := strtofloat(buf);
        setzoom(x);
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
        ToolButton3Click(form1);
        lrot := 0;
      end
      else if param[i] = '-r' then
      begin   // full globe at give rotation
        Inc(i);
        buf := trim(StringReplace(param[i], '"', '', [rfReplaceAll]));
        x   := strtofloat(buf);
        if x <> 0 then
        begin
          ToolButton3.Down := True;
          ToolButton3Click(form1);
          lrot := x;
          GLSceneViewer1MouseMove(nil, [ssLeft], lastx, lasty);
        end;
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
  buf, buf1: string;
{$ifdef darwin}
  i:      integer;
{$endif}
{$ifdef win32}
  PIDL:   PItemIDList;
  Folder: array[0..MAX_PATH] of char;
const
  CSIDL_PERSONAL = $0005;   // My Documents
  CSIDL_APPDATA  = $001a;   // <user name>\Application Data
  CSIDL_LOCAL_APPDATA = $001c;
  // <user name>\Local Settings\Applicaiton Data (non roaming)
{$endif}
begin
{$ifdef darwin}
  appdir := getcurrentdir;
  if not DirectoryExists(slash(appdir) + slash('data') + slash('planet')) then
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
  privatedir := expandfilename(PrivateDir);
  configfile := expandfilename(Defaultconfigfile);
  CdCconfig  := ExpandFileName(DefaultCdCconfig);
{$endif}
{$ifdef win32}
  SHGetSpecialFolderLocation(0, CSIDL_LOCAL_APPDATA, PIDL);  // cdc in local appdata
  SHGetPathFromIDList(PIDL, Folder);
  buf1 := trim(Folder);
  SHGetSpecialFolderLocation(0, CSIDL_APPDATA, PIDL);        // vma in appdata
  SHGetPathFromIDList(PIDL, Folder);
  buf := trim(Folder);
  if buf1 = '' then
    buf1     := buf;  // old windows version
  privatedir := slash(buf) + privatedir;
  configfile := Defaultconfigfile;
  configfile := slash(privatedir) + configfile;
  CdCconfig  := slash(buf1) + DefaultCdCconfig;
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
  Photlun := slash(appdir) + DefaultPhotlun;     // Photlun normally at same location as vma
  if not FileExists(Photlun) then
    Photlun := DefaultPhotlun; // if not try in $PATH
  Datlun    := slash(appdir) + DefaultDatlun;
  if not FileExists(Datlun) then
    Datlun := DefaultDatlun;
  helpdir  := slash(appdir) + slash('doc');
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
          'If it is not installed at a standard location create a logical link zoneinfo in skychart data directory.',
          mtError, [mbAbort], 0);
        Halt;
      end;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  decimalseparator := '.';
  dbedited  := False;
  minfoc    := 100;
  perfdeltay := 0.00001;
  lockchart := False;
  StartedByDS := False;
  MeasuringDistance := False;
  distancestart := False;
  locktrackbar := False;
  lockscrollbar := False;
  CurrentEyepiece := 0;
  EyepieceRatio := 1;
  zoom      := 1;
  phasehash := False;
  phaseumbrachanging := False;
  useDBN    := 6;
  compresstexture := False;
  showoverlay := True;
  LastScopeTracking := 0;
  UseComputerTime := True;
  AsMultiTexture := False;
  GetAppDir;
  skipresize := True;
  skipresize := False;
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
  tz    := TCdCTimeZone.Create;
  CursorImage1 := TCursorImage.Create;
  hires := Tbitmap.Create;
  hires500 := Tbitmap.Create;
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
 moon1.Moon.Align:=alClient;
 moon1.onMoonClick:=MoonClickEvent;
 moon1.onGetMsg:=GetMsg;
 moon1.onGetLabel:=GetLabel;
 moon1.TexturePath:=slash(appdir)+slash('Textures');
 moon1.OverlayPath:=slash(appdir)+slash('Textures')+slash('Overlay');
  SetLang1;
  readdefault;
  currentid := '';
  librl     := 0;
  librb     := 0;
  lrot      := 0;
  zoom      := 1;
  lastx     := 0;
  lasty     := 0;
  SkipIdent := False;
  dblox     := TMlb2.Create;
  dbnotes   := TMlb2.Create;
  GetSkyChartInfo;
  CartesduCiel1.Visible := CdCdir > '';
  InitGraphic(Sender);
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
  if multi_instance then
  begin
    NewWindowButton.Visible := False;
    Button15.Visible := False;
    Caption := Caption + '<2>';
  end;
  AsMultiTexture := (GLSceneViewer1.Buffer.LimitOf[limNbTextureUnits] > 1);
end;

procedure TForm1.UpdTerminateur;
var
  interest, diam, i: integer;
  l1, l2: double;
  buf:    string;
begin
  listbox1.Clear;
  if tphase < 180 then
  begin
    l1 := tphase - 90 + librl;
    l2 := l1 + 12;
  end
  else
  begin
    l2 := tphase - 90 + librl - 180;
    l1 := l2 - 12;
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
      listbox1.Items.Add(m[27]);
  end;
end;

procedure Tform1.ListObject(delta: double);
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
  begin
    result:=row.ByField[fn].AsString;
//    result:=UTF8Encode(row.ByField[fn].AsString);
  end;
begin
  nom := GetField('NAME');
  memo.Lines.Add(nom);
  i := row.ByField['DBN'].AsInteger;
  if i > 9 then
  begin
    buf := m[75] + b + IntToStr(i) + b;
    for j := 0 to form2.CheckListBox1.Count - 1 do
      if (form2.CheckListBox1.Items.Objects[j] as TDBinfo).dbnum = i then
        buf := buf + form2.CheckListBox1.Items[j];
    memo.Lines.Add(buf);
  end;
  memo.Lines.Add(m[56] + b + GetField('TYPE'));
  memo.Lines.Add(m[49] + b + GetField('PERIOD'));
  memo.Lines.Add(m[57]); //Taille
  memo.Lines.Add(m[17] + b + GetField('LENGTHKM') + 'x' +
    GetField('WIDEKM') + m[18] + b + '/' + b + GetField('LENGTHMI') +
    'x' + GetField('WIDEMI') + m[19]);
  buf  := GetField('HEIGHTM');
  buf2 := GetField('HEIGHTFE');
  if buf = buf2 then
    txt := m[20] + b + buf  // inconnue
  else
  begin
    txt := m[20] + b;
    val(buf, dummy, i);
    if i = 0 then
      txt := txt + buf + m[21] + b + '/' + b;
    val(buf2, dummy, i);
    if i = 0 then
      txt := txt + buf2 + m[22];
  end;
  memo.Lines.Add(txt);
  memo.Lines.Add(m[23] + b + GetField('RAPPORT'));
  memo.Lines.Add(m[58]); //Description
  if GetField('GENERAL') > '' then
    memo.Lines.Add(GetField('GENERAL'));
  if GetField('SLOPES') > '' then
    memo.Lines.Add(GetField('SLOPES'));
  if GetField('WALLS') > '' then
    memo.Lines.Add(GetField('WALLS'));
  if GetField('FLOOR') > '' then
    memo.Lines.Add(GetField('FLOOR'));
  memo.Lines.Add(m[59]); //Observation
  memo.Lines.Add(m[24] + b + GetField('INTERESTC'));
  buf  := GetField('MOONDAYS');
  buf2 := GetField('MOONDAYM');
  if buf = buf2 then
    txt := m[25] + b + buf
  else
    txt := m[25] + b + buf + b + m[26] + b + buf2;
  memo.Lines.Add(txt);
  memo.Lines.Add(m[28] + b + GetField('PRINSTRU'));
  memo.Lines.Add(m[60]); //Position
  memo.Lines.Add(m[10] + b + GetField('LONGIC'));
  memo.Lines.Add(m[11] + b + GetField('LATIC'));
  memo.Lines.Add(m[12] + b + GetField('QUADRANT'));
  memo.Lines.Add(m[13] + b + GetField('AREA'));
  memo.Lines.Add(m[61]); //Atlas
  memo.Lines.Add(m[14] + b + GetField('RUKL') + ' ' + GetField('RUKLC'));
  buf := GetField('VISCARDY');
  if trim(buf) > '' then
    memo.Lines.Add(m[15] + b + buf);
  buf := GetField('HATFIELD');
  if trim(buf) > '' then
    memo.Lines.Add(m[16] + b + buf);
  buf := GetField('WESTFALL');
  if trim(buf) > '' then
    memo.Lines.Add(m[66] + b + buf);
  buf := GetField('WOOD');
  if trim(buf) > '' then
    memo.Lines.Add(m[72] + b + buf);
  dblox.Gofirst;
  ok := dblox.MatchData('NAME', '=', nom);
  if not ok then
    ok := dblox.SeekData('NAME', '=', nom);
  if ok then
  begin
    buf := m[65];
    while ok do
    begin
      carte := dblox.GetDATA('PLATE');
      buf   := buf + b + carte;
      ok    := dblox.SeekData('NAME', '=', nom);
    end;
    memo.Lines.Add(buf);
  end;
  memo.Lines.Add(m[62]); //Origine
  memo.Lines.Add(m[63] + b + GetField('NAMEDETAIL'));
  if (trim(GetField('WORK') + GetField('NATIONLITY')) > '') and
    (trim(GetField('CENTURYC') + GetField('COUNTRY')) > '') then
  begin
    case wordformat of
      0: memo.Lines.Add(GetField('CENTURYC') + b +
          GetField('NATIONLITY') + b + GetField('WORK') + b +
          m[2] + b + GetField('COUNTRY'));
      1: memo.Lines.Add(GetField('WORK') + b +
          GetField('NATIONLITY') + b + m[1] + b + GetField('CENTURYC') +
          b + m[2] + b + GetField('COUNTRY'));
      2: memo.Lines.Add(GetField('NATIONLITY') + b +
          GetField('WORK') + b + GetField('CENTURYC') + b +
          m[2] + b + GetField('COUNTRY'));
    end;
    memo.Lines.Add(m[3] + b + GetField('BIRTHPLACE') + b + m[4] +
      b + GetField('BIRTHDATE'));
    memo.Lines.Add(m[5] + b + GetField('DEATHPLACE') + b + m[4] +
      b + GetField('DEATHDATE'));
  end;
  if (trim(GetField('FACTS')) <> '??') and
    (trim(GetField('FACTS')) <> '') then
    memo.Lines.Add(m[64] + b + GetField('FACTS'));
  memo.Lines.Add(m[6] + b + GetField('NAMEORIGIN'));
  memo.Lines.Add(m[7] + b + GetField('LANGRENUS'));
  memo.Lines.Add(m[8] + b + GetField('HEVELIUS'));
  memo.Lines.Add(m[9] + b + GetField('RICCIOLI'));
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
  nom, carte, url, img, remoteurl, buf, buf2: string;
  ok:   boolean;
  i, j: integer;
  function GetField(fn:string):string;
  begin
    result:=row.ByField[fn].AsString;
//    result:=UTF8Encode(row.ByField[fn].AsString);
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
  txt  := txt + t3 + m[56] + t3end + b + GetField('TYPE') + '<br>';
  txt  := txt + t3 + m[49] + t3end + b + GetField('PERIOD') + '<br>';
  txt  := txt + b + '<br>';
  txt  := txt + t2 + m[57] + t2end + '<br>'; //Taille
  txt  := txt + t3 + m[17] + t3end + b + GetField('LENGTHKM') + 'x' +
    GetField('WIDEKM') + m[18] + b + '/' + b + GetField('LENGTHMI') +
    'x' + GetField('WIDEMI') + m[19] + '<br>';
  buf  := GetField('HEIGHTM');
  buf2 := GetField('HEIGHTFE');
  if buf = buf2 then
    txt := txt + t3 + m[20] + t3end + b + buf + '<br>'  // inconnue
  else
  begin
    txt := txt + t3 + m[20] + t3end + b;
    val(buf, dummy, i);
    if i = 0 then
      txt := txt + buf + m[21] + b + '/' + b;
    val(buf2, dummy, i);
    if i = 0 then
      txt := txt + buf2 + m[22];
    txt   := txt + '<br>';
  end;
  txt := txt + t3 + m[23] + t3end + b + GetField('RAPPORT') + '<br>';
  txt := txt + b + '<br>';
  txt := txt + t2 + m[58] + t2end + '<br>'; //Description
  if GetField('GENERAL') > '' then
    txt := txt + GetField('GENERAL') + '<br>';
  if GetField('SLOPES') > '' then
    txt := txt + GetField('SLOPES') + '<br>';
  if GetField('WALLS') > '' then
    txt := txt + GetField('WALLS') + '<br>';
  if GetField('FLOOR') > '' then
    txt := txt + GetField('FLOOR') + '<br>';
  txt   := txt + b + '<br>';
  txt   := txt + t2 + m[59] + t2end + '<br>'; //Observation
  txt   := txt + t3 + m[24] + t3end + b + GetField('INTERESTC') + '<br>';
  buf   := GetField('MOONDAYS');
  buf2  := GetField('MOONDAYM');
  if buf = buf2 then
    txt := txt + t3 + m[25] + t3end + b + buf + '<br>'
  else
    txt := txt + t3 + m[25] + t3end + b + buf + b + m[26] + b + buf2 + '<br>';
  txt := txt + t3 + m[28] + t3end + b + GetField('PRINSTRU') + '<br>';
  txt   := txt + b + '<br>';
  txt   := txt + t2 + m[60] + t2end + '<br>'; //Position
  txt   := txt + t3 + m[10] + t3end + b + GetField('LONGIC') + '<br>';
  txt   := txt + t3 + m[11] + t3end + b + GetField('LATIC') + '<br>';
  txt   := txt + t3 + m[12] + t3end + b + GetField('QUADRANT') + '<br>';
  txt   := txt + t3 + m[13] + t3end + b + GetField('AREA') + '<br>';
  txt   := txt + b + '<br>';
  txt   := txt + t2 + m[61] + t2end + '<br>'; //Atlas
  // RUKL link
  carte := GetField('RUKL') + ' ' + GetField('RUKLC');
  img   := padzeros(GetField('RUKL'), 2);
  url   := ruklprefix + img + ruklsuffix;
  if fileexists(url) then
    url := ' <A HREF="file://' + url + '">' + carte + '</A>'
  else
    url := carte;
  txt := txt + t3 + m[14] + t3end + b + url + '<br>';
  buf := GetField('VISCARDY');
  if trim(buf) > '' then
    txt := txt + t3 + m[15] + t3end + b + buf + '<br>';
  buf   := GetField('HATFIELD');
  if trim(buf) > '' then
    txt := txt + t3 + m[16] + t3end + b + buf + '<br>';
  buf   := GetField('WESTFALL');
  if trim(buf) > '' then
    txt := txt + t3 + m[66] + t3end + b + buf + '<br>';
  buf   := GetField('WOOD');
  if trim(buf) > '' then
    txt := txt + t3 + m[72] + t3end + b + buf + '<br>';
  if ok then
  begin
    txt := txt + t3 + m[65] + t3end;
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
      txt := txt + b + ' <A HREF="' + url + '">' + carte + '</A>';
      ok  := dblox.SeekData('NAME', '=', nom);
    end;
    txt := txt + '<br>';
  end;
  txt := txt + b + '<br>';
  txt := txt + t2 + m[62] + t2end + '<br>'; //Origine
  txt := txt + t3 + m[63] + t3end + b + GetField('NAMEDETAIL') + '<br>';
  if (trim(GetField('WORK') + GetField('NATIONLITY')) > '') and
    (trim(GetField('CENTURYC') + GetField('COUNTRY')) > '') then
  begin
    case wordformat of
      0: txt := txt + GetField('CENTURYC') + b + GetField('NATIONLITY') +
          b + GetField('WORK') + b + m[2] + b + GetField('COUNTRY') + '<br>';
      // english
      1: txt := txt + GetField('WORK') + b + GetField('NATIONLITY') +
          b + m[1] + b + GetField('CENTURYC') + b + m[2] + b +
          GetField('COUNTRY') + '<br>';
      // francais, italian
      2: txt := txt + GetField('NATIONLITY') + b + GetField('WORK') +
          b + GetField('CENTURYC') + b + m[2] + b + GetField('COUNTRY') + '<br>';
      // russian
    end;
    txt := txt + t3 + m[3] + t3end + b + GetField('BIRTHPLACE') + b +
      m[4] + b + GetField('BIRTHDATE') + '<br>';
    txt := txt + t3 + m[5] + t3end + b + GetField('DEATHPLACE') + b +
      m[4] + b + GetField('DEATHDATE') + '<br>';
  end;
  if (trim(GetField('FACTS')) <> '??') and
    (trim(GetField('FACTS')) <> '') then
    txt := txt + t3 + m[64] + t3end + b + GetField('FACTS') + '<br>';
  txt   := txt + t3 + m[6] + t3end + b + GetField('NAMEORIGIN') + '<br>';
  txt   := txt + t3 + m[7] + t3end + b + GetField('LANGRENUS') + '<br>';
  txt   := txt + t3 + m[8] + t3end + b + GetField('HEVELIUS') + '<br>';
  txt   := txt + t3 + m[9] + t3end + b + GetField('RICCIOLI') + '<br>';
  txt   := txt + b + '<br>';
  txt   := txt + '</body></html>';
  Label7.Caption := GetField('PROFIL');
  Label7.Font.Size := 8;
  Label7.Left := 8;
  Label7.Top := 8;
  while (Label7.Font.Size > 3) and (Label7.Width > GroupBox1.ClientWidth) do
    Label7.Font.Size := Label7.Font.Size - 1;
  Label7.Left := (GroupBox1.ClientWidth - Label7.Width) div 2;
  Label7.Top  := (GroupBox1.ClientHeight - Label7.Height + 4) div 2;
  statusbar1.Panels[0].Text := m[10] + GetField('LONGIN');
  statusbar1.Panels[1].Text := m[11] + GetField('LATIN');
  Addtolist(nom);
end;

procedure Tform1.GetDBgrid;
var
  dbcol: integer;
begin
  if dbedited then
    if messagedlg(
      'Attention les modifications de la base de donnée n''ont pas été enregistrée et seront perdue. Continuer quand même ?',
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
    ShowMessage('Pas d''enregistrement sélectionné!');
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
  RefreshLabel;
  dbedited := False;
end;

procedure TForm1.Button20Click(Sender: TObject);  // delete dans la db!
var
  cmd: string;
begin
  if editrow <= 0 then
  begin
    ShowMessage('Pas d''enregistrement sélectionné en base de donnée!');
    exit;
  end;
  if messagedlg('Vraiment supprimer ' + stringgrid2.Cells[1, 1] +
    ' de la base de donnée ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    cmd := 'delete from moon where ID=' + IntToStr(editrow) + ';';
    dbm.Query(cmd);
    dbjournal(extractfilename(dbm.database), 'DELETE DBN=' + stringgrid2.Cells[1, 0] +
      ' NAME=' + stringgrid2.Cells[1, 1] + ' ID=' + IntToStr(editrow));
    dbedited := False;
    editrow  := -1;
    btnEffacer.Click;
    moon1.setmark(0, 0, '');
    RefreshLabel;
  end;
end;

procedure TForm1.btnEffacerClick(Sender: TObject);
// remise a blanc de toute les champs, on fait rien dans la db.
var
  dbcol: integer;
begin
  if dbedited then
    if messagedlg(
      'Attention les modifications de la base de donnée n''ont pas été enregistrée et seront perdue. Continuer quand même ?',
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
  i  := pos(m[18], ws);
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
    dbn := '2' // indicé face visible
  else
    dbn := '4'; // indicé face cachée
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
    if messagedlg(m[67], mtConfirmation, [mbYes, mbNo], 0) = mrYes then
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
  l, b, x, y: double;
  txt: string;
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
    if toolbutton3.down then
    begin
      // if full globe always center the formation
      lrot := -l;
      GLSceneViewer1MouseMove(nil, [ssLeft], lastx, lasty);
    end;
    if (not projMoon(l, b, librl, librb, x, y)) then
    begin
      // not on our current side, search next object
      SearchName(n, center);
      exit;
    end;
    currentl    := l;
    currentb    := b;
    currentid   := searchlist[searchpos];
    currentname := dbm.Results[0].ByField['NAME'].AsString;
    if center then begin
      moon1.CenterAt(deg2rad*currentl, deg2rad*currentb);
      moon1.SetMark(deg2rad*currentl, deg2rad*currentb, capitalize(currentname));
    end;
    GetHTMLDetail(dbm.Results[0], txt);
    SetDescText(txt);
    if dbtab.TabVisible then
      GetDBgrid;
    GetNotes(form1.Combobox1.Text);
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
    s := TStringStream.Create(Value);
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

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  Firstsearch := True;
  SearchName(Combobox1.Text, True);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  try
    if dummycube4 <> nil then
      DummyCube4.DeleteChildren;
    if dummycube5 <> nil then
      DummyCube5.DeleteChildren;
    GLSceneViewer1.Buffer.DestroyRC;
    dblox.Free;
    dbnotes.Free;
    hires.Free;
    hires500.Free;
    if hiresok then
    begin
      closefile(fh);
    end;
    if hires500ok then
    begin
      closefile(fh500);
    end;
    tz.Free;
    Fplanet.Free;
    overlayimg.Free;
    overlayhi.Free;
    searchlist.Free;
    if CursorImage1 <> nil then
    begin
      CursorImage1.FreeImage;
      CursorImage1.Free;
    end;
  except
  end;
end;

procedure TForm1.Quitter1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.RefreshMoonImage;
var
  moonrise, moonset, moontransit, azimuthrise, azimuthset: string;
  jd0, st0, q, tz1, cphase, colong, hh, az, ah: double;
  v1, v2, v3, v4, v5, v6, v7, v8, v9: double;
  gpa, glibrb, gsunincl, glibrl: double;
  aa, mm, dd, i, col, j: integer;
  h, mn, s, ms: word;
  ds1, ds2, n, ex1, ey1, xx, yy: integer;
  th, ex, ey, ci, si, sph: double;
  rvx, rvy: boolean;
  p:  array[0..23] of Tpoint;
  ph: Tbitmap;
const
  b = ' ';
begin
  st0 := 0;
  Fplanet.Moon(CurrentJD, ra, Dec, dist, dkm, diam, phase, illum);
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
  rad := ra;
  ded := Dec;
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
  if FollowNorth then
  begin
    CameraOrientation := rmod(-PA + PoleOrientation + 360, 360);
    moon1.Orientation:=CameraOrientation;
  end;
  StatusBar1.Panels[2].Text := m[51] + ': ' + date2str(curyear, currentmonth, currentday) +
    '   ' + m[50] + ': ' + timtostr(currenttime);
  phaseoffset := 0;

  djd(nmjd - (DT_UT) / 24, aa, mm, dd, hh);
  decodetime(hh / 24, h, mn, s, ms);
  tz1 := gettimezone(encodedatetime(aa, mm, dd, h, mn, s, ms));
  djd(nmjd + (tz1 - DT_UT) / 24, aa, mm, dd, hh);
  labelnm.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);

  djd(fqjd - (DT_UT) / 24, aa, mm, dd, hh);
  decodetime(hh / 24, h, mn, s, ms);
  tz1 := gettimezone(encodedatetime(aa, mm, dd, h, mn, s, ms));
  djd(fqjd + (tz1 - DT_UT) / 24, aa, mm, dd, hh);
  labelfq.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);

  djd(fmjd - (DT_UT) / 24, aa, mm, dd, hh);
  decodetime(hh / 24, h, mn, s, ms);
  tz1 := gettimezone(encodedatetime(aa, mm, dd, h, mn, s, ms));
  djd(fmjd + (tz1 - DT_UT) / 24, aa, mm, dd, hh);
  labelfm.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);

  djd(lqjd - (DT_UT) / 24, aa, mm, dd, hh);
  decodetime(hh / 24, h, mn, s, ms);
  tz1 := gettimezone(encodedatetime(aa, mm, dd, h, mn, s, ms));
  djd(lqjd + (tz1 - DT_UT) / 24, aa, mm, dd, hh);
  labellq.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);

  Stringgrid1.colwidths[1] := 150;
  i := 0;
  if geocentric then
  begin
    Stringgrid1.Cells[0, i] := form2.Label16.Caption + ':';
    Stringgrid1.Cells[1, i] := form2.CheckBox3.Caption;
  end
  else
  begin
    Stringgrid1.Cells[0, i] := form2.Label16.Caption + ':';
    Stringgrid1.Cells[1, i] := demtostr(ObsLatitude) + ' ' + demtostr(ObsLongitude) +
      ' Tz:' + armtostr(timezone);
  end;
  Inc(i);
  Stringgrid1.Cells[0, i] := m[51] + ':';
  Stringgrid1.Cells[1, i] := date2str(curyear, currentmonth, currentday) +
    ' ' + timtostr(currenttime);
  djd(currentjd, aa, mm, dd, hh);
  Inc(i);
  Stringgrid1.Cells[0, i] := m[51] + ' (DT):';
  Stringgrid1.Cells[1, i] := date2str(aa, mm, dd) + ' ' + timtostr(hh);
  Inc(i);
  Stringgrid1.Cells[0, i] := '(J2000) ' + m[29];
  Stringgrid1.Cells[1, i] := artostr(rad2deg * ra / 15);
  Inc(i);
  Stringgrid1.Cells[0, i] := '(J2000) ' + m[30];
  Stringgrid1.Cells[1, i] := detostr(rad2deg * Dec);
  Inc(i);
  Stringgrid1.Cells[0, i] := '(' + m[51] + ')' + b + m[29];
  Stringgrid1.Cells[1, i] := artostr(rad2deg * rad / 15);
  Inc(i);
  Stringgrid1.Cells[0, i] := '(' + m[51] + ')' + b + m[30];
  Stringgrid1.Cells[1, i] := detostr(rad2deg * ded);
  Inc(i);
  Stringgrid1.Cells[0, i] := m[31];
  Stringgrid1.Cells[1, i] := IntToStr(round(dkm)) + m[18];
  Inc(i);
  Stringgrid1.Cells[0, i] := m[36];
  Stringgrid1.Cells[1, i] := formatfloat(f2, diam / 60) + lmin;
  Inc(i);
  Stringgrid1.Cells[0, i] := m[48];
  Stringgrid1.Cells[1, i] := formatfloat(f1, colong) + ldeg;
  Inc(i);
  Stringgrid1.Cells[0, i] := m[32];
  Stringgrid1.Cells[1, i] := formatfloat(f1, phase) + ldeg;
  Inc(i);
  Stringgrid1.Cells[0, i] := m[46];
  Stringgrid1.Cells[1, i] := formatfloat(f2, lunaison) + ' ' + m[47];
  Inc(i);
  Stringgrid1.Cells[0, i] := m[35];
  Stringgrid1.Cells[1, i] := formatfloat(f1, illum * 100) + '%';
  Inc(i);
  Stringgrid1.Cells[0, i] := m[45];
  Stringgrid1.Cells[1, i] := formatfloat(f1, sunincl) + ldeg;
  Inc(i);
  Stringgrid1.Cells[0, i] := m[33];
  Stringgrid1.Cells[1, i] := demtostr(librb);
  Inc(i);
  Stringgrid1.Cells[0, i] := m[34];
  Stringgrid1.Cells[1, i] := demtostr(librl);
  Inc(i);
  Stringgrid1.Cells[0, i] := m[37];
  Stringgrid1.Cells[1, i] := formatfloat(f1, pa) + ldeg;
  if not geocentric then
  begin
    eq2hz(st0 - rad, ded, az, ah);
    az := rmod(rad2deg * az + 180, 360);
    Inc(i);
    Stringgrid1.Cells[0, i] := m[73];
    Stringgrid1.Cells[1, i] := demtostr(az);
    Inc(i);
    Stringgrid1.Cells[0, i] := m[74];
    Stringgrid1.Cells[1, i] := demtostr(rad2deg * ah);
    Fplanet.PlanetRiseSet(11, jd(CurYear, CurrentMonth, CurrentDay, 0),
      True, moonrise, moontransit, moonset, azimuthrise, azimuthset, v1, v2, v3, v4, v5, v6, v7, v8, v9, j);
    Inc(i);
    Stringgrid1.Cells[0, i] := m[38];
    Stringgrid1.Cells[1, i] := moonrise;
    Inc(i);
    Stringgrid1.Cells[0, i] := m[39];
    Stringgrid1.Cells[1, i] := moontransit;
    Inc(i);
    Stringgrid1.Cells[0, i] := m[40];
    Stringgrid1.Cells[1, i] := moonset;
    Inc(i);
    Stringgrid1.Cells[0, i] := m[41];
    Stringgrid1.Cells[1, i] := azimuthrise;
    if obslatitude > 0 then
    begin
      Inc(i);
      Stringgrid1.Cells[0, i] := m[55];
      Stringgrid1.Cells[1, i] := dedtostr(90 - obslatitude + rad2deg * Dec);
    end
    else
    begin
      Inc(i);
      Stringgrid1.Cells[0, i] := m[55];
      Stringgrid1.Cells[1, i] := dedtostr(90 + obslatitude - rad2deg * Dec);
    end;
    Inc(i);
    Stringgrid1.Cells[0, i] := m[42];
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
  if not phaseeffect then
  begin
    phase   := 1;
    cphase  := 1;
    sunincl := 0;
  end;
  librlong := librl;
  librlat  := librb;
  if (not librationeffect) or ToolButton3.Down then
  begin
    librl := 0;
    librb := 0;
  end;

  if not ToolButton3.Down then
  begin
    moon1.LibrLat := librb;
    moon1.LibrLon := -librl;
  end;
  moon1.Phase:=deg2rad*cphase;
  moon1.SunIncl:=deg2rad*sunincl;
  moon1.ShowPhase:=phaseeffect;
  moon1.RefreshLabel;
 { glscene1.BeginUpdate;
  dummycube1.BeginUpdate;
  if not ToolButton3.Down then
  begin
    resetorientation;
    dummycube1.PitchAngle := librb;
    dummycube1.TurnAngle := -librl;
    dummycube1.up.x := 0;
  end;
  OrientLightSource(cphase, sunincl);
  GLLightSource2.Shining := not phaseeffect; }
  LibrationMark(librlong, librlat);
  dummycube1.EndUpdate;
  //moon1.SetMark(0, 0, '');
{  ShowSphere;
  RefreshLabel;
  glscene1.EndUpdate;}
end;

procedure TForm1.ShowCoordinates(x, y: integer);
var
  xx, yy, l, b: double;
begin
  lastx     := x;
  lasty     := y;
  lastyzoom := y;
  Window2World(x, y, xx, yy);
  InvProjMoon(2 * xx, 2 * yy, librl, librb, l, b);
  statusbar1.Panels[0].Text := m[10] + stringreplace(formatfloat(f1, l), 'Nan', '   ', []);
  statusbar1.Panels[1].Text := m[11] + stringreplace(formatfloat(f1, b), 'Nan', '   ', []);
end;

procedure  TForm1.SetZoomBar;
begin
locktrackbar := True;
trackbar1.max := round(10 * log10(moon1.ZoomMax));
locktrackbar := True;
trackbar1.position := round(10 * log10(moon1.Zoom));
end;

procedure  TForm1.GetMsg(Sender: TObject; value: String);
begin
statusbar1.Panels[3].Text := value;
SetZoomBar;
end;

procedure TForm1.MeasureDistance(x, y: integer);
var
  xx, yy, l, b, d: double;
begin
  window2world(x, y, xx, yy);
  InvProjMoon(2 * xx, 2 * yy, librl, librb, l, b);
  d  := rad2deg*angulardistance(deg2rad*l, deg2rad*b, deg2rad*startl, deg2rad*startb);
  edit1.Text := formatfloat(f1, deg2rad * d * Rmoon) + m[18];
  xx := startxx - xx;
  yy := startyy - yy;
  d  := sqrt(xx * xx + yy * yy) * diam / 3600;
  edit2.Text := copy(Deptostr(d), 5, 99);
  xx := startxx - xx / 2;
  yy := startyy - yy / 2;
  InvProjMoon(2 * xx, 2 * yy, librl, librb, l, b);
  edit3.Text := formatfloat(f1, l);
  edit4.Text := formatfloat(f1, b);
  x := x - startx;
  y := y - starty;
  d := sqrt(x * x + y * y);
  HUDSprite2.Width := d;
  HUDSprite2.Position.X := startx + x / 2;
  HUDSprite2.Position.Y := starty + y / 2;
  HUDSprite2.Rotation := -rad2deg * (arctan2(y, x));
end;

procedure TForm1.IdentXY(x, y: integer);
var
  xx, yy, l, b: double;
  txt: string;
begin
  Window2World(x, y, xx, yy);
  if InvProjMoon(2 * xx, 2 * yy, librl, librb, l, b) and SearchAtPos(l, b) then
  begin
    l := dbm.Results[0].ByField['LONGIN'].AsFloat;
    b := dbm.Results[0].ByField['LATIN'].AsFloat;
    searchl := l;
    searchb := b;
    currentl := l;
    currentb := b;
    currentname := dbm.Results[0].ByField['NAME'].AsString;
    projMoon(l, b, librl, librb, xx, yy);
    Combobox1.Text := currentname;
    moon1.SetMark(deg2rad*currentl, deg2rad*currentb, Combobox1.Text);
    RefreshLabel;
    GetHTMLDetail(dbm.Results[0], txt);
    SetDescText(txt);
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
  else
    moon1.SetMark(0, 0, '');
end;

procedure TForm1.IdentLB(l, b: single);
var
  txt: string;
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
    GetHTMLDetail(dbm.Results[0], txt);
    SetDescText(txt);
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

procedure TForm1.FormShow(Sender: TObject);
begin
 moon1.Init;
 moon1.texture:='Clementine';
 if moon1.CanBump then begin
   moon1.BumpPath:=slash(appdir)+slash('Textures')+slash('Bumpmap');
   moon1.Bumpmap:=false;
 end else begin
 end;
 moon1.ShowPhase:=false;
 moon1.VisibleSideLock:=true;
 moon1.Labelcolor:=autolabelcolor;
end;

procedure TForm1.Init;
begin
  Setlang;
  screen.cursor := crHourGlass;
  LoadDB(dbm);
  screen.cursor := crDefault;
  ListUserDB;
  InitLopamIdx;
  InitTelescope;
  tz.TimeZoneFile := ZoneDir + StringReplace(ObsTZ, '/', PathDelim, [rfReplaceAll]);
  timezone := tz.SecondsOffset / 3600;
  LibrationButton.Down := librationeffect;
  PhaseButton.Down := phaseeffect;
  if fileexists(slash(appdir) + slash('data') + 'retic.cur') then
  begin
    CursorImage1.LoadFromFile(slash(appdir) + slash('data') + 'retic.cur');
    Screen.Cursors[crRetic] := CursorImage1.Handle;
  end;
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
    form1.Top    := 0;
    form1.Left   := 0;
    form1.Width  := screen.Width;
    form1.Height := screen.Height;
  end;
  moon1.SetMark(0, 0, '');
  MoveCamera(0, 0);
  setzoom(minfoc);
  ReadParam;
  memo2.Width    := PrintTextWidth;
  label10.Left   := toolbar2.left + toolbar2.Width + 2;
  trackbar1.Left := label10.Left + label10.Width + 2;
  toolbar1.Left  := trackbar1.Left + trackbar1.Width + 1;
  RefreshMoonImage;
  if currentname <> '' then
  begin
    firstsearch := True;
    searchname(currentname, False);
  end;
  setzoom(curfoc);
  btnEffacerClick(nil);
  SetEyepieceMenu;
  if (not multi_instance) and (currentid = '') then
  begin
    // show an interesting object
    Combobox2.ItemIndex   := 0;
    Combobox3.ItemIndex   := 6;
    RadioGroup1.ItemIndex := 1;
    Updterminateur;
    Firstsearch := True;
    SearchText  := trim(copy(ListBox1.Items[0], 2, 999));
    SearchName(SearchText, False);
    Combobox3.ItemIndex := 0;
    currentphase := -999;
  end;
  moon1.Mirror:=checkbox2.Checked;
  SetZoomBar;
  moon1.RefreshLabel;
end;

procedure TForm1.Configuration1Click(Sender: TObject);
var
  reboot, reloaddb, systemtimechange: boolean;
  oldreduc, oldreducfar, i, j, oldmaxima, oldtexture: integer;
  yy, x, y: double;
begin
  try
    reboot   := False;
    reloaddb := False;
    form2.Edit1.Text := formatfloat(f2, abs(ObsLatitude));
    form2.Edit2.Text := formatfloat(f2, abs(ObsLongitude));
    form2.Edit3.Text := formatfloat(f1, timezone);
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
    ListUserDB;
    form2.checkbox4.Checked := True;
    form2.checkbox1.Checked := phaseeffect;
    form2.checkbox2.Checked := librationeffect;
    form2.checkbox3.Checked := Geocentric;
    form2.checkbox5.Checked := showlabel;
    form2.checkbox6.Checked := showmark;
    form2.checkbox14.Checked := showlibrationmark;
    form2.checkbox17.Checked := labelcenter;
    form2.checkbox18.Checked := minilabel;
    form2.Shape1.Brush.Color := marklabelcolor;
    form2.Shape2.Brush.Color := markcolor;
    form2.Shape3.Brush.Color := autolabelcolor;
    form2.TrackBar2.Position := -LabelDensity;
    form2.TrackBar3.Position := round(LabelSize * 100);
    form2.TrackBar4.Position := marksize;
    config.newlang := language;
    oldreduc := reducetexture;
    case reducetexture of
      1: form2.RadioGroup1.ItemIndex := 0;
      2: form2.RadioGroup1.ItemIndex := 1;
      4: form2.RadioGroup1.ItemIndex := 2;
    end;
    oldreducfar := reducetexturefar;
    case reducetexturefar of
      1: form2.RadioGroup3.ItemIndex := 0;
      2: form2.RadioGroup3.ItemIndex := 1;
      4: form2.RadioGroup3.ItemIndex := 2;
    end;
    form2.updown1.Position := maxima;
    oldmaxima := maxima;
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
    form2.CheckBox10.Checked := MipMaps;
    if hiresfile = 'hires.jpg' then
      form2.radiogroup2.ItemIndex := 0
    else if hiresfile = 'hires_clem.jpg' then
      form2.radiogroup2.ItemIndex := 1
    else if hiresfile = 'hires_lopam.jpg' then
      form2.radiogroup2.ItemIndex := 2
    else
      form2.radiogroup2.ItemIndex := -1;
    oldtexture := form2.radiogroup2.ItemIndex;
    form2.CheckBox15.Checked := LopamDirect;
    form2.ruklprefix.Text := ruklprefix;
    form2.ruklsuffix.Text := ruklsuffix;
    form2.hiresfn := hiresfile;
    form2.combobox5.Text := remext(overlayname);
    form2.trackbar5.position := overlaylum;
    form2.combobox5change(Sender);
    form2.checkbox11.Checked := showoverlay;
    form2.checkbox16.Checked := UseComputerTime;
    FormPos(Form2, mouse.cursorpos.x, mouse.cursorpos.y);
    Form2.showmodal;
    if form2.ModalResult = mrOk then
    begin
      screen.cursor := crhourglass;
      if (form2.combobox5.Text <> remext(overlayname)) or
        (form2.trackbar5.position <> overlaylum) or
        (form2.checkbox11.Checked <> showoverlay) then
        reboot    := True;
      overlayname := form2.combobox5.Text + '.jpg';
      overlaylum  := form2.trackbar5.position;
      showoverlay := form2.checkbox11.Checked;
      if hiresfile <> form2.hiresfn then
      begin
        hiresfile := form2.hiresfn;
        if copy(hiresfile, 6, 1) <> '.' then
        begin
          i := pos('.', hiresfile);
          if i > 6 then
            imgsuffix := copy(hiresfile, 6, i - 6);
        end
        else
          imgsuffix := '';
        OpenHires(True);
        reboot := True;
      end;
      ruklprefix    := form2.ruklprefix.Text;
      ruklsuffix    := form2.ruklsuffix.Text;
      markcolor     := form2.Shape2.Brush.Color;
      marklabelcolor    := form2.Shape1.Brush.Color;
      autolabelcolor := form2.Shape3.Brush.Color;
      LabelDensity  := abs(form2.TrackBar2.Position);
      LabelSize     := form2.TrackBar3.Position / 100;
      marksize      := form2.TrackBar4.Position;
      showlabel     := form2.checkbox5.Checked;
      showmark      := form2.checkbox6.Checked;
      showlibrationmark := form2.checkbox14.Checked;
      labelcenter   := form2.checkbox17.Checked;
      minilabel     := form2.checkbox18.Checked;
      Showautolabel := showlabel and (Labeldensity < 1000);
      HUDText1.ModulateColor.AsWinColor := marklabelcolor;
      HUDText1.Scale.SetVector(Label3dSize * LabelSize, Label3dSize * LabelSize, 1);
      moon1.Labelcolor:=autolabelcolor;
      InitLabel;
      InitSprite;
      Obslatitude := strtofloat(form2.Edit1.Text);
      if form2.ComboBox1.ItemIndex = 1 then
        Obslatitude := -Obslatitude;
      Obslongitude  := strtofloat(form2.Edit2.Text);
      if form2.ComboBox2.ItemIndex = 0 then
        Obslongitude := -Obslongitude;
      systemtimechange := UseComputerTime <> form2.checkbox16.Checked;
      timezone := strtofloat(form2.Edit3.Text);
      UseComputerTime := form2.checkbox16.Checked;
      timezone := gettimezone(now);
      if systemtimechange then
      begin
        InitDate;
        reboot := True;
      end;
      phaseeffect     := form2.checkbox1.Checked;
      librationeffect := form2.checkbox2.Checked;
      Geocentric      := form2.checkbox3.Checked;
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
      usedatabase[1] := form2.checkbox19.Checked;
      usedatabase[2] := form2.checkbox20.Checked;
      usedatabase[3] := form2.checkbox21.Checked;
      usedatabase[4] := form2.checkbox22.Checked;
      usedatabase[5] := form2.checkbox23.Checked;
      usedatabase[6] := form2.checkbox24.Checked;
      for i := 0 to form2.Checklistbox1.Count - 1 do
      begin
        j := (form2.Checklistbox1.Items.Objects[i] as TDBinfo).dbnum;
        if usedatabase[j] <> form2.Checklistbox1.Checked[i] then
          reloaddb     := True;
        usedatabase[j] := form2.Checklistbox1.Checked[i];
      end;
      InitObservatoire;
      CurrentJD := jd(CurYear, CurrentMonth, CurrentDay, Currenttime - timezone + DT_UT);
      case form2.RadioGroup1.ItemIndex of
        0: reducetexture := 1;
        1: reducetexture := 2;
        2: reducetexture := 4;
      end;
      case form2.RadioGroup3.ItemIndex of
        0: reducetexturefar := 1;
        1: reducetexturefar := 2;
        2: reducetexturefar := 4;
      end;
      if reducetexture <> oldreduc then
        reboot := True;
      if reducetexturefar <> oldreducfar then
        reboot := True;
      if config.newlang <> language then
      begin
        language := newlang;
        setlang;
        reloaddb := True;
      end;
      if reloaddb then
        LoadDB(dbm);
      MipMaps := form2.CheckBox10.Checked;
      SetMinFilter(MipMaps);
      LibrationButton.Down := librationeffect;
      PhaseButton.Down     := phaseeffect;
      if oldtexture <> form2.radiogroup2.ItemIndex then
        reboot      := True;
      GeologicalMap := False;
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
      if reboot then
      begin
        application.ProcessMessages;
        currenteyepiece := 0;
        annulus1.Visible := False;
        GLsceneViewer1.Buffer.BackgroundColor := clBlack;
        yy := zoom * minfoc;
        Window2World(lastx, lasty, x, y);
        InitGraphic(Sender);
        RefreshMoonImage;
        if yy < minfoc then
          yy := minfoc;
        SetZoom(yy);
        application.ProcessMessages;
        Formresize(Sender);
      end
      else
        RefreshMoonImage;
      SaveDefault;
    end;
  finally
    screen.cursor := crdefault;
  end;
end;

procedure TForm1.PairSplitterSide2Resize(Sender: TObject);
begin
 if not skippanelresize then
    ToolsWidth:=ClientWidth-PairSplitter1.Position;
 skippanelresize:=false;
end;

procedure TForm1.FormResize(Sender: TObject);
var
  dx: integer;
begin
  if skipresize then
    exit;
  if csDestroying in form1.ComponentState then
    exit;
  if csLoading in form1.ComponentState then
    exit;
  skippanelresize:=true;
  PageControl1.width:=ToolsWidth;
  PairSplitter1.Position:=ClientWidth-ToolsWidth;
  RefreshLabel;
{  form1.glscene1.BeginUpdate;
  HUDSprite2.Visible := False;
  PanelMoon.left   := 0;
  PanelMoon.top    := ControlBar1.Height;
  PanelMoon.Height := ClientHeight - ControlBar1.Height - StatusBar1.Height;
  PanelMoon.Width  := PanelMoon.Height;
  panel2.left   := 0;
  panel2.top    := form1.ControlBar1.Height;
  panel2.Height := form1.ClientHeight - form1.ControlBar1.Height - form1.StatusBar1.Height;
  panel2.Width  := panel2.Height;
  glsceneviewer1.Width := panel2.clientwidth - scrollbar2.Width;
  glsceneviewer1.Height := panel2.clientHeight - scrollbar1.Height;
  pagecontrol1.left := panel2.left + panel2.Width;
  pagecontrol1.top := ControlBar1.Height;
  pagecontrol1.Width := form1.ClientWidth - panel2.Width;
  pagecontrol1.Height := form1.ClientHeight - ControlBar1.Height - form1.StatusBar1.Height;
  Mark(shapePositionX, shapePositionY, hudtext1.Text);
  RefreshLabel;
  form1.glscene1.EndUpdate;
  statusbar1.left  := 0;
  statusbar1.top   := form1.ClientHeight - form1.StatusBar1.Height;
  statusbar1.Width := form1.ClientWidth; }
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  initdate;
  RefreshMoonImage;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  form1.heure.Value      := 0;
  form1.minute.Value     := 0;
  form1.seconde.Value    := 0;
  form1.updown4.position := 0;
  form1.updown5.position := 0;
  form1.updown6.position := 0;
  Button4Click(Sender);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  y, m, d: integer;
  h, n, s: word;
begin
  y     := form1.annee.Value;
  m     := form1.mois.Value;
  d     := form1.jour.Value;
  h     := form1.heure.Value;
  n     := form1.minute.Value;
  s     := form1.seconde.Value;
  timezone := gettimezone(encodedatetime(y, m, d, h, n, s, 0));
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
  if locktrackbar then
  begin
    locktrackbar := False;
    exit;
  end;
ZoomTimer.Enabled:=false;
ZoomTimer.Enabled:=true;
end;

procedure TForm1.ZoomTimerTimer(Sender: TObject);
begin
ZoomTimer.Enabled:=false;
moon1.Zoom := round(power(10, trackbar1.position / 10));
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
var
  xx, yy: double;
begin
  if moon1.zoom = 1 then
    moon1.CenterAt(0, 0)
  else
    moon1.CenterMark;
  RefreshLabel;
end;

procedure TForm1.SetFullScreen;
begin
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  i: integer;
begin
  if key = 27 then
    SetFullScreen;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := True;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
  hnd:    thandle;
  OldVal: longint;
  ok:     boolean;
begin
  try
    if not multi_instance then
    begin
      SaveDefault;
    end;
    param.Free;
{$ifdef mswindows}
    if CloseCdC then
    begin
      hnd := findwindow(nil, PChar(CdCcaption));
      if hnd > 0 then
        PostMessage(hnd, WM_CLOSE, 0, 0);   // close skychart (use WM_CLOSE for the close prompt)
    end;
    if CloseVMAbrowser then
    begin
      hnd := findwindow(nil, PChar(VMAbrowser));
      if hnd > 0 then
        PostMessage(hnd, WM_CLOSE, 0, 0);   // close VMA Browser (use WM_CLOSE to save the config)
    end;
    if ClosePhotlun then
      ExecNoWait(Photlun + ' -quit');
{$endif}
    if scopelibok then
      if ScopeConnected then
      begin
        ScopeDisconnect(ok);
      end;
  except
  end;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  FilePopup.PopUp();
end;

procedure TForm1.ShowImg(desc, nom: string; forceinternal: boolean);
var
  buf, buf1: string;
  i, j: integer;
  jpg: Tjpegimage;
  bmp: Tbitmap;
begin
  chdir(appdir);
  i := -1;
  for j := 0 to maximgdir - 1 do
    if uppercase(trim(desc)) = uppercase(trim(imgdir[j, 2])) then
      i := j;
  if i >= 0 then
  begin
    buf  := Slash(imgdir[i, 0]) + trim(nom);
    buf1 := imgdir[i, 1];
  end
  else
  begin
    buf  := slash(desc) + trim(nom);
    buf1 := '';
  end;
  if fileexists(buf) then
  begin
    if externalimage and (not forceinternal) then
    begin
      Inc(lastima);
      if lastima >= maxima then
        lastima := 0;
      jpg := Tjpegimage.Create;
      bmp := Tbitmap.Create;
      try
        i := pos('.', nom);
        if i > 0 then
          nom := copy(nom, 1, i - 1);
        if uppercase(extractfileext(buf)) = '.JPG' then
        begin
          jpg.LoadFromFile(buf);
          bmp.Assign(jpg);
        end
        else
          bmp.LoadFromFile(buf);
        bmp.Canvas.Font.Name   := 'Arial';
        bmp.canvas.Font.Height := -9;
        bmp.Canvas.TextOut(0, 0, nom + ' ' + buf1);
        buf := tempdir + '$temp' + IntToStr(lastima) + '.bmp';
        bmp.SaveToFile(buf);
      finally
        bmp.Free;
        jpg.Free;
      end;
      ExecNoWait(externalimagepath + ' "' + buf + '"');
    end
    else
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
end;

procedure TForm1.ToolButton8Click(Sender: TObject);
begin
  HelpPopup.PopUp();
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
  moon1.Zoom:=1;
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
  form1.glscene1.BeginUpdate;
  sphere1.Slices := i;
  sphere1.Stacks := i;
  sphere2.Slices := i;
  sphere2.Stacks := i;
  sphere3.Slices := i;
  sphere3.Stacks := i;
  sphere4.Slices := i;
  sphere4.Stacks := i;
  sphere5.Slices := i;
  sphere5.Stacks := i;
  sphere6.Slices := i;
  sphere6.Stacks := i;
  sphere7.Slices := i;
  sphere7.Stacks := i;
  sphere8.Slices := i;
  sphere8.Stacks := i;
  case sphere1.Slices of
    45: i  := 16;
    90: i  := 32;
    180: i := 64;
    360: i := 128;
    720: i := 256;
  end;
  hiressphere.Slices    := i;
  hiressphere.Stacks    := i;
  hiressphere500.Slices := i;
  hiressphere500.Stacks := i;
  form1.glscene1.EndUpdate;
end;

procedure TForm1.PerfTimerTimer(Sender: TObject);
begin
  Label15.Caption := Format(m[44] + ' %.2f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.PageControl1Changing(Sender: TObject; var AllowChange: boolean);
begin
  if pagecontrol1.ActivePage = Outils.Caption then
  begin
    moon1.SetMark(0, 0, '');
    GLSceneViewer1.Cursor := crRetic;
    HUDSprite2.Visible    := False;
  end;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  if (pagecontrol1.ActivePage = Reglage.Caption) then
  begin
    PerfTimer.Enabled      := True;
    GLCadencer1.Enabled := True;
    Label15.Caption     := m[44] + ' 0 FPS';
    case GLsceneviewer1.Buffer.Acceleration of
      chaUnknown: label17.Caption  := m[68];
      chaHardware: label17.Caption := m[69];
      chaSoftware: label17.Caption := m[70];
    end;
    label18.Caption := m[71] + ' : ' + IntToStr(
      GLmaterialLibrary1.Materials[0].Material.Texture.Image.Width) + 'x' +
      IntToStr(GLmaterialLibrary1.Materials[0].Material.Texture.Image.Height);
  end
  else
  begin
    PerfTimer.Enabled      := False;
    GLCadencer1.Enabled := False;
  end;

  if pagecontrol1.ActivePage = Terminateur.Caption then
  begin
    if currentphase <> tphase then
      UpdTerminateur;
  end;
  if pagecontrol1.ActivePage = Outils.Caption then
  begin
    if measuringdistance then
    begin
      GLSceneViewer1.Cursor := crCross;
      Button11.Caption      := m[53];
    end
    else
    begin
      GLSceneViewer1.Cursor := crRetic;
      Button11.Caption      := m[52];
    end;
  end
  else
  begin
    measuringdistance     := False;
    GLSceneViewer1.Cursor := crRetic;
    HUDSprite2.Visible    := False;
  end;
end;



procedure TForm1.Stop1Click(Sender: TObject);
begin
  RotationCadencer.Enabled := False;
end;

procedure TForm1.EastWest1Click(Sender: TObject);
begin
  rotdirection := -rotdirection;
end;

procedure TForm1.N10seconde1Click(Sender: TObject);
begin
  combobox4.ItemIndex := 0;
  rotstep := 10;
  RotationCadencer.Enabled := False;
  RotationCadencer.Enabled := True;
end;

procedure TForm1.N5seconde1Click(Sender: TObject);
begin
  combobox4.ItemIndex := 1;
  rotstep := 5;
  RotationCadencer.Enabled := False;
  RotationCadencer.Enabled := True;
end;

procedure TForm1.N1seconde1Click(Sender: TObject);
begin
  combobox4.ItemIndex := 2;
  rotstep := 1;
  RotationCadencer.Enabled := False;
  RotationCadencer.Enabled := True;
end;

procedure TForm1.N05seconde1Click(Sender: TObject);
begin
  combobox4.ItemIndex := 3;
  rotstep := 0.5;
  RotationCadencer.Enabled := False;
  RotationCadencer.Enabled := True;
end;

procedure TForm1.N02seconde1Click(Sender: TObject);
begin
  combobox4.ItemIndex := 4;
  rotstep := 0.2;
  RotationCadencer.Enabled := False;
  RotationCadencer.Enabled := True;
end;

procedure TForm1.ComboBox4Change(Sender: TObject);
begin
  case combobox4.ItemIndex of
    0: rotstep := 10;
    1: rotstep := 5;
    2: rotstep := 1;
    3: rotstep := 0.5;
    4: rotstep := 0.2;
  end;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  RotationCadencer.Enabled := False;
  sleep(50);
  rotdirection := 1;
  RotationCadencer.Enabled := True;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);

begin
  RotationCadencer.Enabled := False;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);

begin
  RotationCadencer.Enabled := False;
  sleep(50);
  rotdirection := -1;
  RotationCadencer.Enabled := True;
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
  setzoom(4 * minfoc);
  movecamera(0.45, 0);
  CameraOrientation := 90;
  moon1.Orientation:=CameraOrientation;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);

begin
  setzoom(4 * minfoc);
  movecamera(-0.45, 0);
  CameraOrientation := -90;
  moon1.Orientation:=CameraOrientation;
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
  setzoom(2 * minfoc);
  movecamera(0, 0);
  case RadioGroup2.ItemIndex of
    0: CameraOrientation := 0;
    1: CameraOrientation := 180;
  end;
  moon1.Orientation:=CameraOrientation;
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
{$ifdef mswindows}
  Registry1: TRegistry;
{$endif}
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
    if not FileExists(CdC) then
      CdC      := DefaultCdC;
    CdCcaption := 'Cartes du Ciel';
  end;
  // Try CdC V2
{$ifdef mswindows}
  if CdCdir = '' then
  begin
    Registry1 := TRegistry.Create;
    with Registry1 do
    begin
      if Openkey('Software\Astro_PC\Ciel\Config', False) then
      begin
        if ValueExists('AppDir') then
          CdCdir := ReadString('Appdir');
        CloseKey;
        CdC := slash(CdCdir) + 'ciel.exe';
        CdCcaption := 'Cartes du Ciel';
      end;
    end;
  end;
{$endif}
end;

procedure TForm1.CartesduCiel1Click(Sender: TObject);
var
  cmd1, cmd2, cmd3: string;
  i: integer;
begin
{$ifdef mswindows}
  i := findwindow(nil, PChar(CdCcaption));
  if i = 0 then
    CloseCdC := True
  else
  begin
    SendMessage(i, WM_SYSCOMMAND, SC_RESTORE, 0);
    SetForegroundWindow(i);
    exit;
  end;
{$endif}
  ExecNoWait(CdC + ' --unique --nosplash');
end;

procedure TForm1.Aide2Click(Sender: TObject);
var
  fn: string;
begin
  fn := slash(HelpDir) + language + '_Index_Doc.pdf';
  if not FileExists(fn) then
  begin
    fn := slash(HelpDir) + language + '_Index_Doc.html';
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
  fn := slash(AppDir) + slash('Encyclopedia') + language + 'Encyclopedia.html';
  if not FileExists(fn) then
  begin
    fn := slash(AppDir) + slash('Encyclopedia') + 'UK_Encyclopedia.html';
  end;
  ExecuteFile(fn);
end;

procedure TForm1.Position1Click(Sender: TObject);
var
  ok: boolean;
begin
  if multi_instance and (clientwidth = ClientHeight - controlbar1.Height -
    statusbar1.Height) then
    clientwidth := round(1.333 * clientwidth);
  PageControl1Changing(Sender, ok);
  Pagecontrol1.ActivePage := Position.Caption;
  PageControl1Change(Sender);
  combobox1.SetFocus;
end;

procedure TForm1.Notes1Click(Sender: TObject);
var
  ok: boolean;
begin
  if multi_instance and (clientwidth = ClientHeight - controlbar1.Height -
    statusbar1.Height) then
    clientwidth := round(1.333 * clientwidth);
  PageControl1Changing(Sender, ok);
  Pagecontrol1.ActivePage := Notes.Caption;
  PageControl1Change(Sender);
end;

procedure TForm1.x21Click(Sender: TObject);
begin
  setzoom(2 * minfoc);
end;

procedure TForm1.x41Click(Sender: TObject);
begin
  setzoom(4 * minfoc);
end;

procedure TForm1.x81Click(Sender: TObject);
begin
  setzoom(8 * minfoc);
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
  moon1.Orientation:=CameraOrientation;
  Mark(shapePositionX, shapePositionY, hudtext1.Text);
  RefreshLabel;
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
  moon1.Orientation:=CameraOrientation;
  Mark(shapePositionX, shapePositionY, hudtext1.Text);
  RefreshLabel;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  if lockrot then
    exit;
  case RadioGroup2.ItemIndex of
    0: PoleOrientation := 0;
    1: PoleOrientation := 180;
  end;
  ToolButton4.Down := (RadioGroup2.ItemIndex = 1);
  if FollowNorth then
    CameraOrientation := rmod(-PA + PoleOrientation + 360, 360)
  else
    CameraOrientation := Poleorientation;
  moon1.Orientation:=CameraOrientation;
  Mark(shapePositionX, shapePositionY, hudtext1.Text);
  RefreshLabel;
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
  MeasuringDistance := not MeasuringDistance;
  if measuringdistance then
  begin
    Button11.Caption      := m[53];
    GLSceneViewer1.Cursor := crCross;
  end
  else
  begin
    Button11.Caption      := m[52];
    GLSceneViewer1.Cursor := crRetic;
    HUDSprite2.Visible    := False;
  end;
end;

procedure TForm1.Distance1Click(Sender: TObject);
begin
  if multi_instance and (clientwidth = ClientHeight - controlbar1.Height -
    statusbar1.Height) then
    clientwidth := round(1.333 * clientwidth);
  Pagecontrol1.ActivePage := Outils.Caption;
  PageControl1Change(Sender);
  Button11.Caption  := m[53];
  MeasuringDistance := False;
  Button11Click(Sender);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  FollowNorth := CheckBox1.Checked;
  if FollowNorth then
    CameraOrientation := rmod(-PA + PoleOrientation + 360, 360)
  else
    CameraOrientation := Poleorientation;
  moon1.Orientation:=CameraOrientation;
  RefreshLabel;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  ToolButton6.Down := checkbox2.Checked;
  moon1.Mirror:=checkbox2.Checked;
  Mark(shapePositionX, shapePositionY, hudtext1.Text);
//  RefreshLabel;
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
      SnapShot(b, saveimagewhite);
      b.SaveToFile(ChangeFileExt(savedialog1.FileName, '.bmp'));
    finally
      b.Free;
    end;
  end;
end;

procedure TForm1.BMP15001Click(Sender: TObject);
// pour faire les images 2D
var
  b:  tbitmap;
  sl: boolean;
begin
  savedialog1.DefaultExt := '.bmp';
  savedialog1.Filter     := 'bmp image|*.bmp';
  savedialog1.FileName   := combobox1.Text;
  if savedialog1.Execute then
  begin
    b := Tbitmap.Create;
    try
      sl := showautoLabel;
      showautoLabel := False;
      RefreshLabel;
      Rendertobitmap(b, 1500, False);
      b.SaveToFile(ChangeFileExt(savedialog1.FileName, '.bmp'));
    finally
      showautoLabel := sl;
      b.Free;
    end;
  end;
end;

procedure TForm1.BMP30001Click(Sender: TObject);
var
  b:  tbitmap;
  sl: boolean;
begin
  savedialog1.DefaultExt := '.bmp';
  savedialog1.Filter     := 'bmp image|*.bmp';
  savedialog1.FileName   := combobox1.Text;
  if savedialog1.Execute then
  begin
    b := Tbitmap.Create;
    try
      sl := showautoLabel;
      showautoLabel := False;
      RefreshLabel;
      Rendertobitmap(b, 3000, False);
      b.SaveToFile(ChangeFileExt(savedialog1.FileName, '.bmp'));
    finally
      showautoLabel := sl;
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
      SnapShot(b, saveimagewhite);
      j.Assign(b);
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
    SnapShot(b, False);
    b.SaveToFile(slash(tempdir) + fn);
    ShowImg(tempdir, fn, False);
  finally
    b.Free;
  end;
end;

procedure TForm1.Selectiondimprimante1Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
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
    xmin := Canvas.ClipRect.left + round(LeftMargin * PrinterResolution / 25.4);
    xmax := Canvas.ClipRect.right;
    ymin := Canvas.ClipRect.top;
    ymax := Canvas.ClipRect.bottom;
    Canvas.Font.Name := memo2.Font.Name;
    Canvas.Font.Color := clBlack;
    Canvas.Font.Size := 8;
    hl   := round(Canvas.TextHeight('H') * 1.1);
    Canvas.Font.Style := [fsBold];
    buf1 := UTF8Encode(form1.Caption)+':  '+vmaurl;
    Canvas.TextOut(Xmin + 10, ymin, buf1);
    Canvas.Font.Style := [];
    if language = 'FR' then
      buf1 := UTF8Encode('Lunar formations database V2.1 © Ch. Legrand,  Reproduction interdite / Pour usage personnel uniquement')
    else
      buf1 := UTF8Encode('Lunar formations database V2.1 © Ch. Legrand,  Forbidden copy / For personal use only');
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
      snapshot(b, True);
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
  Desc1.CopyToClipboard;
end;

procedure TForm1.SelectAll1Click(Sender: TObject);
begin
  Desc1.SelectAll;
end;

procedure TForm1.LgendeGologique1Click(Sender: TObject);
var
  fn: string;
begin
  chdir(appdir);
  fn := 'LegendGeo_' + language + '.jpg';
  if not fileexists(slash('Textures') + fn) then
    fn := 'LegendGeo_UK.jpg';
  showimg('Textures', fn, True);
end;

procedure TForm1.ToolButton7Click(Sender: TObject);
var
  param, fx, fy: string;
  i: integer;
begin
{$ifdef mswindows}
  i := findwindow(nil, PChar('PHOTLUN'));
  if i = 0 then
    ClosePhotlun := True;
{$endif}
  if flipx < 0 then
    fx := '1'
  else
    fx := '0';
  if poleorientation > 0 then
    fy := '1'
  else
    fy := '0';
  param := ' -fx ' + fx + ' -fy ' + fy;
  param := param + ' -nx -n "' + currentname + '"';
  ExecNoWait(Photlun + param);
end;

procedure TForm1.OverlayCaption1Click(Sender: TObject);
var
  dir: string;
begin
  chdir(appdir);
  dir := slash('Textures') + slash('overlay') + slash('caption');
  if fileexists(dir + overlayname) then
    showimg(dir, overlayname, True);
end;

procedure SetHires500;
begin
  // no more used
end;

procedure SetHires;
begin
  // no more used
end;

procedure TForm1.LabelTimerTimer(Sender: TObject);
begin
  LabelTimer.Enabled := False;
  SetHires;
  if (showautoLabel or (CurrentSelection <> '')) and (zoom >= 1) and
    (not RotationCadencer.Enabled) and (not TelescopeTimer.Enabled) then
    SetLabel;
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
begin
  if ToolButton3.Down then
  begin
    librl := 0;
    librb := 0;
    GroupBox4.Visible := False;
    ToolButton2.Enabled := False;
    checkbox2.Visible := False;
    ToolButton6.Enabled := False;
    form1.Arrowline1.Visible := False;
    moon1.Mirror:=False;
    GroupBox4.Visible := False;
    GroupBox3.Visible := True;
    LibrationButton.Enabled := False;
    lrot := 0;
    moon1.VisibleSideLock:=false;
    moon1.Zoom:=1;
{    setzoom(minfoc);
    movecamera(0, 0);
    GLSceneViewer1MouseMove(Sender, [ssLeft], lastx, lasty);   }
    Rotation1.Visible := True;
  end
  else
  begin
    checkbox2.Visible := True;
    ToolButton6.Enabled := True;
    ToolButton2.Enabled := True;
    RotationCadencer.Enabled := False;
    Rotation1.Visible := False;
    GroupBox3.Visible := False;
    LibrationButton.Enabled := True;
    GroupBox4.Visible := True;
    lrot := 0;
    moon1.Zoom:=1;
{    setzoom(minfoc);
    movecamera(0, 0); }
    case RadioGroup2.ItemIndex of
      0: CameraOrientation := 0;
      1: CameraOrientation := 180;
    end;
    moon1.Orientation:=CameraOrientation;
    moon1.VisibleSideLock:=true;
    moon1.Mirror:=checkbox2.Checked;
    RefreshMoonImage;
  end;
end;

procedure TForm1.Button14Click(Sender: TObject);
begin
  Glsceneviewer1.Buffer.ShowInfo;
end;

procedure TForm1.ZoomEyepieceClick(Sender: TObject);
begin
  with Sender as TMenuItem do
    CurrentEyepiece := tag;
  if CurrentEyepiece = 0 then
  begin
    annulus1.Visible := False;
    GLsceneViewer1.Buffer.BackgroundColor := clBlack;
  end
  else
  begin
    SetZoom(minfoc * (diam / 60) / eyepiecefield[CurrentEyepiece]);
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
    GLsceneViewer1.Buffer.BackgroundColor := $00202020;
    annulus1.Visible := True;
  end;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  doublebuf := CheckBox3.Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  stencilbuf := CheckBox4.Checked;
end;

procedure TForm1.CheckBox8Click(Sender: TObject);
begin
  compresstexture := CheckBox8.Checked;
end;

procedure TForm1.InitTelescope;
var
  fs:   TSearchRec;
  i, p: integer;
  buf:  string;
begin
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
    GroupBox4.Visible := True;
    if scopeinterface <> '' then
      Combobox5.Text := scopeinterface
    else
      Combobox5.Text := Combobox5.items[0];
    scopeinterface := ComboBox5.Text;
  end;
end;

procedure TForm1.ComboBox5Change(Sender: TObject);
begin
  scopeinterface := ComboBox5.Text;
end;

// Scope menu
procedure TForm1.Button16Click(Sender: TObject);
begin
  if not scopelibok then
    InitScopeLibrary(slash(appdir) + scopeinterface + '.tid');
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
  ok1, ok2, ok3: boolean;
  ii:   integer;
  buf:  shortstring;
  r, d: double;
begin
  if scopelibok and scopeconnected and marked then
  begin
    ScopeGetInfo(buf, OK1, OK2, OK3, ii);
    if not ok3 then
      exit;
    Moon2RaDec(markx, marky, r, d);
    ScopeGoto(r, d, ok1);
  end
  else
    ShowMessage('Please first connect the telescope and select a formation.');
  // add to translation!
end;

// Sync button
procedure TForm1.Button18Click(Sender: TObject);
var
  ok1, ok2, ok3: boolean;
  ii:   integer;
  buf:  shortstring;
  r, d: double;
begin
  if scopelibok and scopeconnected and marked then
  begin
    ScopeGetInfo(buf, OK1, OK2, OK3, ii);
    if not ok2 then
      exit;
    Moon2RaDec(markx, marky, r, d);
    ScopeAlign('markname', r, d);
  end
  else
    ShowMessage('Please first connect the telescope and select a formation.');
end;

// track
procedure TForm1.CheckBox6Click(Sender: TObject);
var
  ok1, ok2, ok3: boolean;
  ii:  integer;
  buf: shortstring;
begin
  ok1 := False;
  if scopelibok and scopeconnected then
    ScopeGetInfo(buf, OK1, OK2, OK3, ii);
  if not ok1 then
    CheckBox6.Checked := False
  else
  begin
    TelescopeTimer.Interval := max(250, ii);
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
  r, d, x, y: double;
  ok:  boolean;
  buf: string;
begin
  ScopeGetRaDec(r, d, ok);
  if ok then
  begin
    initdate;
    ok  := marked;
    x   := markx;
    y   := marky;
    buf := markname;
    RefreshMoonImage;
    if ok then
      mark(x, y, buf);
    RaDec2Moon(r, d, x, y);
    if (abs(x) < 0.5) and (abs(y) < 0.5) then
      MoveCamera(x, y);
    if CheckBox7.Checked and marked and
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
  djd(nmjd + (timezone - DT_UT) / 24, aa, mm, dd, hh);
  form1.labelnm.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);
  djd(fqjd + (timezone - DT_UT) / 24, aa, mm, dd, hh);
  form1.labelfq.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);
  djd(fmjd + (timezone - DT_UT) / 24, aa, mm, dd, hh);
  form1.labelfm.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);
  djd(lqjd + (timezone - DT_UT) / 24, aa, mm, dd, hh);
  form1.labellq.Caption := date2str(aa, mm, dd) + ' ' + timmtostr(hh);
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
var
  param: string;
  i:     integer;
begin
  { TODO : new window }
  // desactivated until new Moon object is implemented
end;

procedure TForm1.LibrationButtonClick(Sender: TObject);
begin
  librationeffect := LibrationButton.Down;
  RefreshMoonImage;
end;

procedure TForm1.PhaseButtonClick(Sender: TObject);
begin
  phaseeffect := PhaseButton.Down;
  RefreshMoonImage;
end;

procedure TForm1.DataBase1Click(Sender: TObject);
var
  param: string;
  i:     integer;
begin
{$ifdef mswindows}
  i := findwindow(nil, PChar(VMAbrowser));
  if i = 0 then
    CloseVMAbrowser := True;
{$endif}
  param := ' -nx ';
  ExecNoWait(Datlun + param);
end;

procedure TForm1.SpeedButton8Click(Sender: TObject);
begin
  form2.colordialog1.color := autolabelcolor;
  if form2.colordialog1.Execute then
  begin
    autolabelcolor := form2.colordialog1.color;
    moon1.Labelcolor:=autolabelcolor;
    InitLabel;
    RefreshLabel;
  end;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  RemoveMark1.Visible := (CurrentSelection <> '');
end;

procedure TForm1.RemoveMark1Click(Sender: TObject);
begin
  CurrentSelection := '';
  RefreshLabel;
end;


 ///////////////////////////////////////////////////////////////////
 //  To be moved to pu_moon
 ///////////////////////////////////////////////////////////////////

function SetWhiteColor(x: integer): TColor;
begin
  Result := x + (x shl 8) + (x shl 16);
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  GLlightSource1.Ambient.AsWinColor := SetWhitecolor(Trackbar2.position);
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
  GLlightSource1.diffuse.AsWinColor := SetWhitecolor(Trackbar3.position);
end;

procedure TForm1.TrackBar4Change(Sender: TObject);
begin
  GLlightSource1.specular.AsWinColor := SetWhitecolor(Trackbar4.position);
end;

{procedure TForm1.SetMirror(onoff: boolean);
begin
  with form1 do
  begin
    if onoff then
    begin
      GLMirror1.Visible     := True;
      glcamera1.Direction.Z := -1;
      glcamera1.Direction.X := 0;
      glcamera1.Direction.Y := 0;
    end
    else
    begin
      GLMirror1.Visible     := False;
      glcamera1.Direction.Z := 1;
      glcamera1.Direction.X := 0;
      glcamera1.Direction.Y := 0;
    end;
    flipx := sgn(GLCamera1.Direction.z);
  end;
end;    }

procedure TForm1.ShowSphere;
// hide non visible sphere to improve performance
var
  ls: double;
  m:  string;

{  function Shows(l: double; sphere: TGLSphere; n: boolean): boolean;
  var
    p: double;
  begin
    if (n and (librb > 0.5)) or (not n and (librb < -0.5)) then
      Result := True
    else
    begin
      p := rad2deg*angulardistance(deg2rad*l, 0, deg2rad*(sphere.Start + sphere.Stop) / 2, 0);
      if p < 135 then
        Result := True
      else
        Result := False;
    end;
    sphere.Visible := Result;
  end;}

begin
{  GLSceneViewer1.Buffer.BeginUpdate;
  sphere1.Visible := False;
  sphere2.Visible := False;
  sphere3.Visible := False;
  sphere4.Visible := False;
  sphere5.Visible := False;
  sphere6.Visible := False;
  sphere7.Visible := False;
  sphere8.Visible := False;
  ls := round(rmod(180 - dummycube1.TurnAngle + 360, 360));
  m  := '';
  if Shows(ls, sphere1, True) then
    m := m + ' 1';
  if Shows(ls, sphere2, True) then
    m := m + ' 2';
  if Shows(ls, sphere3, False) then
    m := m + ' 3';
  if Shows(ls, sphere4, False) then
    m := m + ' 4';
  if Shows(ls, sphere5, True) then
    m := m + ' 5';
  if Shows(ls, sphere6, True) then
    m := m + ' 6';
  if Shows(ls, sphere7, False) then
    m := m + ' 7';
  if Shows(ls, sphere8, False) then
    m := m + ' 8';
  GLSceneViewer1.Buffer.EndUpdate;
  //  showmessage(m);      }
end;

procedure TForm1.SetCompression(onoff: boolean);
var
  cmp: TGLTextureCompression;
  i:   integer;
begin
  if onoff then
  begin
    cmp := tcStandard;
  end
  else
  begin
    cmp := tcDefault;
  end;
  for i := 0 to 19 do
  begin
    GLMaterialLibrary1.Materials[i].Material.Texture.Compression := cmp;
  end;
end;

function TForm1.ProjMoon(l, b, lc, bc: double; var X, Y: double): boolean;
var
  hh, s1, s2, s3, c1, c2, c3: extended;
begin
  hh := l - lc + lrot;
  if hh > 180 then
    hh := hh - 360;
  if hh < -180 then
    hh := hh + 360;
  if (abs(hh) > 95) then
  begin
    Result := False;
    exit;
  end;   // tolerance sur le limbe.
  sincos(Deg2Rad * bc, s1, c1);
  sincos(Deg2Rad * b, s2, c2);
  sincos(Deg2Rad * hh, s3, c3);
  x      := -(c2 * s3);
  y      := (s2 * c1 - c2 * s1 * c3);
  x      := sgn(x) * MinValue([abs(x), 10000]) / 2;
  y      := sgn(y) * MinValue([abs(y), 10000]) / 2;
  Result := True;
end;

function TForm1.InvProjMoon(x, y, lc, bc: double; var l, b: double): boolean;
var
  r, s1, c1, s: extended;
begin
  sincos(deg2rad * bc, s1, c1);
  s := 1 - x * x - y * y;
  if (s >= 0) then
  begin
    r := sqrt(s);
    l := lc - lrot - rad2deg * arctan2(x, (c1 * r - y * s1));
    if l > 180 then
      l := l - 360;
    if l < -180 then
      l    := 360 + l;
    b      := rad2deg * arcsin(y * c1 + s1 * r);
    Result := True;
  end
  else
  begin
    l      := nan;
    b      := nan;
    Result := False;
  end;
end;

procedure TForm1.InitLabel;
var
  i: integer;
  newlabel: TGLHUDText;
begin
  form1.DummyCube4.DeleteChildren;
  for i := 0 to Maxlabel do
  begin
    newlabel      := TGLHUDText(form1.DummyCube4.AddNewChild(TGLHUDText));
    newlabel.Name := 'MoonLabel' + IntToStr(i);
    newlabel.Visible := False;
    newlabel.BitmapFont := form1.Bitmapfont1;
    newlabel.Layout := tlCenter;
    newlabel.Up.SetVector(0, 1, 0);
    newlabel.Scale.SetVector(Label3dSize * LabelSize, Label3dSize * LabelSize, 1);
    newlabel.ModulateColor.AsWinColor := autolabelcolor;
  end;
end;

procedure TForm1.MoreSprite;
var
  i: integer;
  newSprite: TGLHUDSprite;
begin
  for i := 0 to InitialSprite - 1 do
  begin
    newSprite      := TGLHUDSprite(form1.DummyCube5.AddNewChild(TGLHUDSprite));
    newSprite.Name := 'MoonSprite' + IntToStr(maxsprite + i);
    newSprite.Visible := False;
    newSprite.Material.FrontProperties.Emission.AsWinColor := MarkColor;
    newSprite.Material.FrontProperties.Ambient.Color := clrBlack;
    newSprite.Material.FrontProperties.Diffuse.Color := clrBlack;
    newSprite.Material.FrontProperties.Specular.Color := clrBlack;
    newSprite.Up.SetVector(0, 1, 0);
    newSprite.Scale.SetVector(1, 1, 1);
    newSprite.Width  := marksize;
    newSprite.Height := marksize;
  end;
  maxsprite := maxsprite + InitialSprite;
end;

procedure TForm1.InitSprite;
begin
  maxsprite := 0;
  DummyCube5.DeleteChildren;
  MoreSprite;
end;

procedure TForm1.ClearLabel;
var
  i: integer;
begin
  if (DummyCube4.Count > 0) and DummyCube4.Children[0].Visible then
    for i := 0 to Maxlabel do
      with DummyCube4.Children[i] as TGLHUDText do
        Visible := False;
  if (DummyCube5.Count > 0) and DummyCube5.Children[0].Visible then
    for i := 0 to MaxSprite - 1 do
      with DummyCube5.Children[i] as TGLHUDSprite do
        Visible := False;
end;

procedure TForm1.Mark(x, y: double; txt: string);
var
  xx, yy: integer;
begin
  marked := False;
  markx  := x;
  marky  := y;
  shapepositionX := x;
  shapepositionY := y;
  hudsprite1.Width := marksize;
  hudsprite1.Height := marksize;
  if (x = 0) and (y = 0) then
  begin
    hudsprite1.Visible := False;
    hudtext1.Visible   := False;
  end
  else
  begin
    glscene1.BeginUpdate;
    world2window(x, y, xx, yy);
    marked   := True;
    markname := txt;
    if showmark then
    begin
      hudsprite1.Visible    := True;
      hudsprite1.Material.FrontProperties.Emission.AsWinColor := MarkColor;
      hudsprite1.Position.X := xx;
      hudsprite1.Position.Y := yy;
    end
    else
      hudsprite1.Visible := False;
    if showlabel then
    begin
      hudtext1.Visible := True;
      hudtext1.Position.Y := yy;
      hudtext1.Text := txt;
      if xx < (glsceneviewer1.Width / 2) then
      begin
        hudtext1.Position.X := xx + 4 * BitmapFont1.HSpace;
        hudtext1.Alignment  := taLeftJustify;
      end
      else
      begin
        hudtext1.Position.X := xx - 4 * BitmapFont1.HSpace;
        hudtext1.Alignment  := taRightJustify;
      end;
    end
    else
      hudtext1.Visible := False;
    glscene1.EndUpdate;
  end;
end;


procedure TForm1.LibrationMark(librl, librb: double);
var
  a, x, y: double;
  xx, yy:  integer;
begin
  with Arrowline1 do
    if ShowLibrationMark and (not ToolButton3.Down) then
    begin
      Visible := True;
      a      := arctan2(librb, -librl);
      x      := sqrt(librb * librb + librl * librl);
      Height := x * 0.004 / zoom;
      position.X := (0.5 + Height / 1.5) * cos(a);
      position.Y := (0.5 + Height / 1.5) * sin(a);
      TopRadius := Height / 10;
      BottomRadius := TopRadius;
      TopArrowHeadRadius := 3 * TopRadius;
      TopArrowHeadHeight := 6 * TopRadius;
      Turnangle := -rad2deg * a;
      Material.FrontProperties.Emission.AsWinColor := MarkColor;
    end
    else
    begin
      Visible := False;
    end;
end;

procedure TForm1.SetScrollBar(xx, yy: extended);
var
  x1, y1, s1, c1: extended;
begin
  sincos(deg2rad * CameraOrientation, s1, c1);
  x1 := (xx * c1 - yy * s1);
  y1 := (xx * s1 + yy * c1);
  form1.scrollbar1.Position := -round(x1 * 1000);
  form1.scrollbar2.Position := -round(y1 * 1000);
end;

procedure TForm1.MoveCamera(x, y: double);
var
  xx, yy: integer;
begin
  curx := x;
  cury := y;
  with form1 do
  begin
    glscene1.BeginUpdate;
    HUDSprite2.Visible    := False;
    GLSceneViewer1.Cursor := crRetic;
    GLCamera1.Position.y  := y;
    GLCamera1.Position.x  := x;
    Annulus1.Position.x   := GLCamera1.Position.x;
    Annulus1.Position.y   := GLCamera1.Position.y;
    SetScrollBar(x, y);
    Mark(shapePositionX, shapePositionY, hudtext1.Text);
    RefreshLabel;
    glscene1.EndUpdate;
  end;
end;


procedure TForm1.SetMinFilter(onoff: boolean);
var
  mi: TGLMinFilter;
  i:  integer;
begin
  if onoff then
  begin
    mi := miNearestMipmapNearest;
  end
  else
  begin
    mi := miLinear;
  end;
  GLScene1.BeginUpdate;
  for i := 0 to 8 do
  begin
    GLMaterialLibrary1.Materials[i].Material.Texture.MinFilter := mi;
  end;
  GLScene1.EndUpdate;
  GLSceneViewer1.Update;
end;

procedure TForm1.SetZoom(yy: double);
var
  t:  double;
  dx: integer;
begin
  if maxfoc > 0 then
  begin
    glscene1.BeginUpdate;
    HUDSprite2.Visible := False;
    t := yy;
    if yy < 25 then
      yy := 25;
    if yy > maxfoc then
      yy   := maxfoc;
    t      := yy / t;
    EyepieceRatio := t;
    curfoc := yy;
    GLCamera1.FocalLength := yy;
    zoom   := GLCamera1.FocalLength / minfoc;
    if currenteyepiece > 0 then
    begin
      annulus1.BottomInnerRadius := t * 0.5 / zoom;
      statusbar1.Panels[3].Text  := m[43] + ' ' + formatfloat(f1, t * minfoc * (diam / 60) / yy) + lmin;
    end
    else
      statusbar1.Panels[3].Text := m[43] + ' ' + formatfloat(f1, 100 * (diam / 60) / yy) + lmin;
    locktrackbar := True;
    trackbar1.position := round(100 * log10(GLCamera1.FocalLength));
    Mark(shapePositionX, shapePositionY, hudtext1.Text);
    LibrationMark(librlong, librlat);
    RefreshLabel;
    glscene1.EndUpdate;
  end;
end;

procedure TForm1.Window2World(x, y: integer; var xx, yy: double);
var
  k, s1, c1, x1, y1: extended;
begin
  k := (zoom * (minfoc / 100) * GLSceneViewer1.Width);
  sincos(deg2rad * CameraOrientation, s1, c1);
  x1 := flipx * (x - GLSceneViewer1.Width / 2);
  y1 := (y - GLSceneViewer1.Height / 2);
  xx := (x1 * c1 + y1 * s1);
  yy := (-x1 * s1 + y1 * c1);
  xx := GLCamera1.Position.x - xx / k;
  yy := GLCamera1.Position.y - yy / k;
end;

procedure TForm1.World2Window(xx, yy: double; var x, y: integer);
var
  k, s1, c1, x1, y1: extended;
begin
  k := (zoom * (minfoc / 100) * GLSceneViewer1.Width);
  sincos(deg2rad * CameraOrientation, s1, c1);
  xx := k * (GLCamera1.Position.x - xx);
  yy := k * (GLCamera1.Position.y - yy);
  x1 := (xx * c1 - yy * s1);
  y1 := (xx * s1 + yy * c1);
  x  := round(flipx * x1 + GLSceneViewer1.Width / 2);
  y  := round(y1 + GLSceneViewer1.Height / 2);
end;

procedure TForm1.SetCameraOrientation;
var
  c1, s1: extended;
begin
  sincos(deg2rad * CameraOrientation, s1, c1);
  HUDSprite2.Visible := False;
  GLCamera1.BeginUpdate;
  GLCamera1.Up.X := s1;
  GLCamera1.Up.Y := c1;
  GLCamera1.Up.X := s1;
  GLCamera1.Up.Y := c1;
  GLCamera1.Up.X := s1;
  GLCamera1.Up.Y := c1;
  GLCamera1.EndUpdate;
  SetScrollBar(GLCamera1.Position.x, GLCamera1.Position.y);
  Mark(shapePositionX, shapePositionY, hudtext1.Text);
end;


procedure TForm1.LoadOverlay(fn: string; lum: integer);
var
  i, n, ws, hs, x0, y0: integer;
  b:   Tbitmap;
  j:   Tjpegimage;
  p:   Pbytearray;
  src, dest: Trect;
  max: double;
const
  slice: array[1..8] of integer = (6, 1, 2, 5, 8, 3, 4, 7);
begin
  if showoverlay and fileexists(Slash(appdir) + Slash('Textures') + Slash('overlay') + fn) then
  begin
    j := TJpegImage.Create;
    b := Tbitmap.Create;
    try
      j.LoadFromFile(Slash(appdir) + Slash('Textures') + Slash('overlay') + fn);
      overlayimg.Assign(j);
      { TODO : adjust new overlay alpha chanel }
{   if (lum<>0) then begin
   end;}
      ws      := overlayimg.Width div 4;
      hs      := overlayimg.Height div 2;
      b.Width := ws;
      b.Height := hs;
      for i := 1 to 8 do
      begin
        n := slice[i] + 8;
        if i <= 4 then
        begin
          x0 := (i - 1) * ws;
          y0 := 0;
        end
        else
        begin
          x0 := (i - 5) * ws;
          y0 := hs;
        end;
        dest := rect(0, 0, ws, hs);
        src  := rect(x0, y0, x0 + ws, y0 + hs);
        b.Canvas.CopyRect(dest, overlayimg.canvas, src);
        GLmaterialLibrary1.Materials[n].Material.Texture.Disabled := False;
        GLmaterialLibrary1.Materials[n].Material.Texture.Image.Assign(b);
        GLMaterialLibrary1.Materials[n].TextureScale.X := 0.999;
        GLMaterialLibrary1.Materials[n].TextureScale.Y := 0.999;
      end;
      if fileexists(slash('Textures') + slash('overlay') + slash('caption') + overlayname) then
      begin
        OverlayCaption1.Caption := remext(overlayname) + ' ' + 'Caption';
        OverlayCaption2.Caption := OverlayCaption1.Caption;
        OverlayCaption1.Visible := True;
        OverlayCaption2.Visible := True;
      end
      else
      begin
        OverlayCaption1.Visible := False;
        OverlayCaption2.Visible := False;
      end;
    finally
      j.Free;
      b.Free;
    end;
  end
  else
  begin
    showoverlay := False;
    OverlayCaption1.Visible := False;
    OverlayCaption2.Visible := False;
    for i := 1 to 8 do
    begin
      n := slice[i] + 8;
      GLmaterialLibrary1.Materials[n].Material.Texture.Disabled := True;
    end;
  end;
end;


procedure TForm1.InitGraphic(Sender: TObject);
var
  j:  Tjpegimage;
  b:  Tbitmap;
  fn: string;

{  procedure LoadTexture(n, r: integer; img: string);
  begin
    GLmaterialLibrary1.Materials[n].Material.Texture.Disabled := False;
    GLMaterialLibrary1.Materials[n].TextureScale.X := 0.999;
    GLMaterialLibrary1.Materials[n].TextureScale.Y := 0.999;
    j.LoadFromFile(Slash(appdir) + Slash('Textures') + img);
    if r = 1 then
      GLmaterialLibrary1.Materials[n].Material.Texture.Image.Assign(j)
    else
    begin
      b.Width  := j.Width div r;
      b.Height := j.Height div r;
      b.Canvas.StretchDraw(rect(0, 0, b.Width, b.Height), j);
      GLmaterialLibrary1.Materials[n].Material.Texture.Image.Assign(b);
    end;
  end; }

begin
  PanelMoon.Visible := True;
  InitLabel;
  InitSprite;

{  j := TJpegImage.Create;
  b := Tbitmap.Create;
  lockrot := True;
  if PoleOrientation = 0 then
    RadioGroup2.ItemIndex := 0
  else
    RadioGroup2.ItemIndex := 1;
  lockrot := False;
//  panel2.Visible := True;
  GLSceneViewer1.Cursor := crRetic;
  try
    OpenHires(False);
  except
  end;
  LoadTexture(0, reducetexture, 'moon' + imgsuffix + '_c1.jpg');
  LoadTexture(1, reducetexture, 'moon' + imgsuffix + '_c2.jpg');
  LoadTexture(2, reducetexture, 'moon' + imgsuffix + '_c3.jpg');
  LoadTexture(3, reducetexture, 'moon' + imgsuffix + '_c4.jpg');
  LoadTexture(4, reducetexturefar, 'moon' + imgsuffix + '_c5.jpg');
  LoadTexture(5, reducetexturefar, 'moon' + imgsuffix + '_c6.jpg');
  LoadTexture(6, reducetexturefar, 'moon' + imgsuffix + '_c7.jpg');
  LoadTexture(7, reducetexturefar, 'moon' + imgsuffix + '_c8.jpg');
  minfoc := 95;
  maxfoc := round(maxfocbase * GLmaterialLibrary1.Materials[
    0].Material.Texture.Image.Height / 1024);
//  trackbar1.Min := round(100 * log10(minfoc));
//  trackbar1.Max := round(100 * log10(maxfoc));
//  trackbar1.position := round(100 * log10(minfoc));
  checkbox1.Checked := FollowNorth;
  SetCameraOrientation;

  Reglage.TabVisible := True;
  label11.Visible    := True;
  trackbar3.Visible  := True;
  label12.Visible    := True;
  trackbar4.Visible  := True;
  panel8.Visible     := True;
  button14.Visible   := True;
  groupbox2.Visible  := True;

  Outils.TabVisible := True;
  Button12.Visible  := True;
  Button13.Visible  := True;
  checkbox1.Visible := True;
  Distance1.Visible := True;
  Enregistrersous1.Visible := True;
  bitmapfont1.Glyphs.LoadFromFile(Slash(appdir) + Slash('Textures') + 'font_label_bold.bmp');
  HUDText1.ModulateColor.AsWinColor := labelcolor;
  HUDText1.Scale.SetVector(Label3dSize * LabelSize, Label3dSize * LabelSize, 1);
  Trackbar2.position := GLlightSource1.ambient.AsWinColor and $FF;
  Trackbar3.position := GLlightSource1.diffuse.AsWinColor and $FF;
  Trackbar4.position := GLlightSource1.specular.AsWinColor and $FF;
  if sphere1.Slices = 180 then
    Trackbar5.position := 3
  else if sphere1.Slices = 90 then
    Trackbar5.position := 2
  else if sphere1.Slices = 45 then
    Trackbar5.position := 1
  else
    Trackbar5.position := 1;
  CheckBox3.Checked := doublebuf;
  CheckBox4.Checked := stencilbuf;
  CheckBox8.Checked := compresstexture;
  SetMinFilter(MipMaps);
  LoadOverlay(overlayname, overlaylum);
  j.Free;
  b.Free;  }
end;

procedure TForm1.OrientLightSource(phase, sunincl: double);
begin
  glscene1.BeginUpdate;
  GLLightSource1.BeginUpdate;
  GLLightSource1.Position.z := -LightDist * cos(degtorad(phase));
  GLLightSource1.Position.x := -LightDist * sin(degtorad(phase));
  if abs(sunincl) > 89.9 then
    sunincl := sgn(sunincl) * 89.9;
  GLLightSource1.Position.y := -LightDist * tan(degtorad(-sunincl));
  GLLightSource1.SpotDirection.x := GLLightSource1.Position.x;
  GLLightSource1.SpotDirection.y := GLLightSource1.Position.y;
  GLLightSource1.SpotDirection.z := GLLightSource1.Position.z;
  GLLightSource1.EndUpdate;
  glscene1.EndUpdate;
end;

procedure TForm1.resetorientation;
begin
  with DummyCube1 do
  begin
    RollAngle := 0;
    TurnAngle := 0;
    PitchAngle := 0;
    direction.X := 0;
    direction.Y := 0;
    direction.Z := 1;
    Up.X := 0;
    Up.Y := 1;
    Up.Z := 0;
  end;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  xx, yy: double;
begin
  clickX := X;
  clickY := Y;
  if measuringdistance and (button = mbLeft) then
  begin
    window2world(x, y, xx, yy);
    startx  := x;
    starty  := y;
    startxx := xx;
    startyy := yy;
    InvProjMoon(2 * xx, 2 * yy, librl, librb, startl, startb);
    distancestart := True;
    mark(0, 0, '');
    HUDSprite2.Visible    := True;
    HUDSprite2.Material.FrontProperties.Emission.AsWinColor := MarkColor;
    HUDSprite2.Width      := 1;
    HUDSprite2.Position.X := startx;
    HUDSprite2.Position.Y := starty;
    HUDSprite2.Rotation   := 0;
  end;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  xx, yy, l, b: double;
begin
{  if (not GLSceneViewer1.Focused) then
    ActiveControl := GLSceneViewer1; }
  if (abs(clickX - X) > 3) or (abs(clickY - Y) > 3) then
  begin
    try
      lockmove := True;
      if shift = [ssLeft] then
      begin
        if measuringdistance and distancestart then
        begin
          // distance
          MeasureDistance(x, y);
          ShowCoordinates(x, y);
        end
        else if ToolButton3.down then
        begin
          // rotation
          SkipIdent := True;
          window2world(x, y, xx, yy);
          window2world(lastx, lasty, l, b);
          lrot := rmod(100 * (l - xx) + lrot + 360, 360);
          if lrot > 180 then
            lrot := lrot - 360;
          yy     := b - yy + GLCamera1.Position.y;
          lastx  := x;
          lasty  := y;
          glscene1.BeginUpdate;
          resetorientation;
          dummycube1.PitchAngle  := librb;
          dummycube1.TurnAngle   := -librl + lrot;
          GLLightSource2.Shining := not phaseeffect;
          if abs(yy) < 0.5 then
            GLCamera1.Position.y := yy;
          Annulus1.Position.x    := GLCamera1.Position.x;
          Annulus1.Position.y    := GLCamera1.Position.y;
          SetScrollBar(GLCamera1.Position.x, GLCamera1.Position.y);
          mark(0, 0, '');
          ShowSphere;
          RefreshLabel;
          glscene1.EndUpdate;
        end
        else
        begin
          // deplacement
          SkipIdent := True;
          window2world(x, y, xx, yy);
          window2world(lastx, lasty, l, b);
          xx    := l - xx + GLCamera1.Position.x;
          yy    := b - yy + GLCamera1.Position.y;
          lastx := x;
          lasty := y;
          glscene1.BeginUpdate;
          if abs(xx) < 0.5 then
            GLCamera1.Position.x := xx;
          if abs(yy) < 0.5 then
            GLCamera1.Position.y := yy;
          Annulus1.Position.x    := GLCamera1.Position.x;
          Annulus1.Position.y    := GLCamera1.Position.y;
          SetScrollBar(xx, yy);
          Mark(shapePositionX, shapePositionY, hudtext1.Text);
          RefreshLabel;
          glscene1.EndUpdate;
        end;
      end
      else if shift = [ssmiddle] then
      begin
        // zoom
        yy := GLCamera1.FocalLength * (1 - (y - lastyzoom) / 100);
        lastyzoom := y;
        if yy < minfoc then
          yy := minfoc;
        SetZoom(yy);
      end
      else
      begin
        ShowCoordinates(x, y);
      end;
    finally
      lockmove := False;
    end;
  end;
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
var
  xx, yy, x1, y1, s1, c1: extended;
begin
  HUDSprite2.Visible := False;
  sincos(deg2rad * CameraOrientation, s1, c1);
  x1 := -scrollbar1.Position / 1000;
  y1 := -scrollbar2.Position / 1000;
  xx := (x1 * c1 + y1 * s1);
  yy := (-x1 * s1 + y1 * c1);
  glscene1.BeginUpdate;
  GLCamera1.Position.x := xx;
  GLCamera1.Position.y := yy;
  Annulus1.Position.x := GLCamera1.Position.x;
  Annulus1.Position.y := GLCamera1.Position.y;
  curx := xx;
  cury := yy;
  Mark(shapePositionX, shapePositionY, hudtext1.Text);
  RefreshLabel;
  glscene1.EndUpdate;
end;

procedure TForm1.MoonClickEvent(Sender: TObject; Button: TMouseButton;
                     Shift: TShiftState; X, Y: Integer;
                     OnMoon: boolean; Lon, Lat: Single);
begin
if ssLeft in Shift then begin
  if OnMoon then begin
     identLB(Rad2Deg*Lon,Rad2Deg*Lat);
     moon1.SetMark(deg2rad*currentl,deg2rad*currentb,capitalize(currentname));
  end else begin
     moon1.SetMark(0,0,'');
  end;
end;
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if measuringdistance and distancestart then
  begin
    // distance
    MeasureDistance(x, y);
    ShowCoordinates(x, y);
    distancestart := False;
  end
  else if button = mbLeft then
  begin
    if not SkipIdent then
    begin
      // identification
      IdentXY(x, y);
    end
    else
      SkipIdent := False;
  end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
  x:      double;
  hd, bg: Tpoint;
begin
  hd := glsceneviewer1.ClientToScreen(point(0, 0));
  bg := glsceneviewer1.ClientToScreen(point(glsceneviewer1.Width, glsceneviewer1.Height));
  if (mousepos.X > hd.x) and (mousepos.X < bg.x) and (mousepos.Y > hd.y) and (mousepos.Y < bg.y) then
  begin
    handled := True;
    if wheeldelta > 0 then
      x := wheelstep * zoom * minfoc
    else
      x := minfoc * zoom / wheelstep;
    if x < minfoc then
      x := minfoc;
    Setzoom(x);
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  perfdeltay := -perfdeltay;
  GLCamera1.Position.y := GLCamera1.Position.y + perfdeltay;
end;

procedure TForm1.RotationCadencerProgress(Sender: TObject;
  const deltaTime, newTime: double);
begin
  lrot := lrot + rotstep * rotdirection * deltatime;
  GLSceneViewer1MouseMove(nil, [ssLeft], lastx, lasty);
end;

procedure TForm1.SnapShot(var bmp: TBitmap; white: boolean);
var
  bmp32: TGLBitmap32;
begin
  if white then
  begin
    GLSceneViewer1.Buffer.BackgroundColor := clWhite;
    GLSceneViewer1.Update;
  end;
  bmp32 := GLSceneViewer1.Buffer.CreateSnapShot;
  bmp   := bmp32.Create32BitsBitmap;
  GLSceneViewer1.Buffer.BackgroundColor := clBlack;
  bmp32.Free;
end;

procedure TForm1.RenderToBitmap(var bmp: TBitmap; size: integer; white: boolean);
begin
  bmp.PixelFormat := pf24bit;
  bmp.Width  := size;
  bmp.Height := size;
  if white then
    GLSceneViewer1.Buffer.BackgroundColor := clWhite;
  GLSceneViewer1.Buffer.RenderToBitmap(bmp, 96);
  GLSceneViewer1.Buffer.BackgroundColor := clBlack;
end;


initialization
{$I virtualmoon1.lrs}

end.

