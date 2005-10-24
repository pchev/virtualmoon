unit virtualmoon1;
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
{$IFDEF LINUX}
  QTypes, QForms, QControls, QExtCtrls, QStdCtrls, QGraphics,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows, Forms,  StdCtrls, ExtCtrls, Graphics,
   DdeMan,
{$ENDIF}
{$IFDEF opengl}
  GLScene, GLObjects, GLMisc, GLWin32Viewer, GLTexture, Info,
  GLcontext, GLCadencer, GLBitmapFont, GLHUDObjects, GLGraphics,
  GLGraph, GLMirror, AsyncTimer, GLUtils, GLCrossPlatForm,
{$ENDIF}
  mlb2, Printers,  Controls,
  Messages, SysUtils, Classes, Dialogs,  math,
  ComCtrls, Mask, Menus, jpeg, Buttons, ToolWin,
  EnhEdits, IniFiles, Grids, BigIma, HTMLLite, passql, passqlite,
  GLGeomObjects;


type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    Fichier1: TMenuItem;
    Quitter1: TMenuItem;
    Configuration1: TMenuItem;
    Panel2: TPanel;
    PageControl1: TPageControl;
    Position: TTabSheet;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Ephemerides: TTabSheet;
    Panel4: TPanel;
    Label6: TLabel;
    Label9: TLabel;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    jour: TLongEdit;
    mois: TLongEdit;
    annee: TLongEdit;
    seconde: TLongEdit;
    minute: TLongEdit;
    heure: TLongEdit;
    Button4: TButton;
    Button5: TButton;
    Aide1: TMenuItem;
    Apropos1: TMenuItem;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    UpDown3: TUpDown;
    UpDown4: TUpDown;
    UpDown5: TUpDown;
    UpDown6: TUpDown;
    GroupBox1: TGroupBox;
    Label7: TLabel;
    Terminateur: TTabSheet;
    Reglage: TTabSheet;
    TrackBar2: TTrackBar;
    Label8: TLabel;
    TrackBar3: TTrackBar;
    Label11: TLabel;
    TrackBar4: TTrackBar;
    Label12: TLabel;
    Bevel1: TBevel;
    Label14: TLabel;
    Timer1: TAsyncTimer;
    ComboBox1: TComboBox;
    Button3: TSpeedButton;
    Button6: TSpeedButton;
    Button7: TSpeedButton;
    Button8: TSpeedButton;
    EphTimer1: TTimer;
    Panel5: TPanel;
    ListBox1: TListBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    RadioGroup1: TRadioGroup;
    DdeClientConv1: TDdeClientConv;
    DdeClientItem1: TDdeClientItem;
    ChartTimer: TTimer;
    Aide2: TMenuItem;
    StringGrid1: TStringGrid;
    PopupMenu1: TPopupMenu;
    Position1: TMenuItem;
    Image2: TMenuItem;
    Centre1: TMenuItem;
    Zoom1: TMenuItem;
    x11: TMenuItem;
    x21: TMenuItem;
    x41: TMenuItem;
    Outils: TTabSheet;
    Bevel3: TBevel;
    Button12: TButton;
    Label22: TLabel;
    Button13: TButton;
    Bevel4: TBevel;
    Label23: TLabel;
    Button11: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
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
    BMP1: TMenuItem;
    SaveDialog1: TSaveDialog;
    JPG1: TMenuItem;
    PrinterSetupDialog1: TPrinterSetupDialog;
    Selectiondimprimante1: TMenuItem;
    Memo2: TMemo;
    Desc1: ThtmlLite;
    PopupMenu2: TPopupMenu;
    Copy1: TMenuItem;
    SelectAll1: TMenuItem;
    LgendeGologique1: TMenuItem;
    StatusBar1: TStatusBar;
    LabelTimer: TTimer;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    Glossaire1: TMenuItem;
    ControlBar1: TControlBar;
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
    dbtab: TTabSheet;
    StringGrid2: TStringGrid;
    Panel6: TPanel;
    Button9: TButton;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Enregistredist: TButton;
    btnEffacer: TButton;
    ToolButton3: TToolButton;
    BMP15001: TMenuItem;
    Eyepiece1: TMenuItem;
    e11: TMenuItem;
    e21: TMenuItem;
    e31: TMenuItem;
    e41: TMenuItem;
    e51: TMenuItem;
    e61: TMenuItem;
    e71: TMenuItem;
    e81: TMenuItem;
    e91: TMenuItem;
    e101: TMenuItem;
    e01: TMenuItem;
    BMP30001: TMenuItem;
    Notes: TTabSheet;
    Memo1: TMemo;
    Panel7: TPanel;
    notes_name: TLabel;
    Button15: TButton;
    Notes1: TMenuItem;
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
    Panel2D: TPanel;
    HorzScrollBar: TScrollBar;
    VertScrollBar: TScrollBar;
    Panel3: TPanel;
    Shape2: TShape;
    Shape1: TShape;
    Label5: TLabel;
    Image1: TImage;
    DebugLabel: TLabel;
    ReglageTimer: TTimer;
    Panel8: TPanel;
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
    CheckBox19: TCheckBox;
    CheckBox5: TCheckBox;
    DummyCube1: TGLDummyCube;
    RotationCadencer: TGLCadencer;
    Rotation1: TMenuItem;
    N5seconde1: TMenuItem;
    Stop1: TMenuItem;
    N10seconde1: TMenuItem;
    N1seconde1: TMenuItem;
    N02seconde1: TMenuItem;
    N05seconde1: TMenuItem;
    EastWest1: TMenuItem;
    GroupBox3: TGroupBox;
    Label4: TLabel;
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
    Edit5: TEdit;
    trackdelay: TUpDown;
    NM: TImage;
    LabelNM: TLabel;
    LabelFQ: TLabel;
    LabelFM: TLabel;
    LabelLQ: TLabel;
    FQ: TImage;
    FM: TImage;
    LQ: TImage;
    nextM: TImage;
    prevM: TImage;
    dbm: TLiteDB;
    TrackBar6: TTrackBar;
    GLLightSource2: TGLLightSource;
    Label27: TLabel;
    SpeedButton7: TSpeedButton;
    Label28: TLabel;
    Encyclopedia1: TMenuItem;
    NewWindowButton: TToolButton;
    Snapshot1: TMenuItem;
    LibrationButton: TToolButton;
    PhaseButton: TToolButton;
    Button10: TButton;
    DataBase1: TMenuItem;
    N4: TMenuItem;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    Button19: TButton;
    Button20: TButton;
    SpeedButton8: TSpeedButton;
    ToolButton11: TToolButton;
    DummyCube5: TGLDummyCube;
    RemoveMark1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Quitter1Click(Sender: TObject);
    procedure Configuration1Click(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
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
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ToolButton9Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure Button3MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button6MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button7MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button8MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EphTimer1Timer(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ChartTimerTimer(Sender: TObject);
    procedure DdeClientItem1Change(Sender: TObject);
    procedure Aide2Click(Sender: TObject);
    procedure Position1Click(Sender: TObject);
    procedure x21Click(Sender: TObject);
    procedure x41Click(Sender: TObject);
    procedure Button12MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button13MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button11Click(Sender: TObject);
    procedure Distance1Click(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject;
      var AllowChange: Boolean);
    procedure CheckBox1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CartesduCiel1Click(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure BMP1Click(Sender: TObject);
    procedure SnapShot(var bmp : TBitmap; white:boolean);
    procedure RenderToBitmap(var bmp : TBitmap; size : integer; white:boolean);
    procedure JPG1Click(Sender: TObject);
    procedure Selectiondimprimante1Click(Sender: TObject);
    procedure Imprimer1Click(Sender: TObject);
    procedure Desc1HotSpotClick(Sender: TObject; const SRC: String;
      var Handled: Boolean);
    procedure Copy1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure LgendeGologique1Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure LabelTimerTimer(Sender: TObject);
    procedure Glossaire1Click(Sender: TObject);
    procedure x81Click(Sender: TObject);
    procedure Desc1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Button9Click(Sender: TObject);
    procedure StringGrid2SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure EnregistredistClick(Sender: TObject);
    procedure btnEffacerClick(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure BMP15001Click(Sender: TObject);
    procedure ZoomEyepieceClick(Sender: TObject);
    procedure BMP30001Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UpdNotesClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Notes1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBar2DChange(Sender: TObject);
    procedure ReglageTimerTimer(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox19Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CheckBox5Click(Sender: TObject);
    procedure RotationCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
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
    procedure TrackBar6Change(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
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
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
    procedure InitGraphic(Sender: TObject);
    procedure LoadOverlay(fn:string; lum:integer);
    Procedure SetLabel;
    function SearchAtPos(l,b: double):boolean;
    Procedure ListObject(delta: double);
    Procedure InitTelescope;
    procedure SetFullScreen;
    procedure GetDBgrid;
  end;

var
  Form1: TForm1;
  dblox,dbnotes : TMlb2;

type TDBInfo = class(TObject)
     dbnum: integer;
     end;

function InvProjMoon (x,y,lc,bc : Double ; VAR l,b : Double ):boolean;
function ProjMoon(l,b,lc,bc : Double ; VAR X,Y : Double ):boolean;
procedure World2Window(xx,yy : double; var x,y : integer);
procedure Window2World(x,y : integer; var xx,yy : double);
Procedure GetDetail(row:TResultRow; memo:Tmemo);
Procedure GetHTMLDetail(row:TResultRow; var txt:string);
Procedure RefreshMoonImage;
Procedure ShowImg(desc,nom:string; forceinternal:boolean);
function SearchName(n: string; center: boolean):boolean;
Procedure OpenHires(forcerebuild:boolean);

const AVLversion = '3.0';
      Splashversion ='Version 3.0  2005-09-28';
      d1 = '0.0';
      d2 = '0.00';
      d3 = '0.000';
      d4 = '0.0000';
      Rmoon = 1737.103;  // moon radius Km
      crlf = chr(10)+chr(13);
      tab  = chr(9);
      LightDist=20;
      cameradist=2000;
      crRetic = 5;
      crPointing = 6;
      ox=36; oy=36; os=1500; px=0.95467; py=0.95467; //image 1500x1500, lune 1432x1432
      nummessage = 75;
      MaxLabel=500;
      InitialSprite=1000;
      AbsoluteMaxSprite=5000;
      Label3dSize=1;
      Label2dSize=-11;
{$ifdef vmalight}
      maxfocbase=1200;
      IdMsg='Virtual_Moon_Atlas_Light_message';
      ExitMsg='Virtual_Moon_Atlas_Light_exit';
      versionname='Light';
{$endif}
{$ifdef vmabasic}
      maxfocbase=1200;
      IdMsg='Virtual_Moon_Atlas_Basic_message';
      ExitMsg='Virtual_Moon_Atlas_Basic_exit';
      versionname='Basic';
{$endif}
{$ifdef vmaexpert}
   {$ifndef vmapro}
      maxfocbase=1900;
      IdMsg='Virtual_Moon_Atlas_Expert_message';
      ExitMsg='Virtual_Moon_Atlas_Expert_exit';
      versionname='Expert';
   {$endif}
{$endif}
{$ifdef vmapro}
      maxfocbase=1900;
      IdMsg='Virtual_Moon_Atlas_Pro_message';
      ExitMsg='Virtual_Moon_Atlas_Pro_exit';
      versionname='Pro';
{$endif}
    VMAbrowser='VMA Browser';
    
var lastx,lasty,lastyzoom,posmin,posmax,ax,ay,MaxSprite : integer;
    ReduceTexture,ReduceTextureFar,LastIma,maximgdir,maxima,startx,starty,saveimagesize,lastscrollx,lastscrolly : Integer;
    LeftMargin, PrintTextWidth,clickX,clickY : integer;
    PrintEph, PrintDesc, MipMaps, GeologicalMap, externalimage, PrintChart,lopamdirect,doublebuf,stencilbuf,hiresok,hires500ok :Boolean;
    PicZoom: array of double;
    PicTop,PicLeft: array of integer;
    librl,librb,lrot,inclination,librlong,librlat,wheelstep,EphStep,fov,searchl,searchb,markx,marky,flipx,rotstep,lunaison : double;
    ra,dec,rad,ded,dist,dkm,diam,phase,illum,pa,sunincl,timezone,currentphase,tphase,LabelSize,bx,by,bxpos,dummy : double;
    editrow,notesrow,hi_w,hi_wd,hi_dl,hi_mult,rotdirection,searchpos : integer;
    dbedited : Boolean = false;
    SkipIdent,Firstsearch,phaseeffect,librationeffect,geocentric,FollowNorth,notesok,notesedited,labelcenter,minilabel,SafeMode : boolean;
    useOpenGL,lockmove,lockrepeat,DDEreceiveok,showlabel,showautolabel,showmark,showlibrationmark,marked,saveimagewhite,skipresize : boolean;
    searchtext, imac1, imac2, imac3,lopamplateurl,lopamnameurl,lopamdirecturl,lopamlocalurl,lopamplatesuffix,lopamnamesuffix,lopamdirectsuffix,lopamlocalsuffix : string;
    externalimagepath,helpprefix,AntiAlias,ruklprefix,ruklsuffix,hiresfile,exitpassword,password,transmsg,scopeinterface,markname,currentname,currentid : string;
    appname : string;
    multi_instance, CloseVMAbrowser : boolean;
    m : array[1..nummessage] of string;
    shapepositionX, shapepositionY, CameraOrientation, PoleOrientation,startl,startb,startxx,startyy : double;
    curfoc, curx, cury : double;
    maxfoc, LabelDensity, overlaylum, phaseoffset : integer;
    minfoc : integer = 100;
    perfdeltay : double =0.00001;
    labelcolor,markcolor,autolabelcolor : Tcolor;
    Ima: array of TBigImaForm;
    ddeparam,tmpmap,tmpdir,currenttexture,imgsuffix,overlayname,currentselection : string;
    CielHnd : Thandle;
    lockchart : Boolean = false;
    StartedByDS : Boolean = false;
    MeasuringDistance : Boolean = false;
    distancestart : Boolean = false;
    SkyChartStarting : boolean = false;
    OldWindowProc : Pointer; {Variable for the old windows proc}
    MyMsg,ExitRequest : DWord; {custom systemwide message}
    param : Tstringlist;
    imgdir : array of array[0..2] of string;
    lf : TLogFont;
    rotatefont : Tfont;
    rotatefontresol : integer;
//    LONGIN,LATIN,WIDEKM,WIDEMI,LENGTHKM,LENGTHMI,FNAME,INTERESTN,DIAMINST : integer;
    LONGIN,LATIN,WIDEKM,WIDEMI,LENGHTKM,LENGHTMI,FNAME,INTERESTN,DIAMINST,wordformat : integer;
    Image2D,EyepieceMask : Tbitmap;
//    trace : Boolean = false;
    locktrackbar : Boolean = false;
    lockscrollbar: Boolean = false;
    hires,hires500,overlayhi,overlayimg: Tbitmap;
    pal : Hpalette;
    fh,fh500 : file;
    eyepiecename: array[1..10]of string;
    eyepiecefield,eyepiecemirror,eyepiecerotation: array[1..10]of integer;
    CurrentEyepiece : integer =0;
    EyepieceRatio : double =1;
    zoom : double =1;
    phasehash : Boolean = false;
    phaseumbrachanging : Boolean = false;
    phaseumbra: Tcolor;
    useDBN: integer = 5;
    db_age : array[1..5] of integer;
    farsidetexture : Boolean = true;
    showoverlay : Boolean = true;
    LastScopeTracking : double = 0;
    nmjd,fqjd,fmjd,lqjd,currentl,currentb : double;
    searchlist: Tstringlist;
    UseComputerTime : Boolean = true;

implementation

uses telescope, config, skylib,planet1, splashunit,Sky_DDE_Util, imglistunit,
     glossary, fmsg, dbutil;

{$R *.DFM}
{$R curretic.res}

{$I vmoon.inc}

//////////////////////////////////////////////////////////////////
begin
  ShowWindow(Application.Handle, SW_HIDE);
end.


