unit virtualmoon2;
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
  Windows, Forms, Controls, StdCtrls, ExtCtrls, Graphics,
   DdeMan,
{$ENDIF}
{$IFDEF opengl}
  GLScene, GLObjects, GLMisc, GLWin32Viewer, GLTexture, Info,
  GLcontext, GLCadencer, GLBitmapFont, GLHUDObjects, GLGraphics,
  GLGraph, geometry, GLMirror, AsyncTimer,
{$ENDIF}
  mlb2, Printers,
  Messages, SysUtils, Classes, Dialogs,  math,
  ComCtrls, Mask, Menus, jpeg, Buttons, ToolWin,
  EnhEdits, IniFiles, Grids, BigIma, HTMLLite;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    Fichier1: TMenuItem;
    Quitter1: TMenuItem;
    Configuration1: TMenuItem;
    PageControl1: TPageControl;
    Position: TTabSheet;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Ephemerides: TTabSheet;
    Panel4: TPanel;
    Label6: TLabel;
    Label9: TLabel;
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
    Button10: TButton;
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
    Panel2: TPanel;
    Panel2D: TPanel;
    HorzScrollBar: TScrollBar;
    VertScrollBar: TScrollBar;
    Panel3: TPanel;
    Shape2: TShape;
    Shape1: TShape;
    Label5: TLabel;
    Image1: TImage;
    DebugLabel: TLabel;
    Panel8: TPanel;
    Label13: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    TrackBar5: TTrackBar;
    CheckBox19: TCheckBox;
    ReglageTimer: TTimer;
    Button14: TButton;
    GroupBox2: TGroupBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
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
    procedure Button10Click(Sender: TObject);
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
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrollBar2DChange(Sender: TObject);
    procedure SnapShot(var bmp : TBitmap; white:boolean);
    procedure CheckBox19Click(Sender: TObject);
    procedure ReglageTimerTimer(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CheckBox5Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure InitGraphic(Sender: TObject);
  end;

var
  Form1: TForm1;
  db,dblox,dbnotes : TMlb2;

function InvProjMoon (xx,yy,lc,bc : Double ; VAR l,b : Double ):boolean;
function ProjMoon(l,b,lc,bc : Double ; VAR X,Y : Double ):boolean;
procedure World2Window(xx,yy : double; var x,y : integer);
procedure Window2World(x,y : integer; var xx,yy : double);
function SearchAtPos(l,b: double):boolean;
Procedure GetDetail(memo:Tmemo);
Procedure GetHTMLDetail(var txt:string);
Procedure RefreshMoonImage;
Procedure ShowImg(desc,nom:string; forceinternal:boolean);
function SearchName(n: string; center: boolean):boolean;
{$IFDEF opengl}
Procedure OpenHires(forcerebuild:boolean);
{$ENDIF}

const AVLversion = '2.0';
      Splashversion ='Version 2.0  2003-10-25';
      d1 = '0.0';
      d2 = '0.00';
      Rmoon = 1737.103;  // moon radius Km
      crlf = chr(10)+chr(13);
      tab  = chr(9);
      LightDist=20;
      cameradist=2000;
      crRetic = 5;
      crPointing = 6;
      ox=36; oy=36; os=1500; px=0.95467; py=0.95467; //image 1500x1500, lune 1432x1432
      nummessage = 72;
      MaxLabel=200;
      Label3dSize=0.6;
      Label2dSize=-11;
{$ifdef vmalight}
      maxfocbase=1200;
      IdMsg='Virtual_Moon_Atlas_Light_message';
      versionname='Light';
{$endif}
{$ifdef vmabasic}
      maxfocbase=1200;
      IdMsg='Virtual_Moon_Atlas_Basic_message';
      versionname='Basic';
{$endif}
{$ifdef vmaexpert}
      maxfocbase=1900;
      IdMsg='Virtual_Moon_Atlas_Expert_message';
      versionname='Expert';
{$endif}

var lastx,lasty,lastyzoom,posmin,posmax,ax,ay : integer;
    ReduceTexture,ReduceTextureFar,LastIma,maximgdir,maxima,startx,starty,saveimagesize,lastscrollx,lastscrolly : Integer;
    LeftMargin, PrintTextWidth,clickX,clickY : integer;
    PrintEph, PrintDesc, MipMaps, GeologicalMap, externalimage, PrintChart,lopamdirect,doublebuf,stencilbuf,hiresok :Boolean;
    PicZoom: array of double;
    PicTop,PicLeft: array of integer;
    librl,librb,lrot,librlong,librlat,wheelstep,EphStep,fov,searchl,searchb,markx,marky,flipx,rotstep,dummy : double;
    ra,dec,dist,dkm,diam,phase,illum,pa,sunincl,timezone,currentphase,tphase,LabelSize,bx,by,bxpos : double;
    lunaison,editrow,notesrow,hi_w,hi_wd,hi_dl,rotdirection : integer;
    dbedited : Boolean = false;
    SkipIdent,Firstsearch,phaseeffect,librationeffect,geocentric,FollowNorth,notesok,notesedited,labelcenter,minilabel,SafeMode : boolean;
    useOpenGL,lockmove,lockrepeat,DDEreceiveok,showlabel,showautolabel,showmark,showlibrationmark,marked,saveimagewhite,skipresize : boolean;
    searchtext, imac1, imac2, imac3,lopamplateurl,lopamnameurl,lopamdirecturl,lopamlocalurl,lopamplatesuffix,lopamnamesuffix,lopamdirectsuffix,lopamlocalsuffix,olddatabase : string;
    externalimagepath,helpprefix,AntiAlias,ruklprefix,ruklsuffix,hiresfile,exitpassword,password,transmsg : string;
    m : array[1..nummessage] of string;
    shapepositionX, shapepositionY, CameraOrientation, PoleOrientation,startl,startb,startxx,startyy : double;
    maxfoc, LabelDensity : integer;
    minfoc : integer = 100;
    perfdeltay : double =0.00001;
    labelcolor,markcolor,autolabelcolor : Tcolor;
    Ima: array of TBigImaForm;
    ddeparam,tmpmap,tmpdir,currenttexture,imgsuffix : string;
    CielHnd : Thandle;
    lockchart : Boolean = false;
    StartedByDS : Boolean = false;
    MeasuringDistance : Boolean = false;
    distancestart : Boolean = false;
    SkyChartStarting : boolean = false;
    OldWindowProc : Pointer; {Variable for the old windows proc}
    MyMsg : DWord; {custom systemwide message}
    param : Tstringlist;
    imgdir : array of array[0..2] of string;
    lf : TLogFont;
    rotatefont : Tfont;
    LONGIN,LATIN,WIDEKM,WIDEMI,LENGHTKM,LENGHTMI,FNAME,INTERESTN,DIAMINST,wordformat : integer;
//    LONGIN,LATIN,WIDEKM,WIDEMI,LENGTHKM,LENGTHMI,FNAME,INTERESTN,DIAMINST : integer;
    Image2D,Eyepiecemask : Tbitmap;
//    trace : Boolean = false;
    locktrackbar : Boolean = false;
    lockscrollbar: Boolean = false;
    hires: Tbitmap;
    pal : Hpalette;
    fh : file;                                   
    eyepiecename: array[1..10]of string;
    eyepiecefield,eyepiecemirror,eyepiecerotation: array[1..10]of integer;
    CurrentEyepiece : integer =0;
    EyepieceRatio : double =1;
    zoom : double =1;
    phasehash : Boolean = false;
    phaseumbrachanging : Boolean = false;
    phaseumbra: Tcolor;
    database : array[1..4] of string;
    usedatabase :array[1..4] of boolean;
    db_age : array[1..4] of integer;
    farsidetexture : Boolean = true;

implementation

uses config, skylib,planet1, splashunit,Sky_DDE_Util, imglistunit, glossary,
  fmsg;

{$R *.DFM}
{$R curretic.res}

{$I vmoon.inc}

begin
  ShowWindow(Application.Handle, SW_HIDE);
end.
