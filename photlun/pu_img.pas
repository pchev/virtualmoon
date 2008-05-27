unit pu_img;

{$mode objfpc}{$H+}

interface

uses u_bitmap,
  lazjpeg,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  cu_zoomimage, ComCtrls, ExtCtrls;

type

  { Tf_img }

  Tf_img = class(TForm)
    StatusBar1: TStatusBar;
    ResizeTimer: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ZoomImage1: TZoomImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ResizeTimerTimer(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
    procedure ZoomImage1Resize(Sender: TObject);
  private
    { private declarations }
    Fimage: string;
    bmp,tmpbmp: TBitmap;
    lum,con: integer;
    rot: double;
    procedure SetImage(value:string);
    procedure Redraw;
  public
    { public declarations }
    property image: string read Fimage write SetImage;
  end; 

var
  f_img: Tf_img;

implementation

procedure Tf_img.FormCreate(Sender: TObject);
begin
  bmp:=TBitmap.Create;
  tmpbmp:=TBitmap.Create;
  lum:=0;
  con:=0;
end;

procedure Tf_img.FormDestroy(Sender: TObject);
begin
  bmp.Free;
  tmpbmp.Free;
end;

procedure Tf_img.ResizeTimerTimer(Sender: TObject);
begin
 ResizeTimer.Enabled:=false;
 ZoomImage1.Picture.Assign(tmpbmp);
end;

procedure Tf_img.Redraw;
begin
 BitmapRotation(bmp,tmpbmp,rot,false);
 BitmapLumCon(tmpbmp,lum,con);
 ZoomImage1.Picture.Assign(tmpbmp);
end;

procedure Tf_img.ToolButton1Click(Sender: TObject);
begin
 ZoomImage1.Zoom:=ZoomImage1.Zoom*1.5;
 ZoomImage1.Draw;
end;

procedure Tf_img.ToolButton2Click(Sender: TObject);
begin
 ZoomImage1.Zoom:=ZoomImage1.Zoom/1.5;
 ZoomImage1.Draw;
end;

procedure Tf_img.ToolButton3Click(Sender: TObject);
begin
 rot:=rot-pi/4;
 Redraw;
end;

procedure Tf_img.ToolButton4Click(Sender: TObject);
begin
 rot:=rot+pi/4;
 Redraw;
end;

procedure Tf_img.ToolButton5Click(Sender: TObject);
begin
 lum:=lum+10;
 Redraw;
end;

procedure Tf_img.ToolButton6Click(Sender: TObject);
begin
 lum:=lum-10;
 Redraw;
end;

procedure Tf_img.ToolButton7Click(Sender: TObject);
begin
 con:=con+10;
 Redraw;
end;

procedure Tf_img.ToolButton8Click(Sender: TObject);
begin
 con:=con-10;
 Redraw;
end;

procedure Tf_img.ToolButton9Click(Sender: TObject);
begin

end;


procedure Tf_img.ToolButton10Click(Sender: TObject);
begin

end;

procedure Tf_img.ZoomImage1Resize(Sender: TObject);
begin
 ResizeTimer.Enabled:=false;
 ResizeTimer.Enabled:=true;
end;

procedure Tf_img.SetImage(value:string);
var jpeg:TJPEGImage;
begin
if value<>Fimage then begin
  try
  lum:=0;
  con:=0;
  rot:=0;
  jpeg:=TJPEGImage.create;
  jpeg.LoadFromFile(value);
  bmp.Assign(jpeg);
  tmpbmp.Assign(bmp);
  ZoomImage1.Picture.Assign(tmpbmp);
  Fimage:=value;
  caption:='PhotLun : '+ChangeFileExt(ExtractFileName(value),'');
  finally
  jpeg.Free;
  end;
end;
end;


initialization
  {$I pu_img.lrs}

end.

