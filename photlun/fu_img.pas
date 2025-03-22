unit fu_img;

{$mode ObjFPC}{$H+}

interface

uses  BGRABitmap, BGRABitmapTypes, math,
  Classes, SysUtils, Forms, Controls, ExtCtrls, Buttons, StdCtrls;

type

  TImgDrawingControl = class(TCustomControl)
  public
    property OnPaint;
    property OnDblClick;
    property onMouseDown;
    property onMouseMove;
    property onMouseUp;
    property OnMouseWheel;
    property OnResize;
    property PopupMenu;
  end;

  { Tf_img }

  Tf_img = class(TFrame)
    ImageList1: TImageList;
    Info: TLabel;
    PanelImg: TPanel;
    PanelBot: TPanel;
    PanelTop: TPanel;
    PlotTimer: TTimer;
    BtnFlipH: TSpeedButton;
    BtnFlipV: TSpeedButton;
    BtnDetach: TSpeedButton;
    BtnClose: TSpeedButton;
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnDetachClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure PlotTimerTimer(Sender: TObject);
    procedure BtnFlipHClick(Sender: TObject);
    procedure BtnFlipVClick(Sender: TObject);
  private
    Fimage: string;
    img_Width,img_Height : integer;
    OrigX,OrigY,MouseDownX,MouseDownY,Mx,My: integer;
    ZoomMin,ImgZoom,ImgScale0,ImgCx,ImgCy: double;
    LockTimerPlot,LockMouseWheel,FlipHorz,FlipVert,MouseMoving: boolean;
    ImaBmp,ScrBmp: TBGRABitmap;
    Image1: TImgDrawingControl;
    FonClose, FonDetach: TNotifyEvent;
    Procedure ClearImage;
    procedure SetImage(value:string);
    procedure PlotImage;
    procedure Image1Paint(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    property image: string read Fimage write SetImage;
    property imgWidth: integer read img_Width;
    property imgHeight: integer read img_Height;
    property onClose: TNotifyEvent read FonClose write FonClose;
    property onDetach: TNotifyEvent read FonDetach write FonDetach;
  end;

implementation

const ZoomMax=5;

{$R *.lfm}

constructor Tf_img.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
// SetLang;
 FlipHorz:=false;
 FlipVert:=false;
 LockTimerPlot:=false;
 MouseMoving:=false;
 ImgZoom:=0;
 ImgCx:=0;
 ImgCy:=0;
 ImaBmp:=TBGRABitmap.Create;
 ScrBmp:=TBGRABitmap.Create;
 ScrBmp.SetSize(PanelImg.Width,PanelImg.Height);
 Image1:=TImgDrawingControl.Create(self);
 Image1.Parent:=PanelImg;
 Image1.Align:=alClient;
 Image1.OnPaint:=@Image1Paint;
 Image1.OnMouseDown:=@Image1MouseDown;
 Image1.OnMouseMove:=@Image1MouseMove;
 Image1.OnMouseUp:=@Image1MouseUp;
 Image1.OnMouseWheel:=@Image1MouseWheel;
end;

destructor Tf_img.Destroy;
begin
 ImaBmp.Free;
 ScrBmp.Free;
 inherited Destroy;
end;

procedure Tf_img.FrameResize(Sender: TObject);
begin
  ScrBmp.SetSize(PanelImg.Width,PanelImg.Height);
  PlotImage;
end;

procedure Tf_img.BtnCloseClick(Sender: TObject);
begin
  if Assigned(FonClose) then FonClose(self);
  Free;
end;

procedure Tf_img.BtnDetachClick(Sender: TObject);
begin
  if Assigned(FonDetach) then FonDetach(self);
end;

procedure Tf_img.PlotTimerTimer(Sender: TObject);
begin
  if LockTimerPlot then exit;
  PlotTimer.Enabled:=false;
  LockTimerPlot:=true;
  PlotImage;
  LockTimerPlot:=false;
end;

procedure Tf_img.BtnFlipHClick(Sender: TObject);
begin
  FlipHorz:=BtnFlipH.Down;
  PlotImage;
end;

procedure Tf_img.BtnFlipVClick(Sender: TObject);
begin
  FlipVert:=BtnFlipV.Down;
  PlotImage;
end;

Procedure Tf_img.ClearImage;
begin
ScrBmp.FillRect(0,0,ScrBmp.Width,ScrBmp.Height,clBlackOpaque);
end;

procedure Tf_img.SetImage(value:string);
begin
 Fimage:=value;
 ImaBmp.LoadFromFile(Fimage);
 img_Width:=ImaBmp.Width;
 img_Height:=ImaBmp.Height;
 PlotImage;
end;

Procedure Tf_img.PlotImage;
var r1,r2: double;
    w,h,px,py: integer;
    tmpbmp,str: TBGRABitmap;
    rmode: TResampleMode;
begin
  if (img_Height=0)or(img_Width=0) then exit;
  r1:=ScrBmp.Width/imabmp.Width;
  r2:=ScrBmp.Height/imabmp.Height;
  ZoomMin:=minvalue([1.0,r1,r2]);
  if (ZoomMin<1)and((ImgZoom<ZoomMin)or(abs(ImgZoom-ZoomMin)<0.01)) then ImgZoom:=0;
  ClearImage;
  imabmp.ResampleFilter:=rfBestQuality;
  rmode:=rmFineResample;

  if ImgZoom=0 then begin
    // adjust
    r1:=img_Width/img_Height;
    w:=ScrBmp.width;
    h:=ScrBmp.height;
    r2:=w/h;
    if r1>r2 then begin
      h:=max(1,trunc(w/r1));
      ImgScale0:=h/img_Height;
      px:=0;
      py:=(ScrBmp.Height-h) div 2;
    end else begin
      w:=max(1,trunc(h*r1));
      ImgScale0:=w/img_Width;
      px:=(ScrBmp.width-w) div 2;
      py:=0;
    end;
    OrigX:=round(px/ImgScale0);
    OrigY:=round(py/ImgScale0);
    str:=ImaBmp.Resample(w,h,rmode) as TBGRABitmap;
    ScrBmp.PutImage(px,py,str,dmSet);
    str.Free;
  end
  else if ImgZoom=1 then begin
     // zoom 1
     px:=round(ImgCx)-((img_Width-ScrBmp.Width) div 2);
     py:=round(ImgCy)-((img_Height-ScrBmp.Height) div 2);
     OrigX:=px;
     OrigY:=py;
     ScrBmp.PutImage(px,py,imabmp,dmSet);
  end
  else begin
     // other zoom
     if ImgZoom<ZoomMin then ImgZoom:=ZoomMin;
     tmpbmp:=TBGRABitmap.Create(round(ScrBmp.Width/ImgZoom),round(ScrBmp.Height/ImgZoom),clBlackOpaque);
     px:=round(ImgCx)-((img_Width-tmpbmp.Width) div 2);
     py:=round(ImgCy)-((img_Height-tmpbmp.Height) div 2);
     OrigX:=px;
     OrigY:=py;
     tmpbmp.PutImage(px,py,ImaBmp,dmSet);
     str:=tmpbmp.Resample(ScrBmp.Width,ScrBmp.Height,rmSimpleStretch) as TBGRABitmap;
     ScrBmp.PutImage(0,0,str,dmSet);
     str.Free;
     tmpbmp.Free;
  end;

  if FlipHorz then ScrBmp.HorizontalFlip;
  if FlipVert then ScrBmp.VerticalFlip;

  Image1.Invalidate;
end;

procedure Tf_img.Image1Paint(Sender: TObject);
begin
  ScrBmp.Draw(Image1.Canvas,0,0,true);
end;

procedure Tf_img.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDownX:=X;
  MouseDownY:=Y;
  if Shift=[ssLeft] then begin
    if (ImgZoom>0) then begin
       Mx:=X;
       My:=y;
       MouseMoving:=true;
       screen.Cursor:=crHandPoint;
    end;
  end
end;

procedure Tf_img.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
  X:=Max(1,Min(Image1.Width,X));
  Y:=Max(1,Min(Image1.Height,Y));
  if MouseMoving then begin
    if FlipHorz then
      ImgCx:=ImgCx - (X-Mx) / ImgZoom
    else
      ImgCx:=ImgCx + (X-Mx) / ImgZoom;
    if FlipVert then
      ImgCy:=ImgCy - (Y-My) / ImgZoom
    else
      ImgCy:=ImgCy + (Y-My) / ImgZoom;
    PlotTimer.Enabled:=true;
  end;
  Mx:=X;
  My:=Y;
end;

procedure Tf_img.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if MouseMoving then begin
    ImgCx:=ImgCx + (X-Mx) / ImgZoom;
    ImgCy:=ImgCy + (Y-My) / ImgZoom;
    PlotImage;
    Mx:=X;
    My:=Y;
end;
MouseMoving:=false;
screen.Cursor:=crDefault;
end;

procedure Tf_img.Image1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  zf,r1,r2: double;
begin
  if LockMouseWheel then
    exit;
  LockMouseWheel := True;
  try
    handled := True;
    if wheeldelta > 0 then
      zf := 1.25
    else
      zf := 0.8;
    if ImgZoom=0 then begin
      r1:=ScrBmp.Width/imabmp.Width;
      r2:=ScrBmp.Height/imabmp.Height;
      ImgZoom:=minvalue([r1,r2]);
    end;
    ImgZoom:=ImgZoom*zf;
    if ImgZoom>ZoomMax then ImgZoom:=ZoomMax;
    if ImgZoom<ZoomMin then ImgZoom:=ZoomMin;
    PlotImage;
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
  finally
    LockMouseWheel := False;
  end;
end;

end.

