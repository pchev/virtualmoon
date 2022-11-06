unit pu_demprofile;

{$mode ObjFPC}{$H+}

interface

uses  u_constant, cu_dem, u_translation, u_util, math,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Menus, TAGraph, TASeries, TAChartUtils, TAChartAxisUtils;

type

  { Tf_demprofile }

  Tf_demprofile = class(TForm)
    Button10x: TSpeedButton;
    Button1x: TSpeedButton;
    Button2x: TSpeedButton;
    Button5x: TSpeedButton;
    ButtonReset: TSpeedButton;
    ButtonSave: TSpeedButton;
    DemProfile: TChart;
    DemProfileLineSeries1: TLineSeries;
    DemProfileLineSeries2: TLineSeries;
    Label2: TLabel;
    LabelPos: TLabel;
    MenuItemSavePNG: TMenuItem;
    MenuItemSaveCSV: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Panel3: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonxClick(Sender: TObject);
    procedure DemProfileAxisList1GetMarkText(Sender: TObject; var AText: String; AMark: Double);
    procedure DemProfileMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DemProfileResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemSaveCSVClick(Sender: TObject);
    procedure MenuItemSavePNGClick(Sender: TObject);
  private
    Fdemlib: TdemLibrary;
    Fdist,Fhmin,Fhmax,FScale: double;
    procedure AdjustScale;
  public
    procedure SetLang;
    procedure PlotProfile(lon1,lat1,lon2,lat2: array of double);
    property demlib: TdemLibrary read Fdemlib write Fdemlib;
  end;

var
  f_demprofile: Tf_demprofile;

implementation

{$R *.lfm}

procedure Tf_demprofile.FormCreate(Sender: TObject);
begin
 ScaleFormForFontSize(self,96);
 FScale:=0;
 ButtonReset.Down:=true;
 LabelPos.Caption:='';
 label1.Caption:='';
end;

procedure Tf_demprofile.SetLang;
begin
  ButtonReset.Caption:=rsMax2;
  label2.Caption:=rsAmplificatio;
  DemProfile.AxisList[0].Title.Caption:=rsElevation+' [m]';
  DemProfile.AxisList[1].Title.Caption:=rst_69+' [km]';
  ButtonSave.Caption:=rsSaveProfile+'…';
  MenuItemSavePNG.Caption:=rsAsPNGPicture;
  MenuItemSaveCSV.Caption:=rsAsCSVFile;
end;

procedure Tf_demprofile.ButtonxClick(Sender: TObject);
begin
  Fscale:=TButton(sender).tag;
  AdjustScale;
end;

procedure Tf_demprofile.DemProfileAxisList1GetMarkText(Sender: TObject; var AText: String; AMark: Double);
begin
  if AMark=0 then
    AText:=rsStart
  else
    AText:=FloatToStr(AMark);
end;

procedure Tf_demprofile.DemProfileMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var i: integer;
    dx,r,px,py:double;
begin
  if (x>5)and(x<(DemProfile.Width-5))and(y>5)and(y<(DemProfile.Height-5)) then begin
  try
    r:=99999;
    for i:=0 to DemProfileLineSeries1.Count-1 do begin
      dx:=abs(X-DemProfile.XGraphToImage(DemProfileLineSeries1.Source.Item[i]^.X));
      if dx<r then begin
        px:=DemProfileLineSeries1.Source.Item[i]^.X;
        py:=DemProfileLineSeries1.Source.Item[i]^.Y;
        r:=dx;
      end;
    end;
    LabelPos.Caption:=rst_69+':'+FormatFloat(f2,px)+'km '+rsElevation+':'+FormatFloat(f1,py)+'m';
   except
  end;
  end else begin
    LabelPos.Caption:='';
  end;
end;

procedure Tf_demprofile.DemProfileResize(Sender: TObject);
begin
  AdjustScale;
end;

procedure Tf_demprofile.AdjustScale;
var x,y,yy,dy,xx,rc,rs: double;
begin
if FScale=0 then begin
  DemProfile.Extent.XMin:=0;
  DemProfile.Extent.XMax:=Fdist/1000;
  DemProfile.Extent.YMin:=fhmin;
  DemProfile.Extent.YMax:=fhmax;
  DemProfile.Extent.UseXMin:=true;
  DemProfile.Extent.UseXMax:=true;
  DemProfile.Extent.UseYMin:=true;
  DemProfile.Extent.UseYMax:=true;
end
else begin
  x:=Fdist;
  y:=Fhmax-Fhmin;
  rc:=DemProfile.ChartHeight/DemProfile.ChartWidth;
  rs:=FScale*y/x;
  if rc>=rs then begin
    yy:=x*rc;
    dy:=(yy-Fscale*y)/2/FScale;
    DemProfile.Extent.XMin:=0;
    DemProfile.Extent.XMax:=Fdist/1000;
    DemProfile.Extent.YMin:=fhmin-dy;
    DemProfile.Extent.YMax:=fhmax+dy;
    DemProfile.Extent.UseXMin:=true;
    DemProfile.Extent.UseXMax:=true;
    DemProfile.Extent.UseYMin:=true;
    DemProfile.Extent.UseYMax:=true;
  end
  else begin
    xx:=FScale*y/rc/1000;
    DemProfile.Extent.XMin:=0;
    DemProfile.Extent.XMax:=xx;
    DemProfile.Extent.YMin:=fhmin;
    DemProfile.Extent.YMax:=fhmax;
    DemProfile.Extent.UseXMin:=true;
    DemProfile.Extent.UseXMax:=true;
    DemProfile.Extent.UseYMin:=true;
    DemProfile.Extent.UseYMax:=true;
  end;
end;
end;

procedure Tf_demprofile.PlotProfile(lon1,lat1,lon2,lat2: array of double);
var x,r,r0,lon,lat,s,ddeg,totdist: double;
    i,n:integer;
    gc: TGreatCircle;
const
    numpoint=1000;
begin
  if (demlib=nil)or(not demlib.Open) then exit;
  if (NumDist=0)or(lon1[0]=lon2[0])and(lat1[0]=lat2[0]) then exit;
  DemProfileLineSeries1.Clear;
  DemProfileLineSeries2.Clear;
  Fdist:=0;
  r0:=0;
  Fhmin:=999999;
  Fhmax:=-999999;
  totdist:=0;
  // find dem resolution
  for i:=0 to NumDist-1 do begin
    GreatCircle(lon1[i],lat1[i],lon2[i],lat2[i],Rmoon,gc);
    totdist:=totdist+gc.dist;
  end;
  ddeg:=360*totdist/(pi2*Rmoon);
  demlib.SetResolution(3,numpoint/ddeg);
  // plot
  for i:=0 to NumDist-1 do begin
    GreatCircle(lon1[i],lat1[i],lon2[i],lat2[i],Rmoon,gc);
    Fdist:=Fdist+gc.dist*1000;
    s:=gc.s01;
    n:=0;
    repeat
      inc(n);
      PointOnCircle(gc,s,lat,lon);
      lat:=lat*rad2deg;
      lon:=lon*rad2deg;
      if lon<0 then lon:=lon+360;
      x:=demlib.GetElevation(3,lon,lat);
      if x<>NoHeight then begin
        Fhmin:=min(x,Fhmin);
        Fhmax:=max(x,Fhmax);
        r:=r0+(s-gc.s01)*gc.radius;
        DemProfileLineSeries1.AddXY(r,x);
      end;
      s:=s+(1/demlib.GetResolution(3))*deg2rad;
    until s>gc.s02;
    if i<(NumDist-1) then begin
      DemProfileLineSeries2.AddXY(r,-999999,'',clNone);
      DemProfileLineSeries2.AddXY(r,999999,'',clRed);
    end;
    r0:=r0+gc.dist;
  end;
  AdjustScale;
  Label1.Caption:=StringReplace(rsm_10,':','',[])+'/'+StringReplace(rsm_11,':','',[])+blank+LowerCase(rsFrom)+blank+formatfloat(f3, rad2deg*lon1[0])+blank+'/'+blank+formatfloat(f3, rad2deg*lat1[0])+
                  blank+LowerCase(rsTo)+blank+formatfloat(f3, rad2deg*lon2[NumDist-1])+'/'+formatfloat(f3, rad2deg*lat2[NumDist-1])+
                  crlf+rsUsing+' ldem_'+inttostr(demlib.GetResolution(3))+','+blank+LowerCase(rst_69)+blank+formatfloat(f3, r0)+lowercase(rsm_18)+
                  ','+blank+rsElevation+blank+rsMin+':'+formatfloat(f1, Fhmin)+'m'+blank+rsMax+':'+formatfloat(f1, Fhmax)+'m';


end;

procedure Tf_demprofile.ButtonSaveClick(Sender: TObject);
begin
 PopupMenu1.PopUp;
end;

procedure Tf_demprofile.MenuItemSavePNGClick(Sender: TObject);
var fn: string;
begin
  SaveDialog1.DefaultExt:='.png';
  SaveDialog1.Filter:='PNG file|*.png';
  if SaveDialog1.Execute then begin
    fn:=SaveDialog1.FileName;
    ChangeFileExt(fn,'.png');
    DemProfile.SaveToFile(TPortableNetworkGraphic,fn);
  end;
end;

procedure Tf_demprofile.MenuItemSaveCSVClick(Sender: TObject);
var i:integer;
    fn,buf: string;
    f: TextFile;
begin
  SaveDialog1.DefaultExt:='.csv';
  SaveDialog1.Filter:='CSV file|*.csv';
  if SaveDialog1.Execute then begin
    fn:=SaveDialog1.FileName;
    ChangeFileExt(fn,'.csv');
    AssignFile(f,fn);
    Rewrite(f);
    buf:='Index;Distance;Height';
    writeln(f,buf);
    for i:=0 to DemProfileLineSeries1.Count-1 do begin
      buf:=inttostr(i)+';'+FormatFloat(f3,DemProfileLineSeries1.XValue[i])+';'+FormatFloat(f1,DemProfileLineSeries1.YValue[i]);
      writeln(f,buf);
    end;
    CloseFile(f);
  end;
end;

end.

