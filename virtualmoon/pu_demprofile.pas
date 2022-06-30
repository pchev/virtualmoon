unit pu_demprofile;

{$mode ObjFPC}{$H+}

interface

uses  u_constant, cu_dem, u_translation, u_util, math,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, TAGraph, TASeries;

type

  { Tf_demprofile }

  Tf_demprofile = class(TForm)
    Button10x: TSpeedButton;
    Button1x: TSpeedButton;
    Button2x: TSpeedButton;
    Button5x: TSpeedButton;
    ButtonReset: TSpeedButton;
    DemProfile: TChart;
    DemProfileLineSeries1: TLineSeries;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    procedure ButtonxClick(Sender: TObject);
    procedure DemProfileResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Fdem: Tdem;
    Fdist,Fhmin,Fhmax,FScale: double;
    procedure AdjustScale;
  public
    procedure PlotProfile(lon1,lat1,lon2,lat2: double);
    property dem: Tdem read Fdem write Fdem;
  end;

var
  f_demprofile: Tf_demprofile;

implementation

{$R *.lfm}

procedure Tf_demprofile.FormCreate(Sender: TObject);
begin
 FScale:=0;
 ButtonReset.Down:=true;
 label2.Caption:=rsAmplificatio;
 DemProfile.AxisList[0].Title.Caption:=rsHeight+' [m]';
 DemProfile.AxisList[1].Title.Caption:=rst_69+' [km]';
end;

procedure Tf_demprofile.ButtonxClick(Sender: TObject);
begin
  Fscale:=TButton(sender).tag;
  AdjustScale;
end;

procedure Tf_demprofile.DemProfileResize(Sender: TObject);
begin
  AdjustScale;
end;

procedure Tf_demprofile.AdjustScale;
var x,y,yy,dy,xx,rc,rs: double;
begin
if FScale=0 then begin
  DemProfile.AxisList[0].Range.UseMin:=false;
  DemProfile.AxisList[0].Range.UseMax:=false;
  DemProfile.AxisList[1].Range.UseMin:=false;
  DemProfile.AxisList[1].Range.UseMax:=false;
end
else begin
  x:=Fdist;
  y:=Fhmax-Fhmin;
  rc:=DemProfile.ChartHeight/DemProfile.ChartWidth;
  rs:=FScale*y/x;
  if rc>=rs then begin
    yy:=x*rc;
    dy:=(yy-Fscale*y)/2/FScale;
    DemProfile.AxisList[0].Range.Min:=fhmin-dy;
    DemProfile.AxisList[0].Range.Max:=fhmax+dy;
    DemProfile.AxisList[0].Range.UseMin:=true;
    DemProfile.AxisList[0].Range.UseMax:=true;
    DemProfile.AxisList[1].Range.UseMin:=false;
    DemProfile.AxisList[1].Range.UseMax:=false;
  end
  else begin
    xx:=FScale*y/rc/1000;
    DemProfile.AxisList[1].Range.Min:=0;
    DemProfile.AxisList[1].Range.Max:=xx;
    DemProfile.AxisList[1].Range.UseMin:=true;
    DemProfile.AxisList[1].Range.UseMax:=true;
    DemProfile.AxisList[0].Range.UseMin:=false;
    DemProfile.AxisList[0].Range.UseMax:=false;
  end;
end;
end;

procedure Tf_demprofile.PlotProfile(lon1,lat1,lon2,lat2: double);
var x,r,lon,lat,s: double;
    n:integer;
    gc: TGreatCircle;
begin
  if (lon1=lon2)and(lat1=lat2) then exit;
  DemProfileLineSeries1.Clear;
  dem.GreatCircle(lon1,lat1,lon2,lat2,Rmoon,gc);
  Fdist:=gc.dist*1000;
  Fhmin:=999999;
  Fhmax:=-999999;
  s:=gc.s01;
  n:=0;
  repeat
    inc(n);
    dem.PointOnCircle(gc,s,lat,lon);
    lat:=lat*rad2deg;
    lon:=lon*rad2deg;
    if lon<0 then lon:=lon+360;
    x:=dem.GetDemElevation(lon,lat);
    Fhmin:=min(x,Fhmin);
    Fhmax:=max(x,Fhmax);
    r:=(s-gc.s01)*gc.radius;
    DemProfileLineSeries1.AddXY(r,x);
    s:=s+(1/dem.DemHdr.MAP_RESOLUTION)*deg2rad;
  until s>gc.s02;
  AdjustScale;
  Label1.Caption:=rsm_10+'/'+rsm_11+' '+formatfloat(f3, rad2deg*lon1)+'/'+formatfloat(f3, rad2deg*lat1) +
                  ' ; '+formatfloat(f3, rad2deg*lon2)+'/'+formatfloat(f3, rad2deg*lat2)+
                  ', '+LowerCase(rst_69)+' '+formatfloat(f3, gc.dist)+lowercase(rsm_18);


end;

end.

