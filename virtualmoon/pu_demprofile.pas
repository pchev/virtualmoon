unit pu_demprofile;

{$mode ObjFPC}{$H+}

interface

uses  u_constant, cu_dem, u_translation, u_util,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, TAGraph, TASeries;

type

  { Tf_demprofile }

  Tf_demprofile = class(TForm)
    DemProfile: TChart;
    DemProfileLineSeries1: TLineSeries;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    procedure Panel2Click(Sender: TObject);
  private
    Fdem: Tdem;
  public
    procedure PlotProfile(lon1,lat1,lon2,lat2: double);
    property dem: Tdem read Fdem write Fdem;
  end;

var
  f_demprofile: Tf_demprofile;

implementation

{$R *.lfm}

procedure Tf_demprofile.Panel2Click(Sender: TObject);
begin

end;

procedure Tf_demprofile.PlotProfile(lon1,lat1,lon2,lat2: double);
var x,r,lon,lat,s: double;
    n:integer;
    gc: TGreatCircle;
begin
  if (lon1=lon2)and(lat1=lat2) then exit;
  DemProfileLineSeries1.Clear;
  dem.GreatCircle(lon1,lat1,lon2,lat2,Rmoon,gc);
  s:=gc.s01;
  n:=0;
  repeat
    inc(n);
    dem.PointOnCircle(gc,s,lat,lon);
    lat:=lat*rad2deg;
    lon:=lon*rad2deg;
    if lon<0 then lon:=lon+360;
    x:=dem.GetDemElevation(lon,lat);
    r:=(s-gc.s01)*gc.radius;
    DemProfileLineSeries1.AddXY(r,x);
    s:=s+(1/dem.DemHdr.MAP_RESOLUTION)*deg2rad;
  until s>gc.s02;
  Label1.Caption:=rsm_10+'/'+rsm_11+' '+formatfloat(f2, rad2deg*lon1)+'/'+formatfloat(f2, rad2deg*lat1) +
                  ' - '+formatfloat(f2, rad2deg*lon2)+'/'+formatfloat(
                    f2, rad2deg*lat2)+
                  ' '+LowerCase(rst_69)+' '+formatfloat(f2, gc.dist)+lowercase(rsm_18);
end;

end.

