unit pu_demprofile;

{$mode ObjFPC}{$H+}

interface

uses  u_constant, cu_dem,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, TAGraph, TASeries;

type

  { Tf_demprofile }

  Tf_demprofile = class(TForm)
    DemProfile: TChart;
    DemProfileLineSeries1: TLineSeries;
    Panel1: TPanel;
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

procedure Tf_demprofile.PlotProfile(lon1,lat1,lon2,lat2: double);
var x,r,lon,lat,s: double;
    n:integer;
    gc: TGreatCircle;
begin
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
end;

end.

