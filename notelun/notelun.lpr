program notelun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, printer4lazarus, notelun_main, dbutil, u_constant, u_util, cu_tz, cu_planet, u_projection, libsql, pu_search, pu_date, notelun_setup, cu_print, pu_listselection, pu_export;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(Tf_notelun, f_notelun);
  Application.CreateForm(Tf_search, f_search);
  Application.CreateForm(Tf_date, f_date);
  Application.CreateForm(TFSetup, FSetup);
  Application.CreateForm(Tf_listselection, f_listselection);
  Application.CreateForm(Tf_export, f_export);
  Application.Run;
end.

