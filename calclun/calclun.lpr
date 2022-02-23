program calclun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, printer4lazarus, anchordockpkg, calclun_main, cspice, sysutils, moon_spice, u_util, u_projection, u_constant, pas_spice, config, u_translation, splashunit, downldialog, libsql
  { you can add units after this };

{$R *.res}

begin
  (* // To stdout by default, uncomment to write to file
  {$ifdef USEHEAPTRC}
    {$ifdef mswindows}
      DeleteFile('C:\Temp\calclun_heap.trc');
      SetHeapTraceOutput('C:\Temp\calclun_heap.trc');
    {$else}
      DeleteFile('/tmp/calclun_heap.trc');
      SetHeapTraceOutput('/tmp/calclun_heap.trc');
    {$endif}
  {$endif}
  *)

  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(Tf_calclun, f_calclun);

  Application.CreateForm(Tsplash, splash);
  Splashversion := AVLversion+blank+compile_time;
  splash.VersionName:=VersionName;
  splash.Splashversion:=Splashversion;
  splash.show;

  Application.CreateForm(Tf_config, f_config);
  Application.Run;
end.

