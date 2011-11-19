program catlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, glscene_designtime, catlun_main, dbutil, fmsg, pu_moon, libsql,
  Printer4Lazarus, TurboPowerIPro;

begin
  Application.Initialize;
  Application.CreateForm(Tf_catlun, f_catlun);
  Application.Run;
end.

