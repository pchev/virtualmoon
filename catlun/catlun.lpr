program catlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, catlun_main, dbutil, libsql, Printer4Lazarus,
  TurboPowerIPro, glscenelazarus
  { you can add units after this };

begin
  Application.Initialize;
  Application.CreateForm(Tf_catlun, f_catlun);
  Application.Run;
end.

