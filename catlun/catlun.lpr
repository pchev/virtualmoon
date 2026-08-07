program catlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, GLScene_RunTime, catlun_main, pu_moon,
  Printer4Lazarus, TurboPowerIPro;

begin
  Application.Initialize;
  Application.CreateForm(Tf_catlun, f_catlun);
  Application.Run;
end.

