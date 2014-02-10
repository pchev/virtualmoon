program cclun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, cclun_main, u_translation, u_constant;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_cclun, f_cclun);
  Application.Run;
end.

