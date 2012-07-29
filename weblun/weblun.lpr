program weblun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, weblun_main, u_translation, libsql, uniqueinstance_package
  { you can add units after this };

var i : integer;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_weblun, f_weblun);
  if not f_weblun.param.Find('-quit',i) then begin
     f_weblun.InitApp;
     Application.Run;
  end
  else Application.Terminate;
end.

