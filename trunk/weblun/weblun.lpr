program weblun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, weblun_main, u_translation, libsql, uniqueinstance_package, downldialog
  { you can add units after this };

var i : integer;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_weblun, f_weblun);
  if not f_weblun.param.Find('-quit',i) then begin
     Application.Run;
  end
  else Application.Terminate;
end.

