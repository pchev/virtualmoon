program weblun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, printer4lazarus, weblun_main, u_translation, u_util, u_constant, libsql, downldialog
  { you can add units after this };

var i : integer;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Title:='';
  Application.Initialize;
  Application.CreateForm(Tf_weblun, f_weblun);
  if (f_weblun.param.IndexOf('-quit')<0) then begin
     Application.Run;
  end
  else Application.Terminate;
end.

