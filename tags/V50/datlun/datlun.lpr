program datlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources
  { add your units here },
  u_util, fmsg, libsql, mlb2, vmabrowser1, vmabrowser2, vmabrowser3,
  vmabrowser4, vmabrowser5, dbutil, uniqueinstance_package;

var i : integer;


{$IFDEF WINDOWS}{$R datlun.rc}{$ENDIF}

begin
  {$I datlun.lrs}
  Application.Title:='DatLun';
  Application.Initialize;
  Application.CreateForm(Tf_main, f_main);
  if not f_main.param.Find('-quit',i) then begin
      Application.CreateForm(TColumns, Columns);
      Application.CreateForm(TLoadCSV, LoadCSV);
      Application.CreateForm(TSelectDB, SelectDB);
      Application.CreateForm(TSelection, Selection);
      f_main.InitApp;
      Application.Run;
  end
  else Application.Terminate;
end.

