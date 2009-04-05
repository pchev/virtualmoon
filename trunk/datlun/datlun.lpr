program datlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here },
  u_util, fmsg, libsql, mlb2,
  vmabrowser1, vmabrowser2, vmabrowser3, vmabrowser4, vmabrowser5, uniqueinstance_package;

const IdMutex='Virtual_Moon_Atlas_Browser_mutex';
var ok : boolean;
    f : textfile;
    i : integer;


begin
  Application.Title:='DatLun';
  Application.Initialize;
  Application.ShowMainForm := true;
  Application.CreateForm(Tf_main, f_main);
  Application.CreateForm(TColumns, Columns);
  Application.CreateForm(TLoadCSV, LoadCSV);
  Application.CreateForm(TMsgForm, MsgForm);
  Application.CreateForm(TSelectDB, SelectDB);
  Application.CreateForm(TSelection, Selection);
  f_main.InitApp;
  Application.Run;
end.

