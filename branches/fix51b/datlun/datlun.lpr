program datlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  InterfaceBase, LCLVersion, // version number
  Forms, LResources
  { add your units here },
  u_util, fmsg, libsql, mlb2, vmabrowser1, vmabrowser2, vmabrowser3,
  vmabrowser4, vmabrowser5, dbutil, u_constant, uniqueinstance_package;

var i : integer;


{$IFDEF WINDOWS}{$R datlun.rc}{$ENDIF}

begin
  {$I datlun.lrs}
  Application.Title:='DatLun';
  Application.Initialize;
  compile_time:={$I %DATE%}+' '+{$I %TIME%};
  compile_version:='Lazarus '+lcl_version+' Free Pascal '+{$I %FPCVERSION%}+' '+{$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%}+'-'+LCLPlatformDirNames[WidgetSet.LCLPlatform];
  Splashversion := AVLversion+blank+compile_time;
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

