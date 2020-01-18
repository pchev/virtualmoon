program datlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  InterfaceBase, LCLVersion, lclplatformdef, // version number
  Forms, printer4lazarus,
  fmsg, libsql, mlb2, vmabrowser1, vmabrowser2, vmabrowser3,
  vmabrowser4, vmabrowser5, u_util, u_constant;

var i : integer;

{$R *.res}

begin
  Application.Title:='DatLun';
  Application.Initialize;
  compile_time:={$I %DATE%}+' '+{$I %TIME%};
  compile_version:='Lazarus '+lcl_version+' Free Pascal '+{$I %FPCVERSION%}+' '+{$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%}+'-'+LCLPlatformDirNames[WidgetSet.LCLPlatform];
  Splashversion := AVLversion+blank+compile_time;
  Application.CreateForm(Tf_main, f_main);
  if (f_main.param.IndexOf('-quit')<0) then begin
      Application.CreateForm(TColumns, Columns);
      Application.CreateForm(TLoadCSV, LoadCSV);
      Application.CreateForm(TSelectDB, SelectDB);
      Application.CreateForm(TSelection, Selection);
      f_main.InitApp;
      Application.Run;
  end
  else Application.Terminate;
end.

