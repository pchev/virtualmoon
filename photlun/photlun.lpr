program photlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  InterfaceBase, LCLVersion, lclplatformdef, // version number
  Forms, printer4lazarus, pu_photlun, libsql, fu_img, pu_config, u_constant;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  compile_time:={$I %DATE%}+' '+{$I %TIME%};
  compile_version:='Lazarus '+lcl_version+' Free Pascal '+{$I %FPCVERSION%}+' '+{$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%}+'-'+LCLPlatformDirNames[WidgetSet.LCLPlatform];
  Splashversion := AVLversion+blank+compile_time;
  Application.CreateForm(Tf_photlun, f_photlun);
  if (f_photlun.param.IndexOf('-quit')<0) then begin
    Application.CreateForm(Tf_config, f_config);
    Application.Run;
  end
  else Application.Terminate;

end.

