program photlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  InterfaceBase, LCLVersion, lclplatformdef, // version number
  Forms, imagesforlazarus
  { add your units here }, pu_photlun, pu_photo, u_translation,
  uniqueinstance_package, pu_config, u_constant, u_bitmap, u_util;

 var i:integer;

{$R *.res}

begin
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

