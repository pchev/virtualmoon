program photlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  InterfaceBase, LCLVersion, // version number
  Forms, LResources, imagesforlazarus
  { add your units here }, pu_photlun, pu_photo, u_translation,
  uniqueinstance_package, pu_config, u_constant, u_bitmap, u_util;

 var i:integer;

{$IFDEF WINDOWS}{$R photlun.rc}{$ENDIF}

begin
  {$I photlun.lrs}
  Application.Initialize;
  compile_time:={$I %DATE%}+' '+{$I %TIME%};
  compile_version:='Lazarus '+lcl_version+' Free Pascal '+{$I %FPCVERSION%}+' '+{$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%}+'-'+LCLPlatformDirNames[WidgetSet.LCLPlatform];
  Splashversion := AVLversion+blank+compile_time;
  Application.CreateForm(Tf_photlun, f_photlun);
  if not f_photlun.param.Find('-quit',i) then begin
    Application.CreateForm(Tf_config, f_config);
    Application.Run;
  end
  else Application.Terminate;
end.

