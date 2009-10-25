program photlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources
  { add your units here }, pu_photlun, zoomimage, pu_photo, u_translation, uniqueinstance_package,
  pu_config;

 var i:integer;

{$IFDEF WINDOWS}{$R photlun.rc}{$ENDIF}

begin
  {$I photlun.lrs}
  Application.Initialize;
  Application.CreateForm(Tf_photlun, f_photlun);
  if not f_photlun.param.Find('-quit',i) then begin
    Application.CreateForm(Tf_config, f_config);
    Application.Run;
  end
  else Application.Terminate;
end.

