program photlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, pu_photlun, zoomimage, pu_photo, u_translation, uniqueinstance_package,
  pu_config;





begin
  Application.Initialize;
  Application.CreateForm(Tf_photlun, f_photlun);
  Application.CreateForm(Tf_config, f_config);
  Application.Run;
end.

