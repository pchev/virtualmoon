program vmapro;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, LResources, virtualmoon1, config,
  CraterList, dbutil, fmsg, glossary, splashunit, telescope,
  ImagesForLazarus, elp82, series96, TurboPowerIPro, u_constant, cu_tz,
  cu_planet, u_projection, u_util, pu_moon, uniqueinstance_package;

{$IFDEF WINDOWS}{$R vmapro.rc}{$ENDIF}

begin
  {$I vmapro.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  splash := Tsplash.create(application);
  splash.VersionName:=VersionName;
  splash.Splashversion:=Splashversion;
  splash.transmsg:=transmsg;
  splash.show;
  splash.refresh;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(Tf_craterlist, f_craterlist);
  Form1.Init;
  Application.Run;
end.

