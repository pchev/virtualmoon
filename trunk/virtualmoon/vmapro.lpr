program vmapro;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, LResources, virtualmoon1, config,
  CraterList, dbutil, fmsg, glossary, splashunit, telescope, SysUtils,
  ImagesForLazarus, elp82, TurboPowerIPro, u_constant, cu_tz, cu_planet,
  u_projection, u_util, pu_moon, u_translation_database, u_translation,
  uniqueinstance_package;

{$IFDEF WINDOWS}{$R vmapro.rc}{$ENDIF}

begin
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

