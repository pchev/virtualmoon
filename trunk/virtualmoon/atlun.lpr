program atlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, LResources, virtualmoon1, config,
  CraterList, dbutil, fmsg, glossary, splashunit, telescope, SysUtils, TurboPowerIPro, u_constant, cu_tz, cu_planet,
  u_projection, u_util, pu_moon, u_translation_database, u_translation,
  uniqueinstance_package, pu_features, BigIma, uDE, mlb2;

var i:integer;

{$IFDEF WINDOWS}{$R atlun.rc}{$ENDIF}

begin
  {$I atlun.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  if not Form1.param.Find('-quit',i) then begin
      splash := Tsplash.create(application);
      splash.VersionName:=VersionName;
      splash.Splashversion:=Splashversion;
      splash.transmsg:=transmsg;
      splash.show;
      splash.refresh;
      Application.CreateForm(TForm2, Form2);
      Application.CreateForm(Tf_craterlist, f_craterlist);
      Application.CreateForm(Tf_features, f_features);
 //     Form1.Init;
      Application.Run;
  end
  else Application.Terminate;
end.

