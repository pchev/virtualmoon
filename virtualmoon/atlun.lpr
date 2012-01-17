program atlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  InterfaceBase, LCLVersion, // version number
  Forms, glscene_runtime, virtualmoon1, config,
  CraterList, dbutil, fmsg, glossary, splashunit, telescope, SysUtils, TurboPowerIPro, u_constant, cu_tz, cu_planet,
  u_projection, u_util, pu_moon, u_translation_database, u_translation,
  uniqueinstance_package, pu_features, BigIma, uDE, mlb2, pu_ephem;

var i:integer;

{$R *.res}

begin
  Application.Initialize;
  compile_time:={$I %DATE%}+' '+{$I %TIME%};
  compile_version:='Lazarus '+lcl_version+' Free Pascal '+{$I %FPCVERSION%}+' '+{$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%}+'-'+LCLPlatformDirNames[WidgetSet.LCLPlatform];
  Splashversion := AVLversion+blank+compile_time;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(Tf_craterlist, f_craterlist);
  Application.CreateForm(Tf_features, f_features);
  if not Form1.param.Find('-quit',i) then begin
      splash := Tsplash.create(application);
      splash.VersionName:=VersionName;
      splash.Splashversion:=Splashversion;
      splash.transmsg:=transmsg;
   {   splash.show;
      splash.refresh;  }
 //     Form1.Init;
      Application.Run;
  end
  else Application.Terminate;
end.

