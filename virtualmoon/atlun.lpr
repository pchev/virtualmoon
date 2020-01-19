program atlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Dialogs, virtualmoon1, config, Math, OpenGLAdapter, OpenGLTokens,
  GLScene_RunTime, CraterList, dbutil, fmsg, glossary, splashunit,
  SysUtils, TurboPowerIPro, lazcontrols, u_constant, cu_tz, cu_planet, u_projection, u_util,
  pu_moon, u_translation_database, u_translation, uniqueinstance_package, synapse,
  pu_features, BigIma, uDE, mlb2, pu_ephem, tabsdock, pu_ascomclient, pu_indiclient, cu_ascomrest;

{$R *.res}

begin
  Application.Scaled:=True;
  {$ifdef USEHEAPTRC}
  SetHeapTraceOutput('/tmp/vma_heap.trc');
  {$endif}
  Application.Initialize;
  if not InitOpenGL then begin
     showmessage('Could not load the OpenGL library '+opengl32+' and '+glu32+crlf+'Please check your OpenGL installation.');
     halt;
  end;
  Application.CreateForm(TForm1, Form1);
  if (Form1.param.IndexOf('-quit')<0) then begin
      Splashversion := AVLversion+blank+compile_time;
      splash := Tsplash.create(application);
      splash.VersionName:=VersionName;
      splash.Splashversion:=Splashversion;
      splash.transmsg:=transmsg;
      splash.show;
      splash.refresh;
  end;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(Tf_craterlist, f_craterlist);
  Application.CreateForm(Tf_features, f_features);
  Application.CreateForm(Tf_tabsdock, f_tabsdock);
  Application.CreateForm(Tpop_scope, pop_scope);
  Application.CreateForm(Tpop_indi, pop_indi);
  if (Form1.param.IndexOf('-quit')<0) then begin
      Application.Run;
  end
  else Application.Terminate;
end.

