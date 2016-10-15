program atlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  InterfaceBase, LCLVersion, // version number
  Forms, Dialogs, virtualmoon1, config, Math, OpenGLAdapter, OpenGLTokens,
  GLScene_RunTime, CraterList, dbutil, fmsg, glossary, splashunit, telescope,
  SysUtils, TurboPowerIPro, u_constant, cu_tz, cu_planet, u_projection, u_util,
  pu_moon, u_translation_database, u_translation, uniqueinstance_package,
  pu_features, BigIma, uDE, mlb2, pu_ephem;

var i:integer;

{$R *.res}

begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  {$ifdef USEHEAPTRC}
  SetHeapTraceOutput('/tmp/vma_heap.trc');
  {$endif}
  Application.Initialize;
  compile_time:={$I %DATE%}+' '+{$I %TIME%};
  compile_version:='Lazarus '+lcl_version+' Free Pascal '+{$I %FPCVERSION%}+' '+{$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%}+'-'+LCLPlatformDirNames[WidgetSet.LCLPlatform];
  Splashversion := AVLversion+blank+compile_time;
  if not InitOpenGL then begin
     showmessage('Could not load the OpenGL library '+opengl32+' and '+glu32+crlf+'Please check your OpenGL installation.');
     halt;
  end;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(Tf_craterlist, f_craterlist);
  Application.CreateForm(Tf_features, f_features);
  if not Form1.param.Find('-quit',i) then begin
      splash := Tsplash.create(application);
      splash.VersionName:=VersionName;
      splash.Splashversion:=Splashversion;
      splash.transmsg:=transmsg;
      splash.show;
      splash.refresh;
      Application.Run;
  end
  else Application.Terminate;
end.

