program atlun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  {$ifdef mswindows}
  windows,
  {$endif}
  Forms, Dialogs, virtualmoon1, config, Math, OpenGLAdapter, OpenGLTokens,
  GLScene_RunTime, CraterList, dbutil, fmsg, glossary, splashunit, SysUtils,
  TurboPowerIPro, lazcontrols, tachartlazaruspkg, u_constant, cu_tz, cu_planet,
  u_projection, u_util, pu_moon, u_translation_database, u_translation,
  uniqueinstance_package, laz_synapse, downldialog, BigIma, uDE, mlb2,
  tabsdock, pu_ascomclient, pu_indiclient, cu_ascomrest, cu_dem, pu_demprofile, pu_updcatalog, cu_httpdownload;

{$R *.res}

{$ifdef mswindows}
{$ifdef cpui386}
// allow to use more than 2GB of memory
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
{$endif}
{$endif}

begin
  (* // To stdout by default, uncomment to write to file
  {$ifdef USEHEAPTRC}
  DeleteFile('/tmp/vma_heap.trc');
  SetHeapTraceOutput('/tmp/vma_heap.trc');
  {$endif}
  *)
  Application.Initialize;
  if not InitOpenGL then begin
     showmessage('Could not load the OpenGL library '+opengl32+' and '+glu32+crlf+'Please check your OpenGL installation.');
     halt;
  end;
  Application.CreateForm(TForm1, Form1);
  if (Form1.param.Count=0) then begin
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
  Application.CreateForm(Tf_tabsdock, f_tabsdock);
  Application.CreateForm(Tpop_scope, pop_scope);
  Application.CreateForm(Tpop_indi, pop_indi);
  Application.CreateForm(Tf_demprofile, f_demprofile);
  Application.CreateForm(Tf_updcatalog, f_updcatalog);
  if (Form1.param.IndexOf('-quit')<0) then begin
      Application.Run;
  end
  else Application.Terminate;
end.

