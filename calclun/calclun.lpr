program calclun;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, printer4lazarus, tachartlazaruspkg, calclun_main, cspice, sysutils, moon_spice, u_util, u_projection, u_constant, pas_spice
  { you can add units after this };

{$R *.res}

begin
  {$ifdef USEHEAPTRC}
    {$ifdef mswindows}
      DeleteFile('C:\Temp\calclun_heap.trc');
      SetHeapTraceOutput('C:\Temp\calclun_heap.trc');
    {$else}
      DeleteFile('/tmp/calclun_heap.trc');
      SetHeapTraceOutput('/tmp/calclun_heap.trc');
    {$endif}
  {$endif}

  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(Tf_calclun, f_calclun);
  Application.Run;
end.

