program img2png;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, img2png1, LResources
  { you can add units after this };

{$IFDEF WINDOWS}{$R img2png.rc}{$ENDIF}

begin
  {$I img2png.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

