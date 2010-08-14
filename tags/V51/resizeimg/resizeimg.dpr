program resizeimg;

uses
  Forms,
  Windows,
  SysUtils,
  fiximg1 in 'fiximg1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
