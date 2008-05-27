program fiximg;

uses
  Forms,
  Windows, SysUtils,
  fiximg1 in 'fiximg1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  dir:=ParamStr(1);
  if UpperCase(ExtractFileExt(dir))='.JPG' then Application.ShowMainForm := false;
  Application.Title := 'FixImg';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
