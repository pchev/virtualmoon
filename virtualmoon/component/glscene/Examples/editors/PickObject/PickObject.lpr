program PickObject;

{$mode objfpc}{$h+}

uses
  Forms, Interfaces,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
