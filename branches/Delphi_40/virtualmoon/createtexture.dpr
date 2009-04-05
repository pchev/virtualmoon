program createtexture;

uses
  Forms,
  pu_texture in 'pu_texture.pas' {Form1};

{$R admin.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

