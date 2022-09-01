program ArchiveEdit;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms, GLScene_RunTime, Interfaces,
  Main in 'Main.pas' {Form1},
  FolderDialog in 'FolderDialog.pas' {FDialog},
  FolderSelect in 'FolderSelect.pas' {FolderSel};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFDialog, FDialog);
  Application.CreateForm(TFolderSel, FolderSel);
  Application.Run;
end.
