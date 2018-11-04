unit tabsdock;

{$mode objfpc}{$H+}

interface

uses   u_translation,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { Tf_tabsdock }

  Tf_tabsdock = class(TForm)
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    FonReturnControl: TNotifyEvent;
  public
    procedure SetLang;
    property onReturnControl: TNotifyEvent read FonReturnControl write FonReturnControl;
  end;

var
  f_tabsdock: Tf_tabsdock;

implementation

{$R *.lfm}

{ Tf_tabsdock }

procedure Tf_tabsdock.SetLang;
begin
  Caption:=rstitle;
end;

procedure Tf_tabsdock.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=False;
  if Assigned(FonReturnControl) then FonReturnControl(self);
end;

end.

