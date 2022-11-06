unit pu_listselection;

{$mode ObjFPC}{$H+}

interface

uses  u_util,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { Tf_listselection }

  Tf_listselection = class(TForm)
    btncancel: TButton;
    btnok: TButton;
    Selection: TComboBox;
    Prompt: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  f_listselection: Tf_listselection;

implementation

{$R *.lfm}

{ Tf_listselection }

procedure Tf_listselection.FormCreate(Sender: TObject);
begin
  ScaleFormForFontSize(self,96);
end;

end.

