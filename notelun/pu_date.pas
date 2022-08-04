unit pu_date;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, EditBtn, Buttons, Spin, StdCtrls;

type

  { Tf_date }

  Tf_date = class(TForm)
    btncancel: TButton;
    btnok: TButton;
    DateEdit1: TDateEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    H: TSpinEdit;
    M: TSpinEdit;
    S: TSpinEdit;
    procedure SpeedButton1Click(Sender: TObject);
  private

  public

  end;

var
  f_date: Tf_date;

implementation

{$R *.lfm}

{ Tf_date }

procedure Tf_date.SpeedButton1Click(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

end.

