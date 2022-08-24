unit pu_listselection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { Tf_listselection }

  Tf_listselection = class(TForm)
    btncancel: TButton;
    btnok: TButton;
    Selection: TComboBox;
    Prompt: TLabel;
  private

  public

  end;

var
  f_listselection: Tf_listselection;

implementation

{$R *.lfm}

end.

