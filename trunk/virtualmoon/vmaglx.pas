unit vmaglx;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLWin32Viewer;

type
  Tformglx = class(TForm)
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  formglx: Tformglx;

implementation

uses virtualmoonx;

{$R *.dfm}

procedure Tformglx.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  form1.FormMouseWheel(Sender,Shift,WheelDelta,MousePos,Handled);
end;

end.
