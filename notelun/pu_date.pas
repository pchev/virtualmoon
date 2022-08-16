unit pu_date;

{$mode ObjFPC}{$H+}

interface

uses u_constant,
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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    function GetDate: double;
    procedure SetDate(value: double);
  public
    property Date: double read GetDate write SetDate;
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

procedure Tf_date.FormCreate(Sender: TObject);
begin
  DateEdit1.DateFormat:=datedisplay;

end;

procedure Tf_date.FormShow(Sender: TObject);
begin
  ActiveControl:=btncancel;
end;

function Tf_date.GetDate: double;
begin
  result:=trunc(DateEdit1.Date)+(h.Value+m.Value/60+s.Value/3600)/24;
end;

procedure Tf_date.SetDate(value: double);
var x: double;
begin
  DateEdit1.Date:=trunc(value);
  x:=frac(value)*24;
  h.Value:=trunc(x);
  x:=frac(x)*60;
  m.Value:=trunc(x);
  x:=frac(x)*60;
  s.Value:=trunc(x);
end;

end.

