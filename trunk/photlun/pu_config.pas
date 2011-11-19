unit pu_config;

{$mode objfpc}{$H+}

interface

uses
  u_translation, u_util,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ExtCtrls;

type

  { Tf_config }

  Tf_config = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    PanelBottom: TPanel;
    PanelTop: TPanel;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    { private declarations }
    procedure SetLang;
  public
    { public declarations }
  end; 

var
  f_config: Tf_config;

implementation

{$R pu_config.lfm}

{ Tf_config }

procedure Tf_config.SetLang;
begin
    caption:=rsLibrarySetti;
    StringGrid1.Cells[0, 0]:=rsRotation;
    StringGrid1.Cells[1, 0]:=rsName;
    StringGrid1.Cells[2, 0]:=rsFolder;
    Label1.Caption:=rsImageFolders;
    Button1.Caption:=rsOK;
    Button2.Caption:=rsCancel;
end;

procedure Tf_config.FormShow(Sender: TObject);
begin
  StringGrid1.ColWidths[0]:=StringGrid1.DefaultColWidth;
  StringGrid1.ColWidths[1]:=StringGrid1.DefaultColWidth;
  StringGrid1.ColWidths[2]:=StringGrid1.ClientWidth-StringGrid1.ColWidths[0]-StringGrid1.ColWidths[1];
  StringGrid1.Selection := Rect(0,3,0,3);
end;

procedure Tf_config.FormCreate(Sender: TObject);
begin
  {$ifdef mswindows}
  ScaleForm(self,Screen.PixelsPerInch/96);
  {$endif}
  SetLang;
end;

procedure Tf_config.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  if (aCol=1)and(aRow<=2) then CanSelect:=False;
end;

end.
