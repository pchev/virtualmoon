unit pu_search;

{$mode ObjFPC}{$H+}

interface

uses passql, passqlite, u_translation, u_util,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls, StdCtrls;

type

  { Tf_search }

  Tf_search = class(TForm)
    btncancel: TButton;
    Edit1: TEdit;
    ListBox1: TListBox;
    Panel1: TPanel;
    btnsearch: TSpeedButton;
    btnok: TButton;
    procedure btnsearchClick(Sender: TObject);
    procedure Edit1EditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    procedure SetLang;
  public
    dbm: TLiteDB;
    procedure SetFormation(txt: string);
  end;

var
  f_search: Tf_search;

implementation

{$R *.lfm}

{ Tf_search }

procedure Tf_search.SpeedButton1Click(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure Tf_search.SetLang;
begin
  btnsearch.Caption:=rsSearch;
  btnok.Caption:=rsOK;
  btncancel.Caption:=rsCancel;
end;

procedure Tf_search.SetFormation(txt: string);
begin
  edit1.Text:=txt;
  btnsearchClick(nil);
end;

procedure Tf_search.btnsearchClick(Sender: TObject);
var
  i:   integer;
  n: string;
begin
  n:=trim(Edit1.Text);
  if Length(n)<3 then exit;
  dbm.Query('select id,name from moon ' + ' where '+
      ' NAME like "' + trim(uppercase(n)) + '%"' +
      ' order by NAME limit 100;');
  ListBox1.Clear;
  for i := 0 to dbm.RowCount - 1 do
  begin
    ListBox1.Items.AddObject(dbm.Results[i].Format[1].AsString,TObject(PtrInt(dbm.Results[i].Format[0].AsInteger)));
  end;
  if ListBox1.Items.Count>0 then begin
     ListBox1.Selected[0]:=true;
  end;
end;

procedure Tf_search.Edit1EditingDone(Sender: TObject);
begin
  ActiveControl:=ListBox1;
  btnsearchClick(Sender);
end;

procedure Tf_search.FormCreate(Sender: TObject);
begin
  ScaleFormForFontSize(self,96);
  SetLang;
end;

procedure Tf_search.FormShow(Sender: TObject);
begin
  ActiveControl:=Edit1;
end;

end.

