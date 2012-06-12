// FLibMaterialPicker
{: Egg<p>

 Allows choosing a material in a material library<p>

    <b>Historique : </b><font size=-1><ul>
      <li>05/09/08 - DanB - Removed Kylix support
      <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
      <li>19/12/06 - DaStr - LBMaterials.OnDblClick now handled
      <li>03/07/04 - LR  - Make change for Linux
      <li>14/02/00 - Egg - Creation
    </ul></font>
}
unit FLibMaterialPicker;

interface

{$I GLScene.inc}

uses
{$IFDEF GLS_DELPHI_XE2_UP}
  VCL.Forms, VCL.StdCtrls, VCL.Buttons, VCL.Controls,
{$ELSE}
  Forms, StdCtrls, Buttons, Controls,
{$ENDIF}
  FRMaterialPreview,  Classes, GLViewer,
  GLMaterial;

type
  TLibMaterialPicker = class(TForm)
    LBMaterials: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    BBOk: TBitBtn;
    BBCancel: TBitBtn;
    MPPreview: TRMaterialPreview;
    procedure LBMaterialsClick(Sender: TObject);
    procedure LBMaterialsKeyPress(Sender: TObject; var Key: Char);
    procedure LBMaterialsDblClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    function Execute(var materialName: TGLLibMaterialName;
      materialLibrary: TGLAbstractMaterialLibrary): Boolean;
  end;

function LibMaterialPicker: TLibMaterialPicker;
procedure ReleaseLibMaterialPicker;

implementation

{$R *.dfm}

var
  vLibMaterialPicker: TLibMaterialPicker;

function LibMaterialPicker: TLibMaterialPicker;
begin
  if not Assigned(vLibMaterialPicker) then
    vLibMaterialPicker := TLibMaterialPicker.Create(nil);
  Result := vLibMaterialPicker;
end;

procedure ReleaseLibMaterialPicker;
begin
  if Assigned(vLibMaterialPicker) then
  begin
    vLibMaterialPicker.Free;
    vLibMaterialPicker := nil;
  end;
end;

// Execute
//

function TLibMaterialPicker.Execute(var materialName: TGLLibMaterialName;
  materialLibrary: TGLAbstractMaterialLibrary): Boolean;
begin
  with LBMaterials do
  begin
    materialLibrary.SetNamesToTStrings(LBMaterials.Items);
    ItemIndex := Items.IndexOf(materialName);
    if (ItemIndex < 0) and (Items.Count > 0) then
      ItemIndex := 0;
    BBOk.Enabled := (Items.Count > 0);
  end;
  LBMaterialsClick(Self);
  Result := (ShowModal = mrOk);
  if Result then
  begin
    with LBMaterials do
      if ItemIndex >= 0 then
        materialName := Items[ItemIndex]
      else
        materialName := '';
  end;
end;

procedure TLibMaterialPicker.LBMaterialsClick(Sender: TObject);
begin
  with LBMaterials do
    if ItemIndex >= 0 then
      MPPreview.LibMaterial := TGLAbstractLibMaterial(Items.Objects[ItemIndex]);
end;

procedure TLibMaterialPicker.LBMaterialsKeyPress(Sender: TObject;
  var Key: Char);
begin
  LBMaterialsClick(Sender);
end;

procedure TLibMaterialPicker.LBMaterialsDblClick(Sender: TObject);
begin
  BBOk.Click;
end;

initialization

finalization
  ReleaseLibMaterialPicker;

end.

