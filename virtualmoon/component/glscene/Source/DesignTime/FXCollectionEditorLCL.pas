// FXCollectionEditor
{: Egg<p>

  Edits a TXCollection<p>

  <b>Historique : </b><font size=-1><ul>
      <li>20/05/10 - Yar - Fixes for Linux x64
      <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
      <li>03/07/04 - LR - Make change for Linux
      <li>12/07/03 - DanB - Fixed crash when owner deleted
      <li>27/02/02 - Egg - Fixed crash after item deletion
      <li>11/04/00 - Egg - Fixed crashes in IDE
    <li>06/04/00 - Egg - Creation
  </ul></font>
}
unit FXCollectionEditorLCL;

interface

{$i GLScene.inc}

uses
  LResources, Forms, XCollection, Controls, Classes, ActnList,
  Menus, ComCtrls;

type

  { TXCollectionEditor }

  TXCollectionEditor = class(TForm)
    ListView: TListView;
    PMListView: TPopupMenu;
    ActionList: TActionList;
    ACRemove: TAction;
    ACMoveUp: TAction;
    ACMoveDown: TAction;
    ImageList: TImageList;
    MIAdd: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Moveup1: TMenuItem;
    Movedown1: TMenuItem;
    ToolBar1: TToolBar;
    TBAdd: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    PMToolBar: TPopupMenu;
    procedure ListViewClick(Sender: TObject);
    procedure TBAddClick(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ACRemoveExecute(Sender: TObject);
    procedure ACMoveUpExecute(Sender: TObject);
    procedure ACMoveDownExecute(Sender: TObject);
    procedure PMToolBarPopup(Sender: TObject);
    procedure PMListViewPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { private declarations }
    FXCollection: TXCollection;
    updatingListView: boolean;
    procedure PrepareListView;
    procedure PrepareXCollectionItemPopup(AParent: TMenuItem);
    procedure OnAddXCollectionItemClick(Sender: TObject);
    procedure OnNameChanged(Sender: TObject);
    procedure OnXCollectionDestroyed(Sender: TObject);
  public
    { public declarations }
    procedure SetXCollection(aXCollection: TXCollection);
  end;

function XCollectionEditor: TXCollectionEditor;
procedure ReleaseXCollectionEditor;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  SysUtils, GLBehaviours, GLScene, Dialogs, PropEdits;

resourcestring
  cXCollectionEditor = 'XCollection editor';

var
  vXCollectionEditor: TXCollectionEditor;

function XCollectionEditor: TXCollectionEditor;
begin
  if not Assigned(vXCollectionEditor) then
    vXCollectionEditor := TXCollectionEditor.Create(nil);
  Result := vXCollectionEditor;
end;

procedure ReleaseXCollectionEditor;
begin
  if Assigned(vXCollectionEditor) then
  begin
    vXCollectionEditor.Release;
    vXCollectionEditor := nil;
  end;
end;

// FormCreate
procedure TXCollectionEditor.FormCreate(Sender: TObject);
begin
  RegisterGLBehaviourNameChangeEvent(OnNameChanged);
  RegisterXCollectionDestroyEvent(OnXCollectionDestroyed);
end;

// FormDestroy

procedure TXCollectionEditor.FormDestroy(Sender: TObject);
begin
  DeRegisterGLBehaviourNameChangeEvent(OnNameChanged);
  DeRegisterXCollectionDestroyEvent(OnXCollectionDestroyed);
end;

// FormHide

procedure TXCollectionEditor.FormHide(Sender: TObject);
begin
  SetXCollection(nil);
  ReleaseXCollectionEditor;
end;

// SetXCollection

procedure TXCollectionEditor.SetXCollection(aXCollection: TXCollection);
begin
  FXCollection := aXCollection;
  if Assigned(FXCollection) then
  begin
    Caption := FXCollection.GetNamePath;
  end
  else
  begin
    Caption := cXCollectionEditor;
  end;
  PrepareListView;
end;

// TBAddClick

procedure TXCollectionEditor.TBAddClick(Sender: TObject);
begin
  TBAdd.CheckMenuDropdown;
end;

procedure TXCollectionEditor.ListViewClick(Sender: TObject);
var
  sel: boolean;
begin
  if Assigned(GlobalDesignHook) and (not updatingListView) then
  begin
    // setup enablings
    sel := (ListView.Selected <> nil);
    TBAdd.Enabled := Assigned(GlobalDesignHook);
    ACRemove.Enabled := sel;
    ACMoveUp.Enabled := sel and (ListView.Selected.Index > 0);
    ACMoveDown.Enabled := sel and (ListView.Selected.Index < ListView.Items.Count - 1);
    if Assigned(GlobalDesignHook) then
      if sel then
        GlobalDesignHook.SelectOnlyThis(TXCollectionItem(ListView.Selected.Data))
      else
        GlobalDesignHook.SelectOnlyThis(nil);
  end;
end;

// ListViewChange

procedure TXCollectionEditor.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  sel: boolean;
begin
  if (Change = ctState) and Assigned(GlobalDesignHook) and (not updatingListView) then
  begin
    // setup enablings
    sel := (ListView.Selected <> nil);
    TBAdd.Enabled := Assigned(GlobalDesignHook);
    ACRemove.Enabled := sel;
    ACMoveUp.Enabled := sel and (ListView.Selected.Index > 0);
    ACMoveDown.Enabled := sel and (ListView.Selected.Index < ListView.Items.Count - 1);
    if Assigned(GlobalDesignHook) then
      if sel then
        GlobalDesignHook.SelectOnlyThis(TXCollectionItem(ListView.Selected.Data))
      else
        GlobalDesignHook.SelectOnlyThis(nil);
  end;
end;

// PrepareListView

procedure TXCollectionEditor.PrepareListView;
var
  i: integer;
  prevSelData: Pointer;
  XCollectionItem: TXCollectionItem;
begin
  Assert(Assigned(ListView));
  updatingListView := True;
  try
    if ListView.Selected <> nil then
      prevSelData := ListView.Selected.Data
    else
      prevSelData := nil;
    with ListView, ListView.Items do
    begin
      BeginUpdate;
      Clear;
      if Assigned(FXCollection) then
      begin
        for i := 0 to FXCollection.Count - 1 do
          with Add do
          begin
            XCollectionItem := FXCollection[i];
            Caption := Format('%d - %s', [i, XCollectionItem.Name]);
            SubItems.Add(XCollectionItem.FriendlyName);
            Data := XCollectionItem;
          end;
        if prevSelData <> nil then
          ListView.Selected := ListView.Items.FindData(prevSelData);
      end;
      EndUpdate;
    end;
  finally
    updatingListView := False;
  end;
  ListViewChange(Self, nil, ctState);
end;

// PrepareXCollectionItemPopup

procedure TXCollectionEditor.PrepareXCollectionItemPopup(AParent: TMenuItem);
var
  i: integer;
  list: TList;
  XCollectionItemClass: TXCollectionItemClass;
  mi: TMenuItem;
begin
  list := GetXCollectionItemClassesList(FXCollection.ItemsClass);
  try
    AParent.Clear;
    for i := 0 to list.Count - 1 do
    begin
      XCollectionItemClass := TXCollectionItemClass(list[i]);
      mi := TMenuItem.Create(owner);
      mi.Caption := XCollectionItemClass.FriendlyName;
      mi.OnClick := OnAddXCollectionItemClick;
      mi.Tag := integer(XCollectionItemClass);
      mi.Enabled := Assigned(FXCollection) and FXCollection.CanAdd(XCollectionItemClass);
      AParent.Add(mi);
    end;
  finally
    list.Free;
  end;
end;

// OnNameChanged

procedure TXCollectionEditor.OnNameChanged(Sender: TObject);
begin
  if TXCollectionItem(Sender).Owner = FXCollection then
    PrepareListView;
end;

// OnXCollectionDestroyed

procedure TXCollectionEditor.OnXCollectionDestroyed(Sender: TObject);
begin
  if TXCollection(Sender) = FXCollection then
    Close;
end;

// OnAddXCollectionItemClick

procedure TXCollectionEditor.OnAddXCollectionItemClick(Sender: TObject);
var
  XCollectionItemClass: TXCollectionItemClass;
  XCollectionItem: TXCollectionItem;
begin
  XCollectionItemClass := TXCollectionItemClass(PtrUInt((Sender as TMenuItem).Tag));
  XCollectionItem := XCollectionItemClass.Create(FXCollection);
  PrepareListView;
  ListView.Selected := ListView.Items.FindData(XCollectionItem);
  GlobalDesignHook.Modified(Sender);
end;

// ACRemoveExecute

procedure TXCollectionEditor.ACRemoveExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    GlobalDesignHook.Modified(Sender);
    GlobalDesignHook.SelectOnlyThis(nil);
    TXCollectionItem(ListView.Selected.Data).Free;
    ListView.Selected.Free;
    ListViewChange(Self, nil, ctState);
  end;
end;

// ACMoveUpExecute

procedure TXCollectionEditor.ACMoveUpExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    TXCollectionItem(ListView.Selected.Data).MoveUp;
    PrepareListView;
    GlobalDesignHook.Modified(Sender);
  end;
end;

// ACMoveDownExecute

procedure TXCollectionEditor.ACMoveDownExecute(Sender: TObject);
begin
  if ListView.Selected <> nil then
  begin
    TXCollectionItem(ListView.Selected.Data).MoveDown;
    PrepareListView;
    GlobalDesignHook.Modified(Sender);
  end;
end;

// PMToolBarPopup

procedure TXCollectionEditor.PMToolBarPopup(Sender: TObject);
begin
  PrepareXCollectionItemPopup(PMToolBar.Items);
end;

// PMListViewPopup

procedure TXCollectionEditor.PMListViewPopup(Sender: TObject);
begin
  PrepareXCollectionItemPopup(MIAdd);
end;

initialization

  {$i FXCollectionEditorLCL.lrs}

finalization
  ReleaseXCollectionEditor;

end.
