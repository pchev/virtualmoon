// FXCollectionEditor
{: Egg<p>

	Edits a TXCollection<p>

	<b>Historique : </b><font size=-1><ul>
      <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
      <li>03/07/04 - LR - Make change for Linux
      <li>12/07/03 - DanB - Fixed crash when owner deleted        
      <li>27/02/02 - Egg - Fixed crash after item deletion
      <li>11/04/00 - Egg - Fixed crashes in IDE
		<li>06/04/00 - Egg - Creation
	</ul></font>
}
unit FXCollectionEditor;

interface

{$i GLScene.inc}

uses
  LResources, Forms, XCollection, Messages, ImgList, Controls, Classes, ActnList,
  Menus, ComCtrls, Propedits;

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
	 procedure ListViewChange(Sender: TObject; Item: TListItem;
		Change: TItemChange);
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
	 FXCollection : TXCollection;
//    ownerComponent : TComponent;
//CRB-We want this:    FDesigner : {$ifdef GLS_DELPHI_6_UP} IDesigner {$else} IFormDesigner {$endif};
    updatingListView : Boolean;
         function AddToHackList(aXC: TXCollectionItemClass): Integer;
	 procedure PrepareListView;
  	 procedure PrepareXCollectionItemPopup(parent : TMenuItem);
	 procedure OnAddXCollectionItemClick(Sender : TObject);
    procedure OnNameChanged(Sender : TObject);
    procedure OnXCollectionDestroyed(Sender : TObject);
  protected
	 procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { public declarations }
    procedure SetXCollection(aXCollection: TXCollection);
(*CRB-want    procedure SetXCollection(aXCollection: TXCollection;
         designer: {$ifdef GLS_DELPHI_6_UP} IDesigner {$else} IFormDesigner {$endif});
*)        
  end;

function XCollectionEditor : TXCollectionEditor;
procedure ReleaseXCollectionEditor;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  GLMisc, SysUtils, GLBehaviours, GLScene, Dialogs; 

resourcestring
   cXCollectionEditor = 'XCollection editor';

var
  hackList: TList;
	vXCollectionEditor : TXCollectionEditor;

function XCollectionEditor : TXCollectionEditor;
begin
	if not Assigned(vXCollectionEditor) then
      vXCollectionEditor:=TXCollectionEditor.Create(nil);
	Result:=vXCollectionEditor;
end;

procedure ReleaseXCollectionEditor;
begin
	if Assigned(vXCollectionEditor) then begin
		vXCollectionEditor.Release;
		vXCollectionEditor:=nil;
	end;
end;

// FormCreate
procedure TXCollectionEditor.FormCreate(Sender: TObject);
begin
   RegisterGLBehaviourNameChangeEvent(OnNameChanged);
   RegisterXCollectionDestroyEvent(OnXCollectionDestroyed);
end;

// FormDestroy
//
procedure TXCollectionEditor.FormDestroy(Sender: TObject);
begin
	DeRegisterGLBehaviourNameChangeEvent(OnNameChanged);
        DeRegisterXCollectionDestroyEvent(OnXCollectionDestroyed);        
end;

// FormHide
//
procedure TXCollectionEditor.FormHide(Sender: TObject);
begin
   SetXCollection(nil);
   ReleaseXCollectionEditor;
end;

function TXCollectionEditor.AddToHackList(aXC: TXCollectionItemClass): Integer;
begin
  hackList.Add(aXC);
  Result := hackList.Count - 1;
end;

// SetXCollection
//
procedure TXCollectionEditor.SetXCollection(aXCollection: TXCollection);
begin
//	if Assigned(ownerComponent) then
//		ownerComponent.RemoveFreeNotification(Self);
	FXCollection:=aXCollection;
	if Assigned(FXCollection) then begin
//		if Assigned(FXCollection.Owner) and (FXCollection.Owner is TComponent) then
//		ownerComponent:=TComponent(FXCollection.Owner);
//		if Assigned(ownerComponent) then
//			ownerComponent.FreeNotification(Self);
      Caption:=FXCollection.GetNamePath;
   end else begin
//      ownerComponent:=nil;
      Caption:=cXCollectionEditor;
   end;
   PrepareListView;
end;

// TBAddClick
//
procedure TXCollectionEditor.TBAddClick(Sender: TObject);
begin
	TBAdd.CheckMenuDropdown;
end;

procedure TXCollectionEditor.ListViewClick(Sender: TObject);
var
	sel : Boolean;
begin
	if Assigned(GlobalDesignHook) and (not updatingListView) then begin
		// setup enablings
		sel:=(ListView.Selected<>nil);
      TBAdd.Enabled:=Assigned(GlobalDesignHook);
		ACRemove.Enabled:=sel;
		ACMoveUp.Enabled:=sel and (ListView.Selected.Index>0);
		ACMoveDown.Enabled:=sel and (ListView.Selected.Index<ListView.Items.Count-1);
      if Assigned(GlobalDesignHook) then
         if sel then
           GlobalDesignHook.SelectOnlyThis(TXCollectionItem(ListView.Selected.Data))
         else
           GlobalDesignHook.SelectOnlyThis(nil);
(*            Designer.SelectComponent(TXCollectionItem(ListView.Selected.Data))
{$ifndef GLS_DELPHI_4}
         else Designer.NoSelection;
{$else}
         else Designer.SelectComponent(nil);
{$endif} *)
	end;
end;

// ListViewChange
//
procedure TXCollectionEditor.ListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
{var
	sel : Boolean;}
begin
(*	if (Change=ctState) and Assigned(GlobalDesignHook) and (not updatingListView) then begin
		// setup enablings
		sel:=(ListView.Selected<>nil);
      TBAdd.Enabled:=Assigned(GlobalDesignHook);
		ACRemove.Enabled:=sel;
		ACMoveUp.Enabled:=sel and (ListView.Selected.Index>0);
		ACMoveDown.Enabled:=sel and (ListView.Selected.Index<ListView.Items.Count-1);
      if Assigned(GlobalDesignHook) then
         if sel then
           GlobalDesignHook.SelectOnlyThis(TXCollectionItem(ListView.Selected.Data))
         else
           GlobalDesignHook.SelectOnlyThis(nil);*)
//            Designer.SelectComponent(TXCollectionItem(ListView.Selected.Data))
//{$ifndef GLS_DELPHI_4}
//         else Designer.NoSelection;
//{$else}
//         else Designer.SelectComponent(nil);
//{$endif}
//	end;
end;

// PrepareListView
//
procedure TXCollectionEditor.PrepareListView;
var
	i : Integer;
	prevSelData : Pointer;
	XCollectionItem : TXCollectionItem;
begin
   Assert(Assigned(ListView));
   updatingListView:=True;
   try
      if ListView.Selected<>nil then
         prevSelData:=ListView.Selected.Data
      else prevSelData:=nil;
      with ListView, ListView.Items do begin
         BeginUpdate;
         Clear;
         if Assigned(FXCollection) then begin
            for i:=0 to FXCollection.Count-1 do with Add do begin
               XCollectionItem:=FXCollection[i];
               Caption:=Format('%d - %s', [i, XCollectionItem.Name]);
               SubItems.Add(XCollectionItem.FriendlyName);
               Data:=XCollectionItem;
            end;
            if prevSelData<>nil then
//               ListView.Selected:=ListView.FindData(0, prevSelData, True, False);
               ListView.Selected:=ListView.Items.FindData(prevSelData);
         end;
         EndUpdate;
      end;
   finally
      updatingListView:=False;
   end;
   ListViewChange(Self, nil, ctState);
end;

// PrepareXCollectionItemPopup
//
procedure TXCollectionEditor.PrepareXCollectionItemPopup(parent : TMenuItem);
var
	i : Integer;
	list : TList;
	XCollectionItemClass : TXCollectionItemClass;
	mi : TMenuItem;
begin
	list:=GetXCollectionItemClassesList(FXCollection.ItemsClass);
	try
		parent.Clear;
		for i:=0 to list.Count-1 do begin
			XCollectionItemClass:=TXCollectionItemClass(list[i]);
			mi:=TMenuItem.Create(owner);
			mi.Caption:=XCollectionItemClass.FriendlyName;
			mi.OnClick:=OnAddXCollectionItemClick;
			mi.Tag:=AddToHackList(XCollectionItemClass);
			mi.Enabled:=Assigned(FXCollection) and FXCollection.CanAdd(XCollectionItemClass);
			parent.Add(mi);
		end;
	finally
		list.Free;
	end;
end;

// OnNameChanged
//
procedure TXCollectionEditor.OnNameChanged(Sender : TObject);
begin
   if TXCollectionItem(Sender).Owner=FXCollection then
      PrepareListView;
end;

// OnXCollectionDestroyed
//
procedure TXCollectionEditor.OnXCollectionDestroyed(Sender : TObject);
begin
   if TXCollection(Sender)=FXCollection then
      Close;
end;

// Notification
//
procedure TXCollectionEditor.Notification(AComponent: TComponent; Operation: TOperation);
begin
     {	if (Operation=opRemove) and (AComponent=ownerComponent) then begin
		ownerComponent:=nil;
		SetXCollection(nil, nil);
		Close;
	end;
        }
	inherited;
end;

// OnAddXCollectionItemClick
//
procedure TXCollectionEditor.OnAddXCollectionItemClick(Sender : TObject);
var
	XCollectionItemClass : TXCollectionItemClass;
	XCollectionItem : TXCollectionItem;
begin
	XCollectionItemClass:=TXCollectionItemClass(hackList[(Sender as TMenuItem).Tag]);
	XCollectionItem:=XCollectionItemClass.Create(FXCollection);
	PrepareListView;
//	ListView.Selected:=ListView.FindData(0, XCollectionItem, True, False);
	ListView.Selected:=ListView.Items.FindData(XCollectionItem);
//   Designer.Modified;
   GlobalDesignHook.Modified(Sender);
end;

// ACRemoveExecute
//
procedure TXCollectionEditor.ACRemoveExecute(Sender: TObject);
begin
	if ListView.Selected<>nil then begin
      GlobalDesignHook.Modified(Sender);
      GlobalDesignHook.SelectOnlyThis(nil);
(*{$ifndef GLS_DELPHI_4}
      Designer.NoSelection;
{$else}
      Designer.SelectComponent(nil);
{$endif}*)
		TXCollectionItem(ListView.Selected.Data).Free;
      ListView.Selected.Free;
      ListViewChange(Self, nil, ctState);
	end;
end;

// ACMoveUpExecute
//
procedure TXCollectionEditor.ACMoveUpExecute(Sender: TObject);
begin
	if ListView.Selected<>nil then begin
		TXCollectionItem(ListView.Selected.Data).MoveUp;
		PrepareListView;
      GlobalDesignHook.Modified(Sender);
	end;
end;

// ACMoveDownExecute
//
procedure TXCollectionEditor.ACMoveDownExecute(Sender: TObject);
begin
	if ListView.Selected<>nil then begin
		TXCollectionItem(ListView.Selected.Data).MoveDown;
		PrepareListView;
      GlobalDesignHook.Modified(Sender);
	end;
end;

// PMToolBarPopup
//
procedure TXCollectionEditor.PMToolBarPopup(Sender: TObject);
begin
	PrepareXCollectionItemPopup(PMToolBar.Items);
end;

// PMListViewPopup
//
procedure TXCollectionEditor.PMListViewPopup(Sender: TObject);
begin
	PrepareXCollectionItemPopup(MIAdd);
end;

initialization
  hackList := TList.Create;
  {$i fxcollectioneditor.lrs}

finalization
  hackList.Free;
   ReleaseXCollectionEditor;

end.


