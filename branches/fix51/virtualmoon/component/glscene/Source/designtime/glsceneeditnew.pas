unit glsceneeditnew;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, lresources, forms, controls, graphics, dialogs, comctrls,
  stdctrls, buttons, menus,

  ComponentEditors,

  GLScene, GLLazarusRegister;

type
  TGLSceneEditorForm = class(TForm)
    MIAddCamera: TMenuItem;
    MIAddObject: TMenuItem;
    MISeparator1: TMenuItem;
    MIDelete: TMenuItem;
    MISeparator2: TMenuItem;
    MIMoveUp: TMenuItem;
    MIMoveDown: TMenuItem;
    SceneTreeView: TTreeView;
    TreeViewPopup: TPopupMenu;
    procedure MIAddObjectClick(Sender: TObject);
    procedure MIAddCameraClick(Sender: TObject);
    procedure MIDeleteClick(Sender: TObject);
    procedure MIMoveDownClick(Sender: TObject);
    procedure MIMoveUpClick(Sender: TObject);
    procedure SceneTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SceneTreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SceneTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure SceneTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure SceneTreeViewMouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SceneTreeViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SceneTreeViewSelectionChanged(Sender: TObject);
    procedure TreeViewPopupPopup(Sender: TObject);

  private
    { private declarations }
    FScene: TGLScene;
    FCurrentDesigner: TComponentEditorDesigner;
    
    FCamerasNode,
    FObjectsNode : TTreeNode;
    
    FLastMouseDownPos : TPoint;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure OnBaseSceneObjectNameChanged(Sender : TObject);
    function AddGLSObjectToTreeView(AParent : TTreeNode; AName : String; obj : TGLBaseSceneObject) : TTreeNode;
    procedure RepopulateSceneTreeView(parentNode : TTreeNode);
    function UniqueName(Component: TComponent): string;

  public
    { public declarations }
    constructor Create(AOwner : TComponent); override;
    procedure SetScene(AScene : TGLScene; ADesigner : TComponentEditorDesigner);

  end;

function GLSceneEditorForm : TGLSceneEditorForm;
procedure ReleaseGLSceneEditorForm;

implementation

var
  vGLSceneEditorForm: TGLSceneEditorForm;

function GLSceneEditorForm : TGLSceneEditorForm;
begin
  if not Assigned(vGLSceneEditorForm) then
    vGLSceneEditorForm:=TGLSceneEditorForm.Create(nil);
  Result:=vGLSceneEditorForm;
end;

procedure ReleaseGLSceneEditorForm;
begin
  if Assigned(vGLSceneEditorForm) then begin
    vGLSceneEditorForm.Free;
    vGLSceneEditorForm:=nil;
  end;
end;

function FindNodeByData(treeNodes : TTreeNodes; data : Pointer;
  baseNode : TTreeNode = nil) : TTreeNode;
var
  n : TTreeNode;
begin
  Result:=nil;
  if Assigned(baseNode) then begin
    n:=baseNode.getFirstChild;
    while Assigned(n) do begin
      if n.Data=data then begin
        Result:=n; Break;
      end else if n.HasChildren then begin
        Result:=FindNodeByData(treeNodes, data, n);
        if Assigned(Result) then Break;
      end;
      n:=baseNode.GetNextChild(n);
    end;
  end else begin
    n:=treeNodes.GetFirstNode;
    while Assigned(n) do begin
      if n.Data=data then begin
        Result:=n; Break;
      end else if n.HasChildren then begin
        Result:=FindNodeByData(treeNodes, data, n);
        if Assigned(Result) then Break;
      end;
      n:=n.getNextSibling;
    end;
  end;
end;

constructor TGLSCeneEditorForm.Create(AOwner : TComponent);
begin
  inherited;
  RegisterGLBaseSceneObjectNameChangeEvent(@OnBaseSceneObjectNameChanged);
end;

procedure TGLSceneEditorForm.Notification(AComponent: TComponent; Operation: TOperation);
var
  node : TTreeNode;
begin
  if Operation = opRemove then begin
    if FScene = AComponent then begin
      FScene:=nil;
      SetScene(nil, nil);
    end else begin
      node:=FindNodeByData(SceneTreeView.Items, AComponent);
      if Assigned(node) then begin
        if SceneTreeView.Selected = node then
          SceneTreeView.Selected:=node.Parent;
        RepopulateSceneTreeView(node.Parent);
      end;
    end;
  end;
  inherited;
end;

procedure TGLSceneEditorForm.OnBaseSceneObjectNameChanged(Sender : TObject);
var
  node : TTreeNode;
begin
  node:=FindNodeByData(SceneTreeView.Items, Sender);
  if Assigned(node) then
    node.Text:=(Sender as TGLBaseSceneObject).Name;
end;

function TGLSceneEditorForm.AddGLSObjectToTreeView(AParent : TTreeNode; AName : String; obj : TGLBaseSceneObject) : TTreeNode;
var
  i : Integer;
begin
  Result:=SceneTreeView.Items.AddChildObject(AParent, AName, obj);
  for i:=0 to obj.Count-1 do
    AddGLSObjectToTreeView(Result, obj.Children[i].name, obj.Children[i]);
end;

procedure TGLSceneEditorForm.RepopulateSceneTreeView(parentNode : TTreeNode);
var
  i : Integer;
  parentObj : TGLBaseSceneObject;
begin
  if not Assigned(parentNode) then Exit;
  parentObj:=TGLBaseSceneObject(parentNode.Data);
  parentNode.DeleteChildren;
  for i:=0 to parentObj.Count-1 do
    AddGLSObjectToTreeView(parentNode, parentObj[i].Name, parentObj[i]);
end;

procedure TGLSceneEditorForm.SetScene(AScene : TGLScene; ADesigner : TComponentEditorDesigner);

  function GetOrCreateCategory(category : String) : TMenuItem;
  var
    i : Integer;
  begin
    Result:=nil;
    for i:=0 to MIAddObject.Count-1 do
      if MIAddObject.Items[i].Caption = category then begin
        Result:=MIAddObject.Items[i];
        Break;
      end;
    if not Assigned(Result) then begin
      Result:=TMenuItem.Create(MIAddObject);
      Result.Caption:=category;
      MIAddObject.Add(Result);
    end;
  end;

var
  i : Integer;
  SceneObjects : TStringList;
  newItem, parentItem : TMenuItem;
  category : String;
begin
  FScene:=AScene;
  FCurrentDesigner:=ADesigner;

  SceneTreeView.Items.Clear;

  if not (Assigned(FScene) and Assigned(FCurrentDesigner)) then Exit;

  SceneTreeView.Items.BeginUpdate;
  FCamerasNode:=AddGLSObjectToTreeView(nil, 'Cameras', FScene.Cameras);
  FObjectsNode:=AddGLSObjectToTreeView(nil, 'Objects', FScene.Objects);

  SceneObjects:=TStringList.Create;
  ObjectManager.GetRegisteredSceneObjects(SceneObjects);
  MIAddObject.Clear;
  for i:=0 to SceneObjects.Count-1 do begin
    parentItem:=MIAddObject;
    
    category:=ObjectManager.GetCategory(TGLSceneObjectClass(SceneObjects.Objects[i]));
    if category<>'' then
      parentItem:=GetOrCreateCategory(category);

    newItem:=TMenuItem.Create(parentItem);
    newItem.Caption:=SceneObjects[i];
    newItem.Tag:=Integer(SceneObjects.Objects[i]);
    newItem.OnClick:=TNotifyEvent(@MIAddObjectClick);
    parentItem.Add(newItem);
  end;
  SceneObjects.Free;

  FCamerasNode.Expand(False);
  FObjectsNode.Expand(False);
  SceneTreeView.Items.EndUpdate;
end;

function TGLSceneEditorForm.UniqueName(Component: TComponent): string;
begin
  Result := FCurrentDesigner.CreateUniqueComponentName(Component.ClassName);
end;

procedure TGLSceneEditorForm.MIAddObjectClick(Sender: TObject);
var
  selectedNode : TTreeNode;
  objParent, newObject : TGLBaseSceneObject;
  newClass : TGLSceneObjectClass;
begin
  selectedNode:=SceneTreeView.Selected;
  if not Assigned(selectedNode) then
    selectedNode:=FObjectsNode;
  objParent:=TGLBaseSceneObject(selectedNode.Data);

  newClass:=TGLSceneObjectClass(TMenuItem(Sender).Tag);
  newObject:=newClass.Create(FCurrentDesigner.Form);
  newObject.Name:=UniqueName(newObject);
  newObject.MoveTo(objParent);
  SceneTreeView.Selected:=AddGLSObjectToTreeView(selectedNode, newObject.Name, newObject);
end;

procedure TGLSceneEditorForm.MIAddCameraClick(Sender: TObject);
var
  selectedNode : TTreeNode;
  objParent, newCamera : TGLBaseSceneObject;
  newClass : TGLSceneObjectClass;
begin
  selectedNode:=SceneTreeView.Selected;
  if not Assigned(selectedNode) then
    selectedNode:=FCamerasNode;
  objParent:=TGLBaseSceneObject(selectedNode.Data);

  newClass:=TGLCamera;
  newCamera:=newClass.Create(FCurrentDesigner.Form);
  newCamera.Name:=UniqueName(newCamera);
  newCamera.MoveTo(objParent);
  SceneTreeView.Selected:=AddGLSObjectToTreeView(selectedNode, newCamera.Name, newCamera);
end;

procedure TGLSceneEditorForm.MIDeleteClick(Sender: TObject);
var
  obj : TGLBaseSceneObject;
begin
  if not Assigned(SceneTreeView.Selected) then
    Exit;
  if (SceneTreeView.Selected = FCamerasNode)
  or (SceneTreeView.Selected = FObjectsNode) then
    Exit;

  obj:=TGLBaseSceneObject(SceneTreeView.Selected.Data);
  if Assigned(obj) then begin
    obj.Free;
    SceneTreeView.Selected.Free;
  end;
end;

procedure TGLSceneEditorForm.MIMoveUpClick(Sender: TObject);
var
  obj : TGLBaseSceneObject;
  parentNode : TTreeNode;
begin
  if not Assigned(SceneTreeView.Selected) then
    Exit;
  if (SceneTreeView.Selected = FCamerasNode)
  or (SceneTreeView.Selected = FObjectsNode) then
    Exit;

  obj:=TGLBaseSceneObject(SceneTreeView.Selected.Data);
  if Assigned(obj) then begin
    obj.MoveUp;
    parentNode:=SceneTreeView.Selected.Parent;
    RepopulateSceneTreeView(parentNode);
    SceneTreeView.Selected:=parentNode[obj.Index];
  end;
end;

procedure TGLSceneEditorForm.SceneTreeViewMouseDown(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FLastMouseDownPos:=Point(X,Y);
end;

procedure TGLSceneEditorForm.SceneTreeViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  node : TTreeNode;
begin
  if Shift=[ssLeft] then begin
    node:=SceneTreeView.Selected;
    if Assigned(node) and (node.Level>1) then
      if (Abs(FLastMouseDownPos.x-x)>4) or (Abs(FLastMouseDownPos.y-y)>4) then
        SceneTreeView.BeginDrag(False);
  end;
end;

procedure TGLSceneEditorForm.SceneTreeViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=False;
  if Assigned(SceneTreeView.DropTarget) then begin
    Accept:=True;
  end;
end;

procedure TGLSceneEditorForm.SceneTreeViewDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  dstNode, srcNode : TTreeNode;
begin
  if not (Source = SceneTreeView) then Exit;
  srcNode:=SceneTreeView.Selected;
  dstNode:=SceneTreeView.DropTarget;
  if Assigned(srcNode) and Assigned(dstNode) then begin
    // ... perform the move
  end;
end;

procedure TGLSceneEditorForm.MIMoveDownClick(Sender: TObject);
var
  obj : TGLBaseSceneObject;
  parentNode : TTreeNode;
begin
  if not Assigned(SceneTreeView.Selected) then
    Exit;
  if (SceneTreeView.Selected = FCamerasNode)
  or (SceneTreeView.Selected = FObjectsNode) then
    Exit;

  obj:=TGLBaseSceneObject(SceneTreeView.Selected.Data);
  if Assigned(obj) then begin
    obj.MoveDown;
    parentNode:=SceneTreeView.Selected.Parent;
    RepopulateSceneTreeView(parentNode);
    SceneTreeView.Selected:=parentNode[obj.Index];
  end;
end;

procedure TGLSceneEditorForm.SceneTreeViewEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  AllowEdit:=(Node.Level>1);
end;

procedure TGLSceneEditorForm.SceneTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
var
  obj : TGLBaseSceneObject;
begin
  obj:=TGLBaseSceneObject(SceneTreeView.Selected.Data);
  if FScene.FindSceneObject(S) = nil then
    obj.Name:=S
  else begin
    Messagedlg('A component named '+S+' already exists',mtWarning,[mbok],0);
    S:=obj.Name;
  end;
end;

procedure TGLSceneEditorForm.TreeViewPopupPopup(Sender: TObject);
var
  selectedObj : TGLBaseSceneObject;
begin
  if Assigned(SceneTreeView.Selected) then begin
    if SceneTreeView.Selected = FCamerasNode then begin
      MIAddObject.Enabled:=False;
      MIAddCamera.Enabled:=True;
    end else begin
      MIAddObject.Enabled:=True;
      MIAddCamera.Enabled:=False;
    end;

    if (SceneTreeView.Selected = FObjectsNode)
    or (SceneTreeView.Selected = FCamerasNode) then begin
      MIDelete.Enabled:=False;
      MIMoveUp.Enabled:=False;
      MIMoveDown.Enabled:=False;
    end else begin
      MIDelete.Enabled:=True;
      selectedObj:=TGLBaseSceneObject(SceneTreeView.Selected.Data);
      MIMoveUp.Enabled:=(selectedObj.Index>0);
      MIMoveDown.Enabled:=(selectedObj.Index<selectedObj.Parent.Count-1);
    end;
  end else begin
    MIAddObject.Enabled:=False;
    MIAddCamera.Enabled:=False;
    MIDelete.Enabled:=False;
  end;
end;

procedure TGLSceneEditorForm.SceneTreeViewSelectionChanged(Sender: TObject);
var
  obj : TGLBaseSceneObject;
begin
  if Assigned(SceneTreeView.Selected) then begin
    if  (SceneTreeView.Selected <> FObjectsNode)
    and (SceneTreeView.Selected <> FCamerasNode) then begin
      obj:=TGLBaseSceneObject(SceneTreeView.Selected.Data);
      if Assigned(obj) then
        FCurrentDesigner.SelectOnlyThisComponent(obj);
    end;
  end;
end;

initialization

  {$I GLSceneEditNew.lrs}

finalization

   ReleaseGLSceneEditorForm;

end.

