//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   The object manager is used for registering classes together with a category,
   description + icon, so that they can be displayed visually.  This can then
   be used by run-time or design-time scene editors for choosing which
   scene objects to place into a scene.

   TODO: add some notification code, so that when a scene object is registered/
   unregistered, any editor that is using the object manager can be notified.

  History :  
       11/11/09 - DaStr - Improved FPC compatibility
                             (thanks Predator) (BugtrackerID = 2893580)
       25/07/09 - DaStr - Added $I GLScene.inc
       26/03/09 - DanB - Added PopulateMenuWithRegisteredSceneObjects procedure.
       14/03/09 - DanB - Created by moving TGLObjectManager in from GLSceneRegister.pas,
                            made some slight adjustments to allow resources being loaded
                            from separate packages.
  
}

unit GLObjectManager;

interface

{$I GLScene.inc}

uses
  Classes,
  SysUtils,
  Graphics,
  Controls,
  Menus,
   
  GLCrossPlatform,
  GLScene,LResources;

type

  PSceneObjectEntry = ^TGLSceneObjectEntry;
  // holds a relation between an scene object class, its global identification,
  // its location in the object stock and its icon reference
  TGLSceneObjectEntry = record
    ObjectClass: TGLSceneObjectClass;
    Name: string; // type name of the object
    Category: string; // category of object
    Index, // index into "FSceneObjectList"
    ImageIndex: Integer; // index into "FObjectIcons"
  end;

  // TGLObjectManager
  //
  TGLObjectManager = class(TComponent)
  private
     
    FSceneObjectList: TList;
    FObjectIcons: TImageList; // a list of icons for scene objects
{$IFDEF MSWINDOWS}
    FOverlayIndex, // indices into the object icon list
{$ENDIF}
    FSceneRootIndex,
      FCameraRootIndex,
      FLightsourceRootIndex,
      FObjectRootIndex: Integer;
  protected
     
    procedure DestroySceneObjectList;
    function FindSceneObjectClass(AObjectClass: TGLSceneObjectClass;
      const ASceneObject: string = ''): PSceneObjectEntry;

  public
     
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


    procedure CreateDefaultObjectIcons;

    function GetClassFromIndex(Index: Integer): TGLSceneObjectClass;
    function GetImageIndex(ASceneObject: TGLSceneObjectClass): Integer;
    function GetCategory(ASceneObject: TGLSceneObjectClass): string;
    procedure GetRegisteredSceneObjects(ObjectList: TStringList);
    procedure PopulateMenuWithRegisteredSceneObjects(AMenuItem: TMenuItem; aClickEvent: TNotifyEvent);
    // Registers a stock object and adds it to the stock object list
    procedure RegisterSceneObject(ASceneObject: TGLSceneObjectClass; const aName, aCategory: string; ResourceModule: Cardinal; ResourceName: string = ''); overload;

    // Unregisters a stock object and removes it from the stock object list
    procedure UnRegisterSceneObject(ASceneObject: TGLSceneObjectClass);

    property ObjectIcons: TImageList read FObjectIcons;
    property SceneRootIndex: Integer read FSceneRootIndex;
    property LightsourceRootIndex: Integer read FLightsourceRootIndex;
    property CameraRootIndex: Integer read FCameraRootIndex;
    property ObjectRootIndex: Integer read FObjectRootIndex;
  end;
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

//----------------- TGLObjectManager ---------------------------------------------

// Create
//
constructor TGLObjectManager.Create(AOwner: TComponent);
begin
  inherited;
  FSceneObjectList := TList.Create;
  // FObjectIcons Width + Height are set when you add the first bitmap
  FObjectIcons := TImageList.CreateSize(16, 16);

  CreateDefaultObjectIcons;

end;

// Destroy
//
destructor TGLObjectManager.Destroy;
begin
  DestroySceneObjectList;
  FObjectIcons.Free;
  inherited Destroy;
end;

// FindSceneObjectClass
//
function TGLObjectManager.FindSceneObjectClass(AObjectClass: TGLSceneObjectClass;
  const aSceneObject: string = ''): PSceneObjectEntry;
var
  I: Integer;
  Found: Boolean;
begin
  Result := nil;
  Found := False;
  with FSceneObjectList do
  begin
    for I := 0 to Count - 1 do
      with TGLSceneObjectEntry(Items[I]^) do
        if (ObjectClass = AObjectClass) and (Length(ASceneObject) = 0)
          or (CompareText(Name, ASceneObject) = 0) then
        begin
          Found := True;
          Break;
        end;
    if Found then
      Result := Items[I];
  end;
end;

// GetClassFromIndex
//
function TGLObjectManager.GetClassFromIndex(Index: Integer): TGLSceneObjectClass;
begin
  if Index < 0 then
    Index := 0;
  if Index > FSceneObjectList.Count - 1 then
    Index := FSceneObjectList.Count - 1;
  Result := TGLSceneObjectEntry(FSceneObjectList.Items[Index + 1]^).ObjectClass;
end;

// GetImageIndex
//
function TGLObjectManager.GetImageIndex(ASceneObject: TGLSceneObjectClass): Integer;
var
  classEntry: PSceneObjectEntry;
begin
  classEntry := FindSceneObjectClass(ASceneObject);
  if Assigned(classEntry) then
    Result := classEntry^.ImageIndex
  else
    Result := 0;
end;

// GetCategory
//

function TGLObjectManager.GetCategory(ASceneObject: TGLSceneObjectClass): string;
var
  classEntry: PSceneObjectEntry;
begin
  classEntry := FindSceneObjectClass(ASceneObject);
  if Assigned(classEntry) then
    Result := classEntry^.Category
  else
    Result := '';
end;

// GetRegisteredSceneObjects
//
procedure TGLObjectManager.GetRegisteredSceneObjects(objectList: TStringList);
var
  i: Integer;
begin
  if Assigned(objectList) then
    with objectList do
    begin
      Clear;
      for i := 0 to FSceneObjectList.Count - 1 do
        with TGLSceneObjectEntry(FSceneObjectList.Items[I]^) do
          AddObject(Name, Pointer(ObjectClass));
    end;
end;

procedure TGLObjectManager.PopulateMenuWithRegisteredSceneObjects(AMenuItem: TMenuItem;
  aClickEvent: TNotifyEvent);
var
  objectList: TStringList;
  i, j: Integer;
  item, currentParent: TMenuItem;
  currentCategory: string;
  soc: TGLSceneObjectClass;
begin
  objectList := TStringList.Create;
  try
    GetRegisteredSceneObjects(objectList);
    for i := 0 to objectList.Count - 1 do
      if objectList[i] <> '' then
      begin
        currentCategory := GetCategory(TGLSceneObjectClass(objectList.Objects[i]));
        if currentCategory = '' then
          currentParent := AMenuItem
        else
        begin
          currentParent := NewItem(currentCategory, 0, False, True, nil, 0, '');
          AMenuItem.Add(currentParent);
        end;
        for j := i to objectList.Count - 1 do
          if objectList[j] <> '' then
          begin
            soc := TGLSceneObjectClass(objectList.Objects[j]);
            if currentCategory = GetCategory(soc) then
            begin
              item := NewItem(objectList[j], 0, False, True, aClickEvent, 0, '');
              item.ImageIndex := GetImageIndex(soc);
              currentParent.Add(item);
              objectList[j] := '';
              if currentCategory = '' then
                Break;
            end;
          end;
      end;
  finally
    objectList.Free;
  end;
end;

// RegisterSceneObject
//
{$IFNDEF FPC}
// RegisterSceneObject
//

procedure TGLObjectManager.RegisterSceneObject(ASceneObject: TGLSceneObjectClass;
  const aName, aCategory: string);
var
  resBitmapName: string;
  bmp: TBitmap;
begin
  // Since no resource name was provided, assume it's the same as class name
  resBitmapName := ASceneObject.ClassName;
  bmp := TBitmap.Create;
  try
    // Try loading bitmap from module that class is in
    GLLoadBitmapFromInstance(FindClassHInstance(ASceneObject), bmp, resBitmapName);
    if bmp.Width = 0 then
      GLLoadBitmapFromInstance(HInstance, bmp, resBitmapName);
    // If resource was found, register scene object with bitmap
    if bmp.Width <> 0 then
    begin
      RegisterSceneObject(ASceneObject, aName, aCategory, bmp);
    end
    else
      // Resource not found, so register without bitmap
      RegisterSceneObject(ASceneObject, aName, aCategory, nil);
  finally
    bmp.Free;
  end;
end;

// RegisterSceneObject
//

procedure TGLObjectManager.RegisterSceneObject(ASceneObject: TGLSceneObjectClass; const aName, aCategory: string; aBitmap: TBitmap);
var
  newEntry: PSceneObjectEntry;
  bmp: TBitmap;
begin
  if Assigned(RegisterNoIconProc) then
    RegisterNoIcon([aSceneObject]);
  with FSceneObjectList do
  begin
    // make sure no class is registered twice
    if Assigned(FindSceneObjectClass(ASceneObject, AName)) then
      Exit;
    New(NewEntry);
    try
      with NewEntry^ do
      begin
        // object stock stuff
        // registered objects list stuff
        ObjectClass := ASceneObject;
        NewEntry^.Name := aName;
        NewEntry^.Category := aCategory;
        Index := FSceneObjectList.Count;
        if Assigned(aBitmap) then
        begin
          bmp := TBitmap.Create;
          try
            // If we just add the bitmap, and it has different dimensions, then
            // all icons will be cleared, so ensure this doesn't happen
            bmp.PixelFormat := glpf24bit;
            bmp.Width := FObjectIcons.Width;
            bmp.Height := FObjectIcons.Height;
            bmp.Canvas.Draw(0, 0, aBitmap);
            FObjectIcons.AddMasked(bmp, bmp.Canvas.Pixels[0, 0]);
            ImageIndex := FObjectIcons.Count - 1;
          finally
            bmp.free;
          end;
        end
        else
          ImageIndex := 0;
      end;
      Add(NewEntry);
    finally
      //
    end;
  end;
end;

// RegisterSceneObject
//

procedure TGLObjectManager.RegisterSceneObject(ASceneObject: TGLSceneObjectClass; const aName, aCategory: string; ResourceModule: Cardinal; ResourceName: string = '');
var
  bmp: TBitmap;
  resBitmapName: string;
begin
  if ResourceName = '' then
    resBitmapName := ASceneObject.ClassName
  else
    resBitmapName := ResourceName;
  bmp := TBitmap.Create;
  try
    // Load resource
    if (ResourceModule <> 0) then
      GLLoadBitmapFromInstance(ResourceModule, bmp, resBitmapName);
    // If the resource was found, then register scene object using the bitmap
    if bmp.Width > 0 then
      RegisterSceneObject(ASceneObject, aName, aCategory, bmp)
    else
      // Register the scene object with no icon
      RegisterSceneObject(ASceneObject, aName, aCategory, nil);
  finally
    bmp.Free;
  end;
end;
{$ELSE}

procedure TGLObjectManager.RegisterSceneObject(ASceneObject: TGLSceneObjectClass; const aName, aCategory: string; ResourceModule: Cardinal; ResourceName: string = '');
var
  newEntry: PSceneObjectEntry;
  pic: TPicture;
  resBitmapName: string;
begin
  //>>Lazarus will crash at this function
  if Assigned(RegisterNoIconProc) then
    RegisterNoIcon([ASceneObject]);
  //Writeln('GL Registered ',ASceneObject.classname);
  Classes.RegisterClass(ASceneObject);
  with FSceneObjectList do
  begin
    // make sure no class is registered twice
    if Assigned(FindSceneObjectClass(ASceneObject, AName)) then
      Exit;
    New(NewEntry);
    pic := TPicture.Create;
    try
      with NewEntry^ do
      begin
        // object stock stuff
        // registered objects list stuff
        ObjectClass := ASceneObject;
        NewEntry^.Name := aName;
        NewEntry^.Category := aCategory;
        Index := FSceneObjectList.Count;
        resBitmapName := ASceneObject.ClassName;
        if LazarusResources.Find(resBitmapName) <> nil then
        begin
          try
            FObjectIcons.AddLazarusResource(resBitmapName);
          except
          end;
          ImageIndex := FObjectIcons.Count - 1;
        end
        else
        begin
          ImageIndex := 0;
        end;
      end;
      Add(NewEntry);
    finally
      pic.Free;
    end;
  end;
end;
{$ENDIF}

// UnRegisterSceneObject
//

procedure TGLObjectManager.UnRegisterSceneObject(ASceneObject: TGLSceneObjectClass);
var
  oldEntry: PSceneObjectEntry;
begin
  // find the class in the scene object list
  OldEntry := FindSceneObjectClass(ASceneObject);
  // found?
  if assigned(OldEntry) then
  begin
    // remove its entry from the list of registered objects
    FSceneObjectList.Remove(OldEntry);
    // finally free the memory for the entry
    Dispose(OldEntry);
  end;
end;

// CreateDefaultObjectIcons
//


procedure TGLObjectManager.CreateDefaultObjectIcons;
begin
  with FObjectIcons do
  begin
    if LazarusResources.Find('gls_cross') <> nil then
      try
        AddLazarusResource('gls_cross');
      except
      end;
    // FOverlayIndex:=Count-1;
    if LazarusResources.Find('gls_root') <> nil then
      try
        AddLazarusResource('gls_root');
      except
      end;
    FSceneRootIndex := Count - 1;
    if LazarusResources.Find('gls_camera') <> nil then
      try
        AddLazarusResource('gls_camera');
      except
      end;
    FCameraRootIndex := Count - 1;
    if LazarusResources.Find('gls_lights') <> nil then
      try
        AddLazarusResource('gls_lights');
      except
      end;
    FLightsourceRootIndex := Count - 1;
    if LazarusResources.Find('gls_objects') <> nil then
      try
        AddLazarusResource('gls_objects');
      except
      end;
    FObjectRootIndex := Count - 1;
    if LazarusResources.Find('gls_objects') <> nil then
      try
        AddLazarusResource('gls_objects');
      except
      end;
  end;
end;

// DestroySceneObjectList
//

procedure TGLObjectManager.DestroySceneObjectList;
var
  i: Integer;
begin
  with FSceneObjectList do
  begin
    for i := 0 to Count - 1 do
      Dispose(PSceneObjectEntry(Items[I]));
    Free;
  end;
end;

initialization

{$I ../../Resources/GLSceneObjects.lrs}


end.

