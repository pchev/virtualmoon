unit glmaniphelper;

INTERFACE

uses
  classes, vectorgeometry, glscene, gltexture, opengl1x, sysutils,
  vectorlists, glcrossplatform, glcontext, glsilhouette, globjects,
  geometrybb, glgizmo, glviewer, controls, forms, glgizmoutils,
  contnrs, graphics,dialogs
  {$ifdef win32}
  ,windows
  {$else}
  {$endif}
  ;

CONST
  ROTATE_CURSOR = 5;
  POINT_TO_CURSOR = 6;
  MOVE_CURSOR = 7;

  INVISIBLE_OBJECTS_TAG = -1;

TYPE
  TMovingAxis = (maNone, maAxisX, maAxisY, maAxisZ, maAxisXY, maAxisXZ, maAxisYZ);
  TDisplayableAxisType = (daAxisX, daAxisY, daAxisZ);
  TDisplayableAxis = SET OF TDisplayableAxisType;

  TManipulationType = (mtMove, mtRotate);
  TRotationType = (rtGizmo, rtLocal);

  TGLSceneViewerCollection = CLASS;

  TGLManipHelper = CLASS(TComponent)
  PRIVATE
    BoundingBoxList: TObjectList;

    FMovingAxis: TMovingAxis;
    FScene: TGLScene;
    FManipulationType: TManipulationType;

{
    mx,
    my: Integer;
}
    FRotationSpeed: Single;
    FSelected: TObjectList;
    FSelectionCenter: TVector;
    FRotationType: TRotationType;

    FSensitivity: Integer;
    FOnSelectionChange: TNotifyEvent;
    FOnMoving: TNotifyEvent;
    FCameraPosition: TVector;
    FCamera: TGLCamera;
    FGizmoPosition: TVector;
    FDisplayableAxis: TDisplayableAxis;
    FViewers: TGLSceneViewerCollection;

    FActiveIndex: Integer;
    FEnabled: Boolean;

    PROCEDURE SetMovingAxis(CONST Value: TMovingAxis);
    PROCEDURE SetScene(CONST Value: TGLScene);
    PROCEDURE SetManipulationType(CONST Value: TManipulationType);
    PROCEDURE RotateAroundArbitraryAxis(anObject: TGLBaseSceneObject; Axis,
      Origin: TAffineVector; angle: Single);
    FUNCTION GetPickedObject(x, y, Index: Integer; Sensitivity: Integer = 1): TGLBaseSceneObject;
    PROCEDURE SetRotationSpeed(CONST Value: Single);
    PROCEDURE SetRotationType(CONST Value: TRotationType);

    PROCEDURE DrawSelectionRect;
    PROCEDURE StartRectSelection(X, Y: Integer);
    PROCEDURE DoRectSelection(X, Y: Integer);
    PROCEDURE SetSensitivity(CONST Value: Integer);

    PROCEDURE SetCameraPosition(CONST Value: TVector);
    PROCEDURE SetCamera(CONST Value: TGLCamera);
    PROCEDURE SetDisplayableAxis(CONST Value: TDisplayableAxis);
    PROCEDURE SetViewers(CONST Value: TGLSceneViewerCollection);
    PROCEDURE SetActiveIndex(CONST Value: Integer);
  PROTECTED
    GizmoDummyCube: TGLDummyCube;
    GizmoX,
      GizmoY,
      GizmoZ: TGLGizmoArrow;
    GizmoCornerXY,
      GizmoCornerXZ,
      GizmoCornerYZ: TGLGizmoCorner;

    PROCEDURE DoSelectionChange;

    PROPERTY DisplayableAxis: TDisplayableAxis READ FDisplayableAxis WRITE SetDisplayableAxis;
  PUBLIC
    CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
    DESTRUCTOR Destroy; OVERRIDE;

    PROCEDURE UpdateGizmo;
    PROCEDURE UpdateBB;

    PROCEDURE AddObject(AObject: TGLCustomSceneObject);
    PROCEDURE ClearObjects;

    FUNCTION TranslateSelected(oldPos, newPos: TVector): Boolean;
    PROCEDURE RotateSelected(DeltaX, DeltaY: Integer);

    PROPERTY ManipulationType: TManipulationType READ FManipulationType WRITE SetManipulationType;
    PROPERTY SelectionCenter: TVector READ FSelectionCenter;
    PROPERTY Selected: TObjectList READ FSelected;
    PROPERTY CameraPosition: TVector READ FCameraPosition WRITE SetCameraPosition;
    PROPERTY Camera: TGLCamera READ FCamera WRITE SetCamera;
  PUBLISHED
    PROPERTY Scene: TGLScene READ FScene WRITE SetScene;
    PROPERTY MovingAxis: TMovingAxis READ FMovingAxis WRITE SetMovingAxis;
    PROPERTY RotationSpeed: Single READ FRotationSpeed WRITE SetRotationSpeed;
    PROPERTY RotationType: TRotationType READ FRotationType WRITE SetRotationType;
    PROPERTY Sensitivity: Integer READ FSensitivity WRITE SetSensitivity;
    PROPERTY Viewers: TGLSceneViewerCollection READ FViewers WRITE SetViewers;
    PROPERTY ActiveIndex: Integer READ FActiveIndex WRITE SetActiveIndex;

    PROPERTY OnSelectionChange: TNotifyEvent READ FOnSelectionChange WRITE FOnSelectionChange;
    PROPERTY OnMoving: TNotifyEvent READ FOnMoving WRITE FOnMoving;
    PROPERTY Enabled: Boolean READ FEnabled WRITE FEnabled;
  END;


  TGLSceneViewerItem = CLASS(TCollectionItem)
  PRIVATE
    FCanvas: TCanvas;

    mx, my: Integer;

    pick: TGLCustomSceneObject;
    OldCursorPick: TGLCustomSceneObject;
    lastMouseWorldPos: TVector;
    lastcamWorldPos: TVector;
    diffPos: TVector;

    FRectSelection: Boolean;
    FSelRect: TRect;

    FViewer: TGLSceneViewer;
    FMouseDown: TMouseEvent;
    FMouseUp: TMouseEvent;
    FMouseMove: TMouseMoveEvent;
    FBeforeRender: TNotifyEvent;

    FDisplayableAxis: TDisplayableAxis;

    PROCEDURE ViewerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    PROCEDURE ViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    PROCEDURE ViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);


    FUNCTION MouseWorldPos(x, y: Integer): TVector;

    PROCEDURE SetDisplayableAxis(CONST Value: TDisplayableAxis);
    PROCEDURE SetViewer(CONST Value: TGLSceneViewer);
    FUNCTION GetPickedObject(x, y: Integer; Sensitivity: Integer = 1): TGLBaseSceneObject;
    PROCEDURE DrawSelectionRect;
    PROCEDURE DoRectSelection(X, Y: Integer);
    PROCEDURE StartRectSelection(X, Y: Integer);
    PROCEDURE GetRectanglePickedObject(ARect: TRect);
    PROCEDURE ViewerBeforeRender(Sender: TObject);
  PROTECTED
  PUBLIC
    CONSTRUCTOR Create(Collection: TCollection); OVERRIDE;
    DESTRUCTOR Destroy; OVERRIDE;
  PUBLISHED
    PROPERTY DisplayableAxis: TDisplayableAxis READ FDisplayableAxis WRITE SetDisplayableAxis;
    PROPERTY Viewer: TGLSceneViewer READ FViewer WRITE SetViewer;
  END;

  TGLSceneViewerCollection = CLASS(TCollection)
  PRIVATE
    FOwner: TComponent;
  PROTECTED
    FUNCTION GetOwner: TPersistent; OVERRIDE;
    FUNCTION GetItem(Index: Integer): TGLSceneViewerItem; REINTRODUCE;
    PROCEDURE SetItem(Index: Integer; Value:
      TGLSceneViewerItem);
    PROCEDURE Update(Item: TGLSceneViewerItem); REINTRODUCE;
  PUBLIC
    CONSTRUCTOR Create(AOwner: TComponent);
    DESTRUCTOR Destroy; OVERRIDE;

    FUNCTION Add: TGLSceneViewerItem;
    FUNCTION Insert(Index: Integer): TGLSceneViewerItem;
    PROCEDURE Delete(Index: Integer); REINTRODUCE;

    PROPERTY Items[Index: Integer]: TGLSceneViewerItem
    READ GetItem
      WRITE SetItem; DEFAULT;
  END;

IMPLEMENTATION

{.$R 'Cursor.res'}

{ TGLManipHelper }

PROCEDURE TGLManipHelper.RotateAroundArbitraryAxis(anObject: TGLBaseSceneObject; Axis,
  Origin: TAffineVector; angle: Single);
VAR
  M, M1, M2, M3: TMatrix;
BEGIN
  M1 := CreateTranslationMatrix(VectorNegate(Origin));
  M2 := CreateRotationMatrix(Axis, Angle * PI / 180);
  M3 := CreateTranslationMatrix(Origin);
  M := MatrixMultiply(M1, M2);
  M := MatrixMultiply(M, M3);
  anObject.Matrix := MatrixMultiply(anObject.Matrix, M);

  //Just a workarround to update angles...
  anObject.Roll(0);
  anObject.Pitch(0);
  anObject.Turn(0);
END;

CONSTRUCTOR TGLManipHelper.Create(AOwner: TComponent);
BEGIN
  INHERITED;

  FViewers := TGLSceneViewerCollection.Create(Self);


  IF NOT (csDesigning IN ComponentState) THEN
    BEGIN
      FEnabled := True;
      FSelected := TObjectList.Create;
      FSelected.OwnsObjects := False;

      BoundingBoxList := TObjectList.Create;
      BoundingBoxList.OwnsObjects := True;

      GizmoDummyCube := TGLDummyCube.Create(Self);
      GizmoDummyCube.Visible := False;
      GizmoDummyCube.Tag := INVISIBLE_OBJECTS_TAG;

      GizmoX := TGLGizmoArrow.Create(Self);
      GizmoX.GizmoType := gtAxisX;
      GizmoX.Name := 'GizmoX';
      GizmoX.Height := 0.5;
      GizmoX.Tag := INVISIBLE_OBJECTS_TAG;

      GizmoDummyCube.AddChild(GizmoX);

      GizmoY := TGLGizmoArrow.Create(Self);
      GizmoY.GizmoType := gtAxisY;
      GizmoY.Name := 'GizmoY';
      GizmoY.Height := 0.5;
      GizmoY.Tag := INVISIBLE_OBJECTS_TAG;

      GizmoDummyCube.AddChild(GizmoY);

      GizmoZ := TGLGizmoArrow.Create(Self);
      GizmoZ.GizmoType := gtAxisZ;
      GizmoZ.Name := 'GizmoZ';
      GizmoZ.Height := 0.5;
      GizmoZ.Tag := INVISIBLE_OBJECTS_TAG;

      GizmoDummyCube.AddChild(GizmoZ);

      GizmoCornerXY := TGLGizmoCorner.Create(Self);
      GizmoCornerXY.GizmoType := gtPlaneXY;
      GizmoCornerXY.Name := 'GizmoXY';
      GizmoCornerXY.Height := 0.2;
      GizmoCornerXY.Distance := 0.5;
      GizmoCornerXY.Tag := INVISIBLE_OBJECTS_TAG;

      GizmoDummyCube.AddChild(GizmoCornerXY);

      GizmoCornerXZ := TGLGizmoCorner.Create(Self);
      GizmoCornerXZ.GizmoType := gtPlaneXZ;
      GizmoCornerXZ.Name := 'GizmoXZ';
      GizmoCornerXZ.Height := 0.2;
      GizmoCornerXZ.Distance := 0.5;
      GizmoCornerXZ.Tag := INVISIBLE_OBJECTS_TAG;

      GizmoDummyCube.AddChild(GizmoCornerXZ);

      GizmoCornerYZ := TGLGizmoCorner.Create(Self);
      GizmoCornerYZ.GizmoType := gtPlaneYZ;
      GizmoCornerYZ.Name := 'GizmoYZ';
      GizmoCornerYZ.Height := 0.2;
      GizmoCornerYZ.Distance := 0.5;
      GizmoCornerYZ.Tag := INVISIBLE_OBJECTS_TAG;

      GizmoDummyCube.AddChild(GizmoCornerYZ);

    END;
  FRotationSpeed := -0.25;
  FSensitivity := 1;
END;

DESTRUCTOR TGLManipHelper.Destroy;
BEGIN
  FViewers.Free;

  IF NOT (csDesigning IN ComponentState) THEN
    BEGIN
      IF Assigned(FScene) THEN
        BEGIN
          FScene.Objects.Remove(GizmoDummyCube, false);
        END;

      GizmoDummyCube.Free;

      FSelected.Free;
      BoundingBoxList.Free;

    END;

  FScene := NIL;

  INHERITED;
END;

PROCEDURE TGLManipHelper.SetMovingAxis(CONST Value: TMovingAxis);
BEGIN
  FMovingAxis := Value;
END;

PROCEDURE TGLManipHelper.SetScene(CONST Value: TGLScene);
BEGIN
  FScene := Value;

  IF csDesigning IN ComponentState THEN
    Exit;

  IF Assigned(FScene) THEN
    BEGIN
      FScene.Objects.AddChild(GizmoDummyCube);
    END;
END;

PROCEDURE TGLManipHelper.UpdateGizmo;
VAR
  Position: TVector;
  Radius: TGLFloat;
BEGIN
  IF FSelected.Count = 0 THEN
    BEGIN
      GizmoDummyCube.Visible := False;
      Exit;
    END;




  FastBall(FSelected, Position, Radius);
  FastBall(FSelected, Position, Radius);

  FSelectionCenter := Position;

//  exit;

  GizmoDummyCube.Visible := True;

  IF FCamera.CameraStyle = csPerspective THEN
    BEGIN
      FGizmoPosition := VectorSubtract(Position, FCamera.AbsolutePosition);
      NormalizeVector(FGizmoPosition);

      ScaleVector(FGizmoPosition, 4);

      FGizmoPosition := VectorAdd(FCamera.AbsolutePosition, FGizmoPosition);
    END
  ELSE
    BEGIN
      FGizmoPosition := Position;
    END;
  IF daAxisX IN FDisplayableAxis THEN
    BEGIN
      GizmoX.SetTransformPosition(FGizmoPosition);
    END
  ELSE
    BEGIN
      GizmoX.SetTransformPosition(FCamera.AbsolutePosition);
    END;
  IF daAxisY IN FDisplayableAxis THEN
    BEGIN
      GizmoY.SetTransformPosition(FGizmoPosition);
    END
  ELSE
    BEGIN
      GizmoY.SetTransformPosition(FCamera.AbsolutePosition);
    END;
  IF daAxisZ IN FDisplayableAxis THEN
    BEGIN
      GizmoZ.SetTransformPosition(FGizmoPosition);
    END
  ELSE
    BEGIN
      GizmoZ.SetTransformPosition(FCamera.AbsolutePosition);
    END;

  IF [daAxisX, daAxisY] <= FDisplayableAxis THEN
    BEGIN
      GizmoCornerXY.SetTransformPosition(FGizmoPosition);
    END
  ELSE
    BEGIN
      GizmoCornerXY.SetTransformPosition(FCamera.AbsolutePosition);
    END;
  IF [daAxisX, daAxisZ] <= FDisplayableAxis THEN
    BEGIN
      GizmoCornerXZ.SetTransformPosition(FGizmoPosition);
    END
  ELSE
    BEGIN
      GizmoCornerXZ.SetTransformPosition(FCamera.AbsolutePosition);
    END;
  IF [daAxisZ, daAxisY] <= FDisplayableAxis THEN
    BEGIN
      GizmoCornerYZ.SetTransformPosition(FGizmoPosition);
    END
  ELSE
    BEGIN
      GizmoCornerYZ.SetTransformPosition(FCamera.AbsolutePosition);
    END;
END;


PROCEDURE TGLManipHelper.SetManipulationType(
  CONST Value: TManipulationType);
BEGIN
  IF FManipulationType <> Value THEN
    BEGIN
      FManipulationType := Value;
    END;
END;


PROCEDURE TGLManipHelper.SetRotationSpeed(CONST Value: Single);
BEGIN
  FRotationSpeed := Value;
END;

PROCEDURE TGLManipHelper.AddObject(AObject: TGLCustomSceneObject);
VAR
  NewBoundingBox: TGLBoundingBox;
  i: Integer;
BEGIN
  IF FSelected.IndexOf(AObject) > -1 THEN
    BEGIN
      FOR i := 0 TO BoundingBoxList.Count - 1 DO
        BEGIN
          IF TGLBoundingBox(BoundingBoxList[i]).AssignedObject = AObject THEN
            BEGIN
              BoundingBoxList.Delete(i);
              break;
            END;
        END;
      FSelected.Delete(FSelected.IndexOf(AObject));
    END
  ELSE
    BEGIN
      FSelected.Add(AObject);
      NewBoundingBox := TGLBoundingBox.Create(Self);
      BoundingBoxList.Add(NewBoundingBox);
      NewBoundingBox.AssignObj(AObject);
      NewBoundingBox.Tag := INVISIBLE_OBJECTS_TAG;
      NewBoundingBox.Visible := True;

      IF Assigned(FScene) THEN
        BEGIN
          FScene.Objects.AddChild(NewBoundingBox);
        END;
    END;
END;

PROCEDURE TGLManipHelper.ClearObjects;
BEGIN
  BoundingBoxList.Clear;
  FSelected.Clear;
  UpdateGizmo;
  UpdateBB;
END;

PROCEDURE TGLManipHelper.SetRotationType(CONST Value: TRotationType);
BEGIN
  FRotationType := Value;
END;

PROCEDURE TGLManipHelper.UpdateBB;
VAR
  i: Integer;
BEGIN
  FOR i := 0 TO BoundingboxList.COunt - 1 DO
    TGLBoundingBox(BoundingboxList[i]).UpdatePosition
END;

PROCEDURE TGLManipHelper.SetSensitivity(CONST Value: Integer);
BEGIN
  FSensitivity := Value;
END;

PROCEDURE TGLManipHelper.DoSelectionChange;
BEGIN
  IF Assigned(OnSelectionChange) THEN
    OnSelectionChange(Self);
END;

PROCEDURE TGLManipHelper.SetCameraPosition(CONST Value: TVector);
BEGIN
  FCameraPosition := Value;
END;

PROCEDURE TGLManipHelper.SetCamera(CONST Value: TGLCamera);
BEGIN
  IF FCamera <> Value THEN
    BEGIN
      FCamera := Value;
      UpdateGizmo;
    END;
END;

PROCEDURE TGLManipHelper.SetDisplayableAxis(CONST Value: TDisplayableAxis);
BEGIN
  FDisplayableAxis := Value;
END;

PROCEDURE TGLManipHelper.SetViewers(CONST Value: TGLSceneViewerCollection);
BEGIN
  FViewers.Assign(Value);
END;

PROCEDURE TGLManipHelper.SetActiveIndex(CONST Value: Integer);
BEGIN
  FActiveIndex := Value;
END;

PROCEDURE TGLManipHelper.DoRectSelection(X, Y: Integer);
BEGIN
  //
END;

PROCEDURE TGLManipHelper.DrawSelectionRect;
BEGIN

END;

FUNCTION TGLManipHelper.GetPickedObject(x, y, Index,
  Sensitivity: Integer): TGLBaseSceneObject;
BEGIN
  Result := nil;
END;

PROCEDURE TGLManipHelper.StartRectSelection(X, Y: Integer);
BEGIN

END;


PROCEDURE TGLManipHelper.RotateSelected(DeltaX, DeltaY: Integer);
VAR
  i: Integer;
  Center: TVector;
BEGIN

  FOR i := 0 TO FSelected.Count - 1 DO
    BEGIN
      IF RotationType = rtGizmo THEN
        Center := FSelectionCenter
      ELSE
        Center := (FSelected[i] AS TGLBaseSceneObject).Position.AsVector;

      CASE MovingAxis OF
        maAxisX: BEGIN
            RotateAroundArbitraryAxis(FSelected[i] AS TGLBaseSceneObject, XVector, AffineVectorMake(Center), DeltaY * FRotationSpeed);
          END;
        maAxisY: BEGIN
            RotateAroundArbitraryAxis(FSelected[i] AS TGLBaseSceneObject, YVector, AffineVectorMake(Center), DeltaY * FRotationSpeed);
          END;
        maAxisZ: BEGIN
            RotateAroundArbitraryAxis(FSelected[i] AS TGLBaseSceneObject, ZVector, AffineVectorMake(Center), DeltaY * FRotationSpeed);
          END;
        maAxisXY: BEGIN
            RotateAroundArbitraryAxis(FSelected[i] AS TGLBaseSceneObject, XVector, AffineVectorMake(Center), DeltaY * FRotationSpeed);
            RotateAroundArbitraryAxis(FSelected[i] AS TGLBaseSceneObject, YVector, AffineVectorMake(Center), DeltaX * FRotationSpeed);
          END;
        maAxisXZ: BEGIN
            RotateAroundArbitraryAxis(FSelected[i] AS TGLBaseSceneObject, XVector, AffineVectorMake(Center), DeltaY * FRotationSpeed);
            RotateAroundArbitraryAxis(FSelected[i] AS TGLBaseSceneObject, ZVector, AffineVectorMake(Center), DeltaX * FRotationSpeed);
          END;
        maAxisYZ: BEGIN
            RotateAroundArbitraryAxis(FSelected[i] AS TGLBaseSceneObject, YVector, AffineVectorMake(Center), DeltaY * FRotationSpeed);
            RotateAroundArbitraryAxis(FSelected[i] AS TGLBaseSceneObject, ZVector, AffineVectorMake(Center), DeltaX * FRotationSpeed);
          END;
      END;
    END;
  UpdateBB;
END;

FUNCTION TGLManipHelper.TranslateSelected(oldPos, newPos: TVector): Boolean;
VAR
  vec1, vec2: TVector;
  i: Integer;
BEGIN
  Result := True;
//  newPos := MouseWorldPos(x, y);
  vec1 := newPos;
  vec2 := oldPos;

  CASE MovingAxis OF
    maAxisX: BEGIN
        vec1[1] := 0;
        vec1[2] := 0;
        vec1[3] := 0;
        vec2[1] := 0;
        vec2[2] := 0;
        vec2[3] := 0;
      END;
    maAxisY: BEGIN
        vec1[0] := 0;
        vec1[2] := 0;
        vec1[3] := 0;
        vec2[0] := 0;
        vec2[2] := 0;
        vec2[3] := 0;
      END;
    maAxisZ: BEGIN
        vec1[0] := 0;
        vec1[1] := 0;
        vec1[3] := 0;
        vec2[0] := 0;
        vec2[1] := 0;
        vec2[3] := 0;
      END;
  END;

//  lastMouseWorldPos:=newPos;

  FOR i := 0 TO FSelected.Count - 1 DO
    (FSelected[i] AS TGLBaseSceneObject).Position.Translate(VectorSubtract(vec1, vec2));

  UpdateGizmo;
  UpdateBB;
END;

{ TGLSceneViewerCollection }

FUNCTION TGLSceneViewerCollection.Add: TGLSceneViewerItem;
BEGIN
  Result := TGLSceneViewerItem.Create(Self);
END;

CONSTRUCTOR TGLSceneViewerCollection.Create(AOwner: TComponent);
BEGIN
  INHERITED Create(TGLSceneViewerItem);
  FOwner := AOwner;
END;

PROCEDURE TGLSceneViewerCollection.Delete(Index: Integer);
BEGIN
  INHERITED;
END;

DESTRUCTOR TGLSceneViewerCollection.Destroy;
BEGIN

  INHERITED;
END;

FUNCTION TGLSceneViewerCollection.GetItem(
  Index: Integer): TGLSceneViewerItem;
VAR
  ci: TCollectionItem;
BEGIN
  ci := INHERITED GetItem(Index);
  Result := TGLSceneViewerItem(ci);
END;

FUNCTION TGLSceneViewerCollection.GetOwner: TPersistent;
BEGIN
  Result := FOwner;
END;

FUNCTION TGLSceneViewerCollection.Insert(
  Index: Integer): TGLSceneViewerItem;
BEGIN
  Result := Add;
  Result.Index := Index;
END;

PROCEDURE TGLSceneViewerCollection.SetItem(Index: Integer;
  Value: TGLSceneViewerItem);
BEGIN
  INHERITED SetItem(Index, Value AS TCollectionItem);
END;

PROCEDURE TGLSceneViewerCollection.Update(Item: TGLSceneViewerItem);
BEGIN
  INHERITED Update(Item AS TCollectionItem);
END;

{ TGLSceneViewerItem }

CONSTRUCTOR TGLSceneViewerItem.Create(Collection: TCollection);
BEGIN
  INHERITED Create(Collection);
  FCanvas := TCanvas.Create;
END;

DESTRUCTOR TGLSceneViewerItem.Destroy;
BEGIN
  FCanvas.Free;
  INHERITED;
END;

FUNCTION TGLSceneViewerItem.GetPickedObject(x, y: Integer; Sensitivity: Integer = 1): TGLBaseSceneObject;
VAR
  pkList: TGLPickList;
  i: Integer;
BEGIN
  ViewerBeforeRender(FViewer);

  Result := NIL;
  pkList := FViewer.Buffer.GetPickedObjects(Classes.Rect(x - Sensitivity, y - Sensitivity, x + Sensitivity, y + Sensitivity));
  TRY
    IF pkList.Count > 0 THEN
      BEGIN
        FOR i := 0 TO pkList.Count - 1 DO
          BEGIN
            IF (Pos('Gizmo', pkList.Hit[i].Name) = 1) THEN
              BEGIN
                Result := pkList.Hit[i];
                Break;
              END;
          END;

        IF Result = NIL THEN
          BEGIN
            i := -1;
            REPEAT
              inc(i);
            UNTIL (i = pkList.Count) OR (pkList.Hit[i].Tag <> INVISIBLE_OBJECTS_TAG);

            IF i <> pkList.Count THEN
              Result := pkList.Hit[i];
          END;
      END
    ELSE Result := NIL;
  FINALLY
    pkList.Free;
  END;
END;

FUNCTION TGLSceneViewerItem.MouseWorldPos(x, y: Integer): TVector;
VAR
  v: TVector;
BEGIN
  y := FViewer.Height - y;
  IF (Collection.Owner AS TGLManipHelper).Selected.Count <> 0 THEN BEGIN
      IF Viewer.Camera.CameraStyle = csPerspective THEN
        BEGIN
          SetVector(v, x, y, 0);

          CASE (Collection.Owner AS TGLManipHelper).MovingAxis OF
            maAxisX: BEGIN
                FViewer.Buffer.ScreenVectorIntersectWithPlaneXZ(v, (Collection.Owner AS TGLManipHelper).SelectionCenter[1], Result);
              END;
            maAxisY: BEGIN
                FViewer.Buffer.ScreenVectorIntersectWithPlaneYZ(v, (Collection.Owner AS TGLManipHelper).SelectionCenter[0], Result);
              END;
            maAxisZ: BEGIN
                FViewer.Buffer.ScreenVectorIntersectWithPlaneYZ(v, (Collection.Owner AS TGLManipHelper).SelectionCenter[0], Result);
              END;
            maAxisXY: BEGIN
                FViewer.Buffer.ScreenVectorIntersectWithPlaneXY(v, (Collection.Owner AS TGLManipHelper).SelectionCenter[2], Result);
              END;
            maAxisXZ: BEGIN
                FViewer.Buffer.ScreenVectorIntersectWithPlaneXZ(v, (Collection.Owner AS TGLManipHelper).SelectionCenter[1], Result);
              END;
            maAxisYZ: BEGIN
                FViewer.Buffer.ScreenVectorIntersectWithPlaneYZ(v, (Collection.Owner AS TGLManipHelper).SelectionCenter[0], Result);
              END;
          END;
        END
      ELSE
        BEGIN
          Result := VectorMake(Viewer.Buffer.OrthoScreenToWorld(x, y));
          if DisplayableAxis = [daAxisZ, daAxisY] then
            Result[2] := -Result[2];
          Result[0] := -Result[0];
        END;
    END
  ELSE
    SetVector(Result, NullVector);
END;

PROCEDURE TGLSceneViewerItem.SetDisplayableAxis(
  CONST Value: TDisplayableAxis);
BEGIN
  IF FDisplayableAxis <> Value THEN
    BEGIN
      FDisplayableAxis := Value;
    END;
END;

PROCEDURE TGLSceneViewerItem.SetViewer(CONST Value: TGLSceneViewer);
BEGIN
  FViewer := Value;

  IF csDesigning IN (Collection.Owner AS TGLManipHelper).ComponentState THEN
    Exit;

  IF Assigned(FViewer) THEN
    BEGIN
      FMouseDown := FViewer.OnMouseDown;
      FMouseMove := FViewer.OnMouseMove;
      FMouseUp := FViewer.OnMouseUp;
      FBeforeRender := FViewer.BeforeRender;

      FViewer.OnMouseDown := ViewerMouseDown;
      FViewer.OnMouseUp := ViewerMouseUp;
      FViewer.OnMouseMove := ViewerMouseMove;
      FViewer.BeforeRender := ViewerBeforeRender;
    END;
END;

PROCEDURE TGLSceneViewerItem.ViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
BEGIN
  IF csDesigning IN (Collection.Owner AS TGLManipHelper).ComponentState THEN
    Exit;

  IF Assigned(FMouseDown) THEN
    FMouseDown(Sender, Button, Shift, X, Y);

  mx := x; my := y;
  LastCamWorldPos := VectorMake(Viewer.Buffer.OrthoScreenToWorld(x, y));

  IF ssLeft IN Shift THEN
    BEGIN
      pick := (GetPickedObject(x, y, (Collection.Owner AS TGLManipHelper).Sensitivity) AS TGLCustomSceneObject);

      IF Assigned(Pick) THEN
        BEGIN
          IF Pick.Name = 'GizmoX' THEN
            BEGIN
              (Collection.Owner AS TGLManipHelper).MovingAxis := maAxisX;
            END
          ELSE IF Pick.Name = 'GizmoY' THEN
            BEGIN
              (Collection.Owner AS TGLManipHelper).MovingAxis := maAxisY;
            END
          ELSE IF Pick.Name = 'GizmoZ' THEN
            BEGIN
              (Collection.Owner AS TGLManipHelper).MovingAxis := maAxisZ;
            END
          ELSE IF Pick.Name = 'GizmoXY' THEN
            BEGIN
              (Collection.Owner AS TGLManipHelper).MovingAxis := maAxisXY;
            END
          ELSE IF Pick.Name = 'GizmoXZ' THEN
            BEGIN
              (Collection.Owner AS TGLManipHelper).MovingAxis := maAxisXZ;
            END
          ELSE IF Pick.Name = 'GizmoYZ' THEN
            BEGIN
              (Collection.Owner AS TGLManipHelper).MovingAxis := maAxisYZ;
            END
          ELSE IF Pick.Tag <> INVISIBLE_OBJECTS_TAG THEN
            BEGIN
              FRectSelection := False;

              IF NOT (ssCtrl IN Shift) THEN
                BEGIN
                  (Collection.Owner AS TGLManipHelper).ClearObjects;
                END;

              (Collection.Owner AS TGLManipHelper).AddObject(Pick);

              IF Viewer.Camera.CameraStyle = csOrthogonal THEN
                (Collection.Owner AS TGLManipHelper).MovingAxis := maAxisXY
              ELSE
                (Collection.Owner AS TGLManipHelper).MovingAxis := maNone;
              (Collection.Owner AS TGLManipHelper).UpdateGizmo;
              (Collection.Owner AS TGLManipHelper).DoSelectionChange;
            END
          ELSE
            BEGIN
              (Collection.Owner AS TGLManipHelper).ClearObjects;
              (Collection.Owner AS TGLManipHelper).DoSelectionChange;
              StartRectSelection(X, Y);
            END;

          lastMouseWorldPos := MouseWorldPos(x, y);
          diffPos := VectorSubtract(MouseWorldPos(x, y), (Collection.Owner AS TGLManipHelper).SelectionCenter);

        END
      ELSE
        BEGIN
          (Collection.Owner AS TGLManipHelper).ClearObjects;
          (Collection.Owner AS TGLManipHelper).UpdateGizmo;
          (Collection.Owner AS TGLManipHelper).DoSelectionChange;
          StartRectSelection(X, Y);
        END;
    END;
END;

PROCEDURE TGLSceneViewerItem.ViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
VAR
    newPos: TVector;
  CursorPick: TGLCustomSceneObject;
  oldPos: TAffineVector;
BEGIN
  IF csDesigning IN (Collection.Owner AS TGLManipHelper).ComponentState THEN
    Exit;

  IF Assigned(FMouseMove) THEN
    FMouseMove(Sender, Shift, X, Y);
  IF Shift = [ssRight] THEN
    BEGIN
      IF (Collection.Owner AS TGLManipHelper).FEnabled THEN
        BEGIN
          IF FViewer.Camera.CameraStyle = csPerspective THEN
            FViewer.Camera.MoveAroundTarget(my - y, mx - x)
          ELSE IF FViewer.Camera.CameraStyle = csOrthogonal THEN
            BEGIN
              OldPos := FViewer.Camera.Position.AsAffineVector;
              FViewer.Camera.Position.Translate(VectorSubtract(LastCamworldPos, VectorMake(Viewer.Buffer.OrthoScreenToWorld(x, y))));
              LastCamWorldPos := VectorMake(Viewer.Buffer.OrthoScreenToWorld(x, y));
              CASE (Collection.Owner AS TGLManipHelper).MovingAxis OF
                maAxisXY:
                  BEGIN
                    FViewer.Camera.Position.Z := OldPos[2];
                  END;
                maAxisXZ:
                  BEGIN
                    FViewer.Camera.Position.Y := OldPos[1];
                  END;
                maAxisYZ:
                  BEGIN
                    FViewer.Camera.Position.X := OldPos[0];
                  END;

              END;
            END;
        END;
      mx := x; my := y;
      (Collection.Owner AS TGLManipHelper).UpdateGizmo;
    END
  ELSE IF Shift = [ssLeft] THEN
    BEGIN
      IF FRectSelection THEN
        BEGIN
          DoRectSelection(X, Y);
          exit;
        END;
    END;

  IF (Shift = [ssLeft]) AND ((Collection.Owner AS TGLManipHelper).Selected.Count <> 0) AND ((Collection.Owner AS TGLManipHelper).MovingAxis <> maNone) THEN
    BEGIN
      IF (Collection.Owner AS TGLManipHelper).ManipulationType = mtMove THEN
        BEGIN
          newPos := MouseWorldPos(x, y);
          IF (Collection.Owner AS TGLManipHelper).TranslateSelected({lastMouseWorldPos}VectorAdd((Collection.Owner AS TGLManipHelper).SelectionCenter, diffPos), newPos) THEN
            BEGIN
              lastMouseWorldPos := newPos;
            END;
          IF Assigned((Collection.Owner AS TGLManipHelper).FOnMoving) THEN
            (Collection.Owner AS TGLManipHelper).FOnMoving(self);
        END
      ELSE
        BEGIN
          (Collection.Owner AS TGLManipHelper).RotateSelected(mx - x, my - y);
          IF Assigned((Collection.Owner AS TGLManipHelper).FOnMoving) THEN
            (Collection.Owner AS TGLManipHelper).FOnMoving(self);
        END;
      {$IFDEF WIN32}
      IF Mouse.CursorPos.Y = Screen.Height - 1 THEN
        BEGIN
          SetCursorPos(Mouse.CursorPos.X, 1);
          my := y - Screen.Height + 1;
        END
      ELSE IF Mouse.CursorPos.Y = 0 THEN
        BEGIN
          SetCursorPos(Mouse.CursorPos.X, Screen.Height - 2);
          my := y + Screen.Height - 1;
        END
      ELSE
      {$ENDIF}
        BEGIN
          my := y;
        END;

      {$IFDEF WIN32}
      IF Mouse.CursorPos.X = Screen.Width - 1 THEN
        BEGIN
          SetCursorPos(0, Mouse.CursorPos.Y);
          mx := x - Screen.Width;
        END
      ELSE IF Mouse.CursorPos.X = 0 THEN
        BEGIN
          SetCursorPos(Screen.Width - 1, Mouse.CursorPos.Y);
          mx := x + Screen.Width;
        END
      ELSE
      {$ENDIF}
        BEGIN
          mx := x;
        END;
    END
  ELSE IF (Shift = []) THEN
    BEGIN
      CursorPick := (GetPickedObject(x, y, (Collection.Owner AS TGLManipHelper).Sensitivity) AS TGLCustomSceneObject);
      IF OldCursorPick <> CursorPick THEN
        BEGIN
          IF (CursorPick <> NIL) AND (Pos('Gizmo', CursorPick.Name) = 1) THEN
            BEGIN
              IF (Collection.Owner AS TGLManipHelper).ManipulationType = mtMove THEN
                Screen.Cursor := MOVE_CURSOR
              ELSE
                Screen.Cursor := ROTATE_CURSOR;


              IF CursorPick IS TGLGizmoArrow THEN
                (CursorPick AS TGLGizmoArrow).Selected := True;

              IF CursorPick IS TGLGizmoCorner THEN
                BEGIN
                  (CursorPick AS TGLGizmoCorner).Selected := True;
                  IF CursorPick.Name = 'GizmoXY' THEN
                    BEGIN
                      (Collection.Owner AS TGLManipHelper).GizmoX.Selected := True;
                      (Collection.Owner AS TGLManipHelper).GizmoY.Selected := True;
                    END
                  ELSE IF CursorPick.Name = 'GizmoXZ' THEN
                    BEGIN
                      (Collection.Owner AS TGLManipHelper).GizmoX.Selected := True;
                      (Collection.Owner AS TGLManipHelper).GizmoZ.Selected := True;
                    END
                  ELSE IF CursorPick.Name = 'GizmoYZ' THEN
                    BEGIN
                      (Collection.Owner AS TGLManipHelper).GizmoY.Selected := True;
                      (Collection.Owner AS TGLManipHelper).GizmoZ.Selected := True;
                    END;
                END;
            END
          ELSE IF Assigned(CursorPick) AND (CursorPick.Tag <> INVISIBLE_OBJECTS_TAG) THEN
            BEGIN
              Screen.Cursor := POINT_TO_CURSOR;
            END
          ELSE
            BEGIN
              Screen.Cursor := crDefault;
            END;

          IF (OldCursorPick IS TGLGizmoArrow) THEN
            (OldCursorPick AS TGLGizmoArrow).Selected := False;

          IF (OldCursorPick IS TGLGizmoCorner) THEN
            BEGIN
              (OldCursorPick AS TGLGizmoCorner).Selected := False;
              IF OldCursorPick.Name = 'GizmoXY' THEN
                BEGIN
                  (Collection.Owner AS TGLManipHelper).GizmoX.Selected := False;
                  (Collection.Owner AS TGLManipHelper).GizmoY.Selected := False;
                END
              ELSE IF OldCursorPick.Name = 'GizmoXZ' THEN
                BEGIN
                  (Collection.Owner AS TGLManipHelper).GizmoX.Selected := False;
                  (Collection.Owner AS TGLManipHelper).GizmoZ.Selected := False;
                END
              ELSE IF OldCursorPick.Name = 'GizmoYZ' THEN
                BEGIN
                  (Collection.Owner AS TGLManipHelper).GizmoY.Selected := False;
                  (Collection.Owner AS TGLManipHelper).GizmoZ.Selected := False;
                END;
            END;

          OldCursorPick := CursorPick;
        END;
    END;
END;

PROCEDURE TGLSceneViewerItem.ViewerMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
BEGIN
  IF csDesigning IN (Collection.Owner AS TGLManipHelper).ComponentState THEN
    Exit;

  IF Assigned(FMouseUp) THEN
    FMouseUp(Sender, Button, Shift, X, Y);

  pick := NIL;
//  Screen.Cursor := crDefault;

  IF FRectSelection THEN
    BEGIN
      DrawSelectionRect;
      (Collection.Owner AS TGLManipHelper).Selected.Clear;

      IF (FSelRect.Right - FSelRect.Left <> 0) OR
        (FSelRect.Bottom - FSelRect.Top <> 0) THEN
        BEGIN
      //picking list bug if rectangle height or width = 0 then
      //selection are all objects
      //Expading the rectangle to be atl least 1 pixel with makes
      //selection correct.

          IF FSelRect.Right - FSelRect.Left = 0 THEN
            inc(FSelRect.Right);

          IF FSelRect.Bottom - FSelRect.Top = 0 THEN
            inc(FSelRect.Bottom);

          GetRectanglePickedObject(FSelRect);
        END;


      (Collection.Owner AS TGLManipHelper).UpdateGizmo;
      FRectSelection := False;
    END;
END;


PROCEDURE TGLSceneViewerItem.ViewerBeforeRender(Sender: TObject);
BEGIN
  IF Assigned(FBeforeRender) THEN
    FBeforeRender(Sender);

  (Collection.Owner AS TGLManipHelper).DisplayableAxis := FDisplayableAxis;
  (Collection.Owner AS TGLManipHelper).Camera := FViewer.Camera;
END;


PROCEDURE TGLSceneViewerItem.DrawSelectionRect;
VAR
  SR: TRect;
BEGIN
  IF NOT Assigned(FViewer) THEN
    Exit;
  {$IFDEF WIN32}
  WITH FCanvas DO
    BEGIN
      Handle := GetDCEx(FViewer.Handle, 0, DCX_CACHE OR DCX_CLIPSIBLINGS);
      TRY
        WITH Pen DO
          BEGIN
            Style := psSolid;
            Mode := pmXor;
            Width := 1;
            Style := psDot;
            Color := clWhite;
          END;
        Brush.Style := bsClear;
        SR := FSelRect;
        MapWindowPoints(0, Handle, SR, 2);
        WITH SR DO Rectangle(Left + 1, Top + 1, Right, Bottom);
      FINALLY
        ReleaseDC(0, FCanvas.Handle);
        FCanvas.Handle := 0;
      END;
    END;
  {$ENDIF}
END;


PROCEDURE TGLSceneViewerItem.StartRectSelection(X, Y: Integer);
BEGIN
  FRectSelection := True;
  FSelRect := Classes.Rect(X, Y, X, Y);
END;

PROCEDURE TGLSceneViewerItem.DoRectSelection(X, Y: Integer);
BEGIN
  DrawSelectionRect;
  FSelRect.BottomRight.X := X;
  FSelRect.BottomRight.Y := Y;
  DrawSelectionRect;
END;

PROCEDURE TGLSceneViewerItem.GetRectanglePickedObject(ARect: TRect);
VAR
  pkList: TGLPickList;
  i: Integer;
BEGIN
  pkList := FViewer.Buffer.GetPickedObjects(ARect);
  TRY
    IF pkList.Count > 0 THEN
      BEGIN
        FOR i := 0 TO pkList.Count - 1 DO
          BEGIN
            IF pkList[i].Tag <> INVISIBLE_OBJECTS_TAG THEN
              (Collection.Owner AS TGLManipHelper).AddObject(pkList[i] AS TGLCustomSceneObject);
          END;
      END;
  FINALLY
    pkList.Free;
  END;
END;




INITIALIZATION

FINALIZATION

END.

