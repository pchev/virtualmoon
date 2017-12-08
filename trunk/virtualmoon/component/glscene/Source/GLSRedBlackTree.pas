//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  GLScene Red Black Tree

  USAGE
  The TRedBlackTree generic class behaves somewhat like a TList:
  it stores _Value_ by _Key_
  and uses the same comparison function as TList.Sort (TListSortCompare).
  Functions Clear, Add, Delete, First and Last are equivalent,
  except that First and Last return a _Key_ so they
  can be used for comparisons in loops.

  All _Key_ occur only once in the tree if DuplicateKeys if False:
  when the same value is added twice, the second one is not stored.

  When DuplicateKeys is enabled the second comparison function is used
  for sort _Value_ and it duplicates not allowed.

  To be able to manage the tree, the Create constructor has a argument
  specifying the comparison function that should be used.

  The function Find can be used to find a _Value_ that was put in the tree,
  it searches for the given _Key_ using the comparison function given
  at time of object creation.

  The functions NextKey and PrevKey can be used to "walk" through the tree:
  given a _Key_, NextKey replace it with the smallest key that
  is larger than _Key_, PrevKey returns the largest key that is
  smaller than _Key_. For Last and First key result not returned.

    History :  
       05/05/11 - Yar - Fugfixed method Add for Lazarus (unclear node's fields)
       04/12/10 - Yar - Improved duplicate keys storing
       04/08/10 - Yar - Fixed field section for FPC 2.5.1 (Bugtracker ID = 3039424)
       19/04/10 - Yar - Creation (based on grbtree jzombi aka Jani Matyas)
    
}

unit GLSRedBlackTree;

interface

{$I GLScene.inc}

uses
  LCLVersion,
  Classes, GLCrossPlatform;

type

  TRBColor = (clRed, clBlack);

  // TRedBlackTree
  //
{$IFDEF GLS_GENERIC_PREFIX}
  generic
{$ENDIF}
  GRedBlackTree < TKey, TValue > = class
   
  public
    type
      TKeyCompareFunc = function(const Item1, Item2: TKey): Integer;
      TValueCompareFunc = function(const Item1, Item2: TValue): Boolean;
      TForEachProc = procedure (AKey: TKey; AValue: TValue; out AContinue: Boolean);
     
    {$IFDEF GLS_DELPHI_OR_CPPB}
       TRBNode = class
         Key: TKey;
         Left, Right, Parent, Twin: TRBNode;
         Color: TRBColor;
         Value: TValue;
       end;
    {$ELSE}
       TRBNode = ^TRBNodeRec;
       TRBNodeRec = record
         Key: TKey;
         Left, Right, Parent, Twin: TRBNode;
         Color: TRBColor;
         Value: TValue;
       end;
    {$ENDIF}
    var
      FRoot: TRBNode;
      FLeftmost: TRBNode;
      FRightmost: TRBNode;
      FLastFound: TRBNode;
      FLastNode: TRBNode;
      FCount: Integer;
      FKeyCompareFunc: TKeyCompareFunc;
      FDuplicateKeys: Boolean;
      FValueCompareFunc: TValueCompareFunc;
      FOnChange: TNotifyEvent;

    function FindNode(const key: TKey): TRBNode;
    procedure RotateLeft(var x: TRBNode);
    procedure RotateRight(var x: TRBNode);
    function Minimum(var x: TRBNode): TRBNode;
    function Maximum(var x: TRBNode): TRBNode;
    function GetFirst: TKey;
    function GetLast: TKey;
    procedure SetDuplicateKeys(Value: Boolean);
    class procedure FastErase(x: TRBNode);

  public
     
    constructor Create(KeyCompare: TKeyCompareFunc; ValueCompare: TValueCompareFunc);
    destructor Destroy; override;

    procedure Clear;
    { Find value by key. }
    function Find(const key: TKey; out Value: TValue): Boolean;
    function NextKey(var key: TKey; out Value: TValue): Boolean;
    function PrevKey(var key: TKey; out Value: TValue): Boolean;
    function NextDublicate(out Value: TValue): Boolean;
    procedure Add(const key: TKey; const Value: TValue);
    procedure Delete(const key: TKey);
    procedure ForEach(AProc: TForEachProc);
    property Count: Integer read FCount;
    property First: TKey read GetFirst;
    property Last: TKey read GetLast;
    property DuplicateKeys: Boolean read FDuplicateKeys write SetDuplicateKeys;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

function CompareInteger(const Item1, Item2: Integer): Integer;

implementation

function CompareInteger(const Item1, Item2: Integer): Integer;
begin
  if Item1 < Item2 then
  begin
    Result := -1;
  end
  else if (Item1 = Item2) then
  begin
    Result := 0;
  end
  else
  begin
    Result := 1;
  end
end;

constructor GRedBlackTree.Create(KeyCompare: TKeyCompareFunc; ValueCompare: TValueCompareFunc);
begin
  inherited Create;
  Assert(Assigned(KeyCompare));
  FKeyCompareFunc := KeyCompare;
  FValueCompareFunc := ValueCompare;
  FRoot := nil;
  FLeftmost := nil;
  FRightmost := nil;
  FDuplicateKeys := Assigned(ValueCompare);
end;

destructor GRedBlackTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

class procedure GRedBlackTree.FastErase(x: TRBNode);
var
  y: TRBNode;
begin
  if (x.left <> nil) then
    FastErase(x.left);
  if (x.right <> nil) then
    FastErase(x.right);
  repeat
    y := x;
    x := x.Twin;

    Dispose(y);

  until x = nil;
end;

procedure GRedBlackTree.Clear;
begin
  if (FRoot <> nil) then
    FastErase(FRoot);
  FRoot := nil;
  FLeftmost := nil;
  FRightmost := nil;
  FCount := 0;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function GRedBlackTree.Find(const key: TKey; out Value: TValue): Boolean;
begin
  FLastFound := FindNode(key);
  Result := Assigned(FLastFound);
  if Result then
    Value := FLastFound.Value;
end;

function GRedBlackTree.FindNode(const key: TKey): TRBNode;
var
  cmp: integer;
begin
  Result := FRoot;
  while (Result <> nil) do
  begin
    cmp := FKeyCompareFunc(Result.Key, key);
    if cmp < 0 then
    begin
      Result := Result.right;
    end
    else if cmp > 0 then
    begin
      Result := Result.left;
    end
    else
    begin
      break;
    end;
  end;
end;

function GRedBlackTree.NextDublicate(out Value: TValue): Boolean;
begin
  if Assigned(FLastFound) then
  begin
    if Assigned(FLastFound.Twin) then
    begin
      FLastFound := FLastFound.Twin;
      Value := FLastFound.Value;
      exit(True);
    end;
  end;
  Result := False;
end;

procedure GRedBlackTree.RotateLeft(var x: TRBNode);
var
  y: TRBNode;
begin
  y := x.right;
  x.right := y.left;
  if (y.left <> nil) then
  begin
    y.left.parent := x;
  end;
  y.parent := x.parent;
  if (x = FRoot) then
  begin
    FRoot := y;
  end
  else if (x = x.parent.left) then
  begin
    x.parent.left := y;
  end
  else
  begin
    x.parent.right := y;
  end;
  y.left := x;
  x.parent := y;
end;

procedure GRedBlackTree.RotateRight(var x: TRBNode);
var
  y: TRBNode;
begin
  y := x.left;
  x.left := y.right;
  if (y.right <> nil) then
  begin
    y.right.parent := x;
  end;
  y.parent := x.parent;
  if (x = FRoot) then
  begin
    FRoot := y;
  end
  else if (x = x.parent.right) then
  begin
    x.parent.right := y;
  end
  else
  begin
    x.parent.left := y;
  end;
  y.right := x;
  x.parent := y;
end;

function GRedBlackTree.Minimum(var x: TRBNode): TRBNode;
begin
  Result := x;
  while (Result.left <> nil) do
    Result := Result.left;
end;

function GRedBlackTree.Maximum(var x: TRBNode): TRBNode;
begin
  Result := x;
  while (Result.right <> nil) do
    Result := Result.right;
end;

procedure GRedBlackTree.Add(const key: TKey; const Value: TValue);
var
  x, y, z, zpp: TRBNode;
  cmp: Integer;
begin

  New(z);

  { Initialize fields in new node z }
  z.Key := key;
  z.left := nil;
  z.right := nil;
  z.color := clRed;
  z.Value := Value;
  z.Twin := nil;

  { Maintain FLeftmost and FRightmost nodes }
  if ((FLeftmost = nil) or (FKeyCompareFunc(key, FLeftmost.Key) < 0)) then
  begin
    FLeftmost := z;
  end;
  if ((FRightmost = nil) or (FKeyCompareFunc(FRightmost.Key, key) < 0)) then
  begin
    FRightmost := z;
  end;

  { Insert node z }
  y := nil;
  x := FRoot;
  while (x <> nil) do
  begin
    y := x;
    cmp := FKeyCompareFunc(key, x.Key);
    if cmp < 0 then
      x := x.left
    else if cmp > 0 then
      x := x.right
    else
    begin
      { Key already exists in tree. }
      if FDuplicateKeys then
      begin
        { Check twins chain for value dublicate. }
        repeat
          if FValueCompareFunc(Value, x.Value) then
          begin
            y := nil;
            break;
          end;
          y := x;
          x := x.Twin;
        until x = nil;
        if Assigned(y) then
        begin
          { Add dublicate key to end of twins chain. }
          y.Twin := z;
          Inc(FCount);
          if Assigned(FOnChange) then
            FOnChange(Self);
          exit;
        end;
        { Value already exists in tree. }
      end;

      Dispose(z);

      //a jzombi: memory leak: if we don't put it in the tree, we shouldn't hold it in the memory
      exit;
    end;
  end;
  z.parent := y;
  if (y = nil) then
  begin
    FRoot := z;
  end
  else if (FKeyCompareFunc(key, y.Key) < 0) then
  begin
    y.left := z;
  end
  else
  begin
    y.right := z;
  end;

  { Rebalance tree }
  while ((z <> FRoot) and (z.parent.color = clRed)) do
  begin
    zpp := z.parent.parent;
    if (z.parent = zpp.left) then
    begin
      y := zpp.right;
      if ((y <> nil) and (y.color = clRed)) then
      begin
        z.parent.color := clBlack;
        y.color := clBlack;
        zpp.color := clRed;
        z := zpp;
      end
      else
      begin
        if (z = z.parent.right) then
        begin
          z := z.parent;
          rotateLeft(z);
        end;
        z.parent.color := clBlack;
        zpp.color := clRed;
        rotateRight(zpp);
      end;
    end
    else
    begin
      y := zpp.left;
      if ((y <> nil) and (y.color = clRed)) then
      begin
        z.parent.color := clBlack;
        y.color := clBlack;
        zpp.color := clRed; //c jzombi: zpp.color := clRed;
        z := zpp;
      end
      else
      begin
        if (z = z.parent.left) then
        begin
          z := z.parent;
          rotateRight(z);
        end;
        z.parent.color := clBlack;
        zpp.color := clRed; //c jzombi: zpp.color := clRed;
        rotateLeft(zpp);
      end;
    end;
  end;
  FRoot.color := clBlack;
  Inc(FCount);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure GRedBlackTree.Delete(const key: TKey);
var
  w, x, y, z, x_parent: TRBNode;
  tmpcol: TRBColor;
begin
  z := FindNode(key);
  if z = nil then
    exit;

  y := z;
  x := nil;
  x_parent := nil;

  if (y.left = nil) then
  begin { z has at most one non-null child. y = z. }
    x := y.right; { x might be null. }
  end
  else
  begin
    if (y.right = nil) then
    begin { z has exactly one non-null child. y = z. }
      x := y.left; { x is not null. }
    end
    else
    begin
      { z has two non-null children.  Set y to }
      y := y.right; {   z's successor.  x might be null. }
      while (y.left <> nil) do
      begin
        y := y.left;
      end;
      x := y.right;
    end;
  end;

  if (y <> z) then
  begin
    { "copy y's sattelite data into z" }
    { relink y in place of z.  y is z's successor }
    z.left.parent := y;
    y.left := z.left;
    if (y <> z.right) then
    begin
      x_parent := y.parent;
      if (x <> nil) then
      begin
        x.parent := y.parent;
      end;
      y.parent.left := x; { y must be a child of left }
      y.right := z.right;
      z.right.parent := y;
    end
    else
    begin
      x_parent := y;
    end;
    if (FRoot = z) then
    begin
      FRoot := y;
    end
    else if (z.parent.left = z) then
    begin
      z.parent.left := y;
    end
    else
    begin
      z.parent.right := y;
    end;
    y.parent := z.parent;
    tmpcol := y.color;
    y.color := z.color;
    z.color := tmpcol;
    y := z;
    { y now points to node to be actually deleted }
  end
  else
  begin { y = z }
    x_parent := y.parent;
    if (x <> nil) then
    begin
      x.parent := y.parent;
    end;
    if (FRoot = z) then
    begin
      FRoot := x;
    end
    else
    begin
      if (z.parent.left = z) then
      begin
        z.parent.left := x;
      end
      else
      begin
        z.parent.right := x;
      end;
    end;
    if (FLeftmost = z) then
    begin
      if (z.right = nil) then
      begin { z.left must be null also }
        FLeftmost := z.parent;
      end
      else
      begin
        FLeftmost := minimum(x);
      end;
    end;
    if (FRightmost = z) then
    begin
      if (z.left = nil) then
      begin { z.right must be null also }
        FRightmost := z.parent;
      end
      else
      begin { x == z.left }
        FRightmost := maximum(x);
      end;
    end;
  end;

  { Rebalance tree }
  if (y.color = clBlack) then
  begin
    while ((x <> FRoot) and ((x = nil) or (x.color = clBlack))) do
    begin
      if (x = x_parent.left) then
      begin
        w := x_parent.right;
        if (w.color = clRed) then
        begin
          w.color := clBlack;
          x_parent.color := clRed;
          rotateLeft(x_parent);
          w := x_parent.right;
        end;
        if (((w.left = nil) or
          (w.left.color = clBlack)) and
          ((w.right = nil) or
          (w.right.color = clBlack))) then
        begin
          w.color := clRed;
          x := x_parent;
          x_parent := x_parent.parent;
        end
        else
        begin
          if ((w.right = nil) or (w.right.color = clBlack)) then
          begin
            w.left.color := clBlack;
            w.color := clRed;
            rotateRight(w);
            w := x_parent.right;
          end;
          w.color := x_parent.color;
          x_parent.color := clBlack;
          if (w.right <> nil) then
          begin
            w.right.color := clBlack;
          end;
          rotateLeft(x_parent);
          x := FRoot; { break; }
        end
      end
      else
      begin
        { same as above, with right <. left. }
        w := x_parent.left;
        if (w.color = clRed) then
        begin
          w.color := clBlack;
          x_parent.color := clRed;
          rotateRight(x_parent);
          w := x_parent.left;
        end;
        if (((w.right = nil) or
          (w.right.color = clBlack)) and
          ((w.left = nil) or
          (w.left.color = clBlack))) then
        begin
          w.color := clRed;
          x := x_parent;
          x_parent := x_parent.parent;
        end
        else
        begin
          if ((w.left = nil) or (w.left.color = clBlack)) then
          begin
            w.right.color := clBlack;
            w.color := clRed;
            rotateLeft(w);
            w := x_parent.left;
          end;
          w.color := x_parent.color;
          x_parent.color := clBlack;
          if (w.left <> nil) then
          begin
            w.left.color := clBlack;
          end;
          rotateRight(x_parent);
          x := FRoot; { break; }
        end;
      end;
    end;
    if (x <> nil) then
    begin
      x.color := clBlack;
    end;
  end;
  while Assigned(y.Twin) do
  begin
    z := y;
    y := y.Twin;

    Dispose(z);

  end;

  Dispose(y);

  Dec(FCount);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function GRedBlackTree.NextKey(var key: TKey; out Value: TValue): Boolean;
var
  x, y: TRBNode;
begin
  if Assigned(FLastNode) and (FKeyCompareFunc(FLastNode.Key, key) = 0) then
    x := FLastNode
  else
    x := FindNode(key);
  if x = nil then
    exit;
  if (x.right <> nil) then
  begin
    x := x.right;
    while (x.left <> nil) do
    begin
      x := x.left;
    end;
  end
  else if (x.parent <> nil) then
  begin
    y := x.parent;
    while Assigned(y) and (x = y.right) do
    begin
      x := y;
      y := y.parent;
    end;
    if (x.right <> y) then
      x := y;
  end
  else
    x := FRoot;
  if x = nil then
    exit(False);
  key := x.Key;
  FLastNode := x;
  Value := x.Value;
  Result := True;
end;

function GRedBlackTree.PrevKey(var key: TKey; out Value: TValue): Boolean;
var
  x, y: TRBNode;
begin
  if Assigned(FLastNode) and (FKeyCompareFunc(FLastNode.Key, key) = 0) then
    x := FLastNode
  else
    x := FindNode(key);
  if x = nil then
    exit(False);
  if (x.left <> nil) then
  begin
    y := x.left;
    while (y.right <> nil) do
    begin
      y := y.right;
    end;
    x := y;
  end
  else if (x.parent <> nil) then
  begin
    y := x.parent;
    while (x = y.left) do
    begin
      x := y;
      y := y.parent;
    end;
    x := y;
  end
  else
    x := FRoot;
  if x = nil then
    exit(False);
  key := x.Key;
  FLastNode := x;
  Value := x.Value;
  Result := True;
end;

function GRedBlackTree.GetFirst: TKey;
begin
  Result := FLeftMost.Key;
end;

function GRedBlackTree.GetLast: TKey;
begin
  Result := FRightMost.Key;
end;

procedure GRedBlackTree.ForEach(AProc: TForEachProc);
var
  x, y, z: TRBNode;
  cont: Boolean;
begin
  if Assigned(FLeftMost) then
  begin
    x := FLeftMost;
    repeat
      z := x;
      repeat
        AProc(z.Key, z.Value, cont);
        if not cont then
          exit;
        z := z.Twin;
      until z = nil;
      // Next node
      if (x.right <> nil) then
      begin
        x := x.right;
        while (x.left <> nil) do
        begin
          x := x.left;
        end;
      end
      else if (x.parent <> nil) then
      begin
        y := x.parent;
        while (x = y.right) do
        begin
          x := y;
          y := y.parent;
        end;
        if (x.right <> y) then
          x := y;
      end
      else
        x := FRoot;
    until x = FRightMost;
    if cont and (FLeftMost <> FRightMost) then
      AProc(FRightMost.Key, FRightMost.Value, cont);
  end;
end;

procedure GRedBlackTree.SetDuplicateKeys(Value: Boolean);
begin
  if Value and Assigned(FValueCompareFunc) then
    FDuplicateKeys := True
  else
    FDuplicateKeys := False;
end;

end.
