//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLCoordinates<p>

   Coordinate related classes.<p>

	<b>History : </b><font size=-1><ul>
      <li>05/10/08 - DanB - Created, from GLMisc.pas
	</ul></font>
}
unit GLCoordinates;

interface

uses Classes, VectorGeometry, VectorTypes, OpenGL1x, BaseClasses, GLCrossPlatform;

{$I GLScene.inc}

type

   // TGLCoordinatesStyle
   //
   {: Identifie le type de donn�es stock�es au sein d'un TGLCustomCoordinates.<p>
      <ul><li>csPoint2D : a simple 2D point (Z=0, W=0)
      <ul><li>csPoint : un point (W=1)
      <li>csVector : un vecteur (W=0)
      <li>csUnknown : aucune contrainte
      </ul> }
   TGLCoordinatesStyle = (csPoint2D, csPoint, csVector, csUnknown);

	// TGLCustomCoordinates
	//
	{: Stores and homogenous vector.<p>
		This class is basicly a container for a TVector, allowing proper use of
		delphi property editors and editing in the IDE. Vector/Coordinates
		manipulation methods are only minimal.<br>
		Handles dynamic default values to save resource file space.<p> }
	TGLCustomCoordinates = class (TGLUpdateAbleObject)
		private
			{ Private Declarations }
			FCoords : TVector;
         FStyle : TGLCoordinatesStyle; // NOT Persistent
         FPDefaultCoords : PVector;
			procedure SetAsPoint2D(const Value : TVector2f);
			procedure SetAsVector(const value : TVector);
			procedure SetAsAffineVector(const value : TAffineVector);
      function GetAsAffineVector : TAffineVector;
      function GetAsPoint2D : TVector2f;
			procedure SetCoordinate(index : Integer; const aValue : TGLFloat);
      function GetAsString : String;

		protected
			{ Protected Declarations }
         procedure SetDirectVector(const v : TVector);

			procedure DefineProperties(Filer: TFiler); override;
			procedure ReadData(Stream: TStream);
			procedure WriteData(Stream: TStream);

		public
			{ Public Declarations }
         constructor CreateInitialized(aOwner : TPersistent; const aValue : TVector;
                                       const aStyle : TGLCoordinatesStyle = csUnknown);
         destructor Destroy; override;
			procedure Assign(Source: TPersistent); override;
         procedure WriteToFiler(writer : TWriter);
         procedure ReadFromFiler(reader : TReader);

         procedure Initialize(const value : TVector);
			procedure NotifyChange(Sender : TObject); override;

         {: Identifies the coordinates styles.<p>
            The property is NOT persistent, csUnknown by default, and should be
            managed by owner object only (internally).<p>
            It is used by the TGLCustomCoordinates for internal "assertion" checks
            to detect "misuses" or "misunderstandings" of what the homogeneous
            coordinates system implies. }
         property Style : TGLCoordinatesStyle read FStyle write FStyle;

			procedure Translate(const translationVector : TVector); overload;
			procedure Translate(const translationVector : TAffineVector); overload;
			procedure AddScaledVector(const factor : Single; const translationVector : TVector); overload;
			procedure AddScaledVector(const factor : Single; const translationVector : TAffineVector); overload;
         procedure Rotate(const anAxis : TAffineVector; anAngle: Single); overload;
         procedure Rotate(const anAxis : TVector; anAngle: Single); overload;
         procedure Normalize;
         procedure Invert;
         procedure Scale(factor : Single);
         function  VectorLength : TGLFloat;
         function  VectorNorm : TGLFloat;
         function  MaxXYZ : Single;
         function  Equals(const aVector : TVector) : Boolean;

         procedure SetVector(const x, y: Single; z : Single = 0); overload;
         procedure SetVector(const x, y, z, w: Single); overload;
         procedure SetVector(const v : TAffineVector); overload;
         procedure SetVector(const v : TVector); overload;

         procedure SetPoint(const x, y, z : Single); overload;
         procedure SetPoint(const v : TAffineVector); overload;
         procedure SetPoint(const v : TVector); overload;

         procedure SetPoint2D(const x, y: Single); overload;
         procedure SetPoint2D(const v : TAffineVector); overload;
         procedure SetPoint2D(const v : TVector); overload;
         procedure SetPoint2D(const v : TVector2f); overload;

         procedure SetToZero;
         function AsAddress : PGLFloat;

         {: The coordinates viewed as a vector.<p>
            Assigning a value to this property will trigger notification events,
            if you don't want so, use DirectVector instead. }
			property AsVector : TVector read FCoords write SetAsVector;

         {: The coordinates viewed as an affine vector.<p>
            Assigning a value to this property will trigger notification events,
            if you don't want so, use DirectVector instead.<br>
            The W component is automatically adjustes depending on style. }
			property AsAffineVector : TAffineVector read GetAsAffineVector write SetAsAffineVector;

         {: The coordinates viewed as a 2D point.<p>
            Assigning a value to this property will trigger notification events,
            if you don't want so, use DirectVector instead. }
			property AsPoint2D : TVector2f read GetAsPoint2D write SetAsPoint2D;

      property X: TGLFloat index 0 read FCoords[0] write SetCoordinate;
      property Y: TGLFloat index 1 read FCoords[1] write SetCoordinate;
      property Z: TGLFloat index 2 read FCoords[2] write SetCoordinate;
			property W: TGLFloat index 3 read FCoords[3] write SetCoordinate;

         {: The coordinates, in-between brackets, separated by semi-colons. }
         property AsString : String read GetAsString;

         //: Similar to AsVector but does not trigger notification events
         property DirectVector : TVector read FCoords write SetDirectVector;
         property DirectX : TGLFloat read FCoords[0] write FCoords[0];
         property DirectY : TGLFloat read FCoords[1] write FCoords[1];
         property DirectZ : TGLFloat read FCoords[2] write FCoords[2];
         property DirectW : TGLFloat read FCoords[3] write FCoords[3];
  	end;

   {: A TGLCustomCoordinates that publishes X, Y properties. }
    TGLCoordinates2 = class(TGLCustomCoordinates)
    published
      property X stored False;
      property Y stored False;
  	end;

   {: A TGLCustomCoordinates that publishes X, Y, Z properties. }
    TGLCoordinates3 = class(TGLCustomCoordinates)
    published
      property X stored False;
      property Y stored False;
      property Z stored False;
  	end;

   // TGLCoordinates4
   //
   {: A TGLCustomCoordinates that publishes X, Y, Z, W properties. }
	  TGLCoordinates4 = class (TGLCustomCoordinates)
    published
      property X stored False;
      property Y stored False;
      property Z stored False;
      property W stored False;
    end;

   // TGLCoordinates
   //
    TGLCoordinates = TGLCoordinates3;


    //Actually Sender should be TGLCustomCoordinates, but that would require
    //changes in a some other GLScene units and some other projects that use
    //TGLCoordinatesUpdateAbleComponent
    IGLCoordinatesUpdateAble = interface(IInterface)
    ['{ACB98D20-8905-43A7-AFA5-225CF5FA6FF5}']
      procedure CoordinateChanged(Sender: TGLCustomCoordinates);
    end;


   // TGLCoordinatesUpdateAbleComponent
   //
   TGLCoordinatesUpdateAbleComponent = class (TGLUpdateAbleComponent, IGLCoordinatesUpdateAble)
      public
	      { Public Declarations }
         procedure CoordinateChanged(Sender: TGLCustomCoordinates); virtual; abstract;
   end;

var
   // Specifies if TGLCustomCoordinates should allocate memory for
   // their default values (ie. design-time) or not (run-time)
   vUseDefaultCoordinateSets : Boolean = False;

implementation

uses SysUtils;

const
  csVectorHelp =   'If you are getting assertions here, consider using the SetPoint procedure';
  csPointHelp  =   'If you are getting assertions here, consider using the SetVector procedure';
  csPoint2DHelp =  'If you are getting assertions here, consider using one of the SetVector or SetPoint procedures';

  // ------------------
// ------------------ TGLCustomCoordinates ------------------
// ------------------

// CreateInitialized
//
constructor TGLCustomCoordinates.CreateInitialized(aOwner : TPersistent; const aValue : TVector;
                                             const aStyle : TGLCoordinatesStyle = csUnknown);
begin
   Create(aOwner);
   Initialize(aValue);
   FStyle:=aStyle;
end;

// Destroy
//
destructor TGLCustomCoordinates.Destroy;
begin
   if Assigned(FPDefaultCoords) then
      Dispose(FPDefaultCoords);
   inherited;
end;

// Initialize
//
procedure TGLCustomCoordinates.Initialize(const value : TVector);
begin
   FCoords:=value;
   if vUseDefaultCoordinateSets then begin
      if not Assigned(FPDefaultCoords) then
         New(FPDefaultCoords);
      FPDefaultCoords^:=value;
   end;
end;

// Assign
//
procedure TGLCustomCoordinates.Assign(Source: TPersistent);
begin
   if Source is TGLCustomCoordinates then
      FCoords:=TGLCustomCoordinates(Source).FCoords
   else inherited;
end;

// WriteToFiler
//
procedure TGLCustomCoordinates.WriteToFiler(writer : TWriter);
var
   writeCoords : Boolean;
begin
   with writer do begin
      WriteInteger(0); // Archive Version 0
      if vUseDefaultCoordinateSets then
         writeCoords:=not VectorEquals(FPDefaultCoords^, FCoords)
      else writeCoords:=True;
      WriteBoolean(writeCoords);
      if writeCoords then
         Write(FCoords[0], SizeOf(FCoords));
   end;
end;

// ReadFromFiler
//
procedure TGLCustomCoordinates.ReadFromFiler(reader : TReader);
var
   n : Integer;
begin
   with reader do begin
      ReadInteger; // Ignore ArchiveVersion
      if ReadBoolean then begin
         n:=SizeOf(FCoords);
         Assert(n=4*SizeOf(Single));
         Read(FCoords[0], n);
      end else if Assigned(FPDefaultCoords) then
         FCoords:=FPDefaultCoords^;
   end;
end;

// DefineProperties
//
procedure TGLCustomCoordinates.DefineProperties(Filer: TFiler);
begin
	inherited;
	Filer.DefineBinaryProperty('Coordinates', ReadData, WriteData,
                              not (Assigned(FPDefaultCoords) and VectorEquals(FPDefaultCoords^, FCoords)));
end;

// ReadData
//
procedure TGLCustomCoordinates.ReadData(Stream: TStream);
begin
	Stream.Read(FCoords, SizeOf(FCoords));
end;

// WriteData
//
procedure TGLCustomCoordinates.WriteData(Stream: TStream);
begin
	Stream.Write(FCoords, SizeOf(FCoords));
end;

// NotifyChange
//
procedure TGLCustomCoordinates.NotifyChange(Sender : TObject);
var
  Int: IGLCoordinatesUpdateAble;
begin
  if  Supports(Owner, IGLCoordinatesUpdateAble, Int) then
    Int.CoordinateChanged(TGLCoordinates(Self))
  else
    inherited NotifyChange(Sender);
end;

// Translate
//
procedure TGLCustomCoordinates.Translate(const translationVector : TVector);
begin
	FCoords[0]:=FCoords[0]+translationVector[0];
	FCoords[1]:=FCoords[1]+translationVector[1];
	FCoords[2]:=FCoords[2]+translationVector[2];
	NotifyChange(Self);
end;

// Translate
//
procedure TGLCustomCoordinates.Translate(const translationVector : TAffineVector);
begin
	FCoords[0]:=FCoords[0]+translationVector[0];
	FCoords[1]:=FCoords[1]+translationVector[1];
	FCoords[2]:=FCoords[2]+translationVector[2];
	NotifyChange(Self);
end;

// AddScaledVector (hmg)
//
procedure TGLCustomCoordinates.AddScaledVector(const factor : Single; const translationVector : TVector);
var
   f : Single;
begin
   f:=factor;
   CombineVector(FCoords, translationVector, f);
	NotifyChange(Self);
end;

// AddScaledVector (affine)
//
procedure TGLCustomCoordinates.AddScaledVector(const factor : Single; const translationVector : TAffineVector);
var
   f : Single;
begin
   f:=factor;
   CombineVector(FCoords, translationVector, f);
	NotifyChange(Self);
end;

// Rotate (affine)
//
procedure TGLCustomCoordinates.Rotate(const anAxis : TAffineVector; anAngle : Single);
begin
   RotateVector(FCoords, anAxis, anAngle);
   NotifyChange(Self);
end;

// Rotate (hmg)
//
procedure TGLCustomCoordinates.Rotate(const anAxis : TVector; anAngle : Single);
begin
   RotateVector(FCoords, anAxis, anAngle);
   NotifyChange(Self);
end;

// Normalize
//
procedure TGLCustomCoordinates.Normalize;
begin
   NormalizeVector(FCoords);
   NotifyChange(Self);
end;

// Invert
//
procedure TGLCustomCoordinates.Invert;
begin
   NegateVector(FCoords);
   NotifyChange(Self);
end;

// Scale
//
procedure TGLCustomCoordinates.Scale(factor : Single);
begin
   ScaleVector(PAffineVector(@FCoords)^, factor);
   NotifyChange(Self);
end;

// VectorLength
//
function TGLCustomCoordinates.VectorLength : TGLFloat;
begin
   Result:=VectorGeometry.VectorLength(FCoords);
end;

// VectorNorm
//
function TGLCustomCoordinates.VectorNorm : TGLFloat;
begin
   Result:=VectorGeometry.VectorNorm(FCoords);
end;

// MaxXYZ
//
function TGLCustomCoordinates.MaxXYZ : Single;
begin
   Result:=VectorGeometry.MaxXYZComponent(FCoords);
end;

// Equals
//
function TGLCustomCoordinates.Equals(const aVector : TVector) : Boolean;
begin
   Result:=VectorEquals(FCoords, aVector);
end;

// SetVector (affine)
//
procedure TGLCustomCoordinates.SetVector(const x, y: Single; z : Single = 0);
begin
  Assert(FStyle = csVector, csVectorHelp);
  VectorGeometry.SetVector(FCoords, x, y, z);
  NotifyChange(Self);
end;

// SetVector (TAffineVector)
//
procedure TGLCustomCoordinates.SetVector(const v : TAffineVector);
begin
  Assert(FStyle = csVector, csVectorHelp);
  VectorGeometry.SetVector(FCoords, v);
  NotifyChange(Self);
end;

// SetVector (TVector)
//
procedure TGLCustomCoordinates.SetVector(const v : TVector);
begin
  Assert(FStyle = csVector, csVectorHelp);
  VectorGeometry.SetVector(FCoords, v);
  NotifyChange(Self);
end;

// SetVector (hmg)
//
procedure TGLCustomCoordinates.SetVector(const x, y, z, w : Single);
begin
  Assert(FStyle = csVector, csVectorHelp);
  VectorGeometry.SetVector(FCoords, x, y, z, w);
  NotifyChange(Self);
end;

// SetDirectVector
//
procedure TGLCustomCoordinates.SetDirectVector(const v : TVector);
begin
   FCoords[0]:=v[0];
   FCoords[1]:=v[1];
   FCoords[2]:=v[2];
   FCoords[3]:=v[3];
end;

// SetToZero
//
procedure TGLCustomCoordinates.SetToZero;
begin
   FCoords[0]:=0;
   FCoords[1]:=0;
   FCoords[2]:=0;
   if FStyle=csPoint then
      FCoords[3]:=1
   else FCoords[3]:=0;
	NotifyChange(Self);
end;

// SetPoint
//
procedure TGLCustomCoordinates.SetPoint(const x, y, z : Single);
begin
  Assert(FStyle = csPoint, csPointHelp);
  VectorGeometry.MakePoint(FCoords, x, y, z);
  NotifyChange(Self);
end;

// SetPoint (TAffineVector)
//
procedure TGLCustomCoordinates.SetPoint(const v : TAffineVector);
begin
  Assert(FStyle = csPoint, csPointHelp);
  VectorGeometry.MakePoint(FCoords, v);
  NotifyChange(Self);
end;

// SetPoint (TVector)
//
procedure TGLCustomCoordinates.SetPoint(const v : TVector);
begin
  Assert(FStyle = csPoint, csPointHelp);
  VectorGeometry.MakePoint(FCoords, v);
  NotifyChange(Self);
end;

// SetPoint2D
//
procedure TGLCustomCoordinates.SetPoint2D(const x, y : Single);
begin
  Assert(FStyle = csPoint2D, csPoint2DHelp);
  VectorGeometry.MakeVector(FCoords, x, y, 0);
  NotifyChange(Self);
end;

// SetPoint2D (TAffineVector)
//
procedure TGLCustomCoordinates.SetPoint2D(const v : TAffineVector);
begin
  Assert(FStyle = csPoint2D, csPoint2DHelp);
  VectorGeometry.MakeVector(FCoords, v);
  NotifyChange(Self);
end;

// SetPoint2D (TVector)
//
procedure TGLCustomCoordinates.SetPoint2D(const v : TVector);
begin
  Assert(FStyle = csPoint2D, csPoint2DHelp);
  VectorGeometry.MakeVector(FCoords, v);
  NotifyChange(Self);
end;

// SetPoint2D (TVector2f)
//
procedure TGLCustomCoordinates.SetPoint2D(const v : TVector2f);
begin
  Assert(FStyle = csPoint2D, csPoint2DHelp);
  VectorGeometry.MakeVector(FCoords, v[0], v[1], 0);
  NotifyChange(Self);
end;

// AsAddress
//
function TGLCustomCoordinates.AsAddress : PGLFloat;
begin
   Result:=@FCoords;
end;

// SetAsVector
//
procedure TGLCustomCoordinates.SetAsVector(const value: TVector);
begin
   FCoords:=value;
   case FStyle of
      csPoint2D :
      begin
        FCoords[2] := 0;
        FCoords[3] := 0;
      end;
      csPoint :  FCoords[3]:=1;
      csVector : FCoords[3]:=0;
    else
      Assert(False);
   end;
	NotifyChange(Self);
end;

// SetAsAffineVector
//
procedure TGLCustomCoordinates.SetAsAffineVector(const value : TAffineVector);
begin
   case FStyle of
      csPoint2D : MakeVector(FCoords, value);
      csPoint :   MakePoint(FCoords, value);
      csVector:   MakeVector(FCoords, value);
   else
      Assert(False);
   end;
	NotifyChange(Self);
end;

// SetAsPoint2D
//
procedure TGLCustomCoordinates.SetAsPoint2D(const Value : TVector2f);
begin
   case FStyle of
      csPoint2D, csPoint,  csVector:
      begin
        FCoords[0] := Value[0];
        FCoords[1] := Value[1];
        FCoords[2] := 0;
        FCoords[3] := 0;
      end;
   else
      Assert(False);
   end;
	NotifyChange(Self);
end;

// GetAsAffineVector
//
function TGLCustomCoordinates.GetAsAffineVector : TAffineVector;
begin
   VectorGeometry.SetVector(Result, FCoords);
end;

// GetAsPoint2D
//
function TGLCustomCoordinates.GetAsPoint2D : TVector2f;
begin
   Result[0] := FCoords[0];
   Result[1] := FCoords[1];
end;

// SetCoordinate
//
procedure TGLCustomCoordinates.SetCoordinate(index : Integer; const aValue : TGLFloat);
begin
	FCoords[index]:=aValue;
	NotifyChange(Self);
end;

// GetAsString
//
function TGLCustomCoordinates.GetAsString : String;
begin
   case Style of
     csPoint2D: Result := Format('(%g; %g)',         [FCoords[0], FCoords[1]]);
     csPoint:   Result := Format('(%g; %g; %g)',     [FCoords[0], FCoords[1], FCoords[2]]);
     csVector:  Result := Format('(%g; %g; %g; %g)', [FCoords[0], FCoords[1], FCoords[2], FCoords[3]]);
   else
     Assert(False);
   end;
end;

initialization

  RegisterClasses([TGLCoordinates2, TGLCoordinates3, TGLCoordinates4]);

end.
