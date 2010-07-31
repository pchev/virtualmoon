//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLPerlin<p>

   Classes for generating perlin noise.<p>

   The components and classes in the unit are a base to generate textures and heightmaps from,
   A Perlin Height Data Source have been included as an example. Use this combined with a terrain renderer for an infinite random landscape <p>

      $Log: glperlin.pas,v $
      Revision 1.1  2006/01/10 20:50:45  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.3  2006/01/09 20:45:50  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/12/04 16:53:05  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.1  2005/12/01 21:24:11  z0m3ie
      *** empty log message ***

      Revision 1.3  2005/08/03 00:41:39  z0m3ie
      - added automatical generated History from CVS

	<b>History : </b><font size=-1><ul>
      <li>31/03/07 - DaStr - Added $I GLScene.inc
      <li>28/03/07 - DaStr - Added explicit pointer dereferencing
                            (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
      <li>08/07/04 - LR - Correction for Linux
      <li>29/01/03 - JaJ - Submitted to GLScene.
	</ul></font>
}
unit GLPerlin;

interface

{$I GLScene.inc}

uses
  Classes, SysUtils, VectorGeometry, GLPerlinBase, GLHeightData, GLCrossPlatform;

Type
  TGLPerlinInterpolation = (pi_none, pi_simple, pi_linear,pi_Smoothed, pi_Cosine, pi_cubic);
  TGLBasePerlinOctav = class
  private
    FAmplitude     : Double;
    FScale         : Double;
    FInterpolation : TGLPerlinInterpolation;
    FSmoothing     : TGLPerlinInterpolation;
  public
    Procedure Generate; dynamic; abstract;
    property  Interpolation : TGLPerlinInterpolation read FInterpolation write FInterpolation;
    property  Smoothing : TGLPerlinInterpolation read FSmoothing write FSmoothing;
    property  Amplitude : Double read FAmplitude write FAmplitude;
    property  Scale : Double read FScale write FScale;
  End;
  TGLPerlinOctav = class of TGLBasePerlinOctav;

  TGLBasePerlin = class(TComponent)
  private
    FPersistence : Double;
    FNumber_Of_Octaves : Integer;
    FOctaves : TList;
    FOctavClass : TGLPerlinOctav;
    FInterpolation : TGLPerlinInterpolation;
    FSmoothing     : TGLPerlinInterpolation;
  protected
    function PerlinNoise_1D(x : Double) : Double;
    function PerlinNoise_2D(x,y : Double) : Double;
    function GetOctave(index : Integer) : TGLBasePerlinOctav;
    Procedure SetPersistence(val : double);
    Procedure Set_Number_Of_Octaves(val : integer);
  public
    Constructor Create(AOwner : TComponent); override;
    Procedure Generate; dynamic; abstract;
    Property Octaves[index : Integer] : TGLBasePerlinOctav read GetOctave;

  published
    property Smoothing : TGLPerlinInterpolation read FSmoothing write FSmoothing;
    property Interpolation : TGLPerlinInterpolation read FInterpolation write FInterpolation;
    property Persistence : Double read FPersistence write SetPersistence;
    property Number_Of_Octaves : Integer read FNumber_Of_Octaves write Set_Number_Of_Octaves;
  End;

  TGL1DPerlin = class(TGLBasePerlin)
    Function GetPerlinValue_1D(X : Double) : Double;
  published
  End;

  TGL2DPerlinOctav = class(TGLBasePerlinOctav)
  public
    Data : T2DPerlinArray;
    Width, Height : Integer;
    XStart,YStart : Integer;
    XStep, YStep  : Integer;
    YRate : Integer;
    Procedure Generate; override;
    Function  GetDataSmoothed(X,Y : Integer) : Double;
    Function  GetData(X,Y : Integer) : Double;

    Function  GetCubic(X,Y : Double) : Double;
    Function  GetCosine(X,Y : Double) : Double;
    Function  GetPerling(X,Y : Double) : Double;
    Procedure Generate_CubicInterpolate;
    Procedure Generate_SmoothInterpolate;
    Procedure Generate_NonInterpolated;
  End;

  TGL2DPerlin = class(TGLBasePerlin)
  private
  public
    Width, Height : Integer;
    XStart,YStart : Integer;
    XStep, YStep  : Integer;
    MaxValue, MinValue : Double;
    Constructor Create(AOwner : TComponent); override;
    Procedure Generate; override;
    Function GetPerlinValue_2D(X,Y : Double) : Double;
    Procedure MakeBitmap(Param : TGLBitmap);
    Procedure SetHeightData(heightData : THeightData);
  End;

  TGLPerlinHDS = class(THeightDataSource)
  private
    FInterpolation : TGLPerlinInterpolation;
    FSmoothing     : TGLPerlinInterpolation;
    FPersistence : Double;
    FNumber_Of_Octaves : Integer;
    FLines          : TStrings;
    FLinesChanged : Boolean;
    FXStart, FYStart : Integer;

  public
    MaxValue, MinValue : Double;
    Stall : Boolean;
    Constructor Create(AOwner : TComponent); override;
    procedure  StartPreparingData(heightData : THeightData); override;
    procedure  WaitFor;
    property  Lines  : TStrings read FLines;
    property  LinesChanged : Boolean read FLinesChanged write FLinesChanged;
  published
    property  Interpolation : TGLPerlinInterpolation read FInterpolation write FInterpolation;
    property  Smoothing : TGLPerlinInterpolation read FSmoothing write FSmoothing;
    property Persistence : Double read FPersistence write FPersistence;
    property Number_Of_Octaves : Integer read FNumber_Of_Octaves write FNumber_Of_Octaves;
    property MaxPoolSize;
    property XStart : Integer read FXStart write FXStart;
    property YStart : Integer read FYStart write FYStart;
  End;

  TGLPerlinHDSThread = class (THeightDataThread)
    Perlin       : TGL2DPerlin;
    PerlinSource : TGLPerlinHDS;
    Procedure OpdateOutSide;
    Procedure Execute; override;
  end;

implementation

type
   PSmallInt = ^SmallInt;

function TGLBasePerlin.PerlinNoise_1D(x : Double) : Double;

Var
  int_x : Integer;
  frac_x : Double;

Begin
  int_x := Round(Int(x));
  frac_x := X-int_x;
  case Interpolation of
    pi_none   : Result := 0;
    pi_simple : Result := Perlin_Random1(int_x);
    pi_linear : Result := Linear_Interpolate(Perlin_Random1(int_x),Perlin_Random1(int_x+1),frac_x);
    pi_cubic  : Result := Cubic_Interpolate(Perlin_Random1(int_x-1),Perlin_Random1(int_x),Perlin_Random1(int_x+1),Perlin_Random1(int_x+2),frac_x);
    pi_cosine : Result := Cosine_Interpolate(Perlin_Random1(int_x),Perlin_Random1(int_x+1),frac_x);
    else raise exception.create('TGLBasePerlin.PerlinNoise_1D, Interpolation not implemented!');
  End;
End;

function TGLBasePerlin.PerlinNoise_2D(x,y : Double) : Double;
Var
  int_x, int_y : Integer;
//  frac_y,
  frac_x : Double;

Begin
  int_x := Round(Int(x));
  int_y := Round(Int(y));
  frac_x := X-int_x;
//  frac_y := y-int_y;
  case Interpolation of
    pi_none   : Result := 0;
    pi_simple : Result := Perlin_Random2(int_x,int_y);
    pi_linear : Result := Linear_Interpolate(Perlin_Random1(int_x),Perlin_Random1(int_x+1),frac_x);
    pi_cubic  : Result := Cubic_Interpolate(Perlin_Random1(int_x-1),Perlin_Random1(int_x),Perlin_Random1(int_x+1),Perlin_Random1(int_x+2),frac_x);
    pi_cosine : Result := Cosine_Interpolate(Perlin_Random1(int_x),Perlin_Random1(int_x+1),frac_x);
    else raise exception.create('TGLBasePerlin.PerlinNoise_1D, Interpolation not implemented!');
  End;
End;

function TGLBasePerlin.GetOctave(index : Integer) : TGLBasePerlinOctav;
Begin
  Result := TGLBasePerlinOctav(FOctaves[index]);
End;

Procedure TGLBasePerlin.Set_Number_Of_Octaves(val : integer);

Var
  XC : Integer;
  NewScale : Integer;
  Octav : TGLBasePerlinOctav;
Begin
  If val <> FNumber_Of_Octaves then
  Begin
    FNumber_Of_Octaves := Val;
    For XC := FOctaves.Count to FNumber_Of_Octaves-1 do
    Begin
      Octav := FOctavClass.Create;
      If FPersistence = 0 then
        Octav.FAmplitude := 0
      else
        Octav.FAmplitude := exp(ln(FPersistence)*(XC+1));
      Octav.FInterpolation := Interpolation;
      Octav.FSmoothing := Smoothing;
      FOctaves.Add(Octav);
    End;
    For XC := FOctaves.Count-1 downto FNumber_Of_Octaves do
    Begin
      Octav := Octaves[XC];
      FOctaves.Delete(XC);
      Octav.Free;
    End;

    NewScale := 1;
    For XC := FOctaves.Count-1 downto 0 do
    Begin
      Octaves[XC].Scale := NewScale;
      NewScale := NewScale shl 1;
    End;

  End;
end;

Procedure TGLBasePerlin.SetPersistence(val : double);

Var
  XC : integer;
Begin
  If FPersistence <> val then
  Begin
    FPersistence := val;
    For XC := FOctaves.Count to FNumber_Of_Octaves-1 do
    Begin
      Octaves[XC].FAmplitude := exp(ln(FPersistence)*XC);
    End;
  End;
End;

Constructor TGLBasePerlin.Create(AOwner : TComponent);

Begin
  inherited;
  FOctaves := TList.Create;
  FNumber_Of_Octaves := 0;
  FInterpolation := pi_Cosine;
  FSmoothing     := pi_cubic;
End;

Function TGL1DPerlin.GetPerlinValue_1D(X : Double) : Double;
Var
  total, p,  frequency, amplitude : Double;
  n, i : Integer;
Begin
  total := 0;
  p := persistence;
  n := Number_Of_Octaves - 1;

  For I := 0 to n do
  Begin

      frequency := 2*i;
      amplitude := p*i;

      total := total + PerlinNoise_1D(x * frequency) * amplitude;

  end;

  Result := total;
End;

Procedure TGL2DPerlinOctav.Generate;

Var
  YC : Integer;

Begin
  SetLength(Data,Height+3);     // needed for smoothing
  For YC := 0 to Height+2 do
  SetLength(Data[yc],Width+3);  // needed for smoothing
  case Smoothing of
    pi_cubic :
    Begin
      Generate_CubicInterpolate;
    End;
    pi_Smoothed :
    Begin
      Generate_SmoothInterpolate;
    End;
    pi_none   : ;
    pi_simple :
    Begin
      Generate_NonInterpolated;
    End;
  End;
End;

Function  TGL2DPerlinOctav.GetPerling(X,Y : Double) : Double;

Begin
  Result := 0;
  case Interpolation of
    pi_cubic :
    Begin
      Result := getCubic(X,Y);
    End;
    pi_Smoothed :
    Begin
    End;
    pi_Cosine :
    Begin
      Result := GetCosine(X,Y);
    End;
  End;
End;

Procedure TGL2DPerlinOctav.Generate_CubicInterpolate;

Var
  B1, B2, B3, B4, T1 : T1DPerlinArray;
  StripWidth : Integer;
  Offset : Integer;
  YC : Integer;


Begin
  T1 := Nil;
  StripWidth := Width+6;
  SetLength(B1,StripWidth);
  SetLength(B2,StripWidth);
  SetLength(B3,StripWidth);
  SetLength(B4,StripWidth);
  Offset := (XStart-1)+(YStart-1)*YStep*YRate;
  Perlin_Random1DStrip(Offset,StripWidth,XStep,FAmplitude,B1);
  inc(Offset,YRate*YStep);
  Perlin_Random1DStrip(Offset,StripWidth,XStep,FAmplitude,B2);
  inc(Offset,YRate*YStep);
  Perlin_Random1DStrip(Offset,StripWidth,XStep,FAmplitude,B3);
  inc(Offset,YRate*YStep);
  For YC := 0 to Height+2 do
  Begin
    Perlin_Random1DStrip(Offset,StripWidth,XStep,FAmplitude,B4);
    inc(Offset,YRate*YStep);
    Cubic_Interpolate_Strip(B1,B2,B3,B4,Data[YC],Width+3);
    T1 := B1;
    B1 := B2;
    B2 := B3;
    B3 := B4;
    B4 := T1;
  End;
  SetLength(B1,0);
  SetLength(B2,0);
  SetLength(B3,0);
  SetLength(B4,0);
End;

Procedure TGL2DPerlinOctav.Generate_SmoothInterpolate;

Var
  B1, B2, B3, T1 : T1DPerlinArray;
  StripWidth : Integer;
  Offset : Integer;
  YC : Integer;


Begin
  T1 := Nil;
  StripWidth := Width+5;
  SetLength(B1,StripWidth);
  SetLength(B2,StripWidth);
  SetLength(B3,StripWidth);
  Offset := (XStart-1)+(YStart-1)*YStep*YRate;
  Perlin_Random1DStrip(Offset,StripWidth,XStep,FAmplitude,B1);
  inc(Offset,YRate*YStep);
  Perlin_Random1DStrip(Offset,StripWidth,XStep,FAmplitude,B2);
  inc(Offset,YRate*YStep);
  For YC := 0 to Height+2 do
  Begin
    Perlin_Random1DStrip(Offset,StripWidth,XStep,FAmplitude,B3);
    inc(Offset,YRate*YStep);
    Smooth_Interpolate_Strip(B1,B2,B3,Data[YC],Width+3);
    T1 := B1;
    B1 := B2;
    B2 := B3;
    B3 := T1;
  End;
  SetLength(B1,0);
  SetLength(B2,0);
  SetLength(B3,0);
End;

Procedure TGL2DPerlinOctav.Generate_NonInterpolated;

Var
  Offset : Integer;
  YC : Integer;


Begin
  Offset := XStart+YStart*YStep*YRate;
  For YC := 0 to Height+2 do
  Begin
    Perlin_Random1DStrip(Offset,Width+3,XStep,FAmplitude,Data[YC]);
    inc(Offset,YRate*YStep);

  End;
End;

Function  TGL2DPerlinOctav.GetDataSmoothed(X,Y : Integer) : Double;

Begin
  Result := (Data[Y][X]+ Data[Y][X+2]+ Data[Y+2][X]+ Data[Y+2][X+2])/16+
            (Data[Y+1][X]+ Data[Y+1][X+2]+ Data[Y][X+1]+ Data[Y+2][X+1])/8+
            Data[Y+1][X+1]/4;{}
End;


Function  TGL2DPerlinOctav.GetData(X,Y : Integer) : Double;

Begin
  Result := Data[Y][X];
End;

Function  TGL2DPerlinOctav.GetCubic(X,Y : Double) : Double;

Var
  X_Int : Integer;
  Y_Int : Integer;
  X_Frac,
  Y_Frac : Double;

Begin
  X_Int := Round(Int(X));
  Y_Int := Round(Int(Y));
  X_Frac := X-X_Int;
  Y_Frac := Y-Y_Int;

  Result := (Cubic_Interpolate(GetData(X_Int,Y_Int+1),GetData(X_Int+1,Y_Int+1),GetData(X_Int+2,Y_Int+1),GetData(X_Int+3,Y_Int+1),X_Frac)
         +   Cubic_Interpolate(GetData(X_Int+1,Y_Int),GetData(X_Int+1,Y_Int+1),GetData(X_Int+1,Y_Int+2),GetData(X_Int+1,Y_Int+3),Y_Frac)) / 2;
End;


Function  TGL2DPerlinOctav.GetCosine(X,Y : Double) : Double;

Var
  X_Int : Integer;
  Y_Int : Integer;
  X_Frac,
  Y_Frac : Double;

Begin
  X_Int := Round(Int(X));
  Y_Int := Round(Int(Y));
  X_Frac := X-X_Int;
  Y_Frac := Y-Y_Int;

  Result := Cosine_Interpolate(
      Cosine_Interpolate(GetData(X_Int,Y_Int),GetData(X_Int+1,Y_Int),X_Frac),
      Cosine_Interpolate(GetData(X_Int,Y_Int+1),GetData(X_Int+1,Y_Int+1),X_Frac),
      Y_Frac);
End;

Constructor TGL2DPerlin.Create(AOwner : TComponent);

Begin
  inherited;
  Width := 256;
  Height := 256;
  XStart := 0;
  YStart := 0;
  XStep := 1;
  YStep := 1;
  FOctavClass := TGL2DPerlinOctav;
End;

Procedure TGL2DPerlin.Generate;

Var
  i : Integer;

Begin
  For I := 0 to Number_Of_Octaves-1 do
  With TGL2DPerlinOctav(Octaves[I]) do
  Begin
    Width  := Round(Ceil(self.Width / Scale));
    Height := Round(Ceil(self.Height / Scale));
    XStart := Round(self.XStart / Scale);
    YStart := Round(self.YStart / Scale);
    XStep  := self.XStep;
    YStep  := self.YStep;
    YRate  := 243*57*57;
    Generate;
  end;
End;

Function TGL2DPerlin.GetPerlinValue_2D(X,Y : Double) : Double;
Var
  total, frequency, amplitude : Double;
  i : Integer;
Begin
  total := 0;
  For I := 0 to Number_Of_Octaves-1 do
  Begin

      frequency := 2*i;
      amplitude := persistence*i;

      total := total + PerlinNoise_2D(x * frequency, y * frequency) * amplitude;

  end;

  Result := total;
End;

Procedure TGL2DPerlin.MakeBitmap(Param : TGLBitmap);

Var
  XC, YC : Integer;
  Octaver : Integer;
  Posi : PByte;
  B : Integer;
  Value : Double;
  S : String;

Begin
{$ifndef FPC}
  MaxValue := -1;
  MinValue := 100;

  Param.Width := Width;
  Param.Height := Height;

  For YC := 0 to Height-1 do
  Begin
    Posi := Param.ScanLine[YC];
    For XC := 0 to Width-1 do
    Begin
      Value := 0;
      For Octaver := 0 to FNumber_Of_Octaves-1 do
      With TGL2DPerlinOctav(Octaves[Octaver]) do
        Value := Value + GetPerling(XC/Scale,YC/Scale);

      Value := Value+0.5;
      If MaxValue < Value then
      MaxValue := Value;

      If MinValue > Value then
      MinValue := Value;

      If Value > 1.0 then
      Begin
        S := '';
        For Octaver := 0 to FNumber_Of_Octaves-1 do
        With TGL2DPerlinOctav(Octaves[Octaver]) do
          S := S + FloatToStr(GetPerling(XC/Scale,YC/Scale))+' ,';
        Delete(S,Length(S)-1,2);
//        raise Exception.create('In Cubic_Interpolate_Strip a value greater than 1 occured! value = '+FloatToStr(Value)+' values=['+S+']');
      End;

      B := Round(Value * $FF) and $FF;
      Posi^ := b;
      inc(Integer(Posi));
    End;
  End;
{$ELSE}
  assert(false,'MakeBitmap not supported at this time');
{$ENDIF}
End;

Procedure TGL2DPerlin.SetHeightData(heightData : THeightData);


Var
  XC, YC : Integer;
  Octaver : Integer;
  Posi : PSmallInt;
  Value : Double;
  S : String;

Begin

  MaxValue := -1;
  MinValue := 100;

  heightData.Allocate(hdtSmallInt);

  Posi := @heightData.SmallIntData^[0];
  For YC := 0 to Height-1 do
  Begin
    For XC := 0 to Width-1 do
    Begin
      Value := 0;
      For Octaver := 0 to FNumber_Of_Octaves-1 do
      With TGL2DPerlinOctav(Octaves[Octaver]) do
        Value := Value + GetPerling(XC/Scale,YC/Scale);

      // value = [-0,5 .. 0,5]
      Posi^ := Round(Value * 256*100); //100 instead of 128 to keep it well in range!

      If MaxValue < Value then
      MaxValue := Value;

      If MinValue > Value then
      MinValue := Value;

      If Value > 1.0 then
      Begin
        S := '';
        For Octaver := 0 to FNumber_Of_Octaves-1 do
        With TGL2DPerlinOctav(Octaves[Octaver]) do
          S := S + FloatToStr(GetPerling(XC/Scale,YC/Scale))+' ,';
        Delete(S,Length(S)-1,2);
//        raise Exception.create('In Cubic_Interpolate_Strip a value greater than 1 occured! value = '+FloatToStr(Value)+' values=['+S+']');
      End;

      inc(Posi);
    End;
  End;
End;

Constructor TGLPerlinHDS.Create(AOwner : TComponent);

Begin
  inherited;
  FLines := TStringList.Create;
  FInterpolation := pi_Cosine;
  FSmoothing     := pi_cubic;
  FPersistence   := 0.4;
  FNumber_Of_Octaves := 6;
  MaxValue := -MaxInt;
  MinValue := MaxInt;
  MaxThreads := 1;
End;

procedure TGLPerlinHDS.StartPreparingData(heightData : THeightData);

Var
  Perlin : TGL2DPerlin;
  Thread : TGLPerlinHDSThread;

Begin
  If Stall then
    heightData.DataState:=hdsNone
  else
    heightData.DataState:=hdsPreparing;
  Perlin := TGL2DPerlin.Create(Self);
  Perlin.Width  := HeightData.Size;
  Perlin.Height := HeightData.Size;
  Perlin.XStart := heightData.XLeft+XStart;
  Perlin.YStart := heightData.YTop+YStart;

  Perlin.Interpolation := Interpolation;
  Perlin.Smoothing     := Smoothing;
  Perlin.Persistence := Persistence;
  Perlin.Number_Of_Octaves := Number_Of_Octaves;

  If MaxThreads > 1 then
  Begin
    Thread := TGLPerlinHDSThread.Create(True);
    Thread.FreeOnTerminate := True;
    heightData.Thread := Thread;
    Thread.FHeightData := heightData;
    Thread.Perlin := Perlin;
    Thread.PerlinSource := Self;
    Thread.Resume;
  End else
  Begin
    Perlin.Generate;
    Perlin.SetHeightData(heightData);
    heightData.DataState:=hdsReady;

    If MaxValue < Perlin.MaxValue then
    MaxValue := Perlin.MaxValue;

    If MinValue < Perlin.MinValue then
    MinValue := Perlin.MinValue;
    Perlin.Free;
  end;

  Lines.Add('Prepared Perlin ('+IntToStr(Perlin.XStart)+','+IntToStr(Perlin.YStart)+') size '+IntToStr(Perlin.Width));
  LinesChanged := True;
End;

procedure  TGLPerlinHDS.WaitFor;

Var
  HDList : TList;
  HD : THeightData;
  XC : Integer;
Begin
  Repeat
    HDList := Data.LockList;
    try
      HD := Nil;
      For XC := 0 to HDList.Count-1 do
      Begin
        HD := THeightData(hdList[XC]);
        If HD.DataState <> hdsReady then Break;
      End;
      If Assigned(HD) then 
      If HD.DataState = hdsReady then Break;
    finally
      Data.UnlockList;
    End;
    Sleep(10);
  Until False;
End;

Procedure TGLPerlinHDSThread.Execute;

Begin
  Perlin.Generate;
  Perlin.SetHeightData(FheightData);
  FheightData.DataState:=hdsReady;

  If PerlinSource.MaxValue < Perlin.MaxValue then
  PerlinSource.MaxValue := Perlin.MaxValue;

  If PerlinSource.MinValue < Perlin.MinValue then
  PerlinSource.MinValue := Perlin.MinValue;
  Perlin.Free;
end;

Procedure TGLPerlinHDSThread.OpdateOutSide;

Begin
End;

initialization

  RegisterClasses([TGLPerlinHDS]);

end.
