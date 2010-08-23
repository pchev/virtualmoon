//
// This unit is part of the GLScene Project, http://glscene.org
//
{: RGBE<p>

 <b>History : </b><font size=-1><ul>
        <li>20/01/10 - Yar - Creation
   </ul><p>
}
unit RGBE;

interface

{$I GLScene.inc}

uses
  Classes, VectorTypes, VectorGeometry;

procedure float2rgbe(var rgbe: TVector4b; const red, green, blue: Single);
procedure rgbe2float(var red, green, blue: Single; const rgbe: TVector4b);
procedure LoadRLEpixels(stream : TStream; dst: PSingle;
  scanline_width,	num_scanlines: integer);
procedure LoadRGBEpixels(stream : TStream; dst: PSingle; numpixels: integer);

implementation

uses
  SysUtils;

type
  ERGBEexception = class(Exception);
{ Extract exponent and mantissa from X }
procedure Frexp(X: Extended; var Mantissa: Extended; var Exponent: Integer);
{ Mantissa ptr in EAX, Exponent ptr in EDX }
{$IFDEF GLS_NO_ASM}
begin
  Exponent:=0;
  if (X<>0) then
    if (abs(X)<0.5) then
      repeat
        X:=X*2;
        Dec(Exponent);
      until (abs(X)>=0.5)
    else
      while (abs(X)>=1) do
        begin
        X:=X/2;
        Inc(Exponent);
        end;
  Mantissa:=X;
{$ELSE}
asm
        FLD     X
        PUSH    EAX
        MOV     dword ptr [edx], 0    { if X = 0, return 0 }

        FTST
        FSTSW   AX
        FWAIT
        SAHF
        JZ      @@Done

        FXTRACT                 // ST(1) = exponent, (pushed) ST = fraction
        FXCH

// The FXTRACT instruction normalizes the fraction 1 bit higher than
// wanted for the definition of frexp() so we need to tweak the result
// by scaling the fraction down and incrementing the exponent.

        FISTP   dword ptr [edx]
        FLD1
        FCHS
        FXCH
        FSCALE                  // scale fraction
        INC     dword ptr [edx] // exponent biased to match
        FSTP ST(1)              // discard -1, leave fraction as TOS

@@Done:
        POP     EAX
        FSTP    tbyte ptr [eax]
        FWAIT
{$ENDIF}
end;

function Ldexp(X: Extended; const P: Integer): Extended;
{$IFDEF GLS_NO_ASM}
begin
   ldexp:=x*intpower(2.0,p);
{$ELSE}
  { Result := X * (2^P) }
asm
        PUSH    EAX
        FILD    dword ptr [ESP]
        FLD     X
        FSCALE
        POP     EAX
        FSTP    ST(1)
        FWAIT
{$ENDIF}
end;

// standard conversion from float pixels to rgbe pixels
procedure float2rgbe(var rgbe: TVector4b; const red, green, blue: Single);
var
  v, m: Extended;
  e: integer;
begin
  v := red;
  if (green > v) then v := green;
  if (blue > v) then v := blue;
  if (v < 1e-32) then begin
    rgbe[0] := 0;
    rgbe[1] := 0;
    rgbe[2] := 0;
    rgbe[3] := 0;
  end
  else begin
    FrExp(v, m, e);
    m := m * 256/v;
    rgbe[0] := Floor(red * v);
    rgbe[1] := Floor(green * v);
    rgbe[2] := Floor(blue * v);
    rgbe[3] := Floor(e + 128);
  end;
end;

// standard conversion from rgbe to float pixels
// note: Ward uses ldexp(col+0.5,exp-(128+8)).  However we wanted pixels
//       in the range [0,1] to map back into the range [0,1].
procedure rgbe2float(var red, green, blue: Single; const rgbe: TVector4b);
var
  f: single;
begin
  if rgbe[3]<>0 then   //nonzero pixel
  begin
    f := ldexp(1.0, rgbe[3]-(128+8));
    red := rgbe[0] * f;
    green := rgbe[1] * f;
    blue := rgbe[2] * f;
  end
  else begin
    red := 0;
    green := 0;
    blue := 0;
  end;
end;

procedure LoadRLEpixels(stream : TStream; dst: PSingle;
  scanline_width,	num_scanlines: Integer);
var
  rgbeTemp: TVector4b;
  buf: TVector2b;
  rf, gf, bf: Single;
  scanline_buffer: PByteArray;
  ptr, ptr_end: PByte;
  i, count: integer;
begin
  if (scanline_width < 8) or (scanline_width > $7FFF) then begin
    //run length encoding is not allowed so read flat
    LoadRGBEPixels(stream, dst, scanline_width*num_scanlines);
    Exit;
  end;

  scanline_buffer := nil;
  while num_scanlines > 0 do
  begin
    stream.Read(rgbeTemp, SizeOf( TVector4b ));
    if (rgbeTemp[0] <> 2)
    or (rgbeTemp[1] <> 2)
    or (rgbeTemp[2] and $80 <> 0) then
    begin
      // this file is not run length encoded
      rgbe2float( rf, gf, bf, rgbeTemp);
      dst^ := rf; Inc(dst);
      dst^ := gf; Inc(dst);
      dst^ := bf; Inc(dst);
      if Assigned(scanline_buffer) then FreeMem( scanline_buffer );
      LoadRGBEpixels(stream, dst, scanline_width*num_scanlines-1);
      Exit;
    end;
    if ((Integer(rgbeTemp[2]) shl 8) or rgbeTemp[3]) <> scanline_width then begin
      if Assigned(scanline_buffer) then FreeMem( scanline_buffer );
      raise ERGBEexception.Create('Wrong scanline width.');
    end;

    if not Assigned(scanline_buffer) then ReallocMem(scanline_buffer, 4*scanline_width);

    ptr := PByte( scanline_buffer );
    // read each of the four channels for the scanline into the buffer
    for i := 0 to 3 do
    begin
      ptr_end := @scanline_buffer[(i+1)*scanline_width];
      while Integer(ptr) < Integer(ptr_end) do
      begin
        stream.Read(buf, SizeOf( TVector2b ));
        if buf[0] > 128 then
        begin //a run of the same value
          count := buf[0]-128;
          if (count = 0) or (count > Integer(ptr_end) - Integer(ptr)) then
          begin
            FreeMem( scanline_buffer );
            raise ERGBEexception.Create('Bad HDR scanline data.');
          end;
          while count > 0 do
          begin
            ptr^ := buf[1];
            Dec( count );
            Inc( ptr );
          end;
        end
        else begin //a non-run
          count := buf[0];
          if (count = 0) or (count > Integer(ptr_end) - Integer(ptr)) then
          begin
            FreeMem( scanline_buffer );
            raise ERGBEexception.Create('Bad HDR scanline data.');
          end;
          ptr^ := buf[1];
          Dec( count );
          Inc( ptr );
          if count>0 then stream.Read(ptr^, Count);
          Inc( ptr, count);
        end;
      end;
    end;

    // now convert data from buffer into floats
    for i := 0 to scanline_width-1 do
    begin
      rgbeTemp[0] := scanline_buffer[i];
      rgbeTemp[1] := scanline_buffer[i+scanline_width];
      rgbeTemp[2] := scanline_buffer[i+2*scanline_width];
      rgbeTemp[3] := scanline_buffer[i+3*scanline_width];
      rgbe2float( rf, gf, bf, rgbeTemp);
      dst^ := rf; Inc(dst);
      dst^ := gf; Inc(dst);
      dst^ := bf; Inc(dst);
    end;
    Dec( num_scanlines );
  end;
  if Assigned(scanline_buffer) then FreeMem( scanline_buffer );
end;

procedure LoadRGBEpixels(stream : TStream; dst: PSingle; numpixels: integer);
var
  rgbeTemp: TVector4b;
  rf, gf, bf: Single;
begin
  while numpixels>0 do
  begin
    stream.Read(rgbeTemp, SizeOf( TVector4b ));
    rgbe2float( rf, gf, bf, rgbeTemp);
    dst^ := rf; Inc(dst);
    dst^ := gf; Inc(dst);
    dst^ := bf; Inc(dst);
    Dec( numpixels );
  end;
end;

end.
