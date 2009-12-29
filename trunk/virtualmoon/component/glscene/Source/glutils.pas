//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLUtils<p>

   Miscellaneous support utilities & classes.<p>

      $Log: glutils.pas,v $
      Revision 1.1  2006/01/10 20:50:46  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.4  2006/01/09 20:45:51  z0m3ie
      *** empty log message ***

      Revision 1.3  2006/01/08 21:04:12  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/12/04 16:53:06  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.1  2005/12/01 21:24:11  z0m3ie
      *** empty log message ***

      Revision 1.3  2005/08/03 00:41:39  z0m3ie
      - added automatical generated History from CVS

	<b>History : </b><font size=-1><ul>
      <li>24/03/08 - DaStr - Removed OpenGL1x dependancy
                             Moved TGLMinFilter and TGLMagFilter from GLUtils.pas
                              to GLGraphics.pas (BugTracker ID = 1923844)
      <li>25/03/07 - DaStr - Replaced StrUtils with GLCrossPlatform
      <li>23/03/07 - DaStr - Removed compiler warnings caused by
                               SaveComponentToFile and LoadComponentFromFile
      <li>22/03/07 - DaStr - Added SaveComponentToFile, LoadComponentFromFile
      <li>07/02/07 - DaStr - Added StringToColorAdvanced() functions
      <li>05/09/03 - EG - Creation from GLMisc split
   </ul></font>
}
unit GLUtils;

interface

uses
  // VCL
  Classes, SysUtils, Graphics,

  // GLScene
  VectorGeometry, GLCrossPlatform;

{$i GLScene.inc}

type
  EGLUtilsException = class(Exception);

	TSqrt255Array = array [0..255] of Byte;
	PSqrt255Array = ^TSqrt255Array;

//: Copies the values of Source to Dest (converting word values to integer values)
procedure WordToIntegerArray(Source: PWordArray; Dest: PIntegerArray; Count: Cardinal);
//: Round ups to the nearest power of two, value must be positive
function RoundUpToPowerOf2(value : Integer): Integer;
//: Round down to the nearest power of two, value must be strictly positive
function RoundDownToPowerOf2(value : Integer): Integer;
//: Returns True if value is a true power of two
function IsPowerOf2(value : Integer) : Boolean;
{: Read a CRLF terminated string from a stream.<p>
   The CRLF is NOT in the returned string. }
function ReadCRLFString(aStream : TStream) : String;
//: Write the string and a CRLF in the stream
procedure WriteCRLFString(aStream : TStream; const aString : String);
//: TryStrToFloat
function TryStrToFloat(const strValue : String; var val : Extended) : Boolean;
//: StrToFloatDef
function StrToFloatDef(const strValue : String; defValue : Extended = 0) : Extended;

//: Converts a string into color
function StringToColorAdvancedSafe(const Str: string; const Default: TColor): TColor;
//: Converts a string into color
function TryStringToColorAdvanced(const Str: string; var OutColot: TColor): Boolean;
//: Converts a string into color
function StringToColorAdvanced(const Str: string): TColor;

{: Parses the next integer in the string.<p>
   Initial non-numeric characters are skipper, p is altered, returns 0 if none
   found. '+' and '-' are acknowledged. }
function ParseInteger(var p : PChar) : Integer;
{: Parses the next integer in the string.<p>
   Initial non-numeric characters are skipper, p is altered, returns 0 if none
   found. Both '.' and ',' are accepted as decimal separators. }
function ParseFloat(var p : PChar) : Extended;

{: Saves "data" to "filename". }
procedure SaveStringToFile(const fileName, data : String);
{: Returns the content of "filename". }
function LoadStringFromFile(const fileName : String) : String;

{: Saves component to a file. }
procedure SaveComponentToFile(const Component: TComponent; const FileName: string; const AsText: Boolean = True);
{: Loads component from a file. }
procedure LoadComponentFromFile(const Component: TComponent; const FileName: string; const AsText: Boolean = True);

{: Returns the size of "filename".<p>
   Returns 0 (zero) is file does not exists. }
function SizeOfFile(const fileName : String) : Int64;

{: Returns a pointer to an array containing the results of "255*sqrt(i/255)". }
function GetSqrt255Array : PSqrt255Array;

//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------
implementation
//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------

uses ApplicationFileIO;

var
	vSqrt255 : TSqrt255Array;

resourcestring
  gluInvalidColor = '''%s'' is not a valid color format!';

// WordToIntegerArray
//
procedure WordToIntegerArray(Source: PWordArray; Dest: PIntegerArray; Count: Cardinal); assembler;
// EAX contains Source
// EDX contains Dest
// ECX contains Count
asm
              JECXZ @@Finish
              PUSH ESI
              PUSH EDI
              MOV ESI,EAX
              MOV EDI,EDX
              XOR EAX,EAX
@@1:          LODSW
              STOSD
              DEC ECX
              JNZ @@1
              POP EDI
              POP ESI
@@Finish:
end;

// RoundUpToPowerOf2
//
function RoundUpToPowerOf2(value : Integer) : Integer;
begin
   Result:=1;
   while (Result<value) do Result:=Result shl 1;
end;

// RoundDownToPowerOf2
//
function RoundDownToPowerOf2(value : Integer) : Integer;
begin
   if value>0 then begin
      Result:=1 shl 30;
      while Result>value do Result:=Result shr 1;
   end else Result:=1;
end;

// IsPowerOf2
//
function IsPowerOf2(value : Integer) : Boolean;
begin
   Result:=(RoundUpToPowerOf2(value)=value);
end;

// ReadCRLFString
//
function ReadCRLFString(aStream : TStream) : String;
var
   c : Char;
   CR, LF: Char;
begin
   Result := '';
   CR := #0;
   LF := #0;
   while ((CR <> #13) or (LF <> #10)) and (aStream.Read(c, 1) > 0) do begin
     Result := Result + c;
     CR := LF;
     LF := c;
   end;
   Result := StringReplace(Result, #13, '', [rfReplaceAll]);
   Result := StringReplace(Result, #10, '', [rfReplaceAll]);
end;

// WriteCRLFString
//
procedure WriteCRLFString(aStream : TStream; const aString : String);
const
   cCRLF : Integer = $0A0D;
begin
   with aStream do begin
      Write(aString[1], Length(aString));
      Write(cCRLF, 2);
   end;
end;

// TryStrToFloat
//
function TryStrToFloat(const strValue : String; var val : Extended): Boolean;
var
   i, j, divider, lLen, exponent : Integer;
   c : Char;
   v : Extended;
begin
   if strValue='' then begin
      Result:=False;
      Exit;
   end else v:=0;
   lLen:=Length(strValue);
   while (lLen>0) and (strValue[lLen]=' ') do Dec(lLen);
   divider:=lLen+1;
   exponent:=0;
	for i:=1 to lLen do begin
      c:=strValue[i];
      case c of
         ' ' : if v<>0 then begin
            Result:=False;
            Exit;
         end;
         '0'..'9' : v:=(v*10)+Integer(c)-Integer('0');
         ',', '.' : begin
            if (divider>lLen) then
               divider:=i+1
            else begin
               Result:=False;
               Exit;
            end;
         end;
         '-', '+' : if i>1 then begin
            Result:=False;
            Exit;
         end;
         'e', 'E' : begin
            if i+1>lLen then begin
               Result:=False;
               Exit;
            end;
            for j:=i+1 to lLen do begin
               c:=strValue[j];
               case c of
                  '-', '+' : if j<>i+1 then begin
         				Result:=False;
                     Exit;
                  end;
                  '0'..'9' : exponent:=(exponent*10)+Integer(c)-Integer('0');
               else
                  Result:=False;
                  Exit;
               end;
            end;
            if strValue[i+1]<>'-' then
               exponent:=-exponent;
            exponent:=exponent-1;
            lLen:=i;
            if divider>lLen then
               divider:=lLen;
            Break;
         end;
		else
         Result:=False;
         Exit;
      end;
   end;
   divider:=lLen-divider+exponent+1;
   if strValue[1]='-' then begin
      v:=-v;
   end;
   if divider<>0 then
      v:=v*Exp(-divider*Ln(10));
   val:=v;
   Result:=True;
end;

// StrToFloatDef
//
function StrToFloatDef(const strValue : String; defValue : Extended = 0) : Extended;
begin
   if not TryStrToFloat(strValue, Result) then
      result:=defValue;
end;

// StringToColorAdvancedSafe
//
function StringToColorAdvancedSafe(const Str: string; const Default: TColor): TColor;
begin
  if not TryStringToColorAdvanced(Str, Result) then
    Result := Default;
end;

// StringToColorAdvanced
//
function StringToColorAdvanced(const Str: string): TColor;
begin
  if not TryStringToColorAdvanced(Str, Result) then
    raise EGLUtilsException.CreateResFmt(@gluInvalidColor, [Str]);
end;

// TryStringToColorAdvanced
//
function TryStringToColorAdvanced(const Str: string; var OutColot: TColor): Boolean;
var
  Code, I: Integer;
  Temp:    string;
begin
  Result := True;
  Temp := Str;

  Val(Temp, I, Code); //to see if it is a number
  if Code = 0 then
    OutColot := TColor(I)                                       //Str = $0000FF
  else
  begin
    if not IdentToColor(Temp, Longint(OutColot)) then           //Str = clRed
    begin
      if AnsiStartsText('clr', Temp) then                       //Str = clrRed
      begin
        Delete(Temp, 3, 1);
        if not IdentToColor(Temp, Longint(OutColot)) then
          Result := False;
      end
      else if not IdentToColor('cl' + Temp, Longint(OutColot)) then //Str = Red
        Result := False;
    end;
  end;
end;


// ParseInteger
//
function ParseInteger(var p : PChar) : Integer;
var
   neg : Boolean;
   c : Char;
begin
   Result:=0;
   if p=nil then Exit;
   neg:=False;
   // skip non-numerics
   while not (p^ in [#0, '0'..'9', '+', '-']) do Inc(p);
   c:=p^;
   if c='+' then
      Inc(p)
   else if c='-' then begin
      neg:=True;
      Inc(p);
   end;
   // Parse numerics
   while True do begin
      c:=p^;
      if not (c in ['0'..'9']) then Break;
      Result:=Result*10+Integer(c)-Integer('0');
      Inc(p);
   end;
   if neg then
      Result:=-Result;
end;

// ParseFloat
//
function ParseFloat(var p : PChar) : Extended;
var
   decimals, expSign, exponent : Integer;
   c : Char;
   neg : Boolean;
begin
   Result:=0;
   if p=nil then Exit;
   // skip non-numerics
   while not (p^ in [#0, '0'..'9', '+', '-']) do Inc(p);
   c:=p^;
   if c='+' then begin
      neg:=False;
      Inc(p);
   end else if c='-' then begin
      neg:=True;
      Inc(p);
   end else neg:=False;
   // parse numbers
   while (p^ in ['0'..'9']) do begin
      Result:=Result*10+(Integer(p^)-Integer('0'));
      Inc(p);
   end;
   // parse dot, then decimals, if any
   decimals:=0;
   if (p^='.') then begin
      Inc(p);
      while (p^ in ['0'..'9']) do begin
         Result:=Result*10+(Integer(p^)-Integer('0'));
         Inc(p);
         Dec(decimals);
      end;
   end;
   // parse exponent, if any
   if (p^ in ['e', 'E']) then begin
      Inc(p);
      // parse exponent sign
      c:=p^;
      if c='-' then begin
         expSign:=-1;
         Inc(p);
      end else if c='+' then begin
         expSign:=1;
         Inc(p);
      end else expSign:=1;
      // parse exponent
      exponent:=0;
      while (p^ in ['0'..'9']) do begin
         exponent:=exponent*10+(Integer(p^)-Integer('0'));
         Inc(p);
      end;
      decimals:=decimals+expSign*exponent;
   end;
   if decimals<>0 then Result:=Result*Exp(decimals*Ln(10));
   if neg then Result:=-Result;
end;

// SaveStringToFile
//
procedure SaveStringToFile(const fileName, data : String);
var
   n : Cardinal;
	fs : TStream;
begin
	fs:=CreateFileStream(fileName, fmCreate);
   try
      n:=Length(data);
      if n>0 then
      	fs.Write(data[1], n);
   finally
   	fs.Free;
   end;
end;

// LoadStringFromFile
//
function LoadStringFromFile(const fileName : String) : String;
var
   n : Cardinal;
	fs : TStream;
begin
   if FileExists(fileName) then begin
   	fs:=CreateFileStream(fileName, fmOpenRead+fmShareDenyNone);
      try
         n:=fs.Size;
   	   SetLength(Result, n);
         if n>0 then
         	fs.Read(Result[1], n);
      finally
   	   fs.Free;
      end;
   end else Result:='';
end;

// SaveComponentToFile
//
procedure SaveComponentToFile(const Component: TComponent; const FileName: string; const AsText: Boolean);
var
  Stream: TStream;
  MemStream: TMemoryStream;
begin
  Stream := CreateFileStream(FileName, fmCreate);
  try
    if AsText then
    begin
      MemStream := TMemoryStream.Create;
      try
        MemStream.WriteComponent(Component);
        MemStream.Position := 0;
        ObjectBinaryToText(MemStream, Stream);
      finally
        MemStream.Free;
      end;
    end
    else
      Stream.WriteComponent(Component);
  finally
    Stream.Free;
  end;
end;

// LoadComponentFromFile
//
procedure LoadComponentFromFile(const Component: TComponent; const FileName: string; const AsText: Boolean = True);
var
  Stream: TStream;
  MemStream: TMemoryStream;
begin
  Stream := CreateFileStream(FileName, fmOpenRead);
  try
    if AsText then
    begin
      MemStream := TMemoryStream.Create;
      try
        ObjectTextToBinary(Stream, MemStream);
        MemStream.Position := 0;
        MemStream.ReadComponent(Component);
      finally
        MemStream.Free;
      end;
    end
    else
      Stream.ReadComponent(Component);
  finally
    Stream.Free;
  end;
end;

// SizeOfFile
//
function SizeOfFile(const fileName : String) : Int64;
var
	fs : TStream;
begin
   if FileExists(fileName) then begin
   	fs:=CreateFileStream(fileName, fmOpenRead+fmShareDenyNone);
      try
         Result:=fs.Size;
      finally
   	   fs.Free;
      end;
   end else Result:=0;
end;

// GetSqrt255Array
//
function GetSqrt255Array : PSqrt255Array;
const
   cOneDiv255 = 1/255;
var
	i : Integer;
begin
	if vSqrt255[255]<>255 then begin
		for i:=0 to 255 do
			vSqrt255[i]:=Integer(Trunc(255*Sqrt(i*cOneDiv255)));
	end;
	Result:=@vSqrt255;
end;

end.
