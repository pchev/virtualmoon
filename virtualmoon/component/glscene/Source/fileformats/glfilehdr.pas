//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFileHDR<p>

 <b>History : </b><font size=-1><ul>
        <li>23/11/10 - DaStr - Added $I GLScene.inc
        <li>23/01/10 - Yar - Added to AssignFromTexture CurrentFormat parameter
        <li>20/01/10 - Yar - Creation
   </ul><p>
}
unit GLFileHDR;

{$I GLScene.inc}

interface

uses
  Classes, SysUtils,
  OpenGL1x, GLContext, GLGraphics, GLTextureFormat,
  ApplicationFileIO;

type

  TGLHDRImage = class ( TGLBaseImage )
  private
    function  GetProgramType: Ansistring;
    procedure SetProgramType(aval: Ansistring);
  protected
    fGamma: Single;           // image has already been gamma corrected with
                              // given gamma.  defaults to 1.0 (no correction) */
    fExposure: Single;        // a value of 1.0 in an image corresponds to
			                        // <exposure> watts/steradian/m^2.
			                        // defaults to 1.0
    fProgramType: string[16];
  public
    class function Capabilities : TDataFileCapabilities; override;

    procedure LoadFromFile(const  filename: string); override;
    procedure LoadFromStream(stream : TStream); override;

    procedure AssignFromTexture(textureContext: TGLContext;
                                const textureHandle: TGLenum;
                                textureTarget: TGLenum;
                                const CurrentFormat: Boolean;
                                const intFormat: TGLInternalFormat); override;

    property Data           : PGLPixel32Array read FData;
    property Width          : Integer read fWidth;
    property Height         : Integer read fHeight;
    property Depth          : Integer read fDepth;
    property MipLevels      : Integer read fMipLevels;
    property ColorFormat    : GLenum  read fColorFormat;
    property InternalFormat : TGLInternalFormat read fInternalFormat;
    property DataType       : GLenum  read fDataType;
    property ElementSize    : Integer read fElementSize;

    property Gamma: Single            read fGamma;
    property Exposure: Single         read fExposure;
    property ProgramType: Ansistring  read GetProgramType write SetProgramType;
	end;

implementation

uses
  RGBE, VectorTypes, VectorGeometry;

// ------------------
// ------------------ TGLHDRImage ------------------
// ------------------

// LoadFromFile
//
procedure TGLHDRImage.LoadFromFile(const  filename: string);
var
   fs : TStream;
begin
  if FileStreamExists(fileName) then begin
    fs:=CreateFileStream(fileName, fmOpenRead);
    try
      LoadFromStream(fs);
    finally
      fs.Free;
      ResourceName  := filename;
    end;
  end
  else raise EInvalidRasterFile.CreateFmt('File %s not found', [filename]);
end;

procedure TGLHDRImage.LoadFromStream(stream : TStream);
const
  cRgbeFormat32bit = 'FORMAT=32-bit_rle_rgbe';
  cGamma = 'GAMMA=';
  cEXPOSURE = 'EXPOSURE=';
  cY = '-Y ';
var
  buf: array [0..1023] of AnsiChar;
  header : TStringList;
  s, sn: string;
  lineSize : integer;
  tempBuf, top, bottom : PByte;
  i, j, err: Integer;
  formatDefined: boolean;

  function CmpWord(const word: string): boolean;
  var l: Integer; ts: string;
  begin
    Result := false;
    ts := header.Strings[i];
    if Length(word)>Length(ts) then Exit;
    for l := 1 to Length(word) do
      if word[l]<>ts[l] then Exit;
    Result := true;
  end;

begin
  fProgramtype := '';
  fGamma := 1.0;
  fExposure := 1.0;
  // Read HDR header
  stream.Read(buf, Length(buf)*sizeOf(AnsiChar));
  header := TStringList.Create;
  s:=''; i:=0; j:=0;
  while i<Length(buf) do
  begin
    if buf[i]=#0 then Break;
    if buf[i]=#10 then
    begin
      header.Add(s);
      s:='';
      Inc(i); j:=i;
      Continue;
    end;
    s:=s+String(buf[i]);
    Inc(i);
  end;
  if i<Length(buf) then stream.Position := j
  else raise EInvalidRasterFile.Create('Can''t find HDR header end.');

  if (header.Strings[0][1] <> '#') or (header.Strings[0][2] <> '?') then
  begin
    header.Free;
    raise EInvalidRasterFile.Create('Bad HDR initial token.');
  end;
  // Get program type
  SetProgramtype( AnsiString(Copy(header.Strings[0], 3, Length(header.Strings[0])-2)) );

  formatDefined := false;
  for i := 1 to header.Count - 1 do
  begin
    if header.Strings[i] = cRgbeFormat32bit then formatDefined := true
    else if CmpWord(cGamma) then
    begin
      j := Length(cGamma);
      s := Copy(header.Strings[i], j+1, Length(header.Strings[i])-j);
      val(s, fGamma, err);
    end
    else if CmpWord(cEXPOSURE) then
    begin
      j := Length(cEXPOSURE);
      s := Copy(header.Strings[i], j+1, Length(header.Strings[i])-j);
      val(s, fExposure, err);
    end
    else if CmpWord(cY) then
    begin
      j := Length(cY);
      s := Copy(header.Strings[i], j+1, Length(header.Strings[i])-j);
      j := Pos(' ', s);
      sn := Copy(s, 1, j-1);
      val(sn, fHeight, err);
      Delete(s, 1, j+3); // scip '+X '
      val(s, fWidth, err);
    end
  end; // for i
  header.Free;

  if not formatDefined then
    raise EInvalidRasterFile.Create('no FORMAT specifier found.');

  if (fWidth=0) or (fHeight=0) then
    raise EInvalidRasterFile.Create('Bad image dimension.');
  //set all the parameters
  fDepth          := 0;
  fMipLevels      := 1;
  fColorFormat    := GL_RGB;
  fInternalFormat := tfRGBA_FLOAT32;
  fDataType       := GL_FLOAT;
  fCubeMap        := false;
  fTextureArray   := false;
  fLevels.Clear;  fLevels.Add(nil);
  fElementSize    := GetTextureElementSize(tfFLOAT_RGB32);
  ReallocMem( fData, DataSize);
  LoadRLEpixels( stream, PSingle(fData), fWidth, fHeight);

  //hdr images come in upside down then flip it
  lineSize := fElementSize * fWidth;
  GetMem( tempBuf, lineSize);
  top := PByte( fData );
  bottom := top;
  Inc( bottom, lineSize * (fHeight - 1) );
  for j := 0 to (height div 2)-1 do
  begin
    Move( top^,  tempBuf^, lineSize);
    Move( bottom^, top^, lineSize);
    Move( tempBuf^, bottom^, lineSize);
    Inc( top, lineSize);
    Dec( bottom, lineSize);
  end;
  FreeMem( tempBuf );
end;

function  TGLHDRImage.GetProgramType: Ansistring;
begin
  Result := fProgramType;
end;

procedure TGLHDRImage.SetProgramType(aval: Ansistring);
var
  i: integer;
begin
  for i := 1 to Length(fProgramType) do fProgramType[i] := aval[i];
end;

// AssignFromTexture
//
procedure TGLHDRImage.AssignFromTexture(textureContext: TGLContext;
                                        const textureHandle: TGLenum;
                                        textureTarget: TGLenum;
                                        const CurrentFormat: Boolean;
                                        const intFormat: TGLInternalFormat);
var
  oldContext : TGLContext;
  contextActivate : Boolean;
  texFormat, texResident: Cardinal;
  residentFormat : TGLInternalFormat;

begin
  if not ((textureTarget=GL_TEXTURE_2D)
  or (textureTarget=GL_TEXTURE_RECTANGLE)) then Exit;

  oldContext:=CurrentGLContext;
  contextActivate:=(oldContext<>textureContext);
  if contextActivate then
  begin
    if Assigned(oldContext) then oldContext.Deactivate;
    textureContext.Activate;
  end;

  try
    textureContext.GLStates.SetGLCurrentTexture(0, textureTarget, textureHandle);
    //Check for texture is resident in texture memory
    glGetTexParameteriv(textureTarget, GL_TEXTURE_RESIDENT, @texResident);
    if texResident=GL_TRUE then
    begin
      fMipLevels := 0;
      fCubeMap  := false;
      fTextureArray := false;
      fColorFormat := GL_RGB;
      fDataType := GL_FLOAT;
      // Check level existence
      glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_INTERNAL_FORMAT, @texFormat);
      if texFormat > 1 then
      begin
        glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_WIDTH, @fWidth);
        glGetTexLevelParameteriv(textureTarget, 0, GL_TEXTURE_HEIGHT, @fHeight);
        fDepth:=0;
        residentFormat := OpenGLFormatToInternalFormat( texFormat );
        if CurrentFormat then
          fInternalFormat := residentFormat
        else
          fInternalFormat := intFormat;
        Inc(fMipLevels);
      end;
      if fMipLevels>0 then
      begin
        fElementSize := GetTextureElementSize(fColorFormat, fDataType);
        ReallocMem(FData, DataSize);
        fLevels.Clear;
        fLevels.Add(fData);
        glGetTexImage(textureTarget, 0, fColorFormat, fDataType, fData);
      end
      else fMipLevels:=1;
    end;
    CheckOpenGLError;
  finally
    if contextActivate then
    begin
      textureContext.Deactivate;
      if Assigned(oldContext) then oldContext.Activate;
    end;
  end;
end;

// Capabilities
//
class function TGLHDRImage.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead{, dfcWrite}];
end;

initialization
   { Register this Fileformat-Handler with GLScene }
   RegisterRasterFormat('hdr','High Dynamic Range Image', TGLHDRImage);

end.
