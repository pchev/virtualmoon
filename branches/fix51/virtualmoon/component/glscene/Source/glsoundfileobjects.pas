//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSoundFileFormat<p>

	Support classes for loading various fileformats.<p>
   These classes work together like vector file formats or Delphi's TGraphic classes.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>16/10/08 - UweR - Compatibility fix for Delphi 2009
      <li>07/06/07 - DaStr - Added $I GLScene.inc
      <li>26/01/05 - JAJ - Removed leak formed by never freeing vSoundFileFormats.
                            Reported by Dikoe Kenguru.
      <li>16/03/01 - Egg - TGLWAVFile.Capabilities
      <li>16/07/00 - Egg - Made use of new TDataFile class
      <li>09/06/00 - Egg - Added WAVDataSize
      <li>04/06/00 - Egg - Creation
	</ul></font>
}
unit GLSoundFileObjects;

interface

{$I GLScene.inc}

uses Classes, ApplicationFileIO;

type

	// TGLSoundSampling
	//
   {: Defines a sound sampling quality. }
	TGLSoundSampling = class (TPersistent)
	   private
	      { Private Declarations }
         FOwner : TPersistent;
         FFrequency : Integer;
         FNbChannels : Integer;
         FBitsPerSample : Integer;

	   protected
	      { Protected Declarations }
         function GetOwner : TPersistent; override;

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TPersistent);
         destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         function BytesPerSec : Integer;
         function BytesPerSample : Integer;

         //function WaveFormat : TWaveFormatEx;

	   published
	      { Published Declarations }
         {: Sampling frequency in Hz (= samples per sec) }
         property Frequency : Integer read FFrequency write FFrequency default 22050;
         {: Nb of sampling channels.<p>
            1 = mono, 2 = stereo, etc. }
         property NbChannels : Integer read FNbChannels write FNbChannels default 1;
         {: Nb of bits per sample.<p>
            Common values are 8 and 16 bits. }
         property BitsPerSample : Integer read FBitsPerSample write FBitsPerSample default 8;
	end;

   // TGLSoundFile
   //
   {: Abstract base class for different Sound file formats.<p>
      The actual implementation for these files (WAV, RAW...) must be done
      seperately. The concept for TGLSoundFile is very similar to TGraphic
      (see Delphi Help).<p>
      Default implementation for LoadFromFile/SaveToFile are to directly call the
      relevent stream-based methods, ie. you will just have to override the stream
      methods in most cases. }
   TGLSoundFile = class (TDataFile)
      private
         { Private Declarations }
         FSampling : TGLSoundSampling;

      protected
         { Protected Declarations }
         procedure SetSampling(const val : TGLSoundSampling);

      public
         { Public Declarations }
	      constructor Create(AOwner: TPersistent); override;
         destructor Destroy; override;

         procedure PlayOnWaveOut; dynamic;

         {: Returns a pointer to the sample data viewed as an in-memory WAV File. }
	      function WAVData : Pointer; virtual; abstract;
         {: Returns the size (in bytes) of the WAVData. }
         function WAVDataSize : Integer; virtual; abstract;
         {: Returns a pointer to the sample data viewed as an in-memory PCM buffer. }
	      function PCMData : Pointer; virtual; abstract;
         {: Length of PCM data, in bytes. }
	      function LengthInBytes : Integer; virtual; abstract;
         {: Nb of intensity samples in the sample. }
	      function LengthInSamples : Integer;
         {: Length of play of the sample at nominal speed in seconds. }
	      function LengthInSec : Single;

         property Sampling : TGLSoundSampling read FSampling write SetSampling;
   end;

   TGLSoundFileClass = class of TGLSoundFile;
{$MESSAGE Warning 'Temporarily disabled TGLWAVFile to get this unit compile in linux' }
(*
   // TGLWAVFile
   //
   {: Support for Windows WAV format. }
   TGLWAVFile = class (TGLSoundFile)
      private
         { Public Declarations }
         waveFormat : TWaveFormatEx;
         pcmOffset : Integer;
         data : array of Byte; // used to store WAVE bitstream

      protected
         { Protected Declarations }

      public
         { Private Declarations }
         function CreateCopy(AOwner: TPersistent) : TDataFile; override;

         class function Capabilities : TDataFileCapabilities; override;

         procedure LoadFromStream(Stream: TStream); override;
         procedure SaveToStream(Stream: TStream); override;

         procedure PlayOnWaveOut; override;

	      function WAVData : Pointer; override;
         function WAVDataSize : Integer; override;
	      function PCMData : Pointer; override;
	      function LengthInBytes : Integer; override;
   end;
*)
   // TGLGenericSoundFile
   //
   {: Support for generic sound files, as most libs detect filetype themselfs anyway.<p>
      *Partial* support only, access to PCMData is NOT supported. }
   TGLGenericSoundFile = class (TGLSoundFile)
      private
         { Public Declarations }
         data : array of byte; // used to store full bitstream including header

      protected
         { Protected Declarations }

      public
         { Private Declarations }
         function CreateCopy(AOwner: TPersistent) : TDataFile; override;

         class function Capabilities : TDataFileCapabilities; override;

         procedure LoadFromStream(Stream: TStream); override;
         procedure SaveToStream(Stream: TStream); override;

         procedure PlayOnWaveOut; override;

	      function WAVData : Pointer; override;
         function WAVDataSize : Integer; override;
	      function PCMData : Pointer; override;
	      function LengthInBytes : Integer; override;
   end;
   // TGLMP3File
   //
   {: Support for MP3 format.<p>
      *Partial* support only, access to PCMData is NOT supported. }
   TGLMP3File = class (TGLGenericSoundFile)
      public
         procedure PlayOnWaveOut; override;
   end;

   // TGLSoundFileFormat
   //
   TGLSoundFileFormat = record
      SoundFileClass : TGLSoundFileClass;
      Extension      : String;
      Description    : String;
      DescResID      : Integer;
   end;
   PSoundFileFormat = ^TGLSoundFileFormat;

   // TGLSoundFileFormatsList
   //
   TGLSoundFileFormatsList = class(TList)
      public
         { Public Declarations }
         destructor Destroy; override;
         procedure Add(const Ext, Desc: String; DescID: Integer; AClass: TGLSoundFileClass);
         function FindExt(Ext: string): TGLSoundFileClass;
         procedure Remove(AClass: TGLSoundFileClass);
         procedure BuildFilterStrings(SoundFileClass: TGLSoundFileClass; var Descriptions, Filters: string);
   end;

{$MESSAGE Warning 'Temporarily disabled WaveOut functions to get his unit compile in linux' }
(*
procedure PlayOnWaveOut(pcmData : Pointer; lengthInBytes : Integer;
                        sampling : TGLSoundSampling); overload;
function PlayOnWaveOut(pcmData : Pointer; lengthInBytes : Integer;
                        waveFormat : TWaveFormatEx) : HWaveOut; overload;
*)
function GetGLSoundFileFormats : TGLSoundFileFormatsList;
procedure RegisterSoundFileFormat(const AExtension, ADescription: String; AClass: TGLSoundFileClass);
procedure UnregisterSoundFileClass(AClass: TGLSoundFileClass);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, GLStrings{, consts};

{$MESSAGE Warning 'Temporarily disabled to get his unit compile in linux' }
(*
   TRIFFChunkInfo = packed record
      ckID : FOURCC;
      ckSize : LongInt;
   end;

const
  WAVE_Format_ADPCM = 2;
*)

var
   vSoundFileFormats : TGLSoundFileFormatsList;

// GeTGLSoundFileFormats
//
function GetGLSoundFileFormats : TGLSoundFileFormatsList;
begin
   if not Assigned(vSoundFileFormats)then
      vSoundFileFormats := TGLSoundFileFormatsList.Create;
   Result := vSoundFileFormats;
end;

// RegisterSoundFileFormat
//
procedure RegisterSoundFileFormat(const AExtension, ADescription: String; AClass: TGLSoundFileClass);
begin
   RegisterClass(AClass);
	GetGLSoundFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

// UnregisterSoundFileClass
//
procedure UnregisterSoundFileClass(AClass: TGLSoundFileClass);
begin
	if Assigned(vSoundFileFormats) then
		vSoundFileFormats.Remove(AClass);
end;

{$MESSAGE Warning 'Temporarily disabled to get his unit compile in linux' }
(*

procedure _waveOutCallBack(hwo : HWAVEOUT; uMsg : Cardinal;
                           dwInstance, dwParam1, dwParam2 : Integer); stdcall;
begin
   if uMsg=WOM_DONE then
      waveOutClose(hwo);
end;

// PlayOnWaveOut (sampling)
//
procedure PlayOnWaveOut(pcmData : Pointer; lengthInBytes : Integer;
                        sampling : TGLSoundSampling);
var
   wfx : TWaveFormatEx;
   hwo : hwaveout;
   wh : wavehdr;
   mmres : MMRESULT;
begin
   wfx:=sampling.WaveFormat;
   mmres:=waveOutOpen(@hwo, WAVE_MAPPER, @wfx, Cardinal(@_waveOutCallBack), 0, CALLBACK_FUNCTION);
   Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
   wh.dwBufferLength:=lengthInBytes;
   wh.lpData:=pcmData;
   wh.dwFlags:=0;
   wh.dwLoops:=1;
   wh.lpNext:=nil;
   mmres:=waveOutPrepareHeader(hwo, @wh, SizeOf(wavehdr));
   Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
   mmres:=waveOutWrite(hwo, @wh, SizeOf(wavehdr));
   Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
end;

// PlayOnWaveOut (waveformat)
//
function PlayOnWaveOut(pcmData : Pointer; lengthInBytes : Integer;
                       waveFormat : TWaveFormatEx) : HWaveOut;
var
   hwo : hwaveout;
   wh : wavehdr;
   mmres : MMRESULT;
begin
   mmres:=waveOutOpen(@hwo, WAVE_MAPPER, @waveFormat, Cardinal(@_waveOutCallBack),
                      0, CALLBACK_FUNCTION);
   Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
   wh.dwBufferLength:=lengthInBytes;
   wh.lpData:=pcmData;
   wh.dwFlags:=0;
   wh.dwLoops:=1;
   wh.lpNext:=nil;
   mmres:=waveOutPrepareHeader(hwo, @wh, SizeOf(wavehdr));
   Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
   mmres:=waveOutWrite(hwo, @wh, SizeOf(wavehdr));
   Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
   Result:=hwo;
end;
*)

// ------------------
// ------------------ TGLSoundSampling ------------------
// ------------------

// Create
//
constructor TGLSoundSampling.Create(AOwner: TPersistent);
begin
	inherited Create;
   FOwner:=AOwner;
   FFrequency:=22050;
   FNbChannels:=1;
   FBitsPerSample:=8;
end;

// Destroy
//
destructor TGLSoundSampling.Destroy;
begin
	inherited Destroy;
end;

// Assign
//
procedure TGLSoundSampling.Assign(Source: TPersistent);
begin
   if Source is TGLSoundSampling then begin
      FFrequency:=TGLSoundSampling(Source).Frequency;
      FNbChannels:=TGLSoundSampling(Source).NbChannels;
      FBitsPerSample:=TGLSoundSampling(Source).BitsPerSample;
   end else inherited;
end;

// GetOwner
//
function TGLSoundSampling.GetOwner : TPersistent;
begin
   Result:=FOwner;
end;

// BytesPerSec
//
function TGLSoundSampling.BytesPerSec : Integer;
begin
   Result:=(FFrequency*FBitsPerSample*FNbChannels) shr 3;
end;

// BytesPerSample
//
function TGLSoundSampling.BytesPerSample : Integer;
begin
   Result:=FBitsPerSample shr 3;
end;

// WaveFormat
//
{$MESSAGE Warning 'Temporarily disabled to get this unit compile in linux' }
(*
function TGLSoundSampling.WaveFormat : TWaveFormatEx;
begin
   Result.nSamplesPerSec:=Frequency;
   Result.nChannels:=NbChannels;
   Result.wFormatTag:=Wave_Format_PCM;
   Result.nAvgBytesPerSec:=BytesPerSec;
   Result.wBitsPerSample:=BitsPerSample;
   Result.nBlockAlign:=1024;
   Result.cbSize:=SizeOf(TWaveFormatEx);
end;
*)
// ------------------
// ------------------ TGLSoundFile ------------------
// ------------------

// Create
//
constructor TGLSoundFile.Create(AOwner: TPersistent);
begin
   inherited;
   FSampling:=TGLSoundSampling.Create(Self);
end;

// Destroy
//
destructor TGLSoundFile.Destroy;
begin
   FSampling.Free;
   inherited;
end;

// SetSampling
//
procedure TGLSoundFile.SetSampling(const val : TGLSoundSampling);
begin
   FSampling.Assign(val);
end;

// PlayOnWaveOut
//
procedure TGLSoundFile.PlayOnWaveOut;
begin
  {$MESSAGE Warning 'Temporarily disabled TGLWAVFile to get this unit compile in linux' }
  // GLSoundFileObjects.PlayOnWaveOut(PCMData, LengthInSamples, Sampling);
end;

// LengthInSamples
//
function TGLSoundFile.LengthInSamples : Integer;
var
   d : Integer;
begin
   d:=Sampling.BytesPerSample*Sampling.NbChannels;
   if d>0 then
   	Result:=LengthInBytes div d
   else Result:=0;
end;

// LengthInSec
//
function TGLSoundFile.LengthInSec : Single;
begin
	Result:=LengthInBytes/Sampling.BytesPerSec;
end;

{$MESSAGE Warning 'Temporarily disabled TGLWAVFile to get this unit compile in linux' }
(*
// ------------------
// ------------------ TGLWAVFile ------------------
// ------------------

// CreateCopy
//
function TGLWAVFile.CreateCopy(AOwner: TPersistent) : TDataFile;
begin
   Result:=inherited CreateCopy(AOwner);
   if Assigned(Result) then begin
      TGLWAVFile(Result).waveFormat:=waveFormat;
      TGLWAVFile(Result).data := Copy(data);
   end;
end;

// Capabilities
//
class function TGLWAVFile.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead, dfcWrite];
end;

// LoadFromStream
//
procedure TGLWAVFile.LoadFromStream(stream : TStream);
var
   ck : TRIFFChunkInfo;
   dw, bytesToGo, startPosition, totalSize : Integer;
   id : Cardinal;
   dwDataOffset, dwDataSamples : Integer;
begin
   // this WAVE loading code is an adaptation of the 'minimalist' sample from
   // the Microsoft DirectX SDK.
   Assert(Assigned(stream));
   dwDataOffset:=0;
   // Check RIFF Header
   startPosition:=stream.Position;
   stream.Read(ck, SizeOf(TRIFFChunkInfo));
   Assert((ck.ckID=mmioStringToFourCC('RIFF',0)), 'RIFF required');
   totalSize:=ck.ckSize+SizeOf(TRIFFChunkInfo);
   stream.Read(id, SizeOf(Integer));
   Assert((id=mmioStringToFourCC('WAVE',0)), 'RIFF-WAVE required');
   // lookup for 'fmt '
   repeat
      stream.Read(ck, SizeOf(TRIFFChunkInfo));
      bytesToGo:=ck.ckSize;
      if (ck.ckID = mmioStringToFourCC('fmt ',0)) then begin
         if waveFormat.wFormatTag=0 then begin
            dw:=ck.ckSize;
            if dw>SizeOf(TWaveFormatEx) then
               dw:=SizeOf(TWaveFormatEx);
            stream.Read(waveFormat, dw);
            bytesToGo:=ck.ckSize-dw;
         end;
         // other 'fmt ' chunks are ignored (?)
      end else if (ck.ckID = mmioStringToFourCC('fact',0)) then begin
         if (dwDataSamples = 0) and (waveFormat.wFormatTag = WAVE_Format_ADPCM) then begin
            stream.Read(dwDataSamples, SizeOf(LongInt));
            Dec(bytesToGo, SizeOf(LongInt));
         end;
         // other 'fact' chunks are ignored (?)
      end else if (ck.ckID = mmioStringToFourCC('data',0)) then begin
         dwDataOffset:=stream.Position-startPosition;
         Break;
      end;
      // all other sub-chunks are ignored, move to the next chunk
      stream.Seek(bytesToGo, soFromCurrent);
   until Stream.Position = 2048; // this should never be reached
   // Only PCM wave format is recognized
//   Assert((waveFormat.wFormatTag=Wave_Format_PCM), 'PCM required');
   // seek start of data
   pcmOffset:=dwDataOffset;
   SetLength(data, totalSize);
   stream.Position:=startPosition;
   if totalSize>0 then
      stream.Read(data[0], totalSize);
   // update Sampling data
   with waveFormat do begin
      Sampling.Frequency:=nSamplesPerSec;
      Sampling.NbChannels:=nChannels;
      Sampling.BitsPerSample:=wBitsPerSample;
   end;
end;

// SaveToStream
//
procedure TGLWAVFile.SaveToStream(stream: TStream);
begin
   if Length(data)>0 then
      stream.Write(data[0], Length(data));
end;

// PlayOnWaveOut
//
procedure TGLWAVFile.PlayOnWaveOut;
begin
   PlaySound(WAVData, 0, SND_ASYNC+SND_MEMORY);
//   GLSoundFileObjects.PlayOnWaveOut(PCMData, LengthInBytes, waveFormat);
end;

// WAVData
//
function TGLWAVFile.WAVData : Pointer;
begin
   if Length(data)>0 then
      Result:=@data[0]
   else Result:=nil;
end;

// WAVDataSize
//
function TGLWAVFile.WAVDataSize : Integer;
begin
   Result:=Length(data);
end;

// PCMData
//
function TGLWAVFile.PCMData : Pointer;
begin
   if Length(data)>0 then
      Result:=@data[pcmOffset]
   else Result:=nil;
end;

// LengthInBytes
//
function TGLWAVFile.LengthInBytes : Integer;
begin
   Result:=Length(data)-pcmOffset;
end;
*)

// ------------------
// ------------------ TGLMP3File ------------------
// ------------------

// CreateCopy
//
function TGLGenericSoundFile.CreateCopy(AOwner: TPersistent) : TDataFile;
begin
   Result:=inherited CreateCopy(AOwner);
   if Assigned(Result) then begin
      TGLGenericSoundFile(Result).data:=copy(data);
   end;
end;

// Capabilities
//
class function TGLGenericSoundFile.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead, dfcWrite];
end;

// LoadFromStream
//
procedure TGLGenericSoundFile.LoadFromStream(stream : TStream);
begin
   // MP3 isn't actually, just loaded directly...
   Assert(Assigned(stream));
   SetLength(data, stream.Size);
   if Length(data)>0 then
      stream.Read(data[0], Length(data));
end;

// SaveToStream
//
procedure TGLGenericSoundFile.SaveToStream(stream: TStream);
begin
   if Length(data)>0 then
      stream.Write(data[0], Length(data));
end;

// PlayOnWaveOut
//
procedure TGLGenericSoundFile.PlayOnWaveOut;
begin
   Assert(False, 'MP3 playback on WaveOut not supported.');
end;

// WAVData
//
function TGLGenericSoundFile.WAVData : Pointer;
begin
   if Length(data)>0 then
      Result:=@data[0]
   else Result:=nil;
end;

// WAVDataSize
//
function TGLGenericSoundFile.WAVDataSize : Integer;
begin
   Result:=Length(data);
end;

// PCMData
//
function TGLGenericSoundFile.PCMData : Pointer;
begin
   Result:=nil;
end;

// LengthInBytes
//
function TGLGenericSoundFile.LengthInBytes : Integer;
begin
   Result:=0;
end;

// ------------------
// ------------------ TGLMP3File ------------------
// ------------------

procedure TGLMP3File.PlayOnWaveOut;
begin
   Assert(False, 'MP3 playback on WaveOut not supported.');
end;

// ------------------
// ------------------ TGLSoundFileFormatsList ------------------
// ------------------

// Destroy
//
destructor TGLSoundFileFormatsList.Destroy;
var
   i : Integer;
begin
   for i:=0 to Count-1 do Dispose(PSoundFileFormat(Items[i]));
   inherited;
end;

// Add
//
procedure TGLSoundFileFormatsList.Add(const Ext, Desc: String; DescID: Integer;
                                     AClass: TGLSoundFileClass);
var
   newRec: PSoundFileFormat;
begin
   New(newRec);
   with newRec^ do begin
      Extension := AnsiLowerCase(Ext);
      SoundFileClass := AClass;
      Description := Desc;
      DescResID := DescID;
   end;
   inherited Add(NewRec);
end;

// FindExt
//
function TGLSoundFileFormatsList.FindExt(Ext: string): TGLSoundFileClass;
var
   i : Integer;
begin
   Ext := AnsiLowerCase(Ext);
   for I := Count-1 downto 0 do with PSoundFileFormat(Items[I])^ do
      if (Extension = Ext) or ('.'+Extension = Ext) then begin
         Result := SoundFileClass;
         Exit;
      end;
   Result := nil;
end;

// Remove
//
procedure TGLSoundFileFormatsList.Remove(AClass: TGLSoundFileClass);
var
   i : Integer;
   p : PSoundFileFormat;
begin
   for I := Count-1 downto 0 do begin
      P := PSoundFileFormat(Items[I]);
      if P^.SoundFileClass.InheritsFrom(AClass) then begin
         Dispose(P);
         Delete(I);
      end;
   end;
end;

// BuildFilterStrings
//
procedure TGLSoundFileFormatsList.BuildFilterStrings(SoundFileClass: TGLSoundFileClass;
                                                    var Descriptions, Filters: string);
var
   c, i : Integer;
   p    : PSoundFileFormat;
begin
   Descriptions := '';
   Filters := '';
   C := 0;
   for I := Count-1 downto 0 do begin
      P := PSoundFileFormat(Items[I]);
      if P^.SoundFileClass.InheritsFrom(SoundFileClass) and (P^.Extension <> '') then
         with P^ do begin
            if C <> 0 then begin
               Descriptions := Descriptions+'|';
               Filters := Filters+';';
            end;
            if (Description = '') and (DescResID <> 0) then
               Description := LoadStr(DescResID);
            FmtStr(Descriptions, '%s%s (*.%s)|*.%2:s',
                   [Descriptions, Description, Extension]);
            FmtStr(Filters, '%s*.%s', [Filters, Extension]);
            Inc(C);
         end;
   end;
   if C > 1 then
      FmtStr(Descriptions, '%s (%s)|%1:s|%s', ['All', Filters, Descriptions]);//k00m
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
{$MESSAGE Warning 'Temporarily disabled TGLWAVFile to get this unit compile in linux' }
//  RegisterSoundFileFormat('wav', 'Windows WAV files', TGLWAVFile);
  RegisterSoundFileFormat('wav', 'Windows WAV files', TGLGenericSoundFile);
  RegisterSoundFileFormat('mp3', 'MPEG Layer3 files', TGLMP3File);
finalization

  FreeAndNil(vSoundFileFormats);

end.

