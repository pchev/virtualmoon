//
// This unit is part of the GLScene Project, http://glscene.org
//
{: ApplicationFileIO<p>

   Components and fonction that abstract file I/O access for an application.<br>
   Allows re-routing file reads to reads from a single archive file f.i.<p>

 <b>History : </b><font size=-1><ul>
      <li>10/11/12 - PW - Added CPPB compatibility: used TAFIOFileStreamEvent as procedure
                     instead of function for GLS_CPPB
      <li>25/08/10 - DaStr - Fixed compiler warnings
      <li>25/07/10 - Yar - Added TGLSResourceStream class and CreateResourceStream string
      <li>23/01/10 - Yar - Change LoadFromStream to dynamic
      <li>29/01/07 - DaStr - Moved registration to GLSceneRegister.pas
      <li>02/08/04 - LR, YHC - BCB corrections: fixed BCB Compiler error "E2370 Simple type name expected"
      <li>05/06/03 - EG - TDataFile moved in from GLMisc
      <li>31/01/03 - EG - Added FileExists mechanism
      <li>21/11/02 - EG - Creation
 </ul></font>
}
unit ApplicationFileIO;

interface

{$I GLScene.inc}

uses
  Classes,
  SysUtils,
  BaseClasses
  {$IFDEF FPC}
  ,
  LResources
  {$ELSE}
  ,
  Windows
  {$ENDIF}
  {$IFDEF GLS_LOGGING}, GLSLog {$ENDIF};


const
  GLS_RC_DDS_Type = {$IFNDEF FPC}RT_RCDATA{$ELSE} 'DDS'{$ENDIF};
  GLS_RC_JPG_Type = {$IFNDEF FPC}RT_RCDATA{$ELSE} 'JPG'{$ENDIF};
  GLS_RC_XML_Type = {$IFNDEF FPC}RT_RCDATA{$ELSE} 'XML'{$ENDIF};
  GLS_RC_String_Type = {$IFNDEF FPC}RT_RCDATA{$ELSE} 'STR'{$ENDIF};

type

  TGLSApplicationResource = (
    aresNone,
    aresSplash,
    aresTexture,
    aresMaterial,
    aresSampler,
    aresFont,
    aresMesh);

  // TAFIOCreateFileStream
  //
  TAFIOCreateFileStream = function(const fileName: string; mode: Word): TStream;

  // TAFIOFileStreamExists
  //
  TAFIOFileStreamExists = function(const fileName: string): Boolean;

  // TAFIOFileStreamEvent
  //
   TAFIOFileStreamEvent = procedure (const fileName : String; mode : Word;var stream : TStream) of object;

  // TAFIOFileStreamExistsEvent
  //
  TAFIOFileStreamExistsEvent = function(const fileName: string): Boolean of object;

  // TApplicationFileIO
  //
    {: Allows specifying a custom behaviour for ApplicationFileIO's CreateFileStream.<p>
       The component should be considered a helper only, you can directly specify
       a function via the vAFIOCreateFileStream variable.<br>
       If multiple TApplicationFileIO components exist in the application,
       the last one created will be the active one. }
  TApplicationFileIO = class(TComponent)
  private
    { Private declarations }
    FOnFileStream: TAFIOFileStreamEvent;
    FOnFileStreamExists: TAFIOFileStreamExistsEvent;

  protected
    { Protected declarations }

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
      {: Event that allows you to specify a stream for the file.<p>
         Destruction of the stream is at the discretion of the code that
         invoked CreateFileStream. Return nil to let the default mechanism
         take place (ie. attempt a regular file system access). }
    property OnFileStream: TAFIOFileStreamEvent read FOnFileStream write FOnFileStream;
    {: Event that allows you to specify if a stream for the file exists.<p> }
    property OnFileStreamExists: TAFIOFileStreamExistsEvent read FOnFileStreamExists write FOnFileStreamExists;
  end;

  // TDataFileCapabilities
  //
  TDataFileCapability = (dfcRead, dfcWrite);
  TDataFileCapabilities = set of TDataFileCapability;

  // TDataFile
  //
  {: Abstract base class for data file formats interfaces.<p>
     This class declares base file-related behaviours, ie. ability to load/save
     from a file or a stream.<p>
     It is highly recommended to overload ONLY the stream based methods, as the
     file-based one just call these, and stream-based behaviours allow for more
     enhancement (such as other I/O abilities, compression, cacheing, etc.)
     to this class, without the need to rewrite subclasses. }
  TDataFile = class(TGLUpdateAbleObject)
  private
    { Private Declarations }
    FResourceName: string;
    procedure SetResourceName(const AName: string);
  public
    { Public Declarations }

    {: Describes what the TDataFile is capable of.<p>
       Default value is [dfcRead]. }
    class function Capabilities: TDataFileCapabilities; virtual;

    {: Duplicates Self and returns a copy.<p>
       Subclasses should override this method to duplicate their data. }
    function CreateCopy(AOwner: TPersistent): TDataFile; dynamic;

    procedure LoadFromFile(const fileName: string); dynamic;
    procedure SaveToFile(const fileName: string); dynamic;
    procedure LoadFromStream(stream: TStream); dynamic;
    procedure SaveToStream(stream: TStream); dynamic;
    procedure Initialize; dynamic;
    {: Optionnal resource name.<p>
       When using LoadFromFile/SaveToFile, the filename is placed in it,
       when using the Stream variants, the caller may place the resource
       name in it for parser use. }
    property ResourceName: string read FResourceName write SetResourceName;
  end;

  TDataFileClass = class of TDataFile;
  TGLSResourceStream = {$IFNDEF FPC}TResourceStream{$ELSE}TLazarusResourceStream{$ENDIF};

  //: Returns true if an ApplicationFileIO has been defined
function ApplicationFileIODefined: Boolean;

{: Creates a file stream corresponding to the fileName.<p>
   If the file does not exists, an exception will be triggered.<br>
   Default mechanism creates a regular TFileStream, the 'mode' parameter
   is similar to the one for TFileStream. }
function CreateFileStream(const fileName: string;
  mode: Word = fmOpenRead + fmShareDenyNone): TStream;
{: Queries is a file stream corresponding to the fileName exists.<p> }
function FileStreamExists(const fileName: string): Boolean;

{: Create a resource stream. }
function CreateResourceStream(const ResName: string; ResType: PChar): TGLSResourceStream;

function StrToGLSResType(const AStrRes: string): TGLSApplicationResource;

var
  vAFIOCreateFileStream: TAFIOCreateFileStream = nil;
  vAFIOFileStreamExists: TAFIOFileStreamExists = nil;

  // ---------------------------------------------------------------------
  // ---------------------------------------------------------------------
  // ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

var
  vAFIO: TApplicationFileIO = nil;

// ApplicationFileIODefined
//

function ApplicationFileIODefined: Boolean;
begin
  Result := (Assigned(vAFIOCreateFileStream) and Assigned(vAFIOFileStreamExists))
    or Assigned(vAFIO);
end;

// CreateFileStream
//

function CreateFileStream(const fileName: string;
  mode: Word = fmOpenRead + fmShareDenyNone): TStream;
begin
  if Assigned(vAFIOCreateFileStream) then
    Result := vAFIOCreateFileStream(fileName, mode)
  else
  begin
      Result:=nil;
      if Assigned(vAFIO) and Assigned(vAFIO.FOnFileStream) then
         vAFIO.FOnFileStream(fileName, mode, Result);
      if not Assigned(Result) then begin
         if ((mode and fmCreate)=fmCreate) or FileExists(fileName) then
            Result:=TFileStream.Create(fileName, mode)
         else raise Exception.Create('File not found: "'+fileName+'"');
      end;
   end;
end;

// FileStreamExists
//

function FileStreamExists(const fileName: string): Boolean;
begin
  if Assigned(vAFIOFileStreamExists) then
    Result := vAFIOFileStreamExists(fileName)
  else
  begin
    if Assigned(vAFIO) and Assigned(vAFIO.FOnFileStreamExists) then
      Result := vAFIO.FOnFileStreamExists(fileName)
    else
      Result := FileExists(fileName);
  end;
end;

// FileStreamExists
//

function CreateResourceStream(const ResName: string; ResType: PChar): TGLSResourceStream;
{$IFNDEF FPC}
var
  InfoBlock: HRSRC;
{$ELSE}
  {$ifndef ver2_2}
var
  FPResource: TFPResourceHandle;
  function IsResourceExist: Boolean;
  begin
    FPResource := FindResource(HInstance, PChar(ResName), ResType);
    Result := FPResource <> 0;
  end;
  {$ENDIF}
{$ENDIF}
begin
  Result := nil;
{$IFNDEF FPC}
  InfoBlock := FindResource(HInstance, PChar(ResName), ResType);
  if InfoBlock <> 0 then
    Result := TResourceStream.Create(HInstance, ResName, ResType)
{$ELSE}
  if LazarusResources.Find(ResName, ResType) <> nil then
    Result := TLazarusResourceStream.Create(ResName, ResType)
  {$ifndef ver2_2}
  else if IsResourceExist then
    Result := TLazarusResourceStream.CreateFromHandle(HInstance, FPResource)
  {$ENDIF}
{$ENDIF}
  else
    GLSLogger.LogError(Format('Can''t create stream of application resource "%s"', [ResName]));
end;

// ------------------
// ------------------ TApplicationFileIO ------------------
// ------------------

// Create
//

constructor TApplicationFileIO.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  vAFIO := Self;
end;

// Destroy
//

destructor TApplicationFileIO.Destroy;
begin
  vAFIO := nil;
  inherited Destroy;
end;


// ------------------
// ------------------ TDataFile ------------------
// ------------------

// Capabilities
//
class function TDataFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead];
end;


// CreateCopy
//
function TDataFile.CreateCopy(AOwner: TPersistent): TDataFile;
begin
  if Self <> nil then
    Result := TDataFileClass(Self.ClassType).Create(AOwner)
  else
    Result := nil;
end;

// LoadFromFile
//

procedure TDataFile.LoadFromFile(const fileName: string);
var
  fs: TStream;
begin
  ResourceName := ExtractFileName(fileName);
  fs := CreateFileStream(fileName, fmOpenRead + fmShareDenyNone);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

// SaveToFile
//

procedure TDataFile.SaveToFile(const fileName: string);
var
  fs: TStream;
begin
  ResourceName := ExtractFileName(fileName);
  fs := CreateFileStream(fileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

// LoadFromStream
//

procedure TDataFile.LoadFromStream(stream: TStream);
begin
  Assert(False, 'Imaport for ' + ClassName + ' to ' + stream.ClassName + ' not available.');
end;

// SaveToStream
//

procedure TDataFile.SaveToStream(stream: TStream);
begin
  Assert(False, 'Export for ' + ClassName + ' to ' + stream.ClassName + ' not available.');
end;

procedure TDataFile.Initialize;
begin
end;

procedure TDataFile.SetResourceName(const AName: string);
begin
  FResourceName := AName;
end;

function StrToGLSResType(const AStrRes: string): TGLSApplicationResource;
begin
  if AStrRes = '[SAMPLERS]' then
  begin
    Result := aresSampler;
  end
  else if AStrRes = '[TEXTURES]' then
  begin
    Result := aresTexture;
  end
  else if AStrRes = '[MATERIALS]' then
  begin
    Result := aresMaterial;
  end
  else if AStrRes = '[STATIC MESHES]' then
  begin
    Result := aresMesh;
  end
  else if AStrRes = '[SPLASH]' then
  begin
    Result := aresSplash;
  end
  else if AStrRes = '[FONTS]' then
  begin
    Result := aresFont;
  end
  else
    Result := aresNone;
end;

end.

