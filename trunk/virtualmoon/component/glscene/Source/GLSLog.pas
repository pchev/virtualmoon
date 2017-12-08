//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  Activate GLS_LOGGING in "GLSCene.inc" to turn on inner GLScene logger.
  You may have only one instance of TGLSLogger
  To obtain it, call UserLog() function from any unit.

  History :  
   25/03/13 - DaStr - Added WriteInternalMessages and DisplayErrorDialogs options
   30/01/13 - DaStr - Added "save-old-logs" option
   09/01/13 - DaStr - Added Log buffering and auto-splitting options
                         Other misc changes.
   18/01/11 - Yar - Added message sending to IDE memo in design time
   07/01/10 - Yar - Added formated string logging
   29/11/10 - Yar - Added log raising in Linux
   04/11/10 - DaStr - Added Delphi5 and Delphi6 compatibility
                         Fixed unit description
   07/09/10 - Yar - Added Enabled property to TGLLogSession
   02/04/10 - Yar - Added properties TimeFormat, LogLevels to TGLSLogger
                       Added function UserLog.
                       GLS_LOGGING now only turn on inner GLScene logger
   24/03/10 - Yar - Added TGLSLogger component,
                       possibility to use a more than one of log,
                       limit the number of error messages
   06/03/10 - Yar - Added to GLScene
   

  (C) 2004-2007 George "Mirage" Bakhtadze.
  <a href="http://www.casteng.com">www.casteng.com</a>  
  The source code may be used under either MPL 1.1 or LGPL 2.1 license.
  See included license.txt file  
  Unit contains some text file related utilities and logging class
}

unit GLSLog;

interface

{$I GLScene.inc}

uses
{$IFDEF GLS_DELPHI_OR_CPPB}
  Windows,
{$ENDIF}
 Dialogs,
 Controls,
 StrUtils, Classes, SysUtils, GLCrossPlatform, SyncObjs
{$IFDEF MSWINDOWS} , ShellApi {$ENDIF}
{$IFDEF LINUX} , Process {$ENDIF};

type
  { : Levels of importance of log messages }
  TLogLevel = (lkDebug, lkInfo, lkNotice, lkWarning, lkError, lkFatalError);
  { : Log level setting type }
  TLogLevels = set of TLogLevel;

  { What to do when number of messages exceeds message limit. }
  TLogMessageLimitAction = (mlaContinue, mlaStopLogging, mlaHalt);

var
  llMessageLimit: array [TLogLevel] of Integer = (MaxInt, MaxInt, MaxInt,
    500, 100, 10);

  lkPrefix: array [TLogLevel] of string = (' (D)  ', ' (i)  ', ' (M)  ',
    ' (W)  ', ' (Er)  ', ' (!!)  ');

const
  llMax: TLogLevels = [lkDebug, lkInfo, lkNotice, lkWarning, lkError, lkFatalError];
  llMedium: TLogLevels = [lkNotice, lkWarning, lkError, lkFatalError];
  llMin: TLogLevels = [lkError, lkFatalError];

type
  { : Log date and time setting type }
  TLogTimeFormat = (
    { : doesn't output any time information }
    lfNone,

    { : include date in the log }
    lfDate,

    { : include time in the log }
    lfTime,

    { : include time in the log, including milliseconds }
    lfTimeExact,

    { : include date and time in the log }
    lfDateTime,

    { : include time elapsed since startup in the log }
    lfElapsed);

  { How log is buffered. }
  TLogBufferingMode =
  (
   lbmWriteEmidiatly,
   lbmWritePeriodically,
   lbmWriteInTheEnd
  );

  { : Class reference to log session class }
  CLogSession = class of TGLLogSession;
  TGLLogSession = class;

  { Thread that periodically flushes the buffer to disk. }
  TLogBufferFlushThread = class(TThread)
  private
    FParent: TGLLogSession;
  protected
    procedure Execute; override;
  public
    constructor Create(const AParent: TGLLogSession);
  end;

  { Thread that checks file size and splits the file if nessesary. }
  TLogCheckSizeThread = class(TThread)
  private
    FParent: TGLLogSession;
  protected
    procedure Execute; override;
  public
    constructor Create(const AParent: TGLLogSession);
  end;

  { Abstract Logger class }
  TGLLogSession = class(TPersistent)
  private
    FBuffer: TStringList;
    FBuffered: Boolean;
    FBufferProcessingThread: TLogBufferFlushThread;
    FCheckLogSizeThread: TLogCheckSizeThread;
    FFlushBufferPeriod: Integer;
    FLogFile: Text; // TextFile.
    FDestroying: Boolean;

    FOriginalLogFileName: string;   // Original name
    FCurrentLogFileName: string;    // Current log file, if original exceeded certain size limit.
    FUsedLogFileNames: TStringList; // List of all created log files.

    FLogLevels: TLogLevels;
    FEnabled: Boolean;
    FBufferCriticalSection: TCriticalSection;
    FFileAccessCriticalSection: TCriticalSection;
    FModeTitles: array [TLogLevel] of string;
    FLogKindCount: array [TLogLevel] of Integer;
    FLogThreadId: Boolean;
    FMessageLimitAction: TLogMessageLimitAction;
    { : Determines which date or time to include in the log }
    FTimeFormat: TLogTimeFormat;
    { : Startup timestamp in milliseconds }
    FStartedMs: Cardinal;
    FLogFileMaxSize: Integer;
    FCheckFileSizePeriod: Integer;
    FDisplayLogOnExitIfItContains: TLogLevels;
    FWriteInternalMessages: Boolean;
    FDisplayErrorDialogs: Boolean;

    procedure SetBuffered(const Value: Boolean);
    procedure SetMode(const NewMode: TLogLevels);
    procedure ChangeBufferedState();
    procedure SetEnabled(const Value: Boolean);
    procedure SetLogFileMaxSize(const Value: Integer);
  protected
    procedure PrintLogLevels();
    procedure PrintLogStatistics();
    function AttachLogFile(const AFileName: string; const AResetFile: Boolean = True): Boolean;
    procedure ClearLogsInTheSameDir();
    procedure BackUpOldLogs(const ACurrentLogFileName: string);
    procedure CreateNewLogFileIfNeeded();

    { : Appends a string to log. Thread-safe. }
    procedure AppendLog(const AString: string; const ALevel: TLogLevel; const ALogTime: Boolean = True);

    { Writes string to log. Returns True if everything went ok.}
    function DoWriteToLog(const AString: string): Boolean; virtual;

    { Writes FBuffer to log. Returns True if everything went ok.}
    function DoWriteBufferToLog(): Boolean; virtual;

    { Resets log. Returns True if everything went ok.}
    function DoResetLog: Boolean; virtual;
  public
  {$IFNDEF GLS_LOGGING}
      constructor OnlyCreate;
  {$ENDIF}
    { Initializes a log session with the specified log file name, time and level settings }
    constructor Init(const AFileName: string;
      const ATimeFormat: TLogTimeFormat; const ALevels: TLogLevels;
      const ALogThreadId: Boolean = True; const ABuffered: Boolean = False;
      const AMaxSize: Integer = 0; const ABackUpOldLogs: Boolean = False;
      const AClearOldLogs: Boolean = True; const AWriteInternalMessages: Boolean = True); virtual;

    { : Destructor }
    destructor Destroy; override;

    { : General Logging procedures }
    procedure Log(const Desc: string; const Level: TLogLevel = lkInfo);
    procedure LogAdv(const args: array of const; const ALevel: TLogLevel = lkError);
    procedure LogException(const E: Exception; const aFunctionName: string;
      const args: array of const; const ALevel: TLogLevel = lkError);

    { : Logs a string  Desc  if  Level 
      matches current GLS_LOGGING level (see @Link(LogLevels)) }
    procedure LogDebug(const Desc: string);
    procedure LogInfo(const Desc: string);
    procedure LogNotice(const Desc: string);
    procedure LogWarning(const Desc: string);
    procedure LogError(const Desc: string);
    procedure LogFatalError(const Desc: string);
    procedure LogEmtryLine();

    { : Logs a formatted string assembled from a format string and an array of arguments. }
    procedure LogDebugFmt(const Desc: string; const Args: array of const );
    procedure LogInfoFmt(const Desc: string; const Args: array of const );
    procedure LogNoticeFmt(const Desc: string; const Args: array of const );
    procedure LogWarningFmt(const Desc: string; const Args: array of const );
    procedure LogErrorFmt(const Desc: string; const Args: array of const );
    procedure LogFatalErrorFmt(const Desc: string; const Args: array of const );

    { : Mics procedures. }
    procedure DisplayLog();
    procedure FlushBuffer(); // If log is buffered, calling this will flush the buffer.

    { : Set of levels which to include in the log }
    property LogLevels: TLogLevels read FLogLevels write SetMode
      default [lkDebug, lkInfo, lkNotice, lkWarning, lkError, lkFatalError];
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Buffered: Boolean read FBuffered write SetBuffered default False;
    property FlushBufferPeriod: Integer read FFlushBufferPeriod write FFlushBufferPeriod default 5000; // In ms.
    property LogThreadId: Boolean read FLogThreadId write FLogThreadId default True;
    property DisplayErrorDialogs: Boolean read FDisplayErrorDialogs write FDisplayErrorDialogs default True;
    property MessageLimitAction: TLogMessageLimitAction read FMessageLimitAction write FMessageLimitAction default mlaHalt;
    property WriteInternalMessages: Boolean read FWriteInternalMessages write FWriteInternalMessages default True;

    { To always display log, put all log types. To never display log, leave this empty. }
    property DisplayLogOnExitIfItContains: TLogLevels read FDisplayLogOnExitIfItContains write FDisplayLogOnExitIfItContains
      default [lkDebug, lkInfo, lkNotice, lkWarning, lkError, lkFatalError];


    { If LogFileMaxSize is not 0, then:
       1) At start, all logs with the same extention will be deleted.
       2) All logs wil be periodically cheked for FileSize.
          New log file will be created when this size exceeds limit.
    }
    property LogFileMaxSize: Integer  read FLogFileMaxSize  write SetLogFileMaxSize default 0; // In bytes, limited to 2Gb.
    property CheckFileSizePeriod: Integer read FCheckFileSizePeriod write FCheckFileSizePeriod default 4000; // In ms.
  end;

  // TGLSLoger
  //

  { : Abstract class for control loging. }

  TGLSLogger = class(TComponent)
  private
     
    FReplaceAssertion: Boolean;
    FTimeFormat: TLogTimeFormat;
    FLogLevels: TLogLevels;
    FLog: TGLLogSession;
    procedure SetReplaceAssertion(Value: Boolean);
    function GetLog: TGLLogSession;
  protected
     

  public
     
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { : Set component primary and then UserLog return it's log }
    procedure DoPrimary;
    property Log: TGLLogSession read GetLog;
  published
     
    property ReplaceAssertion: Boolean read FReplaceAssertion
      write SetReplaceAssertion default False;
    { : Only design time sets. Define Log initial properties }
    property TimeFormat: TLogTimeFormat read FTimeFormat write FTimeFormat
      default lfElapsed;
    property LogLevels: TLogLevels read FLogLevels write FLogLevels
      default [lkDebug, lkInfo, lkNotice, lkWarning, lkError, lkFatalError];
  end;

  TIDELogProc = procedure(const AMsg: string);

  { : Return logger wich created by TGLSLogger component }
function UserLog: TGLLogSession;
function SkipBeforeSTR(var TextFile: Text; SkipSTR: string): Boolean;
function ReadLine(var TextFile: Text): string;

{ : GLScene inner logger.
    DaStr: Converted to a function, because in case of a DLL and main app using this module,
    log is written to the same file on initialization and finalization,
    which is not what one might want. This also allows to create a GLSLogger with
    custom parameters for user's application, for example a different log path
    (Often the EXE application directory is read-only).
 }
function GLSLogger(): TGLLogSession;
procedure UseCustomGLSLogger(const ALogger: TGLLogSession);

function ConstArrayToString(const Elements: array of const): String;

var
  vIDELogProc: TIDELogProc;

implementation

var
  v_GLSLogger: TGLLogSession;
  vAssertErrorHandler: TAssertErrorProc;
  vCurrentLogger: TGLSLogger;

{ : GLScene inner logger. Create on first use, not in unit initialization. }
function GLSLogger(): TGLLogSession;
begin
  if v_GLSLogger = nil then
  begin
  {$IFDEF GLS_LOGGING}
    v_GLSLogger := TGLLogSession.Init(Copy(ExtractFileName(ParamStr(0)), 1,
    Length(ExtractFileName(ParamStr(0))) - Length(ExtractFileExt(ParamStr(0)))) +
    '.log', lfElapsed, llMax);
  {$ELSE}
    v_GLSLogger := TGLLogSession.OnlyCreate;
  {$ENDIF}
  end;

  Result := v_GLSLogger;
end;

procedure UseCustomGLSLogger(const ALogger: TGLLogSession);
begin
  if (v_GLSLogger <> nil) then v_GLSLogger.Destroy;
  v_GLSLogger := ALogger;
end;

const
//VarRec -> String
  vTypeDesc :     Array[0..16] of String = ('vtInteger','vtBoolean','vtChar',
                  'vtExtended','vtString','vtPointer','vtPChar','vtObject', 'vtClass',
                  'vtWideChar','vtPWideChar','vtAnsiString','vtCurrency','vtVariant',
                  'vtInterface','vtWideString','vtInt64');
  vTypeAsSring :  Array[0..17] of String = ('Integer     : ', 'Boolean     : ', 'Char        : ',
                  'Extended    : ', 'String      : ', 'Pointer     : ', 'PChar       : ',
                  'TObject     : ', 'Class       : ', 'WideChar    : ', 'PWideChar   : ',
                  'AnsiString  : ', 'Currency    : ', 'Variant     : ', 'Interface   : ',
                  'WideString  : ', 'Int64       : ', '#HLType     : ');

{ Function from HotLog by Olivier Touzot "QnnO".}
Function GetOriginalValue(s:String):String;
//  Called to remove the false 'AnsiString :' assertion, for pointers and objects
Begin
  result := RightStr(s,Length(s)-19);
End;

{ Function from HotLog by Olivier Touzot "QnnO".}
Function VarRecToStr(vr:TVarRec):String;
// See D6PE help topic "TVarRec"
Begin
  Result := vTypeAsSring[vr.VType] + ' ';
  TRY
    With vr Do
    Case VType of
       vtInteger:    result := result + IntToStr(VInteger);
       vtBoolean:    result := result + BoolToStr(VBoolean, True);
       vtChar:       Result := Result + string(VChar);
       vtExtended:   Result := Result + FloatToStr(VExtended^);
       vtString:     result := result + string(VString^);
// maintened in case of future need, but will actually not arrive.
       vtPointer:    result := result + '^(' +  Format('%P', [(addr(VPointer)) ]) +')';
       vtPChar:      result := Result + string(VPChar);
// ...
       vtObject:     Begin
                       If VObject = Nil Then result := result + '^(NIL)'
                       Else result := result + VObject.classname;
                     End;
// ...
       vtClass:      result := result + VClass.classname;
       vtWideChar:   Result := Result + string(VWideChar);
       vtPWideChar:  Result := Result + VPWideChar;
       vtAnsiString: Result := Result + string(VAnsiString);
       vtCurrency:   result := result + CurrToStr(VCurrency^);
       vtVariant:    Result := Result + string(VVariant^);
       vtInterface:  Result := Result + '(Interfaced object)';
       vtWideString: Result := Result + string(VWideString^);
       vtInt64:      Result := Result + IntToStr(VInt64^);
       else          result := result + Format('[#HLvrType(%d)]',       // "Else" not possible...
                               [ integer(vr.VType) ]);                  // ...with D6, but laters ?
    End;{case}
  EXCEPT
      result := result + Format('[#HLvrValue(%s)]', [vTypeDesc[vr.VType]]);
  END;
end;

{ Function from HotLog by Olivier Touzot "QnnO".}
Function GetBasicValue(s:String; vKind:Byte):String;
var iTmp : Integer;
    wasTObject: Boolean;
Begin
  Result := s;
  If s = '' Then exit;
  TRY
    iTmp := Pos('$_H_',s);
    wasTObject := (Pos('$_H_TObject',s) > 0);
    If (iTmp > 0 ) Then Result := GetOriginalValue(s);           // converts fake strings back to original
    Result := RightStr(Result, length(result)-15);               // From now on, works on "result"
    If (vKind In [vtString,vtAnsiString,vtWideString,vtPChar,vtWideChar,vtPWideChar])
    And Not(wasTObject) Then Exit
    Else Begin
           iTmp   := Pos(' ',Result);
           If ( iTmp > 0 ) And (iTmp < Length(result))
              Then result := LeftStr(result, iTmp);
         End;
  EXCEPT; END;
End;

{ Function from HotLog by Olivier Touzot "QnnO".}
function ConstArrayToString(const Elements: array of const): String;
// -2-> Returns � string, surrounded by parenthesis : '(elts[0]; ...; elts[n-1]);'
//      ("Basic infos" only.)
Var i: Integer;
    s,sep: String;
Begin
  TRY
    if Length(Elements) = 0 then
    begin
      Result := '';
      Exit;
    end;

    Result := '(';
    sep := '; ';
    For i:= Low(Elements) to High(Elements) do
    Begin
      s := VarRecToStr(Elements[I]);
      Result := Result + GetBasicValue(s,Elements[i].VType) + sep;
    End;
    Result := LeftStr(Result, length(result)-2) + ');' ;      // replaces last ", " by final ");".

  EXCEPT result := '[#HLvrConvert]';
  END;
End;


function UserLog: TGLLogSession;
begin
  if Assigned(vCurrentLogger) then
    Result := vCurrentLogger.Log
  else
    Result := nil;
end;

function RemovePathAndExt(const aFileName: string): string;
var
  lExtIndex: Integer;
begin
  Result := ExtractFileName(aFileName);
  lExtIndex := Pos(ExtractFileExt(Result), Result);
  Result := Copy(Result, 1, lExtIndex - 1);
end;



procedure LogedAssert(const Message, FileName: ShortString; LineNumber: Integer;
  ErrorAddr: Pointer);
begin
  UserLog.Log(Message + ': in ' + FileName + ' at line ' +
    IntToStr(LineNumber), lkError);
  Abort;
end;


function FileSize(const aFilename: String): Integer;
var
  sr : TSearchRec;
begin
  if FindFirst(aFilename, faAnyFile, sr ) = 0 then
  begin
    Result := sr.Size;
    FindClose(sr);
  end
  else
     Result := -1;
end;

function SkipBeforeSTR(var TextFile: Text; SkipSTR: string): Boolean;
var
  s: string;
begin
  repeat
    readln(TextFile, s);
    if s = SkipSTR then
    begin
      Result := True;
      Exit;
    end;
  until False;
  Result := False;
end;

function ReadLine(var TextFile: Text): string;
var
  i: Word;
var
  s: string;
begin
  if EOF(TextFile) then
    Exit;
  i := 1;
  repeat
    readln(TextFile, s);
  until (s <> '') and (s[1] <> '#') or EOF(TextFile);
  if s <> '' then
  begin
    while s[i] = ' ' do
      inc(i);
    if i = Length(s) then
      s := ''
    else
      s := Copy(s, i, Length(s) - i + 1);
  end;
  Result := s;
end;

// ------------------
// ------------------ TGLSLogger ------------------
// ------------------

constructor TGLSLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeFormat := lfElapsed;
  FLogLevels := llMax;
  vAssertErrorHandler := AssertErrorProc;
  vCurrentLogger := Self;
end;

destructor TGLSLogger.Destroy;
begin
  if vCurrentLogger = Self then
    vCurrentLogger := nil;
  if Assigned(FLog) then
    FLog.Destroy;
  inherited Destroy;
end;

function TGLSLogger.GetLog: TGLLogSession;
begin
  if not Assigned(FLog) then
    FLog := TGLLogSession.Init(Name + '.log', FTimeFormat, FLogLevels);
  Result := FLog;
end;

procedure TGLSLogger.DoPrimary;
begin
  vCurrentLogger := Self;
end;

procedure TGLSLogger.SetReplaceAssertion(Value: Boolean);
begin
  if Value <> FReplaceAssertion then
  begin
    FReplaceAssertion := Value;
    case FReplaceAssertion of
      True:
        AssertErrorProc := @LogedAssert;
      False:
        AssertErrorProc := @vAssertErrorHandler;
    end;
  end;
end;

// ------------------
// ------------------ TGLLogSession ------------------
// ------------------

procedure TGLLogSession.BackUpOldLogs(const ACurrentLogFileName: string);
var
  sRec: TSearchRec;
  lLogFileName: string;
  lLogOriginalDir: string;
  lLogSaveDir: string;
  lLogExt: string;

  procedure SaveCurrentFile();
  var
    lErrorMessage: string;
    lFile: File;
  begin
    if not FDisplayErrorDialogs then
      RenameFile(lLogOriginalDir + sRec.Name, lLogSaveDir + sRec.Name)
    else
    begin
      lErrorMessage := 'Renaming of "%s" failed with error : %d. Try again?';
      while not RenameFile(lLogOriginalDir + sRec.Name, lLogSaveDir + sRec.Name) do
      begin

        if MessageDlg(Format(lErrorMessage, [lLogOriginalDir + sRec.Name,
           GetLastOSError]), mtWarning, mbYesNo, -1) = mrNo then Break;

        AssignFile(lFile, lLogOriginalDir + sRec.Name);
        CloseFile(lFile);
      end;
    end;
  end;

begin
  lLogExt := ExtractFileExt(ACurrentLogFileName);
  lLogFileName := RemovePathAndExt(ACurrentLogFileName);
  lLogOriginalDir := ExtractFilePath(ACurrentLogFileName);
  lLogSaveDir := lLogOriginalDir + FormatDateTime('yyyy-mm-dd  hh-nn-ss', Now);

  if not CreateDir(lLogSaveDir) then Exit;
  lLogSaveDir := lLogSaveDir + SysUtils.PathDelim;
    
  If FindFirst(lLogOriginalDir + lLogFileName + '*' + lLogExt, faAnyfile, sRec) = 0 then
  begin
    try
      SaveCurrentFile();
    except
    end;

    while ( FindNext(sRec) = 0 ) do
    try
      SaveCurrentFile();
    except
    end;
    FindClose(sRec);
  end;
end;

procedure TGLLogSession.SetBuffered(const Value: Boolean);
begin
  if FBuffered = Value then Exit;
  FBuffered := Value;
  ChangeBufferedState();
end;

procedure TGLLogSession.SetEnabled(const Value: Boolean);
begin
  if (FEnabled = Value) then Exit;
  FEnabled := Value;
  if (FEnabled) then
    Log('Logging session resumed')
  else
    Log('Logging session paused');
end;

procedure TGLLogSession.SetLogFileMaxSize(const Value: Integer);
begin
  if FLogFileMaxSize = Value then Exit;
  FLogFileMaxSize := Value;

  if FLogFileMaxSize > 0 then
  begin
    FCheckLogSizeThread := TLogCheckSizeThread.Create(Self);
{$IFDEF GLS_DELPHI_2009_DOWN}
    FCheckLogSizeThread.Resume();
{$ELSE}
    FCheckLogSizeThread.Start();
{$ENDIF}
  end
  else
  begin
    FCheckLogSizeThread.Terminate();

    // DaStr: Not really safe because we can wait forever.
    // But other methods known to me are platform-dependant.
    FCheckLogSizeThread.WaitFor();

    FCheckLogSizeThread.Free();
  end;
end;

procedure TGLLogSession.SetMode(const NewMode: TLogLevels);
begin
{$IFNDEF GLS_LOGGING}
  if Self = v_GLSLogger then
    Exit;
{$ENDIF}
  FLogLevels := NewMode;
  PrintLogLevels();
end;

function TGLLogSession.DoResetLog: Boolean;
begin
  try
    FFileAccessCriticalSection.Enter;
    Rewrite(FLogFile);
    CloseFile(FLogFile);
    FFileAccessCriticalSection.Leave;
    Result := True;
  except on E: Exception do
    begin
      // Ignore exceptions.
      Result := False;
      FFileAccessCriticalSection.Leave;
    end;
  end;
end;

function TGLLogSession.DoWriteBufferToLog: Boolean;
var
  I: Integer;
  lLast: Integer;
begin
  try
    // Open file.
    FFileAccessCriticalSection.Enter;
    Append(FLogFile);

    // Write buffer.
    lLast := FBuffer.Count - 1;
    for I := 0 to lLast do
      WriteLn(FLogFile, FBuffer[I]);

    // Clear buffer.
    FBufferCriticalSection.Enter;
    FBuffer.Clear();
    FBufferCriticalSection.Leave;

    // Close file.
    CloseFile(FLogFile);
    FFileAccessCriticalSection.Release();

    Result := True;
  except
    // Ignore exceptions.
   Result := False;
   FFileAccessCriticalSection.Release();
  end;
end;

function TGLLogSession.DoWriteToLog(const AString: string): Boolean;
begin
  try
    FFileAccessCriticalSection.Enter;
    Append(FLogFile);
    WriteLn(FLogFile, AString);
    CloseFile(FLogFile);
    FFileAccessCriticalSection.Release();
    Result := True;
  except
    // Ignore exceptions.
    Result := False;
    FFileAccessCriticalSection.Release();
  end;
end;

procedure TGLLogSession.FlushBuffer;
begin
  if Buffered then
    DoWriteBufferToLog();
end;

constructor TGLLogSession.Init(const AFileName: string;
  const ATimeFormat: TLogTimeFormat; const ALevels: TLogLevels;
  const ALogThreadId: Boolean = True; const ABuffered: Boolean = False;
  const AMaxSize: Integer = 0; const ABackUpOldLogs: Boolean = False;
  const AClearOldLogs: Boolean = True; const AWriteInternalMessages: Boolean = True);
var
  i: Integer;
  ModeStr: string;
begin
  FBuffer := TStringList.Create();
  FLogThreadId := ALogThreadId;
  FFlushBufferPeriod := 5000; // 5 sec.
  FCheckFileSizePeriod := 4000; // 4 sec.
  FBufferCriticalSection := TCriticalSection.Create;
  FFileAccessCriticalSection := TCriticalSection.Create;
  FBuffered := ABuffered; // Do not call the setter, create thread later.
  FStartedMs := GLGetTickCount;
  FTimeFormat := ATimeFormat;
  FLogLevels := ALevels;
  FMessageLimitAction := mlaHalt;
  FDisplayErrorDialogs := True;
  FDisplayLogOnExitIfItContains := [lkError, lkFatalError];
  FWriteInternalMessages := AWriteInternalMessages;

  // Set up strings.
  FModeTitles[lkDebug] := 'debug info';
  FModeTitles[lkInfo] := 'info';
  FModeTitles[lkNotice] := 'notices';
  FModeTitles[lkWarning] := 'warnings';
  FModeTitles[lkError] := 'errors';
  FModeTitles[lkFatalError] := 'fatal errors';

  case FTimeFormat of
    lfNone:
      ModeStr := 'no timestamp mode.';
    lfDate:
      ModeStr := 'date only mode.';
    lfTime:
      ModeStr := 'time only mode.';
    lfTimeExact:
      ModeStr := 'time mode with milliseconds.';
    lfDateTime:
      ModeStr := 'date and time mode.';
    lfElapsed:
      ModeStr := 'elapsed time mode.';
  end;

  if ABackUpOldLogs then
    BackUpOldLogs(AFileName);

  // Attach log file.
  FUsedLogFileNames := TStringList.Create();
  FOriginalLogFileName := AFileName;
  FEnabled := AttachLogFile(AFileName, AClearOldLogs);

  // Clear all logs and set log max size.
  if AMaxSize > 0 then
    ClearLogsInTheSameDir();
  Self.SetLogFileMaxSize(AMaxSize);

  // Reset log counters.
  for i := Ord( Low(TLogLevel)) to Ord( High(TLogLevel)) do
    FLogKindCount[TLogLevel(i)] := 0;
   
  // Print some initial logs.
  if FWriteInternalMessages then
  begin
    Log('Log subsystem started in ' + ModeStr, lkInfo);
    PrintLogLevels();
    Log('Buffered mode: ' + BoolToStr(FBuffered, True), lkInfo);
  end;

  // Start BufferProcessing thread.
  if FBuffered then
    ChangeBufferedState();
end;

{$IFNDEF GLS_LOGGING}

constructor TGLLogSession.OnlyCreate;
begin
  inherited;
end;

{$ENDIF}

procedure TGLLogSession.PrintLogLevels;
var
  ModeStr: string;
  i: Integer;
begin
  ModeStr := '[';
  for i := Ord( Low(TLogLevel)) to Ord( High(TLogLevel)) do
    if TLogLevel(i) in FLogLevels then
    begin
      if ModeStr <> '[' then
        ModeStr := ModeStr + ', ';
      ModeStr := ModeStr + FModeTitles[TLogLevel(i)] + ' ' +
        Trim(lkPrefix[TLogLevel(i)]);
    end;
  ModeStr := ModeStr + ']';
  if FLogLevels = [] then
    ModeStr := 'nothing';
  Log('Logging ' + ModeStr, lkInfo);
end;

procedure TGLLogSession.PrintLogStatistics;
begin
  Log('Logged fatal_errors: ' + IntToStr(FLogKindCount[lkFatalError]) +
    ', errors: ' + IntToStr(FLogKindCount[lkError]) +
    ', warnings: ' + IntToStr(FLogKindCount[lkWarning]) +
    ', notices: ' + IntToStr(FLogKindCount[lkNotice]) +
    ', info: ' + IntToStr(FLogKindCount[lkInfo]) +
    ', debug: ' + IntToStr(FLogKindCount[lkDebug]));
end;

function TGLLogSession.AttachLogFile(const AFileName: string; const AResetFile: Boolean = True): Boolean;
var
  lPath: string;
begin
  try
    lPath := ExtractFilePath(AFileName);
    if Length(lPath) > 0 then
    begin
      FCurrentLogFileName := AFileName;
      ForceDirectories(lPath);
    end
    else
      FCurrentLogFileName := IncludeTrailingPathDelimiter(GetCurrentDir) + AFileName;

    FFileAccessCriticalSection.Enter;
    AssignFile(FLogFile, FCurrentLogFileName);
    FFileAccessCriticalSection.Leave;
    FUsedLogFileNames.Add(FCurrentLogFileName);
    
    if not FileExists(FCurrentLogFileName) then
      Result := DoResetLog()
    else
    begin  
      if not AResetFile then
        Result := True
      else
        Result := DoResetLog();    
    end;

  except
    FFileAccessCriticalSection.Leave;
    Result := False;
  end;
end;

procedure TGLLogSession.ChangeBufferedState();
begin
  if (FBuffered) then
  begin
    FBufferProcessingThread := TLogBufferFlushThread.Create(Self);
{$IFDEF GLS_DELPHI_2009_DOWN}
    FBufferProcessingThread.Resume();
{$ELSE}
    FBufferProcessingThread.Start();
{$ENDIF}
  end
  else
  begin
    FBufferProcessingThread.Terminate();

    // DaStr: Not really safe because we can wait forever.
    // But other methods known to me are platform-dependant.
    FBufferProcessingThread.WaitFor();

    FBufferProcessingThread.Free();
  end;
end;

procedure TGLLogSession.ClearLogsInTheSameDir;
var
  sRec: TSearchRec;
  lFilePath: string;

  procedure DeleteCurrentFile();
  begin
    if FCurrentLogFileName <> lFilePath + sRec.Name then
      DeleteFile(lFilePath + sRec.Name);
  end;
  
begin
  lFilePath := ExtractFilePath(FCurrentLogFileName);

  If FindFirst(lFilePath + RemovePathAndExt(FCurrentLogFileName) +
    '*' + ExtractFileExt(FCurrentLogFileName), faAnyfile, sRec) = 0 then
  begin
    try
      DeleteCurrentFile()
    except
    end;
    
    while ( FindNext(sRec) = 0 ) do
    try
      DeleteCurrentFile();
    except
    end;
    FindClose(sRec);
  end;
end;

procedure TGLLogSession.CreateNewLogFileIfNeeded;
var
  lNewFileName: string;
  I, Index: Integer;
  lFileSize: Integer;
begin
  try
    FFileAccessCriticalSection.Enter;
    lFileSize := FileSize(FCurrentLogFileName);
    FFileAccessCriticalSection.Leave();
  except
    lFileSize := -1;
    FFileAccessCriticalSection.Leave();
  end;

  if lFileSize >= FLogFileMaxSize then
  begin
    I := 1;
    lNewFileName := FOriginalLogFileName;

    repeat
      Index := LastDelimiter('.', FOriginalLogFileName);
      if Index = -1 then Exit;
      lNewFileName := FOriginalLogFileName;
      Insert('_' + IntToStr(I), lNewFileName, Index);
      Inc(i);
    until
      not FileExists(lNewFileName);

    if FWriteInternalMessages then
    begin
      Log(Format('Creating new log file "%s" because old one became too big (%d bytes)',
        [lNewFileName, lFileSize]));
    end;
    AttachLogFile(lNewFileName, True);
  end;
end;

destructor TGLLogSession.Destroy;
var
  I: TLogLevel;
begin
  FDestroying := True;
{$IFNDEF GLS_LOGGING}
  if Self = v_GLSLogger then
    Exit;
{$ENDIF}

  if FWriteInternalMessages then
  begin
    PrintLogStatistics();
    Log('Log session shutdown');
  end;

  SetBuffered(False);
  DoWriteBufferToLog(); // Terminates TLogBufferFlushThread.
  FBuffer.Free;
  SetLogFileMaxSize(0); // Terminates TLogCheckSizeThread.

  // Display log?
  for I := Low(TLogLevel) to High(TLogLevel) do
    if (I in FDisplayLogOnExitIfItContains) and (FLogKindCount[I] > 0) then
  begin
    DisplayLog();
    Break;
  end;

  if Self = v_GLSLogger then
    v_GLSLogger := nil;

  FUsedLogFileNames.Destroy;
  FBufferCriticalSection.Destroy;
  FFileAccessCriticalSection.Destroy;
end;

procedure TGLLogSession.DisplayLog;
{$IFDEF LINUX}
var
  lProcess: TProcess;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', 'C:\WINDOWS\notepad.exe',
    PChar(FCurrentLogFileName), nil, 1);
{$ENDIF}
{$IFDEF LINUX}
  lProcess := TProcess.Create(nil);
  lProcess.CommandLine := 'gedit ' + FCurrentLogFileName;
  lProcess.Execute;
  lProcess.Destroy;
{$ENDIF}
end;

procedure TGLLogSession.Log(const Desc: string; const Level: TLogLevel = lkInfo);
begin
  AppendLog(Desc, Level);
end;

procedure TGLLogSession.LogAdv(const args: array of const;
  const ALevel: TLogLevel);
begin
  Log(constArrayToString(args), ALevel);
end;

procedure TGLLogSession.LogDebug(const Desc: string);
begin
  Log(Desc, lkDebug);
end;

procedure TGLLogSession.LogInfo(const Desc: string);
begin
  Log(Desc, lkInfo);
end;

procedure TGLLogSession.LogNotice(const Desc: string);
begin
  Log(Desc, lkNotice);
end;

procedure TGLLogSession.LogWarning(const Desc: string);
begin
  Log(Desc, lkWarning);
end;

procedure TGLLogSession.LogEmtryLine;
begin
 if not FEnabled then Exit;

{$IFNDEF GLS_LOGGING}
  if Self = v_GLSLogger then
    Exit;
{$ENDIF}

  if FBuffered then
  begin
    // Critical section is always used.
    FBufferCriticalSection.Enter;
    FBuffer.Add('');
    FBufferCriticalSection.Leave;
  end
  else
  begin
    DoWriteToLog('');
  end;


  // IDELogProc.
  if (Self = v_GLSLogger) and Assigned(vIDELogProc) then
    vIDELogProc('');
end;

procedure TGLLogSession.LogError(const Desc: string);
begin
  Log(Desc, lkError);
end;

procedure TGLLogSession.LogFatalError(const Desc: string);
begin
  Log(Desc, lkFatalError);
end;

procedure TGLLogSession.LogDebugFmt(const Desc: string;
  const Args: array of const );
begin
  Log(Format(Desc, Args), lkDebug);
end;

procedure TGLLogSession.LogInfoFmt(const Desc: string;
  const Args: array of const );
begin
  Log(Format(Desc, Args), lkInfo);
end;

procedure TGLLogSession.LogNoticeFmt(const Desc: string;
  const Args: array of const );
begin
  Log(Format(Desc, Args), lkWarning);
end;

procedure TGLLogSession.LogWarningFmt(const Desc: string;
  const Args: array of const );
begin
  Log(Format(Desc, Args), lkWarning);
end;

procedure TGLLogSession.LogErrorFmt(const Desc: string;
  const Args: array of const );
begin
  Log(Format(Desc, Args), lkError);
end;

procedure TGLLogSession.LogException(const E: Exception; const aFunctionName: string;
  const args: array of const; const ALevel: TLogLevel = lkError);
begin
  Log('Exception in ' + aFunctionName + ': ' + E.Message + string(#13#10) +
   'Input parameters:' + string(#13#10) +
   constArrayToString(args), ALevel);
end;

procedure TGLLogSession.LogFatalErrorFmt(const Desc: string;
  const Args: array of const );
begin
  Log(Format(Desc, Args), lkFatalError);
end;

procedure TGLLogSession.AppendLog(const AString: string; const ALevel: TLogLevel;
  const ALogTime: Boolean);
var
  line: string;
begin
{$IFNDEF GLS_LOGGING}
  if Self = v_GLSLogger then
    Exit;
{$ENDIF}

  if not(ALevel in LogLevels) or not FEnabled then
    Exit;

  if ALogTime then
    case FTimeFormat of
      lfNone:
        line := lkPrefix[ALevel] + AString;
      lfDate:
        line := DateToStr(Now) + #9 + lkPrefix[ALevel] + AString;
      lfTime:
        line := TimeToStr(Now) + #9 + lkPrefix[ALevel] + AString;
      lfTimeExact:
        line := FormatDateTime('hh:nn:ss zzz "ms"', Now) + #9 + lkPrefix[ALevel] + AString;
      lfDateTime:
        line := DateTimeToStr(Now) + #9 + lkPrefix[ALevel] + AString;
      lfElapsed:
        line := IntToStr(GLGetTickCount - FStartedMs) + #9 +
          lkPrefix[ALevel] + AString;
    end
  else
    line := AString;

{$IFDEF GLS_MULTITHREAD}
  if (FLogThreadId) then
    line := #9 + 'Thread ID ' + IntToStr(PtrUInt(GetCurrentThreadId)) + #9 + line;
{$ENDIF}


  if FBuffered then
  begin
    // Critical section is always used.
    FBufferCriticalSection.Enter;
    FBuffer.Add(line);
    FBufferCriticalSection.Leave;
  end
  else
  begin
    DoWriteToLog(line);
  end;


  // IDELogProc.
  if (Self = v_GLSLogger) and Assigned(vIDELogProc) then
    vIDELogProc('GLScene: ' + line);


  // Message limit?
  Inc(FLogKindCount[ALevel]);
  if llMessageLimit[ALevel] < FLogKindCount[ALevel] then
    case FMessageLimitAction of
      mlaContinue: { Do nothing. } ;

      mlaStopLogging:
      begin
        Log('Logging stopped due to reaching message limit (' +
          FModeTitles[ALevel] +  ' = ' + IntToStr(FLogKindCount[ALevel]) + ')');
        FEnabled := False;
      end;

      mlaHalt:
      begin
        Log('Application halted due to reaching log message limit (' +
          FModeTitles[ALevel] +  ' = ' + IntToStr(FLogKindCount[ALevel]) + ')');
        SetBuffered(False);
        Halt;
      end;
    end;
end;

{ TLogBufferFlushThread }

constructor TLogBufferFlushThread.Create(const AParent: TGLLogSession);
begin
  FParent := AParent;
  inherited Create(True);
end;

procedure TLogBufferFlushThread.Execute;
begin
  while (not Terminated) or (FParent.FBuffer.Count > 0) do
  begin
    FParent.DoWriteBufferToLog();
    Sleep(FParent.FFlushBufferPeriod);
  end;
end;

{ TLogCheckSizeThread }

constructor TLogCheckSizeThread.Create(const AParent: TGLLogSession);
begin
  FParent := AParent;
  inherited Create(True);
end;

procedure TLogCheckSizeThread.Execute;
begin
  while (not Terminated and not FParent.FDestroying) do
  begin
    FParent.CreateNewLogFileIfNeeded();
    Sleep(FParent.FCheckFileSizePeriod);
  end;
end;

initialization

finalization
  if (v_GLSLogger <> nil) then v_GLSLogger.Destroy;
  

end.
