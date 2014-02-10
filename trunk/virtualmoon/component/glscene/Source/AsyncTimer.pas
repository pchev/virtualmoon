//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : Asynchronous timer component (actual 1 ms resolution).<p>

  This component is based on ThreadedTimer by Carlos Barbosa.<p>

  <b>History : </b><font size=-1><ul>
  <li>06/05/09 - DanB - removed TThreadPriority, was needed for Kylix, but not FPC
  <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTracekrID=1681585)
  <li>28/06/04 - LR - Added TThreadPriority for Linux
  <li>24/09/02 - EG - Fixed ThreadPriority default value (Nelson Chu)
  <li>20/01/02 - EG - Simplifications, dropped Win32 dependencies
  <li>05/04/00 - GrC - Enabled checks to prevent events after destroy
  <li>01/04/00 - EG - Re-Creation, minor changes over Carlos's code
  </ul></font>
}
unit AsyncTimer;

interface

{$I GLScene.inc}

uses Classes, SyncObjs;

const
  cDEFAULT_TIMER_INTERVAL = 1000;

type

  // TAsyncTimer
  //
  { : Asynchronous timer component (actual 1 ms resolution, if CPU fast enough).<p>
    Keep in mind timer resolution is obtained <i>in-between</i> events, but
    events are not triggered every x ms. For instance if you set the interval to
    5 ms, and your Timer event takes 1 ms to complete, Timer events will actually
    be triggered every 5+1=6 ms (that's why it's "asynchronous").<p>
    This component is based on ThreadedTimer by Carlos Barbosa. }
  TAsyncTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FOnTimer: TNotifyEvent;
    FTimerThread: TThread;
    FMutex: TCriticalSection;
  protected
    procedure SetEnabled(Value: Boolean);
    function GetInterval: Word;
    procedure SetInterval(Value: Word);
    function GetThreadPriority: TThreadPriority;
    procedure SetThreadPriority(Value: TThreadPriority);
    procedure DoTimer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Interval: Word read GetInterval write SetInterval
      default cDEFAULT_TIMER_INTERVAL;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property ThreadPriority: TThreadPriority read GetThreadPriority
      write SetThreadPriority default tpTimeCritical;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, GLCrossPlatform;

type

  // TTimerThread
  //
  TTimerThread = class(TThread)
  private
    FOwner: TAsyncTimer;
    FInterval: Word;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean); virtual;
  end;

  // Create
  //
constructor TTimerThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
end;

// Execute
//
procedure TTimerThread.Execute;
var
  lastTick, nextTick, curTick, perfFreq: Int64;
begin
  QueryPerformanceFrequency(perfFreq);
  QueryPerformanceCounter(lastTick);
  nextTick := lastTick + (FInterval * perfFreq) div 1000;
  while not Terminated do
  begin
    FOwner.FMutex.Acquire;
    FOwner.FMutex.Release;
    while not Terminated do
    begin
      QueryPerformanceCounter(lastTick);
      if lastTick >= nextTick then
        break;
      Sleep(1);
    end;
    if not Terminated then
    begin
      // if time elapsed run user-event
      Synchronize(FOwner.DoTimer);
      QueryPerformanceCounter(curTick);
      nextTick := lastTick + (FInterval * perfFreq) div 1000;
      if nextTick <= curTick then
      begin
        // CPU too slow... delay to avoid monopolizing what's left
        nextTick := curTick + (FInterval * perfFreq) div 1000;
      end;
    end;
  end;
end;

{ TAsyncTimer }

// Create
//
constructor TAsyncTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // create timer thread
  FMutex := TCriticalSection.Create;
  FMutex.Acquire;
  FTimerThread := TTimerThread.Create(False);

  with TTimerThread(FTimerThread) do
  begin
    FOwner := Self;
    FreeOnTerminate := False;
    Priority := tpTimeCritical;
    FInterval := cDEFAULT_TIMER_INTERVAL;
  end;
end;

// Destroy
//
destructor TAsyncTimer.Destroy;
begin
  Enabled := False;
  FTimerThread.Terminate;
  FMutex.Release;
  CheckSynchronize;
  // wait & free
  FTimerThread.WaitFor;
  FTimerThread.Free;
  FMutex.Free;
  inherited Destroy;
end;

// DoTimer
//
procedure TAsyncTimer.DoTimer;
begin
  if Enabled and Assigned(FOnTimer) then
    FOnTimer(Self);
end;

// SetEnabled
//
procedure TAsyncTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if FEnabled then
    begin
      // When enabled resume thread
      if TTimerThread(FTimerThread).FInterval > 0 then
        FMutex.Release;
    end
    else
      FMutex.Acquire;
  end;
end;

function TAsyncTimer.GetInterval: Word;
begin
  Result := TTimerThread(FTimerThread).FInterval;
end;

procedure TAsyncTimer.SetInterval(Value: Word);
begin
  if Value <> TTimerThread(FTimerThread).FInterval then
  begin
    TTimerThread(FTimerThread).FInterval := Value;
  end;
end;

function TAsyncTimer.GetThreadPriority: TThreadPriority;
begin
  Result := FTimerThread.Priority;
end;

procedure TAsyncTimer.SetThreadPriority(Value: TThreadPriority);
begin
  FTimerThread.Priority := Value;
end;

initialization

  RegisterClass(TAsyncTimer);


end.
