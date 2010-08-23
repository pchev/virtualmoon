unit cadencerasap;

{
  $Log: cadencerasap.pas,v $
  Revision 1.1  2006/01/10 20:50:46  z0m3ie
  recheckin to make shure that all is lowercase

  Revision 1.4  2006/01/09 21:02:33  z0m3ie
  *** empty log message ***

  Revision 1.6  2005/12/04 16:53:04  z0m3ie
  renamed everything to lowercase to get better codetools support and avoid unit finding bugs

  Revision 1.5  2005/08/29 17:38:45  z0m3ie
  changed InterfaceObject to Widgetset (changed in Lazarus)

  Revision 1.4  2005/08/29 08:21:26  k00m
  Need this change to work with the last lazarus Lazarus-0.9.9-20050829-win32.

  Revision 1.3  2005/08/07 19:50:02  z0m3ie
  *** empty log message ***

}

{$mode objfpc}{$H+}


interface

uses
  classes, sysutils,forms
  {$ifdef fpc}
  , interfacebase, lclintf, lmessages, controls
  {$else}
  {$ifdef mswindows}
  ,windows
  {$endif}
  {$endif}
  ;

{$IFDEF LCL}
const
  LM_GLCADENCER = LM_INTERFACELAST+325;
  LM_GLTIMER    = LM_INTERFACELAST+326;
{$ENDIF}

type

   { TASAPHandler }

   TASAPHandler = class
     private
      FTooFastCounter : Integer;
      FTimer : Cardinal;
      {$IFNDEF FPC}
      {$IFDEF MSWINDOWS}
      FWindowHandle : HWND;
      procedure WndProc(var Msg: TMessage);
      {$ENDIF}
      {$ELSE}
      FMessageTime : LongInt;
    public
      procedure TimerProc;
      procedure Cadence(var Msg: TLMessage); message LM_GLCADENCER;
      {$ENDIF}
      constructor Create;
      destructor Destroy; override;
   end;

var
   vWMTickCadencer : Cardinal;
   vASAPCadencerList : TList;
   vHandler : TASAPHandler;

const
   fMSG_TIMER = 10;
   cTickGLCadencer = 'TickGLCadencer';

implementation

uses glcadencer;

// ------------------
// ------------------ TASAPHandler ------------------
// ------------------

// Create
//
constructor TASAPHandler.Create;
begin
  inherited Create;
{$IFDEF FPC}
  FTimer := 0;
  FMessageTime := GetTickCount;
//TODO: make this widgetset independent
  FTimer:=InterfaceBase.WidgetSet.CreateTimer(1, @TimerProc);
{$ELSE}
{$IFDEF LINUX}
{$ELSE}
{$ifdef GLS_DELPHI_6_UP}
   FWindowHandle:=Classes.AllocateHWnd(WndProc);
{$else}
   FWindowHandle:=AllocateHWnd(WndProc);
{$endif}
   PostMessage(FWindowHandle, vWMTickCadencer, 0, 0);
{$ENDIF}
{$ENDIF}
end;

// Destroy
//
destructor TASAPHandler.Destroy;
begin
{$IFDEF FPC}
  if FTimer <> 0 then
    begin
//TODO: make this widgetset independent
      WidgetSet.DestroyTimer(FTimer);
      FTimer := 0;
    end;
{$ELSE}
{$IFDEF LINUX}
{$ELSE}
{$ifdef GLS_DELPHI_6_UP}
   if FTimer<>0 then
      KillTimer(FWindowHandle, FTimer);
{$ifdef GLS_DELPHI_6_UP}
 	Classes.DeallocateHWnd(FWindowHandle);
{$else}
 	DeallocateHWnd(FWindowHandle);
{$endif}
{$endif}
{$ENDIF}
{$ENDIF}
   inherited Destroy;
end;

var
   vWndProcInLoop : Boolean;

{$IFNDEF FPC}
{$IFDEF MSWINDOWS}
// WndProc
//
procedure TASAPHandler.WndProc(var Msg: TMessage);
var
   i : Integer;
   cad : TGLCadencer;
begin
//   Windows.Beep(440, 10);
   with Msg do begin
      if Msg=WM_TIMER then begin
         KillTimer(FWindowHandle, FTimer);
         FTimer:=0;
      end;
      if (Msg<>WM_TIMER) and (Cardinal(GetMessageTime)=GetTickCount) then begin
         // if we're going too fast, "sleep" for 1 msec
         Inc(FTooFastCounter);
         if FTooFastCounter>5000 then begin
            if FTimer=0 then
               FTimer:=SetTimer(FWindowHandle, 1, 1, nil);
            FTooFastCounter:=0;
         end;
      end else FTooFastCounter:=0;
      if FTimer<>0 then begin
         Result:=0;
         Exit;
      end;
      if not vWndProcInLoop then begin
         vWndProcInLoop:=True;
         try
         	if (Msg=vWMTickCadencer) or (Msg=WM_TIMER) then begin
               // Progress
               for i:=vASAPCadencerList.Count-1 downto 0 do begin
                  cad:=TGLCadencer(vASAPCadencerList[i]);
                  if     Assigned(cad) and (cad.Mode=cmASAP)
                     and cad.Enabled and (cad.FProgressing=0) then begin
                     if Application.Terminated then begin
                        // force stop
                        cad.Enabled:=False
                     end else begin
                        try
                           // do stuff
                           cad.Progress;
                        except
                           Application.HandleException(Self);
                           // it faulted, stop it
                           cad.Enabled:=False
                        end
                     end;
                  end;
               end;
               // care for nils
               vASAPCadencerList.Pack;
               if vASAPCadencerList.Count=0 then begin
                  vASAPCadencerList.Free;
                  vASAPCadencerList:=nil;
                  vHandler.Free;
                  vHandler:=nil;
               end else begin
                  // Prepare the return of the infernal loop...
                  PostMessage(FWindowHandle, vWMTickCadencer, 0, 0);
               end;
            end;
         finally
            vWndProcInLoop:=False;
         end;
      end;
		Result:=0;
	end;
end;
{$ENDIF}
{$ELSE}

procedure TASAPHandler.TimerProc;
var
  NewMsg : TLMessage;
begin
  NewMsg.Msg := LM_GLTIMER;
  Cadence(NewMsg);
end;

procedure TASAPHandler.Cadence(var Msg: TLMessage);
var
  NewMsg : TLMessage;
  i : Integer;
   cad : TGLCadencer;
begin
  with Msg do
    begin
    if FTimer <> 0 then
      begin
//TODO: make this widgetset independent
        WidgetSet.DestroyTimer(FTimer);
        FTimer := 0;
      end;
    if FTimer<> 0 then begin
      Exit;
      end;
      if (Msg<>LM_GLTIMER) and (FMessageTime=GetTickCount) then begin
         // if we're going too fast, "sleep" for 1 msec
         Inc(FTooFastCounter);
         if FTooFastCounter>5000 then begin
            if FTimer=0 then
//TODO: make this widgetset independent
               FTimer:=WidgetSet.CreateTimer(1, @TimerProc);
            FTooFastCounter:=0;
         end;
      end else FTooFastCounter:=0;
      if FTimer<>0 then begin
         Exit;
      end;
      if not vWndProcInLoop then begin
         vWndProcInLoop:=True;
         try
         	if (Msg=LM_GLCADENCER) or (Msg=LM_GLTIMER) then begin
               // Progress
               for i:=vASAPCadencerList.Count-1 downto 0 do begin
                  cad:=TGLCadencer(vASAPCadencerList[i]);
                  if     Assigned(cad) and (cad.Mode=cmASAP)
                     and cad.Enabled and (cad.FProgressing=0) then begin
                     if Application.Terminated then begin
                        // force stop
                        cad.Enabled:=False
                     end else begin
                        try
                           // do stuff
                           cad.Progress;
                        except
                           Application.HandleException(Self);
                           // it faulted, stop it
                           cad.Enabled:=False
                        end
                     end;
                  end;
               end;
               // care for nils
               vASAPCadencerList.Pack;
               if vASAPCadencerList.Count=0 then begin
                  vASAPCadencerList.Free;
                  vASAPCadencerList:=nil;
                  vHandler.Free;
                  vHandler:=nil;
               end else begin
                  // Prepare the return of the infernal loop...
                  FMessageTime := GetTickCount;
//TODO: make this widgetset independent
                  FTimer:=WidgetSet.CreateTimer(1, @TimerProc);

//                  NewMsg.Msg := LM_GLCADENCER;
//                  Dispatch(NewMsg);
               end;
            end;
         finally
            vWndProcInLoop:=False;
         end;
      end;
    end;
end;
{$ENDIF}

end.

