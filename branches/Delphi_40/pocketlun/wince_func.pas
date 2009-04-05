unit wince_func;
{
Copyright (C) 2007 Patrick Chevalley

http://www.astrosurf.com/avl
pch@freesurf.ch

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses windows, Forms, Controls, IniFiles, cu_tz,  // winceproc,
  Classes, SysUtils, dialogs;
  
type Tvma_window = (w_none,w_main,w_info,w_ephemeris,w_about,w_calendar,w_config,w_config_display,w_help,w_search,w_notes,w_photo,w_tools);

  function CondUTF8Decode(v:string):string;
  function CondUTF8Encode(v:string):string;
  Function Slash(nom : string) : string;
  procedure HideBtn(hnd: HWND);
  function words(str,sep : string; p,n : integer) : string;
  procedure ShowOKBtn(hnd: HWND);
  procedure ShowCancelBtn(hnd: HWND);
  Function ExecuteFile(const FileName: string; const Params: string =''): integer;
  procedure ExecNoWait(module,cmd: string);
  Function VMAShowmodal(f,mf:Tcustomform): Tmodalresult;
  Function HomeDirectory: string;
  Function ProgramFilesDirectory: string;
  Function AppDataDirectory: string;
  procedure GetCaption;
  procedure OneInstance;
  function MemoryAvailable: integer;
  procedure GetLanguageIDs(var Lang, FallbackLang: string);

{type Tvma_windowproc = class(TObject)
  private
     StdWindowProc: Windows.WNDPROC;
     function VMAWindowProc(WindowHandle: HWnd; Msg: UInt; WParam: Windows.WParam; LParam: Windows.LParam): LResult; cdecl;
  public
     function  InitWindowProc(hnd: HWND):boolean;
     procedure RestoreWindowProc(hnd: HWND);
  end;}

  
const
  WS_NONAVDONEBUTTON = $10000;
  SHDB_SHOWCANCEL = $0004;
  
var
  AppCaption : widestring;
  AppDir, language: string;

implementation

{$ifdef lclgtk2} {$define cdcutf8} {$endif}
{$ifdef lclqt} {$define cdcutf8} {$endif}
{$ifdef lclcarbon} {$define cdcutf8} {$endif}

function CondUTF8Decode(v:string):string;
begin
{$ifdef cdcutf8}
result:=v;
{$else}
result:=UTF8Decode(v);
{$endif}
end;

function CondUTF8Encode(v:string):string;
begin
{$ifdef cdcutf8}
result:=v;
{$else}
result:=UTF8Encode(v);
{$endif}
end;

Function Slash(nom : string) : string;
begin
result:=trim(nom);
if copy(result,length(nom),1)<>PathDelim then result:=result+PathDelim;
end;

function words(str,sep : string; p,n : integer) : string;
var     i,j : Integer;
begin
result:='';
str:=trim(str);
for i:=1 to p-1 do begin
 j:=pos(' ',str);
 if j=0 then j:=length(str)+1;
 str:=trim(copy(str,j,length(str)));
end;
for i:=1 to n do begin
 j:=pos(' ',str);
 if j=0 then j:=length(str)+1;
 result:=result+trim(copy(str,1,j))+sep;
 str:=trim(copy(str,j,length(str)));
end;
end;

procedure OneInstance;
var hExisting: THandle;
begin
{$ifdef wince}
  hExisting := FindWindow(nil,PWideChar(AppCaption));
  if hExisting<>0 then begin
     SetForegroundWindow(hExisting or 1);
     screen.Cursor:=crDefault;
     Halt(1);
  end;
{$endif}
end;

procedure GetCaption;
begin
  AppCaption:='PocketLun';
end;

// problem with wince showmodal, it scramble compoment on form.
// this simple one work.
Function VMAShowmodal(f,mf:Tcustomform): Tmodalresult;
begin
f.ModalResult:=mrNone;
mf.Caption:='';
f.Caption:=AppCaption;
f.ShowModal;
//f.show;
//while f.ModalResult=mrNone do application.ProcessMessages;
f.Caption:='';
mf.Caption:=AppCaption;
result:=f.ModalResult;
//f.Close;
SetForegroundWindow(mf.handle);
end;

procedure HideBtn(hnd: HWND);
var
  CurrentStyle, NewStyle : PtrInt;
begin
{$ifdef wince}
// hide the X button
  CurrentStyle := Windows.GetWindowLong(hnd, GWL_STYLE);
  NewStyle := CurrentStyle or WS_NONAVDONEBUTTON;
  Windows.SetWindowLong(hnd, GWL_STYLE, NewStyle);
// hide the ok/cancel button
  SHDoneButton(hnd,SHDB_HIDE);
{$endif}
end;

procedure ShowOKBtn(hnd: HWND);
begin
{$ifdef wince}
  SHDoneButton(hnd,SHDB_SHOW);
{$endif}
end;

procedure ShowCancelBtn(hnd: HWND);
begin
{$ifdef wince}
  SHDoneButton(hnd,SHDB_SHOWCANCEL);
{$endif}
end;

{$ifdef wince}
function ShellExecute(ahWnd: HWND; Operation, FileName, Parameters,Directory: PChar; ShowCmd: Integer): HINST;
var
seInfo: SHELLEXECUTEINFO;
lpseInfo: LPSHELLEXECUTEINFO;
TmpStr: AnsiString;
TmpWStr: array [0..255] of WideChar;
begin
seInfo.hwnd:= ahWnd;
seInfo.fMask:= 0;
seInfo.lpVerb:= PWideChar(Operation);
TmpStr:= '"'+FileName+'"';
StringToWideChar(TmpStr, TmpWStr, 255);
seInfo.lpFile:= PWideChar(TmpWstr);
seInfo.lpParameters:= PWideChar(Parameters);
seInfo.lpDirectory:= PWideChar(Directory);
seInfo.cbSize:= Sizeof(seInfo);
seInfo.nShow:=ShowCmd;
seinfo.fMask:=SEE_MASK_FLAG_NO_UI;
lpseInfo:= @seInfo;
ShellExecuteEx(lpseInfo);
result:=seInfo.hInstApp;
end;
{$endif}

Function ExecuteFile(const FileName: string; const Params: string =''): integer;
var
  zFileName, zVerb, zParams, zDir: array[0..255] of Char;
begin
  Result := ShellExecute(Application.MainForm.Handle, nil, StrPCopy(zFileName, FileName+chr(0)),StrPCopy(zParams, Params), StrPCopy(zDir, ''), SW_SHOWNORMAL);
end;

procedure ExecNoWait(module,cmd: string);
var
   bchExec,bchModule: array[0..1024] of widechar;
   pchEXEC,pchModule: PWidechar;
   pi: TProcessInformation;
begin
   pchExec := @bchExec;
   pchModule:= @bchModule;
   StringToWideChar(module+chr(0),bchModule,1024);
   StringToWideChar(cmd+chr(0),bchExec,1024);
   FillChar(pi,sizeof(pi),0);
   try
     CreateProcess(pchModule,pchExec,Nil,Nil,false,CREATE_NEW_CONSOLE, Nil,Nil,Nil,pi);
    except;
    end;
end;

Function HomeDirectory: string;
var dir : array [0..max_path] of widechar;
begin
{$ifdef wince}
if SHGetSpecialFolderPath(0,dir,CSIDL_PERSONAL,false)
   then result:=dir
   else result:='';
{$else}
  result:='';
{$endif}
end;

Function ProgramFilesDirectory: string;
const CSIDL_PROGRAM_FILES=$0026;
var dir : array [0..max_path] of widechar;
begin
{$ifdef wince}
if SHGetSpecialFolderPath(0,dir,CSIDL_PROGRAM_FILES,false)
   then result:=dir
   else result:='';
{$else}
  result:='';
{$endif}
end;

Function AppDataDirectory: string;
var dir : array [0..max_path] of widechar;
begin
{$ifdef wince}
if SHGetSpecialFolderPath(0,dir,CSIDL_APPDATA,false)
   then result:=dir
   else result:='';
{$else}
  result:='';
{$endif}
end;

function MemoryAvailable: integer;
var mem : MEMORYSTATUS;
begin
mem.dwLength:=sizeof(mem);
GlobalMemoryStatus(@mem);
result:=mem.dwAvailPhys;
end;

procedure GetLanguageIDs(var Lang, FallbackLang: string);
var
  UserLCID: LCID;
  i: integer;
  Buffer: array[1..4] of widechar;
  Country, buf: string;
begin
  //defaults
  Lang := '';
  FallbackLang:='';
  UserLCID := GetUserDefaultLCID;
  if GetLocaleInfo(UserLCID, LOCALE_SABBREVLANGNAME, @Buffer[1], 4)<>0 then
     FallbackLang := lowercase(copy(Buffer,1,2));
  if GetLocaleInfo(UserLCID, LOCALE_SABBREVCTRYNAME, @Buffer[1], 4)<>0 then begin
    Country := copy(Buffer,1,2);

    // some 2 letter codes are not the first two letters of the 3 letter code
    // there are probably more, but first let us see if there are translations
    if (Buffer='PRT') then Country:='PT';

    Lang := FallbackLang+'_'+Country;
  end;
end;


///////////////////// Tvma_windowproc

(*
function Tvma_windowproc.VMAWindowProc(WindowHandle: HWnd; Msg: UInt; WParam: Windows.WParam; LParam: Windows.LParam): LResult; cdecl;
var
    WindowInfo: PWindowInfo;
    ok: boolean;
begin
{ok:=false;
  if msg = WM_COMMAND  then begin
        WindowInfo := GetWindowInfo(WindowHandle);
        { WINCE menubar button }
        if (WindowInfo^.WinControl is TCustomForm) then
           case lo(WParam) of
             { OK button }
             IDOK: begin
                if (TCustomForm(WindowInfo^.WinControl).DefaultControl<>nil) then
                   TCustomForm(WindowInfo^.WinControl).DefaultControl.ExecuteDefaultAction;
                ok:=true;
             end;
             { Cancel button }
             IDCANCEL: begin
                if (TCustomForm(WindowInfo^.WinControl).CancelControl<>nil) then
                   TCustomForm(WindowInfo^.WinControl).CancelControl.ExecuteCancelAction;
                ok:=true;
             end;
           end;
      if ok then begin
          Result := 0;
          exit;
      end;
  end; }
 {Call the original winproc}
 Result := Windows.CallWindowProc(StdWindowProc,WindowHandle, msg, WParam, LParam);
end;

function Tvma_windowproc.InitWindowProc(hnd: HWND):boolean;
begin
{Set form's windows proc to ours and remember the old window proc}
StdWindowProc := Windows.WNDPROC(windows.SetWindowLong(hnd,GWL_WNDPROC,LONG(@VMAWindowProc)));

result:=long(@StdWindowProc)<>0;
end;

procedure Tvma_windowproc.RestoreWindowProc(hnd: HWND);
begin
{Set form1's window proc back to it's original procedure}
  windows.SetWindowLong(hnd,
               GWL_WNDPROC,
               LONG(StdWindowProc));
end;
*)

end.

