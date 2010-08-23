unit telescope;

{$MODE Delphi}
{$H+}
interface

Uses
   dynlibs,Types,LCLIntf,Dialogs;

Procedure InitScopeLibrary(fn:string);
Procedure UnloadScopeLibrary;

type
TScopeConnect   = Procedure(var ok : boolean); stdcall;
TScopeDisconnect= Procedure(var ok : boolean); stdcall;
TScopeAlign     = Procedure(source : string; ra,dec : double); stdcall;
TScopeShowModal = Procedure(var ok : boolean); stdcall;
TScopeShow      = Procedure; stdcall;
TScopeClose     = Procedure; stdcall;
TScopeGetRaDec  = Procedure (var ar,de : double; var ok : boolean); stdcall;
TScopeGetAltAz  = Procedure (var alt,az : double; var ok : boolean); stdcall;
TScopeGetName   = Procedure (var name : shortstring); stdcall;
TScopeReset     = Procedure ; stdcall;
TScopeConnected  = Function   : boolean ; stdcall;
TScopeGetInfo    = Procedure (var Name : shortstring; var QueryOK,SyncOK,GotoOK : boolean; var refreshrate : integer); stdcall;
TScopeInitialized = Function   : boolean ; stdcall;
TScopeSetObs     = Procedure (latitude,longitude : double); stdcall;
TScopeGoto       = Procedure (ar,de : double; var ok : boolean); stdcall;
TScopeReadConfig = Procedure (ConfigPath : shortstring; var ok : boolean); stdcall;

var
    ScopeConnect : TScopeConnect;
    ScopeDisconnect : TScopeDisconnect;
    ScopeAlign : TScopeAlign;
    ScopeShowModal : TScopeShowModal;
    ScopeShow : TScopeShow;
    ScopeClose : TScopeClose;
    ScopeGetRaDec : TScopeGetRaDec;
    ScopeGetAltAz : TScopeGetAltAz;
    ScopeGetName : TScopeGetName;
    ScopeReset : TScopeReset;
    ScopeConnected : TScopeConnected;
    ScopeInitialized : TScopeInitialized;
    ScopeGetInfo : TScopeGetInfo;
    ScopeSetObs : TScopeSetObs;
    ScopeGoto : TScopeGoto;
    ScopeReadConfig : TScopeReadConfig;
    scopelib : Dword = 0;
    scopelibok : boolean = false;
    scopeinitok : boolean = false;

implementation

Procedure UnloadScopeLibrary;
var ok : boolean;
begin
if scopelib<>0 then begin
   scopelibok:=false;
   ScopeDisconnect(ok);
   ScopeClose;
   scopelib:=0;
//   CoUnInitialize
end;
end;

Procedure InitScopeLibrary(fn:string);
begin
UnloadScopeLibrary;
scopelib := LoadLibrary(Pchar(fn));
if scopelib<>0 then begin
    ScopeClose := TScopeClose(GetProcAddress(scopelib, 'ScopeClose'));
    ScopeConnect := TScopeConnect(GetProcAddress(scopelib, 'ScopeConnect'));
    ScopeDisconnect := TScopeDisconnect(GetProcAddress(scopelib, 'ScopeDisconnect'));
    ScopeAlign := TScopeAlign(GetProcAddress(scopelib, 'ScopeAlign'));
    ScopeShowModal := TScopeShowModal(GetProcAddress(scopelib, 'ScopeShowModal'));
    ScopeShow := TScopeShow(GetProcAddress(scopelib, 'ScopeShow'));
    ScopeGetRaDec := TScopeGetRaDec(GetProcAddress(scopelib, 'ScopeGetRaDec'));
    ScopeGetAltAz := TScopeGetAltAz(GetProcAddress(scopelib, 'ScopeGetAltAz'));
    ScopeGetName := TScopeGetName(GetProcAddress(scopelib, 'ScopeGetName'));
    ScopeReset := TScopeReset(GetProcAddress(scopelib, 'ScopeReset'));
    ScopeConnected := TScopeConnected(GetProcAddress(scopelib, 'ScopeConnected'));
    ScopeInitialized := TScopeInitialized(GetProcAddress(scopelib, 'ScopeInitialized'));
    ScopeGetInfo := TScopeGetInfo(GetProcAddress(scopelib, 'ScopeGetInfo'));
    ScopeSetObs := TScopeSetObs(GetProcAddress(scopelib, 'ScopeSetObs'));
    ScopeGoto := TScopeGoto(GetProcAddress(scopelib, 'ScopeGoto'));
    ScopeReadConfig := TScopeReadConfig(GetProcAddress(scopelib, 'ScopeReadConfig'));

    scopelibok:=true;
//    CoInitialize(nil);
end else begin
    scopelibok:=false;
    Showmessage('Error opening '+fn);
end;
end;

end.
