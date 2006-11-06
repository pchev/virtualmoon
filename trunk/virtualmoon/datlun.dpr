program datlun;
{
Copyright (C) 2006 Patrick Chevalley

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

uses
  Forms,
  SysUtils,
  Windows,
  vmabrowser1 in 'vmabrowser1.pas' {Form1},
  vmabrowser2 in 'vmabrowser2.pas' {Selection},
  vmabrowser3 in 'vmabrowser3.pas' {Columns},
  vmabrowser4 in 'vmabrowser4.pas' {LoadCSV},
  vmabrowser5 in 'vmabrowser5.pas' {SelectDB};

{$R *.res}

const IdMutex='Virtual_Moon_Atlas_Browser_mutex';
var ok : boolean;
    f : textfile;
    i : integer;                                        

Function CreateMyMutex:boolean;
begin
 {Attempt to create a named mutex}
  CreateMutex(nil, false, IdMutex);
 {if it failed then there is another instance}
  if GetLastError = ERROR_ALREADY_EXISTS then result:=false else result:=true;
end;

begin
  // Let our window return to foreground when calling SetForegroundWindow.
//  SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, nil, SPIF_SENDWININICHANGE or SPIF_UPDATEINIFILE);
  ok:=CreateMyMutex;
  if not ok then begin
   //Send all windows our custom message - only our other
   //instance will recognise it, and restore itself
    if paramcount>0 then begin
       assignfile(f,'@@param@@');
       rewrite(f);
       for i:=1 to paramcount do
          writeln(f,paramstr(i));
       Closefile(f);
       sleep(100);
    end;
    SendMessage(HWND_BROADCAST,
                RegisterWindowMessage(IdMsg),
                paramcount,
                0);
   //Lets quit
    Halt(0);
  end;
  Application.Initialize;
  Application.UpdateFormatSettings:=false;
  decimalseparator:='.';
 {Tell Delphi to un-hide it's hidden application window}
 {This allows our instance to have a icon on the task bar}
  Application.ShowMainForm := true;
  ShowWindow(Application.Handle, SW_RESTORE);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSelection, Selection);
  Application.CreateForm(TColumns, Columns);
  Application.CreateForm(TLoadCSV, LoadCSV);
  Application.CreateForm(TSelectDB, SelectDB);
  Application.Run;
end.
