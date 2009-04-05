unit Sky_DDE_Util;
{
Copyright (C) 2003 Patrick Chevalley

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
interface

uses Windows,Forms,SysUtils,Registry,ShellApi, IniFiles;
//
Function GetSkyChartInfo : boolean;
Function StartSkyChart(param : string) : boolean;
function SkyChartRunning : boolean;

var
    skychartcaption : string ='Cartes du Ciel';
    cieldir : string = '';
    ciellang : string = '';
    skychartok : boolean;

implementation


function SkyChartRunning : boolean;
begin
result:= findwindow(nil,Pchar(skychartcaption)) <> 0 ;    // find skychart main window
end;

Function Exec(FileName, Params, DefaultDir: string): THandle;
// execute a command
var
  zFileName, zParams, zDir: array[0..255] of Char;
begin
Result := ShellExecute(Application.MainForm.Handle, nil, StrPCopy(zFileName, FileName),
                       StrPCopy(zParams, Params), StrPCopy(zDir, DefaultDir), SW_SHOWNORMAL);
end;

Function GetSkyChartInfo : boolean;
var Registry1: TRegistry;
    inifile : Tinifile;
begin
Registry1 := TRegistry.Create;         // find if skychart is installed
with Registry1 do begin                // and where to start it
  result:=Openkey('Software\Astro_PC\Ciel\Config',false);
  if result then
     if ValueExists('Language') then ciellang:=ReadString('Language'); // current language
     if ValueExists('AppDir') then cieldir:=ReadString('Appdir')   // install directory
                              else result:=false;
  CloseKey;
end;
Registry1.Free;
if ciellang<>'' then begin
   inifile:=Tinifile.create(cieldir+'\language_'+ciellang+'.ini');   // find window caption in language ini file
   if Inifile.SectionExists('main_') then begin
      skychartcaption:=inifile.ReadString('main_','title',skychartcaption);
   end;
   inifile.free;
end;
end;

Function StartSkyChart(param : string) : boolean;
begin
result := cieldir>'';
if result then begin
   Exec(cieldir+'\ciel.exe',param,cieldir);        // execute command
end;
end;

end.

