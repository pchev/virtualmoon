program vmabasic;
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
uses
  Forms,
  Registry,
  SysUtils,
  Windows,
  skylib,
  virtualmoon1 in 'virtualmoon1.pas' {Form1},
  config in 'config.pas' {Form2},
  BigIma in 'BigIma.pas' {BigImaForm},
  splashunit in 'splashunit.pas' {splash},
  imglistunit in 'imglistunit.pas' {Imglist},
  glossary in 'glossary.pas' {Gloss},
  fmsg in 'fmsg.pas' {MsgForm};

{$R *.RES}

const IdMsg='Virtual_Moon_Atlas_Basic_message';
      IdMutex='Virtual_Moon_Atlas_Basic_mutex';
      exeName='vmabasic.exe';

var Registry1: TRegistry;
    buf: shortstring;
    ok : boolean;
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
  Registry1 := TRegistry.Create;       // get install directory
  with Registry1 do begin
    ok:=Openkey('Software\Astro_PC\VirtualMoon',true);
    if ok then begin
      if ValueExists('Install_Dir') then begin
         buf:=ReadString('Install_Dir');
         if DirectoryExists(buf) and Fileexists(buf+'\'+exename) then chdir(buf)
         else begin
              buf:=getcurrentdir;
              if Fileexists(buf+'\'+exename) then WriteString('Install_Dir',buf);  // sinon on l'initialise
         end;
      end else begin
         buf:=getcurrentdir;
         if Fileexists(buf+'\'+exename) then WriteString('Install_Dir',buf);  // sinon on l'initialise
      end;
      CloseKey;
    end;
  end;
  Registry1.free;

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
 {Tell Delphi to un-hide it's hidden application window}
 {This allows our instance to have a icon on the task bar}
  Application.ShowMainForm := true;
  ShowWindow(Application.Handle, SW_RESTORE);
  Application.CreateForm(TForm1, Form1);
  splash := Tsplash.create(application);
  splash.show;
  splash.refresh;
  Application.CreateForm(TImglist, Imglist);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
