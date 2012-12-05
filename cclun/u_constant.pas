unit u_constant;
{
Copyright (C) 2002 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

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
{
 Type and constant declaration
}
{$mode objfpc}{$H+}
interface

uses
     Classes, Controls, Graphics;

 const
  crlf = chr(10)+chr(13);
  cpyr = chr($a9)+chr($c2);  // ©
  AVLversion = '6.1';
  VersionName = 'CCLun';
  avlcpy = 'Copyright '+cpyr+' 2002-2012 Christian Legrand, Patrick Chevalley';
  vmaurl='http://ap-i.net/avl';
  blank=' ';
  {$ifdef linux}
        DefaultHome='~/';
        DefaultPrivateDir='~/.virtualmoon';
        Defaultconfigfile='~/.virtualmoon/vma.rc';
        SharedDir='../share/virtualmoon';
        DefaultTmpDir='tmp';
        DefaultAtlun='atlun';
        DefaultPhotlun='photlun';
        DefaultDatlun='datlun';
        DefaultWeblun='weblun';
  {$endif}
  {$ifdef darwin}
        DefaultHome='~/';
        DefaultPrivateDir='~/.virtualmoon';
        Defaultconfigfile='~/.virtualmoon/vma.rc';
        SharedDir='/usr/share/virtualmoon';
        DefaultTmpDir='tmp';
        DefaultAtlun='atlun.app/Contents/MacOS/atlun';
        DefaultPhotlun='photlun.app/Contents/MacOS/photlun';
        DefaultDatlun='datlun.app/Contents/MacOS/datlun';
        DefaultWeblun='weblun.app/Contents/MacOS/weblun';
  {$endif}
  {$ifdef mswindows}
        DefaultPrivateDir='virtualmoon';
        Defaultconfigfile='vma.rc';
        SharedDir='.\';
        DefaultTmpDir='tmp';
        DefaultAtlun='atlun.exe';
        DefaultPhotlun='photlun.exe';
        DefaultDatlun='datlun.exe';
        DefaultWeblun='weblun.exe';
  {$endif}                                    

var
    appdir, privatedir, bindir, configfile,tempdir,helpdir: string;
    AtLun,Photlun,DatLun,WebLun: string;
    language, uplanguage, helpprefix: string;
    {$ifdef darwin}
         OpenFileCMD:string = 'open';   //
    {$else}
         OpenFileCMD:string = 'xdg-open';   // default FreeDesktop.org
    {$endif}


implementation

end.
