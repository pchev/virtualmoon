program PocketLun;
{
Copyright (C) 2007 Patrick Chevalley

http:/ap-i.net/avl
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

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,Forms, sysutils, controls, imagesforlazarus,
  Inifiles, pu_pocketlun, libsql, pu_info,
pu_ephemeris, pu_about, pu_config_display, pu_config, pu_calendar, pu_search,
pu_setdate, cu_moon, u_astro, series96main, elp82main, CDCjdcalendar,
pu_notes, pu_photo, wince_func, u_translation;

begin
  screen.cursor:=crHourGlass;
  GetCaption;
  OneInstance;
  Application.Initialize;
  Application.CreateForm(Tf_pocketlun, f_pocketlun);
  Application.CreateForm(Tf_setdate, f_setdate);
  Application.CreateForm(Tf_info, f_info);
  Application.CreateForm(Tf_ephemeris, f_ephemeris);
  Application.CreateForm(Tf_about, f_about);
  Application.CreateForm(Tf_calendar, f_calendar);
  Application.CreateForm(Tf_config, f_config);
try
  Application.CreateForm(Tf_config_display, f_config_display);  //?win32
finally
  Application.CreateForm(Tf_search, f_search);
  Application.CreateForm(Tf_notes, f_notes);
  Application.CreateForm(Tf_photo, f_photo);
  Application.Run;
end;
end.

