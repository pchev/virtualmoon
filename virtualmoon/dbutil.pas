unit dbutil;
{
  This file is to be maintened in datlun
  and copied to virtualmoon if changed
}
{
Copyright (C) 2006 Patrick Chevalley

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
{$MODE objfpc}
{$H+}

interface

Uses Forms, Dialogs, SysUtils, mlb2, passql, passqlite;

const
    DBversion=700;
    MaxDBN=200;
    NumMoonDBFields = 97;
    MoonDBFields : array[1..NumMoonDBFields,1..2] of string = (
      ('NAME','text'),
      ('LUN','text'),
      ('LUN_REDUCED','text'),
      ('NAME_TYPE','text'),
      ('TYPE','text'),
      ('TYPE_IAU','text'),
      ('SUBTYPE','text'),
      ('PROCESS','text'),
      ('PERIOD','text'),
      ('PERIOD_SOURCE','text'),
      ('GEOLOGY','text'),
      ('NAME_DETAIL','text'),
      ('NAME_ORIGIN','text'),
      ('IAU_APPROVAL','text'),
      ('LANGRENUS','text'),
      ('HEVELIUS','text'),
      ('RICCIOLI','text'),
      ('WORK','text'),
      ('COUNTRY','text'),
      ('NATIONLITY','text'),
      ('CENTURY_N','float'),
      ('CENTURY_C','text'),
      ('BIRTH_PLACE','text'),
      ('BIRTH_DATE','text'),
      ('DEATH_PLACE','text'),
      ('DEATH_DATE','text'),
      ('FACTS','text'),
      ('LONGI_N','float'),
      ('LONGI_N_360','float'),
      ('LONGI_C','text'),
      ('LATI_N','float'),
      ('LATI_C','text'),
      ('FACE','text'),
      ('QUADRANT','text'),
      ('AREA','text'),
      ('RUKL','text'),
      ('RUKL_C','text'),
      ('VISCARDY','text'),
      ('HATFIELD','text'),
      ('WESTFALL','text'),
      ('WOOD','text'),
      ('LOPAM','text'),
      ('LENGTH_KM','float'),
      ('WIDE_KM','float'),
      ('LENGTH_MI','float'),
      ('WIDE_MI','float'),
      ('LENGTH_ARCSEC','float'),
      ('HEIGHT_M','float'),
      ('HEIGHT_FE','float'),
      ('RAPPORT','float'),
      ('PROFIL','text'),
      ('FLOOR_DIAMETER_KM','float'),
      ('PEAK_HEIGHT_KM','float'),
      ('PEAK_DIAMETER_KM','float'),
      ('EXCAVATION_DEPTH_KM','float'),
      ('MELTING_DEPTH_KM','float'),
      ('EJECTA_THICK_M_1RADIUS','float'),
      ('EJECTA_THICK_M_3RADIUS','float'),
      ('EJECTA_THICK_M_5RADIUS','float'),
      ('RADAR_BRIGHT_HALO_RADIUS','float'),
      ('RADAR_DARK_HALO_RADIUS','float'),
      ('GENERAL1','text'),
      ('GENERAL2','text'),
      ('SLOPES','text'),
      ('WALLS','text'),
      ('FLOOR','text'),
      ('ELGER_1895','text'),
      ('INTEREST_N','integer'),
      ('INTEREST_C','text'),
      ('LUNATION','integer'),
      ('MOONDAY_S','text'),
      ('MOONDAY_M','text'),
      ('DIAM_INST','integer'),
      ('TH_INSTRU','text'),
      ('PR_INSTRU','text'),
      ('IAU_FEATURE_NAME','text'),
      ('IAU_CLEAN_FEATURE_NAME','text'),
      ('IAU_FEATURE_ID','text'),
      ('IAU_DIAMETER','text'),
      ('IAU_CENTER_LATITUDE','text'),
      ('IAU_CENTER_LONGITUDE','text'),
      ('IAU_NORTHERN_LATITUDE','text'),
      ('IAU_SOUTHERN_LATITUDE','text'),
      ('IAU_EASTERN_LONGITUDE','text'),
      ('IAU_WESTERN_LONGITUDE','text'),
      ('IAU_COORDINATE_SYSTEM','text'),
      ('IAU_CONTINENT','text'),
      ('IAU_ETHNICITY','text'),
      ('IAU_FEATURE_TYPE','text'),
      ('IAU_FEATURE_TYPE_CODE','text'),
      ('IAU_QUAD_NAME','text'),
      ('IAU_QUAD_CODE','text'),
      ('IAU_APPROVAL_STATUS','text'),
      ('IAU_APPROVAL_DATE','text'),
      ('IAU_REFERENCE','text'),
      ('IAU_ORIGIN','text'),
      ('IAU_LINK','text')
      );
      FDBN=1;
      FNAME=2;
      FLONGIN=29;
      FLATIN=32;
var
    sidelist: string;
    database : array[1..9] of string;
    usedatabase :array[1..MaxDBN] of boolean;

Procedure LoadDB(dbm: TLiteDB);
Procedure CreateDB(dbm: TLiteDB);
Procedure ConvertDB(dbm: TLiteDB; fn,side:string);
procedure DBjournal(dbname,txt:string);

implementation

Uses  u_constant, u_util, fmsg;

Procedure CreateDB(dbm: TLiteDB);
var i,dbv: integer;
    cmd,buf: string;
begin
buf:=dbm.QueryOne('select version from dbversion;');
dbv:=StrToIntDef(buf,0);
if dbv<DBversion then begin
 dbm.Query('drop table moon;');
 dbm.Query('drop table file_date;');
 dbm.Query('drop table user_database;');
 dbm.Query('drop table dbversion;');
 dbm.Commit;
 dbm.Query('Vacuum;');
 cmd:='create table moon ( '+
    'ID INTEGER PRIMARY KEY,'+
    'DBN integer';
 for i:=1 to NumMoonDBFields do begin
   cmd:=cmd+','+MoonDBFields[i,1]+' '+MoonDBFields[i,2];
 end;
 cmd:=cmd+');';
 dbm.Query(cmd);
 if dbm.LastError<>0 then dbjournal(extractfilename(dbm.database),copy(cmd,1,60)+'...  Error: '+dbm.ErrorMessage);
 dbm.Query('create index moon_pos on moon (long_in,lat_in);');
 dbm.Query('create index moon_name on moon (dbn,name);');
 dbjournal(extractfilename(dbm.database),'CREATE TABLE MOON');

 cmd:='create table file_date ( '+
    'DBN integer,'+
    'FDATE integer'+
    ');';
 dbm.Query(cmd);
 if dbm.LastError<>0 then dbjournal(extractfilename(dbm.database),copy(cmd,1,60)+'...  Error: '+dbm.ErrorMessage);
 dbjournal(extractfilename(dbm.database),'CREATE TABLE FILE_DATE');

 cmd:='create table user_database ( '+
    'DBN integer,'+
    'NAME text'+
    ');';
 dbm.Query(cmd);
 if dbm.LastError<>0 then dbjournal(extractfilename(dbm.database),copy(cmd,1,60)+'...  Error: '+dbm.ErrorMessage);
 dbjournal(extractfilename(dbm.database),'CREATE TABLE USER_DATABASE');

 cmd:='create table dbversion ( '+
    'version integer'+
    ');';
 dbm.Query(cmd);
 cmd:='insert into dbversion values('+inttostr(DBversion)+');';
 dbm.Query(cmd);
 if dbm.LastError<>0 then dbjournal(extractfilename(dbm.database),copy(cmd,1,60)+'...  Error: '+dbm.ErrorMessage);
 dbjournal(extractfilename(dbm.database),'CREATE TABLE DBVERSION');
end;
end;

Procedure ConvertDB(dbm: TLiteDB; fn,side:string);
var cmd,v: string;
    i,imax,ii:integer;
    db1:Tmlb2;
begin
if MsgForm=nil then Application.CreateForm(TMsgForm, MsgForm);
MsgForm.Label1.caption:=ExtractFileName(fn)+crlf+'Preparing Database. Please Wait ...';
msgform.show;
msgform.Refresh;
application.ProcessMessages;
db1:=Tmlb2.Create;
try
db1.init;
db1.LoadFromFile(fn);
db1.GoFirst;
dbm.StartTransaction;
dbm.Query('delete from moon where DBN='+side+';');
dbm.Commit;
dbjournal(extractfilename(dbm.database),'DELETE ALL DBN='+side);
v:='';
for i:=1 to NumMoonDBFields do begin
  ii:=db1.GetFieldIndex(MoonDBFields[i,1]);
  if ii=0 then v:=v+MoonDBFields[i,1]+'; ';
end;
if v>'' then dbjournal(extractfilename(dbm.database), fn+' missing fields: '+v);
dbm.StartTransaction;
repeat
  cmd:='insert into moon values(NULL,'+side+',';
  for i:=1 to NumMoonDBFields do begin
    v:=db1.GetData(MoonDBFields[i,1]);
    v:=stringreplace(v,',','.',[rfreplaceall]); // look why we need that ???
    v:=stringreplace(v,'""','''',[rfreplaceall]);
    v:=stringreplace(v,'"','',[rfreplaceall]);
    cmd:=cmd+'"'+v+'",';
  end;
  cmd:=copy(cmd,1,length(cmd)-1)+');';
  dbm.Query(cmd);
  if dbm.LastError<>0 then dbjournal(extractfilename(dbm.database),copy(cmd,1,60)+'...  Error: '+dbm.ErrorMessage);
  db1.GoNext;
until db1.EndOfFile;
imax:=dbm.GetLastInsertID;
dbm.Query('update moon set wide_km=0 where wide_km="";');
dbm.Query('update moon set wide_km=0 where wide_km="?";');
dbm.Query('update moon set wide_mi=0 where wide_mi="";');
dbm.Query('update moon set wide_mi=0 where wide_mi="?";');
dbm.Query('update moon set length_km=0 where length_km="";');
dbm.Query('update moon set length_km=0 where length_km="?";');
dbm.Query('update moon set length_mi=0 where length_mi="";');
dbm.Query('update moon set length_mi=0 where length_mi="?";');
dbm.Query('delete from file_date where dbn='+side+';');
dbm.Query('insert into file_date values ('+side+','+inttostr(fileage(fn))+');');
dbm.Commit;
dbjournal(extractfilename(dbm.database),'INSERT DBN='+side+' MAX ID='+inttostr(imax));
finally
db1.Clear;
db1.Free;
end;
end;

Procedure LoadDB(dbm: TLiteDB);
var i,db_age : integer;
    buf,missingf:string;
    needvacuum: boolean;
begin
missingf:='';
needvacuum:=false;
buf:=Slash(DBdir)+'dbmoon7'+uplanguage+'.dbl';
dbm.Use(utf8encode(buf));
sidelist:='1';
for i:=2 to maxdbn do if usedatabase[i] then sidelist:=sidelist+','+inttostr(i);
try
buf:=Slash(appdir)+Slash('Database')+'AVL Named EN'+uplanguage+'.csv';
if fileexists(buf) then database[1]:=buf
   else database[1]:=Slash(appdir)+Slash('Database')+'AVL Named EN.csv';

buf:=Slash(appdir)+Slash('Database')+'AVL Satellite '+uplanguage+'.csv';
if fileexists(buf) then database[2]:=buf
   else database[2]:=Slash(appdir)+Slash('Database')+'AVL Satellite EN.csv';

buf:=Slash(appdir)+Slash('Database')+'AVL Registered '+uplanguage+'.csv';
if fileexists(buf) then database[5]:=buf
   else database[5]:=Slash(appdir)+Slash('Database')+'AVL Registered EN.csv';

buf:=Slash(appdir)+Slash('Database')+'AVL Unnamed '+uplanguage+'.csv';
if fileexists(buf) then database[8]:=buf
   else database[8]:=Slash(appdir)+Slash('Database')+'AVL Unnamed EN.csv';

CreateDB(dbm);
for i:=1 to 4 do begin
  if usedatabase[i] then begin
     buf:=dbm.QueryOne('select fdate from file_date where dbn='+inttostr(i)+';');
     if buf='' then db_age:=0 else db_age:=strtoint(buf);
     if fileexists(database[i]) then begin
     if (db_age<fileage(database[i])) then begin
        dbjournal(extractfilename(dbm.database),'LOAD DATABASE DBN='+inttostr(i)+' FROM FILE: '+database[i]+' FILE DATE: '+ DateTimeToStr(FileDateToDateTime(fileage(database[i]))) );
        convertDB(dbm,database[i],inttostr(i));
        needvacuum:=true;
     end;
     end
     else missingf:=missingf+database[i]+blank;
  end;
end;
if needvacuum then dbm.Query('Vacuum;');
finally
if MsgForm<>nil then MsgForm.Close;
if missingf>'' then
   MessageDlg('Some database files are missing, the program may not work correctly.'+crlf+missingf,mtError,[mbClose],0);
end;
end;

procedure DBjournal(dbname,txt:string);
var f : textfile;
    fn: string;
const dbj='database_journal.txt';
begin
fn:=Slash(DBdir)+dbj;
if fileexists(fn) then begin
  assignfile(f,fn);
  append(f);
end else begin
  assignfile(f,fn);
  rewrite(f);
end;
writeln(f,FormatDateTime('yyyy"-"mm"-"dd" "hh":"nn":"ss',Now),' DB=',dbname,' ',txt);
closefile(f);
end;

end.
