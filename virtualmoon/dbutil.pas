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
    DBversion=600;
    MaxDBN=200;
    NumMoonDBFields = 82;
    MoonDBFields : array[1..NumMoonDBFields,1..2] of string = (
      ('NAME','text'),
      ('LUN','text'),
      ('LUN_REDUCED','text'),
      ('NAMETYPE','text'),
      ('TYPE','text'),
      ('SUBTYPE','text'),
      ('PERIOD','text'),
      ('PROCESS','text'),  
      ('GEOLOGY','text'),
      ('NAMEDETAIL','text'),
      ('NAMEORIGIN','text'),
      ('LANGRENUS','text'),
      ('HEVELIUS','text'),
      ('RICCIOLI','text'),
      ('WORK','text'),
      ('COUNTRY','text'),
      ('NATIONLITY','text'),
      ('CENTURYN','float'),
      ('CENTURYC','text'),
      ('BIRTHPLACE','text'),
      ('BIRTHDATE','text'),
      ('DEATHPLACE','text'),
      ('DEATHDATE','text'),
      ('FACTS','text'),
      ('LONGIN','float'),
      ('LONGIN360','float'),
      ('LONGIC','text'),
      ('LATIN','float'),
      ('LATIC','text'),
      ('FACE','text'),
      ('QUADRANT','text'),
      ('AREA','text'),
      ('RUKL','text'),
      ('RUKLC','text'),
      ('VISCARDY','text'),
      ('HATFIELD','text'),
      ('WESTFALL','text'),
      ('WOOD','text'),
      ('LOPAM','text'),
      ('LENGTHKM','float'),
      ('WIDEKM','float'),
      ('LENGTHMI','float'),
      ('WIDEMI','float'),
      ('HEIGHTM','float'),
      ('HEIGHTFE','float'),
      ('RAPPORT','float'),
      ('PROFIL','text'),
      ('GENERAL','text'),
      ('SLOPES','text'),
      ('WALLS','text'),
      ('FLOOR','text'),
      ('TIPS','text'),              
      ('INTERESTN','integer'),
      ('INTERESTC','text'),
      ('LUNATION','integer'),
      ('MOONDAYS','text'),
      ('MOONDAYM','text'),
      ('DIAMINST','integer'),
      ('THINSTRU','text'),
      ('PRINSTRU','text'),
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
      FLONGIN=26;
      FLATIN=29;
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
 dbm.Query('create index moon_pos on moon (longin,latin);');
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
dbm.Query('update moon set widekm=0 where widekm="";');
dbm.Query('update moon set widekm=0 where widekm="?";');
dbm.Query('update moon set widemi=0 where widemi="";');
dbm.Query('update moon set widemi=0 where widemi="?";');
dbm.Query('update moon set lengthkm=0 where lengthkm="";');
dbm.Query('update moon set lengthkm=0 where lengthkm="?";');
dbm.Query('update moon set lengthmi=0 where lengthmi="";');
dbm.Query('update moon set lengthmi=0 where lengthmi="?";');
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
buf:=Slash(DBdir)+'dbmoon6_u'+uplanguage+'.dbl';
dbm.Use(utf8encode(buf));
sidelist:='1';
for i:=2 to maxdbn do if usedatabase[i] then sidelist:=sidelist+','+inttostr(i);
try
buf:=Slash(appdir)+Slash('Database')+'Nearside_Named_u'+uplanguage+'.csv';
if fileexists(buf) then database[1]:=buf
   else database[1]:=Slash(appdir)+Slash('Database')+'Nearside_Named_uEN.csv';
buf:=Slash(appdir)+Slash('Database')+'Nearside_Satellite_u'+uplanguage+'.csv';
if fileexists(buf) then database[2]:=buf
   else database[2]:=Slash(appdir)+Slash('Database')+'Nearside_Satellite_uEN.csv';
buf:=Slash(appdir)+Slash('Database')+'Farside_Named_u'+uplanguage+'.csv';
if fileexists(buf) then database[3]:=buf
   else database[3]:=Slash(appdir)+Slash('Database')+'Farside_Named_uEN.csv';
buf:=Slash(appdir)+Slash('Database')+'Farside_Satellite_u'+uplanguage+'.csv';
if fileexists(buf) then database[4]:=buf
   else database[4]:=Slash(appdir)+Slash('Database')+'Farside_Satellite_uEN.csv';
buf:=Slash(appdir)+Slash('Database')+'Historical_u'+uplanguage+'.csv';
if fileexists(buf) then database[5]:=buf
   else database[5]:=Slash(appdir)+Slash('Database')+'Historical_uEN.csv';
buf:=Slash(appdir)+Slash('Database')+'Pyroclastic_u'+uplanguage+'.csv';
if fileexists(buf) then database[6]:=buf
   else database[6]:=Slash(appdir)+Slash('Database')+'Pyroclastic_uEN.csv';
buf:=Slash(appdir)+Slash('Database')+'Domes_u'+uplanguage+'.csv';
if fileexists(buf) then database[7]:=buf
   else database[7]:=Slash(appdir)+Slash('Database')+'Domes_uEN.csv';
buf:=Slash(appdir)+Slash('Database')+'Nearside_Unnamed_u'+uplanguage+'.csv';
if fileexists(buf) then database[8]:=buf
   else database[8]:=Slash(appdir)+Slash('Database')+'Nearside_Unnamed_uEN.csv';
buf:=Slash(appdir)+Slash('Database')+'Farside_Unnamed_u'+uplanguage+'.csv';
if fileexists(buf) then database[9]:=buf
   else database[9]:=Slash(appdir)+Slash('Database')+'Farside_Unnamed_uEN.csv';
CreateDB(dbm);
for i:=1 to 9 do begin
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
