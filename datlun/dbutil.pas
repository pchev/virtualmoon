unit dbutil;
{
  This file is to be maintened in datlun
  and copied to virtualmoon if changed
}
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
{$MODE objfpc}
{$H+}

interface

Uses Forms, Dialogs, SysUtils, mlb2, passql, passqlite;

const
    MaxDBN=100;

var
    sidelist: string;
    database : array[1..7] of string;
    usedatabase :array[1..MaxDBN] of boolean;

Procedure LoadDB(dbm: TLiteDB);
Procedure CreateDB(dbm: TLiteDB);
Procedure ConvertDB(dbm: TLiteDB; fn,side:string);
procedure DBjournal(dbname,txt:string);

implementation

Uses  u_constant, u_util, fmsg;

Procedure CreateDB(dbm: TLiteDB);
var i: integer;
    buf,cmd: string;
begin
buf:='';
dbm.query('select name from sqlite_master where type="table";');
for i:=0 to dbm.RowCount do buf:=buf+dbm.Results[i][0]+' ';
if pos('moon',buf)=0 then begin
 cmd:='create table moon ( '+
    'ID INTEGER PRIMARY KEY,'+
    'DBN integer,'+
    'NAME text,'+
    'TYPE text,'+
    'PERIOD text,'+
    'NAMEDETAIL text,'+
    'NAMEORIGIN text,'+
    'LANGRENUS text,'+
    'HEVELIUS text,'+
    'RICCIOLI text,'+
    'WORK text,'+
    'COUNTRY text,'+
    'NATIONLITY text,'+
    'CENTURYN float,'+
    'CENTURYC text,'+
    'BIRTHPLACE text,'+
    'BIRTHDATE text,'+
    'DEATHPLACE text,'+
    'DEATHDATE text,'+
    'FACTS text,'+
    'LONGIN float,'+
    'LONGIC text,'+
    'LATIN float,'+
    'LATIC text,'+
    'QUADRANT text,'+
    'AREA text,'+
    'RUKL text,'+
    'RUKLC text,'+
    'VISCARDY text,'+
    'HATFIELD text,'+
    'WESTFALL text,'+
    'WOOD text,'+
    'LOPAM text,'+
    'LENGTHKM float,'+
    'WIDEKM float,'+
    'LENGTHMI float,'+
    'WIDEMI float,'+
    'HEIGHTM float,'+
    'HEIGHTFE float,'+
    'RAPPORT float,'+
    'PROFIL text,'+
    'GENERAL text,'+
    'SLOPES text,'+
    'WALLS text,'+
    'FLOOR text,'+
    'INTERESTN integer,'+
    'INTERESTC text,'+
    'LUNATION integer,'+
    'MOONDAYS text,'+
    'MOONDAYM text,'+
    'DIAMINST integer,'+
    'THINSTRU text,'+
    'PRINSTRU text'+
    ');';
 dbm.Query(cmd);
 dbm.Query('create index moon_pos on moon (longin,latin);');
 dbm.Query('create index moon_name on moon (dbn,name);');
 dbjournal(extractfilename(dbm.database),'CREATE TABLE MOON');
end;
if pos('file_date',buf)=0 then begin
 cmd:='create table file_date ( '+
    'DBN integer,'+
    'FDATE integer'+
    ');';
 dbm.Query(cmd);
 dbjournal(extractfilename(dbm.database),'CREATE TABLE FILE_DATE');
end;
if pos('user_database',buf)=0 then begin
 cmd:='create table user_database ( '+
    'DBN integer,'+
    'NAME text'+
    ');';
 dbm.Query(cmd);
 dbjournal(extractfilename(dbm.database),'CREATE TABLE USER_DATABASE');
end;
end;

Procedure ConvertDB(dbm: TLiteDB; fn,side:string);
var cmd,v: string;
    i,imax:integer;
    db1:Tmlb2;
begin
if MsgForm=nil then Application.CreateForm(TMsgForm, MsgForm);
//MsgForm:=TMsgForm.create(nil);
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
dbm.Query('Vacuum;');
dbjournal(extractfilename(dbm.database),'DELETE ALL DBN='+side);
dbm.StartTransaction;
repeat
  cmd:='insert into moon values(NULL,'+side+',';
  for i:=1 to db1.FieldCount do begin
    v:=db1.GetDataByIndex(i);
    v:=stringreplace(v,',','.',[rfreplaceall]); // look why we need that ???
    v:=stringreplace(v,'""','''',[rfreplaceall]);
    v:=stringreplace(v,'"','',[rfreplaceall]);
    cmd:=cmd+'"'+v+'",';
  end;
  cmd:=copy(cmd,1,length(cmd)-1)+');';
  dbm.Query(cmd);
//  showmessage(dbm.ErrorMessage);
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
//msgform.close;
//msgform.free;
db1.Clear;
db1.Free;
end;
end;

Procedure LoadDB(dbm: TLiteDB);
var i,db_age : integer;
    buf:string;
begin
buf:=Slash(DBdir)+'dbmoon3_u'+language+'.dbl';
dbm.Use(utf8encode(buf));
sidelist:='1';
for i:=2 to maxdbn do if usedatabase[i] then sidelist:=sidelist+','+inttostr(i);
try
buf:=Slash(appdir)+Slash('Database')+'Nearside_Named_u'+language+'.csv';
if fileexists(buf) then database[1]:=buf
   else database[1]:=Slash(appdir)+Slash('Database')+'Nearside_Named_uen.csv';
buf:=Slash(appdir)+Slash('Database')+'Nearside_Satellite_u'+language+'.csv';
if fileexists(buf) then database[2]:=buf
   else database[2]:=Slash(appdir)+Slash('Database')+'Nearside_Satellite_uen.csv';
buf:=Slash(appdir)+Slash('Database')+'Farside_Named_u'+language+'.csv';
if fileexists(buf) then database[3]:=buf
   else database[3]:=Slash(appdir)+Slash('Database')+'Farside_Named_uen.csv';
buf:=Slash(appdir)+Slash('Database')+'Farside_Satellite_u'+language+'.csv';
if fileexists(buf) then database[4]:=buf
   else database[4]:=Slash(appdir)+Slash('Database')+'Farside_Satellite_uen.csv';
buf:=Slash(appdir)+Slash('Database')+'Historical_u'+language+'.csv';
if fileexists(buf) then database[5]:=buf
   else database[5]:=Slash(appdir)+Slash('Database')+'Historical_uen.csv';
buf:=Slash(appdir)+Slash('Database')+'Pyroclastic_u'+language+'.csv';
if fileexists(buf) then database[6]:=buf
   else database[6]:=Slash(appdir)+Slash('Database')+'Pyroclastic_uen.csv';
buf:=Slash(appdir)+Slash('Database')+'Domes_u'+language+'.csv';
if fileexists(buf) then database[7]:=buf
   else database[7]:=Slash(appdir)+Slash('Database')+'Domes_uen.csv';
CreateDB(dbm);
for i:=1 to 7 do begin
  if usedatabase[i] then begin
     buf:=dbm.QueryOne('select fdate from file_date where dbn='+inttostr(i)+';');
     if buf='' then db_age:=0 else db_age:=strtoint(buf);
     if fileexists(database[i]) and (db_age<fileage(database[i])) then begin
        dbjournal(extractfilename(dbm.database),'LOAD DATABASE DBN='+inttostr(i)+' FROM FILE: '+database[i]+' FILE DATE: '+ DateTimeToStr(FileDateToDateTime(fileage(database[i]))) );
        convertDB(dbm,database[i],inttostr(i));
     end;
  end;
end;               
finally
if MsgForm<>nil then MsgForm.Close;
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
