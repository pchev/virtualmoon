unit dbutil;

interface

Uses SysUtils, mlb2, passql, passqlite;

const
    MaxDBN=100;

var
    sidelist: string;
    database : array[1..5] of string;
    usedatabase :array[1..MaxDBN] of boolean;

Procedure LoadDB(dbm: TLiteDB);
Procedure CreateDB(dbm: TLiteDB);
Procedure ConvertDB(dbm: TLiteDB; fn,side:string);
procedure DBjournal(dbm: TLiteDB; txt:string);

implementation

Uses  Skylib, fmsg;

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
 dbjournal(dbm,'CREATE TABLE MOON');
end;
if pos('file_date',buf)=0 then begin
 cmd:='create table file_date ( '+
    'DBN integer,'+
    'FDATE integer'+
    ');';
 dbm.Query(cmd);
 dbjournal(dbm,'CREATE TABLE FILE_DATE');
end;
if pos('user_database',buf)=0 then begin
 cmd:='create table user_database ( '+
    'DBN integer,'+
    'NAME text'+
    ');';
 dbm.Query(cmd);
 dbjournal(dbm,'CREATE TABLE USER_DATABASE');
end;
end;

Procedure ConvertDB(dbm: TLiteDB; fn,side:string);
var cmd,v: string;
    i,imax:integer;
    db1:Tmlb2;
begin
MsgForm:=TMsgForm.create(nil);
MsgForm.Label1.caption:='Preparing Database. Please Wait ...';
msgform.show;
msgform.Refresh;
db1:=Tmlb2.Create;
try
db1.init;
db1.LoadFromFile(fn);
db1.GoFirst;
dbm.StartTransaction;
dbm.Query('delete from moon where DBN='+side+';');
dbm.Query('Vacuum;');
dbm.Commit;
dbjournal(dbm,'DELETE ALL DBN='+side);
dbm.StartTransaction;
repeat
  cmd:='insert into moon values(NULL,'+side+',';
  for i:=1 to db1.FieldCount do begin
    v:=stringreplace(db1.GetDataByIndex(i),',','.',[rfreplaceall]); // look why we need that ???
    v:=stringreplace(v,'""','''',[rfreplaceall]);
    v:=stringreplace(v,'"','',[rfreplaceall]);
    cmd:=cmd+'"'+v+'",';
  end;
  cmd:=copy(cmd,1,length(cmd)-1)+');';
  dbm.Query(cmd);
  db1.GoNext;
until db1.EndOfFile;
imax:=dbm.GetLastInsertID;
dbm.Query('update moon set widekm=0 where widekm="";');
dbm.Query('update moon set widemi=0 where widemi="";');
dbm.Query('update moon set lengthkm=0 where lengthkm="";');
dbm.Query('update moon set lengthmi=0 where lengthmi="";');
dbm.Query('delete from file_date where dbn='+side+';');
dbm.Query('insert into file_date values ('+side+','+inttostr(fileage(fn))+');');
dbm.Commit;
dbjournal(dbm,'INSERT DBN='+side+' MAX ID='+inttostr(imax));
finally
msgform.close;
msgform.free;
db1.Free;
end;
end;

Procedure LoadDB(dbm: TLiteDB);
var i,db_age : integer;
    buf:string;
begin
buf:=Slash(appdir)+Slash('database')+'dbmoon3_'+language+'.dbl';
dbm.Use(buf);
sidelist:='1';
for i:=2 to maxdbn do if usedatabase[i] then sidelist:=sidelist+','+inttostr(i);
try
buf:=Slash(appdir)+Slash('database')+'Nearside_Named_'+language+'.csv';
if fileexists(buf) then database[1]:=buf
   else database[1]:=Slash(appdir)+Slash('database')+'Nearside_Named_UK.csv';
buf:=Slash(appdir)+Slash('database')+'Nearside_satellite_'+language+'.csv';
if fileexists(buf) then database[2]:=buf
   else database[2]:=Slash(appdir)+Slash('database')+'Nearside_Satellite_UK.csv';
buf:=Slash(appdir)+Slash('database')+'Farside_named_'+language+'.csv';
if fileexists(buf) then database[3]:=buf
   else database[3]:=Slash(appdir)+Slash('database')+'Farside_Named_UK.csv';
buf:=Slash(appdir)+Slash('database')+'Farside_satellite_'+language+'.csv';
if fileexists(buf) then database[4]:=buf
   else database[4]:=Slash(appdir)+Slash('database')+'Farside_Satellite_UK.csv';
buf:=Slash(appdir)+Slash('database')+'Historical_'+language+'.csv';
if fileexists(buf) then database[5]:=buf
   else database[5]:=Slash(appdir)+Slash('database')+'Historical_UK.csv';
CreateDB(dbm);
for i:=1 to 5 do begin
  if usedatabase[i] then begin
     buf:=dbm.QueryOne('select fdate from file_date where dbn='+inttostr(i)+';');
     if buf='' then db_age:=0 else db_age:=strtoint(buf);
     if fileexists(database[i]) and (db_age<fileage(database[i])) then begin
        dbjournal(dbm,'LOAD DATABASE DBN='+inttostr(i)+' FROM FILE: '+database[i]+' FILE DATE: '+ DateTimeToStr(FileDateToDateTime(fileage(database[i]))) );
        convertDB(dbm,database[i],inttostr(i));
     end;
  end;
end;
finally
end;
end;

procedure DBjournal(dbm: TLiteDB; txt:string);
var f : textfile;
    fn: string;
const dbj='database_journal.txt';
begin
fn:=Slash(appdir)+Slash('database')+dbj;
if fileexists(fn) then begin
  assignfile(f,fn);
  append(f);
end else begin
  assignfile(f,fn);
  rewrite(f);
end;
writeln(f,FormatDateTime('yyyy"-"mm"-"dd" "hh":"nn":"ss',Now),' DB=',extractfilename(dbm.database),' ',txt);
closefile(f);
end;

end.