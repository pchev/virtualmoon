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

Uses Forms, Dialogs, Classes, SysUtils, passql, passqlite;

const
    DBversion=800;
    DBname='dbmoon8';
    MaxDB=99;
    MaxDBN=200;
    UserDBN=100;
    NumMoonDBFields = 97;
    MoonDBFields : array[1..NumMoonDBFields,1..2] of string = (
      ('NAME','text'),
      ('LUN','text'),
      ('LUN_REDUCED','text'),
      ('NAME_TYPE','text'),
      ('IAU_TYPE','text'),
      ('TYPE','text'),
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
      ('VISCARDY','text'),
      ('WESTFALL','text'),
      ('WOOD','text'),
      ('LOPAM','text'),
      ('CLEMENTINE','text'),
      ('CENTURY_20TH','text'),
      ('HATFIELD','text'),
      ('REISEATLAS','text'),
      ('CHANGE1','text'),
      ('DISCOVER_MOON','text'),
      ('TIMES','text'),
      ('KAGUYA','text'),
      ('BYRNE_NEAR','text'),
      ('BYRNE_FAR','text'),
      ('SIX_INCH','text'),
      ('DASE','text'),
      ('PAU','text'),
      ('LUNA_COGNITA','text'),
      ('LAC','text'),
      ('LENGTH_KM','float'),
      ('WIDE_KM','float'),
      ('LENGTH_ARCSEC','float'),
      ('HEIGHT_M','float'),
      ('RAPPORT','float'),
      ('PROFIL','text'),
      ('GENERAL_1','text'),
      ('GENERAL_2','text'),
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
    database : array[1..MaxDB] of string;
    usedatabase :array[1..MaxDBN] of boolean;
    ILCDCols: TStringList;

Procedure ListDB;
Procedure LoadDB(dbm: TLiteDB);
Procedure CreateDB(dbm: TLiteDB);
Procedure ConvertDB(dbm: TLiteDB; fn,side:string);
procedure DBjournal(dbname,txt:string);
Procedure LoadILCD(fn,path,tname:string; dbm: TLiteDB);
procedure LoadILCDcols(fn,path,lang:string);

implementation

Uses  u_constant, u_util, fmsg;

Procedure CreateDB(dbm: TLiteDB);
var i,dbv: integer;
    cmd,buf: string;
    ok:boolean;
begin
ok:=dbm.query('select name from moon order by name LIMIT 1;');  // try to detect corrupt database file
buf:=dbm.QueryOne('select version from dbversion;');
dbv:=StrToIntDef(buf,0);
if (dbv<DBversion)or(not ok) then begin
 buf:=dbm.DataBase;
 dbm.Close;
 if FileExists(buf) then DeleteFile(buf);
 dbm.DataBase:=buf;
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
var cmd,v,buf,fnn,dbv: string;
    i,imax,ii,j,n:integer;
    hdr,row: TStringList;
    f: TextFile;
    idx: array[0..NumMoonDBFields] of integer;
begin
if MsgForm=nil then Application.CreateForm(TMsgForm, MsgForm);
MsgForm.Label1.caption:=ExtractFileName(fn)+crlf+'Preparing Database. Please Wait ...';
msgform.show;
msgform.Refresh;
application.ProcessMessages;
hdr:=TStringList.Create;
row:=TStringList.Create;
dbv:=dbm.QueryOne('select version from dbversion;');
dbm.Query('update dbversion set version=0;');
try
AssignFile(f,fn);
Reset(f);
ReadLn(f,buf);
SplitRec2(buf,';',hdr);
dbm.Query('PRAGMA journal_mode = MEMORY');
dbm.Query('PRAGMA synchronous = OFF');
dbm.StartTransaction;
dbm.Query('delete from moon where DBN='+side+';');
dbm.Commit;
dbjournal(extractfilename(dbm.database),'DELETE ALL DBN='+side);
v:='';
for i:=1 to NumMoonDBFields do begin
  ii:=hdr.IndexOf(MoonDBFields[i,1]);
  idx[i]:=ii;
  if ii<0 then v:=v+MoonDBFields[i,1]+'; ';
end;
if v>'' then dbjournal(extractfilename(dbm.database), fn+' missing fields: '+v);
dbm.StartTransaction;
n:=0;   // single file
j:=0;
fnn:=fn;
repeat
  repeat
    ReadLn(f,buf);
    SplitRec2(buf,';',row);
    if row.Count<>hdr.Count then begin
      dbjournal(extractfilename(fnn), ' skip bad record: '+copy(buf,1,20)+' fields='+inttostr(row.Count)+' expected='+inttostr(hdr.Count));
      continue;
    end;
    cmd:='insert into moon values(NULL,'+side+',';
    for i:=1 to NumMoonDBFields do begin
      if idx[i]>=0 then
        v:=row[idx[i]]
      else
        v:='';
      v:=stringreplace(v,',','.',[rfreplaceall]);
      v:=stringreplace(v,'""','''',[rfreplaceall]);
      v:=stringreplace(v,'"','',[rfreplaceall]);
      cmd:=cmd+'"'+v+'",';
    end;
    cmd:=copy(cmd,1,length(cmd)-1)+');';
    dbm.Query(cmd);
    if dbm.LastError<>0 then dbjournal(extractfilename(dbm.database),copy(cmd,1,60)+'...  Error: '+dbm.ErrorMessage);
  until EOF(f);
  CloseFile(f);
  inc(j);
  if j<=n then begin
    fnn:=StringReplace(fn,'-0','-'+inttostr(j),[]);
    dbjournal(extractfilename(dbm.database),'Process file: '+fnn);
    MsgForm.Label1.caption:=ExtractFileName(fnn)+crlf+'Preparing Database. Please Wait ...';
    MsgForm.Refresh;
    application.ProcessMessages;
    AssignFile(f,fnn);
    Reset(f);
  end;
until j>n;
imax:=dbm.GetLastInsertID;
dbm.Query('update moon set wide_km=0 where wide_km="";');
dbm.Query('update moon set wide_km=0 where wide_km="?";');
dbm.Query('update moon set length_km=0 where length_km="";');
dbm.Query('update moon set length_km=0 where length_km="?";');
dbm.Query('delete from file_date where dbn='+side+';');
dbm.Query('insert into file_date values ('+side+','+inttostr(fileage(fn))+');');
dbm.Query('update dbversion set version='+dbv+';');
dbm.Commit;
dbjournal(extractfilename(dbm.database),'INSERT DBN='+side+' MAX ID='+inttostr(imax));
finally
hdr.Free;
row.Free;
dbm.Query('PRAGMA journal_mode = DELETE');
dbm.Query('PRAGMA synchronous = NORMAL');
end;
end;

Procedure ListDB;
var f:    Tsearchrec;
    i,j,p: integer;
    dben: TStringList;
    buf,nb: string;
begin
{ Mandatory database file pattern:
  - number between 1 and 9
  - underscore character _
  - name of the database to show in list. Can be translated. _ replaced by space in list.
  - underscore character _
  - uppercase language code, two character
  - .csv

  Example:
    1_Nearside_Named_EN.csv
    7_Historical_EN.csv
    7_Historique_FR.csv
}
  DatabaseList.Clear;
  DatabaseList.Sorted:=true;
  dben:=TStringList.Create;
  dben.Sorted:=true;
  i:=findfirst(Slash(appdir)+Slash('Database')+'*_*_EN.csv', faNormal, f);
  while (i=0) do begin
    dben.Add(f.Name);
    i:=FindNext(f);
  end;
  findclose(f);
  if uplanguage='EN' then begin
    DatabaseList.Assign(dben);
  end
  else begin
    for i:=0 to dben.Count-1 do begin
       p:=pos('_',dben[i]);
       nb:=copy(dben[i],1,p-1);
       j:=findfirst(Slash(appdir)+Slash('Database')+nb+'_*_'+uplanguage+'.csv', faNormal, f);
       if j=0 then
         DatabaseList.add(f.Name)
       else
         DatabaseList.add(dben[i]);
       FindClose(f);
    end;
  end;
  DatabaseList.Sorted:=false;
  for i:=1 to MaxDB do
    database[i]:='';
  for i:=0 to DatabaseList.Count-1 do begin
    buf:=DatabaseList[i];
    database[i+1]:=Slash(appdir)+Slash('Database')+buf;
    p:=pos('_',dben[i]);
    Delete(buf,1,p);
    Delete(buf,Length(buf)-6,7);
    buf:=StringReplace(buf,'_',' ',[rfReplaceAll]);
    DatabaseList[i]:=buf;
  end;
  dben.Free;
end;

Procedure LoadDB(dbm: TLiteDB);
var i,db_age : integer;
    buf,missingf:string;
    needvacuum: boolean;
begin
missingf:='';
needvacuum:=false;
buf:=Slash(DBdir)+DBname+uplanguage+'.dbl';
dbm.Use(utf8encode(buf));
try
ListDB;
CreateDB(dbm);
sidelist:='1';
for i:=2 to maxdbn do if usedatabase[i] and (database[i]<>'') then sidelist:=sidelist+','+inttostr(i);
for i:=1 to MaxDB do begin
  if usedatabase[i] and (database[i]<>'') then begin
     if (pos('_Unnamed',database[i])>0)or(pos('_non_nommées',database[i])>0) then
       UnnamedList:=UnnamedList+' '+inttostr(i)+' ';
     buf:=dbm.QueryOne('select fdate from file_date where dbn='+inttostr(i)+';');
     if buf='' then db_age:=0 else db_age:=strtoint(buf);
     if fileexists(database[i]) then begin
     if (db_age<fileage(database[i])) then begin
        dbjournal(extractfilename(dbm.database),'LOAD DATABASE DBN='+inttostr(i)+' FROM FILE: '+database[i]+' FILE DATE: '+ DateTimeToStr(FileDateToDateTime(fileage(database[i]))) );
        convertDB(dbm,database[i],inttostr(i));
        needvacuum:=true;
     end;
     end
     else begin
       usedatabase[i]:=false;
       if i<>5 then missingf:=missingf+database[i]+blank;
     end;
  end
  else usedatabase[i]:=false;
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

Procedure LoadILCD(fn,path,tname:string; dbm: TLiteDB);
var buf,cols,cmd,v: string;
    i:integer;
    col,val: TStringList;
    f: textfile;
const colcount=80;
begin
 if FileExists(slash(path)+fn+'.csv') then begin
    cmd:='select * from '+tname+' limit 1';
    dbm.Query(cmd);
    if dbm.LastError<>0 then begin
      col:=TStringList.Create;
      val:=TStringList.Create;
      try
      AssignFile(f,slash(path)+fn+'.csv');
      Reset(f);
      ReadLn(f,cols);
      SplitRec2(cols,';',col);
      if col.Count<colcount then exit;
      dbjournal(extractfilename(dbm.database),'CREATE TABLE '+tname);
      cmd:='create table '+tname+' ( ';
      for i:=0 to colcount-1 do begin
        cmd:=cmd+col[i]+' text,';
      end;
      cmd:=copy(cmd,1,length(cmd)-1);
      cmd:=cmd+');';
      dbm.Query(cmd);
      if dbm.LastError<>0 then begin
        dbjournal(extractfilename(dbm.database),copy(cmd,1,60)+'...  Error: '+dbm.ErrorMessage);
        exit;
      end;
      cmd:='create index '+tname+'_idx on '+tname+'('+col[0]+');';
      dbm.Query(cmd);
      if dbm.LastError<>0 then begin
        dbjournal(extractfilename(dbm.database),copy(cmd,1,60)+'...  Error: '+dbm.ErrorMessage);
        exit;
      end;
      while not eof(f) do begin
        ReadLn(f,buf);
        SplitRec2(buf,';',val);
        if val.Count<colcount then exit;
        cmd:='insert into '+tname+' values (';
        for i:=0 to colcount-1 do begin
          v:=val[i];
          v:=stringreplace(v,'""','''',[rfreplaceall]);
          v:=stringreplace(v,'"','',[rfreplaceall]);
          cmd:=cmd+'"'+v+'",';
        end;
        cmd:=copy(cmd,1,length(cmd)-1);
        cmd:=cmd+');';
        dbm.Query(cmd);
        if dbm.LastError<>0 then begin
           dbjournal(extractfilename(dbm.database),copy(cmd,1,60)+'...  Error: '+dbm.ErrorMessage);
           break;
        end;
      end;
      CloseFile(f);
      finally
       col.Free;
       val.Free;
      end;
    end;
 end;
end;

procedure LoadILCDcols(fn,path,lang:string);
var fname,buf:string;
    f:TextFile;
begin
  if ILCDCols=nil then ILCDCols:=TStringList.Create;
  fname:=slash(path)+fn+'_'+lang+'.txt';
  if not FileExists(fname) then begin
    fname:=slash(path)+fn+'_EN.txt';
    if not FileExists(fname) then
      fname:=slash(path)+fn+'.csv';
  end;
  if FileExists(fname) then begin
    AssignFile(f,fname);
    Reset(f);
    ReadLn(f,buf);
    CloseFile(f);
    SplitRec2(buf,';',ILCDCols);
  end;
end;

end.
