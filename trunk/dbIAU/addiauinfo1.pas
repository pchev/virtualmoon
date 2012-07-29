unit addiauinfo1;

{$mode objfpc}{$H+}

interface

uses  mlb2, Math,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    dblang: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label0: TLabel;
    Label7: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    dbiau,dbnn,dbns,dbfn,dbfs,dbnnfr,dbnsfr,dbfnfr,dbfsfr: TMlb2;
    vdbnn,vdbns,vdbfn,vdbfs: boolean;
    matchnn,matchns,matchfn,matchfs,matchnew,nomatch,multimatch,niau,ilang: integer;
    addlun,addlunred,addnametype,addsubtype,addprocess,addgeology,addlong360,addface,addtips : boolean;
    lang,currentname:string;
    f0,f1,f2,fstat,fdiam,fkmmi,frepsize: textfile;
    avlname,clname,rname,newname,newdb,delname: TStringList;
    procedure LoadCleanName;
    procedure LoadNewName;
    procedure OpenDB;
    procedure CloseDB;
    procedure AddIAUFields(db: TMlb2);
    procedure AddIAUData(db: TMlb2);
    procedure AddNewFields(var db: TMlb2);
    function AddNewName(n: string):boolean;
    procedure FixSize(var db,dbfr: TMlb2; dbn:string);
    procedure LoadDelName;
    Procedure AddTmpName(db:TMlb2);
    Procedure DelNonIAU(db:TMlb2);
    Procedure DelTmpName(db:TMlb2);
    function NameMatch(n: string; db:TMlb2):boolean;
    function CleanName(str:string):string;
    function SafeData(str:string):string;
    function wordspace(str:string):string;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

const
  blank=' ';
  tab=chr(9);
  f6='0.000000';
  f3='0.000';
  quadrant : array[0..4,0..1] of string = (
    ('Nord-Est','North-East'),
    ('Nord-Ouest','North-West'),
    ('Sud-Est','South-East'),
    ('Sud-Ouest','South-West'),
    ('',''));
  face : array[0..2,0..1] of string = (
    ('Face visible','Nearside'),
    ('Face cachée','Farside'),
    ('Zone des librations','Librations zone'));
  moondays : array[0..14,0..1] of string = (
    ('Non observable','not observable'),
    ('Deux jours après la Nouvelle Lune','Two days after New Moon'),
    ('Deux jours après la Nouvelle Lune','Two days after New Moon'),
    ('Trois jours après la Nouvelle Lune','Three days after New Moon'),
    ('Quatre jours après la Nouvelle Lune','Four days after New Moon'),
    ('Cinq jours après la Nouvelle Lune','Five days after New Moon'),
    ('Six jours après la Nouvelle Lune','Six days after New Moon'),
    ('Premier Quartier','First Quarter'),
    ('Un jour après le Premier Quartier','One day after First Quarter'),
    ('Deux jours après le Premier Quartier','Two days after First Quarter'),
    ('Trois jours après le Premier Quartier','Three days after First Quarter'),
    ('Quatre jours après le Premier Quartier','Four days after First Quarter'),
    ('Cinq jours après le Premier Quartier','Five days after First Quarter'),
    ('Six jours après le Premier Quartier','Six days after First Quarter'),
    ('Pleine Lune','Full Moon'));
  moondaym : array[0..14,0..1] of string = (
    ('Non observable','not observable'),
    ('Un jour après la Pleine Lune','One day after Full Moon'),
    ('Un jour après la Pleine Lune','One day after Full Moon'),
    ('Deux jours après la Pleine Lune','Two days after Full Moon'),
    ('Trois jours après la Pleine Lune','Three days after Full Moon'),
    ('Quatre jours après la Pleine Lune','Four days after Full Moon'),
    ('Cinq jours après la Pleine Lune','Five days after Full Moon'),
    ('Six jours après la Pleine Lune','Six days after Full Moon'),
    ('Dernier Quartier','Last Quarter'),
    ('Un jour après le Dernier Quartier','One day after Last Quarter'),
    ('Deux jours après le Dernier Quartier','Two days after Last Quarter'),
    ('Trois jours après le Dernier Quartier','Three days after Last Quarter'),
    ('Quatre jours après le Dernier Quartier','Four days after Last Quarter'),
    ('Cinq jours après le Dernier Quartier','Five days after Last Quarter'),
    ('Six jours après le Dernier Quartier','Six days after Last Quarter'));
  TxtLon : array[0..1,0..1] of string = (
    ('Est','East'),
    ('Ouest','West'));
  TxtLat : array[0..1,0..1] of string = (
    ('Nord','North'),
    ('Sud','South'));


{ TForm1 }

function TForm1.wordspace(str:string):string;
var i : integer;
    c : char;
begin
c:=blank;
result:='';
for i:=1 to length(str) do begin
  if str[i]=blank then begin
     if c<>blank then result:=result+str[i];
  end else result:=result+str[i];
  c:=str[i];
end;
end;

function TForm1.SafeData(str:string):string;
begin
  str:=StringReplace(str,';','.',[rfReplaceAll]);
  result:=str;
end;

function TForm1.CleanName(str:string):string;
var i:integer;
begin
for i:=0 to avlname.Count-1 do begin
   if str=avlname[i] then begin
      str:=clname[i];
      break;
   end;
end;
str:=StringReplace(str,'-',' ',[rfReplaceAll]);
str:=StringReplace(str,'.',' ',[rfReplaceAll]);
str:=StringReplace(str,'''',' ',[rfReplaceAll]);
result:=str;
end;

procedure TForm1.LoadCleanName;
var f: textfile;
    buf,n,cn,rn:string;
    i:integer;
begin
AssignFile(f,'addiau-clean-name.txt');
reset(f);
repeat
  readln(f,buf);
  i:=pos(';',buf);
  if i>0 then begin
     n:=trim(copy(buf,1,i-1));
     buf:=trim(copy(buf,i+1,999));
     i:=pos(';',buf);
     if i>0 then begin
       cn:=trim(copy(buf,1,i-1));
       rn:=trim(copy(buf,i+1,999));
     end
     else begin
       cn:=buf;
       rn:='';
     end;
     avlname.Add(n);
     clname.Add(cn);
     rname.Add(rn);
  end;
until eof(f);
closefile(f);
end;

Procedure  TForm1.AddTmpName(db:TMlb2);
begin
db.AddField('TMP_NAME');
db.GoFirst;
repeat
  db.SetData('TMP_NAME',uppercase(wordspace(trim(CleanName(db.GetData('NAME'))))));
  db.GoNext;
until db.EndOfFile;
end;

procedure TForm1.LoadDelName;
var f: textfile;
    buf,n,db:string;
    i:integer;
begin
AssignFile(f,'addiau-suppression.txt');
reset(f);
repeat
  readln(f,buf);
  delname.Add(buf);
until eof(f);
closefile(f);
end;

Procedure  TForm1.DelNonIAU(db:TMlb2);
var buf: string;
    i: integer;
    deleted: boolean;
begin
db.GoFirst;
repeat
  buf:=uppercase(wordspace(trim(CleanName(db.GetData('NAME')))));
  deleted:=false;
  for i:=0 to delname.Count-1 do begin
     if buf=delname[i] then begin
         db.RemoveRow;
         deleted:=true;
         break;
     end;
  end;
  if not deleted then db.GoNext;
until db.EndOfFile;
end;

Procedure  TForm1.DelTmpName(db:TMlb2);
begin
db.RemoveField('TMP_NAME');
end;

procedure TForm1.LoadNewName;
var f: textfile;
    buf,n,db:string;
    i:integer;
begin
AssignFile(f,'addiau-new_features.txt');
reset(f);
repeat
  readln(f,buf);
  i:=pos(';',buf);
  if i>0 then begin
     n:=trim(copy(buf,1,i-1));
     db:=trim(copy(buf,i+1,999));
     newname.Add(n);
     newdb.Add(db);
  end;
until eof(f);
closefile(f);
end;

procedure TForm1.OpenDB;
begin
   lang:=trim(dblang.Text);
   dbiau:=Tmlb2.Create;
   dbiau.init;
   dbiau.CSVSeparator:=',';
   dbiau.QuoteSeparator:='"';
   dbiau.LoadFromISAMFile('MOON_nomenclature.csv');   // ! DOS cr+lf  !
   dbnn:=Tmlb2.Create;
   dbnn.init;
   vdbnn:=dbnn.LoadFromFile('../Database/Nearside_Named_u'+lang+'.csv');
   dbns:=Tmlb2.Create;
   dbns.init;
   vdbns:=dbns.LoadFromFile('../Database/Nearside_Satellite_u'+lang+'.csv');
   dbfn:=Tmlb2.Create;
   dbfn.init;
   vdbfn:=dbfn.LoadFromFile('../Database/Farside_Named_u'+lang+'.csv');
   dbfs:=Tmlb2.Create;
   dbfs.init;
   vdbfs:=dbfs.LoadFromFile('../Database/Farside_Satellite_u'+lang+'.csv');
   if lang<>'FR' then begin
     dbnnfr:=Tmlb2.Create;
     dbnnfr.init;
     dbnnfr.LoadFromFile('../Database/Nearside_Named_uFR.csv');
     dbnsfr:=Tmlb2.Create;
     dbnsfr.init;
     dbnsfr.LoadFromFile('../Database/Nearside_Satellite_uFR.csv');
     dbfnfr:=Tmlb2.Create;
     dbfnfr.init;
     dbfnfr.LoadFromFile('../Database/Farside_Named_uFR.csv');
     dbfsfr:=Tmlb2.Create;
     dbfsfr.init;
     dbfsfr.LoadFromFile('../Database/Farside_Satellite_uFR.csv');
   end;
end;

procedure TForm1.AddIAUFields(db: TMlb2);
begin
if db.GetFieldIndex('IAU_FEATURE_NAME')=0 then begin
   db.AddField('IAU_FEATURE_NAME');
   db.AddField('IAU_CLEAN_FEATURE_NAME');
   db.AddField('IAU_FEATURE_ID');
   db.AddField('IAU_DIAMETER');
   db.AddField('IAU_CENTER_LATITUDE');
   db.AddField('IAU_CENTER_LONGITUDE');
   db.AddField('IAU_NORTHERN_LATITUDE');
   db.AddField('IAU_SOUTHERN_LATITUDE');
   db.AddField('IAU_EASTERN_LONGITUDE');
   db.AddField('IAU_WESTERN_LONGITUDE');
   db.AddField('IAU_COORDINATE_SYSTEM');
   db.AddField('IAU_CONTINENT');
   db.AddField('IAU_ETHNICITY');
   db.AddField('IAU_FEATURE_TYPE');
   db.AddField('IAU_FEATURE_TYPE_CODE');
   db.AddField('IAU_QUAD_NAME');
   db.AddField('IAU_QUAD_CODE');
   db.AddField('IAU_APPROVAL_STATUS');
   db.AddField('IAU_APPROVAL_DATE');
   db.AddField('IAU_REFERENCE');
   db.AddField('IAU_ORIGIN');
   db.AddField('IAU_LINK');
end;
end;

function pctchk(val,ref,pct:double):boolean;
var pc:double;
begin
if (abs(val-ref)<1) then result:=true
else begin
  pc:=pct*val;
  result:=((ref>(val-pc))and(ref<(val+pc)));
end;
end;

procedure TForm1.AddIAUData(db: TMlb2);
var i,facenum:integer;
    buf,id,la,lo,lun,lunr,iaucode,fac:string;
    x,y:double;
begin
buf:=trim(SafeData(dbiau.GetData('LINK')));
i:=LastDelimiter('/',buf);
id:=trim(copy(buf,i+1,99));
db.SetData('IAU_FEATURE_NAME',trim(SafeData(dbiau.GetData('FEATURE'))));
db.SetData('IAU_CLEAN_FEATURE_NAME',trim(SafeData(dbiau.GetData('CLEAN_FEAT'))));
db.SetData('IAU_FEATURE_ID',id);
db.SetData('IAU_DIAMETER',trim(SafeData(dbiau.GetData('DIAMETER'))));
db.SetData('IAU_CENTER_LATITUDE',trim(SafeData(dbiau.GetData('CENTER_LAT'))));
db.SetData('IAU_CENTER_LONGITUDE',trim(SafeData(dbiau.GetData('CENTER_LON'))));
db.SetData('IAU_NORTHERN_LATITUDE',trim(SafeData(dbiau.GetData('MAX_LAT'))));
db.SetData('IAU_SOUTHERN_LATITUDE',trim(SafeData(dbiau.GetData('MIN_LAT'))));
db.SetData('IAU_EASTERN_LONGITUDE',trim(SafeData(dbiau.GetData('MAX_LON'))));
db.SetData('IAU_WESTERN_LONGITUDE',trim(SafeData(dbiau.GetData('MIN_LON'))));
db.SetData('IAU_COORDINATE_SYSTEM','LOLA2011');
db.SetData('IAU_CONTINENT',trim(SafeData(dbiau.GetData('CONTINENT'))));
db.SetData('IAU_ETHNICITY',trim(SafeData(dbiau.GetData('ETHNICITY'))));
db.SetData('IAU_FEATURE_TYPE',trim(SafeData(dbiau.GetData('TYPE'))));
db.SetData('IAU_FEATURE_TYPE_CODE',trim(SafeData(dbiau.GetData('CODE'))));
db.SetData('IAU_QUAD_NAME',trim(SafeData(dbiau.GetData('QUAD_NAME'))));
db.SetData('IAU_QUAD_CODE',trim(SafeData(dbiau.GetData('QUAD_CODE'))));
db.SetData('IAU_APPROVAL_STATUS',trim(SafeData(dbiau.GetData('APPROVAL'))));
db.SetData('IAU_APPROVAL_DATE',trim(SafeData(dbiau.GetData('APPROVALDT'))));
db.SetData('IAU_REFERENCE','');
db.SetData('IAU_ORIGIN',trim(SafeData(dbiau.GetData('ORIGIN'))));
db.SetData('IAU_LINK',trim(SafeData(dbiau.GetData('LINK'))));
// correct name
buf:='';
if currentname>'' then begin
  for i:=0 to avlname.Count-1 do begin
     if currentname=avlname[i] then begin
        buf:=rname[i];
        break;
     end;
  end;
end;
if buf>'' then db.SetData('NAME',buf);
// replace coordinates
x:=strtofloat(dbiau.GetData('CENTER_LON'));
db.SetData('LONGIN360',formatfloat(f6,x));
if x>180 then x:=x-360;
db.SetData('LONGIN',formatfloat(f6,x));
buf:=formatfloat(f3,abs(x))+'° ';
if x>0 then buf:=buf+TxtLon[0,ilang] else buf:=buf+TxtLon[1,ilang];
db.SetData('LONGIC',buf);
y:=strtofloat(dbiau.GetData('CENTER_LAT'));
db.SetData('LATIN',formatfloat(f6,y));
buf:=formatfloat(f3,abs(y))+'° ';
if y>0 then buf:=buf+TxtLat[0,ilang] else buf:=buf+TxtLat[1,ilang];
db.SetData('LATIC',buf);
 // New LUN from coordinates
 la:=formatfloat('0000',abs(y*100));
 if y>=0 then la:=la+'N'
         else la:=la+'S';
 lo:=formatfloat('00000',abs(x*100));
 lunr:=la+lo;
 if x>=0 then lo:=lo+'E'
         else lo:=lo+'W';
 iaucode:=trim(SafeData(dbiau.GetData('CODE')));
 lun:=iaucode+la+lo;
 db.SetData('LUN',lun);
 db.SetData('LUN_REDUCED',lunr);
 // name type
 db.SetData('NAMETYPE',trim(SafeData(dbiau.GetData('CODE'))));
 // face
 if (x>=0)and(x<=80) then facenum:=0
 else if (x<0)and(x>=-80) then facenum:=0
 else if (x>80)and(x<=100) then facenum:=2
 else if (x<-80)and(x>=-100) then facenum:=2
 else facenum:=1;
 fac:=face[facenum,ilang];
 db.SetData('FACE',fac);
end;

procedure TForm1.FixSize(var db,dbfr: TMlb2; dbn:string);
var buf,nam,oldval: string;
    diam,lkm,lmi,wkm,wmi: double;
begin
// Replace , by . in size
buf:=trim(db.GetData('LENGTHKM'));
buf:=stringreplace(buf,',','.',[]);
db.SetData('LENGTHKM',buf);
buf:=trim(db.GetData('LENGTHMI'));
buf:=stringreplace(buf,',','.',[]);
db.SetData('LENGTHMI',buf);
buf:=trim(db.GetData('WIDEKM'));
buf:=stringreplace(buf,',','.',[]);
db.SetData('WIDEKM',buf);
buf:=trim(db.GetData('WIDEMI'));
buf:=stringreplace(buf,',','.',[]);
db.SetData('WIDEMI',buf);
// Check size
diam:=strtofloat(trim(dbiau.GetData('DIAMETER')));
buf:=trim(db.GetData('LENGTHKM'));
lkm:=strtofloatdef(buf,0);
oldval:='oldlkm='+buf;
buf:=trim(db.GetData('LENGTHMI'));
lmi:=strtofloatdef(buf,0);
oldval:=oldval+tab+'oldlmi='+buf;
buf:=trim(db.GetData('WIDEKM'));
wkm:=strtofloatdef(buf,0);
oldval:=oldval+tab+'oldwkm='+buf;
buf:=trim(db.GetData('WIDEMI'));
wmi:=strtofloatdef(buf,0);
oldval:=oldval+tab+'oldwmi='+buf;
if (abs(lkm-diam)>1)and(not pctchk(lkm,diam,0.5)) then
    writeln(fdiam,dbn+' '+db.GetData('NAME')+tab+'lkm='+db.GetData('LENGTHKM')+tab+'iau-diam='+dbiau.GetData('DIAMETER'));
if (not pctchk(lkm,lmi*1.67,0.1))or(not pctchk(wkm,wmi*1.67,0.1)) then
    writeln(fkmmi,dbn+' '+db.GetData('NAME')+tab+oldval);
// copy FR size value
if lang<>'FR' then begin
   nam:=db.GetData('NAME');
   dbfr.GoFirst;
   if dbfr.SeekData('NAME','=',nam) then begin
     buf:=trim(dbfr.GetData('LENGTHKM'));
     buf:=stringreplace(buf,',','.',[]);
     db.SetData('LENGTHKM',buf);
     buf:=trim(dbfr.GetData('LENGTHMI'));
     buf:=stringreplace(buf,',','.',[]);
     db.SetData('LENGTHMI',buf);
     buf:=trim(dbfr.GetData('WIDEKM'));
     buf:=stringreplace(buf,',','.',[]);
     db.SetData('WIDEKM',buf);
     buf:=trim(dbfr.GetData('WIDEMI'));
     buf:=stringreplace(buf,',','.',[]);
     db.SetData('WIDEMI',buf);
     writeln(frepsize,dbn+' '+db.GetData('NAME')+tab+'lkm='+db.GetData('LENGTHKM')+tab+'lmi='+db.GetData('LENGTHMI')+tab+'wkm='+db.GetData('WIDEKM')+tab+'wmi='+db.GetData('WIDEMI')+tab+oldval);
   end;
end;
end;

procedure TForm1.AddNewFields(var db: TMlb2);
var ndb: TMlb2;
    i: integer;
const numfield=58;
      dbf: array[1..numfield] of string = (
     'NAME','LUN','LUN_REDUCED','NAMETYPE','TYPE','SUBTYPE','PERIOD','GEOLOGY',
     'NAMEDETAIL','NAMEORIGIN','LANGRENUS','HEVELIUS','RICCIOLI','WORK','COUNTRY',
     'NATIONLITY','CENTURYN','CENTURYC','BIRTHPLACE','BIRTHDATE','DEATHPLACE',
     'DEATHDATE','FACTS','LONGIN','LONGIN360','LONGIC','LATIN','LATIC','FACE',
     'QUADRANT','AREA','RUKL','RUKLC','VISCARDY','HATFIELD','WESTFALL','WOOD',
     'LOPAM','LENGTHKM','WIDEKM','LENGTHMI','WIDEMI','HEIGHTM','HEIGHTFE',
     'RAPPORT','PROFIL','GENERAL','SLOPES','WALLS','FLOOR','INTERESTN','INTERESTC',
     'LUNATION','MOONDAYS','MOONDAYM','DIAMINST','THINSTRU','PRINSTRU');
begin
addlun:=false;
addlunred:=false;
addnametype:=false;
addsubtype:=false;
addprocess:=false;
addgeology:=false;
addlong360:=false;
addface:=false;
addtips:=false;
if db.GetFieldIndex('LUN')=0 then addlun:=true;
if db.GetFieldIndex('LUN_REDUCED')=0 then addlunred:=true;
if db.GetFieldIndex('NAMETYPE')=0 then addnametype:=true;
if db.GetFieldIndex('SUBTYPE')=0 then addsubtype:=true;
if db.GetFieldIndex('PROCESS')=0 then addprocess:=true;
if db.GetFieldIndex('GEOLOGY')=0 then addgeology:=true;
if db.GetFieldIndex('LONGIN360')=0 then addlong360:=true;
if db.GetFieldIndex('FACE')=0 then addface:=true;
if db.GetFieldIndex('TIPS')=0 then addtips:=true;
if addlun or addlunred or addnametype or addsubtype or addprocess or addgeology or addlong360 or addface or addtips then begin
  ndb:=Tmlb2.Create;
  ndb.init;
  for i:=1 to numfield do begin
    ndb.AddField(dbf[i]);
  end;
  db.GoFirst;
  repeat
     ndb.AddRow;
     for i:=1 to numfield do begin
        ndb.SetData(dbf[i],db.GetData(dbf[i]));
     end;
     db.GoNext;
  until db.EndOfFile;
  db.Clear;
  db.Free;
  db:=ndb;
end;
end;

function TForm1.AddNewName(n: string):boolean;
var i,quadrantnum,lunation:integer;
    dbn,buf,quad,lunat,mdays,mdaym:string;
    db:TMlb2;
    x,y,d:double;
begin
dbn:='';
result:=false;
for i:=0 to newname.Count-1 do begin
   if n=newname[i] then begin
      dbn:=newdb[i];
      break;
   end;
end;
if dbn>'' then begin
   if dbn='nn' then db:=dbnn
   else if dbn='ns' then db:=dbns
   else if dbn='fn' then db:=dbfn
   else if dbn='fs' then db:=dbfs
   else exit;
   currentname:='';
   db.AddRow;
   AddIAUData(db);
   db.SetData('NAME',uppercase(wordspace(trim(SafeData(dbiau.GetData('CLEAN_FEAT'))))));
   db.SetData('TYPE',trim(SafeData(dbiau.GetData('TYPE'))));
   db.SetData('NAMEDETAIL',trim(SafeData(dbiau.GetData('ORIGIN'))));
   db.SetData('NAMEORIGIN','IAU');
   db.SetData('NATIONLITY',trim(SafeData(dbiau.GetData('ETHNICITY'))));
   db.SetData('COUNTRY',trim(SafeData(dbiau.GetData('CONTINENT'))));
   x:=strtofloat(dbiau.GetData('CENTER_LON'));
   if x>180 then x:=x-360;
   db.SetData('LONGIN',formatfloat(f6,x));
   buf:=formatfloat(f3,abs(x))+'° ';
   if x>0 then buf:=buf+TxtLon[0,ilang] else buf:=buf+TxtLon[1,ilang];
   db.SetData('LONGIC',buf);
   y:=strtofloat(dbiau.GetData('CENTER_LAT'));
   db.SetData('LATIN',formatfloat(f6,y));
   buf:=formatfloat(f3,abs(y))+'° ';
   if y>0 then buf:=buf+TxtLat[0,ilang] else buf:=buf+TxtLat[0,ilang];
   db.SetData('LATIC',buf);
   db.SetData('AREA',trim(SafeData(dbiau.GetData('QUAD_NAME'))));
   buf:=trim(SafeData(dbiau.GetData('DIAMETER')));
   db.SetData('LENGTHKM',buf);
   db.SetData('WIDEKM',buf);
   d:=strtofloatdef(buf,0);
   buf:=formatfloat('0.00',d/1.67);
   db.SetData('LENGTHMI',buf);
   db.SetData('WIDEMI',buf);
    // quadrant
    if y>=0 then begin
       if (x>=0)and(x<=90) then quadrantnum:=0
       else if (x<0)and(x>-90) then quadrantnum:=1
       else quadrantnum:=4;
    end else begin
       if (x>=0)and(x<=90) then quadrantnum:=2
       else if (x<0)and(x>-90) then quadrantnum:=3
       else quadrantnum:=4;
    end;
    quad:=quadrant[quadrantnum,ilang];
    db.SetData('QUADRANT',quad);
    // lunaison
    case floor(x) of
     -90..-78 : lunation:=14;
     -77..-65 : lunation:=13;
     -64..-52 : lunation:=12;
     -51..-39 : lunation:=11;
     -38..-26 : lunation:=10;
     -25..-13 : lunation:=9;
     -12..-1  : lunation:=8;
      0..12   : lunation:=7;
      13..25  : lunation:=6;
      26..38  : lunation:=5;
      39..51  : lunation:=4;
      52..64  : lunation:=3;
      65..77  : lunation:=2;
      78..90  : lunation:=1;
      else lunation:=0;
    end;
    lunat:=IntToStr(lunation);
    db.SetData('LUNATION',lunat);
    mdays:=moondays[lunation,ilang];
    db.SetData('MOONDAYS',mdays);
    mdaym:=moondaym[lunation,ilang];
    db.SetData('MOONDAYM',mdaym);
   result:=true;
end;
end;

procedure TForm1.CloseDB;
begin
   dbiau.Clear;
   dbiau.Free;
   dbnn.Clear;
   dbnn.Free;
   dbns.Clear;
   dbns.Free;
   dbfn.Clear;
   dbfn.Free;
   dbfs.Clear;
   dbfs.Free;
   if lang<>'FR' then begin
     dbnnfr.Clear;
     dbnnfr.Free;
     dbnsfr.Clear;
     dbnsfr.Free;
     dbfnfr.Clear;
     dbfnfr.Free;
     dbfsfr.Clear;
     dbfsfr.Free;
   end;
end;

function TForm1.NameMatch(n: string; db:TMlb2):boolean;
begin
db.GoFirst;
if db.GetData('TMP_NAME')=n then result:=true
else result:=db.SeekData('TMP_NAME','=',n);
if result then currentname:=db.GetData('NAME')
          else currentname:='';
end;

procedure TForm1.Button1Click(Sender: TObject);
var cname,mf: string;
    match: integer;
begin
 matchnn:=0;
 matchns:=0;
 matchfn:=0;
 matchfs:=0;
 matchnew:=0;
 nomatch:=0;
 multimatch:=0;
 niau:=0;
 if dblang.Text='FR' then ilang:=0 else ilang:=1;
 LoadCleanName;
 LoadNewName;
 LoadDelName;
 OpenDB;
 AddNewFields(dbnn);
 AddNewFields(dbns);
 AddNewFields(dbfn);
 AddNewFields(dbfs);
 AddIAUFields(dbnn);
 AddIAUFields(dbns);
 AddIAUFields(dbfn);
 AddIAUFields(dbfs);
 DelNonIAU(dbnn);
 DelNonIAU(dbns);
 DelNonIAU(dbfn);
 DelNonIAU(dbfs);
 AddTmpName(dbnn);
 AddTmpName(dbns);
 AddTmpName(dbfn);
 AddTmpName(dbfs);
 AssignFile(f0,'no_match_'+dblang.Text+'.txt');
 rewrite(f0);
 AssignFile(f2,'no_match_name_'+dblang.Text+'.txt');
 rewrite(f2);
 AssignFile(f1,'multi_match_'+dblang.Text+'.txt');
 rewrite(f1);
 AssignFile(fdiam,'error_diam_'+dblang.Text+'.txt');
 rewrite(fdiam);
 AssignFile(fkmmi,'error_kmmi_'+dblang.Text+'.txt');
 rewrite(fkmmi);
 AssignFile(frepsize,'replace_size_'+dblang.Text+'.txt');
 rewrite(frepsize);
 dbiau.GoFirst;
 label7.Caption:='0';
 label7.Refresh;
 Application.ProcessMessages;
 repeat
   match:=0;
   mf:='';
   inc(niau);
   if (niau mod 100) = 0 then begin label7.Caption:=inttostr(niau); label7.Refresh; Application.ProcessMessages; end;
   cname:=dbiau.GetData('CLEAN_FEAT');
   cname:=uppercase(wordspace(trim(cname)));
   if NameMatch(cname,dbnn) then begin
      AddIAUData(dbnn);
      FixSize(dbnn,dbnnfr,'nn');
      inc(matchnn);
      inc(match);
      mf:=mf+' nn';
   end;
   if NameMatch(cname,dbns) then begin
      AddIAUData(dbns);
      FixSize(dbns,dbnsfr,'ns');
      inc(matchns);
      inc(match);
      mf:=mf+' ns';
   end;
   if NameMatch(cname,dbfn) then begin
      AddIAUData(dbfn);
      FixSize(dbfn,dbfnfr,'fn');
      inc(matchfn);
      inc(match);
      mf:=mf+' fn';
   end;
   if NameMatch(cname,dbfs) then begin
      AddIAUData(dbfs);
      FixSize(dbfs,dbfsfr,'fs');
      inc(matchfs);
      inc(match);
      mf:=mf+' fs';
   end;
   if match=0 then begin
      if AddNewName(cname) then begin
         inc(matchnew);
         inc(match);
      end;
   end;
   if match=0 then begin
      writeln(f0,cname+' ; '+dbiau.GetData('TYPE')+' ; '+dbiau.GetData('DIAMETER')+' ; '+dbiau.GetData('CENTER_LON')+' ; '+dbiau.GetData('CENTER_LAT'));
      writeln(f2,cname);
      inc(nomatch);
   end;
   if match>1 then begin
      writeln(f1,cname+' : '+mf);
      inc(multimatch);
   end;
   dbiau.GoNext;
 until dbiau.EndOfFile;
 DelTmpName(dbnn);
 DelTmpName(dbns);
 DelTmpName(dbfn);
 DelTmpName(dbfs);
 if vdbnn then dbnn.SaveToFile('Nearside_Named_u'+lang+'.csv');
 if vdbns then dbns.SaveToFile('Nearside_Satellite_u'+lang+'.csv');
 if vdbfn then dbfn.SaveToFile('Farside_Named_u'+lang+'.csv');
 if vdbfs then dbfs.SaveToFile('Farside_Satellite_u'+lang+'.csv');
 CloseFile(f0);
 CloseFile(f1);
 CloseFile(f2);
 CloseFile(fdiam);
 CloseFile(fkmmi);
 CloseFile(frepsize);
 CloseDB;
 Label0.Caption:='IAU Names '+inttostr(niau);
 Label1.Caption:='Nearside_Named '+inttostr(matchnn);
 Label2.Caption:='Nearside_Satellite '+inttostr(matchns);
 Label3.Caption:='Farside_Named '+inttostr(matchfn);
 Label4.Caption:='Farside_Satellite '+inttostr(matchfs);
 Label5.Caption:='No match '+inttostr(nomatch);
 Label6.Caption:='Multi match '+inttostr(multimatch);
 AssignFile(fstat,'stats_'+dblang.Text+'.txt');
 rewrite(fstat);
 writeln(fstat,Label0.Caption);
 writeln(fstat,Label1.Caption);
 writeln(fstat,Label2.Caption);
 writeln(fstat,Label3.Caption);
 writeln(fstat,Label4.Caption);
 writeln(fstat,Label5.Caption);
 writeln(fstat,Label6.Caption);
 CloseFile(fstat);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  avlname:=TStringList.Create;
  clname:=TStringList.Create;
  rname:=TStringList.Create;
  newname:=TStringList.Create;
  newdb:=TStringList.Create;
  delname:=TStringList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  avlname.Free;
  clname.Free;
  rname.Free;
  newname.Free;
  newdb.Free;
  delname.Free;
end;

end.

