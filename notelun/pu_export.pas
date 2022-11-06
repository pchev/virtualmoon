unit pu_export;

{$mode ObjFPC}{$H+}

interface

uses  u_constant, u_util, libsql, passql, passqlite, u_translation,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { Tf_export }

  Tf_export = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckGroup1: TCheckGroup;
    Edit1: TEdit;
    SaveDialog1: TSaveDialog;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetLang;
    function FormatDate(val:string;long:boolean=false):string;
    procedure ExportInfo(fn,repcr: string);
    procedure ExportObservation(fn,repcr: string);
  public

  end;

var
  f_export: Tf_export;

implementation

{$R *.lfm}

{ Tf_export }

procedure Tf_export.FormCreate(Sender: TObject);
begin
  ScaleFormForFontSize(self,96);
  SetLang;
  CheckGroup1.Checked[0]:=true;
  CheckGroup1.Checked[1]:=true;
end;

procedure Tf_export.SetLang;
begin
  Caption:=rsExport;
  CheckGroup1.Caption:=rsNotesToExpor;
  CheckGroup1.Items[0]:=rsInformationN;
  CheckGroup1.Items[1]:=rsObservationN;
  CheckBox1.Caption:=rsReplaceLineB;
  button1.Caption:=rsCancel;
  Button2.Caption:=rsExport;
end;

procedure Tf_export.Button2Click(Sender: TObject);
var txt: string;
begin
  if CheckBox1.Checked then
    txt:=trim(edit1.text)
  else
    txt:='';
  if CheckGroup1.Checked[0] then begin
    SaveDialog1.Title:=rsExportInform;
    if SaveDialog1.Execute then
      ExportInfo(SaveDialog1.FileName,txt);
  end;
  if CheckGroup1.Checked[1] then begin
    SaveDialog1.Title:=rsExportObserv;
    if SaveDialog1.Execute then
      ExportObservation(SaveDialog1.FileName,txt);
  end;
  Close;
end;

function Tf_export.FormatDate(val:string;long:boolean=false):string;
var dt: double;
begin
  dt:=StrToFloatDef(val,0);
  if dt=0 then
    result:=''
  else begin
    if long then
      result:=FormatDateTime(datetimedisplay,dt)
    else
      result:=FormatDateTime(datedisplay,dt);
  end;
end;

procedure Tf_export.ExportObservation(fn,repcr: string);
var cmd,buf,txt: string;
    f: textfile;
    i: integer;
begin
  AssignFile(f,fn);
  Rewrite(f);
  writeln(f,'"FORMATION","DATESTART","DATEEND","LOCATION","OBSERVER","METEO","SEEING","INSTRUMENT","BARLOW","EYEPIECE","CAMERA","NOTE","FILES"');
  cmd:='select FORMATION,DATESTART,DATEEND,LOCATION,OBSERVER,METEO,SEEING,INSTRUMENT,BARLOW,EYEPIECE,CAMERA,NOTE,FILES from flatobs';
  dbnotes.Query(cmd);
  for i:=0 to dbnotes.RowCount-1 do begin
    buf:='"'+SafeSqlText(dbnotes.Results[i][0])+'","';
    buf:=buf+FormatDate(dbnotes.Results[i][1],true)+'","';
    buf:=buf+FormatDate(dbnotes.Results[i][2],true)+'","';
    buf:=buf+SafeSqlText(dbnotes.Results[i][3])+'","';
    buf:=buf+SafeSqlText(dbnotes.Results[i][4])+'","';
    buf:=buf+SafeSqlText(dbnotes.Results[i][5])+'","';
    buf:=buf+SafeSqlText(dbnotes.Results[i][6])+'","';
    buf:=buf+SafeSqlText(dbnotes.Results[i][7])+'","';
    buf:=buf+SafeSqlText(dbnotes.Results[i][8])+'","';
    buf:=buf+SafeSqlText(dbnotes.Results[i][9])+'","';
    buf:=buf+SafeSqlText(dbnotes.Results[i][10])+'","';
    txt:=dbnotes.Results[i][11];
    if repcr>'' then txt:=StringReplace(txt,#10,repcr,[rfReplaceAll]);
    buf:=buf+SafeSqlText(txt)+'","';
    txt:=dbnotes.Results[i][12];
    if repcr>'' then txt:=StringReplace(txt,#10,repcr,[rfReplaceAll]);
    buf:=buf+SafeSqlText(txt)+'"';
    writeln(f,buf);
  end;
  CloseFile(f);
end;

procedure Tf_export.ExportInfo(fn,repcr: string);
var cmd,buf,txt: string;
    f: textfile;
    i: integer;
begin
  AssignFile(f,fn);
  Rewrite(f);
  writeln(f,'"FORMATION","DATE","AUTHOR","NOTE","FILES"');
  cmd:='select FORMATION,DATE,AUTHOR,NOTE,FILES from infonotes order by id';
  dbnotes.Query(cmd);
  for i:=0 to dbnotes.RowCount-1 do begin
    buf:='"'+SafeSqlText(dbnotes.Results[i][0])+'","';
    buf:=buf+FormatDate(dbnotes.Results[i][1])+'","';
    buf:=buf+SafeSqlText(dbnotes.Results[i][2])+'","';
    txt:=dbnotes.Results[i][3];
    if repcr>'' then txt:=StringReplace(txt,#10,repcr,[rfReplaceAll]);
    buf:=buf+SafeSqlText(txt)+'","';
    txt:=dbnotes.Results[i][4];
    if repcr>'' then txt:=StringReplace(txt,#10,repcr,[rfReplaceAll]);
    buf:=buf+SafeSqlText(txt)+'"';
    writeln(f,buf);
  end;
  CloseFile(f);

end;

end.

