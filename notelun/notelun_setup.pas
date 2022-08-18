unit notelun_setup;

{$mode ObjFPC}{$H+}

interface

uses passql, passqlite, u_constant, u_util,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Grids, Buttons;

type

  { TFSetup }

  TFSetup = class(TForm)
    BtnAddRow: TButton;
    BtnCancel: TButton;
    BtnSave: TButton;
    PageControl1: TPageControl;
    Panel1: TPanel;
    StringGridCamera: TStringGrid;
    StringGridLocation: TStringGrid;
    StringGridObserver: TStringGrid;
    StringGridInstrument: TStringGrid;
    StringGridBarlow: TStringGrid;
    StringGridEyepiece: TStringGrid;
    TabSheetCamera: TTabSheet;
    TabSheetEyepiece: TTabSheet;
    TabSheetBarlow: TTabSheet;
    TabSheetInstrument: TTabSheet;
    TabSheetObserver: TTabSheet;
    TabSheetLocation: TTabSheet;
    procedure BtnAddRowClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGridValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
  private
    LocationModified,ObserverModified,InstrumentModified,BarlowModified,EyepieceModified,CameraModified: boolean;
    Procedure SetLang;
    procedure ClearGrid(grid:TStringGrid);
    Procedure ClearData;
    Procedure LoadData;
    function SaveGrid(grid: TStringGrid; table,cmdprefix: string ):boolean;
    function SaveData:boolean;
  public

  end;

var
  FSetup: TFSetup;

implementation

{$R *.lfm}

{ TFSetup }

procedure TFSetup.FormCreate(Sender: TObject);
begin
  SetLang;
end;

procedure TFSetup.FormDestroy(Sender: TObject);
begin
  ClearData;
end;

procedure TFSetup.SetLang;
begin
  TabSheetLocation.Caption:='Location';
  StringGridLocation.Cells[0,0]:='Name';
  StringGridLocation.Cells[1,0]:='Longitude';
  StringGridLocation.Cells[2,0]:='Latitude';
  StringGridLocation.Cells[3,0]:='Elevation [meter]';
  StringGridLocation.Cells[4,0]:='Timezone';
  TabSheetObserver.Caption:='Observer';
  StringGridObserver.Cells[0,0]:='Name';
  StringGridObserver.Cells[1,0]:='First name';
  StringGridObserver.Cells[2,0]:='Pseudo';
  StringGridObserver.Cells[3,0]:='Contact';
  TabSheetInstrument.Caption:='Instrument';
  StringGridInstrument.Cells[0,0]:='Name';
  StringGridInstrument.Cells[1,0]:='Type';
  StringGridInstrument.Cells[2,0]:='Diameter [mm]';
  StringGridInstrument.Cells[3,0]:='Focal [mm]';
  StringGridInstrument.Cells[4,0]:='F/D';
  TabSheetBarlow.Caption:='Barlow/Reducer';
  StringGridBarlow.Cells[0,0]:='Name';
  StringGridBarlow.Cells[1,0]:='Power';
  TabSheetEyepiece.Caption:='Eyepiece';
  StringGridEyepiece.Cells[0,0]:='Name';
  StringGridEyepiece.Cells[1,0]:='Focal [mm]';
  StringGridEyepiece.Cells[2,0]:='Field [°]';
  TabSheetCamera.Caption:='Camera';
  StringGridCamera.Cells[0,0]:='Name';
  StringGridCamera.Cells[1,0]:='Horizontal resolution [pixel]';
  StringGridCamera.Cells[2,0]:='Vertical resolution [pixel]';
  StringGridCamera.Cells[3,0]:='Pixel size [µ]';
end;

procedure TFSetup.ClearData;
begin
  ClearGrid(StringGridLocation);
  ClearGrid(StringGridObserver);
  ClearGrid(StringGridInstrument);
  ClearGrid(StringGridBarlow);
  ClearGrid(StringGridEyepiece);
  ClearGrid(StringGridCamera);
  LocationModified:=false;
  ObserverModified:=false;
  InstrumentModified:=false;
  BarlowModified:=false;
  EyepieceModified:=false;
  CameraModified:=false;
end;

procedure TFSetup.ClearGrid(grid:TStringGrid);
var i: integer;
begin
  for i:=1 to grid.RowCount-1 do begin
    if grid.Objects[0,i]<>nil then begin
      grid.Objects[0,i].Free;
      grid.Objects[0,i]:=nil;
    end;
  end;
  grid.RowCount:=1;
end;

procedure TFSetup.FormShow(Sender: TObject);
begin
  LoadData;
end;

procedure TFSetup.StringGridValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
var x,y: double;
    n: integer;
begin
  case TStringGrid(sender).tag of
    1: if OldValue<>NewValue then begin // Location
         LocationModified:=true;
         if (aCol in [1,2,3]) and (not IsNumber(NewValue)) then NewValue:=OldValue;
       end;
    2: if OldValue<>NewValue then begin // Observer
         ObserverModified:=true;
       end;
    3: if OldValue<>NewValue then begin // Instrument
         InstrumentModified:=true;
         if aCol=2 then begin // diameter change, set f/d
           val(NewValue,x,n);
           if n=0 then begin
             val(TStringGrid(sender).Cells[3,aRow],y,n);
             if (n=0)and(x>0) then
               TStringGrid(sender).Cells[4,aRow]:=FormatFloat(f1,y/x)
           end
           else
             NewValue:=OldValue;
         end;
         if aCol=3 then begin // focal change, set f/d
           val(NewValue,x,n);
           if n=0 then begin
             val(TStringGrid(sender).Cells[2,aRow],y,n);
             if (n=0)and(y>0) then
               TStringGrid(sender).Cells[4,aRow]:=FormatFloat(f1,x/y)
           end
           else
             NewValue:=OldValue;
         end;
         if aCol=4 then begin // f/d change, set focal
           val(NewValue,x,n);
           if n=0 then begin
             val(TStringGrid(sender).Cells[2,aRow],y,n);
             if n=0 then
               TStringGrid(sender).Cells[3,aRow]:=FormatFloat(f1,x*y)
           end
           else
             NewValue:=OldValue;
         end;
       end;
    4: if OldValue<>NewValue then begin// Barlow
         BarlowModified:=true;
         if (aCol in [1]) and (not IsNumber(NewValue)) then NewValue:=OldValue;
       end;
    5: if OldValue<>NewValue then begin // Eyepiece
         EyepieceModified:=true;
         if (aCol in [1,2]) and (not IsNumber(NewValue)) then NewValue:=OldValue;
       end;
    6: if OldValue<>NewValue then begin // Camera
         CameraModified:=true;
         if (aCol in [1,2,3]) and (not IsNumber(NewValue)) then NewValue:=OldValue;
       end;
  end;
end;

procedure TFSetup.BtnSaveClick(Sender: TObject);
begin
  if SaveData then
    ModalResult:=mrOK;
end;

procedure TFSetup.BtnCancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TFSetup.BtnAddRowClick(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
    0: StringGridLocation.RowCount:=StringGridLocation.RowCount+1;
    1: StringGridObserver.RowCount:=StringGridObserver.RowCount+1;
    2: StringGridInstrument.RowCount:=StringGridInstrument.RowCount+1;
    3: StringGridBarlow.RowCount:=StringGridBarlow.RowCount+1;
    4: StringGridEyepiece.RowCount:=StringGridEyepiece.RowCount+1;
    5: StringGridCamera.RowCount:=StringGridCamera.RowCount+1;
  end;
end;

Procedure TFSetup.LoadData;
var cmd: string;
    i: integer;
    id: TNoteID;
begin
  ClearData;
  cmd:='select id,name,longitude,latitude,elevation,timezone from location order by id asc';
  dbnotes.Query(cmd);
  StringGridLocation.RowCount:=dbnotes.RowCount+2;
  for i:=0 to dbnotes.RowCount-1 do begin
    id:=TNoteID.Create;
    id.id:=dbnotes.Results[i].Format[0].AsInteger;
    StringGridLocation.Objects[0,i+1]:=id;
    StringGridLocation.Cells[0,i+1]:=dbnotes.Results[i][1];
    StringGridLocation.Cells[1,i+1]:=dbnotes.Results[i][2];
    StringGridLocation.Cells[2,i+1]:=dbnotes.Results[i][3];
    StringGridLocation.Cells[3,i+1]:=dbnotes.Results[i][4];
    StringGridLocation.Cells[4,i+1]:=dbnotes.Results[i][5];
  end;
  cmd:='select id,name,firstname,pseudo,contact from observer order by id asc';
  dbnotes.Query(cmd);
  StringGridObserver.RowCount:=dbnotes.RowCount+2;
  for i:=0 to dbnotes.RowCount-1 do begin
    id:=TNoteID.Create;
    id.id:=dbnotes.Results[i].Format[0].AsInteger;
    StringGridObserver.Objects[0,i+1]:=id;
    StringGridObserver.Cells[0,i+1]:=dbnotes.Results[i][1];
    StringGridObserver.Cells[1,i+1]:=dbnotes.Results[i][2];
    StringGridObserver.Cells[2,i+1]:=dbnotes.Results[i][3];
    StringGridObserver.Cells[3,i+1]:=dbnotes.Results[i][4];
  end;
  cmd:='select id,name,type,diameter,focal,fd from instrument order by id asc';
  dbnotes.Query(cmd);
  StringGridInstrument.RowCount:=dbnotes.RowCount+2;
  for i:=0 to dbnotes.RowCount-1 do begin
    id:=TNoteID.Create;
    id.id:=dbnotes.Results[i].Format[0].AsInteger;
    StringGridInstrument.Objects[0,i+1]:=id;
    StringGridInstrument.Cells[0,i+1]:=dbnotes.Results[i][1];
    StringGridInstrument.Cells[1,i+1]:=dbnotes.Results[i][2];
    StringGridInstrument.Cells[2,i+1]:=dbnotes.Results[i][3];
    StringGridInstrument.Cells[3,i+1]:=dbnotes.Results[i][4];
    StringGridInstrument.Cells[4,i+1]:=dbnotes.Results[i][5];
    try
    StringGridInstrument.Cells[4,i+1]:=FormatFloat(f2,StrToFloat(dbnotes.Results[i][4])/StrToFloat(dbnotes.Results[i][3]));
    except
      StringGridInstrument.Cells[4,i+1]:='';
    end;
  end;
  cmd:='select id,name,power from barlow order by id asc';
  dbnotes.Query(cmd);
  StringGridBarlow.RowCount:=dbnotes.RowCount+2;
  for i:=0 to dbnotes.RowCount-1 do begin
    id:=TNoteID.Create;
    id.id:=dbnotes.Results[i].Format[0].AsInteger;
    StringGridBarlow.Objects[0,i+1]:=id;
    StringGridBarlow.Cells[0,i+1]:=dbnotes.Results[i][1];
    StringGridBarlow.Cells[1,i+1]:=dbnotes.Results[i][2];
  end;
  cmd:='select id,name,focal,field from eyepiece order by id asc';
  dbnotes.Query(cmd);
  StringGridEyepiece.RowCount:=dbnotes.RowCount+2;
  for i:=0 to dbnotes.RowCount-1 do begin
    id:=TNoteID.Create;
    id.id:=dbnotes.Results[i].Format[0].AsInteger;
    StringGridEyepiece.Objects[0,i+1]:=id;
    StringGridEyepiece.Cells[0,i+1]:=dbnotes.Results[i][1];
    StringGridEyepiece.Cells[1,i+1]:=dbnotes.Results[i][2];
    StringGridEyepiece.Cells[2,i+1]:=dbnotes.Results[i][3];
  end;
  cmd:='select id,name,pixelx,pixely,pixelsize from camera order by id asc';
  dbnotes.Query(cmd);
  StringGridCamera.RowCount:=dbnotes.RowCount+2;
  for i:=0 to dbnotes.RowCount-1 do begin
    id:=TNoteID.Create;
    id.id:=dbnotes.Results[i].Format[0].AsInteger;
    StringGridCamera.Objects[0,i+1]:=id;
    StringGridCamera.Cells[0,i+1]:=dbnotes.Results[i][1];
    StringGridCamera.Cells[1,i+1]:=dbnotes.Results[i][2];
    StringGridCamera.Cells[2,i+1]:=dbnotes.Results[i][3];
    StringGridCamera.Cells[3,i+1]:=dbnotes.Results[i][4];
  end;
end;

function TFSetup.SaveGrid(grid: TStringGrid; table,cmdprefix: string ):boolean;
var cmd: string;
    i,j: integer;
    newid: TNoteID;
    newinsert: boolean;
begin
  for i:=1 to grid.RowCount-1 do begin
    if (grid.Cells[0,i]='')or(grid.Cells[1,i]='') then continue;
    cmd:=cmdprefix;
    if grid.Objects[0,i]=nil then begin
      newinsert:=true;
      cmd:=cmd+'null,"'
    end
    else begin
      newinsert:=false;
      cmd:=cmd+IntToStr(TNoteID(grid.Objects[0,i]).id)+',"';
    end;
    for j:=0 to grid.ColCount-1 do begin
      cmd:=cmd+SafeSqlText(grid.Cells[j,i])+'","';
    end;
    delete(cmd,length(cmd)-1,2);
    cmd:=cmd+')';
    dbnotes.Query(cmd);
    if dbnotes.LastError<>0 then begin
      ShowMessage(table+' error row '+inttostr(i)+', '+dbnotes.ErrorMessage);
      result:=false;
      exit;
    end;
    if newinsert then begin
      newid:=TNoteID.Create;
      newid.id:=dbnotes.LastInsertID;
      grid.Objects[0,i]:=newid;
    end;
  end;
end;

function TFSetup.SaveData:boolean;
begin
  result:=false;
  if LocationModified and (StringGridLocation.RowCount>1) then begin
    if not SaveGrid(StringGridLocation,'Location','replace into location (id,name,longitude,latitude,elevation,timezone) values (') then exit;
  end;
  if ObserverModified and (StringGridObserver.RowCount>1) then begin
    if not SaveGrid(StringGridObserver,'Observer','replace into observer (id,name,firstname,pseudo,contact) values (') then exit;
  end;
  if InstrumentModified and (StringGridInstrument.RowCount>1) then begin
    if not SaveGrid(StringGridInstrument,'Instrument','replace into instrument (id,name,type,diameter,focal,fd) values (') then exit;
  end;
  if BarlowModified and (StringGridBarlow.RowCount>1) then begin
    if not SaveGrid(StringGridBarlow,'Barlow','replace into barlow (id,name,power) values (') then exit;
  end;
  if EyepieceModified and (StringGridEyepiece.RowCount>1) then begin
    if not SaveGrid(StringGridEyepiece,'Eyepiece','replace into eyepiece (id,name,focal,field) values (') then exit;
  end;
  if CameraModified and (StringGridCamera.RowCount>1) then begin
    if not SaveGrid(StringGridCamera,'Camera','replace into camera (id,name,pixelx,pixely,pixelsize) values (') then exit;
  end;
  result:=true
end;

end.

