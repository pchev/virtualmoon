unit notelun_setup;

{$mode ObjFPC}{$H+}

interface

uses passql, passqlite, u_translation, u_constant, u_util, cu_tz,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Grids, Buttons;

type

  { TFSetup }

  TFSetup = class(TForm)
    BtnAddRow: TButton;
    BtnCancel: TButton;
    BtnSave: TButton;
    CheckBoxEphemeris: TCheckBox;
    CheckBoxReverseSort: TCheckBox;
    FontNote: TEdit;
    FontFixed: TEdit;
    FontDialog1: TFontDialog;
    GroupBoxPrint: TGroupBox;
    GroupBoxNote: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RadioGroupSortList: TRadioGroup;
    SpeedButtonFont: TSpeedButton;
    SpeedButtonFixedFont: TSpeedButton;
    StringGridCamera: TStringGrid;
    StringGridLocation: TStringGrid;
    StringGridObserver: TStringGrid;
    StringGridInstrument: TStringGrid;
    StringGridBarlow: TStringGrid;
    StringGridEyepiece: TStringGrid;
    TabSheetList: TTabSheet;
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
    procedure PageControl1Change(Sender: TObject);
    procedure SpeedButtonFixedFontClick(Sender: TObject);
    procedure SpeedButtonFontClick(Sender: TObject);
    procedure StringGridValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
  private
    LocationModified,ObserverModified,InstrumentModified,BarlowModified,EyepieceModified,CameraModified: boolean;
    tz: TCdCTimeZone;
    Procedure SetLang;
    procedure ClearGrid(grid:TStringGrid);
    Procedure ClearData;
    Procedure LoadData;
    function SaveLocationGrid(grid: TStringGrid; table,cmdprefix: string ):boolean;
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
var s: TStringList;
begin
  SetLang;
  tz:=TCdCTimeZone.Create;
  tz.LoadZoneTab(ZoneDir+'zone.tab');
  s:=TStringList.Create;
  s.Assign(tz.ZoneTabZone);
  s.Sort;
  StringGridLocation.Columns[5].PickList.Assign(s);
  s.Free;
end;

procedure TFSetup.FormDestroy(Sender: TObject);
begin
  ClearData;
  tz.Free;
end;

procedure TFSetup.SetLang;
begin
  BtnAddRow.Caption:=rsAddRow;
  BtnSave.Caption:=rsSave;
  BtnCancel.Caption:=rsCancel;

  TabSheetLocation.Caption:=rsLocation;
  StringGridLocation.Columns[0].Title.Caption:=rsName;
  StringGridLocation.Columns[1].Title.Caption:=rsEW;
  StringGridLocation.Columns[1].PickList[0]:=rsE;
  StringGridLocation.Columns[1].PickList[1]:=rsW;
  StringGridLocation.Columns[2].Title.Caption:=rsLongitude;
  StringGridLocation.Columns[3].Title.Caption:=rsLatitude;
  StringGridLocation.Columns[4].Title.Caption:=rsElevationMet;
  StringGridLocation.Columns[5].Title.Caption:=rsTimezone2;
  TabSheetObserver.Caption:=rsObserver;
  StringGridObserver.Cells[0, 0]:=rsName;
  StringGridObserver.Cells[1, 0]:=rsFirstName;
  StringGridObserver.Cells[2, 0]:=rsPseudo;
  StringGridObserver.Cells[3, 0]:=rsContact;
  TabSheetInstrument.Caption:=rsInstrument;
  StringGridInstrument.Cells[0, 0]:=rsName;
  StringGridInstrument.Cells[1, 0]:=rsType;
  StringGridInstrument.Cells[2, 0]:=rsDiameterMm;
  StringGridInstrument.Cells[3, 0]:=rsFocalMm;
  StringGridInstrument.Cells[4, 0]:=rsFD;
  TabSheetBarlow.Caption:=rsBarlowReduce2;
  StringGridBarlow.Cells[0, 0]:=rsName;
  StringGridBarlow.Cells[1, 0]:=rsPower;
  TabSheetEyepiece.Caption:=rsEyepiece;
  StringGridEyepiece.Cells[0, 0]:=rsName;
  StringGridEyepiece.Cells[1, 0]:=rsFocalMm;
  StringGridEyepiece.Cells[2, 0]:=rsField+' [°]';
  TabSheetCamera.Caption:=rsCamera;
  StringGridCamera.Cells[0, 0]:=rsName;
  StringGridCamera.Cells[1, 0]:=rsHorizontalRe;
  StringGridCamera.Cells[2, 0]:=rsVerticalReso;
  StringGridCamera.Cells[3, 0]:=rsPixelSize+' [µ]';

  TabSheetList.Caption:=rsListAndNotes;
  RadioGroupSortList.Caption:=rsDefaultListS;
  RadioGroupSortList.Items[0]:=rsByFormationN;
  RadioGroupSortList.Items[1]:=rsByDate;
  RadioGroupSortList.Items[2]:=rsByTypeOfNote;
  CheckBoxReverseSort.Caption:=rsReverseSortO;
  GroupBoxNote.Caption:=rsNoteOptions;
  CheckBoxEphemeris.Caption:=rsShowEphemeri;
  GroupBoxPrint.Caption:=rsPrintOptions;
  label1.Caption:=rsFontToPrintN;
  label2.Caption:=rsFixedPitchFo;

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

procedure TFSetup.PageControl1Change(Sender: TObject);
begin
  BtnAddRow.Visible:=PageControl1.ActivePageIndex<>6;
end;

procedure TFSetup.SpeedButtonFixedFontClick(Sender: TObject);
begin
  FontDialog1.Font.Name:=FontFixed.Text;
  FontDialog1.Options:=[fdFixedPitchOnly,fdEffects,fdNoSizeSel,fdNoStyleSel];
  if FontDialog1.Execute then
    FontFixed.Text:=FontDialog1.Font.Name;
end;

procedure TFSetup.SpeedButtonFontClick(Sender: TObject);
begin
  FontDialog1.Font.Name:=FontNote.Text;
  FontDialog1.Options:=[fdEffects,fdNoSizeSel,fdNoStyleSel];
  if FontDialog1.Execute then
    FontNote.Text:=FontDialog1.Font.Name;
end;

procedure TFSetup.StringGridValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
var x,y: double;
    n: integer;
begin
  case TStringGrid(sender).tag of
    1: if OldValue<>NewValue then begin // Location
         LocationModified:=true;
         if (aCol=1)and(NewValue<>rsE)and(NewValue<>rsW) then NewValue:=OldValue;
         if (aCol=2) then begin
           val(NewValue,x,n);
           if (n<>0)or(x<0)or(x>180) then NewValue:=OldValue;
         end;
         if (aCol=3) then begin
           val(NewValue,x,n);
           if (n<>0)or(x<-90)or(x>90) then NewValue:=OldValue;
         end;
         if (aCol=4) then begin
           val(NewValue,x,n);
           if (n<>0)or(x<-1000)or(x>10000) then NewValue:=OldValue;
         end;
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
    x: double;
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
    x:=dbnotes.Results[i].Format[2].AsFloat;
    if x>0 then
      StringGridLocation.Cells[1,i+1]:=rsE
    else
      StringGridLocation.Cells[1,i+1]:=rsW;
    StringGridLocation.Cells[2,i+1]:=FormatFloat(f4,abs(x));
    StringGridLocation.Cells[3,i+1]:=dbnotes.Results[i][3];
    StringGridLocation.Cells[4,i+1]:=dbnotes.Results[i][4];
    StringGridLocation.Cells[5,i+1]:=dbnotes.Results[i][5];
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
  RadioGroupSortList.ItemIndex:=DefaultSortCol;
  CheckBoxReverseSort.Checked:=DefaultReverseSort;
  CheckBoxEphemeris.Checked:=ShowEphemeris;
  FontNote.Text:=PrintNoteFont;
  FontFixed.Text:=PrintFixedFont;
end;

function TFSetup.SaveLocationGrid(grid: TStringGrid; table,cmdprefix: string ):boolean;
var cmd: string;
    i,j: integer;
    x: double;
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
    cmd:=cmd+SafeSqlText(grid.Cells[0,i])+'","'; //name
    x:=StrToFloatDef(grid.Cells[2,i],0);
    if trim(grid.Cells[1,i])=rsW then x:=-x;
    cmd:=cmd+FormatFloat(f4,x)+'","';
    for j:=3 to grid.ColCount-1 do begin
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
  result:=true;
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
  result:=true;
end;

function TFSetup.SaveData:boolean;
begin
  result:=false;
  if LocationModified and (StringGridLocation.RowCount>1) then begin
    if not SaveLocationGrid(StringGridLocation,'Location','replace into location (id,name,longitude,latitude,elevation,timezone) values (') then exit;
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
  DefaultSortCol     := RadioGroupSortList.ItemIndex;
  DefaultReverseSort := CheckBoxReverseSort.Checked;
  ShowEphemeris      := CheckBoxEphemeris.Checked;
  PrintNoteFont      := FontNote.Text;
  PrintFixedFont     := FontFixed.Text;

  result:=true
end;

end.

