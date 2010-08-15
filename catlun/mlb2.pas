(*******************************************************************
MY LITTLE BASE 2.0.1 delphi source code
CopyRights owned by S.A.R.L ANIROM Multimedia Marseille FRANCE
http://www.anirom.com
except for the public domain Excel export section found on the web
MLB official website is http://www.mylittlebase.org

This source code is Freeware
You can copy it and use it freely for any purpose (even commercial)
but you must add in the about box of your program that it uses
MyLittleBase source code (http://www.mylittlebase.org)
You can freely distribute this unmodified source code containing
this copyright notice
You can modify it for your own purposes, but you cannot distribute
the modified code as mylittlebase without the written consent from ANIROM
You can write external modules using this unmodified source code
and distribute them

ANIROM Multimedia assumes no liability of any kind
use this code at your own risks or do not use it
*******************************************************************)
{$mode delphi}{$H+}
{$WARNINGS OFF}
unit mlb2;

interface

uses
    SysUtils, Classes;

const MLB_MAJOR_VERSION = 2;
const MLB_MINOR_VERSION = 01;
const MLB_AFTER = true;
const MLB_BEFORE = false;
const MLB_LOWEST = false;
const MLB_GREATEST = true;
const MLB_FORWARD = true;
const MLB_BACKWARD = false;
{errors}
const MLB_ERROR_NOPE = 0;      {NO ERROR HAS BEEN FOUND}
const MLB_ERROR_UNKNOWN = 1;   {ERROR NOT DOCUMENTED}
const MLB_ERROR_BADFORMAT = 2; {THE FILE FORMAT IS NOT CORRECT}
const MLB_ERROR_IO = 3;        {INPUT-OUTPUT ERROR WHILE READING/WRITING FILES}
{-------EXCEL DEBUT INTERFACE !!!}
Const

  TMlb2_Space : char = chr(32);
  TMlb2_Tab   : char = chr(9);
  TMlb2_CR    : char = chr(13);
  TMlb2_LF    : char = chr(10);

  {BOF}
  TMlb2_BOF       = $0009;
  BIT_BIFF5 = $0800;
  BIT_BIFF4 = $0400;
  BIT_BIFF3 = $0200;
  BOF_BIFF5 = TMlb2_BOF or BIT_BIFF5;
  BOF_BIFF4 = TMlb2_BOF or BIT_BIFF4;
  BOF_BIFF3 = TMlb2_BOF or BIT_BIFF3;
  {EOF}
  BIFF_EOF = $000a;
  {Dimensions}
  DIMENSIONS = $0000;
  DIMENSIONS_BIFF4 = DIMENSIONS or BIT_BIFF3;
  DIMENSIONS_BIFF3 = DIMENSIONS or BIT_BIFF3;
  {Document types}
  DOCTYPE_XLS = $0010;
  DOCTYPE_XLC = $0020;
  DOCTYPE_XLM = $0040;
  DOCTYPE_XLW = $0100;
  {Use with output functions}
  VER_BIFF4 = $04;
  VER_BIFF3 = $03;
  VER_BIFF2 = $02;
  {Structures}
  LEN_RECORDHEADER = 4;
  {Data types }
  CellBlank   = 1;
  CellLongInt = 2;
  CellDouble  = 4;
  CellLabel   = 8;
  CellBoolean = 16; { or error }

type MLB_int1 = byte;
type MLB_int2 = Word;
type MLB_int4 = LongInt;
type MLB_endian_test = array [1..2] of MLB_int1;
type PMLB_endian_test = ^MLB_endian_test;
Type
  TFileName = String;
  string10 = String[10]; String255 = string[255];
  chartype = array[0..255] of char;

  PBaseSave = ^TBaseSave;
  TBaseSave = object
    Charfile : file of char;
    DataString : String255; Separator : char;
    MinSaveRecs, MaxSaveRecs, MinSaveCols, MaxSaveCols : word;
    CellType, Row, Col : LongInt;
    DataPointer : pointer;
    EndOfLine : boolean;

    Constructor Init(SaveFileName : String );
    procedure WriteBlank; virtual;
    procedure WriteLongInt; virtual;
    procedure WriteDouble; virtual;
    procedure WriteLabel (var w : word); virtual;
    procedure WriteData(AType, ARow, ACol: LongInt; AData: Pointer); virtual;
    Destructor Done; virtual;
  end;

  PASCII = ^TASCII;
  TASCII = object(TBaseSave)
    Constructor Init( SaveFileName : TFileName );
    Destructor Done; virtual;
  end;

  PExcelTab = ^TExcelTab;
  TExcelTab = object(TBaseSave)
    Constructor Init(SaveFileName : TFileName );
    Destructor Done; virtual;
  end;

  PBIFF2 = ^TBIFF2;
  TBIFF2 = object(TBaseSave)
    {BIFFtime, BIFFdata : double;} BIFFColumn : byte;
    ExcelFile : File;
    VerBIFF, TypeDOC : word;
    typerec, lendata : word;

    constructor Init(AFileName : TFileName);
    destructor Done; virtual;
    procedure BIFFBOF; virtual;
    procedure BIFFDIM; virtual;
    procedure WriteBOF; virtual;
    procedure WriteRecordHeader; virtual;
    procedure WriteDimensions; virtual;
    procedure WriteEOF; virtual;
    procedure WriteData(AType, ARow, ACol: LongInt; AData: Pointer); virtual;
    procedure WriteBlank; virtual;
    procedure WriteLongInt; virtual;
    procedure WriteDouble; virtual;
    procedure WriteLabel (var w : word); virtual;
    procedure WriteBoolean; virtual;
  end;

  PBIFF3 = ^TBIFF3;
  TBIFF3 = object(TBIFF2)
    procedure BIFFBOF; virtual;
    procedure BIFFDIM; virtual;
  end;

  PBIFF4 = ^TBIFF4;
  TBIFF4 = object(TBIFF3)
    procedure BIFFBOF; virtual;
  end;

  PBIFF5 = ^TBIFF5;
  TBIFF5 = object(TBIFF4)
    procedure BIFFBOF; virtual;
  end;

var PSaveFile : PBaseSave;
    index: integer;
{EXCEL FIN INTERFACE}

type
{TKLIST DEBUT INTERFACE}
  PListItem = ^TListItem;
  TListItem = record
        item: Pointer;
        Prev, Next: PListItem;
  end;

  TKList = class(TObject)
  private
        first, current, last: PListItem;
        index, n: LongInt;

        function best_pointer(k: LongInt): LongInt;
  public
        constructor Create;
        destructor Destroy; override;

        procedure Add(item: pointer);
        function Insert(k: LongInt; item: pointer): boolean;
        function Init(ditems: boolean): boolean;
        function Remove(k: LongInt): pointer;
        function Delete(ditems: boolean): boolean;
        procedure Purge;
        function GetItem(k: LongInt): pointer;
        function SetItem(k: LongInt; p: pointer): pointer;
        function GetIndex: LongInt;
        function Count: LongInt;

        procedure AddString(s: string);
        function InsertString(k: LongInt; s: string): boolean;
        function GetString(k: LongInt): string;
        function SetString(k: LongInt; s: string): boolean;
        function IndexOfString(s: string): LongInt;

        function Empty: boolean;
        function Go(k: LongInt): boolean;
        function GoFirst: boolean;
        function GoLast: boolean;
        function GoNext: boolean;
        function GoPrevious: boolean;

        function Exchange(k1, k2: LongInt): boolean;
  end;

  TKBaseList = class(TObject)
  private
        list: TKList;
        function ReadCount: LongInt;
        function ReadItems(index1: LongInt): pointer;
        procedure WriteItems(index1: LongInt; v: pointer);
  public
        constructor Create;
        destructor Destroy; override;
        property Count: LongInt read ReadCount;
        property Items[index1: LongInt]: pointer read ReadItems write WriteItems;
        procedure Clear;
        procedure Pack;
        procedure Add(p: pointer);
        procedure Insert(position1: LongInt; p: pointer);
        procedure Delete(k: LongInt);
        procedure Exchange(k1, k2: LongInt);
  end;

  TKStringList = class(TObject)
  private
        list: TKList;
        function ReadCount: LongInt;
        function ReadStrings(index1: LongInt): string;
        procedure WriteStrings(index1: LongInt; v: string);
  public
        constructor Create;
        destructor Destroy; override;
        procedure Assign(tk: TKStringList);
        property Count: LongInt read ReadCount;
        property Strings[index1: LongInt]: string read ReadStrings write WriteStrings;
        procedure Clear;
        function IndexOf(s: string): LongInt;
        procedure Add(s: string);
        function Delete(k: LongInt): boolean;
  end;
{TKLIST FIN INTERFACE}


{CONCORDANCES DEBUT INTERFACE !!!}
type
  TConcordances = class(TObject)
  private
    function del_spaces(var s: string): string;
  public
    space_matching: boolean;
    case_matching: boolean;
    like_matching: boolean;

    constructor Create;
    destructor Destroy; override;
    function SI_VERIFICATION(sujet1, sujet2: string): boolean;
    function Concordance(sujet1, sujet2: string): boolean;
  end;
{CONCORDANCES FIN INTERFACE !!!}
const RF_BUFFER_SIZE = 1000;

type
  TMlb2_ROW = TKStringList;
  TMlb2_SLOTS = TKBaseList;

  TMlb2ParseCSV = class(TObject)
  private
        csvline: string;
        index: LongInt;
  public
        CSVSeparator: string;
        constructor Create;
        destructor Destroy; override;

        procedure Init(s1: string);
        function NextField(var field: string): Boolean;
        function FromN(s1: string): string;
        function ToN(s1: string): string;
  end;
  PTMlb2IntegerList = ^TMlb2IntegerList;
  TMlb2IntegerList = record
       k: integer;
       nextfield: PTMlb2IntegerList;
  end;
  TMlbFusionArray = string;
  PTMlb2 = ^TMlb2;
  TMlb2 = class(TObject)
  protected
  name_: string;
  current, position: LongInt;
  fields: TMlb2_ROW;
  ftypes: TMlb2_ROW;
  data: TMlb2_SLOTS;
  psv: TMlb2ParseCSV;
  rowcopy: array [1..2] of TMlb2_ROW;
  firstseek: boolean;
  {ISAMPARSE}
      RFBuffer: array [1..RF_BUFFER_SIZE] of char;
      RFI: LongInt;
      RFD: LongInt;
      CHAR1, CHAR2: char;
      VCHAR: char;
      in_quotes, with_quotes: boolean;
      Token: string;
      LikeAgent: TConcordances;
      direction : boolean;

  function local_endian: byte;
  procedure init_error;
  function row(row1: LongInt): TMlb2_ROW;
  function lastrow: TMlb2_ROW;
  function currentrow: TMlb2_ROW;
  function trim2(s1: string): string;
  function FieldNameRead(index1: LongInt): string;
  procedure FieldNameWrite(index1: LongInt; v: string);
  function DataTypeRead(index1: LongInt): string;
  procedure DataTypeWrite(index1: LongInt; v: string);
  function AccessDataRead(field1, index1: LongInt): string;
  function find_extension(filename1: string): string;
  procedure write_text_as_binary(var f: file; t: string);
  function read_text_as_binary(H: integer): string;
  function read_int2_from_other_endian(H: integer): MLB_int2;
  function read_int4_from_other_endian(H: integer): MLB_int4;
  function same_endian(endian1: byte): boolean;
  function tonz(m, n: integer): string;
  function getName: string;
  procedure setName(name1: string);
  procedure nameFromFilename(filename1: string);

  {ISAMPARSE}
    function RFGetChar(H: integer): LongInt;
    function GetNextToken(H: integer): boolean;
    function IsSeparator(s: char): boolean;
    function quote2(s: string): string;
  public
        Distinct: boolean;
        QuoteSeparator: string;
        CSVSeparator: string;
        BeginningOfFile: boolean;
        EndOfFile: boolean;
        MLBError: integer;
        MLBErrorComment: string;
        constructor Create;
        destructor Destroy; override;

        procedure Init;
        procedure Clear;
        procedure Assign(var mlb: TMlb2);
        function GetVersion: String;
        function GetVersionNumber: Integer;
        property Name: string read getName write setName;

        function AddField(fieldname1: string): Boolean;
        function RemoveField(fieldname1: string): Boolean;
        property FieldName[index1: LongInt]: string read FieldNameRead write FieldNameWrite;
        property DataType[index1: LongInt]: string read DataTypeRead write DataTypeWrite;
        function FieldCount: LongInt;

        procedure AddRow;
        function InsertRow(where1: boolean): boolean;
        function RemoveRow: Boolean;
        function RemoveRowByIndex(k: LongInt): Boolean;
        function CopyRow: boolean;
        function PasteRow: boolean;
        function CopyRowBySlot(slot: integer): boolean;
        function PasteRowBySlot(slot: integer): boolean;
        function InitFieldWithData(fieldname1: string; data1: string): boolean;
        function InitFieldWithValue(fieldname1: string; value1: Extended): boolean;
        procedure ForceRows(nrows: LongInt);
        function RowCount: LongInt;

        function GetCurrentRow: LongInt;
        function IsEmpty: Boolean;

        function Go(row1: LongInt): Boolean;
        function GoFirst: Boolean;
        function GoLast: Boolean;
        function GoNext: Boolean;
        function GoPrevious: Boolean;
        function BeginSeek(direction1: boolean): Boolean;
        function EndSeek: Boolean;
        function SeekData(fieldname1, comp1, value1: string): boolean;
        function SeekFloat(fieldname1, comp1: string; value1: Extended): boolean;
        function MatchData(fieldname1, comp1, value1: string): boolean;
        function MatchFloat(fieldname1, comp1: string; value1: Extended): boolean;
        function SavePosition: boolean;
        function RestorePosition: boolean;
        function GetPosition: LongInt;

        function GetData(fieldname1: string): string;
        function SetData(fieldname1: string; data1: string): Boolean;
        function GetDataByIndex(index1: LongInt): string;
        function SetDataByIndex(index1: LongInt; data1: string): Boolean;
        function GetFloat(fieldname1: string): Extended;
        function SetFloat(fieldname1: string; float1: Extended): Boolean;
        function GetFloatByIndex(index1: LongInt): Extended;
        function SetFloatByIndex(index1: LongInt; float1: Extended): Boolean;
        function GetFieldName(index1: LongInt): string;
        function GetFieldIndex(fieldname1: string): LongInt;
        property AccessData[field1, index1: LongInt]: string read AccessDataRead;

        function LoadFromFile(filename1: string): Boolean;
        function LoadFromCSVFile(filename1: string): Boolean;
        function LoadFieldsFromCSVFile(filename1: string): Boolean;
        function LoadFromISAMFile(filename1: string): Boolean;
        function LoadFromMLBFile(filename1: string): Boolean;
        function SaveToFile(FileName1: string): boolean;
        function SaveToCSVFile(filename1: string): Boolean;
        function AppendToCSVFile(filename1: string): Boolean;
        function SaveToISAMFile(filename1: string): Boolean;
        function SaveToMLBFile(filename1: string): Boolean;
        function SaveToExcelFile(FileName1: string): boolean;

        function RobustStrToFloat(s1: string): Extended;
        function RobustFloatToStr(v1: Extended): string;
        function SortByData(fieldname1: string; lowest2greatest1: boolean): boolean;
        function SortByFloat(fieldname1: string; lowest2greatest1: boolean): boolean;
        function RangeSortByData(fieldname1: string; lowest2greatest1: boolean; from1, to1: LongInt): boolean;
        function RangeSortByFloat(fieldname1: string; lowest2greatest1: boolean; from1, to1: LongInt): boolean;
        procedure RandomSort;
        procedure MakeDistinct;
        function AreSameRows(k, l: LongInt): boolean;
        function Fusion(var dest_mlb, source_mlb: TMlb2; a1: TMlbFusionArray): boolean;
  end;

function Trim(s1: string): string;
implementation

function Trim(s1: string): string;
var i: LongInt;
    r: string;
begin
     i := 1;
     while (i<=length(s1)) and (s1[i] = ' ') do i:=i+1;
     r := Copy(s1, i, length(s1)-i+1);
     i := length(r);
     while (i>0) and (r[i] = ' ') do i:=i-1;
     Result := Copy(r, 1, i);
end;

{CONCORDANCES DEBUT IMPLEMENTATION !!!}
constructor TConcordances.Create;
begin
     inherited Create;
     like_matching := false;
     case_matching := true;
     space_matching := true;
end;

destructor TConcordances.Destroy;
begin
     inherited Destroy;
end;

function TConcordances.del_spaces(var s: string): string;
var i: integer;
    token: string;
begin
     i := 1;
     token := '';
     while (i<=length(s)) do begin
         if (s[i]=' ') then begin
         end else begin
             token := token + s[i];
         end;
         Inc(i, 1);
     end;
     Result := token;
end;

function TConcordances.SI_VERIFICATION(sujet1, sujet2: string): boolean;
var s1, s2: string;
begin
     s1 := sujet1;
     s2 := sujet2;
     if space_matching then begin
     end else begin
        del_spaces(s1);
        del_spaces(s2);
     end;
     if case_matching then begin
     end else begin
        s1 := UpperCase(s1);
        s2 := UpperCase(s2);
     end;
     if like_matching then begin
        Result := Concordance(s1, s2);
     end else begin
        Result := s1 = s2;
     end;
end;

function TConcordances.Concordance(sujet1, sujet2: string): boolean;
var i: integer;
    j: integer;
    k: integer;
    concorde: boolean;
    n1, n2: integer;
    capting_set: boolean;
    set_not: boolean;
    set_separator: boolean;
    myset: set of char;
    mychar: char;
begin
     i := 1;
     j := 0;
     concorde := true;
     capting_set := false;
     set_separator := false;
     set_not := false;
     myset := [];
     mychar := #0;
     while (i<=length(sujet1)) and concorde do begin
           if capting_set then begin
               if sujet1[i] = ']' then begin
                   capting_set := false;
                   Dec(i, 1);
               end else if (sujet1[i] = '-') and (mychar<>#0) then begin
                   set_separator := true;
               end else if (sujet1[i] = '!') and (mychar=#0) then begin
                   set_not := true;
               end else begin
                   if set_separator then begin
                      for k:=ord(mychar)+1 to ord(sujet1[i]) do begin
                          myset := myset + [chr(k)];
                      end;
                      set_separator := false;
                   end else begin
                      mychar := sujet1[i];
                      myset := myset + [mychar];
                   end;
               end;
           end else begin
               if sujet1[i] = '*' then begin
                  concorde := false;
                  n1 := 0;
                  repeat
                        Inc(i, 1);
                        Inc(n1, 1);
                  until (i>length(sujet1)) or not (sujet1[i] in ['*', '?']);
                  Dec(n1, 1);
                  n2 := 0;
                  if (i<=length(sujet1)) and (sujet1[i]='#') then begin
                     Inc(j, 1);
                     while (j<=length(sujet2)) and (sujet2[j] in ['0'..'9']) do begin
                           Inc(j, 1);
                           Inc(n2, 1);
                     end;
                  end else if (i<=length(sujet1)) then begin
                     Inc(j, 1);
                     while (j<=length(sujet2)) and (sujet2[j]<>sujet1[i]) do begin
                           Inc(j, 1);
                           Inc(n2, 1);
                     end;
                  end else begin
                     n2 := length(sujet2)-j+1;
                     j := length(sujet2) + 1;
                     concorde := n2>=n1;
                  end;
                  concorde := concorde or ((j<=length(sujet2)) and (n2>=n1));
               end else if sujet1[i] = '?' then begin
                   Inc(j, 1);
                   concorde := (j<=length(sujet2));
               end else if sujet1[i] = '#' then begin
                   Inc(j, 1);
                   concorde := (j<=length(sujet2)) and (sujet2[j] in ['0'..'9']);
               end else if sujet1[i] = '[' then begin
                   set_separator := false;
                   set_not := false;
                   capting_set := true;
                   myset := [];
                   mychar := #0;
               end else if sujet1[i] = ']' then begin
                   Inc(j, 1);
                   if set_not then begin
                       concorde := (j<=length(sujet2)) and not (sujet2[j] in myset);
                   end else begin
                       concorde := (j<=length(sujet2)) and (sujet2[j] in myset);
                   end;
               end else begin
                   Inc(j, 1);
                   concorde := (j<=length(sujet2)) and (sujet2[j]=sujet1[i]);
               end;
           end;
           Inc(i, 1);
     end;
     Result := concorde and (j>=length(sujet2));
end;
{CONCORDANCES FIN IMPLEMENTATION !!!}

procedure TMlb2.nameFromFilename(filename1: string);
var f: string;
    i: integer;
begin
     f := ExtractFileName(filename1);
     i := length(f);
     while (i>0) and (f[i]<>'.') do begin
           Dec(i, 1);
     end;
     Name := Copy(f, 1, i-1);
end;

function TMlb2.getName: string;
begin
     Result := name_;
end;

procedure TMlb2.setName(name1: string);
begin
     name_ := name1;
end;

constructor TMlb2.Create;
begin
     inherited Create;

     Name := '';
     MLBError := MLB_ERROR_NOPE;
     MLBErrorComment := '';
     QuoteSeparator := '"';
     CSVSeparator := ';';
     fields := TMlb2_ROW.Create;
     {fields.Sorted := False;}
     ftypes := TMlb2_ROW.Create;
     {ftypes.Sorted := False;}
     data := TMlb2_SLOTS.Create;
     psv := TMlb2ParseCSV.Create;
     LikeAgent := TConcordances.Create;
     LikeAgent.case_matching := False;
     LikeAgent.space_matching := True;
     LikeAgent.like_matching := True;
     direction := MLB_FORWARD;
     rowcopy[1] := TMlb2_ROW.Create;
     rowcopy[2] := TMlb2_ROW.Create;
     Init;
end;

destructor TMlb2.Destroy;
begin
     Init;
     psv.Free;
     rowcopy[1].Free;
     rowcopy[2].Free;
     data.Free;
     fields.Free;
     ftypes.Free;
     LikeAgent.Free;
     inherited Destroy;
end;

procedure TMlb2.Assign(var mlb: TMlb2);
var i, j: LongInt;
begin
     Init;
     for i:=1 to mlb.FieldCount do begin
         AddField(mlb.FieldName[i]);
         DataType[i] := mlb.DataType[i];
     end;
     for i:=1 to mlb.RowCount do begin
         AddRow;
         for j:=1 to mlb.FieldCount do begin
             SetDataByIndex(j, mlb.AccessData[j, i]);
         end;
     end;
end;

{ISAM PARSE FUNCTIONS --------------------------------------------}
function TMlb2.RFGetChar(H: integer): LongInt;
var r: integer;
begin
     Inc(RFI, 1);
     if RFI>RFD then begin
        r := FileRead(H, RFBuffer, RF_BUFFER_SIZE);
        RFD := r;
        if r=0 then begin
           Result := 1;
           Exit;
        end;
        RFI := 1;
     end else begin
     end;
     CHAR1 := RFBuffer[RFI];
     Inc(RFI, 1);
     if RFI>RFD then begin
        r := FileRead(H, RFBuffer, RF_BUFFER_SIZE);
        RFD := r;
        if r=0 then begin
           Result := 2;
           Exit;
        end;
        RFI := 1;
     end else begin
     end;
     CHAR2 := RFBuffer[RFI];
     Dec(RFI, 1);
     Result := 0;
end;

function TMlb2.IsSeparator(s: char): boolean;
begin
     Result := (s=CSVSeparator) or (s=chr(13));
end;

function TMlb2.GetNextToken(H: integer): boolean;
var  gr: LongInt;
     trouve: boolean;
begin
     trouve := false;
     token := '';
     gr := 0;
     in_quotes := false;
     with_quotes := false;
     VCHAR := ' ';
     while (gr=0) and not trouve do begin
         gr := RFGetChar(H);
         if (gr<>1) then begin
             if not in_quotes then begin
             {CAS OU ON EST PAS DANS LES QUOTES}
                 if CHAR1=QuoteSeparator then begin
                    with_quotes := true;
                    in_quotes := true;
                 end else if CHAR1=' ' then begin
                 end else if CHAR1=chr(10) then begin
                 end else if IsSeparator(CHAR1) then begin
                     VCHAR := CHAR1;
                     trouve := true;
                 end else begin
                     token := token + CHAR1;
                 end;
             end else begin
             {CAS OU ON EST DANS LES QUOTES}
                 if CHAR1=QuoteSeparator then begin
                    if (gr<>2) and (CHAR2=QuoteSeparator) then begin
                       token := token + QuoteSeparator;
                       RFGetChar(H);
                    end else begin
                       in_quotes := false;
                    end;
                 {end else if CHAR1='"' then begin}
                 end else begin
                    token := token + CHAR1;
                 end;
             end;
         end else begin
         end;
     end;
     Result := gr=0;
end;

function TMlb2.LoadFromISAMFile(filename1: string): Boolean;
var H: integer;
    fin, premier: boolean;
    k: LongInt;
begin
     H := FileOpen(filename1, $0);
     if H>0 then begin
        nameFromFilename(filename1);
        Init;
        RFI := 0;
        RFD := 0;

        VCHAR := ' ';
        {Lecture des champs}
        fin := false;
        while (VCHAR<>#13) and (not fin) do begin
             fin := not GetNextToken(H);
             if Token<>'' then begin
                AddField(Token);
             end else begin
             end;
        end;

        {Lecture des Données}
        while not fin do begin
              VCHAR := ' ';
              premier := true;
              k := 1;
              while (VCHAR<>#13) and (not fin) do begin
                   fin := not GetNextToken(H);
                   if not fin then begin
                     if premier then begin
                        GoLast;
                        AddRow;
                     end;
                     if Token<>'' then begin
                        if premier then begin
                            premier := false;
                            if not with_quotes then begin
                               ftypes.Strings[k-1] := 'FLOAT';
                            end else begin
                               ftypes.Strings[k-1] := 'STRING';
                            end;
                        end else begin
                        end;
                        SetDataByIndex(k, Token);
                     end else begin
                        if premier then begin
                            premier := false;
                            if not with_quotes then begin
                               ftypes.Strings[k-1] := 'STRING';
                            end else begin
                            end;
                        end else begin
                        end;
                     end;
                     Inc(k, 1);
                   end;
              end;
        end;

        FileClose(H);
        Result := true;
     end else begin
        Result := false;
     end;
end;
{-------------------------------------------------------------------}

procedure TMlb2.init_error;
begin
     MLBError := MLB_ERROR_NOPE;
     MLBErrorComment := '';
end;

function TMlb2.GetVersionNumber: Integer;
begin
     init_error;
     Result := 100*MLB_MAJOR_VERSION + MLB_MINOR_VERSION;
end;

function TMlb2.tonz(m, n: integer): string;
var k1: string;
    i: integer;
begin
     k1 := IntToStr(m);
     for i:=1 to n-length(k1) do begin
         k1 := '0' + k1;
     end;
     Result := k1;
end;

function TMlb2.GetVersion: String;
begin
     init_error;
     Result := 'MyLittleBase version ' + IntToStr(MLB_MAJOR_VERSION) + '.' + tonz(MLB_MINOR_VERSION, 2);
end;

function TMlb2.trim2(s1: string): string;
var i: LongInt;
begin
     i := 1;
     while (i<=length(s1)) and (s1[i] = ' ') do i:=i+1;
     Result := Copy(s1, i, length(s1)-i+1);
end;

function TMlb2.quote2(s: string): string;
var i: LongInt;
    r: string;
begin
     i:=1;
     r := '';
     while i<=length(s) do begin
           if s[i]='"' then begin
              r := r + '""';
           end else begin
              r := r + s[i];
           end;
           i := i + 1;
     end;
     Result := r;
end;

function TMlb2.AccessDataRead(field1, index1: LongInt): string;
begin
     If (field1>0) and (field1<=fields.Count) then begin
        Result := row(index1-1).Strings[field1-1];
     end else begin
        Result := '';
     end;
end;

function TMlb2.DataTypeRead(index1: LongInt): string;
begin
     If (index1>0) and (index1<=fields.Count) then begin
        Result := ftypes.Strings[index1-1];
     end else begin
        Result := '';
     end;
end;

procedure TMlb2.DataTypeWrite(index1: LongInt; v: string);
begin
     If (index1>0) and (index1<=fields.Count) then begin
        ftypes.Strings[index1-1] := v;
     end else begin
     end;
end;

function TMlb2.FieldNameRead(index1: LongInt): string;
begin
     If (index1>0) and (index1<=fields.Count) then begin
        Result := fields.Strings[index1-1];
     end else begin
        Result := '';
     end;
end;

procedure TMlb2.FieldNameWrite(index1: LongInt; v: string);
begin
     If (index1>0) and (index1<=fields.Count) then begin
        fields.Strings[index1-1] := v;
     end else begin
     end;
end;

{INITIALISATION DES STRUCTURES DE DONNEES}
procedure TMlb2.Init;
var i: LongInt;
begin
     init_error;
     fields.Clear;
     ftypes.Clear;
     For i:=0 to (data.Count-1) do begin
         TMlb2_ROW(data.Items[i]).Free;
     end;
     data.Clear;
     data.Pack;
     current := -1;
     position := -1;
     firstseek := False;
     Distinct := False;
     BeginningOfFile := True;
     EndOfFile := True;
end;

procedure TMlb2.Clear;
var i: LongInt;
begin
     init_error;
     For i:=0 to (data.Count-1) do begin
         TMlb2_ROW(data.Items[i]).Free;
     end;
     data.Clear;
     data.Pack;
     current := -1;
     position := -1;
     firstseek := False;
     Distinct := False;
     BeginningOfFile := True;
     EndOfFile := True;
end;

procedure TMlb2.MakeDistinct;
var i, j: LongInt;
begin
    init_error;
    i := 1;
    while (i<=RowCount) do begin
            j := i+1;
            while (j<=RowCount) do begin
                    if (AreSameRows(i, j)) then begin
                            RemoveRowByIndex(j);
                            Dec(j, 1);
                    end else begin
                    end;
                    Inc(j, 1);
            end;
            Inc(i, 1);
    end;
end;

{AJOUT D'UN CHAMPS A LA TABLE}
function TMlb2.AddField(fieldname1: string): Boolean;
var i: LongInt;
begin
     if (Length(Trim2(fieldname1))>0) and (fields.IndexOf(fieldname1) < 0) then begin
      fields.Add(fieldname1);
      ftypes.Add('STRING');
      rowcopy[1].Add('');
      rowcopy[2].Add('');
      {Ajouter un élément à tous les data}
      For i:=0 to data.Count-1 do begin
          row(i).Add('');
      end;
      Result := True;
     end else begin
      Result := False;
     end;
end;

function TMlb2.row(row1: LongInt): TMlb2_ROW;
begin
     if (row1>=0) and (row1<data.Count) then begin
        Result := TMlb2_ROW(data.Items[row1]);
     end else begin
        Result := nil;
     end;
end;

function TMlb2.lastrow: TMlb2_ROW;
begin
     Result := row(data.Count-1);
end;

function TMlb2.currentrow: TMlb2_ROW;
begin
     Result := TMlb2_ROW(data.Items[current]);
end;

procedure TMlb2.AddRow;
var i: LongInt;
begin
     data.Add(TMlb2_ROW.Create);
     {lastrow.Sorted := False;}
     {Ajouter autant de valeurs que de champs}
     For i:=1 to fields.Count do begin
         lastrow.Add('');
     end;
     GoLast;
end;

function TMlb2.InsertRow(where1: boolean): boolean;
var i: LongInt;
    myrow: TMlb2_ROW;
begin
     myrow := TMlb2_ROW.Create;
     if where1 then begin
        data.Insert(current+1, myrow);
        GoNext;
     end else begin
        data.Insert(current, myrow);
     end;
     {myrow.Sorted := False;}
     {Ajouter autant de valeurs que de champs}
     For i:=1 to fields.Count do begin
         myrow.Add('');
     end;
     Result := True;
end;

function TMlb2.GetCurrentRow: LongInt;
begin
     Result := current + 1;
end;

function TMlb2.IsEmpty: Boolean;
begin
     Result := data.Count<=0;
end;

function TMlb2.RemoveField(fieldname1: string): Boolean;
var i, k: LongInt;
begin
     k := fields.IndexOf(fieldname1);
     if k>=0 then begin
         fields.Delete(k);
         ftypes.Delete(k);
         rowcopy[1].Delete(k);
         rowcopy[2].Delete(k);
         for i:=0 to data.Count-1 do begin
             row(i).Delete(k);
         end;
         Result := True;
     end else begin
         Result := False;
     end;
end;

function TMlb2.AreSameRows(k, l: LongInt): boolean;
var i: LongInt;
begin
     for i:=1 to FieldCount do begin
        if (AccessData[i, k]<>AccessData[i, l]) then begin
                Result := false;
                Exit;
        end else begin
        end;
     end;
     Result := true;
end;

function TMlb2.RemoveRow: Boolean;
begin
     Result := RemoveRowByIndex(GetCurrentRow);
     if (GetCurrentRow>0) and (GetCurrentRow<=RowCount) then begin
     end else begin
         GoLast;
     end;
end;

function TMlb2.RemoveRowByIndex(k: LongInt): Boolean;
begin
     {detruit la ligne courante}
     If (k>0) and (k<=data.Count) then begin
         row(k-1).Free;
         data.Delete(k-1);
         data.Pack;
         Result := True;
     end else begin
         Result := False;
     end;
end;

function TMlb2.Go(row1: LongInt): Boolean;
begin
     If (row1>0) and (row1<=data.Count) then begin
        current := row1-1;
        BeginningOfFile := False;
        EndOfFile := False;
        Result := True;
     end else begin
        BeginningOfFile := True;
        EndOfFile := True;
        Result := False;
     end;
end;

function TMlb2.GoFirst: Boolean;
begin
     if not IsEmpty then begin
        current := 0;
        BeginningOfFile := False;
        EndOfFile := False;
        Result := True;
     end else begin
        current := -1;
        BeginningOfFile := True;
        EndOfFile := True;
        Result := False;
     end;
end;

function TMlb2.GoLast: Boolean;
begin
     if not IsEmpty then begin
        current := data.Count-1;
        BeginningOfFile := False;
        EndOfFile := False;
        Result := True;
     end else begin
        current := -1;
        BeginningOfFile := True;
        EndOfFile := True;
        Result := False;
     end;
end;

function TMlb2.GoNext: Boolean;
begin
     if not IsEmpty and (current<(data.Count-1)) then begin
        Inc(current, 1);
        BeginningOfFile := False;
        EndOfFile := False;
        Result := True;
     end else begin
        BeginningOfFile := IsEmpty;
        EndOfFile := true;
        Result := False;
     end;
end;

function TMlb2.GoPrevious: Boolean;
begin
     if not IsEmpty and (current>0) then begin
        Dec(current, 1);
        BeginningOfFile := False;
        EndOfFile := False;
        Result := True;
     end else begin
        BeginningOfFile := true;
        EndOfFile := IsEmpty;
        Result := False;
     end;
end;

function TMlb2.GetFieldName(index1: LongInt): string;
begin
     If (index1>0) and (index1<=fields.Count) then begin
        Result := fields.Strings[index1-1];
     end else begin
        Result := '';
     end;
end;

function TMlb2.GetFieldIndex(fieldname1: string): LongInt;
var k: LongInt;
begin
     k := fields.IndexOf(fieldname1);
     If k>=0 then begin
        Result := k+1;
     end else begin
        Result := 0;
     end;
end;

function TMlb2.GetData(fieldname1: string): string;
var k: LongInt;
begin
     k := fields.IndexOf(fieldname1);
     If k>=0 then begin
         If current>=0 then begin
            Result := row(current).Strings[k];
         end else begin
            Result := '';
         end;
     end else begin
         Result := '';
     end;
end;

function TMlb2.SetData(fieldname1: string; data1: string): Boolean;
var k: LongInt;
begin
     k := fields.IndexOf(fieldname1);
     If k>=0 then begin
         If current>=0 then begin
            row(current).Strings[k] := data1;
            Result := True;
         end else begin
            Result := False;
         end;
     end else begin
         Result := False;
     end;
end;


function TMlb2.GetDataByIndex(index1: LongInt): string;
begin
     If (index1>0) and (index1<=fields.Count) then begin
         If current>=0 then begin
            Result := row(current).Strings[index1-1];
         end else begin
            Result := '';
         end;
     end else begin
         Result := '';
     end;
end;

function TMlb2.SetDataByIndex(index1: LongInt; data1: string): Boolean;
begin
     If (index1>0) and (index1<=fields.Count) then begin
         If current>=0 then begin
            row(current).Strings[index1-1] := data1;
            Result := True;
         end else begin
            Result := False;
         end;
     end else begin
         Result := False;
     end;
end;


function TMlb2.GetFloat(fieldname1: string): Extended;
var k: LongInt;
begin
     k := fields.IndexOf(fieldname1);
     If k>=0 then begin
         If current>=0 then begin
            Result := RobustStrToFloat(row(current).Strings[k]);
         end else begin
            Result := 0.0;
         end;
     end else begin
         Result := 0.0;
     end;
end;

function TMlb2.SetFloat(fieldname1: string; float1: Extended): Boolean;
var k: LongInt;
begin
     k := fields.IndexOf(fieldname1);
     If k>=0 then begin
         If current>=0 then begin
            row(current).Strings[k] := RobustFloatToStr(float1);
            Result := True;
         end else begin
            Result := False;
         end;
     end else begin
         Result := False;
     end;
end;

function TMlb2.GetFloatByIndex(index1: LongInt): Extended;
begin
     If (index1>0) and (index1<=fields.Count) then begin
         If current>=0 then begin
            Result := RobustStrToFloat(row(current).Strings[index1-1]);
         end else begin
            Result := 0.0;
         end;
     end else begin
         Result := 0.0;
     end;
end;

function TMlb2.SetFloatByIndex(index1: LongInt; float1: Extended): Boolean;
begin
     If (index1>0) and (index1<=fields.Count) then begin
         If current>=0 then begin
            row(current).Strings[index1-1] := RobustFloatToStr(float1);
            Result := True;
         end else begin
            Result := False;
         end;
     end else begin
         Result := False;
     end;
end;

function TMlb2.find_extension(filename1: string): string;
begin
     if length(filename1)>=3 then begin
         Result := UpperCase(Copy(filename1, length(filename1)-2, 3));
     end else begin
         Result := '';
     end;
end;

function TMlb2.LoadFromFile(filename1: string): Boolean;
var extension: string;
begin
     init_error;
     extension := find_extension(filename1);
     if extension = 'TXT' then begin
         Result := LoadFromISAMFile(filename1);
     end else if extension = 'CSV' then begin
         Result := LoadFromCSVFile(filename1);
     end else if extension = 'MLB' then begin
         Result := LoadFromMLBFile(filename1);
     end else begin
         Result := LoadFromCSVFile(filename1);
     end;
end;

function TMlb2.LoadFromMLBFile(filename1: string): Boolean;
var H: Integer;
    bdummy: MLB_int1;
    ddummy: MLB_int2;
    ldummy: MLB_int4;
    i, j, nf, nv: LongInt;
    bcount: LongInt;
    signature: array [1..3] of char;
begin
     init_error;
     H := FileOpen(FileName1, $0);
     If H>0 then begin
        {Reads the SIGNATURE}
        FileRead(H, signature, 3);
        if (signature[1]<>'M') or (signature[2]<>'L') or (signature[3]<>'B') then begin
           {This is not a MyLittleBase file}
           MLBError := MLB_ERROR_BADFORMAT;
           MLBErrorComment := '1-File''s Signature must be MLB';
           FileClose(H);
           Result := False; Exit;
        end;
        {Reads versions numbers, ignored}
        {MAJOR VERSION NUMBER}
        FileRead(H, bdummy, sizeof(MLB_int1));
        {MINOR VERSION NUMBER}
        FileRead(H, bdummy, sizeof(MLB_int1));
        {LITTLE ENDIAN ?}
        FileRead(H, bdummy, sizeof(MLB_int1));
        if same_endian(bdummy) then begin
{SAME Endian}
            {TABLES COUNT}
            FileRead(H, ddummy, sizeof(MLB_int2));
            If (ddummy<1) then begin
               {number of tables must be at least 1}
               MLBError := MLB_ERROR_BADFORMAT;
               MLBErrorComment := '2-number of tables must be at least 1';
               FileClose(H);
               Result := False; Exit;
            end;
            {ADDITIONAL COUNT} {ignored in MLB 2.00}
            FileRead(H, ddummy, sizeof(MLB_int2));

            {BLOCKID FOR TABLE 1}
            FileRead(H, ddummy, sizeof(MLB_int2));
            if (ddummy<>0) then begin
               {The first block must be a TABLE in this version}
               MLBError := MLB_ERROR_BADFORMAT;
               MLBErrorComment := '3-First Block must be a TABLE';
               FileClose(H);
               Result := False; Exit;
            end;
            Init; {Reinits MyLittleBase}
            {length of table data}
            FileRead(H, bcount, sizeof(MLB_int4));
            {TABLEID FOR TABLE 1 Not used in this version}
            FileRead(H, ddummy, sizeof(MLB_int2));
            {TABLENAME FOR TABLE 1}
            Name := read_text_as_binary(H);

            {FIELDS COUNT}
            FileRead(H, nf, sizeof(MLB_int4));
            {ROWS COUNT}
            FileRead(H, nv, sizeof(MLB_int4));

            for i:=1 to nf do begin
                {Reads THE DATA TYPE}
                FileRead(H, bdummy, sizeof(MLB_int1));
                {Reads FIELDNAME and Adds the new field}
                AddField(read_text_as_binary(H));
                case bdummy of
                     0: begin
                        DataType[i] := 'STRING';
                     end;
                     1: begin
                        DataType[i] := 'FLOAT';
                     end;
                     else begin
                        DataType[i] := 'STRING';
                     end;
                end;
            end;
            for j:=1 to nv do begin
                {Reads the row length, ignored, reserved for read from disk operations}
                FileRead(H, ldummy, sizeof(MLB_int4));
                AddRow;
                for i:=1 to nf do begin
                    SetDataByIndex(i, read_text_as_binary(H));
                end;
            end;
        end else begin
{Other endian}
            {TABLES COUNT}
            ddummy := read_int2_from_other_endian(H);
            If (ddummy<1) then begin
               {number of tables must be at least 1}
               MLBError := MLB_ERROR_BADFORMAT;
               MLBErrorComment := '2-number of tables must be at least 1';
               FileClose(H);
               Result := False; Exit;
            end;
            {ADDITIONAL COUNT} {ignored in MLB 2.00}
            ddummy := read_int2_from_other_endian(H);

            {BLOCKID FOR TABLE 1}
            ddummy := read_int2_from_other_endian(H);
            if (ddummy<>0) then begin
               {The first block must be a TABLE in this version}
               MLBError := MLB_ERROR_BADFORMAT;
               MLBErrorComment := '3-First Block must be a TABLE';
               FileClose(H);
               Result := False; Exit;
            end;
            Init; {Reinits MyLittleBase}
            {length of table data}
            bcount := read_int4_from_other_endian(H);
            {TABLEID FOR TABLE 1 Not used in this version}
            ddummy := read_int2_from_other_endian(H);
            {TABLENAME FOR TABLE 1}
            Name := read_text_as_binary(H);

            {FIELDS COUNT}
            nf := read_int4_from_other_endian(H);
            {ROWS COUNT}
            nv := read_int4_from_other_endian(H);

            for i:=1 to nf do begin
                {Reads THE DATA TYPE}
                FileRead(H, bdummy, sizeof(MLB_int1));
                {Reads FIELDNAME and Adds the new field}
                AddField(read_text_as_binary(H));
                case bdummy of
                     0: begin
                        DataType[i] := 'STRING';
                     end;
                     1: begin
                        DataType[i] := 'FLOAT';
                     end;
                     else begin
                        DataType[i] := 'STRING';
                     end;
                end;
            end;
            for j:=1 to nv do begin
                {Reads the row length, ignored, reserved for read from disk operations}
                ldummy := read_int4_from_other_endian(H);
                AddRow;
                for i:=1 to nf do begin
                    SetDataByIndex(i, read_text_as_binary(H));
                end;
            end;
        end;
        FileClose(H);
        Result := True;
     end else begin
        MLBError := MLB_ERROR_IO;
        MLBErrorComment := '4-Unable to open file for reading';
        Result := False;
     end;
end;

function TMlb2.LoadFromCSVFile(filename1: string): Boolean;
var F: TextFile;
    fline, token: string;
    is_first_line: boolean;
    i, k: LongInt;
begin
     init_error;
     AssignFile(F, filename1);
     {$i-}
        Reset(F);
     {$i+}
     If (Trim(filename1)='') or (IoResult<>0) then begin
        {CloseFile(F);}
        MLBError := MLB_ERROR_IO;
        MLBErrorComment := '1-Unable to open the file for reading';
        Result := False;
        Exit;
     end else begin
         nameFromFilename(filename1);
         Result := True;
         Init;
         is_first_line := True;
         While Not Eof(F) Do begin
               ReadLn(F, fline);
               if (not is_first_line) and (Trim(fline)<>'') then begin
                  AddRow;
               end else begin
               end;
               i := 1;
               k := 0;
               token := '';
               while (i<=length(fline)) do begin
                     if (fline[i] = '\') then begin
                        if (i<length(fline)) then begin
                            if (UpperCase(fline[i+1])='N') then begin
                               token := token + #13 + #10;
                            end else begin
                               token := token + fline[i+1];
                            end;
                            Inc(i, 1);
                        end else begin
                            token := token + fline[i];
                        end;
                     end else if (fline[i]=CSVSeparator) then begin
                          if is_first_line then begin
                             AddField(token);
                          end else begin
                             lastrow.Strings[k] := token;
                          end;
                          Inc(k, 1);
                          token := '';
                     end else begin
                          token := token + fline[i];
                     end;
                     Inc(i, 1);
               end;
               if is_first_line then begin
                 AddField(token);
               end else begin
                 lastrow.Strings[k] := token;
               end;
               is_first_line := False;
         end;
         CloseFile(F);
     end;
end;

function TMlb2.LoadFieldsFromCSVFile(filename1: string): Boolean;
var F: TextFile;
    fline, token: string;
    i, k: LongInt;
begin
     init_error;
     AssignFile(F, filename1);
     {$i-}
        Reset(F);
     {$i+}
     If (Trim(filename1)='') or (IoResult<>0) then begin
        {CloseFile(F);}
        MLBError := MLB_ERROR_IO;
        MLBErrorComment := '1-Unable to open the file for reading';
        Result := False;
        Exit;
     end else begin
         nameFromFilename(filename1);
         Result := True;
         Init;
         if Not Eof(F) then begin
               ReadLn(F, fline);
               i := 1;
               k := 0;
               token := '';
               while (i<=length(fline)) do begin
                     if (fline[i] = '\') then begin
                        if (i<length(fline)) then begin
                            if (UpperCase(fline[i+1])='N') then begin
                               token := token + #13 + #10;
                            end else begin
                               token := token + fline[i+1];
                            end;
                            Inc(i, 1);
                        end else begin
                            token := token + fline[i];
                        end;
                     end else if (fline[i]=CSVSeparator) then begin
                          AddField(token);
                          Inc(k, 1);
                          token := '';
                     end else begin
                          token := token + fline[i];
                     end;
                     Inc(i, 1);
               end;
               AddField(token);
         end else begin
             Result := False;
         end;
         CloseFile(F);
     end;
end;

function TMlb2.SaveToFile(FileName1: string): boolean;
var extension: string;
begin
     extension := find_extension(filename1);
     if extension = 'TXT' then begin
         Result := SaveToISAMFile(filename1);
     end else if extension = 'CSV' then begin
         Result := SaveToCSVFile(filename1);
     end else if extension = 'MLB' then begin
         Result := SaveToMLBFile(filename1);
     end else begin
         Result := SaveToCSVFile(filename1);
     end;
end;

procedure TMlb2.write_text_as_binary(var f: file; t: string);
var l: LongInt;
    r: integer;
    p: PChar;
begin
     l := length(t);
     BlockWrite(f, l, 4, r);
     p := @t[1];
     BlockWrite(f, p^, l, r);
end;

function TMlb2.read_text_as_binary(H: integer): string;
var l: LongInt;
    p: PChar;
    res: string;
begin
     FileRead(H, l, sizeof(LongInt));
     p := AllocMem(l+1);
     FileRead(H, p^, l);
     p[l] := #0;
     res := StrPas(p);
     FreeMem(p, l);
     Result := res;
end;

function TMlb2.local_endian: byte;
var ktest: MLB_int2;
    ptest: PMLB_endian_test;
begin
     ktest := $FF00;
     ptest := PMLB_endian_test(@ktest);
     if (ptest^[2]=$FF) then begin
        Result := 1;
     end else begin
        Result := 0;
     end;
end;

function TMlb2.read_int2_from_other_endian(H: integer): MLB_int2;
var p: array [1..3] of byte;
begin
    FileRead(H, p, 2*sizeof(byte));
    p[3] := p[2]; p[2] := p[1]; p[1] := p[3];
    Result := MLB_int2((@p)^);
end;

function TMlb2.read_int4_from_other_endian(H: integer): MLB_int4;
var p: array [1..5] of byte;
begin
    FileRead(H, p, 4*sizeof(byte));
    p[5] := p[4]; p[4] := p[1]; p[1] := p[5];
    p[5] := p[3]; p[3] := p[2]; p[2] := p[5];
    Result := MLB_int4((@p)^);
end;

function TMlb2.SaveToMLBFile(FileName1: string): boolean;
var F: file;
    r: integer;
    cdummy: char;
    bdummy: MLB_int1;
    ddummy: MLB_int2;
    ldummy: MLB_int4;
    table_offset: LongInt;
    row_offset: LongInt;
    my_offset: LongInt;
    i, j: LongInt;
    bcount, rcount: LongInt;
    data: string;
begin
     init_error;
     AssignFile(F, FileName1);
     {$i-}
     Rewrite(F, 1);
     {$i+}
     If IoResult=0 then begin
        {Write the SIGNATURE}
        cdummy := 'M'; BlockWrite(F, cdummy, sizeof(char), r);
        cdummy := 'L'; BlockWrite(F, cdummy, sizeof(char), r);
        cdummy := 'B'; BlockWrite(F, cdummy, sizeof(char), r);
        {MAJOR VERSION NUMBER}
        bdummy := MLB_MAJOR_VERSION;
        BlockWrite(F, bdummy, sizeof(MLB_int1), r);
        {MINOR VERSION NUMBER}
        bdummy := MLB_MINOR_VERSION;
        BlockWrite(F, bdummy, sizeof(MLB_int1), r);
        {LITTLE ENDIAN ?}
        bdummy := local_endian;
        BlockWrite(F, bdummy, sizeof(MLB_int1), r);
        {TABLES COUNT}
        ddummy := 1;
        BlockWrite(F, ddummy, sizeof(MLB_int2), r);
        {ADDITIONAL COUNT}
        ddummy := 0;
        BlockWrite(F, ddummy, sizeof(MLB_int2), r);

        {BLOCKID FOR TABLE 1}
        ddummy := 0;
        BlockWrite(F, ddummy, sizeof(MLB_int2), r);
        {Saves The Position to Save the block length}
        bcount := 0;
        table_offset := FilePos(F);
        ldummy := 0;
        BlockWrite(F, ldummy, sizeof(MLB_int4), r);
        {TABLEID FOR TABLE 1 Not used in this version}
        ddummy := 0;
        BlockWrite(F, ddummy, sizeof(MLB_int2), r);
        bcount := bcount + 2;
        {TABLENAME FOR TABLE 1}
        write_text_as_binary(F, Name);
        bcount := bcount + 4 + length(Name);

        {FIELDS COUNT}
        ldummy := FieldCount;
        BlockWrite(F, ldummy, sizeof(MLB_int4), r);
        bcount := bcount + 4;
        {ROWS COUNT}
        ldummy := RowCount;
        BlockWrite(F, ldummy, sizeof(MLB_int4), r);
        bcount := bcount + 4;

        for i:=1 to FieldCount do begin
            {SAVES THE DATA TYPE}
            if DataType[i]='STRING' then begin
               bdummy := 0;
               BlockWrite(F, bdummy, sizeof(MLB_int1), r);
            end else if DataType[i]='FLOAT' then begin
               bdummy := 1;
               BlockWrite(F, bdummy, sizeof(MLB_int1), r);
            end else begin
               bdummy := 0;
               BlockWrite(F, bdummy, sizeof(MLB_int1), r);
            end;
            {SAVES FIELDNAMES}
            write_text_as_binary(F, FieldName[i]);
            bcount := bcount + 1 + 4 + length(FieldName[i]);
        end;
        for j:=1 to RowCount do begin
            {Saves the position of the row length}
            rcount := 0;
            row_offset := FilePos(F);
            ldummy := 0;
            BlockWrite(F, ldummy, sizeof(MLB_int4), r);
            for i:=1 to FieldCount do begin
                data := AccessData[i, j];
                write_text_as_binary(F, data);
                rcount := rcount + 4 + length(data);
            end;
            my_offset := FilePos(F);
            Seek(F, row_offset);
            BlockWrite(F, rcount, sizeof(MLB_int4), r);
            Seek(F, my_offset);
            bcount := bcount + 4 + rcount;
        end;
        my_offset := FilePos(F);
        Seek(F, table_offset);
        BlockWrite(F, bcount, sizeof(MLB_int4), r);
        Seek(F, my_offset);
        CloseFile(F);
        Result := True;
     end else begin
        MLBError := MLB_ERROR_IO;
        MLBErrorComment := '1-Unable to open the file for writing';
        Result := False;
     end;
end;

function TMlb2.same_endian(endian1: byte): boolean;
begin
     Result := local_endian = endian1;
end;

function TMlb2.SaveToCSVFile(filename1: string): Boolean;
var F: TextFile;
    i, j: LongInt;
begin
     init_error;
     AssignFile(F, filename1);
     try
        Rewrite(F);
        Result := True;
     except
        CloseFile(F);
        MLBError := MLB_ERROR_IO;
        MLBErrorComment := '1-Unable to open the file for writing';
        Result := False;
        Exit;
     end;
     for i:=0 to fields.Count-2 do begin
         Write(F, fields.Strings[i], CSVSeparator);
     end;
     if fields.Count>0 then begin
        WriteLn(F, fields.Strings[fields.Count-1]);
     end;
     for i:=0 to data.Count-1 do begin
         for j:=0 to fields.Count-2 do begin
             Write(F, psv.ToN(row(i).Strings[j]), CSVSeparator);
         end;
         if not IsEmpty then begin
            WriteLn(F, psv.ToN(row(i).Strings[fields.Count-1]));
         end;
     end;
     CloseFile(F);
end;

function TMlb2.AppendToCSVFile(filename1: string): Boolean;
var F: TextFile;
    i, j: LongInt;
begin
     init_error;
     AssignFile(F, filename1);
     try
        Append(F);
        Result := True;
     except
        CloseFile(F);
        MLBError := MLB_ERROR_IO;
        MLBErrorComment := '1-Unable to open the file for writing';
        Result := False;
        Exit;
     end;
     for i:=0 to data.Count-1 do begin
         for j:=0 to fields.Count-2 do begin
             Write(F, psv.ToN(row(i).Strings[j]), CSVSeparator);
         end;
         if not IsEmpty then begin
            WriteLn(F, psv.ToN(row(i).Strings[fields.Count-1]));
         end;
     end;
     CloseFile(F);
end;

function TMlb2.SaveToISAMFile(filename1: string): Boolean;
var F: TextFile;
    i, j: LongInt;
begin
     init_error;
     AssignFile(F, filename1);
     try
        Rewrite(F);
        Result := True;
     except
        CloseFile(F);
        MLBError := MLB_ERROR_IO;
        MLBErrorComment := '1-Unable to open the file for writing';
        Result := False;
        Exit;
     end;
     for i:=0 to fields.Count-2 do begin
         Write(F, QuoteSeparator, fields.Strings[i], QuoteSeparator, CSVSeparator);
     end;
     if fields.Count>0 then begin
        WriteLn(F, QuoteSeparator, fields.Strings[fields.Count-1], QuoteSeparator);
     end;
     for i:=0 to data.Count-1 do begin
         for j:=0 to fields.Count-2 do begin
             if ftypes.Strings[j] = 'STRING' then begin
                Write(F, QuoteSeparator, quote2(row(i).Strings[j]), QuoteSeparator, CSVSeparator);
             end else begin
                Write(F, row(i).Strings[j], CSVSeparator);
             end;
         end;
         if not IsEmpty then begin
             if ftypes.Strings[fields.Count-1] = 'STRING' then begin
                WriteLn(F, QuoteSeparator, quote2(row(i).Strings[fields.Count-1]), QuoteSeparator);
             end else begin
                WriteLn(F, row(i).Strings[fields.Count-1]);
             end;
         end;
     end;
     CloseFile(F);
end;

procedure TMlb2.ForceRows(nrows: LongInt);
var i, rw: LongInt;
begin
     rw := GetCurrentRow;
     if nrows<RowCount then begin
        for i:=1 to RowCount-nrows do begin
            GoLast;
            RemoveRow;
        end;
     end else if nrows>RowCount then begin
        for i:=1 to nrows-RowCount do begin
            GoLast;
            AddRow;
        end;
     end else begin
     end;
     Go(rw);
end;

function TMlb2.RowCount: LongInt;
begin
     Result := data.Count;
end;

function TMlb2.FieldCount: LongInt;
begin
     Result := fields.Count;
end;

function TMlb2.SavePosition: boolean;
begin
     if (current>=0) and (current<data.Count) then begin
        position := current;
        Result := True;
     end else begin
        Result := False;
     end;
end;

function TMlb2.RestorePosition: boolean;
begin
     if (position>=0) and (position<data.Count) then begin
        current := position;
        Result := True;
     end else begin
        Result := False;
     end;
end;

function TMlb2.RobustStrToFloat(s1: string): Extended;
var i: LongInt;
    anomalie, sortie, cas1, cas2: boolean;
    r: string;
begin
    r := '';
    for i:=1 to length(s1) do begin
         if s1[i] in ['.', ','] then s1[i] := DecimalSeparator;
         case s1[i] of
              '0'..'9', '.', ',', 'E', 'e', '+', '-': begin
                r := r + s1[i];
              end;
         end;
    end;
    {Avant E, [+, -]Chiffres[DC]Chiffres[E, e][+, -]Chiffres}
    {Le premier caractere doit etre [+, -, chiffre]}
    if length(r)>0 then begin
        if (r[1] in ['0'..'9', '+', '-']) then begin
           i := 2;
           anomalie := false;
           sortie := false;
           cas1 := false;
           cas2 := false;
           while not sortie and (i<=length(r)) do begin
                 if not (r[i] in ['0'..'9']) then begin
                    cas1 := r[i]=DecimalSeparator;
                    cas2 := UpperCase(r[i])='E';
                    anomalie := not (cas1 or cas2);
                    sortie := true;
                 end else begin
                 end;
                 Inc(i, 1);
           end;
           if cas1 then begin
              anomalie := false;
              sortie := false;
              while not sortie and (i<=length(r)) do begin
                    if not (r[i] in ['0'..'9']) then begin
                       anomalie := UpperCase(r[i])<>'E';
                       cas2 := True;
                       sortie := true;
                    end else begin
                    end;
                    Inc(i, 1);
              end;
           end;
           if cas2 then begin
               anomalie := anomalie or not (r[i] in ['+', '-', '0'..'9']);
               Inc(i, 1);
               sortie := false;
               while not sortie and (i<=length(r)) do begin
                    if not (r[i] in ['0'..'9']) then begin
                       anomalie := True;
                       sortie := true;
                    end else begin
                    end;
                    Inc(i, 1);
               end;
           end;
           if anomalie then begin
              Result := 0.0;
           end else begin
              Result := StrToFloat(r);
           end;
        end else begin
            Result := 0.0;
        end;
    end else begin
        Result := 0.0;
    end;
end;

function TMlb2.RobustFloatToStr(v1: Extended): string;
begin
     Result := FloatToStr(v1);
end;

function TMlb2.BeginSeek(direction1: boolean): Boolean;
begin
     direction := direction1;
     If Not IsEmpty then begin
        firstseek := True;
        Result := True;
     end else begin
        Result := False;
     end;
end;

function TMlb2.EndSeek: Boolean;
begin
     If Not IsEmpty then begin
         If (current<0) or (current>=data.Count) then begin
            GoLast;
         end else begin
         end;
         firstseek := False;
         Result := True;
     end else begin
         Result := False;
     end;
end;

function TMlb2.MatchData(fieldname1, comp1, value1: string): boolean;
var trouve: boolean;
begin
      trouve := False;
      if (comp1 = '<') then begin
         trouve := GetData(fieldname1)<value1;
      end else if (comp1 = '>') then begin
         trouve := GetData(fieldname1)>value1;
      end else if (comp1 = '=') then begin
         trouve := GetData(fieldname1)=value1;
      end else if (comp1 = '<=') then begin
         trouve := GetData(fieldname1)<=value1;
      end else if (comp1 = '>=') then begin
         trouve := GetData(fieldname1)>=value1;
      end else if (UpperCase(comp1) = 'LIKE') then begin
         trouve := LikeAgent.SI_VERIFICATION(value1, GetData(fieldname1));
      end else begin
      end;
      Result := trouve;
end;

function TMlb2.MatchFloat(fieldname1, comp1: string; value1: Extended): boolean;
var trouve: boolean;
begin
      trouve := False;
      if (comp1 = '<') then begin
         trouve := GetFloat(fieldname1)<value1;
      end else if (comp1 = '>') then begin
         trouve := GetFloat(fieldname1)>value1;
      end else if (comp1 = '=') then begin
         trouve := GetFloat(fieldname1)=value1;
      end else if (comp1 = '<=') then begin
         trouve := GetFloat(fieldname1)<=value1;
      end else if (comp1 = '>=') then begin
         trouve := GetFloat(fieldname1)>=value1;
      end else begin
      end;
      Result := trouve;
end;

function TMlb2.SeekData(fieldname1, comp1, value1: string): boolean;
var trouve: boolean;
    sens: integer;
    rw,k: LongInt;
begin
     if direction = MLB_FORWARD then begin
        sens := 1;
     end else if direction = MLB_BACKWARD then begin
        sens := -1;
     end else begin
        sens := 1;
     end;
     trouve := False;
     if IsEmpty then begin
        Result := False;
     end else begin
        k := fields.IndexOf(fieldname1)+1;
        rw := GetCurrentRow;
        if (current<0) then begin
           current := 0;
        end else if (current>=0) then begin
           if firstseek then begin
           end else begin
              current := current + sens*1;
           end;
        end;
        while (not trouve) and (((direction=MLB_FORWARD) and (current<data.Count))
              or ((direction=MLB_BACKWARD) and (current>0))) do begin
              if (comp1 = '<') then begin
                 trouve := GetDatabyindex(k)<value1;
              end else if (comp1 = '>') then begin
                 trouve := GetDatabyindex(k)>value1;
              end else if (comp1 = '=') then begin
                 trouve := GetDatabyindex(k)=value1;
              end else if (comp1 = '<=') then begin
                 trouve := GetDatabyindex(k)<=value1;
              end else if (comp1 = '>=') then begin
                 trouve := GetDatabyindex(k)>=value1;
              end else if (UpperCase(comp1) = 'LIKE') then begin
                 trouve := LikeAgent.SI_VERIFICATION(value1, GetDatabyindex(k));
              end else begin
              end;
              if not trouve then current := current + sens*1;
        end;
        firstseek := False;
        if not trouve then begin
           Go(rw);
           Result := False;
        end else begin
           Result := True;
        end;
     end;
end;

function TMlb2.SeekFloat(fieldname1, comp1: string; value1: Extended): boolean;
var trouve: boolean;
    sens: integer;
    rw,k: LongInt;
begin
     if direction = MLB_FORWARD then begin
        sens := 1;
     end else if direction = MLB_BACKWARD then begin
        sens := -1;
     end else begin
        sens := 1;
     end;
     trouve := False;
     if IsEmpty then begin
        Result := False;
     end else begin
        k := fields.IndexOf(fieldname1)+1;
        rw := GetCurrentRow;
        if (current<0) then begin
           current := 0;
        end else if (current>=0) then begin
           if firstseek then begin
           end else begin
              current := current + sens*1;
           end;
        end;
        while (not trouve) and (((direction=MLB_FORWARD) and (current<data.Count))
              or ((direction=MLB_BACKWARD) and (current>0))) do begin
              if (comp1 = '<') then begin
                 trouve := GetFloatbyindex(k)<value1;
              end else if (comp1 = '>') then begin
                 trouve := GetFloatbyindex(k)>value1;
              end else if (comp1 = '=') then begin
                 trouve := GetFloatbyindex(k)=value1;
              end else if (comp1 = '<=') then begin
                 trouve := GetFloatbyindex(k)<=value1;
              end else if (comp1 = '>=') then begin
                 trouve := GetFloatbyindex(k)>=value1;
              end else begin
              end;
              if not trouve then current := current + sens*1;
        end;
        firstseek := False;
        if not trouve then begin
           Go(rw);
           Result := False;
        end else begin
           Result := True;
        end;
     end;
end;

function TMlb2.GetPosition: LongInt;
begin
     Result := current + 1;
end;


function TMlb2.RangeSortByData(fieldname1: string; lowest2greatest1: boolean; from1, to1: LongInt): boolean;
    procedure QSortByData(fieldname1: string; lowest2greatest1: boolean; l, r: LongInt);
    var i, j: LongInt;
        v: string;
    begin
         if lowest2greatest1 then begin
              v := TMlb2_ROW(data.Items[(l + r) div 2]).Strings[fields.IndexOf(fieldname1)];
              i := l; j := r;
              repeat
                while TMlb2_ROW(data.Items[i]).Strings[fields.IndexOf(fieldname1)]<v do Inc(i);
                while v < TMlb2_ROW(data.Items[j]).Strings[fields.IndexOf(fieldname1)] do Dec(j);
                if i <= j then begin
                  data.Exchange(i, j);
                  inc(i); dec(j);
                end;
              until i > j;
         end else begin
              v := TMlb2_ROW(data.Items[(l + r) div 2]).Strings[fields.IndexOf(fieldname1)];
              i := l; j := r;
              repeat
                while TMlb2_ROW(data.Items[i]).Strings[fields.IndexOf(fieldname1)]>v do Inc(i);
                while v > TMlb2_ROW(data.Items[j]).Strings[fields.IndexOf(fieldname1)] do Dec(j);
                if i <= j then begin
                  data.Exchange(i, j);
                  inc(i); dec(j);
                end;
              until i > j;
         end;
         if l < j then QSortByData(fieldname1, lowest2greatest1, l, j);
         if i < r then QSortByData(fieldname1, lowest2greatest1, i, r);

         Result := True;
    end;
begin
     if (from1>=1) and (from1<=RowCount) and (to1>from1) and (to1<=RowCount) then begin
        QSortByData(fieldname1, lowest2greatest1, from1-1, to1-1);
        Result := True;
     end else begin
        Result := False;
     end;
end;

function TMlb2.SortByData(fieldname1: string; lowest2greatest1: boolean): boolean;
begin
     Result := RangeSortByData(fieldname1, lowest2greatest1, 1, RowCount);
end;

function TMlb2.RangeSortByFloat(fieldname1: string; lowest2greatest1: boolean; from1, to1: LongInt): boolean;
    procedure QSortByFloat(fieldname1: string; lowest2greatest1: boolean; l, r: LongInt);
    var i, j: LongInt;
        v: Extended;
    begin
         if lowest2greatest1 then begin
              v := RobustStrToFloat(TMlb2_ROW(data.Items[(l + r) div 2]).Strings[fields.IndexOf(fieldname1)]);
              i := l; j := r;
              repeat
                while RobustStrToFloat(TMlb2_ROW(data.Items[i]).Strings[fields.IndexOf(fieldname1)])<v do Inc(i);
                while v < RobustStrToFloat(TMlb2_ROW(data.Items[j]).Strings[fields.IndexOf(fieldname1)]) do Dec(j);
                if i <= j then begin
                  data.Exchange(i, j);
                  inc(i); dec(j);
                end;
              until i > j;
         end else begin
              v := RobustStrToFloat(TMlb2_ROW(data.Items[(l + r) div 2]).Strings[fields.IndexOf(fieldname1)]);
              i := l; j := r;
              repeat
                while RobustStrToFloat(TMlb2_ROW(data.Items[i]).Strings[fields.IndexOf(fieldname1)])>v do Inc(i);
                while v > RobustStrToFloat(TMlb2_ROW(data.Items[j]).Strings[fields.IndexOf(fieldname1)]) do Dec(j);
                if i <= j then begin
                  data.Exchange(i, j);
                  inc(i); dec(j);
                end;
              until i > j;
         end;
         if l < j then QSortByFloat(fieldname1, lowest2greatest1, l, j);
         if i < r then QSortByFloat(fieldname1, lowest2greatest1, i, r);

         Result := True;
    end;
begin
     if (from1>=1) and (from1<=RowCount) and (to1>from1) and (to1<=RowCount) then begin
        QSortByFloat(fieldname1, lowest2greatest1, from1-1, to1-1);
        Result := True;
     end else begin
        Result := False;
     end;
end;

function TMlb2.SortByFloat(fieldname1: string; lowest2greatest1: boolean): boolean;
begin
     Result := RangeSortByFloat(fieldname1, lowest2greatest1, 1, RowCount);
end;

procedure TMlb2.RandomSort;
var i: integer;
    v, r, reste: integer;
    first, l, tmp: PTMlb2IntegerList;
    first1, l1: PTMlb2IntegerList;
begin
     new(first1); l1 := first1;
     l1^.nextfield := nil;
     new(first); l := first;
     for i:=1 to RowCount do begin
         new(l^.nextfield);
         l := l^.nextfield;
         l^.k := i;
     end;
     l^.nextfield := nil;
     reste := RowCount;
     Randomize;
     while reste>0 do begin
           r := Random(reste)+1;
           l := first;
           for i:=1 to r-1 do begin
               l := l^.nextfield;
           end;
           l1^.nextfield := l^.nextfield;
           l1 := l1^.nextfield;
           l^.nextfield := l1^.nextfield;
           Dec(reste, 1);
     end;
     l1^.nextfield := nil;
     dispose(first);
     l := first1;
     v := 0;
     while l^.nextfield <> nil do begin
           l := l^.nextfield;
           Go(l^.k + v);
           CopyRow;
           Go(1);
           InsertRow(MLB_BEFORE);
           Go(1);
           PasteRow;
           Inc(v, 1);
     end;
     for i:=1 to v do begin
         Go(v+1);
         RemoveRow;
     end;
     l := first1;
     for i:=1 to RowCount+1 do begin
         tmp := l^.nextfield;
         dispose(l);
         l := tmp;
     end;
end;

function TMlb2.CopyRowBySlot(slot: integer): boolean;
begin
     If (Not IsEmpty) and (slot>=1) and (slot<=2) then begin
        if current>=0 then begin
           rowcopy[slot].Assign(currentrow);
           Result := True;
        end else begin
           Result := False;
        end;
     end else begin
        Result := False;
     end;
end;

function TMlb2.CopyRow: boolean;
begin
     Result := CopyRowBySlot(1);
end;

function TMlb2.PasteRowBySlot(slot: integer): boolean;
begin
     If (Not IsEmpty) and (slot>=1) and (slot<=2) then begin
        if current>=0 then begin
           currentrow.Assign(rowcopy[slot]);
           Result := True;
        end else begin
           Result := False;
        end;
     end else begin
        Result := False;
     end;
end;

function TMlb2.PasteRow: boolean;
begin
     Result := PasteRowBySlot(1);
end;

function TMlb2.Fusion(var dest_mlb, source_mlb: TMlb2; a1: TMlbFusionArray): boolean;
var f: string;
    i, j, k: integer;
    bexclu: boolean;
    found, exit_while: boolean;
begin
     dest_mlb := TMlb2.Create;
     psv.Init(a1);
     psv.CSVSeparator := CSVSeparator;
     psv.NextField(f);
     if (f<>'') then begin
        if f='COMMON' then begin
           bexclu := true;
        end else if f='ALL' then begin
           bexclu := false;
        end else begin
           psv.Init(a1);
           bexclu := true;
        end;
        {FIELDS CREATION}
        k := 0;
        exit_while := false;
        while (not exit_while) and ((psv.NextField(f)) or (f<>'')) do begin
              if f='*' then begin
                 if bexclu then begin
                    {ALL COMMON FIELDS MUST BE COPIED}
                    for i:=1 to FieldCount do begin
                        j := 1;
                        found := false;
                        while (not found) and (j<=source_mlb.FieldCount) do begin
                              found := FieldName[i]=source_mlb.FieldName[j];
                              Inc(j, 1);
                        end;
                        if found then begin
                           dest_mlb.AddField(FieldName[i]);
                           Inc(k, 1);
                           dest_mlb.DataType[k] := DataType[i];
                        end else begin
                        end;
                    end;
                 end else begin
                    {ALL FIELDS OF THE 2 TABLES MUST BE COPIED}
                    for i:=1 to source_mlb.FieldCount do begin
                           dest_mlb.AddField(source_mlb.FieldName[i]);
                           Inc(k, 1);
                           dest_mlb.DataType[k] := source_mlb.DataType[i];
                    end;
                    for i:=1 to FieldCount do begin
                        if dest_mlb.GetFieldIndex(FieldName[i])<=0 then begin
                           dest_mlb.AddField(FieldName[i]);
                           Inc(k, 1);
                           dest_mlb.DataType[k] := DataType[i];
                        end else begin
                        end;
                    end;
                 end;
                 exit_while := true;
              end else begin
                 {PARSED FIELDS MUST BE COPIED}
                 if dest_mlb.GetFieldIndex(f)<=0 then begin
                    Inc(k, 1);
                    AddField(f);
                    i := GetFieldIndex(f);
                    if i>0 then begin
                          dest_mlb.DataType[k] := DataType[i];
                    end else begin
                          i := source_mlb.GetFieldIndex(f);
                          if i>0 then begin
                             dest_mlb.DataType[k] := source_mlb.DataType[i];
                          end else begin
                          end;
                    end;
                 end else begin
                 end;
              end;
        end;
        {ROWS CREATION}
        for i:=1 to RowCount+source_mlb.RowCount do begin
            dest_mlb.AddRow;
        end;
        dest_mlb.GoFirst;
        for i:=1 to RowCount do begin
            Go(i);
            for j:=1 to dest_mlb.FieldCount do begin
                dest_mlb.SetData(dest_mlb.FieldName[j], GetData(dest_mlb.FieldName[j]));
            end;
            dest_mlb.GoNext;
        end;
        for i:=1 to source_mlb.RowCount do begin
            source_mlb.Go(i);
            for j:=1 to dest_mlb.FieldCount do begin
                dest_mlb.SetData(dest_mlb.FieldName[j], source_mlb.GetData(dest_mlb.FieldName[j]));
            end;
            dest_mlb.GoNext;
        end;
        Result := true;
     end else begin
        Result := false;
     end;
end;

constructor TMlb2ParseCSV.Create;
begin
     inherited Create;
     CSVSeparator := ';';
end;

destructor TMlb2ParseCSV.Destroy;
begin
     inherited Destroy;
end;

procedure TMlb2ParseCSV.Init(s1: string);
begin
     index := 1;
     csvline := s1;
end;

function TMlb2ParseCSV.NextField(var field: string): Boolean;
var read_something: boolean;
    separation: boolean;
begin
     read_something := False;
     separation := false;
     field := '';
     while (index<=length(csvline)) and not separation do begin
           if (csvline[index]=CSVSeparator) then begin
              case index of
                   1: begin
                      separation := true;
                   end;
                   2: begin
                      separation := csvline[index-1]<>'\';
                   end;
                   else begin
                      separation := (csvline[index-1]<>'\') or ((csvline[index-1]='\') and (csvline[index-2]<>'\'));
                   end;
              end;
           end else begin
           end;
           if not separation then begin
              field := field + csvline[index];
              read_something := True;
           end else begin
           end;
           Inc(index, 1);
     end;
{(csvline[index]<>CSVSeparator)}
{Inc(index, 1);}
     Result := read_something or (index<=length(csvline));
end;

function TMlb2ParseCSV.FromN(s1: string): string;
var i: LongInt;
    token: string;
begin
     token := '';
     i := 1;
     while i<=length(s1) do begin
         if s1[i] = '\' then begin
            if (i<length(s1)) then begin
               if (s1[i+1] = 'n') then begin
                  Inc(i, 1);
                  token := token + #13 + #10;
               end else if (s1[i+1] = CSVSeparator) then begin
                  Inc(i, 1);
                  token := token + CSVSeparator;
               end else if (s1[i+1] = '\') then begin
                  Inc(i, 1);
                  token := token + '\';
               end else begin
                  token := token + s1[i];
               end;
            end else begin
               token := token + s1[i];
            end;
         end else begin
             token := token + s1[i];
         end;
         Inc(i, 1);
     end;
     Result := token;
end;

function TMlb2ParseCSV.ToN(s1: string): string;
var i: LongInt;
    token: string;
begin
     token := '';
     for i:=1 to length(s1) do begin
         if ord(s1[i])=13 then begin
            token := token + '\n';
         end else if ord(s1[i])=10 then begin
         end else if s1[i] = '\' then begin
            token := token + '\\';
         end else if s1[i] = CSVSeparator then begin
             token := token + '\' + CSVSeparator;
         end else begin
             token := token + s1[i];
         end;
     end;
     Result := token;
end;

function TMlb2.InitFieldWithData(fieldname1: string; data1: string): boolean;
var wr: LongInt;
begin
     wr := GetCurrentRow;
     if GoFirst and (GetFieldIndex(fieldname1)>0) then begin
        repeat
              SetData(fieldname1, data1);
        until not GoNext;
        Result := True;
     end else begin
        Result := False;
     end;
     Go(wr);
end;

function TMlb2.InitFieldWithValue(fieldname1: string; value1: Extended): boolean;
var wr: LongInt;
begin
     wr := GetCurrentRow;
     if GoFirst and (GetFieldIndex(fieldname1)>0) then begin
        repeat
              SetFloat(fieldname1, value1);
        until not GoNext;
        Result := True;
     end else begin
        Result := False;
     end;
     Go(wr);
end;

{---------TKLIST DEBUT IMPLEMENTATION--------}
constructor TKBaseList.Create;
begin
     list := TKList.Create;
     list.Init(false);
end;

destructor TKBaseList.Destroy;
begin
     list.Free;
end;

function TKBaseList.ReadCount: LongInt;
begin
     Result := list.Count;
end;

function TKBaseList.ReadItems(index1: LongInt): pointer;
begin
     Result := list.GetItem(index1+1);
end;

procedure TKBaseList.WriteItems(index1: LongInt; v: pointer);
begin
     list.SetItem(index1+1, v);
end;

procedure TKBaseList.Clear;
begin
     list.Init(false);
end;

procedure TKBaseList.Pack;
begin
end;

procedure TKBaseList.Add(p: pointer);
begin
     list.Add(p);
end;

procedure TKBaseList.Insert(position1: LongInt; p: pointer);
begin
     list.Insert(position1+1, p);
end;

procedure TKBaseList.Delete(k: LongInt);
begin
     list.Remove(k+1);
end;

procedure TKBaseList.Exchange(k1, k2: LongInt);
begin
     list.Exchange(k1+1, k2+1);
end;


constructor TKStringList.Create;
begin
     list := TKList.Create;
     list.Init(false);
end;

destructor TKStringList.Destroy;
begin
     list.Free;
end;

function TKStringList.ReadCount: LongInt;
begin
     Result := list.Count;
end;

function TKStringList.ReadStrings(index1: LongInt): string;
begin
     Result := list.GetString(index1+1);
end;

procedure TKStringList.WriteStrings(index1: LongInt; v: string);
begin
     list.SetString(index1+1, v);
end;

procedure TKStringList.Clear;
begin
     list.Init(false);
end;

function TKStringList.IndexOf(s: string): LongInt;
var k: integer;
begin
     Result := list.IndexOfString(s)-1;
end;

procedure TKStringList.Add(s: string);
begin
     list.AddString(s);
end;

function TKStringList.Delete(k: LongInt): boolean;
begin
     Result := list.Remove(k+1)<>nil;
end;

procedure TKStringList.Assign(tk: TKStringList);
var i: integer;
begin
     for i:=0 to Count-1 do begin
         Strings[i] := tk.Strings[i];
     end;
end;


function TKList.best_pointer(k: LongInt): LongInt;
begin
     if (k>0) and (k<=n) then begin
        if abs(k-1)<abs(k-index) then begin
           if abs(k-n)<abs(k-1) then begin
              {LAST est le meilleur}
              Result := 3;
           end else begin
              {FIRST est le meilleur}
              Result := 1;
           end;
        end else begin
           if abs(k-n)<abs(k-index) then begin
              {LAST est le meilleur}
              Result := 3;
           end else begin
              {CURRENT est le meilleur}
              Result := 2;
           end;
        end;
     end else begin
        Result := 0;
     end;
end;

constructor TKList.Create;
begin
     inherited Create;
     new(first);
     first^.Next := nil;
     first^.item := nil;
     first^.Prev := nil;
     current := first;
     last := first;

     index := 0;
     n := 0;
end;

destructor TKList.Destroy;
begin
//pch
//     Delete(false);
     Delete(true);
     inherited Destroy;
end;

procedure TKList.Purge;
begin
     Delete(true);
end;

function TKList.GetIndex: LongInt;
begin
     Result := index;
end;

function TKList.Count: LongInt;
begin
     Result := n;
end;

procedure TKList.AddString(s: string);
var mystring: PString;
begin
     new(mystring);
     mystring^ := s;
     Add(mystring);
end;

function TKList.InsertString(k: LongInt; s: string): boolean;
var mystring: PString;
begin
     new(mystring);
     mystring^ := s;
     Insert(k, mystring);
end;

function TKList.GetString(k: LongInt): string;
var p: pointer;
begin
     p := GetItem(k);
     if p=nil then begin
        Result := '';
     end else begin
        Result := String(p^);
     end;
end;

function TKList.SetString(k: LongInt; s: string): boolean;
var p: pointer;
    mystring: PString;
begin
     p := GetItem(k);
     if p=nil then begin
     end else begin
        dispose(p);
     end;
     if (k>0) and (k<=n) then begin
        new(mystring);
        mystring^ := s;
        SetItem(k, mystring);
        Result := True;
     end else begin
        Result := False;
     end;
end;

function TKList.IndexOfString(s: string): LongInt;
var i: LongInt;
    trouve: boolean;
begin
     trouve := False;
     i:=1;
     while (not trouve) and (i<=Count) do begin
           trouve := s=GetString(i);
           Inc(i, 1);
     end;
     if trouve then begin
        Result := i-1;
     end else begin
        Result := 0;
     end;
end;

procedure TKList.Add(item: pointer);
begin
     new(last^.Next);
     Inc(n, 1);
     last^.Next^.Prev := last;
     last := last^.Next;
     last^.item := item;
     last^.Next := nil;
     if index<=0 then GoLast;
end;

function TKList.Insert(k: LongInt; item: pointer): boolean;
var i, necessary: LongInt;
    tmp: PListItem;
begin
     if Go(k) then begin
        new(tmp);
        tmp^.Next := current;
        tmp^.Prev := current^.Prev;
        tmp^.item := item;
        current^.Prev^.Next := tmp;
        current^.Prev := tmp;
        current := tmp;
        Inc(n, 1);
        Result := True;
     end else begin
        necessary := k-n;
        if necessary>0 then begin
           for i:=1 to necessary-1 do begin
               Add(nil);
           end;
           Add(item);
           Result := True;
        end else begin
           Result := False;
        end;
     end;
end;

function TKList.Remove(k: LongInt): pointer;
var tmp: PListItem;
begin
     if Go(k) then begin
        Dec(index, 1); Dec(n, 1);
        tmp := current;
        current^.Prev^.Next := current^.Next;
        if current^.Next=nil then begin
            last := current^.Prev;
        end else begin
            current^.Next^.Prev := current^.Prev;
        end;
        current := current^.Prev;
        Result := tmp^.item;
        dispose(tmp);
     end else begin
        Result := nil;
     end;
end;

function TKList.Delete(ditems: boolean): boolean;
begin
     Init(ditems);
     dispose(first);
end;

function TKList.Init(ditems: boolean): boolean;
var tmp: PListItem;
    i: LongInt;
begin
     Result := Empty;
     current := first^.Next;
     index := 1;
     for i:=1 to n do begin
         tmp := current;
         current := current^.Next;
         if ditems then dispose(tmp^.item);
         dispose(tmp);
         Inc(index, 1);
     end;
     current := first; last := first; index := 0; n := 0;
end;

function TKList.GetItem(k: LongInt): pointer;
begin
     if Go(k) then begin
         Result := current^.item;
     end else begin
         Result := nil;
     end;
end;

function TKList.SetItem(k: LongInt; p: pointer): pointer;
begin
     if Go(k) then begin
         Result := current^.item;
         current^.item := p;
     end else begin
         Result := nil;
     end;
end;

function TKList.Exchange(k1, k2: LongInt): boolean;
var p: pointer;
begin
     if (k1>0) and (k1<=Count) and (k2>0) and (k2<=Count) then begin
        {prendre l'item de k2 dans un pointeur}
        p := GetItem(k2);
        {copier l'item de k1 dans k2}
        SetItem(k2, GetItem(k1));
        {copier le pointeur dans k1}
        SetItem(k1, p);
        Result := True;
     end else begin
        Result := False;
     end;
end;

function TKList.Empty: boolean;
begin
     Result := first=last;
end;

function TKList.Go(k: LongInt): boolean;
var i: LongInt;
begin
     Result := true;
     case best_pointer(k) of
          0: begin
             Result := false;
          end;
          1: begin
             current := first; index := 0;
             for i:=1 to k do begin
                 current := current^.Next;
             end; index := k;
          end;
          2: begin
             if k-index>0 then begin
                for i:=index to k-1 do begin
                    current := current^.Next;
                end; index := k;
             end else begin
                for i:=1 to index-k do begin
                    current := current^.Prev;
                end; index := k;
             end;
          end;
          3: begin
             current := last; index := n;
             for i:=1 to n-k do begin
                 current := current^.Prev;
             end; index := k;
          end;
     end;
end;

function TKList.GoFirst: boolean;
begin
     if Empty then begin
         index := 0;
         n := 0;
         current := first;
     end else begin
         index := 1;
         current := first^.Next;
     end;
     Result := index>0;
end;

function TKList.GoLast: boolean;
begin
     if Empty then begin
         index := 0;
         n := 0;
         current := first;
     end else begin
         index := n;
         current := last;
     end;
     Result := index>0;
end;

function TKList.GoNext: boolean;
begin
     if index<n then begin
        Inc(index, 1);
        current := current^.Next;
        Result := true;
     end else begin
        Result := false;
     end;
end;

function TKList.GoPrevious: boolean;
begin
     if index>1 then begin
        Dec(index, 1);
        current := current^.Prev;
        Result := true;
     end else begin
        Result := false;
     end;
end;
{---------TKLIST FIN IMPLEMENTATION--------}

{---------EXCEL DEBUT IMPLEMENTATION--------}
Constructor TBaseSave.Init;
begin
  MinSaveRecs := 0; MaxSaveRecs := 100;
  MinSaveCols := 0; MaxSaveCols := 100;
  EndOfLine := false;
end;

Procedure TBaseSave.WriteBlank;
begin
  write( CharFile, separator );
end;

Procedure TBaseSave.WriteLongInt;
var ALongIntP : ^LongInt; ALongInt : LongInt;
begin
  ALongIntP := DataPointer; ALongInt := ALongIntP^;
  str(ALongInt, DataString );
end;

Procedure TBaseSave.WriteDouble;
var ADoubleP : ^double; ADouble : double;
begin
  ADoubleP := DataPointer; ADouble := ADoubleP^;
  str(ADouble, DataString );
end;

Procedure TBaseSave.WriteLabel;
var ALabelP : ^CharType; ALabel : CharType;
begin
  ALabelP := DataPointer; ALabel := ALabelP^;
  DataString  := String( ALabel );
  w := length(DataString); {unused by calling method}
end;

Procedure TBaseSave.WriteData;
var i : LongInt; AWordLength : word;
begin
  CellType := AType;
  if Row <> -1 then if Row <> ARow then EndOfLine := true else EndOfLine := false;
  Row := ARow;
  Col := ACol;
  DataPointer := AData;

  case CellType of
    CellBlank   : WriteBlank;
    CellLongInt : WriteLongInt;
    CellDouble  : WriteDouble;
    CellLabel   : WriteLabel(AWordLength);
    CellBoolean : exit; {No boolean types in text files}
    else exit;
  end;
  
  if EndOfLine then begin write ( CharFile, TMlb2_CR ); write ( CharFile, TMlb2_LF ) end;
  for i := 1 to length(DataString) do write( CharFile, DataString[i] );
  write( CharFile, separator );
  
end;

Destructor TBaseSave.Done;
begin
end;

{ASCII files object}

Constructor TASCII.Init;
begin
  TBaseSave.Init( SaveFileName );
  Separator := TMlb2_Space;
  assign( CharFile, SaveFileName );
  Row := -1; col := -1;
  rewrite ( CharFile );
end;

Destructor TASCII.Done;
begin
  TBaseSave.Done; close( CharFile );
end;

{Excel tab-delimited files object}

Constructor TExcelTab.Init;
begin
  TBaseSave.Init( SaveFileName );
  Separator := TMlb2_tab;
  assign( CharFile, SaveFileName );
  Row := -1; col := -1;
  rewrite ( CharFile ); 
end;

Destructor TExcelTab.Done;
begin
  TBaseSave.Done; close( CharFile );
end;

{Excel BIFF2 object}

Constructor TBIFF2.Init;
begin
  TBaseSave.Init( AFileName );
  Assign( ExcelFile, AFileName); Rewrite( ExcelFile, 1 );
  WriteBOF;
  WriteDimensions;
end;

Destructor TBIFF2.Done;
begin
  TBaseSave.Done;
  WriteEOF;
  Close (ExcelFile);
end; 

procedure TBIFF2.BIFFBOF;
begin
  typerec := TMlb2_BOF;
  lendata := 4;
end;

procedure TBIFF2.BIFFDIM;
begin
  typerec := DIMENSIONS;
  lendata := 8;
end;

procedure TBIFF2.WriteBOF;
var awBuf : array[0..2] of word;
begin
  awBuf[0] := 0;
  awBuf[1] := DOCTYPE_XLS;
  awBuf[2] := 0;
  BIFFBOF;
  WriteRecordHeader; 
  Blockwrite(Excelfile, awbuf, lendata);
end;

procedure TBIFF2.WriteRecordHeader;
var awBuf : array[0..1] of word;
begin
  awBuf[0] := typerec;
  awBuf[1] := lendata;
  Blockwrite(Excelfile, awbuf, LEN_RECORDHEADER);
end;

procedure TBIFF2.WriteDimensions;
var awBuf : array[0..4] of word;
begin
  awBuf[0] := MinSaveRecs;
  awBuf[1] := MaxSaveRecs;
  awBuf[2] := MinSaveCols;
  awBuf[3] := MaxSaveCols;
  awBuf[4] := 0;
  BIFFDIM;
  WriteRecordHeader;
  Blockwrite(Excelfile, awbuf, lendata);
end;

procedure TBIFF2.WriteEOF;
begin
  typerec := BIFF_EOF;
  lendata := 0;
  WriteRecordHeader;
end;

Procedure TBIFF2.WriteBlank;
begin
  typerec := 1;
  lendata := 7;
  WriteRecordHeader;
  lendata := 0;
end;

Procedure TBIFF2.WriteLongInt;
begin
  typerec := 2;
  lendata := 9;
  WriteRecordHeader;
  lendata := 2;
end;

Procedure TBIFF2.WriteDouble;
begin
  typerec := 3;
  lendata := 15;
  WriteRecordHeader;
  lendata := 8;
end;

Procedure TBIFF2.WriteLabel(var w : word);
var p: PChar;
begin
  p := PChar(DataPointer);
  w := length(StrPas(p));
  typerec := 4;
  lendata := 8+w;
  WriteRecordHeader;
  lendata := w;
end;

Procedure TBIFF2.WriteBoolean;
begin
  typerec := 5;
  lendata := 9;
  WriteRecordHeader;
  lendata := 0;
end;

Procedure TBIFF2.WriteData;
const
  Attribute: Array[0..2] Of Byte = (0, 0, 0); { 24 bit bitfield }
var
  awBuf : array[0..1] of word;
  AWordLength : word; ABoolByte : byte;
begin
  CellType := AType;
  Row := ARow;
  Col := ACol;
  DataPointer := AData;

  case CellType of
    CellBlank   : WriteBlank;
    CellLongInt : WriteLongInt;
    CellDouble  : WriteDouble;
    CellLabel   : WriteLabel(AWordLength);
    CellBoolean : WriteBoolean; { or error }
    else exit;
  end;
  awBuf[0] := Row;
  awBuf[1] := Col;
  Blockwrite(Excelfile, awbuf, sizeof(awBuf));
  BlockWrite(Excelfile, Attribute, SizeOf(Attribute));
  
  if CellType = CellLabel then begin
    ABoolByte := AWordLength;
    BlockWrite(Excelfile, ABoolByte, SizeOf(ABoolByte))
  end else if CellType = CellBoolean then begin
    if byte(DataPointer^) <> 0 then ABoolByte := 1 else ABoolByte := 0;
    BlockWrite(Excelfile, ABoolByte, SizeOf(ABoolByte));
    ABoolByte := 0;
    BlockWrite(Excelfile, ABoolByte, SizeOf(ABoolByte));
  end;
  if lendata <> 0 then BlockWrite(Excelfile, DataPointer^, lendata);
end;

{Excel BIFF3 object}

procedure TBIFF3.BIFFBOF;
begin
  typerec := BOF_BIFF3;
  lendata := 6;
end;

procedure TBIFF3.BIFFDIM;
begin
  typerec := DIMENSIONS_BIFF3;
  lendata := 10;
end;

{Excel BIFF4 object}

procedure TBIFF4.BIFFBOF;
begin
  typerec := BOF_BIFF4;
  lendata := 6;
end;

{Excel BIFF5 object}

procedure TBIFF5.BIFFBOF;
begin
  typerec := BOF_BIFF5;
  lendata := 6;
end;

function TMlb2.SaveToExcelFile(FileName1: string): boolean;
var i, j: LongInt;
    ALabel: PChar;
    Q: PChar;
    k: string;
begin
  PSavefile := New(PBIFF5,Init(FileName1));
  with PSaveFile^ do begin
    for i := 1 to fields.Count do begin
          ALabel := AllocMem(length(fields.Strings[i-1]) + 1);
          k := fields.Strings[i-1] + #0;
          Q := @(k[1]);
          StrCopy(PChar(ALabel), PChar(Q));
          PSaveFile^.WriteData(CellLabel, 0, i-1, ALabel);
          FreeMem(ALabel, length(fields.Strings[i-1]) + 1);
    end;
    for i := 1 to fields.Count do begin
      for j := 1 to data.Count do begin
          ALabel := AllocMem(length(TMlb2_ROW(data.Items[j-1]).Strings[i-1]) + 1);
          k := TMlb2_ROW(data.Items[j-1]).Strings[i-1] + #0;
          Q := @(k[1]);
          StrCopy(PChar(ALabel), PChar(Q));
          PSaveFile^.WriteData(CellLabel, j, i-1, ALabel);
          FreeMem(ALabel, length(TMlb2_ROW(data.Items[j-1]).Strings[i-1]) + 1);
      end;
    end;
  end;
  dispose(PSaveFile,done);
  Result := True;
end;
{------EXCEL FIN IMPLEMENTATION------}

end.

