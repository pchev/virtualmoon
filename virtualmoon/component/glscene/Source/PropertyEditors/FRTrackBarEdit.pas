{: FRTrackBarEdit<p>

   Frame combining a TrackBar and an Edit.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>05/10/08 - DanB - Removed Kylix support   
      <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
      <li>19/12/06 - DaStr - Fixed bug in SetValue, SetValueMin, SetValueMax when
                             changing these values didn't change the Edit's Text
      <li>03/07/04 - LR  - Make change for Linux
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit FRTrackBarEdit;

interface

{$i GLScene.inc}

uses
{$IFDEF GLS_DELPHI_XE2_UP}
  VCL.Forms, VCL.StdCtrls, VCL.ComCtrls, VCL.Controls,
{$ELSE}
  Forms, StdCtrls, ComCtrls, Controls,
{$ENDIF}
  Classes;

type
  TRTrackBarEdit = class(TFrame)
    TrackBar: TTrackBar;
    Edit: TEdit;
    procedure TrackBarChange(Sender: TObject);
    procedure EditChange(Sender: TObject);
  private
    { Déclarations privées }
    procedure SetValue(const val : Integer);
    function GetValue : Integer;
    procedure SetValueMin(const val : Integer);
    function GetValueMin : Integer;
    procedure SetValueMax(const val : Integer);
    function GetValueMax : Integer;
  public
    { Déclarations publiques }
    property Value : Integer read GetValue write SetValue;
    property ValueMin : Integer read GetValueMin write SetValueMin;
    property ValueMax : Integer read GetValueMax write SetValueMax;
  end;

implementation

{$R *.dfm}

uses
  SysUtils; 

procedure TRTrackBarEdit.TrackBarChange(Sender: TObject);
begin
   Edit.Text:=IntToStr(TrackBar.Position);
end;

procedure TRTrackBarEdit.EditChange(Sender: TObject);
var
   i : Integer;
begin
   try
      i:=StrToInt(Edit.Text);
      TrackBar.Position:=i;
   except
      // ignore
   end;
end;

// SetValue
//
procedure TRTrackBarEdit.SetValue(const val : Integer);
begin
   TrackBar.Position:=val;
   TrackBarChange(Self);
end;

// GetValue
//
function TRTrackBarEdit.GetValue : Integer;
begin
   Result:=TrackBar.Position;
end;

// SetValueMax
//
procedure TRTrackBarEdit.SetValueMax(const val : Integer);
begin
   TrackBar.Max:=val;
   TrackBarChange(Self);
end;

// GetValueMax
//
function TRTrackBarEdit.GetValueMax : Integer;
begin
   Result:=TrackBar.Max;
end;

// SetValueMin
//
procedure TRTrackBarEdit.SetValueMin(const val : Integer);
begin
   TrackBar.Min:=val;
   TrackBarChange(Self);
end;

// GetValueMin
//
function TRTrackBarEdit.GetValueMin : Integer;
begin
   Result:=TrackBar.Min;
end;

end.



