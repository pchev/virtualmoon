{: FVectorEditor<p>

   Editor for a vector.<p>

      $Log: fvectoreditor.pas,v $
      Revision 1.1  2006/01/10 20:50:44  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.1  2006/01/09 21:02:30  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/08/03 00:41:38  z0m3ie
      - added automatical generated History from CVS

   <b>Historique : </b><font size=-1><ul>
      <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
      <li>03/07/04 - LR - Make change for Linux
      <li>?/?/? -  - Creation
   </ul></font>
}
unit FVectorEditor;

interface

{$i GLScene.inc}

uses
  {$ifdef fpc}lresources,{$endif}
  Forms, ComCtrls, StdCtrls, ToolWin, ExtCtrls, Buttons, Graphics, Controls, 
  Classes;


type
  TVectorEditorForm = class(TForm)
    EDx: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EDy: TEdit;
    EDz: TEdit;
    BBok: TBitBtn;
    BBcancel: TBitBtn;
    IMx: TImage;
    IMy: TImage;
    IMz: TImage;
    SpeedButton1: TSpeedButton;
    SBmX: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SBmY: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SBmZ: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SBUnit: TSpeedButton;
    SpeedButton9: TSpeedButton;
    Bevel1: TBevel;
    SBInvert: TSpeedButton;
    procedure TBxClick(Sender: TObject);
    procedure TByClick(Sender: TObject);
    procedure TBzClick(Sender: TObject);
    procedure TBnullClick(Sender: TObject);
    procedure EDxChange(Sender: TObject);
    procedure EDyChange(Sender: TObject);
    procedure EDzChange(Sender: TObject);
    procedure SBmXClick(Sender: TObject);
    procedure SBmYClick(Sender: TObject);
    procedure SBmZClick(Sender: TObject);
    procedure SBUnitClick(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SBInvertClick(Sender: TObject);
  private
    { private declarations }
    vx, vy, vz : Single;
    procedure TestInput(edit : TEdit; imError : TImage; var dest : Single);
  public
    { public declarations }
    function Execute(var x, y, z : Single) : Boolean;
  end;

function VectorEditorForm : TVectorEditorForm;
procedure ReleaseVectorEditorForm;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  SysUtils, VectorGeometry, GLUtils; 


var
	vVectorEditorForm : TVectorEditorForm;

function VectorEditorForm : TVectorEditorForm;
begin
	if not Assigned(vVectorEditorForm) then
      vVectorEditorForm:=TVectorEditorForm.Create(nil);
	Result:=vVectorEditorForm;
end;

procedure ReleaseVectorEditorForm;
begin
	if Assigned(vVectorEditorForm) then begin
	   vVectorEditorForm.Free; vVectorEditorForm:=nil;
	end;
end;

// Execute
//
function TVectorEditorForm.Execute(var x, y, z : Single) : Boolean;
begin
   // setup dialog fields
   vx:=x;
   vy:=y;
   vz:=z;
   EDx.Text:=FloatToStr(vx);
   EDy.Text:=FloatToStr(vy);
   EDz.Text:=FloatToStr(vz);
   // show the dialog
   Result:=(ShowModal=mrOk);
   if Result then begin
      x:=vx;
      y:=vy;
      z:=vz;
   end;
end;

procedure TVectorEditorForm.TestInput(edit : TEdit; imError : TImage; var dest : Single);
begin
   if Visible then begin
      try
         dest:=StrToFloat(edit.Text);
         imError.Visible:=False;
      except
         imError.Visible:=True;
      end;
      BBOk.Enabled:=not (IMx.Visible or IMy.Visible or IMz.Visible);
   end;
end;

procedure TVectorEditorForm.TBxClick(Sender: TObject);
begin
   EDx.Text:='1'; EDy.Text:='0'; EDz.Text:='0';
end;

procedure TVectorEditorForm.TByClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='1'; EDz.Text:='0';
end;

procedure TVectorEditorForm.TBzClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='0'; EDz.Text:='1';
end;

procedure TVectorEditorForm.TBnullClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='0'; EDz.Text:='0';
end;

procedure TVectorEditorForm.EDxChange(Sender: TObject);
begin
   TestInput(EDx, IMx, vx);
end;

procedure TVectorEditorForm.EDyChange(Sender: TObject);
begin
   TestInput(EDy, IMy, vy);
end;

procedure TVectorEditorForm.EDzChange(Sender: TObject);
begin
   TestInput(EDz, IMz, vz);
end;

procedure TVectorEditorForm.SBmXClick(Sender: TObject);
begin
   EDx.Text:='-1'; EDy.Text:='0'; EDz.Text:='0';
end;

procedure TVectorEditorForm.SBmYClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='-1'; EDz.Text:='0';
end;

procedure TVectorEditorForm.SBmZClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='0'; EDz.Text:='-1';
end;

procedure TVectorEditorForm.SBUnitClick(Sender: TObject);
begin
   EDx.Text:='1'; EDy.Text:='1'; EDz.Text:='1';
end;

procedure TVectorEditorForm.SpeedButton9Click(Sender: TObject);
var
   v : TAffineVector;
begin
   SetVector(v, StrToFloatDef(EDx.Text, 0), StrToFloatDef(EDy.Text, 0), StrToFloatDef(EDz.Text, 0));
   if VectorLength(v)=0 then
      v:=NullVector
   else NormalizeVector(v);
   EDx.Text:=FloatToStr(v[0]);
   EDy.Text:=FloatToStr(v[1]);
   EDz.Text:=FloatToStr(v[2]);
end;

procedure TVectorEditorForm.SBInvertClick(Sender: TObject);
var
   v : TAffineVector;
begin
   SetVector(v, StrToFloatDef(EDx.Text, 0), StrToFloatDef(EDy.Text, 0), StrToFloatDef(EDz.Text, 0));
   NegateVector(v);
   EDx.Text:=FloatToStr(v[0]);
   EDy.Text:=FloatToStr(v[1]);
   EDz.Text:=FloatToStr(v[2]);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  {$IFDEF FPC}{$i FVectorEditor.lrs}{$ENDIF}

finalization

   ReleaseVectorEditorForm;

end.



