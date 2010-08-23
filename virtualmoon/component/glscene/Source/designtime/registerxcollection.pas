//
// This unit is part of the GLScene Project, http://glscene.org
//
{: RegisterXCollection<p>

	Register TXCollection property editor<p>

	<b>History : </b><font size=-1><ul>
      <li>11/11/09 - DaStr - Improved FPC compatibility (merged from gls4laz)
                             (thanks Predator) (BugtrackerID = 2893580)
      <li>03/07/04 - LR - Removed ..\ from the GLScene.inc
      <li>16/04/00 - Egg - Creation
	</ul></font>
}
unit RegisterXCollection;

interface

{$i GLScene.inc}

uses
  Classes, XCollection,
  {$IFDEF FPC}
     componenteditors, propedits
  {$ELSE}
    {$IFDEF GLS_DELPHI_6_UP} DesignEditors, DesignIntf {$ELSE} DsgnIntf {$ENDIF}
  {$ENDIF}
   ;

type

	// TGLXCollectionProperty
	//
	TXCollectionProperty = class(TClassProperty)
		public
			{ Public Declarations }
			function GetAttributes: TPropertyAttributes; override;
			procedure Edit; override;
	end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses FXCollectionEditor;


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TXCollection), nil, '', TXCollectionProperty);
end;

//----------------- TXCollectionProperty ------------------------------------

// GetAttributes
//
function TXCollectionProperty.GetAttributes: TPropertyAttributes;
begin
	Result:=[paDialog];
end;

// Edit
//
procedure TXCollectionProperty.Edit;
begin
   with XCollectionEditor do begin
   {$IFDEF FPC}
      SetXCollection(TXCollection(GetOrdValue));
   {$ELSE}
      SetXCollection(TXCollection(GetOrdValue), Self.Designer);
   {$ENDIF}
      Show;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   
end.

