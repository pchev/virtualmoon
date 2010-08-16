//
// This unit is part of the GLScene Project, http://glscene.org
//
{: PictureRegisteredFormats<p>

   Hacks into the VCL to access the list of TPicture registered TGraphic formats<p>

   <b>History : </b><font size=-1><ul>
      <li>25/01/10 - DaStr - Updated warning about a possible crash while using the
                              'Use Debug DCUs' compiler option (BugTrackerID=1586936)
      <li>10/11/09 - DaStr - Replaced all Delphi2005+ IFDEFs with a single one
      <li>07/11/09 - DaStr - Improved FPC compatibility
                              (BugtrackerID = 2893580) (thanks Predator)
      <li>16/10/08 - UweR - Added IFDEF for Delphi 2009
      <li>06/04/08 - DanB - Change to HackTPictureRegisteredFormats due to Char changing
                            size in Delphi 2009
      <li>06/04/08 - DaStr - Added IFDEFs for Delphi 5 compatibility
      <li>20/12/06 - DaStr - Added a warning about optimization turned off
                             in HackTPictureRegisteredFormats (BugTrackerID=1586936)
      <li>08/03/06 - ur - Added Delphi 2006 support
      <li>28/02/05 - EG - Added BPL support
      <li>24/02/05 - EG - Creation
   </ul></font>
}
unit PictureRegisteredFormats;

interface

{$I GLScene.inc}

uses Classes, Graphics;

{$ifdef GLS_DELPHI_5} {$define PRF_HACK_PASSES}  {$endif} // Delphi 5
{$ifdef GLS_DELPHI_6} {$define PRF_HACK_PASSES}  {$endif} // Delphi 6
{$ifdef GLS_DELPHI_7} {$define PRF_HACK_PASSES}  {$endif} // Delphi 7
                                                          // skip Delphi 8
{$ifdef GLS_DELPHI_2005_UP} {$define PRF_HACK_PASSES} {$endif} // Delphi 2005+
{$ifdef FPC}            {$define PRF_HACK_PASSES}  {$endif}    // FPC

{$ifndef PRF_HACK_PASSES}
  {$Message Warn 'PRF hack not tested for this Delphi version!'}
{$endif}

{: Returns the TGraphicClass associated to the extension, if any.<p>
   Accepts anExtension with or without the '.' }
function GraphicClassForExtension(const anExtension : String) : TGraphicClass;

{: Adds to the passed TStrings the list of registered formats.<p>
   Convention is "extension=description" for the string, the Objects hold
   the corresponding TGraphicClass (extensions do not include the '.'). }
procedure HackTPictureRegisteredFormats(destList : TStrings);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   PInteger = ^Integer;

// GraphicClassForExtension
//
function GraphicClassForExtension(const anExtension : String) : TGraphicClass;
var
   i : Integer;
   sl : TStringList;
   buf : String;
begin
   Result:=nil;
   if anExtension='' then Exit;
   if anExtension[1]='.' then
      buf:=Copy(anExtension, 2, MaxInt)
   else buf:=anExtension;
   sl:=TStringList.Create;
   try
      HackTPictureRegisteredFormats(sl);
      i:=sl.IndexOfName(buf);
      if i>=0 then
         Result:=TGraphicClass(sl.Objects[i]);
   finally
      sl.Free;
   end;
end;

type
   PFileFormat = ^TFileFormat;
   TFileFormat = record
      GraphicClass: TGraphicClass;
      Extension: string;
      Description: string;
      DescResID: Integer;
   end;

// HackTPictureRegisteredFormats
//
procedure HackTPictureRegisteredFormats(destList : TStrings);
var
   pRegisterFileFormat, pCallGetFileFormat, pGetFileFormats, pFileFormats : PAnsiChar;
   iCall : Integer;
   i : Integer;
   list : TList;
   fileFormat : PFileFormat;
begin
   // Warning: This will crash when Graphics.pas is compiled with the 'Use Debug DCUs' option.

   pRegisterFileFormat:=PAnsiChar(@TPicture.RegisterFileFormat);
   if pRegisterFileFormat[0]=#$FF then // in case of BPL redirector
      pRegisterFileFormat:=PAnsiChar(PInteger(PInteger(@pRegisterFileFormat[2])^)^);
   pCallGetFileFormat:=@pRegisterFileFormat[16];
   iCall:=PInteger(pCallGetFileFormat)^;
   pGetFileFormats:=@pCallGetFileFormat[iCall+4];
   pFileFormats:=PAnsiChar(PInteger(@pGetFileFormats[2])^);
   list:=TList(PInteger(pFileFormats)^);
   if list<>nil then begin
      for i:=0 to list.Count-1 do begin
         fileFormat:=PFileFormat(list[i]);
         destList.AddObject(fileFormat.Extension+'='+fileFormat.Description,
                                              TObject(fileFormat.GraphicClass));
      end;
   end;
end;

end.
