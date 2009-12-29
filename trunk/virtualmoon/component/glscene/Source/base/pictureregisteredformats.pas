//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFile3DSSceneObjects<p>

   Hacks into the VCL to access the list of TPicture registered TGraphic formats<p>

      $Log: pictureregisteredformats.pas,v $
      Revision 1.1  2006/01/10 20:50:44  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.1  2006/01/09 21:01:42  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/12/04 16:52:59  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.1  2005/12/01 21:24:10  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/08/03 00:41:38  z0m3ie
      - added automatical generated History from CVS

   <b>History : </b><font size=-1><ul>
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
{$ifdef GLS_DELPHI_9} {$define PRF_HACK_PASSES}  {$endif} // Delphi 2005
{$ifdef GLS_DELPHI_10}{$define PRF_HACK_PASSES}  {$endif} // Delphi 2006
{$ifdef LCL}    {$define PRF_HACK_PASSES} {$endif} // LCL


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
   pRegisterFileFormat, pCallGetFileFormat, pGetFileFormats, pFileFormats : PChar;
   iCall : Integer;
   i : Integer;
   list : TList;
   fileFormat : PFileFormat;
begin
{$IFDEF GLS_DELPHI_7_DOWN}
{$IFDEF GLS_DELPHI_6_UP}
  {$IFOPT O-}
    {$Message Warn 'This may crash when Graphics.pas is compiled with optimization Off'}
  {$ENDIF}
{$ENDIF}
{$ENDIF}
   pRegisterFileFormat:=PChar(@TPicture.RegisterFileFormat);
   if pRegisterFileFormat[0]=#$FF then // in case of BPL redirector
      pRegisterFileFormat:=PChar(PInteger(PInteger(@pRegisterFileFormat[2])^)^);
   pCallGetFileFormat:=@pRegisterFileFormat[16];
   iCall:=PInteger(pCallGetFileFormat)^;
   pGetFileFormats:=@pCallGetFileFormat[iCall+4];
   pFileFormats:=PChar(PInteger(@pGetFileFormats[2])^);
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
