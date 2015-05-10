//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : PlugInIntf<p>

  An interface unit to GLScene plug-ins.<p>
  For more information see help file for writing plug-ins.<p>

  <b>History : </b><font size=-1><ul>
  <li>17/11/14 - PW - Renamed from PlugInIntf.pas to GLPlugInIntf.pas
  <li>16/10/08 - UweR - Compatibility fix for Delphi 2009
  PlugIn interface stays at PAnsiChar
  <li>02/04/07 - DaStr - Added $I GLScene.inc
  <li>28/07/01 - EG - Creation
  </ul></font>
}

unit GLPlugInIntf;

interface

{$I GLScene.inc}

type
  TPIServiceType = (stRaw, stObject, stBitmap, stTexture, stImport, stExport);
  TPIServices = set of TPIServiceType;

  TEnumCallBack = procedure(Name: PAnsiChar); stdcall;

  TEnumResourceNames = procedure(Service: TPIServiceType;
    Callback: TEnumCallBack); stdcall;
  TGetServices = function: TPIServices; stdcall;
  TGetVendor = function: PAnsiChar; stdcall;
  TGetDescription = function: PAnsiChar; stdcall;
  TGetVersion = function: PAnsiChar; stdcall;

implementation

end.
