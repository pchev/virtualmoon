//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSceneRegisterWinOnlyLCL<p>

   Contains registration for Design-Time Lazarus Windows-Only units.<p>
   Because of Lazarus'es limitations, these need to be separated from the main
   GLSceneRegisterLCL.pas

   <b>History :</b><font size=-1><ul>
      <li>07/01/10 - DaStr - Removed GLLCLFullScreenViewer because it became
                              cross-platform (thanks Predator)
      <li>24/11/09 - DanB - Added some more windows only units
      <li>22/11/09 - DaStr - Initial version (by Predator)
   </ul></font>
}

unit GLSceneRegisterWinOnlyLCL;

{$IFNDEF MSWINDOWS}{$Message Error 'Unit not supported'}{$ENDIF}

interface

uses
   Classes, GLSceneRegisterLCL, GLStrings,
   GLSpaceText,
   GLAVIRecorder, Joystick, ScreenSaver, GLSMWaveOut, LResources;

procedure Register;

implementation

procedure Register;
begin
   RegisterComponents('GLScene',
                      [TGLSMWaveOut
                        ]);

   RegisterComponents('GLScene Utils',
                      [TAVIRecorder,  TJoystick, TScreenSaver
                      ]);

end;

initialization

   {$I nonGLSceneLCL.lrs}

   with ObjectManager do begin
      RegisterSceneObject(TGLSpaceText, 'SpaceText', glsOCDoodad, HInstance);
   end;

finalization
end.
