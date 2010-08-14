//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLViewer<p>

   Platform independant viewer.<p>

    History:
      <li>17/09/07 - DaStr - Replaced $IFNDEF KYLIX to $IFDEF MSWINDOWS in 
                              SetupVSync() because wgl* functions are Windows-specific
      <li>12/09/07 - DaStr - Fixed SetupVSync() function (Bugtracker ID = 1786279)
                             Made cross-platform code easier to read
      <li>12/07/07 - DaStr - Added SetupVSync
      <li>30/03/07 - DaStr - Another update after the previous fix (removed class())
                             Added TVSyncMode type and constants.
      <li>24/03/07 - DaStr - Update for Windows after the previous fix
      <li>21/03/07 - DaStr - Improved Cross-Platform compatibility
                             (thanks Burkhard Carstens) (Bugtracker ID = 1684432)
      <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTrackerID=1681585)
      <li>24/01/02 -  EG   - Initial version
}

unit GLViewer;

interface

{$I GLScene.inc}

uses
  classes,
  forms,
  controls,
  OpenGL1x,
  {$IFDEF GLS_DELPHI_OR_CPPB} GLWin32Viewer; {$ENDIF}
  {$IFDEF FPC}                GLLCLViewer;   {$ENDIF}

type
{$IFDEF FPC}  //For FPC, always use LCLViewer
  TGLSceneViewer = class(TGLSceneViewerLCL)
  end;
  TVSyncMode = GLLCLViewer.TVSyncMode;
{$ENDIF FPC}

{$IFDEF GLS_DELPHI_OR_CPPB}
  TGLSceneViewer = class(TGLSceneViewerWin32)
  end;
  TVSyncMode = GLWin32Viewer.TVSyncMode;
{$ENDIF GLS_DELPHI_OR_CPPB}

const
{$IFDEF FPC}
  // TVSyncMode.
  vsmSync = GLLCLViewer.vsmSync;
  vsmNoSync = GLLCLViewer.vsmNoSync;
{$ENDIF FPC}

{$IFDEF GLS_DELPHI_OR_CPPB}
  // TVSyncMode.
  vsmSync = GLWin32Viewer.vsmSync;
  vsmNoSync = GLWin32Viewer.vsmNoSync;
{$ENDIF GLS_DELPHI_OR_CPPB}


procedure SetupVSync(const AVSyncMode : TVSyncMode);

implementation

procedure SetupVSync(const AVSyncMode : TVSyncMode);
{$IFDEF MSWINDOWS}
var
  I: Integer;
begin
  if WGL_EXT_swap_control then
  begin
    I := wglGetSwapIntervalEXT;
    case AVSyncMode of
      vsmSync  : if I <> 1 then wglSwapIntervalEXT(1);
      vsmNoSync: if I <> 0 then wglSwapIntervalEXT(0);
    else
       Assert(False);
    end;
  end;
end;
{$ELSE}
begin
   Assert(False, 'SetupVSync only implemented for Windows!')
end;
{$ENDIF}


initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterClass(TGLSceneViewer);

end.
