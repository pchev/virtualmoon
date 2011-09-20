unit GLS_SDL;

interface

uses
  SDLWindow, GLSDLContext, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('SDLWindow', @SDLWindow.Register); 
  RegisterUnit('GLSDLContext', @GLSDLContext.Register); 
end; 

initialization
  RegisterPackage('GLS_SDL', @Register); 
end.
