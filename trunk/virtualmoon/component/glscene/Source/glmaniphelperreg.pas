unit glmaniphelperreg;

{
      $Log: glmaniphelperreg.pas,v $
      Revision 1.1  2006/01/10 20:50:45  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.2  2006/01/09 20:45:49  z0m3ie
      *** empty log message ***

      Revision 1.1  2005/12/01 21:24:11  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/08/03 00:41:39  z0m3ie
      - added automatical generated History from CVS

}

interface

uses
  glmaniphelper, glgizmo;

procedure Register;

implementation

uses classes;

procedure Register;
begin
  RegisterComponents('GLScene Utils', [TGLManipHelper]);
end;

end.
