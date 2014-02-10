//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLNewtonRegister<p>

  Design time registration code for the Newton Manager.<p>

  <b>History : </b><font size=-1><ul>
  <li>04/01/11 - FP - Removed Joint
  <li>15/07/10 - FP - Creation by Franck Papouin
  </ul></font>
}

unit GLNewtonRegister;

interface

uses
  Classes, GLNGDManager;

procedure register;

implementation

// Register
//
procedure register;
begin
  RegisterClasses([TGLNGDManager, TGLNGDDynamic, TGLNGDStatic]);
  RegisterComponents('GLScene', [TGLNGDManager]);
end;

end.
