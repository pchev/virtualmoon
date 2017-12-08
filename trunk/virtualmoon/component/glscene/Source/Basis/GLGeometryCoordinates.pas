//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Helper functions to convert between different three dimensional coordinate
   systems. Room for optimisations.

	 History :  
       10/12/14 - PW - Renamed GeometryCoordinates unit to GLGeometryCoordinates
       30/04/03 - EG - Hyperbolic functions moved to GLVectorGeometry.pas
       30/04/03 - ARH - Remove math.pas dependency
       24/04/03 - ARH - Double versions added
       10/04/03 - ARH - Added ProlateSpheroidal,OblateSpheroidal,
                           BiPolarCylindrical
       09/04/03 - ARH - Initial Version (Cylindrical,Spherical)
    
}
unit GLGeometryCoordinates;

interface

uses
  GLVectorGeometry;

{ Convert cylindrical to cartesian [single]. theta in rad}
procedure Cylindrical_Cartesian(const r,theta,z1:single;var x,y,z:single);overload;
{ Convert cylindrical to cartesian [double]. theta in rads}
procedure Cylindrical_Cartesian(const r,theta,z1:double;var x,y,z:double);overload;
{ Convert cylindrical to cartesian [single] (with error check). theta in rad}
procedure Cylindrical_Cartesian(const r,theta,z1:single;var x,y,z:single;
  var ierr:integer);overload;
{ Convert cylindrical to cartesian [double] (with error check). theta in rad}
procedure Cylindrical_Cartesian(const r,theta,z1:double;var x,y,z:double;
  var ierr:integer);overload;

{ Convert cartesian to cylindrical [single]}
procedure Cartesian_Cylindrical(const x,y,z1:single; var r,theta,z:single);overload;
{ Convert cartesion to cylindrical [double]}
procedure Cartesian_Cylindrical(const x,y,z1:double; var r,theta,z:double);overload;

{ Convert spherical to cartesion. [single] theta,phi in rads}
procedure Spherical_Cartesian(const r,theta,phi:single;var x,y,z:single);overload;
{ Convert spherical to cartesion. [double] theta,phi in rads}
procedure Spherical_Cartesian(const r,theta,phi:double;var x,y,z:double);overload;
{ Convert spherical to cartesian [single] (with error check).theta,phi in rad}
procedure Spherical_Cartesian(const r,theta,phi:single;var x,y,z:single;
  var ierr:integer);overload;
{ Convert spherical to cartesian [double] (with error check).theta,phi in rad}
procedure Spherical_Cartesian(const r,theta,phi:double;var x,y,z:double;
  var ierr:integer);overload;

{ Convert cartesian to spherical [single]}
procedure Cartesian_Spherical(const x,y,z:single; var r,theta,phi:single);overload;
procedure Cartesian_Spherical(const v : TAffineVector; var r, theta, phi : Single); overload;
{ Convert cartesion to spherical [double]}
procedure Cartesian_Spherical(const x,y,z:double; var r,theta,phi:double);overload;

{ Convert Prolate-Spheroidal to Cartesian. [single] eta, phi in rad}
procedure ProlateSpheroidal_Cartesian(const xi,eta,phi,a:single;
  var x,y,z:single);overload;
{ Convert Prolate-Spheroidal to Cantesian. [double] eta,phi in rad}
procedure ProlateSpheroidal_Cartesian(const xi,eta,phi,a:double;
  var x,y,z:double);overload;
{ Convert Prolate-Spheroidal to Cartesian [single](with error check). eta,phi in rad}
procedure ProlateSpheroidal_Cartesian(const xi,eta,phi,a:single; var x,y,z:single;
  var ierr:integer);overload;
{ Convert Prolate-Spheroidal to Cartesian [single](with error check). eta,phi in rad}
procedure ProlateSpheroidal_Cartesian(const xi,eta,phi,a:double; var x,y,z:double;
  var ierr:integer);overload;

{ Convert Oblate-Spheroidal to Cartesian. [Single] eta, phi in rad}
procedure OblateSpheroidal_Cartesian(const xi,eta,phi,a:single;
  var x,y,z:single);overload;
{ Convert Oblate-Spheroidal to Cartesian. [Double] eta, phi in rad}
procedure OblateSpheroidal_Cartesian(const xi,eta,phi,a:double;
  var x,y,z:double);overload;
{ Convert Oblate-Spheroidal to Cartesian (with error check). eta,phi in rad}
procedure OblateSpheroidal_Cartesian(const xi,eta,phi,a:single; var x,y,z:single;
  var ierr:integer);overload;
{ Convert Oblate-Spheroidal to Cartesian (with error check).[Double] eta,phi in rad}
procedure OblateSpheroidal_Cartesian(const xi,eta,phi,a:double; var x,y,z:double;
  var ierr:integer);overload;

{ Convert Bipolar to Cartesian. u in rad}
procedure BipolarCylindrical_Cartesian(const u,v,z1,a:single;
  var x,y,z:single);overload;
{ Convert Bipolar to Cartesian. [Double] u in rad}
procedure BipolarCylindrical_Cartesian(const u,v,z1,a:double;
  var x,y,z:double);overload;
{ Convert Bipolar to Cartesian (with error check). u in rad}
procedure BipolarCylindrical_Cartesian(const u,v,z1,a:single; var x,y,z:single;
  var ierr:integer);overload;
{ Convert Bipolar to Cartesian (with error check). [Double] u in rad}
procedure BipolarCylindrical_Cartesian(const u,v,z1,a:double; var x,y,z:double;
  var ierr:integer);overload;

implementation

// ----- Cylindrical_Cartesian -------------------------------------------------
{** Convert Cylindrical to Cartesian with no checks.
Ref: http://mathworld.wolfram.com/CylindricalCoordinates.html}
procedure Cylindrical_Cartesian(const r,theta,z1:single;var x,y,z:single);

begin
  GLVectorGeometry.sincos(theta,r,y,x);
  z := z1;
end;
// ----- Cylindrical_Cartesian -------------------------------------------------
{** Convert Cylindrical to Cartesian with no checks. Double version
Ref: http://mathworld.wolfram.com/CylindricalCoordinates.html}
procedure Cylindrical_Cartesian(const r,theta,z1:double;var x,y,z:double);

begin
  GLVectorGeometry.sincos(theta,r,y,x);
  z := z1;
end;
// ----- Cylindrical_Cartesian -------------------------------------------------
{** Convert Cylindrical to Cartesian with checks.
ierr: [0] = ok,
      [1] = r out of bounds. Acceptable r: [0,inf)
      [2] = theta out of bounds. Acceptable theta: [0,2pi)
      [3] = z1 out of bounds. Acceptable z1 : (-inf,inf)
Ref: http://mathworld.wolfram.com/CylindricalCoordinates.html}
procedure Cylindrical_Cartesian(const r,theta,z1:single;var x,y,z:single;
  var ierr:integer);

begin
{** check input parameters}
  if (r < 0.0) then
    ierr := 1
  else if ((theta < 0.0) or (theta >= 2*pi)) then
    ierr := 2
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    GLVectorGeometry.sincos(theta,r,y,x);
    z := z1;
  end;
end;
// ----- Cylindrical_Cartesian -------------------------------------------------
{** Convert Cylindrical to Cartesian with checks.
ierr: [0] = ok,
      [1] = r out of bounds. Acceptable r: [0,inf)
      [2] = theta out of bounds. Acceptable theta: [0,2pi)
      [3] = z1 out of bounds. Acceptable z1 : (-inf,inf)
Ref: http://mathworld.wolfram.com/CylindricalCoordinates.html}
procedure Cylindrical_Cartesian(const r,theta,z1:double;var x,y,z:double;
  var ierr:integer);

begin
{** check input parameters}
  if (r < 0.0) then
    ierr := 1
  else if ((theta < 0.0) or (theta >= 2*pi)) then
    ierr := 2
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    GLVectorGeometry.sincos(theta,r,y,x);
    z := z1;
  end;
end;
// ----- Cartesian_Cylindrical -------------------------------------------------
{** Convert Cartesian to Cylindrical no checks. Single}
procedure Cartesian_Cylindrical(const x,y,z1:single; var r,theta,z:single);

begin
  r := sqrt(x*x+y*y);
  theta := GLVectorGeometry.arctan2(y,x);
  z := z1;
end;
// ----- Cartesian_Cylindrical -------------------------------------------------
{** Convert Cartesian to Cylindrical no checks. Duoble}
procedure Cartesian_Cylindrical(const x,y,z1:double; var r,theta,z:double);

begin
  r := sqrt(x*x+y*y);
  theta := GLVectorGeometry.arctan2(y,x);
  z := z1;
end;
// ----- Spherical_Cartesian ---------------------------------------------------
{** Convert Spherical to Cartesian with no checks.
Ref: http://mathworld.wolfram.com/SphericalCoordinates.html}
procedure Spherical_Cartesian(const r,theta,phi:single; var x,y,z:single);

var
  a : single;

begin
  GLVectorGeometry.sincos(phi,r,a,z);   // z = r*cos(phi), a=r*sin(phi)
  GLVectorGeometry.sincos(theta,a,y,x); // x = a*cos(theta), y = a*sin(theta)}
end;
// ----- Spherical_Cartesian ---------------------------------------------------
{** Convert Spherical to Cartesian with no checks. Double version.
Ref: http://mathworld.wolfram.com/SphericalCoordinates.html}
procedure Spherical_Cartesian(const r,theta,phi:double; var x,y,z:double);

var
  a : double;

begin
  GLVectorGeometry.sincos(phi,r,a,z);   // z = r*cos(phi), a=r*sin(phi)
  GLVectorGeometry.sincos(theta,a,y,x); // x = a*cos(theta), y = a*sin(theta)}
end;
// ----- Spherical_Cartesian ---------------------------------------------------
{** Convert Spherical to Cartesian with checks.
ierr: [0] = ok,
      [1] = r out of bounds
      [2] = theta out of bounds
      [3] = phi out of bounds
Ref: http://mathworld.wolfram.com/SphericalCoordinates.html}
procedure Spherical_Cartesian(const r,theta,phi:single; var x,y,z:single;
  var ierr:integer);

var
  a : single;

begin
  if (r < 0.0) then
    ierr := 1
  else if ((theta < 0.0) or (theta >= 2*pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2*pi)) then
    ierr := 3
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    GLVectorGeometry.sincos(phi,r,a,z);   // z = r*cos(phi), a=r*sin(phi)
    GLVectorGeometry.sincos(theta,a,y,x); // x = a*cos(theta), y = a*sin(theta)}
  end;
end;
// ----- Spherical_Cartesian ---------------------------------------------------
{** Convert Spherical to Cartesian with checks.
ierr: [0] = ok,
      [1] = r out of bounds
      [2] = theta out of bounds
      [3] = phi out of bounds
Ref: http://mathworld.wolfram.com/SphericalCoordinates.html}
procedure Spherical_Cartesian(const r,theta,phi:double; var x,y,z:double;
  var ierr:integer);

var
  a : double;

begin
  if (r < 0.0) then
    ierr := 1
  else if ((theta < 0.0) or (theta >= 2*pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2*pi)) then
    ierr := 3
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    GLVectorGeometry.sincos(phi,r,a,z);   // z = r*cos(phi), a=r*sin(phi)
    GLVectorGeometry.sincos(theta,a,y,x); // x = a*cos(theta), y = a*sin(theta)}
  end;
end;

// ----- Cartesian_Spherical ---------------------------------------------------
{** convert Cartesian to Spherical, no checks, single
Ref: http://mathworld.wolfram.com/SphericalCoordinates.html
NB: Could be optimised by using jclmath.pas unit?
}
procedure Cartesian_Spherical(const x,y,z:single; var r,theta,phi:single);

begin
  r := sqrt((x*x)+(y*y)+(z*z));
  theta := GLVectorGeometry.arctan2(y,x);
  phi := GLVectorGeometry.arccos(z/r);
end;

// Cartesian_Spherical
//
procedure Cartesian_Spherical(const v : TAffineVector; var r, theta, phi : Single);
begin
   r:=VectorLength(v);
   theta:=ArcTan2(v.V[1], v.V[0]);
   phi:=ArcCos(v.V[2]/r);
end;

// ----- Cartesian_Spherical ---------------------------------------------------
{** convert Cartesian to Spherical, no checks, double
Ref: http://mathworld.wolfram.com/SphericalCoordinates.html
NB: Could be optimised by using jclmath.pas unit?
}
procedure Cartesian_Spherical(const x,y,z:double; var r,theta,phi:double);

begin
  r := sqrt((x*x)+(y*y)+(z*z));
  theta := GLVectorGeometry.arctan2(y,x);
  phi := GLVectorGeometry.arccos(z/r);
end;
// ----- ProlateSpheroidal_Cartesian -------------------------------------------
{** Convert Prolate-Spheroidal to Cartesian with no checks.
A system of curvilinear coordinates in which two sets of coordinate surfaces are
obtained by revolving the curves of the elliptic cylindrical coordinates about
the x-axis, which is relabeled the z-axis. The third set of coordinates
consists of planes passing through this axis.
The coordinate system is parameterised by parameter a. A default value of a=1 is
suggesed:
http://documents.wolfram.com/v4/AddOns/StandardPackages/Calculus/VectorAnalysis.html
Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html}
procedure ProlateSpheroidal_Cartesian(const xi,eta,phi,a:single;var x,y,z:single);

var
  sn,cs,snphi,csphi,shx,chx : single;

begin
  GLVectorGeometry.sincos(eta,a,sn,cs);
  GLVectorGeometry.SinCos(phi,snphi,csphi);
  shx:=sinh(xi);
  chx:=cosh(xi);
  x := sn*shx*csphi;   // x = a*sin(eta)*sinh(xi)*cos(phi)
  y := sn*shx*snphi;   // y = a*sin(eta)*sinh(xi)*sin(phi)
  z := cs*chx;         // z = a*cos(eta)*cosh(xi)
end;
// ----- ProlateSpheroidal_Cartesian -------------------------------------------
{** Convert Prolate-Spheroidal to Cartesian with no checks. Double version.
A system of curvilinear coordinates in which two sets of coordinate surfaces are
obtained by revolving the curves of the elliptic cylindrical coordinates about
the x-axis, which is relabeled the z-axis. The third set of coordinates
consists of planes passing through this axis.
The coordinate system is parameterised by parameter a. A default value of a=1 is
suggesed:
http://documents.wolfram.com/v4/AddOns/StandardPackages/Calculus/VectorAnalysis.html
Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html}
procedure ProlateSpheroidal_Cartesian(const xi,eta,phi,a:double;var x,y,z:double);

var
  sn,cs,snphi,csphi,shx,chx : double;

begin
  GLVectorGeometry.sincos(eta,a,sn,cs);
  GLVectorGeometry.sincos(phi,snphi,csphi);
  shx:=sinh(xi);
  chx:=cosh(xi);
  x := sn*shx*csphi;   // x = a*sin(eta)*sinh(xi)*cos(phi)
  y := sn*shx*snphi;   // y = a*sin(eta)*sinh(xi)*sin(phi)
  z := cs*chx;         // z = a*cos(eta)*cosh(xi)
end;
// ----- ProlateSpheroidal_Cartesian -------------------------------------------
{** Convert Prolate-Spheroidal to Cartesian with checks.
ierr: [0] = ok,
      [1] = xi out of bounds. Acceptable xi: [0,inf)
      [2] = eta out of bounds. Acceptable eta: [0,pi]
      [3] = phi out of bounds. Acceptable phi: [0,2pi)
Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html}
procedure ProlateSpheroidal_Cartesian(const xi,eta,phi,a:single; var x,y,z:single;
  var ierr:integer);overload;

var
  sn,cs,snphi,csphi,shx,chx : single;

begin
  if (xi < 0.0) then
    ierr := 1
  else if ((eta < 0.0) or (eta > pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2*pi)) then
    ierr := 3
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    GLVectorGeometry.sincos(eta,a,sn,cs);
    GLVectorGeometry.sincos(phi,snphi,csphi);

    shx:=sinh(xi);
    chx:=cosh(xi);

    x := sn*shx*csphi;   // x = a*sin(eta)*sinh(xi)*cos(phi)
    y := sn*shx*snphi;   // y = a*sin(eta)*sinh(xi)*sin(phi)
    z := cs*chx;         // z = a*cos(eta)*cosh(xi)
  end;
end;
// ----- ProlateSpheroidal_Cartesian -------------------------------------------
{** Convert Prolate-Spheroidal to Cartesian with checks. Double Version.
ierr: [0] = ok,
      [1] = xi out of bounds. Acceptable xi: [0,inf)
      [2] = eta out of bounds. Acceptable eta: [0,pi]
      [3] = phi out of bounds. Acceptable phi: [0,2pi)
Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html}
procedure ProlateSpheroidal_Cartesian(const xi,eta,phi,a:double; var x,y,z:double;
  var ierr:integer);overload;

var
  sn,cs,snphi,csphi,shx,chx : double;

begin
  if (xi < 0.0) then
    ierr := 1
  else if ((eta < 0.0) or (eta > pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2*pi)) then
    ierr := 3
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    GLVectorGeometry.sincos(eta,a,sn,cs);
    GLVectorGeometry.sincos(phi,snphi,csphi);

    shx:=sinh(xi);
    chx:=cosh(xi);

    x := sn*shx*csphi;   // x = a*sin(eta)*sinh(xi)*cos(phi)
    y := sn*shx*snphi;   // y = a*sin(eta)*sinh(xi)*sin(phi)
    z := cs*chx;         // z = a*cos(eta)*cosh(xi)
  end;
end;
// ----- OblateSpheroidal_Cartesian -------------------------------------------
{** Convert Oblate-Spheroidal to Cartesian with no checks.
A system of curvilinear coordinates in which two sets of coordinate surfaces are
 obtained by revolving the curves of the elliptic cylindrical coordinates about
the y-axis which is relabeled the z-axis. The third set of coordinates consists
of planes passing through this axis.
The coordinate system is parameterised by parameter a. A default value of a=1 is
suggesed:
http://documents.wolfram.com/v4/AddOns/StandardPackages/Calculus/VectorAnalysis.html
Ref: http://mathworld.wolfram.com/OblateSpheroidalCoordinates.html}
procedure OblateSpheroidal_Cartesian(const xi,eta,phi,a:single;var x,y,z:single);

var
  sn,cs,snphi,csphi,shx,chx : single;

begin
  GLVectorGeometry.sincos(eta,a,sn,cs);
  GLVectorGeometry.sincos(phi,snphi,csphi);

  shx:=sinh(xi);
  chx:=cosh(xi);

  x := cs*chx*csphi; // x = a*cos(eta)*cosh(xi)*cos(phi)
  y := cs*chx*snphi; // y = a*cos(eta)*cosh(xi)*sin(phi)
  z := sn*shx;       // z = a*sin(eta)*sinh(xi)
end;
// ----- OblateSpheroidal_Cartesian -------------------------------------------
{** Convert Oblate-Spheroidal to Cartesian with no checks. Double Version.
A system of curvilinear coordinates in which two sets of coordinate surfaces are
 obtained by revolving the curves of the elliptic cylindrical coordinates about
the y-axis which is relabeled the z-axis. The third set of coordinates consists
of planes passing through this axis.
The coordinate system is parameterised by parameter a. A default value of a=1 is
suggesed:
http://documents.wolfram.com/v4/AddOns/StandardPackages/Calculus/VectorAnalysis.html
Ref: http://mathworld.wolfram.com/OblateSpheroidalCoordinates.html}
procedure OblateSpheroidal_Cartesian(const xi,eta,phi,a:double;var x,y,z:double);

var
  sn,cs,snphi,csphi,shx,chx : double;

begin
  GLVectorGeometry.sincos(eta,a,sn,cs);
  GLVectorGeometry.sincos(phi,snphi,csphi);

  shx:=sinh(xi);
  chx:=cosh(xi);

  x := cs*chx*csphi; // x = a*cos(eta)*cosh(xi)*cos(phi)
  y := cs*chx*snphi; // y = a*cos(eta)*cosh(xi)*sin(phi)
  z := sn*shx;       // z = a*sin(eta)*sinh(xi)
end;
// ----- OblateSpheroidal_Cartesian -------------------------------------------
{** Convert Oblate-Spheroidal to Cartesian with checks.
ierr: [0] = ok,
      [1] = xi out of bounds. Acceptable xi: [0,inf)
      [2] = eta out of bounds. Acceptable eta: [-0.5*pi,0.5*pi]
      [3] = phi out of bounds. Acceptable phi: [0,2*pi)
Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html}
procedure OblateSpheroidal_Cartesian(const xi,eta,phi,a:single; var x,y,z:single;
  var ierr:integer);overload;

var
  sn,cs,snphi,csphi,shx,chx : single;

begin
  if (xi < 0.0) then
    ierr := 1
  else if ((eta < -0.5*pi) or (eta > 0.5*pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2*pi)) then
    ierr := 3
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    GLVectorGeometry.SinCos(eta,a,sn,cs);
    GLVectorGeometry.sincos(phi,snphi,csphi);

    shx:=sinh(xi);
    chx:=cosh(xi);

    x := cs*chx*csphi; // x = a*cos(eta)*cosh(xi)*cos(phi)
    y := cs*chx*snphi; // y = a*cos(eta)*cosh(xi)*sin(phi)
    z := sn*shx;       // z = a*sin(eta)*sinh(xi)
  end;
end;
// ----- OblateSpheroidal_Cartesian -------------------------------------------
{** Convert Oblate-Spheroidal to Cartesian with checks. Double Version.
ierr: [0] = ok,
      [1] = xi out of bounds. Acceptable xi: [0,inf)
      [2] = eta out of bounds. Acceptable eta: [-0.5*pi,0.5*pi]
      [3] = phi out of bounds. Acceptable phi: [0,2*pi)
Ref: http://mathworld.wolfram.com/ProlateSpheroidalCoordinates.html}
procedure OblateSpheroidal_Cartesian(const xi,eta,phi,a:double; var x,y,z:double;
  var ierr:integer);overload;

var
  sn,cs,snphi,csphi,shx,chx : double;
  
begin
  if (xi < 0.0) then
    ierr := 1
  else if ((eta < -0.5*pi) or (eta > 0.5*pi)) then
    ierr := 2
  else if ((phi < 0.0) or (phi >= 2*pi)) then
    ierr := 3
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    GLVectorGeometry.SinCos(eta,a,sn,cs);
    GLVectorGeometry.sincos(phi,snphi,csphi);

    shx:=sinh(xi);
    chx:=cosh(xi);

    x := cs*chx*csphi; // x = a*cos(eta)*cosh(xi)*cos(phi)
    y := cs*chx*snphi; // y = a*cos(eta)*cosh(xi)*sin(phi)
    z := sn*shx;       // z = a*sin(eta)*sinh(xi)
  end;
end;
// ----- BipolarCylindrical_Cartesian ------------------------------------------
{** Convert BiPolarCylindrical to Cartesian with no checks.
http://mathworld.wolfram.com/BipolarCylindricalCoordinates.html }
procedure BipolarCylindrical_Cartesian(const u,v,z1,a:single;var x,y,z:single);

var
  cs,sn,shx,chx:single;

begin
  GLVectorGeometry.SinCos(u,sn,cs);
  shx:=sinh(v);
  chx:=cosh(v);

  x := a*shx/(chx-cs);
  y := a*sn/(chx-cs);
  z := z1;
end;
// ----- BipolarCylindrical_Cartesian ------------------------------------------
{** Convert BiPolarCylindrical to Cartesian with no checks. Double Version
http://mathworld.wolfram.com/BipolarCylindricalCoordinates.html }
procedure BipolarCylindrical_Cartesian(const u,v,z1,a:double;var x,y,z:double);

var
  cs,sn,shx,chx:double;

begin
  GLVectorGeometry.SinCos(u,sn,cs);
  shx:=sinh(v);
  chx:=cosh(v);

  x := a*shx/(chx-cs);
  y := a*sn/(chx-cs);
  z := z1;
end;
// ----- BipolarCylindrical_Cartesian ------------------------------------------
{** Convert Oblate-Spheroidal to Cartesian with checks.
ierr: [0] = ok,
      [1] = u out of bounds. Acceptable u: [0,2*pi)
      [2] = v out of bounds. Acceptable v: (-inf,inf)
      [3] = z1 out of bounds. Acceptable z1: (-inf,inf)
Ref: http://mathworld.wolfram.com/BiPolarCylindricalCoordinates.html}
procedure BipolarCylindrical_Cartesian(const u,v,z1,a:single;var x,y,z:single;
  var ierr:integer);overload;

var
  cs,sn,shx,chx:single;

begin
  if ((u < 0.0) or (u >= 2*pi)) then
    ierr := 1
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    GLVectorGeometry.SinCos(u,sn,cs);

    shx:=sinh(v);
    chx:=cosh(v);

    x := a*shx/(chx-cs);
    y := a*sn/(chx-cs);
    z := z1;
  end;
end;
// ----- BipolarCylindrical_Cartesian ------------------------------------------
{** Convert Oblate-Spheroidal to Cartesian with checks. Double Version
ierr: [0] = ok,
      [1] = u out of bounds. Acceptable u: [0,2*pi)
      [2] = v out of bounds. Acceptable v: (-inf,inf)
      [3] = z1 out of bounds. Acceptable z1: (-inf,inf)
Ref: http://mathworld.wolfram.com/BiPolarCylindricalCoordinates.html}
procedure BipolarCylindrical_Cartesian(const u,v,z1,a:double;var x,y,z:double;
  var ierr:integer);overload;

var
  cs,sn,shx,chx:double;

begin
  if ((u < 0.0) or (u >= 2*pi)) then
    ierr := 1
  else
    ierr := 0;

  if (ierr = 0) then
  begin
    GLVectorGeometry.SinCos(u,sn,cs);
    shx:=sinh(v);
    chx:=cosh(v);

    x := a*shx/(chx-cs);
    y := a*sn/(chx-cs);
    z := z1;
  end;
end;
// =============================================================================
end.


