unit glgizmoutils;

interface

uses
  opengl1x, glscene, vectorgeometry, contnrs;

(* 

// Copyright 2001, softSurfer (www.softsurfer.com)
// This code may be freely used and modified for any purpose
// providing that this copyright notice is included with it.
// SoftSurfer makes no warranty for this code, and cannot be held
// liable for any real or imagined damage resulting from its use.
// Users of this code must verify correctness for their application.


// Assume that classes are already given for the objects:
//    Point and Vector with
//        coordinates {float x, y;}
//        operators for:
//            Point  = Point ± Vector
//            Vector = Point - Point
//            Vector = Vector ± Vector
//            Vector = Scalar * Vector    (scalar product)
//            Vector = Vector / Scalar    (scalar division)
//    Ball with a center and radius {Point center; float radius;}
//===================================================================


// dot product which allows vector operations in arguments
#define dot(u,v)   ((u).x * (v).x + (u).y * (v).y)
#define norm2(v)   dot(v,v)        // norm2 = squared length of vector
#define norm(v)    sqrt(norm2(v))  // norm = length of vector
#define d(u,v)     norm(u-v)       // distance = norm of difference


// fastBall(): a fast approximation of the bounding ball for a point set
//               based on the algorithm given by [Jack Ritter, 1990]
//    Input:  an array V[] of n points
//    Output: a bounding ball = {Point center; float radius;}
void
fastBall( Point V[], int n, Ball* B)
{
    Point C;                           // Center of ball
    float rad, rad2;                   // radius and radius squared
    float xmin, xmax, ymin, ymax;      // bounding box extremes
    int   Pxmin, Pxmax, Pymin, Pymax;  // index of V[] at box extreme

    // find a large diameter to start with
    // first get the bounding box and V[] extreme points for it
    xmin = xmax = V[0].x;
    ymin = ymax = V[0].y;
    Pxmin = Pxmax = Pymin = Pymax = 0;
    for (int i=1; i<n; i++) {
        if (V[i].x < xmin) {
            xmin = V[i].x;
            Pxmin = i;
        }
        else if (V[i].x > xmax) {
            xmax = V[i].x;
            Pxmax = i;
        }
        if (V[i].y < ymin) {
            ymin = V[i].y;
            Pymin = i;
        }
        else if (V[i].y > ymax) {
            ymax = V[i].y;
            Pymax = i;
        }
    }
    // select the largest extent as an initial diameter for the ball
    Vector dVx = V[Pxmax] - V[Pxmin]; // diff of Vx max and min
    Vector dVy = V[Pymax] - V[Pymin]; // diff of Vy max and min
    float dx2 = norm2(dVx); // Vx diff squared
    float dy2 = norm2(dVy); // Vy diff squared
    if (dx2 >= dy2) {                     // x direction is largest extent
        C = V[Pxmin] + (dVx / 2.0);         // Center = midpoint of extremes
        rad2 = norm2(V[Pxmax] - C);         // radius squared
    }
    else {                                // y direction is largest extent
        C = V[Pymin] + (dVy / 2.0);         // Center = midpoint of extremes
        rad2 = norm2(V[Pymax] - C);         // radius squared
    }
    rad = sqrt(rad2);

    // now check that all points V[i] are in the ball
    // and if not, expand the ball just enough to include them
    Vector dV;
    float dist, dist2;
    for (int i=0; i<n; i++) {
        dV = V[i] - C;
        dist2 = norm2(dV);
        if (dist2 <= rad2)    // V[i] is inside the ball already
            continue;
        // V[i] not in ball, so expand ball to include it
        dist = sqrt(dist2);
        rad = (rad + dist) / 2.0;         // enlarge radius just enough
        rad2 = rad * rad;
        C = C + ((dist-rad)/dist) * dV;   // shift Center toward V[i]
    }
    B->Center = C;
    B->radius = rad;
    return;
}

*)

procedure FastBall(AObjects: TObjectList; var APosition: TVector; var ARadius: TGlFloat);



implementation

procedure FastBall(AObjects: TObjectList; var APosition: TVector; var ARadius: TGlFloat);
var
  C: TVector;
  rad, rad2: TGLFloat;                   // radius and radius squared
  min, max: TVector;

//    float xmin, xmax, ymin, ymax;      // bounding box extremes
  Pxmin, Pxmax,
  Pymin, Pymax,
  Pzmin, pzmax: Integer;  // index of V[] at box extreme
  dx, dy, dz: TGLFloat;
  i: Integer;

  dV,
  dVx,
  dVy,
  dVz: TVector;

  dist, dist2: TGLFloat;
begin
  if AObjects.Count = 0 then
    Exit;

  // first get the bounding box and V[] extreme points for it
  // find a large diameter to start with
  min := (AObjects[0] as TGLBaseSceneObject).AbsolutePosition;

  Pxmin := 0;
  Pxmax := 0;
  Pymin := 0;
  Pymax := 0;
  Pzmin := 0;
  Pzmax := 0;

  for i := 1 to AObjects.Count - 1 do
  begin
    if ((AObjects[i] as TGLBaseSceneObject).AbsolutePosition[0] < min[0]) then
    begin
      min[0] := (AObjects[i] as TGLBaseSceneObject).AbsolutePosition[0];
      Pxmin  := i;
    end
    else if ((AObjects[i] as TGLBaseSceneObject).AbsolutePosition[0] > max[0]) then
    begin
      max[0] := (AObjects[i] as TGLBaseSceneObject).AbsolutePosition[0];
      Pxmax  := i;
    end;

    if ((AObjects[i] as TGLBaseSceneObject).AbsolutePosition[1] < min[1]) then
    begin
      min[1] := (AObjects[i] as TGLBaseSceneObject).AbsolutePosition[1];
      Pymin  := i;
    end
    else if ((AObjects[i] as TGLBaseSceneObject).AbsolutePosition[1] > max[1]) then
    begin
      max[1] := (AObjects[i] as TGLBaseSceneObject).AbsolutePosition[1];
      Pymax  := i;
    end;

    if ((AObjects[i] as TGLBaseSceneObject).AbsolutePosition[2] < min[2]) then
    begin
      min[2] := (AObjects[i] as TGLBaseSceneObject).AbsolutePosition[2];
      Pzmin  := i;
    end
    else if ((AObjects[i] as TGLBaseSceneObject).AbsolutePosition[2] > max[2]) then
    begin
      max[2] := (AObjects[i] as TGLBaseSceneObject).AbsolutePosition[2];
      Pzmax  := i;
    end;
  end;

  // select the largest extent as an initial diameter for the ball
  dVx := VectorSubtract((AObjects[Pxmax] as TGLBaseSceneObject).AbsolutePosition,
           (AObjects[Pxmin] as TGLBaseSceneObject).AbsolutePosition);

  dVy := VectorSubtract((AObjects[Pymax] as TGLBaseSceneObject).AbsolutePosition,
           (AObjects[Pymin] as TGLBaseSceneObject).AbsolutePosition);

  dVz := VectorSubtract((AObjects[Pzmax] as TGLBaseSceneObject).AbsolutePosition,
           (AObjects[Pzmin] as TGLBaseSceneObject).AbsolutePosition);

  dx := VectorLength(dVx);
  dy := VectorLength(dVy);
  dz := VectorLength(dVz);

  if (dx >= dy) then                     // x direction is largest extent
  begin
    if (dx >= dz) then
    begin
      C := VectorAdd((AObjects[Pxmin] as TGLBaseSceneObject).AbsolutePosition, VectorScale(dVx, 1/2));         // Center = midpoint of extremes
      rad := VectorLength(VectorSubtract((AObjects[Pxmin] as TGLBaseSceneObject).AbsolutePosition, C));         // radius squared
    end
    else
    begin
      C := VectorAdd((AObjects[Pzmin] as TGLBaseSceneObject).AbsolutePosition, VectorScale(dVz, 1/2));         // Center = midpoint of extremes
      rad := VectorLength(VectorSubtract((AObjects[Pzmin] as TGLBaseSceneObject).AbsolutePosition, C));         // radius squared
    end;
  end
  else                                 // y direction is largest extent
  begin
    if (dy >= dz) then
    begin
      C := VectorAdd((AObjects[Pymin] as TGLBaseSceneObject).AbsolutePosition, VectorScale(dVy, 1/2));         // Center = midpoint of extremes
      rad := VectorLength(VectorSubtract((AObjects[Pymin] as TGLBaseSceneObject).AbsolutePosition, C));         // radius squared
    end
    else
    begin
      C := VectorAdd((AObjects[Pzmin] as TGLBaseSceneObject).AbsolutePosition, VectorScale(dVz, 1/2));         // Center = midpoint of extremes
      rad := VectorLength(VectorSubtract((AObjects[Pzmin] as TGLBaseSceneObject).AbsolutePosition, C));         // radius squared
    end;
  end;

  rad2 := sqr(rad);


  // now check that all points V[i] are in the ball
  // and if not, expand the ball just enough to include them
  for i := 0 to AObjects.Count - 1 do
  begin
    dV := VectorSubtract((AObjects[i] as TGLBaseSceneObject).AbsolutePosition, C);
    dist2 := sqr(VectorLength(dV));
    if (dist2 <= rad2) then   // V[i] is inside the ball already
        continue;
    // V[i] not in ball, so expand ball to include it
    dist := sqrt(dist2);
    rad := (rad + dist) / 2.0;         // enlarge radius just enough
    rad2 := rad * rad;
    C := VectorAdd(C, VectorScale(dV, ((dist-rad)/dist)));   // shift Center toward V[i]
  end;
  APosition := C;
  ARadius := rad;
end;

end.
