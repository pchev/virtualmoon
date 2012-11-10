//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLLinePFX<p>

   A PFX whose particles are lines

   <b>History : </b><font size=-1><ul>
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>22/04/10 - Yar - Fixes after GLState revision
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>12/10/08 - DanB - updated to use RCI
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>30/03/07 - DaStr - Added $I GLScene.inc
      <li>20/02/05 - EG - Creation
   </ul></font>
}
unit GLLinePFX;

interface

{$I GLScene.inc}

uses Classes, PersistentClasses, VectorGeometry, GLParticleFX, GLTexture,
     GLColor, GLRenderContextInfo;

type

   // TGLLineParticle
   //
   {: Linear particle.<p> }
   TGLLineParticle = class (TGLParticle)
      private
         { Private Declarations }
         FDirection : TAffineVector;
         FLength : Single;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         procedure WriteToFiler(writer : TVirtualWriter); override;
         procedure ReadFromFiler(reader : TVirtualReader); override;

         {: Direction of the line. }
         property Direction : TAffineVector read FDirection write FDirection;
         {: Length of the line }
         property Length : Single read FLength write FLength;
   end;

   // TGLLinePFXManager
   //
   {: Polygonal particles FX manager.<p>
      The particles of this manager are made of N-face regular polygon with
      a core and edge color. No texturing is available.<br>
      If you render large particles and don't have T&L acceleration, consider
      using TGLPointLightPFXManager. }
   TGLLinePFXManager = class (TGLLifeColoredPFXManager)
      private
         { Private Declarations }
         Fvx, Fvy : TAffineVector;        // NOT persistent
         FNvx, FNvy : TAffineVector;        // NOT persistent
         FDefaultLength : Single;

      protected
         { Protected Declarations }
         function StoreDefaultLength : Boolean;

         function TexturingMode : Cardinal; override;
         procedure InitializeRendering(var rci: TRenderContextInfo); override;
         procedure BeginParticles(var rci: TRenderContextInfo); override;
         procedure RenderParticle(var rci: TRenderContextInfo; aParticle : TGLParticle); override;
         procedure EndParticles(var rci: TRenderContextInfo); override;
         procedure FinalizeRendering(var rci: TRenderContextInfo); override;

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

         class function ParticlesClass : TGLParticleClass; override;
         function CreateParticle : TGLParticle; override;

	   published
	      { Published Declarations }
         property DefaultLength : Single read FDefaultLength write FDefaultLength stored StoreDefaultLength;

         property ParticleSize;
         property ColorInner;
         property ColorOuter;
         property LifeColors;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGLTokens, GLContext {$IFDEF GLS_DELPHI}, VectorTypes{$ENDIF};

// ------------------
// ------------------ TGLLinePFXManager ------------------
// ------------------

// Create
//
constructor TGLLinePFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FDefaultLength:=1;
end;

// Destroy
//
destructor TGLLinePFXManager.Destroy;
begin
   inherited Destroy;
end;

// ParticlesClass
//
class function TGLLinePFXManager.ParticlesClass : TGLParticleClass;
begin
   Result:=TGLLineParticle;
end;

// CreateParticle
//
function TGLLinePFXManager.CreateParticle : TGLParticle;
begin
   Result:=inherited CreateParticle;
   TGLLineParticle(Result).FLength:=DefaultLength;
end;

// TexturingMode
//
function TGLLinePFXManager.TexturingMode : Cardinal;
begin
   Result:=0;
end;

// InitializeRendering
//
procedure TGLLinePFXManager.InitializeRendering(var rci: TRenderContextInfo);
var
   i : Integer;
   matrix : TMatrix;
begin
   inherited;
   GL.GetFloatv(GL_MODELVIEW_MATRIX, @matrix);
   for i:=0 to 2 do begin
      Fvx[i]:=matrix[i][0];
      Fvy[i]:=matrix[i][1];
   end;
   FNvx:=VectorNormalize(Fvx);
   FNvy:=VectorNormalize(Fvy);
end;

// BeginParticles
//
procedure TGLLinePFXManager.BeginParticles(var rci: TRenderContextInfo);
begin
   ApplyBlendingMode(rci);
end;

// RenderParticle
//
procedure TGLLinePFXManager.RenderParticle(var rci: TRenderContextInfo; aParticle : TGLParticle);
var
   lifeTime, sizeScale, fx, fy, f : Single;
   inner, outer : TColorVector;
   pos, dir, start, stop, dv : TAffineVector;
begin
   lifeTime:=CurrentTime-aParticle.CreationTime;
   ComputeColors(lifeTime, inner, outer);
   if ComputeSizeScale(lifeTime, sizeScale) then
      sizeScale:=sizeScale*ParticleSize
   else sizeScale:=ParticleSize;

   pos:=aParticle.Position;

   with TGLLineParticle(aParticle) do begin
      dir:=VectorNormalize(aParticle.Velocity);
      f:=Length*0.5;
   end;

   start:=VectorCombine(pos, dir, 1, f);
   stop:=VectorCombine(pos, dir, 1, -f);

   fx:=VectorDotProduct(dir, FNvy)*sizeScale;
   fy:=-VectorDotProduct(dir, FNvx)*sizeScale;

   dv:=VectorCombine(Fvx, Fvy, fx, fy);

   GL.Begin_(GL_TRIANGLE_FAN);
      GL.Color4fv(@inner);
      GL.Vertex3fv(@start);
      GL.Color4fv(@outer);
      GL.Vertex3f(start[0]+dv[0], start[1]+dv[1], start[2]+dv[2]);
      GL.Vertex3f(stop[0]+dv[0], stop[1]+dv[1], stop[2]+dv[2]);
      GL.Color4fv(@inner);
      GL.Vertex3fv(@stop);
      GL.Color4fv(@outer);
      GL.Vertex3f(stop[0]-dv[0], stop[1]-dv[1], stop[2]-dv[2]);
      GL.Vertex3f(start[0]-dv[0], start[1]-dv[1], start[2]-dv[2]);
   GL.End_;
end;

// EndParticles
//
procedure TGLLinePFXManager.EndParticles(var rci: TRenderContextInfo);
begin
   UnapplyBlendingMode(rci);
end;

// FinalizeRendering
//
procedure TGLLinePFXManager.FinalizeRendering(var rci: TRenderContextInfo);
begin
   inherited;
end;

// StoreDefaultLength
//
function TGLLinePFXManager.StoreDefaultLength : Boolean;
begin
   Result:=(FDefaultLength<>1);
end;

// ------------------
// ------------------ TGLLineParticle ------------------
// ------------------

// WriteToFiler
//
procedure TGLLineParticle.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      Write(FDirection, SizeOf(FDirection));
      WriteFloat(FLength);
   end;
end;

// ReadFromFiler
//
procedure TGLLineParticle.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      Read(FDirection, SizeOf(FDirection));
      FLength:=ReadFloat;
   end else RaiseFilerException(archiveVersion);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   // class registrations
   RegisterClasses([TGLLineParticle, TGLLinePFXManager]);

end.
