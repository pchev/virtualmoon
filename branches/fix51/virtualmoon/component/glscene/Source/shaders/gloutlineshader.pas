{: GLOutlineShader<p>

   A simple shader that adds an outline to an object. <p>

   Limitations: <br>
     <li> 1. Object can be transparent (color alpha < 1) if it doesn't
                   overlap itself. Texture transparency doesn't work.
     <li> 2. Doesn't work with objects (e.g. TGLFreeForm) having it's own
                   color array.
     <li> 3. Doesn't Works with visible backfaces.<p>

   <b>History : </b><font size=-1><ul>
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>06/06/07 - DaStr - Added $I GLScene.inc
                             Added GLColor to uses (BugtrackerID = 1732211)
      <li>25/02/07 - DaStr - Moved registration to GLSceneRegister.pas
      <li>05/06/04 - NelC - Fixed bug with textured object
      <li>14/12/03 - NelC - Removed BlendLine, automatically determine if blend   
      <li>20/10/03 - NelC - Removed unnecessary properties. Shader now honors
                            rci.ignoreMaterials.
      <li>04/09/03 - NelC - Converted into a component from the TOutlineShader
                            in the multipass demo.
   </ul></font>
}
unit GLOutlineShader;

interface

{$I GLScene.inc}

uses
  Classes, GLMaterial, GLCrossPlatform, GLColor, GLRenderContextInfo;

type

   // TGLOutlineShader
   //
   TGLOutlineShader = class(TGLShader)
      private
         { Private Declarations }
         FPassCount : integer;
         FLineColor     : TGLColor;
         FOutlineSmooth : Boolean;
         FOutlineWidth  : Single;

         procedure SetOutlineWidth(v : single);
         procedure SetOutlineSmooth(v : boolean);

      protected
         { Protected Declarations }
         procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
         function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;

      public
         { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

      published
         { Published Declarations }
         property LineColor : TGLColor read FLineColor write FLineColor;
         {: Line smoothing control }
         property LineSmooth : Boolean read FOutlineSmooth write SetOutlineSmooth default false;
         property LineWidth : Single read FOutlineWidth write SetOutlineWidth;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses OpenGL1x, GLState;

// ------------------
// ------------------ TGLOutlineShader ------------------
// ------------------

// Create
//
constructor TGLOutlineShader.Create(AOwner : TComponent);
begin
	inherited;
   FOutlineSmooth:=False;
   FOutLineWidth:=2;
   FLineColor:=TGLColor.CreateInitialized(Self, clrBlack);
   ShaderStyle:=ssLowLevel;
end;

// Destroy
//
destructor TGLOutlineShader.Destroy;
begin
   FLineColor.Free;
   inherited;
end;

// DoApply
//
procedure TGLOutlineShader.DoApply(var rci: TRenderContextInfo; Sender : TObject);
begin
  // We first draw the object as usual in the first pass. This allows objects
  // with color alpha < 1 to be rendered correctly with outline.
  FPassCount:=1;
end;

// DoUnApply
//
function TGLOutlineShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
   if rci.ignoreMaterials then begin
      Result:=False;
      Exit;
   end;
   case FPassCount of
      1 : begin
         // Now set up to draw the outline in the second pass
         rci.GLStates.PushAttrib([sttEnable, sttCurrent, sttPolygon, sttHint,
                                  sttDepthBuffer, sttLine]);

         rci.GLStates.Disable(stLighting);

         if FOutlineSmooth then begin
            rci.GLStates.LineSmoothHint := hintNicest;
            rci.GLStates.Enable(stLineSmooth);
         end else begin
            rci.GLStates.Disable(stLineSmooth);
         end;

         if FOutlineSmooth or (FlineColor.Alpha<1) then begin
            rci.GLStates.Enable(stBlend);
            rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
         end else begin
            rci.GLStates.Disable(stBlend);
         end;

         glColor4fv(FlineColor.AsAddress);

         rci.GLStates.LineWidth := FOutlineWidth;
         rci.GLStates.PolygonMode := pmLines;
         rci.GLStates.CullFaceMode := cmFront;
         rci.GLStates.DepthFunc := cfLEqual;

         with rci.GLStates do begin
           Disable(stTexture2D);
           Disable(stTextureCubeMap);
         end;

         FPassCount:=2;
         Result:=True;  // go for next pass
      end;
      2 : begin
         // Restore settings
         rci.GLStates.PopAttrib;
         Result:=False; // we're done
      end;
   else
      Assert(False);
      Result:=False;
   end;
end;

// SetOutlineWidth
//
procedure TGLOutlineShader.SetOutlineWidth(v: single);
begin
   if FOutlineWidth<>v then begin
      FOutlineWidth:=v;
      NotifyChange(self);
   end;
end;

// SetOutlineSmooth
//
procedure TGLOutlineShader.SetOutlineSmooth(v: boolean);
begin
   if FOutlineSmooth<>v then begin
      FOutlineSmooth:=v;
      NotifyChange(self);
   end;
end;

end.

