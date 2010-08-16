//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSceneRegister<p>

   Registration unit for GLScene library components, property editors and
      IDE experts.<p>

	<b>History : </b><font size=-1><ul>
      <li>14/03/09 - DanB - Split TObjectManager to GLObjectManager.pas.  Moved property
                            editors to interface section, and made their methods public.
      <li>08/10/08 - DanB - Added DynamicTexture unit (to allow choosing this at designtime)
                            + register TGLSLPostBlurShader
      <li>05/10/08 - DanB - Change required due Texture/TextureImageEditor separation
                            + GLMisc split, tidied up some old ifdefs
      <li>24/03/08 - DaStr - Moved TGLMinFilter and TGLMagFilter from GLUtils.pas
                              to GLGraphics.pas (BugTracker ID = 1923844)  
      <li>21/03/08 - DaStr - Renamed TMMat to TGLTextureSharingShaderMaterial 
      <li>17/03/08 - mrqzzz - Registered TGLTextureSharingShader
      <li>20/01/08 - DaStr - Registered TGLCapsule (thanks Dave Gravel)
                             Registered TGLGizmo
      <li>06/11/07 - mrqzzz - Registered material picker for TGLActorProxy
      <li>18/09/07 - DaStr - Added TGLMaterialProxy, TGLAbsoluteHUDText,
                              TGLResolutionIndependantHUDText
      <li>12/07/07 - DaStr - Improved Cross-Platform compatibility
                             (Bugtracker ID = 1684432)
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
      <li>23/03/07 - fig - Added TGLSLProjectedTextures
      <li>17/03/07 - DaStr - Dropped Kylix support in favor of FPC (BugTrackerID=1681585)
      <li>14/03/07 - DaStr - SpriteAnimation now makes use of
                                         TGLLibMaterialName's property editor
      <li>04/03/07 - DaStr - Added TGLPostShaderHolder
      <li>28/02/07 - LIN   - Added GLShadowHDS
      <li>25/02/07 - DaStr - Added TGLPostEffect
                             Moved all terrain components to a separate tab
                             Moved all shader components registration here
      <li>23/02/07 - DaStr - Added TGLSLShader, TGLSLDiffuseSpecularShader,
                             TGLSLBumpShader, TGLAsmShader, TGLShaderCombiner
                             TGLSmoothNavigator, TGLSmoothUserInterface
                             Moved TGLLibMaterialNameProperty to the interface
                                                                        section
      <li>21/02/07 - DaStr - Added TGLActorProxy and TGLMotionBlur
      <li>16/02/07 - DaStr - Added GLMaterialMultiProxy
      <li>15/02/07 - DaStr - Added GLConsole and GLAtmosphere
      <li>13/02/07 - LIN   - Added GLAsyncHDS and GLTexturedHDS
      <li>06/02/07 - DaStr - Added GLSimpleNavigation
      <li>29/01/07 - DaStr - Added GLEParticleMasksManager, moved registration
                               procedures from other units to this one
      <li>21/01/07 - DaStr - TGLLibMaterialNameProperty.Edit fixed
                                   (to support IGLMaterialLibrarySupported)
      <li>23/12/04 - PhP - "Animated Sprite" moved to advanced objects category
      <li>13/10/04 - MRQZZZ - Added GLTrail
      <li>03/07/04 - LR - Completly review to take account designtime for Linux
                          Note a problem with TGLColorProperty
      <li>28/06/04 - LR - Changed LoadBitmap to GLLoadBitmapFromInstance
      <li>12/04/04 - EG - LibMaterialName property editor for SkyBox
      <li>22/08/02 - EG - RegisterPropertiesInCategory (Robin Gerrets)
      <li>08/04/02 - EG - Added verb to TGLSceneEditor
      <li>26/01/02 - EG - Color property drawing in D6 too now
      <li>22/08/01 - EG - D6 related changes
      <li>08/07/01 - EG - Register for TGLExtrusionSolid (Uwe Raabe)
      <li>18/02/01 - EG - Added Terrain/HeightData objects
      <li>21/01/01 - EG - Enhanced GetAttributes for some property editors
      <li>09/10/00 - EG - Added registration for TGLMultiPolygon
      <li>09/06/00 - EG - Added TSoundFileProperty & TSoundNameProperty
      <li>23/05/00 - EG - Added GLCollision
      <li>16/05/00 - EG - Delphi 4 Compatibility
      <li>28/04/00 - EG - Removed ObjectStock in TObjectManager (was useless)
      <li>26/04/00 - EG - Added Categories in ObjectManager,
                          enhanced GetRegisteredSceneObjects
      <li>16/04/00 - EG - Objects icons are now loaded from ressources using
                          ClassName (more VCL-like)
      <li>11/04/00 - EG - Components now install under 'GLScene',
                          Fixed DestroySceneObjectList (thanks Uwe Raabe)
      <li>06/04/00 - EG - Added TGLBehavioursProperty
      <li>18/03/00 - EG - Added TGLImageClassProperty
      <li>13/03/00 - EG - Updated TGLTextureImageProperty
      <li>14/02/00 - EG - Added MaterialLibrary editor and picker
      <li>09/02/00 - EG - ObjectManager moved in, ObjectManager is now fully
                          object-oriented and encapsulated
      <li>06/02/00 - EG - Fixed TGLScenedEditor logic
                          (was causing Delphi IDE crashes on package unload)
      <li>05/02/00 - EG - Added TGLColorProperty and TGLCoordinatesProperty
	</ul></font>
}
unit GLSceneRegister;

// Registration unit for GLScene library
// 30-DEC-99 ml: scene editor added, structural changes

interface

{$i GLScene.inc}

uses
   {$ifdef windows}windows,{$endif}
   Classes, Controls, StdCtrls, Graphics,
   lresources, propedits, lclintf, ComponentReg, componenteditors,
   GLScene, GLColor, GLCrossPlatform, GLObjectManager;


type

	// TGLLibMaterialNameProperty
	//
	TGLLibMaterialNameProperty = class(TStringProperty)
		public
			{ Public Declarations }
         function GetAttributes: TPropertyAttributes; override;
			procedure Edit; override;
	end;

   // TGLSceneViewerEditor
   //
   TGLSceneViewerEditor = class(TComponentEditor)
      public
         { Public Declarations }
         procedure ExecuteVerb(Index: Integer); override;
         function GetVerb(Index: Integer): String; override;
         function GetVerbCount: Integer; override;
   end;

   // TGLSceneEditor
   //
   TGLSceneEditor = class (TComponentEditor)
      public
         { Public Declarations }
         procedure Edit; override;

         procedure ExecuteVerb(Index: Integer); override;
         function GetVerb(Index: Integer): String; override;
         function GetVerbCount: Integer; override;
   end;

   // TResolutionProperty
   //
   TResolutionProperty = class (TPropertyEditor)
      public
         { Public Declarations }
         function GetAttributes: TPropertyAttributes; override;
         function GetValue : String; override;
         procedure GetValues(Proc: TGetStrProc); override;
         procedure SetValue(const Value: String); override;
   end;

   // TClassProperty
   //
   TGLTextureProperty = class (TClassProperty)
      public
         { Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
   end;

   // TGLTextureImageProperty
   //
   TGLTextureImageProperty = class(TClassProperty)
		public
         { Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
         procedure Edit; override;
   end;

   // TGLImageClassProperty
   //
   TGLImageClassProperty = class(TClassProperty)
		public
			{ Public Declarations }
         function GetAttributes : TPropertyAttributes; override;
         procedure GetValues(proc : TGetStrProc); override;
        function GetValue : String; override;
        procedure SetValue(const value : String); override;
   end;


   TGLColorProperty = class (TClassProperty)
      private
        { Private Declarations }

      protected
        { Protected Declarations }
         function ColorToBorderColor(aColor: TColorVector; selected : Boolean) : TColor;

      public
	function GetAttributes: TPropertyAttributes; override;
	procedure GetValues(Proc: TGetStrProc); override;
	procedure Edit; override;

(*	      {$ifdef GLS_COMPILER_5}
	      procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); override;
	      procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); override;
	      {$endif}
	      {$ifdef GLS_COMPILER_6_UP}
         // Well, i don't know why it's doesn't work with Kylix ...
         {$ifdef WIN32}
         // ICustomPropertyListDrawing  stuff
         procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
         procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
         procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TGLRect; ASelected: Boolean);
         // CustomPropertyDrawing
         procedure PropDrawName(ACanvas: TCanvas; const ARect: TGLRect; ASelected: Boolean);
         procedure PropDrawValue(ACanvas: TCanvas; const ARect: TGLRect; ASelected: Boolean);
         {$endif}
         {$endif}
*)
        function GetValue: String; override;
	procedure SetValue(const Value: string); override;
   end;

   // TVectorFileProperty
   //
   TVectorFileProperty = class (TClassProperty)
      public
         { Public Declarations }
         function GetAttributes: TPropertyAttributes; override;
         function GetValue: String; override;
         procedure Edit; override;
         procedure SetValue(const Value: string); override;
   end;

   // TSoundFileProperty
   //
   TSoundFileProperty = class (TClassProperty)
      public
         { Public Declarations }
         function GetAttributes : TPropertyAttributes; override;
         function GetValue: String; override;
         procedure Edit; override;
   end;

   // TSoundNameProperty
   //
   TSoundNameProperty = class (TStringProperty)
      public
         { Public Declarations }
         function GetAttributes : TPropertyAttributes; override;
      	procedure GetValues(Proc: TGetStrProc); override;
   end;


   // TGLCoordinatesProperty
   //
   TGLCoordinatesProperty = class(TClassProperty)
      public
         { Public Declarations }
         function GetAttributes: TPropertyAttributes; override;
         procedure Edit; override;
   end;

   // TGLMaterialProperty
   //
   TGLMaterialProperty = class(TClassProperty)
		public
			{ Public Declarations }
         function GetAttributes: TPropertyAttributes; override;
         procedure Edit; override;
   end;

   // TReuseableDefaultEditor
   //
   {: Editor copied from DsgnIntf.<p>
      Could have been avoided, if only that guy at Borland didn't chose to
      publish only half of the stuff (and that's not the only class with
      that problem, most of the subitems handling code in TGLSceneBaseObject is
      here for the same reason...), the "protected" wasn't meant just to lure
      programmers into code they can't reuse... Arrr! and he did that again
      in D6! Grrr... }
{$ifdef GLS_DELPHI_6_UP}
   TReuseableDefaultEditor = class (TComponentEditor, IDefaultEditor)
{$else}
   TReuseableDefaultEditor = class (TComponentEditor)
{$endif}
      protected
			{ Protected Declarations }
{$ifdef GLS_DELPHI_6_UP}
         FFirst: IProperty;
         FBest: IProperty;
         FContinue: Boolean;
         procedure CheckEdit(const Prop : IProperty);
         procedure EditProperty(const Prop: IProperty; var Continue: Boolean); virtual;
{$else}
         FFirst: TPropertyEditor;
         FBest: TPropertyEditor;
         FContinue: Boolean;
         procedure CheckEdit(PropertyEditor : TPropertyEditor);
         procedure EditProperty(PropertyEditor : TPropertyEditor;
                                var Continue, FreeEditor : Boolean); virtual;
{$endif}

      public
         { Public Declarations }
         procedure Edit; override;
   end;

   // TGLMaterialLibraryEditor
   //
   {: Editor for material library.<p> }
   TGLMaterialLibraryEditor = class(TReuseableDefaultEditor{$ifdef GLS_DELPHI_6_UP}, IDefaultEditor{$endif})
      protected
{$ifdef GLS_DELPHI_6_UP}
         procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
{$else}
         procedure EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean); override;
{$endif}
	end;

   // TGLAnimationNameProperty
   //
   TGLAnimationNameProperty = class(TStringProperty)
		public
			{ Public Declarations }
         function GetAttributes : TPropertyAttributes; override;
         procedure GetValues(proc : TGetStrProc); override;
   end;

resourcestring
   { OpenGL property category }
   sOpenGLCategoryName = 'OpenGL';


procedure Register;

//: Auto-create for object manager
function ObjectManager : TObjectManager;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
   Dialogs, TypInfo, VectorGeometry, GLTexture, SysUtils, GLStrings,
   GLObjects, GLVectorFileObjects, GLExtrusion, GLMultiPolygon, GLMesh, GLPortal,
   GLGraph, GLParticles, GLHUDObjects, GLSkydome, GLBitmapFont, GLLensFlare,
   GLMirror, GLParticleFX, GLShadowPlane, GLTerrainRenderer, GLShadowVolume,
   GLTeapot, GLPolyhedron, GLGeomObjects, GLTextureImageEditors, GLMultiProxy,
   GLSkyBox, GLState, GLUtils, GLTilePlane, GLTree, GLImposter, GLWaterPlane,
   GLPerlinPFX, GLTexLensFlare, GLFireFX, GLThorFX, GLSceneEdit, FVectorEditor,
   GLCadencer, GLCollision, GLHeightData, GLzBuffer, GLGui, GLBumpmapHDS,
   AsyncTimer, GLWindows, GLWindowsFont, GLHeightTileFileHDS, GLTexturedHDS,
   GLAnimatedSprite, GLFeedback, GLProjectedTextures, GLBlur, GLTrail, GLPerlin,
   GLLinePFX, GLScriptBase, GLGameMenu, GLEParticleMasksManager, {GLAVIRecorder,}
   GLTimeEventsMgr, GLNavigator, GLMaterialScript, GLFPSMovement, GLDCE,
   ApplicationFileIO,  GLScreen, GLVfsPAK, GLSimpleNavigation,
   GLAsyncHDS, GLConsole, GLAtmosphere, GLProxyObjects, GLMaterialMultiProxy,
   GLSLShader, GLSLDiffuseSpecularShader, GLSLBumpShader, GLAsmShader,
   GLShaderCombiner, GLSmoothNavigator, GLPostEffects, GLPhongShader,
   GLTexCombineShader, GLCelShader, GLOutlineShader, GLMultiMaterialShader,
   GLBumpShader, GLHiddenLineShader, GLUserShader, GLShadowHDS, GLSLProjectedTextures,
   GLViewer, GLGizmo, GLTextureSharingShader, GLGraphics, GLCoordinates,
   GLRenderContextInfo, GLNodes,
   GLMaterial, GLDynamicTexture, GLSLPostBlurShader, BaseClasses, GLExplosionFx,
   GLCameraController, GLFBORenderer,
   GLSound, GLSoundFileObjects,
   {}
   // Image file formats
   {Uses ScanLine, needs fixing: DDS,} TGA,
   // Vector file formats
   GLFile3DS, GLFileASE, GLFileB3D, GLFileGL2, GLFileGTS, GLFileLMTS,
   GLFileLWO, GLFileMD2, GLFileMD3, GLFileMD5, GLFileMDC, GLFileMS3D, GLFileNMF,
   GLFileNurbs, GLFileObj, {Uses ScanLine, needs fixing: GLFileOCT,} GLFilePLY, GLFileQ3BSP, GLFileSMD, GLFileSTL,
   GLFileTIN, GLFileVRML

   {$ifdef windows}
     ,gllclfullscreenviewer, GLSpaceText, Joystick, ScreenSaver
     ,GLWideBitmapFont //  does it in win now ?
     //GLAVIRecorder, doesn't even compile in win due to missing commDlg
   {$endif}
   {,glsdlcontext,glscriptbase,}
   ;

var
   vObjectManager : TObjectManager;

function ObjectManager : TObjectManager;
begin
   if not Assigned(vObjectManager) then
      vObjectManager:=TObjectManager.Create(nil);
   Result:=vObjectManager;
end;


//----------------- TGLSceneViewerEditor ---------------------------------------

// ExecuteVerb
//
procedure TGLSceneViewerEditor.ExecuteVerb(Index : Integer);
var
   source : TGLSceneViewer;
begin
  source:=Component as TGLSceneViewer;
  case Index of
    0 : source.Buffer.ShowInfo;
  end;
end;

// GetVerb
//
function TGLSceneViewerEditor.GetVerb(Index : Integer) : String;
begin
  case Index of
    0 : Result:='Show context info';
  end;
end;

// GetVerbCount
//
function TGLSceneViewerEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

//----------------- TGLSceneEditor ---------------------------------------------

// Edit
//
procedure TGLSceneEditor.Edit;
begin
   with GLSceneEditorForm do begin
      SetScene(Self.Component as TGLScene, Self.Designer);
      Show;
   end;
end;

// ExecuteVerb
//
procedure TGLSceneEditor.ExecuteVerb(Index : Integer);
begin
   case Index of
      0 : Edit;
   end;
end;

// GetVerb
//
function TGLSceneEditor.GetVerb(Index : Integer) : String;
begin
   case Index of
      0 : Result:='Show Scene Editor';
   end;
end;

// GetVerbCount
//
function TGLSceneEditor.GetVerbCount: Integer;
begin
   Result:=1;
end;

//----------------- TResolutionProperty ----------------------------------------

// GetAttributes
//
function TResolutionProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paValueList];
end;

// GetValue
//
function TResolutionProperty.GetValue : String;
begin
{$IFDEF MSWINDOWS}
   Result:=vVideoModes[GetOrdValue].Description;
{$ELSE}
  //Testing!!!
   with vVideoModes[GetOrdValue]^ do
     Result:=Inttostr(hdisplay)+' x '+Inttostr(vdisplay)+', '+'0 bpp';
{$ENDIF}
end;

// GetValues
//
procedure TResolutionProperty.GetValues(Proc: TGetStrProc);
var
   i : Integer;
begin
{$IFDEF MSWINDOWS}
   for i:=0 to vNumberVideoModes-1 do
      Proc(vVideoModes[i].Description);
{$ELSE}
   for i:=0 to vNumberVideoModes-1 do
      with vVideoModes[i]^ do
      Proc(Inttostr(hdisplay)+'x'+Inttostr(vdisplay)+'x'+'0');
{$ENDIF}
end;

// SetValue
//
procedure TResolutionProperty.SetValue(const Value: String);

const Nums = ['0'..'9'];

var XRes,YRes,BPP : Integer;
    Pos, SLength  : Integer;
    TempStr       : String;

begin
  if CompareText(Value,'default') <> 0 then
  begin
    // initialize scanning
    TempStr:=Trim(Value)+'|'; // ensure at least one delimiter
    SLength:=Length(TempStr);
    XRes:=0; YRes:=0; BPP:=0;
    // contains the string something?
    if SLength > 1 then
    begin
      // determine first number
      for Pos:=1 to SLength do
        if not (TempStr[Pos] in Nums) then Break;
      if Pos <= SLength then
      begin
        // found a number?
        XRes:=StrToInt(Copy(TempStr,1,Pos-1));
        // search for following non-numerics
        for Pos:=Pos to SLength do
          if TempStr[Pos] in Nums then Break;
        Delete(TempStr,1,Pos-1); // take it out of the String
        SLength:=Length(TempStr); // rest length of String
        if SLength > 1 then // something to scan?
        begin
          // determine second number
          for Pos:=1 to SLength do
            if not (TempStr[Pos] in Nums) then Break;
          if Pos <= SLength then
          begin
            YRes:=StrToInt(Copy(TempStr,1,Pos-1));
            // search for following non-numerics
            for Pos:=Pos to SLength do
              if TempStr[Pos] in Nums then Break;
            Delete(TempStr,1,Pos-1); // take it out of the String
            SLength:=Length(TempStr); // rest length of String
            if SLength > 1 then
            begin
              for Pos:=1 to SLength do
                if not (TempStr[Pos] in Nums) then Break;
              if Pos <= SLength then BPP:=StrToInt(Copy(TempStr,1,Pos-1));
            end;
          end;
        end;
      end;
    end;
    SetOrdValue(GetIndexFromResolution(XRes,YRes,BPP));
  end
  else SetOrdValue(0);
end;

//----------------- TGLTextureProperty -----------------------------------------

function TGLTextureProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paSubProperties];
end;

//----------------- TGLTextureImageProperty ------------------------------------

// GetAttributes
//
function TGLTextureImageProperty.GetAttributes: TPropertyAttributes;
begin
	Result:=[paDialog];
end;

// Edit
//
procedure TGLTextureImageProperty.Edit;
begin
 if EditGLTextureImage(TGLTextureImage(GetOrdValue)) then
  {Designer.}Modified;
end;

//----------------- TGLImageClassProperty --------------------------------------

// GetAttributes
//
function TGLImageClassProperty.GetAttributes: TPropertyAttributes;
begin
	Result:=[paValueList];
end;

// GetValues
//
procedure TGLImageClassProperty.GetValues(proc: TGetStrProc);
var
	i : Integer;
	sl : TStrings;
begin
	sl:=GetGLTextureImageClassesAsStrings;
	try
		for i:=0 to sl.Count-1 do proc(sl[i]);
	finally
		sl.Free;
	end;
end;

// GetValue
//
function TGLImageClassProperty.GetValue : String;
begin
	Result:=FindGLTextureImageClass(GetStrValue).FriendlyName;
end;

// SetValue
//
procedure TGLImageClassProperty.SetValue(const value : String);
var
	tic : TGLTextureImageClass;
begin
	tic:=FindGLTextureImageClassByFriendlyName(value);
	if Assigned(tic) then
		SetStrValue(tic.ClassName)
	else SetStrValue('');
	Modified;
end;

//----------------- TGLColorproperty -----------------------------------------------------------------------------------

procedure TGLColorProperty.Edit;
var
   colorDialog : TColorDialog;
   glColor : TGLColor;
begin
   colorDialog:=TColorDialog.Create(nil);
   try
      glColor:=TGLColor(GetObjectValue);
      {$IFNDEF FPC}{$ifdef WIN32}
      colorDialog.Options:=[cdFullOpen];
      {$endif}{$ENDIF}
      colorDialog.Color:=ConvertColorVector(glColor.Color);
      if colorDialog.Execute then begin
         glColor.Color:=ConvertWinColor(colorDialog.Color);
         Modified;
      end;
   finally
      colorDialog.Free;
   end;
end;

function TGLColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paSubProperties, paValueList, paDialog];
end;

procedure TGLColorProperty.GetValues(Proc: TGetStrProc);
begin
  ColorManager.EnumColors(Proc);
end;

function TGLColorProperty.GetValue: String;
begin
  Result:=ColorManager.GetColorName(TGLColor(GetObjectValue).Color);
end;

procedure TGLColorProperty.SetValue(const Value: string);
begin
  TGLColor(GetObjectValue).Color:=ColorManager.GetColor(Value);
  Modified;
end;

// ColorToBorderColor
//
function TGLColorProperty.ColorToBorderColor(aColor: TColorVector; selected : Boolean) : TColor;
begin
   if (aColor[0]>0.75) or (aColor[1]>0.75) or (aColor[2]>0.75) then
      Result:=clBlack
   else if selected then
      Result:=clWhite
   else Result:=ConvertColorVector(AColor);
end;

(* CRB ??
{$ifdef GLS_COMPILER_5}
procedure TGLColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
                                         const ARect: TRect; ASelected: Boolean);
var
   vRight: Integer;
   vOldPenColor,
   vOldBrushColor: TColor;
   Color: TColorVector;
begin
   vRight:=(ARect.Bottom - ARect.Top) + ARect.Left;
   with ACanvas do try
      vOldPenColor:=Pen.Color;
      vOldBrushColor:=Brush.Color;

      Pen.Color:=Brush.Color;
      Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);

      Color:=ColorManager.GetColor(Value);
      Brush.Color:=ConvertColorVector(Color);
      Pen.Color:=ColorToBorderColor(Color, ASelected);

      Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);

      Brush.Color:=vOldBrushColor;
      Pen.Color:=vOldPenColor;
   finally
      inherited ListDrawValue(Value, ACanvas, Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom), ASelected);
   end;
end;

procedure TGLColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
   // draws the small color rectangle in the object inspector
   if GetVisualValue<>'' then
      ListDrawValue(GetVisualValue, ACanvas, ARect, True)
   else inherited PropDrawValue(ACanvas, ARect, ASelected);
end;
{$endif}

{$ifdef GLS_COMPILER_6_UP}
{$ifdef WIN32}
procedure TGLColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
   if GetVisualValue <> '' then
      ListDrawValue(GetVisualValue, ACanvas, ARect, True)
   else DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TGLColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
                                         const ARect: TRect; ASelected: Boolean);
var
   vRight: Integer;
   vOldPenColor,
   vOldBrushColor: TColor;
   Color: TColorVector;
begin
   vRight:=(ARect.Bottom - ARect.Top) + ARect.Left;
   with ACanvas do try
      vOldPenColor:=Pen.Color;
      vOldBrushColor:=Brush.Color;

      Pen.Color:=Brush.Color;
      Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);

      Color:=ColorManager.GetColor(Value);
      Brush.Color:=ConvertColorVector(Color);
      Pen.Color:=ColorToBorderColor(Color, ASelected);

      Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);

      Brush.Color:=vOldBrushColor;
      Pen.Color:=vOldPenColor;
   finally
      DefaultPropertyListDrawValue(Value, ACanvas, Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom),
                                   ASelected);
   end;
end;

procedure TGLColorProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
   AWidth := AWidth + ACanvas.TextHeight('M');
end;

procedure TGLColorProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
   // Nothing
end;

procedure TGLColorProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
   DefaultPropertyDrawName(Self, ACanvas, ARect);
end;
{$endif WIN32}
{$endif GLS_COMPILER_6_UP}
{$endif GLS_COMPILER_5_UP}
*)
//----------------- TVectorFileProperty ----------------------------------------

// GetAttributes
//
function TVectorFileProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog];
end;

// GetValue
//
function TVectorFileProperty.GetValue: String;
begin
   Result:=GetStrValue;
end;

// Edit
//
procedure TVectorFileProperty.Edit;
var
   ODialog   : TOpenDialog;
   Component : TGLFreeForm;
   Desc, F    : String;
begin
   Component:=GetComponent(0) as TGLFreeForm;
   ODialog:=TOpenDialog.Create(nil);
   try
      GetVectorFileFormats.BuildFilterStrings(TVectorFile, Desc, F);
      ODialog.Filter:=Desc;
      if ODialog.Execute then begin
         Component.LoadFromFile(ODialog.FileName);
         Modified;
      end;
   finally
      ODialog.Free;
   end;
end;

// SetValue
//
procedure TVectorFileProperty.SetValue(const Value: string);
begin
   SetStrValue(Value);
end;

//----------------- TSoundFileProperty -----------------------------------------

// GetAttributes
//
function TSoundFileProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog];
end;

// GetValue
//
function TSoundFileProperty.GetValue: String;
var
   sample : TGLSoundSample;
begin
   sample:=GetComponent(0) as TGLSoundSample;
   if sample.Data<>nil then
      Result:='('+sample.Data.ClassName+')'
   else Result:='(empty)';
end;

// Edit
//
procedure TSoundFileProperty.Edit;
var
   ODialog   : TOpenDialog;
   sample : TGLSoundSample;
   Desc, F    : String;
begin
   sample:=GetComponent(0) as TGLSoundSample;
   ODialog:=TOpenDialog.Create(nil);
   try
      GetGLSoundFileFormats.BuildFilterStrings(TGLSoundFile, Desc, F);
      ODialog.Filter:=Desc;
      if ODialog.Execute then begin
         sample.LoadFromFile(ODialog.FileName);
         Modified;
      end;
   finally
      ODialog.Free;
   end;
end;

//----------------- TSoundNameProperty -----------------------------------------

// GetAttributes
//
function TSoundNameProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paValueList];
end;

// GetValues
//
procedure TSoundNameProperty.GetValues(Proc: TGetStrProc);
var
   i : Integer;
   source : TGLBaseSoundSource;
begin
   source:=(GetComponent(0) as TGLBaseSoundSource);
   if Assigned(source.SoundLibrary) then with source.SoundLibrary do
      for i:=0 to Samples.Count-1 do Proc(Samples[i].Name);
end;

//----------------- TGLCoordinatesProperty -------------------------------------

// GetAttributes
//
function TGLCoordinatesProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog, paSubProperties];
end;

// Edit;
//
procedure TGLCoordinatesProperty.Edit;
var
   glc : TGLCoordinates;
   x, y, z : Single;
begin
   glc:=TGLCoordinates(GetOrdValue);
   x:=glc.x;
   y:=glc.y;
   z:=glc.z;
   if VectorEditorForm.Execute(x, y, z) then begin
      glc.AsVector:=VectorMake(x, y, z);
      Modified;
   end;
end;

//----------------- TGLMaterialProperty --------------------------------------------------------------------------------

// GetAttributes
//
function TGLMaterialProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog, paSubProperties];
end;

// Edit
//
procedure TGLMaterialProperty.Edit;
begin
{$WARNING crossbuilder - needs MaterialEditorForm }
(*
   if MaterialEditorForm.Execute(TGLMaterial(GetOrdValue)) then
      Modified;
*)
end;

//----------------- TReuseableDefaultEditor --------------------------------------------------------------------------------

// CheckEdit
//
{$ifdef GLS_DELPHI_6_UP}
procedure TReuseableDefaultEditor.CheckEdit(const Prop : IProperty);
begin
  if FContinue then
    EditProperty(Prop, FContinue);
end;
{$else}
procedure TReuseableDefaultEditor.CheckEdit(PropertyEditor: TPropertyEditor);
var
  FreeEditor: Boolean;
begin
  FreeEditor:=True;
  try
    if FContinue then EditProperty(PropertyEditor, FContinue, FreeEditor);
  finally
    if FreeEditor then PropertyEditor.Free;
  end;
end;
{$endif}

// EditProperty
//
{$ifdef GLS_DELPHI_6_UP}
procedure TReuseableDefaultEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
var
  PropName: string;
  BestName: string;
  MethodProperty: IMethodProperty;

  procedure ReplaceBest;
  begin
    FBest := Prop;
    if FFirst = FBest then FFirst := nil;
  end;

begin
  if not Assigned(FFirst) and
    Supports(Prop, IMethodProperty, MethodProperty) then
    FFirst := Prop;
  PropName := Prop.GetName;
  BestName := '';
  if Assigned(FBest) then BestName := FBest.GetName;
  if CompareText(PropName, 'ONCREATE') = 0 then
    ReplaceBest
  else if CompareText(BestName, 'ONCREATE') <> 0 then
    if CompareText(PropName, 'ONCHANGE') = 0 then
      ReplaceBest
    else if CompareText(BestName, 'ONCHANGE') <> 0 then
      if CompareText(PropName, 'ONCLICK') = 0 then
        ReplaceBest;
end;
{$else}
procedure TReuseableDefaultEditor.EditProperty(PropertyEditor: TPropertyEditor;
															  var Continue, FreeEditor: Boolean);
var
  PropName: string;
  BestName: string;

  procedure ReplaceBest;
  begin
    FBest.Free;
	 FBest:=PropertyEditor;
    if FFirst = FBest then FFirst:=nil;
    FreeEditor:=False;
  end;

begin
  if not Assigned(FFirst) and (PropertyEditor is TMethodProperty) then
  begin
    FreeEditor:=False;
    FFirst:=PropertyEditor;
  end;
  PropName:=PropertyEditor.GetName;
  BestName:='';
  if Assigned(FBest) then BestName:=FBest.GetName;
  if CompareText(PropName, 'ONCREATE') = 0 then
    ReplaceBest
  else if CompareText(BestName, 'ONCREATE') <> 0 then
    if CompareText(PropName, 'ONCHANGE') = 0 then
		ReplaceBest
    else if CompareText(BestName, 'ONCHANGE') <> 0 then
      if CompareText(PropName, 'ONCLICK') = 0 then
        ReplaceBest;
end;
{$endif}

// Edit
//
{.$ifdef GLS_DELPHI_6_UP}
procedure TReuseableDefaultEditor.Edit;
(*
var
{$IFDEF FPC}
  Components : TDesignerSelections;
{$ELSE}
  Components: IDesignerSelections;
{$ENDIF}
*)
begin
(*  Components := TDesignerSelections.Create;
  FContinue := True;
  Components.Add(Component);
  FFirst := nil;
  FBest := nil;
  try
    GetComponentProperties(Components, tkAny, Designer, CheckEdit);
    if FContinue then
      if Assigned(FBest) then
        FBest.Edit
      else if Assigned(FFirst) then
        FFirst.Edit;
  finally
    FFirst := nil;
    FBest := nil;
  end;*)
end;
(*{$else}
procedure TReuseableDefaultEditor.Edit;
var
{$IFDEF FPC}
  Components : TDesignerSelections;
{$ELSE}
  Components : TDesignerSelectionList;
{$ENDIF}
begin
  Components:=TDesignerSelectionList.Create;
  try
    FContinue:=True;
    Components.Add(Component);
    FFirst:=nil;
    FBest:=nil;
	 try
      GetComponentProperties(Components, tkAny, Designer, CheckEdit);
      if FContinue then
        if Assigned(FBest) then
          FBest.Edit
        else if Assigned(FFirst) then
          FFirst.Edit;
    finally
      FFirst.Free;
      FBest.Free;
	 end;
  finally
    Components.Free;
  end;
end;
{$endif}
*)
//----------------- TGLMaterialLibraryEditor --------------------------------------------------------------------------------

// EditProperty
//
{$ifdef GLS_DELPHI_6_UP}
procedure TGLMaterialLibraryEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
begin
   if CompareText(Prop.GetName, 'MATERIALS') = 0 then begin
      FBest:=Prop;
   end;
end;
{$else}
procedure TGLMaterialLibraryEditor.EditProperty(PropertyEditor: TPropertyEditor;
                                                var Continue, FreeEditor: Boolean);
begin
   if CompareText(PropertyEditor.GetName, 'MATERIALS') = 0 then begin
      FBest.Free;
      FBest:=PropertyEditor;
      FreeEditor:=False;
   end;
end;
{$endif}

//----------------- TGLLibMaterialNameProperty ---------------------------------

// GetAttributes
//
function TGLLibMaterialNameProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog];
end;

// Edit
//
procedure TGLLibMaterialNameProperty.Edit;
var
  buf: string;
  ml: TGLMaterialLibrary;
  obj: TPersistent;
  Int: IGLMaterialLibrarySupported;
begin
	buf := GetStrValue;
  obj := GetComponent(0);
  if Supports(Obj, IGLMaterialLibrarySupported, Int) then
    ml := Int.GetMaterialLibrary
  else
  begin
    ml := nil;
    Assert(False, 'oops, unsupported...');
  end;
  {$WARNING crossbuilder - this needs more DesignTime units }
  {
	if not Assigned(ml) then
		ShowMessage('Select the material library first.')
	else if LibMaterialPicker.Execute(buf, ml) then
		SetStrValue(buf);
  }
end;

//----------------- TGLAnimationNameProperty -----------------------------------

// GetAttributes
//
function TGLAnimationNameProperty.GetAttributes: TPropertyAttributes;
begin
	Result:=[paValueList];
end;

// GetValues
//
procedure TGLAnimationNameProperty.GetValues(proc: TGetStrProc);
var
	i : Integer;
   animControler : TGLAnimationControler;
   actor : TGLActor;
begin
   animControler:=(GetComponent(0) as TGLAnimationControler);
   if Assigned(animControler) then begin
      actor:=animControler.Actor;
      if Assigned(actor) then with actor.Animations do begin
         for i:=0 to Count-1 do
            proc(Items[i].Name);
      end;
	end;
end;


procedure Register;
begin
   RegisterComponents('GLScene',
                      [TGLScene,
                       TGLSceneViewer, TGLMemoryViewer,
                       TGLMaterialLibrary,
                       TGLCadencer,
                       TGLGuiLayout,
                       TGLBitmapFont, TGLWindowsBitmapFont, TGLStoredBitmapFont,
                       TGLScriptLibrary,
                       TGLSoundLibrary
                       {$ifdef WINDOWS}
                       ,TGLWideBitmapFont
                       ,TGLFullScreenViewer
                       {$endif}
                      ]);

   RegisterComponents('GLScene PFX',
                      [
                       TGLCustomPFXManager,
                       TGLPolygonPFXManager, TGLPointLightPFXManager,
                       TGLCustomSpritePFXManager,
                       TGLPerlinPFXManager, TGLLinePFXManager,
                       TGLFireFXManager, TGLThorFXManager,
                       TGLEParticleMasksManager
                      ]);

   RegisterComponents('GLScene Utils',
                      [TAsyncTimer, TGLStaticImposterBuilder,
                       TCollisionManager, TGLAnimationControler,
                       //{$IFDEF WINDOWS}TAVIRecorder,{$ENDIF}
                       TGLDCEManager, TGLFPSMovementManager,
                       TGLMaterialScripter, TGLUserInterface, TGLNavigator,
                       TGLSmoothNavigator, TGLSmoothUserInterface,
                       TGLTimeEventsMGR, TApplicationFileIO, TGLVfsPAK,
                       TGLSimpleNavigation, TGLGizmo, TGLCameraController
                       {$IFDEF MSWINDOWS}
                       , TJoystick, TScreenSaver
                       {$ENDIF}
                      ]);

   RegisterComponents('GLScene Terrain',
                      [TGLBitmapHDS, TGLCustomHDS, TGLHeightTileFileHDS,
                       TGLBumpmapHDS, TGLPerlinHDS, TGLTexturedHDS, TGLAsyncHDS,
                       TGLShadowHDS
                      ]);

   RegisterComponents('GLScene Shaders',
                      [ TGLTexCombineShader, TGLPhongShader, TGLUserShader,
                        TGLHiddenLineShader, TGLCelShader, TGLOutlineShader,
                        TGLMultiMaterialShader, TGLBumpShader,
                        TGLSLShader, TGLSLDiffuseSpecularShader, TGLSLBumpShader,
                        TGLAsmShader,TGLShaderCombiner,TGLTextureSharingShader,
                        TGLSLPostBlurShader
                      ]);

   RegisterComponentEditor(TGLSceneViewer, TGLSceneViewerEditor);
   RegisterComponentEditor(TGLScene, TGLSceneEditor);
//   RegisterComponentEditor(TGLMaterialLibrary, TGLMaterialLibraryEditor);


   RegisterPropertyEditor(TypeInfo(TResolution), nil, '', TResolutionProperty);
   RegisterPropertyEditor(TypeInfo(TGLTexture), TGLMaterial, '', TGLTextureProperty);
   RegisterPropertyEditor(TypeInfo(TGLTextureImage), TGLTexture, '', TGLTextureImageProperty);
   RegisterPropertyEditor(TypeInfo(String), TGLTexture, 'ImageClassName', TGLImageClassProperty);

   RegisterPropertyEditor(TypeInfo(TGLSoundFile), TGLSoundSample, '', TSoundFileProperty);
   RegisterPropertyEditor(TypeInfo(String), TGLBaseSoundSource, 'SoundName', TSoundNameProperty);

   RegisterPropertyEditor(TypeInfo(TGLCoordinates), nil, '', TGLCoordinatesProperty);

   RegisterPropertyEditor(TypeInfo(TGLColor), nil, '', TGLColorProperty);
   RegisterPropertyEditor(TypeInfo(TGLMaterial), nil, '', TGLMaterialProperty);

   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterial, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLLibMaterial, 'Texture2Name', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLSkyBox, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLEParticleMask, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLGameMenu, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterialMultiProxyMaster, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLSLBumpShader, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TSpriteAnimation, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterialProxy, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLActorProxy, '', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TActorAnimationName), TGLAnimationControler, '', TGLAnimationNameProperty);
   RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLTextureSharingShaderMaterial, 'LibMaterialName', TGLLibMaterialNameProperty);
   RegisterPropertyEditor(TypeInfo(TFileName), TGLFreeForm, 'FileName', TVectorFileProperty);
//Still needed?   RegisterClasses([TGLCoordinates]);

   ObjectManager.RegisterSceneObjectsToIDE;

end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
  {$i GLSceneRegister.lrs}
  {$i gllazarusobjects.lrs}

   GLColor.vUseDefaultColorSets:=True;
   GLCoordinates.vUseDefaultCoordinateSets:=True;
   //ReadVideoModes;
   with ObjectManager do begin
      CreateDefaultObjectIcons(HInstance);
      RegisterSceneObject(TGLCamera, 'Camera', '', HInstance);
      RegisterSceneObject(TGLLightSource, 'LightSource', '', HInstance);
      RegisterSceneObject(TGLDummyCube, 'DummyCube', '', HInstance);

      //Basic geometry
      RegisterSceneObject(TGLSprite, 'Sprite', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLPoints, 'Points', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLLines, 'Lines', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLPlane, 'Plane', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLPolygon, 'Polygon', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLCube, 'Cube', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLFrustrum, 'Frustrum', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLSphere, 'Sphere', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLDisk, 'Disk', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLCone, 'Cone', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLCylinder, 'Cylinder', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLCapsule, 'Capsule', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLDodecahedron, 'Dodecahedron', glsOCBasicGeometry, HInstance);
      RegisterSceneObject(TGLIcosahedron, 'Icosahedron', glsOCBasicGeometry, HInstance);

      //Advanced geometry
      RegisterSceneObject(TGLAnimatedSprite, 'Animated Sprite', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLArrowLine, 'ArrowLine', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLAnnulus, 'Annulus', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLExtrusionSolid, 'ExtrusionSolid', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLMultiPolygon, 'MultiPolygon', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLPipe, 'Pipe', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLRevolutionSolid, 'RevolutionSolid', glsOCAdvancedGeometry, HInstance);
      RegisterSceneObject(TGLTorus, 'Torus', glsOCAdvancedGeometry, HInstance);

      //Mesh objects
      RegisterSceneObject(TGLActor, 'Actor', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLFreeForm, 'FreeForm', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLMesh, 'Mesh', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLTilePlane, 'TilePlane', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLPortal, 'Portal', glsOCMeshObjects, HInstance);
      RegisterSceneObject(TGLTerrainRenderer, 'TerrainRenderer', glsOCMeshObjects, HInstance);

      //Graph-plotting objects
      RegisterSceneObject(TGLFlatText, 'FlatText', glsOCGraphPlottingObjects, HInstance);
      RegisterSceneObject(TGLHeightField, 'HeightField', glsOCGraphPlottingObjects, HInstance);
      RegisterSceneObject(TGLXYZGrid, 'XYZGrid', glsOCGraphPlottingObjects, HInstance);

      //Particle systems
      RegisterSceneObject(TGLParticles, 'Particles', glsOCParticleSystems, HInstance);
      RegisterSceneObject(TGLParticleFXRenderer, 'PFX Renderer', glsOCParticleSystems, HInstance);

      //Environment objects
      RegisterSceneObject(TGLEarthSkyDome, 'EarthSkyDome', glsOCEnvironmentObjects, HInstance);
      RegisterSceneObject(TGLSkyDome, 'SkyDome', glsOCEnvironmentObjects, HInstance);
      RegisterSceneObject(TGLSkyBox, 'SkyBox', glsOCEnvironmentObjects, HInstance);
      RegisterSceneObject(TGLAtmosphere, 'Atmosphere', glsOCEnvironmentObjects, HInstance);

      // HUD objects.
      RegisterSceneObject(TGLHUDSprite, 'HUD Sprite', glsOCHUDObjects, HInstance);
      RegisterSceneObject(TGLHUDText, 'HUD Text', glsOCHUDObjects, HInstance);
      RegisterSceneObject(TGLResolutionIndependantHUDText, 'Resolution Independant HUD Text', glsOCHUDObjects, HInstance);
      RegisterSceneObject(TGLAbsoluteHUDText, 'Absolute HUD Text', glsOCHUDObjects, HInstance);
      RegisterSceneObject(TGLGameMenu, 'GameMenu', glsOCHUDObjects, HInstance);
      RegisterSceneObject(TGLConsole, 'Console', glsOCHUDObjects, HInstance);

      // GUI objects.
      RegisterSceneObject(TGLBaseControl, 'Root Control', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLPopupMenu, 'GLPopupMenu', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLForm, 'GLForm', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLPanel, 'GLPanel', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLButton, 'GLButton', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLCheckBox, 'GLCheckBox', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLEdit, 'GLEdit', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLLabel, 'GLLabel', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLAdvancedLabel, 'GLAdvancedLabel', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLScrollbar, 'GLScrollbar', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLStringGrid, 'GLStringGrid', glsOCGuiObjects, HInstance);
      RegisterSceneObject(TGLCustomControl, 'GLBitmapControl', glsOCGuiObjects, HInstance);

      //Special objects
      RegisterSceneObject(TGLLensFlare, 'LensFlare', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLTextureLensFlare, 'TextureLensFlare', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLMirror, 'Mirror', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLShadowPlane, 'ShadowPlane', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLShadowVolume, 'ShadowVolume', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLZShadows, 'ZShadows', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLSLTextureEmitter, 'GLSL Texture Emitter', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLSLProjectedTextures, 'GLSL Projected Textures', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLTextureEmitter, 'Texture Emitter', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLProjectedTextures, 'Projected Textures', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLBlur, 'Blur', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLMotionBlur, 'MotionBlur', glsOCSpecialObjects, HInstance);
      {$IFDEF WINDOWS}
      RegisterSceneObject(TGLSpaceText, 'SpaceText', glsOCDoodad, HInstance);
      {$ENDIF}
      RegisterSceneObject(TGLTrail, 'GLTrail', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLPostEffect, 'PostEffect', glsOCSpecialObjects, HInstance);
      RegisterSceneObject(TGLPostShaderHolder, 'PostShaderHolder', glsOCSpecialObjects, HInstance);

      // Doodad objects.
      RegisterSceneObject(TGLTeapot, 'Teapot', glsOCDoodad, HInstance);
      RegisterSceneObject(TGLTree, 'Tree', glsOCDoodad, HInstance);
      RegisterSceneObject(TGLWaterPlane, 'WaterPlane', glsOCDoodad, HInstance);

      // Proxy objects.
      RegisterSceneObject(TGLProxyObject, 'ProxyObject', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLColorProxy, 'ColorProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLFreeFormProxy, 'FreeFormProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLMaterialProxy, 'MaterialProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLActorProxy, 'ActorProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLMultiProxy, 'MultiProxy', glsOCProxyObjects, HInstance);
      RegisterSceneObject(TGLMaterialMultiProxy, 'MaterialMultiProxy', glsOCProxyObjects, HInstance);

      // Other objects.
      RegisterSceneObject(TGLDirectOpenGL, 'Direct OpenGL', '', HInstance);
      RegisterSceneObject(TGLRenderPoint, 'Render Point', '', HInstance);
      RegisterSceneObject(TGLImposter, 'Imposter Sprite', '', HInstance);
      RegisterSceneObject(TGLFeedback, 'OpenGL Feedback', '', HInstance);
      RegisterSceneObject(TGLFBORenderer, 'OpenGL FrameBuffer', '', HInstance);
   end;

finalization

   ObjectManager.Free;

end.
