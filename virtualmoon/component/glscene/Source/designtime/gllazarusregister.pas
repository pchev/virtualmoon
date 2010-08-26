//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLLazarusRegister<p>

   This is a clone of GLSceneRegister modified for use with Lazarus.<p>
   
   Registration unit for GLScene library components, property editors and
      IDE experts.<p>



      $Log: gllazarusregister.pas,v $
      Revision 1.3  2006/03/01 10:08:42  skinhat
      glplot cf glplott

      Revision 1.2  2006/01/12 19:44:02  z0m3ie
      *** empty log message ***

      Revision 1.1  2006/01/10 20:50:44  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.1  2006/01/09 21:02:30  z0m3ie
      *** empty log message ***

      Revision 1.15  2005/11/14 21:38:07  z0m3ie
      making this stuff again Linux compatible please dont break multi platform support again

      Revision 1.14  2005/10/11 20:31:42  z0m3ie
      - removed ode and sdl stuff becouse dll´s arend loaded dynamically
        maybe we should make seperate packages or load dlls dynamic

      Revision 1.13  2005/09/17 22:40:44  k00m
      *** empty log message ***

      Revision 1.12  2005/09/16 18:38:05  k00m
      Register my ode component and more about viewer.

      Revision 1.11  2005/09/09 23:16:18  z0m3ie
      linux corrections

      Revision 1.10  2005/08/23 03:36:36  k00m
      adding basic color support.

      Revision 1.9  2005/08/23 03:00:48  k00m
      correction from me with the glsceneedit.

      Revision 1.8  2005/08/23 02:44:28  k00m
      register HDS and remove some bad thing I have adding before.

      Revision 1.7  2005/08/22 23:22:25  k00m
      updated register again not ready.

      Revision 1.6  2005/08/22 22:12:06  k00m
      try to register more to see what working.

      Revision 1.5  2005/08/22 20:43:49  k00m
      adding and register sound type.

      Revision 1.4  2005/08/22 20:27:38  k00m
      adding soundfile unit.

      Revision 1.3  2005/08/22 20:07:47  k00m
      Adding Sounds.

      Revision 1.2  2005/08/03 00:41:38  z0m3ie
      - added automatical generated History from CVS


	<b>History : </b><font size=-1><ul>
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
unit GLLazarusRegister;

// Registration unit for GLScene library
// 30-DEC-99 ml: scene editor added, structural changes

interface

{$i GLScene.inc}

uses
   {$ifdef windows}windows,{$endif}
   classes, controls, stdctrls, dialogs, glscene, lresources, propedits, lclintf, ComponentReg;


type

   PSceneObjectEntry = ^TGLSceneObjectEntry;
   // holds a relation between an scene object class, its global identification,
   // its location in the object stock and its icon reference
   TGLSceneObjectEntry = record
      ObjectClass : TGLSceneObjectClass;
      Name : String[32];     // type name of the object
      Category : String[32]; // category of object
      Index,                 // index into "FObjectStock"
      ImageIndex : Integer;  // index into "FObjectIcons"
   end;

   // TObjectManager
   //
   TObjectManager = class (TObject)
      private
         { Private Declarations }
         FSceneObjectList : TList;
         FObjectIcons : TImageList;       // a list of icons for scene objects
         {.$ifdef WINDOWS}
         FOverlayIndex,                   // indices into the object icon list
         {.$endif}
         FSceneRootIndex,
         FCameraRootIndex,
         FLightsourceRootIndex,
         FObjectRootIndex,
         FStockObjectRootIndex : Integer;
         procedure RegisterSceneObjectsToIDE;

      protected
			{ Protected Declarations }
         procedure CreateDefaultObjectIcons;
         procedure DestroySceneObjectList;
         function FindSceneObjectClass(AObjectClass: TGLSceneObjectClass;
                                       const ASceneObject: String = '') : PSceneObjectEntry;

      public
         { Public Declarations }
         constructor Create;
         destructor Destroy; override;

         function GetClassFromIndex(Index: Integer): TGLSceneObjectClass;
         function GetImageIndex(ASceneObject: TGLSceneObjectClass) : Integer;
         function GetCategory(ASceneObject: TGLSceneObjectClass) : String;
         procedure GetRegisteredSceneObjects(ObjectList: TStringList);
         //: Registers a stock object and adds it to the stock object list
         procedure RegisterSceneObject(ASceneObject: TGLSceneObjectClass; const aName, aCategory : String);
         //: Unregisters a stock object and removes it from the stock object list
         procedure UnRegisterSceneObject(ASceneObject: TGLSceneObjectClass);
//         procedure Notify(Sender: TPlugInManager; Operation: TOperation; PlugIn: Integer); override;

         property ObjectIcons: TImageList read FObjectIcons;
         property SceneRootIndex: Integer read FSceneRootIndex;
         property LightsourceRootIndex: Integer read FLightsourceRootIndex;
         property CameraRootIndex: Integer read FCameraRootIndex;
         property ObjectRootIndex: Integer read FObjectRootIndex;

   end;

	// TGLLibMaterialNameProperty
	//
	TGLLibMaterialNameProperty = class(TStringProperty)
		public
			{ Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
			procedure Edit; override;
	end;

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
   TypInfo, VectorGeometry, GLTexture, SysUtils, GLCrossPlatform, GLStrings,
   GLObjects, GLVectorFileObjects, GLExtrusion, GLMultiPolygon, GLMesh, GLPortal,
   GLGraph, GLParticles, GLHUDObjects, GLSkydome, GLBitmapFont, GLLensFlare,
   GLMirror, GLParticleFX, GLShadowPlane, GLTerrainRenderer, GLShadowVolume,
   GLTeapot, GLPolyhedron, GLGeomObjects, GLTextureImageEditors, GLMultiProxy,
   GLSkyBox, GLState, GLUtils, GLTilePlane, GLTree, GLImposter, GLWaterPlane,
   GLPerlinPFX, GLTexLensFlare, GLFireFX, GLThorFX, GLSceneEdit, FVectorEditor,
   GLCadencer, GLCollision, GLHeightData, GLzBuffer, GLGui, GLBumpmapHDS,
   AsyncTimer, GLWindows, GLWindowsFont, GLHeightTileFileHDS, GLTexturedHDS,
   GLAnimatedSprite, GLFeedback, GLProjectedTextures, GLBlur, GLTrail, GLPerlin,
   GLLinePFX, GLScriptBase, GLGameMenu, GLEParticleMasksManager,
   GLTimeEventsMgr, GLNavigator, GLMaterialScript, GLFPSMovement, GLDCE,
   ApplicationFileIO,  GLScreen, GLMisc, GLVfsPAK, GLSimpleNavigation,
   GLAsyncHDS, GLConsole, GLAtmosphere, GLProxyObjects, GLMaterialMultiProxy,
   GLSLShader, GLSLDiffuseSpecularShader, GLSLBumpShader, GLAsmShader,
   GLShaderCombiner, GLSmoothNavigator, GLPostEffects, GLPhongShader,
   GLTexCombineShader, GLCelShader, GLOutlineShader, GLMultiMaterialShader,
   GLBumpShader, GLHiddenLineShader, GLUserShader, GLShadowHDS, GLSLProjectedTextures,
   GLSound, GLSoundFileObjects,
   Graphics,
   GLColor, GLViewer, GLGizmo, GLTextureSharingShader, GLGraphics,
   GLDynamicTexture,
   {$ifdef windows}
     gllclfullscreenviewer, GLSpaceText,
     //GLWideBitmapFont,  doesn't even compile in win due to missing TTextRecA
     //GLAVIRecorder, doesn't even compile in win due to missing commDlg
   {$endif}
   componenteditors
   {,glsdlcontext,glscriptbase,}
   ;

resourcestring
   { OpenGL property category }
   sOpenGLCategoryName = 'OpenGL';


var
   vObjectManager : TObjectManager;

function ObjectManager : TObjectManager;
begin
   if not Assigned(vObjectManager) then
      vObjectManager:=TObjectManager.Create;
   Result:=vObjectManager;
end;


{ TODO : Moving property editors to the public interface }

type

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
      protected
         { Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
   end;

   // TGLTextureImageProperty
   //
   TGLTextureImageProperty = class(TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
         procedure Edit; override;
   end;

   // TGLImageClassProperty
   //
   TGLImageClassProperty = class(TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes : TPropertyAttributes; override;
         procedure GetValues(proc : TGetStrProc); override;

      public
        { Public Declarations }
        function GetValue : String; override;
        procedure SetValue(const value : String); override;
   end;


   TGLColorProperty = class (TClassProperty)
      private
        { Private Declarations }
      protected
        { Protected Declarations }
	function GetAttributes: TPropertyAttributes; override;
	procedure GetValues(Proc: TGetStrProc); override;
	procedure Edit; override;

        function ColorToBorderColor(aColor: TColorVector; selected : Boolean) : TColor;

      public
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
      protected
         { Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
         function GetValue: String; override;
         procedure Edit; override;
         procedure SetValue(const Value: string); override;
   end;

   // TSoundFileProperty
   //
   TSoundFileProperty = class (TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes : TPropertyAttributes; override;
         function GetValue: String; override;
         procedure Edit; override;
   end;

   // TSoundNameProperty
   //
   TSoundNameProperty = class (TStringProperty)
      protected
         { Protected Declarations }
         function GetAttributes : TPropertyAttributes; override;
      	procedure GetValues(Proc: TGetStrProc); override;
   end;


   // TGLCoordinatesProperty
   //
   TGLCoordinatesProperty = class(TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
         procedure Edit; override;
   end;

   // TGLMaterialProperty
   //
   TGLMaterialProperty = class(TClassProperty)
      protected
         { Protected Declarations }
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
      protected
         { Protected Declarations }
         function GetAttributes : TPropertyAttributes; override;
         procedure GetValues(proc : TGetStrProc); override;

      public
         { Public Declarations }
   end;


//----------------- TObjectManager ---------------------------------------------

// Create
//
constructor TObjectManager.Create;
begin
  inherited;
  FSceneObjectList:=TList.Create;
  CreateDefaultObjectIcons;
end;

// Destroy
//
destructor TObjectManager.Destroy;
begin
   DestroySceneObjectList;
   FObjectIcons.Free;
   inherited Destroy;
end;

// Notify
//
//procedure TObjectManager.Notify(Sender: TPlugInManager; Operation: TOperation; PlugIn: Integer);
//begin
//end;

// FindSceneObjectClass
//
function TObjectManager.FindSceneObjectClass(AObjectClass: TGLSceneObjectClass;
                           const aSceneObject: String = '') : PSceneObjectEntry;
var
   I     : Integer;
   Found : Boolean;
begin
   Result:=nil;
   Found:=False;
   with FSceneObjectList do begin
      for I:=0 to Count-1 do
         with TGLSceneObjectEntry(Items[I]^) do
         if (ObjectClass = AObjectClass) and (Length(ASceneObject) = 0)
               or (CompareText(Name, ASceneObject) = 0) then begin
            Found:=True;
            Break;
         end;
      if Found then Result:=Items[I];
   end;
end;

// GetClassFromIndex
//
function TObjectManager.GetClassFromIndex(Index: Integer): TGLSceneObjectClass;
begin
   if Index<0 then
      Index:=0;
   if Index>FSceneObjectList.Count-1 then
      Index:=FSceneObjectList.Count-1;
  Result:=TGLSceneObjectEntry(FSceneObjectList.Items[Index+1]^).ObjectClass;
end;

// GetImageIndex
//
function TObjectManager.GetImageIndex(ASceneObject: TGLSceneObjectClass) : Integer;
var
   classEntry : PSceneObjectEntry;
begin
   classEntry:=FindSceneObjectClass(ASceneObject);
   if Assigned(classEntry) then
      Result:=classEntry^.ImageIndex
   else Result:=0;
end;

// GetCategory
//
function TObjectManager.GetCategory(ASceneObject: TGLSceneObjectClass) : String;
var
   classEntry : PSceneObjectEntry;
begin
   classEntry:=FindSceneObjectClass(ASceneObject);
   if Assigned(classEntry) then
      Result:=classEntry^.Category
   else Result:='';
end;

// GetRegisteredSceneObjects
//
procedure TObjectManager.GetRegisteredSceneObjects(objectList: TStringList);
var
   i : Integer;
begin
   if Assigned(objectList) then with objectList do begin
      Clear;
      for i:=1 to FSceneObjectList.Count-1 do
         with TGLSceneObjectEntry(FSceneObjectList.Items[I]^) do
            AddObject(Name, Pointer(ObjectClass));
   end;
end;


procedure TObjectManager.RegisterSceneObjectsToIDE;
var i : integer;
begin
  if not(Assigned(RegisterNoIconProc)) then exit;
  for i := 0 to FSceneObjectList.count-1 do begin
     RegisterNoIcon([PSceneObjectEntry(FSceneObjectList[i])^.ObjectClass]);
  end;
end;


// RegisterSceneObject
//
procedure TObjectManager.RegisterSceneObject(ASceneObject: TGLSceneObjectClass;
                                             const aName, aCategory : String);
var
   newEntry  : PSceneObjectEntry;
   pic       : TPicture;
   resBitmapName : String;
   bmp : TBitmap;
begin
//>>Lazarus will crash at this function
   if Assigned(RegisterNoIconProc) then
      RegisterNoIcon([ASceneObject]);
   //Writeln('GL Registered ',ASceneObject.classname);
   Classes.RegisterClass(ASceneObject);
   with FSceneObjectList do begin
      // make sure no class is registered twice
      if Assigned(FindSceneObjectClass(ASceneObject, AName)) then Exit;
      New(NewEntry);
      pic:=TPicture.Create;
      try
         with NewEntry^ do begin
            // object stock stuff
            // registered objects list stuff
            ObjectClass:=ASceneObject;
            NewEntry^.Name:=aName;
            NewEntry^.Category:=aCategory;
            Index:=FSceneObjectList.Count;
            resBitmapName:=ASceneObject.ClassName;
            {$IFNDEF FPC}
            GLLoadBitmapFromInstance(Pic.Bitmap,resBitmapName);
            bmp:=TBitmap.Create;
            bmp.PixelFormat:=glpf24bit;
            bmp.Width:=24; bmp.Height:=24;
            bmp.Canvas.Draw(0, 0, Pic.Bitmap);
            Pic.Bitmap:=bmp;
            bmp.Free;
            if Cardinal(Pic.Bitmap.Handle)<>0 then begin
               FObjectIcons.AddMasked(Pic.Bitmap, Pic.Bitmap.Canvas.Pixels[0, 0]);
               ImageIndex:=FObjectIcons.Count-1;
            {$ELSE}
            if LazarusResources.Find(resBitmapName) <> nil then begin
              try FObjectIcons.AddLazarusResource(resBitmapName); except end;
              ImageIndex:=FObjectIcons.Count-1;
            {$ENDIF}
            end else begin
              ImageIndex:=0;
              //writeln('No Image loaded for :',resBitmapName);
            end;
         end;
       Add(NewEntry);
      finally
         pic.Free;
      end;
   end;
end;

// UnRegisterSceneObject
//
procedure TObjectManager.UnRegisterSceneObject(ASceneObject: TGLSceneObjectClass);
var
   oldEntry : PSceneObjectEntry;
begin
   // find the class in the scene object list
   OldEntry:=FindSceneObjectClass(ASceneObject);
   // found?
   if assigned(OldEntry) then begin
      // remove its entry from the list of registered objects
      FSceneObjectList.Remove(OldEntry);
      // finally free the memory for the entry
      Dispose(OldEntry);
   end;
end;

// CreateDefaultObjectIcons
//
procedure TObjectManager.CreateDefaultObjectIcons;
var
   pic : TPicture;
begin
   {$IFNDEF FPC}
   pic:=TPicture.Create;
   // load first pic to get size
   GLLoadBitmapFromInstance(Pic.Bitmap,'gls_cross');
   FObjectIcons:=TImageList.CreateSize(Pic.Width, Pic.height);

   with FObjectIcons, pic.Bitmap.Canvas do begin
      try
         // There's a more direct way for loading images into the image list, but
         // the image quality suffers too much
         AddMasked(Pic.Bitmap, Pixels[0, 0]);
         {$ifdef WIN32}
         FOverlayIndex:=Count-1;
         Overlay(FOverlayIndex, 0); // used as indicator for disabled objects
         {$endif}
         GLLoadBitmapFromInstance(Pic.Bitmap,'gls_root');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FSceneRootIndex:=Count-1;
         GLLoadBitmapFromInstance(Pic.Bitmap,'gls_camera');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FCameraRootIndex:=Count-1;
         GLLoadBitmapFromInstance(Pic.Bitmap,'gls_lights');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FLightsourceRootIndex:=Count-1;
         GLLoadBitmapFromInstance(Pic.Bitmap,'gls_objects');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FObjectRootIndex:=Count-1;
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FStockObjectRootIndex:=Count-1;
      finally
         Pic.Free;
      end;
   end;
   {$ELSE}
   FObjectIcons:=TImageList.CreateSize(16, 16);
   with FObjectIcons do begin
         if LazarusResources.Find('gls_cross') <> nil then
           try AddLazarusResource('gls_cross'); except end;
         FOverlayIndex:=Count-1;
         //missing in lazarus: TImageList.overlay
         //Overlay(FOverlayIndex, 0); // used as indicator for disabled objects
         if LazarusResources.Find('gls_root') <> nil then
           try AddLazarusResource('gls_root'); except end;
         FSceneRootIndex:=Count-1;
         if LazarusResources.Find('gls_camera') <> nil then
           try AddLazarusResource('gls_camera'); except end;
         FCameraRootIndex:=Count-1;
         if LazarusResources.Find('gls_lights') <> nil then
           try AddLazarusResource('gls_lights'); except end;
         FLightsourceRootIndex:=Count-1;
         if LazarusResources.Find('gls_objects') <> nil then
           try AddLazarusResource('gls_objects'); except end;
         FObjectRootIndex:=Count-1;
         if LazarusResources.Find('gls_objects') <> nil then
           try AddLazarusResource('gls_objects'); except end;
         FStockObjectRootIndex:=Count-1;
   end;
   {$ENDIF}
end;

// DestroySceneObjectList
//
procedure TObjectManager.DestroySceneObjectList;
var
   i : Integer;
begin
   with FSceneObjectList do begin
      for i:=0 to Count-1 do
         Dispose(PSceneObjectEntry(Items[I]));
      Free;
   end;
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
   Result:=vVideoModes[GetOrdValue].Description;
end;

// GetValues
//
procedure TResolutionProperty.GetValues(Proc: TGetStrProc);
var
   i : Integer;
begin
   for i:=0 to vNumberVideoModes-1 do
      Proc(vVideoModes[i].Description);
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
var
	ownerTexture : TGLTexture;
begin
	ownerTexture:=TGLTextureImage(GetOrdValue).OwnerTexture;
	if ownerTexture.Image.Edit then
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
                       TGLScriptLibrary
                       {$ifdef WINDOWS}
                       ,TGLFullScreenViewer
                       //,TGLWideBitmapFont
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
                       TGLSimpleNavigation, TGLGizmo
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
                        TGLAsmShader,TGLShaderCombiner,TGLTextureSharingShader
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
  {$i GLLazarusRegister.lrs}
  {$i gllazarusobjects.lrs}

   GLMisc.vUseDefaultSets:=True;
   //ReadVideoModes;
   with ObjectManager do begin
      RegisterSceneObject(TGLCamera, 'Camera', '');
      RegisterSceneObject(TGLLightSource, 'LightSource', '');
      RegisterSceneObject(TGLDummyCube, 'DummyCube', '');

      //Basic geometry
      RegisterSceneObject(TGLSprite, 'Sprite', glsOCBasicGeometry);
      RegisterSceneObject(TGLPoints, 'Points', glsOCBasicGeometry);
      RegisterSceneObject(TGLLines, 'Lines', glsOCBasicGeometry);
      RegisterSceneObject(TGLPlane, 'Plane', glsOCBasicGeometry);
      RegisterSceneObject(TGLPolygon, 'Polygon', glsOCBasicGeometry);
      RegisterSceneObject(TGLCube, 'Cube', glsOCBasicGeometry);
      RegisterSceneObject(TGLFrustrum, 'Frustrum', glsOCBasicGeometry);
      RegisterSceneObject(TGLSphere, 'Sphere', glsOCBasicGeometry);
      RegisterSceneObject(TGLDisk, 'Disk', glsOCBasicGeometry);
      RegisterSceneObject(TGLCone, 'Cone', glsOCBasicGeometry);
      RegisterSceneObject(TGLCylinder, 'Cylinder', glsOCBasicGeometry);
      RegisterSceneObject(TGLCapsule, 'Capsule', glsOCBasicGeometry);
      RegisterSceneObject(TGLDodecahedron, 'Dodecahedron', glsOCBasicGeometry);
      RegisterSceneObject(TGLIcosahedron, 'Icosahedron', glsOCBasicGeometry);

      //Advanced geometry
      RegisterSceneObject(TGLAnimatedSprite, 'Animated Sprite', glsOCAdvancedGeometry);
      RegisterSceneObject(TGLArrowLine, 'ArrowLine', glsOCAdvancedGeometry);
      RegisterSceneObject(TGLAnnulus, 'Annulus', glsOCAdvancedGeometry);
      RegisterSceneObject(TGLExtrusionSolid, 'ExtrusionSolid', glsOCAdvancedGeometry);
      RegisterSceneObject(TGLMultiPolygon, 'MultiPolygon', glsOCAdvancedGeometry);
      RegisterSceneObject(TGLPipe, 'Pipe', glsOCAdvancedGeometry);
      RegisterSceneObject(TGLRevolutionSolid, 'RevolutionSolid', glsOCAdvancedGeometry);
      RegisterSceneObject(TGLTorus, 'Torus', glsOCAdvancedGeometry);

      //Mesh objects
      RegisterSceneObject(TGLActor, 'Actor', glsOCMeshObjects);
      RegisterSceneObject(TGLFreeForm, 'FreeForm', glsOCMeshObjects);
      RegisterSceneObject(TGLMesh, 'Mesh', glsOCMeshObjects);
      RegisterSceneObject(TGLTilePlane, 'TilePlane', glsOCMeshObjects);
      RegisterSceneObject(TGLPortal, 'Portal', glsOCMeshObjects);
      RegisterSceneObject(TGLTerrainRenderer, 'TerrainRenderer', glsOCMeshObjects);

      //Graph-plotting objects
      RegisterSceneObject(TGLFlatText, 'FlatText', glsOCGraphPlottingObjects);
      RegisterSceneObject(TGLHeightField, 'HeightField', glsOCGraphPlottingObjects);
      RegisterSceneObject(TGLXYZGrid, 'XYZGrid', glsOCGraphPlottingObjects);

      //Particle systems
      RegisterSceneObject(TGLParticles, 'Particles', glsOCParticleSystems);
      RegisterSceneObject(TGLParticleFXRenderer, 'PFX Renderer', glsOCParticleSystems);

      //Environment objects
      RegisterSceneObject(TGLEarthSkyDome, 'EarthSkyDome', glsOCEnvironmentObjects);
      RegisterSceneObject(TGLSkyDome, 'SkyDome', glsOCEnvironmentObjects);
      RegisterSceneObject(TGLSkyBox, 'SkyBox', glsOCEnvironmentObjects);
      RegisterSceneObject(TGLAtmosphere, 'Atmosphere', glsOCEnvironmentObjects);

      // HUD objects.
      RegisterSceneObject(TGLHUDSprite, 'HUD Sprite', glsOCHUDObjects);
      RegisterSceneObject(TGLHUDText, 'HUD Text', glsOCHUDObjects);
      RegisterSceneObject(TGLResolutionIndependantHUDText, 'Resolution Independant HUD Text', glsOCHUDObjects);
      RegisterSceneObject(TGLAbsoluteHUDText, 'Absolute HUD Text', glsOCHUDObjects);
      RegisterSceneObject(TGLGameMenu, 'GameMenu', glsOCHUDObjects);
      RegisterSceneObject(TGLConsole, 'Console', glsOCHUDObjects);

      // GUI objects.
      RegisterSceneObject(TGLBaseControl, 'Root Control', glsOCGuiObjects);
      RegisterSceneObject(TGLPopupMenu, 'GLPopupMenu', glsOCGuiObjects);
      RegisterSceneObject(TGLForm, 'GLForm', glsOCGuiObjects);
      RegisterSceneObject(TGLPanel, 'GLPanel', glsOCGuiObjects);
      RegisterSceneObject(TGLButton, 'GLButton', glsOCGuiObjects);
      RegisterSceneObject(TGLCheckBox, 'GLCheckBox', glsOCGuiObjects);
      RegisterSceneObject(TGLEdit, 'GLEdit', glsOCGuiObjects);
      RegisterSceneObject(TGLLabel, 'GLLabel', glsOCGuiObjects);
      RegisterSceneObject(TGLAdvancedLabel, 'GLAdvancedLabel', glsOCGuiObjects);
      RegisterSceneObject(TGLScrollbar, 'GLScrollbar', glsOCGuiObjects);
      RegisterSceneObject(TGLStringGrid, 'GLStringGrid', glsOCGuiObjects);
      RegisterSceneObject(TGLCustomControl, 'GLBitmapControl', glsOCGuiObjects);

      //Special objects
      RegisterSceneObject(TGLLensFlare, 'LensFlare', glsOCSpecialObjects);
      RegisterSceneObject(TGLTextureLensFlare, 'TextureLensFlare', glsOCSpecialObjects);
      RegisterSceneObject(TGLMirror, 'Mirror', glsOCSpecialObjects);
      RegisterSceneObject(TGLShadowPlane, 'ShadowPlane', glsOCSpecialObjects);
      RegisterSceneObject(TGLShadowVolume, 'ShadowVolume', glsOCSpecialObjects);
      RegisterSceneObject(TGLZShadows, 'ZShadows', glsOCSpecialObjects);
      RegisterSceneObject(TGLSLTextureEmitter, 'GLSL Texture Emitter', glsOCSpecialObjects);
      RegisterSceneObject(TGLSLProjectedTextures, 'GLSL Projected Textures', glsOCSpecialObjects);
      RegisterSceneObject(TGLTextureEmitter, 'Texture Emitter', glsOCSpecialObjects);
      RegisterSceneObject(TGLProjectedTextures, 'Projected Textures', glsOCSpecialObjects);
      RegisterSceneObject(TGLBlur, 'Blur', glsOCSpecialObjects);
      RegisterSceneObject(TGLMotionBlur, 'MotionBlur', glsOCSpecialObjects);
      {$IFDEF WINDOWS}
      RegisterSceneObject(TGLSpaceText, 'SpaceText', glsOCDoodad);
      {$ENDIF}
      RegisterSceneObject(TGLTrail, 'GLTrail', glsOCSpecialObjects);

      RegisterSceneObject(TGLPostEffect, 'PostEffect', glsOCSpecialObjects);
      RegisterSceneObject(TGLPostShaderHolder, 'PostShaderHolder', glsOCSpecialObjects);

      // Doodad objects.
      RegisterSceneObject(TGLTeapot, 'Teapot', glsOCDoodad);
      RegisterSceneObject(TGLTree, 'Tree', glsOCDoodad);
      RegisterSceneObject(TGLWaterPlane, 'WaterPlane', glsOCDoodad);

      // Proxy objects.
      RegisterSceneObject(TGLProxyObject, 'ProxyObject', glsOCProxyObjects);
      RegisterSceneObject(TGLColorProxy, 'ColorProxy', glsOCProxyObjects);
      RegisterSceneObject(TGLFreeFormProxy, 'FreeFormProxy', glsOCProxyObjects);
      RegisterSceneObject(TGLMaterialProxy, 'MaterialProxy', glsOCProxyObjects);
      RegisterSceneObject(TGLActorProxy, 'ActorProxy', glsOCProxyObjects);
      RegisterSceneObject(TGLMultiProxy, 'MultiProxy', glsOCProxyObjects);
      RegisterSceneObject(TGLMaterialMultiProxy, 'MaterialMultiProxy', glsOCProxyObjects);

      // Other objects.
      RegisterSceneObject(TGLDirectOpenGL, 'Direct OpenGL', '');
      RegisterSceneObject(TGLRenderPoint, 'Render Point', '');
      RegisterSceneObject(TGLImposter, 'Imposter Sprite', '');
      RegisterSceneObject(TGLFeedback, 'OpenGL Feedback', '');
   end;

finalization

   ObjectManager.Free;

end.
