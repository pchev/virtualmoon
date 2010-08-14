{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit glscenelazarus; 

interface

uses
    ApplicationFileIO, ARBProgram, AsyncTimer, BumpMapping, CurvesAndSurfaces, 
  GeometryBB, GLCanvas, GLKeyboard, GLTextureCombiners, HeightTileFile, jpeg, 
  MeshUtils, Octree, OpenGL1x, PerlinNoise, PersistentClasses, 
  PictureRegisteredFormats, Polynomials, SpatialPartitioning, Spline, 
  VectorGeometry, VectorLists, VectorTypes, VerletClasses, VerletHairClasses, 
  XCollection, XOpenGL, FVectorEditor, FXCollectionEditor, GLLazarusRegister, 
  GLSceneEdit, GLTextureImageEditors, Info, RegisterXCollection, GLFile3DS, 
  GLFileASE, GLFileGL2, GLFileGTS, GLFileLWO, GLFileMD2, GLFileMD3, GLFileMD5, 
  GLFileMDC, GLFileMS3D, GLFileNMF, GLFileNurbs, GLFileObj, GLFilePLY, 
  GLFileSMD, GLFileSTL, GLFileTIN, GLFileVRML, glfilex, GLVfsPAK, Q3MD3, TGA, 
  Utils3DS, GLAnimatedSprite, GLAtmosphere, GLBaseMeshSilhouette, 
  GLBehaviours, GLBitmapFont, GLBlur, GLBSP, GLBumpmapHDS, GLCadencer, 
  GLCollision, GLConsole, GLContext, GLDCE, GLDCEMisc, GLEllipseCollision, 
  GLEParticleMasksManager, GLExplosionFx, GLExtrusion, GLFeedback, GLFireFX, 
  GLGameMenu, GLGeomObjects, GLGraph, GLGraphics, GLGui, GLHeightData, 
  GLHeightTileFileHDS, GLHUDObjects, GLImposter, GLLensFlare, GLLinePFX, 
  GLMaterialScript, GLMesh, GLMeshCSG, GLMeshOptimizer, GLMirror, GLMisc, 
  GLMovement, GLMultiPolygon, GLMultiProxy, GLObjects, GLParametricSurfaces, 
  GLParticleFX, GLParticles, GLPerlin, GLPerlinBase, GLPerlinPFX, 
  GLPolyhedron, GLPortal, GLPostEffects, GLProcTextures, GLProjectedTextures, 
  GLProxyObjects, GLROAMPatch, glscanlinedgraphics, GLScene, GLScreen, 
  GLShadowPlane, GLShadowVolume, GLSilhouette, GLSkyBox, GLSkydome, 
  GLSLProjectedTextures, GLSound, GLSpatialPartitioning, GLState, GLStrings, 
  GLTeapot, GLTerrainRenderer, GLTexLensFlare, GLTexture, GLThorFX, 
  GLTilePlane, GLTrail, GLTree, GLUtils, GLVectorFileObjects, GLVerletClasses, 
  GLVerletClothify, GLVerletSkeletonColliders, GLWaterPlane, GLWindows, 
  GLzBuffer, cadencerasap, GLCrossPlatform, GLViewer, GLColor, 
  GLDynamicTexture, GLLCLViewer, GLGizmo, GLFileB3D, GLSLBumpShader, 
  GLSLDiffuseSpecularShader, GLSLPostBlurShader, GLSLShader, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLLazarusRegister', @GLLazarusRegister.Register); 
  RegisterUnit('RegisterXCollection', @RegisterXCollection.Register); 
  RegisterUnit('GLAnimatedSprite', @GLAnimatedSprite.Register); 
  RegisterUnit('GLFeedback', @GLFeedback.Register); 
  RegisterUnit('GLSound', @GLSound.Register); 
  RegisterUnit('GLTree', @GLTree.Register); 
end; 

initialization
  RegisterPackage('glscenelazarus', @Register); 
end.
