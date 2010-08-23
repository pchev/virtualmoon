//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLStrings<p>

	String constants that are used in many GLScene units<p>

      $Log: glstrings.pas,v $
      Revision 1.1  2006/01/10 20:50:46  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.3  2006/01/09 20:45:50  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/12/04 16:53:06  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.1  2005/12/01 21:24:11  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/08/03 00:41:39  z0m3ie
      - added automatical generated History from CVS

	<b>History :</b><font size=-1><ul>
      23/02/07 - DaStr - Added glsDot, glsUnsupportedType, glsUncompatibleTypes,
                         glsUnknownType, glsShaderNeedsAtLeastOneLightSource(Ex),
                         glsCadencerNotDefined(Ex), glsSceneViewerNotDefined
      16/02/07 - DaStr - Added glsOCProxyObjects, glsError, glsErrorEx,
                         glsMatLibNotDefined, glsMaterialNotFoundInMatlib(Ex)
      26/08/02 - EG - Added missing header, added glsUnknownExtension
	</ul></font>
}
unit GLStrings;

interface

const
  // General
  glsDot = '.';
  glsError   = 'Error!';
  glsErrorEx = 'Error: ';

resourcestring
  // SceneViewer
  glsNoRenderingContext = 'Could not create a rendering context';
  glsWrongVersion       = 'Need at least OpenGL version 1.1';
  glsTooManyLights      = 'Too many lights in the scene';
  glsDisplayList        = 'Failed to create a new display list for object ''%s''';
  glsWrongBitmapCanvas  = 'Couldn''t create a rendering context for the given bitmap';
  glsWrongPrinter       = 'Couldn''t render to printer';
  glsAlreadyRendering   = 'Already rendering';
  glsSceneViewerNotDefined = {glsErrorEx +} 'SceneViewer not defined!';

  // GLCadencer
  glsCadencerNotDefined   = {glsErrorEx +} 'Cadencer not defined!';
  glsCadencerNotDefinedEx = {glsErrorEx +} 'Cadencer not defined for  the ''%s'' component';

  // Shaders
  glsShaderNeedsAtLeastOneLightSource   = {glsErrorEx +} 'This shader needs at least one LightSource!';
  glsShaderNeedsAtLeastOneLightSourceEx = {glsErrorEx +} 'Shader ''%s'' needs at least one LightSource!';

  // GLTree
  glsSceneRoot  = 'Scene root';
  glsObjectRoot = 'Scene objects';
  glsCameraRoot = 'Cameras';
  glsCamera     = 'Camera';

  // GLTexture
  glsImageInvalid = 'Could not load texture, image is invalid';
  glsNoNewTexture = 'Could not get new texture name';

  // GLMaterials
  glsMatLibNotDefined = {glsErrorEx +} 'Material Library not defined!';
  glsMaterialNotFoundInMatlib = {glsErrorEx +} 'Material not found in current Material Library!';
  glsMaterialNotFoundInMatlibEx = {glsErrorEx +} 'Material "%s" not found in current Material Library!';

  // GLObjects
  glsSphereTopBottom = 'The top angle must be higher than the bottom angle';
  glsSphereStartStop = 'The start angle must be smaller than then stop angle';
  glsMaterialNotFound = 'Loading failed: could not find material %s';
  glsInterleaveNotSupported = 'Interleaved Array format not supported yet. Sorry.';

  // common messages
  glsOutOfMemory = 'Fatal: Out of memory';
  glsFailedOpenFile = 'Could not open file: %s';
  glsNoDescriptionAvailable = 'No description available';
  glsUnBalancedBeginEndUpdate = 'Unbalanced Begin/EndUpdate';
  glsUnknownExtension = 'Unknown file extension (%s), maybe you forgot to add the support '
                       +'unit to your uses? (%s?)' ;

  glsUncompatibleTypes = {glsErrorEx +} 'uncompatible types!';
  glsUnknownType       = {glsErrorEx +} 'unknown type!';
  glsUnsupportedType   = {glsErrorEx +} 'unsupported type!';

  // object categories
  glsOCBasicGeometry = 'Basic geometry';
  glsOCAdvancedGeometry = 'Advanced geometry';
  glsOCMeshObjects = 'Mesh objects';
  glsOCParticleSystems = 'Particle systems';
  glsOCEnvironmentObjects = 'Environment objects';
  glsOCSpecialObjects = 'Special objects';
  glsOCGraphPlottingObjects = 'Graph-plotting objects';
  glsOCDoodad = 'Doodad objects';
  glsOCHUDObjects = 'HUD objects';
  glsOCGuiObjects = 'GUI objects';
  glsOCProxyObjects = 'Proxy objects';

implementation

end.

