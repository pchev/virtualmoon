==========================================
 
   GLScene For Lazarus 1.0

==========================================

Requirements:
#############

This version of glscene needs at least:
* fpc-2.1.4 or greater
* lazarus-0.9.23 svn revision 11348 (2007-06-21) or greater

Until the next stable versions (fpc-2.2 / lazarus-0.9.24) are out, you might want to get a snapshot
from http://www.hu.freepascal.org/lazarus/ 
or install freepascal and lazarus from subversion.

Installation:
#############

ZIP:
* in "lazarus/components/" folder execute "unzip glscene-1.0.0.2.zip"
  This will create the lazarus/componets/glscene/ folder

SVN:
* in "lazarus/components/" folder execute "svn co http://svn.freepascal.org/svn/fpcprojects/glscene/trunk glscene"
  This will create the lazarus/componets/glscene/ folder

Finish the installation:
* copy "lazarus/components/glscene/globallinks/*.lpl" to "lazarus/packager/globallinks/"
* start lazarus
* select "Configure installed packages ..." from the "Components" menu
* select "glscenelazarus" from the "Available packages" list
* click "Install selection" button
* click "Save and rebuild IDE" button
* repeat the last 4 steps for the other packages you want to use

List of packages:
#################
* glscenelazarus     The main package.
* glscenecgshader    Cg Shader components
* glsceneode         ODE components
* glss_fmodlaz       FMOD component
* gls_winonly        Windows only, contains sound support over mmsystem, BASS support and GLScreenSaver component

More Information:
#################

* Browse the svn repository at "http://www.freepascal.org/cgi-bin/viewcvs.cgi/glscene/trunk/?root=fpcprojects"
* Visit and extend the lazarus wiki page at "http://wiki.lazarus.freepascal.org/GLScene"
* Visit the webpage of the original GLScene project at "http://glscene.org/"
* join freenode.net IRC and chat with
** other glscene for lazarus users on "#glscene" channel
** lazarus users and developers on "#lazarus-ide" channel
** freepascal users and developers on "#fpc" channel

Have fun!
