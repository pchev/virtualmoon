This directory contain a copy of GlScene from 
https://sourceforge.net/p/glscene/

The command used to get the code is:
svn export  https://svn.code.sf.net/p/glscene/code/branches/GLSceneLCL glscene

The patch file Complete.patch is necessary to make it work with Lazarus 2.2.0
Apply the patch with the command:
patch -p0 < Complete.patch

In addition it contain a makefile for build convenience on the linux build server.

