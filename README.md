OpenFrames
==========

An API that provides real-time interactive 3D graphics for scientific simulations.

-------------------------------------------------------------------------

**Copyright 2013 Ravishankar Mathur**
**OpenFrames** is distributed under the Apache v2 License. See the LICENSE file for details.

-------------------------------------------------------------------------

**OpenFrames** is an Application Programming Interface (API) that provides the ability to add interactive 3D graphics to any application. This readme file provides a quick and dirty introduction to OpenFrames.

As a preface, it should be noted that OpenFrames is based on several existing APIs. The major one is OpenGL, which is a graphics API included with all mainstream operating systems and supported by all mainstream graphics hardware. Other API used is OpenSceneGraph (explained shortly). Note that the "Open" prefix in these APIs means that they are Open Source. An Open Source API is one for which the source code is freely available (with licensing restrictions) to anyone. Read more about the Open Source initiative at http://www.opensource.org.

OpenSceneGraph is an API that provides cross-platform scene graph management. A scene graph is a concept that groups objects with similar characteristics together. This grouping then enables operations to be quickly applied to groups of objects. Read more about it at http://www.openscenegraph.org.

Instructions to build and install OpenFrames are available in the INSTALL file at the root of the source code distribution.

OpenSceneGraph is freely downloadable (under a modified LGPL license) from the Internet, and is required to use the OpenFrames API. While its presence is required, you do not have to know anything about it to use OpenFrames. You can download OpenSceneGraph by going to http://www.openscenegraph.org and getting the latest binary distribution for your operating system. Install these binaries, and you will be ready to use OpenFrames. Note that the installer will also install some example applications that you should try out to make sure that everything works correctly before attempting to compile OpenFrames.

Having installed and tested OpenSceneGraph, you should be ready to install OpenFrames. Go to the VisualStudio directory, and open up the 'VisualStudio' workspace (in VS .NET 7 or better). Make sure that the Win32 Release configuration is active, and build the project. If the stars are aligned correctly, you will get an OpenFrames.dll in the OpenFrames/bin/ directory and an OpenFrames.lib in the OpenFrames/lib/ directory. You should add the OpenFrames.lib file to the linker's list in your own project. Alternatively, you can use the CMake GUI to build OpenFrames, but it is suggested that you do so in a "build" subdirectory of the main OpenFrames directory.

FORTRAN compatibility is provided through the FORTRAN project in the OpenFrames solution. Build the project, and you should get the OpenFrames.mod module in the /lib/ directory. You should USE this module in your FORTRAN program to be able to use OpenFrames capabilities.
