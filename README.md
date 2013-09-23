OpenFrames
==========

An API that provides real-time interactive 3D graphics for scientific simulations.

-------------------------------------------------------------------------

**Copyright 2013 Ravishankar Mathur**
**OpenFrames** is distributed under the Apache v2 License. See the LICENSE file for details.

-------------------------------------------------------------------------

**OpenFrames** is an Application Programming Interface (API) that provides the ability to add interactive 3D graphics to any application. This readme file provides a quick and dirty introduction to OpenFrames.

As a preface, it should be noted that OpenFrames is based on several existing APIs. The major one is OpenGL, which is a graphics API widely available on all mainstream operating systems and supported by almost all graphics hardware. Other APIs used are OpenThreads and OpenSceneGraph (explained shortly). Note that the "Open" prefix in these APIs means that they are Open Source. An Open Source API is one for which the source code is freely available (with licensing restrictions) to anyone. Read more about the Open Source initiative at http://www.opensource.org.

OpenThreads is an API that provides a uniform cross-platform threading interface. Read more about it at http://openthreads.sourceforge.net. OpenThreads is distributed as a part of OpenSceneGraph.

OpenSceneGraph is an API that provides cross-platform scene graph management. A scene graph is a concept that groups objects with similar characteristics together. This grouping then enables operations to be quickly applied to groups of objects. Read more about it at http://www.openscenegraph.org.

These two APIs are freely downloadable from the internet, and are required to use the OpenFrames API. While their presence is required, you do not have to know anything about them to use OpenFrames. You can download them all at once by going to http://www.openscenegraph.org and getting the latest binary distribution for your operating system. Install these binaries, and you will be ready to use OpenFrames. Note that the installer will also install some demos that you should try out to make sure that everything works correctly before attempting to compile OpenFrames.

Having installed and tested OpenSceneGraph, you should be ready to install OpenFrames. Go to the VisualStudio directory, and open up the 'VisualStudio' workspace (in VS .NET 7 or better). Make sure that the Win32 Release configuration is active, and build the project. If the stars are aligned correctly, you will get an OpenFrames.dll in the OpenFrames/bin/ directory and an OpenFrames.lib in the OpenFrames/lib/ directory. You should add the OpenFrames.lib file to the linker's list in your own project. Alternatively, you can use the CMake GUI to build OpenFrames, but it is suggested that you do so in a "build" subdirectory of the main OpenFrames directory.

FORTRAN compatibility is provided through the FORTRAN project in the OpenFrames solution. Build the project, and you should get the OpenFrames.mod module in the /lib/ directory. You should USE this module in your FORTRAN program to be able to use OpenFrames capabilities.
