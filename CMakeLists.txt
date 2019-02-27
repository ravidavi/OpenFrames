CMAKE_MINIMUM_REQUIRED(VERSION 3.5.2)
PROJECT(OpenFrames)

# Set where to find extra CMake modules
SET(CMAKE_MODULE_PATH "{CMAKE_MODULE_PATH}" ${CMAKE_CURRENT_SOURCE_DIR}/CMakeModules)

# Make the makefile output verbose (for compiler error checking)
#SET(CMAKE_VERBOSE_MAKEFILE on)

# User options
OPTION( OF_BUILD_DEMOS "Build Demo Programs" ON )
OPTION( OF_FORTRAN_MODULE "Compile Fortran Module" OFF )
OPTION( OF_PYTHON_MODULE "Compile Python Module" OFF )
OPTION( OF_QT_SUPPORT "Compile with Qt support" OFF )
#OPTION( OF_USE_OSGEARTH "Enable osgEarth" OFF )

# Options for VR support
SET( VR_OpenVR "OpenVR" )
SET( VR_OpenVRStub "OpenVR Stub" )
SET( OF_VR_TYPE ${VR_OpenVRStub} CACHE STRING "Compile with VR support")
SET_PROPERTY( CACHE OF_VR_TYPE PROPERTY STRINGS ${VR_OpenVR} ${VR_OpenVRStub} )

# Default to Release configuration on single-config generators
# e.g. make, but not VisualStudio or XCode
# Note that CMAKE_BUILD_TYPE is initialized to "" on the initial cmake
# run, so we check for that and set the default appropriately
if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
  SET(CMAKE_BUILD_TYPE Release CACHE STRING "Build Configuration (Release, Debug, RelWithDebInfo, or MinSizeRel" FORCE)
endif()

# Set debug postfix
SET(CMAKE_DEBUG_POSTFIX "d" CACHE STRING "Postfix for debug libraries and executables")

# Enable multiprocessor builds on Visual Studio
if(MSVC)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP")
endif()

# Enforce C++11
SET(CMAKE_CXX_STANDARD 11)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)
SET(CMAKE_CXX_EXTENSIONS OFF)

# Enable RPATH on Mac/Linux so that shared libraries can be found
# relative to each other.
if(UNIX)
  if(APPLE)
    SET(CMAKE_MACOSX_RPATH TRUE) # Enable Mac RPATH
  else()
    # Linux searches the RPATH before the system LD_LIBRARY_PATH, so enable
    # the use of RUNPATH which is searched after LD_LIBRARY_PATH. This makes
    # the Linux search order similar to Mac/Windows.
    SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--enable-new-dtags")
    SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,--enable-new-dtags")
  endif()
endif()

# Set debug preprocessor define
SET_PROPERTY(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS $<$<OR:$<CONFIG:Debug>,$<CONFIG:RelWithDebInfo>>:OF_DEBUG>)

# Set default installation location
# Can be overridden by using "-DCMAKE_INSTALL_PREFIX=/foo/bar" 
# at command line 
IF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  SET(CMAKE_INSTALL_PREFIX "${PROJECT_SOURCE_DIR}/OpenFrames-${CMAKE_SYSTEM_NAME}-install" CACHE PATH "OpenFrames installation location" FORCE)
ENDIF()

# Additional Linux compile/link flags
IF(UNIX AND NOT APPLE)
  # Tell CMake Find* commands to search for lib64 paths on Linux
  SET_PROPERTY(GLOBAL PROPERTY FIND_LIBRARY_USE_LIB64_PATHS ON)

  # Generate link errors for undefined symbols
  # This is enabled by default on OSX but not Linux
  SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,--no-undefined")
ENDIF()

# Find OpenSceneGraph
FIND_PACKAGE(OpenSceneGraph 3.5.6 COMPONENTS osg osgText osgGA osgDB osgUtil osgViewer osgParticle osgManipulator osgVolume)

# Give user a hint if OSG was not found
IF(NOT OPENSCENEGRAPH_FOUND)
  SET(OSG_DIR "" CACHE PATH "Set to base OpenSceneGraph install path")
  MESSAGE(FATAL_ERROR "OpenSceneGraph NOT FOUND: Please set OSG_DIR variable to the OpenSceneGraph install path and re-configure.")
ENDIF()

IF( OF_USE_OSGEARTH )
    # Find OsgEarth
    FIND_PACKAGE(OsgEarth)

    # Give user a hint if OsgEarth was not found
    IF(NOT OSGEARTH_FOUND)
      SET(OSGEARTH_DIR "" CACHE PATH "Set to base osgEarth install path")
      MESSAGE(FATAL_ERROR "OsgEarth NOT FOUND: Please set OSGEARTH_DIR variable to the osgEarth install path and re-configure.")
    ENDIF()
ENDIF()

# Find OpenGL
# On Mac, check if OSG is X11-based. This is required because CMake's
# FindOpenGL does not look for X11/GL itself
IF(APPLE)
  EXECUTE_PROCESS(
    COMMAND otool -L "${OSGVIEWER_LIBRARY}"
    COMMAND grep libX11
    RESULT_VARIABLE OSG_USE_X11
    OUTPUT_QUIET ERROR_QUIET
    )
  IF(OSG_USE_X11 EQUAL 0)
    SET(OF_USE_X11 TRUE)
  ELSE()
    SET(OF_USE_X11 FALSE)
  ENDIF()

  # If OSG uses X11, then we should also use X11 OpenGL
  UNSET(OPENGL_gl_LIBRARY CACHE)
  UNSET(OPENGL_glu_LIBRARY CACHE)
  UNSET(OPENGL_INCLUDE_DIR CACHE)
  IF(OF_USE_X11)
    FIND_PACKAGE(X11 REQUIRED)
    GET_FILENAME_COMPONENT(X11LIBDIR ${X11_X11_LIB} DIRECTORY)
    SET(OPENGL_gl_LIBRARY ${X11LIBDIR}/libGL.dylib CACHE PATH "OSX X11 libGL")
    SET(OPENGL_glu_LIBRARY ${X11LIBDIR}/libGLU.dylib CACHE PATH "OSX X11 libGLU")
    SET(OPENGL_INCLUDE_DIR ${X11_INCLUDE_DIR} CACHE PATH "OSX X11 OpenGL Includes")
  ENDIF()
ENDIF()
FIND_PACKAGE(OpenGL)

# This is a workaround to the broken OPTIONAL signature in CMake 2.8-10
IF( OF_FORTRAN_MODULE )
  ENABLE_LANGUAGE(Fortran OPTIONAL)
  GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
ENDIF( OF_FORTRAN_MODULE )

# Find OpenVR
IF( ${OF_VR_TYPE} MATCHES ${VR_OpenVRStub} )
  MESSAGE(STATUS "OpenFrames enabling stub OpenVR support")
ELSEIF( ${OF_VR_TYPE} MATCHES ${VR_OpenVR} )
  MESSAGE(STATUS "OpenFrames enabling OpenVR support")
  FIND_PACKAGE(OpenVR REQUIRED)
  SET_PROPERTY(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS USE_OPENVR)
ENDIF()

# Find Qt
IF( OF_QT_SUPPORT )
  FIND_PACKAGE(Qt5 COMPONENTS Core Widgets OpenGL)
  IF( NOT Qt5_FOUND )
    MESSAGE("Qt5 not found: will not build Qt support or demos.")
  ENDIF()
ENDIF()

# Go to the src directory and look for CMake instructions there
ADD_SUBDIRECTORY(src)
ADD_SUBDIRECTORY(swig)

# Go to the test directory and look for CMake instructions there
IF(OF_BUILD_DEMOS)
  ADD_SUBDIRECTORY("Demos")
ENDIF(OF_BUILD_DEMOS)
