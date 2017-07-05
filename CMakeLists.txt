CMAKE_MINIMUM_REQUIRED(VERSION 3.5.2)
PROJECT(OpenFrames)

# Set where to find extra CMake modules
SET(CMAKE_MODULE_PATH "{CMAKE_MODULE_PATH}" ${CMAKE_CURRENT_SOURCE_DIR}/CMakeModules)

# Make the makefile output verbose (for compiler error checking)
#SET(CMAKE_VERBOSE_MAKEFILE on)

# User options
OPTION( OF_BUILD_DEMOS "Build Demo Programs" ON )
OPTION( OF_FORTRAN_MODULE "Compile Fortran Module" OFF )
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

# Enable multiprocessor builds on Visual Studio
if(MSVC)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP")
endif()

# Enforce C++11
SET(CMAKE_CXX_STANDARD 11)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)
SET(CMAKE_CXX_EXTENSIONS OFF)

# Set debug preprocessor define
SET_PROPERTY(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS $<$<OR:$<CONFIG:Debug>,$<CONFIG:RelWithDebInfo>>:OF_DEBUG>)

# Set default installation location
# Can be overridden by using "-DCMAKE_INSTALL_PREFIX=/foo/bar" 
# at command line 
IF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  SET(CMAKE_INSTALL_PREFIX "${PROJECT_SOURCE_DIR}/OpenFrames-${CMAKE_SYSTEM_NAME}-install" CACHE PATH "OpenFrames installation location" FORCE)
ENDIF()

# Find OpenSceneGraph
FIND_PACKAGE(OpenSceneGraph 3.4.0 COMPONENTS osg osgText osgGA osgDB osgUtil osgViewer osgParticle)

# Give user a hint if OSG was not found
IF(NOT OPENSCENEGRAPH_FOUND)
  MESSAGE(FATAL_ERROR "OpenSceneGraph NOT FOUND: Please set OSG_DIR variable to the OpenSceneGraph install path and re-configure.")
ENDIF()

IF( OF_USE_OSGEARTH )
    # Find OsgEarth
    FIND_PACKAGE(OsgEarth)

    # Give user a hint if OsgEarth was not found
    IF(NOT OSGEARTH_FOUND)
      MESSAGE(FATAL_ERROR "OsgEarth NOT FOUND: Please set OSGEARTH_DIR variable to the OsgEarth install path and re-configure.")
    ENDIF()
ENDIF()

# Find OpenGL
# On Mac, check if OSG is X11-based. This is required because CMake's
# FindOpenGL and does not look for X11/GL itself
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
ENDIF()

# Go to the src directory and look for CMake instructions there
ADD_SUBDIRECTORY(src)

# Go to the test directory and look for CMake instructions there
IF(OF_BUILD_DEMOS)
  ADD_SUBDIRECTORY("Demos")
ENDIF(OF_BUILD_DEMOS)
