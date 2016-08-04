CMAKE_MINIMUM_REQUIRED(VERSION 3.5.2)
PROJECT(OpenFrames)

# Make the makefile output verbose (for compiler error checking)
#SET(CMAKE_VERBOSE_MAKEFILE on)

# Find OpenSceneGraph
FIND_PACKAGE(OpenSceneGraph 3.4.0 COMPONENTS osg osgText osgGA osgDB osgUtil osgViewer osgParticle)

# Give user a hint if OSG was not found
IF(NOT OPENSCENEGRAPH_FOUND)
  MESSAGE(FATAL_ERROR "OpenSceneGraph NOT FOUND: Please set OSG_DIR variable to the OpenSceneGraph path and re-configure.")
ENDIF()

# Find OpenGL
# On Mac, check if OSG is X11-based. This is required because CMake's
# FindOpenGL module does not look for X11/GL itself
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

# Set default installation location
# Can be overridden by using "-DCMAKE_INSTALL_PREFIX=/foo/bar" 
# at command line 
IF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  IF(APPLE)
    IF(OF_USE_X11)
      SET(CMAKE_INSTALL_PREFIX "${PROJECT_SOURCE_DIR}/OpenFrames-${CMAKE_SYSTEM_NAME}-X11" CACHE PATH "OpenFrames installation location" FORCE)
    ELSE()
      SET(CMAKE_INSTALL_PREFIX "${PROJECT_SOURCE_DIR}/OpenFrames-${CMAKE_SYSTEM_NAME}-Cocoa" CACHE PATH "OpenFrames installation location" FORCE)
    ENDIF()
  ELSE()
    SET(CMAKE_INSTALL_PREFIX "${PROJECT_SOURCE_DIR}/OpenFrames-${CMAKE_SYSTEM_NAME}" CACHE PATH "OpenFrames installation location" FORCE)
  ENDIF()
ENDIF()

# User options
OPTION( OF_BUILD_TESTS "Build Test Programs" ON )
OPTION( OF_FORTRAN_MODULE "Compile Fortran Module" OFF )

# This is a workaround to the broken OPTIONAL signature in CMake 2.8-10
IF( OF_FORTRAN_MODULE )
  ENABLE_LANGUAGE(Fortran OPTIONAL)
  GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
  IF(Fortran_COMPILER_NAME MATCHES "ifort.*")
    SET(OF_FORTRAN_IVF TRUE)
  ELSE()
    SET(OF_FORTRAN_IVF FALSE)
  ENDIF()
ENDIF( OF_FORTRAN_MODULE )

# Go to the src directory and look for CMake instructions there
ADD_SUBDIRECTORY(src)

# Go to the test directory and look for CMake instructions there
ADD_SUBDIRECTORY("Demos/CPP Test")
ADD_SUBDIRECTORY("Demos/wxWidgets Test")
ADD_SUBDIRECTORY("Demos/Winteracter Test")
