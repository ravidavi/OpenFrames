CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
PROJECT(OpenFrames)

# Make the makefile output verbose (for compiler error checking)
#SET(CMAKE_VERBOSE_MAKEFILE on)

# Find OpenGL
FIND_PACKAGE(OpenGL)
FIND_PACKAGE(OpenSceneGraph 3.0.0 REQUIRED osgText osgGA osgDB osgUtil osgViewer osgParticle)

# User options
OPTION( OF_BUILD_TESTS "Build Test Programs" ON )
OPTION( OF_FORTRAN_MODULE "Compile Fortran Module" OFF )

# This is a workaround to the broken OPTIONAL signature in CMake 2.8-10
IF( OF_FORTRAN_MODULE )
  ENABLE_LANGUAGE(Fortran OPTIONAL)
  GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
ENDIF( OF_FORTRAN_MODULE )

# Go to the src directory and look for CMake instructions there
ADD_SUBDIRECTORY(src)

# Go to the test directory and look for CMake instructions there
ADD_SUBDIRECTORY("Demos/CPP Test")
