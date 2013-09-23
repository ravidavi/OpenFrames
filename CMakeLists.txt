CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(OpenFrames)

# Make the makefile output verbose (for compiler error checking)
#SET(CMAKE_VERBOSE_MAKEFILE on)

# Find OpenGL
FIND_PACKAGE(OpenGL)

# User option to build test programs
OPTION( BUILD_TESTS "Build Test Programs" ON )

# Go to the src directory and look for CMake instructions there
ADD_SUBDIRECTORY(src)

# Go to the test directory and look for CMake instructions there
ADD_SUBDIRECTORY("Demos/CPP Test")
