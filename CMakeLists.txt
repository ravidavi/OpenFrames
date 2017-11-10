CMAKE_MINIMUM_REQUIRED(VERSION 3.5.2)
PROJECT(osgqtcube)

# Make the makefile output verbose (for compiler error checking)
#SET(CMAKE_VERBOSE_MAKEFILE on)

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
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP4")
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
  SET(CMAKE_INSTALL_PREFIX "${PROJECT_SOURCE_DIR}/install" CACHE PATH "Installation location" FORCE)
ENDIF()

# Find OpenSceneGraph
FIND_PACKAGE(OpenSceneGraph 3.5.6 COMPONENTS osg osgText osgGA osgDB osgUtil osgViewer osgParticle)

# Give user a hint if OSG was not found
IF(NOT OPENSCENEGRAPH_FOUND)
  SET(OSG_DIR "" CACHE PATH "Set to base OpenSceneGraph install path")
  MESSAGE(FATAL_ERROR "OpenSceneGraph NOT FOUND: Please set OSG_DIR variable to the OpenSceneGraph install path and re-configure.")
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

# Instruct CMake to run the Qt moc automatically when needed.
set(CMAKE_AUTOMOC ON)

# Find Qt
FIND_PACKAGE(Qt5 COMPONENTS Core Widgets OpenGL WebEngineWidgets)
IF(NOT Qt5_FOUND)
  MESSAGE("Qt5 not found.")
  RETURN()
ENDIF()

################################################################################
#               TARGETS                                                        #
################################################################################

# Specify that we should look for headers locally
INCLUDE_DIRECTORIES(${OPENSCENEGRAPH_INCLUDE_DIRS})

# Specify headers for Qt example
FILE(GLOB osgqtcube_HEADERS *.hpp)
LIST(APPEND osgqtcube_SOURCES /
  osgqtcube.cpp /
  QGraphicsViewAdapter.cpp /
  QWidgetImage.cpp /
  OSGQuickImage.cpp /
  OSGQuickAdapter.cpp)

# Specify which file should be compiled for current test
ADD_EXECUTABLE(osgqtcube ${osgqtcube_SOURCES} ${RESOURCES} ${osgqtcube_HEADERS})
QT5_USE_MODULES(osgqtcube Core Widgets OpenGL WebEngineWidgets)

# Tell linker to link against OpenSceneGraph library
TARGET_LINK_LIBRARIES(osgqtcube Qt5::Widgets ${OPENSCENEGRAPH_LIBRARIES})

# On Mac/Linux include path to Qt in RPATH
# Note that this only applies to this demo. In a real application the Qt
# libraries/frameworks should be placed relative to the main application
# and a relative RPATH should be specified.
SET_TARGET_PROPERTIES(${curr_demo} PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)

# Executable postfix needs to be explicitly specified
SET_TARGET_PROPERTIES(osgqtcube PROPERTIES DEBUG_POSTFIX ${CMAKE_DEBUG_POSTFIX})

# Install demo in CMAKE_INSTALL_PREFIX/bin directory
#INSTALL(
#  TARGETS osgqtcube
#  RUNTIME DESTINATION bin
#  )
