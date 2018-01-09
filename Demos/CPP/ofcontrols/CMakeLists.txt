# Specify that we should look for headers locally
INCLUDE_DIRECTORIES(${OpenFrames_SOURCE_DIR}/include ${OPENSCENEGRAPH_INCLUDE_DIRS})

# Create alias for current demo
SET( curr_demo ofcontrols )

# Instruct CMake to run the Qt moc automatically when needed.
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

# Find Qt
FIND_PACKAGE(Qt5 COMPONENTS Core Widgets OpenGL)
IF(NOT Qt5_FOUND)
  MESSAGE("Qt5 not found: will not build Qt controls demo.")
  RETURN()
ENDIF()

# Specify headers for Qt example
FILE(GLOB osgqtcube_HEADERS *.hpp)
LIST(APPEND osgqtcube_SOURCES
  ofcontrols.cpp
  QGraphicsViewAdapter.cpp
  QWidgetImage.cpp
  ControlPanel.cpp)

# Specify which file should be compiled for current test
ADD_EXECUTABLE(${curr_demo} ${osgqtcube_SOURCES} ${osgqtcube_HEADERS})
QT5_USE_MODULES(${curr_demo} Core Widgets OpenGL)

# Tell linker to link against OpenSceneGraph library
TARGET_LINK_LIBRARIES(${curr_demo} OpenFrames ${OPENSCENEGRAPH_LIBRARIES})

# On Mac/Linux include path to Qt in RPATH
# Note that this only applies to this demo. In a real application the Qt
# libraries/frameworks should be placed relative to the main application
# and a relative RPATH should be specified.
SET_TARGET_PROPERTIES(${curr_demo} PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)

# Executable postfix needs to be explicitly specified
SET_TARGET_PROPERTIES(${curr_demo} PROPERTIES DEBUG_POSTFIX ${CMAKE_DEBUG_POSTFIX})

# Install demo in CMAKE_INSTALL_PREFIX/bin directory
INSTALL(
  TARGETS ${curr_demo}
  RUNTIME DESTINATION bin
  )
