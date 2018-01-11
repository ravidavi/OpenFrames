# Specify that we should look for headers locally
INCLUDE_DIRECTORIES(${OpenFrames_SOURCE_DIR}/include ${OPENSCENEGRAPH_INCLUDE_DIRS})

# Create alias for current demo
SET( curr_demo ofqt )

# Instruct CMake to run the Qt moc automatically when needed.
set(CMAKE_AUTOMOC ON)

# Find Qt
IF(NOT OF_USE_QT)
  RETURN()
ENDIF()

# Specify headers for Qt example
FILE(GLOB OFQT_HEADERS *.h)
LIST(APPEND ofqt_SOURCES
  main.cpp
  mainwindow.cpp
  ofwidget.cpp
  ofwindow.cpp
  ofrenderpool.cpp
  renderthread.cpp
  renderproxy.cpp)

# Specify files for Qt example
ADD_EXECUTABLE(${curr_demo} ${OFQT_HEADERS} ${ofqt_SOURCES})

# Tell linker to use OpenFrames, OpenSceneGraph, and Qt5Widgets
TARGET_LINK_LIBRARIES(${curr_demo} OpenFrames ${OPENSCENEGRAPH_LIBRARIES} Qt5::Core Qt5::Widgets)

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
