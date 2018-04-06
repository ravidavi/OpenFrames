# Specify that we should look for headers locally
INCLUDE_DIRECTORIES(${OpenFrames_SOURCE_DIR}/include ${OPENSCENEGRAPH_INCLUDE_DIRS})

# Create alias for current demo
SET( curr_demo ofcontrols )

# Find Qt
# There are two methods for using Qt Ui files as your QWidgets:
# 1) Run time processing, which loads the Ui file from the disk or Qt resources. (For example resource.qrc)
# 2) Compile time processing, which builds headers and classes for your Ui file. (For example )
# See: https://doc.qt.io/qt-5/designer-using-a-ui-file.html
# See: https://cmake.org/cmake/help/v3.0/manual/cmake-qt.7.html

IF( NOT Qt5_FOUND )
  RETURN()
ENDIF()

FIND_PACKAGE(Qt5 COMPONENTS UiTools) # Library for building ui files at runtime using (1)

IF( NOT Qt5_FOUND )
  RETURN()
ENDIF()

set(CMAKE_AUTOMOC ON)

# For inclusion of resource.qrc containing movespherewidget.ui using (1)
set(CMAKE_AUTORCC ON)

# For inclusion of sphereoptionswidget.ui at compile time using (2)
set(CMAKE_AUTOUIC ON)

# Specify headers for Qt example
FILE(GLOB osgqtcube_HEADERS *.hpp)
LIST(APPEND osgqtcube_SOURCES
  ofcontrols.cpp
  ofrenderpool.cpp
  ofwindow.cpp
  renderthread.cpp
  )

# Attach the ui files as a resource using (1)
ADD_EXECUTABLE(${curr_demo} ${osgqtcube_SOURCES} ${osgqtcube_HEADERS} resources.qrc)

# Alternatively, you can prebuild ui files

# Tell linker to link against OpenSceneGraph library
TARGET_LINK_LIBRARIES(${curr_demo} OpenFrames ${OPENSCENEGRAPH_LIBRARIES} Qt5::Core Qt5::Widgets Qt5::UiTools)

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
