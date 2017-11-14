# Specify that we should look for headers locally
INCLUDE_DIRECTORIES(${OpenFrames_SOURCE_DIR}/include ${OPENSCENEGRAPH_INCLUDE_DIRS})

# Find wxWidgets
FIND_PACKAGE(wxWidgets COMPONENTS net gl core base)
IF(NOT wxWidgets_FOUND)
  MESSAGE("wxWidgets not found: will not build wxWidgets demo.")
  RETURN()
ENDIF()

INCLUDE(${wxWidgets_USE_FILE})

# Specify tests that should be built
SET( curr_demo ofwxwidgets )

# Specify which file should be compiled for this demo
ADD_EXECUTABLE(${curr_demo} ${curr_demo}.cpp)

# Tell linker to link against dependencies
TARGET_LINK_LIBRARIES(${curr_demo} OpenFrames ${OPENSCENEGRAPH_LIBRARIES} ${wxWidgets_LIBRARIES})

# Executable postfix needs to be explicitly specified
SET_TARGET_PROPERTIES(${curr_demo} PROPERTIES DEBUG_POSTFIX ${CMAKE_DEBUG_POSTFIX})

# Specify to use Subsystem:Windows on VisualStudio
IF(WIN32)
  IF(MSVC)
    SET_TARGET_PROPERTIES(${curr_demo} PROPERTIES LINK_FLAGS "/SUBSYSTEM:WINDOWS")
  ENDIF(MSVC)
ENDIF(WIN32)

# Install demo in CMAKE_INSTALL_PREFIX/bin directory
INSTALL(
  TARGETS ${curr_demo}
  RUNTIME DESTINATION bin
  )
