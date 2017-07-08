# Specify that we should look for headers locally
INCLUDE_DIRECTORIES(${OpenFrames_SOURCE_DIR}/include ${OPENSCENEGRAPH_INCLUDE_DIRS})

# Create alias for current demo
SET( curr_demo ofcinterface )

# Specify which file should be compiled for current test
ADD_EXECUTABLE(${curr_demo} ${curr_demo}.cpp)

# Tell linker to link against OpenFrames C-Interface library
TARGET_LINK_LIBRARIES(${curr_demo} OpenFrames_Interface_C ${OPENSCENEGRAPH_LIBRARIES})

# Executable postfix needs to be explicitly specified
SET_TARGET_PROPERTIES(${curr_demo} PROPERTIES DEBUG_POSTFIX ${CMAKE_DEBUG_POSTFIX})

# Install demo in CMAKE_INSTALL_PREFIX/bin directory
INSTALL(
  TARGETS ${curr_demo}
  RUNTIME DESTINATION bin
  )
