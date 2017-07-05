# Specify that we should look for headers locally
INCLUDE_DIRECTORIES(${OpenFrames_SOURCE_DIR}/include ${OPENSCENEGRAPH_INCLUDE_DIRS})

# Create alias for current demo
SET( curr_demo ofrealtimeupdate )

# Specify which file should be compiled for current test
ADD_EXECUTABLE(${curr_demo} ${curr_demo}.cpp)

# Tell linker to link against OpenFrames library
TARGET_LINK_LIBRARIES(${curr_demo} OpenFrames ${OPENSCENEGRAPH_LIBRARIES})

# Specify debug library suffix
SET_TARGET_PROPERTIES(${curr_demo} PROPERTIES DEBUG_POSTFIX "d")

# Install demo in CMAKE_INSTALL_PREFIX/bin directory
INSTALL(
  TARGETS ${curr_demo}
  RUNTIME DESTINATION bin
  )
