# Specify that we should look for headers locally
INCLUDE_DIRECTORIES(${OpenFrames_SOURCE_DIR}/include ${OPENSCENEGRAPH_INCLUDE_DIRS})

# Create alias for current demo
SET(curr_demo ofpointing)

# Specify which file should be compiled for current test
ADD_EXECUTABLE(${curr_demo} ofpointing.cpp)

# Tell linker to link against OpenFrames library
TARGET_LINK_LIBRARIES(${curr_demo} PRIVATE OpenFrames ${OPENSCENEGRAPH_LIBRARIES})

# Enable OpenVR
IF( ${OF_VR_TYPE} MATCHES ${VR_OpenVR} )
  TARGET_INCLUDE_DIRECTORIES(${curr_demo} PUBLIC ${OPENVR_SDK_INCLUDE_DIRS})
  TARGET_LINK_LIBRARIES(${curr_demo} PRIVATE ${OPENVR_SDK_LIBRARIES})
ENDIF()

# Executable postfix needs to be explicitly specified
SET_TARGET_PROPERTIES(${curr_demo} PROPERTIES DEBUG_POSTFIX ${CMAKE_DEBUG_POSTFIX})

# Install demo in CMAKE_INSTALL_PREFIX/bin directory
INSTALL(
  TARGETS ${curr_demo}
  RUNTIME DESTINATION bin
  )
