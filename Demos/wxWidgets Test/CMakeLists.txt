# Specify that we should look for headers locally
INCLUDE_DIRECTORIES(${OpenFrames_SOURCE_DIR}/include ${OPENSCENEGRAPH_INCLUDE_DIRS})

# Find wxWidgets
FIND_PACKAGE(wxWidgets REQUIRED net gl core base)
INCLUDE(${wxWidgets_USE_FILE})

# Specify tests that should be built
SET( ALL_TESTS wxtest1 wxtest_bare )

IF(OF_BUILD_TESTS) # If user asked for test programs
  FOREACH(curr_test ${ALL_TESTS})
    # Specify which file should be compiled for current test
    ADD_EXECUTABLE(${curr_test} ${curr_test}.cpp)

    # Tell linker to link against dependencies
    TARGET_LINK_LIBRARIES(${curr_test} ${PROJECT_NAME} ${OPENSCENEGRAPH_LIBRARIES} ${wxWidgets_LIBRARIES})

    # Specify debug library suffix
    SET_TARGET_PROPERTIES(${curr_test} PROPERTIES DEBUG_POSTFIX "d")

    # Install test in current directory, not in CMAKE_INSTALL_PREFIX directory
    # Necessary since models/images are located relative to here
    INSTALL(
      TARGETS ${curr_test}
      RUNTIME DESTINATION bin
    )
  ENDFOREACH(curr_test)

  INSTALL(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../Images
    DESTINATION ${CMAKE_INSTALL_PREFIX}
  )

  INSTALL(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../Models
    DESTINATION ${CMAKE_INSTALL_PREFIX}
  )
ENDIF(OF_BUILD_TESTS)
