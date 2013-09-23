PROJECT(OpenFrames_Tests)

# Specify that we should look for headers locally
INCLUDE_DIRECTORIES(${OpenFrames_SOURCE_DIR}/include)

# Specify tests that should be built
SET( ALL_TESTS test1 test2 test3 )

IF(BUILD_TESTS) # If user asked for test programs
  FOREACH(curr_test ${ALL_TESTS})
    # Specify which file should be compiled for current test
    ADD_EXECUTABLE(${curr_test} ${curr_test}.cpp)

    # Tell linker to link against OpenFrames library
    TARGET_LINK_LIBRARIES(${curr_test} OpenFrames)

    # Install test in its own directory, not in /usr/local/bin
    INSTALL(
      TARGETS ${curr_test}
      RUNTIME DESTINATION ${OpenFrames_Tests_SOURCE_DIR}
    )
  ENDFOREACH(curr_test)
ENDIF(BUILD_TESTS)
