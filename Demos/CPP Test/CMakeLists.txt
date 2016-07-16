# Specify that we should look for headers locally
INCLUDE_DIRECTORIES(${OpenFrames_SOURCE_DIR}/include ${OPENSCENEGRAPH_INCLUDE_DIRS})

# Specify tests that should be built
SET( ALL_TESTS test1 test2 test3 )

IF(OF_BUILD_TESTS) # If user asked for test programs
  # Specify IVF for Fortran source files
  IF(OF_FORTRAN_IVF)
    SET_SOURCE_FILES_PROPERTIES(
      test2.cpp
      PROPERTIES COMPILE_DEFINITIONS IVF_CALLS
      )
  ENDIF()

  FOREACH(curr_test ${ALL_TESTS})
    # Specify which file should be compiled for current test
    ADD_EXECUTABLE(${curr_test} ${curr_test}.cpp)

    # Tell linker to link against OpenFrames library
    TARGET_LINK_LIBRARIES(${curr_test} ${PROJECT_NAME} ${OPENSCENEGRAPH_LIBRARIES})

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
