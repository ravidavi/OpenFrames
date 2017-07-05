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
SET( ALL_TESTS wxtest1 wxtest_bare )

IF(OF_BUILD_TESTS) # If user asked for test programs
  FOREACH(curr_test ${ALL_TESTS})
    # Specify which file should be compiled for current test
    ADD_EXECUTABLE(${curr_test} ${curr_test}.cpp)

    # Tell linker to link against dependencies
    TARGET_LINK_LIBRARIES(${curr_test} OpenFrames ${OPENSCENEGRAPH_LIBRARIES} ${wxWidgets_LIBRARIES})

    # Specify debug library suffix
    SET_TARGET_PROPERTIES(${curr_test} PROPERTIES DEBUG_POSTFIX "d")

	# Specify to use Subsystem:Windows on VisualStudio
	IF(WIN32)
	  IF(MSVC)
	    SET_TARGET_PROPERTIES(${curr_test} PROPERTIES LINK_FLAGS "/SUBSYSTEM:WINDOWS")
	  ENDIF(MSVC)
	ENDIF(WIN32)

    # Install test in current directory, not in CMAKE_INSTALL_PREFIX directory
    # Necessary since models/images are located relative to here
    INSTALL(
      TARGETS ${curr_test}
      RUNTIME DESTINATION bin
    )
  ENDFOREACH(curr_test)

ENDIF(OF_BUILD_TESTS)
