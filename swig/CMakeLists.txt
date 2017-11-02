IF(OF_PYTHON_MODULE)
  MESSAGE(STATUS "OpenFrames enabling Python interface") 

  FIND_PACKAGE(SWIG REQUIRED)
  INCLUDE(${SWIG_USE_FILE})

  FIND_PACKAGE(PythonLibs REQUIRED)
  INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_DIR})
  LINK_DIRECTORIES(${PYTHON_LIB_DIR})

  INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/../include)
  INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

  IF(NOT PYTHON_DEBUG_LIBRARY)
    # This preprocessor variable disables the automatic linking of pythonXY_d.lib to use the release version of python.lib
    ADD_DEFINITIONS(-DSWIG_PYTHON_INTERPRETER_NO_DEBUG)
    # With Visual Studio we need to turn off iterator debugging because we are linking against the release version of pythonXY.lib
    IF(MSVC)
      ADD_DEFINITIONS(-D_ITERATOR_DEBUG_LEVEL=0)
    ENDIF()
  ENDIF()

  # This needs to be defined for OF_Interface.h
  SET(CMAKE_SWIG_FLAGS "-DOF_EXPORT")
  SET_PROPERTY(SOURCE PyOFInterface.i PROPERTY CPLUSPLUS ON)

  # On windows the _WIN64 is autodefined by the compiler cl.exe when using the Win64 toolchain
  IF(${CMAKE_SIZEOF_VOID_P} EQUAL 8)
     SET_SOURCE_FILES_PROPERTIES(SOURCE swig_tests.i PROPERTIES SWIG_FLAGS "-D_WIN64")
  ENDIF()

  # define Python bindings target
  SWIG_ADD_LIBRARY(PyOFInterface LANGUAGE python SOURCES PyOFInterface.i)
  SWIG_LINK_LIBRARIES(PyOFInterface ${PYTHON_LIBRARY} OpenFrames_Interface_C) # TODO Create debug versions
  SET_TARGET_PROPERTIES(_PyOFInterface PROPERTIES DEBUG_POSTFIX  "")

  # Set RPATH to find shared libraries in default locations on Mac/Linux
  IF(UNIX)
    IF(APPLE)
      SET_TARGET_PROPERTIES(_PyOFInterface PROPERTIES INSTALL_RPATH
      "@loader_path/;@loader_path/../plugins/;@loader_path/../plugins/proprietary/"
      )
    ELSE()
      SET_TARGET_PROPERTIES(_PyOFInterface PROPERTIES INSTALL_RPATH
      "\$ORIGIN/;\$ORIGIN/../plugins/;\$ORIGIN/../plugins/proprietary/"
      )
    ENDIF()
  ENDIF()

  IF(WIN32)
    INSTALL(TARGETS _PyOFInterface LIBRARY DESTINATION bin/OpenFrames)
    INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/PyOFInterface.py DESTINATION bin/OpenFrames)
  ELSE()
    # dump at bin, python modules need to execute from GmatBase for now
    SET(INSTALL_DIR bin/OpenFrames)
    INSTALL(TARGETS _PyOFInterface LIBRARY DESTINATION ${INSTALL_DIR})
    INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/PyOFInterface.py DESTINATION ${INSTALL_DIR})
  ENDIF()
ENDIF()
