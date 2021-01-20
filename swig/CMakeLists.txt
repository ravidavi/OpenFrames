IF(OF_PYTHON_MODULE)
  MESSAGE(STATUS "OpenFrames enabling Python interface") 

  # Handle SWIG policy warning
  cmake_policy(SET CMP0078 "OLD")
  cmake_policy(SET CMP0086 "NEW")

  # Include CMake SWIG package
  FIND_PACKAGE(SWIG REQUIRED)
  INCLUDE(${SWIG_USE_FILE})

  # Requires Python3
  # We use this instead of FindPython since that has some issues finding Python3 on OSX
  FIND_PACKAGE(Python3 REQUIRED COMPONENTS Development)

  # Disable automatic linking of pythonXY_d.lib to use the release version of python.lib
  ADD_DEFINITIONS(-DSWIG_PYTHON_INTERPRETER_NO_DEBUG)
  # With Visual Studio we need to turn off iterator debugging because we are linking against the release version of pythonXY.lib
  IF(MSVC)
    ADD_DEFINITIONS(-D_ITERATOR_DEBUG_LEVEL=0)
  ENDIF()

  # --------------------------
  # ----- PyOFInterfaceC -----

  # Set appropriate SWIG flags
  SET_PROPERTY(SOURCE PyOFInterface.i PROPERTY CPLUSPLUS ON)
  SET_PROPERTY(SOURCE PyOFInterface.i PROPERTY USE_TARGET_INCLUDE_DIRECTORIES ON)

  # Since the SWIG source file references headers implicitly, CMake does not know where they are located
  # and therefore does not add them as SWIG dependencies. So we must collect those files and tell CMake
  # to add them as dependencies explicitly. We use "swig -MM" for this (see Swig docs).
  GET_TARGET_PROPERTY(SWIG_INCL_DIRS OpenFrames_Interface_C INCLUDE_DIRECTORIES)
  LIST(TRANSFORM SWIG_INCL_DIRS PREPEND "-I") # Indicate include directory
  EXECUTE_PROCESS(COMMAND ${SWIG_EXECUTABLE} -MM -python ${SWIG_INCL_DIRS} PyOFInterface.i
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                  OUTPUT_VARIABLE SWIG_HEADER_DEPS
                  )
  STRING(REPLACE "\\" ";" SWIG_HEADER_DEPS ${SWIG_HEADER_DEPS}) # Turn into CMake list
  LIST(TRANSFORM SWIG_HEADER_DEPS STRIP) # Remove leading/trailing whitespaces from each filepath
  LIST(REMOVE_AT SWIG_HEADER_DEPS 0 1) # First two files are not actually dependencies
  SET_PROPERTY(SOURCE PyOFInterface.i PROPERTY DEPENDS ${SWIG_HEADER_DEPS})

  # On windows the _WIN64 is autodefined by the compiler cl.exe when using the Win64 toolchain
  IF(${CMAKE_SIZEOF_VOID_P} EQUAL 8)
    SET_PROPERTY(SOURCE PyOFInterface.i PROPERTY SWIG_FLAGS -D_WIN64 -DOF_EXPORT)
  ELSE()
    SET_PROPERTY(SOURCE PyOFInterface.i PROPERTY SWIG_FLAGS -DOF_EXPORT)
  ENDIF()
  SOURCE_GROUP("Generated" FILES ${CMAKE_CURRENT_BINARY_DIR}/PyOFInterface.py)

  # Define SWIG Python library
  SWIG_ADD_LIBRARY(PyOFInterfaceC LANGUAGE python SOURCES PyOFInterface.i)
  SWIG_LINK_LIBRARIES(PyOFInterfaceC Python3::Module OpenFrames_Interface_C)
  SET_TARGET_PROPERTIES(${SWIG_MODULE_PyOFInterfaceC_REAL_NAME} PROPERTIES DEBUG_POSTFIX "")

  # ----------------
  # ----- PyOF -----

  # FIXME: Is this needed?
  INCLUDE_DIRECTORIES(${OSG_INCLUDE_DIR})

  # Set appropriate SWIG flags
  SET_PROPERTY(SOURCE PyOF.i PROPERTY CPLUSPLUS ON)
  SET_PROPERTY(SOURCE PyOF.i PROPERTY USE_TARGET_INCLUDE_DIRECTORIES ON)

  # Get SWIG source file dependency headers (see comment above for details))
  GET_TARGET_PROPERTY(SWIG_INCL_DIRS_OF OpenFrames INCLUDE_DIRECTORIES)
  LIST(TRANSFORM SWIG_INCL_DIRS_OF PREPEND "-I") # Indicate include directory
  SET(SWIG_INCL_DIRS_OSG ${OPENSCENEGRAPH_LIBRARIES})
  LIST(TRANSFORM SWIG_INCL_DIRS_OSG PREPEND "-I") # Indicate include directory
  EXECUTE_PROCESS(COMMAND ${SWIG_EXECUTABLE} -MM -python ${SWIG_INCL_DIRS_OF} ${SWIG_INCL_DIRS_OSG} PyOF.i
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                  OUTPUT_VARIABLE SWIG_HEADER_DEPS
                  )
  STRING(REPLACE "\\" ";" SWIG_HEADER_DEPS ${SWIG_HEADER_DEPS}) # Turn into CMake list
  LIST(TRANSFORM SWIG_HEADER_DEPS STRIP) # Remove leading/trailing whitespaces from each filepath
  LIST(REMOVE_AT SWIG_HEADER_DEPS 0 1) # First two files are not actually dependencies
  SET_PROPERTY(SOURCE PyOF.i PROPERTY DEPENDS ${SWIG_HEADER_DEPS})

  # On windows the _WIN64 is autodefined by the compiler cl.exe when using the Win64 toolchain
  IF(${CMAKE_SIZEOF_VOID_P} EQUAL 8)
    SET_PROPERTY(SOURCE PyOF.i PROPERTY SWIG_FLAGS -D_WIN64 -DOF_EXPORT)
  ELSE()
    SET_PROPERTY(SOURCE PyOF.i PROPERTY SWIG_FLAGS -DOF_EXPORT)
  ENDIF()
  SOURCE_GROUP("Generated" FILES ${CMAKE_CURRENT_BINARY_DIR}/PyOF.py)

  # define Python bindings target
  SWIG_ADD_LIBRARY(PyOF LANGUAGE python SOURCES PyOF.i)
  SWIG_LINK_LIBRARIES(PyOF Python3::Module OpenFrames ${OPENSCENEGRAPH_LIBRARIES})
  SET_TARGET_PROPERTIES(${SWIG_MODULE_PyOF_REAL_NAME} PROPERTIES DEBUG_POSTFIX "")

  # ------------------------------
  # ----- PyOFInterfaceIFORT -----

  IF(OF_FORTRAN_MODULE)
    # Work around for Visual Studio where swig header dependencies are not properly detected
    ADD_CUSTOM_COMMAND(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/PyOFInterfaceIFORT.i COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/configure_PyOFInterfaceIFORT.cmake DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/PyOFInterface.i)
    SET_PROPERTY(SOURCE ${CMAKE_CURRENT_BINARY_DIR}/PyOFInterfaceIFORT.i PROPERTY CPLUSPLUS ON)
    SET_PROPERTY(SOURCE ${CMAKE_CURRENT_BINARY_DIR}/PyOFInterfaceIFORT.i USE_TARGET_INCLUDE_DIRECTORIES ON)
    # On windows the _WIN64 is autodefined by the compiler cl.exe when using the Win64 toolchain
    IF(${CMAKE_SIZEOF_VOID_P} EQUAL 8)
      SET_PROPERTY(SOURCE ${CMAKE_CURRENT_BINARY_DIR}/PyOFInterfaceIFORT.i PROPERTY SWIG_FLAGS -D_WIN64 -DOF_EXPORT)
    ELSE()
      SET_PROPERTY(SOURCE ${CMAKE_CURRENT_BINARY_DIR}/PyOFInterfaceIFORT.i PROPERTY SWIG_FLAGS -DOF_EXPORT)
    ENDIF()
    SOURCE_GROUP("Generated" FILES ${CMAKE_CURRENT_BINARY_DIR}/PyOFInterfaceIFORT.i ${CMAKE_CURRENT_BINARY_DIR}/PyOFInterfaceIFORT.py)

    # define Python bindings target
    SWIG_ADD_LIBRARY(PyOFInterfaceIFORT LANGUAGE python SOURCES ${CMAKE_CURRENT_BINARY_DIR}/PyOFInterfaceIFORT.i)
    SWIG_LINK_LIBRARIES(PyOFInterfaceIFORT Python3::Module OpenFrames_Interface_IFORT)
    SET_TARGET_PROPERTIES(${SWIG_MODULE_PyOFInterfaceIFORT_REAL_NAME} PROPERTIES DEBUG_POSTFIX  "")
  ENDIF( OF_FORTRAN_MODULE )

  # Set RPATH to find shared libraries in default locations on Mac/Linux
  IF(UNIX)
    IF(APPLE)
      SET(MAC_RPATHS "@loader_path/;@loader_path/../../lib/")
      SET_TARGET_PROPERTIES(${SWIG_MODULE_PyOFInterfaceC_REAL_NAME} PROPERTIES INSTALL_RPATH
        "${MAC_RPATHS}"
      )
      SET_TARGET_PROPERTIES(${SWIG_MODULE_PyOF_REAL_NAME} PROPERTIES INSTALL_RPATH
        "${MAC_RPATHS}"
      )
      IF( OF_FORTRAN_MODULE )
        SET_TARGET_PROPERTIES(${SWIG_MODULE_PyOFInterfaceIFORT_REAL_NAME} PROPERTIES INSTALL_RPATH
          "${MAC_RPATHS}"
        )
      ENDIF( OF_FORTRAN_MODULE )
    ELSE()
      SET_TARGET_PROPERTIES(${SWIG_MODULE_PyOFInterfaceC_REAL_NAME} PROPERTIES INSTALL_RPATH
      "\$ORIGIN/"
      )
      SET_TARGET_PROPERTIES(${SWIG_MODULE_PyOF_REAL_NAME} PROPERTIES INSTALL_RPATH
      "\$ORIGIN/"
      )
      IF( OF_FORTRAN_MODULE )
        SET_TARGET_PROPERTIES(${SWIG_MODULE_PyOFInterfaceIFORT_REAL_NAME} PROPERTIES INSTALL_RPATH
        "\$ORIGIN/"
        )
      ENDIF( OF_FORTRAN_MODULE )
    ENDIF()
  ENDIF()

  # Install Swig modules
  INSTALL(TARGETS ${SWIG_MODULE_PyOFInterfaceC_REAL_NAME} LIBRARY DESTINATION ${OF_INTERFACE_DIR})
  INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/PyOFInterfaceC.py DESTINATION ${OF_INTERFACE_DIR})
  INSTALL(TARGETS ${SWIG_MODULE_PyOF_REAL_NAME} LIBRARY DESTINATION ${OF_INTERFACE_DIR})
  INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/PyOF.py DESTINATION ${OF_INTERFACE_DIR})
  IF( OF_FORTRAN_MODULE )
    INSTALL(TARGETS ${SWIG_MODULE_PyOFInterfaceIFORT_REAL_NAME} LIBRARY DESTINATION ${OF_INTERFACE_DIR})
    INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/PyOFInterfaceIFORT.py DESTINATION ${OF_INTERFACE_DIR})
  ENDIF( OF_FORTRAN_MODULE )
ENDIF()
