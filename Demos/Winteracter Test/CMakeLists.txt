# Only continue if Fortran was found and X11 is being used
IF(NOT OF_FORTRAN_MODULE OR NOT OF_USE_X11)
  RETURN()
ENDIF()

# Find Winteracter
SET(WINTER_ROOT "$ENV{WINTER}" CACHE PATH "Root directory of Winteracter installation")
IF(NOT WINTER_ROOT)
  MESSAGE(WARNING "Winteracter root directory not found. Please set WINTER_ROOT variable. Will not build Winteracter example.")
  RETURN()
ENDIF()
SET(WINTER_INCLUDE_DIR "${WINTER_ROOT}/lib.ifi64")
LINK_DIRECTORIES(${WINTER_INCLUDE_DIR} ${X11LIBDIR})

# Find Open Motif
IF(APPLE)
  FIND_LIBRARY(WINTER_MOTIF_LIB
    Xm
    PATHS ${WINTER_INCLUDE_DIR} ${X11LIBDIR}
    )
  IF(NOT WINTER_MOTIF_LIB)
    MESSAGE(WARNING "Open Motif NOT FOUND. Please add the directory containing libXm to CMAKE_LIBRARY_PATH. Will not build Winteracter example.")
    RETURN()
  ENDIF()
ENDIF()

# Custom step to build resource.rc file using Winteracter's rc compiler
# Disabled for now
#ADD_CUSTOM_TARGET(trivial_rc 
#  "${WINTER_ROOT}/bin/rc" -cif8 "-sarch x86_64" -o "${CMAKE_CURRENT_BINARY_DIR}/resource.o" resource.rc
#  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
#  VERBATIM
#  )

SET(TARGET_NAME wint_trivial)
ADD_EXECUTABLE(wint_trivial trivial.f90)
TARGET_LINK_LIBRARIES(wint_trivial OpenFrames_Interface_IFORT ${X11_X11_LIB} ${X11_Xext_LIB} ${X11_Xmu_LIB} ${X11_Xft_LIB} ${WINTER_MOTIF_LIB} wint wintGL winticonv Xt fontconfig iconv ${OPENGL_LIBRARIES})
SET_TARGET_PROPERTIES(wint_trivial PROPERTIES 
  LINK_FLAGS "-arch x86_64"
  DEBUG_POSTFIX "d"
  )
TARGET_INCLUDE_DIRECTORIES(wint_trivial PUBLIC "${WINTER_INCLUDE_DIR}" ${PROJECT_BINARY_DIR}/modules)

INSTALL(
  TARGETS wint_trivial
  RUNTIME DESTINATION bin
  )
