ADD_DEFINITIONS(-DOF_LIBRARY) # Needed to compile OpenFrames as a library

# Define the library name that we will create
SET(LIB_NAME OpenFrames)

# Define the headers used to compile
SET(OF_HEADER_PATH ${OpenFrames_SOURCE_DIR}/include)

# Define all the source files needed to compile
ADD_LIBRARY(${LIB_NAME} SHARED
    CoordinateAxes.cpp
    CurveArtist.cpp
    DepthPartitionNode.cpp
    DescendantTracker.cpp
    DistanceAccumulator.cpp
    DrawableTrajectory.cpp
    FramePathVerifier.cpp
    FrameTracker.cpp
    FrameTransform.cpp
    FramerateLimiter.cpp
    LatLonGrid.cpp
    MarkerArtist.cpp
    Model.cpp
    RadialPlane.cpp
    ReferenceFrame.cpp
    RenderRectangle.cpp
    SegmentArtist.cpp
    Sphere.cpp
    StarField.cpp
    SubtreeTracker.cpp
    Trajectory.cpp
    TrajectoryArtist.cpp
    TransformAccumulator.cpp
    Vector.cpp
    View.cpp
    WindowProxy.cpp
)

# Create C interface library
SET(INTERFACE_NAME ${LIB_NAME}_Interface_C)
ADD_LIBRARY(${INTERFACE_NAME} SHARED
  OF_Interface.cpp
  )

# Specify include directories for compiling
TARGET_INCLUDE_DIRECTORIES(${LIB_NAME} PUBLIC ${OF_HEADER_PATH} ${OPENSCENEGRAPH_INCLUDE_DIRS})
#TARGET_INCLUDE_DIRECTORIES(${INTERFACE_NAME} PUBLIC ${OF_HEADER_PATH} PRIVATE ${OPENSCENEGRAPH_INCLUDE_DIRS})

# Specify dependencies for linking
TARGET_LINK_LIBRARIES(${LIB_NAME} PUBLIC ${OPENSCENEGRAPH_LIBRARIES} PRIVATE ${OPENGL_LIBRARIES})
TARGET_LINK_LIBRARIES(${INTERFACE_NAME} PRIVATE ${LIB_NAME})

# Specify debug library suffix
SET_TARGET_PROPERTIES(${LIB_NAME} PROPERTIES DEBUG_POSTFIX "d")
SET_TARGET_PROPERTIES(${INTERFACE_NAME} PROPERTIES DEBUG_POSTFIX "d")

# Additional includes/libs for X11
IF(OF_USE_X11)
  TARGET_INCLUDE_DIRECTORIES(${INTERFACE_NAME} PRIVATE ${X11_X11_INCLUDE_PATH})
  TARGET_LINK_LIBRARIES(${INTERFACE_NAME} PRIVATE ${X11_X11_LIB})
  TARGET_COMPILE_DEFINITIONS(${INTERFACE_NAME} PRIVATE OF_USE_X11)
ENDIF()

# Specify where headers should be installed
INSTALL(
  DIRECTORY ${OF_HEADER_PATH}/
  DESTINATION include
)

# Specify where library should be installed
INSTALL(
  TARGETS ${LIB_NAME} ${INTERFACE_NAME}
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin
)

# The Fortran module project
IF(OF_FORTRAN_MODULE)

  SET(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

  IF(Fortran_COMPILER_NAME MATCHES "ifort.*")
    SET(INTERFACE_NAME ${LIB_NAME}_Interface_IFORT)
    SET(CALL_TYPE IFORT_CALLS)
  ELSEIF(Fortran_COMPILER_NAME MATCHES "gfortran.*")
    SET(INTERFACE_NAME ${LIB_NAME}_Interface_GFORTRAN)
    SET(CALL_TYPE GFORTRAN_CALLS)
  ELSE()
    MESSAGE(WARNING "Fortran compiler ${Fortran_COMPILER_NAME} not supported")
    RETURN()
  ENDIF()

  ADD_LIBRARY(${INTERFACE_NAME} SHARED
    OF_Interface.cpp
    OpenFrames.f90
    ) 
  
  # Specify calling conventions for Fortran
  SET_PROPERTY(TARGET ${INTERFACE_NAME}
    APPEND PROPERTY COMPILE_DEFINITIONS ${CALL_TYPE}
    )
  SET_TARGET_PROPERTIES(${INTERFACE_NAME} PROPERTIES DEBUG_POSTFIX "d")

  TARGET_INCLUDE_DIRECTORIES(${INTERFACE_NAME} PUBLIC ${OF_HEADER_PATH} ${OPENSCENEGRAPH_INCLUDE_DIRS})
  TARGET_LINK_LIBRARIES(${INTERFACE_NAME} PRIVATE ${LIB_NAME} ${OPENSCENEGRAPH_LIBRARIES})

  # Specify where the Fortran module should be installed
  # This is currently kind of a hack: Different compilers will generate the
  # .mod file in varying subdirectories, so we will copy all possible .mod
  # file locations and tell CMake to ignore nonexistent ones
  INSTALL(
    FILES 
    ${CMAKE_Fortran_MODULE_DIRECTORY}/OpenFrames.mod
    ${CMAKE_Fortran_MODULE_DIRECTORY}/Debug/OpenFrames.mod
    ${CMAKE_Fortran_MODULE_DIRECTORY}/Release/OpenFrames.mod
    DESTINATION lib
    OPTIONAL
    )

  # Specify where interface library should be installed
  INSTALL(
    TARGETS ${INTERFACE_NAME}
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
    )

ENDIF(OF_FORTRAN_MODULE)
