ADD_DEFINITIONS(-DOF_LIBRARY) # Needed to compile OpenFrames as a library

# Intel Visual Fortran requires a special flag
IF( OF_FORTRAN_MODULE )
  IF(Fortran_COMPILER_NAME MATCHES "ifort.*")
    ADD_DEFINITIONS(-DIVF_CALLS)
  ENDIF(Fortran_COMPILER_NAME MATCHES "ifort.*")
ENDIF( OF_FORTRAN_MODULE )

# Define the library name that we will create
SET(LIB_NAME ${PROJECT_NAME})

# Define the headers used to compile
SET(OF_HEADER_PATH ${OpenFrames_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES(${OF_HEADER_PATH} ${OPENSCENEGRAPH_INCLUDE_DIRS})

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
    OF_FORTRAN.cpp
    RadialPlane.cpp
    ReferenceFrame.cpp
    RenderRectangle.cpp
    SegmentArtist.cpp
    Sphere.cpp
    SubtreeTracker.cpp
    Trajectory.cpp
    TrajectoryArtist.cpp
    TransformAccumulator.cpp
    Vector.cpp
    View.cpp
    WindowProxy.cpp
)

# Specify dependencies for linking
TARGET_LINK_LIBRARIES(${LIB_NAME} ${OPENGL_LIBRARIES} ${OPENSCENEGRAPH_LIBRARIES})

# Specify debug library suffix
SET_TARGET_PROPERTIES(${LIB_NAME} PROPERTIES DEBUG_POSTFIX "d")

# Specify where headers should be installed
INSTALL(
  DIRECTORY ${OF_HEADER_PATH}/
  DESTINATION include
)

# Specify where library should be installed
INSTALL(
  TARGETS ${LIB_NAME}
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin
)

# The Fortran module project
IF(OF_FORTRAN_MODULE)

  SET(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

  ADD_LIBRARY(${LIB_NAME}_Fortran SHARED
    OpenFrames.f90
  )

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

ENDIF(OF_FORTRAN_MODULE)
