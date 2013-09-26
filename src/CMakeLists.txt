ADD_DEFINITIONS(-DOF_LIBRARY) # Needed to compile OpenFrames as a library

# Define the library name that we will create
SET(LIB_NAME ${PROJECT_NAME})

# Define the headers used to compile
INCLUDE_DIRECTORIES(${OpenFrames_SOURCE_DIR}/include ${OPENSCENEGRAPH_INCLUDE_DIRS})
SET(HEADER_PATH ${OpenFrames_SOURCE_DIR}/include/${LIB_NAME})
SET(LIB_PUBLIC_HEADERS
    ${HEADER_PATH}/CoordinateAxes
    ${HEADER_PATH}/CurveArtist
    ${HEADER_PATH}/DepthPartitionNode
    ${HEADER_PATH}/DescendantTracker
    ${HEADER_PATH}/DistanceAccumulator
    ${HEADER_PATH}/DrawableTrajectory
    ${HEADER_PATH}/Export
    ${HEADER_PATH}/FrameManager
    ${HEADER_PATH}/FramePathVerifier
    ${HEADER_PATH}/FrameTracker
    ${HEADER_PATH}/FrameTransform
    ${HEADER_PATH}/FramerateLimiter
    ${HEADER_PATH}/LatLonGrid
    ${HEADER_PATH}/MarkerArtist
    ${HEADER_PATH}/Model
    ${HEADER_PATH}/OF_FORTRAN
    ${HEADER_PATH}/RadialPlane
    ${HEADER_PATH}/ReferenceFrame
    ${HEADER_PATH}/RenderRectangle
    ${HEADER_PATH}/SegmentArtist
    ${HEADER_PATH}/Sphere
    ${HEADER_PATH}/SubtreeTracker
    ${HEADER_PATH}/Trajectory
    ${HEADER_PATH}/TrajectoryArtist
    ${HEADER_PATH}/TransformAccumulator
    ${HEADER_PATH}/Vector
    ${HEADER_PATH}/View
    ${HEADER_PATH}/WindowProxy
)

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
  FILES ${LIB_PUBLIC_HEADERS}
  DESTINATION include/${LIB_NAME}
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

  SET(FMODDIR ${CMAKE_BINARY_DIR}/modules)
  SET(CMAKE_Fortran_MODULE_DIRECTORY ${FMODDIR})

  ADD_LIBRARY(${LIB_NAME}_Fortran MODULE
    OpenFrames.f90
  )

  # Specify where the Fortran module should be installed
  INSTALL(
    FILES ${FMODDIR}/OpenFrames.mod
    DESTINATION lib
  )

ENDIF(OF_FORTRAN_MODULE)
