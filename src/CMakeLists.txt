ADD_DEFINITIONS(-DOF_LIBRARY) # Needed to compile OpenFrames as a library

# Define the library name that we will create
SET(LIB_NAME OpenFrames)

# Define the headers used to compile
SET(OF_HEADER_PATH ${OpenFrames_SOURCE_DIR}/include)
FILE(GLOB OF_HEADER_FILES "${OF_HEADER_PATH}/OpenFrames/*.hpp")

# Define all the source files needed to compile
IF(Qt5_FOUND)
  set(CMAKE_AUTOMOC ON)
ENDIF()
ADD_LIBRARY(${LIB_NAME} SHARED
    ${OF_HEADER_FILES}
    ${OF_HEADER_PATH}/OpenFrames/Export.h
    CoordinateAxes.cpp
    CurveArtist.cpp
    DepthPartitioner.cpp
    DescendantTracker.cpp
    DistanceAccumulator.cpp
    DrawableTrajectory.cpp
    FramePathVerifier.cpp
    FramePointer.cpp
    FrameTracker.cpp
    FrameTransform.cpp
    FramerateLimiter.cpp
    LatLonGrid.cpp
    MarkerArtist.cpp
    Model.cpp
    OpenVRDevice_Common.cpp
    RadialPlane.cpp
    ReferenceFrame.cpp
    RenderRectangle.cpp
    SegmentArtist.cpp
    SkySphere.cpp
    Sphere.cpp
    SubtreeTracker.cpp
    Trajectory.cpp
    TrajectoryArtist.cpp
    TrajectoryFollower.cpp
    TransformAccumulator.cpp
    Utilities.cpp
    Vector.cpp
    View.cpp
    VRUtils.cpp
    WindowProxy.cpp
)
IF(Qt5_FOUND)
  set(CMAKE_AUTOMOC OFF)
ENDIF()

# Include Qt widget panel if Qt was found
IF(Qt5_FOUND)
  TARGET_SOURCES(${LIB_NAME} PRIVATE QWidgetPanel.cpp QtOSGAdapters.cpp)
ENDIF()

# Include OpenVR support if it was enabled
IF( ${OF_VR_TYPE} MATCHES ${VR_OpenVRStub} )
  TARGET_SOURCES(${LIB_NAME} PRIVATE OpenVRDevice_Stub.cpp)
ELSEIF( ${OF_VR_TYPE} MATCHES ${VR_OpenVR} )
  TARGET_SOURCES(${LIB_NAME} PRIVATE OpenVRDevice.cpp)
ENDIF()

# Initialize list of all targets to be installed
SET(ALL_TARGETS ${LIB_NAME})

# Specify include directories for compiling
TARGET_INCLUDE_DIRECTORIES(${LIB_NAME} PUBLIC ${OF_HEADER_PATH} ${OPENSCENEGRAPH_INCLUDE_DIRS} ${OSGEARTH_INCLUDE_DIR})
IF( ${OF_VR_TYPE} MATCHES ${VR_OpenVR} )
  TARGET_INCLUDE_DIRECTORIES(${LIB_NAME} PUBLIC ${OPENVR_SDK_INCLUDE_DIRS})
ENDIF()

# Create C interface library
SET(CINTERFACE_NAME ${LIB_NAME}_Interface_C)
ADD_LIBRARY(${CINTERFACE_NAME} SHARED
  ${OF_HEADER_PATH}/OpenFrames/OF_Interface.h
  OF_Interface.cpp
  )
SET(ALL_TARGETS ${ALL_TARGETS} ${CINTERFACE_NAME})

# Specify calling conventions for C library
SET_PROPERTY(TARGET ${FORTINTERFACE_NAME}
  APPEND PROPERTY COMPILE_DEFINITIONS C_CALLS
  )

# Specify dependencies for linking
TARGET_LINK_LIBRARIES(${LIB_NAME} PUBLIC ${OPENSCENEGRAPH_LIBRARIES} PRIVATE ${OPENGL_LIBRARIES} ${OSGEARTH_LIBRARY})
IF(Qt5_FOUND)
  TARGET_LINK_LIBRARIES(${LIB_NAME} PRIVATE Qt5::Core Qt5::Widgets Qt5::OpenGL)
ENDIF()
IF(${OF_VR_TYPE} MATCHES ${VR_OpenVR})
  TARGET_LINK_LIBRARIES(${LIB_NAME} PRIVATE ${OPENVR_SDK_LIBRARIES})
ENDIF()
TARGET_LINK_LIBRARIES(${CINTERFACE_NAME} PRIVATE ${LIB_NAME})

# Additional includes/libs for X11
IF(OF_USE_X11)
  TARGET_INCLUDE_DIRECTORIES(${CINTERFACE_NAME} PRIVATE ${X11_X11_INCLUDE_PATH})
  TARGET_LINK_LIBRARIES(${CINTERFACE_NAME} PRIVATE ${X11_X11_LIB})
  TARGET_COMPILE_DEFINITIONS(${CINTERFACE_NAME} PRIVATE OF_USE_X11)
ENDIF()

# The Fortran module project
IF(OF_FORTRAN_MODULE)
  SET(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

  IF(Fortran_COMPILER_NAME MATCHES "ifort.*")
    SET(FORTINTERFACE_NAME ${LIB_NAME}_Interface_IFORT)
    SET(CALL_TYPE IFORT_CALLS)
  ELSEIF(Fortran_COMPILER_NAME MATCHES "gfortran.*")
    SET(FORTINTERFACE_NAME ${LIB_NAME}_Interface_GFORTRAN)
    SET(CALL_TYPE GFORTRAN_CALLS)
  ELSE()
    MESSAGE(WARNING "Fortran compiler ${Fortran_COMPILER_NAME} not supported")
    RETURN()
  ENDIF()

  ADD_LIBRARY(${FORTINTERFACE_NAME} SHARED
    ${OF_HEADER_PATH}/OpenFrames/OF_Interface.h
    OF_Interface.cpp
    OpenFrames.f90
    ) 
  SET(ALL_TARGETS ${ALL_TARGETS} ${FORTINTERFACE_NAME})
  
  # Specify calling conventions for Fortran
  SET_PROPERTY(TARGET ${FORTINTERFACE_NAME}
    APPEND PROPERTY COMPILE_DEFINITIONS ${CALL_TYPE}
    )

  # Link to main OpenFrames lib
  TARGET_LINK_LIBRARIES(${FORTINTERFACE_NAME} PRIVATE ${LIB_NAME})

  # Additional includes/libs for X11
  IF(OF_USE_X11)
    TARGET_INCLUDE_DIRECTORIES(${FORTINTERFACE_NAME} PRIVATE ${X11_X11_INCLUDE_PATH})
    TARGET_LINK_LIBRARIES(${FORTINTERFACE_NAME} PRIVATE ${X11_X11_LIB})
    TARGET_COMPILE_DEFINITIONS(${FORTINTERFACE_NAME} PRIVATE OF_USE_X11)
  ENDIF()

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

# Specify where headers should be installed
INSTALL(
  DIRECTORY ${OF_HEADER_PATH}/
  DESTINATION include
)

# Specify where library should be installed
INSTALL(
  TARGETS ${ALL_TARGETS}
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin
)

# Set RPATH to find dependencies in same directory as OpenFrames libraries
# This is useful for when OpenFrames is installed alongside OSG
if(UNIX)
  if(APPLE)
    SET_TARGET_PROPERTIES(${ALL_TARGETS} PROPERTIES INSTALL_RPATH
      "@loader_path/"
      )
  else()
    SET_TARGET_PROPERTIES(${ALL_TARGETS} PROPERTIES INSTALL_RPATH
      "\$ORIGIN/"
      )
  endif()
endif()
