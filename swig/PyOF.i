%module ${SWIG_MODULE_NAME}

%include "std_string.i"

%{
#include "OpenThreads/Exports"
#include "OpenThreads/Thread"

#include "OpenFrames/CoordinateAxes.hpp"
#include "OpenFrames/CurveArtist.hpp"
#include "OpenFrames/CustomLineSegments.hpp"
#include "OpenFrames/DepthPartitioner.hpp"
#include "OpenFrames/DescendantTracker.hpp"
#include "OpenFrames/DistanceAccumulator.hpp"
#include "OpenFrames/DrawableTrajectory.hpp"
#include "OpenFrames/EllipticCone.hpp"
#include "OpenFrames/FocalPointShadowMap.hpp"
#include "OpenFrames/FrameManager.hpp"
#include "OpenFrames/FramePathVerifier.hpp"
#include "OpenFrames/FramePointer.hpp"
#include "OpenFrames/FrameTracker.hpp"
#include "OpenFrames/FrameTransform.hpp"
#include "OpenFrames/FramerateLimiter.hpp"
#include "OpenFrames/LatLonGrid.hpp"
#include "OpenFrames/MarkerArtist.hpp"
#include "OpenFrames/Model.hpp"
#include "OpenFrames/OpenVRDevice.hpp"
#include "OpenFrames/PolyhedralCone.hpp"
#include "OpenFrames/RadialPlane.hpp"
#include "OpenFrames/RectangularCone.hpp"
#include "OpenFrames/ReferenceFrame.hpp"
#include "OpenFrames/RenderRectangle.hpp"
#include "OpenFrames/SegmentArtist.hpp"
#include "OpenFrames/SkySphere.hpp"
#include "OpenFrames/Sphere.hpp"
#include "OpenFrames/SubtreeTracker.hpp"
#include "OpenFrames/Trajectory.hpp"
#include "OpenFrames/TrajectoryArtist.hpp"
#include "OpenFrames/TrajectoryFollower.hpp"
#include "OpenFrames/TransformAccumulator.hpp"
#include "OpenFrames/Utilities.hpp"
#include "OpenFrames/Vector.hpp"
#include "OpenFrames/View.hpp"
#include "OpenFrames/VRUtils.hpp"
#include "OpenFrames/WindowProxy.hpp"
%}

%ignore *::cloneType;
%ignore *::clone;

%include "OpenFrames/FrameTransform.hpp" // Required for ReferenceFrame
%include "OpenFrames/ReferenceFrame.hpp" // Required for many classes
%include "OpenFrames/Trajectory.hpp" // Required for TrajectoryArtist
%include "OpenFrames/TrajectoryArtist.hpp" // Required for many classes
%include "OpenFrames/FrameTracker.hpp" // Required for DescendantTracker and many classes
%include "OpenFrames/DescendantTracker.hpp" // Required for TransformVisitor in TransformAccumulator
%include "OpenFrames/TransformAccumulator.hpp" // Required for View
%include "OpenFrames/View.hpp" // Required for many classes

%include "OpenFrames/CoordinateAxes.hpp"

%include "OpenFrames/CurveArtist.hpp"
%include "OpenFrames/CustomLineSegments.hpp"
%include "OpenFrames/DepthPartitioner.hpp"

%include "OpenFrames/DistanceAccumulator.hpp"
%include "OpenFrames/DrawableTrajectory.hpp"

%include "OpenFrames/PolyhedralCone.hpp" // Required for EllipticCone
%include "OpenFrames/EllipticCone.hpp"
%include "OpenFrames/FocalPointShadowMap.hpp"
%include "OpenFrames/FrameManager.hpp"
%include "OpenFrames/FramePathVerifier.hpp"
%include "OpenFrames/FramePointer.hpp"
%include "OpenFrames/FramerateLimiter.hpp"
%include "OpenFrames/LatLonGrid.hpp"
%include "OpenFrames/MarkerArtist.hpp"
%include "OpenFrames/Model.hpp"
%include "OpenFrames/OpenVRDevice.hpp"
%include "OpenFrames/RadialPlane.hpp"
%include "OpenFrames/RectangularCone.hpp"

%include "OpenFrames/Sphere.hpp" // Required for SkySphere
%include "OpenFrames/SkySphere.hpp" // Required for RenderRectangle
%include "OpenFrames/VRUtils.hpp" // Required for RenderRectangle
%include "OpenFrames/RenderRectangle.hpp"
%include "OpenFrames/SegmentArtist.hpp"
%include "OpenFrames/SubtreeTracker.hpp"
%include "OpenFrames/TrajectoryFollower.hpp"
%include "OpenFrames/Vector.hpp"

// Need to include these OSG classes to access the functions inherited by WindowProxy
%ignore OpenThreads::Thread::setProcessorAffinity;
%ignore OpenThreads::GetNumberOfProcessors;
%ignore OpenThreads::SetProcessorAffinityOfCurrentThread;
%include "OpenThreads/Exports"
%include "OpenThreads/Thread"
%include "OpenFrames/WindowProxy.hpp"
