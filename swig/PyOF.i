/***********************************
 Copyright 2020 Ravishankar Mathur
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 ***********************************/
 
// Note for developers: The destructors for the imported classes are protected
// so users may receive a warning regarding memory leaks at runtime in Python

%module(directors="1", threads="1") ${SWIG_MODULE_NAME}

// Default to no multithread support. This will be overridden for specific classes.
%nothread;

%include "std_string.i"

%{
#include "osg/Export"

#include "osg/Vec3d"
#include "osg/Quat"

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


// Code for function callbacks
%pythoncode
%{

import ctypes

# a ctypes callback prototype
py_callback_uint_bool = ctypes.CFUNCTYPE(None, ctypes.POINTER(ctypes.c_uint), ctypes.POINTER(ctypes.c_bool))
py_callback_uint = ctypes.CFUNCTYPE(None, ctypes.POINTER(ctypes.c_uint))
py_keypress_callback = ctypes.CFUNCTYPE(None, ctypes.POINTER(ctypes.c_uint), ctypes.POINTER(ctypes.c_uint), ctypes.POINTER(ctypes.c_uint), ctypes.POINTER(ctypes.c_int))
py_mousemotion_callback = ctypes.CFUNCTYPE(None, ctypes.POINTER(ctypes.c_uint), ctypes.POINTER(ctypes.c_uint), ctypes.POINTER(ctypes.c_uint), ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_float))
py_button_callback = ctypes.CFUNCTYPE(None, ctypes.POINTER(ctypes.c_uint), ctypes.POINTER(ctypes.c_uint), ctypes.POINTER(ctypes.c_uint), ctypes.POINTER(ctypes.c_uint))

%}

// Typemaps for function callbacks
%typemap(in) void (*fcn)(unsigned int*, bool*) {
    $1 = (void (*)(unsigned int*, bool*))PyLong_AsVoidPtr($input);
}
%typemap(in) void (*fcn)(unsigned int*) {
    $1 = (void (*)(unsigned int*))PyLong_AsVoidPtr($input);
}
%typemap(in) void (*fcn)(unsigned int*, unsigned int*, unsigned int*, int*) {
    $1 = (void (*)(unsigned int*, unsigned int*, unsigned int*, int*))PyLong_AsVoidPtr($input);
}
%typemap(in) void (*fcn)(unsigned int*, unsigned int*, unsigned int*, float*, float*) {
    $1 = (void (*)(unsigned int*, unsigned int*, unsigned int*, float*, float*))PyLong_AsVoidPtr($input);
}
%typemap(in) void (*fcn)(unsigned int*, unsigned int*, unsigned int*, unsigned int*) {
    $1 = (void (*)(unsigned int*, unsigned int*, unsigned int*, unsigned int*))PyLong_AsVoidPtr($input);
}

// Rewrite Python wrapper functions that use callbacks:
%feature("shadow") OpenFrames::WindowProxy::setMakeCurrentFunction %{
def setMakeCurrentFunction(self, fcn):
    f = py_callback_uint_bool(fcn)
    fcn_p = ctypes.cast(f, ctypes.c_void_p).value
    
    return _PyOF.WindowProxy_setMakeCurrentFunction(self, fcn_p)
%}

%feature("shadow") OpenFrames::WindowProxy::setUpdateContextFunction %{
def setUpdateContextFunction(self, fcn):
    f = py_callback_uint_bool(fcn)
    fcn_p = ctypes.cast(f, ctypes.c_void_p).value
    
    return _PyOF.WindowProxy_setUpdateContextFunction(self, fcn_p)
%}

%feature("shadow") OpenFrames::WindowProxy::setSwapBuffersFunction %{
def setSwapBuffersFunction(self, fcn):
    f = py_callback_uint(fcn)
    fcn_p = ctypes.cast(f, ctypes.c_void_p).value
    
    return _PyOF.WindowProxy_setSwapBuffersFunction(self, fcn_p)
%}

%feature("shadow") OpenFrames::WindowProxy::setKeyPressCallback %{
def setKeyPressCallback(self, fcn):
    f = py_keypress_callback(fcn)
    fcn_p = ctypes.cast(f, ctypes.c_void_p).value
    
    return _PyOF.WindowProxy_setKeyPressCallback(self, fcn_p)
%}

%feature("shadow") OpenFrames::WindowProxy::setMouseMotionCallback %{
def setMouseMotionCallback(self, fcn):
    f = py_mousemotion_callback(fcn)
    fcn_p = ctypes.cast(f, ctypes.c_void_p).value
    
    return _PyOF.WindowProxy_setMouseMotionCallback(self, fcn_p)
%}

%feature("shadow") OpenFrames::WindowProxy::setButtonPressCallback %{
def setButtonPressCallback(self, fcn):
    f = py_button_callback(fcn)
    fcn_p = ctypes.cast(f, ctypes.c_void_p).value
    
    return _PyOF.WindowProxy_setButtonPressCallback(self, fcn_p)
%}

%feature("shadow") OpenFrames::WindowProxy::setButtonReleaseCallback %{
def setButtonReleaseCallback(self, fcn):
    f = py_button_callback(fcn)
    fcn_p = ctypes.cast(f, ctypes.c_void_p).value
    
    return _PyOF.WindowProxy_setButtonReleaseCallback(self, fcn_p)
%}

// OpenFrames::GraphicsContextCallback class allows Python-based GUI APIs
// to be used with OpenFrames. It must be subclassed on the Python side,
// so we use the SWIG director feature for this.
%feature("director") OpenFrames::GraphicsContextCallback;

// OpenFrames::GraphicsContextCallback class virtual methods will call
// into Python through its GIL, so multithreaded support must be enabled.
%thread OpenFrames::GraphicsContextCallback::makeCurrent;
%thread OpenFrames::GraphicsContextCallback::updateContext;
%thread OpenFrames::GraphicsContextCallback::swapBuffers;

// WindowProxy::pauseAnimation waits for the animation state
%thread OpenFrames::WindowProxy::pauseAnimation;

%include "cpointer.i"
%pointer_class(double, doublep);

%inline %{
static osg::Vec3d osgVec3d(double x, double y, double z) {
    return osg::Vec3d(x, y, z);
}

static void setOsgVec3d(osg::Vec3d &vec, int i, double val) {
    vec[i] = val;
}

static double getOsgVec3d(const osg::Vec3d &vec, int i) {
    return vec[i];
}

static osg::Quat osgQuat(double x, double y, double z, double w) {
    return osg::Quat(x, y, z, w);
}

static void setOsgQuat(osg::Quat &quat, int i, double val) {
    quat[i] = val;
}

static double getOsgQuat(const osg::Quat &quat, int i) {
    return quat[i];
}
%}


%include "std_vector.i"
%template(AngleArray) std::vector<double>;


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
%import "OpenThreads/Exports"
%include "OpenThreads/Thread"
%include "OpenFrames/WindowProxy.hpp"
