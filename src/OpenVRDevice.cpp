/***********************************
 Copyright 2017 Ravishankar Mathur
 
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

#include <OpenFrames/OpenVRDevice.hpp>
#include <OpenFrames/ReferenceFrame.hpp>
#include <osg/Matrixd>
#include <osg/Notify>
#include <openvr.h>

namespace OpenFrames{
  static void convertMatrix34(osg::Matrixf &mat, const vr::HmdMatrix34_t &mat34)
  {
    mat.set(
            mat34.m[0][0], mat34.m[1][0], mat34.m[2][0], 0.0,
            mat34.m[0][1], mat34.m[1][1], mat34.m[2][1], 0.0,
            mat34.m[0][2], mat34.m[1][2], mat34.m[2][2], 0.0,
            mat34.m[0][3], mat34.m[1][3], mat34.m[2][3], 1.0
            );
  }
  
  static void convertMatrix44(osg::Matrixf &mat, const vr::HmdMatrix44_t &mat44)
  {
    mat.set(
            mat44.m[0][0], mat44.m[1][0], mat44.m[2][0], mat44.m[3][0],
            mat44.m[0][1], mat44.m[1][1], mat44.m[2][1], mat44.m[3][1],
            mat44.m[0][2], mat44.m[1][2], mat44.m[2][2], mat44.m[3][2],
            mat44.m[0][3], mat44.m[1][3], mat44.m[2][3], mat44.m[3][3]
            );
  }
  
  /*************************************************************/
  /** Get the specified OpenVR device property string */
  /*************************************************************/
  std::string GetDeviceProperty(vr::IVRSystem* vrSystem, vr::TrackedDeviceProperty prop)
  { 
    // Allocate and populate the property string
    char buffer[vr::k_unMaxPropertyStringSize];
    vr::ETrackedPropertyError propError = vr::TrackedProp_Success;
    vrSystem->GetStringTrackedDeviceProperty(vr::k_unTrackedDeviceIndex_Hmd, prop, buffer, vr::k_unMaxPropertyStringSize, &propError);
    if (propError != vr::TrackedProp_Success)
    {
      osg::notify(osg::WARN) << "OpenFrames::OpenVRDevice ERROR: Could not get device property " << vrSystem->GetPropErrorNameFromEnum(propError) << std::endl;
      return "";
    }

    // Copy property string to std::string
    std::string propval(buffer);
    return propval;
  }
  
  /*************************************************************/
  OpenVRDevice::OpenVRDevice(float worldUnitsPerMeter, float userHeightInMeters)
  : _worldUnitsPerMeter(worldUnitsPerMeter),
  _userHeightInMeters(userHeightInMeters),
  _width(0),
  _height(0),
  _isInitialized(false),
  _vrSystem(nullptr),
  _vrRenderModels(nullptr),
  _ipd(-1.0)
  {}
  
  OpenVRDevice::~OpenVRDevice()
  {
    shutdownVR();
  }
  
  /*************************************************************/
  bool OpenVRDevice::initVR()
  {
    // Check if an OpenVR HMD is available
    if(!vr::VR_IsHmdPresent())
    {
      osg::notify(osg::WARN) << "OpenFrames::OpenVRDevice ERROR: No valid HMD present." << std::endl;
      return (_isInitialized = false);
    }
    
    // Initialize OpenVR and the SteamVR runtime
    vr::EVRInitError vrError = vr::VRInitError_None;
    _vrSystem = vr::VR_Init(&vrError, vr::VRApplication_Scene);
    if(vrError != vr::VRInitError_None)
    {
      _vrSystem = nullptr;
      osg::notify(osg::WARN) << "OpenFrames::OpenVRDevice ERROR: " << vr::VR_GetVRInitErrorAsEnglishDescription(vrError) << std::endl;
      return (_isInitialized = false);
    }
    
    // Initialize OpenVR Compositor
    if(!vr::VRCompositor())
    {
      _vrSystem = nullptr;
      vr::VR_Shutdown();
      osg::notify(osg::WARN) << "OpenFrames::OpenVRDevice ERROR: Compositor could not be initialized." << std::endl;
      return (_isInitialized = false);
    }
    vr::VRCompositor()->SetTrackingSpace(vr::TrackingUniverseStanding); // Room-scale VR
    
    // Get render models for HMD components (e.g. controllers)
    _vrRenderModels = (vr::IVRRenderModels*)vr::VR_GetGenericInterface(vr::IVRRenderModels_Version, &vrError);
    if(vrError != vr::VRInitError_None)
    {
      _vrRenderModels = nullptr;
      _vrSystem = nullptr;
      vr::VR_Shutdown();
      osg::notify(osg::WARN) << "OpenFrames::OpenVRDevice ERROR: " << vr::VR_GetVRInitErrorAsEnglishDescription(vrError) << std::endl;
      return (_isInitialized = false);
    }
    
    // Print HMD driver info
    std::string driverName = GetDeviceProperty(_vrSystem, vr::Prop_TrackingSystemName_String);
    std::string deviceSerialNumber = GetDeviceProperty(_vrSystem, vr::Prop_SerialNumber_String);
    osg::notify(osg::NOTICE) << "OpenVR HMD driver name: " << driverName << std::endl;
    osg::notify(osg::NOTICE) << "OpenVR HMD device serial number: " << deviceSerialNumber << std::endl;
    
    // Get render texture size
    uint32_t w, h;
    _vrSystem->GetRecommendedRenderTargetSize(&w, &h);
    _width = w;
    _height = h;
    osg::notify(osg::NOTICE) << "OpenVR eye texture width = " << _width << ", height = " << _height << std::endl;

    // Update the per-eye projection matrices
    // The view offset matrices will be computed per-frame since IPD can change
    updateProjectionMatrices();
    
    return (_isInitialized = true);
  }
  
  /*************************************************************/
  void OpenVRDevice::shutdownVR()
  {
    if(_isInitialized)
    {
      _vrRenderModels = nullptr;
      _vrSystem = nullptr;
      vr::VR_Shutdown();
      _isInitialized = false;
    }
  }
  
  /*************************************************************/
  void OpenVRDevice::updateProjectionMatrices()
  {
    vr::HmdMatrix44_t proj;

    // Get right eye projection. Using unit depth minimizes
    // precision losses in the projection matrix
    proj = _vrSystem->GetProjectionMatrix(vr::Eye_Right, 1.0, 2.0);
    convertMatrix44(_rightProj, proj);

    // Get left eye projection. Using unit depth minimizes
    // precision losses in the projection matrix
    proj = _vrSystem->GetProjectionMatrix(vr::Eye_Left, 1.0, 2.0);
    convertMatrix44(_leftProj, proj);

    // Center projection is average of right and left
    _centerProj = (_rightProj + _leftProj)*0.5;
  }

  /*************************************************************/
  void OpenVRDevice::updateViewOffsets()
  {
    vr::HmdMatrix34_t view;
    osg::Matrixf viewMat;

    // Get right eye view
    view = _vrSystem->GetEyeToHeadTransform(vr::Eye_Right);
    convertMatrix34(viewMat, view);
    osg::Vec3f rightVec = viewMat.getTrans();

    // Get left eye view
    view = _vrSystem->GetEyeToHeadTransform(vr::Eye_Left);
    convertMatrix34(viewMat, view);
    osg::Vec3f leftVec = viewMat.getTrans();

    // If IPD has changed, then recompute offset matrices
    float ipd = (rightVec - leftVec).length();
    if (ipd != _ipd)
    {
      osg::notify(osg::ALWAYS) << "OpenVR Interpupillary Distance: " << ipd * 1000.0f << "mm" << std::endl;

      // Scale offsets according to world unit scale
      rightVec *= -_worldUnitsPerMeter; // Flip direction since we want Head to Eye transform for OSG
      leftVec *= -_worldUnitsPerMeter;
      osg::Vec3f centerVec = (rightVec + leftVec)*0.5;

      _rightViewOffset.makeTranslate(rightVec);
      _leftViewOffset.makeTranslate(leftVec);
      _centerViewOffset.makeTranslate(centerVec);
      _ipd = ipd;
    }
  }

  /*************************************************************/
  void OpenVRDevice::waitGetPoses()
  {
    // Get poses for all VR devices, and wait for the signal to start rendering
    vr::TrackedDevicePose_t poses[vr::k_unMaxTrackedDeviceCount];
    for (int i = 0; i < vr::k_unMaxTrackedDeviceCount; ++i)
      poses[i].bPoseIsValid = false;
    vr::VRCompositor()->WaitGetPoses(poses, vr::k_unMaxTrackedDeviceCount, NULL, 0);
    
    // Update view data using HMD pose
    const vr::TrackedDevicePose_t& hmdPose = poses[vr::k_unTrackedDeviceIndex_Hmd];
    if (hmdPose.bPoseIsValid)
    {
      // Extract the HMD's Head to World matrix
      osg::Matrixf hmdHeadToWorld;
      convertMatrix34(hmdHeadToWorld, hmdPose.mDeviceToAbsoluteTracking);
      
      // Apply scale, and subtract user's height from Y component
      // Note that the OpenVR world frame is Y-up
      hmdHeadToWorld(3, 0) *= _worldUnitsPerMeter;
      hmdHeadToWorld(3, 1) = (hmdHeadToWorld(3,1) - _userHeightInMeters)*_worldUnitsPerMeter;
      hmdHeadToWorld(3, 2) *= _worldUnitsPerMeter;
      _hmdPose.invert(hmdHeadToWorld);
    }
  }
  
  /*************************************************************/
  void OpenVRDevice::submitFrame(GLuint rightEyeTexName, GLuint leftEyeTexName)
  {
    // Convert from OpenGL texture to OpenVR texture
    vr::Texture_t rightEyeTexture = {(void*)rightEyeTexName, vr::TextureType_OpenGL, vr::ColorSpace_Gamma};
    vr::Texture_t leftEyeTexture = {(void*)leftEyeTexName, vr::TextureType_OpenGL, vr::ColorSpace_Gamma};
    
    // Submit eye textures to OpenVR
    vr::EVRCompositorError rightError = vr::VRCompositor()->Submit(vr::Eye_Right, &rightEyeTexture);
    vr::EVRCompositorError leftError = vr::VRCompositor()->Submit(vr::Eye_Left, &leftEyeTexture);
  }
  
} // !namespace OpenFrames
