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
#include <osg/Notify>
#include <openvr.h>

namespace OpenFrames{
  
  /*************************************************************/
  /** Get the specified OpenVR device property string */
  /*************************************************************/
  std::string GetDeviceProperty(vr::IVRSystem* vrSystem, vr::TrackedDeviceProperty prop)
  {
    // Get the length of the property string
    vr::ETrackedPropertyError propError = vr::TrackedProp_Success;
    uint32_t bufferLen = vrSystem->GetStringTrackedDeviceProperty(vr::k_unTrackedDeviceIndex_Hmd, prop, NULL, 0, &propError);
    if(propError != vr::TrackedProp_Success)
    {
      osg::notify(osg::WARN) << "OpenFrames::OpenVRDevice ERROR: Could not get device property " << vrSystem->GetPropErrorNameFromEnum(propError) << std::endl;
      return "";
    }
    
    // Allocate and populate the property string
    char buffer[bufferLen];
    bufferLen = vrSystem->GetStringTrackedDeviceProperty(vr::k_unTrackedDeviceIndex_Hmd, prop, buffer, bufferLen);
    
    // Copy property string to std::string
    std::string propval = buffer;
    return propval;
  }
  
  /*************************************************************/
  OpenVRDevice::OpenVRDevice(double worldUnitsPerMeter)
  : _worldUnitsPerMeter(worldUnitsPerMeter),
  _width(0),
  _height(0),
  _isInitialized(false),
  _vrSystem(nullptr),
  _vrRenderModels(nullptr)
  {}
  
  OpenVRDevice::~OpenVRDevice() {}
  
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
    
    std::string driverName = GetDeviceProperty(_vrSystem, vr::Prop_TrackingSystemName_String);
    std::string deviceSerialNumber = GetDeviceProperty(_vrSystem, vr::Prop_SerialNumber_String);
    
    osg::notify(osg::NOTICE) << "OpenVR HMD driver name: "<< driverName << std::endl;
    osg::notify(osg::NOTICE) << "OpenVR HMD device serial number: " << deviceSerialNumber << std::endl;
    
    return (_isInitialized = true);
  }
  
  
  
} // !namespace OpenFrames
