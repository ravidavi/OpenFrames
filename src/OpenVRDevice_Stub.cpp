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

// Assume 3 stub tracked devices here: HMD + 2 base stations
const unsigned int numTrackedDevices = 3;

namespace OpenFrames{

  /*************************************************************/
  OpenVRDevice::OpenVRDevice(float worldUnitsPerMeter, float userHeight)
  : _worldUnitsPerMeter(worldUnitsPerMeter),
  _userHeight(userHeight),
  _width(0),
  _height(0),
  _isInitialized(false),
  _vrSystem(nullptr),
  _vrRenderModels(nullptr),
  _ipd(-1.0)
  {
    // Allocate render data for each possible tracked device
    // The render data struct is a light wrapper, so there is no size concern here
    _deviceIDToModel.resize(numTrackedDevices);
  }
  
  OpenVRDevice::~OpenVRDevice()
  {
    shutdownVR();
  }
  
  /*************************************************************/
  bool OpenVRDevice::initVR()
  {
    osg::notify(osg::NOTICE) << "Using OpenVR stub" << std::endl;
    
    // Set texture size similar to what OpenVR would return
    _width = 1512;  // 1.4*1080
    _height = 1680; // 1.4*1200
    osg::notify(osg::NOTICE) << "VR eye texture width = " << _width << ", height = " << _height << std::endl;

    // Update the per-eye projection matrices
    // The view offset matrices will be computed per-frame since IPD can change
    updateProjectionMatrices();
    
    // Get render models for controllers and other devices
    updateDeviceRenderModels();
    
    return (_isInitialized = true);
  }
  
  /*************************************************************/
  void OpenVRDevice::shutdownVR()
  {
    _deviceNameToModel.clear();
    _deviceIDToModel.clear();
    _isInitialized = false;
  }
  
  /*************************************************************/
  void OpenVRDevice::updateDeviceRenderModels()
  {
    // Loop through all possible tracked devices except the HMD (already loaded)
    for(unsigned int deviceID = 1; deviceID < numTrackedDevices; ++deviceID)
    {
      // Get device name and set up its render model
      setupRenderModelForTrackedDevice(deviceID);
    }
  }
  
  /*************************************************************/
  void OpenVRDevice::setupRenderModelForTrackedDevice(uint32_t deviceID)
  {
    // Get name of tracked device
    std::string deviceName;
    if(deviceID == 0) deviceName = "HMD_Stub";
    else deviceName = "BaseStation_Stub";
    
    // Find device model by name
    DeviceModelMap::iterator i = _deviceNameToModel.find(deviceName);
    
    // If found, then just make sure it is properly referenced by its ID
    if(i != _deviceNameToModel.end())
    {
      _deviceIDToModel[deviceID] = i->second;
      osg::notify(osg::NOTICE) << "OpenFrames::OpenVRDeviceStub: Render model already set up for device " << deviceName << std::endl;
      return;
    }
    
    DeviceModel newDevice; // The new device to be set up
    
    osg::notify(osg::NOTICE) << "OpenFrames::OpenVRDeviceStub: Setting up render model for device " << deviceName << std::endl;
    
    // Model is set up now, so initialize its ReferenceFrame
    newDevice._refFrame = new ReferenceFrame(deviceName);
    _deviceNameToModel[deviceName] = newDevice;
    _deviceIDToModel[deviceID] = newDevice;
  }
  
  /*************************************************************/
  void OpenVRDevice::updateProjectionMatrices()
  {
    // Create right/left/center projection matrices. Using unit depth minimizes
    // precision losses in the projection matrix
    _rightProj.makePerspective(110.0, (float)_width/(float)_height, 1.0, 2.0);
    _leftProj.makePerspective(110.0, (float)_width/(float)_height, 1.0, 2.0);

    // Center projection is average of right and left
    _centerProj = (_rightProj + _leftProj)*0.5;
  }

  /*************************************************************/
  void OpenVRDevice::updateViewOffsets()
  {
    // Get right eye view
    osg::Vec3f rightVec;

    // Get left eye view
    osg::Vec3f leftVec;

    // If IPD has changed, then recompute offset matrices
    float ipd = (rightVec - leftVec).length();
    if (ipd != _ipd)
    {
      osg::notify(osg::ALWAYS) << "VR Interpupillary Distance: " << ipd * 1000.0f << "mm" << std::endl;

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
    // Stub HMD has no transformation
    _hmdPose.makeIdentity();
    
    // Simulate pose for base stations
    for(unsigned int i = 1; i < numTrackedDevices; ++i)
    {
      _deviceIDToModel[i]._valid = true; // Always valid
      
      // This should be set to simulated base station pose
      _deviceIDToModel[i]._pose.makeIdentity();
      
      // Set base station model's location from its pose
      osg::Quat att = _deviceIDToModel[i]._pose.getRotate();
      osg::Vec3d pos = _deviceIDToModel[i]._pose.getTrans();
      _deviceIDToModel[i]._refFrame->setPosition(pos[0], pos[1], pos[2]);
      _deviceIDToModel[i]._refFrame->setAttitude(att[0], att[1], att[2], att[3]);
    }
  }
  
  /*************************************************************/
  void OpenVRDevice::submitFrame(GLuint rightEyeTexName, GLuint leftEyeTexName)
  {
    // Nothing to do here
  }
  
} // !namespace OpenFrames
