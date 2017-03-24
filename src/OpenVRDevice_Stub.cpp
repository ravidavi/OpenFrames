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
#include <osg/PositionAttitudeTransform>

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
    
    // Set up a camera for the device render models
    // These models exist in local space (the room), so their view matrix should have
    // the World->Local transform removed. This is done by premultiplying by the inverse
    // of the World->Local transform. OpenVRTrackball automatically sets this inverse
    // as the view matrix for the render model Camera, so we just need to specify the
    // pre-multiply transform order here.
    _deviceModels = new osg::Camera();
    _deviceModels->setTransformOrder(osg::Camera::PRE_MULTIPLY);
    
    // Make sure to render device models in the same context/viewport as parent camera
    _deviceModels->setRenderOrder(osg::Camera::NESTED_RENDER);
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
    _deviceNameToData.clear();
    _deviceIDToModel.clear();
    _deviceModels->removeChildren(0, _deviceModels->getNumChildren());
    _isInitialized = false;
  }
  
  /*************************************************************/
  void OpenVRDevice::updateDeviceRenderModels()
  {
    // Loop through all possible tracked devices except the HMD (already loaded)
    for(unsigned int deviceID = 1; deviceID < numTrackedDevices; ++deviceID)
    {
      // Set up device render model
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
    
    // Find device data by name
    DeviceDataMap::iterator i = _deviceNameToData.find(deviceName);
    
    // If not found, then create device data
    if(i == _deviceNameToData.end())
    {
      osg::notify(osg::NOTICE) << "OpenFrames::OpenVRDeviceStub: Setting up render data for device " << deviceName << std::endl;

      DeviceData newDevice; // Empty device data since this is a stub
      _deviceNameToData[deviceName] = newDevice;
    }
    
    // Set up device model if needed
    if(_deviceIDToModel[deviceID]._data == NULL)
    {
      osg::notify(osg::NOTICE) << "OpenFrames::OpenVRDeviceStub: Setting up render model for device " << deviceName << deviceID << std::endl;
      
      // Set data for current device model
      _deviceIDToModel[deviceID]._data = &_deviceNameToData[deviceName];
      
      float radius = 0.1f;
      float height = 0.2f;
      
      // Create device model's render model and add it to the render group
      osg::Geode *geode = new osg::Geode;
      geode->addDrawable(new osg::ShapeDrawable(new osg::Capsule(osg::Vec3(),radius, height)));
      osg::PositionAttitudeTransform *xform = new osg::PositionAttitudeTransform;
      xform->addChild(geode);
      _deviceIDToModel[deviceID]._renderModel = xform;
      _deviceModels->addChild(xform);
    }
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
      
      osg::Matrixf matDeviceToWorld; // Device to World transform
      
      // Set simulated base station pose
      if(i == 1)
      {
        matDeviceToWorld.makeRotate(osg::Quat(10.0, osg::Vec3d(1, 0, 0)));
        matDeviceToWorld.postMultTranslate(osg::Vec3d(-3, 0, -3));
      }
      else if(i == 2)
      {
        matDeviceToWorld.makeRotate(osg::Quat(10.0, osg::Vec3d(0, 1, 0)));
        matDeviceToWorld.postMultTranslate(osg::Vec3d(2, 0, -2));
      }
      
      // Set base station model's location from its pose
      osg::PositionAttitudeTransform *xform = static_cast<osg::PositionAttitudeTransform*>(_deviceIDToModel[i]._renderModel.get());
      osg::Vec3d pos = matDeviceToWorld.getTrans();
      osg::Quat att = matDeviceToWorld.getRotate();
      xform->setPosition(pos);
      xform->setAttitude(att);
    }
  }
  
  /*************************************************************/
  void OpenVRDevice::submitFrame(GLuint rightEyeTexName, GLuint leftEyeTexName)
  {
    // Nothing to do here
  }
  
} // !namespace OpenFrames
