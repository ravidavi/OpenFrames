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
#include <cmath>
#include <climits>
#include <cstring>

#ifdef _WIN32
#include <Windows.h>
#else
#include <unistd.h>
#endif

namespace OpenFrames{
  
  /** Sleep for specified number of milliseconds */
  void ThreadSleep( unsigned long nMilliseconds )
  {
#if defined(_WIN32)
    ::Sleep( nMilliseconds );
#else
    usleep( nMilliseconds * 1000 );
#endif
  }
  
  /*************************************************************/
  /** Transpose a 3x4 OpenVR matrix to an osg::Matrix */
  /*************************************************************/
  static void convertMatrix34(osg::Matrixd &mat, const vr::HmdMatrix34_t &mat34)
  {
    mat.set(
            mat34.m[0][0], mat34.m[1][0], mat34.m[2][0], 0.0,
            mat34.m[0][1], mat34.m[1][1], mat34.m[2][1], 0.0,
            mat34.m[0][2], mat34.m[1][2], mat34.m[2][2], 0.0,
            mat34.m[0][3], mat34.m[1][3], mat34.m[2][3], 1.0
            );
  }

  /*************************************************************/
  /** Transpose a 4x4 OpenVR matrix to an osg::Matrix */
  /*************************************************************/
  static void convertMatrix44(osg::Matrixd &mat, const vr::HmdMatrix44_t &mat44)
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
  std::string GetTrackedDeviceString(vr::IVRSystem* vrSystem, vr::TrackedDeviceIndex_t deviceID, vr::TrackedDeviceProperty prop)
  { 
    // Allocate and populate the property string
    char buffer[vr::k_unMaxPropertyStringSize];
    vr::ETrackedPropertyError propError = vr::TrackedProp_Success;
    vr::TrackedDeviceIndex_t ovrID = deviceID + vr::k_unTrackedDeviceIndex_Hmd;
    vrSystem->GetStringTrackedDeviceProperty(ovrID, prop, buffer, vr::k_unMaxPropertyStringSize, &propError);
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
  OpenVREvent::VREvent::VREvent()
  {
    _ovrEvent = new vr::VREvent_t;
    _controllerState = new vr::VRControllerState_t;
  }

  /*************************************************************/
  OpenVREvent::VREvent::~VREvent()
  {
    delete _ovrEvent;
    delete _controllerState;
  }

  /*************************************************************/
  void OpenVREvent::VREvent::operator=(const OpenVREvent::VREvent &other)
  {
    *_ovrEvent = *(other._ovrEvent);
    *_controllerState = *(other._controllerState);
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
      osg::notify(osg::WARN) << "OpenFrames::OpenVRDevice ERROR: OpenVR could not be initialized. OpenVR says: " << vr::VR_GetVRInitErrorAsEnglishDescription(vrError) << std::endl;
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
      osg::notify(osg::WARN) << "OpenFrames::OpenVRDevice ERROR: Generic interface error. OpenVR says: " << vr::VR_GetVRInitErrorAsEnglishDescription(vrError) << std::endl;
      return (_isInitialized = false);
    }
    
    // Print HMD driver info
    std::string driverName = GetTrackedDeviceString(_vrSystem, vr::k_unTrackedDeviceIndex_Hmd, vr::Prop_TrackingSystemName_String);
    std::string deviceSerialNumber = GetTrackedDeviceString(_vrSystem, vr::k_unTrackedDeviceIndex_Hmd, vr::Prop_SerialNumber_String);
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
    
    // Allocate render data for each possible tracked device
    // The render data struct is a light wrapper, so there is no size concern here
    _deviceIDToModel.resize(vr::k_unMaxTrackedDeviceCount);

    // Get render models for controllers and other devices
    createDeviceRenderModels();
    
    return (_isInitialized = true);
  }
  
  /*************************************************************/
  void OpenVRDevice::shutdownVR()
  {
    if(_isInitialized)
    {
      _vrRenderModels = nullptr;
      _vrSystem = nullptr;
      _deviceNameToGeode.clear();
      _deviceIDToModel.clear();
      _deviceModels->removeChildren(0, _deviceModels->getNumChildren());
      vr::VR_Shutdown();
      _isInitialized = false;
    }
  }
  
  /*************************************************************/
  void OpenVRDevice::createDeviceRenderModels()
  {
    // Loop through all tracked devices
    for(unsigned int deviceID = 0; deviceID < _deviceIDToModel.size(); ++deviceID)
    {
      // Make sure device is connected
      vr::TrackedDeviceIndex_t ovrID = deviceID + vr::k_unTrackedDeviceIndex_Hmd;
      if( !_vrSystem->IsTrackedDeviceConnected(ovrID) ) continue;
      
      // Get device name and set up its render model
      setupRenderModelForTrackedDevice(deviceID);
    }
  }
  
  /*************************************************************/
  void OpenVRDevice::setupRenderModelForTrackedDevice(uint32_t deviceID)
  {
    // Get device name by OpenVR ID
    std::string deviceName = GetTrackedDeviceString(_vrSystem, deviceID, vr::Prop_RenderModelName_String);

    // Find device model by name
    DeviceGeodeMap::iterator i = _deviceNameToGeode.find(deviceName);
    
    // If not found, then load device data
    if(i == _deviceNameToGeode.end())
    {
      osg::notify(osg::NOTICE) << "OpenFrames::OpenVRDevice: Setting up render data for device " << deviceName << std::endl;
      
      vr::EVRRenderModelError error; // Error code
      
      // Load device render data
      vr::RenderModel_t* deviceModel;
      while(true)
      {
        error = vr::VRRenderModels()->LoadRenderModel_Async(deviceName.c_str(), &deviceModel);
        if(error != vr::VRRenderModelError_Loading) break; // Only valid error
        
        ThreadSleep(10);
      }
      
      if(error != vr::VRRenderModelError_None)
      {
        osg::notify(osg::WARN) << "OpenFrames::OpenVRDevice ERROR: Unable to load render model for device " << deviceName << ". OpenVR says: " << vr::VRRenderModels()->GetRenderModelErrorNameFromEnum(error) << std::endl;
        return;
      }
      
      // Load device texture data
      vr::RenderModel_TextureMap_t* deviceTexture;
      while(true)
      {
        error = vr::VRRenderModels()->LoadTexture_Async(deviceModel->diffuseTextureId, &deviceTexture);
        if(error != vr::VRRenderModelError_Loading) break; // Only valid error
        
        ThreadSleep(10);
      }

      if(error != vr::VRRenderModelError_None)
      {
        osg::notify(osg::WARN) << "OpenFrames::OpenVRDevice ERROR: Unable to load render texture for device " << deviceName << ". OpenVR says: " << vr::VRRenderModels()->GetRenderModelErrorNameFromEnum(error) << std::endl;
        vr::VRRenderModels()->FreeRenderModel(deviceModel); // Free model data
        return;
      }
      
      uint32_t numVertices = deviceModel->unVertexCount;
      uint32_t vertexSize = sizeof(vr::RenderModel_Vertex_t);
      //osg::notify(osg::NOTICE) << numVertices << " vertices = " << numVertices*vertexSize << " bytes" << std::endl;

      // Copy vertex positions into an OSG array
      osg::Vec3Array *positions = new osg::Vec3Array(numVertices);
      for (uint32_t i = 0; i < numVertices; ++i)
      {
        std::memcpy((*positions)[i]._v, (deviceModel->rVertexData)[i].vPosition.v, sizeof(vr::HmdVector3_t));
      }
      //osg::notify(osg::NOTICE) << "position size = " << positions->size()*sizeof(vr::HmdVector3_t) << std::endl;

      // Set plain white as the color so that textures aren't artifically colored
      osg::Vec4Array *colors = new osg::Vec4Array;
      colors->push_back(osg::Vec4(1.0, 1.0, 1.0, 1.0));

      // Copy vertex normals into an OSG array
      osg::Vec3Array *normals = new osg::Vec3Array(numVertices);
      for (uint32_t i = 0; i < numVertices; ++i)
      {
        std::memcpy((*normals)[i]._v, (deviceModel->rVertexData)[i].vNormal.v, sizeof(vr::HmdVector3_t));
      }
      //osg::notify(osg::NOTICE) << "normal size = " << normals->size()*sizeof(vr::HmdVector3_t) << std::endl;

      // Copy vertex texture coordinates into an OSG array
      osg::Vec2Array *texCoords = new osg::Vec2Array(numVertices);
      for (uint32_t i = 0; i < numVertices; ++i)
      {
        std::memcpy((*texCoords)[i]._v, (deviceModel->rVertexData)[i].rfTextureCoord, 2 * sizeof(float));
      }
      //osg::notify(osg::NOTICE) << "texcoord size = " << texCoords->size()*2*sizeof(float) << std::endl;

      // Set up a geometry object to draw the device model
      osg::Geometry *deviceGeom = new osg::Geometry;
      deviceGeom->setUseDisplayList(false);
      deviceGeom->setUseVertexBufferObjects(true);
      deviceGeom->getOrCreateVertexBufferObject()->setUsage(GL_STATIC_DRAW);
      deviceGeom->setVertexArray(positions);
      deviceGeom->setColorArray(colors, osg::Array::BIND_OVERALL);
      deviceGeom->setNormalArray(normals, osg::Array::BIND_PER_VERTEX);
      deviceGeom->setTexCoordArray(0, texCoords, osg::Array::BIND_PER_VERTEX);
      uint32_t numIndices = 3 * deviceModel->unTriangleCount;
      osg::DrawElementsUShort *indices = new osg::DrawElementsUShort(osg::PrimitiveSet::TRIANGLES, numIndices, deviceModel->rIndexData);
      deviceGeom->addPrimitiveSet(indices);
      //osg::notify(osg::NOTICE) << "Number of indices = " << numIndices << std::endl;

      // Create OSG image to hold OpenVR image data
      uint16_t width = deviceTexture->unWidth;
      uint16_t height = deviceTexture->unHeight;
      //osg::notify(osg::NOTICE) << "Texture width = " << width << ", height = " << height << std::endl;
      osg::Image *image = new osg::Image;
      image->setImage(width, height, 1, GL_RGBA8, GL_RGBA, GL_UNSIGNED_BYTE, 
        (unsigned char*)(deviceTexture->rubTextureMapData), osg::Image::NO_DELETE);

      // Create OSG texture to hold image
      osg::Texture2D *tex = new osg::Texture2D;
      tex->setSourceFormat(GL_RGBA);
      tex->setInternalFormat(GL_RGBA8);
      tex->setFilter(osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR);
      tex->setFilter(osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR);
      tex->setWrap(osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE);
      tex->setWrap(osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE);
      tex->setTextureSize(width, height);
      tex->setImage(image);

      // Create OSG geode to hold the model and texture
      osg::Geode *geode = new osg::Geode();
      osg::StateSet *ss = geode->getOrCreateStateSet();
      ss->setTextureAttributeAndModes(0, tex, osg::StateAttribute::ON); // Bind texture
      geode->addDrawable(deviceGeom);

      // Assign render data to map
      _deviceNameToGeode[deviceName] = geode;

      // Free up OpenVR device model and texture memory
      vr::VRRenderModels()->FreeRenderModel(deviceModel);
      vr::VRRenderModels()->FreeTexture(deviceTexture);
    }
    
    // Set up device model if needed
    if(_deviceIDToModel[deviceID]._modelTransform->getNumChildren() == 0)
    {
      osg::notify(osg::NOTICE) << "OpenFrames::OpenVRDevice: Setting up transform for device " << deviceName << deviceID << std::endl;
      
      // Add device model's transform to the group of all rendered devices
      _deviceIDToModel[deviceID]._modelTransform->addChild(_deviceNameToGeode[deviceName]);
      _deviceModels->addChild(_deviceIDToModel[deviceID]._modelTransform);

      // Set device class
      vr::ETrackedDeviceClass devClass = _vrSystem->GetTrackedDeviceClass(deviceID);
      switch (devClass)
      {
      case vr::TrackedDeviceClass_HMD:
        _deviceIDToModel[deviceID]._class = HMD;
        break;
      case vr::TrackedDeviceClass_Controller:
        _deviceIDToModel[deviceID]._class = CONTROLLER;
        createAndAddLaserToController(deviceID); // Add pick laser to controller
        break;
      case vr::TrackedDeviceClass_TrackingReference:
        _deviceIDToModel[deviceID]._class = BASESTATION;
        break;
      default:
        _deviceIDToModel[deviceID]._class = NONE;
        break;
      }
    }
  }
  
  /*************************************************************/
  void OpenVRDevice::updateProjectionMatrices()
  {
    vr::HmdMatrix44_t proj;

    // Get right eye projection. Using unit depth minimizes
    // precision losses in the projection matrix
    proj = _vrSystem->GetProjectionMatrix(vr::Eye_Right, 1.0, 2.0);
    convertMatrix44(_rightEyeProj, proj);

    // Get left eye projection. Using unit depth minimizes
    // precision losses in the projection matrix
    proj = _vrSystem->GetProjectionMatrix(vr::Eye_Left, 1.0, 2.0);
    convertMatrix44(_leftEyeProj, proj);

    // Center projection is average of right and left
    // OSG doesn't have a matrix addition for Matrixd (facepalm)
    osg::Matrixf rightEyeProjf = _rightEyeProj;
    osg::Matrixf leftEyeProjf = _leftEyeProj;
    _centerProj = (rightEyeProjf + leftEyeProjf)*0.5;
  }

  /*************************************************************/
  void OpenVRDevice::updateViewOffsets()
  {
    vr::HmdMatrix34_t view;
    osg::Matrixd viewMat;

    // Get right eye view offset vector (Eye to Head transform)
    view = _vrSystem->GetEyeToHeadTransform(vr::Eye_Right);
    convertMatrix34(viewMat, view);
    _rightEyeViewOffsetRaw = viewMat.getTrans();

    // Get left eye view offset vector (Eye to Head transform)
    view = _vrSystem->GetEyeToHeadTransform(vr::Eye_Left);
    convertMatrix34(viewMat, view);
    _leftEyeViewOffsetRaw = viewMat.getTrans();
    
    // Compute HMD center view offset vector (Eye to Head transform)
    _centerViewOffsetRaw = (_rightEyeViewOffsetRaw + _leftEyeViewOffsetRaw)*0.5;
    _rightEyeViewOffsetRaw -= _centerViewOffsetRaw;
    _leftEyeViewOffsetRaw -= _centerViewOffsetRaw;

    // Print new IPD value if needed
    double ipd = (_rightEyeViewOffsetRaw - _leftEyeViewOffsetRaw).length();
    if (ipd != _ipd)
    {
      osg::notify(osg::ALWAYS) << "VR Interpupillary Distance: " << ipd * 1000.0f << "mm" << std::endl;
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
    
    osg::Matrixd matDeviceToWorld; // Device to World transform

    // Get poses for all devices (e.g. HMD, controllers, base stations, etc.)
    for (unsigned int i = 0; i < _deviceIDToModel.size(); ++i)
    {
      // Only extract data if pose is valid
      _deviceIDToModel[i]._valid = poses[i].bPoseIsValid;
      if (_deviceIDToModel[i]._valid)
      {
        // Extract the device's raw Local to World matrix, given in room coordinates
        vr::TrackedDeviceIndex_t ovrID = i + vr::k_unTrackedDeviceIndex_Hmd;
        convertMatrix34(matDeviceToWorld, poses[ovrID].mDeviceToAbsoluteTracking);
        matDeviceToWorld(3, 1) -= _userHeight; // Subtract user's height, OpenVR world is Y-up
        _deviceIDToModel[i]._rawDeviceToWorld = matDeviceToWorld;
        
        // Enable device model's transform so that it will be rendered
        // Only enable controllers, since we don't want to render anything else
        if(_deviceIDToModel[i]._class == CONTROLLER)
          _deviceIDToModel[i]._modelTransform->setNodeMask(0xffffffff);
        else
          _deviceIDToModel[i]._modelTransform->setNodeMask(0x0);
      }

      // Otherwise disable its model transform
      else
      {
        _deviceIDToModel[i]._modelTransform->setNodeMask(0x0);
      }
    }
    
    // Update the HMD->Eye view offset vectors
    updateViewOffsets();
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
  
  /*************************************************************/
  bool OpenVRDevice::pollNextEvent(OpenVREvent *event)
  {
    // Get one OpenVR event
    vr::VREvent_t *pEvent = event->_vrEventData._ovrEvent;
    bool hasEvent = _vrSystem->PollNextEvent(pEvent, sizeof(vr::VREvent_t));
    if (hasEvent)
    {
      switch (pEvent->eventType)
      {
      case vr::VREvent_TrackedDeviceActivated:
        // Set up the render model for the new device
        setupRenderModelForTrackedDevice(pEvent->trackedDeviceIndex);
        break;
      }

      // Get the controller state for the event
      vr::VRControllerState_t *state = event->_vrEventData._controllerState;
      _vrSystem->GetControllerState(pEvent->trackedDeviceIndex, state, sizeof(vr::VRControllerState_t));
    }
    return hasEvent;
  }

  /*************************************************************/
  void OpenVRDevice::getControllerState(uint32_t deviceID, vr::VRControllerState_t *state)
  {
    _vrSystem->GetControllerState(deviceID, state, sizeof(vr::VRControllerState_t));
  }

  /*************************************************************/
  bool OpenVREventDevice::checkEvents()
  {
    osg::ref_ptr<OpenVREvent> event = new OpenVREvent;
    bool hasEvents = false;

    // Loop while OpenVR events are available
    event->setTime(osg::Timer::instance()->delta_s(_eventQueue->getStartTick(), osg::Timer::instance()->tick()));
    while (_ovrDevice->pollNextEvent(event))
    {
      hasEvents = true;
      _eventQueue->addEvent(event);
      event = new OpenVREvent;
      event->setTime(osg::Timer::instance()->delta_s(_eventQueue->getStartTick(), osg::Timer::instance()->tick()));
    }
    return hasEvents;
  }

  /*************************************************************/
  bool OpenVRTrackball::handle(const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& us)
  {
    // Check if incoming event is an OpenVR event
    const OpenVREvent *event = dynamic_cast<const OpenVREvent*>(&ea);
    if (event)
    {
      // Get OpenVR event data
      const vr::VREvent_t *ovrEvent = event->_vrEventData._ovrEvent;
      vr::TrackedDeviceIndex_t deviceID = ovrEvent->trackedDeviceIndex;
      const vr::VRControllerState_t *state = event->_vrEventData._controllerState;
      
      // Only process event if it's from a controller
      if (deviceID >= _ovrDevice->getNumDeviceModels()) return false;
      if (_ovrDevice->getDeviceModel(deviceID)->_class != OpenVRDevice::CONTROLLER) return false;
      
      // Convert controller event types to view changes in VR space
      switch (ovrEvent->eventType)
      {
      case(vr::VREvent_ButtonPress) :
      {
        // Grip button pressed state transitions: No Motion -> Translate/Rotate -> Scale
        if (state->ulButtonPressed & vr::ButtonMaskFromId(vr::k_EButton_Grip))
        {
          // Go from No Motion -> Translate/Rotate when a controller's grip button is pressed
          if (_motionData._mode == NONE)
          {
            // Translate/Rotate uses Device 1 to control the view
            _motionData._mode = _motionData._prevMode;
            _motionData._device1ID = deviceID;
            _motionData._device2ID = UINT_MAX; // Ignore device 2
            saveCurrentMotionData();
          }

          // Go from Translate/Rotate -> Scale when the "other" controller's grip button is pressed
          else if (((_motionData._mode == TRANSLATE) || (_motionData._mode == ROTATE)) && (_motionData._device1ID != deviceID))
          {
            // Scale uses Device 1 & 2 to change the WorldUnits/Meter ratio
            _motionData._prevMode = _motionData._mode; // Save current mode
            _motionData._prevTime = event->getTime(); // Save event time
            _motionData._mode = SCALE;
            _motionData._device2ID = deviceID;
            saveCurrentMotionData();
          }
        }

        // If trigger is pressed, then pick a point on the ground grid
        else if (state->ulButtonPressed & vr::ButtonMaskFromId(vr::k_EButton_SteamVR_Trigger))
        {
          // Go from No Motion -> Pick when a controller's touchpad is pressed
          if (_motionData._mode == NONE)
          {
            // Pick using Device 1
            //_motionData._mode = PICK;
            _motionData._device1ID = deviceID;
            _motionData._device2ID = UINT_MAX; // Ignore device 2
            saveCurrentMotionData();
          }
        }

        break;
      }

      case(vr::VREvent_ButtonUnpress) :
      {
        // Button unpressed state transitions: Scale -> Translate/Rotate -> No Motion
        if (state->ulButtonPressed == 0)
        {
          // Go from Translate/Rotate -> No Motion when controller's grip is unpressed
          if (((_motionData._mode == TRANSLATE) || (_motionData._mode == ROTATE)) && (_motionData._device1ID == deviceID))
          {
            // Save current mode for when button is pressed again
            _motionData._prevMode = _motionData._mode;
            _motionData._mode = NONE;
          }

          // Go from Scale -> Translate/Rotate when either controller's grip is unpressed
          else if ((_motionData._mode == SCALE) &&
            ((_motionData._device1ID == deviceID) || (_motionData._device2ID == deviceID)))
          {
            // If the second grip button was just tapped, then switch translate/rotate modes
            // Otherwise it was actually pressed so keep the previous translate/rotate mode
            // Alternatively, if the WorldUnits/Meter scale was actually changed, then keep
            // the previous translate/rotate mode even if the grip button was just tapped.
            const double tapDuration = 0.25;
            const double maxScaleChange = 0.01;
            double pressDuration = event->getTime() - _motionData._prevTime;
            double scaleChange = std::abs(_motionData._origWorldUnitsPerMeter / _ovrDevice->getWorldUnitsPerMeter() - 1.0);
            if ((pressDuration >= tapDuration) || (scaleChange > maxScaleChange))
            {
              _motionData._mode = _motionData._prevMode;
            }
            else if (_motionData._prevMode == TRANSLATE) _motionData._mode = ROTATE;
            else if (_motionData._prevMode == ROTATE) _motionData._mode = TRANSLATE;
            else
            {
              osg::notify(osg::WARN) << "OpenFrames::OpenVRTrackball WARNING: previous mode invalid. Defaulting to TRANSLATE." << std::endl;
              _motionData._mode = TRANSLATE;
            }

            // Translate/Rotate uses Device 1 to control the view, so indicate that the
            // controller with grip button still pressed should be used for view changes
            _motionData._device1ID = _motionData._device1ID + _motionData._device2ID - deviceID;
            _motionData._device2ID = UINT_MAX; // Ignore device 2
            saveCurrentMotionData();
          }
        }
        break;
      }

      case(vr::VREvent_ButtonTouch) :
      {
        // If trigger is touched, then show the controller's laser
        if (state->ulButtonTouched & vr::ButtonMaskFromId(vr::k_EButton_SteamVR_Trigger))
        {
          osg::MatrixTransform* laserXform = _ovrDevice->getControllerLaser(deviceID);
          if (laserXform) laserXform->setNodeMask(0xffffffff);
        }
        break;
      }

      case(vr::VREvent_ButtonUntouch) :
      {
        // If trigger is untouched, then hide the controller's laser
        if ((state->ulButtonTouched & vr::ButtonMaskFromId(vr::k_EButton_SteamVR_Trigger)) == 0x0)
        {
          osg::MatrixTransform* laserXform = _ovrDevice->getControllerLaser(deviceID);
          if (laserXform) laserXform->setNodeMask(0x0);
        }
        break;
      }
      }

      /*
      osg::notify(osg::NOTICE) << "Switching to ";
      if (_motionData._mode == NONE) osg::notify(osg::NOTICE) << "NONE" << std::endl;
      else if (_motionData._mode == TRANSLATE) osg::notify(osg::NOTICE) << "TRANSLATE" << std::endl;
      else if (_motionData._mode == ROTATE) osg::notify(osg::NOTICE) << "ROTATE" << std::endl;
      else if (_motionData._mode == SCALE) osg::notify(osg::NOTICE) << "SCALE" << std::endl;
      */

      return false;
    }
    else // Otherwise process it as a regular trackball event
    {
      // Compute new motion-based view at each frame
      if (ea.getEventType() == osgGA::GUIEventAdapter::FRAME)
        processMotion();

      return FollowingTrackball::handle(ea, us);
    }
  }

} // !namespace OpenFrames
