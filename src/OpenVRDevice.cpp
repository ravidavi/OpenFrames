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
#include <osg/MatrixTransform>
#include <openvr.h>

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
  static void convertMatrix34(osg::Matrixf &mat, const vr::HmdMatrix34_t &mat34)
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
  std::string GetTrackedDeviceString(vr::IVRSystem* vrSystem, vr::TrackedDeviceIndex_t device, vr::TrackedDeviceProperty prop)
  { 
    // Allocate and populate the property string
    char buffer[vr::k_unMaxPropertyStringSize];
    vr::ETrackedPropertyError propError = vr::TrackedProp_Success;
    vrSystem->GetStringTrackedDeviceProperty(device, prop, buffer, vr::k_unMaxPropertyStringSize, &propError);
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
    _deviceIDToModel.resize(vr::k_unMaxTrackedDeviceCount);
    
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

    // We will scale device models according to the provided WorldUnit/Meter ratio, so
    // make sure that model normals are rescaled by OpenGL
    //_deviceModels->getOrCreateStateSet()->setMode(GL_RESCALE_NORMAL, osg::StateAttribute::ON);
    _deviceModels->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
  }
  
  /*************************************************************/
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
    
    // Get render models for controllers and other devices
    updateDeviceRenderModels();
    
    return (_isInitialized = true);
  }
  
  /*************************************************************/
  void OpenVRDevice::shutdownVR()
  {
    if(_isInitialized)
    {
      _vrRenderModels = nullptr;
      _vrSystem = nullptr;
      _deviceNameToData.clear();
      _deviceIDToModel.clear();
      _deviceModels->removeChildren(0, _deviceModels->getNumChildren());
      vr::VR_Shutdown();
      _isInitialized = false;
    }
  }
  
  /*************************************************************/
  void OpenVRDevice::updateDeviceRenderModels()
  {    
    // Loop through all possible tracked devices except the HMD (already loaded)
    for(vr::TrackedDeviceIndex_t deviceID = vr::k_unTrackedDeviceIndex_Hmd + 1; 
      deviceID < vr::k_unMaxTrackedDeviceCount; ++deviceID)
    {
      // Make sure device is connected
      if( !_vrSystem->IsTrackedDeviceConnected(deviceID) ) continue;
      
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
    DeviceDataMap::iterator i = _deviceNameToData.find(deviceName);
    
    // If not found, then load device data
    if(i == _deviceNameToData.end())
    {
      osg::notify(osg::NOTICE) << "OpenFrames::OpenVRDevice: Setting up render data for device " << deviceName << std::endl;
      
      vr::EVRRenderModelError error; // Error code
      DeviceData newDevice; // The new device to be set up
      
      // Load device render data
      while(true)
      {
        error = vr::VRRenderModels()->LoadRenderModel_Async(deviceName.c_str(), &(newDevice._deviceModel));
        if(error != vr::VRRenderModelError_Loading) break; // Only valid error
        
        ThreadSleep(10);
      }
      
      if(error != vr::VRRenderModelError_None)
      {
        osg::notify(osg::WARN) << "OpenFrames::OpenVRDevice ERROR: Unable to load render model for device " << deviceName << ". OpenVR says: " << vr::VRRenderModels()->GetRenderModelErrorNameFromEnum(error) << std::endl;
        return;
      }
      
      // Load device texture data
      while(true)
      {
        error = vr::VRRenderModels()->LoadTexture_Async(newDevice._deviceModel->diffuseTextureId, &(newDevice._deviceTexture));
        if(error != vr::VRRenderModelError_Loading) break; // Only valid error
        
        ThreadSleep(10);
      }

      if(error != vr::VRRenderModelError_None)
      {
        osg::notify(osg::WARN) << "OpenFrames::OpenVRDevice ERROR: Unable to load render texture for device " << deviceName << ". OpenVR says: " << vr::VRRenderModels()->GetRenderModelErrorNameFromEnum(error) << std::endl;
        vr::VRRenderModels()->FreeRenderModel(newDevice._deviceModel); // Free model data
        return;
      }
      
      uint32_t numVertices = newDevice._deviceModel->unVertexCount;
      uint32_t vertexSize = sizeof(vr::RenderModel_Vertex_t);
      osg::notify(osg::NOTICE) << numVertices << " vertices = " << numVertices*vertexSize << " bytes" << std::endl;

      // Copy vertex positions into an OSG array
      osg::Vec3Array *positions = new osg::Vec3Array(numVertices);
      for (uint32_t i = 0; i < numVertices; ++i)
      {
        std::memcpy((*positions)[i]._v, (newDevice._deviceModel->rVertexData)[i].vPosition.v, sizeof(vr::HmdVector3_t));
      }
      osg::notify(osg::NOTICE) << "position size = " << positions->size()*sizeof(vr::HmdVector3_t) << std::endl;

      // Copy vertex normals into an OSG array
      osg::Vec3Array *normals = new osg::Vec3Array(numVertices);
      for (uint32_t i = 0; i < numVertices; ++i)
      {
        std::memcpy((*normals)[i]._v, (newDevice._deviceModel->rVertexData)[i].vNormal.v, sizeof(vr::HmdVector3_t));
      }
      osg::notify(osg::NOTICE) << "normal size = " << normals->size()*sizeof(vr::HmdVector3_t) << std::endl;

      // Copy vertex texture coordinates into an OSG array
      osg::Vec2Array *texCoords = new osg::Vec2Array(numVertices);
      for (uint32_t i = 0; i < numVertices; ++i)
      {
        std::memcpy((*texCoords)[i]._v, (newDevice._deviceModel->rVertexData)[i].rfTextureCoord, 2 * sizeof(float));
      }
      osg::notify(osg::NOTICE) << "texcoord size = " << texCoords->size()*2*sizeof(float) << std::endl;

      // Set up a geometry object to draw the device model
      osg::Geometry *deviceGeom = new osg::Geometry;
      deviceGeom->setUseDisplayList(false);
      deviceGeom->setUseVertexBufferObjects(true);
      deviceGeom->getOrCreateVertexBufferObject()->setUsage(GL_STATIC_DRAW);
      deviceGeom->setVertexArray(positions);
      deviceGeom->setNormalArray(normals, osg::Array::BIND_PER_VERTEX);
      deviceGeom->setTexCoordArray(0, texCoords, osg::Array::BIND_PER_VERTEX);
      uint32_t numIndices = 3 * newDevice._deviceModel->unTriangleCount;
      osg::DrawElementsUShort *indices = new osg::DrawElementsUShort(osg::PrimitiveSet::TRIANGLES, numIndices, newDevice._deviceModel->rIndexData);
      deviceGeom->addPrimitiveSet(indices);
      osg::notify(osg::NOTICE) << "Number of indices = " << numIndices << std::endl;

      // Create OSG image to hold OpenVR image data
      uint16_t width = newDevice._deviceTexture->unWidth;
      uint16_t height = newDevice._deviceTexture->unHeight;
      osg::notify(osg::NOTICE) << "Texture width = " << width << ", height = " << height << std::endl;
      osg::Image *image = new osg::Image;
      image->setImage(width, height, 1, GL_RGBA8, GL_RGBA, GL_UNSIGNED_BYTE, 
        (unsigned char*)(newDevice._deviceTexture->rubTextureMapData), osg::Image::NO_DELETE);

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
      newDevice._osgTexture = tex;

      // Create OSG model
      newDevice._osgModel = new osg::Geode();
      newDevice._osgModel->addDrawable(deviceGeom);

      // Assign render data to map
      _deviceNameToData[deviceName] = newDevice;
    }
    
    // Set up device model if needed
    if(_deviceIDToModel[deviceID]._data == NULL)
    {
      osg::notify(osg::NOTICE) << "OpenFrames::OpenVRDevice: Setting up render model for device " << deviceName << deviceID << std::endl;
      
      // Set data for current device model
      _deviceIDToModel[deviceID]._data = &_deviceNameToData[deviceName];
      
      float radius = 0.05f;
      float height = 0.1f;
      
      // Create device model's render model and add it to the render group
      //osg::Geode *geode = new osg::Geode;
      //geode->addDrawable(new osg::ShapeDrawable(new osg::Capsule(osg::Vec3(), radius, height)));
      osg::MatrixTransform *xform = new osg::MatrixTransform;
      osg::StateSet *ss = xform->getOrCreateStateSet();
      ss->setTextureAttributeAndModes(0, _deviceNameToData[deviceName]._osgTexture, osg::StateAttribute::ON); // Bind texture
      //xform->addChild(geode);
      xform->addChild(_deviceNameToData[deviceName]._osgModel);
      _deviceIDToModel[deviceID]._renderModel = xform;
      _deviceModels->addChild(xform);
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
      osg::notify(osg::NOTICE) << "OpenVR Interpupillary Distance: " << ipd * 1000.0f << "mm" << std::endl;

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
    
    osg::Matrixf matDeviceToWorld; // Device to World transform

    // Update view data using HMD pose
    const vr::TrackedDevicePose_t& hmdPose = poses[vr::k_unTrackedDeviceIndex_Hmd];
    if (hmdPose.bPoseIsValid)
    {
      // Extract the HMD's Head to World matrix
      convertMatrix34(matDeviceToWorld, hmdPose.mDeviceToAbsoluteTracking);
      
      // The OpenVR HMD units are in meters, so we need to convert its position
      // to world units. Here we also subtract the user's height from Y component.
      // Note that the OpenVR world frame is Y-up
      matDeviceToWorld(3, 0) *= _worldUnitsPerMeter;
      matDeviceToWorld(3, 1) = (matDeviceToWorld(3, 1) - _userHeight)*_worldUnitsPerMeter;
      matDeviceToWorld(3, 2) *= _worldUnitsPerMeter;

      // Invert since we want World to HMD transform
      _hmdPose.invert(matDeviceToWorld);
    }

    // Get poses for remaining devices (e.g. controllers, base stations, etc.)
    for (int i = vr::k_unTrackedDeviceIndex_Hmd + 1; i < vr::k_unMaxTrackedDeviceCount; ++i)
    {
      // Only process device if it has been set up
      if(_deviceIDToModel[i]._renderModel.valid())
      {
        _deviceIDToModel[i]._valid = poses[i].bPoseIsValid;
      }
      else
      {
        _deviceIDToModel[i]._valid = false;
        continue;
      }

      // Only extract data if pose is valid
      if (_deviceIDToModel[i]._valid)
      {
        // Extract the device's Local to World matrix
        convertMatrix34(matDeviceToWorld, poses[i].mDeviceToAbsoluteTracking);

        // Apply world unit translation, and subtract user's height from Y component
        // Note that the OpenVR world frame is Y-up
        matDeviceToWorld(3, 0) *= _worldUnitsPerMeter;
        matDeviceToWorld(3, 1) = (matDeviceToWorld(3, 1) - _userHeight)*_worldUnitsPerMeter;
        matDeviceToWorld(3, 2) *= _worldUnitsPerMeter;

        // Since the device model is assumed in meters, we need to scale it to world units
        // The normals will need to be rescaled, which is done by the containing Camera
        matDeviceToWorld.preMultScale(osg::Vec3d(_worldUnitsPerMeter, _worldUnitsPerMeter, _worldUnitsPerMeter));

        // Set device model's transform from its adjusted pose in world units
        osg::MatrixTransform *xform = static_cast<osg::MatrixTransform*>(_deviceIDToModel[i]._renderModel.get());
        xform->setMatrix(matDeviceToWorld);
        
        // Enable device model
        _deviceIDToModel[i]._renderModel->setNodeMask(0xffffffff);
      }

      // Otherwise disable its model
      else
      {
        _deviceIDToModel[i]._renderModel->setNodeMask(0x0);
      }
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
