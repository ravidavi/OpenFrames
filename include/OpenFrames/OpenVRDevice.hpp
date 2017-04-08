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

#ifndef _OF_OPENVRDEVICE_
#define _OF_OPENVRDEVICE_

#include <OpenFrames/Export.h>
#include <OpenFrames/View.hpp>
#include <osg/ref_ptr>
#include <osg/Referenced>
#include <osg/Quat>
#include <osg/MatrixTransform>
#include <osg/Texture>
#include <osg/Vec3d>
#include <osgGA/Device>

/** 
Classes declared in OpenVR header. We don't include the OpenVR header here because
the user might want to use the Stub version of the OpenVR device.
*/
namespace vr {
  class IVRSystem;
  class IVRRenderModels;
  struct VREvent_t;
  struct VRControllerState001_t;
  typedef VRControllerState001_t VRControllerState_t;
}

namespace OpenFrames {
  struct VRTextureBuffer; // Used by OpenVRSwapBuffers below
  
  /******************************************
  * OpenFrames API, class OpenVREvent
  * Wrap OpenVR events in an OSG-compatible event adapter
  ******************************************/
  class OpenVREvent : public osgGA::GUIEventAdapter
  {
  public:
    OpenVREvent()
    {
      setEventType(osgGA::GUIEventAdapter::USER);
    }

    struct VREvent
    {
      VREvent();
      ~VREvent();

      void operator=(const VREvent &other);

      vr::VREvent_t *_ovrEvent;
      vr::VRControllerState_t *_controllerState;
    } _vrEventData;
  };

  /******************************************
   * Ravi Mathur
   * OpenFrames API, class OpenVRDevice
   * Represents data needed to use an OpenVR-supported HMD.
   ******************************************/
  class OF_EXPORT OpenVRDevice : public osg::Referenced
  {
    // The trackball needs access to the device render model camera
    friend class OpenVRTrackball;
    
  public:
    /** Create a new OpenVR device, specifying relationship between world units and
     real-world meters, and the user height in meters. */
    OpenVRDevice(float worldUnitsPerMeter, float userHeight);

    /**
     Initialize OpenVR and connect to the HMD
     Return status: whether OpenVR was initialized
     */
    bool initVR();
    void shutdownVR();

    /** Get the per-eye texture size recommended by OpenVR */
    void getRecommendedTextureSize(int& w, int& h)
    {
      w = _width; h = _height;
    }

    /** Get whether OpenVR has been initialized */
    inline bool isInitialized() { return _isInitialized; }
    
    /** Get render models for devices */
    void updateDeviceRenderModels();
    osg::Camera* getDeviceRenderModels() { return _deviceModels; }

    /** Update and get the per-eye projection matrix */
    void updateProjectionMatrices();
    osg::Matrixd& getRightEyeProjectionMatrix() { return _rightProj; }
    osg::Matrixd& getLeftEyeProjectionMatrix() { return _leftProj; }
    osg::Matrixd& getCenterProjectionMatrix() { return _centerProj; }

    /** Update and get the per-eye view matrix */
    void updateViewOffsets();
    osg::Matrixd& getRightEyeViewOffsetMatrix() { return _rightViewOffset; }
    osg::Matrixd& getLeftEyeViewOffsetMatrix() { return _leftViewOffset; }
    osg::Matrixd& getCenterViewOffsetMatrix() { return _centerViewOffset; }

    /** Update poses (positions/orientations) of all VR devices, and wait
     for the signal to start rendering. Note that this should be called
     just before the start of the rendering pass. */
    void waitGetPoses();
    osg::Matrixd& getHMDPoseMatrix() { return _hmdPose; }
    
    /** Get/set the world units in meters */
    void setWorldUnitsPerMeter(float worldUnitsPerMeter) { _worldUnitsPerMeter = worldUnitsPerMeter; }
    float getWorldUnitsPerMeter() { return _worldUnitsPerMeter; }
    
    /** Get/set the user height in meters */
    void setUserHeight(double userHeight) { _userHeight = userHeight; }
    double getUserHeight() { return _userHeight; }

    /** Submits the latest rendered eye textures to OpenVR */
    void submitFrame(GLuint rightEyeTexName, GLuint leftEyeTexName);

    /** Get the next OpenVR event */
    bool pollNextEvent(OpenVREvent *event);
    
  protected:
    virtual ~OpenVRDevice();
    
    /** Load a device's render model by its OpenVR ID */
    void setupRenderModelForTrackedDevice(uint32_t deviceID);
    
    double _worldUnitsPerMeter; // Distance units per real-world meter
    double _userHeight; // Height of user's HMD origin in meters
    int _width, _height; // Per-eye texture dimensions
    
    bool _isInitialized; // Whether OpenVR is initialized
    
    vr::IVRSystem* _vrSystem; // OpenVR interface
    vr::IVRRenderModels* _vrRenderModels; // Controller models

    /** Map between a device's OpenVR name and its OSG render model */
    typedef std::map<std::string, osg::ref_ptr<osg::Geode> > DeviceModelMap;
    DeviceModelMap _deviceNameToModel;
    
    enum DeviceClass
    {
      NONE = 0,
      HMD = 1,
      BASESTATION = 2,
      CONTROLLER = 3
    };

    /** Encapsulates an OpenVR device's model */
    struct DeviceModel
    {
      DeviceModel() : _valid(false), _class(NONE) {}
      osg::ref_ptr<osg::MatrixTransform> _modelTransform;
      osg::Matrixd _rawDeviceToWorld;
      bool _valid;
      DeviceClass _class;
    };

    /** Vector of each device's rendering data indexed by its OpenVR id */
    typedef std::vector<DeviceModel> DeviceModelVector;
    DeviceModelVector _deviceIDToModel;
    
    // Group that contains all device models
    osg::ref_ptr<osg::Camera> _deviceModels;
    
    // Per-eye asymmetric projection matrices
    osg::Matrixd _rightProj, _leftProj, _centerProj;
    
    // Per-eye view matrices, transform Head to Eye space
    osg::Matrixd _rightViewOffset, _leftViewOffset, _centerViewOffset;
    double _ipd; // Interpupillary distance

    // World to Head view transformation
    osg::Matrixd _hmdPose;

    // Translational offset for each device pose (in VR room-space coordinates)
    osg::Vec3d _poseOffsetRaw;
  };
  
  /******************************************
  * OpenFrames API, class OpenVREventDevice
  * Polls for OpenVR events and stores them in its OSG event queue
  ******************************************/
  class OpenVREventDevice : public osgGA::Device
  {
  public:
    OpenVREventDevice(OpenVRDevice *ovrDevice)
      : _ovrDevice(ovrDevice)
    {
      setCapabilities(osgGA::Device::RECEIVE_EVENTS);
    }

    // Check for events and store them in event queue
    virtual bool checkEvents();

  private:
    osg::observer_ptr<OpenVRDevice> _ovrDevice;
  };

  /******************************************
   * OpenFrames API, class OpenVRPoseCallback
   * Updates HMD and pose data from OpenVR. This should be attached as an
   * update callback to the view's master camera
   ******************************************/
  class OpenVRPoseCallback : public osg::Callback
  {
  public:
    OpenVRPoseCallback(OpenVRDevice *ovrDevice)
    : _ovrDevice(ovrDevice)
    { }
    
    virtual bool run(osg::Object* object, osg::Object* data);
    
  private:
    osg::observer_ptr<OpenVRDevice> _ovrDevice;
  };
  
  /******************************************
   * OpenFrames API, class OpenVRSlaveCallback
   * Compute per-eye view matrix without changing projection matrix. This should
   * be attached as a slave update callback to each VR camera.
   ******************************************/
  struct OpenVRSlaveCallback : public osg::View::Slave::UpdateSlaveCallback
  {
    enum CameraType
    {
      RIGHT_CAMERA,
      LEFT_CAMERA,
      MONO_CAMERA
    };
    
    OpenVRSlaveCallback(CameraType cameraType, OpenVRDevice *ovrDevice)
    : _cameraType(cameraType), _ovrDevice(ovrDevice)
    { }
    
    virtual void updateSlave(osg::View& view, osg::View::Slave& slave);
    
    CameraType _cameraType;
    osg::observer_ptr<OpenVRDevice> _ovrDevice;
  };
  
  /******************************************
   * OpenFrames API, class OpenVRTrackball
   * Extends FollowingTrackball to include the OpenVR HMD transform.
   * This results in the World->Head transform, which can be combined
   * with the Head->Eye transform to create the per-eye view matrix.
   ******************************************/
  class OpenVRTrackball : public FollowingTrackball
  {
  public:
    OpenVRTrackball(OpenVRDevice *ovrDevice);
    
    virtual const char* className() const { return "OpenVRTrackball"; }

    // Get World to Head matrix
    virtual osg::Matrixd getInverseMatrix() const;

    // Handle event
    virtual bool handle(const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& us);

    // Update internal view matrix and update the specified camera
    virtual void updateCamera(osg::Camera& camera);

  private:
    osg::observer_ptr<OpenVRDevice> _ovrDevice;

    enum MotionMode
    {
      NONE = 0,
      TRANSLATE,
      ROTATE,
      SCALE
    };

    /** Data used when computing world transformations during user events */
    struct MotionData
    {
      MotionMode _mode;
      MotionMode _prevMode;
      double _prevTime;
      unsigned int _device1ID;
      unsigned int _device2ID;
      osg::Matrixd _device1OrigPose;
      osg::Matrixd _device1OrigPoseRaw;
      osg::Matrixd _device2OrigPose;
      osg::Matrixd _device2OrigPoseRaw;
      double _origWorldUnitsPerMeter;
      osg::Vec3d _origCenter;
      osg::Quat _origRotation;
      double _origDistance;
      osg::Matrixd _origTrackball;
      osg::Vec3d _origPoseOffsetRaw;
    } _motionData;
    void saveCurrentMotionData();
  };
  
  /******************************************
   * OpenFrames API, class OpenVRSwapBuffers
   * Submit eye textures to OpenVR. This should be attached as a swapbuffers
   * callback to a graphics context.
   ******************************************/
  struct OpenVRSwapBuffers : public osg::GraphicsContext::SwapCallback
  {
  public:
    OpenVRSwapBuffers(OpenVRDevice *ovrDevice, VRTextureBuffer *texBuffer);
    
    // Submit eye textures to OpenVR and do standard swap buffers
    virtual void swapBuffersImplementation(osg::GraphicsContext *gc);
    
    osg::observer_ptr<OpenVRDevice> _ovrDevice;
    osg::observer_ptr<VRTextureBuffer> _texBuffer;
  };
  
} // !namespace OpenFrames

#endif  // !define _OF_OPENVRDEVICE_
