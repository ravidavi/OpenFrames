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
#include <algorithm>

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
  public:
    /** Create a new OpenVR device, specifying relationship between world units and
     real-world meters, and the user height in meters. */
    OpenVRDevice(double worldUnitsPerMeter, double userHeight);

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
    osg::MatrixTransform* getDeviceRenderModels() { return _deviceModels; }
    
    /** An OpenVR device's class */
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
      DeviceModel() : _valid(false), _class(NONE)
      {
        _modelTransform = new osg::MatrixTransform;
        _modelTransform->setNodeMask(0x0);
      }
      osg::ref_ptr<osg::MatrixTransform> _modelTransform; // In world units
      osg::Matrixd _rawDeviceToWorld; // In OpenVR units [meters]
      bool _valid;
      DeviceClass _class;
    };
    
    /** Get device models */
    unsigned int getNumDeviceModels() { return _deviceIDToModel.size(); }
    const DeviceModel* getDeviceModel(unsigned int id)
    {
      if(id < _deviceIDToModel.size()) return &(_deviceIDToModel[id]);
      else return NULL;
    }
    
    /** Get the HMD pose matrix, in world units */
    const osg::Matrixd& getHMDPoseMatrix()
    { return _deviceIDToModel[0]._modelTransform->getMatrix(); }

    /** Update the per-eye projection matrix */
    void updateProjectionMatrices();
    osg::Matrixd& getRightEyeProjectionMatrix() { return _rightEyeProj; }
    osg::Matrixd& getLeftEyeProjectionMatrix() { return _leftEyeProj; }
    osg::Matrixd& getCenterProjectionMatrix() { return _centerProj; }
    
    /** Get the per-eye world offset vector, in world units (see worldUnitsPerMeter) */
    osg::Vec3d& getRightEyeViewOffset() { return _rightEyeViewOffset; }
    osg::Vec3d& getLeftEyeViewOffset() { return _leftEyeViewOffset; }
    osg::Vec3d& getCenterViewOffset() { return _centerViewOffset; }

    /** Update raw poses (in OpenVR units [meters]) of all VR devices, and wait
     for the signal to start the next frame. */
    void waitGetPoses();
    void computeDeviceTransforms(); // Compute Local<->World device transforms
    
    /** Get/set the world units per meter ratio and its limits */
    void setWorldUnitsPerMeter(const double& worldUnitsPerMeter);
    double getWorldUnitsPerMeter() { return _worldUnitsPerMeter; }
    void setWorldUnitsPerMeterLimits(const double& minWorldUnitsPerMeter,
                                     const double& maxWorldUnitsPerMeter)
    {
      _minWorldUnitsPerMeter = std::max(0.0, minWorldUnitsPerMeter);
      _maxWorldUnitsPerMeter = std::max(_minWorldUnitsPerMeter, maxWorldUnitsPerMeter);
      
      _worldUnitsPerMeter = std::max(_minWorldUnitsPerMeter, std::min(_worldUnitsPerMeter, _maxWorldUnitsPerMeter));
    }
    void getWorldUnitsPerMeterLimits(double &minWorldUnitsPerMeter, double &maxWorldUnitsPerMeter)
    {
      minWorldUnitsPerMeter = _minWorldUnitsPerMeter;
      maxWorldUnitsPerMeter = _maxWorldUnitsPerMeter;
    }
    
    /** Get/set the user height in meters */
    void setUserHeight(double userHeight) { _userHeight = userHeight; }
    double getUserHeight() { return _userHeight; }

    /** Get the controller laser */
    osg::MatrixTransform* getControllerLaser(uint32_t deviceID);

    /** Get the ground plane */
    osg::MatrixTransform* getGroundPlane() { return _roomGround; }

    /** Submits the latest rendered eye textures to OpenVR */
    void submitFrame(GLuint rightEyeTexName, GLuint leftEyeTexName);

    /** Get the next OpenVR event */
    bool pollNextEvent(OpenVREvent *event);

    /** Get the state of the specified OpenVR controller */
    void getControllerState(uint32_t deviceID, vr::VRControllerState_t *state);
    
  protected:
    virtual ~OpenVRDevice();
    
    /** Update the per-eye raw view offset vector, in OpenVR units [meters] */
    void updateViewOffsets();
    
    /** Create the render models for each OpenVR device and misc UI elements */
    void createDeviceRenderModels();

    /** Load a device's render model by its OpenVR ID */
    void setupRenderModelForTrackedDevice(uint32_t deviceID);

    /** Add laser to controller */
    void createAndAddLaserToController(uint32_t deviceID);
    
    double _worldUnitsPerMeter; // Distance units per real-world meter
    double _minWorldUnitsPerMeter, _maxWorldUnitsPerMeter;
    double _userHeight; // Height of user's HMD origin in meters
    int _width, _height; // Per-eye texture dimensions
    
    bool _isInitialized; // Whether OpenVR is initialized
    
    vr::IVRSystem* _vrSystem; // OpenVR interface
    vr::IVRRenderModels* _vrRenderModels; // Controller models

    /** Map between a device's OpenVR name and its OSG render model */
    typedef std::map<std::string, osg::ref_ptr<osg::Geode> > DeviceGeodeMap;
    DeviceGeodeMap _deviceNameToGeode;

    /** Vector of each device's rendering data indexed by its OpenVR id */
    typedef std::vector<DeviceModel> DeviceModelVector;
    DeviceModelVector _deviceIDToModel;
    
    // Group that contains all device models
    osg::ref_ptr<osg::MatrixTransform> _deviceModels;

    // Transform that contains midpoint between controllers
    osg::ref_ptr<osg::MatrixTransform> _controllerMidpoint;

    // Transform that contains local ground plane
    osg::ref_ptr<osg::MatrixTransform> _roomGround;

    // Geode that contains picking laser for each controller
    osg::ref_ptr<osg::Geode> _controllerLaser;
    const std::string _controllerLaserName = "ControllerLaser";

    // Per-eye asymmetric projection matrices
    osg::Matrixd _rightEyeProj, _leftEyeProj, _centerProj;
    
    // Per-eye view offsets, transform from HMD to Eye space
    osg::Vec3d _rightEyeViewOffset, _leftEyeViewOffset, _centerViewOffset;
    osg::Vec3d _rightEyeViewOffsetRaw, _leftEyeViewOffsetRaw, _centerViewOffsetRaw;
    
    double _ipd; // Current interpupillary distance
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

    // Get World to HMD (Center) matrix
    virtual osg::Matrixd getInverseMatrix() const;

    // Handle event
    virtual bool handle(const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& us);

  protected:
    osg::observer_ptr<OpenVRDevice> _ovrDevice;
    
    // Transformation from room space to trackball space
    osg::Matrixd _roomPose;
    
    // Get the Room->World transform for the trackball itself, not including any
    // custom view transformations by FollowingTrackball. This allows transformations
    // between room space and the space viewed by the osgGA::Trackball.
    void getTrackballRoomToWorldMatrix(osg::Matrixd& matrix);

    void processMotion();
    
    /** Type of user motion currently being handled */
    enum MotionMode
    {
      NONE = 0,
      TRANSLATE,
      ROTATE,
      SCALE,
      PICK
    };

    /** Data used when computing world transformations during user events */
    struct MotionData
    {
      MotionMode _mode;
      MotionMode _prevMode;
      double _prevTime;
      uint32_t _device1ID;
      uint32_t _device2ID;
      osg::Matrixd _device1OrigPoseRaw;
      osg::Matrixd _device2OrigPoseRaw;
      double _origWorldUnitsPerMeter;
      osg::Quat _origRotation;
      osg::Matrixd _origTrackball;
      osg::Matrixd _origRoomPose;
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
