/***********************************
 Copyright 2023 Ravishankar Mathur
 
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

/** \file OpenVRDevice.hpp
 * Declaration of OpenVRDevice class.
 */

#ifndef _OF_OPENVRDEVICE_
#define _OF_OPENVRDEVICE_

#include <OpenFrames/Export.h>
#include <OpenFrames/View.hpp>
#include <osg/ref_ptr>
#include <osg/Referenced>
#include <osg/Quat>
#include <osg/LineWidth>
#include <osg/MatrixTransform>
#include <osg/Texture>
#include <osg/Vec3d>
#include <osgGA/Device>
#include <osgViewer/ViewerEventHandlers>
#include <algorithm>

/**
 * \cond SKIP_VR_NAMESPACE_PROCESSING
 * doxygen seems to get upset about the fact that a) VRControllerState001_t
 * isn't defined anywhere and b) this undefined thing is then used in a
 * typedef.
 */
/** 
Classes declared in OpenVR header. We don't include the OpenVR header here because
the user might want to use the Stub version of the OpenVR device.
*/
namespace vr
{
  class IVRSystem;
  class IVRRenderModels;
  struct VREvent_t;
  struct VRControllerState001_t;
  typedef VRControllerState001_t VRControllerState_t;
}
/**
 * \endcond
 */

namespace OpenFrames
{
  struct VRTextureBuffer; // Used by OpenVRSwapBuffers below

  /**
   * \class OpenVREvent
   *
   * \brief An OSG-compatible event adapter for OpenVR.
   *
   * This class wraps OpenVR events in an OSG-compatible event adapter.
   */
  class OF_EXPORT OpenVREvent : public osgGA::GUIEventAdapter
  {
  public:
    OpenVREvent();
    ~OpenVREvent();

    void operator=(const OpenVREvent &other);

    vr::VREvent_t *_ovrEvent; // The actual OpenVR event
  };

  /**
   * \class OpenVRDevice
   *
   * \brief Represents data needed to use an OpenVR-supported HMD.
   *
   */
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
    void getRecommendedTextureSize(int& w, int& h) const
    {
      w = _width; h = _height;
    }

    /** Get whether OpenVR has been initialized */
    inline bool isInitialized() const { return _isInitialized; }

    /** Get render models for devices */
    osg::MatrixTransform* getDeviceRenderModels() const { return _deviceModels; }

    /**
     * \class LaserModel
     *
     * \brief Encapsulates the laser attached to OpenVR devices (usually controllers).
     *
     */
    class OF_EXPORT LaserModel : public osg::Referenced
    {
    public:
      LaserModel();

      /// Get Local->World matrix (transforms points in Laser frame to Controller frame)
      osg::MatrixTransform* getTransform() const { return _laserTransform; }

      /// Show/hide the laser
      /// Note that delay-hiding the laser can be overridden by showing the laser
      void showLaser(bool show);
      bool isLaserShown() const { return (_laserTransform->getNodeMask() == ~0x0); }
      
      /// Set a delay for hiding the laser, in seconds
      /// If delay > 0 then any request to hide the laser will be applied only after its
      /// parameters have not changed for the specified amount of time.
      /// Laser hides immediately if delay <= 0
      /// Laser stays on indefinitely if delay = DBL_MAX
      void setLaserHideDelay(const double& delay) { _hideDelay = delay; }
      double getLaserHideDelay() const { return _hideDelay; }

      /// Laser color
      void setColor(const osg::Vec4& color);
      const osg::Vec4& getColor() const { return _colors->back(); }
      void setDefaultColor(const osg::Vec4& color) { _defaultColor = color; }
      const osg::Vec4& getDefaultColor() const { return _defaultColor; }

      /// Laser length (meters)
      void setLength(const double& length);
      double getLength() const { return -(*_vertices)[1].z(); }
      void setDefaultLength(const double& length) { _defaultLength = (length >= 0.0) ? length : _defaultLength; }
      double getDefaultLength() const { return _defaultLength; }

      /// Laser width (pixels)
      void setWidth(const float& width);
      float getWidth() const { return _lineWidth->getWidth(); }
      void setDefaultWidth(const float& width) { _defaultWidth = (width > 0.0) ? width : _defaultWidth; }
      float getDefaultWidth() const { return _defaultWidth; }

      /// Last time any laser property was updated
      const osg::Timer_t& getUpdateTime() const { return _lastUpdateTime; }

      // Set all laser parameters to their defaults
      void restoreDefaults();

    protected:
      virtual ~LaserModel();

      osg::ref_ptr<osg::MatrixTransform> _laserTransform;
      osg::Timer_t _lastUpdateTime; // Time (in seconds since osg::Timer epoch) of last laser update
      osg::ref_ptr<osg::Geometry> _geom;
      osg::ref_ptr<osg::Vec3Array> _vertices;
      osg::ref_ptr<osg::Vec4Array> _colors;
      osg::ref_ptr<osg::LineWidth> _lineWidth;
      double _hideDelay;
      double _defaultLength;
      osg::Vec4 _defaultColor;
      float _defaultWidth;
    };

    /** An OpenVR device's class */
    enum DeviceClass
    {
      NONE = 0,
      HMD = 1,
      BASESTATION = 2,
      CONTROLLER = 3
    };

    /**
     * \class DeviceModel
     *
     * \brief Encapsulates an OpenVR device's model.
     *
     */
    struct OF_EXPORT DeviceModel
    {
      DeviceModel() : _valid(false), _class(NONE), _controllerState(nullptr)
      {
        _modelTransform = new osg::MatrixTransform;
        _modelTransform->setNodeMask(0x0);
      }
      ~DeviceModel();

      osg::ref_ptr<osg::MatrixTransform> _modelTransform; // In world units
      osg::Matrixd _rawDeviceToWorld; // In OpenVR units [meters]
      bool _valid; // Whether device is actively being tracked by OpenVR
      DeviceClass _class;

      // These variables only apply if the device class is CONTROLLER, otherwise they are NULL
      // TODO: subclass DeviceModel with appropriate device classes 
      osg::ref_ptr<LaserModel> _laser; // The controller laser
      vr::VRControllerState_t *_controllerState; // Most recent controller state (buttons, axes, etc.)
    };
    
    /** Get device models */
    unsigned int getNumDeviceModels() const { return _deviceIDToModel.size(); }
    const DeviceModel* getDeviceModel(unsigned int id) const
    {
      if(id < _deviceIDToModel.size()) return &(_deviceIDToModel[id]);
      else return NULL;
    }

    /** Update all device models, e.g. controller states */
    void updateDeviceModels();
    
    /** Get the HMD pose matrix, in world units */
    const osg::Matrixd& getHMDPoseMatrix() const
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
    double getWorldUnitsPerMeter() const { return _worldUnitsPerMeter; }
    void setWorldUnitsPerMeterLimits(const double& minWorldUnitsPerMeter,
                                     const double& maxWorldUnitsPerMeter)
    {
      _minWorldUnitsPerMeter = std::max(0.0, minWorldUnitsPerMeter);
      _maxWorldUnitsPerMeter = std::max(_minWorldUnitsPerMeter, maxWorldUnitsPerMeter);
      
      // Ensure current WorldUnits/Meter is within bounds
      double newWUM = std::max(_minWorldUnitsPerMeter, std::min(_worldUnitsPerMeter, _maxWorldUnitsPerMeter));
      setWorldUnitsPerMeter(newWUM);
    }
    void getWorldUnitsPerMeterLimits(double &minWorldUnitsPerMeter, double &maxWorldUnitsPerMeter) const
    {
      minWorldUnitsPerMeter = _minWorldUnitsPerMeter;
      maxWorldUnitsPerMeter = _maxWorldUnitsPerMeter;
    }
    
    /** Get/set the user height in meters */
    void setUserHeight(double userHeight) { _userHeight = userHeight; }
    double getUserHeight() const { return _userHeight; }

    /** Get the ground plane */
    osg::MatrixTransform* getGroundPlane() { return _roomGround; }

    /** Submits the latest rendered eye textures to OpenVR */
    void submitFrame(GLuint rightEyeTexName, GLuint leftEyeTexName);

    /** Get the next OpenVR event */
    bool pollNextEvent(OpenVREvent *event);

    /** Get the OpenVR subsystem */
    vr::IVRSystem* getVRSystem() const { return _vrSystem; }
    
  protected:
    virtual ~OpenVRDevice();
    
    /** Update the per-eye raw view offset vector, in OpenVR units [meters] */
    void updateViewOffsets();
    
    /** Create the render models for each OpenVR device and misc UI elements */
    void createDeviceRenderModels();

    /** Load a device's render model by its OpenVR ID */
    void setupRenderModelForTrackedDevice(uint32_t deviceID);
    
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
    /// TODO: Make this a vector of pointers, then DeviceModel can be subclassed
    typedef std::vector<DeviceModel> DeviceModelVector;
    DeviceModelVector _deviceIDToModel;

    /**
     * \class DeviceModelEventCallback
     *
     * \brief Event callback that shows/hides a VR controller's laser when its trigger is pressed.
     *
     */
    struct DeviceModelEventCallback : public osgGA::GUIEventHandler
    {
      DeviceModelEventCallback(OpenVRDevice* ovrDevice)
        : _ovrDevice(ovrDevice)
      {}

      // Handle the OpenVR event
      virtual bool handle(const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& aa, osg::Object* obj, osg::NodeVisitor* nv);

      osg::observer_ptr<OpenVRDevice> _ovrDevice;
    };

    // Group that contains all device models
    osg::ref_ptr<osg::MatrixTransform> _deviceModels;

    // Transform that contains midpoint between controllers
    osg::ref_ptr<osg::MatrixTransform> _controllerMidpoint;

    // Transform that contains local ground plane
    osg::ref_ptr<osg::MatrixTransform> _roomGround;

    // Per-eye asymmetric projection matrices
    osg::Matrixd _rightEyeProj, _leftEyeProj, _centerProj;
    
    // Per-eye view offsets, transform from HMD to Eye space
    osg::Vec3d _rightEyeViewOffset, _leftEyeViewOffset, _centerViewOffset;
    osg::Vec3d _rightEyeViewOffsetRaw, _leftEyeViewOffsetRaw, _centerViewOffsetRaw;
  };

  /**
   * \class OpenVREventDevice
   *
   * \brief Polls for OpenVR events and stores them in its OSG event queue.
   *
   */
  class OF_EXPORT OpenVREventDevice : public osgGA::Device
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

  /**
   * \class OpenVRSlaveCallback
   *
   * \brief This class computes per-eye view matrices.
   *
   * This struct-class computes per-eye view matrices without changing the projection matrix. This
   * should be attached as a slave update callback to each VR camera.
   */
  struct OF_EXPORT OpenVRSlaveCallback : public osg::View::Slave::UpdateSlaveCallback
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

  /**
   * \class OpenVRTrackball
   *
   * \brief This class extends FollowingTrackball to include the OpenVR HMD transform.
   *
   * This class extends FollowingTrackball to include the OpenVR HMD transform,
   * resulting in the World->Head transform. That can be combined with the
   * Head->Eye transform to create the per-eye view matrices.
   */
  class OF_EXPORT OpenVRTrackball : public FollowingTrackball
  {
  public:
    OpenVRTrackball(OpenVRDevice *ovrDevice);
    
    virtual const char* className() const { return "OpenVRTrackball"; }

    // Inherited from FollowingTrackball
    virtual osg::Matrixd getMatrix() const; // HMD (Center) to World matrix
    virtual osg::Matrixd getInverseMatrix() const; // World to HMD (Center) matrix

    // Get Room to Trackball matrix
    // Additional transformation matrices for an OpenVRTrackball can be computed as follows:
    // Room -> ViewFrame: tb->getRoomToTrackballMatrix() * tb->osgGA::TrackballManipulator::getMatrix()
    // Room -> World:     tb->getRoomToTrackballMatrix() * tb->FollowingTrackball::getMatrix()
    const osg::Matrixd& getRoomToTrackballMatrix() const { return _roomPose; }
    void setRoomToTrackballMatrix(const osg::Matrixd& roomPose) { _roomPose = roomPose; }
    
    // Get/set default WorldUnits/Meter ratio
    double getDefaultWorldUnitsPerMeterRatio() const { return _defaultWorldUnitsPerMeter; }
    void setDefaultWorldUnitsPerMeterRatio(const double& wum) { _defaultWorldUnitsPerMeter = wum; }
    
    // Specify action when user presses one grip button on the VR controller
    enum OneButtonMode
    {
      ONEBUTTON_TRANSLATE,
      ONEBUTTON_ROTATE,
      ONEBUTTON_DISABLE
    };
    void setOneButtonMode(OneButtonMode mode) { _oneButtonMode = mode; }
    OneButtonMode getOneButtonMode() const { return _oneButtonMode; }

    // Specify action when user presses grip buttons on both VR controllers
    enum TwoButtonMode
    {
      TWOBUTTON_ROTATESCALE,
      TWOBUTTON_DISABLE
    };
    void setTwoButtonMode(TwoButtonMode mode) { _twoButtonMode = mode; }
    TwoButtonMode getTwoButtonMode() const { return _twoButtonMode; }

    // Handle event
    virtual bool handle(const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& us);

  protected:
    osg::observer_ptr<OpenVRDevice> _ovrDevice;
    
    // Transformation from room space to trackball space
    osg::Matrixd _roomPose, _savedRoomPose;

    double _savedWorldUnitsPerMeter;    // Last saved WorldUnits/Meter ratio
    double _defaultWorldUnitsPerMeter;  // Default WorldUnits/Meter ratio
    
    // Process user's controller motion into view changes
    void processMotion();
    void processOneButtonMotion();
    void processTwoButtonMotion();
    
    // Save, restore, and reset VR-related trackball state
    virtual void saveState();
    virtual void restoreState();
    virtual void resetState();
    
    /** Type of user motion when one or two buttons are pressed */
    enum MotionMode
    {
      NONE = 0,
      ONEBUTTON,
      TWOBUTTON
    };
    MotionMode _motionMode;

    OneButtonMode _oneButtonMode;
    TwoButtonMode _twoButtonMode;

    /**
     * \class MotionData
     *
     * \brief Data used when computing world transformations during user events.
     *
     */
    struct MotionData
    {
      MotionMode _mode;
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

  /**
   * \class OpenVRSwapBuffers
   *
   * \brief This class submits eye textures to OpenVR.
   *
   * This class submit eye textures to OpenVR and should be attached as a swapbuffers
   * callback to a graphics context.
   */
  struct OF_EXPORT OpenVRSwapBuffers : public osg::GraphicsContext::SwapCallback
  {
  public:
    OpenVRSwapBuffers(OpenVRDevice *ovrDevice, VRTextureBuffer *texBuffer);
    
    // Submit eye textures to OpenVR and do standard swap buffers
    virtual void swapBuffersImplementation(osg::GraphicsContext *gc);
    
    osg::observer_ptr<OpenVRDevice> _ovrDevice;
    osg::observer_ptr<VRTextureBuffer> _texBuffer;
  };

  /**
   * \class OpenVRImageHandler
   *
   * \brief Event handler that enables clicking on an image using VR controllers.
   *
   */
  class OF_EXPORT OpenVRImageHandler : public osgViewer::InteractiveImageHandler
  {
  public:
    OpenVRImageHandler(const OpenVRDevice *ovrDevice, osg::Image* image);

    META_Object(OpenFrames, OpenVRImageHandler);

    // Inherited from InteractiveImageHandler
    // Translate an OpenVR controller event to a Qt widget click
    virtual bool handle(const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& aa, osg::Object* obj, osg::NodeVisitor* nv);

    // Set properties of laser when it is pointed at an image
    void setSelectedWidth(const float& width) { _laserSelectedWidth = (width > 0) ? width : _laserSelectedWidth; }
    float getSelectedWidth() { return _laserSelectedWidth; }

    void setSelectedColor(const osg::Vec4& color) { _laserSelectedColor = color; }
    const osg::Vec4& getSelectedColor() const { return _laserSelectedColor; }

    // Set how far the trigger must be pulled to signal a mousedown event, in range [0, 1]
    void setTriggerThreshold(const float& threshold)
    {
      if ((threshold >= 0.0) && (threshold <= 1.0)) _triggerThreshold = threshold;
    }
    float getTriggerThreshold() const { return _triggerThreshold; }

  protected:
    virtual ~OpenVRImageHandler() {}

    OpenVRImageHandler() :
      osgViewer::InteractiveImageHandler()
    {}

    OpenVRImageHandler(const OpenVRImageHandler &ovrih, const osg::CopyOp &copyop = osg::CopyOp::SHALLOW_COPY) :
      osgViewer::InteractiveImageHandler(ovrih, copyop)
    {}

    /// Process the image pick operatoin at each frame
    void processImagePick();

    /// Get the value of the controller's trigger button
    float getTriggerValue(const vr::VRControllerState_t *controllerState) const;

    /** Type of image pick action currently being handled */
    enum PickMode
    {
      NONE = 0,
      MOUSEACTION // Mouse move or click
    };

    /**
     * \class PickData
     *
     * \brief Data used when computing world transformations during user events.
     *
     */
    struct PickData
    {
      PickMode mode;
      uint32_t deviceID;
      OpenVRTrackball* trackball;
      osg::NodePath nodePath;
    } _pickData;
    void saveCurrentPickData(PickMode mode, osgViewer::View* view, osg::NodeVisitor* nv, uint32_t device1ID);

    osg::observer_ptr<const OpenVRDevice> _ovrDevice;

    float _laserSelectedWidth;
    osg::Vec4 _laserSelectedColor;
    float _triggerThreshold;
  };

} // !namespace OpenFrames

#endif  // !define _OF_OPENVRDEVICE_
