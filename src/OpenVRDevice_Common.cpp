/***********************************
 Copyright 2018 Ravishankar Mathur
 
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
#include <OpenFrames/VRUtils.hpp>
#include <osgDB/ReadFile>
#include <osg/io_utils>
#include <osgUtil/RayIntersector>

namespace OpenFrames{

  /*************************************************************/
  // Callback that overrides the current VR camera's World->Eye modelview
  // matrix with the appropriate Room->Eye modelview matrix. This is
  // needed since OpenVR devices exist in room space, not in world space.
  class OpenVRDeviceTransformCallback : public osg::Callback
  {
  public:
    OpenVRDeviceTransformCallback(OpenVRDevice *ovrDevice)
    : _ovrDevice(ovrDevice)
    {}

    virtual bool run(osg::Object* object, osg::Object* data)
    {
      osg::MatrixTransform *deviceTransform = dynamic_cast<osg::MatrixTransform*>(object);
      osgUtil::CullVisitor *cv = dynamic_cast<osgUtil::CullVisitor*>(data);
      if (deviceTransform && cv)
      {
        // Make sure the current camera is a VR camera
        osg::Camera *vrCam = cv->getCurrentCamera();
        osg::View *view = vrCam->getView();
        osg::View::Slave *slave = view->findSlaveForCamera(vrCam);

        if (slave)
        {
          // VR cameras have a custom slave callback
          OpenVRSlaveCallback *vrSlaveCallback = dynamic_cast<OpenVRSlaveCallback*>(slave->_updateSlaveCallback.get());
          if (vrSlaveCallback)
          {
            // Get the HMD Center->Eye offset matrix
            osg::Vec3d viewOffset;
            if (vrSlaveCallback->_cameraType == OpenVRSlaveCallback::RIGHT_CAMERA)
            {
              viewOffset = _ovrDevice->getRightEyeViewOffset();
            }
            else if (vrSlaveCallback->_cameraType == OpenVRSlaveCallback::LEFT_CAMERA)
            {
              viewOffset = _ovrDevice->getLeftEyeViewOffset();
            }
            // Nothing to do for MONO_CAMERA since the view matrix includes the Center offset vector

            // Get the Room -> HMD (Center) view matrix, which will be stored as the
            // current modelview matrix
            osg::ref_ptr<osg::RefMatrixd> currViewMatrix = cv->getModelViewMatrix();

            // Append the HMD Center->Eye view offset
            currViewMatrix->postMultTranslate(viewOffset);

            // Replace the CullVisitor's current modelview matrix
            cv->popModelViewMatrix();
            cv->pushModelViewMatrix(currViewMatrix.get(), deviceTransform->getReferenceFrame());
          }
        }
      }

      // Continue traversing if needed
      return traverse(object, data);
    }

  protected:
    osg::observer_ptr<OpenVRDevice> _ovrDevice;
  };
  
  /*************************************************************/
  OpenVRDevice::OpenVRDevice(double worldUnitsPerMeter, double userHeight)
    : _worldUnitsPerMeter(-1.0),
    _minWorldUnitsPerMeter(0.0),
    _maxWorldUnitsPerMeter(DBL_MAX),
    _userHeight(userHeight),
    _width(0),
    _height(0),
    _isInitialized(false),
    _vrSystem(nullptr),
    _vrRenderModels(nullptr)
  {
    // Set up a transform for the device render models
    // These models exist in local space (the room), so their view matrix should only
    // include the HMD transform.
    _deviceModels = new osg::MatrixTransform();
    _deviceModels->setCullCallback(new OpenVRDeviceTransformCallback(this));
    _deviceModels->setReferenceFrame(osg::Transform::ABSOLUTE_RF); // Override view matrix
    _deviceModels->setName("OpenVR Device Models");

    // We will scale device models according to the provided WorldUnit/Meter ratio, so
    // make sure that model normals are rescaled by OpenGL
    //_deviceModels->getOrCreateStateSet()->setMode(GL_RESCALE_NORMAL, osg::StateAttribute::ON);
    _deviceModels->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF);

    // Create the sphere that represents the controller midpoint
    float radius = 0.01; // Radius in meters
    osg::Geode *geode = new osg::Geode;
    osg::ShapeDrawable *sd = new osg::ShapeDrawable(new osg::Sphere(osg::Vec3(), radius));
    sd->setColor(osg::Vec4(1.0, 1.0, 1.0, 0.2));
    geode->addDrawable(sd);
    _controllerMidpoint = new osg::MatrixTransform;
    _controllerMidpoint->addChild(geode);
    _deviceModels->addChild(_controllerMidpoint);

    // Set midpoint sphere texture
    osg::Image *image = osgDB::readImageFile("Images/marble.jpg");
    osg::StateSet* ss;
    if (image)
    {
      osg::Texture2D *texture = new osg::Texture2D(image);
      texture->setFilter(osg::Texture::MIN_FILTER, osg::Texture::LINEAR);
      texture->setFilter(osg::Texture::MAG_FILTER, osg::Texture::LINEAR);
      ss = geode->getOrCreateStateSet();
      ss->setTextureAttributeAndModes(0, texture, osg::StateAttribute::ON);
      ss->setMode(GL_BLEND, osg::StateAttribute::ON);
      ss->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
    }

    // Create a flat grid to represent the ground
    {
      // Create the ground plane
      osg::Vec3Array* vertices = new osg::Vec3Array(4);
      int groundSize = 10;
      (*vertices)[0].set(-groundSize, 0, -groundSize);
      (*vertices)[1].set(groundSize, 0, -groundSize);
      (*vertices)[2].set(groundSize, 0, groundSize);
      (*vertices)[3].set(-groundSize, 0, groundSize);
      osg::Vec4Array* color = new osg::Vec4Array;
      color->push_back(osg::Vec4(0.5, 0.5, 0.5, 0.1));
      osg::Geometry* planeGeom = new osg::Geometry();
      planeGeom->setUseDisplayList(false);
      planeGeom->setUseVertexBufferObjects(true);
      planeGeom->getOrCreateVertexBufferObject()->setUsage(GL_STATIC_DRAW);
      planeGeom->setVertexArray(vertices);
      planeGeom->setColorArray(color, osg::Array::BIND_OVERALL);
      planeGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS, 0, 4));

      // Create the ground grid
      vertices = new osg::Vec3Array;
      int numGridPoints = 0;
      double y_offset = 0.001;
      for (int i = -groundSize; i <= groundSize; ++i)
      {
        // Add vertical line
        vertices->push_back(osg::Vec3d(i, y_offset, -groundSize));
        vertices->push_back(osg::Vec3d(i, y_offset, groundSize));

        // Add horizontal line
        vertices->push_back(osg::Vec3d(-groundSize, y_offset, i));
        vertices->push_back(osg::Vec3d(groundSize, y_offset, i));

        numGridPoints += 4;
      }
      color = new osg::Vec4Array;
      color->push_back(osg::Vec4(0.8, 0.8, 0.8, 0.5));
      osg::Geometry* gridGeom = new osg::Geometry();
      gridGeom->setUseDisplayList(false);
      gridGeom->setUseVertexBufferObjects(true);
      gridGeom->getOrCreateVertexBufferObject()->setUsage(GL_STATIC_DRAW);
      gridGeom->setVertexArray(vertices);
      gridGeom->setColorArray(color, osg::Array::BIND_OVERALL);
      gridGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES, 0, numGridPoints));

      // Create text to show WorldUnits/Meter ratio
      osgText::Text *wumText = new osgText::Text();
      wumText->setFontResolution(30, 30);
      wumText->setFont("arial.ttf");
      wumText->setCharacterSizeMode(osgText::Text::OBJECT_COORDS);
      wumText->setCharacterSize(0.1);
      wumText->setPosition(osg::Vec3(0.0, y_offset, 0.0));
      wumText->setAxisAlignment(osgText::Text::USER_DEFINED_ROTATION);
      osg::Quat wumRotate;
      wumRotate.makeRotate(-osg::PI_2, osg::Vec3(1, 0, 0)); // Using XZ_PLANE would make text upside-down
      wumText->setRotation(wumRotate);

      // Create geode to hold plane and grid geometries
      geode = new osg::Geode;
      geode->addDrawable(planeGeom);
      geode->addDrawable(gridGeom);
      geode->addDrawable(wumText);
      ss = geode->getOrCreateStateSet();
      ss->setMode(GL_BLEND, osg::StateAttribute::ON);
      ss->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);

      // Finalize setup of room decorations
      _roomGround = new osg::MatrixTransform;
      _roomGround->addChild(geode);
      _deviceModels->addChild(_roomGround);

      setWorldUnitsPerMeter(worldUnitsPerMeter); // Set the WorldUnits/Meter text
    }
  }

  /*************************************************************/
  OpenVRDevice::~OpenVRDevice()
  {
    shutdownVR();
  }

  /*************************************************************/
  struct LaserUpdateCallback : public osg::Callback
  {
    LaserUpdateCallback(OpenVRDevice::LaserModel* laser)
      : _laser(laser)
    {}

    virtual bool run(osg::Object* object, osg::Object* data)
    {
      // Restore laser to default if enough time has passed since it was last updated
      osg::Timer_t lastUpdateTime = _laser->getUpdateTime();
      osg::Timer_t currTime = osg::Timer::instance()->tick();
      double dt = osg::Timer::instance()->delta_s(lastUpdateTime, currTime);
      if (dt > 0.1) _laser->restoreDefaults();

      // Continue with other callbacks
      return osg::Callback::traverse(object, data);
    }

    osg::observer_ptr<OpenVRDevice::LaserModel> _laser;
  };

  /*************************************************************/
  OpenVRDevice::LaserModel::LaserModel()
  {
    // Set laser defaults
    _defaultLength = 10.0;
    _defaultColor = osg::Vec4(1, 1, 1, 1);
    _defaultWidth = 2.0;

    // Initialize update time
    _lastUpdateTime = osg::Timer::instance()->tick();

    // Create the laser that will be used for VR controller picking
    _vertices = new osg::Vec3Array(2);
    (*_vertices)[0].set(0, 0, 0);
    (*_vertices)[1].set(0, 0, -_defaultLength); // Laser along -Z axis in room units (meters)
    _colors = new osg::Vec4Array;
    _colors->push_back(_defaultColor);
    _geom = new osg::Geometry();
    _geom->setUseDisplayList(false);
    _geom->setUseVertexBufferObjects(true);
    _geom->getOrCreateVertexBufferObject()->setUsage(GL_STATIC_DRAW);
    _geom->setVertexArray(_vertices);
    _geom->setColorArray(_colors, osg::Array::BIND_OVERALL);
    _geom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES, 0, 2));
    _lineWidth = new osg::LineWidth(_defaultWidth);
    _geom->getOrCreateStateSet()->setAttribute(_lineWidth);
    osg::Geode* controllerLaser = new osg::Geode;
    controllerLaser->addDrawable(_geom);

    _laserTransform = new osg::MatrixTransform;
    _laserTransform->setName("Laser");
    _laserTransform->addChild(controllerLaser);
    _laserTransform->setNodeMask(0x0); // Hide laser by default
    _laserTransform->setUpdateCallback(new LaserUpdateCallback(this));
  }

  OpenVRDevice::LaserModel::~LaserModel() {}

  /*************************************************************/
  void OpenVRDevice::LaserModel::setColor(const osg::Vec4& color)
  {
    // Only update color if it changed
    if (color != getColor())
    {
      _colors->back() = color;
      _colors->dirty();
      _lastUpdateTime = osg::Timer::instance()->tick();
    }
  }

  /*************************************************************/
  void OpenVRDevice::LaserModel::setLength(const double& length)
  {
    // Only update length if it changed and the new length >= 0
    if ((length >= 0.0) && (length != getLength()))
    {
      (*_vertices)[1].z() = -length;
      _vertices->dirty();
      _geom->dirtyBound();
      _lastUpdateTime = osg::Timer::instance()->tick();
    }
  }

  /*************************************************************/
  void OpenVRDevice::LaserModel::setWidth(const float& width)
  {
    // Only update width if it changed and the new width >= 0
    if ((width > 0.0) && (width != getWidth()))
    {
      _lineWidth->setWidth(width);
      _lastUpdateTime = osg::Timer::instance()->tick();
    }
  }

  /*************************************************************/
  void OpenVRDevice::LaserModel::restoreDefaults()
  {
    setColor(_defaultColor);
    setLength(_defaultLength);
    setWidth(_defaultWidth);
  }

  /*************************************************************/
  void OpenVRDevice::computeDeviceTransforms()
  {
    osg::Matrixd matDeviceToWorld; // Device to World transform
    osg::Matrixd matWorldToDevice; // For HMD
    osg::Matrixd matMidpoint; // For controller midpoint
    unsigned int numActiveControllers = 0;
    osg::Vec3d v1, v2;
    
    // Compute transforms for all devices
    for (int i = 0; i < _deviceIDToModel.size(); ++i)
    {
      if(!_deviceIDToModel[i]._valid) continue;
      
      // Convert from meters to world units
      matDeviceToWorld = _deviceIDToModel[i]._rawDeviceToWorld;
      matDeviceToWorld(3, 0) = matDeviceToWorld(3, 0)*_worldUnitsPerMeter;
      matDeviceToWorld(3, 1) = matDeviceToWorld(3, 1)*_worldUnitsPerMeter;
      matDeviceToWorld(3, 2) = matDeviceToWorld(3, 2)*_worldUnitsPerMeter;
      
      if(_deviceIDToModel[i]._class == HMD)
      {
        // Invert since HMD represents World->Local view matrix
        matWorldToDevice.invert(matDeviceToWorld);
        _deviceIDToModel[i]._modelTransform->setMatrix(matWorldToDevice);
      }
      else
      {
        // Save controller orientation and accumulate midpoint position
        if (_deviceIDToModel[i]._class == CONTROLLER)
        {
          ++numActiveControllers;
          if (numActiveControllers == 1)
          {
            v1.set(_deviceIDToModel[i]._rawDeviceToWorld(2, 0),
              _deviceIDToModel[i]._rawDeviceToWorld(2, 1),
              _deviceIDToModel[i]._rawDeviceToWorld(2, 2));
          }
          else
          {
            v2.set(_deviceIDToModel[i]._rawDeviceToWorld(2, 0),
              _deviceIDToModel[i]._rawDeviceToWorld(2, 1),
              _deviceIDToModel[i]._rawDeviceToWorld(2, 2));
          }

          matMidpoint(3, 0) += matDeviceToWorld(3, 0);
          matMidpoint(3, 1) += matDeviceToWorld(3, 1);
          matMidpoint(3, 2) += matDeviceToWorld(3, 2);
        }
          
        // Non-HMD devices need their models scaled to world units
        matDeviceToWorld.preMultScale(osg::Vec3d(_worldUnitsPerMeter, _worldUnitsPerMeter, _worldUnitsPerMeter));
        
        // Set device model's transform from its adjusted pose in world units
        _deviceIDToModel[i]._modelTransform->setMatrix(matDeviceToWorld);
      }
    }

    // Compute controller midpoint transform
    if (numActiveControllers >= 2)
    {
      matMidpoint(3, 0) /= (double)numActiveControllers; // Average controller position
      matMidpoint(3, 1) /= (double)numActiveControllers;
      matMidpoint(3, 2) /= (double)numActiveControllers;
      matMidpoint(0, 0) = _worldUnitsPerMeter; // Scale midpoint model (same as above matrix.preMultScale)
      matMidpoint(1, 1) = _worldUnitsPerMeter;
      matMidpoint(2, 2) = _worldUnitsPerMeter;
      _controllerMidpoint->setMatrix(matMidpoint);
      _controllerMidpoint->setNodeMask(0xffffffff);

      // Fade midpoint sphere as controller z-axes (axial direction) get further apart
      double dist = 3.0*(v1*v2) - 2.0; // Range [-5, 1] since each vector is unit length
      if (dist <= 0.0) dist = 0.0; // Clamp to range [0, 1]
      else dist = 0.5*dist; // Scale to range [0, 0.5]
      osg::Geode *geode = static_cast<osg::Geode*>(_controllerMidpoint->getChild(0));
      osg::ShapeDrawable *sd = static_cast<osg::ShapeDrawable*>(geode->getDrawable(0));
      sd->setColor(osg::Vec4(1.0, 1.0, 1.0, dist));
    }
    else _controllerMidpoint->setNodeMask(0x0);

    // Set scale for ground plane, and place it on the ground
    matDeviceToWorld.makeIdentity();
    matDeviceToWorld(0, 0) = _worldUnitsPerMeter;
    matDeviceToWorld(1, 1) = _worldUnitsPerMeter;
    matDeviceToWorld(2, 2) = _worldUnitsPerMeter;
    matDeviceToWorld(3, 1) = -getUserHeight()*_worldUnitsPerMeter;
    _roomGround->setMatrix(matDeviceToWorld);

    // Scale view offsets according to WorldUnit/Meter scale
    // Note that the vector direction is flipped since we want the Head to Eye transform for OSG
    _rightEyeViewOffset = _rightEyeViewOffsetRaw*(-_worldUnitsPerMeter);
    _leftEyeViewOffset = _leftEyeViewOffsetRaw*(-_worldUnitsPerMeter);
    _centerViewOffset = _centerViewOffsetRaw*(-_worldUnitsPerMeter);

    // Set Room->HMDCenter matrix as VR device transform
    matWorldToDevice = getHMDPoseMatrix();
    matWorldToDevice.postMultTranslate(_centerViewOffset);
    _deviceModels->setMatrix(matWorldToDevice);
  }

  /*************************************************************/
  void OpenVRDevice::setWorldUnitsPerMeter(const double& worldUnitsPerMeter)
  {
    // Make sure WorldUnits/Meter changed
    if (_worldUnitsPerMeter == worldUnitsPerMeter) return;

    // Make sure WorldUnits/Meter stays within desired bounds
    _worldUnitsPerMeter = std::max(_minWorldUnitsPerMeter, std::min(worldUnitsPerMeter, _maxWorldUnitsPerMeter));

    // Update ground text
    osg::Geode *geode = static_cast<osg::Geode*>(_roomGround->getChild(0));
    for (unsigned int i = 0; i < geode->getNumDrawables(); ++i)
    {
      osgText::Text *wumText = dynamic_cast<osgText::Text*>(geode->getDrawable(i));
      if (wumText)
      {
        std::string wumString = std::to_string(_worldUnitsPerMeter);
        size_t loc = wumString.find('.');
        if (loc != std::string::npos)
        {
          wumString.resize(loc + 4);
        }
        wumText->setText(wumString + " Units");
        return;
      }
    }
  }

  /*************************************************************/
  void OpenVRSlaveCallback::updateSlave(osg::View& view, osg::View::Slave& slave)
  {
    // Get the HMD->Eye offset matrix
    osg::Vec3d viewOffset;
    if(_cameraType == RIGHT_CAMERA)
    {
      viewOffset = _ovrDevice->getRightEyeViewOffset();
    }
    else if(_cameraType == LEFT_CAMERA)
    {
      viewOffset = _ovrDevice->getLeftEyeViewOffset();
    }
    // Nothing to do for MONO_CAMERA since the view matrix includes the Center offset vector
    
    // Get the World->HMD (Center) view matrix
    osg::Matrixd viewMatrix = view.getCamera()->getViewMatrix();
    
    // Compute and set the World->Eye view matrix
    viewMatrix.postMultTranslate(viewOffset);
    slave._camera->setViewMatrix(viewMatrix);
    
    // Call parent update slave implementation
    slave.updateSlaveImplementation(view);
  }
  
  /*************************************************************/
  OpenVRTrackball::OpenVRTrackball(OpenVRDevice *ovrDevice)
  : _ovrDevice(ovrDevice)
  {
    _savedWorldUnitsPerMeter = _ovrDevice->getWorldUnitsPerMeter();
    _defaultWorldUnitsPerMeter = _savedWorldUnitsPerMeter;

    // The motion mode defines how controllers change the scene in response
    // to user inputs. Start with no motion.
    _motionData._mode = NONE;
    _motionData._prevMode = TRANSLATE; // Initial button press will go to this mode
    _motionData._prevTime = 0.0;
  }

  /*******************************************************/
  void OpenVRTrackball::saveState()
  {
    // Save state for parent trackball
    FollowingTrackball::saveState();
    
    // Save room pose
    _savedRoomPose = _roomPose;

    // Save WorldUnits/Meter ratio
    _savedWorldUnitsPerMeter = _ovrDevice->getWorldUnitsPerMeter();
  }

  /*******************************************************/
  void OpenVRTrackball::restoreState()
  {
    // Restore state for parent trackball
    FollowingTrackball::restoreState();
    
    // Restore room pose
    _roomPose = _savedRoomPose;

    // Restore WorldUnits/Meter ratio
    _ovrDevice->setWorldUnitsPerMeter(_savedWorldUnitsPerMeter);
  }
  
  /*******************************************************/
  void OpenVRTrackball::resetState()
  {
    // Reset state for parent trackball
    FollowingTrackball::resetState();
    
    // Reset room pose
    _roomPose.makeIdentity();
    _savedRoomPose.makeIdentity();
    
    // Restore WorldUnits/Meter ratio
    _ovrDevice->setWorldUnitsPerMeter(_defaultWorldUnitsPerMeter);
    _savedWorldUnitsPerMeter = _defaultWorldUnitsPerMeter;
  }
  
  /*******************************************************/
  osg::Matrixd OpenVRTrackball::getMatrix() const
  {
    return osg::Matrix::inverse(OpenVRTrackball::getInverseMatrix());
  }

  /*************************************************************/
  osg::Matrixd OpenVRTrackball::getInverseMatrix() const
  {
    // Get World->Local (trackball space) view matrix
    osg::Matrixd matWorldToLocal = FollowingTrackball::getInverseMatrix();
    
    // Include Trackball->Room transform
    matWorldToLocal.postMult(osg::Matrixd::inverse(_roomPose));
    
    // Include Room->HMD transform
    matWorldToLocal.postMult(_ovrDevice->getHMDPoseMatrix());
    
    // Append HMDCenter view offset vector (midpoint of right/left eyes)
    matWorldToLocal.postMultTranslate(_ovrDevice->getCenterViewOffset());
    
    // Return final World->HMDCenter matrix
    return matWorldToLocal;
  }

  /*************************************************************/
  void OpenVRTrackball::processMotion()
  {
    // Exaggerate controller motion beyond a predetermined distance threshold so that
    // the view can be moved faster with larger controller motions.
    // Note that threshold distance depends on arm length, which depends on height.
    // Height/Armspan = 1.0 and ShoulderWidth/Height = 0.3 (see Golden Ratio)
    double armLength = (_ovrDevice->getUserHeight()*0.7) / 2.0; // [meters]
    double fastMotionThreshold = 1.0 - armLength / 4.0;

    const OpenVRDevice::DeviceModel *device1Model = _ovrDevice->getDeviceModel(_motionData._device1ID);
    const OpenVRDevice::DeviceModel *device2Model = _ovrDevice->getDeviceModel(_motionData._device2ID);

    // Handle world transformations based on current motion mode
    switch (_motionData._mode)
    {
    case(ROTATE) :
    {
      // Set the trackball rotation based on controller motion
      // Start by getting the controller location when the button was pressed
      osg::Vec3d origPos = _motionData._device1OrigPoseRaw.getTrans()*_motionData._origWorldUnitsPerMeter;

      // Convert to position relative to trackball's world space
      osg::Vec3d origPosWorld = origPos * _motionData._origTrackball;

      // Next get the current controller location relative to trackball center
      osg::Matrixd device1CurrPoseRaw = device1Model->_rawDeviceToWorld;
      osg::Vec3d currPos = device1CurrPoseRaw.getTrans()*_motionData._origWorldUnitsPerMeter;
      osg::Vec3d currPosWorld = currPos * _motionData._origTrackball;

      // Compute the rotation from current -> original controller positions
      osg::Quat deltaRotation;
      deltaRotation.makeRotate(currPosWorld, origPosWorld);
      osg::Quat newRotation(_motionData._origRotation);
      newRotation *= deltaRotation;
      osgGA::TrackballManipulator::setRotation(newRotation);

      // If we keep the original state constant, then it's likely that the user will try to do
      // a 180-degree rotation which is singular (no Quat::makeRotate solution). Therefore we save
      // the new pose state at each frame, which results in incremental rotations instead of one
      // big rotation from the initial to final controller positions.
      _motionData._device1OrigPoseRaw = device1CurrPoseRaw;
      _motionData._origTrackball = _roomPose * osgGA::TrackballManipulator::getMatrix();
      _motionData._origRotation = newRotation;

      break;
    }

    case(TRANSLATE) :
    {
      // Set the device pose offset based on controller location
      // Start by getting the controller location when the button was pressed
      osg::Vec3d origPos = _motionData._device1OrigPoseRaw.getTrans();

      // Next get the current controller location
      osg::Vec3d currPos = device1Model->_rawDeviceToWorld.getTrans();

      // Move room in the opposite direction as controller motion.
      osg::Vec3d deltaPos = origPos - currPos;
      double deltaLen = deltaPos.length() + fastMotionThreshold;
      if (deltaLen > 1.0)
      {
        deltaPos *= deltaLen*deltaLen;
      }

      // Compute new room position based on controller motion
      _roomPose = _motionData._origRoomPose;
      _roomPose.preMultTranslate(deltaPos*_motionData._origWorldUnitsPerMeter);

      break;
    }

    case(SCALE) :
    {
      // Set the WorldUnits/Meter ratio based on how the controllers are moved together/apart
      // Start by getting the controller distance when the action was started
      osg::Vec3d device1TransOrig = _motionData._device1OrigPoseRaw.getTrans();
      osg::Vec3d device2TransOrig = _motionData._device2OrigPoseRaw.getTrans();
      double origDist = (device1TransOrig - device2TransOrig).length();

      // Get the center point between controllers
      osg::Vec3d origCenter = (device1TransOrig + device2TransOrig) * 0.5;

      // Get the current controller distance
      osg::Vec3d device1TransCurr = device1Model->_rawDeviceToWorld.getTrans();
      osg::Vec3d device2TransCurr = device2Model->_rawDeviceToWorld.getTrans();
      osg::Vec3d currCenter = (device1TransCurr + device2TransCurr) * 0.5; // Center point between controllers
      double currDist = (device1TransCurr - device2TransCurr).length();
      double distRatio = origDist / currDist; // controllers apart -> 0, controllers together -> inf

      // Exaggerate large controller motions
      double deltaLen = std::abs(currDist - origDist); // Change in controller distance
      if (deltaLen > 1.0 - fastMotionThreshold)
      {
        distRatio = std::pow(distRatio, deltaLen + fastMotionThreshold);
      }
      distRatio = std::pow(distRatio - 1.0, 3.0) + 1.0;

      // Compute new WorldUnits/Meter ratio based on scaled ratio of controller distances
      double newWorldUnitsPerMeter = _motionData._origWorldUnitsPerMeter * distRatio;
      _ovrDevice->setWorldUnitsPerMeter(newWorldUnitsPerMeter);

      // Account for WorldUnits/Meter ratio hitting limits
      newWorldUnitsPerMeter = _ovrDevice->getWorldUnitsPerMeter();
      distRatio = newWorldUnitsPerMeter / _motionData._origWorldUnitsPerMeter;

      // Compute new pose offset location that keeps the central point at the same world
      // location both before and after the scale.
      // - Expand universe from original center point between controllers
      // - Shrink universe to current center point between controllers
      osg::Vec3d centerPoint;
      if (distRatio < 1.0) centerPoint = origCenter; // Expand universe
      else centerPoint = currCenter; // Shrink universe
      _roomPose = _motionData._origRoomPose;
      _roomPose.preMultTranslate(centerPoint*_motionData._origWorldUnitsPerMeter*(1.0 - distRatio));

      // Rotating the controllers about each other rotates the scene about the controller midpoint
      // This is done by moving the controller midpoint to the origin, then performing the rotation,
      // then moving the controller midpoint back to its original position.
      // Since _roomPose is the Room->Trackball transform, these steps are applied in reverse
      // order using premultiplication

      // Step 3: Move controller midpoint back to its original location
      osg::Vec3d origCenterWU = origCenter * newWorldUnitsPerMeter;
      _roomPose.preMultTranslate(origCenterWU);

      // Step 2: Rotate room about new origin (controller center) based on controller motion
      // Note that the room should rotate in the opposite direction as controllers, since the
      // goal is to make the scene appear to rotate in the same direction as controllers.
      osg::Vec3d origDeviceLine = device2TransOrig - device1TransOrig;
      osg::Vec3d currDeviceLine = device2TransCurr - device1TransCurr;
      osg::Quat q;
      q.makeRotate(currDeviceLine, origDeviceLine);
      _roomPose.preMultRotate(q);

      // Step 1: Move controller midpoint to the room center
      _roomPose.preMultTranslate(-origCenterWU);

      break;
    }

    case(PICK) :
    {
      // Get transform from laser space to controller space
      // This may be non-identity for VR controllers whose ideal pointing direction
      // is not parallel to the controller's x/y/z axes (e.g. Oculus Touch)
      const osg::Matrixd &matLaserToController = device1Model->_laser->getMatrix();

      // Get transform from controller space to room space
      const osg::Matrixd &matControllerToRoom = device1Model->_rawDeviceToWorld;

      // Compute full transform from laser to room space
      osg::Matrixd matLaserToRoom = matLaserToController * matControllerToRoom;

      // Start by getting the controller origin in room space
      // Note that we need this in world units (not meters) since the ground plane is scaled to world units
      osg::Vec3d startPoint = matLaserToRoom.getTrans()*_motionData._origWorldUnitsPerMeter;

      // Compute the line segment along the controller's -Z axis in room space
      osg::Vec3d endPoint = osg::Vec3d(0, 0, -1)*matLaserToRoom*_motionData._origWorldUnitsPerMeter;
      osg::Vec3d rayDir = endPoint - startPoint;

      // Perform the pick operation on the ground plane, which is already in room-space coordinates
      //osgUtil::LineSegmentIntersector* intersector = new osgUtil::LineSegmentIntersector(startPoint, endPoint);
      osgUtil::RayIntersector* intersector = new osgUtil::RayIntersector(startPoint, rayDir);
      osgUtil::IntersectionVisitor iv(intersector);
      _ovrDevice->getGroundPlane()->accept(iv);
      if (intersector->containsIntersections())
      {
        auto intersections = intersector->getIntersections();
        osg::notify(osg::NOTICE) << "Got " << intersections.size() << " intersections:\n";
        for (auto&& intersection : intersections)
        {
          osg::notify(osg::NOTICE) << "  - Local intersection point = " << intersection.localIntersectionPoint << std::endl;
          osg::Vec3d viewPoint = intersection.getWorldIntersectPoint()*_roomPose*osgGA::TrackballManipulator::getMatrix();
          osg::notify(osg::NOTICE) << "  - View  intersection point = " << viewPoint << std::endl;
          osg::Vec3d worldPoint = intersection.getWorldIntersectPoint()*_roomPose*FollowingTrackball::getMatrix();
          osg::notify(osg::NOTICE) << "  - World intersection point = " << worldPoint << std::endl;
        }
      }
      else
      {
        osg::notify(osg::NOTICE) << "No intersections!" << std::endl;
      }

      _motionData._mode = NONE; // Revert to no motion
      break;
    }

    default:
      break;
    }

    // Compute Local <-> World transforms for HMD and other VR devices
    _ovrDevice->computeDeviceTransforms();
  }
  
  /*************************************************************/
  void OpenVRTrackball::saveCurrentMotionData()
  {
    const OpenVRDevice::DeviceModel *device1Model = _ovrDevice->getDeviceModel(_motionData._device1ID);
    _motionData._device1OrigPoseRaw = device1Model->_rawDeviceToWorld;
    
    if (_motionData._device2ID < _ovrDevice->getNumDeviceModels())
    {
      const OpenVRDevice::DeviceModel *device2Model = _ovrDevice->getDeviceModel(_motionData._device2ID);
      _motionData._device2OrigPoseRaw = device2Model->_rawDeviceToWorld;
    }
    
    _motionData._origWorldUnitsPerMeter = _ovrDevice->getWorldUnitsPerMeter();
    _motionData._origRotation = osgGA::TrackballManipulator::getRotation();
    _motionData._origTrackball = _roomPose * osgGA::TrackballManipulator::getMatrix();
    _motionData._origRoomPose = _roomPose;
  }
  
  /*************************************************************/
  OpenVRSwapBuffers::OpenVRSwapBuffers(OpenVRDevice *ovrDevice, VRTextureBuffer *texBuffer)
  : _ovrDevice(ovrDevice), _texBuffer(texBuffer)
  { }
  
  /*************************************************************/
  void OpenVRSwapBuffers::swapBuffersImplementation(osg::GraphicsContext *gc)
  {
    // Get OpenGL texture names
    unsigned int contextID = gc->getState()->getContextID();
    GLuint rightEyeTexName = _texBuffer->_rightColorTex->getTextureObject(contextID)->id();
    GLuint leftEyeTexName = _texBuffer->_leftColorTex->getTextureObject(contextID)->id();
    
    // Submit eye textures to OpenVR
    _ovrDevice->submitFrame(rightEyeTexName, leftEyeTexName);
    
    // Run default swap buffers
    gc->swapBuffersImplementation();

    // Get updated device poses for next frame
    _ovrDevice->waitGetPoses();
  }

  /*************************************************************/
  OpenVRImageHandler::OpenVRImageHandler(const OpenVRDevice *ovrDevice, osg::Image* image) :
    osgViewer::InteractiveImageHandler(image),
    _ovrDevice(ovrDevice)
  {
    _pickData.mode = NONE;
    _laserSelectedColor = osg::Vec4(1, 1, 1, 1);
    _laserSelectedWidth = 4.0;
  }

  /*************************************************************
  * Determine the image location at which a VR controller is pointing,
  * and send a mouse event to the image for further processing.
  *************************************************************/
  void OpenVRImageHandler::processImagePick()
  {
    // Check if image pick processing is needed
    if (_pickData.mode == NONE) return;

    // Pick coordinates on image
    bool validPick = false;
    int x = 0, y = 0;

    const OpenVRDevice::DeviceModel *deviceModel = _ovrDevice->getDeviceModel(_pickData.deviceID);

    // Get transform from laser space to controller space
    // This may be non-identity for VR controllers whose ideal pointing direction
    // is not parallel to the controller's x/y/z axes (e.g. Oculus Touch)
    const osg::Matrixd &matLaserToController = deviceModel->_laser->getMatrix();

    // Get transform from controller space to room space
    const osg::Matrixd &matControllerToRoom = _ovrDevice->getDeviceModel(_pickData.deviceID)->_rawDeviceToWorld;

    // Compute full transform from laser to room space
    osg::Matrixd matLaserToRoom = matLaserToController * matControllerToRoom;

    // Get controller laser start and end points in room space and world units
    osg::Vec3d startPoint = matLaserToRoom.getTrans() * _ovrDevice->getWorldUnitsPerMeter(); // Start point is origin
    osg::Vec3d endPoint = osg::Vec3d(0, 0, -1) * matLaserToRoom * _ovrDevice->getWorldUnitsPerMeter(); // Use unit length since we just want laser direction

    // Transform controller laser to the local coordinate space of the geometry containing the image
    // First determine if the geometry is in world space or room space. Note that room space includes attached to a VR controller
    // Do this by checking if the geometry's NodePath contains the topmost VR device transform
    osg::NodePath::iterator itrDevices = std::find(_pickData.nodePath.begin(), _pickData.nodePath.end(), _ovrDevice->getDeviceRenderModels());

    osg::Matrixd matRoomToGeomLocal; // Final transform from Room to Geometry's local space
    double ratioMetersPerLaserDistance; // Conversion from pick point distance to meters, depends on world vs room/controller space

    // Geometry in room/controller space
    if (itrDevices != _pickData.nodePath.end())
    {
      // Controller laser already in room space, so just get the additional NodePath to the geometry
      if ((itrDevices+1) < (_pickData.nodePath.end()-1))
      {
        osg::NodePath geomNodePath(itrDevices + 1, _pickData.nodePath.end() - 1); // Prune room-space NodePath to geometry
        matRoomToGeomLocal = osg::computeWorldToLocal(geomNodePath);
      }
      // else the topmost VR device transform is the geometry's direct parent and there is nothing more to do

      ratioMetersPerLaserDistance = 1.0;
    }
    else // Geometry in world space
    {
      // Transform to geometry's local space
      osg::Matrixd matRoomToWorld = _pickData.trackball->getRoomToTrackballMatrix() * _pickData.trackball->FollowingTrackball::getMatrix();
      if (_pickData.nodePath.size() > 1)
      {
        osg::NodePath geomNodePath(_pickData.nodePath.begin(), _pickData.nodePath.end() - 1); // Prune world-space NodePath to geometry
        osg::Matrixd matWorldToGeomLocal = osg::computeWorldToLocal(geomNodePath);
        matRoomToGeomLocal = matRoomToWorld * matWorldToGeomLocal;
      }
      else matRoomToGeomLocal = matRoomToWorld;

      ratioMetersPerLaserDistance = 1.0 / _ovrDevice->getWorldUnitsPerMeter();
    }

    // Compute the transformed laser start point and direction
    startPoint = startPoint * matRoomToGeomLocal;
    endPoint = endPoint * matRoomToGeomLocal;
    osg::Vec3d rayDir = endPoint - startPoint;

    // Perform the pick operation
    osg::ref_ptr<osgUtil::RayIntersector> intersector = new osgUtil::RayIntersector(startPoint, rayDir);
    osgUtil::IntersectionVisitor iv(intersector);
    _pickData.nodePath.back()->accept(iv);
    if (intersector->containsIntersections())
    {
      auto intersections = intersector->getIntersections();
      /*
      osg::notify(osg::NOTICE) << "Got " << intersections.size() << " intersections for " << _pickData.nodePath.back()->getName() << std::endl;
      for (auto&& intersection : intersections)
      {
        osg::notify(osg::NOTICE) << "  - Local intersection point = " << intersection.localIntersectionPoint << std::endl;
      }
      */

      osg::Vec2 tc(0.5f, 0.5f); // Intersection tex coord

      // Get nearest intersection
      const osgUtil::RayIntersector::Intersection& intersection = *(intersections.begin());
      osg::Drawable* drawable = intersection.drawable.get();
      osg::Geometry* geometry = drawable ? drawable->asGeometry() : nullptr;
      osg::Vec3Array* vertices = geometry ? dynamic_cast<osg::Vec3Array*>(geometry->getVertexArray()) : nullptr;
      if (vertices)
      {
        // Get vertex indices
        const osgUtil::RayIntersector::Intersection::IndexList& indices = intersection.indexList;
        const osgUtil::RayIntersector::Intersection::RatioList& ratios = intersection.ratioList;

        if (indices.size() == 3 && ratios.size() == 3)
        {
          unsigned int i1 = indices[0];
          unsigned int i2 = indices[1];
          unsigned int i3 = indices[2];

          float r1 = ratios[0];
          float r2 = ratios[1];
          float r3 = ratios[2];

          osg::Array* texcoords = (geometry->getNumTexCoordArrays() > 0) ? geometry->getTexCoordArray(0) : nullptr;
          osg::Vec2Array* texcoords_Vec2Array = dynamic_cast<osg::Vec2Array*>(texcoords);
          if (texcoords_Vec2Array)
          {
            // Compute intersection tex coord
            osg::Vec2 tc1 = (*texcoords_Vec2Array)[i1];
            osg::Vec2 tc2 = (*texcoords_Vec2Array)[i2];
            osg::Vec2 tc3 = (*texcoords_Vec2Array)[i3];
            tc = tc1*r1 + tc2*r2 + tc3*r3;
          }

          // Compute intersection x,y coords on image
          if (_image.valid())
          {
            x = int(float(_image->s()) * tc.x());
            y = int(float(_image->t()) * tc.y());

            deviceModel->_laser->setLength(intersection.distance*ratioMetersPerLaserDistance); // Set laser length
            deviceModel->_laser->setWidth(_laserSelectedWidth);
            deviceModel->_laser->setColor(_laserSelectedColor);

            validPick = true;
          }
        }
      }
    }

    if (!validPick) return;

    // Dispatch the appropriate mouse event to the Image based on the trigger state
    float triggerValue = getTriggerValue(deviceModel->_controllerState);
    if (triggerValue < 0.99) // Trigger partially pressed: mouse move
    {
      _image->sendPointerEvent(x, y, 0);
    }
    else // Trigger fully pressed: left-click
    {
      _image->sendPointerEvent(x, y, osgGA::GUIEventAdapter::LEFT_MOUSE_BUTTON);

    }
  }

  /*************************************************************/
  void OpenVRImageHandler::saveCurrentPickData(PickMode mode, osgViewer::View* view, osg::NodeVisitor* nv, uint32_t deviceID)
  {
    const OpenVRDevice::DeviceModel *device1Model = _ovrDevice->getDeviceModel(deviceID);
    OpenVRTrackball *trackball = dynamic_cast<OpenVRTrackball*>(view->getCameraManipulator());

    if ((device1Model == nullptr) || (trackball == nullptr) || (nv == nullptr) || nv->getNodePath().empty())
    {
      _pickData.mode = NONE;
      _pickData.nodePath.clear();
    }
    else
    {
      _pickData.mode = mode;
      _pickData.deviceID = deviceID;
      _pickData.trackball = trackball;
      _pickData.nodePath = nv->getNodePath();
    }
  }

} // !namespace OpenFrames
