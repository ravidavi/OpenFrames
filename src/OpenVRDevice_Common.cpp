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
#include <OpenFrames/VRUtils.hpp>

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
    : _worldUnitsPerMeter(worldUnitsPerMeter),
    _minWorldUnitsPerMeter(0.0),
    _maxWorldUnitsPerMeter(DBL_MAX),
    _userHeight(userHeight),
    _width(0),
    _height(0),
    _isInitialized(false),
    _vrSystem(nullptr),
    _vrRenderModels(nullptr),
    _ipd(-1.0)
  {
    // Set up a transform for the device render models
    // These models exist in local space (the room), so their view matrix should only
    // include the HMD transform. The OpenVRPoseCallback will set this matrix at every frame.
    _deviceModels = new osg::MatrixTransform();
    _deviceModels->setCullCallback(new OpenVRDeviceTransformCallback(this));
    _deviceModels->setReferenceFrame(osg::Transform::ABSOLUTE_RF); // Override view matrix
    _deviceModels->setName("OpenVR Device Models");

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
  void OpenVRDevice::computeDeviceTransforms()
  {
    osg::Matrixd matDeviceToWorld; // Device to World transform
    osg::Matrix matWorldToDevice; // For HMD
    
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
        // Non-HMD devices need their models scaled to world units
        matDeviceToWorld.preMultScale(osg::Vec3d(_worldUnitsPerMeter, _worldUnitsPerMeter, _worldUnitsPerMeter));
        
        // Set device model's transform from its adjusted pose in world units
        _deviceIDToModel[i]._modelTransform->setMatrix(matDeviceToWorld);
      }
    }
  }
  
  /*************************************************************/
  void OpenVRDevice::computeViewOffsets(const osg::Vec3d& rightEyeRaw, const osg::Vec3d& leftEyeRaw)
  {
    // Get right eye view offset vector (Eye to Head transform)
    _rightEyeViewOffsetRaw = rightEyeRaw;
    
    // Get left eye view offset vector (Eye to Head transform)
    _leftEyeViewOffsetRaw = leftEyeRaw;
    
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
    
    // Scale offsets according to WorldUnit/Meter scale
    // Note that the vector direction is flipped since we want the Head to Eye transform for OSG
    _rightEyeViewOffset = _rightEyeViewOffsetRaw*(-_worldUnitsPerMeter);
    _leftEyeViewOffset = _leftEyeViewOffsetRaw*(-_worldUnitsPerMeter);
    _centerViewOffset = _centerViewOffsetRaw*(-_worldUnitsPerMeter);
  }
  
  /*************************************************************/
  bool OpenVRPoseCallback::run(osg::Object* object, osg::Object* data)
  {
    // Get updated view offset matrices
    // These can change if the user changes the HMD's IPD
    _ovrDevice->updateViewOffsets();
    
    // Get updated poses for all devices
    _ovrDevice->waitGetPoses();

    // Set Room->HMDCenter matrix as VR device transform
    osg::Matrixd matWorldToLocal = _ovrDevice->getHMDPoseMatrix();
    matWorldToLocal.postMultTranslate(_ovrDevice->getCenterViewOffset());
    _ovrDevice->getDeviceRenderModels()->setMatrix(matWorldToLocal);
    
    // Continue traversing if needed
    return traverse(object, data);
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
    // The motion mode defines how controllers change the scene in response
    // to user inputs. Start with no motion.
    _motionData._mode = NONE;
    _motionData._prevMode = ROTATE; // Initial button press will go to Rotate mode
    _motionData._prevTime = 0.0;
  }
  
  /*************************************************************/
  osg::Matrixd OpenVRTrackball::getInverseMatrix() const
  {
    // Get World->Local (trackball space) view matrix
    osg::Matrixd matWorldToLocal = FollowingTrackball::getInverseMatrix();
    
    // Translate from trackball to room origin
    matWorldToLocal.postMultTranslate(-_roomOffset);
    
    // Include Room->HMD transform
    matWorldToLocal.postMult(_ovrDevice->getHMDPoseMatrix());
    
    // Append HMDCenter view offset vector (midpoint of right/left eyes)
    matWorldToLocal.postMultTranslate(_ovrDevice->getCenterViewOffset());
    
    // Return final World->HMDCenter matrix
    return matWorldToLocal;
  }
  
  /*************************************************************/
  void OpenVRTrackball::updateCamera(osg::Camera& camera)
  {
    camera.setViewMatrix(getInverseMatrix());

    // Exaggerate controller motion beyond a predetermined distance threshold so that
    // the view can be moved faster with larger controller motions.
    // Note that threshold distance depends on arm length, which depends on height.
    // Height/Armspan = 1.0 and ShoulderWidth/Height = 0.3 (see Golden Ratio)
    double armLength = (_ovrDevice->getUserHeight()*0.7) / 2.0; // [meters]
    double fastMotionThreshold = 1.0 - armLength / 4.0;
    
    // Handle world transformations based on current motion mode
    switch (_motionData._mode)
    {
      case(ROTATE) :
      {
        // Set the trackball rotation and distance based on controller motion
        // Start by getting the controller location when the button was pressed
        osg::Vec3d origPos = _motionData._device1OrigPose.getTrans();
        
        // Convert to position relative to trackball center
        osg::Vec3d origPosWorld = origPos * _motionData._origTrackball;
        
        // Next get the current controller location relative to trackball center
        const OpenVRDevice::DeviceModel *device1Model = _ovrDevice->getDeviceModel(_motionData._device1ID);
        osg::Matrixd device1CurrPose = device1Model->_modelTransform->getMatrix();
        osg::Vec3d currPos = device1CurrPose.getTrans();
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
        _motionData._device1OrigPose = device1CurrPose;
        getTrackballRoomToWorldMatrix(_motionData._origTrackball);
        _motionData._origRotation = newRotation;
        
        break;
      }
        
      case(TRANSLATE) :
      {
        // Set the device pose offset based on controller location
        // Start by getting the controller location when the button was pressed
        osg::Vec3d origPos = _motionData._device1OrigPoseRaw.getTrans();
        
        // Next get the current controller location
        const OpenVRDevice::DeviceModel *device1Model = _ovrDevice->getDeviceModel(_motionData._device1ID);
        osg::Vec3d currPos = device1Model->_rawDeviceToWorld.getTrans();
        
        // Move pose offset in the opposite direction as controller motion.
        osg::Vec3d deltaPos = origPos - currPos;
        double deltaLen = deltaPos.length() + fastMotionThreshold;
        if (deltaLen > 1.0)
        {
          deltaPos *= deltaLen*deltaLen;
        }
        
        // Compute new pose offset based on controller motion
        _roomOffset = _motionData._origRoomOffset + deltaPos*_motionData._origWorldUnitsPerMeter;
        
        break;
      }
        
      case(SCALE) :
      {
        // Set the WorldUnits/Meter ratio based on how the controllers are moved together/apart
        // Start by getting the controller distance when the action was started
        osg::Vec3d device1Trans = _motionData._device1OrigPoseRaw.getTrans();
        osg::Vec3d device2Trans = _motionData._device2OrigPoseRaw.getTrans();
        double origDist = (device1Trans - device2Trans).length();
        
        // Get the center point between controllers
        osg::Vec3d origCenter = (device1Trans + device2Trans) * 0.5;
        
        // Get the current controller distance
        const OpenVRDevice::DeviceModel *device1Model = _ovrDevice->getDeviceModel(_motionData._device1ID);
        const OpenVRDevice::DeviceModel *device2Model = _ovrDevice->getDeviceModel(_motionData._device2ID);
        device1Trans = device1Model->_rawDeviceToWorld.getTrans();
        device2Trans = device2Model->_rawDeviceToWorld.getTrans();
        osg::Vec3d currCenter = (device1Trans + device2Trans) * 0.5; // Center point between controllers
        double currDist = (device1Trans - device2Trans).length();
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
        // Expand universe from point between controllers when motion was started
        // Shrink universe to current point between controllers
        osg::Vec3d centerPoint;
        if (distRatio < 1.0) centerPoint = origCenter; // Expand universe
        else centerPoint = currCenter; // Shrink universe
        _roomOffset = _motionData._origRoomOffset + centerPoint*(_motionData._origWorldUnitsPerMeter*(1.0 - distRatio));
        
        break;
      }
        
      default:
        break;
    }    
  }
  
  /*************************************************************/
  void OpenVRTrackball::getTrackballRoomToWorldMatrix(osg::Matrixd& matrix)
  {
    // Translate from room origin to trackball origin
    matrix.makeTranslate(_roomOffset);
    
    // Transform from trackball space to view space
    matrix.postMult(osgGA::TrackballManipulator::getMatrix());
  }
  
  /*************************************************************/
  void OpenVRTrackball::saveCurrentMotionData()
  {
    const OpenVRDevice::DeviceModel *device1Model = _ovrDevice->getDeviceModel(_motionData._device1ID);
    _motionData._device1OrigPoseRaw = device1Model->_rawDeviceToWorld;
    _motionData._device1OrigPose = device1Model->_modelTransform->getMatrix();
    
    if (_motionData._device2ID < _ovrDevice->getNumDeviceModels())
    {
      const OpenVRDevice::DeviceModel *device2Model = _ovrDevice->getDeviceModel(_motionData._device2ID);
      _motionData._device2OrigPoseRaw = device2Model->_rawDeviceToWorld;
      _motionData._device2OrigPose = device2Model->_modelTransform->getMatrix();
    }
    
    _motionData._origWorldUnitsPerMeter = _ovrDevice->getWorldUnitsPerMeter();
    _motionData._origRotation = osgGA::TrackballManipulator::getRotation();
    getTrackballRoomToWorldMatrix(_motionData._origTrackball);
    _motionData._origRoomOffset = _roomOffset;
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
  }

} // !namespace OpenFrames
