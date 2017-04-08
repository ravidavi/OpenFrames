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
#include <osg/Matrixd>

namespace OpenFrames{
  
  /*************************************************************/
  bool OpenVRPoseCallback::run(osg::Object* object, osg::Object* data)
  {
    // Get updated view offset matrices
    // These can change if the user changes the HMD's IPD
    _ovrDevice->updateViewOffsets();
    
    // Get updated poses for all devices
    _ovrDevice->waitGetPoses();
    
    // Continue traversing if needed
    return traverse(object, data);
  }

  /*************************************************************/
  void OpenVRSlaveCallback::updateSlave(osg::View& view, osg::View::Slave& slave)
  {
    // Get the HMD->Eye offset matrix
    osg::Matrixd viewOffset;
    if(_cameraType == RIGHT_CAMERA)
    {
      viewOffset = _ovrDevice->getRightEyeViewOffsetMatrix();
    }
    else if(_cameraType == LEFT_CAMERA)
    {
      viewOffset = _ovrDevice->getLeftEyeViewOffsetMatrix();
    }
    else
    {
      viewOffset = _ovrDevice->getCenterViewOffsetMatrix();
    }
    
    // Get the World->HMD view matrix
    osg::Camera *masterCamera = view.getCamera();
    osg::Matrixd viewMatrix = masterCamera->getViewMatrix();
    
    // Compute and set the World->Eye view matrix
    viewMatrix.postMult(viewOffset);
    slave._camera->setViewMatrix(viewMatrix);
    
    // Call parent update slave implementation
    slave.updateSlaveImplementation(view);
  }
  
  /*************************************************************/
  osg::Matrixd OpenVRTrackball::getInverseMatrix() const
  {
    // Get World to Local view matrix
    osg::Matrixd matWorldToLocal = FollowingTrackball::getInverseMatrix();
    
    // OpenVR device models exist in local space, not in world space, so premult their
    // transform by the inverse of the World->Local matrix. We do this by setting
    // the render model Camera's view matrix as the inverse, and telling it to
    // use the pre-multiply transform order (see OpenVRDevice ctor)
    osg::Matrixd matLocalToWorld;
    matLocalToWorld.invert(matWorldToLocal);
    _ovrDevice->_deviceModels->setViewMatrix(matLocalToWorld);
    
    // Get Local to Head matrix
    osg::Matrixd hmdPose = _ovrDevice->getHMDPoseMatrix();
    
    // Compute World to Head matrix, which will be returned
    matWorldToLocal.postMult(hmdPose);
    return matWorldToLocal;
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
  void OpenVRTrackball::updateCamera(osg::Camera& camera)
  {
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
        osg::Matrixd device1CurrPose = _ovrDevice->_deviceIDToModel[_motionData._device1ID]._modelTransform->getMatrix();
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
        _motionData._origTrackball = osgGA::TrackballManipulator::getMatrix();
        _motionData._origRotation = newRotation;
        
        break;
      }
        
      case(TRANSLATE) :
      {
        // Set the device pose offset based on controller location
        // Start by getting the controller location when the button was pressed
        osg::Vec3d origPos = _motionData._device1OrigPoseRaw.getTrans();
        
        // Next get the current controller location
        osg::Vec3d currPos = _ovrDevice->_deviceIDToModel[_motionData._device1ID]._rawDeviceToWorld.getTrans();
        
        // Move pose offset in the opposite direction as controller motion.
        osg::Vec3d deltaPos = origPos - currPos;
        double deltaLen = deltaPos.length() + fastMotionThreshold;
        if (deltaLen > 1.0)
        {
          deltaPos *= deltaLen*deltaLen;
        }
        
        // Compute new pose offset based on controller motion
        _ovrDevice->_poseOffsetRaw = _motionData._origPoseOffsetRaw + deltaPos;
        
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
        device1Trans = _ovrDevice->_deviceIDToModel[_motionData._device1ID]._rawDeviceToWorld.getTrans();
        device2Trans = _ovrDevice->_deviceIDToModel[_motionData._device2ID]._rawDeviceToWorld.getTrans();
        osg::Vec3d currCenter = (device1Trans + device2Trans) * 0.5; // Center point between controllers
        double currDist = (device1Trans - device2Trans).length();
        double deltaLen = std::abs(currDist - origDist); // Change in controller distance
        
        // Controller position has jitter, so ignore small changes
        double distRatio;
        if (deltaLen < 0.02) distRatio = 1.0;
        else distRatio = origDist / currDist; // controllers apart -> 0, controllers together -> inf
        
        // Exaggerate large controller motions
        if (deltaLen > 1.0 - fastMotionThreshold)
        {
          distRatio = std::pow(distRatio, deltaLen + fastMotionThreshold);
        }
        distRatio = std::pow(distRatio - 1.0, 3.0) + 1.0;
        
        // Compute new WorldUnits/Meter ratio based on scaled ratio of controller distances
        double newWorldUnitsPerMeter = _motionData._origWorldUnitsPerMeter * distRatio;
        _ovrDevice->setWorldUnitsPerMeter(newWorldUnitsPerMeter);
        
        // Compute new pose offset location that keeps the central point at the same world
        // location both before and after the scale.
        // Expand universe from point between controllers when motion was started
        // Shrink universe to current point between controllers
        osg::Vec3d centerPoint;
        if (distRatio < 1.0) centerPoint = origCenter; // Expand universe
        else centerPoint = currCenter; // Shrink universe
        _ovrDevice->_poseOffsetRaw = (centerPoint*(1.0 - distRatio) + _motionData._origPoseOffsetRaw) / distRatio;
        
        break;
      }
        
      default:
        break;
    }
    
    camera.setViewMatrix(getInverseMatrix());
  }
  
  /*************************************************************/
  void OpenVRTrackball::saveCurrentMotionData()
  {
    _motionData._device1OrigPoseRaw = _ovrDevice->_deviceIDToModel[_motionData._device1ID]._rawDeviceToWorld;
    _motionData._device1OrigPose = _ovrDevice->_deviceIDToModel[_motionData._device1ID]._modelTransform->getMatrix();
    
    if (_motionData._device2ID < _ovrDevice->_deviceIDToModel.size())
    {
      _motionData._device2OrigPoseRaw = _ovrDevice->_deviceIDToModel[_motionData._device2ID]._rawDeviceToWorld;
      _motionData._device2OrigPose = _ovrDevice->_deviceIDToModel[_motionData._device2ID]._modelTransform->getMatrix();
    }
    
    _motionData._origWorldUnitsPerMeter = _ovrDevice->getWorldUnitsPerMeter();
    _motionData._origCenter = osgGA::TrackballManipulator::getCenter();
    _motionData._origRotation = osgGA::TrackballManipulator::getRotation();
    _motionData._origDistance = osgGA::TrackballManipulator::getDistance();
    _motionData._origTrackball = osgGA::TrackballManipulator::getMatrix();
    _motionData._origPoseOffsetRaw = _ovrDevice->_poseOffsetRaw;
  }

} // !namespace OpenFrames
