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
    if(_cameraType == LEFT_CAMERA)
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
    // Get Local to Head matrix
    osg::Matrixd hmdPose = _ovrDevice->getHMDPoseMatrix();
    
    // Get World to Local view matrix
    osg::Matrixd matrix = FollowingTrackball::getInverseMatrix();
    
    // Compute World to Head matrix
    matrix.postMult(hmdPose);
    return matrix;
  }

} // !namespace OpenFrames
