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

#include <OpenFrames/VRUtils.hpp>

#include <iostream>

namespace OpenFrames{
  
  VRTextureBuffer::VRTextureBuffer(int width, int height)
  {
    // Set texture size if specified
    bool setSize = true;
    if((width <= 0) || (height <= 0))
    {
      setSize = false;
    }
    
    // Set up right eye color buffer
    _rightColorTex = new osg::Texture2D();
    _rightColorTex->setSourceFormat(GL_RGBA);
    _rightColorTex->setInternalFormat(GL_RGBA8);
    _rightColorTex->setFilter(osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR);
    _rightColorTex->setFilter(osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR);
    _rightColorTex->setWrap(osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE);
    _rightColorTex->setWrap(osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE);
    if(setSize) _rightColorTex->setTextureSize(width, height);
    
    // Set up right eye depth buffer
    _rightDepthTex = new osg::Texture2D();
    _rightDepthTex->setSourceFormat(GL_DEPTH_COMPONENT);
    _rightDepthTex->setSourceType(GL_UNSIGNED_INT);
    _rightDepthTex->setInternalFormat(GL_DEPTH_COMPONENT24);
    _rightDepthTex->setBorderWidth(0);
    _rightDepthTex->setFilter(osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR);
    _rightDepthTex->setFilter(osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR);
    _rightDepthTex->setWrap(osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE);
    _rightDepthTex->setWrap(osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE);
    if(setSize) _rightDepthTex->setTextureSize(width, height);
    
    // Set up left eye color buffer
    _leftColorTex = new osg::Texture2D();
    _leftColorTex->setSourceFormat(GL_RGBA);
    _leftColorTex->setInternalFormat(GL_RGBA8);
    _leftColorTex->setFilter(osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR);
    _leftColorTex->setFilter(osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR);
    _leftColorTex->setWrap(osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE);
    _leftColorTex->setWrap(osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE);
    if(setSize) _leftColorTex->setTextureSize(width, height);
    
    // Set up left eye depth buffer
    _leftDepthTex = new osg::Texture2D();
    _leftDepthTex->setSourceFormat(GL_DEPTH_COMPONENT);
    _leftDepthTex->setSourceType(GL_UNSIGNED_INT);
    _leftDepthTex->setInternalFormat(GL_DEPTH_COMPONENT24);
    _leftDepthTex->setBorderWidth(0);
    _leftDepthTex->setFilter(osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR);
    _leftDepthTex->setFilter(osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR);
    _leftDepthTex->setWrap(osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE);
    _leftDepthTex->setWrap(osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE);
    if(setSize) _leftDepthTex->setTextureSize(width, height);
  }
  
  VRTextureBuffer::~VRTextureBuffer() {}
  
  VRCamera::VRCamera(VRTextureBuffer *texBuffer, int camNum, StereoMode mode)
  : _texBuffer(texBuffer),
  _mode(mode)
  {
    // Create camera
    _rightCamera = new osg::Camera();
    _leftCamera = new osg::Camera();
    _monoCamera = new osg::Camera();
    
    // Set camera name
    _rightCamera->setName("VRRight" + std::to_string(camNum));
    _leftCamera->setName("VRLeft" + std::to_string(camNum));
    _monoCamera->setName("VRMono" + std::to_string(camNum));
    
    // Don't allow cameras to be culled out
    _rightCamera->setCullingActive(false);
    _leftCamera->setCullingActive(false);
    _monoCamera->setCullingActive(false);
    
    // Don't cull small features
    _rightCamera->setCullingMode(osg::CullSettings::ENABLE_ALL_CULLING & ~osg::CullSettings::SMALL_FEATURE_CULLING);
    _leftCamera->setCullingMode(osg::CullSettings::ENABLE_ALL_CULLING & ~osg::CullSettings::SMALL_FEATURE_CULLING);
    _monoCamera->setCullingMode(osg::CullSettings::ENABLE_ALL_CULLING & ~osg::CullSettings::SMALL_FEATURE_CULLING);
    
    // Use absolute transform
    _rightCamera->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
    _leftCamera->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
    _monoCamera->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
    
    // Don't capture events if user clicks on camera viewport
    _rightCamera->setAllowEventFocus(false);
    _leftCamera->setAllowEventFocus(false);
    _monoCamera->setAllowEventFocus(false);
    
    // Attach right eye color/depth buffers
    _rightCamera->setRenderTargetImplementation(osg::Camera::FRAME_BUFFER_OBJECT);
    _rightCamera->attach(osg::Camera::COLOR_BUFFER0, _texBuffer->_rightColorTex);
    _rightCamera->attach(osg::Camera::DEPTH_BUFFER, _texBuffer->_rightDepthTex);
    
    // Attach left eye color/depth buffers
    _leftCamera->setRenderTargetImplementation(osg::Camera::FRAME_BUFFER_OBJECT);
    _leftCamera->attach(osg::Camera::COLOR_BUFFER0, _texBuffer->_leftColorTex);
    _leftCamera->attach(osg::Camera::DEPTH_BUFFER, _texBuffer->_leftDepthTex);
    
    // Attach mono eye color/depth buffers
    _monoCamera->setRenderTargetImplementation(osg::Camera::FRAME_BUFFER_OBJECT);
    _monoCamera->attach(osg::Camera::COLOR_BUFFER0, _texBuffer->_rightColorTex);
    _monoCamera->attach(osg::Camera::COLOR_BUFFER1, _texBuffer->_leftColorTex);
    _monoCamera->attach(osg::Camera::DEPTH_BUFFER, _texBuffer->_rightDepthTex);
    
    // Set viewports
    int w = _texBuffer->_rightColorTex->getTextureWidth();
    int h = _texBuffer->_rightColorTex->getTextureHeight();
    _rightCamera->setViewport(0, 0, w, h);
    _leftCamera->setViewport(0, 0, w, h);
    _monoCamera->setViewport(0, 0, w, h);
    
    // Disable unneeded cameras
    if(_mode == MONO)
    {
      _rightCamera->setNodeMask(0x0);
      _leftCamera->setNodeMask(0x0);
    }
    else if(_mode == STEREO)
    {
      _monoCamera->setNodeMask(0x0);
    }
  }
  
  VRCamera::~VRCamera() {}
  
  unsigned int VRCamera::getNumCameras()
  {
    if(_mode == MONO) return 1;
    else if(_mode == STEREO) return 2;
    else return 3;
  }
  
  osg::Camera* VRCamera::getCamera(unsigned int camNum)
  {
    if(_mode == MONO)
    {
      if(camNum == 0) return _monoCamera;
      else return NULL;
    }
    else if(_mode == STEREO)
    {
      if(camNum == 0) return _rightCamera;
      else if(camNum == 1) return _leftCamera;
      else return NULL;
    }
    else
    {
      if(camNum == 0) return _rightCamera;
      else if(camNum == 1) return _leftCamera;
      else if(camNum == 2) return _monoCamera;
      else return NULL;
    }
  }
  
  void VRCamera::setProjectionMatrix(osg::Matrixd& projmat, double &zNear)
  {
    if(_mode == MONO)
    {
      _monoCamera->setProjectionMatrix(projmat);
    }
    else if(_mode == STEREO)
    {
      _rightCamera->setProjectionMatrix(projmat);
      _leftCamera->setProjectionMatrix(projmat);
    }
    else
    {
      if(zNear > 0.1) // Near plane > 100m means no stereo rendering
      {
        _monoCamera->setNodeMask(0xffffffff); // Enable mono camera
        _monoCamera->setProjectionMatrix(projmat);
        
        _rightCamera->setNodeMask(0x0); // Disable stereo cameras
        _leftCamera->setNodeMask(0x0);
      }
      else // Near plane <= 100m means stereo rendering
      {
        _rightCamera->setNodeMask(0xffffffff); // Enable stereo cameras
        _leftCamera->setNodeMask(0xffffffff);
        _rightCamera->setProjectionMatrix(projmat);
        _leftCamera->setProjectionMatrix(projmat);
        
        _monoCamera->setNodeMask(0x0); // Disable mono camera
      }
    }
  }
  
  void VRCamera::setViewMatrix(osg::Matrixd& viewmat)
  {
    for(unsigned int i = 0; i < getNumCameras(); ++i)
    {
      getCamera(i)->setViewMatrix(viewmat);
    }
  }
  
  void VRCamera::disableCameras()
  {
    for(unsigned int i = 0; i < getNumCameras(); ++i)
    {
      getCamera(i)->setNodeMask(0x0);
    }
  }
  
  VRCameraManager::VRCameraManager(VRTextureBuffer *texBuffer)
  : _texBuffer(texBuffer)
  {}
  
  VRCameraManager::~VRCameraManager()
  {
    reset();
  }
  
  // Create a new VRCamera if needed, and add it as a slave
  void VRCameraManager::enableCamera(unsigned int camNum,
                                     osg::GraphicsContext* gc,
                                     osg::Camera* masterCamera,
                                     double &zNear, double &zFar)
  {
    if(_vrCameraList.size() <= camNum) _vrCameraList.resize(camNum+1);
    VRCamera *vrcam = _vrCameraList[camNum].get();
    
    if(!vrcam) // Create a new VRCamera
    {
      vrcam = new VRCamera(_texBuffer, camNum, VRCamera::AUTO);
      
      osg::Camera *cam;
      for(unsigned int i = 0; i < vrcam->getNumCameras(); ++i)
      {
        cam = vrcam->getCamera(i);
        cam->setGraphicsContext(gc);
        cam->setRenderOrder(masterCamera->getRenderOrder(), camNum);
        cam->setComputeNearFarMode(osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR);
        if(camNum == 0 && _clearColorBuffer)
          cam->setClearMask(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        else
          cam->setClearMask(GL_DEPTH_BUFFER_BIT);
        
        masterCamera->getView()->addSlave(cam, true);
      }
      
      _vrCameraList[camNum] = vrcam;
    }
    
    // Update projection matrix depth planes
    osg::Matrixd projmat = masterCamera->getProjectionMatrix();
    updateProjectionMatrix(projmat, zNear, zFar);
    
    // Set camera rendering matrices
    vrcam->setProjectionMatrix(projmat, zNear); // This enables cameras as needed
    vrcam->setViewMatrix(masterCamera->getViewMatrix());
  }
  
  void VRCameraManager::disableCameras(unsigned int start)
  {
    for(int i = start; i < _vrCameraList.size(); ++i)
    {
      _vrCameraList[i]->disableCameras();
    }
  }
  
  void VRCameraManager::reset()
  {
    for(int i = 0; i < _vrCameraList.size(); ++i)
    {
      VRCamera *vrcam = _vrCameraList[i];
      for(int j = 0; j < vrcam->getNumCameras(); ++j)
      {
        osg::View *view = vrcam->getCamera(j)->getView();
        if(view)
        {
          unsigned int pos = view->findSlaveIndexForCamera(vrcam->getCamera(j));
          view->removeSlave(pos);
        }
      }
    }
    
    _vrCameraList.clear();
  }
  
  void VRCameraManager::setClearColorBuffer(bool clear)
  {
    // Call parent method
    CameraManager::setClearColorBuffer(clear);
    
    // Set first camera's clear color buffer setting if needed
    if(_vrCameraList.size() > 0)
    {
      VRCamera *vrcam = _vrCameraList[0];
      osg::Camera *cam;
      for(unsigned int i = 0; i < vrcam->getNumCameras(); ++i)
      {
        cam = vrcam->getCamera(i);
        if(_clearColorBuffer)
          cam->setClearMask(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        else
          cam->setClearMask(GL_DEPTH_BUFFER_BIT);
      }
    }
    
  }
  
} // !namespace OpenFrames
