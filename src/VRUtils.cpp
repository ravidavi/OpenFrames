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

static const std::string vrCamNamePrefix("VRCam");

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
    
    // Set up textured quad that will draw each color texture
    osg::Geometry* geom = osg::createTexturedQuadGeometry(osg::Vec3(), osg::Vec3(1, 0, 0), osg::Vec3(0, 1, 0));
    osg::Geode *quad = new osg::Geode;
    quad->addDrawable(geom);
    
    // Create camera that will render the right textured quad
    _rightTexCamera = new osg::Camera();
    _rightTexCamera->addChild(quad);
    _rightTexCamera->setReferenceFrame(osg::Transform::ABSOLUTE_RF); // Use our own view/projection matrix
    _rightTexCamera->setProjectionMatrixAsOrtho2D(0, 1, 0, 1); // Same as textured quad's bounds
    _rightTexCamera->setRenderOrder(osg::Camera::NESTED_RENDER); // Render within parent Camera's render stage
    _rightTexCamera->setComputeNearFarMode(osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR);
    osg::StateSet *ss = _rightTexCamera->getOrCreateStateSet();
    ss->setRenderBinDetails(-100, "RenderBin"); // Render before rest of parent Camera's scene
    ss->setMode(GL_LIGHTING, osg::StateAttribute::OFF); // Don't need lighting to copy a texture
    ss->setMode(GL_DEPTH_TEST, osg::StateAttribute::OFF); // Don't want depth test to clip out texture
    ss->setTextureAttributeAndModes(0, _rightColorTex, osg::StateAttribute::ON); // Bind colorTex

    
    // Create camera that will render the left textured quad
    _leftTexCamera = new osg::Camera();
    _leftTexCamera->addChild(quad);
    _leftTexCamera->setReferenceFrame(osg::Transform::ABSOLUTE_RF); // Use our own view/projection matrix
    _leftTexCamera->setProjectionMatrixAsOrtho2D(0, 1, 0, 1); // Same as textured quad's bounds
    _leftTexCamera->setRenderOrder(osg::Camera::NESTED_RENDER); // Render within parent Camera's render stage
    _leftTexCamera->setComputeNearFarMode(osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR);
    ss = _leftTexCamera->getOrCreateStateSet();
    ss->setRenderBinDetails(-100, "RenderBin"); // Render before rest of parent Camera's scene
    ss->setMode(GL_LIGHTING, osg::StateAttribute::OFF); // Don't need lighting to copy a texture
    ss->setMode(GL_DEPTH_TEST, osg::StateAttribute::OFF); // Don't want depth test to clip out texture
    ss->setTextureAttributeAndModes(0, _leftColorTex, osg::StateAttribute::ON); // Bind colorTex
  }
  
  VRTextureBuffer::~VRTextureBuffer() {}
  
  VRCamera::VRCamera(VRTextureBuffer *texBuffer, int camNum, StereoMode mode, bool useMSAA)
  : _texBuffer(texBuffer),
  _mode(mode),
  _useMSAA(useMSAA)
  {
    // Create camera
    _rightCamera = new osg::Camera();
    _leftCamera = new osg::Camera();
    _monoCamera = new osg::Camera();
    
    // Set camera name
    _rightCamera->setName(vrCamNamePrefix + std::to_string(camNum) + "Right");
    _leftCamera->setName(vrCamNamePrefix + std::to_string(camNum) + "Left");
    _monoCamera->setName(vrCamNamePrefix + std::to_string(camNum) + "Mono");
    
    // Don't allow cameras to be culled out
    _rightCamera->setCullingActive(false);
    _leftCamera->setCullingActive(false);
    _monoCamera->setCullingActive(false);
    
    // Don't capture events if user clicks on camera viewport
    _rightCamera->setAllowEventFocus(false);
    _leftCamera->setAllowEventFocus(false);
    _monoCamera->setAllowEventFocus(false);
    
    // Setup MSAA/CSAA parameters
    int samples = 0, colorSamples = 0;
    if(_useMSAA)
    {
      samples = 4;
      colorSamples = 4;
    }
    
    // Attach right eye color/depth buffers
    _rightCamera->setRenderTargetImplementation(osg::Camera::FRAME_BUFFER_OBJECT);
    _rightCamera->attach(osg::Camera::COLOR_BUFFER0, _texBuffer->_rightColorTex, 0, 0, false, samples, colorSamples);
    if(_useMSAA)
    {
      // With MSAA, we can save memory by not creating a resolve depth buffer. So don't
      // attach one, and tell OSG to not create one. It will still create a render
      // depth buffer since we don't change the render mask.
      _rightCamera->setImplicitBufferAttachmentResolveMask(0);
    }
    else
    {
      // Without MSAA, we must attach depth buffer
      _rightCamera->attach(osg::Camera::DEPTH_BUFFER, _texBuffer->_rightDepthTex);
    }
    
    // Attach left eye color/depth buffers
    _leftCamera->setRenderTargetImplementation(osg::Camera::FRAME_BUFFER_OBJECT);
    _leftCamera->attach(osg::Camera::COLOR_BUFFER0, _texBuffer->_leftColorTex, 0, 0, false, samples, colorSamples);
    if(_useMSAA)
      _leftCamera->setImplicitBufferAttachmentResolveMask(0);
    else
      _leftCamera->attach(osg::Camera::DEPTH_BUFFER, _texBuffer->_leftDepthTex);
    
    // Attach mono eye color/depth buffers
    _monoCamera->setRenderTargetImplementation(osg::Camera::FRAME_BUFFER_OBJECT);
    _monoCamera->attach(osg::Camera::COLOR_BUFFER0, _texBuffer->_rightColorTex, 0, 0, false, samples, colorSamples);
    _monoCamera->attach(osg::Camera::COLOR_BUFFER1, _texBuffer->_leftColorTex, 0, 0, false, samples, colorSamples);
    if(_useMSAA)
      _monoCamera->setImplicitBufferAttachmentResolveMask(0);
    else
      _monoCamera->attach(osg::Camera::DEPTH_BUFFER, _texBuffer->_rightDepthTex);
    
    // Set viewports assuming all textures are same size
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
  
  // Get camera at given position
  osg::Camera* VRCamera::getCamera(unsigned int pos)
  {
    if(_mode == MONO)
    {
      if(pos == 0) return _monoCamera;
      else return NULL;
    }
    else if(_mode == STEREO)
    {
      if(pos == 0) return _rightCamera;
      else if(pos == 1) return _leftCamera;
      else return NULL;
    }
    else
    {
      if(pos == 0) return _rightCamera;
      else if(pos == 1) return _leftCamera;
      else if(pos == 2) return _monoCamera;
      else return NULL;
    }
  }
  
  // Disable each camera
  void VRCamera::disableCameras()
  {
    for(unsigned int i = 0; i < getNumCameras(); ++i)
    {
      getCamera(i)->setNodeMask(0x0);
    }
  }
  
  // Set whether the color buffer is cleared by this VRCamera
  void VRCamera::setClearColorBuffer(bool clear)
  {
    if(clear)
    {
      _monoCamera->setClearMask(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      _rightCamera->setClearMask(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      _leftCamera->setClearMask(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    }
    else
    {
      _monoCamera->setClearMask(GL_DEPTH_BUFFER_BIT);
      _rightCamera->setClearMask(GL_DEPTH_BUFFER_BIT);
      _leftCamera->setClearMask(GL_DEPTH_BUFFER_BIT);
    }
  }
  
  void VRCamera::updateCameras(osg::Matrixd& rightView, osg::Matrixd& leftView,
                               osg::Matrixd& centerView, osg::Matrixd& rightProj,
                               osg::Matrixd& leftProj, osg::Matrixd& centerProj,
                               const double &zNear)
  {
    // If MSAA is enabled and we don't need to clear the color buffer, then
    // use the MSAA camera to chain the existing color texture
    bool useMSAACam = _useMSAA && !getClearColorBuffer();
    
    if((_mode == MONO) || ((_mode == AUTO) && (zNear > 0.1)))
    {
      _monoCamera->setNodeMask(0xffffffff); // Enable mono camera
      _monoCamera->setViewMatrix(centerView);
      _monoCamera->setProjectionMatrix(centerProj);
      
      // Add MSAA texture chaining camera to mono camera
      if(useMSAACam && !_monoCamera->containsNode(_texBuffer->_rightTexCamera))
        _monoCamera->addChild(_texBuffer->_rightTexCamera);
      
      _rightCamera->setNodeMask(0x0); // Disable stereo cameras
      _leftCamera->setNodeMask(0x0);
    }
    else if(_mode == STEREO || ((_mode == AUTO) && (zNear <= 0.1)))
    {
      _rightCamera->setNodeMask(0xffffffff); // Enable right camera
      _rightCamera->setViewMatrix(rightView);
      _rightCamera->setProjectionMatrix(rightProj);
      
      // Add MSAA texture chaining camera to right camera
      if(useMSAACam && !_rightCamera->containsNode(_texBuffer->_rightTexCamera))
        _rightCamera->addChild(_texBuffer->_rightTexCamera);
      
      _leftCamera->setNodeMask(0xffffffff); // Enable left camera
      _leftCamera->setViewMatrix(leftView);
      _leftCamera->setProjectionMatrix(leftProj);
      
      // Add MSAA texture chaining camera to left camera
      if(useMSAACam && !_leftCamera->containsNode(_texBuffer->_leftTexCamera))
        _leftCamera->addChild(_texBuffer->_leftTexCamera);
      
      _monoCamera->setNodeMask(0x0); // Disable mono camera
    }
    else
    {
      std::cerr<< "OpenFrames::VRCamera ERROR: Invalid combination of StereoMode (" << _mode << ") and zNear (" << zNear << ")" << std::endl;
    }
  }
  
  VRCameraManager::VRCameraManager(VRTextureBuffer *texBuffer)
  : _texBuffer(texBuffer)
  {}
  
  VRCameraManager::~VRCameraManager()
  {
    reset();
  }
  
  std::string VRCameraManager::getCameraName(unsigned int camNum)
  {
    if(camNum < _vrCameraList.size()) return (vrCamNamePrefix + std::to_string(camNum));
    else return "VR Invalid Camera Number";
  }
  
  // Create a new VRCamera if needed, and add it as a slave
  void VRCameraManager::enableCamera(unsigned int camNum,
                                     osg::GraphicsContext* gc,
                                     osg::Camera* masterCamera,
                                     const double &zNear, const double &zFar)
  {
    if(_vrCameraList.size() <= camNum) _vrCameraList.resize(camNum+1);
    VRCamera *vrcam = _vrCameraList[camNum].get();
    
    if(!vrcam) // Create a new VRCamera
    {
      vrcam = new VRCamera(_texBuffer.get(), camNum, VRCamera::STEREO, true); // Use MSAA
      
      osg::Camera *cam;
      for(unsigned int i = 0; i < vrcam->getNumCameras(); ++i)
      {
        // Set camera graphics context
        cam = vrcam->getCamera(i);
        cam->setGraphicsContext(gc);
        
        // Cameras are rendered in order of increasing render number, so
        // set this camera's number as its render number
        cam->setRenderOrder(osg::Camera::PRE_RENDER, camNum);
        
        // We will compute the view and projection matrices ourselves
        cam->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
        cam->setComputeNearFarMode(osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR);
        
        // Add camera as slave, and tell it to use the master Camera's scene
        masterCamera->getView()->addSlave(cam, true);
      }
      
      // Specify whether first VRCamera should clear color buffer
      if(camNum == 0 && _clearColorBuffer)
        vrcam->setClearColorBuffer(true);
      else
        vrcam->setClearColorBuffer(false);
      
      // Store new VRCamera in internal camera list
      _vrCameraList[camNum] = vrcam;
    }
    
    // Update projection matrix depth planes
    if (_ovrDevice.get())
    {
      // Get per-eye projection matrices and update them with new depth planes
      osg::Matrixd rightProj = _ovrDevice->getRightEyeProjectionMatrix();
      osg::Matrixd leftProj = _ovrDevice->getLeftEyeProjectionMatrix();
      osg::Matrixd centerProj = _ovrDevice->getCenterProjectionMatrix();
      OpenFrames::updateProjectionMatrix(rightProj, zNear, zFar);
      OpenFrames::updateProjectionMatrix(leftProj, zNear, zFar);
      OpenFrames::updateProjectionMatrix(centerProj, zNear, zFar);

      // Get eye offset matrices
      osg::Matrixd rightOffset, leftOffset, centerOffset;
      rightOffset = _ovrDevice->getRightEyeViewOffsetMatrix();
      leftOffset = _ovrDevice->getLeftEyeViewOffsetMatrix();
      centerOffset = _ovrDevice->getCenterViewOffsetMatrix();

      // Compose per-eye view matrix as: Camera*HMD*EyeOffset
      // At this point, the master camera view matrix already contains the HMD pose
      // so we only need to add the eye offset matrix
      osg::Matrixd& masterView = masterCamera->getViewMatrix();
      osg::Matrixd rightView(masterView), leftView(masterView), centerView(masterView);
      rightView.postMult(rightOffset);
      leftView.postMult(leftOffset);
      centerView.postMult(centerOffset);

      vrcam->updateCameras(rightView, leftView, centerView, rightProj, leftProj, centerProj, zNear);
    }
    else
    {
      // Update camera matrices and properties using master camera data
      osg::Matrixd projmat = masterCamera->getProjectionMatrix();
      OpenFrames::updateProjectionMatrix(projmat, zNear, zFar);
      vrcam->updateCameras(masterCamera->getViewMatrix(), projmat, zNear);
    }
  }
  
  // Disable all cameras starting with the specified index
  void VRCameraManager::disableCameras(unsigned int start)
  {
    for(int i = start; i < _vrCameraList.size(); ++i)
    {
      _vrCameraList[i]->disableCameras();
    }
  }
  
  // Detach all our cameras from the main scene, then erase the cameras
  void VRCameraManager::reset()
  {
    // Loop over all our VRCameras
    for(unsigned int i = 0; i < _vrCameraList.size(); ++i)
    {
      // Get every camera and remove it from the parent osg::View
      VRCamera *vrcam = _vrCameraList[i];
      for(unsigned int j = 0; j < vrcam->getNumCameras(); ++j)
      {
        osg::View *view = vrcam->getCamera(j)->getView();
        if(view)
        {
          unsigned int pos = view->findSlaveIndexForCamera(vrcam->getCamera(j));
          view->removeSlave(pos);
        }
        else
        {
          std::cerr<< "OpenFrames::VRCameraManager WARNING: " << vrcam->getCamera(j)->getName() << " does not have a parent osg::View. Cannot remove." << std::endl;
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
    if(!_vrCameraList.empty())
    {
      _vrCameraList[0]->setClearColorBuffer(clear);
    }
    
  }
  
} // !namespace OpenFrames
