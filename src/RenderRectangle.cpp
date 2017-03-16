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

#include <OpenFrames/RenderRectangle.hpp>
#include <osg/Geometry>
#include <osg/Group>
#include <osg/LineWidth>
#include <osg/Depth>
#include <osg/StateSet>
#include <iostream>

// New = DepthPartitioner, with slave cameras
// Old = DepthPartitionNode, with nested cameras
const bool useNewDP = true;

namespace OpenFrames
{
  // Print name of each camera that tries to cull-traverse a scene
  class VisitorNameNode : public osg::Node
  {
  public:
    VisitorNameNode() {}
    virtual void traverse(osg::NodeVisitor &nv)
    {
      osgUtil::CullVisitor *cv = nv.asCullVisitor();
      if(cv)
      {
        osg::Camera *cam = cv->getCurrentCamera();
        std::cout<< "Cull from Camera " << cam->getName() << std::endl;
      }
    }
  };
  
  class GetPosesCallback : public osg::View::Slave::UpdateSlaveCallback
  {
  public:
    GetPosesCallback(OpenVRDevice *ovrDevice)
    : _ovrDevice(ovrDevice)
    { }
    
    virtual void updateSlave(osg::View& view, osg::View::Slave& slave)
    {
      _ovrDevice->waitGetPoses();
    }
    
  private:
    osg::observer_ptr<OpenVRDevice> _ovrDevice;
  };

  // Slave callback to inherit master camera view matrix but not its projection matrix
  // Used by the background camera for VR rendering
  class BackCameraViewCallback : public osg::View::Slave::UpdateSlaveCallback
  {
  public:
    virtual void updateSlave(osg::View& view, osg::View::Slave& slave)
    {
      slave._camera->setViewMatrix(view.getCamera()->getViewMatrix());
      slave._camera->inheritCullSettings(*(view.getCamera()), slave._camera->getInheritanceMask());
    }
  };
  
  RenderRectangle::RenderRectangle(OpenVRDevice *ovrDevice, VRTextureBuffer *vrTextureBuffer)
  : _ovrDevice(ovrDevice), _vrTextureBuffer(vrTextureBuffer)
  {
    // Test if VR is enabled
    _useVR = false;
    if(_ovrDevice.get() && _vrTextureBuffer.get())
    {
      if(_ovrDevice->isInitialized()) _useVR = true;
    }
    
    // Create the Camera that will draw HUD elements
    _hudCamera = new osg::Camera;
    _hudCamera->setName("HUD");
    
    // The user's scene
    _scene = new osg::Group;
    
    // For debugging, write names of all cameras that try to traverse the scene
    //_scene->addChild(new VisitorNameNode);
    
    // Create the Camera that will draw background elements
    _backCamera = new osg::Camera;
    _backCamera->setName("Background");
    
    // If using VR, then create a stereo VR camera for background elements
    if (_useVR) _backCameraVR = new VRCamera(_vrTextureBuffer.get(), -1, VRCamera::STEREO, false);

    // Create the Camera that will mirror the VR scene to the window
    _mirrorCamera = new osg::Camera;
    _mirrorCamera->setName("Mirror");
    
    // Create the sky sphere
    _skySphere = new SkySphere("Sky Sphere");
    
    // The SceneView is responsible for doing the update, cull, and
    // draw operations for the ReferenceFrame scene.
    _sceneView = new osgViewer::View();
    _sceneView->getCamera()->setName("Master");
    
    // Create the auto depth partitioner
    _depthPartitionNode = new DepthPartitionNode;
    _depthPartitioner = new DepthPartitioner;
    
    // If VR is used, then update depth partitioner with VR callbacks
    if(_useVR)
    {
      // Create a camera manager to handle VR cameras
      VRCameraManager *vrCamManager = new VRCameraManager(_vrTextureBuffer.get());
      vrCamManager->setOpenVRDevice(_ovrDevice.get());
      
      // Create callback to update VR poses
      GetPosesCallback *poseCallback = new GetPosesCallback(_ovrDevice.get());
      
      // Customize depth partitioner with callbacks
      _depthPartitioner->getCallback()->setCameraManager(vrCamManager);
      _depthPartitioner->getCallback()->setUpdateCallback(poseCallback);
    }
    if(useNewDP) _depthPartitioner->setViewToPartition(_sceneView);
    
    // Create a default view and make it active
    _defaultView = new View;
    _currView = 0;
    _sceneView->setCameraManipulator(getCurrentView()->getTrackball());
    
    // Initialize parameters
    _init();
  }
  
  RenderRectangle::~RenderRectangle() { }
  
  void RenderRectangle::_init()
  {
    // Master camera reference will be used throughout
    osg::Camera *masterCam = _sceneView->getCamera();
    
    // Set up cameras
    {
      // Set up HUD camera render properties
      // Note that a HUD should never be drawn on a VR display to avoid eye strain
      _hudCamera->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
      _hudCamera->setClearMask(GL_DEPTH_BUFFER_BIT);
      _hudCamera->setAllowEventFocus(false);
      _hudCamera->setRenderOrder(osg::Camera::POST_RENDER, 101); // Render after other cameras
      _hudCamera->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
      _hudCamera->setViewMatrix(osg::Matrix::identity());
      _hudCamera->setProjectionMatrix(osg::Matrix::ortho2D(0, 1, 0, 1));
      _hudCamera->setProjectionResizePolicy(osg::Camera::FIXED); // Resizing should not affect projection matrix
      _sceneView->addSlave(_hudCamera, false);
      
      // Set up background camera render properties
      if(_useVR)
      { 
        // Set individual VR subcamera properties
        osg::Camera *cam;
        for (unsigned int i = 0; i < _backCameraVR->getNumCameras(); ++i)
        {
          cam = _backCameraVR->getCamera(i);

          // Cameras are rendered in order of increasing render number, so
          // set this camera's number as its render number
          cam->setRenderOrder(osg::Camera::PRE_RENDER, -1);

          // We will compute the view and projection matrices ourselves
          // NOTE: Change this to ABSOLUTE_RF after VR view matrix has been implemented
          cam->setReferenceFrame(osg::Transform::RELATIVE_RF);
          //cam->setComputeNearFarMode(osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR);

          // Add camera as slave, and tell it not to use the master Camera's scene
          _sceneView->addSlave(cam, false);

          // Add callback to only use master camera's view matrix
          // NOTE: Remvove this after VR view matrix has been implemented 
          _sceneView->findSlaveForCamera(cam)->_updateSlaveCallback = new BackCameraViewCallback;
        }

        // Set general VR camera properties
        _backCameraVR->setClearColorBuffer(true);
        osg::Matrixd rightProj = _ovrDevice->getRightEyeProjectionMatrix();
        osg::Matrixd leftProj = _ovrDevice->getLeftEyeProjectionMatrix();
        osg::Matrixd centerProj = _ovrDevice->getCenterProjectionMatrix();
        _backCameraVR->updateCameras(osg::Matrixd(), rightProj, leftProj, centerProj, 1.0);
        masterCam->setProjectionMatrix(centerProj);
      }
      else
      {
        _backCamera->setReferenceFrame(osg::Transform::RELATIVE_RF);
        _backCamera->setClearMask(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT);
        _backCamera->setAllowEventFocus(false);
        _backCamera->setRenderOrder(osg::Camera::PRE_RENDER, -1); // Render before other cameras
        _backCamera->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
        _sceneView->addSlave(_backCamera, false);
      }

      // Set up mirror camera render properties
      _mirrorCamera->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
      _mirrorCamera->setClearMask(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT);
      _mirrorCamera->setAllowEventFocus(false);
      _mirrorCamera->setRenderOrder(osg::Camera::POST_RENDER, 100); // Render just before HUD
      _mirrorCamera->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
      _mirrorCamera->setViewMatrix(osg::Matrix::identity());
      _mirrorCamera->setProjectionMatrix(osg::Matrix::ortho2D(0, 1, 0, 1));
      if(_useVR) // Set up mirror camera to show one eye texture
      {
        osg::Geometry* geom = osg::createTexturedQuadGeometry(osg::Vec3(), osg::Vec3(1, 0, 0), osg::Vec3(0, 1, 0));
        osg::Geode *quad = new osg::Geode;
        quad->addDrawable(geom);
        _mirrorCamera->addChild(quad);
        _mirrorCamera->getOrCreateStateSet()->setTextureAttributeAndModes(0, _vrTextureBuffer->_rightColorTex, osg::StateAttribute::ON);
        _sceneView->addSlave(_mirrorCamera, false);
      }

      // Main camera shouldn't clear its color buffer
      masterCam->setClearMask(GL_DEPTH_BUFFER_BIT);
      if(_useVR) masterCam->setProjectionResizePolicy(osg::Camera::FIXED);
    }
    
    // Set up border rectangle
    {
      // Create the geode that will contain the border
      _borderGeode = new osg::Geode;
      _borderGeode->setCullingActive(false);
      osg::StateSet* ss = _borderGeode->getOrCreateStateSet();
      ss->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
      ss->setMode(GL_DEPTH_TEST, osg::StateAttribute::OFF);
      ss->setAttribute(new osg::LineWidth);
      
      // Specify points that make up the border
      osg::Vec3Array* vertices = new osg::Vec3Array;
      vertices->push_back(osg::Vec3(0, 0, -1));
      vertices->push_back(osg::Vec3(0.999, 0, -1));
      vertices->push_back(osg::Vec3(0.999, 0.999, -1));
      vertices->push_back(osg::Vec3(0, 0.999, -1));
      
      // Create a dummy color for the border
      osg::Vec4Array *colors = new osg::Vec4Array;
      colors->push_back(osg::Vec4(0, 0, 0, 1.0));
      
      // Set up the geometry object for the border
      osg::Geometry *geom = new osg::Geometry;
      geom->setVertexArray(vertices);
      geom->setColorArray(colors);
      geom->setColorBinding(osg::Geometry::BIND_OVERALL);
      geom->addPrimitiveSet(new osg::DrawArrays(GL_LINE_LOOP, 0, 4));
      _borderGeode->addDrawable(geom);
      
      _hudCamera->addChild(_borderGeode); // Add border to HUD
      setSelected(false); // Set the border to look deselected by default
    }
    
    // Set up the Sky Sphere
    {
      _skySphere->setDrawMode(SkySphere::NONE); // Disabled by default
      if (_useVR)
      {
        osg::Camera *cam;
        for (unsigned int i = 0; i < _backCameraVR->getNumCameras(); ++i)
        {
          cam = _backCameraVR->getCamera(i);
          cam->addChild(_skySphere->getGroup());
        }
      }
      else
        _backCamera->addChild(_skySphere->getGroup());
    }
    
    // Set up the depth partitioner
    {
      // Don't clear color buffer so that background is preserved
      _depthPartitionNode->setClearColorBuffer(false);
      _depthPartitioner->getCallback()->setClearColorBuffer(false);
      if(!useNewDP) _scene->addChild(_depthPartitionNode.get());
    }
    
    // Set up the SceneView
    _sceneView->setSceneData(_scene);
    masterCam->setCullingMode(osg::CullSettings::DEFAULT_CULLING & ~osg::CullSettings::SMALL_FEATURE_CULLING);
    masterCam->setNearFarRatio(0.0001);
  }
  
  void RenderRectangle::setFrameManager(FrameManager *fm)
  {
    // We're already using the given FrameManager
    if(_frameManager.get() == fm) return;
    
    if(_frameManager.valid()) // Remove the old scene
    {
      _depthPartitionNode->removeChild(_frameManager->getData());
    }
    
    // Set a default view
    if(fm == NULL)
    {
      _defaultView->setViewFrame(NULL, NULL);
      _defaultView->resetTrackball();
    }
    else
    {
      if(useNewDP) _scene->addChild(fm->getData());
      _depthPartitionNode->addChild(fm->getData()); // Set the new scene
      _defaultView->setViewFrame(fm->getFrame(), fm->getFrame());
      _defaultView->resetTrackball();
    }
    
    _frameManager = fm; // Set the new FrameManager
  }
  
  void RenderRectangle::setGraphicsContext(osg::GraphicsContext *gc)
  {
    _sceneView->getCamera()->setGraphicsContext(gc);
    _hudCamera->setGraphicsContext(gc);
    _mirrorCamera->setGraphicsContext(gc);
    if (_useVR)
    {
      osg::Camera *cam;
      for (unsigned int i = 0; i < _backCameraVR->getNumCameras(); ++i)
      {
        cam = _backCameraVR->getCamera(i);
        cam->setGraphicsContext(gc);
      }
    }
    else
      _backCamera->setGraphicsContext(gc);
  }
  
  void RenderRectangle::setViewport(int x, int y, int w, int h)
  {
    _sceneView->getCamera()->setViewport(x, y, w, h);
    _hudCamera->setViewport(x, y, w, h);
    _mirrorCamera->setViewport(x, y, w, h);
    if(!_useVR)
    {
      _backCamera->setViewport(x, y, w, h);
    }
  }
  
  /** Set this RenderRectangle to have a "selected" or "deselected" look */
  void RenderRectangle::setSelected(bool select)
  {
    // Set color
    osg::Geometry *geom = dynamic_cast<osg::Geometry*>(_borderGeode->getDrawable(0));
    osg::Vec4Array *colors = dynamic_cast<osg::Vec4Array*>(geom->getColorArray());
    if(select)
    {
      (*colors)[0].set(1.0, 0, 0, 1.0); // Make red
    }
    else
    {
      (*colors)[0].set(0, 1.0, 0, 1.0); // Make green
    }
    
    // Set line width
    osg::LineWidth *lw = dynamic_cast<osg::LineWidth*>(_borderGeode->getStateSet()->getAttribute(osg::StateAttribute::LINEWIDTH));
    if(select)
    {
      lw->setWidth(3.0); // Set a 3.0 line width
    }
    else
    {
      lw->setWidth(1.0); // Set a 1.0 line width
    }
    
    geom->dirtyDisplayList(); // Indicate that the border should be redrawn
  }
  
  /** Get the selected state of this RenderRectangle */
  bool RenderRectangle::getSelected()
  {
    osg::LineWidth *lw = dynamic_cast<osg::LineWidth*>(_borderGeode->getStateSet()->getAttribute(osg::StateAttribute::LINEWIDTH));
    if(lw->getWidth() == 1.0) return false;
    else return true;
  }
  
  /** Set whether the border rectangle is shown or not */
  void RenderRectangle::setShowBorder(bool show)
  {
    if(show) _borderGeode->setNodeMask(0xffffffff);
    else _borderGeode->setNodeMask(0x0);
  }
  
  void RenderRectangle::setSkySphereTexture(const std::string& fname)
  {
    unsigned int currDrawMode = _skySphere->getDrawMode();
    if(_skySphere->setTextureMap(fname))
      // Add TEXTURE to existing drawmode
      _skySphere->setDrawMode(currDrawMode | SkySphere::TEXTURE);
    else
      // Remove TEXTURE from existing drawmode
      _skySphere->setDrawMode(currDrawMode & ~SkySphere::TEXTURE);
  }
  
  bool RenderRectangle::setSkySphereStarData(const std::string& catalogName,
                                             float minMag, float maxMag,
                                             unsigned int maxNumStars,
                                             float starScale)
  {
    unsigned int currDrawMode = _skySphere->getDrawMode();
    bool success = _skySphere->setStarData(catalogName, minMag, maxMag, maxNumStars, starScale);
    if(success)
      // Add STARFIELD to existing drawmode
      _skySphere->setDrawMode(currDrawMode | SkySphere::STARFIELD);
    else
      // Remove STARFIELD from existing drawmode
      _skySphere->setDrawMode(currDrawMode & ~SkySphere::STARFIELD);
    
    return success;
  }
  
  void RenderRectangle::setBackgroundColor(float r, float g, float b)
  {
    _sceneView->getCamera()->setClearColor(osg::Vec4(r, g, b, 1.0));
  }
  
  void RenderRectangle::addView(View *view)
  {
    if(view != NULL)
    {
      _views.push_back(view);
      
      // Added first view, so make sure that it is selected
      if(_views.size() == 1)
      {
        _currView = 0;
        selectCurrentView();
      }
    }
  }
  
  void RenderRectangle::removeView(View *view)
  {
    if(view != NULL && !_views.empty())
    {
      osg::ref_ptr<View> cv = getCurrentView(); // Save currently selected view
      osg::ref_ptr<View> v_save = view; // Save to make sure View doesn't get deallocated
      
      // Find the view to be deleted
      ViewList::iterator i = std::find(_views.begin(), _views.end(), view);
      while(i != _views.end())
      {
        // Save this view's trackball then erase it from the list
        (*i)->saveTrackball();
        _views.erase(i);
        
        i = std::find(_views.begin(), _views.end(), view); // Find next instance of view
      }
      
      // We erased the currently selected view, so select the first view
      if(cv == v_save)
      {
        _currView = 0;
        selectCurrentView(); // Will select first view (or default view if no stored views)
      }
      else selectView(cv); // Select what was already selected before
    }
  }
  
  void RenderRectangle::removeAllViews()
  {
    // Save each view's trackball
    for(ViewList::iterator i = _views.begin(); i != _views.end(); ++i)
    {
      (*i)->saveTrackball();
    }
    
    _views.clear();
    selectCurrentView(); // Will select the default view
  }
  
  void RenderRectangle::nextView()
  {
    // Save the current View's trackball
    getCurrentView()->saveTrackball();
    
    // Cycle to next View
    if(!_views.empty())
    {
      if(_currView >= _views.size()-1) _currView = 0;
      else _currView++;
    }
    
    // Set the new View's trackball as active
    selectCurrentView();
  }
  
  void RenderRectangle::previousView()
  {
    // Save the current View's trackball
    getCurrentView()->saveTrackball();
    
    // Cycle to previous View
    if(!_views.empty())
    {
      if(_currView == 0) _currView = _views.size()-1;
      else _currView--;
    }
    
    // Set the new View's trackball as active
    selectCurrentView();
  }
  
  void RenderRectangle::selectView(View *view)
  {
    // Search for the requested view in the list
    for(unsigned int i = 0; i < _views.size(); ++i)
    {
      // If found, apply that view
      if(_views[i] == view)
      {
        // Save the previous View's trackball
        getCurrentView()->saveTrackball();
        
        _currView = i; // Select the new View
        
        // Set the new View's trackball as active
        selectCurrentView();
        return;
      }
    }
  }
  
  void RenderRectangle::selectView(unsigned int newView)
  {
    if(_views.empty() || (newView >= _views.size())) return;
    
    // Save the previous View's trackball
    getCurrentView()->saveTrackball();
    
    _currView = newView; // Select the new View
    
    // Set the new View's trackball as active
    selectCurrentView();
  }
  
  void RenderRectangle::selectCurrentView()
  {
    _sceneView->setCameraManipulator(getCurrentView()->getTrackball());
    applyCurrentViewProjection();
  }
  
  View* RenderRectangle::getCurrentView()
  {
    if(_currView >= _views.size()) return _defaultView.get();
    else return _views[_currView].get();
  }
  
  void RenderRectangle::applyCurrentViewProjection()
  {
    if (_useVR) return; // VR applies its own projection

    View *view = getCurrentView();
    
    /** Adjust the perspective projection with the current viewport size */
    if(view->getProjectionType() == View::PERSPECTIVE)
    {
      // Get current field of view
      double fov, ratio;
      view->getPerspective(fov, ratio);
      
      // Get main camera viewport or depth partitioner's viewport
      osg::Viewport *vp = _sceneView->getCamera()->getViewport();
      if(vp) ratio = (double)vp->width() / (double)vp->height();
      
      // Set new aspect ratio
      view->setPerspective(fov, ratio); // Set new aspect ratio
    }
    else
    {
      std::cerr<< "OpenFrames::RenderRectangle: Ortho projection not supported" << std::endl;
    }
    
    // Apply the projection matrix
    _sceneView->getCamera()->setProjectionMatrix(view->getProjectionMatrix());
  }
  
}
