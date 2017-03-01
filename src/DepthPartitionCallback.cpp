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

#include <OpenFrames/DepthPartitionCallback.hpp>
#include <osgUtil/CullVisitor>
#include <iostream>

namespace OpenFrames
{
  DepthPartitionCallback::DepthPartitionCallback()
  : _active(true),
  _clearColorBuffer(true),
  _renderOrder(osg::Camera::PRE_RENDER)
  {
    _distAccumulator = new DistanceAccumulator;
  }
  
  DepthPartitionCallback::~DepthPartitionCallback() {}
  
  void DepthPartitionCallback::setActive(bool active)
  {
    if(_active == active) return;
    _active = active;
  }
  
  void DepthPartitionCallback::reset()
  {
    if(!_view.valid()) return;
    
    // Reinstate master camera's graphics state
    _view->getCamera()->setGraphicsContext(_gc);
    _view->getCamera()->setViewport(_viewport);
    
    // Remove slave cameras
    for(int i = 0; i < _cameraList.size(); ++i)
    {
      unsigned int pos = _view->findSlaveIndexForCamera(_cameraList[i]);
      _view->removeSlave(pos);
    }
    
    _cameraList.clear(); // Remove all cameras
  }
  
  void DepthPartitionCallback::setClearColorBuffer(bool clear)
  {
    _clearColorBuffer = clear;
    
    // Tell the first camera whether to clear the color buffer.
    if(!_cameraList.empty())
    {
      if(clear)
        _cameraList[0]->setClearMask(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      else
        _cameraList[0]->setClearMask(GL_DEPTH_BUFFER_BIT);
    }
  }
  
  void DepthPartitionCallback::setRenderOrder(osg::Camera::RenderOrder order)
  {
    _renderOrder = order;
    
    // Update the render order for existing Cameras
    for(unsigned int i = 0; i < _cameraList.size(); ++i)
    {
      _cameraList[i]->setRenderOrder(_renderOrder, i);
    }
  }
  
  bool DepthPartitionCallback::run(osg::Object* object, osg::Object* data)
  {
    traverse(object, data); // Traverse nested callbacks
    
    if(!_active) return false; // Don't analyze if inactive
 
    // Ensure Camera is the original one that was specified
    osg::Camera *camera = dynamic_cast<osg::Camera*>(object);
    if(!validateCamera(camera)) return false;
    
    // If the scene hasn't been defined then don't do anything
    if(!_view->getSceneData()) return false;
    
    osgViewer::View *view = dynamic_cast<osgViewer::View*>(camera->getView());
    
    // Collect information on the current transformation matrices
    osg::Matrixd &viewmat = camera->getViewMatrix();
    osg::Matrixd &projmat = camera->getProjectionMatrix();
    
    // Prepare for scene traversal
    _distAccumulator->setMatrices(viewmat, projmat);
    _distAccumulator->setNearFarRatio(camera->getNearFarRatio());
    _distAccumulator->reset();
    
    // Step 1: Traverse the scene, collecting near/far distances.
    _view->getSceneData()->accept(*(_distAccumulator.get()));
    
    // Step 2: Compute the near and far distances for every Camera that
    // should be used to render the scene
    _distAccumulator->computeCameraPairs();
    DistanceAccumulator::PairList& camPairs = _distAccumulator->getCameraPairs();
    
    // Step 3: Create the Cameras that will draw each depth segment, and add them as slaves
    unsigned int numCameras = camPairs.size(); // Get the number of cameras
    if(numCameras > 0)
    {
      osg::Camera *currCam;
      DistanceAccumulator::DistancePair currNearFar;
      osg::Matrixd proj;
      
      for(int i = 0; i < numCameras; ++i)
      {
        // Create and activate the camera
        currCam = createOrReuseCamera(i, camera);
        
        // Copy parent projection matrix and update the near/far planes
        proj = projmat;
        currNearFar = camPairs[i];  // (near,far) pair for current camera
        updateProjectionMatrix(proj, camPairs[i].first, camPairs[i].second);
        
        // Set the camera rendering info
        currCam->setNodeMask(0xffffffff);
        currCam->setGraphicsContext(camera->getGraphicsContext());
        currCam->setViewport(camera->getViewport());
        currCam->setProjectionMatrix(proj);
        currCam->setViewMatrix(viewmat);
      }
    }
    
    // Step 4: Deactivate unused cameras
    for(int i = numCameras; i < _cameraList.size(); ++i)
    {
      _cameraList[i]->setNodeMask(0x0);
    }
    
    return true;
  }
  
  osg::Camera* DepthPartitionCallback::createOrReuseCamera(unsigned int camNum,
                                                           osg::Camera* masterCamera)
  {
    if(_cameraList.size() <= camNum) _cameraList.resize(camNum+1);
    osg::Camera *camera = _cameraList[camNum].get();
    
    if(!camera) // Create a new Camera
    {
      camera = new osg::Camera;
      camera->setCullingActive(false);
      camera->setCullingMode(masterCamera->getCullingMode());
      camera->setRenderOrder(_renderOrder, camNum);
      camera->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
      camera->setName(std::string("DPCam") + std::to_string(camNum));
      
      // We will compute the near/far planes ourselves
      camera->setComputeNearFarMode(osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR);
      
      if(camNum == 0 && _clearColorBuffer)
      {
        camera->setClearMask(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        camera->setClearColor(masterCamera->getClearColor());
      }
      else
        camera->setClearMask(GL_DEPTH_BUFFER_BIT);
      
      // Add Camera as slave to master
      _view->addSlave(camera, true);
      
      // Store new camera in master camera list
      _cameraList[camNum] = camera;
    }
    
    return camera;
  }
  
  void DepthPartitionCallback::updateProjectionMatrix(osg::Matrix& proj, double near, double far)
  {
    double left, right, bottom, top, zNear, zFar;
    
    // Clamp the projection matrix z values to the range (near, far)
    double epsilon = 1.0e-6;
    if(fabs(proj(0,3)) < epsilon &&
       fabs(proj(1,3)) < epsilon &&
       fabs(proj(2,3)) < epsilon) // Projection is Orthographic
    {
      // Get the current orthographic projection parameters
      proj.getOrtho(left, right, bottom, top, zNear, zFar);
      
      // Use the custom computed near/far values
      proj.makeOrtho(left, right, bottom, top, near, far);
    }
    else // Projection is Perspective
    {
      // Get the current perspective projection parameters
      proj.getFrustum(left, right, bottom, top, zNear, zFar);
      
      // Use the custom computed near/far values
      const double nz = near/zNear;
      proj.makeFrustum(left*nz, right*nz, bottom*nz, top*nz, near, far);
    }
  }
  
  bool DepthPartitionCallback::validateCamera(osg::Camera *cam)
  {
    // Use Camera's View as this partitioner's View
    if(!_view.valid())
    {
      osgViewer::View *view = dynamic_cast<osgViewer::View*>(cam->getView());
      if(view == NULL) return false;
      else _view = view;
    }
    
    // Make sure Camera is the master of the current View
    if(_view->getCamera() != cam) return false;
    return true;
  }
  
} // OpenFrames namespace
