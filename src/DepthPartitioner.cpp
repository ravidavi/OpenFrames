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

#include <OpenFrames/DepthPartitioner.hpp>
#include <osgUtil/CullVisitor>
#include <iostream>
#include <iomanip>

// DepthPartitioner camera name components
static const std::string dpCamNamePrefix("DPCam");
static const std::string dpMainCamName(dpCamNamePrefix+"Main");

namespace OpenFrames
{
  DepthPartitioner::DepthPartitioner(osgViewer::View *view)
  {
    // Create slave Camera that will hold the depth partitioner
    if(view && view->getCamera())
      _dpMainSlaveCamera = new osg::Camera(*(view->getCamera()));
    else
      _dpMainSlaveCamera = new osg::Camera();
    _dpMainSlaveCamera->setNodeMask(0x0);
    
    // Create callback that will perform depth partitioning
    _dpCallback = new DepthPartitionCallback();

    setViewToPartition(view);
  }
  
  void DepthPartitioner::setViewToPartition(osgViewer::View *view)
  {
    if(_view == view) return; // Already partitioning the View
    
    // Make sure another DepthPartitioner isn't already analyzing the View
    if(view != NULL)
    {
      unsigned int numSlaves = view->getNumSlaves();
      for(unsigned int i = 0; i < numSlaves; ++i)
      {
        if(view->getSlave(i)._camera->getName() == dpMainCamName)
        {
          std::cerr<< "DepthPartitioner ERROR: View already has an attached  DepthPartitioner." << std::endl;
          return;
        }
      }
    }
    
    // Remove all depth partitioning objects from previous View
    if(_view.valid())
    {
      // Detach main slave Camera from previous View
      _view->removeSlave(_view->findSlaveIndexForCamera(_dpMainSlaveCamera));
      
      // Detach depth partition slave cameras from previous view
      _dpCallback->reset();
    }
    
    _view = view; // Set new View
    
    // Add depth partitioning objects to new View
    if(_view.valid())
    {
      // Add main slave camera, don't use master scene data
      _view->addSlave(_dpMainSlaveCamera, false);
      
      // Add DepthPartitionCallback as a slave update callback
      osg::View::Slave *slave = _view->findSlaveForCamera(_dpMainSlaveCamera);
      slave->_updateSlaveCallback = _dpCallback;
    }
  }
  
  void DepthPartitioner::setGraphicsContext(osg::GraphicsContext *gc)
  {
    _dpMainSlaveCamera->setGraphicsContext(gc);
  }
  
  void DepthPartitioner::setViewport(int x, int y, int w, int h)
  {
    _dpMainSlaveCamera->setViewport(x, y, w, h);
  }
  
  void DepthPartitioner::setClearColorBuffer(bool clear)
  {
    _dpCallback->setClearColorBuffer(clear);
  }
  
  DepthPartitioner::~DepthPartitioner()
  {
    setViewToPartition(NULL);
  }
  
  DepthPartitionCallback::DepthPartitionCallback()
  : _active(true),
  _clearColorBuffer(true),
  _renderOrder(osg::Camera::PRE_RENDER)
  {
    _distAccumulator = new DistanceAccumulator;
  }
  
  DepthPartitionCallback::~DepthPartitionCallback()
  {
    reset();
  }
  
  void DepthPartitionCallback::setActive(bool active)
  {
    _active = active;
    
    // Disable all cameras if needed
    // Cameras will be re-enabled in the update callback as needed
    if(!_active)
    {
      for(unsigned int i = 0; i < _cameraList.size(); ++i)
      {
        _cameraList[i]->setNodeMask(0xf);
      }
    }
  }
  
  void DepthPartitionCallback::reset()
  {
    // Remove slave cameras
    for(int i = 0; i < _cameraList.size(); ++i)
    {
      osg::View *view = _cameraList[i]->getView();
      if(view)
      {
        unsigned int pos = view->findSlaveIndexForCamera(_cameraList[i]);
        view->removeSlave(pos);
      }
    }
    
    _cameraList.clear(); // Erase all cameras
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
  
  void DepthPartitionCallback::updateSlave(osg::View& view, osg::View::Slave& slave)
  {
    slave.updateSlaveImplementation(view);
    
    if(!_active) return; // Don't analyze if inactive
    
    // If the scene hasn't been defined then don't do anything
    osgViewer::View *sceneView = dynamic_cast<osgViewer::View*>(&view);
    if(!sceneView || !sceneView->getSceneData()) return;
    
    // Get transformation matrices from master Camera
    osg::Camera *camera = slave._camera;
    osg::Matrixd &viewmat = camera->getViewMatrix();
    osg::Matrixd &projmat = camera->getProjectionMatrix();
    
    // Prepare for scene traversal
    _distAccumulator->setMatrices(viewmat, projmat);
    _distAccumulator->setNearFarRatio(camera->getNearFarRatio());
    _distAccumulator->reset();
    
    // Step 1: Traverse the scene, collecting near/far distances.
    sceneView->getSceneData()->accept(*(_distAccumulator.get()));
    
    // Step 2: Compute the near and far distances for every Camera that
    // should be used to render the scene
    _distAccumulator->computeCameraPairs();
    DistanceAccumulator::PairList& camPairs = _distAccumulator->getCameraPairs();
    
    // Step 3: Create the slave Cameras that will draw each depth segment
    unsigned int numCameras = camPairs.size(); // Get the number of cameras
    if(numCameras > 0)
    {
      osg::Camera *currCam;
      osg::Matrixd proj;
      
      for(int i = 0; i < numCameras; ++i)
      {
        // Create and activate the camera
        currCam = createOrReuseCamera(i, camera);
        
        // Copy parent projection matrix and update the near/far planes
        proj = projmat;
        updateProjectionMatrix(proj, camPairs[i].first, camPairs[i].second);
        std::cout<< std::setprecision(10) << currCam->getName() << ": " << camPairs[i].first << ", " << camPairs[i].second << std::endl;
        
        // Set the camera rendering state
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
  }
  
  osg::Camera* DepthPartitionCallback::createOrReuseCamera(unsigned int camNum,
                                                           osg::Camera* mainCamera)
  {
    if(_cameraList.size() <= camNum) _cameraList.resize(camNum+1);
    osg::Camera *camera = _cameraList[camNum].get();
    
    if(!camera) // Create a new Camera
    {
      camera = new osg::Camera;
      camera->setCullingActive(false);
      camera->setCullingMode(mainCamera->getCullingMode());
      camera->setRenderOrder(_renderOrder, camNum);
      camera->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
      camera->setName(std::string("DPCam") + std::to_string(camNum));
      
      // We will compute the near/far planes ourselves
      camera->setComputeNearFarMode(osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR);
      
      if(camNum == 0 && _clearColorBuffer)
      {
        camera->setClearMask(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        camera->setClearColor(mainCamera->getClearColor());
      }
      else
        camera->setClearMask(GL_DEPTH_BUFFER_BIT);
      
      // Add Camera as slave to master
      mainCamera->getView()->addSlave(camera, true);
      
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
} // OpenFrames namespace
