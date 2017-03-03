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
  : _view(NULL) // Don't set to incoming view yet
  {
    // Create slave Camera to manage the depth partitioner
    // Note this camera won't have any scene of its own, it just analyzes the scene
    // and creates other slave cameras as needed
    _dpMainSlaveCamera = new osg::Camera();
    _dpMainSlaveCamera->setNodeMask(0x0);
    _dpMainSlaveCamera->setAllowEventFocus(false);
    
    // Create slave callback that will perform depth partitioning
    _dpCallback = new DepthPartitionCallback();

    setViewToPartition(view);
  }
  
  bool DepthPartitioner::setViewToPartition(osgViewer::View *view)
  {
    if(_view == view) return true; // We are already partitioning the View
    
    // Make sure another DepthPartitioner isn't already partitioning the new View
    if(view != NULL)
    {
      unsigned int numSlaves = view->getNumSlaves();
      for(unsigned int i = 0; i < numSlaves; ++i)
      {
        // Check if slave's update callback is a DepthPartitionCallback
        DepthPartitionCallback *dpcb = dynamic_cast<DepthPartitionCallback*>(view->getSlave(i)._updateSlaveCallback.get());
        if(dpcb != NULL)
        {
          std::cerr<< "OpenFrames::DepthPartitioner ERROR: View already has an attached  DepthPartitioner." << std::endl;
          return false;
        }
      }
    }
    
    // Remove all depth partitioning objects from current View
    if(_view != NULL)
    {
      // Reenable current master camera
      _view->getCamera()->setNodeMask(0xffffffff);
      
      // Detach our main slave Camera from current View
      // This also removes the DepthPartitionCallback
      _view->removeSlave(_view->findSlaveIndexForCamera(_dpMainSlaveCamera));
      
      // Detach depth partition slave cameras from current View
      _dpCallback->reset();
    }
    
    _view = view; // Set new View
    
    // Add depth partitioning objects to new View
    if(_view != NULL)
    {
      // Add main slave camera, don't use master scene data
      _view->addSlave(_dpMainSlaveCamera, false);
      
      // Add DepthPartitionCallback as a slave update callback
      osg::View::Slave *slave = _view->findSlaveForCamera(_dpMainSlaveCamera);
      slave->_updateSlaveCallback = _dpCallback;
      
      // Disable the new master camera
      _view->getCamera()->setNodeMask(0x0);
    }
    
    return true;
  }
  
  DepthPartitioner::~DepthPartitioner()
  {
    setViewToPartition(NULL);
  }
  
  DepthPartitionCallback::DepthPartitionCallback()
  : _clearColorBuffer(true)
  {
    _distAccumulator = new DistanceAccumulator;
  }
  
  DepthPartitionCallback::~DepthPartitionCallback()
  {
    reset();
  }
  
  void DepthPartitionCallback::reset()
  {
    // Remove slave cameras from current View
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
  
  void DepthPartitionCallback::updateSlave(osg::View& view, osg::View::Slave& slave)
  {
    // If the scene hasn't been defined then don't do anything
    osgViewer::View *sceneView = dynamic_cast<osgViewer::View*>(&view);
    if(!sceneView || !sceneView->getSceneData()) return;
    
    // Get data from master Camera
    osg::Camera *camera = view.getCamera();
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
        
        // Copy main projection matrix and update the near/far planes
        proj = projmat;
        updateProjectionMatrix(proj, camPairs[i].first, camPairs[i].second);
        //std::cout<< std::setprecision(10) << currCam->getName() << " near = " << camPairs[i].first << ", far = " << camPairs[i].second << std::endl;
        
        // Set the camera rendering state
        currCam->setNodeMask(0xffffffff);
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
      camera = new osg::Camera();
      camera->setCullingActive(false);
      camera->setRenderOrder(mainCamera->getRenderOrder(), camNum);
      camera->setName(dpCamNamePrefix + std::to_string(camNum));
      camera->setGraphicsContext(mainCamera->getGraphicsContext());
      camera->setViewport(mainCamera->getViewport());
      camera->setAllowEventFocus(false);
      
      // We will compute the projection matrix ourselves
      camera->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
      camera->setComputeNearFarMode(osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR);
      
      if(camNum == 0 && _clearColorBuffer)
        camera->setClearMask(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      else
        camera->setClearMask(GL_DEPTH_BUFFER_BIT);
      
      // Add Camera as slave, and tell it to use the master Camera's scene
      mainCamera->getView()->addSlave(camera, true);
      
      // Store new camera in internal camera list
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
