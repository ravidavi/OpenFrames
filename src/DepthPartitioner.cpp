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
  DepthPartitioner::DepthPartitioner(osgViewer::View *view, VRTextureBuffer *texBuffer)
  : _view(NULL) // Don't set to incoming view yet
  {
    // Create slave Camera to manage the depth partitioner
    // Note this camera won't have any scene of its own, it just analyzes the scene
    // and creates other slave cameras as needed
    _dpMainSlaveCamera = new osg::Camera();
    _dpMainSlaveCamera->setNodeMask(0x0);
    _dpMainSlaveCamera->setAllowEventFocus(false);
    
    // Create slave callback that will perform depth partitioning
    _dpCallback = new DepthPartitionCallback(texBuffer);

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
  
  DepthPartitionCallback::DepthPartitionCallback(VRTextureBuffer *texBuffer)
  : _clearColorBuffer(true),
  _texBuffer(texBuffer)
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
    for(int i = 0; i < numCameras; ++i)
    {
      // Create and activate the camera
      if(!_texBuffer)
      {
        createOrReuseCamera(i, camera, camPairs[i].first, camPairs[i].second);
      }
      else
      {
        createOrReuseVRCamera(i, camera, camPairs[i].first, camPairs[i].second);
      }
    }
    
    // Step 4: Deactivate unused cameras
    for(int i = numCameras; i < _cameraList.size(); ++i)
    {
      _cameraList[i]->setNodeMask(0x0);
    }
  }
  
  void DepthPartitionCallback::createOrReuseCamera(unsigned int camNum,
                                                   osg::Camera* mainCamera,
                                                   double &zNear, double &zFar)
  {
    if(_cameraList.size() <= camNum) _cameraList.resize(camNum+1);
    osg::Camera *newcam = _cameraList[camNum].get();
    
    if(!newcam) // Create a new Camera
    {
      newcam = new osg::Camera();
      newcam->setCullingActive(false);
      newcam->setRenderOrder(mainCamera->getRenderOrder(), camNum);
      newcam->setName(dpCamNamePrefix + std::to_string(camNum));
      newcam->setGraphicsContext(mainCamera->getGraphicsContext());
      newcam->setViewport(mainCamera->getViewport());
      newcam->setAllowEventFocus(false);
      
      // We will compute the projection matrix ourselves
      newcam->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
      newcam->setComputeNearFarMode(osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR);
      
      if(camNum == 0 && _clearColorBuffer)
        newcam->setClearMask(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      else
        newcam->setClearMask(GL_DEPTH_BUFFER_BIT);
      
      // Add Camera as slave, and tell it to use the master Camera's scene
      mainCamera->getView()->addSlave(newcam, true);
      
      // Store new camera in internal camera list
      _cameraList[camNum] = newcam;
    }
    
    // Update projection depth planes
    osg::Matrixd projmat = mainCamera->getProjectionMatrix();
    updateProjectionMatrix(projmat, zNear, zFar);
    
    // Set camera rendering matrices
    newcam->setProjectionMatrix(projmat);
    newcam->setViewMatrix(mainCamera->getViewMatrix());
    
    // Activate camera
    newcam->setNodeMask(0xffffffff);
  }
  
  void DepthPartitionCallback::createOrReuseVRCamera(unsigned int camNum,
                                                     osg::Camera* mainCamera,
                                                     double &zNear, double &zFar)
  {
    if(_vrCameraList.size() <= camNum) _vrCameraList.resize(camNum+1);
    VRCamera *vrcam = _vrCameraList[camNum].get();
    
    if(!vrcam) // Create a new VRCamera
    {
      vrcam = new VRCamera(_texBuffer, camNum, VRCamera::AUTO);
      vrcam->setGraphicsContext(mainCamera->getGraphicsContext());
      
      osg::Camera *cam;
      for(unsigned int i = 0; i < vrcam->getNumCameras(); ++i)
      {
        cam = vrcam->getCamera(i);
        cam->setRenderOrder(mainCamera->getRenderOrder(), camNum);
        cam->setComputeNearFarMode(osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR);
        if(camNum == 0 && _clearColorBuffer)
          cam->setClearMask(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        else
          cam->setClearMask(GL_DEPTH_BUFFER_BIT);
        
        mainCamera->getView()->addSlave(cam, true);
      }
      
      _vrCameraList[camNum] = vrcam;
    }
    
    // Update projection depth planes
    osg::Matrixd projmat = mainCamera->getProjectionMatrix();
    updateProjectionMatrix(projmat, zNear, zFar);
    
    // Set camera rendering matrices
    vrcam->setProjectionMatrix(projmat, zNear);
    vrcam->setViewMatrix(mainCamera->getViewMatrix());
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
