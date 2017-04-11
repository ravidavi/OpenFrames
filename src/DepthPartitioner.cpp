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
#include <OpenFrames/Utilities.hpp>
#include <osgUtil/CullVisitor>
#include <iostream>
#include <iomanip>

// DepthPartitioner camera name components
static const std::string dpCamNamePrefix("DPCam");
static const std::string dpMainCamName(dpCamNamePrefix+"Main");

namespace OpenFrames
{

  /**********************************************/
  DepthPartitioner::DepthPartitioner()
  : _view(NULL)
  {
    // Create main slave Camera to manage the depth partitioner
    // Note this camera won't have any scene of its own, it just analyzes the scene
    // and creates other slave cameras as needed
    _dpMainSlaveCamera = new osg::Camera();
    _dpMainSlaveCamera->setName(dpMainCamName);
    
    // Disable main slave camera's scene traversal
    _dpMainSlaveCamera->setNodeMask(0x0);
    
    // Allow main slave camera to recieve events. Needed since master camera won't
    // recieve events after its graphics context is detached
    _dpMainSlaveCamera->setAllowEventFocus(true);
    
    // We will set the projection matrix ourselves. Needed since master camera
    // won't be resized once its graphics context is detached
    _dpMainSlaveCamera->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
    
    // Create slave callback that will perform depth partitioning
    _dpCallback = new DepthPartitionCallback;
  }

  /**********************************************/
  bool DepthPartitioner::setViewToPartition(osgViewer::View *view)
  {
    if(_view == view) return true; // We are already partitioning the View
    
    // Make sure another DepthPartitioner isn't already partitioning the new View
    if(view != NULL)
    {
      // Loop through all slaves, and check whether one of them already has a DepthPartitionCallback
      unsigned int numSlaves = view->getNumSlaves();
      for(unsigned int i = 0; i < numSlaves; ++i)
      {
        // Check if slave's update callback is a DepthPartitionCallback
        DepthPartitionCallback *dpcb = dynamic_cast<DepthPartitionCallback*>(view->getSlave(i)._updateSlaveCallback.get());
        if(dpcb != NULL)
        {
          std::cerr<< "OpenFrames::DepthPartitioner ERROR: View already has an attached DepthPartitioner." << std::endl;
          return false;
        }
      }
    }
    
    // Remove all depth partitioning objects from current View
    if(_view != NULL)
    {
      // Reattach the current master camera graphics context and viewport
      if(_view->getCamera()->getGraphicsContext() == NULL)
      {
        _view->getCamera()->setGraphicsContext(_dpMainSlaveCamera->getGraphicsContext());
        _view->getCamera()->setViewport(_dpMainSlaveCamera->getViewport());
        _dpMainSlaveCamera->setGraphicsContext(NULL);
        _dpMainSlaveCamera->setViewport(NULL);
      }
      
      // Detach our main slave Camera from current View
      // This also removes the DepthPartitionCallback
      _view->removeSlave(_view->findSlaveIndexForCamera(_dpMainSlaveCamera));
      
      // Detach depth partition slave cameras from current View
      _dpCallback->reset();
    }
    
    _view = view; // Set new View
    
    // Enable depth partitioning for new View
    if(_view != NULL)
    {
      // Add main slave camera, don't use master scene data
      _view->addSlave(_dpMainSlaveCamera, false);
      
      // Add DepthPartitionCallback as a slave update callback
      osg::View::Slave *slave = _view->findSlaveForCamera(_dpMainSlaveCamera);
      slave->_updateSlaveCallback = _dpCallback;
      
      // The new master camera's graphics context and viewport will be
      // automatically acquired by the DepthPartitionCallback
    }
    
    return true;
  }
  
  /**********************************************/
  DepthPartitioner::~DepthPartitioner()
  {
    setViewToPartition(NULL);
  }
  
  /**********************************************/
  /** Creates osg::Cameras and adds them as slaves to the main osg::View */
  struct BasicCameraManager : public DepthPartitionCallback::CameraManager
  {
    BasicCameraManager() {}
    virtual ~BasicCameraManager() { reset(); }
    
    virtual std::string getCameraName(unsigned int camNum)
    {
      if(camNum < _cameraList.size()) return _cameraList[camNum]->getName();
      else return "Invalid Camera Number";
    }
    
    // Create a new Camera if needed, and add it as a slave
    virtual void enableCamera(unsigned int camNum,
                              osg::Camera* mainCam,
                              const double &zNear, const double &zFar)
    {
      if(_cameraList.size() <= camNum) _cameraList.resize(camNum+1);
      osg::Camera *newcam = _cameraList[camNum].get();
      
      if(!newcam) // Create a new Camera
      {
        newcam = new osg::Camera();
        newcam->setCullingActive(false);
        newcam->setAllowEventFocus(false);
        newcam->setRenderOrder(osg::Camera::POST_RENDER, camNum);
        newcam->setName(dpCamNamePrefix + std::to_string(camNum));
        newcam->setGraphicsContext(mainCam->getGraphicsContext());
        newcam->setViewport(mainCam->getViewport());
        
        // We will manage the projection matrix ourselves
        newcam->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
        newcam->setProjectionResizePolicy(osg::Camera::FIXED);
        newcam->setComputeNearFarMode(osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR);
        
        // First camera gets clear mask from main camera
        if (camNum == 0)
          newcam->setClearMask(mainCam->getClearMask());
        else // Remaining cameras only clear depth buffer
          newcam->setClearMask(GL_DEPTH_BUFFER_BIT);
        
        // Add Camera as slave, and tell it to use the master Camera's scene
        mainCam->getView()->addSlave(newcam, true);
        
        // Store new camera in internal camera list
        _cameraList[camNum] = newcam;
      }
      
      // Update projection depth planes
      osg::Matrixd projmat = mainCam->getProjectionMatrix();
      OpenFrames::updateProjectionMatrix(projmat, zNear, zFar);
      
      // Set camera rendering matrices
      newcam->setProjectionMatrix(projmat);
      newcam->setViewMatrix(mainCam->getViewMatrix());
      
      // Activate camera
      newcam->setNodeMask(0xffffffff);
    }
    
    // Disable all cameras starting with the specified index
    virtual void disableCameras(unsigned int start)
    {
      for(int i = start; i < _cameraList.size(); ++i)
      {
        _cameraList[i]->setNodeMask(0x0);
      }
    }
    
    // Detach all our cameras from the main scene, then erase the cameras
    virtual void reset()
    {
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
    
    // Cameras that should be used to draw the scene. These cameras
    // will be reused on every frame in order to save time.
    typedef std::vector< osg::ref_ptr<osg::Camera> > CameraList;
    CameraList _cameraList;
  };
  
  /**********************************************/
  DepthPartitionCallback::DepthPartitionCallback()
  : _numActiveCameras(0)
  {
    _distAccumulator = new DistanceAccumulator;
    _cameraManager = new BasicCameraManager;
  }
  
  DepthPartitionCallback::~DepthPartitionCallback()
  {
    reset();
  }
  
  /**********************************************/
  void DepthPartitionCallback::setCameraManager(CameraManager *cameraManager)
  {
    if(cameraManager == NULL)
      _cameraManager = new BasicCameraManager;
    else
      _cameraManager = cameraManager;
  }
  
  /**********************************************/
  void DepthPartitionCallback::updateSlave(osg::View& view, osg::View::Slave& slave)
  {
    // If the scene hasn't been defined then don't do anything
    osgViewer::View *sceneView = dynamic_cast<osgViewer::View*>(&view);
    if(!sceneView || !sceneView->getSceneData()) return;
    
    // Capture the master camera's graphics context
    osg::Camera *masterCam = view.getCamera();
    osg::Camera *dpMainSlaveCam = slave._camera;
    if(masterCam->getGraphicsContext())
    {
      if(dpMainSlaveCam->getGraphicsContext())
      {
        std::cerr<< "OpenFrames::DepthPartitionCallback::updateSlave ERROR: Graphics context already exists and cannot be changed." << std::endl;
        return;
      }
      
      // Copy graphics context and viewport to main slave camera, so that it
      // can recieve events instead of the disabled master camera.
      dpMainSlaveCam->setGraphicsContext(masterCam->getGraphicsContext());
      dpMainSlaveCam->setViewport(masterCam->getViewport());
      
      // Disable master camera by detaching its graphics context, which will be
      // reattached when the DepthPartitioner is moved to another View
      masterCam->setGraphicsContext(NULL);
      masterCam->setViewport(NULL);
    }
    
    // Make sure there is a valid graphics context
    osg::GraphicsContext *gc = dpMainSlaveCam->getGraphicsContext();
    if(gc == NULL)
    {
      std::cerr<< "OpenFrames::DepthPartitionCallback::updateSlave ERROR: No valid Graphics Context!" << std::endl;
      return;
    }
    
    // Get the master camera's view matrix and cull settings
    // The projection matrix is automatically adjusted by OSG
    dpMainSlaveCam->setViewMatrix(masterCam->getViewMatrix());
    slave.updateSlaveImplementation(view);
    
    // Prepare for scene traversal
    _distAccumulator->setMatrices(dpMainSlaveCam->getViewMatrix(), dpMainSlaveCam->getProjectionMatrix());
    _distAccumulator->setNearFarRatio(dpMainSlaveCam->getNearFarRatio());
    _distAccumulator->setMinZNear(_cameraManager->getMinZNear());
    _distAccumulator->reset();
    
    // Step 1: Traverse the scene, collecting near/far distances.
    sceneView->getSceneData()->accept(*(_distAccumulator.get()));
    
    // Step 2: Compute the near and far distances for each Camera that
    // should be used to render the scene
    _distAccumulator->computeCameraPairs();
    DistanceAccumulator::PairList& camPairs = _distAccumulator->getCameraPairs();
    
    // Step 3: Create the slave Cameras that will draw each depth segment
    unsigned int numCameras = camPairs.size(); // Get the number of cameras
    if(numCameras != _numActiveCameras)
    {
      _numActiveCameras = numCameras;
      //std::cout<< "OpenFrames::DepthPartitionCallback using " << _numActiveCameras << " cameras" << std::endl;
    }
    for(unsigned int i = 0; i < numCameras; ++i)
    {
      // Create a new camera if needed, and activate it
      // Note that we slightly extend the far plane to get rid of the "seam" between depth segments
      // Extending the far plane does not really degrade depth precision
      _cameraManager->enableCamera(i, dpMainSlaveCam, camPairs[i].first, camPairs[i].second*1.002);
      //std::cout<< std::defaultfloat << std::setprecision(5) << "Camera " << _cameraManager->getCameraName(i) << " near = " << camPairs[i].first << ", far = " << camPairs[i].second << std::endl;
    }
    
    // Step 4: Disable remaining unused cameras
    _cameraManager->disableCameras(numCameras);
  }
  
} // OpenFrames namespace
