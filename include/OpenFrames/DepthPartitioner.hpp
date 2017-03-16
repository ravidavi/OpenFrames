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

#ifndef _OF_DEPTHPARTITIONER_
#define _OF_DEPTHPARTITIONER_

#include <OpenFrames/Export.h>
#include <OpenFrames/DistanceAccumulator.hpp>
#include <osg/Camera>
#include <osgViewer/View>

namespace OpenFrames
{
  class DepthPartitionCallback;

  /** OpenFrames function that updates a projection matrix with specified near/far plane */
  void updateProjectionMatrix(osg::Matrix& proj, const double &zNear, const double &zfar);

  /**********************************************************
   * Ravi Mathur
   * OpenFrames API, class DepthPartitioner
   * Sets up depth partitioning on an osgViewer::View
   * Note that an instance of this class can only be attached to one
   * View at a time.
   **********************************************************/
  class OF_EXPORT DepthPartitioner : public virtual osg::Referenced
  {
  public:
    DepthPartitioner();
    
    /** Specify which View to partition */
    bool setViewToPartition(osgViewer::View *view);
    osgViewer::View* getViewToPartition() { return _view; }
    
    /** Get the update callback that does the actual partitioning */
    DepthPartitionCallback* getCallback() { return _dpCallback; }
    
  protected:
    ~DepthPartitioner();
    
    osg::ref_ptr<osg::Camera> _dpMainSlaveCamera;
    osg::ref_ptr<DepthPartitionCallback> _dpCallback;
    osgViewer::View *_view;
  };

  /**********************************************************
   * Ravi Mathur
   * OpenFrames API, class DepthPartitionCallback
   * Analyzes a scene, then partitions it into several segments that can
   * be rendered separately. Each segment is small enough in the
   * z-direction to avoid depth buffer problems for very large scenes.
   * Note this must be a slave update callback, since normal update callbacks
   * are called before the camera manipulator (e.g. trackball) is applied.
   **********************************************************/
  class OF_EXPORT DepthPartitionCallback : public osg::View::Slave::UpdateSlaveCallback
  {
  public:
    DepthPartitionCallback();
    
    /** Remove all internal slave cameras from current View */
    void reset() { _cameraManager->reset(); }
    
    /** Specify whether the color buffer should be cleared before the first Camera draws its scene. */
    void setClearColorBuffer(bool clear) { _cameraManager->setClearColorBuffer(clear); }
    
    /** Set/get the maximum scene traversal depth, defaults to UINT_MAX */
    void setMaxTraversalDepth(unsigned int depth)
    { _distAccumulator->setMaxDepth(depth); }
    
    unsigned int getMaxTraversalDepth() const
    { return _distAccumulator->getMaxDepth(); }
    
    /** Define the callback function */
    virtual void updateSlave(osg::View& view, osg::View::Slave& slave);
    
    /** Manage cameras for the depth partition callback. */
    struct CameraManager : public virtual osg::Referenced
    {
      CameraManager() : _clearColorBuffer(true) {}
      virtual ~CameraManager() {}
      
      // Get the internal name of a camera
      virtual std::string getCameraName(unsigned int camNum) = 0;
      
      // Create a new camera and add it as a slave to the given camera
      virtual void enableCamera(unsigned int camNum,
                                osg::GraphicsContext* gc,
                                osg::Camera* masterCamera,
                                const double &zNear, const double &zFar) = 0;
      
      // Disable all cameras after specified start camera number
      virtual void disableCameras(unsigned int start) = 0;
      
      // Clear all internal cameras and revert the CameraManger to an unused
      // and empty state
      virtual void reset() = 0;
      
      // Have first camera clear the color buffer
      virtual void setClearColorBuffer(bool clear) { _clearColorBuffer = clear; }
      bool _clearColorBuffer;
    };
    
    /** Specify a new camera manager. NULL means reset to basic camera manager */
    void setCameraManager(CameraManager *cameraManager);
    
    /** Specify a callback used to perform user-defined work */
    void setUpdateCallback(osg::View::Slave::UpdateSlaveCallback *updateCallback)
    { _updateCallback = updateCallback; }
    
  protected:
    virtual ~DepthPartitionCallback();
    
    // The NodeVisitor that computes depth partitions for the scene
    osg::ref_ptr<DistanceAccumulator> _distAccumulator;
    
    // The camera manager that creates cameras
    osg::ref_ptr<CameraManager> _cameraManager;
    
    // Callback that performs user-defined work
    osg::ref_ptr<osg::View::Slave::UpdateSlaveCallback> _updateCallback;
    
    unsigned int _numActiveCameras;
  };
  
} // !namespace OpenFrames

#endif
