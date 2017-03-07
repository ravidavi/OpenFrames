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
#include <OpenFrames/VRUtils.hpp>
#include <osg/Camera>
#include <osgViewer/View>

namespace OpenFrames
{
  class DepthPartitionCallback;
  
  /**********************************************************
   * Ravi Mathur
   * OpenFrames API, class DepthPartitioner
   * Sets up depth partitioning on an osgViewer::View
   * Note that an instance of this class can only be attached to one
   * View at a time.
   **********************************************************/
  class OF_EXPORT DepthPartitioner : public osg::Referenced
  {
  public:
    DepthPartitioner(osgViewer::View *view = NULL, VRTextureBuffer *texBuffer = NULL);
    
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
    friend class DepthPartitioner;
    
  public:
    DepthPartitionCallback(VRTextureBuffer *texBuffer);
    
    /** Specify whether the color buffer should be cleared before the first Camera draws its scene. */
    void setClearColorBuffer(bool clear);
    inline bool getClearColorBuffer() const { return _clearColorBuffer; }
    
    /** Set/get the maximum scene traversal depth, defaults to UINT_MAX */
    void setMaxTraversalDepth(unsigned int depth)
    { _distAccumulator->setMaxDepth(depth); }
    
    inline unsigned int getMaxTraversalDepth() const
    { return _distAccumulator->getMaxDepth(); }
    
    /** Define the callback function */
    virtual void updateSlave(osg::View& view, osg::View::Slave& slave);
    
  protected:
    ~DepthPartitionCallback();
    
    /** Remove depth partitioner from current View */
    void reset();
    
    // Updates a projection matrix with specified near/far plane
    void updateProjectionMatrix(osg::Matrix& proj,
                                double near, double far);
    
    // Creates a new Camera object with default settings
    void createOrReuseCamera(unsigned int camNum,
                             osg::Camera* masterCamera, double &zNear, double &zFar);
    void createOrReuseVRCamera(unsigned int camNum,
                               osg::Camera* masterCamera, double &zNear, double &zFar);
    
    // The NodeVisitor that computes depth partitions for the scene
    osg::ref_ptr<DistanceAccumulator> _distAccumulator;
    
    bool _clearColorBuffer;
    
    // Cameras that should be used to draw the scene. These cameras
    // will be reused on every frame in order to save time.
    typedef std::vector< osg::ref_ptr<osg::Camera> > CameraList;
    CameraList _cameraList;
    
    typedef std::vector< osg::ref_ptr<VRCamera> > VRCameraList;
    VRCameraList _vrCameraList;
    
    osg::ref_ptr<VRTextureBuffer> _texBuffer;
  };
  
} // !namespace OpenFrames

#endif
