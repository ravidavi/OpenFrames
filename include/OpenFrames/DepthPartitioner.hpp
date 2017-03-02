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
    DepthPartitioner(osgViewer::View *view = NULL);
    
    /** Specify which View to partition */
    void setViewToPartition(osgViewer::View *view);
    
    /** Set the GraphicsContext to use for depth partitioning Cameras */
    void setGraphicsContext(osg::GraphicsContext *gc);
    
    /** Set the Viewport to use for depth partitioning Cameras */
    void setViewport(int x, int y, int w, int h);
    
    /** Set whether the color buffer should be cleared by the first Camera */
    void setClearColorBuffer(bool clear);
    
  protected:
    ~DepthPartitioner();
    
    osg::ref_ptr<osg::Camera> _dpMainSlaveCamera;
    osg::ref_ptr<DepthPartitionCallback> _dpCallback;
    osg::ref_ptr<osgViewer::View> _view;
  };
  
  /**********************************************************
   * Ravi Mathur
   * OpenFrames API, class DepthPartitionCallback
   * Analyzes a scene, then partitions it into several segments that can
   * be rendered separately. Each segment is small enough in the
   * z-direction to avoid depth buffer problems for very large scenes.
   **********************************************************/
  class OF_EXPORT DepthPartitionCallback : public osg::View::Slave::UpdateSlaveCallback
  {
  public:
    DepthPartitionCallback();
    
    /** Set the active state. If not active, no depth partitioning will be performed. */
    void setActive(bool active);
    inline bool getActive() const { return _active; }
    
    /** Remove depth partitioner from current View */
    void reset();
    
    /** Specify whether the color buffer should be cleared before the first Camera draws its scene. */
    void setClearColorBuffer(bool clear);
    inline bool getClearColorBuffer() const { return _clearColorBuffer; }
    
    /** Specify the render order for each Camera */
    void setRenderOrder(osg::Camera::RenderOrder order);
    inline osg::Camera::RenderOrder getRenderOrder() const
    { return _renderOrder; }
    
    /** Set/get the maximum scene traversal depth, defaults to UINT_MAX */
    void setMaxTraversalDepth(unsigned int depth)
    { _distAccumulator->setMaxDepth(depth); }
    
    inline unsigned int getMaxTraversalDepth() const
    { return _distAccumulator->getMaxDepth(); }
    
    /** Define the callback function */
    virtual void updateSlave(osg::View& view, osg::View::Slave& slave);
    
  protected:
    ~DepthPartitionCallback();
    
    // Updates a projection matrix with specified near/far plane
    void updateProjectionMatrix(osg::Matrix& proj,
                                double near, double far);
    
    // Creates a new Camera object with default settings
    osg::Camera* createOrReuseCamera(unsigned int camNum,
                                     osg::Camera* masterCamera);
    
    bool _active; // Whether partitioning is active on the scene
    
    // The NodeVisitor that computes depth partitions for the scene
    osg::ref_ptr<DistanceAccumulator> _distAccumulator;
    
    osg::Camera::RenderOrder _renderOrder;
    bool _clearColorBuffer;
    
    // Cameras that should be used to draw the scene. These cameras
    // will be reused on every frame in order to save time.
    typedef std::vector< osg::ref_ptr<osg::Camera> > CameraList;
    CameraList _cameraList;
  };
  
} // !namespace OpenFrames

#endif
