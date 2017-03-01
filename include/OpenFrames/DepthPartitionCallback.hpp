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

#ifndef _OF_DEPTHPARTITIONCALLBACK_
#define _OF_DEPTHPARTITIONCALLBACK_

#include <OpenFrames/Export.h>
#include <OpenFrames/DistanceAccumulator.hpp>
#include <osg/Camera>
#include <osgViewer/View>

namespace OpenFrames
{
  
/**********************************************************
 * Ravi Mathur
 * OpenFrames API, class DepthPartitionCallback
 * Analyzes a scene, then partitions it into several segments that can
 * be rendered separately. Each segment is small enough in the
 * z-direction to avoid depth buffer problems for very large scenes.
 * To use, add as update callback to an osgViewer::View's master camera.
 * Note that once attached as a callback, an instance of this class
 * should NOT be used for other Camera's callbacks.
 **********************************************************/
class OF_EXPORT DepthPartitionCallback : public osg::Callback
{
public:
  DepthPartitionCallback();
  
  /** Set the active state. If not active, no depth partitioning will be performed. */
  void setActive(bool active);
  inline bool getActive() const { return _active; }
  
  /** Reset depth partitioner so that it can be added to another Camera */
  void reset();
  
  /** Specify whether the color buffer should be cleared before the first Camera draws its scene. */
  void setClearColorBuffer(bool clear);
  inline bool getClearColorBuffer() const { return _clearColorBuffer; }
  
  /** Specify the render order for each Camera */
  void setRenderOrder(osg::Camera::RenderOrder order);
  inline osg::Camera::RenderOrder getRenderOrder() const
  { return _renderOrder; }
  
  /** Set/get the maximum depth that the scene will be traversed to.
   Defaults to UINT_MAX. */
  void setMaxTraversalDepth(unsigned int depth)
  { _distAccumulator->setMaxDepth(depth); }
  
  inline unsigned int getMaxTraversalDepth() const
  { return _distAccumulator->getMaxDepth(); }
  
  /** Override the update traversal */
  virtual bool run(osg::Object* object, osg::Object* data);
  
protected:
  ~DepthPartitionCallback();
  
  // Updates a projection matrix with specified near/far plane
  void updateProjectionMatrix(osg::Matrix& proj,
                              double near, double far);
  
  // Creates a new Camera object with default settings
  osg::Camera* createOrReuseCamera(unsigned int camNum,
                                   osg::Camera* masterCamera);
  
  // Make sure provided camera is the original camera
  bool validateCamera(osg::Camera *cam);
  
  bool _active; // Whether partitioning is active on the scene
  
  // The NodeVisitor that computes depth partitions for the scene
  osg::ref_ptr<DistanceAccumulator> _distAccumulator;
  
  osg::Camera::RenderOrder _renderOrder;
  bool _clearColorBuffer;
  
  // Cameras that should be used to draw the scene. These cameras
  // will be reused on every frame in order to save time.
  typedef std::vector< osg::ref_ptr<osg::Camera> > CameraList;
  CameraList _cameraList;
  
  osg::ref_ptr<osgViewer::View> _view;
  osg::ref_ptr<osg::GraphicsContext> _gc;
  osg::ref_ptr<osg::Viewport> _viewport;
};

} // !namespace OpenFrames

#endif
