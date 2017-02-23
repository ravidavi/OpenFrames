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

#ifndef _OF_DEPTHPARTITIONNODE_
#define _OF_DEPTHPARTITIONNODE_

#include <OpenFrames/Export.h>
#include <OpenFrames/DistanceAccumulator.hpp>
#include <osg/Camera>

namespace OpenFrames
{

/**********************************************************
 * Ravi Mathur
 * OpenFrames API, class DepthPartitionNode
 * A type of osg::Group that analyzes a scene, then partitions it into
 * several segments that can be rendered separately. Each segment
 * is small enough in the z-direction to avoid depth buffer problems
 * for very large scenes.
**********************************************************/
class OF_EXPORT DepthPartitionNode : public osg::Group
{
  public:
	DepthPartitionNode();

	// Don't allow copying from another DPN
	DepthPartitionNode(const DepthPartitionNode& dpn, 
	              const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY){}

	META_Node( OpenFrames, DepthPartitionNode ); // Common Node functions

	/** Set the active state. If not active, this node will simply add the
	    specified scene as its child, without analyzing it at all. */
	void setActive(bool active);
	inline bool getActive() const { return _active; }

	/** Specify whether the color buffer should be cleared before the first
	    Camera draws its scene. */
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

	/** Override the cull traversal */
	virtual void traverse(osg::NodeVisitor &nv);

	/** Catch child management functions so the Cameras can be informed
	    of added or removed children. */
	virtual bool addChild(osg::Node *child);
	virtual bool insertChild(unsigned int index, osg::Node *child);
	virtual bool removeChild(osg::Node *child);
	virtual bool removeChild(unsigned int pos, unsigned int numRemove = 1);
	virtual bool setChild(unsigned int i, osg::Node *node);

  protected:
	typedef std::vector< osg::ref_ptr<osg::Camera> > CameraList;

	~DepthPartitionNode();

	void init();
  
  // Updates a projection matrix with specified near/far plane
  void updateProjectionMatrix(osg::Matrix& proj,
                              double near, double far);

	// Creates a new Camera object with default settings
	osg::Camera* createOrReuseCamera(unsigned int camNum,
                                   const osg::Camera* parentCamera);

	bool _active; // Whether partitioning is active on the scene

	// The NodeVisitor that computes cameras for the scene
	osg::ref_ptr<DistanceAccumulator> _distAccumulator;

	osg::Camera::RenderOrder _renderOrder;
	bool _clearColorBuffer;

	// Cameras that should be used to draw the scene.  These cameras
	// will be reused on every frame in order to save time and memory.
	CameraList _cameraList;
	unsigned int _numCameras; // Number of Cameras actually being used
};

}

#endif
