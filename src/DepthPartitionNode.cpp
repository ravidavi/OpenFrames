/***********************************
   Copyright 2013 Ravishankar Mathur

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

#include <OpenFrames/DepthPartitionNode>
#include <osgUtil/CullVisitor>

namespace OpenFrames
{

DepthPartitionNode::DepthPartitionNode()
{
	_distAccumulator = new DistanceAccumulator;
	init();
}

DepthPartitionNode::~DepthPartitionNode() {}

void DepthPartitionNode::init()
{
	_active = true;
	_numCameras = 0;
	setCullingActive(false);
	_renderOrder = osg::Camera::POST_RENDER;
	_clearColorBuffer = true;
}

void DepthPartitionNode::setActive(bool active)
{
	if(_active == active) return;
	_active = active;
}

void DepthPartitionNode::setClearColorBuffer(bool clear)
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

void DepthPartitionNode::setRenderOrder(osg::Camera::RenderOrder order)
{
	_renderOrder = order;

	// Update the render order for existing Cameras
	for(unsigned int i = 0; i < _cameraList.size(); ++i)
	{
	  _cameraList[i]->setRenderOrder(_renderOrder);
	}
}

void DepthPartitionNode::traverse(osg::NodeVisitor &nv)
{
	// If the scene hasn't been defined then don't do anything
	unsigned int numChildren = _children.size();
	if(numChildren == 0) return;

	// If the node is not active then don't analyze it
	if(!_active)
	{
	  // Traverse the graph as usual, by calling the parent class osg::Group's traverse method.
	  Group::traverse(nv);
	  return;
	}

	// If the visitor is not a cull visitor, pass it directly onto the scene.
	osgUtil::CullVisitor* cv = dynamic_cast<osgUtil::CullVisitor*>(&nv);
	if(!cv) 
	{ 
	  Group::traverse(nv);
	  return; 
	}

	// We are in the cull traversal, so first collect information on the
	// current modelview and projection matrices and viewport.
	osg::RefMatrix *modelview = cv->getModelViewMatrix();
	osg::RefMatrix *projection = cv->getProjectionMatrix();
	osg::Viewport* viewport = cv->getViewport();

	// Prepare for scene traversal.
	_distAccumulator->setMatrices(*modelview, *projection);
	_distAccumulator->setNearFarRatio(cv->getNearFarRatio());
	_distAccumulator->reset();

	// Step 1: Traverse the children, collecting the near/far distances.
	unsigned int i;
	for(i = 0; i < numChildren; ++i)
	{
	  _children[i]->accept(*(_distAccumulator.get()));
	}

	// Step 2: Compute the near and far distances for every Camera that
	// should be used to render the scene.
	_distAccumulator->computeCameraPairs();
	DistanceAccumulator::PairList& camPairs = _distAccumulator->getCameraPairs();

	// Step 3: Create the Cameras, and add them as children.
	_numCameras = camPairs.size(); // Get the number of cameras
	if(_numCameras > 0)
	{
	  osg::Camera *currCam;
	  DistanceAccumulator::DistancePair currPair;

	  for(i = 0; i < _numCameras; ++i)
	  {
	    // Create the camera, and clamp it's projection matrix
	    currPair = camPairs[i];  // (near,far) pair for current camera
	    currCam = createOrReuseCamera(*projection, currPair.first, 
	                                  currPair.second, i);

	    // Set the modelview matrix and viewport of the camera
	    currCam->setViewMatrix(*modelview);
	    currCam->setViewport(viewport);

	    // Redirect the CullVisitor to the current camera
	    currCam->accept(nv);
	  }

	  // Set the clear color for the first camera
	  _cameraList[0]->setClearColor(cv->getRenderStage()->getClearColor());
	}
}

bool DepthPartitionNode::addChild(osg::Node *child)
{
	return insertChild(_children.size(), child);
}

bool DepthPartitionNode::insertChild(unsigned int index, osg::Node *child)
{
	if(!Group::insertChild(index, child)) return false; // Insert child

	// Insert child into each Camera
	unsigned int totalCameras = _cameraList.size();
	for(unsigned int i = 0; i < totalCameras; ++i)
	{
	  _cameraList[i]->insertChild(index, child);
	}
	return true;
}

bool DepthPartitionNode::removeChild(osg::Node *child)
{
	return Group::removeChild(child);
}

bool DepthPartitionNode::removeChild(unsigned int pos, unsigned int numRemove)
{
	if(!Group::removeChild(pos, numRemove)) return false; // Remove child

	// Remove child from each Camera
	unsigned int totalCameras = _cameraList.size();
	for(unsigned int i = 0; i < totalCameras; ++i)
	{
	  _cameraList[i]->removeChild(pos, numRemove);
	}
	return true;
}

bool DepthPartitionNode::setChild(unsigned int i, osg::Node *node)
{
	if(!Group::setChild(i, node)) return false; // Set child

	// Set child for each Camera
	unsigned int totalCameras = _cameraList.size();
	for(unsigned int i = 0; i < totalCameras; ++i)
	{
	  _cameraList[i]->setChild(i, node);
	}
	return true;
}

osg::Camera* DepthPartitionNode::createOrReuseCamera(const osg::Matrix& proj, 
                            double near, double far, const unsigned int &camNum)
{
	if(_cameraList.size() <= camNum) _cameraList.resize(camNum+1);
	osg::Camera *camera = _cameraList[camNum].get();
	
	if(!camera) // Create a new Camera
	{
	  camera = new osg::Camera;
	  camera->setCullingActive(false);
	  camera->setCullingMode(osg::CullSettings::ENABLE_ALL_CULLING & ~osg::CullSettings::SMALL_FEATURE_CULLING);
	  camera->setRenderOrder(_renderOrder);
	  camera->setReferenceFrame(osg::Transform::ABSOLUTE_RF);

	  // We will compute the near/far planes ourselves
	  camera->setComputeNearFarMode(osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR);

	  if(camNum == 0 && _clearColorBuffer)
	    camera->setClearMask(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	  else
	    camera->setClearMask(GL_DEPTH_BUFFER_BIT);

	  // Add our children to the new Camera's children
	  unsigned int numChildren = _children.size();
	  for(unsigned int i = 0; i < numChildren; ++i)
	  {
	    camera->addChild(_children[i].get());
	  }

	  _cameraList[camNum] = camera;
	}

	osg::Matrixd &projection = camera->getProjectionMatrix();
	projection = proj;

	// Slightly inflate the near & far planes to avoid objects at the
	// extremes being clipped out.
	near *= 0.999;
	far *= 1.001;

	// Clamp the projection matrix z values to the range (near, far)
	double epsilon = 1.0e-6;
	if(fabs(projection(0,3)) < epsilon &&
	   fabs(projection(1,3)) < epsilon &&
	   fabs(projection(2,3)) < epsilon ) // Projection is Orthographic
	{
	  epsilon = -1.0/(far - near); // Used as a temp variable
	  projection(2,2) = 2.0*epsilon;
	  projection(3,2) = (far + near)*epsilon;
	}
	else // Projection is Perspective
	{
	  double trans_near = (-near*projection(2,2) + projection(3,2)) /
	                      (-near*projection(2,3) + projection(3,3));
	  double trans_far = (-far*projection(2,2) + projection(3,2)) /
	                     (-far*projection(2,3) + projection(3,3));
	  double ratio = fabs(2.0/(trans_near - trans_far));
	  double center = -0.5*(trans_near + trans_far);

	  projection.postMult(osg::Matrixd(1.0, 0.0, 0.0, 0.0,
	                                   0.0, 1.0, 0.0, 0.0,
	 			           0.0, 0.0, ratio, 0.0,
					   0.0, 0.0, center*ratio, 1.0));
	}

	return camera;
}

}
