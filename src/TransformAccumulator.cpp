/***********************************
   Copyright 2018 Ravishankar Mathur

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

/** \file TransformAccumulator.cpp
 * TransformVisitor- and TransformAccumulator-classes function definitions.
 */

#include <OpenFrames/TransformAccumulator.hpp>
#include <OpenFrames/ReferenceFrame.hpp>
#include <OpenFrames/FrameTransform.hpp>
#include <OpenFrames/DescendantTracker.hpp>
#include <osg/Matrix>

namespace OpenFrames {

TransformVisitor::TransformVisitor()
	: NodeVisitor()
{
	  _coordMode = LOCAL_TO_WORLD;
}

TransformVisitor::~TransformVisitor() {}

void TransformVisitor::apply(osg::Transform& transform)
{
	  if( _coordMode == LOCAL_TO_WORLD )
	    transform.computeLocalToWorldMatrix(_matrix, this);
	  else // WORLD_TO_LOCAL
	    transform.computeWorldToLocalMatrix(_matrix, this);
}

void TransformVisitor::accumulate(const FramePath &framePath)
{
	  _matrix.makeIdentity();

	  for( _itr = framePath.begin(); _itr != framePath.end(); ++_itr )
	    (*_itr)->getTransform()->accept(*this);
}

TransformAccumulator::TransformAccumulator() { _init(); }

TransformAccumulator::TransformAccumulator( ReferenceFrame* root, ReferenceFrame* frame )
{
	_init();
	_lookAtPath->setRoot(root);
	_lookAtPath->trackDescendant(frame);
}

void TransformAccumulator::_init()
{
	_lookAtPath = new DescendantTracker;
	_transformVisitor = new TransformVisitor;
}
	
TransformAccumulator::~TransformAccumulator() {}

/** Get/Set the root frame */
ReferenceFrame* TransformAccumulator::getRoot()
{
	return _lookAtPath->getRoot();
}

void TransformAccumulator::setRoot( ReferenceFrame* root )
{
	_lookAtPath->setRoot(root);
}

/** Get/Set the origin frame */
ReferenceFrame* TransformAccumulator::getOrigin()
{
	return _lookAtPath->getTrackedDescendant();
}

bool TransformAccumulator::setOrigin( ReferenceFrame* frame )
{
	return _lookAtPath->trackDescendant(frame);
}

bool TransformAccumulator::isValid()
{
	return _lookAtPath->isTrackingDescendant();
}

/** Get the transform from the origin frame to its root frame */
osg::Matrixd& TransformAccumulator::getLocalToWorld()
{
	_transformVisitor->_coordMode = TransformVisitor::LOCAL_TO_WORLD;
	_transformVisitor->accumulate(_lookAtPath->getFramePath());

	return _transformVisitor->_matrix;
}

/** Get the transform from the root frame to the origin frame */
osg::Matrixd& TransformAccumulator::getWorldToLocal()
{
	_transformVisitor->_coordMode = TransformVisitor::WORLD_TO_LOCAL;
	_transformVisitor->accumulate(_lookAtPath->getFramePath());

	return _transformVisitor->_matrix;
}

} // !namespace OpenFrames
