/***********************************
   Copyright 2020 Ravishankar Mathur

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

/** \file TransformAccumulator.hpp
 * Declaration of TransformAccumulator class.
 */

#ifndef _OF_TRANSFORMACCUMULATOR_
#define _OF_TRANSFORMACCUMULATOR_

#include <OpenFrames/Export.h>
#include <OpenFrames/DescendantTracker.hpp>

#include <osg/Matrix>
#include <osg/NodeVisitor>
#include <osg/Referenced>
#include <osg/ref_ptr>

namespace OpenFrames
{
  class ReferenceFrame;

  /**
   * \class TransformVisitor
   *
   * \brief Accumulates the transform along a path.
   *
   * This NodeVisitor accumulates the transform along a given path
   * of ReferenceFrames.
   */
  class OF_EXPORT TransformVisitor : public osg::NodeVisitor
  {
  public:
	enum CoordMode
	{
	  WORLD_TO_LOCAL,
	  LOCAL_TO_WORLD
	};

	TransformVisitor();

	virtual void apply(osg::Transform& transform);
	void accumulate(const FramePath &framePath);

	CoordMode _coordMode;
	osg::Matrixd _matrix;

  protected:
	virtual ~TransformVisitor();

	FramePath::const_iterator _itr;
  };

  /**
   * \class TransformAccumulator
   *
   * \brief Computes the transformation of a ReferenceFrame wrt an ancestor.
   *
   * This class computes the transformation of a ReferenceFrame with respect to any
   * of its ancestor frames. To use, just specify the root frame and the target
   * frame then call the getLocalToWorld() or getWorldToLocal() method. The
   * accumulated transform between root & target frames is returned.
   */
  class OF_EXPORT TransformAccumulator : public osg::Referenced
  {
  public:
	TransformAccumulator();
	TransformAccumulator( ReferenceFrame* root, ReferenceFrame* frame );

	/** Get/Set the root frame */	
	ReferenceFrame* getRoot();
	void setRoot( ReferenceFrame* root );

	/** Get/Set the origin frame */
	ReferenceFrame* getOrigin();
	bool setOrigin( ReferenceFrame* frame );

	/** Check if the transform from root to origin exists. */
	bool isValid();

	/** Get the transform from origin to its root frame */
	osg::Matrixd& getLocalToWorld();

	/** Get the transform from the root to origin */
	osg::Matrixd& getWorldToLocal();

  protected:
	virtual ~TransformAccumulator();
	void _init();

	  // FramePath from the root to the origin frame
	osg::ref_ptr<DescendantTracker> _lookAtPath;

	  // NodeVisitor that computes the transform from root to origin
	osg::ref_ptr<TransformVisitor> _transformVisitor;
  }; // !class TransformAccumulator

}  // !namespace OpenFrames

#endif  // !_OF_TRANSFORMACCUMULATOR_
