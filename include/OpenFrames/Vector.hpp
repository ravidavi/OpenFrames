/***********************************
   Copyright 2023 Ravishankar Mathur

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

/** \file Vector.hpp
 * Declaration of Vector class.
 */

#ifndef _OF_VECTOR_
#define _OF_VECTOR_

#include <OpenFrames/Export.h>

#include <osg/ShapeDrawable>
#include <osg/Referenced>
#include <osg/ref_ptr>

namespace OpenFrames
{
  /**
   * \class Vector
   *
   * \brief This class defines an actual drawable Vector of variable geometry.
   *
   * This class computes the transformation of a ReferenceFrame with respect to any
   * of its ancestor frames. To use, just specify the root frame and the target
   * frame then call the getLocalToWorld() or getWorldToLocal() method. The
   * accumulated transform between root & target frames is returned.
   */
  class OF_EXPORT Vector : public osg::Referenced
  {
  public:
	Vector( const osg::Vec3d& vec );

	/** Set/get the length of the vector body and head */
	void setLength( const double &body, const double &head );
	void getLength( double &body, double &head) const;
	double getTotalLength() const;

	/** Set/get the radius of the vector body and head */
	void setRadius( const double &body, const double &head );
	void getRadius( double &body, double &head) const;

	/** Set/get the starting (base) position of the vector */
	void setBasePosition( const osg::Vec3d& b );
	inline const osg::Vec3d& getBasePosition() const { return _basepos; }

	/** Set/get the direction that the vector points towards */
	void setDirection( const osg::Vec3d& d );
	inline const osg::Vec3d& getDirection() const { return _dir; }

	/** Get the actual OSG ShapeDrawable (a Drawable) for the vector */
	inline osg::ShapeDrawable* getVector() const { return _vec.get(); }

	void reposition();

  protected:
	virtual ~Vector();

	osg::ref_ptr<osg::Cylinder> _body;
	osg::ref_ptr<osg::Cone> _head;
	osg::ref_ptr<osg::ShapeDrawable> _vec;

	osg::Vec3d _basepos; // Vector base position
	osg::Vec3d _dir;     // Vector direction
  };

} // ! namespace OpenFrames

#endif // ! _OF_VECTOR_
