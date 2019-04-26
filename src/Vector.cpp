/***********************************
   Copyright 2019 Ravishankar Mathur

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

/** \file Vector.cpp
 * Vector-class function definitions.
 */

#include <OpenFrames/Vector.hpp>
#include <osg/ShapeDrawable>
#include <osg/BlendFunc>

namespace OpenFrames
{

/** Constructor.  Create a drawable vector that matches the given vector */
Vector::Vector( const osg::Vec3d& vec )
{
	  // A Vector is made of a cylinder with a cone at its end.
	_body = new osg::Cylinder;
	_head = new osg::Cone;

	  // Group the body & head together.
	osg::CompositeShape* shape = new osg::CompositeShape;
	shape->addChild(_body.get());
	shape->addChild(_head.get());

	  // Reduce the detail for the vector (don't need high detail) and
	  // leave out the face of the cylinder that connects to the cone.
	osg::TessellationHints* hints = new osg::TessellationHints;
	hints->setDetailRatio(1.0f);
	hints->setCreateTop(false);

	  // Create an actual drawable entity out of the vector
	_vec = new osg::ShapeDrawable(shape, hints);
  _vec->setUseDisplayList(false);
  _vec->setUseVertexBufferObjects(true);

	// Enable transparency and color blending
	osg::StateSet *ss = _vec->getOrCreateStateSet();
	osg::BlendFunc *bf = new osg::BlendFunc();
	bf->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA);
	ss->setAttributeAndModes(bf);
	ss->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);

	// Don't draw the insides of the vector (in case the camera happens
	// to be inside the vector looking out)
	ss->setMode(GL_CULL_FACE, osg::StateAttribute::ON);

	  // Set the length and direction of the vector to match that
	  // of the given vector.  By default this vector's starting
	  // point will be the origin.
	double len = vec.length();
	setLength(0.7*len, 0.3*len);
	setRadius(0.05*len, 0.1*len);
	setDirection(vec);
	setBasePosition(osg::Vec3d());
}

Vector::~Vector() { }

/** Set the length of the vector */
void Vector::setLength( const double &body, const double &head )
{
	if((body <= 0.0) || (head <= 0.0)) return;

	// Set up size of vector's body and head
	_body->setHeight(body);
	_head->setHeight(head);

	reposition(); // Reposition the vector since lengths have changed
}

void Vector::getLength( double &body, double &head ) const
{
	body = _body->getHeight();
	head = _head->getHeight();
}

double Vector::getTotalLength() const
{
	return (_body->getHeight() + _head->getHeight());
}

/** Set the radius of the vector */
void Vector::setRadius( const double &body, const double &head )
{
	if((body <= 0.0) || (head <= 0.0)) return;

	_body->setRadius(body);
	_head->setRadius(head);

	// We don't need to reposition the vector since none of the
	// vector's lengths have changed. But we do need to indicate
	// that the drawable has changed.
	_vec->dirtyBound();
  _vec->build();
}

void Vector::getRadius( double &body, double &head ) const
{
	body = _body->getRadius();
	head = _head->getRadius();
}

/** Set the starting position of the vector */
void Vector::setBasePosition( const osg::Vec3d& b )
{
	_basepos = b;
	reposition();
}

/** Set the direction that the vector points in */
void Vector::setDirection( const osg::Vec3d& d )
{
	if( d.isNaN() || (d.length() == 0.0) ) return;

	_dir = d;

	  // Note that here it is assumed that the default orientation
	  // of a cylinder or cone is aligned with the Z axis.
	osg::Quat rot;  
	rot.makeRotate( osg::Vec3d(0.0, 0.0, 1.0), _dir );
	_body->setRotation(rot);
	_head->setRotation(rot);

	reposition();  // Reposition the vector
}

void Vector::reposition()
{
	double bodylen = _body->getHeight();

	  // Must translate body & head such that base of body is
          // at base position and base of head is at end of body
	_body->setCenter(_basepos + _dir*bodylen*0.5);
	_head->setCenter(_basepos + _dir*(bodylen - _head->getBaseOffset()));

	  // Indicate that the drawable has changed somehow
	_vec->dirtyBound();
  _vec->build();
}

} // !namespace OpenFrames
