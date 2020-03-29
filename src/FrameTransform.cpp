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

/** \file FrameTransform.cpp
 * Definitions for the FrameTransform class.
 */

#include <OpenFrames/FrameTransform.hpp>
#include <osgUtil/CullVisitor>

namespace OpenFrames
{

FrameTransform::FrameTransform()
{
	reset();
}

FrameTransform::~FrameTransform() {}

void FrameTransform::reset()
{
	_disabled = false;
	_followEye = false;
	setPosition(0.0, 0.0, 0.0);
	setAttitude(0.0, 0.0, 0.0, 1.0);
	setScale(1.0, 1.0, 1.0);
	setPivot(0.0, 0.0, 0.0);
}

void FrameTransform::setPosition(const double &x, const double &y, const double &z)
{
	_position[0] = x;
	_position[1] = y;
	_position[2] = z;
	dirtyBound();
}

void FrameTransform::setPosition(const osg::Vec3d &pos)
{
  _position = pos;
  dirtyBound();
}

void FrameTransform::getPosition(double &x, double &y, double &z) const
{
	if(_disabled) x = y = z = 0.0;
	else
	{
	  x = _position[0];
	  y = _position[1];
	  z = _position[2];
	}
}

void FrameTransform::getPosition(osg::Vec3d &pos) const
{
  if (_disabled) pos.set(0.0, 0.0, 0.0);
  else pos = _position;
}

void FrameTransform::setAttitude(const double &rx, const double &ry,
				const double &rz, const double &angle)
{
	_attitude._v[0] = rx;
	_attitude._v[1] = ry;
	_attitude._v[2] = rz;
	_attitude._v[3] = angle;
	dirtyBound();
}

void FrameTransform::setAttitude(const osg::Quat &att)
{
  _attitude = att;
  dirtyBound();
}

void FrameTransform::getAttitude(double &rx, double &ry, double &rz, double &angle) const
{
	if(_disabled)
	{
	  rx = ry = rz = 0.0;
	  angle = 1.0;
	}
	else
	{
	  rx = _attitude._v[0];
	  ry = _attitude._v[1];
	  rz = _attitude._v[2];
	  angle = _attitude._v[3];
	}
}

void FrameTransform::getAttitude(osg::Quat &att) const
{
  if (_disabled) att.set(0.0, 0.0, 0.0, 1.0);
  else att = _attitude;
}

void FrameTransform::setScale(const double &sx, const double &sy, const double &sz)
{
	_scale[0] = sx;
	_scale[1] = sy;
	_scale[2] = sz;
	dirtyBound();
}

void FrameTransform::getScale(double &sx, double &sy, double &sz)
{
	if(_disabled) sx = sy = sz = 1.0;
	else
	{
	  sx = _scale[0];
	  sy = _scale[1];
	  sz = _scale[2];
	}
}

void FrameTransform::setPivot(const double &px, const double &py, const double &pz)
{
	_pivot[0] = px;
	_pivot[1] = py;
	_pivot[2] = pz;
	dirtyBound();
}

void FrameTransform::getPivot(double &px, double &py, double &pz)
{
	if(_disabled) px = py = pz = 1.0;
	else
	{
	  px = _pivot[0];
	  py = _pivot[1];
	  pz = _pivot[2];
	}
}

/** Compute the matrix that will transform a point in the local frame to a
    point in the world frame. Given a transform consisting of a translation,
    rotation, scale, and pivot, the point is first translated wrt the pivot,
    then scaled in the local frame, then rotated to the world frame, then 
    translated in the world frame. 

    Here, "matrix" is a transform from the parent frame to the world frame,
    so we only need to add (premultiply) the local transformations to it.
*/
bool FrameTransform::computeLocalToWorldMatrix(osg::Matrix& matrix, osg::NodeVisitor* nv) const
{
	if(_disabled) return false; // Don't do anything

	// Compute the transform relative to the parent node
	if(_referenceFrame == RELATIVE_RF)
	{
	  // If we are following the user's eye (ie for a Sky Sphere), then
	  // first translate for that offset.
	  if(_followEye)
	  {
	    osgUtil::CullVisitor* cv = dynamic_cast<osgUtil::CullVisitor*>(nv);
	    if(cv)
	    {
              // Can't use cv->getEyeLocal() since Vec3=Vec3f
	      osg::Vec3d eye = osg::Matrix::inverse(*cv->getModelViewMatrix()).getTrans();
	      matrix.preMultTranslate(eye);
	    }
	  }

	  // "matrix" is the world matrix (from parent to world frame)
	  matrix.preMultTranslate(_position);
	  matrix.preMultRotate(_attitude);
	  matrix.preMultScale(_scale);
	  matrix.preMultTranslate(-_pivot);
	}
	else // ABSOLUTE_RF
	{
	  matrix.makeRotate(_attitude);
	  matrix.postMultTranslate(_position);
	  matrix.preMultScale(_scale);
	  matrix.preMultTranslate(-_pivot);
	}

	return true;
}

/** Compute the matrix that transforms a point in the world frame to a point
    in the local frame. Transforms are applied in the opposite order as in
    the computeLocalToWorldMatrix() function. 

    Here, "matrix" is a transform from the world to the parent frame, so we
    only need to add (postmultiply) local transforms to this.
*/
bool FrameTransform::computeWorldToLocalMatrix(osg::Matrix& matrix, osg::NodeVisitor* nv) const
{
	if(_disabled) return false; // Don't do anything

	// Any zero scale leads to a singularity in the matrix
	if(_scale[0] == 0.0 || _scale[1] == 0.0 || _scale[2] == 0.0)
	  return false;

	if(_referenceFrame == RELATIVE_RF)
	{
	  if(_followEye)
	  {
	    osgUtil::CullVisitor* cv = dynamic_cast<osgUtil::CullVisitor*>(nv);
	    if(cv)
	    {
	      osg::Vec3d eye = osg::Matrix::inverse(*cv->getModelViewMatrix()).getTrans();
	      //osg::Vec3d eye = cv->getEyeLocal();
	      matrix.postMultTranslate(-eye);
	    }
	  }
	  
	  // "matrix" is the local matrix (from world to parent frame)
	  matrix.postMultTranslate(-_position);
	  matrix.postMultRotate(_attitude.inverse());
	  matrix.postMultScale(osg::Vec3d(1.0/_scale[0], 1.0/_scale[1], 1.0/_scale[2]));
	  matrix.postMultTranslate(_pivot);
	}
	else
	{
	  matrix.makeRotate(_attitude.inverse());
	  matrix.preMultTranslate(-_position);
	  matrix.postMultScale(osg::Vec3d(1.0/_scale[0], 1.0/_scale[1], 1.0/_scale[2]));
	  matrix.postMultTranslate(_pivot);
	}

	return true;
}
  
} // !namespace OpenFrames
