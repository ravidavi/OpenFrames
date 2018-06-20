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

#ifndef _OF_FRAMETRANSFORM_
#define _OF_FRAMETRANSFORM_

#include <OpenFrames/Export.h>
#include <osg/Transform>

namespace OpenFrames
{

/************************************
 * Ravi Mathur
 * OpenFrames API, class FrameTransform
 * This class implements a transformation from local to world coordinates and back.  It is a subclass of the OSG class osg::Transform.
************************************/
class OF_EXPORT FrameTransform : public osg::Transform
{
  public:

	FrameTransform();

	// Don't allow copying from another FrameTransform
	FrameTransform(const FrameTransform& xform, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY) {}

	  // Standard OSG Node methods
	META_Node( OpenFrames, FrameTransform );

	void reset();

	// Set whether this frame should follow the viewer's eye point.
	// This is used ie for a SkyBox.
	inline void setFollowEye(bool f) { _followEye = f; }
	inline bool getFollowEye() const { return _followEye; }

	// Set whether this frame applies its transform or not.
	inline void setDisabled(bool d) { _disabled = d; }
	inline bool isDisabled() const { return _disabled; }

	// Set position wrt parent frame
	void setPosition(const double &x, const double &y, const double &z);
  void setPosition(const osg::Vec3d &pos);
	void getPosition(double &x, double &y, double &z) const;
  void getPosition(osg::Vec3d &pos) const;

	// Set attitude (orientation) wrt parent frame. Note that this is
	// a quaternion (qx, qy, qx, qw).
	void setAttitude(const double &rx, const double &ry, const double &rz, const double &angle);
  void setAttitude(const osg::Quat &att);
	void getAttitude(double &rx, double &ry, double &rz, double &angle) const;
  void getAttitude(osg::Quat &att) const;

	// Set scale wrt parent frame
	void setScale(const double &sx, const double &sy, const double &sz);
	void getScale(double &sx, double &sy, double &sz);

	// Set the pivot point about which scales & rotations are computed.
	void setPivot(const double &px, const double &py, const double &pz);
	void getPivot(double &px, double &py, double &pz);

	// Inherited functions to compute transformation matrix
	virtual bool computeLocalToWorldMatrix(osg::Matrix& matrix, osg::NodeVisitor* nv) const;
	virtual bool computeWorldToLocalMatrix(osg::Matrix& matrix, osg::NodeVisitor* nv) const;

  protected:
	virtual ~FrameTransform();

	osg::Vec3d _position; // Position relative to parent frame's origin
	osg::Quat _attitude;  // Attitude relative to parent frame
	osg::Vec3d _scale;    // Scale in addition to parent frame's scale
	osg::Vec3d _pivot;    // Pivot point relative to local origin

	// If disabled, this transform will have no effect
	bool _disabled;

	// Allows frame to follow the current eye point.  Only effective if
	// the reference frame is RELATIVE_RF
	bool _followEye;
};

} // !namespace OpenFrames

#endif // !define _OF_FRAMETRANSFORM_
