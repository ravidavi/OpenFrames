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

#ifndef _OF_FRAMEPOINTER_
#define _OF_FRAMEPOINTER_

#include <OpenFrames/Export.h>
#include <OpenFrames/ReferenceFrame.hpp>
#include <OpenFrames/TransformAccumulator.hpp>
#include <osg/Callback>
#include <osg/observer_ptr>

namespace OpenFrames
{

/***************************************************************
 * Ravi Mathur
 * OpenFrames API, class FramePointer
 * Class that rotates a FrameTransform object such that a given vector (inside
 * that frame) will always point towards another ReferenceFrame in the same scene.
 * The rotation performed is the "shortest" rotation that points the desired
 * vector towards the destination ReferenceFrame's origin.
 * Eg: If the given vector is (1, 0, 0), then the x-axis of the FrameTransform
 *     will point towards the destination ReferenceFrame.
 * To use: The FramePointer should be added as an update callback
 * via frameTransformObject->setUpdateCallback(framePointerObject).
***************************************************************/
class OF_EXPORT FramePointer : public osg::Callback
{
public:
  FramePointer(const osg::Vec3d& pointingVec = osg::Vec3d(1.0, 0.0, 0.0));

	/// Don't allow copying from another FramePointer
	FramePointer(const FramePointer &tf, const osg::CopyOp &copyop) {}

	META_Object(OpenFrames, FramePointer);
  
  /// Set the current and destination frames, as well as the common root of the scene
  void setPointingFrames(ReferenceFrame *root, ReferenceFrame *current, ReferenceFrame *dest);
  
  /// Set the vector in the current frame that should point towards the destination frame
  void setPointingVector(const osg::Vec3d& pointingVec) { _pointingVec = pointingVec; }
  const osg::Vec3d& getPointingVec() const { return _pointingVec; }
	
  /// Inherited from osg::Callback, implements the callback.
  virtual bool run(osg::Object* object, osg::Object* data);

protected:
	virtual ~FramePointer();
  
  /// Compute the transform that points the current frame towards the destination frame
  void _computeTransform();
  
  osg::ref_ptr<TransformAccumulator> _xform_parent; // Path to parent frame
  osg::ref_ptr<TransformAccumulator> _xform_dest;   // Path to destination frame
  osg::observer_ptr<ReferenceFrame> _currentFrame;  // Frame for this FramePointer
  
  osg::Vec3d _pointingVec; // The vector in the current frame that should point towards the destination
};

} // !namespace OpenFrames

#endif // !define _OF_FRAMEPOINTER_
