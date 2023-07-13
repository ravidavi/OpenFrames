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

#include <OpenFrames/FramePointer.hpp>

namespace OpenFrames
{

FramePointer::FramePointer(const osg::Vec3d& pointingVec)
{
  _xform_parent = new TransformAccumulator;
  _xform_dest = new TransformAccumulator;
  _pointingVec = pointingVec;
}

FramePointer::~FramePointer()
{
}

void FramePointer::setPointingFrames(ReferenceFrame *root, ReferenceFrame *current, ReferenceFrame *dest)
{
  // Make sure current frame has a parent
  if(!current || (current->getNumParents() == 0)) return;
  
  _currentFrame = current;
  
  // Set the parent reference frame
  // TODO: Save a link from FrameTransform to it's containing ReferenceFrame, so that
  //       the user does not have to specify that ReferenceFrame explicitly.
  _xform_parent->setRoot(root);
  _xform_parent->setOrigin(current->getParent(0));
  
  // Set the destination reference frame
  _xform_dest->setRoot(root);
  _xform_dest->setOrigin(dest);
}

bool FramePointer::run(osg::Object* object, osg::Object* data)
{
  // Compute the transform that points the parent frame towards the destination frame
  _computeTransform();
  
  // Call nested callbacks and traverse rest of scene graph
  return osg::Callback::traverse(object, data);
}
  
void FramePointer::_computeTransform()
{
  // Make sure parent and destination frames are valid
  if(!_xform_parent->isValid() || !_xform_dest->isValid()) return;
  
  // Get transform from destination frame's local space to parent's local space
  osg::Matrixd dest_to_parent = _xform_dest->getLocalToWorld() * _xform_parent->getWorldToLocal();
  
  // Get vector to destination frame's origin in parent frame's space
  osg::Vec3d destVec = dest_to_parent.getTrans();
  
  // Get vector to current frame's origin in parent frame's space
  osg::Vec3d currentVec;
  _currentFrame->getPosition(currentVec);
  
  // Get vector from current frame to destination frame
  osg::Vec3d destVecRelative = destVec - currentVec;
  
  // Direct rotation from desired pointing vector to destination vector
  osg::Quat q;
  q.makeRotate(_pointingVec, destVecRelative);
  
  _currentFrame->setAttitude(q);
}
  
} // !namespace OpenFrames
