/***********************************
 Copyright 2017 Ravishankar Mathur, Emergent Space Technologies Inc.
 
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

#ifndef _OF_UTILITIES_
#define _OF_UTILITIES_

#include <osg/Matrixd>
#include <osg/View>

namespace OpenFrames {
  
  /** OpenFrames function that updates a projection matrix with specified near/far plane */
  void updateProjectionMatrix(osg::Matrix& proj, const double &zNear, const double &zfar);
  
  /** Get the osg::View's master camera's viewport. If that does not exist, then get its
   first available slave camera's viewport. */
  osg::Viewport* getMainViewport(osg::View *view);
  
} // !namespace OpenFrames

#endif  // !define _OF_UTILITIES_
