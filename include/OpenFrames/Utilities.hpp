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

/** \file Utilities.hpp
 * Declaration of Utilities class.
 */

#ifndef _OF_UTILITIES_
#define _OF_UTILITIES_

#include <osg/Matrixd>
#include <osg/View>

namespace OpenFrames {
  
  /** OpenFrames function that updates a projection matrix with specified near/far plane */
  void updateProjectionMatrix(osg::Matrix& proj, const double &zNear, const double &zfar);
  
  /** Get the osg::View's viewport by searching its master camera then slave cameras */
  osg::Viewport* getMainViewport(osg::View *view);
  
  /** Get the osg::View's graphics context by searching its master camera then slave cameras */
  osg::GraphicsContext* getMainGraphicsContext(osg::View *view);
  
} // !namespace OpenFrames

#endif  // !define _OF_UTILITIES_
