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

/** \file DoubleSingleUtils.hpp
 * Definition of DoubleSingleUtils utility functions.
 */

#ifndef _OF_DOUBLESINGLEUTILS_
#define _OF_DOUBLESINGLEUTILS_

#include <OpenFrames/Export.h>
#include <osg/Vec3d>
#include <osg/Vec3f>

/***********************************************************
 * Ravi Mathur
 * OpenFrames API, Double-Single Utilities
 * Provides utility functions to represent osg::Vec3d as
 * a pair of osg::Vec3f.
***********************************************************/
namespace OpenFrames
{

  // Split Vec3d into two Vec3f
  static inline void DS_Split(const osg::Vec3d &point, osg::Vec3f &high, osg::Vec3f &low)
  {
    high = point;
    low  = point - high;
  }

  // Perform a - b, where a and b are pairs of Vec3f
  static inline void DS_Subtract(const osg::Vec3f &a_high,
                                 const osg::Vec3f &a_low,
                                 const osg::Vec3f &b_high,
                                 const osg::Vec3f &b_low,
                                 osg::Vec3f &result)
  {
    osg::Vec3f t1, t2, e, c_high, c_low;
    t1 = a_low - b_low;
    e  = t1 - a_low;
    t2 = ((-b_low - e) + (a_low - (t1 - e))) + a_high - b_high;

    c_high = t1 + t2;
    c_low  = t2 - (c_high - t1);
    result = c_high + c_low;
  }

}
#endif
