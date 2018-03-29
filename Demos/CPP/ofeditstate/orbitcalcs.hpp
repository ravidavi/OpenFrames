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

#ifndef ORBITCALCS_HPP
#define ORBITCALCS_HPP

#include <osg/Vec3d>

const double mu_earth = 3.986004415e5; // [km^3/s^2]

namespace OpenFrames
{
  class Trajectory;
}

// Fill a trajectory with points for the provided elliptical orbital state
// Assumes units are [km] and [s]
void fillTrajectory(const osg::Vec3d& r, const osg::Vec3d& v, OpenFrames::Trajectory *traj);

#endif /* orbitcalcs_hpp */
