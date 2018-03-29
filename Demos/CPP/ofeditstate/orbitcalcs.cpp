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

#include <OpenFrames/Trajectory.hpp>
#include "orbitcalcs.hpp"

void fillTrajectory(const osg::Vec3d& r, const osg::Vec3d& v, OpenFrames::Trajectory *traj)
{
  // Check if provided state is an elliptical orbit
  double rmag = r.length();
  double vmag = v.length();
  double energy = v*v/2.0 - mu_earth/rmag;
  
  // Only proceed if orbit is elliptical
  if(energy > 0.0) return;
}
