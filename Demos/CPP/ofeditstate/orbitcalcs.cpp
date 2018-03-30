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
#include <osg/Quat>
#include <osg/io_utils>
#include <cmath>
#include "orbitcalcs.hpp"

void fillTrajectory(const double& a, const double& e,
                    const double& i, const double& w, const double& RAAN,
                    OpenFrames::Trajectory *traj)
{
  // Only proceed if orbit is elliptical
  if(e >= 1.0) return;
  
  // Clear trajectory in preparation
  traj->clear();
  
  // Create points around the ellipse, starting with zero true anomaly
  const int numPoints = 360;
  double ta;
  osg::Vec3d r, v;
  osg::Quat att;
  for(int point = 0; point < numPoints; ++point)
  {
    // Compute true anomaly for current point
    ta = ((double)point)*2.0*osg::PI/((double)numPoints);
    
    // Compute cartesian state for current point
    KepToCart(ta, a, e, i, w, RAAN, r, v);
    
    // Compute attitude that makes spacecraft y-axis along velocity vector
    att.makeRotate(osg::Vec3d(0, 1, 0), v);
    
    traj->addTime(ta);
    traj->addPosition(r._v);
    traj->setOptional(0, v._v);
    traj->addAttitude(att._v);
  }
}

void KepToCart(const double& ta, const double& a, const double& e,
               const double& i, const double& w, const double& RAAN,
               osg::Vec3d& r, osg::Vec3d& v)
{
  // Semilatus rectum
  double p = a*(1.0 - e*e);
  
  // Radius
  double rmag = p / (1.0 + e*std::cos(ta));
  
  // Angular momentum magnitude
  double hmag = std::sqrt(mu_earth*p);
  
  // Common trig calculations
  double cRAAN = std::cos(RAAN);
  double sRAAN = std::sin(RAAN);
  double cwta = std::cos(w+ta);
  double swta = std::sin(w+ta);
  double ci = std::cos(i);
  double si = std::sin(i);
  double sta = std::sin(ta);
  
  // Position
  r[0] = rmag*(cRAAN*cwta - sRAAN*swta*ci);
  r[1] = rmag*(sRAAN*cwta + cRAAN*swta*ci);
  r[2] = rmag*swta*si;
  
  // Velocity
  double C1 = hmag*e*sta/(rmag*p);
  double C2 = hmag/rmag;
  v[0] = r[0]*C1 - C2*(cRAAN*swta + sRAAN*cwta*ci);
  v[1] = r[1]*C1 - C2*(sRAAN*swta - cRAAN*cwta*ci);
  v[2] = r[2]*C1 + C2*cwta*si;
}

void CartToKep(const osg::Vec3d& r, const osg::Vec3d& v,
               double& ta, double& a, double& e,
               double& i, double& w, double& RAAN)
{
  // Convert provided cartesian state to Keplerian elements
  double rmag = r.length();
  double vmag = v.length();
  osg::Vec3d h = r ^ v;
  double hmag = h.length();
  double energy = v*v/2.0 - mu_earth/rmag;
  a = -mu_earth/(2.0*energy);
  e = std::sqrt(1.0 - h*h/(a*mu_earth));
  i = std::acos(h[2]/hmag);
  RAAN = std::atan2(h[0], -h[1]);
  double wta;
  if(std::abs(i) < 1.0e-12) wta = 0.0;
  else wta = std::atan2(r[2]/std::sin(i), r[0]*std::cos(RAAN) + r[1]*std::sin(RAAN));
  if(std::abs(e) < 1.0e-12) ta = 0.0;
  else ta = std::acos((a*(1.0-e*e) - rmag)/(e*rmag));
  if(r*v < 0.0) ta = 2.0*osg::PI - ta;
  w = wta - ta;
}
