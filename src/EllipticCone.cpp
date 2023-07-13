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

/** \file EllipticCone.cpp
 * EllipticCone class function definitions.
 */

#include <OpenFrames/EllipticCone.hpp>
#include <limits>

namespace OpenFrames
{
  
EllipticCone::EllipticCone(const std::string &name)
: PolyhedralCone(name)
{
  init();
}

EllipticCone::EllipticCone(const std::string &name, float r, float g, float b, float a)
: PolyhedralCone(name, r, g, b, a)
{
  init();
}

EllipticCone::~EllipticCone() {}

void EllipticCone::setPrimaryAngles(const double& a, const double& b)
{
  _a = a;
  _b = b;
  createEllipticCone();
}

void EllipticCone::init()
{
  setPrimaryAngles(osg::DegreesToRadians(60.0), osg::DegreesToRadians(30.0));
  setDrawMode(SIDES | BASE_OUTLINE);
}

void EllipticCone::createEllipticCone()
{
  AngleArray clockAngles, coneAngles;
  const double epsilon = 1.0e-6; // Precision tolerance
  const double step = osg::PI / 60.0;
  double maxAngle = 2.0*osg::PI - epsilon;

  // Need better vertex spacing
  double coneAngle, ca, sa;
  for(double angle = 0.0; angle < maxAngle; angle += step)
  {
    clockAngles.push_back(angle);

    ca = std::cos(angle);
    sa = std::sin(angle);
    coneAngle = _a * _b / std::sqrt(_b*_b*ca*ca + _a*_a*sa*sa);
    coneAngles.push_back(coneAngle);
  }

  setVertexAngles(clockAngles, coneAngles);
}

bool EllipticCone::isVisible(osg::Vec3d point, const double& minDistance, const double& maxDistance) const
{
  // Make sure point is within distance limits, noting that cone points along -z axis
  if((-point.z() < minDistance) || (-point.z() > maxDistance)) return false;

  // Cone apex is at the origin and is visible by the cone (prevents divide by zero later)
  if(point.length2() == 0.0) return true;

  // Scale point to have distance (z component) of 1
  point /= -point.z();

  // Compute point clock & cone angles
  double cone = std::atan(std::sqrt(point.x()*point.x() + point.y()*point.y()));
  double clock = std::atan2(point.y(), point.x());

  // Compute max cone angle at point's clock angle
  double ca = std::cos(clock);
  double sa = std::sin(clock);
  double maxCone = _a * _b / std::sqrt(_b*_b*ca*ca + _a*_a*sa*sa);

  if(cone <= maxCone) return true;
  else return false;
}

} // !namespace OpenFrames
