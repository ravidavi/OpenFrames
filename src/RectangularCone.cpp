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

/** \file RectangularCone.cpp
 * RectangularCone class function definitions.
 */

#include <OpenFrames/RectangularCone.hpp>

namespace OpenFrames
{
  
RectangularCone::RectangularCone(const std::string &name)
: PolyhedralCone(name)
{
  init();
}

RectangularCone::RectangularCone(const std::string &name, float r, float g, float b, float a)
: PolyhedralCone(name, r, g, b, a)
{
  init();
}

RectangularCone::~RectangularCone() {}

void RectangularCone::setPrimaryAngles(const double& x, const double& y)
{
  _x = x;
  _y = y;
  createRectangularCone();
}

void RectangularCone::init()
{
  setPrimaryAngles(osg::DegreesToRadians(60.0), osg::DegreesToRadians(30.0));
  setDrawMode(SIDES | EDGES | BASE_OUTLINE);
}

void RectangularCone::createRectangularCone()
{
  AngleArray clockAngles, coneAngles;

  double cone = std::sqrt(_x*_x + _y*_y);
  double clock = std::atan(_y / _x);

  clockAngles = { clock, osg::PI - clock, osg::PI + clock, -clock };
  coneAngles = { cone, cone, cone, cone };

  setVertexAngles(clockAngles, coneAngles);
}
} // !namespace OpenFrames
