/***********************************
   Copyright 2016 Ravishankar Mathur

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

#include <OpenFrames/SkySphere.hpp>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>

namespace OpenFrames{

SkySphere::SkySphere(const std::string &name)
: Sphere(name)
{
  _init();
}

SkySphere::~SkySphere() {}

void SkySphere::_init()
{
  // Hide Sphere's axes and labels
  showAxes(ReferenceFrame::NO_AXES);
  showAxesLabels(ReferenceFrame::NO_AXES);
  showNameLabel(false);

  // Make sure Sphere follows the eye
  getTransform()->setFollowEye(true);

  // Disable lighting and depth testing
  osg::StateSet *ss = _geode->getOrCreateStateSet();
  ss->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
  ss->setMode(GL_DEPTH_TEST, osg::StateAttribute::OFF);

  // Draw SkySphere before other objects so it's always in background
  ss->setRenderBinDetails(-1, "RenderBin");

  // Create Geometry that will contain stars
  _starGeom = new osg::Geometry;
  _starGeom->setName("StarFieldDrawable");
  _geode->addDrawable(_starGeom);

  // By default draw both texture and starfield
  setDrawMode(TEXTURE + STARFIELD);
}

void SkySphere::setDrawMode(unsigned int drawMode)
{
  // Enable/disable drawable containing textured sphere
  if(drawMode & TEXTURE)
    _sphereSD->setNodeMask(0xffffffff);
  else
    _sphereSD->setNodeMask(0x0);

  // Enable/disable drawable containing starfield
  if(drawMode & STARFIELD)
    _starGeom->setNodeMask(0xffffffff);
  else
    _starGeom->setNodeMask(0x0);
}

unsigned int SkySphere::getDrawMode()
{
  // Check if textured Sphere is drawn
  unsigned int useTexture = _sphereSD->getNodeMask() & TEXTURE;

  // Check if starfield is drawn
  unsigned int useStarfield = _starGeom->getNodeMask() & STARFIELD;

  // Return draw mode
  return (useTexture + useStarfield);
}


bool SkySphere::setStarCatalog(const std::string &fname)
{
  // Open the star catalog file
  std::ifstream starfile(fname);
  if(!starfile.is_open()) 
  {
    std::cerr<< "OpenFrames::SkySphere ERROR: Could not open file " << fname << std::endl;
    return false;
  }

  // Throw away header line
  std::string line;
  std::getline(starfile, line);

  // Loop over all stars
  float ra, dec, dist, mag, colorindex;
  int numStars = 0;
  const int maxNumStars = 10000;
  Star currStar;
  osg::Vec3Array *vertices = new osg::Vec3Array;
  osg::Vec4Array *colors = new osg::Vec4Array;
  vertices->reserveArray(maxNumStars);
  colors->reserveArray(maxNumStars);
  colors->setBinding(osg::Array::BIND_PER_VERTEX);
  osg::Vec3 currVert;
  osg::Vec4 currColor;
  while(starfile && (numStars < maxNumStars))
  {
    // Get line
    std::getline(starfile, line);
    std::stringstream ss(line);

    // Extract star info
    ss >> ra >> dec >> dist >> mag >> colorindex;

    currStar.ra = ra*osg::PI/12.0;
    currStar.dec = dec*osg::PI/180.0;
    currStar.mag = mag;
    currStar.colorindex = colorindex;

    StarToPoint(currStar, currVert, currColor);
    vertices->push_back(currVert);
    colors->push_back(currColor);
    ++numStars;
  }
  starfile.close();
  vertices->push_back(osg::Vec3(2, 0, 0));
  colors->push_back(currColor);

  // Add vertices and colors to star Geometry
  _starGeom->setVertexArray(vertices);
  _starGeom->setColorArray(colors);
  _starGeom->removePrimitiveSet(0, _starGeom->getNumPrimitiveSets());
  _starGeom->addPrimitiveSet(new osg::DrawArrays(GL_POINTS, 0, vertices->size()));
  _starGeom->dirtyBound();

  return true;
}

// See following URL for color & point size equations
// http://ww.nightscapes.net/techniques/TechnicalPapers/StarColors.pdf
void SkySphere::StarToPoint(const Star &star, osg::Vec3 &pos, osg::Vec4 & color)
{
  // Star position
  const float r = 1.0; // Assume Star at unit sphere distance
  pos[0] = r*std::cos(star.ra)*std::cos(star.dec);
  pos[1] = r*std::sin(star.ra)*std::cos(star.dec);
  pos[2] = r*std::sin(star.dec);

  // Star color
  color[0] = 1.0;
  color[1] = 0.0;
  color[2] = 0.0;

  // Star pixel diameter (use color alpha to send this to vertex shader)
  const float La = std::pow(100.0, -2.0/5.0);
  float L = std::pow(100.0, -star.mag/5.0);
  //color[3] = 2.0*std::sqrt(L / (std::pow(L, 2.0/3.0) + La));
  color[3] = 1.0;
}

} // !namespace OpenFrames
