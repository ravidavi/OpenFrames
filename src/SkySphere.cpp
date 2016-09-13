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
#include <osg/BlendFunc>
#include <osg/PointSprite>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <sstream>

namespace OpenFrames{

// Implement vertex shader to compute star position/size/color
static const char *OFSkySphere_VertSource = {
  "#version 120\n"
  "uniform mat4 osg_ModelViewProjectionMatrix;\n"
  "attribute vec4 osg_Vertex;\n"

  "void main(void)\n"
  "{\n"
  // Position and color are just passed through, but point size is
  // encoded in the alpha component of color
  "  gl_Position = osg_ModelViewProjectionMatrix*osg_Vertex;\n"
  "  gl_FrontColor = gl_Color;\n"
  "  gl_FrontColor.w = 1.0;\n"
  "  gl_PointSize = gl_Color.w;\n"
  "}\n"
};

// Implement frament shader to render star according to its size and color
static const char *OFSkySphere_FragSource = {
  "#version 120\n"
  "vec2 v;\n"
  "float alpha;\n"
  "const float r_cutoff = 0.25;\n"
  "const float x = -0.5/(r_cutoff - 0.5);\n"
  "const float y = 0.5/(r_cutoff - 0.5);\n"

  "void main(void)\n"
  "{\n"
  // gl_PointCoord has range (x,y) in [0, 1] each, with y-down
  // Move origin to point center, with extents [-0.5, 0.5]
  "  v = gl_PointCoord - vec2(0.5);\n"

  // Fragment gets star color
  "  gl_FragColor = gl_Color;\n"

  // Fragment alpha is based on distance from point center
  // Fades alpha = 1 -> 0 starting at half of point radius
  "  alpha = mix(x, y, length(v));\n"
  "  alpha = clamp(0.0, 1.0, alpha);\n"
  "  gl_FragColor.w = alpha;\n"
  "}\n"
};

SkySphere::SkySphere(const std::string &name)
: Sphere(name)
{
  _init();
}

SkySphere::~SkySphere() {}

void SkySphere::_init()
{
  _minMag = -2.0;
  _maxMag = 8.0;
  _maxNumStars = 10000;
  _starScale = 4.0;

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

  // Set vertex and fragment shaders for stars
  osg::Shader *vertShader = new osg::Shader(osg::Shader::VERTEX, OFSkySphere_VertSource);
  osg::Shader *fragShader = new osg::Shader(osg::Shader::FRAGMENT, OFSkySphere_FragSource);
  osg::Program *program = new osg::Program;
  program->setName("OFSkySphere_ShaderProgram");
  program->addShader(vertShader);
  program->addShader(fragShader);
  ss = _starGeom->getOrCreateStateSet();
  ss->setAttribute(program);
  ss->setMode(GL_PROGRAM_POINT_SIZE, osg::StateAttribute::ON);

  // Set up point sprite
  osg::PointSprite *sprite = new osg::PointSprite();
  ss->setTextureAttributeAndModes(0, sprite);

  // Set blending for stars
  osg::BlendFunc *fn = new osg::BlendFunc();
  fn->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE);
  ss->setAttributeAndModes(fn);

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


bool SkySphere::setStarData(const std::string &catalogName, float minMag, float maxMag, unsigned int numStars, float starScale) 
{
  if((minMag >= maxMag) || (numStars == 0) || (starScale <= 0.0)) return false;
  _starCatalogFile = catalogName;
  _minMag = minMag;
  _maxMag = maxMag;
  _maxNumStars = numStars;
  _starScale = starScale;
  return processStars();
}

bool SkySphere::processStars()
{
  if(_starCatalogFile.empty())
  {
    // Clear star geometry data
    _starGeom->setVertexArray(NULL);
    _starGeom->setColorArray(NULL);
    _starGeom->removePrimitiveSet(0, _starGeom->getNumPrimitiveSets());
    return false;
  }

  // Open the star catalog file
  std::ifstream starfile(_starCatalogFile);
  if(!starfile.is_open()) 
  {
    std::cerr<< "OpenFrames::SkySphere ERROR: Could not open file " << _starCatalogFile << std::endl;
    return false;
  }

  // Throw away header line
  std::string line;
  std::getline(starfile, line);

  // Loop over all stars
  float ra, dec, dist, mag, colorindex;
  int numStars = 0;
  float maxSize = 0.0, minSize = 10000.0;
  Star currStar;
  osg::Vec3Array *vertices = new osg::Vec3Array;
  osg::Vec4Array *colors = new osg::Vec4Array;
  vertices->reserveArray(_maxNumStars);
  colors->reserveArray(_maxNumStars);
  colors->setBinding(osg::Array::BIND_PER_VERTEX);
  osg::Vec3 currVert;
  osg::Vec4 currColor;
  while(starfile && (numStars < _maxNumStars))
  {
    // Get line
    std::getline(starfile, line);
    std::istringstream ss(line);

    // Extract star info
    ss >> ra >> dec >> mag;
    ss >> colorindex;
    if(ss.fail()) colorindex = 0.0;

    // Filter by magnitude
    if((mag < _minMag) || (mag > _maxMag)) continue;

    //currStar.ra = ra*osg::PI/12.0 + 2.0*osg::PI_2;
    currStar.ra = ra*osg::PI/12.0;    // Hours to radians
    currStar.dec = dec*osg::PI/180.0; // Degrees to radians
    currStar.mag = mag;
    currStar.colorindex = colorindex;

    StarToPoint(currStar, currVert, currColor);
    currColor[3] *= _starScale; // Scale star pixel size
    vertices->push_back(currVert);
    colors->push_back(currColor);
    ++numStars;
    if(currColor[3] > maxSize) maxSize = currColor[3];
    if(currColor[3] < minSize) minSize = currColor[3];
  }
  starfile.close();

  std::cout<< std::setprecision(2) << std::fixed << "OpenFrames plotting " << numStars << " stars in magnitude range [" << _minMag << "," << _maxMag << "], and pixel size range [" << minSize << "," << maxSize << "]" << std::endl;

  // Add vertices and colors to star Geometry
  _starGeom->setVertexArray(vertices);
  _starGeom->setColorArray(colors);
  _starGeom->removePrimitiveSet(0, _starGeom->getNumPrimitiveSets());
  _starGeom->addPrimitiveSet(new osg::DrawArrays(GL_POINTS, 0, vertices->size()));

  return true;
}

// See following URL for point size equation
// http://ww.nightscapes.net/techniques/TechnicalPapers/StarColors.pdf
// See the following URL for color equation
// http://www.tannerhelland.com/4435/convert-temperature-rgb-algorithm-code/
void SkySphere::StarToPoint(const Star &star, osg::Vec3 &pos, osg::Vec4 & color)
{
  // Star position, spherical to cartesian
  const float radius = 1.0; // Assume star plotted at unit sphere distance
  pos[0] = radius*std::cos(star.ra)*std::cos(star.dec);
  pos[1] = radius*std::sin(star.ra)*std::cos(star.dec);
  pos[2] = radius*std::sin(star.dec);

  // Star pixel diameter (use color alpha to send this to vertex shader)
  // Diameter computation takes both brightness and luminance into account
  const float La = std::pow(100.0, -2.0/5.0); // Transition luminance
  float L = std::pow(100.0, -star.mag/5.0); // Relative luminance
  color[3] = 2.0*std::sqrt(L / (std::pow(L, 2.0/3.0) + La)); // Pixel diameter 

  // Star color index to temperature (Kelvin) using Ballesteros' formula
  double T = 4600.0*(1.0/(0.92*star.colorindex + 1.7) + 1.0/(0.92*star.colorindex + 0.62));
  T *= 0.01;

  // Red value
  float r;
  if(T <= 66.0) r = 255.0;
  else r = 329.698727446 * std::pow(T-60.0, -0.1332047592);

  if(r < 0.0) r = 0.0;
  else if(r > 255.0) r = 255.0;
  color[0] = r/255.0;

  // Green value
  float g;
  if(T <= 66.0) g = 99.4708025861 * std::log(T) - 161.1195681661;
  else g = 288.1221695283 * std::pow(T-60.0, -0.0755148492);

  if(g < 0.0) g = 0.0;
  else if(g > 255.0) g = 255.0;
  color[1] = g/255.0;

  // Blue value
  float b;
  if(T >= 66.0) b = 255.0;
  else
  {
    if(T <= 19.0) b = 0.0;
    else b = 138.5177312231 * std::log(T-10.0) - 305.0447927307;
  }

  if(b < 0.0) b = 0.0;
  else if(b > 255.0) b = 255.0;
  color[2] = b/255.0;

  // Debug: draw all stars as green
  //color[1] = 1.0;
  //color[0] = color[2] = 0.0;
}

} // !namespace OpenFrames
