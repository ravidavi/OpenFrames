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

  "void main(void)\n"
  "{\n"
  // Position and color are just passed through, but point size is
  // encoded in the alpha component of color
  "  gl_Position = osg_ModelViewProjectionMatrix*gl_Vertex;\n"
  "  gl_FrontColor = gl_Color;\n"
  "  gl_PointSize = gl_Color.w;\n"
  "}\n"
};

// Implement frament shader to render star according to its size and color
static const char *OFSkySphere_FragSource = {
  "#version 120\n"
  "vec2 v;\n"
  "float alpha;\n"

  // Cutoff radius where fragment alpha starts fading to zero
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
  "  alpha = clamp(alpha, 0.0, 1.0);\n"
  "  gl_FragColor.a = alpha;\n"
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
  // Initialize star rendering parameters
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

  // Create new StateSet to specify star-specific OpenGL properties
  ss = new osg::StateSet();

  // Create vertex and fragment shaders for stars
  osg::Shader *vertShader = new osg::Shader(osg::Shader::VERTEX, OFSkySphere_VertSource);
  osg::Shader *fragShader = new osg::Shader(osg::Shader::FRAGMENT, OFSkySphere_FragSource);
  osg::Program *program = new osg::Program;
  program->setName("OFSkySphere_ShaderProgram");
  program->addShader(vertShader);
  program->addShader(fragShader);
  ss->setAttribute(program);
  ss->setMode(GL_PROGRAM_POINT_SIZE, osg::StateAttribute::ON);

  // Set up point sprite
  osg::PointSprite *sprite = new osg::PointSprite();
  ss->setTextureAttributeAndModes(0, sprite);

  // Set additive blending for stars
  osg::BlendFunc *fn = new osg::BlendFunc();
  fn->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE);
  ss->setAttributeAndModes(fn);

  // Create star bins (osg::Geometries) that will hold sets of stars
  for(unsigned int i = 0; i < _starBinCount; ++i)
  {
    _starBinGeoms[i] = new osg::Geometry;
    _starBinGeoms[i]->setName("StarFieldDrawable"+std::to_string(i));
    _starBinGeoms[i]->setStateSet(ss);
    _starBinGeoms[i]->setVertexArray(new osg::Vec3Array());
    osg::Vec4Array *colors = new osg::Vec4Array;
    colors->setBinding(osg::Array::BIND_PER_VERTEX);
    _starBinGeoms[i]->setColorArray(colors);
    _geode->addDrawable(_starBinGeoms[i]);
  }

  // By default draw both texture and starfield
  setDrawMode(TEXTURE + STARFIELD);
}

void SkySphere::setDrawMode(unsigned int drawMode)
{
  // Enable/disable drawable containing textured sphere
  osg::Node::NodeMask sphereMask;
  if(drawMode & TEXTURE) sphereMask = 0xffffffff;
  else sphereMask = 0x0;
  _sphereSD->setNodeMask(sphereMask);

  // Enable/disable drawable containing starfield
  osg::Node::NodeMask starMask;
  if(drawMode & STARFIELD) starMask = 0xffffffff;
  else starMask = 0x0;
  for(int i = 0; i < _starBinGeoms.size(); ++i)
  {
    _starBinGeoms[i]->setNodeMask(starMask);
  }
}

unsigned int SkySphere::getDrawMode()
{
  // Check if textured Sphere is drawn
  unsigned int useTexture = _sphereSD->getNodeMask() & TEXTURE;

  // Check if starfield is drawn
  // We can just check the first bin since all of them have the same mask
  unsigned int useStarfield = _starBinGeoms[0]->getNodeMask() & STARFIELD;

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
    clearStars();
    return false;
  }

  // Open the star catalog file
  std::ifstream starfile(_starCatalogFile);
  if(!starfile.is_open()) 
  {
    std::cerr<< "OpenFrames::SkySphere ERROR: Could not open file " << _starCatalogFile << std::endl;
    return false;
  }

  // Clear all stars before processing new ones
  clearStars();

  // Throw away header line
  std::string line;
  std::getline(starfile, line);

  // Loop over all stars
  float ra, dec, mag, colorindex;
  unsigned int numStars = 0;
  float maxSize = 0.0, minSize = 10000.0;
  float maxMag = 0.0, minMag = 10000.0;
  Star currStar;
  osg::Vec3Array *vertices;
  osg::Vec4Array *colors;
  osg::Vec3 currVert;
  osg::Vec4 currColor;
  unsigned int currBin;
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

    // Prepare star data for processing
    //currStar.ra = ra*osg::PI/12.0 + 2.0*osg::PI_2;
    currStar.ra = ra*osg::PI/12.0;    // Hours to radians
    currStar.dec = dec*osg::PI/180.0; // Degrees to radians
    currStar.mag = mag;
    currStar.colorindex = colorindex;

    // Process current star
    StarToPoint(currStar, currVert, currColor);
    currColor[3] *= _starScale; // Scale star pixel size
    //if(currStar.dec > 89.2*osg::PI/180.0) currColor[3] = 20.0;
    currBin = getStarBin(currVert);
    vertices = static_cast<osg::Vec3Array*>(_starBinGeoms[currBin]->getVertexArray());
    colors = static_cast<osg::Vec4Array*>(_starBinGeoms[currBin]->getColorArray());
    vertices->push_back(currVert);
    colors->push_back(currColor);
    ++numStars;
    if(mag > maxMag) maxMag = mag;
    if(mag < minMag) minMag = mag;
    if(currColor[3] > maxSize) maxSize = currColor[3];
    if(currColor[3] < minSize) minSize = currColor[3];
  }
  starfile.close();

  std::cout<< std::setprecision(2) << std::fixed << "OpenFrames plotting " << numStars << " stars in magnitude range [" << minMag << "," << maxMag << "], and pixel size range [" << minSize << "," << maxSize << "]" << std::endl;

  // Tell all star bins to draw their 
  for(unsigned int i = 0; i < _starBinGeoms.size(); ++i)
  {
    vertices = static_cast<osg::Vec3Array*>(_starBinGeoms[i]->getVertexArray());
    unsigned int currBinSize = vertices->size();

#ifdef OF_DEBUG
    std::cout<< "Star Bin " << i << " has " << currBinSize << " stars" << std::endl;
#endif

    if(currBinSize > 0)
      _starBinGeoms[i]->addPrimitiveSet(new osg::DrawArrays(GL_POINTS, 0, currBinSize));
  }

  return true;
}

// See following URL for point size equation
// http://ww.nightscapes.net/techniques/TechnicalPapers/StarColors.pdf
// See the following URL for color equation
// http://www.tannerhelland.com/4435/convert-temperature-rgb-algorithm-code/
void SkySphere::StarToPoint(const Star &star, osg::Vec3 &pos, osg::Vec4 & color)
{
  // Star position, spherical to cartesian, assuming unit sphere
  pos[0] = std::cos(star.ra)*std::cos(star.dec);
  pos[1] = std::sin(star.ra)*std::cos(star.dec);
  pos[2] = std::sin(star.dec);

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

// Map point on unit sphere to point on unit cube, then map that point
// to a star bin number. This allows spatial grouping of stars so that
// they can be culled when not in view.
// This algorithm does an inverse mapping of the simple cube->sphere
// mapping that normalizes cube points onto the unit sphere. This mapping
// does not produce evenly-spaced points, but is much better than
// tesselating a sphere by latitude/longitude. Alternative algorithms
// can be investigated in the future.
unsigned int SkySphere::getStarBin(const osg::Vec3 &p)
{
  unsigned int face; // Cube face
  float s, t; // Star coordinates on a cube face
  float planedist;

  const float &x = p[0];
  const float &y = p[1];
  const float &z = p[2];
  float ax = std::abs(x);
  float ay = std::abs(y);
  float az = std::abs(z);

  // Map the star to a cube face, and compute its s-t coordinates
  // s-t nomenclature is just an alternative to x-y since the plane
  // could be oriented in any direction

  // +/- Z cube face maps to bin 4/5
  // This happens when |Z| > |X| and |Z| > |Y|
  if((az >= ax) && (az >= ay))
  {
    if(z > 0) face = 4;
    else face = 5;
    planedist = 1.0/az;
    s = planedist*x;
    t = planedist*y;
  }

  // +/- X cube face maps to bin 0/1
  // This happens when |X| > |Y|
  else if(ax >= ay)
  {
    if(x > 0) face = 0;
    else face = 1;
    planedist = 1.0/ax;
    s = planedist*y;
    t = planedist*z;
  }

  // +/- Y cube face maps to bin 2/3
  // This happens for all other cases
  else
  {
    if(y > 0) face = 2;
    else face = 3;
    planedist = 1.0/ay;
    s = planedist*x;
    t = planedist*z;
  }

  // For any plane, s and t are in range [-1, 1] 
  // We need them in range [0, 1] before computing grid location
  s = (s + 1.0)*0.5;
  t = (t + 1.0)*0.5;
  if(s < 0) s = 0.0;
  if(t < 0) t = 0.0;

  // Bins are arranged on a uniform grid within a plane
  float inversespacing = 1.0/(float)_starBinSpacing;
  unsigned int s_bin = (unsigned int)(s/inversespacing);
  unsigned int t_bin = (unsigned int)(t/inversespacing);
  if(s_bin == _starBinSpacing) --s_bin;
  if(t_bin == _starBinSpacing) --t_bin;

  // Compute bin number as n-adic expansion in base _starBinSpacing
  unsigned int bin = s_bin + t_bin*_starBinSpacing + face*_starBinSpacing*_starBinSpacing;

  return bin;
}

void SkySphere::clearStars()
{
  osg::Vec3Array *vertices;
  osg::Vec4Array *colors;

  // Clear star geometry data
  for(unsigned int i = 0; i < _starBinGeoms.size(); ++i)
  {
    vertices = static_cast<osg::Vec3Array*>(_starBinGeoms[i]->getVertexArray());
    colors = static_cast<osg::Vec4Array*>(_starBinGeoms[i]->getColorArray());
    vertices->clear();
    colors->clear();
    _starBinGeoms[i]->removePrimitiveSet(0, _starBinGeoms[i]->getNumPrimitiveSets());
  }
}

} // !namespace OpenFrames
