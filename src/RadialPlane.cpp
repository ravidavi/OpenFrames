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

#include <OpenFrames/RadialPlane.hpp>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/PolygonOffset>
#include <osg/Depth>
#include <osg/BlendFunc>

#include <algorithm> // For std::min

namespace OpenFrames
{
  
RadialPlane::RadialPlane(const std::string &name)
	: ReferenceFrame(name)
{
	init();
}

RadialPlane::RadialPlane(const std::string &name, float r, float g,
                             float b, float a)
	: ReferenceFrame(name, r, g, b, a)
{
	init();
}

RadialPlane::~RadialPlane() {}

void RadialPlane::showContents(bool showContents)
{
	if (showContents)
	{
	  _planeGeode->setNodeMask(0xffffffff);
	  _linesGeode->setNodeMask(0xffffffff);
	}
	else
	{
	  _planeGeode->setNodeMask(0x0);
	  _linesGeode->setNodeMask(0x0);
	}
}

bool RadialPlane::getContentsShown() const
{
	return (_planeGeode->getNodeMask() != 0x0 || _linesGeode->getNodeMask() != 0x0);
}

void RadialPlane::init()
{
	// Create geode to hold the plane geometries
	_planeGeode = new osg::Geode;
	_planeGeode->setName(_name + " plane");
	osg::StateSet *planeSS = _planeGeode->getOrCreateStateSet();

	// Create geode to hold the radial/longitude lines geometries
	_linesGeode = new osg::Geode;
	_linesGeode->setName(_name + " lines");
	osg::StateSet *linesSS = _linesGeode->getOrCreateStateSet();

	// Disable lighting
	planeSS->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
	linesSS->setMode(GL_LIGHTING, osg::StateAttribute::OFF);

	// Enable standard color blending and transparency
	osg::BlendFunc *bf = new osg::BlendFunc();
	bf->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA);
	planeSS->setAttributeAndModes(bf);
	planeSS->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
	linesSS->setAttributeAndModes(bf);
	linesSS->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);

	// Don't write to the depth buffer so that transparent objects behind
	// this plane (eg other planes) will still be drawn properly.
	osg::Depth *depth = new osg::Depth();
	depth->setWriteMask(false);
	planeSS->setAttributeAndModes(depth);
	linesSS->setAttributeAndModes(depth);

	// Make sure that the plane is always drawn behind the lines by
	// offsetting it slightly. See glPolygonOffset man page.
	osg::PolygonOffset *offset = new osg::PolygonOffset(1, 1);
	planeSS->setAttributeAndModes(offset);

	// Create a geometry drawable for the radial circles & longitudinal
	// lines, and another one for the 0-degree longitude line.
	// Note that the geometry drawables for the actual plane itself
	// will be created and set up on the fly.
	_linesGeom = new osg::Geometry;
	_lonGeom = new osg::Geometry;
  
  // Use VBOs
  _linesGeom->setUseDisplayList(false);
  _linesGeom->setUseVertexBufferObjects(true);
  _lonGeom->setUseDisplayList(false);
  _lonGeom->setUseVertexBufferObjects(true);

	// Make the 0 degree longitude line thicker
	osg::StateSet *ss = _lonGeom->getOrCreateStateSet();
	ss->setAttribute(new osg::LineWidth(3.0));

	// Create the arrays for the vertex data
	_lineVertices = new osg::Vec2dArray;
	_planeVertices = new osg::Vec2dArray;

	// Create the color vectors
	_lineColor = new osg::Vec4Array(1);
	_planeColor = new osg::Vec4Array(1);

	// Bind the vertex and color data to the lines geometry objects.
	// Note that the plane geometries will be set up on the fly.
	_linesGeom->setVertexArray(_lineVertices.get());
	_linesGeom->setColorArray(_lineColor.get());
	_linesGeom->setColorBinding(osg::Geometry::BIND_OVERALL);
	_lonGeom->setVertexArray(_lineVertices.get());
	_lonGeom->setColorArray(_lineColor.get());
	_lonGeom->setColorBinding(osg::Geometry::BIND_OVERALL);

	// Attach the drawables to the main geode. The plane geometries will
	// be added to the main geode on the fly.
	_linesGeode->addDrawable(_linesGeom.get());
	_linesGeode->addDrawable(_lonGeom.get());

	// Attach the geodes to the frame's transform
	_xform->addChild(_planeGeode.get());
	_xform->addChild(_linesGeode.get());

	// Set default values
	_planeLOD = 10;
	setParameters(1.0, 0.5, osg::DegreesToRadians(30.0));
	setLineColor(1.0, 1.0, 1.0, 0.2);  // Solid white
	setPlaneColor(0.8, 0.8, 0.8, 0.2); // Semitransparent grey
}

void RadialPlane::setParameters(const double &radius, const double &radSpace,
				  const double &lonSpace)
{
	bool recreatePlane = false;

	if((_radius != radius) && (radius > 0.0))
	{
	  _radius = radius;
	  moveXAxis(osg::Vec3(radius, 0, 0), 0.5*radius);
	  moveYAxis(osg::Vec3(0, radius, 0), 0.5*radius);
	  moveZAxis(osg::Vec3(0, 0, radius), 0.5*radius);
	  recreatePlane = true;
	}

	if((_radialSpacing != radSpace) && (radSpace >= 0.0))
	{
	  _radialSpacing = radSpace;
	  recreatePlane = true;
	}

	if((_longitudeSpacing != lonSpace) && (lonSpace >= 0.0))
	{
	  _longitudeSpacing = lonSpace;
	  recreatePlane = true;
	}

	if(recreatePlane) createPlane();
}

void RadialPlane::getParameters(double &radius, double &radSpace, double &lonSpace) const
{
	radius = _radius;
	radSpace = _radialSpacing;
	lonSpace = _longitudeSpacing;
}

void RadialPlane::setPlaneColor(const osg::Vec4 &color)
{
	// Set color of the radial plane
	(*_planeColor)[0] = color;
  _planeColor->dirty();
}

void RadialPlane::setPlaneColor(float r, float g, float b, float a)
{
	setPlaneColor(osg::Vec4(r, g, b, a));
}

void RadialPlane::setLineColor(const osg::Vec4 &color)
{
	// Set color of the longitude lines & radial circles
	(*_lineColor)[0] = color;
  _lineColor->dirty();
}

void RadialPlane::setLineColor(float r, float g, float b, float a)
{
	setLineColor(osg::Vec4(r, g, b, a));
}

const osg::BoundingSphere& RadialPlane::getBound() const
{
  	// Have bounding sphere encompass plane and axes/labels, but centered
	// on the plane (since that is the object of interest)
	ReferenceFrame::getBound();
	osg::BoundingSphere bs = osg::BoundingSphere(osg::Vec3(), _radius);
	bs.expandRadiusBy(_bound);
	_bound = bs;

	return _bound;
}

void RadialPlane::createPlane()
{
	unsigned int start; // Starting index of current line
	const double epsilon = 1.0e-6; // Precision tolerance
	const double step = osg::PI/180.0; // Step size for radial circles
	double max = 2.0*osg::PI - epsilon;

	// Prepare geometries for new vertices that will be computed
	_linesGeom->removePrimitiveSet(0, _linesGeom->getNumPrimitiveSets());
	_lonGeom->removePrimitiveSet(0, _lonGeom->getNumPrimitiveSets());

	// Delete the plane geometries; they'll be recreated later
	_planeGeode->removeDrawables(0, _planeGeode->getNumDrawables());

	// Remove existing vertices
	_lineVertices->clear();
	_planeVertices->clear();

	// Variables associated with the translucent radial plane
	const double radius2 = _radius*_radius;
	const double width = _radius/(double)_planeLOD;
	double x, y;
	double xmax, ymax, xupper, yupper; // Temp vars
	osg::Vec2d v[5]; // 5 possible vertices per cell
	osg::Vec2d vert[5]; // Vertices submitted for cell creation
	unsigned int nV; // Number of vertices in current cell

	// The plane will be tesselated with a grid of polygon cells. Interior
	// cells will be squares. Cells on the boundary circle will be either
	// triangles, quadrilaterals, or pentagons, depeding on how they
	// intersect the boundary circle.
	for(x = 0.0; x < _radius - epsilon; x += width)
	{
	  // RadialPlane boundary y-value at current x-location
	  ymax = sqrt(radius2 - x*x);

	  for(y = 0.0; y < ymax - epsilon; y += width)
	  {
	    // RadialPlane boundary x-value at current y-location
	    xmax = sqrt(radius2 - y*y);

	    // First vertex is always the current location
	    v[0].set(x, y);

	    // Second vertex moves as far as allowed in the x-direction
	    v[1].set(std::min(x+width, xmax), y);

	    nV = 2; // So far we have 2 vertices

	    // If the second vertex went the full width, then the next 
	    // vertex moves as far as allowed in the y-direction
	    if(v[1].x() < xmax - epsilon)
	    {
	      yupper = sqrt(radius2 - v[1].x()*v[1].x());
	      v[nV].set(v[1].x(), std::min(y+width, yupper));
	      ++nV;
	    }

	    // Assuming there are 5 vertices, the 5th one moves as far as
	    // allowed in the y-direction from the first vertex
	    v[4].set(x, std::min(y+width, ymax));

	    // If the 5th vertex went the full width, then the next vertex
	    // moves as far as allowed in the x-direction
	    if(v[4].y() < ymax - epsilon)
	    {
	      xupper = sqrt(radius2 - v[4].y()*v[4].y());
	      v[nV].set(std::min(x+width, xupper), v[4].y());
	      ++nV;
	    }

	    // Finally set the last vertex
	    v[nV] = v[4];
	    ++nV;

	    // Directly add first quadrant cell since we just computed it.
	    addCell(v, nV);

	    // Add second quadrant cell (negate x coordinates)
	    // Also, reverse vertex order so that vertices are entered
	    // counter-clockwise (to make sure they are still front facing).
	    for(unsigned int i = 0; i < nV; ++i)
	    {
	      vert[i].set(-v[nV-1-i].x(), v[nV-1-i].y());
	    }
	    addCell(vert, nV);

	    // Add third quadrant cell (negate x&y coordinates)
	    // Note that vertex order is maintained.
	    for(unsigned int i = 0; i < nV; ++i)
	    {
	      vert[i] = -v[i];
	    }
	    addCell(vert, nV);

	    // Add fourth quadrant cell (negate y coordinates)
	    // Again, reverse vertex order so that vertices are entered
	    // counter-clockwise and are still front facing.
	    for(unsigned int i = 0; i < nV; ++i)
	    {
	      vert[i].set(v[nV-1-i].x(), -v[nV-1-i].y());
	    }
	    addCell(vert, nV);

	  } // for(cycle through y coordinates)
	} // for(cycle through x coordinates)

	// Create radial circles if required
	if(_radialSpacing > 0.0)
	{
	  // Create points for the plane's boundary
	  for(double alpha = 0.0; alpha < max; alpha += step)
	  {
	    x = _radius*cos(alpha); y = _radius*sin(alpha);
	    _lineVertices->push_back(osg::Vec2d(x, y));
	  }

	  // Add the boundary to the lines geometry
	  _linesGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_LOOP, 0, _lineVertices->size()));

	  // Create points for each radial circle except for the boundary
	  for(double curr_r = _radialSpacing; curr_r < _radius - epsilon; curr_r += _radialSpacing)
	  {
	    // Save starting index of current radial circle
	    start = _lineVertices->size();

	    // Create points for current radial circle
	    for(double alpha = 0; alpha < max; alpha += step)
	    {
	      x = curr_r*cos(alpha); y = curr_r*sin(alpha);
	      _lineVertices->push_back(osg::Vec2d(x, y));
	    }

	    // Add the current radial circle to the lines geometry
	    _linesGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_LOOP, start, _lineVertices->size() - start));
	  }
	}

	// Create longitude lines if required
	if(_longitudeSpacing > 0.0)
	{
	  // Save starting index of longitude lines
	  start = _lineVertices->size();

	  for(double alpha = 0; alpha < max; alpha += _longitudeSpacing)
	  {
	    x = _radius*cos(alpha); y = _radius*sin(alpha);
	    _lineVertices->push_back(osg::Vec2d(0.0, 0.0));
	    _lineVertices->push_back(osg::Vec2d(x, y));
	  }

	  // Add the 0 degree line to a separate geometry so it can be thicker
	  _lonGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES, start, 2));

	  // Add rest of the longitude lines to the lines geometry
	  start += 2;
	  if(_lineVertices->size() > start)
	  {
	    _linesGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES, start, _lineVertices->size() - start));
	  }
	}
  
  // Indicate that data has changed and should be re-rendered
  _lineVertices->dirty();
  _planeVertices->dirty();
}

void RadialPlane::addCell(const osg::Vec2d v[], unsigned int nV)
{
	// Store the starting index of this cell's vertices
	unsigned int start = _planeVertices->size();

	// Add the vertices as necessary
	for(unsigned int i = 0; i < nV; ++i)
	{
	  _planeVertices->push_back(v[i]);
	}

	// Create geometry for this cell
	osg::Geometry *currCell = new osg::Geometry();
  currCell->setUseDisplayList(false);
  currCell->setUseVertexBufferObjects(true);
	currCell->setVertexArray(_planeVertices.get());
	currCell->setColorArray(_planeColor.get());
	currCell->setColorBinding(osg::Geometry::BIND_OVERALL);

	// Draw the appropriate type of polygon
	switch(nV)
	{
	  case 3:
	    currCell->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::TRIANGLES, start, nV));
	    break;

	  case 4:
	    currCell->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS, start, nV));
	    break;

	  case 5:
	    currCell->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::POLYGON, start, nV));
	    break;
	}

	// Add an outline of the cell for debugging
	//currCell->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_LOOP, start, nV));

	// Add the cell drawable to the geode
	_planeGeode->addDrawable(currCell);
}

} // !namespace OpenFrames
