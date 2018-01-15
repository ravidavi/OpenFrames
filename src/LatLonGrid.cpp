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

#include <OpenFrames/LatLonGrid.hpp>
#include <osg/LineWidth>
#include <osg/Math>

namespace OpenFrames
{

LatLonGrid::LatLonGrid(const std::string &name)
	: ReferenceFrame(name)
{
	_init();
}

LatLonGrid::LatLonGrid(const std::string &name, float r, float g,
                             float b, float a)
	: ReferenceFrame(name, r, g, b, a)
{
	_init();
}

LatLonGrid::~LatLonGrid() {}

void LatLonGrid::setParameters(const double &radius, const double &latSpace, const double &lonSpace)
{
  	bool recreateGrid = false;

	if((_radius != radius) && (radius > 0))
	{
	  _radius = radius;
	  moveXAxis(osg::Vec3(radius, 0, 0), 0.5*radius);
	  moveYAxis(osg::Vec3(0, radius, 0), 0.5*radius);
	  moveZAxis(osg::Vec3(0, 0, radius), 0.5*radius);
	  recreateGrid = true;
	}

	if((_latSpacing != latSpace) && (latSpace >= 0))
	{
	  _latSpacing = latSpace;
	  recreateGrid = true;
	}

	if((_lonSpacing != lonSpace) && (lonSpace >= 0))
	{
	  _lonSpacing = lonSpace;
	  recreateGrid = true;
	}

	if(recreateGrid) _createGrid();
}

void LatLonGrid::getParameters(double &radius, double &latSpace, double &lonSpace) const
{
	radius = _radius;
	latSpace = _latSpacing;
	lonSpace = _lonSpacing;
}

const osg::BoundingSphere& LatLonGrid::getBound() const
{
  	// Have bounding sphere encompass grid and axes/labels, but centered
	// on the grid (since that is the object of interest)
	ReferenceFrame::getBound();
	osg::BoundingSphere bs = osg::BoundingSphere(osg::Vec3(), _radius);
	bs.expandRadiusBy(_bound);
	_bound = bs;

	return _bound;
}

void LatLonGrid::setColor( const osg::Vec4 &color )
{
	ReferenceFrame::setColor(color);
	(*_colors)[0] = color;
	_gridGeom->dirtyDisplayList();
	_mainGeom->dirtyDisplayList();
}

void LatLonGrid::_init()
{
	// Create geode to hold each geometry drawable
	_geode = new osg::Geode;
	_geode->setName(_name);

	// Disable lighting computations since we're only drawing lines
        _geode->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF);

	// Create a geometry drawable for the equator and prime meridian, and
	// another one for the rest of the grid lines.
	_gridGeom = new osg::Geometry;
	_mainGeom = new osg::Geometry;

	// Create the arrays for the vertex data and colors
	_vertices = new osg::Vec3Array;
	_colors = new osg::Vec4Array(1);

	// Set greater line thickness for the equator/prime meridian.
	_mainGeom->getOrCreateStateSet()->setAttribute(new osg::LineWidth(3.0));

	// Bind the vertex and color data to the geometry objects
	_gridGeom->setVertexArray(_vertices.get());
	_gridGeom->setColorArray(_colors.get());
	_gridGeom->setColorBinding(osg::Geometry::BIND_OVERALL);
	_mainGeom->setVertexArray(_vertices.get());
	_mainGeom->setColorArray(_colors.get());
	_mainGeom->setColorBinding(osg::Geometry::BIND_OVERALL);

	// Attach the drawables to the main geode, then to the local transform
	_geode->addDrawable(_gridGeom.get());
	_geode->addDrawable(_mainGeom.get());
	_xform->addChild(_geode.get());

	// Set up defaults for sphere size, line spacing, and color
	setParameters(1.0, osg::DegreesToRadians(30.0), osg::DegreesToRadians(30.0));
	setColor(getColor());
}

void LatLonGrid::_createGrid()
{
	double alpha, beta; // Spherical angles to compute grid points
	double ca, cb, sa, sb; // Sine & Cosine of alpha and beta angles
	unsigned int start; // Starting index of current lat/lon line
	double lod; // Level of Detail scale used when drawing latitude lines
	bool first; // Flag to indicate equator/prime meridian
	const double epsilon = 1.0e-6; // Precision tolerance

	// Remove existing vertices
	_vertices->clear();
	if(_gridGeom->getNumPrimitiveSets() > 0)
	  _gridGeom->removePrimitiveSet(0, _gridGeom->getNumPrimitiveSets());
	if(_mainGeom->getNumPrimitiveSets() > 0)
	  _mainGeom->removePrimitiveSet(0, _mainGeom->getNumPrimitiveSets());

	// Draw lines of latitude, starting at the equator and going
	// north/south by the specified spacing.
	if(_latSpacing > 0.0)
	{
	  for(beta = 0, first = true; beta < osg::PI_2-epsilon; beta += _latSpacing)
	  {
	    cb = cos(beta); sb = sin(beta);

	    // Reduce the number of points used to create a latitude line as
	    // the line gets further north and south, since those latitude
	    // lines are shorter than the equator.
	    lod = 0.5*(sb + 1.0);

	    // Latitude line north of equator
	    start = _vertices->size();
	    for(alpha = 0; alpha < 2.0*osg::PI-epsilon; alpha += lod*osg::PI/180.0)
	    {
	      ca = cos(alpha); sa = sin(alpha);
	      _vertices->push_back(osg::Vec3(_radius*ca*cb, _radius*sa*cb, _radius*sb));
	    }

	    if(first)
	    {
	      // Draw equator as an OpenGL LINE_LOOP
	      _mainGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_LOOP, start, _vertices->size() - start));
	      first = false;
	      continue;
	    }
	    else
	    {
	      // Draw current northern latitude as an OpenGL LINE_LOOP
	      _gridGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_LOOP, start, _vertices->size()-start));
	    }

	    // Latitude line south of equator.
	    start = _vertices->size();
	    for(alpha = 0; alpha < 2.0*osg::PI-epsilon; alpha += lod*osg::PI/180.0)
	    {
	      ca = cos(alpha); sa = sin(alpha);
	      _vertices->push_back(osg::Vec3(_radius*ca*cb, _radius*sa*cb, -_radius*sb));
	    }

	    // Draw current southern latitude as an OpenGL LINE_LOOP
	    _gridGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_LOOP, start, _vertices->size()-start));
	  }
	}

	// Draw lines of longitude, starting at the prime meridian and going
	// east by the specified spacing.
	if(_lonSpacing > 0.0)
	{
	  start = _vertices->size();
	  double step = osg::PI/360.0;
	  for(alpha=0, first=true; alpha < 2.0*osg::PI-epsilon; alpha += _lonSpacing)
	  {
	    ca = cos(alpha); sa = sin(alpha);

	    // Compute meridian for given alpha, starting where the previous
	    // meridian left off.
	    for(beta = -osg::PI_2*osg::sign(step); fabs(beta) <= osg::PI_2+epsilon; beta += step)
	    {
	      cb = cos(beta); sb = sin(beta);
	      _vertices->push_back(osg::Vec3(_radius*ca*cb, _radius*sa*cb, _radius*sb));
	    }
	    step *= -1.0; // Reverse direction of travel

	    if(first)
	    {
	      // Draw Prime Meridian as an OpenGL LINE_STRIP
	      _mainGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_STRIP, start, _vertices->size() - start));
	      start = _vertices->size();
	      first = false;
	    }
	  }

	  // We can add all other longitude lines as one collective primitive set,
	  // since all of their vertices are connected at the poles.
	  _gridGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_STRIP, start, _vertices->size()-start));
	}
}

} // !namespace OpenFrames
