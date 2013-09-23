/***********************************
   Copyright 2013 Ravishankar Mathur

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

#include <OpenFrames/CoordinateAxes>
#include <osg/AlphaFunc>
#include <osg/BlendFunc>
#include <osg/Point>
#include <osg/PointSprite>
#include <osgDB/ReadFile>
#include <iostream>

namespace OpenFrames
{

CoordinateAxes::CoordinateAxes(const std::string &name)
	: ReferenceFrame(name)
{
	_init();
}

CoordinateAxes::CoordinateAxes(const std::string &name, float r, float g,
                             float b, float a)
	: ReferenceFrame(name, r, g, b, a)
{
	_init();
}

CoordinateAxes::~CoordinateAxes() {}

void CoordinateAxes::setAxisLength(double len)
{
	if((len != _axisLength) && (len >= 0.0))
	{
	  _axisLength = len;
	  _createAxes();

	  moveXAxis(osg::Vec3(len, 0.0, 0.0), 0.5*len);
	  moveYAxis(osg::Vec3(0.0, len, 0.0), 0.5*len);
	  moveZAxis(osg::Vec3(0.0, 0.0, len), 0.5*len);

	  _computeTickAttenuation();
	}
}

void CoordinateAxes::setDrawAxes(unsigned int axes)
{
	if(_drawAxes != axes)
	{
	  _drawAxes = axes;
	  _createAxes();
	}
}

void CoordinateAxes::setTickSpacing(double major_sp, double minor_sp)
{
	bool create = false;

	if((major_sp != _majorTickSpacing) && (major_sp >= 0.0))
	{
	  _majorTickSpacing = major_sp;
	  create = true;
	}

	if((minor_sp != _minorTickSpacing) && (minor_sp >= 0.0))
	{
	  _minorTickSpacing = minor_sp;
	  create = true;
	}

	if(create) _createAxes();
}

void CoordinateAxes::setTickSize(unsigned int majorSize, unsigned int minorSize)
{
	osg::Point *point;

	if(majorSize > 0)
	{
	  // Get the Point parameter for the major tick marks
	  point = static_cast<osg::Point*>(_majorTickGeom->getOrCreateStateSet()->getAttribute(osg::StateAttribute::POINT));
	  point->setSize(majorSize);
	  point->setMaxSize(majorSize);
	}

	if(minorSize > 0)
	{
	  // Get the Point parameter for the minor tick marks
	  point = static_cast<osg::Point*>(_minorTickGeom->getOrCreateStateSet()->getAttribute(osg::StateAttribute::POINT));
	  point->setSize(minorSize);
	  point->setMaxSize(minorSize);
	}
}

bool CoordinateAxes::setTickImage(const std::string &fname, bool force_reload)
{
	osg::StateSet *ss = _tickGeode->getOrCreateStateSet();

	if(fname.length() == 0) // Use default OpenGL point as marker
	{
	  ss->removeAttribute(osg::StateAttribute::BLENDFUNC);
	  ss->removeTextureAttribute(0, osg::StateAttribute::POINTSPRITE);
	  ss->removeTextureAttribute(0, osg::StateAttribute::TEXTURE);
	  return true;
	}

	// Check if there is already a texture being used.
	osg::Texture2D* texture = dynamic_cast<osg::Texture2D*>(ss->getTextureAttribute(0, osg::StateAttribute::TEXTURE));

	// If the current texture has the same filename as the new texture, then reload only if we have to.
	if(texture && (texture->getImage()->getFileName() == fname) && !force_reload) return true;

	osg::Image *image = osgDB::readImageFile(fname); // Load image from file
	if(image)
	{
	  // Set up blending so marker looks nice
	  osg::BlendFunc *fn = new osg::BlendFunc();
	  fn->setFunction(osg::BlendFunc::SRC_ALPHA, 
	                  osg::BlendFunc::ONE_MINUS_SRC_ALPHA);
	  ss->setAttributeAndModes(fn);

	  // Set up point sprite
	  osg::PointSprite *sprite = new osg::PointSprite();
	  ss->setTextureAttributeAndModes(0, sprite);

	  // Specify texture to use for point sprite
	  osg::Texture2D *tex = new osg::Texture2D();
	  tex->setImage(image);
	  ss->setTextureAttributeAndModes(0, tex);

	  return true;
	}
	else
	{
	  std::cerr<< "CoordinateAxes ERROR: Image file \'" << fname << "\' could not be found!" << std::endl;
	  return false; // Image was not found
	}
}

const osg::BoundingSphere& CoordinateAxes::getBound() const
{
	ReferenceFrame::getBound(); // Get the ReferenceFrame's bounds
	
	// Initialize a BoundingSphere that would just contain the coordinate axes
	osg::BoundingSphere bs;
	bs._radius = _axisLength;

	// Expand to encompass the rest of the reference frame
	bs.expandRadiusBy(_bound);

	// Finally, set this bound to be the frame's BoundingSphere
	_bound = bs;	
	return _bound;
}

void CoordinateAxes::setColor( const osg::Vec4 &color )
{
	// Set new color
	ReferenceFrame::setColor(color);
	(*_colors)[0] = color;

	// Inform geometries that something has changed
	_axesGeom->dirtyDisplayList();
	_majorTickGeom->dirtyDisplayList();
	_minorTickGeom->dirtyDisplayList();
}

void CoordinateAxes::_init()
{
	// Create geode to hold all the geometries
	_axesGeode = new osg::Geode;
	_tickGeode = new osg::Geode;

	_axesGeode->setName(_name + " axes geode");
	_tickGeode->setName(_name + " tick geode");

	// Set up an AlphaFunc to throw away tick mark pixels that are
	// nearly transparent.
	osg::AlphaFunc *alphaFunc = new osg::AlphaFunc;
	alphaFunc->setFunction(osg::AlphaFunc::GEQUAL, 0.05f);
	_tickGeode->getOrCreateStateSet()->setAttributeAndModes(alphaFunc);

	// Create the geometry drawables for the actual x/y/z axes, as well
	// as for the major and minor tick marks
	_axesGeom = new osg::Geometry;
	_majorTickGeom = new osg::Geometry;
	_minorTickGeom = new osg::Geometry;

	// Set up point parameters for the major/minor tick marks
	_majorTickGeom->getOrCreateStateSet()->setAttribute(new osg::Point);
	_minorTickGeom->getOrCreateStateSet()->setAttribute(new osg::Point);

	// Create the arrays for vertex and color data
	_vertices = new osg::Vec3dArray; // Vertices will be added later
	_colors = new osg::Vec4Array(1); // Color will be specified later

	// Disable lighting computations
	_axesGeom->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
	_majorTickGeom->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
	_minorTickGeom->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF);

	// Attach vertex and color data to the geometries
	_axesGeom->setVertexArray(_vertices.get());
	_axesGeom->setColorArray(_colors.get());
	_axesGeom->setColorBinding(osg::Geometry::BIND_OVERALL);
	_majorTickGeom->setVertexArray(_vertices.get());
	_majorTickGeom->setColorArray(_colors.get());
	_majorTickGeom->setColorBinding(osg::Geometry::BIND_OVERALL);
	_minorTickGeom->setVertexArray(_vertices.get());
	_minorTickGeom->setColorArray(_colors.get());
	_minorTickGeom->setColorBinding(osg::Geometry::BIND_OVERALL);

	// Add everything to this ReferenceFrame
	_axesGeode->addDrawable(_axesGeom.get());
	_tickGeode->addDrawable(_majorTickGeom.get());
	_tickGeode->addDrawable(_minorTickGeom.get());
	_xform->addChild(_axesGeode.get());
	_xform->addChild(_tickGeode.get());

	// Set up defaults
	_majorTickSpacing = 0.5;
	_minorTickSpacing = 0.25;
	_drawAxes = X_AXIS | Y_AXIS | Z_AXIS;
	(*_colors)[0] = getColor();
	setTickSize(8, 4); // 8-pixel major and 4-pixel minor tick marks
	setAxisLength(1.0);
}

void CoordinateAxes::_computeTickAttenuation()
{
	osg::Point *point;
	float a, b, c; // Coefficients for attenuation

	// The computed point size is based on the specified point size and
	// the attenuation parameters a, b, c. See glPointParameter with the
	// parameter of GL_POINT_DISTANCE_ATTENUATION for more info.
	// The equation is: 
	//   computed size = clamp(specified size * sqrt(1/(a + b*d + c*d*d)))
	//   where d is the eye-distance to the point
	// Here we compute attenuation parameters such that the tick marks
	// will be visible at distances proportional to the size of the axes.
	a = c = 0.0;
	b = 1.0/_axisLength;
	osg::Vec3 attenuation(a, b, c);

	// Get the Point parameter for the major tick marks
	point = static_cast<osg::Point*>(_majorTickGeom->getOrCreateStateSet()->getAttribute(osg::StateAttribute::POINT));
	point->setDistanceAttenuation(attenuation);

	// Get the Point parameter for the minor tick marks
	point = static_cast<osg::Point*>(_minorTickGeom->getOrCreateStateSet()->getAttribute(osg::StateAttribute::POINT));
	point->setDistanceAttenuation(attenuation);
}

void CoordinateAxes::_createAxes()
{
	unsigned int start; // Starting index in vertex array
	double mark; // Point to place tick mark
	double val;  // Temp variable

	// Prepare geometries for new vertices that will be computed
	_axesGeom->removePrimitiveSet(0, _axesGeom->getNumPrimitiveSets());
	_majorTickGeom->removePrimitiveSet(0, _majorTickGeom->getNumPrimitiveSets());
	_minorTickGeom->removePrimitiveSet(0, _minorTickGeom->getNumPrimitiveSets());

	// Clear all existing vertices
	_vertices->clear();

	// First add vertices for the axes that need to be drawn
	if(_drawAxes & X_AXIS)
	{
	  _vertices->push_back(osg::Vec3d(0.0, 0.0, 0.0));
	  _vertices->push_back(osg::Vec3d(_axisLength, 0.0, 0.0));
	}

	if(_drawAxes & Y_AXIS)
	{
	  _vertices->push_back(osg::Vec3d(0.0, 0.0, 0.0));
	  _vertices->push_back(osg::Vec3d(0.0, _axisLength, 0.0));
	}

	if(_drawAxes & Z_AXIS)
	{
	  _vertices->push_back(osg::Vec3d(0.0, 0.0, 0.0));
	  _vertices->push_back(osg::Vec3d(0.0, 0.0, _axisLength));
	}

	// Tell the axes geometry to draw data as lines
	_axesGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES, 0, _vertices->size()));

	start = _vertices->size(); // Start of major tick mark vertices

	// Now add vertices for each major tick mark on each axis
	for(mark = _majorTickSpacing; mark <= _axisLength; mark += _majorTickSpacing)
	{
	  if(_drawAxes & X_AXIS) // On X-axis
	    _vertices->push_back(osg::Vec3d(mark, 0.0, 0.0));

	  if(_drawAxes & Y_AXIS) // On Y-axis
	    _vertices->push_back(osg::Vec3d(0.0, mark, 0.0));

	  if(_drawAxes & Z_AXIS) // On Z-axis
	    _vertices->push_back(osg::Vec3d(0.0, 0.0, mark));
	}

	// Tell the major tick mark geometry to draw data as points
	_majorTickGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::POINTS, start, _vertices->size() - start));

	start = _vertices->size(); // Start of minor tick mark vertices

	// Now add vertices for each minor tick mark on each axis
	for(mark = _minorTickSpacing; mark <= _axisLength; mark += _minorTickSpacing)
	{
	  // Make sure the minor tick mark isn't on top of a major tick mark
	  val = mark / _majorTickSpacing;
	  val = val - (int)val;
	  if((val <= 1.0e-6) || (val >= 0.9999)) continue;

	  // Otherwise go ahead and add the minor tick mark
	  if(_drawAxes & X_AXIS) // On X-axis
	    _vertices->push_back(osg::Vec3d(mark, 0.0, 0.0));

	  if(_drawAxes & Y_AXIS) // On Y-axis
	    _vertices->push_back(osg::Vec3d(0.0, mark, 0.0));

	  if(_drawAxes & Z_AXIS) // On Z-axis
	    _vertices->push_back(osg::Vec3d(0.0, 0.0, mark));
	}

	// Tell the minor tick mark geometry to draw data as points
	_minorTickGeom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::POINTS, start, _vertices->size() - start));
}

} // !namespace OpenFrames
